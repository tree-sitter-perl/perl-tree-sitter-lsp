# Nested hash-key intelligence

**Status: ALL THREE TIERS LANDED (June 2026, `nested-hashkey`
branch).** Tier 1: `$row->{name}` resolves to the column def via the
post-fold variable-owner upgrade. Tier 2: `HashWithKeys` (per-key value
types, `open` spread flag) with `->{key}` narrowing through assignment
hops, double-drills, and sub-return literals; two lattice rules made it
sound (all-hash-shaped return arms unify coarse; structure dominates
rep in `subsumes_narrowing` + the plain-type fold). Tier 3: array
literals type as per-index `Sequence` tuples (heterogeneous answers
per index instead of bailing), `->[N]` projects, and the mixed drill
`$obj->{users}->[0]->{name}` chains end-to-end. Displays stay coarse
("HashRef") for hashes; sequences display `Sequence<…>` elided past 4
elements. The closed-shape-miss diagnostic landed (whole-story
gated), and mutation effects are modeled: the mutation-extension pass
(`witnesses::emit_mutation_extension_witnesses`) folds `$v->{k} = …`
writes into the shape — unconditional static-key writes EXTEND a
closed shape (value typed from the RHS), conditional / dynamic /
scope-crossing writes switch it open. Still open below: the remaining
deferred items.

Builds on the sealed-enum `ParametricType` shape landed in
Part 5c (`docs/adr/parametric-types.md`). Each flavor carries
its own data fields, so recursive nesting (`HashRef<ArrayRef
<Str>>` etc.) lives in flavor-specific `Box<InferredType>`
slots — no `Vec<TypeArg>` shoehorning. This work is about
*emitting* the right flavor for hash-shaped data and
*consuming* it via narrowing.

## What "nested hash-key intelligence" means

Three increasingly ambitious cases:

```perl
# Tier 1 — hash-key access on a Parametric value
my $row = $rs->find($id);     # Parametric{ResultSet, [Class("Users")]}
$row->{name};                 # cursor on `name` should resolve to the column def

# Tier 2 — structurally-typed hash drill-down
my $config = { db => { host => 'localhost', port => 5432 } };
$config->{db};                # → HashWithKeys{host: String, port: Numeric}
$config->{db}->{host};        # → String

# Tier 3 — mixed array/hash drill
my $obj = { users => [ { name => 'A', id => 1 } ] };
$obj->{users};                # → ArrayRef<HashWithKeys{name: String, id: Numeric}>
$obj->{users}->[0]->{name};   # → String
```

Today: tier 1 fails because `add_columns` synthesizes only `Method`
symbols, not `HashKeyDef`. Tiers 2/3 fail because there's no
structurally-typed hash variant — hash literals get folded to
`HashRef` (rep-only) and any per-key type information is lost.

## Tier 1 — hash-key access on a Parametric value

**Goal:** `$row->{name}` lands on the `name` column def, same as
`$row->name` does today.

**Diff sketch:**
- `Builder::visit_dbic_add_columns` synthesizes `HashKeyDef` symbols
  alongside the existing `Method` symbols, owner
  `HashKeyOwner::Class(current_package)`. Mirrors what Mojo `has`
  already does.
- `FileAnalysis::find_definition`'s `RefKind::HashKeyAccess` arm:
  when invocant types as `Parametric`, look up
  `HashKeyOwner::Class(type_args[0].as_class())` (DBIC's row class)
  in addition to / instead of the receiver's base class. Today it
  consults the receiver class via `resolve_hash_owner_from_tree`.

**Cost: ~30 LOC + tests.** Reuses every consumer path; no new
variant. Ships the moment `add_columns` synths HashKeyDef.

**Test cases:**
- `$row->{name}` lands on column def (basic).
- `$row->{not_a_column}` returns None.
- Cross-file: producer defines columns, consumer accesses
  `$row->{name}` after `$rs->find($id)`. Composes through Parametric
  threading.

## Tier 2 — structurally-typed hashes (`HashWithKeys`)

**Goal:** Hash literals with literal keys carry per-key type info;
`->{key}` access narrows to the per-key type.

**New variant:**
```rust
InferredType::HashWithKeys {
    keys: Vec<(String, TypeArg)>,
    /// True if the literal includes a `...` / `%$other` spread, so
    /// keys are open-ended. Closed (false) → unknown keys are an
    /// error/diagnostic; open (true) → unknown keys may resolve via
    /// the spread source.
    open: bool,
}
```

`Vec<(String, TypeArg)>` (not `HashMap`) — order-preserving for
stable display, plus most hashes have ≤10 keys so linear lookup
beats hashing.

**Builder emission rule:**
- `{ k => V₁, k₂ => V₂, … }` literal → `HashWithKeys { keys: [(k,
  type_of(V₁)), ...], open: false }`. Already half-done in the
  HashKeyDef synthesis path; needs the type side.
- `{ %$other, k => V }` → `HashWithKeys { keys: [...], open: true }`
  (the `open` flag carries the spread).
- Hash assigned to a `my %h` → infer at use sites (Tier 3 territory).

**Reducer additions:**
- `HashKeyAccess` chain narrowing — when `expr->{KEY}` follows an
  expression typed `HashWithKeys`, the result types as the
  per-key entry. Mirrors the variable narrowing in
  `FrameworkAwareTypeFold` but for hash-access spans.
- Constraint composition: if the producing literal had `open: true`,
  unknown keys fall through to the spread's source.

**Diagnostic opportunity:** Closed-shape access to a missing key
(`$config->{typeo}` when `typeo` isn't a literal key) is a
diagnostic — same family as PL-class checks. Defer to the diagnostic
framework workstream; emit the witness now, surface later.

**Cost: ~150 LOC + tests.** Real work.

**Test cases:**
- Direct literal: `my $c = { host => 'x' }; $c->{host}` → String def.
- Nested: `my $c = { db => { host => 'x' } }; $c->{db}->{host}` →
  String narrowing through `db`.
- Re-bound: `my %h = (k => 1); my $r = \%h; $r->{k}` → Numeric.
- Sub return literal: `sub cfg { { host => 'x' } } cfg()->{host}`.
- Open shape: `my $c = { %$base, host => 'x' }` — keys from `$base`
  contribute when `host` isn't a hit.

## Tier 3 — array element narrowing + mixed drill

**Goal:** `$obj->{users}->[0]->{name}` chains through array element
type and hash narrowing.

**Reuses Parametric for arrays:**
- `[1, 'x']` → `Parametric { base: "ArrayRef", type_args:
  [TypeArg::Type(Numeric), TypeArg::Type(String)] }` for known
  element types. Or `Parametric { base: "ArrayRef", type_args:
  [TypeArg::Type(Union { ... })] }` if we add a `Union` variant.
  Decide at implementation time; for the CRM's idiom (`[ Foo->new,
  Foo->new ]` — homogeneous), `Parametric { base: "ArrayRef",
  type_args: [TypeArg::Class("Foo")] }` is the common case.

**Reducer:** `expr->[N]` and `expr->[N..M]` narrow to the element
type. `[N]` of a homogeneous `Parametric{ArrayRef, [T]}` returns T;
heterogeneous (`type_args.len() > 1`) is more complex — bail to
unknown for now, address with sum types (Part 5b) when motivated.

**Cost: ~50 LOC on top of tier 2.**

**Test cases:**
- `[ Foo->new ]->[0]->method` types correctly.
- `$obj->{users}->[0]->{name}` end-to-end.
- Heterogeneous `[1, 'x']->[0]` → unknown (we don't claim).

## Sequencing & priority

1. **Tier 1** — bundle into Part 5c PR if cheap, otherwise next PR.
   Pure win; no new variant; just synthesizes HashKeyDef + extends
   one consumer arm.
2. **Tier 2** — own PR. Adds `HashWithKeys` variant + emission for
   hash literals + chain narrowing. Land before Tier 3.
3. **Tier 3** — own PR. Reuses Parametric for arrays + element
   narrowing. Composes with Tier 2 for the mixed drill.

## Out of scope

- **Inferring hash structure from `bless` targets.** `bless { a => 1,
  b => 2 }, 'Foo'` produces a Foo with hash structure {a, b} —
  the existing `bless`-target handling already records this for
  HashKeyDef synthesis on `Foo`; Tier 2 would let the type system
  see the per-key types too. Bundle if obvious; defer otherwise.
- **JSON / YAML / TOML loaded data.** Could in principle be a Tier 2
  emission if we parse the schema, but that's a separate plugin
  domain.
- **Mutation-induced widening.** LANDED — see the mutation-extension
  pass note up top. Residual flavors still open: literal `%hash`
  variables (`my %h = (a => 1); $h{b}` — no shape TC is minted for
  hash-sigil variables, so neither extension nor the diagnostic apply;
  the same model, second spelling), array index writes on `Sequence`
  tuples, and conditional-reassignment disagreement (`$spec = {…}
  unless ref $spec` — still a trust-gate suppression via
  `reassigned_scalars`, not a lattice fold).
- **Recursive type constraints from Type::Tiny / Moose.** `isa =>
  HashRef[ArrayRef[Str]]` ASTs. This is a separate plugin emission
  story (`use Types::Standard` plugin extends Moo synthesis to
  read the constraint expression and emit recursive Parametric).
  Tracked under residual Part 5c, future phase.
