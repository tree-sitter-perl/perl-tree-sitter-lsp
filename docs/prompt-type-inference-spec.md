# Type Inference + Invariant Derivation Spec

> Home for type-related architecture: what inference data lives in
> `FileAnalysis`, how it propagates through bindings, and the next set of
> modeling work (invocant mutations, hash-key unions, dynamic dispatch
> over method lists, functional-operator invariants).
>
> Cross-refs:
> - `docs/prompt-unification-spec.md` — refs_by_target, CachedModule, FileStore.
> - `docs/prompt-ref-coverage-provenance.md` — ref emission + derivation
>   chains (rule 7 + 8 from CLAUDE.md).

## Part 0 — What already lives in the data model

The type system today is a handful of small pieces on `FileAnalysis`.
Listing them here so future specs don't reinvent the wheel.

| Field / structure | Role |
|---|---|
| `InferredType` enum | Concrete types the builder can infer: `HashRef`, `ArrayRef`, `CodeRef`, `Regexp`, `Numeric`, `String`, `ClassName(String)`, `FirstParam { package }`. |
| `TypeConstraint { variable, scope, constraint_span, inferred_type }` | A point-in-time assertion that `variable` is of type `T` in a scope. Multiple per variable are allowed — `inferred_type(var, point)` picks the closest one before the point. |
| `CallBinding { variable, func_name, scope, span }` | `my $cfg = get_config();` — records the call-return link so the return-type post-pass can propagate types. |
| `MethodCallBinding { variable, invocant_var, method_name, scope, span }` | `my $c = $obj->meth();` — same idea for methods. Dynamic method names (`$obj->$m()`) expand through constant folding at emission time. |
| `HashKeyOwner::Sub { package: Option<String>, name: String }` | Package-qualified key ownership. Distinguishes `Alpha::get_config`'s `host` from `Beta::get_config`'s `host`. |
| `HashKeyOwner::Class(String)` | Moo/Moose/DBIC hash slots owned by a class. |
| `HashKeyOwner::Variable { name, def_scope }` | Fallback when a literal `%h` is just accessed directly — owner is the variable itself. |
| `sub_return_delegations: HashMap<String, String>` (builder-only) | `sub chain { return other() }` — the return-type pass and HashKey owner fixup walk this to the sub that actually owns the keys. |
| `refs_by_target: HashMap<SymbolId, Vec<RefId>>` | O(1) reverse index. Every HashKeyAccess whose owner resolves is linked here to its HashKeyDef symbol, so `references` / `rename` work without tree access. |

Landed behaviors:

- Qualified calls (`Foo::bar()`) and chained delegation (`sub chain { return get_config() }`) both propagate return types + hash-key ownership.
- Dynamic method dispatch (`$obj->$m()` where `$m = 'get_config'`) resolves via `resolve_constant_strings` and flows into `MethodCallBinding`.
- The HashKey owner fixup runs over **both** `call_bindings` and `method_call_bindings`, and both paths walk the delegation chain to the sub that owns the HashKeyDefs.
- **Consumer-side cross-file resolution.** `use Lib qw(get_config); my $c = get_config(); $c->{host}` — the builder can't resolve the owner at build time (imported), but `enrich_imported_types_with_keys` retries the owner fixup against `imported_hash_keys` and then `rebuild_enrichment_indices` re-runs the phase-5 linker against the newly-injected synthetic HashKeyDefs. `refs_by_target` stays correct across the enrichment boundary.

What's in tests (`file_analysis::tests::test_phase5_*`):

- `_hash_key_access_links_to_def_symbol`
- `_find_references_on_hash_key_def_without_tree`
- `_refs_by_target_index_populated_for_variables`
- `_qualified_sub_call_links_hash_key_access`
- `_hash_key_owner_flows_through_intermediate_sub`
- `_dynamic_method_call_via_constant_folding`
- `_demo_file_shape_resolves_all_access_sites`
- `_consumer_side_cross_file_resolves_via_enrichment`

---

## Part 1 — Invocant mutations

### Motivation

`$self->{count}++` / `$self->{items} = [...]` change the *contents* of the
invocant, not just read them. Today the builder treats these as
`HashKeyAccess` refs but doesn't update type beliefs about the hash
shape. Two concrete consequences:

- A Moo class with `has count => (is => 'ro')`: the reader symbol is
  known; `$self->count++` should be flagged as writing through a reader.
  Not tracked today.
- Emergent keys: `sub init { $self->{dynamic_thing} = 1 }` introduces a
  key that isn't declared via `has`. Completion on `$self->{` should
  surface it (scoped to the enclosing class).

### Design

Extend `AccessKind` already present on `Ref` — a HashKeyAccess in LHS
position gets `AccessKind::Write`. Most of this is already there; the
gap is in using it.

Two new derived tables on `FileAnalysis`:

```rust
/// Per (class, key) — whether we've seen any writes, and what type the
/// RHS had. Powers Moo-reader-violation diagnostics and helps narrow
/// `$self->{k}` type inside the class.
pub hash_key_mutations: Vec<HashKeyMutation>,

pub struct HashKeyMutation {
    pub owner: HashKeyOwner,
    pub key: String,
    pub write_span: Span,
    pub rhs_type: Option<InferredType>,
}
```

`hash_key_mutations` is populated in the builder during assignment
visits: any `lhs = hash_element_expression, access=Write` with an owner
of `Class(_)` or `Sub{_, "new"}` (the constructor's hash) emits a
mutation record.

### Derived capabilities

- **Dynamic key completion.** `$self->{` inside a class completes with
  the union of `has` declarations + every mutated key observed on
  `$self->{...}` across the class's methods.
- **Read-only violation diagnostics.** Writing through a Moo `is => 'ro'`
  accessor is a hint.
- **Narrower `$self->{k}` types.** If every mutation of `$self->{queue}`
  has `rhs_type = ArrayRef`, the read site can assume ArrayRef.

### Open questions

- Should mutation be class-scoped or method-scoped? Most value comes
  from class-scoped unions; method-scoped is strictly narrower but
  rarely useful in practice.
- `delete $self->{k}` as a "drop" signal — ignore for now, revisit if
  it bites.

---

## Part 2 — Hash key unions

### Motivation

Today each HashKeyDef has exactly one owner. Real code composes:

```perl
my $full = { %$defaults, %$overrides };
$full->{host};   # key comes from either source
```

And return merges:

```perl
sub make_result {
    my $base = $self->get_defaults;
    return { %$base, extra => 1 };
}
```

We emit an InferredType::HashRef for `$full` but don't know where its
keys come from. Completion offers nothing; references from `$full->{k}`
don't link to either source.

### Design

Extend `HashKeyOwner` — or better, add a `Union(Vec<HashKeyOwner>)`
variant:

```rust
pub enum HashKeyOwner {
    Class(String),
    Variable { name: String, def_scope: ScopeId },
    Sub { package: Option<String>, name: String },
    /// Keys pooled from multiple origins, e.g. `{ %$a, %$b, literal => 1 }`.
    Union(Vec<HashKeyOwner>),
}
```

Completion and refs treat Union as "any matching member does."

Builder changes:

- `visit_anon_hash` already detects hash construction; extend to walk
  `%$var` splices and record their owners (from the `$var`'s inferred
  type or call binding).
- Literal keys inside a union get owned by the enclosing sub (existing
  behavior).
- The synthesized `HashKeyDef` for a union's literal keys should carry
  the Union owner *plus* each individual owner — duplicates are OK, the
  linker dedupes.

### Interplay with refs_by_target

`build_indices`'s linker currently matches `(target_name, owner)` by
structural equality. For Union, we need "owner matches Union's member."
Easiest: when indexing, expand each Union def into one entry per member
(so a Union-owned def shows up under each member's lookup key).

### Open questions

- Cycles: `$a = { %$b }; $b = { %$a };` — cap recursion; don't chase
  variable-based owners through multiple hops in the builder.
- Cross-file splices (`{ %{ OtherMod::get() } }`) — defer to enrichment
  post-pass; union resolution happens after dep resolution lands their
  return keys.

---

## Part 3 — Method loops: dynamic dispatch inferences

### Motivation

```perl
for my $meth (@various_methods) {
    $invocant->$meth(9001);
}
```

Today `$meth` is a variable — the existing dynamic-dispatch path
constant-folds when `$meth` holds a single string. It gives up when
`$meth` is a loop variable over a list.

But we CAN say a lot here. Both sides carry rich invariants:

- `@various_methods` must be a list of method names (strings or
  identifiers) that `$invocant` responds to.
- `$invocant` must support *every* method in the list — this is a
  structural type constraint.
- Each call takes `9001` as arg 0, so every method in the list must
  accept `9001` at its first parameter position.

This is where dependent-type-flavored assertions show up: the list of
names is data that constrains the invocant's type.

### Design

New structure: `DynamicDispatchLoop`:

```rust
pub struct DynamicDispatchLoop {
    pub invocant: String,          // "$invocant"
    pub method_var: String,        // "$meth"
    pub method_list_source: MethodListSource,
    pub call_site: Span,
    pub arg_types_at_site: Vec<Option<InferredType>>,
}

pub enum MethodListSource {
    /// `@arr = qw(a b c)` or a literal list.
    Literal(Vec<String>),
    /// `@arr` is the loop variable — we can list the concrete names.
    ResolvedFromArray { array_name: String, names: Vec<String> },
    /// `@arr` comes from a sub call we don't know the contents of.
    Unknown,
}
```

Collected during the walk:

- `for my $m (list)` + `$inv->$m(...)` inside the body emits one
  `DynamicDispatchLoop` record.
- The `list` expression is resolved via `resolve_constant_strings`
  (which already handles `qw(...)`, `@const_array` references, and
  fat-comma lists).

### Derived capabilities

- **Method-existence diagnostics.** For each name in the list, if
  `$invocant`'s class doesn't resolve the method, emit a hint.
- **Invocant typing.** If any one of the methods in the list is only
  declared on class `Foo`, `$invocant` is narrowed to `Foo`-or-subclass.
  (Soft — a single resolved name doesn't prove class membership by
  itself, but it's useful for completion at the cursor.)
- **Refactoring safety.** A rename of `$invocant`'s class method `doX`
  needs to update every entry in `@various_methods` that contains
  `'doX'`. The existing provenance tracking for string literals (see
  ref-coverage-provenance spec) makes this doable.

### Open questions

- Arg type propagation across multiple calls — if every method in the
  list takes a Numeric first param and we pass `9001`, that's fine; if
  any takes a HashRef, the code's already wrong. Surface as a diagnostic.
- Loops-over-computed-lists (`for my $m (@{ $obj->methods })`). Treat
  as `MethodListSource::Unknown` for now; revisit when we have enough
  method-return tracking to mine the inner call.

---

## Part 4 — Functional operator invariants

### Motivation

```perl
my @titles = map $_->{title}, @articles;
```

In one expression, Perl is telling us three facts:

- `@articles` contains hash refs.
- Each element has a `title` key.
- `@titles` is an ArrayRef (list context) of whatever `title` resolves
  to (typically String).

Today we track none of this. Users get no completion on `$articles[0]->{`,
no rename support on `title`, and `@titles` has no inferred element type.

Similar for `grep`, `first`, `any`, `all`, `reduce`. Each has a predicate
block whose body gives us facts about the list element.

### Design

A new pass — **list-element inference from functional operators** —
runs in the builder and drops type constraints + hash-key ownership on
the list operand.

Detection:

- `map EXPR, LIST` and `map { BLOCK } LIST` — the expression/block is
  applied with `$_` bound to each list element.
- `grep`, `first`, `any`, `all`, `reduce` — same shape (some with `$a/$b`).

For each call:

1. Walk the block/expression looking for uses of `$_`.
2. Collect facts about `$_`: accessed as `$_->{key}` → HashRef with
   `key`; accessed as `$_->method` → must implement `method`; dereffed
   as `@$_` → ArrayRef; etc.
3. Translate those facts into constraints on the LIST:
   - `HashRef + key 'k'` constraint on `$_` → every element of
     `@articles` is HashRef with `k`.
   - `Method m` constraint on `$_` → every element is an object that
     implements `m`.

Emission:

- Push a synthetic `TypeConstraint` for each list variable tagged at
  the map/grep call span, with the derived `InferredType`.
- Inject synthetic `HashKeyDef` symbols for observed keys, owned by a
  new `HashKeyOwner::ListElement { array_name }` or attached to the
  existing owner that best fits.

### Derived capabilities

- `$articles[0]->{` completes with `title` (and any other keys observed
  in map/grep bodies over `@articles`).
- Renaming `title` in the HashKeyDef updates the access inside the map
  block.
- `@articles = [...literal]`: compare each literal element's shape
  against the map-derived constraint; warn on mismatch.
- Reduce: `reduce { $a + $b->{count} } 0, @stats` lets us say `@stats`
  elements have a `count` key.

### Interplay with the existing type system

The block-derived facts use the same `InferredType` / `HashKeyDef`
plumbing — this is an **emission rule**, not a new data model. Mirrors
the framework-synthesis pattern from the unification spec.

### Open questions

- Nested scopes: `map { map { $_->{k} } @$_ } @outer` — facts about
  inner `$_` apply to inner scope; outer `$_` gets the ArrayRef fact.
- Block returns vs expression form — the element-type return of `map`
  can be inferred from the block's last-expression type (we already
  track `last_expr_type` per sub scope; extend to map blocks).
- `reduce` specifically is cleanest when `$a` and `$b` are tracked
  separately — `$a`'s type becomes the accumulator, `$b` the element.

---

## Part 5 — Dependent-type patterns

The earlier parts handle "static types about values." This part covers
**types parameterised by values** — the chunk of the HoTT conversation
that actually transfers to a Perl static analyzer. Three patterns, all
built on existing `InferredType` + emission rules. No new type theory,
no univalence, no paths.

### 5a — Value-indexed returns (Pi types)

`get_config('host')` returns `String`; `get_config('port')` returns
`Int`. The return type is a function of the **literal value** of the
argument.

**Data shape.** Extend `ExportedSub`-style metadata already carried on
`Symbol`:

```rust
// On SymbolDetail::Sub:
keyed_returns: Option<HashMap<String, InferredType>>,
```

Set at build time when the builder sees a sub whose body is a dispatch
on the first arg:

```perl
sub get_config {
    my ($key) = @_;
    return { host => 'localhost', port => 5432 }->{$key};
}
```

The anon-hash literal we already harvest gives us `host → String`,
`port → Numeric`. Under Pi-type semantics, calling `get_config('host')`
with a literal first-arg yields `String`.

**Emission rule.** During `visit_sub`, detect the `{ ... }->{$param}`
shape (or `my %table = (...); return $table{$param}`). If every RHS is
a typed literal, record `keyed_returns` on the sub's Symbol.

**Consumer side.** `infer_expression_type` on a call where the first
arg is a literal string looks `keyed_returns[lit]` first; falls back to
the plain `return_type` otherwise. Wires into the existing
`CallBinding` fixup without new structures.

**Practical payoff.** `get_config('host')->` completes with `String`
methods. `$cfg = get_config('port'); $cfg + 1` doesn't trip "numeric on
non-numeric" diagnostics.

### 5b — Sum types + tag-discriminated narrowing

```perl
sub fetch {
    return { ok => 1, data => $x } if $ok;
    return { ok => 0, error => $msg };
}
my $r = fetch();
if ($r->{ok}) {
    $r->{data};   # narrowed to the "ok" branch's shape
} else {
    $r->{error};  # narrowed to the "err" branch's shape
}
```

**Data shape.** Extend `InferredType`:

```rust
InferredType::Union(Vec<Arc<InferredType>>),
```

and a minor addition to `TypeConstraint` so narrowing produces a
scope-limited override:

```rust
pub struct TypeConstraint {
    pub variable: String,
    pub scope: ScopeId,
    pub constraint_span: Span,
    pub inferred_type: InferredType,
    /// Only applies inside this span (if-body / unless-body). Absent →
    /// applies from constraint_span onward within `scope`.
    pub narrowing_scope: Option<Span>,
}
```

**Emission rule — construction side.** `resolve_return_types` already
collects all return branches. When multiple branches produce distinct
literal hash shapes, emit `InferredType::Union([Shape1, Shape2, ...])`
instead of collapsing to a single HashRef.

**Emission rule — narrowing side.** Walk `if` / `unless` conditions:

| Condition pattern | Effect in the then-body |
|---|---|
| `if ($v)` / `if (defined $v)` | Narrow `$v` to non-undef members of its Union. |
| `if ($v->{tag} eq 'literal')` | Narrow `$v` to Union members whose `tag` shape matches `literal`. |
| `if (ref $v eq 'ARRAY')` | Narrow `$v` to `ArrayRef`. |
| `if (blessed $v)` | Narrow away non-object members. |

Each narrowing emits a `TypeConstraint` with `narrowing_scope` set to
the if-body span. `inferred_type(var, point)` picks the narrowed
constraint when point is inside the body, falls back to the outer
constraint otherwise.

**Practical payoff.** Accessing `$r->{data}` inside the truthy branch
completes only with `data`-branch keys, not the err-branch's `error`.
Diagnostics flag `$r->{error}` in the ok-branch as "key not in this
variant."

### 5c — Parametric types (ResultSet[R], indexed string collections)

`$schema->resultset('Users')` returns a `ResultSet<Users>` where
`Users` is the DBIC Result class. The valid keys for
`$rs->search({ KEY => ... })` depend on `Users`.

**Data shape.** Extend `InferredType`:

```rust
InferredType::Parametric {
    base: String,        // "ResultSet"
    type_arg: String,    // "MyApp::Schema::Result::Users"
},
```

**Emission rules.**

- `$schema->resultset('Users')` (literal first arg) → the binding site
  records `InferredType::Parametric { base: "ResultSet", type_arg: canonicalized_class }`.
- `$rs->search(...)` / `$rs->search_rs(...)` / `$rs->find(...)`: return
  type is the same `Parametric` — the parameter threads through.

**Consumer side — context-sensitive completion.** When
`cursor_context.rs` detects cursor in a hash-key position inside
`search-family method arg[0]` on a receiver of type
`InferredType::Parametric { base: "ResultSet", type_arg }`:

- Look up the HashKeyDefs (+ synthesized column accessors) in
  `Package(type_arg)`. Offer them as completions.
- Emit an `unresolved-dbic-column` diagnostic when a literal key in the
  call doesn't exist on the Result class.

**Relationship prefetch** (`->search({...}, { join => 'posts' })`):
expand the valid key set with `posts.col` entries from the joined Result
class's HashKeyDefs. Phase-2 complexity — same framework.

**Practical payoff.** DBIC column completion inside `->search`,
goto-def on `name` in `->search({ name => ... })` jumps to the
`add_columns` site, rename safety, unknown-column diagnostics.

### What to skip

- **Univalence.** "Equivalent types ARE equal" is beautiful but
  engineering-useless here. We already collapse equivalent
  representations ad-hoc (framework synthesis across Moo/Moose/Mojo::Base
  producing the same accessor shape); no need for a formal mechanism.
- **Path induction / identity types as first-class values.** Rename
  is "transport along equality" in HoTT terms; we do it procedurally
  via `refs_by_target` and that's the right level.
- **Higher inductive types.** Perl has no user-visible quotient
  structures to model.
- **Full dependent inference.** Undecidable. The three patterns above
  are the sweet spot — they cover the common idioms (value-indexed
  lookups, sum-type results, parametric collections) with bounded
  analysis cost.

### Interplay with landed phase-5 work

All three patterns compose with the current model:

- Value-indexed returns (5a) piggy-back on existing `HashKeyDef` harvesting + `CallBinding` fixup. The `keyed_returns` table is read by a small extension in `resolve_return_types`.
- Sum types (5b) add a new `InferredType` variant + narrowing-scoped `TypeConstraint`. `inferred_type(var, point)` already picks the closest constraint; the narrowing field is a refinement of that lookup.
- Parametric types (5c) add a new `InferredType` variant + a new cursor-context variant. Everything else (HashKeyDef lookup, column completion) reuses the existing pipeline.

No new graph. No parallel data model. Same univalence-style collapse
the unification refactor applied to FrameworkEntity.

---

## Implementation ordering

All parts are mostly independent and can land separately.

| Part | Build cost | User-visible win | Prerequisite |
|---|---|---|---|
| 1 — Invocant mutations | small; new field + assignment detection | dynamic-key completion; Moo ro violation hints | none |
| 2 — Hash key unions | medium; owner enum extension + linker change | completion + references on merged hashes | none |
| 3 — Method loops | medium-large; new collection pass | diagnostics + invocant narrowing | const-folding infra (already there) |
| 4 — map/grep/reduce invariants | medium; block-walk pass | @-array key inference, list-element types | none |
| 5a — Value-indexed returns | small; new `keyed_returns` on SymbolDetail::Sub | context-sensitive return types for dispatch subs | none |
| 5b — Sum types + narrowing | medium; Union variant + narrowing_scope on TypeConstraint | tag-discriminated narrowing; diagnostics on wrong-variant access | none |
| 5c — Parametric types (DBIC) | large; new `Parametric` variant + cursor-context wiring | DBIC column completion inside search/update/find; rename on columns | 1, 2 ideally |

Land each behind a pin-the-fix test pair (one showing the previously-
broken behavior, one showing the now-correct one). This matches the
precedent set by the phase-5 tests.

## Non-goals

- **Full structural typing.** We're not inventing a row-polymorphic
  record calculus. The goal is to cover the 90% of Perl idioms with
  cheap heuristics and fail gracefully otherwise.
- **Runtime-dependent types** (e.g. values loaded from config files).
  Static-only.
- **Dynamic import shapes.** `$module->import()` at runtime with a
  computed symbol list is out of scope; we only track what's declarable
  at the source level.
