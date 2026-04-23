# Type Inference + Invariant Derivation Spec

> Home for type-related architecture: what inference data lives in
> `FileAnalysis`, how it propagates through bindings, and the next set of
> modeling work (invocant mutations, hash-key unions, dynamic dispatch
> over method lists, functional-operator invariants, blessed/rep
> product via observation IR, and fact witnesses as a cross-stack
> primitive with plugin-registered reducers).
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

## Part 6 — Observation IR + framework-aware resolution

### Motivation

Today `InferredType` is a flat sum: `HashRef | ArrayRef | ClassName(Foo) | …`.
Siblings in the sum are assumed mutually exclusive. But bless makes the
"blessed hashref" case — `$self` is simultaneously an object of class `Foo`
AND a hashref underneath. Observing `$self->{key}` inside `sub name` of
Mojolicious::Routes::Route today looks like a *contradiction* ("was
ClassName, now HashRef"), because the sum can only hold one. The
inference site overwrites, the type flips to HashRef, and the chain
dies at the next method call.

The shape the code expresses is a **product**: (dispatch axis) × (rep
axis). But the product's right home is the IR, not the user-facing
type. Representation depends on the framework — Mojo::Base is
hashref-backed, Perl 5.38 `class` uses inside-out / opaque refs, DBIC
Result classes are something else again, and 1998-vintage
`bless [], $class` is arrayref-backed. The IR collects facts; the
resolver projects to a concrete `InferredType` using framework context.

### Design

Keep `InferredType` as the query-visible flat enum. Add a constraint
bag keyed by variable + scope — a list of observations, never
overwriting:

```rust
enum TypeObservation {
    ClassAssertion(String),        // `my $x = Foo->new`
    FirstParamInMethod,            // `shift` / `$_[0]` as invocant
    HashRefAccess,                 // `$v->{k}`, `%$v`, `@$v{…}`
    ArrayRefAccess,                // `$v->[i]`, `@$v`
    CodeRefInvocation,             // `$v->()`, `&$v`
    NumericUse, StringUse, RegexpUse,
    BlessTarget(Rep),              // `bless [], $class` → Rep::Array
    ReturnOf(SymbolId),            // fluent-chain binding target
    // …
}
```

Resolver signature:

```rust
fn resolve_type(
    obs: &[TypeObservation],
    framework_ctx: FrameworkCtx,
) -> Option<InferredType>
```

Framework context carries the enclosing package's framework mode
(reuse `builder::FrameworkMode` — Moo/Moose/MojoBase/etc.). Rules the
resolver applies, roughly in order:

1. **Class beats rep when consistent.** `ClassAssertion(Foo)` or
   `FirstParamInMethod` with a known enclosing class dominates
   `HashRefAccess` / `ArrayRefAccess` IF the framework's backing rep
   matches the access kind. Mojo::Base + HashRefAccess = keep the
   class. Perl 5.38 `class` + HashRefAccess = contradiction (warn).
2. **Bless target pins the rep axis.** Seeing `bless [], $c` in the
   same scope tells the resolver the rep side of the product is
   `Array`.
3. **Return-of chains fold.** `ReturnOf(sym)` looks up the sub's
   return type; multiple ReturnOfs collapse via the existing multi-
   dispatch-on-arity path (already landed).
4. **Generic rep observations** with no class evidence resolve to the
   plain HashRef / ArrayRef / CodeRef type.

The existing `inferred_type(var, point)` becomes the query API; it
calls `resolve_type` once per query, optionally memoised per (var,
point). `TypeConstraint` stays for "asserted final type" edge cases
(`my $x = Foo->new` is still directly a ClassAssertion observation,
emitted at the assignment span).

### Derived capabilities

- **`sub name`-style methods don't flip their `$self` type.** The bag
  is `[FirstParamInMethod, HashRefAccess("name"), HashRefAccess("custom")]`;
  Mojo::Base says rep is Hash, so the resolver keeps
  `ClassName(Route)`. Chain downstream of the method call stays typed.
- **Bless variance handled uniformly.** Arrayref-backed classes work
  out of the box — `BlessTarget(Array)` as an observation forces the
  rep axis.
- **Debuggable.** Inspect the bag; find the rogue observation; fix
  the rule.

### The hard sub-problem: arity multidispatch from procedural bodies

Declared signatures already give us arity-multidispatch return types
(landed). The tough case is **procedural** arity-discrimination:

```perl
sub name {
    my $self = shift;
    return $self->{name} unless @_;              # arity 0 → String
    @$self{qw/name custom/} = (shift, 1);
    return $self;                                 # arity ≥1 → self (fluent)
}
```

No signature to read; the arity branch is expressed via `@_` tests
(`unless @_`, `if @_ == 1`, `if scalar(@_) == N`, `return … unless @_`,
`die unless @_ == 2`, `my ($x) = @_; return unless defined $x`).
Extracting the arity-indexed return map from that requires:

- Identifying *arity-gating conditionals*: recognize the shapes
  (`unless @_`, `if @_ == N`, `if scalar @_ == N`, `@_ > N`, …) as
  branches on the caller's arity.
- Per-branch return-type collection: each arity-gated branch gets
  its own return-type inference that produces a
  `keyed_returns`-like arity→type map (Part 5a's structure, but keyed
  by arity rather than by first-arg literal).
- Defaults: the "fall-through" branch is the catch-all; its return
  type applies to any arity not explicitly branched on.

This is genuinely hard because:

- The arity tests are idiomatic, not structural — every Perl codebase
  spells them slightly differently. The detector needs several shapes.
- Branches mutate variables (`shift` inside the condition reduces
  `@_`); tracking whether the test fires before or after a `shift`
  changes which arity the branch covers.
- Implicit returns from fall-through combine with explicit early
  returns (`return $self->{k} unless @_` + fall-through `return $self`).

Not required for Mojo day-to-day — declared signatures cover the 90%
case. Worth doing when we target plain-OOP Perl codebases or want to
type through `name`-style getter/setter pairs. Land behind a focused
test suite per arity shape.

### Interplay with landed work

- **Fluent chain** (already done): multi-hop `$a->m1(…)->m2(…)->m3()`
  return-type propagation with arity-dispatch via declared signatures
  — this chapter doesn't touch it. The new IR emits `ReturnOf(sym)`
  observations that feed through the existing chain resolver.
- **Ternary descent** (not done, independent): `A ? B : C`
  infer-both-arms lives in `infer_expression_type`. Simple to land,
  should land before this chapter — once the resolver is framework-
  aware, ternary just pushes observations from both arms and the
  resolver folds.
- **Part 5b — Sum types + narrowing**: the narrowing constraint
  (`if (ref $v eq 'ARRAY')`) becomes another observation kind with a
  scoped applicability. Composes naturally.

### Non-goals (for this chapter)

- Rewriting every `InferredType` consumer. The resolver projects back
  to `InferredType`; query sites don't care about the bag.
- Full effect-tracking (which branch reads, which writes, which
  throws). Keep to type-relevant observations.
- Tracking arity through `@_` shift-sequences in depth — the arity
  detector matches a finite set of idioms; anything stranger falls
  back to the declared-signature path (which is zero information for
  procedural bodies, and that's fine).

---

## Part 7 — Fact witnesses

### Motivation

Across the stack we already carry "facts about values" but each one
has its own struct, its own index, its own consumer:

- `TypeConstraint` — a scoped type belief about a variable.
- `CallBinding` / `MethodCallBinding` — "this variable was assigned
  the return value of that call."
- `HashKeyOwner` — who owns this hash key.
- `package_parents` — inheritance edges.
- `imported_hash_keys` — keys brought in via cross-file enrichment.
- Provenance chains (rule 9 in CLAUDE.md) — the string `'Users#list'`
  derives a method ref; renaming the method transports through.

Each is ad-hoc. The first-class concept hiding across all of them is
a **witness**: typed evidence that some proposition holds about a
specific code location, tagged with its source so consumers can trace
it back. The pieces above can migrate to this shape incrementally;
and several blocked items elsewhere in this spec (chain-accumulated
route facts, Part 6's framework-aware resolver, Part 5c's DBIC column
inference, rename-through-equivalence in the provenance spec) all
want the same primitive.

The Part 7 driving task is chain-typed route aggregation — collapsing
`$r->get('/users')->to('Users#list')->name('users_list')` into one
`<route> GET /users → Users#list` outline entry. That task can't land
until witnesses exist, so the mojo-routes rewrite is the first
user-visible payoff. Several more fall out of the same plumbing.

### Core abstraction

```rust
pub struct Witness {
    pub attachment: WitnessAttachment,
    pub source:     WitnessSource,
    pub payload:    WitnessPayload,
    /// Where the user can see why — rendered in hover, diagnostic
    /// "because:" text, provenance traversal. Never None; empty
    /// span means the core synthesized this with no source anchor.
    pub span:       Span,
}

pub enum WitnessAttachment {
    /// Traditional scope-indexed variable facts — what TypeConstraint
    /// / CallBinding already index by.
    Variable { name: String, scope: ScopeId },
    /// An expression's result. Every method/function call in the
    /// tree produces a `Ref`; facts about the returned value attach
    /// here. This is what makes chain aggregation possible.
    Expression(RefId),
    /// A symbol property (e.g. "this sub is a dispatcher", "this
    /// class is a ResultSet").
    Symbol(SymbolId),
    /// A specific call site — properties of the call, not its result
    /// or its receiver.
    CallSite(RefId),
    /// Hash key metadata — provenance of keys, writes into them,
    /// derivations from literal sources.
    HashKey { owner: HashKeyOwner, name: String },
    /// Package-level facts (isa edges, mode tags).
    Package(String),
}

pub enum WitnessSource {
    Builder(&'static str),      // pass identifier, e.g. "signature_extraction"
    Plugin(String),             // plugin id, e.g. "mojo-routes"
    Enrichment(&'static str),   // e.g. "imported_hash_keys"
    DerivedFrom(RefId),         // a derivation edge — another witness produced this one
}

pub enum WitnessPayload {
    /// Type belief (what Part 6 and TypeConstraint carry).
    InferredType(InferredType),
    /// Keyed typed fact. Keys are free-form within a (source, family)
    /// scope — the reducer that consumes them is responsible for
    /// knowing its own schema.
    Fact { family: String, key: String, value: FactValue },
    /// "This witness's subject is derived from that ref." Rename
    /// transport walks these edges; provenance rendering traces them.
    Derivation,
    /// Plugin-defined payload that doesn't fit the above. Stored as
    /// structured rhai Dynamic / serde Value, not opaque bytes, so
    /// cross-plugin inspection is possible.
    Custom { family: String, data: serde_json::Value },
}

pub enum FactValue {
    Str(String),
    List(Vec<String>),
    Bool(bool),
    Num(f64),
    Map(BTreeMap<String, FactValue>),  // composite facts
}
```

All witnesses live in a single bag on `FileAnalysis`, with indexes
by attachment kind for cheap filtering. Existing structures can
be reconstructed as typed views over the bag — no immediate forced
migration; each existing struct gets a "project to witnesses" method
and consumers choose when to cut over.

### Callback-driven reduction

A bag of raw witnesses isn't the consumable shape. Consumers want
folded answers: "what's the type of this variable here?" "what
route is being built by this chain?" "does this hash key exist?"

Folding is **plugin-chosen**, not hardcoded. Last-write-wins is wrong
for DBIC where-clauses (they append); list-append is wrong for a
route's `target` (second `->to(...)` overrides); set-union is wrong
for a scope-narrowed type (the narrower scope wins). Rather than
bake a policy, register reducers:

```rust
pub trait WitnessReducer: Send + Sync {
    /// Which witnesses does this reducer claim? Typically filters on
    /// attachment kind + source + payload family — "Expression
    /// witnesses from mojo-routes with family 'route'."
    fn claims(&self, w: &Witness) -> bool;

    /// Fold the claimed witnesses into a single value. Input is
    /// already filtered (only witnesses this reducer claimed);
    /// ordering is by `span` unless the reducer overrides. Output is
    /// whatever shape the reducer's consumers need.
    fn reduce(&self, ws: &[&Witness]) -> ReducedValue;

    /// Emission trigger for reducers that produce side-effects
    /// (synthesizing a Handler at chain completion, emitting a
    /// diagnostic when a fact set is inconsistent). Default: none
    /// — the reducer is pure projection.
    fn on_complete(&self, ws: &[&Witness]) -> Vec<EmitAction> { vec![] }
}

pub enum ReducedValue {
    Type(InferredType),
    FactSet(BTreeMap<String, FactValue>),
    Custom(serde_json::Value),
    None,  // reducer declined / insufficient evidence
}
```

Rhai plugins register reducers via the existing plugin trait:

```rhai
fn reducers() {
    [
        #{
            claims: #{
                attachment_kind: "Expression",
                source_plugin:   "mojo-routes",
                payload_family:  "route",
            },
            // Rhai fn that takes the witness slice and returns
            // the folded fact map.
            reduce: "route_reduce",
            // Rhai fn that takes the slice and returns EmitActions.
            // Called once per attachment when the reducer's
            // `sufficient` predicate flips true.
            on_complete: "route_emit_if_complete",
            sufficient: "route_is_complete",
        },
    ]
}
```

This gives plugins full control over fold semantics (LWW, append,
max, custom merge) and emission timing (threshold-based, end-of-
statement, end-of-file). The core provides the bag, the indexing,
the reducer-dispatch loop, and a couple of common reducers for the
cases that are genuinely universal (type-observation fold for
Part 6, derivation walk for rename).

### Consumers across the stack

| Consumer | How witnesses / reducers are used |
|---|---|
| **Part 6 resolver** | Built-in reducer: claims `InferredType` payloads on `Variable` / `Expression` attachments, folds via framework-aware rules to one `InferredType`. Becomes the single impl of `inferred_type(var, point)`. |
| **Completion** | Queries reducers per cursor target: "for this expression, give me the fact set." Mojo helpers, DBIC columns, route-by-URL — all come through the same fetch. |
| **Diagnostics** | Pulls `span` from the witnesses that justified a type belief, renders as "because: …" secondary info on the diagnostic. |
| **Rename** | Walks `WitnessSource::DerivedFrom` edges as a graph. Renaming a string literal transports through every derivation whose source is that literal. |
| **Hover** | "Type derived from:" appends a list of justifying spans, taken straight from the witness bag. |
| **Workspace symbol** | Named `Fact`s with well-known keys (e.g. `route.path`) can be searched alongside symbol names. |
| **Plugins** | Read facts their own reducer produced for downstream plugin logic (mojo-routes' `->to` reading facts set by `->get`). Write new witnesses during their emit hooks. |

### Applications

**A — Chain-typed route aggregation (the driving case).**
mojo-routes registers a `route` reducer claiming Expression
witnesses with `family: "route"`. Each of `->get/post/…/under(PATH)`
attaches `verb` / `path` facts to the call's result; `->name(X)`
attaches `name`; `->to('Ctrl#act')` attaches `target`. The reducer's
`sufficient` predicate fires when `verb ∧ path` holds; `on_complete`
emits ONE Handler — `name: "GET /users"`,
`outline_label: "GET /users → Users#list"`, `display: Route` —
plus the existing MethodCallRef into `Users::list` and a url_for-keyed
alias. Second `->to` on the same chain? LWW via the reducer (the
reducer returns the last-span target). `under '/admin' => sub { … }`?
The outer path fact inherits into the callback's observation scope
and the reducer concatenates: inner `get '/users'` resolves to path
`/admin/users`, as one witness-equivalence class. `<action>` retires.

**B — DBIC search-chain accumulation (Part 5c customer).**
dbic plugin registers a `where_clause` reducer claiming Expression
witnesses with `family: "dbic.predicates"`. Each `->search({...})` /
`->search_rs({...})` call appends its literal hash's keys + values
to a list-valued fact. `cursor_context.rs` at `->find({...})` or
`->first` queries the reducer for the accumulated predicate stack
and offers column completion narrowed by Parametric type_arg. Rename
on a column transports through every witness in the chain.

**C — Part 6's framework-aware resolver as a built-in reducer.**
Part 6's `TypeObservation` becomes the payload-less shape of a
`Witness { payload: InferredType(_) | Fact { family: "repr", ... } }`.
The framework-aware resolver is a reducer claiming those witnesses
and applying Part 6's rules. No longer a bespoke pipeline — one
reducer among many, with the framework context threaded in via a
reducer-specific side table.

**D — Rename / provenance transport.**
Every time a plugin derives a ref from a source (string literal →
method call, `has` → constructor hash key), it attaches a
`DerivationWitness { source: DerivedFrom(origin_ref), payload:
Derivation }` to the derived ref. The rename engine's transport step
walks these as a DAG — renaming the origin chases the edges. Replaces
the ad-hoc provenance tables mentioned in the ref-coverage-provenance
spec with the same primitive everything else uses.

**E — Sum-type narrowing (Part 5b).**
The `if ($r->{ok})` narrowing constraint is a `Witness { attachment:
Variable, payload: InferredType(narrowed), source: Builder("narrowing"),
span: if_body_span }`. The type-fold reducer prefers the narrowest-
span witness containing the query point. Absorbs the proposed
`narrowing_scope` field into a property of the span itself.

### HoTT / proof-theoretic framing

Witnesses are proof terms inhabiting typed judgments. The judgment's
subject is the `attachment`; the proposition is the `payload`; the
`source` is the proof's origin (rule name / plugin id / derivation
edge). A reducer is an eliminator — a recursor over an inductive
family of witnesses that projects to a concrete value. Plugins
registering reducers are defining their own elimination principles
for their fact families.

This is cleaner than making `InferredType` dependently-typed because
reduction is explicit rather than definitional: different consumers
can use different reducers over the same bag, and unhandled witness
kinds simply don't participate in a given query. The one quotient
we commit to is the `under` / path-concat equivalence — outline
entries are stable under route-presentation refactoring — and we
implement it as a reducer rule, not a typed equality.

Skipped: first-class identity types, univalent transport, path
induction. Derivation witnesses (the `DerivedFrom` source) carry
exactly the rename-transport information without needing `≡` to be
a type. The other HoTT-adjacent ideas either collapse to LWW or are
out of scope for a static analyzer.

### Phased implementation

| Phase | Work | Exit criterion |
|---|---|---|
| 0 | `Witness` struct + bag + attachment-indexed lookup on `FileAnalysis`. Serde / bincode round-trip. No consumers yet. | Empty-feature landing: builder emits zero witnesses, all existing tests pass, the bag is present and queryable. |
| 1 | Reducer trait + registry. Dispatch loop: given a query `(attachment, question)`, scan the bag, dispatch to claiming reducers, return fused value. Built-in `type_fold` reducer over `InferredType` payloads (subsumes the current closest-constraint-wins logic in `inferred_type`). | `inferred_type(var, point)` switched to go through the reducer for variable-attachment witnesses; all type-inference tests still pass. |
| 2 | Rhai plugin API: `receiver_facts` in `CallContext`, plugin can both read and emit `Witness` values from `on_method_call`. Plugins can register reducers via a top-level `reducers()` fn. | Synthetic test plugin round-trips a fact across three chain links and reduces to a fact map; no production consumers yet. |
| 3 | Deferred reducer emission: per-statement flush point, reducers whose `sufficient` predicate flips true inside a statement get `on_complete` called exactly once per attachment, EmitActions collected and applied. | Unit test: one `->get->name->to` chain, reducer's `on_complete` runs once, produces exactly one Handler regardless of chain order. |
| 4 | **Application A** — mojo-routes rewrite. Replace the `<action>` Handler with the route reducer's aggregated `<route>` emission. Keep MethodCallRef emission (gd on `'Users#list'` still jumps). | Demo pin test shows one `<route> GET /users → Users#list` per chain; no `<action>` entries anywhere. |
| 5 | **Application A cont'd** — `under` / `group` lexical binder + cross-variable chain split (`my $r1 = $r->get('/x'); $r1->to('Y#z')`). Facts inherit through assignment binding. | Pin tests cover both. |
| 6 | **Application C** — migrate Part 6's resolver to a witness reducer. Part 6's `TypeObservation` enum collapses into the witness bag. | Part 6's blessed+hashref demo works through the witness layer; resolver is reducer-dispatched. |
| 7 | **Applications B, D, E** — DBIC search accumulation, derivation-based rename transport, narrowing as scoped type witnesses. Each is additive, lands independently. | Per-application pin tests; no regression in rename / completion suites. |

### Interplay with the rest of the spec

- **Part 1 — Invocant mutations.** `HashKeyMutation` becomes a
  `Witness { attachment: HashKey, payload: Fact { family: "mutation",
  key: "rhs_type", value: … } }`. The mutation reducer unions over
  all writes for a (class, key) pair.
- **Part 2 — Hash key unions.** `HashKeyOwner::Union` becomes a
  derivation witness pattern — the Union is a reducer output over
  multiple owner witnesses on the same HashKey attachment.
- **Part 5a — Value-indexed returns.** `keyed_returns` is just a
  CallSite-attached witness family indexed by first-arg literal; the
  reducer projects to the matching return type.
- **Part 5c — Parametric types.** Application B above; the DBIC
  search-chain reducer is how Part 5c actually gets its column facts.
- **Part 6 — Observation IR.** Application C; Part 6 is reframed
  as a specific reducer, not a parallel pipeline.

### Non-goals

- **Full migration of existing structs on day one.** Old structures
  keep working; they expose "project to witnesses" adapters so
  reducer-based consumers can see them, but the storage doesn't
  have to move until a case benefits.
- **Cross-file witness transport.** A fact set that ends at a file
  boundary (partially-configured route returned from a sub for the
  caller to finish) is rare enough to punt; enrichment already
  covers the common cross-file cases via the existing structures.
- **Effect facts.** Whether a block mutates state, throws, or
  performs I/O — out of scope; the fact model is for static type/
  structure/derivation only.
- **Cycles in derivation.** The DAG assumption is load-bearing for
  transport. Detection + break on insertion, consistent with what
  the builder already does for sub-return delegation.

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
| 6 — Observation IR + framework-aware resolver | large; constraint bag + projection fn, migrate existing sites | blessed-vs-hashref refinement without flips; uniform bless-rep handling across Mojo/5.38-class/DBIC/legacy | ternary descent first |
| 6b — Arity multidispatch from procedural bodies | large; arity-gating conditional detector + per-branch return types | `sub name { $self->{k} unless @_; … return $self }` pairs type correctly | 6 |
| 7 — Fact witnesses (bag + reducers) | large; unified witness bag, reducer registry, plugin-registered fold semantics, then incremental migration of existing structs | cross-stack primitive for type beliefs, chain facts, derivation edges, narrowing, framework rules; enables route aggregation (App A), DBIC search chains (B), Part 6 as a reducer (C), provenance rename (D), sum-type narrowing (E) | none, but lands in phases (Phase 0–3 core, 4+ applications) |

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
