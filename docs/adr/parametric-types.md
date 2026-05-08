# ADR: Parametric types — sealed flavor enum, per-axis policy

A DBIC `$schema->resultset('Foo')` value carries two classes — the
ResultSet base for method dispatch, the row class for hash-key
args. One value, two type axes. The analyzer needs both.

The flat-shape spike (`Parametric { base: String, type_args:
Vec<TypeArg> }` with a `class_name()` / `hash_key_class()` accessor
pair on `InferredType`) hit the same wall every flat encoding does:
consumers special-cased on data layout (`if base == "ResultSet" {
use type_args[0] }`), the dual-class question grew a method-name
allowlist (`if target_name in {search, find}`), cross-file gating
needed an `_open` emitter sibling. Each fix was a reflex against
the encoding, not a structural answer.

## Decisions worth keeping

### Sealed flavor enum, not flat shape

```rust
pub enum InferredType {
    // ... existing flat variants ...
    Parametric(ParametricType),
}

pub enum ParametricType {
    /// DBIC. `base` for dispatch, `row` for hash-key args.
    /// Two distinct fields because the duality is intrinsic.
    ResultSet { base: String, row: String },

    /// Type-level projection. `RowOf<ResultSet>` reduces to
    /// `ClassName(row)`. Lazy — the bag's projection reducer
    /// evaluates when consumed. Plugin emits `RowOf(receiver)`
    /// for `find` / `first` / `single` / etc. when the port lands.
    RowOf(Box<InferredType>),

    // Future: Wrapped { class, inner } for Promise/Future/Lazy,
    // ListOf { class?, element } for Mojo::Collection/ArrayRef,
    // HashRef { key, value? }, Plugin { id, args } escape hatch.
    // See docs/prompt-return-type-expressions.md.
}
```

`Box<dyn Trait>` (literal "abstract base class") loses serde —
trait objects don't roundtrip through bincode without `typetag`.
The sealed enum gives the same expressivity, full serde derives,
and exhaustiveness checking that catches missing-variant bugs at
compile time.

### Per-axis methods on the flavor, not on `InferredType`

Each flavor declares its policy in its own impl:

```rust
impl ParametricType {
    fn class_name(&self) -> Option<&str>;            // dispatch class
    fn hash_key_class(&self) -> Option<&str>;        // direct $x->{key}
    fn method_arg_owner(&self, m: &str) -> Option<HashKeyOwner>;  // recv->m({K=>...})
}
```

`ResultSet` returns `&base` for `class_name`, `&row` for
`hash_key_class`, and `Some(Class(row))` for `method_arg_owner`
when `m` is a row-keyed method (search/find/create/update/...) —
None otherwise. Future flavors carry their own policy inside
their own match arm. **No data-layout-special-casing on
`InferredType` accessors.** No method-name allowlist in
consumers.

### `RowOf` is a type, not a method-table entry

`find`'s return type isn't "ClassName(row)" baked at the call site
or hardcoded in a per-method table — it's `RowOf(receiver_type)`.
The bag-side `ParametricProjectionReducer` evaluates by recursing
into the operand. Composes: `ListOf<None, RowOf<Receiver>>` is the
shape for `->all` when it lands.

The "evil hardcoding list" of "method X returns Y" is structurally
impossible — core knows projection rules (`RowOf<ResultSet>` → row
class), not method tables. Plugins compose the rules.

### Cross-file owner fix: build emits, enrichment fills

`emit_method_call_arg_keys` runs at build time, where receiver
types resolvable only via cross-file lookup come up empty —
the builder doesn't carry `module_index`. The chain
`$c->sner_r->search({KEY})` (helper synth + cross-file row
class) is the canonical case.

Three paths in the emitter:
- **Parametric receiver claims the method.** Flavor's
  `method_arg_owner(method)` returns Some — emit unconditionally,
  the type is the gate.
- **Locally-typed non-Parametric receiver.** Strict-eq
  `has_hash_key_def` gate. Constructor keys via Mojo `has`, etc.
- **Chain receiver, type unresolvable at build.** Emit
  `HashKeyAccess { owner: None }` eagerly. `FileAnalysis::
  fix_chain_receiver_hash_key_owners` runs from
  `finalize_post_walk` (no module_index — fills in-file chain
  receivers via `call_ref_by_start` recursion) and from
  `enrich_imported_types_with_keys` (with module_index —
  fills cross-file). Both routes share the same routine; the
  `module_index` argument is the only difference. Idempotent —
  only None-owner refs are touched, so a second run is a no-op.

Same precedent as the cross-file invocant refresh: build emits
what it can, enrichment fills cross-file gaps. The base-count
machinery on `FileAnalysis` (already in place for plugin
namespaces and inheritance edges) makes the two-phase fill
work without re-emission churn.

### Match invariant: zero `_ => …` fall-throughs on `ParametricType`

Every match handles every variant explicitly. The compiler is the
safety net for the (deferred) `Plugin` escape hatch — when it
lands, every consumer lights up mechanically. A `_ => …` arm
defeats that, so we ban them by convention.

## Why recursive from day one

Considered `Vec<String>` vs recursive shape. Recursive won.

`HashRef[ArrayRef[Str]]` is already idiomatic Perl in Moo / Moose
constraint expressions. Locking the data model against it would
have been self-inflicted. Mojo::Promise<X> is a near-term motivator
(CRM has it in 10+ files). Migration cost from flat → recursive
later is real (~20 sites + every consumer that started threading
inner-string structure as encoded format); cost of recursive from
day one is ~10 LOC. Easy call.

`Box<InferredType>` serde works fine through bincode + zstd + JSON.
The `Class(String)` fast-path on `TypeArg` (a soft denormalization
that v1 carried) went away in v2 — flavors now carry their own
fields directly, no shoehorning into a `Vec<TypeArg>`.

## Why two class accessors per flavor

`ResultSet` answers two questions:

1. What class do this value's *methods* come from? base
   (`$rs->search`, `$rs->all`).
2. What class do this value's *hash-key args* belong to? row
   (the `add_columns`-synthesized columns).

If we only exposed one accessor, consumers route through it for
both questions and get the wrong answer for one. The first design
attempt had a `hash_key_lookup_class` helper with a hardcoded
`target_name in {search, find}` allowlist — wrong axis: the rule
isn't about the method name, it's about which dimension of the
type the consumer is reading. Push the rule onto the type, not
the consumer.

When a future flavor — `Wrapped<X>`, `ListOf<X>`, `Plugin<id>` —
wants different narrowing, it implements its own per-axis
methods. No core changes.

## Trade-offs

**Variant explosion is bounded.** Concrete Perl flavors are a
small set (DBIC, Mojo Promise, Mojo Collection, Type::Tiny
HashRef/ArrayRef, GraphQL types when we get there). Plugin
escape hatch handles the long tail. Don't ship a variant without
an emitter.

**Cache invalidation.** `EXTRACT_VERSION` bumped 23 → 24 for the
v2 redesign. Bumping is free; old blobs re-resolve lazily.

## Where this is going

The next pillar is receiver-relative return types: `return_type`
becomes a `ReturnExpr` admitting `Receiver` placeholders and
`UnionOnArgs` branches, so `find` declares `RowOf(Receiver)` once
on the symbol instead of every call site emitting it. Same shape
subsumes Mojo `has`'s arity dispatch. Spec:
`docs/prompt-return-type-expressions.md`. The DBIC port, nested-
hash-key tiers, and the deferred `Plugin` escape hatch all queue
behind it — the ROADMAP carries their order.

## Test discipline

External-behavior tests assert against `find_definition` /
`refs_to` / completion. They don't pattern-match on
`InferredType::Parametric(...)` internals; they survived the
v1 → v2 encoding change unchanged.

One internal-shape pin
(`parametric_resultset_carries_base_and_row`) deliberately
matches on the variant: it asserts the resultset call's witness
is `Parametric(ResultSet { base, row })` with both fields
populated. Without it, a refactor to a single-class encoding
silently breaks `RowOf` rather than tripping a test — external
tests can't catch that class of regression because they only
see the projection's output.

Pattern: behavior tests (most) + one shape pin per load-bearing
variant.
