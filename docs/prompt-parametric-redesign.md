# Parametric type redesign — forward-thinking working doc

**Status:** working doc. Captures all design decisions from the
Phase 1 review for the redesign about to land on
`feat/parametric-resultset` (PR #35) and the next pillar that
follows.

Phase 1 of Part 5c (committed on the branch as
`Parametric { base: String, type_args: Vec<TypeArg> }`) carried
flat-shape reflexes — a `class_name()` / `hash_key_class()` accessor
pair on `InferredType`, a per-method search-family allowlist, an
`_open` emitter sibling, and a TypeArg enum that existed only to
shoehorn recursion into a flat slice. All of it is reflexive
complexity that vanishes under a flavor-typed redesign.

This doc is the spec for the redesign + the next architectural
pillar (receiver-relative return types) that subsumes arity dispatch
and per-method projection together.

## Section 1 — Phase 1 redesign (immediate)

### Variant shape

```rust
pub enum InferredType {
    // existing flat variants ...
    Parametric(ParametricType),
}

pub enum ParametricType {
    /// DBIC. `base` = the resolved resultset class (default
    /// `DBIx::Class::ResultSet`, or a discovered custom resultset
    /// class). `row` = the row class (where `add_columns`
    /// synthesizes its column accessors). Two distinct fields
    /// because the value carries dual identity — dispatch goes
    /// through `base`, hash-key arg owner is `row`.
    ///
    /// Pinned by *internal-shape* tests (alongside the external-
    /// behavior tests) so a refactor to a single-class encoding
    /// silently breaks RowOf at the projection site rather than
    /// silently returning the wrong dimension.
    ResultSet { base: String, row: String },

    /// `Wrapped<class, T>` — a class wrapping a single typed
    /// value. Covers Mojo::Promise, IO::Async::Future,
    /// Lazy::Eval, Maybe, Try::Tiny — anything that's "a class
    /// around an inner T."  Effect facts (when they land) attach
    /// to this shape regardless of `class`, so Promise<X> and
    /// Future<X> share their effect handling without a
    /// per-class trait.
    Wrapped { class: String, inner: Box<InferredType> },

    /// `ListOf<class?, T>` — homogeneous collection of T,
    /// optionally inside a wrapper class. Bare ArrayRef
    /// (`class = None`), Mojo::Collection
    /// (`class = "Mojo::Collection"`), DBIC `->all`
    /// (Tier 3 nested-hashkey work) all share this shape.
    /// Heterogeneous lists are sum-types territory (Part 5b).
    ListOf { class: Option<String>, element: Box<InferredType> },

    /// `HashRef[K]` / `HashRef[K, V]` — Type::Tiny constraint
    /// shape. No class wrapper today (no Perl idiom I know of
    /// types a hashref AND wraps it in a class); add `class:
    /// Option<String>` if one shows up.
    HashRef { key: Box<InferredType>, value: Option<Box<InferredType>> },

    // Type-level operators (lazy projections — reduced by
    // `ParametricProjectionReducer` in the bag, not at consumer
    // read-time):

    /// `RowOf<T>` — unwrap T's row dimension. `RowOf<ResultSet
    /// { base, row }>` reduces to `ClassName(row)`. For T's
    /// without a row dimension, reduces to `None`.
    RowOf(Box<InferredType>),
}
```

### Per-axis methods on `ParametricType`

Each flavor declares its policy. No fall-through `_ => …` arms on
`ParametricType` matches anywhere — the compiler must light up every
consumer when a variant is added. (See "No fall-through" rule
below.)

```rust
impl ParametricType {
    /// Class to consult for method dispatch. ResultSet returns
    /// `base`; Wrapped/Collection return their wrapper class;
    /// HashRef/ArrayRef without a class return None.
    pub fn class_name(&self) -> Option<&str>;

    /// Class to consult for direct `recv->{key}` hash-key access
    /// on this value. Distinct from `method_arg_owner` (which is
    /// for `recv->method({KEY => ...})` — different position,
    /// different semantics). ResultSet returns row (HRI shape,
    /// not currently supported but the field is right). Wrapped
    /// / Collection return None. HashRef returns None today
    /// (Tier 2 nested-hashkey work refines this).
    pub fn hash_key_class(&self) -> Option<&str>;

    /// Owner for hash-key args of `recv->method({KEY => ...})`.
    /// `Some(owner)` means "this flavor claims this method's
    /// args; emit unconditionally with this owner — the type
    /// is the gate." `None` means "this flavor doesn't claim,
    /// fall through to the strict-eq local-symbol path."
    pub fn method_arg_owner(&self, method: &str) -> Option<HashKeyOwner>;
}
```

### Bag-side projection reducer

A new reducer (`ParametricProjectionReducer`, NOT bolted onto
`FrameworkAwareTypeFold`) claims `InferredType` payloads where the
type is `Parametric(<operator>(...))` and reduces by evaluating the
operator's operand recursively. Returns the projected `InferredType`.

This is bag-side, not consumer-side. Why not consumer-side: we have
reducers for everything else; staying in the framework keeps the
flow uniform and lets operators chain
(`RowOf<RowOf<X>>` reduces in one query). Lazy across worklist
iterations — if the operator's operand isn't reducible yet
(cross-file enrichment hasn't typed it), the witness stays in the
bag and reduces later.

### Emitter

`extract_resultset_parametric` returns `Some(InferredType::Parametric
(ParametricType::ResultSet { base, row }))`. No more `Vec<TypeArg>`
shoehorning.

The plugin port (queued, see `prompt-dbic-as-plugin.md`) emits the
operators directly. Phase 1 redesign keeps emission in the builder
for `recv->resultset('Foo')`; everything else (find, search, all,
…) waits for the plugin.

### Consumer routing

`find_definition`'s hash-key-arg arm asks the flavor:

```rust
let cn = match invocant_type {
    Some(InferredType::Parametric(p)) => p.method_arg_owner(method)
        .and_then(|HashKeyOwner::Class(c)| Some(c)),  // strict-eq gate skipped
    _ => None,
}.or_else(|| receiver.class_name().map(str::to_string));
```

`emit_method_call_arg_keys` similarly: if invocant is Parametric,
ask the flavor for `method_arg_owner`. If `Some`, emit with that
owner unconditionally (flavor is the gate). If `None`, fall
through to the existing `Sub { package, name }` strict-eq path.

The `_open` emitter sibling collapses: it's just "what
`emit_call_arg_key_accesses` does when the flavor pins the owner."
One emitter, one gate question, flavor-driven.

### Drop list

- `TypeArg` enum — the flat-shape workaround dies. Each flavor
  carries its own data fields.
- `parametric_type_args()` accessor on `InferredType` — consumers
  match on the flavor and walk its specific fields.
- `_open` emitter sibling — flavor's `method_arg_owner` decides.
- The search-family allowlist (`if target_name in {search,
  search_rs, find}`) — the flavor's `method_arg_owner` is method-
  aware; ResultSet returns `Some` for those methods and None for
  `count` / `exists` / `delete` (which take filters or no args).

### EXTRACT_VERSION 23 → 24.

### No `_ => …` fall-through rule

**On every match against `ParametricType`, every variant is
explicitly handled.** No catchall arms. The Plugin escape hatch is
deferred (see below) precisely because Rust's exhaustiveness check
will force its addition correctly when it lands — and a `_ => …`
fall-through anywhere defeats that safety net.

The few sites that genuinely don't care which flavor (e.g. logging,
debug formatting) get an `as_str()` / `Display` impl that handles
each variant's rendering. They never `match _ => …`.

### Tests

- External behavior: 9 existing pins in
  `src/parametric_resultset_tests.rs` survive intact (assert on
  `find_definition` / `refs_to`). The redesign changes the encoding
  under them.
- Internal shape: NEW unit test asserting that the resultset call's
  bag-resolved type is exactly `Parametric(ResultSet { base:
  "DBIx::Class::ResultSet", row: "Foo" })`. Encoding-direction pin
  per the parameter-pinning concern.

## Section 2 — Receiver-relative return types (next pillar)

The Phase 1 redesign retires per-method projection tables for the
`recv->method(...) → some-type-expression-of-receiver` shape via
operators like `RowOf<Receiver>`. But the operator is *only* on the
receiver — emitted at the call site, plugin-typed.

A natural extension: **return types are expressions over Receiver,
Args, Self.** Static `return_type: Option<InferredType>` becomes
`return_type: ReturnExpr` where `ReturnExpr` admits placeholders
the bag substitutes at each call site.

```rust
pub enum ReturnExpr {
    /// A concrete type — same shape as today's
    /// `Option<InferredType>`.
    Concrete(InferredType),
    /// `Receiver` — substituted with the call site's receiver
    /// type at query time. `find` for DBIC declares
    /// `RowOf(Receiver)` once on the symbol; every call site
    /// gets receiver-correct projection without re-emitting.
    Receiver,
    /// Apply a parametric operator to a sub-expression.
    Operator(ParametricType /* with sub-exprs as ReturnExpr */),
    /// Union over arg-shape — subsumes arity dispatch.
    /// `{ args.is_empty() => T, _ => Self }` is the Mojo
    /// fluent-accessor shape.
    UnionOnArgs { branches: Vec<(ArgGuard, ReturnExpr)> },
}

pub enum ArgGuard {
    /// `args.is_empty()` — the 0-arg arm.
    Empty,
    /// `args.len() >= n` — at-least guard.
    AtLeast(u32),
    /// Exactly N args.
    Exact(u32),
    /// Default (catch-all) — last in the branch list.
    Any,
}
```

### What this subsumes

**Per-method projection.** DBIC's `find` declares
`return_type: ReturnExpr::Operator(RowOf(Receiver))` once on the
symbol. The bag at each call site substitutes Receiver with the
local receiver's type, evaluates the RowOf operator, returns the
projection. The plugin `on_method_call` emit hook goes away for
this case — it's all symbol-declarative.

**Arity dispatch.** Mojo `has 'name' => 'default';` synthesizes
TWO things today: a 0-arg getter (returns String) and a 1-arg
writer (returns Self). Today this is a per-arm `ArityReturn`
witness consumed by `FluentArityDispatch` — its own bespoke reducer
with its own observation variant. Under ReturnExpr:

```rust
return_type: ReturnExpr::UnionOnArgs {
    branches: vec![
        (ArgGuard::Empty, ReturnExpr::Concrete(String)),
        (ArgGuard::Any, ReturnExpr::Receiver),
    ],
}
```

One union, two branches. `FluentArityDispatch` retires —
`UnionOnArgs` evaluation is the same machinery that evaluates
`RowOf` and friends. Same reducer, same flow. The
arity-as-bolted-on observation variant + reducer + ArityBranch
enum (currently called out in `docs/d4-review-followups.md` as
"premature to generalize until a second guard kind appears") IS
that second case — and the generalization is a union over arg
shapes, not a `Vec<Guard>`.

### Why this matters

- One mechanism subsumes per-method projection AND arity dispatch.
- Future cases (truthiness dispatch, `wantarray` context, type-of-
  arg dispatch) all become new `ArgGuard` variants, not new
  reducers.
- The "method tables in core are evil" concern goes away
  *structurally* — the table is a `ReturnExpr` declared on the
  symbol, owned by whoever emits the symbol (plugin or builder).
- Symbol-declarative > expression-emission for stable knowledge
  about method semantics. Plugins emit fewer call-site witnesses.

### Cost

Adding `ReturnExpr` + the substitution machinery is a real lift —
probably 200-300 LOC + retiring the arity reducer's bespoke parts.
Buys: the entire arity dispatch family collapses, and the
Phase-1-redesign's call-site emitter for find/search/etc. moves to
symbol-declarative when the plugin port lands. **Net LOC negative**
once landed.

### Sequencing

1. Phase 1 redesign (Section 1 of this doc) — concrete flavors +
   call-site operators. **Up next, this branch.**
2. Plugin extraction (`docs/prompt-dbic-as-plugin.md`) — DBIC
   moves out of core, plugin emits operators at call sites.
3. ReturnExpr (this section) — symbol-declarative return types
   subsume call-site operator emission AND arity dispatch.
4. Migrate Mojo `has` / DBIC plugin's per-method emissions to
   `return_type: ReturnExpr::...`. Retire `ArityReturn` reducer
   and `FluentArityDispatch`'s bespoke parts.

## Section 3 — Open follow-ups

### Plugin escape hatch (`Plugin<id>(data)`)

Deferred per Rust exhaustiveness — adding it later forces every
match site to handle it (no silent skips). Likely shape:

```rust
pub enum ParametricType {
    // ... concrete + operator variants ...
    Plugin { plugin_id: u32, args: Vec<InferredType> },
}
```

`Vec<InferredType>` for args because we can't make assumptions
about plugin-defined arities; the plugin's reducer is responsible
for type-checking its own shape. Per-axis methods on
`ParametricType` route Plugin variants through a registry the
plugin populates at load time.

Adding this cleanly requires the plugin trait surface to grow a
parametric-axis trait family — natural to ship together with the
Plugin variant.

### Effect facts

`Wrapped<Class, T>` is the natural attachment point for effects.
`Promise<X>` carries IO; `Try<X>` carries exception possibility;
etc. Effects on Wrapped (regardless of class) compose with the
inner T's facts. This is queued on its own pillar — out of scope
for the immediate redesign, but the data shape is already correct.

### Cross-cuts to existing specs

- `docs/prompt-parametric-semantics.md` — supplanted by Section 1.
  Per-flavor semantics live as methods on `ParametricType`, not
  in a pluggable registry (until Plugin escape hatch lands).
- `docs/prompt-type-system-encoding.md` — its dual-class-axis
  problem is solved by per-flavor methods (each flavor's policy
  is its impl). The two-bag-attachments seam still queues for
  graph rework.
- `docs/prompt-type-is-the-gate.md` — Section 1's
  `method_arg_owner` returning `Some` when the flavor is the gate
  IS the type-is-the-gate generalization for this case.
- `docs/prompt-dbic-as-plugin.md` — emissions migrate to
  `Parametric(ResultSet { base, row })` etc. Per-method projection
  table moves from "in the plugin" to "expressed as `RowOf` /
  `ListOf<None, RowOf>` / etc. operators emitted by the plugin"
  in Phase 1, and to `return_type: ReturnExpr` declarations in
  Section 2.
- `docs/prompt-nested-hashkey.md` — Tier 1 sanity test red-pinned
  in `parametric_resultset_tests.rs` will go green when find's
  `RowOf(Receiver)` projection lands (Section 2 OR plugin port
  emitting `Parametric(RowOf(...))` at find call sites).

### What's NOT in scope

- The flavor-typed redesign doesn't model **bless-on-existing**
  (`bless $ref, "Foo"` mutates the ref's type). That's an effect
  fact on top of Wrapped or a per-variable mutation reducer
  (Part 1 residual).
- Heterogeneous lists / sum-typed elements — Part 5b territory.
- Cross-file ResultSet stub generation. The custom resultset_class
  discovery (red-pin test j) still queues with the DBIC plugin.

## "Type as data" — the philosophy

CLAUDE.md invariant #10: never special-case for a particular
shape. Phase 1 of Part 5c shipped with three special-case
reflexes (`hash_key_class()` accessor, `_open` emitter, search-
family allowlist) we then had to back out. The redesign's win is
that those reflexes become *unrepresentable*:

- Per-flavor methods carry policy on the flavor — no method-name
  allowlist in consumers.
- `RowOf` operator carries projection rules in the type — no
  per-method return-type table in core.
- `ReturnExpr::UnionOnArgs` (Section 2) carries arity dispatch in
  the type — no bespoke arity reducer.
- No `_ => …` fall-throughs on `ParametricType` matches — the
  compiler enforces "every flavor handled at every site."

Every shape-specific decision becomes a TYPE-LEVEL fact. The
type-system encoding spec (`prompt-type-system-encoding.md`)
gets to a stronger form: not just "wrong-axis reads
unrepresentable" but "shape-specific behavior unrepresentable
outside the flavor's own impl." The whole codebase eventually
converges on "match on the flavor, dispatch via its methods" —
no logic strewn across consumers.
