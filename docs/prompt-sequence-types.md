# Sequence Types — Phased Spec

> **Status: design.** Lands *after*
> `docs/prompt-type-inference-unification.md` Step 3 (closures
> retired, `ReducerContext` in place, `Symbol.return_type`
> bag-mediated, framework synthesis emits witnesses).
>
> This spec assumes the unification has happened. It deliberately
> keeps nothing from the `spike/sequence-witnesses` branch —
> implementation starts fresh on the cleaner foundation. The spike
> validated the design; this spec is the build plan.
>
> Cross-refs:
> - `docs/prompt-type-inference-unification.md` — prerequisite refactor.
> - `docs/prompt-type-inference-spec.md` — witness/reducer model.
> - `CLAUDE.md` "Build pipeline phases" + "Worklist invariants".

## TL;DR

Add a single new `InferredType` variant (`Sequence(ArrayShape)`)
and the bag machinery to populate and query it. The walker observes
contributions to positional containers; reducers fold those into
shape information; access expressions project shapes back to
element types. No carrier distinction (Array vs ArrayRef collapse
into one variant), no deferred build-side fields, no closure
threading — those concerns evaporate in the post-unification world.

The spec phases the work in five steps. Each step ships green and
adds capability; later steps don't reshuffle earlier ones. Phase 1
is a self-contained bag refactor (no walker changes). Phase 2 is
walker emission. Phase 3 is framework integration. Phases 4–5 are
follow-on capability (pipelines, escape analysis); each is a
separate design pass and can be deferred indefinitely.

## What the spike validated

The spike's bag-side design was right. Carry forward:

- **One type variant** for positional containers: `Sequence(ArrayShape)`.
  Drop the carrier distinction (Array vs ArrayRef). Perl's
  list-vs-scalar context is a property of the *expression's
  evaluation context*, not the contained data.
- **Five-variant shape lattice**: `Empty | Homogeneous(T) | Tuple([T..])
  | CycleTuple([T..K]) | Heterogeneous`. Ordered by informativeness;
  classifier specializes when contributions admit precise structure,
  generalizes when they don't.
- **Identity at emission time**: `ArrayId::{Lexical, HashSlot, Anonymous}`.
  Resolved by the walker; reducers never compute identity, only fold
  by attachment.
- **Two attachment kinds**: `Container(ArrayId)` for shape and
  `SequenceAccess(Span)` for read sites.
- **Three observation kinds**: `ArrayContribution { type, position }`,
  `ArrayReset`, `ArrayAccessAt { container, position }`.

What the spike got wrong, and the spec corrects:

- `Heterogeneous` carried a `Box<InferredType>` payload that was
  ~always `ArrayRef` sentinel. **Drop the payload.** `Heterogeneous`
  is unit; `element_at` returns `None`.
- The spike kept `InferredType::ArrayRef` alongside `Sequence(Empty)`
  as two ways to spell the same thing. **Drop `ArrayRef`.** Migrate
  consumers to `Sequence(Empty)` as the "we know it's array-shaped,
  contents unknown" answer.
- The spike reused `resolve_return_type`'s "Object subsumes HashRef"
  laxness for slot LUB and silently mis-classified `[User, Order]`.
  **Use a strict `unify_via_object_subsumption` helper.** Rename
  `resolve_return_type` to expose its semantics (it's not a generic
  LUB).
- Cycle detection requires length ≥ 4. Length 2 is indistinguishable
  from a 2-tuple.
- The spike added `array_shape_of` as a `ReducerQuery` closure. In
  the post-unification world this is **`ctx.shape_of(id)`** — a
  method on `ReducerContext`, not a caller-installed closure. No
  canonicality leak.

What gets sharper after unification (assumed available):

- No `deferred_seq_access` field on `ReturnInfo`. Returns through
  the generic `DeferredType::Bag(WitnessAttachment)` mechanism.
- No `seed_slot_return_getters_into_return_types`. The generic seed
  pass handles every Symbol-attached witness uniformly.
- No build-side / FA-side wrapper duplication. One query path:
  `bag_query(WitnessAttachment::SequenceAccess(span))` works the
  same at build time and query time.
- Framework synthesis emits `SlotReturnGetter` instead of pinning
  `return_type` directly — that's already the convention post-Step 3.

## Data model

```rust
pub enum InferredType {
    // … existing variants except ArrayRef, which is removed.
    Sequence(ArrayShape),
}

pub enum ArrayShape {
    Empty,
    Homogeneous(Box<InferredType>),
    Tuple(Vec<InferredType>),       // length ≤ TUPLE_LIMIT, mixed types
    CycleTuple(Vec<InferredType>),  // period K, requires ≥ 2 full cycles
    Heterogeneous,                  // unit; no useful element type
}

const TUPLE_LIMIT: usize = 6;

impl ArrayShape {
    pub fn element_at(&self, index: i32) -> Option<InferredType>;
    pub fn iter_element(&self) -> Option<InferredType>;
}

pub enum ArrayId {
    Lexical { scope: ScopeId, name: String },
    HashSlot { container: String, key: String },
    Anonymous { origin: Span },
}

pub enum ContributionPosition {
    Index(i32),
    Tail,
    Head,
    Iter,
    Unknown,
}

pub enum AccessPosition {
    Index(i32),
    Iter,
    Splat,
}
```

`element_at` projects:
- `Empty | Heterogeneous` → `None`
- `Homogeneous(t)` → `Some(t)` for any index
- `Tuple(slots)` → bounds-checked indexing, supports negative
- `CycleTuple(period)` → modular indexing

`iter_element` returns the LUB of all element positions, `None`
when shape is Empty or Heterogeneous.

## Witness vocabulary

Two new attachments:

```rust
pub enum WitnessAttachment {
    // … existing
    Container(ArrayId),
    SequenceAccess(Span),
}
```

Three new observations:

```rust
pub enum TypeObservation {
    // … existing
    ArrayContribution { contributed: InferredType, position: ContributionPosition },
    ArrayReset,
    ArrayAccessAt { container: ArrayId, position: AccessPosition },
    SlotReturnGetter { container: ArrayId },  // for framework synthesis
}
```

`ArrayContribution` and `ArrayReset` attach to `Container(_)`.
`ArrayAccessAt` attaches to `SequenceAccess(_)`. `SlotReturnGetter`
attaches to `Symbol(_)` for synthesized accessors.

## Reducers

Three new reducers, each stateless, each claiming a precise
attachment + observation pair.

### ShapeReducer

Claims: `Container(_)` carrying `ArrayContribution` or `ArrayReset`.

Folds contributions through the temporal-filter rule (drop
witnesses past query point) and the reset-generation rule (drop
contributions older than the latest reset before query point), then
classifies:

```
classify(contribs):
  if empty           → Empty
  if all-tail/iter   → Homogeneous (or Heterogeneous if types disagree)
  if pure-indexed:
    per-slot LUB
    if all slots agree              → Homogeneous
    if length ≥ 4, period 2 detected → CycleTuple
    if length ≤ TUPLE_LIMIT, dense   → Tuple
    else                            → Heterogeneous
  if mixed indexed + tail            → collapse to Homogeneous (LUB)
```

Cross-method ordering not honored — across subs, all generations
contribute (LUB), since static call-graph analysis is out of scope.

### ElementAtReducer

Claims: `SequenceAccess(_)` carrying `ArrayAccessAt`.

Reads the source container's shape via `ctx.shape_of(container)`,
projects per the access position. `Iter` returns
`shape.iter_element()`; `Index(N)` returns `shape.element_at(N)`;
`Splat` returns `Sequence(shape)` (sub-sequence preserves shape for
v1 — slice-narrowing is a follow-on).

### SlotReturnReducer

Claims: `Symbol(_)` carrying `SlotReturnGetter`.

For synthesized accessors that return `$self->{KEY}`. Reads
`ctx.shape_of(container)` and lifts the result back as
`Sequence(shape)`. `Empty` shape → no useful refinement → yields to
the framework's baked default (next reducer in priority).

Registry order: PluginOverride first (priority), then SlotReturn
(framework-aware), then ShapeReducer / ElementAtReducer
(attachment-disjoint), then the rest.

## Builder emission contract

The walker observes; reducers fold. Emission rules (each is one
visitor or visitor-fragment):

| CST shape | Emit |
|---|---|
| `push @TARGET, $a, $b, …` | `ArrayContribution { contributed: type_of(a), position: Tail }` per arg, on `Container(resolve_array_id(TARGET))` |
| `unshift @TARGET, …` | Same, with `position: Head` |
| `[a, b, c]` | `ArrayContribution { contributed: type_of(slot_n), position: Index(n) }` per slot, on `Container(Anonymous { origin: literal_span })` |
| `my @x = (a, b, c)` | Same per slot, on `Container(Lexical { scope, name: "@x" })` |
| `@x = ()` / `$obj->{k} = []` | `ArrayReset` on the LHS's container |
| `@x = (a, b, c)` (non-fresh) | `ArrayReset` + per-slot `ArrayContribution(Index(n))` |
| `$arr[N]` / `$obj->{k}->[N]` | `ArrayAccessAt { container, position: Index(N) }` on `SequenceAccess(node_span)` |
| `for my $e (@arr)` | `ArrayAccessAt { container, position: Iter }` on the iter var's `Variable` attachment |
| `@arr[A..B]` | `ArrayAccessAt { ..., position: Splat }` on `SequenceAccess(node_span)` |

`type_of(node)` is the post-unification canonical query:
`bag_query(canonical_attachment(node))`. No procedural walker-side
inference is needed for sequences specifically.

`resolve_array_id(node)`: structural recognizer for the LHS of a
push or assignment site. Cases:

- `array(varname X)` → `Lexical { current_scope, "@X" }`
- `array(varname (block (... EXPR)))` → recurse on EXPR (the `@{ EXPR }` form)
- `array(varname (scalar X))` → `None` (alias through a scalar; lossy by design — see Phase 5 note on aliasing)
- `container_variable(varname X)` → `Lexical { current_scope, "@X" }` (the access form, `$arr[N]`)
- `anonymous_array_expression` → `Anonymous { origin: span }`
- `hash_element_expression` (with $self/__PACKAGE__ receiver, or a bag-resolvable receiver class) → `HashSlot { container: class, key }`
- `method_call_expression` where the method is a known framework getter for class C → look up the getter's `SlotReturnGetter` witness; collapse to its container id
- everything else → `None` (drop emission; no guess)

The accessor → slot collapse is the load-bearing piece for
`$self->kids` and `$self->{kids}` referring to the same container.
After unification, the lookup is a single `ctx.query` on the
method's Symbol attachment.

## Framework integration

Mojo::Base / Moo `has 'KEY'` synthesis emits two witnesses per
attribute (post-Step 3, framework synthesis is witness-driven, not
field-pinning):

1. **Getter symbol** carries `SlotReturnGetter { container: HashSlot { class, "KEY" } }`.
   The reducer projects the slot's folded shape back as the getter's
   return type. With no contributions, the slot's `Empty` shape
   yields to a fallback baked default (e.g. a typed Moo `isa => 'Str'`
   would emit a low-priority `InferredType::String` witness too).

2. **Writer symbol** (Mojo::Base / Moo fluent setter) emits a
   `FluentClass(class)` observation on its Symbol attachment.
   Same pattern as today's plugin override flow, just witness-shaped.

Synthesized HashKeyDef entries for constructor-key completion are
unaffected — they're already a separate concern.

## Phasing

Each phase ships green. Each phase delivers user-visible value.
Later phases don't reshuffle earlier ones.

### Phase 1 — Lattice + reducers (bag-side only).

Add `Sequence`, `ArrayShape`, `ArrayId`, position enums.
Remove `InferredType::ArrayRef`; migrate consumers to
`Sequence(Empty)`. Rename `resolve_return_type` to expose its
semantics; add a strict `unify_via_object_subsumption` helper.
Add the three new attachments + observations + reducers. Register
reducers in priority order through the registry.

**No walker changes.** Tests are entirely hand-crafted witnesses;
they pin the lattice's classification rules (≥4 for cycle, strict
unification, temporal/reset filters) and the reducer projections.

Acceptance: 20-ish unit tests covering each shape variant, every
projection mode, the temporal filter, the reset-generation rule.
Full suite green.

Estimated diff: ~600 lines added, ~50 deleted (ArrayRef
migration). One file each: `file_analysis.rs` (data),
`witnesses.rs` (attachments + observations + reducers),
`witnesses_tests.rs` (tests).

### Phase 2 — Builder emission.

Add `resolve_array_id(node)` and the visit-time emission helpers
(push/unshift, anonymous literals, lexical inits, indexed reads,
resets, iter-var typing). Wire them into the dispatcher in
`visit()`.

**No framework integration.** Mojo `has` accessors continue to
return their baked defaults at this phase — they don't yet emit
`SlotReturnGetter`.

Acceptance: emission tests on real Perl source via `build_fa()`,
asserting the bag has the expected contributions / accesses /
resets for each shape. The key user-visible change at this phase is
that `my @users; push @users, User->new; …; $users[0]` types as
`User` end-to-end through `bag_query(SequenceAccess(span))` —
falls out of the canonical-attachment path with zero special-case
wiring (no `deferred_seq_access` field, no
`seq_access_type_via_bag`).

Estimated diff: ~300 lines added (emission helpers + dispatcher
wiring). No deletions; this phase is purely additive.

### Phase 3 — Framework integration.

Mojo::Base and Moo `has` synthesis emits `SlotReturnGetter` for
getters and `FluentClass` for writers. The `resolve_array_id`
helper learns the accessor → slot collapse via lookup of the
synthesized Symbol's witness.

Acceptance: a Mojo::Base class with `has 'kids'` and an
`add_child` that pushes onto `$self->{kids}` (or `$self->kids`)
makes `$self->kids->[-1]` resolve to the element type, with no
plugin override and no special-case wiring. The current
`mojolicious-routes::_route` override is provably retire-able for
the *single-file* case.

Estimated diff: ~150 lines added (synthesis emission + accessor
collapse), ~80 deleted (the `_route` override and any related
shims that become redundant).

### Phase 4 — Cross-file mutation effects (Regime 2).

Sub signatures gain mutation effects: alongside the return type, a
list of effects per parameter ("pushes type T at position Tail onto
arg-N's container," "resets arg-N's container," etc.). The walker
infers effects from the body — same machinery as return-type
inference, just claiming a different attachment.

Call sites read the callee's effect signature and synthesize
contributions on the resolved arg's container. Cross-file flow
goes through the existing module index — same channel return
types ride.

This is where `add_child` mutating `$self->children` from a
*different* method (or a *different file*) becomes visible. The
real-world `Mojolicious::Routes::Route::_route` chain types
end-to-end through this phase, eliminating the override entirely.

Acceptance: real Mojolicious source (under `~/.plenv/.../Mojolicious/Routes/Route.pm`)
parses such that `_route`'s return type folds to
`ClassName(Mojolicious::Routes::Route)` without a plugin override.

This is a real new design surface; it gets its own spec doc when
phase 3 lands. The shape and timing depend on what the unification
has finalized for cross-file effect propagation.

### Phase 5 — Pipeline reducers.

`@dst = map { BLOCK } @src` (and grep / sort / splat) emit a
`SequencePipeline { src, dst, kind, block }` observation on the
dst container. A new reducer pulls the src's shape and applies the
kind-specific transform (Map: replace element type with block's
return; Grep/Sort: preserve element type; Splat: union of src
element types). Composes with everything else.

Same machinery for `@all = (@a, @b)` concat — `Splat([a, b])` kind
on `dst`, reducer LUBs.

Estimated diff: ~250 lines, all additive. Tests cover the four
pipeline kinds against fold-known src shapes.

## Out of scope

These are real but separable design surfaces; each gets its own
spec when its turn comes:

- **`wantarray` / context-dispatched returns.** Same arity-axis
  trick, plus a context dimension. Mechanical to bolt on.
- **Tuple destructure for `my ($a, $b) = func()`.** Once Phase 5's
  pipeline reducers exist, list-context returns can be typed as
  `Sequence(Tuple([...]))`, and per-position scalar destructure
  consults the tuple shape. Modest extra emission rule.
- **Slice narrowing.** `@arr[0..2]` could project a sub-tuple from
  a Tuple shape. Phase 5+ extension to the splat reducer.
- **Aliasing through scalars across function boundaries.** Sharing
  `ArrayId` through a scalar parameter requires aliasing analysis
  beyond identity-at-emission-time. Effect signatures cover the
  common cases; full aliasing analysis is out of scope indefinitely.
- **Duck-tuple narrowing.** "I see different methods called on
  different positions; the type is whatever has all those methods."
  HoTT-y, defensible, deferred — no real-world Perl that the
  analyzer cares about needs it.

## Net cost

Through Phase 3 (the realistic ship-target before Phase 4's
cross-file work begins): **~1100 lines added**, ~130 lines deleted.
The spike's equivalent diff was ~2300 added, ~30 deleted; the
post-unification version is roughly half the size for the same
functionality.

The ratio holds because every spike-side bridging mechanism —
deferred fields, dual wrappers, seed passes, closure threading,
override fallbacks — evaporates in the unified world.

## Acceptance criteria, per phase

- **Phase 1.** Hand-crafted-witness tests covering: each shape
  variant; element-at projection on each; iter-element LUB; the
  temporal filter; the reset-generation rule; `Heterogeneous` is
  unit and yields `None` from `element_at`. `InferredType::ArrayRef`
  no longer exists in the codebase. `resolve_return_type` is
  renamed; strict `unify_via_object_subsumption` is the LUB primitive
  for shape inference.

- **Phase 2.** Real-Perl-source emission tests: anonymous literal,
  lexical literal init (Tuple), pure-push (Homogeneous), mixed
  init+push (collapses to Homogeneous), reset on `@x = ()`,
  indexed read at `$arr[0]` and `$arr[-1]`, iter var typed via
  `for my $e (@arr)`. Each test asserts both the bag's contents
  and the resulting type at the access site through `bag_query`.

- **Phase 3.** Mojo::Base `has 'kids'` getter return type folds to
  `Sequence(<shape>)` when the slot has contributions; falls back
  to a witness-emitted default when it doesn't. The
  Mojolicious::Routes::Route `_route` override is removed; the
  *intra-file* chain (synthetic Route fixture exercising
  `add_child` + `children->[-1]` within one parsed file) types
  end-to-end without it. The cross-file real-Mojo case may still
  need the override pending Phase 4.

- **Phase 4.** Real Mojolicious's `_route` types end-to-end with
  no override anywhere. Effect-signature inference is robust to
  recursion / cycles in the callee body (terminates in
  `MAX_FOLD_ITERATIONS`).

- **Phase 5.** `map`, `grep`, `sort`, splat / concat all type
  through the pipeline reducer. Block-return inference for
  `map { … }` is consistent with the rest of return-type inference
  (i.e., goes through the bag, not a special path).

## Closing note

The spike taught us the design; the unification clears the way
for the implementation to be small. This spec is the build plan
on the cleared ground. Each phase is independently shippable, each
delivers user-visible value, and the final shape — through Phase 3
or 5 — is roughly half the code the spike needed for the same
capability.
