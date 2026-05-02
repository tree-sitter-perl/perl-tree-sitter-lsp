# Type Inference: Unifying Walk-Time and Bag-Time Into One Path

> **Status: design.** Motivating evidence from the
> `spike/sequence-witnesses` branch (May 2026). The spike validated
> the witness/reducer story for sequence types and, in doing so,
> made a duplication legible that's been quietly accreting since
> the worklist refactor: the walker has its own type-inference path
> that runs in parallel with the bag, and every new feature pays a
> tax to bridge the two. This doc proposes a staircase of refactors
> — each independently green-shipping — that collapses the parallel
> path into a single bag-mediated query.
>
> Cross-refs:
> - `docs/prompt-type-inference-spec.md` — the witness/reducer model.
> - `docs/prompt-type-inference-worklist-refactor.md` — the previous
>   refactor in this lineage; landed April 2026.
> - `CLAUDE.md` "Build pipeline phases" + "Worklist invariants" — the
>   live pipeline this builds on top of.

## TL;DR

The codebase has **two truths about types**: a walk-time eager
inference path (`infer_returned_value_type_seen`,
`infer_expression_type`, `Symbol.return_type` pinned at synthesis,
`TypeConstraint` pushed inline) and a bag-time path (witnesses +
reducers + `inferred_type_via_bag`). The bag was supposed to be the
single truth; in practice the walk-time path persists and every new
type feature accretes a `deferred_X` field, a `seed_X_into_return_types`
shim, or a plugin override to bridge the two.

**The unifying observation:** every `deferred_X` field is a
`WitnessAttachment` in disguise — a key into the bag that the walker
captures and the post-walk fold queries. Generalizing that one
field, then progressively migrating the walk-time eager path through
the same key, collapses the duplication. Walker only *observes*; bag
is the *only* truth; queries always go through
`bag_query(canonical_attachment(node))`.

This doc proposes the refactor as a four-step staircase (each step
ships green, each step delivers value, the work can stop at any
step). It is **orthogonal to sequence-typing** — sequences just made
the duplication legible. The staircase below is the right next thing
to ship; sequence-typing redoes cleanly on top of step 2 or step 4
in much less code than the spike took.

## Why — observations from the sequence spike

The spike added one new conceptual feature (sequence types: `push`
contributions, indexed reads, shape inference). Net diff: ~2300 added
lines. About **one third** of that was bridging the new emission to
the existing walk-time inference path. Specifically:

1. **Two `deferred_*` fields, both shaped identically.**
   `ReturnInfo.deferred_var: Option<(String, Point)>` predates the
   spike. The spike added `ReturnInfo.deferred_seq_access: Option<Span>`.
   Both are walk-time-captured keys for post-walk bag lookup. The only
   difference is which `WitnessAttachment` they encode. A third one
   (for method-chain receivers) is already inevitable — the next
   reducer that types something the walker can't will need its own
   field.

2. **Two `var_type_via_bag` / `seq_access_type_via_bag` wrappers**
   plus FA-side mirrors `inferred_type_via_bag` /
   `sequence_access_type_at`. Same shape, different `WitnessAttachment`,
   different closures wired into `ReducerQuery`. Generalizing them
   to one method that takes an attachment would have made the spike
   ~150 lines smaller.

3. **A new `seed_slot_return_getters_into_return_types` pass** added
   alongside `seed_plugin_overrides_into_return_types`. Both walk
   `self.symbols`, find a witness on `Symbol(sym_id)`, query its
   reduced type, and write `return_types[name] = t`. They differ
   in *which observation kind they look for* and *which provenance
   they record*. The plugin pass was already a special case; the
   spike added a second instance, which makes it a pattern. Two more
   passes (`propagate_via_delegation`, `propagate_via_self_method_tails`)
   already do morally the same thing through different code.

4. **The `_route` plugin override stayed alive.** It exists because
   real Mojolicious does
   `my $route = $self->add_child(...)->children->[-1]; return $route;`
   and the chain typer doesn't reliably see the slot's shape through
   a fluent-method receiver. The walker types each hop through eager
   inference (`receiver_type_for`, `resolve_invocant_class_tree`)
   which doesn't go through the bag — so the bag-known shape on
   `HashSlot{Route, "children"}` is invisible to the chain hop that
   needs it. The override exists *because the walker doesn't query
   the bag*. Wherever the walker does its own inference, the bag's
   answer is unreachable.

These are four symptoms of one underlying disease.

## The pattern: `deferred_X` is `WitnessAttachment` in disguise

Strip the names away from the two existing fields:

```rust
deferred_var:        (name, point)   →  Variable { name, scope_at(point), point }
deferred_seq_access: span            →  SequenceAccess(span)
```

Both are **a key into the bag**. Captured at walk time so the
post-walk fold can look up the type without re-walking the CST.
That's exactly what `WitnessAttachment` is.

The minimal generalization:

```rust
struct ReturnInfo {
    scope: ScopeId,
    eager_type: Option<InferredType>,
    deferred:   Option<DeferredType>,
    arity_branch: Option<ArityBranch>,
    span: Span,
}

enum DeferredType {
    Variable { name: String, point: Point },  // current var-defer logic
    Bag(WitnessAttachment),                   // catches everything else
}
```

`collect_return_arm_types` dispatches: variable defers go through
`var_type_via_bag` (which already walks the scope chain — the lone
remaining specialization); everything else goes through one generic
`bag_query(att)`. Adding the next deferred kind is "emit on
`WitnessAttachment::X`, point the deferred at it." Zero new fields,
zero new wrappers, zero new seed passes.

This is a real, ~50-line cleanup that ships green on its own and
removes a knob the codebase has been turning ad-hoc.

## The deeper problem: two truths

`deferred_*` is a symptom; the disease is that the walker has its
own parallel type-inference path. The walk-time path was the
original way; the bag came later and was supposed to subsume it
(per `docs/prompt-type-inference-spec.md`); the worklist refactor
landed reducer-driven query at *query* time but left build-time
inference largely procedural.

The procedural pieces still living in the walk:

- **`infer_expression_type`** — types literal/constructor
  expressions; consulted by visit-time decisions (RHS-of-assignment,
  return-arm classification, push-value typing, …).
- **`infer_returned_value_type_seen`** — the big walker-side
  return-type inferrer. Has cases for ternaries, `shift->method`
  shortcuts, scalar-variable lookups against `type_constraints`,
  `method_call_expression` chains via `resolve_invocant_class_tree`.
  Roughly 150 lines of cases, each parallel to a reducer that
  already exists.
- **`Symbol.return_type`** — pinned by framework synthesis (Mojo::Base
  fluent writer, Moo `has` accessor) and by walk-time return inference.
  The worklist `write_back_sub_return_types` overwrites it with
  bag-resolved values, but consumers still read the field directly
  (`sub_return_type`, `sub_return_type_local`, callers of these).
- **`TypeConstraint`** — pushed inline by visitors. The
  `populate_witness_bag` pass mirrors them as Variable witnesses,
  but the constraint vector is itself read by some legacy paths.

Every place where a walk-time path produces a type without going
through the bag is a place where the bag's answer is unreachable
*at that decision point*. The chain typer's blindness to slot
shapes is one such place. Plugin overrides exist to patch the
specific cases where this hurts most.

**Even the bag's own internals leak this duality** — the next
section explains how `ReducerQuery`'s closures let callers inject
non-bag answers into reducers, encoding "two truths" right in the
recursion path of the canonical query mechanism. That's the third
instance of the same problem: the cleanup isn't just deleting
walk-time inference, it's removing every interface that lets
non-bag answers re-enter the system.

## The closure leak: a third instance of the same problem

`ReducerQuery` carries two `Option<&dyn Fn>` closures:

```rust
pub return_of:       Option<&'a dyn Fn(&ReturnOfKey) -> Option<InferredType>>,
pub array_shape_of:  Option<&'a dyn Fn(&ArrayId)     -> Option<ArrayShape>>,
```

Reducers call them when they need to look up "another sub's return
type" or "another container's shape." Callers install whatever
closure they want.

This is the same two-truths problem at a finer scale. **The
closures let the caller inject answers the bag doesn't necessarily
agree with.** The reducer doesn't ask the bag — it asks whatever
the caller chose to install. If the canonical truth is supposed to
be the bag, allowing the caller to short-circuit it is exactly the
wrong invariant.

Concrete evidence:

- `return_of`'s historical justification was that "what does sub X
  return" had multiple answer sources: the bag, `Symbol.return_type`,
  `imported_return_types`. The closure let each call site decide
  which to consult. That's just two-truths, projected through the
  reducer interface.
- `array_shape_of` (added in the spike) has *no* such justification.
  Every installed instance does the same thing — query the bag for
  `Container(id)` through the registry. It's pure ceremony with the
  shape of "this could have been an override but isn't."

The deeper smell: a reducer that recurses through a caller-supplied
closure is fundamentally saying "I trust whoever called me to give
me consistent recursive answers." That trust isn't earned anywhere.
Today's call sites all happen to install bag-consistent closures,
but nothing in the type system enforces it. A future bug or
well-meaning override could install a closure that returns a type
disagreeing with what the bag would say, and the reducer would
silently propagate the disagreement.

The fix collapses cleanly into Step 3 of the staircase: replace
the closures with a `ReducerContext` handle that exposes only
bag-mediated queries. The reducer can recurse, but only through the
canonical path.

```rust
fn reduce(
    &self,
    ws: &[&Witness],
    q: &ReducerQuery,
    ctx: &ReducerContext,
) -> ReducedValue;

impl ReducerContext {
    fn query(&self, att: &WitnessAttachment) -> ReducedValue;
    // sugar:
    fn shape_of(&self, id: &ArrayId) -> Option<ArrayShape>;
    fn sub_return_type(&self, name: &str) -> Option<InferredType>;
}
```

Both closure fields die. Termination has the same guarantees as
today — bag is monotone, lattice is finite, reducers are stateless.
Nothing in the new shape allows a caller to inject a non-canonical
answer.

This is *the* enforcement mechanism for "the bag is canonical": once
the reducer's only recursion path is `ctx.query`, every type query
in the system is bag-mediated by construction, not by convention.

## The unifying claim

**Every expression node has one canonical bag attachment. The
walker only emits witnesses; the bag is the only truth; every type
query goes through `bag_query(canonical_attachment(node))`.**

```rust
fn canonical_attachment(node: Node) -> Option<WitnessAttachment> {
    match node.kind() {
        "scalar"                      => Variable { name, scope_at(point), .. },
        "array_element_expression"    => SequenceAccess(span),
        "method_call_expression"      => Expression(refidx_of(node)),
        "function_call_expression"
      | "ambiguous_function_call_expression"
                                      => Expression(refidx_of(node)),
        "anonymous_array_expression"  => Container(Anonymous { origin: span }),
        "anonymous_hash_expression"   => /* future: Container(...) */,
        "string_literal" | "number" | "interpolated_string_literal"
                                      => /* eager — closed under syntax */,
        // …
    }
}

fn type_of(node: Node) -> Option<InferredType> {
    bag_query(canonical_attachment(node)?)
}
```

Then `infer_returned_value_type_seen`'s 150-line case-on-kind
match collapses to `type_of(child)`. The cases that recursed into
chains (`method_call_expression` → resolve invocant class → look
up method's return) become trivial: chain hops already have
Expression(refidx) attachments seeded by `populate_witness_bag`'s
method-call mirror; the bag answers.

Walker's job shrinks to: emit witnesses observing what's in the CST.
Nothing more.

## What this would let us delete

Concrete estimate, post-full-unification:

| Deletes | Lines |
|---|---|
| `infer_returned_value_type` + `infer_returned_value_type_seen` | ~150 |
| `var_type_via_bag` + `seq_access_type_via_bag` + FA mirrors | ~80 |
| `seed_plugin_overrides_into_return_types` + `seed_slot_return_getters_into_return_types` + the two propagate_via_* passes (collapse to one generic pass) | ~250 |
| `infer_expression_type`'s scalar-lookup / call-name / method-call cases | ~60 |
| Misc `deferred_*` plumbing | ~30 |
| `return_of` + `array_shape_of` closures on `ReducerQuery`; every closure-construction site (per-FA-method, per-build-pass) | ~80 |
| `apply_type_overrides` (becomes plain witness emission) | ~40 |
| **Net** | **~690** |

Replaced with: `canonical_attachment(node)` (~40 lines, one match),
`type_of(node)` (~10 lines), one generic seed pass (~50 lines).
~470-line net reduction, ignoring whatever the migration adds in
test-side adjustments.

## Migration: a four-step staircase

Each step ships green. Each step pays its own way. Stopping at any
step still leaves a cleaner codebase than today.

### Step 1 — Generalize `deferred_*`. **Tiny. ~50-line diff.**

Add `enum DeferredType { Variable {..}, Bag(WitnessAttachment) }`.
Collapse the two fields into one. `collect_return_arm_types`
dispatches: variable defers go through the existing scope-walking
helper; bag defers go through the registry directly.

No semantic change. Removes one field, removes the duplicate
build-side wrapper, makes the next deferred kind cost zero.

This is the minimum-viable cleanup that legibly extracts the
pattern. Anyone reading the code afterward sees "we capture
attachments at walk time" instead of "we have N ad-hoc deferred
fields."

### Step 2 — Generic seed pass. **Small. ~150-line diff (mostly deletions).**

Replace `seed_plugin_overrides_into_return_types`,
`seed_slot_return_getters_into_return_types`, `propagate_via_delegation`,
and `propagate_via_self_method_tails` with one
`seed_return_types_from_bag` that walks every Symbol with a
sub/method kind, queries `bag.for_attachment(Symbol(sym.id))`
through the registry, writes the result to `return_types`, and
records provenance based on which reducer produced the answer.

The reducers already exist (PluginOverride, NamedSubReturn,
SlotReturn, Delegation, SelfMethodTail, FluentArityDispatch). They
already have priority via `WitnessSource::priority()` and registry
order. A single seed pass that just queries each Symbol gives them
the correct precedence for free. The four bespoke passes were each
re-implementing one reducer's query in seed-pass form.

After this step, adding a "this kind of sub gets its return type
from a witness" feature is exactly "write a reducer." No new seed
pass.

### Step 3 — Symbol.return_type becomes a cache; closures die. **Medium. Touches lots of consumers.**

Two coupled changes that together enforce "the bag is canonical."

**3a. `Symbol.return_type` reads go through the bag.** Audit every
consumer of `Symbol.return_type` / `SymbolDetail::Sub.return_type`
/ `sub_return_type` / `sub_return_type_local`. Replace with
`bag_query(WitnessAttachment::Symbol(sym_id))` going through the
registry. The field becomes a cache that the worklist refreshes
(or goes away — depending on cost-of-recomputation analysis).

`apply_type_overrides` (synthesis-time direct-write of plugin
override types) becomes "emit a Plugin-priority Symbol witness" and
nothing else. The plugin override no longer needs a special
"don't clobber me" branch in the fold; its high-priority witness
naturally dominates via the reducer registry.

Framework synthesis's direct `return_type:` pin in the SymbolDetail
constructor goes away too — replaced with witness emissions
(SlotReturnGetter for getters, fluent-class observations for
writers, isa-projected observations for typed Moo accessors).

**3b. `ReducerQuery` closures collapse into a `ReducerContext`
handle.** Drop both `return_of` and `array_shape_of` from
`ReducerQuery`. Add a `ReducerContext` that the registry passes to
each reducer:

```rust
pub fn reduce(
    &self,
    ws: &[&Witness],
    q: &ReducerQuery,
    ctx: &ReducerContext,
) -> ReducedValue;

impl ReducerContext {
    pub fn query(&self, att: &WitnessAttachment) -> ReducedValue;
    pub fn shape_of(&self, id: &ArrayId) -> Option<ArrayShape>;
    pub fn sub_return_type(&self, name: &str) -> Option<InferredType>;
}
```

`ctx.query` recurses into the registry on the same bag. The two
sugar methods are convenience wrappers — they call `ctx.query`
under the hood and project the result. Reducers can recursively
query, but only through bag-mediated paths.

Removes the canonicality leak called out above: no caller can
install a closure that returns answers the bag wouldn't. The
"bag is canonical" invariant is enforced *by construction*, not by
convention.

3a and 3b ship together because both touch the build-time return-
type query path. Splitting them would leave a window where the
closures still exist but are wired to bag-only resolution — extra
churn for no benefit.

### Step 4 — Walker only observes. **Largest. The full unification.**

`infer_returned_value_type_seen`, `infer_expression_type`'s
non-literal cases, `receiver_type_for`'s scalar-lookup branch — all
delete. The walker emits witnesses for every meaningful expression
as it visits; it never returns a type during the walk.

ReturnInfo stops carrying `eager_type`. It stores the return
expression's `(span, kind)` (or just node identity); the post-walk
fold computes its `canonical_attachment` and queries.

`TypeConstraint` becomes a witness emitter, not a parallel data
structure. (Or stays as a cache, like Symbol.return_type. Same
question.)

After step 4, the only walk-time inference is for *closed-under-syntax*
expressions: a string literal is a String, a number is Numeric,
`{...}` is HashRef. Anything that depends on resolving a name or
following a chain goes through the bag.

## How sequence-typing fits in

The spike is not the production design. It validated the
witness/reducer model for sequence types — that work is real and
the design (`InferredType::Sequence(ArrayShape)`, ShapeReducer,
ElementAtReducer, SlotReturnReducer, the `array_shape_of` closure
threading) is correct. But the *integration* of sequence emission
with the walk-time inference path required two new bridges
(`deferred_seq_access`, `seed_slot_return_getters_into_return_types`)
that this doc identifies as the pattern to retire.

**Recommended order:**

1. Land step 1 and step 2 of this refactor on a separate branch.
2. Re-derive the sequence-typing spike on top, on a fresh branch.
   With step 2 in place, the seed pass disappears (the
   `SlotReturnReducer` already does the work; the new generic seed
   pass calls it). The `deferred_seq_access` field disappears (it
   becomes `DeferredType::Bag(SequenceAccess(span))`). The two
   wrappers (`seq_access_type_via_bag` + `sequence_access_type_at`)
   collapse into bag queries against `SequenceAccess(span)` going
   through the registry.

   Estimated post-refactor sequence-typing diff: ~1100 lines vs the
   spike's ~2300. Same functionality, none of the bridging shims.

3. Optionally land step 3 and 4 either before or after sequences.
   Sequences don't depend on them; the unification is for code
   quality and for the next type feature to land cheaply.

Step 4 unblocks the Mojo `_route` chain *without* an override: when
every chain hop queries the bag for invocant types, the slot-shape
known on `HashSlot{Route, "children"}` is reachable, and the
`add_child(...)->children->[-1]` chain types as Route end-to-end.
That's the test that currently fails on the spike branch and that
keeps the override alive — and it falls out of the unification,
not out of more sequence work.

## What this won't solve

Worth being clear-eyed about. The unification removes duplication
and makes the bag the single truth. It does **not** add new
inference power. Three known design surfaces remain genuinely
unsolved:

1. **Cross-method / cross-file mutation effects.** A method that
   pushes onto `$self->{kids}` records the contribution in the bag.
   A *caller* in a different file calling that method has no way
   to see "this call mutates self.kids" because mutations aren't a
   return value. Fixing this is the **callee-effect signatures**
   work — a sub's signature gets, alongside its return type, a
   list of effects ("pushes type T onto arg-N's container"). Call
   sites synthesize contributions on the resolved arg's container.
   Composes cleanly with the rest of the unification (just another
   witness emitted at call sites), but it's its own design surface
   and its own implementation effort.

2. **`wantarray` / context-dispatched returns.** A sub returning
   different things in scalar vs list context. Same arity-axis
   trick as `FluentArityDispatch`, but with a context dimension.
   Mechanical to add once the unification is in place; just a new
   reducer.

3. **Aliasing through scalars across function boundaries.**
   `my $r = \@x; foo($r)` — `$r` carries the array's identity, but
   when it crosses into `foo`, callee-side contributions emit
   against `foo`'s parameter id, not `@x`'s id. Effect signatures
   solve some of this; full aliasing analysis is way out of scope.

## Acceptance criteria, per step

- **Step 1.** All existing tests green. `ReturnInfo` has one
  `deferred` field; the two old fields are gone. Adding a new
  deferred kind requires touching only `WitnessAttachment` and the
  emission point.

- **Step 2.** All existing tests green. Four seed/propagate passes
  collapse to one. Provenance per Symbol is recorded by which
  reducer answered. Diff is mostly deletions.

- **Step 3.** All existing tests green. `Symbol.return_type` reads
  go through the bag. `apply_type_overrides` is deleted (overrides
  are just high-priority witnesses). Framework synthesis emits
  witnesses; doesn't pin `return_type` directly. **`ReducerQuery`
  no longer carries `return_of` / `array_shape_of` closures**;
  reducers receive a `ReducerContext` handle and recurse via
  `ctx.query`. No code path can answer a type query through anything
  other than the bag.

- **Step 4.** All existing tests green. `infer_returned_value_type_seen`
  and `infer_expression_type`'s non-literal cases are deleted. The
  Mojo `_route` chain types end-to-end without an override (the
  `mojo-routes.rhai` `overrides()` returns `[]`).

- **Sequence-typing redo (independent).** All sequence tests from
  the `spike/sequence-witnesses` branch port to the new foundation
  with substantially less code (target: ≤ 60% of spike's diff).

## Closing note

The pattern this doc names — "every `deferred_X` field is a bag
attachment in disguise" — is a strong indicator the build-time
inference path is one refactor away from collapsing into the bag
entirely. That's the right next milestone after the worklist
refactor. The sequence-typing spike that surfaced it is preserved
as design rationale and validation; its code is throwaway pending
the cleaner foundation.
