# ADR: Flow-sensitive narrowing

A guard (`$x->isa('Foo')`, `ref($x) eq 'HASH'`, `return unless defined
$x`) refines a subject's type over the region it dominates, then the
type widens back at the region's exit. Lives in its own file,
`src/builder/narrowing.rs` — a child module of `builder` so its
`impl Builder` methods keep private-field access while staying the single
tree-sitter consumer (rule #1: `build()` drives it).

## It's an emission feature, not a query change

The query path already does flow-sensitive lookup: on a `Variable`
attachment with a point, `ReducerRegistry` returns the **narrowest-span
`InferredType` witness containing the point**, skips witnesses whose span
*starts after* the point (temporal), and **excludes** witnesses whose
span doesn't contain it. That exclusion *is* the un-narrowing — a
witness scoped to the guarded region stops containing points past the
region, so the wide (zero-extent) witness wins again. Monotone bag, no
retraction. So narrowing only had to **emit** span-extent witnesses;
"widen at block exit" is span arithmetic, not a deletion.

The one trap: narrowing does **not** route through `push_type_constraint`
(which zero-extents the witness, `span.start..span.start`). A narrowing
witness must carry the **full region span** so narrowest-span-wins picks
it inside and scoped-exclusion drops it outside. Source tag
`Builder("narrowing")`, emitted in the live walk so it survives
enrichment truncation.

## Soundness: truncate at the first disturbance

A span-extent witness would shadow a *later* reassignment's temporal
witness, so the region is truncated at the first op that could disturb
the subject (`first_subject_write` / `first_place_invalidation`).
Conservative by construction — it under-narrows, never lies. The bias is
load-bearing for the diagnostics built on top: they miss some real bugs
but never invent a false `Undef`/`Optional`.

## Subjects: variables and places, one keying

A guard subject is a variable (`$x`) or a **place** — a chain of constant
hash/array projections off a scalar root (`$self->{a}{b}`,
`cst::canonical_place_path`). Both are keyed by the subject's **source
spelling** on the same `Variable` attachment, so the reducer, scope-walk,
and narrow-filter are untouched — a place witness is just a span-scoped
slot witness. The place-vs-variable distinction lives in exactly two
spots: the truncation rule and the one `method_call_invocant_class` query
seam. Truncation generalizes to multi-hop as "an opaque use of any
**proper prefix** of the place (root or intermediate element) disturbs
it, unless that prefix is the base of a longer access (a read)."

Accessor places (`$self->name`) are out: an accessor isn't a stable slot
(it can return a different object per call, with side effects), so
soundness needs a stricter no-call-between-guard-and-use model.

## Polarity and the negative lattice

`GuardFact { subject, op, asserts_when_true }`; `op` is `NarrowOp::To(T)`
(`isa`/`ref-eq`) or `StripOptional` (`defined`/`blessed`). The region a
guard dominates either asserts the guard TRUE or FALSE;
`op_for_region(holds)` returns the fact's op when polarity matches, else
`op.negated()`. `To(_).negated()` is `None` ("not Foo" has no positive
lookup target → emit nothing, stay wide); `StripOptional.negated()` is
`To(Undef)`. So the `defined`-family negatives (`else` of `if (defined
$x)`, `return if defined $x`, `unless (defined $x)`) narrow to the bottom
`Undef`, lighting up the early-exit rows **for free** through the
existing polarity plumbing — only the `else`-block emit (`trailing_else`)
was new code. General negation stays parked (see `optional-types.md`).

## `defined`/`blessed` strip via a re-emittable fold pass

`StripOptional` can't resolve at walk time: the subject's `Optional`
often comes from a sub return that only converges during the fold. So
`recognize_*` records the guard (subject, region, and the guard's own
`query_point` — *before* the region, so the read sees the un-narrowed
type and the pass's own output can't feed back), and
`emit_defined_narrowing_witnesses` re-derives `Optional<T> → T` each fold
iteration (clear-and-emit on tag `defined_narrowing`).

## Residual

- elsif-chain cumulative negation (needs intersection across conditions);
- general `Not`/`Difference` negation — parked, no positive lookup
  target, no consumer value;
- accessor places.

Diagnostics this enables: `docs/prompt-narrowing-diagnostics.md`.
