# Optional / Maybe types — design

**Status: Phase 1 implementing.** The complement of flow narrowing
(`docs/prompt-flow-narrowing.md` §"Sum / optional types"): a function
that early-returns undef produces `Foo | undef`, which today collapses
to `None` (arm disagreement). `Optional(Box<InferredType>)` captures it,
and `defined $x` / `blessed $x` narrowing strips it back to `T` — the
two features are two halves of one story.

Scoped to **`Optional<T>` (value-or-undef), not arbitrary unions.**
`Foo | Bar` stays `None` until a motivating case lands. `Maybe<T>` is
the dominant Perl idiom and maps 1:1 onto Type::Tiny `Maybe[T]` /
`Optional[T]`.

## The lattice variant

`InferredType::Optional(Box<InferredType>)`, appended at the **END** of
the enum (`file_analysis.rs`) for bincode variant-index stability — same
discipline as `Sequence` / `TypeConstraintOf` / `BrandedRoute` /
`HashWithKeys`. Bump `EXTRACT_VERSION`.

Blast radius is small: exhaustive (`_`-less) matches on `InferredType`
are confined to `file_analysis.rs` and terminate in `_ =>` wildcards
(unlike `ParametricType`, which forbids `_`). So adding the variant does
not cascade compile errors.

**`class_name()` → `None` for `Optional(_)`** (no arm; falls through the
existing `_`). An `Optional<Foo>` is *not definitely* a `Foo` — letting
it dispatch as `Foo` is the exact unsoundness the variant exists to
surface. `is_object()` (`= class_name().is_some()`) is `false`
automatically; `hash_key_class()` → `None` automatically. The user is
meant to narrow (`defined`/`blessed`) first; the narrowing yields
`ClassName(Foo)`, which *does* dispatch. Dispatch-through-optional is a
deliberate non-feature.

## The join — where {T, undef} becomes Optional<T>

The shared join is `resolve_return_type` (`file_analysis.rs`). Today undef
arms are **invisible** to it: a `return undef` arm pushes an
`Edge(Expr(undef_span))` that materializes to nothing (the `undef`
rvalue has no `Expr` payload — `expr_payload` falls to `_ => None`), and
a bare `return;` pushes no arm at all. So `{Foo, undef}` looks like just
`{Foo}` → `Foo`, losing the optionality.

**Fix — make the undef arm legible without adding an `Undef` lattice
variant:** mark it with a witness **source tag** `Builder("undef_arm")`
on the existing `SymbolReturnArm(sid)` / `BranchArm(span)` attachment.
`publish_return_arm_witnesses` detects the bare-`return;` (`None` body)
and `undef_expression` body cases and pushes the marker. The fold then
counts undef-marker witnesses separately from typed arms and:

- **≥1 undef-arm AND value-arms resolve to a single non-optional `T`** →
  `Optional(Box::new(T))`.
- **no undef-arm** → unchanged (existing behavior; existing tests are a
  no-op for the new rule).
- **value-arms genuinely conflict** (`Foo` vs `Bar`) → still `None` (no
  arbitrary union this phase).
- **only undef-arms** → `None` (no `Unknown` to wrap; a sub that only
  `return undef` has no useful value type).

Keeping undef out of the lattice (only `Optional` enters) is the
load-bearing choice: a private `Undef` variant would leak into every
consumer.

## `subsumes_narrowing`

- `(Optional(a), Optional(b)) => a.subsumes_narrowing(b)` — recurse.
- `(a, Optional(b)) => a.subsumes_narrowing(b)` — a concrete `self` is at
  least as specific as an optional narrowing (narrowing already
  happened).
- `(Optional(_), concrete)` — default `false`: the optional does NOT
  subsume a concrete narrowing, so a `defined`/`blessed` guard's
  `Optional<T> → T` refinement wins. (No arm; the discriminant fallback
  is already `false`.)

## Phased plan

**Phase 1 (this doc's target):** `sub f { return undef unless $x;
return Foo->new }` ⇒ `Optional<ClassName(Foo)>`, all existing tests
green.
1. Append `Optional(Box<InferredType>)`; bump `EXTRACT_VERSION`.
2. `publish_return_arm_witnesses`: push `Builder("undef_arm")` markers
   for bare `return;` and `return undef`.
3. `resolve_return_type`: the partition rule above (thread the undef-arm
   signal in).
4. `SymbolReturnArmFold`: collect undef-marker count, pass to the join.
5. `subsumes_narrowing`: the two Optional arms.
6. `class_name()` untouched (Optional → `None`).
7. Tests: the target sub; existing arm-fold tests stay green.

**Phase 2 — generalize + light up narrowing:**
- Route `BranchArmFold` (hand-rolled ternary fold) through the shared
  join so `$c ? Foo->new : undef` ⇒ `Optional<Foo>` (DRY).
- `SlotTypeFold` honors Optional (mostly free — undef skips the
  class-conflict guard, flows into the join).
- `InferredType::optional_inner()` accessor; wire the **`defined` /
  `blessed`** guards (today unrecognized) to narrow `Optional<T> → T`.
  This needs `GuardFact.narrowed` to become a `NarrowOp { To(InferredType),
  Defined }` enum — `Defined` is a *query* (read the subject's incoming
  type, strip `Optional`), not a constant type. Emit only when the
  incoming type is `Optional` (else `defined` is identity).

**Phase 3 — negatives + provenance + Type::Tiny:**
- Negative-polarity rows (`else` of `if (defined G)`, `return if defined
  G`) become expressible **for the `defined`/`blessed` family only** —
  the false arm is the undef side, a concrete element. Needs a bottom
  (`Undef`) element for the else witness. `isa`/`ref-eq` negatives still
  need real negation/union (parked).
- `TypeProvenance::ReducerFold { reducer: "optional_join" }` for
  `--dump-package`.
- Map Type::Tiny `Maybe[T]` / `Optional[T]` onto `Optional(Box<T>)` at
  the `type_constraint_names` seam.

## Sequencing wrinkle (Phase 2+)

A `defined` narrowing whose subject's `Optional` comes from a sub return
resolved during `fold_to_fixed_point` (phase 6) can't be computed in the
live walk — the return type isn't in the bag yet. The `defined` emit for
that case moves to a post-fold pass. The `my ($x) = @_`-typed and
assignment-typed subjects work in the live walk.
