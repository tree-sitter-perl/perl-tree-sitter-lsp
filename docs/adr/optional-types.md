# ADR: Optional / Maybe types

`InferredType::Optional(Box<InferredType>)` (value-or-undef, Type::Tiny
`Maybe[T]`) and `InferredType::Undef` (the bottom element) are the
complement of flow narrowing (`flow-narrowing.md`): a function that
early-returns undef produces `Foo | undef`, which used to collapse to
`None`. The two features are halves of one story — `Optional` creates the
imprecision, a `defined`/`blessed` guard resolves it, the negative side
knows it's `Undef`.

Scoped to `Optional<T>` and a bottom `Undef`, **not** arbitrary unions:
`Foo | Bar` stays `None` until a motivating case lands. Both variants are
appended at the **END** of the enum for bincode variant-index stability
(bump `EXTRACT_VERSION`); exhaustive matches are confined to
`file_analysis.rs` and use `_ =>`, so the blast radius is small.

## The variants don't dispatch — that's the point

`class_name()` returns `None` for both `Optional` and `Undef`, so
`is_object()` is `false` and method dispatch finds nothing. An optional
is *not definitely* an instance and a known-`Undef` definitely isn't —
letting either dispatch as its inner would be the exact unsoundness these
variants exist to surface. The user narrows first; the narrowing yields a
concrete `ClassName` that *does* dispatch. `subsumes_narrowing`: a
concrete `self` subsumes an incoming `Optional` narrowing (narrowing
already happened); the reverse is `false` so a `defined` guard's
`Optional<T> → T` wins. `Undef` rides the discriminant fallback
(`(Undef, Undef) → true`, else `false`).

## The undef arm is a marker, not a lattice value

`undef` has no rvalue type to ride an `Edge`, and a private `Undef`
*return* type would leak into every `resolve_return_type` consumer. So an
undef arm — `return undef`, bare `return;`, an `undef` ternary branch —
is marked with a `Builder("undef_arm")` Fact on the
`SymbolReturnArm` / `BranchArm` attachment. `join_return_arms(value_arms,
has_undef)` then lifts `{T, undef}` to `Optional<T>`; value-arm conflict
(`Foo` vs `Bar`) still yields `None`; only-undef yields `None`.
`Undef` enters the lattice **only** as a narrowing witness (the negative
side of a `defined` guard), never from the arm join.

## Production and consumption

- **Production:** `return undef` and bare `return;` (the dominant
  `return unless …; return $x` idiom) → `Optional` via the arm marker;
  the ternary `$c ? T : undef` via the same marker on `BranchArm` (which
  keeps its **strict** arm agreement — `$c ? Foo : Bar` stays `None`, not
  the return join's loose object-subsumption); Type::Tiny `isa =>
  'Maybe[T]'` via `map_isa_to_type` recursing on the inner constraint.
- **Consumption:** `defined`/`blessed` strip `Optional<T> → T`
  (`flow-narrowing.md`); the `else`/`return if` negative narrows to
  `Undef`.

Provenance: an `Optional` return is tagged `optional_join` in its
`TypeProvenance::ReducerFold` evidence so `--dump-package` explains it.

## Residual

- **Empty-list `return ()` arm.** The ternary form (`$c ? Foo->new : ()`)
  is handled — `()` (a `stub_expression`) is marked an undef arm in the
  branch-arm emission, lifting `{T, ()}` to `Optional<T>`. The `return ()`
  statement form is not yet: the return-arm undef check (`is_undef_arm`)
  recognizes a bodyless `return;` and `return undef`, but not a
  `stub_expression` body. Same scalar-context coercion rationale as bare
  `return;`; extend `is_undef_arm` to cover `stub_expression`.
- **All-undef returns → `Undef`.** A sub whose every arm is `undef`
  (`sub f { return undef }`) types `None` today, not the definitive
  `Undef` — `join_return_arms` sees `arms=[] && has_undef` and falls
  through. Soundness gate: return `Undef` only when `undef_count ==
  total_arm_count`, because `arms=[] && has_undef` also covers "a value
  arm we couldn't type (its edge materialized to nothing → no witness)
  plus an undef arm", where the sub returns *something*. So it needs a
  total-arm count the fold doesn't track (untypeable arms leave no
  trace). Payoff: feeds the method-on-`Undef` (D1) and always-false-guard
  (D4) diagnostics.
- `SlotTypeFold` production ({T, undef} slot writes → `Optional`);
- the bareword Type::Tiny `Maybe[…]` constructor form (the
  `TypeConstraintOf` path, vs the quoted-string form that landed);
- diagnostics on unguarded `Optional` / known-`Undef` derefs:
  `docs/prompt-narrowing-diagnostics.md`.
