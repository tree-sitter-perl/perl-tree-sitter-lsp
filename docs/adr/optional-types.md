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

## Strict typing, lenient receiver projection

Two projections, deliberately split:

- **`class_name()` stays strict** — `None` for both `Optional` and
  `Undef`. The fold's return-type math depends on this: a sub returning
  `Optional<Foo>` must keep *typing* as `Optional<Foo>` (else
  `{Optional<Foo>, Foo}` arm joins would collapse and lose the
  optionality). `subsumes_narrowing`: a concrete `self` subsumes an
  incoming `Optional` narrowing (narrowing already happened); the reverse
  is `false` so a `defined` guard's `Optional<T> → T` wins. `Undef` rides
  the discriminant fallback (`(Undef, Undef) → true`, else `false`).

- **`class_name_lenient()` peels `Optional`** — the consumer-side
  receiver projection that `method_call_invocant_class` (and thus
  goto-def / hover / references / rename / completion) uses. An
  `Optional<Foo>` receiver resolves its methods on `Foo` *without* a
  guard: type-silence on a half-written value reads as a broken IDE, not
  a hint, so navigation stays lenient. The "might be `undef`" warning is
  owned by a deref **diagnostic** (`docs/prompt-narrowing-diagnostics.md`,
  next round of the epic), not by a dead receiver. `Undef` still peels to
  nothing — you can't navigate the methods of *definitely* nothing.

The narrowing path is unchanged and still wins where it applies: a
`defined`/`blessed` guard yields a concrete `ClassName` that dispatches
strictly, ahead of the lenient peel.

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
  `Undef`. Every method-receiver feature (goto-def / hover / references /
  rename / completion) peels an unguarded `Optional<T>` to `T` via
  `class_name_lenient` — see "Strict typing, lenient receiver projection"
  above. The deref-safety diagnostic is the deferred other half
  (`docs/prompt-narrowing-diagnostics.md`).

Provenance: an `Optional` return is tagged `optional_join` in its
`TypeProvenance::ReducerFold` evidence so `--dump-package` explains it.

## Forward work

Production gaps (empty-list `return ()`, all-undef → `Undef`,
`SlotTypeFold`, the bareword `Maybe[…]` constructor form) live in
`docs/prompt-optional-types.md`. Diagnostics on unguarded `Optional` /
known-`Undef` derefs: `docs/prompt-narrowing-diagnostics.md`.
