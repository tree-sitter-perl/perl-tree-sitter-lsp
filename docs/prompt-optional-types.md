# Optional / Maybe types

**Landed.** Decision record: `docs/adr/optional-types.md`.

## Residual forward work

### Production gaps

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
- **`SlotTypeFold` production** — `{T, undef}` slot writes (`$self->{x} =
  $maybe`) → `Optional` on the slot.
- **Bareword Type::Tiny `Maybe[…]` constructor form** — the
  `TypeConstraintOf` path; the quoted-string `isa => 'Maybe[T]'` form
  landed, the bareword `isa => Maybe[Int]` form goes through the separate
  constructor path.

### Consumption

Diagnostics on unguarded `Optional` / known-`Undef` derefs (landed):
`docs/adr/narrowing-diagnostics.md`.
