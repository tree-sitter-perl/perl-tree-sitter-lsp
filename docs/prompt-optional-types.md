# Optional / Maybe types

**Landed.** Decision record: `docs/adr/optional-types.md`.

Residual forward work:
- `SlotTypeFold` production ({T, undef} slot writes → `Optional`);
- bareword Type::Tiny `Maybe[…]` constructor form (the `TypeConstraintOf`
  path; the quoted-string `isa => 'Maybe[T]'` form landed);
- diagnostics on unguarded `Optional` / known-`Undef` derefs:
  `docs/prompt-narrowing-diagnostics.md`.
