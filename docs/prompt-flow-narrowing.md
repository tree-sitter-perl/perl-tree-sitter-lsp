# Flow-sensitive narrowing

**Landed.** Decision record: `docs/adr/flow-narrowing.md`.
Executable spec / playground: `test_files/narrowing_playground.pl`.

## Residual forward work

### Subject coverage

- **Accessor places** (`$self->name`) — parked. An accessor isn't a
  stable slot (it can return a different object per call, with side
  effects), so soundness needs a stricter no-call-between-guard-and-use
  model.

### Dynamic-key aliasing — soundness/precision knob

Dynamic-key places (`$self->{$k}`) currently take **Option A**: keyed by
spelling, the region truncates when a key scalar is reassigned, and a
write via a *different* key spelling that equals `$k` at runtime doesn't
truncate (a precision gap, never a crash — the existing constant-key
conservatism). **Option B** closes that aliasing hole: treat **any**
dynamic-key write to the container (`$self->{$j} = …`) as an *escape* that
re-widens every narrowed slot of that container — sound, but over-truncates
when the dynamic write hit a different key, and it applies to constant-key
places too. It is the general soundness-vs-precision knob for slot
narrowing; flipping to B is a localized change to `first_place_invalidation`
(truncate on *any* dynamic-key write to the container, not just the
matching spelling). Revisit if a motivating soundness case appears.

### Negation

- **General `Not` / `Difference` negation** — parked: no positive lookup
  target, no consumer value. "Not Foo" has nothing to dispatch on.

Diagnostics the lattice enables (landed): `docs/adr/narrowing-diagnostics.md`.
