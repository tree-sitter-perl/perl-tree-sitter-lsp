# Flow-sensitive narrowing

**Landed.** Decision record: `docs/adr/flow-narrowing.md`.
Executable spec / playground: `test_files/narrowing_playground.pl`.

Residual forward work:
- elsif-chain cumulative negation (intersection across conditions);
- general `Not` / `Difference` negation — parked, no positive lookup
  target, no consumer value;
- accessor places (`$self->name`) — an accessor isn't a stable slot.

Diagnostics the lattice now enables: `docs/prompt-narrowing-diagnostics.md`.
