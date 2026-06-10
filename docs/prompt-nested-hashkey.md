# Nested hash-key intelligence — residuals

**Status: LANDED (June 2026, PR #50).** All three tiers plus the
follow-through: `$row->{name}` → column def (post-fold owner upgrade),
`HashWithKeys` structural narrowing through hops / double-drills /
sub returns / imports, `Sequence` index projection and the mixed
drill, cross-file drill hops (`Projected` edge payload), the
closed-shape unknown-key diagnostic (whole-story gated, HINT),
mutation modeled on the shape (extend / open), and the literal
`%hash` spelling with every slice form. **The load-bearing decisions
live in `docs/adr/structural-shapes.md`** (data model, lattice rules,
Projected drills, canonical `%h`, mutation extension, the trust gate,
calibration discipline); `docs/adr/sequence-types.md` owns the array
side. This file keeps only what's still open.

## Residuals

- **Conditional-reassignment disagreement.** `$spec = { optional =>
  !$spec } unless ref $spec` — one arm has a known shape, the other
  keeps an unknown value. Today this is a trust-gate suppression
  (`reassigned_scalars`); the modeled fix is a lattice fold where the
  two assignment witnesses disagree-to-widen (BranchArmFold's shape,
  applied to conditional reassignment). When it lands, the
  reassignment clause comes out of the gate the way key writes did.
- **Sequence widening.** Direct in-bounds index writes retype/append
  (landed); out-of-bounds, conditional, and dynamic index writes stay
  unmodeled — `Sequence` has no open flag, and a bare-ArrayRef
  downgrade loses to structure-dominates-rep subsumption. Harmless
  while no array-index diagnostic exists (`element_at`'s None is an
  honest miss); adding `open` to `Sequence` is the fix if one ever
  lands.
- **Inferring hash structure from `bless` targets.** `bless { a => 1
  }, 'Foo'` already records HashKeyDef synthesis on `Foo`; letting
  the type system see the per-key types too is unbundled future work.
- **JSON / YAML / TOML loaded data.** A schema-reading plugin
  emission story, separate domain.
- **Recursive type constraints from Type::Tiny / Moose.** `isa =>
  HashRef[ArrayRef[Str]]` ASTs — a `use Types::Standard` plugin
  extending Moo synthesis to emit recursive Parametric. Tracked under
  residual Part 5c.
