# Sequence Types — parked phases

**Status: PARKED — QA pulls.** The spike (tuple shape +
`element_at` projection, `adr/sequence-types.md`) covers everything
real-world QA has asked of arrays so far; zero crm findings pulled
anything below. Every phase is purely additive on the witness bag, so
there is no do-now-or-pay-later tax. When a finding arrives, expand
the matching phase from this list (full design sketches in git
history: `prompt-sequence-types.md` @ 9d34441).

1. **Shape lattice** — classify Homogeneous / CycleTuple /
   Heterogeneous on fold; widen on disagreement instead of dropping
   to bare ArrayRef. Includes the `open` flag on `Sequence` (today
   out-of-bounds/conditional/dynamic index writes are unmodeled —
   harmless while no array-index diagnostic exists).
2. **`Container(ArrayId)` attachment** — cross-method contributions
   to one array (push in one sub, read in another).
3. **Framework slot shapes** — `has` accessors returning typed
   sequences (`isa => ArrayRef[Str]` → element type on projection).
4. **Pipeline reducers** — `SequenceTransform`/`SeqOp` for
   map/grep/sort/reverse chains. The likeliest first pull (crm is
   map-heavy).
