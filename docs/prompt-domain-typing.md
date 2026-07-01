# Domain typing from usage — the storage-vs-domain tier

Design for the "`op_type` is really an `opcode`" slice. Distilled from the
throwaway spike (branch ref `worktree-agent-ab549e3babef20f1a:docs/spike-domain-typing.md`)
+ the field-slot generalization. Queue item #2 in `docs/cpp-golive-map.md`.

## Problem

A C var/field has a **storage type** (declared) that discards its **domain
type** (what it means). perl5 `op_type` is declared `PERL_BITFIELD16 → unsigned
short` (storage), but it's *always* compared/assigned against `enum opcode`
values (`OP_CONST`, `OP_SCOPE`, …) — its useful type is `opcode`, recoverable
only from usage. This is the whole "int-used-as-enum" family (flags, states,
kinds), pervasive in C.

## Measured signal (spike, real perl5) — low-risk

- op.c: 85.7% of `op_type` sites are strong opcode evidence; all core `*.c`:
  83.9%; ~88% clearly opcode.
- Genuine raw-int noise: **2 sites / 1759, both comment text.** ~99.9%
  coherence; any threshold in (0.5, 0.99) fires cleanly — **no tuning problem.**
- The hypothesised noise (`op_type < OP_max`) is actually signal (`OP_max` is an
  enumerator).

## Design — new bag variants + ONE new primitive

### The primitive: a language-generic field-slot attachment

`WitnessAttachment::Field { owner, name }` — the subject is a **storage slot (a
declaration)**, NOT a scope/span. **Project-wide gathering:** every access to
the slot, anywhere in the program, folds onto the *same* subject. That's the new
granularity — "this named slot, everywhere" vs the bag's existing local subjects
("this variable, here" / "this span"). **Language-generic from birth** —
`{owner, name}`, not `{struct, name}` — so C struct fields AND Perl fields
(Corinna `field` / `HashKey` / Moo `has` / DBIC columns) plug into the same
subject. C emission is this slice; the Perl sources are queue #3.

Why it skips the long-distance tier: `op_type` is **whole-value-stable** (one
named slot; every use references it by name), so the field *identity* is the
aggregation key — no flow-chasing. `Field{…}` is precisely the terminal a future
value-provenance chase lands on.

### Witness + reducer (slot into the existing machine)

- Payload `DomainCompare { enum_type }` — pushed at each comparison / assignment
  / switch-case / typed-arg site where the slot interacts with a typed value;
  `enum_type` comes free from the enumerator's own C type.
- Reducer `DomainCoherenceFold` (the `FrameworkAwareTypeFold` pattern
  retargeted) → `NominalDomain { storage, domain, confidence }`. Coherence fold:
  mostly-agree → the domain; truly-mixed → widen/none.

### Two-tier: storage sound, domain defeasible

The type that *flows* stays the storage type (sound — arithmetic, size, the flow
tier). The domain is a **defeasible refinement** for the human surfaces only:
- **Hover:** `op_type: opcode` with the storage leaf as drill-down (compose with
  slice 2's leaf: e.g. `opcode` *(stored as `uint16_t`)*).
- **Type-constrained completion (the killer app):** at `op_type == |` / `= |`,
  offer `OP_*` — the domain enum's values.
- Never overrides storage for correctness.

### The bridge (free, bidirectional)

The domain is `Edge(TypeName(enum))` on the `Field`. goto-def reads it forward
(offer the `enum opcode` def). find-refs reads the witness spans backward (refs
on the enum surface the field's sites — the witness bag *is* the reverse index).
No new mechanism.

## Prerequisite (met)

Slice 3 (roles) collapsed `op_type` into ONE `BASEOP.op_type` subject — the
single aggregation target the project-wide fold needs. Inheritance gave domain
typing its subject.

## Scope

- **This slice (C):** generic `Field{owner, name}` + `DomainCompare` +
  `DomainCoherenceFold` + **C emission** (route field-access uses to the Field) +
  the bridge. Acceptance: `op_type` → `opcode` (hover + bridge; completion if
  cheap).
- **Queue #3:** wire Perl sources onto the same `Field` subject.
- **Parked:** parametric domains; the `#define`-family case (flag-sets —
  subset-of vs one-of, name synthesis); the long-distance/flow cases (a domain
  established through flow, or a value with no stable named home — the
  value-provenance tier terminates on `Field{…}`).
