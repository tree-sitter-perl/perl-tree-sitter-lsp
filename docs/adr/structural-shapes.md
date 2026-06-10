# ADR: Structural hash shapes — `HashWithKeys`, mutation extension, and the whole-story gate

Hash literals carry their keys in the type: `{ host => 'x' }` is not a
bare `HashRef`, it's a shape — and `->{key}` narrows through it, across
assignment hops, double-drills, sub returns, and imports. This ADR is
the data model + lattice + trust contract; `prompt-nested-hashkey.md`
keeps the residual phases. Landed across the `nested-hashkey` branch
(PR #50). Arrays got the same treatment earlier — see
`sequence-types.md`; the two variants share every rule below that
mentions them together.

## Decisions worth keeping

### `HashWithKeys { keys, open }` is an `InferredType` variant

```rust
InferredType::HashWithKeys {
    keys: Vec<(String, Option<Box<InferredType>>)>,
    open: bool,
}
```

`Vec`, not `HashMap` — order-preserving for stable display, and real
hashes have ≤10 keys so linear lookup beats hashing. A key's value
slot is `Option` so an unresolvable value keeps its key in the shape
without claiming a type. Placed at the END of the enum (same rule as
`Sequence`): cached-blob bincode variant indices stay stable; new
variants go at the end and bump `EXTRACT_VERSION`.

`open: false` is a CLAIM: the key set is exhaustive, an unknown key is
a miss consumers may act on (the diagnostic, completion ranking).
`open: true` withdraws it — a spread (`%$other`, `%other`, and array
spreads like `@_`: `my %h = (default => 1, @_)` is the
args-with-defaults idiom), a dynamic key, or a widening write means
unknown keys may exist. Everything trust-shaped in this ADR exists to
keep `open: false` honest.

### One shape builder, both spellings

`Builder::hash_literal_type` types `{ k => v, … }` AND
`my %h = (k => v, …)` — a list literal assigned to a hash variable IS
the hash literal; its meaning comes from the LHS sigil, so the
assignment visit routes it through the same builder rather than
minting a second path. Pair-walking is separator-agnostic and
positional (fat-comma carries no code semantics — CLAUDE.md), and a
spread occupies ONE list slot, so pairing skips it as a unit.

### Structure dominates rep; structured-vs-structured subsumes on equality only

Two `subsumes_narrowing` rules keep shapes sound under latest-wins
reduction:

- `HashWithKeys` subsumes `HashRef` (and `Sequence` subsumes
  `ArrayRef`): a deref site re-deriving the coarse rep cannot
  downgrade the shape it's reading through.
- Two structured types subsume only on EQUALITY: a genuinely
  different shape — a reassignment, a mutation extension — must win
  as latest. Anything cleverer (key-subset subsumption) would let a
  stale wider shape mask a legitimate replacement.

Return-arm folding stays coarse: arms that are all hash-shaped unify
to `HashRef`, not to a key union — arm disagreement is not a shape
claim.

### Drills are `Projected` edge witnesses, not values

```rust
WitnessPayload::Projected { base: WitnessAttachment, step: ProjectionStep }
pub enum ProjectionStep { HashKey(String), ArrayIndex(i32) }
```

"Edges, not values" applied to `->{k}` / `->[N]`: the walker emits the
projection relationship; the registry materializes the base at query
time — where the module index is in hand, so imported shapes project
too — and narrows by the step (`key_value_type` / `element_at`). This
single payload is what made cross-file drill hops work: the chase had
dead-ended only because drill expressions emitted no witness at all.

A `Variable` base scope-walks like the `Edge(Variable)` materialize
arm (the shape witness lives on the decl scope, not the access scope).
Container-form reads (`$h{k}`, grammar field `hash:`) project off the
hash variable's own attachment directly.

### Canonical `%h` is the one identity for the container spelling

`$h{k}` reads `%h`, not scalar `$h` — same node kind as the deref
form, different variable. The `HashKeyAccess` ref's `var_text`, the
shape witness attachment, and the `KeyWrite` all carry the canonical
`%h`, so the three producers and every consumer agree without
translation. The grammar disambiguates for free (`hash:` field with
`container_variable` vs a plain `scalar` child); the one residual trap
— postfix deref slices — uses field `hashref:` instead, encoded once
in the slice-write arm.

### Mutation is modeled on the shape, not gated away

The route-branding lesson (`route-branding.md`): an effect on a value
belongs on the value's TYPE, not in consumer-side suppression lists.
`$v->{k} = …` is therefore not a trust break — the walk records a
`KeyWrite` (var, key-or-dynamic, scope, span, RHS span, syntactic
conditionality), and the mutation-extension pass
(`witnesses::emit_mutation_extension_witnesses`) folds them into
Variable shape witnesses:

- **Unconditional static-key write → EXTEND.** The key joins the
  closed shape, its value typed from the RHS expression; the read
  drills back typed, and a typo beside it still hints.
- **Dynamic key, conditional write, scope-crossing write, or slice
  write → OPEN.** Conditionality is syntactic
  (`cst::is_conditionally_executed`: if/unless, postfix modifiers,
  ternary, loops, short-circuit chains — over-marking is the safe
  direction, it only widens) plus structural at the pass (the write's
  scope chain crossing any boundary before the attachment scope:
  nested block or closure, execution unknowable). Slice writes land
  several keys at once; all six spellings (`@h{…}`/`%h{k}`,
  `@$h{…}`/`%$h{k}`, `$h->@{…}`/`$h->%{…}`) record an open-switching
  `KeyWrite`.

Extension witnesses use zero-width spans at the write position — the
same temporal contract as TC mirrors: invisible to reads before the
write, latest-wins after. The pass is re-emittable in the worklist
fold (clear-and-emit, tag `mutation_extension`) and append-only at
enrichment — post-finalize removal would shift the sealed
`base_witness_count`, and duplicate pushes are idempotent under
latest-wins, truncated away by the next enrichment cycle. `KeyWrite`s
persist on `FileAnalysis` precisely so enrichment can re-run the pass
once imported shapes land.

### The whole-story gate is two clauses, each naming its modeled replacement

`FileAnalysis::closed_shape_is_whole_story(var)` =
not escaped ∧ not reassigned. The unknown-hash-key diagnostic fires
only behind it.

- **Escape** (`escaped_scalars`, builder-recorded): a scalar READ in
  any non-element-access position — call argument, RHS alias,
  invocant, sigil deref — hands the reference to code that may mutate
  the referent. Slice bases are excluded (a slice read copies values
  out). Hashes are sharper: bare `func(%h)` flattens to copies — the
  callee cannot add keys — so only `\%h` ref-taking escapes.
- **Reassignment** (`reassigned_scalars`): a Write targeting the
  variable itself (`$v = …`, `%h = …`). Element writes are NOT here —
  they're mutations, modeled above.

Each clause is the trust-gate stand-in for a lattice widening that
isn't modeled yet (escape widening; conditional-reassignment
disagreement — `$spec = {…} unless ref $spec`). When one gets
modeled, its clause comes OUT of the gate, the way key writes did.

### Calibration is part of done

Every change to the gate, the lattice, or the emission set re-runs
whole-substrate diagnostics (2,293 pinned CPAN modules). The
diagnostic ships at ZERO false positives there; the 23 pre-gate FPs
(Capture::Tiny, Params::ValidationCompiler, Mojolicious::Renderer, …)
are the corpus the clauses were derived against, and the gold rows in
`gold-corpus/fixtures/nested-*.json` pin both halves — the hint that
must fire and the writes/spreads/escapes that must stay silent.
