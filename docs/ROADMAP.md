# perl-lsp Roadmap

> Where the engine is heading. Cross-refs into individual prompt-* and
> adr-* docs for detail. Read the ADRs for landed architecture; read
> the prompts for what's queued.

## Direction

Focus is **type intelligence** + **engine architecture**. Most user-facing
LSP features are already wired (completion, hover, goto-def, references,
rename, semantic tokens, sig help, inlay hints, etc.). The next round of
user-visible wins comes from making them **smarter**, not adding more —
which is downstream of a better-typed engine and a cleaner core.

User-facing features (Mojo polish, CLI diagnostics, distribution) are
backburner: ship when types + architecture are in a healthy place.

---

## Primary arc: type inference

Four docs, one engine:

| Doc | Axis | Status |
|---|---|---|
| `prompt-type-inference-unification.md` | **HOW** — collapse walker + bag into one path | **Steps 1–4 landed (PR #27)** — historical |
| `working-bag-residual.md` | **FINISH THE COLLAPSE** — four directives for "bag is the only truth" | **Active queue.** Each directive = one PR |
| `prompt-type-inference-residual.md` | **WHAT'S MISSING** — Parts 1–5 fact classes | each is a reducer+emitter pair |
| `prompt-sequence-types.md` | **FIRST BIG CONSUMER** — sequence type lattice | 5 phases; ~half the spike's diff on a clean foundation |

### Dependency map

```
staircase 1–4 (LANDED, PR #27) ─┐
                                 ├─▶ bag-residual D1 (methods in the bag)
                                 ├─▶ bag-residual D2 (one expression attachment)
                                 ├─▶ bag-residual D3 (one attachment per fact)
                                 ├─▶ bag-residual D4 (one canonical store)
                                 │       │
                                 │       └─▶ sequence-types phases 1-3
                                 │
                                 └─▶ residual Parts 1-5 (independent)
```

- **Unification staircase** is done. PR #27 subsumed Steps 1–4: the walker only
  observes, edge-payload witnesses replace closure-driven chase, deferred-var /
  BranchArm / ReturnOf observations are gone, chain typer + receiver typing go
  through the bag.
- **Bag-residual directives** (D1–D4) are the new keystone. Each one closes a
  remaining dual-path or syntactic-bake hack. They are sequenced because D1
  unblocks the cleanups in D3/D4 (`MethodOnClass` attachment is the shape
  cross-file dispatch + the "no source-tag claims" rule both need). Land in
  order; do not split a directive across PRs — partial lands recreate the
  two-paths-coexist state we just left.
- **Sequence-types phases 1-3** can start once D2 lands (the unified
  `Expr(Span)` attachment is what sequence types thread their lattice through).
  Phases 4-5 (cross-file mutation effects, pipelines) compose on top.
- **Residual Parts 1-5** (invocant mutations, hash-key unions, method loops,
  functional operators, value-indexed returns / sum types / parametric types)
  remain independent reducer+emitter pairs. Order by value; pick what hurts
  most when missing.

### Recommended sequencing

1. **Bag-residual D1** — `MethodOnClass{class, name}` attachment + inheritance
   edge emitter + cross-file bridges as edges. Replaces
   `find_method_return_type_seen`'s recursive ancestor walk. Unlocks D3/D4.
2. **Bag-residual D2** — one `Expr(Span)` attachment for every expression value.
   Kills `arm_payload`'s node-kind dispatch, `ReturnArm`, `last_expr_type`.
   Unblocks sequence-types.
3. **Bag-residual D3** — one attachment per fact, no source-tag claims. Kills
   `WitnessAttachment::NamedSub` and the source-tag claim filters in
   `SubReturnReducer` / `FrameworkAwareTypeFold`.
4. **Bag-residual D4** — `Symbol.return_type` becomes a lazy cache,
   `imported_return_types` dies, walk-time bag is live (no `pending_witnesses`
   staging, no walk-time TC-first read).
5. **Sequence-types phases 1-3** on the clean foundation.
6. **Residual Parts** (start with whichever is hurting most — likely Part 5b
   narrowing or Part 5c parametric types for DBIC, depending on workload).

### Hardening, slot anywhere

- `MAX_FOLD_ITERATIONS` is `debug_assert!`-only; release builds have no cap and
  rely entirely on the lattice argument. Pick: real release-mode bound with
  `tracing::error!` and break, or trust the lattice and remove the debug check.
- Arity is bolted on (own `ReducerQuery` field, own observation variant, own
  reducer, own `ArityBranch` enum). Generalizing to a `Vec<Guard>` shape is
  premature until a second guard kind appears (type-of-arg dispatch,
  truthiness, `wantarray`). Do **not** ship more arity-shaped facts that need
  their own reducer in the meantime.

---

## Next architectural pillar (after types)

### Graph walking — typed-edge graph with one walker

Detailed in `prompt-graph-walking.md`. Replaces the original
unification-spec Phase 1 (Namespace enum). The real problem isn't node
identity; it's that we have four parallel models for what's morally one
graph (`Scope` tree, `package_parents`, `PluginNamespace.bridges`,
`FileStore`+`RoleMask`) walked by four bespoke functions. Forgetting one
is a class of "feature mysteriously degraded" bug.

Shape: typed edges (parent / inherits / imports / bridges_* / file_role /
binds_*), branded edges where instance identity matters (the `$minion` /
`$app` cases), one `walk(graph, origin, edge_kind_mask, brand_ctx)`
function. Today's bespoke walkers collapse into `walk` calls with
different masks. Lives as an isolated module that consumes
`&FileAnalysis` — the builder doesn't grow this responsibility.

**Scheduled after the type-inference quad** (staircase + bag-residual +
sequence-types + residual Parts). Type intelligence is the priority; graph
rework is the next pillar. Migration is strangler fig — port one query at
a time.

### Eager Ref targets (Phase 5 of the original unification)

`Ref.resolves_to: Option<SymbolId>` → `Ref.target: SymbolId` (with
`UNRESOLVED` sentinel). Removes lazy owner-resolution from query paths.
Independent of the graph question; can land any time. `refs_by_target`
already exists; just the eager-target invariant is missing.

### Why this matters

The bag-residual directives are the immediate priority — finishing the
collapse the staircase started. Graph rework is what unblocks Phase 6
(Openness diagnostic), multi-app Mojo support, and the eventual
`Symbol.home_scope` move (was: phase 7's `home_namespace`). All follow the
same pattern: collapse ad-hoc code paths doing morally-similar work into
one canonical mechanism; enforce the invariant by construction.

---

## Backburner (user-facing)

Wired today; smarter is downstream of the engine work above.

| Doc | What |
|---|---|
| `prompt-mojo-todo.md` | Stash keys, hook completion, route naming + url_for, plugin chain transitive helpers, config completion |
| `prompt-cli-tools.md` | Diagnostic framework (PL001–PL010, `.perl-lsp.json`, suppression directives, SARIF), `--migrate`, remaining analysis subcommands |
| `prompt-ref-provenance.md` | Constant-fold `folded_from`, framework-attribute unified rename, package→file rename, inheritance override scoping |
| `prompt-synthetic-use.md` | `EmitAction::SyntheticUse` so plugins can mirror real `use` lines — unblocks trivial style-kit support (`Import::Base` / `ToolKit` shims) |
| `prompt-wasm-web-extension.md` | Cargo workspace split + browser extension; orthogonal to everything else |

These are ship-when-ready, not blocked. Several are worth slotting between
type-inference phases when an engine refactor needs cooling time.

---

## Out of scope

- Multi-workspace / monorepo.
- Cross-file rename of deps (deps are read-only — `RoleMask::EDITABLE`
  enforces this).
- Effect facts (mutation / throw / IO).
- Full dependent inference / univalence.
- `wantarray` / context-dispatched returns (mechanical to add when the
  pattern surfaces; not currently prioritized).
- Aliasing through scalars across function boundaries.
- Dynamic Namespace extension at runtime by third-party plugins (depends on
  the graph rework above).

---

## Reading order for someone joining

1. `CLAUDE.md` — live architecture, build pipeline, worklist invariants,
   plugin mechanics, type-inference query path. Source of truth.
2. `docs/adr/*.md` — load-bearing decisions for landed work (pod rendering,
   plugin system, error recovery, file store + resolve).
3. This roadmap — what's queued, what blocks what.
4. The relevant `prompt-*.md` for the workstream you're picking up.
