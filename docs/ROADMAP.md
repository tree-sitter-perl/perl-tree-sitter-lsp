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

Three docs, three axes, one engine:

| Doc | Axis | Status |
|---|---|---|
| `prompt-type-inference-unification.md` | **HOW** — collapse walker + bag into one path | 4-step staircase, each ships green |
| `prompt-type-inference-residual.md` | **WHAT'S MISSING** — Parts 1–5 fact classes | each is a reducer+emitter pair |
| `prompt-sequence-types.md` | **FIRST BIG CONSUMER** — sequence type lattice | 5 phases; ~half the spike's diff on a clean foundation |

### Dependency map

```
unification step 1 ─┐
                    ├─▶ step 2 ─▶ step 3 ─┬─▶ step 4 (full collapse)
                    │                     │
                    │                     └─▶ sequence-types phases 1-3
                    │
                    └─▶ residual Parts 1-5  (independent of the staircase)
```

- **Unification steps 1+2** are small (~50 + ~150 lines, mostly deletions). No new
  capability; debt service that makes everything after it cheaper.
- **Unification step 3** retires the `ReducerQuery` closures and replaces them
  with `ReducerContext`. Enforces "the bag is canonical" *by construction*.
  Unblocks sequence-types proper.
- **Step 4** is the full collapse (delete `infer_returned_value_type_seen` etc.).
  Optional in the sense that 1-3 already pay for themselves.
- **Sequence-types phases 1-3** depend on step 3 for the closure-free
  `ReducerContext`. Phase 4-5 (cross-file mutation effects, pipelines) are
  separate design surfaces that compose on top.
- **Residual Parts 1-5** (invocant mutations, hash-key unions, method loops,
  functional operators, value-indexed returns / sum types / parametric types)
  are each independent reducer+emitter pairs. Order by value; pick what hurts
  most when missing.

### Recommended sequencing

1. Steps 1+2 of the unification staircase (debt service, fast).
2. Step 3 (canonicality enforcement). The keystone for what follows.
3. Sequence-types phases 1-3, fresh on the clean foundation.
4. Residual Parts (start with whichever is hurting most — likely Part 5b
   narrowing or Part 5c parametric types for DBIC, depending on workload).
5. Step 4 of the staircase, when convenient.

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

**Scheduled after the type-inference triplet.** Type intelligence is the
priority; graph rework is the next pillar. Migration is strangler fig —
port one query at a time.

### Eager Ref targets (Phase 5 of the original unification)

`Ref.resolves_to: Option<SymbolId>` → `Ref.target: SymbolId` (with
`UNRESOLVED` sentinel). Removes lazy owner-resolution from query paths.
Independent of the graph question; can land any time. `refs_by_target`
already exists; just the eager-target invariant is missing.

### Why this matters

The type-inference unification staircase is the immediate priority. Graph
rework is what unblocks Phase 6 (Openness diagnostic), multi-app Mojo
support, and the eventual `Symbol.home_scope` move (was: phase 7's
`home_namespace`). All three follow the same pattern: collapse ad-hoc
code paths doing morally-similar work into one canonical mechanism;
enforce the invariant by construction.

---

## Backburner (user-facing)

Wired today; smarter is downstream of the engine work above.

| Doc | What |
|---|---|
| `prompt-mojo-todo.md` | Stash keys, hook completion, route naming + url_for, plugin chain transitive helpers, config completion |
| `prompt-cli-tools.md` | Diagnostic framework (PL001–PL010, `.perl-lsp.json`, suppression directives, SARIF), `--migrate`, remaining analysis subcommands |
| `prompt-ref-provenance.md` | Constant-fold `folded_from`, framework-attribute unified rename, package→file rename, inheritance override scoping |
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
