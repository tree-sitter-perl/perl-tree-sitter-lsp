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
| `working-bag-residual.md` | **FINISH THE COLLAPSE** — four directives for "bag is the only truth" | **D1–D4 landed (PR #31).** D4-G follow-up open |
| `prompt-forward-reference-resolution.md` | **REGRESSION** — walk-time sym lookups miss forward-defined callees | **LANDED** — post-walk resolver |
| `prompt-cross-file-invocant-refresh.md` | **REGRESSION** — `invocant_class` cache stale after enrichment, cross-file refs under-match | **LANDED (PR #34)** — bag-only resolver |
| `prompt-type-inference-residual.md` | **WHAT'S MISSING** — Parts 1–5 fact classes | each is a reducer+emitter pair |
| `prompt-sequence-types.md` | **FIRST BIG CONSUMER** — sequence type lattice | 5 phases; ~half the spike's diff on a clean foundation |

### Dependency map

```
staircase 1–4 (LANDED, PR #27) ─┐
                                 ├─▶ bag-residual D1 redo (LANDED dc4315f, with residue → D3)
                                 ├─▶ bag-residual D2 (LANDED — one expression attachment)
                                 ├─▶ bag-residual D3 (LANDED — NamedSub variant gone)
                                 ├─▶ bag-residual D4 + follow-up (LANDED, PR #31 —
                                 │       FA.type_constraints gone, EXTRACT_VERSION 21)
                                 │       │
                                 │       ├─▶ forward-reference resolution (LANDED)
                                 │       ├─▶ cross-file invocant refresh (LANDED, PR #34)
                                 │       │
                                 │       └─▶ sequence-types phases 1-3
                                 │
                                 ├─▶ residual Parts 1-5 (independent)
                                 │
                                 └─▶ docs/prompt-cleanups.md (small, parallel,
                                       independent — pick anytime)
```

★ = known regression with a pinned `#[ignore]` test that fails until
fixed. Both ★ regressions have landed (forward-reference resolution
+ cross-file invocant refresh). The bag is now canonical at every
phase: no cached projections, no parallel paths. Sequence types and
the residual Parts build directly on top with no remaining
regression backlog gating them.

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
  - **D1 landed in `dc4315f`** with two bullets routed forward to D3:
    inheritance via `Edge(MethodOnClass(parent, name))` witnesses (the
    structural walk landed in `query_rec` instead) and deleting
    `build_imported_return_types` (the FA field went, the function survived
    with bag-routed reads). The residue is explicit in
    `docs/working-bag-residual.md` D3 — D3 is now larger than originally
    scoped.
- **Sequence-types phases 1-3** can start once D2 lands (the unified
  `Expr(Span)` attachment is what sequence types thread their lattice through).
  Phases 4-5 (cross-file mutation effects, pipelines) compose on top.
- **Residual Parts 1-5** (invocant mutations, hash-key unions, method loops,
  functional operators, value-indexed returns / sum types / parametric types)
  remain independent reducer+emitter pairs. Order by value; pick what hurts
  most when missing.

### Recommended sequencing

1. **Bag-residual D1 (redo)** — **LANDED in `dc4315f`.** Deleted
   `SymbolDetail::Sub.return_type`, `find_method_return_type_seen` /
   `_raw` / `self_method_tail`, the build-time chase, the
   `imported_return_types` FA field. Added `MethodOnClass{class, name}`
   attachment + `MethodOnClassReducer` + `BagContext.module_index` /
   `package_parents`. Fixed DFS-MRO order. Two bullets did not land
   as written: edge-witness emission for inheritance, and deletion of
   `build_imported_return_types`. Both routed to D3 with explicit
   tracking in `working-bag-residual.md`. Tests: 508 unit (was 506,
   +2 regressions). First attempt
   (`refactor/bag-residual-d1-method-on-class`, commit `c322178`) was
   abandoned for being additive — its cautionary value preserved in
   the directive's "what NOT to repeat" section.
2. **Bag-residual D2** — **LANDED** on
   `refactor/bag-residual-d2-expr-attachment`. One `Expr(Span)`
   attachment for every expression value. Killed `arm_payload`,
   `ReturnArm`, `ReturnArmReturn`, `last_expr_type`,
   `arm_types_per_ri` / `returns_by_scope` plumbing,
   `collect_return_arm_types`. Added `expr_payload` (single
   dispatch), `emit_expr_witness` (recursive arm population), and
   `SymbolReturnArmFold` (1+-arm `resolve_return_type` for
   Symbol-attached return arms; `BranchArmFold` keeps the ≥2 rule
   for ternary RHS on Variable/Expr). Tests: 509 unit + 93 e2e
   green. Unblocks sequence-types.
3. **Bag-residual D3** — **LANDED** on `refactor/bag-residual-d3`.
   Killed `WitnessAttachment::NamedSub` outright (compiler-driven
   subtractive deletion of 17 consumer sites). Dual writeback
   collapsed to `Symbol(sym_id)` + `MethodOnClass{class, name}`.
   Cross-file imports route through `module_index.find_exporters` +
   recursive `Symbol(cached_sid)` query in
   `query_sub_return_type`. `build_imported_return_types` deleted;
   `enrich_imported_types_with_keys` derives its inputs inline.
   Build-time + enrichment-time `Edge(MethodOnClass(parent, name))`
   inheritance edges replace the eager half of `query_rec`'s
   structural walk; the walk stays as a fallback for hand-crafted
   FAs and cross-file plugin bridges (where build-time pre-emission
   would be N×N). Method-call `Expression(refidx)` edges now key on
   class via a re-emittable worklist pass. Source-tag claim filters
   on `SubReturnReducer` collapsed to a single `branch_arm`
   exclusion. Cache `EXTRACT_VERSION` bumped 19 → 20. Tests: 507
   unit + 93 e2e green.
4. **Bag-residual D4 + follow-up** — **LANDED in PR #31.** Killed
   `pending_witnesses` staging, the walk-time TC-first read,
   `self_method_tails` / `return_self_method`, and the per-arm Edge
   expansion. Follow-up commits in the same PR deleted
   `FileAnalysis.type_constraints` outright (cache `EXTRACT_VERSION`
   bumped 20 → 21), migrated `--dump-package vars_in_scope` and the
   plugin sandbox diff to bag reads, and pushed two known regressions
   into red-pinned tests rather than leaving them undocumented:
   - **forward-reference resolution.** **LANDED.** Walk queues every
     name-driven `expr_payload` arm (`function_call_expression` /
     `ambiguous_function_call_expression` / `bareword` /
     `scoped_identifier`) whose symbol-table lookup came up empty as
     `(name, expr_span)` in `Builder.unresolved_call_targets`. New
     post-walk pass `resolve_forward_call_targets` runs between
     `populate_witness_bag` and `fold_to_fixed_point` and pushes the
     missing `Expr(span) → Edge(Symbol(sid))` witness for every entry
     that resolves against the now-final symbol table. Same source
     tag (`expression`) and span as the walk-time path so reducers
     can't tell the two emission sites apart. Tests: 512 unit + 93
     e2e green; coverage expanded beyond the Carp shape to ternary
     arms, scoped-identifier calls, implicit returns, and
     self-method tails (see `forward_reference_*` in
     `builder_tests.rs`). Spec:
     `docs/prompt-forward-reference-resolution.md`.
   - **cross-file `invocant_class` refresh.** **LANDED in PR #34.**
     The hybrid (option 3) recommended in the original spec was
     superseded by option 2 (drop the cached field outright). The
     decisive factor: chain hops where the inner receiver's class
     is only known via cross-file enrichment
     (`$x->makeFoo()->ping()` where `makeFoo` returns a cross-file
     class). Option 3's bag fallback hits a string-based resolver
     that doesn't walk a chain at read time; option 2's helper
     recurses on the inner receiver via `call_ref_by_start` and
     re-resolves fresh, so cross-file enrichment composes through
     every chain hop without any read-time refresh logic. Single
     dispatcher: `FileAnalysis::method_call_invocant_class(ref,
     module_index)` — invocant shape (variable / chain / function-
     call receiver / bareword / `__PACKAGE__` / `shift` / `$_[0]`)
     dispatches into bag queries. `Builder.method_call_invocant`
     remains as build-only worklist state, never copied into FA.
     Bonus collapses: 7-arg `resolve_method_invocant_public` →
     2-arg helper; `module_index` threaded uniformly through every
     in-file reader (was index-blind); `_with_index` API
     duplicates removed; perf bench gated on `--features
     perf_bench` (~11.5ms per `refs_to` over 200 files × 50
     calls). Tiebreak invariants on `call_ref_by_start` pinned by
     8 targeted tests in `src/call_ref_index_tests.rs`. Tests:
     525 unit + 20 e2e green.

   Forward-ref landed first (higher-frequency hit; Carp pattern is
   everywhere). Cross-file invocant refresh now also landed.

5. **Sequence-types phases 1-3** on the clean foundation. Can land in
   parallel with the regression fixes above; not architecturally
   blocked, just lower priority because correctness regressions
   should clear first.
6. **Residual Parts** (start with whichever is hurting most — likely Part 5b
   narrowing or Part 5c parametric types for DBIC, depending on workload).

### Hardening, slot anywhere

- `MAX_FOLD_ITERATIONS` got an all-builds bound in PR #31 (release `eprintln!`
  + `break`). Open follow-ups: route through `tracing::error!` instead of
  `eprintln!` (LSP stderr noise), and add a synthetic-oscillator test so the
  release path doesn't bit-rot. See `docs/d4-review-followups.md` items 1, 4.
- `apply_chain_typing_assignments` and `FileAnalysis::inferred_type` both do
  full-bag scans now (the price of dropping `type_constraints` as a parallel
  index). Cheap to fix with a `HashSet<(name, scope, point)>` if profiling
  ever flags them. See `docs/d4-review-followups.md` items 2, 3.
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

The bag-residual directives are done — the bag is canonical. Both
red-pinned regressions have landed: forward-ref was *emit-side* (the
witness never got pushed for forward-defined callees, fixed by the
post-walk resolver), invocant-refresh was *cache-side* (the cached
projection on refs went stale; fixed by deleting the cache outright
and routing every reader through one bag-routed dispatcher). The
audited claim — "all type intelligence routes through the bag" —
now holds at every phase, with no parallel cache paths and no
shape-specific resolvers per reader. New type intelligence (residual
fact classes, sequence types) is now purely additive: emit a witness,
write a reducer, no parallel path to keep in sync.

Graph rework is what unblocks Phase 6 (Openness diagnostic), multi-app
Mojo support, and the eventual `Symbol.home_scope` move (was: phase
7's `home_namespace`). All follow the same pattern: collapse ad-hoc
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
