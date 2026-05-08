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

| Doc | Axis | Status |
|---|---|---|
| `prompt-type-inference-residual.md` | **WHAT'S MISSING** — Parts 1–5 fact classes | each is a reducer+emitter pair |
| `prompt-nested-hashkey.md` | hash-key intelligence on Parametric values + structural hashes + array-element narrowing | Tier 1 partially landed; Tiers 2/3 queued |
| `prompt-return-type-expressions.md` | **NEXT PILLAR** — receiver-relative return types subsume per-method projection + arity dispatch | queued; ~200–300 LOC |
| `prompt-type-system-encoding.md` | **TYPE-SAFE AXIS DISPATCH** — make wrong-axis class-name reads unrepresentable | discussion; partially deferred to graph-walking |
| `prompt-type-is-the-gate.md` | **GENERALIZE STRICT-EQ GATES** — `type_says()` answers replace local-symbol-table presence checks | two instances landed (Part 5c); general refactor open |
| `prompt-dbic-as-plugin.md` | **MOVE DBIC OUT OF CORE** — port `visit_dbic_*` family + Parametric emission to a plugin | queued behind ReturnExpr + type-system-encoding |
| `prompt-sequence-types.md` | sequence type lattice | 5 phases on the clean foundation |

Landed work has its durable record in `docs/adr/` (`parametric-types.md`
for the Part 5c flavor enum + cross-file deferred owner fix;
`plugin-system.md` for the `return_via_edge` lazy-return mechanism;
`file-store-and-resolve.md` for forward-reference resolution + cross-file
invocant refresh) and the commit history.

### Notes on the queued work

- **Sequence-types phases 1-3** thread a lattice through the unified
  `Expr(Span)` attachment. Phases 4-5 (cross-file mutation effects,
  pipelines) compose on top.
- **Residual Parts 1-5** — invocant mutations, hash-key unions, method
  loops, functional operators, value-indexed returns. Independent
  reducer+emitter pairs. Order by value.
- **Receiver-relative return types** — the next architectural pillar.
  `return_type: ReturnExpr` admitting `Receiver` placeholders +
  `UnionOnArgs` branches. Subsumes per-method projection (DBIC `find`
  declares `RowOf(Receiver)` once on the symbol) AND arity dispatch
  (Mojo `has` accessors as `{ args.is_empty() => T, _ => Self }`).
  Retires the `FluentArityDispatch` family. Queued before the
  DBIC-plugin port.
- **Type-system encoding for axis dispatch** — defer until the full axis
  set is known (Element, Wrapped, Effect on top of Dispatch + HashKey).
- **Type-is-the-gate** — defer the general refactor until a second
  motivating site arrives. CLAUDE.md invariant #10 keeps new code on
  the right path in the meantime.
- **DBIC as a plugin** — the "everything is a plugin" direction makes
  core's per-ORM specials a CLAUDE.md #10 smell. Queued behind
  ReturnExpr + type-system-encoding so the plugin owns its semantics
  from day one.

### Hardening, slot anywhere

- `MAX_FOLD_ITERATIONS` got an all-builds bound in PR #31. Open
  follow-ups: route through `tracing::error!` instead of `eprintln!`
  (LSP stderr noise), and add a synthetic-oscillator test so the release
  path doesn't bit-rot. See `docs/d4-review-followups.md` items 1, 4.
- `apply_chain_typing_assignments` and `FileAnalysis::inferred_type` both
  do full-bag scans. Cheap to fix with a `HashSet<(name, scope, point)>`
  if profiling ever flags them. See `docs/d4-review-followups.md` items
  2, 3.
- Arity is bolted on (own `ReducerQuery` field, own observation variant,
  own reducer, own `ArityBranch` enum). Generalizing to a `Vec<Guard>`
  shape is premature until a second guard kind appears (type-of-arg
  dispatch, truthiness, `wantarray`). Do **not** ship more arity-shaped
  facts that need their own reducer in the meantime — fold them into
  `ReturnExpr` instead.
- **`return_via_edge` chases lack provenance.** Helper Methods that
  resolve their return type by chasing a `CodeRef.return_edge` (anon-
  literal `Expr(span)` or `\&foo` `MethodOnClass{...}`) record no
  `TypeProvenance` for the chase — `--dump-package` shows the resolved
  type but not "via Edge → MethodOnClass{Producer, build_rs} → reducer
  fold". Not a regression (the older span-based path was the same), but
  weaker than `MethodOnClassReducer`'s existing provenance story for
  method dispatch. Fix: stamp `TypeProvenance::Delegation { kind:
  "callable_return_edge", via: <attachment-tag> }` when the chain
  typer's `coderef_call_expression` arm fires, and mirror it onto the
  synthesized Method's symbol when `EmitAction::Method.return_via_edge`
  resolves.

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

**Scheduled after the type-inference quad.** Type intelligence is the
priority; graph rework is the next pillar. Migration is strangler fig —
port one query at a time.

### Eager Ref targets

`Ref.resolves_to: Option<SymbolId>` → `Ref.target: SymbolId` (with
`UNRESOLVED` sentinel). Removes lazy owner-resolution from query paths.
Independent of the graph question; can land any time. `refs_by_target`
already exists; just the eager-target invariant is missing.

### Why this matters

The bag is canonical at every phase: no cached projections, no parallel
paths. New type intelligence is purely additive — emit a witness, write
a reducer, no parallel path to keep in sync. Graph rework is what
unblocks Phase 6 (Openness diagnostic), multi-app Mojo support, and the
eventual `Symbol.home_scope` move. All follow the same pattern: collapse
ad-hoc code paths doing morally-similar work into one canonical
mechanism; enforce the invariant by construction.

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
   plugin system, error recovery, file store + resolve, parametric types).
3. This roadmap — what's queued, what blocks what.
4. The relevant `prompt-*.md` for the workstream you're picking up.
