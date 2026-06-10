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
| `prompt-nested-hashkey.md` | structural hash shapes + drills + the unknown-key diagnostic | LANDED (PR #50) — decisions in `adr/structural-shapes.md`; file keeps residuals |
| `prompt-type-system-encoding.md` | **TYPE-SAFE AXIS DISPATCH** — make wrong-axis class-name reads unrepresentable | discussion; partially deferred to graph-walking |
| `prompt-type-is-the-gate.md` | **GENERALIZE STRICT-EQ GATES** — `type_says()` answers replace local-symbol-table presence checks | two instances landed (Part 5c); general refactor open |
| `prompt-dbic-as-plugin.md` | **MOVE DBIC OUT OF CORE** — port `visit_dbic_*` family + Parametric emission to a plugin | ReturnExpr gate cleared; still behind type-system-encoding |
| `prompt-sequence-types.md` | sequence type lattice — residual phases | ADR `sequence-types.md` for the landed data model; prompt for full lattice + cross-method + pipelines |

Landed work has its durable record in `docs/adr/` (`bag-canonical.md`
for the invariant that closes the staircase — the bag is the only
source of types, edges chase, materialized projections are bugs;
`sequence-types.md` for the positional-container data model + the
`SequenceTransform/SeqOp` pattern for list operators (spike at
`ec62653`); `parametric-types.md` for the Part 5c flavor enum +
cross-file deferred owner fix; `return-expr.md` for receiver-
relative return types subsuming per-method projection + arity
dispatch; `plugin-system.md` for the `return_via_edge` lazy-return
mechanism + the declarative-manifest family (`overrides` / `dispatch_verbs`
/ `type_constraint_names` / `param_types` / `app_surface_consumers`) +
`SyntheticUse`; `type-constraints.md` for `TypeConstraintOf` + `Maybe`
erasure; `route-branding.md` for partial-route default inheritance via
`BrandedRoute`; `file-store-and-resolve.md` for forward-reference
resolution + cross-file invocant refresh) and the commit history.

Genuinely-unsolved problems (the hard boundaries the forward designs run
into — untyped param/hash-element boundaries, qualified-name suppression,
runtime export generators) live in `docs/open-problems.md`.

### Notes on the queued work

- **Sequence types** — spike landed (`ec62653`) in ~90 LOC: tuple
  shape on `Variable{name, scope}` + walk-time push contribution +
  `array_element_expression` projection in `resolve_expression_type`.
  See `adr/sequence-types.md`. Residual phases queued in
  `prompt-sequence-types.md`: full shape lattice (Homogeneous /
  CycleTuple / Heterogeneous classification), `Container(ArrayId)`
  attachment for cross-method contributions, framework `has`
  accessor synthesis returning slot shapes, pipeline reducers
  (`SequenceTransform` + `SeqOp` for map/grep/sort/reverse). Each
  is purely additive — no retrofit.
- **Residual Parts 1-5** — invocant mutations, hash-key unions, method
  loops, functional operators, value-indexed returns. Independent
  reducer+emitter pairs. Order by value.
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
- Arg-shape dispatch is unified under `ReturnExpr::UnionOnArgs` /
  `ArgGuard` (see `docs/adr/return-expr.md`). New guard kinds
  (type-of-arg, truthiness, `wantarray`) become new `ArgGuard`
  variants — no new reducers. Do **not** ship more guard-shaped facts
  that need their own reducer; fold them into `ArgGuard`.
- **DBIC parametric column-key completion at `->search({ | })`.**
  Goto-def from a typed key in `$rs->search({ name => 'X' })` lands
  on the `add_columns` def (proves the parametric chain resolves),
  but completion at an *empty* `->search({ | })` returns no items.
  The `find_call_context` path routes empty-hash-arg key completion
  through `complete_keyval_args`, which has no parametric-ResultSet
  branch — it should ask the call's typed receiver
  (`Parametric(ResultSet { row })`) for its column-key set and emit
  those alongside any built-in `keyval_args`. Pin already lives in
  `test_e2e_dbic_parametric.lua`'s header comment.
- **Cursor-context qualified-path detection should lean on the parser.**
  `extract_package_from_prefix` in `cursor_context.rs` walks the source
  text backward to recover the `Foo::Bar` chunk preceding the cursor's
  `::`. It now correctly handles Unicode (PR #42), but it's still
  reimplementing Perl's identifier rules in a text walkback — the
  tree-sitter-perl parse already knows where qualified-name tokens
  begin and end. Plan: thread the tree into `detect_cursor_context`
  (or move QualifiedPath into `detect_cursor_context_tree`) and ask
  the enclosing node for its span; fall back to the text walkback
  only when the cursor sits inside an ERROR node (mid-edit). Side
  benefit: drops `is_perl_package_name` + `is_perl_word_char` once
  the tree path is authoritative. Same shape applies to
  `extract_invocant_from_prefix` (also byte-walks, also reimplements
  identifier rules) — same fix, same site.
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
   plugin system, error recovery, file store + resolve, parametric types,
   return-expr, sequence types, type constraints, route branding,
   bag-canonical typing).
3. This roadmap — what's queued, what blocks what.
4. `docs/open-problems.md` — the hard boundaries deferred deliberately.
5. The relevant `prompt-*.md` for the workstream you're picking up.
6. `docs/qa-findings.md` — open false-positive worklist from the QA sweeps;
   `docs/qa-design-items.md` for the subset needing design first;
   `docs/parser-shortcomings.md` for the upstream tree-sitter-perl gaps
   (hand-off to the parser team — repros + downstream impact).
