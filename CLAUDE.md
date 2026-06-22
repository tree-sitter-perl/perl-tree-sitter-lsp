# perl-lsp

A Perl LSP server built on ts-parser-perl (crates.io) and tower-lsp.

## Build & test

```
cargo build --release
cargo test                              # unit tests
./run_e2e.sh                            # e2e tests (needs nvim + release build)
gold-corpus/run.pl                      # gold harness (needs release build + installed substrate)
perl-lsp --dump-package <root> <pkg>    # debug type inference for a package
```

The gold harness (`gold-corpus/README.md`) is an exact-assertion regression
net over real CPAN modules from a snapshot-pinned substrate
(`gold-corpus/local/`). `fixtures/*.json` is the source of truth; statuses:
gold (must hold → FAIL on regression), xfail (known gap → XPASS when a fix
lands, promote the row), provisional (reported, never fails). A crash is
always a hard fail. Run it alongside `cargo test` + `./run_e2e.sh` before
calling a change verified; `gold-corpus/run.pl --emit <cap> <file> <row>
<col>` authors new rows. Known gaps live in `gold-corpus/KNOWN-GAPS.md`.

## Architecture

Four layers, data flows down only — enforced by `src/layering_tests.rs`
(the import DAG, the model's Point-only tree-sitter surface, and
single-point grammar access all fail `cargo test` on violation; new
source files must be assigned a layer in its `layer_map`):

```
LSP adapter      symbols.rs, backend.rs       → LSP protocol types
Cross-file       module_index/_resolver/_cache → CachedModule (Arc<FileAnalysis>)
Builder          builder.rs                   → produces FileAnalysis
Data model       file_analysis.rs             → FileAnalysis (serde, bincode-cacheable)
```

See `docs/ROADMAP.md` for the forward design corpus entry point. `docs/adr/file-store-and-resolve.md` covers the landed cross-file unification: single role-tagged FileStore + RoleMask + `refs_to`, plus `resolve_symbol` (cursor→`ResolvedTarget`) — the one entry point both LSP handlers (references/rename) and their CLI mirrors use to identify the target before calling `refs_to`. Never map `RenameKind`→`TargetRef` inline in a handler; per-feature policy on a target is a method on `TargetRef` (e.g. `supports_cross_file_rename`). Graph walking — one lazy `walk` over the visibility edges (inheritance / bridges / descendants) — landed: `docs/adr/graph-walking.md` (`GraphView`, the closed `EdgeKind` + exhaustive `edges_from`, the Model-layer placement, the file-role/lexical boundaries). Forward work in `docs/prompt-graph-walking.md`: the deferred Scope-node taxonomy, and **instance brands** (per-object dispatch scoping, multi-app Mojo) — spiked and PARKED as a downstream consumer of the long-distance value-provenance tier (`prompt-type-inference-residual.md`), not a standalone feature.

### Rules (read before writing code)

1. **All tree-sitter CST traversal happens inside `build()`.** No other file walks tree-sitter nodes, calls `child_by_field_name`, or uses `TreeCursor`. To add CST-derived data: extend `visit_*` in `builder.rs`. Builder plugins (separate modules taking `&mut FileAnalysis` + `&Tree` + `&[u8]`) are fine — they preserve the single entry point. Multiple post-walk passes inside `build()` are allowed and named (see "Build pipeline phases") for cases needing resolved state.

   **Within sanctioned tree consumers, speak `cst.rs`, not raw nodes.** `src/cst.rs` is the typed view over the CST (rust-analyzer style: zero-copy wrappers via `typed_node!` + `cast`, `NodeExt` for field-text/span/named-children, `pair_nodes` for separator-agnostic pair walking, `call_args` for flat call arguments, `varname_child`/`canonical_container_name` for variable identity, `is_conventional_invocant_scalar` for receiver detection). Each grammar trap is encoded there exactly once. New visitor code uses the typed accessors; re-spelling `child_by_field_name(..).utf8_text(..)` chains or a fresh `kind() == "..."` probe for a shape `cst.rs` already models is a bug. Name-level Perl conventions (`is_constructor_name`, `is_conventional_invocant_name`) live in `src/conventions.rs` — pure `&str`, importable by tree-free layers like `file_analysis.rs`.

2. **`file_analysis.rs` is the single source of truth.** All analysis results live in `FileAnalysis`. Query methods belong here. No `tree_sitter` imports.

3. **`symbols.rs` is a thin adapter** — `FileAnalysis` types → LSP types. No analysis, no tree walks, no Perl semantics decisions.

4. **`module_resolver.rs` calls the builder, then queries `FileAnalysis`** — never walks the tree directly.

5. **DRY: shared extraction logic goes on `FileAnalysis`.** Two callers needing the same data → one method, both call it.

6. **`cursor_context.rs` is the position-dependent exception** — gets a tree + source for completion/sig-help context. Does NOT modify `FileAnalysis`.

7. **Every meaningful token gets a ref.** If `ref_at(pos)` returns nothing or returns too-broad, the builder is missing emission. Overlapping refs → `ref_at` returns the **narrowest span**. Common gaps: fat-comma keys in calls (`connect(timeout => 30)` needs its own `HashKeyAccess`), hash literal keys, framework-synthesized entities (Moo `has name` → `HashKeyDef` for constructor, not just accessor).

8. **Plugin-synthesized content is owned by `PluginNamespace`, not Perl classes.** See `docs/adr/plugin-system.md`. Cross-file lookup goes through `ModuleIndex::for_each_entity_bridged_to(class, ...)` — do NOT add parallel reverse indexes (retired: `class_content_index`, `modules_with_class_content`). Plugins have **emit hooks** (`on_use`, `on_function_call`, `on_method_call`) — parse-time, declarative, return `Vec<EmitAction>` — and **query hooks** (`on_signature_help`, `on_completion`) — cursor-time, imperative, for shape-dependent behavior. `Silent` / `exclusive` answers suppress native paths when plugin knows native will mishandle the slot.

9. **Provenance: derived refs trace to source** for rename/cross-ref. Constant folding (`my $m = 'process'; $self->$m()`), `has` declarations (accessor + constructor key + internal hash key), import lists, return hash keys → caller derefs (`HashKeyOwner::Sub`), package→file path, inherited overrides.

10. **Never special-case for a particular shape — you are always wrong.** Any code that branches on "is this method named `search`", "does this base equal `DBIx::Class::ResultSet`", "did this come from a plugin vs the real walker", or "is this return-type a String" is encoding a partial enumeration of shapes that match a behavior. The list is always incomplete — tomorrow's case that should match silently doesn't; tomorrow's case that shouldn't match silently does. **Fix:** encode the "wants behavior X" property on the *type / value / witness* itself, so consumers ask the value the question and the value answers. The consumer never sees the shape.

   Recurring forms of this antipattern:
   - **Method-name allowlists** in consumers (`if target_name in {search, find}`). Push the rule onto the *type*: `InferredType::hash_key_class()` for the parametric-arg case. See `docs/adr/parametric-types.md`.
   - **Lossy-string returns** (`Option<String>` for a class name when the source had `Option<InferredType>`). The string is a consumer-side projection, not a return-type contract. Helpers that resolve "what type does this expression produce" return `Option<InferredType>`. Class-name accessors (`class_name()`, `dispatch_class()`, etc.) are caller-side methods on the rich type. Cleanup pass landed; back-compat thin wrappers remain (`method_call_invocant_class`, `resolve_invocant_class_tree`, `invocant_text_to_class`) — they each call the typed sibling and project at the consumer.
   - **Real vs synthetic branches** in worker code (`if synthetic { skip step }`). Synthetic paths run the same body, factored to a function. Termination concerns go on the dispatcher (seen-sets, depth caps) — not on the worker.
   - **Per-base / per-name lookup tables in core** for behavior plugins should own. The plugin trait carries the rule; core dispatches generically.

   The discipline is hard because the special case is always the smallest diff right now. Reaching up to the general path is the larger commit. Do the larger commit anyway — the special case never stays cheap, and provenance / observability / future flavors of the same rule all rot when they're not paying it forward.

### Comment style

Comments earn their keep by explaining **why**, not **what**. The code already says what it does; a comment that restates it is dead weight and drifts the moment the code changes.

- **Write the why:** non-obvious invariants, ordering constraints ("registered first so X claims before Y"), surprising trade-offs, the war story behind a workaround, and pointers to the ADR that owns the decision. One or two tight lines beats a paragraph.
- **Don't narrate history.** No "replaces the old X", "used to be Y", "D3 added", "the staircase deleted", "Spike goals", "Part 6 of the spec", "as Phase 4–6 land". Git remembers; the comment shouldn't. Describe what *is*, not what *was* or what *changed*.
- **Don't restate the architecture.** If it's in this file or derivable from the types, link or omit — don't paraphrase it inline.
- **Don't enumerate every match arm / field in prose** when the code is self-evident; comment only the arm that's surprising.
- A doc comment (`///`) on a public item states its contract; reserve the long-form rationale for the one spot that owns it, not every caller.

When a comment grows past a few lines, that's a smell: either the code wants a clearer name/shape, or the rationale belongs in an ADR under `docs/adr/` with a one-line pointer here.

### File map

- `main.rs` — entry, CLI modes (`--rename`, `--workspace-symbol`, `--dump-package`, `--parse`, `--clear-cache`, `--version`). `cli_full_startup(root)` = "act like LSP just started". `perl-lsp --parse <file|-->` prints the tree-sitter parse tree (`--` reads stdin) — use it to check node kinds/fields instead of guessing. `perl-lsp --clear-cache [<root>]` wipes the SQLite module cache — with `<root>` only that project's hashed cache dir, without it the whole `~/.cache/perl-lsp` tree. Prefer this over `rm -rf` (it canonicalizes `<root>` to the same hash the server writes under).
- `backend.rs` — `LanguageServer` impl, request routing.
- `document.rs` — open-file `Document` (tree + text + analysis + stable_outline).
- `file_store.rs` — unified store for open + workspace FileAnalyses, role-tagged, dedup'd by path.
- `file_analysis.rs` — data model; serde-derived.
- `builder.rs` — CST→FileAnalysis. ONLY tree-sitter consumer.
- `cst.rs` — typed view over the CST (typed wrappers, `NodeExt`, pair walking, call args, varname canonicalization). The vocabulary every tree consumer uses.
- `conventions.rs` — Perl name semantics, parsed once: `MethodToken` (FQ/SUPER/main method-token qualifier), `InvocantText` (variable/bareword/`__PACKAGE__`/positional-receiver invocant shape), plus name predicates (constructor, conventional invocant, `__PACKAGE__`). Pure `&str`; no tree-sitter. Never re-derive these shapes with `rsplit_once("::")` / `starts_with('$')` string ops in a consumer.
- `pod.rs` — POD→markdown via tree-sitter-pod.
- `cursor_context.rs` — position-dependent context.
- `symbols.rs` — LSP adapter.
- `resolve.rs` — cross-file `refs_to` + `RoleMask` (OPEN/WORKSPACE/DEPENDENCY/BUILTIN). All cross-file queries route here.
- `module_index.rs` — cross-file dep API, `CachedModule`, `SubInfo`, reverse index (`func → modules`).
- `module_resolver.rs` — background `std::thread`, in-process parsing, workspace indexing (Rayon).
- `module_cache.rs` — SQLite (schema v9, bincode+zstd FileAnalysis blob).
- `cpanfile.rs` — cpanfile via tree-sitter queries.
- `witnesses.rs` — witness bag + reducer registry for type inference.
- `timings.rs` — **all profiling/timing instrumentation lives here.** Per-module build timings (`--timings` / `PERL_LSP_TIMINGS`, slowest-first report) and per-phase timing (`timings::phase` + the `bphase!`/`tphase!` call-site sugar, gated by `PERL_LSP_PHASE_TIMING`). Don't hand-roll env-gated `eprintln!` timers — route new timing through here so the gate is read once and the output format stays uniform.

## Cross-file resolution

- `ModuleIndex` runs a dedicated `std::thread` for FS I/O (never blocks tokio). `Arc<DashMap>` shared with async handlers.
- `CachedModule { path, analysis: Arc<FileAnalysis> }` — full FileAnalysis survives module boundary (refs, type_constraints, call_bindings, framework_imports, package_parents). `SubInfo<'_>` view gives ExportedSub-style accessors.
- Reverse index: `DashMap<func_name, Vec<module_name>>` for O(1) exporter lookup.
- SQLite cache per project at `~/.cache/perl-lsp/<hash>/modules.db` (honors `$XDG_CACHE_HOME`). `EXTRACT_VERSION` bump triggers priority re-resolution without dropping the table. To nuke it (e.g. before a cold-start repro), use `perl-lsp --clear-cache [<root>]` — not `rm -rf`.
- **Plugin fingerprint** — bundled plugin sources + every `.rhai` in `$PERL_LSP_PLUGIN_DIR` are hashed. Mismatch on startup hard-clears modules table (same machinery as `validate_inc_paths`). Editing a plugin invalidates cache so QA isn't served stale blobs.
- Async handlers only call `_cached` methods (zero I/O).
- After resolution, diagnostics refresh for all open files (clears stale false positives).

### Cross-file enrichment

`enrich_imported_types_with_keys()` on `FileAnalysis` propagates imported return types and hash keys: pushes `TypeConstraint`s for call bindings to imported funcs, injects synthetic `HashKeyDef` symbols for cross-file hash-key completion. Idempotent: `base_type_constraint_count` / `base_symbol_count` / `base_witness_count` set after initial build; enrichment truncates back to baseline before appending. `rebuild_enrichment_indices()` rebuilds index maps after. Called from `publish_diagnostics()` and the resolver refresh callback — i.e. **OPEN documents only**; workspace-index + dependency files are built without enrichment. Type/method resolution through inheritance survives this via the `MethodOnClass` query-time walk; **dispatch-verb resolution** survives it via the query-time `ReceiverGated` seam (`FileAnalysis::applicable_dispatches`, resolved in `refs_to` / goto-def — `docs/adr/receiver-gated-dispatch.md`). Still deferred: `ClassIsa`/`param_types` ancestry-gated *emission* (Phase 2 mints these on the same seam) — see `docs/prompt-enrichment-inheritance-residual.md` for the per-manifest applicability matrix.

## Type inference (witness bag)

**The bag is the only source of types.** Type production is `bag.push(...)`; type consumption is `bag_query_attachment(att)` through `ReducerRegistry::query`. There is no second source. `Builder::infer_expression_type` is gone; closed-syntax cases live in `expr_payload`'s match called by `emit_expr_witness`. `Builder.resolved_returns` is gone; walk-time synthesis pushes `Symbol(sid)` witnesses directly. See `docs/adr/bag-canonical.md` for the load-bearing decisions; `docs/adr/structural-shapes.md` for structural hash shapes (`HashWithKeys`, `Projected` drills, mutation extension, the whole-story trust gate).

**Edges, not values.** If a registry query on attachment `A` already resolves through an edge chase, do NOT re-push the materialized `InferredType(t)` onto `A` as a "cache." The registry's chase IS the canonical flow; the materialization is a parallel store that drifts. Every published witness is either (a) a source value the walker uniquely knows (a literal's type, a plugin's declaration, a framework's `ReturnExpr`) or (b) an `Edge` to another attachment.

**Two strict phases:**

1. **Collect** in `Builder::populate_witness_bag()`: mirror every `TypeConstraint` as Variable witness; push `HashRefAccess` from `$v->{k}` refs; push `mutation` Facts on hash-key writes; push `Symbol(sid) → Edge(Expr(last_expr_span))` for each Sub/Method scope without explicit returns (the implicit-return chain).

2. **Reduce** via `ReducerRegistry` in `src/witnesses.rs`. Reducers in registration order — earlier entries claim first:
   - `PluginOverrideReducer` — Plugin-priority `Symbol(_) + InferredType` short-circuit. `WitnessSource::priority()` returns 100 for `Plugin(_)`; 10 for everything else.
   - `ReturnExprReducer` — `WitnessPayload::ReturnExpr(_)` shapes: `Concrete` / `Receiver` (substitutes `q.receiver`) / `Operator(RowOf(_))` / `UnionOnArgs` (dispatches `q.arity_hint`). See `docs/adr/return-expr.md`.
   - `SymbolReturnArmFold` — `SymbolReturnArm(sid)` attachment; folds per-arm `Edge(Expr(body_span))` witnesses via `resolve_return_type` (1+ arms agree → `Some(t)`, disagree → `None`). `Symbol(sid)` carries one `Edge(SymbolReturnArm(sid))` chain witness per arm so consumers querying the symbol's return materialize through.
   - `BranchArmFold` — agreement across ≥2 `branch_arm`-source `Edge` witnesses on `Variable` / `Expr` for ternary RHS.
   - `FrameworkAwareTypeFold` — folds `TypeObservation` (rep / class / bless / numeric / regex / string) using package's `FrameworkFact`. Class-identity dominates rep when frameworks agree (Mojo `$self->{x} = ...; return $self` keeps `ClassName`). Temporal: skips witnesses past the query point.
   - `MethodOnClassReducer` — `MethodOnClass{class, name}` primary fallback. Inheritance + cross-file bridges resolve via `query_rec`'s structural recursion on the same attachment shape, with inheritance edges (`MethodOnClass{child, m} → Edge(MethodOnClass{parent, m})`) pushed by the writeback so the registry walks them like any other edge.
   - `SubReturnReducer` — `Symbol(_) + InferredType` latest-wins, runs last; backstop when no higher-priority reducer answered.
   - `ExprReturn` — `Expr(_) + InferredType` latest-wins.

**Single shared query path.** `query_variable_type(bag, scopes, framework, var, scope, point)` and `query_sub_return_type(bag, symbols, name, arity_hint, ...)` in `witnesses.rs` are the only scope-walk + framework + reducer dispatch sites. `FileAnalysis::inferred_type_via_bag` / `sub_return_type_at_arity` (query-time) and `Builder::bag_query_attachment` / `bag_query_expr_span` (build-time) are thin wrappers over the registry. Identical rules in both by construction.

**`TypeConstraint` is an input-parameter shape.** The struct exists so callers can package `(variable, scope, span, inferred_type)` for `push_type_constraint(tc)`, which translates it into Variable witnesses on the bag. There is no `Vec<TypeConstraint>` field — the bag is the only storage. Enrichment truncates the bag back to `base_witness_count` before re-deriving.

**Query entry points** (call these, not internal helpers):

- `inferred_type_via_bag(var, point)` — variable type at a point.
- `sub_return_type_at_arity(name, arity)` — sub return type (locals + imports).
- `method_call_return_type_via_bag(ref_idx)` — Expression-attached return type (lets `$r->get('/x')->to('Y#z')` resolve `->to`'s receiver without an intermediate variable).
- `find_method_return_type(class, method, module_index, arity)` — class-keyed method return type. Thin wrapper that builds a `MethodOnClass{class, name}` `ReducerQuery`; the registry handles inheritance walks and cross-file bridges.
- `mutated_keys_on_class(class)` — dynamic-key completion on `$self->{`.

**Legacy:** `FileAnalysis::inferred_type(var, point)` is raw-state introspection only (no framework/branch/arity rules). Doc-flagged "not a type query." Two narrow uses: `resolve_method_call_types` early-out, and tests asserting on raw builder state. Use `inferred_type_via_bag` everywhere else.

**Adding type behavior = adding a reducer.** Never bypass the bag with direct `InferredType` writes or parallel query helpers. New fact → push a witness (`Edge` if it routes to an existing source); new fold → write a reducer.

**Cache durability:** `WitnessBag` and `package_framework` are `#[serde(default)]`, ride bincode+zstd cache blob. Bump `EXTRACT_VERSION` on shape/rule changes.

### Build pipeline phases

`build_with_plugins()` in `builder.rs` runs in fixed order. Each pass consumes state the previous produced.

1. **live walk** (`visit_*`) — emits Symbols/Refs/Scopes/TypeConstraints, queues plugin emissions, records `ReturnInfo`s (scope + arity branch + body span — no walk-time types). Walks emit `Expr(span)` witnesses via `emit_expr_witness` at every meaningful expression node (literal types directly, name-dependent shapes as `Edge` payloads pointing at `Variable` / `Symbol` / `Expression`).
2. `resolve_variable_refs()` — scalar refs → `resolves_to`.
3. `resolve_hash_key_owners()` — HashKeyAccess → HashKeyOwner via TC types.
4. `apply_type_overrides()` — plugin manifests push **Plugin-priority** witnesses on `Symbol(sub_id)`. The `PluginOverrideReducer`'s priority short-circuit (Plugin > Builder) makes them dominate every other reducer. Provenance is recorded as `TypeProvenance::PluginOverride` so `--dump-package` can answer "why does this return X?". Runs BEFORE the worklist fold so dependent inference sees overrides via the bag.
5. `populate_witness_bag()` — one-shot bag seed. Mirrors walk-time `TypeConstraint`s as Variable witnesses; pushes `HashRefAccess` Observations from `$v->{k}` refs; pushes `mutation` Facts on hash-key writes; pushes `Symbol(sid) → Edge(Expr(last_expr_span))` for each user-defined Sub/Method scope without explicit returns (the implicit-return chain). Bag is canonical after this.
6. `fold_to_fixed_point(chain_idx)` — **the worklist fold**. Each iteration runs `ChainTypingReducer::PreFold` (assignment + return-arm refresh) followed by `resolve_return_types`: `emit_arity_return_witnesses` (re-emittable per-Symbol UnionOnArgs publication for arity-discriminated subs) → `emit_method_call_return_edges` (re-emittable `Expression(refidx) → Edge(MethodOnClass{...})` for known invocants) → `seed_return_types_from_bag` (pure read; queries `Symbol(sid)` per Sub/Method scope, builds the name-keyed `return_types` map for downstream consumers, preserves `PluginOverride` / `Delegation` provenance) → `write_back_sub_return_types` (pushes `MethodOnClass{class, name} → Edge(Symbol(sid))` for primaries; plugin-namespace bridges; inheritance edges) → `propagate_call_bindings_to_constraints` → `fixup_call_bound_hash_key_owners`. The loop exits when the snapshot — per-Sub registry answer + bag len + invocant cache size — stops moving; `MAX_FOLD_ITERATIONS = 64` is the debug-only safety net. After the lattice settles, `ChainTypingReducer::PostFold` runs once to fill `invocant_class` on `MethodCall` refs.
7. `resolve_tail_pod_docs()` — POD docs for subs lacking preceding doc.
8. `FileAnalysis::new(...)` — construct FA, build indices, `resolve_method_call_types(None)` as text-based MCB fallback.
9. `fa.finalize_post_walk()` — seal `base_*_count` for idempotent re-enrichment.

`Builder::resolve_invocant_class_tree` is the **single** build-time symbolic-execution function (chain typing, return-arm refresh, invocant filling); `invocant_type_at_node` is its node-kind dispatcher. Adding a second is wrong; add cases. Query-time expression typing is **tree-free**: `FileAnalysis::expr_type_at_span(span, module_index)` is the one entry — it reads the `Expr(span)` witnesses the builder records per invocant (`emit_invocant_expr_witnesses` PostFold pass) plus exact-span call-ref returns, materializing edges through the registry. `method_call_invocant_class(r, module_index)` resolves an invocant via it (no tree). `resolve_expression_type(node, …)` is now a thin `node → span → expr_type_at_span` adapter, degrading to a node-kind walk only for raw-cursor/incomplete-ERROR completion. There is no second structure-discovery walker to "keep in sync."

### Worklist invariants

The fold driver in step 6 is the only place type inference iterates. Adding a new fact-fold means **adding a reducer**, not changing the driver:

- **Reducers are stateless.** A `WitnessReducer` claims a `WitnessAttachment` shape (`Symbol(_)` / `Variable{..}` / `Expression(_)` / etc.) and folds the witnesses for that attachment into a `ReducedValue`. They live in `witnesses.rs` and register through `ReducerRegistry::with_defaults()`. The worklist driver never special-cases a reducer — it just runs the registry on each attachment.
- **Witnesses are monotone.** Once a witness is in the bag, it stays. New facts append; no reducer rewrites or deletes another's witness. Termination follows from the lattice (`InferredType` is a finite enum, ~12 variants) plus the snapshot check: when nothing new appears in two consecutive iterations, the fixed point is reached.
- **Edges, not values** (stated under "Type inference" above). The two structural enforcers: mirrors between attachments go through `Edge(target)` — see writeback's `MethodOnClass{class, name} → Edge(Symbol(sid))`, the canonical shape ("the edge IS the mirror") — and re-emittable passes clear-and-emit (next bullet). A materialized `InferredType` re-pushed onto an edge-reachable attachment is the parallel-store bug the staircase deleted.
- **Re-emittable passes are clear-and-emit.** Every builder pass that legitimately re-derives its bag contribution per iteration calls `WitnessBag::remove_by_source_tag(...)` at the start of every run, then re-pushes from current state. Current re-emittable tags: `arity_detection` (per-Symbol UnionOnArgs), `method_call_return` (call-site `Edge(MethodOnClass{...})`), `local_return` / `plugin_bridge` / `inheritance` (writeback), `call_binding` (propagator). Chain typing's TC-existence check serves the same role for chain-assignment witnesses. Anything else added to a fold step that pushes witnesses MUST follow this idempotency pattern or the worklist will spin.
- **Walker only observes.** No walk-time function returns a type without first emitting the witness. `emit_expr_witness` is the canonical entry; `expr_payload` carries the closed-syntax bake for literals and the `Edge` payloads for name-dependent shapes. Consumers that need a type at walk time do `emit_expr_witness(node); bag_query_expr_span(span)` — emit first, query after.
- **Source priority breaks ties.** `WitnessSource::priority()` returns 100 for `Plugin(_)` and 10 for everything else. The `PluginOverrideReducer` runs first in the registry and short-circuits when it sees a higher-priority witness on a Symbol attachment. New "this answer must dominate" sources go on this priority axis — never as a special-case branch in another reducer.
- **`TypeConstraint` writes go through `push_type_constraint(tc)`.** The struct is an input-parameter shape; the helper translates it into Variable witnesses on the bag. No `Vec<TypeConstraint>` field exists — the bag is the only storage. Direct bag pushes that bypass `push_type_constraint` are allowed (it's just a sugar wrapper) but miss the `ClassAssertion` / `FirstParamInMethod` Observation companions; prefer the helper for `InferredType::ClassName` / `FirstParam` payloads.

### Debugging type inference

`perl-lsp --dump-package <root> <package>` runs full server startup (workspace index, SQLite warm, on-demand @INC resolve, enrichment) then dumps every sub in `<package>` as JSON. Per sub: bag-resolved params, `return_type`, arity-projected returns at 0/1/2/None, witness count, framework, parents, plus:

- **`return_type_provenance`** — traces every non-default return type. `PluginOverride{plugin_id, reason}`, `ReducerFold{reducer, evidence}` (e.g. `reducer="return_arms"`), `Delegation{delegation_kind, via}`. Wire new derivation paths via `Builder.type_provenance` keyed by SymbolId; flushes into `FileAnalysis.type_provenance`. Variants in `file_analysis.rs::TypeProvenance`.
- **`vars_in_scope`** — every TC scoped to the sub's body. Surfaces chain assignment results: `$route` typed as `Mojolicious::Routes::Route` → chain typer worked. Combine with provenance on each method in the chain to find which hop broke.

## tree-sitter-perl gotchas

Inspect any snippet's CST with `perl-lsp --parse <file>` (or `echo '...' | perl-lsp --parse --`) rather than guessing node kinds/fields.

- `subroutine_declaration_statement` / `method_declaration_statement` — fields: `name`, `body`, `lexical`.
- `variable_declaration` — `variable` (single) or `variables` (paren list).
- `package_statement` / `class_statement` / `use_statement` — field: `name` / `module`.
- `function_call_expression` (with parens) vs `ambiguous_function_call_expression` (no parens, includes `bless { ... }`).
- `method_call_expression` — `invocant`, `method`.
- `scalar` / `array` / `hash` = sigil + varname.
- `child_by_field_name("right")` on `assignment_expression` returns `(` paren — iterate `named_child(i)` instead.
- `child_by_field_name("hash")` on `$obj->{key}` returns None — use first named child.
- ERROR nodes wrap subs in incomplete source — scan ERROR children for patterns like `my ($self) = @_`.
- **Fat-comma `=>` has NO code semantics — it is a comma that autoquotes a bareword LHS.** `a => 1` is *identical* to `'a', 1`; `{ A => 1, B => 2 }` is identical to `{ 'A', 1, 'B', 2 }`. So for **correctness** you must **never** gate pair-list walking (`use` import args, `%EXPORT_TAGS`/`%hash` literals, `use constant {…}`, Moo `has` options, Sub::Exporter `exports`/`groups`) on the `fat_comma`/`=>` node — that silently drops the plain-comma spelling (this exact bug hid `use constant { 'GAMMA', 3 }`). Treat the `{…}`/`(…)` as a **flat positional sequence**: pair `elem[2k]`→key, `elem[2k+1]`→value, autoquoting bareword keys, identically whether the separator was `,` or `=>`. Requiring `=>` is a rule-#10 shape-branch; use the shared positional-pair-walking helpers rather than re-deriving.
  - But `=>` **does carry *human* semantics** — a human writes it to signal "the LHS is a key/label, the RHS its value." That's a legitimate **hint** we may lean on in the future (e.g. to disambiguate an otherwise-ambiguous list, or to bias which element is the "name"). Allowed as a *heuristic/tie-breaker*, **never** as a hard gate — code that drops the plain-comma form is still a bug. Keep the correctness path separator-agnostic; treat `=>` only as extra signal on top.

## Inheritance & frameworks

- `package_parents: HashMap<String, Vec<String>>` — unified from `use parent`, `use base`, `@ISA`, `class :isa`, `class :does`, `with` (Moo/Moose), `__PACKAGE__->load_components` (DBIC).
- `resolve_method_in_ancestors()` — DFS parent walk (Perl's default MRO), depth limit 20. `MethodResolution::Local { class, sym_id }` vs `CrossFile { class }`.
- `parents_of(class, package_parents, module_index, consumers)` is the **single** parent-enumeration seam: local ∪ cross-file ∪ the synthetic `APP_SURFACE_CLASS` edge for manifest-declared `app_surface_consumers` (the Mojo helper/plugin "app surface", `docs/adr/plugin-system.md`). `for_each_ancestor_class`, `collect_ancestor_methods`, and the `MethodOnClass` walk in `witnesses.rs` all route through it so the edge is injected once. The consumer set rides `FileAnalysis.app_surface_consumers` (baked from the plugin manifest) and `BagContext`.
- `complete_methods_for_class` walks ancestors, dedups by name (child shadows parent).
- Frameworks (`FrameworkMode::{Moo, Moose, MojoBase}`) detected per-package from `use`. `has 'name' => (...)` synthesizes Method symbol + HashKeyDefs (constructor key + internal hash key). `isa` constraints map to `InferredType` (`Str`/`Int`/`HashRef`/`InstanceOf['X']`/...). Mojo::Base accessors get fluent `ClassName(current_package)` return. **Accessor-option vocabulary (`predicate`/`clearer`/`writer`/`reader`/`builder`/`handles`) lives in `frameworks/moo.rhai`, not core** — core's `extract_has_options` walks the `has` CST (rule #1) into a decision-ready `CallContext.has_options`; the plugin owns "which keyword → which method name + return edge". New Moo behavior goes in the plugin. The default accessor / isa / constructor-key synthesis is still native in `visit_has_call` (a larger move; this seam is the path for everything past it). **Role-ness is plugin-declared too**: the `role_makers()` manifest (base engines in `frameworks/moo.rhai`) feeds an open set; the builder bakes the per-package verdict as `FileAnalysis.role_packages` and `is_role_package` reads it — core holds no engine list (`docs/adr/role-contracts.md`).
- DBIC: `__PACKAGE__->add_columns` / `has_many` / `belongs_to` / `has_one` / `might_have` synthesize accessors with typed returns.
- Synthesized methods are standard symbols — completion/hover/goto-def/inheritance just work, including cross-file (resolver runs full builder).

## Workspace indexing

- `workspace_index` indexed at startup (Rayon `par_iter`, `.gitignore`-aware via `ignore` crate, 1MB cap, `catch_unwind` per file). Standard extensions (`*.pm`/`*.pl`/`*.t`) are type-pruned at the walk; extensionless ENTRYPOINT scripts (`#!/usr/bin/env perl` — Mojo::Lite apps where `plugin 'X'` loads live) are found by a SHALLOW shebang scan over root + `bin/` + `script/` (`scan_entrypoint_scripts`; its `extra: &[String]` param is the seam for a future workspace-config `entrypoint_dirs` — callers pass `&[]` today). Don't broaden the entrypoint scan to a recursive walk — it would enumerate non-Perl source trees.
- File watcher via `workspace/didChangeWatchedFiles` for incremental updates (`spawn_blocking`).
- Query priority: `documents` (open, freshest) → `workspace_index` (all project files) → `module_index` (external @INC).

## LSP capabilities

documentSymbol, definition, references, hover, rename (+ prepareRename), completion, signatureHelp, inlayHint, documentHighlight, selectionRange, foldingRange, formatting (perltidy), rangeFormatting, semanticTokens/full, codeAction (auto-import), linkedEditingRange, workspace/symbol, diagnostics (unresolved function/method warnings).

## Key dependencies

`tower-lsp 0.20` (`#[tower_lsp::async_trait]`), `tree-sitter 0.25`, `ts-parser-perl` (exports `LANGUAGE: LanguageFn`), `dashmap 6`, `rusqlite 0.32` (bundled), `ts-parser-pod`, `serde 1` + `bincode 1` + `zstd 0.13` (FileAnalysis cache blob).
