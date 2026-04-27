# perl-lsp

A Perl LSP server built on ts-parser-perl (crates.io) and tower-lsp.

## Build & test

```
cargo build --release
cargo test                              # unit tests
./run_e2e.sh                            # e2e tests (needs nvim + release build)
perl-lsp --dump-package <root> <pkg>    # debug type inference for a package
```

## Architecture

Four layers, data flows down only:

```
LSP adapter      symbols.rs, backend.rs       → LSP protocol types
Cross-file       module_index/_resolver/_cache → CachedModule (Arc<FileAnalysis>)
Builder          builder.rs                   → produces FileAnalysis
Data model       file_analysis.rs             → FileAnalysis (serde, bincode-cacheable)
```

See `docs/prompt-unification-spec.md` for the cross-file unification roadmap (phases 2–5 landed; 1, 6–7 pending).

### Rules (read before writing code)

1. **All tree-sitter CST traversal happens inside `build()`.** No other file walks tree-sitter nodes, calls `child_by_field_name`, or uses `TreeCursor`. To add CST-derived data: extend `visit_*` in `builder.rs`. Builder plugins (separate modules taking `&mut FileAnalysis` + `&Tree` + `&[u8]`) are fine — they preserve the single entry point. Multiple post-walk passes inside `build()` are allowed and named (see "Build pipeline phases") for cases needing resolved state.

2. **`file_analysis.rs` is the single source of truth.** All analysis results live in `FileAnalysis`. Query methods belong here. No `tree_sitter` imports.

3. **`symbols.rs` is a thin adapter** — `FileAnalysis` types → LSP types. No analysis, no tree walks, no Perl semantics decisions.

4. **`module_resolver.rs` calls the builder, then queries `FileAnalysis`** — never walks the tree directly.

5. **DRY: shared extraction logic goes on `FileAnalysis`.** Two callers needing the same data → one method, both call it.

6. **`cursor_context.rs` is the position-dependent exception** — gets a tree + source for completion/sig-help context. Does NOT modify `FileAnalysis`.

7. **Every meaningful token gets a ref.** If `ref_at(pos)` returns nothing or returns too-broad, the builder is missing emission. Overlapping refs → `ref_at` returns the **narrowest span**. Common gaps: fat-comma keys in calls (`connect(timeout => 30)` needs its own `HashKeyAccess`), hash literal keys, framework-synthesized entities (Moo `has name` → `HashKeyDef` for constructor, not just accessor).

8. **Plugin-synthesized content is owned by `PluginNamespace`, not Perl classes.** See `docs/prompt-plugin-architecture.md`. Cross-file lookup goes through `ModuleIndex::for_each_entity_bridged_to(class, ...)` — do NOT add parallel reverse indexes (retired: `class_content_index`, `modules_with_class_content`). Plugins have **emit hooks** (`on_use`, `on_function_call`, `on_method_call`) — parse-time, declarative, return `Vec<EmitAction>` — and **query hooks** (`on_signature_help`, `on_completion`) — cursor-time, imperative, for shape-dependent behavior. `Silent` / `exclusive` answers suppress native paths when plugin knows native will mishandle the slot.

9. **Provenance: derived refs trace to source** for rename/cross-ref. Constant folding (`my $m = 'process'; $self->$m()`), `has` declarations (accessor + constructor key + internal hash key), import lists, return hash keys → caller derefs (`HashKeyOwner::Sub`), package→file path, inherited overrides.

### File map

- `main.rs` — entry, CLI modes (`--rename`, `--workspace-symbol`, `--dump-package`, `--version`). `cli_full_startup(root)` = "act like LSP just started".
- `backend.rs` — `LanguageServer` impl, request routing.
- `document.rs` — open-file `Document` (tree + text + analysis + stable_outline).
- `file_store.rs` — unified store for open + workspace FileAnalyses, role-tagged, dedup'd by path.
- `file_analysis.rs` — data model; serde-derived.
- `builder.rs` — CST→FileAnalysis. ONLY tree-sitter consumer.
- `pod.rs` — POD→markdown via tree-sitter-pod.
- `cursor_context.rs` — position-dependent context.
- `symbols.rs` — LSP adapter.
- `resolve.rs` — cross-file `refs_to` + `RoleMask` (OPEN/WORKSPACE/DEPENDENCY/BUILTIN). All cross-file queries route here.
- `module_index.rs` — cross-file dep API, `CachedModule`, `SubInfo`, reverse index (`func → modules`).
- `module_resolver.rs` — background `std::thread`, in-process parsing, workspace indexing (Rayon).
- `module_cache.rs` — SQLite (schema v9, bincode+zstd FileAnalysis blob).
- `cpanfile.rs` — cpanfile via tree-sitter queries.
- `witnesses.rs` — witness bag + reducer registry for type inference.

## Cross-file resolution

- `ModuleIndex` runs a dedicated `std::thread` for FS I/O (never blocks tokio). `Arc<DashMap>` shared with async handlers.
- `CachedModule { path, analysis: Arc<FileAnalysis> }` — full FileAnalysis survives module boundary (refs, type_constraints, call_bindings, framework_imports, package_parents). `SubInfo<'_>` view gives ExportedSub-style accessors.
- Reverse index: `DashMap<func_name, Vec<module_name>>` for O(1) exporter lookup.
- SQLite cache per project at `~/.cache/perl-lsp/<hash>/modules.db`. `EXTRACT_VERSION` bump triggers priority re-resolution without dropping the table.
- **Plugin fingerprint** — bundled plugin sources + every `.rhai` in `$PERL_LSP_PLUGIN_DIR` are hashed. Mismatch on startup hard-clears modules table (same machinery as `validate_inc_paths`). Editing a plugin invalidates cache so QA isn't served stale blobs.
- Async handlers only call `_cached` methods (zero I/O).
- After resolution, diagnostics refresh for all open files (clears stale false positives).

### Cross-file enrichment

`enrich_imported_types_with_keys()` on `FileAnalysis` propagates imported return types and hash keys: pushes `TypeConstraint`s for call bindings to imported funcs, injects synthetic `HashKeyDef` symbols for cross-file hash-key completion. Idempotent: `base_type_constraint_count` / `base_symbol_count` / `base_witness_count` set after initial build; enrichment truncates back to baseline before appending. `rebuild_enrichment_indices()` rebuilds index maps after. Called from `publish_diagnostics()` and the resolver refresh callback.

## Type inference (witness bag)

**The bag is THE single type-query path.** `TypeConstraint`, call bindings, framework synthesis, cross-file enrichment — all *feeders* into `FileAnalysis.witnesses`. Queries read via reducers. There is no second path, no escape hatch.

**Two strict phases:**

1. **Collect** in `Builder::populate_witness_bag()`: mirror every `TypeConstraint` as Variable witness; push `HashRefAccess` from `$v->{k}` refs; push `ReturnOfName(method)` on method-call Expression attachments (powers fluent-chain receivers); push `mutation` Facts on hash-key writes; drain walk-time idiom witnesses (branch arms, arity gating). Untyped-policy facts only.

2. **Reduce** via `ReducerRegistry` in `src/witnesses.rs`. Built-in reducers:
   - `FrameworkAwareTypeFold` — folds `TypeObservation` using package's `FrameworkFact` (Moo/Moose/MojoBase/Plain). Class-identity dominates rep when frameworks agree (Mojo `$self->{x} = ...; return $self` keeps `ClassName`). `BlessTarget(Rep)` pins the rep axis. Temporal: skips witnesses past the query point.
   - `BranchArmFold` — agreement across `BranchArm(InferredType)` per arm; needs ≥2 arms to claim. Single-arm ifs yield to `Symbol.return_type`. Claims `Variable` / `Symbol` / `Expression` attachments uniformly via `emit_branch_arm_witnesses`.
   - `FluentArityDispatch` — `ArityReturn { arg_count, return_type }` per arm. Walk classifies (Zero/Exact(N)/Default) but does NOT bake type — type comes from bag-aware fold in `resolve_return_types`. Only emitted for genuinely arity-discriminated subs.
   - `NamedSubReturn` — `NamedSub(name)` attachment for cross-file imports. Pushed by enrichment; same path as locals.

**Single shared query path.** `query_variable_type(bag, scopes, framework, var, scope, point)` and `query_sub_return_type(bag, symbols, name, arity_hint)` in `witnesses.rs` are the ONLY scope-walk + framework + reducer dispatch sites. `Builder::var_type_via_bag` (build-time) and `FileAnalysis::inferred_type_via_bag` / `sub_return_type_at_arity` (query-time) are thin wrappers. Identical rules in both by construction.

**Bag-and-`type_constraints` stay in sync.** Always go through `FileAnalysis::push_type_constraint(tc)` — writes both. Direct `type_constraints.push` is forbidden outside the builder seed step. Enrichment truncates both before re-deriving.

**Query entry points** (call these, not internal helpers):

- `inferred_type_via_bag(var, point)` — variable type at a point.
- `sub_return_type_at_arity(name, arity)` — sub return type (locals + imports).
- `method_call_return_type_via_bag(ref_idx)` — Expression-attached `ReturnOfName` fold (lets `$r->get('/x')->to('Y#z')` resolve `->to`'s receiver without an intermediate variable).
- `mutated_keys_on_class(class)` — dynamic-key completion on `$self->{`.

**Legacy:** `FileAnalysis::inferred_type(var, point)` is raw-state introspection only (no framework/branch/arity rules). Doc-flagged "not a type query." Two narrow uses: `resolve_method_call_types` early-out, and tests asserting on raw builder state. Use `inferred_type_via_bag` everywhere else.

**Adding type behavior = adding a reducer.** Never bypass the bag with direct `InferredType` writes or parallel query helpers. New fact → push a witness; new fold → write a reducer.

**Cache durability:** `WitnessBag` and `package_framework` are `#[serde(default)]`, ride bincode+zstd cache blob. Bump `EXTRACT_VERSION` on shape/rule changes.

### Build pipeline phases

`build_with_plugins()` in `builder.rs` runs in fixed order. Each pass consumes state the previous produced.

1. **live walk** (`visit_*`) — emits Symbols/Refs/Scopes/TypeConstraints, queues plugin emissions, records ReturnInfos with walk-time-typed values.
2. `resolve_variable_refs()` — scalar refs → `resolves_to`.
3. `resolve_hash_key_owners()` — HashKeyAccess → HashKeyOwner via TC types.
4. `apply_type_overrides()` — plugin manifests pin Sub return types inference can't reach (`TypeProvenance::PluginOverride`). Runs BEFORE the witness fold so dependent inference sees overrides.
5. `populate_witness_bag()` — one-shot bag seed. Bag is canonical after this.
6. `resolve_return_types()` — **fold pass 1**. Witness fold per Sub/Method, agreement combinator, delegation + `self_method_tails` propagate to fixed-point. Skips `PluginOverride` subs so step 4 wins.
7. `type_assignments_into_bag(tree)` — chain-typing pass. Walks every `assignment_expression`, types rhs via `resolve_invocant_class_tree` (single recursive expression typer — handles scalar/method-chain/bareword/shift/$_[0]/function-call uniformly). Lands as TC + Variable witness in lockstep. **Must run after step 6** — chain rhs may walk through other subs' return types (`my $route = $self->_route(...)->requires(...)->to(...)`).
8. `refresh_return_arm_types(tree)` — re-evaluate `ReturnInfo.inferred_type` against richer state. Same typer the live walk used. Only upgrades `None → Some`.
9. `resolve_return_types()` — **fold pass 2**. Closes the chain-vs-return chicken-and-egg (`_generate_route` folds only after `$route` is typed; tail-delegation cascades through every verb method).
10. `resolve_invocant_classes_post_pass(tree)` — fill `invocant_class` on MethodCall refs the live walk left empty (`get_foo()->bar()` needs `get_foo`'s now-known return type). Same recursive typer.
11. `resolve_tail_pod_docs()` — POD docs for subs lacking preceding doc.
12. `FileAnalysis::new(...)` — construct FA, build indices, `resolve_method_call_types(None)` as text-based MCB fallback.
13. `fa.finalize_post_walk()` — seal `base_*_count` for idempotent re-enrichment.

`Builder::resolve_invocant_class_tree` is the **single** symbolic-execution function (steps 7, 8, 10). Adding a second is wrong; add cases. `FileAnalysis::resolve_expression_type` is the FA-side mirror at query time (cursor context, hover, completion) — keep them in sync.

### Debugging type inference

`perl-lsp --dump-package <root> <package>` runs full server startup (workspace index, SQLite warm, on-demand @INC resolve, enrichment) then dumps every sub in `<package>` as JSON. Per sub: bag-resolved params, `return_type`, arity-projected returns at 0/1/2/None, witness count, framework, parents, plus:

- **`return_type_provenance`** — traces every non-default return type. `PluginOverride{plugin_id, reason}`, `ReducerFold{reducer, evidence}` (e.g. `reducer="return_arms"`), `Delegation{delegation_kind, via}`. Wire new derivation paths via `Builder.type_provenance` keyed by SymbolId; flushes into `FileAnalysis.type_provenance`. Variants in `file_analysis.rs::TypeProvenance`.
- **`vars_in_scope`** — every TC scoped to the sub's body. Surfaces chain assignment results: `$route` typed as `Mojolicious::Routes::Route` → chain typer worked. Combine with provenance on each method in the chain to find which hop broke.

## tree-sitter-perl gotchas

- `subroutine_declaration_statement` / `method_declaration_statement` — fields: `name`, `body`, `lexical`.
- `variable_declaration` — `variable` (single) or `variables` (paren list).
- `package_statement` / `class_statement` / `use_statement` — field: `name` / `module`.
- `function_call_expression` (with parens) vs `ambiguous_function_call_expression` (no parens, includes `bless { ... }`).
- `method_call_expression` — `invocant`, `method`.
- `scalar` / `array` / `hash` = sigil + varname.
- `child_by_field_name("right")` on `assignment_expression` returns `(` paren — iterate `named_child(i)` instead.
- `child_by_field_name("hash")` on `$obj->{key}` returns None — use first named child.
- ERROR nodes wrap subs in incomplete source — scan ERROR children for patterns like `my ($self) = @_`.

## Inheritance & frameworks

- `package_parents: HashMap<String, Vec<String>>` — unified from `use parent`, `use base`, `@ISA`, `class :isa`, `class :does`, `with` (Moo/Moose), `__PACKAGE__->load_components` (DBIC).
- `resolve_method_in_ancestors()` — DFS parent walk (Perl's default MRO), depth limit 20. `MethodResolution::Local { class, sym_id }` vs `CrossFile { class }`.
- `complete_methods_for_class` walks ancestors, dedups by name (child shadows parent).
- Frameworks (`FrameworkMode::{Moo, Moose, MojoBase}`) detected per-package from `use`. `has 'name' => (...)` synthesizes Method symbol + HashKeyDefs (constructor key + internal hash key). `isa` constraints map to `InferredType` (`Str`/`Int`/`HashRef`/`InstanceOf['X']`/...). Mojo::Base accessors get fluent `ClassName(current_package)` return.
- DBIC: `__PACKAGE__->add_columns` / `has_many` / `belongs_to` / `has_one` / `might_have` synthesize accessors with typed returns.
- Synthesized methods are standard symbols — completion/hover/goto-def/inheritance just work, including cross-file (resolver runs full builder).

## Workspace indexing

- `workspace_index` indexed at startup (Rayon `par_iter`, `.gitignore`-aware via `ignore` crate, 1MB cap, `catch_unwind` per file).
- File watcher via `workspace/didChangeWatchedFiles` for incremental updates (`spawn_blocking`).
- Query priority: `documents` (open, freshest) → `workspace_index` (all project files) → `module_index` (external @INC).

## LSP capabilities

documentSymbol, definition, references, hover, rename (+ prepareRename), completion, signatureHelp, inlayHint, documentHighlight, selectionRange, foldingRange, formatting (perltidy), rangeFormatting, semanticTokens/full, codeAction (auto-import), linkedEditingRange, workspace/symbol, diagnostics (unresolved function/method warnings).

## Key dependencies

`tower-lsp 0.20` (`#[tower_lsp::async_trait]`), `tree-sitter 0.25`, `ts-parser-perl` (exports `LANGUAGE: LanguageFn`), `dashmap 6`, `rusqlite 0.32` (bundled), `ts-parser-pod`, `serde 1` + `bincode 1` + `zstd 0.13` (FileAnalysis cache blob).
