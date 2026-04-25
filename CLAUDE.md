# perl-lsp

A Perl LSP server built on ts-parser-perl (crates.io) and tower-lsp.

## Build

```
cargo build          # debug
cargo build --release  # optimized
```

## Architecture

### Layers (strict — read this before writing any code)

The codebase has four layers. Data flows **down** only. Each layer may only depend on layers below it.

```
  LSP adapter      symbols.rs, backend.rs       → LSP protocol types
  ─────────────────────────────────────────────────────────────────
  Cross-file       module_index.rs,             → CachedModule (path + Arc<FileAnalysis>)
                   module_resolver.rs,            plus SubInfo<'_> view helpers.
                   module_cache.rs                No lossy per-module summary type.
  ─────────────────────────────────────────────────────────────────
  Builder          builder.rs                   → produces FileAnalysis
  ─────────────────────────────────────────────────────────────────
  Data model       file_analysis.rs             → FileAnalysis, Symbol, Ref, types
                                                  (serde-derived graph, serializable
                                                  via bincode for SQLite storage)
```

Unification refactor status: phases 2, 3, 4, 5 landed.
- **Phase 2** — full FileAnalysis stored for deps (no more ModuleExports fidelity cliff).
- **Phase 3** — unified `FileStore` collapses `documents` + `workspace_index` (src/file_store.rs). Dedup: opening a workspace file promotes it to `Open`; closing demotes back.
- **Phase 4** — `resolve.rs` + `RoleMask`; cross-file references now search workspace + deps (previously single-file).
- **Phase 5** — `refs_by_target: HashMap<SymbolId, Vec<RefId>>` index + eager HashKeyAccess linkage to HashKeyDef at build time. `find_references` no longer requires a tree to match hash-key accesses; `Backend::references` now walks cross-file for hash keys too. Pin-the-fix tests in `file_analysis::tests::test_phase5_*` and `resolve::tests::test_refs_to_*`.
- **Phase 5 follow-ups** — `HashKeyOwner::Sub` is package-qualified (no more cross-file name collisions); qualified calls (`Pkg::foo()`) produce `call_bindings`; delegation chains (`sub chain { return other() }`) propagate return types + hash-key ownership; dynamic method dispatch (`$obj->$m()` where `$m='name'`) resolves via constant folding through `MethodCallBinding`.

Phases 1 (Namespace) and 6–7 still pending; see `docs/prompt-unification-spec.md`.

Type inference + invariant derivation: `docs/prompt-type-inference-spec.md` captures what's landed and the next set of modeling directions. Spec parts 6 (framework-aware resolver) and 7 (fact-witness bag + reducers) are now landed in code — see "Witness bag + reducers" below.

**Rules:**

1. **All tree-sitter CST traversal happens inside `build()`.** No other file should walk tree-sitter nodes, call `child_by_field_name`, iterate children, or use `TreeCursor`. The `build()` function in `builder.rs` is the single entry point that takes a `Tree` and returns a `FileAnalysis` — everything that needs the CST lives inside that call. **To add new CST-derived data:** add extraction to the relevant `visit_*` method in `builder.rs` and store the result in `FileAnalysis` (as a field on `Symbol`, a new map, etc.). The builder already visits every sub, package, class, and variable node — use that pass, don't create a second one. Builder plugins (separate modules called from `build()` that take `&mut FileAnalysis` + `&Tree` + `&[u8]`) can decouple framework-specific concerns while preserving the single-entry-point invariant.

2. **`file_analysis.rs` is the single source of truth.** All analysis results — symbols, refs, scopes, types, documentation, parameters — live in `FileAnalysis`. Query methods belong here. No `tree_sitter` imports allowed in this file.

3. **`symbols.rs` is a thin adapter.** It converts `FileAnalysis` types to LSP protocol types. It does NOT perform analysis, walk trees, or make decisions about Perl semantics. If you find yourself writing an `if` about Perl language behavior in `symbols.rs`, it belongs in `builder.rs` or `file_analysis.rs`.

4. **`module_resolver.rs` calls the builder, then queries `FileAnalysis`.** It should never walk the tree directly. The resolver's job is: find `.pm` file → call `builder::build()` → extract what it needs from the resulting `FileAnalysis` via query methods.

5. **DRY: shared extraction logic goes on `FileAnalysis`.** If two code paths need the same data from a `FileAnalysis`, add a method to `FileAnalysis` that both call. Never duplicate the extraction loop.

6. **`cursor_context.rs` is the exception:** it receives a tree + source for cursor-position analysis (completion context, signature help context). This is acceptable because cursor context is inherently position-dependent and runs on the already-parsed tree. It should NOT modify `FileAnalysis`.

7. **Every meaningful token gets a ref.** Every token the user might put their cursor on should have a `Ref` that explains what it means in context. If a token is meaningful but `ref_at()` returns nothing (or returns a wrong/too-broad ref), the builder is missing a ref emission. This is how completion, goto-def, hover, rename, and references all work — they start with the ref at the cursor position.

   Common gaps to watch for: fat-comma keys in call arguments (`connect(timeout => 30)` — `timeout` needs its own `HashKeyAccess` ref, not just the enclosing `MethodCall`), hash literal keys (`{ status => 'ok' }` — the `HashKeyDef` must be findable via `symbol_at`/`ref_at`), and framework-synthesized entities (Moo `has name` should produce `HashKeyDef` entries for the constructor, not just accessor methods).

   When multiple refs overlap at a position, `ref_at` must return the **most specific** (narrowest span). A `HashKeyAccess` for `timeout` inside a `MethodCall` for `connect` should win over the `MethodCall`.

8. **Plugin-synthesized content is owned by `PluginNamespace`, not Perl classes.** See `docs/prompt-plugin-architecture.md` for the full model. Short form:

   - A `PluginNamespace` (in `file_analysis.rs`) is a plugin-controlled scope with `id`, `kind`, `bridges`, and `entities` (SymbolIds). Helpers / routes / tasks live in namespaces, NOT as methods bolted onto `Mojolicious::Controller` etc. Multi-instance (two apps in one workspace) works because each gets its own namespace id.

   - `Bridge::Class(name)` declares "Perl expressions typed as `name` can see this namespace's entities". The single cross-file lookup is `ModuleIndex::for_each_entity_bridged_to(class, |cached, sym|) { ... }` — it walks `bridges_index`, applies the namespace-level `Bridge::Class` filter, and the per-entity `sym.package` filter. No other reverse indexes for plugin content.

   - **Retired** (do not reintroduce): `class_content_index` / `modules_with_class_content`. They inferred plugin reach from `symbol.package`; explicit plugin-declared bridges replaced them. If a new lookup needs to find plugin-emitted methods on a class, route through `for_each_entity_bridged_to` — do NOT add a parallel reverse index.

   - Plugins have TWO kinds of hooks:
     - **Emit hooks** (`on_use`, `on_function_call`, `on_method_call`) run at parse time. Plugin returns `Vec<EmitAction>` for the builder to apply. Declarative.
     - **Query hooks** (`on_signature_help`, `on_completion`) run at cursor time in `symbols.rs`. Plugin inspects `SigHelpQueryContext` / `CompletionQueryContext` and returns an answer. Imperative. Used for shape-dependent behavior (arrayref-wrapped handler args, dispatcher name slots) that data emission can't express.

   - `PluginSigHelpAnswer::Silent` / `PluginCompletionAnswer.exclusive: bool` let a plugin claim a cursor slot without offering content — suppresses native paths that would mishandle it. Use sparingly: only when the plugin knows native will get it wrong.

   - `PluginCompletionAnswer.dispatch_targets_for: Option<DispatchTargetRequest>` asks the core to populate completion items from Handler symbols with a given owner + dispatcher names. Plugins delegate rather than re-walking the symbol table.

9. **Provenance: refs should trace back to their source.** When a ref is derived from another value (constant folding, import re-export, framework synthesis), the derivation chain should be traceable for rename and cross-referencing. Key provenance chains:

   - **Constant folding:** `my $m = 'process'; $self->$m()` → the `MethodCall` ref targeting `"process"` was derived from the string literal `'process'`. Renaming `process` should update the source string.
   - **`has` declarations:** `has name => (is => 'ro')` is a single source of truth that produces: an accessor Method symbol, a `HashKeyDef` for the constructor (`->new(name => ...)`), and a `HashKeyDef` for the internal hash (`$self->{name}`). Renaming any one should update all.
   - **Import lists:** `use Foo qw(bar)` — the string `bar` in the import list should rename when `sub bar` in Foo is renamed.
   - **Return hash keys → caller derefs:** `sub get_config { return { host => ... } }` then `$cfg->{host}` — the key `host` in the caller is derived from the return hash. `HashKeyOwner::Sub("get_config")` links them.
   - **Package name → file path:** Renaming `MyApp::Controller::Users` could offer to rename/move the `.pm` file.
   - **Inherited overrides:** Renaming `Animal::speak` should surface `Dog::speak` for coordinated rename.

### File map

- `src/main.rs` — Entry point, stdio transport, CLI modes (`--rename`, `--workspace-symbol`, `--dump-package`, `--version`). `cli_full_startup(root)` is the shared "act like the LSP server just started" helper used by every cross-file CLI command.
- `src/backend.rs` — `LanguageServer` trait implementation (tower-lsp), request routing. Open + workspace files live in the shared `FileStore`.
- `src/document.rs` — `Document` (tree + text + analysis + stable_outline) for open files
- `src/file_store.rs` — Unified store for open + workspace FileAnalyses (role-tagged, dedup'd by path)
- `src/file_analysis.rs` — Data model: scopes, symbols, refs, imports, type inference, priority constants. Serde-derived.
- `src/builder.rs` — Single-pass CST → FileAnalysis builder (the ONLY tree-sitter consumer)
- `src/pod.rs` — POD→markdown converter (tree-sitter-pod AST walk, handles nested formatting/lists/data regions)
- `src/cursor_context.rs` — Cursor position analysis: completion/signature/selection context
- `src/symbols.rs` — LSP adapter layer (converts FileAnalysis types to LSP types)
- `src/resolve.rs` — Cross-file `refs_to` + `RoleMask` (OPEN, WORKSPACE, DEPENDENCY, BUILTIN). Every cross-file query routes through here.
- `src/module_index.rs` — Cross-file dep API, `CachedModule`, `SubInfo`, reverse index (`func → modules`), concurrent cache
- `src/module_resolver.rs` — Background resolver thread, in-process parsing, workspace indexing (Rayon) into FileStore
- `src/module_cache.rs` — SQLite persistence (schema v9, bincode+zstd of FileAnalysis), mtime validation
- `src/cpanfile.rs` — cpanfile parsing via tree-sitter queries
- `src/witnesses.rs` — Two-phase type inference: (1) typed witnesses attached to variables/expressions/symbols/hash-keys, (2) reducer registry that folds the bag into a single `InferredType`. See "Witness bag + reducers" below.

## Key Dependencies

- `tower-lsp 0.20` — LSP framework (uses `#[tower_lsp::async_trait]`)
- `tree-sitter 0.25` — Parsing
- `ts-parser-perl` — crates.io, exports `LANGUAGE: LanguageFn`
- `dashmap 6` — Concurrent document store + module cache
- `rusqlite 0.32` — SQLite persistence for module index (bundled)
- `ts-parser-pod` — crates.io, POD→AST for documentation rendering
- `serde 1` (derive) + `bincode 1` + `zstd 0.13` — FileAnalysis serialization for the module cache blob

## tree-sitter-perl Node Types

Key nodes and their fields:
- `subroutine_declaration_statement` — fields: `name` (bareword), `body` (block), `lexical` ("my")
- `method_declaration_statement` — same fields as above
- `variable_declaration` — fields: `variable` (scalar/array/hash), `variables` (paren list)
- `package_statement` — field: `name` (package)
- `class_statement` — field: `name` (package)
- `use_statement` — field: `module` (package)
- `function_call_expression` — field: `function`, `arguments`
- `ambiguous_function_call_expression` — field: `function`, `arguments` (no-paren calls)
- `method_call_expression` — fields: `invocant`, `method`
- `scalar` = "$" + varname, `array` = "@" + varname, `hash` = "%" + varname

## Testing

```
cargo test                    # unit tests
cargo build --release && ./run_e2e.sh   # e2e tests (requires nvim + release build)
```

E2e tests use Neovim headless mode. They exercise the full LSP protocol over stdio.

## Cross-file Module Resolution

- `ModuleIndex` uses a dedicated `std::thread` for filesystem I/O (never blocks tokio).
- `Arc<DashMap>` shared between resolver thread and async LSP handlers.
- Cache value is `Option<Arc<CachedModule>>` — `CachedModule { path: PathBuf, analysis: Arc<FileAnalysis> }`. **Full FileAnalysis survives the module boundary** — refs, type_constraints, call_bindings, method_call_bindings, imports, framework_imports, complete package_parents.
- `SubInfo<'_>` view on `CachedModule` gives callers the old `ExportedSub`-style accessors (`def_line`, `params`, `is_method`, `return_type`, `doc`, `hash_keys`) without a separate summary type.
- Reverse index: `DashMap<func_name, Vec<module_name>>` for O(1) exporter lookup.
- Export extraction runs in-process (no subprocess isolation — tree-sitter-perl grammar is stable).
- cpanfile parsed with tree-sitter queries at startup, deps pre-resolved with progress reporting.
- SQLite cache per project (`~/.cache/perl-lsp/<hash>/modules.db`), **schema v9** with a single `analysis BLOB` column containing `zstd(bincode(FileAnalysis))`. `EXTRACT_VERSION` still bumps on builder changes to trigger priority re-resolution without dropping the table.
- **Plugin fingerprint** (`plugin::rhai_host::plugin_fingerprint`) hashes bundled plugin sources + every `.rhai` in `$PERL_LSP_PLUGIN_DIR`. Stored in the `meta` table; mismatch on startup hard-clears the modules table (same machinery as `validate_inc_paths`). Without this, editing a plugin and restarting the LSP would serve stale FileAnalysis blobs — making plugin QA impossible.
- Async handlers only use `_cached` methods — zero I/O.
- After resolution, diagnostics are refreshed for all open files (clears stale false positives).

### Witness bag + reducers (type inference, two phases)

`InferredType` is a flat sum (`HashRef | ArrayRef | ClassName(_) | …`) — fine for "what is this", terrible for "what do all these observations together say". The witness pipeline splits inference into two phases. **The split is strict.** Collection records raw, untyped-policy facts. Reduction applies framework rules and produces typed answers. There is no third "we already folded a bit at collection time" layer, and there are no escape hatches that bypass the bag.

**Phase 1 — collect.** During the builder's single pass:

- Every `TypeConstraint` is mirrored into `FileAnalysis.witnesses` (a `WitnessBag`) as a `Variable`-attachment witness. Class-identity constraints (`ClassName`, `FirstParam`) also push the corresponding `TypeObservation` so the framework-aware reducer can see them.
- Every `$v->{k}` ref pushes a `HashRefAccess` observation against `$v`.
- Every method call ref pushes a `ReturnOfName(method)` observation on the call's `Expression` attachment (powers fluent-chain receiver resolution).
- Every write through a class- or sub-owned hash key pushes a `mutation` `Fact` on the `HashKey` attachment.
- Walk-time idiom detectors (branch arms, arity gating) push directly via `Builder.pending_witnesses`, drained into the bag at the end of the walk.

This happens once, in `Builder::populate_witness_bag()`. After that point the bag is the source of truth for the witness pipeline; nothing else seeds it.

**Phase 2 — reduce.** Reduction goes through a `ReducerRegistry` (`src/witnesses.rs`). Each `WitnessReducer` claims a subset (attachment kind + source + payload family) and folds into a `ReducedValue`. Built-in reducers:

- **`FrameworkAwareTypeFold`** — Part 6. Folds `TypeObservation` payloads using the enclosing package's `FrameworkFact` (Moo / Moose / MojoBase / Plain). Class-identity dominates rep observations when the framework's backing rep agrees — so `sub name { … $self->{name} = … ; return $self }` inside a Mojo::Base class keeps `$self` as `ClassName(Foo)`. `BlessTarget(Rep)` pins the rep axis (arrayref-backed classes). Temporal ordering: witnesses past the query point are skipped, so a later `$x = Foo->new` can't poison an earlier `$x->[0]` site.
- **`BranchArmFold`** — Part 5b. `BranchArm(InferredType)` per arm; agreement → that type, disagreement → `None`. Claims `Variable` (`my $x = $c ? A : B`), `Symbol` (`if/else { return A } else { return B }` and `return $c ? A : B`), and `Expression` attachments uniformly. Builder emits via the attachment-agnostic `emit_branch_arm_witnesses(cond_expr, attachment, ctx)`.
- **`FluentArityDispatch`** — Part 6b. `ArityReturn { arg_count, return_type }` per arity-gated return branch. The walk *classifies* each `return_expression`'s arity branch (Zero / Exact(N) / Default) but does NOT bake a type into the witness — the type comes from the bag-aware fold in `Builder::resolve_return_types`, same phase as `Symbol.return_type` reduction. Witnesses are emitted only for subs with at least one Zero/Exact arm (genuinely arity-discriminated); plain subs leave their default to `Symbol.return_type` so agreement across arms isn't bypassed. The reducer matches exact-arity, falls back to agreement-folded Default.
- **`NamedSubReturn`** — Plain `InferredType` payload on `NamedSub(name)` attachments. Pushed by `enrich_imported_types_with_keys` for every cross-file return type. Lets `query_sub_return_type` resolve imports through the SAME bag path it uses for locals — no parallel `imported_return_types` lookup at consumer sites.
- **`BranchArmFold`** requires **two or more** arms to claim a result — a single `if (cond) { return X }` is not "agreement", and the reducer yields to the next path so `Symbol.return_type` (which sees ALL return arms) wins.

**Single shared query path.** `witnesses::query_variable_type(bag, scopes, package_framework, var, scope, point)` is the *only* place the scope-chain walk + framework-fact lookup + reducer dispatch lives for variable lookups. `witnesses::query_sub_return_type(bag, symbols, sub_name, arity_hint)` is the parallel for sub returns — handles **local symbols** (via `Symbol` attachment) and **imported subs** (via `NamedSub` attachment, pushed by enrichment) through one path. Both `Builder::var_type_via_bag` (used during build for the return-arm fold) and `FileAnalysis::inferred_type_via_bag` / `sub_return_type_at_arity` (used at query time) are thin wrappers over these helpers. Identical rules in both places by construction. No "if no local symbol, take a different path" escape hatches.

**Single point of reduction for `Symbol.return_type`.** `Builder::resolve_return_types` runs *after* `populate_witness_bag()`. Per `return $var` site it asks the bag (one query, framework-aware). Per literal/constructor return it uses the type the walk could read off directly. Then `resolve_return_type`'s agreement combinator yields the sub's return type. Delegation chains and `self_method_tails` propagate after that. Call-binding constraints (`my $cfg = get_config()`) are pushed in the same pass — both into `self.type_constraints` and into `self.bag` as a Variable witness, so the bag stays canonical.

**Bag-and-`type_constraints` stay in sync — always.** Direct `self.type_constraints.push(...)` is no longer used outside the builder's seed step. `FileAnalysis::push_type_constraint(tc)` writes to both tables in lockstep. The local post-pass (`resolve_method_call_types(None)`) and cross-file enrichment (`enrich_imported_types_with_keys`) both go through it. `base_witness_count` is sealed alongside `base_type_constraint_count` after `finalize_post_walk()`, and enrichment truncates *both* before re-deriving — so repeat enrichment is fully idempotent.

**Query entry points on `FileAnalysis`** (the canonical names — internal helpers exist but consumers should call these):

- `inferred_type_via_bag(var, point)` — variable type at a point. Returns owned `InferredType`. Goes through `query_variable_type`. The bag is canonical (every constraint flows through `push_type_constraint`); no fallback to a legacy linear scan in production.
- `sub_return_type_at_arity(name, arity)` — sub return type. Goes through `query_sub_return_type`, which dispatches to `Symbol` attachment for locals and `NamedSub` for imports through the same reducer registry.
- `method_call_return_type_via_bag(ref_idx)` — Expression-attached `ReturnOfName` fold. Lets `$r->get('/x')->to('Y#z')` resolve the receiver of `->to` without an intermediate variable.
- `mutated_keys_on_class(class)` — Part 1 hook for dynamic-key completion on `$self->{`. Reads `mutation` facts on `HashKey` attachments.

**Legacy holdouts (use only for raw-state introspection, NOT type queries):**

- `FileAnalysis::inferred_type(var, point) -> Option<&InferredType>` — raw linear scan of `type_constraints` with no framework / branch / arity rules. Doc-flagged as "not a type query." Two narrow legitimate uses: the `resolve_method_call_types` early-out (`.is_some()` check on raw constraint state), and tests asserting on what the builder pushed. Anything else should call `inferred_type_via_bag`.

**Cache durability.** Both `WitnessBag` and `package_framework` are `#[serde(default)]` and ride the existing bincode + zstd module-cache blob. `EXTRACT_VERSION` bumps when the bag's shape or fold rules change, so stale cache entries re-resolve with priority instead of serving empty bags.

The bag is **additive**: `TypeConstraint` / `CallBinding` / `MethodCallBinding` / framework synthesis / cross-file enrichment all keep working. Witnesses layer on top for cases the flat sum couldn't express; new behavior lands as new reducers, not new variants on `InferredType`.

### Debugging the inference pipeline

`perl-lsp --dump-package <root> <package>` runs the same startup as the LSP server (workspace index, SQLite cache warm, on-demand @INC resolve, post-build enrichment), then dumps every sub in `<package>` as JSON: name, params with bag-resolved types, raw `return_type`, `sub_return_type_at_arity` projections at arity 0/1/2/None, `return_self_method` tail, witness count on the sub's symbol, framework, parents, doc summary. Use it to inspect what the reducers are folding and where chains die. Workspace packages are found by symbol scan; external @INC packages come from `module_index.get_cached(name)` populated by the startup pass.

### Cross-file type enrichment

Post-build enrichment propagates imported return types and hash keys into the local `FileAnalysis`. This is a layered step — the builder's single-pass architecture is unchanged.

- `enrich_imported_types_with_keys()` on `FileAnalysis`: pushes `TypeConstraint`s for call bindings referencing imported functions, injects synthetic `HashKeyDef` symbols for cross-file hash key completion
- `sub_return_type()` falls back to `imported_return_types` map after checking local subs; `sub_return_type_local()` checks only local symbols (used by the enrichment method to avoid self-referencing)
- Idempotency: `base_type_constraint_count` / `base_symbol_count` set after initial build; enrichment truncates back to baseline before appending, so repeated calls don't accumulate duplicates
- `rebuild_enrichment_indices()` rebuilds `type_constraints_by_var`, `symbols_by_name`, and `symbols_by_scope` after enrichment
- Backend wiring: `Arc<ModuleIndex>` on `Backend`, refresh callback uses `OnceLock<Arc<ModuleIndex>>` for deferred init (resolver thread has no tokio context, spawns async work via captured `Handle`)
- `enrich_analysis()` called from `publish_diagnostics()` and from the refresh callback (which re-enriches all open documents when a module resolves)
- `InferredType` is serde-derived; signature help still emits string tags (`"HashRef"`, `"Object:Foo"`, ...) via `inferred_type_to_tag` for its `param_types` LSP JSON field. All SQLite storage now uses bincode, not tags.
- `EXTRACT_VERSION` tracks builder output version; stale entries loaded from cache but re-resolved with priority

### Inheritance chain resolution

- `FileAnalysis.package_parents: HashMap<String, Vec<String>>` — unified parent map from all inheritance sources
- Sources: `use parent`, `use base`, `@ISA = (...)`, `class :isa(Parent)`, `class :does(Role)`, `with 'Role'` (Moo/Moose), `__PACKAGE__->load_components(...)` (DBIC) — all feed `package_parents` during builder walk
- `resolve_method_in_ancestors()` does DFS parent walk (matching Perl's default MRO), depth limit 20
- `MethodResolution` enum: `Local { class, sym_id }` for same-file, `CrossFile { class }` for module index lookup
- `complete_methods_for_class` walks ancestors, deduplicates by name (child methods shadow parent)
- Cross-file: `ModuleExports.parents` stored in SQLite `parents` TEXT column (JSON array)
- `ModuleIndex.parents_cached(module_name)` returns parent list for cross-file inheritance walking

### Framework accessor synthesis

- `FrameworkMode` enum (`Moo`, `Moose`, `MojoBase`) detected per-package from `use` statements
- Moo/Moose: `has 'name' => (is => 'ro'/'rw'/'lazy'/'rwp'/'bare')` synthesizes `SymKind::Method` symbols
- `isa` type constraints mapped to `InferredType`: `Str` → String, `Int`/`Num` → Numeric, `HashRef`/`ArrayRef`/`CodeRef`, `InstanceOf['Foo']` → ClassName, Moose class names
- Mojo::Base: `has 'name'` produces rw accessor with fluent return type `ClassName(current_package)`; `use Mojo::Base 'Parent'` also feeds `package_parents`
- DBIC: `__PACKAGE__->add_columns(...)` synthesizes column accessors; `has_many`/`belongs_to`/`has_one`/`might_have` synthesize relationship accessors with typed returns
- Synthesized methods are standard symbols — completion, hover, goto-def, inheritance all work automatically
- Cross-file: resolver runs full builder on each module, so framework accessors appear in `ModuleExports.subs` for cross-file resolution

### Cross-file param types

- `ExportedParam.inferred_type: Option<String>` carries body-inferred param types across file boundaries
- Param type serialized as `"type"` field in JSON for SQLite storage; `SignatureInfo.param_types` delivers pre-resolved types for cross-file signature help

### Workspace indexing

- `workspace_index: Arc<DashMap<PathBuf, FileAnalysis>>` — full `FileAnalysis` for every `.pm`/`.pl`/`.t` in the workspace
- Indexed at startup with Rayon `par_iter` + `ignore` crate for `.gitignore` respect. `catch_unwind` per file, 1MB size cap
- File watcher via `workspace/didChangeWatchedFiles` for incremental re-indexing (runs in `spawn_blocking`)
- Query priority: `documents` (open files, freshest) → `workspace_index` (all project files) → `module_index` (external `@INC` modules)
- Enables `workspace/symbol` search and cross-file rename across all project files, not just open ones
- Benchmarks: 274-file Mojolicious in 204ms, 657-file DBIx-Class in 167ms (release build)

## LSP Capabilities

- `textDocument/documentSymbol` — outline of subs, packages, variables, classes (with fields/methods as children)
- `textDocument/definition` — go-to-def for variables (scope-aware), subs, methods (type-inferred), packages/classes, hash keys; resolves through expression chains
- `textDocument/references` — scope-aware for variables, file-wide for functions/packages/hash keys; resolves through expression chains
- `textDocument/hover` — shows declaration line, inferred types, return types, class-aware for methods
- `textDocument/rename` — scope-aware for variables; cross-file for functions/methods/packages (searches documents + workspace index); `prepareRename` support
- `textDocument/completion` — scope-aware variables (cross-sigil forms), subs, methods (type-inferred with return type detail), packages, hash keys, auto-import from cached modules, deref snippets for typed references, module names on `use` lines, import lists inside `qw()`
- `textDocument/signatureHelp` — parameter info with inferred types for subs/methods (signature syntax + legacy @_ pattern), triggers on `(` and `,`
- `textDocument/inlayHint` — type annotations for variables (Object/HashRef/ArrayRef/CodeRef) and sub return types
- `textDocument/documentHighlight` — highlight all occurrences with read/write distinction
- `textDocument/selectionRange` — expand/shrink selection via tree-sitter node hierarchy
- `textDocument/foldingRange` — blocks, subs, classes, pod sections
- `textDocument/formatting` — shells out to perltidy (respects .perltidyrc)
- `textDocument/rangeFormatting` — perltidy on selected line range
- `textDocument/semanticTokens/full` — variable tokens with modifiers: scalar/array/hash, declaration, modification
- `textDocument/codeAction` — auto-import for unresolved functions
- `textDocument/linkedEditingRange` — simultaneous editing of all references in scope
- `workspace/symbol` — search symbols across all workspace-indexed files
- Diagnostics — unresolved function/method warnings (skips builtins, local subs, imported functions)
