# Unification Refactor Spec

## Summary

Collapse the LSP's three parallel file-analysis stores and its two parallel "named thing" models (`Symbol`/`Ref` vs. `FrameworkEntity`) into a single uniform graph. Every Perl definition — user code, framework-synthesized accessor, external `@INC` module export — lives as a `Symbol`. Every reference — bareword, method call, hash key, string literal at a known call site — lives as a `Ref` pointing at a `Symbol`. Every file — open, workspace, dependency — stores a full `FileAnalysis` in a single store distinguished only by a role tag.

The goal is representation independence: adding a new framework, a new LSP feature, or a new file source should require zero parallel plumbing.

## Motivation

The codebase today has three patterns that all tell the same story: features don't flow automatically through the data model, so each new thing needs hand-wiring.

### Retrofit commits

Samples from the last few months where a feature had to be re-wired after the fact:

- `8cd703a` — added `PackageRef` refs for `use parent` + `FunctionCall` refs for imported symbols. These should have flowed through from the start.
- `6ba8846` — added tree/source params to `rename_at()` to enable hash key owner resolution inside the query. Rename worked for some hash keys and not others because resolution was lazy and context-dependent.
- `9af3a48` — `rename_sub` had to learn to check both `FunctionCall` and `MethodCall` refs. Bespoke per-kind matching where one `refs_by_target` lookup should suffice.
- `a206335` — references + signature help had to learn to walk inheritance. Parent resolution was silently ignored by some queries.

These aren't bug fixes. They're the sound of an abstraction failing.

### Parallel graph proposals

The Mojo intelligence spec (`docs/prompt-mojo-intelligence.md`, pre-update) proposed a `FrameworkEntity` vector as a parallel data structure alongside `Symbol`/`Ref`. Every feature would then need per-`EntityKind` branches in goto-def, references, rename, completion, hover. We recognized this as a combinatorial dead end and chose instead to lift `Symbol`/`Ref` into a string-namespace view — which this spec formalizes.

### Fidelity cliff at module boundaries

`ModuleExports` strips from `FileAnalysis`:

- all `refs` (so cross-file references is broken)
- all `type_constraints` / `call_bindings` / `method_call_bindings` (so cross-file type inference truncates)
- all `imports` (so we can't see transitive imports)
- `framework_imports` (so framework-synthesized calls across files lose context)
- secondary `package_parents` entries (only the primary package's parents survive)
- hash key defs outside of return-value analysis

A workspace file stores a full `FileAnalysis`. An `@INC` module stores a lossy `ModuleExports`. Same conceptual data, wildly different fidelity. This is the biggest single source of "cross-file feature mysteriously degraded."

### Three file stores, per-query tier walks

`Backend.documents` (open files), `Backend.workspace_index` (all workspace files), `ModuleIndex.cache` (deps). Each LSP feature independently decides which subset to query:

- `goto_def`: documents → module_index. Skips workspace_index.
- `find_references`: documents + workspace_index. Skips module_index.
- `completion`: documents + module_index for imports. Skips workspace_index for local completion.
- `hover`: documents → module_index.

No single "resolve this reference anywhere" function. Each query hand-picks tiers and forgets one.

## Guiding principle: equivalence, not parallelism

When a new feature's natural data is equivalent to something already in the model — just in a different namespace or a different storage tier — **extend the existing graph**. Do not build a parallel one.

The equivalences to exploit:

1. A Mojo helper / Minion task / event name is equivalent to a sub definition; the name happens to live in a string literal rather than a bareword.
2. A framework scope (Mojo app, plugin, role composition) is equivalent to a Perl package with synthetic parent edges.
3. A workspace file, a dependency file, and an open file are equivalent storages for a `FileAnalysis`; they differ only in write permissions and change cadence.
4. An unresolved reference in a library and a genuine typo in an endpoint are equivalent events at the ref layer; they differ only in whether the containing namespace chain terminates `Closed`.

Each of those equivalences, once recognized, collapses two-graphs-worth of wiring into a single annotation plus emission rule.

## Core invariants (target state)

After all phases land, these are the properties the codebase holds:

1. **Every meaningful token has a `Ref`.** Every bareword, method name, hash key, package name, and framework-specific string-literal-at-a-known-call-site emits a `Ref` during builder walk. (Already the stated rule in CLAUDE.md, not currently enforced.)

2. **Every `Ref` resolves to a `Symbol` at build time.** `Ref.target: SymbolId` is non-optional; when emission can't determine the target, the target is the reserved `UNRESOLVED` symbol (with the original name preserved for diagnostic emission). No post-pass lazy resolution during queries.

3. **Every `Symbol` has a home namespace.** Every `Symbol` carries the namespace it lives in. Namespaces are Perl packages or framework-specific scopes (Mojo app, plugin, action).

4. **Namespace parents form a graph.** `namespace_parents: HashMap<Namespace, Vec<Namespace>>` represents inheritance, role composition, plugin loading, and under-chain bridging. A single DFS walker (already exists as `resolve_method_in_ancestors`) traverses it.

5. **Every namespace has a derived `Openness`.** `Closed` namespaces (endpoints, scripts, concrete classes) expect refs to resolve; `Open` namespaces (roles, plugins, abstract bases) tolerate unresolved refs because consumers will provide them.

6. **One file store, role-tagged.** `files: Arc<FileStore>` with `FileEntry { role: FileRole, analysis: Arc<FileAnalysis>, ... }`. All entries store full `FileAnalysis` — no lossy summary type.

7. **One `resolve()` function.** All queries (goto-def, references, rename, completion, hover, diagnostics) call a single `resolve_symbol(name, from_namespace, role_mask)` that walks the namespace graph across the unified file store.

## Implementation ordering

| Phase | Scope | Status | Depends on |
|-------|-------|--------|------------|
| 1 | `Namespace` enum + `namespace_parents` widening | **Deferred** — pluggability TBD | — |
| 2 | Kill `ModuleExports`; full `FileAnalysis` in SQLite | **Next** | — |
| 3 | Merge `workspace_index` into unified `FileStore` | Queued | 2 |
| 4 | Unified `resolve()` + role-masked query dispatch | Queued | 3 |
| 5 | Eager `Ref` target resolution + `refs_by_target` | Queued | — |
| 6 | `Openness` classification + unified diagnostic rule | Queued | 1 |
| 7 | Framework emission rules (lands Mojo intelligence) | Blocked | 1, 5 |

Phase 1 is deferred by request; the pluggability question (enum-only vs. catch-all variant for third-party framework extension) needs more thought. Phases 2–4 can proceed without phase 1 because they operate on the existing string-based `package_parents` and introduce no new semantic concepts — they're pure storage and dispatch unification.

Phase 5 is independent of 1–4 but scheduled after 4 because its payoff is largest once the unified store exists (cross-file `refs_by_target` becomes meaningful). It could be pulled earlier if query-tax pain demands it.

Phases 6 and 7 block on phase 1.

---

## Part 1: Namespace generalization [DEFERRED]

### Motivation

Today `package_parents: HashMap<String, Vec<String>>` represents inheritance between Perl packages. It handles `use parent`, `use base`, `@ISA`, `use Mojo::Base 'Parent'`, `class :isa(Parent)`, `class :does(Role)`, `with 'Role'` (Moo/Moose), `__PACKAGE__->load_components(...)` (DBIC).

It does not handle cross-framework scopes:

- A Mojo app's helpers live conceptually in the app's namespace, not any single Perl package.
- A plugin's helpers register into the host app's namespace at load time, not the plugin's package.
- Route actions live in an action-specific scope that inherits from the controller, the app, and any `under()`-chain actions.
- Stash keys are scoped per-action, with parent edges from under() chain bridging.

We need one scope-parent graph that handles all of these uniformly, so the existing inheritance walker carries framework features automatically.

### Design sketch

```rust
enum Namespace {
    Package(String),
    MojoApp(String),
    MojoPlugin(String),
    MojoController(String),
    MojoAction { controller: String, action: String },
    LiteApp(PathBuf),
    // Pluggability TBD — see open question below.
}
```

`package_parents` → `namespace_parents: HashMap<Namespace, Vec<Namespace>>`. All ~45 call sites get the enum key. The walker in `resolve_method_in_ancestors` accepts `&Namespace`.

Implicit edges added at emission time:

- `MojoController(X) ← MojoApp(app_of(X))`
- `MojoAction(ctrl, action) ← MojoController(ctrl)`
- `MojoAction(ctrl, action) ← MojoAction(ctrl', under_action)` for each `under` that bridges to it
- `MojoApp(host) ← MojoPlugin(p)` added when host file contains `$self->plugin('p')`

SQLite: canonical `Display` form for the enum so storage stays TEXT (e.g. `pkg:MyApp`, `mojo-app:MyApp`, `mojo-plugin:Mojolicious::Plugin::Auth`, `mojo-action:MyApp::Controller::Users#list`).

### Open question: pluggability

- **Enum-only.** Rust's exhaustive-match checks every site, guaranteeing no forgotten branches. But adding a framework = recompile.
- **`Custom { kind: String, name: String }` catch-all.** Any framework can introduce a Namespace at runtime. Loses exhaustiveness.
- **Hybrid.** Known frameworks get enum variants; `Custom { kind, name }` covers the long tail. Best of both; exhaustiveness on hot paths, open enrollment for the rest.

**Decision deferred.** Resume this discussion when ready.

### Concrete changes (when phase lands)

- New file: `src/namespace.rs` (or add to `file_analysis.rs`).
- Widen `Symbol.package: Option<String>` → `Symbol.home_namespace: Namespace`.
- Widen `package_parents` → `namespace_parents`.
- Rewrite ~45 call sites; type system drives the refactor.
- SQLite schema bump: `namespace` TEXT via Display/FromStr.

---

## Part 2: Kill ModuleExports

### Motivation

`ModuleExports` exists because dep-extraction used to run in isolated subprocesses for crash protection during early tree-sitter-perl grammar instability. Subprocesses meant serialized summaries across IPC.

Extraction is now in-process. The grammar is stable. The compression earns no rent — but it costs us every field listed in the Motivation section above: `refs`, `type_constraints`, `call_bindings`, `method_call_bindings`, `imports`, full `package_parents`, `framework_imports`, most `hash_keys`.

Concretely: cross-file references is broken because dep files have no refs table. Cross-file type inference truncates because call bindings don't travel. Hovering on a method call whose type originated in a dep's local variable loses the type at the module boundary.

This is the single highest-ROI unification step because it eliminates the fidelity cliff entirely.

### Current state

Key shapes (`src/module_index.rs`):

```rust
pub struct ModuleExports {
    pub path: PathBuf,
    pub export: Vec<String>,
    pub export_ok: Vec<String>,
    pub subs: HashMap<String, ExportedSub>,
    pub parents: Vec<String>,
}

pub struct ExportedSub {
    pub name: String,
    pub def_line: u32,
    pub params: Vec<ExportedParam>,
    pub is_method: bool,
    pub return_type: Option<String>,
    pub hash_keys: Vec<String>,
    pub doc: Option<String>,
}
```

Resolver path (`src/module_resolver.rs`): `resolve_module()` calls `builder::build()` → full `FileAnalysis` → `collect_export_metadata()` compresses to `ModuleExports` → stored in `ModuleIndex.cache` and SQLite.

SQLite schema is v8 with `subs` JSON column and `parents` JSON column plus `EXTRACT_VERSION` tracking.

Query path: async handlers call `ModuleIndex.get_exports_cached(name)` and work with `ModuleExports` directly; they cannot reach the underlying `FileAnalysis` because it was thrown away.

### Design

Three changes:

1. **Delete `ModuleExports`, `ExportedSub`, `ExportedParam`, `SignatureInfo`, `collect_export_metadata()`, and the `EXTRACT_VERSION` counter.**

2. **Cache stores full `FileAnalysis`.** `ModuleIndex.cache: DashMap<String, Arc<FileAnalysis>>`. SQLite schema v9: one `analysis BLOB` column holding `zstd(bincode(FileAnalysis))`.

3. **Query methods return `Arc<FileAnalysis>`.** `get_exports_cached(name) → Option<Arc<FileAnalysis>>`. Callers that previously iterated `ModuleExports.subs` iterate `fa.symbols.iter().filter(|s| fa.is_exported(s))`. Callers that read `ModuleExports.parents` read `fa.package_parents[primary_package]`.

### Invariants after phase 2

- Dep `FileAnalysis` has identical fidelity to workspace `FileAnalysis`.
- No code path produces or consumes a "partial" `FileAnalysis`.
- SQLite contents round-trip: deserialized cache entry equals a fresh `build()` of the same source (modulo tree-sitter tree, which is not serialized).

### Concrete changes

**Modified files:**

- `src/module_index.rs`:
  - Drop `ModuleExports`, `ExportedSub`, `ExportedParam`, `SignatureInfo`.
  - `cache: DashMap<String, Arc<FileAnalysis>>`.
  - Query methods (`get_exports_cached`, `parents_cached`, `find_exporters`, `get_modules_exporting`, `complete_for_import`) rewritten as thin views over `FileAnalysis`. Where they used to return `ExportedSub`, they return either `Arc<FileAnalysis>` + symbol name, or a minimal `ResolvedSymbol` wrapper that carries (module_name, sym_id).
  - Reverse index keys stay the same (`func_name → Vec<module_name>`) but are populated from `fa.symbols` filtered to exports.

- `src/module_cache.rs`:
  - Schema v9: `CREATE TABLE modules (name TEXT PRIMARY KEY, path TEXT, mtime INTEGER, size INTEGER, analysis BLOB)`.
  - `insert(name, path, &FileAnalysis)` serializes via `bincode::serialize` + `zstd::encode_all`.
  - `load(name) → Option<Arc<FileAnalysis>>` deserializes.
  - Migration from v8: drop + rebuild. Dep cache is derived; no user data is lost.

- `src/module_resolver.rs`:
  - Stop calling `collect_export_metadata()`. Store `Arc<FileAnalysis>` directly.
  - Delete `EXTRACT_VERSION` logic.
  - `new_for_test()` / `spawn_test_resolver()` adapt to the new signature.

- `src/file_analysis.rs`:
  - `enrich_imported_types_with_keys()` reads from the resolved dep's `Arc<FileAnalysis>` instead of `ModuleExports.subs`. The method is much simpler now — it calls `fa.symbols.iter()` filtered to exports.
  - `sub_return_type()` / `sub_return_type_local()` unchanged in signature; internally consult dep FileAnalyses via module_index.
  - Add `pub fn is_exported(&self, sym: &Symbol) -> bool` helper (consults `self.export` / `self.export_ok` or module convention).
  - Add `#[derive(Serialize, Deserialize)]` to `FileAnalysis`, `Symbol`, `Ref`, `Scope`, `ScopeId`, `SymbolId`, `TypeConstraint`, `InferredType`, `CallBinding`, `MethodCallBinding`, `HashKeyOwner`, `AccessKind`, `RefKind`, `SymKind`, `Span`, `FoldKind`, `FoldRange`, `ParamInfo`.

- `src/backend.rs`, `src/symbols.rs`:
  - Callsites that referenced `ExportedSub.return_type`, `.params`, `.hash_keys`, `.doc`, etc., now read from `Symbol` / adjacent `FileAnalysis` fields directly.

- `src/main.rs`: CLI modes that touched `ModuleExports` (e.g. JSON dumps for `--workspace-symbol`) rewritten to use the richer model.

**Cross-crate type serialization:**

- `tree_sitter::Point`: add serde wrapper `PointRepr { row: u32, col: u32 }` or enable tree-sitter's `serde` feature if available. Check feature flags first.
- `tree_sitter::Range`, `tree_sitter::Tree`: Tree is not serialized (re-parse on demand if needed). Range uses the same Point wrapper.

**Dependencies to add (Cargo.toml):**

```toml
bincode = "1"
zstd = "0.13"
serde = { version = "1", features = ["derive"] }
```

### Tradeoffs

- **SQLite size.** Full FileAnalysis is 5–20× larger than ModuleExports uncompressed. zstd brings it to ~2–4× on typical modules. Acceptable; benchmark during implementation. If unacceptable for very large caches, revisit: lazy rehydration (compressed blob in memory, decompress on demand), or per-field dropping (e.g., omit `refs` from serialization but that's the whole point of this phase so no).
- **Serde derive reach.** ~15 types get serde derives; all are pure data. One-time lift.
- **Migration.** Schema v8 → v9 is rebuild-on-start (dep cache is a derived artifact; drop and re-resolve). Resolver handles this invisibly.
- **bincode vs. postcard vs. JSON.** bincode: small, fast, non-self-describing. postcard: smaller, also non-self-describing, no-std-friendly. JSON: bigger, human-debuggable. Start with bincode + zstd; switch to JSON temporarily during debugging if cache contents ever need inspection.
- **Serde for tree-sitter types.** If tree-sitter lacks serde support, wrapper types add ~20 lines of shim.

### Done when

- `cargo test` passes.
- `cargo build --release && ./run_e2e.sh` passes.
- Dep cache rebuilds on first launch (schema migration).
- `grep -rE "ModuleExports|ExportedSub|ExportedParam" src/` returns nothing.
- Spot-check: cross-file hover on a method call whose return type was inferred from a local variable in the dep shows the inferred type (previously lost at the module boundary).
- Spot-check: `find_references` on a sub defined in an `@INC` module returns call sites from within that module (previously nothing — deps had no refs).

---

## Part 3: Merge workspace_index into unified file store

### Motivation

Today:

- `Backend.documents: Arc<DashMap<Url, Document>>` — open files. Full FileAnalysis, **enriched** with imported types.
- `Backend.workspace_index: Arc<DashMap<PathBuf, FileAnalysis>>` — every .pm/.pl/.t. Full FileAnalysis, **not enriched**, queried only by `workspace/symbol` and rename.
- `ModuleIndex.cache: DashMap<String, Arc<FileAnalysis>>` (after phase 2) — @INC modules.

Three maps, three lifecycles, three per-query tier walks. `workspace_index` is not enriched, so cross-file type inference into workspace files degrades compared to open files. Open files are never merged into `workspace_index` even though they *are* workspace files.

One store is simpler to reason about, simpler to invalidate, and eliminates the "workspace file that's open" duplication.

### Design

```rust
pub struct FileEntry {
    pub role: FileRole,
    pub path: PathBuf,
    pub module_name: Option<String>,         // @INC deps have one
    pub url: Option<Url>,                    // open files have one
    pub analysis: Arc<FileAnalysis>,
    pub tree: Option<Tree>,                  // only for Open
    pub stable_outline: Option<StableOutline>, // only for Open
    pub mtime: SystemTime,
    pub enriched: bool,
}

pub enum FileRole {
    Open,        // user is editing
    Workspace,   // project file, not open
    Dependency,  // @INC module
    BuiltIn,     // Perl core (future)
}

pub enum FileKey {
    Path(PathBuf),
    Module(String),
    Url(Url),
}

pub struct FileStore {
    by_path: DashMap<PathBuf, Arc<FileEntry>>,
    by_module: DashMap<String, Arc<FileEntry>>,       // secondary index
    by_url: DashMap<Url, Arc<FileEntry>>,             // secondary index for open files
    reverse_sub_index: DashMap<String, Vec<FileKey>>, // func_name → files defining it
}
```

Rules:

- Open files have `role: Open`, indexed by `path` + `url`.
- A workspace file that becomes open flips from `Workspace` to `Open`; the `Arc<FileEntry>` is replaced (cheap; callers hold Arcs so stale reads see consistent snapshots).
- Dep resolution inserts `role: Dependency` keyed by `path` + `module_name`.
- Closing an open file demotes to `Workspace` (keeping the analysis; tree/stable_outline cleared).
- `enriched: bool` flags whether import-type enrichment has run; queries that need it trigger enrichment on demand and cache the result.

### Invariants after phase 3

- Every file in the project is represented at most once (by_path is the primary index).
- Deduplication: opening a workspace file flips role; does not create a second entry.
- `FileRole` is the sole behavior gate. Query logic is identical across roles.

### Concrete changes

- **New file:** `src/file_store.rs`. Houses `FileStore`, `FileEntry`, `FileRole`, `FileKey`. Exposes `insert`, `get`, `promote_to_open(path|url)`, `demote_to_workspace(url)`, `enrich_if_needed(key)`.

- **`src/backend.rs`:**
  - Replace `documents` and `workspace_index` fields with `files: Arc<FileStore>`.
  - `did_open` calls `files.promote_to_open(url, text)`.
  - `did_close` calls `files.demote_to_workspace(url)`.
  - `did_change` updates the open entry's tree + analysis.
  - `did_change_watched_files` re-indexes workspace role entries.
  - `index_workspace` populates `role: Workspace` entries for paths not already open.

- **`src/module_index.rs`:**
  - `ModuleIndex.cache` becomes a view: `by_module` filtered to `role: Dependency`.
  - Resolver thread inserts via `FileStore.insert(role: Dependency, ...)`.
  - Public API stays stable; internals delegate to `FileStore`.

- **`src/module_resolver.rs`:**
  - Resolver inserts into `FileStore`.
  - File-watcher hooks the same store.

- **Enrichment:**
  - `enrich_analysis()` moves to `FileStore::enrich(file_key)`.
  - Called lazily on first query that needs cross-file types.
  - Idempotent via `base_type_constraint_count` / `base_symbol_count` (existing machinery).
  - Flag `enriched: true` after completion; cleared when the entry's analysis is rebuilt.

- **`src/document.rs`:**
  - `Document` struct shrinks: its fields merge into `FileEntry`. Remove.

### Tradeoffs

- **`Arc` overhead.** Per-file allocation. Negligible at workspace scale.
- **Role transitions.** Atomic swap via DashMap's entry API.
- **Enrichment cost.** Uniform enrichment across workspace files is new CPU. Mitigation: lazy.
- **Concurrency.** Resolver thread inserts; async handlers read. Arc<FileEntry> means readers get consistent snapshots without locking the whole entry. Already the pattern.

### Done when

- `cargo test` passes.
- E2E tests pass.
- `grep -E "workspace_index|Backend.documents" src/` returns only legacy doc comments.
- Feature parity: opening a workspace file shows identical hover/goto-def/references compared to the previous build.
- New behavior: `find-references` on a symbol defined in workspace file A, used in workspace file B, returns results even when B is **not open** (previously broken).

---

## Part 4: Unified resolve() + role-masked query dispatch

### Motivation

Each LSP handler today has its own tier walk. `goto_def` picks documents + module_index; `references` picks documents + workspace_index; `completion` picks documents + module_index-for-imports. Each is a place where a tier can be forgotten (references currently skips deps; this is why "find references across `cpan` modules" doesn't work).

Concentrating tier-walking in one function + role masks makes every query a one-liner and eliminates the class of "forgot a tier" bug.

### Design

```rust
bitflags::bitflags! {
    pub struct RoleMask: u8 {
        const OPEN       = 1 << 0;
        const WORKSPACE  = 1 << 1;
        const DEPENDENCY = 1 << 2;
        const BUILTIN    = 1 << 3;
        const EDITABLE   = Self::OPEN.bits() | Self::WORKSPACE.bits();
        const VISIBLE    = Self::EDITABLE.bits() | Self::DEPENDENCY.bits() | Self::BUILTIN.bits();
    }
}

pub struct ResolvedSymbol {
    pub file: FileKey,
    pub sym_id: SymbolId,
    pub namespace: String,           // or Namespace, post-phase-1
}

pub struct RefLocation {
    pub file: FileKey,
    pub ref_id: RefId,
    pub span: Span,
}

pub fn resolve_symbol(
    files: &FileStore,
    name: &str,
    from: &str,              // from_namespace; String pre-phase-1, &Namespace post
    mask: RoleMask,
) -> Vec<ResolvedSymbol>;

pub fn refs_to(
    files: &FileStore,
    target: &ResolvedSymbol,
    mask: RoleMask,
) -> Vec<RefLocation>;
```

Walking rules:

- `resolve_symbol`: start at `from` namespace, walk `namespace_parents` DFS, check each visited namespace's symbols for a match by name. Filter by `role` mask on each file entry.
- `refs_to`: iterate all files in the mask; for each, consult `fa.refs_by_target.get(target.sym_id)` (phase 5) or fall back to `fa.refs.iter().filter(|r| matches)` until phase 5 lands.

### Query bindings

| Feature | Call |
|---------|------|
| goto_def | `resolve_symbol(name, ns, VISIBLE)` — pick best match (highest specificity, closest in namespace walk) |
| references | `refs_to(target, VISIBLE)` |
| rename | `refs_to(target, EDITABLE)` — exclude deps (read-only) |
| completion | `resolve_symbol(prefix, ns, VISIBLE)` prefix-filtered, then shaped to LSP CompletionItem |
| hover | `resolve_symbol(name, ns, VISIBLE).first()` — best match, formatted |
| workspace/symbol | `resolve_symbol(query, root_ns, EDITABLE)` |
| diagnostics (unresolved) | `resolve_symbol(name, ns, VISIBLE).is_empty()` in `Closed` chain → warn (phase 6) |

### Invariants after phase 4

- No LSP handler directly iterates `FileStore.by_path` / `by_module` / `by_url`.
- Every cross-file query goes through `resolve_symbol` or `refs_to`.
- Role masks are explicit at every call site.

### Concrete changes

- **New file:** `src/resolve.rs`. Houses `resolve_symbol`, `refs_to`, `RoleMask`, `ResolvedSymbol`, `RefLocation`.
- **`src/symbols.rs`:** rewrite `goto_definition`, `find_references`, `rename`, `completion_items`, `hover_info` to call `resolve_symbol`/`refs_to`.
- **`src/backend.rs`:** `handle_workspace_symbol` calls `resolve_symbol(query, root, EDITABLE)`.
- **`src/file_analysis.rs`:** `find_references`, `find_definition`, etc., become thin helpers delegating to the unified resolver. Local-only queries stay (scope-aware variable resolution within a file is always single-file); cross-file queries route through `resolve_symbol`.

### Tradeoffs

- **Testing surface.** Every query rewrite is a potential regression. E2E tests before + after each rewrite; add coverage for cross-role cases.
- **Role mask discipline.** New rule: every new query picks a role mask. Lint-friendly once a doc comment convention is set.

### Done when

- `cargo test` passes.
- E2E tests pass including:
  - Rename does not touch dep files.
  - References *does* include dep files (new behavior).
  - Workspace/symbol returns workspace + open results.
- `grep -E "documents\.(get|iter)|workspace_index\.(get|iter)|cache\.(get|iter)" src/` shows hits only inside `src/file_store.rs` and `src/resolve.rs`.

---

## Part 5: Eager Ref target resolution + refs_by_target

### Motivation

Per the earlier audit, refs are emitted during the builder walk with `resolves_to: Option<SymbolId> = None`. Resolution is post-pass, context-dependent, and partially lazy inside queries (see `collect_refs_for_target` needing tree + source). This is the root cause of several "goto-def works but references doesn't" bugs — the ref exists but can't be matched by the query because owner resolution requires a tree that the query doesn't have.

Enforcing eager resolution collapses every per-query re-resolution path into a single index lookup.

### Design

- `Ref.target: SymbolId` (not `Option<SymbolId>`).
- Reserved sentinel `UNRESOLVED: SymbolId = SymbolId(u32::MAX)` for refs whose referent genuinely cannot be determined at build time. `target_name` (the string form) is preserved for diagnostic emission and deferred cross-file resolution.
- Builder post-pass runs full resolution before returning `FileAnalysis`. Hash key owner resolution, method-call receiver resolution, and import-based resolution all happen here.
- `refs_by_target: HashMap<SymbolId, Vec<RefId>>` built during the same post-pass. `UNRESOLVED` refs collect in `refs_by_target[UNRESOLVED]`; queries that want them (diagnostics, auto-import) look there.
- Cross-file resolution (refs whose target lives in another file) resolves during enrichment — once deps are loaded, the `UNRESOLVED` list is re-scanned.

### Invariants after phase 5

- After `build()` returns, every `Ref.target` is a valid `SymbolId` (possibly `UNRESOLVED`).
- No query resolves owners or targets at query time for local refs.
- Cross-file resolution happens in `FileStore::enrich()` and updates `refs_by_target` accordingly.

### Concrete changes

- **`src/file_analysis.rs`:**
  - `Ref.resolves_to: Option<SymbolId>` → `Ref.target: SymbolId`.
  - Add `refs_by_target: HashMap<SymbolId, Vec<RefId>>`.
  - `refs_to(sym_id) → &[RefId]` (public helper).
  - Add `UNRESOLVED` sentinel and `is_unresolved(sym_id)` helper.

- **`src/builder.rs`:**
  - Post-walk pass `resolve_all_refs(&mut fa, tree, source)` runs after the tree walk, before returning.
  - Hash key owner resolution moves from `resolve_hash_key_owners()` into this pass.
  - Inheritance-based method resolution for intra-file method calls happens here (cross-file deferred to enrichment).
  - `debug_assert!(fa.refs.iter().all(|r| r.target != SymbolId::INVALID))` after the pass.

- **Enrichment:**
  - `enrich_imported_types_with_keys` also retries resolution for `UNRESOLVED` refs now that deps are visible.

- **Query rewrites:**
  - `find_references` → `fa.refs_to(sym_id)` + optional kind filter.
  - `rename_at` → `fa.refs_to(sym_id)` + span mapping.
  - `document_highlight` → `fa.refs_to(sym_id)` + read/write split.

### Tradeoffs

- **Build cost.** One additional post-pass over refs. Well within latency budget.
- **Discipline.** Every new emission rule must ensure the ref resolves or explicitly sets `UNRESOLVED`. debug_assert enforces.
- **Cross-file re-resolution.** When a new dep loads, previously-UNRESOLVED refs may resolve. Triggered via enrichment.

### Done when

- `cargo test` passes.
- `grep -rE "Option<SymbolId>" src/file_analysis.rs` returns nothing.
- Spot-check: references on a Moo constructor arg key (`MyClass->new(name => ...)`) shows all call sites including the `has` declaration — previously silently incomplete.
- Spot-check: rename of a helper flows through all call sites uniformly.

---

## Part 6: Openness classification + unified diagnostic rule [POST 1]

### Motivation

Unresolved function/method diagnostics today either fire too eagerly (inside role code, plugins, abstract bases) or miss real bugs (they silently skip anything framework-imported). No single rule handles "this is a library, unresolved is fine" vs. "this is an endpoint, unresolved is a bug."

### Design

- `Openness` is **derived** from namespace markers, not stored as ground truth:
  - `use Moo::Role` / `use Moose::Role` / `use Role::Tiny` → Open.
  - `use Mojo::Base 'Mojolicious::Plugin'` or `sub register` without `sub startup` → Open.
  - `use Mojo::Base 'Mojolicious'` + `sub startup` → Closed.
  - Controllers (`*::Controller::*`) with routes targeting them → Closed.
  - Referenced as a parent by other namespaces but never directly instantiated → Open.
  - `.pl`, `.t`, standalone scripts → Closed.
  - Default: Closed.

- Single diagnostic rule: walk the namespace chain from the ref's site upward via `namespace_parents`. If the chain terminates in Closed without the ref resolving (via `refs_by_target[UNRESOLVED]` membership), warn. If the chain hits Open first, suppress.

- Covers:
  - Unresolved function call.
  - Unresolved method call (including in role bodies where the consumer will provide).
  - Unresolved stash key (per-action namespace).
  - Unresolved route target (controller#action).
  - Unresolved helper/hook reference.

### Concrete changes

- **`src/file_analysis.rs`:**
  - `Openness` enum (`Closed` | `Open`).
  - `namespace_openness(&Namespace) -> Openness` computed during post-build.
  - `chain_terminates_closed(&Namespace) -> bool` walks `namespace_parents`.

- **`src/backend.rs`:**
  - `publish_diagnostics` uses the unified rule across all unresolved kinds.
  - Delete bespoke `framework_imports` suppression once the unified rule subsumes it (framework-synthesized namespaces classified Open suppress correctly).

- **Tests:**
  - Library `.pm` with `use Moo::Role` does not warn on unresolved method calls.
  - Endpoint controller warns on unresolved helpers.
  - Plugin suppresses unresolved-helper warnings for the helpers it defines.

---

## Part 7: Framework emission rules [BLOCKED ON 1, 5]

### Motivation

With the unified model in place, framework synthesis is pure emission — symbols and refs land in the right namespaces automatically, and all LSP features work without per-framework wiring.

See `docs/prompt-mojo-intelligence.md` (updated alongside this spec) for the Mojo-specific emission rules.

### Design

- Decompose `builder.rs` into:
  - `builder.rs` (core walk: scopes, symbols, refs, types, exports).
  - `builder_framework.rs` (Moo/Moose/Mojo::Base: `has`, `extends`, `with`, role composition).
  - `builder_dbic.rs` (DBIC: `add_columns`, `has_many`, `belongs_to`, `load_components`, `__PACKAGE__->table`).
  - `builder_mojo.rs` (Mojo intelligence: routes, helpers, tasks, hooks, stash, plugins, events, Lite DSL).

- Each plugin is a pure emission rule: given tree + source + in-progress FileAnalysis, emit Symbols and Refs into the right Namespaces. No parallel data structures; no per-EntityKind query wiring.

- Post-build extensions run after the main walk, taking `&mut FileAnalysis` + `&Tree` + `&[u8]`.

### Concrete changes

See `docs/prompt-mojo-intelligence.md` for emission-rule tables per feature.

---

## Non-goals

- **Re-parsing on demand for deps.** `FileAnalysis` serialization round-trips the derived data; the tree-sitter `Tree` is not serialized. Features that need the tree re-parse on demand (already how `cursor_context.rs` works for open files). For deps, this is extremely rare.
- **Dynamic Namespace extension at runtime.** Third-party framework plug-ins that register `Namespace` variants are out of scope; depends on the phase-1 pluggability decision.
- **Cross-file rename of deps.** Dep files are read-only. Rename stops at `role: Dependency`.
- **Cross-workspace symbol graph.** Single-workspace only; monorepo multi-root is not in scope.
- **Full Namespace rollout before phase 2.** Phases 2–4 operate on existing `String`-keyed `package_parents`. Phase 1 widens the type afterwards without redoing phases 2–4.

## Open questions

1. **bincode vs. postcard vs. JSON for SQLite.** bincode + zstd is the default; switch during debugging if needed. Postcard is a strong candidate if cache-size matters more than the ~5% bincode edge in speed. JSON is out except as a debug feature flag.
2. **Lazy rehydration for giant dep caches.** Benchmark first. If a fully-hot cache hits >500MB RAM, move to compressed-in-memory with decompress-on-query.
3. **Namespace pluggability.** (Phase 1 open.) Enum-only, `Custom(String)` catch-all, or hybrid.
4. **Openness detection markers.** Is "never directly instantiated" sufficient for abstract base detection, or is an explicit annotation (e.g. `# @abstract`) worth adding for the edge case?
5. **`refs_by_target` memory.** Per-file is fine. Global aggregation on demand via `resolve()`; no global index.
6. **Tree-sitter serde compatibility.** Confirm whether `tree_sitter::Point` and `tree_sitter::Range` have serde support in v0.25, or whether we need thin wrapper types.

## Reference: affected files

| File | Phase | Nature of change |
|------|-------|------------------|
| `src/file_analysis.rs` | 2, 5, 6 | serde derives; eager ref resolution; openness |
| `src/module_index.rs` | 2, 3 | drop ModuleExports; view over FileStore |
| `src/module_resolver.rs` | 2, 3 | store FileAnalysis; insert via FileStore |
| `src/module_cache.rs` | 2 | schema v9; bincode + zstd |
| `src/backend.rs` | 3, 4, 6 | replace stores; unified dispatch; diagnostic rule |
| `src/symbols.rs` | 4 | rewrite handlers to call resolve/refs_to |
| `src/document.rs` | 3 | merge into FileEntry; remove |
| `src/builder.rs` | 5, 7 | eager ref resolution; decompose for framework plugins |
| `src/file_store.rs` | 3 | **new** |
| `src/resolve.rs` | 4 | **new** |
| `src/namespace.rs` | 1 | **new** (deferred) |
| `src/builder_framework.rs` | 7 | **new** |
| `src/builder_dbic.rs` | 7 | **new** |
| `src/builder_mojo.rs` | 7 | **new** |

## Tests

Unit tests in each phase land alongside changes. E2E regression baseline before each phase; diff after. Key e2e assertions per phase:

- **Phase 2:** cross-file hover shows dep-inferred types; cross-file references finds dep call sites.
- **Phase 3:** references on a workspace-only-closed file works.
- **Phase 4:** rename does not touch deps; references crosses into deps.
- **Phase 5:** references on Moo constructor arg keys works.
- **Phase 6:** library .pm does not warn on unresolved self-method calls.
- **Phase 7:** Mojo route goto-def, helper completion, task signature help, event completion — see Mojo spec.
