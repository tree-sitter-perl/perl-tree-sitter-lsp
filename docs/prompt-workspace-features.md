# Workspace Features: Indexing, Symbol, Rename, and Subprocess Removal

## Motivation

Competitive gap analysis against PerlNavigator identified several LSP features we're missing that are trivial to add given our existing infrastructure. More importantly, **workspace indexing** unlocks all of them at full power — not just across open files, but across the entire project.

This spec also retires subprocess isolation for module resolution. The tree-sitter-perl grammar is battle-tested in production; subprocess overhead and the JSON serialization boundary are no longer justified.

---

## Part A: Workspace Indexing

### Problem

Today we have two disconnected stores:

| Store | Contents | Scope |
|-------|----------|-------|
| `documents: DashMap<Url, Document>` | Full `FileAnalysis` + tree-sitter `Tree` | Open files only |
| `module_index: DashMap<String, ModuleExports>` | Exports, params, return types, parents | `@INC` modules (external) |

Project-local files that aren't open are invisible until a `use` statement triggers module_index resolution. This means workspace/symbol, cross-file rename, and cross-file find-references only work across open files.

### Design

#### New store

```rust
workspace_index: Arc<DashMap<PathBuf, FileAnalysis>>,
```

Full `FileAnalysis` for every `.pm`/`.pl`/`.t` file in the workspace. No tree retained (only open files need trees for incremental re-parsing).

**Query priority order** (for any cross-file lookup):
1. `documents` — open files, most up-to-date (has tree for re-parse)
2. `workspace_index` — background-indexed project files
3. `module_index` — external `@INC` modules (exports-only view)

#### File discovery

Scan workspace root for `**/*.pm`, `**/*.pl`, `**/*.t`. Use the `ignore` crate (what ripgrep uses) for `.gitignore` respect. Skip `blib/`, `local/`, `.build/`, `node_modules/`, `_build/`.

New dependency:
```toml
ignore = "0.4"
```

#### Indexing with Rayon

New dependency:
```toml
rayon = "1"
```

```rust
fn index_workspace(
    root: &Path,
    index: &DashMap<PathBuf, FileAnalysis>,
) -> usize {
    let files: Vec<PathBuf> = WalkBuilder::new(root)
        .types(TypesBuilder::new().add("perl", "*.pm").add("perl", "*.pl").add("perl", "*.t").build().unwrap())
        .build()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .filter(|e| e.metadata().map(|m| m.len() < 1_000_000).unwrap_or(false))
        .map(|e| e.into_path())
        .collect();

    let count = AtomicUsize::new(0);

    files.par_iter().for_each(|path| {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let source = std::fs::read_to_string(path).ok()?;
            let mut parser = create_parser();
            let tree = parser.parse(&source, None)?;
            Some(build(&tree, source.as_bytes()))
        }));

        match result {
            Ok(Some(analysis)) => {
                index.insert(path.clone(), analysis);
                count.fetch_add(1, Ordering::Relaxed);
            }
            Ok(None) => { /* parse failed, skip */ }
            Err(_) => {
                log::warn!("Panic while indexing {:?}, skipping", path);
            }
        }
    });

    count.load(Ordering::Relaxed)
}
```

**Safety:** `catch_unwind` around each file's parse+build. If tree-sitter panics (shouldn't happen with a stable grammar, but defense in depth), that file is skipped. File size cap at 1MB. Rayon handles thread count automatically (defaults to num_cpus).

#### When to index

**Startup:** After `initialize()` provides workspace root. Report progress via `window/workDoneProgress` (same pattern as cpanfile indexing). Non-blocking — LSP is usable immediately, workspace features light up as indexing completes.

**File changes:** Register `workspace/didChangeWatchedFiles` for `**/*.pm`, `**/*.pl`, `**/*.t`. On change/create, re-index the file. On delete, remove from index.

```rust
// In initialize(), register watchers:
let registrations = vec![Registration {
    id: "perl-file-watcher".to_string(),
    method: "workspace/didChangeWatchedFiles".to_string(),
    register_options: Some(serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
        watchers: vec![
            FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.pm".into()), kind: None },
            FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.pl".into()), kind: None },
            FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.t".into()), kind: None },
        ],
    }).unwrap()),
}];
client.register_capability(registrations).await;
```

```rust
async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
    for change in params.changes {
        let path = uri_to_path(&change.uri);
        match change.typ {
            FileChangeType::DELETED => {
                self.workspace_index.remove(&path);
            }
            _ => {
                // Re-index the file (created or changed)
                self.reindex_file(&path);
            }
        }
    }
}
```

**Open file override:** When `did_open` fires, the `documents` store takes precedence. When `did_close` fires, the workspace index version is still there. No special handling needed — query priority order handles it.

#### Module index integration

With workspace indexing, **local modules are already indexed with full FileAnalysis**. The module_index can read from the workspace index for local modules instead of re-parsing them. For external `@INC` modules not in the workspace, the module_index continues to resolve them (but now in-process — see Part D).

### Files to modify

| File | Change |
|------|--------|
| `src/backend.rs` | Add `workspace_index: Arc<DashMap<PathBuf, FileAnalysis>>`. Spawn indexing on init. Add `did_change_watched_files`. |
| `src/module_resolver.rs` | New `index_workspace()` function using Rayon. |
| `Cargo.toml` | Add `rayon = "1"`, `ignore = "0.4"` dependencies. |

---

## Part B: `workspace/symbol`

### What it does

User types a query in "Go to Symbol in Workspace" picker. Returns matching symbols across all indexed files.

### Implementation

```rust
async fn symbol(
    &self,
    params: WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    let query = params.query.to_lowercase();
    let mut results = Vec::new();

    // Search open documents first (freshest analysis)
    let mut seen_paths: HashSet<PathBuf> = HashSet::new();
    for entry in self.documents.iter() {
        let uri = entry.key().clone();
        if let Some(path) = uri_to_path(&uri) {
            seen_paths.insert(path);
        }
        let doc = entry.value();
        for sym in &doc.analysis.symbols {
            if sym.name.to_lowercase().contains(&query) {
                results.push(symbol_to_info(sym, uri.clone()));
            }
        }
    }

    // Then workspace index (skip files already covered by open docs)
    for entry in self.workspace_index.iter() {
        if seen_paths.contains(entry.key()) { continue; }
        let uri = path_to_uri(entry.key());
        for sym in &entry.value().symbols {
            if sym.name.to_lowercase().contains(&query) {
                results.push(symbol_to_info(sym, uri.clone()));
            }
        }
    }

    Ok(Some(results))
}
```

**Capability registration:**

```rust
workspace_symbol_provider: Some(OneOf::Left(true)),
```

---

## Part C: Workspace Rename (cross-file)

### Current state

`rename_at()` calls `find_references()` which is single-file. `WorkspaceEdit` already supports multi-file edits, but we only ever insert one URI.

### Design

**Variables:** Stay single-file. Lexical scope doesn't cross files.

**Functions/subs:** Search all documents + workspace_index for `FunctionCall` refs matching the name.

**Packages:** Search all documents + workspace_index for `PackageRef` refs + `use` statement module names.

**Methods:** Search by method name. Since we can't guarantee the `$obj` in another file is the same class, this is best-effort — rename all method calls with the matching name. This matches what most LSPs do (TypeScript, Go) for method rename.

### Implementation

**New enum in `file_analysis.rs`:**

```rust
pub enum RenameKind {
    Variable,
    Function(String),
    Package(String),
    Method(String),
    HashKey(String),
}

pub fn rename_kind_at(&self, point: Point) -> Option<RenameKind>
```

**New query methods on `FileAnalysis`:**

```rust
/// Find all occurrences of a function name (def + refs) for rename.
pub fn rename_function(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)>

/// Find all occurrences of a package name (def + refs + use statements) for rename.
pub fn rename_package(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)>

/// Find all occurrences of a method name (def + call refs) for rename.
pub fn rename_method(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)>
```

**In `backend.rs`:**

```rust
async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;
    let new_name = &params.new_name;
    let doc = self.documents.get(uri)?;
    let point = position_to_point(params.text_document_position.position);
    let rename_kind = doc.analysis.rename_kind_at(point);

    match rename_kind {
        Some(RenameKind::Variable) => {
            // Single-file only
            Ok(symbols::rename(&doc.analysis, pos, uri, new_name))
        }
        Some(RenameKind::Function(ref name)) |
        Some(RenameKind::Package(ref name)) |
        Some(RenameKind::Method(ref name)) => {
            // Cross-file: search all stores
            let rename_fn = match &rename_kind {
                Some(RenameKind::Function(_)) => FileAnalysis::rename_function,
                Some(RenameKind::Package(_)) => FileAnalysis::rename_package,
                Some(RenameKind::Method(_)) => FileAnalysis::rename_method,
                _ => unreachable!(),
            };

            let mut all_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

            // Helper: collect edits from a FileAnalysis
            let mut collect = |uri: Url, analysis: &FileAnalysis| {
                let edits = rename_fn(analysis, name, new_name);
                if !edits.is_empty() {
                    all_changes.entry(uri).or_default().extend(
                        edits.into_iter().map(|(span, text)| TextEdit {
                            range: span_to_range(span),
                            new_text: text,
                        })
                    );
                }
            };

            // Search open documents
            let mut seen_paths = HashSet::new();
            for entry in self.documents.iter() {
                if let Some(path) = uri_to_path(entry.key()) {
                    seen_paths.insert(path);
                }
                collect(entry.key().clone(), &entry.value().analysis);
            }

            // Search workspace index
            for entry in self.workspace_index.iter() {
                if seen_paths.contains(entry.key()) { continue; }
                collect(path_to_uri(entry.key()), entry.value());
            }

            Ok(Some(WorkspaceEdit {
                changes: Some(all_changes),
                ..Default::default()
            }))
        }
        _ => Ok(None),
    }
}
```

### `prepareRename` support

```rust
rename_provider: Some(OneOf::Right(RenameOptions {
    prepare_provider: Some(true),
    work_done_progress_options: Default::default(),
})),
```

```rust
async fn prepare_rename(
    &self,
    params: TextDocumentPositionParams,
) -> Result<Option<PrepareRenameResponse>> {
    let doc = self.documents.get(&params.text_document.uri)?;
    let point = position_to_point(params.position);

    if let Some(sym) = doc.analysis.symbol_at(point) {
        return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
            range: span_to_range(sym.selection_span),
            placeholder: sym.name.clone(),
        }));
    }
    if let Some(r) = doc.analysis.ref_at(point) {
        return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
            range: span_to_range(r.span),
            placeholder: r.target_name.clone(),
        }));
    }
    Ok(None)
}
```

---

## Part D: Remove subprocess isolation

### Why

The tree-sitter-perl grammar is stable. Subprocess isolation was a development-time safety net. The costs:
- Process spawn per module (~5ms overhead each)
- Full JSON serialization boundary for `ExportedSub` (params, overloads, return types, hash keys)
- `--parse-exports` CLI mode and associated argument parsing
- Two code paths: subprocess JSON vs direct-parse (even after the dedup refactor, there's still the serialization layer)

### What changes

**Delete:**
- `parse_in_subprocess()` in `module_resolver.rs`
- `subprocess_main()` in `module_resolver.rs`
- `--parse-exports` CLI handling in `main.rs`
- JSON serialization/deserialization in `parse_in_subprocess` (the `serde_json::Map` building loop)
- `#[cfg(not(test))]` / `#[cfg(test)]` split on `parse_module()` — there's now one path

**Replace with:**

```rust
fn parse_module(inc_paths: &[PathBuf], module_name: &str) -> Option<ModuleExports> {
    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    if metadata.len() > 1_000_000 {
        log::warn!("Skipping '{}' — file too large ({} bytes)", module_name, metadata.len());
        return None;
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let mut parser = create_parser();
    let (export, export_ok, tree) = extract_exports(&mut parser, &source)?;
    let analysis = build(&tree, source.as_bytes());
    let (subs, parents) = collect_export_metadata(&analysis, &export, &export_ok, Some(module_name));
    Some(ModuleExports { path, export, export_ok, subs, parents })
}
```

One function. No subprocess, no JSON, no cfg splits. `catch_unwind` optional for defense-in-depth (same as workspace indexing).

**After the constant-folding work** (which moves export extraction into the builder), this simplifies further — `extract_exports` goes away and we read `analysis.export`/`analysis.export_ok` directly.

### Safety

- `catch_unwind` around `parse + build` for each module (catches Rust panics)
- File size cap at 1MB (prevents pathological inputs)
- True C-level segfaults in tree-sitter: not caught, but the grammar is stable. If one surfaces, it's a grammar bug to fix, not something to silently swallow.

### Testing

The `#[cfg(test)]` path already uses direct parsing (no subprocess). Removing the subprocess path means production matches test behavior exactly — no more behavioral divergence.

---

## Part E: Range Formatting

### What it does

Format a selected range instead of the whole document. We already shell out to perltidy for full-document formatting.

### Implementation

Extract lines from `range.start.line` to `range.end.line`, run perltidy, replace those lines. Perltidy handles incomplete Perl reasonably well for statement-level selections.

```rust
document_range_formatting_provider: Some(OneOf::Left(true)),
```

---

## Part F: Linked Editing Range

Low priority. When you select a variable name, all references in scope highlight for simultaneous editing. Reuse `find_references`, return as editable ranges.

```rust
linked_editing_range_provider: Some(LinkedEditingRangeServerCapabilities::Simple(true)),
```

---

## Implementation ordering

| Phase | Work | Depends on |
|-------|------|-----------|
| **1** | Workspace indexing (Rayon + ignore) | Nothing |
| **2** | Remove subprocess isolation | Nothing (independent) |
| **3** | `workspace/symbol` | Phase 1 |
| **4** | `prepareRename` | Nothing |
| **5** | Workspace rename (cross-file) | Phase 1 |
| **6** | Range formatting | Nothing |
| **7** | Linked editing range | Nothing (low priority) |

Phases 1 and 2 can be done in parallel. Phases 3-7 are all independent of each other (3 and 5 just benefit from Phase 1 being done).

## Files to modify

| File | Change |
|------|--------|
| `Cargo.toml` | Add `rayon`, `ignore` dependencies |
| `src/backend.rs` | Add `workspace_index`. Spawn indexing. Add `symbol`, `prepare_rename`, `range_formatting`, `did_change_watched_files` handlers. Modify `rename` for cross-file. Register capabilities + file watchers. |
| `src/symbols.rs` | Expose `fa_symbol_kind`, `span_to_range` as pub. Add `symbol_to_info` helper. |
| `src/file_analysis.rs` | Add `RenameKind`, `rename_kind_at`, `rename_function`, `rename_package`, `rename_method`. |
| `src/module_resolver.rs` | Add `index_workspace()`. Delete `parse_in_subprocess`, `subprocess_main`. Unify `parse_module` to single in-process path. |
| `src/main.rs` | Delete `--parse-exports` CLI handling. |

## Tests

### Workspace indexing
```rust
#[test]
fn test_index_workspace_finds_pm_files() {
    // Create temp dir with .pm files, index, verify DashMap populated
}

#[test]
fn test_index_workspace_skips_large_files() {
    // File > 1MB should not appear in index
}

#[test]
fn test_index_workspace_respects_gitignore() {
    // File in .gitignore'd dir should not appear
}
```

### Workspace symbol
```rust
#[test]
fn test_workspace_symbol_searches_all_indexed() {
    // Index two files, query matches symbols from both
}

#[test]
fn test_workspace_symbol_open_file_takes_precedence() {
    // Same file in documents and workspace_index — documents version wins
}
```

### Cross-file rename
```rust
#[test]
fn test_rename_function_cross_file() {
    // File A: defines `sub process_data { ... }`
    // File B: calls `process_data()`
    // Rename at A → edits in both files
}

#[test]
fn test_rename_variable_stays_single_file() {
    // Lexical variable rename doesn't touch other files
}

#[test]
fn test_rename_package_cross_file() {
    // File A: `package Foo::Bar;`
    // File B: `use Foo::Bar;` and `Foo::Bar->new()`
    // Rename → updates all locations
}
```

### Subprocess removal
```rust
#[test]
fn test_parse_module_direct() {
    // List::Util resolves and parses without subprocess
    // (existing test, but now runs the same path as production)
}
```
