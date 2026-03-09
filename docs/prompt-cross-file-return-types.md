# Task: Cross-file Return Type Intelligence

**Branch:** create `cross-file-return-types` from `main`
**Baseline:** 193 unit tests, 48 e2e tests, 0 warnings

## Goal

When a function is imported from another module and called locally, propagate its return type into the local type system. After this PR:

```perl
use Config::DB qw(get_config);
my $cfg = get_config();
$cfg->{host};               # ← hash key completion works (return type is HashRef)
```

The return type of `get_config` is analyzed from the module's Perl source by the existing builder, then stored in the module index and propagated at call sites.

**Scope:** pure-Perl modules only. XS modules, higher-order functions (where return type depends on a callback), and type stubs are out of scope — those are separate PRs.

## Architecture

The existing plumbing does 90% of the work:

1. **Subprocess** (`--parse-exports` in `main.rs`) already parses module source with the builder — just needs to also emit return types
2. **Builder** already computes `sub_return_type()` for every sub/method — we're just not extracting it in the subprocess
3. **CallBinding post-pass** (builder.rs:1754) already resolves local return types to variable type constraints — we add a fallback for imported functions
4. **`sub_return_type()`** (file_analysis.rs:554) already resolves locally — we add a fallback to check imported return types

## Data flow (current → new)

```
Subprocess:  source → builder → extract @EXPORT names → JSON → resolver
                                                    ↓ NEW
                               + extract return types per export → JSON

Resolver:    JSON → ModuleExports { export, export_ok }
                                                    ↓ NEW
                    → ModuleExports { export, export_ok, return_types }

Cache:       DashMap + SQLite → store return_types
                                                    ↓ NEW
Query time:  sub_return_type() falls back to imported_return_types map
             call binding post-pass checks imported_return_types map
```

## Step 1: InferredType serialization

Add serialization to `InferredType` for JSON IPC and SQLite TEXT storage.

In `file_analysis.rs`, add two functions:

```rust
pub fn inferred_type_to_tag(ty: &InferredType) -> String
pub fn inferred_type_from_tag(tag: &str) -> Option<InferredType>
```

Format: simple string tags. Object types use `Object:ClassName`.

| InferredType | Tag |
|---|---|
| `ClassName("Foo")` | `"Object:Foo"` |
| `FirstParam { package: "Foo" }` | `"Object:Foo"` |
| `HashRef` | `"HashRef"` |
| `ArrayRef` | `"ArrayRef"` |
| `CodeRef` | `"CodeRef"` |
| `Regexp` | `"Regexp"` |
| `Numeric` | `"Numeric"` |
| `String` | `"String"` |

Deserialization: `"Object:Foo"` → `ClassName("Foo")`. Everything else is a direct variant match.

**Why not serde?** It would pull in derive macros across the crate for one serialization point. Simple string tags are sufficient and keep the dependency surface minimal.

## Step 2: Subprocess — emit return types

In `module_resolver.rs`, function `subprocess_main()` (line 415):

Currently it calls `extract_exports()` which uses tree-sitter queries on `@EXPORT`/`@EXPORT_OK`. It does NOT run the builder.

Change it to:

1. Parse with tree-sitter (already done)
2. Run `builder::build(&tree, source.as_bytes())` to get a `FileAnalysis`
3. Extract exports from `FileAnalysis.imports` or keep the existing query-based extraction
4. For each exported function name, look up `analysis.sub_return_type(name)` and serialize
5. Output JSON with return types:

```json
{
  "export": ["foo", "bar"],
  "export_ok": ["baz", "qux"],
  "return_types": {
    "foo": "HashRef",
    "bar": "Object:MyClass",
    "baz": "ArrayRef"
  }
}
```

The `return_types` key is a flat map of `func_name → type_tag`. Only include entries where the return type is known (not every exported function will have an inferrable return type).

**Important:** The subprocess currently uses `extract_exports()` which is a lightweight tree-sitter query — it does NOT build a full `FileAnalysis`. Running the builder is heavier but still fast (the builder is single-pass). The 5-second subprocess timeout provides a safety net.

**Import note:** `subprocess_main` is in `module_resolver.rs` but the builder import needs `use crate::builder`. This already compiles since both are in the same crate.

## Step 3: Resolver — parse return types from subprocess output

In `module_resolver.rs`, function `parse_in_subprocess()` (line 332):

After parsing the JSON (line 383), also extract the `return_types` field:

```rust
let return_types: HashMap<String, String> = json
    .get("return_types")
    .and_then(|v| serde_json::from_value(v.clone()).ok())
    .unwrap_or_default();
```

Convert tags to `InferredType` and store in the `ModuleExports`:

```rust
let return_types: HashMap<String, InferredType> = raw_return_types
    .into_iter()
    .filter_map(|(k, v)| inferred_type_from_tag(&v).map(|ty| (k, ty)))
    .collect();
```

Also update `resolve_and_parse()` (test path, line 458) to extract return types from the builder's `FileAnalysis` directly (no JSON round-trip needed in tests).

## Step 4: Extend ModuleExports

In `module_index.rs`, add return types to `ModuleExports` (line 25):

```rust
pub struct ModuleExports {
    pub path: PathBuf,
    pub export: Vec<String>,
    pub export_ok: Vec<String>,
    pub return_types: HashMap<String, InferredType>,  // NEW
}
```

Add a convenience method:

```rust
impl ModuleExports {
    pub fn return_type(&self, func_name: &str) -> Option<&InferredType> {
        self.return_types.get(func_name)
    }
}
```

Add a public method on `ModuleIndex` for querying:

```rust
/// Look up the return type of an imported function. Zero I/O.
pub fn get_return_type_cached(&self, func_name: &str) -> Option<InferredType> {
    let modules = self.reverse_index.get(func_name)?;
    for module_name in modules.value() {
        if let Some(entry) = self.cache.get(module_name) {
            if let Some(ref exports) = *entry {
                if let Some(ty) = exports.return_types.get(func_name) {
                    return Some(ty.clone());
                }
            }
        }
    }
    None
}
```

Update all places that construct `ModuleExports` to include `return_types`:
- `parse_in_subprocess()` — from JSON
- `resolve_and_parse()` — from builder's FileAnalysis
- `warm_cache()` — from SQLite
- Test code: `insert_cache()`, test fixtures

The `insert_into_cache` function (line 282) and `rebuild_reverse_index` don't need changes — they work with `ModuleExports` opaquely.

## Step 5: SQLite schema migration

In `module_cache.rs`:

1. Bump `SCHEMA_VERSION` from `"2"` to `"3"` — this triggers a full cache rebuild (existing migration logic drops + recreates the table)
2. Add `return_types TEXT NOT NULL DEFAULT '{}'` column to the `modules` table
3. In `save_to_db()`: serialize `return_types` as JSON (`HashMap<String, String>` of name → type_tag)
4. In `warm_cache()`: deserialize `return_types` from the JSON column, convert tags to `InferredType`

The schema change triggers a full cache invalidation on first run — all modules get re-parsed with the new subprocess that extracts return types. This is expected and correct.

## Step 6: Propagate imported return types into FileAnalysis

This is where cross-file types enter the local type system. Two integration points:

### 6a: `sub_return_type()` fallback

In `file_analysis.rs`, add a field to `FileAnalysis`:

```rust
pub struct FileAnalysis {
    // ... existing fields ...
    /// Return types from imported functions (populated after build from module index).
    pub imported_return_types: HashMap<String, InferredType>,
}
```

Default to empty HashMap in `FileAnalysis::new()`.

Update `sub_return_type()` (line 554) to check the imported map as a fallback:

```rust
pub fn sub_return_type(&self, name: &str) -> Option<&InferredType> {
    // Check local subs first
    for sym in &self.symbols {
        if sym.name == name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
            if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
                if return_type.is_some() {
                    return return_type.as_ref();
                }
            }
        }
    }
    // Fallback: check imported return types
    self.imported_return_types.get(name)
}
```

This means `resolve_expression_type()`, `hover_info()`, completion detail, and all other code that calls `sub_return_type()` automatically gets cross-file support with zero changes.

### 6b: Enrich call bindings

Add a method on `FileAnalysis`:

```rust
/// Resolve call bindings for imported functions. Call after building,
/// when module index data is available.
pub fn enrich_imported_types(&mut self, imported_returns: HashMap<String, InferredType>) {
    // Push type constraints for call bindings that reference imported functions
    let mut new_constraints = Vec::new();
    for binding in &self.call_bindings {
        // Skip if already resolved (local sub or builtin)
        if self.sub_return_type_local(&binding.func_name).is_some()
            || builtin_return_type(&binding.func_name).is_some()
        {
            continue;
        }
        if let Some(rt) = imported_returns.get(&binding.func_name) {
            new_constraints.push(TypeConstraint {
                variable: binding.variable.clone(),
                scope: binding.scope,
                constraint_span: binding.span,
                inferred_type: rt.clone(),
            });
        }
    }
    self.type_constraints.extend(new_constraints);
    self.imported_return_types = imported_returns;
    // Rebuild the type_constraints_by_var index
    self.rebuild_type_index();
}
```

You'll need to split the current `sub_return_type` into a `sub_return_type_local` (checks only local symbols) and the public version (checks local + imported). The enrichment method needs the local-only version to avoid infinite loops.

Also add `rebuild_type_index()` — extract the `type_constraints_by_var` index-building from `build_indices()` so it can be called after enrichment.

### 6c: Call enrichment from backend

In `backend.rs`, add a helper that builds the imported return types map and enriches the analysis:

```rust
fn enrich_analysis(&self, uri: &Url) {
    if let Some(mut doc) = self.documents.get_mut(uri) {
        let imported_returns = self.build_imported_return_types(&doc.analysis);
        doc.analysis.enrich_imported_types(imported_returns);
    }
}

fn build_imported_return_types(&self, analysis: &FileAnalysis) -> HashMap<String, InferredType> {
    let mut map = HashMap::new();
    for import in &analysis.imports {
        if let Some(exports) = self.module_index.get_exports_cached(&import.module_name) {
            for (name, ty) in &exports.return_types {
                map.insert(name.clone(), ty.clone());
            }
        }
    }
    map
}
```

Call `enrich_analysis(uri)` in two places:

1. **`did_open`** — after inserting the document and requesting module resolution. Note: modules may not be resolved yet at this point. That's fine — the enrichment will run again on the diagnostic refresh.

2. **Diagnostic refresh callback** (`on_refresh` closure in `Backend::new`, line 27) — this fires after each module is resolved. Currently it publishes empty diagnostics. Change it to re-enrich all open documents and then re-publish diagnostics.

The refresh callback currently can't access `module_index` due to a circular dependency (noted in the comment at line 34). The fix: move the module index into an `Arc` so the refresh closure can capture a reference to it. Or: move the enrichment + diagnostic logic into a method on `Backend` and have the callback call it through a channel/flag.

The simplest approach: make `module_index` an `Arc<ModuleIndex>` on `Backend`. The refresh closure captures `Arc::clone(&module_index)`. Then it can call `get_exports_cached` to build the return types map and update the documents.

**This is the trickiest part of the PR.** The current architecture has a noted limitation (backend.rs line 34-45) where the refresh callback can't access the module index. Solving this is prerequisite for proper cross-file type propagation. The `Arc<ModuleIndex>` approach is clean and matches the existing `Arc<DashMap>` pattern for documents.

## Step 7: Update diagnostic refresh

With the module index accessible from the refresh callback, change it from publishing empty diagnostics to:

1. For each open document:
   a. Build the imported return types map
   b. Call `enrich_imported_types()` on the analysis
   c. Re-collect diagnostics (with full module index access)
   d. Publish diagnostics

This also fixes the existing TODO about stale diagnostics.

## Files to modify

| File | What |
|---|---|
| `src/file_analysis.rs` | `inferred_type_to_tag`/`from_tag`, `imported_return_types` field, `enrich_imported_types()`, split `sub_return_type` local/imported, `rebuild_type_index()` |
| `src/module_resolver.rs` | `subprocess_main` runs builder + emits return types, `parse_in_subprocess` parses return types, `resolve_and_parse` extracts from builder |
| `src/module_index.rs` | `ModuleExports.return_types` field, `get_return_type_cached()` method |
| `src/module_cache.rs` | Schema v3, `return_types` column, serialize/deserialize in save/warm |
| `src/backend.rs` | `Arc<ModuleIndex>`, `enrich_analysis()`, improved refresh callback |
| `src/document.rs` | No changes needed — Document stores FileAnalysis mutably via DashMap::get_mut |

## What NOT to do

- Don't add type stubs / hardcoded return types for CPAN modules — separate PR
- Don't add cross-file method completion (that needs method lists, not just export return types) — separate PR
- Don't change the builder's single-pass architecture — enrichment is a post-build step
- Don't add `serde` derive macros to InferredType — use simple string tags
- Don't try to infer return types for higher-order functions (map, grep, reduce patterns) — return Unknown
- Don't change how local return type inference works — this only adds a fallback for imported functions

## Test plan

### Unit tests

1. **InferredType serialization roundtrip**: all variants survive `to_tag` → `from_tag`
2. **`sub_return_type` fallback**: build a FileAnalysis, enrich with imported types, verify `sub_return_type("imported_func")` returns the imported type
3. **`enrich_imported_types`**: verify it pushes TypeConstraints for call bindings referencing imported functions, and that `inferred_type("$var", point)` returns the imported return type
4. **ModuleExports with return types**: construct, insert into cache, verify `get_return_type_cached` works
5. **SQLite roundtrip**: save ModuleExports with return types, warm cache, verify return types survive

### E2E tests (if feasible)

Testing cross-file in e2e requires a real module on disk that the LSP can resolve. This may be complex to set up. Consider adding a test module in `test_files/lib/` and configuring `@INC` for tests. But this is optional for the first PR — unit tests are sufficient for correctness.

## Verification

1. `cargo test` — all existing tests pass + new tests
2. `cargo build` — no warnings
3. E2E tests pass (existing ones should not regress)
4. Manual test: open a file that imports a module with typed returns, verify hover/completion works
