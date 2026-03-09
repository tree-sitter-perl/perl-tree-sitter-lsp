# Task: Cross-file Function Metadata

**Branch:** create `cross-file-function-metadata` from `main`
**Baseline:** 200 unit tests, 53 e2e tests, 0 warnings

## Goal

Enrich the per-export metadata extracted during module analysis so that all LSP features work correctly for imported functions. After this PR:

```perl
use Carp qw(croak);
use Config::DB qw(get_config);

croak("oops");     # go-to-def jumps to `sub croak` in Carp.pm (not line 1)
                   # hover shows signature + "imported from Carp"
                   # signature help shows parameter info on `(`

get_config();      # completion detail shows "→ HashRef"
                   # hover shows params + return type
```

Currently the module index stores export lists, return types, and hash keys — but no definition lines or parameter signatures. Four features are broken or incomplete for imported functions:

| Feature | Current behavior | Target |
|---|---|---|
| **Go to definition** | Jumps to line 1 of .pm file | Jump to `sub foo {` line |
| **Hover** | "imported from Module" (no types) | Shows params + return type + module |
| **Completion detail** | "imported from Module" | Shows `→ ReturnType` when known |
| **Signature help** | Completely fails | Shows parameter names/types on `(` |

## Architecture

The builder already computes everything we need per-sub:
- `Symbol.span` — definition line number
- `SymbolDetail::Sub { params, is_method, return_type }` — full signature

We just need to extract this during subprocess analysis and store it in `ModuleExports`.

### New data type

Add to `module_index.rs`:

```rust
/// Metadata for an exported function, extracted during module analysis.
#[derive(Debug, Clone)]
pub struct ExportedSub {
    /// Line number of the `sub foo {` definition (0-indexed).
    pub def_line: u32,
    /// Parameter names with metadata.
    pub params: Vec<ExportedParam>,
    /// Whether this is a method (has $self/$class first param).
    pub is_method: bool,
    /// Inferred return type, if known.
    pub return_type: Option<InferredType>,
    /// Hash key names from return value, if returns HashRef.
    pub hash_keys: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ExportedParam {
    pub name: String,
    pub is_slurpy: bool,
}
```

This replaces the separate `return_types` and `hash_keys` maps with a single `subs: HashMap<String, ExportedSub>` on `ModuleExports`.

### ModuleExports change

```rust
pub struct ModuleExports {
    pub path: PathBuf,
    pub export: Vec<String>,
    pub export_ok: Vec<String>,
    /// Per-function metadata for exported subs.
    pub subs: HashMap<String, ExportedSub>,
}
```

The `return_types` and `hash_keys` fields are removed — their data lives in `ExportedSub.return_type` and `ExportedSub.hash_keys`.

## Step 1: Define ExportedSub + ExportedParam

In `module_index.rs`, add the types above. Add a convenience method:

```rust
impl ModuleExports {
    pub fn sub_info(&self, name: &str) -> Option<&ExportedSub> {
        self.subs.get(name)
    }

    /// Convenience: return type for a function (used by enrichment).
    pub fn return_type(&self, name: &str) -> Option<&InferredType> {
        self.subs.get(name).and_then(|s| s.return_type.as_ref())
    }

    /// Convenience: hash keys for a function (used by enrichment).
    pub fn hash_keys(&self, name: &str) -> Option<&[String]> {
        self.subs.get(name).map(|s| s.hash_keys.as_slice()).filter(|k| !k.is_empty())
    }
}
```

## Step 2: Extract in subprocess

In `module_resolver.rs`, `subprocess_main()` (line 435):

Currently extracts `return_types` and `hash_keys` as separate maps. Change to extract a unified `subs` map:

```rust
let mut subs_map = serde_json::Map::new();
let analysis = crate::builder::build(&tree, source.as_bytes());
for name in export.iter().chain(export_ok.iter()) {
    let mut sub_info = serde_json::Map::new();

    // Find the sub symbol for definition line + params
    for sym in &analysis.symbols {
        if sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
            sub_info.insert("def_line".into(), sym.span.start.row.into());
            if let SymbolDetail::Sub { ref params, is_method, .. } = sym.detail {
                sub_info.insert("is_method".into(), is_method.into());
                let params_json: Vec<serde_json::Value> = params.iter()
                    .map(|p| serde_json::json!({
                        "name": p.name,
                        "is_slurpy": p.is_slurpy,
                    }))
                    .collect();
                sub_info.insert("params".into(), params_json.into());
            }
            break;
        }
    }

    // Return type
    if let Some(ty) = analysis.sub_return_type(name) {
        sub_info.insert("return_type".into(),
            serde_json::Value::String(inferred_type_to_tag(ty)));
    }

    // Hash keys
    let owner = HashKeyOwner::Sub(name.clone());
    let keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
        .iter().map(|s| s.name.clone()).collect();
    if !keys.is_empty() {
        sub_info.insert("hash_keys".into(), serde_json::json!(keys));
    }

    if !sub_info.is_empty() {
        subs_map.insert(name.clone(), sub_info.into());
    }
}
```

Output JSON becomes:

```json
{
  "export": ["foo", "bar"],
  "export_ok": ["baz"],
  "subs": {
    "foo": {
      "def_line": 42,
      "params": [{"name": "$config_path", "is_slurpy": false}],
      "is_method": false,
      "return_type": "HashRef",
      "hash_keys": ["host", "port"]
    },
    "bar": {
      "def_line": 78,
      "params": [{"name": "$msg", "is_slurpy": false}],
      "is_method": false
    }
  }
}
```

**Backward compat:** The subprocess output format is internal (parent process always matches the same binary), so there's no backward compatibility concern.

Also update `resolve_and_parse()` (line 506, test path) to extract the same data from the builder directly.

## Step 3: Parse subs from JSON

In `module_resolver.rs`, `parse_in_subprocess()`:

After parsing JSON, deserialize the `subs` field:

```rust
let subs: HashMap<String, ExportedSub> = json
    .get("subs")
    .and_then(|v| v.as_object())
    .map(|obj| {
        obj.iter().filter_map(|(name, val)| {
            let def_line = val.get("def_line")?.as_u64()? as u32;
            let is_method = val.get("is_method").and_then(|v| v.as_bool()).unwrap_or(false);
            let params = val.get("params")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|p| {
                    Some(ExportedParam {
                        name: p.get("name")?.as_str()?.to_string(),
                        is_slurpy: p.get("is_slurpy").and_then(|v| v.as_bool()).unwrap_or(false),
                    })
                }).collect())
                .unwrap_or_default();
            let return_type = val.get("return_type")
                .and_then(|v| v.as_str())
                .and_then(|s| inferred_type_from_tag(s));
            let hash_keys = val.get("hash_keys")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                .unwrap_or_default();
            Some((name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys }))
        }).collect()
    })
    .unwrap_or_default();
```

## Step 4: SQLite schema migration

In `module_cache.rs`:

1. Bump `SCHEMA_VERSION` from `"4"` to `"5"` — triggers full cache rebuild
2. Replace `return_types TEXT` and `hash_keys TEXT` columns with single `subs TEXT NOT NULL DEFAULT '{}'`
3. In `save_to_db()`: serialize `subs` as JSON — each value has `def_line`, `params`, `is_method`, `return_type` (as tag), `hash_keys`
4. In `warm_cache()`: deserialize `subs` JSON back to `HashMap<String, ExportedSub>`

The schema migration drops the old table and recreates — all modules get re-parsed on first run.

## Step 5: Update enrichment

In `file_analysis.rs`, `enrich_imported_types_with_keys()`:

The method signature stays the same (takes return types + hash keys maps). The **callers** (`build_imported_return_types` in `backend.rs`) need to extract from the new `ExportedSub` structure:

```rust
fn build_imported_return_types(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
) -> (HashMap<String, InferredType>, HashMap<String, Vec<String>>) {
    let mut type_map = HashMap::new();
    let mut keys_map = HashMap::new();
    for import in &analysis.imports {
        if let Some(exports) = module_index.get_exports_cached(&import.module_name) {
            for (name, sub_info) in &exports.subs {
                if let Some(ref ty) = sub_info.return_type {
                    type_map.insert(name.clone(), ty.clone());
                }
                if !sub_info.hash_keys.is_empty() {
                    keys_map.insert(name.clone(), sub_info.hash_keys.clone());
                }
            }
        }
    }
    (type_map, keys_map)
}
```

The enrichment logic itself (`enrich_imported_types_with_keys`) is unchanged — it still receives the same `(HashMap<String, InferredType>, HashMap<String, Vec<String>>)` pair.

## Step 6: Fix go-to-definition

In `symbols.rs`, `find_definition()` (line 76):

Currently returns `Range::default()` (line 1) for the .pm file. Change to use `def_line`:

```rust
if let Some((import, module_path)) =
    resolve_imported_function(analysis, &r.target_name, module_index)
{
    if let Ok(module_uri) = Url::from_file_path(&module_path) {
        // Try to get the actual definition line from the module index
        let def_range = module_index
            .get_exports_cached(&import.module_name)
            .and_then(|e| e.sub_info(&r.target_name))
            .map(|sub_info| Range {
                start: Position { line: sub_info.def_line, character: 0 },
                end: Position { line: sub_info.def_line, character: 0 },
            })
            .unwrap_or_default();

        return Some(GotoDefinitionResponse::Array(vec![
            Location { uri: uri.clone(), range: span_to_range(import.span) },
            Location { uri: module_uri, range: def_range },
        ]));
    }
}
```

## Step 7: Enrich hover for imported functions

In `symbols.rs`, `hover_info()` (line 321):

Currently shows just "imported from Module". Enrich with return type and params:

```rust
if let Some((import, _path)) =
    resolve_imported_function(analysis, &r.target_name, module_index)
{
    let mut parts = Vec::new();

    // Show signature if available
    if let Some(exports) = module_index.get_exports_cached(&import.module_name) {
        if let Some(sub_info) = exports.sub_info(&r.target_name) {
            // Build signature line: sub foo($param1, $param2) → HashRef
            let params_str = sub_info.params.iter()
                .map(|p| p.name.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            let mut sig = format!("sub {}({})", r.target_name, params_str);
            if let Some(ref rt) = sub_info.return_type {
                sig.push_str(&format!(" → {}", format_inferred_type(rt)));
            }
            parts.push(format!("```perl\n{}\n```", sig));
        }
    }

    parts.push(format!("*imported from `{}`*", import.module_name));
    let markdown = parts.join("\n\n");
    // ... return Hover with markdown
}
```

## Step 8: Add return type to completion detail

In `symbols.rs`, `imported_function_completions()` (line 629):

When building completion candidates, include return type in the detail:

```rust
// For explicitly imported symbols (line 648-655):
let detail = if let Some(ref exports) = exports {
    if let Some(sub_info) = exports.sub_info(name) {
        if let Some(ref rt) = sub_info.return_type {
            format!("→ {} ({})", format_inferred_type(rt), import.module_name)
        } else {
            format!("imported from {}", import.module_name)
        }
    } else {
        format!("imported from {}", import.module_name)
    }
} else {
    format!("imported from {}", import.module_name)
};
```

Apply the same pattern to the auto-add-to-qw and unimported-module completion paths.

## Step 9: Cross-file signature help

In `symbols.rs`, `signature_help()` (line 414):

Currently only checks local subs. Add a fallback to the module index. The function needs `module_index` as a new parameter.

**In `backend.rs`:** Pass `&self.module_index` to `signature_help()`.

**In `symbols.rs`:**

```rust
pub fn signature_help(
    analysis: &FileAnalysis,
    tree: &Tree,
    text: &str,
    pos: Position,
    module_index: &ModuleIndex,  // NEW
) -> Option<SignatureHelp> {
    let point = position_to_point(pos);
    let call_ctx = cursor_context::find_call_context(tree, text.as_bytes(), point)?;

    // Try local first
    if let Some(sig_info) = analysis.signature_for_call(
        &call_ctx.name, call_ctx.is_method, call_ctx.invocant.as_deref(), point,
    ) {
        // ... existing local signature logic (unchanged)
        return Some(build_signature_help(&sig_info, &call_ctx, analysis));
    }

    // Fallback: imported function
    if !call_ctx.is_method {
        if let Some((import, _)) =
            resolve_imported_function(analysis, &call_ctx.name, module_index)
        {
            if let Some(exports) = module_index.get_exports_cached(&import.module_name) {
                if let Some(sub_info) = exports.sub_info(&call_ctx.name) {
                    let param_labels: Vec<String> = sub_info.params.iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let params: Vec<ParameterInformation> = param_labels.iter()
                        .map(|label| ParameterInformation {
                            label: ParameterLabel::Simple(label.clone()),
                            documentation: None,
                        })
                        .collect();
                    let label = format!("{}({})", call_ctx.name, param_labels.join(", "));
                    return Some(SignatureHelp {
                        signatures: vec![SignatureInformation {
                            label,
                            documentation: None,
                            parameters: Some(params),
                            active_parameter: Some(call_ctx.active_param as u32),
                        }],
                        active_signature: Some(0),
                        active_parameter: Some(call_ctx.active_param as u32),
                    });
                }
            }
        }
    }

    None
}
```

**Note:** We don't have inferred types for imported function parameters (that would require running the builder with context from the call site). The signature shows parameter names only. This is still much better than nothing.

## Step 10: Update all ModuleExports construction sites

Every place that builds a `ModuleExports` needs updating:

1. **`parse_in_subprocess()`** — from JSON (Step 3)
2. **`resolve_and_parse()`** — from builder's FileAnalysis (Step 2)
3. **`warm_cache()`** — from SQLite (Step 4)
4. **`save_to_db()`** — to SQLite (Step 4)
5. **Test code** — `insert_cache()` calls, test fixtures: replace `return_types` + `hash_keys` with `subs`
6. **`get_return_type_cached()`** on ModuleIndex — update to use `subs` instead of `return_types`

## Files to modify

| File | What |
|---|---|
| `src/module_index.rs` | `ExportedSub`, `ExportedParam` types, replace `return_types`/`hash_keys` with `subs` on `ModuleExports`, update `get_return_type_cached`, convenience methods |
| `src/module_resolver.rs` | `subprocess_main` extracts full sub metadata, `parse_in_subprocess` parses `subs` from JSON, `resolve_and_parse` extracts from builder |
| `src/module_cache.rs` | Schema v5, `subs TEXT` column replaces `return_types`+`hash_keys`, serialize/deserialize |
| `src/symbols.rs` | Fix `find_definition` to use `def_line`, enrich `hover_info` with params/return type, add return type to completion detail, add module_index param to `signature_help` |
| `src/backend.rs` | Pass `module_index` to `signature_help`, update `build_imported_return_types` to use `subs` |
| `src/file_analysis.rs` | No changes — enrichment interface stays the same |

## What NOT to do

- Don't extract inferred types for imported function parameters — we'd need the call site context for that. Just show parameter names.
- Don't add cross-file references — that's a project-wide index, separate PR.
- Don't change the enrichment interface (`enrich_imported_types_with_keys`) — it still receives the same `(HashMap<String, InferredType>, HashMap<String, Vec<String>>)`.
- Don't change the builder — all data is already available in `FileAnalysis`.
- Don't add parameter default values to `ExportedParam` — they're source-dependent strings that may not make sense cross-file.

## Test plan

### Unit tests

1. **ExportedSub serialization roundtrip**: construct `ExportedSub`, serialize to JSON, parse back, verify all fields survive
2. **`sub_info()` convenience method**: construct `ModuleExports` with `subs`, verify lookup works
3. **`return_type()` and `hash_keys()` convenience methods**: verify they extract from `subs` correctly
4. **SQLite roundtrip**: save `ModuleExports` with `subs`, warm cache, verify all fields survive
5. **Existing enrichment tests**: should pass unchanged (enrichment interface didn't change)

### E2E tests

Add to `test_e2e_cross_file.lua` (uses `test_files/lib/TestExporter.pm`):

1. **go-to-def on imported function**: verify it jumps to the correct line in the .pm file (not line 1)
2. **hover on imported function**: verify it shows parameter names and return type
3. **completion detail**: verify imported function completions show `→ ReturnType`
4. **signature help**: verify `imported_func(` shows parameter names

## Verification

1. `cargo test` — all 200 existing tests pass + new tests
2. `cargo build` — no warnings
3. All 53 existing e2e tests pass + new cross-file e2e tests
4. Manual test: open a file that imports `Carp qw(croak)`, go-to-def on `croak` lands on `sub croak` in Carp.pm
