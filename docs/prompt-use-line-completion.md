# `use` Line Completion: Module Names + Import Lists

## Problem

No completion intelligence on `use` lines today. When a user types `use Mo|`, nothing happens. When they type `use List::Util qw(ma|)`, nothing happens. This is one of the most common editing actions in Perl and we offer zero assistance.

## Two features

### Feature 1: Module name completion

```perl
use Mo|
    → Moo
    → Moose
    → Mojo::Base
    → Module::Build
    → ...

use Mojo::|
    → Mojo::Base
    → Mojo::UserAgent
    → Mojo::DOM
    → ...
```

### Feature 2: Import list completion

```perl
use List::Util qw(ma|)
    → max
    → maxstr
    → max_by

use DBI qw(|)
    → looks_like_number
    → neat
    → ...

use Foo 'ba|'
    → bar
    → baz
```

---

## Part 1: Module Name Discovery

### Data sources (tiered)

| Priority | Source | Data available | When populated |
|----------|--------|---------------|----------------|
| 0 (best) | Workspace packages | Full FileAnalysis | Workspace indexing (startup) |
| 1 | cpanfile / already-resolved modules | Full ModuleExports | Module resolution (startup + on-demand) |
| 2 | `@INC` filesystem scan | Name only (no exports) | Background scan (startup) |

### `@INC` module name scan

New background scan at startup alongside cpanfile resolution. Walks `@INC` directories, converts paths to module names. No parsing — just directory traversal.

```rust
/// Known module names from @INC scan. Name only — no exports until resolved.
available_modules: Arc<DashMap<String, PathBuf>>,
```

```rust
fn scan_inc_module_names(inc_paths: &[PathBuf]) -> HashMap<String, PathBuf> {
    let mut modules = HashMap::new();
    for inc in inc_paths {
        for entry in WalkDir::new(inc).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.extension().map(|e| e == "pm").unwrap_or(false) {
                if let Ok(rel) = path.strip_prefix(inc) {
                    let module_name = rel.to_string_lossy()
                        .trim_end_matches(".pm")
                        .replace('/', "::");
                    modules.insert(module_name, path.to_path_buf());
                }
            }
        }
    }
    modules
}
```

This runs once at startup on the resolver thread (non-blocking). Even with 10K+ modules across `@INC`, it's sub-second — no file reads, just `readdir` + path manipulation.

**Deduplication:** If a module appears in both `module_index` (resolved) and `available_modules` (scanned), the resolved version wins — it has export data.

### Trigger resolution on selection

When the user selects a Tier 2 module (name-only, no exports), trigger background resolution:

```rust
// In completion item for unresolved modules:
CompletionItem {
    label: "Mojo::UserAgent".into(),
    kind: Some(CompletionItemKind::MODULE),
    detail: Some("(not yet indexed)".into()),
    // On commit, the use statement is inserted and did_change fires,
    // which triggers request_resolve() for the module — exports arrive shortly
    ..Default::default()
}
```

No special wiring needed — `did_change` already calls `request_resolve()` for all `use` statement modules. So completing `use Mojo::UserAgent` → saving/typing → module resolves → import list completion is available.

---

## Part 2: Cursor Context Detection

### New `CursorContext` variant

```rust
pub enum CursorContext {
    Variable { sigil: char },
    Method { invocant_type: Option<InferredType>, invocant_text: String },
    HashKey { owner_type: Option<InferredType>, var_text: String, source_sub: Option<String> },
    /// On a `use` line — completing module name or import list.
    UseStatement {
        /// Module name typed so far (e.g. "Mojo::Ba" or "" if just "use ")
        module_prefix: String,
        /// If true, cursor is inside the import list (qw, parens, or bare string after module)
        in_import_list: bool,
        /// The fully typed module name (only set when in_import_list is true)
        module_name: Option<String>,
    },
    General,
}
```

### Detection logic

In `detect_cursor_context`, before the existing trigger-char checks:

```rust
// Check if we're on a use/require line
let trimmed = before_cursor.trim_start();
if trimmed.starts_with("use ") || trimmed.starts_with("require ") {
    return detect_use_context(trimmed, point, analysis);
}
```

```rust
fn detect_use_context(line: &str, point: Point, analysis: Option<&FileAnalysis>) -> CursorContext {
    // "use Foo::Bar qw(baz|)"
    // "use Foo::Bar 'ba|'"
    // "use Foo::Ba|"

    let after_use = line.strip_prefix("use ").or_else(|| line.strip_prefix("require ")).unwrap();

    // Check if cursor is inside qw(), parens, or bare string after module name
    // Heuristic: if we see module_name followed by whitespace + qw( or ( or '
    // then we're in the import list.

    // Find the module name: first non-whitespace token that looks like a module
    let module_end = after_use.find(|c: char| c.is_whitespace() || c == '(' || c == '\'')
        .unwrap_or(after_use.len());
    let module_prefix = &after_use[..module_end];

    // If there's content after the module name, we might be in the import list
    let rest = after_use[module_end..].trim_start();
    if !rest.is_empty() && !module_prefix.is_empty() {
        // We're past the module name — in the import list
        return CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some(module_prefix.to_string()),
        };
    }

    // Still typing the module name
    CursorContext::UseStatement {
        module_prefix: module_prefix.to_string(),
        in_import_list: false,
        module_name: None,
    }
}
```

This is text-based (no tree needed), same as the existing `detect_cursor_context`. The heuristics don't need to be perfect — false positives just mean we show module completions where they're not useful, which is harmless.

---

## Part 3: Module Name Completion

### When `CursorContext::UseStatement { in_import_list: false, .. }`

Collect module names from all three tiers, prefix-filter, sort by priority:

```rust
fn complete_module_names(
    prefix: &str,
    module_index: &ModuleIndex,
    workspace_index: Option<&DashMap<PathBuf, FileAnalysis>>,
    available_modules: &DashMap<String, PathBuf>,
) -> Vec<CompletionItem> {
    let prefix_lower = prefix.to_lowercase();
    let mut seen = HashSet::new();
    let mut items = Vec::new();

    // Tier 0: workspace packages
    if let Some(ws) = workspace_index {
        for entry in ws.iter() {
            for (pkg, _) in &entry.value().package_parents {
                // Also check symbols for package names
            }
            // Walk symbols for Package kind
            for sym in &entry.value().symbols {
                if sym.kind == SymKind::Package && sym.name.to_lowercase().starts_with(&prefix_lower) {
                    if seen.insert(sym.name.clone()) {
                        items.push(module_completion_item(&sym.name, PRIORITY_LOCAL, "workspace"));
                    }
                }
            }
        }
    }

    // Tier 1: resolved modules
    for entry in module_index.cache_iter() {
        let name = entry.key();
        if name.to_lowercase().starts_with(&prefix_lower) && seen.insert(name.clone()) {
            items.push(module_completion_item(name, PRIORITY_IMPORTED, "indexed"));
        }
    }

    // Tier 2: @INC scan (name only)
    for entry in available_modules.iter() {
        let name = entry.key();
        if name.to_lowercase().starts_with(&prefix_lower) && seen.insert(name.clone()) {
            items.push(module_completion_item(name, PRIORITY_DYNAMIC, "available"));
        }
    }

    items
}

fn module_completion_item(name: &str, priority: u32, source: &str) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::MODULE),
        detail: Some(source.to_string()),
        sort_text: Some(format!("{:03}{}", priority, name)),
        ..Default::default()
    }
}
```

### Hierarchical namespace completion

When prefix contains `::`, filter intelligently. `use Mojo::` should show `Mojo::Base`, `Mojo::UserAgent`, etc. — not `Mojolicious`. The `starts_with` prefix filter already handles this naturally.

For long namespaces, consider showing the "next segment" only:
```
use DBIx::Class::|
    → DBIx::Class::Core
    → DBIx::Class::ResultSet
    → DBIx::Class::Schema
```

Not `DBIx::Class::Helper::ResultSet::Shortcut` — too deep. Show immediate children only, then drill down on next `::`. This is a UX enhancement for later — flat list works fine initially.

### Trigger characters

Add `:` as additional intelligence for module completion — when the user types `::`, re-trigger completion with the updated prefix. `:` is already a trigger character (for `Foo::bar` package-qualified calls), so no registration change needed.

---

## Part 4: Import List Completion

### When `CursorContext::UseStatement { in_import_list: true, module_name: Some(name) }`

Look up the module's exports from the module index:

```rust
fn complete_import_list(
    module_name: &str,
    module_index: &ModuleIndex,
) -> Vec<CompletionItem> {
    let exports = match module_index.get_exports_cached(module_name) {
        Some(e) => e,
        None => return vec![loading_placeholder(module_name)],
    };

    let mut items = Vec::new();

    // @EXPORT items (auto-imported, but useful to see)
    for name in &exports.export {
        let sub_info = exports.sub_info(name);
        items.push(import_completion_item(name, sub_info, true));
    }

    // @EXPORT_OK items (opt-in)
    for name in &exports.export_ok {
        let sub_info = exports.sub_info(name);
        items.push(import_completion_item(name, sub_info, false));
    }

    items
}

fn import_completion_item(name: &str, sub_info: Option<&ExportedSub>, is_default: bool) -> CompletionItem {
    let detail = if let Some(info) = sub_info {
        let mut d = String::new();
        if is_default { d.push_str("@EXPORT "); }
        if let Some(ref rt) = info.return_type {
            d.push_str(&format!("→ {}", format_inferred_type(rt)));
        }
        if d.is_empty() { None } else { Some(d) }
    } else {
        if is_default { Some("@EXPORT".into()) } else { None }
    };

    CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail,
        sort_text: Some(format!("{:03}{}", if is_default { PRIORITY_IMPORTED } else { PRIORITY_UNIMPORTED }, name)),
        ..Default::default()
    }
}
```

### Loading placeholder

When the module isn't resolved yet (Tier 2 selection), show a placeholder and trigger resolution:

```rust
fn loading_placeholder(module_name: &str) -> CompletionItem {
    CompletionItem {
        label: format!("loading {}...", module_name),
        kind: Some(CompletionItemKind::TEXT),
        detail: Some("Module is being indexed — completions will appear shortly".into()),
        insert_text: Some(String::new()), // don't insert anything
        sort_text: Some("999".into()), // sort last
        ..Default::default()
    }
}
```

To make re-query work after resolution completes: return `CompletionResponse::List(CompletionList { is_incomplete: true, items })`. This tells the editor to re-request completions when the user types more. After the module resolves (typically <1s), the next keystroke will get full results.

```rust
// In backend.rs completion handler, when in UseStatement import list context:
if is_use_import_context {
    let items = complete_import_list(&module_name, &self.module_index);
    let is_loading = items.iter().any(|i| i.insert_text == Some(String::new()));
    if is_loading {
        // Trigger resolution
        self.module_index.request_resolve(&module_name);
    }
    return Ok(Some(CompletionResponse::List(CompletionList {
        is_incomplete: is_loading,
        items,
    })));
}
```

**How `is_incomplete` works:** The editor caches the completion list. When `is_incomplete: true`, the editor re-requests on every keystroke (or on a timer, depending on client). So after the module resolves, the next keystroke replaces the placeholder with real exports. The user experience is: see "loading..." → type one more character → see real completions.

---

## Part 5: Wiring

### `cursor_context.rs`

- Add `UseStatement` variant to `CursorContext`
- Add `detect_use_context` function
- Detect in `detect_cursor_context` before trigger-char checks

### `symbols.rs` — completion handler

```rust
// In completion_items, match on CursorContext::UseStatement:
CursorContext::UseStatement { ref module_prefix, in_import_list, ref module_name } => {
    if in_import_list {
        if let Some(ref name) = module_name {
            complete_import_list(name, module_index)
        } else {
            vec![]
        }
    } else {
        complete_module_names(module_prefix, module_index, workspace_index, available_modules)
    }
}
```

### `backend.rs`

- Change `CompletionResponse::Array(items)` to `CompletionResponse::List(CompletionList { is_incomplete, items })` when in use-statement context with loading state
- Pass `available_modules` to `completion_items`

### `module_index.rs`

- Add `available_modules: Arc<DashMap<String, PathBuf>>` field
- Add `scan_available_modules()` called from resolver thread at startup
- Add `cache_iter()` method to expose the cache DashMap for iteration (or use existing `get_exports_cached` pattern)

### `module_resolver.rs`

- Add `scan_inc_module_names` function
- Call it at startup after `@INC` discovery, before entering main resolve loop
- Populate `available_modules`

---

## Files to modify

| File | Change |
|------|--------|
| `src/cursor_context.rs` | Add `UseStatement` variant, `detect_use_context` |
| `src/symbols.rs` | Add `complete_module_names`, `complete_import_list`, `loading_placeholder`, `import_completion_item` |
| `src/backend.rs` | Wire UseStatement context to new completion functions. Use `CompletionList` with `is_incomplete` for loading state. Pass available_modules. |
| `src/module_index.rs` | Add `available_modules` field, `scan_available_modules`, `cache_iter` |
| `src/module_resolver.rs` | Add `scan_inc_module_names`, call at startup |

---

## Tests

### Module name completion
```rust
#[test]
fn test_use_context_module_prefix() {
    let ctx = detect_use_context("use Mojo::Ba", ...);
    assert!(matches!(ctx, CursorContext::UseStatement {
        ref module_prefix, in_import_list: false, ..
    } if module_prefix == "Mojo::Ba"));
}

#[test]
fn test_use_context_import_list() {
    let ctx = detect_use_context("use List::Util qw(ma", ...);
    assert!(matches!(ctx, CursorContext::UseStatement {
        in_import_list: true,
        module_name: Some(ref name), ..
    } if name == "List::Util"));
}

#[test]
fn test_use_context_bare_string() {
    let ctx = detect_use_context("use Foo 'ba", ...);
    assert!(matches!(ctx, CursorContext::UseStatement {
        in_import_list: true,
        module_name: Some(ref name), ..
    } if name == "Foo"));
}
```

### Import list completion
```rust
#[test]
fn test_import_list_shows_exports() {
    // Set up module_index with List::Util having export_ok = ["max", "min", "sum"]
    // Complete inside qw() → should return all three
}

#[test]
fn test_import_list_loading_placeholder() {
    // Module not yet resolved → returns loading placeholder
}
```

### E2E
```lua
t.test("completion: use List::U offers List::Util", function()
  -- Type "use List::U" on a new line, trigger completion
  -- Verify List::Util appears
end)

t.test("completion: use List::Util qw(ma offers max", function()
  -- Type full use statement with qw(), trigger completion inside
  -- Verify max/maxstr appear
end)
```

---

## Edge cases

### `use parent` / `use base`
These take class names, not function names. The import list completion should detect these and offer module names instead of function names inside the list.

### `use constant`
Takes a constant name, not an import. Skip import list completion.

### `use Foo ()`
Empty parens = no imports. Completing inside `()` should still offer exports.

### `require Foo`
Same as `use` for module name completion. No import list (require doesn't import).

### Version numbers
`use Foo 5.014` — the number after the module is a version, not an import. Detect numeric token after module name and don't trigger import list completion.

### `-flags`
`use Mojo::Base -base` — flags starting with `-` are framework-specific. Don't filter them out of the prefix, but they won't match any exports (which is correct — they're not imports).
