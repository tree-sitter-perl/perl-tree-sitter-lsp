# Refactor: Unified String List Extraction

## Problem

Six independent implementations of "extract strings from qw / paren list / bare string" in builder.rs. Three of them copy-paste the per-word qw span-tracking math. Two of them (`emit_parent_class_refs` / `emit_import_symbol_refs`) are near-identical 150-line functions differing only in `RefKind`.

### Current functions

| Function | Returns | qw | parens | bare string | per-word spans |
|---|---|---|---|---|---|
| `extract_use_import_list` (1194) | `Vec<String>` + qw close Point | yes | yes (via `collect_string_values`) | yes | no |
| `collect_string_values` (1245) | `Vec<String>` | **no** | yes | yes | no |
| `@ISA` inline (1284) | `Vec<String>` | yes (inline copy) | yes (via `collect_string_values`) | no | no |
| `emit_parent_class_refs` (1020) | side effect (refs) | yes + spans | yes (via helper) | yes | yes |
| `emit_import_symbol_refs` (1108) | side effect (refs) | yes + spans | yes (via helper) | yes | yes |
| `extract_array_attr_names` (1928) | `Vec<(String, Span)>` | yes + spans | yes (recursive) | yes | yes |

## Fix

### Step 1: One extractor to rule them all

```rust
/// Extract strings from a node's children: qw(), paren lists, bare strings.
/// Returns (text, span) pairs where span covers each individual word.
/// Walks direct children of `node` looking for string-bearing child types.
fn extract_string_list(&self, node: Node<'a>) -> Vec<(String, Span)>
```

Handles:
- `quoted_word_list` → walk `string_content` children, `split_whitespace`, compute per-word `Span` from `sc.start_position()` + offset tracking
- `string_literal` / `interpolated_string_literal` → extract `string_content`, span from content node
- `parenthesized_expression` / `list_expression` → recurse
- `anonymous_array_expression` → recurse (for `has [qw(foo bar)]`)

This subsumes the qw span math that's currently copy-pasted in 3 places.

### Step 2: Convenience wrapper for names-only

```rust
/// Extract strings without spans. Convenience for callers that don't need positions.
fn extract_string_names(&self, node: Node<'a>) -> Vec<String> {
    self.extract_string_list(node).into_iter().map(|(s, _)| s).collect()
}
```

### Step 3: Unified ref emitter

```rust
/// Emit refs for string names found in a node's children.
/// Only emits refs for names in `filter` set.
fn emit_refs_for_strings(
    &mut self,
    node: Node<'a>,
    filter: &HashSet<&str>,
    ref_kind: RefKind,
) {
    for (text, span) in self.extract_string_list(node) {
        if filter.contains(text.as_str()) {
            self.add_ref(ref_kind.clone(), span, text, AccessKind::Read);
        }
    }
}
```

This replaces both `emit_parent_class_refs` + `emit_parent_refs_in_list` AND `emit_import_symbol_refs` + `emit_import_refs_in_list` — **4 functions → 1**.

### Step 4: Migrate call sites

**`extract_use_import_list`** → calls `extract_string_list`, plucks qw close position separately:
```rust
fn extract_use_import_list(&self, node: Node<'a>) -> (Vec<String>, Option<Point>) {
    // Get the qw close position (special case — only this caller needs it)
    let qw_close = self.find_qw_close_position(node);
    let names = self.extract_string_names(node);
    (names, qw_close)
}

fn find_qw_close_position(&self, node: Node<'a>) -> Option<Point> {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if child.kind() == "quoted_word_list" {
                let end = child.end_position();
                return Some(Point::new(end.row, end.column.saturating_sub(1)));
            }
        }
    }
    None
}
```

**`visit_use_statement` parent refs** (line 1000):
```rust
// Before: self.emit_parent_class_refs(node, &parents);
// After:
let parent_set: HashSet<&str> = parents.iter().map(|s| s.as_str()).collect();
self.emit_refs_for_strings(node, &parent_set, RefKind::PackageRef);
```

**`visit_use_statement` import refs** (line 1009):
```rust
// Before: self.emit_import_symbol_refs(node, &imported_symbols);
// After:
let sym_set: HashSet<&str> = imported_symbols.iter().map(|s| s.as_str()).collect();
self.emit_refs_for_strings(node, &sym_set, RefKind::FunctionCall);
```

**`@ISA` inline** (line 1277-1298):
```rust
// Before: 20 lines of inline qw + collect_string_values
// After:
let mut parents = Vec::new();
for i in 0..node.named_child_count() {
    if let Some(child) = node.named_child(i) {
        parents.extend(self.extract_string_names(child));
    }
}
```

Wait — `@ISA` iterates `named_child_count()` on the assignment node, not a use-statement. The extractor needs to handle being called on various parent node types. Since `extract_string_list` walks the direct children of its argument looking for string-bearing types, this works: pass each named child of the assignment node.

Actually, simpler: just call `extract_string_names` on each relevant child node. The `list_expression` and `quoted_word_list` children are what we want.

**`extract_array_attr_names`** (line 1928):
```rust
// Before: 40-line function
// After:
fn extract_array_attr_names(&self, node: Node<'a>, names: &mut Vec<(String, Span)>) {
    names.extend(self.extract_string_list(node));
}
```

Or just replace call sites with `self.extract_string_list(node)` directly and delete the function.

### Step 5: Delete dead code

Remove:
- `collect_string_values` (1245) — replaced by `extract_string_names`
- `emit_parent_class_refs` (1020) — replaced by `emit_refs_for_strings`
- `emit_parent_refs_in_list` (1083) — helper for above
- `emit_import_symbol_refs` (1108) — replaced by `emit_refs_for_strings`
- `emit_import_refs_in_list` (1168) — helper for above
- `extract_array_attr_names` (1928) — replaced by `extract_string_list`

**6 functions deleted, 3 added** (`extract_string_list`, `extract_string_names`, `emit_refs_for_strings`). Net: -3 functions, ~-150 lines.

## Edge cases

### `RefKind::Clone`

`RefKind` needs to implement `Clone` for `emit_refs_for_strings` to work (it emits multiple refs with the same kind). Check if it already does — if not, the `ref_kind` param can be a closure or the function can take the kind by value and clone internally. Actually, `RefKind::PackageRef` and `RefKind::FunctionCall` are simple enum variants with no data — they're `Copy`. The variants with data (`MethodCall`, `HashKeyAccess`) won't be used here. So passing by value works.

### Multi-line qw

```perl
use parent qw(
    Foo
    Bar
);
```

The `string_content` node spans multiple lines. The current `split_whitespace` + column offset math assumes single-line content. For multi-line qw, `sc_start.column + col_offset` would be wrong after a newline.

**Check:** Does tree-sitter-perl emit one `string_content` node for the entire qw content, or one per line? If one node spans lines, the span math needs to track row changes. This is a pre-existing bug in all 3 copy-pasted implementations — the refactor should fix it once.

Fix in `extract_string_list`: instead of `split_whitespace` with manual column tracking, iterate bytes tracking row/col:

```rust
// For qw content, compute per-word spans handling newlines
let bytes = text.as_bytes();
let mut row = sc_start.row;
let mut col = sc_start.column;
let mut i = 0;
while i < bytes.len() {
    // Skip whitespace, tracking newlines
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        if bytes[i] == b'\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
        i += 1;
    }
    // Collect word
    let word_start = (row, col);
    let word_begin = i;
    while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
        col += 1;
        i += 1;
    }
    if word_begin < i {
        let word = &text[word_begin..i];
        let span = Span {
            start: Point::new(word_start.0, word_start.1),
            end: Point::new(row, col),
        };
        results.push((word.to_string(), span));
    }
}
```

This handles single-line and multi-line qw correctly.

## Files to modify

| File | Change |
|------|--------|
| `src/builder.rs` | Add `extract_string_list`, `extract_string_names`, `emit_refs_for_strings`, `find_qw_close_position`, `infer_expression_type`. Delete 8 functions. Migrate call sites. |
| `src/module_resolver.rs` | Add `collect_export_metadata`. Rewrite `subprocess_main` and `resolve_and_parse` as thin callers. |

## Tests

All existing tests should pass unchanged — behavior is identical, just consolidated.

Add one new test for multi-line qw spans:

```rust
#[test]
fn test_extract_string_list_multiline_qw() {
    let fa = build_fa("
        package Multi;
        use parent qw(
            Foo
            Bar
        );
    ");
    let refs: Vec<_> = fa.refs.iter()
        .filter(|r| matches!(r.kind, RefKind::PackageRef))
        .collect();
    assert_eq!(refs.len(), 2);
    // Foo should be on its own line, not at a bogus column
    let foo_ref = refs.iter().find(|r| r.target_name == "Foo").unwrap();
    assert!(foo_ref.span.start.row > 2, "Foo should be on a line after 'use parent qw('");
}
```

## Step 6: Unify expression-type-from-node inference (`builder.rs`)

### Problem

Three overlapping functions infer an `InferredType` from a CST expression node:

| Function | String | Numeric | HashRef | ArrayRef | CodeRef | Regexp | Constructor | Sub-body unwrap | Variable lookup |
|---|---|---|---|---|---|---|---|---|---|
| `infer_literal_type` (1407) | | | yes | yes | yes | yes | | | |
| `infer_mojo_default_type` (1422) | yes | yes | yes | yes | | | yes | yes | |
| `infer_return_value_type` (1457) | | | yes* | yes* | yes* | yes* | yes | | yes |

`infer_return_value_type` delegates to `infer_literal_type` + constructor check + variable lookup. `infer_mojo_default_type` is a near-superset of `infer_literal_type` but was written independently. The overlap is small today but will grow as we add more expression patterns (e.g., function call return types, ternary branches).

### Fix

Replace `infer_literal_type` and `infer_mojo_default_type` with a single:

```rust
/// Infer type from an expression node: literals, constructors, sub-body last expr.
/// Does NOT look up variables (callers that need that do it separately).
fn infer_expression_type(&self, node: Node<'a>) -> Option<InferredType>
```

Handles all of:
- `string_literal` / `interpolated_string_literal` → String
- `number` → Numeric
- `anonymous_hash_expression` → HashRef
- `anonymous_array_expression` → ArrayRef
- `anonymous_subroutine_expression` → CodeRef (for literal context) OR recurse into body (for default-value context — see below)
- `quoted_regexp` → Regexp
- `method_call_expression` → constructor check via `extract_constructor_class`

**Sub-body handling:** The tricky case. For `infer_literal_type`, `sub { }` = CodeRef. For Mojo defaults, `sub { [] }` = ArrayRef (unwrap the body). Solution: a boolean parameter or a separate wrapper:

```rust
fn infer_expression_type(&self, node: Node<'a>, unwrap_sub_body: bool) -> Option<InferredType>
```

When `unwrap_sub_body = true` (Mojo default context): recurse into last expression of sub body.
When `unwrap_sub_body = false` (literal/return context): return CodeRef.

### Callers after refactor

- **`visit_has_call` (Mojo)**: `self.infer_expression_type(default_node, true)` — unwrap sub bodies
- **`infer_return_value_type`**: `self.infer_expression_type(child, false)` replaces `Self::infer_literal_type(child)` + constructor check. Variable lookup remains after.
- **Type constraint extraction**: anywhere the builder matches literal nodes for type constraints can use this too

### Net effect

2 functions → 1. ~25 lines removed, single point of maintenance for "what type is this expression node."

## Step 7: Deduplicate resolver export loops (`module_resolver.rs`)

### Problem

`subprocess_main` and `resolve_and_parse` each have ~100 lines of near-identical logic:
1. Iterate `@EXPORT` + `@EXPORT_OK` names → find matching symbols → build primary + overloads
2. Build `exported_names: HashSet` → iterate remaining symbols → merge duplicates as overloads

The two paths diverge only in output format: subprocess builds `serde_json::Map`, direct-parse builds `HashMap<String, ExportedSub>`. This duplication was acceptable when the loop was simple, but the overload merge logic (collect all matching syms, first → primary, rest → overloads, HashSet skip) makes the parallel maintenance burden real.

### Fix

Extract a shared function that does the symbol → export mapping once, returning the structured result:

```rust
/// Collect export metadata from a FileAnalysis for a set of export names.
/// Returns (subs, parents) ready for serialization or direct use.
fn collect_export_metadata(
    analysis: &FileAnalysis,
    export: &[String],
    export_ok: &[String],
    module_name: Option<&str>,
) -> (HashMap<String, ExportedSub>, Vec<String>)
```

This function contains ALL the logic currently duplicated:
- Parent extraction (prefer exact module_name match, fall back to single-entry)
- First loop: collect matching symbols per export name, primary + overloads
- `exported_names` HashSet construction
- Second loop: non-exported symbols for inheritance, merge-as-overload for duplicates
- Param type inference via `analysis.inferred_type()`
- Hash key collection via `analysis.hash_key_defs_for_owner()`
- Return type via `analysis.sub_return_type()`

### Callers after refactor

**`subprocess_main`**: calls `collect_export_metadata`, then serializes the `HashMap<String, ExportedSub>` to JSON. The serialization is straightforward — iterate the map and build `serde_json::Map`. This is ~20 lines of mechanical key/value insertion, no branching logic.

**`resolve_and_parse`**: calls `collect_export_metadata`, uses the result directly. The function body shrinks to: parse → extract_exports → `collect_export_metadata` → wrap in `ModuleExports`.

### Why scope it here

The string-list refactor already touches `builder.rs` extraction. The resolver loops consume builder output. Doing both in the same pass means:
- One review cycle, one test run, one set of before/after line counts
- The `ExportedParam` construction (which uses `inferred_type_to_tag`) is part of the serialization boundary — cleaning up string handling and export handling together keeps the boundary consistent
- Avoids a second "cleanup" PR that re-reads the same code

### Net effect

~200 lines deleted from `module_resolver.rs` (two ~100-line loops → one ~80-line shared function + two ~15-line callers). Combined with Step 5, total refactor removes ~350 lines across 2 files.

## Verification

1. `cargo test` — all 273 existing tests pass
2. Confirm net line count reduction (~375 lines less across builder.rs + module_resolver.rs)
3. Manual: goto-def on parent class names and import symbols still works
4. Manual: Moo `has [qw(foo bar)]` still synthesizes both accessors
5. Manual: cross-file overloads survive subprocess round-trip (Mojo accessor from another module resolves getter vs setter correctly)
