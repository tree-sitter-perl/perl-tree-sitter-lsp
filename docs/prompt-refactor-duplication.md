# Task: Refactor Duplicated Concerns in file_analysis.rs

**Branch:** `file-analysis` (continue from HEAD)
**Baseline:** 192 tests passing, 0 warnings

## Context

After several rounds of feature work (expression propagation, sub hash intelligence, hover, builtins), several concerns are now duplicated across multiple code paths. This cleanup rounds consolidates them.

## P0a — Bug: `resolve_target_at` missing tree-based invocant resolution

`resolve_target_at()` (file_analysis.rs, ~line 1198) handles `MethodCall` refs using only the string-based `resolve_invocant_class()`. It **ignores `invocant_span`**, unlike `find_definition()` and `hover_info()` which both use `resolve_method_invocant()` with tree-based resolution.

Since `resolve_target_at` feeds `find_references`, `find_highlights`, and `rename_at`, **references/highlights/rename don't resolve through expression chains**. For `$calc->get_self()->add()`, goto-def works but find-references on `add` falls through to "any method with that name".

### Fix

1. Add `tree: Option<&Tree>` and `source_bytes: Option<&[u8]>` parameters to `resolve_target_at()` (it already receives them from callers `find_references`, `find_highlights`)
2. Change the `MethodCall` arm to use `resolve_method_invocant()` instead of `resolve_invocant_class()`:

```rust
RefKind::MethodCall { ref invocant, ref invocant_span, .. } => {
    let class_name = self.resolve_method_invocant(
        invocant, invocant_span, r.scope, point, tree, source_bytes,
    );
    // ... rest unchanged
}
```

3. Thread the tree/source params from `find_references` and `find_highlights` into `resolve_target_at`. `rename_at` currently passes `None, None` for tree/source — that's fine, it stays as-is (rename doesn't support chains yet, and that's documented as intentional).

### Test

Add a test that verifies references work through chains:

```perl
package Foo;
sub new { bless {}, shift }
sub bar { }
package main;
my $f = Foo->new();
$f->bar();        # direct call
```

Verify that `find_references` on `bar` (the method def) returns both the definition and the `$f->bar()` call. This should already work. Then add the chain case:

```perl
sub get_foo { return Foo->new() }
get_foo()->bar();   # chained call
```

Verify `find_references` on `bar` also finds this chained usage (currently broken, should pass after fix).

## P0b — Merge `Method`/`MethodOnExpression` and `HashKey`/`HashKeyOnExpression` variants

`CursorContext` has parallel variant pairs that force every consumer into duplicate branches:

- `Method { invocant: String }` + `MethodOnExpression { invocant_type: InferredType }`
- `HashKey { var_text: String }` + `HashKeyOnExpression { owner_type: InferredType, source_sub: Option<String> }`

The split exists because the tree-based detector (`detect_cursor_context_tree`) has an `is_complex_expression` guard that skips simple `$var`/bareword invocants, deferring to the text-based detector which returns the string variants. Consumers then resolve the string to a type themselves — duplicating what the tree-based detector could have done.

### Fix

1. **Remove the `is_complex_expression` guard** in `detect_cursor_context_tree`. Let it handle simple cases:
   - `scalar` node → `analysis.inferred_type(text, point)` → return type if found
   - `package`/`bareword` → `Some(InferredType::ClassName(text))`
   - Complex expressions → `analysis.resolve_expression_type()` (unchanged)

2. **Merge variants** in `CursorContext`:

```rust
/// After `->` — method completion. Type is resolved when available.
Method { invocant_type: Option<InferredType>, invocant_text: String },
/// After `->{` — hash key completion. Type is resolved when available.
HashKey { owner_type: Option<InferredType>, var_text: String, source_sub: Option<String> },
```

The `invocant_text`/`var_text` string is always populated (from text-based fallback or tree text). The `Option<InferredType>` is `Some` when resolution succeeds (the common case), `None` when the tree is in ERROR and we can't resolve.

3. **Text-based fallback** (`detect_cursor_context`) still exists but now also tries to resolve the type before returning. It receives `analysis: Option<&FileAnalysis>` (optional, for the case where it's called standalone in tests). When analysis is available:
   - Variable invocant → `analysis.inferred_type(invocant, point)`
   - Bareword → `Some(ClassName(invocant))`

4. **Update consumers** in `symbols.rs`:
   - Completion: one `Method` arm, checks `invocant_type` first, falls back to `analysis.complete_methods(&invocant_text, point)` when `None`
   - Same for `HashKey`
   - Deref snippets: one place, checks `invocant_type`

This eliminates 4 variants → 2 variants and removes duplicate branches from every consumer.

### What to preserve

- `detect_cursor_context` (text-based) stays as fallback — it's critical for ERROR recovery when tree nodes are garbage
- The `detect_from_error_node` path in tree-based detection stays — it handles incomplete expressions
- `invocant_text` is kept on the merged variant so consumers that need the raw string (e.g. `complete_methods` string-based resolution) still have it

### Test impact

- All existing `CursorContext` unit tests need updating for the new variant shape
- No behavioral change expected — same resolution, just earlier in the pipeline

## P1a — Extract shared ref collection from find_references / find_highlights

`find_references()` (~line 928-993) and `find_highlights()` (~line 996-1057) are ~90% identical. Both:

1. Call `resolve_target_at()` to get the target symbol
2. Include the declaration
3. Collect all `resolves_to` refs
4. Collect name-matched refs for subs/packages (identical block)
5. Collect hash key refs with tree-based owner resolution (identical block)
6. Sort + dedup

### Fix

Extract:

```rust
fn collect_refs_for_target(
    &self,
    target_id: SymbolId,
    include_decl: bool,
    tree: Option<&Tree>,
    source_bytes: Option<&[u8]>,
) -> Vec<(Span, AccessKind)>
```

This does steps 2-6. Then:

- `find_references` calls it, maps to `Vec<Span>` (dropping access kind)
- `find_highlights` calls it, returns `Vec<(Span, AccessKind)>` directly

## P1b — `complete_methods` should delegate to `complete_methods_for_class`

`complete_methods()` (~line 1640) resolves the invocant to a class name, then scans all symbols for methods **three separate times** in three different code paths (class with implicit new, class subs, package subs fallback). `complete_methods_for_class()` (~line 717) does this in a single clean pass and also includes return type info in the detail string via `method_detail()`.

### Fix

Rewrite `complete_methods` to:

```rust
pub fn complete_methods(&self, invocant: &str, point: Point) -> Vec<CompletionCandidate> {
    let class_name = if !invocant.starts_with('$')
        && !invocant.starts_with('@')
        && !invocant.starts_with('%')
    {
        Some(invocant.to_string())
    } else {
        self.resolve_invocant_class(
            invocant,
            self.scope_at(point).unwrap_or(ScopeId(0)),
            point,
        )
    };

    if let Some(ref cn) = class_name {
        let candidates = self.complete_methods_for_class(cn);
        if !candidates.is_empty() {
            return candidates;
        }
    }

    // Fallback: all subs/methods in file
    self.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
        .map(|s| CompletionCandidate {
            label: s.name.clone(),
            kind: s.kind,
            detail: Some(if matches!(s.kind, SymKind::Method) { "method" } else { "sub" }.to_string()),
            insert_text: None,
            sort_priority: PRIORITY_FILE_WIDE,
            additional_edits: vec![],
        })
        .collect()
}
```

This eliminates ~60 lines and gains return type info in completion detail for free.

## P2a — Consolidate three hash key completion methods

`complete_hash_keys()`, `complete_hash_keys_for_class()`, `complete_hash_keys_for_sub()` share the same inner loop: get defs from owner → dedup → build candidates.

### Fix

Extract:

```rust
fn complete_hash_keys_for_owner(&self, owner: &HashKeyOwner) -> Vec<CompletionCandidate> {
    let defs = self.hash_key_defs_for_owner(owner);
    let mut seen = HashSet::new();
    let mut candidates = Vec::new();

    for def in defs {
        if !seen.insert(def.name.clone()) {
            continue;
        }
        let is_dynamic = matches!(&def.detail, SymbolDetail::HashKeyDef { is_dynamic: true, .. });
        let detail = match owner {
            HashKeyOwner::Class(name) => format!("{}->{{{}}}", name, def.name),
            HashKeyOwner::Variable { name, .. } => format!("{}{{{}}}", name, def.name),
            HashKeyOwner::Sub(name) => format!("{}()->{{{}}}", name, def.name),
        };
        candidates.push(CompletionCandidate {
            label: def.name.clone(),
            kind: SymKind::Variable,
            detail: Some(detail),
            insert_text: None,
            sort_priority: if is_dynamic { PRIORITY_DYNAMIC } else { PRIORITY_FILE_WIDE },
            additional_edits: vec![],
        });
    }
    candidates
}
```

Then:
- `complete_hash_keys` resolves the owner, calls `complete_hash_keys_for_owner`
- `complete_hash_keys_for_class(cn)` → `complete_hash_keys_for_owner(&HashKeyOwner::Class(cn.into()))`
- `complete_hash_keys_for_sub(name)` → `complete_hash_keys_for_owner(&HashKeyOwner::Sub(name.into()))`

The `_for_class` and `_for_sub` methods become one-liners (keep them as convenience wrappers or inline at call sites).

## P2b — Deduplicate `node_to_span`

Defined identically in `builder.rs:10` and `cursor_context.rs:661`.

### Fix

Add a `pub(crate)` version to `file_analysis.rs` (both modules already import from there):

```rust
pub(crate) fn node_to_span(node: tree_sitter::Node) -> Span {
    Span { start: node.start_position(), end: node.end_position() }
}
```

Remove the local copies in builder.rs and cursor_context.rs, import from `file_analysis`.

## P3 — `extract_call_name` duplication

`cursor_context.rs:223` has `extract_call_name_from_node(node, source) -> Option<String>`.
`builder.rs:~1394` has `extract_call_name(node) -> Option<String>` (uses `self.source`).

Both extract function/method names from call expression nodes with identical match arms.

### Fix

Move to `file_analysis.rs` as:

```rust
pub(crate) fn extract_call_name(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "method_call_expression" => node.child_by_field_name("method")
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| s.to_string()),
        "function_call_expression" | "ambiguous_function_call_expression" =>
            node.child_by_field_name("function")
                .and_then(|n| n.utf8_text(source).ok())
                .map(|s| s.to_string()),
        _ => None,
    }
}
```

Builder's version is a method on `&self` that uses `self.source` — change it to call the shared function with `self.source`.

## Files to modify

| File | What |
|---|---|
| `src/file_analysis.rs` | P0a fix, P1a extract, P1b simplify, P2a extract, P2b add `node_to_span`, P3 add `extract_call_name` |
| `src/cursor_context.rs` | P0b merge variants, remove `is_complex_expression` guard, update `detect_cursor_context` signature, P2b remove local `node_to_span`, P3 use shared `extract_call_name` |
| `src/symbols.rs` | P0b update consumers for merged variants (completion, snippets) |
| `src/builder.rs` | P2b remove local `node_to_span`, P3 delegate `extract_call_name` |

## What NOT to do

- Don't merge `find_definition` / `hover_info` / `resolve_target_at` — they serve different purposes and the dispatch logic is different enough per variant
- Don't change `complete_methods_for_class` — it's the "good" version; `complete_methods` should delegate to it
- Don't touch the completion dispatch in `symbols.rs` — that's clean
- Don't add new features — this is pure refactoring

## Verification

1. `cargo test` — all 192 tests pass (no new tests except P0 chain references test)
2. E2E tests still pass: `nvim --headless -u test_nvim_init.lua`
3. No new warnings from `cargo build`
