# Task: Inline Expression Propagation for Method Chaining

**Branch:** `type-constraints-spec` (continue from HEAD)
**Baseline:** 163 tests passing, 0 warnings

## Goal

Make completion, goto-def, and hover work through **call-expression chains** like:

```perl
get_config()->{host}          # hash key completion from return type
$cfg->get_db_config()->{host} # method return type → hash key completion
build_user()->name()          # method completion from function return type
$app->get_db()->prepare()     # chained method calls
```

Currently these don't work because `cursor_context.rs` extracts invocants as plain strings (`$var` or `ClassName`), so a call expression like `get_config()` or `$cfg->get_db_config()` is never captured.

## Architecture

**No new storage needed.** The atoms already exist:
- Variable types: `inferred_type(var_name, point)` → `InferredType`
- Sub return types: `sub_return_type(name)` → `InferredType` (on `SymbolDetail::Sub`)

Expression type resolution is a **query-time tree walk** that composes these atoms. The implementation has two parts:

## Part 1: `resolve_expression_type()` in `file_analysis.rs`

Add a new public method:

```rust
pub fn resolve_expression_type(&self, node: Node, source: &[u8], point: Point) -> Option<InferredType>
```

Recursive tree walk on the CST node:

| Node kind | Resolution |
|---|---|
| `scalar` | `self.inferred_type(node_text, point)` |
| bareword (no sigil, no `->`) | `Some(InferredType::Object(node_text))` — it's a class name |
| `method_call_expression` | resolve invocant → get class → find method → return its `return_type` |
| `function_call_expression` / `ambiguous_function_call_expression` | extract function name → `self.sub_return_type(name)` |
| `hash_element_expression` | resolve the base expression (for chaining, not for keys) |
| anything else | `None` |

For `method_call_expression`: the invocant is `node.child_by_field_name("invocant")`. Resolve it recursively → get `InferredType` → if `Object(cn)`, find method `cn::method_name` → get its `return_type`. This naturally handles arbitrary chaining depth.

**Important:** This method needs `Node` + `source` (for `node.utf8_text(source)`). It also needs access to `FileAnalysis` (`&self`). The `source` parameter is the file source bytes.

## Part 2: Tree-based cursor context detection

Currently `detect_cursor_context()` is text-only (line 50 of `cursor_context.rs`). Add a **new function** that uses the tree:

```rust
pub fn detect_cursor_context_tree(tree: &Tree, source: &[u8], point: Point, analysis: &FileAnalysis) -> CursorContext
```

This should handle the `Method` and `HashKey` cases using the tree instead of text:

1. Find the node at/before the cursor point
2. Walk up to find the relevant expression:
   - If we're after `->` inside a `method_call_expression`: the invocant is the `invocant` field → return `CursorContext::Method` but now the invocant field needs to carry enough info for resolution
   - If we're after `->{` inside a `hash_element_expression`: similar

**The key change to `CursorContext`:**

The `Method { invocant: String }` and `HashKey { var_text: String }` variants currently carry strings. They need to carry enough information for expression type resolution. Add new variants:

```rust
CursorContext::MethodOnExpression { invocant_type: InferredType }
CursorContext::HashKeyOnExpression { owner_type: InferredType }
```

Where the type is resolved during context detection by calling `resolve_expression_type()` on the invocant node. The `analysis` param on `detect_cursor_context_tree` enables this — it calls `analysis.resolve_expression_type(invocant_node, source, point)` during detection.

Keep the existing string-based variants for the simple `$var->` and `$var->{` cases — the new variants are for when the invocant is a complex expression (call, chain, etc.).

## Changes to `symbols.rs`

In `collect_completions()` (around line 206), call the new tree-based detector instead of (or as fallback after) the text-based one. Handle the new variants:

```rust
CursorContext::MethodOnExpression { ref invocant_type } => {
    if let Some(cn) = invocant_type.class_name() {
        analysis.complete_methods_for_class(cn, point)
    } else {
        Vec::new()
    }
}
CursorContext::HashKeyOnExpression { ref owner_type } => {
    // If it's an Object, offer hash keys for that class
    // If it's a HashRef, we'd need structure map (future)
    // For now: Object → class hash keys
    if let Some(cn) = owner_type.class_name() {
        analysis.complete_hash_keys_for_class(cn, point)
    } else {
        Vec::new()
    }
}
```

You may need to extract `complete_methods_for_class(class_name, point)` from the existing `complete_methods()` which currently takes a string invocant and resolves it internally. Factor out the class-specific completion logic so both paths can use it.

## Changes to goto-def and hover

The `RefKind::MethodCall { invocant: String }` stored in refs during the build phase is also string-based. For **chained method calls** in goto-def/hover, the invocant stored is the text of the invocant expression (e.g., `$cfg->get_db_config()`). Currently `resolve_invocant_class()` only handles `$var` and barewords.

Add `invocant_span` to `MethodCall` refs:

```rust
RefKind::MethodCall { invocant: String, invocant_span: Option<Span> }
```

During the build phase in `builder.rs`, when recording a `MethodCall` ref where the invocant is a complex expression (not a simple `$var` or bareword), store the invocant node's span.

At resolution time in goto-def/hover: when `invocant_span` is `Some`, use the tree to get the node at that span and call `resolve_expression_type()`. When `None`, fall back to current string-based resolution via `resolve_invocant_class()`.

This means goto-def/hover methods that currently take `(source: &str, point)` will also need the `tree: &Tree` parameter when resolving chained invocants. Thread it through from `backend.rs` where both `tree` and `source` are available.

## Test plan

Add tests to the existing test suite:

### 1. `resolve_expression_type` unit tests in `file_analysis.rs`

These need a tree (since the method takes a `Node`), so use `build_fa()`:

```perl
sub get_config { return { host => "localhost" } }
my $x = get_config();
# verify: resolve_expression_type on the `get_config()` call node → HashRef
```

```perl
package Foo;
sub new { bless {}, shift }
sub get_bar { return Bar->new() }
package Bar;
sub new { bless {}, shift }
sub do_thing { }
package main;
my $f = Foo->new();
# verify: resolve_expression_type on `$f->get_bar()` node → Object(Bar)
```

### 2. Integration tests via `build_fa()`

```perl
sub get_config { return { host => "localhost" } }
my $db = get_config()->{host};
```
Verify `get_config` has return type HashRef.

```perl
package Foo;
sub new { bless {}, shift }
sub get_bar { return Bar->new() }
package Bar;
sub new { bless {}, shift }
sub do_thing { }
package main;
my $f = Foo->new();
$f->get_bar()->do_thing();
```
Verify the chained resolution works — `get_bar` returns `Object(Bar)`, `do_thing` resolves in `Bar`.

### 3. Completion tests

Verify `get_config()->{` offers hash keys and `build_user()->` offers methods. These may require more test infrastructure since completion currently goes through `symbols.rs` → `cursor_context` → `file_analysis`.

## Files to modify

| File | What |
|---|---|
| `src/file_analysis.rs` | Add `resolve_expression_type()`, extract `complete_methods_for_class()` |
| `src/cursor_context.rs` | Add `detect_cursor_context_tree()`, new `CursorContext` variants |
| `src/symbols.rs` | Wire new context variants into completion, thread tree to goto-def/hover |
| `src/builder.rs` | Store `invocant_span` on `MethodCall` refs for complex invocants |

## What NOT to do

- Don't change the text-based `detect_cursor_context()` — keep it as fallback
- Don't add cross-file resolution — this is same-file only for now
- Don't add new `InferredType` variants — the existing ones suffice
- Don't change `TypeConstraint` storage — expression resolution is query-time, not build-time
- Don't handle `Foo::bar()` qualified calls — that requires a different dispatch layer

## Reference

- Spec: `docs/type-constraint-discovery.md` — "Propagation — two paths" section
- `sub_return_type()` at `file_analysis.rs:514`
- `resolve_invocant_class()` at `file_analysis.rs:953`
- `detect_cursor_context()` at `cursor_context.rs:50`
- `collect_completions()` at `symbols.rs:~206`
- tree-sitter nodes: `method_call_expression` has `invocant` + `method` fields, `function_call_expression` has `function` + `arguments` fields
