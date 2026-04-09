# Error-Resilient Builder: Stable Outline + ERROR Node Recovery

## Problem

During typing, the document is almost always in an invalid state. Two failure modes destroy the structural skeleton (packages, imports, subs, classes):

### Failure mode 1: ERROR nodes swallow adjacent declarations

Typing `reduce` on a blank line creates an ERROR node that absorbs the next `package MojoApp;` statement. The builder doesn't recurse into ERROR nodes for structural elements, so MojoApp disappears from the symbol table.

### Failure mode 2: Misparsing without ERROR nodes

```perl
$self->

sub hiThisWasDefinedBefore () {
}
```

tree-sitter parses `sub` as a method name (invocant `$self`, method `sub`). The entire sub declaration becomes part of a method call expression. No ERROR node — the tree is "valid" but wrong. The sub vanishes from the symbol table.

This happens whenever top-level code mixes with declarations, which is semi-common in scripts, test files, and during editing.

### Impact

Every feature that depends on the structural skeleton breaks:
- Auto-import inserts `use` in the wrong package
- Completion offers methods from the wrong class context
- Document outline loses subs during editing
- Diagnostics flap (functions appear unresolved, then resolved)
- Semantic tokens lose namespace/function coloring near the cursor
- `package_at()` returns the wrong package

## Fix: Two layers

### Layer 1: Stable structural outline on Document

Keep a lightweight structural skeleton that survives parse degradation:

```rust
struct StableOutline {
    packages: Vec<(String, usize)>,   // (name, line)
    imports: Vec<(String, usize)>,    // (module_name, line)
    subs: Vec<(String, usize)>,       // (name, line)
    classes: Vec<(String, usize)>,    // (name, line)
}
```

**On Document:**

```rust
pub struct Document {
    pub text: String,
    pub tree: Tree,
    pub analysis: FileAnalysis,
    pub stable_outline: StableOutline,
}
```

**Update rule:** After every parse, compare the new analysis's structural counts against the stable outline. For each category (packages, imports, subs, classes):
- If the current parse has >= as many entries as the stable outline: **update** the stable outline from the current parse (the parse is good)
- If the current parse has fewer: **keep** the stable outline (the parse lost something)

```rust
fn update_stable_outline(outline: &mut StableOutline, analysis: &FileAnalysis, source: &str) {
    let current_packages = extract_packages(analysis);
    let current_imports = extract_imports(analysis);
    let current_subs = extract_subs(analysis);
    let current_classes = extract_classes(analysis);

    if current_packages.len() >= outline.packages.len() {
        outline.packages = current_packages;
    } else {
        // Validate stale entries against source text
        outline.packages.retain(|(name, line)| {
            source.lines().nth(*line)
                .map(|l| l.trim_start().starts_with("package"))
                .unwrap_or(false)
        });
    }
    // Same for imports, subs, classes...
}
```

**Source-text validation:** When we keep stale entries, verify each one still exists in the actual source text. This handles legitimate deletions:
- User deletes `package MojoApp;` → line no longer starts with `package` → entry dropped
- User is typing nearby and tree-sitter misparses → line still says `package MojoApp;` → entry preserved

This is O(n) where n = number of structural elements (usually < 50). Runs only when degradation is detected.

**Line adjustment:** When the user inserts or deletes lines, the stable outline's line numbers shift. Since we use `TextDocumentSyncKind::FULL`, we can adjust line numbers during the diff computation in `Document::update`:
- If the edit inserts N lines before a stable entry, shift its line by +N
- If the edit deletes N lines before a stable entry, shift by -N

Or simpler: just validate against source text (which is always current). The line number in the stable outline is an approximation used for range queries — being off by 1-2 lines doesn't break `find_use_insertion_position` because it only cares about which package range contains the cursor.

**Who uses it:** `find_use_insertion_position` and any future function that needs package/import/sub line ranges. When the current analysis has fewer packages than the stable outline, use the outline's package list for range calculation.

### Layer 2: Builder ERROR node recovery

When the builder encounters an ERROR node, recurse into its children and recover structural declarations:

```rust
fn recover_structural_from_error(&mut self, error_node: Node<'a>) {
    for i in 0..error_node.child_count() {
        if let Some(child) = error_node.child(i) {
            match child.kind() {
                "package_statement" => self.visit_package(child),
                "use_statement" => self.visit_use(child),
                "subroutine_declaration_statement" => self.visit_sub(child),
                "method_declaration_statement" => self.visit_sub(child),
                "class_statement" => self.visit_class(child),
                "ERROR" => self.recover_structural_from_error(child),
                _ => {}
            }
        }
    }
}
```

This handles failure mode 1 (ERROR swallowing declarations) directly. The structural elements are still in the tree as children of the ERROR — we just need to look for them.

This does NOT handle failure mode 2 (misparsing without ERROR) — that's what the stable outline is for.

### How they work together

| Scenario | Layer 1 (stable outline) | Layer 2 (ERROR recovery) | Result |
|----------|------------------------|-------------------------|--------|
| Normal typing, no structural loss | Updated from current parse | Not needed | Correct |
| ERROR swallows `package` | Keeps old package entry | Recovers package from ERROR | Both work, ERROR recovery is primary |
| `$self->` eats `sub` declaration | Keeps old sub entry | Can't help (no ERROR) | Stable outline saves it |
| User legitimately deletes a package | Source-text validation drops it | Not applicable | Correct |
| User is retyping `package Fo` (mid-edit) | Source-text check: line doesn't start with `package` → dropped | Not applicable | Correct (drops stale entry) |

Layer 2 (ERROR recovery) is the primary defense — it works at the tree level, produces real symbols, and benefits all features automatically.

Layer 1 (stable outline) is insurance for the cases ERROR recovery can't handle — misparsing without ERROR nodes.

## Implementation

### Phase 1: Builder ERROR recovery (~20 lines in builder.rs)

In the builder's main dispatch, add ERROR handling:

```rust
"ERROR" => {
    self.recover_structural_from_error(node);
}
```

Plus the `recover_structural_from_error` method. The existing `visit_package`, `visit_use`, `visit_sub`, `visit_class` methods handle all the symbol creation and scope management.

### Phase 2: Stable outline on Document (~50 lines in document.rs)

Add `StableOutline` struct and update logic to `Document::update`. Add a method on `FileAnalysis` or `Document` that returns the merged package/import/sub list (current analysis + stable fallback for degraded categories).

`find_use_insertion_position` in `symbols.rs` uses the merged list instead of only `analysis.symbols`.

### Phase 3: Propagate to other structural queries

Any function that currently scans `analysis.symbols` for packages/subs/imports should use the merged list when available. Key callsites:
- `find_use_insertion_position` (the original bug)
- `package_at` (completion context)
- Document outline / `documentSymbol`
- Workspace symbol (if a sub disappears mid-edit, it shouldn't vanish from Ctrl+T)

## Files to modify

| File | Change |
|------|--------|
| `src/builder.rs` | Add `recover_structural_from_error` method + ERROR dispatch |
| `src/document.rs` | Add `StableOutline` struct, update logic in `Document::update` |
| `src/symbols.rs` | `find_use_insertion_position` uses stable outline as fallback |
| `src/file_analysis.rs` | Optional: method to merge current symbols with stable fallback |

## Tests

### ERROR recovery tests

```rust
#[test]
fn test_error_recovery_package_survives() {
    let source = "package Foo;\nuse Moo;\n\nreduce\npackage Bar;\nuse Moose;\n";
    let fa = build_fa(source);
    let pkgs: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Package))
        .map(|s| s.name.as_str()).collect();
    assert!(pkgs.contains(&"Bar"), "Bar should survive ERROR from 'reduce'");
}

#[test]
fn test_error_recovery_sub_survives() {
    let source = "package Foo;\nreduce\nsub process { }\n";
    let fa = build_fa(source);
    let subs: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Sub))
        .map(|s| s.name.as_str()).collect();
    assert!(subs.contains(&"process"), "sub should survive ERROR");
}

#[test]
fn test_error_recovery_import_survives() {
    let source = "package Foo;\nreduce\nuse List::Util qw(max);\n";
    let fa = build_fa(source);
    let imports: Vec<&str> = fa.imports.iter()
        .map(|i| i.module_name.as_str()).collect();
    assert!(imports.contains(&"List::Util"), "import should survive ERROR");
}
```

### Stable outline tests

```rust
#[test]
fn test_stable_outline_survives_misparse() {
    let original = "package Foo;\nsub bar { }\n\npackage main;\n";
    let mut doc = Document::new(original.to_string()).unwrap();
    assert_eq!(doc.stable_outline.subs.len(), 1);

    // Simulate $self-> eating the sub
    let broken = "package Foo;\n$self->\nsub bar { }\n\npackage main;\n";
    doc.update(broken.to_string());

    // If sub disappeared from current parse, stable outline should still have it
    let has_bar = doc.stable_outline.subs.iter().any(|(name, _)| name == "bar");
    assert!(has_bar, "stable outline should preserve 'bar' through misparse");
}

#[test]
fn test_stable_outline_drops_deleted() {
    let original = "package Foo;\nsub bar { }\npackage Baz;\n";
    let mut doc = Document::new(original.to_string()).unwrap();

    // User legitimately deletes sub bar
    let without_bar = "package Foo;\npackage Baz;\n";
    doc.update(without_bar.to_string());

    // Stable outline should NOT keep bar (source text validation)
    let has_bar = doc.stable_outline.subs.iter().any(|(name, _)| name == "bar");
    assert!(!has_bar, "stable outline should drop legitimately deleted sub");
}

#[test]
fn test_stable_outline_updates_on_good_parse() {
    let original = "package Foo;\nsub bar { }\n";
    let mut doc = Document::new(original.to_string()).unwrap();

    // Add a new sub
    let added = "package Foo;\nsub bar { }\nsub baz { }\n";
    doc.update(added.to_string());

    // Stable outline should now have both
    assert_eq!(doc.stable_outline.subs.len(), 2);
}
```

### Auto-import regression test

```rust
#[test]
fn test_auto_import_position_during_typing() {
    let original = std::fs::read_to_string("test_files/frameworks.pl").unwrap();
    let mut doc = Document::new(original).unwrap();

    // Simulate typing 'reduce' on line 19
    let mut lines: Vec<String> = doc.text.lines().map(String::from).collect();
    lines.insert(19, "reduce".to_string());
    doc.update(lines.join("\n"));

    // Auto-import for cursor at line 19 should insert in MooApp's range (around line 9),
    // NOT in MojoApp's range (around line 30)
    let pos = find_use_insertion_position_with_fallback(&doc, Point::new(19, 0));
    assert!(pos.line < 15, "should insert near MooApp's imports, got L{}", pos.line + 1);
}
```
