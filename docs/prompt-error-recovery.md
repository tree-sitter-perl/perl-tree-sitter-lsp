# Error-Resilient Builder: Recover Structural Elements from ERROR Nodes

## Problem

During typing, tree-sitter wraps broken regions in ERROR nodes. These ERROR nodes can swallow adjacent valid declarations — `package`, `use`, `sub`, `class` statements get absorbed as children of the ERROR instead of appearing as top-level nodes.

The builder doesn't recurse into ERROR nodes for structural elements. So when the user types `reduce` on a blank line, the ERROR node swallows `package MojoApp;` on the next line, MojoApp disappears from the symbol table, and every feature that depends on package structure breaks:

- Auto-import inserts `use` in the wrong package (the bug that found this)
- `package_at()` returns the wrong package
- Completion offers methods from the wrong class
- Diagnostics flap (functions appear unresolved, then resolved)
- Semantic tokens lose namespace coloring near the cursor

This happens on **every keystroke** that creates a momentarily invalid document — which is most keystrokes.

## Root Cause

The builder's `visit_children` dispatches on `node.kind()`. When it encounters an ERROR node, it either skips it or only looks for specific patterns (like `my ($self) = @_` for param recovery). It does NOT recover structural declarations (packages, imports, subs, classes) from ERROR nodes.

tree-sitter preserves these nodes as children of the ERROR — they're still there, typed correctly, with correct spans. We just don't look for them.

## Fix

When the builder encounters an ERROR node, recurse into its children and recover structural declarations. These are the "skeleton" of the file that should survive editing:

```rust
// In the builder's visit/dispatch method:
"ERROR" => {
    self.recover_structural_from_error(node);
}
```

```rust
fn recover_structural_from_error(&mut self, error_node: Node<'a>) {
    for i in 0..error_node.child_count() {
        if let Some(child) = error_node.child(i) {
            match child.kind() {
                // Structural declarations — the file's skeleton
                "package_statement" => self.visit_package(child),
                "use_statement" => self.visit_use(child),
                "subroutine_declaration_statement" => self.visit_sub(child),
                "method_declaration_statement" => self.visit_sub(child),
                "class_statement" => self.visit_class(child),
                // Recurse into nested ERROR nodes
                "ERROR" => self.recover_structural_from_error(child),
                // Skip everything else — expressions, variables, etc.
                // inside ERROR are unreliable
                _ => {}
            }
        }
    }
}
```

## What to recover

| Node kind | Why | What it preserves |
|-----------|-----|-------------------|
| `package_statement` | Package boundaries for insertion, scoping, completion context | Package symbols, `package_at()` |
| `use_statement` | Import detection for auto-import, diagnostics, completion | Import list, framework mode detection |
| `subroutine_declaration_statement` | Function/method symbols for completion, goto-def, outline | Sub symbols with params |
| `method_declaration_statement` | Same as above for `method` keyword | Method symbols |
| `class_statement` | Class structure for Perl 5.38 class syntax | Class symbols, `:isa`/`:does` |

## What NOT to recover

Expressions, variable declarations, assignments, method calls, and other non-structural nodes inside ERROR are unreliable — they're the REASON the ERROR exists. Trying to recover refs or type constraints from broken expressions would produce garbage. Only recover declarations that form the file's structural skeleton.

## Scope management

The recovered declarations need correct scope context. When a `package_statement` is recovered from an ERROR, it should still update `self.current_package` so subsequent recovered nodes (like `use_statement` in the same ERROR) are attributed to the right package.

The `visit_package`, `visit_use`, `visit_sub`, `visit_class` methods already handle scope management — they push/pop scopes, set `current_package`, etc. Calling them from ERROR recovery reuses this logic.

One concern: if the ERROR spans multiple packages, recovering them in order still works because the visitor methods process them sequentially, updating scope state as they go — same as if they weren't in an ERROR.

## What this fixes

**Immediately:**
- Auto-import insertion position (the original bug)
- `package_at()` during typing
- Framework mode detection surviving edits (if `use Moo;` is inside an ERROR, we still detect Moo mode)

**Broadly:**
- Completion context is stable during typing (correct package → correct methods)
- Diagnostics don't flap when typing near package/use boundaries
- Semantic tokens don't lose namespace coloring near the cursor
- Document symbols outline is stable during editing

## Tests

```rust
#[test]
fn test_error_recovery_package_survives() {
    // Simulate typing a bareword between packages
    let source = "
package Foo;
use Moo;
sub foo { }

reduce
package Bar;
use Moose;
sub bar { }
";
    let fa = build_fa(source);
    // Both packages should be detected despite the ERROR from 'reduce'
    let pkgs: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Package))
        .map(|s| s.name.as_str())
        .collect();
    assert!(pkgs.contains(&"Foo"), "Foo should survive ERROR");
    assert!(pkgs.contains(&"Bar"), "Bar should survive ERROR");
}

#[test]
fn test_error_recovery_imports_survive() {
    let source = "
package Foo;
use Moo;
reduce
use List::Util qw(max);
sub foo { }
";
    let fa = build_fa(source);
    let imports: Vec<&str> = fa.imports.iter()
        .map(|i| i.module_name.as_str())
        .collect();
    assert!(imports.contains(&"Moo"), "use Moo should survive");
    assert!(imports.contains(&"List::Util"), "use List::Util should survive");
}

#[test]
fn test_error_recovery_sub_survives() {
    let source = "
package Foo;
reduce
sub process {
    my ($self, $input) = @_;
    return $input;
}
";
    let fa = build_fa(source);
    let subs: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
        .map(|s| s.name.as_str())
        .collect();
    assert!(subs.contains(&"process"), "sub process should survive ERROR");
}

#[test]
fn test_error_recovery_framework_mode_survives() {
    let source = "
package Foo;
use Moo;
reduce
has name => (is => 'ro');
";
    let fa = build_fa(source);
    // 'has' should still be detected as a Moo accessor despite ERROR
    let methods: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Method))
        .map(|s| s.name.as_str())
        .collect();
    assert!(methods.contains(&"name"), "Moo accessor should survive ERROR recovery");
}

#[test]
fn test_error_recovery_multi_package_insertion() {
    // The original bug: auto-import position with dirty document
    let source = "
package Foo;
use Moo;
has name => (is => 'ro');
reduce
package Bar;
use Moose;
sub bar { }
";
    let fa = build_fa(source);
    // Insert position at line 4 (the 'reduce' line) should be after 'use Moo' (line 2),
    // NOT after 'use Moose' (line 6)
    let pkgs: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Package))
        .map(|s| s.name.as_str())
        .collect();
    assert!(pkgs.contains(&"Foo"));
    assert!(pkgs.contains(&"Bar"));
    // Both packages detected means find_use_insertion_position works correctly
}

#[test]
fn test_error_recovery_class_survives() {
    let source = "
use feature 'class';
reduce
class Foo {
    field $x :param;
    method bar { }
}
";
    let fa = build_fa(source);
    let classes: Vec<&str> = fa.symbols.iter()
        .filter(|s| matches!(s.kind, SymKind::Class))
        .map(|s| s.name.as_str())
        .collect();
    assert!(classes.contains(&"Foo"), "class should survive ERROR");
}
```

## Files to modify

| File | Change |
|------|--------|
| `src/builder.rs` | Add ERROR node handling in the main dispatch: call `recover_structural_from_error` which recurses into ERROR children looking for package/use/sub/class nodes |

That's it. One file, one new method, ~20 lines. The existing `visit_package`/`visit_use`/`visit_sub`/`visit_class` methods handle the actual processing — we're just making sure they get called even when their nodes are wrapped in ERROR.
