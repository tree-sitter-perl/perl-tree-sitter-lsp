# Constant Folding + Export Extraction in Builder

## Problem

Three gaps in the current string resolution pipeline:

### 1. Export extraction lives outside the builder

`extract_exports()` in `module_resolver.rs` uses tree-sitter queries to match `@EXPORT = qw(...)` directly in the CST. This is the **only** non-builder CST traversal outside `cursor_context.rs`. It can't see constants, variable references, or `push` accumulation — so modules that factor their export lists get partial or empty results.

### 2. No constant/variable resolution in string lists

```perl
use constant BASE_EXPORTS => qw(encode decode);
our @EXPORT_OK = (BASE_EXPORTS, 'validate');
# We see: @EXPORT_OK = ['validate']   ← misses encode, decode

my @COMMON = qw(connect disconnect);
our @EXPORT_OK = (@COMMON, 'status');
# We see: @EXPORT_OK = ['status']     ← misses connect, disconnect

our @EXPORT_OK = qw(foo);
push @EXPORT_OK, 'bar', 'baz';
# We see: @EXPORT_OK = ['foo']        ← misses bar, baz
```

### 3. Dynamic method calls via string interpolation

```perl
my $method = "get_${field}";
$self->$method();            # completely opaque today

for my $attr (qw(name email)) {
    my $getter = "get_$attr";
    $self->$getter();         # could resolve to get_name, get_email
}
```

This is a 1-layer interpolation fold — we're not trying to solve arbitrary string computation, just the case where a variable holds a constant string or a string with one interpolated constant.

## Design

### Step 1: Constant string accumulation in builder

Add to the builder state:

```rust
/// Known compile-time string values, accumulated during the walk.
/// Keyed by variable/constant name (e.g. "@COMMON", "BASE_CLASS", "$PREFIX").
/// Values are ordered lists of resolved strings.
constant_strings: HashMap<String, Vec<String>>,
```

Populated during the walk from these sources:

| Source | Key | Value |
|--------|-----|-------|
| `use constant NAME => 'val'` | `NAME` (bareword) | `["val"]` |
| `use constant NAME => qw(a b)` | `NAME` | `["a", "b"]` |
| `our @FOO = qw(a b)` at package scope | `@FOO` | `["a", "b"]` |
| `my @FOO = qw(a b)` at package scope | `@FOO` | `["a", "b"]` |
| `push @FOO, 'c', 'd'` | `@FOO` | append `["c", "d"]` to existing |
| `our $PREFIX = 'My::App'` | `$PREFIX` | `["My::App"]` |

**Scope rule:** Only accumulate from package-level (file scope or package block scope) assignments. Variables inside subs are too dynamic. The builder already tracks scope depth — check that the current scope is a package/file scope.

**Recursion:** When resolving a value, if it references another known constant, expand it. Cap at depth 3 to prevent pathological cases. In practice, Perl constant chains are 0-1 levels deep.

```rust
/// Resolve a name to its known constant string values.
/// Recurses through constant references up to max_depth.
fn resolve_constant_strings(&self, name: &str, depth: u8) -> Option<Vec<String>> {
    if depth > 3 { return None; }
    let values = self.constant_strings.get(name)?;
    let mut resolved = Vec::new();
    for val in values {
        // If val is itself a known constant name, recurse
        if let Some(expanded) = self.resolve_constant_strings(val, depth + 1) {
            resolved.extend(expanded);
        } else {
            resolved.push(val.clone());
        }
    }
    Some(resolved)
}
```

### Step 2: Integrate into `extract_string_list`

Currently `extract_string_list` only handles literal nodes (strings, qw). Extend it to resolve:

- **Bareword children** (constant names): if the bareword matches a key in `constant_strings`, expand to the constant's string values with synthesized spans (use the bareword's own span for all expanded values — good enough for refs).
- **Array variable children** (`@FOO`): if `@FOO` is in `constant_strings`, expand.
- **Scalar variable children** (`$FOO`): if `$FOO` is in `constant_strings`, expand (single value).

```rust
// Inside extract_string_list, after existing string/qw handling:
"bareword" | "autoquoted_bareword" => {
    if let Ok(text) = child.utf8_text(self.source) {
        if let Some(values) = self.resolve_constant_strings(text, 0) {
            let span = node_to_span(child);
            for val in values {
                results.push((val, span));
            }
        }
    }
}
"array" => {
    if let Ok(text) = child.utf8_text(self.source) {
        if let Some(values) = self.resolve_constant_strings(text, 0) {
            let span = node_to_span(child);
            for val in values {
                results.push((val, span));
            }
        }
    }
}
```

This means every caller of `extract_string_list` — exports, parents, import lists, `has` attribute names — automatically gets constant resolution for free. No per-call-site changes.

### Step 3: Move export extraction into the builder

Currently:
```
module_resolver.rs::extract_exports()     ← tree-sitter queries, separate from builder
  → returns (export, export_ok, tree)
module_resolver.rs::collect_export_metadata()  ← queries FileAnalysis
```

After:
```
builder.rs::build()                       ← sees @EXPORT assignments during the walk
  → FileAnalysis.export: Vec<String>
  → FileAnalysis.export_ok: Vec<String>
module_resolver.rs::collect_export_metadata()  ← reads from FileAnalysis directly
```

**In the builder:** When visiting an assignment to `@EXPORT` or `@EXPORT_OK`, use `extract_string_names` on the RHS (which now resolves constants). Store results on `FileAnalysis`.

```rust
// In visit_assignment, after @ISA handling:
if var_text == "@EXPORT" || var_text == "@EXPORT_OK" {
    let names = /* extract from RHS using extract_string_names */;
    if var_text == "@EXPORT" {
        self.export.extend(names);
    } else {
        self.export_ok.extend(names);
    }
}
```

**Handle `push`:** When visiting a function call to `push` where the first arg is `@EXPORT` or `@EXPORT_OK`, extract the remaining args as strings and append.

**FileAnalysis fields:**
```rust
pub export: Vec<String>,
pub export_ok: Vec<String>,
```

**module_resolver.rs changes:**
- `extract_exports()` deleted (the tree-sitter queries for exports)
- `query_cache` entries for export queries deleted
- `subprocess_main` and `resolve_and_parse`: call `build()`, read `analysis.export` and `analysis.export_ok` directly
- `collect_export_metadata` signature unchanged — it already takes slices

### Step 4: String interpolation for dynamic method dispatch

This is the fun one. When a scalar variable is assigned a string with interpolation, and that string contains exactly one interpolated variable whose value is known, we can resolve the result.

**Accumulation:** During `visit_assignment`, when the RHS is an `interpolated_string_literal`:

```perl
my $method = "get_$field";     # field is known constant → resolve
my $class = "My::${module}";   # module is known constant → resolve
my $name = "prefix_${attr}_suffix";  # attr is known → resolve
```

Parse the interpolated string's children (tree-sitter gives us `string_content` + `scalar` nodes interleaved). If every interpolated variable is in `constant_strings`, concatenate and store the result.

```rust
// In visit_assignment, for interpolated string RHS:
"interpolated_string_literal" => {
    if let Some(resolved) = self.try_fold_interpolated_string(right) {
        self.constant_strings
            .entry(var_text.to_string())
            .or_default()
            .push(resolved);
    }
}
```

```rust
/// Try to resolve an interpolated string to a concrete value.
/// Returns None if any interpolated variable is unknown.
fn try_fold_interpolated_string(&self, node: Node<'a>) -> Option<String> {
    let mut result = String::new();
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            match child.kind() {
                "string_content" | "escape_sequence" => {
                    if let Ok(text) = child.utf8_text(self.source) {
                        result.push_str(text);
                    }
                }
                "scalar" => {
                    let var = child.utf8_text(self.source).ok()?;
                    let values = self.resolve_constant_strings(var, 0)?;
                    // Only fold if it resolves to exactly one value
                    if values.len() != 1 { return None; }
                    result.push_str(&values[0]);
                }
                _ => return None, // complex interpolation, bail
            }
        }
    }
    Some(result)
}
```

**Where this pays off for method dispatch:**

```perl
# In builder, method_call_expression with scalar invocant:
my $method = "get_name";
$self->$method();
# builder sees $method in constant_strings → resolves to "get_name"
# → emits MethodCall ref to "get_name" → goto-def works, completion works
```

For the loop case:
```perl
for my $attr (qw(name email)) {
    my $getter = "get_$attr";
    ...
}
```

This is trickier — `$attr` takes multiple values across loop iterations. The `constant_strings` map would store `$attr => ["name", "email"]`, and the interpolation fold would produce MULTIPLE results for `$getter`: `["get_name", "get_email"]`. Each becomes a potential method call target.

**In `visit_method_call`:** When the method name node is a `scalar` (dynamic dispatch), look it up in `constant_strings`. If it resolves, emit refs/bindings for each resolved name:

```rust
// In visit_method_call, when method is a scalar variable:
if method_node.kind() == "scalar" {
    if let Ok(var_text) = method_node.utf8_text(self.source) {
        if let Some(method_names) = self.resolve_constant_strings(var_text, 0) {
            for name in method_names {
                // Emit method call ref for each resolved name
                self.add_ref(RefKind::MethodCall, span, name, AccessKind::Read);
            }
        }
    }
}
```

This means `$self->$method()` where `$method = "get_name"` lights up with goto-def, hover, completion — all the method intelligence we already have.

### Step 5: Loop variable constant accumulation

For the loop pattern to work, the builder needs to accumulate loop variable values:

```perl
for my $x (qw(a b c)) { ... }
# Store: $x => ["a", "b", "c"]

for my $x (@KNOWN_LIST) { ... }
# Resolve @KNOWN_LIST, store result as $x's values
```

In `visit_for_loop` (or wherever `for`/`foreach` is handled): if the iteration list resolves via `extract_string_names`, store the results under the loop variable name in `constant_strings`.

This is bounded — loop variables are scoped to the loop body, and the builder already tracks scope. When exiting the loop scope, the constant could be removed (though leaving it is harmless since the variable name won't collide — it's lexically scoped).

## Files to modify

| File | Change |
|------|--------|
| `src/builder.rs` | Add `constant_strings` map, accumulation in `visit_assignment`/`visit_use_statement`/`visit_for`, `resolve_constant_strings`, `try_fold_interpolated_string`. Extend `extract_string_list` with bareword/variable resolution. Add export extraction during walk. Handle `push` calls. Handle dynamic method dispatch in `visit_method_call`. |
| `src/file_analysis.rs` | Add `export: Vec<String>`, `export_ok: Vec<String>` fields. |
| `src/module_resolver.rs` | Delete `extract_exports()`. Update `subprocess_main`, `resolve_and_parse`, `collect_export_metadata` to read exports from `FileAnalysis`. |
| `src/query_cache.rs` | Delete export query cache entries (if extraction queries are fully removed). |

## Ordering

1. **Step 1 + 2 first** (constant accumulation + string list integration) — this is the foundation. All downstream features depend on it.
2. **Step 3 next** (move exports into builder) — depends on Step 2 for constant-aware export extraction. Deletes the query-based extraction.
3. **Steps 4 + 5 together** (interpolation folding + loop variables + dynamic dispatch) — the payoff. Depends on Step 1 for the constant map.

Steps 1-3 are the safe, high-value cleanup. Steps 4-5 are the "so cool it's beyond description" part. Each step is independently shippable and testable.

## Tests

### Constant accumulation
```rust
#[test]
fn test_use_constant_string() {
    let fa = build_fa("
        package Foo;
        use constant NAME => 'hello';
        use parent NAME;
    ");
    assert_eq!(fa.package_parents.get("Foo").unwrap(), &vec!["hello".to_string()]);
}

#[test]
fn test_constant_array_in_exports() {
    let fa = build_fa("
        package Foo;
        use Exporter 'import';
        my @COMMON = qw(alpha beta);
        our @EXPORT_OK = (@COMMON, 'gamma');
    ");
    assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn test_push_exports() {
    let fa = build_fa("
        package Foo;
        use Exporter 'import';
        our @EXPORT_OK = qw(foo);
        push @EXPORT_OK, 'bar', 'baz';
    ");
    assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
}

#[test]
fn test_recursive_constant_resolution() {
    let fa = build_fa("
        package Foo;
        use Exporter 'import';
        use constant BASE => qw(a b);
        use constant ALL => (BASE, 'c');
        our @EXPORT_OK = (ALL);
    ");
    assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
}
```

### Export extraction in builder
```rust
#[test]
fn test_builder_extracts_exports_qw() {
    let fa = build_fa("
        package Foo;
        use Exporter 'import';
        our @EXPORT = qw(delta);
        our @EXPORT_OK = qw(alpha beta gamma);
    ");
    assert_eq!(fa.export, vec!["delta"]);
    assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn test_builder_extracts_exports_paren() {
    let fa = build_fa("
        package Bar;
        our @EXPORT_OK = ('foo', 'bar', 'baz');
    ");
    assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
}
```

### String interpolation folding
```rust
#[test]
fn test_interpolated_string_constant_fold() {
    let fa = build_fa("
        package Foo;
        use Moo;
        has name => (is => 'rw');
        my $field = 'name';
        my $method = \"get_$field\";
        sub test { my $self = shift; $self->$method() }
    ");
    // Should have a MethodCall ref targeting "get_name"
    let refs: Vec<_> = fa.refs.iter()
        .filter(|r| matches!(r.kind, RefKind::MethodCall) && r.target_name == "get_name")
        .collect();
    assert!(!refs.is_empty(), "dynamic method call should resolve to get_name");
}

#[test]
fn test_loop_variable_multi_resolve() {
    let fa = build_fa("
        package Foo;
        sub test {
            my $self = shift;
            for my $attr (qw(name email)) {
                my $getter = \"get_$attr\";
                $self->$getter();
            }
        }
    ");
    let method_refs: Vec<_> = fa.refs.iter()
        .filter(|r| matches!(r.kind, RefKind::MethodCall))
        .map(|r| r.target_name.as_str())
        .collect();
    assert!(method_refs.contains(&"get_name"), "should resolve get_name");
    assert!(method_refs.contains(&"get_email"), "should resolve get_email");
}
```

## Verification

1. `cargo test` — all existing tests pass (constant folding is additive, doesn't change behavior for literal-only cases)
2. New tests for each step above
3. E2E: module with factored exports (`my @COMMON = qw(...); our @EXPORT_OK = (@COMMON, ...)`) resolves cross-file
4. E2E: dynamic method call with constant variable gets goto-def
5. Verify `extract_exports` query code is fully deleted after Step 3
6. Manual: check that the `query_cache` entries for exports are removed if no longer needed
