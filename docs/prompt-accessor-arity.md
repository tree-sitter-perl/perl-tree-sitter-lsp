# Arity-aware accessor resolution

## Problem

Framework accessor synthesis creates two symbols for rw-style attributes (getter + setter), but method resolution always picks the first match. This causes wrong return types for fluent APIs (Mojo::Base) and is already silently wrong for Moo `rw` attributes. The cross-file path (`ExportedSub`) drops one overload entirely since it's keyed by name.

### Current state

**Moo `rw`** — already two symbols, but `find_method_return_type` returns the first one (getter, no return type). The setter's return type is also unset, so no practical impact yet — but will be wrong once types are set.

**Mojo::Base** — currently one symbol with setter signature and wrong return type (`ClassName(pkg)` always). Should be two symbols:
- Getter: 0 params, `return_type: None` (inferable from usage via normal type constraint machinery)
- Setter: 1 param `$val`, `return_type: Some(ClassName(pkg))` (fluent `$self`)

**Cross-file** — `ModuleExports.subs` is `HashMap<String, ExportedSub>`. When the subprocess/direct-parse encounters two symbols with the same name, the second is skipped (`contains_key` guard). One overload is silently lost.

## Fix

### Step 1: Mojo::Base — synthesize two symbols

In `visit_has_call`, replace the `FrameworkMode::MojoBase` branch:

```rust
FrameworkMode::MojoBase => {
    for (name, sel_span) in &attr_names {
        // Getter: no params, return type inferable from usage
        self.add_symbol(
            name.clone(),
            SymKind::Method,
            node_to_span(node),
            *sel_span,
            SymbolDetail::Sub {
                params: vec![],
                is_method: true,
                return_type: None,
                doc: None,
            },
        );
        // Setter: fluent, returns $self for chaining
        self.add_symbol(
            name.clone(),
            SymKind::Method,
            node_to_span(node),
            *sel_span,
            SymbolDetail::Sub {
                params: vec![ParamInfo {
                    name: "$val".into(),
                    default: None,
                    is_slurpy: false,
                }],
                is_method: true,
                return_type: self.current_package.as_ref()
                    .map(|pkg| InferredType::ClassName(pkg.clone())),
                doc: None,
            },
        );
    }
}
```

### Step 2: `ExportedSub` gains overloads

Add an `overloads` field to carry additional signatures for the same method name. The existing top-level fields (`params`, `return_type`) remain the **primary** overload (getter / 0-arg variant). This is backwards-compatible: all existing consumers continue reading the primary fields unchanged.

```rust
pub struct ExportedSub {
    pub def_line: u32,
    pub params: Vec<ExportedParam>,           // primary overload (getter)
    pub is_method: bool,
    pub return_type: Option<InferredType>,    // primary overload return type
    pub hash_keys: Vec<String>,
    pub doc: Option<String>,
    pub overloads: Vec<ExportedOverload>,     // NEW: additional overloads (empty for normal subs)
}

/// An additional calling convention for the same method name.
#[derive(Debug, Clone)]
pub struct ExportedOverload {
    pub params: Vec<ExportedParam>,
    pub return_type: Option<InferredType>,
}
```

For most subs, `overloads` is empty (`Vec::new()`). For Mojo::Base accessors and Moo `rw`, it has one entry (the setter). This generalizes to any future overload pattern.

### Step 3: Subprocess serialization

**Subprocess JSON output** — when iterating `analysis.symbols` to build `subs_map`, detect duplicate names and merge into overloads:

In `subprocess_main` (line 570), the loop currently skips duplicates with `if subs_map.contains_key(&sym.name) { continue; }`. Change to: if the name already exists, append an overload entry instead of skipping.

```rust
// Line 570 (subprocess_main, non-export symbols loop):
for sym in &analysis.symbols {
    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }

    let mut sub_info = /* ... build sub_info as before ... */;

    if let Some(existing) = subs_map.get_mut(&sym.name) {
        // Duplicate name — append as overload
        let existing_obj = existing.as_object_mut().unwrap();
        let overloads = existing_obj
            .entry("overloads")
            .or_insert_with(|| serde_json::json!([]));
        let overload = serde_json::json!({
            "params": sub_info.get("params").cloned().unwrap_or(serde_json::json!([])),
            "return_type": sub_info.get("return_type").cloned(),
        });
        overloads.as_array_mut().unwrap().push(overload);
    } else {
        subs_map.insert(sym.name.clone(), sub_info.into());
    }
}
```

Same merge logic applies to the first loop (export/export_ok symbols, line ~510).

**Subprocess JSON deserialization** (`parse_in_subprocess`, line ~433) — read the `overloads` array:

```rust
// After building the ExportedSub from JSON:
let overloads = sub_obj.get("overloads")
    .and_then(|v| v.as_array())
    .map(|arr| arr.iter().filter_map(|o| {
        let params = o.get("params")
            .and_then(|p| p.as_array())
            .map(|arr| /* same param deserialization as primary */)
            .unwrap_or_default();
        let return_type = o.get("return_type")
            .and_then(|v| v.as_str())
            .and_then(|s| InferredType::from_tag(s));
        Some(ExportedOverload { params, return_type })
    }).collect())
    .unwrap_or_default();
```

### Step 4: Direct-parse path (`resolve_and_parse`)

Same merge logic as subprocess. In `resolve_and_parse` (line ~707), the second loop currently skips duplicates. Change to merge:

```rust
// Line 708 (resolve_and_parse, non-export symbols loop):
for sym in &analysis.symbols {
    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }

    if let Some(existing) = subs.get_mut(&sym.name) {
        // Merge as overload
        let mut params = Vec::new();
        if let SymbolDetail::Sub { params: ref p, .. } = sym.detail {
            params = p.iter()
                .map(|pi| {
                    let param_type = analysis.inferred_type(&pi.name, sym.span.end)
                        .map(|t| inferred_type_to_tag(t));
                    ExportedParam { name: pi.name.clone(), is_slurpy: pi.is_slurpy, inferred_type: param_type }
                })
                .collect();
        }
        let return_type = analysis.sub_return_type(&sym.name).cloned();
        existing.overloads.push(ExportedOverload { params, return_type });
        continue;
    }

    // ... existing code to build and insert ExportedSub ...
    // Initialize overloads: Vec::new()
}
```

### Step 5: SQLite cache serialization

The `subs` column already stores JSON. The `overloads` array is just another field in each sub's JSON object. Old cache entries without it → empty vec on deserialization (already handled by the `unwrap_or_default()` pattern).

In `module_cache.rs`:

**`deserialize_subs_json`** — add overloads parsing:

```rust
// After building ExportedSub from JSON:
let overloads = sub_obj.get("overloads")
    .and_then(|v| v.as_array())
    .map(|arr| arr.iter().filter_map(|o| {
        let params = /* same param parsing */;
        let return_type = o.get("return_type")
            .and_then(|v| v.as_str())
            .and_then(|s| InferredType::from_tag(s));
        Some(ExportedOverload { params, return_type })
    }).collect())
    .unwrap_or_default();
```

**Serialization** (`subs` to JSON for SQLite) — the existing serialization writes `ExportedSub` fields to JSON. Add the `overloads` array. Subs with empty overloads can omit the field (saves space).

No `EXTRACT_VERSION` bump needed — old entries just have `overloads: []` implicitly. But if this ships alongside the param types change (which already bumps to 3), that's fine.

### Step 6: Arity-aware `find_method_return_type`

`find_method_return_type` is the single funnel through which method return types flow. It's called from:

1. `resolve_expression_type` (line 786) — has the tree node, can count args
2. `resolve_method_call_types` post-pass (line 707) — no tree node, assignment context
3. `method_detail` for hover/completion (line 943) — no call context, display only
4. `find_references` hover info (line 1375) — no call context

Only caller 1 needs the right overload. Callers 2-4 should get the getter (primary) type.

**Change signature:**

```rust
pub(crate) fn find_method_return_type(
    &self,
    class_name: &str,
    method_name: &str,
    module_index: Option<&ModuleIndex>,
    arg_count: Option<usize>,  // NEW: None = prefer getter (primary / 0-param)
) -> Option<InferredType>
```

**Local resolution** — when `arg_count` is provided and multiple symbols match:

```rust
Some(MethodResolution::Local { sym_id, .. }) => {
    let candidates: Vec<_> = self.symbols_named(method_name).iter()
        .filter(|&&sid| {
            let s = self.symbol(sid);
            matches!(s.kind, SymKind::Sub | SymKind::Method)
                && self.symbol_in_class(sid, class_name)
        })
        .copied()
        .collect();

    if candidates.len() <= 1 || arg_count.is_none() {
        // Single symbol or no arity info — primary (first match)
        if let SymbolDetail::Sub { ref return_type, .. } = self.symbol(sym_id).detail {
            return return_type.clone();
        }
        return None;
    }

    // Multiple overloads: pick by param count
    let target = arg_count.unwrap();
    for sid in &candidates {
        if let SymbolDetail::Sub { ref params, ref return_type, .. } = self.symbol(*sid).detail {
            if params.len() == target {
                return return_type.clone();
            }
        }
    }
    // No exact match — fall back to primary
    if let SymbolDetail::Sub { ref return_type, .. } = self.symbol(sym_id).detail {
        return_type.clone()
    } else {
        None
    }
}
```

**Cross-file resolution** — check `ExportedSub.overloads` by arity:

```rust
Some(MethodResolution::CrossFile { ref class }) => {
    module_index.and_then(|idx| {
        let exports = idx.get_exports_cached(class)?;
        let sub = exports.subs.get(method_name)?;

        // If no arity info or no overloads, return primary
        let target = match arg_count {
            Some(n) if !sub.overloads.is_empty() => n,
            _ => return sub.return_type.clone(),
        };

        // Check primary params
        if sub.params.len() == target {
            return sub.return_type.clone();
        }
        // Check overloads
        for overload in &sub.overloads {
            if overload.params.len() == target {
                return overload.return_type.clone();
            }
        }
        // Fallback to primary
        sub.return_type.clone()
    })
}
```

### Step 7: Update call sites

**`resolve_expression_type`** (line 786) — has the node, can count args:

```rust
"method_call_expression" => {
    // ... existing invocant resolution ...
    if method_text == "new" {
        return Some(InferredType::ClassName(class_name.to_string()));
    }
    let arg_count = self.count_call_args(node, source);
    self.find_method_return_type(class_name, method_text, module_index, Some(arg_count))
}
```

**New helper** on `FileAnalysis`:

```rust
/// Count arguments in a call expression (0 vs 1+ is the key distinction).
fn count_call_args(&self, node: tree_sitter::Node, _source: &[u8]) -> usize {
    let args = match node.child_by_field_name("arguments") {
        Some(a) => a,
        None => return 0,
    };
    match args.kind() {
        "parenthesized_expression" | "list_expression" => args.named_child_count(),
        _ => 1, // single argument
    }
}
```

**`resolve_method_call_types` post-pass** (line 707) — pass `None`. The assignment pattern `my $x = $obj->name` is a getter call. Passing `None` returns the primary (getter) type, which is correct.

```rust
if let Some(rt) = self.find_method_return_type(&cn, &binding.method_name, module_index, None) {
```

**Display callers** (`method_detail`, `find_references` hover) — pass `None`:

```rust
self.find_method_return_type(class_name, method_name, module_index, None)
```

### Step 8: Tests

```rust
#[test]
fn test_mojo_getter_setter_distinct() {
    let fa = build_fa("
package Foo;
use Mojo::Base -base;
has 'name';
");
    let methods: Vec<_> = fa.symbols.iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 2, "should synthesize getter + setter");

    let getter = methods.iter().find(|m| {
        if let SymbolDetail::Sub { ref params, .. } = m.detail { params.is_empty() } else { false }
    });
    let setter = methods.iter().find(|m| {
        if let SymbolDetail::Sub { ref params, .. } = m.detail { params.len() == 1 } else { false }
    });
    assert!(getter.is_some(), "should have a 0-param getter");
    assert!(setter.is_some(), "should have a 1-param setter");

    // Getter: no return type (inferable from usage)
    if let SymbolDetail::Sub { ref return_type, .. } = getter.unwrap().detail {
        assert!(return_type.is_none());
    }
    // Setter: fluent return
    if let SymbolDetail::Sub { ref return_type, .. } = setter.unwrap().detail {
        assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Foo".into())));
    }
}

#[test]
fn test_mojo_fluent_chain_resolves() {
    let src = "
package Foo;
use Mojo::Base -base;
has 'name';
has 'age';
sub greet {
    my ($self) = @_;
    my $result = $self->name('Bob')->age;
    return $result;
}
";
    let fa = build_fa(src);
    let tree = parse(src);
    // $self->name('Bob') has args → setter → returns Foo
    // ->age has no args → getter → returns None (unknown)
    // The chain should resolve: name('Bob') returns Foo, ->age is valid on Foo
    let method_refs: Vec<_> = fa.refs.iter()
        .filter(|r| r.target_name == "age" && matches!(r.kind, RefKind::MethodCall { .. }))
        .collect();
    assert!(!method_refs.is_empty(), "should have method call ref for 'age'");
}

#[test]
fn test_moo_rw_arity_resolution() {
    let fa = build_fa("
package Foo;
use Moo;
has 'name' => (is => 'rw', isa => 'Str');
");
    // Both getter and setter have Str return type (Moo rw returns value, not $self)
    let rt_getter = fa.find_method_return_type("Foo", "name", None, Some(0));
    assert_eq!(rt_getter, Some(InferredType::String));
    let rt_setter = fa.find_method_return_type("Foo", "name", None, Some(1));
    assert_eq!(rt_setter, Some(InferredType::String));
    let rt_default = fa.find_method_return_type("Foo", "name", None, None);
    assert_eq!(rt_default, Some(InferredType::String));
}

#[test]
fn test_exported_sub_overloads_roundtrip() {
    // Test that overloads survive serialization through JSON + SQLite
    let sub = ExportedSub {
        def_line: 5,
        params: vec![],  // getter: no params
        is_method: true,
        return_type: None,
        hash_keys: vec![],
        doc: None,
        overloads: vec![ExportedOverload {
            params: vec![ExportedParam { name: "$val".into(), is_slurpy: false, inferred_type: None }],
            return_type: Some(InferredType::ClassName("Foo".into())),
        }],
    };
    // Serialize to JSON, deserialize back
    let json = serialize_subs_json(&[("name".into(), sub.clone())]);
    let roundtripped = deserialize_subs_json(&json);
    let rt = roundtripped.get("name").unwrap();
    assert_eq!(rt.overloads.len(), 1);
    assert_eq!(rt.overloads[0].params.len(), 1);
    assert_eq!(rt.overloads[0].return_type, Some(InferredType::ClassName("Foo".into())));
}
```

## Files modified

| File | Change |
|------|--------|
| `src/builder.rs` | Mojo::Base branch: two symbols (getter + setter) instead of one |
| `src/file_analysis.rs` | `find_method_return_type` gains `arg_count: Option<usize>`, arity-aware overload selection for both Local and CrossFile branches. `count_call_args` helper. Update all call sites. |
| `src/module_index.rs` | `ExportedOverload` struct. `overloads: Vec<ExportedOverload>` field on `ExportedSub`. |
| `src/module_resolver.rs` | Subprocess: merge duplicate-name symbols into overloads instead of skipping. Direct-parse (`resolve_and_parse`): same merge logic. Deserialization: read `overloads` array. |
| `src/module_cache.rs` | `deserialize_subs_json`: read `overloads`. Serialization: write `overloads` (omit if empty for compact JSON). |

## Design notes

### Why primary + overloads, not a flat Vec

`ExportedSub` has many consumers. Changing `params`/`return_type` from direct fields to `overloads[0].params` would require modifying every call site for zero behavioral gain. The primary fields remain the default view — all existing code keeps working unchanged. The `overloads` vec is additive: empty for normal subs, non-empty only for accessor patterns.

### Arity matching is 0 vs non-zero

We only need to distinguish "no args" (getter) from "has args" (setter). The `count_call_args` helper doesn't need to be precise for complex expressions — `0 vs non-zero` is the reliable distinction. If an accessor is called with 0 named children in the arguments node, it's a getter.

### Getter is always the primary overload

Convention: the primary `ExportedSub` fields always hold the getter (0-param) signature. Overloads hold setter and any future variants. This means:
- Consumers without arity context (hover, completion detail, signature help) naturally show the getter — which is the "what does this return" answer users expect.
- `arg_count: None` in `find_method_return_type` returns the primary (getter) type — safe default.
- The subprocess/direct-parse merge logic: first symbol encountered becomes primary, subsequent ones become overloads. Since the builder synthesizes the getter first, this works automatically.

### Completion deduplication

`complete_methods_for_class` already deduplicates by name via `seen_names: HashSet`. Two local symbols → one completion entry (getter's detail shown). Cross-file: one `ExportedSub` per name → naturally one entry. No changes needed.

### Signature help

`signature_for_call` currently returns a single `SignatureInfo`. With overloads, it could show multiple signatures via LSP's `SignatureHelp.signatures` array + `activeSignature` selection. Not in scope for this change — showing the getter signature is correct default behavior. Can be improved later.

### `MethodCallBinding` post-pass

Passes `None` for `arg_count` → getter type. This is correct: `my $x = $obj->name` is a getter call. The fluent pattern `$obj->name($val)->chain` doesn't assign to a variable — it flows through `resolve_expression_type` which HAS the node and DOES pass arity.
