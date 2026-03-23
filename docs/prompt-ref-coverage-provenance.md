# Ref Coverage + Provenance: Builder Improvements

## Motivation

CLAUDE.md Rules 7 and 8 identify two architectural principles that aren't fully implemented:

**Rule 7 (Every meaningful token gets a ref):** Several token positions are meaningful but have no ref, causing rename, goto-def, and completion to fail silently. These are gaps in the builder's ref emission.

**Rule 8 (Provenance):** Some refs are derived from other values (constant folding, framework synthesis, import re-export), but the derivation chain isn't traceable. Rename can find the derived ref but can't update the source.

This spec addresses both with concrete builder changes.

---

## Part 1: Ref Coverage Gaps

### Gap A: `ref_at()` specificity — narrowest span wins

**Problem:** When cursor is on `timeout` in `connect(timeout => 30)`, `ref_at()` returns the enclosing `MethodCall` ref (spans the whole expression `$obj->connect(timeout => 30)`), not the individual key. Every LSP feature that starts with `ref_at()` gets the wrong answer.

**Fix:** `ref_at()` currently returns the first ref whose span contains the point. Change to return the **narrowest** ref:

```rust
pub fn ref_at(&self, point: Point) -> Option<&Ref> {
    self.refs.iter()
        .filter(|r| contains_point(&r.span, point))
        .min_by_key(|r| {
            let span = r.span;
            (span.end.row - span.start.row, span.end.column - span.start.column)
        })
}
```

This is a prerequisite for all other gaps — without it, adding narrower refs won't help because the broad `MethodCall` ref will still win.

**Impact:** Affects all `ref_at()` callers (rename, goto-def, hover, references, highlight). Should be a no-op for positions where only one ref exists. Needs careful testing to ensure existing behavior isn't degraded when multiple refs overlap.

### Gap B: Fat-comma keys in call arguments

**Problem:** `connect(timeout => 30)` — `timeout` is meaningful (it's a named parameter) but has no ref. The builder sees the `list_expression` inside the call arguments, sees the `autoquoted_bareword` `timeout`, but doesn't emit a ref for it.

**Fix:** During `visit_method_call` and `visit_function_call`, when processing arguments, detect fat-comma pairs and emit `HashKeyAccess` refs for the keys:

```rust
// In visit_method_call / visit_function_call, after extracting the call name:
if let Some(args) = node.child_by_field_name("arguments") {
    self.emit_call_arg_key_refs(args, &call_name);
}

fn emit_call_arg_key_refs(&mut self, args: Node<'a>, call_name: &str) {
    let owner = HashKeyOwner::Sub(call_name.to_string());
    let pairs = self.extract_fat_comma_pairs(args);
    for (key, _val) in pairs {
        // Find the key node's span (need to locate the autoquoted_bareword)
        // Emit HashKeyAccess ref
        self.add_ref(
            RefKind::HashKeyAccess { owner: owner.clone() },
            key_span,
            key.clone(),
            AccessKind::Read,
        );
    }
}
```

**Complication:** `extract_fat_comma_pairs` currently returns `(String, String)` — it strips spans. Need a variant that returns `(String, Span, String)` or emit refs inline during the pair extraction.

**Scoping:** The `HashKeyOwner::Sub(call_name)` scopes these keys to the called function. When `connect` has `my ($self, %opts) = @_`, the `timeout` key access resolves to the `HashKeyDef` for `timeout` in `connect`'s body (from `$opts{timeout}` or `return { timeout => ... }`). This is the existing `HashKeyOwner` resolution chain.

**For Moo/Moose constructor args:** `Foo->new(username => ...)` — the call name is `new`, and Bug D already synthesizes `HashKeyDef` entries owned by `Sub("new")`. So once Gap B is fixed, constructor arg keys automatically resolve to the `has` declaration via the `new` owner chain.

### Gap C: Fat-comma keys in hash literals not findable

**Problem:** `return { status => 'ok' }` — the `status` key has a `HashKeyDef` symbol, but `symbol_at()` doesn't find it at the cursor position because the selection span may not be set up correctly.

**Fix:** Verify that `HashKeyDef` symbols have `selection_span` set to the key text span (not the whole hash expression). The builder's `add_hash_key_def` (or wherever HashKeyDef symbols are created) should use the key's node span as the selection span. Then `symbol_at()` — which searches by selection_span — will find them.

Also ensure `ref_at()` can find `HashKeyDef` positions. Currently `ref_at` only searches `self.refs`, not `self.symbols`. For consistency with Rule 7 ("cursor on a token → ref explains it"), either:
- Emit a `HashKeyAccess` self-ref at the def site (pointing to itself), or
- Have `rename_kind_at` fall through to `symbol_at` for HashKeyDef symbols (it already does for Sub/Method/Package — just needs to also handle HashKeyDef)

### Gap D: `find_references` for hash keys

**Problem:** `rename_kind_at` correctly returns `HashKey("timeout")`, but `rename_at` → `find_references` returns empty for hash keys. The `find_references` path for hash keys needs to collect all `HashKeyDef` symbols + `HashKeyAccess` refs matching the name within the owner scope.

**Fix:** In `find_references`, when the target is a hash key:

```rust
// Collect all HashKeyDef symbols with matching name + owner
for sym in &self.symbols {
    if sym.name == key_name && matches!(sym.detail, SymbolDetail::HashKeyDef { ref owner, .. } if *owner == target_owner) {
        spans.push(sym.selection_span);
    }
}
// Collect all HashKeyAccess refs with matching name + owner
for r in &self.refs {
    if r.target_name == key_name && matches!(r.kind, RefKind::HashKeyAccess { ref owner } if *owner == target_owner) {
        spans.push(r.span);
    }
}
```

The `target_owner` comes from the ref/symbol at the cursor position. For `$opts{timeout}` → owner is `Sub("connect")`. For `$result->{status}` → owner is `Sub("get_result")`.

### Gap E: Hash deref keys (`$obj->{verbose}`)

**Problem:** `$obj->{verbose}` — the `verbose` key inside `{}` deref should resolve to the hash key owner based on `$obj`'s type. If `$obj` was constructed with `bless { verbose => ... }`, the key should link to that def.

**Current state:** The builder already emits `HashKeyAccess` refs for `$obj->{key}` patterns (this is how hash key completion works). The gap is specifically in how `find_references` collects them for rename. Once Gap D is fixed, this should work automatically.

---

## Part 2: Provenance

### What is provenance?

Provenance = the ability to trace a ref back to the source value that produced it. Currently refs point forward (usage → definition) but not backward through derivation chains.

### Provenance Chain 1: Constant folding

**Current state:** `my $m = 'process'; $self->$m()` — the builder folds `$m` to `"process"` and emits a `MethodCall` ref targeting `"process"`. Rename finds the ref. But the rename replaces the `$m` variable span at the call site, NOT the string literal `'process'` in the assignment.

**The right behavior:** Renaming `process` should:
1. Find the `sub process` definition → rename it
2. Find all `MethodCall`/`FunctionCall` refs targeting `"process"` → rename them
3. For refs that were DERIVED from constant folding, trace back to the source string and rename it too

**Data model addition:**

```rust
pub struct Ref {
    // ... existing fields ...
    /// If this ref was derived from constant folding, the source string literal's span.
    /// Rename should update the source span in addition to (or instead of) this ref's span.
    pub folded_from: Option<Span>,
}
```

When the builder emits a ref from a constant-folded value, it stores the span of the source string literal:

```rust
// In visit_method_call, when resolving $method via constant_strings:
if let Some(resolved) = self.resolve_constant_strings(method_var, 0) {
    for rname in resolved {
        self.add_ref_with_provenance(
            RefKind::MethodCall { ... },
            node_to_span(node),
            rname,
            AccessKind::Read,
            Some(source_string_span),  // span of 'process' in my $m = 'process'
        );
    }
}
```

The `rename_sub` function then checks `folded_from`:

```rust
for r in &self.refs {
    if r.target_name == old_name {
        match &r.kind {
            RefKind::FunctionCall => edits.push((r.span, new_name.to_string())),
            RefKind::MethodCall { method_name_span, .. } => {
                edits.push((*method_name_span, new_name.to_string()));
            }
            _ => {}
        }
        // Also rename the source string if this ref was constant-folded
        if let Some(source_span) = r.folded_from {
            edits.push((source_span, new_name.to_string()));
        }
    }
}
```

**Tracking the source span:** The builder needs to propagate source spans through the constant folding chain. When `constant_strings` is populated, store the span alongside the value:

```rust
/// Known compile-time string values with their source locations.
constant_strings: HashMap<String, Vec<(String, Span)>>,
//                                       ^^^^^^  ^^^^^
//                                       value    where the value was defined
```

When resolving, the span follows the value through the chain.

### Provenance Chain 2: Import list rename

**Current state:** `use Foo qw(bar)` — the builder emits a `FunctionCall` ref for `bar` via `emit_refs_for_strings`. When `sub bar` in Foo is renamed, `rename_sub` searches for `FunctionCall` refs targeting `"bar"` — and SHOULD find the import list ref.

**Test needed:** Verify this actually works. If `emit_refs_for_strings` correctly emits `FunctionCall` refs with the right span (the string content span inside `qw()`), then rename should update the import list automatically. This may already work — needs QA.

### Provenance Chain 3: `has` → accessor → constructor → internal hash

**Current state after Bug D fix:** `has name => (is => 'ro')` produces:
- Method symbol `name` (accessor) ✓
- `HashKeyDef` `name` owned by `Sub("new")` ✓ (Bug D fix)
- `HashKeyDef` `name` owned by the class (internal `$self->{name}`) — only if bless hash has it

**What's missing:** These three manifestations of `name` aren't linked. Renaming the method doesn't rename the hash keys. Renaming a hash key doesn't rename the accessor.

**Fix:** When `rename_kind_at` identifies a rename target that corresponds to a `has` attribute (detected by: the symbol was synthesized by framework accessor code, or the HashKeyDef is owned by `Sub("new")` in a Moo/Moose class), the rename should expand to cover ALL manifestations:

```rust
// In the rename handler (backend.rs or file_analysis.rs):
if is_framework_attribute(analysis, &name) {
    // Rename the accessor method (rename_sub handles this)
    // ALSO rename hash key defs owned by "new" with this name
    // ALSO rename hash key accesses with this name owned by the class
}
```

This is a special case of provenance: the `has` declaration is the source of truth, and all derived symbols/refs should rename together.

**Implementation approach:** Add a `rename_framework_attribute` method on `FileAnalysis` that collects:
1. All symbols named `old_name` with `SymKind::Method` in the class (accessor)
2. All `HashKeyDef` symbols named `old_name` owned by `Sub("new")` (constructor)
3. All `HashKeyDef` symbols named `old_name` owned by the class (internal hash)
4. All `MethodCall` refs targeting `old_name` (method calls)
5. All `FunctionCall` refs targeting `old_name` (function-style calls)
6. All `HashKeyAccess` refs targeting `old_name` with matching owner (hash derefs + constructor args)

Return all spans. The backend handler uses this instead of `rename_sub` when the rename target is a framework attribute.

### Provenance Chain 4: Return hash key → caller deref

**Current state:** `sub get_config { return { host => ... } }` then `$cfg->{host}` in caller. The `host` key in the return hash has owner `Sub("get_config")`. The `host` in `$cfg->{host}` also resolves to owner `Sub("get_config")` (via return type → hash key propagation in the enrichment pass).

**This should work already** once Gap D (find_references for hash keys) is fixed. The `HashKeyOwner::Sub("get_config")` links them. The rename would collect all HashKeyDef + HashKeyAccess entries with matching name + owner.

**Cross-file:** The enrichment pass (`enrich_imported_types_with_keys`) already propagates hash keys across files. Cross-file hash key rename would need to search workspace_index for matching owner + name. This is a natural extension of the cross-file rename pattern.

### Provenance Chain 5: Package name → file path

**Problem:** Renaming `MyApp::Controller::Users` should offer to rename/move `lib/MyApp/Controller/Users.pm`.

**LSP support:** `WorkspaceEdit` supports `documentChanges` with `RenameFile` operations:

```rust
WorkspaceEdit {
    document_changes: Some(vec![
        DocumentChangeOperation::Op(ResourceOp::Rename(RenameFile {
            old_uri: old_path_uri,
            new_uri: new_path_uri,
            options: None,
            annotation_id: None,
        })),
        // ... text edits ...
    ]),
}
```

**Implementation:** When `rename_kind_at` returns `Package(name)`:
1. Compute the expected file path: `name.replace("::", "/") + ".pm"` relative to workspace root
2. If the file exists, include a `RenameFile` operation in the `WorkspaceEdit`
3. Also include text edits for all `PackageRef` refs + package declarations across the workspace

**Scope:** This is a stretch goal. The text-level package rename already works. The file rename is a UX enhancement.

### Provenance Chain 6: Inherited override tracking

**Problem:** Renaming `Animal::speak` should surface `Dog::speak` for coordinated rename.

**Current state:** `rename_sub` searches ALL files for any method named `speak` — it doesn't distinguish classes. So renaming `speak` in `Animal` would also rename `speak` in `Dog`, `Cat`, and any unrelated class that happens to have a `speak` method.

**Better behavior:** When renaming a method, check the inheritance chain:
1. Find all subclasses of the defining class (via `package_parents` reverse lookup)
2. For each subclass, check if it has an override of the same method
3. Include overrides in the rename (they're intentionally the same API)
4. Optionally warn if unrelated classes also have a method with the same name

**Implementation:** This requires a reverse parent lookup (`child_classes_of(parent_name)`) across the workspace index. The data is already there — `package_parents` maps child → parents. Building the reverse is a scan of all `FileAnalysis` entries.

**Scope:** Defer. The current behavior (rename all methods with the same name) is aggressive but not wrong for most cases. Scoped rename is a refinement.

---

## Implementation ordering

| Phase | Work | Depends on |
|-------|------|-----------|
| **1a** | `ref_at()` specificity — narrowest span wins | Nothing |
| **1b** | Emit `HashKeyAccess` refs for fat-comma keys in call args (Gap B) | 1a |
| **1c** | Fix `find_references` for hash keys (Gap D) | Nothing |
| **1d** | Verify `HashKeyDef` findable by `symbol_at` (Gap C) | Nothing |
| **2a** | Constant folding provenance — `folded_from: Option<Span>` on Ref | Nothing |
| **2b** | `rename_framework_attribute` — unified has/accessor/key rename | 1b, 1c |
| **2c** | Verify import list rename works (Provenance Chain 2) | Nothing (may already work) |
| **2d** | Cross-file hash key rename via workspace_index | 1c |
| **3a** | Package rename → file rename (Provenance Chain 5) | Nothing |
| **3b** | Inherited override tracking (Provenance Chain 6) | Nothing |

Phases 1a-1d are the "every token gets a ref" foundation. Phase 2 is provenance. Phase 3 is stretch.

---

## Tests

### Ref coverage
```rust
#[test]
fn test_ref_at_narrowest_span() {
    // $obj->connect(timeout => 30)
    // Cursor on 'timeout' → should get HashKeyAccess, not MethodCall
}

#[test]
fn test_call_arg_fat_comma_has_ref() {
    // $obj->connect(timeout => 30, host => 'localhost')
    // 'timeout' and 'host' should each have HashKeyAccess refs
}

#[test]
fn test_moo_constructor_arg_has_ref() {
    // Foo->new(username => 'alice')
    // 'username' should have HashKeyAccess ref owned by Sub("new")
    // Should resolve to HashKeyDef from has declaration
}

#[test]
fn test_hash_key_rename_from_access() {
    // my $t = $opts{timeout};
    // Rename 'timeout' → should update $opts{timeout}, return { timeout => ... }, and connect(timeout => ...)
}

#[test]
fn test_hash_key_rename_from_deref() {
    // $result->{status}
    // Rename 'status' → should update return { status => 'ok' } and all $result->{status} accesses
}
```

### Provenance
```rust
#[test]
fn test_constant_fold_rename_updates_source_string() {
    // my $m = 'process'; $self->$m()
    // Rename 'process' → should update sub def, the $self->$m() call site, AND 'process' string literal
}

#[test]
fn test_import_list_renamed_with_sub() {
    // use Foo qw(bar); bar();
    // Rename sub bar in Foo → should update 'bar' in qw(), bar() call, and sub bar def
}

#[test]
fn test_framework_attribute_unified_rename() {
    // package Foo; use Moo; has name => (is => 'ro');
    // Foo->new(name => 'x'); $foo->name; $self->{name}
    // Rename from any position → updates all four
}
```

### E2E
```lua
t.test("rename: hash key from $opts{timeout} updates call site + return hash", function() end)
t.test("rename: Moo has name → updates accessor + constructor + hash deref", function() end)
t.test("rename: constant-folded method updates source string", function() end)
t.test("rename: sub rename updates import list qw()", function() end)
```

### CLI QA
```bash
# Hash key rename
perl-lsp --rename /tmp/test lib/Thorough.pm 6 19 max_wait
# Should show edits for $opts{timeout}, return { timeout => }, connect(timeout =>)

# Moo attribute rename
perl-lsp --rename /tmp/test lib/Foo.pm 3 4 display_name
# Should show edits for has username, $self->username, ->new(username =>), $self->{username}

# Constant fold rename
perl-lsp --rename /tmp/test lib/Const.pm 18 4 handle
# Should show edits for sub process, $self->$method() call, AND 'process' string literal
```
