# Ref Provenance: Residual Forward Work

> CLAUDE.md rules 7 (every meaningful token gets a ref) + 8 (provenance) are
> the principles. Phase 1 of the original ref-coverage doc — narrowest-span
> `ref_at`, fat-comma key emission for call args, `RenameKind` dispatch — is
> in. This doc is the residual: derivation chains where rename can find the
> derived ref but can't update the source.

## What's still missing

### Constant-fold provenance — `Ref.folded_from`

`my $m = 'process'; $self->$m()` — the builder folds `$m` to `"process"`
and emits a `MethodCall` ref targeting `"process"`. Rename of `process`
finds the call site but **doesn't update the source string literal** in the
assignment.

**Proposed shape:**

```rust
pub struct Ref {
    // ... existing
    pub folded_from: Option<Span>,  // span of source string literal
}
```

When the builder emits a ref from a constant-folded value, store the source
string's span. `rename_sub` (and friends) check `folded_from` and add an
edit at the source span.

`constant_strings` needs to track spans alongside values:
`HashMap<String, Vec<(String, Span)>>`. Span follows the value through the
chain.

### Framework-attribute unified rename

`has name => (is => 'ro')` produces three things named `name`:

1. accessor `Method` symbol
2. `HashKeyDef` owned by `Sub("new")` (constructor key — already emitted)
3. `HashKeyDef` for internal `$self->{name}` access (when present)

Renaming any one of them today doesn't update the others. The fix is a
unified rename helper that, when `rename_kind_at` recognizes a framework
attribute, collects:

- accessor Method symbol(s) named `old_name` in the class
- `HashKeyDef` symbols owned by `Sub("new")` with the name (constructor)
- `HashKeyDef` symbols owned by the class (internal hash)
- `MethodCall` / `FunctionCall` refs targeting the name
- `HashKeyAccess` refs targeting the name with matching owner

Detection: symbol was synthesized by framework accessor code, OR the
`HashKeyDef` is owned by `Sub("new")` in a Moo/Moose/Mojo::Base class.

### Import list rename verification

`use Foo qw(bar)` — the builder emits a `FunctionCall` ref for `bar` via
`emit_refs_for_strings`. When `sub bar` in `Foo` is renamed, `rename_sub`
should find this ref and update the import list. **May already work** —
needs a regression test, then either pin or fix.

### Package rename → file rename (stretch)

Renaming `MyApp::Controller::Users` should offer to rename
`lib/MyApp/Controller/Users.pm`. LSP's `WorkspaceEdit.documentChanges`
supports `RenameFile`. Compute expected path from package name; include in
edit if the file exists.

### Inheritance override scoping (stretch)

Renaming `Animal::speak` should surface `Dog::speak` (intentional API) and
NOT rename `unrelated::speak` (accidental name collision). Today's
`rename_sub` searches by name across all files — too aggressive.

Needs reverse parent lookup (`child_classes_of(parent)`) across the
workspace. Data is there in `package_parents`; building the reverse is a
scan.

## Test coverage to add

```rust
#[test]
fn test_constant_fold_rename_updates_source_string() {
    // my $m = 'process'; $self->$m()
    // Rename 'process' → updates sub def, $self->$m() call site, AND
    // the 'process' string literal.
}

#[test]
fn test_framework_attribute_unified_rename() {
    // package Foo; use Moo; has name => (is => 'ro');
    // Foo->new(name => 'x'); $foo->name; $self->{name}
    // Rename from any position → updates all four.
}

#[test]
fn test_import_list_renamed_with_sub() {
    // use Foo qw(bar); bar();
    // Rename sub bar in Foo → updates 'bar' in qw(), bar() call site,
    // and sub bar def.
}
```
