# Task: Inheritance Chain Resolution

**Branch:** create `inheritance` from `main`
**Baseline:** 229 tests, 0 warnings

## Goal

When a user types `$obj->` and `$obj` is typed as `Child`, show methods from `Child` AND all its ancestors. Same for go-to-def, hover, and return type resolution through method chains.

Currently `complete_methods_for_class`, `find_method_in_class`, and `find_method_return_type` all search only the immediate class. This task adds parent chain walking — unified across all inheritance declaration styles.

## Inheritance sources (all must feed the same model)

| Source | Example | Prevalence |
|---|---|---|
| `use parent` | `use parent 'DBI::db'` | Modern standard |
| `use parent -norequire` | `use parent -norequire, 'Local::Base'` | Same, skip require |
| `use base` | `use base qw(Exporter LWP::UserAgent)` | Older, still common |
| `@ISA` assignment | `our @ISA = ('Foo', 'Bar')` | Legacy |
| `class :isa()` | `class Point3D :isa(Point)` | Perl 5.38 corinna |

All produce the same output: a list of parent class names for the current package.

## Data model

### FileAnalysis: unified package parents

```rust
/// Parent classes for each package in this file.
/// Populated by the builder from use parent/base, @ISA, and class :isa.
pub package_parents: HashMap<String, Vec<String>>,
```

This is on `FileAnalysis`, not `SymbolDetail`. A package can be declared with `package` (no `SymbolDetail::Class`) and still have parents via `use parent`. The `SymbolDetail::Class { parent, .. }` field continues to exist for Perl 5.38 class-specific data (fields, roles), but the canonical inheritance source becomes `package_parents`.

The builder populates `package_parents` from ALL sources. For `class :isa(Foo)`, it writes to BOTH `SymbolDetail::Class { parent }` (for field/role data) and `package_parents` (for method resolution).

### ModuleExports: cross-file parents

```rust
pub struct ModuleExports {
    pub path: PathBuf,
    pub export: Vec<String>,
    pub export_ok: Vec<String>,
    pub subs: HashMap<String, ExportedSub>,
    pub parents: Vec<String>,  // NEW
}
```

The subprocess extraction emits parents in JSON. The resolver stores them. Method resolution can walk the chain across module boundaries.

### ModuleIndex: parent lookup

```rust
/// Return cached parent classes for a module.
pub fn parents_cached(&self, module_name: &str) -> Vec<String> {
    self.cache.get(module_name)
        .and_then(|entry| entry.as_ref())
        .map(|e| e.parents.clone())
        .unwrap_or_default()
}
```

## Builder extraction

All inheritance detection happens during the CST walk in `builder.rs`. The builder already tracks `current_package`. When it encounters an inheritance declaration, it appends to a `package_parents: HashMap<String, Vec<String>>` on the builder state, which transfers to `FileAnalysis` at the end.

### `use parent` / `use base`

When visiting a `use_statement` where the module is `parent` or `base`:
- Extract the class names from the arguments (string literals, `qw()` lists)
- Skip `-norequire` flag arguments (starts with `-`)
- Append to `package_parents[current_package]`

### `@ISA` assignment

When visiting an assignment where the LHS is `@ISA`:
- Extract class names from the RHS (string literals in a list, `qw()`)
- Replace (not append) `package_parents[current_package]` since `@ISA =` is a full replacement

### `class :isa(Parent)`

Already extracted into `SymbolDetail::Class { parent }`. Add a parallel write to `package_parents`.

## Method resolution with inheritance

### Core: `resolve_method_in_ancestors`

A new method on `FileAnalysis` that walks the parent chain:

```rust
/// Walk the inheritance chain to find a method.
/// Returns the class it was found in + the method's SymbolId (for local)
/// or class name (for cross-file lookup).
pub fn resolve_method_in_ancestors(
    &self,
    class_name: &str,
    method_name: &str,
    module_index: Option<&ModuleIndex>,
) -> Option<MethodResolution>

pub enum MethodResolution {
    /// Found in a local class within this file.
    Local { class: String, sym_id: SymbolId },
    /// Found in a cross-file module (use ModuleIndex to get details).
    CrossFile { class: String },
}
```

**Algorithm (DFS, matches Perl's default MRO):**

1. Check `class_name` directly (current behavior)
2. Look up `self.package_parents[class_name]` for local parents
3. For each parent:
   a. If parent is a local package → recurse with `resolve_method_in_ancestors`
   b. If parent is a cross-file module → check `module_index.get_exports_cached(parent).subs[method_name]`
   c. If found, return it. If not, get the parent's parents via `module_index.parents_cached(parent)` and recurse
4. Stop at depth 20 (safety limit for circular inheritance)

### Wire into existing call sites

There are 5 methods in `file_analysis.rs` that currently search only the immediate class. Each needs the parent chain walk:

| Method | Currently does | Change |
|---|---|---|
| `complete_methods_for_class` | Collects methods from one class | Collect from class + all ancestors, dedup by name (child wins) |
| `find_method_in_class` | Finds method def span in one class | Walk ancestors, return first match |
| `find_method_return_type` | Finds return type in one class | Walk ancestors, return first match |
| `method_detail` | Formats "Class → Type" | Show actual defining class if inherited |
| `symbol_in_class` | Checks scope chain for one class | Also match if class is an ancestor of the target |

**Important:** These methods currently don't take a `ModuleIndex` parameter. They'll need an `Option<&ModuleIndex>` parameter (or access it through an enrichment pass). The local-only path (no ModuleIndex) still works for single-file analysis and tests.

### Completion detail for inherited methods

When a method is inherited, the completion detail should indicate the source:

```
$obj->connect()    DBI::db (from DBI)
$obj->prepare()    DBI::db
$obj->new()        DBI::db (from Class::Accessor)
```

Local methods show the class name. Inherited methods show "ClassName (from AncestorName)".

## Cross-file storage

### Subprocess output

`subprocess_main` and `resolve_and_parse` already extract module metadata. Add parent extraction:

```json
{
  "export": ["connect"],
  "export_ok": ["..."],
  "subs": { ... },
  "parents": ["DBI", "Exporter"]
}
```

The subprocess runs the full builder on the module file. Read `package_parents` from the resulting `FileAnalysis` — use the primary package's parents (the package matching the module name).

### SQLite

Add a `parents TEXT NOT NULL DEFAULT '[]'` column to the modules table. This is a real schema change → bump `SCHEMA_VERSION` to `"8"`.

JSON array of class name strings: `["DBI", "Exporter"]`.

### ModuleExports

Add `parents: Vec<String>` to `ModuleExports`. Serialize/deserialize as JSON array.

### EXTRACT_VERSION

Also bump `EXTRACT_VERSION` to `2`. Existing cached modules without parent info get lazily re-resolved (the extract versioning system from the previous PR handles this).

## What about multiple packages per file?

A single `.pm` file can define multiple packages. `package_parents` is a HashMap keyed by package name, so it handles this naturally. For cross-file storage, `ModuleExports.parents` stores the parents of the *primary* package (the one matching the module name). Methods from secondary packages in the same file aren't exported anyway, so this is sufficient.

## What NOT to do

- Don't implement C3 MRO — DFS is correct for single inheritance (vast majority) and reasonable for multiple. C3 can be added later if needed.
- Don't try to resolve parents at query time via `require` — only use what's in the builder output and the module cache
- Don't walk inheritance for non-method lookups (variables, hash keys) — only methods are inherited in Perl
- Don't add Moo/Moose `extends`/`with` in this PR — the data model supports it but extraction is a separate task
- Don't change `SymbolDetail::Class` structure — it keeps its field/role data. `package_parents` is the unified inheritance source
- Don't add circular inheritance detection beyond the depth limit — it's not worth the complexity

## Tests

### Unit tests (builder — extraction)

```rust
#[test]
fn test_use_parent_single() {
    let fa = build_fa("
        package Child;
        use parent 'Parent';
        sub child_method { }
    ");
    assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
}

#[test]
fn test_use_parent_multiple() {
    let fa = build_fa("
        package Multi;
        use parent qw(Foo Bar);
    ");
    assert_eq!(fa.package_parents.get("Multi").unwrap(), &vec!["Foo".to_string(), "Bar".to_string()]);
}

#[test]
fn test_use_parent_norequire() {
    let fa = build_fa("
        package Local;
        use parent -norequire, 'My::Base';
    ");
    assert_eq!(fa.package_parents.get("Local").unwrap(), &vec!["My::Base".to_string()]);
}

#[test]
fn test_use_base() {
    let fa = build_fa("
        package Old;
        use base 'Legacy::Base';
    ");
    assert_eq!(fa.package_parents.get("Old").unwrap(), &vec!["Legacy::Base".to_string()]);
}

#[test]
fn test_isa_assignment() {
    let fa = build_fa("
        package Direct;
        our @ISA = ('Alpha', 'Beta');
    ");
    assert_eq!(fa.package_parents.get("Direct").unwrap(), &vec!["Alpha".to_string(), "Beta".to_string()]);
}

#[test]
fn test_class_isa_populates_package_parents() {
    let fa = build_fa("
        class Child :isa(Parent) { }
    ");
    assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
}
```

### Unit tests (method resolution — local inheritance)

```rust
#[test]
fn test_inherited_method_completion() {
    let fa = build_fa("
        package Animal;
        sub speak { }
        sub eat { }

        package Dog;
        use parent 'Animal';
        sub fetch { }
    ");
    let methods = fa.complete_methods_for_class("Dog");
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"fetch"), "own method");
    assert!(names.contains(&"speak"), "inherited from Animal");
    assert!(names.contains(&"eat"), "inherited from Animal");
}

#[test]
fn test_child_method_overrides_parent() {
    let fa = build_fa("
        package Base;
        sub greet { }

        package Override;
        use parent 'Base';
        sub greet { }  // override
    ");
    let methods = fa.complete_methods_for_class("Override");
    let greet_count = methods.iter().filter(|c| c.label == "greet").count();
    assert_eq!(greet_count, 1, "child override should shadow parent");
}

#[test]
fn test_find_method_in_parent() {
    let fa = build_fa("
        package Base;
        sub base_method { }

        package Child;
        use parent 'Base';
    ");
    let span = fa.find_method_in_class("Child", "base_method");
    assert!(span.is_some(), "should find inherited method");
}

#[test]
fn test_inherited_return_type() {
    let fa = build_fa("
        package Factory;
        sub create { Factory->new(@_) }

        package SpecialFactory;
        use parent 'Factory';
    ");
    let rt = fa.find_method_return_type("SpecialFactory", "create");
    assert!(rt.is_some(), "should find return type from parent");
}

#[test]
fn test_multi_level_inheritance() {
    let fa = build_fa("
        package A;
        sub from_a { }

        package B;
        use parent 'A';
        sub from_b { }

        package C;
        use parent 'B';
        sub from_c { }
    ");
    let methods = fa.complete_methods_for_class("C");
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"from_a"));
    assert!(names.contains(&"from_b"));
    assert!(names.contains(&"from_c"));
}

#[test]
fn test_class_isa_inherits_methods() {
    let fa = build_fa("
        class Parent {
            method greet() { }
        }
        class Child :isa(Parent) {
            method wave() { }
        }
    ");
    let methods = fa.complete_methods_for_class("Child");
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"wave"), "own method");
    assert!(names.contains(&"greet"), "inherited from Parent");
}
```

### Cross-file tests (using `insert_cache`)

`ModuleIndex::new_for_test()` + `insert_cache()` lets us build an arbitrary module graph in memory — no filesystem, no subprocess, no resolver thread. This is how existing cross-file tests work (see `test_find_exporters`, `test_get_return_type_cached`).

```rust
#[test]
fn test_cross_file_inherited_method_completion() {
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // Grandparent: DBI has `connect`
    idx.insert_cache("DBI", Some(ModuleExports {
        path: PathBuf::from("/fake/DBI.pm"),
        export: vec![],
        export_ok: vec![],
        subs: {
            let mut s = HashMap::new();
            s.insert("connect".into(), ExportedSub {
                def_line: 10, params: vec![], is_method: true,
                return_type: None, hash_keys: vec![], doc: None,
            });
            s
        },
        parents: vec![],
    }));

    // Parent: DBI::db inherits from DBI, has `prepare`
    idx.insert_cache("DBI::db", Some(ModuleExports {
        path: PathBuf::from("/fake/DBI/db.pm"),
        export: vec![],
        export_ok: vec![],
        subs: {
            let mut s = HashMap::new();
            s.insert("prepare".into(), ExportedSub {
                def_line: 5, params: vec![], is_method: true,
                return_type: None, hash_keys: vec![], doc: None,
            });
            s
        },
        parents: vec!["DBI".into()],
    }));

    // Local code inherits from DBI::db
    let fa = build_fa("
        package MyDB;
        use parent 'DBI::db';
        sub custom_query { }
    ");

    let methods = fa.complete_methods_for_class("MyDB", Some(&idx));
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"custom_query"), "own method");
    assert!(names.contains(&"prepare"), "from DBI::db");
    assert!(names.contains(&"connect"), "from DBI (grandparent)");
}

#[test]
fn test_cross_file_method_override() {
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // Parent has `process`
    idx.insert_cache("Base::Worker", Some(ModuleExports {
        path: PathBuf::from("/fake/Base/Worker.pm"),
        export: vec![],
        export_ok: vec![],
        subs: {
            let mut s = HashMap::new();
            s.insert("process".into(), ExportedSub {
                def_line: 1, params: vec![], is_method: true,
                return_type: None, hash_keys: vec![], doc: None,
            });
            s
        },
        parents: vec![],
    }));

    // Local child overrides `process`
    let fa = build_fa("
        package MyWorker;
        use parent 'Base::Worker';
        sub process { }
    ");

    let methods = fa.complete_methods_for_class("MyWorker", Some(&idx));
    let process_count = methods.iter().filter(|c| c.label == "process").count();
    assert_eq!(process_count, 1, "local override should shadow parent");
}

#[test]
fn test_cross_file_return_type_through_inheritance() {
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    idx.insert_cache("Fetcher", Some(ModuleExports {
        path: PathBuf::from("/fake/Fetcher.pm"),
        export: vec![],
        export_ok: vec![],
        subs: {
            let mut s = HashMap::new();
            s.insert("fetch".into(), ExportedSub {
                def_line: 1, params: vec![], is_method: true,
                return_type: Some(InferredType::HashRef),
                hash_keys: vec!["status".into(), "body".into()],
                doc: None,
            });
            s
        },
        parents: vec![],
    }));

    let fa = build_fa("
        package MyFetcher;
        use parent 'Fetcher';
    ");

    let rt = fa.find_method_return_type("MyFetcher", "fetch", Some(&idx));
    assert_eq!(rt, Some(InferredType::HashRef));
}

#[test]
fn test_parents_cached() {
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    idx.insert_cache("Child::Mod", Some(ModuleExports {
        path: PathBuf::from("/fake/Child/Mod.pm"),
        export: vec![],
        export_ok: vec![],
        subs: HashMap::new(),
        parents: vec!["Parent::Mod".into(), "Mixin::Role".into()],
    }));

    let parents = idx.parents_cached("Child::Mod");
    assert_eq!(parents, vec!["Parent::Mod", "Mixin::Role"]);
    assert!(idx.parents_cached("Unknown::Mod").is_empty());
}
```

## Verification

1. `cargo test` — all 229 existing tests pass + new tests
2. `cargo build` — no warnings
3. Manual: `./target/release/perl-lsp --parse-exports /path/to/DBI/db.pm` — check `parents` field in output
4. Manual: open a file with `use parent 'Foo'`, type `$self->` inside a method — verify parent's methods appear in completion
5. Manual: go-to-def on an inherited method call — should jump to the parent's definition
6. Manual: hover on inherited method — should show the defining class
