use super::*;

fn parse_source_to_cached(source: &str, module_name: &str) -> Arc<CachedModule> {
    use tree_sitter::Parser;
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    let analysis = crate::builder::build(&tree, source.as_bytes());
    Arc::new(CachedModule::new(
        PathBuf::from(format!("/fake/{}.pm", module_name.replace("::", "/"))),
        Arc::new(analysis),
    ))
}

#[test]
fn test_resolve_module_list_util() {
    let idx = ModuleIndex::new_for_test();
    let path = idx.resolve_module("List::Util");
    if !idx.inc_paths().is_empty() {
        assert!(path.is_some(), "List::Util should be resolvable");
        let p = path.unwrap();
        assert!(p.to_str().unwrap().contains("List/Util.pm"));
    }
}

#[test]
fn test_extract_exports_list_util() {
    let idx = ModuleIndex::new_for_test();
    if idx.inc_paths().is_empty() {
        return;
    }
    let cached = idx.get_cached_blocking("List::Util");
    assert!(cached.is_some(), "Should parse List::Util");
    let cached = cached.unwrap();
    assert!(
        cached.analysis.export_ok.contains(&"first".to_string()),
        "List::Util should export_ok 'first', got: {:?}",
        cached.analysis.export_ok
    );
    assert!(
        cached.analysis.export_ok.contains(&"any".to_string()),
        "List::Util should export_ok 'any'"
    );
    assert!(
        cached.analysis.export_ok.contains(&"min".to_string()),
        "List::Util should export_ok 'min'"
    );
}

#[test]
fn test_module_resolution_not_found() {
    let idx = ModuleIndex::new_for_test();
    assert!(idx.resolve_module("Nonexistent::Module::XYZ123").is_none());
}

#[test]
fn test_resolver_thread_flow() {
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);
    if idx.inc_paths().is_empty() {
        return;
    }
    idx.request_resolve("Carp");
    assert!(
        idx.wait_resolved("Carp", std::time::Duration::from_secs(10)),
        "Carp should be resolved via thread"
    );
    let cached = idx.get_cached("Carp").unwrap();
    assert!(
        cached.analysis.export.contains(&"carp".to_string()),
        "Carp should export 'carp', got: {:?}",
        cached.analysis.export
    );
    assert!(
        cached.analysis.export.contains(&"croak".to_string()),
        "Carp should export 'croak'"
    );
}

#[test]
fn test_find_exporters() {
    let idx = ModuleIndex::new_for_test();

    let foobar_src = "package Foo::Bar;\nour @EXPORT = qw(alpha);\nour @EXPORT_OK = qw(beta);\nsub alpha {}\nsub beta {}\n1;";
    idx.insert_cache(
        "Foo::Bar",
        Some(parse_source_to_cached(foobar_src, "Foo::Bar")),
    );

    let bazqux_src =
        "package Baz::Qux;\nour @EXPORT_OK = qw(beta gamma);\nsub beta {}\nsub gamma {}\n1;";
    idx.insert_cache(
        "Baz::Qux",
        Some(parse_source_to_cached(bazqux_src, "Baz::Qux")),
    );

    assert_eq!(idx.find_exporters("alpha"), vec!["Foo::Bar"]);
    assert_eq!(idx.find_exporters("beta"), vec!["Baz::Qux", "Foo::Bar"]);
    assert!(idx.find_exporters("nonexistent").is_empty());
}

#[test]
fn test_find_exporters_uses_reverse_index() {
    let idx = ModuleIndex::new_for_test();
    let src = "package My::Mod;\nour @EXPORT = qw(foo);\nour @EXPORT_OK = qw(bar);\nsub foo {}\nsub bar {}\n1;";
    idx.insert_cache("My::Mod", Some(parse_source_to_cached(src, "My::Mod")));

    assert!(!idx.modules_with_symbol("foo").is_empty());
    assert!(!idx.modules_with_symbol("bar").is_empty());
    assert_eq!(idx.find_exporters("foo"), vec!["My::Mod"]);
    assert_eq!(idx.find_exporters("bar"), vec!["My::Mod"]);
}

#[test]
fn test_rebuild_reverse_index_recovers_warm_path_exporters() {
    // The warm path (`warm_cache`) writes straight into `cache_raw()` and
    // never touches the reverse index, so `find_exporters` is blind until a
    // rebuild. The export-only name (`weaken`-style XS export with no Perl
    // body, hence no `symbols` entry) is the case `indexable_symbol_names`
    // alone misses — the B6 cold/warm attribution regression.
    let idx = ModuleIndex::new_for_test();
    let src = "package Scalar::Util;\nour @EXPORT_OK = qw(weaken blessed);\n1;";
    let cached = parse_source_to_cached(src, "Scalar::Util");

    // Simulate warm_cache: direct insert, no reverse-index update.
    idx.cache_raw().insert("Scalar::Util".to_string(), Some(cached));
    assert!(
        idx.find_exporters("weaken").is_empty(),
        "warm insert must not populate the reverse index on its own"
    );

    idx.rebuild_reverse_index_from_cache();
    assert_eq!(idx.find_exporters("weaken"), vec!["Scalar::Util"]);
    assert_eq!(idx.find_exporters("blessed"), vec!["Scalar::Util"]);
}

#[test]
fn test_find_exporters_exporter_extensible() {
    // Names declared via `export(...)` and `:Export` attributes are
    // discoverable cross-file — the goto-def proxy for a consumer's import.
    let idx = ModuleIndex::new_for_test();
    let src = "package My::Ext;\nuse Exporter::Extensible -exporter_setup => 1;\nexport(qw( foo $bar -tag ));\nsub foo {}\nsub bar :Export {}\n1;";
    idx.insert_cache("My::Ext", Some(parse_source_to_cached(src, "My::Ext")));
    assert_eq!(idx.find_exporters("foo"), vec!["My::Ext"]);
    assert_eq!(idx.find_exporters("bar"), vec!["My::Ext"]);
    // Sigil'd / tag entries aren't subs — not advertised.
    assert!(idx.find_exporters("$bar").is_empty());
    assert!(idx.find_exporters("-tag").is_empty());
}

#[test]
fn test_find_exporters_exporter_declare() {
    let idx = ModuleIndex::new_for_test();
    let src = "package My::Decl;\nuse Exporter::Declare;\ndefault_export foo => sub { 1 };\nexport bar => sub { 2 };\nexports qw/a b/;\nsub bar {}\n1;";
    idx.insert_cache("My::Decl", Some(parse_source_to_cached(src, "My::Decl")));
    assert_eq!(idx.find_exporters("foo"), vec!["My::Decl"]);
    assert_eq!(idx.find_exporters("bar"), vec!["My::Decl"]);
    assert_eq!(idx.find_exporters("a"), vec!["My::Decl"]);
}

#[test]
fn test_find_exporters_importer_menu() {
    let idx = ModuleIndex::new_for_test();
    let src = "package My::Menu;\nsub IMPORTER_MENU {\n  return ( export => [qw/foo bar/], export_ok => ['baz'] );\n}\nsub foo {}\n1;";
    idx.insert_cache("My::Menu", Some(parse_source_to_cached(src, "My::Menu")));
    assert_eq!(idx.find_exporters("foo"), vec!["My::Menu"]);
    assert_eq!(idx.find_exporters("baz"), vec!["My::Menu"]);
}

#[test]
fn test_get_return_type_cached() {
    use crate::file_analysis::InferredType;

    let idx = ModuleIndex::new_for_test();

    // Source with two exported subs with clear return types.
    let src = r#"
package Config::DB;
our @EXPORT_OK = qw(get_config make_obj);

sub get_config {
    return { host => 'localhost', port => 5432 };
}

sub make_obj {
    return MyClass->new;
}

1;
"#;
    idx.insert_cache(
        "Config::DB",
        Some(parse_source_to_cached(src, "Config::DB")),
    );

    assert!(
        idx.get_return_type_cached("get_config").is_some_and(|t| t.is_hash_shaped()),
        "hash-shaped",
    );
    assert_eq!(
        idx.get_return_type_cached("make_obj"),
        Some(InferredType::ClassName("MyClass".into()))
    );
    assert_eq!(idx.get_return_type_cached("nonexistent"), None);
}

#[test]
fn runtime_exporter_names_resolve_as_exporters() {
    // A package whose exports come from a runtime exporter setup
    // (Sub::Exporter / Moose::Exporter / Type::Library) must be found
    // by `find_exporters` so consumer goto-def / diagnostics resolve.
    let idx = ModuleIndex::new_for_test();

    let sub_exp = "package Sugar::Sub;\n\
        use Sub::Exporter -setup => { exports => [qw/sweeten/] };\n\
        sub sweeten { }\n1;";
    idx.insert_cache("Sugar::Sub", Some(parse_source_to_cached(sub_exp, "Sugar::Sub")));

    let moose_exp = "package Sugar::Moose;\n\
        use Moose::Exporter;\n\
        Moose::Exporter->setup_import_methods(as_is => [qw/has_column/]);\n\
        sub has_column { }\n1;";
    idx.insert_cache("Sugar::Moose", Some(parse_source_to_cached(moose_exp, "Sugar::Moose")));

    let type_lib = "package My::Types;\n\
        use Type::Library -base;\n\
        __PACKAGE__->add_type({ name => 'PositiveInt' });\n\
        sub PositiveInt { }\n1;";
    idx.insert_cache("My::Types", Some(parse_source_to_cached(type_lib, "My::Types")));

    assert_eq!(idx.find_exporters("sweeten"), vec!["Sugar::Sub"]);
    assert_eq!(idx.find_exporters("has_column"), vec!["Sugar::Moose"]);
    assert_eq!(idx.find_exporters("PositiveInt"), vec!["My::Types"]);
}

#[test]
fn test_children_index_direct_and_transitive() {
    let idx = ModuleIndex::new_for_test();

    let role = "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n1;";
    idx.insert_cache("My::Role", Some(parse_source_to_cached(role, "My::Role")));

    // Direct composer.
    let composer = "package My::Composer;\nuse Moo;\nwith 'My::Role';\nsub fetch { }\n1;";
    idx.insert_cache("My::Composer", Some(parse_source_to_cached(composer, "My::Composer")));

    // Role-composing-role, then a composer of THAT role — the
    // transitive hop the descendant walk must reach.
    let subrole = "package My::SubRole;\nuse Moo::Role;\nwith 'My::Role';\n1;";
    idx.insert_cache("My::SubRole", Some(parse_source_to_cached(subrole, "My::SubRole")));
    let deep = "package My::Deep;\nuse Moo;\nwith 'My::SubRole';\nsub fetch { }\n1;";
    idx.insert_cache("My::Deep", Some(parse_source_to_cached(deep, "My::Deep")));

    assert_eq!(
        idx.modules_with_parent("My::Role"),
        vec!["My::Composer", "My::SubRole"],
        "direct children only",
    );

    let mut packages: Vec<String> = Vec::new();
    idx.for_each_descendant_package("My::Role", |pkg, _cached| {
        packages.push(pkg.to_string());
        std::ops::ControlFlow::Continue(())
    });
    packages.sort();
    assert_eq!(
        packages,
        vec!["My::Composer", "My::Deep", "My::SubRole"],
        "descendant walk crosses the role-composing-role hop",
    );
}

#[test]
fn test_children_index_survives_warm_rebuild_and_purge() {
    // B6: the children edge must be fed by the warm rebuild path, not
    // just the insert path — and purged on re-registration.
    let idx = ModuleIndex::new_for_test();
    let child = "package Kid;\nuse parent 'Base::Class';\n1;";
    let cached = parse_source_to_cached(child, "Kid");

    // Simulate warm_cache: direct insert, indexes untouched.
    idx.cache_raw().insert("Kid".to_string(), Some(cached));
    assert!(idx.modules_with_parent("Base::Class").is_empty());

    idx.rebuild_reverse_index_from_cache();
    assert_eq!(idx.modules_with_parent("Base::Class"), vec!["Kid"]);

    // Re-registration with the parent edge gone must drop the stale edge.
    let orphaned = "package Kid;\nsub solo { }\n1;";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(orphaned, None).unwrap();
    let analysis = Arc::new(crate::builder::build(&tree, orphaned.as_bytes()));
    idx.register_workspace_module(PathBuf::from("/fake/Kid.pm"), analysis);
    assert!(
        idx.modules_with_parent("Base::Class").is_empty(),
        "purge on re-registration must drop the stale parent edge",
    );
}
