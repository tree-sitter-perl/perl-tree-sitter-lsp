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

    assert!(idx.reverse_index.contains_key("foo"));
    assert!(idx.reverse_index.contains_key("bar"));
    assert_eq!(idx.find_exporters("foo"), vec!["My::Mod"]);
    assert_eq!(idx.find_exporters("bar"), vec!["My::Mod"]);
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

    assert_eq!(
        idx.get_return_type_cached("get_config"),
        Some(InferredType::HashRef)
    );
    assert_eq!(
        idx.get_return_type_cached("make_obj"),
        Some(InferredType::ClassName("MyClass".into()))
    );
    assert_eq!(idx.get_return_type_cached("nonexistent"), None);
}
