use super::*;

#[test]
fn test_resolve_module_list_util() {
    let inc_paths = discover_inc_paths();
    if inc_paths.is_empty() {
        return;
    }
    let path = resolve_module_path(&inc_paths, "List::Util");
    assert!(path.is_some(), "List::Util should be resolvable");
    let p = path.unwrap();
    assert!(p.to_str().unwrap().contains("List/Util.pm"));
}

#[test]
fn test_extract_exports_qw() {
    let source = r#"
package Foo;
use Exporter 'import';
our @EXPORT_OK = qw(alpha beta gamma);
our @EXPORT = qw(delta);
1;
"#;
    let mut parser = create_parser();
    let tree = parser.parse(source, None).unwrap();
    let analysis = crate::builder::build(&tree, source.as_bytes());
    assert_eq!(analysis.export, vec!["delta"]);
    assert_eq!(analysis.export_ok, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn test_extract_exports_parenthesized() {
    let source = r#"
package Bar;
our @EXPORT_OK = ('foo', 'bar', 'baz');
1;
"#;
    let mut parser = create_parser();
    let tree = parser.parse(source, None).unwrap();
    let analysis = crate::builder::build(&tree, source.as_bytes());
    assert_eq!(analysis.export_ok, vec!["foo", "bar", "baz"]);
}

#[test]
fn test_discover_inc_paths() {
    let paths = discover_inc_paths();
    if !paths.is_empty() {
        assert!(paths.iter().all(|p| p.is_dir()));
    }
}

#[test]
fn test_uri_to_path() {
    assert_eq!(
        uri_to_path("file:///Users/foo/project"),
        Some(PathBuf::from("/Users/foo/project"))
    );
    assert_eq!(uri_to_path("http://example.com"), None);
}
