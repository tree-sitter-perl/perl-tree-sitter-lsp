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
fn insert_into_cache_none_does_not_clobber_indexed_module() {
    // A workspace-indexed module (built with plugins, carries a Handler
    // symbol) must survive a later on-demand @INC miss for the same name.
    // The miss happens for project modules under a relative `use lib` the
    // resolver's @INC doesn't cover; clobbering with `None` while leaving
    // the reverse index pointing at it orphaned cross-file Handler /
    // dispatch lookup (mojo-events goto-def + sig help).
    let source = r#"
package Demo::Has::Event;
use parent 'Mojo::EventEmitter';
sub new {
    my $self = bless {}, shift;
    $self->on('ready', sub { my ($s, $ts) = @_; });
    $self;
}
1;
"#;
    let mut parser = create_parser();
    let tree = parser.parse(source, None).unwrap();
    let analysis = std::sync::Arc::new(crate::builder::build(&tree, source.as_bytes()));
    assert!(
        analysis.symbols.iter().any(|s| matches!(s.kind, crate::file_analysis::SymKind::Handler)),
        "fixture should synthesize a Handler symbol via the mojo-events plugin",
    );

    let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
    let reverse_index: DashMap<String, Vec<String>> = DashMap::new();
    let bridges_index: DashMap<String, Vec<String>> = DashMap::new();
    let cached = Arc::new(CachedModule::new(PathBuf::from("/x/Demo/Has/Event.pm"), analysis));

    // Workspace-index style insert: a resolved module.
    insert_into_cache(&cache, &reverse_index, &bridges_index, "Demo::Has::Event", Some(cached));
    assert!(cache.get("Demo::Has::Event").as_deref().unwrap().is_some());

    // On-demand resolver miss: `None`. Must NOT clobber the indexed copy.
    insert_into_cache(&cache, &reverse_index, &bridges_index, "Demo::Has::Event", None);
    assert!(
        cache.get("Demo::Has::Event").as_deref().unwrap().is_some(),
        "a None on-demand miss clobbered an already-indexed module",
    );

    // A genuine resolved entry still updates (sanity: the guard only
    // protects against None-over-Some).
    let tree2 = parser.parse(source, None).unwrap();
    let analysis2 = std::sync::Arc::new(crate::builder::build(&tree2, source.as_bytes()));
    let cached2 = Arc::new(CachedModule::new(PathBuf::from("/y/Demo/Has/Event.pm"), analysis2));
    insert_into_cache(&cache, &reverse_index, &bridges_index, "Demo::Has::Event", Some(cached2));
    assert_eq!(
        cache.get("Demo::Has::Event").as_deref().unwrap().as_ref().unwrap().path,
        PathBuf::from("/y/Demo/Has/Event.pm"),
    );
}

#[test]
fn test_uri_to_path() {
    assert_eq!(
        uri_to_path("file:///Users/foo/project"),
        Some(PathBuf::from("/Users/foo/project"))
    );
    assert_eq!(uri_to_path("http://example.com"), None);
}
