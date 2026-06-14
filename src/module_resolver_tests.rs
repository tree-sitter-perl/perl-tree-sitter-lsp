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
    let edges = ModuleEdgeIndexes::new();
    let cached = Arc::new(CachedModule::new(PathBuf::from("/x/Demo/Has/Event.pm"), analysis));

    // Workspace-index style insert: a resolved module.
    insert_into_cache(&cache, &edges, "Demo::Has::Event", Some(cached));
    assert!(cache.get("Demo::Has::Event").as_deref().unwrap().is_some());

    // On-demand resolver miss: `None`. Must NOT clobber the indexed copy.
    insert_into_cache(&cache, &edges, "Demo::Has::Event", None);
    assert!(
        cache.get("Demo::Has::Event").as_deref().unwrap().is_some(),
        "a None on-demand miss clobbered an already-indexed module",
    );

    // A genuine resolved entry still updates (sanity: the guard only
    // protects against None-over-Some).
    let tree2 = parser.parse(source, None).unwrap();
    let analysis2 = std::sync::Arc::new(crate::builder::build(&tree2, source.as_bytes()));
    let cached2 = Arc::new(CachedModule::new(PathBuf::from("/y/Demo/Has/Event.pm"), analysis2));
    insert_into_cache(&cache, &edges, "Demo::Has::Event", Some(cached2));
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

#[test]
fn entrypoint_scan_finds_shebang_scripts_in_conventional_dirs() {
    let dir = std::env::temp_dir().join(format!("qx-entry-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("bin")).unwrap();
    std::fs::create_dir_all(dir.join("script")).unwrap();
    std::fs::create_dir_all(dir.join("lib")).unwrap();
    // root-level Perl entrypoint (no extension) — found
    std::fs::write(dir.join("jobs"), "#!/usr/bin/env perl\nuse Mojolicious::Lite;\n").unwrap();
    // bin/ + script/ entrypoints — found
    std::fs::write(dir.join("bin/login"), "#! /usr/bin/perl\n").unwrap();
    std::fs::write(dir.join("script/cron"), "#!/usr/bin/env perl\n").unwrap();
    // non-Perl shebang — not found
    std::fs::write(dir.join("deploy"), "#!/bin/bash\n").unwrap();
    // extensionless script buried in lib/ — NOT scanned by default
    std::fs::write(dir.join("lib/buried"), "#!/usr/bin/env perl\n").unwrap();

    let mut found: Vec<String> = scan_entrypoint_scripts(&dir, &[])
        .iter()
        .map(|p| p.file_name().unwrap().to_string_lossy().to_string())
        .collect();
    found.sort();
    assert_eq!(found, vec!["cron", "jobs", "login"]);

    // the config seam: an `extra` dir brings its entrypoints in.
    std::fs::create_dir_all(dir.join("daemons")).unwrap();
    std::fs::write(dir.join("daemons/worker"), "#!/usr/bin/env perl\n").unwrap();
    let mut with_extra: Vec<String> = scan_entrypoint_scripts(&dir, &["daemons".into()])
        .iter()
        .map(|p| p.file_name().unwrap().to_string_lossy().to_string())
        .collect();
    with_extra.sort();
    assert_eq!(with_extra, vec!["cron", "jobs", "login", "worker"]);
    std::fs::remove_dir_all(&dir).ok();
}
