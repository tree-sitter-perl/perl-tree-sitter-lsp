use super::*;
use std::path::PathBuf;

fn parse(source: &str) -> FileAnalysis {
    use tree_sitter::Parser;
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

#[test]
fn test_refs_to_finds_sub_across_workspace_files() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_test_a.pm");
    let path_b = PathBuf::from("/tmp/resolve_test_b.pm");

    // File A defines sub foo and exports it.
    let fa_a = parse("package A;\nour @EXPORT_OK = qw/foo/;\nsub foo { 42 }\n1;\n");
    store.insert_workspace(path_a.clone(), fa_a);

    // File B imports foo from A and calls it.
    let fa_b = parse("package B;\nuse A qw/foo/;\nsub bar { foo(); }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub {
                package: Some("A".to_string()),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    // Expect at least the decl in A and the call in B.
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)
                && r.access == AccessKind::Declaration),
        "expected declaration of foo in file A, got {:?}",
        results,
    );
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)
                && r.access == AccessKind::Read),
        "expected call to foo in file B, got {:?}",
        results,
    );
}

/// Exporter::Extensible: `export(...)` + `:Export` register subs as
/// exports without `@EXPORT_OK`. A consumer's `use X 'name'` must fan
/// out to the def under `refs_to`.
#[test]
fn test_refs_to_exporter_extensible_cross_file() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_ext_a.pm");
    let path_b = PathBuf::from("/tmp/resolve_ext_b.pm");

    let fa_a = parse(
        "package Ext;\nuse Exporter::Extensible -exporter_setup => 1;\nexport(qw/foo/);\nsub foo { 42 }\nsub bar :Export {}\n1;\n",
    );
    store.insert_workspace(path_a.clone(), fa_a);

    let fa_b = parse("package C;\nuse Ext qw/foo/;\nsub baz { foo(); }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub { package: Some("Ext".to_string()) },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)
            && r.access == AccessKind::Declaration),
        "expected declaration of foo in Ext, got {:?}",
        results,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)
            && r.access == AccessKind::Read),
        "expected call to foo in consumer, got {:?}",
        results,
    );
}

/// Exporter::Declare: `default_export name => sub {}` registers exports.
#[test]
fn test_refs_to_exporter_declare_cross_file() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_decl_a.pm");
    let path_b = PathBuf::from("/tmp/resolve_decl_b.pm");

    let fa_a = parse(
        "package Decl;\nuse Exporter::Declare;\ndefault_export foo => sub { 42 };\nsub foo { 42 }\n1;\n",
    );
    store.insert_workspace(path_a.clone(), fa_a);

    let fa_b = parse("package C;\nuse Decl qw/foo/;\nsub baz { foo(); }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub { package: Some("Decl".to_string()) },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)),
        "expected def of foo in Decl, got {:?}",
        results,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)
            && r.access == AccessKind::Read),
        "expected call to foo in consumer, got {:?}",
        results,
    );
}

/// Importer consumer form: `use Importer 'Src' => qw/foo/` imports foo
/// from Src — the call must fan out to Src's def, not stop at Importer.
#[test]
fn test_refs_to_importer_consumer_cross_file() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_imp_src.pm");
    let path_b = PathBuf::from("/tmp/resolve_imp_consumer.pm");

    let fa_a = parse("package Src::Mod;\nour @EXPORT_OK = qw/foo/;\nsub foo { 42 }\n1;\n");
    store.insert_workspace(path_a.clone(), fa_a);

    let fa_b = parse(
        "package C;\nuse Importer 'Src::Mod' => qw/foo/;\nsub baz { foo(); }\n1;\n",
    );
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub { package: Some("Src::Mod".to_string()) },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)
            && r.access == AccessKind::Declaration),
        "expected decl of foo in Src::Mod, got {:?}",
        results,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)),
        "expected import/call of foo in consumer pinned to Src::Mod, got {:?}",
        results,
    );
}

/// False-positive guard: a `sub export {}` in a non-exporter package
/// must not register phantom exports that pollute cross-file refs.
#[test]
fn test_refs_to_export_not_registered_without_use() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_noexport.pm");
    let fa_a = parse("package Plain;\nsub export {}\nexport('phantom');\n1;\n");
    store.insert_workspace(path_a.clone(), fa_a);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "phantom".to_string(),
            kind: TargetKind::Sub { package: Some("Plain".to_string()) },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(results.is_empty(), "no phantom export, got {:?}", results);
}

/// Adversarial #1: a dotted helper `users.create` and a route's
/// `Users#create` share a method name but live on different
/// classes. gr on one must NOT pick up the other.
///
/// Helper leaf lives on `Mojolicious::Controller::_Helper::users`.
/// Route target is `Users::create`. They only share a name.
#[test]
fn refs_to_helper_leaf_excludes_unrelated_route_with_same_method_name() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/helper_route_overlap.pm");

    let fa = parse(
        r#"
package MyApp;
use Mojolicious::Lite;

$app->helper('users.create', sub ($c, $user) {});
$app->routes->post('/users')->to(controller => 'Users', action => 'create');
"#,
    );
    store.insert_workspace(path.clone(), fa);

    // gr on the helper's `create` leaf (class = _Helper::users).
    // Must NOT include the route's `create` ref (targets Users).
    let helper_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "create".to_string(),
            kind: TargetKind::Method {
                class: "Mojolicious::Controller::_Helper::users".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    // Route's 'create' string sits at column ~67 on line 5
    // (0-indexed). Anything there is the route, not the helper.
    for r in &helper_results {
        let col = r.span.start.column;
        assert!(
            !(r.span.start.row == 5 && col > 50),
            "gr on helper leaf _Helper::users::create picked up the \
                 route's Users::create ref (line {}, col {}) — unrelated \
                 class, shouldn't appear",
            r.span.start.row,
            col,
        );
    }

    // Mirror: gr on the route's `create` target (class = Users).
    // Must NOT include the helper's Method declaration.
    let route_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "create".to_string(),
            kind: TargetKind::Method {
                class: "Users".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    // Helper's 'create' leaf is at line 4 col ~13 (inside the
    // 'users.create' string — the plugin narrows spans to the leaf
    // segment).
    for r in &route_results {
        assert!(
            !(r.span.start.row == 4 && r.span.start.column < 30),
            "gr on route Users::create picked up the helper leaf \
                 _Helper::users::create (line {}, col {}) — unrelated class",
            r.span.start.row,
            r.span.start.column,
        );
    }
}

/// Adversarial #2: two plain Perl packages, each with its own
/// `run` method. `$f->run` targets `Foo::run`; `$b->run` targets
/// `Bar::run`. gr on Foo's run must not union with Bar's.
#[test]
fn refs_to_method_is_class_scoped_plain_packages() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/two_classes_same_method.pm");

    let fa = parse(
        r#"
package Foo;
sub new { bless {}, shift }
sub run { "foo" }

package Bar;
sub new { bless {}, shift }
sub run { "bar" }

package main;
my $f = Foo->new;
my $b = Bar->new;
$f->run;
$b->run;
1;
"#,
    );
    store.insert_workspace(path.clone(), fa);

    let foo_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "run".to_string(),
            kind: TargetKind::Method {
                class: "Foo".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    // Must include Foo::run decl and `$f->run` call. Must NOT
    // include Bar::run decl or `$b->run` call.
    let foo_lines: Vec<usize> = foo_results.iter().map(|r| r.span.start.row).collect();
    assert!(
        foo_lines.contains(&3), // `sub run` in package Foo
        "Foo::run decl (line 3) missing from Foo results: {:?}",
        foo_lines
    );
    assert!(
        foo_lines.contains(&12), // `$f->run` call
        "$f->run call (line 12) missing from Foo results: {:?}",
        foo_lines
    );
    assert!(
        !foo_lines.contains(&7), // `sub run` in package Bar
        "Bar::run decl (line 7) wrongly included in Foo results: {:?}",
        foo_lines
    );
    assert!(
        !foo_lines.contains(&13), // `$b->run` call
        "$b->run call (line 13) wrongly included in Foo results: {:?}",
        foo_lines
    );

    let bar_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "run".to_string(),
            kind: TargetKind::Method {
                class: "Bar".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    let bar_lines: Vec<usize> = bar_results.iter().map(|r| r.span.start.row).collect();
    assert!(
        bar_lines.contains(&7),
        "Bar::run decl (line 7) missing from Bar results: {:?}",
        bar_lines
    );
    assert!(
        bar_lines.contains(&13),
        "$b->run call (line 13) missing from Bar results: {:?}",
        bar_lines
    );
    assert!(
        !bar_lines.contains(&3),
        "Foo::run decl (line 3) wrongly included in Bar results: {:?}",
        bar_lines
    );
    assert!(
        !bar_lines.contains(&12),
        "$f->run call (line 12) wrongly included in Bar results: {:?}",
        bar_lines
    );
}

/// NAV (b) repro (i): method references read the build-time-frozen
/// dispatch edge, not a query-time invocant re-derivation. An untyped
/// invocant (`my $w = COND ? external() : undef`) stamps NO edge, so its
/// `->frobnicate` sites are deterministically EXCLUDED, while the typed
/// `$self->frobnicate` (enclosing-class invocant) IS included. This is
/// the determinism the unification buys: no enriched-vs-workspace
/// divergence — the edge is frozen once per build.
#[test]
fn refs_to_method_excludes_untyped_invocant_includes_self() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/nav_untyped_invocant.pm");

    let fa = parse(
        r#"package Widget;
sub frobnicate { 1 }
sub run {
  my $self = shift;
  my $w = $ENV{X} ? external() : undef;
  $w->frobnicate;
  $w->frobnicate;
  $self->frobnicate;
}
1;
"#,
    );
    store.insert_workspace(path.clone(), fa);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "frobnicate".to_string(),
            kind: TargetKind::Method {
                class: "Widget".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    let lines: Vec<usize> = results.iter().map(|r| r.span.start.row).collect();
    // def `sub frobnicate` (row 1) + `$self->frobnicate` (row 7).
    assert!(
        lines.contains(&1),
        "frobnicate decl (row 1) missing: {:?}",
        lines
    );
    assert!(
        lines.contains(&7),
        "$self->frobnicate (row 7) missing: {:?}",
        lines
    );
    // Both untyped `$w->frobnicate` sites (rows 5, 6) EXCLUDED.
    assert!(
        !lines.contains(&5) && !lines.contains(&6),
        "untyped $w->frobnicate sites (rows 5,6) wrongly included: {:?}",
        lines
    );
    // Exactly two hits: the decl + the one typed call.
    assert_eq!(
        results.len(),
        2,
        "expected exactly decl + $self call, got {:?}",
        lines
    );
}

/// NAV regression (iv): a typed same-file invocant (`my $w = Widget->new`)
/// still resolves fully — every `$w->frobnicate` site is matched via the
/// frozen edge. Guards against the unification dropping valid matches.
#[test]
fn refs_to_method_typed_same_file_invocant_resolves_fully() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/nav_typed_invocant.pm");

    let fa = parse(
        r#"package Widget;
sub new { bless {}, shift }
sub frobnicate { 1 }
sub run {
  my $w = Widget->new;
  $w->frobnicate;
  $w->frobnicate;
}
1;
"#,
    );
    store.insert_workspace(path.clone(), fa);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "frobnicate".to_string(),
            kind: TargetKind::Method {
                class: "Widget".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    let lines: Vec<usize> = results.iter().map(|r| r.span.start.row).collect();
    assert!(lines.contains(&2), "frobnicate decl (row 2) missing: {:?}", lines);
    assert!(
        lines.iter().filter(|&&l| l == 5).count() == 1
            && lines.iter().filter(|&&l| l == 6).count() == 1,
        "both typed $w->frobnicate sites (rows 5,6) must resolve: {:?}",
        lines
    );
}

/// NAV honest-miss: a genuinely-untyped receiver — `my $x =
/// external(); $x->m()` — has no inferable class, so NO dispatch edge
/// is stamped, and goto-def returns `None`. It must NEVER jump to an
/// arbitrary same-named sub (the deleted same-name fallback). This is
/// the libwww case: `$ua->get` where `$ua` came from an opaque
/// constructor we can't see.
#[test]
fn goto_def_untyped_receiver_is_honest_miss() {
    use tree_sitter::{Parser, Point};
    let src = r#"package Foo;
sub m { 1 }
package main;
my $x = external();
$x->m();
"#;
    let mut parser = Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());
    // Cursor on `m` in `$x->m()`.
    let row = src.lines().position(|l| l.starts_with("$x->m")).unwrap();
    let col = "$x->".len();
    let def = fa.find_definition(Point::new(row, col), None);
    assert_eq!(
        def, None,
        "untyped `$x->m` (where $x = external()) must be an honest miss, \
         never a same-name jump to Foo::m. got: {:?}",
        def,
    );
}

/// NAV honest-miss, multi-candidate flood guard: two unrelated classes
/// both define `frob`. An untyped `$x->frob` must resolve to `None` —
/// no flood of arbitrary same-named subs. With the same-name fallback
/// gone, the absence of an edge is the only answer.
#[test]
fn goto_def_untyped_receiver_multi_candidate_no_flood() {
    use tree_sitter::{Parser, Point};
    let src = r#"package A;
sub frob { 1 }
package B;
sub frob { 2 }
package main;
my $x = make_something();
$x->frob;
"#;
    let mut parser = Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());
    let row = src.lines().position(|l| l.starts_with("$x->frob")).unwrap();
    let col = "$x->".len();
    let def = fa.find_definition(Point::new(row, col), None);
    assert_eq!(
        def, None,
        "untyped `$x->frob` with two unrelated `frob` definitions must \
         be None — no same-name flood. got: {:?}",
        def,
    );
}

/// Adversarial #3: Corinna classes with same method name. Both
/// call shapes must class-resolve correctly:
///
///   (a) Variable-bound: `my $s = Sner->new; $s->hi` — `$s` gets
///       a ClassName(Sner) type constraint, so invocant resolution
///       finds the class via the variable-type flow.
///   (b) Inline chain: `Sner->new->hi` — the outer `->hi`'s
///       invocant is a `method_call_expression`. The build-time
///       `invocant_class` field on MethodCall refs is populated by
///       `resolve_invocant_class_tree`, which walks chain invocants
///       including `<Class>->new` constructors.
///
/// Critical invariant either way: no cross-linking between Sner
/// and Bler.
#[test]
fn refs_to_method_is_class_scoped_corinna() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/corinna_classes.pm");

    let fa = parse(
        r#"use v5.38;

class Sner {
    method hi {}
}
class Bler {
    method hi {}
}

my $s = Sner->new;
my $b = Bler->new;
$s->hi;
$b->hi;
Sner->new->hi;
Bler->new->hi;
1;
"#,
    );
    store.insert_workspace(path.clone(), fa);

    let sner_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "hi".to_string(),
            kind: TargetKind::Method {
                class: "Sner".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    let sner_lines: Vec<usize> = sner_results.iter().map(|r| r.span.start.row).collect();
    assert!(
        sner_lines.contains(&3),
        "Sner::hi decl missing: {:?}",
        sner_lines
    );
    assert!(
        sner_lines.contains(&11),
        "$s->hi (variable-bound) missing: {:?}",
        sner_lines
    );
    assert!(
        sner_lines.contains(&13),
        "Sner->new->hi (inline chain) missing — chain invocant resolution \
             via build-time invocant_class should cover this: {:?}",
        sner_lines
    );
    // No Bler anywhere in Sner results.
    assert!(
        !sner_lines.contains(&6),
        "Bler::hi decl wrongly in Sner results: {:?}",
        sner_lines
    );
    assert!(
        !sner_lines.contains(&12),
        "$b->hi wrongly in Sner results: {:?}",
        sner_lines
    );
    assert!(
        !sner_lines.contains(&14),
        "Bler->new->hi wrongly in Sner results: {:?}",
        sner_lines
    );

    let bler_results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "hi".to_string(),
            kind: TargetKind::Method {
                class: "Bler".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    let bler_lines: Vec<usize> = bler_results.iter().map(|r| r.span.start.row).collect();
    assert!(
        bler_lines.contains(&6),
        "Bler::hi decl missing: {:?}",
        bler_lines
    );
    assert!(bler_lines.contains(&12), "$b->hi missing: {:?}", bler_lines);
    assert!(
        bler_lines.contains(&14),
        "Bler->new->hi (inline chain) missing: {:?}",
        bler_lines
    );
    assert!(
        !bler_lines.contains(&3),
        "Sner::hi decl wrongly in Bler results: {:?}",
        bler_lines
    );
    assert!(
        !bler_lines.contains(&11),
        "$s->hi wrongly in Bler results: {:?}",
        bler_lines
    );
    assert!(
        !bler_lines.contains(&13),
        "Sner->new->hi wrongly in Bler results: {:?}",
        bler_lines
    );
}

/// ALL FOUR LSP paths (hover, gd, gr, rename) must class-scope
/// for two packages sharing a method name. Single resolver post
/// option-2 — `FileAnalysis::method_call_invocant_class(ref, idx)`
/// — but the four user-visible surfaces still each need their own
/// integration coverage:
///
///   1. find_definition (gd) — bag-routed via the helper, then
///      walks ancestors for cross-class fallback.
///   2. hover_info (K) — same helper; walks ancestors for the
///      "*from BaseClass*" provenance.
///   3. find_references / refs_to (gr) — iterates every MethodCall
///      ref and filters by helper-resolved invocant class.
///   4. rename_method_in_class — class-scopes edits to MethodCall
///      refs whose helper answer matches.
///
/// Classic copy-paste risk. This test drives all four LSP-visible
/// surfaces with a single Foo/Bar fixture and proves every path
/// stays on Foo when the cursor is on Foo, and on Bar when on
/// Bar. If any path drifts, its assertion fires.
#[test]
fn all_four_lsp_paths_class_scope_on_shared_method_name() {
    use crate::file_analysis::RenameKind;
    use tree_sitter::Parser;

    let src = r#"package Foo;
sub new { bless {}, shift }
sub run { "foo" }

package Bar;
sub new { bless {}, shift }
sub run { "bar" }

package main;
my $f = Foo->new;
my $b = Bar->new;
$f->run;
$b->run;
1;
"#;
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());

    // Line/col positions (0-indexed):
    //   line 2, col 4  — `sub run` decl in Foo
    //   line 6, col 4  — `sub run` decl in Bar
    //   line 11, col 4 — `$f->run` (cursor on "run"). Line text:
    //                    "$f->run;" → cols 0..1 = "$f", 2..3 = "->",
    //                    4..6 = "run".
    //   line 12, col 4 — `$b->run` (cursor on "run").
    let _run_in_foo_decl = tree_sitter::Point { row: 2, column: 4 };
    let _run_in_bar_decl = tree_sitter::Point { row: 6, column: 4 };
    let f_run_call = tree_sitter::Point { row: 11, column: 4 };
    let b_run_call = tree_sitter::Point { row: 12, column: 4 };

    // ---- (1) hover — cursor on `$f->run`. Must mention Foo, not Bar.
    let hover = fa
        .hover_info(f_run_call, src, None)
        .expect("hover on $f->run returns something");
    assert!(
        hover.contains("Foo"),
        "hover on $f->run should mention Foo; got: {:?}",
        hover
    );
    assert!(
        !hover.contains("Bar"),
        "hover on $f->run leaked Bar into the content: {:?}",
        hover
    );

    // Mirror: hover on $b->run mentions Bar, not Foo.
    let hover_b = fa
        .hover_info(b_run_call, src, None)
        .expect("hover on $b->run returns something");
    assert!(
        hover_b.contains("Bar"),
        "hover on $b->run should mention Bar; got: {:?}",
        hover_b
    );
    assert!(
        !hover_b.contains("Foo"),
        "hover on $b->run leaked Foo into the content: {:?}",
        hover_b
    );

    // ---- (2) goto-def — $f->run must jump to Foo::run (line 2), not Bar::run (line 6).
    let gd_f = fa
        .find_definition(f_run_call, None)
        .expect("gd on $f->run resolves");
    assert_eq!(
        gd_f.start.row, 2,
        "gd on $f->run jumped to line {} (expected 2 = Foo::run)",
        gd_f.start.row
    );

    let gd_b = fa
        .find_definition(b_run_call, None)
        .expect("gd on $b->run resolves");
    assert_eq!(
        gd_b.start.row, 6,
        "gd on $b->run jumped to line {} (expected 6 = Bar::run)",
        gd_b.start.row
    );

    // ---- (3) references — via rename_kind_at → TargetRef → refs_to.
    let target_from_f = match fa.rename_kind_at(f_run_call, None) {
        Some(RenameKind::Method { name, class }) => TargetRef {
            name,
            kind: TargetKind::Method { class },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        other => panic!(
            "rename_kind_at($f->run) should be Method{{class=Foo}}, got {:?}",
            other
        ),
    };
    // Verify the class field was populated as Foo (not Bar, not missing).
    if let TargetKind::Method { ref class } = target_from_f.kind {
        assert_eq!(
            class, "Foo",
            "rename_kind_at($f->run) resolved class as {:?}, expected Foo",
            class
        );
    }

    // ---- (4) rename — BEFORE moving `fa` into the store. Rename
    // Foo::run to renamed_run. Must edit Foo::run decl + $f->run
    // call, NOT Bar::run or $b->run.
    let edits = fa.rename_method_in_class("run", "Foo", "renamed_run", None);
    let edit_lines: Vec<usize> = edits.iter().map(|(span, _)| span.start.row).collect();
    assert!(
        edit_lines.contains(&2),
        "rename Foo::run missed the decl: {:?}",
        edit_lines
    );
    assert!(
        edit_lines.contains(&11),
        "rename Foo::run missed the $f->run call: {:?}",
        edit_lines
    );
    assert!(
        !edit_lines.contains(&6),
        "rename Foo::run wrongly rewrote Bar::run decl: {:?}",
        edit_lines
    );
    assert!(
        !edit_lines.contains(&12),
        "rename Foo::run wrongly rewrote $b->run call: {:?}",
        edit_lines
    );

    // Move `fa` into the store for the refs_to walk.
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/lsp_paths_fixture.pm");
    store.insert_workspace(path.clone(), fa);

    let foo_refs = refs_to(&store, None, &target_from_f, RoleMask::EDITABLE);
    let foo_lines: Vec<usize> = foo_refs.iter().map(|r| r.span.start.row).collect();
    assert!(
        foo_lines.contains(&2), // Foo::run decl
        "gr(Foo::run) missed decl: {:?}",
        foo_lines
    );
    assert!(
        foo_lines.contains(&11), // $f->run call
        "gr(Foo::run) missed $f->run call: {:?}",
        foo_lines
    );
    assert!(
        !foo_lines.contains(&6), // Bar::run decl
        "gr(Foo::run) wrongly included Bar::run decl: {:?}",
        foo_lines
    );
    assert!(
        !foo_lines.contains(&12), // $b->run call
        "gr(Foo::run) wrongly included $b->run call: {:?}",
        foo_lines
    );
}

/// Adversarial: two plain packages each declare `sub hi` and
/// `@EXPORT_OK = qw/hi/`. The caller does `use Sner;` (which
/// imports NOTHING — no @EXPORT, only @EXPORT_OK) and
/// `use Bler qw/hi/` (explicit import). The bare `hi()` call
/// therefore resolves to Bler's, not Sner's.
///
/// All four LSP paths must stay on the imported one (Bler):
///   * hover on `hi()` mentions Bler, not Sner.
///   * gd on `hi()` jumps to `sub hi` in Bler.
///   * gr on `hi()` includes the call + Bler::hi decl only.
///   * rename rewrites Bler::hi + the call, NOT Sner::hi.
///
/// Currently RED. The cross-class fix I landed only covered
/// MethodCall refs; FunctionCall refs are still name-only, and
/// the five different resolvers all collapse same-named subs
/// across packages.
///
/// The structural fix: `RefKind::FunctionCall` needs
/// `resolved_package: Option<String>`, populated at build time
/// by consulting `Imports`. Every resolver (find_definition,
/// hover_info, rename_kind_at, refs_to, rename_sub) keys off
/// that field. Same shape as `invocant_class` on MethodCall.
#[test]
fn sub_refs_respect_import_graph_not_just_name() {
    use crate::file_analysis::RenameKind;
    use tree_sitter::Parser;

    // Note: tree-sitter-perl parses bare `hi;` (no parens) as a
    // plain bareword, not a function call — so we write `hi();`
    // here to get a real `function_call_expression`. The
    // import-graph concern this test exercises is independent
    // of the parens question.
    //
    // Three packages, each with `sub hi`:
    //   - Sner — has @EXPORT_OK=qw/hi/, `use Sner;` (no import list
    //     → imports nothing, only @EXPORT would auto-import).
    //   - Bler — has @EXPORT_OK=qw/hi/, `use Bler qw/hi/` imports
    //     `hi` explicitly. This is the one our `hi()` call hits.
    //   - Xler — has `sub hi` too but no @EXPORT/@EXPORT_OK and
    //     never `use`d. It's just *there* in the file, its `hi`
    //     is unreachable to the main call. Double-pin: name-only
    //     resolution would union all three; correct resolution
    //     ignores Xler entirely.
    let src = r#"package Sner {
    our @EXPORT_OK = qw/hi/;
    sub hi {}
}
package Bler {
    our @EXPORT_OK = qw/hi/;
    sub hi {}
}
package Xler {
    sub hi {}
}

use Sner;
use Bler qw/hi/;

hi();
"#;
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());

    // Positions (0-indexed rows; blank lines count):
    //   row 2  — `    sub hi {}` in Sner
    //   row 6  — `    sub hi {}` in Bler
    //   row 9  — `    sub hi {}` in Xler (unreachable — never imported)
    //   row 15 — `hi()` call
    let hi_call = tree_sitter::Point { row: 15, column: 0 };

    let kind = fa.rename_kind_at(hi_call, None);
    let hover = fa.hover_info(hi_call, src, None);
    let gd = fa.find_definition(hi_call, None);

    // Rename kind — for gr/rename construction.
    let target = match kind.as_ref() {
        Some(RenameKind::Function { name, package }) => TargetRef {
            name: name.clone(),
            kind: TargetKind::Sub {
                package: package.clone(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        Some(RenameKind::Method { name, class }) => TargetRef {
            name: name.clone(),
            kind: TargetKind::Method {
                class: class.clone(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        other => panic!("unexpected rename_kind_at = {:?}", other),
    };

    let rename_edits = match &kind {
        Some(RenameKind::Function { name, package }) => {
            fa.rename_sub_in_package(name, package, "renamed_hi", None)
        }
        Some(RenameKind::Method { name, class }) => {
            fa.rename_method_in_class(name, class, "renamed_hi", None)
        }
        _ => Vec::new(),
    };

    let store = FileStore::new();
    let path = PathBuf::from("/tmp/sub_import_fixture.pm");
    store.insert_workspace(path.clone(), fa);
    let refs_result = refs_to(&store, None, &target, RoleMask::EDITABLE);

    // ---- Collect all findings ----
    let mut failures: Vec<String> = Vec::new();

    // (1) hover: must mention Bler (imported source), not Sner or Xler.
    match &hover {
        Some(h) if h.contains("Bler") && !h.contains("Sner") && !h.contains("Xler") => { /* ok */ }
        Some(h) => failures.push(format!(
            "hover on `hi()` is ambiguous or names the wrong package; got: {:?} \
                 (should mention Bler; must not mention Sner or Xler)",
            h
        )),
        None => failures.push("hover on `hi()` returned None".into()),
    }

    // (2) gd: must jump to Bler::hi (row 6). Not Sner::hi (row 2), not Xler::hi (row 9).
    match gd {
        Some(s) if s.start.row == 6 => { /* ok */ }
        Some(s) if s.start.row == 2 => failures.push(format!(
            "gd jumped to Sner::hi (row 2) — \
                    should follow imports to Bler::hi (row 6)"
        )),
        Some(s) if s.start.row == 9 => failures.push(format!(
            "gd jumped to Xler::hi (row 9) — \
                    Xler isn't imported, it should be ignored"
        )),
        Some(s) => failures.push(format!(
            "gd jumped to row {} — expected row 6 (Bler::hi)",
            s.start.row
        )),
        None => failures.push("gd returned None".into()),
    }

    // (3) gr: should include Bler::hi decl (row 6) + `hi()` call (row 15).
    // Must NOT include Sner::hi decl (row 2) or Xler::hi decl (row 9).
    let ref_rows: Vec<usize> = refs_result.iter().map(|r| r.span.start.row).collect();
    if !ref_rows.contains(&15) {
        failures.push(format!("gr missed the hi() call at row 15: {:?}", ref_rows));
    }
    if !ref_rows.contains(&6) {
        failures.push(format!("gr missed Bler::hi decl at row 6: {:?}", ref_rows));
    }
    if ref_rows.contains(&2) {
        failures.push(format!(
            "gr wrongly unioned Sner::hi decl at row 2: {:?}",
            ref_rows
        ));
    }
    if ref_rows.contains(&9) {
        failures.push(format!(
            "gr wrongly unioned Xler::hi decl at row 9: {:?}",
            ref_rows
        ));
    }

    // (4) rename: should rewrite Bler::hi + hi() call; NOT Sner::hi or Xler::hi.
    let rename_rows: Vec<usize> = rename_edits.iter().map(|(s, _)| s.start.row).collect();
    if !rename_rows.contains(&15) {
        failures.push(format!(
            "rename missed hi() call at row 15: {:?}",
            rename_rows
        ));
    }
    if !rename_rows.contains(&6) {
        failures.push(format!(
            "rename missed Bler::hi decl at row 6: {:?}",
            rename_rows
        ));
    }
    if rename_rows.contains(&2) {
        failures.push(format!(
            "rename wrongly rewrote Sner::hi decl at row 2: {:?}",
            rename_rows
        ));
    }
    if rename_rows.contains(&9) {
        failures.push(format!(
            "rename wrongly rewrote Xler::hi decl at row 9: {:?}",
            rename_rows
        ));
    }

    assert!(
        failures.is_empty(),
        "import-graph-aware resolution broken across paths:\n  - {}",
        failures.join("\n  - "),
    );
}

/// Adversarial: documentHighlight (the in-editor "highlight all
/// references of this identifier" feature) must respect the
/// same class/package scoping as gr and rename. The mojo demo
/// shape — a helper `users.create` and a route with
/// `action => 'create'` — has two distinct `create` symbols in
/// one file. Cursor on one must NOT highlight the other.
#[test]
fn document_highlight_respects_scope_on_shared_method_name() {
    use tree_sitter::Parser;

    let src = r#"
package MyApp;
use Mojolicious::Lite;
use Mojolicious;

my $app = Mojolicious->new;
$app->helper('users.create' => sub ($c, $name, $email) {});

my $r = app->routes;
$r->post('/users')->to(controller => 'Users', action => 'create');
"#;
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());

    // Lines (0-indexed; file starts with a blank row 0):
    //   row 6 — `$app->helper('users.create' => sub ...);`
    //   row 9 — `$r->post('/users')->to(controller => 'Users', action => 'create');`
    let line_helper = 6;
    let line_route = 9;

    let src_line = |n: usize| src.lines().nth(n).unwrap_or("");
    let helper_col = src_line(line_helper)
        .find("create")
        .expect("'create' on helper line");
    let route_col = src_line(line_route)
        .rfind("create")
        .expect("'create' on route line");

    // documentHighlight from the HELPER side: cursor inside
    // `users.create`. Must light up only helper-related spans,
    // NOT the route's `create` string at row 8.
    let helper_pt = tree_sitter::Point {
        row: line_helper,
        column: helper_col + 2,
    };
    let helper_highlights = fa.find_highlights(helper_pt, None);
    let helper_rows: Vec<usize> = helper_highlights.iter().map(|(s, _)| s.start.row).collect();
    assert!(
        helper_rows.contains(&line_helper),
        "helper highlight missed its own site: {:?}",
        helper_rows
    );
    assert!(
        !helper_rows.contains(&line_route),
        "helper highlight leaked into the route line — class-scoping broken: {:?}",
        helper_rows
    );

    // Mirror: documentHighlight from the ROUTE side. Cursor on
    // 'create' in `action => 'create'`. Must light up the route
    // ref only, NOT the helper's `create` site at row 5.
    let route_pt = tree_sitter::Point {
        row: line_route,
        column: route_col + 2,
    };
    let route_highlights = fa.find_highlights(route_pt, None);
    let route_rows: Vec<usize> = route_highlights.iter().map(|(s, _)| s.start.row).collect();
    assert!(
        route_rows.contains(&line_route),
        "route highlight missed its own site: {:?}",
        route_rows
    );
    assert!(
        !route_rows.contains(&line_helper),
        "route highlight leaked into the helper line — class-scoping broken: {:?}",
        route_rows
    );
}

/// Cross-file: gr on a `Users::create` method ref in one file
/// must find the definition in another workspace file AND every
/// call site across files — without cross-linking same-named
/// methods on unrelated classes.
#[test]
fn references_cross_file_method_respects_class_scope() {
    use crate::file_analysis::RenameKind;
    use tree_sitter::Parser;

    let store = FileStore::new();
    let parse_build = |source: &str| {
        let mut parser = Parser::new();
        parser
            .set_language(&ts_parser_perl::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        crate::builder::build(&tree, source.as_bytes())
    };

    // File 1: the route declaration.
    let f1_src = r#"
package MyApp;
use Mojolicious::Lite;

$app->helper('users.create' => sub ($c, $name) {});

my $r = app->routes;
$r->post('/users')->to(controller => 'Users', action => 'create');
"#;
    let f1 = parse_build(f1_src);
    store.insert_workspace(PathBuf::from("/tmp/app.pm"), f1);

    // File 2: Users controller with `sub create`.
    let f2_src = r#"
package Users;
sub create {
    my ($self, %args) = @_;
    return { ok => 1 };
}

sub list { }
1;
"#;
    let f2 = parse_build(f2_src);
    store.insert_workspace(PathBuf::from("/tmp/users.pm"), f2);

    // File 3: another caller that creates Users and invokes create.
    let f3_src = r#"
package Consumer;
use Users;
my $u = Users->new;
$u->create(name => 'alice');
1;
"#;
    let f3 = parse_build(f3_src);
    store.insert_workspace(PathBuf::from("/tmp/consumer.pm"), f3);

    // Probe: cursor on the route's 'create' action string in f1.
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let f1_tree = parser.parse(f1_src, None).unwrap();
    let f1_fa = crate::builder::build(&f1_tree, f1_src.as_bytes());
    // Row 7: `$r->post('/users')->to(controller => 'Users', action => 'create');`
    let route_row = 7usize;
    let line_route = f1_src.lines().nth(route_row).unwrap_or("");
    let create_col = line_route.rfind("create").expect("'create' on route line");
    let cursor = tree_sitter::Point {
        row: route_row,
        column: create_col + 2,
    };

    let kind = f1_fa.rename_kind_at(cursor, None);
    let target = match kind {
        Some(RenameKind::Method { name, class }) => TargetRef {
            name,
            kind: TargetKind::Method { class },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        other => panic!("expected Method, got {:?}", other),
    };
    // class must be "Users" — from the plugin's emitted invocant_class.
    if let TargetKind::Method { ref class } = target.kind {
        assert_eq!(class, "Users", "target class should be Users");
    }

    let refs = refs_to(&store, None, &target, RoleMask::EDITABLE);

    // Collect (file-basename, row) for clarity.
    let hits: Vec<(String, usize)> = refs
        .iter()
        .map(|r| {
            let fname = match &r.key {
                FileKey::Path(p) => p.file_name().unwrap().to_str().unwrap().to_string(),
                FileKey::Url(u) => u.to_string(),
            };
            (fname, r.span.start.row)
        })
        .collect();

    // Expected:
    //   users.pm     — `sub create` decl (Users class)
    //   consumer.pm  — `$u->create(...)` call
    //   app.pm       — the route's 'create' ref at row 7
    let has_users_decl = hits.iter().any(|(f, _)| f == "users.pm");
    let has_consumer_call = hits.iter().any(|(f, _)| f == "consumer.pm");
    let has_route_ref = hits.iter().any(|(f, r)| f == "app.pm" && *r == route_row);

    assert!(
        has_users_decl,
        "gr missed Users::create decl (users.pm row 2): {:?}",
        hits
    );
    assert!(
        has_consumer_call,
        "gr missed $u->create caller (consumer.pm row 4): {:?}",
        hits
    );
    assert!(has_route_ref, "gr missed route ref in app.pm: {:?}", hits);

    // Must NOT include the helper's `users.create` in app.pm —
    // that's on a different class.
    let helper_row = f1_src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$app->helper"))
        .map(|(i, _)| i)
        .unwrap();
    let leaked_helper = hits.iter().any(|(f, r)| f == "app.pm" && *r == helper_row);
    assert!(
        !leaked_helper,
        "gr wrongly included the helper on a different class at row {}: {:?}",
        helper_row, hits
    );
}

#[test]
fn test_refs_to_empty_when_no_hits() {
    let store = FileStore::new();
    let fa = parse("package Only;\nsub bar { 1 }\n1;\n");
    store.insert_workspace(PathBuf::from("/tmp/resolve_no_hits.pm"), fa);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "nonexistent".to_string(),
            kind: TargetKind::Sub { package: None },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(results.is_empty());
}

/// Phase 5: hash key references are emitted with owner resolved at build
/// time when the binding is local. refs_to then matches both the def and
/// the access in the same file via the HashKeyOfSub target.
///
/// Cross-file HashKeyAccess owners are currently resolved through the
/// enrichment path (which injects synthetic HashKeyDefs in the consumer
/// file); this test covers the same-file case that lands purely via phase
/// 5 build-time linking. A follow-up (enrichment rebuild of
/// refs_by_target) will close the cross-file consumer-access case.
#[test]
fn test_refs_to_finds_hash_key_def_and_access_same_file() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/resolve_hash_same.pm");

    let fa = parse(
            "package Lib;\nsub get_config { return { host => 1, port => 2 } }\nmy $cfg = get_config();\nmy $h = $cfg->{host};\n1;\n",
        );
    store.insert_workspace(path.clone(), fa);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "host".to_string(),
            kind: TargetKind::HashKeyOfSub {
                package: Some("Lib".to_string()),
                name: "get_config".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    // Expect at least the HashKeyDef decl AND the HashKeyAccess (both in
    // the same file). Previously, the access required a tree argument to
    // resolve its owner — now it's linked at build time.
    let has_decl = results.iter().any(|r| r.access == AccessKind::Declaration);
    let has_access = results.iter().any(|r| r.access == AccessKind::Read);
    assert!(has_decl, "expected HashKeyDef decl, got {:?}", results);
    assert!(has_access, "expected HashKeyAccess, got {:?}", results);
}

/// Cross-file: the HashKeyDef is discoverable in a different file even
/// when the consumer's access site hasn't been enriched yet. This is a
/// partial cross-file fix — consumer-side access resolution still needs
/// enrichment to run in the consumer's FileAnalysis.
#[test]
fn test_refs_to_finds_cross_file_hash_key_def() {
    let store = FileStore::new();
    let path_lib = PathBuf::from("/tmp/resolve_hash_cross_lib.pm");

    let fa_lib = parse("package Lib;\nsub get_config { return { host => 1, port => 2 } }\n1;\n");
    store.insert_workspace(path_lib.clone(), fa_lib);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "host".to_string(),
            kind: TargetKind::HashKeyOfSub {
                package: Some("Lib".to_string()),
                name: "get_config".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_lib)),
        "expected HashKeyDef match in Lib, got {:?}",
        results,
    );
}

/// THE bug the user caught: two different packages each with `sub X`
/// returning `{ key => ... }` must not cross-pollute. Before the
/// package-qualified `HashKeyOwner::Sub`, every file's `host` key would
/// show up regardless of which `get_config` defined it. Now only the
/// matching package's def does.
#[test]
fn test_refs_to_package_qualified_sub_owner_isolates_name_collisions() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_pkg_a.pm");
    let path_b = PathBuf::from("/tmp/resolve_pkg_b.pm");

    // Two different packages, same sub name, same key name.
    let fa_a = parse("package Alpha;\nsub get_config { return { host => 'alpha' } }\n1;\n");
    store.insert_workspace(path_a.clone(), fa_a);

    let fa_b = parse("package Beta;\nsub get_config { return { host => 'beta' } }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    // Query for `host` in Alpha's get_config — must NOT match Beta's.
    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "host".to_string(),
            kind: TargetKind::HashKeyOfSub {
                package: Some("Alpha".to_string()),
                name: "get_config".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)),
        "expected Alpha hit, got {:?}",
        results,
    );
    assert!(
        !results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)),
        "Beta's get_config must NOT show up in Alpha's references, got {:?}",
        results,
    );
}

/// Fully-qualified call (`A::foo()`) references must reach the def in
/// package A, even though the call's `target_name` is the whole path
/// `A::foo` while the symbol is keyed by the bare name `foo`. The
/// `resolved_package` qualifier pins package A.
#[test]
fn test_refs_to_qualified_call_resolves_to_def() {
    let store = FileStore::new();
    let path_a = PathBuf::from("/tmp/resolve_qual_a.pm");
    let path_b = PathBuf::from("/tmp/resolve_qual_b.pm");

    let fa_a = parse("package A;\nsub foo { 42 }\n1;\n");
    store.insert_workspace(path_a.clone(), fa_a);

    // No import — the qualifier names the package directly.
    let fa_b = parse("package B;\nsub bar { A::foo(); A::foo() }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub {
                package: Some("A".to_string()),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    // Decl in A.
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)),
        "expected the def in A, got {:?}",
        results,
    );
    // Both qualified call sites in B.
    let b_hits = results
        .iter()
        .filter(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b))
        .count();
    assert_eq!(b_hits, 2, "expected both A::foo() call sites, got {:?}", results);
}

/// A qualified call to package A's `foo` must not match a same-named
/// `foo` in package C — `resolved_package` isolates the qualifier.
#[test]
fn test_refs_to_qualified_call_isolates_package() {
    let store = FileStore::new();
    let path_b = PathBuf::from("/tmp/resolve_qual_iso_b.pm");

    let fa_b = parse("package B;\nsub bar { A::foo(); C::foo() }\n1;\n");
    store.insert_workspace(path_b.clone(), fa_b);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub {
                package: Some("A".to_string()),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    // Only the A::foo() site, never C::foo().
    let b_hits = results
        .iter()
        .filter(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b))
        .count();
    assert_eq!(b_hits, 1, "only A::foo() should match, got {:?}", results);
}

#[test]
fn test_refs_to_role_mask_excludes_workspace() {
    let store = FileStore::new();
    let fa = parse("package P;\nsub foo {}\n1;\n");
    store.insert_workspace(PathBuf::from("/tmp/masked.pm"), fa);

    // OPEN-only mask should miss this workspace file.
    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub { package: None },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::OPEN,
    );
    assert!(results.is_empty());
}

/// Red-pin: cross-file `invocant_class` is set once at build time and
/// never refreshed. When a consumer file's `$b` invocant only becomes
/// typeable post-enrichment (because the producer's return type lives
/// in another file), the consumer's `$b->method()` MethodCall ref keeps
/// `invocant_class: None` forever — the bag learns the type but the
/// ref field doesn't get re-derived. `refs_to`'s
/// `(invocant_class, scope) match (Some(cn), Some(pkg)) => cn == pkg`
/// filter (resolve.rs:256-258) excludes unresolved invocants, so the
/// cross-file call site silently doesn't show up in references for the
/// target method.
///
/// Reproduces with two files:
///   - Producer B: exports `make_b`, which returns `bless {}, 'B'`.
///     `make_b`'s return type IS resolvable at build time (closed
///     under syntax) → `Symbol(make_b) → ClassName('B')` lands in B's
///     bag.
///   - Consumer A: `use B qw(make_b); my $b = make_b(); $b->touch();`.
///     At build time of A the imported sub's return type isn't
///     visible → `$b` untyped → `$b->touch()` ref's `invocant_class`
///     stays None.
///   - `enrich_imported_types_with_keys` on A does push a Variable
///     witness for `$b: ClassName('B')` (so `inferred_type_via_bag`
///     answers correctly) but does NOT re-fill the MethodCall ref's
///     `invocant_class`.
///   - `refs_to` for `B::touch` then misses A's call site.
///
/// Fix surface: either invalidate-and-rebuild the consumer when
/// upstream types arrive (heavy), or post-enrichment re-run
/// `apply_chain_typing_invocants` (or its FA-side equivalent) so refs
/// see the now-resolvable types. A query-time materializer that
/// resolves invocant from the bag on every refs_to read would also
/// work and avoids the cache-invalidation question.
#[test]
fn references_cross_file_invocant_resolved_post_enrichment() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let producer_src = r#"
package B;
use Exporter 'import';
our @EXPORT_OK = qw(make_b);

sub new    { return bless {}, shift }
sub make_b { return B->new }
sub touch  { 1 }
1;
"#;
    let consumer_src = r#"
use B qw(make_b);
my $b = make_b();
$b->touch();
1;
"#;

    let producer_path = PathBuf::from("/tmp/refs_xfile_b.pm");
    let consumer_path = PathBuf::from("/tmp/refs_xfile_a.pm");

    // Module index sees the producer so enrichment can resolve
    // `make_b`'s return type via cross-file scan.
    let idx = ModuleIndex::new_for_test();
    let producer_fa = parse(producer_src);
    idx.register_workspace_module(producer_path.clone(), Arc::new(producer_fa));

    // Build the consumer and enrich it. Post-enrichment, the bag
    // should know `$b: ClassName('B')` — verify that invariant first
    // so the test fails for the right reason.
    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));
    let b_type = consumer_fa.inferred_type_via_bag(
        "$b",
        tree_sitter::Point { row: 4, column: 0 },
    );
    assert_eq!(
        b_type.as_ref().and_then(|t| t.class_name()),
        Some("B"),
        "precondition: enrichment must type $$b as B; got {:?}",
        b_type,
    );

    // Build a FileStore over both files. `refs_to` reads
    // `invocant_class` directly off the consumer's MethodCall ref —
    // that field was populated at consumer-build time when B's types
    // weren't yet known.
    let store = FileStore::new();
    store.insert_workspace(producer_path, parse(producer_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);

    let target = TargetRef {
        name: "touch".to_string(),
        kind: TargetKind::Method { class: "B".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let refs = refs_to(&store, Some(&idx), &target, RoleMask::WORKSPACE);
    let consumer_hit = refs.iter().any(|r| {
        matches!(&r.key, FileKey::Path(p) if p == &consumer_path)
    });
    assert!(
        consumer_hit,
        "refs_to(B::touch) missed consumer's $$b->touch() call site. \
         invocant_class on the MethodCall ref is None because the \
         consumer was built before B's return types were known, and \
         enrichment does not refresh ref fields. hits: {:?}",
        refs.iter().map(|r| (&r.key, r.span.start.row)).collect::<Vec<_>>(),
    );
}

/// Companion: `find_highlights`'s cross-file fallback path
/// must match both `$b->touch()` sites once enrichment has typed
/// `$b: B`. Cursor on the first call; the second has the same None
/// build-time-resolved invocant_class — both should highlight.
#[test]
fn find_highlights_cross_file_invocant_resolved_post_enrichment() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let producer_src = r#"
package B;
use Exporter 'import';
our @EXPORT_OK = qw(make_b);

sub new    { return bless {}, shift }
sub make_b { return B->new }
sub touch  { 1 }
1;
"#;
    let consumer_src = r#"
use B qw(make_b);
my $b = make_b();
$b->touch();
$b->touch();
1;
"#;

    let producer_path = PathBuf::from("/tmp/highlights_xfile_b.pm");

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(producer_path, Arc::new(parse(producer_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    // Cursor on the first $b->touch() — column 4 lands on `t` of touch.
    let highlights = consumer_fa.find_highlights(
        tree_sitter::Point { row: 3, column: 4 },
        Some(&idx));

    assert_eq!(
        highlights.len(),
        2,
        "find_highlights should match both $$b->touch() sites once \
         enrichment has typed $$b: B. got {:?}",
        highlights,
    );
}

/// Multi-hop: producer's exported sub returns a class that inherits
/// from another. The consumer's `$x->ancestor_method()` call site
/// resolves to the parent class only after enrichment + ancestor
/// walk. Confirms the bag-only path composes with cross-file
/// inheritance resolution.
#[test]
fn refs_to_cross_file_invocant_inherited_method() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let parent_src = r#"
package P;
sub new { return bless {}, shift }
sub ping { "pong" }
1;
"#;
    let child_src = r#"
package C;
use parent 'P';
use Exporter 'import';
our @EXPORT_OK = qw(make_c);
sub make_c { return C->new }
1;
"#;
    let consumer_src = r#"
use C qw(make_c);
my $x = make_c();
$x->ping();
1;
"#;

    let parent_path = PathBuf::from("/tmp/multihop_p.pm");
    let child_path = PathBuf::from("/tmp/multihop_c.pm");
    let consumer_path = PathBuf::from("/tmp/multihop_consumer.pm");

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(parent_path.clone(), Arc::new(parse(parent_src)));
    idx.register_workspace_module(child_path.clone(), Arc::new(parse(child_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    // Sanity: enrichment typed $x as C.
    let x_type = consumer_fa.inferred_type_via_bag(
        "$x",
        tree_sitter::Point { row: 4, column: 0 },
    );
    assert_eq!(
        x_type.as_ref().and_then(|t| t.class_name()),
        Some("C"),
        "precondition: enrichment must type $$x as C; got {:?}",
        x_type,
    );

    let store = FileStore::new();
    store.insert_workspace(parent_path, parse(parent_src));
    store.insert_workspace(child_path, parse(child_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);

    // Target the parent's `ping` — refs_to uses class-keyed Method
    // matching, so `$x->ping()` (whose nearest invocant class is C)
    // doesn't directly equal P. Targeting `C::ping` (the inherited
    // shape) is what matches today.
    let refs = refs_to(
        &store,
        Some(&idx),
        &TargetRef {
            name: "ping".to_string(),
            kind: TargetKind::Method { class: "C".to_string() },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::WORKSPACE,
    );
    assert!(
        refs.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &consumer_path)),
        "refs_to(C::ping) missed consumer's $$x->ping() call site \
         (multi-hop: $$x typed as C only post-enrichment). hits: {:?}",
        refs.iter().map(|r| (&r.key, r.span.start.row)).collect::<Vec<_>>(),
    );
}

/// Perf bench, gated on `--features perf_bench` so the default
/// `cargo test` never sees it (no `#[ignore]` count). Generates
/// a synthetic workspace with N files × M method-call sites and
/// measures `refs_to` wall time. Useful when changing the
/// invocant resolver — gut-check the cost. Run with:
///   cargo test --release --features perf_bench -- --nocapture \
///     bench_refs_to_invocant_resolution
#[cfg(feature = "perf_bench")]
#[test]
fn bench_refs_to_invocant_resolution() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    use std::time::Instant;

    const N_FILES: usize = 200;
    const M_CALLS: usize = 50;
    const R_REPS: usize = 50;

    // Producer: defines `B::touch` plus a constructor.
    let producer_src = r#"
package B;
use Exporter 'import';
our @EXPORT_OK = qw(make_b);

sub new    { return bless {}, shift }
sub make_b { return B->new }
sub touch  { 1 }
1;
"#;

    let store = FileStore::new();
    let idx = ModuleIndex::new_for_test();
    let producer_path = PathBuf::from("/tmp/bench_b.pm");
    idx.register_workspace_module(producer_path.clone(), Arc::new(parse(producer_src)));
    store.insert_workspace(producer_path, parse(producer_src));

    // N consumer files, each with M method-call sites that mix:
    //  - $b->touch() (var invocant, cross-file enrichment)
    //  - B->touch() (bareword invocant)
    //  - $self->touch() (enclosing-class invocant)
    //  - $b->touch()->touch() (chain invocant)
    //  - make_b()->touch() (function-call invocant)
    for f in 0..N_FILES {
        let mut src = String::from(
            "package Consumer;\nuse B qw(make_b);\nsub run {\n  my $self = shift;\n  my $b = make_b();\n",
        );
        for c in 0..M_CALLS {
            match c % 5 {
                0 => src.push_str("  $b->touch();\n"),
                1 => src.push_str("  B->touch();\n"),
                2 => src.push_str("  $self->touch();\n"),
                3 => src.push_str("  $b->touch()->touch();\n"),
                _ => src.push_str("  make_b()->touch();\n"),
            }
        }
        src.push_str("}\n1;\n");
        let path = PathBuf::from(format!("/tmp/bench_consumer_{}.pm", f));
        let mut fa = parse(&src);
        fa.enrich_imported_types_with_keys(Some(&idx));
        store.insert_workspace(path, fa);
    }

    let target = TargetRef {
        name: "touch".to_string(),
        kind: TargetKind::Method { class: "B".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };

    // Warm-up — JIT'd registry caches, lazy index allocs.
    let _ = refs_to(&store, Some(&idx), &target, RoleMask::WORKSPACE);

    let t0 = Instant::now();
    let mut total_hits: usize = 0;
    for _ in 0..R_REPS {
        let refs = refs_to(&store, Some(&idx), &target, RoleMask::WORKSPACE);
        total_hits += refs.len();
    }
    let elapsed = t0.elapsed();
    let per_call = elapsed / (R_REPS as u32);
    eprintln!(
        "BENCH refs_to: {} files × {} calls × {} reps = {} hits in {:?} ({:?}/call avg)",
        N_FILES, M_CALLS, R_REPS, total_hits, elapsed, per_call,
    );
}

/// Discriminator: a chain hop where the inner receiver's class is
/// only known via cross-file enrichment. Hybrid branch (cache + bag
/// fallback inside `invocant_class_of_method_call`) handles VARIABLE
/// invocants whose type comes from enrichment, but does NOT handle
/// CHAIN invocants (`$x->makeFoo()->ping()`) when the inner
/// `$x->makeFoo()` ref's invocant_class cache stayed None at build
/// time — the hybrid's bag fallback hits `resolve_invocant_class`
/// which doesn't know how to walk a chain at read time. Option 2's
/// helper recurses on the inner receiver via `call_ref_by_start`,
/// re-resolving fresh, so cross-file enrichment composes through
/// every chain hop.
///
/// Expected:
///   * Option 2 branch: passes (refs_to finds the consumer's chain hop).
///   * Hybrid branch: fails (chain hop's class stays unknown).
#[test]
fn refs_to_cross_file_chain_hop_post_enrichment() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // Producer P — defines a class returning itself (so makeFoo's return is P).
    let producer_src = r#"
package P;
use Exporter 'import';
our @EXPORT_OK = qw(makeP);

sub new     { return bless {}, shift }
sub makeP   { return P->new }
sub makeFoo { return P->new }
sub ping    { 1 }
1;
"#;
    // Consumer A — uses P, builds a chain whose inner receiver
    // ($x) is typed only by cross-file enrichment.
    let consumer_src = r#"
use P qw(makeP);
my $x = makeP();
$x->makeFoo()->ping();
1;
"#;

    let producer_path = PathBuf::from("/tmp/chain_xfile_p.pm");
    let consumer_path = PathBuf::from("/tmp/chain_xfile_consumer.pm");

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(producer_path.clone(), Arc::new(parse(producer_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    // Sanity: $x types as P via enrichment.
    let x_type = consumer_fa.inferred_type_via_bag(
        "$x",
        tree_sitter::Point { row: 4, column: 0 },
    );
    assert_eq!(
        x_type.as_ref().and_then(|t| t.class_name()),
        Some("P"),
        "precondition: enrichment must type $$x as P; got {:?}",
        x_type,
    );

    let store = FileStore::new();
    store.insert_workspace(producer_path, parse(producer_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);

    // refs_to(P::ping) — the consumer's `->ping()` is reachable
    // only by typing `$x->makeFoo()` (the chain hop's invocant)
    // through the cross-file P::makeFoo return type. Only works
    // if the helper resolves the chain hop fresh against the bag.
    let refs = refs_to(
        &store,
        Some(&idx),
        &TargetRef {
            name: "ping".to_string(),
            kind: TargetKind::Method { class: "P".to_string() },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::WORKSPACE,
    );
    assert!(
        refs.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &consumer_path)),
        "refs_to(P::ping) missed the chain hop $$x->makeFoo()->ping(). \
         The inner $$x->makeFoo() invocant ($$x) was typed only by \
         cross-file enrichment; chain typing must compose through the \
         bag at read time. hits: {:?}",
        refs.iter().map(|r| (&r.key, r.span.start.row)).collect::<Vec<_>>(),
    );
}

/// `find_highlights` chain-hop case: receiver class only known
/// via cross-file `MethodOnClass` resolution (`$x->makeFoo()->ping()`
/// — `makeFoo` returns a cross-file class). Was a red-pin until
/// the polish commit threaded `module_index` through
/// `find_highlights` / `collect_refs_for_target` /
/// `find_references` / `rename_kind_at` / `rename_callable_in_scope`,
/// matching `crate::resolve::refs_to`'s already-threaded shape.
#[test]
fn find_highlights_cross_file_chain_hop_post_enrichment() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let producer_src = r#"
package P;
use Exporter 'import';
our @EXPORT_OK = qw(makeP);

sub new     { return bless {}, shift }
sub makeP   { return P->new }
sub makeFoo { return P->new }
sub ping    { 1 }
1;
"#;
    let consumer_src = r#"
use P qw(makeP);
my $x = makeP();
$x->makeFoo()->ping();
$x->makeFoo()->ping();
1;
"#;

    let producer_path = PathBuf::from("/tmp/highlights_chain_p.pm");
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(producer_path, Arc::new(parse(producer_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    // Cursor on the first `->ping()` — column 18 lands on `p` of ping.
    let highlights = consumer_fa.find_highlights(
        tree_sitter::Point { row: 3, column: 18 },
        Some(&idx));

    // Both call sites of `->ping()` share the same chain receiver
    // shape; they must highlight together once chain typing
    // composes through the bag at read time.
    assert_eq!(
        highlights.len(),
        2,
        "find_highlights should match both $$x->makeFoo()->ping() sites \
         once chain typing through cross-file enrichment is threaded \
         via module_index. got {:?}",
        highlights,
    );
}

/// Minion task references fan out cross-file. `$minion->add_task('T' => sub)`
/// stamps a `Handler` owned by `Class("Minion")`; every dispatch site
/// (`enqueue`, and crm's tenant sugar) emits a `DispatchCall` ref carrying
/// the same `(name, owner)`. Pairing is owner+name only — the dispatcher
/// verb never enters the match — so `refs_to(TargetKind::Handler)` collects
/// the registration AND every caller across files. This is the resolve-side
/// contract the LSP `references` handler and the `--references` CLI both lean
/// on for "go to references on a minion task".
#[test]
fn refs_to_handler_fans_out_across_files() {
    let store = FileStore::new();
    let path_reg = PathBuf::from("/tmp/resolve_minion_reg.pm");
    let path_call = PathBuf::from("/tmp/resolve_minion_call.pm");

    // Registry file: add_task stamps the Handler (owner Class("Minion")).
    let fa_reg = parse(
        "package App::Tasks;\nuse Minion;\n\
sub setup ($minion) {\n  $minion->add_task('send_email' => sub ($job, $to) { 1 });\n}\n1;\n",
    );
    store.insert_workspace(path_reg.clone(), fa_reg);

    // Caller file: plain enqueue dispatch site → DispatchCall ref.
    let fa_call = parse(
        "package App::Caller;\nuse Minion;\n\
sub fire ($minion) {\n  $minion->enqueue('send_email' => ['a@b']);\n}\n1;\n",
    );
    store.insert_workspace(path_call.clone(), fa_call);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "send_email".to_string(),
            kind: TargetKind::Handler {
                owner: crate::file_analysis::HandlerOwner::Class("Minion".to_string()),
                name: "send_email".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_reg)),
        "expected the add_task registration in the registry file, got {:?}",
        results,
    );
    assert!(
        results
            .iter()
            .any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_call)),
        "expected the cross-file enqueue caller, got {:?}",
        results,
    );
}

/// The query-time `ReceiverGated` gap closer: a Handler `refs_to` must
/// include a dispatch call-site in a NON-OPEN workspace file whose receiver
/// `isa Minion` only CROSS-FILE and which does NOT `use Minion` itself. The
/// emit-hook path can't fire (no `use Minion` trigger) and the file is never
/// enriched (workspace, not open), so nothing materializes a `DispatchCall`
/// ref. Under enrichment-eager promotion this call site was invisible
/// (the previously-`#[ignore]`d gap); query-time resolution of the gated
/// candidate against the cross-file `My::Minion isa Minion` chain surfaces
/// it. See `docs/adr/receiver-gated-dispatch.md`.
#[test]
fn refs_to_handler_finds_dispatch_in_unenriched_cross_file_subclass() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // `My::Minion isa Minion`, established cross-file in the index.
    let minion_sub_path = PathBuf::from("/tmp/rg_my_minion.pm");
    let minion_sub_fa = parse("package My::Minion;\nuse parent 'Minion';\nsub new { bless {}, shift }\n1;\n");
    idx.register_workspace_module(minion_sub_path, Arc::new(minion_sub_fa));

    // Registry file (open/workspace): add_task stamps the Handler.
    let path_reg = PathBuf::from("/tmp/rg_minion_reg.pm");
    let fa_reg = parse(
        "package App::Tasks;\nuse Minion;\n\
sub setup ($minion) {\n  $minion->add_task('send_email' => sub ($job, $to) { 1 });\n}\n1;\n",
    );
    store.insert_workspace(path_reg.clone(), fa_reg);

    // Caller: receiver is a cross-file Minion SUBCLASS, and the file does
    // NOT `use Minion`. Built without enrichment (insert_workspace mirrors
    // the indexer). No materialized DispatchCall ref — must resolve lazily.
    let path_call = PathBuf::from("/tmp/rg_minion_call.pm");
    let fa_call = parse(
        "package App::Worker;\n\
sub fire {\n  my $self = shift;\n  my $minion = My::Minion->new;\n  $minion->enqueue('send_email' => ['a@b']);\n}\n1;\n",
    );
    // Sanity: no DispatchCall ref was materialized at build (query-time path
    // is the only thing that can surface this site).
    assert!(
        !fa_call.refs.iter().any(|r|
            matches!(&r.kind, crate::file_analysis::RefKind::DispatchCall { .. })),
        "precondition: the caller must have NO materialized DispatchCall ref",
    );
    store.insert_workspace(path_call.clone(), fa_call);

    let results = refs_to(
        &store,
        Some(&idx),
        &TargetRef {
            name: "send_email".to_string(),
            kind: TargetKind::Handler {
                owner: crate::file_analysis::HandlerOwner::Class("Minion".to_string()),
                name: "send_email".to_string(),
            },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_reg)),
        "expected the add_task registration; got {:?}",
        results,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_call)),
        "expected the cross-file enqueue call-site in the unenriched subclass \
         worker to surface via query-time gated resolution; got {:?}",
        results,
    );
}

/// A package that declares its exports via a runtime exporter setup
/// (Sub::Exporter) must let cross-file `refs_to` fan the defining sub
/// out to a consumer's `use X qw/name/` import and call site — the
/// same as a `@EXPORT_OK` sub. Models the export so the import resolves.
#[test]
fn refs_to_fans_runtime_exported_sub_to_consumer() {
    let store = FileStore::new();
    let path_def = PathBuf::from("/tmp/runtime_export_def.pm");
    let path_use = PathBuf::from("/tmp/runtime_export_use.pm");

    // Exporting package: names come from Sub::Exporter, not @EXPORT_OK.
    let fa_def = parse(
        "package Sugar::Sub;\n\
         use Sub::Exporter -setup => { exports => [qw/sweeten/] };\n\
         sub sweeten { 42 }\n1;\n",
    );
    store.insert_workspace(path_def.clone(), fa_def);

    // Consumer imports and calls it.
    let fa_use = parse(
        "package Consumer;\n\
         use Sugar::Sub qw/sweeten/;\n\
         sub run { sweeten(); }\n1;\n",
    );
    store.insert_workspace(path_use.clone(), fa_use);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "sweeten".to_string(),
            kind: TargetKind::Sub { package: Some("Sugar::Sub".to_string()) },
            method_classes: Vec::new(), scope: OverrideScope::Dispatch,
        },
        RoleMask::EDITABLE,
    );

    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_def)
            && r.access == AccessKind::Declaration),
        "expected declaration of sweeten in the exporting package; got {:?}",
        results,
    );
    assert!(
        results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_use)),
        "expected the consumer's import/call of sweeten to fan out; got {:?}",
        results,
    );
}

/// Bare `use Bank;` auto-imports `@EXPORT` — a fact the single-file walk
/// can't see, so the consumer's `make_account()` call is left
/// `resolved_package: None`. Both rename directions must still link source
/// and consumer: `refs_to` defers the call's package resolution to the
/// index at query time (`deferred_call_package`), the same lazy seam method
/// dispatch + deferred hash-key owners use. (The explicit-`qw/.../` case
/// already pins at build time — this is the implicit-export gap.)
#[test]
fn refs_to_links_implicit_export_to_bare_use_consumer() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let def = PathBuf::from("/tmp/impl_export_def.pm");
    let consumer = PathBuf::from("/tmp/impl_export_use.pl");

    let def_src = "package Bank;\n\
         use Exporter 'import';\n\
         our @EXPORT = ('make_account');\n\
         sub make_account { 1 }\n1;\n";
    // Bare `use Bank;` — no import list; `make_account` arrives via @EXPORT.
    let use_src = "use Bank;\nmy $a = make_account('Ada');\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(def.clone(), Arc::new(parse(def_src)));

    store.insert_workspace(def.clone(), parse(def_src));
    store.insert_workspace(consumer.clone(), parse(use_src));

    let target = TargetRef {
        name: "make_account".to_string(),
        kind: TargetKind::Sub { package: Some("Bank".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let refs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));

    assert!(hit(&def), "missed Bank::make_account decl. hits: {:?}", refs);
    assert!(
        hit(&consumer),
        "missed bare-`use`-consumer's make_account() call (implicit @EXPORT). hits: {:?}",
        refs,
    );
}

/// Cross-file return-hash key rename: a sub's `return { host => … }` key,
/// consumed in another file as `$c->{host}` where `$c = imported_sub()`.
/// Both directions must reach the producer's real def + the consumer access:
///   * owner identity is unified (producer package stamped on the consumer
///     access — `Sub{Some("Cfg"), get_config}` both sides);
///   * the consumer access in an UNENRICHED workspace file (owner `Variable`)
///     re-derives cross-file via `deferred_hash_key_owner`'s function-binding
///     arm, so a producer-origin rename doesn't depend on open-doc enrichment;
///   * no synthetic stub exists, so no phantom `0:0` decl appears.
#[test]
fn refs_to_links_return_hash_key_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let prod = PathBuf::from("/tmp/rk_cfg.pm");
    let cons = PathBuf::from("/tmp/rk_consumer.pl");

    let prod_src = "package Cfg;\n\
         use Exporter 'import';\n\
         our @EXPORT = ('get_config');\n\
         sub get_config { return { host => 'h', port => 1 }; }\n1;\n";
    let cons_src = "use Cfg;\nmy $c = get_config();\nmy $h = $c->{host};\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(prod.clone(), Arc::new(parse(prod_src)));
    store.insert_workspace(prod.clone(), parse(prod_src));
    // Consumer stays UNENRICHED in the store (a workspace file), exercising the
    // deferred owner re-derivation rather than the eager enrichment fixup.
    store.insert_workspace(cons.clone(), parse(cons_src));

    let target = TargetRef {
        name: "host".to_string(),
        kind: TargetKind::HashKeyOfSub {
            package: Some("Cfg".to_string()),
            name: "get_config".to_string(),
        },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    assert!(target.supports_cross_file_rename(), "HashKeyOfSub must rename cross-file now");

    let refs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));
    assert!(hit(&prod), "missed producer's host def. hits: {:?}", refs);
    assert!(
        hit(&cons),
        "missed consumer's $c->{{host}} access (deferred cross-file owner). hits: {:?}",
        refs,
    );
    // The unified model has no zero-span synthetic stub → no phantom decl.
    assert!(
        !refs.iter().any(|r| r.span.start.row == 0 && r.span.start.column == 0),
        "phantom 0:0 edit must not appear. hits: {:?}",
        refs,
    );
    // `port` (sibling key, same sub) must stay out.
    assert_eq!(refs.len(), 2, "exactly producer def + consumer access. hits: {:?}", refs);
}

/// Cross-file plain-sub references: an exported sub defined in one
/// workspace file, imported and called from two others, plus an
/// unrelated same-named sub in a *different* package that must NOT
/// be collected. Mirrors the crm `info_to_task` case (def in
/// TaskInfo, callers in TaskProxy + tests). The def-site query is the
/// one the hand-rolled CLI walk used to miss — here exercised through
/// `refs_to`, the single path both backend and CLI now share.
#[test]
fn references_cross_file_sub_fans_out_and_stays_package_scoped() {
    let store = FileStore::new();
    let def = PathBuf::from("/tmp/xsub_def.pm");
    let caller1 = PathBuf::from("/tmp/xsub_c1.pm");
    let caller2 = PathBuf::from("/tmp/xsub_c2.pm");
    let decoy = PathBuf::from("/tmp/xsub_decoy.pm");

    store.insert_workspace(
        def.clone(),
        parse("package TaskInfo;\nuse Exporter 'import';\nour @EXPORT_OK = qw/info_to_task/;\nsub info_to_task { 1 }\n1;\n"),
    );
    store.insert_workspace(
        caller1.clone(),
        parse("package TaskProxy;\nuse TaskInfo qw/info_to_task/;\nsub run { info_to_task(); }\n1;\n"),
    );
    store.insert_workspace(
        caller2.clone(),
        parse("use TaskInfo qw/info_to_task/;\ninfo_to_task();\n1;\n"),
    );
    // Decoy: a *different* package with a same-named sub, never imported.
    store.insert_workspace(
        decoy.clone(),
        parse("package Other;\nsub info_to_task { 99 }\nsub use_it { info_to_task(); }\n1;\n"),
    );

    let target = TargetRef {
        name: "info_to_task".to_string(),
        kind: TargetKind::Sub { package: Some("TaskInfo".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let refs = refs_to(&store, None, &target, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));

    assert!(hit(&def), "missed TaskInfo def. hits: {:?}", refs);
    assert!(hit(&caller1), "missed TaskProxy caller. hits: {:?}", refs);
    assert!(hit(&caller2), "missed top-level caller. hits: {:?}", refs);
    assert!(
        !hit(&decoy),
        "cross-linked Other::info_to_task (unrelated package). hits: {:?}",
        refs,
    );
}

/// Cross-file method references via inheritance: a method defined on
/// a parent/role, called on a child instance (`$child->m()`) in
/// another file, must surface when the *parent* class is the target.
/// This is the crm role case (`Clove::Role::REST::success` called as
/// `$c->success` in every controller that `with`s the role) and the
/// `todays_rate`/`add_data` shape generally. The matcher uses
/// `method_rename_chain`, so the parent is on the invocant's
/// resolution chain; an unrelated class sharing the method name is
/// not, and stays out.
#[test]
fn references_cross_file_method_matches_inheriting_invocant() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // Role/parent defines `success`; child consumes it via `use parent`.
    let role_src = "package Role::REST;\nsub success { 1 }\n1;\n";
    let child_src = "package Ctrl;\nuse parent 'Role::REST';\nsub find ($c) { $c->success(); }\n1;\n";
    // Decoy: an unrelated class with its own `success`, called on its
    // own instance — must NOT be attributed to Role::REST.
    let decoy_src = "package Loner;\nsub new { bless {}, shift }\nsub success { 0 }\nsub go { my $x = Loner->new; $x->success(); }\n1;\n";

    let role_path = PathBuf::from("/tmp/inh_role.pm");
    let child_path = PathBuf::from("/tmp/inh_child.pm");
    let decoy_path = PathBuf::from("/tmp/inh_decoy.pm");

    // Module index carries parents so the child's cross-file ancestor
    // walk reaches Role::REST.
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(role_path.clone(), Arc::new(parse(role_src)));
    idx.register_workspace_module(child_path.clone(), Arc::new(parse(child_src)));

    let store = FileStore::new();
    store.insert_workspace(role_path.clone(), parse(role_src));
    store.insert_workspace(child_path.clone(), parse(child_src));
    store.insert_workspace(decoy_path.clone(), parse(decoy_src));

    let target = TargetRef {
        name: "success".to_string(),
        kind: TargetKind::Method { class: "Role::REST".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let refs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));

    assert!(hit(&role_path), "missed Role::REST::success decl. hits: {:?}", refs);
    assert!(
        hit(&child_path),
        "missed $c->success() in child controller (inherited from Role::REST). hits: {:?}",
        refs,
    );
    assert!(
        !hit(&decoy_path),
        "cross-linked Loner::success (unrelated class, own method). hits: {:?}",
        refs,
    );
}

/// `references_mask_for`: a target declared in editable space (open or
/// workspace) scopes to EDITABLE so "find references" never scans
/// @INC; a target with no editable declaration widens to VISIBLE.
#[test]
fn references_mask_scopes_to_editable_for_project_symbols() {
    let store = FileStore::new();
    let def = PathBuf::from("/tmp/mask_def.pm");
    store.insert_workspace(
        def.clone(),
        parse("package Proj;\nsub thing { 1 }\n1;\n"),
    );

    // Declared in the workspace → editable, no dep scan.
    let in_ws = TargetRef {
        name: "thing".to_string(),
        kind: TargetKind::Sub { package: Some("Proj".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    assert_eq!(
        references_mask_for(&store, None, &in_ws).bits(),
        RoleMask::EDITABLE.bits(),
        "project-declared sub should scope to EDITABLE",
    );

    // No editable declaration anywhere → widen to VISIBLE so refs into
    // a dependency-defined symbol still surface.
    let dep_only = TargetRef {
        name: "nowhere".to_string(),
        kind: TargetKind::Sub { package: Some("CPAN::Thing".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    assert_eq!(
        references_mask_for(&store, None, &dep_only).bits(),
        RoleMask::VISIBLE.bits(),
        "symbol with no editable decl should widen to VISIBLE",
    );
}

// ---- Rename-specific pins --------------------------------------------------------
//
// `rename_via_refs_to` in backend.rs calls `refs_to(EDITABLE)` directly —
// so these tests exercise the same code path rename uses. If rename and
// references ever diverge, a test here (EDITABLE) will disagree with
// the references test (VISIBLE/EDITABLE via references_mask_for).

/// Rename a base-class method: call sites on child-class invocants in
/// other workspace files must be included. This is the inheritance
/// fan-out the old per-file `rename_method_in_class` missed — it matched
/// `invocant_class == target_class` exactly, so `$child->ping()` (where
/// invocant class is "Child") fell out when targeting "Base::ping".
///
/// `refs_to` uses `method_rename_chain(invocant_class)` which checks
/// whether the target class is anywhere on the invocant's resolution
/// chain, so `$child->ping()` targeting Base IS matched when Child
/// inherits Base's `ping`.
///
/// Cross-file parent resolution requires a ModuleIndex (the consumer
/// file doesn't declare Child's parents — only child.pm does). The
/// module index is how production code surfaces this: the LSP backend
/// passes `Some(&self.module_index)` to every `refs_to` call.
#[test]
fn rename_base_method_includes_child_call_sites() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // Base class file: defines the method being renamed.
    let base_src = r#"
package Base;
sub new { bless {}, shift }
sub ping { "pong" }
1;
"#;
    // Child class file: inherits from Base. No local `ping` override.
    let child_src = r#"
package Child;
use parent 'Base';
1;
"#;
    // Consumer file: calls `ping` on a Child instance.
    let consumer_src = r#"
package Consumer;
use Child;
my $c = Child->new;
$c->ping;
1;
"#;
    // Decoy file: unrelated class with a same-named method — must NOT appear.
    let decoy_src = r#"
package Decoy;
sub new { bless {}, shift }
sub ping { "decoy" }
my $d = Decoy->new;
$d->ping;
1;
"#;

    let base_path = PathBuf::from("/tmp/rename_base.pm");
    let child_path = PathBuf::from("/tmp/rename_child.pm");
    let consumer_path = PathBuf::from("/tmp/rename_consumer.pm");
    let decoy_path = PathBuf::from("/tmp/rename_decoy.pm");

    // Register base + child in module index so cross-file parent resolution
    // works (consumer_fa sees Child's parents via idx.parents_cached("Child")).
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(base_path.clone(), Arc::new(parse(base_src)));
    idx.register_workspace_module(child_path.clone(), Arc::new(parse(child_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    let store = FileStore::new();
    store.insert_workspace(base_path.clone(), parse(base_src));
    store.insert_workspace(child_path.clone(), parse(child_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);
    store.insert_workspace(decoy_path.clone(), parse(decoy_src));

    // Targeting Base::ping (where rename cursor would sit at the `sub ping` declaration).
    let target = TargetRef {
        name: "ping".to_string(),
        kind: TargetKind::Method { class: "Base".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };

    // Rename uses EDITABLE — workspace-only, no dep scan.
    let locs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let hits: Vec<(&str, usize)> = locs.iter().map(|r| {
        let fname = match &r.key {
            FileKey::Path(p) => p.file_name().unwrap().to_str().unwrap(),
            FileKey::Url(_) => "url",
        };
        (fname, r.span.start.row)
    }).collect();

    // Base::ping declaration must be included.
    assert!(
        hits.iter().any(|(f, _)| *f == "rename_base.pm"),
        "rename missed Base::ping declaration: {:?}", hits,
    );
    // Consumer's $c->ping call on a Child invocant must be included —
    // Child inherits ping from Base so it's on the rename chain.
    assert!(
        hits.iter().any(|(f, _)| *f == "rename_consumer.pm"),
        "rename missed Consumer's $$c->ping call (inherited from Base via Child): {:?}", hits,
    );
    // Decoy::ping is an unrelated class — must NOT be included.
    assert!(
        !hits.iter().any(|(f, _)| *f == "rename_decoy.pm"),
        "rename wrongly included Decoy::ping (unrelated class): {:?}", hits,
    );
}

/// Rename never edits dependency files. A `ping` method that also appears
/// in a dep module (registered in ModuleIndex, not in the workspace store)
/// must not produce edits for that dep file — `RoleMask::EDITABLE` stops
/// at OPEN + WORKSPACE, which is what `rename_via_refs_to` uses.
#[test]
fn rename_does_not_edit_dep_files() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // The dep defines Base::ping. It's in the module index (dep cache),
    // not in the workspace store.
    let dep_src = r#"
package Base;
sub new { bless {}, shift }
sub ping { "pong" }
1;
"#;
    let dep_path = PathBuf::from("/tmp/rename_dep_base.pm");

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(dep_path.clone(), Arc::new(parse(dep_src)));

    // Consumer in the workspace calls ping on a Base invocant.
    let consumer_src = r#"
package Consumer;
my $b = Base->new;
$b->ping;
1;
"#;
    let consumer_path = PathBuf::from("/tmp/rename_dep_consumer.pm");

    let store = FileStore::new();
    store.insert_workspace(consumer_path.clone(), parse(consumer_src));

    let target = TargetRef {
        name: "ping".to_string(),
        kind: TargetKind::Method { class: "Base".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };

    // EDITABLE mask — rename never scans deps.
    let editable_locs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    for loc in &editable_locs {
        assert!(
            !matches!(&loc.key, FileKey::Path(p) if p == &dep_path),
            "rename emitted an edit for the dep file (read-only): {:?}", editable_locs,
        );
    }

    // Sanity: VISIBLE would find the dep decl.
    let visible_locs = refs_to(&store, Some(&idx), &target, RoleMask::VISIBLE);
    assert!(
        visible_locs.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &dep_path)),
        "sanity: VISIBLE should see dep file's ping decl: {:?}", visible_locs,
    );
}

/// Rename and references agree on the target set for a base-class Method:
/// both call `refs_to` with the same target, so the only difference should
/// be the mask (EDITABLE vs references_mask_for's EDITABLE-or-VISIBLE).
/// When the method is defined in workspace, references_mask_for returns
/// EDITABLE — so the result sets are IDENTICAL.
///
/// This is the DRY invariant: rename and references share one code path.
#[test]
fn rename_and_references_agree_on_same_base_method_target() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let base_src = r#"
package Greeter;
sub new { bless {}, shift }
sub hello { "hi" }
1;
"#;
    let child_src = r#"
package ChildGreeter;
use parent 'Greeter';
1;
"#;
    let consumer_src = r#"
package Main;
use ChildGreeter;
my $g = ChildGreeter->new;
$g->hello;
1;
"#;

    let base_path = PathBuf::from("/tmp/agree_greeter.pm");
    let child_path = PathBuf::from("/tmp/agree_child.pm");
    let consumer_path = PathBuf::from("/tmp/agree_consumer.pm");

    // Module index carries the Child→Base parent edge for cross-file chain walk.
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(base_path.clone(), Arc::new(parse(base_src)));
    idx.register_workspace_module(child_path.clone(), Arc::new(parse(child_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    let store = FileStore::new();
    store.insert_workspace(base_path.clone(), parse(base_src));
    store.insert_workspace(child_path.clone(), parse(child_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);

    let target = TargetRef {
        name: "hello".to_string(),
        kind: TargetKind::Method { class: "Greeter".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };

    // references uses references_mask_for (EDITABLE when def in workspace).
    let ref_mask = references_mask_for(&store, Some(&idx), &target);
    assert_eq!(ref_mask.bits(), RoleMask::EDITABLE.bits(),
        "precondition: references_mask_for should be EDITABLE when def is in workspace");

    let rename_locs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let ref_locs    = refs_to(&store, Some(&idx), &target, ref_mask);

    // Collect (path, row) pairs for easy comparison.
    let to_set = |v: &[crate::resolve::RefLocation]| -> std::collections::BTreeSet<(String, usize)> {
        v.iter().map(|r| {
            let p = match &r.key {
                FileKey::Path(p) => p.display().to_string(),
                FileKey::Url(u) => u.to_string(),
            };
            (p, r.span.start.row)
        }).collect()
    };
    let rename_set = to_set(&rename_locs);
    let ref_set    = to_set(&ref_locs);

    assert_eq!(
        rename_set, ref_set,
        "rename and references disagree on target set for Greeter::hello\n\
         rename-only: {:?}\nrefs-only: {:?}",
        rename_set.difference(&ref_set).collect::<Vec<_>>(),
        ref_set.difference(&rename_set).collect::<Vec<_>>(),
    );
    // Also verify the consumer (child invocant) is in BOTH sets.
    assert!(
        rename_set.iter().any(|(f, _)| f.contains("agree_consumer")),
        "both rename and references must include consumer's $$g->hello call: {:?}",
        rename_set,
    );
}

/// Rename invoked AT THE CHILD CALL SITE of an inherited base method must
/// still edit the parent declaration. Here `rename_kind_at` returns the
/// cursor's static class (`MyWorker`), not the defining class (`BaseWorker`):
/// the parent `sub process` decl lives in a class the target's strict
/// `class` field never names. The fix precomputes the inheritance
/// rename-chain (`[MyWorker, .., BaseWorker]`) on the target — built from the
/// originating analysis, the only one that knows MyWorker's parents — so
/// `symbol_defines_target` admits a `sub NAME` in ANY chain class. Without
/// it the base declaration edit silently drops (the e2e inheritance rename
/// regression).
#[test]
fn rename_from_child_call_site_includes_inherited_base_declaration() {
    use crate::file_analysis::RenameKind;
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let base_src = "package BaseWorker;\nsub new { bless {}, shift }\nsub process { 1 }\n1;\n";
    // Child inherits process; no local override.
    let child_src = "package MyWorker;\nuse parent 'BaseWorker';\n1;\n";
    // Script calls process() on a MyWorker instance.
    let script_src = "use MyWorker;\nmy $worker = MyWorker->new;\n$worker->process();\n";

    let base_path = PathBuf::from("/tmp/cs_base.pm");
    let child_path = PathBuf::from("/tmp/cs_child.pm");
    let script_path = PathBuf::from("/tmp/cs_script.pl");

    // The originating analysis (the script) must know MyWorker's parents to
    // build the chain — production threads the module index for exactly this.
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(base_path.clone(), Arc::new(parse(base_src)));
    idx.register_workspace_module(child_path.clone(), Arc::new(parse(child_src)));

    let mut script_fa = parse(script_src);
    script_fa.enrich_imported_types_with_keys(Some(&idx));

    // Resolve the target the way the rename handler does: cursor on the
    // `process` token of `$worker->process()`, mapped via from_rename_kind.
    let call_point = tree_sitter::Point { row: 2, column: 9 };
    let rk = script_fa.rename_kind_at(call_point, Some(&idx));
    assert!(
        matches!(&rk, Some(RenameKind::Method { class, .. }) if class == "MyWorker"),
        "precondition: call-site rename_kind_at should resolve invocant class MyWorker, got {:?}",
        rk,
    );
    let target = TargetRef::from_rename_kind(rk.unwrap(), &script_fa, Some(&idx), OverrideScope::Dispatch)
        .expect("Method maps to a target");
    assert!(
        target.method_classes.iter().any(|c| c == "BaseWorker"),
        "chain must reach the defining ancestor BaseWorker, got {:?}",
        target.method_classes,
    );

    let store = FileStore::new();
    store.insert_workspace(base_path.clone(), parse(base_src));
    store.insert_workspace(child_path.clone(), parse(child_src));
    store.insert_workspace(script_path.clone(), script_fa);

    let locs = refs_to(&store, Some(&idx), &target, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| locs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));

    assert!(
        hit(&base_path),
        "rename from child call site dropped the BaseWorker::process declaration edit. hits: {:?}",
        locs,
    );
    assert!(
        hit(&script_path),
        "rename from child call site missed the $worker->process() call edit. hits: {:?}",
        locs,
    );
}

// ---- resolve_symbol: the single cursor→target entry point ----

/// Every kind that maps to a cross-file target must come back as
/// `Target`, lexical variables as `Local`, and blank space as `None` —
/// the same answers regardless of which handler (LSP or CLI) asks.
#[test]
fn test_resolve_symbol_kinds() {
    let src = "\
package Counter;
sub new { my ($class) = @_; return bless { count => 0 }, $class }
sub bump { my ($self) = @_; $self->{count}++; my $local = 1; return $local }
1;
";
    let fa = parse(src);
    let at = |row, col| resolve_symbol(&fa, tree_sitter::Point { row, column: col }, None);

    // `bump` decl → callable target scoped to the package ("same callable,
    // two shapes": decls surface as Sub even when call sites are Method).
    match at(2, 5) {
        Some(ResolvedTarget::Target(t)) => {
            assert_eq!(t.name, "bump");
            assert!(
                matches!(&t.kind, TargetKind::Sub { package: Some(p) } if p == "Counter"),
                "expected Sub scoped to Counter, got {:?}",
                t.kind,
            );
            assert!(t.supports_cross_file_rename());
        }
        other => panic!("expected callable target for bump decl, got {:?}", other),
    }

    // Package name → Package target.
    match at(0, 9) {
        Some(ResolvedTarget::Target(t)) => {
            assert!(matches!(t.kind, TargetKind::Package));
            assert!(t.supports_cross_file_rename());
        }
        other => panic!("expected Package target, got {:?}", other),
    }

    // `$local` → lexical, single-file.
    let local_col = src.lines().nth(2).unwrap().find("$local").unwrap() + 1;
    assert!(
        matches!(at(2, local_col), Some(ResolvedTarget::Local)),
        "expected Local for lexical $local, got {:?}",
        at(2, local_col),
    );
}

/// An owned hash key resolves to a cross-file HashKeyOfBridged target —
/// A Moo internal slot (`$self->{size}`) is one spelling of the `size` attr,
/// so it resolves to the attr's projection Group — the same group its `has`
/// decl, ctor key, reader, and mapped accessors resolve to. It
/// is NOT a plain `HashKeyOfBridged` rename: that would miss the accessor /
/// ctor-key sites the group carries. The group walks cross-file via
/// `group_rename_edits`.
#[test]
fn test_resolve_symbol_internal_slot_resolves_to_attr_group() {
    let src = "\
package Widget;
use Moo;
has size => (is => 'ro');
sub describe { my ($self) = @_; return $self->{size} }
1;
";
    let fa = parse(src);
    // Cursor on `size` inside `$self->{size}`.
    let col = src.lines().nth(3).unwrap().find("{size}").unwrap() + 1;
    match resolve_symbol(&fa, tree_sitter::Point { row: 3, column: col }, None) {
        Some(ResolvedTarget::Group { members, .. }) => {
            assert!(
                members.iter().any(|m| matches!(
                    m.target.kind,
                    TargetKind::HashKeyOfSub { .. } | TargetKind::InternalHashKey { .. }
                )),
                "slot group should carry the attr's ctor-key / internal-slot members: {:?}",
                members,
            );
        }
        other => panic!("expected the size attr projection Group, got {:?}", other),
    }
}

// ---- field projection groups: cross-file union ----

/// `field $x :param :reader` in Point.pm; a consumer constructs
/// `Point->new(x => 1)` and reads `$p->x`. References/rename from the
/// field decl must surface the consumer's ctor key and reader call;
/// from the consumer's key, the field must surface back.
#[test]
fn test_field_group_unions_across_files() {
    let store = FileStore::new();
    let point_path = PathBuf::from("/tmp/fieldgroup_point.pm");
    let user_path = PathBuf::from("/tmp/fieldgroup_user.pl");

    let point_src = "\
use v5.38;
class Point {
    field $x :param :reader;
    method magnitude () { return $x * $x; }
}
1;
";
    let user_src = "\
use Point;
my $p = Point->new(x => 3);
my $val = $p->x;
";
    let point_fa = parse(point_src);
    let user_fa = parse(user_src);
    store.insert_workspace(point_path.clone(), point_fa);
    store.insert_workspace(user_path.clone(), user_fa);

    let origin_fa = store.workspace_raw().get(&point_path).unwrap().value().clone();
    // Cursor on `$x` in the field decl (row 2, col 11 = bare name).
    let resolved = resolve_symbol(&origin_fa, tree_sitter::Point { row: 2, column: 11 }, None)
        .expect("field decl resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group, got {:?}", resolved);
    };
    assert!(pinned_spans.is_empty(), "local mint has no pinned spans");
    assert!(!local_spans.is_empty(), "field var spellings present");
    assert_eq!(members.len(), 2, "reader + ctor-key members: {:?}", members);

    let locs = group_refs(
        &store,
        None,
        &FileKey::Path(point_path.clone()),
        &local_spans,
        &pinned_spans,
        &members,
        None,
    );
    let in_user: Vec<_> = locs
        .iter()
        .filter(|l| matches!(&l.key, FileKey::Path(p) if p == &user_path))
        .map(|l| (l.span.start.row, l.span.start.column))
        .collect();
    assert!(
        in_user.contains(&(1, 19)),
        "consumer ctor key `x` included; user-file hits: {:?}",
        in_user,
    );
    assert!(
        in_user.contains(&(2, 14)),
        "consumer reader call `->x` included; user-file hits: {:?}",
        in_user,
    );
}

/// Consumer-side cursor, class elsewhere: from the ctor key (or accessor
/// call) in a file that only `use`s Point, the group is minted from the
/// CLASS's cached analysis — its field-variable/decl spans pin to the
/// class file, so rename from the consumer rewrites the field decl and
/// body uses over there too.
#[test]
fn test_consumer_cursor_mints_group_from_class_analysis() {
    let point_src = "\
use v5.38;
class Point {
    field $x :param :reader;
    method magnitude () { return $x * $x; }
}
1;
";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let class_path = PathBuf::from("/tmp/grp_mint_point.pm");
    idx.insert_cache(
        "Point",
        Some(std::sync::Arc::new(crate::module_index::CachedModule::new(
            class_path.clone(),
            std::sync::Arc::new(parse(point_src)),
        ))),
    );

    let consumer = parse("use Point;\nmy $p = Point->new(x => 3);\nmy $v = $p->x;\n");

    // From the ctor key `x` (row 1, col 19).
    let resolved = resolve_symbol(&consumer, tree_sitter::Point { row: 1, column: 19 }, Some(&idx))
        .expect("consumer key resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group from consumer key, got {:?}", resolved);
    };
    assert!(local_spans.is_empty(), "remote mint: no origin spans");
    assert_eq!(members.len(), 2, "reader + ctor-key members");
    assert!(
        pinned_spans.iter().all(|(p, _)| p == &class_path),
        "pinned to the class file: {:?}",
        pinned_spans,
    );
    // Decl (row 2) + body use (row 3) pinned from the class analysis.
    let pinned_rows: Vec<usize> = pinned_spans.iter().map(|(_, s)| s.start.row).collect();
    assert!(
        pinned_rows.contains(&2) && pinned_rows.contains(&3),
        "field decl + body use pinned: {:?}",
        pinned_rows,
    );

    // From the accessor call `->x` (row 2, col 12): same group shape.
    let resolved = resolve_symbol(&consumer, tree_sitter::Point { row: 2, column: 12 }, Some(&idx))
        .expect("consumer accessor resolves");
    assert!(
        matches!(resolved, ResolvedTarget::Group { ref pinned_spans, .. } if !pinned_spans.is_empty()),
        "accessor-call cursor mints the remote group, got {:?}",
        resolved,
    );
}

/// Cross-file mapped rename: the consumer's `$w->has_size` predicate
/// call re-derives to `has_extent` when the attr renames — per-member
/// replacement texts via group_rename_edits.
#[test]
fn test_group_rename_rederives_mapped_members_cross_file() {
    let store = FileStore::new();
    let class_path = PathBuf::from("/tmp/grp_map_widget.pm");
    let user_path = PathBuf::from("/tmp/grp_map_user.pl");
    store.insert_workspace(
        class_path.clone(),
        parse("package Widget;\nuse Moo;\nhas size => (is => 'ro', predicate => 1);\n1;\n"),
    );
    store.insert_workspace(
        user_path.clone(),
        parse("use Widget;\nmy $w = Widget->new(size => 3);\nprint $w->size if $w->has_size;\n"),
    );

    let class_fa = store.workspace_raw().get(&class_path).unwrap().value().clone();
    // Cursor on the attr decl token `size` (row 2, col 4).
    let resolved = resolve_symbol(&class_fa, tree_sitter::Point { row: 2, column: 4 }, None)
        .expect("attr decl resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group, got {:?}", resolved);
    };
    let edits = group_rename_edits(
        &store,
        None,
        &FileKey::Path(class_path.clone()),
        &local_spans,
        &pinned_spans,
        &members,
        "extent",
    );
    let user_edits: Vec<_> = edits
        .iter()
        .filter(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &user_path))
        .map(|(l, t)| (l.span.start.row, l.span.start.column, t.clone()))
        .collect();
    assert!(
        user_edits.contains(&(2, 22, "has_extent".to_string())),
        "consumer predicate call re-derived; user edits: {:?}",
        user_edits,
    );
    assert!(
        user_edits.iter().any(|(r, _, t)| *r == 1 && t == "extent"),
        "consumer ctor key renamed bare; user edits: {:?}",
        user_edits,
    );
}

/// Explicit-string accessor names (`predicate => 'has_size'`) define the method
/// AT that string, so renaming the attr must rewrite the defining string too —
/// not just the call sites — or Moo keeps minting `has_size` while callers say
/// `has_dim` (non-compiling). The `=> 1` derived form has no string and is
/// covered by `test_group_rename_rederives_mapped_members_cross_file`.
#[test]
fn moo_explicit_string_accessor_renames_its_defining_string() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/moo_str_widget.pm");
    let src = "package Widget;\nuse Moo;\n\
        has size => (is => 'rw', predicate => 'has_size', clearer => 'clear_size');\n\
        sub area { my $self = shift; return $self->size if $self->has_size; }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    // Cursor on the `has size` attr token (row 2, col 4).
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 2, column: 4 }, None)
        .expect("attr decl resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group, got {:?}", resolved);
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "dim",
    );
    let lines: Vec<&str> = src.lines().collect();
    let materialized: Vec<(String, String)> = edits
        .iter()
        .map(|(l, t)| {
            (lines[l.span.start.row][l.span.start.column..l.span.end.column].to_string(), t.clone())
        })
        .collect();
    // The defining strings rewrite to the affixed new name, and the call site
    // stays consistent with them.
    for want in [("has_size", "has_dim"), ("clear_size", "clear_dim")] {
        assert!(
            materialized.contains(&(want.0.to_string(), want.1.to_string())),
            "defining string {:?} must rewrite to {:?}; got {:?}",
            want.0, want.1, materialized,
        );
    }
    // has_dim appears at least twice: the predicate defining string + the call.
    assert!(
        materialized.iter().filter(|(_, t)| t == "has_dim").count() >= 2,
        "predicate string AND its call rename together; got {:?}",
        materialized,
    );
}

/// `our` package globals rename cross-file: `$Cfg::debug` is the same variable
/// everywhere, so renaming the `our` decl reaches every qualified access in
/// other files, and vice versa. A lexical `my` stays single-file (`Local`).
#[test]
fn package_var_our_renames_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let lib = PathBuf::from("/tmp/pv_cfg.pm");
    let app = PathBuf::from("/tmp/pv_app.pl");
    let lib_src = "package Cfg;\nour $debug = 0;\nsub on { $debug = 1 }\n1;\n";
    let app_src = "use Cfg;\nprint $Cfg::debug;\n$Cfg::debug = 5;\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(lib.clone(), Arc::new(parse(lib_src)));
    store.insert_workspace(lib.clone(), parse(lib_src));
    store.insert_workspace(app.clone(), parse(app_src));

    let lib_fa = store.workspace_raw().get(&lib).unwrap().value().clone();

    // Cursor on the `our $debug` decl resolves to a cross-file PackageVar.
    let col = lib_src.lines().nth(1).unwrap().find("debug").unwrap();
    let resolved = resolve_symbol(&lib_fa, tree_sitter::Point { row: 1, column: col }, Some(&idx))
        .expect("our decl resolves");
    let ResolvedTarget::Target(t) = resolved else { panic!("expected Target, got {:?}", resolved) };
    assert!(
        matches!(&t.kind, TargetKind::PackageVar { package } if package == "Cfg"),
        "our var should resolve to PackageVar, got {:?}",
        t.kind,
    );
    assert!(t.supports_cross_file_rename());

    let refs = refs_to(&store, Some(&idx), &t, RoleMask::EDITABLE);
    let hit = |p: &PathBuf| refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p));
    assert!(hit(&lib), "reaches the decl file. refs: {:?}", refs);
    assert!(hit(&app), "reaches the cross-file $Cfg::debug accesses. refs: {:?}", refs);
    // Both qualified accesses in app.pl + decl + unqualified in lib.
    assert!(refs.len() >= 4, "decl + unqualified + 2 qualified accesses: {:?}", refs);

    // A lexical `my` stays single-file (Local) — package globals only.
    let my_fa = parse("my $x = 1;\nsub f { $x + 1 }\n");
    assert!(
        matches!(resolve_symbol(&my_fa, tree_sitter::Point { row: 0, column: 3 }, None), Some(ResolvedTarget::Local)),
        "lexical my stays Local",
    );
}

/// The rename name guard rejects corrupting (empty/whitespace/sigil-only)
/// names so neither entry point emits a token-deleting edit set, while real
/// names (sigil-bearing variable names included) pass.
#[test]
fn rename_name_guard_rejects_empty_and_whitespace() {
    use crate::resolve::is_valid_rename_name;
    for bad in ["", " ", "   ", "\t", "$", "@", "%", "$ ", "  @  "] {
        assert!(!is_valid_rename_name(bad), "should reject {bad:?}");
    }
    for ok in ["dim", "foo_bar", "$scalar", "@list", "%map", "RENAMED"] {
        assert!(is_valid_rename_name(ok), "should accept {ok:?}");
    }
}

/// Array/hash package globals rename the bare name tail only — qualified
/// element/slice reads (`$Pkg::items[0]`, `$Pkg::map{k}`) span the whole
/// `$Pkg::name`, so the rewrite must be anchored at the span end or it eats the
/// sigil + `Pkg::` qualifier and produces invalid Perl.
#[test]
fn package_var_array_hash_globals_rename_name_tail_only() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let lib = PathBuf::from("/tmp/pvc_pkg.pm");
    let app = PathBuf::from("/tmp/pvc_user.pl");
    let lib_src = "package Pkg;\nour @items = (1, 2, 3);\nour %map = (a => 1);\n1;\n";
    let app_src = "use Pkg;\nmy $f = $Pkg::items[0];\nmy @s = @Pkg::items[0, 1];\nmy $v = $Pkg::map{a};\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(lib.clone(), Arc::new(parse(lib_src)));
    store.insert_workspace(lib.clone(), parse(lib_src));
    store.insert_workspace(app.clone(), parse(app_src));
    let lib_fa = store.workspace_raw().get(&lib).unwrap().value().clone();

    // Cursor on `our @items`.
    let col = lib_src.lines().nth(1).unwrap().find("items").unwrap();
    let ResolvedTarget::Target(t) =
        resolve_symbol(&lib_fa, tree_sitter::Point { row: 1, column: col }, Some(&idx)).unwrap()
    else {
        panic!("our @items should resolve to a target")
    };
    let refs = refs_to(&store, Some(&idx), &t, RoleMask::EDITABLE);

    // Every edit in user.pl must cover exactly `items` (the name tail), never
    // starting on the `$`/`@` sigil or the `Pkg::` qualifier.
    let app_edits: Vec<_> = refs
        .iter()
        .filter(|r| matches!(&r.key, FileKey::Path(p) if p == &app))
        .collect();
    assert_eq!(app_edits.len(), 2, "both element + slice accesses: {:?}", app_edits);
    for e in &app_edits {
        let line = app_src.lines().nth(e.span.start.row).unwrap();
        let slice = &line[e.span.start.column..e.span.end.column];
        assert_eq!(slice, "items", "rewrite only the name tail, got {slice:?} in {line:?}");
    }
}

/// A `main` package global unifies its spellings *within its file* — decl, bare
/// reads, `$main::x`, `$::x` all rename together — but stays FILE-LOCAL: `main`
/// is the shared namespace of unrelated package-less scripts, so it resolves to
/// a flat origin-file `Group`, not a cross-file `PackageVar` (which would sweep
/// a different script's `main::x`). See the entrypoint-analysis note in resolve.
#[test]
fn package_var_main_global_is_file_local_group() {
    let src = "our $gv = 1;\nprint $gv;\nsub f { return $gv + 1 }\nprint $main::gv;\nprint $::gv;\n";
    let fa = parse(src);
    let ResolvedTarget::Group { local_spans, members, .. } =
        resolve_symbol(&fa, tree_sitter::Point { row: 0, column: 5 }, None).unwrap()
    else {
        panic!("a `main` global should resolve to a file-local Group, not a cross-file target")
    };
    assert!(members.is_empty(), "a flat package-var group has no projection members");
    let mut rows: Vec<usize> = local_spans.iter().map(|s| s.start.row).collect();
    rows.sort_unstable();
    // decl(0) + bare $gv(1) + bare $gv in sub(2) + $main::gv(3) + $::gv(4).
    assert_eq!(rows, vec![0, 1, 2, 3, 4], "every in-file spelling renames: {local_spans:?}");
}

/// Two unrelated package-less scripts both declare `our $config` (both in
/// `main`). Renaming one must NOT rewrite the other — `main` globals are
/// file-local until entrypoint analysis can prove two files are one program.
#[test]
fn package_var_main_global_does_not_cross_scripts() {
    let store = FileStore::new();
    let a = PathBuf::from("/tmp/pvm_a.pl");
    let b = PathBuf::from("/tmp/pvm_b.pl");
    let a_src = "our $config = 1;\nprint $config;\n";
    store.insert_workspace(a.clone(), parse(a_src));
    store.insert_workspace(b.clone(), parse("our $config = 2;\nprint $config;\n"));

    let a_fa = store.workspace_raw().get(&a).unwrap().value().clone();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&a_fa, tree_sitter::Point { row: 0, column: 5 }, None).unwrap()
    else {
        panic!("main global should be a file-local Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(a.clone()), &local_spans, &pinned_spans, &members, "settings",
    );
    assert!(
        edits.iter().all(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &a)),
        "rename must stay in a.pl, never touch b.pl: {edits:?}",
    );
    assert_eq!(edits.len(), 2, "a.pl decl + bare read only: {edits:?}");
}

/// `OverrideScope` toggle: a method overridden in a child resolves to the
/// whole override family under `Hierarchy` (the default IDE refactor) but only
/// its own dispatch chain under `Dispatch`. Membership is edge-gated (`@ISA`),
/// never name-matched.
#[test]
fn override_scope_hierarchy_unions_dispatch_is_precise() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let base = PathBuf::from("/tmp/os_base.pm");
    let child = PathBuf::from("/tmp/os_child.pm");
    let base_src = "package Base;\nsub new { bless {}, shift }\nsub shared { 1 }\n1;\n";
    let child_src =
        "package Child;\nuse parent 'Base';\nsub shared { my $s = shift; $s->SUPER::shared() + 1 }\n1;\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(base.clone(), Arc::new(parse(base_src)));
    idx.register_workspace_module(child.clone(), Arc::new(parse(child_src)));
    store.insert_workspace(base.clone(), parse(base_src));
    store.insert_workspace(child.clone(), parse(child_src));

    let base_fa = store.workspace_raw().get(&base).unwrap().value().clone();

    // Hierarchy (default): Base::shared's family includes the Child override,
    // so a rename reaches Child's file.
    let h = TargetRef::method(
        "shared".to_string(), "Base".to_string(), &base_fa, Some(&idx), OverrideScope::Hierarchy,
    );
    assert!(
        h.method_classes.iter().any(|c| c == "Child"),
        "hierarchy family must include the override class: {:?}",
        h.method_classes,
    );
    let hrefs = refs_to(&store, Some(&idx), &h, RoleMask::EDITABLE);
    assert!(
        hrefs.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &child)),
        "hierarchy rename must reach the Child override: {:?}",
        hrefs,
    );

    // Dispatch: precise — the chain stops at the defining class, so the Child
    // override is NOT pulled into Base::shared's family.
    let d = TargetRef::method(
        "shared".to_string(), "Base".to_string(), &base_fa, Some(&idx), OverrideScope::Dispatch,
    );
    assert!(
        !d.method_classes.iter().any(|c| c == "Child"),
        "dispatch chain must NOT include the override class: {:?}",
        d.method_classes,
    );
}

/// Renaming imports (`use Exp beta => { -as => 'rb' }`). Two
/// distinct identities: the REMOTE name `beta` is the source `Exp::beta`
/// (renames together, across all consumers); the LOCAL alias `rb` is a
/// binding in the CONSUMING package (the `-as` value + local calls) that
/// never touches the exporter — not `Exp::beta`, not a stray `Exp::rb`.
#[test]
fn test_renaming_import_remote_joins_source_alias_stays_local() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let store = FileStore::new();
    let exp = PathBuf::from("/tmp/rni_exp.pm");
    let cons = PathBuf::from("/tmp/rni_cons.pm");
    let exp_src = "package Exp;\nuse Exporter 'import';\nour @EXPORT_OK = ('beta');\nsub beta { 1 }\n1;\n";
    let cons_src = "package Consumer;\nuse Exp beta => { -as => 'rb' };\nsub run { rb() }\n1;\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(exp.clone(), Arc::new(parse(exp_src)));
    store.insert_workspace(exp.clone(), parse(exp_src));
    store.insert_workspace(cons.clone(), parse(cons_src));

    let hit = |refs: &[RefLocation], p: &PathBuf| {
        refs.iter().any(|r| matches!(&r.key, FileKey::Path(x) if x == p))
    };

    // Source rename reaches the consumer's REMOTE `beta` token.
    let src = TargetRef {
        name: "beta".to_string(),
        kind: TargetKind::Sub { package: Some("Exp".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let src_refs = refs_to(&store, Some(&idx), &src, RoleMask::EDITABLE);
    assert!(hit(&src_refs, &exp), "source def missing: {:?}", src_refs);
    assert!(hit(&src_refs, &cons), "remote `beta` token must join the source: {:?}", src_refs);

    // Alias rename is local to the consuming package — never the exporter.
    let alias = TargetRef {
        name: "rb".to_string(),
        kind: TargetKind::Sub { package: Some("Consumer".to_string()) },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let alias_refs = refs_to(&store, Some(&idx), &alias, RoleMask::EDITABLE);
    assert!(hit(&alias_refs, &cons), "alias `-as` value + call missing: {:?}", alias_refs);
    assert!(
        !hit(&alias_refs, &exp),
        "alias rename must NOT touch the exporter: {:?}",
        alias_refs,
    );
    assert!(alias_refs.len() >= 2, "alias group = `-as` value + call: {:?}", alias_refs);
}

/// Over-reach: a hash key in a method call's args that isn't a column
/// (or verb param) must NOT hijack the method. `ref_at` returns the method-call
/// ref because its span covers the args, but only the method-name token renames
/// the method — gated on `method_name_span`.
#[test]
fn arg_key_does_not_hijack_enclosing_method() {
    let src = "package U;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id/);\n\
        sub go { my $self = shift; $self->search({ id => 1 }, { order_by => 'x' }); }\n1;\n";
    let fa = parse(src);
    let col = src.lines().nth(3).unwrap().find("order_by").unwrap();
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 3, column: col }, None);
    assert!(
        !matches!(&resolved, Some(ResolvedTarget::Target(t)) if matches!(&t.kind, TargetKind::Method { .. })),
        "a non-column arg key must never resolve to the enclosing method: {resolved:?}",
    );
}

/// A DBIC column's single-hashref call args (`update`/`find`
/// /`search` with one `{ col => ... }`) are column-keyed — the keys are
/// `Class`-owned columns, not verb params — so renaming the column reaches them.
#[test]
fn dbic_column_rename_reaches_single_arg_call_keys() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_argkey.pm");
    let src = "package U;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id name/);\n\
        sub go { my $self = shift; $self->update({ name => 1 }); return $self->find({ name => 2 }); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "RENAMED",
    );
    let rows: std::collections::BTreeSet<usize> = edits.iter().map(|(l, _)| l.span.start.row).collect();
    // Column def (row 2) + the `update` and `find` arg keys (both row 3).
    assert!(rows.contains(&2), "column def renames: {rows:?}");
    assert!(rows.contains(&3), "single-arg update/find column keys join: {rows:?}");
    assert!(
        edits.iter().filter(|(l, _)| l.span.start.row == 3).count() >= 2,
        "both the update and find arg keys: {edits:?}",
    );
}

/// Multi-arg `search(\%cond, \%attrs)`: only the FIRST hashref (the column
/// conditions) is column-keyed — the trailing `\%attrs` hash (`order_by`/…) is
/// never walked, so renaming a column joins `\%cond` keys but never an attr.
#[test]
fn dbic_column_rename_multiarg_search_excludes_attrs_hash() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_multiarg.pm");
    let src = "package U;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id name/);\n\
        sub go { my $self = shift; $self->search({ name => 1 }, { order_by => 'id' }); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    let lines: Vec<&str> = src.lines().collect();
    for (l, _) in &edits {
        let s = &lines[l.span.start.row][l.span.start.column..l.span.end.column];
        assert_eq!(s, "name", "only `\\%cond` column keys rename, never `order_by`: {edits:?}");
    }
    assert!(
        edits.iter().any(|(l, _)| l.span.start.row == 3),
        "the search `\\%cond` column key joins the column: {edits:?}",
    );
}

/// Fluent-verb chain off the VALID DBIC entry: `my $rs = $schema->resultset('User')`
/// types `$rs` to a `ResultSet<User>`; `$rs->search({col})` carries it (fluent),
/// `$rs->find({col})` joins the column key, and `my $u = $rs->find; $u->col`
/// (find→row→accessor through `RowOf`) does too. Renaming a column rewrites the
/// whole chain. `Class->search` is NOT valid DBIC (search lives on the resultset,
/// which only comes from the schema) and is deliberately not typed.
#[test]
fn dbic_column_rename_reaches_fluent_resultset_chain() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_fluent.pm");
    let src = "package User;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id name/);\n\
        sub go { my ($self, $schema) = @_; my $rs = $schema->resultset('User'); my $f = $rs->search({ name => 1 }); my $u = $rs->find({ name => 2 }); return $u->name; }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "RENAMED",
    );
    let lines: Vec<&str> = src.lines().collect();
    for (l, _) in &edits {
        let s = &lines[l.span.start.row][l.span.start.column..l.span.end.column];
        assert_eq!(s, "name", "every edit hits a `name` token: {edits:?}");
    }
    // Row 3: the `search` arg key, the `$rs->find` arg key, AND the `$u->name`
    // accessor — all three resultset-chain sites join the column.
    assert!(
        edits.iter().filter(|(l, _)| l.span.start.row == 3).count() >= 3,
        "search arg + $rs->find arg + $u->name accessor all join: {edits:?}",
    );
}

/// A DBIC column is a `Bridged` key, not a hash slot. Renaming the column from
/// any spelling rewrites the accessor + the condition-arg keys but NEVER a
/// `$row->{col}` deref (undef in DBIC). And a cursor ON the deref doesn't resolve
/// to the column group — it's not a column reference at all.
#[test]
fn dbic_column_rename_excludes_row_hashref_deref() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_deref.pm");
    let src = "package User;\n\
use base 'DBIx::Class::Core';\n\
__PACKAGE__->add_columns(qw/id name/);\n\
sub go {\n\
    my ($self, $schema) = @_;\n\
    my $rs = $schema->resultset('User');\n\
    my $row = $rs->find({ name => 1 });\n\
    my $bad = $row->{name};\n\
    return $row->name;\n\
}\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    let rows: std::collections::BTreeSet<usize> = edits.iter().map(|(l, _)| l.span.start.row).collect();
    // def (2), find-condition arg (6), accessor (8) — NOT the `$row->{name}`
    // deref (7): a column isn't a hash slot.
    assert_eq!(rows, [2, 6, 8].into_iter().collect(), "deref row 7 excluded: {edits:?}");
    // A cursor on the `$row->{name}` deref is NOT the column group.
    let deref_col = src.lines().nth(7).unwrap().find("name").unwrap();
    assert!(
        !matches!(
            resolve_symbol(&fa, tree_sitter::Point { row: 7, column: deref_col }, None),
            Some(ResolvedTarget::Group { .. })
        ),
        "cursor on the $row->{{name}} deref must not resolve to the column",
    );
}

/// A Moo `has name` attribute's group includes the cross-file CONSTRUCTOR-arg
/// key (`Widget->new(name => …)`), owned `Sub{class,new}` — so renaming the
/// attribute reaches the ctor key, and a cursor on the ctor key renames the
/// whole group (including itself). The DBIC column-keyed seam must not hijack
/// the Moo ctor key to a `Class` owner (it's not a column).
#[test]
fn moo_attr_group_includes_cross_file_constructor_key() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    let store = FileStore::new();
    let lib = PathBuf::from("/tmp/moo_ctor_widget.pm");
    let app = PathBuf::from("/tmp/moo_ctor_app.pl");
    let lib_src = "package Widget;\nuse Moo;\nhas name => (is => 'rw');\nsub greet { my $self = shift; $self->name }\n1;\n";
    let app_src = "use Widget;\nmy $w = Widget->new(name => 'bob');\nprint $w->name;\n";
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(lib.clone(), Arc::new(parse(lib_src)));
    store.insert_workspace(lib.clone(), parse(lib_src));
    store.insert_workspace(app.clone(), parse(app_src));
    let lib_fa = store.workspace_raw().get(&lib).unwrap().value().clone();
    let col = lib_src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&lib_fa, tree_sitter::Point { row: 2, column: col }, Some(&idx)).expect("attr resolves")
    else {
        panic!("expected a Moo attr Group")
    };
    let edits = group_rename_edits(
        &store, Some(&idx), &FileKey::Path(lib.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    // app.pl row 1 is the `Widget->new(name => …)` ctor key.
    assert!(
        edits.iter().any(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &app) && l.span.start.row == 1),
        "attr rename must reach the cross-file constructor key: {edits:?}",
    );
}

/// A custom-named accessor that does NOT embed the attr (`predicate =>
/// 'has_size'` for attr `x`) is an independent method — a cursor on it must
/// rename IT, not the attr group (else the click silently renames a different
/// token). Only an embedding name (`has_size` for `size`) reverse-maps.
#[test]
fn non_embedding_mapped_accessor_renames_itself_not_the_attr() {
    let src = "package W;\nuse Moo;\nhas x => (is => 'rw', predicate => 'has_size');\n\
        sub probe { my $self = shift; return $self->has_size; }\n1;\n";
    let fa = parse(src);
    let col = src.lines().nth(3).unwrap().find("has_size").unwrap();
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 3, column: col }, None);
    assert!(
        matches!(&resolved, Some(ResolvedTarget::Target(t)) if t.name == "has_size"),
        "cursor on the non-embedding `has_size` must rename has_size itself, not attr `x`: {resolved:?}",
    );
}

/// Inheritance: renaming a base-class attr reaches a SUBCLASS construction
/// (`Dog->new(name => …)` where `Dog extends Animal`). The base attr's ctor-key
/// member `HashKeyOfSub{Animal,new}` matches the subclass ctor key across `@ISA`.
#[test]
fn inherited_attr_rename_reaches_subclass_constructor_key() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    let store = FileStore::new();
    let animal = PathBuf::from("/tmp/inh_animal.pm");
    let dog = PathBuf::from("/tmp/inh_dog.pm");
    let app = PathBuf::from("/tmp/inh_run.pl");
    let animal_src = "package Animal;\nuse Moo;\nhas name => (is => 'rw');\n1;\n";
    let dog_src = "package Dog;\nuse Moo;\nextends 'Animal';\n1;\n";
    let app_src = "use Dog;\nmy $d = Dog->new(name => 'Rex');\nprint $d->name;\n";
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(animal.clone(), Arc::new(parse(animal_src)));
    idx.register_workspace_module(dog.clone(), Arc::new(parse(dog_src)));
    store.insert_workspace(animal.clone(), parse(animal_src));
    store.insert_workspace(dog.clone(), parse(dog_src));
    store.insert_workspace(app.clone(), parse(app_src));
    let animal_fa = store.workspace_raw().get(&animal).unwrap().value().clone();
    let col = animal_src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&animal_fa, tree_sitter::Point { row: 2, column: col }, Some(&idx)).expect("attr resolves")
    else {
        panic!("expected a Moo attr Group")
    };
    let edits = group_rename_edits(
        &store, Some(&idx), &FileKey::Path(animal.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    assert!(
        edits.iter().any(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &app) && l.span.start.row == 1),
        "base attr rename must reach the subclass `Dog->new(name => …)` ctor key: {edits:?}",
    );
}

/// A Corinna `field` is per-class PRIVATE storage — NOT inherited like a Moo
/// attr. Renaming a subclass's field (where an ancestor declares the same field
/// name) must stay in the subclass: the inheritance bridge must not widen
/// field-backed groups, and the reader is scoped precisely (not the family).
#[test]
fn corinna_field_subclass_does_not_bleed_to_ancestor() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    let store = FileStore::new();
    let base = PathBuf::from("/tmp/cor_base.pm");
    let deriv = PathBuf::from("/tmp/cor_deriv.pm");
    let base_src = "use v5.38;\nclass CBase { field $size :param :reader; method show { return $size; } }\n";
    let deriv_src = "use v5.38;\nclass CDeriv :isa(CBase) { field $size :param :reader; method other { return $size; } }\n";
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(base.clone(), Arc::new(parse(base_src)));
    idx.register_workspace_module(deriv.clone(), Arc::new(parse(deriv_src)));
    store.insert_workspace(base.clone(), parse(base_src));
    store.insert_workspace(deriv.clone(), parse(deriv_src));
    let deriv_fa = store.workspace_raw().get(&deriv).unwrap().value().clone();
    let col = deriv_src.lines().nth(1).unwrap().find("size").unwrap();
    let resolved = resolve_symbol(&deriv_fa, tree_sitter::Point { row: 1, column: col }, Some(&idx))
        .expect("field resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected a field Group, got {resolved:?}")
    };
    let edits = group_rename_edits(
        &store, Some(&idx), &FileKey::Path(deriv.clone()), &local_spans, &pinned_spans, &members, "vol",
    );
    assert!(
        edits.iter().all(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &deriv)),
        "Corinna field rename must stay in the subclass, never touch the ancestor: {edits:?}",
    );
}

/// An OVERRIDDEN attribute (subclass redeclares the base's `has name`) renames
/// as ONE family under Hierarchy (the chosen scope) — from either class's decl
/// the edit set is identical and spans both decls + both classes' ctor keys.
/// Minting from the root-most declarer makes the two cursors symmetric.
#[test]
fn overridden_attr_renames_whole_family_symmetrically() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    let store = FileStore::new();
    let animal = PathBuf::from("/tmp/ov_animal.pm");
    let dog = PathBuf::from("/tmp/ov_dog.pm");
    let animal_src = "package Animal;\nuse Moo;\nhas name => (is => 'rw');\n1;\n";
    let dog_src = "package Dog;\nuse Moo;\nextends 'Animal';\nhas name => (is => 'rw');\n1;\n";
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(animal.clone(), Arc::new(parse(animal_src)));
    idx.register_workspace_module(dog.clone(), Arc::new(parse(dog_src)));
    store.insert_workspace(animal.clone(), parse(animal_src));
    store.insert_workspace(dog.clone(), parse(dog_src));

    let edits_from = |path: &PathBuf, src: &str, row: usize| {
        let fa = store.workspace_raw().get(path).unwrap().value().clone();
        let col = src.lines().nth(row).unwrap().find("name").unwrap();
        let r = resolve_symbol(&fa, tree_sitter::Point { row, column: col }, Some(&idx)).unwrap();
        let ResolvedTarget::Group { local_spans, pinned_spans, members } = r else {
            panic!("expected Group, got {r:?}")
        };
        let mut got: Vec<String> = group_rename_edits(
            &store, Some(&idx), &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
        )
        .iter()
        .map(|(l, _)| match &l.key {
            FileKey::Path(p) => format!("{}:{}", p.file_name().unwrap().to_string_lossy(), l.span.start.row),
            FileKey::Url(u) => format!("{u}:{}", l.span.start.row),
        })
        .collect();
        got.sort();
        got.dedup();
        got
    };
    let from_animal = edits_from(&animal, animal_src, 2);
    let from_dog = edits_from(&dog, dog_src, 3);
    assert_eq!(from_animal, from_dog, "override rename is symmetric (family)");
    assert!(
        from_animal.iter().any(|s| s.starts_with("ov_animal.pm"))
            && from_animal.iter().any(|s| s.starts_with("ov_dog.pm")),
        "the family spans both class decls: {from_animal:?}",
    );
}

/// A multi-key condition hashref links EVERY column key, not just the first.
/// Perl right-nests the tail pairs of `{ a => 1, b => 2, c => 3 }`; walking only
/// the top-level children would see only `a`. (`cst::pair_nodes` flattens it.)
#[test]
fn dbic_multi_key_hashref_links_all_columns() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_multikey.pm");
    let src = "package U;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/alpha beta gamma/);\n\
        sub go { my $self = shift; $self->search({ alpha => 1, beta => 2, gamma => 3 }); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    // Rename the MIDDLE column `beta` — it must reach the search arg key (row 3).
    let col = src.lines().nth(2).unwrap().find("beta").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    assert!(
        edits.iter().any(|(l, _)| l.span.start.row == 3),
        "the 2nd-key `beta` search arg must link to the column: {edits:?}",
    );
}

/// A method dispatched through a scalar (`my $m='poke'; $self->$m()`) is a
/// reference to the method but NOT a literal name — rename must skip it (else it
/// rewrites the `$m` variable and corrupts the dispatch), while references lists
/// it. Same rewritable split as folded handlers, now for callables.
#[test]
fn folded_method_dispatch_site_is_non_rewritable() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/folded_dispatch.pm");
    let src = "package D;\nsub poke { my $self = shift; return 1; }\n\
        sub run { my $self = shift; my $m = 'poke'; return $self->$m(); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 1, column: 4 }, None)
        .expect("sub poke resolves");
    let ResolvedTarget::Target(t) = resolved else { panic!("expected a Target, got {resolved:?}") };
    let refs = refs_to(&store, None, &t, RoleMask::EDITABLE);
    // The `$self->$m()` site (row 2) is present but frozen; the decl is rewritable.
    let lines: Vec<&str> = src.lines().collect();
    let folded = refs.iter().find(|r| {
        lines[r.span.start.row][r.span.start.column..r.span.end.column].starts_with("$m")
    });
    let folded = folded.expect("the folded $m dispatch site is a reference");
    assert!(!folded.rewritable, "the folded dispatch site must NOT be rewritten: {folded:?}");
    assert!(
        refs.iter().any(|r| r.rewritable),
        "the `sub poke` decl must still be rewritable: {refs:?}",
    );
}

/// Over-reach guard: a class that defines its OWN `sub <verb>` (shadowing a
/// DBIC column-keyed verb name) is NOT column-keyed — the call dispatches to the
/// user's method, whose hash arg isn't columns. Renaming the column must not
/// touch that custom method's `{ col => … }` arg key.
#[test]
fn dbic_custom_sub_shadowing_verb_is_not_column_keyed() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_shadow.pm");
    let src = "package Tag;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id name/);\n\
        sub create { my ($self, $args) = @_; return $args->{name}; }\n\
        sub go { my $self = shift; $self->create({ name => 'x' }); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    assert!(
        edits.iter().all(|(l, _)| l.span.start.row != 4),
        "custom `sub create`'s arg key (row 4) must NOT be column-keyed: {edits:?}",
    );
}

/// Over-reach guard: column-keying narrows to POSITIONAL arg 0. A
/// `search($cond, \%attrs)` (scalar cond) has no inline column keys — the
/// trailing `\%attrs` hash must never be walked as if it were the conditions.
#[test]
fn dbic_scalar_cond_does_not_column_key_attrs_hash() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/dbic_scalarcond.pm");
    let src = "package Row;\nuse base 'DBIx::Class::Core';\n\
        __PACKAGE__->add_columns(qw/id name/);\n\
        sub go { my ($self, $cond) = @_; $self->search($cond, { name => 'attrs' }); }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));
    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    let col = src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    assert!(
        edits.iter().all(|(l, _)| l.span.start.row != 3),
        "the `\\%attrs` hash key (row 3) must NOT be walked when cond is a scalar: {edits:?}",
    );
}

/// Cross-file: a column rename reaches a consumer's `Class->search({ col => … })`
/// arg key (columns defined in another file). The consumer emits the key with a
/// deferred owner; the column-keyed-verb seam mints `Class` at query time so it
/// joins the group, both directions.
#[test]
fn dbic_column_rename_reaches_cross_file_consumer_arg_key() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    let store = FileStore::new();
    let lib = PathBuf::from("/tmp/dbic_user.pm");
    let app = PathBuf::from("/tmp/dbic_app.pl");
    let lib_src = "package User;\nuse base 'DBIx::Class::Core';\n__PACKAGE__->add_columns(qw/id name/);\n1;\n";
    let app_src = "use User;\nmy $rs = User->search({ name => 'x' });\n";
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(lib.clone(), Arc::new(parse(lib_src)));
    store.insert_workspace(lib.clone(), parse(lib_src));
    store.insert_workspace(app.clone(), parse(app_src));
    let lib_fa = store.workspace_raw().get(&lib).unwrap().value().clone();
    let col = lib_src.lines().nth(2).unwrap().find("name").unwrap();
    let ResolvedTarget::Group { local_spans, pinned_spans, members } =
        resolve_symbol(&lib_fa, tree_sitter::Point { row: 2, column: col }, Some(&idx)).expect("column resolves")
    else {
        panic!("expected a column attr Group")
    };
    let edits = group_rename_edits(
        &store, Some(&idx), &FileKey::Path(lib.clone()), &local_spans, &pinned_spans, &members, "X",
    );
    assert!(
        edits.iter().any(|(l, _)| matches!(&l.key, FileKey::Path(p) if p == &app)),
        "column rename reaches the cross-file `User->search({{ name }})` arg key: {edits:?}",
    );
}

/// Event (Handler) rename. A literal event-name site is rewritable
/// and its span is the **inside-the-quotes** name (so rename keeps the quotes);
/// a folded site — variable (`my $e='connect'; on($e)`) OR constant
/// (`use constant EVT=>'connect'; on(EVT)`) — whose span IS that other
/// identifier is a reference but NOT rewritable (references lists it, rename
/// skips it, so it can't corrupt the variable/constant). `refs_to` carries the
/// distinction.
#[test]
fn test_event_handler_refs_mark_folded_site_non_rewritable() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/ev_handler.pm");
    let src = "package App;\n\
         use parent 'Mojo::EventEmitter';\n\
         use constant EVT => 'connect';\n\
         sub setup {\n\
         my $self = shift;\n\
         $self->on('connect', sub { 1 });\n\
         my $e = 'connect';\n\
         $self->on($e, sub { 1 });\n\
         $self->on(EVT, sub { 1 });\n\
         $self->emit('connect');\n\
         }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));

    let target = TargetRef {
        name: "connect".to_string(),
        kind: TargetKind::Handler {
            owner: crate::file_analysis::HandlerOwner::Class("App".to_string()),
            name: "connect".to_string(),
        },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    assert!(target.supports_cross_file_rename(), "Handler renames cross-file now");

    let refs = refs_to(&store, None, &target, RoleMask::EDITABLE);
    let lines: Vec<&str> = src.lines().collect();
    let mut rewritable = 0;
    let mut frozen = std::collections::BTreeSet::new();
    for r in &refs {
        let slice = &lines[r.span.start.row][r.span.start.column..r.span.end.column];
        if r.rewritable {
            // Quote-preservation: the rewrite is the bare name, never `'connect'`.
            assert_eq!(slice, "connect", "rewritable site must be the inner name: {r:?}");
            rewritable += 1;
        } else {
            frozen.insert(slice.to_string());
        }
    }
    assert_eq!(rewritable, 2, "the two literal `'connect'` sites: {refs:?}");
    assert_eq!(
        frozen,
        ["$e", "EVT"].iter().map(|s| s.to_string()).collect(),
        "the variable AND constant folds are frozen (kept, not rewritten): {refs:?}",
    );
}

/// A DBIC column's accessor (`$row->name`) and its key uses
/// (`search({name=>…})`, `$row->{name}`) are one renameable unit. The
/// synthesized accessor Method + the same-span `Class`-owned column HashKeyDef
/// form an attr group whose `HashKeyOfBridged` member catches the key uses
/// (`found_by` reaches the `Sub{class, verb}`-owned search args), so rename
/// from either face rewrites both. Before, accessor → `Method` and column key
/// → `HashKeyOfBridged` were disjoint.
#[test]
fn test_dbic_column_accessor_and_key_form_one_group() {
    let fa = parse(
        "package Schema::Result::User;\n\
         use base 'DBIx::Class::Core';\n\
         __PACKAGE__->add_columns(qw/id name/);\n\
         1;\n",
    );
    // Cursor on the synthesized accessor / column `name` token (row 2).
    let col = "__PACKAGE__->add_columns(qw/id ".len();
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 2, column: col }, None)
        .expect("column resolves");
    let ResolvedTarget::Group { members, .. } = resolved else {
        panic!("expected a column attr Group, got {:?}", resolved);
    };
    assert!(
        members.iter().any(|m| matches!(m.target.kind, TargetKind::Method { .. })),
        "group carries the accessor Method member: {:?}",
        members,
    );
    assert!(
        members.iter().any(|m| matches!(m.target.kind, TargetKind::HashKeyOfBridged(_))),
        "group carries the HashKeyOfBridged member (search/deref keys): {:?}",
        members,
    );
}

/// Reverse direction: a consumer-side cursor on a name-mapped
/// accessor (`$w->has_size`) OR an internal slot (`$w->{size}`) resolves to
/// the same cross-file attr group as the decl — so rename from any spelling
/// rewrites every spelling. Before, the slot fell to a plain
/// `HashKeyOfBridged` (single-file, missed the accessors) and the mapped
/// accessor only matched itself + the decl.
#[test]
fn test_consumer_mapped_accessor_and_slot_resolve_to_attr_group_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let class_path = PathBuf::from("/tmp/grp_rev_widget.pm");
    let user_path = PathBuf::from("/tmp/grp_rev_user.pl");
    let class_src =
        "package Widget;\nuse Moo;\nhas size => (is => 'rw', predicate => 1, clearer => 1);\n1;\n";
    let user_src = "use Widget;\n\
         my $w = Widget->new(size => 3);\n\
         $w->has_size;\n\
         my $d = $w->{size};\n";

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(class_path.clone(), Arc::new(parse(class_src)));
    let store = FileStore::new();
    store.insert_workspace(class_path.clone(), parse(class_src));
    store.insert_workspace(user_path.clone(), parse(user_src));

    let consumer = store.workspace_raw().get(&user_path).unwrap().value().clone();

    // Both spellings must mint the remote group (pinned to the class file) and
    // carry the accessor / ctor-key members, so a rename fans out across both.
    for (label, row, needle) in [("mapped accessor", 2usize, "has_size"), ("internal slot", 3, "{size}")] {
        let col = user_src.lines().nth(row).unwrap().find(needle).unwrap()
            + if needle.starts_with('{') { 1 } else { 0 };
        let resolved = resolve_symbol(&consumer, tree_sitter::Point { row, column: col }, Some(&idx))
            .unwrap_or_else(|| panic!("{label} cursor should resolve"));
        let ResolvedTarget::Group { pinned_spans, members, .. } = resolved else {
            panic!("{label}: expected remote Group, got {:?}", resolved);
        };
        assert!(!pinned_spans.is_empty(), "{label}: group pinned to the class file");
        assert!(
            members.iter().any(|m| !m.target.method_classes.is_empty()
                || matches!(m.target.kind, TargetKind::Method { .. } | TargetKind::HashKeyOfSub { .. })),
            "{label}: group carries accessor/ctor-key members: {:?}",
            members,
        );
    }
}

/// Internal slot pokes join the group cross-file: a subclass (or any
/// promiscuous consumer) reaching into `$self->{size}` renames with the
/// attr — under STRICT Class-owner matching, so another sub's
/// `(size => 1)` arg keys in unrelated classes stay out.
#[test]
fn test_internal_slot_pokes_join_group_cross_file() {
    let store = FileStore::new();
    let class_path = PathBuf::from("/tmp/grp_slot_widget.pm");
    let sub_path = PathBuf::from("/tmp/grp_slot_subclass.pm");
    store.insert_workspace(
        class_path.clone(),
        parse("package Widget;\nuse Moo;\nhas size => (is => 'rw');\n1;\n"),
    );
    // Subclass pokes the parent's slot directly — classic promiscuous Perl.
    store.insert_workspace(
        sub_path.clone(),
        parse("package Gadget;\nuse Moo;\nextends 'Widget';\nsub poke { my ($self) = @_; return $self->{size}; }\n1;\n"),
    );

    let class_fa = store.workspace_raw().get(&class_path).unwrap().value().clone();
    let resolved = resolve_symbol(&class_fa, tree_sitter::Point { row: 2, column: 4 }, None)
        .expect("attr decl resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group, got {:?}", resolved);
    };
    assert!(
        members.iter().any(|m| matches!(m.target.kind, TargetKind::InternalHashKey { .. })),
        "internal-key member minted: {:?}",
        members,
    );
    let edits = group_rename_edits(
        &store,
        None,
        &FileKey::Path(class_path.clone()),
        &local_spans,
        &pinned_spans,
        &members,
        "extent",
    );
    assert!(
        edits.iter().any(|(l, t)| {
            matches!(&l.key, FileKey::Path(p) if p == &sub_path) && t == "extent"
        }),
        "subclass slot poke renamed; edits: {:?}",
        edits,
    );
}

/// Old-school `bless { key => ... }, $class` keys are instance slots of the
/// class — renaming the bless key must reach every `$self->{key}` access,
/// symmetric with renaming from an access. Before the `InternalKey`
/// projection on bless keys, the from-key direction minted only the strict
/// `HashKeyOfSub{C, new}` member (matching the bless key alone) and missed
/// the `Class(C)`-owned accesses; the from-access direction worked via
/// `found_by`. This pins the symmetry.
#[test]
fn rename_from_bless_key_reaches_self_slot_accesses() {
    let store = FileStore::new();
    let path = PathBuf::from("/tmp/bless_slot.pm");
    let src = "package Calc;\n\
         sub new { my ($class) = @_; return bless { history => [] }, $class; }\n\
         sub add { my ($self) = @_; push @{$self->{history}}, 1; }\n\
         sub log { my ($self) = @_; return $self->{history}; }\n1;\n";
    store.insert_workspace(path.clone(), parse(src));

    let fa = store.workspace_raw().get(&path).unwrap().value().clone();
    // Cursor on the bless key `history` (row 1, inside `bless { history`).
    let key_col = src.lines().nth(1).unwrap().find("history").unwrap();
    let resolved = resolve_symbol(&fa, tree_sitter::Point { row: 1, column: key_col }, None)
        .expect("bless key resolves");
    let ResolvedTarget::Group { local_spans, pinned_spans, members } = resolved else {
        panic!("expected Group, got {:?}", resolved);
    };
    assert!(
        members.iter().any(|m| matches!(m.target.kind, TargetKind::InternalHashKey { .. })),
        "bless key must mint an internal-key member so $self->{{history}} accesses join: {:?}",
        members,
    );
    let edits = group_rename_edits(
        &store, None, &FileKey::Path(path.clone()),
        &local_spans, &pinned_spans, &members, "log",
    );
    // bless key (1) + two $self->{history} accesses (rows 2, 3) = 3 spellings.
    assert_eq!(
        edits.len(), 3,
        "bless key + both $self->{{history}} accesses should rename; edits: {:?}",
        edits,
    );
}

#[test]
fn test_implementations_of_role_requires_fans_out_to_composers() {
    use crate::module_index::{CachedModule, ModuleIndex};
    use std::sync::Arc;

    let idx = ModuleIndex::new_for_test();
    let insert = |name: &str, src: &str| {
        let analysis = Arc::new(parse(src));
        idx.insert_cache(
            name,
            Some(Arc::new(CachedModule::new(
                PathBuf::from(format!("/fake/{}.pm", name.replace("::", "/"))),
                analysis,
            ))),
        );
    };
    insert("My::Role", "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n1;\n");
    insert(
        "My::Composer",
        "package My::Composer;\nuse Moo;\nwith 'My::Role';\nsub fetch { 42 }\n1;\n",
    );
    // Role-composing-role: re-requires the contract (a marker, not an
    // implementation) and adds a transitive hop to reach My::Deep.
    insert(
        "My::SubRole",
        "package My::SubRole;\nuse Moo::Role;\nwith 'My::Role';\nrequires 'fetch';\n1;\n",
    );
    insert("My::Deep", "package My::Deep;\nuse Moo;\nwith 'My::SubRole';\nsub fetch { 7 }\n1;\n");

    let target = TargetRef {
        name: "fetch".to_string(),
        kind: TargetKind::Method { class: "My::Role".to_string() },
        method_classes: Vec::new(), scope: OverrideScope::Dispatch,
    };
    let origin = parse("package Probe;\n1;\n");
    let results = implementations_of(&origin, Some(&idx), &target);
    let files: Vec<String> = results
        .iter()
        .map(|r| match &r.key {
            FileKey::Path(p) => p.display().to_string(),
            FileKey::Url(u) => u.to_string(),
        })
        .collect();
    assert_eq!(
        files,
        vec!["/fake/My/Composer.pm", "/fake/My/Deep.pm"],
        "direct + transitive composer defs, sorted; the SubRole re-requires marker excluded",
    );

    // Non-Method targets have no descendant-implementation semantics.
    let pkg_target = TargetRef::new("My::Role".to_string(), TargetKind::Package);
    assert!(implementations_of(&origin, Some(&idx), &pkg_target).is_empty());
}
