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
        .find_definition(f_run_call, Some(&tree), Some(src.as_bytes()), None)
        .expect("gd on $f->run resolves");
    assert_eq!(
        gd_f.start.row, 2,
        "gd on $f->run jumped to line {} (expected 2 = Foo::run)",
        gd_f.start.row
    );

    let gd_b = fa
        .find_definition(b_run_call, Some(&tree), Some(src.as_bytes()), None)
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
    let gd = fa.find_definition(hi_call, Some(&tree), Some(src.as_bytes()), None);

    // Rename kind — for gr/rename construction.
    let target = match kind.as_ref() {
        Some(RenameKind::Function { name, package }) => TargetRef {
            name: name.clone(),
            kind: TargetKind::Sub {
                package: package.clone(),
            },
        },
        Some(RenameKind::Method { name, class }) => TargetRef {
            name: name.clone(),
            kind: TargetKind::Method {
                class: class.clone(),
            },
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
    let helper_highlights = fa.find_highlights(helper_pt, Some(&tree), Some(src.as_bytes()), None);
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
    let route_highlights = fa.find_highlights(route_pt, Some(&tree), Some(src.as_bytes()), None);
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
        None,
        None,
        Some(&idx),
    );

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
        None,
        None,
        Some(&idx),
    );

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
