//! Unified query surface across FileStore + ModuleIndex.
//!
//! All cross-file LSP queries (references, rename, workspace/symbol) route
//! through this module so that each handler is a one-liner against a single
//! role-masked function instead of reinventing the per-tier walk.
//!
//! As of phase 4, `refs_to` collects references to a named target across the
//! callers-visible set of files. `resolve_symbol` is left as future work — the
//! current handlers still use cursor-position tree resolution to identify the
//! target, then call into `refs_to` for the cross-file collection.

use std::path::PathBuf;

use tower_lsp::lsp_types::Url;

use crate::file_analysis::{AccessKind, FileAnalysis, HandlerOwner, RefKind, Span, SymKind};
use crate::file_store::{FileKey, FileStore};
use crate::module_index::ModuleIndex;

bitflags::bitflags! {
    /// Which file roles a query should search. Handlers pick the mask that
    /// fits their semantics: rename is EDITABLE (skip deps, they're read-only);
    /// references is VISIBLE (include deps, read-only reads are fine).
    pub struct RoleMask: u8 {
        const OPEN       = 1 << 0;
        const WORKSPACE  = 1 << 1;
        const DEPENDENCY = 1 << 2;
        const BUILTIN    = 1 << 3;

        const EDITABLE = Self::OPEN.bits() | Self::WORKSPACE.bits();
        const VISIBLE  = Self::OPEN.bits() | Self::WORKSPACE.bits() | Self::DEPENDENCY.bits() | Self::BUILTIN.bits();
    }
}

/// Identifies what we're collecting references to.
#[derive(Debug, Clone)]
pub struct TargetRef {
    pub name: String,
    pub kind: TargetKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetKind {
    /// A sub defined in a specific package. `None` = the sub has
    /// no package context (top-level script). Matches `Sub`/`Method`
    /// symbols whose `package` field equals this, and `FunctionCall`
    /// refs whose `resolved_package` equals this. Package-scoping
    /// mirrors method's class-scoping — name-only matching
    /// cross-links `Foo::run` and `Bar::run`.
    Sub { package: Option<String> },
    /// A method on a specific class. Matches `Sub`/`Method` symbols
    /// whose `package == class`, and `MethodCall` refs whose invocant
    /// resolves to `class`.
    Method { class: String },
    /// A package/class/module name — matches PackageRef refs.
    Package,
    /// A hash key owned by a specific sub's return value. `package` is the
    /// sub's defining package (or None for top-level / unpackaged subs);
    /// matches `HashKeyOwner::Sub { package, name }` by structural equality.
    HashKeyOfSub { package: Option<String>, name: String },
    /// A hash key owned by a class (Moo `has` slots, DBIC columns on a Result class).
    HashKeyOfClass(String),
    /// A `Handler` symbol registered on a class (Mojo events, Dancer
    /// routes, etc.). Both the definition (`Handler` symbol) and call
    /// sites (`DispatchCall` refs) match; stacked registrations all
    /// surface separately so features can enumerate every handler.
    Handler {
        owner: HandlerOwner,
        name: String,
    },
}

/// A located reference in some file.
#[derive(Debug, Clone)]
pub struct RefLocation {
    pub key: FileKey,
    pub span: Span,
    /// Read/Write/Declaration — used by document_highlight callers that will
    /// migrate to `refs_to` in a follow-up.
    #[allow(dead_code)]
    pub access: AccessKind,
}

impl RefLocation {
    pub fn to_url(&self) -> Option<Url> {
        match &self.key {
            FileKey::Url(u) => Some(u.clone()),
            FileKey::Path(p) => Url::from_file_path(p).ok(),
        }
    }
}

/// Collect every reference to `target` across the masked file set.
///
/// - `files`   — open + workspace store
/// - `module_index` — dep cache (consulted only if mask includes Dependency)
pub fn refs_to(
    files: &FileStore,
    module_index: Option<&ModuleIndex>,
    target: &TargetRef,
    mask: RoleMask,
) -> Vec<RefLocation> {
    let mut out = Vec::new();

    // Open files (canonical — workspace entries for open paths are skipped).
    let mut covered_paths: std::collections::HashSet<PathBuf> = std::collections::HashSet::new();
    if mask.contains(RoleMask::OPEN) {
        files.for_each_open_mut(|url, doc| {
            let url = url.clone();
            if let Ok(p) = url.to_file_path() {
                covered_paths.insert(p);
            }
            collect_from_analysis(&FileKey::Url(url), &doc.analysis, target, &mut out);
        });
    } else {
        // Even if open isn't in the mask, track the paths so a WORKSPACE walk
        // doesn't duplicate them (an open file's pre-close state isn't meaningful).
        files.for_each_open_mut(|url, _doc| {
            if let Ok(p) = url.to_file_path() {
                covered_paths.insert(p);
            }
        });
    }

    // Workspace files.
    if mask.contains(RoleMask::WORKSPACE) {
        for entry in files.workspace_raw().iter() {
            if covered_paths.contains(entry.key()) {
                continue;
            }
            collect_from_analysis(&FileKey::Path(entry.key().clone()), entry.value(), target, &mut out);
        }
    }

    // Dependencies (read-only modules from @INC).
    if mask.contains(RoleMask::DEPENDENCY) {
        if let Some(idx) = module_index {
            idx.for_each_cached(|_module_name, cached| {
                let key = FileKey::Path(cached.path.clone());
                collect_from_analysis(&key, &cached.analysis, target, &mut out);
            });
        }
    }

    // Sort for stable output, dedupe by (path, span).
    out.sort_by(|a, b| {
        key_for_sort(&a.key)
            .cmp(&key_for_sort(&b.key))
            .then_with(|| {
                (a.span.start.row, a.span.start.column)
                    .cmp(&(b.span.start.row, b.span.start.column))
            })
    });
    out.dedup_by(|a, b| file_key_eq(&a.key, &b.key) && a.span == b.span);
    out
}

fn key_for_sort(k: &FileKey) -> PathBuf {
    match k {
        FileKey::Path(p) => p.clone(),
        FileKey::Url(u) => u.to_file_path().unwrap_or_else(|_| PathBuf::from(u.as_str())),
    }
}

fn file_key_eq(a: &FileKey, b: &FileKey) -> bool {
    key_for_sort(a) == key_for_sort(b)
}

fn collect_from_analysis(
    key: &FileKey,
    analysis: &FileAnalysis,
    target: &TargetRef,
    out: &mut Vec<RefLocation>,
) {
    use crate::file_analysis::{HashKeyOwner, SymbolDetail};

    // Include declaration spans when this file defines the target.
    for sym in &analysis.symbols {
        if sym.name != target.name {
            continue;
        }
        // Treat a sub and a method in the same package as the same
        // callable — Perl's only distinction between them is call
        // shape. `Sub { package }` matches exactly that scope (None
        // = top-level script sub); `Method { class }` is the
        // `Sub { package: Some(class) }` case with stricter intent.
        let callable_scope: Option<Option<String>> = match &target.kind {
            TargetKind::Sub { package } => Some(package.clone()),
            TargetKind::Method { class } => Some(Some(class.clone())),
            _ => None,
        };
        let matches_kind = match &target.kind {
            TargetKind::Sub { .. } | TargetKind::Method { .. } => {
                let scope = callable_scope.as_ref().unwrap();
                matches!(sym.kind, SymKind::Sub | SymKind::Method)
                    && sym.package == *scope
            }
            TargetKind::Package => matches!(
                sym.kind,
                SymKind::Package | SymKind::Class | SymKind::Module
            ),
            TargetKind::HashKeyOfSub { package, name } => matches!(
                &sym.detail,
                SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { package: op, name: on },
                    ..
                } if op == package && on == name
            ),
            TargetKind::HashKeyOfClass(wanted) => matches!(
                &sym.detail,
                SymbolDetail::HashKeyDef { owner: HashKeyOwner::Class(n), .. } if n == wanted
            ),
            TargetKind::Handler { owner, name: hname } => {
                sym.name == *hname && matches!(
                    &sym.detail,
                    SymbolDetail::Handler { owner: o, .. } if o == owner
                )
            }
        };
        if matches_kind {
            out.push(RefLocation {
                key: key.clone(),
                span: sym.selection_span,
                access: AccessKind::Declaration,
            });
        }
    }

    // Collect usage refs.
    let callable_scope_for_refs: Option<Option<String>> = match &target.kind {
        TargetKind::Sub { package } => Some(package.clone()),
        TargetKind::Method { class } => Some(Some(class.clone())),
        _ => None,
    };
    for r in &analysis.refs {
        if r.target_name != target.name {
            continue;
        }
        // Sub + Method both match any call into that scope — function
        // or method shape — per the "same callable, two shapes"
        // invariant. Filter is a single scope comparison.
        let matches_kind = match (&target.kind, &r.kind) {
            (TargetKind::Sub { .. } | TargetKind::Method { .. },
             RefKind::FunctionCall { resolved_package }) => {
                let scope = callable_scope_for_refs.as_ref().unwrap();
                resolved_package == scope
            }
            (TargetKind::Sub { .. } | TargetKind::Method { .. },
             RefKind::MethodCall { invocant_class, .. }) => {
                // Method-shaped call: match only when the ref's
                // invocant resolved to the target scope. No text
                // fallback — unresolved invocants are excluded rather
                // than cross-linked. (Build-time
                // `resolve_invocant_class_tree` handles chains.)
                let scope = callable_scope_for_refs.as_ref().unwrap();
                match (invocant_class, scope) {
                    (Some(cn), Some(pkg)) => cn == pkg,
                    _ => false, // package-less sub or unpinned method
                }
            }
            (TargetKind::Package, RefKind::PackageRef) => true,
            (
                TargetKind::HashKeyOfSub { package, name },
                RefKind::HashKeyAccess { owner, .. },
            ) => matches!(
                owner,
                Some(HashKeyOwner::Sub { package: op, name: on })
                    if op == package && on == name
            ),
            (TargetKind::HashKeyOfClass(wanted), RefKind::HashKeyAccess { owner, .. }) => {
                matches!(owner, Some(HashKeyOwner::Class(n)) if n == wanted)
            }
            (TargetKind::Handler { owner, name: hname },
             RefKind::DispatchCall { owner: ref_owner, .. }) => {
                r.target_name == *hname
                    && matches!(ref_owner, Some(o) if o == owner)
            }
            _ => false,
        };
        if matches_kind {
            out.push(RefLocation {
                key: key.clone(),
                span: r.span,
                access: r.access,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse(source: &str) -> FileAnalysis {
        use tree_sitter::Parser;
        let mut parser = Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
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
                kind: TargetKind::Sub { package: Some("A".to_string()) },
            },
            RoleMask::EDITABLE,
        );

        // Expect at least the decl in A and the call in B.
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)
                && r.access == AccessKind::Declaration),
            "expected declaration of foo in file A, got {:?}", results,
        );
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)
                && r.access == AccessKind::Read),
            "expected call to foo in file B, got {:?}", results,
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

        let fa = parse(r#"
package MyApp;
use Mojolicious::Lite;

$app->helper('users.create', sub ($c, $user) {});
$app->routes->post('/users')->to(controller => 'Users', action => 'create');
"#);
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
                r.span.start.row, col,
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
                r.span.start.row, r.span.start.column,
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

        let fa = parse(r#"
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
"#);
        store.insert_workspace(path.clone(), fa);

        let foo_results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "run".to_string(),
                kind: TargetKind::Method { class: "Foo".to_string() },
            },
            RoleMask::EDITABLE,
        );
        // Must include Foo::run decl and `$f->run` call. Must NOT
        // include Bar::run decl or `$b->run` call.
        let foo_lines: Vec<usize> = foo_results.iter().map(|r| r.span.start.row).collect();
        assert!(foo_lines.contains(&3),  // `sub run` in package Foo
            "Foo::run decl (line 3) missing from Foo results: {:?}", foo_lines);
        assert!(foo_lines.contains(&12), // `$f->run` call
            "$f->run call (line 12) missing from Foo results: {:?}", foo_lines);
        assert!(!foo_lines.contains(&7), // `sub run` in package Bar
            "Bar::run decl (line 7) wrongly included in Foo results: {:?}", foo_lines);
        assert!(!foo_lines.contains(&13), // `$b->run` call
            "$b->run call (line 13) wrongly included in Foo results: {:?}", foo_lines);

        let bar_results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "run".to_string(),
                kind: TargetKind::Method { class: "Bar".to_string() },
            },
            RoleMask::EDITABLE,
        );
        let bar_lines: Vec<usize> = bar_results.iter().map(|r| r.span.start.row).collect();
        assert!(bar_lines.contains(&7),
            "Bar::run decl (line 7) missing from Bar results: {:?}", bar_lines);
        assert!(bar_lines.contains(&13),
            "$b->run call (line 13) missing from Bar results: {:?}", bar_lines);
        assert!(!bar_lines.contains(&3),
            "Foo::run decl (line 3) wrongly included in Bar results: {:?}", bar_lines);
        assert!(!bar_lines.contains(&12),
            "$f->run call (line 12) wrongly included in Bar results: {:?}", bar_lines);
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

        let fa = parse(r#"use v5.38;

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
"#);
        store.insert_workspace(path.clone(), fa);

        let sner_results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "hi".to_string(),
                kind: TargetKind::Method { class: "Sner".to_string() },
            },
            RoleMask::EDITABLE,
        );
        let sner_lines: Vec<usize> = sner_results.iter().map(|r| r.span.start.row).collect();
        assert!(sner_lines.contains(&3),
            "Sner::hi decl missing: {:?}", sner_lines);
        assert!(sner_lines.contains(&11),
            "$s->hi (variable-bound) missing: {:?}", sner_lines);
        assert!(sner_lines.contains(&13),
            "Sner->new->hi (inline chain) missing — chain invocant resolution \
             via build-time invocant_class should cover this: {:?}", sner_lines);
        // No Bler anywhere in Sner results.
        assert!(!sner_lines.contains(&6),
            "Bler::hi decl wrongly in Sner results: {:?}", sner_lines);
        assert!(!sner_lines.contains(&12),
            "$b->hi wrongly in Sner results: {:?}", sner_lines);
        assert!(!sner_lines.contains(&14),
            "Bler->new->hi wrongly in Sner results: {:?}", sner_lines);

        let bler_results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "hi".to_string(),
                kind: TargetKind::Method { class: "Bler".to_string() },
            },
            RoleMask::EDITABLE,
        );
        let bler_lines: Vec<usize> = bler_results.iter().map(|r| r.span.start.row).collect();
        assert!(bler_lines.contains(&6),
            "Bler::hi decl missing: {:?}", bler_lines);
        assert!(bler_lines.contains(&12),
            "$b->hi missing: {:?}", bler_lines);
        assert!(bler_lines.contains(&14),
            "Bler->new->hi (inline chain) missing: {:?}", bler_lines);
        assert!(!bler_lines.contains(&3),
            "Sner::hi decl wrongly in Bler results: {:?}", bler_lines);
        assert!(!bler_lines.contains(&11),
            "$s->hi wrongly in Bler results: {:?}", bler_lines);
        assert!(!bler_lines.contains(&13),
            "Sner->new->hi wrongly in Bler results: {:?}", bler_lines);
    }

    /// ALL FOUR LSP paths (hover, gd, gr, rename) must class-scope
    /// for two packages sharing a method name. Five different resolvers
    /// exist in the codebase:
    ///
    ///   1. `Builder::resolve_invocant_class_tree` — populates
    ///      `RefKind::MethodCall.invocant_class` at build time.
    ///   2. `FileAnalysis::resolve_method_invocant` — tree-based,
    ///      used by `find_definition` and `hover_info`.
    ///   3. `FileAnalysis::resolve_invocant_class` — text fallback
    ///      used by resolve_method_invocant.
    ///   4. `FileAnalysis::invocant_text_to_class` — used by
    ///      `rename_kind_at` and `refs_to` fallback.
    ///   5. `FileAnalysis::rename_method_in_class` — my new one;
    ///      filters rename edits by class.
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
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let fa = crate::builder::build(&tree, src.as_bytes());

        // Line/col positions (0-indexed):
        //   line 2, col 4  — `sub run` decl in Foo
        //   line 6, col 4  — `sub run` decl in Bar
        //   line 11, col 4 — `$f->run` (cursor on "run"). Line text:
        //                    "$f->run;" → cols 0..1 = "$f", 2..3 = "->",
        //                    4..6 = "run".
        //   line 12, col 4 — `$b->run` (cursor on "run").
        let run_in_foo_decl = tree_sitter::Point { row: 2, column: 4 };
        let run_in_bar_decl = tree_sitter::Point { row: 6, column: 4 };
        let f_run_call = tree_sitter::Point { row: 11, column: 4 };
        let b_run_call = tree_sitter::Point { row: 12, column: 4 };

        // ---- (1) hover — cursor on `$f->run`. Must mention Foo, not Bar.
        let hover = fa.hover_info(f_run_call, src, Some(&tree), None)
            .expect("hover on $f->run returns something");
        assert!(hover.contains("Foo"),
            "hover on $f->run should mention Foo; got: {:?}", hover);
        assert!(!hover.contains("Bar"),
            "hover on $f->run leaked Bar into the content: {:?}", hover);

        // Mirror: hover on $b->run mentions Bar, not Foo.
        let hover_b = fa.hover_info(b_run_call, src, Some(&tree), None)
            .expect("hover on $b->run returns something");
        assert!(hover_b.contains("Bar"),
            "hover on $b->run should mention Bar; got: {:?}", hover_b);
        assert!(!hover_b.contains("Foo"),
            "hover on $b->run leaked Foo into the content: {:?}", hover_b);

        // ---- (2) goto-def — $f->run must jump to Foo::run (line 2), not Bar::run (line 6).
        let gd_f = fa.find_definition(f_run_call, Some(&tree), Some(src.as_bytes()), None)
            .expect("gd on $f->run resolves");
        assert_eq!(gd_f.start.row, 2,
            "gd on $f->run jumped to line {} (expected 2 = Foo::run)", gd_f.start.row);

        let gd_b = fa.find_definition(b_run_call, Some(&tree), Some(src.as_bytes()), None)
            .expect("gd on $b->run resolves");
        assert_eq!(gd_b.start.row, 6,
            "gd on $b->run jumped to line {} (expected 6 = Bar::run)", gd_b.start.row);

        // ---- (3) references — via rename_kind_at → TargetRef → refs_to.
        let target_from_f = match fa.rename_kind_at(f_run_call) {
            Some(RenameKind::Method { name, class }) => TargetRef {
                name,
                kind: TargetKind::Method { class },
            },
            other => panic!("rename_kind_at($f->run) should be Method{{class=Foo}}, got {:?}", other),
        };
        // Verify the class field was populated as Foo (not Bar, not missing).
        if let TargetKind::Method { ref class } = target_from_f.kind {
            assert_eq!(class, "Foo",
                "rename_kind_at($f->run) resolved class as {:?}, expected Foo", class);
        }

        // ---- (4) rename — BEFORE moving `fa` into the store. Rename
        // Foo::run to renamed_run. Must edit Foo::run decl + $f->run
        // call, NOT Bar::run or $b->run.
        let edits = fa.rename_method_in_class("run", "Foo", "renamed_run");
        let edit_lines: Vec<usize> = edits.iter().map(|(span, _)| span.start.row).collect();
        assert!(edit_lines.contains(&2),
            "rename Foo::run missed the decl: {:?}", edit_lines);
        assert!(edit_lines.contains(&11),
            "rename Foo::run missed the $f->run call: {:?}", edit_lines);
        assert!(!edit_lines.contains(&6),
            "rename Foo::run wrongly rewrote Bar::run decl: {:?}", edit_lines);
        assert!(!edit_lines.contains(&12),
            "rename Foo::run wrongly rewrote $b->run call: {:?}", edit_lines);

        // Move `fa` into the store for the refs_to walk.
        let store = FileStore::new();
        let path = PathBuf::from("/tmp/lsp_paths_fixture.pm");
        store.insert_workspace(path.clone(), fa);

        let foo_refs = refs_to(&store, None, &target_from_f, RoleMask::EDITABLE);
        let foo_lines: Vec<usize> = foo_refs.iter().map(|r| r.span.start.row).collect();
        assert!(foo_lines.contains(&2),  // Foo::run decl
            "gr(Foo::run) missed decl: {:?}", foo_lines);
        assert!(foo_lines.contains(&11), // $f->run call
            "gr(Foo::run) missed $f->run call: {:?}", foo_lines);
        assert!(!foo_lines.contains(&6), // Bar::run decl
            "gr(Foo::run) wrongly included Bar::run decl: {:?}", foo_lines);
        assert!(!foo_lines.contains(&12), // $b->run call
            "gr(Foo::run) wrongly included $b->run call: {:?}", foo_lines);
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
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let fa = crate::builder::build(&tree, src.as_bytes());

        // Positions (0-indexed rows; blank lines count):
        //   row 2  — `    sub hi {}` in Sner
        //   row 6  — `    sub hi {}` in Bler
        //   row 9  — `    sub hi {}` in Xler (unreachable — never imported)
        //   row 15 — `hi()` call
        let hi_call = tree_sitter::Point { row: 15, column: 0 };

        let kind = fa.rename_kind_at(hi_call);
        let hover = fa.hover_info(hi_call, src, Some(&tree), None);
        let gd = fa.find_definition(hi_call, Some(&tree), Some(src.as_bytes()), None);

        // Rename kind — for gr/rename construction.
        let target = match kind.as_ref() {
            Some(RenameKind::Function { name, package }) => TargetRef {
                name: name.clone(),
                kind: TargetKind::Sub { package: package.clone() },
            },
            Some(RenameKind::Method { name, class }) => TargetRef {
                name: name.clone(),
                kind: TargetKind::Method { class: class.clone() },
            },
            other => panic!("unexpected rename_kind_at = {:?}", other),
        };

        let rename_edits = match &kind {
            Some(RenameKind::Function { name, package }) =>
                fa.rename_sub_in_package(name, package, "renamed_hi"),
            Some(RenameKind::Method { name, class }) =>
                fa.rename_method_in_class(name, class, "renamed_hi"),
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
                 (should mention Bler; must not mention Sner or Xler)", h)),
            None => failures.push("hover on `hi()` returned None".into()),
        }

        // (2) gd: must jump to Bler::hi (row 6). Not Sner::hi (row 2), not Xler::hi (row 9).
        match gd {
            Some(s) if s.start.row == 6 => { /* ok */ }
            Some(s) if s.start.row == 2 =>
                failures.push(format!("gd jumped to Sner::hi (row 2) — \
                    should follow imports to Bler::hi (row 6)")),
            Some(s) if s.start.row == 9 =>
                failures.push(format!("gd jumped to Xler::hi (row 9) — \
                    Xler isn't imported, it should be ignored")),
            Some(s) => failures.push(format!(
                "gd jumped to row {} — expected row 6 (Bler::hi)", s.start.row)),
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
            failures.push(format!("gr wrongly unioned Sner::hi decl at row 2: {:?}", ref_rows));
        }
        if ref_rows.contains(&9) {
            failures.push(format!("gr wrongly unioned Xler::hi decl at row 9: {:?}", ref_rows));
        }

        // (4) rename: should rewrite Bler::hi + hi() call; NOT Sner::hi or Xler::hi.
        let rename_rows: Vec<usize> = rename_edits.iter().map(|(s, _)| s.start.row).collect();
        if !rename_rows.contains(&15) {
            failures.push(format!("rename missed hi() call at row 15: {:?}", rename_rows));
        }
        if !rename_rows.contains(&6) {
            failures.push(format!("rename missed Bler::hi decl at row 6: {:?}", rename_rows));
        }
        if rename_rows.contains(&2) {
            failures.push(format!("rename wrongly rewrote Sner::hi decl at row 2: {:?}", rename_rows));
        }
        if rename_rows.contains(&9) {
            failures.push(format!("rename wrongly rewrote Xler::hi decl at row 9: {:?}", rename_rows));
        }

        assert!(
            failures.is_empty(),
            "import-graph-aware resolution broken across paths:\n  - {}",
            failures.join("\n  - "),
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

        let fa_lib = parse(
            "package Lib;\nsub get_config { return { host => 1, port => 2 } }\n1;\n",
        );
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
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_lib)),
            "expected HashKeyDef match in Lib, got {:?}", results,
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
        let fa_a = parse(
            "package Alpha;\nsub get_config { return { host => 'alpha' } }\n1;\n",
        );
        store.insert_workspace(path_a.clone(), fa_a);

        let fa_b = parse(
            "package Beta;\nsub get_config { return { host => 'beta' } }\n1;\n",
        );
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
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)),
            "expected Alpha hit, got {:?}", results,
        );
        assert!(
            !results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)),
            "Beta's get_config must NOT show up in Alpha's references, got {:?}", results,
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
}
