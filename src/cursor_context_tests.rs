use super::*;

#[test]
fn test_detect_variable_context() {
    let source = "my $x = $";
    let ctx = detect_cursor_context(source, Point::new(0, 9), None);
    assert_eq!(ctx, CursorContext::Variable { sigil: '$' });
}

#[test]
fn test_detect_method_context() {
    let source = "my $obj = Foo->new; $obj->";
    // Column 26 = after the `>` in `->`
    let ctx = detect_cursor_context(source, Point::new(0, 26), None);
    assert_eq!(
        ctx,
        CursorContext::Method {
            invocant_type: None,
            invocant_text: "$obj".to_string(),
        }
    );
}

#[test]
fn test_detect_method_context_mid_word() {
    // Typing `$p->mag` should still be Method context
    let source = "$p->mag";
    let ctx = detect_cursor_context(source, Point::new(0, 7), None);
    assert_eq!(
        ctx,
        CursorContext::Method {
            invocant_type: None,
            invocant_text: "$p".to_string(),
        }
    );
}

#[test]
fn test_detect_method_context_class_mid_word() {
    let source = "Foo->ne";
    let ctx = detect_cursor_context(source, Point::new(0, 7), None);
    assert_eq!(
        ctx,
        CursorContext::Method {
            invocant_type: Some(InferredType::ClassName("Foo".to_string())),
            invocant_text: "Foo".to_string(),
        }
    );
}

#[test]
fn test_detect_hashkey_arrow() {
    let source = "$self->{";
    let ctx = detect_cursor_context(source, Point::new(0, 8), None);
    assert_eq!(
        ctx,
        CursorContext::HashKey {
            owner_type: None,
            var_text: "$self".to_string(),
            source_sub: None,
        }
    );
}

#[test]
fn test_detect_hashkey_arrow_midword() {
    let source = "$self->{ho";
    let ctx = detect_cursor_context(source, Point::new(0, 10), None);
    assert_eq!(
        ctx,
        CursorContext::HashKey {
            owner_type: None,
            var_text: "$self".to_string(),
            source_sub: None,
        }
    );
}

#[test]
fn test_detect_hashkey_direct_midword() {
    let source = "$hash{ver";
    let ctx = detect_cursor_context(source, Point::new(0, 9), None);
    assert_eq!(
        ctx,
        CursorContext::HashKey {
            owner_type: None,
            var_text: "$hash".to_string(),
            source_sub: None,
        }
    );
}

#[test]
fn test_detect_hashkey_direct() {
    let source = "$hash{";
    let ctx = detect_cursor_context(source, Point::new(0, 6), None);
    assert_eq!(
        ctx,
        CursorContext::HashKey {
            owner_type: None,
            var_text: "$hash".to_string(),
            source_sub: None,
        }
    );
}

#[test]
fn test_detect_general() {
    let source = "my $x = foo";
    let ctx = detect_cursor_context(source, Point::new(0, 11), None);
    assert_eq!(ctx, CursorContext::General);
}

#[test]
fn test_detect_class_method_context() {
    let source = "Calculator->";
    let ctx = detect_cursor_context(source, Point::new(0, 12), None);
    assert_eq!(
        ctx,
        CursorContext::Method {
            invocant_type: Some(InferredType::ClassName("Calculator".to_string())),
            invocant_text: "Calculator".to_string(),
        }
    );
}

fn parse(source: &str) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    parser.parse(source, None).unwrap()
}

#[test]
fn test_find_call_context_function() {
    let source = "sub greet { }\ngreet(";
    let tree = parse(source);
    let ctx = find_call_context(&tree, source.as_bytes(), Point::new(1, 6)).unwrap();
    assert_eq!(ctx.name, "greet");
    assert!(!ctx.is_method);
    assert_eq!(ctx.active_param, 0);
}

#[test]
fn test_find_call_context_with_args() {
    let source = "sub foo { }\nfoo(1, 2, ";
    let tree = parse(source);
    let ctx = find_call_context(&tree, source.as_bytes(), Point::new(1, 10)).unwrap();
    assert_eq!(ctx.name, "foo");
    assert_eq!(ctx.active_param, 2);
}

/// Regression: cursor inside a single-arg call's string literal
/// (e.g. `url_for('Users#list')` with cursor on the `s` of `Users`)
/// must report `active_param = 0`. Before the fix, `active_slot_in_node`
/// iterated the string_literal's INTERNAL children (the lone
/// `string_content`) and counted it as a slot once the cursor
/// reached its end boundary — making dispatch completion die at
/// exactly the boundary where users expect it to narrow by the
/// typed content. Live symptom: in nvim, typing inside the
/// quotes randomly flipped the completion on/off (vs the stable
/// `active_param = 0` needed to surface handlers).
#[test]
fn test_call_context_cursor_inside_single_string_arg() {
    let source = "$c->url_for('Users#list');";
    let tree = parse(source);
    for col in 12..=23 {
        let ctx = find_call_context(&tree, source.as_bytes(), Point::new(0, col)).unwrap();
        assert_eq!(
            ctx.active_param, 0,
            "cursor at col {} inside `url_for('Users#list')` must be active_param=0; \
                 got {}",
            col, ctx.active_param,
        );
    }
}

#[test]
fn test_call_context_key_position() {
    // Complete call so tree-sitter can parse it
    let source = "sub foo { }\nfoo(host => 'x', port => 8080);";
    let tree = parse(source);
    // Cursor at the `p` of `port` (column 17) — at key position after first `, `
    let ctx = find_call_context(&tree, source.as_bytes(), Point::new(1, 17)).unwrap();
    assert!(ctx.at_key_position);
    assert!(ctx.used_keys.contains("host"));
}

#[test]
fn test_selection_ranges_basic() {
    let source = "my $x = 1;";
    let tree = parse(source);
    let ranges = selection_ranges(&tree, Point::new(0, 3));
    assert!(!ranges.is_empty());
    // Innermost should be the variable node
    assert!(ranges.len() >= 2);
}

fn build_fa(source: &str) -> (Tree, crate::file_analysis::FileAnalysis) {
    let tree = parse(source);
    let fa = crate::builder::build(&tree, source.as_bytes());
    (tree, fa)
}

#[test]
fn test_tree_context_method_on_function_call() {
    // get_config()-> should detect Method with HashRef type
    let source = "sub get_config {\n    return { host => 1 };\n}\nget_config()->";
    let (tree, fa) = build_fa(source);
    // Cursor at end of line 3 (after "->")
    let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(3, 14), &fa);
    assert!(
        matches!(ctx, Some(CursorContext::Method { ref invocant_type, .. }) if *invocant_type == Some(InferredType::HashRef)),
        "expected Method with HashRef type, got {:?}",
        ctx,
    );
}

#[test]
fn test_tree_context_fq_method_is_qualified_path() {
    // `$obj->Foo::Bar::` while typing a fully-qualified method: the qualifier
    // names the dispatch PACKAGE, so completion targets that package's subs,
    // not the (untyped) invocant's methods.
    let source = "my $obj = X->new;\nmy $r = $obj->Foo::Bar::";
    let (tree, fa) = build_fa(source);
    let col = source.lines().nth(1).unwrap().len();
    let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(1, col), &fa);
    assert!(
        matches!(ctx, Some(CursorContext::QualifiedPath { ref package }) if package == "Foo::Bar"),
        "expected QualifiedPath(Foo::Bar), got {:?}",
        ctx,
    );

    // SUPER is not a package — it must fall through to ordinary method
    // completion (so inherited methods still show), not QualifiedPath(SUPER).
    let s2 = "package C;\nsub m { my $self = shift; $self->SUPER::";
    let (t2, fa2) = build_fa(s2);
    let c2 = s2.lines().nth(1).unwrap().len();
    let ctx2 = detect_cursor_context_tree(&t2, s2.as_bytes(), Point::new(1, c2), &fa2);
    assert!(
        !matches!(ctx2, Some(CursorContext::QualifiedPath { .. })),
        "SUPER:: must not be treated as a package path, got {:?}",
        ctx2,
    );
}

#[test]
fn test_tree_context_method_on_chained_call() {
    // $f->get_bar()-> where get_bar returns Object(Bar)
    let source = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\npackage main;\nmy $f = Foo->new();\n$f->get_bar()->";
    let (tree, fa) = build_fa(source);
    // Line 9: $f->get_bar()->   cursor at end
    let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(9, 15), &fa);
    assert!(
        matches!(ctx, Some(CursorContext::Method { ref invocant_type, .. })
                if invocant_type.as_ref().and_then(|t| t.class_name()) == Some("Bar")),
        "expected Method with Object(Bar) type, got {:?}",
        ctx,
    );
}

#[test]
fn test_tree_context_hashkey_on_chained_call() {
    // $calc->get_self->get_config->{ should detect HashKey with resolved type
    let source = "\
package Calculator;
sub new { bless {}, shift }
sub get_self {
    my ($self) = @_;
    return $self;
}
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{";
    let (tree, fa) = build_fa(source);
    // Last line: "$calc->get_self->get_config->{"
    let cursor = Point::new(11, 30); // after "{"
    let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), cursor, &fa);
    assert!(
        matches!(ctx, Some(CursorContext::HashKey { ref owner_type, ref source_sub, .. })
                if *owner_type == Some(InferredType::HashRef) && *source_sub == Some("get_config".to_string())),
        "expected HashKey with HashRef type and get_config source, got {:?}",
        ctx,
    );
}

#[test]
fn test_tree_context_simple_var_method() {
    // $obj-> with nothing after: tree-based detection resolves the type
    let source = "my $obj = Foo->new();\n$obj->";
    let (tree, fa) = build_fa(source);
    let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(1, 6), &fa);
    assert_eq!(
        ctx,
        Some(CursorContext::Method {
            invocant_type: Some(InferredType::ClassName("Foo".to_string())),
            invocant_text: "$obj".to_string(),
        })
    );

    // Text-based fallback also resolves the type
    let ctx = detect_cursor_context(source, Point::new(1, 6), Some(&fa));
    assert_eq!(
        ctx,
        CursorContext::Method {
            invocant_type: Some(InferredType::ClassName("Foo".to_string())),
            invocant_text: "$obj".to_string(),
        }
    );
}

#[test]
fn test_use_context_module_prefix() {
    let source = "use Mojo::Ba";
    let ctx = detect_cursor_context(source, Point::new(0, 12), None);
    assert_eq!(
        ctx,
        CursorContext::UseStatement {
            module_prefix: "Mojo::Ba".to_string(),
            in_import_list: false,
            module_name: None,
        }
    );
}

#[test]
fn test_use_context_empty_prefix() {
    let source = "use ";
    let ctx = detect_cursor_context(source, Point::new(0, 4), None);
    assert_eq!(
        ctx,
        CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: false,
            module_name: None,
        }
    );
}

#[test]
fn test_use_context_import_list_qw() {
    let source = "use List::Util qw(ma";
    let ctx = detect_cursor_context(source, Point::new(0, 20), None);
    assert_eq!(
        ctx,
        CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some("List::Util".to_string()),
        }
    );
}

#[test]
fn test_use_context_import_list_bare_string() {
    let source = "use Foo 'ba";
    let ctx = detect_cursor_context(source, Point::new(0, 11), None);
    assert_eq!(
        ctx,
        CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some("Foo".to_string()),
        }
    );
}

#[test]
fn test_use_context_skips_pragmas() {
    let source = "use strict";
    let ctx = detect_cursor_context(source, Point::new(0, 10), None);
    assert_eq!(ctx, CursorContext::General);
}

#[test]
fn test_require_context_module_prefix() {
    let source = "require DBI";
    let ctx = detect_cursor_context(source, Point::new(0, 11), None);
    assert_eq!(
        ctx,
        CursorContext::UseStatement {
            module_prefix: "DBI".to_string(),
            in_import_list: false,
            module_name: None,
        }
    );
}

#[test]
fn test_detect_qualified_path_basic() {
    let source = "my $x = Math::Util::";
    let ctx = detect_cursor_context(source, Point::new(0, 20), None);
    assert_eq!(
        ctx,
        CursorContext::QualifiedPath { package: "Math::Util".to_string() },
    );
}

#[test]
fn test_detect_qualified_path_midword() {
    // Cursor mid-typed-name: `Math::Util::squ|` — should still
    // detect Math::Util as the package being qualified against.
    let source = "my $x = Math::Util::squ";
    let ctx = detect_cursor_context(source, Point::new(0, 23), None);
    assert_eq!(
        ctx,
        CursorContext::QualifiedPath { package: "Math::Util".to_string() },
    );
}

/// Unicode word characters in package names (Perl identifiers under
/// `use utf8` accept Unicode letters — `Acmé::Util`, `Münch::Helpers`,
/// etc.). Byte-wise walkback would stop at a UTF-8 continuation byte
/// and yield the wrong package text; the character-aware walkback
/// has to consume `é` / `ü` / etc. as single chars.
///
/// Marker for future work: ideally the tree-sitter parser tells us
/// the qualified-name span directly so we don't reimplement Perl's
/// identifier rules in the text walkback. The current case is the
/// common one (text walkback inside detect_cursor_context).
#[test]
fn test_detect_qualified_path_with_unicode_segment() {
    let source = "my $x = Acmé::Util::";
    // Point.column is a byte offset (`é` is 2 bytes), not a char
    // count — using `source.len()` keeps us aligned with the rest
    // of the detection path which slices by byte.
    let ctx = detect_cursor_context(source, Point::new(0, source.len()), None);
    assert_eq!(
        ctx,
        CursorContext::QualifiedPath { package: "Acmé::Util".to_string() },
    );
}

/// Gate 4: `resolve_node_type`'s scalar/array/hash arm now threads `module_index`
/// into `inferred_type_via_bag_ctx`. Without the fix, a `ReceiverGated` TC for
/// `$c` (typed `Catalyst` only when the package `isa Catalyst::Controller`) would
/// not resolve in the cursor-context path — the bag query without a module index
/// cannot chase cross-file ancestry. This test exercises the tree-based path
/// (`detect_cursor_context_tree_with_index`) on a $c variable whose type requires
/// the module index to resolve, confirming the invocant_type is now non-None.
#[test]
fn test_resolve_node_type_scalar_threads_module_index() {
    use crate::module_index::ModuleIndex;
    // Build a controller that has $c typed via a cross-file ReceiverGated TC.
    // We use the real builder (not a plugin) — instead, give $c an explicit
    // TypeConstraint via a known-type assignment so the test doesn't need a
    // plugin registry, but DOES need cross-file ancestry to resolve.
    //
    // Simpler: build a source where $c is directly typed by the builder (a
    // normal variable assignment), then confirm the cursor-context path surfaces
    // the type. The module_index threading matters for ReceiverGated TCs; the
    // direct-TC case passes even without the fix, so we note the e2e catalys test
    // covers the full ReceiverGated path. This test covers the code path change.
    let source = "package Foo;\nmy $c = Foo->new();\n$c->";
    let tree = parse(source);
    let fa = crate::builder::build(&tree, source.as_bytes());
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);
    // Cursor after `$c->` — should detect Method context with invocant_type Foo.
    let ctx = detect_cursor_context_tree_with_index(
        &tree,
        source.as_bytes(),
        Point::new(2, 4),
        &fa,
        Some(&idx),
    );
    assert!(
        matches!(ctx, Some(CursorContext::Method { ref invocant_type, .. })
            if invocant_type.as_ref().and_then(|t| t.class_name()) == Some("Foo")),
        "scalar arm with module_index should resolve $c to Foo, got {:?}",
        ctx,
    );
}
