//! Tiebreak / shape pins for `FileAnalysis::call_ref_by_start` —
//! the start-point → call-shaped-ref index that powers chain
//! receiver dispatch in `method_call_invocant_class`.
//!
//! The index is small but its rules are subtle: only MethodCall +
//! FunctionCall refs go in; identical-start collisions are won by
//! smallest span; equal spans first-write-wins; recursion needs the
//! `is_self` pointer guard so a ref doesn't chase itself. This file
//! pins each rule with a targeted test so future refactors of the
//! invocant resolver can't silently break one without lighting up
//! a specific failure.

use super::*;
use tree_sitter::{Parser, Point};

fn parse(source: &str) -> FileAnalysis {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

/// Find the index of the (last) ref whose `target_name` matches and
/// whose kind matches the predicate. Test helper — multiple refs may
/// share a name across a chain (e.g. `Foo->new->new` would have two
/// `new` MethodCalls), but our fixtures keep names unique.
fn ref_idx<F: Fn(&RefKind) -> bool>(fa: &FileAnalysis, name: &str, kind_pred: F) -> usize {
    fa.refs
        .iter()
        .enumerate()
        .find(|(_, r)| r.target_name == name && kind_pred(&r.kind))
        .map(|(i, _)| i)
        .unwrap_or_else(|| {
            panic!(
                "no ref with target_name={:?} matching predicate; refs: {:?}",
                name,
                fa.refs.iter().map(|r| (&r.target_name, &r.kind)).collect::<Vec<_>>(),
            )
        })
}

fn is_method_call(k: &RefKind) -> bool {
    matches!(k, RefKind::MethodCall { .. })
}

fn is_function_call(k: &RefKind) -> bool {
    matches!(k, RefKind::FunctionCall { .. })
}

/// `Foo->new->bar()`: outer `bar`'s `invocant_span` starts at the
/// same point as the inner `Foo->new` call's start. The index must
/// resolve that start point to the **inner** `new` ref, not the
/// outer `bar` (smallest-span wins). This is the rule that makes
/// chain dispatch in `method_call_invocant_class` recurse onto the
/// genuine receiver instead of looping back on itself.
#[test]
fn smallest_span_wins_method_chain() {
    let fa = parse(
        r#"
package Foo;
sub new { bless {}, shift }
sub bar { 1 }
package main;
Foo->new->bar();
"#,
    );

    let bar_idx = ref_idx(&fa, "bar", is_method_call);
    let new_idx = ref_idx(&fa, "new", is_method_call);

    let bar = &fa.refs[bar_idx];
    let RefKind::MethodCall { invocant_span: Some(bar_invocant_span), .. } = &bar.kind else {
        panic!("bar is a MethodCall");
    };

    // Both `bar` (outer) and `new` (inner) have call-spans starting
    // at the same point as the chain's leftmost token (`Foo`'s
    // start, which is also `bar.invocant_span.start`).
    let key = bar_invocant_span.start;
    let resolved = fa.call_ref_by_start.get(&key).copied();

    assert_eq!(
        resolved,
        Some(new_idx),
        "lookup at the chain's leftmost point must return the \
         inner `new` MethodCall (smaller span), not the outer `bar`. \
         got: {:?} (bar={}, new={})",
        resolved, bar_idx, new_idx,
    );

    // And critically: not the outer ref itself. If this fires, the
    // `is_self` guard in `method_call_invocant_class` is the only
    // thing protecting against an infinite recursion.
    assert_ne!(resolved, Some(bar_idx));
}

/// `make_b()->touch()`: outer `touch`'s `invocant_span` starts at
/// the same point as the `make_b` FunctionCall ref's span. The
/// `FunctionCall` ref's span covers just the function-name node
/// (`make_b`), which is strictly contained in the outer
/// MethodCall's span — so smallest-span wins picks the
/// FunctionCall.
///
/// This is what lets `method_call_invocant_class` chase
/// `make_b()->touch()` back to `make_b`'s zero-arg return type
/// without depending on any cached field.
#[test]
fn smallest_span_wins_function_call_inner() {
    let fa = parse(
        r#"
sub make_b { bless {}, 'B' }
make_b()->touch();
"#,
    );

    let touch_idx = ref_idx(&fa, "touch", is_method_call);
    let make_b_idx = ref_idx(&fa, "make_b", is_function_call);

    let touch = &fa.refs[touch_idx];
    let RefKind::MethodCall { invocant_span: Some(touch_invocant_span), .. } = &touch.kind else {
        panic!("touch is a MethodCall");
    };

    let resolved = fa.call_ref_by_start.get(&touch_invocant_span.start).copied();
    assert_eq!(
        resolved,
        Some(make_b_idx),
        "lookup at touch's invocant_span.start must return the \
         `make_b` FunctionCall ref (narrower span). got: {:?}",
        resolved,
    );
}

/// Only call-shaped refs (MethodCall, FunctionCall) enter the
/// index. Variable / HashKeyAccess / ContainerAccess refs sharing
/// the same start point don't.
///
/// `$x->m(); $x->{k}`: the `$x->{k}` access produces a
/// HashKeyAccess + ContainerAccess pair at `$x`'s start, neither
/// of which is call-shaped. Chain dispatch must never hand off to
/// a non-call kind, so we cross-check every entry the parser
/// produces against the kind invariant. (PackageRef shape — the
/// other natural source of "non-call ref at chain start" — turns
/// out not to be emitted by tree-sitter-perl for the bareword in
/// `Foo->m()`; the outer MethodCall already covers that span. The
/// invariant still holds, just with one less natural collider.)
#[test]
fn non_call_kinds_at_call_start_dont_enter_index() {
    let fa = parse(
        r#"
package Foo;
sub new { bless {}, shift }
sub m { 1 }
package main;
my $x = Foo->new;
$x->m();
my $k = $x->{key};
"#,
    );

    // Cross-check: nothing in the index points at a non-call kind.
    for (&_pt, &idx) in fa.call_ref_by_start.iter() {
        let kind = &fa.refs[idx].kind;
        assert!(
            matches!(kind, RefKind::MethodCall { .. } | RefKind::FunctionCall { .. }),
            "non-call kind in call_ref_by_start: {:?}",
            kind,
        );
    }

    // And specifically: any HashKeyAccess / ContainerAccess /
    // Variable ref starting at the same point as `$x` is excluded.
    let m_idx = ref_idx(&fa, "m", is_method_call);
    let m = &fa.refs[m_idx];
    let RefKind::MethodCall { invocant_span: Some(m_invocant_span), .. } = &m.kind else {
        panic!("m is a MethodCall");
    };
    let key_point = m_invocant_span.start;
    let resolved = fa.call_ref_by_start.get(&key_point).copied();

    // The only call-shaped ref starting at `$x`'s point is the
    // outer `m` itself — the index must point there. (Recursion's
    // `is_self` guard takes care of the rest; see
    // `is_self_guard_falls_through_to_variable_branch`.)
    assert_eq!(
        resolved,
        Some(m_idx),
        "only call ref at $$x's start is `m` itself",
    );

    // Sanity: at least one non-call ref *does* start at that
    // point (otherwise the test asserts nothing).
    let non_call_at_same_start = fa
        .refs
        .iter()
        .any(|r| {
            r.span.start == key_point
                && !matches!(r.kind, RefKind::MethodCall { .. } | RefKind::FunctionCall { .. })
        });
    assert!(
        non_call_at_same_start,
        "fixture should produce at least one non-call ref starting at $$x's point; \
         refs: {:?}",
        fa.refs
            .iter()
            .map(|r| (r.span.start, &r.target_name, &r.kind))
            .collect::<Vec<_>>(),
    );
}

/// `is_self` guard: when `Foo->m()` is the only call-shaped ref at
/// the chain's leftmost point, the index lookup from `m`'s
/// `invocant_span` returns `m` itself. The recursion in
/// `method_call_invocant_class` must detect that (pointer compare
/// against the input ref) and bail out, so the bareword invocant
/// branch can resolve `Foo` → "Foo" at the bottom of the function.
///
/// Without the guard: infinite recursion (or loops back through
/// the same call edge).
#[test]
fn is_self_guard_resolves_bareword_invocant() {
    let fa = parse(
        r#"
package Foo;
sub m { 1 }
package main;
Foo->m();
"#,
    );
    let m = &fa.refs[ref_idx(&fa, "m", is_method_call)];

    // Sanity: index lookup at m's invocant_span DOES point at `m`
    // itself (no smaller call ref to displace it).
    let RefKind::MethodCall { invocant_span: Some(span), .. } = &m.kind else {
        panic!("m is a MethodCall");
    };
    let resolved = fa.call_ref_by_start.get(&span.start).copied();
    let m_idx = ref_idx(&fa, "m", is_method_call);
    assert_eq!(resolved, Some(m_idx));

    // The functional consequence of the guard: resolution returns
    // "Foo" (the bareword invocant), not None or a hang.
    let class = fa.method_call_invocant_class(m, None);
    assert_eq!(class.as_deref(), Some("Foo"));
}

/// Same guard, variable invocant: `$x->m()` — the only call ref at
/// `$x`'s start is the outer `m` itself (a Variable ref isn't
/// call-shaped, so it's not in the index). Recursion detects self,
/// bails, falls through to the bag query for `$x`.
///
/// This test pins the interaction: the variable branch is reached
/// even when the chain branch would otherwise loop on itself.
#[test]
fn is_self_guard_falls_through_to_variable_branch() {
    let fa = parse(
        r#"
package Bar;
sub new { bless {}, shift }
sub m { 1 }
package main;
my $x = Bar->new;
$x->m();
"#,
    );
    let m = &fa.refs[ref_idx(&fa, "m", is_method_call)];

    let RefKind::MethodCall { invocant_span: Some(span), .. } = &m.kind else {
        panic!("m is a MethodCall");
    };
    // The outer `m` is the only call-shaped ref starting at `$x`'s
    // point — `$x` itself is a Variable ref, not indexed.
    let resolved = fa.call_ref_by_start.get(&span.start).copied();
    let m_idx = ref_idx(&fa, "m", is_method_call);
    assert_eq!(
        resolved,
        Some(m_idx),
        "with no inner call receiver, the index points at the outer \
         method call itself; the `is_self` guard must keep us from \
         recursing on it",
    );

    // Functional consequence: `$x` is typed as `Bar` via the
    // build-time TC; the variable branch in
    // `method_call_invocant_class` reads that from the bag.
    assert_eq!(fa.method_call_invocant_class(m, None).as_deref(), Some("Bar"));
}

/// Equal spans: when two MethodCall refs occupy the exact same
/// span (a synthesized / plugin-emitted shape that the parser
/// can't naturally produce), the index keeps the **first** insert
/// — smallest-span comparison uses strict `<`, so neither ref
/// displaces the other.
///
/// Pin this with a hand-built FileAnalysis. Documents the
/// resolved tie behavior so a future refactor that loosens the
/// comparison to `<=` (which would silently flip the winner)
/// trips this assertion.
#[test]
fn equal_span_first_write_wins() {
    let span = Span {
        start: Point::new(0, 0),
        end: Point::new(0, 10),
    };
    // Two MethodCall refs at the exact same span. Order matters
    // because we're pinning "first wins".
    let refs = vec![
        Ref {
            kind: RefKind::MethodCall {
                invocant: crate::conventions::InvocantName::assume_canonical("$a"),
                invocant_span: Some(Span {
                    start: Point::new(0, 0),
                    end: Point::new(0, 2),
                }),
                method_name_span: Span {
                    start: Point::new(0, 4),
                    end: Point::new(0, 5),
                },
                member_op: None,
            },
            span,
            scope: ScopeId(0),
            target_name: "first".into(),
            access: AccessKind::Read,
            resolves_to: None,
            resolved_method_target: None,
            folded_from: None,
        },
        Ref {
            kind: RefKind::MethodCall {
                invocant: crate::conventions::InvocantName::assume_canonical("$b"),
                invocant_span: Some(Span {
                    start: Point::new(0, 0),
                    end: Point::new(0, 2),
                }),
                method_name_span: Span {
                    start: Point::new(0, 6),
                    end: Point::new(0, 7),
                },
                member_op: None,
            },
            span,
            scope: ScopeId(0),
            target_name: "second".into(),
            access: AccessKind::Read,
            resolves_to: None,
            resolved_method_target: None,
            folded_from: None,
        },
    ];

    let fa = FileAnalysis::new(crate::file_analysis::FileAnalysisParts {
        scopes: vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        refs,
        ..Default::default()
    });

    let resolved = fa.call_ref_by_start.get(&span.start).copied();
    assert_eq!(
        resolved,
        Some(0),
        "first insert wins on equal spans; got idx {:?}",
        resolved,
    );
}

/// Stress: a deep chain `a()->b()->c()->d()->e()->f()` resolves
/// without recursion blow-up. Each hop's index lookup lands on the
/// next-inner call ref (smallest-span wins), so recursion strictly
/// narrows; total depth is bounded by chain length.
///
/// Concrete pin: top-level `f`'s invocant_class resolves through
/// every hop. We don't define typed bodies — the resolver should
/// return `None` (no class info), but it must do so in finite time
/// without panicking.
#[test]
fn deep_chain_terminates() {
    let fa = parse(
        r#"
sub a { 1 }
a()->b()->c()->d()->e()->f();
"#,
    );

    let f_idx = ref_idx(&fa, "f", is_method_call);
    let f = &fa.refs[f_idx];

    // No panic, no hang. Return value can be None (no return-type
    // info available); the point is termination.
    let _ = fa.method_call_invocant_class(f, None);
}

/// Cross-check: every ref in `call_ref_by_start` is actually a
/// MethodCall or FunctionCall. Guards against a future code
/// addition that puts another kind in.
#[test]
fn index_only_holds_call_shaped_kinds() {
    let fa = parse(
        r#"
package Foo;
sub new { bless {}, shift }
sub bar { 1 }
package main;
my $x = Foo->new;
$x->bar();
make_b()->touch();
sub make_b { 1 }
sub touch { 1 }
"#,
    );

    for (&_pt, &idx) in fa.call_ref_by_start.iter() {
        let kind = &fa.refs[idx].kind;
        assert!(
            matches!(kind, RefKind::MethodCall { .. } | RefKind::FunctionCall { .. }),
            "call_ref_by_start contains non-call kind: {:?}",
            kind,
        );
    }
}
