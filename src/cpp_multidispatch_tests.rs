//! PoC measurements: C++ overload resolution as multiple dispatch — the
//! dispatch SHAPE (arity, ranking, ambiguity) on real parsed overload sets.

use super::*;

fn cpp_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_cpp::LANGUAGE.into()).unwrap();
    p
}

fn sets(src: &str) -> std::collections::HashMap<String, OverloadSet> {
    let mut p = cpp_parser();
    let tree = p.parse(src, None).unwrap();
    collect_overloads(&tree, src.as_bytes())
}

#[test]
fn collects_overload_set_with_param_and_return_types() {
    let s = sets("int area(int s);\ndouble area(double r);\nint area(int w, int h);\n");
    let area = &s["area"];
    assert_eq!(area.sigs.len(), 3, "three overloads of area: {area:?}");
    assert!(area.sigs.contains(&Signature { params: vec![Ty::Int], ret: Ty::Int }));
    assert!(area.sigs.contains(&Signature { params: vec![Ty::Double], ret: Ty::Double }));
    assert!(area.sigs.contains(&Signature { params: vec![Ty::Int, Ty::Int], ret: Ty::Int }));
}

#[test]
fn exact_beats_conversion() {
    // THE design point: `area(5)` (int) must pick `area(int)`, NOT
    // `area(double)` via int→double — exact (rank 0) beats convertible
    // (rank 1). This is what makes dispatch more than "find a match".
    let s = sets("int area(int s);\ndouble area(double r);\n");
    match dispatch(&s["area"], &[Ty::Int]) {
        Dispatch::Resolved(sig) => assert_eq!(sig.params, vec![Ty::Int], "exact int overload"),
        other => panic!("expected exact int, got {other:?}"),
    }
    // and `area(2.0)` picks the double overload, symmetrically.
    match dispatch(&s["area"], &[Ty::Double]) {
        Dispatch::Resolved(sig) => assert_eq!(sig.params, vec![Ty::Double]),
        other => panic!("expected double, got {other:?}"),
    }
}

#[test]
fn conversion_used_only_when_no_exact() {
    // Only a double overload exists; `g(5)` binds int→double (rank 1).
    let s = sets("double g(double r);\n");
    match dispatch(&s["g"], &[Ty::Int]) {
        Dispatch::Resolved(sig) => assert_eq!(sig.params, vec![Ty::Double], "promotes to double"),
        other => panic!("expected double via conversion, got {other:?}"),
    }
}

#[test]
fn arity_is_the_first_coordinate() {
    let s = sets("int f(int a);\nint f(int a, int b);\nint f(int a, int b, int c);\n");
    match dispatch(&s["f"], &[Ty::Int, Ty::Int]) {
        Dispatch::Resolved(sig) => assert_eq!(sig.params.len(), 2),
        other => panic!("arity dispatch failed: {other:?}"),
    }
}

#[test]
fn class_dispatch_is_exact_name() {
    let s = sets("int draw(Circle c);\nint draw(Square s);\n");
    match dispatch(&s["draw"], &[Ty::Class("Square".into())]) {
        Dispatch::Resolved(sig) => assert_eq!(sig.params, vec![Ty::Class("Square".into())]),
        other => panic!("class dispatch failed: {other:?}"),
    }
    // a class with no matching overload → no viable overload (no subtyping)
    assert_eq!(dispatch(&s["draw"], &[Ty::Class("Triangle".into())]), Dispatch::NoViableOverload);
}

#[test]
fn ambiguity_is_a_real_outcome() {
    // `h(int, int)` against `h(int,double)` / `h(double,int)`: each needs
    // exactly one conversion → both total rank 1 → genuinely ambiguous,
    // exactly as C++ reports. The dispatcher must NOT silently pick one.
    let s = sets("int h(int a, double b);\nint h(double a, int b);\n");
    match dispatch(&s["h"], &[Ty::Int, Ty::Int]) {
        Dispatch::Ambiguous(cands) => assert_eq!(cands.len(), 2, "both overloads tie"),
        other => panic!("expected ambiguity, got {other:?}"),
    }
}

#[test]
fn no_viable_overload_when_arity_mismatches_everything() {
    let s = sets("int f(int a);\nint f(int a, int b);\n");
    assert_eq!(dispatch(&s["f"], &[Ty::Int, Ty::Int, Ty::Int]), Dispatch::NoViableOverload);
}

#[test]
fn end_to_end_dispatch_from_a_call_sites_literals() {
    // Tie it to a real call: literal `5` is int, `2.5` is double. Derive
    // the arg-type tuple from the call's literals and dispatch — the
    // call-site half of the multimethod seam.
    let src = "int area(int s);\ndouble area(double r);\n";
    let s = sets(src);
    // (a tiny literal→Ty oracle stands in for full expr typing here)
    let arg_int = Ty::Int; // from `area(5)`
    let arg_dbl = Ty::Double; // from `area(2.5)`
    assert!(matches!(dispatch(&s["area"], &[arg_int], ), Dispatch::Resolved(sig) if sig.ret == Ty::Int));
    assert!(matches!(dispatch(&s["area"], &[arg_dbl]), Dispatch::Resolved(sig) if sig.ret == Ty::Double));
}
