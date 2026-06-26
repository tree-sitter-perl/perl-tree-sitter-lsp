//! Measures superposition: both `#ifdef` arms as live code → a
//! presence-tagged return union that collapses when arms agree and
//! monomorphizes by config when they diverge.

use super::*;
use crate::c_preproc::Config;
use crate::file_analysis::InferredType;

fn c_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_c::LANGUAGE.into()).unwrap();
    p
}

#[test]
fn agreeing_arms_collapse_to_concrete_without_config() {
    // Both arms return an int → the #ifdef is type-irrelevant; the union
    // collapses to a concrete type with NO config. ("monomorphize to
    // concrete" when arms agree.)
    let mut p = c_parser();
    let src = "int f(void) {\n#ifdef FAST\n  return 1;\n#else\n  return 2;\n#endif\n}\n";
    let tree = p.parse(src, None).unwrap();
    let u = function_return_union(&tree, src.as_bytes(), "f");
    assert_eq!(u.members.len(), 2, "both arms walked: {u:?}");
    assert_eq!(u.concrete(), Some(InferredType::Numeric), "arms agree → concrete");
}

#[test]
fn diverging_arms_are_a_union_that_monomorphizes_by_config() {
    // Arms return different types → a real union; no config gives no
    // concrete answer, but each member carries its presence guard, so a
    // config later picks the concrete member.
    let mut p = c_parser();
    let src = "\
const char* g(void) {
#ifdef LEGACY
  return 0;
#else
  return \"new\";
#endif
}
";
    let tree = p.parse(src, None).unwrap();
    let u = function_return_union(&tree, src.as_bytes(), "g");

    // the union, presence-tagged
    assert!(
        u.members.contains(&UnionMember { ty: InferredType::Numeric, guard: Guard::Defined("LEGACY".into()) }),
        "legacy arm: {u:?}",
    );
    assert!(
        u.members.contains(&UnionMember { ty: InferredType::String, guard: Guard::NotDefined("LEGACY".into()) }),
        "modern arm: {u:?}",
    );

    // config-less: a genuine union, no concrete collapse
    assert_eq!(u.concrete(), None, "divergent arms → union, not concrete");

    // monomorphize by config — the user's "give a config, get the type"
    assert_eq!(
        u.monomorphize(&Config::new().with("LEGACY", "1")),
        Some(InferredType::Numeric),
        "LEGACY → the int arm",
    );
    assert_eq!(
        u.monomorphize(&Config::new()),
        Some(InferredType::String),
        "no LEGACY → the string arm",
    );
}

#[test]
fn defined_condition_and_unconditional_return() {
    let mut p = c_parser();
    // mixes a #if defined() guard with an unconditional return.
    let src = "\
int h(int x) {
  if (x) return 7;
#if defined(DBG)
  return 1;
#else
  return 2;
#endif
}
";
    let tree = p.parse(src, None).unwrap();
    let u = function_return_union(&tree, src.as_bytes(), "h");
    // unconditional `return 7;` is Always; the two #if arms are guarded.
    assert!(u.members.iter().any(|m| m.guard == Guard::Always));
    assert!(u.members.iter().any(|m| m.guard == Guard::Defined("DBG".into())));
    assert!(u.members.iter().any(|m| m.guard == Guard::NotDefined("DBG".into())));
    // all three return ints → concrete despite the branching.
    assert_eq!(u.concrete(), Some(InferredType::Numeric));
}
