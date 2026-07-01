//! C/C++ `typedef` / `using` type-alias resolution as bag edges: a declared
//! type that is a typedef spelling chases the alias graph
//! (`TypeName` attachment) to its underlying leaf, instead of dying at the
//! spelling. Covers primitive aliases, `using X = Y`, alias chains, and the
//! self-referential tag typedef that must not loop.

use super::{cpp_pack, extract};
use crate::file_analysis::InferredType;
use tree_sitter::Point;

fn fa(src: &str) -> crate::file_analysis::FileAnalysis {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&tree_sitter_cpp::LANGUAGE.into()).unwrap();
    let tree = parser.parse(src, None).unwrap();
    extract(&tree, src.as_bytes(), &cpp_pack()).unwrap().into_file_analysis()
}

#[test]
fn primitive_typedef_alias_resolves_underlying_spelling() {
    // `typedef unsigned short U16` — the underlying is a multi-word primitive
    // spelling `annot_type` doesn't fold to a lattice type, so it stays the
    // raw `ClassName("unsigned short")` leaf: hover shows the real spelling.
    let src = "\
typedef unsigned short U16;
void f() { U16 a; }
";
    let fa = fa(src);
    assert_eq!(
        fa.inferred_type_via_bag("a", Point { row: 1, column: 15 }),
        Some(InferredType::ClassName("unsigned short".into())),
    );
}

#[test]
fn using_alias_resolves() {
    let src = "\
using U16 = unsigned short;
void f() { U16 a; }
";
    let fa = fa(src);
    assert_eq!(
        fa.inferred_type_via_bag("a", Point { row: 1, column: 15 }),
        Some(InferredType::ClassName("unsigned short".into())),
    );
}

#[test]
fn alias_chain_chases_through_the_graph() {
    // `W16 → V16 → unsigned short`: each hop is an `Edge(TypeName(_))`, chased
    // by the registry's generic edge walk (edges, not values).
    let src = "\
typedef unsigned short V16;
typedef V16 W16;
void f() { W16 b; }
";
    let fa = fa(src);
    assert_eq!(
        fa.inferred_type_via_bag("b", Point { row: 2, column: 15 }),
        Some(InferredType::ClassName("unsigned short".into())),
    );
}

#[test]
fn primitive_keyword_typedef_folds_to_lattice() {
    // `typedef unsigned u32` — the underlying IS an `annot_type`-recognized
    // keyword, so the alias resolves to the value-lattice type, not a spelling.
    let src = "\
typedef unsigned u32;
void f() { u32 c; }
";
    let fa = fa(src);
    assert_eq!(
        fa.inferred_type_via_bag("c", Point { row: 1, column: 15 }),
        Some(InferredType::Numeric),
    );
}

#[test]
fn self_referential_tag_typedef_does_not_loop() {
    // `typedef struct SV SV` — the alias name equals the tag, so the alias
    // edge is `TypeName("SV") → Edge(TypeName("SV"))`. The shared visited set
    // must break the cycle and terminate at the `ClassName("SV")` leaf
    // (not spin / overflow).
    let src = "\
typedef struct SV SV;
void f(SV* p) { p->x; }
";
    let fa = fa(src);
    assert_eq!(
        fa.inferred_type_via_bag("p", Point { row: 1, column: 12 }),
        Some(InferredType::ClassName("SV".into())),
    );
}

#[test]
fn typedef_struct_receiver_member_type() {
    // A `typedef struct S_ { int field_f; } S;` receiver `S* p` resolves the
    // member `field_f` — the typedef case of the plain-struct member lookup.
    let src = "\
typedef struct S_ { int field_f; } S;
void g(S* p) { p->field_f; }
";
    let fa = fa(src);
    assert_eq!(
        fa.field_type_on_class("S", "field_f", None),
        Some(InferredType::Numeric),
    );
}
