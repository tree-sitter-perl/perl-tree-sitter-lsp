//! Measures B1: the C lexer hack resolved by REPARSE — the corrected
//! tree is canonical, so downstream consumers need no side-table.

use super::*;

fn c_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_c::LANGUAGE.into()).unwrap();
    p
}

fn parse(p: &mut tree_sitter::Parser, src: &str) -> Tree {
    p.parse(src, None).unwrap()
}

/// The kind of the statement node covering byte `at`.
fn stmt_kind_at(tree: &Tree, at: usize) -> String {
    let mut node = tree
        .root_node()
        .named_descendant_for_byte_range(at, at)
        .unwrap();
    while let Some(par) = node.parent() {
        if par.kind() == "translation_unit" || par.kind() == "compound_statement" {
            break;
        }
        node = par;
    }
    node.kind().to_string()
}

#[test]
fn lexer_hack_same_syntax_opposite_tree() {
    // Identical syntax `_ * _;`, opposite meaning. After reparse the
    // TREE itself is correct — no consultation, no verdict side-table.
    let mut p = c_parser();
    let src = "typedef int t;\nint a;\nt * x;\na * y;\n";
    let tree = parse(&mut p, src);
    let table = SymbolTable::from_tree(&tree, src.as_bytes());

    let (rewritten, map) = disambiguate(&tree, src, &table);
    assert!(rewritten.contains("(a * y)"), "value site wrapped: {rewritten:?}");
    assert!(rewritten.contains("\nt * x;"), "type site untouched: {rewritten:?}");

    let after = parse(&mut p, &rewritten);
    let tx = rewritten.find("t * x").unwrap();
    let ay = rewritten.find("a * y").unwrap();
    assert_eq!(stmt_kind_at(&after, tx), "declaration", "t is a type → pointer decl");
    assert_eq!(stmt_kind_at(&after, ay), "expression_statement", "a is a value → multiply");

    // anchor remap: the `y` in the rewritten source points at original `y`.
    let y_t = rewritten.find('y').unwrap();
    assert_eq!(&src[map.to_original(y_t)..map.to_original(y_t) + 1], "y");
}

#[test]
fn lexer_hack_unknown_name_is_identity() {
    // Conservative: no evidence the first name is a value → no rewrite,
    // tree-sitter's declaration default stands. Zero false positives.
    let mut p = c_parser();
    let src = "Mystery * p;\n";
    let tree = parse(&mut p, src);
    let table = SymbolTable::from_tree(&tree, src.as_bytes());
    let (rewritten, _) = disambiguate(&tree, src, &table);
    assert_eq!(rewritten, src, "unknown first name → identity (no flip)");
}

#[test]
fn lexer_hack_cross_file_flip_via_header() {
    // THE DIFFERENTIATOR. tree-sitter is blind to the header, so
    // `gain * level;` stays its wrong declaration guess. Merge the
    // header's `extern int gain` (gain is a value) and the reparse
    // rewrites it to a multiply — resolution tree-sitter structurally
    // cannot do; the perl-lsp cross-file table does by construction.
    let mut p = c_parser();
    let header = "extern int gain;\n";
    let htree = parse(&mut p, header);
    let header_table = SymbolTable::from_tree(&htree, header.as_bytes());
    assert!(header_table.values.contains("gain"));

    let src = "void f(void) {\n    gain * level;\n}\n";
    let stree = parse(&mut p, src);

    // local-only: gain unknown → identity, the wrong declaration stays
    let local = SymbolTable::from_tree(&stree, src.as_bytes());
    let (local_rw, _) = disambiguate(&stree, src, &local);
    assert_eq!(local_rw, src, "without the header, the bug stands");
    let at = src.find("gain").unwrap();
    assert_eq!(stmt_kind_at(&parse(&mut p, src), at), "declaration", "blind = wrong");

    // cross-file: merge the header fact → the reparse flips it
    let mut merged = local.clone();
    merged.merge(&header_table);
    let (rw, _) = disambiguate(&stree, src, &merged);
    assert!(rw.contains("(gain * level)"), "flipped: {rw:?}");
    let after = parse(&mut p, &rw);
    assert_eq!(
        stmt_kind_at(&after, rw.find("gain").unwrap()),
        "expression_statement",
        "the header's `extern int gain` makes it a multiply",
    );
}

#[test]
fn lexer_hack_cross_file_typedef_stays_declaration() {
    // The type direction: a header typedef keeps `Pixel * p;` a genuine
    // declaration (the default agrees, and the table makes it a fact).
    let mut p = c_parser();
    let header = "typedef struct Pixel Pixel;\n";
    let htree = parse(&mut p, header);
    let htable = SymbolTable::from_tree(&htree, header.as_bytes());
    assert!(htable.types.contains("Pixel"));

    let src = "Pixel * p;\n";
    let stree = parse(&mut p, src);
    let mut table = SymbolTable::from_tree(&stree, src.as_bytes());
    table.merge(&htable);
    let (rw, _) = disambiguate(&stree, src, &table);
    assert_eq!(rw, src, "type LHS → no rewrite, declaration stands");
    assert_eq!(stmt_kind_at(&parse(&mut p, &rw), 0), "declaration");
}



#[test]
fn b1_leak_collision_detected_and_monomorphized() {
    // The B1 leak: an #ifdef makes `a` a TYPE in one arm, a VALUE in the
    // other, so `a * b;` outside the #ifdef is config-dependent.
    let mut p = c_parser();
    let src = "#ifdef X\ntypedef int a;\n#else\nint a;\n#endif\na * b;\n";

    // superposed (config-less): the table sees BOTH → Ambiguous, and
    // disambiguate refuses to silently pick (identity, no false flip).
    let tree = parse(&mut p, src);
    let table = SymbolTable::from_tree(&tree, src.as_bytes());
    assert_eq!(table.classify("a"), Category::Ambiguous, "type-and-value collision");
    let (rw, _) = disambiguate(&tree, src, &table);
    assert_eq!(rw, src, "ambiguous name → no rewrite, no silent flip");

    // MONOMORPHIZE by config — the pieces compose: select_config blanks
    // the dead arm, so the table is unambiguous again.
    use crate::c_preproc::{select_config, Config};
    // X defined → only `typedef int a` survives → a is a Type → decl.
    let sel_x = select_config(src, &Config::new().with("X", "1"));
    let tx = parse(&mut p, &sel_x.source);
    let tab_x = SymbolTable::from_tree(&tx, sel_x.source.as_bytes());
    assert_eq!(tab_x.classify("a"), Category::Type, "X → a is a type");
    let (rw_x, _) = disambiguate(&tx, &sel_x.source, &tab_x);
    assert!(!rw_x.contains("(a * b)"), "X → declaration, no wrap: {rw_x:?}");

    // !X → only `int a` survives → a is a Value → multiply (wrapped).
    let sel_nx = select_config(src, &Config::new());
    let tnx = parse(&mut p, &sel_nx.source);
    let tab_nx = SymbolTable::from_tree(&tnx, sel_nx.source.as_bytes());
    assert_eq!(tab_nx.classify("a"), Category::Value, "!X → a is a value");
    let (rw_nx, _) = disambiguate(&tnx, &sel_nx.source, &tab_nx);
    assert!(rw_nx.contains("(a * b)"), "!X → multiply, wrapped: {rw_nx:?}");
}
