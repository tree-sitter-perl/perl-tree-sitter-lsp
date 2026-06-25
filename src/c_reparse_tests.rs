//! Measures B1: the C lexer hack resolved by the symbol table, in-file
//! and (the differentiator) cross-file.

use super::*;

fn c_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_c::LANGUAGE.into()).unwrap();
    p
}

fn parse(p: &mut tree_sitter::Parser, src: &str) -> Tree {
    p.parse(src, None).unwrap()
}

#[test]
fn lexer_hack_same_syntax_opposite_verdict() {
    // The textbook ambiguity, both arms in one file. `t` is a type,
    // `a` is a value — identical syntax `_ * _;`, opposite meaning,
    // decided purely by the symbol table.
    let mut p = c_parser();
    let src = "typedef int t;\nint a;\nt * x;\na * y;\n";
    let tree = parse(&mut p, src);
    let table = SymbolTable::from_tree(&tree, src.as_bytes());
    assert!(table.types.contains("t"), "typedef collected: {table:?}");
    assert!(table.values.contains("a"), "variable collected: {table:?}");

    let verdicts = resolve_lexer_hacks(&tree, src.as_bytes(), &table);
    assert!(
        verdicts.contains(&LexerHack::Declaration { type_name: "t".into(), var_name: "x".into() }),
        "`t * x;` is a pointer declaration: {verdicts:?}",
    );
    assert!(
        verdicts.contains(&LexerHack::Multiply { lhs: "a".into(), rhs: "y".into() }),
        "`a * y;` is a multiply, NOT a declaration of `y`: {verdicts:?}",
    );

    // the zero-FP payoff: no phantom variable `y` is minted.
    let declared: Vec<&str> = verdicts.iter().filter_map(|v| v.declares_var()).collect();
    assert!(declared.contains(&"x"), "x is declared");
    assert!(!declared.contains(&"y"), "y must NOT be declared (it's a multiply operand)");
}

#[test]
fn lexer_hack_unknown_name_keeps_declaration_default() {
    // Conservative posture: with no evidence that the first name is a
    // value, we keep tree-sitter's declaration default — no speculative
    // flips, no false positives.
    let mut p = c_parser();
    let src = "Mystery * p;\n";
    let tree = parse(&mut p, src);
    let table = SymbolTable::from_tree(&tree, src.as_bytes());
    let verdicts = resolve_lexer_hacks(&tree, src.as_bytes(), &table);
    assert_eq!(
        verdicts,
        vec![LexerHack::Declaration { type_name: "Mystery".into(), var_name: "p".into() }],
        "unknown first name → trust the default",
    );
}

#[test]
fn lexer_hack_cross_file_flip_via_header() {
    // THE DIFFERENTIATOR. tree-sitter is blind to the header, so it
    // guesses `gain * level;` is a declaration. The cross-file symbol
    // table knows `gain` is a VALUE (extern int from the header) and
    // flips it to a multiply — resolution tree-sitter structurally
    // cannot do, and the perl-lsp cross-file engine does by construction.
    let mut p = c_parser();

    let header = "extern int gain;\n";
    let htree = parse(&mut p, header);
    let header_table = SymbolTable::from_tree(&htree, header.as_bytes());
    assert!(header_table.values.contains("gain"), "header exposes gain as a value");

    let src = "void f(void) {\n    gain * level;\n}\n";
    let stree = parse(&mut p, src);

    // local-only view: gain is unknown → declaration default (the bug)
    let local = SymbolTable::from_tree(&stree, src.as_bytes());
    assert_eq!(
        resolve_lexer_hacks(&stree, src.as_bytes(), &local),
        vec![LexerHack::Declaration { type_name: "gain".into(), var_name: "level".into() }],
        "without the header, tree-sitter's wrong guess stands",
    );

    // cross-file view: merge the header fact → the flip
    let mut merged = local.clone();
    merged.merge(&header_table);
    assert_eq!(
        resolve_lexer_hacks(&stree, src.as_bytes(), &merged),
        vec![LexerHack::Multiply { lhs: "gain".into(), rhs: "level".into() }],
        "the header's `extern int gain` flips it to a multiply",
    );
}

#[test]
fn lexer_hack_cross_file_confirms_typedef() {
    // The type direction: a header typedef makes `Pixel * p;` a genuine
    // declaration — and we KNOW it rather than guessing. (Here the
    // default agrees, but the table makes it a fact, not luck.)
    let mut p = c_parser();
    let header = "typedef struct Pixel Pixel;\n";
    let htree = parse(&mut p, header);
    let htable = SymbolTable::from_tree(&htree, header.as_bytes());
    assert!(htable.types.contains("Pixel"), "header typedef collected: {htable:?}");

    let src = "Pixel * p;\n";
    let stree = parse(&mut p, src);
    let mut table = SymbolTable::from_tree(&stree, src.as_bytes());
    table.merge(&htable);
    assert_eq!(
        resolve_lexer_hacks(&stree, src.as_bytes(), &table),
        vec![LexerHack::Declaration { type_name: "Pixel".into(), var_name: "p".into() }],
    );
}
