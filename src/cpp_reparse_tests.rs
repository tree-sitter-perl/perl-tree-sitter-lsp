//! Measures the C++ reparse seam: macro expansion → corrected re-parse
//! → recovered symbols through the production cpp_pack extractor, with
//! anchor remap back to original source.

use super::*;
use crate::query_extract::{cpp_pack, extract};

#[path = "cpp_obstacle.rs"]
mod cpp_obstacle;

fn cpp_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_cpp::LANGUAGE.into()).unwrap();
    p
}

fn parse(p: &mut tree_sitter::Parser, src: &str) -> tree_sitter::Tree {
    p.parse(src, None).unwrap()
}

fn errors(node: tree_sitter::Node) -> usize {
    let mut n = 0;
    let mut cur = node.walk();
    let mut stack = vec![node];
    while let Some(x) = stack.pop() {
        if x.is_error() || x.is_missing() {
            n += 1;
        }
        for c in x.children(&mut cur) {
            stack.push(c);
        }
    }
    n
}

fn extracted_names(p: &mut tree_sitter::Parser, src: &str) -> Vec<String> {
    let tree = parse(p, src);
    extract(&tree, src.as_bytes(), &cpp_pack())
        .unwrap()
        .symbols
        .iter()
        .map(|s| s.name.clone())
        .collect()
}

fn sample(name: &str) -> &'static cpp_obstacle::Sample {
    cpp_obstacle::SAMPLES.iter().find(|s| s.name == name).unwrap()
}

#[test]
fn object_macro_recovers_corrupted_class() {
    let mut p = cpp_parser();
    let src = sample("api_export_attr").src;

    // baseline: class evaporates into a function_definition
    let before = parse(&mut p, src);
    assert!(
        before.root_node().to_sexp().contains("function_definition"),
        "baseline corruption: class parsed as a function",
    );
    assert!(!extracted_names(&mut p, src).contains(&"Widget".to_string()), "Widget lost pre-reparse");

    let tree = parse(&mut p, src);
    let (rewritten, map) = preprocess(&tree, src);

    // the corrected parse: a real class, error-free
    let after = parse(&mut p, &rewritten);
    assert_eq!(errors(after.root_node()), 0, "expansion clears the parse: {rewritten}");
    assert!(after.root_node().to_sexp().contains("class_specifier"), "real class recovered");

    // through the production extractor: Widget + its methods are back
    let names = extracted_names(&mut p, &rewritten);
    assert!(names.contains(&"Widget".to_string()), "class recovered: {names:?}");
    assert!(names.contains(&"draw".to_string()), "method recovered: {names:?}");
    assert!(names.contains(&"resize".to_string()), "method recovered: {names:?}");

    // anchor remap: `Widget` in the rewritten source maps back onto the
    // original `Widget` token (despite the inserted attribute bytes).
    let w_t = rewritten.find("Widget").unwrap();
    let w_o = map.to_original(w_t);
    assert_eq!(&src[w_o..w_o + 6], "Widget", "anchor lands on original Widget");
}

#[test]
fn function_macro_expands_to_member_declarations() {
    let mut p = cpp_parser();
    let src = sample("decl_macro").src;

    // the macro-generated members are absent pre-expansion
    assert!(!extracted_names(&mut p, src).contains(&"GetRuntimeClass".to_string()));

    let tree = parse(&mut p, src);
    let (rewritten, _map) = preprocess(&tree, src);

    // DECLARE_DYNAMIC(MyObj) expanded its body with cls→MyObj; the
    // virtual method is now a real, extractable symbol.
    assert!(rewritten.contains("GetRuntimeClass"), "body expanded: {rewritten}");
    assert!(rewritten.contains("MyObj* Ptr"), "param substituted: {rewritten}");
    let names = extracted_names(&mut p, &rewritten);
    assert!(names.contains(&"GetRuntimeClass".to_string()), "method synthesized by expansion: {names:?}");
    assert!(names.contains(&"MyObj".to_string()), "class still present: {names:?}");
}

#[test]
fn no_macros_is_identity() {
    let mut p = cpp_parser();
    let src = sample("clean_baseline").src;
    let tree = parse(&mut p, src);
    let (rewritten, map) = preprocess(&tree, src);
    assert_eq!(rewritten, src, "no expandable macros → identity");
    assert_eq!(map.to_original(42), 42);
}

#[test]
fn cpp_reparse_obstacle_delta_report() {
    let mut p = cpp_parser();
    println!("\n===== C++ reparse seam: before → after macro expansion =====");
    println!(
        "{:<18} {:>12}   {:>14}",
        "sample", "errors b→a", "Tier-1 recall b→a"
    );
    let (mut rb, mut ra, mut tot) = (0, 0, 0);
    for s in cpp_obstacle::SAMPLES {
        let tree = parse(&mut p, s.src);
        let eb = errors(tree.root_node());
        let nb = extracted_names(&mut p, s.src);
        let hb = s.expected.iter().filter(|n| nb.iter().any(|g| g == *n)).count();

        let _ = tree;
        let (rewritten, _) = preprocess_validated(&mut p, s.src);
        let ta = parse(&mut p, &rewritten);
        let ea = errors(ta.root_node());
        let na = extracted_names(&mut p, &rewritten);
        let ha = s.expected.iter().filter(|n| na.iter().any(|g| g == *n)).count();

        rb += eb;
        ra += ea;
        tot += s.expected.len();
        println!(
            "{:<18} {:>5} → {:<4}   {:>6}/{} → {}/{}",
            s.name,
            eb,
            ea,
            hb,
            s.expected.len(),
            ha,
            s.expected.len(),
        );
        let _ = &mut tot;
    }
    println!("----- totals: errors {rb} → {ra}; expected-symbol pool = {tot} -----\n");
}
