//! Measures A2: config-driven `#ifdef` selection → blank-in-place →
//! clean re-parse with identity spans.

use super::*;

fn c_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_c::LANGUAGE.into()).unwrap();
    p
}

fn damage(tree: &tree_sitter::Tree) -> usize {
    let mut n = 0;
    let mut cur = tree.root_node().walk();
    let mut st = vec![tree.root_node()];
    while let Some(x) = st.pop() {
        if x.is_error() || x.is_missing() {
            n += 1;
        }
        for c in x.children(&mut cur) {
            st.push(c);
        }
    }
    n
}

#[test]
fn a2_ifdef_split_construct_recovers() {
    // The doc's A2 fixture: a #if splits the return type from the name.
    // Broken: 4 ERROR nodes. After config selection (`#if 0` → else):
    // a clean function_definition named `main`, at its ORIGINAL offset.
    let mut p = c_parser();
    let src = "int\n#if 0\nfoo\n#else\nmain\n#endif\n(void) {}\n";
    assert!(damage(&p.parse(src, None).unwrap()) > 0, "baseline is broken");

    let sel = select_config(src, &Config::new());
    assert!(sel.unresolved.is_empty(), "`#if 0` is decidable");
    let tree = p.parse(&sel.source, None).unwrap();
    assert_eq!(damage(&tree), 0, "blanked source parses clean: {:?}", sel.source);

    // it's a function definition named `main`...
    let sexp = tree.root_node().to_sexp();
    assert!(sexp.contains("function_definition"), "{sexp}");
    // ...and identity spans: `main` sits at its original byte offset.
    let orig = src.find("main").unwrap();
    let node = tree.root_node().named_descendant_for_byte_range(orig, orig + 4).unwrap();
    assert_eq!(node.utf8_text(sel.source.as_bytes()).unwrap(), "main", "span identity");
}

#[test]
fn a2_ifdef_selects_branch_by_config() {
    // The configuration decides which branch is live. Same source, two
    // configs, two different live functions.
    let mut p = c_parser();
    let src = "#ifdef DEBUG\nint dbg(void){return 1;}\n#else\nint rel(void){return 0;}\n#endif\n";

    let off = select_config(src, &Config::new());
    let on = select_config(src, &Config::new().with("DEBUG", "1"));

    let toff = p.parse(&off.source, None).unwrap();
    let ton = p.parse(&on.source, None).unwrap();
    assert_eq!(damage(&toff), 0);
    assert_eq!(damage(&ton), 0);
    assert!(off.source.contains("rel") && !off.source.contains("dbg"), "no DEBUG → rel: {:?}", off.source);
    assert!(on.source.contains("dbg") && !on.source.contains("rel"), "DEBUG → dbg: {:?}", on.source);
}

#[test]
fn a2_nested_and_defined_and_negation() {
    let mut p = c_parser();
    let src = "\
#if defined(A)
int a;
#if !defined(B)
int ab;
#endif
#endif
int always;
";
    let sel = select_config(src, &Config::new().with("A", "1"));
    let tree = p.parse(&sel.source, None).unwrap();
    assert_eq!(damage(&tree), 0);
    // A defined, B not → both `a` and `ab` live; `always` always live.
    assert!(sel.source.contains("int a;"), "{:?}", sel.source);
    assert!(sel.source.contains("int ab;"), "{:?}", sel.source);
    assert!(sel.source.contains("int always;"));

    // with B also defined, the inner !defined(B) arm goes dead
    let sel2 = select_config(src, &Config::new().with("A", "1").with("B", "1"));
    assert!(sel2.source.contains("int a;"));
    assert!(!sel2.source.contains("int ab;"), "inner arm dead: {:?}", sel2.source);
}

#[test]
fn a2_unresolved_condition_routes_to_probe() {
    // A condition the lite evaluator can't decide (arithmetic / macro
    // value) is RECORDED for the cpp probe, not guessed — and treated as
    // false meanwhile (conservative). This is the user-config + probe
    // handoff the design calls for.
    let src = "#if VERSION > 2\nint newapi;\n#else\nint oldapi;\n#endif\n";
    let sel = select_config(src, &Config::new());
    assert_eq!(
        sel.unresolved,
        vec![Unresolved { line: 0, condition: "#if VERSION > 2".into() }],
        "compound condition handed to the probe",
    );
    // conservative default: unresolved #if is false → the #else is live
    assert!(sel.source.contains("oldapi") && !sel.source.contains("newapi"), "{:?}", sel.source);
}

// ---- parallel realities: don't PICK a config, cover BOTH and reduce ----

fn fn_names(p: &mut tree_sitter::Parser, src: &str) -> Vec<String> {
    let tree = p.parse(src, None).unwrap();
    let q = tree_sitter::Query::new(
        &tree_sitter_c::LANGUAGE.into(),
        "(function_definition declarator: (function_declarator declarator: (identifier) @n))",
    ).unwrap();
    let mut out = Vec::new();
    let mut cur = tree_sitter::QueryCursor::new();
    use tree_sitter::StreamingIterator;
    let mut it = cur.matches(&q, tree.root_node(), src.as_bytes());
    while let Some(m) = it.next() {
        for c in m.captures { out.push(c.node.utf8_text(src.as_bytes()).unwrap().to_string()); }
    }
    out
}

#[derive(Debug, PartialEq, Eq)]
enum Presence { Always, When(String) }

#[test]
fn a2_parallel_realities_reduce_to_presence_conditions() {
    // Config-INDEPENDENT: no user config. We fork both realities of the
    // single #ifdef, blank-and-parse each (clean trees), and REDUCE —
    // a symbol present in ALL realities is unconditional; one present in
    // some is tagged with its presence condition. This is "reparse
    // except parallel realities": the dead branch in reality A is the
    // live branch in reality B.
    let mut p = c_parser();
    let src = "\
int common(void){return 0;}
#ifdef DEBUG
int dbg(void){return 1;}
#else
int rel(void){return 0;}
#endif
";
    // the two realities (this is the per-#ifdef cross product; here N=1)
    let off = select_config(src, &Config::new());
    let on = select_config(src, &Config::new().with("DEBUG", "1"));
    let names_off = fn_names(&mut p, &off.source);
    let names_on = fn_names(&mut p, &on.source);

    // reduce: union, tagged by which realities each symbol appears in
    let all: std::collections::BTreeSet<&String> = names_off.iter().chain(&names_on).collect();
    let presence: Vec<(String, Presence)> = all
        .into_iter()
        .map(|n| {
            let in_on = names_on.contains(n);
            let in_off = names_off.contains(n);
            let pres = match (in_on, in_off) {
                (true, true) => Presence::Always,
                (true, false) => Presence::When("defined(DEBUG)".into()),
                (false, true) => Presence::When("!defined(DEBUG)".into()),
                (false, false) => unreachable!(),
            };
            (n.clone(), pres)
        })
        .collect();

    // BOTH branches' symbols are in the analysis, each condition-tagged —
    // without ever choosing a build config.
    assert!(presence.contains(&("common".into(), Presence::Always)), "{presence:?}");
    assert!(presence.contains(&("dbg".into(), Presence::When("defined(DEBUG)".into()))), "{presence:?}");
    assert!(presence.contains(&("rel".into(), Presence::When("!defined(DEBUG)".into()))), "{presence:?}");
}

