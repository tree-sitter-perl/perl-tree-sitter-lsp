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

/// The load-bearing invariant of the two-tier own/external split: the fast
/// path (locals fixpointed over a cached, pre-expanded external table) and the
/// slow path (the old single-tier merge+fixpoint) produce BYTE-IDENTICAL
/// expansion. Exercised on a macro-heavy mix that hits every tricky case the
/// split must preserve: external-referencing-external, external-referencing-
/// local, and a local `#define` shadowing an external one.
#[test]
fn two_tier_matches_single_tier_expansion() {
    let mut p = cpp_parser();

    // EXTERNAL macros (as if gathered from #included headers).
    let mut ext = std::collections::BTreeMap::new();
    let mut def = |m: &mut std::collections::BTreeMap<String, Macro>, n: &str, params: Option<&[&str]>, body: &str| {
        m.insert(
            n.to_string(),
            Macro {
                params: params.map(|ps| ps.iter().map(|s| s.to_string()).collect()),
                body: body.to_string(),
                guards: Vec::new(),
                def_line: 0,
            },
        );
    };
    // external → external (mutual expansion, baked once in the cache)
    def(&mut ext, "API", None, "EXPORTED");
    def(&mut ext, "EXPORTED", None, "__attribute__((visibility(\"default\")))");
    def(&mut ext, "WRAP", Some(&["x"]), "API x");
    // external body that NAMES a local macro (LOCAL_TAG) — forces the slow path
    def(&mut ext, "TAGGED", None, "LOCAL_TAG int");
    // a name the file will SHADOW with its own #define
    def(&mut ext, "MAXLEN", None, "256");

    let external = PreExpandedExternal::from_raw(std::sync::Arc::new(ext));

    let cases = [
        // clean split: only external-referencing-external, no interaction
        "class API Widget { void draw(); };\nint sz = MAXN;\n#define MAXN 8\nWRAP(class) Gadget {};\n",
        // external body references a local macro → slow path must engage
        "#define LOCAL_TAG const\nTAGGED foo;\nclass API Thing { void go(); };\n",
        // local shadows an external name → slow path must engage
        "#define MAXLEN 16\nint buf[MAXLEN];\nclass API Box { void run(); };\n",
        // both interactions at once
        "#define LOCAL_TAG volatile\n#define MAXLEN 32\nTAGGED n = MAXLEN;\nclass API Q {};\n",
    ];

    for src in cases {
        let tree = parse(&mut p, src);
        // args: (…, alias_only, force_slow). Full mode both; the last flag
        // toggles fast two-tier vs the old single-tier merge.
        let (fast, fmap) = preprocess_with_mode_inner(&tree, src, &external, false, false);
        let (slow, smap) = preprocess_with_mode_inner(&tree, src, &external, false, true);
        assert_eq!(fast, slow, "expansion drift (full) on:\n{src}\nfast:\n{fast}\nslow:\n{slow}");
        assert_eq!(fmap.edits_for_test(), smap.edits_for_test(), "splice-map drift on:\n{src}");

        // alias-only mode (the parse-damage fallback) must also agree.
        let (fa, _) = preprocess_with_mode_inner(&tree, src, &external, true, false);
        let (sa, _) = preprocess_with_mode_inner(&tree, src, &external, true, true);
        assert_eq!(fa, sa, "alias-only expansion drift on:\n{src}");
    }
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

// --- SpliceMap: binary search ≡ the former linear scan --------------------
//
// `to_original` / `replacement_at` map extracted spans back to user text;
// a wrong result silently breaks goto-def/hover/rename for C/C++. These
// reference impls ARE the old linear scan; the fuzz test asserts the
// binary-search production path returns byte-identical results for every
// transformed offset over randomized (incl. zero-width, adjacent) edits.

fn ref_to_original(edits: &[(usize, usize, usize)], transformed: usize) -> usize {
    let mut shift: isize = 0;
    for &(os, oe, nlen) in edits {
        let ts = (os as isize + shift) as usize;
        if transformed < ts {
            return (transformed as isize - shift) as usize;
        }
        if transformed < ts + nlen {
            return os;
        }
        shift += nlen as isize - (oe - os) as isize;
    }
    (transformed as isize - shift) as usize
}

fn ref_replacement_at(edits: &[(usize, usize, usize)], transformed: usize) -> Option<(usize, usize)> {
    let mut shift: isize = 0;
    for &(os, oe, nlen) in edits {
        let ts = (os as isize + shift) as usize;
        if transformed < ts {
            return None;
        }
        if transformed < ts + nlen {
            return Some((os, oe));
        }
        shift += nlen as isize - (oe - os) as isize;
    }
    None
}

/// The PERL_BITFIELD16 shape: three config-variant `#define`s under
/// `#ifdef` / `#if defined` / `#else`. Every variant is modeled with its
/// guard trail — nothing collapses to the collection-order winner.
#[test]
fn collect_macro_variants_captures_guard_trails() {
    let mut p = cpp_parser();
    let src = "\
#ifdef WIN32
#define M U16
#endif
#if defined(HAS_NON_INT_BITFIELDS)
#define M U16
#else
#define M unsigned
#endif
struct s { M x:9; };
";
    let tree = parse(&mut p, src);
    let variants = collect_macro_variants(&tree, src.as_bytes());
    let m = variants.get("M").expect("M has variants");
    assert_eq!(m.len(), 3, "all three config variants modeled, none pruned");

    let by_line: std::collections::HashMap<usize, &Macro> =
        m.iter().map(|v| (v.def_line, v)).collect();
    // win32 variant — guarded by the #ifdef.
    assert_eq!(by_line[&1].guards, vec!["defined(WIN32)".to_string()]);
    assert_eq!(by_line[&1].body, "U16");
    // the `#if defined(...)` then-branch.
    assert_eq!(by_line[&4].guards, vec!["defined(HAS_NON_INT_BITFIELDS)".to_string()]);
    // the `#else` branch — the condition is NEGATED.
    assert_eq!(by_line[&6].guards, vec!["!defined(HAS_NON_INT_BITFIELDS)".to_string()]);
    assert_eq!(by_line[&6].body, "unsigned");
}

/// End-to-end reachability + join over the captured variants: WIN32 absent →
/// its variant is UNREACHABLE-labeled (not dropped); the HAS knob's two
/// branches are UNKNOWN; the body join types the macro as an integer.
#[test]
fn variant_model_ranks_and_joins() {
    use crate::cpp_macro_model::{KnownConfig, MacroSite, MacroVariant, MacroVariants, Reachability};
    let mut p = cpp_parser();
    // Primitive bodies so the REAL cpp_pack annot_type types every variant
    // (a typedef'd integer alias like `U16` needs typedef resolution — a
    // documented residual; the join model itself is body-agnostic).
    let src = "\
#ifdef WIN32
#define M short
#endif
#if defined(HAS_NON_INT_BITFIELDS)
#define M unsigned
#else
#define M int
#endif
";
    let tree = parse(&mut p, src);
    let raw = collect_macro_variants(&tree, src.as_bytes());
    let m = MacroVariants {
        name: "M".into(),
        variants: raw["M"]
            .iter()
            .map(|v| MacroVariant {
                body: v.body.clone(),
                params: v.params.clone(),
                guards: v.guards.clone(),
                site: MacroSite { file: None, line: v.def_line },
            })
            .collect(),
    };
    // Known config: nothing predefined; HAS_NON_INT_BITFIELDS is a knob we've
    // seen #defined somewhere (universe), WIN32 is not.
    let cfg = KnownConfig::new(
        Default::default(),
        ["HAS_NON_INT_BITFIELDS".to_string()].into_iter().collect(),
    );
    let ranked = m.ranked(&cfg);
    // last one is the win32 variant, unreachable + labeled.
    let (last, r) = ranked.last().unwrap();
    assert_eq!(last.body, "short");
    assert_eq!(last.guards, vec!["defined(WIN32)".to_string()]);
    assert_eq!(r.label().as_deref(), Some("unreachable: WIN32 undefined"));
    // the two HAS branches are UNKNOWN (kept, not guessed).
    assert!(ranked[..2]
        .iter()
        .all(|(_, r)| matches!(r, Reachability::Unknown { .. })));
    // join over the REAL pack predicate: short ∨ unsigned ∨ int → integer.
    let pack = crate::query_extract::cpp_pack();
    assert_eq!(
        m.join_type(pack.annot_type),
        Some(crate::file_analysis::InferredType::Numeric)
    );
}

#[test]
fn splicemap_binsearch_matches_linear_scan() {
    // Deterministic LCG — no external rng dep.
    let mut state: u64 = 0x9e3779b97f4a7c15;
    let mut next = |bound: usize| {
        state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (state >> 33) as usize % bound.max(1)
    };

    for _ in 0..4000 {
        let src_len = 8 + next(40);
        let src: String = std::iter::repeat('a').take(src_len).collect();
        // Random sorted, disjoint splices. Adjacent (gap 0) + empty
        // replacements are generated so the zero-width `ts`-tie path fires.
        let mut splices: Vec<Splice> = Vec::new();
        let mut cur = 0usize;
        while cur < src_len {
            let gap = next(3); // 0 → adjacent to the previous edit
            let start = cur + gap;
            if start >= src_len {
                break;
            }
            let end = (start + next(4)).min(src_len); // width 0..3
            let rep_len = next(4); // 0..3, incl. empty
            let replacement: String = std::iter::repeat('X').take(rep_len).collect();
            splices.push(Splice { start, end, replacement });
            cur = end + 1;
        }
        let (out, map) = apply(&src, &mut splices);
        for t in 0..=out.len() {
            assert_eq!(
                map.to_original(t),
                ref_to_original(&map.edits, t),
                "to_original mismatch at {t} for edits {:?}",
                map.edits
            );
            assert_eq!(
                map.replacement_at(t),
                ref_replacement_at(&map.edits, t),
                "replacement_at mismatch at {t} for edits {:?}",
                map.edits
            );
        }
    }
}

/// The macro identity lane: `collect_macro_defs` captures each `#define` with
/// its def span and — for a function-like DIRECT-DELEGATION wrapper — the
/// callee it forwards to. Only a body that IS one whole call `G(args)` is a
/// delegation; `F(x) + 1` and object-like macros are not.
#[test]
fn collect_macro_defs_recognizes_delegation() {
    let mut p = cpp_parser();
    let src = "\
#define WRAP(x) realFunc(x)
#define THREADED(x) Perl_new(aTHX_ x)
#define NOTDELEG(x) realFunc(x) + 1
#define OBJLIKE 100
";
    let defs = crate::cpp_reparse::collect_macro_defs(&mut p, src);
    let by_name: std::collections::HashMap<&str, &crate::file_analysis::MacroDef> =
        defs.iter().map(|d| (d.name.as_str(), d)).collect();

    assert_eq!(by_name["WRAP"].delegate.as_deref(), Some("realFunc"));
    assert_eq!(by_name["THREADED"].delegate.as_deref(), Some("Perl_new"));
    // A call that is only PART of the body is not delegation.
    assert_eq!(by_name["NOTDELEG"].delegate, None);
    // Object-like macros never delegate (no params).
    assert_eq!(by_name["OBJLIKE"].delegate, None);
    assert!(by_name["OBJLIKE"].params.is_none());
    assert!(by_name["WRAP"].params.is_some());

    // The def span lands on the macro NAME (not the `#define` keyword).
    let wrap = by_name["WRAP"];
    assert_eq!(wrap.selection_span.start.row, 0);
    assert_eq!(wrap.selection_span.start.column, 8); // after "#define "
}

// ===== Member-block macros as roles =====

#[test]
fn member_block_macro_classified_blanked_and_minted() {
    let mut p = cpp_parser();
    // BASEOP is a field-block macro pasted STANDALONE into two structs; a
    // one-member REFCNT proves roles-all-the-way. `PERL_BITFIELD16` types the
    // key field (re-sourced `TypeName` edge).
    let src = "\
#define PERL_BITFIELD16 unsigned short
#define BASEOP PERL_BITFIELD16 op_type:9; int op_flags;
#define REFCNT int op_refcnt;
struct op { BASEOP };
struct unop { BASEOP int* op_first; };
struct sv { REFCNT };
";
    let plan = crate::cpp_reparse::plan_member_blocks(&mut p, src);
    assert!(!plan.is_empty(), "member-block macros should be detected");

    // Blank mode: the use is whitespace in the parse view, so the structs parse
    // clean — but the ORIGINAL still holds the token (identity preserved).
    assert!(plan.blanked_source.contains("struct op {        };") || plan.blanked_source.contains("struct op {  "),
        "BASEOP use blanked: {:?}", plan.blanked_source);
    assert!(plan.blanked_source.contains("BASEOP") == false || !plan.blanked_source.contains("{ BASEOP"),
        "no BASEOP use survives in the parse view");
    assert_eq!(errors(parse(&mut p, &plan.blanked_source).root_node()), 0, "blanked source parses clean");

    // Parent edges — the copypasta IS inheritance.
    assert!(plan.edges.contains(&("op".to_string(), "BASEOP".to_string())));
    assert!(plan.edges.contains(&("unop".to_string(), "BASEOP".to_string())));
    assert!(plan.edges.contains(&("sv".to_string(), "REFCNT".to_string())));

    // One synthetic base per macro, members parsed from the config-active body.
    let baseop = plan.bases.iter().find(|b| b.macro_name == "BASEOP").expect("BASEOP base");
    let names: Vec<&str> = baseop.members.iter().map(|m| m.name.as_str()).collect();
    assert_eq!(names, vec!["op_type", "op_flags"]);
    let op_type = &baseop.members[0];
    assert_eq!(op_type.type_text, "PERL_BITFIELD16");
    // Positioned at the real `#define` body token (line 1, not the use sites).
    assert_eq!(op_type.name_span.start.row, 1);

    // Roles all the way: even a one-member macro is a role.
    let refcnt = plan.bases.iter().find(|b| b.macro_name == "REFCNT").expect("REFCNT base");
    assert_eq!(refcnt.members.len(), 1);
    assert_eq!(refcnt.members[0].name, "op_refcnt");
}

#[test]
fn non_member_block_macros_are_untouched() {
    let mut p = cpp_parser();
    // A value macro and a function-like macro are NOT member blocks — no plan.
    let src = "\
#define MAX 100
#define MIN(a,b) ((a)<(b)?(a):(b))
struct s { int x; };
";
    let plan = crate::cpp_reparse::plan_member_blocks(&mut p, src);
    assert!(plan.is_empty(), "no member-block macros here");
    assert_eq!(plan.blanked_source, src, "source unchanged");
}
