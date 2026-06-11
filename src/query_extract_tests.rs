//! Differential harness: the query-skeleton vs the real builder, over
//! the repo's test corpus (+ a substrate sample when present). The
//! numbers this prints ARE the spike's findings — run with
//! `cargo test query_skeleton -- --nocapture`.

use super::*;
use std::collections::HashSet;
use std::path::PathBuf;

fn corpus_files() -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_files")];
    while let Some(dir) = stack.pop() {
        let Ok(rd) = std::fs::read_dir(&dir) else { continue };
        for e in rd.flatten() {
            let p = e.path();
            if p.is_dir() {
                stack.push(p);
            } else if matches!(p.extension().and_then(|s| s.to_str()), Some("pl" | "pm")) {
                out.push(p);
            }
        }
    }
    // Substrate sample: realism check against real CPAN code. The
    // snapshot lives in the main checkout; absent (CI) we just skip.
    let substrate = PathBuf::from("/home/veesh/personal/perl-tree-sitter-lsp/gold-corpus/local/lib/perl5");
    if substrate.is_dir() {
        let mut subs: Vec<PathBuf> = Vec::new();
        let mut stack = vec![substrate];
        while let Some(dir) = stack.pop() {
            if subs.len() >= 60 {
                break;
            }
            let Ok(rd) = std::fs::read_dir(&dir) else { continue };
            for e in rd.flatten() {
                let p = e.path();
                if p.is_dir() {
                    stack.push(p);
                } else if p.extension().and_then(|s| s.to_str()) == Some("pm")
                    && std::fs::metadata(&p).map(|m| m.len() < 100_000).unwrap_or(false)
                {
                    subs.push(p);
                    if subs.len() >= 60 {
                        break;
                    }
                }
            }
        }
        out.extend(subs);
    }
    out.sort();
    out
}

#[derive(Default)]
struct Tally {
    builder: usize,
    matched: usize,
    misses: Vec<String>,
}

impl Tally {
    fn recall(&self) -> f64 {
        if self.builder == 0 {
            return 1.0;
        }
        self.matched as f64 / self.builder as f64
    }
}

#[test]
fn query_skeleton_differential_report() {
    use crate::file_analysis::SymKind;
    let pack = perl_pack();
    let mut parser = crate::builder::create_parser();

    // builder kind → skeleton kinds that may legitimately answer for it
    let kind_map: &[(SymKind, &[&str], &str)] = &[
        (SymKind::Package, &["package"], "package"),
        (SymKind::Class, &["package", "class"], "class"),
        (SymKind::Sub, &["sub", "method", "anon", "constant"], "sub"),
        (SymKind::Method, &["sub", "method", "anon"], "method"),
        (SymKind::Variable, &["var"], "variable"),
    ];

    let mut tallies: std::collections::HashMap<&str, Tally> = Default::default();
    let mut synthesized_unmatched: std::collections::HashMap<String, usize> = Default::default();
    let mut ref_tally = Tally::default();
    let mut skel_ref_extras = 0usize;
    let mut skel_ref_total = 0usize;
    let mut files = 0usize;
    let mut query_errors = 0usize;

    for path in corpus_files() {
        let Ok(source) = std::fs::read_to_string(&path) else { continue };
        let Some(tree) = parser.parse(&source, None) else { continue };
        let fa = crate::builder::build(&tree, source.as_bytes());
        let skel = match extract(&tree, source.as_bytes(), &pack) {
            Ok(s) => s,
            Err(e) => {
                query_errors += 1;
                eprintln!("query error on {}: {e}", path.display());
                continue;
            }
        };
        files += 1;

        let skel_defs: HashSet<(String, usize, usize)> = skel
            .symbols
            .iter()
            .map(|s| (s.kind.clone(), s.name_start.row, s.name_start.column))
            .collect();

        for (bkind, skel_kinds, label) in kind_map {
            let t = tallies.entry(label).or_default();
            for sym in fa.symbols.iter().filter(|s| s.kind == *bkind) {
                t.builder += 1;
                let pos = (sym.selection_span.start.row, sym.selection_span.start.column);
                // builder anchors variable selection on the varname
                // (post-sigil); the skeleton captures the whole sigiled
                // node — accept either anchor.
                let hit = skel_kinds.iter().any(|sk| {
                    skel_defs.contains(&(sk.to_string(), pos.0, pos.1))
                        || (pos.1 > 0 && skel_defs.contains(&(sk.to_string(), pos.0, pos.1 - 1)))
                });
                if hit {
                    t.matched += 1;
                } else {
                    // SEMANTIC synthesis can never come from a def
                    // pattern — bucket it by the recorded facts so the
                    // recall number speaks only about syntactic defs:
                    // plugin namespace, requires markers, and the
                    // has-accessor projection families.
                    let sym_id = crate::file_analysis::SymbolId(
                        fa.symbols.iter().position(|s2| std::ptr::eq(s2, sym)).unwrap() as u32,
                    );
                    let plugin_or_marker = !matches!(sym.namespace, crate::file_analysis::Namespace::Language)
                        || fa.contract_symbols.contains(&sym_id)
                        || fa.attr_projections.iter().any(|pr| {
                            pr.attr == sym.name
                                && Some(pr.class.as_str()) == sym.package.as_deref()
                        })
                        || sym.span.start == sym.span.end;
                    // Three-way bucket. A SYNTACTIC def in Perl is
                    // exactly `sub NAME` / `method NAME`; a name token
                    // present at the site without that keyword is
                    // syntax a PATTERN can't pair (use-constant tables,
                    // codegen loops); no name at the site at all is
                    // semantic synthesis.
                    let line = source.lines().nth(pos.0).unwrap_or("");
                    let before = &line[..pos.1.min(line.len())];
                    // the keyword probe only means something for subs/
                    // methods; other kinds are syntactic by default
                    let keyword_def = !matches!(bkind, SymKind::Sub | SymKind::Method)
                        || before.trim_end().ends_with("sub")
                        || before.trim_end().ends_with("method");
                    if plugin_or_marker || !keyword_def {
                        *synthesized_unmatched.entry(sym.name.clone()).or_default() += 1;
                    } else if t.misses.len() < 12 {
                        t.misses.push(format!(
                            "{}:{}:{} {} ({:?})",
                            path.file_name().unwrap().to_string_lossy(),
                            pos.0 + 1,
                            pos.1,
                            sym.name,
                            bkind,
                        ));
                    }
                }
            }
        }

        // refs: positions of call/method/var reads the builder records
        use crate::file_analysis::RefKind;
        let skel_ref_pos: HashSet<(usize, usize)> =
            skel.refs.iter().map(|r| (r.start.row, r.start.column)).collect();
        skel_ref_total += skel.refs.len();
        let mut builder_ref_pos: HashSet<(usize, usize)> = HashSet::new();
        for r in &fa.refs {
            if matches!(
                r.kind,
                RefKind::FunctionCall { .. } | RefKind::MethodCall { .. } | RefKind::Variable
            ) {
                builder_ref_pos.insert((r.span.start.row, r.span.start.column));
            }
        }
        for pos in &builder_ref_pos {
            ref_tally.builder += 1;
            // skeleton var refs anchor on the varname (post-sigil): a
            // builder `$x` ref at col c matches a skeleton ref at c+1.
            if skel_ref_pos.contains(pos) || skel_ref_pos.contains(&(pos.0, pos.1 + 1)) {
                ref_tally.matched += 1;
            }
        }
        skel_ref_extras += skel
            .refs
            .iter()
            .filter(|r| {
                !builder_ref_pos.contains(&(r.start.row, r.start.column))
                    && !(r.start.column > 0
                        && builder_ref_pos.contains(&(r.start.row, r.start.column - 1)))
            })
            .count();
    }

    println!("\n===== query-skeleton differential ({files} files) =====");
    for (_, _, label) in kind_map {
        let t = &tallies[label];
        println!(
            "  {:<9} builder {:>5}  matched {:>5}  recall {:>6.1}%",
            label,
            t.builder,
            t.matched,
            t.recall() * 100.0,
        );
        for m in &t.misses {
            println!("      miss: {m}");
        }
    }
    let synth_total: usize = synthesized_unmatched.values().sum();
    println!("  zero-syntax (synthesized) builder symbols, unreachable by ANY pattern: {synth_total}");
    println!(
        "  refs      builder {:>5}  matched {:>5}  recall {:>6.1}%  (skeleton extras: {} of {})",
        ref_tally.builder,
        ref_tally.matched,
        ref_tally.recall() * 100.0,
        skel_ref_extras,
        skel_ref_total,
    );
    assert_eq!(query_errors, 0, "query failed to compile/run somewhere");

    // Spike floor: the purely syntactic skeleton should all but
    // reproduce the builder's package/sub rows. These asserts make the
    // claim falsifiable rather than narrative.
    assert!(tallies["package"].recall() >= 0.95, "package recall");
    assert!(tallies["sub"].recall() >= 0.90, "sub recall");
}
#[test]
fn field_queryability_must_be_probed_per_node() {
    // THE trap this spike found: `variable:`/`variables:` fields the
    // CST prints (and child_by_field_name serves) match ZERO in the
    // query engine on variable_declaration — while `variable:` on
    // for_statement matches fine. Field queryability is per-node and
    // only observable by measuring; a .scm pack fails SILENTLY here.
    use tree_sitter::{Query, QueryCursor, StreamingIterator};
    let src = "my ($a, $b) = @_;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    for pat in [
        "(variable_declaration variables: (scalar) @v)",
        "(variable_declaration variables: [(scalar) (array) (hash)] @v)",
        "(variable_declaration (scalar) @v)",
    ] {
        let q = Query::new(&tree.language(), pat).unwrap();
        let mut c = QueryCursor::new();
        let mut ms = c.matches(&q, tree.root_node(), src.as_bytes());
        let mut n = 0;
        while let Some(m) = ms.next() {
            n += m.captures.len();
        }
        println!("PAT {pat} -> {n} captures");
        if pat.contains("variables:") {
            assert_eq!(n, 0, "if this now matches, the grammar fixed its field tables — simplify skeleton.scm");
        } else if !pat.contains("variables:") && !pat.contains("[") {
            assert_eq!(n, 2);
        }
    }
}
