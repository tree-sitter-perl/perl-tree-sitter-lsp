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

// ---- spike 2: the witness bag fed from captures alone ----

#[test]
fn walker_free_file_analysis_answers_type_queries() {
    // No builder anywhere in this test: captures → witnesses → a real
    // FileAnalysis → the production reducer registry answers.
    let src = "my $x = \"hello\";\nmy $n = 42;\nmy $h = {};\nmy $y = $x;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &perl_pack()).unwrap();
    let fa = skel.into_file_analysis();

    let end = tree_sitter::Point { row: 4, column: 0 };
    use crate::file_analysis::InferredType;
    assert_eq!(fa.inferred_type_via_bag("$x", end), Some(InferredType::String));
    assert_eq!(fa.inferred_type_via_bag("$n", end), Some(InferredType::Numeric));
    assert_eq!(fa.inferred_type_via_bag("$h", end), Some(InferredType::HashRef));
    // THE edge chase: $y → Variable($x) → Expr(literal) — three hops
    // through the production registry, zero walker code.
    assert_eq!(fa.inferred_type_via_bag("$y", end), Some(InferredType::String));
}

#[test]
fn python_pack_same_driver_same_engine() {
    // The cross-language existence proof: a different grammar, a
    // ~40-line query pack, the SAME driver and the SAME engine.
    let src = "\
import os

class Greeter:
    def greet(self, name):
        msg = \"hi\"
        return msg

x = \"hello\"
n: int = compute()
y = x
";
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_python::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &python_pack()).unwrap();

    // outline: class + method + module-level vars
    let names: Vec<(String, String)> = skel
        .symbols
        .iter()
        .map(|s| (s.kind.clone(), s.name.clone()))
        .collect();
    assert!(names.contains(&("class".into(), "Greeter".into())), "{names:?}");
    assert!(names.contains(&("sub".into(), "greet".into())), "{names:?}");
    assert!(names.contains(&("var".into(), "x".into())), "{names:?}");
    assert_eq!(skel.imports, vec!["os"]);

    let fa = skel.into_file_analysis();
    let end = tree_sitter::Point { row: 10, column: 0 };
    use crate::file_analysis::InferredType;
    // literal
    assert_eq!(fa.inferred_type_via_bag("x", end), Some(InferredType::String));
    // annotation — ring 3 in the tree, emitted by the pack predicate
    assert_eq!(fa.inferred_type_via_bag("n", end), Some(InferredType::Numeric));
    // edge chase across variables
    assert_eq!(fa.inferred_type_via_bag("y", end), Some(InferredType::String));
    // scoped local inside the method body
    let inside = tree_sitter::Point { row: 5, column: 8 };
    assert_eq!(fa.inferred_type_via_bag("msg", inside), Some(InferredType::String));
}

// ---- spike 3: cross-file through the production engine ----
//
// `resolve_imports_with_pack` lives HERE because the layering test
// (correctly) rejected it from query_extract.rs: import resolution is
// Index-layer work — in a real implementation it sits beside
// module_resolver (Index imports Build, never the reverse). The spike
// keeps it in the test suite, which is exempt by design.

/// Resolve a file's imports into a `ModuleIndex` — the GENERIC
/// cross-file loop. The only per-language inputs are the pack's
/// `module_paths` predicate and a parser factory; registration rides
/// `insert_cache`, which feeds the production reverse indexes
/// (`ModuleEdgeIndexes`), so `modules_with_symbol` /
/// `module_declaring_method_in_package` / the MRO walk all work
/// unchanged on pack-built modules.
fn resolve_imports_with_pack(
    imports: &[String],
    root: &std::path::Path,
    pack: &LangPack,
    mk_parser: &mut dyn FnMut() -> tree_sitter::Parser,
    idx: &crate::module_index::ModuleIndex,
) -> Result<(), String> {
    for module in imports {
        for rel in (pack.module_paths)(module) {
            let path = root.join(&rel);
            let Ok(source) = std::fs::read_to_string(&path) else { continue };
            let mut parser = mk_parser();
            let Some(tree) = parser.parse(&source, None) else { continue };
            let fa = extract(&tree, source.as_bytes(), pack)?.into_file_analysis();
            idx.insert_cache(
                module,
                Some(std::sync::Arc::new(crate::module_index::CachedModule::new(
                    path,
                    std::sync::Arc::new(fa),
                ))),
            );
            break;
        }
    }
    Ok(())
}



fn python_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
    p
}

fn python_fa(src: &str) -> (crate::file_analysis::FileAnalysis, Vec<String>) {
    let mut parser = python_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &python_pack()).unwrap();
    let imports = skel.imports.clone();
    (skel.into_file_analysis(), imports)
}

#[test]
fn python_cross_file_function_refs_through_refs_to() {
    // Two pack-built FileAnalyses in the production FileStore; the
    // production refs_to walks them — declaration in a.py, call in
    // b.py — with zero Perl and zero engine edits.
    let (fa_a, _) = python_fa("def helper(x):\n    return x\n");
    let (fa_b, _) = python_fa("from a import helper\n\nz = helper(1)\n");

    let store = crate::file_store::FileStore::new();
    let pa = std::path::PathBuf::from("/fake/py/a.py");
    let pb = std::path::PathBuf::from("/fake/py/b.py");
    store.insert_workspace(pa.clone(), fa_a);
    store.insert_workspace(pb.clone(), fa_b);

    let target = crate::resolve::TargetRef::new(
        "helper".into(),
        crate::resolve::TargetKind::Sub { package: None },
    );
    let locs = crate::resolve::refs_to(&store, None, &target, crate::resolve::RoleMask::EDITABLE);
    let by_file: Vec<(String, crate::file_analysis::AccessKind)> = locs
        .iter()
        .map(|l| {
            let f = match &l.key {
                crate::file_store::FileKey::Path(p) => {
                    p.file_name().unwrap().to_string_lossy().to_string()
                }
                crate::file_store::FileKey::Url(u) => u.to_string(),
            };
            (f, l.access)
        })
        .collect();
    assert!(
        by_file.contains(&("a.py".into(), crate::file_analysis::AccessKind::Declaration)),
        "expected the def in a.py, got {by_file:?}",
    );
    assert!(
        by_file.contains(&("b.py".into(), crate::file_analysis::AccessKind::Read)),
        "expected the call in b.py, got {by_file:?}",
    );
}

#[test]
fn python_cross_file_method_dispatch_through_mro_walk() {
    // The full chain: pack resolver (module_paths predicate) registers
    // a.py in the production ModuleIndex; b's bag types `g` via the
    // PEP8 constructor predicate; resolve_method_in_ancestors finds
    // greet on Greeter CROSS-FILE through the production index arms.
    let dir = std::env::temp_dir().join(format!("qx-spike-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("a.py"),
        "class Greeter:\n    def greet(self, name):\n        return name\n",
    )
    .unwrap();

    let src_b = "from a import Greeter\n\ng = Greeter()\nr = g.greet(\"x\")\n";
    let (fa_b, imports_b) = python_fa(src_b);

    let idx = crate::module_index::ModuleIndex::new_for_test();
    resolve_imports_with_pack(&imports_b, &dir, &python_pack(), &mut python_parser, &idx)
        .unwrap();

    use crate::file_analysis::InferredType;
    let end = tree_sitter::Point { row: 4, column: 0 };
    assert_eq!(
        fa_b.inferred_type_via_bag("g", end),
        Some(InferredType::ClassName("Greeter".into())),
        "constructor convention types the instance",
    );
    match fa_b.resolve_method_in_ancestors("Greeter", "greet", Some(&idx)) {
        Some(crate::file_analysis::MethodResolution::CrossFile { class, def_module }) => {
            assert_eq!(class, "Greeter");
            assert_eq!(def_module.as_deref(), Some("a"));
        }
        other => panic!("expected cross-file resolution of greet, got {other:?}"),
    }
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn dbg_operator_patterns() {
    use tree_sitter::{Query, QueryCursor, StreamingIterator};
    let src = "my $y = $x + 1;\nmy $s = $a . \"b\";\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    for pat in [
        r#"(binary_expression left: (scalar) @v "+")"#,
        r#"(binary_expression left: (scalar) @v operator: "+")"#,
        r#"(binary_expression left: (scalar) @v ".")"#,
    ] {
        match Query::new(&tree.language(), pat) {
            Ok(q) => {
                let mut c = QueryCursor::new();
                let mut ms = c.matches(&q, tree.root_node(), src.as_bytes());
                let mut n = 0;
                while let Some(m) = ms.next() { n += m.captures.len(); }
                println!("PAT {pat} -> {n}");
            }
            Err(e) => println!("PAT {pat} -> ERR {e}"),
        }
    }
}

#[test]
fn operator_evidence_types_through_usage() {
    // The operator-orientation answer: $x's initializer is unknowable,
    // but `$x + 1` observes it numeric and `$s . "!"` observes $s
    // stringy — usage-site evidence through the production fold.
    let src = "my $x = f();\nmy $s = g();\nmy $y = $x + 1;\nmy $t = $s . \"!\";\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let fa = extract(&tree, src.as_bytes(), &perl_pack())
        .unwrap()
        .into_file_analysis();
    let end = tree_sitter::Point { row: 4, column: 0 };
    use crate::file_analysis::InferredType;
    assert_eq!(fa.inferred_type_via_bag("$x", end), Some(InferredType::Numeric));
    assert_eq!(fa.inferred_type_via_bag("$s", end), Some(InferredType::String));
}

#[test]
fn python_workspace_rename_for_free() {
    // The exact LSP rename path — resolve_symbol at the cursor, then
    // refs_to, then text edits — over two pack-built Python files.
    let src_a = "def helper(x):\n    return x\n";
    let src_b = "from a import helper\n\nz = helper(1)\n";
    let (fa_a, _) = python_fa(src_a);
    let (fa_b, _) = python_fa(src_b);

    let store = crate::file_store::FileStore::new();
    let pa = std::path::PathBuf::from("/fake/py/a.py");
    let pb = std::path::PathBuf::from("/fake/py/b.py");
    store.insert_workspace(pa.clone(), fa_a);
    store.insert_workspace(pb.clone(), fa_b);

    // Cursor on the CALL in b.py (line 2, inside "helper").
    let fa_b_view = store.workspace_raw().get(&pb).unwrap().clone();
    let point = tree_sitter::Point { row: 2, column: 6 };
    let resolved = crate::resolve::resolve_symbol(&fa_b_view, point, None);
    let target = match resolved {
        Some(crate::resolve::ResolvedTarget::Target(t)) => t,
        other => panic!("expected a walkable target at the call, got {other:?}"),
    };
    assert_eq!(target.name, "helper");

    let locs = crate::resolve::refs_to(&store, None, &target, crate::resolve::RoleMask::EDITABLE);
    assert_eq!(locs.len(), 3, "decl + import spelling + call, got {locs:?}");

    // Apply the edits the way backend::rename does: replace each span
    // with the new name, per file, right-to-left.
    let mut texts: std::collections::HashMap<std::path::PathBuf, String> =
        [(pa.clone(), src_a.to_string()), (pb.clone(), src_b.to_string())].into();
    for loc in &locs {
        let crate::file_store::FileKey::Path(p) = &loc.key else { panic!() };
        let text = texts.get_mut(p).unwrap();
        let lines: Vec<&str> = text.lines().collect();
        let line = lines[loc.span.start.row];
        let mut new_line = line.to_string();
        new_line.replace_range(loc.span.start.column..loc.span.end.column, "fetch_all");
        let mut new_lines: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
        new_lines[loc.span.start.row] = new_line;
        *text = new_lines.join("\n") + "\n";
    }
    assert_eq!(texts[&pa], "def fetch_all(x):\n    return x\n");
    assert_eq!(texts[&pb], "from a import fetch_all\n\nz = fetch_all(1)\n");
}


#[test]
fn dbg_r_cst() {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_r::LANGUAGE.into()).unwrap();
    let src = "library(dplyr)\nsource(\"util.R\")\nf <- function(x) {\n  y <- x + 1\n  y\n}\ndf <- data.frame(age = c(30), name = c(\"ada\"))\nm <- df$age\nprint.myclass <- function(obj) obj\n";
    let tree = p.parse(src, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}

// ---- spike 4: the R pack ----

fn r_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_r::LANGUAGE.into()).unwrap();
    p
}

fn r_fa(src: &str) -> (crate::file_analysis::FileAnalysis, Vec<String>, SkeletonAnalysis) {
    let mut parser = r_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &r_pack()).unwrap();
    let imports = skel.imports.clone();
    let symbols = SkeletonAnalysis {
        symbols: skel.symbols.clone(),
        ..Default::default()
    };
    (skel.into_file_analysis(), imports, symbols)
}

#[test]
fn r_outline_imports_and_s3_names() {
    let src = "library(dplyr)\nsource(\"util.R\")\n\nadd_one <- function(x) {\n  y <- x + 1\n  y\n}\n\nprint.myclass <- function(obj) obj\n";
    let (_fa, imports, skel) = r_fa(src);
    assert_eq!(imports, vec!["dplyr", "util.R"]);
    let subs: Vec<&str> = skel
        .symbols
        .iter()
        .filter(|s| s.kind == "sub")
        .map(|s| s.name.as_str())
        .collect();
    // S3 method names fall out of the def pattern verbatim — the
    // convention layer (print.myclass IS print on myclass) is a later
    // pack predicate, but the outline is already right.
    assert_eq!(subs, vec!["add_one", "print.myclass"]);
    // params + locals are vars, the function names are NOT doubled
    let vars: Vec<&str> = skel
        .symbols
        .iter()
        .filter(|s| s.kind == "var")
        .map(|s| s.name.as_str())
        .collect();
    assert_eq!(vars, vec!["x", "y", "obj"]);
}

#[test]
fn r_data_frame_columns_are_static_keys() {
    // THE feature no R tooling has statically: df's columns as part of
    // its TYPE, from the data.frame literal, through the production
    // witness engine.
    let src = "df <- data.frame(age = c(30, 41), name = c(\"ada\", \"grace\"))\nlater <- 1\n";
    let (fa, _, _) = r_fa(src);
    let end = tree_sitter::Point { row: 1, column: 0 };
    use crate::file_analysis::InferredType;
    match fa.inferred_type_via_bag("df", end) {
        Some(InferredType::HashWithKeys { keys, open }) => {
            let names: Vec<&str> = keys.iter().map(|(k, _)| k.as_str()).collect();
            assert_eq!(names, vec!["age", "name"]);
            assert!(!open);
        }
        other => panic!("expected the column shape, got {other:?}"),
    }
}

#[test]
fn r_cross_file_refs_and_workspace_rename() {
    // util.R defines fetch_data; analysis.R sources it and calls it.
    // refs_to + rename across files, R-flavored resolution = the
    // source() path string, verbatim.
    let dir = std::env::temp_dir().join(format!("qx-r-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let src_util = "fetch_data <- function(path) {\n  path\n}\n";
    std::fs::write(dir.join("util.R"), src_util).unwrap();
    let src_main = "source(\"util.R\")\n\nresult <- fetch_data(\"db.csv\")\n";

    let (fa_main, imports, _) = r_fa(src_main);
    let (fa_util, _, _) = r_fa(src_util);

    // cross-file registration through the pack resolver
    let idx = crate::module_index::ModuleIndex::new_for_test();
    resolve_imports_with_pack(&imports, &dir, &r_pack(), &mut r_parser, &idx).unwrap();
    assert!(
        idx.get_cached("util.R").is_some(),
        "source() path registered as a module",
    );

    // rename across the two files via the production path
    let store = crate::file_store::FileStore::new();
    let pm = std::path::PathBuf::from("/fake/r/analysis.R");
    let pu = std::path::PathBuf::from("/fake/r/util.R");
    store.insert_workspace(pm.clone(), fa_main);
    store.insert_workspace(pu.clone(), fa_util);

    let point = tree_sitter::Point { row: 2, column: 12 }; // on fetch_data call
    let fa_view = store.workspace_raw().get(&pm).unwrap().clone();
    let target = match crate::resolve::resolve_symbol(&fa_view, point, None) {
        Some(crate::resolve::ResolvedTarget::Target(t)) => t,
        other => panic!("expected target, got {other:?}"),
    };
    assert_eq!(target.name, "fetch_data");
    let locs = crate::resolve::refs_to(&store, None, &target, crate::resolve::RoleMask::EDITABLE);
    let files: Vec<String> = locs
        .iter()
        .map(|l| match &l.key {
            crate::file_store::FileKey::Path(p) => {
                p.file_name().unwrap().to_string_lossy().to_string()
            }
            _ => unreachable!(),
        })
        .collect();
    assert!(files.contains(&"util.R".to_string()), "decl found: {files:?}");
    assert!(files.contains(&"analysis.R".to_string()), "call found: {files:?}");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn dbg_cmake_cst() {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_cmake::LANGUAGE.into()).unwrap();
    let src = "set(MY_FLAG ON)\ninclude(util.cmake)\nadd_subdirectory(src)\nfunction(register_widget name)\n  message(STATUS \"${name} ${MY_FLAG}\")\nendfunction()\nadd_library(widgets STATIC a.c)\ntarget_link_libraries(widgets PRIVATE core)\nregister_widget(button)\n";
    let tree = p.parse(src, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}

// ---- spike 5: the CMake pack ----

fn cmake_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_cmake::LANGUAGE.into()).unwrap();
    p
}

fn cmake_skel(src: &str) -> SkeletonAnalysis {
    let mut parser = cmake_parser();
    let tree = parser.parse(src, None).unwrap();
    extract(&tree, src.as_bytes(), &cmake_pack()).unwrap()
}

#[test]
fn cmake_outline_targets_vars_and_interpolated_refs() {
    let src = "set(MY_FLAG ON)\ninclude(util.cmake)\nadd_subdirectory(src)\n\
               function(register_widget name)\n  message(STATUS \"${name} ${MY_FLAG}\")\nendfunction()\n\
               add_library(widgets STATIC a.c)\ntarget_link_libraries(widgets PRIVATE core)\nregister_widget(button)\n";
    let skel = cmake_skel(src);

    let defs: Vec<(String, String)> = skel
        .symbols
        .iter()
        .map(|s| (s.kind.clone(), s.name.clone()))
        .collect();
    assert!(defs.contains(&("var".into(), "MY_FLAG".into())), "{defs:?}");
    assert!(defs.contains(&("sub".into(), "register_widget".into())), "{defs:?}");
    assert!(defs.contains(&("sub".into(), "widgets".into())), "target def: {defs:?}");
    assert!(defs.contains(&("var".into(), "name".into())), "param def: {defs:?}");
    assert_eq!(skel.imports, vec!["util.cmake", "src"]);

    // ${MY_FLAG} INSIDE a quoted string is a real var ref — Perl's
    // regex-interpolation work, free.
    assert!(
        skel.refs.iter().any(|r| r.kind == "var" && r.name == "MY_FLAG"),
        "interpolated var ref: {:?}",
        skel.refs.iter().filter(|r| r.kind == "var").collect::<Vec<_>>(),
    );
    // target_link_libraries args reference targets; PRIVATE is a
    // keyword and must not become a ref
    assert!(skel.refs.iter().any(|r| r.kind == "call" && r.name == "widgets"));
    assert!(skel.refs.iter().any(|r| r.kind == "call" && r.name == "core"));
    assert!(!skel.refs.iter().any(|r| r.name == "PRIVATE"));
}

#[test]
fn cmake_cross_file_function_rename_and_subdirectory_resolution() {
    let dir = std::env::temp_dir().join(format!("qx-cmake-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("src")).unwrap();
    let src_util = "function(register_widget name)\n  message(STATUS \"${name}\")\nendfunction()\n";
    std::fs::write(dir.join("util.cmake"), src_util).unwrap();
    std::fs::write(dir.join("src/CMakeLists.txt"), "add_library(inner STATIC b.c)\n").unwrap();
    let src_root = "include(util.cmake)\nadd_subdirectory(src)\n\nregister_widget(button)\n";

    let root_skel = cmake_skel(src_root);
    let imports = root_skel.imports.clone();
    let fa_root = root_skel.into_file_analysis();
    let fa_util = cmake_skel(src_util).into_file_analysis();

    // resolution: util.cmake verbatim, src → src/CMakeLists.txt
    let idx = crate::module_index::ModuleIndex::new_for_test();
    resolve_imports_with_pack(&imports, &dir, &cmake_pack(), &mut cmake_parser, &idx).unwrap();
    assert!(idx.get_cached("util.cmake").is_some());
    assert!(
        idx.get_cached("src").is_some(),
        "add_subdirectory(src) must resolve src/CMakeLists.txt",
    );

    // workspace rename of the function, cursor on the call site
    let store = crate::file_store::FileStore::new();
    let pr = std::path::PathBuf::from("/fake/cmake/CMakeLists.txt");
    let pu = std::path::PathBuf::from("/fake/cmake/util.cmake");
    store.insert_workspace(pr.clone(), fa_root);
    store.insert_workspace(pu.clone(), fa_util);

    let fa_view = store.workspace_raw().get(&pr).unwrap().clone();
    let point = tree_sitter::Point { row: 3, column: 4 };
    let target = match crate::resolve::resolve_symbol(&fa_view, point, None) {
        Some(crate::resolve::ResolvedTarget::Target(t)) => t,
        other => panic!("expected target, got {other:?}"),
    };
    assert_eq!(target.name, "register_widget");
    let locs = crate::resolve::refs_to(&store, None, &target, crate::resolve::RoleMask::EDITABLE);
    let files: Vec<String> = locs
        .iter()
        .map(|l| match &l.key {
            crate::file_store::FileKey::Path(p) => {
                p.file_name().unwrap().to_string_lossy().to_string()
            }
            _ => unreachable!(),
        })
        .collect();
    assert!(files.contains(&"util.cmake".to_string()), "def: {files:?}");
    assert!(files.contains(&"CMakeLists.txt".to_string()), "call: {files:?}");
    std::fs::remove_dir_all(&dir).ok();
}
