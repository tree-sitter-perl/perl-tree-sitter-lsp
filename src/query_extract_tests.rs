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
                // Exact column: the builder anchors a variable's
                // selection on the sigil, and the skeleton captures the
                // whole sigiled node — both start at the sigil, so no
                // anchor fudge is needed (an earlier `col - 1` tolerance
                // papered over a mis-stated anchor; see skeleton.scm).
                let hit = skel_kinds
                    .iter()
                    .any(|sk| skel_defs.contains(&(sk.to_string(), pos.0, pos.1)));
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
    // The corrected story behind skeleton.scm's variable-def patterns.
    // The earlier finding claimed the `variable:`/`variables:` fields on
    // variable_declaration both "match ZERO in the query engine"; that
    // was a long-standing mis-measurement (the sibling for_statement
    // `variable:` matches fine — nobody cross-checked). The real,
    // narrower shape, proved here with numbers:
    //   - single `variable:` resolves to the (scalar)/(array)/(hash)
    //     node and IS queryable;
    //   - paren-list `variables:` resolves in the query engine to the
    //     anonymous `(` token (same field-table trap as `right:` on
    //     assignment_expression), so a NAMED-node matcher under it finds
    //     nothing — but `variables: _` binds the paren and the inner
    //     vars are reachable as siblings.
    use tree_sitter::{Query, QueryCursor, StreamingIterator};
    let src = "my $x = 1;\nmy ($a, $b) = @_;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let count = |pat: &str| -> usize {
        let q = Query::new(&tree.language(), pat).unwrap();
        let mut c = QueryCursor::new();
        let mut ms = c.matches(&q, tree.root_node(), src.as_bytes());
        let mut n = 0;
        while let Some(m) = ms.next() {
            n += m.captures.len();
        }
        n
    };
    // single field: queryable, resolves to the named var node.
    assert_eq!(count("(variable_declaration variable: (scalar (varname) @v))"), 1);
    // paren-list field: a named-node matcher under it finds nothing,
    // because the field resolves to the anonymous `(` token...
    assert_eq!(count("(variable_declaration variables: (scalar) @v)"), 0);
    assert_eq!(count("(variable_declaration variables: (_) @v)"), 0);
    // ...which `variables: _` (wildcard, matches anon nodes too) binds:
    // exactly one paren per paren-list declaration.
    assert_eq!(count("(variable_declaration variables: _ @v)"), 1);
    // Both fields CAN be reached if wanted (single directly, paren-list
    // via the paren-anchored sibling) — but skeleton.scm needs neither:
    // the field-less discriminator `(_ (varname))` covers both spellings
    // in one pattern (the single form here, plus the two paren-list vars).
    assert_eq!(count("(variable_declaration variable: (_ (varname) @v))"), 1);
    assert_eq!(count("(variable_declaration variables: _ (_ (varname) @v))"), 2);
    assert_eq!(count("(variable_declaration (_ (varname) @v))"), 3);
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

// ---- C++ obstacle course: measure macro-induced parse damage ----

#[path = "cpp_obstacle.rs"]
mod cpp_obstacle;

fn cpp_parser() -> tree_sitter::Parser {
    let mut p = tree_sitter::Parser::new();
    p.set_language(&tree_sitter_cpp::LANGUAGE.into()).unwrap();
    p
}

/// (error_nodes, missing_nodes, deepest_error_byte_span_total)
fn count_damage(node: tree_sitter::Node) -> (usize, usize, usize) {
    let mut errors = 0;
    let mut missing = 0;
    let mut err_bytes = 0;
    let mut cursor = node.walk();
    let mut stack = vec![node];
    while let Some(n) = stack.pop() {
        if n.is_error() {
            errors += 1;
            err_bytes += n.end_byte() - n.start_byte();
        }
        if n.is_missing() {
            missing += 1;
        }
        for c in n.children(&mut cursor) {
            stack.push(c);
        }
    }
    (errors, missing, err_bytes)
}

/// Does the identifier `name` appear ANYWHERE as a named leaf in the
/// tree? (i.e. is there at least a token the skeleton could capture).
fn name_present(node: tree_sitter::Node, src: &[u8], name: &str) -> bool {
    let mut cursor = node.walk();
    let mut stack = vec![node];
    while let Some(n) = stack.pop() {
        if n.named_child_count() == 0 && n.utf8_text(src).ok() == Some(name) {
            return true;
        }
        for c in n.children(&mut cursor) {
            stack.push(c);
        }
    }
    false
}

#[test]
fn cpp_obstacle_course_damage_report() {
    use cpp_obstacle::SAMPLES;
    let mut parser = cpp_parser();
    println!("\n===== C++ macro obstacle course: parse-damage report =====");
    println!(
        "{:<18} {:>6} {:>7} {:>8}  {:>5}  expected-names-reachable",
        "sample", "errors", "missing", "err-byte", "names"
    );
    let mut total_err = 0;
    let mut total_reach = 0;
    let mut total_names = 0;
    for s in SAMPLES {
        let tree = parser.parse(s.src, None).unwrap();
        let root = tree.root_node();
        let (errors, missing, err_bytes) = count_damage(root);
        let reachable: Vec<&str> = s
            .expected
            .iter()
            .copied()
            .filter(|n| name_present(root, s.src.as_bytes(), n))
            .collect();
        let lost: Vec<&str> = s
            .expected
            .iter()
            .copied()
            .filter(|n| !reachable.contains(n))
            .collect();
        total_err += errors;
        total_reach += reachable.len();
        total_names += s.expected.len();
        println!(
            "{:<18} {:>6} {:>7} {:>8}  {:>2}/{:<2}  lost: {:?}",
            s.name,
            errors,
            missing,
            err_bytes,
            reachable.len(),
            s.expected.len(),
            lost,
        );
    }
    println!(
        "----- totals: {total_err} error nodes; {total_reach}/{total_names} \
         expected names reachable as tokens -----\n"
    );
    // The report is the deliverable; this guard just ensures the corpus
    // parses at all (no panic) and the baseline is clean.
    let clean = &SAMPLES[0];
    let tree = parser.parse(clean.src, None).unwrap();
    let (errors, _, _) = count_damage(tree.root_node());
    assert_eq!(errors, 0, "clean baseline must parse error-free");
}


#[test]
fn dbg_cpp_attr_probe() {
    let mut parser = cpp_parser();
    let cases = [
        ("expand-to-attr", "class __attribute__((visibility(\"default\"))) Widget {\npublic:\n  void draw();\n};\n"),
        ("strip-empty",     "class  Widget {\npublic:\n  void draw();\n};\n"),
        ("declspec-attr",   "class __declspec(dllexport) Widget {\npublic:\n  void draw();\n};\n"),
    ];
    for (name, src) in cases {
        let tree = parser.parse(src, None).unwrap();
        let (errors, missing, _) = count_damage(tree.root_node());
        println!("\n--- {name}: errors={errors} missing={missing} ---\n{}", tree.root_node().to_sexp());
    }
}

#[test]
fn dbg_cpp_cst() {
    use cpp_obstacle::SAMPLES;
    let mut parser = cpp_parser();
    for s in SAMPLES {
        let tree = parser.parse(s.src, None).unwrap();
        println!("\n========== {} ==========\n{}", s.name, tree.root_node().to_sexp());
    }
}

fn cpp_skel(src: &str) -> SkeletonAnalysis {
    let mut parser = cpp_parser();
    let tree = parser.parse(src, None).unwrap();
    extract(&tree, src.as_bytes(), &cpp_pack()).unwrap()
}

#[test]
fn cpp_tier1_extraction_report() {
    use cpp_obstacle::SAMPLES;
    println!("\n===== C++ Tier-1 skeleton extraction (pack through driver) =====");
    let mut got_total = 0;
    let mut exp_total = 0;
    for s in SAMPLES {
        let skel = cpp_skel(s.src);
        let names: Vec<String> = skel.symbols.iter().map(|s| s.name.clone()).collect();
        let hit: Vec<&str> = s.expected.iter().copied().filter(|n| names.iter().any(|g| g == n)).collect();
        let miss: Vec<&str> = s.expected.iter().copied().filter(|n| !hit.contains(n)).collect();
        got_total += hit.len();
        exp_total += s.expected.len();
        let kinds: Vec<String> = skel
            .symbols
            .iter()
            .map(|s| format!("{}:{}", s.kind, s.name))
            .collect();
        println!(
            "{:<18} {:>2}/{:<2}  miss: {:<28}  extracted: {:?}",
            s.name,
            hit.len(),
            s.expected.len(),
            format!("{miss:?}"),
            kinds,
        );
    }
    println!("----- Tier-1 recall: {got_total}/{exp_total} expected symbols extracted -----\n");
}

#[test]
fn cpp_clean_baseline_outline() {
    let skel = cpp_skel(cpp_obstacle::SAMPLES[0].src);
    let defs: Vec<(String, String)> =
        skel.symbols.iter().map(|s| (s.kind.clone(), s.name.clone())).collect();
    assert!(defs.contains(&("class".into(), "Shape".into())), "{defs:?}");
    assert!(defs.contains(&("class".into(), "Circle".into())), "{defs:?}");
    assert!(defs.contains(&("method".into(), "area".into())), "{defs:?}");
    assert!(defs.contains(&("var".into(), "radius".into())), "{defs:?}");
    assert!(defs.contains(&("sub".into(), "main".into())), "{defs:?}");
    // namespace stickiness: Shape/Circle carry package=geo
    let shape = skel.symbols.iter().find(|s| s.name == "Shape").unwrap();
    assert_eq!(shape.package.as_deref(), Some("geo"), "namespace context");
}

#[test]
fn cpp_walker_free_type_queries() {
    // The same proof the Python pack gives, on C++ syntax: a REAL
    // FileAnalysis built from capture events alone answers type queries
    // through the production witness bag + reducer registry. C++'s leak
    // is declared types — pervasive, so annot_type carries the load.
    let src = "\
namespace geo { class Circle { public: double area(); }; }
int main() {
    int n = 5;
    std::string s = \"hi\";
    geo::Circle c;
    auto m = n;
    return 0;
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    // cursor on `return 0;` (row 6) — past every declaration. Queries
    // are TEMPORAL: a variable has no type before its declaration point,
    // which is exactly the production rule (no guessing the future).
    let inside = tree_sitter::Point { row: 6, column: 4 };
    assert_eq!(fa.inferred_type_via_bag("n", inside), Some(InferredType::Numeric), "int decl");
    assert_eq!(fa.inferred_type_via_bag("s", inside), Some(InferredType::String), "std::string decl");
    // The namespace qualifier is stripped — classes/members are keyed by
    // the unqualified name (@context.class), so `geo::Circle` types as
    // `Circle` and member completion resolves through the hierarchy.
    assert_eq!(
        fa.inferred_type_via_bag("c", inside),
        Some(InferredType::ClassName("Circle".into())),
        "class-instance decl",
    );
    // `auto m = n` — the cross-variable edge chase: m → Expr(n) →
    // Variable(n) → Numeric, through the production registry untouched.
    assert_eq!(fa.inferred_type_via_bag("m", inside), Some(InferredType::Numeric), "auto edge chase");
    // and temporality holds: before its declaration, n has no type
    assert_eq!(
        fa.inferred_type_via_bag("n", tree_sitter::Point { row: 1, column: 0 }),
        None,
        "no type before declaration",
    );
}

#[test]
fn cpp_type_inference_through_macro_reparse() {
    // The whole stack composes. A declarator-position macro DESTROYS the
    // class (it reparses as a function), so the class SYMBOL — the thing
    // goto-def / members / inheritance need — does not exist. `b`'s
    // *nominal* type (`ClassName("Box")`) survives, because it comes from
    // b's own declaration, but it points at nothing. Reparse via
    // expansion recovers the class symbol and its fields, making that
    // nominal type RESOLVABLE; the bag then types `b` and the edge chase
    // `auto same = b` on the reparsed tree.
    let mut parser = cpp_parser();
    let src = "\
#define API_EXPORT __attribute__((visibility(\"default\")))
class API_EXPORT Box {
public:
    int width;
};
int main() {
    Box b;
    auto same = b;
    return 0;
}
";
    // pre-reparse: the class SYMBOL evaporated (only its corrupted
    // remains), so navigation to `Box` and its `width` field is dead.
    let raw_defs: Vec<(String, String)> = cpp_skel(src)
        .symbols
        .iter()
        .map(|s| (s.kind.clone(), s.name.clone()))
        .collect();
    assert!(!raw_defs.contains(&("class".into(), "Box".into())), "class lost pre-reparse: {raw_defs:?}");

    // reparse (validated macro expansion), then extract + query
    let (rewritten, _map) = crate::cpp_reparse::preprocess_validated(&mut parser, src);
    let skel = cpp_skel(&rewritten);
    let defs: Vec<(String, String)> = skel.symbols.iter().map(|s| (s.kind.clone(), s.name.clone())).collect();
    assert!(defs.contains(&("class".into(), "Box".into())), "class recovered: {defs:?}");
    assert!(defs.contains(&("var".into(), "width".into())), "field recovered: {defs:?}");

    use crate::file_analysis::InferredType;
    let fa = skel.into_file_analysis();
    let pt = tree_sitter::Point { row: 8, column: 4 }; // on `return 0;`
    assert_eq!(
        fa.inferred_type_via_bag("b", pt),
        Some(InferredType::ClassName("Box".into())),
        "b types as the recovered class through the bag",
    );
    // the edge chase rides on top of the macro-reparsed tree
    assert_eq!(
        fa.inferred_type_via_bag("same", pt),
        Some(InferredType::ClassName("Box".into())),
        "auto edge chase on a macro-recovered class",
    );
}

#[test]
fn cpp_cross_file_include_resolution_and_rename() {
    let dir = std::env::temp_dir().join(format!("qx-cpp-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let header = "int compute(int x);\n";
    std::fs::write(dir.join("util.h"), header).unwrap();
    let main_src = "#include \"util.h\"\n\nint main() {\n    return compute(41);\n}\n";

    let main_skel = cpp_skel(main_src);
    let imports = main_skel.imports.clone();
    assert_eq!(imports, vec!["util.h"], "quoted include captured clean: {imports:?}");

    let idx = crate::module_index::ModuleIndex::new_for_test();
    resolve_imports_with_pack(&imports, &dir, &cpp_pack(), &mut cpp_parser, &idx).unwrap();
    assert!(idx.get_cached("util.h").is_some(), "#include must resolve util.h");

    // cross-file rename: cursor on the call `compute` in main → def in
    // util.h + call in main, through the production resolve/refs path
    let store = crate::file_store::FileStore::new();
    let pm = std::path::PathBuf::from("/fake/cpp/main.cpp");
    let ph = std::path::PathBuf::from("/fake/cpp/util.h");
    store.insert_workspace(pm.clone(), main_skel.into_file_analysis());
    store.insert_workspace(ph.clone(), cpp_skel(header).into_file_analysis());

    let fa_view = store.workspace_raw().get(&pm).unwrap().clone();
    let point = tree_sitter::Point { row: 3, column: 11 };
    let target = match crate::resolve::resolve_symbol(&fa_view, point, None) {
        Some(crate::resolve::ResolvedTarget::Target(t)) => t,
        other => panic!("expected target, got {other:?}"),
    };
    assert_eq!(target.name, "compute");
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
    assert!(files.contains(&"util.h".to_string()), "def in header: {files:?}");
    assert!(files.contains(&"main.cpp".to_string()), "call in main: {files:?}");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn cpp_cross_file_enum_variant_goto_def() {
    // A bare enum-constant use (`OP_SCOPE`) resolves cross-file to its
    // enumerator def, even though the enum-hover pass stamps the parent enum
    // as the constant's `package` (a type annotation, for `RED: Color` hover).
    // Two mechanisms cooperate: register_symbols keys file-scope values on
    // File scope (not package absence), so the constant is findable by name;
    // and the use mints an unresolved `Variable` ref that the query-time
    // cross-file tail chases by that name.
    use crate::file_analysis::RefKind;
    let header = "enum opcode {\n    OP_NULL,\n    OP_SCOPE,\n};\n";
    let header_fa = cpp_skel(header).into_file_analysis();
    let op = header_fa.symbols.iter().find(|s| s.name == "OP_SCOPE").unwrap();
    assert_eq!(op.package.as_deref(), Some("opcode"),
        "enum constant carries its enum as the (type) package");

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let hpath = std::path::PathBuf::from("/fake/cpp/opcodes.h");
    idx.register_symbols(hpath, std::sync::Arc::new(header_fa));
    assert!(idx.get_cached("OP_SCOPE").is_some(),
        "packaged file-scope enum constant is registered by name");

    let use_src = "int is_scope(int t) {\n    return t == OP_SCOPE;\n}\n";
    let use_fa = cpp_skel(use_src).into_file_analysis();
    assert!(
        use_fa.refs.iter().any(|r| matches!(r.kind, RefKind::Variable)
            && r.target_name == "OP_SCOPE"
            && r.resolves_to.is_none()),
        "a use with no LOCAL decl still mints an unresolved Variable ref"
    );

    let uri = tower_lsp::lsp_types::Url::from_file_path("/fake/cpp/use.c").unwrap();
    let pos = tower_lsp::lsp_types::Position { line: 1, character: 16 };
    let resp = crate::symbols::find_definition(&use_fa, pos, &uri, &idx)
        .expect("cross-file enum-variant goto-def resolves");
    let loc = match resp {
        tower_lsp::lsp_types::GotoDefinitionResponse::Scalar(l) => l,
        other => panic!("expected a single location, got {other:?}"),
    };
    assert!(loc.uri.path().ends_with("opcodes.h"), "lands in the header: {}", loc.uri);
    assert_eq!(loc.range.start.line, 2, "lands on the OP_SCOPE enumerator (line 3)");
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




#[test]
fn python_isinstance_narrows_within_the_guard() {
    // `if isinstance(x, Foo): ...` refines x to Foo INSIDE the block and
    // nowhere else — guard narrowing via scoped witness.
    let src = "\
class Foo:
    def run(self):
        pass
x = maybe()
if isinstance(x, Foo):
    x.run()
y = 1
";
    let mut parser = python_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &python_pack()).unwrap();
    let fa = skel.into_file_analysis();
    use crate::file_analysis::InferredType;
    // inside the guard (row 5, `x.run()`): x is Foo
    assert_eq!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 5, column: 4 }),
        Some(InferredType::ClassName("Foo".into())),
        "narrowed inside the guard",
    );
    // after the guard (row 6): the refinement does not leak
    assert_eq!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 6, column: 0 }),
        None,
        "narrowing scoped to the block — gone after",
    );
}

#[test]
fn python_narrowing_truncates_at_reassignment() {
    // THE cross-language cutoff: `isinstance` narrows x to Foo, then a
    // reassignment INSIDE the block rebinds it — x must stop being Foo after
    // the rebind (the edge-driven cutoff, same as Perl). Pre-lift, the scoped
    // witness leaked Foo to the whole block.
    let src = "\
class Foo:
    def run(self):
        pass
x = maybe()
if isinstance(x, Foo):
    x.run()
    x = other()
    x.run()
y = 1
";
    let mut parser = python_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &python_pack()).unwrap();
    let fa = skel.into_file_analysis();
    use crate::file_analysis::InferredType;
    assert_eq!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 5, column: 4 }),
        Some(InferredType::ClassName("Foo".into())),
        "narrowed before the reassignment",
    );
    assert_ne!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 7, column: 4 }),
        Some(InferredType::ClassName("Foo".into())),
        "reassignment rebinds x → narrowing truncated by the cross-language cutoff",
    );
}

#[test]
fn python_for_loop_var_rebind_truncates_narrowing() {
    // A `for x in …` loop INSIDE the guard rebinds x (the loop var leaks in
    // Python) — so the narrowing ends at the loop, via the bind-shape Rebind
    // edge feeding the cutoff.
    let src = "\
class Foo:
    def run(self):
        pass
x = maybe()
if isinstance(x, Foo):
    x.run()
    for x in items:
        pass
    x.run()
";
    let mut parser = python_parser();
    let tree = parser.parse(src, None).unwrap();
    let skel = extract(&tree, src.as_bytes(), &python_pack()).unwrap();
    let fa = skel.into_file_analysis();
    use crate::file_analysis::InferredType;
    assert_eq!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 5, column: 4 }),
        Some(InferredType::ClassName("Foo".into())),
        "narrowed before the loop",
    );
    assert_ne!(
        fa.inferred_type_via_bag("x", tree_sitter::Point { row: 8, column: 4 }),
        Some(InferredType::ClassName("Foo".into())),
        "the loop var rebinds x → narrowing truncated",
    );
}

#[test]
fn cpp_use_after_move_flags_read_before_reassign() {
    // `std::move(x)` leaves x moved-from: the row-3 read is a bug; the row-5
    // read is fine because `x = other()` reassigns it (the FlowEdge rebind
    // ends the moved-from region, same cutoff as narrowing).
    let src = "\
void f() {
  Widget x;
  sink(std::move(x));
  x.use();
  x = other();
  x.use();
}
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(diags.len(), 1, "exactly one use-after-move: {diags:?}");
    // the flagged read is the row-3 `x.use()` receiver, not the row-5 one.
    assert_eq!(diags[0].range.start.line, 3, "row-3 read flagged: {diags:?}");
    assert!(
        diags.iter().all(|d| d.range.start.line != 5),
        "row-5 read (post-reassign) is clean: {diags:?}",
    );
}

#[test]
fn cpp_conditional_move_does_not_flag_sibling_arm() {
    // GOAL-2.1: if/else arms are their own @scope now, so a `std::move` in the
    // `if` arm bounds its moved-from region to that arm — the read of `x` in the
    // `else` arm (and after the if) is a different scope subtree, not flagged.
    let src = "\
void f(bool c) {
  Widget x;
  if (c) {
    sink(std::move(x));
  } else {
    x.use();
  }
  x.use();
}
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(
        diags.len(),
        0,
        "the move is scoped to the if-arm — sibling else / post-if reads are clean: {diags:?}",
    );
}

#[test]
fn cpp_conditional_move_still_flags_same_arm_read() {
    // The arm scoping must not over-suppress: a read AFTER the move IN THE SAME
    // arm is still the real use-after-move bug.
    let src = "\
void f(bool c) {
  Widget x;
  if (c) {
    sink(std::move(x));
    x.use();
  }
}
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(diags.len(), 1, "same-arm read after move is still flagged: {diags:?}");
    assert_eq!(diags[0].range.start.line, 4, "the row-4 same-arm read: {diags:?}");
}

#[test]
fn cpp_reset_via_method_ends_moved_from_region() {
    // GOAL-2.2: a rebinding method call (`x.clear()`) puts the moved-from object
    // back into a known state — the moved-from region ends at the reset, so the
    // reset's own receiver read AND the later `x.use()` are clean.
    let src = "\
void f() {
  Widget x;
  sink(std::move(x));
  x.clear();
  x.use();
}
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(
        diags.len(),
        0,
        "x.clear() rebinds x — the reset and the post-reset use are clean: {diags:?}",
    );
}

#[test]
fn cpp_non_reset_method_after_move_still_flags() {
    // The rebind-method set is specific: an ordinary method call (`x.use()`) is
    // NOT a reset, so the canonical use-after-move still flags.
    let src = "\
void f() {
  Widget x;
  sink(std::move(x));
  x.use();
}
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(diags.len(), 1, "non-reset use after move still flags: {diags:?}");
}

#[test]
fn cpp_dynamic_cast_guard_narrows() {
    // `if (dynamic_cast<Derived*>(b))` refines b to Derived inside the block —
    // the cpp analog of python isinstance, via the now-wired narrow_guard.
    let src = "\
void f(Base* b) {
    if (dynamic_cast<Derived*>(b)) {
        b->run();
    }
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    assert_eq!(
        fa.inferred_type_via_bag("b", tree_sitter::Point { row: 2, column: 8 }),
        Some(InferredType::ClassName("Derived".into())),
        "dynamic_cast narrows b to Derived inside the guard",
    );
}

#[test]
fn cpp_optional_engaged_guard_narrows_to_inner_type() {
    // Guard-testing a `std::optional<Widget>` as engaged proves it holds a
    // Widget inside the block, so `opt->` / `*opt` resolve on Widget there.
    // Two engagement shapes — bare truthiness `if (opt)` and `if
    // (opt.has_value())` — both peel T off the DECLARED optional type; the
    // refinement is gone after the block and dies at a reassignment (the same
    // edge-driven cutoff as dynamic_cast / isinstance).
    let src = "\
void f(std::optional<Widget> a, std::optional<Widget> b) {
    if (a) {
        a->run();
        a = other();
        a->run();
    }
    a->run();
    if (b.has_value()) {
        b->go();
    }
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    let widget = || Some(InferredType::ClassName("Widget".into()));
    // bare `if (a)`: a narrows to Widget inside (row 2)…
    assert_eq!(
        fa.inferred_type_via_bag("a", tree_sitter::Point { row: 2, column: 8 }),
        widget(),
        "bare `if (a)` narrows the engaged optional to its inner Widget",
    );
    // …but the reassignment inside the block ends it (row 4, post-rebind)…
    assert_ne!(
        fa.inferred_type_via_bag("a", tree_sitter::Point { row: 4, column: 8 }),
        widget(),
        "reassignment rebinds a → narrowing truncated by the cutoff",
    );
    // …and it does not leak past the block (row 6).
    assert_ne!(
        fa.inferred_type_via_bag("a", tree_sitter::Point { row: 6, column: 4 }),
        widget(),
        "narrowing scoped to the guard block — gone after",
    );
    // `if (b.has_value())`: same narrowing via the method-form guard (row 8).
    assert_eq!(
        fa.inferred_type_via_bag("b", tree_sitter::Point { row: 8, column: 8 }),
        widget(),
        "`has_value()` narrows the engaged optional to its inner Widget",
    );
}

#[test]
fn cpp_optional_same_name_narrows_to_own_inner_type_per_function() {
    // GOAL-3 regression: `annot_text_by_var` is keyed by (name, SCOPE), so two
    // functions each with a `std::optional<...> opt` of a DIFFERENT inner type
    // peel the RIGHT T. Pre-fix it was keyed by bare name — last-declaration-
    // wins gave BOTH functions the last one's inner type.
    let src = "\
void f(std::optional<Widget> opt) {
    if (opt) {
        opt->run();
    }
}
void g(std::optional<Gadget> opt) {
    if (opt) {
        opt->go();
    }
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    // f's opt narrows to its own Widget (row 2)…
    assert_eq!(
        fa.inferred_type_via_bag("opt", tree_sitter::Point { row: 2, column: 8 }),
        Some(InferredType::ClassName("Widget".into())),
        "f's opt peels Widget from its OWN std::optional<Widget>",
    );
    // …and g's opt narrows to its own Gadget (row 7), not a sibling's inner type.
    assert_eq!(
        fa.inferred_type_via_bag("opt", tree_sitter::Point { row: 7, column: 8 }),
        Some(InferredType::ClassName("Gadget".into())),
        "g's opt peels Gadget from its OWN std::optional<Gadget> — no cross-fn leak",
    );
}

#[test]
fn cpp_optional_guard_does_not_narrow_non_optional_subject() {
    // The narrowing keys on the subject BEING a std::optional (rule #10, on the
    // type not a name): a bare `if (p)` over a non-optional pointer must not
    // invent an inner type, and `opt.value_or(x)` (not an engagement test) must
    // not narrow even though opt IS optional.
    let src = "\
void f(Widget* p, std::optional<Widget> opt) {
    if (p) {
        p->run();
    }
    if (opt.value_or(0)) {
        opt->run();
    }
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    // `p` stays its declared pointee type (Widget) — NOT re-narrowed to some
    // peeled inner — but critically the bare-if didn't fabricate a bogus type.
    // The load-bearing assertion: opt is not narrowed by a non-engagement call.
    assert_ne!(
        fa.inferred_type_via_bag("opt", tree_sitter::Point { row: 5, column: 8 }),
        Some(InferredType::ClassName("Widget".into())),
        "value_or() is not an engagement guard → opt not narrowed to inner",
    );
    // sanity: `p` is a pointer local, so it types as its pointee Widget from the
    // declaration (unchanged by the bare-if), proving the guard added nothing.
    assert_eq!(
        fa.inferred_type_via_bag("p", tree_sitter::Point { row: 2, column: 8 }),
        Some(InferredType::ClassName("Widget".into())),
        "non-optional pointer keeps its declared pointee type; bare-if is a no-op",
    );
}

#[test]
fn cpp_pointer_declared_vars_get_their_pointee_type() {
    // `T* p;` and the dynamic_cast condition-form both type the var to
    // the pointee class (pointer-ness dropped for navigation).
    let src = "\
void f(Base* b) {
    Widget* w;
    if (Derived* d = dynamic_cast<Derived*>(b)) {
        d->go();
    }
    w->run();
}
";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    // w at its use (row 5)
    assert_eq!(
        fa.inferred_type_via_bag("w", tree_sitter::Point { row: 5, column: 4 }),
        Some(InferredType::ClassName("Widget".into())),
        "T* w typed to Widget",
    );
    // d declared in the if-condition, used in the block (row 3)
    assert_eq!(
        fa.inferred_type_via_bag("d", tree_sitter::Point { row: 3, column: 8 }),
        Some(InferredType::ClassName("Derived".into())),
        "dynamic_cast'd d typed to Derived",
    );
}

#[test]
fn cpp_reference_declared_var_types_to_referent() {
    let src = "void f(Widget& src) {\n    Widget& r = src;\n    r.run();\n}\n";
    let fa = cpp_skel(src).into_file_analysis();
    use crate::file_analysis::InferredType;
    assert_eq!(
        fa.inferred_type_via_bag("r", tree_sitter::Point { row: 2, column: 4 }),
        Some(InferredType::ClassName("Widget".into())),
    );
}

#[test]
fn cpp_member_op_mismatches_drive_off_deref_depth() {
    use crate::file_analysis::MemberOp;
    // p: Box* (wants ->) accessed with `.` → mismatch.
    // b: Box   (wants .)  accessed with `->` → mismatch.
    // pp: Box** (DEEP — `(*pp)->`) accessed with `.` → NO fix (show-only).
    let src = "\
struct Box { int w; };
void f() {
    Box* p;
    Box b;
    Box** pp;
    p.w;
    b->w;
    pp.w;
}
";
    let fa = cpp_skel(src).into_file_analysis();
    // op-DX rides the MethodCall refs now (p.w, b->w, pp.w each mint one with
    // a `member_op`); the mismatch query joins each with its receiver's stack.
    let mm = fa.member_op_mismatches();
    assert_eq!(mm.len(), 2, "p and b mismatch; pp is DEEP/show-only: {mm:?}");

    // p.w → expected Arrow, typed Dot
    let p = mm.iter().find(|m| m.op_span.start.row == 5).expect("p.w mismatch");
    assert_eq!(p.typed, MemberOp::Dot);
    assert_eq!(p.expected, MemberOp::Arrow);

    // b->w → expected Dot, typed Arrow
    let b = mm.iter().find(|m| m.op_span.start.row == 6).expect("b->w mismatch");
    assert_eq!(b.typed, MemberOp::Arrow);
    assert_eq!(b.expected, MemberOp::Dot);

    // pp.w (row 7) yields no mismatch — DEEP.
    assert!(mm.iter().all(|m| m.op_span.start.row != 7), "pp is show-only");
}

#[test]
fn cpp_move_in_scopeless_operator_body_does_not_leak_to_sibling() {
    // GOAL-1 regression: the `operator[]` body mints its OWN @scope now (the
    // universal `(function_definition) @scope`). Before, operator/cast/dtor
    // shapes minted none, so a `std::move` inside one attributed to the
    // enclosing CLASS scope — and its moved-from region covered every sibling
    // method, false-flagging their reads of a same-named var. The move here is
    // inside `operator[]` (a shape that had no scope); `b()`'s read of `x` must
    // NOT be flagged.
    let src = "\
struct S {
  int operator[](int i) {
    Widget x;
    sink(std::move(x));
    return 0;
  }
  void b() {
    x.use();
  }
};
";
    let fa = cpp_skel(src).into_file_analysis();
    let diags = crate::symbols::pack_use_after_move_diagnostics(&fa);
    assert_eq!(
        diags.len(),
        0,
        "operator[]'s move is scoped to its own body — no leak to sibling b(): {diags:?}",
    );
}

// ---- Domain typing (int-used-as-enum): a storage slot's DOMAIN recovered
// from usage. `op_type` is stored `uint16_t` but always compared against
// `enum opcode` values — the sites fold onto the language-generic
// `Field{owner, name}` subject via `DomainCoherenceFold`. ----

#[test]
fn cpp_domain_typing_field_folds_to_enum() {
    let src = "\
enum opcode { OP_NULL, OP_CONST, OP_SCOPE };
struct op { enum opcode op_type; };
void a(struct op* o) { if (o->op_type == OP_CONST) { } }
void b(struct op* o) { if (o->op_type == OP_SCOPE) { } }
";
    let fa = cpp_skel(src).into_file_analysis();
    // Both comparison sites captured as raw domain evidence.
    assert_eq!(fa.domain_sites.len(), 2, "sites: {:?}", fa.domain_sites);
    // The slot's usage-recovered domain is the enum, folded over ≥2 functions,
    // owner resolved to the declaring struct.
    let dom = fa
        .field_domain_for_owner("op", "op_type", None)
        .expect("op_type has a recovered domain");
    assert_eq!(dom.domain, "opcode");
    assert!(dom.confidence > 0.99, "coherent: {}", dom.confidence);
    // Same answer through the ancestor-walked owner (the hover/goto-def path).
    assert_eq!(
        fa.field_domain("op", "op_type", None).map(|d| d.domain),
        Some("opcode".to_string()),
    );
    // Reverse bridge: the enum surfaces the field's sites (gd/gr symmetry).
    assert_eq!(fa.field_sites_for_enum("opcode", None).len(), 2);
    // An unrelated enum surfaces nothing.
    assert!(fa.field_sites_for_enum("nonesuch", None).is_empty());
}

#[test]
fn cpp_domain_typing_mixed_slot_stays_none() {
    // A slot compared mostly against non-enum values (raw ints) has no
    // coherent domain — the majority vote refuses to commit.
    let src = "\
enum opcode { OP_NULL, OP_CONST };
struct op { int op_type; };
void a(struct op* o) { if (o->op_type == 0) { } }
void b(struct op* o) { if (o->op_type == 1) { } }
void c(struct op* o) { if (o->op_type == OP_CONST) { } }
";
    let fa = cpp_skel(src).into_file_analysis();
    // The `== 0` / `== 1` operands are number literals, not enumerators, so
    // only one site resolves to an enum — below the majority.
    assert_eq!(fa.field_domain_for_owner("op", "op_type", None), None);
}
