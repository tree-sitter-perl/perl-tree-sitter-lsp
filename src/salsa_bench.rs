//! Salsa incremental-computation evaluation spike (feature `salsa_bench`).
//!
//! This is NOT production code. It exists to answer one question concretely:
//! if cross-file *enrichment* were modeled as a Salsa tracked query instead of
//! the current "re-enrich every open doc on any resolver callback" brute force,
//! how much recompute does an edit actually trigger — and how much bookkeeping
//! does it cost us in source?
//!
//! It runs the REAL `builder::build` on a REAL workspace, extracts the same
//! cross-file *surface* enrichment reads (exported sub return types + hash
//! keys, imports, parents), and wires the enrichment dependency graph as Salsa
//! tracked functions. Then it edits one file two ways — a body-only edit and a
//! surface-changing edit — and reports how many file analyses Salsa actually
//! recomputed, versus the naive "rebuild everything" cost.
//!
//! See `docs/eval-salsa-incremental.md` for the writeup this feeds.

use crate::builder;
use crate::module_resolver::create_parser;
use salsa::Setter;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::OnceLock;
use std::time::Instant;

// ── Recompute counters ────────────────────────────────────────────────
// Incremented at the TOP of each tracked-fn body, so they count actual
// (re)executions — memoized hits don't touch them. This is how we read the
// invalidation blast radius directly.
static BUILD_RUNS: AtomicUsize = AtomicUsize::new(0);
static ENRICH_RUNS: AtomicUsize = AtomicUsize::new(0);

// ── Salsa input: one source file ──────────────────────────────────────
#[salsa::input]
struct SourceFile {
    #[returns(ref)]
    module: String,
    #[returns(ref)]
    text: String,
}

// ── The cross-file surface that enrichment actually consumes ──────────
// Deliberately excludes sub bodies / positions: a body-only edit that leaves
// the export surface identical yields an EQUAL `Surface`, so Salsa's backdating
// (early cutoff) stops the change from propagating to dependents. That is the
// whole point of the experiment.
#[derive(Clone, PartialEq, Eq, Debug, salsa::Update)]
struct Surface {
    imports: Vec<String>,
    parents: Vec<String>,
    // (exported sub name, stringified return type, hash-key count)
    exports: Vec<(String, String, usize)>,
}

// What enrichment produces for a file: how many imported keys/return types it
// could resolve by reaching into its dependencies' surfaces.
#[derive(Clone, PartialEq, Eq, Debug, salsa::Update)]
struct Enriched {
    resolved_imported_keys: usize,
    resolved_imported_returns: usize,
}

// name -> input. The file SET is fixed for a session; only text mutates (via
// the tracked input), so an untracked name lookup returning a stable input id
// is sound — the dependency edge is created when we read that input's text
// inside a tracked fn.
static FILE_MAP: OnceLock<HashMap<String, SourceFile>> = OnceLock::new();

fn file_for(module: &str) -> Option<SourceFile> {
    FILE_MAP.get().and_then(|m| m.get(module).copied())
}

#[salsa::db]
#[derive(Default, Clone)]
struct BenchDb {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for BenchDb {}

/// Parse + build a single file and project it down to its cross-file surface.
/// This is the expensive query (runs the real builder). Memoized per input;
/// re-runs only when the file's text changes.
#[salsa::tracked]
fn surface(db: &dyn salsa::Database, file: SourceFile) -> Surface {
    BUILD_RUNS.fetch_add(1, Ordering::Relaxed);
    let text = file.text(db);
    let mut parser = create_parser();
    let tree = match parser.parse(text, None) {
        Some(t) => t,
        None => {
            return Surface {
                imports: vec![],
                parents: vec![],
                exports: vec![],
            }
        }
    };
    let fa = builder::build(&tree, text.as_bytes());

    let imports: Vec<String> = fa.imports.iter().map(|i| i.module_name.clone()).collect();
    let mut parents: Vec<String> = fa
        .package_parents
        .values()
        .flatten()
        .cloned()
        .collect();
    parents.sort();
    parents.dedup();

    let mut exports: Vec<(String, String, usize)> = Vec::new();
    for sym in &fa.symbols {
        if !matches!(
            sym.kind,
            crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
        ) {
            continue;
        }
        // OO surface: every public Sub/Method is part of the cross-file
        // method surface (MethodOnClass walks all of them), not just
        // Exporter @EXPORT names. Skip leading-underscore privates.
        if sym.name.starts_with('_') {
            continue;
        }
        let ret = fa
            .symbol_return_type_via_bag(sym.id, None)
            .map(|t| format!("{:?}", t))
            .unwrap_or_default();
        let owner = crate::file_analysis::HashKeyOwner::Sub {
            package: None,
            name: sym.name.clone(),
        };
        let key_count = fa.hash_keys_for_owner(&owner).len();
        exports.push((sym.name.clone(), ret, key_count));
    }
    exports.sort();

    Surface {
        imports,
        parents,
        exports,
    }
}

/// Enrichment of one file: reach into the surfaces of its imports AND its
/// parent chain (recursively, along parents) to resolve imported return types
/// and hash keys. Reading those surfaces records the cross-file dependency
/// edges automatically — this is the bookkeeping we hand-roll today.
#[salsa::tracked]
fn enriched(db: &dyn salsa::Database, file: SourceFile) -> Enriched {
    ENRICH_RUNS.fetch_add(1, Ordering::Relaxed);
    let me = surface(db, file);

    let mut keys = 0usize;
    let mut returns = 0usize;

    // Direct imports (peers): one-level surface read, no recursion (peer
    // `use` graphs can be cyclic; we avoid Salsa cycle handling here).
    for imp in &me.imports {
        if let Some(dep) = file_for(imp) {
            let s = surface(db, dep);
            for (_, ret, hk) in &s.exports {
                if !ret.is_empty() { returns += 1; }
                keys += hk;
            }
        }
    }

    // Parent chain: recurse `enriched` along parents. The base hierarchy is a
    // tree, so this is acyclic — editing the root invalidates every descendant
    // exactly like the real "always enriched" blast radius would.
    for parent in &me.parents {
        if let Some(p) = file_for(parent) {
            let pe = enriched(db, p);
            keys += pe.resolved_imported_keys;
            returns += pe.resolved_imported_returns;
            let ps = surface(db, p);
            for (_, ret, hk) in &ps.exports {
                if !ret.is_empty() {
                    returns += 1;
                }
                keys += hk;
            }
        }
    }

    Enriched {
        resolved_imported_keys: keys,
        resolved_imported_returns: returns,
    }
}

fn path_to_module(lib_root: &std::path::Path, path: &std::path::Path) -> Option<String> {
    let rel = path.strip_prefix(lib_root).ok()?;
    let s = rel.to_str()?;
    let s = s.strip_suffix(".pm")?;
    Some(s.replace(['/', '\\'], "::"))
}

fn collect_pm_files(root: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(rd) = std::fs::read_dir(&dir) else { continue };
        for entry in rd.flatten() {
            let p = entry.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().and_then(|e| e.to_str()) == Some("pm") {
                out.push(p);
            }
        }
    }
    out
}

/// Entry point: `perl-lsp --salsa-bench <root>`.
pub fn run(root: &str) {
    let root_path = std::path::Path::new(root);
    let lib_root = root_path.join("lib");
    let lib_root = if lib_root.is_dir() {
        lib_root
    } else {
        root_path.to_path_buf()
    };

    let files = collect_pm_files(&lib_root);
    println!("salsa-bench: {} .pm files under {}", files.len(), lib_root.display());

    let mut db = BenchDb::default();

    // Create inputs + build the name->input map.
    let mut map: HashMap<String, SourceFile> = HashMap::new();
    let mut all_inputs: Vec<SourceFile> = Vec::new();
    let mut base0: Option<SourceFile> = None;
    for path in &files {
        let Some(module) = path_to_module(&lib_root, path) else { continue };
        let Ok(text) = std::fs::read_to_string(path) else { continue };
        let input = SourceFile::new(&db, module.clone(), text);
        map.insert(module.clone(), input);
        all_inputs.push(input);
        if module == "Base::Level0" {
            base0 = Some(input);
        }
    }
    let n = all_inputs.len();
    FILE_MAP.set(map).ok();

    // ── Warm: force `enriched` for every file (cold memoization). ──────
    BUILD_RUNS.store(0, Ordering::Relaxed);
    ENRICH_RUNS.store(0, Ordering::Relaxed);
    let t0 = Instant::now();
    for f in &all_inputs {
        let _ = enriched(&db, *f);
    }
    let warm_ms = t0.elapsed().as_secs_f64() * 1000.0;
    let warm_builds = BUILD_RUNS.swap(0, Ordering::Relaxed);
    let warm_enrich = ENRICH_RUNS.swap(0, Ordering::Relaxed);
    println!(
        "\nWARM (cold memo over {} files): {:.1} ms  | surface builds={}  enriched runs={}",
        n, warm_ms, warm_builds, warm_enrich
    );

    // ── Re-query with NO edit: should be pure memo hits (0 recompute). ─
    let t = Instant::now();
    for f in &all_inputs {
        let _ = enriched(&db, *f);
    }
    let noedit_ms = t.elapsed().as_secs_f64() * 1000.0;
    println!(
        "RE-QUERY (no edit): {:.2} ms | surface builds={} enriched runs={}  (expect 0/0 — all memoized)",
        noedit_ms,
        BUILD_RUNS.swap(0, Ordering::Relaxed),
        ENRICH_RUNS.swap(0, Ordering::Relaxed),
    );

    let Some(base0) = base0 else {
        println!("\n(no Base::Level0 in corpus — skipping edit scenarios)");
        return;
    };

    // ── Scenario A: body-only edit to the ROOT base class. ─────────────
    // Append a comment line: text changes, exported surface does NOT.
    // Salsa should re-run `surface(Base0)` once, detect an equal Surface
    // (backdating), and recompute NOTHING downstream.
    let orig = base0.text(&db).clone();
    base0
        .set_text(&mut db)
        .to(format!("{}\n# body-only edit, no surface change\n", orig));
    let t = Instant::now();
    for f in &all_inputs {
        let _ = enriched(&db, *f);
    }
    let a_ms = t.elapsed().as_secs_f64() * 1000.0;
    println!(
        "\nSCENARIO A — body-only edit to Base::Level0 (root of {}-deep chain, {} descendants):",
        chain_depth(&db),
        n - 1
    );
    println!(
        "  {:.2} ms | surface builds={}  enriched runs={}  (early-cutoff: dependents should NOT recompute)",
        a_ms,
        BUILD_RUNS.swap(0, Ordering::Relaxed),
        ENRICH_RUNS.swap(0, Ordering::Relaxed),
    );

    // ── Scenario B: surface-changing edit to the ROOT base class. ──────
    // Add a new exported sub: the Surface changes, so every descendant whose
    // `enriched` transitively read Base0 must recompute.
    base0.set_text(&mut db).to(format!(
        "{}\nsub newly_added_export {{ my ($self) = @_; return {{ brandnew => 1 }}; }}\n",
        orig
    ));
    let t = Instant::now();
    for f in &all_inputs {
        let _ = enriched(&db, *f);
    }
    let b_ms = t.elapsed().as_secs_f64() * 1000.0;
    let b_builds = BUILD_RUNS.swap(0, Ordering::Relaxed);
    let b_enrich = ENRICH_RUNS.swap(0, Ordering::Relaxed);
    println!("\nSCENARIO B — surface-changing edit to Base::Level0:");
    println!(
        "  {:.2} ms | surface builds={}  enriched runs={}  (genuine blast radius)",
        b_ms, b_builds, b_enrich
    );

    // ── Naive baseline: cost of rebuilding ALL files once (what an ──────
    // always-enriched-WITHOUT-dependency-tracking design pays per edit).
    let t = Instant::now();
    let mut parser = create_parser();
    for f in &all_inputs {
        let text = f.text(&db);
        if let Some(tree) = parser.parse(text, None) {
            let _ = builder::build(&tree, text.as_bytes());
        }
    }
    let naive_ms = t.elapsed().as_secs_f64() * 1000.0;
    println!(
        "\nNAIVE baseline (rebuild all {} files, no incrementality): {:.1} ms",
        n, naive_ms
    );

    println!("\n── summary ──");
    println!("  warm (full)            : {:>8.1} ms / {} builds", warm_ms, warm_builds);
    println!("  no-edit re-query       : {:>8.2} ms / 0 builds", noedit_ms);
    println!("  body-only edit (A)     : {:>8.2} ms", a_ms);
    println!("  surface edit (B)       : {:>8.2} ms / {} builds, {} enrich", b_ms, b_builds, b_enrich);
    println!("  naive rebuild-all      : {:>8.1} ms", naive_ms);
}

fn chain_depth(_db: &BenchDb) -> usize {
    FILE_MAP
        .get()
        .map(|m| m.keys().filter(|k| k.starts_with("Base::Level")).count())
        .unwrap_or(0)
}
