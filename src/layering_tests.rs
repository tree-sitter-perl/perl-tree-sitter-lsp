//! The layer DAG, enforced. CLAUDE.md's architecture rules #1/#2 say
//! data flows down only and the model never touches the tree; this
//! suite makes a violation a red `cargo test` instead of a review
//! catch. (The alternative — a crate-per-layer workspace — buys the
//! same guarantee from the compiler at the price of five published
//! crates; the executed-and-rejected split lives on branch `workspace-split`.)

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// Layer order — an import may only point at the same layer or lower.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Layer {
    Model = 0,
    Cst = 1,
    Build = 2,
    Index = 3,
    Lsp = 4,
}

/// Module → layer. Every non-test module must be assigned: the
/// `unassigned module` assertion below makes adding a file without
/// placing it in the architecture a test failure, not a drift.
fn layer_map() -> HashMap<&'static str, Layer> {
    use Layer::*;
    HashMap::from([
        ("file_analysis", Model),
        ("witnesses", Model),
        ("conventions", Model),
        ("graph", Model),
        ("cst", Cst),
        ("builder", Build),
        ("plugin", Build),
        ("pod", Build),
        ("cpanfile", Build),
        ("query_cache", Build),
        ("module_index", Index),
        ("module_resolver", Index),
        ("module_cache", Index),
        ("file_store", Index),
        ("resolve", Index),
        ("document", Index),
        ("timings", Index),
        ("builtins_pod", Index),
        ("backend", Lsp),
        ("symbols", Lsp),
        ("cursor_context", Lsp),
        ("plugin_cli", Lsp),
        ("main", Lsp),
        ("layering_tests", Lsp),
    ])
}

/// Source files per module, relative to `src/`. Test suites are
/// exempt — they deliberately drive lower layers through upper ones.
fn module_sources() -> Vec<(&'static str, Vec<PathBuf>)> {
    let src = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut out: Vec<(&'static str, Vec<PathBuf>)> = Vec::new();
    let map = layer_map();
    for entry in fs::read_dir(&src).expect("read src/") {
        let path = entry.expect("dir entry").path();
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()).map(str::to_string)
        else {
            continue;
        };
        if path.is_dir() {
            // A module directory (`plugin/`, `builder/`) contributes its
            // `.rs` files to that module's layer — same enforcement as the
            // top-level `<module>.rs`, so a submodule can't dodge the DAG.
            if let Some(name) = map.keys().copied().find(|k| *k == stem) {
                let files = fs::read_dir(&path)
                    .unwrap_or_else(|_| panic!("read {stem}/"))
                    .filter_map(|e| {
                        let p = e.ok()?.path();
                        let s = p.file_stem()?.to_str()?;
                        (p.extension()? == "rs" && !s.ends_with("_tests") && !s.ends_with("_test"))
                            .then_some(p)
                    })
                    .collect();
                out.push((name, files));
            }
            continue;
        }
        if path.extension().is_none_or(|e| e != "rs") || stem.ends_with("_tests")
            || stem.ends_with("_test")
        {
            continue;
        }
        let name: &'static str = map
            .keys()
            .copied()
            .find(|k| *k == stem)
            .unwrap_or_else(|| panic!("unassigned module src/{stem}.rs — add it to layer_map()"));
        out.push((name, vec![path]));
    }
    out
}

/// `crate::xxx` references in non-test code, with `use` lines and
/// inline paths both counted. Lines inside `#[cfg(test)]` regions are
/// NOT excluded — test modules live in `_tests.rs` files here, which
/// the walker already skips.
fn crate_refs(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let bytes = text.as_bytes();
    let needle = b"crate::";
    let mut i = 0;
    while let Some(j) = text[i..].find("crate::").map(|j| i + j) {
        i = j + needle.len();
        // skip `::crate::` false positives and doc-comment mentions in
        // strings is overkill; module names are what we extract.
        let rest = &bytes[i..];
        let end = rest
            .iter()
            .position(|c| !(c.is_ascii_alphanumeric() || *c == b'_'))
            .unwrap_or(rest.len());
        if end > 0 {
            out.push(text[i..i + end].to_string());
        }
    }
    out
}

/// Rule: every `crate::X` reference points at the same layer or lower.
#[test]
fn imports_flow_down_only() {
    let map = layer_map();
    let mut violations = Vec::new();
    for (module, files) in module_sources() {
        let my_layer = map[module];
        for f in &files {
            let text = fs::read_to_string(f).expect("read source");
            for target in crate_refs(&text) {
                let Some(&target_layer) = map.get(target.as_str()) else {
                    continue; // not a module path (a type/fn at crate root, etc.)
                };
                if target_layer > my_layer {
                    violations.push(format!(
                        "{} ({:?}) imports crate::{} ({:?}) — data flows down only",
                        f.display(),
                        my_layer,
                        target,
                        target_layer,
                    ));
                }
            }
        }
    }
    assert!(violations.is_empty(), "layer violations:\n{}", violations.join("\n"));
}

/// Rule #2's teeth: the model layer never touches the tree. The only
/// tree-sitter name it may utter is `Point` (plus the serde shim that
/// wraps it). `cst` may not appear at all — the typed view is for
/// sanctioned tree consumers, and the model is not one.
#[test]
fn model_layer_cannot_walk_trees() {
    let map = layer_map();
    let mut violations = Vec::new();
    for (module, files) in module_sources() {
        if map[module] != Layer::Model {
            continue;
        }
        for f in &files {
            let text = fs::read_to_string(f).expect("read source");
            for (ln, line) in text.lines().enumerate() {
                let mut i = 0;
                while let Some(j) = line[i..].find("tree_sitter::").map(|j| i + j) {
                    i = j + "tree_sitter::".len();
                    let rest = &line[i..];
                    let end = rest
                        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                        .unwrap_or(rest.len());
                    let name = &rest[..end];
                    if name != "Point" {
                        violations.push(format!(
                            "{}:{}: tree_sitter::{} — the model is Point-only",
                            f.display(),
                            ln + 1,
                            name,
                        ));
                    }
                }
                for forbidden in ["TreeCursor", "child_by_field_name", "named_child("] {
                    if line.contains(forbidden) {
                        violations.push(format!(
                            "{}:{}: `{}` — tree walking belongs in the builder",
                            f.display(),
                            ln + 1,
                            forbidden,
                        ));
                    }
                }
            }
            if text.contains("crate::cst") {
                violations.push(format!(
                    "{}: imports crate::cst — the typed view is for tree consumers",
                    f.display(),
                ));
            }
        }
    }
    assert!(violations.is_empty(), "rule #2 violations:\n{}", violations.join("\n"));
}

/// Only the builder layer (and `cst` itself) may speak the grammar:
/// `ts_parser_perl::` anywhere above `build` means a second parser
/// entry point is growing. The index layer gets a pass for parsing
/// (resolver/document call `builder::create_parser`), so the check is
/// on the grammar crate, not `tree_sitter` generally.
#[test]
fn grammar_stays_in_the_builder_layer() {
    let map = layer_map();
    let mut violations = Vec::new();
    for (module, files) in module_sources() {
        let layer = map[module];
        if layer == Layer::Build || layer == Layer::Cst {
            continue;
        }
        // main.rs hosts --parse; backend/document parse via
        // builder::create_parser. Direct grammar naming outside
        // build/cst is the smell.
        for f in &files {
            let text = fs::read_to_string(f).expect("read source");
            for (ln, line) in text.lines().enumerate() {
                if line.contains("ts_parser_perl::") {
                    violations.push(format!(
                        "{}:{}: names the grammar directly — route through builder::create_parser",
                        f.display(),
                        ln + 1,
                    ));
                }
            }
        }
    }
    assert!(violations.is_empty(), "grammar violations:\n{}", violations.join("\n"));
}
