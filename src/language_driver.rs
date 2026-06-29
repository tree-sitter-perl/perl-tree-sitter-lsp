//! Multi-language serving seam — the `LanguageDriver` keystone
//! (`docs/prompt-multi-language.md`). One trait the server routes
//! through; Perl is the always-present reference driver (and the
//! gold-corpus regression net), pack languages are opt-in features.
//!
//! Distribution identity is a feature flag, not a repo split: a
//! `cpp-lsp` is this binary built `--features cpp`; the default Perl
//! build never links a C++ grammar. The crate stays single + lockstep
//! (the layering test is the seam) until a second driver makes a cargo
//! *workspace* earn its keep — see `docs/gold-roadmap.md`.

use crate::file_analysis::FileAnalysis;
use std::path::Path;

/// Everything the server needs to host one language: parse + analyze a
/// file to a `FileAnalysis`, claim its extensions, and resolve a
/// module name to candidate paths (cross-file).
pub trait LanguageDriver: Send + Sync {
    fn id(&self) -> &'static str;
    fn extensions(&self) -> &[&'static str];
    /// A fresh parser for this language — for the open `Document` to hold
    /// a tree (incremental edits, position handlers). NOTE: this parses
    /// the ORIGINAL source; `analyze` may run a pre-parse transform (C++
    /// macro expansion) internally, so the two trees can differ on
    /// macro-heavy files (the span-remap follow-up reconciles them).
    fn make_parser(&self) -> tree_sitter::Parser;
    /// Source → `FileAnalysis`.
    fn analyze(&self, source: &str) -> FileAnalysis;
    /// Source + the file's path → `FileAnalysis`. The path lets a driver
    /// resolve cross-file context (C++ gathers `#define`s from `#include`d
    /// headers so namespace/export macros expand). Default ignores it.
    fn analyze_with_path(&self, source: &str, _path: Option<&Path>) -> FileAnalysis {
        self.analyze(source)
    }
    /// Module name → workspace-relative candidate paths.
    fn module_paths(&self, module: &str) -> Vec<String>;
    /// Completion trigger characters for this language — the registry
    /// unions them into the LSP `completionProvider` slot, so the client
    /// auto-fires completion (e.g. on `.`/`->`) for the right files.
    fn trigger_chars(&self) -> &[&'static str];
}

/// Perl — the reference driver. Wraps the production builder; behaviour
/// is exactly the current single-file analysis path.
pub struct PerlDriver;

impl LanguageDriver for PerlDriver {
    fn id(&self) -> &'static str {
        "perl"
    }
    fn extensions(&self) -> &[&'static str] {
        &["pm", "pl", "t"]
    }
    fn make_parser(&self) -> tree_sitter::Parser {
        crate::builder::create_parser()
    }
    fn analyze(&self, source: &str) -> FileAnalysis {
        let mut parser = crate::builder::create_parser();
        match parser.parse(source, None) {
            Some(tree) => crate::builder::build(&tree, source.as_bytes()),
            None => FileAnalysis::new(Default::default()),
        }
    }
    fn module_paths(&self, module: &str) -> Vec<String> {
        vec![format!("{}.pm", module.replace("::", "/"))]
    }
    fn trigger_chars(&self) -> &[&'static str] {
        // Sigils open variable completion; `>`/`:`/`{` open
        // method/pkg/hash-key slots; `(`/`,` are signature-help adjacent.
        &["$", "@", "%", ">", ":", "{", "(", ","]
    }
}

/// A pack-language driver — the generic, query-driven path. One value
/// per language: a grammar, a `LangPack` (capture predicates), and an
/// optional pre-parse `transform` (C++ uses it for macro reparse;
/// others pass through). The whole multi-language story for a language
/// whose extraction is query-shaped is a `PackDriver { ... }` literal.
#[cfg(any(feature = "cpp", feature = "python", feature = "r", feature = "cmake"))]
pub struct PackDriver {
    id: &'static str,
    exts: &'static [&'static str],
    make_parser: fn() -> tree_sitter::Parser,
    pack: fn() -> crate::query_extract::LangPack,
    /// (source, external macros) → (transformed source, anchor map), run
    /// before parsing (C++ macro expansion). The map remaps extracted
    /// spans back to ORIGINAL coordinates. `None` = pass-through (identity).
    transform: Option<
        fn(
            &mut tree_sitter::Parser,
            &str,
            &std::collections::BTreeMap<String, crate::cpp_reparse::Macro>,
        ) -> (String, crate::cpp_reparse::SpliceMap),
    >,
    /// Path-aware cross-file macro gather (C++ #include resolution). Given
    /// the file path + source, returns macros from #included headers to
    /// seed `transform`. `None` = no cross-file macros.
    gather_macros: Option<
        fn(
            &Path,
            &str,
            &mut tree_sitter::Parser,
        ) -> std::sync::Arc<std::collections::BTreeMap<String, crate::cpp_reparse::Macro>>,
    >,
}

#[cfg(any(feature = "cpp", feature = "python", feature = "r", feature = "cmake"))]
impl LanguageDriver for PackDriver {
    fn id(&self) -> &'static str {
        self.id
    }
    fn extensions(&self) -> &[&'static str] {
        self.exts
    }
    fn make_parser(&self) -> tree_sitter::Parser {
        (self.make_parser)()
    }
    fn analyze(&self, source: &str) -> FileAnalysis {
        self.analyze_with_path(source, None)
    }
    fn analyze_with_path(&self, source: &str, path: Option<&Path>) -> FileAnalysis {
        let mut parser = (self.make_parser)();
        // Cross-file macros from #included headers (C++), so a macro
        // #defined elsewhere (SPDLOG_NAMESPACE_BEGIN) expands here.
        let external = match (self.gather_macros, path) {
            (Some(g), Some(p)) => g(p, source, &mut parser),
            _ => std::sync::Arc::new(std::collections::BTreeMap::new()),
        };
        let (src, map) = match self.transform {
            Some(t) => t(&mut parser, source, &external),
            None => (source.to_string(), crate::cpp_reparse::SpliceMap::default()),
        };
        let Some(tree) = parser.parse(&src, None) else { return FileAnalysis::new(Default::default()) };
        match crate::query_extract::extract(&tree, src.as_bytes(), &(self.pack)()) {
            Ok(mut skel) => {
                // remap extracted spans from transformed → original coords
                // (no-op for identity / pass-through languages).
                remap_spans(&mut skel, &src, source, &map);
                skel.into_file_analysis()
            }
            Err(_) => FileAnalysis::new(Default::default()),
        }
    }
    fn module_paths(&self, module: &str) -> Vec<String> {
        ((self.pack)().module_paths)(module)
    }
    fn trigger_chars(&self) -> &[&'static str] {
        (self.pack)().trigger_chars
    }
}

#[cfg(feature = "cpp")]
fn cpp_driver() -> PackDriver {
    PackDriver {
        id: "cpp",
        // `.c` too — tree-sitter-cpp parses C (a near-subset), and MISRA /
        // embedded code is C-heavy. One driver serves both.
        exts: &["cpp", "cc", "cxx", "hpp", "hh", "h", "c"],
        make_parser: || {
            let mut p = tree_sitter::Parser::new();
            p.set_language(&tree_sitter_cpp::LANGUAGE.into()).expect("cpp grammar");
            p
        },
        pack: crate::query_extract::cpp_pack,
        // reparse past the preprocessor before extraction; the anchor map
        // carries the recovered spans back to the original coordinates.
        transform: Some(crate::cpp_reparse::preprocess_validated_with),
        gather_macros: Some(crate::cpp_reparse::included_macros),
    }
}

#[cfg(feature = "python")]
fn python_driver() -> PackDriver {
    PackDriver {
        id: "python",
        exts: &["py"],
        make_parser: || {
            let mut p = tree_sitter::Parser::new();
            p.set_language(&tree_sitter_python::LANGUAGE.into()).expect("python grammar");
            p
        },
        pack: crate::query_extract::python_pack,
        transform: None,
        gather_macros: None,
    }
}

#[cfg(feature = "r")]
fn r_driver() -> PackDriver {
    PackDriver {
        id: "r",
        exts: &["R", "r"],
        make_parser: || {
            let mut p = tree_sitter::Parser::new();
            p.set_language(&tree_sitter_r::LANGUAGE.into()).expect("r grammar");
            p
        },
        pack: crate::query_extract::r_pack,
        transform: None,
        gather_macros: None,
    }
}

#[cfg(feature = "cmake")]
fn cmake_driver() -> PackDriver {
    PackDriver {
        // CMakeLists.txt (no extension match) is a follow-up; `.cmake` now.
        id: "cmake",
        exts: &["cmake"],
        make_parser: || {
            let mut p = tree_sitter::Parser::new();
            p.set_language(&tree_sitter_cmake::LANGUAGE.into()).expect("cmake grammar");
            p
        },
        pack: crate::query_extract::cmake_pack,
        transform: None,
        gather_macros: None,
    }
}

/// Remap extracted skeleton spans from transformed coords back to
/// original source coords via the anchor map. A no-op for an identity
/// map (clean/pass-through files round-trip byte→point→byte unchanged),
/// so it's safe to always call. Covers navigation spans (symbols / refs
/// / scopes); witness spans (type queries) are the follow-up.
#[cfg(any(feature = "cpp", feature = "python", feature = "r", feature = "cmake"))]
fn remap_spans(
    skel: &mut crate::query_extract::SkeletonAnalysis,
    transformed: &str,
    original: &str,
    map: &crate::cpp_reparse::SpliceMap,
) {
    use tree_sitter::Point;
    let t = LineIndex::new(transformed);
    let o = LineIndex::new(original);
    let r = |p: Point| -> Point { o.point(map.to_original(t.byte(p))) };
    for s in &mut skel.symbols {
        s.start = r(s.start);
        s.end = r(s.end);
        s.name_start = r(s.name_start);
        s.name_end = r(s.name_end);
    }
    // A ref/read that came OUT of a macro expansion collapses to a
    // zero-width point under `to_original` (every expanded byte maps to the
    // splice site) — goto-def/hover would then miss it. Give it the macro
    // CALL site's extent instead, so `newThing(5)` resolves to the expanded
    // `Perl_newThing` (see-through to the function).
    let remap_span = |start: Point, end: Point| -> (Point, Point) {
        match map.replacement_at(t.byte(start)) {
            Some((os, oe)) => (o.point(os), o.point(oe)),
            None => (r(start), r(end)),
        }
    };
    for rf in &mut skel.refs {
        (rf.start, rf.end) = remap_span(rf.start, rf.end);
    }
    for (_, _, span) in &mut skel.var_reads {
        (span.start, span.end) = remap_span(span.start, span.end);
    }
    for sc in &mut skel.scopes {
        sc.span.start = r(sc.span.start);
        sc.span.end = r(sc.span.end);
    }
}

/// Line-start byte offsets, for Point↔byte conversion (Point.column is a
/// byte offset within its row).
#[cfg(any(feature = "cpp", feature = "python", feature = "r", feature = "cmake"))]
struct LineIndex {
    starts: Vec<usize>,
}

#[cfg(any(feature = "cpp", feature = "python", feature = "r", feature = "cmake"))]
impl LineIndex {
    fn new(s: &str) -> Self {
        let mut starts = vec![0];
        for (i, b) in s.bytes().enumerate() {
            if b == b'\n' {
                starts.push(i + 1);
            }
        }
        LineIndex { starts }
    }
    fn byte(&self, p: tree_sitter::Point) -> usize {
        self.starts.get(p.row).copied().unwrap_or(0) + p.column
    }
    fn point(&self, byte: usize) -> tree_sitter::Point {
        let row = self.starts.partition_point(|&s| s <= byte).saturating_sub(1);
        tree_sitter::Point { row, column: byte - self.starts[row] }
    }
}

/// The drivers this binary was compiled to serve. Perl always; pack
/// languages per feature.
pub struct LanguageRegistry {
    drivers: Vec<Box<dyn LanguageDriver>>,
}

impl LanguageRegistry {
    pub fn with_enabled() -> Self {
        let mut drivers: Vec<Box<dyn LanguageDriver>> = vec![Box::new(PerlDriver)];
        #[cfg(feature = "cpp")]
        drivers.push(Box::new(cpp_driver()));
        #[cfg(feature = "python")]
        drivers.push(Box::new(python_driver()));
        #[cfg(feature = "r")]
        drivers.push(Box::new(r_driver()));
        #[cfg(feature = "cmake")]
        drivers.push(Box::new(cmake_driver()));
        LanguageRegistry { drivers }
    }

    pub fn for_path(&self, path: &Path) -> Option<&dyn LanguageDriver> {
        let ext = path.extension()?.to_str()?;
        self.drivers.iter().find(|d| d.extensions().contains(&ext)).map(|d| d.as_ref())
    }

    pub fn for_id(&self, id: &str) -> Option<&dyn LanguageDriver> {
        self.drivers.iter().find(|d| d.id() == id).map(|d| d.as_ref())
    }

    /// Configured language ids — what this distribution serves.
    pub fn languages(&self) -> Vec<&'static str> {
        self.drivers.iter().map(|d| d.id()).collect()
    }

    /// Union of every served language's completion trigger characters,
    /// for the LSP `completionProvider.triggerCharacters` slot.
    pub fn trigger_chars(&self) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        for d in &self.drivers {
            for c in d.trigger_chars() {
                if !out.iter().any(|s| s == c) {
                    out.push((*c).to_string());
                }
            }
        }
        out
    }
}

#[cfg(test)]
#[path = "language_driver_tests.rs"]
mod tests;
