use tree_sitter::{InputEdit, Point, Tree};

use crate::builder;
use crate::file_analysis::{FileAnalysis, SymKind};

/// Lightweight structural skeleton that survives parse degradation.
/// Updated only when the current parse has >= as many entries as before.
/// Entries validated against source text to handle legitimate deletions.
#[derive(Debug, Clone, Default)]
pub struct StableOutline {
    pub packages: Vec<(String, usize)>,  // (name, line)
    pub imports: Vec<(String, usize)>,   // (module_name, line)
    pub subs: Vec<(String, usize)>,      // (name, line)
}

pub struct Document {
    pub text: String,
    pub tree: Tree,
    pub analysis: FileAnalysis,
    pub stable_outline: StableOutline,
    /// Driver id: "perl" (the reference path) or a pack language ("cpp").
    /// The FileAnalysis-based handlers are language-agnostic; the
    /// tree/cursor handlers (completion, signature-help, selection-range)
    /// are Perl cursor-context-specific and skip when this isn't "perl".
    pub language: &'static str,
    /// The file's path, for cross-file analysis (C++ resolves #include'd
    /// macros relative to it). `None` for Perl / unrouted docs.
    pub path: Option<std::path::PathBuf>,
}

impl Document {
    /// Build a pack-language document through its driver (the multi-
    /// language path). Holds the original-source tree + the driver's
    /// analysis. Full reparse on update (no incremental for v1).
    pub fn new_routed(
        text: String,
        driver: &dyn crate::language_driver::LanguageDriver,
        path: Option<std::path::PathBuf>,
    ) -> Option<Self> {
        let mut parser = driver.make_parser();
        let tree = parser.parse(&text, None)?;
        let analysis = driver.analyze_with_path(&text, path.as_deref());
        let stable_outline = StableOutline::from_analysis(&analysis);
        Some(Document { text, tree, analysis, stable_outline, language: driver.id(), path })
    }

    pub fn new(text: String) -> Option<Self> {
        let mut parser = create_parser();
        let t0 = std::time::Instant::now();
        let tree = parser.parse(&text, None)?;
        let elapsed = t0.elapsed();
        if elapsed.as_millis() > 100 {
            log::warn!("Slow parse (new): {:?} for {} bytes", elapsed, text.len());
        }
        if crate::timings::phases_enabled() {
            eprintln!(
                "[PHASE] {:<32} {:>8.2} ms ({} bytes)",
                "parse",
                elapsed.as_secs_f64() * 1000.0,
                text.len()
            );
        }
        let analysis = crate::timings::phase("build()", || builder::build(&tree, text.as_bytes()));
        let stable_outline =
            crate::timings::phase("stable_outline", || StableOutline::from_analysis(&analysis));
        Some(Document { text, tree, analysis, stable_outline, language: "perl", path: None })
    }

    /// Pack-language FAST path for a keystroke: reparse the tree + swap text,
    /// WITHOUT the expensive `FileAnalysis` rebuild (macro gather/expand/
    /// extract — ~0.7s on a big macro-heavy C file). Position features stay
    /// live on the fresh tree/text; the caller debounces `rebuild_analysis`
    /// so typing doesn't pay the rebuild per keystroke. No-op for Perl (its
    /// build is cheap — it uses the synchronous `update`).
    pub fn update_text_only(&mut self, new_text: String) {
        if self.language == "perl" {
            return;
        }
        let reg = crate::language_driver::LanguageRegistry::with_enabled();
        if let Some(driver) = reg.for_id(self.language) {
            let mut parser = driver.make_parser();
            if let Some(tree) = parser.parse(&new_text, None) {
                self.tree = tree;
            }
            self.text = new_text;
        }
    }

    /// Write back an analysis built off-lock (from a text snapshot) +
    /// refresh the outline. Pairs with a `spawn_blocking` rebuild.
    pub fn apply_rebuilt(&mut self, analysis: crate::file_analysis::FileAnalysis) {
        self.analysis = analysis;
        self.stable_outline.update(&self.analysis, &self.text);
    }

    pub fn update(&mut self, new_text: String) {
        // Pack languages: full reparse + analyze through the driver (no
        // incremental edit path yet). The reparse/text-swap is exactly
        // `update_text_only`; this just isn't debounced. Perl falls through.
        if self.language != "perl" {
            self.update_text_only(new_text);
            let reg = crate::language_driver::LanguageRegistry::with_enabled();
            if let Some(driver) = reg.for_id(self.language) {
                self.analysis = driver.analyze_with_path(&self.text, self.path.as_deref());
                self.stable_outline.update(&self.analysis, &self.text);
            }
            return;
        }
        // Diff old vs new to find the changed region, then tell tree-sitter
        // exactly what changed so it can do targeted incremental reparsing.
        // This preserves valid nodes outside the edit, producing much better
        // trees when the user is mid-typing (e.g. `$self->` without finishing).
        let old = self.text.as_bytes();
        let new = new_text.as_bytes();

        // Find common prefix
        let prefix_len = old.iter().zip(new.iter()).take_while(|(a, b)| a == b).count();

        // Find common suffix (not overlapping with prefix)
        let old_remaining = old.len() - prefix_len;
        let new_remaining = new.len() - prefix_len;
        let suffix_len = old[prefix_len..]
            .iter()
            .rev()
            .zip(new[prefix_len..].iter().rev())
            .take_while(|(a, b)| a == b)
            .count()
            .min(old_remaining)
            .min(new_remaining);

        let start_byte = prefix_len;
        let old_end_byte = old.len() - suffix_len;
        let new_end_byte = new.len() - suffix_len;

        if start_byte < old_end_byte || start_byte < new_end_byte {
            let start_position = byte_to_point(old, start_byte);
            let old_end_position = byte_to_point(old, old_end_byte);
            let new_end_position = byte_to_point(new, new_end_byte);

            self.tree.edit(&InputEdit {
                start_byte,
                old_end_byte,
                new_end_byte,
                start_position,
                old_end_position,
                new_end_position,
            });
        }

        let mut parser = create_parser();
        let t0 = std::time::Instant::now();
        if let Some(tree) = parser.parse(&new_text, Some(&self.tree)) {
            self.tree = tree;
        }
        let elapsed = t0.elapsed();
        if elapsed.as_millis() > 100 {
            log::warn!("Slow parse (update): {:?} for {} bytes", elapsed, new_text.len());
        }
        self.text = new_text;
        self.analysis = builder::build(&self.tree, self.text.as_bytes());
        self.stable_outline.update(&self.analysis, &self.text);
    }
}

impl StableOutline {
    fn from_analysis(analysis: &FileAnalysis) -> Self {
        let mut outline = StableOutline::default();
        for sym in &analysis.symbols {
            match sym.kind {
                SymKind::Package | SymKind::Class => {
                    outline.packages.push((sym.name.clone(), sym.selection_span.start.row));
                }
                SymKind::Sub | SymKind::Method => {
                    outline.subs.push((sym.name.clone(), sym.selection_span.start.row));
                }
                _ => {}
            }
        }
        for imp in &analysis.imports {
            outline.imports.push((imp.module_name.clone(), imp.span.start.row));
        }
        outline
    }

    fn update(&mut self, analysis: &FileAnalysis, source: &str) {
        let current = StableOutline::from_analysis(analysis);

        // For each category: if current parse has >= entries, update.
        // Otherwise keep old entries but validate against source text.
        if current.packages.len() >= self.packages.len() {
            self.packages = current.packages;
        } else {
            self.packages.retain(|(name, line)| {
                source.lines().nth(*line)
                    .map(|l| {
                        let t = l.trim_start();
                        (t.starts_with("package") || t.starts_with("class")) && t.contains(name.as_str())
                    })
                    .unwrap_or(false)
            });
        }

        if current.imports.len() >= self.imports.len() {
            self.imports = current.imports;
        } else {
            self.imports.retain(|(name, line)| {
                source.lines().nth(*line)
                    .map(|l| l.trim_start().starts_with("use") && l.contains(name.as_str()))
                    .unwrap_or(false)
            });
        }

        if current.subs.len() >= self.subs.len() {
            self.subs = current.subs;
        } else {
            self.subs.retain(|(name, line)| {
                source.lines().nth(*line)
                    .map(|l| {
                        let t = l.trim_start();
                        (t.starts_with("sub") || t.starts_with("method")) && t.contains(name.as_str())
                    })
                    .unwrap_or(false)
            });
        }
    }

    /// Get package line ranges for use insertion position fallback.
    /// Returns sorted (name, start_line) pairs.
    pub fn package_lines(&self) -> &[(String, usize)] {
        &self.packages
    }
}

/// Convert a byte offset to a tree-sitter Point (row, column).
fn byte_to_point(source: &[u8], byte_offset: usize) -> Point {
    let mut row = 0;
    let mut col = 0;
    for &b in &source[..byte_offset.min(source.len())] {
        if b == b'\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Point::new(row, col)
}

use crate::builder::create_parser;

#[cfg(test)]
#[path = "document_tests.rs"]
mod tests;

#[cfg(test)]
#[path = "document_incremental_pkg_test.rs"]
mod incremental_pkg_test;
