use tree_sitter::{InputEdit, Parser, Point, Tree};

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
}

impl Document {
    pub fn new(text: String) -> Option<Self> {
        let mut parser = create_parser();
        let t0 = std::time::Instant::now();
        let tree = parser.parse(&text, None)?;
        let elapsed = t0.elapsed();
        if elapsed.as_millis() > 100 {
            log::warn!("Slow parse (new): {:?} for {} bytes", elapsed, text.len());
        }
        let analysis = builder::build(&tree, text.as_bytes());
        let stable_outline = StableOutline::from_analysis(&analysis);
        Some(Document { text, tree, analysis, stable_outline })
    }

    pub fn update(&mut self, new_text: String) {
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

fn create_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .expect("Failed to load Perl grammar");
    parser
}

#[cfg(test)]
#[path = "document_tests.rs"]
mod tests;

#[cfg(test)]
#[path = "document_incremental_pkg_test.rs"]
mod incremental_pkg_test;
