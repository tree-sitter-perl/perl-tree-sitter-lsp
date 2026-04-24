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
mod tests {
    use super::*;

    #[test]
    fn test_document_new() {
        let doc = Document::new("my $x = 1;".to_string()).unwrap();
        assert_eq!(doc.text, "my $x = 1;");
        assert_eq!(doc.tree.root_node().kind(), "source_file");
    }

    #[test]
    fn test_document_update() {
        let mut doc = Document::new("my $x = 1;".to_string()).unwrap();
        doc.update("my $y = 2;".to_string());
        assert_eq!(doc.text, "my $y = 2;");
        assert_eq!(doc.tree.root_node().kind(), "source_file");
    }

    #[test]
    fn test_byte_to_point() {
        assert_eq!(byte_to_point(b"hello\nworld", 0), Point::new(0, 0));
        assert_eq!(byte_to_point(b"hello\nworld", 5), Point::new(0, 5));
        assert_eq!(byte_to_point(b"hello\nworld", 6), Point::new(1, 0));
        assert_eq!(byte_to_point(b"hello\nworld", 8), Point::new(1, 2));
    }

    #[test]
    fn test_incremental_update_preserves_tree() {
        // Simulate typing `$self->` mid-line in a sub body
        let original = "sub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    return $result;\n}\n";
        let mut doc = Document::new(original.to_string()).unwrap();

        // Insert `$self->` at beginning of line 3 (after "    ")
        let edited = "sub add {\n    my ($self, $a, $b) = @_;\n    $self->my $result = $a + $b;\n    return $result;\n}\n";
        doc.update(edited.to_string());

        // The tree should still have a root source_file, not be completely borked
        assert_eq!(doc.tree.root_node().kind(), "source_file");

        // The sub declaration should still be recognized
        let root = doc.tree.root_node();
        let mut found_sub = false;
        for i in 0..root.named_child_count() {
            if let Some(child) = root.named_child(i) {
                if child.kind() == "subroutine_declaration_statement" {
                    found_sub = true;
                    break;
                }
            }
        }
        assert!(found_sub, "sub declaration should survive partial edit");
    }
}

#[cfg(test)]
mod incremental_pkg_test {
    use super::*;

    #[test]
    fn test_incremental_preserves_packages() {
        let original = std::fs::read_to_string("test_files/frameworks.pl").unwrap();
        let mut doc = Document::new(original.clone()).unwrap();

        // Count packages in clean parse
        let clean_pkgs: Vec<String> = doc.analysis.symbols.iter()
            .filter(|s| matches!(s.kind, crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class))
            .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
            .collect();
        println!("Clean parse packages:");
        for p in &clean_pkgs { println!("  {}", p); }

        // Simulate typing 'reduce' on line 19 (0-indexed)
        let lines: Vec<&str> = original.lines().collect();
        let mut dirty_lines: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
        dirty_lines.insert(19, "reduce".to_string());
        let dirty = dirty_lines.join("\n");

        // Use incremental update (same as LSP did_change path)
        doc.update(dirty);

        let dirty_pkgs: Vec<String> = doc.analysis.symbols.iter()
            .filter(|s| matches!(s.kind, crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class))
            .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
            .collect();
        println!("\nIncremental parse (after typing 'reduce' on L20):");
        for p in &dirty_pkgs { println!("  {}", p); }

        println!("\nTree has errors: {}", doc.tree.root_node().has_error());

        // Check if MojoApp survived
        let has_mojo = dirty_pkgs.iter().any(|p| p.contains("MojoApp"));
        println!("MojoApp survived incremental: {}", has_mojo);

        // For comparison, do a FRESH parse of the same dirty text
        let fresh = Document::new(dirty_lines.join("\n")).unwrap();
        let fresh_pkgs: Vec<String> = fresh.analysis.symbols.iter()
            .filter(|s| matches!(s.kind, crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class))
            .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
            .collect();
        println!("\nFresh parse (same dirty text, no incremental):");
        for p in &fresh_pkgs { println!("  {}", p); }
        let fresh_has_mojo = fresh_pkgs.iter().any(|p| p.contains("MojoApp"));
        println!("MojoApp survived fresh: {}", fresh_has_mojo);
    }
}
