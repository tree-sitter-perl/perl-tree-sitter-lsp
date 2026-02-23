use tree_sitter::{InputEdit, Parser, Point, Tree};

pub struct Document {
    pub text: String,
    pub tree: Tree,
}

impl Document {
    pub fn new(text: String) -> Option<Self> {
        let mut parser = create_parser();
        let tree = parser.parse(&text, None)?;
        Some(Document { text, tree })
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
        if let Some(tree) = parser.parse(&new_text, Some(&self.tree)) {
            self.tree = tree;
        }
        self.text = new_text;
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
        .set_language(&tree_sitter_perl::LANGUAGE.into())
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
