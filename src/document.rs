use tree_sitter::{Parser, Tree};

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
        let mut parser = create_parser();
        if let Some(tree) = parser.parse(&new_text, Some(&self.tree)) {
            self.tree = tree;
        }
        self.text = new_text;
    }
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
}
