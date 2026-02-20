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
