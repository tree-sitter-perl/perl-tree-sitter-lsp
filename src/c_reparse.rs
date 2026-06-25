//! SPIKE: the C lexer hack, resolved by the symbol table (Class B1 of
//! `~/personal/resume/research-static-analysis.md`).
//!
//! `a * b;` is the canonical C ambiguity: a multiply-expression if `a`
//! is a value, a pointer DECLARATION of `b` if `a` is a type. The
//! research doc flags this as the "no symbol table" wall that defeats
//! vanilla tree-sitter and (per the lexer-hack literature) Clang's own
//! lexer punts to Sema. The doc's bet: perl-lsp's symbol-table + reparse
//! engine — the same primitive that resolves Perl prototypes, where a
//! declaration fact changes how later tokens parse — makes B1 reachable
//! WITHOUT a compiler frontend.
//!
//! The probe sharpened the bet. tree-sitter-c does not even guess both
//! ways: it **defaults `a * b;` to a declaration, always**, error-free.
//! So the hack is a FLAT category flip at one span (decl ↔ expr), driven
//! by one bit — is the first name a type or a value? That is strictly
//! easier than the Perl case: no token regrouping, no text re-parse, no
//! AnchorMap. The resolution layer reinterprets the node in place.
//!
//! Posture: conservative / zero-FP. We override tree-sitter's
//! declaration default ONLY on positive evidence that the first name is
//! a value. Unknown names keep the default — no speculative flips. The
//! cross-file table (a header's `typedef` / `extern`) is the perl-lsp
//! differentiator: tree-sitter cannot see it; we can. Not wired into the
//! pipeline; measured by `c_reparse_tests.rs`.

use std::collections::HashSet;
use tree_sitter::{Query, QueryCursor, StreamingIterator, Tree};

/// The one fact the lexer hack turns on, per name: is it a type or a
/// value? Collected per file; `merge`d across files for the cross-file
/// resolution that tree-sitter structurally cannot do.
#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    pub types: HashSet<String>,
    pub values: HashSet<String>,
}

impl SymbolTable {
    pub fn from_tree(tree: &Tree, src: &[u8]) -> Self {
        let mut t = SymbolTable::default();
        t.absorb(tree, src);
        t
    }

    pub fn merge(&mut self, other: &SymbolTable) {
        self.types.extend(other.types.iter().cloned());
        self.values.extend(other.values.iter().cloned());
    }

    /// `Some(true)` = type, `Some(false)` = value, `None` = unknown.
    /// A name that is both (shadowing) resolves as a type here — a
    /// finer model would scope it; the spike keeps the file-flat view.
    pub fn classify(&self, name: &str) -> Option<bool> {
        if self.types.contains(name) {
            Some(true)
        } else if self.values.contains(name) {
            Some(false)
        } else {
            None
        }
    }

    fn absorb(&mut self, tree: &Tree, src: &[u8]) {
        let lang = tree.language();
        let query = Query::new(&lang, TABLE_QUERY).expect("table query");
        let names: Vec<&str> = query.capture_names().to_vec();
        let mut cursor = QueryCursor::new();
        let mut it = cursor.matches(&query, tree.root_node(), src);
        while let Some(m) = it.next() {
            for c in m.captures {
                let txt = c.node.utf8_text(src).unwrap_or("").to_string();
                match names[c.index as usize] {
                    "type" => {
                        self.types.insert(txt);
                    }
                    "val" => {
                        self.values.insert(txt);
                    }
                    _ => {}
                }
            }
        }
    }
}

// Types: typedef targets + struct/union/enum tags. Values: declarations
// with a CONCRETE (primitive/sized) type, initialized declarators,
// function names, parameters. We deliberately do NOT harvest values from
// `(pointer_declarator (identifier))` under a `type_identifier` — that
// is the ambiguous node itself, and trusting it would feed the phantom
// variable straight back into the table.
const TABLE_QUERY: &str = r#"
(type_definition declarator: (type_identifier) @type)
(struct_specifier name: (type_identifier) @type)
(union_specifier name: (type_identifier) @type)
(enum_specifier name: (type_identifier) @type)

(declaration type: (primitive_type) declarator: (identifier) @val)
(declaration type: (sized_type_specifier) declarator: (identifier) @val)
(declaration type: (primitive_type)
  declarator: (init_declarator declarator: (identifier) @val))
(declaration type: (sized_type_specifier)
  declarator: (init_declarator declarator: (identifier) @val))
(function_declarator declarator: (identifier) @val)
(parameter_declaration declarator: (identifier) @val)
(parameter_declaration declarator: (pointer_declarator declarator: (identifier) @val))
"#;

// The ambiguous site: tree-sitter parsed `T * V;` as a pointer
// declaration. Whether that is right depends entirely on T.
const HACK_QUERY: &str = r#"
(declaration
  type: (type_identifier) @lhs
  declarator: (pointer_declarator declarator: (identifier) @rhs)) @site
"#;

/// What `T * V;` actually IS, once the table speaks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerHack {
    /// `T` is a type → a genuine pointer declaration of `var: T*`.
    Declaration { type_name: String, var_name: String },
    /// `T` is a value → a multiply expression-statement `T * V`; there
    /// is NO variable `V`, and both names are reads.
    Multiply { lhs: String, rhs: String },
}

impl LexerHack {
    /// The phantom a naive extractor would wrongly mint: under a
    /// Multiply verdict, `var_name` is NOT a declared variable.
    pub fn declares_var(&self) -> Option<&str> {
        match self {
            LexerHack::Declaration { var_name, .. } => Some(var_name),
            LexerHack::Multiply { .. } => None,
        }
    }
}

/// Resolve every ambiguous `T * V;` in `tree` against `table`.
/// Conservative: flips tree-sitter's declaration default to a multiply
/// ONLY when `T` is a known value. Unknown `T` keeps the default.
pub fn resolve_lexer_hacks(tree: &Tree, src: &[u8], table: &SymbolTable) -> Vec<LexerHack> {
    let lang = tree.language();
    let query = Query::new(&lang, HACK_QUERY).expect("hack query");
    let names: Vec<&str> = query.capture_names().to_vec();
    let mut out = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&query, tree.root_node(), src);
    while let Some(m) = it.next() {
        let mut lhs = None;
        let mut rhs = None;
        for c in m.captures {
            let txt = c.node.utf8_text(src).unwrap_or("").to_string();
            match names[c.index as usize] {
                "lhs" => lhs = Some(txt),
                "rhs" => rhs = Some(txt),
                _ => {}
            }
        }
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else { continue };
        match table.classify(&lhs) {
            Some(false) => out.push(LexerHack::Multiply { lhs, rhs }),
            _ => out.push(LexerHack::Declaration { type_name: lhs, var_name: rhs }),
        }
    }
    out
}

#[cfg(test)]
#[path = "c_reparse_tests.rs"]
mod tests;
