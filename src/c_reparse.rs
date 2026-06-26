//! SPIKE: the C lexer hack, resolved by REPARSE (Class B1 of
//! `~/personal/resume/research-static-analysis.md`).
//!
//! `a * b;` is the canonical C ambiguity: a multiply-expression if `a`
//! is a value, a pointer DECLARATION of `b` if `a` is a type. The probe
//! pinned tree-sitter-c's behavior: it **defaults to a declaration,
//! unconditionally** (error-free, even when `a` is plainly a value
//! in-file) — a silent wrong tree, decided by one bit: is the first name
//! a type or a value?
//!
//! Why reparse and not a verdict-on-the-side: the engine has no IR, so a
//! side table saying "this declaration node is really a multiply" would
//! have to be consulted by every downstream tree consumer (builder,
//! extractor, ref/symbol emission) — a parallel store that drifts. The
//! consistent fix, the same one the Perl prototype reparenthesizer and
//! the C++ macro expander use: **rewrite the source so the re-parse is
//! unambiguous**, then downstream just walks the corrected tree. The
//! symbol table is consulted ONCE, here, strictly upstream.
//!
//! The transform: wrap a value-LHS site in parens. `a * b;` → `(a * b);`
//! parses as `expression_statement(parenthesized_expression(binary))` —
//! `(a * b)` cannot be a declarator, so tree-sitter commits to the
//! multiply. (A leading `+` or trailing `+0` both go to ERROR; `(a)*b`
//! becomes a cast. Whole-statement parens is the one clean forcing
//! edit.) An `AnchorMap` carries spans back to original source.
//!
//! Posture: conservative / zero-FP. We rewrite ONLY on positive evidence
//! that the first name is a value. Unknown names keep tree-sitter's
//! declaration default. The cross-file table (a header's `typedef` /
//! `extern`) is the perl-lsp differentiator: tree-sitter cannot see it.
//! Not wired into the pipeline; measured by `c_reparse_tests.rs`.

use std::collections::HashSet;
use tree_sitter::{Node, Query, QueryCursor, StreamingIterator, Tree};

/// The one fact the lexer hack turns on, per name: type or value?
/// Collected per file; `merge`d across files for the cross-file
/// resolution tree-sitter structurally cannot do.
/// A name's lexer-hack-relevant category.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    Type,
    Value,
    /// Both a type and a value across the file — config-dependent (B1
    /// leak). Never disambiguate these by superposition.
    Ambiguous,
    Unknown,
}

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

    /// A name's category. `Ambiguous` = collected as BOTH a type and a
    /// value across the file (the B1 leak: an `#ifdef` made it a type in
    /// one arm and a value in another, so code outside the `#ifdef`
    /// parses differently per config). Disambiguation must NOT silently
    /// pick for an ambiguous name — it's config-dependent; monomorphize
    /// by running `select_config` first, then collecting the table.
    pub fn classify(&self, name: &str) -> Category {
        match (self.types.contains(name), self.values.contains(name)) {
            (true, true) => Category::Ambiguous,
            (true, false) => Category::Type,
            (false, true) => Category::Value,
            (false, false) => Category::Unknown,
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
// the ambiguous `(pointer_declarator (identifier))` node — trusting it
// would feed the phantom variable straight back into the table.
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

// The ambiguous site tree-sitter parsed as a pointer declaration.
const HACK_QUERY: &str = r#"
(declaration
  type: (type_identifier) @lhs
  declarator: (pointer_declarator declarator: (identifier) @rhs))
"#;

/// Transformed-source ↔ original-source map (insertion-only: every edit
/// is a `(` or `)` wrap). A byte inside an inserted paren collapses to
/// the insertion site; otherwise we subtract preceding insertions.
#[derive(Debug, Default, Clone)]
pub struct AnchorMap {
    inserts: Vec<(usize, usize)>, // (transformed_pos, original_pos), sorted
}

impl AnchorMap {
    pub fn to_original(&self, transformed: usize) -> usize {
        let mut shift = 0usize;
        for &(t_at, _orig) in &self.inserts {
            if transformed > t_at {
                shift += 1; // each insert is one byte, fully before us
            } else if transformed == t_at {
                return t_at - shift; // on the inserted paren → its site
            }
        }
        transformed - shift
    }
}

/// Rewrite every value-LHS `T * V;` to `(T * V);` so the re-parse is an
/// unambiguous multiply. Returns the rewritten source and anchor map;
/// the caller re-parses. Type/unknown sites are left untouched (the
/// declaration default stands).
pub fn disambiguate(tree: &Tree, src: &str, table: &SymbolTable) -> (String, AnchorMap) {
    let lang = tree.language();
    let query = Query::new(&lang, HACK_QUERY).expect("hack query");
    let names: Vec<&str> = query.capture_names().to_vec();
    // (insert_at_byte, is_open) — wrap the multiply in parens.
    let mut inserts: Vec<(usize, bool)> = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&query, tree.root_node(), src.as_bytes());
    while let Some(m) = it.next() {
        let mut lhs: Option<Node> = None;
        let mut rhs: Option<Node> = None;
        for c in m.captures {
            match names[c.index as usize] {
                "lhs" => lhs = Some(c.node),
                "rhs" => rhs = Some(c.node),
                _ => {}
            }
        }
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else { continue };
        let name = lhs.utf8_text(src.as_bytes()).unwrap_or("");
        // rewrite ONLY on unambiguous value evidence. Type / Unknown keep
        // tree-sitter's declaration default; Ambiguous is config-
        // dependent and must not be silently flipped.
        if table.classify(name) == Category::Value {
            inserts.push((lhs.start_byte(), true)); // `(` before the lhs
            inserts.push((rhs.end_byte(), false)); // `)` after the rhs
        }
    }
    apply(src, &mut inserts)
}

fn apply(src: &str, inserts: &mut [(usize, bool)]) -> (String, AnchorMap) {
    inserts.sort_by_key(|&(at, open)| (at, !open)); // close-before-open on ties
    let mut out = String::with_capacity(src.len() + inserts.len());
    let mut map = AnchorMap::default();
    let mut prev = 0usize;
    for &(at, open) in inserts.iter() {
        out.push_str(&src[prev..at]);
        map.inserts.push((out.len(), at));
        out.push(if open { '(' } else { ')' });
        prev = at;
    }
    out.push_str(&src[prev..]);
    (out, map)
}

#[cfg(test)]
#[path = "c_reparse_tests.rs"]
mod tests;
