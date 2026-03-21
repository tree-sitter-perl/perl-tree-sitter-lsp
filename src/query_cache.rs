//! Pre-compiled tree-sitter queries, cached via `OnceLock`.
//!
//! Each query is compiled once on first access and reused for all subsequent calls.
//! Thread-safe and zero-cost after initialization.
//!
//! To add a new query:
//! 1. Add a public function that returns `&'static Query`
//! 2. Define the S-expression pattern as a string literal
//! 3. Use `get_or_init` with `OnceLock` to compile once
//!
//! ## Export extraction coverage
//!
//! Supported patterns:
//! - `@EXPORT = qw(foo bar)` / `@EXPORT_OK = qw(foo bar)` (with or without `our`)
//! - `@EXPORT = ('foo', 'bar')` / single element `('foo')`
//! - Mixed: `@EXPORT = ('foo', qw/bar baz/)`
//! - `(qw/.../)` wrapped in redundant parens
//!
//! Not supported (intentionally):
//! - `push @EXPORT, 'foo'` — different AST shape (function call, not assignment),
//!   rare in practice, and often conditional. Modules using `push` typically also
//!   have a base `@EXPORT =` assignment we already catch.

use std::sync::OnceLock;
use tree_sitter::Query;

fn perl_language() -> tree_sitter::Language {
    tree_sitter_perl::LANGUAGE.into()
}

/// Query for `requires 'Module::Name'` calls in cpanfiles.
///
/// Matches both explicit `requires('Foo')` and ambiguous `requires 'Foo'` forms,
/// with single args and multi-args (version constraints). The `.` anchor on
/// `list_expression` ensures only the first string (module name) matches.
pub fn cpanfile_requires() -> &'static Query {
    static QUERY: OnceLock<Query> = OnceLock::new();
    QUERY.get_or_init(|| {
        Query::new(
            &perl_language(),
            r#"
            [
              (function_call_expression
                function: (_) @fn
                (string_literal (string_content) @module))
              (function_call_expression
                function: (_) @fn
                (list_expression . (string_literal (string_content) @module)))
              (ambiguous_function_call_expression
                function: (_) @fn
                (string_literal (string_content) @module))
              (ambiguous_function_call_expression
                function: (_) @fn
                (list_expression . (string_literal (string_content) @module)))
            ]
            (#eq? @fn "requires")
            "#,
        )
        .expect("cpanfile_requires query should compile")
    })
}

/// Query for `@EXPORT = qw(...)` and `@EXPORT_OK = qw(...)` assignments.
/// Now only used in tests — export extraction moved to builder.
#[cfg(test)]
pub fn exports_qw() -> &'static Query {
    static QUERY: OnceLock<Query> = OnceLock::new();
    QUERY.get_or_init(|| {
        Query::new(
            &perl_language(),
            r#"
            [
              (assignment_expression
                left: [
                  (variable_declaration (array) @var)
                  (array) @var
                ]
                (quoted_word_list (string_content) @words))
              (assignment_expression
                left: [
                  (variable_declaration (array) @var)
                  (array) @var
                ]
                (list_expression
                  (quoted_word_list (string_content) @words)))
            ]
            "#,
        )
        .expect("exports_qw query should compile")
    })
}

/// Query for `@EXPORT = ('foo', 'bar')` list assignments.
/// Now only used in tests — export extraction moved to builder.
#[cfg(test)]
pub fn exports_paren_list() -> &'static Query {
    static QUERY: OnceLock<Query> = OnceLock::new();
    QUERY.get_or_init(|| {
        Query::new(
            &perl_language(),
            r#"
            [
              (assignment_expression
                left: [
                  (variable_declaration (array) @var)
                  (array) @var
                ]
                (list_expression
                  (string_literal (string_content) @word)))
              (assignment_expression
                left: [
                  (variable_declaration (array) @var)
                  (array) @var
                ]
                (string_literal (string_content) @word))
            ]
            "#,
        )
        .expect("exports_paren_list query should compile")
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::{Parser, QueryCursor, StreamingIterator};

    fn parse(source: &str) -> tree_sitter::Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&perl_language())
            .unwrap();
        parser.parse(source, None).unwrap()
    }

    #[test]
    fn test_cpanfile_requires_cached() {
        // First call compiles, second call returns same reference.
        let q1 = cpanfile_requires();
        let q2 = cpanfile_requires();
        assert!(std::ptr::eq(q1, q2), "Should return same cached query");
    }

    #[test]
    fn test_cpanfile_requires_matches() {
        let source = "requires 'DBI';\nrequires 'JSON', '>= 2.0';";
        let tree = parse(source);
        let query = cpanfile_requires();
        let module_idx = query.capture_index_for_name("module").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut modules = Vec::new();
        while let Some(m) = matches.next() {
            for cap in m.captures {
                if cap.index == module_idx {
                    modules.push(cap.node.utf8_text(source.as_bytes()).unwrap().to_string());
                }
            }
        }
        assert_eq!(modules, vec!["DBI", "JSON"]);
    }

    #[test]
    fn test_exports_qw_matches() {
        let source = r#"
our @EXPORT_OK = qw(alpha beta gamma);
our @EXPORT = qw(delta);
"#;
        let tree = parse(source);
        let query = exports_qw();
        let var_idx = query.capture_index_for_name("var").unwrap();
        let words_idx = query.capture_index_for_name("words").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut results: Vec<(String, Vec<String>)> = Vec::new();
        while let Some(m) = matches.next() {
            let mut var_name = String::new();
            let mut words = Vec::new();
            for cap in m.captures {
                let text = cap.node.utf8_text(source.as_bytes()).unwrap();
                if cap.index == var_idx {
                    var_name = text.to_string();
                } else if cap.index == words_idx {
                    words.extend(text.split_whitespace().map(String::from));
                }
            }
            if !var_name.is_empty() {
                results.push((var_name, words));
            }
        }
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, "@EXPORT_OK");
        assert_eq!(results[0].1, vec!["alpha", "beta", "gamma"]);
        assert_eq!(results[1].0, "@EXPORT");
        assert_eq!(results[1].1, vec!["delta"]);
    }

    #[test]
    fn test_exports_paren_list_matches() {
        let source = r#"
our @EXPORT_OK = ('foo', 'bar', 'baz');
"#;
        let tree = parse(source);
        let query = exports_paren_list();
        let var_idx = query.capture_index_for_name("var").unwrap();
        let word_idx = query.capture_index_for_name("word").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut var_name = String::new();
        let mut words = Vec::new();
        while let Some(m) = matches.next() {
            for cap in m.captures {
                let text = cap.node.utf8_text(source.as_bytes()).unwrap();
                if cap.index == var_idx && var_name.is_empty() {
                    var_name = text.to_string();
                } else if cap.index == word_idx {
                    words.push(text.to_string());
                }
            }
        }
        assert_eq!(var_name, "@EXPORT_OK");
        assert_eq!(words, vec!["foo", "bar", "baz"]);
    }

    #[test]
    fn test_exports_paren_list_single_element() {
        // Single element: no list_expression, bare string_literal on right
        let source = "our @EXPORT_OK = ('single');\n";
        let tree = parse(source);
        let query = exports_paren_list();
        let word_idx = query.capture_index_for_name("word").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut words = Vec::new();
        while let Some(m) = matches.next() {
            for cap in m.captures {
                if cap.index == word_idx {
                    words.push(cap.node.utf8_text(source.as_bytes()).unwrap().to_string());
                }
            }
        }
        assert_eq!(words, vec!["single"]);
    }

    #[test]
    fn test_exports_qw_no_our() {
        // Without `our` — bare @EXPORT
        let source = "@EXPORT = qw(no_our);\n";
        let tree = parse(source);
        let query = exports_qw();
        let var_idx = query.capture_index_for_name("var").unwrap();
        let words_idx = query.capture_index_for_name("words").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut var_name = String::new();
        let mut words = Vec::new();
        while let Some(m) = matches.next() {
            for cap in m.captures {
                let text = cap.node.utf8_text(source.as_bytes()).unwrap();
                if cap.index == var_idx {
                    var_name = text.to_string();
                } else if cap.index == words_idx {
                    words.extend(text.split_whitespace().map(String::from));
                }
            }
        }
        assert_eq!(var_name, "@EXPORT");
        assert_eq!(words, vec!["no_our"]);
    }

    #[test]
    fn test_exports_qw_inside_parens() {
        // (qw/.../) — tree-sitter drops parens, qw is direct child
        let source = "our @EXPORT = (qw/lolz this is nested/);\n";
        let tree = parse(source);
        let query = exports_qw();
        let words_idx = query.capture_index_for_name("words").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        let mut words = Vec::new();
        while let Some(m) = matches.next() {
            for cap in m.captures {
                if cap.index == words_idx {
                    words.extend(
                        cap.node.utf8_text(source.as_bytes()).unwrap()
                            .split_whitespace().map(String::from),
                    );
                }
            }
        }
        assert_eq!(words, vec!["lolz", "this", "is", "nested"]);
    }

    #[test]
    fn test_exports_mixed_strings_and_qw() {
        // ('now', 'what', qw/man this/) — need both queries to get all words
        let source = "our @EXPORT_OK = ('now', 'what', qw/man this is stuffs/);\n";
        let tree = parse(source);
        let root = tree.root_node();
        let bytes = source.as_bytes();

        // Collect from qw query
        let qw = exports_qw();
        let qw_words_idx = qw.capture_index_for_name("words").unwrap();
        let mut cursor1 = QueryCursor::new();
        let mut matches1 = cursor1.matches(qw, root, bytes);
        let mut all_words = Vec::new();
        while let Some(m) = matches1.next() {
            for cap in m.captures {
                if cap.index == qw_words_idx {
                    all_words.extend(
                        cap.node.utf8_text(bytes).unwrap()
                            .split_whitespace().map(String::from),
                    );
                }
            }
        }

        // Collect from paren_list query
        let pl = exports_paren_list();
        let pl_word_idx = pl.capture_index_for_name("word").unwrap();
        let mut cursor2 = QueryCursor::new();
        let mut matches2 = cursor2.matches(pl, root, bytes);
        while let Some(m) = matches2.next() {
            for cap in m.captures {
                if cap.index == pl_word_idx {
                    all_words.push(cap.node.utf8_text(bytes).unwrap().to_string());
                }
            }
        }

        all_words.sort();
        assert_eq!(all_words, vec!["is", "man", "now", "stuffs", "this", "what"]);
    }

    #[test]
    fn test_exports_empty_qw() {
        let source = "our @EXPORT = qw();\n";
        let tree = parse(source);

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(exports_qw(), tree.root_node(), source.as_bytes());
        assert!(matches.next().is_none(), "empty qw should produce no matches");
    }

    #[test]
    fn test_exports_empty_parens() {
        let source = "our @EXPORT_OK = ();\n";
        let tree = parse(source);

        let mut cursor1 = QueryCursor::new();
        let mut matches1 = cursor1.matches(exports_qw(), tree.root_node(), source.as_bytes());
        assert!(matches1.next().is_none());

        let mut cursor2 = QueryCursor::new();
        let mut matches2 = cursor2.matches(exports_paren_list(), tree.root_node(), source.as_bytes());
        assert!(matches2.next().is_none());
    }
}
