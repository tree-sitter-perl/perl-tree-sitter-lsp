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
    ts_parser_perl::LANGUAGE.into()
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
#[path = "query_cache_tests.rs"]
mod tests;
