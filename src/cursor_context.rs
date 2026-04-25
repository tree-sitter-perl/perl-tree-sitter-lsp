//! Cursor context detection — all on-demand tree walking lives here.
//!
//! Rule: if it touches the tree, it goes in this module.
//! FileAnalysis handles resolution via table lookups; this module
//! figures out *what* the user is doing at the cursor.

use std::collections::HashSet;
use tree_sitter::{Node, Point, Tree};

use crate::file_analysis::{extract_call_name, node_to_span, FileAnalysis, InferredType, Span};

// ---- Types ----

/// What the user is doing at the cursor position.
/// Detected from the text before the cursor (no tree needed).
#[derive(Debug, PartialEq)]
pub enum CursorContext {
    /// After `$`, `@`, or `%` — variable completion.
    Variable { sigil: char },
    /// After `->` — method completion. Type is resolved when available.
    Method { invocant_type: Option<InferredType>, invocant_text: String },
    /// After `$var->{` or `$hash{` — hash key completion. Type is resolved when available.
    HashKey { owner_type: Option<InferredType>, var_text: String, source_sub: Option<String> },
    /// On a `use`/`require` line — completing module name or import list.
    UseStatement {
        /// Module name typed so far (e.g. "Mojo::Ba" or "" if just "use ")
        module_prefix: String,
        /// If true, cursor is inside the import list (qw, parens, or bare string after module)
        in_import_list: bool,
        /// The fully typed module name (only set when in_import_list is true)
        module_name: Option<String>,
    },
    /// No specific trigger — general completion.
    General,
}

/// Context about a function/method call enclosing the cursor.
/// Everything computed from the tree so callers need zero tree access.
#[derive(Debug)]
pub struct CallContext {
    /// Name of the function or method being called.
    pub name: String,
    /// Whether this is a method call (via `->`) vs a function call.
    pub is_method: bool,
    /// The invocant text (e.g. `$self`, `Calculator`) for method calls.
    pub invocant: Option<String>,
    /// Zero-based index of the active parameter (comma count before cursor).
    pub active_param: usize,
    /// Whether the cursor is at a key position in key => value pairs
    /// (even separator count = key, odd = value).
    pub at_key_position: bool,
    /// Keys already typed at the call site (barewords/strings before `=>`).
    pub used_keys: HashSet<String>,
    /// The first argument as a constant string when it folds to a literal.
    /// Used by string-dispatch signature help (`$x->emit('ready', CURSOR)`
    /// looks this up against HashKeyDefs to surface handler params). `None`
    /// when the first arg is a variable/expression.
    pub first_arg_string: Option<String>,
}

// ---- Cursor context detection (text-only, no tree) ----

/// Detect what the user is doing at the cursor from the text before cursor.
/// When `analysis` is provided, also tries to resolve invocant/owner types.
pub fn detect_cursor_context(source: &str, point: Point, analysis: Option<&FileAnalysis>) -> CursorContext {
    let line = match source.lines().nth(point.row) {
        Some(l) => l,
        None => return CursorContext::General,
    };
    let before = if point.column <= line.len() {
        &line[..point.column]
    } else {
        line
    };
    let trimmed = before.trim_end();

    // Check for use/require line — module name or import list completion
    {
        let line_trimmed = line.trim_start();
        if line_trimmed.starts_with("use ") || line_trimmed.starts_with("require ") {
            let keyword = if line_trimmed.starts_with("use ") { "use " } else { "require " };
            let after_keyword = line_trimmed.strip_prefix(keyword).unwrap_or("");
            // How much of the line is before the cursor?
            let leading_ws = line.len() - line.trim_start().len();
            let cursor_in_line = if point.column > leading_ws + keyword.len() {
                point.column - leading_ws - keyword.len()
            } else {
                0
            };
            let before_cursor_in_after = if cursor_in_line <= after_keyword.len() {
                &after_keyword[..cursor_in_line]
            } else {
                after_keyword
            };
            return detect_use_context(before_cursor_in_after, keyword == "require ");
        }
    }

    // Check for hash key completion: $hash{ or $self->{ (including mid-word: $var->{ho)
    {
        let check = strip_trailing_identifier(trimmed);
        if check.ends_with('{') {
            let prefix = check[..check.len() - 1].trim_end();
            // Arrow hash access: $var->{
            if prefix.ends_with("->") {
                let var_prefix = prefix[..prefix.len() - 2].trim_end();
                let var_text = extract_invocant_from_prefix(var_prefix);
                if !var_text.is_empty() {
                    let owner_type = resolve_text_invocant(var_text, point, analysis);
                    return CursorContext::HashKey {
                        owner_type,
                        var_text: var_text.to_string(),
                        source_sub: None,
                    };
                }
            }
            // Direct hash access: $hash{
            let var_text = extract_invocant_from_prefix(prefix);
            if !var_text.is_empty() && var_text.starts_with('$') {
                let owner_type = resolve_text_invocant(var_text, point, analysis);
                return CursorContext::HashKey {
                    owner_type,
                    var_text: var_text.to_string(),
                    source_sub: None,
                };
            }
        }
    }

    // Check for -> (method completion), including mid-word: $p->mag
    {
        let check = strip_trailing_identifier(trimmed);
        if check.ends_with("->") {
            let prefix = check[..check.len() - 2].trim_end();
            let invocant = extract_invocant_from_prefix(prefix);
            if !invocant.is_empty() {
                let invocant_type = resolve_text_invocant(invocant, point, analysis);
                return CursorContext::Method {
                    invocant_type,
                    invocant_text: invocant.to_string(),
                };
            }
        }
    }

    // Check for sigil trigger
    if let Some(last_char) = trimmed.chars().last() {
        if matches!(last_char, '$' | '@' | '%') {
            return CursorContext::Variable { sigil: last_char };
        }
    }

    CursorContext::General
}

/// Resolve an invocant type from its text, using analysis when available.
fn resolve_text_invocant(text: &str, point: Point, analysis: Option<&FileAnalysis>) -> Option<InferredType> {
    if !text.starts_with('$') && !text.starts_with('@') && !text.starts_with('%') {
        // Bareword — treat as class name
        Some(InferredType::ClassName(text.to_string()))
    } else if let Some(a) = analysis {
        a.inferred_type(text, point).cloned()
    } else {
        None
    }
}

/// Strip trailing identifier chars (partial method name typed so far).
/// `"$p->mag"` → `"$p->"`, `"$p->"` → `"$p->"` (unchanged).
fn strip_trailing_identifier(s: &str) -> &str {
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        let ch = bytes[i - 1] as char;
        if ch.is_alphanumeric() || ch == '_' {
            i -= 1;
        } else {
            break;
        }
    }
    &s[..i]
}

/// Extract the invocant token from the text before `->` or `{`.
/// Detect whether we're completing a module name or an import list on a use/require line.
/// `text` is the content after "use " / "require ", up to the cursor position.
fn detect_use_context(text: &str, is_require: bool) -> CursorContext {
    // Skip special pragmas that don't take module-style arguments
    if text.starts_with("strict") || text.starts_with("warnings") || text.starts_with("utf8")
        || text.starts_with("feature") || text.starts_with("constant ")
        || text.starts_with("lib ") || text.starts_with("v5")
    {
        return CursorContext::General;
    }

    // Find the module name: sequence of word chars and :: up to whitespace/paren/quote/semicolon
    let module_end = text.find(|c: char| c.is_whitespace() || c == '(' || c == '\'' || c == '"' || c == ';')
        .unwrap_or(text.len());
    let module_prefix = &text[..module_end];

    // If there's content after the module name, we're in the import list
    let rest = text[module_end..].trim_start();
    if !rest.is_empty() && !module_prefix.is_empty() && !is_require {
        // Skip use parent/base — those take class names, handled elsewhere
        if module_prefix == "parent" || module_prefix == "base" {
            return CursorContext::General;
        }
        return CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some(module_prefix.to_string()),
        };
    }

    // Still typing the module name
    CursorContext::UseStatement {
        module_prefix: module_prefix.to_string(),
        in_import_list: false,
        module_name: None,
    }
}

fn extract_invocant_from_prefix(prefix: &str) -> &str {
    let bytes = prefix.as_bytes();
    let end = bytes.len();
    let mut i = end;
    while i > 0 {
        i -= 1;
        let ch = bytes[i] as char;
        if ch.is_alphanumeric() || ch == '_' || ch == ':' {
            continue;
        }
        if ch == '$' || ch == '@' || ch == '%' {
            return &prefix[i..end];
        }
        return &prefix[i + 1..end];
    }
    &prefix[..end]
}

// ---- Tree-based cursor context detection ----

/// Detect cursor context using the tree + analysis for expression type resolution.
///
/// Handles cases the text-based detector can't: call-expression invocants
/// like `get_config()->` or `$obj->get_bar()->`.
///
/// Returns `None` if no expression-based context is detected (caller should
/// fall back to text-based detection).
#[cfg(test)]
pub fn detect_cursor_context_tree(
    tree: &Tree,
    source: &[u8],
    point: Point,
    analysis: &FileAnalysis,
) -> Option<CursorContext> {
    detect_cursor_context_tree_with_index(tree, source, point, analysis, None)
}

/// Variant that threads a module index through for cross-file type
/// resolution. Callers that have an index (the LSP completion path)
/// use this so chained calls like `$c->helper_root->...` see methods
/// declared in other files — without the index, `$c->users` can't
/// resolve to the helper's synthetic return type and completion
/// falls through to the untyped path.
pub fn detect_cursor_context_tree_with_index(
    tree: &Tree,
    source: &[u8],
    point: Point,
    analysis: &FileAnalysis,
    module_index: Option<&crate::module_index::ModuleIndex>,
) -> Option<CursorContext> {
    // Find the node at/just before the cursor
    let check_point = if point.column > 0 {
        Point::new(point.row, point.column - 1)
    } else {
        point
    };
    let node = tree.root_node().descendant_for_point_range(check_point, check_point)?;

    // Walk up looking for an incomplete method_call_expression or hash_element_expression
    let mut current = node;
    for _ in 0..20 {
        match current.kind() {
            "method_call_expression" => {
                // Check if cursor is after -> (in the method position)
                if let Some(invocant_node) = current.child_by_field_name("invocant") {
                    if invocant_node.end_position() < point {
                        // stub_expression means the parser absorbed "()" from a function
                        // call like `get_config()->`. The real invocant is the parent
                        // ambiguous_function_call_expression (i.e. the full `get_config()` call).
                        if invocant_node.kind() == "stub_expression" {
                            if let Some(parent) = current.parent() {
                                if parent.kind() == "ambiguous_function_call_expression"
                                    || parent.kind() == "function_call_expression"
                                {
                                    let invocant_text = parent.utf8_text(source).unwrap_or("").to_string();
                                    let invocant_type = resolve_node_type(parent, source, analysis, point, module_index);
                                    return Some(CursorContext::Method { invocant_type, invocant_text });
                                }
                            }
                        }
                        let invocant_text = invocant_node.utf8_text(source).unwrap_or("").to_string();
                        let invocant_type = resolve_node_type(invocant_node, source, analysis, point, module_index);
                        return Some(CursorContext::Method { invocant_type, invocant_text });
                    }
                }
            }
            "hash_element_expression" => {
                // Check if cursor is inside the {key} part
                if let Some(base) = current.named_child(0) {
                    if base.end_position() < point {
                        let var_text = base.utf8_text(source).unwrap_or("").to_string();
                        let owner_type = resolve_node_type(base, source, analysis, point, module_index);
                        let source_sub = extract_call_name(base, source);
                        return Some(CursorContext::HashKey { owner_type, var_text, source_sub });
                    }
                }
            }
            "anonymous_hash_expression" => {
                // `{ key => ... }` literal. If it lives as a positional
                // argument to a method/function call, the hash's owner
                // is the called sub — mirror of "inside ->{…}" detection
                // so `foo($x, { | })` completes against foo()'s final
                // hashref-arg keys. Only trigger when the cursor is at
                // a key position inside the literal (even-comma count).
                if let Some(call_name) = enclosing_call_for_hash_arg(current, source) {
                    if cursor_at_hash_key_slot(current, point) {
                        return Some(CursorContext::HashKey {
                            owner_type: None,
                            var_text: String::new(),
                            source_sub: Some(call_name),
                        });
                    }
                }
            }
            "ERROR" => {
                // Incomplete expression: look for `expr->{` or `expr->` patterns
                // in error recovery
                if let Some(ctx) = detect_from_error_node(current, source, point, analysis, module_index) {
                    return Some(ctx);
                }
            }
            _ => {}
        }
        current = current.parent()?;
    }
    None
}

/// For an `anonymous_hash_expression` node, find the enclosing
/// method/function call whose argument list contains it. Returns the
/// callee name; None when the hash literal lives outside any call
/// (e.g. `my $cfg = { ... }` — a pure constructor, no caller context).
/// Used by cursor-context detection to route `foo($x, { | })` to the
/// callee's HashKey completions without touching the core's hash-
/// ownership model.
fn enclosing_call_for_hash_arg(hash_node: Node, source: &[u8]) -> Option<String> {
    // Walk up until we find the call whose args include this hash.
    // Stop at a sub-scope boundary (block/sub-body) so we don't
    // leak across function boundaries.
    let mut n = hash_node;
    for _ in 0..16 {
        let parent = n.parent()?;
        match parent.kind() {
            "method_call_expression" => {
                let method = parent.child_by_field_name("method")?;
                return method.utf8_text(source).ok().map(|s| s.to_string());
            }
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let func = parent.child_by_field_name("function")?;
                return func.utf8_text(source).ok().map(|s| s.to_string());
            }
            "block" | "subroutine_declaration_statement"
            | "method_declaration_statement" | "anonymous_subroutine_expression"
            | "class_statement" | "package_statement" => return None,
            _ => {}
        }
        n = parent;
    }
    None
}

/// Inside an `anonymous_hash_expression`, determine whether the cursor
/// sits at a key slot (even number of `,`/`=>` separators since the
/// opening `{`). Matches the call-arg key-vs-value counting.
fn cursor_at_hash_key_slot(hash_node: Node, point: Point) -> bool {
    count_separators_before(hash_node, point) % 2 == 0
}

/// Resolve the type of a tree-sitter node used as an invocant or hash owner.
/// Handles both simple variables (via `inferred_type`) and complex expressions.
fn resolve_node_type(
    node: Node,
    source: &[u8],
    analysis: &FileAnalysis,
    point: Point,
    module_index: Option<&crate::module_index::ModuleIndex>,
) -> Option<InferredType> {
    match node.kind() {
        "scalar" | "array" | "hash" => {
            let text = node.utf8_text(source).ok()?;
            analysis.inferred_type(text, point).cloned()
        }
        "bareword" | "package" => {
            let text = node.utf8_text(source).ok()?;
            Some(InferredType::ClassName(text.to_string()))
        }
        _ => analysis.resolve_expression_type(node, source, module_index),
    }
}

/// Try to detect expression context from an ERROR node.
///
/// When typing `get_config()->` or `$obj->get_bar()->`, tree-sitter often
/// wraps the incomplete expression in an ERROR node. Look for patterns like:
/// - `[call_expr] -> [method?]` → method completion on call expression
/// - `[call_expr] -> {` → hash key completion on call expression
fn detect_from_error_node(
    error: Node,
    source: &[u8],
    point: Point,
    analysis: &FileAnalysis,
    module_index: Option<&crate::module_index::ModuleIndex>,
) -> Option<CursorContext> {
    let mut last_expr: Option<Node> = None;
    let mut saw_arrow = false;
    let mut saw_brace = false;

    for i in 0..error.child_count() {
        let child = error.child(i)?;
        if child.start_position() > point {
            break;
        }

        match child.kind() {
            "method_call_expression" | "function_call_expression"
            | "ambiguous_function_call_expression" | "hash_element_expression"
            | "scalar" | "array" | "hash" | "bareword" | "package" => {
                last_expr = Some(child);
                saw_arrow = false;
                saw_brace = false;
            }
            "->" => {
                saw_arrow = true;
                saw_brace = false;
            }
            "{" if saw_arrow => {
                saw_brace = true;
            }
            _ => {}
        }
    }

    if !saw_arrow {
        return None;
    }

    if let Some(expr) = last_expr {
        let text = expr.utf8_text(source).unwrap_or("").to_string();
        let resolved_type = resolve_node_type(expr, source, analysis, point, module_index);
        if saw_brace {
            let source_sub = extract_call_name(expr, source);
            return Some(CursorContext::HashKey { owner_type: resolved_type, var_text: text, source_sub });
        } else {
            return Some(CursorContext::Method { invocant_type: resolved_type, invocant_text: text });
        }
    }

    None
}

// ---- Call context detection (tree-based) ----

/// Find the enclosing function/method call at the cursor, if any.
/// Returns a CallContext with everything pre-computed from the tree.
pub fn find_call_context(tree: &Tree, source: &[u8], point: Point) -> Option<CallContext> {
    let (name, is_method, invocant, args_node) = find_call_at_cursor(tree, source, point)?;

    let active_param = match args_node {
        Some(args) => active_slot_in_node(args, point),
        None => 0,
    };

    let at_key_position = match args_node {
        Some(args) => count_separators_before(args, point) % 2 == 0,
        None => true, // no args yet = key position
    };

    let used_keys = match args_node {
        Some(args) => collect_used_keys_at_callsite(args, source),
        None => HashSet::new(),
    };

    let first_arg_string = args_node.and_then(|args| first_arg_as_string(args, source));

    Some(CallContext {
        name,
        is_method,
        invocant,
        active_param,
        at_key_position,
        used_keys,
        first_arg_string,
    })
}

/// If the first argument of a call is a string literal or bareword, return
/// its content. Used for string-dispatch signature help.
fn first_arg_as_string(args: Node, source: &[u8]) -> Option<String> {
    let first = if args.kind() == "list_expression" || args.kind() == "parenthesized_expression" {
        args.named_child(0)?
    } else {
        args
    };
    match first.kind() {
        "string_literal" | "interpolated_string_literal" => {
            // Pull the `string_content` child — quote-flavor-agnostic
            // (q{}, qq{}, heredocs, '' vs "") and avoids the
            // trim-delimiters brittleness.
            for i in 0..first.named_child_count() {
                if let Some(child) = first.named_child(i) {
                    if child.kind() == "string_content" {
                        return child.utf8_text(source).ok().map(|s| s.to_string());
                    }
                }
            }
            Some(String::new())
        }
        "bareword" | "autoquoted_bareword" => {
            first.utf8_text(source).ok().map(|s| s.to_string())
        }
        _ => None,
    }
}

/// Find call context at cursor — walks up from cursor to find enclosing call.
/// Returns (function/method name, is_method, invocant text, arguments node).
fn find_call_at_cursor<'a>(
    tree: &'a Tree,
    source: &'a [u8],
    point: Point,
) -> Option<(String, bool, Option<String>, Option<Node<'a>>)> {
    // Try at cursor, then one column back (cursor may be right after `(` or `,`)
    let try_points = if point.column > 0 {
        vec![point, Point::new(point.row, point.column - 1)]
    } else {
        vec![point]
    };

    for pt in try_points {
        if let Some(result) = find_call_at_cursor_from(tree, source, pt, point) {
            return Some(result);
        }
    }

    // Fallback: cursor may be in trailing whitespace after an ERROR node.
    let root = tree.root_node();
    for i in 0..root.child_count() {
        if let Some(child) = root.child(i) {
            if child.kind() == "ERROR"
                && child.end_position().row == point.row
                && child.end_position() <= point
            {
                if let Some(result) = parse_call_from_error(child, source, point) {
                    return Some(result);
                }
                if let Some(result) = find_call_from_error_sibling(child, source, point) {
                    return Some(result);
                }
            }
        }
    }
    None
}

fn find_call_at_cursor_from<'a>(
    tree: &'a Tree,
    source: &'a [u8],
    start_point: Point,
    cursor: Point,
) -> Option<(String, bool, Option<String>, Option<Node<'a>>)> {
    let mut node = tree
        .root_node()
        .descendant_for_point_range(start_point, start_point)?;

    for _ in 0..30 {
        match node.kind() {
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let name = node
                    .child_by_field_name("function")
                    .and_then(|n| n.utf8_text(source).ok())?;
                let args = node.child_by_field_name("arguments");
                return Some((name.to_string(), false, None, args));
            }
            "method_call_expression" => {
                let name = node
                    .child_by_field_name("method")
                    .and_then(|n| n.utf8_text(source).ok())?;
                let invocant = node
                    .child_by_field_name("invocant")
                    .and_then(|n| n.utf8_text(source).ok())
                    .map(|s| s.to_string());
                let args = node.child_by_field_name("arguments");
                if args.is_some() {
                    return Some((name.to_string(), true, invocant, args));
                }
                // No arguments field — check if next sibling is an ERROR with `(`
                let args_from_error = node
                    .parent()
                    .and_then(|p| p.next_sibling())
                    .filter(|s| s.kind() == "ERROR")
                    .and_then(|err| {
                        for i in 0..err.child_count() {
                            if let Some(c) = err.child(i) {
                                if c.kind() == "list_expression" {
                                    return Some(c);
                                }
                            }
                        }
                        None
                    });
                return Some((name.to_string(), true, invocant, args_from_error));
            }
            "ERROR" => {
                if let Some(result) = parse_call_from_error(node, source, cursor) {
                    return Some(result);
                }
                if let Some(result) = find_call_from_error_sibling(node, source, cursor) {
                    return Some(result);
                }
            }
            _ => {}
        }
        node = node.parent()?;
    }
    None
}

/// When an ERROR node contains just `(` and optional args, the actual call expression
/// may be in a preceding sibling.
fn find_call_from_error_sibling<'a>(
    error: Node<'a>,
    source: &'a [u8],
    _point: Point,
) -> Option<(String, bool, Option<String>, Option<Node<'a>>)> {
    let mut args_node: Option<Node<'a>> = None;
    for i in 0..error.child_count() {
        if let Some(child) = error.child(i) {
            if child.kind() == "list_expression" {
                args_node = Some(child);
            }
        }
    }

    let prev = error.prev_sibling()?;
    let call_node = if prev.kind() == "expression_statement" {
        prev.named_child(0)?
    } else {
        prev
    };

    match call_node.kind() {
        "method_call_expression" => {
            let name = call_node
                .child_by_field_name("method")
                .and_then(|n| n.utf8_text(source).ok())?;
            let invocant = call_node
                .child_by_field_name("invocant")
                .and_then(|n| n.utf8_text(source).ok())
                .map(|s| s.to_string());
            let args = call_node.child_by_field_name("arguments").or(args_node);
            Some((name.to_string(), true, invocant, args))
        }
        "function_call_expression" | "ambiguous_function_call_expression" => {
            let name = call_node
                .child_by_field_name("function")
                .and_then(|n| n.utf8_text(source).ok())?;
            let args = call_node.child_by_field_name("arguments").or(args_node);
            Some((name.to_string(), false, None, args))
        }
        "bareword" => {
            let name = call_node.utf8_text(source).ok()?;
            Some((name.to_string(), false, None, args_node))
        }
        _ => None,
    }
}

/// Parse a call expression from an ERROR node.
fn parse_call_from_error<'a>(
    error: Node<'a>,
    source: &'a [u8],
    _cursor: Point,
) -> Option<(String, bool, Option<String>, Option<Node<'a>>)> {
    let mut func_name: Option<String> = None;
    let mut method_name: Option<String> = None;
    let mut invocant: Option<String> = None;
    let mut has_arrow = false;
    let mut found_open_paren = false;
    let mut args_node: Option<Node<'a>> = None;

    for i in 0..error.child_count() {
        let child = error.child(i)?;

        if matches!(child.kind(), "scalar" | "array" | "hash" | "package") {
            invocant = child.utf8_text(source).ok().map(|s| s.to_string());
        } else if child.kind() == "->" {
            has_arrow = true;
            if invocant.is_none() {
                invocant = func_name.take();
            }
        } else if child.kind() == "method" && has_arrow {
            method_name = child.utf8_text(source).ok().map(|s| s.to_string());
        } else if child.kind() == "bareword" || child.kind() == "function" {
            func_name = child.utf8_text(source).ok().map(|s| s.to_string());
        } else if child.kind() == "(" {
            found_open_paren = true;
        } else if child.kind() == "list_expression" && found_open_paren {
            args_node = Some(child);
        }
    }

    if !found_open_paren {
        return None;
    }

    if let Some(name) = method_name {
        Some((name, true, invocant, args_node))
    } else if let Some(name) = func_name {
        Some((name, false, None, args_node))
    } else {
        // Recover function name from source text before `(`
        let paren_byte = (0..error.child_count())
            .filter_map(|i| error.child(i))
            .find(|c| c.kind() == "(")
            .map(|c| c.start_byte());
        if let Some(pb) = paren_byte {
            let start = error.start_byte();
            if pb > start {
                let text = std::str::from_utf8(&source[start..pb]).unwrap_or("").trim();
                if !text.is_empty()
                    && text
                        .bytes()
                        .all(|b| b.is_ascii_alphanumeric() || b == b'_')
                {
                    return Some((text.to_string(), false, None, args_node));
                }
            }
        }
        None
    }
}

// ---- Counting helpers ----

/// Index of the positional-argument slot the cursor is sitting in,
/// scoped to a single call-args / container node. Tree-based: counts
/// the NAMED children (which are the positional arguments) whose end
/// lies at-or-before the cursor. Counts fat-commas and literal commas
/// the same way, because they're both just separators between named
/// children in the parser — there's no text matching here.
///
///   `enqueue('t', [a], {})` cursor inside `[a]` → 1 named child
///     (the string_literal `'t'`) has ended before cursor → slot 1.
///
///   `enqueue(t => [a], {})` cursor inside `[a]` → 1 named child
///     (the autoquoted_bareword `t`) has ended → slot 1. Same answer.
pub fn active_slot_in_node(node: Node, cursor: Point) -> usize {
    let scope = item_scope(node);
    // Only `list_expression` carries comma-separated slots. Single-arg
    // calls like `url_for('Users#list')` pass the argument NODE
    // directly as `arguments` (per the tree-sitter-perl CLAUDE.md
    // note), so `scope` is the `string_literal` itself — iterating
    // its named children counts INTERNAL structure (the lone
    // `string_content`) and double-trips the "past this slot"
    // boundary when the cursor reaches the content's end. That makes
    // dispatch completion flip to `active_param=1` at the exact
    // closing-quote cursor position the user lands on with `i` to
    // edit inside the string.
    if scope.kind() != "list_expression" {
        return 0;
    }
    let mut count = 0;
    let c_pos = (cursor.row, cursor.column);
    let mut walker = scope.walk();
    for child in scope.named_children(&mut walker) {
        let e = child.end_position();
        if (e.row, e.column) <= c_pos {
            // Child has ended at-or-before the cursor — cursor is
            // past this slot.
            count += 1;
        } else {
            // Cursor is inside this child (or before it). Either way,
            // this is the slot the cursor is currently in; stop.
            break;
        }
    }
    count
}

/// tree-sitter-perl wraps multi-item container contents in an inner
/// `list_expression`, so `[a, b, c]` parses as an
/// `anonymous_array_expression` whose only named child is a
/// `list_expression` holding the items. Callers want to iterate the
/// items directly — descend through that single-child wrapper.
fn item_scope(node: Node) -> Node {
    let mut walker = node.walk();
    let named: Vec<_> = node.named_children(&mut walker).collect();
    if named.len() == 1 && named[0].kind() == "list_expression" {
        return named[0];
    }
    node
}

/// Count both `,` and `=>` separators before cursor at top-level depth.
/// Even count = key position, odd count = value position.
fn count_separators_before(node: Node, cursor: Point) -> usize {
    let mut count = 0;
    let mut depth = 0;
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if child.start_position() >= cursor {
                break;
            }
            match child.kind() {
                "(" | "[" | "{" => depth += 1,
                ")" | "]" | "}" => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                "," | "=>" if depth == 0 => count += 1,
                _ => {}
            }
        }
    }
    count
}

/// Collect keys already used at the call site (barewords/strings before `=>`).
/// Collect keys already present in the innermost hash literal at
/// the cursor position. Returns an empty set when the cursor isn't
/// inside one. Completion uses this to avoid re-offering a key the
/// user has already written.
pub fn used_keys_in_enclosing_hash(tree: &Tree, source: &[u8], point: Point) -> HashSet<String> {
    let check_point = if point.column > 0 {
        Point::new(point.row, point.column - 1)
    } else {
        point
    };
    let Some(mut node) = tree.root_node().descendant_for_point_range(check_point, check_point)
        else { return HashSet::new() };
    for _ in 0..16 {
        if node.kind() == "anonymous_hash_expression" {
            // Child layout varies: sometimes the pairs are direct
            // children of the hash node (`{ x => 1 }`); sometimes
            // they're wrapped in a list_expression (`{ x => 1, y
            // => 2 }`). Probe both — the pair-detector is the same
            // shape regardless.
            let mut used = collect_used_keys_at_callsite(node, source);
            for i in 0..node.named_child_count() {
                if let Some(inner) = node.named_child(i) {
                    if inner.kind() == "list_expression" {
                        used.extend(collect_used_keys_at_callsite(inner, source));
                    }
                }
            }
            return used;
        }
        let Some(parent) = node.parent() else { break };
        node = parent;
    }
    HashSet::new()
}

fn collect_used_keys_at_callsite(args: Node, source: &[u8]) -> HashSet<String> {
    let mut used = HashSet::new();
    for i in 0..args.child_count() {
        if let Some(child) = args.child(i) {
            if let Some(next) = child.next_sibling() {
                if next.kind() == "=>" {
                    if let Some(key) = extract_key_text(child, source) {
                        used.insert(key);
                    }
                }
            }
        }
    }
    used
}

/// Extract the text of a key node (bareword, string literal, etc.)
fn extract_key_text(node: Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "autoquoted_bareword" => node.utf8_text(source).ok().map(|s| s.to_string()),
        "string_literal" | "interpolated_string_literal" => {
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    if child.kind() == "string_content" {
                        return child.utf8_text(source).ok().map(|s| s.to_string());
                    }
                }
            }
            Some(String::new())
        }
        "string_content" => node.utf8_text(source).ok().map(|s| s.to_string()),
        _ => None, // dynamic keys — don't include
    }
}

// ---- Plugin query-hook context ----

/// Build the `SigHelpQueryContext` / `CompletionQueryContext` a plugin
/// query hook consumes. Extracts the innermost call at the cursor, the
/// innermost nested container (array/hash), and the enclosing package.
/// Returns `None` when no enclosing call exists and no plugin could
/// plausibly claim the slot.
///
/// Lives in cursor_context.rs (rule #6 — this is cursor-position
/// analysis) so the plugin hook wiring in symbols.rs stays an adapter
/// and doesn't walk the tree directly (rule #3).
pub fn build_plugin_query_context(
    analysis: &FileAnalysis,
    tree: &Tree,
    source: &[u8],
    point: Point,
) -> Option<crate::plugin::SigHelpQueryContext> {
    use crate::plugin::{CallFrame, ContainerFrame, ContainerKind, SigHelpQueryContext};

    let call_ctx = find_call_context(tree, source, point);
    let call = call_ctx.as_ref().map(|c| {
        // Minimal arg list: just the first positional as a folded string
        // when available. Dispatcher plugins (Minion, mojo-events) need
        // the task / event name to look up the right Handler. We don't
        // walk the full arg list here — that would duplicate the
        // builder's extraction at cursor time, and plugins that need
        // more can request richer context via a future extension.
        let args = match c.first_arg_string.as_ref() {
            Some(s) => vec![crate::plugin::ArgInfo {
                text: s.clone(),
                string_value: Some(s.clone()),
                span: crate::file_analysis::Span {
                    start: point,
                    end: point,
                },
                content_span: None,
                inferred_type: Some(InferredType::String),
                sub_params: Vec::new(),
            }],
            None => Vec::new(),
        };
        CallFrame {
            is_method: c.is_method,
            name: c.name.clone(),
            receiver_text: c.invocant.clone(),
            receiver_type: c.invocant.as_deref()
                .and_then(|inv| analysis.invocant_text_to_class(Some(inv), point))
                .map(InferredType::ClassName),
            args,
            cursor_arg_index: c.active_param,
        }
    });

    // Innermost nested container: walk up from cursor to find the
    // first anonymous_array_expression or anonymous_hash_expression
    // before we exit the enclosing call. Returned alongside an
    // optional `use M ...` module name when the matched container
    // is the args of a use statement — lets plugins claim use-line
    // option-key completion (`use DDP { caller_info => 1 }`) without
    // the core hard-coding any specific module.
    let (container, current_use_module) = {
        let mut n = tree.root_node().descendant_for_point_range(point, point);
        let mut found: Option<ContainerFrame> = None;
        let mut found_node: Option<Node> = None;
        while let Some(node) = n {
            match node.kind() {
                "anonymous_array_expression" => {
                    found = Some(ContainerFrame {
                        kind: ContainerKind::Array,
                        active_slot: active_slot_in_node(node, point),
                        existing_keys: Vec::new(),
                    });
                    found_node = Some(node);
                    break;
                }
                "anonymous_hash_expression" => {
                    let used = used_keys_in_enclosing_hash(tree, source, point);
                    found = Some(ContainerFrame {
                        kind: ContainerKind::Hash,
                        active_slot: active_slot_in_node(node, point),
                        existing_keys: used.into_iter().collect(),
                    });
                    found_node = Some(node);
                    break;
                }
                "method_call_expression" | "function_call_expression"
                | "ambiguous_function_call_expression" => break,
                _ => {}
            }
            n = node.parent();
        }
        // If the matched container is a direct child of a use_statement,
        // surface the module name. Walking only one level up is correct:
        // the parser shape is `use_statement → anonymous_hash_expression`
        // (no list_expression intermediary at the args level for the
        // hash form). Bail on anything more nested — that's not a
        // use-line args slot.
        let use_module = found_node.and_then(|hash| {
            let parent = hash.parent()?;
            if parent.kind() != "use_statement" { return None; }
            let module = parent.child_by_field_name("module")?;
            module.utf8_text(source).ok().map(|s| s.to_string())
        });
        (found, use_module)
    };

    let current_package = analysis.package_at(point).map(|s| s.to_string());

    if call.is_none() && container.is_none() { return None; }
    Some(SigHelpQueryContext {
        call,
        cursor_inside: container,
        current_package,
        current_use_module,
    })
}

// ---- Selection ranges (tree-based) ----

/// Build selection range spans from innermost to outermost node.
pub fn selection_ranges(tree: &Tree, point: Point) -> Vec<Span> {
    let mut node = match tree
        .root_node()
        .named_descendant_for_point_range(point, point)
    {
        Some(n) => n,
        None => return vec![],
    };

    let mut ranges = vec![node_to_span(node)];
    while let Some(parent) = node.parent() {
        ranges.push(node_to_span(parent));
        node = parent;
    }
    ranges
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_variable_context() {
        let source = "my $x = $";
        let ctx = detect_cursor_context(source, Point::new(0, 9), None);
        assert_eq!(ctx, CursorContext::Variable { sigil: '$' });
    }

    #[test]
    fn test_detect_method_context() {
        let source = "my $obj = Foo->new; $obj->";
        // Column 26 = after the `>` in `->`
        let ctx = detect_cursor_context(source, Point::new(0, 26), None);
        assert_eq!(
            ctx,
            CursorContext::Method {
                invocant_type: None,
                invocant_text: "$obj".to_string(),
            }
        );
    }

    #[test]
    fn test_detect_method_context_mid_word() {
        // Typing `$p->mag` should still be Method context
        let source = "$p->mag";
        let ctx = detect_cursor_context(source, Point::new(0, 7), None);
        assert_eq!(
            ctx,
            CursorContext::Method {
                invocant_type: None,
                invocant_text: "$p".to_string(),
            }
        );
    }

    #[test]
    fn test_detect_method_context_class_mid_word() {
        let source = "Foo->ne";
        let ctx = detect_cursor_context(source, Point::new(0, 7), None);
        assert_eq!(
            ctx,
            CursorContext::Method {
                invocant_type: Some(InferredType::ClassName("Foo".to_string())),
                invocant_text: "Foo".to_string(),
            }
        );
    }

    #[test]
    fn test_detect_hashkey_arrow() {
        let source = "$self->{";
        let ctx = detect_cursor_context(source, Point::new(0, 8), None);
        assert_eq!(
            ctx,
            CursorContext::HashKey {
                owner_type: None,
                var_text: "$self".to_string(),
                source_sub: None,
            }
        );
    }

    #[test]
    fn test_detect_hashkey_arrow_midword() {
        let source = "$self->{ho";
        let ctx = detect_cursor_context(source, Point::new(0, 10), None);
        assert_eq!(
            ctx,
            CursorContext::HashKey {
                owner_type: None,
                var_text: "$self".to_string(),
                source_sub: None,
            }
        );
    }

    #[test]
    fn test_detect_hashkey_direct_midword() {
        let source = "$hash{ver";
        let ctx = detect_cursor_context(source, Point::new(0, 9), None);
        assert_eq!(
            ctx,
            CursorContext::HashKey {
                owner_type: None,
                var_text: "$hash".to_string(),
                source_sub: None,
            }
        );
    }

    #[test]
    fn test_detect_hashkey_direct() {
        let source = "$hash{";
        let ctx = detect_cursor_context(source, Point::new(0, 6), None);
        assert_eq!(
            ctx,
            CursorContext::HashKey {
                owner_type: None,
                var_text: "$hash".to_string(),
                source_sub: None,
            }
        );
    }

    #[test]
    fn test_detect_general() {
        let source = "my $x = foo";
        let ctx = detect_cursor_context(source, Point::new(0, 11), None);
        assert_eq!(ctx, CursorContext::General);
    }

    #[test]
    fn test_detect_class_method_context() {
        let source = "Calculator->";
        let ctx = detect_cursor_context(source, Point::new(0, 12), None);
        assert_eq!(
            ctx,
            CursorContext::Method {
                invocant_type: Some(InferredType::ClassName("Calculator".to_string())),
                invocant_text: "Calculator".to_string(),
            }
        );
    }

    fn parse(source: &str) -> Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&ts_parser_perl::LANGUAGE.into())
            .unwrap();
        parser.parse(source, None).unwrap()
    }

    #[test]
    fn test_find_call_context_function() {
        let source = "sub greet { }\ngreet(";
        let tree = parse(source);
        let ctx =
            find_call_context(&tree, source.as_bytes(), Point::new(1, 6)).unwrap();
        assert_eq!(ctx.name, "greet");
        assert!(!ctx.is_method);
        assert_eq!(ctx.active_param, 0);
    }

    #[test]
    fn test_find_call_context_with_args() {
        let source = "sub foo { }\nfoo(1, 2, ";
        let tree = parse(source);
        let ctx =
            find_call_context(&tree, source.as_bytes(), Point::new(1, 10)).unwrap();
        assert_eq!(ctx.name, "foo");
        assert_eq!(ctx.active_param, 2);
    }

    /// Regression: cursor inside a single-arg call's string literal
    /// (e.g. `url_for('Users#list')` with cursor on the `s` of `Users`)
    /// must report `active_param = 0`. Before the fix, `active_slot_in_node`
    /// iterated the string_literal's INTERNAL children (the lone
    /// `string_content`) and counted it as a slot once the cursor
    /// reached its end boundary — making dispatch completion die at
    /// exactly the boundary where users expect it to narrow by the
    /// typed content. Live symptom: in nvim, typing inside the
    /// quotes randomly flipped the completion on/off (vs the stable
    /// `active_param = 0` needed to surface handlers).
    #[test]
    fn test_call_context_cursor_inside_single_string_arg() {
        let source = "$c->url_for('Users#list');";
        let tree = parse(source);
        for col in 12..=23 {
            let ctx =
                find_call_context(&tree, source.as_bytes(), Point::new(0, col)).unwrap();
            assert_eq!(
                ctx.active_param, 0,
                "cursor at col {} inside `url_for('Users#list')` must be active_param=0; \
                 got {}",
                col, ctx.active_param,
            );
        }
    }

    #[test]
    fn test_call_context_key_position() {
        // Complete call so tree-sitter can parse it
        let source = "sub foo { }\nfoo(host => 'x', port => 8080);";
        let tree = parse(source);
        // Cursor at the `p` of `port` (column 17) — at key position after first `, `
        let ctx =
            find_call_context(&tree, source.as_bytes(), Point::new(1, 17)).unwrap();
        assert!(ctx.at_key_position);
        assert!(ctx.used_keys.contains("host"));
    }

    #[test]
    fn test_selection_ranges_basic() {
        let source = "my $x = 1;";
        let tree = parse(source);
        let ranges = selection_ranges(&tree, Point::new(0, 3));
        assert!(!ranges.is_empty());
        // Innermost should be the variable node
        assert!(ranges.len() >= 2);
    }

    fn build_fa(source: &str) -> (Tree, crate::file_analysis::FileAnalysis) {
        let tree = parse(source);
        let fa = crate::builder::build(&tree, source.as_bytes());
        (tree, fa)
    }

    #[test]
    fn test_tree_context_method_on_function_call() {
        // get_config()-> should detect Method with HashRef type
        let source = "sub get_config {\n    return { host => 1 };\n}\nget_config()->";
        let (tree, fa) = build_fa(source);
        // Cursor at end of line 3 (after "->")
        let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(3, 14), &fa);
        assert!(
            matches!(ctx, Some(CursorContext::Method { ref invocant_type, .. }) if *invocant_type == Some(InferredType::HashRef)),
            "expected Method with HashRef type, got {:?}", ctx,
        );
    }

    #[test]
    fn test_tree_context_method_on_chained_call() {
        // $f->get_bar()-> where get_bar returns Object(Bar)
        let source = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\npackage main;\nmy $f = Foo->new();\n$f->get_bar()->";
        let (tree, fa) = build_fa(source);
        // Line 9: $f->get_bar()->   cursor at end
        let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(9, 15), &fa);
        assert!(
            matches!(ctx, Some(CursorContext::Method { ref invocant_type, .. })
                if invocant_type.as_ref().and_then(|t| t.class_name()) == Some("Bar")),
            "expected Method with Object(Bar) type, got {:?}", ctx,
        );
    }

    #[test]
    fn test_tree_context_hashkey_on_chained_call() {
        // $calc->get_self->get_config->{ should detect HashKey with resolved type
        let source = "\
package Calculator;
sub new { bless {}, shift }
sub get_self {
    my ($self) = @_;
    return $self;
}
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{";
        let (tree, fa) = build_fa(source);
        // Last line: "$calc->get_self->get_config->{"
        let cursor = Point::new(11, 30); // after "{"
        let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), cursor, &fa);
        assert!(
            matches!(ctx, Some(CursorContext::HashKey { ref owner_type, ref source_sub, .. })
                if *owner_type == Some(InferredType::HashRef) && *source_sub == Some("get_config".to_string())),
            "expected HashKey with HashRef type and get_config source, got {:?}", ctx,
        );
    }

    #[test]
    fn test_tree_context_simple_var_method() {
        // $obj-> with nothing after: tree-based detection resolves the type
        let source = "my $obj = Foo->new();\n$obj->";
        let (tree, fa) = build_fa(source);
        let ctx = detect_cursor_context_tree(&tree, source.as_bytes(), Point::new(1, 6), &fa);
        assert_eq!(
            ctx,
            Some(CursorContext::Method {
                invocant_type: Some(InferredType::ClassName("Foo".to_string())),
                invocant_text: "$obj".to_string(),
            })
        );

        // Text-based fallback also resolves the type
        let ctx = detect_cursor_context(source, Point::new(1, 6), Some(&fa));
        assert_eq!(
            ctx,
            CursorContext::Method {
                invocant_type: Some(InferredType::ClassName("Foo".to_string())),
                invocant_text: "$obj".to_string(),
            }
        );
    }

    #[test]
    fn test_use_context_module_prefix() {
        let source = "use Mojo::Ba";
        let ctx = detect_cursor_context(source, Point::new(0, 12), None);
        assert_eq!(ctx, CursorContext::UseStatement {
            module_prefix: "Mojo::Ba".to_string(),
            in_import_list: false,
            module_name: None,
        });
    }

    #[test]
    fn test_use_context_empty_prefix() {
        let source = "use ";
        let ctx = detect_cursor_context(source, Point::new(0, 4), None);
        assert_eq!(ctx, CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: false,
            module_name: None,
        });
    }

    #[test]
    fn test_use_context_import_list_qw() {
        let source = "use List::Util qw(ma";
        let ctx = detect_cursor_context(source, Point::new(0, 20), None);
        assert_eq!(ctx, CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some("List::Util".to_string()),
        });
    }

    #[test]
    fn test_use_context_import_list_bare_string() {
        let source = "use Foo 'ba";
        let ctx = detect_cursor_context(source, Point::new(0, 11), None);
        assert_eq!(ctx, CursorContext::UseStatement {
            module_prefix: String::new(),
            in_import_list: true,
            module_name: Some("Foo".to_string()),
        });
    }

    #[test]
    fn test_use_context_skips_pragmas() {
        let source = "use strict";
        let ctx = detect_cursor_context(source, Point::new(0, 10), None);
        assert_eq!(ctx, CursorContext::General);
    }

    #[test]
    fn test_require_context_module_prefix() {
        let source = "require DBI";
        let ctx = detect_cursor_context(source, Point::new(0, 11), None);
        assert_eq!(ctx, CursorContext::UseStatement {
            module_prefix: "DBI".to_string(),
            in_import_list: false,
            module_name: None,
        });
    }
}
