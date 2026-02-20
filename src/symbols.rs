use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Point, Tree};

// ---- Coordinate conversion ----

fn ts_point_to_position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn position_to_ts_point(pos: Position) -> Point {
    Point::new(pos.line as usize, pos.character as usize)
}

fn node_to_range(node: Node) -> Range {
    Range {
        start: ts_point_to_position(node.start_position()),
        end: ts_point_to_position(node.end_position()),
    }
}

// ---- Symbol at cursor ----

enum CursorSymbol {
    Variable { text: String },
    Function { name: String },
    Package { name: String },
}

fn symbol_at_cursor(tree: &Tree, source: &[u8], pos: Position) -> Option<CursorSymbol> {
    let point = position_to_ts_point(pos);
    let mut node = tree
        .root_node()
        .named_descendant_for_point_range(point, point)?;

    for _ in 0..10 {
        match node.kind() {
            "scalar" | "array" | "hash" => {
                let text = node.utf8_text(source).ok()?;
                return Some(CursorSymbol::Variable {
                    text: text.to_string(),
                });
            }
            // TSP uses container_variable for hash/array element access bases:
            // $hash{key} → container_variable in hash: field → canonical is %hash
            // $arr[0]    → container_variable in array: field → canonical is @arr
            "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
                let canonical = canonical_var_name(node, source)?;
                return Some(CursorSymbol::Variable { text: canonical });
            }
            "function" | "method" => {
                let text = node.utf8_text(source).ok()?;
                return Some(CursorSymbol::Function {
                    name: text.to_string(),
                });
            }
            "package" => {
                let text = node.utf8_text(source).ok()?;
                return Some(CursorSymbol::Package {
                    name: text.to_string(),
                });
            }
            "bareword" => {
                let text = node.utf8_text(source).ok()?;
                if let Some(parent) = node.parent() {
                    if parent.kind() == "package_statement"
                        || parent.kind() == "class_statement"
                        || parent.kind() == "use_statement"
                    {
                        return Some(CursorSymbol::Package {
                            name: text.to_string(),
                        });
                    }
                }
                return Some(CursorSymbol::Function {
                    name: text.to_string(),
                });
            }
            "varname" => {
                node = node.parent()?;
                continue;
            }
            _ => {
                node = node.parent()?;
                continue;
            }
        }
    }
    None
}

// ---- Document symbols ----

#[allow(deprecated)]
pub fn extract_symbols(tree: &Tree, source: &str) -> Vec<DocumentSymbol> {
    let source_bytes = source.as_bytes();
    let mut symbols = Vec::new();
    collect_symbols(tree.root_node(), source_bytes, &mut symbols);
    symbols
}

#[allow(deprecated)]
fn collect_symbols(node: Node, source: &[u8], symbols: &mut Vec<DocumentSymbol>) {
    match node.kind() {
        "subroutine_declaration_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    let mut children = Vec::new();
                    if let Some(body) = node.child_by_field_name("body") {
                        collect_symbols(body, source, &mut children);
                    }
                    symbols.push(DocumentSymbol {
                        name: format!("sub {}", name),
                        detail: None,
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None,
                        range: node_to_range(node),
                        selection_range: node_to_range(name_node),
                        children: if children.is_empty() {
                            None
                        } else {
                            Some(children)
                        },
                    });
                    return;
                }
            }
        }
        "method_declaration_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    let mut children = Vec::new();
                    if let Some(body) = node.child_by_field_name("body") {
                        collect_symbols(body, source, &mut children);
                    }
                    symbols.push(DocumentSymbol {
                        name: format!("method {}", name),
                        detail: None,
                        kind: SymbolKind::METHOD,
                        tags: None,
                        deprecated: None,
                        range: node_to_range(node),
                        selection_range: node_to_range(name_node),
                        children: if children.is_empty() {
                            None
                        } else {
                            Some(children)
                        },
                    });
                    return;
                }
            }
        }
        "package_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    symbols.push(DocumentSymbol {
                        name: name.to_string(),
                        detail: Some("package".to_string()),
                        kind: SymbolKind::NAMESPACE,
                        tags: None,
                        deprecated: None,
                        range: node_to_range(node),
                        selection_range: node_to_range(name_node),
                        children: None,
                    });
                }
            }
        }
        "class_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    symbols.push(DocumentSymbol {
                        name: name.to_string(),
                        detail: Some("class".to_string()),
                        kind: SymbolKind::CLASS,
                        tags: None,
                        deprecated: None,
                        range: node_to_range(node),
                        selection_range: node_to_range(name_node),
                        children: None,
                    });
                }
            }
        }
        "variable_declaration" => {
            let keyword = get_decl_keyword(node, source).unwrap_or("my");
            let mut vars = Vec::new();
            collect_declared_vars(node, source, &mut vars);
            for (name, sel_node) in vars {
                symbols.push(DocumentSymbol {
                    name,
                    detail: Some(keyword.to_string()),
                    kind: SymbolKind::VARIABLE,
                    tags: None,
                    deprecated: None,
                    range: node_to_range(node),
                    selection_range: node_to_range(sel_node),
                    children: None,
                });
            }
            return;
        }
        "use_statement" => {
            if let Some(module_node) = node.child_by_field_name("module") {
                if let Ok(name) = module_node.utf8_text(source) {
                    symbols.push(DocumentSymbol {
                        name: format!("use {}", name),
                        detail: None,
                        kind: SymbolKind::MODULE,
                        tags: None,
                        deprecated: None,
                        range: node_to_range(node),
                        selection_range: node_to_range(module_node),
                        children: None,
                    });
                }
            }
            return;
        }
        _ => {}
    }

    // Recurse into children
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            collect_symbols(child, source, symbols);
        }
    }
}

fn get_decl_keyword<'a>(decl: Node<'a>, source: &'a [u8]) -> Option<&'a str> {
    let first = decl.child(0)?;
    first.utf8_text(source).ok()
}

/// Walk a variable_declaration subtree and collect all declared variable
/// names (scalar/array/hash). Handles both single vars and paren lists,
/// without relying on field names that break on hidden rules.
fn collect_declared_vars<'a>(
    node: Node<'a>,
    source: &'a [u8],
    out: &mut Vec<(String, Node<'a>)>,
) {
    if matches!(node.kind(), "scalar" | "array" | "hash") {
        if let Ok(name) = node.utf8_text(source) {
            out.push((name.to_string(), node));
        }
        return;
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_declared_vars(child, source, out);
        }
    }
}

// ---- Go to definition ----

pub fn find_definition(
    tree: &Tree,
    source: &str,
    pos: Position,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, pos)?;

    match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            let target_range = find_variable_def(tree.root_node(), source_bytes, text, pos)?;
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: target_range,
            }))
        }
        CursorSymbol::Function { ref name } => {
            let (_, sel_range) = find_sub_def(tree.root_node(), source_bytes, name)?;
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: sel_range,
            }))
        }
        CursorSymbol::Package { .. } => None,
    }
}

fn find_variable_def(
    root: Node,
    source: &[u8],
    target_text: &str,
    before: Position,
) -> Option<Range> {
    let before_point = position_to_ts_point(before);
    let mut best: Option<(Range, usize)> = None;
    find_variable_def_walk(root, source, target_text, before_point, &mut best);
    best.map(|(range, _)| range)
}

fn find_variable_def_walk(
    node: Node,
    source: &[u8],
    target_text: &str,
    before_point: Point,
    best: &mut Option<(Range, usize)>,
) {
    if node.kind() == "variable_declaration" {
        if node.start_position() <= before_point {
            if let Some(var_node) = decl_contains_variable(node, source, target_text) {
                let scope = enclosing_scope(node);
                let scope_end = scope.end_position();

                if scope_end >= before_point {
                    let scope_size = scope_end.row - scope.start_position().row;
                    let should_replace = match best {
                        Some((_, prev_size)) => scope_size <= *prev_size,
                        None => true,
                    };
                    if should_replace {
                        *best = Some((node_to_range(var_node), scope_size));
                    }
                }
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            find_variable_def_walk(child, source, target_text, before_point, best);
        }
    }
}

fn decl_contains_variable<'a>(
    decl: Node<'a>,
    source: &[u8],
    target: &str,
) -> Option<Node<'a>> {
    // Walk all descendants — variable_declaration only contains declared
    // variables (the RHS of any assignment is in the parent node).
    // This handles both single vars (my $x) and lists (my ($x, %h))
    // without relying on field names that break on hidden rules.
    find_var_in_subtree(decl, source, target)
}

fn find_var_in_subtree<'a>(
    node: Node<'a>,
    source: &[u8],
    target: &str,
) -> Option<Node<'a>> {
    if matches!(node.kind(), "scalar" | "array" | "hash")
        && node.utf8_text(source).ok() == Some(target)
    {
        return Some(node);
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(found) = find_var_in_subtree(child, source, target) {
                return Some(found);
            }
        }
    }
    None
}

fn enclosing_scope(node: Node) -> Node {
    let mut current = node;
    while let Some(parent) = current.parent() {
        if parent.kind() == "block" || parent.kind() == "source_file" {
            return parent;
        }
        current = parent;
    }
    current
}

fn find_sub_def(root: Node, source: &[u8], name: &str) -> Option<(Range, Range)> {
    find_sub_def_walk(root, source, name)
}

fn find_sub_def_walk(node: Node, source: &[u8], name: &str) -> Option<(Range, Range)> {
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(name_node) = node.child_by_field_name("name") {
            if name_node.utf8_text(source).ok() == Some(name) {
                return Some((node_to_range(node), node_to_range(name_node)));
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(result) = find_sub_def_walk(child, source, name) {
                return Some(result);
            }
        }
    }
    None
}

// ---- Find references ----

pub fn find_references(tree: &Tree, source: &str, pos: Position, uri: &Url) -> Vec<Location> {
    let source_bytes = source.as_bytes();
    let cursor_sym = match symbol_at_cursor(tree, source_bytes, pos) {
        Some(s) => s,
        None => return vec![],
    };

    let mut ranges = Vec::new();

    match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            // Scope-aware: resolve which declaration this reference points to,
            // then only include references that resolve to the same declaration.
            let target_def = find_variable_def(tree.root_node(), source_bytes, text, pos);

            let mut all_refs = Vec::new();
            collect_variable_refs(tree.root_node(), source_bytes, text, &mut all_refs);

            for ref_range in all_refs {
                let ref_def = find_variable_def(
                    tree.root_node(),
                    source_bytes,
                    text,
                    ref_range.start,
                );
                if ref_def == target_def {
                    ranges.push(ref_range);
                }
            }
        }
        CursorSymbol::Function { ref name } => {
            collect_function_refs(tree.root_node(), source_bytes, name, &mut ranges);
        }
        CursorSymbol::Package { ref name } => {
            collect_package_refs(tree.root_node(), source_bytes, name, &mut ranges);
        }
    }

    ranges
        .into_iter()
        .map(|range| Location {
            uri: uri.clone(),
            range,
        })
        .collect()
}

fn collect_variable_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Range>) {
    match node.kind() {
        "scalar" | "array" | "hash" => {
            if node.utf8_text(source).ok() == Some(target) {
                refs.push(node_to_range(node));
            }
            return;
        }
        "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
            if canonical_var_name(node, source).as_deref() == Some(target) {
                refs.push(node_to_range(node));
            }
            return;
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_variable_refs(child, source, target, refs);
        }
    }
}

/// Resolve a container_variable to its canonical sigil+name.
/// TSP already distinguishes the context via parent field names:
///   hash: field → %name, array: field → @name
fn canonical_var_name(node: Node, source: &[u8]) -> Option<String> {
    let varname = get_varname(node, source)?;

    match node.kind() {
        "keyval_container_variable" => return Some(format!("%{}", varname)),
        "slice_container_variable" => {
            // @hash{@keys} or @array[@idxs] — check parent field
            if let Some(parent) = node.parent() {
                if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
                    return Some(format!("%{}", varname));
                }
            }
            return Some(format!("@{}", varname));
        }
        _ => {} // container_variable — check parent field below
    }

    if let Some(parent) = node.parent() {
        if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
            return Some(format!("%{}", varname));
        }
        if parent.child_by_field_name("array").map_or(false, |a| a.id() == node.id()) {
            return Some(format!("@{}", varname));
        }
    }
    // Fallback to literal text
    node.utf8_text(source).ok().map(|s| s.to_string())
}

fn get_varname<'a>(node: Node<'a>, source: &'a [u8]) -> Option<&'a str> {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "varname" {
                return child.utf8_text(source).ok();
            }
        }
    }
    None
}

fn collect_function_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Range>) {
    match node.kind() {
        "subroutine_declaration_statement" | "method_declaration_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if name_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_range(name_node));
                }
            }
        }
        "function_call_expression" => {
            if let Some(func_node) = node.child_by_field_name("function") {
                if func_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_range(func_node));
                }
            }
        }
        "method_call_expression" => {
            if let Some(method_node) = node.child_by_field_name("method") {
                if method_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_range(method_node));
                }
            }
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_function_refs(child, source, target, refs);
        }
    }
}

fn collect_package_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Range>) {
    if node.kind() == "package" && node.utf8_text(source).ok() == Some(target) {
        refs.push(node_to_range(node));
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_package_refs(child, source, target, refs);
        }
    }
}

// ---- Rename ----

pub fn rename(
    tree: &Tree,
    source: &str,
    pos: Position,
    uri: &Url,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, pos)?;

    // For variables, the new name from the editor won't include the sigil,
    // but our references include it. Preserve the original sigil.
    let refs = find_references(tree, source, pos, uri);
    if refs.is_empty() {
        return None;
    }

    let edits: Vec<TextEdit> = refs
        .into_iter()
        .map(|loc| {
            let new_text = match &cursor_sym {
                CursorSymbol::Variable { text } => {
                    // Keep the sigil from the original, replace the name part.
                    let sigil = &text[..1]; // $, @, or %
                    let replacement = if new_name.starts_with(|c| "$@%".contains(c)) {
                        new_name.to_string()
                    } else {
                        format!("{}{}", sigil, new_name)
                    };
                    replacement
                }
                _ => new_name.to_string(),
            };
            TextEdit {
                range: loc.range,
                new_text,
            }
        })
        .collect();

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

// ---- Hover ----

pub fn hover_info(tree: &Tree, source: &str, pos: Position) -> Option<Hover> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, pos)?;

    let content = match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            if let Some(def_range) = find_variable_def(tree.root_node(), source_bytes, text, pos) {
                let line_start = def_range.start.line as usize;
                let line = source.lines().nth(line_start).unwrap_or("");
                format!("```perl\n{}\n```", line.trim())
            } else {
                format!("`{}`", text)
            }
        }
        CursorSymbol::Function { ref name } => {
            if let Some((full_range, _)) = find_sub_def(tree.root_node(), source_bytes, name) {
                let line_start = full_range.start.line as usize;
                let line = source.lines().nth(line_start).unwrap_or("");
                format!("```perl\n{}\n```", line.trim())
            } else {
                format!("`{}`", name)
            }
        }
        CursorSymbol::Package { ref name } => {
            format!("**package** `{}`", name)
        }
    };

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}
