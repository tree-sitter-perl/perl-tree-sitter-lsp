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
                return;
            }
            // Don't return — dereference expressions like @{$hash{key}} parse as
            // an array node wrapping a block with container_variable inside.
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

    let refs = find_references(tree, source, pos, uri);
    if refs.is_empty() {
        return None;
    }

    // Strip any sigil the user may have typed — we only rename the varname.
    let bare_name = new_name.trim_start_matches(['$', '@', '%']);

    let edits: Vec<TextEdit> = refs
        .into_iter()
        .map(|loc| {
            match &cursor_sym {
                CursorSymbol::Variable { .. } => {
                    // Only replace the varname, not the sigil. The sigil is
                    // always the first character of the range.
                    let mut range = loc.range;
                    range.start.character += 1;
                    TextEdit {
                        range,
                        new_text: bare_name.to_string(),
                    }
                }
                _ => TextEdit {
                    range: loc.range,
                    new_text: bare_name.to_string(),
                },
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

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn parse(source: &str) -> Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_perl::LANGUAGE.into())
            .unwrap();
        parser.parse(source, None).unwrap()
    }

    fn test_uri() -> Url {
        Url::parse("file:///test.pl").unwrap()
    }

    // ---- Document symbols ----

    #[test]
    fn test_symbols_sub() {
        let src = "sub foo { 1 }";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "sub foo");
        assert_eq!(syms[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_symbols_package() {
        let src = "package Foo;";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Foo");
        assert_eq!(syms[0].kind, SymbolKind::NAMESPACE);
    }

    #[test]
    fn test_symbols_variables() {
        let src = "my $x = 1;\nour @list;";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 2);
        assert_eq!(syms[0].name, "$x");
        assert_eq!(syms[0].kind, SymbolKind::VARIABLE);
        assert_eq!(syms[0].detail.as_deref(), Some("my"));
        assert_eq!(syms[1].name, "@list");
        assert_eq!(syms[1].detail.as_deref(), Some("our"));
    }

    #[test]
    fn test_symbols_use() {
        let src = "use Carp;";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "use Carp");
        assert_eq!(syms[0].kind, SymbolKind::MODULE);
    }

    #[test]
    fn test_symbols_nested() {
        let src = "sub foo {\n    my $x = 1;\n}";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        let children = syms[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "$x");
    }

    #[test]
    fn test_symbols_paren_list() {
        let src = "sub foo {\n    my ($x, %h) = @_;\n}";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        let children = syms[0].children.as_ref().unwrap();
        let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"$x"));
        assert!(names.contains(&"%h"));
    }

    // ---- Go to definition ----

    #[test]
    fn test_def_variable() {
        let src = "my $x = 1;\nprint $x;";
        let tree = parse(src);
        let uri = test_uri();
        // cursor on $x in print at L1:6
        let resp = find_definition(&tree, src, Position::new(1, 7), &uri);
        let loc = match resp.unwrap() {
            GotoDefinitionResponse::Scalar(loc) => loc,
            _ => panic!("expected scalar"),
        };
        assert_eq!(loc.range.start.line, 0); // jumps to L0 declaration
    }

    #[test]
    fn test_def_scoped() {
        let src = "\
my $x = 1;
sub foo {
    my $x = 2;
    print $x;
}";
        let tree = parse(src);
        let uri = test_uri();
        // cursor on $x in print at L3:10
        let resp = find_definition(&tree, src, Position::new(3, 11), &uri);
        let loc = match resp.unwrap() {
            GotoDefinitionResponse::Scalar(loc) => loc,
            _ => panic!("expected scalar"),
        };
        assert_eq!(loc.range.start.line, 2); // inner my $x at L2, not L0
    }

    #[test]
    fn test_def_sub() {
        let src = "sub add { 1 }\nadd();";
        let tree = parse(src);
        let uri = test_uri();
        // cursor on add() call at L1:0
        let resp = find_definition(&tree, src, Position::new(1, 0), &uri);
        let loc = match resp.unwrap() {
            GotoDefinitionResponse::Scalar(loc) => loc,
            _ => panic!("expected scalar"),
        };
        assert_eq!(loc.range.start.line, 0);
    }

    #[test]
    fn test_def_container_variable() {
        let src = "\
sub new {
    my (%args) = @_;
    return $args{verbose};
}";
        let tree = parse(src);
        let uri = test_uri();
        // cursor on $args in $args{verbose} at L2:11
        let resp = find_definition(&tree, src, Position::new(2, 12), &uri);
        let loc = match resp.unwrap() {
            GotoDefinitionResponse::Scalar(loc) => loc,
            _ => panic!("expected scalar"),
        };
        assert_eq!(loc.range.start.line, 1); // jumps to %args decl
    }

    // ---- References ----

    #[test]
    fn test_refs_variable_scoped() {
        let src = "\
sub add {
    my $result = 1;
    return $result;
}
sub sub2 {
    my $result = 2;
    return $result;
}";
        let tree = parse(src);
        let uri = test_uri();
        // refs from $result in add (L1:8)
        let refs = find_references(&tree, src, Position::new(1, 8), &uri);
        let lines: Vec<u32> = refs.iter().map(|l| l.range.start.line).collect();
        assert!(lines.contains(&1));
        assert!(lines.contains(&2));
        assert!(!lines.contains(&5)); // sub2's $result
        assert!(!lines.contains(&6));
    }

    #[test]
    fn test_refs_container_variable() {
        let src = "\
sub foo {
    my (%opts) = @_;
    print $opts{name};
    return %opts;
}";
        let tree = parse(src);
        let uri = test_uri();
        // refs from %opts decl at L1:8
        let refs = find_references(&tree, src, Position::new(1, 9), &uri);
        let lines: Vec<u32> = refs.iter().map(|l| l.range.start.line).collect();
        assert!(lines.contains(&1)); // declaration
        assert!(lines.contains(&2)); // $opts{name}
        assert!(lines.contains(&3)); // %opts return
    }

    #[test]
    fn test_refs_deref() {
        let src = "\
sub foo {
    my (%hash) = @_;
    my @vals = @{$hash{keys}};
    return %hash;
}";
        let tree = parse(src);
        let uri = test_uri();
        let refs = find_references(&tree, src, Position::new(1, 9), &uri);
        let lines: Vec<u32> = refs.iter().map(|l| l.range.start.line).collect();
        assert!(lines.contains(&1)); // declaration
        assert!(lines.contains(&2)); // @{$hash{keys}}
        assert!(lines.contains(&3)); // %hash return
    }

    #[test]
    fn test_refs_function() {
        let src = "sub add { 1 }\nmy $x = add();";
        let tree = parse(src);
        let uri = test_uri();
        let refs = find_references(&tree, src, Position::new(0, 4), &uri);
        let lines: Vec<u32> = refs.iter().map(|l| l.range.start.line).collect();
        assert!(lines.contains(&0)); // declaration
        assert!(lines.contains(&1)); // call
    }

    #[test]
    fn test_refs_package() {
        let src = "package Foo;\nFoo->new();";
        let tree = parse(src);
        let uri = test_uri();
        let refs = find_references(&tree, src, Position::new(0, 8), &uri);
        assert!(refs.len() >= 1); // at least the declaration
    }

    // ---- Rename ----

    #[test]
    fn test_rename_variable() {
        let src = "\
sub foo {
    my $result = 1;
    return $result;
}";
        let tree = parse(src);
        let uri = test_uri();
        let edit = rename(&tree, src, Position::new(1, 8), &uri, "output").unwrap();
        let edits = &edit.changes.unwrap()[&uri];
        // All edits should replace "result" (not "$result")
        assert!(edits.iter().all(|e| e.new_text == "output"));
        // Sigil columns should be skipped (start.character should be 1 past the ref start)
        for e in edits {
            let orig_line = src.lines().nth(e.range.start.line as usize).unwrap();
            let before_edit = &orig_line[..e.range.start.character as usize];
            let last_char = before_edit.chars().last().unwrap();
            assert_eq!(last_char, '$');
        }
    }

    #[test]
    fn test_rename_strips_sigil() {
        let src = "my $x = 1;\nprint $x;";
        let tree = parse(src);
        let uri = test_uri();
        let edit = rename(&tree, src, Position::new(0, 4), &uri, "$y").unwrap();
        let edits = &edit.changes.unwrap()[&uri];
        assert!(edits.iter().all(|e| e.new_text == "y"));
    }

    #[test]
    fn test_rename_container() {
        let src = "\
sub foo {
    my (%opts) = @_;
    print $opts{name};
    return %opts;
}";
        let tree = parse(src);
        let uri = test_uri();
        let edit = rename(&tree, src, Position::new(1, 9), &uri, "config").unwrap();
        let edits = &edit.changes.unwrap()[&uri];
        assert!(edits.iter().all(|e| e.new_text == "config"));

        // Apply edits and verify sigils are preserved
        let mut result = src.to_string();
        let mut sorted: Vec<_> = edits.clone();
        sorted.sort_by(|a, b| {
            b.range.start.line.cmp(&a.range.start.line)
                .then(b.range.start.character.cmp(&a.range.start.character))
        });
        for e in &sorted {
            let lines: Vec<&str> = result.split('\n').collect();
            let line = lines[e.range.start.line as usize];
            let new_line = format!(
                "{}{}{}",
                &line[..e.range.start.character as usize],
                e.new_text,
                &line[e.range.end.character as usize..]
            );
            let mut new_lines: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
            new_lines[e.range.start.line as usize] = new_line;
            result = new_lines.join("\n");
        }
        assert!(result.contains("%config"));       // declaration
        assert!(result.contains("$config{name}"));  // container access
        assert!(result.contains("return %config")); // plain hash ref
        assert!(!result.contains("%config{"));       // no wrong sigil on access
    }

    #[test]
    fn test_rename_function() {
        let src = "sub add { 1 }\nadd();";
        let tree = parse(src);
        let uri = test_uri();
        let edit = rename(&tree, src, Position::new(0, 4), &uri, "sum").unwrap();
        let edits = &edit.changes.unwrap()[&uri];
        assert!(edits.iter().all(|e| e.new_text == "sum"));
        assert_eq!(edits.len(), 2);
    }

    // ---- Hover ----

    #[test]
    fn test_hover_variable() {
        let src = "my $x = 42;\nprint $x;";
        let tree = parse(src);
        let hover = hover_info(&tree, src, Position::new(1, 7)).unwrap();
        match hover.contents {
            HoverContents::Markup(m) => assert!(m.value.contains("my $x = 42")),
            _ => panic!("expected markup"),
        }
    }

    #[test]
    fn test_hover_function() {
        let src = "sub greet { print 'hi' }\ngreet();";
        let tree = parse(src);
        let hover = hover_info(&tree, src, Position::new(1, 0)).unwrap();
        match hover.contents {
            HoverContents::Markup(m) => assert!(m.value.contains("sub greet")),
            _ => panic!("expected markup"),
        }
    }

    #[test]
    fn test_hover_package() {
        let src = "package Foo;";
        let tree = parse(src);
        let hover = hover_info(&tree, src, Position::new(0, 8)).unwrap();
        match hover.contents {
            HoverContents::Markup(m) => assert!(m.value.contains("Foo")),
            _ => panic!("expected markup"),
        }
    }
}
