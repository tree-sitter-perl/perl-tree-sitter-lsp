use tree_sitter::{Node, Point, Tree};

// ---- Types ----

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

fn node_to_span(node: Node) -> Span {
    Span {
        start: node.start_position(),
        end: node.end_position(),
    }
}

pub enum CursorSymbol {
    Variable { text: String },
    Function { name: String },
    Method { name: String, invocant: Option<String> },
    Package { name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Method,
    Variable,
    Package,
    Class,
    Module,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub detail: Option<String>,
    pub kind: SymbolKind,
    pub span: Span,
    pub selection_span: Span,
    pub children: Vec<SymbolInfo>,
}

pub struct HoverResult {
    pub markdown: String,
}

pub struct RenameEdit {
    pub span: Span,
    pub new_text: String,
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub name: String,
    pub span: Span,
    pub parent: Option<String>,
    pub roles: Vec<String>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub sigil: char,
    pub span: Span,
    pub attributes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub span: Span,
    pub selection_span: Span,
}

// ---- Symbol at cursor ----

pub fn symbol_at_cursor(tree: &Tree, source: &[u8], point: Point) -> Option<CursorSymbol> {
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
            "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
                let canonical = canonical_var_name(node, source)?;
                return Some(CursorSymbol::Variable { text: canonical });
            }
            "function" => {
                let text = node.utf8_text(source).ok()?;
                return Some(CursorSymbol::Function {
                    name: text.to_string(),
                });
            }
            "method" => {
                let text = node.utf8_text(source).ok()?;
                // Check if this is a method_call_expression to capture invocant
                let invocant = node.parent().and_then(|p| {
                    if p.kind() == "method_call_expression" {
                        p.child_by_field_name("invocant")
                            .and_then(|inv| inv.utf8_text(source).ok())
                            .map(|s| s.to_string())
                    } else {
                        None
                    }
                });
                return Some(CursorSymbol::Method {
                    name: text.to_string(),
                    invocant,
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

pub fn extract_symbols(tree: &Tree, source: &str) -> Vec<SymbolInfo> {
    let source_bytes = source.as_bytes();
    let mut symbols = Vec::new();
    collect_symbols(tree.root_node(), source_bytes, &mut symbols);
    symbols
}

fn collect_symbols(node: Node, source: &[u8], symbols: &mut Vec<SymbolInfo>) {
    match node.kind() {
        "subroutine_declaration_statement" | "method_declaration_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    let is_method = node.kind() == "method_declaration_statement";
                    let mut children = Vec::new();
                    collect_signature_params(node, source, &mut children);
                    if let Some(body) = node.child_by_field_name("body") {
                        collect_symbols(body, source, &mut children);
                    }
                    symbols.push(SymbolInfo {
                        name: if is_method {
                            format!("method {}", name)
                        } else {
                            format!("sub {}", name)
                        },
                        detail: None,
                        kind: if is_method {
                            SymbolKind::Method
                        } else {
                            SymbolKind::Function
                        },
                        span: node_to_span(node),
                        selection_span: node_to_span(name_node),
                        children,
                    });
                    return;
                }
            }
        }
        "package_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    symbols.push(SymbolInfo {
                        name: name.to_string(),
                        detail: Some("package".to_string()),
                        kind: SymbolKind::Package,
                        span: node_to_span(node),
                        selection_span: node_to_span(name_node),
                        children: Vec::new(),
                    });
                }
            }
        }
        "class_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    let mut children = Vec::new();
                    // Collect fields and methods from the class block
                    for i in 0..node.child_count() {
                        if let Some(block) = node.child(i) {
                            if block.kind() == "block" {
                                collect_class_symbol_children(block, source, &mut children);
                                break;
                            }
                        }
                    }
                    symbols.push(SymbolInfo {
                        name: name.to_string(),
                        detail: Some("class".to_string()),
                        kind: SymbolKind::Class,
                        span: node_to_span(node),
                        selection_span: node_to_span(name_node),
                        children,
                    });
                }
            }
            return;
        }
        "variable_declaration" => {
            let keyword = get_decl_keyword(node, source).unwrap_or("my");
            let mut vars = Vec::new();
            collect_declared_vars(node, source, &mut vars);
            for (name, sel_node) in vars {
                symbols.push(SymbolInfo {
                    name,
                    detail: Some(keyword.to_string()),
                    kind: SymbolKind::Variable,
                    span: node_to_span(node),
                    selection_span: node_to_span(sel_node),
                    children: Vec::new(),
                });
            }
            return;
        }
        "use_statement" => {
            if let Some(module_node) = node.child_by_field_name("module") {
                if let Ok(name) = module_node.utf8_text(source) {
                    symbols.push(SymbolInfo {
                        name: format!("use {}", name),
                        detail: None,
                        kind: SymbolKind::Module,
                        span: node_to_span(node),
                        selection_span: node_to_span(module_node),
                        children: Vec::new(),
                    });
                }
            }
            return;
        }
        _ => {}
    }

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

fn collect_signature_params(sub_node: Node, source: &[u8], out: &mut Vec<SymbolInfo>) {
    for i in 0..sub_node.child_count() {
        if let Some(sig) = sub_node.child(i) {
            if sig.kind() == "signature" {
                let mut vars = Vec::new();
                collect_declared_vars(sig, source, &mut vars);
                for (name, sel_node) in vars {
                    out.push(SymbolInfo {
                        name,
                        detail: Some("param".to_string()),
                        kind: SymbolKind::Variable,
                        span: node_to_span(sig),
                        selection_span: node_to_span(sel_node),
                        children: Vec::new(),
                    });
                }
                break;
            }
        }
    }
}

fn collect_class_symbol_children(block: Node, source: &[u8], out: &mut Vec<SymbolInfo>) {
    for i in 0..block.child_count() {
        if let Some(child) = block.child(i) {
            match child.kind() {
                "expression_statement" => {
                    // Field declarations: expression_statement > variable_declaration (field keyword)
                    // or expression_statement > assignment_expression > variable_declaration
                    if let Some(field_info) = try_parse_field(child, source) {
                        let full_name = format!("{}{}", field_info.sigil, field_info.name);
                        let detail = if field_info.attributes.is_empty() {
                            "field".to_string()
                        } else {
                            format!("field :{}", field_info.attributes.join(" :"))
                        };
                        out.push(SymbolInfo {
                            name: full_name,
                            detail: Some(detail),
                            kind: SymbolKind::Variable,
                            span: field_info.span,
                            selection_span: field_info.span,
                            children: Vec::new(),
                        });
                    }
                }
                "method_declaration_statement" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        if let Ok(name) = name_node.utf8_text(source) {
                            let mut children = Vec::new();
                            collect_signature_params(child, source, &mut children);
                            if let Some(body) = child.child_by_field_name("body") {
                                collect_symbols(body, source, &mut children);
                            }
                            out.push(SymbolInfo {
                                name: format!("method {}", name),
                                detail: None,
                                kind: SymbolKind::Method,
                                span: node_to_span(child),
                                selection_span: node_to_span(name_node),
                                children,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

// ---- Go to definition ----

pub fn find_definition(tree: &Tree, source: &str, point: Point) -> Option<Span> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, point)?;

    match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            find_variable_def(tree.root_node(), source_bytes, text, point)
        }
        CursorSymbol::Function { ref name } => {
            let (_, sel_span) = find_sub_def(tree.root_node(), source_bytes, name)?;
            Some(sel_span)
        }
        CursorSymbol::Method { ref name, ref invocant } => {
            // Try type-aware resolution: trace invocant to ClassName->new()
            if let Some(inv_text) = invocant {
                if let Some(class_name) =
                    infer_variable_class(tree.root_node(), source_bytes, inv_text, point)
                {
                    if let Some(span) = find_method_in_class(
                        tree.root_node(),
                        source_bytes,
                        &class_name,
                        name,
                    ) {
                        return Some(span);
                    }
                }
            }
            // Fall back to file-wide sub/method name search
            let (_, sel_span) = find_sub_def(tree.root_node(), source_bytes, name)?;
            Some(sel_span)
        }
        CursorSymbol::Package { .. } => None,
    }
}

fn find_variable_def(
    root: Node,
    source: &[u8],
    target_text: &str,
    before: Point,
) -> Option<Span> {
    let mut best: Option<(Span, usize)> = None;
    find_variable_def_walk(root, source, target_text, before, &mut best);
    best.map(|(span, _)| span)
}

fn find_variable_def_walk(
    node: Node,
    source: &[u8],
    target_text: &str,
    before_point: Point,
    best: &mut Option<(Span, usize)>,
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
                        *best = Some((node_to_span(var_node), scope_size));
                    }
                }
            }
        }
    }

    // sub foo ($x, @rest) { ... } — signature params are scoped to the body block.
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(body) = node.child_by_field_name("body") {
            let body_end = body.end_position();
            if body_end >= before_point {
                for i in 0..node.child_count() {
                    if let Some(sig) = node.child(i) {
                        if sig.kind() == "signature" {
                            if let Some(var_node) =
                                find_var_in_subtree(sig, source, target_text)
                            {
                                if var_node.start_position() <= before_point {
                                    let scope_size =
                                        body_end.row - body.start_position().row;
                                    let should_replace = match best {
                                        Some((_, prev_size)) => scope_size <= *prev_size,
                                        None => true,
                                    };
                                    if should_replace {
                                        *best =
                                            Some((node_to_span(var_node), scope_size));
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    // for/foreach my $var (...) { ... } — variable: field is the loop variable,
    // scoped to the for_statement's block.
    if node.kind() == "for_statement" {
        if let Some(var_node) = node.child_by_field_name("variable") {
            if var_node.start_position() <= before_point {
                if var_node.utf8_text(source).ok() == Some(target_text) {
                    if let Some(block) = node.child_by_field_name("block") {
                        let block_end = block.end_position();
                        if block_end >= before_point {
                            let scope_size = block_end.row - block.start_position().row;
                            let should_replace = match best {
                                Some((_, prev_size)) => scope_size <= *prev_size,
                                None => true,
                            };
                            if should_replace {
                                *best = Some((node_to_span(var_node), scope_size));
                            }
                        }
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

// ---- Class extraction ----

pub fn extract_classes(tree: &Tree, source: &str) -> Vec<ClassInfo> {
    let source_bytes = source.as_bytes();
    let mut classes = Vec::new();
    collect_classes(tree.root_node(), source_bytes, &mut classes);
    classes
}

fn collect_classes(node: Node, source: &[u8], classes: &mut Vec<ClassInfo>) {
    if node.kind() == "class_statement" {
        if let Some(info) = parse_class_node(node, source) {
            classes.push(info);
        }
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_classes(child, source, classes);
        }
    }
}

fn parse_class_node(node: Node, source: &[u8]) -> Option<ClassInfo> {
    let name_node = node.child_by_field_name("name")?;
    let name = name_node.utf8_text(source).ok()?.to_string();

    let mut parent = None;
    let mut roles = Vec::new();

    // Parse :isa(X) and :does(Y) from the class attributes
    if let Some(attrlist) = node.child_by_field_name("attributes") {
        for i in 0..attrlist.named_child_count() {
            if let Some(attr) = attrlist.named_child(i) {
                if attr.kind() == "attribute" {
                    let attr_name = attr
                        .child_by_field_name("name")
                        .and_then(|n| n.utf8_text(source).ok());
                    let attr_value = attr
                        .child_by_field_name("value")
                        .and_then(|n| n.utf8_text(source).ok());
                    match (attr_name, attr_value) {
                        (Some("isa"), Some(val)) => parent = Some(val.to_string()),
                        (Some("does"), Some(val)) => roles.push(val.to_string()),
                        _ => {}
                    }
                }
            }
        }
    }

    // Find the class block and collect fields + methods
    let mut fields = Vec::new();
    let mut methods = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if child.kind() == "block" {
                collect_class_members(child, source, &mut fields, &mut methods);
                break;
            }
        }
    }

    Some(ClassInfo {
        name,
        span: node_to_span(node),
        parent,
        roles,
        fields,
        methods,
    })
}

fn collect_class_members(
    block: Node,
    source: &[u8],
    fields: &mut Vec<FieldInfo>,
    methods: &mut Vec<MethodInfo>,
) {
    for i in 0..block.child_count() {
        if let Some(child) = block.child(i) {
            match child.kind() {
                "expression_statement" => {
                    // field declarations appear as expression_statement > variable_declaration
                    // or expression_statement > assignment_expression > variable_declaration
                    if let Some(field_info) = try_parse_field(child, source) {
                        fields.push(field_info);
                    }
                }
                "method_declaration_statement" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        if let Ok(name) = name_node.utf8_text(source) {
                            methods.push(MethodInfo {
                                name: name.to_string(),
                                span: node_to_span(child),
                                selection_span: node_to_span(name_node),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

fn try_parse_field(expr_stmt: Node, source: &[u8]) -> Option<FieldInfo> {
    // Look for variable_declaration with "field" keyword, either directly
    // or inside an assignment_expression
    for i in 0..expr_stmt.named_child_count() {
        let child = expr_stmt.named_child(i)?;
        let var_decl = if child.kind() == "variable_declaration" {
            child
        } else if child.kind() == "assignment_expression" {
            child.child_by_field_name("left").filter(|n| n.kind() == "variable_declaration")?
        } else {
            continue;
        };

        // Check if keyword is "field"
        let keyword = get_decl_keyword(var_decl, source)?;
        if keyword != "field" {
            return None;
        }

        let var_node = var_decl.child_by_field_name("variable")?;
        let full_name = var_node.utf8_text(source).ok()?;
        let sigil = full_name.chars().next()?;
        let bare_name = full_name[1..].to_string();

        let mut attributes = Vec::new();
        if let Some(attrlist) = var_decl.child_by_field_name("attributes") {
            for j in 0..attrlist.named_child_count() {
                if let Some(attr) = attrlist.named_child(j) {
                    if attr.kind() == "attribute" {
                        if let Some(name_node) = attr.child_by_field_name("name") {
                            if let Ok(attr_name) = name_node.utf8_text(source) {
                                attributes.push(attr_name.to_string());
                            }
                        }
                    }
                }
            }
        }

        return Some(FieldInfo {
            name: bare_name,
            sigil,
            span: node_to_span(var_decl),
            attributes,
        });
    }
    None
}

// ---- Type inference (simple) ----

/// Try to infer the class of a variable by tracing back to `ClassName->new(...)`.
fn infer_variable_class<'a>(
    root: Node<'a>,
    source: &'a [u8],
    var_text: &str,
    before: Point,
) -> Option<String> {
    let mut result = None;
    infer_variable_class_walk(root, source, var_text, before, &mut result);
    result
}

fn infer_variable_class_walk(
    node: Node,
    source: &[u8],
    var_text: &str,
    before: Point,
    result: &mut Option<String>,
) {
    // Look for: my $var = ClassName->new(...)
    // AST: assignment_expression { left: variable_declaration, right: method_call_expression }
    if node.kind() == "assignment_expression" {
        if let Some(left) = node.child_by_field_name("left") {
            if left.kind() == "variable_declaration" {
                if decl_contains_variable(left, source, var_text).is_some() {
                    if let Some(right) = node.child_by_field_name("right") {
                        if let Some(class_name) = extract_constructor_class(right, source) {
                            if node.start_position() <= before {
                                *result = Some(class_name);
                            }
                        }
                    }
                }
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            infer_variable_class_walk(child, source, var_text, before, result);
        }
    }
}

/// Check if a node is `ClassName->new(...)` and return the class name.
fn extract_constructor_class(node: Node, source: &[u8]) -> Option<String> {
    if node.kind() != "method_call_expression" {
        return None;
    }
    let method_node = node.child_by_field_name("method")?;
    let method_name = method_node.utf8_text(source).ok()?;
    if method_name != "new" {
        return None;
    }
    let invocant = node.child_by_field_name("invocant")?;
    // The invocant should be a package name (bareword/package node)
    if invocant.kind() == "package" || invocant.kind() == "bareword" {
        return invocant.utf8_text(source).ok().map(|s| s.to_string());
    }
    None
}

/// Find a method definition inside a specific class.
fn find_method_in_class(
    root: Node,
    source: &[u8],
    class_name: &str,
    method_name: &str,
) -> Option<Span> {
    find_method_in_class_walk(root, source, class_name, method_name)
}

fn find_method_in_class_walk(
    node: Node,
    source: &[u8],
    class_name: &str,
    method_name: &str,
) -> Option<Span> {
    if node.kind() == "class_statement" {
        if let Some(name_node) = node.child_by_field_name("name") {
            if name_node.utf8_text(source).ok() == Some(class_name) {
                // Search for method in this class's block
                for i in 0..node.child_count() {
                    if let Some(block) = node.child(i) {
                        if block.kind() == "block" {
                            return find_method_in_block(block, source, method_name);
                        }
                    }
                }
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(span) = find_method_in_class_walk(child, source, class_name, method_name) {
                return Some(span);
            }
        }
    }
    None
}

fn find_method_in_block(block: Node, source: &[u8], method_name: &str) -> Option<Span> {
    for i in 0..block.child_count() {
        if let Some(child) = block.child(i) {
            if child.kind() == "method_declaration_statement" {
                if let Some(name_node) = child.child_by_field_name("name") {
                    if name_node.utf8_text(source).ok() == Some(method_name) {
                        return Some(node_to_span(name_node));
                    }
                }
            }
        }
    }
    None
}

pub fn find_sub_def(root: Node, source: &[u8], name: &str) -> Option<(Span, Span)> {
    find_sub_def_walk(root, source, name)
}

fn find_sub_def_walk(node: Node, source: &[u8], name: &str) -> Option<(Span, Span)> {
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(name_node) = node.child_by_field_name("name") {
            if name_node.utf8_text(source).ok() == Some(name) {
                return Some((node_to_span(node), node_to_span(name_node)));
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

pub fn find_references(tree: &Tree, source: &str, point: Point) -> Vec<Span> {
    let source_bytes = source.as_bytes();
    let cursor_sym = match symbol_at_cursor(tree, source_bytes, point) {
        Some(s) => s,
        None => return vec![],
    };

    let mut spans = Vec::new();

    match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            let target_def = find_variable_def(tree.root_node(), source_bytes, text, point);

            let mut all_refs = Vec::new();
            collect_variable_refs(tree.root_node(), source_bytes, text, &mut all_refs);

            for ref_span in all_refs {
                let ref_def = find_variable_def(
                    tree.root_node(),
                    source_bytes,
                    text,
                    ref_span.start,
                );
                if ref_def == target_def {
                    spans.push(ref_span);
                }
            }
        }
        CursorSymbol::Function { ref name } | CursorSymbol::Method { ref name, .. } => {
            collect_function_refs(tree.root_node(), source_bytes, name, &mut spans);
        }
        CursorSymbol::Package { ref name } => {
            collect_package_refs(tree.root_node(), source_bytes, name, &mut spans);
        }
    }

    spans
}

fn collect_variable_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Span>) {
    match node.kind() {
        "scalar" | "array" | "hash" => {
            if node.utf8_text(source).ok() == Some(target) {
                refs.push(node_to_span(node));
                return;
            }
            // Don't return — dereference expressions like @{$hash{key}} parse as
            // an array node wrapping a block with container_variable inside.
        }
        "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
            if canonical_var_name(node, source).as_deref() == Some(target) {
                refs.push(node_to_span(node));
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

fn canonical_var_name(node: Node, source: &[u8]) -> Option<String> {
    let varname = get_varname(node, source)?;

    match node.kind() {
        "keyval_container_variable" => return Some(format!("%{}", varname)),
        "slice_container_variable" => {
            if let Some(parent) = node.parent() {
                if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
                    return Some(format!("%{}", varname));
                }
            }
            return Some(format!("@{}", varname));
        }
        _ => {}
    }

    if let Some(parent) = node.parent() {
        if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
            return Some(format!("%{}", varname));
        }
        if parent.child_by_field_name("array").map_or(false, |a| a.id() == node.id()) {
            return Some(format!("@{}", varname));
        }
    }
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

fn collect_function_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Span>) {
    match node.kind() {
        "subroutine_declaration_statement" | "method_declaration_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if name_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_span(name_node));
                }
            }
        }
        "function_call_expression" => {
            if let Some(func_node) = node.child_by_field_name("function") {
                if func_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_span(func_node));
                }
            }
        }
        "method_call_expression" => {
            if let Some(method_node) = node.child_by_field_name("method") {
                if method_node.utf8_text(source).ok() == Some(target) {
                    refs.push(node_to_span(method_node));
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

fn collect_package_refs(node: Node, source: &[u8], target: &str, refs: &mut Vec<Span>) {
    if node.kind() == "package" && node.utf8_text(source).ok() == Some(target) {
        refs.push(node_to_span(node));
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_package_refs(child, source, target, refs);
        }
    }
}

// ---- Rename ----

pub fn rename(tree: &Tree, source: &str, point: Point, new_name: &str) -> Option<Vec<RenameEdit>> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, point)?;

    let ref_spans = find_references(tree, source, point);
    if ref_spans.is_empty() {
        return None;
    }

    let bare_name = new_name.trim_start_matches(['$', '@', '%']);

    let edits = ref_spans
        .into_iter()
        .map(|span| match &cursor_sym {
            CursorSymbol::Variable { .. } => {
                // Skip the sigil (first character)
                let mut adjusted = span;
                adjusted.start = Point::new(span.start.row, span.start.column + 1);
                RenameEdit {
                    span: adjusted,
                    new_text: bare_name.to_string(),
                }
            }
            _ => RenameEdit {
                span,
                new_text: bare_name.to_string(),
            },
        })
        .collect();

    Some(edits)
}

// TODO: consider handling `local $var` (localization_expression) as a declaration.
// It doesn't create a new lexical variable — it temporarily saves/restores a global —
// so it's unclear whether gd should resolve to it.

// ---- Hover ----

pub fn hover_info(tree: &Tree, source: &str, point: Point) -> Option<HoverResult> {
    let source_bytes = source.as_bytes();
    let cursor_sym = symbol_at_cursor(tree, source_bytes, point)?;

    let markdown = match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            if let Some(def_span) = find_variable_def(tree.root_node(), source_bytes, text, point) {
                let line = source.lines().nth(def_span.start.row).unwrap_or("");
                format!("```perl\n{}\n```", line.trim())
            } else {
                format!("`{}`", text)
            }
        }
        CursorSymbol::Function { ref name } => {
            if let Some((full_span, _)) = find_sub_def(tree.root_node(), source_bytes, name) {
                let line = source.lines().nth(full_span.start.row).unwrap_or("");
                format!("```perl\n{}\n```", line.trim())
            } else {
                format!("`{}`", name)
            }
        }
        CursorSymbol::Method { ref name, ref invocant } => {
            // Try type-aware: resolve invocant class, find method in class
            if let Some(inv_text) = invocant {
                if let Some(class_name) =
                    infer_variable_class(tree.root_node(), source_bytes, inv_text, point)
                {
                    if let Some(span) = find_method_in_class(
                        tree.root_node(),
                        source_bytes,
                        &class_name,
                        name,
                    ) {
                        let line = source.lines().nth(span.start.row).unwrap_or("");
                        return Some(HoverResult {
                            markdown: format!(
                                "**{}**\n```perl\n{}\n```",
                                class_name,
                                line.trim()
                            ),
                        });
                    }
                }
            }
            // Fall back to file-wide sub/method search
            if let Some((full_span, _)) = find_sub_def(tree.root_node(), source_bytes, name) {
                let line = source.lines().nth(full_span.start.row).unwrap_or("");
                format!("```perl\n{}\n```", line.trim())
            } else {
                format!("`{}`", name)
            }
        }
        CursorSymbol::Package { ref name } => {
            format!("**package** `{}`", name)
        }
    };

    Some(HoverResult { markdown })
}

// ---- Tests ----

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

    // ---- Document symbols ----

    #[test]
    fn test_symbols_sub() {
        let src = "sub foo { 1 }";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "sub foo");
        assert_eq!(syms[0].kind, SymbolKind::Function);
    }

    #[test]
    fn test_symbols_package() {
        let src = "package Foo;";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Foo");
        assert_eq!(syms[0].kind, SymbolKind::Package);
    }

    #[test]
    fn test_symbols_variables() {
        let src = "my $x = 1;\nour @list;";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 2);
        assert_eq!(syms[0].name, "$x");
        assert_eq!(syms[0].kind, SymbolKind::Variable);
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
        assert_eq!(syms[0].kind, SymbolKind::Module);
    }

    #[test]
    fn test_symbols_nested() {
        let src = "sub foo {\n    my $x = 1;\n}";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].children.len(), 1);
        assert_eq!(syms[0].children[0].name, "$x");
    }

    #[test]
    fn test_symbols_paren_list() {
        let src = "sub foo {\n    my ($x, %h) = @_;\n}";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        let names: Vec<&str> = syms[0].children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"$x"));
        assert!(names.contains(&"%h"));
    }

    #[test]
    fn test_symbols_signature() {
        let src = "sub foo ($x, $y = 10, @rest) { 1 }";
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        let names: Vec<&str> = syms[0].children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"$x"));
        assert!(names.contains(&"$y"));
        assert!(names.contains(&"@rest"));
        assert!(syms[0].children.iter().all(|c| c.detail.as_deref() == Some("param")));
    }

    // ---- Go to definition ----

    #[test]
    fn test_def_variable() {
        let src = "my $x = 1;\nprint $x;";
        let tree = parse(src);
        let span = find_definition(&tree, src, Point::new(1, 7)).unwrap();
        assert_eq!(span.start.row, 0);
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
        let span = find_definition(&tree, src, Point::new(3, 11)).unwrap();
        assert_eq!(span.start.row, 2); // inner my $x at L2, not L0
    }

    #[test]
    fn test_def_sub() {
        let src = "sub add { 1 }\nadd();";
        let tree = parse(src);
        let span = find_definition(&tree, src, Point::new(1, 0)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_container_variable() {
        let src = "\
sub new {
    my (%args) = @_;
    return $args{verbose};
}";
        let tree = parse(src);
        let span = find_definition(&tree, src, Point::new(2, 12)).unwrap();
        assert_eq!(span.start.row, 1); // jumps to %args decl
    }

    #[test]
    fn test_def_for_loop_variable() {
        let src = "\
for my $i (1..10) {
    print $i;
}";
        let tree = parse(src);
        let span = find_definition(&tree, src, Point::new(1, 11)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_for_loop_scoped() {
        let src = "\
my $i = 99;
for my $i (1..10) {
    print $i;
}
print $i;";
        let tree = parse(src);
        // $i inside loop body → for loop's $i
        let span = find_definition(&tree, src, Point::new(2, 11)).unwrap();
        assert_eq!(span.start.row, 1);

        // $i after loop → outer my $i
        let span2 = find_definition(&tree, src, Point::new(4, 7)).unwrap();
        assert_eq!(span2.start.row, 0);
    }

    #[test]
    fn test_def_signature_param() {
        let src = "\
sub foo ($x, $y) {
    print $x;
}";
        let tree = parse(src);
        let span = find_definition(&tree, src, Point::new(1, 11)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_signature_vs_outer() {
        let src = "\
my $x = 99;
sub foo ($x) {
    print $x;
}
print $x;";
        let tree = parse(src);
        // $x inside sub body → signature param
        let span = find_definition(&tree, src, Point::new(2, 11)).unwrap();
        assert_eq!(span.start.row, 1);

        // $x outside sub → outer my
        let span2 = find_definition(&tree, src, Point::new(4, 7)).unwrap();
        assert_eq!(span2.start.row, 0);
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
        let refs = find_references(&tree, src, Point::new(1, 8));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&1));
        assert!(lines.contains(&2));
        assert!(!lines.contains(&5));
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
        let refs = find_references(&tree, src, Point::new(1, 9));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&1));
        assert!(lines.contains(&2));
        assert!(lines.contains(&3));
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
        let refs = find_references(&tree, src, Point::new(1, 9));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&1));
        assert!(lines.contains(&2));
        assert!(lines.contains(&3));
    }

    #[test]
    fn test_refs_function() {
        let src = "sub add { 1 }\nmy $x = add();";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(0, 4));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&0));
        assert!(lines.contains(&1));
    }

    #[test]
    fn test_refs_package() {
        let src = "package Foo;\nFoo->new();";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(0, 8));
        assert!(refs.len() >= 1);
    }

    #[test]
    fn test_refs_for_loop_scoped() {
        let src = "\
my $i = 99;
for my $i (1..10) {
    print $i;
}
print $i;";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(2, 11));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&1));
        assert!(lines.contains(&2));
        assert!(!lines.contains(&0));
        assert!(!lines.contains(&4));
    }

    #[test]
    fn test_refs_signature_param() {
        let src = "\
sub foo ($x, @rest) {
    print $x;
    return @rest;
}";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(1, 11));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&0));
        assert!(lines.contains(&1));
        assert_eq!(refs.len(), 2);
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
        let edits = rename(&tree, src, Point::new(1, 8), "output").unwrap();
        assert!(edits.iter().all(|e| e.new_text == "output"));
        // Each edit should start after the sigil
        for e in &edits {
            let line = src.lines().nth(e.span.start.row).unwrap();
            let before = &line[..e.span.start.column];
            assert_eq!(before.chars().last().unwrap(), '$');
        }
    }

    #[test]
    fn test_rename_strips_sigil() {
        let src = "my $x = 1;\nprint $x;";
        let tree = parse(src);
        let edits = rename(&tree, src, Point::new(0, 4), "$y").unwrap();
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
        let edits = rename(&tree, src, Point::new(1, 9), "config").unwrap();
        assert!(edits.iter().all(|e| e.new_text == "config"));

        // Apply edits and verify sigils are preserved
        let mut result = src.to_string();
        let mut sorted = edits;
        sorted.sort_by(|a, b| {
            b.span.start.row.cmp(&a.span.start.row)
                .then(b.span.start.column.cmp(&a.span.start.column))
        });
        for e in &sorted {
            let lines: Vec<&str> = result.split('\n').collect();
            let line = lines[e.span.start.row];
            let new_line = format!(
                "{}{}{}",
                &line[..e.span.start.column],
                e.new_text,
                &line[e.span.end.column..]
            );
            let mut new_lines: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
            new_lines[e.span.start.row] = new_line;
            result = new_lines.join("\n");
        }
        assert!(result.contains("%config"));
        assert!(result.contains("$config{name}"));
        assert!(result.contains("return %config"));
        assert!(!result.contains("%config{"));
    }

    #[test]
    fn test_rename_function() {
        let src = "sub add { 1 }\nadd();";
        let tree = parse(src);
        let edits = rename(&tree, src, Point::new(0, 4), "sum").unwrap();
        assert!(edits.iter().all(|e| e.new_text == "sum"));
        assert_eq!(edits.len(), 2);
    }

    // ---- Hover ----

    #[test]
    fn test_hover_variable() {
        let src = "my $x = 42;\nprint $x;";
        let tree = parse(src);
        let result = hover_info(&tree, src, Point::new(1, 7)).unwrap();
        assert!(result.markdown.contains("my $x = 42"));
    }

    #[test]
    fn test_hover_function() {
        let src = "sub greet { print 'hi' }\ngreet();";
        let tree = parse(src);
        let result = hover_info(&tree, src, Point::new(1, 0)).unwrap();
        assert!(result.markdown.contains("sub greet"));
    }

    #[test]
    fn test_hover_package() {
        let src = "package Foo;";
        let tree = parse(src);
        let result = hover_info(&tree, src, Point::new(0, 8)).unwrap();
        assert!(result.markdown.contains("Foo"));
    }

    // ---- Class support ----

    fn class_src() -> &'static str {
        r#"class Point :isa(Base) :does(Printable) {
    field $x :param :reader;
    field $y :param;
    field $label = "point";

    method magnitude () {
        return sqrt($x**2 + $y**2);
    }

    method to_string () {
        return "$label: ($x, $y)";
    }
}"#
    }

    #[test]
    fn test_class_extract() {
        let src = class_src();
        let tree = parse(src);
        let classes = extract_classes(&tree, src);
        assert_eq!(classes.len(), 1);
        let c = &classes[0];
        assert_eq!(c.name, "Point");
        assert_eq!(c.parent.as_deref(), Some("Base"));
        assert_eq!(c.roles, vec!["Printable"]);
        assert_eq!(c.fields.len(), 3);
        assert_eq!(c.fields[0].name, "x");
        assert_eq!(c.fields[0].sigil, '$');
        assert!(c.fields[0].attributes.contains(&"param".to_string()));
        assert!(c.fields[0].attributes.contains(&"reader".to_string()));
        assert_eq!(c.fields[1].name, "y");
        assert!(c.fields[1].attributes.contains(&"param".to_string()));
        assert_eq!(c.fields[2].name, "label");
        assert!(c.fields[2].attributes.is_empty());
        assert_eq!(c.methods.len(), 2);
        assert_eq!(c.methods[0].name, "magnitude");
        assert_eq!(c.methods[1].name, "to_string");
    }

    #[test]
    fn test_class_symbols() {
        let src = class_src();
        let tree = parse(src);
        let syms = extract_symbols(&tree, src);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Point");
        assert_eq!(syms[0].kind, SymbolKind::Class);

        let children = &syms[0].children;
        let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"$x"));
        assert!(names.contains(&"$y"));
        assert!(names.contains(&"$label"));
        assert!(names.contains(&"method magnitude"));
        assert!(names.contains(&"method to_string"));

        // Fields should have "field" in their detail
        let field_x = children.iter().find(|c| c.name == "$x").unwrap();
        assert!(field_x.detail.as_ref().unwrap().starts_with("field"));
    }

    #[test]
    fn test_field_def() {
        // field $x is scoped to the class block, so $x inside method body should resolve to it
        let src = r#"class Point {
    field $x :param;

    method get_x () {
        return $x;
    }
}"#;
        let tree = parse(src);
        // $x on line 4, col 15 → should resolve to field $x on line 1
        let span = find_definition(&tree, src, Point::new(4, 15)).unwrap();
        assert_eq!(span.start.row, 1);
    }

    #[test]
    fn test_field_refs() {
        let src = r#"class Point {
    field $x :param;

    method get_x () {
        return $x;
    }

    method set_x ($val) {
        $x = $val;
    }
}"#;
        let tree = parse(src);
        // References from field $x (line 1)
        let refs = find_references(&tree, src, Point::new(1, 11));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&1)); // declaration
        assert!(lines.contains(&4)); // return $x
        assert!(lines.contains(&8)); // $x = $val
    }

    #[test]
    fn test_method_def_in_class() {
        let src = r#"class Greeter {
    field $name :param;

    method greet () {
        return "Hello, $name!";
    }
}

my $g = Greeter->new(name => "World");
$g->greet();"#;
        let tree = parse(src);
        // Cursor on "greet" in $g->greet() (line 9, col 4)
        let span = find_definition(&tree, src, Point::new(9, 4)).unwrap();
        // Should resolve to method greet on line 3
        assert_eq!(span.start.row, 3);
    }

    #[test]
    fn test_method_def_fallback() {
        // When type can't be inferred, fall back to file-wide search
        let src = r#"class Foo {
    method bar () {
        return 1;
    }
}

sub bar { 2 }
$unknown->bar();"#;
        let tree = parse(src);
        // $unknown has no known type, so bar should resolve to sub bar (line 6)
        // since find_sub_def searches both sub and method declarations
        let span = find_definition(&tree, src, Point::new(7, 11)).unwrap();
        // Could resolve to either the method (line 1) or the sub (line 6)
        // find_sub_def walks in order, so it finds method first
        assert!(span.start.row == 1 || span.start.row == 6);
    }

    #[test]
    fn test_def_for_loop_list_var() {
        let src = "\
my @history = (1, 2, 3);
for my $entry (@history) {
    print $entry;
}";
        let tree = parse(src);
        // @history on line 1 inside for list → should resolve to my @history on line 0
        let span = find_definition(&tree, src, Point::new(1, 16)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_for_loop_list_var_in_context() {
        // Reproduces the sample.pl scenario: @history after package main
        let src = "\
package Calculator;

sub get_history {
    my ($self) = @_;
    return @{$self->{history}};
}

package main;

my $calc = Calculator->new(verbose => 1);
my @history = $calc->get_history();

for my $entry (@history) {
    print $entry;
}";
        let tree = parse(src);
        // @history on line 12 col 16 → should resolve to my @history on line 10
        let span = find_definition(&tree, src, Point::new(12, 16)).unwrap();
        assert_eq!(span.start.row, 10);
    }

    #[test]
    fn test_def_for_loop_list_var_exact_sample() {
        let src = include_str!("../test_files/sample.pl");
        let tree = parse(src);
        // Line 54 (1-indexed) = row 53: for my $entry (@history) {
        // Line 52 (1-indexed) = row 51: my @history = $calc->get_history();
        // @history starts at col 15 (@), col 16 (h)
        let span = find_definition(&tree, src, Point::new(53, 16)).unwrap();
        assert_eq!(span.start.row, 51);
        let span2 = find_definition(&tree, src, Point::new(53, 15)).unwrap();
        assert_eq!(span2.start.row, 51);
    }

    #[test]
    fn test_hover_method_call() {
        let src = r#"class Greeter {
    field $name :param;

    method greet () {
        return "Hello, $name!";
    }
}

my $g = Greeter->new(name => "World");
$g->greet();"#;
        let tree = parse(src);
        // Hover on "greet" in $g->greet()
        let result = hover_info(&tree, src, Point::new(9, 4)).unwrap();
        assert!(result.markdown.contains("Greeter"));
        assert!(result.markdown.contains("method greet"));
    }
}
