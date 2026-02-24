use tree_sitter::{Node, Point, Tree};

// ---- Types ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    HashKey { key_name: String, owner: HashKeyOwner },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKeyOwner {
    /// Keys belong to a class namespace (e.g. Calculator, Point)
    Class(String),
    /// Keys belong to a plain hash variable in a specific scope
    Variable { name: String, def_span: Span },
}

pub struct HashKeyInfo {
    pub key_name: String,
    pub owner: HashKeyOwner,
    pub span: Span,
    pub is_dynamic: bool,
    pub source_text: String,
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

#[derive(Debug, Clone)]
pub struct CompletionCandidate {
    pub label: String,
    pub kind: SymbolKind,
    pub detail: Option<String>,
    pub insert_text: Option<String>,
    pub sort_priority: u8, // lower = higher priority (0 = innermost scope)
}

#[derive(Debug, PartialEq)]
enum CompletionMode {
    Variable { sigil: char },
    Method { invocant: String },
    HashKey { var_text: String },
    General,
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
            "autoquoted_bareword" => {
                // Hash key in braces: $hash{key} or in fat comma context: key => val
                let text = node.utf8_text(source).ok()?;
                if let Some(parent) = node.parent() {
                    // Direct hash element: $hash{bareword} or $self->{bareword}
                    if parent.kind() == "hash_element_expression" {
                        // Check if this node is the key (either via field or by position)
                        let is_key = parent.child_by_field_name("key")
                            .map_or(false, |k| k.id() == node.id());
                        if is_key {
                            if let Some(var_text) = get_hash_from_element(parent, source) {
                                let owner = resolve_key_owner(
                                    find_root(node),
                                    source,
                                    &var_text,
                                    point,
                                )?;
                                return Some(CursorSymbol::HashKey {
                                    key_name: text.to_string(),
                                    owner,
                                });
                            }
                        }
                    }
                    // Fat comma context in list: key => val  inside hash construction
                    if parent.kind() == "list_expression" {
                        if let Some(owner) = resolve_list_key_owner(node, parent, source, point) {
                            return Some(CursorSymbol::HashKey {
                                key_name: text.to_string(),
                                owner,
                            });
                        }
                    }
                }
                // Fall through to bareword handling
                return Some(CursorSymbol::Function {
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
                    // Bareword as invocant of method call → class name
                    if parent.kind() == "method_call_expression" {
                        if let Some(inv) = parent.child_by_field_name("invocant") {
                            if inv.id() == node.id() {
                                return Some(CursorSymbol::Package {
                                    name: text.to_string(),
                                });
                            }
                        }
                    }
                }
                return Some(CursorSymbol::Function {
                    name: text.to_string(),
                });
            }
            "string_content" => {
                // String key inside hash braces: $hash{"key"}
                if let Some(parent) = node.parent() {
                    if let Some(grandparent) = parent.parent() {
                        if grandparent.kind() == "hash_element_expression" {
                            let is_key = grandparent.child_by_field_name("key")
                                .map_or(false, |k| k.id() == parent.id());
                            if is_key {
                                let text = node.utf8_text(source).ok()?;
                                if let Some(var_text) = get_hash_from_element(grandparent, source) {
                                    let owner = resolve_key_owner(
                                        find_root(node),
                                        source,
                                        &var_text,
                                        point,
                                    )?;
                                    return Some(CursorSymbol::HashKey {
                                        key_name: text.to_string(),
                                        owner,
                                    });
                                }
                            }
                        }
                    }
                    // String in list expression (hash construction)
                    // string_content → string_literal → list_expression
                    if let Some(grandparent) = parent.parent() {
                        if grandparent.kind() == "list_expression" {
                            if let Some(owner) = resolve_list_key_owner(parent, grandparent, source, point) {
                                let text = node.utf8_text(source).ok()?;
                                return Some(CursorSymbol::HashKey {
                                    key_name: text.to_string(),
                                    owner,
                                });
                            }
                        }
                    }
                }
                node = node.parent()?;
                continue;
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
            // Try type-aware resolution
            if let Some(inv_text) = invocant {
                // If invocant has no sigil, it's a bareword class name directly
                let class_name = if inv_text.starts_with('$')
                    || inv_text.starts_with('@')
                    || inv_text.starts_with('%')
                {
                    infer_variable_class(tree.root_node(), source_bytes, inv_text, point)
                } else {
                    Some(inv_text.clone())
                };

                if let Some(ref cn) = class_name {
                    // Try to find the method in the class
                    if let Some(span) = find_method_in_class(
                        tree.root_node(),
                        source_bytes,
                        cn,
                        name,
                    ) {
                        return Some(span);
                    }
                    // Method not found in class (e.g. auto-generated "new") → go to class def
                    if let Some(span) = find_package_def(tree.root_node(), source_bytes, cn) {
                        return Some(span);
                    }
                }
            }
            // Fall back to file-wide sub/method name search
            let (_, sel_span) = find_sub_def(tree.root_node(), source_bytes, name)?;
            Some(sel_span)
        }
        CursorSymbol::Package { ref name } => {
            find_package_def(tree.root_node(), source_bytes, name)
        }
        CursorSymbol::HashKey { ref key_name, ref owner } => {
            let keys = collect_hash_keys(tree.root_node(), source_bytes, owner, point);
            keys.iter()
                .filter(|k| k.key_name == *key_name && !k.is_dynamic)
                .map(|k| k.span)
                .next()
        }
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

fn find_package_def(root: Node, source: &[u8], name: &str) -> Option<Span> {
    find_package_def_walk(root, source, name)
}

fn find_package_def_walk(node: Node, source: &[u8], name: &str) -> Option<Span> {
    if node.kind() == "package_statement" || node.kind() == "class_statement" {
        if let Some(name_node) = node.child_by_field_name("name") {
            if name_node.utf8_text(source).ok() == Some(name) {
                return Some(node_to_span(name_node));
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(span) = find_package_def_walk(child, source, name) {
                return Some(span);
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
        CursorSymbol::HashKey { ref key_name, ref owner } => {
            let keys = collect_hash_keys(tree.root_node(), source_bytes, owner, point);
            for k in keys {
                if k.key_name == *key_name {
                    spans.push(k.span);
                }
            }
        }
    }

    spans
}

// ---- Document highlight ----

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    Read,
    Write,
}

pub struct Highlight {
    pub span: Span,
    pub kind: HighlightKind,
}

pub fn document_highlights(tree: &Tree, source: &str, point: Point) -> Vec<Highlight> {
    let source_bytes = source.as_bytes();
    let cursor_sym = match symbol_at_cursor(tree, source_bytes, point) {
        Some(s) => s,
        None => return vec![],
    };

    match cursor_sym {
        CursorSymbol::Variable { ref text } => {
            let target_def = find_variable_def(tree.root_node(), source_bytes, text, point);

            let mut all_refs = Vec::new();
            collect_variable_refs(tree.root_node(), source_bytes, text, &mut all_refs);

            all_refs
                .into_iter()
                .filter(|ref_span| {
                    let ref_def = find_variable_def(
                        tree.root_node(),
                        source_bytes,
                        text,
                        ref_span.start,
                    );
                    ref_def == target_def
                })
                .map(|span| {
                    let kind = if is_write_position(tree, source_bytes, span.start) {
                        HighlightKind::Write
                    } else {
                        HighlightKind::Read
                    };
                    Highlight { span, kind }
                })
                .collect()
        }
        CursorSymbol::Function { ref name } | CursorSymbol::Method { ref name, .. } => {
            let mut spans = Vec::new();
            collect_function_refs(tree.root_node(), source_bytes, name, &mut spans);
            spans
                .into_iter()
                .map(|span| Highlight {
                    span,
                    kind: HighlightKind::Read,
                })
                .collect()
        }
        CursorSymbol::Package { ref name } => {
            let mut spans = Vec::new();
            collect_package_refs(tree.root_node(), source_bytes, name, &mut spans);
            spans
                .into_iter()
                .map(|span| Highlight {
                    span,
                    kind: HighlightKind::Read,
                })
                .collect()
        }
        CursorSymbol::HashKey { ref key_name, ref owner } => {
            let keys = collect_hash_keys(tree.root_node(), source_bytes, owner, point);
            keys.into_iter()
                .filter(|k| k.key_name == *key_name)
                .map(|k| Highlight {
                    span: k.span,
                    kind: HighlightKind::Read,
                })
                .collect()
        }
    }
}

/// Check if a variable at `point` is in a write (assignment) position.
fn is_write_position(tree: &Tree, _source: &[u8], point: Point) -> bool {
    let node = match tree
        .root_node()
        .named_descendant_for_point_range(point, point)
    {
        Some(n) => n,
        None => return false,
    };

    // Walk up to check if this variable is on the left side of an assignment,
    // or is part of a declaration
    let mut current = node;
    for _ in 0..10 {
        if let Some(parent) = current.parent() {
            match parent.kind() {
                "variable_declaration" => return true,
                "assignment_expression" => {
                    // Check if we're on the left side
                    if let Some(left) = parent.child_by_field_name("left") {
                        if left.start_position() <= point && left.end_position() >= point {
                            return true;
                        }
                    }
                    return false;
                }
                "signature" => return true,
                "for_statement" => {
                    // Loop variable is a write
                    if let Some(var) = parent.child_by_field_name("variable") {
                        if var.start_position() <= point && var.end_position() >= point {
                            return true;
                        }
                    }
                    return false;
                }
                _ => {
                    current = parent;
                    continue;
                }
            }
        }
        break;
    }
    false
}

// ---- Selection range ----

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

// ---- Folding ranges ----

#[derive(Debug, Clone)]
pub struct FoldRange {
    pub start_line: usize,
    pub end_line: usize,
    pub kind: FoldKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FoldKind {
    Region,
    Comment,
}

pub fn folding_ranges(tree: &Tree, source: &str) -> Vec<FoldRange> {
    let mut ranges = Vec::new();
    collect_fold_ranges(tree.root_node(), source.as_bytes(), &mut ranges);
    // Pod sections
    collect_pod_folds(source, &mut ranges);
    ranges
}

fn collect_fold_ranges(node: Node, source: &[u8], out: &mut Vec<FoldRange>) {
    let foldable = match node.kind() {
        "block" | "do_block" => true,
        "subroutine_declaration_statement"
        | "method_declaration_statement"
        | "class_statement"
        | "package_statement" => {
            // Fold from the declaration line to the end of the body
            // but only if it spans multiple lines
            false // we'll fold their block children instead
        }
        "if_statement" | "unless_statement" | "while_statement" | "until_statement"
        | "for_statement" | "foreach_statement" => true,
        "heredoc_body" | "heredoc_body_qq" | "heredoc_body_qx" => true,
        _ => false,
    };

    if foldable {
        let start = node.start_position().row;
        let end = node.end_position().row;
        if end > start {
            out.push(FoldRange {
                start_line: start,
                end_line: end,
                kind: FoldKind::Region,
            });
        }
    }

    // Also fold multi-line comments (consecutive # lines)
    // Handled separately — just recurse into children here
    let _ = source; // used by pod fold collector

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_fold_ranges(child, source, out);
        }
    }
}

fn collect_pod_folds(source: &str, out: &mut Vec<FoldRange>) {
    let mut pod_start: Option<usize> = None;
    for (i, line) in source.lines().enumerate() {
        if line.starts_with("=head")
            || line.starts_with("=pod")
            || line.starts_with("=begin")
            || line.starts_with("=over")
            || line.starts_with("=item")
            || line.starts_with("=encoding")
            || line.starts_with("=for")
        {
            if pod_start.is_none() {
                pod_start = Some(i);
            }
        }
        if line.starts_with("=cut") {
            if let Some(start) = pod_start {
                out.push(FoldRange {
                    start_line: start,
                    end_line: i,
                    kind: FoldKind::Comment,
                });
                pod_start = None;
            }
        }
    }
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

    // Bareword used as invocant of method call → class name reference
    if node.kind() == "bareword" && node.utf8_text(source).ok() == Some(target) {
        if let Some(parent) = node.parent() {
            if parent.kind() == "method_call_expression" {
                if let Some(inv) = parent.child_by_field_name("invocant") {
                    if inv.id() == node.id() {
                        refs.push(node_to_span(node));
                    }
                }
            }
        }
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

    match &cursor_sym {
        CursorSymbol::HashKey { ref key_name, ref owner } => {
            let keys = collect_hash_keys(tree.root_node(), source_bytes, owner, point);
            let edits: Vec<RenameEdit> = keys
                .into_iter()
                .filter(|k| k.key_name == *key_name)
                .map(|k| RenameEdit {
                    span: k.span,
                    new_text: new_name.to_string(),
                })
                .collect();
            if edits.is_empty() { return None; }
            return Some(edits);
        }
        _ => {}
    }

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
                let class_name = if inv_text.starts_with('$')
                    || inv_text.starts_with('@')
                    || inv_text.starts_with('%')
                {
                    infer_variable_class(tree.root_node(), source_bytes, inv_text, point)
                } else {
                    Some(inv_text.clone())
                };

                if let Some(ref cn) = class_name {
                    if let Some(span) = find_method_in_class(
                        tree.root_node(),
                        source_bytes,
                        cn,
                        name,
                    ) {
                        let line = source.lines().nth(span.start.row).unwrap_or("");
                        return Some(HoverResult {
                            markdown: format!(
                                "**{}**\n```perl\n{}\n```",
                                cn,
                                line.trim()
                            ),
                        });
                    }
                    // Auto-generated method like "new" → show class info
                    return Some(HoverResult {
                        markdown: format!("**{}**::`{}`", cn, name),
                    });
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
        CursorSymbol::HashKey { ref key_name, ref owner } => {
            let keys = collect_hash_keys(tree.root_node(), source_bytes, owner, point);
            let matching: Vec<&HashKeyInfo> = keys.iter()
                .filter(|k| k.key_name == *key_name && !k.is_dynamic)
                .collect();
            let count = matching.len();
            let owner_str = match owner {
                HashKeyOwner::Class(ref cn) => format!("{}->", cn),
                HashKeyOwner::Variable { ref name, .. } => format!("{}", name),
            };
            let first_line = matching.first().and_then(|k| {
                source.lines().nth(k.span.start.row)
            }).unwrap_or("");
            format!(
                "`{}{{{}}}`  \n{} occurrence{}\n```perl\n{}\n```",
                owner_str,
                key_name,
                count,
                if count == 1 { "" } else { "s" },
                first_line.trim()
            )
        }
    };

    Some(HoverResult { markdown })
}

// ---- Hash key analysis ----

/// Extract the key text from a hash key node.
/// Returns (key_text, is_dynamic).
fn extract_key_text(node: Node, source: &[u8]) -> Option<(String, bool)> {
    match node.kind() {
        "autoquoted_bareword" => {
            node.utf8_text(source).ok().map(|s| (s.to_string(), false))
        }
        "string_literal" | "interpolated_string_literal" => {
            // Look for string_content child
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    if child.kind() == "string_content" {
                        return child.utf8_text(source).ok().map(|s| (s.to_string(), false));
                    }
                }
            }
            // Empty string literal
            Some(("".to_string(), false))
        }
        "string_content" => {
            node.utf8_text(source).ok().map(|s| (s.to_string(), false))
        }
        // Dynamic key: $var, @arr, expression, etc.
        "scalar" | "array" | "hash" | "function_call_expression"
        | "method_call_expression" | "container_variable" => {
            node.utf8_text(source).ok().map(|s| (s.to_string(), true))
        }
        _ => {
            // Unknown node kind — treat as dynamic if it has text
            node.utf8_text(source).ok().map(|s| (s.to_string(), true))
        }
    }
}

/// Get the hash variable node from a hash_element_expression.
/// Handles both `$hash{key}` (where hash is a field) and `$self->{key}` (where it's not).
fn get_hash_from_element(he: Node, source: &[u8]) -> Option<String> {
    // Try the `hash` field first (works for $hash{key} style)
    if let Some(hash_node) = he.child_by_field_name("hash") {
        return resolve_hash_element_var(hash_node, source);
    }
    // For arrow syntax $obj->{key}: the scalar/variable is the first named child
    for i in 0..he.named_child_count() {
        if let Some(child) = he.named_child(i) {
            match child.kind() {
                "scalar" | "array" | "hash" | "container_variable"
                | "slice_container_variable" | "keyval_container_variable"
                | "hash_element_expression" => {
                    return resolve_hash_element_var(child, source);
                }
                _ => continue,
            }
        }
    }
    None
}

/// Given a hash variable node, resolve to its canonical name.
fn resolve_hash_element_var(node: Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "scalar" | "array" | "hash" => {
            node.utf8_text(source).ok().map(|s| s.to_string())
        }
        "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
            canonical_var_name(node, source)
        }
        "hash_element_expression" => {
            // Nested: $self->{a}{b} — resolve from the inner hash
            get_hash_from_element(node, source)
        }
        _ => node.utf8_text(source).ok().map(|s| s.to_string()),
    }
}

/// Determine the HashKeyOwner for a variable used as a hash.
fn resolve_key_owner(
    root: Node,
    source: &[u8],
    var_text: &str,
    point: Point,
) -> Option<HashKeyOwner> {
    // First check if the variable can be traced to a class
    if let Some(class_name) = infer_variable_class(root, source, var_text, point) {
        return Some(HashKeyOwner::Class(class_name));
    }

    // Check first-param-is-instance heuristic
    if let Some(class_name) = infer_first_param_class(root, source, var_text, point) {
        return Some(HashKeyOwner::Class(class_name));
    }

    // Fall back to plain variable
    let def_span = find_variable_def(root, source, var_text, point)
        .unwrap_or(Span {
            start: Point::new(0, 0),
            end: Point::new(0, 0),
        });
    Some(HashKeyOwner::Variable {
        name: var_text.to_string(),
        def_span,
    })
}

/// Check if a variable is the first parameter of an enclosing sub/method in a package.
fn infer_first_param_class(
    root: Node,
    source: &[u8],
    var_text: &str,
    point: Point,
) -> Option<String> {
    let mut node = root.named_descendant_for_point_range(point, point)?;
    let mut sub_node: Option<Node> = None;
    for _ in 0..30 {
        if node.kind() == "subroutine_declaration_statement"
            || node.kind() == "method_declaration_statement"
        {
            sub_node = Some(node);
            break;
        }
        node = node.parent()?;
    }
    let sub_node = sub_node?;

    let first_param = get_first_param(sub_node, source)?;
    if first_param != var_text {
        return None;
    }

    find_enclosing_package(sub_node, source)
}

/// Get the first parameter name of a sub/method.
fn get_first_param(sub_node: Node, source: &[u8]) -> Option<String> {
    // Check signature first
    for i in 0..sub_node.child_count() {
        if let Some(sig) = sub_node.child(i) {
            if sig.kind() == "signature" {
                for j in 0..sig.named_child_count() {
                    if let Some(param) = sig.named_child(j) {
                        if matches!(param.kind(), "scalar" | "array" | "hash") {
                            return param.utf8_text(source).ok().map(|s| s.to_string());
                        }
                    }
                }
            }
        }
    }

    // Check for `my ($self, ...) = @_` in the body
    if let Some(body) = sub_node.child_by_field_name("body") {
        for i in 0..body.named_child_count() {
            if let Some(stmt) = body.named_child(i) {
                let assign = if stmt.kind() == "expression_statement" {
                    stmt.named_child(0).filter(|n| n.kind() == "assignment_expression")
                } else if stmt.kind() == "assignment_expression" {
                    Some(stmt)
                } else {
                    None
                };
                if let Some(assign) = assign {
                    if let Some(right) = assign.child_by_field_name("right") {
                        if right.utf8_text(source).ok() == Some("@_") {
                            if let Some(left) = assign.child_by_field_name("left") {
                                let mut vars = Vec::new();
                                collect_declared_vars(left, source, &mut vars);
                                if let Some((name, _)) = vars.first() {
                                    return Some(name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Find the enclosing package name for a node.
fn find_enclosing_package(node: Node, source: &[u8]) -> Option<String> {
    let mut current = node;
    for _ in 0..30 {
        if let Some(parent) = current.parent() {
            if parent.kind() == "class_statement" {
                return parent.child_by_field_name("name")
                    .and_then(|n| n.utf8_text(source).ok())
                    .map(|s| s.to_string());
            }
            current = parent;
        } else {
            break;
        }
    }

    // No class_statement — look for the most recent package_statement before this node
    let source_file = current;
    let mut last_package: Option<String> = None;
    for i in 0..source_file.child_count() {
        if let Some(child) = source_file.child(i) {
            if child.start_position() > node.start_position() {
                break;
            }
            if child.kind() == "package_statement" {
                if let Some(name) = child.child_by_field_name("name") {
                    if let Ok(text) = name.utf8_text(source) {
                        last_package = Some(text.to_string());
                    }
                }
            }
        }
    }
    last_package
}

/// Collect all hash keys belonging to a specific owner.
fn collect_hash_keys(
    root: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    context_point: Point,
) -> Vec<HashKeyInfo> {
    let mut keys = Vec::new();
    collect_hash_keys_walk(root, source, owner, context_point, &mut keys);
    keys.sort_by(|a, b| {
        a.span.start.row.cmp(&b.span.start.row)
            .then(a.span.start.column.cmp(&b.span.start.column))
    });
    keys.dedup_by(|a, b| a.key_name == b.key_name && a.span == b.span);
    keys
}

fn collect_hash_keys_walk(
    node: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    context_point: Point,
    keys: &mut Vec<HashKeyInfo>,
) {
    if node.kind() == "hash_element_expression" {
        if let Some(key_node) = node.child_by_field_name("key") {
            if let Some(var_text) = get_hash_from_element(node, source) {
                let resolved_owner = resolve_key_owner(
                    find_root(node),
                    source,
                    &var_text,
                    node.start_position(),
                );
                if let Some(ref ro) = resolved_owner {
                    if owners_match(ro, owner) {
                        if let Some((key_text, is_dynamic)) = extract_key_text(key_node, source) {
                            keys.push(HashKeyInfo {
                                key_name: key_text.clone(),
                                owner: ro.clone(),
                                span: node_to_span(key_node),
                                is_dynamic,
                                source_text: key_node.utf8_text(source)
                                    .unwrap_or("")
                                    .to_string(),
                            });
                        }
                    }
                }
            }
        }
    }

    if node.kind() == "anonymous_hash_expression" {
        collect_construction_keys(node, source, owner, context_point, keys);
    }

    if node.kind() == "assignment_expression" {
        collect_assignment_construction_keys(node, source, owner, context_point, keys);
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_hash_keys_walk(child, source, owner, context_point, keys);
        }
    }
}

fn collect_construction_keys(
    anon_hash: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    context_point: Point,
    keys: &mut Vec<HashKeyInfo>,
) {
    let construction_owner = resolve_anon_hash_owner(anon_hash, source, context_point);
    if let Some(ref co) = construction_owner {
        if !owners_match(co, owner) {
            return;
        }
    } else {
        return;
    }

    for i in 0..anon_hash.named_child_count() {
        if let Some(list) = anon_hash.named_child(i) {
            if list.kind() == "list_expression" {
                collect_list_keys(list, source, construction_owner.as_ref().unwrap(), keys);
            }
        }
    }
}

fn collect_assignment_construction_keys(
    assign: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    _context_point: Point,
    keys: &mut Vec<HashKeyInfo>,
) {
    let left = match assign.child_by_field_name("left") {
        Some(l) => l,
        None => return,
    };

    // Find the actual right-side content. child_by_field_name("right") may return
    // a parenthesis when multiple children share the [right] field label.
    // Scan all named children on the right side for list_expression or anonymous_hash_expression.
    let hash_var = find_hash_var_in_lhs(left, source);
    if let Some(ref var_text) = hash_var {
        let resolved = resolve_key_owner(
            find_root(assign),
            source,
            var_text,
            assign.start_position(),
        );
        if let Some(ref ro) = resolved {
            if owners_match(ro, owner) {
                // Find list_expression or anonymous_hash_expression among children
                for i in 0..assign.named_child_count() {
                    if let Some(child) = assign.named_child(i) {
                        if child.kind() == "list_expression" {
                            collect_list_keys(child, source, ro, keys);
                            return;
                        }
                        if child.kind() == "anonymous_hash_expression" {
                            if let Some(list) = child.named_child(0) {
                                if list.kind() == "list_expression" {
                                    collect_list_keys(list, source, ro, keys);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn collect_list_keys(
    list: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    keys: &mut Vec<HashKeyInfo>,
) {
    // Use fat-comma detection: any autoquoted_bareword or string followed by =>
    // is a key. This handles nested list_expressions reliably.
    collect_fat_comma_keys_walk(list, source, owner, keys);
}

fn collect_fat_comma_keys_walk(
    node: Node,
    source: &[u8],
    owner: &HashKeyOwner,
    keys: &mut Vec<HashKeyInfo>,
) {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            // Check if this child is followed by =>
            if child.is_named() {
                let next_sibling = node.child(i + 1);
                let is_before_fat_comma = next_sibling
                    .map_or(false, |ns| ns.kind() == "=>");
                if is_before_fat_comma {
                    if let Some((key_text, is_dynamic)) = extract_key_text(child, source) {
                        keys.push(HashKeyInfo {
                            key_name: key_text,
                            owner: owner.clone(),
                            span: node_to_span(child),
                            is_dynamic,
                            source_text: child.utf8_text(source).unwrap_or("").to_string(),
                        });
                    }
                }
            }
            // Recurse into nested list_expressions
            if child.kind() == "list_expression" {
                collect_fat_comma_keys_walk(child, source, owner, keys);
            }
        }
    }
}

fn find_hash_var_in_lhs(left: Node, source: &[u8]) -> Option<String> {
    if left.kind() == "hash" {
        return left.utf8_text(source).ok().map(|s| s.to_string());
    }
    if left.kind() == "variable_declaration" {
        let mut vars = Vec::new();
        collect_declared_vars(left, source, &mut vars);
        for (name, _) in vars {
            if name.starts_with('%') {
                return Some(name);
            }
        }
    }
    None
}

fn is_bless_call(node: Node, source: &[u8]) -> bool {
    let kind = node.kind();
    if kind == "function_call_expression" || kind == "ambiguous_function_call_expression" {
        if let Some(func) = node.child_by_field_name("function") {
            return func.utf8_text(source).ok() == Some("bless");
        }
    }
    false
}

fn resolve_anon_hash_owner(
    anon_hash: Node,
    source: &[u8],
    _context_point: Point,
) -> Option<HashKeyOwner> {
    let parent = anon_hash.parent()?;

    // Direct child of bless call
    if is_bless_call(parent, source) {
        if let Some(pkg) = find_enclosing_package(anon_hash, source) {
            return Some(HashKeyOwner::Class(pkg));
        }
    }

    // Walk up a few levels to find bless (handles list_expression wrappers)
    let mut ancestor = parent;
    for _ in 0..5 {
        if is_bless_call(ancestor, source) {
            if let Some(pkg) = find_enclosing_package(anon_hash, source) {
                return Some(HashKeyOwner::Class(pkg));
            }
        }
        ancestor = match ancestor.parent() {
            Some(p) => p,
            None => break,
        };
    }

    // Assigned to a variable: my $ref = { ... }
    // Walk up to find assignment_expression
    let mut current = anon_hash;
    for _ in 0..5 {
        if let Some(p) = current.parent() {
            if p.kind() == "assignment_expression" {
                if let Some(left) = p.child_by_field_name("left") {
                    let var_text = get_var_text_from_lhs(left, source);
                    if let Some(vt) = var_text {
                        return resolve_key_owner(
                            find_root(anon_hash),
                            source,
                            &vt,
                            p.start_position(),
                        );
                    }
                }
            }
            current = p;
        } else {
            break;
        }
    }

    None
}

fn get_var_text_from_lhs(left: Node, source: &[u8]) -> Option<String> {
    if matches!(left.kind(), "scalar" | "array" | "hash") {
        return left.utf8_text(source).ok().map(|s| s.to_string());
    }
    if left.kind() == "variable_declaration" {
        let mut vars = Vec::new();
        collect_declared_vars(left, source, &mut vars);
        if let Some((name, _)) = vars.first() {
            return Some(name.clone());
        }
    }
    None
}

fn find_root(node: Node) -> Node {
    let mut current = node;
    while let Some(parent) = current.parent() {
        current = parent;
    }
    current
}

/// Determine the owner for a key node inside a list_expression (hash construction).
/// Checks if the node is at a key position and if the list is in a hash context.
fn resolve_list_key_owner(
    key_node: Node,
    list_node: Node,
    source: &[u8],
    point: Point,
) -> Option<HashKeyOwner> {
    // Verify this node is at a key position (even index among named children)
    let mut idx = 0;
    let mut is_key = false;
    for i in 0..list_node.child_count() {
        if let Some(child) = list_node.child(i) {
            if !child.is_named() {
                continue;
            }
            if child.id() == key_node.id() {
                is_key = idx % 2 == 0;
                break;
            }
            idx += 1;
        }
    }
    if !is_key {
        return None;
    }

    // Check if the list is in a hash context — walk up a few levels
    let mut ancestor = list_node;
    for _ in 0..5 {
        let parent = match ancestor.parent() {
            Some(p) => p,
            None => return None,
        };

        if parent.kind() == "anonymous_hash_expression" {
            return resolve_anon_hash_owner(parent, source, point);
        }

        if parent.kind() == "assignment_expression" {
            if let Some(left) = parent.child_by_field_name("left") {
                if let Some(hash_var) = find_hash_var_in_lhs(left, source) {
                    return resolve_key_owner(
                        find_root(list_node),
                        source,
                        &hash_var,
                        parent.start_position(),
                    );
                }
            }
        }

        ancestor = parent;
    }

    None
}

fn owners_match(a: &HashKeyOwner, b: &HashKeyOwner) -> bool {
    match (a, b) {
        (HashKeyOwner::Class(ref ca), HashKeyOwner::Class(ref cb)) => ca == cb,
        (HashKeyOwner::Variable { ref name, ref def_span },
         HashKeyOwner::Variable { name: ref name2, def_span: ref def_span2 }) => {
            name == name2 && def_span == def_span2
        }
        _ => false,
    }
}

// ---- Keyval arg completion ----

/// Count both `,` and `=>` separators before cursor at top-level depth.
/// Even = key position, odd = value position.
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
fn collect_used_keys_at_callsite(args: Node, source: &[u8]) -> std::collections::HashSet<String> {
    let mut used = std::collections::HashSet::new();
    for i in 0..args.child_count() {
        if let Some(child) = args.child(i) {
            // Check if next sibling is `=>`
            if let Some(next) = child.next_sibling() {
                if next.kind() == "=>" {
                    if let Some((key, _)) = extract_key_text(child, source) {
                        used.insert(key);
                    }
                }
            }
        }
    }
    used
}

/// Collect hash keys accessed via a specific base name inside a subtree.
/// E.g., for base_name "args", finds keys from `$args{host}`, `$args{port}`, etc.
fn collect_hash_keys_for_base_name(
    node: Node,
    source: &[u8],
    base_name: &str,
    keys: &mut Vec<String>,
) {
    if node.kind() == "hash_element_expression" {
        if let Some(var_text) = get_hash_from_element(node, source) {
            // Check if the base name matches (strip sigil)
            let var_base = get_varname_from_text(&var_text);
            if var_base == base_name {
                if let Some(key_node) = node.child_by_field_name("key") {
                    if let Some((key_text, is_dynamic)) = extract_key_text(key_node, source) {
                        if !is_dynamic {
                            keys.push(key_text);
                        }
                    }
                }
            }
        }
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_hash_keys_for_base_name(child, source, base_name, keys);
        }
    }
}

/// Extract the bare variable name from a text like "$args", "%opts", "@list".
fn get_varname_from_text(text: &str) -> &str {
    if text.starts_with('$') || text.starts_with('@') || text.starts_with('%') {
        &text[1..]
    } else {
        text
    }
}

/// Try to offer keyval arg completions if we're at a call site in key position.
fn collect_keyval_arg_completions(
    tree: &Tree,
    source: &[u8],
    point: Point,
) -> Vec<CompletionCandidate> {
    let (name, is_method, invocant, args_node) =
        match find_call_at_cursor(tree, source, point) {
            Some(t) => t,
            None => return Vec::new(),
        };

    // Check key position: even separator count = key, odd = value
    if let Some(ref args) = args_node {
        let sep_count = count_separators_before(*args, point);
        if sep_count % 2 != 0 {
            return Vec::new(); // value position
        }
    }

    let root = tree.root_node();

    // Resolve the class name for scoped lookup
    let class_name = if is_method {
        invocant.as_deref().and_then(|inv| {
            // Bareword invocant = class name directly (e.g. Calculator->new)
            if !inv.starts_with('$') { return Some(inv); }
            // Variable invocant = try to infer class from bless pattern
            infer_class_from_variable(root, source, inv)
        }).map(|s| s.to_string())
    } else {
        None
    };

    // For core class constructors (class Foo { field $x :param; }),
    // collect :param fields — there's no explicit sub new
    if name == "new" {
        if let Some(ref cn) = class_name {
            if let Some(candidates) = collect_class_param_completions(root, source, cn, &args_node) {
                if !candidates.is_empty() {
                    return candidates;
                }
            }
        }
    }

    // Find the sub definition, scoped to class if known
    let sub_def = if let Some(ref cn) = class_name {
        find_sub_def_in_scope(root, source, &name, cn)
    } else {
        find_sub_def(root, source, &name)
    };

    let (sub_span, _) = match sub_def {
        Some(s) => s,
        None => return Vec::new(),
    };
    let sub_node = match root
        .descendant_for_point_range(sub_span.start, sub_span.start)
        .and_then(find_sub_or_method_node)
    {
        Some(n) => n,
        None => return Vec::new(),
    };

    let params = extract_sub_params(sub_node, source);

    // Find slurpy %hash param
    let slurpy = params
        .iter()
        .find(|p| p.is_slurpy && p.name.starts_with('%'));
    let slurpy_name = match slurpy {
        Some(p) => get_varname_from_text(&p.name),
        None => return Vec::new(),
    };

    // Collect keys accessed in the sub body
    let body = match sub_node.child_by_field_name("body") {
        Some(b) => b,
        None => return Vec::new(),
    };
    let mut keys = Vec::new();
    collect_hash_keys_for_base_name(body, source, slurpy_name, &mut keys);

    // Deduplicate
    keys.sort();
    keys.dedup();

    // Filter out already-typed keys at the call site
    let used = args_node
        .map(|a| collect_used_keys_at_callsite(a, source))
        .unwrap_or_default();

    keys.into_iter()
        .filter(|k| !used.contains(k))
        .map(|k| CompletionCandidate {
            label: format!("{} =>", k),
            kind: SymbolKind::Variable,
            detail: Some(format!("{}(%{})", name, slurpy_name)),
            insert_text: Some(format!("{} => ", k)),
            sort_priority: 0,
        })
        .collect()
}

/// Collect :param field names from a core class as keyval completions.
fn collect_class_param_completions(
    root: Node,
    source: &[u8],
    class_name: &str,
    args_node: &Option<Node>,
) -> Option<Vec<CompletionCandidate>> {
    let class_node = find_class_node(root, source, class_name)?;
    let class_info = parse_class_node(class_node, source)?;
    let param_fields: Vec<&FieldInfo> = class_info
        .fields
        .iter()
        .filter(|f| f.attributes.contains(&"param".to_string()))
        .collect();
    if param_fields.is_empty() {
        return None;
    }
    let used = match args_node {
        Some(a) => collect_used_keys_at_callsite(*a, source),
        None => std::collections::HashSet::new(),
    };
    let candidates = param_fields
        .into_iter()
        .map(|f| f.name.trim_start_matches('$').trim_start_matches('@').trim_start_matches('%').to_string())
        .filter(|k| !used.contains(k))
        .map(|k| CompletionCandidate {
            label: format!("{} =>", k),
            kind: SymbolKind::Variable,
            detail: Some(format!("{}->new(:param)", class_name)),
            insert_text: Some(format!("{} => ", k)),
            sort_priority: 0,
        })
        .collect();
    Some(candidates)
}

/// Find a class_statement node by name.
fn find_class_node<'a>(node: Node<'a>, source: &[u8], name: &str) -> Option<Node<'a>> {
    if node.kind() == "class_statement" {
        if let Some(name_node) = node.child_by_field_name("name") {
            if name_node.utf8_text(source).ok() == Some(name) {
                return Some(node);
            }
        }
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(found) = find_class_node(child, source, name) {
                return Some(found);
            }
        }
    }
    None
}

/// Find a sub/method definition scoped to a specific package or class.
fn find_sub_def_in_scope(root: Node, source: &[u8], sub_name: &str, scope_name: &str) -> Option<(Span, Span)> {
    find_sub_def_in_scope_walk(root, source, sub_name, scope_name, &mut None)
}

fn find_sub_def_in_scope_walk(
    node: Node,
    source: &[u8],
    sub_name: &str,
    scope_name: &str,
    current_package: &mut Option<String>,
) -> Option<(Span, Span)> {
    match node.kind() {
        "package_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(text) = name_node.utf8_text(source) {
                    *current_package = Some(text.to_string());
                }
            }
        }
        "class_statement" => {
            let class_name = node.child_by_field_name("name")
                .and_then(|n| n.utf8_text(source).ok());
            if class_name == Some(scope_name) {
                // Search within this class block
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        if child.kind() == "block" {
                            if let Some(result) = find_sub_def_walk(child, source, sub_name) {
                                return Some(result);
                            }
                        }
                    }
                }
            }
            return None; // Don't recurse into other classes
        }
        "subroutine_declaration_statement" | "method_declaration_statement" => {
            if current_package.as_deref() == Some(scope_name) {
                if let Some(name_node) = node.child_by_field_name("name") {
                    if name_node.utf8_text(source).ok() == Some(sub_name) {
                        return Some((node_to_span(node), node_to_span(name_node)));
                    }
                }
            }
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if child.kind() == "class_statement" {
                // Handle class separately (don't inherit current_package into it)
                let class_name = child.child_by_field_name("name")
                    .and_then(|n| n.utf8_text(source).ok());
                if class_name == Some(scope_name) {
                    for j in 0..child.child_count() {
                        if let Some(block) = child.child(j) {
                            if block.kind() == "block" {
                                if let Some(result) = find_sub_def_walk(block, source, sub_name) {
                                    return Some(result);
                                }
                            }
                        }
                    }
                }
                continue;
            }
            if let Some(result) = find_sub_def_in_scope_walk(child, source, sub_name, scope_name, current_package) {
                return Some(result);
            }
        }
    }
    None
}

/// Try to infer the class name from a variable's bless pattern.
fn infer_class_from_variable<'a>(root: Node, source: &[u8], var_text: &str) -> Option<&'a str> {
    // TODO: trace $var back to bless { }, $class or ClassName->new(...)
    // For now, return None — variables fall back to global sub search
    let _ = (root, source, var_text);
    None
}

// ---- Completion ----

pub fn collect_completions(tree: &Tree, source: &str, point: Point) -> Vec<CompletionCandidate> {
    let source_bytes = source.as_bytes();
    let mode = detect_completion_mode(source, point);

    match mode {
        CompletionMode::Variable { sigil } => {
            collect_variable_completions(tree.root_node(), source_bytes, point, sigil)
        }
        CompletionMode::Method { ref invocant } => {
            collect_method_completions(tree, source_bytes, invocant, point)
        }
        CompletionMode::HashKey { ref var_text } => {
            collect_hash_key_completions(tree, source_bytes, var_text, point)
        }
        CompletionMode::General => {
            let mut candidates = Vec::new();
            // Keyval arg completions (if inside a call at key position)
            candidates.extend(collect_keyval_arg_completions(tree, source_bytes, point));
            // Variables (all sigils)
            for sigil in ['$', '@', '%'] {
                candidates.extend(collect_variable_completions(
                    tree.root_node(),
                    source_bytes,
                    point,
                    sigil,
                ));
            }
            // Subs
            candidates.extend(collect_all_subs(tree.root_node(), source_bytes));
            // Package/class names
            candidates.extend(collect_all_packages(tree.root_node(), source_bytes));
            candidates
        }
    }
}

fn detect_completion_mode(source: &str, point: Point) -> CompletionMode {
    let line = match source.lines().nth(point.row) {
        Some(l) => l,
        None => return CompletionMode::General,
    };
    let before = if point.column <= line.len() {
        &line[..point.column]
    } else {
        line
    };
    let trimmed = before.trim_end();

    // Check for hash key completion: $hash{ or $self->{
    if trimmed.ends_with('{') {
        let prefix = trimmed[..trimmed.len() - 1].trim_end();
        // Arrow hash access: $var->{
        if prefix.ends_with("->") {
            let var_prefix = prefix[..prefix.len() - 2].trim_end();
            let var_text = extract_invocant_from_prefix(var_prefix);
            if !var_text.is_empty() {
                return CompletionMode::HashKey {
                    var_text: var_text.to_string(),
                };
            }
        }
        // Direct hash access: $hash{ or %hash to $hash{
        let var_text = extract_invocant_from_prefix(prefix);
        if !var_text.is_empty() && var_text.starts_with('$') {
            // $hash{ → the hash is %hash (container variable)
            return CompletionMode::HashKey {
                var_text: var_text.to_string(),
            };
        }
    }

    // Check for -> (method completion)
    if trimmed.ends_with("->") {
        // Extract the invocant: everything before -> that looks like a variable or bareword
        let prefix = trimmed[..trimmed.len() - 2].trim_end();
        // Find the invocant token (last word/variable)
        let invocant = extract_invocant_from_prefix(prefix);
        if !invocant.is_empty() {
            return CompletionMode::Method {
                invocant: invocant.to_string(),
            };
        }
    }

    // Check for sigil trigger
    if let Some(last_char) = trimmed.chars().last() {
        if matches!(last_char, '$' | '@' | '%') {
            return CompletionMode::Variable { sigil: last_char };
        }
    }

    CompletionMode::General
}

fn extract_invocant_from_prefix(prefix: &str) -> &str {
    // Walk backwards to find the start of the invocant token
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
            // Include the sigil
            return &prefix[i..end];
        }
        // Not a valid invocant character — stop
        return &prefix[i + 1..end];
    }
    &prefix[..end]
}

fn collect_variable_completions(
    root: Node,
    source: &[u8],
    point: Point,
    sigil: char,
) -> Vec<CompletionCandidate> {
    let mut raw: Vec<(String, char, Option<String>, usize)> = Vec::new(); // (bare_name, decl_sigil, detail, scope_size)
    collect_in_scope_vars(root, source, point, &mut raw);

    // Deduplicate by bare name, keeping innermost scope (smallest scope_size)
    // Dedup by (bare_name, sigil) — $args and @args are different variables
    let mut seen = std::collections::HashSet::<(String, char)>::new();
    let mut deduped = Vec::new();
    // Sort by scope_size ascending so we process innermost first
    raw.sort_by_key(|(_, _, _, sz)| *sz);
    for (bare_name, decl_sigil, detail, scope_size) in raw {
        let key = (bare_name.clone(), decl_sigil);
        if seen.contains(&key) {
            continue;
        }
        seen.insert(key);
        deduped.push((bare_name, decl_sigil, detail, scope_size));
    }

    // Generate completion candidates with cross-sigil forms
    let mut candidates = Vec::new();
    for (bare_name, decl_sigil, detail, scope_size) in deduped {
        let priority = std::cmp::min(scope_size, 255) as u8;

        match sigil {
            '$' => {
                // Always offer $name for scalar-declared vars
                if decl_sigil == '$' {
                    candidates.push(CompletionCandidate {
                        label: format!("${}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone(),
                        insert_text: Some(bare_name.clone()),
                        sort_priority: priority,
                    });
                }
                // $arrayname[ for @array element access
                if decl_sigil == '@' {
                    candidates.push(CompletionCandidate {
                        label: format!("${}[]", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone().or(Some(format!("@{}", bare_name))),
                        insert_text: Some(format!("{}[", bare_name)),
                        sort_priority: priority,
                    });
                    // $#arrayname for last index
                    candidates.push(CompletionCandidate {
                        label: format!("$#{}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone().or(Some(format!("last index of @{}", bare_name))),
                        insert_text: Some(format!("#{}", bare_name)),
                        sort_priority: priority.saturating_add(1),
                    });
                }
                // $hashname{ for %hash
                if decl_sigil == '%' {
                    candidates.push(CompletionCandidate {
                        label: format!("${}{{}}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone().or(Some(format!("%{}", bare_name))),
                        insert_text: Some(format!("{}{{", bare_name)),
                        sort_priority: priority,
                    });
                }
            }
            '@' => {
                // @name for arrays
                if decl_sigil == '@' {
                    candidates.push(CompletionCandidate {
                        label: format!("@{}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone(),
                        insert_text: Some(bare_name.clone()),
                        sort_priority: priority,
                    });
                }
                // @arrayname[] for array slice
                if decl_sigil == '@' {
                    candidates.push(CompletionCandidate {
                        label: format!("@{}[]", bare_name),
                        kind: SymbolKind::Variable,
                        detail: Some("array slice".to_string()),
                        insert_text: Some(format!("{}[", bare_name)),
                        sort_priority: priority.saturating_add(1),
                    });
                }
                // @hashname{} for hash slice
                if decl_sigil == '%' {
                    candidates.push(CompletionCandidate {
                        label: format!("@{}{{}}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone().or(Some("hash slice".to_string())),
                        insert_text: Some(format!("{}{{", bare_name)),
                        sort_priority: priority,
                    });
                }
            }
            '%' => {
                // %name for hashes
                if decl_sigil == '%' {
                    candidates.push(CompletionCandidate {
                        label: format!("%{}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: detail.clone(),
                        insert_text: Some(bare_name.clone()),
                        sort_priority: priority,
                    });
                }
                // %arrayname[] for array key/value slice
                if decl_sigil == '@' {
                    candidates.push(CompletionCandidate {
                        label: format!("%{}[]", bare_name),
                        kind: SymbolKind::Variable,
                        detail: Some("array kv slice".to_string()),
                        insert_text: Some(format!("{}[", bare_name)),
                        sort_priority: priority,
                    });
                }
                // %hashname{} for hash key/value slice
                if decl_sigil == '%' {
                    candidates.push(CompletionCandidate {
                        label: format!("%{}{{}}", bare_name),
                        kind: SymbolKind::Variable,
                        detail: Some("hash kv slice".to_string()),
                        insert_text: Some(format!("{}{{", bare_name)),
                        sort_priority: priority.saturating_add(1),
                    });
                }
            }
            _ => {}
        }
    }

    candidates
}

fn collect_in_scope_vars(
    node: Node,
    source: &[u8],
    point: Point,
    out: &mut Vec<(String, char, Option<String>, usize)>, // (bare_name, sigil, detail, scope_size)
) {
    // variable_declaration: check scope contains point
    if node.kind() == "variable_declaration" {
        if node.start_position() <= point {
            let keyword = get_decl_keyword(node, source).unwrap_or("my");
            let scope = enclosing_scope(node);
            let scope_end = scope.end_position();
            if scope_end >= point {
                let scope_size = scope_end.row - scope.start_position().row;
                let mut vars = Vec::new();
                collect_declared_vars(node, source, &mut vars);
                for (name, _) in vars {
                    if let Some(sigil) = name.chars().next() {
                        let bare = name[1..].to_string();
                        out.push((bare, sigil, Some(keyword.to_string()), scope_size));
                    }
                }
            }
        }
    }

    // Signature params: scoped to the sub body
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(body) = node.child_by_field_name("body") {
            let body_end = body.end_position();
            if body_end >= point && body.start_position() <= point {
                let scope_size = body_end.row - body.start_position().row;
                for i in 0..node.child_count() {
                    if let Some(sig) = node.child(i) {
                        if sig.kind() == "signature" {
                            let mut vars = Vec::new();
                            collect_declared_vars(sig, source, &mut vars);
                            for (name, _) in vars {
                                if let Some(sigil) = name.chars().next() {
                                    let bare = name[1..].to_string();
                                    out.push((bare, sigil, Some("param".to_string()), scope_size));
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    // for-loop variable
    if node.kind() == "for_statement" {
        if let Some(var_node) = node.child_by_field_name("variable") {
            if var_node.start_position() <= point {
                if let Some(block) = node.child_by_field_name("block") {
                    let block_end = block.end_position();
                    if block_end >= point {
                        let scope_size = block_end.row - block.start_position().row;
                        if let Ok(name) = var_node.utf8_text(source) {
                            if let Some(sigil) = name.chars().next() {
                                let bare = name[1..].to_string();
                                out.push((bare, sigil, Some("for".to_string()), scope_size));
                            }
                        }
                    }
                }
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_in_scope_vars(child, source, point, out);
        }
    }
}

fn collect_hash_key_completions(
    tree: &Tree,
    source: &[u8],
    var_text: &str,
    point: Point,
) -> Vec<CompletionCandidate> {
    let root = tree.root_node();

    // Try multiple resolution strategies since the cursor line may be incomplete.
    // For direct hash access ($hash{), the actual hash is %hash — try that first.
    // Also scan existing usages in the file to resolve from a complete expression.
    let bare_name = &var_text[1..]; // strip sigil

    // First: try resolving from existing hash usages elsewhere in the file.
    let mut owner = resolve_owner_from_existing_usages(root, source, bare_name);

    // Second: try class inference from first-param scan (handles incomplete source
    // where tree-sitter wraps subs in ERROR nodes). This is prioritized over plain
    // variable resolution because $self->{} should resolve to a class, not a Variable.
    if owner.is_none() {
        owner = infer_class_from_first_param_scan(root, source, var_text);
    }

    // Third: try direct resolution with both sigil forms (e.g. $hash → %hash)
    if owner.is_none() {
        let candidates: Vec<String> = if var_text.starts_with('$') {
            vec![format!("%{}", bare_name), var_text.to_string()]
        } else {
            vec![var_text.to_string()]
        };
        for candidate in &candidates {
            let resolved = resolve_key_owner(root, source, &candidate, point);
            if let Some(ref o) = resolved {
                // Only accept if it found a real definition (not fallback zero span)
                if let HashKeyOwner::Variable { ref def_span, .. } = o {
                    if def_span.start == Point::new(0, 0) && def_span.end == Point::new(0, 0) {
                        continue;
                    }
                }
                owner = resolved;
                break;
            }
        }
    }

    let owner = match owner {
        Some(o) => o,
        None => return Vec::new(),
    };

    let all_keys = collect_hash_keys(root, source, &owner, point);

    // Deduplicate by key name, keeping first occurrence
    let mut seen = std::collections::HashSet::new();
    let mut candidates = Vec::new();
    for key in &all_keys {
        if seen.contains(&key.key_name) {
            continue;
        }
        seen.insert(key.key_name.clone());

        let detail = if key.is_dynamic {
            Some(format!("(dynamic: {})", key.source_text))
        } else {
            match &owner {
                HashKeyOwner::Class(name) => Some(format!("{}->{{{}}}", name, key.key_name)),
                HashKeyOwner::Variable { name, .. } => {
                    Some(format!("{}{{{}}}", name, key.key_name))
                }
            }
        };

        candidates.push(CompletionCandidate {
            label: key.key_name.clone(),
            kind: SymbolKind::Variable,
            detail,
            insert_text: None,
            sort_priority: if key.is_dynamic { 50 } else { 10 },
        });
    }

    candidates
}

/// Find an owner by scanning existing hash_element_expression nodes for the variable.
fn resolve_owner_from_existing_usages(
    root: Node,
    source: &[u8],
    bare_name: &str,
) -> Option<HashKeyOwner> {
    resolve_owner_from_usages_walk(root, source, bare_name)
}

fn resolve_owner_from_usages_walk(
    node: Node,
    source: &[u8],
    bare_name: &str,
) -> Option<HashKeyOwner> {
    if node.kind() == "hash_element_expression" {
        if let Some(var_text) = get_hash_from_element(node, source) {
            let var_bare = &var_text[1..]; // strip sigil
            if var_bare == bare_name {
                return resolve_key_owner(
                    find_root(node),
                    source,
                    &var_text,
                    node.start_position(),
                );
            }
        }
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(owner) = resolve_owner_from_usages_walk(child, source, bare_name) {
                return Some(owner);
            }
        }
    }
    None
}

/// Scan all subs/methods in the file for var_text as first parameter.
/// If found, infer class from enclosing package. Used as a fallback when
/// tree-sitter can't resolve context at incomplete cursor position.
fn infer_class_from_first_param_scan(
    root: Node,
    source: &[u8],
    var_text: &str,
) -> Option<HashKeyOwner> {
    infer_class_scan_walk(root, source, var_text)
}

fn infer_class_scan_walk(
    node: Node,
    source: &[u8],
    var_text: &str,
) -> Option<HashKeyOwner> {
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(first_param) = get_first_param(node, source) {
            if first_param == var_text {
                if let Some(pkg) = find_enclosing_package(node, source) {
                    return Some(HashKeyOwner::Class(pkg));
                }
            }
        }
    }

    // When source is incomplete, tree-sitter may wrap subs in ERROR nodes.
    // Look for `my ($var) = @_` pattern inside ERROR nodes and trace to package.
    if node.kind() == "ERROR" {
        if let Some(owner) = scan_error_node_for_first_param(node, source, var_text) {
            return Some(owner);
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(owner) = infer_class_scan_walk(child, source, var_text) {
                return Some(owner);
            }
        }
    }
    None
}

/// Scan an ERROR node for `my ($var) = @_` patterns that indicate the variable
/// is the first param. Resolve the class from the most recent package statement.
fn scan_error_node_for_first_param(
    error_node: Node,
    source: &[u8],
    var_text: &str,
) -> Option<HashKeyOwner> {
    // Walk descendants looking for assignment expressions with @_ on the right
    scan_error_for_param(error_node, source, var_text)
}

fn scan_error_for_param(
    node: Node,
    source: &[u8],
    var_text: &str,
) -> Option<HashKeyOwner> {
    if node.kind() == "assignment_expression" {
        if let Some(right) = node.child_by_field_name("right") {
            if right.utf8_text(source).ok() == Some("@_") {
                if let Some(left) = node.child_by_field_name("left") {
                    let mut vars = Vec::new();
                    collect_declared_vars(left, source, &mut vars);
                    if let Some((first, _)) = vars.first() {
                        if first == var_text {
                            // Found the pattern — find enclosing package
                            if let Some(pkg) = find_enclosing_package(node, source) {
                                return Some(HashKeyOwner::Class(pkg));
                            }
                        }
                    }
                }
            }
        }
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(owner) = scan_error_for_param(child, source, var_text) {
                return Some(owner);
            }
        }
    }
    None
}

fn collect_method_completions(
    tree: &Tree,
    source: &[u8],
    invocant: &str,
    point: Point,
) -> Vec<CompletionCandidate> {
    let root = tree.root_node();

    // Resolve class name: bareword invocant = direct class name, variable = type inference
    let class_name = if !invocant.starts_with('$') && !invocant.starts_with('@') && !invocant.starts_with('%') {
        Some(invocant.to_string())
    } else {
        infer_variable_class(root, source, invocant, point)
    };

    if let Some(ref cn) = class_name {
        // Check if it's a core class (class_statement)
        if let Some(class_node) = find_class_node(root, source, cn) {
            let mut candidates = Vec::new();
            // Implicit new for core classes
            candidates.push(CompletionCandidate {
                label: "new".to_string(),
                kind: SymbolKind::Method,
                detail: Some(format!("{}->new", cn)),
                insert_text: None,
                sort_priority: 0,
            });
            // Collect declared methods from the class block
            for i in 0..class_node.child_count() {
                if let Some(child) = class_node.child(i) {
                    if child.kind() == "block" {
                        collect_class_methods(child, source, cn, &mut candidates);
                    }
                }
            }
            return candidates;
        }

        // Check if it's a package — collect subs scoped to that package
        let scoped = collect_package_subs(root, source, cn);
        if !scoped.is_empty() {
            return scoped;
        }

        // Try extract_classes as fallback (handles class methods found via type inference)
        let classes = extract_classes(tree, std::str::from_utf8(source).unwrap_or(""));
        if let Some(class) = classes.iter().find(|c| c.name == *cn) {
            let mut candidates = Vec::new();
            // Implicit new for core classes
            candidates.push(CompletionCandidate {
                label: "new".to_string(),
                kind: SymbolKind::Method,
                detail: Some(format!("{}->new", cn)),
                insert_text: None,
                sort_priority: 0,
            });
            for method in &class.methods {
                candidates.push(CompletionCandidate {
                    label: method.name.clone(),
                    kind: SymbolKind::Method,
                    detail: Some(cn.clone()),
                    insert_text: None,
                    sort_priority: 0,
                });
            }
            return candidates;
        }
    }

    // Fall back: all subs and methods in file
    collect_all_subs(root, source)
}

/// Collect method declarations from a class block
fn collect_class_methods(block: Node, source: &[u8], class_name: &str, out: &mut Vec<CompletionCandidate>) {
    for i in 0..block.child_count() {
        if let Some(child) = block.child(i) {
            if child.kind() == "method_declaration_statement" {
                if let Some(name_node) = child.child_by_field_name("name") {
                    if let Ok(name) = name_node.utf8_text(source) {
                        out.push(CompletionCandidate {
                            label: name.to_string(),
                            kind: SymbolKind::Method,
                            detail: Some(class_name.to_string()),
                            insert_text: None,
                            sort_priority: 0,
                        });
                    }
                }
            }
        }
    }
}

/// Collect subs defined under a specific package
fn collect_package_subs(root: Node, source: &[u8], package_name: &str) -> Vec<CompletionCandidate> {
    let mut candidates = Vec::new();
    let mut current_package: Option<String> = None;
    collect_package_subs_walk(root, source, package_name, &mut current_package, &mut candidates);
    candidates
}

fn collect_package_subs_walk(
    node: Node,
    source: &[u8],
    package_name: &str,
    current_package: &mut Option<String>,
    out: &mut Vec<CompletionCandidate>,
) {
    match node.kind() {
        "package_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(text) = name_node.utf8_text(source) {
                    *current_package = Some(text.to_string());
                }
            }
        }
        "subroutine_declaration_statement" | "method_declaration_statement" => {
            if current_package.as_deref() == Some(package_name) {
                if let Some(name_node) = node.child_by_field_name("name") {
                    if let Ok(name) = name_node.utf8_text(source) {
                        let is_method = node.kind() == "method_declaration_statement";
                        out.push(CompletionCandidate {
                            label: name.to_string(),
                            kind: if is_method { SymbolKind::Method } else { SymbolKind::Function },
                            detail: Some(package_name.to_string()),
                            insert_text: None,
                            sort_priority: 0,
                        });
                    }
                }
            }
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            // Don't recurse into class blocks — they have their own handler
            if child.kind() != "class_statement" {
                collect_package_subs_walk(child, source, package_name, current_package, out);
            }
        }
    }
}

fn collect_all_subs(node: Node, source: &[u8]) -> Vec<CompletionCandidate> {
    let mut candidates = Vec::new();
    collect_all_subs_walk(node, source, &mut candidates);
    candidates
}

fn collect_all_subs_walk(node: Node, source: &[u8], out: &mut Vec<CompletionCandidate>) {
    if node.kind() == "subroutine_declaration_statement"
        || node.kind() == "method_declaration_statement"
    {
        if let Some(name_node) = node.child_by_field_name("name") {
            if let Ok(name) = name_node.utf8_text(source) {
                let is_method = node.kind() == "method_declaration_statement";
                out.push(CompletionCandidate {
                    label: name.to_string(),
                    kind: if is_method {
                        SymbolKind::Method
                    } else {
                        SymbolKind::Function
                    },
                    detail: Some(
                        if is_method { "method" } else { "sub" }.to_string(),
                    ),
                    insert_text: None,
                    sort_priority: 10,
                });
            }
        }
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_all_subs_walk(child, source, out);
        }
    }
}

fn collect_all_packages(node: Node, source: &[u8]) -> Vec<CompletionCandidate> {
    let mut candidates = Vec::new();
    collect_all_packages_walk(node, source, &mut candidates);
    candidates
}

fn collect_all_packages_walk(node: Node, source: &[u8], out: &mut Vec<CompletionCandidate>) {
    match node.kind() {
        "package_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    out.push(CompletionCandidate {
                        label: name.to_string(),
                        kind: SymbolKind::Package,
                        detail: Some("package".to_string()),
                        insert_text: None,
                        sort_priority: 20,
                    });
                }
            }
        }
        "class_statement" => {
            if let Some(name_node) = node.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(source) {
                    out.push(CompletionCandidate {
                        label: name.to_string(),
                        kind: SymbolKind::Class,
                        detail: Some("class".to_string()),
                        insert_text: None,
                        sort_priority: 20,
                    });
                }
            }
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_all_packages_walk(child, source, out);
        }
    }
}

// ---- Semantic tokens ----

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarModifier {
    Scalar,
    Array,
    Hash,
}

#[derive(Debug, Clone)]
pub struct SemanticVarToken {
    pub span: Span,
    pub var_type: VarModifier,
    pub is_declaration: bool,
    pub is_modification: bool,
    pub is_readonly: bool,
}

pub fn collect_semantic_tokens(tree: &Tree, source: &str) -> Vec<SemanticVarToken> {
    let source_bytes = source.as_bytes();
    let classes = extract_classes(tree, source);
    let mut tokens = Vec::new();
    collect_semantic_tokens_walk(tree.root_node(), source_bytes, &classes, &mut tokens);
    // Sort by position (line, then column)
    tokens.sort_by(|a, b| {
        a.span.start.row.cmp(&b.span.start.row)
            .then(a.span.start.column.cmp(&b.span.start.column))
    });
    tokens
}

fn collect_semantic_tokens_walk(
    node: Node,
    source: &[u8],
    classes: &[ClassInfo],
    tokens: &mut Vec<SemanticVarToken>,
) {
    match node.kind() {
        "scalar" | "array" | "hash" => {
            let var_type = match node.kind() {
                "scalar" => VarModifier::Scalar,
                "array" => VarModifier::Array,
                "hash" => VarModifier::Hash,
                _ => unreachable!(),
            };
            let is_decl = is_declaration_node(node);
            let is_mod = is_write_node(node);
            let is_readonly = is_readonly_field(node, source, classes);
            tokens.push(SemanticVarToken {
                span: node_to_span(node),
                var_type,
                is_declaration: is_decl,
                is_modification: is_mod && !is_decl,
                is_readonly,
            });
        }
        "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
            // Deref access like $hash{key}, @arr[0..2] — resolve the canonical sigil
            let var_type = match node.kind() {
                "keyval_container_variable" => VarModifier::Hash,
                "slice_container_variable" => {
                    if let Some(parent) = node.parent() {
                        if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
                            VarModifier::Hash
                        } else {
                            VarModifier::Array
                        }
                    } else {
                        VarModifier::Array
                    }
                }
                _ => {
                    // Regular container_variable — check parent field
                    if let Some(parent) = node.parent() {
                        if parent.child_by_field_name("hash").map_or(false, |h| h.id() == node.id()) {
                            VarModifier::Hash
                        } else if parent.child_by_field_name("array").map_or(false, |a| a.id() == node.id()) {
                            VarModifier::Array
                        } else {
                            VarModifier::Scalar
                        }
                    } else {
                        VarModifier::Scalar
                    }
                }
            };
            let is_mod = is_write_node(node);
            tokens.push(SemanticVarToken {
                span: node_to_span(node),
                var_type,
                is_declaration: false,
                is_modification: is_mod,
                is_readonly: false, // container access to fields could be tracked but skip for now
            });
            return; // Don't recurse into children of container_variable
        }
        _ => {}
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            collect_semantic_tokens_walk(child, source, classes, tokens);
        }
    }
}

fn is_declaration_node(node: Node) -> bool {
    let mut current = node;
    for _ in 0..5 {
        if let Some(parent) = current.parent() {
            match parent.kind() {
                "variable_declaration" => return true,
                "signature" => return true,
                "for_statement" => {
                    // Check if this is the loop variable
                    if let Some(var) = parent.child_by_field_name("variable") {
                        if var.id() == current.id() || var.id() == node.id() {
                            return true;
                        }
                    }
                    return false;
                }
                _ => {
                    current = parent;
                    continue;
                }
            }
        }
        break;
    }
    false
}

fn is_write_node(node: Node) -> bool {
    let mut current = node;
    for _ in 0..5 {
        if let Some(parent) = current.parent() {
            match parent.kind() {
                "variable_declaration" => return true,
                "assignment_expression" => {
                    if let Some(left) = parent.child_by_field_name("left") {
                        let np = node.start_position();
                        return left.start_position() <= np && left.end_position() >= np;
                    }
                    return false;
                }
                "signature" => return true,
                "for_statement" => {
                    if let Some(var) = parent.child_by_field_name("variable") {
                        if var.id() == current.id() || var.id() == node.id() {
                            return true;
                        }
                    }
                    return false;
                }
                _ => {
                    current = parent;
                    continue;
                }
            }
        }
        break;
    }
    false
}

/// Check if a variable node refers to a readonly class field.
/// Currently always returns false — core Perl `class` has no :readonly attribute.
/// :reader just creates a getter method, it doesn't make the field immutable.
/// This is a placeholder for when we add framework stubs (e.g. Moo `is => 'ro'`).
fn is_readonly_field(_node: Node, _source: &[u8], _classes: &[ClassInfo]) -> bool {
    false
}

// ---- Signature help ----

#[derive(Debug, Clone)]
pub struct SignatureParam {
    pub name: String,
    pub default: Option<String>,
    pub is_slurpy: bool,
}

#[derive(Debug, Clone)]
pub struct SubSignature {
    pub name: String,
    pub params: Vec<SignatureParam>,
    pub is_method: bool,
}

pub struct SignatureHelpResult {
    pub signature: SubSignature,
    pub active_param: usize,
}

/// Extract parameters from a subroutine node.
/// Tries signature syntax first, then falls back to `my (...) = @_` pattern.
fn extract_sub_params(sub_node: Node, source: &[u8]) -> Vec<SignatureParam> {
    // Try signature syntax: sub foo($x, $y = 10, @rest) { }
    for i in 0..sub_node.child_count() {
        if let Some(sig) = sub_node.child(i) {
            if sig.kind() == "signature" {
                let mut params = Vec::new();
                for j in 0..sig.named_child_count() {
                    if let Some(param) = sig.named_child(j) {
                        match param.kind() {
                            "mandatory_parameter" => {
                                if let Some(var) = first_var_child(param, source) {
                                    params.push(SignatureParam {
                                        name: var,
                                        default: None,
                                        is_slurpy: false,
                                    });
                                }
                            }
                            "optional_parameter" => {
                                let var = first_var_child(param, source);
                                let default = param.child_by_field_name("default")
                                    .or_else(|| {
                                        // Fallback: last named child after the variable
                                        let nc = param.named_child_count();
                                        if nc >= 2 { param.named_child(nc - 1) } else { None }
                                    })
                                    .and_then(|d| d.utf8_text(source).ok())
                                    .map(|s| s.to_string());
                                if let Some(name) = var {
                                    params.push(SignatureParam {
                                        name,
                                        default,
                                        is_slurpy: false,
                                    });
                                }
                            }
                            "slurpy_parameter" => {
                                if let Some(var) = first_var_child(param, source) {
                                    params.push(SignatureParam {
                                        name: var,
                                        default: None,
                                        is_slurpy: true,
                                    });
                                }
                            }
                            // Bare scalar/array/hash directly in signature (some grammars)
                            "scalar" | "array" | "hash" => {
                                if let Ok(text) = param.utf8_text(source) {
                                    let is_slurpy = matches!(param.kind(), "array" | "hash");
                                    params.push(SignatureParam {
                                        name: text.to_string(),
                                        default: None,
                                        is_slurpy,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
                return params;
            }
        }
    }

    // Fallback: look for `my (...) = @_` in the body
    if let Some(body) = sub_node.child_by_field_name("body") {
        for i in 0..body.named_child_count() {
            if let Some(stmt) = body.named_child(i) {
                let assign = if stmt.kind() == "expression_statement" {
                    stmt.named_child(0).filter(|n| n.kind() == "assignment_expression")
                } else if stmt.kind() == "assignment_expression" {
                    Some(stmt)
                } else {
                    None
                };
                if let Some(assign) = assign {
                    if let Some(right) = assign.child_by_field_name("right") {
                        if right.utf8_text(source).ok() == Some("@_") {
                            if let Some(left) = assign.child_by_field_name("left") {
                                let mut vars = Vec::new();
                                collect_declared_vars(left, source, &mut vars);
                                return vars
                                    .into_iter()
                                    .map(|(name, _)| {
                                        let is_slurpy = name.starts_with('@') || name.starts_with('%');
                                        SignatureParam {
                                            name,
                                            default: None,
                                            is_slurpy,
                                        }
                                    })
                                    .collect();
                            }
                        }
                    }
                }
            }
        }
    }

    Vec::new()
}

/// Get the first variable (scalar/array/hash) text from a parameter node.
fn first_var_child(node: Node, source: &[u8]) -> Option<String> {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if matches!(child.kind(), "scalar" | "array" | "hash") {
                return child.utf8_text(source).ok().map(|s| s.to_string());
            }
        }
    }
    None
}

/// Count the number of top-level commas before `cursor` in an arguments node or list.
/// This gives the active parameter index.
fn count_commas_before(node: Node, cursor: Point) -> usize {
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
                "," if depth == 0 => count += 1,
                _ => {
                    // For nested expressions that might contain commas (like function_call_expression),
                    // they're handled as a single node so we don't need to recurse.
                }
            }
        }
    }
    count
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
    // Walk root's children for nearby ERROR nodes that end on the same line.
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

    // Walk up to find the enclosing call expression or ERROR
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
                // Only match if cursor is inside the arguments (after the `(`)
                // If there are no arguments, this is a no-paren call — check next sibling for ERROR with `(`
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
                // This handles: method_call_expression (no parens) + ERROR > (
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
                // First try: scan ERROR children for a full call pattern
                if let Some(result) = parse_call_from_error(node, source, cursor) {
                    return Some(result);
                }
                // Fallback: ERROR might just contain `(` with the call expression
                // as a preceding sibling.
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
/// may be in a preceding sibling. Handle patterns like:
///   expression_statement > method_call_expression   ← sibling
///   ERROR > ( [list_expression]                      ← current
fn find_call_from_error_sibling<'a>(
    error: Node<'a>,
    source: &'a [u8],
    _point: Point,
) -> Option<(String, bool, Option<String>, Option<Node<'a>>)> {
    // Collect args from the ERROR node itself
    let mut args_node: Option<Node<'a>> = None;
    for i in 0..error.child_count() {
        if let Some(child) = error.child(i) {
            if child.kind() == "list_expression" {
                args_node = Some(child);
            }
        }
    }

    // Look at the previous sibling
    let prev = error.prev_sibling()?;

    // Case 1: prev is an expression_statement containing a call
    let call_node = if prev.kind() == "expression_statement" {
        // The call is the child of the expression_statement
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
            // If the call already has arguments, prefer those (complete call)
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
        // Bare function name: expression_statement > bareword("new") + ERROR > (
        "bareword" => {
            let name = call_node.utf8_text(source).ok()?;
            Some((name.to_string(), false, None, args_node))
        }
        _ => None,
    }
}

/// Parse a call expression from an ERROR node.
/// Looks for patterns like: `foo(args...` or `$obj->method(args...`
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

        // Track invocant -> method pattern
        if matches!(child.kind(), "scalar" | "array" | "hash" | "package") {
            invocant = child.utf8_text(source).ok().map(|s| s.to_string());
        } else if child.kind() == "->" {
            has_arrow = true;
            // If we previously saw a bareword as func_name, it's actually the invocant
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
        // Fallback: tree-sitter sometimes drops barewords from ERROR nodes.
        // Recover the function name from source text before the `(`.
        let paren_byte = error.child(0)
            .filter(|c| c.kind() == "(")
            .map(|c| c.start_byte())
            .or_else(|| {
                (0..error.child_count())
                    .filter_map(|i| error.child(i))
                    .find(|c| c.kind() == "(")
                    .map(|c| c.start_byte())
            });
        if let Some(pb) = paren_byte {
            let start = error.start_byte();
            if pb > start {
                let text = std::str::from_utf8(&source[start..pb]).unwrap_or("").trim();
                if !text.is_empty() && text.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
                    return Some((text.to_string(), false, None, args_node));
                }
            }
        }
        None
    }
}

/// Main entry point for signature help.
pub fn signature_help_at_point(
    tree: &Tree,
    source: &str,
    point: Point,
) -> Option<SignatureHelpResult> {
    let source_bytes = source.as_bytes();
    let root = tree.root_node();

    let (name, is_method_call, invocant, args_node) =
        find_call_at_cursor(tree, source_bytes, point)?;

    // Find the sub definition
    let (sub_span, _) = find_sub_def(root, source_bytes, &name)?;

    // Find the actual sub node at that span
    let sub_node = root.descendant_for_point_range(sub_span.start, sub_span.start)?;
    // Walk up to get the actual subroutine/method declaration
    let sub_node = find_sub_or_method_node(sub_node)?;

    let mut params = extract_sub_params(sub_node, source_bytes);

    // Determine if this is a method: either called with -> or first param is $self/$class
    let is_method = is_method_call
        || invocant.is_some()
        || params
            .first()
            .map_or(false, |p| p.name == "$self" || p.name == "$class");

    // Strip $self/$class from method params
    if is_method && !params.is_empty() {
        let first = &params[0].name;
        if first == "$self" || first == "$class" {
            params.remove(0);
        }
    }

    // Count active parameter
    let active_param = match args_node {
        Some(args) => count_commas_before(args, point),
        None => 0, // cursor right after open paren, no args yet
    };

    Some(SignatureHelpResult {
        signature: SubSignature {
            name,
            params,
            is_method,
        },
        active_param,
    })
}

/// Walk up from a node to find the enclosing subroutine/method declaration.
fn find_sub_or_method_node(mut node: Node) -> Option<Node> {
    for _ in 0..10 {
        if matches!(
            node.kind(),
            "subroutine_declaration_statement" | "method_declaration_statement"
        ) {
            return Some(node);
        }
        node = node.parent()?;
    }
    None
}

// ---- Diagnostics ----

#[derive(Debug, Clone)]
pub struct LspDiagnostic {
    pub span: Span,
    pub message: String,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

pub fn collect_diagnostics(tree: &Tree, source: &str) -> Vec<LspDiagnostic> {
    let _ = (tree, source);
    // Placeholder — readonly field write diagnostics will be added when we have
    // framework stubs (e.g. Moo `is => 'ro'`) that can actually determine immutability.
    // Core Perl `class` has no :readonly attribute (:reader just creates a getter).
    Vec::new()
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

    // ---- Bareword invocant ----

    #[test]
    fn test_bareword_invocant_is_package() {
        let src = "Calculator->new();";
        let tree = parse(src);
        // "Calculator" at col 0 is a bareword invocant → should be Package, not Function
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(0, 0));
        assert!(matches!(sym, Some(CursorSymbol::Package { .. })));
    }

    #[test]
    fn test_bareword_invocant_refs() {
        let src = "\
package Calculator;
sub new { 1 }

Calculator->new();";
        let tree = parse(src);
        // Refs from "Calculator" on line 3 → should find package decl and usage
        let refs = find_references(&tree, src, Point::new(3, 0));
        assert!(refs.len() >= 2);
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&0)); // package Calculator
        assert!(lines.contains(&3)); // Calculator->new()
    }

    #[test]
    fn test_def_package_from_invocant() {
        let src = "\
package Calculator;
sub new { 1 }

Calculator->new();";
        let tree = parse(src);
        // gd on "Calculator" bareword invocant → jumps to package declaration
        let span = find_definition(&tree, src, Point::new(3, 0)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_class_from_invocant() {
        let src = r#"class Greeter {
    method greet () { 1 }
}
Greeter->new();"#;
        let tree = parse(src);
        // gd on "Greeter" → jumps to class declaration
        let span = find_definition(&tree, src, Point::new(3, 0)).unwrap();
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_new_on_bareword_class() {
        // gd on "new" in Calculator->new() should resolve to the class/package
        let src = "\
package Calculator;
sub add { 1 }

Calculator->new();";
        let tree = parse(src);
        // Cursor on "new" (line 3, col 13)
        let span = find_definition(&tree, src, Point::new(3, 13)).unwrap();
        // Should jump to package Calculator on line 0
        assert_eq!(span.start.row, 0);
    }

    #[test]
    fn test_def_method_on_bareword_class() {
        // gd on explicit method in Greeter->greet() should resolve to the method
        let src = r#"class Greeter {
    method greet () { 1 }
}
Greeter->greet();"#;
        let tree = parse(src);
        // Cursor on "greet" (line 3)
        let span = find_definition(&tree, src, Point::new(3, 9)).unwrap();
        // Should jump to method greet on line 1
        assert_eq!(span.start.row, 1);
    }

    // ---- Verify gap assumptions ----

    #[test]
    fn test_refs_string_interpolation() {
        // Variables inside interpolated strings should be found by references
        let src = r#"my $name = "World";
print "Hello, $name!";
print "Goodbye $name.";"#;
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(0, 4));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&0)); // declaration
        assert!(lines.contains(&1)); // "Hello, $name!"
        assert!(lines.contains(&2)); // "Goodbye $name."
    }

    #[test]
    fn test_def_state_variable() {
        let src = "\
sub counter {
    state $count = 0;
    $count++;
    return $count;
}";
        let tree = parse(src);
        // $count on line 2 → should resolve to state $count on line 1
        let span = find_definition(&tree, src, Point::new(2, 5)).unwrap();
        assert_eq!(span.start.row, 1);
    }

    #[test]
    fn test_refs_heredoc() {
        let src = "my $name = \"World\";\nmy $msg = <<END;\nHello $name!\nEND";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(0, 4));
        let lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(lines.contains(&0)); // declaration
        assert!(lines.contains(&2)); // heredoc body
    }

    // ---- Completion ----

    fn labels(candidates: &[CompletionCandidate]) -> Vec<&str> {
        candidates.iter().map(|c| c.label.as_str()).collect()
    }

    #[test]
    fn test_complete_variables() {
        let src = "my $x = 1;\nmy $y = 2;\n$";
        let tree = parse(src);
        // After $ on line 2
        let items = collect_completions(&tree, src, Point::new(2, 1));
        let l = labels(&items);
        assert!(l.contains(&"$x"), "missing $x, got {:?}", l);
        assert!(l.contains(&"$y"), "missing $y, got {:?}", l);
    }

    #[test]
    fn test_complete_variables_scoped() {
        let src = "\
my $outer = 1;
sub foo {
    my $inner = 2;
    $
}";
        let tree = parse(src);
        // Inside sub body, both outer and inner should be visible
        let items = collect_completions(&tree, src, Point::new(3, 5));
        let l = labels(&items);
        assert!(l.contains(&"$inner"), "missing $inner, got {:?}", l);
        assert!(l.contains(&"$outer"), "missing $outer, got {:?}", l);
    }

    #[test]
    fn test_complete_after_arrow() {
        let src = r#"class Greeter {
    method greet () { 1 }
    method wave () { 2 }
}
my $g = Greeter->new();
$g->"#;
        let tree = parse(src);
        let items = collect_completions(&tree, src, Point::new(5, 4));
        let l = labels(&items);
        assert!(l.contains(&"greet"), "missing greet, got {:?}", l);
        assert!(l.contains(&"wave"), "missing wave, got {:?}", l);
    }

    #[test]
    fn test_complete_same_name_different_sigil() {
        // %args and @args are different variables — both should appear
        let src = "\
sub new {
    my (%args) = @_;
    my @args;
    $";
        let tree = parse(src);
        let items = collect_completions(&tree, src, Point::new(3, 5));
        let l = labels(&items);
        // Should have $args{} from %args AND $args[] from @args
        assert!(l.contains(&"$args{}"), "missing $args{{}} from %args, got {:?}", l);
        assert!(l.contains(&"$args[]"), "missing $args[] from @args, got {:?}", l);
    }

    #[test]
    fn test_complete_subs() {
        let src = "sub foo { 1 }\nsub bar { 2 }\n";
        let tree = parse(src);
        // General completion (no trigger character)
        let items = collect_completions(&tree, src, Point::new(2, 0));
        let l = labels(&items);
        assert!(l.contains(&"foo"), "missing foo, got {:?}", l);
        assert!(l.contains(&"bar"), "missing bar, got {:?}", l);
    }

    #[test]
    fn test_complete_in_class() {
        let src = r#"class Point {
    field $x :param;
    field $y :param;

    method sum () {
        $
    }
}"#;
        let tree = parse(src);
        // Inside method body, fields should be visible
        let items = collect_completions(&tree, src, Point::new(5, 9));
        let l = labels(&items);
        assert!(l.contains(&"$x"), "missing $x, got {:?}", l);
        assert!(l.contains(&"$y"), "missing $y, got {:?}", l);
    }

    #[test]
    fn test_complete_cross_sigil() {
        let src = "my @arr = (1,2);\nmy %hash = (a => 1);\n$";
        let tree = parse(src);
        let items = collect_completions(&tree, src, Point::new(2, 1));
        let l = labels(&items);
        // $arr[] for array element
        assert!(l.contains(&"$arr[]"), "missing $arr[], got {:?}", l);
        // $hash{} for hash element
        assert!(l.contains(&"$hash{}"), "missing $hash{{}}, got {:?}", l);
        // $#arr for last index
        assert!(l.contains(&"$#arr"), "missing $#arr, got {:?}", l);
    }

    #[test]
    fn test_complete_at_sigil_forms() {
        let src = "my @arr = (1,2);\nmy %hash = (a => 1);\n@";
        let tree = parse(src);
        let items = collect_completions(&tree, src, Point::new(2, 1));
        let l = labels(&items);
        assert!(l.contains(&"@arr"), "missing @arr, got {:?}", l);
        assert!(l.contains(&"@arr[]"), "missing @arr[] (slice), got {:?}", l);
        assert!(l.contains(&"@hash{}"), "missing @hash{{}} (hash slice), got {:?}", l);
        // Should NOT have @hash as a plain array
        assert!(!l.contains(&"@hash"), "should not have plain @hash, got {:?}", l);
    }

    #[test]
    fn test_complete_percent_sigil_forms() {
        let src = "my @arr = (1,2);\nmy %hash = (a => 1);\n%";
        let tree = parse(src);
        let items = collect_completions(&tree, src, Point::new(2, 1));
        let l = labels(&items);
        assert!(l.contains(&"%hash"), "missing %hash, got {:?}", l);
        assert!(l.contains(&"%hash{}"), "missing %hash{{}} (kv slice), got {:?}", l);
        assert!(l.contains(&"%arr[]"), "missing %arr[] (array kv slice), got {:?}", l);
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

    // ---- Document highlight ----

    #[test]
    fn test_highlight_variable_read_write() {
        let src = "\
my $x = 1;
print $x;
$x = 2;";
        let tree = parse(src);
        let highlights = document_highlights(&tree, src, Point::new(0, 4));
        assert_eq!(highlights.len(), 3);
        // Declaration is write
        let decl = highlights.iter().find(|h| h.span.start.row == 0).unwrap();
        assert_eq!(decl.kind, HighlightKind::Write);
        // Usage in print is read
        let usage = highlights.iter().find(|h| h.span.start.row == 1).unwrap();
        assert_eq!(usage.kind, HighlightKind::Read);
        // Assignment is write
        let assign = highlights.iter().find(|h| h.span.start.row == 2).unwrap();
        assert_eq!(assign.kind, HighlightKind::Write);
    }

    #[test]
    fn test_highlight_function() {
        let src = "sub foo { 1 }\nfoo();";
        let tree = parse(src);
        let highlights = document_highlights(&tree, src, Point::new(0, 4));
        assert_eq!(highlights.len(), 2);
        // Functions are all Read (declaration and call)
        assert!(highlights.iter().all(|h| h.kind == HighlightKind::Read));
    }

    // ---- Selection range ----

    #[test]
    fn test_selection_range() {
        let src = "\
sub foo {
    my $x = 1;
}";
        let tree = parse(src);
        // Cursor on $x (line 1, col 7)
        let ranges = selection_ranges(&tree, Point::new(1, 8));
        // Should have multiple levels: $x → variable_declaration → expression_statement → block → sub → source_file
        assert!(ranges.len() >= 3, "expected at least 3 levels, got {}", ranges.len());
        // Innermost should be within line 1
        assert_eq!(ranges[0].start.row, 1);
        // Outermost should be the source file (row 0)
        let last = ranges.last().unwrap();
        assert_eq!(last.start.row, 0);
    }

    // ---- Folding ranges ----

    #[test]
    fn test_folding_ranges() {
        let src = "\
sub foo {
    my $x = 1;
    return $x;
}

sub bar {
    return 2;
}";
        let tree = parse(src);
        let folds = folding_ranges(&tree, src);
        // Should have folds for both sub blocks
        assert!(folds.len() >= 2, "expected at least 2 folds, got {:?}", folds);
        let starts: Vec<usize> = folds.iter().map(|f| f.start_line).collect();
        assert!(starts.contains(&0), "missing fold starting at line 0: {:?}", folds);
        assert!(starts.contains(&5), "missing fold starting at line 5: {:?}", folds);
    }

    #[test]
    fn test_folding_pod() {
        let src = "\
sub foo { 1 }

=head1 NAME

Some documentation

=cut

sub bar { 2 }";
        let tree = parse(src);
        let folds = folding_ranges(&tree, src);
        // Should have a comment fold for the pod section
        let pod_fold = folds.iter().find(|f| f.kind == FoldKind::Comment);
        assert!(pod_fold.is_some(), "missing pod fold: {:?}", folds);
        let pf = pod_fold.unwrap();
        assert_eq!(pf.start_line, 2); // =head1
        assert_eq!(pf.end_line, 6);   // =cut
    }

    #[test]
    fn test_folding_class() {
        let src = r#"class Point {
    field $x :param;

    method magnitude () {
        return $x;
    }
}"#;
        let tree = parse(src);
        let folds = folding_ranges(&tree, src);
        // Should fold the class block and the method block
        assert!(folds.len() >= 2, "expected at least 2 folds, got {:?}", folds);
    }

    // ---- Semantic tokens ----

    #[test]
    fn test_semantic_tokens_basic() {
        let src = "my $x = 1;\nmy @arr = (1,2);\nmy %hash = (a => 1);";
        let tree = parse(src);
        let tokens = collect_semantic_tokens(&tree, src);
        assert!(tokens.len() >= 3, "expected at least 3 tokens, got {:?}", tokens);

        let scalar = tokens.iter().find(|t| t.span.start.row == 0).unwrap();
        assert_eq!(scalar.var_type, VarModifier::Scalar);
        assert!(scalar.is_declaration);

        let array = tokens.iter().find(|t| t.span.start.row == 1).unwrap();
        assert_eq!(array.var_type, VarModifier::Array);
        assert!(array.is_declaration);

        let hash = tokens.iter().find(|t| t.span.start.row == 2).unwrap();
        assert_eq!(hash.var_type, VarModifier::Hash);
        assert!(hash.is_declaration);
    }

    #[test]
    fn test_semantic_tokens_read_write() {
        let src = "\
my $x = 1;
print $x;
$x = 2;";
        let tree = parse(src);
        let tokens = collect_semantic_tokens(&tree, src);
        // Line 0: declaration (write)
        let decl = tokens.iter().find(|t| t.span.start.row == 0).unwrap();
        assert!(decl.is_declaration);
        assert!(!decl.is_modification); // declaration, not modification
        // Line 1: read
        let read = tokens.iter().find(|t| t.span.start.row == 1).unwrap();
        assert!(!read.is_declaration);
        assert!(!read.is_modification);
        // Line 2: modification (write but not declaration)
        let write = tokens.iter().find(|t| t.span.start.row == 2).unwrap();
        assert!(!write.is_declaration);
        assert!(write.is_modification);
    }

    // Readonly field detection is a placeholder — core Perl class has no :readonly.
    // :reader just creates a getter. Readonly diagnostics will come with framework stubs
    // (e.g. Moo `is => 'ro'`).

    // ---- Hash key intelligence ----

    fn dump_tree(node: tree_sitter::Node, source: &[u8], indent: usize) {
        let prefix = " ".repeat(indent);
        let text = if node.child_count() == 0 {
            format!(" {:?}", node.utf8_text(source).unwrap_or("?"))
        } else { "".to_string() };
        let field = node.parent().and_then(|p| {
            for i in 0..p.child_count() {
                if p.child(i).map_or(false, |c| c.id() == node.id()) {
                    return p.field_name_for_child(i as u32).map(|s| format!(" [{}]", s));
                }
            }
            None
        }).unwrap_or_default();
        eprintln!("{}{}{} ({},{})-({},{}){}", prefix, node.kind(), field,
            node.start_position().row, node.start_position().column,
            node.end_position().row, node.end_position().column, text);
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                dump_tree(child, source, indent + 2);
            }
        }
    }

    // Tier 1: Basic hash element access

    #[test]
    fn test_hash_key_at_cursor() {
        let src = "my %hash = (name => 1);\n$hash{name};";
        let tree = parse(src);
        // Cursor on "name" inside $hash{name} (line 1, col 6)
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(1, 6));
        assert!(
            matches!(sym, Some(CursorSymbol::HashKey { .. })),
            "expected HashKey, got {:?}", sym.map(|_| "other")
        );
    }

    #[test]
    fn test_hash_key_string_at_cursor() {
        let src = r#"my %hash = ("key" => 1);
$hash{"key"};"#;
        let tree = parse(src);
        // Cursor on "key" inside $hash{"key"} — on the string_content
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(1, 7));
        assert!(
            matches!(sym, Some(CursorSymbol::HashKey { .. })),
            "expected HashKey for string key"
        );
    }

    #[test]
    fn test_hash_key_definition() {
        let src = "\
package Calculator;

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        history => [],
        verbose => $args{verbose} // 0,
    }, $class;
    return $self;
}

sub add {
    my ($self, $a, $b) = @_;
    push @{$self->{history}}, \"add($a, $b)\";
}";
        let tree = parse(src);
        // Cursor on "history" in $self->{history} (line 13)
        // Should jump to first occurrence in bless { history => [] }
        let span = find_definition(&tree, src, Point::new(13, 20));
        assert!(span.is_some(), "expected definition for hash key 'history'");
        let span = span.unwrap();
        assert_eq!(span.start.row, 5, "expected definition at bless construction, got row {}", span.start.row);
    }

    #[test]
    fn test_hash_key_references() {
        let src = "\
package Calculator;

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        history => [],
        verbose => 0,
    }, $class;
    return $self;
}

sub add {
    my ($self, $a, $b) = @_;
    push @{$self->{history}}, \"add\";
}

sub get_history {
    my ($self) = @_;
    return @{$self->{history}};
}";
        let tree = parse(src);
        // References for "history" from $self->{history} on line 13
        let refs = find_references(&tree, src, Point::new(13, 20));
        assert!(refs.len() >= 3, "expected at least 3 refs for 'history', got {}: {:?}",
            refs.len(), refs.iter().map(|r| r.start.row).collect::<Vec<_>>());
        let rows: Vec<usize> = refs.iter().map(|r| r.start.row).collect();
        assert!(rows.contains(&5), "missing bless construction ref");
        assert!(rows.contains(&13), "missing add method ref");
        assert!(rows.contains(&18), "missing get_history method ref");
    }

    // Tier 2: Construction patterns

    #[test]
    fn test_hash_key_fat_comma_construction() {
        let src = "\
package Foo;
sub new {
    my ($class) = @_;
    my $self = bless {
        name => 'foo',
        count => 0,
    }, $class;
    return $self;
}

sub get_name {
    my ($self) = @_;
    return $self->{name};
}";
        let tree = parse(src);
        // Cursor on "name" in the bless construction (line 4)
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(4, 8));
        assert!(
            matches!(sym, Some(CursorSymbol::HashKey { .. })),
            "expected HashKey in bless construction"
        );
    }

    #[test]
    fn test_hash_key_plain_comma() {
        let src = r#"my %h = ("a", 1, "b", 2);
$h{a};"#;
        let tree = parse(src);
        // References for key "a" should include both the construction and access
        let refs = find_references(&tree, src, Point::new(1, 3));
        assert!(refs.len() >= 1, "expected refs for plain comma hash key 'a'");
    }

    #[test]
    fn test_hash_key_anon_hashref() {
        let src = "my $ref = { foo => 1, bar => 2 };\n$ref->{foo};";
        let tree = parse(src);
        let refs = find_references(&tree, src, Point::new(1, 7));
        // Should find foo in both the anon hash and the access
        assert!(refs.len() >= 2, "expected at least 2 refs for anon hashref key, got {}: {:?}",
            refs.len(), refs.iter().map(|r| r.start.row).collect::<Vec<_>>());
    }

    // Tier 3: Class namespace binding

    #[test]
    fn test_hash_key_class_namespace() {
        let src = "\
package Calculator;

sub new {
    my ($class) = @_;
    return bless { history => [] }, $class;
}

sub add {
    my ($self) = @_;
    push @{$self->{history}}, 'entry';
}

package main;

my $calc = Calculator->new();
$calc->{history};";
        let tree = parse(src);
        // Cursor on "history" in $calc->{history} (line 15)
        // Should find references in bless construction AND $self->{history}
        let refs = find_references(&tree, src, Point::new(15, 8));
        let rows: Vec<usize> = refs.iter().map(|r| r.start.row).collect();
        assert!(refs.len() >= 2, "expected cross-variable refs via class namespace, got {}: {:?}",
            refs.len(), rows);
        // Should include the bless construction on row 4
        assert!(rows.contains(&4), "missing bless construction ref, got {:?}", rows);
    }

    #[test]
    fn test_hash_key_first_param_instance() {
        let src = "\
package Foo;
sub process {
    my ($obj) = @_;
    return $obj->{data};
}";
        let tree = parse(src);
        // $obj is first param in package Foo → should bind to Foo namespace
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(3, 19));
        match sym {
            Some(CursorSymbol::HashKey { ref owner, .. }) => {
                assert!(matches!(owner, HashKeyOwner::Class(ref cn) if cn == "Foo"),
                    "expected Foo class owner, got {:?}", owner);
            }
            _ => panic!("expected HashKey, got {:?}", sym.map(|_| "other")),
        }
    }

    // Tier 4: Rename + hover

    #[test]
    fn test_hash_key_rename() {
        let src = "\
package Foo;
sub new {
    my ($class) = @_;
    return bless { verbose => 0 }, $class;
}

sub is_verbose {
    my ($self) = @_;
    return $self->{verbose};
}";
        let tree = parse(src);
        let edits = rename(&tree, src, Point::new(8, 22), "debug");
        assert!(edits.is_some(), "expected rename edits for hash key");
        let edits = edits.unwrap();
        assert!(edits.len() >= 2, "expected at least 2 rename edits, got {}", edits.len());
        assert!(edits.iter().all(|e| e.new_text == "debug"),
            "all edits should use new name");
    }

    #[test]
    fn test_hash_key_hover() {
        let src = "\
package Foo;
sub new {
    my ($class) = @_;
    return bless { name => 'x' }, $class;
}
sub get {
    my ($self) = @_;
    return $self->{name};
}";
        let tree = parse(src);
        let result = hover_info(&tree, src, Point::new(7, 20));
        assert!(result.is_some(), "expected hover for hash key");
        let md = result.unwrap().markdown;
        assert!(md.contains("name"), "hover should mention key name, got: {}", md);
    }

    // Tier 5: Edge cases

    #[test]
    fn test_hash_key_different_hashes() {
        let src = "\
my %a = (key => 1);
my %b = (key => 2);
$a{key};
$b{key};";
        let tree = parse(src);
        // References from %a's key should not include %b's key
        let refs_a = find_references(&tree, src, Point::new(2, 3));
        let rows_a: Vec<usize> = refs_a.iter().map(|r| r.start.row).collect();
        // Should include line 0 (construction) and line 2 (access), NOT line 1 or 3
        assert!(!rows_a.contains(&1), "should not include %b construction: {:?}", rows_a);
        assert!(!rows_a.contains(&3), "should not include %b access: {:?}", rows_a);
    }

    #[test]
    fn test_hash_key_dynamic() {
        let src = "\
my %hash = (name => 1);
my $key = 'name';
$hash{$key};";
        let tree = parse(src);
        // Cursor on $key inside $hash{$key} — should detect as dynamic
        let sym = symbol_at_cursor(&tree, src.as_bytes(), Point::new(2, 6));
        // Dynamic key should resolve as Variable, not HashKey
        // (because the node is a scalar, not an autoquoted_bareword)
        assert!(
            matches!(sym, Some(CursorSymbol::Variable { .. })),
            "dynamic key $key should resolve as Variable, not HashKey"
        );
    }

    // ---- Hash key completion tests ----

    #[test]
    fn test_hash_key_completion_direct() {
        let src = "\
my %hash = (name => 1, age => 2);
$hash{";
        let tree = parse(src);
        // Cursor right after { on line 1
        let items = collect_completions(&tree, src, Point::new(1, 6));
        let l = labels(&items);
        assert!(l.contains(&"name"), "missing name, got {:?}", l);
        assert!(l.contains(&"age"), "missing age, got {:?}", l);
    }

    #[test]
    fn test_hash_key_completion_arrow() {
        let src = "\
package Calculator;
sub new {
    my ($class) = @_;
    return bless { history => [], verbose => 0 }, $class;
}
sub get {
    my ($self) = @_;
    $self->{";
        let tree = parse(src);
        // Cursor right after { on line 7
        let items = collect_completions(&tree, src, Point::new(7, 12));
        let l = labels(&items);
        assert!(l.contains(&"history"), "missing history, got {:?}", l);
        assert!(l.contains(&"verbose"), "missing verbose, got {:?}", l);
    }

    #[test]
    fn test_hash_key_completion_mode_detection() {
        // Test detect_completion_mode for various patterns
        assert_eq!(
            detect_completion_mode("$hash{", Point::new(0, 6)),
            CompletionMode::HashKey { var_text: "$hash".to_string() }
        );
        assert_eq!(
            detect_completion_mode("$self->{", Point::new(0, 8)),
            CompletionMode::HashKey { var_text: "$self".to_string() }
        );
        // Regular variable trigger should not be hash key
        assert_eq!(
            detect_completion_mode("$", Point::new(0, 1)),
            CompletionMode::Variable { sigil: '$' }
        );
        // Arrow without brace should be method
        assert_eq!(
            detect_completion_mode("$obj->", Point::new(0, 6)),
            CompletionMode::Method { invocant: "$obj".to_string() }
        );
    }

    // ---- Signature help ----

    #[test]
    fn test_sig_help_signature_sub() {
        let src = "sub add($x, $y) { }\nadd(1, 2);";
        let tree = parse(src);
        // Cursor after open paren: add(|
        let result = signature_help_at_point(&tree, src, Point::new(1, 4)).unwrap();
        assert_eq!(result.signature.name, "add");
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$x");
        assert_eq!(result.signature.params[1].name, "$y");
        assert_eq!(result.active_param, 0);
    }

    #[test]
    fn test_sig_help_active_param() {
        let src = "sub add($x, $y, $z) { }\nadd(1, 2, 3);";
        let tree = parse(src);
        // After first comma: add(1, |2, 3)
        let r = signature_help_at_point(&tree, src, Point::new(1, 7)).unwrap();
        assert_eq!(r.active_param, 1);
        // After second comma: add(1, 2, |3)
        let r = signature_help_at_point(&tree, src, Point::new(1, 10)).unwrap();
        assert_eq!(r.active_param, 2);
    }

    #[test]
    fn test_sig_help_legacy_sub() {
        let src = "sub greet { my ($name, $age) = @_; }\ngreet(\"bob\", 30);";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(1, 6)).unwrap();
        assert_eq!(result.signature.name, "greet");
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$name");
        assert_eq!(result.signature.params[1].name, "$age");
        assert_eq!(result.active_param, 0);
    }

    #[test]
    fn test_sig_help_method_hides_self() {
        let src = "package Foo;\nsub bar($self, $x, $y) { }\npackage main;\nmy $obj = Foo->new;\n$obj->bar(1, 2);";
        let tree = parse(src);
        // Cursor inside $obj->bar(|1, 2)
        let result = signature_help_at_point(&tree, src, Point::new(4, 10)).unwrap();
        assert_eq!(result.signature.name, "bar");
        assert_eq!(result.signature.is_method, true);
        // $self should be stripped
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$x");
        assert_eq!(result.signature.params[1].name, "$y");
    }

    #[test]
    fn test_sig_help_legacy_method_hides_self() {
        // Mimics sample.pl pattern: legacy sub with my ($self, ...) = @_
        let src = "package Calculator;\nsub add { my ($self, $a, $b) = @_; }\npackage main;\nmy $calc = Calculator->new;\n$calc->add(2, 3);";
        let tree = parse(src);
        eprintln!("tree:");
        dump_tree(tree.root_node(), src.as_bytes(), 0);
        // Cursor inside $calc->add(|2, 3)  — line 4, col 11
        let result = signature_help_at_point(&tree, src, Point::new(4, 11));
        eprintln!("result: {:?}", result.as_ref().map(|r| (&r.signature.name, &r.signature.params, r.active_param)));
        let result = result.unwrap();
        assert_eq!(result.signature.name, "add");
        // $self should be stripped for method call
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$a");
        assert_eq!(result.signature.params[1].name, "$b");
    }

    #[test]
    fn test_sig_help_incomplete_method() {
        // What happens when user types $calc->add( mid-edit
        let src = "package Calculator;\nsub add { my ($self, $a, $b) = @_; }\npackage main;\nmy $calc = Calculator->new;\n$calc->add(";
        let tree = parse(src);
        eprintln!("incomplete method tree:");
        dump_tree(tree.root_node(), src.as_bytes(), 0);
        let result = signature_help_at_point(&tree, src, Point::new(4, 11));
        eprintln!("incomplete method result: {:?}", result.as_ref().map(|r| (&r.signature.name, &r.signature.params, r.active_param)));
        // Should find add even in ERROR
        assert!(result.is_some(), "should find signature for incomplete method call");
        let r = result.unwrap();
        assert_eq!(r.signature.name, "add");
    }

    #[test]
    fn test_sig_help_unknown_function() {
        let src = "unknown_func(1, 2);";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(0, 13));
        assert!(result.is_none());
    }

    #[test]
    fn test_sig_help_incomplete_call() {
        // Mid-typing: cursor after open paren in ERROR node
        let src = "sub foo($a, $b) { }\nfoo(";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(1, 4));
        // Should find the function even in an ERROR node
        if let Some(r) = result {
            assert_eq!(r.signature.name, "foo");
            assert_eq!(r.signature.params.len(), 2);
            assert_eq!(r.active_param, 0);
        }
        // It's OK if this returns None for now — ERROR support is best-effort
    }

    #[test]
    fn test_sig_help_incomplete_after_first_arg() {
        let src = "sub foo($a, $b, $c) { }\nfoo($x, ";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(1, 8));
        if let Some(r) = result {
            assert_eq!(r.signature.name, "foo");
            assert_eq!(r.active_param, 1);
        }
    }

    #[test]
    fn test_sig_help_optional_param() {
        let src = "sub greet($name, $greeting = \"hello\") { }\ngreet(\"bob\");";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(1, 6)).unwrap();
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$name");
        assert_eq!(result.signature.params[1].name, "$greeting");
        // Optional param should have a default
        assert!(result.signature.params[1].default.is_some());
    }

    #[test]
    fn test_sig_help_slurpy_param() {
        let src = "sub process($first, @rest) { }\nprocess(1, 2, 3);";
        let tree = parse(src);
        let result = signature_help_at_point(&tree, src, Point::new(1, 8)).unwrap();
        assert_eq!(result.signature.params.len(), 2);
        assert_eq!(result.signature.params[0].name, "$first");
        assert_eq!(result.signature.params[1].name, "@rest");
        assert!(result.signature.params[1].is_slurpy);
    }

    // ---- Keyval arg completion ----

    #[test]
    fn test_keyval_signature_sub() {
        let src = "sub connect($self, %opts) {\n    my $h = $opts{host};\n    my $p = $opts{port};\n}\n$obj->connect(";
        let tree = parse(src);
        // Cursor right after ( — key position (line 4, col 14)
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(4, 14));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"host =>"), "should suggest host, got: {:?}", labels);
        assert!(labels.contains(&"port =>"), "should suggest port, got: {:?}", labels);
    }

    #[test]
    fn test_keyval_legacy_sub() {
        // Bare function call — tree-sitter drops "new" from ERROR, recovered from source
        let src = "sub new {\n    my ($class, %args) = @_;\n    my $v = $args{verbose};\n}\nnew(";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(4, 4));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"verbose =>"), "should suggest verbose, got: {:?}", labels);
    }

    #[test]
    fn test_keyval_value_position() {
        // After `=>` we're in value position — no keyval completions
        let src = "sub connect($self, %opts) {\n    $opts{host};\n}\n$obj->connect(host => ";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(3, 22));
        assert!(candidates.is_empty(), "should be empty in value position, got: {:?}", candidates.iter().map(|c| &c.label).collect::<Vec<_>>());
    }

    #[test]
    fn test_keyval_filters_used_keys() {
        let src = "sub connect($self, %opts) {\n    $opts{host};\n    $opts{port};\n}\n$obj->connect(host => 'x', ";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(4, 27));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(!labels.contains(&"host =>"), "host should be filtered out");
        assert!(labels.contains(&"port =>"), "port should still be suggested");
    }

    #[test]
    fn test_keyval_no_slurpy() {
        // Sub without slurpy hash — no keyval completions
        let src = "sub add($x, $y) { }\nadd(";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(1, 4));
        assert!(candidates.is_empty());
    }

    #[test]
    fn test_keyval_through_general_completion() {
        // Verify keyval candidates appear in the full completion flow (bare call)
        let src = "sub new {\n    my ($class, %args) = @_;\n    $args{verbose};\n}\nnew(";
        let tree = parse(src);
        let all = collect_completions(&tree, src, Point::new(4, 4));
        let has_keyval = all.iter().any(|c| c.label == "verbose =>");
        assert!(has_keyval, "keyval should appear in general completions, got: {:?}", all.iter().map(|c| &c.label).collect::<Vec<_>>());
    }

    #[test]
    fn test_keyval_bare_call_mid_file() {
        // Bare function call with code after it — tree splits into
        // expression_statement > bareword("new") + ERROR > (
        let src = "sub new {\n    my ($class, %args) = @_;\n    $args{verbose};\n}\nnew(\nmy $x = 1;\n";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(4, 4));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"verbose =>"), "bare call mid-file should suggest verbose, got: {:?}", labels);
    }

    #[test]
    fn test_keyval_core_class_params() {
        // Core class with :param fields — implicit new should suggest field names
        let src = "use v5.38;\nclass Point {\n    field $x :param;\n    field $y :param;\n    field $label = 'pt';\n}\nPoint->new(";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(6, 11));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"x =>"), "should suggest x, got: {:?}", labels);
        assert!(labels.contains(&"y =>"), "should suggest y, got: {:?}", labels);
        // $label has no :param, should not appear
        assert!(!labels.contains(&"label =>"), "label has no :param, should not appear");
    }

    #[test]
    fn test_keyval_core_class_filters_used() {
        let src = "use v5.38;\nclass Point {\n    field $x :param;\n    field $y :param;\n}\nPoint->new(x => 1, ";
        let tree = parse(src);
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(5, 19));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(!labels.contains(&"x =>"), "x should be filtered out");
        assert!(labels.contains(&"y =>"), "y should still be suggested");
    }

    #[test]
    fn test_keyval_scoped_to_class() {
        // Two packages with sub new — should resolve to the correct one
        let src = "package Foo;\nsub new {\n    my ($class, %args) = @_;\n    $args{alpha};\n}\npackage Bar;\nsub new {\n    my ($class, %args) = @_;\n    $args{beta};\n}\nBar->new(";
        let tree = parse(src);
        // Bar->new( is on line 10 (0-indexed), cursor after (
        let candidates = collect_keyval_arg_completions(&tree, src.as_bytes(), Point::new(10, 9));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"beta =>"), "should suggest beta (Bar's key), got: {:?}", labels);
        assert!(!labels.contains(&"alpha =>"), "should NOT suggest alpha (Foo's key)");
    }

    #[test]
    fn test_method_completion_core_class_has_new() {
        // Core class (class keyword) should always suggest `new` even without explicit sub new
        let src = "use v5.38;\nclass Point {\n    field $x :param;\n    method magnitude() { sqrt($x) }\n}\nPoint->";
        let tree = parse(src);
        let candidates = collect_completions(&tree, std::str::from_utf8(src.as_bytes()).unwrap(), Point::new(5, 7));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"new"), "should suggest implicit new, got: {:?}", labels);
        assert!(labels.contains(&"magnitude"), "should suggest declared method, got: {:?}", labels);
    }

    #[test]
    fn test_method_completion_package_scoped() {
        // Package invocant should only show that package's subs
        let src = "package Foo;\nsub alpha { 1 }\npackage Bar;\nsub beta { 2 }\nBar->";
        let tree = parse(src);
        let candidates = collect_completions(&tree, std::str::from_utf8(src.as_bytes()).unwrap(), Point::new(4, 5));
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"beta"), "should suggest beta, got: {:?}", labels);
        assert!(!labels.contains(&"alpha"), "should NOT suggest alpha (Foo's sub)");
    }
}
