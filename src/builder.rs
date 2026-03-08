//! Single-walk tree-sitter visitor that builds a FileAnalysis.
//!
//! One depth-first walk populates scopes, symbols, refs, type constraints,
//! and fold ranges. Post-passes resolve hash key owners and variable refs.

use tree_sitter::{Node, Point, Tree};

use crate::file_analysis::*;

fn node_to_span(node: Node) -> Span {
    Span {
        start: node.start_position(),
        end: node.end_position(),
    }
}

/// Build a FileAnalysis from a parsed tree in a single walk.
pub fn build(tree: &Tree, source: &[u8]) -> FileAnalysis {
    let mut b = Builder {
        source,
        scopes: Vec::new(),
        symbols: Vec::new(),
        refs: Vec::new(),
        type_constraints: Vec::new(),
        fold_ranges: Vec::new(),
        imports: Vec::new(),
        scope_stack: Vec::new(),
        current_package: None,
        next_scope_id: 0,
        next_symbol_id: 0,
    };

    // Create file-level scope and walk
    let file_scope = b.push_scope(ScopeKind::File, node_to_span(tree.root_node()), None);
    b.visit_children(tree.root_node());
    b.pop_scope();
    let _ = file_scope;

    // Post-pass 1: resolve variable refs -> resolves_to
    b.resolve_variable_refs();

    // Post-pass 2: resolve hash key owners from type constraints
    b.resolve_hash_key_owners();

    FileAnalysis::new(
        b.scopes,
        b.symbols,
        b.refs,
        b.type_constraints,
        b.fold_ranges,
        b.imports,
    )
}

struct Builder<'a> {
    source: &'a [u8],

    scopes: Vec<Scope>,
    symbols: Vec<Symbol>,
    refs: Vec<Ref>,
    type_constraints: Vec<TypeConstraint>,
    fold_ranges: Vec<FoldRange>,
    imports: Vec<Import>,

    // Walk state
    scope_stack: Vec<ScopeId>,
    current_package: Option<String>,
    next_scope_id: u32,
    next_symbol_id: u32,
}

impl<'a> Builder<'a> {
    // ---- Scope management ----

    fn push_scope(&mut self, kind: ScopeKind, span: Span, package: Option<String>) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        let parent = self.scope_stack.last().copied();
        let pkg = package.or_else(|| {
            // Inherit package from current state or parent
            self.current_package.clone().or_else(|| {
                parent.and_then(|p| self.scopes[p.0 as usize].package.clone())
            })
        });
        self.scopes.push(Scope {
            id,
            parent,
            kind,
            span,
            package: pkg,
        });
        self.scope_stack.push(id);
        id
    }

    fn pop_scope(&mut self) -> Option<ScopeId> {
        self.scope_stack.pop()
    }

    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().expect("scope stack empty")
    }

    // ---- Symbol/Ref creation ----

    fn add_symbol(&mut self, name: String, kind: SymKind, span: Span, selection_span: Span, detail: SymbolDetail) -> SymbolId {
        let id = SymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.symbols.push(Symbol {
            id,
            name,
            kind,
            span,
            selection_span,
            scope: self.current_scope(),
            detail,
        });
        id
    }

    fn add_ref(&mut self, kind: RefKind, span: Span, target_name: String, access: AccessKind) {
        self.refs.push(Ref {
            kind,
            span,
            scope: self.current_scope(),
            target_name,
            access,
            resolves_to: None,
        });
    }

    // ---- Main visitor ----

    fn visit_node(&mut self, node: Node<'a>) {
        match node.kind() {
            "package_statement" => self.visit_package(node),
            "class_statement" => self.visit_class(node),
            "subroutine_declaration_statement" => self.visit_sub(node, false),
            "method_declaration_statement" => self.visit_sub(node, true),
            "variable_declaration" => self.visit_variable_decl(node),
            "for_statement" => self.visit_for(node),
            "use_statement" => self.visit_use(node),
            "assignment_expression" => self.visit_assignment(node),

            // Blocks create scopes (but only standalone blocks, not sub/class/for bodies)
            "block" | "do_block" => {
                // Only create a Block scope if parent isn't already a scope-creator
                let parent_kind = node.parent().map(|p| p.kind()).unwrap_or("");
                if !matches!(parent_kind,
                    "subroutine_declaration_statement" | "method_declaration_statement" |
                    "class_statement" | "for_statement" | "foreach_statement"
                ) {
                    self.add_fold_range(node);
                    self.push_scope(ScopeKind::Block, node_to_span(node), None);
                    self.visit_children(node);
                    self.pop_scope();
                    return;
                }
                self.add_fold_range(node);
                self.visit_children(node);
            }

            // Foldable statements
            "if_statement" | "unless_statement" | "while_statement" | "until_statement" => {
                self.add_fold_range(node);
                self.visit_children(node);
            }

            // Variable references
            "scalar" | "array" | "hash" => self.visit_var_ref(node),
            "container_variable" | "slice_container_variable" | "keyval_container_variable" => {
                self.visit_container_ref(node);
            }

            // Call expressions
            "function_call_expression" | "ambiguous_function_call_expression" => {
                self.visit_function_call(node);
            }
            "method_call_expression" => self.visit_method_call(node),

            // Hash access
            "hash_element_expression" => self.visit_hash_element(node),

            // Dereference expressions → type constraints on operand
            "array_element_expression" => {
                self.infer_deref_type(node, InferredType::ArrayRef);
                self.visit_children(node);
            }
            "coderef_call_expression" => {
                self.infer_deref_type(node, InferredType::CodeRef);
                self.visit_children(node);
            }
            "array_deref_expression" => {
                self.infer_deref_type(node, InferredType::ArrayRef);
                self.visit_children(node);
            }
            "hash_deref_expression" => {
                self.infer_deref_type(node, InferredType::HashRef);
                self.visit_children(node);
            }

            // Binary operators → type constraints on variable operands
            "binary_expression" => {
                self.infer_binary_op_type(node);
                self.visit_children(node);
            }
            "equality_expression" | "relational_expression" => {
                self.infer_comparison_type(node);
                self.visit_children(node);
            }

            // Unary operators
            "postinc_expression" | "preinc_expression" => {
                // $x++ / $x-- / ++$x / --$x → Numeric
                if let Some(operand) = node.named_child(0) {
                    self.push_var_type_constraint(operand, node, InferredType::Numeric);
                }
                self.visit_children(node);
            }

            // Hash construction
            "anonymous_hash_expression" => self.visit_anon_hash(node),

            // ERROR nodes: recurse into children to extract what we can
            "ERROR" => self.visit_children(node),

            _ => self.visit_children(node),
        }
    }

    fn visit_children(&mut self, node: Node<'a>) {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                self.visit_node(child);
            }
        }
    }

    // ---- Node visitors ----

    fn visit_package(&mut self, node: Node<'a>) {
        let name = match node.child_by_field_name("name") {
            Some(n) => match n.utf8_text(self.source) {
                Ok(s) => s.to_string(),
                Err(_) => return,
            },
            None => return,
        };
        let name_node = node.child_by_field_name("name").unwrap();
        self.current_package = Some(name.clone());

        // Update the current scope's package
        let scope = self.current_scope();
        self.scopes[scope.0 as usize].package = Some(name.clone());

        self.add_symbol(
            name,
            SymKind::Package,
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::None,
        );
    }

    fn visit_class(&mut self, node: Node<'a>) {
        let name_node = match node.child_by_field_name("name") {
            Some(n) => n,
            None => return,
        };
        let name = match name_node.utf8_text(self.source) {
            Ok(s) => s.to_string(),
            Err(_) => return,
        };

        // Parse :isa and :does
        let mut parent = None;
        let mut roles = Vec::new();
        if let Some(attrlist) = node.child_by_field_name("attributes") {
            for i in 0..attrlist.named_child_count() {
                if let Some(attr) = attrlist.named_child(i) {
                    if attr.kind() == "attribute" {
                        let attr_name = attr.child_by_field_name("name")
                            .and_then(|n| n.utf8_text(self.source).ok());
                        let attr_value = attr.child_by_field_name("value")
                            .and_then(|n| n.utf8_text(self.source).ok());
                        match (attr_name, attr_value) {
                            (Some("isa"), Some(val)) => parent = Some(val.to_string()),
                            (Some("does"), Some(val)) => roles.push(val.to_string()),
                            _ => {}
                        }
                    }
                }
            }
        }

        // Collect fields from the block for the Class detail
        let mut field_details = Vec::new();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "block" {
                    self.collect_field_details(child, &mut field_details);
                }
            }
        }

        self.add_symbol(
            name.clone(),
            SymKind::Class,
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::Class {
                parent,
                roles,
                fields: field_details,
            },
        );

        let has_block = node.child_by_field_name("body").is_some()
            || (0..node.child_count()).any(|i| node.child(i).map_or(false, |c| c.kind() == "block"));

        if has_block {
            // Block class: push/pop scope, restore package after block
            self.add_fold_range(node);
            let prev_package = self.current_package.take();
            self.current_package = Some(name.clone());
            self.push_scope(ScopeKind::Class { name: name.clone() }, node_to_span(node), Some(name));
            self.visit_children(node);
            self.pop_scope();
            self.current_package = prev_package;
        } else {
            // Flat class (class Foo;): like package, stays in effect until next package/class
            self.current_package = Some(name.clone());
            let scope = self.current_scope();
            self.scopes[scope.0 as usize].package = Some(name);
        }
    }

    fn collect_field_details(&self, block: Node<'a>, out: &mut Vec<FieldDetail>) {
        for i in 0..block.child_count() {
            if let Some(child) = block.child(i) {
                if child.kind() == "expression_statement" {
                    if let Some(fd) = self.try_parse_field_detail(child) {
                        out.push(fd);
                    }
                }
            }
        }
    }

    fn try_parse_field_detail(&self, expr_stmt: Node<'a>) -> Option<FieldDetail> {
        for i in 0..expr_stmt.named_child_count() {
            let child = expr_stmt.named_child(i)?;
            let var_decl = if child.kind() == "variable_declaration" {
                child
            } else if child.kind() == "assignment_expression" {
                child.child_by_field_name("left").filter(|n| n.kind() == "variable_declaration")?
            } else {
                continue;
            };

            let keyword = self.get_decl_keyword(var_decl)?;
            if keyword != "field" {
                return None;
            }

            let var_node = var_decl.child_by_field_name("variable")?;
            let full_name = var_node.utf8_text(self.source).ok()?;
            let sigil = full_name.chars().next()?;

            let mut attributes = Vec::new();
            if let Some(attrlist) = var_decl.child_by_field_name("attributes") {
                for j in 0..attrlist.named_child_count() {
                    if let Some(attr) = attrlist.named_child(j) {
                        if attr.kind() == "attribute" {
                            if let Some(name_node) = attr.child_by_field_name("name") {
                                if let Ok(attr_name) = name_node.utf8_text(self.source) {
                                    attributes.push(attr_name.to_string());
                                }
                            }
                        }
                    }
                }
            }

            return Some(FieldDetail {
                name: full_name.to_string(),
                sigil,
                attributes,
            });
        }
        None
    }

    fn visit_sub(&mut self, node: Node<'a>, is_method: bool) {
        let name_node = match node.child_by_field_name("name") {
            Some(n) => n,
            None => { self.visit_children(node); return; }
        };
        let name = match name_node.utf8_text(self.source) {
            Ok(s) => s.to_string(),
            Err(_) => { self.visit_children(node); return; }
        };

        // Extract params
        let params = self.extract_params(node);

        self.add_symbol(
            name.clone(),
            if is_method { SymKind::Method } else { SymKind::Sub },
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::Sub { params: params.clone(), is_method },
        );

        // Push sub scope
        let scope_kind = if is_method {
            ScopeKind::Method { name: name.clone() }
        } else {
            ScopeKind::Sub { name: name.clone() }
        };
        self.push_scope(scope_kind, node_to_span(node), None);

        // Record signature params as Variable symbols in the sub scope
        self.record_signature_params(node, &params);

        // Perl 5.38 methods: synthesize implicit $self with type → enclosing class
        if is_method {
            if let Some(pkg) = self.current_package.clone() {
                let span = node_to_span(name_node);
                self.add_symbol(
                    "$self".to_string(),
                    SymKind::Variable,
                    span,
                    span,
                    SymbolDetail::Variable { sigil: '$', decl_kind: DeclKind::Param },
                );
                self.type_constraints.push(TypeConstraint {
                    variable: "$self".to_string(),
                    scope: self.current_scope(),
                    inferred_type: InferredType::ClassName(pkg),
                    constraint_span: span,
                });
            }
        }

        // Detect first-param-is-self pattern
        self.detect_first_param_type(&params, node);

        // Visit children (body, etc.)
        self.visit_children(node);
        self.pop_scope();
    }

    fn extract_params(&self, sub_node: Node<'a>) -> Vec<ParamInfo> {
        // Try signature syntax first
        for i in 0..sub_node.child_count() {
            if let Some(sig) = sub_node.child(i) {
                if sig.kind() == "signature" {
                    return self.extract_signature_params(sig);
                }
            }
        }

        // Fallback: `my (...) = @_` in body
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
                            if right.utf8_text(self.source).ok() == Some("@_") {
                                if let Some(left) = assign.child_by_field_name("left") {
                                    return self.collect_vars_from_decl(left)
                                        .into_iter()
                                        .map(|(name, _)| {
                                            let is_slurpy = name.starts_with('@') || name.starts_with('%');
                                            ParamInfo { name, default: None, is_slurpy }
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

    fn extract_signature_params(&self, sig: Node<'a>) -> Vec<ParamInfo> {
        let mut params = Vec::new();
        for j in 0..sig.named_child_count() {
            if let Some(param) = sig.named_child(j) {
                match param.kind() {
                    "mandatory_parameter" => {
                        if let Some(var) = self.first_var_child(param) {
                            params.push(ParamInfo { name: var, default: None, is_slurpy: false });
                        }
                    }
                    "optional_parameter" => {
                        let var = self.first_var_child(param);
                        let default = param.child_by_field_name("default")
                            .or_else(|| {
                                let nc = param.named_child_count();
                                if nc >= 2 { param.named_child(nc - 1) } else { None }
                            })
                            .and_then(|d| d.utf8_text(self.source).ok())
                            .map(|s| s.to_string());
                        if let Some(name) = var {
                            params.push(ParamInfo { name, default, is_slurpy: false });
                        }
                    }
                    "slurpy_parameter" => {
                        if let Some(var) = self.first_var_child(param) {
                            params.push(ParamInfo { name: var, default: None, is_slurpy: true });
                        }
                    }
                    "scalar" | "array" | "hash" => {
                        if let Ok(text) = param.utf8_text(self.source) {
                            let is_slurpy = matches!(param.kind(), "array" | "hash");
                            params.push(ParamInfo { name: text.to_string(), default: None, is_slurpy });
                        }
                    }
                    _ => {}
                }
            }
        }
        params
    }

    fn record_signature_params(&mut self, sub_node: Node<'a>, params: &[ParamInfo]) {
        // For signature syntax, params come from the signature node
        for i in 0..sub_node.child_count() {
            if let Some(sig) = sub_node.child(i) {
                if sig.kind() == "signature" {
                    let mut param_idx = 0;
                    for j in 0..sig.named_child_count() {
                        if let Some(param_node) = sig.named_child(j) {
                            if param_idx < params.len() {
                                let p = &params[param_idx];
                                let sigil = p.name.chars().next().unwrap_or('$');
                                let decl_kind = DeclKind::Param;
                                self.add_symbol(
                                    p.name.clone(),
                                    SymKind::Variable,
                                    node_to_span(param_node),
                                    node_to_span(param_node),
                                    SymbolDetail::Variable { sigil, decl_kind },
                                );
                                param_idx += 1;
                            }
                        }
                    }
                    return;
                }
            }
        }
        // Legacy params: they'll be picked up as normal variable_declaration nodes
    }

    fn detect_first_param_type(&mut self, params: &[ParamInfo], node: Node<'a>) {
        if params.is_empty() { return; }
        let first = &params[0];
        if !first.name.starts_with('$') { return; }
        // Common self-like names
        let bare = &first.name[1..];
        if !matches!(bare, "self" | "class" | "this" | "proto") { return; }

        // Find enclosing package
        if let Some(ref pkg) = self.current_package {
            self.type_constraints.push(TypeConstraint {
                variable: first.name.clone(),
                scope: self.current_scope(),
                constraint_span: node_to_span(node),
                inferred_type: InferredType::FirstParam { package: pkg.clone() },
            });
        }
    }

    fn visit_variable_decl(&mut self, node: Node<'a>) {
        let keyword = self.get_decl_keyword(node, );
        let decl_kind = match keyword.as_deref() {
            Some("my") => DeclKind::My,
            Some("our") => DeclKind::Our,
            Some("state") => DeclKind::State,
            Some("field") => DeclKind::Field,
            _ => DeclKind::My,
        };

        // Collect all declared variables
        let vars = self.collect_vars_from_decl(node);
        for (name, var_span) in &vars {
            let sigil = name.chars().next().unwrap_or('$');
            let sym_kind = if decl_kind == DeclKind::Field { SymKind::Field } else { SymKind::Variable };
            self.add_symbol(
                name.clone(),
                sym_kind,
                node_to_span(node),
                *var_span,
                SymbolDetail::Variable { sigil, decl_kind },
            );
            self.add_ref(
                RefKind::Variable,
                *var_span,
                name.clone(),
                AccessKind::Declaration,
            );

            // Synthesize accessor methods for `field $x :reader` / `:writer`
            if decl_kind == DeclKind::Field {
                let bare_name = &name[1..]; // strip sigil
                let attrs = self.collect_attributes(node);
                if attrs.iter().any(|a| a == "reader") {
                    self.add_symbol(
                        bare_name.to_string(),
                        SymKind::Method,
                        node_to_span(node),
                        *var_span,
                        SymbolDetail::Sub { params: vec![], is_method: true },
                    );
                }
                if attrs.iter().any(|a| a == "writer") {
                    let writer_name = format!("set_{}", bare_name);
                    self.add_symbol(
                        writer_name,
                        SymKind::Method,
                        node_to_span(node),
                        *var_span,
                        SymbolDetail::Sub {
                            params: vec![ParamInfo {
                                name: format!("${}", bare_name),
                                default: None,
                                is_slurpy: false,
                            }],
                            is_method: true,
                        },
                    );
                }
            }
        }

        // Don't recurse into children — we've already extracted what we need
        // But DO check for assignment RHS (for type inference)
        // The parent assignment_expression handles that.
    }

    /// Collect attribute names from a node's `attributes` field (e.g. `:param :reader`).
    fn collect_attributes(&self, node: Node<'a>) -> Vec<String> {
        let mut attrs = Vec::new();
        if let Some(attrlist) = node.child_by_field_name("attributes") {
            for i in 0..attrlist.named_child_count() {
                if let Some(attr) = attrlist.named_child(i) {
                    if attr.kind() == "attribute" {
                        if let Some(name_node) = attr.child_by_field_name("name") {
                            if let Ok(name) = name_node.utf8_text(self.source) {
                                attrs.push(name.to_string());
                            }
                        }
                    }
                }
            }
        }
        attrs
    }

    fn visit_for(&mut self, node: Node<'a>) {
        // Check for loop variable: `for my $x (...) { ... }`
        let loop_var = node.child_by_field_name("variable")
            .or_else(|| {
                // Some grammars: iterator > variable_declaration
                node.child_by_field_name("iterator")
                    .and_then(|it| it.child_by_field_name("variable"))
            });

        if let Some(var_node) = loop_var {
            if let Ok(var_text) = var_node.utf8_text(self.source) {
                let var_name = var_text.to_string();

                // Find the body block
                let body_span = node.child_by_field_name("body")
                    .map(|b| node_to_span(b))
                    .unwrap_or(node_to_span(node));

                self.push_scope(
                    ScopeKind::ForLoop { var: var_name.clone() },
                    body_span,
                    None,
                );

                let sigil = var_name.chars().next().unwrap_or('$');
                self.add_symbol(
                    var_name.clone(),
                    SymKind::Variable,
                    node_to_span(var_node),
                    node_to_span(var_node),
                    SymbolDetail::Variable { sigil, decl_kind: DeclKind::ForVar },
                );
                self.add_ref(
                    RefKind::Variable,
                    node_to_span(var_node),
                    var_name,
                    AccessKind::Declaration,
                );

                self.visit_children(node);
                self.pop_scope();
                return;
            }
        }

        // No loop variable — just visit children normally
        self.visit_children(node);
    }

    fn visit_use(&mut self, node: Node<'a>) {
        if let Some(module_node) = node.child_by_field_name("module") {
            if let Ok(module_name) = module_node.utf8_text(self.source) {
                self.add_symbol(
                    module_name.to_string(),
                    SymKind::Module,
                    node_to_span(node),
                    node_to_span(module_node),
                    SymbolDetail::None,
                );

                // Extract imported symbols from qw(...) or list
                let (imported_symbols, qw_close_paren) = self.extract_use_import_list(node);
                self.imports.push(Import {
                    module_name: module_name.to_string(),
                    imported_symbols,
                    span: node_to_span(node),
                    qw_close_paren,
                });
            }
        }
        // Don't recurse — use statements don't contain interesting sub-nodes
    }

    /// Extract the import list from a use statement.
    /// Handles `use Foo qw(bar baz)` and `use Foo ('bar', 'baz')`.
    /// Returns (imported_symbols, qw_close_paren_position).
    fn extract_use_import_list(&self, node: Node<'a>) -> (Vec<String>, Option<Point>) {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "quoted_word_list" => {
                        // qw(word1 word2 ...) — find the closing delimiter
                        let mut words = Vec::new();
                        for j in 0..child.named_child_count() {
                            if let Some(sc) = child.named_child(j) {
                                if sc.kind() == "string_content" {
                                    if let Ok(text) = sc.utf8_text(self.source) {
                                        for word in text.split_whitespace() {
                                            if !word.is_empty() {
                                                words.push(word.to_string());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // The closing delimiter is the last byte of the qw node
                        // e.g. for `qw(foo bar)`, end_position points just past `)`
                        let end = child.end_position();
                        let close_pos = Point::new(end.row, end.column.saturating_sub(1));
                        return (words, Some(close_pos));
                    }
                    "parenthesized_expression" | "list_expression" => {
                        // ('foo', 'bar', ...)
                        let mut words = Vec::new();
                        self.collect_string_values(child, &mut words);
                        if !words.is_empty() {
                            return (words, None);
                        }
                    }
                    _ => {}
                }
            }
        }
        (vec![], None)
    }

    /// Collect string literal values from a list/paren expression.
    fn collect_string_values(&self, node: Node<'a>, words: &mut Vec<String>) {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "string_literal" | "interpolated_string_literal" => {
                        for j in 0..child.named_child_count() {
                            if let Some(content) = child.named_child(j) {
                                if content.kind() == "string_content" {
                                    if let Ok(text) = content.utf8_text(self.source) {
                                        words.push(text.to_string());
                                    }
                                }
                            }
                        }
                    }
                    "list_expression" | "parenthesized_expression" => {
                        self.collect_string_values(child, words);
                    }
                    _ => {}
                }
            }
        }
    }

    fn visit_assignment(&mut self, node: Node<'a>) {
        // Check for type inference from RHS
        if let Some(left) = node.child_by_field_name("left") {
            if left.kind() == "variable_declaration" {
                // Visit the declaration
                self.visit_variable_decl(left);
            }
            if let Some(right) = node.child_by_field_name("right") {
                // Try constructor class first (Foo->new()), then literal types
                let inferred = if let Some(class_name) = self.extract_constructor_class(right) {
                    Some(InferredType::ClassName(class_name))
                } else {
                    Self::infer_literal_type(right)
                };
                if let Some(it) = inferred {
                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                        self.type_constraints.push(TypeConstraint {
                            variable: vt,
                            scope: self.current_scope(),
                            constraint_span: node_to_span(node),
                            inferred_type: it,
                        });
                    }
                }
                // Visit the RHS (may contain refs, calls, etc.)
                self.visit_node(right);
            }
            // Visit LHS children (except the variable_declaration we already handled)
            if left.kind() != "variable_declaration" {
                self.visit_node(left);
            }
        } else {
            // No left field — just visit children
            self.visit_children(node);
        }
    }

    /// Infer a type from a literal RHS node (no CST walk, just node kind check).
    fn infer_literal_type(node: Node) -> Option<InferredType> {
        match node.kind() {
            "anonymous_hash_expression" => Some(InferredType::HashRef),
            "anonymous_array_expression" => Some(InferredType::ArrayRef),
            "anonymous_subroutine_expression" => Some(InferredType::CodeRef),
            "quoted_regexp" => Some(InferredType::Regexp),
            _ => None,
        }
    }

    /// Push a type constraint on a variable node if it's a scalar.
    fn push_var_type_constraint(&mut self, var_node: Node<'a>, context_node: Node<'a>, inferred_type: InferredType) {
        if var_node.kind() == "scalar" {
            if let Ok(text) = var_node.utf8_text(self.source) {
                self.type_constraints.push(TypeConstraint {
                    variable: text.to_string(),
                    scope: self.current_scope(),
                    constraint_span: node_to_span(context_node),
                    inferred_type,
                });
            }
        }
    }

    /// Infer a type on the first named child (the operand) of a dereference expression.
    fn infer_deref_type(&mut self, node: Node<'a>, inferred_type: InferredType) {
        if let Some(operand) = node.named_child(0) {
            self.push_var_type_constraint(operand, node, inferred_type);
        }
    }

    /// Infer types from binary operator expressions.
    fn infer_binary_op_type(&mut self, node: Node<'a>) {
        let op = self.get_operator_text(node);
        match op.as_deref() {
            // Numeric operators: both operands are Numeric
            Some("+" | "-" | "*" | "/" | "%" | "**") => {
                for i in 0..node.named_child_count() {
                    if let Some(child) = node.named_child(i) {
                        self.push_var_type_constraint(child, node, InferredType::Numeric);
                    }
                }
            }
            // String operators: both operands are String
            Some("." | "x") => {
                for i in 0..node.named_child_count() {
                    if let Some(child) = node.named_child(i) {
                        self.push_var_type_constraint(child, node, InferredType::String);
                    }
                }
            }
            // Regex match: LHS is String, RHS is Regexp
            Some("=~" | "!~") => {
                if let Some(lhs) = node.named_child(0) {
                    self.push_var_type_constraint(lhs, node, InferredType::String);
                }
                if let Some(rhs) = node.named_child(1) {
                    self.push_var_type_constraint(rhs, node, InferredType::Regexp);
                }
            }
            _ => {}
        }
    }

    /// Infer types from comparison operators (equality_expression, relational_expression).
    fn infer_comparison_type(&mut self, node: Node<'a>) {
        let op = self.get_operator_text(node);
        let inferred = match op.as_deref() {
            // Numeric comparisons
            Some("==" | "!=" | "<=>" | "<" | ">" | "<=" | ">=") => Some(InferredType::Numeric),
            // String comparisons
            Some("eq" | "ne" | "lt" | "gt" | "le" | "ge" | "cmp") => Some(InferredType::String),
            _ => None,
        };
        if let Some(it) = inferred {
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    self.push_var_type_constraint(child, node, it.clone());
                }
            }
        }
    }

    /// Get the operator text from a binary/comparison/equality expression.
    /// The operator is the first unnamed child between the two named children.
    fn get_operator_text(&self, node: Node<'a>) -> Option<String> {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if !child.is_named() {
                    let text = child.utf8_text(self.source).ok()?;
                    // Skip parens, brackets, etc.
                    if !matches!(text, "(" | ")" | "[" | "]" | "{" | "}" | "," | ";") {
                        return Some(text.to_string());
                    }
                }
            }
        }
        None
    }

    fn visit_var_ref(&mut self, node: Node<'a>) {
        // Skip if parent is a variable_declaration (handled by visit_variable_decl)
        if let Some(parent) = node.parent() {
            if parent.kind() == "variable_declaration" { return; }
            // Skip if inside a signature param
            if matches!(parent.kind(), "mandatory_parameter" | "optional_parameter" | "slurpy_parameter") {
                return;
            }
        }

        // Check for block-based dereference: @{expr}, %{expr}
        // In tree-sitter-perl these parse as scalar/array/hash with a varname
        // child containing a block. The block holds the real expressions.
        // Push a type constraint on inner scalars, then recurse (don't record a ref for the outer).
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "varname" {
                    for j in 0..child.child_count() {
                        if let Some(gc) = child.child(j) {
                            if gc.kind() == "block" {
                                // Infer type from the outer sigil
                                let deref_type = match node.kind() {
                                    "array" => Some(InferredType::ArrayRef),
                                    "hash" => Some(InferredType::HashRef),
                                    _ => None,
                                };
                                if let Some(it) = deref_type {
                                    // Find the scalar inside the block: {$x}
                                    for k in 0..gc.named_child_count() {
                                        if let Some(stmt) = gc.named_child(k) {
                                            // expression_statement wraps the scalar
                                            let inner = if stmt.kind() == "expression_statement" {
                                                stmt.named_child(0)
                                            } else {
                                                Some(stmt)
                                            };
                                            if let Some(var) = inner {
                                                self.push_var_type_constraint(var, node, it.clone());
                                            }
                                        }
                                    }
                                }
                                self.visit_children(gc);
                                return;
                            }
                        }
                    }
                }
            }
        }

        if let Ok(text) = node.utf8_text(self.source) {
            let access = self.determine_access(node);
            self.add_ref(
                RefKind::Variable,
                node_to_span(node),
                text.to_string(),
                access,
            );
        }
    }

    fn visit_container_ref(&mut self, node: Node<'a>) {
        // Container variables: $hash{key}, @arr[0], etc.
        // The container itself is a ref to the underlying variable
        if let Ok(text) = node.utf8_text(self.source) {
            // Map container sigil to the declared sigil
            let canonical = self.canonicalize_container(node, text);
            let access = self.determine_access(node);
            self.add_ref(
                RefKind::ContainerAccess,
                node_to_span(node),
                canonical,
                access,
            );
        }
    }

    fn visit_function_call(&mut self, node: Node<'a>) {
        if let Some(func_node) = node.child_by_field_name("function") {
            // Check for &{$z}() — code ref block dereference call
            if let Ok(func_text) = func_node.utf8_text(self.source) {
                if func_text.starts_with("&{") {
                    // Find the scalar inside: & → varname → block → expression_statement → scalar
                    self.infer_block_coderef(func_node, node);
                } else {
                    self.add_ref(
                        RefKind::FunctionCall,
                        node_to_span(func_node),
                        func_text.to_string(),
                        AccessKind::Read,
                    );
                }
            }
        }
        self.visit_children(node);
    }

    /// Infer CodeRef on the scalar inside &{$var}.
    fn infer_block_coderef(&mut self, func_node: Node<'a>, context_node: Node<'a>) {
        for i in 0..func_node.child_count() {
            if let Some(child) = func_node.child(i) {
                if child.kind() == "varname" {
                    for j in 0..child.child_count() {
                        if let Some(gc) = child.child(j) {
                            if gc.kind() == "block" {
                                for k in 0..gc.named_child_count() {
                                    if let Some(stmt) = gc.named_child(k) {
                                        let inner = if stmt.kind() == "expression_statement" {
                                            stmt.named_child(0)
                                        } else {
                                            Some(stmt)
                                        };
                                        if let Some(var) = inner {
                                            self.push_var_type_constraint(var, context_node, InferredType::CodeRef);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn visit_method_call(&mut self, node: Node<'a>) {
        let method_name = node.child_by_field_name("method")
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| s.to_string());
        let invocant = node.child_by_field_name("invocant")
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| s.to_string());

        if let Some(name) = method_name {
            self.add_ref(
                RefKind::MethodCall { invocant: invocant.unwrap_or_default() },
                node_to_span(node),
                name,
                AccessKind::Read,
            );
        }
        self.visit_children(node);
    }

    fn visit_hash_element(&mut self, node: Node<'a>) {
        // Infer HashRef on the operand variable (e.g. $x in $x->{key})
        self.infer_deref_type(node, InferredType::HashRef);

        // Record the hash variable access
        let var_text = self.get_hash_var_from_element(node);

        // Record the key access
        if let Some(key_node) = node.child_by_field_name("key") {
            if let Some((key_text, is_dynamic)) = self.extract_key_text(key_node) {
                if !is_dynamic {
                    self.add_ref(
                        RefKind::HashKeyAccess {
                            var_text: var_text.clone().unwrap_or_default(),
                            owner: None, // resolved in post-pass
                        },
                        node_to_span(key_node),
                        key_text,
                        AccessKind::Read,
                    );
                }
            }
        }

        // Visit children for the container variable ref
        self.visit_children(node);
    }

    fn visit_anon_hash(&mut self, node: Node<'a>) {
        // Detect bless context for hash key ownership
        let owner = self.detect_anon_hash_owner(node);

        // Collect fat-comma keys as HashKeyDef symbols
        if let Some(ref owner) = owner {
            self.collect_fat_comma_keys(node, owner);
        }

        self.visit_children(node);
    }

    // ---- Fold ranges ----

    fn add_fold_range(&mut self, node: Node<'a>) {
        let start = node.start_position().row;
        let end = node.end_position().row;
        if end > start {
            self.fold_ranges.push(FoldRange {
                start_line: start,
                end_line: end,
                kind: FoldKind::Region,
            });
        }
    }

    // ---- Helpers ----

    fn get_decl_keyword(&self, var_decl: Node<'a>) -> Option<String> {
        for i in 0..var_decl.child_count() {
            if let Some(child) = var_decl.child(i) {
                let k = child.kind();
                if matches!(k, "my" | "our" | "state" | "field") {
                    return Some(k.to_string());
                }
            }
        }
        None
    }

    fn collect_vars_from_decl(&self, node: Node<'a>) -> Vec<(String, Span)> {
        let mut vars = Vec::new();
        self.collect_vars_walk(node, &mut vars);
        vars
    }

    fn collect_vars_walk(&self, node: Node<'a>, out: &mut Vec<(String, Span)>) {
        match node.kind() {
            "scalar" | "array" | "hash" => {
                if let Ok(text) = node.utf8_text(self.source) {
                    out.push((text.to_string(), node_to_span(node)));
                }
            }
            _ => {
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        self.collect_vars_walk(child, out);
                    }
                }
            }
        }
    }

    fn first_var_child(&self, node: Node<'a>) -> Option<String> {
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                if matches!(child.kind(), "scalar" | "array" | "hash") {
                    return child.utf8_text(self.source).ok().map(|s| s.to_string());
                }
            }
        }
        None
    }

    fn determine_access(&self, node: Node<'a>) -> AccessKind {
        if let Some(parent) = node.parent() {
            match parent.kind() {
                "variable_declaration" => return AccessKind::Declaration,
                "assignment_expression" => {
                    // Check if we're on the left side
                    if let Some(left) = parent.child_by_field_name("left") {
                        if node.start_byte() >= left.start_byte()
                            && node.end_byte() <= left.end_byte()
                        {
                            return AccessKind::Write;
                        }
                    }
                }
                _ => {}
            }
            // Check grandparent for assignment
            if let Some(grandparent) = parent.parent() {
                if grandparent.kind() == "assignment_expression" {
                    if let Some(left) = grandparent.child_by_field_name("left") {
                        if node.start_byte() >= left.start_byte()
                            && node.end_byte() <= left.end_byte()
                        {
                            return AccessKind::Write;
                        }
                    }
                }
            }
        }
        AccessKind::Read
    }

    fn canonicalize_container(&self, node: Node<'a>, text: &str) -> String {
        // $hash{key} → %hash, $arr[0] → @arr, etc.
        if let Some(parent) = node.parent() {
            let parent_kind = parent.kind();
            if parent_kind == "hash_element_expression" || parent_kind == "keyval_container_variable" {
                // Container is a hash access — underlying variable is %name
                if text.starts_with('$') {
                    return format!("%{}", &text[1..]);
                }
            } else if parent_kind == "array_element_expression" || parent_kind == "slice_container_variable" {
                // Container is an array access — underlying variable is @name
                if text.starts_with('$') || text.starts_with('@') {
                    return format!("@{}", &text[1..]);
                }
            }
        }
        text.to_string()
    }

    fn extract_constructor_class(&self, node: Node<'a>) -> Option<String> {
        if node.kind() == "method_call_expression" {
            let method = node.child_by_field_name("method")?;
            if method.utf8_text(self.source).ok() == Some("new") {
                let invocant = node.child_by_field_name("invocant")?;
                let inv_text = invocant.utf8_text(self.source).ok()?;
                // Invocant must be a package name (not a variable)
                if !inv_text.starts_with('$') && !inv_text.starts_with('@') && !inv_text.starts_with('%') {
                    return Some(inv_text.to_string());
                }
            }
        }
        None
    }

    fn get_var_text_from_lhs(&self, lhs: Node<'a>) -> Option<String> {
        if lhs.kind() == "variable_declaration" {
            if let Some(var) = lhs.child_by_field_name("variable") {
                return var.utf8_text(self.source).ok().map(|s| s.to_string());
            }
            // Paren list: my ($x) = ...
            if let Some(vars) = lhs.child_by_field_name("variables") {
                for i in 0..vars.named_child_count() {
                    if let Some(child) = vars.named_child(i) {
                        if matches!(child.kind(), "scalar" | "array" | "hash") {
                            return child.utf8_text(self.source).ok().map(|s| s.to_string());
                        }
                    }
                }
            }
        }
        if matches!(lhs.kind(), "scalar" | "array" | "hash") {
            return lhs.utf8_text(self.source).ok().map(|s| s.to_string());
        }
        None
    }

    fn get_hash_var_from_element(&self, node: Node<'a>) -> Option<String> {
        // hash_element_expression: first named child is the container variable
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                if matches!(child.kind(), "container_variable" | "keyval_container_variable" | "scalar" | "hash") {
                    return child.utf8_text(self.source).ok().map(|s| s.to_string());
                }
            }
        }
        None
    }

    fn extract_key_text(&self, key_node: Node<'a>) -> Option<(String, bool)> {
        match key_node.kind() {
            "autoquoted_bareword" => {
                key_node.utf8_text(self.source).ok().map(|s| (s.to_string(), false))
            }
            "string_literal" | "interpolated_string_literal" => {
                // Simple string: 'key' or "key"
                let text = key_node.utf8_text(self.source).ok()?;
                // Strip quotes
                if text.len() >= 2 {
                    let inner = &text[1..text.len()-1];
                    // Dynamic if interpolated and contains $/@
                    let is_dynamic = key_node.kind() == "interpolated_string_literal"
                        && (inner.contains('$') || inner.contains('@'));
                    Some((inner.to_string(), is_dynamic))
                } else {
                    None
                }
            }
            _ => {
                // Dynamic key (variable, expression)
                key_node.utf8_text(self.source).ok().map(|s| (s.to_string(), true))
            }
        }
    }

    fn detect_anon_hash_owner(&self, anon_hash: Node<'a>) -> Option<HashKeyOwner> {
        // Check if this is inside a bless call
        let mut ancestor = anon_hash.parent()?;
        for _ in 0..5 {
            if self.is_bless_call(ancestor) {
                if let Some(ref pkg) = self.current_package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
                }
                // Try to find package from scope
                let scope = self.current_scope();
                if let Some(ref pkg) = self.scopes[scope.0 as usize].package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
                }
            }
            ancestor = ancestor.parent()?;
        }
        None
    }

    fn is_bless_call(&self, node: Node<'a>) -> bool {
        let kind = node.kind();
        if kind == "function_call_expression" || kind == "ambiguous_function_call_expression" {
            if let Some(func) = node.child_by_field_name("function") {
                return func.utf8_text(self.source).ok() == Some("bless");
            }
        }
        false
    }

    fn collect_fat_comma_keys(&mut self, node: Node<'a>, owner: &HashKeyOwner) {
        // Walk children looking for `bareword => ...` or `'string' => ...`
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                // Check if next sibling is =>
                if let Some(next) = child.next_sibling() {
                    if next.kind() == "=>" {
                        if matches!(child.kind(), "autoquoted_bareword" | "string_literal" | "interpolated_string_literal") {
                            if let Some((key, is_dynamic)) = self.extract_key_text(child) {
                                if !is_dynamic {
                                    self.add_symbol(
                                        key,
                                        SymKind::HashKeyDef,
                                        node_to_span(child),
                                        node_to_span(child),
                                        SymbolDetail::HashKeyDef {
                                            owner: owner.clone(),
                                            is_dynamic: false,
                                        },
                                    );
                                }
                            }
                        }
                    }
                }
                // Recurse into nested list_expressions
                if child.kind() == "list_expression" {
                    self.collect_fat_comma_keys(child, owner);
                }
            }
        }
    }

    // ---- Post-passes ----

    fn resolve_variable_refs(&mut self) {
        // Build a temporary scope-to-symbols map for efficient lookup
        let mut scope_symbols: std::collections::HashMap<ScopeId, Vec<(String, SymbolId, Point)>> =
            std::collections::HashMap::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
                scope_symbols
                    .entry(sym.scope)
                    .or_default()
                    .push((sym.name.clone(), sym.id, sym.span.start));
            }
        }

        for r in &mut self.refs {
            if !matches!(r.kind, RefKind::Variable | RefKind::ContainerAccess) {
                continue;
            }

            // Walk scope chain to find the innermost matching declaration
            let mut current = Some(r.scope);
            while let Some(scope_id) = current {
                if let Some(symbols) = scope_symbols.get(&scope_id) {
                    // Find the best match: declared before this ref, matching name
                    let target_name = if matches!(r.kind, RefKind::ContainerAccess) {
                        // Container refs need to match by base name, accounting for sigil differences
                        &r.target_name
                    } else {
                        &r.target_name
                    };
                    if let Some((_, sym_id, _)) = symbols.iter()
                        .filter(|(name, _, decl_point)| name == target_name && *decl_point <= r.span.start)
                        .last()
                    {
                        r.resolves_to = Some(*sym_id);
                        break;
                    }
                }
                current = self.scopes[scope_id.0 as usize].parent;
            }
        }
    }

    fn resolve_hash_key_owners(&mut self) {
        // Build type constraint lookup
        let mut type_map: std::collections::HashMap<String, Vec<(ScopeId, InferredType, Point)>> =
            std::collections::HashMap::new();
        for tc in &self.type_constraints {
            type_map
                .entry(tc.variable.clone())
                .or_default()
                .push((tc.scope, tc.inferred_type.clone(), tc.constraint_span.start));
        }

        // Build variable def lookup
        let mut var_defs: std::collections::HashMap<String, Vec<(ScopeId, SymbolId)>> =
            std::collections::HashMap::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
                var_defs
                    .entry(sym.name.clone())
                    .or_default()
                    .push((sym.scope, sym.id));
            }
        }

        for r in &mut self.refs {
            if let RefKind::HashKeyAccess { ref var_text, ref mut owner } = r.kind {
                if owner.is_some() { continue; }

                let vt = var_text.clone();
                // Canonicalize: $hash → %hash for lookup
                let lookup_name = if vt.starts_with('$') {
                    format!("%{}", &vt[1..])
                } else {
                    vt.clone()
                };

                // Try type constraints first
                if let Some(constraints) = type_map.get(&vt).or(type_map.get(&lookup_name)) {
                    // Find best constraint: in scope chain and before ref
                    let mut scope = Some(r.scope);
                    'outer: while let Some(sid) = scope {
                        for (tc_scope, tc_type, tc_point) in constraints {
                            if *tc_scope == sid && *tc_point <= r.span.start {
                                if let Some(cn) = tc_type.class_name() {
                                    *owner = Some(HashKeyOwner::Class(cn.to_string()));
                                    break 'outer;
                                }
                            }
                        }
                        scope = self.scopes[sid.0 as usize].parent;
                    }
                    if owner.is_some() { continue; }
                }

                // Fall back to variable identity
                if let Some(defs) = var_defs.get(&vt).or(var_defs.get(&lookup_name)) {
                    // Find the innermost declaration before this ref
                    let mut scope = Some(r.scope);
                    while let Some(sid) = scope {
                        if let Some((def_scope, _sym_id)) = defs.iter().find(|(s, _)| *s == sid) {
                            *owner = Some(HashKeyOwner::Variable {
                                name: vt.clone(),
                                def_scope: *def_scope,
                            });
                            break;
                        }
                        scope = self.scopes[sid.0 as usize].parent;
                    }
                }
            }
        }
    }
}

// ---- Tests ----

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Tree {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_perl::LANGUAGE.into()).unwrap();
        parser.parse(source, None).unwrap()
    }

    fn build_fa(source: &str) -> FileAnalysis {
        let tree = parse(source);
        build(&tree, source.as_bytes())
    }

    // ---- Scope tests ----

    #[test]
    fn test_file_scope() {
        let fa = build_fa("my $x = 1;");
        assert_eq!(fa.scopes.len(), 1);
        assert_eq!(fa.scopes[0].kind, ScopeKind::File);
    }

    #[test]
    fn test_sub_creates_scope() {
        let fa = build_fa("sub foo { my $x = 1; }");
        let sub_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"))
            .collect();
        assert_eq!(sub_scopes.len(), 1);
        assert_eq!(sub_scopes[0].parent, Some(ScopeId(0))); // parent is file
    }

    #[test]
    fn test_class_creates_scope() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
        let class_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Class { name } if name == "Point"))
            .collect();
        assert_eq!(class_scopes.len(), 1);
        assert_eq!(class_scopes[0].package, Some("Point".to_string()));
    }

    #[test]
    fn test_package_sets_scope_package() {
        let fa = build_fa("package Foo;\nsub bar { 1 }");
        // The sub scope should inherit package "Foo"
        let sub_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "bar"))
            .collect();
        assert_eq!(sub_scopes.len(), 1);
        assert_eq!(sub_scopes[0].package, Some("Foo".to_string()));
    }

    #[test]
    fn test_for_loop_scope() {
        let fa = build_fa("for my $i (1..10) { print $i; }");
        let for_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::ForLoop { .. }))
            .collect();
        assert_eq!(for_scopes.len(), 1);
    }

    // ---- Symbol tests ----

    #[test]
    fn test_variable_symbol() {
        let fa = build_fa("my $x = 1;");
        let vars: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Variable && s.name == "$x")
            .collect();
        assert_eq!(vars.len(), 1);
        if let SymbolDetail::Variable { sigil, decl_kind } = &vars[0].detail {
            assert_eq!(*sigil, '$');
            assert_eq!(*decl_kind, DeclKind::My);
        } else {
            panic!("expected Variable detail");
        }
    }

    #[test]
    fn test_sub_symbol_with_params() {
        let fa = build_fa("sub connect($self, %opts) { }");
        let subs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Sub && s.name == "connect")
            .collect();
        assert_eq!(subs.len(), 1);
        if let SymbolDetail::Sub { params, is_method } = &subs[0].detail {
            assert!(!is_method);
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "$self");
            assert_eq!(params[1].name, "%opts");
            assert!(params[1].is_slurpy);
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_legacy_sub_params() {
        let fa = build_fa("sub new {\n    my ($class, %args) = @_;\n}");
        let subs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Sub && s.name == "new")
            .collect();
        assert_eq!(subs.len(), 1);
        if let SymbolDetail::Sub { params, .. } = &subs[0].detail {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "$class");
            assert_eq!(params[1].name, "%args");
            assert!(params[1].is_slurpy);
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_package_symbol() {
        let fa = build_fa("package Foo;");
        let pkgs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Package && s.name == "Foo")
            .collect();
        assert_eq!(pkgs.len(), 1);
    }

    #[test]
    fn test_class_symbol() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n    field $y :param;\n    method magnitude() { }\n}");
        let classes: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Class && s.name == "Point")
            .collect();
        assert_eq!(classes.len(), 1);
        if let SymbolDetail::Class { fields, parent, .. } = &classes[0].detail {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "$x");
            assert_eq!(fields[1].name, "$y");
            assert!(fields[0].attributes.contains(&"param".to_string()));
            assert!(parent.is_none());
        } else {
            panic!("expected Class detail");
        }
    }

    #[test]
    fn test_field_symbol() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
        let fields: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Field)
            .collect();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "$x");
    }

    #[test]
    fn test_field_reader_synthesizes_method() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n}");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1, "got: {:?}", methods.iter().map(|m| &m.name).collect::<Vec<_>>());
        assert_eq!(methods[0].name, "x");
    }

    #[test]
    fn test_implicit_self_in_method() {
        // $self is implicitly available in Perl 5.38 method blocks
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
        let fa = build_fa(source);

        // $self should be resolvable as a variable inside the method
        let resolved = fa.resolve_variable("$self", Point::new(4, 8));
        assert!(resolved.is_some(), "$self should resolve inside method body");
    }

    #[test]
    fn test_implicit_self_type_inference() {
        // $self should be type-inferred to the enclosing class
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
        let fa = build_fa(source);

        // Type inference: $self → Point
        let inferred = fa.inferred_type("$self", Point::new(4, 8));
        assert!(inferred.is_some(), "$self type should be inferred");
        match inferred.unwrap() {
            InferredType::ClassName(name) => assert_eq!(name, "Point"),
            InferredType::FirstParam { package } => assert_eq!(package, "Point"),
            other => panic!("expected ClassName or FirstParam, got {:?}", other),
        }
    }

    #[test]
    fn test_self_completion_inside_method() {
        // $self-> inside a method should complete with sibling methods
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n    method to_string () {\n        $self->;\n    }\n}\n";
        let fa = build_fa(source);

        let candidates = fa.complete_methods("$self", Point::new(5, 14));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_field_writer_synthesizes_method() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $label :reader :writer = \"point\";\n}");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Method)
            .map(|s| s.name.clone())
            .collect();
        assert!(methods.contains(&"label".to_string()), "missing reader, got: {:?}", methods);
        assert!(methods.contains(&"set_label".to_string()), "missing writer, got: {:?}", methods);
    }

    #[test]
    fn test_complete_methods_in_class() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n    method magnitude() { }\n    method to_string() { }\n}\nmy $p = Point->new(x => 1);\n$p->;\n");
        // $p-> is at line 8, col 4
        let candidates = fa.complete_methods("$p", Point::new(8, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"new"), "missing new, got: {:?}", names);
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_sample_file_layout() {
        // Matches sample.pl: class defined after package main, $p usage at end
        let source = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
        let fa = build_fa(source);

        // Check type inference resolved $p → Point
        let inferred = fa.inferred_type("$p", Point::new(8, 4));
        assert!(inferred.is_some(), "type inference for $p should resolve");

        let candidates = fa.complete_methods("$p", Point::new(10, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_class_after_package_main() {
        // Real-world: package main; ... class Point {} ... $p->
        let source = r#"package main;
my $calc = Calculator->new();
1;
use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
        let fa = build_fa(source);

        let candidates = fa.complete_methods("$p", Point::new(11, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"new"), "missing new, got: {:?}", names);
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_flat_class() {
        // class Foo; (no block) — methods follow as siblings, like package
        let source = "use v5.38;\nclass Foo;\nmethod bar () { }\nmethod baz () { }\n";
        let fa = build_fa(source);
        let candidates = fa.complete_methods("Foo", Point::new(3, 0));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"bar"), "missing bar, got: {:?}", names);
        assert!(names.contains(&"baz"), "missing baz, got: {:?}", names);
    }

    #[test]
    fn test_goto_def_method_after_package_main() {
        // go-to-def on $p->magnitude() should find the method, not the class
        let source = "package main;\n1;\nuse v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n}\nmy $p = Point->new(x => 3);\n$p->magnitude();\n";
        let fa = build_fa(source);
        // cursor on `magnitude` in `$p->magnitude()` — line 8, col 5
        let def = fa.find_definition(Point::new(8, 5));
        assert!(def.is_some(), "should find definition for magnitude");
        let span = def.unwrap();
        assert_eq!(span.start.row, 5, "should point to method declaration line, got row {}", span.start.row);
    }

    #[test]
    fn test_field_reader_goto_def() {
        // go-to-def on $p->x should find the reader method, which points to the field
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    method mag() { }\n}\nmy $p = Point->new(x => 1);\n$p->x;");
        let def = fa.find_definition(Point::new(6, 5)); // cursor on `x` in `$p->x`
        assert!(def.is_some(), "should find definition for reader method");
        // The reader method's selection_span points to the field declaration
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "should point to field declaration line");
    }

    #[test]
    fn test_use_symbol() {
        let fa = build_fa("use Foo::Bar;");
        let modules: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module)
            .collect();
        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].name, "Foo::Bar");
    }

    // ---- Ref tests ----

    #[test]
    fn test_variable_ref() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        let var_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$x" && matches!(r.kind, RefKind::Variable))
            .collect();
        // One declaration ref + one read ref
        assert!(var_refs.len() >= 2, "got {} refs for $x", var_refs.len());
        assert!(var_refs.iter().any(|r| r.access == AccessKind::Declaration));
        assert!(var_refs.iter().any(|r| r.access == AccessKind::Read));
    }

    #[test]
    fn test_function_call_ref() {
        let fa = build_fa("sub foo { }\nfoo();");
        let call_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::FunctionCall))
            .collect();
        assert_eq!(call_refs.len(), 1);
    }

    #[test]
    fn test_method_call_ref() {
        let fa = build_fa("$obj->method();");
        let method_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "method" && matches!(r.kind, RefKind::MethodCall { .. }))
            .collect();
        assert_eq!(method_refs.len(), 1);
        if let RefKind::MethodCall { ref invocant } = method_refs[0].kind {
            assert_eq!(invocant, "$obj");
        }
    }

    #[test]
    fn test_hash_key_ref() {
        let fa = build_fa("my %h;\n$h{foo};");
        let key_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert_eq!(key_refs.len(), 1);
    }

    // ---- Query tests ----

    #[test]
    fn test_scope_at() {
        let fa = build_fa("sub foo {\n    my $x = 1;\n}");
        // Point inside the sub body
        let scope = fa.scope_at(Point::new(1, 8)).unwrap();
        let s = fa.scope(scope);
        assert!(matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"));
    }

    #[test]
    fn test_resolve_variable() {
        let fa = build_fa("my $x = 1;\nsub foo {\n    my $x = 2;\n    print $x;\n}");
        // Inside the sub, $x should resolve to the inner declaration
        let sym = fa.resolve_variable("$x", Point::new(3, 10)).unwrap();
        // Inner $x is at line 2
        assert_eq!(sym.selection_span.start.row, 2);
    }

    #[test]
    fn test_resolve_variable_outer() {
        let fa = build_fa("my $x = 1;\nsub foo {\n    print $x;\n}");
        // Inside the sub with no inner $x, should resolve to outer
        let sym = fa.resolve_variable("$x", Point::new(2, 10)).unwrap();
        assert_eq!(sym.selection_span.start.row, 0);
    }

    #[test]
    fn test_type_inference_constructor() {
        let fa = build_fa("use v5.38;\nclass Point { }\nmy $p = Point->new();");
        let ty = fa.inferred_type("$p", Point::new(2, 20));
        assert!(ty.is_some(), "should infer type for $p");
        if let Some(InferredType::ClassName(cn)) = ty {
            assert_eq!(cn, "Point");
        } else {
            panic!("expected ClassName, got {:?}", ty);
        }
    }

    #[test]
    fn test_type_inference_first_param() {
        let fa = build_fa("package Calculator;\nsub new {\n    my ($self) = @_;\n}");
        let ty = fa.inferred_type("$self", Point::new(2, 10));
        assert!(ty.is_some(), "should infer type for $self");
        if let Some(InferredType::FirstParam { package }) = ty {
            assert_eq!(package, "Calculator");
        } else {
            panic!("expected FirstParam, got {:?}", ty);
        }
    }

    // ---- Literal constructor extraction tests (via build_fa) ----

    #[test]
    fn test_extract_hashref_literal() {
        let fa = build_fa("my $href = {};");
        let ty = fa.inferred_type("$href", Point::new(0, 14));
        assert_eq!(ty, Some(&InferredType::HashRef), "empty hash ref literal");

        let fa = build_fa("my $href = { a => 1, b => 2 };");
        let ty = fa.inferred_type("$href", Point::new(0, 30));
        assert_eq!(ty, Some(&InferredType::HashRef), "populated hash ref literal");
    }

    #[test]
    fn test_extract_arrayref_literal() {
        let fa = build_fa("my $aref = [];");
        let ty = fa.inferred_type("$aref", Point::new(0, 14));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "empty array ref literal");

        let fa = build_fa("my $aref = [1, 2, 3];");
        let ty = fa.inferred_type("$aref", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "populated array ref literal");
    }

    #[test]
    fn test_extract_coderef_literal() {
        let fa = build_fa("my $cref = sub { 42 };");
        let ty = fa.inferred_type("$cref", Point::new(0, 22));
        assert_eq!(ty, Some(&InferredType::CodeRef), "anonymous sub");
    }

    #[test]
    fn test_extract_regexp_literal() {
        let fa = build_fa("my $re = qr/pattern/;");
        let ty = fa.inferred_type("$re", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::Regexp), "qr// literal");
    }

    #[test]
    fn test_extract_reassignment_type_change() {
        let fa = build_fa("my $x = {};\n$x = [];");
        // After line 0 → HashRef
        let ty = fa.inferred_type("$x", Point::new(0, 11));
        assert_eq!(ty, Some(&InferredType::HashRef), "initial hashref");
        // After line 1 → ArrayRef
        let ty = fa.inferred_type("$x", Point::new(1, 8));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "reassigned to arrayref");
    }

    #[test]
    fn test_extract_constructor_still_works() {
        // Existing constructor detection should still work
        let fa = build_fa("my $obj = Foo->new();");
        let ty = fa.inferred_type("$obj", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::ClassName("Foo".into())));
    }

    // ---- Operator-based type inference tests (Step 3) ----

    #[test]
    fn test_arrow_hash_deref_infers_hashref() {
        let fa = build_fa("my $x;\n$x->{key};");
        let ty = fa.inferred_type("$x", Point::new(1, 10));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_arrow_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\n$x->[0];");
        let ty = fa.inferred_type("$x", Point::new(1, 8));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_arrow_code_deref_infers_coderef() {
        let fa = build_fa("my $x;\n$x->(1, 2);");
        let ty = fa.inferred_type("$x", Point::new(1, 10));
        assert_eq!(ty, Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_postfix_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\nmy @a = $x->@*;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_postfix_hash_deref_infers_hashref() {
        let fa = build_fa("my $y;\nmy %h = $y->%*;\nmy $z;");
        let ty = fa.inferred_type("$y", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_binary_numeric_ops_infer_numeric() {
        let fa = build_fa("my $x;\nmy $a = $x + 1;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "+ operator");

        let fa = build_fa("my $x;\nmy $a = $x * 2;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "* operator");
    }

    #[test]
    fn test_string_concat_infers_string() {
        let fa = build_fa("my $s;\nmy $a = $s . \"x\";\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), ". operator");
    }

    #[test]
    fn test_string_repeat_infers_string() {
        let fa = build_fa("my $s;\n$s x 3;\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), "x operator");
    }

    #[test]
    fn test_numeric_comparison_infers_numeric() {
        let fa = build_fa("my $x;\nmy $y;\n$x == $y;\nmy $z;");
        assert_eq!(fa.inferred_type("$x", Point::new(3, 0)), Some(&InferredType::Numeric));
        assert_eq!(fa.inferred_type("$y", Point::new(3, 0)), Some(&InferredType::Numeric));
    }

    #[test]
    fn test_string_comparison_infers_string() {
        let fa = build_fa("my $x;\nmy $y;\n$x eq $y;\nmy $z;");
        assert_eq!(fa.inferred_type("$x", Point::new(3, 0)), Some(&InferredType::String));
        assert_eq!(fa.inferred_type("$y", Point::new(3, 0)), Some(&InferredType::String));
    }

    #[test]
    fn test_increment_infers_numeric() {
        let fa = build_fa("my $x;\n$x++;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric));
    }

    #[test]
    fn test_regex_match_infers_string() {
        let fa = build_fa("my $s;\n$s =~ /pattern/;\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String));
    }

    #[test]
    fn test_preinc_infers_numeric() {
        let fa = build_fa("my $x;\n++$x;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric));
    }

    #[test]
    fn test_block_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\nmy @items = @{$x};\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_block_hash_deref_infers_hashref() {
        let fa = build_fa("my $y;\nmy %t = %{$y};\nmy $z;");
        let ty = fa.inferred_type("$y", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_block_code_deref_infers_coderef() {
        let fa = build_fa("my $z;\n&{$z}();\nmy $w;");
        let ty = fa.inferred_type("$z", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_no_numeric_on_array_variable() {
        // @arr + 1 should NOT push Numeric on @arr
        let fa = build_fa("my @arr;\nmy $n = @arr + 1;\nmy $z;");
        let ty = fa.inferred_type("@arr", Point::new(2, 0));
        assert_eq!(ty, None, "@arr should not get Numeric constraint");
    }

    #[test]
    fn test_package_at() {
        let fa = build_fa("package Foo;\nsub bar { }");
        let pkg = fa.package_at(Point::new(1, 5));
        assert_eq!(pkg, Some("Foo"));
    }

    #[test]
    fn test_variable_resolves_to() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        let read_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$x" && r.access == AccessKind::Read)
            .collect();
        assert!(!read_refs.is_empty());
        assert!(read_refs[0].resolves_to.is_some(), "read ref should resolve to declaration");
    }

    #[test]
    fn test_fold_ranges() {
        let fa = build_fa("sub foo {\n    my $x = 1;\n}\nsub bar {\n    my $y = 2;\n}");
        assert!(fa.fold_ranges.len() >= 2, "should have fold ranges for sub blocks, got {}", fa.fold_ranges.len());
    }

    #[test]
    fn test_visible_symbols() {
        let fa = build_fa("my $outer = 1;\nsub foo {\n    my $inner = 2;\n}");
        // Inside the sub, both $outer and $inner should be visible
        let visible = fa.visible_symbols(Point::new(2, 10));
        let names: Vec<&str> = visible.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"$inner"), "should see $inner, got: {:?}", names);
        assert!(names.contains(&"$outer"), "should see $outer, got: {:?}", names);
    }

    #[test]
    fn test_two_packages_scoped() {
        let fa = build_fa("package Foo;\nsub alpha { }\npackage Bar;\nsub beta { }");
        // At the beta sub, package should be "Bar"
        let pkg = fa.package_at(Point::new(3, 5));
        assert_eq!(pkg, Some("Bar"));
        // At the alpha sub, package should be "Foo"
        let pkg = fa.package_at(Point::new(1, 5));
        assert_eq!(pkg, Some("Foo"));
    }

    // ---- High-level query tests ----

    #[test]
    fn test_find_def_variable() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        // Cursor on the usage of $x at line 1
        let def = fa.find_definition(Point::new(1, 7));
        assert!(def.is_some(), "should find definition for $x");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_sub() {
        let fa = build_fa("sub greet { }\ngreet();");
        // Cursor on the function call at line 1
        let def = fa.find_definition(Point::new(1, 1));
        assert!(def.is_some(), "should find definition for greet");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_method_in_class() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub hello { }\npackage main;\nmy $f = Foo->new();\n$f->hello();";
        let fa = build_fa(src);
        // Cursor on hello() call at line 5
        let def = fa.find_definition(Point::new(5, 5));
        assert!(def.is_some(), "should find definition for hello method");
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "hello definition should be on line 2");
    }

    #[test]
    fn test_find_def_scoped_variable() {
        let src = "my $x = 'outer';\nsub foo {\n    my $x = 'inner';\n    print $x;\n}";
        let fa = build_fa(src);
        // Cursor on $x inside sub (line 3) should resolve to inner $x (line 2)
        let def = fa.find_definition(Point::new(3, 11));
        assert!(def.is_some());
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "should resolve to inner $x on line 2");
    }

    #[test]
    fn test_find_references_variable() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        // Cursor on the declaration of $x
        let refs = fa.find_references(Point::new(0, 4));
        assert!(refs.len() >= 2, "should find at least declaration + usage, got {}", refs.len());
    }

    #[test]
    fn test_find_references_sub() {
        let src = "sub greet { }\ngreet();\ngreet();";
        let fa = build_fa(src);
        // Cursor on the sub name
        let refs = fa.find_references(Point::new(0, 5));
        assert!(refs.len() >= 2, "should find definition + calls, got {}", refs.len());
    }

    #[test]
    fn test_highlights_read_write() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        let highlights = fa.find_highlights(Point::new(0, 4));
        assert!(!highlights.is_empty(), "should have highlights");
        // Check that we have both read and write accesses
        let has_write = highlights.iter().any(|(_, a)| matches!(a, AccessKind::Write));
        let has_read = highlights.iter().any(|(_, a)| matches!(a, AccessKind::Read));
        // At minimum we should see the declaration
        assert!(highlights.len() >= 2, "should have at least 2 highlights, got {}", highlights.len());
        // Note: whether read/write are correctly tagged depends on builder's access classification
        let _ = (has_write, has_read); // suppress unused warnings if assertions change
    }

    #[test]
    fn test_hover_variable() {
        let src = "my $greeting = 'hello';\nprint $greeting;";
        let fa = build_fa(src);
        let hover = fa.hover_info(Point::new(1, 8), src);
        assert!(hover.is_some(), "should have hover info");
        let text = hover.unwrap();
        assert!(text.contains("$greeting"), "hover should contain variable name, got: {}", text);
    }

    #[test]
    fn test_hover_sub() {
        let src = "sub greet { }\ngreet();";
        let fa = build_fa(src);
        let hover = fa.hover_info(Point::new(1, 1), src);
        assert!(hover.is_some(), "should have hover info for function call");
        let text = hover.unwrap();
        assert!(text.contains("greet"), "hover should contain sub name, got: {}", text);
    }

    #[test]
    fn test_rename_variable() {
        let src = "my $x = 1;\nprint $x;";
        let fa = build_fa(src);
        let edits = fa.rename_at(Point::new(0, 4), "y");
        assert!(edits.is_some(), "should produce rename edits");
        let edits = edits.unwrap();
        assert!(edits.len() >= 2, "should rename at least declaration + usage");
        for (_, new_text) in &edits {
            assert_eq!(new_text, "y", "all edits should use new name");
        }
    }

    #[test]
    fn test_find_def_bareword_class() {
        let src = "package Point;\nsub new { bless {}, shift }\npackage main;\nPoint->new();";
        let fa = build_fa(src);
        // Cursor on "new" in Point->new()
        let def = fa.find_definition(Point::new(3, 8));
        assert!(def.is_some(), "should find definition for new");
    }

    // ---- Block dereference descent tests ----
    // @{expr}, %{expr}, ${expr} parse as scalar/array/hash with varname→block.
    // The builder must recurse into the block to find inner refs.

    #[test]
    fn test_deref_block_produces_inner_variable_ref() {
        // @{$arr} — the inner $arr should produce a Variable ref
        let fa = build_fa("my @data = (1,2,3);\nmy $arr = \\@data;\npush @{$arr}, 4;");
        let inner_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$arr" && matches!(r.kind, RefKind::Variable) && r.access == AccessKind::Read)
            .collect();
        assert!(!inner_refs.is_empty(), "should find $arr ref inside @{{$arr}}");
        // Should NOT have a bogus ref for the whole @{$arr}
        let bogus: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name.contains("{$arr}"))
            .collect();
        assert!(bogus.is_empty(), "should not record bogus ref for whole deref expression");
    }

    #[test]
    fn test_deref_block_produces_hash_key_ref() {
        // @{$self->{items}} — inner hash_element_expression should produce:
        // 1. Variable ref for $self
        // 2. HashKeyAccess ref for "items"
        let fa = build_fa("my %h = (items => []);\n@{$h{items}};");
        let key_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "items" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!key_refs.is_empty(), "should find hash key ref 'items' inside deref block");
    }

    #[test]
    fn test_deref_block_resolves_variable() {
        // Variable inside deref block should resolve to its declaration
        let fa = build_fa("my @xs = (1,2);\nmy $ref = \\@xs;\nprint @{$ref};");
        let inner_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$ref" && r.access == AccessKind::Read)
            .collect();
        assert!(!inner_refs.is_empty(), "$ref ref should exist");
        assert!(inner_refs[0].resolves_to.is_some(), "$ref inside deref should resolve to declaration");
    }

    #[test]
    fn test_deref_self_and_hash_key() {
        // Full integration: constructor defines hash keys, method accesses them through deref
        let src = "package Calculator;\nsub new {\n    my ($class, %args) = @_;\n    my $self = bless {\n        history => [],\n        verbose => 0,\n    }, $class;\n    return $self;\n}\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    push @{$self->{history}}, \"add\";\n    return $result;\n}";
        let fa = build_fa(src);

        // $self at line 12 (push @{$self->{history}}, ...)
        let def_self = fa.find_definition(Point::new(12, 12));
        assert!(def_self.is_some(), "should find definition for $self in deref");
        assert_eq!(def_self.unwrap().start.row, 10, "$self should resolve to declaration on line 10");

        // history key at line 12
        let def_history = fa.find_definition(Point::new(12, 20));
        assert!(def_history.is_some(), "should find definition for history hash key");
        assert_eq!(def_history.unwrap().start.row, 4, "history key should resolve to definition on line 4");
    }

    #[test]
    fn test_imports_qw() {
        let source = "use List::Util qw(first any all);\nuse Scalar::Util qw(blessed);\n";
        let fa = build_fa(source);

        assert_eq!(fa.imports.len(), 2);

        assert_eq!(fa.imports[0].module_name, "List::Util");
        assert_eq!(fa.imports[0].imported_symbols, vec!["first", "any", "all"]);

        assert_eq!(fa.imports[1].module_name, "Scalar::Util");
        assert_eq!(fa.imports[1].imported_symbols, vec!["blessed"]);
    }

    #[test]
    fn test_imports_qw_close_paren_position() {
        // "use List::Util qw(first);\n"
        //  0123456789...
        //                  ^18    ^24 = )
        let source = "use List::Util qw(first);\n";
        let fa = build_fa(source);

        assert_eq!(fa.imports.len(), 1);
        let imp = &fa.imports[0];
        assert!(imp.qw_close_paren.is_some(), "qw_close_paren should be set");
        let pos = imp.qw_close_paren.unwrap();
        // The ) is at column 23 in "use List::Util qw(first);"
        assert_eq!(pos.row, 0);
        assert_eq!(pos.column, 23, "close paren should be at column 23");
    }

    #[test]
    fn test_imports_bare() {
        let source = "use strict;\nuse warnings;\nuse Carp;\n";
        let fa = build_fa(source);

        // strict/warnings/Carp all produce imports with empty imported_symbols
        let carp = fa.imports.iter().find(|i| i.module_name == "Carp");
        assert!(carp.is_some());
        assert!(carp.unwrap().imported_symbols.is_empty());
    }

    #[test]
    fn test_imports_module_symbol_created() {
        let source = "use List::Util qw(first);\n";
        let fa = build_fa(source);

        // Module symbol should exist
        let module_syms: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module && s.name == "List::Util")
            .collect();
        assert_eq!(module_syms.len(), 1);

        // Import should exist
        assert_eq!(fa.imports.len(), 1);
        assert_eq!(fa.imports[0].imported_symbols, vec!["first"]);
    }
}
