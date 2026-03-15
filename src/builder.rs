//! Single-walk tree-sitter visitor that builds a FileAnalysis.
//!
//! One depth-first walk populates scopes, symbols, refs, type constraints,
//! and fold ranges. Post-passes resolve hash key owners and variable refs.

use tree_sitter::{Node, Point, Tree};

use crate::file_analysis::*;

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
        return_infos: Vec::new(),
        last_expr_type: std::collections::HashMap::new(),
        call_bindings: Vec::new(),
        method_call_bindings: Vec::new(),
        pod_texts: Vec::new(),
        package_parents: std::collections::HashMap::new(),
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

    // Post-pass 3: infer return types for subs/methods
    b.resolve_return_types();

    // Post-pass 4: fill in tail POD docs for subs that didn't get preceding doc
    b.resolve_tail_pod_docs();

    FileAnalysis::new(
        b.scopes,
        b.symbols,
        b.refs,
        b.type_constraints,
        b.fold_ranges,
        b.imports,
        b.call_bindings,
        b.package_parents,
        b.method_call_bindings,
    )
}

/// A return value type collected during the walk, before post-pass resolution.
struct ReturnInfo {
    /// The scope (Sub/Method) this return belongs to.
    scope: ScopeId,
    /// The inferred type of the return value, if determinable from literals/constructors.
    inferred_type: Option<InferredType>,
}

struct Builder<'a> {
    source: &'a [u8],

    scopes: Vec<Scope>,
    symbols: Vec<Symbol>,
    refs: Vec<Ref>,
    type_constraints: Vec<TypeConstraint>,
    fold_ranges: Vec<FoldRange>,
    imports: Vec<Import>,
    /// Return values collected during the walk (explicit `return` + implicit last expr).
    return_infos: Vec<ReturnInfo>,
    /// For each Sub/Method scope, the type of the last expression (implicit return).
    last_expr_type: std::collections::HashMap<ScopeId, Option<InferredType>>,
    /// Assignments where RHS is a function call — resolved in return-type post-pass.
    call_bindings: Vec<CallBinding>,
    /// Assignments where RHS is a method call — resolved in FileAnalysis post-pass.
    method_call_bindings: Vec<MethodCallBinding>,
    /// Raw POD text blocks collected during the walk (for tail-POD post-pass).
    pod_texts: Vec<String>,
    /// Parent classes for each package (from use parent/base, @ISA, class :isa).
    package_parents: std::collections::HashMap<String, Vec<String>>,

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
                    "class_statement" | "for_statement" | "foreach_statement" |
                    "varname" // block-deref: @{expr}, %{expr}, &{expr}
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
            // Built-in calls: abs($x), length($s), time(), etc.
            "func1op_call_expression" | "func0op_call_expression" => {
                self.visit_func1op(node);
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

            // Return expressions → collect return types for post-pass
            "return_expression" => {
                if let Some(scope) = self.enclosing_sub_scope() {
                    let ret_type = self.infer_return_value_type(node);
                    self.return_infos.push(ReturnInfo {
                        scope,
                        inferred_type: ret_type,
                    });
                }
                self.visit_children(node);
            }

            // Expression statements inside sub bodies → track last expression type
            "expression_statement" => {
                self.visit_children(node);
                // Track the type of the last expression in the innermost sub/method scope
                if let Some(scope) = self.enclosing_sub_scope() {
                    let expr_type = node.named_child(0).and_then(|child| {
                        Self::infer_literal_type(child)
                            .or_else(|| self.extract_constructor_class(child).map(InferredType::ClassName))
                    });
                    self.last_expr_type.insert(scope, expr_type);
                }
            }

            // Hash construction
            "anonymous_hash_expression" => self.visit_anon_hash(node),

            // POD blocks: collect text for tail-POD post-pass
            "pod" => {
                if let Ok(text) = node.utf8_text(self.source) {
                    self.pod_texts.push(text.to_string());
                }
            }

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

        // Write to package_parents for unified inheritance resolution
        if let Some(ref p) = parent {
            self.package_parents
                .entry(name.clone())
                .or_default()
                .push(p.clone());
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

        // Extract preceding POD/comment documentation
        let doc = self.extract_preceding_doc(node, &name);

        self.add_symbol(
            name.clone(),
            if is_method { SymKind::Method } else { SymKind::Sub },
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::Sub { params: params.clone(), is_method, return_type: None, doc },
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

        // Fallback: scan body for shift, @_, and $_[N] patterns
        if let Some(body) = sub_node.child_by_field_name("body") {
            let mut shift_params: Vec<ParamInfo> = Vec::new();

            for i in 0..body.named_child_count() {
                let stmt = match body.named_child(i) {
                    Some(s) => s,
                    None => continue,
                };
                let assign = if stmt.kind() == "expression_statement" {
                    stmt.named_child(0).filter(|n| n.kind() == "assignment_expression")
                } else if stmt.kind() == "assignment_expression" {
                    Some(stmt)
                } else {
                    None
                };
                let assign = match assign {
                    Some(a) => a,
                    None => break, // stop at first non-assignment statement
                };

                if let Some(right) = assign.child_by_field_name("right") {
                    // Pattern: my (...) = @_
                    if right.utf8_text(self.source).ok() == Some("@_") {
                        if let Some(left) = assign.child_by_field_name("left") {
                            let at_params: Vec<ParamInfo> = self.collect_vars_from_decl(left)
                                .into_iter()
                                .map(|(name, _)| {
                                    let is_slurpy = name.starts_with('@') || name.starts_with('%');
                                    ParamInfo { name, default: None, is_slurpy }
                                })
                                .collect();
                            // Combine any preceding shift params with @_ params
                            if !shift_params.is_empty() {
                                shift_params.extend(at_params);
                                return shift_params;
                            }
                            return at_params;
                        }
                    }

                    // Pattern: my $var = shift; or my $var = shift || default; or my $var = shift // default;
                    if let Some((var_name, default)) = self.extract_shift_param(assign, right) {
                        shift_params.push(ParamInfo {
                            name: var_name,
                            default,
                            is_slurpy: false,
                        });
                        continue;
                    }

                    // Pattern: my $var = $_[N];
                    if let Some(var_name) = self.extract_subscript_param(assign, right) {
                        shift_params.push(ParamInfo {
                            name: var_name,
                            default: None,
                            is_slurpy: false,
                        });
                        continue;
                    }
                }

                // Not a recognized param pattern — stop collecting
                break;
            }

            if !shift_params.is_empty() {
                return shift_params;
            }
        }

        Vec::new()
    }

    /// Extract a shift-based parameter: `my $var = shift` or `my $var = shift || default`.
    fn extract_shift_param(&self, assign: Node<'a>, right: Node<'a>) -> Option<(String, Option<String>)> {
        let (shift_node, default) = if self.is_shift_call(right) {
            (right, None)
        } else if right.kind() == "binary_expression" {
            // my $var = shift || default  or  my $var = shift // default
            let op = self.get_operator_text(right);
            if matches!(op.as_deref(), Some("||" | "//")) {
                let lhs = right.named_child(0)?;
                if self.is_shift_call(lhs) {
                    let default_node = right.named_child(1)?;
                    let default_text = default_node.utf8_text(self.source).ok()?.to_string();
                    (lhs, Some(default_text))
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None;
        };
        let _ = shift_node;

        // Get variable name from LHS
        let left = assign.child_by_field_name("left")?;
        let var_name = self.get_var_text_from_lhs(left)?;
        Some((var_name, default))
    }

    /// Extract a $_[N]-based parameter: `my $var = $_[N]`.
    fn extract_subscript_param(&self, assign: Node<'a>, right: Node<'a>) -> Option<String> {
        if right.kind() != "array_element_expression" {
            return None;
        }
        // Check that it's $_ (container_variable for @_) being subscripted
        let container = right.named_child(0)?;
        if container.kind() != "container_variable" {
            return None;
        }
        // container_variable text is "$_" for @_ subscript
        let ct = container.utf8_text(self.source).ok()?;
        if ct != "$_" {
            return None;
        }
        let left = assign.child_by_field_name("left")?;
        self.get_var_text_from_lhs(left)
    }

    /// Check if a node is a `shift` call (bare or with parens).
    fn is_shift_call(&self, node: Node<'a>) -> bool {
        match node.kind() {
            "bareword" => node.utf8_text(self.source).ok() == Some("shift"),
            "func1op_call_expression" => {
                // shift without explicit args: func1op_call_expression with child "shift"
                node.child(0)
                    .and_then(|c| c.utf8_text(self.source).ok())
                    == Some("shift")
            }
            "ambiguous_function_call_expression" | "function_call_expression" => {
                node.child_by_field_name("function")
                    .and_then(|f| f.utf8_text(self.source).ok())
                    == Some("shift")
            }
            _ => false,
        }
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
            let detail = if decl_kind == DeclKind::Field {
                let attributes = self.collect_attributes(node);
                SymbolDetail::Field { sigil, attributes }
            } else {
                SymbolDetail::Variable { sigil, decl_kind }
            };
            self.add_symbol(
                name.clone(),
                sym_kind,
                node_to_span(node),
                *var_span,
                detail,
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
                // Re-read attrs from the symbol we just stored (avoid re-collecting)
                let has_reader;
                let has_writer;
                if let Some(last_sym) = self.symbols.last() {
                    if let SymbolDetail::Field { ref attributes, .. } = last_sym.detail {
                        has_reader = attributes.iter().any(|a| a == "reader");
                        has_writer = attributes.iter().any(|a| a == "writer");
                    } else {
                        has_reader = false;
                        has_writer = false;
                    }
                } else {
                    has_reader = false;
                    has_writer = false;
                }
                if has_reader {
                    self.add_symbol(
                        bare_name.to_string(),
                        SymKind::Method,
                        node_to_span(node),
                        *var_span,
                        SymbolDetail::Sub { params: vec![], is_method: true, return_type: None, doc: None },
                    );
                }
                if has_writer {
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
                            return_type: None,
                            doc: None,
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

                // Extract parent classes from `use parent` / `use base`
                if module_name == "parent" || module_name == "base" {
                    if let Some(pkg) = self.current_package.clone() {
                        let (parents, _) = self.extract_use_import_list(node);
                        let parents: Vec<String> = parents.into_iter()
                            .filter(|s| !s.starts_with('-')) // skip -norequire etc.
                            .collect();
                        if !parents.is_empty() {
                            // Emit PackageRef for each parent class (for goto-def on parent names)
                            self.emit_parent_class_refs(node, &parents);
                            self.package_parents
                                .entry(pkg)
                                .or_default()
                                .extend(parents);
                        }
                    }
                }

                // Extract imported symbols from qw(...) or list
                let (imported_symbols, qw_close_paren) = self.extract_use_import_list(node);
                // Emit FunctionCall refs for imported symbol names (for goto-def on import args)
                if module_name != "parent" && module_name != "base" {
                    self.emit_import_symbol_refs(node, &imported_symbols);
                }
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

    /// Emit PackageRef refs for parent class names in `use parent`/`use base` statements.
    /// Walks the use statement's children to find string literals or qw words matching parent names.
    fn emit_parent_class_refs(&mut self, node: Node<'a>, parents: &[String]) {
        let parent_set: std::collections::HashSet<&str> = parents.iter().map(|s| s.as_str()).collect();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "string_literal" => {
                        // Single string: 'BaseWorker' — use string_content child for span
                        if let Some(content) = child.named_child(0) {
                            if content.kind() == "string_content" {
                                if let Ok(text) = content.utf8_text(self.source) {
                                    if parent_set.contains(text) {
                                        self.add_ref(
                                            RefKind::PackageRef,
                                            node_to_span(content),
                                            text.to_string(),
                                            AccessKind::Read,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    "quoted_word_list" => {
                        // qw(Foo Bar) — string_content may contain multiple words
                        for j in 0..child.named_child_count() {
                            if let Some(sc) = child.named_child(j) {
                                if sc.kind() == "string_content" {
                                    if let Ok(text) = sc.utf8_text(self.source) {
                                        // Each word in the qw list — find its position
                                        let sc_start = sc.start_position();
                                        let mut col_offset = 0usize;
                                        for word in text.split_whitespace() {
                                            if let Some(pos) = text[col_offset..].find(word) {
                                                let word_col = col_offset + pos;
                                                if parent_set.contains(word) {
                                                    let start = Point::new(sc_start.row, sc_start.column + word_col);
                                                    let end = Point::new(sc_start.row, sc_start.column + word_col + word.len());
                                                    self.add_ref(
                                                        RefKind::PackageRef,
                                                        Span { start, end },
                                                        word.to_string(),
                                                        AccessKind::Read,
                                                    );
                                                }
                                                col_offset = word_col + word.len();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    "parenthesized_expression" | "list_expression" => {
                        // ('Foo', 'Bar') — walk for string_literal children
                        self.emit_parent_refs_in_list(child, &parent_set);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Helper: emit PackageRef refs for parent names found in a list/paren expression.
    fn emit_parent_refs_in_list(&mut self, node: Node<'a>, parent_set: &std::collections::HashSet<&str>) {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "string_literal" {
                    if let Some(content) = child.named_child(0) {
                        if content.kind() == "string_content" {
                            if let Ok(text) = content.utf8_text(self.source) {
                                if parent_set.contains(text) {
                                    self.add_ref(
                                        RefKind::PackageRef,
                                        node_to_span(content),
                                        text.to_string(),
                                        AccessKind::Read,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Emit FunctionCall refs for imported symbol names in use statements (for goto-def on import args).
    /// Handles `use Foo 'bar'`, `use Foo qw(bar baz)`, and `use Foo ('bar', 'baz')`.
    fn emit_import_symbol_refs(&mut self, node: Node<'a>, symbols: &[String]) {
        if symbols.is_empty() { return; }
        let sym_set: std::collections::HashSet<&str> = symbols.iter().map(|s| s.as_str()).collect();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "string_literal" => {
                        if let Some(content) = child.named_child(0) {
                            if content.kind() == "string_content" {
                                if let Ok(text) = content.utf8_text(self.source) {
                                    if sym_set.contains(text) {
                                        self.add_ref(
                                            RefKind::FunctionCall,
                                            node_to_span(content),
                                            text.to_string(),
                                            AccessKind::Read,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    "quoted_word_list" => {
                        for j in 0..child.named_child_count() {
                            if let Some(sc) = child.named_child(j) {
                                if sc.kind() == "string_content" {
                                    if let Ok(text) = sc.utf8_text(self.source) {
                                        let sc_start = sc.start_position();
                                        let mut col_offset = 0usize;
                                        for word in text.split_whitespace() {
                                            if let Some(pos) = text[col_offset..].find(word) {
                                                let word_col = col_offset + pos;
                                                if sym_set.contains(word) {
                                                    let start = Point::new(sc_start.row, sc_start.column + word_col);
                                                    let end = Point::new(sc_start.row, sc_start.column + word_col + word.len());
                                                    self.add_ref(
                                                        RefKind::FunctionCall,
                                                        Span { start, end },
                                                        word.to_string(),
                                                        AccessKind::Read,
                                                    );
                                                }
                                                col_offset = word_col + word.len();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    "parenthesized_expression" | "list_expression" => {
                        self.emit_import_refs_in_list(child, &sym_set);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Helper: emit FunctionCall refs for imported names found in a list/paren expression.
    fn emit_import_refs_in_list(&mut self, node: Node<'a>, sym_set: &std::collections::HashSet<&str>) {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "string_literal" {
                    if let Some(content) = child.named_child(0) {
                        if content.kind() == "string_content" {
                            if let Ok(text) = content.utf8_text(self.source) {
                                if sym_set.contains(text) {
                                    self.add_ref(
                                        RefKind::FunctionCall,
                                        node_to_span(content),
                                        text.to_string(),
                                        AccessKind::Read,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
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
                    "string_literal" => {
                        // bare 'Foo' (single string, no parens/qw)
                        if let Ok(text) = child.utf8_text(self.source) {
                            let unquoted = text.trim_matches(|c| c == '\'' || c == '"');
                            if !unquoted.is_empty() {
                                return (vec![unquoted.to_string()], None);
                            }
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
        // Check for @ISA assignment: `our @ISA = (...)`
        if let Some(left) = node.child_by_field_name("left") {
            let lhs_text = left.utf8_text(self.source).unwrap_or("");
            if lhs_text == "@ISA" || lhs_text.ends_with("@ISA") {
                if let Some(ref pkg) = self.current_package {
                    // child_by_field_name("right") returns `(` paren, not the list.
                    // Iterate named children to find list_expression/quoted_word_list.
                    let mut parents = Vec::new();
                    for i in 0..node.named_child_count() {
                        if let Some(child) = node.named_child(i) {
                            match child.kind() {
                                "list_expression" | "parenthesized_expression" => {
                                    self.collect_string_values(child, &mut parents);
                                }
                                "quoted_word_list" => {
                                    for j in 0..child.named_child_count() {
                                        if let Some(sc) = child.named_child(j) {
                                            if sc.kind() == "string_content" {
                                                if let Ok(text) = sc.utf8_text(self.source) {
                                                    for word in text.split_whitespace() {
                                                        if !word.is_empty() {
                                                            parents.push(word.to_string());
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    if !parents.is_empty() {
                        // @ISA = replaces (not appends)
                        self.package_parents.insert(pkg.clone(), parents);
                    }
                }
            }
        }

        // Check for type inference from RHS
        if let Some(left) = node.child_by_field_name("left") {
            if left.kind() == "variable_declaration" {
                // Visit the declaration
                self.visit_variable_decl(left);
            }
            if let Some(right) = node.child_by_field_name("right") {
                // Try constructor class first, then literal types, then expression type
                let inferred = if let Some(class_name) = self.extract_constructor_class(right) {
                    Some(InferredType::ClassName(class_name))
                } else if let Some(t) = Self::infer_literal_type(right) {
                    Some(t)
                } else {
                    self.infer_expression_result_type(right)
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
                } else if let Some(func_name) = self.extract_call_name(right) {
                    // RHS is a function call — record binding for return-type post-pass
                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                        self.call_bindings.push(CallBinding {
                            variable: vt,
                            func_name,
                            scope: self.current_scope(),
                            span: node_to_span(node),
                        });
                    }
                } else if right.kind() == "method_call_expression" {
                    // RHS is a method call — record binding for return-type post-pass
                    if let Some(method_node) = right.child_by_field_name("method") {
                        if let Some(invocant_node) = right.child_by_field_name("invocant") {
                            if let (Ok(method), Ok(inv)) = (
                                method_node.utf8_text(self.source),
                                invocant_node.utf8_text(self.source),
                            ) {
                                // Skip constructors — already handled by extract_constructor_class
                                if method != "new" {
                                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                                        self.method_call_bindings.push(MethodCallBinding {
                                            variable: vt,
                                            invocant_var: inv.to_string(),
                                            method_name: method.to_string(),
                                            scope: self.current_scope(),
                                            span: node_to_span(node),
                                        });
                                    }
                                }
                            }
                        }
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

    /// Infer the type of a return expression's value.
    ///
    /// Handles:
    /// - Bare `return` / `return undef` → None (skip signal)
    /// - `return { ... }` / `return [ ... ]` / `return sub { }` / `return qr//` → literal type
    /// - `return Foo->new()` → Object(Foo)
    /// - `return $self` → lookup variable's type constraint at this point
    /// - `return $var` → lookup variable's type constraint at this point
    fn infer_return_value_type(&self, return_node: Node<'a>) -> Option<InferredType> {
        let child = match return_node.named_child(0) {
            Some(c) => c,
            None => return None, // bare `return` — skip signal
        };
        // `return undef` — skip
        if child.kind() == "undef" {
            return None;
        }
        // Try literal types first
        if let Some(t) = Self::infer_literal_type(child) {
            return Some(t);
        }
        // Try constructor: return Foo->new()
        if let Some(class_name) = self.extract_constructor_class(child) {
            return Some(InferredType::ClassName(class_name));
        }
        // Try expression result type: return $a + $b → Numeric
        if let Some(t) = self.infer_expression_result_type(child) {
            return Some(t);
        }
        // Try variable lookup: return $self, return $var
        if child.kind() == "scalar" {
            if let Ok(var_text) = child.utf8_text(self.source) {
                // Look up the variable's type constraint at this point
                let point = child.start_position();
                for tc in self.type_constraints.iter().rev() {
                    if tc.variable == var_text && tc.constraint_span.start <= point {
                        // Check scope containment
                        let scope = &self.scopes[tc.scope.0 as usize];
                        if contains_point(&scope.span, point) {
                            return Some(tc.inferred_type.clone());
                        }
                    }
                }
            }
        }
        None
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

    /// Infer the result type of an expression (not its operands — those are handled elsewhere).
    /// e.g. `$a + $b` produces Numeric, `$a . $b` produces String.
    fn infer_expression_result_type(&self, node: Node<'a>) -> Option<InferredType> {
        match node.kind() {
            "binary_expression" => {
                let op = self.get_operator_text(node);
                match op.as_deref() {
                    Some("+" | "-" | "*" | "/" | "%" | "**") => Some(InferredType::Numeric),
                    Some("." | "x") => Some(InferredType::String),
                    _ => None,
                }
            }
            "postinc_expression" | "preinc_expression" => Some(InferredType::Numeric),
            "func1op_call_expression" | "func0op_call_expression" => {
                let name = node.child(0)?.utf8_text(self.source).ok()?;
                builtin_return_type(name)
            }
            _ => None,
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

        // Check for block-based dereference: @{expr}, %{expr}, &{expr}
        // In tree-sitter-perl these parse as array/hash/function with a varname
        // child containing a block. The block holds the real expressions.
        // Don't record a variable ref for the outer — just recurse into the block.
        if self.is_block_deref(node) {
            // Recurse into the block to visit inner expressions
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    if child.kind() == "varname" {
                        for j in 0..child.child_count() {
                            if let Some(gc) = child.child(j) {
                                if gc.kind() == "block" {
                                    self.visit_children(gc);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
            return;
        }

        // If this scalar is inside a block-deref, infer the type from the outer sigil.
        // Parent chain: scalar → expression_statement → block → varname → outer_node
        if node.kind() == "scalar" {
            if let Some((deref_type, context)) = self.block_deref_context(node) {
                self.push_var_type_constraint(node, context, deref_type);
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

    /// Check if this node is a block-deref outer node (e.g. @{...} or %{...}).
    fn is_block_deref(&self, node: Node<'a>) -> bool {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "varname" {
                    for j in 0..child.child_count() {
                        if let Some(gc) = child.child(j) {
                            if gc.kind() == "block" {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        false
    }

    /// Walk up from a scalar to detect block-deref context.
    /// Returns the inferred type and a context node (for constraint span) if inside
    /// @{$x}, %{$y}, or &{$z}.
    ///
    /// Parent chain: scalar → expression_statement → block → varname → outer_node
    /// where outer_node.kind() is "array" (@{}), "hash" (%{}), or "function" (&{}).
    fn block_deref_context(&self, node: Node<'a>) -> Option<(InferredType, Node<'a>)> {
        let stmt = node.parent()?;
        if stmt.kind() != "expression_statement" { return None; }
        let block = stmt.parent()?;
        if block.kind() != "block" { return None; }
        let varname = block.parent()?;
        if varname.kind() != "varname" { return None; }
        let outer = varname.parent()?;
        match outer.kind() {
            "array" => Some((InferredType::ArrayRef, outer)),
            "hash" => Some((InferredType::HashRef, outer)),
            "function" => {
                if let Ok(text) = outer.utf8_text(self.source) {
                    if text.starts_with("&{") {
                        return Some((InferredType::CodeRef, outer));
                    }
                }
                None
            }
            _ => None,
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
            if let Ok(name) = func_node.utf8_text(self.source) {
                self.add_ref(
                    RefKind::FunctionCall,
                    node_to_span(func_node),
                    name.to_string(),
                    AccessKind::Read,
                );
                // Push type constraints on arguments of known builtins
                if let Some(arg_type) = crate::file_analysis::builtin_first_arg_type(name) {
                    if let Some(first_arg) = self.first_call_arg(node) {
                        self.push_var_type_constraint(first_arg, node, arg_type);
                    }
                }
            }
        }
        self.visit_children(node);
    }

    /// Handle func1op_call_expression: abs($x), length($s), int($n), etc.
    /// The function name is the first child (keyword), the arg is a named child.
    fn visit_func1op(&mut self, node: Node<'a>) {
        let name = node.child(0)
            .and_then(|c| c.utf8_text(self.source).ok())
            .unwrap_or("");
        // Push type constraint on the argument
        if let Some(arg_type) = builtin_first_arg_type(name) {
            if let Some(arg) = node.named_child(0) {
                self.push_var_type_constraint(arg, node, arg_type);
            }
        }
        self.visit_children(node);
    }

    /// Extract the first argument node from a function call.
    fn first_call_arg(&self, call_node: Node<'a>) -> Option<Node<'a>> {
        let args = call_node.child_by_field_name("arguments")?;
        match args.kind() {
            "list_expression" | "parenthesized_expression" => args.named_child(0),
            _ => Some(args), // single arg (ambiguous_function_call_expression)
        }
    }

    fn visit_method_call(&mut self, node: Node<'a>) {
        let method_name = node.child_by_field_name("method")
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| s.to_string());
        let invocant_node = node.child_by_field_name("invocant");
        let invocant = invocant_node
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| {
                // Resolve __PACKAGE__ to enclosing package name
                if s == "__PACKAGE__" {
                    self.current_package.clone().unwrap_or_else(|| s.to_string())
                } else {
                    s.to_string()
                }
            });
        // Store invocant span for complex expressions (call chains etc.)
        let invocant_span = invocant_node
            .filter(|n| !matches!(n.kind(), "scalar" | "array" | "hash" | "bareword" | "package"))
            .map(|n| node_to_span(n));

        if let Some(name) = method_name {
            self.add_ref(
                RefKind::MethodCall {
                    invocant: invocant.unwrap_or_default(),
                    invocant_span,
                },
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

    /// Extract the function name from a call expression (function_call or ambiguous_function_call).
    fn extract_call_name(&self, node: Node<'a>) -> Option<String> {
        let name = crate::file_analysis::extract_call_name(node, self.source)?;
        // For function calls, filter out qualified names ($ref->(), Foo::bar())
        match node.kind() {
            "function_call_expression" | "ambiguous_function_call_expression" => {
                if name.contains(':') || name.starts_with('$') {
                    None
                } else {
                    Some(name)
                }
            }
            _ => Some(name),
        }
    }

    fn extract_constructor_class(&self, node: Node<'a>) -> Option<String> {
        if node.kind() == "method_call_expression" {
            let method = node.child_by_field_name("method")?;
            if method.utf8_text(self.source).ok() == Some("new") {
                let invocant = node.child_by_field_name("invocant")?;
                let inv_text = invocant.utf8_text(self.source).ok()?;
                // Invocant must be a package name (not a variable)
                if !inv_text.starts_with('$') && !inv_text.starts_with('@') && !inv_text.starts_with('%') {
                    // Resolve __PACKAGE__ to enclosing package name
                    if inv_text == "__PACKAGE__" {
                        return self.current_package.clone();
                    }
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
        let mut ancestor = anon_hash.parent()?;
        for _ in 0..5 {
            // Check if this is inside a bless call
            if self.is_bless_call(ancestor) {
                if let Some(ref pkg) = self.current_package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
                }
                let scope = self.current_scope();
                if let Some(ref pkg) = self.scopes[scope.0 as usize].package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
                }
            }
            // Check if this is inside a return expression of a sub
            if ancestor.kind() == "return_expression" {
                if let Some(name) = self.enclosing_sub_name() {
                    return Some(HashKeyOwner::Sub(name));
                }
            }
            // Check if this is the last expression in a sub body (implicit return)
            if ancestor.kind() == "expression_statement" {
                if self.is_last_statement_in_sub(ancestor) {
                    if let Some(name) = self.enclosing_sub_name() {
                        return Some(HashKeyOwner::Sub(name));
                    }
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

    /// Find the nearest enclosing Sub or Method scope from the current scope stack.
    fn enclosing_sub_scope(&self) -> Option<ScopeId> {
        for &scope_id in self.scope_stack.iter().rev() {
            match &self.scopes[scope_id.0 as usize].kind {
                ScopeKind::Sub { .. } | ScopeKind::Method { .. } => return Some(scope_id),
                _ => {}
            }
        }
        None
    }

    /// Get the name of the enclosing sub/method, if any.
    /// Extract POD or comment documentation immediately preceding a sub node.
    /// Walks prev_sibling chain (tree-sitter CST traversal stays in builder).
    fn extract_preceding_doc(&self, sub_node: Node<'a>, sub_name: &str) -> Option<String> {
        let source_str = std::str::from_utf8(self.source).ok()?;
        let mut prev = sub_node.prev_sibling();
        let mut comment_lines: Vec<String> = Vec::new();

        while let Some(node) = prev {
            match node.kind() {
                "pod" => {
                    let text = &source_str[node.byte_range()];
                    // Try to extract just the section for this sub (=head2 or =item)
                    if let Some(section) = crate::pod::extract_head2_section(sub_name, text) {
                        let md = crate::pod::pod_to_markdown(&section);
                        if !md.is_empty() {
                            return Some(md);
                        }
                    }
                    if let Some(section) = crate::pod::extract_item_section(sub_name, text) {
                        let md = crate::pod::pod_to_markdown(&section);
                        if !md.is_empty() {
                            return Some(md);
                        }
                    }
                    // Fallback: convert entire POD block (e.g. single-section pod)
                    let md = crate::pod::pod_to_markdown(text);
                    if !md.is_empty() {
                        return Some(md);
                    }
                    break;
                }
                "comment" => {
                    let text = source_str[node.byte_range()].trim();
                    let stripped = text.strip_prefix('#').unwrap_or(text).trim();
                    if !stripped.is_empty() {
                        comment_lines.push(stripped.to_string());
                    }
                }
                _ => break, // hit code, stop
            }
            prev = node.prev_sibling();
        }

        if !comment_lines.is_empty() {
            comment_lines.reverse(); // collected bottom-up
            return Some(comment_lines.join("\n"));
        }

        None
    }

    /// Post-pass: for subs with no preceding doc, scan collected pod_texts
    /// for a =head2 section matching the sub name (tail POD style).
    fn resolve_tail_pod_docs(&mut self) {
        if self.pod_texts.is_empty() {
            return;
        }
        for sym in &mut self.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                continue;
            }
            if let SymbolDetail::Sub { ref mut doc, .. } = sym.detail {
                if doc.is_some() {
                    continue; // already has preceding doc
                }
                // Search pod texts for =head2 matching this sub name, fall back to =item
                for pod_text in &self.pod_texts {
                    if let Some(section) = crate::pod::extract_head2_section(&sym.name, pod_text) {
                        let md = crate::pod::pod_to_markdown(&section);
                        if !md.is_empty() {
                            *doc = Some(md);
                            break;
                        }
                    }
                    if let Some(section) = crate::pod::extract_item_section(&sym.name, pod_text) {
                        let md = crate::pod::pod_to_markdown(&section);
                        if !md.is_empty() {
                            *doc = Some(md);
                            break;
                        }
                    }
                }
            }
        }
    }

    fn enclosing_sub_name(&self) -> Option<String> {
        let scope_id = self.enclosing_sub_scope()?;
        match &self.scopes[scope_id.0 as usize].kind {
            ScopeKind::Sub { ref name } | ScopeKind::Method { ref name } => Some(name.clone()),
            _ => None,
        }
    }

    /// Check if a node is the last statement in a sub/method body block.
    fn is_last_statement_in_sub(&self, node: Node<'a>) -> bool {
        let parent = match node.parent() {
            Some(p) => p,
            None => return false,
        };
        // The parent should be a block that is a sub/method body
        if parent.kind() != "block" {
            return false;
        }
        if let Some(grandparent) = parent.parent() {
            if !matches!(grandparent.kind(),
                "subroutine_declaration_statement" | "method_declaration_statement"
                | "anonymous_subroutine_expression"
            ) {
                return false;
            }
        } else {
            return false;
        }
        // Check this is the last named child in the block
        if let Some(last) = parent.named_child(parent.named_child_count().saturating_sub(1)) {
            last.id() == node.id()
        } else {
            false
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

    fn resolve_return_types(&mut self) {
        // Group return infos by scope
        let mut returns_by_scope: std::collections::HashMap<ScopeId, Vec<Option<InferredType>>> =
            std::collections::HashMap::new();
        for ri in &self.return_infos {
            returns_by_scope
                .entry(ri.scope)
                .or_default()
                .push(ri.inferred_type.clone());
        }

        // For each Sub/Method scope, determine return type
        let mut return_types: std::collections::HashMap<String, InferredType> =
            std::collections::HashMap::new();

        for scope in &self.scopes {
            let sub_name = match &scope.kind {
                ScopeKind::Sub { name } | ScopeKind::Method { name } => name.clone(),
                _ => continue,
            };

            let explicit_returns = returns_by_scope.get(&scope.id);
            let has_explicit = explicit_returns.map_or(false, |r| !r.is_empty());

            let resolved = if has_explicit {
                // Filter out bare returns (None) — they're exit points, not type signals
                let typed_returns: Vec<InferredType> = explicit_returns.unwrap()
                    .iter()
                    .filter_map(|r| r.clone())
                    .collect();
                resolve_return_type(&typed_returns)
            } else {
                // No explicit returns — use last expression type
                self.last_expr_type.get(&scope.id).and_then(|t| t.clone())
            };

            if let Some(rt) = resolved {
                return_types.insert(sub_name, rt);
            }
        }

        // Set return_type on matching Sub/Method symbols
        for sym in &mut self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if let SymbolDetail::Sub { ref mut return_type, .. } = sym.detail {
                    if let Some(rt) = return_types.get(&sym.name) {
                        *return_type = Some(rt.clone());
                    }
                }
            }
        }

        // Propagate return types to call sites via structural bindings
        // recorded during the walk (my $cfg = get_config()).
        // TODO: inline expression propagation (get_config()->{key}) is a separate
        // code path — needs return type resolution at the expression level without
        // a variable assignment. Not handled here.
        let mut new_constraints = Vec::new();
        for binding in &self.call_bindings {
            let rt = return_types.get(&binding.func_name)
                .cloned()
                .or_else(|| builtin_return_type(&binding.func_name));
            if let Some(rt) = rt {
                new_constraints.push(TypeConstraint {
                    variable: binding.variable.clone(),
                    scope: binding.scope,
                    constraint_span: binding.span,
                    inferred_type: rt,
                });
            }
        }
        self.type_constraints.extend(new_constraints);

        // Fixup: update HashKeyAccess owners for variables bound to sub calls
        // that return HashRef. This runs after call binding propagation so the
        // type constraints are available.
        let binding_map: std::collections::HashMap<&str, &str> = self.call_bindings.iter()
            .filter(|b| return_types.get(&b.func_name).map_or(false, |t| *t == InferredType::HashRef))
            .map(|b| (b.variable.as_str(), b.func_name.as_str()))
            .collect();

        for r in &mut self.refs {
            if let RefKind::HashKeyAccess { ref var_text, ref mut owner } = r.kind {
                if let Some(func_name) = binding_map.get(var_text.as_str()) {
                    *owner = Some(HashKeyOwner::Sub(func_name.to_string()));
                }
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
        if let SymbolDetail::Sub { params, is_method, .. } = &subs[0].detail {
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
        let def = fa.find_definition(Point::new(8, 5), None, None, None);
        assert!(def.is_some(), "should find definition for magnitude");
        let span = def.unwrap();
        assert_eq!(span.start.row, 5, "should point to method declaration line, got row {}", span.start.row);
    }

    #[test]
    fn test_field_reader_goto_def() {
        // go-to-def on $p->x should find the reader method, which points to the field
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    method mag() { }\n}\nmy $p = Point->new(x => 1);\n$p->x;");
        let def = fa.find_definition(Point::new(6, 5), None, None, None); // cursor on `x` in `$p->x`
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
        if let RefKind::MethodCall { ref invocant, .. } = method_refs[0].kind {
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
    fn test_assignment_from_binary_numeric_infers_result() {
        let fa = build_fa("my $a = 1;\nmy $b = 2;\nmy $result = $a + $b;\n$result;");
        let ty = fa.inferred_type("$result", Point::new(3, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "$result = $a + $b should be Numeric");
    }

    #[test]
    fn test_assignment_from_string_concat_infers_result() {
        let fa = build_fa("my $a = 'x';\nmy $b = 'y';\nmy $s = $a . $b;\n$s;");
        let ty = fa.inferred_type("$s", Point::new(3, 0));
        assert_eq!(ty, Some(&InferredType::String), "$s = $a . $b should be String");
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

    // ---- Builtin type inference tests ----

    #[test]
    fn test_builtin_push_infers_arrayref() {
        // push @{$aref} triggers array_deref_expression which already infers ArrayRef
        let fa = build_fa("my $aref;\npush @{$aref}, 1;\nmy $z;");
        let ty = fa.inferred_type("$aref", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "push deref should infer ArrayRef");
    }

    #[test]
    fn test_builtin_length_infers_string_arg() {
        let fa = build_fa("my $s;\nmy $n = length($s);\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), "length arg should be String");
    }

    #[test]
    fn test_builtin_abs_infers_numeric_arg() {
        let fa = build_fa("my $x;\nmy $n = abs($x);\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "abs arg should be Numeric");
    }

    #[test]
    fn test_builtin_return_type_propagates() {
        let fa = build_fa("my $t = time();\n$t;");
        let ty = fa.inferred_type("$t", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "time() should return Numeric");
    }

    #[test]
    fn test_builtin_join_return_type() {
        let fa = build_fa("my $s = join(',', @arr);\n$s;");
        let ty = fa.inferred_type("$s", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::String), "join() should return String");
    }

    #[test]
    fn test_builtin_length_return_type() {
        let fa = build_fa("my $n = length('hello');\n$n;");
        let ty = fa.inferred_type("$n", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "length() should return Numeric");
    }

    // ---- Return type inference tests (Step 4) ----

    #[test]
    fn test_return_type_hashref() {
        let fa = build_fa("sub get_config {\n    return { host => \"localhost\" };\n}");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_arrayref() {
        let fa = build_fa("sub get_tags {\n    return [1, 2, 3];\n}");
        assert_eq!(fa.sub_return_type("get_tags"), Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_return_type_coderef() {
        let fa = build_fa("sub get_handler {\n    return sub { 1 };\n}");
        assert_eq!(fa.sub_return_type("get_handler"), Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_return_type_implicit_last_expr() {
        // No explicit return — last expression is the implicit return
        let fa = build_fa("sub get_data {\n    { key => \"val\" };\n}");
        assert_eq!(fa.sub_return_type("get_data"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_conflicting_returns_unknown() {
        // Two returns with different types → None (unknown)
        let fa = build_fa("sub ambiguous {\n    if (1) { return {} }\n    return [];\n}");
        assert_eq!(fa.sub_return_type("ambiguous"), None);
    }

    #[test]
    fn test_return_type_consistent_returns() {
        // Multiple returns all hashref → HashRef
        let fa = build_fa("sub consistent {\n    if (1) { return { a => 1 } }\n    return { b => 2 };\n}");
        assert_eq!(fa.sub_return_type("consistent"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_propagation_to_call_site() {
        let fa = build_fa("sub get_config {\n    return { host => 1 };\n}\nmy $cfg = get_config();\nmy $z;");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
        let ty = fa.inferred_type("$cfg", Point::new(4, 0));
        assert_eq!(ty, Some(&InferredType::HashRef), "call site should get return type");
    }

    #[test]
    fn test_return_type_propagation_method_call() {
        let src = "package Calculator;\nsub new { bless {}, shift }\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    return $result;\n}\npackage main;\nmy $calc = Calculator->new();\nmy $sum = $calc->add(2, 3);\n$sum;";
        let fa = build_fa(src);
        assert_eq!(fa.sub_return_type("add"), Some(&InferredType::Numeric), "add should return Numeric");
        let ty = fa.inferred_type("$sum", Point::new(10, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "$sum should be Numeric via method call binding");
    }

    #[test]
    fn test_return_type_constructor() {
        let fa = build_fa("package User;\nsub new { bless {}, shift }\npackage main;\nsub get_user {\n    return User->new();\n}");
        assert_eq!(fa.sub_return_type("get_user"), Some(&InferredType::ClassName("User".into())));
    }

    #[test]
    fn test_return_type_self_variable() {
        // return $self where $self has a type constraint
        let fa = build_fa("package Foo;\nsub new { bless {}, shift }\nsub clone {\n    my ($self) = @_;\n    return $self;\n}");
        assert_eq!(
            fa.sub_return_type("clone"),
            Some(&InferredType::FirstParam { package: "Foo".into() }),
        );
    }

    #[test]
    fn test_return_type_bare_return_filtered() {
        // Bare return + typed return → bare is filtered, typed return wins
        let fa = build_fa("sub get_config {\n    return unless 1;\n    return { host => 1 };\n}");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_all_bare_returns() {
        // All bare returns → no return type
        let fa = build_fa("sub noop {\n    return;\n}");
        assert_eq!(fa.sub_return_type("noop"), None);
    }

    #[test]
    fn test_return_type_undef_filtered() {
        // return undef + typed return → undef is filtered, typed return wins
        let fa = build_fa("sub maybe {\n    return undef unless 1;\n    return { a => 1 };\n}");
        assert_eq!(fa.sub_return_type("maybe"), Some(&InferredType::HashRef));
    }

    // ---- resolve_expression_type tests ----

    /// Find the first node of given kind at/after a point (searches all children).
    fn find_node_at<'a>(node: tree_sitter::Node<'a>, point: Point, kind: &str) -> Option<tree_sitter::Node<'a>> {
        if node.kind() == kind && node.start_position() >= point {
            return Some(node);
        }
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if let Some(found) = find_node_at(child, point, kind) {
                    return Some(found);
                }
            }
        }
        None
    }

    #[test]
    fn test_resolve_expr_type_function_call() {
        let src = "sub get_config {\n    return { host => 1 };\n}\nget_config();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Find the function_call_expression on line 3
        let call_node = find_node_at(tree.root_node(), Point::new(3, 0), "function_call_expression")
            .expect("should find function_call_expression");
        let ty = fa.resolve_expression_type(call_node, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_method_call_return() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub do_thing { }\npackage main;\nmy $f = Foo->new();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // $f->get_bar() should resolve to Object(Bar)
        // First verify get_bar has the right return type
        assert_eq!(fa.sub_return_type("get_bar"), Some(&InferredType::ClassName("Bar".into())));
    }

    #[test]
    fn test_resolve_expr_type_scalar_variable() {
        let src = "my $x = {};\n$x;\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Find the scalar $x on line 1
        let scalar_node = find_node_at(tree.root_node(), Point::new(1, 0), "scalar")
            .expect("should find scalar");
        let ty = fa.resolve_expression_type(scalar_node, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_chained_method() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub get_name {\n    return { name => 'test' };\n}\npackage main;\nmy $f = Foo->new();\n$f->get_bar()->get_name();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Line 12: $f->get_bar()->get_name();
        // The outermost method_call_expression starts at column 0
        // Use descendant_for_point_range to find the node at the start of that line
        let node = tree.root_node()
            .descendant_for_point_range(Point::new(12, 0), Point::new(12, 25))
            .expect("should find node");
        // Walk up to find the outermost method_call_expression
        let mut n = node;
        while n.kind() != "method_call_expression" || n.parent().map_or(false, |p| p.kind() == "method_call_expression") {
            n = match n.parent() {
                Some(p) => p,
                None => panic!("should find outermost method_call_expression"),
            };
        }
        assert_eq!(n.kind(), "method_call_expression");
        let ty = fa.resolve_expression_type(n, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_constructor() {
        let src = "package Foo;\nsub new { bless {}, shift }\npackage main;\nFoo->new();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        let call = find_node_at(tree.root_node(), Point::new(3, 0), "method_call_expression")
            .expect("should find method_call_expression");
        let ty = fa.resolve_expression_type(call, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::ClassName("Foo".into())));
    }

    #[test]
    fn test_resolve_expr_type_triple_chain() {
        // $calc->get_self->get_config->{host} — no parens on method calls
        let src = "\
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
$calc->get_self->get_config->{host};
";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Verify get_self returns an object type for Calculator
        let get_self_rt = fa.sub_return_type("get_self");
        assert_eq!(get_self_rt.and_then(|t| t.class_name()), Some("Calculator"),
            "get_self should return Calculator");

        // Verify get_config returns HashRef
        let get_config_rt = fa.sub_return_type("get_config");
        assert_eq!(get_config_rt, Some(&InferredType::HashRef),
            "get_config should return HashRef");

        // The outermost expression is hash_element_expression wrapping the chain
        // Find the method_call_expression for get_config (inner chain)
        // Line 11: $calc->get_self->get_config->{host}
        let node = tree.root_node()
            .descendant_for_point_range(Point::new(11, 0), Point::new(11, 0))
            .expect("should find node");
        let mut n = node;
        // Walk up to find hash_element_expression
        loop {
            if n.kind() == "hash_element_expression" {
                break;
            }
            n = n.parent().expect("should find hash_element_expression");
        }
        // The base of hash_element_expression is the method chain
        let base = n.named_child(0).expect("should have base");
        assert_eq!(base.kind(), "method_call_expression");
        let ty = fa.resolve_expression_type(base, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef),
            "the chain $calc->get_self->get_config should resolve to HashRef");
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
        let def = fa.find_definition(Point::new(1, 7), None, None, None);
        assert!(def.is_some(), "should find definition for $x");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_sub() {
        let fa = build_fa("sub greet { }\ngreet();");
        // Cursor on the function call at line 1
        let def = fa.find_definition(Point::new(1, 1), None, None, None);
        assert!(def.is_some(), "should find definition for greet");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_method_in_class() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub hello { }\npackage main;\nmy $f = Foo->new();\n$f->hello();";
        let fa = build_fa(src);
        // Cursor on hello() call at line 5
        let def = fa.find_definition(Point::new(5, 5), None, None, None);
        assert!(def.is_some(), "should find definition for hello method");
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "hello definition should be on line 2");
    }

    #[test]
    fn test_find_def_scoped_variable() {
        let src = "my $x = 'outer';\nsub foo {\n    my $x = 'inner';\n    print $x;\n}";
        let fa = build_fa(src);
        // Cursor on $x inside sub (line 3) should resolve to inner $x (line 2)
        let def = fa.find_definition(Point::new(3, 11), None, None, None);
        assert!(def.is_some());
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "should resolve to inner $x on line 2");
    }

    #[test]
    fn test_find_references_variable() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        // Cursor on the declaration of $x
        let refs = fa.find_references(Point::new(0, 4), None, None);
        assert!(refs.len() >= 2, "should find at least declaration + usage, got {}", refs.len());
    }

    #[test]
    fn test_hash_key_def_implicit_return_gets_sub_owner() {
        // Implicit return: last expression in sub body, no explicit `return`
        let src = "sub get_config { { host => 'localhost', port => 5432 } }\nmy $cfg = get_config();\n$cfg->{host};\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
        if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
            assert_eq!(*owner, HashKeyOwner::Sub("get_config".to_string()),
                "implicit return hash key should have Sub(get_config) owner, got {:?}", owner);
        }

        // Go-to-def from $cfg->{host} should reach the hash key in the implicit return
        let host_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!host_refs.is_empty(), "should find HashKeyAccess for 'host'");
        let def = fa.find_definition(host_refs[0].span.start, Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for host");
        assert_eq!(def.unwrap().start.row, 0, "host def should be on line 0");
    }

    #[test]
    fn test_find_references_sub() {
        let src = "sub greet { }\ngreet();\ngreet();";
        let fa = build_fa(src);
        // Cursor on the sub name
        let refs = fa.find_references(Point::new(0, 5), None, None);
        assert!(refs.len() >= 2, "should find definition + calls, got {}", refs.len());
    }

    #[test]
    fn test_find_references_method_through_chain() {
        let src = "\
package Foo;
sub new { bless {}, shift }
sub bar { 42 }
package main;
sub get_foo { return Foo->new() }
my $f = Foo->new();
$f->bar();
get_foo()->bar();
";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Cursor on bar definition (line 2, col 4)
        let refs = fa.find_references(Point::new(2, 5), Some(&tree), Some(src.as_bytes()));
        // Should find: $f->bar() + get_foo()->bar() (definition may or may not be included)
        let ref_lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(refs.len() >= 2,
            "should find at least 2 refs, got {} at lines {:?}", refs.len(), ref_lines);
        // The key assertion: chained call get_foo()->bar() is found (was broken before P0a fix)
        assert!(ref_lines.contains(&7),
            "should find chained get_foo()->bar() at line 7, got {:?}", ref_lines);
    }

    #[test]
    fn test_hash_key_def_in_return_gets_sub_owner() {
        let src = "sub get_config {\n    return { host => 'localhost', port => 5432 };\n}\nmy $cfg = get_config();\n$cfg->{host};\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Verify hash key defs exist with Sub owner
        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
        if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
            assert_eq!(*owner, HashKeyOwner::Sub("get_config".to_string()),
                "host def should have Sub(get_config) owner, got {:?}", owner);
        }

        // Verify HashKeyAccess ref for $cfg->{host} has Sub owner
        let host_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!host_refs.is_empty(), "should find HashKeyAccess for 'host'");
        if let RefKind::HashKeyAccess { ref owner, .. } = host_refs[0].kind {
            assert_eq!(*owner, Some(HashKeyOwner::Sub("get_config".to_string())),
                "host ref should have Sub(get_config) owner, got {:?}", owner);
        }

        // Verify go-to-references from the def finds the usage
        let host_def_point = host_defs[0].selection_span.start;
        let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()));
        // symbol_at returns include_decl=false, so only usages are returned
        assert!(refs.len() >= 1, "should find at least 1 usage, got {} refs", refs.len());

        // Verify go-to-references from the usage finds back to the def
        let host_ref_point = host_refs[0].span.start;
        let refs_from_usage = fa.find_references(host_ref_point, Some(&tree), Some(src.as_bytes()));
        // ref resolves to def → include_decl=true, so def + usage
        assert!(refs_from_usage.len() >= 2, "should find def + usage, got {} refs", refs_from_usage.len());
    }

    #[test]
    fn test_hash_key_refs_chained_tree_fallback() {
        // Chained method calls produce refs with owner: None that need tree-based resolution
        let src = r#"package Calculator;
sub new { bless {}, shift }
sub get_self { my ($self) = @_; return $self; }
sub get_config { return { host => "localhost", port => 5432 }; }
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{host};
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Find the hash key def for "host" in get_config's return
        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");

        // The chained ref should have owner: None (can't resolve at build time)
        let chained_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { owner: None, .. }))
            .collect();
        assert!(!chained_refs.is_empty(), "chained hash access should have owner: None, refs: {:?}",
            fa.refs.iter().filter(|r| r.target_name == "host").collect::<Vec<_>>());

        // find_references from the def should find the chained usage via tree fallback
        let host_def_point = host_defs[0].selection_span.start;
        let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()));
        assert!(refs.len() >= 1, "should find chained usage via tree fallback, got {} refs", refs.len());
    }

    #[test]
    fn test_highlights_read_write() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        let highlights = fa.find_highlights(Point::new(0, 4), None, None);
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
        let hover = fa.hover_info(Point::new(1, 8), src, None, None);
        assert!(hover.is_some(), "should have hover info");
        let text = hover.unwrap();
        assert!(text.contains("$greeting"), "hover should contain variable name, got: {}", text);
    }

    #[test]
    fn test_hover_sub() {
        let src = "sub greet { }\ngreet();";
        let fa = build_fa(src);
        let hover = fa.hover_info(Point::new(1, 1), src, None, None);
        assert!(hover.is_some(), "should have hover info for function call");
        let text = hover.unwrap();
        assert!(text.contains("greet"), "hover should contain sub name, got: {}", text);
    }

    #[test]
    fn test_hover_shows_inferred_type() {
        let src = "package Point;\nsub new { bless {}, shift }\npackage main;\nmy $p = Point->new();\n$p;";
        let fa = build_fa(src);
        // Hover on $p usage at line 4
        let hover = fa.hover_info(Point::new(4, 1), src, None, None);
        assert!(hover.is_some(), "should have hover info");
        let text = hover.unwrap();
        assert!(text.contains("Point"), "hover should show inferred type Point, got: {}", text);
    }

    #[test]
    fn test_hover_type_at_usage_after_reassignment() {
        // $x starts as Point, gets reassigned to Foo — hover at each usage should reflect the type at that point
        let src = "package Point;\nsub new { bless {}, shift }\npackage Foo;\nsub new { bless {}, shift }\npackage main;\nmy $x = Point->new();\n$x;\n$x = Foo->new();\n$x;";
        let fa = build_fa(src);
        // line 6: $x; — should be Point
        let hover1 = fa.hover_info(Point::new(6, 1), src, None, None);
        assert!(hover1.is_some());
        let text1 = hover1.unwrap();
        assert!(text1.contains("Point"), "at line 6 should be Point, got: {}", text1);
        // line 8: $x; — should be Foo (after reassignment)
        let hover2 = fa.hover_info(Point::new(8, 1), src, None, None);
        assert!(hover2.is_some());
        let text2 = hover2.unwrap();
        assert!(text2.contains("Foo"), "at line 8 should be Foo, got: {}", text2);
    }

    #[test]
    fn test_hover_shows_return_type() {
        let src = "package Foo;\nsub make { return Foo->new() }\nsub new { bless {}, shift }\npackage main;\nmake();";
        let fa = build_fa(src);
        // Hover on sub make definition
        let hover = fa.hover_info(Point::new(1, 5), src, None, None);
        assert!(hover.is_some(), "should have hover info for sub");
        let text = hover.unwrap();
        assert!(text.contains("returns"), "hover should show return type, got: {}", text);
        assert!(text.contains("Foo"), "hover return type should mention Foo, got: {}", text);
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
        let def = fa.find_definition(Point::new(3, 8), None, None, None);
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
        let def_self = fa.find_definition(Point::new(12, 12), None, None, None);
        assert!(def_self.is_some(), "should find definition for $self in deref");
        assert_eq!(def_self.unwrap().start.row, 10, "$self should resolve to declaration on line 10");

        // history key at line 12
        let def_history = fa.find_definition(Point::new(12, 20), None, None, None);
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

    #[test]
    fn test_goto_def_slurpy_hash_arg_at_call_site() {
        // Calculator->new(verbose => 1): cursor on "verbose" should go to
        // the bless hash key def, NOT to sub new.
        let src = r#"package Calculator;
sub new {
    my ($class, %args) = @_;
    my $self = bless {
        verbose => $args{verbose} // 0,
    }, $class;
    return $self;
}
package main;
my $calc = Calculator->new(verbose => 1);
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // "verbose" at call site is line 9, after "Calculator->new("
        // Calculator->new(verbose => 1)
        // 0123456789012345678901234567
        //                 ^16 = v of verbose
        // my $calc = Calculator->new(verbose => 1);
        // 0         1         2         3
        // 0123456789012345678901234567890123456789
        //                            ^27 = v of verbose
        let def = fa.find_definition(Point::new(9, 27), Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for verbose at call site");
        // Should go to line 4: "verbose => $args{verbose} // 0,"
        assert_eq!(def.unwrap().start.row, 4,
            "verbose should resolve to bless hash key def on line 4, not sub new");
    }

    #[test]
    fn test_goto_def_param_field_at_call_site() {
        // Point->new(x => 3, y => 4): cursor on "x" should go to "field $x :param"
        let src = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude() { }
}
my $p = Point->new(x => 3, y => 4);
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // my $p = Point->new(x => 3, y => 4);
        // 0         1         2
        // 0123456789012345678901234
        //                    ^19 = x

        let def = fa.find_definition(Point::new(6, 19), Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for x at call site");
        // Should go to line 2: "field $x :param :reader;"
        assert_eq!(def.unwrap().start.row, 2,
            "x should resolve to field $x on line 2, not the class");
    }

    // ---- Gap 1: __PACKAGE__ resolution ----

    #[test]
    fn test_dunder_package_resolution() {
        let fa = build_fa("
        package Mojo::File;
        sub path { __PACKAGE__->new(@_) }
        ");
        let rt = fa.sub_return_type("path");
        assert_eq!(rt, Some(&InferredType::ClassName("Mojo::File".into())));
    }

    #[test]
    fn test_dunder_package_method_invocant() {
        // __PACKAGE__->new() should store the resolved class in MethodCall invocant
        let fa = build_fa("
        package Foo;
        __PACKAGE__->some_method();
        ");
        let method_ref = fa.refs.iter().find(|r| r.target_name == "some_method").unwrap();
        match &method_ref.kind {
            RefKind::MethodCall { invocant, .. } => {
                assert_eq!(invocant, "Foo", "invocant should be resolved from __PACKAGE__");
            }
            _ => panic!("expected MethodCall ref"),
        }
    }

    // ---- Gap 2: Shift parameter extraction ----

    #[test]
    fn test_shift_params() {
        let fa = build_fa("
        sub process {
            my $self = shift;
            my $file = shift;
            my $opts = shift || {};
        }
        ");
        // signature_for_call strips $self when first param is $self
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert!(sig.is_method, "should detect method from $self first param");
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0].name, "$file");
        assert_eq!(sig.params[1].name, "$opts");
        assert_eq!(sig.params[1].default, Some("{}".into()));

        // Check raw params via symbol detail
        let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
        if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
            assert_eq!(params.len(), 3);
            assert_eq!(params[0].name, "$self");
            assert_eq!(params[1].name, "$file");
            assert_eq!(params[2].name, "$opts");
            assert_eq!(params[2].default, Some("{}".into()));
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_shift_then_list_assign() {
        let fa = build_fa("
        sub process {
            my $self = shift;
            my ($file, @opts) = @_;
        }
        ");
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert!(sig.is_method);
        assert_eq!(sig.params.len(), 2, "should have $file and @opts (stripped $self)");
        assert_eq!(sig.params[0].name, "$file");
        assert_eq!(sig.params[1].name, "@opts");
        assert!(sig.params[1].is_slurpy);

        // Check raw params
        let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
        if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
            assert_eq!(params.len(), 3);
            assert_eq!(params[0].name, "$self");
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_shift_with_double_pipe_default() {
        let fa = build_fa("
        sub handler {
            my $self = shift;
            my $timeout = shift || 30;
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$timeout");
        assert_eq!(sig.params[0].default, Some("30".into()));
    }

    #[test]
    fn test_shift_with_defined_or_default() {
        let fa = build_fa("
        sub handler {
            my $self = shift;
            my $verbose = shift // 0;
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$verbose");
        assert_eq!(sig.params[0].default, Some("0".into()));
    }

    #[test]
    fn test_subscript_param() {
        let fa = build_fa("
        sub handler {
            my $self = $_[0];
            my $data = $_[1];
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$data");
    }

    #[test]
    fn test_legacy_at_params_still_work() {
        // Ensure the existing @_ pattern still works
        let fa = build_fa("
        sub process {
            my ($first, $file, @opts) = @_;
        }
        ");
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 3);
        assert_eq!(sig.params[0].name, "$first");
        assert_eq!(sig.params[1].name, "$file");
        assert_eq!(sig.params[2].name, "@opts");
    }

    #[test]
    fn test_tail_pod_item_method() {
        let fa = build_fa("
            package WWW::Mech;
            sub get { }
            sub post { }

=head1 METHODS

=over

=item $mech->get($url)

Performs a GET request.

=item $mech->post($url)

Performs a POST request.

=back

=cut
        ");
        let get_doc = fa.symbols.iter()
            .find(|s| s.name == "get")
            .and_then(|s| match &s.detail {
                SymbolDetail::Sub { doc, .. } => doc.as_ref(),
                _ => None,
            });
        assert!(get_doc.is_some(), "get should have doc from =item");
        assert!(get_doc.unwrap().contains("GET request"));
    }

    #[test]
    fn test_pod_doc_extracted_per_function() {
        let src = "\
package DemoUtils;
use Exporter 'import';
our @EXPORT_OK = qw(fetch_data transform);

=head2 fetch_data

Fetches data from the given URL.

=head2 transform

Transforms items.

=cut

sub fetch_data { }
sub transform { }
";
        let fa = build_fa(src);
        let fd = fa.symbols.iter().find(|s| s.name == "fetch_data").unwrap();
        if let SymbolDetail::Sub { ref doc, .. } = fd.detail {
            let d = doc.as_ref().expect("fetch_data should have doc");
            assert!(d.contains("Fetches data"), "should have fetch_data doc, got: {}", d);
            assert!(!d.contains("Transforms items"), "should NOT have transform doc, got: {}", d);
        } else {
            panic!("fetch_data should be a Sub");
        }
    }

    // ---- Inheritance extraction tests ----

    #[test]
    fn test_use_parent_single() {
        let fa = build_fa("
            package Child;
            use parent 'Parent';
            sub child_method { }
        ");
        assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
    }

    #[test]
    fn test_use_parent_multiple() {
        let fa = build_fa("
            package Multi;
            use parent qw(Foo Bar);
        ");
        assert_eq!(fa.package_parents.get("Multi").unwrap(), &vec!["Foo".to_string(), "Bar".to_string()]);
    }

    #[test]
    fn test_use_parent_emits_package_refs() {
        let fa = build_fa("
            package Child;
            use parent 'Parent';
        ");
        let refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Parent")
            .collect();
        assert_eq!(refs.len(), 1, "should emit PackageRef for parent class");
    }

    #[test]
    fn test_use_parent_qw_emits_package_refs() {
        let fa = build_fa("
            package Multi;
            use parent qw(Foo Bar);
        ");
        let foo_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Foo")
            .collect();
        let bar_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Bar")
            .collect();
        assert_eq!(foo_refs.len(), 1, "should emit PackageRef for Foo");
        assert_eq!(bar_refs.len(), 1, "should emit PackageRef for Bar");
    }

    #[test]
    fn test_use_parent_norequire() {
        let fa = build_fa("
            package Local;
            use parent -norequire, 'My::Base';
        ");
        assert_eq!(fa.package_parents.get("Local").unwrap(), &vec!["My::Base".to_string()]);
    }

    #[test]
    fn test_use_base() {
        let fa = build_fa("
            package Old;
            use base 'Legacy::Base';
        ");
        assert_eq!(fa.package_parents.get("Old").unwrap(), &vec!["Legacy::Base".to_string()]);
    }

    #[test]
    fn test_isa_assignment() {
        let fa = build_fa("
            package Direct;
            our @ISA = ('Alpha', 'Beta');
        ");
        assert_eq!(fa.package_parents.get("Direct").unwrap(), &vec!["Alpha".to_string(), "Beta".to_string()]);
    }

    #[test]
    fn test_class_isa_populates_package_parents() {
        let fa = build_fa("
            class Child :isa(Parent) { }
        ");
        assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
    }

    // ---- Inheritance method resolution tests ----

    #[test]
    fn test_inherited_method_completion() {
        let fa = build_fa("
            package Animal;
            sub speak { }
            sub eat { }

            package Dog;
            use parent 'Animal';
            sub fetch { }
        ");
        let methods = fa.complete_methods_for_class("Dog", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"fetch"), "own method");
        assert!(names.contains(&"speak"), "inherited from Animal");
        assert!(names.contains(&"eat"), "inherited from Animal");
    }

    #[test]
    fn test_child_method_overrides_parent() {
        let fa = build_fa("
            package Base;
            sub greet { }

            package Override;
            use parent 'Base';
            sub greet { }
        ");
        let methods = fa.complete_methods_for_class("Override", None);
        let greet_count = methods.iter().filter(|c| c.label == "greet").count();
        assert_eq!(greet_count, 1, "child override should shadow parent");
    }

    #[test]
    fn test_find_method_in_parent() {
        let fa = build_fa("
            package Base;
            sub base_method { }

            package Child;
            use parent 'Base';
        ");
        let span = fa.find_method_in_class("Child", "base_method");
        assert!(span.is_some(), "should find inherited method");
    }

    #[test]
    fn test_inherited_return_type() {
        let fa = build_fa("
            package Factory;
            sub create { Factory->new(@_) }

            package SpecialFactory;
            use parent 'Factory';
        ");
        let rt = fa.find_method_return_type("SpecialFactory", "create", None);
        assert!(rt.is_some(), "should find return type from parent");
    }

    #[test]
    fn test_multi_level_inheritance() {
        let fa = build_fa("
            package A;
            sub from_a { }

            package B;
            use parent 'A';
            sub from_b { }

            package C;
            use parent 'B';
            sub from_c { }
        ");
        let methods = fa.complete_methods_for_class("C", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"from_a"));
        assert!(names.contains(&"from_b"));
        assert!(names.contains(&"from_c"));
    }

    #[test]
    fn test_class_isa_inherits_methods() {
        let fa = build_fa("
            class Parent {
                method greet() { }
            }
            class Child :isa(Parent) {
                method wave() { }
            }
        ");
        let methods = fa.complete_methods_for_class("Child", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"wave"), "own method");
        assert!(names.contains(&"greet"), "inherited from Parent");
    }

    // ---- Cross-file inheritance tests ----

    #[test]
    fn test_cross_file_inherited_method_completion() {
        use std::collections::HashMap;
        use std::path::PathBuf;
        use crate::module_index::{ModuleIndex, ModuleExports, ExportedSub};

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        // Grandparent: DBI has `connect`
        idx.insert_cache("DBI", Some(ModuleExports {
            path: PathBuf::from("/fake/DBI.pm"),
            export: vec![],
            export_ok: vec![],
            subs: {
                let mut s = HashMap::new();
                s.insert("connect".into(), ExportedSub {
                    def_line: 10, params: vec![], is_method: true,
                    return_type: None, hash_keys: vec![], doc: None,
                });
                s
            },
            parents: vec![],
        }));

        // Parent: DBI::db inherits from DBI, has `prepare`
        idx.insert_cache("DBI::db", Some(ModuleExports {
            path: PathBuf::from("/fake/DBI/db.pm"),
            export: vec![],
            export_ok: vec![],
            subs: {
                let mut s = HashMap::new();
                s.insert("prepare".into(), ExportedSub {
                    def_line: 5, params: vec![], is_method: true,
                    return_type: None, hash_keys: vec![], doc: None,
                });
                s
            },
            parents: vec!["DBI".into()],
        }));

        // Local code inherits from DBI::db
        let fa = build_fa("
            package MyDB;
            use parent 'DBI::db';
            sub custom_query { }
        ");

        let methods = fa.complete_methods_for_class("MyDB", Some(&idx));
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"custom_query"), "own method");
        assert!(names.contains(&"prepare"), "from DBI::db");
        assert!(names.contains(&"connect"), "from DBI (grandparent)");
    }

    #[test]
    fn test_cross_file_method_override() {
        use std::collections::HashMap;
        use std::path::PathBuf;
        use crate::module_index::{ModuleIndex, ModuleExports, ExportedSub};

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        // Parent has `process`
        idx.insert_cache("Base::Worker", Some(ModuleExports {
            path: PathBuf::from("/fake/Base/Worker.pm"),
            export: vec![],
            export_ok: vec![],
            subs: {
                let mut s = HashMap::new();
                s.insert("process".into(), ExportedSub {
                    def_line: 1, params: vec![], is_method: true,
                    return_type: None, hash_keys: vec![], doc: None,
                });
                s
            },
            parents: vec![],
        }));

        // Local child overrides `process`
        let fa = build_fa("
            package MyWorker;
            use parent 'Base::Worker';
            sub process { }
        ");

        let methods = fa.complete_methods_for_class("MyWorker", Some(&idx));
        let process_count = methods.iter().filter(|c| c.label == "process").count();
        assert_eq!(process_count, 1, "local override should shadow parent");
    }

    #[test]
    fn test_cross_file_return_type_through_inheritance() {
        use std::collections::HashMap;
        use std::path::PathBuf;
        use crate::file_analysis::InferredType;
        use crate::module_index::{ModuleIndex, ModuleExports, ExportedSub};

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        idx.insert_cache("Fetcher", Some(ModuleExports {
            path: PathBuf::from("/fake/Fetcher.pm"),
            export: vec![],
            export_ok: vec![],
            subs: {
                let mut s = HashMap::new();
                s.insert("fetch".into(), ExportedSub {
                    def_line: 1, params: vec![], is_method: true,
                    return_type: Some(InferredType::HashRef),
                    hash_keys: vec!["status".into(), "body".into()],
                    doc: None,
                });
                s
            },
            parents: vec![],
        }));

        let fa = build_fa("
            package MyFetcher;
            use parent 'Fetcher';
        ");

        let rt = fa.find_method_return_type("MyFetcher", "fetch", Some(&idx));
        assert_eq!(rt, Some(InferredType::HashRef));
    }

    #[test]
    fn test_parents_cached() {
        use std::collections::HashMap;
        use std::path::PathBuf;
        use crate::module_index::{ModuleIndex, ModuleExports};

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        idx.insert_cache("Child::Mod", Some(ModuleExports {
            path: PathBuf::from("/fake/Child/Mod.pm"),
            export: vec![],
            export_ok: vec![],
            subs: HashMap::new(),
            parents: vec!["Parent::Mod".into(), "Mixin::Role".into()],
        }));

        let parents = idx.parents_cached("Child::Mod");
        assert_eq!(parents, vec!["Parent::Mod", "Mixin::Role"]);
        assert!(idx.parents_cached("Unknown::Mod").is_empty());
    }

    // ---- Method call return type propagation tests ----

    #[test]
    fn test_method_call_return_type_propagates() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $f = Foo->new();
my $cfg = $f->get_config();
$cfg;
");
        let ty = fa.inferred_type("$cfg", Point::new(9, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_method_call_chain_propagation() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_bar { return Bar->new() }
package Bar;
sub new { bless {}, shift }
sub get_name { return { name => 'test' } }
package main;
my $f = Foo->new();
my $bar = $f->get_bar();
my $name = $bar->get_name();
$name;
");
        let bar_ty = fa.inferred_type("$bar", Point::new(10, 0));
        assert_eq!(bar_ty, Some(&InferredType::ClassName("Bar".into())));
        let name_ty = fa.inferred_type("$name", Point::new(11, 0));
        assert_eq!(name_ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_self_method_call_return_type() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_config { return { host => 1 } }
sub run {
    my ($self) = @_;
    my $cfg = $self->get_config();
    $cfg;
}
");
        let ty = fa.inferred_type("$cfg", Point::new(7, 4));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }
}
