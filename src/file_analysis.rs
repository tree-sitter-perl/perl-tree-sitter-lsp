//! FileAnalysis: single-pass scope graph for Perl source files.
//!
//! Built once per parse/reparse via `builder::build()`. Every LSP query
//! becomes a lookup against these tables instead of a tree walk.
//!
//! Designed to compose into a project index: `HashMap<PathBuf, FileAnalysis>`.

use std::collections::{HashMap, HashSet};
use tree_sitter::Point;

// ---- Shared types (formerly in analysis.rs) ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

#[derive(Debug, Clone)]
pub struct FoldRange {
    pub start_line: usize,
    pub end_line: usize,
    pub kind: FoldKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum FoldKind {
    Region,
    Comment,
}

// ---- IDs ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

// ---- Scope ----

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub span: Span,
    /// The enclosing package/class name at this scope level.
    /// For `package Foo;` regions, this is "Foo".
    /// Inherited from parent when not overridden.
    pub package: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ScopeKind {
    File,
    /// Region after `package Foo;` until next package statement or EOF.
    Package,
    /// `class Foo { ... }` block.
    Class { name: String },
    /// `sub foo { ... }` block.
    Sub { name: String },
    /// `method foo { ... }` block.
    Method { name: String },
    /// Bare `{ ... }`, if/while/for bodies, etc.
    Block,
    /// `for my $x (...) { }` — loop variable scoped to block.
    ForLoop { var: String },
}

// ---- Symbol ----

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymKind,
    pub span: Span,
    pub selection_span: Span,
    /// Scope this symbol is declared in.
    pub scope: ScopeId,
    /// Kind-specific extra data.
    pub detail: SymbolDetail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymKind {
    Variable,
    Sub,
    Method,
    Package,
    Class,
    Module,
    Field,
    HashKeyDef,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum SymbolDetail {
    Variable {
        sigil: char,
        decl_kind: DeclKind,
    },
    Sub {
        params: Vec<ParamInfo>,
        is_method: bool,
        /// Inferred return type from analyzing return statements.
        return_type: Option<InferredType>,
    },
    Class {
        parent: Option<String>,
        roles: Vec<String>,
        fields: Vec<FieldDetail>,
    },
    Field {
        sigil: char,
        attributes: Vec<String>,
    },
    HashKeyDef {
        owner: HashKeyOwner,
        is_dynamic: bool,
    },
    /// Package, Module, or other kinds needing no extra data.
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclKind {
    My,
    Our,
    State,
    Field,
    Param,
    ForVar,
}

#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub name: String,
    pub default: Option<String>,
    pub is_slurpy: bool,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct FieldDetail {
    pub name: String,
    pub sigil: char,
    pub attributes: Vec<String>,
}

// ---- Ref ----

#[derive(Debug, Clone)]
pub struct Ref {
    pub kind: RefKind,
    pub span: Span,
    pub scope: ScopeId,
    pub target_name: String,
    pub access: AccessKind,
    /// For variable refs: which Symbol this resolves to (filled in post-pass).
    pub resolves_to: Option<SymbolId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum RefKind {
    Variable,
    FunctionCall,
    MethodCall {
        invocant: String,
        /// Span of the invocant node (for complex expressions needing tree resolution).
        invocant_span: Option<Span>,
    },
    PackageRef,
    HashKeyAccess {
        var_text: String,
        owner: Option<HashKeyOwner>,
    },
    /// The container variable in `$hash{key}`, `@arr[0]`, etc.
    ContainerAccess,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessKind {
    Read,
    Write,
    Declaration,
}

// ---- Type constraints ----

#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub variable: String,
    pub scope: ScopeId,
    pub constraint_span: Span,
    pub inferred_type: InferredType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferredType {
    /// `$p = Point->new(...)` — variable is an instance of ClassName.
    ClassName(String),
    /// `my ($self) = @_` in `package Foo` — first param is the class.
    FirstParam { package: String },
    /// `bless { ... }, 'Foo'` — result is a Foo.
    BlessResult { package: String },
    /// `$x = {}` or `$x = { ... }` — unblessed hash reference.
    HashRef,
    /// `$x = []` or `$x = [ ... ]` — unblessed array reference.
    ArrayRef,
    /// `$x = sub { ... }` — code reference.
    CodeRef,
    /// `$x = qr/.../` — compiled regular expression.
    Regexp,
    /// Used in numeric context (`+`, `-`, `==`, etc.).
    Numeric,
    /// Used in string context (`.`, `eq`, `=~`, etc.).
    String,
}

impl InferredType {
    /// Extract the class name if this is an object type (ClassName, FirstParam, or BlessResult).
    pub fn class_name(&self) -> Option<&str> {
        match self {
            InferredType::ClassName(name) => Some(name.as_str()),
            InferredType::FirstParam { package } => Some(package.as_str()),
            InferredType::BlessResult { package } => Some(package.as_str()),
            _ => None,
        }
    }

    /// True if this is any Object variant (ClassName, FirstParam, BlessResult).
    pub fn is_object(&self) -> bool {
        self.class_name().is_some()
    }
}

/// Resolve a return type from a list of inferred types (one per return statement).
///
/// Rules (from spec):
/// - All agree → that type
/// - Object subsumes HashRef (overloaded objects are common)
/// - Disagreement → None (Unknown)
///
/// The input should already have bare returns / undef filtered out.
pub fn resolve_return_type(return_types: &[InferredType]) -> Option<InferredType> {
    if return_types.is_empty() {
        return None;
    }
    let first = &return_types[0];
    if return_types.iter().all(|t| t == first) {
        return Some(first.clone());
    }
    // Object subsumes HashRef: if some returns are Object(X) and others are HashRef,
    // the Object wins (overloaded hash access is common in Perl).
    let mut object = None;
    for t in return_types {
        if t.is_object() {
            object = Some(t.clone());
        } else if *t != InferredType::HashRef {
            // Non-HashRef, non-Object disagreement → Unknown
            return None;
        }
    }
    object
}

// ---- Hash key owner (for scope graph) ----

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKeyOwner {
    Class(String),
    Variable { name: String, def_scope: ScopeId },
    /// Hash keys from a sub's return value: `sub get_config { return { host => 1 } }`
    Sub(String),
}

// ---- Call binding ----

/// A variable assigned from a function call: `my $cfg = get_config()`.
/// Stored in FileAnalysis so query-time resolution can follow the chain.
#[derive(Debug, Clone)]
pub struct CallBinding {
    pub variable: String,
    pub func_name: String,
    pub scope: ScopeId,
    pub span: Span,
}

// ---- Import ----

/// A `use Foo::Bar qw(func1 func2)` statement parsed from the source.
#[derive(Debug, Clone)]
pub struct Import {
    /// Module name, e.g. "List::Util".
    pub module_name: String,
    /// Explicitly imported symbols from `qw(...)`. Empty = bare `use Foo;`.
    pub imported_symbols: Vec<String>,
    /// Span of the entire `use` statement.
    pub span: Span,
    /// Position of the closing delimiter of the `qw()` list (the `)` character).
    /// Used to insert new imports into an existing qw list.
    pub qw_close_paren: Option<Point>,
}

// ---- Outline ----

pub struct OutlineSymbol {
    pub name: String,
    pub detail: Option<String>,
    pub kind: SymKind,
    pub span: Span,
    pub selection_span: Span,
    pub children: Vec<OutlineSymbol>,
}

// ---- Semantic tokens ----

#[derive(Debug, Clone)]
pub struct SemanticVarToken {
    pub span: Span,
    pub var_type: VarModifier,
    pub is_declaration: bool,
    pub is_modification: bool,
    pub is_readonly: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarModifier {
    Scalar,
    Array,
    Hash,
}

fn sigil_to_var_modifier(sigil: char) -> VarModifier {
    match sigil {
        '@' => VarModifier::Array,
        '%' => VarModifier::Hash,
        _ => VarModifier::Scalar,
    }
}

// ---- FileAnalysis ----

pub struct FileAnalysis {
    // Core tables
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub refs: Vec<Ref>,
    pub type_constraints: Vec<TypeConstraint>,
    pub fold_ranges: Vec<FoldRange>,
    pub imports: Vec<Import>,
    pub call_bindings: Vec<CallBinding>,

    // Indices (built in post-pass)
    scope_starts: Vec<(Point, ScopeId)>, // sorted by start point
    symbols_by_name: HashMap<String, Vec<SymbolId>>,
    symbols_by_scope: HashMap<ScopeId, Vec<SymbolId>>,
    refs_by_name: HashMap<String, Vec<usize>>,
    type_constraints_by_var: HashMap<String, Vec<usize>>,
}

impl FileAnalysis {
    /// Create a new FileAnalysis with indices built from the raw tables.
    pub fn new(
        scopes: Vec<Scope>,
        symbols: Vec<Symbol>,
        refs: Vec<Ref>,
        type_constraints: Vec<TypeConstraint>,
        fold_ranges: Vec<FoldRange>,
        imports: Vec<Import>,
        call_bindings: Vec<CallBinding>,
    ) -> Self {
        let mut fa = FileAnalysis {
            scopes,
            symbols,
            refs,
            type_constraints,
            fold_ranges,
            imports,
            call_bindings,
            scope_starts: Vec::new(),
            symbols_by_name: HashMap::new(),
            symbols_by_scope: HashMap::new(),
            refs_by_name: HashMap::new(),
            type_constraints_by_var: HashMap::new(),
        };
        fa.build_indices();
        fa
    }

    fn build_indices(&mut self) {
        // Scope starts — sorted for binary search
        self.scope_starts = self.scopes.iter()
            .map(|s| (s.span.start, s.id))
            .collect();
        self.scope_starts.sort_by_key(|(p, _)| (p.row, p.column));

        // Symbols by name
        for sym in &self.symbols {
            self.symbols_by_name
                .entry(sym.name.clone())
                .or_default()
                .push(sym.id);
        }

        // Symbols by scope
        for sym in &self.symbols {
            self.symbols_by_scope
                .entry(sym.scope)
                .or_default()
                .push(sym.id);
        }

        // Refs by target name
        for (i, r) in self.refs.iter().enumerate() {
            self.refs_by_name
                .entry(r.target_name.clone())
                .or_default()
                .push(i);
        }

        // Type constraints by variable name
        for (i, tc) in self.type_constraints.iter().enumerate() {
            self.type_constraints_by_var
                .entry(tc.variable.clone())
                .or_default()
                .push(i);
        }
    }

    // ---- Query methods ----

    /// Find the innermost scope containing a point.
    pub fn scope_at(&self, point: Point) -> Option<ScopeId> {
        let mut best: Option<(ScopeId, usize)> = None; // (id, span_size)
        for scope in &self.scopes {
            if contains_point(&scope.span, point) {
                let size = span_size(&scope.span);
                if best.is_none() || size <= best.unwrap().1 {
                    best = Some((scope.id, size));
                }
            }
        }
        best.map(|(id, _)| id)
    }

    /// Walk the scope chain from a scope upward to file root.
    pub fn scope_chain(&self, start: ScopeId) -> Vec<ScopeId> {
        let mut chain = Vec::new();
        let mut current = Some(start);
        while let Some(id) = current {
            chain.push(id);
            current = self.scopes[id.0 as usize].parent;
        }
        chain
    }

    /// Get the scope struct by ID.
    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0 as usize]
    }

    /// Get the symbol struct by ID.
    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0 as usize]
    }

    /// Find all symbols visible at a point (walks scope chain).
    pub fn visible_symbols(&self, point: Point) -> Vec<&Symbol> {
        let scope = match self.scope_at(point) {
            Some(s) => s,
            None => return Vec::new(),
        };
        let chain = self.scope_chain(scope);
        let mut result = Vec::new();
        for scope_id in &chain {
            if let Some(sym_ids) = self.symbols_by_scope.get(scope_id) {
                for sid in sym_ids {
                    let sym = &self.symbols[sid.0 as usize];
                    // Symbol must be declared before the point (or be a sub/package/class)
                    if sym.span.start <= point || matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class) {
                        result.push(sym);
                    }
                }
            }
        }
        result
    }

    /// Resolve a variable name to its declaration at a given point.
    /// Returns the innermost-scope match (Perl lexical scoping).
    pub fn resolve_variable(&self, name: &str, point: Point) -> Option<&Symbol> {
        let scope = self.scope_at(point)?;
        let chain = self.scope_chain(scope);
        for scope_id in &chain {
            if let Some(sym_ids) = self.symbols_by_scope.get(scope_id) {
                for sid in sym_ids {
                    let sym = &self.symbols[sid.0 as usize];
                    if sym.name == name
                        && matches!(sym.kind, SymKind::Variable | SymKind::Field)
                        && sym.span.start <= point
                    {
                        return Some(sym);
                    }
                }
            }
        }
        None
    }

    /// Get the inferred type of a variable at a point.
    pub fn inferred_type(&self, var_name: &str, point: Point) -> Option<&InferredType> {
        let constraints = self.type_constraints_by_var.get(var_name)?;
        // Find the best constraint: latest one before point, in a scope containing point
        let mut best: Option<&TypeConstraint> = None;
        for &idx in constraints {
            let tc = &self.type_constraints[idx];
            let scope = &self.scopes[tc.scope.0 as usize];
            if contains_point(&scope.span, point) && tc.constraint_span.start <= point {
                if best.is_none() || tc.constraint_span.start > best.unwrap().constraint_span.start {
                    best = Some(tc);
                }
            }
        }
        best.map(|tc| &tc.inferred_type)
    }

    /// Get the return type of a named sub/method.
    pub fn sub_return_type(&self, name: &str) -> Option<&InferredType> {
        for sym in &self.symbols {
            if sym.name == name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
                    return return_type.as_ref();
                }
            }
        }
        None
    }

    /// Resolve the type of an arbitrary CST expression node.
    ///
    /// Recursive tree walk that composes existing atoms:
    /// - `scalar` → `inferred_type(var_text, point)`
    /// - bareword (class name) → `Object(ClassName)`
    /// - `method_call_expression` → resolve invocant → find method → return type
    /// - `function_call_expression` → `sub_return_type(name)`
    /// - `hash_element_expression` → resolve base (for chaining)
    ///
    /// Used at query time (completion, goto-def, hover) — not during the build walk.
    pub fn resolve_expression_type(&self, node: tree_sitter::Node, source: &[u8]) -> Option<InferredType> {
        let point = node.start_position();
        match node.kind() {
            "scalar" => {
                let text = node.utf8_text(source).ok()?;
                self.inferred_type(text, point).cloned()
            }
            "package" | "bareword" => {
                // Bareword = class name
                let text = node.utf8_text(source).ok()?;
                Some(InferredType::ClassName(text.to_string()))
            }
            "method_call_expression" => {
                let invocant_node = node.child_by_field_name("invocant")?;
                let invocant_type = self.resolve_expression_type(invocant_node, source)?;
                let class_name = invocant_type.class_name()?;
                let method_name = node.child_by_field_name("method")?;
                let method_text = method_name.utf8_text(source).ok()?;
                // Constructor call returns Object(ClassName)
                if method_text == "new" {
                    return Some(InferredType::ClassName(class_name.to_string()));
                }
                // Find method's return type
                self.find_method_return_type(class_name, method_text)
            }
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let func_node = node.child_by_field_name("function")?;
                let name = func_node.utf8_text(source).ok()?;
                self.sub_return_type(name).cloned()
            }
            "hash_element_expression" => {
                // For chaining: resolve the base expression
                let base = node.named_child(0)?;
                self.resolve_expression_type(base, source)
            }
            _ => None,
        }
    }

    /// Resolve the hash key owner from a tree node at a given point.
    ///
    /// Finds the enclosing `hash_element_expression`, resolves its base expression
    /// type, and returns the appropriate `HashKeyOwner`. Used by goto-def,
    /// references, completion, and highlights for chained hash access like
    /// `$calc->get_self->get_config->{host}`.
    pub fn resolve_hash_owner_from_tree(
        &self,
        tree: &tree_sitter::Tree,
        source: &[u8],
        point: Point,
    ) -> Option<HashKeyOwner> {
        let hash_elem = tree.root_node()
            .descendant_for_point_range(point, point)
            .and_then(|n| find_ancestor(n, "hash_element_expression"))?;
        let base = hash_elem.named_child(0)?;
        let ty = self.resolve_expression_type(base, source)?;

        // Extract the function/method name for Sub owner
        let sub_name = match base.kind() {
            "method_call_expression" => base.child_by_field_name("method")
                .and_then(|n| n.utf8_text(source).ok())
                .map(|s| s.to_string()),
            "function_call_expression" | "ambiguous_function_call_expression" =>
                base.child_by_field_name("function")
                    .and_then(|n| n.utf8_text(source).ok())
                    .map(|s| s.to_string()),
            _ => None,
        };

        if let Some(name) = sub_name {
            // Check Sub owner has defs; fall through to class if not
            let sub_owner = HashKeyOwner::Sub(name);
            if !self.hash_key_defs_for_owner(&sub_owner).is_empty() {
                return Some(sub_owner);
            }
        }

        if let Some(cn) = ty.class_name() {
            return Some(HashKeyOwner::Class(cn.to_string()));
        }

        None
    }

    /// Check if cursor is on a bareword or string inside call arguments.
    /// Returns the text if so (could be a named arg key).
    fn call_arg_key_at(&self, tree: &tree_sitter::Tree, source: &[u8], point: Point) -> Option<String> {
        let node = tree.root_node().descendant_for_point_range(point, point)?;
        let key_node = match node.kind() {
            "autoquoted_bareword" => node,
            "string_literal" | "interpolated_string_literal" => node,
            _ => {
                let parent = node.parent()?;
                if matches!(parent.kind(), "string_literal" | "interpolated_string_literal") {
                    parent
                } else {
                    return None;
                }
            }
        };
        let text = key_node.utf8_text(source).ok()?;
        if (text.starts_with('\'') || text.starts_with('"')) && text.len() >= 2 {
            Some(text[1..text.len()-1].to_string())
        } else if text.starts_with('\'') || text.starts_with('"') {
            None
        } else {
            Some(text.to_string())
        }
    }

    /// Find a method's return type within a class/package.
    fn find_method_return_type(&self, class_name: &str, method_name: &str) -> Option<InferredType> {
        for sym in &self.symbols {
            if sym.name == method_name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sym.id, class_name) {
                    if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
                        return return_type.clone();
                    }
                }
            }
        }
        None
    }

    /// Find a `:param` field in a class by its bare name (without sigil).
    fn find_param_field(&self, class_name: &str, key: &str) -> Option<Span> {
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Field) {
                if let SymbolDetail::Field { ref attributes, .. } = sym.detail {
                    if attributes.contains(&"param".to_string())
                        && self.symbol_in_class(sym.id, class_name)
                    {
                        let bare = sym.name
                            .trim_start_matches('$')
                            .trim_start_matches('@')
                            .trim_start_matches('%');
                        if bare == key {
                            return Some(sym.selection_span);
                        }
                    }
                }
            }
        }
        None
    }

    /// Complete methods for a known class name (no invocant resolution needed).
    pub fn complete_methods_for_class(&self, class_name: &str) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();

        // Check for class definition → implicit new
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Class) && sym.name == class_name {
                candidates.push(CompletionCandidate {
                    label: "new".to_string(),
                    kind: SymKind::Method,
                    detail: Some(format!("{}->new", class_name)),
                    insert_text: None,
                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                });
                break;
            }
        }

        // Find all subs/methods in this class/package
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sym.id, class_name) {
                    candidates.push(CompletionCandidate {
                        label: sym.name.clone(),
                        kind: sym.kind,
                        detail: Some(class_name.to_string()),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                    });
                }
            }
        }

        candidates
    }

    /// Get the enclosing package name at a point.
    #[allow(dead_code)]
    pub fn package_at(&self, point: Point) -> Option<&str> {
        let scope = self.scope_at(point)?;
        let chain = self.scope_chain(scope);
        for scope_id in &chain {
            let s = &self.scopes[scope_id.0 as usize];
            if let Some(ref pkg) = s.package {
                return Some(pkg.as_str());
            }
        }
        None
    }

    /// Find all symbols with a given name.
    pub fn symbols_named(&self, name: &str) -> &[SymbolId] {
        self.symbols_by_name.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Find all symbols in a given scope.
    #[allow(dead_code)]
    pub fn symbols_in_scope(&self, scope: ScopeId) -> &[SymbolId] {
        self.symbols_by_scope.get(&scope).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Find all refs with a given target name.
    #[allow(dead_code)]
    pub fn refs_named(&self, name: &str) -> Vec<&Ref> {
        self.refs_by_name.get(name)
            .map(|idxs| idxs.iter().map(|&i| &self.refs[i]).collect())
            .unwrap_or_default()
    }

    /// Find all refs that resolve to a specific symbol.
    #[allow(dead_code)]
    pub fn refs_to(&self, target: SymbolId) -> Vec<&Ref> {
        self.refs.iter()
            .filter(|r| r.resolves_to == Some(target))
            .collect()
    }

    /// Find all hash key accesses/definitions for a given owner.
    #[allow(dead_code)]
    pub fn hash_keys_for_owner(&self, owner: &HashKeyOwner) -> Vec<&Ref> {
        self.refs.iter()
            .filter(|r| {
                if let RefKind::HashKeyAccess { owner: Some(ref o), .. } = r.kind {
                    o == owner
                } else {
                    false
                }
            })
            .collect()
    }

    /// Find all hash key definition symbols for a given owner.
    pub fn hash_key_defs_for_owner(&self, owner: &HashKeyOwner) -> Vec<&Symbol> {
        self.symbols.iter()
            .filter(|s| {
                if let SymbolDetail::HashKeyDef { owner: ref o, .. } = s.detail {
                    o == owner
                } else {
                    false
                }
            })
            .collect()
    }

    // ---- Cursor lookup methods ----

    /// Find the ref at a given point (cursor position).
    pub fn ref_at(&self, point: Point) -> Option<&Ref> {
        self.refs.iter().find(|r| contains_point(&r.span, point))
    }

    /// Find the symbol whose selection_span contains the point.
    pub fn symbol_at(&self, point: Point) -> Option<&Symbol> {
        self.symbols.iter().find(|s| contains_point(&s.selection_span, point))
    }

    // ---- High-level queries ----

    /// Go-to-definition: resolve the symbol at cursor to its definition span.
    pub fn find_definition(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Option<Span> {
        // 1. Check if cursor is on a ref
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable => {
                    if let Some(sym_id) = r.resolves_to {
                        return Some(self.symbol(sym_id).selection_span);
                    }
                }
                RefKind::FunctionCall => {
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Sub) {
                            return Some(sym.selection_span);
                        }
                    }
                }
                RefKind::MethodCall { ref invocant, ref invocant_span, .. } => {
                    let class_name = self.resolve_method_invocant(invocant, invocant_span, r.scope, point, tree, source_bytes);

                    // Check if cursor is on a named arg key in the call args,
                    // not on the method name itself. Resolve to hash key def or :param field.
                    if let (Some(t), Some(s), Some(ref cn)) = (tree, source_bytes, &class_name) {
                        if let Some(key_name) = self.call_arg_key_at(t, s, point) {
                            // Try hash key defs (bless hash)
                            let owner = HashKeyOwner::Class(cn.to_string());
                            for def in self.hash_key_defs_for_owner(&owner) {
                                if def.name == key_name {
                                    return Some(def.selection_span);
                                }
                            }
                            // Try :param fields (Perl 5.38+ classes)
                            if let Some(span) = self.find_param_field(cn, &key_name) {
                                return Some(span);
                            }
                        }
                    }

                    if let Some(ref cn) = class_name {
                        // Find method in the resolved class
                        if let Some(span) = self.find_method_in_class(cn, &r.target_name) {
                            return Some(span);
                        }
                        // Method not found (e.g. auto-generated "new") → go to class def
                        if let Some(span) = self.find_package_or_class(cn) {
                            return Some(span);
                        }
                    }
                    // Fall back to any sub/method with that name
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                            return Some(sym.selection_span);
                        }
                    }
                }
                RefKind::PackageRef => {
                    return self.find_package_or_class(&r.target_name);
                }
                RefKind::HashKeyAccess { ref owner, .. } => {
                    // Try the pre-resolved owner first
                    let resolved_owner = owner.clone().or_else(|| {
                        let tree = tree?;
                        let source = source_bytes?;
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start)
                    });
                    if let Some(ref owner) = resolved_owner {
                        for def in self.hash_key_defs_for_owner(owner) {
                            if def.name == r.target_name {
                                return Some(def.selection_span);
                            }
                        }
                    }
                }
                RefKind::ContainerAccess => {
                    return self.resolve_variable(&r.target_name, point)
                        .map(|sym| sym.selection_span);
                }
            }
        }

        // 2. Check if cursor is on a symbol declaration
        if let Some(sym) = self.symbol_at(point) {
            return Some(sym.selection_span);
        }

        None
    }

    /// Find all references to the symbol at cursor.
    pub fn find_references(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Vec<Span> {
        if let Some((target_id, include_decl)) = self.resolve_target_at(point, tree, source_bytes) {
            let sym = self.symbol(target_id);
            let mut spans: Vec<Span> = Vec::new();

            // Include the declaration itself
            if include_decl {
                spans.push(sym.selection_span);
            }

            // Collect all refs that resolve to this symbol
            for r in &self.refs {
                if r.resolves_to == Some(target_id) {
                    spans.push(r.span);
                }
            }

            // For subs/methods/packages/classes, also find refs by name
            if matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class | SymKind::Module) {
                for r in &self.refs {
                    if r.target_name == sym.name && r.resolves_to.is_none() {
                        match (&r.kind, &sym.kind) {
                            (RefKind::FunctionCall, SymKind::Sub) => spans.push(r.span),
                            (RefKind::MethodCall { .. }, SymKind::Sub | SymKind::Method) => spans.push(r.span),
                            (RefKind::PackageRef, SymKind::Package | SymKind::Class | SymKind::Module) => spans.push(r.span),
                            _ => {}
                        }
                    }
                }
            }

            // For hash key definitions, find all accesses with same owner + key name
            if let SymbolDetail::HashKeyDef { ref owner, .. } = sym.detail {
                for r in &self.refs {
                    if let RefKind::HashKeyAccess { owner: ref ro, .. } = r.kind {
                        if r.target_name != sym.name {
                            continue;
                        }
                        let matches = match ro {
                            Some(ref ro) => ro == owner,
                            None => {
                                // Try tree-based resolution for chained expressions
                                if let (Some(t), Some(s)) = (tree, source_bytes) {
                                    self.resolve_hash_owner_from_tree(t, s, r.span.start)
                                        .as_ref()
                                        .map_or(false, |resolved| resolved == owner)
                                } else {
                                    false
                                }
                            }
                        };
                        if matches {
                            spans.push(r.span);
                        }
                    }
                }
            }

            spans.sort_by_key(|s| (s.start.row, s.start.column));
            spans.dedup_by(|a, b| a.start == b.start && a.end == b.end);
            spans
        } else {
            Vec::new()
        }
    }

    /// Document highlights: like references but with read/write annotation.
    pub fn find_highlights(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Vec<(Span, AccessKind)> {
        if let Some((target_id, _)) = self.resolve_target_at(point, tree, source_bytes) {
            let sym = self.symbol(target_id);
            let mut highlights: Vec<(Span, AccessKind)> = Vec::new();

            // Declaration
            highlights.push((sym.selection_span, AccessKind::Declaration));

            // All refs
            for r in &self.refs {
                if r.resolves_to == Some(target_id) {
                    highlights.push((r.span, r.access));
                }
            }

            // Name-matched refs for subs/packages
            if matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class | SymKind::Module) {
                for r in &self.refs {
                    if r.target_name == sym.name && r.resolves_to.is_none() {
                        match (&r.kind, &sym.kind) {
                            (RefKind::FunctionCall, SymKind::Sub) => highlights.push((r.span, r.access)),
                            (RefKind::MethodCall { .. }, SymKind::Sub | SymKind::Method) => highlights.push((r.span, r.access)),
                            (RefKind::PackageRef, SymKind::Package | SymKind::Class | SymKind::Module) => highlights.push((r.span, r.access)),
                            _ => {}
                        }
                    }
                }
            }

            // Hash key refs
            if let SymbolDetail::HashKeyDef { ref owner, .. } = sym.detail {
                for r in &self.refs {
                    if let RefKind::HashKeyAccess { owner: ref ro, .. } = r.kind {
                        if r.target_name != sym.name {
                            continue;
                        }
                        let matches = match ro {
                            Some(ref ro) => ro == owner,
                            None => {
                                if let (Some(t), Some(s)) = (tree, source_bytes) {
                                    self.resolve_hash_owner_from_tree(t, s, r.span.start)
                                        .as_ref()
                                        .map_or(false, |resolved| resolved == owner)
                                } else {
                                    false
                                }
                            }
                        };
                        if matches {
                            highlights.push((r.span, r.access));
                        }
                    }
                }
            }

            highlights.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            highlights.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            highlights
        } else {
            Vec::new()
        }
    }

    /// Hover info: return display text for the symbol at cursor.
    pub fn hover_info(&self, point: Point, source: &str, tree: Option<&tree_sitter::Tree>) -> Option<String> {
        // Check refs first
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    if let Some(sym_id) = r.resolves_to {
                        let sym = self.symbol(sym_id);
                        return Some(self.format_symbol_hover(sym, source));
                    }
                    // Unresolved variable — try resolve ourselves
                    if let Some(sym) = self.resolve_variable(&r.target_name, point) {
                        return Some(self.format_symbol_hover(sym, source));
                    }
                }
                RefKind::FunctionCall => {
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Sub) {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                }
                RefKind::MethodCall { ref invocant, ref invocant_span, .. } => {
                    let class_name = self.resolve_method_invocant(
                        invocant, invocant_span, r.scope, point, tree, Some(source.as_bytes()),
                    );
                    if let Some(ref cn) = class_name {
                        if let Some(span) = self.find_method_in_class(cn, &r.target_name) {
                            let line = source_line_at(source, span.start.row);
                            return Some(format!("```perl\n{}\n```\n\n*class {}*", line.trim(), cn));
                        }
                    }
                    // Fallback
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                }
                RefKind::PackageRef => {
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                }
                RefKind::HashKeyAccess { owner, .. } => {
                    if let Some(ref owner) = owner {
                        let defs = self.hash_key_defs_for_owner(owner);
                        let matching: Vec<_> = defs.iter()
                            .filter(|d| d.name == r.target_name)
                            .collect();
                        if !matching.is_empty() {
                            let lines: Vec<String> = matching.iter()
                                .map(|d| {
                                    let line = source_line_at(source, d.span.start.row);
                                    format!("- `{}`", line.trim())
                                })
                                .collect();
                            return Some(format!("**Hash key `{}`**\n\n{}", r.target_name, lines.join("\n")));
                        }
                    }
                }
            }
        }

        // Check symbols
        if let Some(sym) = self.symbol_at(point) {
            return Some(self.format_symbol_hover(sym, source));
        }

        None
    }

    /// Rename: return all spans + new text for renaming the symbol at cursor.
    pub fn rename_at(&self, point: Point, new_name: &str) -> Option<Vec<(Span, String)>> {
        let refs = self.find_references(point, None, None);
        if refs.is_empty() {
            return None;
        }

        // Determine if this is a variable (sigil handling needed)
        let is_variable = self.ref_at(point)
            .map(|r| matches!(r.kind, RefKind::Variable | RefKind::ContainerAccess))
            .or_else(|| self.symbol_at(point).map(|s| matches!(s.kind, SymKind::Variable | SymKind::Field)))
            .unwrap_or(false);

        let edits: Vec<(Span, String)> = if is_variable {
            // Strip sigil from new_name if present
            let bare_name = if new_name.starts_with('$') || new_name.starts_with('@') || new_name.starts_with('%') {
                &new_name[1..]
            } else {
                new_name
            };
            refs.into_iter().map(|span| {
                // Each ref span includes the sigil; replace only the name part (after sigil)
                let name_span = Span {
                    start: Point::new(span.start.row, span.start.column + 1),
                    end: span.end,
                };
                (name_span, bare_name.to_string())
            }).collect()
        } else {
            refs.into_iter().map(|span| (span, new_name.to_string())).collect()
        };

        Some(edits)
    }

    // ---- Internal resolution helpers ----

    /// Find the target symbol for the thing at cursor. Returns (SymbolId, include_decl_in_refs).
    fn resolve_target_at(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Option<(SymbolId, bool)> {
        // Check refs first
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    if let Some(sym_id) = r.resolves_to {
                        return Some((sym_id, true));
                    }
                    // Try resolving manually
                    if let Some(sym) = self.resolve_variable(&r.target_name, point) {
                        return Some((sym.id, true));
                    }
                }
                RefKind::FunctionCall => {
                    for &sid in self.symbols_named(&r.target_name) {
                        if matches!(self.symbol(sid).kind, SymKind::Sub) {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::MethodCall { ref invocant, .. } => {
                    let class_name = self.resolve_invocant_class(invocant, r.scope, point);
                    // Try class-specific method first
                    if let Some(ref cn) = class_name {
                        for &sid in self.symbols_named(&r.target_name) {
                            let sym = self.symbol(sid);
                            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                                if self.symbol_in_class(sid, cn) {
                                    return Some((sid, true));
                                }
                            }
                        }
                    }
                    // Fallback: any method with that name
                    for &sid in self.symbols_named(&r.target_name) {
                        if matches!(self.symbol(sid).kind, SymKind::Sub | SymKind::Method) {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::PackageRef => {
                    for &sid in self.symbols_named(&r.target_name) {
                        if matches!(self.symbol(sid).kind, SymKind::Package | SymKind::Class | SymKind::Module) {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::HashKeyAccess { ref owner, .. } => {
                    let resolved_owner = owner.clone().or_else(|| {
                        let tree = tree?;
                        let source = source_bytes?;
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start)
                    });
                    if let Some(ref owner) = resolved_owner {
                        for def in self.hash_key_defs_for_owner(owner) {
                            if def.name == r.target_name {
                                return Some((def.id, true));
                            }
                        }
                    }
                }
            }
        }

        // Check if cursor is directly on a symbol declaration
        if let Some(sym) = self.symbol_at(point) {
            return Some((sym.id, false));
        }

        None
    }

    /// Resolve a method call's invocant to a class name, using tree-based resolution
    /// for complex expressions when available.
    fn resolve_method_invocant(
        &self,
        invocant: &str,
        invocant_span: &Option<Span>,
        scope: ScopeId,
        point: Point,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
    ) -> Option<String> {
        // Try tree-based resolution for complex invocants, fall through on failure
        if let (Some(span), Some(tree), Some(src)) = (invocant_span, tree, source_bytes) {
            if let Some(node) = tree.root_node()
                .descendant_for_point_range(span.start, span.end)
            {
                if let Some(ty) = self.resolve_expression_type(node, src) {
                    if let Some(cn) = ty.class_name() {
                        return Some(cn.to_string());
                    }
                }
            }
        }
        // Fall back to string-based resolution
        self.resolve_invocant_class(invocant, scope, point)
    }

    /// Resolve an invocant string to a class name.
    fn resolve_invocant_class(&self, invocant: &str, scope: ScopeId, point: Point) -> Option<String> {
        if invocant.starts_with('$') || invocant.starts_with('@') || invocant.starts_with('%') {
            // Variable invocant → infer type
            self.inferred_type(invocant, point)
                .and_then(|t| t.class_name().map(|s| s.to_string()))
                .or_else(|| {
                    // Check if we're inside a class and $self is the invocant
                    let chain = self.scope_chain(scope);
                    for scope_id in &chain {
                        let s = self.scope(*scope_id);
                        if let ScopeKind::Class { ref name } = s.kind {
                            return Some(name.clone());
                        }
                        if let Some(ref pkg) = s.package {
                            return Some(pkg.clone());
                        }
                    }
                    None
                })
        } else {
            // Bareword invocant = class name directly
            Some(invocant.to_string())
        }
    }

    /// Find a method definition within a class/package.
    fn find_method_in_class(&self, class_name: &str, method_name: &str) -> Option<Span> {
        for &sid in self.symbols_named(method_name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sid, class_name) {
                    return Some(sym.selection_span);
                }
            }
        }
        None
    }

    /// Check if a symbol is defined within a class/package.
    fn symbol_in_class(&self, sym_id: SymbolId, class_name: &str) -> bool {
        let sym = self.symbol(sym_id);
        // Use scope_at to find the innermost scope containing this symbol.
        // For subs, this finds the Sub scope (which has the correct package
        // from when it was defined), rather than the enclosing scope the
        // symbol was recorded in.
        let start_scope = self.scope_at(sym.span.start).unwrap_or(sym.scope);
        let chain = self.scope_chain(start_scope);
        for scope_id in &chain {
            let s = self.scope(*scope_id);
            if let ScopeKind::Class { ref name } = s.kind {
                return name == class_name;
            }
            if let Some(ref pkg) = s.package {
                return pkg == class_name;
            }
        }
        false
    }

    /// Find the definition span of a package or class by name.
    fn find_package_or_class(&self, name: &str) -> Option<Span> {
        for &sid in self.symbols_named(name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                return Some(sym.selection_span);
            }
        }
        None
    }

    fn format_symbol_hover(&self, sym: &Symbol, source: &str) -> String {
        let line = source_line_at(source, sym.span.start.row);
        format!("```perl\n{}\n```", line.trim())
    }

    // ---- Document outline ----

    /// Build document outline as a nested symbol tree.
    /// Returns (name, detail, kind, span, selection_span, children) tuples.
    pub fn document_symbols(&self) -> Vec<OutlineSymbol> {
        // Find the file scope (ScopeId(0))
        let file_scope = ScopeId(0);
        self.outline_children_of(file_scope)
    }

    fn outline_children_of(&self, parent_scope: ScopeId) -> Vec<OutlineSymbol> {
        let mut result = Vec::new();

        for sym in &self.symbols {
            if sym.scope != parent_scope {
                continue;
            }

            let (name, detail, children) = match sym.kind {
                SymKind::Sub => {
                    let body_scope = self.find_body_scope(sym);
                    let children = body_scope
                        .map(|s| self.outline_children_of(s))
                        .unwrap_or_default();
                    (format!("sub {}", sym.name), None, children)
                }
                SymKind::Method => {
                    let body_scope = self.find_body_scope(sym);
                    let children = body_scope
                        .map(|s| self.outline_children_of(s))
                        .unwrap_or_default();
                    (format!("method {}", sym.name), None, children)
                }
                SymKind::Class => {
                    let body_scope = self.find_body_scope(sym);
                    let children = body_scope
                        .map(|s| self.outline_children_of(s))
                        .unwrap_or_default();
                    (sym.name.clone(), Some("class".to_string()), children)
                }
                SymKind::Package => {
                    (sym.name.clone(), Some("package".to_string()), Vec::new())
                }
                SymKind::Module => {
                    (format!("use {}", sym.name), None, Vec::new())
                }
                SymKind::Variable => {
                    let detail = match &sym.detail {
                        SymbolDetail::Variable { decl_kind, .. } => match decl_kind {
                            DeclKind::My => "my",
                            DeclKind::Our => "our",
                            DeclKind::State => "state",
                            DeclKind::Field => "field",
                            DeclKind::Param => "param",
                            DeclKind::ForVar => "for",
                        },
                        _ => "my",
                    };
                    (sym.name.clone(), Some(detail.to_string()), Vec::new())
                }
                SymKind::Field => {
                    (sym.name.clone(), Some("field".to_string()), Vec::new())
                }
                SymKind::HashKeyDef => continue, // Skip hash key defs from outline
            };

            result.push(OutlineSymbol {
                name,
                detail,
                kind: sym.kind,
                span: sym.span,
                selection_span: sym.selection_span,
                children,
            });
        }

        result
    }

    /// Find the body scope for a sub/method/class symbol.
    fn find_body_scope(&self, sym: &Symbol) -> Option<ScopeId> {
        self.scopes.iter().find(|s| {
            let kind_matches = match (&s.kind, &sym.kind) {
                (ScopeKind::Sub { name: sn }, SymKind::Sub) => sn == &sym.name,
                (ScopeKind::Method { name: mn }, SymKind::Method) => mn == &sym.name,
                (ScopeKind::Class { name: cn }, SymKind::Class) => cn == &sym.name,
                _ => false,
            };
            kind_matches && s.span == sym.span
        }).map(|s| s.id)
    }

    // ---- Semantic tokens ----

    /// Collect semantic tokens for all variable references and declarations.
    pub fn semantic_tokens(&self) -> Vec<SemanticVarToken> {
        let mut tokens: Vec<SemanticVarToken> = Vec::new();

        // Variable declarations (from symbols)
        for sym in &self.symbols {
            if !matches!(sym.kind, SymKind::Variable | SymKind::Field) {
                continue;
            }
            let (sigil, is_readonly) = match &sym.detail {
                SymbolDetail::Variable { sigil, decl_kind } => {
                    let readonly = matches!(decl_kind, DeclKind::Field);
                    (*sigil, readonly)
                }
                SymbolDetail::Field { sigil, attributes } => {
                    let readonly = !attributes.iter().any(|a| a == "writer" || a == "mutator" || a == "accessor");
                    (*sigil, readonly)
                }
                _ => continue,
            };
            tokens.push(SemanticVarToken {
                span: sym.selection_span,
                var_type: sigil_to_var_modifier(sigil),
                is_declaration: true,
                is_modification: false,
                is_readonly,
            });
        }

        // Variable references (from refs)
        for r in &self.refs {
            let sigil = match &r.kind {
                RefKind::Variable => r.target_name.chars().next().unwrap_or('$'),
                RefKind::ContainerAccess => {
                    // Container access sigil is the displayed sigil, not the underlying
                    r.target_name.chars().next().unwrap_or('$')
                }
                _ => continue,
            };
            tokens.push(SemanticVarToken {
                span: r.span,
                var_type: sigil_to_var_modifier(sigil),
                is_declaration: false,
                is_modification: matches!(r.access, AccessKind::Write),
                is_readonly: false,
            });
        }

        tokens.sort_by_key(|t| (t.span.start.row, t.span.start.column));
        tokens
    }
}

// ---- Completion priority constants ----
//
// Lower numbers sort first. Used by both file_analysis (local completions)
// and symbols.rs (cross-file completions).

/// Variables, local subs, methods — direct scope match.
pub const PRIORITY_LOCAL: u8 = 0;
/// General subs, hash keys, keyval args — file-wide match.
pub const PRIORITY_FILE_WIDE: u8 = 10;
/// Explicitly imported via `use Foo qw(bar)`.
pub const PRIORITY_EXPLICIT_IMPORT: u8 = 12;
/// Bare `use Foo;` @EXPORT symbol (no qw list to edit).
pub const PRIORITY_BARE_IMPORT: u8 = 15;
/// Auto-add to existing `qw()` list.
pub const PRIORITY_AUTO_ADD_QW: u8 = 18;
/// Sub already used as first param (less relevant).
pub const PRIORITY_LESS_RELEVANT: u8 = 20;
/// Unimported module — inserts full `use` statement.
pub const PRIORITY_UNIMPORTED: u8 = 25;
/// Dynamic hash keys (may not exist).
pub const PRIORITY_DYNAMIC: u8 = 50;

// ---- Completion types ----

/// A completion candidate from FileAnalysis resolution (pure table lookup).
#[derive(Debug, Clone)]
pub struct CompletionCandidate {
    pub label: String,
    pub kind: SymKind,
    pub detail: Option<String>,
    pub insert_text: Option<String>,
    pub sort_priority: u8,
    /// Additional text edits applied when this candidate is accepted (e.g. auto-import).
    pub additional_edits: Vec<(Span, String)>,
}

/// Signature info for a sub/method, resolved from the symbol table.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct SignatureInfo {
    pub name: String,
    pub params: Vec<ParamInfo>,
    pub is_method: bool,
}

// ---- Completion query methods ----

impl FileAnalysis {
    /// Complete variables at a point with cross-sigil forms.
    pub fn complete_variables(&self, point: Point, sigil: char) -> Vec<CompletionCandidate> {
        let visible = self.visible_symbols(point);
        let mut seen = HashSet::<(String, char)>::new();
        let mut candidates = Vec::new();

        // Sort by scope size (innermost first) — stable priority ordering
        let mut vars: Vec<(&Symbol, usize)> = visible
            .into_iter()
            .filter(|s| matches!(s.kind, SymKind::Variable | SymKind::Field))
            .filter_map(|s| {
                if let SymbolDetail::Variable { .. } = &s.detail {
                    let scope = &self.scopes[s.scope.0 as usize];
                    let scope_size = span_size(&scope.span);
                    Some((s, scope_size))
                } else if let SymbolDetail::Field { .. } = &s.detail {
                    let scope = &self.scopes[s.scope.0 as usize];
                    let scope_size = span_size(&scope.span);
                    Some((s, scope_size))
                } else {
                    None
                }
            })
            .collect();
        vars.sort_by_key(|(_, sz)| *sz);

        for (sym, scope_size) in vars {
            let (bare_name, decl_sigil) = match &sym.detail {
                SymbolDetail::Variable { sigil: ds, .. } => {
                    (sym.name[1..].to_string(), *ds)
                }
                SymbolDetail::Field { sigil: ds, .. } => {
                    (sym.name[1..].to_string(), *ds)
                }
                _ => continue,
            };
            let key = (bare_name.clone(), decl_sigil);
            if seen.contains(&key) {
                continue;
            }
            seen.insert(key);

            let priority = std::cmp::min(scope_size, 255) as u8;
            let detail = match &sym.detail {
                SymbolDetail::Variable { decl_kind, .. } => {
                    Some(match decl_kind {
                        DeclKind::My => "my".to_string(),
                        DeclKind::Our => "our".to_string(),
                        DeclKind::State => "state".to_string(),
                        DeclKind::Field => "field".to_string(),
                        DeclKind::Param => "param".to_string(),
                        DeclKind::ForVar => "for".to_string(),
                    })
                }
                SymbolDetail::Field { .. } => Some("field".to_string()),
                _ => None,
            };

            generate_cross_sigil_candidates(
                &bare_name,
                decl_sigil,
                sigil,
                detail,
                priority,
                &mut candidates,
            );
        }

        candidates
    }

    /// Complete methods for an invocant (variable or class name) at a point.
    pub fn complete_methods(&self, invocant: &str, point: Point) -> Vec<CompletionCandidate> {
        // Resolve invocant → class name
        let class_name = if !invocant.starts_with('$')
            && !invocant.starts_with('@')
            && !invocant.starts_with('%')
        {
            Some(invocant.to_string())
        } else {
            self.resolve_invocant_class(
                invocant,
                self.scope_at(point).unwrap_or(ScopeId(0)),
                point,
            )
        };

        if let Some(ref cn) = class_name {
            // Try to find methods scoped to this class
            let mut candidates = Vec::new();
            let mut found_class = false;

            // Check for class definition → collect its methods
            for sym in &self.symbols {
                if matches!(sym.kind, SymKind::Class) && sym.name == *cn {
                    found_class = true;
                    // Implicit new for core classes
                    candidates.push(CompletionCandidate {
                        label: "new".to_string(),
                        kind: SymKind::Method,
                        detail: Some(format!("{}->new", cn)),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                    });
                    break;
                }
            }

            // Find all subs/methods in this class/package
            for sym in &self.symbols {
                if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    if self.symbol_in_class(sym.id, cn) {
                        candidates.push(CompletionCandidate {
                            label: sym.name.clone(),
                            kind: sym.kind,
                            detail: Some(cn.clone()),
                            insert_text: None,
                            sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                        });
                    }
                }
            }

            if !candidates.is_empty() {
                return candidates;
            }

            // Package with subs
            if !found_class {
                for sym in &self.symbols {
                    if matches!(sym.kind, SymKind::Package) && sym.name == *cn {
                        found_class = true;
                        break;
                    }
                }
            }
            if found_class {
                // Return subs scoped to this package
                for sym in &self.symbols {
                    if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                        if self.symbol_in_class(sym.id, cn) {
                            candidates.push(CompletionCandidate {
                                label: sym.name.clone(),
                                kind: sym.kind,
                                detail: Some(cn.clone()),
                                insert_text: None,
                                sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                            });
                        }
                    }
                }
                if !candidates.is_empty() {
                    return candidates;
                }
            }
        }

        // Fallback: all subs/methods in file
        self.symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| CompletionCandidate {
                label: s.name.clone(),
                kind: s.kind,
                detail: Some(
                    if matches!(s.kind, SymKind::Method) {
                        "method"
                    } else {
                        "sub"
                    }
                    .to_string(),
                ),
                insert_text: None,
                sort_priority: PRIORITY_FILE_WIDE,
                    additional_edits: vec![],
            })
            .collect()
    }

    /// Complete hash keys for a variable at a point.
    pub fn complete_hash_keys(&self, var_text: &str, point: Point) -> Vec<CompletionCandidate> {
        let owner = self.resolve_hash_key_owner(var_text, point);
        let owner = match owner {
            Some(o) => o,
            None => return Vec::new(),
        };

        let defs = self.hash_key_defs_for_owner(&owner);
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();

        for def in defs {
            if seen.contains(&def.name) {
                continue;
            }
            seen.insert(def.name.clone());

            let is_dynamic = matches!(
                &def.detail,
                SymbolDetail::HashKeyDef { is_dynamic: true, .. }
            );

            let detail = match &owner {
                HashKeyOwner::Class(name) => Some(format!("{}->{{{}}}", name, def.name)),
                HashKeyOwner::Variable { name, .. } => {
                    Some(format!("{}{{{}}}", name, def.name))
                }
                HashKeyOwner::Sub(name) => Some(format!("{}()->{{{}}}", name, def.name)),
            };

            candidates.push(CompletionCandidate {
                label: def.name.clone(),
                kind: SymKind::Variable,
                detail,
                insert_text: None,
                sort_priority: if is_dynamic { PRIORITY_DYNAMIC } else { PRIORITY_FILE_WIDE },
                    additional_edits: vec![],
            });
        }

        candidates
    }

    /// Complete hash keys for a known class name (from expression type resolution).
    pub fn complete_hash_keys_for_class(&self, class_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        let owner = HashKeyOwner::Class(class_name.to_string());
        let defs = self.hash_key_defs_for_owner(&owner);
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();

        for def in defs {
            if seen.contains(&def.name) {
                continue;
            }
            seen.insert(def.name.clone());
            candidates.push(CompletionCandidate {
                label: def.name.clone(),
                kind: SymKind::Variable,
                detail: Some(format!("{}->{{{}}}", class_name, def.name)),
                insert_text: None,
                sort_priority: PRIORITY_FILE_WIDE,
                additional_edits: vec![],
            });
        }
        candidates
    }

    /// Complete hash keys for a sub's return value (from expression type resolution).
    pub fn complete_hash_keys_for_sub(&self, sub_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        let owner = HashKeyOwner::Sub(sub_name.to_string());
        let defs = self.hash_key_defs_for_owner(&owner);
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();

        for def in defs {
            if seen.contains(&def.name) {
                continue;
            }
            seen.insert(def.name.clone());
            candidates.push(CompletionCandidate {
                label: def.name.clone(),
                kind: SymKind::Variable,
                detail: Some(format!("{}()->{{{}}}", sub_name, def.name)),
                insert_text: None,
                sort_priority: PRIORITY_FILE_WIDE,
                additional_edits: vec![],
            });
        }
        candidates
    }

    /// General completion: all variables (all sigils) + subs + packages.
    pub fn complete_general(&self, point: Point) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();

        // Variables (all sigils)
        for sigil in ['$', '@', '%'] {
            candidates.extend(self.complete_variables(point, sigil));
        }

        // Subs
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                candidates.push(CompletionCandidate {
                    label: sym.name.clone(),
                    kind: sym.kind,
                    detail: Some(
                        if matches!(sym.kind, SymKind::Method) {
                            "method"
                        } else {
                            "sub"
                        }
                        .to_string(),
                    ),
                    insert_text: None,
                    sort_priority: PRIORITY_FILE_WIDE,
                    additional_edits: vec![],
                });
            }
        }

        // Packages/classes
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                candidates.push(CompletionCandidate {
                    label: sym.name.clone(),
                    kind: sym.kind,
                    detail: Some(
                        if matches!(sym.kind, SymKind::Class) {
                            "class"
                        } else {
                            "package"
                        }
                        .to_string(),
                    ),
                    insert_text: None,
                    sort_priority: PRIORITY_LESS_RELEVANT,
                    additional_edits: vec![],
                });
            }
        }

        candidates
    }

    /// Complete keyval args at a call site.
    /// Returns `key =>` completions for unused keys.
    pub fn complete_keyval_args(
        &self,
        call_name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        used_keys: &HashSet<String>,
    ) -> Vec<CompletionCandidate> {
        // For `new` calls on a class, check for :param fields
        if call_name == "new" {
            if let Some(inv) = invocant {
                let class_name = if !inv.starts_with('$') {
                    Some(inv.to_string())
                } else {
                    self.resolve_invocant_class(
                        inv,
                        self.scope_at(point).unwrap_or(ScopeId(0)),
                        point,
                    )
                };
                if let Some(ref cn) = class_name {
                    let param_candidates = self.class_param_completions(cn, used_keys);
                    if !param_candidates.is_empty() {
                        return param_candidates;
                    }
                }
            }
        }

        // Find the sub definition
        let sub_sym = self.find_sub_for_call(call_name, is_method, invocant, point);
        let sub_sym = match sub_sym {
            Some(s) => s,
            None => return Vec::new(),
        };

        // Get params
        let params = match &sub_sym.detail {
            SymbolDetail::Sub { params, .. } => params,
            _ => return Vec::new(),
        };

        // Find slurpy %hash param
        let slurpy = params
            .iter()
            .find(|p| p.is_slurpy && p.name.starts_with('%'));
        let slurpy_name = match slurpy {
            Some(p) => {
                if p.name.starts_with('%') || p.name.starts_with('$') || p.name.starts_with('@') {
                    &p.name[1..]
                } else {
                    &p.name
                }
            }
            None => return Vec::new(),
        };

        // Find hash key accesses for this param name within the sub's body scope
        let body_scope = self.find_body_scope(sub_sym);
        let keys = match body_scope {
            Some(scope_id) => self.hash_keys_in_scope(slurpy_name, scope_id),
            None => Vec::new(),
        };

        keys.into_iter()
            .filter(|k| !used_keys.contains(k))
            .map(|k| CompletionCandidate {
                label: format!("{} =>", k),
                kind: SymKind::Variable,
                detail: Some(format!("{}(%{})", call_name, slurpy_name)),
                insert_text: Some(format!("{} => ", k)),
                sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
            })
            .collect()
    }

    /// Resolve signature info for a call (sub/method name).
    pub fn signature_for_call(
        &self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
    ) -> Option<SignatureInfo> {
        let sub_sym = self.find_sub_for_call(name, is_method, invocant, point)?;

        let (params, sym_is_method) = match &sub_sym.detail {
            SymbolDetail::Sub { params, is_method, .. } => (params.clone(), *is_method),
            _ => return None,
        };

        let mut params = params;
        let is_method = is_method
            || sym_is_method
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

        Some(SignatureInfo {
            name: name.to_string(),
            params,
            is_method,
        })
    }

    // ---- Internal completion helpers ----

    /// Find a sub/method symbol by name, optionally scoped to a class.
    fn find_sub_for_call(
        &self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
    ) -> Option<&Symbol> {
        // Resolve class name for scoped lookup
        let class_name = if is_method {
            invocant.and_then(|inv| {
                if !inv.starts_with('$') && !inv.starts_with('@') && !inv.starts_with('%') {
                    Some(inv.to_string())
                } else {
                    self.resolve_invocant_class(
                        inv,
                        self.scope_at(point).unwrap_or(ScopeId(0)),
                        point,
                    )
                }
            })
        } else {
            None
        };

        // Try class-scoped first
        if let Some(ref cn) = class_name {
            for &sid in self.symbols_named(name) {
                let sym = self.symbol(sid);
                if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    if self.symbol_in_class(sid, cn) {
                        return Some(sym);
                    }
                }
            }
        }

        // Fallback: any sub/method with that name
        for &sid in self.symbols_named(name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                return Some(sym);
            }
        }

        None
    }

    /// Resolve a variable text to a HashKeyOwner for hash key completion.
    fn resolve_hash_key_owner(&self, var_text: &str, point: Point) -> Option<HashKeyOwner> {
        let bare_name = if var_text.starts_with('$') || var_text.starts_with('@') || var_text.starts_with('%') {
            &var_text[1..]
        } else {
            var_text
        };

        // Try type inference → class owner or sub return hash keys
        if let Some(it) = self.inferred_type(var_text, point) {
            if let Some(cn) = it.class_name() {
                return Some(HashKeyOwner::Class(cn.to_string()));
            }
            // HashRef from a call binding → follow to sub's return hash keys
            if *it == InferredType::HashRef {
                for cb in &self.call_bindings {
                    if cb.variable == var_text
                        && cb.span.start <= point
                        && contains_point(&self.scopes[cb.scope.0 as usize].span, point)
                    {
                        return Some(HashKeyOwner::Sub(cb.func_name.clone()));
                    }
                }
            }
        }

        // Try resolving the variable declaration → Variable owner
        // For $hash{}, try %hash first
        let try_names: Vec<String> = if var_text.starts_with('$') {
            vec![format!("%{}", bare_name), var_text.to_string()]
        } else {
            vec![var_text.to_string()]
        };

        for name in &try_names {
            if let Some(sym) = self.resolve_variable(name, point) {
                return Some(HashKeyOwner::Variable {
                    name: name.clone(),
                    def_scope: sym.scope,
                });
            }
        }

        // Check if any existing hash key refs/defs use this bare_name
        for sym in &self.symbols {
            if let SymbolDetail::HashKeyDef { ref owner, .. } = sym.detail {
                match owner {
                    HashKeyOwner::Variable { name, .. } => {
                        let owner_bare = if name.starts_with('$') || name.starts_with('@') || name.starts_with('%') {
                            &name[1..]
                        } else {
                            name
                        };
                        if owner_bare == bare_name {
                            return Some(owner.clone());
                        }
                    }
                    HashKeyOwner::Class(_) | HashKeyOwner::Sub(_) => {}
                }
            }
        }

        None
    }

    /// Collect :param field names from a core class as keyval completions.
    fn class_param_completions(
        &self,
        class_name: &str,
        used_keys: &HashSet<String>,
    ) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Field) {
                if let SymbolDetail::Field { ref attributes, .. } = sym.detail {
                    if attributes.contains(&"param".to_string()) {
                        // Check this field belongs to the class
                        if self.symbol_in_class(sym.id, class_name) {
                            let key = sym
                                .name
                                .trim_start_matches('$')
                                .trim_start_matches('@')
                                .trim_start_matches('%')
                                .to_string();
                            if !used_keys.contains(&key) {
                                candidates.push(CompletionCandidate {
                                    label: format!("{} =>", key),
                                    kind: SymKind::Variable,
                                    detail: Some(format!("{}->new(:param)", class_name)),
                                    insert_text: Some(format!("{} => ", key)),
                                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                                });
                            }
                        }
                    }
                }
            }
        }
        candidates
    }

    /// Find hash key names accessed via a variable in a specific scope.
    fn hash_keys_in_scope(&self, var_bare_name: &str, scope_id: ScopeId) -> Vec<String> {
        let scope_span = &self.scopes[scope_id.0 as usize].span;
        let mut keys = Vec::new();
        let mut seen = HashSet::new();

        for r in &self.refs {
            if let RefKind::HashKeyAccess { ref var_text, .. } = r.kind {
                // Check the var_text's bare name matches
                let ref_bare = if var_text.starts_with('$')
                    || var_text.starts_with('@')
                    || var_text.starts_with('%')
                {
                    &var_text[1..]
                } else {
                    var_text.as_str()
                };
                if ref_bare == var_bare_name && contains_point(scope_span, r.span.start) {
                    if !seen.contains(&r.target_name) {
                        seen.insert(r.target_name.clone());
                        keys.push(r.target_name.clone());
                    }
                }
            }
        }

        keys
    }
}

/// Generate cross-sigil completion candidates for a variable.
fn generate_cross_sigil_candidates(
    bare_name: &str,
    decl_sigil: char,
    requested_sigil: char,
    detail: Option<String>,
    priority: u8,
    out: &mut Vec<CompletionCandidate>,
) {
    match requested_sigil {
        '$' => {
            if decl_sigil == '$' {
                out.push(CompletionCandidate {
                    label: format!("${}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
            }
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("${}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some(format!("@{}", bare_name))),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
                out.push(CompletionCandidate {
                    label: format!("$#{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail
                        .clone()
                        .or(Some(format!("last index of @{}", bare_name))),
                    insert_text: Some(format!("#{}", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                });
            }
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("${}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some(format!("%{}", bare_name))),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
            }
        }
        '@' => {
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("@{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
                out.push(CompletionCandidate {
                    label: format!("@{}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("array slice".to_string()),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                });
            }
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("@{}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some("hash slice".to_string())),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
            }
        }
        '%' => {
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("%{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
                out.push(CompletionCandidate {
                    label: format!("%{}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("hash kv slice".to_string()),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                });
            }
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("%{}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("array kv slice".to_string()),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                });
            }
        }
        _ => {}
    }
}

// ---- Helpers ----

pub(crate) fn contains_point(span: &Span, point: Point) -> bool {
    (span.start.row < point.row || (span.start.row == point.row && span.start.column <= point.column))
        && (point.row < span.end.row || (point.row == span.end.row && point.column <= span.end.column))
}

/// Walk up from a node to find an ancestor of the given kind.
fn find_ancestor<'a>(node: tree_sitter::Node<'a>, kind: &str) -> Option<tree_sitter::Node<'a>> {
    let mut current = node;
    for _ in 0..20 {
        if current.kind() == kind {
            return Some(current);
        }
        current = current.parent()?;
    }
    None
}

fn span_size(span: &Span) -> usize {
    // Use row difference as primary size metric; column as tiebreaker
    let rows = span.end.row.saturating_sub(span.start.row);
    let cols = if rows == 0 {
        span.end.column.saturating_sub(span.start.column)
    } else {
        0
    };
    rows * 10000 + cols
}

fn source_line_at(source: &str, row: usize) -> &str {
    source.lines().nth(row).unwrap_or("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Point;

    /// Helper: build a minimal FileAnalysis with a single file scope and given type constraints.
    fn fa_with_constraints(constraints: Vec<TypeConstraint>) -> FileAnalysis {
        FileAnalysis::new(
            vec![Scope {
                id: ScopeId(0),
                parent: None,
                kind: ScopeKind::File,
                span: Span { start: Point::new(0, 0), end: Point::new(10, 0) },
                package: None,
            }],
            vec![],
            vec![],
            constraints,
            vec![],
            vec![],
            vec![],
        )
    }

    fn constraint(var: &str, row: usize, inferred_type: InferredType) -> TypeConstraint {
        TypeConstraint {
            variable: var.to_string(),
            scope: ScopeId(0),
            constraint_span: Span { start: Point::new(row, 0), end: Point::new(row, 20) },
            inferred_type,
        }
    }

    // ---- Type constraint resolution tests ----

    #[test]
    fn test_resolve_hashref_type() {
        let fa = fa_with_constraints(vec![constraint("$href", 0, InferredType::HashRef)]);
        assert_eq!(fa.inferred_type("$href", Point::new(1, 0)), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_resolve_arrayref_type() {
        let fa = fa_with_constraints(vec![constraint("$aref", 0, InferredType::ArrayRef)]);
        assert_eq!(fa.inferred_type("$aref", Point::new(1, 0)), Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_resolve_coderef_type() {
        let fa = fa_with_constraints(vec![constraint("$cref", 0, InferredType::CodeRef)]);
        assert_eq!(fa.inferred_type("$cref", Point::new(1, 0)), Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_resolve_regexp_type() {
        let fa = fa_with_constraints(vec![constraint("$re", 0, InferredType::Regexp)]);
        assert_eq!(fa.inferred_type("$re", Point::new(1, 0)), Some(&InferredType::Regexp));
    }

    #[test]
    fn test_resolve_numeric_type() {
        let fa = fa_with_constraints(vec![constraint("$n", 0, InferredType::Numeric)]);
        assert_eq!(fa.inferred_type("$n", Point::new(1, 0)), Some(&InferredType::Numeric));
    }

    #[test]
    fn test_resolve_string_type() {
        let fa = fa_with_constraints(vec![constraint("$s", 0, InferredType::String)]);
        assert_eq!(fa.inferred_type("$s", Point::new(1, 0)), Some(&InferredType::String));
    }

    #[test]
    fn test_resolve_reassignment_changes_type() {
        let fa = fa_with_constraints(vec![
            constraint("$x", 0, InferredType::HashRef),
            constraint("$x", 3, InferredType::ArrayRef),
        ]);
        assert_eq!(fa.inferred_type("$x", Point::new(1, 0)), Some(&InferredType::HashRef));
        assert_eq!(fa.inferred_type("$x", Point::new(4, 0)), Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_resolve_sub_return_type() {
        // Hand-craft a FileAnalysis with a sub that has a return type
        let fa = FileAnalysis::new(
            vec![Scope {
                id: ScopeId(0),
                parent: None,
                kind: ScopeKind::File,
                span: Span { start: Point::new(0, 0), end: Point::new(10, 0) },
                package: None,
            }],
            vec![Symbol {
                id: SymbolId(0),
                name: "get_config".to_string(),
                kind: SymKind::Sub,
                span: Span { start: Point::new(0, 0), end: Point::new(2, 1) },
                selection_span: Span { start: Point::new(0, 4), end: Point::new(0, 14) },
                scope: ScopeId(0),
                detail: SymbolDetail::Sub {
                    params: vec![],
                    is_method: false,
                    return_type: Some(InferredType::HashRef),
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
        );
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
        assert_eq!(fa.sub_return_type("nonexistent"), None);
    }

    // ---- Return type resolution tests (decoupled, pure function) ----

    #[test]
    fn test_resolve_return_type_all_agree() {
        assert_eq!(
            resolve_return_type(&[InferredType::HashRef, InferredType::HashRef]),
            Some(InferredType::HashRef),
        );
    }

    #[test]
    fn test_resolve_return_type_disagreement() {
        assert_eq!(
            resolve_return_type(&[InferredType::HashRef, InferredType::ArrayRef]),
            None,
        );
    }

    #[test]
    fn test_resolve_return_type_empty() {
        assert_eq!(resolve_return_type(&[]), None);
    }

    #[test]
    fn test_resolve_return_type_object_subsumes_hashref() {
        // Object + HashRef → Object wins (overloaded objects)
        assert_eq!(
            resolve_return_type(&[
                InferredType::ClassName("Foo".into()),
                InferredType::HashRef,
            ]),
            Some(InferredType::ClassName("Foo".into())),
        );
        // Order shouldn't matter
        assert_eq!(
            resolve_return_type(&[
                InferredType::HashRef,
                InferredType::ClassName("Foo".into()),
            ]),
            Some(InferredType::ClassName("Foo".into())),
        );
    }

    #[test]
    fn test_resolve_return_type_object_does_not_subsume_arrayref() {
        // Object + ArrayRef → disagreement
        assert_eq!(
            resolve_return_type(&[
                InferredType::ClassName("Foo".into()),
                InferredType::ArrayRef,
            ]),
            None,
        );
    }

    #[test]
    fn test_resolve_return_type_single() {
        assert_eq!(
            resolve_return_type(&[InferredType::CodeRef]),
            Some(InferredType::CodeRef),
        );
    }

    #[test]
    fn test_class_name_helper() {
        assert_eq!(InferredType::ClassName("Foo".into()).class_name(), Some("Foo"));
        assert_eq!(InferredType::FirstParam { package: "Bar".into() }.class_name(), Some("Bar"));
        assert_eq!(InferredType::BlessResult { package: "Baz".into() }.class_name(), Some("Baz"));
        assert_eq!(InferredType::HashRef.class_name(), None);
        assert_eq!(InferredType::ArrayRef.class_name(), None);
        assert_eq!(InferredType::CodeRef.class_name(), None);
        assert_eq!(InferredType::Regexp.class_name(), None);
        assert_eq!(InferredType::Numeric.class_name(), None);
        assert_eq!(InferredType::String.class_name(), None);
    }
}
