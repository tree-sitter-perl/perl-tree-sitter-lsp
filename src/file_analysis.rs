//! FileAnalysis: single-pass scope graph for Perl source files.
//!
//! Built once per parse/reparse via `builder::build()`. Every LSP query
//! becomes a lookup against these tables instead of a tree walk.
//!
//! Designed to compose into a project index: `HashMap<PathBuf, FileAnalysis>`.

use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use tree_sitter::Point;

use crate::module_index::ModuleIndex;

// ---- Serde proxy for tree_sitter::Point ----

/// Remote-derive proxy for `tree_sitter::Point`, which doesn't implement serde.
/// Fields mirror `Point` exactly — use `#[serde(with = "PointDef")]` on Point fields.
#[derive(Serialize, Deserialize)]
#[serde(remote = "Point")]
pub(crate) struct PointDef {
    pub row: usize,
    pub column: usize,
}

/// Helper module for `Option<Point>` serialization via the remote-derived proxy.
pub(crate) mod point_opt_serde {
    use super::PointDef;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use tree_sitter::Point;

    pub fn serialize<S: Serializer>(val: &Option<Point>, s: S) -> Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct W<'a>(#[serde(with = "PointDef")] &'a Point);
        val.as_ref().map(W).serialize(s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Option<Point>, D::Error> {
        #[derive(Deserialize)]
        struct W(#[serde(with = "PointDef")] Point);
        Option::<W>::deserialize(d).map(|o| o.map(|W(p)| p))
    }
}

// ---- Shared types (formerly in analysis.rs) ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    #[serde(with = "PointDef")]
    pub start: Point,
    #[serde(with = "PointDef")]
    pub end: Point,
}

/// Extract the function/method name from a call expression node.
pub(crate) fn extract_call_name(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "method_call_expression" => node
            .child_by_field_name("method")
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| s.to_string()),
        "function_call_expression" | "ambiguous_function_call_expression" => node
            .child_by_field_name("function")
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| s.to_string()),
        _ => None,
    }
}

pub(crate) fn node_to_span(node: tree_sitter::Node) -> Span {
    Span {
        start: node.start_position(),
        end: node.end_position(),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FoldRange {
    pub start_line: usize,
    pub end_line: usize,
    pub kind: FoldKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum FoldKind {
    Region,
    Comment,
}

// ---- IDs ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ScopeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SymbolId(pub u32);

// ---- Scope ----

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymKind,
    pub span: Span,
    pub selection_span: Span,
    /// Scope this symbol is declared in.
    pub scope: ScopeId,
    /// The package this symbol belongs to (captured at creation time).
    pub package: Option<String>,
    /// Kind-specific extra data.
    pub detail: SymbolDetail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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
        /// Pre-rendered markdown from POD or comments preceding this sub.
        doc: Option<String>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DeclKind {
    My,
    Our,
    State,
    Field,
    Param,
    ForVar,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamInfo {
    pub name: String,
    pub default: Option<String>,
    pub is_slurpy: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub struct FieldDetail {
    pub name: String,
    pub sigil: char,
    pub attributes: Vec<String>,
}

// ---- Ref ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ref {
    pub kind: RefKind,
    pub span: Span,
    pub scope: ScopeId,
    pub target_name: String,
    pub access: AccessKind,
    /// For variable refs: which Symbol this resolves to (filled in post-pass).
    pub resolves_to: Option<SymbolId>,
}

/// What kind of entity is being renamed — determines single-file vs cross-file scope.
#[derive(Debug)]
pub enum RenameKind {
    Variable,
    Function(String),
    Package(String),
    Method(String),
    HashKey(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum RefKind {
    Variable,
    FunctionCall,
    MethodCall {
        invocant: String,
        /// Span of the invocant node (for complex expressions needing tree resolution).
        invocant_span: Option<Span>,
        /// Span of just the method name (for rename — r.span covers the whole expression).
        method_name_span: Span,
    },
    PackageRef,
    HashKeyAccess {
        var_text: String,
        owner: Option<HashKeyOwner>,
    },
    /// The container variable in `$hash{key}`, `@arr[0]`, etc.
    ContainerAccess,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AccessKind {
    Read,
    Write,
    Declaration,
}

// ---- Type constraints ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeConstraint {
    pub variable: String,
    pub scope: ScopeId,
    pub constraint_span: Span,
    pub inferred_type: InferredType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum InferredType {
    /// `$p = Point->new(...)` — variable is an instance of ClassName.
    ClassName(String),
    /// `my ($self) = @_` in `package Foo` — first param is the class.
    FirstParam { package: String },
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
    /// Extract the class name if this is an object type (ClassName or FirstParam).
    pub fn class_name(&self) -> Option<&str> {
        match self {
            InferredType::ClassName(name) => Some(name.as_str()),
            InferredType::FirstParam { package } => Some(package.as_str()),
            _ => None,
        }
    }

    /// True if this is any Object variant (ClassName or FirstParam).
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HashKeyOwner {
    Class(String),
    Variable { name: String, def_scope: ScopeId },
    /// Hash keys from a sub's return value: `sub get_config { return { host => 1 } }`
    Sub(String),
}

// ---- Call binding ----

/// A variable assigned from a function call: `my $cfg = get_config()`.
/// Stored in FileAnalysis so query-time resolution can follow the chain.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallBinding {
    pub variable: String,
    pub func_name: String,
    pub scope: ScopeId,
    pub span: Span,
}

/// A method call binding: `$var = $invocant->method()`.
/// Recorded during build, resolved in post-pass via `find_method_return_type`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MethodCallBinding {
    pub variable: String,
    pub invocant_var: String,
    pub method_name: String,
    pub scope: ScopeId,
    pub span: Span,
}

// ---- Import ----

/// A `use Foo::Bar qw(func1 func2)` statement parsed from the source.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    /// Module name, e.g. "List::Util".
    pub module_name: String,
    /// Explicitly imported symbols from `qw(...)`. Empty = bare `use Foo;`.
    pub imported_symbols: Vec<String>,
    /// Span of the entire `use` statement.
    pub span: Span,
    /// Position of the closing delimiter of the `qw()` list (the `)` character).
    /// Used to insert new imports into an existing qw list.
    #[serde(with = "point_opt_serde")]
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

// Token type/modifier indices — must match the order in semantic_token_types/modifiers().
// Some are forward-declared for future phases.
pub const TOK_VARIABLE: u32 = 0;
pub const TOK_PARAMETER: u32 = 1;
pub const TOK_FUNCTION: u32 = 2;
pub const TOK_METHOD: u32 = 3;
pub const TOK_MACRO: u32 = 4;
pub const TOK_PROPERTY: u32 = 5;
pub const TOK_NAMESPACE: u32 = 6;
#[allow(dead_code)] pub const TOK_REGEXP: u32 = 7;
#[allow(dead_code)] pub const TOK_ENUM_MEMBER: u32 = 8;
pub const TOK_KEYWORD: u32 = 9;

pub const MOD_DECLARATION: u32 = 0;
pub const MOD_READONLY: u32 = 1;
pub const MOD_MODIFICATION: u32 = 2;
pub const MOD_DEFAULT_LIBRARY: u32 = 3;
#[allow(dead_code)] pub const MOD_DEPRECATED: u32 = 4;
#[allow(dead_code)] pub const MOD_STATIC: u32 = 5;
pub const MOD_SCALAR: u32 = 6;
pub const MOD_ARRAY: u32 = 7;
pub const MOD_HASH: u32 = 8;

#[derive(Debug, Clone)]
pub struct PerlSemanticToken {
    pub span: Span,
    pub token_type: u32,
    pub modifiers: u32,
}

fn sigil_modifier(sigil: char) -> u32 {
    match sigil {
        '@' => 1 << MOD_ARRAY,
        '%' => 1 << MOD_HASH,
        _ => 1 << MOD_SCALAR,
    }
}

// ---- FileAnalysis ----

#[derive(Debug, Serialize, Deserialize)]
pub struct FileAnalysis {
    // Core tables
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub refs: Vec<Ref>,
    pub type_constraints: Vec<TypeConstraint>,
    pub fold_ranges: Vec<FoldRange>,
    pub imports: Vec<Import>,
    pub call_bindings: Vec<CallBinding>,
    pub method_call_bindings: Vec<MethodCallBinding>,

    /// Parent classes for each package in this file.
    /// Populated by the builder from use parent/base, @ISA, and class :isa.
    pub package_parents: HashMap<String, Vec<String>>,

    /// Return types from imported functions (populated after build from module index).
    pub imported_return_types: HashMap<String, InferredType>,

    /// Functions implicitly imported by OOP frameworks (e.g. `has`, `extends`, `with`).
    /// Used to suppress "not defined" diagnostics for these known framework keywords.
    pub framework_imports: HashSet<String>,

    /// Exported function names from `@EXPORT = ...` assignments.
    pub export: Vec<String>,
    /// Exported function names from `@EXPORT_OK = ...` assignments.
    pub export_ok: Vec<String>,

    // Baseline counts — set after build_indices(), used to truncate on re-enrichment.
    #[serde(default)]
    base_type_constraint_count: usize,
    #[serde(default)]
    base_symbol_count: usize,

    // Indices (built in post-pass — skipped by serde; call rebuild_all_indices() after deserialize)
    #[serde(skip, default)]
    scope_starts: Vec<(Point, ScopeId)>, // sorted by start point
    #[serde(skip, default)]
    symbols_by_name: HashMap<String, Vec<SymbolId>>,
    #[serde(skip, default)]
    symbols_by_scope: HashMap<ScopeId, Vec<SymbolId>>,
    #[serde(skip, default)]
    refs_by_name: HashMap<String, Vec<usize>>,
    #[serde(skip, default)]
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
        package_parents: HashMap<String, Vec<String>>,
        method_call_bindings: Vec<MethodCallBinding>,
        framework_imports: HashSet<String>,
        export: Vec<String>,
        export_ok: Vec<String>,
    ) -> Self {
        let mut fa = FileAnalysis {
            scopes,
            symbols,
            refs,
            type_constraints,
            fold_ranges,
            imports,
            call_bindings,
            method_call_bindings,
            package_parents,
            imported_return_types: HashMap::new(),
            framework_imports,
            export,
            export_ok,
            base_type_constraint_count: 0,
            base_symbol_count: 0,
            scope_starts: Vec::new(),
            symbols_by_name: HashMap::new(),
            symbols_by_scope: HashMap::new(),
            refs_by_name: HashMap::new(),
            type_constraints_by_var: HashMap::new(),
        };
        fa.build_indices();
        fa.resolve_method_call_types(None); // local post-pass
        fa.base_type_constraint_count = fa.type_constraints.len();
        fa.base_symbol_count = fa.symbols.len();
        fa
    }

    /// Rebuild all derived indices after deserialization.
    /// Idempotent: safe to call on a freshly deserialized `FileAnalysis` whose
    /// index fields were zeroed by `#[serde(skip, default)]`.
    pub fn after_deserialize(&mut self) {
        // Clear first in case this is called on a populated FileAnalysis.
        self.scope_starts.clear();
        self.symbols_by_name.clear();
        self.symbols_by_scope.clear();
        self.refs_by_name.clear();
        self.type_constraints_by_var.clear();
        self.build_indices();
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

    /// Get the return type of a named sub/method (local definitions only).
    pub fn sub_return_type_local(&self, name: &str) -> Option<&InferredType> {
        for sym in &self.symbols {
            if sym.name == name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
                    if return_type.is_some() {
                        return return_type.as_ref();
                    }
                }
            }
        }
        None
    }

    /// Get the return type of a named sub/method.
    /// Checks local definitions first, then falls back to imported return types.
    pub fn sub_return_type(&self, name: &str) -> Option<&InferredType> {
        self.sub_return_type_local(name)
            .or_else(|| self.imported_return_types.get(name))
    }

    /// Convenience wrapper: enrich with return types only (no hash keys).
    #[cfg(test)]
    pub fn enrich_imported_types(&mut self, imported_returns: HashMap<String, InferredType>) {
        self.enrich_imported_types_with_keys(imported_returns, HashMap::new(), None);
    }

    /// Resolve call bindings for imported functions and inject hash key defs.
    /// Call after building, when module index data is available.
    pub fn enrich_imported_types_with_keys(
        &mut self,
        imported_returns: HashMap<String, InferredType>,
        imported_hash_keys: HashMap<String, Vec<String>>,
        module_index: Option<&ModuleIndex>,
    ) {
        // Truncate back to baseline so repeated enrichment doesn't accumulate duplicates.
        self.type_constraints.truncate(self.base_type_constraint_count);
        self.symbols.truncate(self.base_symbol_count);

        let mut new_constraints = Vec::new();
        for binding in &self.call_bindings {
            // Skip if already resolved (local sub or builtin)
            if self.sub_return_type_local(&binding.func_name).is_some()
                || builtin_return_type(&binding.func_name).is_some()
            {
                continue;
            }
            if let Some(rt) = imported_returns.get(&binding.func_name) {
                new_constraints.push(TypeConstraint {
                    variable: binding.variable.clone(),
                    scope: binding.scope,
                    constraint_span: binding.span,
                    inferred_type: rt.clone(),
                });
            }
        }
        self.type_constraints.extend(new_constraints);
        self.imported_return_types = imported_returns;

        // Inject synthetic HashKeyDef symbols for imported functions' hash keys.
        for (func_name, keys) in &imported_hash_keys {
            let owner = HashKeyOwner::Sub(func_name.clone());
            for key_name in keys {
                let id = SymbolId(self.symbols.len() as u32);
                // Use a zero-size span at file start for synthetic symbols
                let zero_span = Span {
                    start: Point { row: 0, column: 0 },
                    end: Point { row: 0, column: 0 },
                };
                self.symbols.push(Symbol {
                    id,
                    name: key_name.clone(),
                    kind: SymKind::HashKeyDef,
                    span: zero_span,
                    selection_span: zero_span,
                    scope: ScopeId(0),
                    package: None,
                    detail: SymbolDetail::HashKeyDef {
                        owner: owner.clone(),
                        is_dynamic: false,
                    },
                });
            }
        }

        // Cross-file method call return type resolution
        self.resolve_method_call_types(module_index);

        self.rebuild_enrichment_indices();
    }

    /// Resolve method call bindings to type constraints.
    /// Called in local post-pass (None) and cross-file enrichment (Some(module_index)).
    fn resolve_method_call_types(&mut self, module_index: Option<&ModuleIndex>) {
        let bindings = self.method_call_bindings.clone();
        for binding in &bindings {
            // Skip if this variable already has a type at this point
            if self.inferred_type(&binding.variable, binding.span.start).is_some() {
                continue;
            }

            // Resolve invocant to class name
            let class_name = self.resolve_invocant_class(
                &binding.invocant_var,
                binding.scope,
                binding.span.start,
            );

            if let Some(cn) = class_name {
                if let Some(rt) = self.find_method_return_type(&cn, &binding.method_name, module_index, None) {
                    self.type_constraints.push(TypeConstraint {
                        variable: binding.variable.clone(),
                        scope: binding.scope,
                        constraint_span: binding.span,
                        inferred_type: rt,
                    });
                    // Incrementally update the index so the NEXT binding can see this constraint
                    let idx = self.type_constraints.len() - 1;
                    self.type_constraints_by_var
                        .entry(binding.variable.clone())
                        .or_default()
                        .push(idx);
                }
            }
        }
    }

    /// Rebuild indices affected by enrichment (type constraints + symbols).
    fn rebuild_enrichment_indices(&mut self) {
        self.type_constraints_by_var.clear();
        for (i, tc) in self.type_constraints.iter().enumerate() {
            self.type_constraints_by_var
                .entry(tc.variable.clone())
                .or_default()
                .push(i);
        }

        self.symbols_by_name.clear();
        self.symbols_by_scope.clear();
        for sym in &self.symbols {
            self.symbols_by_name
                .entry(sym.name.clone())
                .or_default()
                .push(sym.id);
            self.symbols_by_scope
                .entry(sym.scope)
                .or_default()
                .push(sym.id);
        }
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
    pub fn resolve_expression_type(
        &self,
        node: tree_sitter::Node,
        source: &[u8],
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
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
                let invocant_type = self.resolve_expression_type(invocant_node, source, module_index)?;
                let class_name = invocant_type.class_name()?;
                let method_name = node.child_by_field_name("method")?;
                let method_text = method_name.utf8_text(source).ok()?;
                // Constructor call returns Object(ClassName)
                if method_text == "new" {
                    return Some(InferredType::ClassName(class_name.to_string()));
                }
                let arg_count = self.count_call_args(node);
                self.find_method_return_type(class_name, method_text, module_index, Some(arg_count))
            }
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let func_node = node.child_by_field_name("function")?;
                let name = func_node.utf8_text(source).ok()?;
                self.sub_return_type(name).cloned()
                    .or_else(|| builtin_return_type(name))
            }
            "func1op_call_expression" | "func0op_call_expression" => {
                let name = node.child(0)?.utf8_text(source).ok()?;
                builtin_return_type(name)
            }
            "hash_element_expression" => {
                // For chaining: resolve the base expression
                let base = node.named_child(0)?;
                self.resolve_expression_type(base, source, module_index)
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
        module_index: Option<&ModuleIndex>,
    ) -> Option<HashKeyOwner> {
        let hash_elem = tree.root_node()
            .descendant_for_point_range(point, point)
            .and_then(|n| find_ancestor(n, "hash_element_expression"))?;
        let base = hash_elem.named_child(0)?;
        let ty = self.resolve_expression_type(base, source, module_index)?;

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

    /// Find a method's return type within a class/package, walking inheritance.
    pub(crate) fn find_method_return_type(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
        arg_count: Option<usize>,
    ) -> Option<InferredType> {
        match self.resolve_method_in_ancestors(class_name, method_name, module_index) {
            Some(MethodResolution::Local { sym_id, .. }) => {
                // Collect all matching symbols for arity selection
                let candidates: Vec<_> = self.symbols_named(method_name).iter()
                    .filter(|&&sid| {
                        let s = self.symbol(sid);
                        matches!(s.kind, SymKind::Sub | SymKind::Method)
                            && self.symbol_in_class(sid, class_name)
                    })
                    .copied()
                    .collect();

                if candidates.len() <= 1 || arg_count.is_none() {
                    // Single symbol or no arity info — primary (first match)
                    if let SymbolDetail::Sub { ref return_type, .. } = self.symbol(sym_id).detail {
                        return return_type.clone();
                    }
                    return None;
                }

                // Multiple overloads: pick by param count
                let target = arg_count.unwrap();
                for sid in &candidates {
                    if let SymbolDetail::Sub { ref params, ref return_type, .. } = self.symbol(*sid).detail {
                        if params.len() == target {
                            return return_type.clone();
                        }
                    }
                }
                // No exact match — fall back to primary
                if let SymbolDetail::Sub { ref return_type, .. } = self.symbol(sym_id).detail {
                    return_type.clone()
                } else {
                    None
                }
            }
            Some(MethodResolution::CrossFile { ref class }) => {
                module_index.and_then(|idx| {
                    let cached = idx.get_cached(class)?;
                    let sub = cached.sub_info(method_name)?;

                    // If no arity info or no overloads, return primary.
                    let counts = sub.param_counts();
                    let has_overloads = counts.len() > 1;
                    let target = match arg_count {
                        Some(n) if has_overloads => n,
                        _ => return sub.return_type().cloned(),
                    };

                    sub.return_type_for_arity(target).cloned()
                        .or_else(|| sub.return_type().cloned())
                })
            }
            None => None,
        }
    }

    /// Count arguments in a method/function call expression (excluding invocant).
    pub(crate) fn count_call_args(&self, node: tree_sitter::Node) -> usize {
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return 0,
        };
        match args.kind() {
            "parenthesized_expression" | "list_expression" => args.named_child_count(),
            _ => 1, // single argument
        }
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

    /// Format a method completion detail string, appending return type if known.
    fn method_detail(
        &self,
        class_name: &str,
        method_name: &str,
        defining_class: Option<&str>,
        module_index: Option<&ModuleIndex>,
    ) -> String {
        let base = if let Some(dc) = defining_class {
            if dc != class_name {
                format!("{} (from {})", class_name, dc)
            } else {
                class_name.to_string()
            }
        } else {
            class_name.to_string()
        };
        if let Some(ref rt) = self.find_method_return_type(class_name, method_name, module_index, None) {
            format!("{} → {}", base, format_inferred_type(rt))
        } else {
            base
        }
    }

    /// Complete methods for a known class name, walking the inheritance chain.
    pub fn complete_methods_for_class(
        &self,
        class_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();
        let mut seen_names: HashSet<String> = HashSet::new();

        // Check for class definition → implicit new
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Class) && sym.name == class_name {
                candidates.push(CompletionCandidate {
                    label: "new".to_string(),
                    kind: SymKind::Method,
                    detail: Some(self.method_detail(class_name, "new", None, module_index)),
                    insert_text: None,
                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                });
                seen_names.insert("new".to_string());
                break;
            }
        }

        // Collect methods from this class and all ancestors
        self.collect_ancestor_methods(class_name, class_name, module_index, &mut candidates, &mut seen_names, 0);

        candidates
    }

    /// Recursively collect methods from a class and its ancestors, deduping by name.
    fn collect_ancestor_methods(
        &self,
        original_class: &str,
        class_name: &str,
        module_index: Option<&ModuleIndex>,
        candidates: &mut Vec<CompletionCandidate>,
        seen_names: &mut HashSet<String>,
        depth: usize,
    ) {
        if depth > 20 {
            return;
        }

        // Local methods in this class
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sym.id, class_name) && !seen_names.contains(&sym.name) {
                    seen_names.insert(sym.name.clone());
                    let defining = if class_name != original_class { Some(class_name) } else { None };
                    candidates.push(CompletionCandidate {
                        label: sym.name.clone(),
                        kind: sym.kind,
                        detail: Some(self.method_detail(original_class, &sym.name, defining, module_index)),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                    });
                }
            }
        }

        // Walk local parents
        if let Some(parents) = self.package_parents.get(class_name) {
            for parent in parents {
                self.collect_ancestor_methods(
                    original_class, parent, module_index, candidates, seen_names, depth + 1,
                );
            }
        }

        // Walk cross-file parents
        if let Some(idx) = module_index {
            // Methods from the cross-file module itself
            if let Some(cached) = idx.get_cached(class_name) {
                for sym in &cached.analysis.symbols {
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                        continue;
                    }
                    if seen_names.contains(&sym.name) {
                        continue;
                    }
                    seen_names.insert(sym.name.clone());
                    let is_method = sym.kind == SymKind::Method
                        || matches!(sym.detail, SymbolDetail::Sub { is_method: true, .. });
                    let kind = if is_method { SymKind::Method } else { SymKind::Sub };
                    let defining = if class_name != original_class { Some(class_name) } else { None };
                    let detail = self.method_detail(original_class, &sym.name, defining, module_index);
                    candidates.push(CompletionCandidate {
                        label: sym.name.clone(),
                        kind,
                        detail: Some(detail),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                    });
                }
            }

            // Cross-file parents
            let cross_parents = idx.parents_cached(class_name);
            for parent in &cross_parents {
                // Don't re-collect if it's a local package (already handled above)
                if !self.package_parents.contains_key(parent.as_str()) {
                    self.collect_ancestor_methods(
                        original_class, parent, module_index, candidates, seen_names, depth + 1,
                    );
                }
            }
        }
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
        self.refs.iter()
            .filter(|r| contains_point(&r.span, point))
            .min_by_key(|r| span_size(&r.span))
    }

    /// Find the symbol whose selection_span contains the point.
    pub fn symbol_at(&self, point: Point) -> Option<&Symbol> {
        self.symbols.iter().find(|s| contains_point(&s.selection_span, point))
    }

    // ---- High-level queries ----

    /// Go-to-definition: resolve the symbol at cursor to its definition span.
    pub fn find_definition(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>, module_index: Option<&ModuleIndex>) -> Option<Span> {
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
                    let class_name = self.resolve_method_invocant(invocant, invocant_span, r.scope, point, tree, source_bytes, module_index);

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
                        // Find method in the resolved class (walks inheritance)
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, .. }) => {
                                return Some(self.symbol(sym_id).selection_span);
                            }
                            Some(MethodResolution::CrossFile { .. }) => {
                                // Cross-file method — return None so the LSP adapter
                                // can resolve it via ModuleIndex with the correct URI.
                                return None;
                            }
                            None => {
                                // If the class has known parents, the method might be
                                // inherited but the module index hasn't resolved yet.
                                // Return None so the cross-file block in symbols.rs
                                // gets a chance, rather than jumping to the package decl.
                                if self.package_parents.contains_key(cn) {
                                    return None;
                                }
                            }
                        }
                        // Method not found and no parents — go to class def
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
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start, module_index)
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
        if let Some((target_id, include_decl)) = self.resolve_target_at(point, tree, source_bytes, None) {
            let mut results = self.collect_refs_for_target(target_id, include_decl, tree, source_bytes);
            results.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            results.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            results.into_iter().map(|(span, _)| span).collect()
        } else {
            Vec::new()
        }
    }

    /// Document highlights: like references but with read/write annotation.
    pub fn find_highlights(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Vec<(Span, AccessKind)> {
        if let Some((target_id, _)) = self.resolve_target_at(point, tree, source_bytes, None) {
            let mut results = self.collect_refs_for_target(target_id, true, tree, source_bytes);
            results.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            results.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            results
        } else {
            Vec::new()
        }
    }

    /// Shared implementation for find_references and find_highlights.
    fn collect_refs_for_target(
        &self,
        target_id: SymbolId,
        include_decl: bool,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
    ) -> Vec<(Span, AccessKind)> {
        let sym = self.symbol(target_id);
        let mut results: Vec<(Span, AccessKind)> = Vec::new();

        // Include the declaration itself
        if include_decl {
            results.push((sym.selection_span, AccessKind::Declaration));
        }

        // Collect all refs that resolve to this symbol
        for r in &self.refs {
            if r.resolves_to == Some(target_id) {
                results.push((r.span, r.access));
            }
        }

        // For subs/methods/packages/classes, also find refs by name
        if matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class | SymKind::Module) {
            for r in &self.refs {
                if r.target_name == sym.name && r.resolves_to.is_none() {
                    match (&r.kind, &sym.kind) {
                        (RefKind::FunctionCall, SymKind::Sub) => results.push((r.span, r.access)),
                        (RefKind::MethodCall { .. }, SymKind::Sub | SymKind::Method) => results.push((r.span, r.access)),
                        (RefKind::PackageRef, SymKind::Package | SymKind::Class | SymKind::Module) => results.push((r.span, r.access)),
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
                            if let (Some(t), Some(s)) = (tree, source_bytes) {
                                self.resolve_hash_owner_from_tree(t, s, r.span.start, None)
                                    .as_ref()
                                    .map_or(false, |resolved| resolved == owner)
                            } else {
                                false
                            }
                        }
                    };
                    if matches {
                        results.push((r.span, r.access));
                    }
                }
            }
        }

        results
    }

    /// Hover info: return display text for the symbol at cursor.
    pub fn hover_info(&self, point: Point, source: &str, tree: Option<&tree_sitter::Tree>, module_index: Option<&ModuleIndex>) -> Option<String> {
        // Check refs first
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    // Check if this variable is also a dynamic method call target
                    // (e.g. $self->$method() where $method is a known constant)
                    let method_hover = self.refs.iter()
                        .find(|mr| matches!(mr.kind, RefKind::MethodCall { .. })
                            && contains_point(&mr.span, point)
                            && mr.target_name != r.target_name);
                    if let Some(mr) = method_hover {
                        if let RefKind::MethodCall { ref invocant, ref invocant_span, .. } = mr.kind {
                            let class_name = self.resolve_method_invocant(
                                invocant, invocant_span, mr.scope, point, tree, Some(source.as_bytes()), module_index,
                            );
                            if let Some(ref cn) = class_name {
                                match self.resolve_method_in_ancestors(cn, &mr.target_name, module_index) {
                                    Some(MethodResolution::Local { sym_id, class: ref defining_class, .. }) => {
                                        let sym = self.symbol(sym_id);
                                        let line = source_line_at(source, sym.selection_span.start.row);
                                        let class_label = if defining_class != cn {
                                            format!("{} (from {})", cn, defining_class)
                                        } else {
                                            cn.to_string()
                                        };
                                        let mut text = format!("```perl\n{}\n```\n\n*class {} — resolved from `{}`*", line.trim(), class_label, r.target_name);
                                        if let Some(ref rt) = self.find_method_return_type(cn, &mr.target_name, module_index, None) {
                                            text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(rt)));
                                        }
                                        if let SymbolDetail::Sub { ref doc, .. } = sym.detail {
                                            if let Some(ref d) = doc {
                                                text.push_str(&format!("\n\n{}", d));
                                            }
                                        }
                                        return Some(text);
                                    }
                                    Some(MethodResolution::CrossFile { ref class }) => {
                                        if let Some(idx) = module_index {
                                            if let Some(cached) = idx.get_cached(class) {
                                                if let Some(sub_info) = cached.sub_info(&mr.target_name) {
                                                    let sig = format_cross_file_signature(&mr.target_name, &sub_info);
                                                    let mut text = format!("```perl\n{}\n```\n\n*class {} — resolved from `{}`*", sig, class, r.target_name);
                                                    if let Some(rt) = sub_info.return_type() {
                                                        text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(rt)));
                                                    }
                                                    if let Some(doc) = sub_info.doc() {
                                                        text.push_str(&format!("\n\n{}", doc));
                                                    }
                                                    return Some(text);
                                                }
                                            }
                                        }
                                    }
                                    None => {}
                                }
                            }
                        }
                    }
                    if let Some(sym_id) = r.resolves_to {
                        let sym = self.symbol(sym_id);
                        return Some(self.format_symbol_hover_at(sym, source, point));
                    }
                    // Unresolved variable — try resolve ourselves
                    if let Some(sym) = self.resolve_variable(&r.target_name, point) {
                        return Some(self.format_symbol_hover_at(sym, source, point));
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
                        invocant, invocant_span, r.scope, point, tree, Some(source.as_bytes()), module_index,
                    );
                    if let Some(ref cn) = class_name {
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, class: ref defining_class, .. }) => {
                                let sym = self.symbol(sym_id);
                                let line = source_line_at(source, sym.selection_span.start.row);
                                let class_label = if defining_class != cn {
                                    format!("{} (from {})", cn, defining_class)
                                } else {
                                    cn.to_string()
                                };
                                let mut text = format!("```perl\n{}\n```\n\n*class {}*", line.trim(), class_label);
                                if let Some(ref rt) = self.find_method_return_type(cn, &r.target_name, module_index, None) {
                                    text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(rt)));
                                }
                                return Some(text);
                            }
                            Some(MethodResolution::CrossFile { ref class }) => {
                                if let Some(idx) = module_index {
                                    if let Some(cached) = idx.get_cached(class) {
                                        if let Some(sub_info) = cached.sub_info(&r.target_name) {
                                            let class_label = if class != cn {
                                                format!("{} (from {})", cn, class)
                                            } else {
                                                cn.to_string()
                                            };
                                            let sig = format_cross_file_signature(&r.target_name, &sub_info);
                                            let mut text = format!("```perl\n{}\n```\n\n*class {}*", sig, class_label);
                                            if let Some(rt) = sub_info.return_type() {
                                                text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(rt)));
                                            }
                                            if let Some(doc) = sub_info.doc() {
                                                text.push_str(&format!("\n\n{}", doc));
                                            }
                                            return Some(text);
                                        }
                                    }
                                }
                            }
                            None => {}
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
    pub fn rename_at(&self, point: Point, new_name: &str, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Option<Vec<(Span, String)>> {
        let refs = self.find_references(point, tree, source_bytes);
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

    /// Determine what kind of rename is appropriate for the cursor position.
    pub fn rename_kind_at(&self, point: Point) -> Option<RenameKind> {
        if let Some(r) = self.ref_at(point) {
            return Some(match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => RenameKind::Variable,
                RefKind::FunctionCall => RenameKind::Function(r.target_name.clone()),
                RefKind::MethodCall { .. } => RenameKind::Method(r.target_name.clone()),
                RefKind::PackageRef => RenameKind::Package(r.target_name.clone()),
                RefKind::HashKeyAccess { .. } => RenameKind::HashKey(r.target_name.clone()),
            });
        }
        if let Some(sym) = self.symbol_at(point) {
            return match sym.kind {
                SymKind::Variable | SymKind::Field => Some(RenameKind::Variable),
                SymKind::Sub => Some(RenameKind::Function(sym.name.clone())),
                SymKind::Method => Some(RenameKind::Method(sym.name.clone())),
                SymKind::Package | SymKind::Class => Some(RenameKind::Package(sym.name.clone())),
                _ => None,
            };
        }
        None
    }

    /// Find all occurrences of a sub name (def + ALL call refs) for cross-file rename.
    /// Searches both FunctionCall and MethodCall refs because Perl subs can be called either way.
    pub fn rename_sub(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)> {
        let mut edits = Vec::new();
        for sym in &self.symbols {
            if sym.name == old_name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                edits.push((sym.selection_span, new_name.to_string()));
            }
        }
        for r in &self.refs {
            if r.target_name == old_name {
                match &r.kind {
                    RefKind::FunctionCall => {
                        edits.push((r.span, new_name.to_string()));
                    }
                    RefKind::MethodCall { method_name_span, .. } => {
                        edits.push((*method_name_span, new_name.to_string()));
                    }
                    _ => {}
                }
            }
        }
        edits
    }

    /// Find all occurrences of a package name (def + refs + use statements) for cross-file rename.
    pub fn rename_package(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)> {
        let mut edits = Vec::new();
        for sym in &self.symbols {
            if sym.name == old_name && matches!(sym.kind, SymKind::Package | SymKind::Class | SymKind::Module) {
                edits.push((sym.selection_span, new_name.to_string()));
            }
        }
        for r in &self.refs {
            if r.target_name == old_name && matches!(r.kind, RefKind::PackageRef) {
                edits.push((r.span, new_name.to_string()));
            }
        }
        edits
    }

    // ---- Internal resolution helpers ----

    /// Find the target symbol for the thing at cursor. Returns (SymbolId, include_decl_in_refs).
    fn resolve_target_at(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>, module_index: Option<&ModuleIndex>) -> Option<(SymbolId, bool)> {
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
                RefKind::MethodCall { ref invocant, ref invocant_span, .. } => {
                    let class_name = self.resolve_method_invocant(
                        invocant, invocant_span, r.scope, point, tree, source_bytes, module_index,
                    );
                    // Try inheritance-aware resolution first
                    if let Some(ref cn) = class_name {
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, .. }) => {
                                return Some((sym_id, true));
                            }
                            Some(MethodResolution::CrossFile { .. }) => {
                                // Cross-file: no local SymbolId, fall through to name match
                            }
                            None => {}
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
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start, module_index)
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

    /// Resolve a method call's invocant to a class name (public API for cross-file goto-def).
    pub fn resolve_method_invocant_public(
        &self,
        invocant: &str,
        invocant_span: &Option<Span>,
        scope: ScopeId,
        point: Point,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
        module_index: Option<&ModuleIndex>,
    ) -> Option<String> {
        self.resolve_method_invocant(invocant, invocant_span, scope, point, tree, source_bytes, module_index)
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
        module_index: Option<&ModuleIndex>,
    ) -> Option<String> {
        // Try tree-based resolution for complex invocants, fall through on failure
        if let (Some(span), Some(tree), Some(src)) = (invocant_span, tree, source_bytes) {
            if let Some(node) = tree.root_node()
                .descendant_for_point_range(span.start, span.end)
            {
                if let Some(ty) = self.resolve_expression_type(node, src, module_index) {
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
    #[cfg(test)]
    pub(crate) fn find_method_in_class(&self, class_name: &str, method_name: &str) -> Option<Span> {
        self.find_method_in_class_with_index(class_name, method_name, None)
    }

    /// Find a method definition, walking the inheritance chain if needed.
    #[cfg(test)]
    fn find_method_in_class_with_index(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Option<Span> {
        match self.resolve_method_in_ancestors(class_name, method_name, module_index) {
            Some(MethodResolution::Local { sym_id, .. }) => {
                Some(self.symbol(sym_id).selection_span)
            }
            Some(MethodResolution::CrossFile { .. }) => {
                // Cross-file method found but no local span to return
                // Caller should handle cross-file resolution via ModuleIndex
                None
            }
            None => None,
        }
    }

    /// Walk the inheritance chain to find a method (DFS, matches Perl's default MRO).
    pub fn resolve_method_in_ancestors(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Option<MethodResolution> {
        self.resolve_method_in_ancestors_inner(class_name, method_name, module_index, 0)
    }

    fn resolve_method_in_ancestors_inner(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
        depth: usize,
    ) -> Option<MethodResolution> {
        if depth > 20 {
            return None; // safety limit
        }

        // Check class_name directly (local symbols)
        for &sid in self.symbols_named(method_name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sid, class_name) {
                    return Some(MethodResolution::Local {
                        class: class_name.to_string(),
                        sym_id: sid,
                    });
                }
            }
        }

        // Check local parents from package_parents
        if let Some(parents) = self.package_parents.get(class_name) {
            for parent in parents {
                if let Some(res) = self.resolve_method_in_ancestors_inner(
                    parent, method_name, module_index, depth + 1,
                ) {
                    return Some(res);
                }
            }
        }

        // Check cross-file parents via ModuleIndex
        if let Some(idx) = module_index {
            if let Some(cached) = idx.get_cached(class_name) {
                if cached.has_sub(method_name) {
                    return Some(MethodResolution::CrossFile {
                        class: class_name.to_string(),
                    });
                }
            }
            // Walk cross-file parent chain
            let cross_parents = idx.parents_cached(class_name);
            for parent in &cross_parents {
                // Check if parent is a local package first
                if self.package_parents.contains_key(parent.as_str()) {
                    if let Some(res) = self.resolve_method_in_ancestors_inner(
                        parent, method_name, module_index, depth + 1,
                    ) {
                        return Some(res);
                    }
                } else {
                    // Fully cross-file parent
                    if let Some(res) = self.resolve_cross_file_method(
                        parent, method_name, idx, depth + 1,
                    ) {
                        return Some(res);
                    }
                }
            }
        }

        None
    }

    /// Resolve a method through the cross-file inheritance chain only.
    fn resolve_cross_file_method(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: &ModuleIndex,
        depth: usize,
    ) -> Option<MethodResolution> {
        if depth > 20 {
            return None;
        }
        if let Some(cached) = module_index.get_cached(class_name) {
            if cached.has_sub(method_name) {
                return Some(MethodResolution::CrossFile {
                    class: class_name.to_string(),
                });
            }
        }
        let parents = module_index.parents_cached(class_name);
        for parent in &parents {
            if let Some(res) = self.resolve_cross_file_method(
                parent, method_name, module_index, depth + 1,
            ) {
                return Some(res);
            }
        }
        None
    }

    /// Check if a symbol is defined within a class/package.
    pub(crate) fn symbol_in_class(&self, sym_id: SymbolId, class_name: &str) -> bool {
        let sym = self.symbol(sym_id);
        // Fast path: check the package captured at symbol creation time.
        // This is authoritative for multi-package files where the scope's
        // mutable `package` field gets overwritten by later package statements.
        if let Some(ref pkg) = sym.package {
            return pkg == class_name;
        }
        // Fallback: walk the scope chain for symbols without a package field.
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
        self.format_symbol_hover_at(sym, source, sym.selection_span.end)
    }

    fn format_symbol_hover_at(&self, sym: &Symbol, source: &str, at: Point) -> String {
        let line = source_line_at(source, sym.span.start.row);
        let mut text = format!("```perl\n{}\n```", line.trim());

        // Append inferred type for variables/fields
        if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
            if let Some(it) = self.inferred_type(&sym.name, at) {
                text.push_str(&format!("\n\n*type: {}*", format_inferred_type(it)));
            }
        }

        // Append return type for subs/methods
        if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
            if let SymbolDetail::Sub { return_type: Some(ref rt), .. } = sym.detail {
                text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(rt)));
            }
        }

        text
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
    pub fn semantic_tokens(&self) -> Vec<PerlSemanticToken> {
        let mut tokens: Vec<PerlSemanticToken> = Vec::new();

        // ---- Variable/parameter/self declarations from symbols ----
        for sym in &self.symbols {
            match sym.kind {
                SymKind::Variable | SymKind::Field => {
                    let is_self = sym.name == "$self" || sym.name == "$class";
                    let (sigil, is_readonly, is_param) = match &sym.detail {
                        SymbolDetail::Variable { sigil, decl_kind } => {
                            let readonly = matches!(decl_kind, DeclKind::Field);
                            let is_param = matches!(decl_kind, DeclKind::Param | DeclKind::ForVar);
                            (*sigil, readonly, is_param)
                        }
                        SymbolDetail::Field { sigil, attributes } => {
                            let readonly = !attributes.iter().any(|a| a == "writer" || a == "mutator" || a == "accessor");
                            (*sigil, readonly, true)
                        }
                        _ => continue,
                    };
                    let token_type = if is_self { TOK_KEYWORD } else if is_param { TOK_PARAMETER } else { TOK_VARIABLE };
                    // Don't add sigil modifier for $self/$class — it would override the keyword color
                    let mut mods = if is_self { 0 } else { sigil_modifier(sigil) };
                    mods |= 1 << MOD_DECLARATION;
                    if is_readonly { mods |= 1 << MOD_READONLY; }
                    tokens.push(PerlSemanticToken { span: sym.selection_span, token_type, modifiers: mods });
                }
                SymKind::Package | SymKind::Class => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_NAMESPACE,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                SymKind::Module => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_NAMESPACE,
                        modifiers: 0,
                    });
                }
                SymKind::Sub => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_FUNCTION,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                SymKind::Method => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_METHOD,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                _ => {}
            }
        }

        // ---- Refs: variables, functions, methods, properties, namespaces ----
        let imported_names: std::collections::HashSet<&str> = self.imports.iter()
            .flat_map(|imp| imp.imported_symbols.iter().map(|s| s.as_str()))
            .collect();

        for r in &self.refs {
            // Skip declaration refs — the symbol loop already emits tokens for declarations
            if matches!(r.access, AccessKind::Declaration) {
                continue;
            }
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    let sigil = r.target_name.chars().next().unwrap_or('$');
                    let is_self = r.target_name == "$self" || r.target_name == "$class";
                    let token_type = if is_self { TOK_KEYWORD } else { TOK_VARIABLE };
                    // Don't add sigil modifier for $self/$class — it would override the keyword color
                    let mut mods = if is_self { 0 } else { sigil_modifier(sigil) };
                    if matches!(r.access, AccessKind::Write) { mods |= 1 << MOD_MODIFICATION; }
                    tokens.push(PerlSemanticToken { span: r.span, token_type, modifiers: mods });
                }
                RefKind::FunctionCall => {
                    // Framework DSL keywords → macro, otherwise function
                    let token_type = if self.framework_imports.contains(r.target_name.as_str()) {
                        TOK_MACRO
                    } else {
                        TOK_FUNCTION
                    };
                    let mut mods = 0;
                    if imported_names.contains(r.target_name.as_str()) {
                        mods |= 1 << MOD_DEFAULT_LIBRARY;
                    }
                    tokens.push(PerlSemanticToken { span: r.span, token_type, modifiers: mods });
                }
                RefKind::MethodCall { method_name_span, .. } => {
                    // Use method_name_span for precise highlighting of just the method name
                    let mods = 0; // TODO: readonly for ro accessors, static for class methods
                    tokens.push(PerlSemanticToken { span: *method_name_span, token_type: TOK_METHOD, modifiers: mods });
                }
                RefKind::PackageRef => {
                    tokens.push(PerlSemanticToken { span: r.span, token_type: TOK_NAMESPACE, modifiers: 0 });
                }
                RefKind::HashKeyAccess { .. } => {
                    tokens.push(PerlSemanticToken { span: r.span, token_type: TOK_PROPERTY, modifiers: 0 });
                }
            }
        }

        // ---- HashKeyDef symbols → property tokens ----
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::HashKeyDef) {
                tokens.push(PerlSemanticToken {
                    span: sym.selection_span,
                    token_type: TOK_PROPERTY,
                    modifiers: 1 << MOD_DECLARATION,
                });
            }
        }

        tokens.sort_by_key(|t| (t.span.start.row, t.span.start.column));
        // Dedup by position — if two tokens start at the same (row, col), keep the first
        tokens.dedup_by(|b, a| a.span.start.row == b.span.start.row && a.span.start.column == b.span.start.column);
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

// ---- Method resolution types ----

/// Result of resolving a method through the inheritance chain.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum MethodResolution {
    /// Found in a local class within this file.
    Local { class: String, sym_id: SymbolId },
    /// Found in a cross-file module (use ModuleIndex to get details).
    CrossFile { class: String },
}

/// Result of resolving a sub/method call — local symbol or cross-file metadata.
pub enum ResolvedSub<'a> {
    /// Found locally in this file's symbols.
    Local(&'a Symbol),
    /// Found in a cross-file module via ModuleIndex.
    CrossFile {
        params: Vec<ParamInfo>,
        /// Inferred type per param (parallel to `params`); `None` if unknown.
        param_types: Vec<Option<InferredType>>,
        is_method: bool,
        hash_keys: Vec<String>,
    },
}

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
    /// End of the sub body — used to query inferred types for params.
    pub body_end: Point,
    /// Pre-resolved param types (for cross-file subs where body_end is meaningless).
    pub param_types: Option<Vec<Option<String>>>,
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
            let candidates = self.complete_methods_for_class(cn, None);
            if !candidates.is_empty() {
                return candidates;
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

    /// Complete hash keys for a resolved owner.
    fn complete_hash_keys_for_owner(&self, owner: &HashKeyOwner) -> Vec<CompletionCandidate> {
        let defs = self.hash_key_defs_for_owner(owner);
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();

        for def in defs {
            if !seen.insert(def.name.clone()) {
                continue;
            }

            let is_dynamic = matches!(
                &def.detail,
                SymbolDetail::HashKeyDef { is_dynamic: true, .. }
            );

            let detail = match owner {
                HashKeyOwner::Class(name) => format!("{}->{{{}}}", name, def.name),
                HashKeyOwner::Variable { name, .. } => format!("{}{{{}}}", name, def.name),
                HashKeyOwner::Sub(name) => format!("{}()->{{{}}}", name, def.name),
            };

            candidates.push(CompletionCandidate {
                label: def.name.clone(),
                kind: SymKind::Variable,
                detail: Some(detail),
                insert_text: None,
                sort_priority: if is_dynamic { PRIORITY_DYNAMIC } else { PRIORITY_FILE_WIDE },
                additional_edits: vec![],
            });
        }

        candidates
    }

    /// Complete hash keys for a variable at a point.
    pub fn complete_hash_keys(&self, var_text: &str, point: Point) -> Vec<CompletionCandidate> {
        match self.resolve_hash_key_owner(var_text, point) {
            Some(owner) => self.complete_hash_keys_for_owner(&owner),
            None => Vec::new(),
        }
    }

    /// Complete hash keys for a known class name (from expression type resolution).
    pub fn complete_hash_keys_for_class(&self, class_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        self.complete_hash_keys_for_owner(&HashKeyOwner::Class(class_name.to_string()))
    }

    /// Complete hash keys for a sub's return value (from expression type resolution).
    pub fn complete_hash_keys_for_sub(&self, sub_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        self.complete_hash_keys_for_owner(&HashKeyOwner::Sub(sub_name.to_string()))
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
        module_index: Option<&ModuleIndex>,
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

        // Find the sub definition (local or cross-file)
        let resolved = match self.find_sub_for_call(call_name, is_method, invocant, point, module_index) {
            Some(r) => r,
            None => return Vec::new(),
        };

        match resolved {
            ResolvedSub::Local(sub_sym) => {
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
            ResolvedSub::CrossFile { hash_keys, params, .. } => {
                // Check if any param is slurpy %hash
                let has_slurpy = params.iter().any(|p| p.is_slurpy && p.name.starts_with('%'));
                if !has_slurpy || hash_keys.is_empty() {
                    return Vec::new();
                }

                hash_keys.into_iter()
                    .filter(|k| !used_keys.contains(k))
                    .map(|k| CompletionCandidate {
                        label: format!("{} =>", k),
                        kind: SymKind::Variable,
                        detail: Some(format!("{}()", call_name)),
                        insert_text: Some(format!("{} => ", k)),
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                    })
                    .collect()
            }
        }
    }

    /// Resolve signature info for a call (sub/method name).
    pub fn signature_for_call(
        &self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<SignatureInfo> {
        let resolved = self.find_sub_for_call(name, is_method, invocant, point, module_index)?;

        match resolved {
            ResolvedSub::Local(sub_sym) => {
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
                    body_end: sub_sym.span.end,
                    param_types: None, // local — use inferred_type() with body_end
                })
            }
            ResolvedSub::CrossFile {
                params: cross_params,
                param_types: cross_param_types,
                is_method: cf_is_method,
                ..
            } => {
                let mut params: Vec<ParamInfo> = cross_params;
                let mut param_types: Vec<Option<String>> = cross_param_types
                    .into_iter()
                    .map(|t| t.as_ref().map(inferred_type_to_tag))
                    .collect();

                let is_method = is_method || cf_is_method
                    || params.first().map_or(false, |p| p.name == "$self" || p.name == "$class");

                // Strip $self/$class from method params (and their types).
                if is_method && !params.is_empty() {
                    let first = &params[0].name;
                    if first == "$self" || first == "$class" {
                        params.remove(0);
                        if !param_types.is_empty() {
                            param_types.remove(0);
                        }
                    }
                }

                Some(SignatureInfo {
                    name: name.to_string(),
                    params,
                    is_method,
                    body_end: Point::new(0, 0),
                    param_types: Some(param_types),
                })
            }
        }
    }

    // ---- Internal completion helpers ----

    /// Find a sub/method by name, optionally scoped to a class.
    /// Returns `ResolvedSub::Local` for same-file symbols, or `ResolvedSub::CrossFile`
    /// for inherited methods and imported functions found via `ModuleIndex`.
    fn find_sub_for_call<'s>(
        &'s self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<ResolvedSub<'s>> {
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

        // Try inheritance-aware class-scoped lookup first
        if let Some(ref cn) = class_name {
            match self.resolve_method_in_ancestors(cn, name, module_index) {
                Some(MethodResolution::Local { sym_id, .. }) => {
                    return Some(ResolvedSub::Local(self.symbol(sym_id)));
                }
                Some(MethodResolution::CrossFile { ref class }) => {
                    if let Some(idx) = module_index {
                        if let Some(cached) = idx.get_cached(class) {
                            if let Some(sub_info) = cached.sub_info(name) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
                None => {}
            }
        }

        // Fallback: any local sub/method with that name
        for &sid in self.symbols_named(name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                return Some(ResolvedSub::Local(sym));
            }
        }

        // Fallback: imported function via ModuleIndex
        if !is_method {
            if let Some(idx) = module_index {
                for import in &self.imports {
                    if import.imported_symbols.iter().any(|s| s == name) {
                        if let Some(cached) = idx.get_cached(&import.module_name) {
                            if let Some(sub_info) = cached.sub_info(name) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
                // Also check @EXPORT (bare imports)
                for import in &self.imports {
                    if let Some(cached) = idx.get_cached(&import.module_name) {
                        if cached.analysis.export.iter().any(|s| s == name) {
                            if let Some(sub_info) = cached.sub_info(name) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
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

        // Try type inference → class owner
        if let Some(it) = self.inferred_type(var_text, point) {
            if let Some(cn) = it.class_name() {
                return Some(HashKeyOwner::Class(cn.to_string()));
            }
        }

        // Check call bindings → follow to sub's return hash keys
        for cb in &self.call_bindings {
            if cb.variable == var_text
                && cb.span.start <= point
                && contains_point(&self.scopes[cb.scope.0 as usize].span, point)
            {
                return Some(HashKeyOwner::Sub(cb.func_name.clone()));
            }
        }

        // Check method call bindings → follow to method's return hash keys
        for mcb in &self.method_call_bindings {
            if mcb.variable == var_text
                && mcb.span.start <= point
                && contains_point(&self.scopes[mcb.scope.0 as usize].span, point)
            {
                return Some(HashKeyOwner::Sub(mcb.method_name.clone()));
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

/// Return the known return type for a Perl builtin function, if any.
pub(crate) fn builtin_return_type(name: &str) -> Option<InferredType> {
    match name {
        // Numeric returns
        "time" | "length" | "index" | "rindex" | "abs" | "int" | "sqrt"
        | "hex" | "oct" | "ord" | "rand" | "pos" | "tell"
        | "fileno" => Some(InferredType::Numeric),

        // String returns
        "join" | "uc" | "lc" | "ucfirst" | "lcfirst" | "substr" | "sprintf"
        | "ref" | "chr" | "crypt" | "quotemeta" | "pack" | "readline"
        | "readlink" => Some(InferredType::String),

        _ => None,
    }
}

/// Type constraint to push on the first argument of a Perl builtin.
pub(crate) fn builtin_first_arg_type(name: &str) -> Option<InferredType> {
    match name {
        // Numeric arg builtins
        "abs" | "int" | "sqrt" | "chr"
        | "sin" | "cos" | "atan2" | "log" | "exp" => Some(InferredType::Numeric),

        // String arg builtins
        "uc" | "lc" | "ucfirst" | "lcfirst" | "length" | "chomp" | "chop"
        | "substr" | "index" | "rindex" | "quotemeta"
        | "hex" | "oct" | "ord" => Some(InferredType::String),

        _ => None,
    }
}

/// Serialize an InferredType to a simple string tag.
/// Used by signature help's `param_types` field, which piggy-backs on the
/// pre-unification string representation for backwards-compatible JSON output.
pub fn inferred_type_to_tag(ty: &InferredType) -> String {
    match ty {
        InferredType::ClassName(name) => format!("Object:{}", name),
        InferredType::FirstParam { package } => format!("Object:{}", package),
        InferredType::HashRef => "HashRef".to_string(),
        InferredType::ArrayRef => "ArrayRef".to_string(),
        InferredType::CodeRef => "CodeRef".to_string(),
        InferredType::Regexp => "Regexp".to_string(),
        InferredType::Numeric => "Numeric".to_string(),
        InferredType::String => "String".to_string(),
    }
}

/// Format a cross-file method signature from a SubInfo view.
fn format_cross_file_signature(method_name: &str, sub_info: &crate::module_index::SubInfo<'_>) -> String {
    let params = sub_info.params();
    if params.is_empty() {
        format!("sub {}()", method_name)
    } else {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        format!("sub {}({})", method_name, names.join(", "))
    }
}

/// Build a `ResolvedSub::CrossFile` from a SubInfo view, snapshotting owned data.
fn cross_file_resolved(sub_info: &crate::module_index::SubInfo<'_>) -> ResolvedSub<'static> {
    let params: Vec<ParamInfo> = sub_info.params().to_vec();
    let param_types: Vec<Option<InferredType>> = params
        .iter()
        .map(|p| sub_info.param_inferred_type(&p.name).cloned())
        .collect();
    ResolvedSub::CrossFile {
        params,
        param_types,
        is_method: sub_info.is_method(),
        hash_keys: sub_info.hash_keys().to_vec(),
    }
}

pub(crate) fn format_inferred_type(ty: &InferredType) -> String {
    match ty {
        InferredType::ClassName(name) => name.clone(),
        InferredType::FirstParam { package } => package.clone(),
        InferredType::HashRef => "HashRef".to_string(),
        InferredType::ArrayRef => "ArrayRef".to_string(),
        InferredType::CodeRef => "CodeRef".to_string(),
        InferredType::Regexp => "Regexp".to_string(),
        InferredType::Numeric => "Numeric".to_string(),
        InferredType::String => "String".to_string(),
    }
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
            HashMap::new(),
            vec![],
            HashSet::new(),
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
                package: None,
                detail: SymbolDetail::Sub {
                    params: vec![],
                    is_method: false,
                    return_type: Some(InferredType::HashRef),
                    doc: None,
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            HashSet::new(),
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
        assert_eq!(InferredType::HashRef.class_name(), None);
        assert_eq!(InferredType::ArrayRef.class_name(), None);
        assert_eq!(InferredType::CodeRef.class_name(), None);
        assert_eq!(InferredType::Regexp.class_name(), None);
        assert_eq!(InferredType::Numeric.class_name(), None);
        assert_eq!(InferredType::String.class_name(), None);
    }

    // ---- sub_return_type fallback tests ----

    #[test]
    fn test_sub_return_type_imported_fallback() {
        let mut fa = fa_with_constraints(vec![]);
        let mut imported = HashMap::new();
        imported.insert("get_config".to_string(), InferredType::HashRef);
        fa.enrich_imported_types(imported);

        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
        // Local subs should still return None
        assert_eq!(fa.sub_return_type("nonexistent"), None);
    }

    // ---- enrich_imported_types tests ----

    #[test]
    fn test_enrich_imported_types_pushes_constraints() {
        let mut fa = FileAnalysis::new(
            vec![Scope {
                id: ScopeId(0),
                parent: None,
                kind: ScopeKind::File,
                span: Span { start: Point::new(0, 0), end: Point::new(10, 0) },
                package: None,
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            // A call binding: my $cfg = get_config()
            vec![CallBinding {
                variable: "$cfg".to_string(),
                func_name: "get_config".to_string(),
                scope: ScopeId(0),
                span: Span { start: Point::new(2, 0), end: Point::new(2, 30) },
            }],
            HashMap::new(),
            vec![],
            HashSet::new(),
            vec![],
            vec![],
        );

        let mut imported = HashMap::new();
        imported.insert("get_config".to_string(), InferredType::HashRef);
        fa.enrich_imported_types(imported);

        // Should have pushed a type constraint for $cfg
        assert_eq!(
            fa.inferred_type("$cfg", Point::new(3, 0)),
            Some(&InferredType::HashRef),
            "Enrichment should propagate imported return type to call binding variable"
        );
    }
}
