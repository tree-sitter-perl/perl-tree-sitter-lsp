//! FileAnalysis: single-pass scope graph for Perl source files.
//!
//! Built once per parse/reparse via `builder::build()`. Every LSP query
//! becomes a lookup against these tables instead of a tree walk.
//!
//! Designed to compose into a project index: `HashMap<PathBuf, FileAnalysis>`.

use std::collections::HashMap;
use tree_sitter::Point;

use crate::analysis::{FoldKind, FoldRange, Span};

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
pub enum SymbolDetail {
    Variable {
        sigil: char,
        decl_kind: DeclKind,
    },
    Sub {
        params: Vec<ParamInfo>,
        is_method: bool,
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
pub enum RefKind {
    Variable,
    FunctionCall,
    MethodCall { invocant: String },
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
}

// ---- Hash key owner (for scope graph) ----

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKeyOwner {
    Class(String),
    Variable { name: String, def_scope: ScopeId },
}

// ---- FileAnalysis ----

pub struct FileAnalysis {
    // Core tables
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub refs: Vec<Ref>,
    pub type_constraints: Vec<TypeConstraint>,
    pub fold_ranges: Vec<FoldRange>,

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
    ) -> Self {
        let mut fa = FileAnalysis {
            scopes,
            symbols,
            refs,
            type_constraints,
            fold_ranges,
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

    /// Get the enclosing package name at a point.
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
    pub fn symbols_in_scope(&self, scope: ScopeId) -> &[SymbolId] {
        self.symbols_by_scope.get(&scope).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Find all refs with a given target name.
    pub fn refs_named(&self, name: &str) -> Vec<&Ref> {
        self.refs_by_name.get(name)
            .map(|idxs| idxs.iter().map(|&i| &self.refs[i]).collect())
            .unwrap_or_default()
    }

    /// Find all refs that resolve to a specific symbol.
    pub fn refs_to(&self, target: SymbolId) -> Vec<&Ref> {
        self.refs.iter()
            .filter(|r| r.resolves_to == Some(target))
            .collect()
    }

    /// Find all hash key accesses/definitions for a given owner.
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
    pub fn find_definition(&self, point: Point) -> Option<Span> {
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
                RefKind::MethodCall { invocant } => {
                    let class_name = self.resolve_invocant_class(invocant, r.scope, point);
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
                RefKind::HashKeyAccess { owner, .. } => {
                    if let Some(ref owner) = owner {
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
    pub fn find_references(&self, point: Point) -> Vec<Span> {
        if let Some((target_id, include_decl)) = self.resolve_target_at(point) {
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
                    if let RefKind::HashKeyAccess { owner: Some(ref ro), .. } = r.kind {
                        if ro == owner && r.target_name == sym.name {
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
    pub fn find_highlights(&self, point: Point) -> Vec<(Span, AccessKind)> {
        if let Some((target_id, _)) = self.resolve_target_at(point) {
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
                    if let RefKind::HashKeyAccess { owner: Some(ref ro), .. } = r.kind {
                        if ro == owner && r.target_name == sym.name {
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
    pub fn hover_info(&self, point: Point, source: &str) -> Option<String> {
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
                RefKind::MethodCall { invocant } => {
                    let class_name = self.resolve_invocant_class(invocant, r.scope, point);
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
        let refs = self.find_references(point);
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
    fn resolve_target_at(&self, point: Point) -> Option<(SymbolId, bool)> {
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
                RefKind::MethodCall { invocant } => {
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
                RefKind::HashKeyAccess { owner, .. } => {
                    if let Some(ref owner) = owner {
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

    /// Resolve an invocant string to a class name.
    fn resolve_invocant_class(&self, invocant: &str, scope: ScopeId, point: Point) -> Option<String> {
        if invocant.starts_with('$') || invocant.starts_with('@') || invocant.starts_with('%') {
            // Variable invocant → infer type
            self.inferred_type(invocant, point)
                .and_then(|t| match t {
                    InferredType::ClassName(name) => Some(name.clone()),
                    InferredType::FirstParam { package } => Some(package.clone()),
                    InferredType::BlessResult { package } => Some(package.clone()),
                })
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
}

// ---- Helpers ----

fn contains_point(span: &Span, point: Point) -> bool {
    (span.start.row < point.row || (span.start.row == point.row && span.start.column <= point.column))
        && (point.row < span.end.row || (point.row == span.end.row && point.column <= span.end.column))
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
