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
