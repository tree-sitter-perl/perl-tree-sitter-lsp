//! SPIKE: superposition — both `#ifdef` arms as live code, a value out
//! of them is a presence-tagged union that monomorphizes by config.
//!
//! The LSP reframe of `docs/prompt-parallel-realities.md`: for a
//! branch-complete `#ifdef` (arms are whole statements), tree-sitter
//! keeps BOTH arms in the tree, error-free. So you don't pick a config
//! and you don't enumerate 2^N — both branches coexist in one analysis.
//! A function whose return is `#ifdef`'d simply has multiple returns;
//! the real return type is a UNION over the arms.
//!
//! Two fidelity points, per the "how lossy is the reducer" question:
//!   - arms AGREE → the union collapses to one concrete type with no
//!     config needed (`concrete()`); the `#ifdef` was type-irrelevant.
//!   - arms DIVERGE → a union whose members each carry the arm's
//!     presence GUARD, so supplying a config later monomorphizes to the
//!     concrete member (`monomorphize(config)`). Keeping the guard is
//!     the non-lossy middle: not a bare union (can't monomorphize), not
//!     2^N (no enumeration).
//!
//! This is an adapter onto the engine's existing arm-fold
//! (`BranchArmFold` / `SymbolReturnArmFold`): the `#ifdef` arms are
//! another arm source. The spike folds them directly to keep the demo
//! self-contained; a real build routes them to that reducer. The B1
//! leak (a name whose type/value category differs across arms) is OUT
//! of superposition's reach — see `c_reparse::Category::Ambiguous`.
//! Not wired into the pipeline; measured by `c_superpose_tests.rs`.

use crate::c_preproc::Config;
use crate::file_analysis::InferredType;
use tree_sitter::{Node, Tree};

/// The presence condition of one arm — the `#ifdef` guard in force.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Guard {
    Always,
    Defined(String),
    NotDefined(String),
}

impl Guard {
    /// Does this guard hold under `config`? (All atoms here are
    /// decidable; a real build would return `None` for unknowns and
    /// route them to the probe.)
    pub fn holds(&self, config: &Config) -> bool {
        match self {
            Guard::Always => true,
            Guard::Defined(n) => config.is_defined(n),
            Guard::NotDefined(n) => !config.is_defined(n),
        }
    }
    pub fn label(&self) -> String {
        match self {
            Guard::Always => "always".into(),
            Guard::Defined(n) => format!("defined({n})"),
            Guard::NotDefined(n) => format!("!defined({n})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionMember {
    pub ty: InferredType,
    pub guard: Guard,
}

/// A function's return type, superposed over its `#ifdef` arms.
#[derive(Debug, Clone, Default)]
pub struct ReturnUnion {
    pub members: Vec<UnionMember>,
}

impl ReturnUnion {
    /// The collapse when arms agree: one concrete type, no config needed.
    /// `None` if the members genuinely diverge (a real union).
    pub fn concrete(&self) -> Option<InferredType> {
        let mut it = self.members.iter().map(|m| &m.ty);
        let first = it.next()?;
        it.all(|t| t == first).then(|| first.clone())
    }

    /// Monomorphize: keep only the arms live under `config`; if they
    /// agree, the concrete type — the user's "give a config later, get
    /// the concrete type out of the union."
    pub fn monomorphize(&self, config: &Config) -> Option<InferredType> {
        let live: Vec<&InferredType> = self
            .members
            .iter()
            .filter(|m| m.guard.holds(config))
            .map(|m| &m.ty)
            .collect();
        let first = *live.first()?;
        live.iter().all(|t| **t == *first).then(|| first.clone())
    }
}

/// Build the return union for the function named `fn_name` by walking
/// BOTH arms of any `#ifdef` around its returns.
pub fn function_return_union(tree: &Tree, src: &[u8], fn_name: &str) -> ReturnUnion {
    let mut union = ReturnUnion::default();
    let Some(func) = find_function(tree.root_node(), src, fn_name) else { return union };
    let mut cur = func.walk();
    let mut stack = vec![func];
    while let Some(n) = stack.pop() {
        if n.kind() == "return_statement" {
            if let Some(ty) = return_value_type(n, src) {
                union.members.push(UnionMember { ty, guard: guard_for(n, src) });
            }
        }
        for c in n.children(&mut cur) {
            stack.push(c);
        }
    }
    union
}

fn find_function<'a>(root: Node<'a>, src: &[u8], name: &str) -> Option<Node<'a>> {
    let mut cur = root.walk();
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == "function_definition" {
            if let Some(d) = n.child_by_field_name("declarator") {
                if declarator_name(d, src).as_deref() == Some(name) {
                    return Some(n);
                }
            }
        }
        for c in n.children(&mut cur) {
            stack.push(c);
        }
    }
    None
}

fn declarator_name(node: Node, src: &[u8]) -> Option<String> {
    let mut n = node;
    loop {
        match n.kind() {
            "identifier" => return n.utf8_text(src).ok().map(str::to_string),
            "function_declarator" | "pointer_declarator" | "parenthesized_declarator" => {
                n = n.child_by_field_name("declarator")?;
            }
            _ => return None,
        }
    }
}

/// The type of a `return <expr>;` value (literals only in this spike;
/// call/identifier returns defer — they'd resolve through the bag).
fn return_value_type(ret: Node, src: &[u8]) -> Option<InferredType> {
    let mut cur = ret.walk();
    for c in ret.children(&mut cur) {
        match c.kind() {
            "number_literal" | "char_literal" | "true" | "false" => return Some(InferredType::Numeric),
            "string_literal" => return Some(InferredType::String),
            _ => {}
        }
    }
    None
}

/// The `#ifdef` guard in force at `node`, by walking ancestors. Handles
/// one conditional level (`#ifdef NAME` / `#if defined(NAME)`, then vs
/// else). Nested guards would AND-compose — deferred.
fn guard_for(node: Node, src: &[u8]) -> Guard {
    let mut n = node;
    let mut in_else = false;
    while let Some(p) = n.parent() {
        match p.kind() {
            "preproc_else" | "preproc_elif" => {
                in_else = true;
                n = p;
            }
            "preproc_ifdef" => {
                let name = p
                    .child_by_field_name("name")
                    .and_then(|x| x.utf8_text(src).ok())
                    .unwrap_or("")
                    .to_string();
                return if in_else { Guard::NotDefined(name) } else { Guard::Defined(name) };
            }
            "preproc_if" => {
                // `#if defined(NAME)` — pull the NAME out of the condition.
                let cond = p
                    .child_by_field_name("condition")
                    .and_then(|x| x.utf8_text(src).ok())
                    .unwrap_or("");
                if let Some(name) = cond
                    .trim()
                    .strip_prefix("defined")
                    .map(|r| r.trim().trim_start_matches('(').trim_end_matches(')').trim())
                {
                    return if in_else {
                        Guard::NotDefined(name.to_string())
                    } else {
                        Guard::Defined(name.to_string())
                    };
                }
                return Guard::Always; // non-defined() condition: spike defers
            }
            _ => n = p,
        }
    }
    Guard::Always
}

#[cfg(test)]
#[path = "c_superpose_tests.rs"]
mod tests;
