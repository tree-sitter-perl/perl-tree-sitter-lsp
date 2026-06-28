//! SPIKE / PoC: multiple dispatch (C++ overload resolution) — feeling out
//! the design space for the multimethod seam (`docs/prompt-cpp-reparse.md`
//! "NEXT: multimethods"). C++ makes it concrete because overload
//! resolution *is* multiple dispatch: a call `f(a, b)` selects among the
//! declared `f` overloads by the ARGUMENT-TYPE TUPLE.
//!
//! This is the generalization of the overload Π-type (`overload_pi.rs`,
//! one operator) to N-ary dispatch on a tuple of arg types. The engine
//! seam it models: `ReturnExpr::UnionOnArgs { branches: Vec<(ArgGuard,
//! ReturnExpr)> }` is already a dispatch table; today `ArgGuard` is
//! arity-based. Multimethods = an `ArgGuard` that matches argument TYPES.
//! So the dispatch table here = the overload set; arity is the first
//! coordinate (it pre-filters), types refine it, and RANKING (exact beats
//! conversion) is the part that makes it non-trivial — the thing this PoC
//! exists to surface.
//!
//! What the PoC deliberately models vs. punts:
//!   - models: arity pre-filter, exact-vs-convertible RANKING, ambiguity,
//!     no-viable-overload. These are the dispatch SHAPE.
//!   - punts (the deep/Clang tier): the full conversion lattice (user
//!     conversions, qualification, references), template overloads, ADL,
//!     class subtyping. Class match here is exact-name only.
//! Not wired into the pipeline; measured by `cpp_multidispatch_tests.rs`.

use std::collections::HashMap;
use tree_sitter::{Node, Query, QueryCursor, StreamingIterator, Tree};

/// A coarse type for dispatch — enough lattice to explore ranking without
/// the full C++ conversion machinery.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Int,
    Double,
    Bool,
    Class(String),
    Void,
    Other(String),
}

impl Ty {
    pub fn parse(text: &str) -> Ty {
        match text.trim() {
            "int" | "long" | "short" | "unsigned" | "char" | "size_t" => Ty::Int,
            "double" | "float" => Ty::Double,
            "bool" => Ty::Bool,
            "void" => Ty::Void,
            t if t.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_') && !t.contains(' ') => {
                Ty::Class(t.to_string())
            }
            t => Ty::Other(t.to_string()),
        }
    }
    fn is_numeric(&self) -> bool {
        matches!(self, Ty::Int | Ty::Double | Ty::Bool)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

#[derive(Debug, Default, Clone)]
pub struct OverloadSet {
    pub sigs: Vec<Signature>,
}

/// The outcome of resolving one call against an overload set — the same
/// three answers C++ overload resolution can give.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dispatch {
    Resolved(Signature),
    /// Two or more overloads tie for the best conversion rank.
    Ambiguous(Vec<Signature>),
    NoViableOverload,
}

/// Conversion rank of binding `arg` to a `param` slot. Lower is better.
/// `None` = no conversion exists (not viable). The ladder is deliberately
/// short — exact(0) < numeric-convertible(1) — because that single step
/// is enough to make "exact beats conversion" observable, which is the
/// design point. The real C++ ladder has ~7 rungs; the SHAPE is this.
fn rank(param: &Ty, arg: &Ty) -> Option<u32> {
    if param == arg {
        return Some(0);
    }
    if param.is_numeric() && arg.is_numeric() {
        return Some(1); // int<->double<->bool — a standard conversion
    }
    None // classes don't convert here (no subtyping in the PoC)
}

/// Resolve a call with `args` against an overload set. Arity is the first
/// filter (the `UnionOnArgs` arity guard); then each viable overload gets
/// a total conversion rank; the unique minimum wins, a tie is `Ambiguous`,
/// none viable is `NoViableOverload`.
pub fn dispatch(set: &OverloadSet, args: &[Ty]) -> Dispatch {
    let mut scored: Vec<(u32, &Signature)> = Vec::new();
    for sig in &set.sigs {
        if sig.params.len() != args.len() {
            continue; // arity guard
        }
        let mut total = 0u32;
        let mut viable = true;
        for (p, a) in sig.params.iter().zip(args) {
            match rank(p, a) {
                Some(r) => total += r,
                None => {
                    viable = false;
                    break;
                }
            }
        }
        if viable {
            scored.push((total, sig));
        }
    }
    let Some(&(best, _)) = scored.iter().min_by_key(|(t, _)| *t) else {
        return Dispatch::NoViableOverload;
    };
    let winners: Vec<&Signature> = scored.iter().filter(|(t, _)| *t == best).map(|(_, s)| *s).collect();
    match winners.as_slice() {
        [only] => Dispatch::Resolved((*only).clone()),
        many => Dispatch::Ambiguous(many.iter().map(|s| (*s).clone()).collect()),
    }
}

const OVERLOAD_QUERY: &str = r#"
(declaration
  type: (_) @ret
  declarator: (function_declarator
    declarator: (identifier) @name
    parameters: (parameter_list) @params))
(function_definition
  type: (_) @ret
  declarator: (function_declarator
    declarator: (identifier) @name
    parameters: (parameter_list) @params))
"#;

/// Build the overload sets in `tree`, keyed by function name.
pub fn collect_overloads(tree: &Tree, src: &[u8]) -> HashMap<String, OverloadSet> {
    let lang = tree.language();
    let query = Query::new(&lang, OVERLOAD_QUERY).expect("overload query");
    let names: Vec<&str> = query.capture_names().to_vec();
    let mut out: HashMap<String, OverloadSet> = HashMap::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&query, tree.root_node(), src);
    while let Some(m) = it.next() {
        let mut name = None;
        let mut ret = Ty::Void;
        let mut params_node: Option<Node> = None;
        for c in m.captures {
            match names[c.index as usize] {
                "name" => name = c.node.utf8_text(src).ok().map(str::to_string),
                "ret" => ret = Ty::parse(c.node.utf8_text(src).unwrap_or("")),
                "params" => params_node = Some(c.node),
                _ => {}
            }
        }
        let (Some(name), Some(params_node)) = (name, params_node) else { continue };
        let params = param_types(params_node, src);
        out.entry(name).or_default().sigs.push(Signature { params, ret });
    }
    out
}

/// Ordered parameter types from a `parameter_list`. `(void)` → zero params.
fn param_types(list: Node, src: &[u8]) -> Vec<Ty> {
    let mut cur = list.walk();
    let tys: Vec<Ty> = list
        .named_children(&mut cur)
        .filter(|n| n.kind() == "parameter_declaration")
        .filter_map(|p| p.child_by_field_name("type").map(|t| Ty::parse(t.utf8_text(src).unwrap_or(""))))
        .collect();
    if tys == [Ty::Void] {
        vec![]
    } else {
        tys
    }
}

#[cfg(test)]
#[path = "cpp_multidispatch_tests.rs"]
mod tests;
