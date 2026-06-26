//! SPIKE: overload-on-param as a Π-type that monomorphizes at the call
//! site. A pure type-algebra model (no grammar) of the one thing the
//! production `ReturnExpr` doesn't yet carry.
//!
//! The engine's `ReturnExpr` (witnesses.rs) is already a dependent return
//! type — conceptually `(receiver, arity, args) -> InferredType`,
//! evaluated against `q.receiver` / `q.arity_hint`. Its free variables:
//! `Receiver` (∀R.→R, fluent), `UnionOnArgs` (arg-indexed dispatch), and
//! `Operator(ParametricOp)` (currently only `RowOf`). What's missing for
//! `sub add { $_[0] + $_[1] }` is:
//!   - a free variable for the n-th ARGUMENT's type (`Arg(n)`, the
//!     sibling of `Receiver`), and
//!   - a parametric operator for an overloadable binop
//!     (`ParametricOp::BinOp`), evaluated with the call's arg types.
//!
//! This module models exactly those, so we can watch the monomorphize
//! happen: a function whose body is `arg0 OP arg1` is a Π-type;
//!   - no call context / numeric args → the operator's mono-type
//!     (the "mostly collapse to int"), the operator-evidence default;
//!   - an argument whose class OVERLOADS the operator → that overload's
//!     return type — and if that return is unknown on the first pass, it
//!     comes back `None` and resolves LATE (the worklist edge-chase),
//!     exactly the "provided late" case.
//!
//! Not wired into the pipeline; this is the representation spike that
//! says what a real `ReturnExpr::Arg` + `ParametricOp::BinOp` would do.

use crate::file_analysis::InferredType;
use std::collections::HashMap;

/// A return body parametric over the argument types — the Π-type
/// `(arg types) -> InferredType`. Mirrors `ReturnExpr`; `Arg` is the
/// missing free variable, `BinOp` the missing `ParametricOp`.
#[derive(Debug, Clone)]
pub enum PiBody {
    Concrete(InferredType),
    /// The n-th argument's type — substituted at the call site.
    Arg(usize),
    /// An overloadable binary operator over two sub-bodies.
    BinOp(&'static str, Box<PiBody>, Box<PiBody>),
}

/// What an overloaded operator returns. `Unresolved` = overloaded but the
/// return isn't typed yet (first pass) → resolves late. `Body` = the
/// overload sub's return body, itself possibly PARAMETRIC — which is what
/// makes overload CYCLES possible (`A::(+` returning `$_[0] + $_[1]`).
#[derive(Debug, Clone)]
pub enum OverloadRet {
    Unresolved,
    Body(PiBody),
}

/// Overload facts: `(class, op) → OverloadRet`. Absent key = not overloaded.
#[derive(Debug, Default)]
pub struct OverloadTable {
    ret: HashMap<(String, &'static str), OverloadRet>,
}

impl OverloadTable {
    /// Convenience: a concrete return (`Some`) or unresolved (`None`).
    pub fn set(&mut self, class: &str, op: &'static str, ret: Option<InferredType>) {
        let r = match ret {
            Some(t) => OverloadRet::Body(PiBody::Concrete(t)),
            None => OverloadRet::Unresolved,
        };
        self.ret.insert((class.to_string(), op), r);
    }
    /// A parametric return body — the overload sub returns an expression
    /// over its own arguments (the path to a cycle).
    pub fn set_body(&mut self, class: &str, op: &'static str, body: PiBody) {
        self.ret.insert((class.to_string(), op), OverloadRet::Body(body));
    }
    fn lookup(&self, class: &str, op: &'static str) -> Option<&OverloadRet> {
        self.ret.get(&(class.to_string(), op))
    }
}

/// The operator's mono-type — what Perl's syntax leaks when no overload
/// is in play. Arithmetic → Numeric, concat/repeat → String. This is the
/// "mostly collapse to int" default (the operator-evidence heuristic).
fn op_mono_type(op: &str) -> InferredType {
    match op {
        "." | "x" => InferredType::String,
        _ => InferredType::Numeric, // + - * / and comparisons
    }
}

impl PiBody {
    /// Monomorphize against concrete argument types. Empty / short `args`
    /// = unknown args (e.g. hover with no call site) → the operator
    /// default. `None` = an overload applies but its return is unresolved
    /// (provided late) OR an overload cycle was detected (bottoms out
    /// honestly, never loops).
    pub fn monomorphize(&self, args: &[InferredType], ov: &OverloadTable) -> Option<InferredType> {
        self.eval(args, ov, &mut std::collections::HashSet::new())
    }

    /// `resolving` is the dispatcher's seen-set: the `(class, op)` overload
    /// returns currently being evaluated. Re-entering one is a cycle →
    /// `None`. This is the only thing standing between a self-referential
    /// overload and an infinite loop.
    fn eval(
        &self,
        args: &[InferredType],
        ov: &OverloadTable,
        resolving: &mut std::collections::HashSet<(String, &'static str)>,
    ) -> Option<InferredType> {
        match self {
            PiBody::Concrete(t) => Some(t.clone()),
            PiBody::Arg(n) => Some(args.get(*n).cloned().unwrap_or(InferredType::Numeric)),
            PiBody::BinOp(op, l, r) => {
                let lt = l.eval(args, ov, resolving);
                let rt = r.eval(args, ov, resolving);
                // an operand whose class overloads OP dominates (class-
                // identity dominates rep — FrameworkAwareTypeFold's rule).
                for operand in [&lt, &rt] {
                    if let Some(InferredType::ClassName(c)) = operand {
                        match ov.lookup(c, op) {
                            None => continue,                          // not overloaded
                            Some(OverloadRet::Unresolved) => return None, // late
                            Some(OverloadRet::Body(body)) => {
                                let key = (c.clone(), *op);
                                if !resolving.insert(key.clone()) {
                                    return None; // CYCLE — bottom out, don't loop
                                }
                                // the overload sub's args are the operands.
                                let oargs: Vec<InferredType> =
                                    [lt.clone(), rt.clone()].into_iter().flatten().collect();
                                let res = body.eval(&oargs, ov, resolving);
                                resolving.remove(&key);
                                return res;
                            }
                        }
                    }
                }
                // no overloading operand → the operator's mono-type.
                Some(op_mono_type(op))
            }
        }
    }
}

#[cfg(test)]
#[path = "overload_pi_tests.rs"]
mod tests;
