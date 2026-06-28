//! SPIKE / PoC: templates as WITNESSES, not execution — the second of the
//! two C++-moat builds (`~/personal/resume/research-static-analysis.md`,
//! "C++ SEMANTIC MOAT — DECOMPOSED": "the best call in the plan").
//!
//! A template has no single meaning — one per instantiation. The naive
//! analyzer EXECUTES the template metaprogram (Turing-complete trap). The
//! right move (Rust's monomorphization, every codebase-specific analyzer):
//! **don't execute — collect the concrete type-args that actually appear
//! in the code (the WITNESSES) and project the body through each
//! substitution.** Three facts the research names, all demonstrated here:
//!   1. witnesses-not-execution — `process<Widget>` / `process<Gadget>`
//!      are collected from instantiation sites; the body is never run.
//!   2. it's a FIXPOINT WORKLIST — a `helper<T>` inside `process`'s body
//!      goes concrete (`helper<Widget>`) only once `process<Widget>` is
//!      witnessed, transitively. Same worklist shape as the reparse loop,
//!      macro expansion, and the overload cycle — with the SAME seen-set
//!      for termination (the recursive-template trap is bounded).
//!   3. dependent types dissolve at the witness — `T::value_type` becomes
//!      `Widget::value_type` once `T=Widget`; the `typename` problem
//!      evaporates.
//!
//! Cross-language note (why we care for Perl): a Perl plugin symbol
//! generator — "a helper that creates a group of helpers/tasks" — is the
//! SAME lane: STRUCTURAL PROJECTION over witnesses, NOT execution. The
//! generator's definition is abstract; at each call site (the witness)
//! you substitute the literal args as if written there, then run the
//! projected form through plugin-based symbol SYNTHESIS — symbolic, not
//! run. The only language-specific piece is the projection function: C++
//! substitutes type-params and re-runs name/overload resolution; Perl
//! substitutes literal args and runs the plugin's declarative synthesis.
//! Witness-collection, substitution, the worklist, and the seen-set spine
//! are shared. (Execute-against-a-probe —
//! `docs/adr/importbase-plugin-gen.md` — is the FALLBACK for opaque
//! generators that can't be projected, e.g. Import::Base coderefs; not the
//! primary design.) Not wired into the pipeline; measured by
//! `cpp_templates_tests.rs`.

use std::collections::{HashMap, HashSet};
use tree_sitter::{Node, Tree};

/// A type argument in an instantiation: a concrete type, or one of the
/// enclosing template's parameters (which only goes concrete on
/// projection).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeArg {
    Concrete(String),
    Param(String),
}

/// `name<args>` — a use of a template.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instantiation {
    pub template: String,
    pub args: Vec<TypeArg>,
}

#[derive(Debug, Clone, Default)]
pub struct Template {
    pub name: String,
    pub params: Vec<String>,
    /// `(param, member)` for each `typename param::member` in the body.
    pub dependent_types: Vec<(String, String)>,
    /// `name<...>` uses inside the body (args may be Param or Concrete).
    pub body_instantiations: Vec<Instantiation>,
}

/// What a concrete instantiation resolves to once projected — the
/// dependent types dissolved to concrete `Class::member` names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolved {
    pub inst: Instantiation,
    pub concrete_types: Vec<String>, // e.g. "Widget::value_type"
}

// ---- collection (the witness half: real parse, no execution) ----

pub fn collect_templates(tree: &Tree, src: &[u8]) -> HashMap<String, Template> {
    let mut out = HashMap::new();
    for tmpl in descendants(tree.root_node(), "template_declaration") {
        let params = type_params(tmpl, src);
        let Some(func) = first_descendant(tmpl, "function_definition") else { continue };
        // the NAME is the function_declarator's own `declarator` field (an
        // identifier) — NOT any descendant identifier (params have those).
        let Some(name) = func
            .child_by_field_name("declarator")
            .and_then(|fd| fd.child_by_field_name("declarator"))
            .and_then(|i| i.utf8_text(src).ok())
            .map(str::to_string)
        else {
            continue;
        };
        let body = func.child_by_field_name("body").unwrap_or(func);
        let dependent_types = dependent_types_in(body, src);
        let body_instantiations =
            instantiations_in(body, src).into_iter().map(|i| classify(i, &params)).collect();
        out.insert(
            name.clone(),
            Template { name, params, dependent_types, body_instantiations },
        );
    }
    out
}

/// Seed witnesses: `name<Concrete...>` uses that appear OUTSIDE any
/// template body (regular code) — the concrete instantiations the worklist
/// starts from.
pub fn seed_instantiations(tree: &Tree, src: &[u8]) -> Vec<Instantiation> {
    let mut seeds = Vec::new();
    collect_seeds(tree.root_node(), src, &mut seeds);
    seeds
}

fn collect_seeds(node: Node, src: &[u8], out: &mut Vec<Instantiation>) {
    if node.kind() == "template_declaration" {
        return; // inside a template body → dependent, not a seed
    }
    if node.kind() == "template_function" {
        if let Some(raw) = read_template_function(node, src) {
            let inst = classify(raw, &[]); // no enclosing params → all Concrete
            out.push(inst);
        }
    }
    let mut cur = node.walk();
    for c in node.children(&mut cur) {
        collect_seeds(c, src, out);
    }
}

// ---- projection + the fixpoint worklist ----

/// Monomorphize to a fixpoint. Start from `seeds`; for each concrete
/// instantiation, project its template's body — dependent types dissolve,
/// nested instantiations become new concrete witnesses — and chase those
/// transitively. The seen-set bounds recursive templates (the
/// Turing-complete trap): we never execute, so we never diverge.
pub fn instantiate_to_fixpoint(
    templates: &HashMap<String, Template>,
    seeds: &[Instantiation],
) -> Vec<Resolved> {
    let mut seen: HashSet<Instantiation> = HashSet::new();
    let mut queue: Vec<Instantiation> = seeds.to_vec();
    let mut out = Vec::new();
    while let Some(inst) = queue.pop() {
        if !seen.insert(inst.clone()) {
            continue; // already monomorphized — bounds recursion
        }
        let Some(tmpl) = templates.get(&inst.template) else {
            out.push(Resolved { inst, concrete_types: vec![] }); // opaque template
            continue;
        };
        let subst = substitution(&tmpl.params, &inst.args);
        // dependent types dissolve: `param::member` → `Concrete::member`
        let concrete_types = tmpl
            .dependent_types
            .iter()
            .map(|(p, m)| format!("{}::{}", subst.get(p).cloned().unwrap_or_else(|| p.clone()), m))
            .collect();
        // nested instantiations become new concrete witnesses
        for bi in &tmpl.body_instantiations {
            let args: Vec<TypeArg> = bi
                .args
                .iter()
                .map(|a| match a {
                    TypeArg::Param(p) => TypeArg::Concrete(subst.get(p).cloned().unwrap_or_else(|| p.clone())),
                    c => c.clone(),
                })
                .collect();
            queue.push(Instantiation { template: bi.template.clone(), args });
        }
        out.push(Resolved { inst, concrete_types });
    }
    out
}

fn substitution(params: &[String], args: &[TypeArg]) -> HashMap<String, String> {
    params
        .iter()
        .zip(args)
        .filter_map(|(p, a)| match a {
            TypeArg::Concrete(c) => Some((p.clone(), c.clone())),
            TypeArg::Param(_) => None,
        })
        .collect()
}

fn classify(raw: (String, Vec<String>), params: &[String]) -> Instantiation {
    let args = raw
        .1
        .into_iter()
        .map(|t| if params.contains(&t) { TypeArg::Param(t) } else { TypeArg::Concrete(t) })
        .collect();
    Instantiation { template: raw.0, args }
}

// ---- small typed-tree helpers (spike-local node navigation) ----

fn type_params(tmpl: Node, src: &[u8]) -> Vec<String> {
    let Some(list) = first_descendant(tmpl, "template_parameter_list") else { return vec![] };
    descendants(list, "type_parameter_declaration")
        .into_iter()
        .filter_map(|d| first_descendant(d, "type_identifier").and_then(|t| t.utf8_text(src).ok()))
        .map(str::to_string)
        .collect()
}

fn dependent_types_in(body: Node, src: &[u8]) -> Vec<(String, String)> {
    descendants(body, "dependent_type")
        .into_iter()
        .filter_map(|d| {
            let q = first_descendant(d, "qualified_identifier")?;
            let scope = q.child_by_field_name("scope")?.utf8_text(src).ok()?.to_string();
            let name = q.child_by_field_name("name")?.utf8_text(src).ok()?.to_string();
            Some((scope, name))
        })
        .collect()
}

fn instantiations_in(body: Node, src: &[u8]) -> Vec<(String, Vec<String>)> {
    descendants(body, "template_function")
        .into_iter()
        .filter_map(|n| read_template_function(n, src))
        .collect()
}

/// `(name, [type-arg texts])` for a `template_function` node.
fn read_template_function(node: Node, src: &[u8]) -> Option<(String, Vec<String>)> {
    let name = node.child_by_field_name("name")?.utf8_text(src).ok()?.to_string();
    let args = node.child_by_field_name("arguments")?;
    let mut cur = args.walk();
    let targs: Vec<String> = args
        .named_children(&mut cur)
        .filter(|n| n.kind() == "type_descriptor")
        .filter_map(|td| first_descendant(td, "type_identifier").and_then(|t| t.utf8_text(src).ok()))
        .map(str::to_string)
        .collect();
    Some((name, targs))
}

fn descendants<'a>(node: Node<'a>, kind: &str) -> Vec<Node<'a>> {
    let mut out = Vec::new();
    let mut cur = node.walk();
    let mut stack = vec![node];
    while let Some(n) = stack.pop() {
        if n.kind() == kind && n != node {
            out.push(n);
        }
        for c in n.children(&mut cur) {
            stack.push(c);
        }
    }
    out
}

fn first_descendant<'a>(node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cur = node.walk();
    let mut stack = vec![node];
    while let Some(n) = stack.pop() {
        if n.kind() == kind && n != node {
            return Some(n);
        }
        for c in n.children(&mut cur) {
            stack.push(c);
        }
    }
    None
}

#[cfg(test)]
#[path = "cpp_templates_tests.rs"]
mod tests;
