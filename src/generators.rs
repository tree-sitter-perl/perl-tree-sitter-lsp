//! Productive Perl projection — the generator PROJECTION ENGINE.
//!
//! A plugin-declared symbol generator (`crate::plugin::GeneratorDef`) is a
//! helper whose call synthesizes a GROUP of symbols from its literal args:
//! `make_crud_helpers('user')` projects a `user_id` accessor +
//! `get_user`/`set_user` methods. This is the generalization of Moo `has`
//! synthesis — a call witness projected into a symbol group, every member
//! tracing (provenance) to the call site.
//!
//! STRUCTURAL PROJECTION, never execution. Core collects the witness (the
//! call site + its literal args — `builder.rs`, rule #1), substitutes the
//! args into the plugin's abstract templates, and runs this worklist. A
//! `Generate` action queues a NESTED generator call (the worklist's fuel); a
//! per-call seen-set bounds a generator that generates itself — we never run
//! Perl, so we never diverge.
//!
//! The engine is provenance-generic (`P`): the builder threads
//! `file_analysis::Span` (the root call span) so goto-def / rename on a
//! synthesized symbol land on the generator invocation; the unit tests below
//! thread a cheap token. Provenance CHAINS through nested generation — a
//! transitively-synthesized symbol carries the ROOT call site's `P`.

use std::collections::{HashMap, HashSet};

use crate::plugin::GeneratorDef;

/// A generator call site to project — the witness. `prov` is the provenance
/// payload threaded onto every synthesized member (and chained through nested
/// generation to the root).
#[derive(Debug, Clone)]
pub struct GenWitness<P> {
    pub generator: String,
    pub args: Vec<String>,
    pub prov: P,
}

/// A symbol produced by projecting a generator over a witness.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenSymbol<P> {
    pub name: String,
    /// Plugin-declared kind string (`"method"` / `"accessor"` / `"sub"`); the
    /// builder maps it to a `SymKind`.
    pub kind: String,
    pub from_generator: String,
    pub prov: P,
}

/// Project ONE generator call site into its synthesized symbol group.
///
/// A fixpoint worklist: `Generate` actions enqueue nested generator calls
/// (carrying the root's `prov`); the seen-set — keyed by `(generator, args)`,
/// scoped to THIS call — drops a generator that re-queues itself with the same
/// args, so a recursive generator terminates bounded rather than hanging.
///
/// One call site = one `project` call = one independent seen-set, so two
/// sites that invoke the same generator with the same args still synthesize
/// two distinct groups (each call gets its own provenance).
pub fn project<P: Clone>(
    defs: &HashMap<String, GeneratorDef>,
    root: GenWitness<P>,
) -> Vec<GenSymbol<P>> {
    let mut seen: HashSet<(String, Vec<String>)> = HashSet::new();
    let mut queue: Vec<GenWitness<P>> = vec![root];
    let mut out = Vec::new();
    while let Some(w) = queue.pop() {
        if !seen.insert((w.generator.clone(), w.args.clone())) {
            continue;
        }
        let Some(def) = defs.get(&w.generator) else { continue };
        let subst: HashMap<&str, &str> = def
            .params
            .iter()
            .map(String::as_str)
            .zip(w.args.iter().map(String::as_str))
            .collect();
        for action in &def.actions {
            if let Some(tmpl) = &action.emit {
                out.push(GenSymbol {
                    name: interpolate(tmpl, &subst),
                    kind: action.kind.clone().unwrap_or_else(|| "sub".to_string()),
                    from_generator: w.generator.clone(),
                    prov: w.prov.clone(),
                });
            } else if let Some(generator) = &action.generate {
                queue.push(GenWitness {
                    generator: generator.clone(),
                    args: action.args.iter().map(|t| interpolate(t, &subst)).collect(),
                    prov: w.prov.clone(), // provenance chains to the root call site
                });
            }
        }
    }
    out
}

/// `${param}` interpolation against the substitution. An unknown key expands
/// to empty (a generator template referencing a param the call didn't pass).
fn interpolate(tmpl: &str, subst: &HashMap<&str, &str>) -> String {
    let mut out = String::with_capacity(tmpl.len());
    let bytes = tmpl.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'$' && bytes.get(i + 1) == Some(&b'{') {
            if let Some(end) = tmpl[i + 2..].find('}') {
                let key = &tmpl[i + 2..i + 2 + end];
                out.push_str(subst.get(key).copied().unwrap_or(""));
                i += 2 + end + 1;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plugin::{GeneratorAction, GeneratorDef};

    fn emit(name: &str, kind: &str) -> GeneratorAction {
        GeneratorAction { emit: Some(name.into()), kind: Some(kind.into()), ..Default::default() }
    }
    fn gen(name: &str, args: &[&str]) -> GeneratorAction {
        GeneratorAction {
            generate: Some(name.into()),
            args: args.iter().map(|s| s.to_string()).collect(),
            ..Default::default()
        }
    }
    fn def(name: &str, params: &[&str], actions: Vec<GeneratorAction>) -> (String, GeneratorDef) {
        (
            name.into(),
            GeneratorDef {
                name: name.into(),
                params: params.iter().map(|s| s.to_string()).collect(),
                actions,
            },
        )
    }
    fn wit(generator: &str, args: &[&str], prov: u32) -> GenWitness<u32> {
        GenWitness {
            generator: generator.into(),
            args: args.iter().map(|s| s.to_string()).collect(),
            prov,
        }
    }

    fn crud_defs() -> HashMap<String, GeneratorDef> {
        HashMap::from([def(
            "make_crud_helpers",
            &["name"],
            vec![emit("${name}_id", "accessor"), emit("get_${name}", "method"), emit("set_${name}", "method")],
        )])
    }

    #[test]
    fn projection_synthesizes_the_group_with_provenance() {
        let out = project(&crud_defs(), wit("make_crud_helpers", &["user"], 7));
        let names: Vec<&str> = out.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"user_id"), "{names:?}");
        assert!(names.contains(&"get_user"), "{names:?}");
        assert!(names.contains(&"set_user"), "{names:?}");
        let id = out.iter().find(|s| s.name == "user_id").unwrap();
        assert_eq!(id.prov, 7, "synthesized symbol traces to the call witness");
        assert_eq!(id.kind, "accessor");
    }

    #[test]
    fn two_call_sites_two_distinct_groups() {
        let defs = crud_defs();
        let user = project(&defs, wit("make_crud_helpers", &["user"], 1));
        let post = project(&defs, wit("make_crud_helpers", &["post"], 2));
        assert_eq!(user.iter().find(|s| s.name == "user_id").unwrap().prov, 1);
        assert_eq!(post.iter().find(|s| s.name == "post_id").unwrap().prov, 2);
    }

    #[test]
    fn nested_generation_runs_the_worklist_chaining_provenance() {
        let defs = HashMap::from([
            def(
                "make_resource",
                &["thing"],
                vec![emit("${thing}_table", "accessor"), gen("make_crud_helpers", &["${thing}"])],
            ),
            def(
                "make_crud_helpers",
                &["name"],
                vec![emit("get_${name}", "method"), emit("set_${name}", "method")],
            ),
        ]);
        let out = project(&defs, wit("make_resource", &["widget"], 42));
        let names: HashSet<&str> = out.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains("widget_table"), "direct: {names:?}");
        assert!(names.contains("get_widget"), "transitive: {names:?}");
        assert!(names.contains("set_widget"), "transitive: {names:?}");
        // the transitively-synthesized symbol still traces to the ROOT call.
        assert_eq!(out.iter().find(|s| s.name == "get_widget").unwrap().prov, 42);
    }

    #[test]
    fn recursive_generator_terminates_via_seen_set() {
        let defs = HashMap::from([def(
            "loop_gen",
            &["x"],
            vec![emit("sym_${x}", "method"), gen("loop_gen", &["${x}"])],
        )]);
        let out = project(&defs, wit("loop_gen", &["a"], 0));
        assert_eq!(out.iter().filter(|s| s.name == "sym_a").count(), 1, "bounded: {out:?}");
    }

    #[test]
    fn unknown_generator_synthesizes_nothing() {
        let out = project(&crud_defs(), wit("regular_function", &["user"], 0));
        assert!(out.is_empty(), "no generator → no synthesis: {out:?}");
    }
}
