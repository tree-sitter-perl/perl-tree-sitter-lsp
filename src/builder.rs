//! Single-walk tree-sitter visitor that builds a FileAnalysis.
//!
//! One depth-first walk populates scopes, symbols, refs, type constraints,
//! and fold ranges. Post-passes resolve hash key owners and variable refs.

use std::sync::{Arc, OnceLock};

use tree_sitter::{Node, Point, Tree};

use crate::file_analysis::*;
use crate::plugin::{self, PluginRegistry};

/// Process-wide plugin registry, built once with the bundled Rhai plugins
/// (plus anything discovered under `$PERL_LSP_PLUGIN_DIR`). All `build()`
/// calls share it; tests that need isolation use `build_with_plugins()`.
pub fn default_plugin_registry() -> Arc<PluginRegistry> {
    static REG: OnceLock<Arc<PluginRegistry>> = OnceLock::new();
    REG.get_or_init(|| {
        let engine = Arc::new(plugin::rhai_host::make_engine());
        let mut reg = PluginRegistry::new();
        for p in plugin::rhai_host::load_bundled(engine.clone()) {
            reg.register(p);
        }
        if let Ok(dir) = std::env::var("PERL_LSP_PLUGIN_DIR") {
            let path = std::path::PathBuf::from(dir);
            for p in plugin::rhai_host::load_plugin_dir(&path, engine) {
                reg.register(p);
            }
        }
        Arc::new(reg)
    }).clone()
}

/// Single CST walk that powers the post-walk `ChainTypingReducer`
/// (Phase 5 of the type-inference worklist refactor). Replaces three
/// independent tree walks at the old steps 7/8/10:
///
/// - `assignment_nodes` — every `assignment_expression`, used to type
///   `my $X = <rhs>` (and bare `$X = …`) via `resolve_invocant_class_tree`.
/// - `return_nodes` — every `return_expression`, indexed by span so
///   the return-arm refresh can match it back to a `ReturnInfo`.
/// - `invocant_nodes` — every `method_call_expression`'s invocant,
///   indexed by span so the post-fold invocant-class refresh can
///   find the right node for a `MethodCall` ref's `invocant_span`.
/// - `method_call_args` — every `method_call_expression`'s args
///   node, indexed by the call-expression span. Lets the post-walk
///   keys-as-HashKeyAccess emission look up the args from a
///   MethodCall ref instead of stashing them on the ref itself or
///   running emit at walk time (where invocant_class is still
///   getting refined).
///
/// Tree-sitter-perl `arguments` field shapes encountered in the
/// wild (notes for parser-side adjustments — `args` is the value
/// of `child_by_field_name("arguments")`):
///
/// | Call shape | `args.kind()`                 | Notes |
/// | --- | --- | --- |
/// | `f()`                | (no `arguments` field)        | empty arglist; field absent |
/// | `f($x)`              | `scalar`                      | single non-literal arg, unwrapped |
/// | `f('x')`             | `string_literal`              | single string, unwrapped |
/// | `f('x', 'y')`        | `list_expression`             | flat positional list |
/// | `f(k => v)`          | `list_expression`             | fat-comma is flat in this list |
/// | `Foo->new(k => v)`   | `parenthesized_expression`    | constructor wraps in parens |
/// | `f({k => v})`        | `anonymous_hash_expression`   | hashref-arg shape — DBIC's `search`/`find`. The hash's children are a `list_expression` of the k=>v pairs |
/// | `qw(a b c)`          | `quoted_word_list`            | DBIC `add_columns(qw(...))` shape |
///
/// Recursive-into-wrapper rule: emitters typically walk one level
/// past `parenthesized_expression` / `anonymous_hash_expression`
/// to the enclosed `list_expression`. The k=>v iteration logic
/// expects a flat positional list; nested wrappers want unwrapping
/// at the call site, not in the arg-walker.
///
/// Built once per `build_with_plugins_inner` call and consumed by both
/// `ChainPassMode::PreFold` and `ChainPassMode::PostFold` invocations
/// of the reducer.
struct ChainTypingIndex<'a> {
    assignment_nodes: Vec<Node<'a>>,
    return_nodes: std::collections::HashMap<(Point, Point), Node<'a>>,
    invocant_nodes: std::collections::HashMap<(Point, Point), Node<'a>>,
    method_call_args: std::collections::HashMap<(Point, Point), Node<'a>>,
}

/// Which chain-typing tasks the reducer should apply on this call.
///
/// `PreFold` runs between the two `resolve_return_types` calls —
/// assignments and return arms feed the second fold (assignments via
/// `var_type_via_bag` for `return $var`; return arms directly through
/// `return_infos`). Invocants are query-time outputs (`Ref.invocant_class`)
/// and don't influence the fold, so they wait until after every sub
/// return type is resolved.
///
/// `PostFold` runs once after the second `resolve_return_types` and
/// types method-call invocants (e.g. the `get_foo` in `get_foo()->bar()`)
/// using the now-final symbol table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ChainPassMode {
    PreFold,
    PostFold,
}

/// Walk the tree once, indexing the three node kinds the chain-typing
/// reducer cares about. Pure: reads only tree-sitter structural data,
/// no Builder state. Same recursion shape (depth-first via
/// `named_child(i)`) the three former independent walks all used.
fn build_chain_typing_index<'a>(tree: &'a Tree) -> ChainTypingIndex<'a> {
    let mut idx = ChainTypingIndex {
        assignment_nodes: Vec::new(),
        return_nodes: std::collections::HashMap::new(),
        invocant_nodes: std::collections::HashMap::new(),
        method_call_args: std::collections::HashMap::new(),
    };
    fn walk<'t>(node: Node<'t>, idx: &mut ChainTypingIndex<'t>) {
        match node.kind() {
            "assignment_expression" => {
                idx.assignment_nodes.push(node);
            }
            "return_expression" => {
                idx.return_nodes
                    .insert((node.start_position(), node.end_position()), node);
            }
            "method_call_expression" => {
                if let Some(inv) = node.child_by_field_name("invocant") {
                    idx.invocant_nodes
                        .insert((inv.start_position(), inv.end_position()), inv);
                }
                if let Some(args) = node.child_by_field_name("arguments") {
                    idx.method_call_args
                        .insert((node.start_position(), node.end_position()), args);
                }
            }
            _ => {}
        }
        for i in 0..node.named_child_count() {
            if let Some(c) = node.named_child(i) {
                walk(c, idx);
            }
        }
    }
    walk(tree.root_node(), &mut idx);
    idx
}

/// Build a FileAnalysis from a parsed tree in a single walk.
pub fn build(tree: &Tree, source: &[u8]) -> FileAnalysis {
    build_with_plugins(tree, source, default_plugin_registry())
}

/// Build with a caller-provided plugin registry. Tests use this to swap in
/// deterministic plugin sets; the global default is otherwise shared.
pub fn build_with_plugins(
    tree: &Tree,
    source: &[u8],
    plugins: Arc<PluginRegistry>,
) -> FileAnalysis {
    build_with_plugins_inner(tree, source, plugins, false)
}

/// Test-only entry: build the file, then re-run the worklist fold
/// driver (`fold_to_fixed_point`) one extra time before finalizing.
///
/// Since Phase 6, the fold is fully idempotent: the resulting
/// `FileAnalysis` is byte-identical to a plain `build_with_plugins(...)`
/// call — same `type_provenance`, same `sub_return_type_at_arity`
/// answers, same witness counts. The two re-emittable passes inside
/// `resolve_return_types` (arity-return emission, call-binding
/// propagator) clear their prior outputs before re-emitting, so each
/// fact lands in the bag exactly once regardless of iteration count.
/// The `post_walk_fold_is_observably_idempotent` invariant test
/// asserts the answer-level guarantee directly.
#[cfg(test)]
pub(crate) fn build_with_plugins_extra_re_fold(
    tree: &Tree,
    source: &[u8],
    plugins: Arc<PluginRegistry>,
) -> FileAnalysis {
    build_with_plugins_inner(tree, source, plugins, true)
}

fn build_with_plugins_inner(
    tree: &Tree,
    source: &[u8],
    plugins: Arc<PluginRegistry>,
    extra_re_fold: bool,
) -> FileAnalysis {
    let mut b = Builder {
        source,
        scopes: Vec::new(),
        symbols: Vec::new(),
        refs: Vec::new(),
        deferred_var_types: Vec::new(),
        fold_ranges: Vec::new(),
        imports: Vec::new(),
        return_infos: Vec::new(),
        last_expr_span: std::collections::HashMap::new(),
        call_bindings: Vec::new(),
        method_call_bindings: Vec::new(),
        pod_texts: Vec::new(),
        package_parents: std::collections::HashMap::new(),
        package_uses: std::collections::HashMap::new(),
        sub_return_delegations: std::collections::HashMap::new(),
        resolved_returns: std::collections::HashMap::new(),
        framework_modes: std::collections::HashMap::new(),
        framework_imports: std::collections::HashSet::new(),
        constant_strings: std::collections::HashMap::new(),
        export: Vec::new(),
        export_ok: Vec::new(),
        plugin_namespaces: Vec::new(),
        type_provenance: std::collections::HashMap::new(),
        bag: crate::witnesses::WitnessBag::new(),
        unresolved_call_targets: Vec::new(),
        package_framework: std::collections::HashMap::new(),
        scope_stack: Vec::new(),
        current_package: None,
        next_scope_id: 0,
        next_symbol_id: 0,
        package_ranges: Vec::new(),
        open_statement_package: None,
        plugins,
        method_call_invocant: std::collections::HashMap::new(),
        parametric_emitted_refs: std::collections::HashSet::new(),
    };

    // Create file-level scope and walk
    let file_scope = b.push_scope(ScopeKind::File, node_to_span(tree.root_node()), None);
    b.visit_children(tree.root_node());
    b.pop_scope();
    let _ = file_scope;

    // Flush plugin-emitted VarType constraints now that every scope
    // has been pushed. Each uses scope_at on the declared anchor point
    // so a `$app->helper(... sub { my ($c) = @_; ... })` emission
    // lands inside the callback body rather than the outer file scope.
    let deferred = std::mem::take(&mut b.deferred_var_types);
    for d in deferred {
        let scope = b
            .scopes
            .iter()
            .rev()
            .find(|s| crate::file_analysis::contains_point(&s.span, d.at.start))
            .map(|s| s.id)
            .unwrap_or(ScopeId(0));
        b.push_type_constraint(TypeConstraint {
            variable: d.variable,
            scope,
            constraint_span: d.at,
            inferred_type: d.inferred_type,
        });
    }

    // Post-pass 1: resolve variable refs -> resolves_to
    b.resolve_variable_refs();

    // Post-pass 2: resolve hash key owners from type constraints
    b.resolve_hash_key_owners();

    // Compute per-package framework facts BEFORE return-type fold so
    // the bag-aware reducer has the right context. Mirrors the data
    // the framework-accessor synthesis already consumed during the walk.
    b.package_framework = b
        .framework_modes
        .iter()
        .map(|(pkg, mode)| {
            let ff = match mode {
                FrameworkMode::Moo => crate::witnesses::FrameworkFact::Moo,
                FrameworkMode::Moose => crate::witnesses::FrameworkFact::Moose,
                FrameworkMode::MojoBase => crate::witnesses::FrameworkFact::MojoBase,
            };
            (pkg.clone(), ff)
        })
        .collect();

    // Plugin `overrides()` manifests run first. They pin return
    // types inference can't reach (`Mojolicious::Routes::Route::_route`
    // returning $self via an array-slice idiom). Provenance is
    // recorded in `type_provenance` (PluginOverride) so
    // `--dump-package` can answer "why does this return X?".
    b.apply_type_overrides();

    // Post-walk bag-population pass: ref-derived facts that don't
    // need walk-time visibility — `HashRefAccess` observations from
    // `$v->{k}` refs and invocant-mutation facts from hash-key
    // writes. Variable witnesses for TCs and walk-time idiom witnesses
    // (branch arms, arity gating) are already in `b.bag` — pushed
    // live during the walk.
    b.populate_witness_bag();

    // Forward-reference resolution: walk-time `expr_payload` arms for
    // `function_call_expression` / `bareword` / `scoped_identifier` did
    // a `self.symbols.iter().find` against a partial symbol table.
    // Forward-defined callees (Perl's `sub a { b() } sub b {…}` pattern,
    // canonically Carp's `longmess` → `longmess_heavy`) silently
    // produced no witness. The walk queued each missing lookup; resolve
    // them now against the final symbol table and push the
    // `Expr(span) → Edge(Symbol(sid))` witness the walk would have.
    // See `docs/prompt-forward-reference-resolution.md`.
    b.resolve_forward_call_targets();

    // Phase 6: worklist driver. Replaces the manually-ordered
    // `fold → chain → fold → chain` sequence with one fixed-point
    // loop over chain typing + reducer dispatch. Each iteration runs
    // `ChainPassMode::PreFold` (assignment + return-arm refresh)
    // followed by `resolve_return_types`; the loop exits when the
    // snapshot of Sub/Method return types and bag length stops
    // moving. Invocant-class refresh runs once after the lattice
    // settles.
    //
    // The two re-emittable passes inside `resolve_return_types`
    // (arity-return witnesses, call-binding propagator) became
    // clear-and-emit in this same commit, so the bag stays canonical
    // regardless of how many iterations the loop runs — each fact
    // lands exactly once at the end. Chain typing's TC-existence
    // check keeps it idempotent on the same assignment span.
    //
    // For shallow files (no through-chain dependencies on inferred
    // sub return types) the loop terminates in two iterations: one
    // to derive the initial fold answer, one to confirm stability.
    // Deeper chains take more iterations; `MAX_FOLD_ITERATIONS`
    // (debug-only) catches dependency-tracking bugs that would
    // otherwise spin forever.
    let chain_idx = build_chain_typing_index(tree);
    b.fold_to_fixed_point(&chain_idx);

    // Test-only: re-run the worklist fold one more time to pin
    // idempotency. Production callers always pass `false`; only
    // `build_with_plugins_extra_re_fold` flips this on. Re-running
    // `fold_to_fixed_point` against a settled state should land in
    // 1 iteration (loop sees `prev == cur` immediately) and produce
    // a byte-identical FileAnalysis — including witness counts,
    // unlike the pre-Phase-6 pipeline.
    if extra_re_fold {
        b.fold_to_fixed_point(&chain_idx);
    }

    // Post-pass: emit `HashKeyAccess` refs for even-position stringy
    // args on every resolved `MethodCall` ref (`MooApp->new(name => 'alice')`,
    // helper-emitted controllers, etc.). Runs after `fold_to_fixed_point`
    // so `invocant_class` is canonical against the bag — was a walk-time
    // emission gated on the partially-resolved walk-time class, now it's
    // a single post-walk pass that joins refs to args via the chain
    // typing index.
    b.emit_method_call_arg_keys(&chain_idx);

    // Post-pass 5: fill in tail POD docs for subs that didn't get preceding doc
    b.resolve_tail_pod_docs();

    let bag = std::mem::take(&mut b.bag);
    let package_framework = std::mem::take(&mut b.package_framework);

    let mut fa = FileAnalysis::new(
        b.scopes,
        b.symbols,
        b.refs,
        b.fold_ranges,
        b.imports,
        b.call_bindings,
        b.package_parents,
        b.method_call_bindings,
        b.framework_imports,
        b.export,
        b.export_ok,
        b.plugin_namespaces,
        b.package_uses,
        b.type_provenance,
        b.package_ranges,
    );
    fa.package_framework = package_framework;
    fa.witnesses = bag;
    fa.witnesses.rebuild_index();
    // Finalize: run the legacy text-based MCB resolver as a fallback.
    // For every assignment the unified typer (run before
    // `resolve_return_types` above) couldn't handle, MCB fills in.
    // Cross-file enrichment also reuses MCB resolution without a tree.
    fa.finalize_post_walk();

    fa
}

impl<'a> Builder<'a> {
    /// Push a `TypeConstraint` shape into the witness bag — Variable
    /// `InferredType` + class-assertion / FirstParam observation when
    /// the type is a class identity. Walk-time and worklist callers go
    /// through here so `bag_query_variable` sees seeded types
    /// immediately. Mirrors `FileAnalysis::push_type_constraint`'s
    /// shape (the FA helper handles enrichment-time pushes after the
    /// builder's bag has been moved into the FA).
    pub(crate) fn push_type_constraint(&mut self, tc: TypeConstraint) {
        use crate::witnesses::{
            TypeObservation, Witness, WitnessAttachment, WitnessPayload, WitnessSource,
        };
        let TypeConstraint { variable, scope, constraint_span: span, inferred_type: ty } = tc;
        self.bag.push(Witness {
            attachment: WitnessAttachment::Variable { name: variable.clone(), scope },
            source: WitnessSource::Builder("type_constraint".into()),
            payload: WitnessPayload::InferredType(ty.clone()),
            span: Span { start: span.start, end: span.start },
        });
        match ty {
            InferredType::ClassName(n) => {
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Variable { name: variable, scope },
                    source: WitnessSource::Builder("type_constraint".into()),
                    payload: WitnessPayload::Observation(TypeObservation::ClassAssertion(n)),
                    span,
                });
            }
            InferredType::FirstParam { package } => {
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Variable { name: variable, scope },
                    source: WitnessSource::Builder("type_constraint".into()),
                    payload: WitnessPayload::Observation(TypeObservation::FirstParamInMethod {
                        package,
                    }),
                    span,
                });
            }
            _ => {}
        }
    }

    /// Post-walk pass: ref-derived facts that don't need walk-time
    /// visibility — `HashRefAccess` observations from `$v->{k}` refs
    /// and invocant-mutation facts on hash-key writes. Variable
    /// witnesses for TCs and walk-time idiom witnesses (branch arms,
    /// arity gating) are already in the bag — pushed live during the
    /// walk via `push_type_constraint` and `bag.push` from the emit
    /// sites.
    ///
    /// Method-call return edges (`Expression(refidx) → Edge(MethodOnClass{class, method})`)
    /// are emitted later — by `emit_method_call_return_edges` from
    /// inside the worklist, once `invocant_class` is filled.
    fn populate_witness_bag(&mut self) {
        use crate::witnesses::{
            TypeObservation, Witness, WitnessAttachment, WitnessPayload, WitnessSource,
        };

        // Rep observations from `$v->{k}` access. Method-call return
        // edges on `Expression(refidx)` are emitted later — by the
        // chain-typing PostFold pass once `invocant_class` is filled —
        // as `Edge(MethodOnClass{class, method})`. Without a known
        // class there's no class-keyed answer to chase to, so the
        // emission is gated by chain-typing's own progress.
        let mut hash_obs: Vec<(String, ScopeId, Span)> = Vec::new();
        for r in self.refs.iter() {
            if let RefKind::HashKeyAccess { var_text, .. } = &r.kind {
                if var_text.starts_with('$') {
                    hash_obs.push((var_text.clone(), r.scope, r.span));
                }
            }
        }
        for (var, scope, span) in hash_obs {
            self.bag.push(Witness {
                attachment: WitnessAttachment::Variable { name: var, scope },
                source: WitnessSource::Builder("hash_ref_access".into()),
                payload: WitnessPayload::Observation(TypeObservation::HashRefAccess),
                span,
            });
        }

        // Part 1 — invocant mutations on hash keys.
        let mut mutations: Vec<(HashKeyOwner, String, Span)> = Vec::new();
        for r in &self.refs {
            if let (RefKind::HashKeyAccess { owner, var_text }, AccessKind::Write) =
                (&r.kind, r.access)
            {
                let resolved_owner = match owner {
                    Some(o @ (HashKeyOwner::Class(_) | HashKeyOwner::Sub { .. })) => Some(o.clone()),
                    _ => {
                        if var_text == "$self" {
                            let scope = &self.scopes[r.scope.0 as usize];
                            scope.package.clone().map(HashKeyOwner::Class)
                        } else {
                            None
                        }
                    }
                };
                if let Some(o) = resolved_owner {
                    mutations.push((o, r.target_name.clone(), r.span));
                }
            }
        }
        for (owner, key, span) in mutations {
            self.bag.push(Witness {
                attachment: WitnessAttachment::HashKey { owner, name: key.clone() },
                source: WitnessSource::Builder("invocant_mutation".into()),
                payload: WitnessPayload::Fact {
                    family: "mutation".into(),
                    key: "written_at".into(),
                    value: crate::witnesses::FactValue::Str(key),
                },
                span,
            });
        }
    }

}

/// Which arity branch a `return_expression` represents. Computed
/// from the shape of the return's parent in the CST.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArityBranch {
    /// `return X unless @_;` / `return X if !@_;` — fires when the
    /// caller passed zero additional args.
    Zero,
    /// `return X if @_ == N;` / `if scalar(@_) == N;` / explicit
    /// `if (@_ == N) { return X }`. Exact-N match.
    Exact(u32),
    /// Fall-through `return X;` with no condition wrapper — fires
    /// when no earlier arity-gated branch matched.
    Default,
}

/// Inspect the `return_expression`'s parent. If it's a
/// `postfix_conditional_expression` with `@_` as the condition, we
/// look at the connector keyword (`if` vs `unless`) to decide. If
/// it's a bare expression_statement, this is a default branch.
///
/// Known idioms (spike shortlist):
///   - `return X unless @_;`       → Zero
///   - `return X;`                  → Default
///
/// Unknowns (return None and punt):
///   - `return X if @_;`            (arity >= 1 narrowing)
///   - `return X if @_ == N;`       (exact N)
///   - `return X if scalar @_ …;`   (scalar wrapper)
fn classify_arity_branch(return_node: tree_sitter::Node, source: &[u8]) -> Option<ArityBranch> {
    let Some(parent) = return_node.parent() else { return None };
    match parent.kind() {
        "expression_statement" => classify_bare_return_or_if_arm(parent, source),
        "postfix_conditional_expression" => {
            let cond = parent.child_by_field_name("condition")?;
            let keyword = connector_keyword_between(return_node, cond, source)?;
            classify_arity_condition(cond, source, &keyword)
        }
        _ => None,
    }
}

/// `expression_statement` parent → either a bare `return X` at sub
/// body level (Default), OR inside an `if (@_ == N) { return X }` arm
/// where the conditional_statement's condition is an arity test (Exact).
fn classify_bare_return_or_if_arm(
    expr_stmt: tree_sitter::Node,
    source: &[u8],
) -> Option<ArityBranch> {
    let Some(block) = expr_stmt.parent() else { return None };
    if block.kind() != "block" {
        return None;
    }
    let Some(outer) = block.parent() else { return None };
    match outer.kind() {
        "subroutine_declaration_statement"
        | "method_declaration_statement"
        | "anonymous_subroutine_expression" => Some(ArityBranch::Default),
        "conditional_statement" => {
            // `if (condition) { return X }` — classify by condition.
            // Arity-gated condition → Zero / Exact. Anything else
            // (regular boolean predicate) is still a contributor to
            // the default return shape: the arm runs whenever the
            // condition holds, regardless of arity. Classify as
            // Default so the FluentArityDispatch reducer can fold
            // agreement across every arm that doesn't gate on arity.
            let cond = outer.child_by_field_name("condition")?;
            classify_arity_condition(cond, source, "if").or(Some(ArityBranch::Default))
        }
        _ => None,
    }
}

/// Classify an arity condition node. `keyword` is "if" or "unless".
fn classify_arity_condition(
    cond: tree_sitter::Node,
    source: &[u8],
    keyword: &str,
) -> Option<ArityBranch> {
    // Shape 1: bare `@_`.
    if cond.kind() == "array" {
        let text = cond.utf8_text(source).ok()?.trim();
        if text == "@_" {
            return match keyword {
                "unless" => Some(ArityBranch::Zero),
                _ => None, // `if @_` is "arity >= 1" — not expressible as Exact yet.
            };
        }
        return None;
    }
    // Shape 2: `!@_` → unary_expression with operand @_.
    if cond.kind() == "unary_expression" {
        let op_text = raw_leading_op(cond, source);
        if op_text == "!" {
            if let Some(operand) = cond.child_by_field_name("operand") {
                if operand.kind() == "array" {
                    let text = operand.utf8_text(source).ok()?.trim();
                    if text == "@_" {
                        return match keyword {
                            "if" => Some(ArityBranch::Zero),
                            "unless" => None, // `unless !@_` → arity >= 1, skip
                            _ => None,
                        };
                    }
                }
            }
        }
        return None;
    }
    // Shape 3: `@_ == N` / `scalar(@_) == N` → equality_expression.
    if cond.kind() == "equality_expression" {
        let op = raw_mid_op(cond, source);
        if op != "==" && op != "!=" {
            return None;
        }
        let left = cond.child_by_field_name("left")?;
        let right = cond.child_by_field_name("right")?;
        let n = extract_numeric(right, source)?;
        let counts_args = node_is_arity_magnitude(left, source);
        if !counts_args {
            return None;
        }
        match (keyword, op.as_str()) {
            ("if", "==") => Some(ArityBranch::Exact(n)),
            ("unless", "!=") => Some(ArityBranch::Exact(n)),
            _ => None, // != / >= / etc. — not a single Exact fact.
        }
    } else {
        None
    }
}

/// True if `node` evaluates to the length of `@_` — either `@_`
/// itself (scalar context in an equality) or `scalar(@_)`.
fn node_is_arity_magnitude(node: tree_sitter::Node, source: &[u8]) -> bool {
    if node.kind() == "array" {
        return node.utf8_text(source).map(|s| s.trim() == "@_").unwrap_or(false);
    }
    if node.kind() == "func1op_call_expression" {
        let Some(kw) = node.child(0) else { return false };
        let Ok(name) = kw.utf8_text(source) else { return false };
        if name != "scalar" {
            return false;
        }
        for i in 0..node.named_child_count() {
            if let Some(c) = node.named_child(i) {
                if c.kind() == "array" {
                    return c.utf8_text(source).map(|s| s.trim() == "@_").unwrap_or(false);
                }
            }
        }
    }
    false
}

/// Extract a small unsigned numeric literal from a `number` node.
fn extract_numeric(node: tree_sitter::Node, source: &[u8]) -> Option<u32> {
    if node.kind() != "number" {
        return None;
    }
    node.utf8_text(source).ok()?.trim().parse::<u32>().ok()
}

/// Read the raw bytes between two sibling nodes to recover the
/// postfix keyword (`if` / `unless` / `while` / …). Used because
/// tree-sitter-perl shares one node kind for both `if` and `unless`.
fn connector_keyword_between(
    left: tree_sitter::Node,
    right: tree_sitter::Node,
    source: &[u8],
) -> Option<String> {
    let start = left.end_byte();
    let end = right.start_byte();
    if end <= start {
        return None;
    }
    let between = std::str::from_utf8(&source[start..end]).ok()?.trim();
    // Expect exactly one keyword in between.
    for kw in ["unless", "if", "while", "until"] {
        if between == kw || between.starts_with(kw) && between.trim() == kw {
            return Some(kw.to_string());
        }
        // tolerate extra whitespace (e.g. newlines)
        if between.split_whitespace().next() == Some(kw) {
            return Some(kw.to_string());
        }
    }
    None
}

fn raw_leading_op(node: tree_sitter::Node, source: &[u8]) -> String {
    // Operator is an anonymous child between start and the first named
    // child (operand). Read the bytes before the operand node.
    let Some(operand) = node.child_by_field_name("operand") else {
        return String::new();
    };
    let start = node.start_byte();
    let end = operand.start_byte();
    std::str::from_utf8(&source[start..end])
        .unwrap_or("")
        .trim()
        .to_string()
}

fn raw_mid_op(node: tree_sitter::Node, source: &[u8]) -> String {
    let Some(left) = node.child_by_field_name("left") else {
        return String::new();
    };
    let Some(right) = node.child_by_field_name("right") else {
        return String::new();
    };
    let start = left.end_byte();
    let end = right.start_byte();
    std::str::from_utf8(&source[start..end])
        .unwrap_or("")
        .trim()
        .to_string()
}

/// Structural index of one return-expression in a sub body. Type
/// information lives in the bag: the body expression carries its own
/// `Expr(body_span)` witnesses (literal types, Edges to Variable /
/// Symbol / Expression / nested Expr), and `Symbol(sub_id)` collects
/// per-arm `branch_arm`-source `Edge(Expr(...))` witnesses. What's
/// left in this struct is what consumers still need at a glance:
/// which sub the return belongs to, where it is, the arity bucket it
/// gates on (for `emit_arity_return_witnesses`), and the body span
/// (the `Expr(span)` key both `emit_arity_return_witnesses` and
/// `fold_per_sub_return_arms` query inline via `bag_query_expr_span`).
struct ReturnInfo {
    /// The scope (Sub/Method) this return belongs to.
    scope: ScopeId,
    /// Arity-dispatch classification (`unless @_`, `if @_ == N`, …) so
    /// `resolve_return_types` can emit `ArityReturn` witnesses with
    /// the bag-resolved arm type AT REDUCTION TIME. `None` for
    /// returns that aren't arity-gated.
    arity_branch: Option<ArityBranch>,
    /// Span of the return-expression node — kept for diagnostics and
    /// for matching against arity-classification anchors.
    span: Span,
    /// Span of the inner body expression (the `EXPR` in `return EXPR`).
    /// Doubles as the `WitnessAttachment::Expr(span)` key the per-RI
    /// fold reads. `None` for bare `return;` — those have no value.
    body_span: Option<Span>,
}

/// Strip a `Pkg::Sub::` prefix from a sub-name identifier, returning the bare
/// trailing component. `"Foo::Bar::baz"` → `"baz"`; `"baz"` → `"baz"`. Pure
/// string op — does not consult the symbol table or package state.
fn bare_name(s: &str) -> &str {
    s.rsplit("::").next().unwrap_or(s)
}

/// If `return_node` is `return CALL`, where CALL is a simple named function
/// call or method call, return the bare called name. Otherwise None. Used to
/// collect hash-key ownership delegation chains for post-pass resolution.
fn extract_delegated_call_name<'a>(return_node: Node<'a>, source: &'a [u8]) -> Option<String> {
    // The `return X` node has the expression as its first named child.
    let expr = return_node.named_child(0)?;
    let call_name = match expr.kind() {
        "function_call_expression" | "ambiguous_function_call_expression" => {
            expr.child_by_field_name("function")?.utf8_text(source).ok()?
        }
        "method_call_expression" => {
            expr.child_by_field_name("method")?.utf8_text(source).ok()?
        }
        _ => return None,
    };
    // Strip package prefix — delegation is stored by bare sub name to match
    // the return_types lookup convention.
    Some(bare_name(call_name).to_string())
}

/// Find the `varname` child of a variable node (`scalar`/`array`/`hash`/etc.).
/// The grammar aliases its `_var_indirob` into a `varname` node whose text
/// is the bare variable name — no sigil, no braces. For `${foo}` the outer
/// `scalar` text is `${foo}` but the `varname` child text is just `foo`;
/// for `$:whatever` it's whatever TSP decided is the name token.
///
/// For `${$hash{k}}` and other nontrivial derefs the varname child is a
/// `block` — callers that only want a simple identifier should check the
/// returned node's kind (`varname` text is only meaningful for the
/// identifier form).
fn find_varname_child<'a>(node: Node<'a>) -> Option<Node<'a>> {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "varname" {
                return Some(child);
            }
        }
    }
    None
}

/// Re-parse an `isa` value as Perl and extract the class name from
/// `InstanceOf['Foo::Bar']` / `InstanceOf["Foo::Bar"]`. Tree-sitter-perl
/// parses this as `ambiguous_function_call_expression` with function
/// `InstanceOf` and an `anonymous_array_expression` argument containing
/// a single string literal — we walk that shape and ignore everything
/// else (if the tree doesn't match, this isn't an InstanceOf).
fn parse_instance_of(isa: &str) -> Option<String> {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).ok()?;
    let tree = parser.parse(isa, None)?;
    let source = isa.as_bytes();

    // Walk to the first ambiguous_function_call_expression.
    fn find_call<'a>(node: Node<'a>) -> Option<Node<'a>> {
        if node.kind() == "ambiguous_function_call_expression"
            || node.kind() == "function_call_expression"
        {
            return Some(node);
        }
        for i in 0..node.named_child_count() {
            if let Some(c) = node.named_child(i) {
                if let Some(found) = find_call(c) {
                    return Some(found);
                }
            }
        }
        None
    }
    let call = find_call(tree.root_node())?;
    let func = call.child_by_field_name("function")?;
    if func.utf8_text(source).ok()? != "InstanceOf" {
        return None;
    }
    let args = call.child_by_field_name("arguments")?;
    if args.kind() != "anonymous_array_expression" && args.kind() != "array_ref_expression" {
        return None;
    }
    for i in 0..args.named_child_count() {
        let child = args.named_child(i)?;
        if matches!(child.kind(), "string_literal" | "interpolated_string_literal") {
            for j in 0..child.named_child_count() {
                if let Some(content) = child.named_child(j) {
                    if content.kind() == "string_content" {
                        return content.utf8_text(source).ok().map(|s| s.to_string());
                    }
                }
            }
        }
    }
    None
}

/// Walk the delegation chain starting at `start` until we find a sub that
/// actually owns HashKeyDefs, or run out of links. Cycle-safe via a visited
/// set; caps at a small depth since delegation chains in real code are short.
fn walk_return_delegation_chain(
    start: &str,
    delegations: &std::collections::HashMap<String, String>,
    subs_with_own_keys: &std::collections::HashSet<String>,
) -> String {
    let mut current = start.to_string();
    let mut seen = std::collections::HashSet::new();
    for _ in 0..10 {
        if subs_with_own_keys.contains(&current) {
            return current;
        }
        if !seen.insert(current.clone()) {
            return current; // cycle guard
        }
        match delegations.get(&current) {
            Some(next) => current = next.clone(),
            None => return current,
        }
    }
    current
}

struct DeferredVarType {
    variable: String,
    at: Span,
    inferred_type: InferredType,
}

struct Builder<'a> {
    source: &'a [u8],

    scopes: Vec<Scope>,
    symbols: Vec<Symbol>,
    refs: Vec<Ref>,
    /// Plugin-emitted `VarType` constraints, resolved to scopes only
    /// after the whole CST has been walked (plugin dispatch runs
    /// before we recurse into call args, so at emit-time the target
    /// scope usually doesn't exist yet).
    deferred_var_types: Vec<DeferredVarType>,
    fold_ranges: Vec<FoldRange>,
    imports: Vec<Import>,
    /// Return values collected during the walk (explicit `return` + implicit last expr).
    return_infos: Vec<ReturnInfo>,
    /// For each Sub/Method scope, the body span of the last
    /// top-level expression statement. Used as the implicit-return
    /// query key — `fold_per_sub_return_arms` reads `Expr(span)` via
    /// `bag_query_expr_span` for scopes without an explicit `return`.
    /// Replaces the old `last_expr_type: HashMap<ScopeId,
    /// Option<InferredType>>` dual-store: types now ride the bag,
    /// this map only carries the structural pointer to the source
    /// span.
    last_expr_span: std::collections::HashMap<ScopeId, Span>,
    /// Assignments where RHS is a function call — resolved in return-type post-pass.
    call_bindings: Vec<CallBinding>,
    /// Assignments where RHS is a method call — resolved in FileAnalysis post-pass.
    method_call_bindings: Vec<MethodCallBinding>,
    /// Raw POD text blocks collected during the walk (for tail-POD post-pass).
    pod_texts: Vec<String>,
    /// Parent classes for each package (from use parent/base, @ISA, class :isa).
    package_parents: std::collections::HashMap<String, Vec<String>>,
    /// Modules the current package has `use`d, in source order. Used by
    /// `PluginRegistry::applicable` for `Trigger::UsesModule` matching.
    package_uses: std::collections::HashMap<String, Vec<String>>,
    /// sub_name → delegated sub name, for bodies that are `return other()` or
    /// a bare trailing call. Used to propagate hash-key ownership through
    /// intermediate subs so `sub chain { return get_config() }` doesn't
    /// orphan `$cfg = chain(); $cfg->{host}`.
    sub_return_delegations: std::collections::HashMap<String, String>,
    /// Per-symbol resolved return type. Replaces `SymbolDetail::Sub.return_type`
    /// (deleted in D1 of the bag-residual refactor). Walk-time synthesis
    /// (Mojo/Moo/DBIC accessors) writes here; worklist's `return_types`
    /// fold writes here; the final writeback iterates this map to push
    /// `Symbol(sid)` and (for first-seen `(class, name)`) `MethodOnClass`
    /// witnesses into the bag. Lives only inside the builder — never
    /// makes it into `FileAnalysis`. The bag is the single durable
    /// authority on return types post-build.
    ///
    /// Scheduled for deletion: see `docs/prompt-cleanups.md` #1. Direct
    /// emit at synthesis time would remove this sidecar; what blocks it
    /// is `MethodOnClass` primary-dedup, which currently needs the full
    /// symbol list visible at writeback.
    resolved_returns: std::collections::HashMap<SymbolId, InferredType>,
    /// Framework mode per package (Moo, Moose, MojoBase) for accessor synthesis.
    framework_modes: std::collections::HashMap<String, FrameworkMode>,
    /// Functions implicitly imported by OOP frameworks (has, extends, with, etc.)
    framework_imports: std::collections::HashSet<String>,
    /// Known compile-time string values, accumulated during the walk.
    /// Keyed by variable/constant name (e.g. "@COMMON", "BASE_CLASS", "$PREFIX").
    constant_strings: std::collections::HashMap<String, Vec<String>>,
    /// Exported function names from @EXPORT assignments.
    export: Vec<String>,
    /// Exported function names from @EXPORT_OK assignments.
    export_ok: Vec<String>,
    /// Plugin-declared namespaces collected during the walk via
    /// `EmitAction::PluginNamespace`. Flushed into the final
    /// `FileAnalysis.plugin_namespaces`.
    plugin_namespaces: Vec<crate::file_analysis::PluginNamespace>,
    /// Per-symbol provenance for return types. Populated by plugin
    /// `overrides()` (PluginOverride) and by reducer-driven folds
    /// (ReducerFold). Empty entry == `TypeProvenance::Inferred`.
    /// Flushed into `FileAnalysis.type_provenance` at construction.
    type_provenance: std::collections::HashMap<SymbolId, TypeProvenance>,
    /// The single, unified witness bag — canonical at every phase,
    /// walk-time included. Idiom detectors (branch arms, arity
    /// gating), TC seeding (`push_type_constraint`), and post-walk
    /// passes (hash-key obs, mutations, call-binding propagation) all
    /// push directly here. Moved into `FileAnalysis.witnesses` when
    /// the analysis is constructed — no second seeding pass.
    bag: crate::witnesses::WitnessBag,
    /// Walk-time symbol-table lookups that came up empty for a name
    /// resolvable to a local sub. Perl's name resolution for subs is
    /// forward-tolerant (`sub a { b() } sub b {…}` is legal), but the
    /// walk emits witnesses live, so a forward-defined callee silently
    /// produces no witness in `expr_payload`'s call arms. Re-resolved
    /// against the final symbol table by `resolve_forward_call_targets`
    /// between `populate_witness_bag` and `fold_to_fixed_point`. See
    /// `docs/prompt-forward-reference-resolution.md`.
    unresolved_call_targets: Vec<UnresolvedCallTarget>,
    /// Per-package framework fact, computed from `framework_modes` once
    /// the walk finishes. Available before `resolve_return_types` so
    /// the bag-aware return-arm fold can ask the framework-aware
    /// reducer with the right context.
    package_framework: std::collections::HashMap<String, crate::witnesses::FrameworkFact>,

    // Walk state
    scope_stack: Vec<ScopeId>,
    current_package: Option<String>,
    next_scope_id: u32,
    next_symbol_id: u32,

    /// Flat record of `package`/`class` declarations and the byte
    /// ranges they govern. Independent of the lexical scope tree —
    /// `package Foo;` is not a lexical boundary in Perl. For
    /// statement-form declarations the end is initially seeded with
    /// the file end and gets trimmed when a same-level successor
    /// appears.
    package_ranges: Vec<crate::file_analysis::PackageRange>,
    /// Index in `package_ranges` of the currently-open statement-form
    /// declaration (the one a successor `package X;` / `class X;`
    /// would supplant), if any.
    open_statement_package: Option<usize>,

    /// Framework plugin registry. Shared Arc so multiple builders in one
    /// process avoid re-compiling the same Rhai scripts.
    plugins: Arc<PluginRegistry>,

    /// Build-time chain-typing cache for MethodCall ref invocants.
    /// Keyed by `refs[idx]`. Walk-time fills it for syntactic cases
    /// (constructor pattern `Foo->new`, `__PACKAGE__->m`); PostFold's
    /// `apply_chain_typing_invocants` fills it for variable invocants
    /// whose TC has crystallized. Build-time consumers
    /// (`emit_method_call_arg_keys`, `emit_method_call_return_edges`,
    /// fixed-point movement counter) read it.
    ///
    /// **Build-only.** Never copied into `FileAnalysis`. The reader-
    /// side counterpart is `FileAnalysis::method_call_invocant_class`,
    /// which queries the bag at read time and so picks up cross-file
    /// enrichment automatically.
    method_call_invocant: std::collections::HashMap<usize, String>,

    /// MethodCall ref indices for which we've published an
    /// `InferredType::Parametric` witness on `Expression(refidx)`
    /// — `recv->resultset('Foo')` and search-family threading
    /// targets. `emit_method_call_return_edges` consults this set
    /// and skips publishing its standard `Edge(MethodOnClass)` for
    /// these refs, so the Parametric isn't masked by the receiver
    /// class's plain return-type entry. **Build-only**, like
    /// `method_call_invocant`.
    parametric_emitted_refs: std::collections::HashSet<usize>,
}

/// Owner-and-gating discriminator for `emit_call_arg_key_accesses`.
/// One emitter, three semantics:
///   * `Strict(owner)` — supplied owner; emit only if a matching
///     HashKeyDef is registered for `(key, owner)`. Prevents
///     `Foo::bar(name=>1)` from latching onto unrelated
///     `Sub{Foo,new}` keys when `name` isn't actually a `bar` arg.
///   * `Open(owner)` — supplied owner; emit unconditionally.
///     The receiver's flavor pinned the owner via
///     `method_arg_owner` — the type IS the gate; cross-file
///     producer's HashKeyDef may not be visible at consumer build.
///   * `Deferred` — owner=None at emit time. Post-walk fixup in
///     `FileAnalysis::fix_chain_receiver_hash_key_owners`
///     fills the owner once the enclosing call's receiver type
///     resolves (in-file via `call_ref_by_start` recursion, or
///     cross-file once `module_index` is available).
enum Gate {
    Strict(HashKeyOwner),
    Open(HashKeyOwner),
    Deferred,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FrameworkMode {
    Moo,
    Moose,
    MojoBase,
}

/// Queued by `emit_expr_witness` whenever a name-driven call kind
/// (`function_call_expression`, `bareword`, `scoped_identifier`) couldn't
/// emit its `Edge(Symbol(_))` witness because the callee wasn't in the
/// symbol table yet. Re-resolved post-walk against the final symbols.
#[derive(Debug, Clone)]
struct UnresolvedCallTarget {
    /// Bare sub name (e.g. `longmess_heavy`, stripped of any `Pkg::` prefix).
    name: String,
    /// `Expr(span)` attachment that the missing witness should land on.
    expr_span: Span,
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

    /// Package/class name surrounding `node`. Reads the innermost
    /// containing scope's `package` field — set on both
    /// `package Foo;` and `class Foo { … }` entries, so this works
    /// for either flavor of class declaration. Used by
    /// `invocant_type_at_node` for `$self` / `shift` / `__PACKAGE__`
    /// resolution post-walk, where `self.current_package` is stale
    /// (it holds the walk's last-opened package, not the one
    /// containing the node we're querying).
    fn package_for_node(&self, node: Node<'a>) -> Option<String> {
        let scope_id = self.scope_at_point(node.start_position());
        let mut cur = Some(scope_id);
        while let Some(sid) = cur {
            let s = &self.scopes[sid.0 as usize];
            if let Some(ref pkg) = s.package {
                return Some(pkg.clone());
            }
            cur = s.parent;
        }
        None
    }

    /// Innermost scope containing `point`. Mirrors
    /// `FileAnalysis::scope_at` but reads `&self.scopes` directly so
    /// it's callable from within Builder during and after the walk.
    /// Falls back to `ScopeId(0)` (the file scope) if no scope
    /// matches — a defensive default for cases where the walk hasn't
    /// produced any scope containing the point yet.
    fn scope_at_point(&self, point: Point) -> ScopeId {
        let mut best: Option<(ScopeId, u64)> = None;
        for scope in &self.scopes {
            if !crate::file_analysis::contains_point(&scope.span, point) {
                continue;
            }
            let r = scope.span.end.row.saturating_sub(scope.span.start.row) as u64;
            let c = if scope.span.start.row == scope.span.end.row {
                scope.span.end.column.saturating_sub(scope.span.start.column) as u64
            } else {
                0
            };
            let size = r * 1_000_000 + c;
            if best.is_none() || size <= best.unwrap().1 {
                best = Some((scope.id, size));
            }
        }
        best.map(|(id, _)| id).unwrap_or(ScopeId(0))
    }

    // ---- Symbol/Ref creation ----

    fn add_symbol(&mut self, name: String, kind: SymKind, span: Span, selection_span: Span, detail: SymbolDetail) -> SymbolId {
        self.add_symbol_ns(name, kind, span, selection_span, detail, Namespace::Language)
    }

    fn add_symbol_ns(
        &mut self,
        name: String,
        kind: SymKind,
        span: Span,
        selection_span: Span,
        detail: SymbolDetail,
        namespace: Namespace,
    ) -> SymbolId {
        let id = SymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;
        // Every symbol attaches to the current lexical scope. Package
        // context lives separately in `package_ranges`; the variable
        // resolver gates `our` decls by package match at lookup time
        // (so bare `$version` from a sibling `package main;` doesn't
        // reach a Calculator-package `our $version`).
        self.symbols.push(Symbol {
            id,
            name,
            kind,
            span,
            selection_span,
            scope: self.current_scope(),
            package: self.current_package.clone(),
            detail,
            namespace,
            outline_label: None,
        });
        id
    }

    // ---- Package-range tracking ----

    /// Record a `package Foo;` / `class Foo;` (statement form). Trims
    /// the previously-open statement range to end at `start`, then
    /// pushes a new range whose end is seeded with the file end —
    /// trimmed in turn when a successor appears, or left at file end
    /// if none does.
    fn open_statement_package_range(&mut self, name: String, start: Point) {
        use crate::file_analysis::{PackageKind, PackageRange};
        if let Some(idx) = self.open_statement_package.take() {
            self.package_ranges[idx].span.end = start;
        }
        let file_end = self
            .scope_stack
            .first()
            .map(|id| self.scopes[id.0 as usize].span.end)
            .unwrap_or(start);
        self.package_ranges.push(PackageRange {
            package: name,
            span: Span { start, end: file_end },
            kind: PackageKind::Statement,
        });
        self.open_statement_package = Some(self.package_ranges.len() - 1);
    }

    /// Record a `package Foo { … }` / `class Foo { … }` (block form).
    /// Span is the node's own span — no successor-trimming required.
    /// Block forms do NOT supplant any statement-form range that
    /// brackets them: `package Foo; package Bar { … }` leaves Foo
    /// covering everything outside the Bar block.
    fn push_block_package_range(&mut self, name: String, span: Span) {
        use crate::file_analysis::{PackageKind, PackageRange};
        self.package_ranges.push(PackageRange {
            package: name,
            span,
            kind: PackageKind::Block,
        });
    }

    /// Build-time mirror of `FileAnalysis::package_at`. Used by the
    /// variable resolver to gate `our` decls by package context — the
    /// builder can't call into FileAnalysis (it hasn't been
    /// constructed yet).
    fn package_at_pos(&self, point: Point) -> Option<&str> {
        let mut best: Option<&crate::file_analysis::PackageRange> = None;
        for r in &self.package_ranges {
            if !crate::file_analysis::contains_point(&r.span, point) {
                continue;
            }
            let win = match best {
                None => true,
                Some(prev) => {
                    let cur_start = (r.span.start.row, r.span.start.column);
                    let prev_start = (prev.span.start.row, prev.span.start.column);
                    let cur_size = (
                        r.span.end.row - r.span.start.row,
                        r.span.end.column.saturating_sub(r.span.start.column),
                    );
                    let prev_size = (
                        prev.span.end.row - prev.span.start.row,
                        prev.span.end.column.saturating_sub(prev.span.start.column),
                    );
                    cur_start > prev_start || (cur_start == prev_start && cur_size < prev_size)
                }
            };
            if win {
                best = Some(r);
            }
        }
        best.map(|r| r.package.as_str())
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

    // ---- Plugin dispatch helpers ----

    /// Normalize a call's `arguments` field into a flat list of argument
    /// nodes. Tree-sitter-perl wraps multi-arg lists in `list_expression`;
    /// single-arg calls present the arg directly.
    fn extract_call_args(&self, call_node: Node<'a>) -> Vec<Node<'a>> {
        let args = match call_node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return Vec::new(),
        };
        if args.kind() == "list_expression" || args.kind() == "parenthesized_expression" {
            (0..args.named_child_count())
                .filter_map(|i| args.named_child(i))
                .collect()
        } else {
            vec![args]
        }
    }

    /// Build an `ArgInfo` for a plugin. Constant-folds literals, barewords,
    /// and `$var` references that accumulate in `constant_strings`. When the
    /// arg is an anonymous sub, also extracts its param list so plugins
    /// registering handlers (`->on('ready', sub ($s, $m) {})`) can preserve
    /// the handler signature for later sig-help lookup.
    fn arg_info_for(&self, arg: Node<'a>) -> plugin::ArgInfo {
        let text = arg.utf8_text(self.source).unwrap_or("").to_string();
        let mut content_span: Option<Span> = None;
        let string_value = match arg.kind() {
            "string_literal" | "interpolated_string_literal" => {
                // Read the string_content child — quote-flavor-agnostic
                // (handles q{}, qq!!, heredocs, etc.). An empty literal
                // has no content child, so default to "".
                // Also capture the content span so plugins can address
                // positions inside the string without hardcoding
                // quote-length offsets into the outer node's span.
                for i in 0..arg.named_child_count() {
                    if let Some(c) = arg.named_child(i) {
                        if c.kind() == "string_content" {
                            content_span = Some(node_to_span(c));
                            break;
                        }
                    }
                }
                Some(self.extract_string_content(arg).unwrap_or_default())
            }
            // `autoquoted_bareword` is what the LHS of fat-comma parses
            // as (`key => value`) — `bareword` never appears there. The
            // `bareword` branch catches the other contexts (e.g. unquoted
            // positional args) where the token text IS the string value.
            "autoquoted_bareword" | "bareword" => Some(text.clone()),
            "scalar" | "array" | "hash" => {
                self.resolve_constant_strings(&text, 0)
                    .and_then(|v| v.into_iter().next())
            }
            _ => None,
        };
        let inferred_type = self.infer_expression_type(arg);
        let sub_params = if arg.kind() == "anonymous_subroutine_expression" {
            self.extract_anonymous_sub_params(arg)
        } else {
            Vec::new()
        };
        // `callable_return_edge` flows from whichever
        // `InferredType::CodeRef { return_edge }` is reachable for
        // this arg. Two reachability paths covered uniformly:
        //
        //   helper(name => sub { … })          (literal in arg slot)
        //   my $sub = sub { … }; helper(name => $sub)   (rebound)
        //
        // The literal path goes through `infer_expression_type`'s
        // syntax-closed `anonymous_subroutine_expression` arm; the
        // rebind path goes through `invocant_type_at_node`'s
        // `scalar` arm, which `bag_query_variable`-resolves the
        // variable's TC. Either yields the right `CodeRef` shape;
        // the projection extracts `return_edge`.
        let callable_return_edge = inferred_type
            .as_ref()
            .and_then(InferredType::callable_return_edge)
            .or_else(|| {
                self.invocant_type_at_node(arg)
                    .as_ref()
                    .and_then(InferredType::callable_return_edge)
            });
        plugin::ArgInfo {
            text,
            string_value,
            span: node_to_span(arg),
            content_span,
            inferred_type,
            sub_params,
            callable_return_edge,
        }
    }

    /// Span of the body's last expression on an
    /// `anonymous_subroutine_expression`. Mirrors
    /// `infer_anonymous_sub_return_type`'s body-walk — the last
    /// statement, unwrapped from `expression_statement` /
    /// `return_expression` if necessary, gives us the expression
    /// whose type IS the sub's return when called. Plugins use
    /// this to emit a back-edge from the synthesized Method's
    /// Symbol to that Expr, deferring return-type inference to
    /// query time.
    fn anonymous_sub_body_last_expr_span(&self, node: Node<'a>) -> Option<Span> {
        if node.kind() != "anonymous_subroutine_expression" {
            return None;
        }
        let body = node.child_by_field_name("body")?;
        let mut node = body.named_child(body.named_child_count().checked_sub(1)?)?;
        // Peel through `expression_statement` and `return_expression`
        // wrappers (an explicit `return $expr;` shows up as
        // `expression_statement → return_expression → $expr` in
        // tree-sitter-perl). One unwrap isn't enough.
        loop {
            match node.kind() {
                "expression_statement" | "return_expression" => {
                    node = node.named_child(0)?;
                }
                _ => break,
            }
        }
        Some(node_to_span(node))
    }

    /// Extract params from an anonymous sub. Delegates to the builder's
    /// shared named-sub extractor (signature syntax + `my (...) = @_` +
    /// `shift`/`$_[N]` unpacks, all via tree walking) so the two codepaths
    /// can't diverge.
    fn extract_anonymous_sub_params(&self, sub_node: Node<'a>) -> Vec<plugin::EmittedParam> {
        self.extract_params(sub_node)
            .into_iter()
            .map(|p| plugin::EmittedParam {
                name: p.name,
                default: p.default,
                is_slurpy: p.is_slurpy,
                is_invocant: false,
            })
            .collect()
    }

    /// Best-effort receiver-type resolution for a method call. Handles:
    ///   * `$self` / `__PACKAGE__`        → current package
    ///   * bare `Pkg::Name`               → literal class
    ///   * `$var` with a prior `my $var = Pkg->new` → looked up via
    ///     the bag's Variable witnesses (latest wins). This lets the
    ///     mojo-events plugin resolve `$obj->emit(...)` in a consumer
    ///     file to the producer's class, enabling cross-file def/ref
    ///     pairing.
    /// Resolve a bare `foo()` call to the package whose `sub foo` it
    /// refers to. Order mirrors Perl's name-lookup rule:
    ///
    ///   1. Explicit qualifier (`Foo::bar()` → `Foo`).
    ///   2. Enclosing package that declares `sub <name>` locally (so
    ///      `package Foo { sub bar {} bar(); }` resolves to `Foo`).
    ///   3. Most-recent import whose `imported_symbols` lists this
    ///      name (`use Bler qw/hi/` → `Bler`). Later imports win —
    ///      Perl's later `use` shadows earlier one.
    ///
    /// Returns `None` when none of those pin a package. Downstream
    /// class/package-scoped queries treat `None` as no-match rather
    /// than falling back to name-only union.
    fn resolve_call_package(&self, call_name: &str) -> Option<String> {
        // (1) Qualified: `Foo::bar` → `Foo`.
        if let Some(idx) = call_name.rfind("::") {
            return Some(call_name[..idx].to_string());
        }
        // (2) Enclosing package defines the sub locally.
        if let Some(ref pkg) = self.current_package {
            if self.symbols.iter().any(|s| {
                s.name == call_name
                    && matches!(s.kind, SymKind::Sub | SymKind::Method)
                    && s.package.as_deref() == Some(pkg.as_str())
            }) {
                return Some(pkg.clone());
            }
        }
        // (3) Imports — walk in reverse order so later `use` wins.
        for imp in self.imports.iter().rev() {
            if imp.imported_symbols.iter().any(|s| s.local_name == *call_name) {
                return Some(imp.module_name.clone());
            }
        }
        None
    }

    /// Type any expression node in invocant position. Single
    /// kind-dispatch shared between `resolve_invocant_class_tree`
    /// (which projects to a class name string) and
    /// `receiver_type_for` (which uses the InferredType directly for
    /// plugin trigger matching). Routes everything through the bag
    /// where the bag has the answer; falls back to walk-time
    /// canonical sources (TCs, current_package) only where the bag
    /// can't (variable invocants whose TC mirror happens post-walk).
    ///
    /// Resolves:
    ///   - `Foo->new(...)` (constructor)                       → ClassName(Foo)
    ///   - `X->method(...)` chain                              → bag chase via Expression(refidx)
    ///   - `$self`                                             → ClassName(current_package)
    ///   - `__PACKAGE__`                                       → ClassName(current_package)
    ///   - `$var`                                              → TC for `$var`, then bag
    ///   - bareword `Foo` (sub with ClassName return)          → that class
    ///   - bareword `Foo` (no such sub)                        → ClassName(Foo)
    ///   - `shift` / `$_[0]` in method body                    → ClassName(current_package)
    ///   - `get_foo()->bar()` outer's invocant                 → bag query Symbol(get_foo, arity)
    /// `@`/`%` containers and unrecognized kinds return `None`.
    fn invocant_type_at_node(&self, node: Node<'a>) -> Option<InferredType> {
        match node.kind() {
            "method_call_expression" => {
                // Constructor pattern is closed under syntax — bake
                // before consulting the bag. Same shape the walker's
                // `expr_payload` uses for every rvalue expression.
                if let Some(class) = self.extract_constructor_class(node) {
                    return Some(InferredType::ClassName(class));
                }
                let span = node_to_span(node);
                let idx = self.refs.iter().position(|r| {
                    matches!(r.kind, RefKind::MethodCall { .. }) && r.span == span
                })?;
                // Arity from the method-call node disambiguates fluent
                // accessors (Mojo::Base `has 'title' => 'default'`
                // synthesizes a 0-arg getter returning String AND a
                // 1-arg writer returning $self). Without it the
                // MethodOnClass chase falls through FluentArityDispatch
                // (no matching arm without a hint) and the primary
                // (first-seen sym for the (class, name) pair) wins —
                // which can be the writer's ClassName instead of the
                // getter's String depending on declaration order.
                let arity = self.extract_call_args(node).len() as u32;
                self.bag_query_expression(crate::witnesses::RefIdx(idx as u32), Some(arity))
            }
            "scalar" => {
                let text = node.utf8_text(self.source).ok()?;
                // `$self` short-circuit — the value of the package
                // CONTAINING this node is the canonical answer
                // regardless of whether a TC was seeded for `$self`
                // yet. Use the innermost scope's `package` field
                // (set on package_statement AND class_statement
                // entries) so post-walk callers — where
                // `self.current_package` is stale, holding the
                // walk's last-opened package, not the one
                // surrounding this node — get the right answer.
                if text == "$self" {
                    return self.package_for_node(node).map(InferredType::ClassName);
                }
                // The bag is canonical at every phase, walk-time
                // included: `push_type_constraint` mirrors every TC
                // into a Variable witness live during the walk, so
                // `bag_query_variable` always sees whatever was just
                // seeded by an earlier visit (plus the framework-aware
                // `FirstParam → ClassName` projection).
                let scope = self.scope_at_point(node.start_position());
                self.bag_query_variable(text, scope, node.start_position())
            }
            "bareword" | "package" => {
                let text = node.utf8_text(self.source).ok()?;
                if text == "__PACKAGE__" {
                    return self.package_for_node(node).map(InferredType::ClassName);
                }
                let bare = bare_name(text);
                // Bareword invocant is ambiguous: class-name reference
                // OR zero-arg function call whose return type seeds
                // the chain (`app->routes` where `sub app :: Mojolicious`).
                // Prefer the function-call interpretation; fall back
                // to treating the text as a class.
                if let Some(t) = self.bag_query_named_sub(bare, Some(0)) {
                    return Some(t);
                }
                Some(InferredType::ClassName(text.to_string()))
            }
            // `shift` / `shift()` / `$_[0]` in method-body invocant
            // position all mean `$self`. Use `package_at_pos` for
            // post-walk correctness; same reason as the `$self`
            // case above.
            "func1op_call_expression" if self.is_shift_call(node) => {
                self.package_for_node(node).map(InferredType::ClassName)
            }
            "array_element_expression" => {
                let array = node.child_by_field_name("array")?;
                if array.kind() != "container_variable" { return None; }
                let varname = array.named_child(0)?;
                if varname.utf8_text(self.source).ok() != Some("_") { return None; }
                let index = node.child_by_field_name("index")?;
                if index.utf8_text(self.source).ok() != Some("0") { return None; }
                self.package_for_node(node).map(InferredType::ClassName)
            }
            "function_call_expression" | "ambiguous_function_call_expression" => {
                if self.is_shift_call(node) {
                    return self.package_for_node(node).map(InferredType::ClassName);
                }
                let func = node.child_by_field_name("function")?;
                let name = func.utf8_text(self.source).ok()?;
                let bare = bare_name(name);
                let arg_count = self.extract_call_args(node).len() as u32;
                self.bag_query_named_sub(bare, Some(arg_count))
            }
            _ => None,
        }
    }

    /// Resolve a method-call invocant NODE to a class name. Thin
    /// wrapper over `invocant_type_at_node` that projects the
    /// resulting `InferredType` to a class string. Same caller
    /// contract: callable both walk-time (variable arms read TCs,
    /// the bag has plugin/framework synthesis answers) and
    /// post-walk (the bag is canonical for everything).
    fn resolve_invocant_class_tree(&self, node: Node<'a>) -> Option<String> {
        let t = self.invocant_type_at_node(node)?;
        if let Some(c) = t.class_name() {
            return Some(c.to_string());
        }
        // `FirstParam` projection: a TC carrying `FirstParam{package}`
        // (the `my $self = shift;` idiom) pins the package even
        // when `class_name()` doesn't recognize the variant.
        if let InferredType::FirstParam { package } = t {
            return Some(package);
        }
        // Last fallback for bareword nodes: if the bag returned
        // something non-class, treat the syntactic text as the class.
        // Pre-bag-routing this was the bareword arm's `Some(text.to_string())`
        // tail; preserved here so unrelated InferredType variants
        // (Numeric / String / etc.) on a bareword still degrade to
        // the class-name interpretation rather than vanishing.
        if matches!(node.kind(), "bareword" | "package") {
            return node.utf8_text(self.source).ok().map(|s| s.to_string());
        }
        None
    }
    /// Walk-time helper for plugin emit hooks that need the
    /// receiver's type to decide whether to fire (mojo-events'
    /// `on_method_call` only matches `$emitter->on(...)` when
    /// `$emitter` types as a Mojo::EventEmitter subclass, etc.).
    ///
    /// Thin wrapper over `invocant_type_at_node`. Same node-kind
    /// dispatch the post-walk chain typer uses — variable arms read
    /// TCs (canonical at walk time, mirrored to the bag post-walk),
    /// bareword arms query the local Symbol's return, method-call arms
    /// query Expression(refidx), shift / `$_[0]` / `__PACKAGE__` use
    /// current_package. One function, one dispatch.
    fn receiver_type_for(&self, invocant_node: Option<Node<'a>>) -> Option<InferredType> {
        self.invocant_type_at_node(invocant_node?)
    }

    /// Transitive parent walk within the current file. Depth-limited like
    /// `resolve_method_in_ancestors`. Returns parents in BFS order. Used
    /// for plugin trigger matching so a class that transitively extends
    /// `Mojo::EventEmitter` (via an intermediate base) still fires its
    /// plugins — matches Perl's own MRO behavior.
    fn transitive_parents(&self, pkg: &str) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut stack: Vec<String> = self.package_parents.get(pkg).cloned().unwrap_or_default();
        let mut depth = 0;
        while let Some(p) = stack.pop() {
            if depth > 20 { break; }
            if !seen.insert(p.clone()) { continue; }
            out.push(p.clone());
            if let Some(grandparents) = self.package_parents.get(&p) {
                for gp in grandparents {
                    stack.push(gp.clone());
                }
            }
            depth += 1;
        }
        out
    }

    /// Build the read-only snapshot plugins see, minus the call-shape bits
    /// that only method-call vs function-call callers know.
    fn base_call_context(
        &self,
        args_raw: Vec<Node<'a>>,
        call_span: Span,
        selection_span: Span,
    ) -> plugin::CallContext {
        let args: Vec<plugin::ArgInfo> = args_raw.iter().map(|n| self.arg_info_for(*n)).collect();
        let parents = self.current_package.as_ref()
            .map(|p| self.transitive_parents(p))
            .unwrap_or_default();
        let uses = self.current_package.as_ref()
            .and_then(|p| self.package_uses.get(p))
            .cloned()
            .unwrap_or_default();
        plugin::CallContext {
            call_kind: plugin::CallKind::Function,
            function_name: None,
            method_name: None,
            receiver_text: None,
            receiver_type: None,
            args,
            call_span,
            selection_span,
            current_package: self.current_package.clone(),
            current_package_parents: parents,
            current_package_uses: uses,
        }
    }

    /// Run every applicable plugin's `on_method_call` hook and apply each
    /// returned `EmitAction`.
    /// Run every applicable plugin's `on_use` hook. Used for
    /// `use` statements that autoimport a fixed verb set (Mojolicious::Lite,
    /// Dancer2, etc.) — plugins emit `FrameworkImport` actions per
    /// imported verb so our unresolved-function diagnostic skips them.
    ///
    /// `on_use` bypasses the normal trigger filter: every plugin sees
    /// every use. The plugin checks the module name itself. This is
    /// because `use` statements are the *place where* triggers become
    /// true for a package — the UsesModule("X") trigger wouldn't match
    /// at the exact statement that introduces X.
    fn dispatch_use_plugins(&mut self, ctx: plugin::UseContext) {
        if self.plugins.is_empty() { return; }
        let actions: Vec<(String, plugin::EmitAction)> = self.plugins
            .all()
            .flat_map(|p| {
                let id = p.id().to_string();
                p.on_use(&ctx).into_iter().map(move |a| (id.clone(), a))
            })
            .collect();
        for (plugin_id, action) in actions {
            self.apply_emit_action(plugin_id, action);
        }
    }

    /// Run every applicable plugin's `on_function_call` hook. Used for
    /// top-level calls (`get '/path' => sub {}`, `has 'attr' => ...`)
    /// that aren't method calls. Mirrors dispatch_method_call_plugins.
    fn dispatch_function_call_plugins(&mut self, ctx: plugin::CallContext) {
        if self.plugins.is_empty() { return; }
        let parents = self.current_package.as_ref()
            .map(|p| self.transitive_parents(p))
            .unwrap_or_default();
        let uses = self.current_package.as_ref()
            .and_then(|p| self.package_uses.get(p))
            .cloned()
            .unwrap_or_default();
        let query = plugin::TriggerQuery {
            package_uses: &uses,
            package_parents: &parents,
        };
        let actions: Vec<(String, plugin::EmitAction)> = self.plugins
            .applicable(&query)
            .flat_map(|p| {
                let id = p.id().to_string();
                p.on_function_call(&ctx).into_iter().map(move |a| (id.clone(), a))
            })
            .collect();
        for (plugin_id, action) in actions {
            self.apply_emit_action(plugin_id, action);
        }
    }

    fn dispatch_method_call_plugins(&mut self, ctx: plugin::CallContext) {
        if self.plugins.is_empty() { return; }
        let parents = self.current_package.as_ref()
            .map(|p| self.transitive_parents(p))
            .unwrap_or_default();
        let uses = self.current_package.as_ref()
            .and_then(|p| self.package_uses.get(p))
            .cloned()
            .unwrap_or_default();
        let query = plugin::TriggerQuery {
            package_uses: &uses,
            package_parents: &parents,
        };
        let actions: Vec<(String, plugin::EmitAction)> = self.plugins
            .applicable(&query)
            .flat_map(|p| {
                let id = p.id().to_string();
                p.on_method_call(&ctx).into_iter().map(move |a| (id.clone(), a))
            })
            .collect();
        for (plugin_id, action) in actions {
            self.apply_emit_action(plugin_id, action);
        }
    }

    /// Convert a plugin-produced `EmitAction` into real builder state. All
    /// emitted symbols carry a `Namespace::Framework { id }` tag so downstream
    /// queries can distinguish plugin-synthesized entities from native ones.
    fn apply_emit_action(&mut self, plugin_id: String, action: plugin::EmitAction) {
        let ns = Namespace::framework(plugin_id.clone());
        match action {
            plugin::EmitAction::Method {
                name,
                span,
                selection_span,
                params,
                is_method,
                return_type,
                doc,
                on_class,
                display,
                hide_in_outline,
                opaque_return,
                outline_label,
                return_via_edge,
            } => {
                let return_type_for_bag = return_type.clone();
                let is_class_scoped = on_class.is_some();
                let detail = SymbolDetail::Sub {
                    params: params.into_iter().map(Into::into).collect(),
                    is_method,
                    doc,
                    display,
                    hide_in_outline,
                    opaque_return,
                };
                let target_pkg = on_class.clone().or_else(|| self.current_package.clone());

                let already_emitted = self.symbols.iter().any(|s| {
                    s.name == name
                        && s.kind == SymKind::Method
                        && s.package == target_pkg
                        && s.namespace == ns
                });
                if already_emitted { return; }

                let sid = if let Some(pkg) = on_class {
                    let saved = self.current_package.take();
                    self.current_package = Some(pkg);
                    let id = self.add_symbol_ns(name, SymKind::Method, span, selection_span, detail, ns);
                    self.current_package = saved;
                    id
                } else {
                    self.add_symbol_ns(name, SymKind::Method, span, selection_span, detail, ns)
                };
                if outline_label.is_some() {
                    // Apply to the symbol just pushed. Kept out of
                    // `add_symbol_ns` since outline_label is only
                    // meaningful on plugin-emitted callables; the core
                    // constructor stays narrow.
                    if let Some(s) = self.symbols.iter_mut().find(|s| s.id == sid) {
                        s.outline_label = outline_label;
                    }
                }
                // Mirror the return type into the bag so walk-time and
                // post-walk consumers see the plugin-synthesized sub
                // through the same bag-query path locals + imports
                // already use. Free-function synth gets `Symbol(sid)`;
                // class-scoped synth (`on_class: Some(_)`) skips the
                // bag push — `MethodOnClass{class, name}` is published
                // later by the writeback from `resolved_returns`,
                // keeping same-named methods across unrelated namespaces
                // addressable per-class. Bridges remain the dispatch
                // mechanism for class-scoped plugin synth (CLAUDE.md
                // rule #8).
                if let Some(rt) = return_type_for_bag {
                    use crate::witnesses::{
                        Witness, WitnessAttachment, WitnessPayload, WitnessSource,
                    };
                    self.resolved_returns.insert(sid, rt.clone());
                    if !is_class_scoped {
                        self.bag.push(Witness {
                            attachment: WitnessAttachment::Symbol(sid),
                            source: WitnessSource::Plugin(plugin_id.clone()),
                            payload: WitnessPayload::InferredType(rt),
                            span,
                        });
                    }
                } else if let Some(target_span) = return_via_edge {
                    // Lazy return type: emit `Symbol(sid) → Edge(
                    // Expr(target_span))`. The writeback
                    // (`write_back_sub_return_types`) iterates
                    // symbols and projects whatever payload sits
                    // on each `Symbol(sid)` to `MethodOnClass{class,
                    // name}` for class-scoped methods, so this
                    // single Symbol-attached emission reaches both
                    // attachments. The bag's edge-chase resolver
                    // follows the edge at query time — by then,
                    // the sub body has been walked and
                    // `Expr(target_span)` carries its type.
                    use crate::witnesses::{
                        Witness, WitnessAttachment, WitnessPayload, WitnessSource,
                    };
                    self.bag.push(Witness {
                        attachment: WitnessAttachment::Symbol(sid),
                        source: WitnessSource::Plugin(plugin_id),
                        payload: WitnessPayload::Edge(WitnessAttachment::Expr(target_span)),
                        span,
                    });
                }
            }
            plugin::EmitAction::HashKeyDef { name, owner, span, selection_span } => {
                let detail = SymbolDetail::HashKeyDef { owner, is_dynamic: false };
                self.add_symbol_ns(name, SymKind::HashKeyDef, span, selection_span, detail, ns);
            }
            plugin::EmitAction::HashKeyAccess { name, owner, var_text, span, access } => {
                // `owner: Some(owner)` so the phase-5 linkage pass (which looks
                // for HashKeyAccess → HashKeyDef by name+owner) pairs these
                // refs to both in-file and cross-file defs automatically.
                self.refs.push(Ref {
                    kind: RefKind::HashKeyAccess { var_text, owner: Some(owner) },
                    span,
                    scope: self.current_scope(),
                    target_name: name,
                    access,
                    resolves_to: None,
                });
            }
            plugin::EmitAction::Handler {
                name, owner, dispatchers, params, span, selection_span, display,
                hide_in_outline, outline_label,
            } => {
                let detail = SymbolDetail::Handler {
                    owner,
                    dispatchers,
                    params: params.into_iter().map(Into::into).collect(),
                    display,
                    hide_in_outline,
                };
                let sid = self.add_symbol_ns(name, SymKind::Handler, span, selection_span, detail, ns);
                if outline_label.is_some() {
                    if let Some(s) = self.symbols.iter_mut().find(|s| s.id == sid) {
                        s.outline_label = outline_label;
                    }
                }
            }
            plugin::EmitAction::MethodCallRef { method_name, invocant, span, invocant_span } => {
                // Standard MethodCall ref — gd/gr/hover/rename route to
                // the usual resolution path (inheritance walk + module
                // index + type inference). The plugin's job is just
                // "there's a call to method X on invocant Y here".
                // Plugins declare `invocant` as the intended receiver
                // class (e.g. route plugin uses "Users" for `->to('Users#list')`);
                // treat that as the resolved class unless it's a sigil-shape.
                let invocant_class = if invocant.is_empty()
                    || invocant.starts_with('$')
                    || invocant.starts_with('@')
                    || invocant.starts_with('%')
                {
                    None
                } else {
                    Some(invocant.clone())
                };
                let ref_idx = self.refs.len();
                self.refs.push(Ref {
                    kind: RefKind::MethodCall {
                        invocant,
                        invocant_span,
                        method_name_span: span,
                    },
                    span,
                    scope: self.current_scope(),
                    target_name: method_name,
                    access: AccessKind::Read,
                    resolves_to: None,
                });
                if let Some(c) = invocant_class {
                    self.method_call_invocant.insert(ref_idx, c);
                }
            }
            plugin::EmitAction::DispatchCall { name, dispatcher, owner, span, var_text } => {
                // Same pattern as HashKeyAccess: record the owner so
                // `build_indices` can link the ref to its Handler def in
                // O(1) and `resolve::refs_to` matches cross-file by
                // (owner, name). The var_text lives on the kind for
                // features that want to show the receiver in hover.
                let _ = var_text; // reserved for future hover enrichment
                self.refs.push(Ref {
                    kind: RefKind::DispatchCall { dispatcher, owner: Some(owner) },
                    span,
                    scope: self.current_scope(),
                    target_name: name,
                    access: AccessKind::Read,
                    resolves_to: None,
                });
            }
            plugin::EmitAction::Symbol { name, kind, span, selection_span, detail, return_type } => {
                // The per-symbol return type rides at the action
                // level (not on `SymbolDetail`) since D1 of the
                // bag-residual refactor. We push the type into the
                // bag the same way the `Method` variant does, so the
                // chain typer's bag-routed queries see
                // plugin-synthesized callables (e.g. Mojolicious::Lite's
                // `app` → ClassName(Mojolicious)). One Symbol(sid)
                // push covers every callable kind; the writeback's
                // `MethodOnClass{class, name}` emission picks up
                // class-scoped synths via `resolved_returns`.
                let sid = self.add_symbol_ns(name, kind, span, selection_span, detail, ns);
                if let Some(rt) = return_type {
                    use crate::witnesses::{
                        Witness, WitnessAttachment, WitnessPayload, WitnessSource,
                    };
                    self.resolved_returns.insert(sid, rt.clone());
                    self.bag.push(Witness {
                        attachment: WitnessAttachment::Symbol(sid),
                        source: WitnessSource::Plugin(plugin_id),
                        payload: WitnessPayload::InferredType(rt),
                        span,
                    });
                }
            }
            plugin::EmitAction::PackageParent { package, parent } => {
                self.package_parents.entry(package).or_default().push(parent);
            }
            plugin::EmitAction::FrameworkImport { keyword } => {
                self.framework_imports.insert(keyword);
            }
            plugin::EmitAction::Import { module_name, imported_symbols, span } => {
                // Plugin-synthetic `use` — indistinguishable from a
                // hand-written `use Module qw(name1 name2)` downstream.
                // The whole imported-function machinery (hover, gd,
                // sig-help, unresolved-function diagnostic, completion
                // detail) just works. `qw_close_paren` stays None —
                // there's no qw list to insert into for auto-import.
                self.imports.push(Import {
                    module_name,
                    imported_symbols,
                    span,
                    qw_close_paren: None,
                });
            }
            plugin::EmitAction::VarType { variable, at, inferred_type } => {
                // Scope resolution is deferred to the end of the build —
                // plugin dispatch runs BEFORE we recurse into call
                // arguments, so the callback body's scope doesn't exist
                // yet. Queue the request and apply it once every scope
                // has been pushed.
                self.deferred_var_types.push(DeferredVarType {
                    variable,
                    at,
                    inferred_type,
                });
            }
            plugin::EmitAction::PluginNamespace {
                id,
                kind,
                bridges,
                entity_names,
                decl_span,
            } => {
                // Find-or-create the namespace. Bridges union across
                // repeated emissions so dotted helpers emitted one at
                // a time aggregate into a single namespace; entity_names
                // is resolved now against symbols already emitted by
                // this plugin in THIS dispatch (and any earlier one).
                let plugin_id_for_ns = plugin_id.clone();
                // O(symbols) with O(1) name lookup — the previous
                // `entity_names.iter().any(...)` inside the filter was
                // O(symbols × entity_names). Helpers register dozens
                // of names per app; the quadratic scan compounds.
                let entity_name_set: std::collections::HashSet<&str> =
                    entity_names.iter().map(|s| s.as_str()).collect();
                let entities: Vec<_> = self.symbols.iter()
                    .filter(|s| matches!(
                        &s.namespace,
                        crate::file_analysis::Namespace::Framework { id } if id == &plugin_id_for_ns
                    ))
                    .filter(|s| entity_name_set.contains(s.name.as_str()))
                    .map(|s| s.id)
                    .collect();

                // Namespace identity is (plugin_id, id) — not just `id`.
                // Two plugins that both pick "app" as an id belong to
                // different namespaces; matching only on `id` would
                // silently merge entities and bridges across plugins.
                let existing = self.plugin_namespaces.iter_mut()
                    .find(|n| n.id == id && n.plugin_id == plugin_id_for_ns);
                if let Some(existing) = existing {
                    for b in bridges {
                        if !existing.bridges.contains(&b) {
                            existing.bridges.push(b);
                        }
                    }
                    for e in entities {
                        if !existing.entities.contains(&e) {
                            existing.entities.push(e);
                        }
                    }
                } else {
                    self.plugin_namespaces.push(crate::file_analysis::PluginNamespace {
                        id,
                        plugin_id: plugin_id_for_ns,
                        kind,
                        entities,
                        bridges,
                        decl_span,
                    });
                }
            }
        }
    }

    // ---- Main visitor ----

    fn visit_node(&mut self, node: Node<'a>) {
        match node.kind() {
            "package_statement" => self.visit_package(node),
            "class_statement" => self.visit_class(node),
            "subroutine_declaration_statement" => self.visit_sub(node, false),
            "anonymous_subroutine_expression" => self.visit_anonymous_sub(node),
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
            // $#foo — scalar-shaped but resolves to the underlying @foo.
            // The sigil is `$#`; the varname child holds the bare name.
            "arraylen" => self.visit_arraylen_ref(node),

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
                self.infer_deref_type(node, InferredType::CodeRef { return_edge: None });
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

            // Return expressions → record structural facts pre-visit
            // (scope, arity branch, body span); emit per-expression +
            // per-sub witnesses POST visit_children so the body's refs
            // are already allocated. `expr_payload` for method-call
            // bodies returns `Edge(Expression(refidx))`; finding
            // refidx requires the ref to exist, which requires the
            // walker to have visited the method-call expression.
            "return_expression" => {
                let span = node_to_span(node);
                let body_span = node.named_child(0).map(node_to_span);
                let scope = self.enclosing_sub_scope();
                if let Some(scope) = scope {
                    let arity_branch = classify_arity_branch(node, self.source);
                    self.return_infos.push(ReturnInfo {
                        scope,
                        arity_branch,
                        span,
                        body_span,
                    });
                    // If the return body is `return other()` (a direct call),
                    // record the delegation so hash-key ownership can walk
                    // through the intermediate.
                    if let Some(sub_name) = self.enclosing_sub_name() {
                        if let Some(delegated) = extract_delegated_call_name(node, self.source) {
                            self.sub_return_delegations.insert(sub_name, delegated);
                        }
                    }
                }
                self.visit_children(node);
                if let Some(scope) = scope {
                    self.publish_return_arm_witnesses(node, scope);
                }
            }

            // Expression statements inside sub bodies → track last
            // expression's body span for the implicit-return path.
            // Perl returns the last statement's value, so this IS
            // the sub's implicit return when there's no explicit
            // `return`. Each top-level statement we visit overwrites
            // the prior entry, so when the walk leaves the sub the
            // map points at the genuinely-last statement.
            //
            // IMPORTANT: only statements at the sub body's TOP
            // level count — the outer block must be the sub/method's
            // direct body. The bag-routed delegation chain (`Symbol(_) ←
            // branch_arm Edge → Expr(body) → Edge(call_target)`) handles
            // self-method tails for type inference; no separate map.
            "expression_statement" => {
                self.visit_children(node);
                if let Some(scope) = self.enclosing_sub_scope() {
                    let is_body_top_level = node
                        .parent()
                        .filter(|p| p.kind() == "block")
                        .and_then(|b| b.parent())
                        .map(|gp| {
                            matches!(
                                gp.kind(),
                                "subroutine_declaration_statement"
                                    | "method_declaration_statement"
                                    | "anonymous_subroutine_expression"
                            )
                        })
                        .unwrap_or(false);
                    if is_body_top_level {
                        if let Some(child) = node.named_child(0) {
                            // Make sure the expression has Expr(span)
                            // witnesses populated — `bag_query_expr_span`
                            // resolves through them in the implicit-return
                            // fallback. No-op for compound nodes whose
                            // payload doesn't bake to a witness shape.
                            self.emit_expr_witness(child);
                            self.last_expr_span.insert(scope, node_to_span(child));
                        }
                    }
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

            // ERROR nodes: recover structural declarations (the file's skeleton)
            // but skip expressions/refs which are unreliable inside broken regions
            "ERROR" => self.recover_structural_from_error(node),

            _ => self.visit_children(node),
        }
    }

    /// Recover structural declarations from ERROR nodes.
    /// Only recovers the file's skeleton (packages, imports, subs, classes) —
    /// expressions and refs inside ERROR are unreliable and skipped.
    fn recover_structural_from_error(&mut self, error_node: Node<'a>) {
        for i in 0..error_node.child_count() {
            if let Some(child) = error_node.child(i) {
                match child.kind() {
                    "package_statement" => self.visit_package(child),
                    "use_statement" => self.visit_use(child),
                    "subroutine_declaration_statement" => self.visit_sub(child, false),
                    "method_declaration_statement" => self.visit_sub(child, true),
                    "class_statement" => self.visit_class(child),
                    "ambiguous_function_call_expression" => self.visit_function_call(child),
                    "ERROR" => self.recover_structural_from_error(child),
                    _ => {}
                }
            }
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
        // Capture the pre-existing package BEFORE touching
        // current_package — the block form needs to restore to this
        // value after visiting children. The previous code took()
        // from current_package AFTER already mutating it, so
        // prev_package would hold the NEW value and the restore was
        // a no-op. That leaked `package Foo { ... }`'s name to
        // every statement that followed at the same file scope.
        let prev_package = self.current_package.clone();

        self.add_symbol(
            name.clone(),
            SymKind::Package,
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::None,
        );

        let has_block = (0..node.child_count())
            .any(|i| node.child(i).map_or(false, |c| c.kind() == "block"));
        if has_block {
            // `package Foo { ... }` — record the block as a package
            // range and set current_package for the walk, then
            // restore. The block doesn't push a lexical scope on its
            // own (children will, e.g. via subs/methods inside).
            self.add_fold_range(node);
            self.push_block_package_range(name.clone(), node_to_span(node));
            self.current_package = Some(name);
            self.visit_children(node);
            self.current_package = prev_package;
        } else {
            // `package Foo;` — package context flows to the next
            // sibling `package X;` / `class X;` or end of file.
            // `package_ranges` carries that for `package_at`; the
            // walk-time `current_package` drives synthesised
            // sub/method packages. No lexical scope is pushed —
            // `package Foo;` is not a lexical boundary in Perl.
            self.current_package = Some(name.clone());
            self.open_statement_package_range(name, node.start_position());
        }
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
        // Roles via :does(Role) are also parents for method resolution
        if !roles.is_empty() {
            self.package_parents
                .entry(name.clone())
                .or_default()
                .extend(roles.iter().cloned());
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
            self.push_block_package_range(name.clone(), node_to_span(node));
            self.current_package = Some(name.clone());
            self.push_scope(ScopeKind::Class { name: name.clone() }, node_to_span(node), Some(name));
            self.visit_children(node);
            self.pop_scope();
            self.current_package = prev_package;
        } else {
            // Flat `class Foo;` — same semantics as non-block
            // `package Foo;`: package context flows in
            // `package_ranges`; no lexical scope is pushed. The
            // Class SYMBOL was already emitted above.
            self.current_package = Some(name.clone());
            self.open_statement_package_range(name, node.start_position());
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
        let mut params = self.extract_params(node);

        // Invocant detection for Perl-native subs:
        //   * `method foo { ... }` (v5.38) is always a method — first
        //     positional is the invocant.
        //   * Regular `sub` bodies use two Perl-native signals:
        //       - first positional named `$self`/`$class`/`$this`/`$proto`
        //       - or the enclosing package declares inheritance (a sub
        //         in a subclass is, by Perl OO convention, a method)
        //     Either triggers invocant marking; name stays free so the
        //     user can call it `$c`/`$ctx`/whatever.
        // Framework-specific invocant markers (`as_invocant_params` from
        // a plugin) stack on top via EmittedParam → ParamInfo.
        if let Some(first) = params.first_mut() {
            let name_says_invocant = matches!(
                first.name.as_str(),
                "$self" | "$class" | "$this" | "$proto"
            );
            let pkg_is_subclass = self.current_package
                .as_ref()
                .map_or(false, |p| self.package_parents.contains_key(p));
            if is_method || name_says_invocant || pkg_is_subclass {
                first.is_invocant = true;
            }
        }

        // Extract preceding POD/comment documentation
        let doc = self.extract_preceding_doc(node, &name);

        self.add_symbol(
            name.clone(),
            if is_method { SymKind::Method } else { SymKind::Sub },
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::Sub { params: params.clone(), is_method, doc, display: None, hide_in_outline: false, opaque_return: false },
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
                self.push_type_constraint(TypeConstraint {
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

    /// Walk an `anonymous_subroutine_expression` (a `sub { ... }`
    /// arg literal). No symbol creation (anon subs are values,
    /// not named entities) but DOES push a `Sub` scope around the
    /// body so `enclosing_sub_scope()` returns Some inside it.
    /// Without the scope, `return $x` inside `sub { return $x }`
    /// can't fire the return-arm witness emission, and plugin-
    /// synthesized callables that lazily edge into the body's
    /// `Expr(span)` (mojo-helpers' `return_via_edge`) lose their
    /// target. Conventional name `(anon)` so the scope chain
    /// reads sensibly in `--dump-package`.
    fn visit_anonymous_sub(&mut self, node: Node<'a>) {
        let params = self.extract_params(node);
        self.push_scope(
            ScopeKind::Sub { name: "(anon)".into() },
            node_to_span(node),
            None,
        );
        self.record_signature_params(node, &params);
        self.detect_first_param_type(&params, node);
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
                                    ParamInfo { name, default: None, is_slurpy, is_invocant: false }
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
                    is_invocant: false,
                        });
                        continue;
                    }

                    // Pattern: my $var = $_[N];
                    if let Some(var_name) = self.extract_subscript_param(assign, right) {
                        shift_params.push(ParamInfo {
                            name: var_name,
                            default: None,
                            is_slurpy: false,
                    is_invocant: false,
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
                            params.push(ParamInfo { name: var, default: None, is_slurpy: false, is_invocant: false });
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
                            params.push(ParamInfo { name, default, is_slurpy: false, is_invocant: false });
                        }
                    }
                    "slurpy_parameter" => {
                        if let Some(var) = self.first_var_child(param) {
                            params.push(ParamInfo { name: var, default: None, is_slurpy: true, is_invocant: false });
                        }
                    }
                    "scalar" | "array" | "hash" => {
                        if let Ok(text) = param.utf8_text(self.source) {
                            let is_slurpy = matches!(param.kind(), "array" | "hash");
                            params.push(ParamInfo { name: text.to_string(), default: None, is_slurpy, is_invocant: false });
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

        // Use the `is_invocant` flag set at extract time rather than
        // matching on names. This way `sub list { my ($c) = @_ }` in a
        // controller types `$c` as the current package the same way
        // `sub new { my ($self) = @_ }` always has — the caller side
        // (builder or plugin) owns the invocancy decision.
        if !first.is_invocant { return; }

        if let Some(ref pkg) = self.current_package {
            self.push_type_constraint(TypeConstraint {
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
                        SymbolDetail::Sub { params: vec![], is_method: true, doc: None, display: None, hide_in_outline: false, opaque_return: false },
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
                    is_invocant: false,
                            }],
                            is_method: true,
                            doc: None,
                            display: None,
                    hide_in_outline: false,
                    opaque_return: false,
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
                    var_name.clone(),
                    AccessKind::Declaration,
                );

                // Accumulate loop variable values for constant folding
                // for my $x (qw(a b c)) → $x => ["a", "b", "c"]
                if let Some(list_node) = node.child_by_field_name("list") {
                    let values = self.extract_string_names(list_node);
                    if !values.is_empty() {
                        self.constant_strings.insert(var_name, values);
                    }
                }

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

                // Track uses per-package so `Trigger::UsesModule` matches.
                // Populated before any plugin-dispatch site reads it.
                if let Some(pkg) = self.current_package.as_ref().cloned() {
                    self.package_uses
                        .entry(pkg)
                        .or_default()
                        .push(module_name.to_string());
                }

                // Detect framework mode from use statements
                if let Some(pkg) = self.current_package.as_ref().cloned() {
                    match module_name {
                        "Moo" | "Moo::Role" => {
                            self.framework_modes.insert(pkg, FrameworkMode::Moo);
                            for kw in &["has", "with", "extends", "around", "before", "after"] {
                                self.framework_imports.insert(kw.to_string());
                            }
                        }
                        "Moose" | "Moose::Role" => {
                            self.framework_modes.insert(pkg, FrameworkMode::Moose);
                            for kw in &["has", "with", "extends", "around", "before", "after",
                                        "override", "super", "inner", "augment", "confess", "blessed"] {
                                self.framework_imports.insert(kw.to_string());
                            }
                        }
                        "Mojo::Base" => {
                            // Check args: -strict means no accessors, -base or 'Parent' means MojoBase mode
                            // Collect all args including barewords (which extract_use_import_list skips)
                            let all_args = self.extract_mojo_base_args(node);
                            let is_strict = all_args.iter().any(|a| a == "-strict");
                            if !is_strict {
                                self.framework_modes.insert(pkg.clone(), FrameworkMode::MojoBase);
                                self.framework_imports.insert("has".to_string());
                                // 'Parent' arg (not starting with -) is an inheritance declaration
                                let parents: Vec<String> = all_args.into_iter()
                                    .filter(|s| !s.starts_with('-'))
                                    .collect();
                                if !parents.is_empty() {
                                    let parent_set: std::collections::HashSet<&str> = parents.iter().map(|s| s.as_str()).collect();
                                    self.emit_refs_for_strings(node, &parent_set, RefKind::PackageRef);
                                    self.package_parents
                                        .entry(pkg)
                                        .or_default()
                                        .extend(parents);
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Extract parent classes from `use parent` / `use base`
                if module_name == "parent" || module_name == "base" {
                    if let Some(pkg) = self.current_package.clone() {
                        let (parents, _) = self.extract_use_import_list(node);
                        let parents: Vec<String> = parents.into_iter()
                            .filter(|s| !s.starts_with('-')) // skip -norequire etc.
                            .collect();
                        if !parents.is_empty() {
                            let parent_set: std::collections::HashSet<&str> = parents.iter().map(|s| s.as_str()).collect();
                            self.emit_refs_for_strings(node, &parent_set, RefKind::PackageRef);
                            self.package_parents
                                .entry(pkg)
                                .or_default()
                                .extend(parents);
                        }
                    }
                }

                // Accumulate constant values: use constant NAME => 'val' / qw(a b)
                if module_name == "constant" {
                    self.accumulate_use_constant(node);
                }

                // Extract imported symbols from qw(...) or list
                let (raw_names, qw_close_paren) = self.extract_use_import_list(node);
                // Emit FunctionCall refs for imported symbol names (for goto-def on import args).
                // These refs pin to the module being imported from — the
                // qw list IS the authoritative source.
                if module_name != "parent" && module_name != "base" {
                    if !raw_names.is_empty() {
                        let sym_set: std::collections::HashSet<&str> = raw_names.iter().map(|s| s.as_str()).collect();
                        let kind = RefKind::FunctionCall {
                            resolved_package: Some(module_name.to_string()),
                        };
                        self.emit_refs_for_strings(node, &sym_set, kind);
                    }
                }
                // Tree-sitter parses `qw(a b)` as same-name imports — no
                // syntactic support for `use Foo ( a => { -as => 'b' } )`
                // yet, so this loop is pure `ImportedSymbol::same`. Plugin
                // emissions (below) can produce renaming imports.
                let imported_symbols: Vec<ImportedSymbol> = raw_names
                    .iter()
                    .map(|n| ImportedSymbol::same(n.clone()))
                    .collect();
                self.imports.push(Import {
                    module_name: module_name.to_string(),
                    imported_symbols,
                    span: node_to_span(node),
                    qw_close_paren,
                });

                // Plugin dispatch for use-statements. Mojolicious::Lite
                // autoimports a verb set (`get`, `post`, `helper`, ...)
                // — the plugin emits `Import` actions pointing at the
                // real source module so hover/gd/sig-help flow through
                // the existing imported-function resolution path.
                if !self.plugins.is_empty() {
                    let raw_args = self.extract_mojo_base_args(node);
                    let ctx = plugin::UseContext {
                        module_name: module_name.to_string(),
                        imports: raw_names,
                        raw_args,
                        current_package: self.current_package.clone(),
                        span: node_to_span(node),
                    };
                    self.dispatch_use_plugins(ctx);
                }
            }
        }
        // Don't recurse — use statements don't contain interesting sub-nodes
    }

    /// Check if we're at package scope (file scope or class block, not inside a sub).
    #[allow(dead_code)]
    fn is_package_scope(&self) -> bool {
        for &scope_id in self.scope_stack.iter().rev() {
            match &self.scopes[scope_id.0 as usize].kind {
                ScopeKind::File => return true,
                ScopeKind::Sub { .. } | ScopeKind::Method { .. } => return false,
                _ => continue, // class/block/for-loop at package level are OK
            }
        }
        true // empty stack = file scope
    }

    /// Accumulate `use constant NAME => value` into constant_strings.
    fn accumulate_use_constant(&mut self, node: Node<'a>) {
        // CST: use_statement → module:"constant", list_expression(autoquoted_bareword, value...)
        // Find the list_expression child that contains the constant definition
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "list_expression" {
                    // First named child should be the constant name (autoquoted_bareword)
                    let mut name = None;
                    let mut saw_name = false;
                    for j in 0..child.child_count() {
                        if let Some(c) = child.child(j) {
                            if !c.is_named() { continue; }
                            if !saw_name {
                                if c.kind() == "autoquoted_bareword" || c.kind() == "bareword" {
                                    name = c.utf8_text(self.source).ok().map(|s| s.to_string());
                                }
                                saw_name = true;
                                continue;
                            }
                            // Everything after the name is the value — extract strings
                            if let Some(ref n) = name {
                                let values = self.extract_string_names(c);
                                if !values.is_empty() {
                                    self.constant_strings
                                        .entry(n.clone())
                                        .or_default()
                                        .extend(values);
                                }
                            }
                        }
                    }
                    return;
                }
            }
        }
    }

    /// Extract strings from a node's children: qw(), paren lists, bare strings.
    /// Returns (text, span) pairs where span covers each individual word.
    /// Also handles the case where the node itself is a string/qw (not just its children).
    fn extract_string_list(&self, node: Node<'a>) -> Vec<(String, Span)> {
        // Handle the node itself being a leaf string type
        match node.kind() {
            "quoted_word_list" => {
                let mut results = Vec::new();
                self.extract_qw_word_spans(node, &mut results);
                return results;
            }
            "string_literal" | "interpolated_string_literal" => {
                if let Some(text) = self.extract_string_content(node) {
                    return vec![(text, self.string_content_span(node))];
                }
                return vec![];
            }
            "bareword" | "autoquoted_bareword" => {
                if let Ok(text) = node.utf8_text(self.source) {
                    if let Some(values) = self.resolve_constant_strings(text, 0) {
                        let span = node_to_span(node);
                        return values.into_iter().map(|v| (v, span)).collect();
                    }
                }
                return vec![];
            }
            "array" => {
                if let Ok(text) = node.utf8_text(self.source) {
                    if let Some(values) = self.resolve_constant_strings(text, 0) {
                        let span = node_to_span(node);
                        return values.into_iter().map(|v| (v, span)).collect();
                    }
                }
                return vec![];
            }
            _ => {}
        }
        // Walk children
        let mut results = Vec::new();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "quoted_word_list" => {
                        self.extract_qw_word_spans(child, &mut results);
                    }
                    "string_literal" | "interpolated_string_literal" => {
                        if let Some(text) = self.extract_string_content(child) {
                            results.push((text, self.string_content_span(child)));
                        }
                    }
                    "parenthesized_expression" | "list_expression"
                    | "anonymous_array_expression" => {
                        results.extend(self.extract_string_list(child));
                    }
                    // Constant/variable resolution: barewords and array variables
                    "bareword" | "autoquoted_bareword" => {
                        if let Ok(text) = child.utf8_text(self.source) {
                            if let Some(values) = self.resolve_constant_strings(text, 0) {
                                let span = node_to_span(child);
                                for val in values {
                                    results.push((val, span));
                                }
                            }
                        }
                    }
                    "array" => {
                        if let Ok(text) = child.utf8_text(self.source) {
                            if let Some(values) = self.resolve_constant_strings(text, 0) {
                                let span = node_to_span(child);
                                for val in values {
                                    results.push((val, span));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        results
    }

    /// Extract strings without spans. Convenience for callers that don't need positions.
    fn extract_string_names(&self, node: Node<'a>) -> Vec<String> {
        self.extract_string_list(node).into_iter().map(|(s, _)| s).collect()
    }

    /// Emit refs for string names found in a node's children.
    /// Only emits refs for names in `filter` set.
    fn emit_refs_for_strings(
        &mut self,
        node: Node<'a>,
        filter: &std::collections::HashSet<&str>,
        ref_kind: RefKind,
    ) {
        for (text, span) in self.extract_string_list(node) {
            if filter.contains(text.as_str()) {
                self.add_ref(ref_kind.clone(), span, text, AccessKind::Read);
            }
        }
    }

    /// Extract per-word (text, span) pairs from a quoted_word_list node.
    /// Handles multi-line qw by tracking row/col through whitespace.
    fn extract_qw_word_spans(&self, qw_node: Node<'a>, results: &mut Vec<(String, Span)>) {
        for j in 0..qw_node.named_child_count() {
            if let Some(sc) = qw_node.named_child(j) {
                if sc.kind() == "string_content" {
                    if let Ok(text) = sc.utf8_text(self.source) {
                        let sc_start = sc.start_position();
                        let bytes = text.as_bytes();
                        let mut row = sc_start.row;
                        let mut col = sc_start.column;
                        let mut i = 0;
                        while i < bytes.len() {
                            // Skip whitespace, tracking newlines
                            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                                if bytes[i] == b'\n' {
                                    row += 1;
                                    col = 0;
                                } else {
                                    col += 1;
                                }
                                i += 1;
                            }
                            // Collect word
                            let word_start = (row, col);
                            let word_begin = i;
                            while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
                                col += 1;
                                i += 1;
                            }
                            if word_begin < i {
                                let word = &text[word_begin..i];
                                let span = Span {
                                    start: Point::new(word_start.0, word_start.1),
                                    end: Point::new(row, col),
                                };
                                results.push((word.to_string(), span));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Find the qw close paren position in a use statement (only needed for completion positioning).
    fn find_qw_close_position(&self, node: Node<'a>) -> Option<Point> {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.kind() == "quoted_word_list" {
                    let end = child.end_position();
                    return Some(Point::new(end.row, end.column.saturating_sub(1)));
                }
            }
        }
        None
    }

    /// Resolve a name to its known constant string values.
    /// Recurses through constant references up to max_depth.
    fn resolve_constant_strings(&self, name: &str, depth: u8) -> Option<Vec<String>> {
        if depth > 3 { return None; }
        let values = self.constant_strings.get(name)?;
        let mut resolved = Vec::new();
        for val in values {
            if let Some(expanded) = self.resolve_constant_strings(val, depth + 1) {
                resolved.extend(expanded);
            } else {
                resolved.push(val.clone());
            }
        }
        Some(resolved)
    }

    /// Try to resolve an interpolated string to concrete value(s).
    /// Returns empty vec if any interpolated variable is unknown.
    /// Returns multiple values if a variable resolves to multiple strings (loop var).
    fn try_fold_interpolated_string(&self, node: Node<'a>) -> Vec<String> {
        // Find the string_content child
        let content = match node.named_child(0) {
            Some(c) if c.kind() == "string_content" => c,
            _ => return vec![],
        };

        // Walk the string_content: split into literal text and interpolated variables.
        // Named children are the scalars; text between/around them is literal.
        let content_start = content.start_byte();
        let content_end = content.end_byte();
        let source_bytes = &self.source[content_start..content_end];

        let mut segments: Vec<Vec<String>> = Vec::new();
        let mut pos = 0usize; // position within source_bytes

        for i in 0..content.named_child_count() {
            if let Some(var_node) = content.named_child(i) {
                if var_node.kind() != "scalar" {
                    return vec![]; // complex interpolation, bail
                }
                // Literal text before this variable
                let var_start = var_node.start_byte() - content_start;
                if var_start > pos {
                    let literal = std::str::from_utf8(&source_bytes[pos..var_start]).unwrap_or("");
                    if !literal.is_empty() {
                        segments.push(vec![literal.to_string()]);
                    }
                }
                // Resolve the variable
                let var_text = match var_node.utf8_text(self.source) {
                    Ok(t) => t,
                    Err(_) => return vec![],
                };
                match self.resolve_constant_strings(var_text, 0) {
                    Some(values) if !values.is_empty() => segments.push(values),
                    _ => return vec![],
                }
                pos = var_node.end_byte() - content_start;
            }
        }
        // Literal text after last variable
        if pos < source_bytes.len() {
            let literal = std::str::from_utf8(&source_bytes[pos..]).unwrap_or("");
            if !literal.is_empty() {
                segments.push(vec![literal.to_string()]);
            }
        }

        if segments.is_empty() {
            return vec![];
        }
        // Cartesian product of all segments
        let mut result = vec![String::new()];
        for seg in segments {
            let mut next = Vec::new();
            for prefix in &result {
                for val in &seg {
                    next.push(format!("{}{}", prefix, val));
                }
            }
            result = next;
        }
        result
    }

    /// Extract the import list from a use statement.
    /// Returns (imported_symbols, qw_close_paren_position).
    fn extract_use_import_list(&self, node: Node<'a>) -> (Vec<String>, Option<Point>) {
        let qw_close = self.find_qw_close_position(node);
        let names = self.extract_string_names(node);
        if !names.is_empty() {
            return (names, qw_close);
        }
        (vec![], None)
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
                            parents.extend(self.extract_string_names(child));
                        }
                    }
                    if !parents.is_empty() {
                        // @ISA = replaces (not appends)
                        self.package_parents.insert(pkg.clone(), parents);
                    }
                }
            }

            // Accumulate @EXPORT / @EXPORT_OK assignments
            let var_name = if lhs_text.ends_with("@EXPORT_OK") {
                Some("@EXPORT_OK")
            } else if lhs_text.ends_with("@EXPORT") && !lhs_text.ends_with("@EXPORT_OK") {
                Some("@EXPORT")
            } else {
                None
            };
            if let Some(export_var) = var_name {
                let mut names = Vec::new();
                for i in 0..node.named_child_count() {
                    if let Some(child) = node.named_child(i) {
                        names.extend(self.extract_string_names(child));
                    }
                }
                if !names.is_empty() {
                    if export_var == "@EXPORT" {
                        self.export = names;
                    } else {
                        self.export_ok = names;
                    }
                }
            }

            // Glob assignment inside sub import: *{"$caller::name"} = \&name
            // Detect custom import() that exports via typeglob manipulation.
            if left.kind() == "glob" {
                if self.enclosing_sub_name().as_deref() == Some("import") {
                    for name in self.extract_glob_export_names(left, node) {
                        if !self.export.contains(&name) {
                            self.export.push(name);
                        }
                    }
                }
            }

            // Accumulate array/scalar assignments as constants
            {
                // Strip leading "our " or "my " to get the variable name
                let var_stripped = if lhs_text.starts_with("our ") {
                    &lhs_text[4..]
                } else if lhs_text.starts_with("my ") {
                    &lhs_text[3..]
                } else {
                    lhs_text
                };
                if var_stripped.starts_with('@') {
                    let mut values = Vec::new();
                    for i in 0..node.named_child_count() {
                        if let Some(child) = node.named_child(i) {
                            values.extend(self.extract_string_names(child));
                        }
                    }
                    if !values.is_empty() {
                        self.constant_strings.insert(var_stripped.to_string(), values);
                    }
                } else if var_stripped.starts_with('$') {
                    let var = var_stripped;
                    if let Some(right) = node.child_by_field_name("right") {
                        if right.kind() == "interpolated_string_literal" {
                            // Try interpolated string folding first (has variable refs)
                            let folded = self.try_fold_interpolated_string(right);
                            if !folded.is_empty() {
                                self.constant_strings.insert(var.to_string(), folded);
                            }
                        } else if right.kind() == "string_literal" {
                            // Plain string literal
                            if let Some(text) = self.extract_string_content(right) {
                                self.constant_strings.insert(var.to_string(), vec![text]);
                            }
                        }
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
                let inferred = self.infer_expression_type(right)
                    .or_else(|| self.infer_expression_result_type(right));
                if let Some(it) = inferred {
                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                        self.push_type_constraint(TypeConstraint {
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
                                        // Resolve dynamic method names via constant folding
                                        let method_names = if method.starts_with('$') {
                                            self.resolve_constant_strings(method, 0)
                                                .unwrap_or_else(|| vec![method.to_string()])
                                        } else {
                                            vec![method.to_string()]
                                        };
                                        for mname in method_names {
                                            self.method_call_bindings.push(MethodCallBinding {
                                                variable: vt.clone(),
                                                invocant_var: inv.to_string(),
                                                method_name: mname,
                                                scope: self.current_scope(),
                                                span: node_to_span(node),
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Visit the RHS (may contain refs, calls, etc.)
                self.visit_node(right);

                // Branch-arm detection: if RHS is a ternary, emit
                // per-arm `branch_arm`-source `Edge(Expr(arm_span))`
                // witnesses on the LHS variable, plus the arms' own
                // Expr(span) payloads. POST visit_node — needs the
                // arms' refs to exist so `expr_payload` can resolve
                // `Edge(Expression(refidx))` for method-call arms.
                if right.kind() == "conditional_expression" {
                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                        self.emit_branch_arm_witnesses_for_ternary(&vt, right, node);
                    }
                }
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

    /// Find a local Sub or Method symbol by bare name. Used by
    /// `expr_payload`'s function/bareword arms (walk-time) and
    /// `resolve_forward_call_targets` (post-walk) — both want the same
    /// "is there a local sub I could route a call edge to" answer, so
    /// they share this lookup.
    fn find_callee_symbol(&self, bare: &str) -> Option<SymbolId> {
        self.symbols
            .iter()
            .find(|s| s.name == bare && matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| s.id)
    }

    /// Locate the SymbolId for a Sub/Method named `name` whose body's
    /// inner scope is (an ancestor of) `body_scope`. Scans
    /// `self.symbols` for a matching Sub symbol.
    fn find_sub_symbol_for(&self, name: &str, body_scope: ScopeId) -> Option<SymbolId> {
        // Walk up the scope chain; the Sub scope's own span contains
        // the return. We need the symbol whose selection_span matches
        // the sub name.
        let mut cursor = Some(body_scope);
        while let Some(sid) = cursor {
            let s = &self.scopes[sid.0 as usize];
            if let ScopeKind::Sub { name: n } | ScopeKind::Method { name: n } = &s.kind {
                if n == name {
                    // Find the symbol whose enclosing-scope parent
                    // declared it (sym.scope == s.parent) OR the
                    // symbol's scope equals this one.
                    for sym in &self.symbols {
                        if sym.name == name
                            && matches!(sym.kind, SymKind::Sub | SymKind::Method)
                            && sym.span.start <= s.span.start
                            && s.span.end <= sym.span.end
                        {
                            return Some(sym.id);
                        }
                    }
                    return None;
                }
            }
            cursor = s.parent;
        }
        None
    }

    /// Find the Sub/Method symbol whose body scope IS or CONTAINS
    /// `inner_scope`. Same semantics as `find_sub_symbol_for(name, ...)`
    /// without needing the name in hand. Used by `resolve_return_types`
    /// when emitting deferred `ArityReturn` witnesses keyed by sub.
    fn find_sub_symbol_for_scope(&self, inner_scope: ScopeId) -> Option<SymbolId> {
        let mut cursor = Some(inner_scope);
        while let Some(sid) = cursor {
            let s = &self.scopes[sid.0 as usize];
            if let ScopeKind::Sub { name } | ScopeKind::Method { name } = &s.kind {
                for sym in &self.symbols {
                    if sym.name == *name
                        && matches!(sym.kind, SymKind::Sub | SymKind::Method)
                        && sym.span.start <= s.span.start
                        && s.span.end <= sym.span.end
                    {
                        return Some(sym.id);
                    }
                }
                return None;
            }
            cursor = s.parent;
        }
        None
    }

    /// Compute the right `WitnessPayload` shape for an expression node.
    /// Single dispatch — every rvalue node, whether it appears as a
    /// return body, ternary arm, RHS of assignment, or anywhere else,
    /// runs through here. Two categories:
    ///
    /// - **Closed under syntax** (literals, constructors, arithmetic,
    ///   regexp): walker bakes `InferredType(t)` from the node kind
    ///   alone. There's nothing to resolve — `42` is `Numeric` no
    ///   matter what.
    /// - **Name-dependent / compound** (variables, calls, ternaries):
    ///   walker emits `Edge(...)` to the attachment that carries the
    ///   resolved type. Registry materialization chases at query
    ///   time. Ternaries Edge to their own `Expr(span)` since the
    ///   per-arm witnesses live there.
    fn expr_payload(&mut self, node: Node<'a>) -> Option<crate::witnesses::WitnessPayload> {
        use crate::witnesses::{RefIdx, WitnessAttachment, WitnessPayload};
        match node.kind() {
            // Closed-under-syntax — bake.
            "string_literal" | "interpolated_string_literal" => {
                Some(WitnessPayload::InferredType(InferredType::String))
            }
            "number" => Some(WitnessPayload::InferredType(InferredType::Numeric)),
            "anonymous_hash_expression" => {
                Some(WitnessPayload::InferredType(InferredType::HashRef))
            }
            "anonymous_array_expression" => {
                Some(WitnessPayload::InferredType(InferredType::ArrayRef))
            }
            "quoted_regexp" => Some(WitnessPayload::InferredType(InferredType::Regexp)),
            "anonymous_subroutine_expression" => {
                let return_edge = self.anonymous_sub_body_last_expr_span(node);
                Some(WitnessPayload::InferredType(InferredType::CodeRef { return_edge }))
            }
            "binary_expression"
            | "equality_expression"
            | "relational_expression"
            | "postinc_expression"
            | "preinc_expression"
            | "func0op_call_expression"
            | "func1op_call_expression" => self
                .infer_expression_result_type(node)
                .map(WitnessPayload::InferredType),

            // Variable — Edge to its Variable attachment. Registry
            // materialization routes through `query_variable_type`
            // (scope-walking + framework-aware fold).
            "scalar" => {
                let name = node.utf8_text(self.source).ok()?;
                Some(WitnessPayload::Edge(WitnessAttachment::Variable {
                    name: name.to_string(),
                    scope: self.current_scope(),
                }))
            }

            // Method call — constructor pattern is closed under
            // syntax (`Foo->new` → `ClassName("Foo")`), bake. Otherwise
            // Edge to the call's `Expression(refidx)` attachment;
            // `emit_method_call_return_edges` re-emits
            // `Edge(MethodOnClass{class, method})` there once
            // `invocant_class` is filled, so the chase resolves through.
            "method_call_expression" => {
                if let Some(class) = self.extract_constructor_class(node) {
                    return Some(WitnessPayload::InferredType(InferredType::ClassName(class)));
                }
                let span = node_to_span(node);
                let idx = self.refs.iter().position(|r| {
                    matches!(r.kind, RefKind::MethodCall { .. }) && r.span == span
                })?;
                Some(WitnessPayload::Edge(WitnessAttachment::Expression(RefIdx(
                    idx as u32,
                ))))
            }

            // Function call — Edge to the matching local
            // `Symbol(sid)`. For names that resolve to a local sub
            // (or to a plugin-synthesized sub, since plugin synths
            // also push into `self.symbols` before walk-time use),
            // the chase resolves through. Cross-file imports without
            // a local sym don't pin the Expr(span); chain typing's
            // own resolvers cover the chain-receiver case.
            // Function / bareword / scoped-identifier call — extract the
            // callee's bare name and Edge to its `Symbol(sid)`. Failures
            // (target not yet in symbol table) are recovered post-walk by
            // `resolve_forward_call_targets`; same `forward_callee_name`
            // helper is the source of truth for "what name does this node
            // refer to" so the live and recovery paths can't diverge.
            "function_call_expression"
            | "ambiguous_function_call_expression"
            | "bareword"
            | "scoped_identifier" => {
                let bare = self.forward_callee_name(node)?;
                let sid = self.find_callee_symbol(&bare)?;
                Some(WitnessPayload::Edge(WitnessAttachment::Symbol(sid)))
            }

            // Ternary — Edge to its own Expr(span). The per-arm
            // `branch_arm`-source witnesses live on that attachment;
            // the registry's edge chase + BranchArmFold agree them.
            "conditional_expression" => Some(WitnessPayload::Edge(WitnessAttachment::Expr(
                node_to_span(node),
            ))),

            _ => None,
        }
    }

    /// Emit the `Expr(span)` witnesses for `node` and (recursively)
    /// its sub-arms. For a non-ternary node: one witness on
    /// `Expr(span)` with the node's `expr_payload` (literal type, or
    /// Edge to Variable/Symbol/Expression). For a ternary: two
    /// `branch_arm`-source `Edge(Expr(arm_span))` witnesses on
    /// `Expr(span)`, plus a recursive call per arm so each arm's own
    /// `Expr(span)` is populated. Idempotent on span — multiple
    /// callers (return arm + chain typing + RHS-of-assignment) firing
    /// against the same node produce duplicate witnesses but don't
    /// change query answers, since the latest-wins reducers fold
    /// them all the same way.
    fn emit_expr_witness(&mut self, node: Node<'a>) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        let span = node_to_span(node);
        if node.kind() == "conditional_expression" {
            let arms = [
                node.child_by_field_name("consequent"),
                node.child_by_field_name("alternative"),
            ];
            for arm in arms.into_iter().flatten() {
                // Make sure the arm's own Expr(span) carries its
                // payload — consumers Edge into it from
                // Variable($n)/Symbol(sid) for the `my $n = $c ? A : B`
                // and ternary-return paths.
                self.emit_expr_witness(arm);
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Expr(span),
                    source: WitnessSource::Builder("branch_arm".into()),
                    payload: WitnessPayload::Edge(WitnessAttachment::Expr(node_to_span(arm))),
                    span: node_to_span(arm),
                });
            }
            return;
        }
        if let Some(payload) = self.expr_payload(node) {
            self.bag.push(Witness {
                attachment: WitnessAttachment::Expr(span),
                source: WitnessSource::Builder("expression".into()),
                payload,
                span,
            });
        } else if let Some(name) = self.forward_callee_name(node) {
            // Sub call by bare name whose target wasn't in the symbol
            // table yet — Perl resolves these at symbol-table time, not
            // parse time, so the lookup is legitimately deferred. Queue
            // for `resolve_forward_call_targets` to retry post-walk.
            self.unresolved_call_targets.push(UnresolvedCallTarget {
                name,
                expr_span: span,
            });
        }
    }

    /// Bare callee name for an expression node whose `expr_payload` arm
    /// resolves to `Edge(Symbol(sid))`. `None` for any other kind. Sole
    /// source of truth for "what sub-name does this node reference" —
    /// `expr_payload`'s call arm calls this for the live lookup, and
    /// `emit_expr_witness` calls it again to queue forward-ref retries
    /// when the live lookup misses. Keeping a single extractor is what
    /// makes the live and post-walk paths impossible to diverge.
    fn forward_callee_name(&self, node: Node<'a>) -> Option<String> {
        let raw = match node.kind() {
            "function_call_expression" | "ambiguous_function_call_expression" => {
                node.child_by_field_name("function")?.utf8_text(self.source).ok()?
            }
            "bareword" | "scoped_identifier" => node.utf8_text(self.source).ok()?,
            _ => return None,
        };
        Some(bare_name(raw).to_string())
    }

    /// Post-walk: re-resolve every queued `(name, expr_span)` pair
    /// against the now-final symbol table and push the
    /// `Expr(span) → Edge(Symbol(sid))` witness that
    /// `emit_expr_witness` would have pushed live if the target had
    /// existed. Same source tag (`expression`) and span as the
    /// walk-time path so reducers can't tell the two emission sites
    /// apart. Names that still don't resolve (cross-file imports
    /// without a local sym, typos) are silently dropped — the bag is
    /// monotone, no negative witnesses.
    fn resolve_forward_call_targets(&mut self) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        let queued = std::mem::take(&mut self.unresolved_call_targets);
        for entry in queued {
            let Some(sid) = self.find_callee_symbol(&entry.name) else {
                continue;
            };
            self.bag.push(Witness {
                attachment: WitnessAttachment::Expr(entry.expr_span),
                source: WitnessSource::Builder("expression".into()),
                payload: WitnessPayload::Edge(WitnessAttachment::Symbol(sid)),
                span: entry.expr_span,
            });
        }
    }

    /// Publish witnesses for one explicit `return EXPR`:
    /// - `Expr(body_span)` is populated by `emit_expr_witness` —
    ///   directly with the body's payload for non-ternary, or
    ///   recursively with per-arm `branch_arm` Edges for ternary.
    /// - `Symbol(sub_id)` gets a single `branch_arm`-source
    ///   `Edge(Expr(body_span))` witness. The registry's edge chase +
    ///   per-attachment reducer (BranchArmFold for ternary `Expr`,
    ///   straight payload otherwise) folds the body. No ternary
    ///   special-case here — `emit_expr_witness` owns it on the `Expr`
    ///   side.
    fn publish_return_arm_witnesses(&mut self, return_node: Node<'a>, scope: ScopeId) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        let body = match return_node.named_child(0) {
            Some(b) => b,
            None => return,
        };
        let body_span = node_to_span(body);
        self.emit_expr_witness(body);

        let Some(sub_name) = self.enclosing_sub_name() else { return };
        let Some(sym_id) = self.find_sub_symbol_for(&sub_name, scope) else { return };

        self.bag.push(Witness {
            attachment: WitnessAttachment::Symbol(sym_id),
            source: WitnessSource::Builder("branch_arm".into()),
            payload: WitnessPayload::Edge(WitnessAttachment::Expr(body_span)),
            span: body_span,
        });
    }

    /// RHS-ternary convenience: `my $x = $c ? A : B` → one
    /// `chain_assignment`-source `Edge(Expr(ternary_span))` on
    /// Variable($x). `emit_expr_witness(cond_expr)` recurses to
    /// populate the ternary's per-arm `branch_arm` Edges on
    /// `Expr(ternary_span)`; BranchArmFold reduces them during edge
    /// chase, and the materialized Variable witness goes through
    /// FrameworkAwareTypeFold (which excludes `branch_arm` sources).
    /// Source must NOT be `branch_arm` here — there's only one Edge
    /// from the variable, BranchArmFold's ≥2-arm rule would reject it.
    fn emit_branch_arm_witnesses_for_ternary(
        &mut self,
        lhs_var: &str,
        cond_expr: Node<'a>,
        context: Node<'a>,
    ) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        let scope = self.current_scope();
        let context_span = node_to_span(context);
        self.emit_expr_witness(cond_expr);
        // Zero-span at the assignment start: the synthetic InferredType
        // witness produced by edge materialization inherits this span,
        // and `FrameworkAwareTypeFold`'s point-contains filter only
        // skips *non-zero* spans that miss the query point. Using the
        // same convention as TC-mirror Variable witnesses.
        self.bag.push(Witness {
            attachment: WitnessAttachment::Variable { name: lhs_var.to_string(), scope },
            source: WitnessSource::Builder("chain_assignment".into()),
            payload: WitnessPayload::Edge(WitnessAttachment::Expr(node_to_span(cond_expr))),
            span: Span { start: context_span.start, end: context_span.start },
        });
    }

    /// Closed-under-syntax type observation for an expression node.
    /// The walker observes; it does not chase names. Only kinds whose
    /// type is determined by the syntax alone:
    ///
    /// - Literals (`"foo"`, `42`, `{}`, `[]`, `qr//`).
    /// - Anonymous subs → `CodeRef`.
    /// - Constructor pattern `Bareword->new` → `ClassName`.
    ///
    /// Variables, function calls, name-driven method calls — all
    /// excluded. They go through `expr_payload` as `Edge` payloads
    /// pointing at attachments the bag knows.
    fn infer_expression_type(&self, node: Node<'a>) -> Option<InferredType> {
        match node.kind() {
            "string_literal" | "interpolated_string_literal" => Some(InferredType::String),
            "number" => Some(InferredType::Numeric),
            "anonymous_hash_expression" => Some(InferredType::HashRef),
            "anonymous_array_expression" => Some(InferredType::ArrayRef),
            "quoted_regexp" => Some(InferredType::Regexp),
            "anonymous_subroutine_expression" => Some(InferredType::CodeRef {
                return_edge: self.anonymous_sub_body_last_expr_span(node),
            }),
            "method_call_expression" => {
                self.extract_constructor_class(node).map(InferredType::ClassName)
            }
            _ => None,
        }
    }

    /// What does this anonymous-sub return, by inspecting the body's
    /// last expression? Closed under syntax — recurses through
    /// `infer_expression_type` only, no name lookup. Used by Mojo
    /// `has 'x' => sub { [] }` to type the getter's return.
    fn infer_anonymous_sub_return_type(&self, node: Node<'a>) -> Option<InferredType> {
        if node.kind() != "anonymous_subroutine_expression" {
            return None;
        }
        let body = node.child_by_field_name("body")?;
        let last_stmt = body.named_child(body.named_child_count().checked_sub(1)?)?;
        let expr = match last_stmt.kind() {
            "expression_statement" | "return_expression" => last_stmt.named_child(0)?,
            _ => last_stmt,
        };
        self.infer_expression_type(expr)
            .or_else(|| self.infer_anonymous_sub_return_type(expr))
    }

    /// Push a type constraint on a variable node if it's a scalar.
    fn push_var_type_constraint(&mut self, var_node: Node<'a>, context_node: Node<'a>, inferred_type: InferredType) {
        if var_node.kind() == "scalar" {
            if let Ok(text) = var_node.utf8_text(self.source) {
                self.push_type_constraint(TypeConstraint {
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
                        return Some((InferredType::CodeRef { return_edge: None }, outer));
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

    /// `$#foo` — arraylen. TSP gives us a distinct `arraylen` node
    /// with a `varname` child; the access resolves to `@foo`. We emit
    /// a ContainerAccess ref so goto-def and rename treat it like
    /// any other indirect access into the array.
    fn visit_arraylen_ref(&mut self, node: Node<'a>) {
        let bare = match find_varname_child(node).and_then(|v| v.utf8_text(self.source).ok()) {
            Some(b) => b,
            None => return,
        };
        let access = self.determine_access(node);
        self.add_ref(
            RefKind::ContainerAccess,
            node_to_span(node),
            format!("@{}", bare),
            access,
        );
    }

    fn visit_function_call(&mut self, node: Node<'a>) {
        if let Some(func_node) = node.child_by_field_name("function") {
            if let Ok(name) = func_node.utf8_text(self.source) {
                let resolved_package = self.resolve_call_package(name);
                self.add_ref(
                    RefKind::FunctionCall { resolved_package: resolved_package.clone() },
                    node_to_span(func_node),
                    name.to_string(),
                    AccessKind::Read,
                );
                // Even-position stringy args → HashKeyAccess refs
                // owned by `Sub{resolved_package, name}`. Same
                // mechanism as method-call args; covers
                // `Foo::new(key => val)` and helpers like
                // `connect(timeout => 30)`.
                if let Some(args) = node.child_by_field_name("arguments") {
                    let owner = HashKeyOwner::Sub {
                        package: resolved_package.clone(),
                        name: name.to_string(),
                    };
                    self.emit_call_arg_key_accesses(args, Gate::Strict(owner));
                }
                // Push type constraints on arguments of known builtins
                if let Some(arg_type) = crate::file_analysis::builtin_first_arg_type(name) {
                    if let Some(first_arg) = self.first_call_arg(node) {
                        self.push_var_type_constraint(first_arg, node, arg_type);
                    }
                }
                // Framework accessor synthesis: `has` calls in Moo/Moose/Mojo::Base packages
                if name == "has" {
                    if let Some(mode) = self.current_package.as_ref()
                        .and_then(|pkg| self.framework_modes.get(pkg).copied())
                    {
                        self.visit_has_call(node, mode);
                    }
                }
                // Moose/Moo `extends 'Parent'` — register parent classes
                if name == "extends" {
                    if let Some(pkg) = self.current_package.clone() {
                        if self.framework_modes.contains_key(&pkg) {
                            self.visit_extends_call(node, &pkg);
                        }
                    }
                }
                // Moose/Moo `with 'Role'` — register roles as parents for method resolution
                if name == "with" {
                    if let Some(pkg) = self.current_package.clone() {
                        if self.framework_modes.contains_key(&pkg) {
                            self.visit_extends_call(node, &pkg);
                        }
                    }
                }
                // push @EXPORT, 'foo', 'bar' / push @EXPORT_OK, 'foo'
                if name == "push" {
                    self.visit_push_call(node);
                }

                // Framework plugin dispatch for function calls. Mirrors
                // the method-call path so plugins (e.g. Mojolicious::Lite
                // routes: `get '/path' => sub {}`) get the same
                // trigger/ctx/EmitAction flow as method-call plugins.
                if !self.plugins.is_empty() {
                    let args_raw = self.extract_call_args(node);
                    let func_name_span = node_to_span(func_node);
                    let mut ctx = self.base_call_context(
                        args_raw,
                        node_to_span(node),
                        func_name_span,
                    );
                    ctx.call_kind = plugin::CallKind::Function;
                    ctx.function_name = Some(name.to_string());
                    self.dispatch_function_call_plugins(ctx);
                }
            }
        }
        self.visit_children(node);
    }

    /// Handle `push @EXPORT, 'foo', 'bar'` — append to export lists.
    fn visit_push_call(&mut self, node: Node<'a>) {
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };
        // First arg should be the array, rest are values
        let children: Vec<Node> = if args.kind() == "list_expression" {
            (0..args.child_count()).filter_map(|i| args.child(i)).filter(|c| c.is_named()).collect()
        } else {
            return;
        };
        if children.is_empty() { return; }
        let first = children[0];
        if first.kind() != "array" { return; }
        let arr_name = match first.utf8_text(self.source) {
            Ok(t) => t,
            Err(_) => return,
        };
        let is_export = arr_name.ends_with("@EXPORT") && !arr_name.ends_with("@EXPORT_OK");
        let is_export_ok = arr_name.ends_with("@EXPORT_OK");
        if !is_export && !is_export_ok { return; }

        // Extract string values from remaining args
        let mut values = Vec::new();
        for child in &children[1..] {
            values.extend(self.extract_string_names(*child));
        }
        if !values.is_empty() {
            if is_export {
                self.export.extend(values);
            } else {
                self.export_ok.extend(values);
            }
        }
    }

    /// Extract exported function name(s) from a glob assignment in `sub import`.
    /// Handles: `*{"${caller}::np"} = \&np`, `*{"$caller\::$imported"} = \&p`,
    /// `*{$caller . '::confess'} = \&confess`, and loop patterns.
    /// Returns one or more caller-visible names (from the glob string after "::"),
    /// falling back to the RHS \&name if the glob name is dynamic.
    fn extract_glob_export_names(&self, glob_node: Node<'a>, assign_node: Node<'a>) -> Vec<String> {
        // Try to extract from glob's interpolated string: the name after "::"
        // AST: glob > varname > block > expression_statement > interpolated_string_literal > string_content
        let names = self.extract_names_from_glob(glob_node);
        if !names.is_empty() {
            return names;
        }

        // Fallback: extract function name from RHS \&name
        // AST: refgen_expression > function > varname
        if let Some(right) = assign_node.child_by_field_name("right") {
            return self.extract_names_from_refgen(right);
        }
        vec![]
    }

    /// Walk the glob's interpolated string AST to find the exported name(s) after "::".
    fn extract_names_from_glob(&self, glob_node: Node<'a>) -> Vec<String> {
        // glob > varname > block > expression_statement > interpolated_string_literal
        let content = (|| {
            let varname = glob_node.named_child(0)?;
            let block = varname.named_child(0)?;
            let expr_stmt = block.named_child(0)?;
            let interp = expr_stmt.named_child(0)?;
            if interp.kind() != "interpolated_string_literal" { return None; }
            let c = interp.named_child(0)?;
            if c.kind() == "string_content" { Some(c) } else { None }
        })();
        let content = match content {
            Some(c) => c,
            None => return vec![],
        };

        // Walk string_content: find the last "::" in literal segments,
        // then the part after it is the exported name (literal or variable).
        let content_bytes = &self.source[content.start_byte()..content.end_byte()];
        let content_text = match std::str::from_utf8(content_bytes) {
            Ok(t) => t,
            Err(_) => return vec![],
        };

        // Find position of last "::" in the raw content
        let colons_pos = match content_text.rfind("::") {
            Some(p) => p,
            None => return vec![],
        };
        let after_colons = colons_pos + 2;

        // Check if there's a named child (scalar variable) that starts after the "::"
        let last_idx = match content.named_child_count().checked_sub(1) {
            Some(i) => i,
            None => return vec![],
        };
        let last_child = match content.named_child(last_idx) {
            Some(c) => c,
            None => return vec![],
        };
        if last_child.kind() == "scalar" && last_child.start_byte() >= content.start_byte() + after_colons {
            // The name is a variable like $imported or $name — resolve via constant folding.
            // Use scalar > varname to get the canonical name (without ${} braces).
            if let Some(varname_node) = last_child.named_child(0) {
                let bare_name = varname_node.utf8_text(self.source).unwrap_or("");
                let lookup_key = format!("${}", bare_name);
                if let Some(values) = self.resolve_constant_strings(&lookup_key, 0) {
                    return values;
                }
            }
            return vec![];
        }

        // The name is literal text after "::"
        let suffix = &content_text[after_colons..];
        if !suffix.is_empty() && !suffix.starts_with('$') {
            return vec![suffix.to_string()];
        }

        vec![]
    }

    /// Extract bare function name(s) from a `\&name` or `\&$var` refgen expression.
    fn extract_names_from_refgen(&self, refgen: Node<'a>) -> Vec<String> {
        if refgen.kind() != "refgen_expression" {
            return vec![];
        }
        let func = match refgen.named_child(0) {
            Some(f) if f.kind() == "function" => f,
            _ => return vec![],
        };
        // function > varname, possibly containing a scalar child (for \&$name)
        let varname = match func.named_child(0) {
            Some(v) => v,
            None => return vec![],
        };
        if let Some(scalar) = varname.named_child(0) {
            if scalar.kind() == "scalar" {
                // \&$name — resolve variable via scalar > varname (canonical, no ${} braces)
                if let Some(scalar_varname) = scalar.named_child(0) {
                    let bare_name = scalar_varname.utf8_text(self.source).unwrap_or("");
                    let lookup_key = format!("${}", bare_name);
                    if let Some(values) = self.resolve_constant_strings(&lookup_key, 0) {
                        return values;
                    }
                }
                return vec![];
            }
        }
        // Bare name like \&np — varname text is the function name
        match varname.utf8_text(self.source) {
            Ok(name) => vec![name.to_string()],
            Err(_) => vec![],
        }
    }

    /// Synthesize accessor methods from `has` calls in Moo/Moose/Mojo::Base classes.
    /// Handle Moose/Moo `extends 'Parent::Class', ...` — register parent classes.
    fn visit_extends_call(&mut self, node: Node<'a>, pkg: &str) {
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };
        let parents = self.extract_string_names(args);
        if !parents.is_empty() {
            let parent_set: std::collections::HashSet<&str> = parents.iter().map(|s| s.as_str()).collect();
            self.emit_refs_for_strings(node, &parent_set, RefKind::PackageRef);
            self.package_parents
                .entry(pkg.to_string())
                .or_default()
                .extend(parents);
        }
    }

    /// Handle `__PACKAGE__->load_components('+Full::Name', 'Short::Name')`.
    /// Bare names are prefixed with `DBIx::Class::`, `+` prefix means fully qualified.
    fn visit_load_components(&mut self, node: Node<'a>) {
        let pkg = match self.current_package.clone() {
            Some(p) => p,
            None => return,
        };
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };
        let components: Vec<String> = self.extract_string_names(args).into_iter()
            .map(|name| {
                if let Some(stripped) = name.strip_prefix('+') {
                    stripped.to_string()
                } else {
                    format!("DBIx::Class::{}", name)
                }
            })
            .collect();
        if !components.is_empty() {
            self.package_parents
                .entry(pkg)
                .or_default()
                .extend(components);
        }
    }

    /// Publish a framework-synthesized accessor's return type into the
    /// witness bag instead of relying solely on `Symbol.return_type`.
    ///
    /// `apply_type_overrides` (the plugin path) writes Plugin-priority
    /// witnesses on `Symbol(_)` so the bag — the spec's "single
    /// type-query path" — answers consistently with the symbol's
    /// stored type. Framework accessors used to skip this and only set
    /// `Symbol.return_type` directly. The dump-package round of QA
    /// caught two consequences:
    ///
    ///   1. `symbol_witness_count: 0` for every Mojo::Base / Moo / DBIC
    ///      accessor — bag-level introspection couldn't see them.
    ///   2. Multi-arity sisters (Mojo::Base getter `name()` + writer
    ///      `name($v)`) share a method name. `query_sub_return_type`
    ///      finds the first `Symbol` by name and folds witnesses on
    ///      that id; the writer's distinct return type was never
    ///      visible. `level(1)` returned the getter's `String` instead
    ///      of the invocant's `Mojo::Log`.
    ///
    /// This helper publishes both witnesses and the per-symbol
    /// provenance entry. Two attachment shapes cover the query
    /// surface:
    ///   - `Symbol(sym_id)` — per-symbol queries with an arity hint
    ///     (e.g. the dump-package iteration); also bumps the
    ///     symbol's witness count.
    ///   - `MethodOnClass{class, name}` — cross-symbol arity dispatch
    ///     scoped to the declaring class. The writer's `Some(1)` and
    ///     the getter's `Some(0)` both attach here, and the reducer
    ///     picks by `arity_hint` regardless of which sister `find()`
    ///     returned. Class-keyed so `Sweet::flavor` and
    ///     `Sour::flavor` stay addressable independently.
    ///
    /// Source is `Builder("framework_accessor")` — core synthesis,
    /// not a Plugin override. Provenance is `FrameworkSynthesis`
    /// for the same reason; plugins are user-installed and
    /// configurable, framework `has` synthesis is part of the
    /// analyzer itself.
    fn record_framework_accessor_witness(
        &mut self,
        sym_id: SymbolId,
        name: &str,
        arity: Option<u32>,
        return_type: Option<InferredType>,
        framework: &str,
        reason: String,
    ) {
        use crate::witnesses::{
            TypeObservation, Witness, WitnessAttachment, WitnessPayload, WitnessSource,
        };
        let Some(rt) = return_type else { return };
        // Walk-time synthesis seeds the per-symbol return type
        // directly into `resolved_returns` — this is what the
        // writeback iterates to publish `Symbol(sid)` /
        // `MethodOnClass{class, name}` witnesses. Replaces the old
        // `SymbolDetail::Sub.return_type` field.
        self.resolved_returns.insert(sym_id, rt.clone());
        let zero = Span {
            start: Point { row: 0, column: 0 },
            end: Point { row: 0, column: 0 },
        };
        let observation = TypeObservation::ArityReturn {
            arg_count: arity,
            return_type: rt,
        };
        self.bag.push(Witness {
            attachment: WitnessAttachment::Symbol(sym_id),
            source: WitnessSource::Builder("framework_accessor".to_string()),
            payload: WitnessPayload::Observation(observation.clone()),
            span: zero,
        });
        if let Some(class) = self.current_package.clone() {
            self.bag.push(Witness {
                attachment: WitnessAttachment::MethodOnClass {
                    class,
                    name: name.to_string(),
                },
                source: WitnessSource::Builder("framework_accessor".to_string()),
                payload: WitnessPayload::Observation(observation),
                span: zero,
            });
        }
        self.type_provenance.insert(
            sym_id,
            TypeProvenance::FrameworkSynthesis {
                framework: framework.to_string(),
                reason,
            },
        );
    }

    fn visit_has_call(&mut self, node: Node<'a>, mode: FrameworkMode) {
        // Extract attribute names and options from the `has` call arguments.
        // CST: ambiguous_function_call_expression
        //   function: "has"
        //   arguments: list_expression
        //     [0] string_literal 'name' | array_ref_expression [qw(a b)]
        //     [1] list_expression (is => 'ro', isa => 'Str')   -- options (Moo/Moose)
        //          OR absent (Mojo::Base: has 'name' or has 'name' => 'default')
        let mut attr_names: Vec<(String, Span)> = Vec::new();
        let mut is_value: Option<String> = None;
        let mut isa_value: Option<String> = None;
        let mut mojo_default_node: Option<Node<'a>> = None;

        // Get the arguments node
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };

        // The arguments node might be a list_expression or a single node
        let args_children: Vec<Node> = if args.kind() == "list_expression" || args.kind() == "parenthesized_expression" {
            (0..args.child_count()).filter_map(|i| args.child(i)).collect()
        } else {
            // Single argument (e.g., has 'name')
            vec![args]
        };

        let mut found_first_arg = false;
        for child in &args_children {
            if !child.is_named() { continue; }

            if !found_first_arg {
                found_first_arg = true;
                match child.kind() {
                    "string_literal" | "interpolated_string_literal" => {
                        if let Some(text) = self.extract_string_content(*child) {
                            if !text.starts_with('+') {
                                attr_names.push((text, self.string_content_span(*child)));
                            }
                        }
                    }
                    "bareword" | "autoquoted_bareword" => {
                        if let Ok(text) = child.utf8_text(self.source) {
                            attr_names.push((text.to_string(), node_to_span(*child)));
                        }
                    }
                    "array_ref_expression" | "anonymous_array_expression" => {
                        self.extract_array_attr_names(*child, &mut attr_names);
                    }
                    _ => {}
                }
                continue;
            }

            // After first arg: options (Moo/Moose) or default value (Mojo::Base)
            if mode == FrameworkMode::MojoBase {
                // Capture default value node for type inference
                if mojo_default_node.is_none() {
                    mojo_default_node = Some(*child);
                }
            } else {
                match child.kind() {
                    "list_expression" | "parenthesized_expression" => {
                        let pairs = self.extract_fat_comma_pairs(*child);
                        for (key, val) in &pairs {
                            match key.as_str() {
                                "is" => is_value = Some(val.clone()),
                                "isa" => isa_value = Some(val.clone()),
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if attr_names.is_empty() { return; }

        // Map isa value to InferredType
        let return_type = match mode {
            FrameworkMode::Moo | FrameworkMode::Moose => {
                isa_value.as_deref().and_then(|isa| self.map_isa_to_type(isa, mode))
            }
            FrameworkMode::MojoBase => {
                // Fluent return: ClassName(current_package)
                self.current_package.as_ref().map(|pkg| InferredType::ClassName(pkg.clone()))
            }
        };

        // Determine what accessors to synthesize
        match mode {
            FrameworkMode::Moo | FrameworkMode::Moose => {
                let is = is_value.as_deref();
                match is {
                    Some("bare") => return, // no accessor
                    None => return,         // no `is` = no accessor (Moo/Moose default)
                    _ => {}
                }
                let is_rw = matches!(is, Some("rw"));
                let is_rwp = matches!(is, Some("rwp"));

                let framework = match mode {
                    FrameworkMode::Moo => "Moo",
                    FrameworkMode::Moose => "Moose",
                    FrameworkMode::MojoBase => unreachable!(),
                };
                for (name, sel_span) in &attr_names {
                    // Getter (always present for ro/rw/lazy/rwp)
                    let getter_id = self.add_symbol(
                        name.clone(),
                        SymKind::Method,
                        node_to_span(node),
                        *sel_span,
                        SymbolDetail::Sub {
                            params: vec![],
                            is_method: true,
                            doc: None,
                            display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                        },
                    );
                    self.record_framework_accessor_witness(
                        getter_id,
                        name,
                        Some(0),
                        return_type.clone(),
                        framework,
                        format!("{} `has '{}'` getter (isa)", framework, name),
                    );
                    // Setter for rw
                    if is_rw {
                        let writer_id = self.add_symbol(
                            name.clone(),
                            SymKind::Method,
                            node_to_span(node),
                            *sel_span,
                            SymbolDetail::Sub {
                                params: vec![ParamInfo {
                                    name: "$val".into(),
                                    default: None,
                                    is_slurpy: false,
                    is_invocant: false,
                                }],
                                is_method: true,
                                doc: None,
                                display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                            },
                        );
                        self.record_framework_accessor_witness(
                            writer_id,
                            name,
                            Some(1),
                            return_type.clone(),
                            framework,
                            format!("{} `has '{}'` rw writer", framework, name),
                        );
                    }
                    // Private writer for rwp (Moo only)
                    if is_rwp {
                        let writer_name = format!("_set_{}", name);
                        let writer_id = self.add_symbol(
                            writer_name.clone(),
                            SymKind::Method,
                            node_to_span(node),
                            *sel_span,
                            SymbolDetail::Sub {
                                params: vec![ParamInfo {
                                    name: "$val".into(),
                                    default: None,
                                    is_slurpy: false,
                    is_invocant: false,
                                }],
                                is_method: true,
                                doc: None,
                                display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                            },
                        );
                        self.record_framework_accessor_witness(
                            writer_id,
                            &writer_name,
                            Some(1),
                            return_type.clone(),
                            framework,
                            format!("{} `has '{}'` rwp private writer", framework, name),
                        );
                    }
                }
            }
            FrameworkMode::MojoBase => {
                // Infer getter return type from default value if present
                let getter_type = mojo_default_node.and_then(|n| {
                    if n.kind() == "anonymous_subroutine_expression" {
                        self.infer_anonymous_sub_return_type(n)
                    } else {
                        self.infer_expression_type(n)
                    }
                });
                let fluent_type = self
                    .current_package
                    .as_ref()
                    .map(|pkg| InferredType::ClassName(pkg.clone()));
                let framework = "Mojo::Base";

                // Mojo::Base `has` produces getter + setter (two symbols)
                for (name, sel_span) in &attr_names {
                    // Getter: no params, return type from default value (or None)
                    let getter_id = self.add_symbol(
                        name.clone(),
                        SymKind::Method,
                        node_to_span(node),
                        *sel_span,
                        SymbolDetail::Sub {
                            params: vec![],
                            is_method: true,
                            doc: None,
                            display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                        },
                    );
                    self.record_framework_accessor_witness(
                        getter_id,
                        name,
                        Some(0),
                        getter_type.clone(),
                        framework,
                        format!("Mojo::Base `has '{}'` getter (default-value type)", name),
                    );
                    // Setter: fluent, returns $self for chaining
                    let writer_id = self.add_symbol(
                        name.clone(),
                        SymKind::Method,
                        node_to_span(node),
                        *sel_span,
                        SymbolDetail::Sub {
                            params: vec![ParamInfo {
                                name: "$val".into(),
                                default: None,
                                is_slurpy: false,
                    is_invocant: false,
                            }],
                            is_method: true,
                            doc: None,
                            display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                        },
                    );
                    self.record_framework_accessor_witness(
                        writer_id,
                        name,
                        Some(1),
                        fluent_type.clone(),
                        framework,
                        format!("Mojo::Base `has '{}'` fluent writer (returns invocant)", name),
                    );
                }
            }
        }

        // Synthesize HashKeyDef entries so Foo->new(name => ...) connects to the attribute.
        if let Some(ref _pkg) = self.current_package {
            let owner = HashKeyOwner::Sub {
                package: self.current_package.clone(),
                name: "new".to_string(),
            };
            for (name, sel_span) in &attr_names {
                self.add_symbol(
                    name.clone(),
                    SymKind::HashKeyDef,
                    node_to_span(node),
                    *sel_span,
                    SymbolDetail::HashKeyDef {
                        owner: owner.clone(),
                        is_dynamic: false,

                    },
                );
            }
        }
    }

    /// Extract arguments from `use Mojo::Base ...` including barewords like -strict, -base.
    fn extract_mojo_base_args(&self, node: Node<'a>) -> Vec<String> {
        let mut args = Vec::new();
        let module_end = node.child_by_field_name("module")
            .map(|m| m.end_byte())
            .unwrap_or(0);
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.start_byte() <= module_end { continue; }
                match child.kind() {
                    "autoquoted_bareword" | "bareword" => {
                        if let Ok(text) = child.utf8_text(self.source) {
                            args.push(text.to_string());
                        }
                    }
                    "string_literal" | "interpolated_string_literal" => {
                        if let Some(text) = self.extract_string_content(child) {
                            args.push(text);
                        }
                    }
                    _ => {}
                }
            }
        }
        if args.is_empty() {
            // Fallback to standard extraction
            let (standard, _) = self.extract_use_import_list(node);
            return standard;
        }
        args
    }

    /// Extract string content from a string_literal node (strips quotes).
    fn extract_string_content(&self, node: Node<'a>) -> Option<String> {
        for i in 0..node.named_child_count() {
            if let Some(content) = node.named_child(i) {
                if content.kind() == "string_content" {
                    return content.utf8_text(self.source).ok().map(|s| s.to_string());
                }
            }
        }
        None
    }

    /// Get the span of the string_content inside a string_literal (for selection_span).
    fn string_content_span(&self, node: Node<'a>) -> Span {
        for i in 0..node.named_child_count() {
            if let Some(content) = node.named_child(i) {
                if content.kind() == "string_content" {
                    return node_to_span(content);
                }
            }
        }
        node_to_span(node)
    }

    /// Extract attribute names from an array ref expression: [qw(foo bar)] or ['foo', 'bar']
    fn extract_array_attr_names(&self, node: Node<'a>, names: &mut Vec<(String, Span)>) {
        // Handle bare qw() node directly (e.g. as method arg)
        if node.kind() == "quoted_word_list" {
            self.extract_qw_word_spans(node, names);
            return;
        }
        names.extend(self.extract_string_list(node));
    }

    /// Extract key-value pairs from a fat-comma list expression.
    /// Returns pairs like [("is", "ro"), ("isa", "Str")].
    fn extract_fat_comma_pairs(&self, node: Node<'a>) -> Vec<(String, String)> {
        let mut pairs = Vec::new();
        let mut i = 0;
        let count = node.child_count();
        while i < count {
            let key_node = match node.child(i) {
                Some(c) if c.is_named() => c,
                _ => { i += 1; continue; }
            };

            // Key: bareword/autoquoted_bareword or string
            let key = match key_node.kind() {
                "bareword" | "autoquoted_bareword" => key_node.utf8_text(self.source).ok().map(|s| s.to_string()),
                "string_literal" | "interpolated_string_literal" => self.extract_string_content(key_node),
                _ => { i += 1; continue; }
            };

            let key = match key {
                Some(k) => k,
                None => { i += 1; continue; }
            };

            // Skip '=>'
            i += 1;
            while i < count {
                match node.child(i) {
                    Some(c) if c.kind() == "=>" => { i += 1; break; }
                    Some(c) if !c.is_named() => { i += 1; }
                    _ => break,
                }
            }

            // Value: string, bareword, or skip complex values
            if i < count {
                if let Some(val_node) = node.child(i) {
                    if val_node.is_named() {
                        let val = match val_node.kind() {
                            "bareword" | "autoquoted_bareword" => val_node.utf8_text(self.source).ok().map(|s| s.to_string()),
                            "string_literal" | "interpolated_string_literal" => self.extract_string_content(val_node),
                            _ => None,
                        };
                        if let Some(v) = val {
                            pairs.push((key, v));
                        }
                        i += 1;
                        continue;
                    }
                }
            }
            i += 1;
        }
        pairs
    }


    /// Map a Moo/Moose `isa` type constraint string to an InferredType.
    fn map_isa_to_type(&self, isa: &str, mode: FrameworkMode) -> Option<InferredType> {
        match isa {
            "Str" => Some(InferredType::String),
            "Int" | "Num" => Some(InferredType::Numeric),
            "Bool" => Some(InferredType::Numeric),
            "HashRef" => Some(InferredType::HashRef),
            "ArrayRef" => Some(InferredType::ArrayRef),
            "CodeRef" => Some(InferredType::CodeRef { return_edge: None }),
            "RegexpRef" => Some(InferredType::Regexp),
            _ => {
                // InstanceOf['Foo::Bar'] (Moo style) — the isa value is
                // valid-ish Perl syntax, so re-parse it with tree-sitter
                // and pull the class name out of the tree rather than
                // hand-stripping brackets and quotes.
                if let Some(class) = parse_instance_of(isa) {
                    return Some(InferredType::ClassName(class));
                }
                // Moose allows class names as types (contains :: or starts uppercase)
                if mode == FrameworkMode::Moose && (isa.contains("::") || isa.starts_with(|c: char| c.is_uppercase())) {
                    // Avoid matching Moose type names like "Str", "Int" etc. already handled above
                    if isa.contains("::") || isa.len() > 3 {
                        return Some(InferredType::ClassName(isa.to_string()));
                    }
                }
                None
            }
        }
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
        let method_node = node.child_by_field_name("method");
        let method_name = method_node
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| s.to_string());
        let method_name_span = method_node.map(|n| node_to_span(n))
            .unwrap_or_else(|| node_to_span(node));
        let invocant_node = node.child_by_field_name("invocant");
        let invocant_text = invocant_node
            .and_then(|n| n.utf8_text(self.source).ok())
            .map(|s| s.to_string());
        let invocant = invocant_text.as_ref().map(|s| {
            // Resolve __PACKAGE__ to enclosing package name
            if s == "__PACKAGE__" {
                self.current_package.clone().unwrap_or_else(|| s.to_string())
            } else {
                s.to_string()
            }
        });
        // Always store the invocant span so post-walk refinement
        // (`apply_chain_typing_invocants`) can find the node and
        // fill `invocant_class` for refs that walk-time couldn't
        // resolve (variable invocants whose TC isn't yet seeded,
        // call-chain invocants whose inner sub return type isn't
        // resolved). Without this, a `$obj->method` ref whose
        // walk-time invocant_class was None would stay None
        // forever and class-scoped `refs_to` would silently match
        // too broadly.
        let invocant_span = invocant_node.map(node_to_span);

        // Walk-time invocant_class — closed-under-syntax cases only:
        //   - constructor pattern (`Sner->new->hi`)
        //   - `__PACKAGE__->method(...)`
        //
        // Both are determined by syntax alone — no inference, no
        // bag lookup. Everything else (variable invocants, bareword
        // invocants whose canonical class only the bag knows like
        // `app`, call-chain invocants whose return type is
        // mid-fold) stays None and gets filled by PostFold's
        // `apply_chain_typing_invocants` against the canonical bag.
        // Now that `emit_method_call_arg_keys` runs post-walk, no
        // walk-time consumer reads `invocant_class` — leaving it
        // None at walk-time costs nothing.
        let invocant_class = invocant_node.and_then(|n| match n.kind() {
            "method_call_expression" => self.extract_constructor_class(n),
            "bareword" | "package"
                if n.utf8_text(self.source).ok() == Some("__PACKAGE__") =>
            {
                self.current_package.clone()
            }
            _ => None,
        });

        if let Some(ref name) = method_name {
            // Dynamic method dispatch: $self->$method() — resolve $method if known
            if name.starts_with('$') {
                if let Some(resolved) = self.resolve_constant_strings(name, 0) {
                    for rname in resolved {
                        let idx = self.refs.len();
                        self.add_ref(
                            RefKind::MethodCall {
                                invocant: invocant.clone().unwrap_or_default(),
                                invocant_span,
                                method_name_span,
                            },
                            node_to_span(node),
                            rname,
                            AccessKind::Read,
                        );
                        if let Some(c) = invocant_class.clone() {
                            self.method_call_invocant.insert(idx, c);
                        }
                    }
                }
            } else {
                let idx = self.refs.len();
                self.add_ref(
                    RefKind::MethodCall {
                        invocant: invocant.clone().unwrap_or_default(),
                        invocant_span,
                        method_name_span,
                    },
                    node_to_span(node),
                    name.clone(),
                    AccessKind::Read,
                );
                if let Some(c) = invocant_class.clone() {
                    self.method_call_invocant.insert(idx, c);
                }

                // Part 5c — `recv->resultset('Foo')` is closed
                // under syntax. Push the `Parametric` InferredType
                // at the call's `Expression(refidx)` so chain
                // receivers reading via
                // `method_call_return_type_via_bag` see it, and
                // mark the ref so `emit_method_call_return_edges`
                // skips its standard `Edge(MethodOnClass)` (which
                // would resolve to the receiver class's `resultset`
                // plain return type and mask the row-class arg via
                // `FrameworkAwareTypeFold`'s class-axis short-
                // circuit). Variable-binding chain typing
                // (`my $rs = $schema->resultset(…)`) reads the same
                // attachment via the bag and seeds a TC for `$rs`.
                if let Some(ty) = self.extract_resultset_parametric(node) {
                    self.parametric_emitted_refs.insert(idx);
                    let r_span = self.refs[idx].span;
                    self.bag.push(crate::witnesses::Witness {
                        attachment: crate::witnesses::WitnessAttachment::Expression(
                            crate::witnesses::RefIdx(idx as u32),
                        ),
                        source: crate::witnesses::WitnessSource::Builder(
                            "parametric_resultset".into(),
                        ),
                        payload: crate::witnesses::WitnessPayload::InferredType(ty),
                        span: r_span,
                    });
                }

                // Per-flavor return-type projection. When the
                // receiver types as a Parametric flavor and the
                // flavor declares a `return_projection(method)`
                // (e.g. ResultSet projects through `RowOf<self>`
                // for find / first / single / next / create /
                // find_or_new / find_or_create / update_or_create
                // / new_result), emit the projected operator on
                // this call's `Expression(refidx)`. The bag's
                // `class_name()` evaluation reduces the operator
                // when consumed (`RowOf<ResultSet { row }>` →
                // `ClassName(row)`), so chain typing through
                // `my $row = $rs->find(...)` gives `$row` the
                // row class. No method allowlist in core: the
                // flavor owns its projection table (CLAUDE.md
                // #10).
                //
                // Co-located with the `extract_resultset_parametric`
                // emission so the DBIC plugin port lifts both in
                // one shot.
                if let Some(inv_node) = invocant_node {
                    if let Some(recv_ty) = self.invocant_type_at_node(inv_node) {
                        if let Some(proj) = recv_ty
                            .as_parametric()
                            .and_then(|p| p.return_projection(name))
                        {
                            self.parametric_emitted_refs.insert(idx);
                            let r_span = self.refs[idx].span;
                            self.bag.push(crate::witnesses::Witness {
                                attachment: crate::witnesses::WitnessAttachment::Expression(
                                    crate::witnesses::RefIdx(idx as u32),
                                ),
                                source: crate::witnesses::WitnessSource::Builder(
                                    "parametric_projection".into(),
                                ),
                                payload: crate::witnesses::WitnessPayload::InferredType(
                                    InferredType::Parametric(proj),
                                ),
                                span: r_span,
                            });
                        }
                    }
                }
            }
        }

        // Bareword invocant that's really a function call (`app->routes`
        // where `app` is a plugin-emitted Sub returning Mojolicious).
        // The wider MethodCall ref covers the whole `app->routes` span,
        // so cursor-on-`app` currently lands on a ref that describes
        // `routes` — no semantic token on the bareword, no hover/gd
        // targeting the sub itself. Emitting a narrower FunctionCall
        // ref at the bareword span makes ref_at prefer it (innermost
        // wins), which lets the existing FunctionCall paths (semantic
        // tokens, hover, gd) light up the bareword as the call it is.
        if let Some(inv_node) = invocant_node {
            if inv_node.kind() == "bareword" {
                if let Ok(bw_text) = inv_node.utf8_text(self.source) {
                    // Find the matching sub and capture its package so
                    // the FunctionCall ref's `resolved_package` points
                    // at the real definer — otherwise find_definition
                    // and hover_info's package-scoped match miss it.
                    let matched_pkg = self.symbols.iter().find_map(|s| {
                        if s.name != bw_text { return None; }
                        if !matches!(s.kind, SymKind::Sub | SymKind::Method) { return None; }
                        Some(s.package.clone())
                    });
                    if let Some(pkg) = matched_pkg {
                        self.add_ref(
                            RefKind::FunctionCall { resolved_package: pkg },
                            node_to_span(inv_node),
                            bw_text.to_string(),
                            AccessKind::Read,
                        );
                    }
                }
            }
        }

        // DBIC accessor synthesis: __PACKAGE__->add_columns(...), ->has_many(...), etc.
        let is_pkg_call = invocant_text.as_deref() == Some("__PACKAGE__")
            || (invocant_node.map(|n| n.kind()) == Some("package")
                && invocant_text.as_ref() == self.current_package.as_ref());
        if is_pkg_call {
            if let Some(ref name) = method_name {
                // load_components — register components as parents for method resolution
                // Works for any class (DBIC, Catalyst, etc.) — components are mixins
                if name == "load_components" {
                    self.visit_load_components(node);
                }
                // DBIC accessor synthesis
                if self.is_dbic_class() {
                    self.visit_dbic_class_method(node, name);
                }
            }
        }

        // Even-position stringy args become `HashKeyAccess` refs
        // owned by `Sub{invocant_class, method_name}` — pairs with
        // the HashKeyDef symbols that `has` / `bless { … }`
        // synthesize on the callee side. Emission is deferred to
        // post-walk (`emit_method_call_arg_keys`) so the owner's
        // class can be read off the canonical `invocant_class`
        // (filled by `apply_chain_typing_invocants` against the
        // bag) instead of the partially-resolved walk-time value.
        // The chain-typing index records the args node by call
        // span; the post-walk pass joins refs to args via that
        // span.

        // Framework plugin dispatch. Runs after native handlers so plugin
        // emissions are purely additive — a third-party plugin targeting
        // `->on()` can't accidentally break DBIC `->add_columns()`.
        if !self.plugins.is_empty() {
            if let Some(ref name) = method_name {
                let args_raw = self.extract_call_args(node);
                let mut ctx = self.base_call_context(
                    args_raw,
                    node_to_span(node),
                    method_name_span,
                );
                ctx.call_kind = plugin::CallKind::Method;
                ctx.method_name = Some(name.clone());
                ctx.receiver_text = invocant_text.clone();
                ctx.receiver_type = self.receiver_type_for(invocant_node);
                self.dispatch_method_call_plugins(ctx);
            }
        }

        self.visit_children(node);
    }

    /// Check if the current package inherits from a DBIx::Class package.
    fn is_dbic_class(&self) -> bool {
        if let Some(ref pkg) = self.current_package {
            if let Some(parents) = self.package_parents.get(pkg) {
                return parents.iter().any(|p| p.starts_with("DBIx::Class"));
            }
        }
        false
    }

    /// Synthesize accessor methods from DBIC class method calls.
    fn visit_dbic_class_method(&mut self, node: Node<'a>, method_name: &str) {
        match method_name {
            "add_columns" | "add_column" => {
                self.visit_dbic_add_columns(node);
            }
            "has_many" | "many_to_many" => {
                self.visit_dbic_relationship(node, true);
            }
            "belongs_to" | "has_one" | "might_have" => {
                self.visit_dbic_relationship(node, false);
            }
            _ => {}
        }
    }

    /// Synthesize column accessor methods from __PACKAGE__->add_columns(...).
    fn visit_dbic_add_columns(&mut self, node: Node<'a>) {
        // Arguments from method_call_expression:
        //   qw(id name email)
        //   id => { data_type => 'integer' }, name => { data_type => 'varchar' }
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };

        let mut col_names: Vec<(String, Span)> = Vec::new();

        match args.kind() {
            "quoted_word_list" => {
                self.extract_array_attr_names(args, &mut col_names);
            }
            "parenthesized_expression" | "list_expression" => {
                self.extract_dbic_column_names(args, &mut col_names);
            }
            _ => {}
        }

        for (name, sel_span) in &col_names {
            self.add_symbol(
                name.clone(),
                SymKind::Method,
                node_to_span(node),
                *sel_span,
                SymbolDetail::Sub {
                    params: vec![ParamInfo {
                        name: "$val".into(),
                        default: None,
                        is_slurpy: false,
                    is_invocant: false,
                    }],
                    is_method: true,
                    doc: None,
                    display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                },
            );
        }

        // Also synthesize HashKeyDef symbols owned by the row class
        // so `$rs->search({ name => … })` and `$row->{name}` find
        // their column def. Mirrors what Mojo `has` does for
        // constructor args. The HashKeyAccess consumer at
        // `find_definition`'s method-call-arg arm looks up
        // `HashKeyOwner::Class(<class>)` — when threaded through
        // Parametric ResultSet typing (Part 5c), `<class>` is the
        // row class.
        if let Some(ref pkg) = self.current_package {
            let owner = HashKeyOwner::Class(pkg.clone());
            for (name, sel_span) in &col_names {
                self.add_symbol(
                    name.clone(),
                    SymKind::HashKeyDef,
                    node_to_span(node),
                    *sel_span,
                    SymbolDetail::HashKeyDef {
                        owner: owner.clone(),
                        is_dynamic: false,
                    },
                );
            }
        }
    }

    /// Extract column names from DBIC add_columns argument list.
    /// In `id => { ... }, name => { ... }`, the column names are the fat-comma keys.
    fn extract_dbic_column_names(&self, node: Node<'a>, names: &mut Vec<(String, Span)>) {
        let mut expect_key = true;
        for i in 0..node.child_count() {
            let child = match node.child(i) {
                Some(c) => c,
                None => continue,
            };

            if !child.is_named() {
                if child.kind() == "=>" {
                    expect_key = false; // next named node is a value
                }
                continue;
            }

            if expect_key {
                match child.kind() {
                    "bareword" | "autoquoted_bareword" => {
                        if let Ok(text) = child.utf8_text(self.source) {
                            names.push((text.to_string(), node_to_span(child)));
                        }
                        expect_key = true; // will flip on =>
                    }
                    "string_literal" | "interpolated_string_literal" => {
                        if let Some(text) = self.extract_string_content(child) {
                            names.push((text, self.string_content_span(child)));
                        }
                        expect_key = true;
                    }
                    "quoted_word_list" => {
                        // qw(id name email) — simple form
                        self.extract_array_attr_names(child, names);
                        return;
                    }
                    _ => {}
                }
            } else {
                // Skip the value (hash_ref, string, etc.)
                expect_key = true;
            }
        }
    }

    /// Synthesize relationship accessor from __PACKAGE__->has_many/belongs_to/etc.
    fn visit_dbic_relationship(&mut self, node: Node<'a>, is_resultset: bool) {
        // First arg: accessor name, second arg: related class
        let mut accessor_name: Option<(String, Span)> = None;
        let mut related_class: Option<String> = None;

        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return,
        };

        match args.kind() {
            "parenthesized_expression" | "list_expression" => {
                self.extract_relationship_args(args, &mut accessor_name, &mut related_class);
            }
            _ => {}
        }

        if let Some((name, sel_span)) = accessor_name {
            let return_type = if is_resultset {
                Some(InferredType::ClassName("DBIx::Class::ResultSet".to_string()))
            } else {
                related_class.map(|c| InferredType::ClassName(c))
            };

            let sym_id = self.add_symbol(
                name.clone(),
                SymKind::Method,
                node_to_span(node),
                sel_span,
                SymbolDetail::Sub {
                    params: vec![],
                    is_method: true,
                    doc: None,
                    display: None,
                    hide_in_outline: false,
                    opaque_return: false,
                },
            );
            self.record_framework_accessor_witness(
                sym_id,
                &name,
                Some(0),
                return_type,
                "DBIx::Class",
                if is_resultset {
                    format!("DBIx::Class resultset relationship `{}`", name)
                } else {
                    format!("DBIx::Class row relationship `{}`", name)
                },
            );
        }
    }

    /// Extract accessor name and related class from relationship call arguments.
    fn extract_relationship_args(&self, node: Node<'a>, accessor: &mut Option<(String, Span)>, related: &mut Option<String>) {
        let mut arg_idx = 0;
        for i in 0..node.child_count() {
            let child = match node.child(i) {
                Some(c) if c.is_named() => c,
                _ => continue,
            };
            match child.kind() {
                "string_literal" | "interpolated_string_literal" => {
                    if let Some(text) = self.extract_string_content(child) {
                        if arg_idx == 0 {
                            *accessor = Some((text, self.string_content_span(child)));
                        } else if arg_idx == 1 {
                            *related = Some(text);
                        }
                        arg_idx += 1;
                    }
                }
                "bareword" | "autoquoted_bareword" => {
                    if let Ok(text) = child.utf8_text(self.source) {
                        if arg_idx == 0 {
                            *accessor = Some((text.to_string(), node_to_span(child)));
                        }
                        arg_idx += 1;
                    }
                }
                _ => { arg_idx += 1; }
            }
        }
    }

    fn visit_hash_element(&mut self, node: Node<'a>) {
        // Infer HashRef on the operand variable (e.g. $x in $x->{key})
        self.infer_deref_type(node, InferredType::HashRef);

        // Record the hash variable access
        let var_text = self.get_hash_var_from_element(node);

        // Distinguish read vs write by asking determine_access on the
        // element node itself — `$self->{k} = ...` has this element as
        // the LHS of an assignment, so the grandparent check returns
        // Write. Needed for Part 1 (invocant mutations).
        let element_access = self.determine_access(node);

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
                        element_access,
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
                if let Some(name) = self.build_var_name(node) {
                    out.push((name, node_to_span(node)));
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

    /// Build a variable's canonical name by reading the tree: sigil
    /// comes from the node kind (`scalar` → `$`, `array` → `@`, `hash`
    /// → `%`), bare name comes from the `varname` child. This keeps us
    /// correct for edge cases where the full node text isn't just
    /// `sigil + identifier` — e.g. `${foo}` (text `${foo}`, varname
    /// `foo`), `$:field` (whatever TSP aliases into varname), or any
    /// future TSP-added sigil-bearing syntax. The previous
    /// `node.utf8_text()` + caller-side sigil-stripping broke on every
    /// one of those shapes (`{foo}`, `:field`, etc.).
    ///
    /// Falls back to the full node text when the varname child is
    /// missing (ERROR recovery, partial parses).
    fn build_var_name(&self, node: Node<'a>) -> Option<String> {
        let sigil = match node.kind() {
            "scalar" => '$',
            "array" => '@',
            "hash" => '%',
            _ => return node.utf8_text(self.source).ok().map(|s| s.to_string()),
        };
        let varname = find_varname_child(node).and_then(|v| v.utf8_text(self.source).ok());
        match varname {
            Some(name) => Some(format!("{}{}", sigil, name)),
            None => node.utf8_text(self.source).ok().map(|s| s.to_string()),
        }
    }

    fn first_var_child(&self, node: Node<'a>) -> Option<String> {
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                if matches!(child.kind(), "scalar" | "array" | "hash") {
                    return self.build_var_name(child);
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

    /// Map a container/slice/keyval access node to the name of the
    /// variable it actually reads. The sigil on the access site is
    /// NOT the declared sigil:
    ///
    ///   $foo[0]         → @foo   (array element, under array_element_expression)
    ///   $foo{hi}        → %foo   (hash element, under hash_element_expression)
    ///   @foo[0..1]      → @foo   (array slice — parent `slice_expression` field `array:`)
    ///   @foo{qw/.../}   → %foo   (hash slice — parent `slice_expression` field `hash:`)
    ///   %foo[0..1]      → @foo   (KV slice of array — `keyval_expression` field `array:`)
    ///   %foo{a}         → %foo   (KV slice of hash — `keyval_expression` field `hash:`)
    ///
    /// For slice/keyval we ask the parent which *field* this node is
    /// filling, because the sigil on the child is always `@` (slice)
    /// or `%` (keyval) regardless of the underlying container. Bare
    /// name comes from the `varname` child so forms like `@{$ref}[0]`
    /// (ERROR/block-varname) don't produce garbage.
    fn canonicalize_container(&self, node: Node<'a>, text: &str) -> String {
        let fallback = || text.to_string();
        let parent = match node.parent() { Some(p) => p, None => return fallback() };
        let bare = match find_varname_child(node).and_then(|v| v.utf8_text(self.source).ok()) {
            Some(b) => b,
            None => return fallback(),
        };

        let target_sigil: char = match parent.kind() {
            "array_element_expression" => '@',
            "hash_element_expression" => '%',
            "slice_expression" | "keyval_expression" => {
                if parent.child_by_field_name("array").map_or(false, |c| c == node) {
                    '@'
                } else if parent.child_by_field_name("hash").map_or(false, |c| c == node) {
                    '%'
                } else {
                    return fallback();
                }
            }
            _ => return fallback(),
        };
        format!("{}{}", target_sigil, bare)
    }

    /// Extract the function name from a call expression (function_call or ambiguous_function_call).
    fn extract_call_name(&self, node: Node<'a>) -> Option<String> {
        // Only match actual function calls, not method calls
        // (method calls are handled by MethodCallBinding).
        //
        // We used to reject names containing `:` (qualified calls like
        // `Pkg::Sub::foo()`), which silently dropped those from
        // `call_bindings`. The fixup downstream strips the package prefix
        // before looking the sub up in `return_types`, so qualifiers are
        // fine to pass through here.
        //
        // Dynamic calls like `my $fn = 'get_config'; $fn->()` — the parser
        // yields a function_call_expression with function="$fn". Mirror the
        // method-call path: try constant folding to recover the concrete
        // sub name; fall through to None when the variable isn't a known
        // compile-time constant.
        match node.kind() {
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let name = crate::file_analysis::extract_call_name(node, self.source)?;
                if let Some(stripped) = name.strip_prefix('&') {
                    // `&$fn()` syntax — same deal.
                    if stripped.starts_with('$') {
                        return self.resolve_constant_strings(stripped, 0)
                            .and_then(|names| names.into_iter().next());
                    }
                    return Some(stripped.to_string());
                }
                if name.starts_with('$') {
                    return self.resolve_constant_strings(&name, 0)
                        .and_then(|names| names.into_iter().next());
                }
                Some(name)
            }
            _ => None,
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

    /// `recv->resultset('Foo')` → `Parametric(ResultSet { base,
    /// row })`. `base` is discovered via the
    /// `<NS>::Result::<X>` ↔ `<NS>::ResultSet::<X>` convention if
    /// that class exists in this file; otherwise falls back to
    /// `DBIx::Class::ResultSet` (the universal DBIC default that
    /// runtime DBIC creates dynamically when no custom resultset
    /// class is defined). The fallback hardcode is core-resident
    /// for now — the DBIC plugin (queued, see
    /// `docs/prompt-dbic-as-plugin.md`) will own it once the port
    /// lands.
    ///
    /// First arg must be a string literal — a non-literal
    /// (variable, computed) means the row class is dynamic and
    /// we don't claim.
    fn extract_resultset_parametric(&self, node: Node<'a>) -> Option<InferredType> {
        if node.kind() != "method_call_expression" {
            return None;
        }
        let method = node.child_by_field_name("method")?;
        if method.utf8_text(self.source).ok() != Some("resultset") {
            return None;
        }
        let args = node.child_by_field_name("arguments")?;
        let row_class = self.first_string_or_constfold_arg(args)?;
        let base = self
            .discover_resultset_class(&row_class)
            .unwrap_or_else(|| "DBIx::Class::ResultSet".to_string());
        Some(InferredType::Parametric(crate::file_analysis::ParametricType::ResultSet {
            base,
            row: row_class,
        }))
    }

    /// Like `first_string_literal_arg` but additionally const-folds
    /// a scalar arg (`$sner` where `my $sner = 'Foo'` exists in
    /// scope). Used by `extract_resultset_parametric` so
    /// `$schema->resultset($sner)` types the same as
    /// `$schema->resultset('Foo')` when `$sner` is a known
    /// compile-time constant. Multi-value const-folding (a scalar
    /// that resolves to multiple strings) bails — Parametric's
    /// `row` is a single class, not a sum type yet.
    fn first_string_or_constfold_arg(&self, args: Node<'a>) -> Option<String> {
        if let Some(s) = self.first_string_literal_arg(args) {
            return Some(s);
        }
        // Try the first arg as a `scalar` node with const-foldable
        // value. Same one-level unwrap rule as the literal helper.
        let arg_node = match args.kind() {
            "scalar" => Some(args),
            "parenthesized_expression" | "list_expression" => {
                let mut found: Option<Node<'a>> = None;
                for i in 0..args.named_child_count() {
                    if let Some(c) = args.named_child(i) {
                        found = Some(c);
                        break;
                    }
                }
                found
            }
            _ => None,
        }?;
        if arg_node.kind() != "scalar" {
            return None;
        }
        let var_text = arg_node.utf8_text(self.source).ok()?;
        let folded = self.resolve_constant_strings(var_text, 0)?;
        // Single-value fold only — multi-value (loop variable that
        // takes several strings) doesn't have a single row class to
        // emit. Future Part 5 sum-types work could lift this.
        if folded.len() == 1 {
            folded.into_iter().next()
        } else {
            None
        }
    }

    /// Discover the resultset class for a given row class via the
    /// `<NS>::Result::<X>` ↔ `<NS>::ResultSet::<X>` convention.
    /// Returns `Some(class)` only when the discovered class is
    /// declared in the file's symbols (`SymKind::Package` or
    /// `SymKind::Class`). Cross-file discovery (looking up
    /// `<NS>::ResultSet::<X>` in `module_index`) is queued with
    /// the DBIC plugin port — most projects keep the row class
    /// and resultset class in sibling files of the same dist,
    /// so this in-file discovery covers the common case until
    /// the plugin lands.
    fn discover_resultset_class(&self, row_class: &str) -> Option<String> {
        if !row_class.contains("::Result::") {
            return None;
        }
        let candidate = row_class.replacen("::Result::", "::ResultSet::", 1);
        let exists = self.symbols.iter().any(|s| {
            s.name == candidate
                && matches!(s.kind, SymKind::Package | SymKind::Class)
        });
        if exists { Some(candidate) } else { None }
    }

    /// First named child of a call-arg node that's a string literal,
    /// returning its content. Returns None for non-literal first
    /// args (variables, computed expressions). Shared between the
    /// resultset emission and any future parametric-by-literal
    /// emission rule.
    fn first_string_literal_arg(&self, args: Node<'a>) -> Option<String> {
        // Single-arg method calls land here with `args` itself a
        // `string_literal` node (no surrounding paren / list
        // expression). Multi-arg calls wrap in
        // `list_expression`/`parenthesized_expression`. Handle the
        // bare-string case first, then recurse into wrappers.
        match args.kind() {
            "string_literal" | "interpolated_string_literal" => {
                return self.extract_string_content(args);
            }
            "parenthesized_expression" | "list_expression" => {
                for i in 0..args.named_child_count() {
                    let child = args.named_child(i)?;
                    return match child.kind() {
                        "string_literal" | "interpolated_string_literal" => {
                            self.extract_string_content(child)
                        }
                        "parenthesized_expression" | "list_expression" => {
                            self.first_string_literal_arg(child)
                        }
                        _ => None,
                    };
                }
            }
            _ => {}
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
                let pkg = self.current_package.clone()
                    .or_else(|| self.scopes[self.current_scope().0 as usize].package.clone());
                if let Some(pkg) = pkg {
                    // Inside a sub: register to `Sub{C, sub_name}` —
                    // that's the actual constructor (or whatever sub
                    // does the blessing). `has`-emitted HashKeyDefs
                    // use the same shape, so a single owner encoding
                    // covers both registration paths and call-site
                    // HashKeyAccess refs (Sub{C, method}) match
                    // strict. Top-level blesses (rare) keep the
                    // coarse `Class(C)` form.
                    return Some(match self.enclosing_sub_name() {
                        Some(name) => HashKeyOwner::Sub { package: Some(pkg), name },
                        None => HashKeyOwner::Class(pkg),
                    });
                }
            }
            // Check if this is inside a return expression of a sub
            if ancestor.kind() == "return_expression" {
                if let Some(name) = self.enclosing_sub_name() {
                    return Some(HashKeyOwner::Sub {
                        package: self.current_package.clone(),
                        name,
                    });
                }
            }
            // Check if this is the last expression in a sub body (implicit return)
            if ancestor.kind() == "expression_statement" {
                if self.is_last_statement_in_sub(ancestor) {
                    if let Some(name) = self.enclosing_sub_name() {
                        return Some(HashKeyOwner::Sub {
                            package: self.current_package.clone(),
                            name,
                        });
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

    /// Emit a `HashKeyAccess` ref at every odd-indexed (1st, 3rd, …)
    /// stringy arg inside a call's args node, owned by `owner`. In
    /// Perl, `foo(a => 1, "b", 2, c => 3)` is `foo("a", 1, "b", 2,
    /// "c", 3)` — `=>` is just an autoquoting comma. The keys are
    /// the even-position named args, regardless of which separator
    /// comes after them. Mirrors `collect_fat_comma_keys` (callee
    /// side, emits HashKeyDef symbols); this is the caller side, so
    /// `ref_at` on the key token picks the narrow span over the
    /// broad MethodCall/FunctionCall ref. Without this, cursor on
    /// the key in `MooApp->new(name => 'alice')` lands on the
    /// method ref and rename clobbers the wrong token.
    ///
    /// Gated on a matching HashKeyDef already being registered for
    /// `owner`. Otherwise we'd shadow the broader MethodCall ref
    /// for cases the caller-side has no def to anchor on (`class
    /// Foo { field $x :param }` — Point->new(x => 3, …) needs the
    /// MethodCall ref's `find_param_field` fallback in
    /// `find_definition`).
    fn emit_call_arg_key_accesses(&mut self, args_node: Node<'a>, gate: Gate) {
        // Unwrap one level into a hash literal / paren wrapper —
        // search's `{KEY=>...}` is `anonymous_hash_expression`
        // wrapping a `list_expression` of pairs; constructors are
        // paren-wrapped. Even-position iteration expects a flat
        // pair list. Constructor-style args without the wrapper
        // pass through unchanged.
        let mut effective = args_node;
        if matches!(effective.kind(), "anonymous_hash_expression" | "parenthesized_expression")
            && effective.named_child_count() == 1
        {
            if let Some(inner) = effective.named_child(0) {
                if matches!(inner.kind(), "list_expression" | "anonymous_hash_expression") {
                    effective = inner;
                }
            }
        }
        let named: Vec<Node<'a>> = (0..effective.named_child_count())
            .filter_map(|i| effective.named_child(i))
            .collect();
        // Single-arg calls present the arg directly (no list_expression
        // wrapper). One arg can't be a key/value pair.
        if named.len() < 2
            && effective.kind() != "list_expression"
            && effective.kind() != "parenthesized_expression"
        {
            return;
        }
        for (idx, child) in named.iter().enumerate() {
            if idx % 2 != 0 { continue; }
            if !matches!(
                child.kind(),
                "bareword" | "autoquoted_bareword" | "string_literal" | "interpolated_string_literal"
            ) {
                continue;
            }
            let Some((key, is_dynamic)) = self.extract_key_text(*child) else { continue };
            if is_dynamic { continue; }
            // Gate decides whether to emit + what owner to record.
            // Strict checks the local symbol table for a matching
            // HashKeyDef (prevents `Foo::bar(name=>1)` from latching
            // onto unrelated `Sub{Foo,new}` keys). Open trusts the
            // caller (the receiver's flavor pinned the owner).
            // Deferred emits with `owner: None`; the post-walk
            // fixup in `fix_chain_receiver_hash_key_owners` fills
            // the owner once the receiver type resolves
            // (in-file or cross-file).
            let owner_to_emit: Option<HashKeyOwner> = match &gate {
                Gate::Strict(owner) => {
                    if !self.has_hash_key_def(&key, owner) { continue; }
                    Some(owner.clone())
                }
                Gate::Open(owner) => Some(owner.clone()),
                Gate::Deferred => None,
            };
            let access = self.determine_access(*child);
            // `scope_at_point` instead of `current_scope` so this
            // works both inside the walk (function-call args path)
            // and post-walk (method-call args path; scope_stack is
            // empty by then). Equivalent at walk-time because a
            // node's innermost containing scope IS what
            // current_scope would return.
            let span = node_to_span(*child);
            self.refs.push(Ref {
                kind: RefKind::HashKeyAccess {
                    var_text: String::new(),
                    owner: owner_to_emit,
                },
                span,
                scope: self.scope_at_point(span.start),
                target_name: key,
                access,
                resolves_to: None,
            });
        }
    }

    /// Strict (no `found_by` broadening) check: is a HashKeyDef
    /// registered with this exact `owner` and `name`? Used by
    /// `emit_call_arg_key_accesses` to gate emission — broadening
    /// would let `Foo::bar(name => 1)` latch onto `name` keys
    /// registered to `Sub{Foo, new}`, which they don't logically
    /// belong to.
    fn has_hash_key_def(&self, name: &str, owner: &HashKeyOwner) -> bool {
        self.symbols.iter().any(|s| {
            if s.name != name { return false; }
            matches!(&s.detail, SymbolDetail::HashKeyDef { owner: o, .. } if o == owner)
        })
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
                    // extract_head2_section/extract_item_section return rendered markdown
                    if let Some(md) = crate::pod::extract_head2_section(sub_name, text) {
                        if !md.is_empty() {
                            return Some(md);
                        }
                    }
                    if let Some(md) = crate::pod::extract_item_section(sub_name, text) {
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
                    if let Some(md) = crate::pod::extract_head2_section(&sym.name, pod_text) {
                        if !md.is_empty() {
                            *doc = Some(md);
                            break;
                        }
                    }
                    if let Some(md) = crate::pod::extract_item_section(&sym.name, pod_text) {
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
        // Build a temporary scope-to-symbols map for efficient lookup.
        //
        // `gating_package` is `Some(pkg)` for `our` decls — they're
        // package-globals with a lexical alias, so a use site only
        // resolves to them when the use's enclosing package matches
        // (`$Calculator::version` is reachable as bare `$version`
        // only inside `package Calculator;`). It's `None` for `my` /
        // `state` / `field`, which are pure-lexical: they resolve
        // wherever the lexical scope chain reaches them, regardless
        // of which `package X;` section the use sits under.
        let mut scope_symbols: std::collections::HashMap<
            ScopeId,
            Vec<(String, SymbolId, Point, Option<String>)>,
        > = std::collections::HashMap::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
                let gating_package = match &sym.detail {
                    SymbolDetail::Variable { decl_kind: DeclKind::Our, .. } => sym.package.clone(),
                    _ => None,
                };
                scope_symbols
                    .entry(sym.scope)
                    .or_default()
                    .push((sym.name.clone(), sym.id, sym.span.start, gating_package));
            }
        }

        for idx in 0..self.refs.len() {
            if !matches!(self.refs[idx].kind, RefKind::Variable | RefKind::ContainerAccess) {
                continue;
            }
            let ref_span_start = self.refs[idx].span.start;
            let ref_target = self.refs[idx].target_name.clone();
            let ref_scope = self.refs[idx].scope;

            let use_pkg = self.package_at_pos(ref_span_start).map(|s| s.to_string());

            // Walk scope chain to find the innermost matching declaration
            let mut current = Some(ref_scope);
            while let Some(scope_id) = current {
                if let Some(symbols) = scope_symbols.get(&scope_id) {
                    // Find the best match: declared before this ref, matching name,
                    // and (for `our`) sharing the use's enclosing package.
                    if let Some((_, sym_id, _, _)) = symbols.iter()
                        .filter(|(name, _, decl_point, gating_package)| {
                            if name != &ref_target { return false; }
                            if *decl_point > ref_span_start { return false; }
                            match gating_package {
                                Some(decl_pkg) => use_pkg.as_deref() == Some(decl_pkg.as_str()),
                                None => true,
                            }
                        })
                        .last()
                    {
                        self.refs[idx].resolves_to = Some(*sym_id);
                        break;
                    }
                }
                current = self.scopes[scope_id.0 as usize].parent;
            }
        }
    }

    /// Phase 5 of the worklist refactor: the three post-walk CST walks
    /// (assignment typing, return-arm refresh, invocant-class refresh)
    /// collapse into one **ChainTypingReducer**. A single CST walk
    /// builds a `ChainTypingIndex` (assignment, return-expression, and
    /// invocant nodes by span); the reducer drains the index in two
    /// modes — `PreFold` between the two `resolve_return_types` calls
    /// (assignments + return arms feed fold-2), `PostFold` after the
    /// second fold (invocants are query-time outputs and need every
    /// sub return type resolved).
    ///
    /// The recursive typer (`resolve_invocant_class_tree` for
    /// assignments + invocants, `infer_return_value_type` for return
    /// arms) is unchanged — only the scheduling collapses. Step 6's
    /// first invocation feeds chain types via the same path; step 9
    /// (still the second hardcoded fold call) re-runs and picks up any
    /// chain types that needed the first fold to land. Phase 6 will
    /// replace the explicit pre/post split with a worklist driver.
    ///
    /// Idempotent across both calls — assignments skip if a TC already
    /// exists, return arms only upgrade `None → Some`, invocants skip
    /// if `invocant_class` is already pinned. Running the reducer twice
    /// in `PostFold` mode would type strictly the same set as one call.
    fn run_chain_typing_reducer(
        &mut self,
        idx: &ChainTypingIndex<'a>,
        mode: ChainPassMode,
    ) {
        match mode {
            ChainPassMode::PreFold => {
                self.apply_chain_typing_assignments(idx);
                // Refresh `invocant_class` each iteration too.
                // Variable invocants whose TC just landed in the
                // worklist's previous iteration become resolvable
                // here — earlier this only ran in PostFold, which
                // meant the bag's `method_call_return` edges
                // (re-emitted from filled invocant_class) couldn't
                // see them until the loop already terminated.
                // `apply_chain_typing_invocants` is idempotent (skips
                // refs whose class is already pinned).
                self.apply_chain_typing_invocants(idx);
            }
            ChainPassMode::PostFold => {
                self.apply_chain_typing_invocants(idx);
            }
        }
    }

    /// Phase 6: replace the manually-ordered `fold → chain → fold`
    /// sequence with a fixed-point loop. Each iteration runs
    /// `ChainPassMode::PreFold` (assignment typing + return-arm refresh)
    /// followed by `resolve_return_types` (the reducer-dispatch driver
    /// from Phase 4); when the snapshot of Sub/Method return types and
    /// the bag length stops moving, the lattice has settled and the
    /// loop exits.
    ///
    /// The two re-emittable passes inside `resolve_return_types`
    /// (arity-return witnesses, call-binding propagator) are
    /// **clear-and-emit**: they drop their prior outputs at the start
    /// of every call so the bag stays canonical regardless of how many
    /// iterations the loop runs. Chain typing's bag-existence check
    /// keeps it idempotent on the same assignment span. The result:
    /// each fact lands in the bag exactly once at the end of the loop,
    /// no matter how deep the chain is.
    ///
    /// `MAX_FOLD_ITERATIONS` is the all-builds safety net: the lattice
    /// argument (witnesses are monotonically appended; reduced answers
    /// refine within a finite enum) guarantees termination, but a
    /// dependency-tracking bug could let the snapshot oscillate. The
    /// `debug_assert!` fires in dev so the regression is caught
    /// immediately; in release we log and break to keep the LSP
    /// responsive instead of spinning forever.
    ///
    /// `ChainPassMode::PostFold` (invocant-class refresh on
    /// `MethodCall` refs) runs once after the loop terminates, since
    /// invocant typing is a query-time write that doesn't feed back
    /// into the bag — a single pass against the now-final symbol
    /// table is sufficient.
    fn fold_to_fixed_point(&mut self, idx: &ChainTypingIndex<'a>) {
        const MAX_FOLD_ITERATIONS: usize = 64;
        let mut iters = 0usize;
        let mut prev = self.fold_state_snapshot();
        loop {
            iters += 1;
            debug_assert!(
                iters < MAX_FOLD_ITERATIONS,
                "type-inference fold did not converge in {iters} iterations — \
                 lattice argument or dependency tracking is broken"
            );
            if iters >= MAX_FOLD_ITERATIONS {
                eprintln!(
                    "perl-lsp: type-inference fold exceeded {MAX_FOLD_ITERATIONS} iterations; \
                     bailing out to keep the LSP responsive"
                );
                break;
            }
            self.run_chain_typing_reducer(idx, ChainPassMode::PreFold);
            self.resolve_return_types();
            let cur = self.fold_state_snapshot();
            if cur == prev {
                break;
            }
            prev = cur;
        }
        self.run_chain_typing_reducer(idx, ChainPassMode::PostFold);
    }

    /// Snapshot of the answers the worklist driver tracks for fixed
    /// point detection: every Sub/Method's return type + the bag
    /// length. Two consecutive iterations producing the same snapshot
    /// means no fold pass changed any sub's answer AND chain typing
    /// pushed no new witnesses — the lattice has settled.
    ///
    /// `bag.len()` captures chain typing's monotonic grow. The
    /// re-emittable passes inside `resolve_return_types` (arity,
    /// call-binding) are clear-and-emit, so their bag contribution is
    /// stable across iterations and a flat total bag length means no
    /// new chain-assignment pushes either.
    fn fold_state_snapshot(&self) -> (Vec<(SymbolId, Option<InferredType>)>, usize, usize) {
        let mut answers: Vec<(SymbolId, Option<InferredType>)> = self
            .symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| (s.id, self.resolved_returns.get(&s.id).cloned()))
            .collect();
        answers.sort_by_key(|(id, _)| id.0);
        // Count entries in the build-time invocant cache so
        // progressive chain-typing inside the loop registers as
        // movement — without this, an iteration that only newly
        // fills the invocant cache (driving the next iteration's
        // `emit_method_call_return_edges` to publish a new edge)
        // would produce the same answers/bag snapshot and the loop
        // would terminate prematurely.
        let invocant_filled = self.method_call_invocant.len();
        (answers, self.bag.len(), invocant_filled)
    }

    /// Symbolically execute the rhs of every `my $X = <expr>` and
    /// push the resulting class type into the bag. ONE recursive typer
    /// (`resolve_invocant_class_tree`) handles every expression shape
    /// it knows — scalar lookup, method-call chain, bareword, shift
    /// idiom, function call. No "is it a chain" branch. Whatever the
    /// rhs is, the typer descends.
    ///
    /// Idempotent: skips an assignment if a Variable witness for `$X`
    /// already exists at the assignment's start point. Walk-time
    /// inference covers literals/constructors via the existing path;
    /// this pass fills in anything that was unresolvable at walk time
    /// (specifically chains whose links' return types only became
    /// known after the first `resolve_return_types`).
    ///
    /// Provenance: each pushed witness carries
    /// `WitnessSource::Builder("type_constraint")` (via
    /// `push_type_constraint`) so a future debug dump can answer "why
    /// does $X have this type?" without re-running the typer.
    ///
    /// Reads from the shared `ChainTypingIndex.assignment_nodes` (built
    /// by `build_chain_typing_index`); does not walk the tree itself.
    ///
    /// The recursive typer (`resolve_invocant_class_tree`) uses
    /// `self.current_package` for the `$self` / `shift` / `$_[0]`
    /// fallback. After the live walk, `current_package` is stale
    /// (= last package opened in the file). To make the typer
    /// package-correct at every assignment site, we query
    /// `package_ranges` for the package at the assignment's
    /// position and override `current_package` for the call.
    ///
    /// Why `package_ranges` and not "track package_statement
    /// nodes": `package X;` is a SIBLING of the subs that follow
    /// it in the AST, not a parent — its scope extends forward
    /// through siblings until the next `package`. Walking the
    /// AST and saving/restoring on package_statement entry/exit
    /// doesn't model that. `package_ranges` is the flat record
    /// populated at walk time and trimmed by successor decls;
    /// a point-query gives the right answer at any byte.
    fn apply_chain_typing_assignments(&mut self, idx: &ChainTypingIndex<'a>) {
        let mut to_push: Vec<(String, ScopeId, Span, InferredType)> = Vec::new();
        for &node in &idx.assignment_nodes {
            let (Some(left), Some(right)) = (
                node.child_by_field_name("left"),
                node.child_by_field_name("right"),
            ) else { continue };
            let Some(var) = self.get_var_text_from_lhs(left) else { continue };
            let span = node_to_span(node);
            // Idempotency: skip if a Variable witness for this var was
            // already pushed at the assignment's start point.
            let already_typed = self.bag.all().iter().any(|w| {
                matches!(&w.attachment, crate::witnesses::WitnessAttachment::Variable { name, .. } if name == &var)
                    && matches!(w.payload, crate::witnesses::WitnessPayload::InferredType(_))
                    && w.span.start == span.start
            });
            if already_typed {
                continue;
            }
            // Innermost scope containing this assignment.
            let scope_idx = self
                .scopes
                .iter()
                .enumerate()
                .filter(|(_, s)| crate::file_analysis::contains_point(&s.span, span.start))
                .min_by_key(|(_, s)| {
                    let r = (s.span.end.row.saturating_sub(s.span.start.row)) as u64;
                    let c = if s.span.start.row == s.span.end.row {
                        s.span.end.column.saturating_sub(s.span.start.column) as u64
                    } else {
                        0
                    };
                    r * 1_000_000 + c
                })
                .map(|(i, _)| i);

            let scope_pkg = self.package_at_pos(span.start).map(|s| s.to_string());

            let saved_pkg = self.current_package.clone();
            if scope_pkg.is_some() {
                self.current_package = scope_pkg;
            }
            // Read the RHS's full `InferredType` first — Parametric
            // shapes need to land on the variable as Parametric, not
            // unwrapped to their `base` via `class_name()`. Falls
            // back to the class-only `resolve_invocant_class_tree`
            // for the bareword-degrade tail (a non-class type on a
            // bareword maps to "treat the syntactic text as a
            // class") which the type-aware path doesn't model.
            let ty_opt = self
                .invocant_type_at_node(right)
                .or_else(|| self.resolve_invocant_class_tree(right).map(InferredType::ClassName));
            self.current_package = saved_pkg;

            if let Some(ty) = ty_opt {
                let sid = scope_idx.map(|i| self.scopes[i].id).unwrap_or(ScopeId(0));
                to_push.push((var, sid, span, ty));
            }
        }

        for (variable, scope, constraint_span, ty) in to_push {
            self.push_type_constraint(TypeConstraint {
                variable,
                scope,
                constraint_span,
                inferred_type: ty,
            });
        }
    }

    /// Re-resolve `invocant_class` on every MethodCall ref using the
    /// tree + the now-final symbol table (return types have been
    /// filled in by the second `resolve_return_types`). This catches
    /// function-call chains like `get_foo()->bar()` where the
    /// invocant's class can only be pinned after `get_foo`'s
    /// return_type is known.
    ///
    /// Reads from the shared `ChainTypingIndex.invocant_nodes`. Refs
    /// whose class was already pinned during the walk keep their value.
    fn apply_chain_typing_invocants(&mut self, idx: &ChainTypingIndex<'a>) {
        // Collect ref indices + their invocant nodes first so we
        // don't borrow `self.refs` mutably while also calling the
        // bag-routed resolver (which reads `&self`).
        let mut pending: Vec<(usize, Node<'a>)> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if let RefKind::MethodCall {
                invocant_span: Some(sp),
                ..
            } = &r.kind
            {
                if self.method_call_invocant.contains_key(&i) {
                    continue;
                }
                if let Some(n) = idx.invocant_nodes.get(&(sp.start, sp.end)).copied() {
                    pending.push((i, n));
                }
            }
        }
        for (i, node) in pending {
            if let Some(class) = self.resolve_invocant_class_tree(node) {
                self.method_call_invocant.insert(i, class);
            }
        }
    }

    /// Post-walk: emit even-position stringy args of every resolved
    /// `MethodCall` ref as `HashKeyAccess` refs owned by
    /// `Sub{invocant_class, method_name}`. Pairs with the
    /// `HashKeyDef` symbols that `has` / `bless { … }` synthesize on
    /// the callee side; without these refs, `ref_at` on a constructor
    /// arg only finds the broad `MethodCall` ref and rename clobbers
    /// the wrong token.
    ///
    /// Runs post-PostFold so `invocant_class` is already filled
    /// against the canonical bag — moves the keys-emission gating
    /// off the walk-time `invocant_class.is_some()` shortcut that
    /// previously forced syntactic walk-time class resolution to
    /// keep working.
    fn emit_method_call_arg_keys(&mut self, idx: &ChainTypingIndex<'a>) {
        // Snapshot first so we can `&mut self` the emit loop below.
        // `parametric` = true → Parametric receiver, the type is
        // the gate (cross-file producer's HashKeyDef isn't visible
        // here; the Parametric witness already pinned the class).
        // false → strict has_hash_key_def gating (prevents
        // `Foo::bar(name=>1)` from latching onto `Sub{Foo,new}`
        // keys when `name` isn't actually a `bar` arg).
        // Three paths for hash-key arg owner:
        //
        //  * **Parametric receiver claims the method.** Ask the
        //    flavor: `p.method_arg_owner(method)`. `Some` means
        //    "I claim — emit unconditionally with this owner;
        //    the type IS the gate." Cross-file producer's
        //    HashKeyDef may not be visible at consumer build;
        //    the type already pinned the class.
        //  * **Non-Parametric receiver, locally typed.** Fall
        //    through to `Sub { package: receiver, name: method
        //    }` with `has_hash_key_def` strict-eq gating —
        //    prevents `Foo::bar(name=>1)` from latching onto
        //    `Sub{Foo,new}` keys.
        //  * **Chain receiver, type unresolvable at build.**
        //    Build doesn't have `module_index` access, so a
        //    chain hop whose link-type lives in another file
        //    (helper-style: `$c->sner_r->search({...})`) returns
        //    None at this point. Emit `HashKeyAccess` refs
        //    eagerly with `owner: None`; enrichment runs later
        //    with `module_index` and fixes the owner once the
        //    receiver type is resolvable. Same precedent as
        //    cross-file invocant refresh (PR #34): build emits
        //    what it can; enrichment fills gaps.
        enum Path<'a> {
            Claimed(HashKeyOwner, Node<'a>),
            Strict(HashKeyOwner, Node<'a>),
            Deferred(Node<'a>),
        }
        let mut pending: Vec<Path<'a>> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if !matches!(r.kind, RefKind::MethodCall { .. }) {
                continue;
            }
            let Some(args) = idx
                .method_call_args
                .get(&(r.span.start, r.span.end))
                .copied()
            else {
                continue;
            };
            let invocant_node = idx
                .invocant_nodes
                .get(&match r.kind {
                    RefKind::MethodCall { invocant_span: Some(s), .. } => (s.start, s.end),
                    _ => continue,
                })
                .copied();
            let inv_ty = invocant_node.and_then(|n| self.invocant_type_at_node(n));
            if let Some(claimed) = inv_ty
                .as_ref()
                .and_then(|ty| ty.as_parametric())
                .and_then(|p| p.method_arg_owner(&r.target_name))
            {
                pending.push(Path::Claimed(claimed, args));
                continue;
            }
            if let Some(cls) = self.method_call_invocant.get(&i) {
                pending.push(Path::Strict(
                    HashKeyOwner::Sub {
                        package: Some(cls.clone()),
                        name: r.target_name.clone(),
                    },
                    args,
                ));
                continue;
            }
            // Receiver type unresolvable at build. Only defer for
            // chain receivers — bareword/variable receivers that
            // didn't type are likely user-error or untyped code,
            // not cross-file. Chain receivers (invocant is a
            // method-call expression) are exactly the case
            // post-enrichment can fix.
            let is_chain_receiver = invocant_node
                .map(|n| n.kind() == "method_call_expression")
                .unwrap_or(false);
            if is_chain_receiver {
                pending.push(Path::Deferred(args));
            }
        }
        for path in pending {
            match path {
                Path::Claimed(owner, args) => {
                    self.emit_call_arg_key_accesses(args, Gate::Open(owner));
                }
                Path::Strict(owner, args) => {
                    self.emit_call_arg_key_accesses(args, Gate::Strict(owner));
                }
                Path::Deferred(args) => {
                    self.emit_call_arg_key_accesses(args, Gate::Deferred);
                }
            }
        }
    }


    /// Phase 4 of the worklist refactor: this is the tiny driver. Each
    /// step is a named helper below. The call-binding propagator and
    /// hash-key-owner fixup are post-fold sync passes — they're
    /// conceptually "not reducers" (per the spec) and stay procedural,
    /// but factored out as named methods. Mirror reducers
    /// (`DelegationReducer`, `SelfMethodTailReducer`,
    /// `FluentArityDispatch` for arity, `PluginOverrideReducer`) live
    /// in `witnesses.rs` and carry the same logic — Phase 6's worklist
    /// driver will switch the loop bodies below to registry calls.
    fn resolve_return_types(&mut self) {
        self.emit_arity_return_witnesses();
        self.emit_method_call_return_edges();
        let (mut return_types, mut return_provenance) = self.fold_per_sub_return_arms();
        self.seed_plugin_overrides_into_return_types(&mut return_types, &mut return_provenance);
        self.write_back_sub_return_types(&return_provenance);
        self.propagate_call_bindings_to_constraints(&return_types);
        self.fixup_call_bound_hash_key_owners(&return_types);
    }

    /// Re-emittable: for every `MethodCall` ref whose
    /// `invocant_class` is filled (walk-time syntax-known invocants
    /// like `Foo->m`, plus PostFold-resolved variable invocants),
    /// publish `Expression(refidx) → Edge(MethodOnClass{class, method})`
    /// so the chain typer's `bag_query_expression` chases the
    /// receiver-and-method-resolved type through the class-keyed
    /// attachment. Refs without a filled class skip emission —
    /// without a known class there's no class-keyed slot to target.
    ///
    /// Clear-and-emit on tag `method_call_return` so repeat calls
    /// inside the worklist driver stay idempotent.
    fn emit_method_call_return_edges(&mut self) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};

        self.bag.remove_by_source_tag("method_call_return");

        let mut edges: Vec<Witness> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if !matches!(r.kind, RefKind::MethodCall { .. }) {
                continue;
            }
            // Refs we've already handed a Parametric witness keep
            // their custom InferredType — publishing the receiver-
            // class's plain return-type edge would mask the
            // row-class arg via FrameworkAwareTypeFold's
            // class-axis short-circuit. See
            // `parametric_emitted_refs` doc on the Builder struct.
            if self.parametric_emitted_refs.contains(&i) {
                continue;
            }
            let Some(class) = self.method_call_invocant.get(&i) else {
                continue;
            };
            edges.push(Witness {
                attachment: WitnessAttachment::Expression(crate::witnesses::RefIdx(i as u32)),
                source: WitnessSource::Builder("method_call_return".into()),
                payload: WitnessPayload::Edge(WitnessAttachment::MethodOnClass {
                    class: class.clone(),
                    name: r.target_name.clone(),
                }),
                span: r.span,
            });
        }
        for w in edges {
            self.bag.push(w);
        }
    }

    /// Single-attachment registry query for `Expr(span)`. Used by
    /// `emit_arity_return_witnesses` (per-RI) and the implicit-last-
    /// expression fallback in `fold_per_sub_return_arms`. Threads the
    /// file's scope topology + per-package framework as a `BagContext`
    /// so Edge chases through `Variable{...}` use scope-chain +
    /// framework-aware semantics.
    fn bag_query_expr_span(&self, span: Span) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let att = WitnessAttachment::Expr(span);
        let reg = ReducerRegistry::with_defaults();
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
            };
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint: None,
            context: Some(&ctx),
        };
        match reg.query(&self.bag, &q) {
            ReducedValue::Type(t) => Some(t),
            _ => None,
        }
    }

    /// Bag-routed lookup for a variable's type at a point. Walks the
    /// scope chain via `query_variable_type` (FrameworkAwareTypeFold,
    /// branch-arm fold, etc. all apply). Used by the post-walk chain
    /// typer once the bag is fully populated by `populate_witness_bag`.
    fn bag_query_variable(
        &self,
        name: &str,
        scope: ScopeId,
        point: Point,
    ) -> Option<InferredType> {
        crate::witnesses::query_variable_type(
            &self.bag,
            &self.scopes,
            &self.package_framework,
            name,
            scope,
            point,
        )
    }

    /// Bag-routed lookup for a sub's return type by name. Goes through
    /// `query_sub_return_type` so arity dispatch (FluentArityDispatch),
    /// the per-Symbol stored return (SubReturnReducer), and cross-file
    /// imports (recurse into the cached module's bag) all compose. `arity_hint` is `None` for the
    /// chain-typer's invocant resolution since chain typing wants
    /// "what does this sub return when called as I see it being
    /// called" — for invocant resolution that's typically the
    /// zero-arg form (a chain like `app->routes` calls `app()` then
    /// `routes()` on its return).
    fn bag_query_named_sub(&self, name: &str, arity_hint: Option<u32>) -> Option<InferredType> {
        let ctx = crate::witnesses::BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
            };
        crate::witnesses::query_sub_return_type(
            &self.bag,
            &self.symbols,
            name,
            arity_hint,
            Some(&ctx),
        )
    }

    /// Bag-routed lookup for a method-call expression's return type
    /// via its ref index. Mirrors `FileAnalysis::method_call_return_type_via_bag`
    /// but reads `&self.bag` (the in-progress builder bag). Includes
    /// the `FirstParam → ClassName` projection so chain-typer
    /// consumers see a concrete class instead of a parametric type.
    fn bag_query_expression(
        &self,
        ref_idx: crate::witnesses::RefIdx,
        arity_hint: Option<u32>,
    ) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let att = WitnessAttachment::Expression(ref_idx);
        let reg = ReducerRegistry::with_defaults();
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
            };
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint,
            context: Some(&ctx),
        };
        match reg.query(&self.bag, &q) {
            ReducedValue::Type(InferredType::FirstParam { package }) => {
                Some(InferredType::ClassName(package))
            }
            ReducedValue::Type(t) => Some(t),
            _ => None,
        }
    }


    /// Step 2: emit `ArityReturn` witnesses on `Symbol(sub_id)` — the
    /// payload `FluentArityDispatch` (the spec's "ArityReturnReducer")
    /// folds at query time. Only for arity-DISCRIMINATED subs (≥ 1
    /// `Zero`/`Exact(_)` arm); plain subs don't emit, otherwise
    /// `FluentArityDispatch` would answer with one arm even when the
    /// per-arm fold says "agreement failed" (different arms differ).
    /// Walk-time arity classification stays a `Some(_)` enum on
    /// `ReturnInfo`; the type is read out per-RI by querying the
    /// body's `Expr(span)` against the current bag.
    ///
    /// Idempotent across re-runs — clears every prior `arity_detection`
    /// witness from the bag before re-emitting. The worklist driver
    /// calls `resolve_return_types` repeatedly until fixed point;
    /// without this clear-and-emit, each iteration would duplicate
    /// every arity witness in the bag.
    fn emit_arity_return_witnesses(&mut self) {
        use crate::witnesses::{
            TypeObservation, Witness, WitnessAttachment, WitnessPayload, WitnessSource,
        };

        self.bag.remove_by_source_tag("arity_detection");

        let mut arity_discriminated_scopes: std::collections::HashSet<ScopeId> =
            std::collections::HashSet::new();
        for ri in &self.return_infos {
            if matches!(
                ri.arity_branch,
                Some(ArityBranch::Zero) | Some(ArityBranch::Exact(_))
            ) {
                arity_discriminated_scopes.insert(ri.scope);
            }
        }

        let mut arity_witnesses: Vec<Witness> = Vec::new();
        for ri in &self.return_infos {
            let Some(branch) = ri.arity_branch else { continue };
            if !arity_discriminated_scopes.contains(&ri.scope) {
                continue;
            }
            let Some(t) = ri.body_span.and_then(|sp| self.bag_query_expr_span(sp)) else {
                continue;
            };
            let Some(sym_id) = self.find_sub_symbol_for_scope(ri.scope) else { continue };
            let arg_count = match branch {
                ArityBranch::Zero => Some(0u32),
                ArityBranch::Exact(n) => Some(n),
                ArityBranch::Default => None,
            };
            arity_witnesses.push(Witness {
                attachment: WitnessAttachment::Symbol(sym_id),
                source: WitnessSource::Builder("arity_detection".into()),
                payload: WitnessPayload::Observation(TypeObservation::ArityReturn {
                    arg_count,
                    return_type: t.clone(),
                }),
                span: ri.span,
            });
            // Mirror onto `MethodOnClass{class, name}` so class-keyed
            // dispatch from `find_method_return_type` picks up the
            // arm even when the caller doesn't have a sym_id in hand.
            // Resolve the sub's `(class, name)` via the symbol table —
            // arity-discriminated subs are always concretely named.
            if let Some(sym) = self.symbols.get(sym_id.0 as usize) {
                if let Some(class) = sym.package.clone() {
                    arity_witnesses.push(Witness {
                        attachment: WitnessAttachment::MethodOnClass {
                            class,
                            name: sym.name.clone(),
                        },
                        source: WitnessSource::Builder("arity_detection".into()),
                        payload: WitnessPayload::Observation(TypeObservation::ArityReturn {
                            arg_count,
                            return_type: t,
                        }),
                        span: ri.span,
                    });
                }
            }
        }
        for w in arity_witnesses {
            self.bag.push(w);
        }
    }

    /// Step 3: fold each Sub/Method scope's return arms into a single
    /// type via `resolve_return_type` (1+ arms agree → `Some(t)`,
    /// disagreement → `None`). Falls back to `last_expr_span`'s
    /// Expr(span) query for subs without explicit `return`s — Perl's
    /// last statement is the implicit return. Provenance is recorded
    /// as `ReducerFold { reducer: "return_arms" }` so `--dump-package`
    /// can answer "why does this return X?".
    fn fold_per_sub_return_arms(
        &mut self,
    ) -> (
        std::collections::HashMap<String, InferredType>,
        std::collections::HashMap<String, crate::file_analysis::TypeProvenance>,
    ) {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let mut return_types: std::collections::HashMap<String, InferredType> =
            std::collections::HashMap::new();
        let mut return_provenance: std::collections::HashMap<
            String,
            crate::file_analysis::TypeProvenance,
        > = std::collections::HashMap::new();

        let reg = ReducerRegistry::with_defaults();

        // Collect into a buffer first so we don't hold a `&self`
        // borrow across the `&mut self.resolved_returns.insert(...)`
        // step below.
        let mut updates: Vec<(SymbolId, String, InferredType, Vec<String>)> = Vec::new();

        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
        };

        for scope in &self.scopes {
            let sub_name = match &scope.kind {
                ScopeKind::Sub { name } | ScopeKind::Method { name } => name.clone(),
                _ => continue,
            };

            // Find the sym for this scope first — used both for the
            // bag-source fallback below and for the per-sym update
            // below.
            let sub_sym_id = self
                .symbols
                .iter()
                .find(|s| {
                    s.name == sub_name
                        && matches!(s.kind, SymKind::Sub | SymKind::Method)
                        && s.span.start <= scope.span.start
                        && scope.span.end <= s.span.end
                })
                .map(|s| s.id);

            // Two sources, in priority order:
            //   1. Bag query on `Symbol(sub_id)` — covers explicit
            //      `return EXPR` arms (each emits one `branch_arm`
            //      Edge to `Expr(body_span)` on Symbol) and ternary
            //      returns (each arm emits its own Edge). Both
            //      `SymbolReturnArmFold` (1+ arms via
            //      `resolve_return_type`) and `BranchArmFold` (≥2
            //      arms agree) can answer here; the registered order
            //      gives the Symbol-fold first refusal.
            //   2. `Expr(last_expr_span)` for subs without explicit
            //      returns — Perl's implicit-last-statement return.
            let bag_answer = sub_sym_id.and_then(|id| {
                let att = WitnessAttachment::Symbol(id);
                let q = ReducerQuery {
                    attachment: &att,
                    point: None,
                    framework: FrameworkFact::Plain,
                    arity_hint: None,
                    context: Some(&ctx),
                };
                if let ReducedValue::Type(t) = reg.query(&self.bag, &q) {
                    Some(t)
                } else {
                    None
                }
            });
            let (resolved, evidence) = if let Some(t) = bag_answer {
                (Some(t), vec!["symbol_bag".into()])
            } else {
                let r = self
                    .last_expr_span
                    .get(&scope.id)
                    .and_then(|sp| self.bag_query_expr_span(*sp));
                let ev = if r.is_some() {
                    vec!["last_expr".into()]
                } else {
                    Vec::new()
                };
                (r, ev)
            };

            if let (Some(rt), Some(sid)) = (resolved, sub_sym_id) {
                updates.push((sid, sub_name, rt, evidence));
            }
        }

        // Apply per-sym updates. `resolved_returns` is sym_id-keyed so
        // two packages defining the same sub name (`@ISA = (A, B)`
        // where both A::m and B::m exist) each get their own entry —
        // a name-keyed `HashMap<String, _>` would last-write-wins
        // and silently surface B's answer for both, breaking
        // class-keyed dispatch.
        for (sid, sub_name, rt, evidence) in updates {
            self.resolved_returns.insert(sid, rt.clone());
            return_types.insert(sub_name.clone(), rt);
            return_provenance.insert(
                sub_name,
                crate::file_analysis::TypeProvenance::ReducerFold {
                    reducer: "return_arms".into(),
                    evidence,
                },
            );
        }
        (return_types, return_provenance)
    }

    /// Step 4: seed Plugin overrides into `return_types` before
    /// delegation / self_method_tail propagation. Routes through
    /// `PluginOverrideReducer`'s own `claims` + `reduce` so the
    /// predicate stays in lock-step with the registry — if the reducer
    /// gains a tie-breaker or attachment-shape filter, the seed pass
    /// inherits it for free instead of silently disagreeing.
    fn seed_plugin_overrides_into_return_types(
        &mut self,
        return_types: &mut std::collections::HashMap<String, InferredType>,
        return_provenance: &mut std::collections::HashMap<
            String,
            crate::file_analysis::TypeProvenance,
        >,
    ) {
        use crate::witnesses::{
            FrameworkFact, PluginOverrideReducer, ReducedValue, ReducerQuery, Witness,
            WitnessAttachment, WitnessReducer,
        };

        let plugin_reducer = PluginOverrideReducer;
        // Two-step: collect (sym_id, name, rt) without holding a
        // borrow on self.symbols, then write to resolved_returns
        // and return_types in a separate pass. Same pattern as
        // `fold_per_sub_return_arms` to avoid the &self / &mut self
        // borrow conflict.
        let mut overrides: Vec<(SymbolId, String, InferredType)> = Vec::new();
        for sym in &self.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                continue;
            }
            let att = WitnessAttachment::Symbol(sym.id);
            let claimed: Vec<&Witness> = self
                .bag
                .for_attachment(&att)
                .into_iter()
                .filter(|w| plugin_reducer.claims(w))
                .collect();
            if claimed.is_empty() {
                continue;
            }
            let q = ReducerQuery {
                attachment: &att,
                point: None,
                framework: FrameworkFact::Plain,
                arity_hint: None,
                context: None,
            };
            if let ReducedValue::Type(t) = plugin_reducer.reduce(&claimed, &q) {
                overrides.push((sym.id, sym.name.clone(), t));
            }
        }
        for (sid, name, t) in overrides {
            self.resolved_returns.insert(sid, t.clone());
            return_types.insert(name.clone(), t);
            return_provenance.remove(&name);
        }
    }

    /// Step 7: writeback. Iterate `resolved_returns` and publish each
    /// resolved per-symbol return type to the bag on two attachments:
    ///
    ///   - `Symbol(sym_id)` — id-keyed answer, claimed by
    ///     `SubReturnReducer`. Used by per-symbol queries that already
    ///     know which sym they're asking about (cross-file imports
    ///     recursing into a cached bag, dump-package iteration).
    ///   - `MethodOnClass{class, name}` — class-keyed answer, claimed
    ///     by `MethodOnClassReducer`. Only the FIRST sym for a given
    ///     `(class, name)` pair publishes here (the "primary"); later
    ///     syms participate via `ArityReturn` observations only. This
    ///     is what `find_method_return_type` queries for the no-arity
    ///     fallback.
    ///
    /// Cross-file imports do not get a writeback push here — they
    /// resolve lazily via `query_sub_return_type` walking
    /// `module_index.find_exporters` and recursing into the cached
    /// module's `Symbol(cached_sid)`. Same registry, same rules; no
    /// local mirror needed.
    ///
    /// `return_self_method` is set for subs whose return couldn't
    /// be collapsed in-file. Plugin overrides flow through
    /// `return_types` via the bag-priority seeding step, with
    /// `return_provenance` cleared so we don't clobber the existing
    /// `PluginOverride` entry written by `apply_type_overrides`. No
    /// special-case skip needed — overrides win because they're
    /// higher-priority bag witnesses.
    ///
    /// Idempotent across re-runs: `bag.remove_by_source_tag("local_return")`
    /// (and `"plugin_bridge"` / `"inheritance"`) at the start of every
    /// call drops the prior pass's witnesses before re-emitting. The
    /// worklist driver calls `resolve_return_types` repeatedly until
    /// fixed point; without this clear-and-emit, each iteration would
    /// duplicate every sub's writeback witnesses.
    fn write_back_sub_return_types(
        &mut self,
        return_provenance: &std::collections::HashMap<
            String,
            crate::file_analysis::TypeProvenance,
        >,
    ) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};

        self.bag.remove_by_source_tag("local_return");
        self.bag.remove_by_source_tag("plugin_bridge");
        self.bag.remove_by_source_tag("inheritance");

        // First pass: update `resolved_returns` from worklist's
        // per-name folds and (when no fold answer exists) `return_self_method`
        // from the in-flight self-method-tail bookkeeping. The map is
        // the post-field source of truth for "what does this sym
        // return?" — walk-time synthesis seeds it directly,
        // worklist-resolved folds update it here.
        // `resolved_returns` is the post-D1 source of truth for
        // per-sym return types. Walk-time synthesis writes to it
        // directly (`record_framework_accessor_witness`,
        // plugin emit handlers). Worklist's
        // `fold_per_sub_return_arms` and
        // `seed_plugin_overrides_into_return_types` write per-sym
        // here too — the legacy `return_types: HashMap<String, _>`
        // is kept around only for downstream consumers that look up
        // by name (call-binding propagation, hash-key fixup) and is
        // intentionally last-write-wins on name collisions; the bag
        // path keys by `sym_id` and stays correct.
        for sym in &self.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                continue;
            }
            if let Some(prov) = return_provenance.get(&sym.name) {
                self.type_provenance.insert(sym.id, prov.clone());
            }
        }
        // Publish every Sub/Method's resolved return type to the bag
        // on TWO attachments:
        //   - `Symbol(sym_id)` — id-keyed lookup. The Symbol attachment
        //     carries every per-symbol fact in the bag (arity arms,
        //     plugin overrides). Writeback's value here is the
        //     "if you ask THIS sym what it returns, ignoring class
        //     context, here's the inferred answer." Cross-file imports
        //     reach this same shape by recursing into the cached
        //     module's bag with `Symbol(cached_sid)`.
        //   - `MethodOnClass{class, name}` — class-keyed lookup. The
        //     authoritative class-scoped answer that
        //     `find_method_return_type` queries through. Multiple
        //     symbols can share `(class, name)` (Mojo::Base getter +
        //     writer); the FIRST one's resolved return type is the
        //     "primary" — what callers see when they don't constrain
        //     by arity. Per-arity dispatch lives on the same
        //     attachment via `ArityReturn` observations
        //     (`record_framework_accessor_witness`,
        //     `emit_arity_return_witnesses`'s mirror below).
        // Catches both worklist-resolved returns (set above) and
        // framework-pinned ones (Mojo::Base accessors, Moo `has`,
        // DBIC column accessors — these write directly into
        // `resolved_returns` at walk-time synthesis, never flow
        // through `return_types`).
        let mut writeback_witnesses: Vec<Witness> = Vec::new();
        // Track primary for each (class, method_name) — the first
        // Sub/Method symbol declared in (or synthesized into) the
        // class wins the "primary" slot on `MethodOnClass`. The
        // primary defines the plain `InferredType` answer that
        // `MethodOnClassReducer` returns when the caller has no arity
        // preference. Crucially we walk EVERY sym (rt-less ones too):
        // a Mojo getter without a default (`has 'title'`) has
        // `return_type: None` and must still claim the primary slot —
        // otherwise the secondary fluent writer's `ClassName(C)`
        // would silently surface as the default, wrong-answering
        // `find_method_return_type` at arity=None.
        let mut method_on_class_seen: std::collections::HashSet<(String, String)> =
            std::collections::HashSet::new();
        for sym in &self.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                continue;
            }
            // Mark primary FIRST — even when this sym has no rt, it
            // still occupies the primary slot for (class, name).
            let is_primary = if let Some(class) = sym.package.as_ref() {
                method_on_class_seen.insert((class.clone(), sym.name.clone()))
            } else {
                false
            };
            // Symbol writeback: when this sym has a resolved
            // return type, push it. Two sources:
            //   * `resolved_returns` — walk-time synthesis +
            //     worklist's per-name folds. Concrete InferredType.
            //   * pre-existing `Symbol(sid)` witness with
            //     `Edge(_)` payload — plugin-emitted lazy return
            //     (mojo-helpers' `return_via_edge` shape). The
            //     synthesis emits ONLY on `Symbol(sid)`; this
            //     writeback mirrors to `MethodOnClass{class, name}`
            //     for class-scoped synth so cross-file lookup
            //     reaches it via the same path concrete returns
            //     use.
            let payload_for_writeback: Option<WitnessPayload> =
                if let Some(rt) = self.resolved_returns.get(&sym.id) {
                    Some(WitnessPayload::InferredType(rt.clone()))
                } else {
                    self.bag
                        .all()
                        .iter()
                        .find(|w| {
                            matches!(&w.attachment, WitnessAttachment::Symbol(s) if *s == sym.id)
                                && matches!(w.payload, WitnessPayload::Edge(_))
                        })
                        .map(|w| w.payload.clone())
                };
            let Some(payload) = payload_for_writeback else { continue };
            // Concrete return types come through `resolved_returns`
            // and need pushing on Symbol(sid). Edge-payload returns
            // were already emitted on Symbol(sid) by the plugin;
            // skip re-pushing those (would duplicate the witness).
            let already_on_symbol = matches!(payload, WitnessPayload::Edge(_));
            if !already_on_symbol {
                writeback_witnesses.push(Witness {
                    attachment: WitnessAttachment::Symbol(sym.id),
                    source: WitnessSource::Builder("local_return".into()),
                    payload: payload.clone(),
                    span: sym.span,
                });
            }
            // MethodOnClass primary: only the primary (first sym for
            // (class, name)) publishes the plain payload slot.
            // Secondary syms participate via `ArityReturn`
            // observations only.
            if is_primary {
                if let Some(class) = sym.package.clone() {
                    writeback_witnesses.push(Witness {
                        attachment: WitnessAttachment::MethodOnClass {
                            class,
                            name: sym.name.clone(),
                        },
                        source: WitnessSource::Builder("local_return".into()),
                        payload,
                        span: sym.span,
                    });
                }
            }
        }
        // Plugin-namespace bridges: each `PluginNamespace` declares
        // `bridges: [Class(C1), Class(C2), ...]` for entities that
        // should be reachable from those classes' method dispatch.
        // Emit `MethodOnClass{C_i, entity.name} → Edge(Symbol(entity.id))`
        // edges so `find_method_return_type` resolves bridged
        // entities through the same bag path it uses for direct
        // class methods. Without these edges,
        // `find_method_return_type("Mojolicious", "admin", _, _)`
        // would miss helpers emitted on Mojolicious::Controller and
        // bridged into the Mojolicious lookup namespace.
        let zero = Span {
            start: Point { row: 0, column: 0 },
            end: Point { row: 0, column: 0 },
        };
        for ns in &self.plugin_namespaces {
            for b in &ns.bridges {
                let crate::file_analysis::Bridge::Class(class) = b;
                for sym_id in &ns.entities {
                    let Some(sym) = self.symbols.get(sym_id.0 as usize) else { continue };
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                        continue;
                    }
                    writeback_witnesses.push(Witness {
                        attachment: WitnessAttachment::MethodOnClass {
                            class: class.clone(),
                            name: sym.name.clone(),
                        },
                        source: WitnessSource::Builder("plugin_bridge".into()),
                        payload: WitnessPayload::Edge(WitnessAttachment::Symbol(*sym_id)),
                        span: zero,
                    });
                }
            }
        }
        // Inheritance edges: for every (child, parent) entry in
        // `package_parents`, emit `MethodOnClass(child, m) →
        // Edge(MethodOnClass(parent, m))` for each method `m` known
        // on the parent (locally-declared or framework-synthesized).
        // The registry's edge-chase walks these the same way it
        // walks any other Edge — no procedural ancestor walker
        // needed in `query_rec`. Cross-file parents inherit via
        // the registry's existing cached-bag recursion in
        // `query_rec`'s `MethodOnClass` arm: an edge into
        // `MethodOnClass(P_cross, m)` re-enters `query` with the
        // same attachment shape, and the shared visited set closes
        // any mutual loops.
        //
        // Methods on the parent are enumerated locally — for parents
        // that are themselves cross-file the recursion delivers
        // their methods via the cached bag. Each local method-name
        // emission is enough: when child's `MethodOnClass(C, m)`
        // bag has no local witness, the inheritance Edge points at
        // `MethodOnClass(P, m)` and the registry follows it.
        let parents_snapshot: Vec<(String, Vec<String>)> = self
            .package_parents
            .iter()
            .map(|(c, ps)| (c.clone(), ps.clone()))
            .collect();
        for (child, parents) in &parents_snapshot {
            // Per-child: track which methods already got an
            // inheritance edge, so the FIRST parent in `@ISA` order
            // wins (Perl's default DFS-MRO is left-to-right). Without
            // this dedup, two parents both defining `m` would push
            // two edges on `MethodOnClass(child, m)` and the
            // materializer's latest-wins reducer would silently pick
            // the second-emitted parent.
            let mut emitted_for_child: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            for parent in parents {
                if parent == child {
                    continue;
                }
                for sym in &self.symbols {
                    if sym.package.as_deref() != Some(parent.as_str()) {
                        continue;
                    }
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                        continue;
                    }
                    if !emitted_for_child.insert(sym.name.clone()) {
                        continue;
                    }
                    writeback_witnesses.push(Witness {
                        attachment: WitnessAttachment::MethodOnClass {
                            class: child.clone(),
                            name: sym.name.clone(),
                        },
                        source: WitnessSource::Builder("inheritance".into()),
                        payload: WitnessPayload::Edge(WitnessAttachment::MethodOnClass {
                            class: parent.clone(),
                            name: sym.name.clone(),
                        }),
                        span: zero,
                    });
                }
            }
        }
        for w in writeback_witnesses {
            self.bag.push(w);
        }
    }

    /// Step 8: `CallBindingPropagator` (per Phase 4 spec — not a
    /// witness reducer, just the bag-and-TC sync pass that runs after
    /// the fold). For each `my $cfg = get_config()` binding recorded
    /// during the walk, push BOTH the legacy `TypeConstraint` and the
    /// corresponding `Variable` witness so any later bag query about
    /// `$cfg` sees the call-resolved type without a separate sync pass.
    /// Inline expression propagation (`get_config()->{key}` without an
    /// intermediate variable) is a separate code path — not handled here.
    ///
    /// Idempotent across re-runs — clears every prior `call_binding`
    /// witness from the bag and drops any matching TC before
    /// re-emitting. The worklist driver calls `resolve_return_types`
    /// repeatedly until fixed point; without this clear-and-emit, each
    /// iteration would duplicate the (variable, scope, span) TCs and
    /// Variable witnesses for every binding.
    fn propagate_call_bindings_to_constraints(
        &mut self,
        return_types: &std::collections::HashMap<String, InferredType>,
    ) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};

        // Clear-and-emit on tag `call_binding` so the witnesses stay
        // canonical across worklist iterations.
        self.bag.remove_by_source_tag("call_binding");
        for binding in &self.call_bindings {
            let rt = return_types
                .get(&binding.func_name)
                .cloned()
                .or_else(|| builtin_return_type(&binding.func_name));
            if let Some(rt) = rt {
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Variable {
                        name: binding.variable.clone(),
                        scope: binding.scope,
                    },
                    source: WitnessSource::Builder("call_binding".into()),
                    payload: WitnessPayload::InferredType(rt),
                    span: Span {
                        start: binding.span.start,
                        end: binding.span.start,
                    },
                });
            }
        }
    }

    /// Step 9: hash-key-owner fixup for variables bound to sub calls
    /// that return HashRef. Two normalizations beyond the naive name
    /// match:
    ///   1. Call names may be qualified (`Pkg::foo`) — strip the
    ///      package prefix since `return_types` and the symbol table
    ///      key on the bare name.
    ///   2. The bound func may itself just `return other()` — walk
    ///      the delegation chain to the sub that actually declares
    ///      the hash literal. Otherwise `sub chain { return
    ///      get_config() }` leaves `$cfg = chain(); $cfg->{host}`
    ///      with an owner that has no matching HashKeyDefs.
    fn fixup_call_bound_hash_key_owners(
        &mut self,
        return_types: &std::collections::HashMap<String, InferredType>,
    ) {
        let binding_map: std::collections::HashMap<&str, String> = self
            .call_bindings
            .iter()
            .filter(|b| {
                return_types
                    .get(bare_name(&b.func_name))
                    .map_or(false, |t| *t == InferredType::HashRef)
            })
            .map(|b| (b.variable.as_str(), bare_name(&b.func_name).to_string()))
            .collect();

        let sub_package: std::collections::HashMap<&str, Option<String>> = self
            .symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| (s.name.as_str(), s.package.clone()))
            .collect();

        let subs_with_own_keys: std::collections::HashSet<String> = self
            .symbols
            .iter()
            .filter_map(|s| {
                if let SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. },
                    ..
                } = &s.detail
                {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        // Method-call bindings: `my $c = $obj->method()` (including
        // dynamic `$obj->$m()` where $m was constant-folded during
        // method_call_binding emission). Same ownership logic as
        // function calls — point $c's hash-key accesses at the
        // HashKeyDefs inside `method`.
        let method_binding_map: std::collections::HashMap<&str, String> = self
            .method_call_bindings
            .iter()
            .map(|mcb| (mcb.variable.as_str(), mcb.method_name.clone()))
            .collect();

        for r in &mut self.refs {
            if let RefKind::HashKeyAccess {
                ref var_text,
                ref mut owner,
            } = r.kind
            {
                if let Some(func_name) = binding_map.get(var_text.as_str()) {
                    let resolved = walk_return_delegation_chain(
                        func_name,
                        &self.sub_return_delegations,
                        &subs_with_own_keys,
                    );
                    *owner = Some(HashKeyOwner::Sub {
                        package: sub_package.get(resolved.as_str()).cloned().unwrap_or(None),
                        name: resolved,
                    });
                } else if let Some(method_name) = method_binding_map.get(var_text.as_str()) {
                    let resolved = walk_return_delegation_chain(
                        method_name,
                        &self.sub_return_delegations,
                        &subs_with_own_keys,
                    );
                    if subs_with_own_keys.contains(&resolved) {
                        *owner = Some(HashKeyOwner::Sub {
                            package: sub_package.get(resolved.as_str()).cloned().unwrap_or(None),
                            name: resolved,
                        });
                    }
                }
            }
        }
    }

    /// Apply every `TypeOverride` in the registry to local Sub/Method
    /// symbols. Plugin-asserted return types win over inference; the
    /// override carries a `reason` that we record in
    /// `type_provenance` so a debugger can later answer "why does the
    /// LSP think `_route` returns `Mojolicious::Routes::Route`?"
    /// without re-running the build.
    ///
    /// Targets are matched on (name, package). Methods require an
    /// exact package match — overrides describe the home class, not
    /// the inheritance chain (a base class's override wins for that
    /// base's symbol; subclasses get it via the existing cross-file
    /// resolution path).
    ///
    /// Mechanism: pushes a Plugin-source `InferredType` witness onto
    /// `Symbol(sym_id)`. The `PluginOverrideReducer` priority
    /// short-circuit (witnesses.rs) makes that witness dominate any
    /// inferred Symbol+InferredType evidence in the same fold. Direct
    /// writes to `Symbol.return_type` happen later in
    /// `resolve_return_types`, sourced from the bag — this keeps the
    /// override flow uniform with the rest of the type-inference
    /// pipeline (no parallel "pinned by override" path).
    fn apply_type_overrides(&mut self) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};

        // Snapshot first — can't borrow self.plugins while mutating
        // self.bag + self.type_provenance below.
        let pairs: Vec<(String, plugin::TypeOverride)> = self.plugins
            .overrides()
            .map(|(id, o)| (id.to_string(), o.clone()))
            .collect();
        if pairs.is_empty() {
            return;
        }
        for (plugin_id, ov) in pairs {
            // Collect target SymbolIds in a snapshot so we can mutate
            // self.bag + self.type_provenance below without holding
            // an aliasing borrow on self.symbols.
            let mut targets: Vec<SymbolId> = Vec::new();
            for sym in &self.symbols {
                if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                let target_matches = match &ov.target {
                    plugin::OverrideTarget::Method { class, name } => {
                        sym.name == *name && sym.package.as_deref() == Some(class.as_str())
                    }
                    plugin::OverrideTarget::Sub { package, name } => {
                        sym.name == *name && sym.package == *package
                    }
                };
                if !target_matches { continue; }
                if matches!(sym.detail, SymbolDetail::Sub { .. }) {
                    targets.push(sym.id);
                }
            }
            for sym_id in targets {
                // Zero-extent span — core-synthesized witness, no
                // user-visible "because: …" anchor needed beyond the
                // provenance entry below.
                let zero = Span {
                    start: Point { row: 0, column: 0 },
                    end: Point { row: 0, column: 0 },
                };
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Symbol(sym_id),
                    source: WitnessSource::Plugin(plugin_id.clone()),
                    payload: WitnessPayload::InferredType(ov.return_type.clone()),
                    span: zero,
                });
                self.type_provenance.insert(
                    sym_id,
                    TypeProvenance::PluginOverride {
                        plugin_id: plugin_id.clone(),
                        reason: ov.reason.clone(),
                    },
                );
            }
        }
    }

    fn resolve_hash_key_owners(&mut self) {
        use crate::witnesses::{WitnessAttachment, WitnessPayload};
        // Build type constraint lookup from the bag — Variable
        // witnesses with `InferredType` payloads are the seed-time
        // type-constraint shape (`push_type_constraint` mirrors every
        // TC into one of these). The bag is canonical at this phase.
        let mut type_map: std::collections::HashMap<String, Vec<(ScopeId, InferredType, Point)>> =
            std::collections::HashMap::new();
        for w in self.bag.all() {
            if let (
                WitnessAttachment::Variable { name, scope },
                WitnessPayload::InferredType(t),
            ) = (&w.attachment, &w.payload)
            {
                type_map
                    .entry(name.clone())
                    .or_default()
                    .push((*scope, t.clone(), w.span.start));
            }
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
                                // Hash-key owner: read
                                // `hash_key_class()` so a Parametric
                                // TC narrows to its row-class arg.
                                // For non-Parametric this is the
                                // dispatch class. CLAUDE.md #10.
                                if let Some(cn) = tc_type.hash_key_class() {
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
#[path = "builder_tests.rs"]
mod tests;

#[cfg(test)]
#[path = "type_inference_invariants_tests.rs"]
mod invariants_tests;
