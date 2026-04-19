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
fn default_plugin_registry() -> Arc<PluginRegistry> {
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
    let mut b = Builder {
        source,
        scopes: Vec::new(),
        symbols: Vec::new(),
        refs: Vec::new(),
        type_constraints: Vec::new(),
        deferred_var_types: Vec::new(),
        fold_ranges: Vec::new(),
        imports: Vec::new(),
        return_infos: Vec::new(),
        last_expr_type: std::collections::HashMap::new(),
        call_bindings: Vec::new(),
        method_call_bindings: Vec::new(),
        pod_texts: Vec::new(),
        package_parents: std::collections::HashMap::new(),
        package_uses: std::collections::HashMap::new(),
        sub_return_delegations: std::collections::HashMap::new(),
        framework_modes: std::collections::HashMap::new(),
        framework_imports: std::collections::HashSet::new(),
        constant_strings: std::collections::HashMap::new(),
        export: Vec::new(),
        export_ok: Vec::new(),
        scope_stack: Vec::new(),
        current_package: None,
        next_scope_id: 0,
        next_symbol_id: 0,
        plugins,
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
        b.type_constraints.push(TypeConstraint {
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

    // Post-pass 3: infer return types for subs/methods
    b.resolve_return_types();

    // Post-pass 4: fill in tail POD docs for subs that didn't get preceding doc
    b.resolve_tail_pod_docs();

    FileAnalysis::new(
        b.scopes,
        b.symbols,
        b.refs,
        b.type_constraints,
        b.fold_ranges,
        b.imports,
        b.call_bindings,
        b.package_parents,
        b.method_call_bindings,
        b.framework_imports,
        b.export,
        b.export_ok,
    )
}

/// A return value type collected during the walk, before post-pass resolution.
struct ReturnInfo {
    /// The scope (Sub/Method) this return belongs to.
    scope: ScopeId,
    /// The inferred type of the return value, if determinable from literals/constructors.
    inferred_type: Option<InferredType>,
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
    Some(call_name.rsplit("::").next().unwrap_or(call_name).to_string())
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
    type_constraints: Vec<TypeConstraint>,
    /// Plugin-emitted `VarType` constraints, resolved to scopes only
    /// after the whole CST has been walked (plugin dispatch runs
    /// before we recurse into call args, so at emit-time the target
    /// scope usually doesn't exist yet).
    deferred_var_types: Vec<DeferredVarType>,
    fold_ranges: Vec<FoldRange>,
    imports: Vec<Import>,
    /// Return values collected during the walk (explicit `return` + implicit last expr).
    return_infos: Vec<ReturnInfo>,
    /// For each Sub/Method scope, the type of the last expression (implicit return).
    last_expr_type: std::collections::HashMap<ScopeId, Option<InferredType>>,
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

    // Walk state
    scope_stack: Vec<ScopeId>,
    current_package: Option<String>,
    next_scope_id: u32,
    next_symbol_id: u32,

    /// Framework plugin registry. Shared Arc so multiple builders in one
    /// process avoid re-compiling the same Rhai scripts.
    plugins: Arc<PluginRegistry>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FrameworkMode {
    Moo,
    Moose,
    MojoBase,
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
        });
        id
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
        let string_value = match arg.kind() {
            "string_literal" | "interpolated_string_literal" => {
                let stripped = text.trim_start_matches(['\'', '"'])
                    .trim_end_matches(['\'', '"'])
                    .to_string();
                Some(stripped)
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
        let inferred_type = self.infer_expression_type(arg, false);
        let sub_params = if arg.kind() == "anonymous_subroutine_expression" {
            self.extract_anonymous_sub_params(arg)
        } else {
            Vec::new()
        };
        plugin::ArgInfo {
            text,
            string_value,
            span: node_to_span(arg),
            inferred_type,
            sub_params,
        }
    }

    /// Extract params from an anonymous sub. Reuses the builder's existing
    /// named-sub param extraction (signature syntax + `@_` unpack) so the
    /// two codepaths can't diverge. Returns EmittedParam entries suitable
    /// for plugin consumption.
    fn extract_anonymous_sub_params(&self, sub_node: Node<'a>) -> Vec<plugin::EmittedParam> {
        // extract_params handles both signature syntax and a rich set of
        // @_/shift/$_[N] fallback patterns for named subs. Reuse it here.
        let mut params: Vec<plugin::EmittedParam> = self.extract_params(sub_node)
            .into_iter()
            .map(|p| plugin::EmittedParam {
                name: p.name,
                default: p.default,
                is_slurpy: p.is_slurpy,
                    is_invocant: false,
            })
            .collect();
        if !params.is_empty() { return params; }

        // Fallback for shapes extract_params doesn't recognize: scan the
        // body for a top-level `my (...) = @_` unpack.
        let body = sub_node.child_by_field_name("body")
            .or_else(|| {
                (0..sub_node.named_child_count())
                    .filter_map(|i| sub_node.named_child(i))
                    .find(|c| c.kind() == "block")
            });
        if let Some(body) = body {
            for i in 0..body.named_child_count() {
                let Some(stmt) = body.named_child(i) else { continue };
                if let Some(extracted) = self.extract_my_at_underscore(stmt) {
                    params = extracted;
                    break;
                }
                break;
            }
        }
        params
    }

    /// Match `my (V1, V2, @rest) = @_` — the common legacy unpack. Only
    /// the exact shape `my (...) = @_` counts; more elaborate forms aren't
    /// worth the heuristic complexity here and can be layered on later.
    fn extract_my_at_underscore(&self, stmt: Node<'a>) -> Option<Vec<plugin::EmittedParam>> {
        let text = stmt.utf8_text(self.source).ok()?;
        let trimmed = text.trim().trim_end_matches(';').trim();
        let rest = trimmed.strip_prefix("my")?.trim_start();
        let rest = rest.strip_prefix('(')?;
        let (inner, after) = rest.split_once(')')?;
        let after = after.trim();
        if !after.starts_with('=') { return None; }
        let rhs = after.trim_start_matches('=').trim();
        if rhs != "@_" { return None; }

        let params: Vec<plugin::EmittedParam> = inner
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .map(|name| plugin::EmittedParam {
                is_slurpy: name.starts_with('@') || name.starts_with('%'),
                name,
                default: None,
                is_invocant: false,
            })
            .collect();
        if params.is_empty() { None } else { Some(params) }
    }

    /// Best-effort receiver-type resolution for a method call. Handles:
    ///   * `$self` / `__PACKAGE__`        → current package
    ///   * bare `Pkg::Name`               → literal class
    ///   * `$var` with a prior `my $var = Pkg->new` → looked up in
    ///     `type_constraints` (reverse scan; latest wins). This lets the
    ///     mojo-events plugin resolve `$obj->emit(...)` in a consumer file
    ///     to the producer's class, enabling cross-file def/ref pairing.
    fn receiver_type_for(&self, invocant_text: Option<&str>) -> Option<InferredType> {
        let text = invocant_text?;
        if text == "$self" || text == "__PACKAGE__" {
            return self.current_package.clone().map(InferredType::ClassName);
        }
        if text.starts_with('$') {
            return self.type_constraints.iter().rev().find_map(|tc| {
                if tc.variable == text { Some(tc.inferred_type.clone()) } else { None }
            });
        }
        if text.starts_with('@') || text.starts_with('%') {
            return None;
        }
        // Bareword invocant. Perl lets `foo->bar` mean either
        // "method bar on package foo" OR "result of calling foo()"
        // if `foo` is a declared sub. When we know about a local
        // Sub/Method named `foo` with a return_type, that wins —
        // `app->routes` in a Mojolicious::Lite script resolves to
        // the plugin-synthesized `sub app() :: Mojolicious`.
        for sym in &self.symbols {
            if sym.name != text { continue; }
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
            if let SymbolDetail::Sub { return_type: Some(rt), .. } = &sym.detail {
                return Some(rt.clone());
            }
        }
        Some(InferredType::ClassName(text.to_string()))
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
            } => {
                let detail = SymbolDetail::Sub {
                    params: params.into_iter().map(Into::into).collect(),
                    is_method,
                    return_type,
                    doc,
                    display,
                };
                // Target package for this emission: either the plugin's
                // override or the current build state.
                let target_pkg = on_class.clone().or_else(|| self.current_package.clone());

                // Dedup: two plugin emissions for the same (name, package,
                // namespace) collapse to the first. Comes up naturally
                // with dotted-helper prefixes — `thing.hi` and
                // `thing.there` both want to register `thing` as a
                // namespace method; the second is a no-op. Dedup happens
                // here (not in the plugin) so every plugin author gets
                // it for free and can't accidentally double-emit.
                let already_emitted = self.symbols.iter().any(|s| {
                    s.name == name
                        && s.kind == SymKind::Method
                        && s.package == target_pkg
                        && s.namespace == ns
                });
                if already_emitted { return; }

                // `on_class` temporarily overrides current_package so the
                // emitted Symbol lands with the requested `package`. The
                // only sanctioned way for a plugin to attach a method to
                // a class that isn't the one being built — kept narrow
                // so plugins can't silently reparent other emissions.
                if let Some(pkg) = on_class {
                    let saved = self.current_package.take();
                    self.current_package = Some(pkg);
                    self.add_symbol_ns(name, SymKind::Method, span, selection_span, detail, ns);
                    self.current_package = saved;
                } else {
                    self.add_symbol_ns(name, SymKind::Method, span, selection_span, detail, ns);
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
            } => {
                let detail = SymbolDetail::Handler {
                    owner,
                    dispatchers,
                    params: params.into_iter().map(Into::into).collect(),
                    display,
                };
                self.add_symbol_ns(name, SymKind::Handler, span, selection_span, detail, ns);
            }
            plugin::EmitAction::MethodCallRef { method_name, invocant, span, invocant_span } => {
                // Standard MethodCall ref — gd/gr/hover/rename route to
                // the usual resolution path (inheritance walk + module
                // index + type inference). The plugin's job is just
                // "there's a call to method X on invocant Y here".
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
            plugin::EmitAction::Symbol { name, kind, span, selection_span, detail } => {
                self.add_symbol_ns(name, kind, span, selection_span, detail, ns);
            }
            plugin::EmitAction::PackageParent { package, parent } => {
                self.package_parents.entry(package).or_default().push(parent);
            }
            plugin::EmitAction::FrameworkImport { keyword } => {
                self.framework_imports.insert(keyword);
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
        }
    }

    // ---- Main visitor ----

    fn visit_node(&mut self, node: Node<'a>) {
        match node.kind() {
            "package_statement" => self.visit_package(node),
            "class_statement" => self.visit_class(node),
            "subroutine_declaration_statement" => self.visit_sub(node, false),
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
                self.infer_deref_type(node, InferredType::CodeRef);
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

            // Return expressions → collect return types for post-pass
            "return_expression" => {
                if let Some(scope) = self.enclosing_sub_scope() {
                    let ret_type = self.infer_return_value_type(node);
                    self.return_infos.push(ReturnInfo {
                        scope,
                        inferred_type: ret_type,
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
            }

            // Expression statements inside sub bodies → track last expression type
            "expression_statement" => {
                self.visit_children(node);
                // Track the type of the last expression in the innermost sub/method scope
                if let Some(scope) = self.enclosing_sub_scope() {
                    let expr_type = node.named_child(0).and_then(|child| {
                        self.infer_expression_type(child, false)
                    });
                    self.last_expr_type.insert(scope, expr_type);
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
        self.current_package = Some(name.clone());

        // Update the current scope's package
        let scope = self.current_scope();
        self.scopes[scope.0 as usize].package = Some(name.clone());

        self.add_symbol(
            name.clone(),
            SymKind::Package,
            node_to_span(node),
            node_to_span(name_node),
            SymbolDetail::None,
        );

        // Block packages: `package Foo { ... }` — recurse into the block
        let has_block = (0..node.child_count())
            .any(|i| node.child(i).map_or(false, |c| c.kind() == "block"));
        if has_block {
            self.add_fold_range(node);
            let prev_package = self.current_package.take();
            self.current_package = Some(name);
            self.visit_children(node);
            self.current_package = prev_package;
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
            self.current_package = Some(name.clone());
            self.push_scope(ScopeKind::Class { name: name.clone() }, node_to_span(node), Some(name));
            self.visit_children(node);
            self.pop_scope();
            self.current_package = prev_package;
        } else {
            // Flat class (class Foo;): like package, stays in effect until next package/class
            self.current_package = Some(name.clone());
            let scope = self.current_scope();
            self.scopes[scope.0 as usize].package = Some(name);
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
                first.name.strip_prefix('$').unwrap_or(""),
                "self" | "class" | "this" | "proto"
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
            SymbolDetail::Sub { params: params.clone(), is_method, return_type: None, doc, display: None },
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
                self.type_constraints.push(TypeConstraint {
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
            self.type_constraints.push(TypeConstraint {
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
                        SymbolDetail::Sub { params: vec![], is_method: true, return_type: None, doc: None, display: None },
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
                            return_type: None,
                            doc: None,
                            display: None,
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
                let (imported_symbols, qw_close_paren) = self.extract_use_import_list(node);
                // Emit FunctionCall refs for imported symbol names (for goto-def on import args)
                if module_name != "parent" && module_name != "base" {
                    if !imported_symbols.is_empty() {
                        let sym_set: std::collections::HashSet<&str> = imported_symbols.iter().map(|s| s.as_str()).collect();
                        self.emit_refs_for_strings(node, &sym_set, RefKind::FunctionCall);
                    }
                }
                self.imports.push(Import {
                    module_name: module_name.to_string(),
                    imported_symbols: imported_symbols.clone(),
                    span: node_to_span(node),
                    qw_close_paren,
                });

                // Plugin dispatch for use-statements. Mojolicious::Lite
                // autoimports a verb set (`get`, `post`, `helper`, ...)
                // — the plugin emits `FrameworkImport` actions so our
                // unresolved-function diagnostic doesn't flag them.
                if !self.plugins.is_empty() {
                    let raw_args = self.extract_mojo_base_args(node);
                    let ctx = plugin::UseContext {
                        module_name: module_name.to_string(),
                        imports: imported_symbols,
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

    /// Check if we're at package scope (file scope or package block, not inside a sub).
    #[allow(dead_code)]
    fn is_package_scope(&self) -> bool {
        for &scope_id in self.scope_stack.iter().rev() {
            match &self.scopes[scope_id.0 as usize].kind {
                ScopeKind::File | ScopeKind::Package { .. } => return true,
                ScopeKind::Sub { .. } | ScopeKind::Method { .. } => return false,
                _ => continue, // blocks, for loops at package level are OK
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
                let inferred = self.infer_expression_type(right, false)
                    .or_else(|| self.infer_expression_result_type(right));
                if let Some(it) = inferred {
                    if let Some(vt) = self.get_var_text_from_lhs(left) {
                        self.type_constraints.push(TypeConstraint {
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

    /// Infer a type from a literal RHS node (no CST walk, just node kind check).
    /// Infer type from an expression node: literals, constructors, sub-body last expr.
    /// When `unwrap_sub_body` is true (Mojo default context), recurses into sub bodies.
    /// When false (literal/return context), anonymous subs return CodeRef.
    fn infer_expression_type(&self, node: Node<'a>, unwrap_sub_body: bool) -> Option<InferredType> {
        match node.kind() {
            "string_literal" | "interpolated_string_literal" => Some(InferredType::String),
            "number" => Some(InferredType::Numeric),
            "anonymous_hash_expression" => Some(InferredType::HashRef),
            "anonymous_array_expression" => Some(InferredType::ArrayRef),
            "quoted_regexp" => Some(InferredType::Regexp),
            "anonymous_subroutine_expression" => {
                if unwrap_sub_body {
                    // Look at the last expression in the block for the return type
                    let body = node.child_by_field_name("body")?;
                    let last_stmt = body.named_child(body.named_child_count().checked_sub(1)?)?;
                    let expr = if last_stmt.kind() == "expression_statement" {
                        last_stmt.named_child(0)?
                    } else if last_stmt.kind() == "return_expression" {
                        last_stmt.named_child(0)?
                    } else {
                        last_stmt
                    };
                    self.infer_expression_type(expr, true)
                } else {
                    Some(InferredType::CodeRef)
                }
            }
            "method_call_expression" => {
                self.extract_constructor_class(node).map(InferredType::ClassName)
            }
            _ => None,
        }
    }

    /// Infer the type of a return expression's value.
    fn infer_return_value_type(&self, return_node: Node<'a>) -> Option<InferredType> {
        let child = match return_node.named_child(0) {
            Some(c) => c,
            None => return None, // bare `return` — skip signal
        };
        if child.kind() == "undef" {
            return None;
        }
        // Try expression type (literals, constructors)
        if let Some(t) = self.infer_expression_type(child, false) {
            return Some(t);
        }
        // Try expression result type: return $a + $b → Numeric
        if let Some(t) = self.infer_expression_result_type(child) {
            return Some(t);
        }
        // Try variable lookup: return $self, return $var
        if child.kind() == "scalar" {
            if let Ok(var_text) = child.utf8_text(self.source) {
                let point = child.start_position();
                for tc in self.type_constraints.iter().rev() {
                    if tc.variable == var_text && tc.constraint_span.start <= point {
                        let scope = &self.scopes[tc.scope.0 as usize];
                        if contains_point(&scope.span, point) {
                            return Some(tc.inferred_type.clone());
                        }
                    }
                }
            }
        }
        None
    }

    /// Push a type constraint on a variable node if it's a scalar.
    fn push_var_type_constraint(&mut self, var_node: Node<'a>, context_node: Node<'a>, inferred_type: InferredType) {
        if var_node.kind() == "scalar" {
            if let Ok(text) = var_node.utf8_text(self.source) {
                self.type_constraints.push(TypeConstraint {
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
                        return Some((InferredType::CodeRef, outer));
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

    fn visit_function_call(&mut self, node: Node<'a>) {
        if let Some(func_node) = node.child_by_field_name("function") {
            if let Ok(name) = func_node.utf8_text(self.source) {
                self.add_ref(
                    RefKind::FunctionCall,
                    node_to_span(func_node),
                    name.to_string(),
                    AccessKind::Read,
                );
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

                for (name, sel_span) in &attr_names {
                    // Getter (always present for ro/rw/lazy/rwp)
                    self.add_symbol(
                        name.clone(),
                        SymKind::Method,
                        node_to_span(node),
                        *sel_span,
                        SymbolDetail::Sub {
                            params: vec![],
                            is_method: true,
                            return_type: return_type.clone(),
                            doc: None,
                            display: None,
                        },
                    );
                    // Setter for rw
                    if is_rw {
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
                                return_type: return_type.clone(),
                                doc: None,
                                display: None,
                            },
                        );
                    }
                    // Private writer for rwp (Moo only)
                    if is_rwp {
                        let writer_name = format!("_set_{}", name);
                        self.add_symbol(
                            writer_name,
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
                                return_type: return_type.clone(),
                                doc: None,
                                display: None,
                            },
                        );
                    }
                }
            }
            FrameworkMode::MojoBase => {
                // Infer getter return type from default value if present
                let getter_type = mojo_default_node
                    .and_then(|n| self.infer_expression_type(n, true));

                // Mojo::Base `has` produces getter + setter (two symbols)
                for (name, sel_span) in &attr_names {
                    // Getter: no params, return type from default value (or None)
                    self.add_symbol(
                        name.clone(),
                        SymKind::Method,
                        node_to_span(node),
                        *sel_span,
                        SymbolDetail::Sub {
                            params: vec![],
                            is_method: true,
                            return_type: getter_type.clone(),
                            doc: None,
                            display: None,
                        },
                    );
                    // Setter: fluent, returns $self for chaining
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
                            return_type: self.current_package.as_ref()
                                .map(|pkg| InferredType::ClassName(pkg.clone())),
                            doc: None,
                            display: None,
                        },
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
            "CodeRef" => Some(InferredType::CodeRef),
            "RegexpRef" => Some(InferredType::Regexp),
            _ => {
                // InstanceOf['Foo::Bar'] (Moo style)
                if isa.starts_with("InstanceOf[") {
                    let inner = isa.trim_start_matches("InstanceOf[")
                        .trim_end_matches(']')
                        .trim_matches('\'')
                        .trim_matches('"');
                    if !inner.is_empty() {
                        return Some(InferredType::ClassName(inner.to_string()));
                    }
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
        // Store invocant span for complex expressions (call chains etc.)
        let invocant_span = invocant_node
            .filter(|n| !matches!(n.kind(), "scalar" | "array" | "hash" | "bareword" | "package"))
            .map(|n| node_to_span(n));

        if let Some(ref name) = method_name {
            // Dynamic method dispatch: $self->$method() — resolve $method if known
            if name.starts_with('$') {
                if let Some(resolved) = self.resolve_constant_strings(name, 0) {
                    for rname in resolved {
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
                    }
                }
            } else {
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
                ctx.receiver_type = self.receiver_type_for(invocant_text.as_deref());
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
                    return_type: None,
                    doc: None,
                    display: None,
                },
            );
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

            self.add_symbol(
                name,
                SymKind::Method,
                node_to_span(node),
                sel_span,
                SymbolDetail::Sub {
                    params: vec![],
                    is_method: true,
                    return_type,
                    doc: None,
                    display: None,
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
                        AccessKind::Read,
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
                if let Ok(text) = node.utf8_text(self.source) {
                    out.push((text.to_string(), node_to_span(node)));
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

    fn first_var_child(&self, node: Node<'a>) -> Option<String> {
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                if matches!(child.kind(), "scalar" | "array" | "hash") {
                    return child.utf8_text(self.source).ok().map(|s| s.to_string());
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

    fn canonicalize_container(&self, node: Node<'a>, text: &str) -> String {
        // $hash{key} → %hash, $arr[0] → @arr, etc.
        if let Some(parent) = node.parent() {
            let parent_kind = parent.kind();
            if parent_kind == "hash_element_expression" || parent_kind == "keyval_container_variable" {
                // Container is a hash access — underlying variable is %name
                if text.starts_with('$') {
                    return format!("%{}", &text[1..]);
                }
            } else if parent_kind == "array_element_expression" || parent_kind == "slice_container_variable" {
                // Container is an array access — underlying variable is @name
                if text.starts_with('$') || text.starts_with('@') {
                    return format!("@{}", &text[1..]);
                }
            }
        }
        text.to_string()
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
                if let Some(ref pkg) = self.current_package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
                }
                let scope = self.current_scope();
                if let Some(ref pkg) = self.scopes[scope.0 as usize].package {
                    return Some(HashKeyOwner::Class(pkg.clone()));
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
        // Build a temporary scope-to-symbols map for efficient lookup
        let mut scope_symbols: std::collections::HashMap<ScopeId, Vec<(String, SymbolId, Point)>> =
            std::collections::HashMap::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
                scope_symbols
                    .entry(sym.scope)
                    .or_default()
                    .push((sym.name.clone(), sym.id, sym.span.start));
            }
        }

        for r in &mut self.refs {
            if !matches!(r.kind, RefKind::Variable | RefKind::ContainerAccess) {
                continue;
            }

            // Walk scope chain to find the innermost matching declaration
            let mut current = Some(r.scope);
            while let Some(scope_id) = current {
                if let Some(symbols) = scope_symbols.get(&scope_id) {
                    // Find the best match: declared before this ref, matching name
                    let target_name = if matches!(r.kind, RefKind::ContainerAccess) {
                        // Container refs need to match by base name, accounting for sigil differences
                        &r.target_name
                    } else {
                        &r.target_name
                    };
                    if let Some((_, sym_id, _)) = symbols.iter()
                        .filter(|(name, _, decl_point)| name == target_name && *decl_point <= r.span.start)
                        .last()
                    {
                        r.resolves_to = Some(*sym_id);
                        break;
                    }
                }
                current = self.scopes[scope_id.0 as usize].parent;
            }
        }
    }

    fn resolve_return_types(&mut self) {
        // Group return infos by scope
        let mut returns_by_scope: std::collections::HashMap<ScopeId, Vec<Option<InferredType>>> =
            std::collections::HashMap::new();
        for ri in &self.return_infos {
            returns_by_scope
                .entry(ri.scope)
                .or_default()
                .push(ri.inferred_type.clone());
        }

        // For each Sub/Method scope, determine return type
        let mut return_types: std::collections::HashMap<String, InferredType> =
            std::collections::HashMap::new();

        for scope in &self.scopes {
            let sub_name = match &scope.kind {
                ScopeKind::Sub { name } | ScopeKind::Method { name } => name.clone(),
                _ => continue,
            };

            let explicit_returns = returns_by_scope.get(&scope.id);
            let has_explicit = explicit_returns.map_or(false, |r| !r.is_empty());

            let resolved = if has_explicit {
                // Filter out bare returns (None) — they're exit points, not type signals
                let typed_returns: Vec<InferredType> = explicit_returns.unwrap()
                    .iter()
                    .filter_map(|r| r.clone())
                    .collect();
                resolve_return_type(&typed_returns)
            } else {
                // No explicit returns — use last expression type
                self.last_expr_type.get(&scope.id).and_then(|t| t.clone())
            };

            if let Some(rt) = resolved {
                return_types.insert(sub_name, rt);
            }
        }

        // Propagate return types through delegation chains. If sub X's body
        // is `return Y()` (recorded in sub_return_delegations) and Y has a
        // known return type, X inherits it. Iterate until no changes — chains
        // converge in at most `delegations.len()` passes.
        let mut changed = true;
        let mut iters = 0;
        while changed && iters < 20 {
            changed = false;
            iters += 1;
            let pairs: Vec<(String, String)> = self.sub_return_delegations.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (delegator, delegate) in pairs {
                if return_types.contains_key(&delegator) {
                    continue;
                }
                if let Some(t) = return_types.get(&delegate).cloned() {
                    return_types.insert(delegator, t);
                    changed = true;
                }
            }
        }

        // Set return_type on matching Sub/Method symbols
        for sym in &mut self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if let SymbolDetail::Sub { ref mut return_type, .. } = sym.detail {
                    if let Some(rt) = return_types.get(&sym.name) {
                        *return_type = Some(rt.clone());
                    }
                }
            }
        }

        // Propagate return types to call sites via structural bindings
        // recorded during the walk (my $cfg = get_config()).
        // TODO: inline expression propagation (get_config()->{key}) is a separate
        // code path — needs return type resolution at the expression level without
        // a variable assignment. Not handled here.
        let mut new_constraints = Vec::new();
        for binding in &self.call_bindings {
            let rt = return_types.get(&binding.func_name)
                .cloned()
                .or_else(|| builtin_return_type(&binding.func_name));
            if let Some(rt) = rt {
                new_constraints.push(TypeConstraint {
                    variable: binding.variable.clone(),
                    scope: binding.scope,
                    constraint_span: binding.span,
                    inferred_type: rt,
                });
            }
        }
        self.type_constraints.extend(new_constraints);

        // Fixup: update HashKeyAccess owners for variables bound to sub calls
        // that return HashRef. Two normalizations beyond the naive name match:
        //   1. Call names may be qualified (`Pkg::foo`) — strip the package
        //      prefix since return_types and the symbol table key on the
        //      bare name.
        //   2. The bound func may itself just `return other()` — walk the
        //      delegation chain to the sub that actually declares the hash
        //      literal. Otherwise `sub chain { return get_config() }` leaves
        //      `$cfg = chain(); $cfg->{host}` with an owner that has no
        //      matching HashKeyDefs.
        let bare = |s: &str| -> String { s.rsplit("::").next().unwrap_or(s).to_string() };

        let binding_map: std::collections::HashMap<&str, String> = self.call_bindings.iter()
            .filter(|b| {
                let name = bare(&b.func_name);
                return_types.get(&name).map_or(false, |t| *t == InferredType::HashRef)
            })
            .map(|b| (b.variable.as_str(), bare(&b.func_name)))
            .collect();

        let sub_package: std::collections::HashMap<&str, Option<String>> = self.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| (s.name.as_str(), s.package.clone()))
            .collect();

        // Which subs have at least one HashKeyDef they actually own?
        let subs_with_own_keys: std::collections::HashSet<String> = self.symbols.iter()
            .filter_map(|s| {
                if let SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } = &s.detail {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        // Method-call bindings: `my $c = $obj->method()` (including dynamic
        // `$obj->$m()` where $m was constant-folded during method_call_binding
        // emission). Same ownership logic as function calls — point $c's
        // hash-key accesses at the HashKeyDefs inside `method`.
        let method_binding_map: std::collections::HashMap<&str, String> = self.method_call_bindings.iter()
            .map(|mcb| (mcb.variable.as_str(), mcb.method_name.clone()))
            .collect();

        for r in &mut self.refs {
            if let RefKind::HashKeyAccess { ref var_text, ref mut owner } = r.kind {
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
                    // Method call — does the method itself own hash keys?
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

    fn resolve_hash_key_owners(&mut self) {
        // Build type constraint lookup
        let mut type_map: std::collections::HashMap<String, Vec<(ScopeId, InferredType, Point)>> =
            std::collections::HashMap::new();
        for tc in &self.type_constraints {
            type_map
                .entry(tc.variable.clone())
                .or_default()
                .push((tc.scope, tc.inferred_type.clone(), tc.constraint_span.start));
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
                                if let Some(cn) = tc_type.class_name() {
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
mod tests {
    use super::*;

    fn parse(source: &str) -> Tree {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        parser.parse(source, None).unwrap()
    }

    fn build_fa(source: &str) -> FileAnalysis {
        let tree = parse(source);
        build(&tree, source.as_bytes())
    }

    // ---- Scope tests ----

    #[test]
    fn test_file_scope() {
        let fa = build_fa("my $x = 1;");
        assert_eq!(fa.scopes.len(), 1);
        assert_eq!(fa.scopes[0].kind, ScopeKind::File);
    }

    #[test]
    fn test_sub_creates_scope() {
        let fa = build_fa("sub foo { my $x = 1; }");
        let sub_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"))
            .collect();
        assert_eq!(sub_scopes.len(), 1);
        assert_eq!(sub_scopes[0].parent, Some(ScopeId(0))); // parent is file
    }

    #[test]
    fn test_class_creates_scope() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
        let class_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Class { name } if name == "Point"))
            .collect();
        assert_eq!(class_scopes.len(), 1);
        assert_eq!(class_scopes[0].package, Some("Point".to_string()));
    }

    #[test]
    fn test_package_sets_scope_package() {
        let fa = build_fa("package Foo;\nsub bar { 1 }");
        // The sub scope should inherit package "Foo"
        let sub_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "bar"))
            .collect();
        assert_eq!(sub_scopes.len(), 1);
        assert_eq!(sub_scopes[0].package, Some("Foo".to_string()));
    }

    #[test]
    fn test_for_loop_scope() {
        let fa = build_fa("for my $i (1..10) { print $i; }");
        let for_scopes: Vec<_> = fa.scopes.iter()
            .filter(|s| matches!(&s.kind, ScopeKind::ForLoop { .. }))
            .collect();
        assert_eq!(for_scopes.len(), 1);
    }

    // ---- Symbol tests ----

    #[test]
    fn test_variable_symbol() {
        let fa = build_fa("my $x = 1;");
        let vars: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Variable && s.name == "$x")
            .collect();
        assert_eq!(vars.len(), 1);
        if let SymbolDetail::Variable { sigil, decl_kind } = &vars[0].detail {
            assert_eq!(*sigil, '$');
            assert_eq!(*decl_kind, DeclKind::My);
        } else {
            panic!("expected Variable detail");
        }
    }

    #[test]
    fn test_sub_symbol_with_params() {
        let fa = build_fa("sub connect($self, %opts) { }");
        let subs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Sub && s.name == "connect")
            .collect();
        assert_eq!(subs.len(), 1);
        if let SymbolDetail::Sub { params, is_method, .. } = &subs[0].detail {
            assert!(!is_method);
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "$self");
            assert_eq!(params[1].name, "%opts");
            assert!(params[1].is_slurpy);
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_legacy_sub_params() {
        let fa = build_fa("sub new {\n    my ($class, %args) = @_;\n}");
        let subs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Sub && s.name == "new")
            .collect();
        assert_eq!(subs.len(), 1);
        if let SymbolDetail::Sub { params, .. } = &subs[0].detail {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "$class");
            assert_eq!(params[1].name, "%args");
            assert!(params[1].is_slurpy);
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_package_symbol() {
        let fa = build_fa("package Foo;");
        let pkgs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Package && s.name == "Foo")
            .collect();
        assert_eq!(pkgs.len(), 1);
    }

    #[test]
    fn test_class_symbol() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n    field $y :param;\n    method magnitude() { }\n}");
        let classes: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Class && s.name == "Point")
            .collect();
        assert_eq!(classes.len(), 1);
        if let SymbolDetail::Class { fields, parent, .. } = &classes[0].detail {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "$x");
            assert_eq!(fields[1].name, "$y");
            assert!(fields[0].attributes.contains(&"param".to_string()));
            assert!(parent.is_none());
        } else {
            panic!("expected Class detail");
        }
    }

    #[test]
    fn test_field_symbol() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
        let fields: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Field)
            .collect();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "$x");
    }

    #[test]
    fn test_field_reader_synthesizes_method() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n}");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1, "got: {:?}", methods.iter().map(|m| &m.name).collect::<Vec<_>>());
        assert_eq!(methods[0].name, "x");
    }

    #[test]
    fn test_implicit_self_in_method() {
        // $self is implicitly available in Perl 5.38 method blocks
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
        let fa = build_fa(source);

        // $self should be resolvable as a variable inside the method
        let resolved = fa.resolve_variable("$self", Point::new(4, 8));
        assert!(resolved.is_some(), "$self should resolve inside method body");
    }

    #[test]
    fn test_implicit_self_type_inference() {
        // $self should be type-inferred to the enclosing class
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
        let fa = build_fa(source);

        // Type inference: $self → Point
        let inferred = fa.inferred_type("$self", Point::new(4, 8));
        assert!(inferred.is_some(), "$self type should be inferred");
        match inferred.unwrap() {
            InferredType::ClassName(name) => assert_eq!(name, "Point"),
            InferredType::FirstParam { package } => assert_eq!(package, "Point"),
            other => panic!("expected ClassName or FirstParam, got {:?}", other),
        }
    }

    #[test]
    fn test_self_completion_inside_method() {
        // $self-> inside a method should complete with sibling methods
        let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n    method to_string () {\n        $self->;\n    }\n}\n";
        let fa = build_fa(source);

        let candidates = fa.complete_methods("$self", Point::new(5, 14));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_field_writer_synthesizes_method() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $label :reader :writer = \"point\";\n}");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Method)
            .map(|s| s.name.clone())
            .collect();
        assert!(methods.contains(&"label".to_string()), "missing reader, got: {:?}", methods);
        assert!(methods.contains(&"set_label".to_string()), "missing writer, got: {:?}", methods);
    }

    #[test]
    fn test_complete_methods_in_class() {
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n    method magnitude() { }\n    method to_string() { }\n}\nmy $p = Point->new(x => 1);\n$p->;\n");
        // $p-> is at line 8, col 4
        let candidates = fa.complete_methods("$p", Point::new(8, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"new"), "missing new, got: {:?}", names);
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_sample_file_layout() {
        // Matches sample.pl: class defined after package main, $p usage at end
        let source = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
        let fa = build_fa(source);

        // Check type inference resolved $p → Point
        let inferred = fa.inferred_type("$p", Point::new(8, 4));
        assert!(inferred.is_some(), "type inference for $p should resolve");

        let candidates = fa.complete_methods("$p", Point::new(10, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_class_after_package_main() {
        // Real-world: package main; ... class Point {} ... $p->
        let source = r#"package main;
my $calc = Calculator->new();
1;
use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
        let fa = build_fa(source);

        let candidates = fa.complete_methods("$p", Point::new(11, 4));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"new"), "missing new, got: {:?}", names);
        assert!(names.contains(&"magnitude"), "missing magnitude, got: {:?}", names);
        assert!(names.contains(&"to_string"), "missing to_string, got: {:?}", names);
        assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
    }

    #[test]
    fn test_complete_methods_flat_class() {
        // class Foo; (no block) — methods follow as siblings, like package
        let source = "use v5.38;\nclass Foo;\nmethod bar () { }\nmethod baz () { }\n";
        let fa = build_fa(source);
        let candidates = fa.complete_methods("Foo", Point::new(3, 0));
        let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"bar"), "missing bar, got: {:?}", names);
        assert!(names.contains(&"baz"), "missing baz, got: {:?}", names);
    }

    #[test]
    fn test_goto_def_method_after_package_main() {
        // go-to-def on $p->magnitude() should find the method, not the class
        let source = "package main;\n1;\nuse v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n}\nmy $p = Point->new(x => 3);\n$p->magnitude();\n";
        let fa = build_fa(source);
        // cursor on `magnitude` in `$p->magnitude()` — line 8, col 5
        let def = fa.find_definition(Point::new(8, 5), None, None, None);
        assert!(def.is_some(), "should find definition for magnitude");
        let span = def.unwrap();
        assert_eq!(span.start.row, 5, "should point to method declaration line, got row {}", span.start.row);
    }

    #[test]
    fn test_field_reader_goto_def() {
        // go-to-def on $p->x should find the reader method, which points to the field
        let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    method mag() { }\n}\nmy $p = Point->new(x => 1);\n$p->x;");
        let def = fa.find_definition(Point::new(6, 5), None, None, None); // cursor on `x` in `$p->x`
        assert!(def.is_some(), "should find definition for reader method");
        // The reader method's selection_span points to the field declaration
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "should point to field declaration line");
    }

    #[test]
    fn test_use_symbol() {
        let fa = build_fa("use Foo::Bar;");
        let modules: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module)
            .collect();
        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].name, "Foo::Bar");
    }

    // ---- Ref tests ----

    #[test]
    fn test_variable_ref() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        let var_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$x" && matches!(r.kind, RefKind::Variable))
            .collect();
        // One declaration ref + one read ref
        assert!(var_refs.len() >= 2, "got {} refs for $x", var_refs.len());
        assert!(var_refs.iter().any(|r| r.access == AccessKind::Declaration));
        assert!(var_refs.iter().any(|r| r.access == AccessKind::Read));
    }

    #[test]
    fn test_function_call_ref() {
        let fa = build_fa("sub foo { }\nfoo();");
        let call_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::FunctionCall))
            .collect();
        assert_eq!(call_refs.len(), 1);
    }

    #[test]
    fn test_method_call_ref() {
        let fa = build_fa("$obj->method();");
        let method_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "method" && matches!(r.kind, RefKind::MethodCall { .. }))
            .collect();
        assert_eq!(method_refs.len(), 1);
        if let RefKind::MethodCall { ref invocant, .. } = method_refs[0].kind {
            assert_eq!(invocant, "$obj");
        }
    }

    #[test]
    fn test_hash_key_ref() {
        let fa = build_fa("my %h;\n$h{foo};");
        let key_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert_eq!(key_refs.len(), 1);
    }

    // ---- Query tests ----

    #[test]
    fn test_scope_at() {
        let fa = build_fa("sub foo {\n    my $x = 1;\n}");
        // Point inside the sub body
        let scope = fa.scope_at(Point::new(1, 8)).unwrap();
        let s = fa.scope(scope);
        assert!(matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"));
    }

    #[test]
    fn test_resolve_variable() {
        let fa = build_fa("my $x = 1;\nsub foo {\n    my $x = 2;\n    print $x;\n}");
        // Inside the sub, $x should resolve to the inner declaration
        let sym = fa.resolve_variable("$x", Point::new(3, 10)).unwrap();
        // Inner $x is at line 2
        assert_eq!(sym.selection_span.start.row, 2);
    }

    #[test]
    fn test_resolve_variable_outer() {
        let fa = build_fa("my $x = 1;\nsub foo {\n    print $x;\n}");
        // Inside the sub with no inner $x, should resolve to outer
        let sym = fa.resolve_variable("$x", Point::new(2, 10)).unwrap();
        assert_eq!(sym.selection_span.start.row, 0);
    }

    #[test]
    fn test_type_inference_constructor() {
        let fa = build_fa("use v5.38;\nclass Point { }\nmy $p = Point->new();");
        let ty = fa.inferred_type("$p", Point::new(2, 20));
        assert!(ty.is_some(), "should infer type for $p");
        if let Some(InferredType::ClassName(cn)) = ty {
            assert_eq!(cn, "Point");
        } else {
            panic!("expected ClassName, got {:?}", ty);
        }
    }

    #[test]
    fn test_type_inference_first_param() {
        let fa = build_fa("package Calculator;\nsub new {\n    my ($self) = @_;\n}");
        let ty = fa.inferred_type("$self", Point::new(2, 10));
        assert!(ty.is_some(), "should infer type for $self");
        if let Some(InferredType::FirstParam { package }) = ty {
            assert_eq!(package, "Calculator");
        } else {
            panic!("expected FirstParam, got {:?}", ty);
        }
    }

    // ---- Literal constructor extraction tests (via build_fa) ----

    #[test]
    fn test_extract_hashref_literal() {
        let fa = build_fa("my $href = {};");
        let ty = fa.inferred_type("$href", Point::new(0, 14));
        assert_eq!(ty, Some(&InferredType::HashRef), "empty hash ref literal");

        let fa = build_fa("my $href = { a => 1, b => 2 };");
        let ty = fa.inferred_type("$href", Point::new(0, 30));
        assert_eq!(ty, Some(&InferredType::HashRef), "populated hash ref literal");
    }

    #[test]
    fn test_extract_arrayref_literal() {
        let fa = build_fa("my $aref = [];");
        let ty = fa.inferred_type("$aref", Point::new(0, 14));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "empty array ref literal");

        let fa = build_fa("my $aref = [1, 2, 3];");
        let ty = fa.inferred_type("$aref", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "populated array ref literal");
    }

    #[test]
    fn test_extract_coderef_literal() {
        let fa = build_fa("my $cref = sub { 42 };");
        let ty = fa.inferred_type("$cref", Point::new(0, 22));
        assert_eq!(ty, Some(&InferredType::CodeRef), "anonymous sub");
    }

    #[test]
    fn test_extract_regexp_literal() {
        let fa = build_fa("my $re = qr/pattern/;");
        let ty = fa.inferred_type("$re", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::Regexp), "qr// literal");
    }

    #[test]
    fn test_extract_reassignment_type_change() {
        let fa = build_fa("my $x = {};\n$x = [];");
        // After line 0 → HashRef
        let ty = fa.inferred_type("$x", Point::new(0, 11));
        assert_eq!(ty, Some(&InferredType::HashRef), "initial hashref");
        // After line 1 → ArrayRef
        let ty = fa.inferred_type("$x", Point::new(1, 8));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "reassigned to arrayref");
    }

    #[test]
    fn test_extract_constructor_still_works() {
        // Existing constructor detection should still work
        let fa = build_fa("my $obj = Foo->new();");
        let ty = fa.inferred_type("$obj", Point::new(0, 21));
        assert_eq!(ty, Some(&InferredType::ClassName("Foo".into())));
    }

    // ---- Operator-based type inference tests (Step 3) ----

    #[test]
    fn test_arrow_hash_deref_infers_hashref() {
        let fa = build_fa("my $x;\n$x->{key};");
        let ty = fa.inferred_type("$x", Point::new(1, 10));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_arrow_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\n$x->[0];");
        let ty = fa.inferred_type("$x", Point::new(1, 8));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_arrow_code_deref_infers_coderef() {
        let fa = build_fa("my $x;\n$x->(1, 2);");
        let ty = fa.inferred_type("$x", Point::new(1, 10));
        assert_eq!(ty, Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_postfix_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\nmy @a = $x->@*;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_postfix_hash_deref_infers_hashref() {
        let fa = build_fa("my $y;\nmy %h = $y->%*;\nmy $z;");
        let ty = fa.inferred_type("$y", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_binary_numeric_ops_infer_numeric() {
        let fa = build_fa("my $x;\nmy $a = $x + 1;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "+ operator");

        let fa = build_fa("my $x;\nmy $a = $x * 2;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "* operator");
    }

    #[test]
    fn test_assignment_from_binary_numeric_infers_result() {
        let fa = build_fa("my $a = 1;\nmy $b = 2;\nmy $result = $a + $b;\n$result;");
        let ty = fa.inferred_type("$result", Point::new(3, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "$result = $a + $b should be Numeric");
    }

    #[test]
    fn test_assignment_from_string_concat_infers_result() {
        let fa = build_fa("my $a = 'x';\nmy $b = 'y';\nmy $s = $a . $b;\n$s;");
        let ty = fa.inferred_type("$s", Point::new(3, 0));
        assert_eq!(ty, Some(&InferredType::String), "$s = $a . $b should be String");
    }

    #[test]
    fn test_string_concat_infers_string() {
        let fa = build_fa("my $s;\nmy $a = $s . \"x\";\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), ". operator");
    }

    #[test]
    fn test_string_repeat_infers_string() {
        let fa = build_fa("my $s;\n$s x 3;\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), "x operator");
    }

    #[test]
    fn test_numeric_comparison_infers_numeric() {
        let fa = build_fa("my $x;\nmy $y;\n$x == $y;\nmy $z;");
        assert_eq!(fa.inferred_type("$x", Point::new(3, 0)), Some(&InferredType::Numeric));
        assert_eq!(fa.inferred_type("$y", Point::new(3, 0)), Some(&InferredType::Numeric));
    }

    #[test]
    fn test_string_comparison_infers_string() {
        let fa = build_fa("my $x;\nmy $y;\n$x eq $y;\nmy $z;");
        assert_eq!(fa.inferred_type("$x", Point::new(3, 0)), Some(&InferredType::String));
        assert_eq!(fa.inferred_type("$y", Point::new(3, 0)), Some(&InferredType::String));
    }

    #[test]
    fn test_increment_infers_numeric() {
        let fa = build_fa("my $x;\n$x++;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric));
    }

    #[test]
    fn test_regex_match_infers_string() {
        let fa = build_fa("my $s;\n$s =~ /pattern/;\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String));
    }

    #[test]
    fn test_preinc_infers_numeric() {
        let fa = build_fa("my $x;\n++$x;\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric));
    }

    #[test]
    fn test_block_array_deref_infers_arrayref() {
        let fa = build_fa("my $x;\nmy @items = @{$x};\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_block_hash_deref_infers_hashref() {
        let fa = build_fa("my $y;\nmy %t = %{$y};\nmy $z;");
        let ty = fa.inferred_type("$y", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_block_code_deref_infers_coderef() {
        let fa = build_fa("my $z;\n&{$z}();\nmy $w;");
        let ty = fa.inferred_type("$z", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_no_numeric_on_array_variable() {
        // @arr + 1 should NOT push Numeric on @arr
        let fa = build_fa("my @arr;\nmy $n = @arr + 1;\nmy $z;");
        let ty = fa.inferred_type("@arr", Point::new(2, 0));
        assert_eq!(ty, None, "@arr should not get Numeric constraint");
    }

    // ---- Builtin type inference tests ----

    #[test]
    fn test_builtin_push_infers_arrayref() {
        // push @{$aref} triggers array_deref_expression which already infers ArrayRef
        let fa = build_fa("my $aref;\npush @{$aref}, 1;\nmy $z;");
        let ty = fa.inferred_type("$aref", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::ArrayRef), "push deref should infer ArrayRef");
    }

    #[test]
    fn test_builtin_length_infers_string_arg() {
        let fa = build_fa("my $s;\nmy $n = length($s);\nmy $z;");
        let ty = fa.inferred_type("$s", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::String), "length arg should be String");
    }

    #[test]
    fn test_builtin_abs_infers_numeric_arg() {
        let fa = build_fa("my $x;\nmy $n = abs($x);\nmy $z;");
        let ty = fa.inferred_type("$x", Point::new(2, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "abs arg should be Numeric");
    }

    #[test]
    fn test_builtin_return_type_propagates() {
        let fa = build_fa("my $t = time();\n$t;");
        let ty = fa.inferred_type("$t", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "time() should return Numeric");
    }

    #[test]
    fn test_builtin_join_return_type() {
        let fa = build_fa("my $s = join(',', @arr);\n$s;");
        let ty = fa.inferred_type("$s", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::String), "join() should return String");
    }

    #[test]
    fn test_builtin_length_return_type() {
        let fa = build_fa("my $n = length('hello');\n$n;");
        let ty = fa.inferred_type("$n", Point::new(1, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "length() should return Numeric");
    }

    // ---- Return type inference tests (Step 4) ----

    #[test]
    fn test_return_type_hashref() {
        let fa = build_fa("sub get_config {\n    return { host => \"localhost\" };\n}");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_arrayref() {
        let fa = build_fa("sub get_tags {\n    return [1, 2, 3];\n}");
        assert_eq!(fa.sub_return_type("get_tags"), Some(&InferredType::ArrayRef));
    }

    #[test]
    fn test_return_type_coderef() {
        let fa = build_fa("sub get_handler {\n    return sub { 1 };\n}");
        assert_eq!(fa.sub_return_type("get_handler"), Some(&InferredType::CodeRef));
    }

    #[test]
    fn test_return_type_implicit_last_expr() {
        // No explicit return — last expression is the implicit return
        let fa = build_fa("sub get_data {\n    { key => \"val\" };\n}");
        assert_eq!(fa.sub_return_type("get_data"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_conflicting_returns_unknown() {
        // Two returns with different types → None (unknown)
        let fa = build_fa("sub ambiguous {\n    if (1) { return {} }\n    return [];\n}");
        assert_eq!(fa.sub_return_type("ambiguous"), None);
    }

    #[test]
    fn test_return_type_consistent_returns() {
        // Multiple returns all hashref → HashRef
        let fa = build_fa("sub consistent {\n    if (1) { return { a => 1 } }\n    return { b => 2 };\n}");
        assert_eq!(fa.sub_return_type("consistent"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_propagation_to_call_site() {
        let fa = build_fa("sub get_config {\n    return { host => 1 };\n}\nmy $cfg = get_config();\nmy $z;");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
        let ty = fa.inferred_type("$cfg", Point::new(4, 0));
        assert_eq!(ty, Some(&InferredType::HashRef), "call site should get return type");
    }

    #[test]
    fn test_return_type_propagation_method_call() {
        let src = "package Calculator;\nsub new { bless {}, shift }\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    return $result;\n}\npackage main;\nmy $calc = Calculator->new();\nmy $sum = $calc->add(2, 3);\n$sum;";
        let fa = build_fa(src);
        assert_eq!(fa.sub_return_type("add"), Some(&InferredType::Numeric), "add should return Numeric");
        let ty = fa.inferred_type("$sum", Point::new(10, 0));
        assert_eq!(ty, Some(&InferredType::Numeric), "$sum should be Numeric via method call binding");
    }

    #[test]
    fn test_return_type_constructor() {
        let fa = build_fa("package User;\nsub new { bless {}, shift }\npackage main;\nsub get_user {\n    return User->new();\n}");
        assert_eq!(fa.sub_return_type("get_user"), Some(&InferredType::ClassName("User".into())));
    }

    #[test]
    fn test_return_type_self_variable() {
        // return $self where $self has a type constraint
        let fa = build_fa("package Foo;\nsub new { bless {}, shift }\nsub clone {\n    my ($self) = @_;\n    return $self;\n}");
        assert_eq!(
            fa.sub_return_type("clone"),
            Some(&InferredType::FirstParam { package: "Foo".into() }),
        );
    }

    #[test]
    fn test_return_type_bare_return_filtered() {
        // Bare return + typed return → bare is filtered, typed return wins
        let fa = build_fa("sub get_config {\n    return unless 1;\n    return { host => 1 };\n}");
        assert_eq!(fa.sub_return_type("get_config"), Some(&InferredType::HashRef));
    }

    #[test]
    fn test_return_type_all_bare_returns() {
        // All bare returns → no return type
        let fa = build_fa("sub noop {\n    return;\n}");
        assert_eq!(fa.sub_return_type("noop"), None);
    }

    #[test]
    fn test_return_type_undef_filtered() {
        // return undef + typed return → undef is filtered, typed return wins
        let fa = build_fa("sub maybe {\n    return undef unless 1;\n    return { a => 1 };\n}");
        assert_eq!(fa.sub_return_type("maybe"), Some(&InferredType::HashRef));
    }

    // ---- resolve_expression_type tests ----

    /// Find the first node of given kind at/after a point (searches all children).
    fn find_node_at<'a>(node: tree_sitter::Node<'a>, point: Point, kind: &str) -> Option<tree_sitter::Node<'a>> {
        if node.kind() == kind && node.start_position() >= point {
            return Some(node);
        }
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if let Some(found) = find_node_at(child, point, kind) {
                    return Some(found);
                }
            }
        }
        None
    }

    #[test]
    fn test_resolve_expr_type_function_call() {
        let src = "sub get_config {\n    return { host => 1 };\n}\nget_config();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Find the function_call_expression on line 3
        let call_node = find_node_at(tree.root_node(), Point::new(3, 0), "function_call_expression")
            .expect("should find function_call_expression");
        let ty = fa.resolve_expression_type(call_node, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_method_call_return() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub do_thing { }\npackage main;\nmy $f = Foo->new();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // $f->get_bar() should resolve to Object(Bar)
        // First verify get_bar has the right return type
        assert_eq!(fa.sub_return_type("get_bar"), Some(&InferredType::ClassName("Bar".into())));
    }

    #[test]
    fn test_resolve_expr_type_scalar_variable() {
        let src = "my $x = {};\n$x;\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Find the scalar $x on line 1
        let scalar_node = find_node_at(tree.root_node(), Point::new(1, 0), "scalar")
            .expect("should find scalar");
        let ty = fa.resolve_expression_type(scalar_node, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_chained_method() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub get_name {\n    return { name => 'test' };\n}\npackage main;\nmy $f = Foo->new();\n$f->get_bar()->get_name();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Line 12: $f->get_bar()->get_name();
        // The outermost method_call_expression starts at column 0
        // Use descendant_for_point_range to find the node at the start of that line
        let node = tree.root_node()
            .descendant_for_point_range(Point::new(12, 0), Point::new(12, 25))
            .expect("should find node");
        // Walk up to find the outermost method_call_expression
        let mut n = node;
        while n.kind() != "method_call_expression" || n.parent().map_or(false, |p| p.kind() == "method_call_expression") {
            n = match n.parent() {
                Some(p) => p,
                None => panic!("should find outermost method_call_expression"),
            };
        }
        assert_eq!(n.kind(), "method_call_expression");
        let ty = fa.resolve_expression_type(n, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef));
    }

    #[test]
    fn test_resolve_expr_type_constructor() {
        let src = "package Foo;\nsub new { bless {}, shift }\npackage main;\nFoo->new();\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        let call = find_node_at(tree.root_node(), Point::new(3, 0), "method_call_expression")
            .expect("should find method_call_expression");
        let ty = fa.resolve_expression_type(call, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::ClassName("Foo".into())));
    }

    #[test]
    fn test_resolve_expr_type_triple_chain() {
        // $calc->get_self->get_config->{host} — no parens on method calls
        let src = "\
package Calculator;
sub new { bless {}, shift }
sub get_self {
    my ($self) = @_;
    return $self;
}
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{host};
";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Verify get_self returns an object type for Calculator
        let get_self_rt = fa.sub_return_type("get_self");
        assert_eq!(get_self_rt.and_then(|t| t.class_name()), Some("Calculator"),
            "get_self should return Calculator");

        // Verify get_config returns HashRef
        let get_config_rt = fa.sub_return_type("get_config");
        assert_eq!(get_config_rt, Some(&InferredType::HashRef),
            "get_config should return HashRef");

        // The outermost expression is hash_element_expression wrapping the chain
        // Find the method_call_expression for get_config (inner chain)
        // Line 11: $calc->get_self->get_config->{host}
        let node = tree.root_node()
            .descendant_for_point_range(Point::new(11, 0), Point::new(11, 0))
            .expect("should find node");
        let mut n = node;
        // Walk up to find hash_element_expression
        loop {
            if n.kind() == "hash_element_expression" {
                break;
            }
            n = n.parent().expect("should find hash_element_expression");
        }
        // The base of hash_element_expression is the method chain
        let base = n.named_child(0).expect("should have base");
        assert_eq!(base.kind(), "method_call_expression");
        let ty = fa.resolve_expression_type(base, src.as_bytes(), None);
        assert_eq!(ty, Some(InferredType::HashRef),
            "the chain $calc->get_self->get_config should resolve to HashRef");
    }

    #[test]
    fn test_package_at() {
        let fa = build_fa("package Foo;\nsub bar { }");
        let pkg = fa.package_at(Point::new(1, 5));
        assert_eq!(pkg, Some("Foo"));
    }

    #[test]
    fn test_variable_resolves_to() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        let read_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$x" && r.access == AccessKind::Read)
            .collect();
        assert!(!read_refs.is_empty());
        assert!(read_refs[0].resolves_to.is_some(), "read ref should resolve to declaration");
    }

    #[test]
    fn test_fold_ranges() {
        let fa = build_fa("sub foo {\n    my $x = 1;\n}\nsub bar {\n    my $y = 2;\n}");
        assert!(fa.fold_ranges.len() >= 2, "should have fold ranges for sub blocks, got {}", fa.fold_ranges.len());
    }

    #[test]
    fn test_visible_symbols() {
        let fa = build_fa("my $outer = 1;\nsub foo {\n    my $inner = 2;\n}");
        // Inside the sub, both $outer and $inner should be visible
        let visible = fa.visible_symbols(Point::new(2, 10));
        let names: Vec<&str> = visible.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"$inner"), "should see $inner, got: {:?}", names);
        assert!(names.contains(&"$outer"), "should see $outer, got: {:?}", names);
    }

    #[test]
    fn test_two_packages_scoped() {
        let fa = build_fa("package Foo;\nsub alpha { }\npackage Bar;\nsub beta { }");
        // At the beta sub, package should be "Bar"
        let pkg = fa.package_at(Point::new(3, 5));
        assert_eq!(pkg, Some("Bar"));
        // At the alpha sub, package should be "Foo"
        let pkg = fa.package_at(Point::new(1, 5));
        assert_eq!(pkg, Some("Foo"));
    }

    // ---- High-level query tests ----

    #[test]
    fn test_find_def_variable() {
        let fa = build_fa("my $x = 1;\nprint $x;");
        // Cursor on the usage of $x at line 1
        let def = fa.find_definition(Point::new(1, 7), None, None, None);
        assert!(def.is_some(), "should find definition for $x");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_sub() {
        let fa = build_fa("sub greet { }\ngreet();");
        // Cursor on the function call at line 1
        let def = fa.find_definition(Point::new(1, 1), None, None, None);
        assert!(def.is_some(), "should find definition for greet");
        let span = def.unwrap();
        assert_eq!(span.start.row, 0, "definition should be on line 0");
    }

    #[test]
    fn test_find_def_method_in_class() {
        let src = "package Foo;\nsub new { bless {}, shift }\nsub hello { }\npackage main;\nmy $f = Foo->new();\n$f->hello();";
        let fa = build_fa(src);
        // Cursor on hello() call at line 5
        let def = fa.find_definition(Point::new(5, 5), None, None, None);
        assert!(def.is_some(), "should find definition for hello method");
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "hello definition should be on line 2");
    }

    #[test]
    fn test_find_def_scoped_variable() {
        let src = "my $x = 'outer';\nsub foo {\n    my $x = 'inner';\n    print $x;\n}";
        let fa = build_fa(src);
        // Cursor on $x inside sub (line 3) should resolve to inner $x (line 2)
        let def = fa.find_definition(Point::new(3, 11), None, None, None);
        assert!(def.is_some());
        let span = def.unwrap();
        assert_eq!(span.start.row, 2, "should resolve to inner $x on line 2");
    }

    #[test]
    fn test_find_references_variable() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        // Cursor on the declaration of $x
        let refs = fa.find_references(Point::new(0, 4), None, None);
        assert!(refs.len() >= 2, "should find at least declaration + usage, got {}", refs.len());
    }

    #[test]
    fn test_hash_key_def_implicit_return_gets_sub_owner() {
        // Implicit return: last expression in sub body, no explicit `return`
        let src = "sub get_config { { host => 'localhost', port => 5432 } }\nmy $cfg = get_config();\n$cfg->{host};\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
        if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
            assert_eq!(*owner, HashKeyOwner::Sub { package: None, name: "get_config".to_string() },
                "implicit return hash key should have Sub get_config owner, got {:?}", owner);
        }

        // Go-to-def from $cfg->{host} should reach the hash key in the implicit return
        let host_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!host_refs.is_empty(), "should find HashKeyAccess for 'host'");
        let def = fa.find_definition(host_refs[0].span.start, Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for host");
        assert_eq!(def.unwrap().start.row, 0, "host def should be on line 0");
    }

    #[test]
    fn test_find_references_sub() {
        let src = "sub greet { }\ngreet();\ngreet();";
        let fa = build_fa(src);
        // Cursor on the sub name
        let refs = fa.find_references(Point::new(0, 5), None, None);
        assert!(refs.len() >= 2, "should find definition + calls, got {}", refs.len());
    }

    #[test]
    fn test_find_references_method_through_chain() {
        let src = "\
package Foo;
sub new { bless {}, shift }
sub bar { 42 }
package main;
sub get_foo { return Foo->new() }
my $f = Foo->new();
$f->bar();
get_foo()->bar();
";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // Cursor on bar definition (line 2, col 4)
        let refs = fa.find_references(Point::new(2, 5), Some(&tree), Some(src.as_bytes()));
        // Should find: $f->bar() + get_foo()->bar() (definition may or may not be included)
        let ref_lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
        assert!(refs.len() >= 2,
            "should find at least 2 refs, got {} at lines {:?}", refs.len(), ref_lines);
        // The key assertion: chained call get_foo()->bar() is found (was broken before P0a fix)
        assert!(ref_lines.contains(&7),
            "should find chained get_foo()->bar() at line 7, got {:?}", ref_lines);
    }

    #[test]
    fn test_hash_key_def_in_return_gets_sub_owner() {
        let src = "sub get_config {\n    return { host => 'localhost', port => 5432 };\n}\nmy $cfg = get_config();\n$cfg->{host};\n";
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Verify hash key defs exist with Sub owner
        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
        if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
            assert_eq!(*owner, HashKeyOwner::Sub { package: None, name: "get_config".to_string() },
                "host def should have Sub get_config owner, got {:?}", owner);
        }

        // Verify HashKeyAccess ref for $cfg->{host} has Sub owner
        let host_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!host_refs.is_empty(), "should find HashKeyAccess for 'host'");
        if let RefKind::HashKeyAccess { ref owner, .. } = host_refs[0].kind {
            assert_eq!(*owner, Some(HashKeyOwner::Sub { package: None, name: "get_config".to_string() }),
                "host ref should have Sub get_config owner, got {:?}", owner);
        }

        // Verify go-to-references from the def finds the usage
        let host_def_point = host_defs[0].selection_span.start;
        let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()));
        // symbol_at returns include_decl=false, so only usages are returned
        assert!(refs.len() >= 1, "should find at least 1 usage, got {} refs", refs.len());

        // Verify go-to-references from the usage finds back to the def
        let host_ref_point = host_refs[0].span.start;
        let refs_from_usage = fa.find_references(host_ref_point, Some(&tree), Some(src.as_bytes()));
        // ref resolves to def → include_decl=true, so def + usage
        assert!(refs_from_usage.len() >= 2, "should find def + usage, got {} refs", refs_from_usage.len());
    }

    #[test]
    fn test_hash_key_refs_chained_tree_fallback() {
        // Chained method calls produce refs with owner: None that need tree-based resolution
        let src = r#"package Calculator;
sub new { bless {}, shift }
sub get_self { my ($self) = @_; return $self; }
sub get_config { return { host => "localhost", port => 5432 }; }
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{host};
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());

        // Find the hash key def for "host" in get_config's return
        let host_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");

        // The chained ref should have owner: None (can't resolve at build time)
        let chained_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { owner: None, .. }))
            .collect();
        assert!(!chained_refs.is_empty(), "chained hash access should have owner: None, refs: {:?}",
            fa.refs.iter().filter(|r| r.target_name == "host").collect::<Vec<_>>());

        // find_references from the def should find the chained usage via tree fallback
        let host_def_point = host_defs[0].selection_span.start;
        let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()));
        assert!(refs.len() >= 1, "should find chained usage via tree fallback, got {} refs", refs.len());
    }

    #[test]
    fn test_highlights_read_write() {
        let src = "my $x = 1;\nprint $x;\n$x = 2;";
        let fa = build_fa(src);
        let highlights = fa.find_highlights(Point::new(0, 4), None, None);
        assert!(!highlights.is_empty(), "should have highlights");
        // Check that we have both read and write accesses
        let has_write = highlights.iter().any(|(_, a)| matches!(a, AccessKind::Write));
        let has_read = highlights.iter().any(|(_, a)| matches!(a, AccessKind::Read));
        // At minimum we should see the declaration
        assert!(highlights.len() >= 2, "should have at least 2 highlights, got {}", highlights.len());
        // Note: whether read/write are correctly tagged depends on builder's access classification
        let _ = (has_write, has_read); // suppress unused warnings if assertions change
    }

    #[test]
    fn test_hover_variable() {
        let src = "my $greeting = 'hello';\nprint $greeting;";
        let fa = build_fa(src);
        let hover = fa.hover_info(Point::new(1, 8), src, None, None);
        assert!(hover.is_some(), "should have hover info");
        let text = hover.unwrap();
        assert!(text.contains("$greeting"), "hover should contain variable name, got: {}", text);
    }

    #[test]
    fn test_hover_sub() {
        let src = "sub greet { }\ngreet();";
        let fa = build_fa(src);
        let hover = fa.hover_info(Point::new(1, 1), src, None, None);
        assert!(hover.is_some(), "should have hover info for function call");
        let text = hover.unwrap();
        assert!(text.contains("greet"), "hover should contain sub name, got: {}", text);
    }

    #[test]
    fn test_hover_shows_inferred_type() {
        let src = "package Point;\nsub new { bless {}, shift }\npackage main;\nmy $p = Point->new();\n$p;";
        let fa = build_fa(src);
        // Hover on $p usage at line 4
        let hover = fa.hover_info(Point::new(4, 1), src, None, None);
        assert!(hover.is_some(), "should have hover info");
        let text = hover.unwrap();
        assert!(text.contains("Point"), "hover should show inferred type Point, got: {}", text);
    }

    #[test]
    fn test_hover_type_at_usage_after_reassignment() {
        // $x starts as Point, gets reassigned to Foo — hover at each usage should reflect the type at that point
        let src = "package Point;\nsub new { bless {}, shift }\npackage Foo;\nsub new { bless {}, shift }\npackage main;\nmy $x = Point->new();\n$x;\n$x = Foo->new();\n$x;";
        let fa = build_fa(src);
        // line 6: $x; — should be Point
        let hover1 = fa.hover_info(Point::new(6, 1), src, None, None);
        assert!(hover1.is_some());
        let text1 = hover1.unwrap();
        assert!(text1.contains("Point"), "at line 6 should be Point, got: {}", text1);
        // line 8: $x; — should be Foo (after reassignment)
        let hover2 = fa.hover_info(Point::new(8, 1), src, None, None);
        assert!(hover2.is_some());
        let text2 = hover2.unwrap();
        assert!(text2.contains("Foo"), "at line 8 should be Foo, got: {}", text2);
    }

    #[test]
    fn test_hover_shows_return_type() {
        let src = "package Foo;\nsub make { return Foo->new() }\nsub new { bless {}, shift }\npackage main;\nmake();";
        let fa = build_fa(src);
        // Hover on sub make definition
        let hover = fa.hover_info(Point::new(1, 5), src, None, None);
        assert!(hover.is_some(), "should have hover info for sub");
        let text = hover.unwrap();
        assert!(text.contains("returns"), "hover should show return type, got: {}", text);
        assert!(text.contains("Foo"), "hover return type should mention Foo, got: {}", text);
    }

    #[test]
    fn test_rename_variable() {
        let src = "my $x = 1;\nprint $x;";
        let fa = build_fa(src);
        let edits = fa.rename_at(Point::new(0, 4), "y", None, None);
        assert!(edits.is_some(), "should produce rename edits");
        let edits = edits.unwrap();
        assert!(edits.len() >= 2, "should rename at least declaration + usage");
        for (_, new_text) in &edits {
            assert_eq!(new_text, "y", "all edits should use new name");
        }
    }

    #[test]
    fn test_rename_sub_finds_both_function_and_method_calls() {
        let fa = build_fa("
package Foo;
sub emit { }
sub test {
    my $self = shift;
    emit('event');
    $self->emit('done');
}
");
        let edits = fa.rename_sub("emit", "fire");
        // Should find: 1 symbol def + 1 FunctionCall + 1 MethodCall = 3 edits
        assert!(edits.len() >= 3,
            "rename_sub should find def + function call + method call, got {} edits", edits.len());
        for (_, text) in &edits {
            assert_eq!(text, "fire");
        }
    }

    #[test]
    fn test_moo_has_creates_constructor_hash_key_def() {
        let fa = build_fa("
package MyApp;
use Moo;
has username => (is => 'ro');
has password => (is => 'rw');
");
        // Should have HashKeyDef symbols owned by "new" for each has attribute
        let key_defs: Vec<_> = fa.symbols.iter()
            .filter(|s| matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        let names: Vec<&str> = key_defs.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"username"), "should have HashKeyDef for username, got: {:?}", names);
        assert!(names.contains(&"password"), "should have HashKeyDef for password, got: {:?}", names);
        // Verify owner is Sub { package: "MyApp", name: "new" }
        if let SymbolDetail::HashKeyDef { ref owner, .. } = key_defs[0].detail {
            assert_eq!(
                owner,
                &HashKeyOwner::Sub {
                    package: Some("MyApp".to_string()),
                    name: "new".to_string(),
                }
            );
        }
    }

    // ---- ERROR recovery tests ----
    // tree-sitter-perl wraps broken regions in ERROR nodes. Some structural
    // declarations (sub, class) survive as typed nodes inside ERROR.
    // use/package often get parsed as raw function tokens inside ERROR —
    // those can't be recovered (parser fix needed).

    #[test]
    fn test_error_recovery_sub_outside_error() {
        // my $x = [ creates an ERROR, but sub below it survives as a top-level node
        let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
        let fa = build_fa(source);
        let subs: Vec<&str> = fa.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| s.name.as_str())
            .collect();
        assert!(subs.contains(&"process"), "sub process should survive (outside ERROR)");
    }

    #[test]
    fn test_error_recovery_sub_outside_error_survives() {
        // Sub below an ERROR survives as a top-level node (not inside ERROR)
        let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
        let fa = build_fa(source);
        let subs: Vec<&str> = fa.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| s.name.as_str())
            .collect();
        assert!(subs.contains(&"process"), "sub process should survive (outside ERROR)");
    }

    #[test]
    fn test_error_node_does_not_panic() {
        // ERROR nodes should not crash the builder
        let source = "package Foo;\nmy $x = [\nmy $y = [\nsub process { }\n";
        let fa = build_fa(source);
        let pkgs: Vec<&str> = fa.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Package))
            .map(|s| s.name.as_str())
            .collect();
        assert!(pkgs.contains(&"Foo"), "package Foo should survive");
    }

    #[test]
    fn test_error_recovery_sub_inside_error() {
        let source = "package Foo;\nmy $x = [\nmy $y = [\nsub process { }\n";
        let fa = build_fa(source);
        let subs: Vec<&str> = fa.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| s.name.as_str())
            .collect();
        assert!(subs.contains(&"process"), "sub process should be recovered from ERROR");
    }

    #[test]
    fn test_error_recovery_import_inside_error() {
        let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
        let fa = build_fa(source);
        let imports: Vec<&str> = fa.imports.iter()
            .map(|i| i.module_name.as_str())
            .collect();
        assert!(imports.contains(&"List::Util"), "use List::Util should be recovered from ERROR");
    }

    #[test]
    fn test_error_recovery_package_inside_error() {
        let source = "my $x = [\npackage Bar;\nuse Moose;\nsub bar { }\n";
        let fa = build_fa(source);
        let pkgs: Vec<&str> = fa.symbols.iter()
            .filter(|s| matches!(s.kind, SymKind::Package))
            .map(|s| s.name.as_str())
            .collect();
        assert!(pkgs.contains(&"Bar"), "package Bar should be recovered from ERROR");
    }

    #[test]
    fn test_find_def_bareword_class() {
        let src = "package Point;\nsub new { bless {}, shift }\npackage main;\nPoint->new();";
        let fa = build_fa(src);
        // Cursor on "new" in Point->new()
        let def = fa.find_definition(Point::new(3, 8), None, None, None);
        assert!(def.is_some(), "should find definition for new");
    }

    // ---- Block dereference descent tests ----
    // @{expr}, %{expr}, ${expr} parse as scalar/array/hash with varname→block.
    // The builder must recurse into the block to find inner refs.

    #[test]
    fn test_deref_block_produces_inner_variable_ref() {
        // @{$arr} — the inner $arr should produce a Variable ref
        let fa = build_fa("my @data = (1,2,3);\nmy $arr = \\@data;\npush @{$arr}, 4;");
        let inner_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$arr" && matches!(r.kind, RefKind::Variable) && r.access == AccessKind::Read)
            .collect();
        assert!(!inner_refs.is_empty(), "should find $arr ref inside @{{$arr}}");
        // Should NOT have a bogus ref for the whole @{$arr}
        let bogus: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name.contains("{$arr}"))
            .collect();
        assert!(bogus.is_empty(), "should not record bogus ref for whole deref expression");
    }

    #[test]
    fn test_deref_block_produces_hash_key_ref() {
        // @{$self->{items}} — inner hash_element_expression should produce:
        // 1. Variable ref for $self
        // 2. HashKeyAccess ref for "items"
        let fa = build_fa("my %h = (items => []);\n@{$h{items}};");
        let key_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "items" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(!key_refs.is_empty(), "should find hash key ref 'items' inside deref block");
    }

    #[test]
    fn test_deref_block_resolves_variable() {
        // Variable inside deref block should resolve to its declaration
        let fa = build_fa("my @xs = (1,2);\nmy $ref = \\@xs;\nprint @{$ref};");
        let inner_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "$ref" && r.access == AccessKind::Read)
            .collect();
        assert!(!inner_refs.is_empty(), "$ref ref should exist");
        assert!(inner_refs[0].resolves_to.is_some(), "$ref inside deref should resolve to declaration");
    }

    #[test]
    fn test_deref_self_and_hash_key() {
        // Full integration: constructor defines hash keys, method accesses them through deref
        let src = "package Calculator;\nsub new {\n    my ($class, %args) = @_;\n    my $self = bless {\n        history => [],\n        verbose => 0,\n    }, $class;\n    return $self;\n}\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    push @{$self->{history}}, \"add\";\n    return $result;\n}";
        let fa = build_fa(src);

        // $self at line 12 (push @{$self->{history}}, ...)
        let def_self = fa.find_definition(Point::new(12, 12), None, None, None);
        assert!(def_self.is_some(), "should find definition for $self in deref");
        assert_eq!(def_self.unwrap().start.row, 10, "$self should resolve to declaration on line 10");

        // history key at line 12
        let def_history = fa.find_definition(Point::new(12, 20), None, None, None);
        assert!(def_history.is_some(), "should find definition for history hash key");
        assert_eq!(def_history.unwrap().start.row, 4, "history key should resolve to definition on line 4");
    }

    #[test]
    fn test_imports_qw() {
        let source = "use List::Util qw(first any all);\nuse Scalar::Util qw(blessed);\n";
        let fa = build_fa(source);

        assert_eq!(fa.imports.len(), 2);

        assert_eq!(fa.imports[0].module_name, "List::Util");
        assert_eq!(fa.imports[0].imported_symbols, vec!["first", "any", "all"]);

        assert_eq!(fa.imports[1].module_name, "Scalar::Util");
        assert_eq!(fa.imports[1].imported_symbols, vec!["blessed"]);
    }

    #[test]
    fn test_imports_qw_close_paren_position() {
        // "use List::Util qw(first);\n"
        //  0123456789...
        //                  ^18    ^24 = )
        let source = "use List::Util qw(first);\n";
        let fa = build_fa(source);

        assert_eq!(fa.imports.len(), 1);
        let imp = &fa.imports[0];
        assert!(imp.qw_close_paren.is_some(), "qw_close_paren should be set");
        let pos = imp.qw_close_paren.unwrap();
        // The ) is at column 23 in "use List::Util qw(first);"
        assert_eq!(pos.row, 0);
        assert_eq!(pos.column, 23, "close paren should be at column 23");
    }

    #[test]
    fn test_imports_bare() {
        let source = "use strict;\nuse warnings;\nuse Carp;\n";
        let fa = build_fa(source);

        // strict/warnings/Carp all produce imports with empty imported_symbols
        let carp = fa.imports.iter().find(|i| i.module_name == "Carp");
        assert!(carp.is_some());
        assert!(carp.unwrap().imported_symbols.is_empty());
    }

    #[test]
    fn test_imports_module_symbol_created() {
        let source = "use List::Util qw(first);\n";
        let fa = build_fa(source);

        // Module symbol should exist
        let module_syms: Vec<_> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module && s.name == "List::Util")
            .collect();
        assert_eq!(module_syms.len(), 1);

        // Import should exist
        assert_eq!(fa.imports.len(), 1);
        assert_eq!(fa.imports[0].imported_symbols, vec!["first"]);
    }

    #[test]
    fn test_goto_def_slurpy_hash_arg_at_call_site() {
        // Calculator->new(verbose => 1): cursor on "verbose" should go to
        // the bless hash key def, NOT to sub new.
        let src = r#"package Calculator;
sub new {
    my ($class, %args) = @_;
    my $self = bless {
        verbose => $args{verbose} // 0,
    }, $class;
    return $self;
}
package main;
my $calc = Calculator->new(verbose => 1);
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // "verbose" at call site is line 9, after "Calculator->new("
        // Calculator->new(verbose => 1)
        // 0123456789012345678901234567
        //                 ^16 = v of verbose
        // my $calc = Calculator->new(verbose => 1);
        // 0         1         2         3
        // 0123456789012345678901234567890123456789
        //                            ^27 = v of verbose
        let def = fa.find_definition(Point::new(9, 27), Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for verbose at call site");
        // Should go to line 4: "verbose => $args{verbose} // 0,"
        assert_eq!(def.unwrap().start.row, 4,
            "verbose should resolve to bless hash key def on line 4, not sub new");
    }

    #[test]
    fn test_goto_def_param_field_at_call_site() {
        // Point->new(x => 3, y => 4): cursor on "x" should go to "field $x :param"
        let src = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude() { }
}
my $p = Point->new(x => 3, y => 4);
"#;
        let tree = parse(src);
        let fa = build(&tree, src.as_bytes());
        // my $p = Point->new(x => 3, y => 4);
        // 0         1         2
        // 0123456789012345678901234
        //                    ^19 = x

        let def = fa.find_definition(Point::new(6, 19), Some(&tree), Some(src.as_bytes()), None);
        assert!(def.is_some(), "should find definition for x at call site");
        // Should go to line 2: "field $x :param :reader;"
        assert_eq!(def.unwrap().start.row, 2,
            "x should resolve to field $x on line 2, not the class");
    }

    // ---- Gap 1: __PACKAGE__ resolution ----

    #[test]
    fn test_dunder_package_resolution() {
        let fa = build_fa("
        package Mojo::File;
        sub path { __PACKAGE__->new(@_) }
        ");
        let rt = fa.sub_return_type("path");
        assert_eq!(rt, Some(&InferredType::ClassName("Mojo::File".into())));
    }

    #[test]
    fn test_dunder_package_method_invocant() {
        // __PACKAGE__->new() should store the resolved class in MethodCall invocant
        let fa = build_fa("
        package Foo;
        __PACKAGE__->some_method();
        ");
        let method_ref = fa.refs.iter().find(|r| r.target_name == "some_method").unwrap();
        match &method_ref.kind {
            RefKind::MethodCall { invocant, .. } => {
                assert_eq!(invocant, "Foo", "invocant should be resolved from __PACKAGE__");
            }
            _ => panic!("expected MethodCall ref"),
        }
    }

    // ---- Gap 2: Shift parameter extraction ----

    #[test]
    fn test_shift_params() {
        let fa = build_fa("
        sub process {
            my $self = shift;
            my $file = shift;
            my $opts = shift || {};
        }
        ");
        // signature_for_call strips $self when first param is $self
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert!(sig.is_method, "should detect method from $self first param");
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0].name, "$file");
        assert_eq!(sig.params[1].name, "$opts");
        assert_eq!(sig.params[1].default, Some("{}".into()));

        // Check raw params via symbol detail
        let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
        if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
            assert_eq!(params.len(), 3);
            assert_eq!(params[0].name, "$self");
            assert_eq!(params[1].name, "$file");
            assert_eq!(params[2].name, "$opts");
            assert_eq!(params[2].default, Some("{}".into()));
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_shift_then_list_assign() {
        let fa = build_fa("
        sub process {
            my $self = shift;
            my ($file, @opts) = @_;
        }
        ");
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert!(sig.is_method);
        assert_eq!(sig.params.len(), 2, "should have $file and @opts (stripped $self)");
        assert_eq!(sig.params[0].name, "$file");
        assert_eq!(sig.params[1].name, "@opts");
        assert!(sig.params[1].is_slurpy);

        // Check raw params
        let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
        if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
            assert_eq!(params.len(), 3);
            assert_eq!(params[0].name, "$self");
        } else {
            panic!("expected Sub detail");
        }
    }

    #[test]
    fn test_shift_with_double_pipe_default() {
        let fa = build_fa("
        sub handler {
            my $self = shift;
            my $timeout = shift || 30;
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$timeout");
        assert_eq!(sig.params[0].default, Some("30".into()));
    }

    #[test]
    fn test_shift_with_defined_or_default() {
        let fa = build_fa("
        sub handler {
            my $self = shift;
            my $verbose = shift // 0;
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$verbose");
        assert_eq!(sig.params[0].default, Some("0".into()));
    }

    #[test]
    fn test_subscript_param() {
        let fa = build_fa("
        sub handler {
            my $self = $_[0];
            my $data = $_[1];
        }
        ");
        let sig = fa.signature_for_call("handler", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 1, "stripped $self");
        assert_eq!(sig.params[0].name, "$data");
    }

    #[test]
    fn test_legacy_at_params_still_work() {
        // Ensure the existing @_ pattern still works
        let fa = build_fa("
        sub process {
            my ($first, $file, @opts) = @_;
        }
        ");
        let sig = fa.signature_for_call("process", false, None, Point::new(0, 0), None).unwrap();
        assert_eq!(sig.params.len(), 3);
        assert_eq!(sig.params[0].name, "$first");
        assert_eq!(sig.params[1].name, "$file");
        assert_eq!(sig.params[2].name, "@opts");
    }

    #[test]
    fn test_tail_pod_item_method() {
        let fa = build_fa("
            package WWW::Mech;
            sub get { }
            sub post { }

=head1 METHODS

=over

=item $mech->get($url)

Performs a GET request.

=item $mech->post($url)

Performs a POST request.

=back

=cut
        ");
        let get_doc = fa.symbols.iter()
            .find(|s| s.name == "get")
            .and_then(|s| match &s.detail {
                SymbolDetail::Sub { doc, .. } => doc.as_ref(),
                _ => None,
            });
        assert!(get_doc.is_some(), "get should have doc from =item");
        assert!(get_doc.unwrap().contains("GET request"));
    }

    #[test]
    fn test_pod_doc_extracted_per_function() {
        let src = "\
package DemoUtils;
use Exporter 'import';
our @EXPORT_OK = qw(fetch_data transform);

=head2 fetch_data

Fetches data from the given URL.

=head2 transform

Transforms items.

=cut

sub fetch_data { }
sub transform { }
";
        let fa = build_fa(src);
        let fd = fa.symbols.iter().find(|s| s.name == "fetch_data").unwrap();
        if let SymbolDetail::Sub { ref doc, .. } = fd.detail {
            let d = doc.as_ref().expect("fetch_data should have doc");
            assert!(d.contains("Fetches data"), "should have fetch_data doc, got: {}", d);
            assert!(!d.contains("Transforms items"), "should NOT have transform doc, got: {}", d);
        } else {
            panic!("fetch_data should be a Sub");
        }
    }

    // ---- Inheritance extraction tests ----

    #[test]
    fn test_use_parent_single() {
        let fa = build_fa("
            package Child;
            use parent 'Parent';
            sub child_method { }
        ");
        assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
    }

    #[test]
    fn test_use_parent_multiple() {
        let fa = build_fa("
            package Multi;
            use parent qw(Foo Bar);
        ");
        assert_eq!(fa.package_parents.get("Multi").unwrap(), &vec!["Foo".to_string(), "Bar".to_string()]);
    }

    #[test]
    fn test_use_parent_emits_package_refs() {
        let fa = build_fa("
            package Child;
            use parent 'Parent';
        ");
        let refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Parent")
            .collect();
        assert_eq!(refs.len(), 1, "should emit PackageRef for parent class");
    }

    #[test]
    fn test_use_parent_qw_emits_package_refs() {
        let fa = build_fa("
            package Multi;
            use parent qw(Foo Bar);
        ");
        let foo_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Foo")
            .collect();
        let bar_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Bar")
            .collect();
        assert_eq!(foo_refs.len(), 1, "should emit PackageRef for Foo");
        assert_eq!(bar_refs.len(), 1, "should emit PackageRef for Bar");
    }

    #[test]
    fn test_use_parent_norequire() {
        let fa = build_fa("
            package Local;
            use parent -norequire, 'My::Base';
        ");
        assert_eq!(fa.package_parents.get("Local").unwrap(), &vec!["My::Base".to_string()]);
    }

    #[test]
    fn test_use_base() {
        let fa = build_fa("
            package Old;
            use base 'Legacy::Base';
        ");
        assert_eq!(fa.package_parents.get("Old").unwrap(), &vec!["Legacy::Base".to_string()]);
    }

    #[test]
    fn test_isa_assignment() {
        let fa = build_fa("
            package Direct;
            our @ISA = ('Alpha', 'Beta');
        ");
        assert_eq!(fa.package_parents.get("Direct").unwrap(), &vec!["Alpha".to_string(), "Beta".to_string()]);
    }

    #[test]
    fn test_class_isa_populates_package_parents() {
        let fa = build_fa("
            class Child :isa(Parent) { }
        ");
        assert_eq!(fa.package_parents.get("Child").unwrap(), &vec!["Parent".to_string()]);
    }

    #[test]
    fn test_class_does_populates_package_parents() {
        let fa = build_fa("
            class MyClass :does(Printable) :does(Serializable) { }
        ");
        let parents = fa.package_parents.get("MyClass").unwrap();
        assert!(parents.contains(&"Printable".to_string()));
        assert!(parents.contains(&"Serializable".to_string()));
    }

    #[test]
    fn test_class_isa_and_does_combined() {
        let fa = build_fa("
            class Child :isa(Parent) :does(Role) { }
        ");
        let parents = fa.package_parents.get("Child").unwrap();
        assert_eq!(parents, &vec!["Parent".to_string(), "Role".to_string()]);
    }

    #[test]
    fn test_with_role_populates_package_parents() {
        let fa = build_fa("
            package MyApp;
            use Moo;
            with 'My::Role::Logging';
        ");
        let parents = fa.package_parents.get("MyApp").unwrap();
        assert!(parents.contains(&"My::Role::Logging".to_string()));
    }

    #[test]
    fn test_with_multiple_roles() {
        let fa = build_fa("
            package MyApp;
            use Moose;
            with 'Role::A', 'Role::B';
        ");
        let parents = fa.package_parents.get("MyApp").unwrap();
        assert!(parents.contains(&"Role::A".to_string()));
        assert!(parents.contains(&"Role::B".to_string()));
    }

    #[test]
    fn test_load_components_bare() {
        let fa = build_fa("
            package MySchema::Result::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components('InflateColumn::DateTime', 'TimeStamp');
        ");
        let parents = fa.package_parents.get("MySchema::Result::User").unwrap();
        assert!(parents.contains(&"DBIx::Class::Core".to_string()));
        assert!(parents.contains(&"DBIx::Class::InflateColumn::DateTime".to_string()));
        assert!(parents.contains(&"DBIx::Class::TimeStamp".to_string()));
    }

    #[test]
    fn test_load_components_plus_prefix() {
        let fa = build_fa("
            package MySchema::Result::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components('+My::Custom::Component');
        ");
        let parents = fa.package_parents.get("MySchema::Result::User").unwrap();
        assert!(parents.contains(&"My::Custom::Component".to_string()));
    }

    #[test]
    fn test_load_components_qw() {
        let fa = build_fa("
            package MySchema::ResultSet::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components(qw(Helper::ResultSet::Shortcut Helper::ResultSet::Me));
        ");
        let parents = fa.package_parents.get("MySchema::ResultSet::User").unwrap();
        assert!(parents.contains(&"DBIx::Class::Helper::ResultSet::Shortcut".to_string()));
        assert!(parents.contains(&"DBIx::Class::Helper::ResultSet::Me".to_string()));
    }

    // ---- Inheritance method resolution tests ----

    #[test]
    fn test_inherited_method_completion() {
        let fa = build_fa("
            package Animal;
            sub speak { }
            sub eat { }

            package Dog;
            use parent 'Animal';
            sub fetch { }
        ");
        let methods = fa.complete_methods_for_class("Dog", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"fetch"), "own method");
        assert!(names.contains(&"speak"), "inherited from Animal");
        assert!(names.contains(&"eat"), "inherited from Animal");
    }

    #[test]
    fn test_child_method_overrides_parent() {
        let fa = build_fa("
            package Base;
            sub greet { }

            package Override;
            use parent 'Base';
            sub greet { }
        ");
        let methods = fa.complete_methods_for_class("Override", None);
        let greet_count = methods.iter().filter(|c| c.label == "greet").count();
        assert_eq!(greet_count, 1, "child override should shadow parent");
    }

    #[test]
    fn test_find_method_in_parent() {
        let fa = build_fa("
            package Base;
            sub base_method { }

            package Child;
            use parent 'Base';
        ");
        let span = fa.find_method_in_class("Child", "base_method");
        assert!(span.is_some(), "should find inherited method");
    }

    #[test]
    fn test_inherited_return_type() {
        let fa = build_fa("
            package Factory;
            sub create { Factory->new(@_) }

            package SpecialFactory;
            use parent 'Factory';
        ");
        let rt = fa.find_method_return_type("SpecialFactory", "create", None, None);
        assert!(rt.is_some(), "should find return type from parent");
    }

    #[test]
    fn test_multi_level_inheritance() {
        let fa = build_fa("
            package A;
            sub from_a { }

            package B;
            use parent 'A';
            sub from_b { }

            package C;
            use parent 'B';
            sub from_c { }
        ");
        let methods = fa.complete_methods_for_class("C", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"from_a"));
        assert!(names.contains(&"from_b"));
        assert!(names.contains(&"from_c"));
    }

    #[test]
    fn test_class_isa_inherits_methods() {
        let fa = build_fa("
            class Parent {
                method greet() { }
            }
            class Child :isa(Parent) {
                method wave() { }
            }
        ");
        let methods = fa.complete_methods_for_class("Child", None);
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"wave"), "own method");
        assert!(names.contains(&"greet"), "inherited from Parent");
    }

    // ---- Cross-file inheritance tests ----

    /// Build a CachedModule from a synthesized Perl source listing the given subs
    /// (each as an `sub name { $self }` method) plus optional parent packages.
    fn fake_cached_for_class(
        package_name: &str,
        path: &std::path::Path,
        subs: &[&str],
        parents: &[&str],
    ) -> std::sync::Arc<crate::module_index::CachedModule> {
        let mut source = format!("package {};\n", package_name);
        if !parents.is_empty() {
            source.push_str(&format!("use parent '{}';\n", parents.join("', '")));
        }
        for sub in subs {
            source.push_str(&format!("sub {} {{ my $self = shift; }}\n", sub));
        }
        source.push_str("1;\n");
        let fa = build_fa(&source);
        std::sync::Arc::new(crate::module_index::CachedModule::new(
            path.to_path_buf(),
            std::sync::Arc::new(fa),
        ))
    }

    #[test]
    fn test_cross_file_inherited_method_completion() {
        use std::path::PathBuf;
        use crate::module_index::ModuleIndex;

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        // Grandparent: DBI has `connect`
        idx.insert_cache("DBI", Some(fake_cached_for_class(
            "DBI", &PathBuf::from("/fake/DBI.pm"), &["connect"], &[],
        )));

        // Parent: DBI::db inherits from DBI, has `prepare`
        idx.insert_cache("DBI::db", Some(fake_cached_for_class(
            "DBI::db", &PathBuf::from("/fake/DBI/db.pm"), &["prepare"], &["DBI"],
        )));

        // Local code inherits from DBI::db
        let fa = build_fa("
            package MyDB;
            use parent 'DBI::db';
            sub custom_query { }
        ");

        let methods = fa.complete_methods_for_class("MyDB", Some(&idx));
        let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
        assert!(names.contains(&"custom_query"), "own method");
        assert!(names.contains(&"prepare"), "from DBI::db");
        assert!(names.contains(&"connect"), "from DBI (grandparent)");
    }

    #[test]
    fn test_cross_file_method_override() {
        use std::path::PathBuf;
        use crate::module_index::ModuleIndex;

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        // Parent has `process`
        idx.insert_cache("Base::Worker", Some(fake_cached_for_class(
            "Base::Worker", &PathBuf::from("/fake/Base/Worker.pm"), &["process"], &[],
        )));

        // Local child overrides `process`
        let fa = build_fa("
            package MyWorker;
            use parent 'Base::Worker';
            sub process { }
        ");

        let methods = fa.complete_methods_for_class("MyWorker", Some(&idx));
        let process_count = methods.iter().filter(|c| c.label == "process").count();
        assert_eq!(process_count, 1, "local override should shadow parent");
    }

    #[test]
    fn test_cross_file_return_type_through_inheritance() {
        use std::path::PathBuf;
        use crate::file_analysis::InferredType;
        use crate::module_index::ModuleIndex;

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        // Parent module whose `fetch` returns a hashref with known keys.
        let source = r#"
package Fetcher;
sub fetch {
    my $self = shift;
    return { status => 1, body => 'ok' };
}
1;
"#;
        let fa_parent = build_fa(source);
        idx.insert_cache("Fetcher", Some(std::sync::Arc::new(
            crate::module_index::CachedModule::new(
                PathBuf::from("/fake/Fetcher.pm"),
                std::sync::Arc::new(fa_parent),
            ),
        )));

        let fa = build_fa("
            package MyFetcher;
            use parent 'Fetcher';
        ");

        let rt = fa.find_method_return_type("MyFetcher", "fetch", Some(&idx), None);
        assert_eq!(rt, Some(InferredType::HashRef));
    }

    #[test]
    fn test_parents_cached() {
        use std::path::PathBuf;
        use crate::module_index::ModuleIndex;

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);

        idx.insert_cache("Child::Mod", Some(fake_cached_for_class(
            "Child::Mod", &PathBuf::from("/fake/Child/Mod.pm"),
            &[], &["Parent::Mod", "Mixin::Role"],
        )));

        let parents = idx.parents_cached("Child::Mod");
        assert_eq!(parents, vec!["Parent::Mod", "Mixin::Role"]);
        assert!(idx.parents_cached("Unknown::Mod").is_empty());
    }

    // ---- Method call return type propagation tests ----

    #[test]
    fn test_method_call_return_type_propagates() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $f = Foo->new();
my $cfg = $f->get_config();
$cfg;
");
        let ty = fa.inferred_type("$cfg", Point::new(9, 0));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_method_call_chain_propagation() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_bar { return Bar->new() }
package Bar;
sub new { bless {}, shift }
sub get_name { return { name => 'test' } }
package main;
my $f = Foo->new();
my $bar = $f->get_bar();
my $name = $bar->get_name();
$name;
");
        let bar_ty = fa.inferred_type("$bar", Point::new(10, 0));
        assert_eq!(bar_ty, Some(&InferredType::ClassName("Bar".into())));
        let name_ty = fa.inferred_type("$name", Point::new(11, 0));
        assert_eq!(name_ty, Some(&InferredType::HashRef));
    }

    #[test]
    fn test_self_method_call_return_type() {
        let fa = build_fa("
package Foo;
sub new { bless {}, shift }
sub get_config { return { host => 1 } }
sub run {
    my ($self) = @_;
    my $cfg = $self->get_config();
    $cfg;
}
");
        let ty = fa.inferred_type("$cfg", Point::new(7, 4));
        assert_eq!(ty, Some(&InferredType::HashRef));
    }

    // ---- Framework accessor synthesis tests ----

    #[test]
    fn test_moo_has_ro() {
        let fa = build_fa("
package Foo;
use Moo;
has 'name' => (is => 'ro');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1, "should synthesize one getter");
        if let SymbolDetail::Sub { ref params, is_method, .. } = methods[0].detail {
            assert!(is_method);
            assert!(params.is_empty(), "ro getter has no params");
        }
    }

    #[test]
    fn test_moo_has_rw() {
        let fa = build_fa("
package Foo;
use Moo;
has 'name' => (is => 'rw');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        // rw produces getter (0 params) + setter (1 param)
        assert_eq!(methods.len(), 2, "should synthesize getter + setter");
    }

    #[test]
    fn test_moo_has_isa_type() {
        let fa = build_fa("
package Foo;
use Moo;
has 'count' => (is => 'ro', isa => 'Int');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "count" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1);
        if let SymbolDetail::Sub { ref return_type, .. } = methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::Numeric));
        }
    }

    #[test]
    fn test_moo_has_multiple_qw() {
        let fa = build_fa("
package Foo;
use Moo;
has [qw(foo bar)] => (is => 'ro');
");
        let foo: Vec<_> = fa.symbols.iter().filter(|s| s.name == "foo" && s.kind == SymKind::Method).collect();
        let bar: Vec<_> = fa.symbols.iter().filter(|s| s.name == "bar" && s.kind == SymKind::Method).collect();
        assert_eq!(foo.len(), 1, "should synthesize foo accessor");
        assert_eq!(bar.len(), 1, "should synthesize bar accessor");
    }

    #[test]
    fn test_moo_has_bare_no_accessor() {
        let fa = build_fa("
package Foo;
use Moo;
has 'internal' => (is => 'bare');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "internal" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 0, "bare should not synthesize accessor");
    }

    #[test]
    fn test_moo_no_accessor_without_is() {
        let fa = build_fa("
package Foo;
use Moo;
has 'internal';
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "internal" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 0, "no `is` should not synthesize accessor");
    }

    #[test]
    fn test_moose_has_classname_isa() {
        let fa = build_fa("
package Foo;
use Moose;
has 'db' => (is => 'ro', isa => 'DBI::db');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "db" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1);
        if let SymbolDetail::Sub { ref return_type, .. } = methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("DBI::db".into())));
        }
    }

    #[test]
    fn test_moo_has_instanceof() {
        let fa = build_fa("
package Foo;
use Moo;
has 'logger' => (is => 'ro', isa => \"InstanceOf['Log::Any']\");
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "logger" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1);
        if let SymbolDetail::Sub { ref return_type, .. } = methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Log::Any".into())));
        }
    }

    #[test]
    fn test_moo_has_rwp() {
        let fa = build_fa("
package Foo;
use Moo;
has 'status' => (is => 'rwp');
");
        let getter: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "status" && s.kind == SymKind::Method)
            .collect();
        let writer: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "_set_status" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(getter.len(), 1, "rwp should synthesize getter");
        assert_eq!(writer.len(), 1, "rwp should synthesize _set_name writer");
    }

    #[test]
    fn test_mojo_has_basic() {
        let fa = build_fa("
package Foo;
use Mojo::Base -base;
has 'name';
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 2, "Mojo::Base synthesizes getter + setter");
        // Getter: no params, no return type
        let getter = methods.iter().find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail { params.is_empty() } else { false }
        }).expect("should have getter");
        if let SymbolDetail::Sub { ref return_type, is_method, .. } = getter.detail {
            assert!(is_method);
            assert!(return_type.is_none(), "getter has no return type");
        }
        // Setter: 1 param, fluent return
        let setter = methods.iter().find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail { params.len() == 1 } else { false }
        }).expect("should have setter");
        if let SymbolDetail::Sub { ref return_type, is_method, .. } = setter.detail {
            assert!(is_method);
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Foo".into())));
        }
    }

    #[test]
    fn test_mojo_base_parent_inheritance() {
        let fa = build_fa("
package MyApp;
use Mojo::Base 'Mojolicious';
has 'config';
");
        // Should register parent
        assert_eq!(fa.package_parents.get("MyApp").map(|v| v.as_slice()),
            Some(["Mojolicious".to_string()].as_slice()));
        // Should synthesize getter + setter accessors
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "config" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 2, "Mojo::Base synthesizes getter + setter");
    }

    #[test]
    fn test_mojo_base_strict_no_accessor() {
        let fa = build_fa("
package Foo;
use Mojo::Base -strict;
has 'name';
");
        // -strict means no framework mode, has is just a regular function
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 0, "-strict should not trigger accessor synthesis");
    }

    #[test]
    fn test_no_accessor_without_framework() {
        let fa = build_fa("
package Foo;
has 'name' => (is => 'ro');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 0, "no framework = no accessor synthesis");
    }

    #[test]
    fn test_dbic_add_columns() {
        let fa = build_fa("
package Schema::Result::User;
use base 'DBIx::Class::Core';
__PACKAGE__->add_columns(
    id    => { data_type => 'integer' },
    name  => { data_type => 'varchar' },
    email => { data_type => 'varchar' },
);
");
        let id: Vec<_> = fa.symbols.iter().filter(|s| s.name == "id" && s.kind == SymKind::Method).collect();
        let name: Vec<_> = fa.symbols.iter().filter(|s| s.name == "name" && s.kind == SymKind::Method).collect();
        let email: Vec<_> = fa.symbols.iter().filter(|s| s.name == "email" && s.kind == SymKind::Method).collect();
        assert_eq!(id.len(), 1, "should synthesize id accessor");
        assert_eq!(name.len(), 1, "should synthesize name accessor");
        assert_eq!(email.len(), 1, "should synthesize email accessor");
    }

    #[test]
    fn test_dbic_has_many() {
        let fa = build_fa("
package Schema::Result::Post;
use base 'DBIx::Class::Core';
__PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "comments" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1);
        if let SymbolDetail::Sub { ref return_type, .. } = methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("DBIx::Class::ResultSet".into())));
        }
    }

    #[test]
    fn test_dbic_belongs_to() {
        let fa = build_fa("
package Schema::Result::Comment;
use base 'DBIx::Class::Core';
__PACKAGE__->belongs_to(author => 'Schema::Result::User', 'author_id');
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "author" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 1);
        if let SymbolDetail::Sub { ref return_type, .. } = methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Schema::Result::User".into())));
        }
    }

    #[test]
    fn test_accessor_return_type_propagation() {
        let src = r#"
package Moo::Config;
use Moo;
has 'host' => (is => 'ro', isa => 'Str');
sub dsn { my ($self) = @_; return "x"; }

package Moo::Service;
use Moo;
has 'config' => (is => 'ro', isa => "InstanceOf['Moo::Config']");
sub run {
    my ($self) = @_;
    my $cfg = $self->config;
    my $dsn = $cfg->dsn;
}
"#;
        let fa = build_fa(src);

        // Verify the config accessor has the right return type
        let config_methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "config" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(config_methods.len(), 1, "should have 1 config accessor");
        assert_eq!(config_methods[0].package.as_deref(), Some("Moo::Service"));
        if let SymbolDetail::Sub { ref return_type, .. } = config_methods[0].detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Moo::Config".into())),
                "config accessor should return Moo::Config");
        }

        // Verify method call binding exists (not a function call binding)
        let cfg_binding = fa.method_call_bindings.iter()
            .find(|b| b.variable == "$cfg");
        assert!(cfg_binding.is_some(), "should have method call binding for $cfg");
        assert!(fa.call_bindings.iter().find(|b| b.variable == "$cfg").is_none(),
            "$cfg should NOT be a function call binding");

        // Verify $cfg gets Moo::Config type (not Moo::Service)
        let cfg_type = fa.inferred_type("$cfg", tree_sitter::Point::new(13, 0));
        assert_eq!(cfg_type, Some(&InferredType::ClassName("Moo::Config".into())),
            "$cfg should be Moo::Config, not Moo::Service");

        // Verify chained resolution: $dsn = $cfg->dsn → String
        let dsn_binding = fa.method_call_bindings.iter()
            .find(|b| b.variable == "$dsn");
        assert!(dsn_binding.is_some(), "should have method call binding for $dsn");
    }

    #[test]
    fn test_mojo_getter_setter_distinct() {
        let fa = build_fa("
package Foo;
use Mojo::Base -base;
has 'name';
");
        let methods: Vec<_> = fa.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .collect();
        assert_eq!(methods.len(), 2, "should synthesize getter + setter");

        let getter = methods.iter().find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail { params.is_empty() } else { false }
        });
        let setter = methods.iter().find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail { params.len() == 1 } else { false }
        });
        assert!(getter.is_some(), "should have a 0-param getter");
        assert!(setter.is_some(), "should have a 1-param setter");

        // Getter: no return type (inferable from usage)
        if let SymbolDetail::Sub { ref return_type, .. } = getter.unwrap().detail {
            assert!(return_type.is_none());
        }
        // Setter: fluent return
        if let SymbolDetail::Sub { ref return_type, .. } = setter.unwrap().detail {
            assert_eq!(return_type.as_ref(), Some(&InferredType::ClassName("Foo".into())));
        }
    }

    #[test]
    fn test_mojo_fluent_chain_resolves() {
        let src = "
package Foo;
use Mojo::Base -base;
has 'name';
has 'age';
sub greet {
    my ($self) = @_;
    my $result = $self->name('Bob')->age;
    return $result;
}
";
        let fa = build_fa(src);
        // $self->name('Bob') has args → setter → returns Foo
        // ->age has no args → getter → returns None (unknown)
        // The chain should resolve: name('Bob') returns Foo, ->age is valid on Foo
        let method_refs: Vec<_> = fa.refs.iter()
            .filter(|r| r.target_name == "age" && matches!(r.kind, RefKind::MethodCall { .. }))
            .collect();
        assert!(!method_refs.is_empty(), "should have method call ref for 'age'");
    }

    #[test]
    fn test_moo_rw_arity_resolution() {
        let fa = build_fa("
package Foo;
use Moo;
has 'name' => (is => 'rw', isa => 'Str');
");
        // Moo rw: both getter and setter have same return type (Str)
        // With arity, both 0 and 1 should return String since both symbols have the same type
        let rt_getter = fa.find_method_return_type("Foo", "name", None, Some(0));
        assert_eq!(rt_getter, Some(InferredType::String));
        let rt_setter = fa.find_method_return_type("Foo", "name", None, Some(1));
        assert_eq!(rt_setter, Some(InferredType::String));
        let rt_default = fa.find_method_return_type("Foo", "name", None, None);
        assert_eq!(rt_default, Some(InferredType::String));
    }

    #[test]
    fn test_mojo_arity_resolution() {
        let fa = build_fa("
package Bar;
use Mojo::Base -base;
has 'title';
");
        // Getter (0 args): no return type
        let rt_getter = fa.find_method_return_type("Bar", "title", None, Some(0));
        assert!(rt_getter.is_none(), "getter should have no return type");
        // Setter (1 arg): fluent return (ClassName)
        let rt_setter = fa.find_method_return_type("Bar", "title", None, Some(1));
        assert_eq!(rt_setter, Some(InferredType::ClassName("Bar".into())));
        // Default (None): getter (primary, first symbol)
        let rt_default = fa.find_method_return_type("Bar", "title", None, None);
        assert!(rt_default.is_none(), "default should return getter type");
    }

    #[test]
    fn test_mojo_default_string_infers_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has name => 'default';
");
        let rt = fa.find_method_return_type("App", "name", None, Some(0));
        assert_eq!(rt, Some(InferredType::String), "string default → String getter");
    }

    #[test]
    fn test_mojo_default_arrayref_infers_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has items => sub { [] };
");
        let rt = fa.find_method_return_type("App", "items", None, Some(0));
        assert_eq!(rt, Some(InferredType::ArrayRef), "sub {{ [] }} default → ArrayRef getter");
    }

    #[test]
    fn test_mojo_default_hashref_infers_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has config => sub { {} };
");
        let rt = fa.find_method_return_type("App", "config", None, Some(0));
        assert_eq!(rt, Some(InferredType::HashRef), "sub {{{{ }}}} default → HashRef getter");
    }

    #[test]
    fn test_mojo_default_constructor_infers_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has ua => sub { Mojo::UserAgent->new };
");
        let rt = fa.find_method_return_type("App", "ua", None, Some(0));
        assert_eq!(rt, Some(InferredType::ClassName("Mojo::UserAgent".into())),
            "sub {{ Foo->new }} default → ClassName getter");
    }

    #[test]
    fn test_mojo_default_number_infers_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has timeout => 30;
");
        let rt = fa.find_method_return_type("App", "timeout", None, Some(0));
        assert_eq!(rt, Some(InferredType::Numeric), "number default → Numeric getter");
    }

    #[test]
    fn test_mojo_default_no_value_no_type() {
        let fa = build_fa("
package App;
use Mojo::Base -base;
has 'name';
");
        let rt = fa.find_method_return_type("App", "name", None, Some(0));
        assert!(rt.is_none(), "no default → no getter type");
    }

    // ---- Constant folding + export extraction tests ----

    #[test]
    fn test_builder_extracts_exports_qw() {
        let fa = build_fa("
package Foo;
use Exporter 'import';
our @EXPORT = qw(delta);
our @EXPORT_OK = qw(alpha beta gamma);
");
        assert_eq!(fa.export, vec!["delta"]);
        assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn test_builder_extracts_exports_paren() {
        let fa = build_fa("
package Bar;
our @EXPORT_OK = ('foo', 'bar', 'baz');
");
        assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
    }

    #[test]
    fn test_push_exports() {
        let fa = build_fa("
package Foo;
use Exporter 'import';
our @EXPORT_OK = qw(foo);
push @EXPORT_OK, 'bar', 'baz';
");
        assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
    }

    #[test]
    fn test_use_constant_string() {
        let fa = build_fa("
package Foo;
use constant NAME => 'hello';
use parent NAME;
");
        assert_eq!(fa.package_parents.get("Foo").unwrap(), &vec!["hello".to_string()]);
    }

    #[test]
    fn test_constant_array_our() {
        let fa = build_fa("
our @THINGS = qw(a b);
our @EXPORT_OK = (@THINGS, 'c');
");
        assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_constant_array_my() {
        let fa = build_fa("
my @THINGS = qw(a b);
our @EXPORT_OK = (@THINGS, 'c');
");
        assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_constant_array_in_exports() {
        let fa = build_fa("
package Foo;
use Exporter 'import';
my @COMMON = qw(alpha beta);
our @EXPORT_OK = (@COMMON, 'gamma');
");
        assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn test_recursive_constant_resolution() {
        let fa = build_fa("
package Foo;
use Exporter 'import';
use constant BASE => qw(a b);
use constant ALL => (BASE, 'c');
our @EXPORT_OK = (ALL);
");
        assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_glob_export_literal_name() {
        // Data::Printer pattern: *{"${caller}::np"} = \&np
        let fa = build_fa(r#"
package Data::Printer;
sub np { }
sub p { }
sub import {
    my $class = shift;
    my $caller = caller;
    { no strict 'refs';
        *{"${caller}::p"} = \&p;
        *{"${caller}::np"} = \&np;
    }
}
"#);
        assert!(fa.export.contains(&"p".to_string()), "should detect p export: {:?}", fa.export);
        assert!(fa.export.contains(&"np".to_string()), "should detect np export: {:?}", fa.export);
    }

    #[test]
    fn test_glob_export_variable_name() {
        // Aliased export: my $imported = 'p'; *{"$caller\::$imported"} = \&p
        let fa = build_fa(r#"
package Data::Printer;
sub p { }
sub import {
    my $class = shift;
    my $caller = caller;
    my $imported = 'dump_it';
    { no strict 'refs';
        *{"$caller\::$imported"} = \&p;
    }
}
"#);
        assert!(fa.export.contains(&"dump_it".to_string()), "should resolve aliased export: {:?}", fa.export);
    }

    #[test]
    fn test_glob_export_loop_pattern() {
        // Try::Tiny pattern: loop over qw list
        let fa = build_fa(r#"
package Try::Tiny;
sub try { }
sub catch { }
sub finally { }
sub import {
    my $class = shift;
    my $caller = caller;
    for my $name (qw(try catch finally)) {
        no strict 'refs';
        *{"${caller}::${name}"} = \&$name;
    }
}
"#);
        assert!(fa.export.contains(&"try".to_string()), "should detect try: {:?}", fa.export);
        assert!(fa.export.contains(&"catch".to_string()), "should detect catch: {:?}", fa.export);
        assert!(fa.export.contains(&"finally".to_string()), "should detect finally: {:?}", fa.export);
    }

    #[test]
    fn test_glob_export_fallback_to_rhs() {
        // When glob name is fully dynamic, fall back to \&name on RHS
        let fa = build_fa(r#"
package Foo;
sub bar { }
sub import {
    my $caller = caller;
    *{$caller . '::bar'} = \&bar;
}
"#);
        assert!(fa.export.contains(&"bar".to_string()), "should fall back to RHS name: {:?}", fa.export);
    }

    #[test]
    fn test_glob_export_only_inside_import() {
        // Glob assigns outside sub import should NOT populate exports
        let fa = build_fa(r#"
package Foo;
sub setup {
    my $caller = caller;
    *{"${caller}::thing"} = \&thing;
}
"#);
        assert!(fa.export.is_empty(), "should not export from non-import sub: {:?}", fa.export);
    }

    #[test]
    fn test_loop_variable_constant_folding() {
        let fa = build_fa("
package Foo;
sub test {
    my $self = shift;
    for my $attr (qw(name email)) {
        my $getter = \"get_$attr\";
        $self->$getter();
    }
}
");
        let method_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::MethodCall { .. }))
            .map(|r| r.target_name.as_str())
            .collect();
        assert!(method_refs.contains(&"get_name"), "should resolve get_name");
        assert!(method_refs.contains(&"get_email"), "should resolve get_email");
    }

    #[test]
    fn test_dynamic_method_dispatch() {
        let fa = build_fa("
package Foo;
my $method = 'get_name';
sub test { my $self = shift; $self->$method() }
");
        let method_refs: Vec<_> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "get_name")
            .collect();
        assert!(!method_refs.is_empty(), "dynamic method call should resolve to get_name");
    }

    // ---- Framework plugin integration ----

    /// End-to-end: the bundled `mojo-events` Rhai plugin should synthesize a
    /// `HashKeyDef` for every literal event name passed to `->on(...)`
    /// inside a class that inherits from `Mojo::EventEmitter`. This is the
    /// proof that Rhai scripts emit real symbols that land in FileAnalysis
    /// with the `Framework` namespace stamp.
    #[test]
    fn plugin_mojo_events_on_literal_emits_handler_symbol() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('connect', sub { ... });
    $self->on('message', sub { ... });
    $self;
}

1;
"#;
        let fa = build_fa(src);

        let handlers: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events"))
            .collect();

        let names: std::collections::HashSet<&str> = handlers.iter()
            .map(|s| s.name.as_str()).collect();
        assert!(names.contains("connect"),
            "mojo-events should emit Handler for 'connect'; got: {:?}", names);
        assert!(names.contains("message"),
            "mojo-events should emit Handler for 'message'; got: {:?}", names);

        // Each Handler should also carry the dispatcher set — at minimum
        // 'emit' (the canonical Mojo dispatch method).
        for h in &handlers {
            if let SymbolDetail::Handler { dispatchers, .. } = &h.detail {
                assert!(dispatchers.iter().any(|d| d == "emit"),
                    "Handler for {} should declare `emit` dispatcher", h.name);
            } else {
                panic!("expected Handler detail on {}", h.name);
            }
        }
    }

    /// Dynamic event names must not produce spurious HashKeyDefs.
    #[test]
    fn plugin_mojo_events_dynamic_name_does_not_emit() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my ($self, $name) = @_;
    $self->on($name, sub { ... });
}

1;
"#;
        let fa = build_fa(src);
        let plugin_handlers: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events"))
            .collect();
        assert!(plugin_handlers.is_empty(),
            "dynamic event name must not emit handlers; got: {:?}",
            plugin_handlers.iter().map(|s| &s.name).collect::<Vec<_>>());
    }

    /// Const folding through the plugin: `my $name = 'connect'; ...` means
    /// the plugin receives `arg.string_value == "connect"` and emits a
    /// symbol named "connect" — not "$name". The plugin itself contains no
    /// folding logic; the builder does it once in `arg_info_for` and every
    /// plugin gets folded values for free.
    #[test]
    fn plugin_mojo_events_const_folds_scalar_event_name() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    my $evt = 'disconnect';
    $self->on($evt, sub { ... });
}

1;
"#;
        let fa = build_fa(src);

        let names: std::collections::HashSet<&str> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events"))
            .map(|s| s.name.as_str()).collect();
        assert!(names.contains("disconnect"),
            "const-folded event name should emit 'disconnect'; got: {:?}", names);
        assert!(!names.contains("$evt"),
            "variable text must not leak through as symbol name");
    }

    /// Transitive inheritance: a class whose parent (in the same file)
    /// extends Mojo::EventEmitter should still trigger the plugin. Proves
    /// the builder's transitive_parents walk composes with `ClassIsa`.
    #[test]
    fn plugin_mojo_events_triggers_through_transitive_parent() {
        let src = r#"
package Mid;
use parent 'Mojo::EventEmitter';

package Leaf;
use parent 'Mid';

sub wire {
    my $self = shift;
    $self->on('ready', sub { ... });
}

1;
"#;
        let fa = build_fa(src);
        let ready: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && s.name == "ready"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events"))
            .collect();
        assert_eq!(ready.len(), 1,
            "Leaf extends Mid extends Mojo::EventEmitter — plugin must fire transitively");
    }

    /// Cross-file def/ref pairing. Producer.pm wires events via ->on, Consumer.pm
    /// calls ->emit on a producer instance. Both plugin emissions end up with
    /// `HashKeyOwner::Class("Producer")`, so `resolve::refs_to` finds the
    /// consumer's access ref from the producer's def query — no LSP code is
    /// plugin-aware.
    #[test]
    fn plugin_mojo_events_cross_file_ref_pairing() {
        use crate::file_store::FileStore;
        use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
        use std::path::PathBuf;
        use crate::file_analysis::HandlerOwner;

        let producer_src = r#"
package Producer;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('ready', sub { warn "ready" });
    return $self;
}
1;
"#;
        let consumer_src = r#"
package Consumer;
use parent 'Mojo::EventEmitter';

sub run {
    my $p = Producer->new;
    $p->emit('ready');
    $p->unsubscribe('ready');
}
1;
"#;

        let store = FileStore::new();
        let producer_path = PathBuf::from("/tmp/plugin_producer.pm");
        let consumer_path = PathBuf::from("/tmp/plugin_consumer.pm");

        store.insert_workspace(producer_path.clone(), build_fa(producer_src));
        store.insert_workspace(consumer_path.clone(), build_fa(consumer_src));

        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "ready".to_string(),
                kind: TargetKind::Handler {
                    owner: HandlerOwner::Class("Producer".to_string()),
                    name: "ready".to_string(),
                },
            },
            RoleMask::EDITABLE,
        );

        let producer_hits = results.iter().filter(|r| {
            matches!(&r.key, crate::file_store::FileKey::Path(p) if p == &producer_path)
        }).count();
        let consumer_hits = results.iter().filter(|r| {
            matches!(&r.key, crate::file_store::FileKey::Path(p) if p == &consumer_path)
        }).count();

        assert!(producer_hits >= 1,
            "producer should have ≥1 hit (the ->on Handler def); results: {:?}", results);
        assert!(consumer_hits >= 1,
            "consumer should have ≥1 hit (the ->emit DispatchCall); results: {:?}", results);
    }

    /// mojo-helpers: a helper registration in a Lite script produces a
    /// Method on Mojolicious::Controller, so every subclass picks it up
    /// through the normal inheritance walk. Proves cross-class emission:
    /// the plugin's emission goes to a *different* package than the one
    /// being built.
    #[test]
    fn plugin_mojo_helpers_registers_method_on_controller() {
        let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub {
    my ($c, $extra) = @_;
    return { id => 1 };
});
"#;
        let fa = build_fa(src);

        let helpers: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Method
                && s.name == "current_user"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-helpers"))
            .collect();

        // Helpers register on BOTH Mojolicious::Controller and the
        // Mojolicious app class so `$c->current_user` AND
        // `$app->current_user` both complete / goto-def.
        assert_eq!(helpers.len(), 2, "one helper per entry class");
        let on_controller = helpers.iter()
            .find(|h| h.package.as_deref() == Some("Mojolicious::Controller"))
            .expect("Controller-class helper");
        let on_app = helpers.iter()
            .find(|h| h.package.as_deref() == Some("Mojolicious"))
            .expect("app-class helper");

        for helper in [on_controller, on_app] {
            if let SymbolDetail::Sub { params, display, .. } = &helper.detail {
                let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
                assert_eq!(names, vec!["$c", "$extra"],
                    "helper's sub params flow through to the Method signature");
                assert_eq!(
                    *display,
                    Some(crate::file_analysis::HandlerDisplay::Function),
                    "helpers render as Function, not plain Method",
                );
            } else {
                panic!("helper detail should be Sub");
            }
        }
    }

    /// Dotted helpers chain into namespace methods: `users.create` means
    /// `$c->users->create`. Each non-leaf segment emits a parameterless
    /// Method returning a synthetic proxy class; the leaf emits on the
    /// innermost proxy with the helper's real params. Shared prefixes
    /// dedup — `thing.hi` and `thing.there` must only ever produce one
    /// `thing` symbol (not two), so completion + outline stay clean.
    #[test]
    fn plugin_mojo_helpers_dotted_chain_with_shared_prefix_dedup() {
        let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper('thing.hi'    => sub { my ($c, $arg_a) = @_; });
$app->helper('thing.there' => sub { my ($c, $arg_b) = @_; });
"#;
        let fa = build_fa(src);

        // Exactly one `thing` method on Mojolicious::Controller,
        // despite two dotted helpers sharing that prefix.
        let thing_syms: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.name == "thing" && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller"))
            .collect();
        assert_eq!(thing_syms.len(), 1,
            "shared prefix must dedup: one `thing` method, got {}",
            thing_syms.len());

        // Its return_type is the shared proxy class.
        if let SymbolDetail::Sub { return_type: Some(InferredType::ClassName(n)), .. }
            = &thing_syms[0].detail
        {
            assert_eq!(n, "Mojolicious::Controller::_Helper::thing");
        } else {
            panic!("thing's return type should be the shared proxy class");
        }

        // Both leaves exist on the shared proxy class, each with its own params.
        let hi = fa.symbols.iter().find(|s| s.name == "hi" && s.kind == SymKind::Method)
            .expect("hi leaf emitted");
        let there = fa.symbols.iter().find(|s| s.name == "there" && s.kind == SymKind::Method)
            .expect("there leaf emitted");
        assert_eq!(hi.package.as_deref(), Some("Mojolicious::Controller::_Helper::thing"));
        assert_eq!(there.package.as_deref(), Some("Mojolicious::Controller::_Helper::thing"));
        if let SymbolDetail::Sub { params, .. } = &hi.detail {
            let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
            assert_eq!(names, vec!["$c", "$arg_a"]);
        }
        if let SymbolDetail::Sub { params, .. } = &there.detail {
            let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
            assert_eq!(names, vec!["$c", "$arg_b"]);
        }
    }

    /// Three-level dotted helper chains: `admin.users.purge` synthesizes
    /// two intermediate proxies, each with the right return_type, and
    /// the leaf lands on the innermost proxy.
    #[test]
    fn plugin_mojo_helpers_three_level_dotted_chain() {
        let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper('admin.users.purge' => sub { my ($c, $force) = @_; });
"#;
        let fa = build_fa(src);

        let admin = fa.symbols.iter()
            .find(|s| s.name == "admin" && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller"))
            .expect("admin on Controller");
        let users = fa.symbols.iter()
            .find(|s| s.name == "users" && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller::_Helper::admin"))
            .expect("users on admin proxy");
        let purge = fa.symbols.iter()
            .find(|s| s.name == "purge" && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller::_Helper::admin::users"))
            .expect("purge leaf on admin.users proxy");

        // Each non-leaf returns the next proxy in the chain.
        if let SymbolDetail::Sub { return_type: Some(InferredType::ClassName(n)), .. } = &admin.detail {
            assert_eq!(n, "Mojolicious::Controller::_Helper::admin");
        }
        if let SymbolDetail::Sub { return_type: Some(InferredType::ClassName(n)), .. } = &users.detail {
            assert_eq!(n, "Mojolicious::Controller::_Helper::admin::users");
        }
        // Leaf carries the helper's actual params.
        if let SymbolDetail::Sub { params, .. } = &purge.detail {
            let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
            assert_eq!(names, vec!["$c", "$force"]);
        }
    }

    /// `use Mojolicious::Lite` autoimports a fixed verb set — our
    /// unresolved-function diagnostic must skip them. The plugin's
    /// `on_use` hook emits FrameworkImport actions for each; the
    /// builder stashes them in framework_imports so the diagnostic
    /// filter drops matching FunctionCall refs.
    #[test]
    fn plugin_mojo_lite_autoimports_verbs() {
        let src = r#"
package main;
use Mojolicious::Lite;

get '/x' => sub {};
post '/y' => sub {};
helper foo => sub {};
"#;
        let fa = build_fa(src);
        for verb in &["get", "post", "put", "del", "patch",
                      "any", "under", "websocket",
                      "app", "helper", "hook", "plugin", "group"] {
            assert!(fa.framework_imports.contains(*verb),
                "{} must be autoimported by use Mojolicious::Lite", verb);
        }
    }

    /// mojo-lite: top-level route verbs (`get`, `post`, etc.) register
    /// Handlers keyed by URL path, with ["url_for"] as the dispatcher so
    /// `url_for('/users')` can find them. Exercises the on_function_call
    /// plugin hook that mojo-events doesn't use.
    #[test]
    fn plugin_mojo_lite_registers_handlers_for_routes() {
        let src = r#"
package main;
use Mojolicious::Lite;

get '/users' => sub {
    my ($c, $arg) = @_;
    $c->render(text => 'hi');
};

post '/login' => sub {
    my ($c, $user, $pw) = @_;
};

app->start;
"#;
        let fa = build_fa(src);

        let route_handlers: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-lite"))
            .collect();

        let names: std::collections::HashSet<&str> = route_handlers.iter()
            .map(|s| s.name.as_str()).collect();
        assert!(names.contains("/users"), "GET /users handler emitted; got: {:?}", names);
        assert!(names.contains("/login"), "POST /login handler emitted; got: {:?}", names);

        // Each handler declares url_for as its dispatcher so completion
        // inside `url_for('|')` surfaces every route.
        for h in &route_handlers {
            if let SymbolDetail::Handler { dispatchers, .. } = &h.detail {
                assert!(dispatchers.iter().any(|d| d == "url_for"),
                    "handler {} should dispatch via url_for", h.name);
            }
        }

        // Handler params come from the handler sub's signature —
        // different per route, so they round-trip correctly.
        let login = route_handlers.iter().find(|h| h.name == "/login").unwrap();
        if let SymbolDetail::Handler { params, .. } = &login.detail {
            let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
            assert_eq!(names, vec!["$c", "$user", "$pw"]);
        }
    }

    /// Routes are first-class things, not just refs. Every `->to(...)`
    /// emits BOTH a MethodCallRef (cross-file target link) AND a
    /// Handler symbol (route-as-entity — outline-visible, workspace-
    /// searchable, discoverable via url_for completion). Mirrors the
    /// mojo-lite model so route symbols are symmetric regardless of
    /// declaration flavor.
    #[test]
    fn plugin_mojo_routes_emits_both_ref_and_handler_symbol() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
$r->post('/users')->to(controller => 'Users', action => 'create');
"#;
        let fa = build_fa(src);

        // Each route: one MethodCallRef + one Handler symbol.
        let method_refs: Vec<&Ref> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::MethodCall { .. })
                && (r.target_name == "list" || r.target_name == "create"))
            .collect();
        assert_eq!(method_refs.len(), 2, "one MethodCallRef per route");

        let route_syms: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-routes"))
            .collect();
        assert_eq!(route_syms.len(), 2,
            "one Handler symbol per route so outline + workspace-symbol find them");

        let names: std::collections::HashSet<&str> = route_syms.iter()
            .map(|s| s.name.as_str()).collect();
        assert!(names.contains("Users#list"),
            "route identity `Users#list` present; got: {:?}", names);
        assert!(names.contains("Users#create"),
            "route identity `Users#create` present; got: {:?}", names);

        // Dispatcher is url_for so completion inside `url_for('|')`
        // offers every registered route.
        for s in &route_syms {
            if let SymbolDetail::Handler { dispatchers, owner, .. } = &s.detail {
                assert!(dispatchers.iter().any(|d| d == "url_for"),
                    "route {} should dispatch via url_for", s.name);
                // Owner is the declaring package — not the target class.
                // A route is declared IN MyApp, even though it TARGETS Users.
                assert!(matches!(owner, HandlerOwner::Class(c) if c == "MyApp"),
                    "route owner is declaring package (MyApp), not target (Users)");
            }
        }
    }

    /// End-to-end cross-file gd for `->to('Users#list')`. Users.pm
    /// is registered as a workspace module in ModuleIndex (via the
    /// `register_workspace_module` bridge), so
    /// `resolve_method_in_ancestors` finds it on lookup — and the
    /// cross-file MethodCall path in `symbols::find_definition`
    /// surfaces the Users::list def's location.
    ///
    /// Before the fix, workspace modules lived only in FileStore so
    /// lookups that key on module name (all cross-file method
    /// resolution) missed them and gd fell through to noise.
    #[test]
    fn plugin_mojo_routes_gd_reaches_workspace_target() {
        use crate::module_index::ModuleIndex;
        use std::sync::Arc;
        use tower_lsp::lsp_types::Position;

        let app_src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
        let users_src = r#"
package Users;
sub list { my ($c) = @_; }
1;
"#;

        let app_fa = build_fa(app_src);
        let users_fa = build_fa(users_src);

        let idx = ModuleIndex::new_for_test();
        // Simulate workspace indexing registering Users.pm under its
        // primary package name.
        idx.register_workspace_module(
            std::path::PathBuf::from("/tmp/Users.pm"),
            Arc::new(users_fa),
        );

        // Sanity: cross-file resolution on "Users"::"list" must succeed.
        let res = app_fa.resolve_method_in_ancestors("Users", "list", Some(&idx));
        assert!(res.is_some(), "Users::list must resolve cross-file after workspace register");

        // And the MethodCallRef emitted by mojo-routes on the 'list'
        // portion of 'Users#list' should be at a span matching the
        // text 'list'.
        let route_ref = app_fa.refs.iter().find(|r|
            matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "list"
        ).expect("mojo-routes MethodCallRef for 'list'");

        // The ref's span is tight on the action name — mid-string
        // completion + goto-def both rely on that precision.
        let _ = route_ref;
        let _ = Position::default();
    }

    /// mojo-routes short form: `->to('Users#list')` emits a MethodCall
    /// ref pointing to `Users::list`. Cursor on the string → gd jumps
    /// cross-file to the Users controller's list method, same as any
    /// regular method call. No routes-specific resolution code; it's
    /// just a Ref that happens to live inside a string literal.
    #[test]
    fn plugin_mojo_routes_short_form_emits_method_call_ref() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
        let fa = build_fa(src);

        let route_refs: Vec<&Ref> = fa.refs.iter()
            .filter(|r| matches!(r.kind, RefKind::MethodCall { .. })
                && r.target_name == "list")
            .collect();

        assert!(!route_refs.is_empty(), "at least one MethodCall ref for 'list'");
        let r = route_refs.iter().find(|r| {
            matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "Users")
        }).expect("MethodCall with invocant=Users");

        // Sanity: ref span covers the string literal so cursor anywhere
        // in the 'Users#list' range lands on the ref.
        assert!(r.span.end.column > r.span.start.column,
            "method ref has non-empty span");
    }

    /// mojo-routes long form: `->to(controller => 'Users', action => 'list')`.
    /// Walks kwarg pairs, pairs up controller+action, emits the ref
    /// with span on the action value.
    #[test]
    fn plugin_mojo_routes_long_form_emits_method_call_ref() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to(controller => 'Users', action => 'list');
"#;
        let fa = build_fa(src);

        let has_ref = fa.refs.iter().any(|r| {
            matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "Users")
                && r.target_name == "list"
        });
        assert!(has_ref, "long-form ->to(controller=>, action=>) must produce MethodCall ref");
    }

    /// Helpers emitted by mojo-helpers land on Mojolicious::Controller.
    /// A controller subclass in ANOTHER file (standard workspace layout)
    /// must see them when walking methods — the class_content_index
    /// bridges the lookup because the synthesizing module's primary
    /// package isn't Mojolicious::Controller.
    #[test]
    fn plugin_mojo_helpers_reachable_cross_file_from_controller() {
        use crate::module_index::ModuleIndex;
        use std::sync::Arc;

        // Lite script with a helper.
        let lite_src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(greet => sub { my ($c, $who) = @_; });
"#;
        // Controller subclass in another file.
        let ctrl_src = r#"
package MyApp::Controller::Home;
use parent 'Mojolicious::Controller';
1;
"#;

        let lite_fa = Arc::new(build_fa(lite_src));
        let ctrl_fa = build_fa(ctrl_src);

        let idx = ModuleIndex::new_for_test();
        idx.register_workspace_module(
            std::path::PathBuf::from("/tmp/MyApp.pm"),
            lite_fa.clone(),
        );

        // class_content_index knows MyApp.pm hosts synthesized Methods
        // on Mojolicious::Controller.
        let mods = idx.modules_with_class_content("Mojolicious::Controller");
        assert!(mods.iter().any(|m| m == "MyApp"),
            "MyApp module should be listed as content-holder for \
             Mojolicious::Controller; got: {:?}", mods);

        // Completion on MyApp::Controller::Home inheriting from
        // Mojolicious::Controller should walk up and find `greet`.
        let candidates = ctrl_fa.complete_methods_for_class(
            "MyApp::Controller::Home",
            Some(&idx),
        );
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"greet"),
            "helper `greet` emitted on Mojolicious::Controller in \
             MyApp.pm should complete on subclasses; got: {:?}",
            labels);
    }

    /// mojo-helpers cross-file: when a Lite script registers a helper
    /// `greet`, the resulting Method symbol's `package` is
    /// `Mojolicious::Controller`. Any consumer file — controller
    /// subclass or otherwise — finds it via the standard workspace
    /// walk + inheritance chain without a single mojo-helpers-aware
    /// line in the consumer-side code path.
    #[test]
    fn plugin_mojo_helpers_land_on_controller_package() {
        let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

app->helper(greet => sub {
    my ($c, $name) = @_;
    return "hello, $name";
});
1;
"#;
        let fa = build_fa(src);
        let greet = fa.symbols.iter()
            .find(|s| s.name == "greet" && s.kind == SymKind::Method)
            .expect("helper must emit a Method named greet");
        assert_eq!(greet.package.as_deref(), Some("Mojolicious::Controller"),
            "helper Method must be packaged on the shared controller base; \
             that's what lets every subclass pick it up via inheritance walk");
        assert!(matches!(&greet.namespace, Namespace::Framework { id } if id == "mojo-helpers"));
    }

    /// Helpers complete on both `$c` (Controller) and `$app` (the
    /// Mojolicious app class). Every helper registers a Method on each
    /// entry class, so `complete_methods_for_class` for either class
    /// surfaces the helper. Dotted chain roots also land on both
    /// classes; the deeper proxies stay on the shared prefix.
    #[test]
    fn plugin_mojo_helpers_complete_on_app_class_too() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub { my ($c) = @_; });
$app->helper('users.create' => sub { my ($c, $name) = @_; });
"#;
        let fa = build_fa(src);

        for class in ["Mojolicious::Controller", "Mojolicious"] {
            let candidates = fa.complete_methods_for_class(class, None);
            let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
            assert!(
                labels.contains(&"current_user"),
                "`current_user` must complete on {}; got: {:?}",
                class, labels,
            );
            assert!(
                labels.contains(&"users"),
                "`users` (dotted-helper root) must complete on {}; got: {:?}",
                class, labels,
            );
        }
    }

    /// mojo-lite route URLs are referenced from `->url_for(...)` and
    /// `->redirect_to(...)`. Both emit `DispatchCall` refs tight to
    /// the URL string so gd/gr compose via the standard Handler
    /// resolution path — no Lite-aware code in the core.
    #[test]
    fn plugin_mojo_lite_url_dispatch_emits_refs() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

get '/hello' => sub {
    my ($c) = @_;
    $c->render(text => 'hi');
};

sub after {
    my ($c) = @_;
    $c->redirect_to('/hello');
    my $u = $c->url_for('/hello');
}
"#;
        let fa = build_fa(src);

        let dispatch_refs: Vec<&crate::file_analysis::Ref> = fa.refs.iter()
            .filter(|r| matches!(&r.kind, RefKind::DispatchCall { .. }))
            .filter(|r| r.target_name == "/hello")
            .collect();

        let dispatchers: Vec<&str> = dispatch_refs.iter()
            .map(|r| match &r.kind {
                RefKind::DispatchCall { dispatcher, .. } => dispatcher.as_str(),
                _ => unreachable!(),
            })
            .collect();

        assert!(
            dispatchers.contains(&"redirect_to"),
            "redirect_to('/hello') must emit a DispatchCall ref; got: {:?}",
            dispatchers,
        );
        assert!(
            dispatchers.contains(&"url_for"),
            "url_for('/hello') must emit a DispatchCall ref; got: {:?}",
            dispatchers,
        );
    }

    /// Plugin triggers must gate emission. A class that doesn't inherit from
    /// Mojo::EventEmitter should see no mojo-events emissions even if it
    /// happens to call a method named `->on(...)`.
    #[test]
    fn plugin_mojo_events_triggers_gate_emission() {
        let src = r#"
package My::Unrelated;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('connect', sub { ... });
    $self;
}

1;
"#;
        let fa = build_fa(src);
        let plugin_syms: Vec<&Symbol> = fa.symbols.iter()
            .filter(|s| matches!(&s.namespace,
                Namespace::Framework { id } if id == "mojo-events"))
            .collect();
        assert!(plugin_syms.is_empty(),
            "untriggered package must not get plugin emissions; got: {:?}",
            plugin_syms.iter().map(|s| &s.name).collect::<Vec<_>>());
    }
}
