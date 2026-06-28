//! Rhai-scripted plugin support.
//!
//! A `RhaiPlugin` wraps a compiled Rhai script that exposes top-level
//! functions: `id()`, `triggers()`, `on_function_call(ctx)`, `on_method_call(ctx)`,
//! and/or `on_use(ctx)`. Each callback returns an array of emission object-maps
//! that convert back into strongly-typed `EmitAction` values.
//!
//! All context and emission data crosses the script boundary as Rhai `Dynamic`
//! — we never hand out mutable references to the builder. This keeps scripts
//! pure and lets us test them as functions from input to action list.

use std::sync::Arc;

use rhai::{
    serde::{from_dynamic, to_dynamic},
    Array, Dynamic, Engine, AST,
};

use crate::file_analysis::{HashKeyOwner, InferredType, Span};
use tree_sitter::Point;

use super::{
    CallContext, CompletionQueryContext, ConstraintParam, DispatchVerb, EmitAction,
    FrameworkPlugin, ParamType, PluginCompletionAnswer, PluginSigHelpAnswer, SigHelpQueryContext,
    Trigger, TypeOverride, UseContext,
};

/// An engine built with our helpers and type registrations. Engines are
/// cheap to reuse; scripts share one instance across all callbacks.
pub fn make_engine() -> Engine {
    let mut engine = Engine::new();
    engine.set_max_expr_depths(64, 64);
    // Kill switch: a runaway Rhai script (infinite loop, stuck on a bad
    // input) would otherwise hang the LSP build thread indefinitely.
    // 1M operations is comfortably more than any sensible plugin hook
    // needs (emit hooks top out in the hundreds; query hooks lower) and
    // low enough to bail in well under a second on modern hardware.
    // Override via `PERL_LSP_RHAI_MAX_OPS` for debugging heavy plugins.
    let max_ops: u64 = std::env::var("PERL_LSP_RHAI_MAX_OPS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1_000_000);
    engine.set_max_operations(max_ops);

    // Shorthand constructors for `HashKeyOwner` — Rhai scripts avoid writing
    // enum discriminants by using these helper functions.
    engine.register_fn("owner_sub", |package: String, name: String| {
        let pkg = if package.is_empty() { None } else { Some(package) };
        let owner = HashKeyOwner::Sub { package: pkg, name };
        to_dynamic(owner).unwrap_or(Dynamic::UNIT)
    });
    engine.register_fn("owner_sub_unscoped", |name: String| {
        let owner = HashKeyOwner::Sub { package: None, name };
        to_dynamic(owner).unwrap_or(Dynamic::UNIT)
    });
    engine.register_fn("owner_class", |class: String| {
        let owner = HashKeyOwner::Class(class);
        to_dynamic(owner).unwrap_or(Dynamic::UNIT)
    });

    // `InferredType` convenience constructors — scripts can say
    // `type_class("Foo")` instead of nesting enum variants manually.
    engine.register_fn("type_string", || to_dynamic(InferredType::String).unwrap());
    engine.register_fn("type_numeric", || to_dynamic(InferredType::Numeric).unwrap());
    engine.register_fn("type_hashref", || to_dynamic(InferredType::HashRef).unwrap());
    engine.register_fn("type_arrayref", || to_dynamic(InferredType::ArrayRef).unwrap());
    engine.register_fn("type_coderef", || {
        to_dynamic(InferredType::CodeRef { return_edge: None }).unwrap()
    });
    engine.register_fn("type_regexp", || to_dynamic(InferredType::Regexp).unwrap());
    engine.register_fn("type_class", |class: String| {
        to_dynamic(InferredType::ClassName(class)).unwrap_or(Dynamic::UNIT)
    });

    // Project a constraint type to what it constrains — the rhai mirror of
    // `InferredType::constrained_inner`. A nested constructor param
    // (`Maybe[InstanceOf['Foo']]`) arrives as a `ConstraintParam.ty` typed
    // `TypeConstraintOf(inner)`; a passthrough fold (`Maybe[T]` → T's inner)
    // asks the value for its inner without destructuring the serde shape.
    // Unit for a non-constraint `ty` (or `()`), so the fold declines cleanly.
    engine.register_fn("constrained_inner", |ty: Dynamic| -> Dynamic {
        let Ok(t) = from_dynamic::<InferredType>(&ty) else { return Dynamic::UNIT; };
        match t.constrained_inner() {
            Some(inner) => to_dynamic(inner.clone()).unwrap_or(Dynamic::UNIT),
            None => Dynamic::UNIT,
        }
    });

    // Mark a param-list's first element as the implicit invocant.
    // Framework callbacks typically receive the receiver as their
    // first positional (`$c` for Mojolicious helpers, `$self_in`
    // for Mojo::EventEmitter handlers, etc.); the plugin knows
    // this, the core does not. Running the array through this
    // helper tells sig help / hover / outline to drop param 0 at
    // display time without the core matching on names.
    engine.register_fn("as_invocant_params", |list: Array| -> Array {
        let mut out = list;
        if let Some(first) = out.get_mut(0) {
            if let Ok(mut m) = first.as_map_mut() {
                m.insert("is_invocant".into(), Dynamic::from(true));
            }
        }
        out
    });

    // Subspan helper: plugins frequently want to narrow a parser-given
    // span (e.g. a whole string literal) down to a portion of its
    // content (the method-name half of `"Controller#action"`). This
    // returns a new span on the same row with columns offset from
    // `base`'s start. Plugins pass column *deltas* — 0 for "start of
    // base", `len(content)` for "end" — and we compute the absolute
    // columns.
    engine.register_fn(
        "subspan_cols",
        |base: Dynamic, col_start_delta: i64, col_end_delta: i64| -> Dynamic {
            let Ok(span) = from_dynamic::<Span>(&base) else { return Dynamic::UNIT; };
            let start = Point::new(
                span.start.row,
                (span.start.column as i64 + col_start_delta).max(0) as usize,
            );
            let end = Point::new(
                span.start.row,
                (span.start.column as i64 + col_end_delta).max(0) as usize,
            );
            to_dynamic(Span { start, end }).unwrap_or(Dynamic::UNIT)
        },
    );

    // `classified_pairs(args, start)` — THE shared keyval-pairing primitive
    // over a flat `ctx.args`. Pairs `args[start]`,`args[start+1]` as
    // `(key, value)`, stepping by two — separator-agnostic (`k => v` and
    // `'k', v` walk identically), `start` skips a leading positional head
    // (a route target / attr name). The key is the even arg's `value_shape`
    // Str; the value is the odd arg's full classified `value_shape`, so a
    // caller branches on `p.value.Str` / `p.value.HashPairs` / etc. Lives in
    // the host (not copied per plugin) so `to`, Moo `has`, and any future
    // keyval verb share one implementation. Args reach here already peeled
    // flat (`Builder::flat_call_args`), so the nested `has 'x' => (...)`
    // form pairs the same as the flat `has 'x', k => v` one.
    fn arg_map_field(d: &Dynamic, key: &str) -> Dynamic {
        d.read_lock::<rhai::Map>()
            .and_then(|m| m.get(key).cloned())
            .unwrap_or(Dynamic::UNIT)
    }
    engine.register_fn("classified_pairs", |args: Array, start: i64| -> Array {
        let mut out = Array::new();
        if start < 0 {
            return out;
        }
        let mut i = start as usize;
        while i + 1 < args.len() {
            let key = arg_map_field(&arg_map_field(&args[i], "value_shape"), "Str");
            if let Ok(key) = key.into_string() {
                let val_arg = &args[i + 1];
                let mut m = rhai::Map::new();
                m.insert("key".into(), key.into());
                m.insert("key_span".into(), arg_map_field(&args[i], "span"));
                m.insert("value".into(), arg_map_field(val_arg, "value_shape"));
                m.insert(
                    "value_content_span".into(),
                    arg_map_field(val_arg, "content_span"),
                );
                m.insert("value_span".into(), arg_map_field(val_arg, "span"));
                out.push(Dynamic::from_map(m));
            }
            i += 2;
        }
        out
    });

    engine
}

pub struct RhaiPlugin {
    id: String,
    triggers: Vec<Trigger>,
    overrides: Vec<TypeOverride>,
    dispatch_verbs: Vec<DispatchVerb>,
    load_verbs: Vec<crate::plugin::LoadVerb>,
    param_types: Vec<ParamType>,
    type_constraint_names: Vec<String>,
    app_surface_consumers: Vec<String>,
    role_makers: Vec<String>,
    generators: Vec<crate::plugin::GeneratorDef>,
    arg_name_verbs: Vec<String>,
    topic_route_dsl: Option<crate::plugin::TopicRouteDsl>,
    engine: Arc<Engine>,
    ast: Arc<AST>,
    has_on_function_call: bool,
    has_type_constraint_inner: bool,
    has_on_method_call: bool,
    has_on_use: bool,
    has_on_signature_help: bool,
    has_on_completion: bool,
}

impl RhaiPlugin {
    /// Compile a script from source text and interrogate its metadata
    /// (`id()`, `triggers()`) up-front so dispatch is cheap.
    pub fn from_source(
        source: &str,
        engine: Arc<Engine>,
    ) -> Result<Self, String> {
        let ast = engine
            .compile(source)
            .map_err(|e| format!("rhai compile: {}", e))?;

        let id: String = engine
            .call_fn(&mut rhai::Scope::new(), &ast, "id", ())
            .map_err(|e| format!("rhai `id()`: {}", e))?;

        let trig_dyn: Array = engine
            .call_fn(&mut rhai::Scope::new(), &ast, "triggers", ())
            .map_err(|e| format!("rhai `triggers()`: {}", e))?;

        let mut triggers = Vec::with_capacity(trig_dyn.len());
        for d in trig_dyn {
            let t: Trigger = from_dynamic(&d)
                .map_err(|e| format!("bad trigger from `{}`: {}", id, e))?;
            triggers.push(t);
        }

        let signatures: Vec<String> = ast
            .iter_functions()
            .map(|f| f.name.to_string())
            .collect();

        // `overrides()` is optional. Read once at compile time; missing
        // function == no overrides. A bad return shape logs and treats
        // as empty — same fail-safe as the emit hooks (a broken plugin
        // shouldn't break the build).
        let mut overrides: Vec<TypeOverride> = Vec::new();
        if signatures.iter().any(|n| n == "overrides") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "overrides", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<TypeOverride>(&d) {
                            Ok(o) => overrides.push(o),
                            Err(e) => log::error!(
                                "plugin `{}` overrides() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` overrides() failed: {}", id, e),
            }
        }

        // `dispatch_verbs()` — same optional, fail-safe contract as overrides.
        let mut dispatch_verbs: Vec<DispatchVerb> = Vec::new();
        if signatures.iter().any(|n| n == "dispatch_verbs") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "dispatch_verbs", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<DispatchVerb>(&d) {
                            Ok(v) => dispatch_verbs.push(v),
                            Err(e) => log::error!(
                                "plugin `{}` dispatch_verbs() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` dispatch_verbs() failed: {}", id, e),
            }
        }

        // `load_verbs()` — same optional, fail-safe contract.
        let mut load_verbs: Vec<crate::plugin::LoadVerb> = Vec::new();
        if signatures.iter().any(|n| n == "load_verbs") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "load_verbs", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<crate::plugin::LoadVerb>(&d) {
                            Ok(v) => load_verbs.push(v),
                            Err(e) => log::error!(
                                "plugin `{}` load_verbs() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` load_verbs() failed: {}", id, e),
            }
        }

        // `param_types()` — role-contract parameter typing.
        let mut param_types: Vec<ParamType> = Vec::new();
        if signatures.iter().any(|n| n == "param_types") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "param_types", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<ParamType>(&d) {
                            Ok(v) => param_types.push(v),
                            Err(e) => log::error!(
                                "plugin `{}` param_types() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` param_types() failed: {}", id, e),
            }
        }

        // `type_constraint_names()` — the constraint-constructor dispatch gate.
        let mut type_constraint_names: Vec<String> = Vec::new();
        if signatures.iter().any(|n| n == "type_constraint_names") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "type_constraint_names", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<String>(&d) {
                            Ok(s) => type_constraint_names.push(s),
                            Err(e) => log::error!(
                                "plugin `{}` type_constraint_names() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` type_constraint_names() failed: {}", id, e),
            }
        }

        // `app_surface_consumers()` — the declared receiver set for the
        // app surface; same optional, fail-safe array-of-strings shape.
        let mut app_surface_consumers: Vec<String> = Vec::new();
        if signatures.iter().any(|n| n == "app_surface_consumers") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "app_surface_consumers", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<String>(&d) {
                            Ok(s) => app_surface_consumers.push(s),
                            Err(e) => log::error!(
                                "plugin `{}` app_surface_consumers() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` app_surface_consumers() failed: {}", id, e),
            }
        }

        // `role_makers()` — modules whose `use` makes the consuming
        // package a role; same optional, fail-safe array-of-strings shape.
        let mut role_makers: Vec<String> = Vec::new();
        if signatures.iter().any(|n| n == "role_makers") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "role_makers", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<String>(&d) {
                            Ok(s) => role_makers.push(s),
                            Err(e) => log::error!(
                                "plugin `{}` role_makers() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` role_makers() failed: {}", id, e),
            }
        }

        // `generators()` — symbol-generator manifest; same optional,
        // fail-safe array-of-maps contract as dispatch_verbs.
        let mut generators: Vec<crate::plugin::GeneratorDef> = Vec::new();
        if signatures.iter().any(|n| n == "generators") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "generators", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<crate::plugin::GeneratorDef>(&d) {
                            Ok(g) => generators.push(g),
                            Err(e) => log::error!(
                                "plugin `{}` generators() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` generators() failed: {}", id, e),
            }
        }

        // `arg_name_verbs()` — call verbs wanting flat-arg-name
        // extraction; same optional, fail-safe array-of-strings shape.
        let mut arg_name_verbs: Vec<String> = Vec::new();
        if signatures.iter().any(|n| n == "arg_name_verbs") {
            match engine.call_fn::<Array>(&mut rhai::Scope::new(), &ast, "arg_name_verbs", ()) {
                Ok(arr) => {
                    for d in arr {
                        match from_dynamic::<String>(&d) {
                            Ok(s) => arg_name_verbs.push(s),
                            Err(e) => log::error!(
                                "plugin `{}` arg_name_verbs() bad entry: {}",
                                id,
                                e
                            ),
                        }
                    }
                }
                Err(e) => log::error!("plugin `{}` arg_name_verbs() failed: {}", id, e),
            }
        }

        // `topic_route_dsl()` — optional manifest map; bad shapes log
        // and disable rather than fail the plugin.
        let mut topic_route_dsl: Option<crate::plugin::TopicRouteDsl> = None;
        if signatures.iter().any(|n| n == "topic_route_dsl") {
            match engine.call_fn::<Dynamic>(&mut rhai::Scope::new(), &ast, "topic_route_dsl", ()) {
                Ok(d) => match from_dynamic::<crate::plugin::TopicRouteDsl>(&d) {
                    Ok(t) => topic_route_dsl = Some(t),
                    Err(e) => log::error!("plugin `{}` topic_route_dsl() bad shape: {}", id, e),
                },
                Err(e) => log::error!("plugin `{}` topic_route_dsl() failed: {}", id, e),
            }
        }

        Ok(Self {
            has_on_function_call: signatures.iter().any(|n| n == "on_function_call"),
            has_type_constraint_inner: signatures.iter().any(|n| n == "type_constraint_inner"),
            has_on_method_call: signatures.iter().any(|n| n == "on_method_call"),
            has_on_use: signatures.iter().any(|n| n == "on_use"),
            has_on_signature_help: signatures.iter().any(|n| n == "on_signature_help"),
            has_on_completion: signatures.iter().any(|n| n == "on_completion"),
            id,
            triggers,
            overrides,
            dispatch_verbs,
            load_verbs,
            param_types,
            type_constraint_names,
            app_surface_consumers,
            role_makers,
            generators,
            arg_name_verbs,
            topic_route_dsl,
            engine,
            ast: Arc::new(ast),
        })
    }

    /// Call a Rhai query hook that returns a single map (sig help)
    /// or nil. Returns `None` if the script's fn returned unit or
    /// the call failed — plugins stay silent unless they have
    /// something to contribute.
    fn call_opt_map<T: serde::de::DeserializeOwned>(&self, fn_name: &str, arg: Dynamic) -> Option<T> {
        let out: Result<Dynamic, _> =
            self.engine.call_fn(&mut rhai::Scope::new(), &self.ast, fn_name, (arg,));
        let v = match out {
            Ok(v) => v,
            Err(e) => {
                // A Rhai call failure is a plugin bug (bad script, kill
                // switch triggered, panic inside the VM) — log at
                // ERROR, not warn. `warn!` gets filtered by default
                // log levels and the user sees missing features with
                // no hint.
                log::error!("plugin `{}`::{} failed: {}", self.id, fn_name, e);
                return None;
            }
        };
        if v.is_unit() { return None; }
        match from_dynamic::<T>(&v) {
            Ok(parsed) => Some(parsed),
            Err(e) => {
                log::error!("plugin `{}`::{} bad return: {}", self.id, fn_name, e);
                None
            }
        }
    }

    fn dispatch(&self, fn_name: &str, arg: Dynamic) -> Vec<EmitAction> {
        let out: Result<Array, _> =
            self.engine.call_fn(&mut rhai::Scope::new(), &self.ast, fn_name, (arg,));
        let arr = match out {
            Ok(a) => a,
            Err(e) => {
                log::error!("plugin `{}`::{} failed: {}", self.id, fn_name, e);
                return Vec::new();
            }
        };
        arr.into_iter()
            .filter_map(|d| {
                from_dynamic::<EmitAction>(&d)
                    .map_err(|e| {
                        log::error!(
                            "plugin `{}`::{} bad emission: {}",
                            self.id,
                            fn_name,
                            e
                        )
                    })
                    .ok()
            })
            .collect()
    }
}

impl FrameworkPlugin for RhaiPlugin {
    fn id(&self) -> &str {
        &self.id
    }

    fn triggers(&self) -> &[Trigger] {
        &self.triggers
    }

    fn overrides(&self) -> &[TypeOverride] {
        &self.overrides
    }

    fn dispatch_verbs(&self) -> &[DispatchVerb] {
        &self.dispatch_verbs
    }

    fn load_verbs(&self) -> &[crate::plugin::LoadVerb] {
        &self.load_verbs
    }

    fn param_types(&self) -> &[ParamType] {
        &self.param_types
    }

    fn type_constraint_names(&self) -> &[String] {
        &self.type_constraint_names
    }

    fn app_surface_consumers(&self) -> &[String] {
        &self.app_surface_consumers
    }

    fn role_makers(&self) -> &[String] {
        &self.role_makers
    }

    fn generators(&self) -> &[crate::plugin::GeneratorDef] {
        &self.generators
    }

    fn arg_name_verbs(&self) -> &[String] {
        &self.arg_name_verbs
    }

    fn topic_route_dsl(&self) -> Option<crate::plugin::TopicRouteDsl> {
        self.topic_route_dsl.clone()
    }

    fn type_constraint_inner(
        &self,
        name: &str,
        params: &[ConstraintParam],
    ) -> Option<InferredType> {
        if !self.has_type_constraint_inner {
            return None;
        }
        let params_dyn = to_dynamic(params).ok()?;
        let out: Result<Dynamic, _> = self.engine.call_fn(
            &mut rhai::Scope::new(),
            &self.ast,
            "type_constraint_inner",
            (name.to_string(), params_dyn),
        );
        let v = match out {
            Ok(v) => v,
            Err(e) => {
                log::error!("plugin `{}`::type_constraint_inner failed: {}", self.id, e);
                return None;
            }
        };
        if v.is_unit() {
            return None;
        }
        match from_dynamic::<InferredType>(&v) {
            Ok(t) => Some(t),
            Err(e) => {
                log::error!(
                    "plugin `{}`::type_constraint_inner bad return: {}",
                    self.id,
                    e
                );
                None
            }
        }
    }

    fn on_function_call(&self, ctx: &CallContext) -> Vec<EmitAction> {
        if !self.has_on_function_call {
            return Vec::new();
        }
        match to_dynamic(ctx) {
            Ok(d) => self.dispatch("on_function_call", d),
            Err(e) => {
                log::warn!("plugin `{}`: ctx serialize: {}", self.id, e);
                Vec::new()
            }
        }
    }

    fn on_method_call(&self, ctx: &CallContext) -> Vec<EmitAction> {
        if !self.has_on_method_call {
            return Vec::new();
        }
        match to_dynamic(ctx) {
            Ok(d) => self.dispatch("on_method_call", d),
            Err(e) => {
                log::warn!("plugin `{}`: ctx serialize: {}", self.id, e);
                Vec::new()
            }
        }
    }

    fn on_signature_help(&self, ctx: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> {
        if !self.has_on_signature_help { return None; }
        let d = to_dynamic(ctx).ok()?;
        self.call_opt_map("on_signature_help", d)
    }

    fn on_completion(&self, ctx: &CompletionQueryContext) -> Option<PluginCompletionAnswer> {
        if !self.has_on_completion { return None; }
        let d = to_dynamic(ctx).ok()?;
        self.call_opt_map("on_completion", d)
    }

    fn on_use(&self, ctx: &UseContext) -> Vec<EmitAction> {
        if !self.has_on_use {
            return Vec::new();
        }
        match to_dynamic(ctx) {
            Ok(d) => self.dispatch("on_use", d),
            Err(e) => {
                log::warn!("plugin `{}`: use ctx serialize: {}", self.id, e);
                Vec::new()
            }
        }
    }
}

// ---- Bundled plugins ----

/// Bundled Rhai script sources shipped with the binary. Third-party plugins
/// can be loaded from disk via `load_plugin_dir`.
const BUNDLED: &[(&str, &str)] = &[
    ("mojo-events", include_str!("../../frameworks/mojo-events.rhai")),
    ("mojo-helpers", include_str!("../../frameworks/mojo-helpers.rhai")),
    ("mojo-routes", include_str!("../../frameworks/mojo-routes.rhai")),
    ("mojo-lite", include_str!("../../frameworks/mojo-lite.rhai")),
    ("minion", include_str!("../../frameworks/minion.rhai")),
    ("data-printer", include_str!("../../frameworks/data-printer.rhai")),
    ("dbic-resultddl", include_str!("../../frameworks/dbic-resultddl.rhai")),
    ("dbic", include_str!("../../frameworks/dbic.rhai")),
    ("type-tiny", include_str!("../../frameworks/type-tiny.rhai")),
    ("dancer", include_str!("../../frameworks/dancer.rhai")),
    ("moo", include_str!("../../frameworks/moo.rhai")),
    ("catalyst", include_str!("../../frameworks/catalyst.rhai")),
];

pub fn load_bundled(engine: Arc<Engine>) -> Vec<Box<dyn FrameworkPlugin>> {
    let mut out: Vec<Box<dyn FrameworkPlugin>> = Vec::new();
    for (id, src) in BUNDLED {
        match RhaiPlugin::from_source(src, engine.clone()) {
            Ok(p) => {
                log::info!("loaded bundled plugin `{}`", id);
                out.push(Box::new(p));
            }
            Err(e) => {
                log::warn!("bundled plugin `{}` failed to load: {}", id, e);
            }
        }
    }
    out
}

/// The workspace root, pinned from the SAME value the LSP/CLI hands
/// `ModuleIndex::set_workspace_root` — so repo-local plugin discovery and
/// the per-project SQLite cache agree on what "the project" is. If they
/// were derived independently (e.g. a cwd ancestor-walk), a plugin set
/// loaded against one root could invalidate a cache keyed on another.
static WORKSPACE_ROOT: std::sync::RwLock<Option<std::path::PathBuf>> =
    std::sync::RwLock::new(None);

/// Record the workspace root for repo-local plugin discovery. Accepts the
/// same `file://…` URI (or bare path) passed to the module index; `None`
/// clears it (no project root → no repo-local plugins). Call this before
/// the first `build()` so the process-wide registry sees it.
pub fn set_workspace_root(root: Option<&str>) {
    let path = root.map(|r| {
        std::path::PathBuf::from(r.strip_prefix("file://").unwrap_or(r))
    });
    if let Ok(mut guard) = WORKSPACE_ROOT.write() {
        *guard = path;
    }
}

/// Directories to load user `.rhai` plugins from, in priority order.
///
/// 1. `$PERL_LSP_PLUGIN_DIR` — explicit global opt-in (a power user's
///    personal plugin collection), honored whenever it points at a dir.
/// 2. `<workspace-root>/.perl-lsp/` — a project shipping plugins for its
///    own kits, with zero global config. The root is whatever the client
///    sent at `initialize` (or the CLI's root arg), the exact value the
///    SQLite cache is keyed on — pinned here, never re-derived, so the
///    fingerprint that invalidates the cache and the plugins actually
///    loaded stay in lockstep. With no root set (single-file CLI modes),
///    falls back to `./.perl-lsp` so running inside a project still works.
///
/// Both the loader and the cache fingerprint call this, so "what gets
/// loaded" and "what invalidates the cache" can't drift apart.
pub fn plugin_search_dirs() -> Vec<std::path::PathBuf> {
    let mut dirs: Vec<std::path::PathBuf> = Vec::new();
    if let Ok(dir) = std::env::var("PERL_LSP_PLUGIN_DIR") {
        let p = std::path::PathBuf::from(dir);
        if p.is_dir() {
            dirs.push(p);
        }
    }
    let root = WORKSPACE_ROOT
        .read()
        .ok()
        .and_then(|g| g.clone())
        .or_else(|| std::env::current_dir().ok());
    if let Some(root) = root {
        let repo = root.join(".perl-lsp");
        if repo.is_dir() && !dirs.contains(&repo) {
            dirs.push(repo);
        }
    }
    dirs
}

/// Stable hash of the plugin set the next `build()` will load. Used by the
/// SQLite module cache to invalidate stored FileAnalysis blobs when the
/// plugins that produced them have changed — without this, editing a
/// `.rhai` and restarting the LSP serves stale cross-file analyses.
///
/// Inputs:
///   * Every bundled plugin's `(id, source)` pair — catches binary
///     rebuilds whose only change was a `frameworks/*.rhai` edit.
///   * Every `.rhai` file in each `plugin_search_dirs()` entry, with its
///     path — catches user-plugin add / remove / rename / edit across
///     LSP restarts, whether the plugin lives in `$PERL_LSP_PLUGIN_DIR`
///     or a repo-local `.perl-lsp/`.
///
/// Read-only: no compile, no side effects, no log spam. Fails open
/// (returns the bundled-only hash) if a dir can't be read, matching
/// the rest of the loader's silently-tolerant behavior.
pub fn plugin_fingerprint() -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();

    for (id, src) in BUNDLED {
        id.hash(&mut hasher);
        src.hash(&mut hasher);
    }

    for path in plugin_search_dirs() {
        // Sort entries by path so the hash is independent of readdir order.
        let mut entries: Vec<std::path::PathBuf> = match std::fs::read_dir(&path) {
            Ok(read) => read
                .flatten()
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("rhai"))
                .collect(),
            Err(_) => Vec::new(),
        };
        entries.sort();
        for p in entries {
            // Path is part of the hash so a rename invalidates.
            p.to_string_lossy().hash(&mut hasher);
            if let Ok(src) = std::fs::read_to_string(&p) {
                src.hash(&mut hasher);
            }
        }
    }

    format!("{:016x}", hasher.finish())
}

/// Load all `*.rhai` files from a directory. Used for user-installed plugins.
pub fn load_plugin_dir(
    dir: &std::path::Path,
    engine: Arc<Engine>,
) -> Vec<Box<dyn FrameworkPlugin>> {
    let mut out: Vec<Box<dyn FrameworkPlugin>> = Vec::new();
    let read = match std::fs::read_dir(dir) {
        Ok(r) => r,
        Err(_) => return out,
    };
    for entry in read.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("rhai") {
            continue;
        }
        let source = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                log::warn!("plugin {}: read: {}", path.display(), e);
                continue;
            }
        };
        match RhaiPlugin::from_source(&source, engine.clone()) {
            Ok(p) => {
                log::info!("loaded plugin {} from {}", p.id(), path.display());
                out.push(Box::new(p));
            }
            Err(e) => log::warn!("plugin {}: {}", path.display(), e),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_analysis::Span;
    use tree_sitter::Point;

    fn sp(r1: usize, c1: usize, r2: usize, c2: usize) -> Span {
        Span { start: Point::new(r1, c1), end: Point::new(r2, c2) }
    }

    #[test]
    fn minimal_plugin_loads_and_dispatches() {
        let src = r#"
            fn id() { "demo" }
            fn triggers() { [ #{ UsesModule: "Demo" } ] }
            fn on_function_call(ctx) {
                if ctx.function_name == "greet" {
                    return [
                        #{
                            Method: #{
                                name: "hello",
                                span: ctx.call_span,
                                selection_span: ctx.selection_span,
                                params: [],
                                is_method: true,
                                return_type: (),
                                doc: (),
                            }
                        }
                    ];
                }
                []
            }
        "#;

        let engine = Arc::new(make_engine());
        let plugin = RhaiPlugin::from_source(src, engine).expect("compiles");
        assert_eq!(plugin.id(), "demo");
        assert_eq!(plugin.triggers().len(), 1);

        let ctx = CallContext {
            call_kind: super::super::CallKind::Function,
            function_name: Some("greet".into()),
            method_name: None,
            receiver_text: None,
            receiver_call_name: None,
            receiver_type: None,
            receiver_route_defaults: Vec::new(),
            args: vec![],
            call_span: sp(0, 0, 0, 5),
            selection_span: sp(0, 0, 0, 5),
            current_package: Some("Demo::App".into()),
            current_package_parents: vec![],
            current_package_uses: vec!["Demo".into()],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_function_call(&ctx);
        assert_eq!(emissions.len(), 1);
        match &emissions[0] {
            EmitAction::Method { name, is_method, .. } => {
                assert_eq!(name, "hello");
                assert!(*is_method);
            }
            other => panic!("unexpected emission: {:?}", other),
        }
    }

    #[test]
    fn generators_manifest_parses_from_rhai() {
        // The symbol-generator manifest round-trips through Rhai maps:
        // flat `#{ emit, kind }` / `#{ generate, args }` actions.
        let src = r#"
            fn id() { "gen-demo" }
            fn triggers() { [ #{ Always: () } ] }
            fn generators() {
                [
                    #{ name: "make_crud_helpers", params: ["thing"], actions: [
                        #{ emit: "${thing}_id", kind: "accessor" },
                        #{ emit: "get_${thing}", kind: "method" },
                    ] },
                    #{ name: "make_resource", params: ["res"], actions: [
                        #{ emit: "${res}_table", kind: "accessor" },
                        #{ generate: "make_crud_helpers", args: ["${res}"] },
                    ] },
                ]
            }
        "#;
        let engine = Arc::new(make_engine());
        let plugin = RhaiPlugin::from_source(src, engine).expect("compiles");
        let gens = plugin.generators();
        assert_eq!(gens.len(), 2);

        let crud = gens.iter().find(|g| g.name == "make_crud_helpers").unwrap();
        assert_eq!(crud.params, vec!["thing".to_string()]);
        assert_eq!(crud.actions.len(), 2);
        assert_eq!(crud.actions[0].emit.as_deref(), Some("${thing}_id"));
        assert_eq!(crud.actions[0].kind.as_deref(), Some("accessor"));

        let res = gens.iter().find(|g| g.name == "make_resource").unwrap();
        let nested = res.actions.iter().find(|a| a.generate.is_some()).unwrap();
        assert_eq!(nested.generate.as_deref(), Some("make_crud_helpers"));
        assert_eq!(nested.args, vec!["${res}".to_string()]);
    }

    #[test]
    fn plugin_fingerprint_invariants() {
        // Two contracts in one test (env var is process-global, so
        // splitting these into parallel-safe tests would race):
        //
        //   1. Stability — identical inputs must produce identical
        //      hashes, otherwise we'd nuke the SQLite cache on every
        //      LSP startup.
        //   2. Sensitivity — editing a `.rhai` in the user plugin dir
        //      must change the fingerprint, so the cache invalidates
        //      on the next LSP restart and the author can QA their
        //      changes.
        let dir = std::env::temp_dir().join(format!(
            "perl-lsp-fp-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let plugin_path = dir.join("test.rhai");

        let saved = std::env::var("PERL_LSP_PLUGIN_DIR").ok();
        std::env::set_var("PERL_LSP_PLUGIN_DIR", &dir);

        std::fs::write(&plugin_path, r#"fn id() { "v1" } fn triggers() { [] }"#).unwrap();
        let v1a = plugin_fingerprint();
        let v1b = plugin_fingerprint();

        std::fs::write(&plugin_path, r#"fn id() { "v2" } fn triggers() { [] }"#).unwrap();
        let v2 = plugin_fingerprint();

        // Restore env BEFORE asserting so a panic doesn't leak the override.
        match saved {
            Some(v) => std::env::set_var("PERL_LSP_PLUGIN_DIR", v),
            None => std::env::remove_var("PERL_LSP_PLUGIN_DIR"),
        }
        let _ = std::fs::remove_file(&plugin_path);
        let _ = std::fs::remove_dir(&dir);

        assert!(!v1a.is_empty(), "fingerprint should never be empty");
        assert_eq!(v1a, v1b, "fingerprint must be deterministic");
        assert_ne!(v1a, v2, "fingerprint must change when a user plugin's source changes");
    }

    #[test]
    fn bundled_script_compiles() {
        // Diagnostic: if load_bundled drops ANY script due to a compile
        // error, surface the real error instead of the opaque
        // "not found" failure later. Each bundled script is exercised.
        let engine = Arc::new(make_engine());
        for (id, src) in [
            ("mojo-events", include_str!("../../frameworks/mojo-events.rhai")),
            ("mojo-helpers", include_str!("../../frameworks/mojo-helpers.rhai")),
            ("mojo-routes", include_str!("../../frameworks/mojo-routes.rhai")),
            ("mojo-lite", include_str!("../../frameworks/mojo-lite.rhai")),
            ("minion", include_str!("../../frameworks/minion.rhai")),
            ("data-printer", include_str!("../../frameworks/data-printer.rhai")),
            ("dbic-resultddl", include_str!("../../frameworks/dbic-resultddl.rhai")),
            ("dbic", include_str!("../../frameworks/dbic.rhai")),
            ("type-tiny", include_str!("../../frameworks/type-tiny.rhai")),
            ("dancer", include_str!("../../frameworks/dancer.rhai")),
            ("moo", include_str!("../../frameworks/moo.rhai")),
            ("catalyst", include_str!("../../frameworks/catalyst.rhai")),
        ] {
            RhaiPlugin::from_source(src, engine.clone())
                .unwrap_or_else(|e| panic!("{}.rhai failed to compile: {e}", id));
        }
    }

    #[test]
    fn bundled_mojo_events_loads_and_emits() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "mojo-events")
            .expect("mojo-events is bundled");

        let evt_span = sp(3, 15, 3, 23);
        let cb_span = sp(3, 25, 3, 40);
        let ctx = CallContext {
            call_kind: CallKind::Method,
            function_name: None,
            method_name: Some("on".into()),
            receiver_text: Some("$self".into()),
            receiver_call_name: None,
            receiver_type: Some(InferredType::ClassName("My::Emitter".into())),
            receiver_route_defaults: Vec::new(),
            args: vec![
                ArgInfo {
                    text: "'connect'".into(),
                    string_value: Some("connect".into()),
                    string_values: Vec::new(),
                    span: evt_span,
                    content_span: None,
                    inferred_type: Some(InferredType::String), sub_params: vec![], callable_return_edge: None, ref_sub_name: None, value_shape: Default::default(),
                },
                ArgInfo {
                    text: "sub { ... }".into(),
                    string_value: None,
                    string_values: Vec::new(),
                    span: cb_span,
                    content_span: None,
                    inferred_type: Some(InferredType::CodeRef { return_edge: None }), sub_params: vec![], callable_return_edge: None, ref_sub_name: None, value_shape: Default::default(),
                },
            ],
            call_span: sp(3, 4, 3, 45),
            selection_span: sp(3, 10, 3, 12),
            current_package: Some("My::Emitter".into()),
            current_package_parents: vec!["Mojo::EventEmitter".into()],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_method_call(&ctx);
        // DispatchCall (ref) + Handler (def) + PluginNamespace (bridge).
        assert_eq!(emissions.len(), 3,
            "dispatch call + handler + namespace; got: {:?}", emissions);

        let has_dispatch = emissions.iter().any(|e| {
            matches!(e, EmitAction::DispatchCall { name, dispatcher, .. }
                if name == "connect" && dispatcher == "on")
        });
        assert!(has_dispatch, "missing DispatchCall for 'connect' via ->on");

        let has_handler = emissions.iter().any(|e| {
            matches!(e, EmitAction::Handler { name, .. } if name == "connect")
        });
        assert!(has_handler, "missing Handler symbol for 'connect'");

        let has_namespace = emissions.iter().any(|e| {
            matches!(e, EmitAction::PluginNamespace { id, kind, entity_names, .. }
                if id == "mojo-events:My::Emitter"
                    && kind == "events"
                    && entity_names.iter().any(|n| n == "connect"))
        });
        assert!(has_namespace,
            "missing PluginNamespace for My::Emitter events; got: {:?}", emissions);
    }

    #[test]
    fn bundled_dbic_resultddl_synthesizes_accessors() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "dbic-resultddl")
            .expect("dbic-resultddl is bundled");

        // `col text => text;` and `has_many searches => {...};` each install
        // an accessor named by the (autoquoted) first arg.
        let cases = [("col", "text"), ("has_many", "searches"), ("belongs_to", "product")];
        for (func, accessor) in cases {
            let name_span = sp(1, 4, 1, 8);
            let ctx = CallContext {
                call_kind: CallKind::Function,
                function_name: Some(func.into()),
                method_name: None,
                receiver_text: None,
                receiver_call_name: None,
                receiver_type: None,
                receiver_route_defaults: Vec::new(),
                args: vec![ArgInfo {
                    text: accessor.into(),
                    string_value: Some(accessor.into()),
                    string_values: Vec::new(),
                    span: name_span,
                    content_span: None,
                    inferred_type: Some(InferredType::String),
                    sub_params: vec![],
                    callable_return_edge: None,
                    ref_sub_name: None, value_shape: Default::default(),
                }],
                call_span: sp(1, 0, 1, 20),
                selection_span: sp(1, 0, 1, 3),
                current_package: Some("My::Schema::Result::Thing".into()),
                current_package_parents: vec![],
                current_package_uses: vec!["DBIx::Class::ResultDDL".into()],
                has_options: None,
                arg_names: Vec::new(),
                receiver_is_package: false,
            };

            let emissions = plugin.on_function_call(&ctx);
            let has_method = emissions.iter().any(|e| {
                matches!(e, EmitAction::Method { name, is_method, .. }
                    if name == accessor && *is_method)
            });
            assert!(has_method,
                "{func} '{accessor}' should synthesize an accessor Method; got: {emissions:?}");
        }
    }

    #[test]
    fn dbic_resultddl_skips_dynamic_and_non_dsl() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled.into_iter().find(|p| p.id() == "dbic-resultddl").unwrap();

        let mk = |func: &str, string_value: Option<String>| CallContext {
            call_kind: CallKind::Function,
            function_name: Some(func.into()),
            method_name: None,
            receiver_text: None,
            receiver_call_name: None,
            receiver_type: None,
            receiver_route_defaults: Vec::new(),
            args: vec![ArgInfo {
                text: "x".into(),
                string_value,
                string_values: Vec::new(),
                span: sp(1, 4, 1, 8),
                content_span: None,
                inferred_type: None,
                sub_params: vec![],
                callable_return_edge: None,
                ref_sub_name: None, value_shape: Default::default(),
            }],
            call_span: sp(1, 0, 1, 20),
            selection_span: sp(1, 0, 1, 3),
            current_package: Some("My::Schema::Result::Thing".into()),
            current_package_parents: vec![],
            current_package_uses: vec!["DBIx::Class::ResultDDL".into()],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        // Dynamic column name (`col $field => ...`) — nothing to synthesize.
        assert!(plugin.on_function_call(&mk("col", None)).is_empty(),
            "dynamic col name must be skipped");
        // A non-DSL function with a string arg is not our concern.
        assert!(plugin.on_function_call(&mk("table", Some("embeddings".into()))).is_empty(),
            "`table` declares no accessor and must emit nothing");
    }

    #[test]
    fn mojo_events_skips_dynamic_event_name() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "mojo-events")
            .unwrap();

        let ctx = CallContext {
            call_kind: CallKind::Method,
            function_name: None,
            method_name: Some("on".into()),
            receiver_text: Some("$self".into()),
            receiver_call_name: None,
            receiver_type: Some(InferredType::ClassName("Foo".into())),
            receiver_route_defaults: Vec::new(),
            args: vec![
                // Dynamic name — string_value is None, so plugin must skip.
                ArgInfo {
                    text: "$name".into(),
                    string_value: None,
                    string_values: Vec::new(),
                    span: sp(0, 0, 0, 5),
                    content_span: None,
                    inferred_type: None, sub_params: vec![], callable_return_edge: None, ref_sub_name: None, value_shape: Default::default(),
                },
                ArgInfo {
                    text: "sub {}".into(),
                    string_value: None,
                    string_values: Vec::new(),
                    span: sp(0, 6, 0, 12),
                    content_span: None,
                    inferred_type: Some(InferredType::CodeRef { return_edge: None }), sub_params: vec![], callable_return_edge: None, ref_sub_name: None, value_shape: Default::default(),
                },
            ],
            call_span: sp(0, 0, 0, 15),
            selection_span: sp(0, 0, 0, 2),
            current_package: Some("Foo".into()),
            current_package_parents: vec!["Mojo::EventEmitter".into()],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_method_call(&ctx);
        assert!(emissions.is_empty(), "dynamic name must not emit");
    }

    #[test]
    fn rhai_overrides_function_is_read_at_compile_time() {
        // A plugin that defines a top-level `overrides()` function:
        // the host reads it once at load and exposes the list via
        // FrameworkPlugin::overrides — no runtime call cost. This
        // pins the static-manifest contract; if a future refactor
        // moves overrides() to a per-build hook, this test breaks
        // and forces a rethink.
        let src = r#"
            fn id() { "demo-overrides" }
            fn triggers() { [] }
            fn overrides() {
                [
                    #{
                        target: #{ Method: #{ class: "Foo", name: "bar" } },
                        return_type: #{ ClassName: "Foo" },
                        reason: "test",
                    }
                ]
            }
        "#;
        let engine = Arc::new(make_engine());
        let plugin = RhaiPlugin::from_source(src, engine).expect("compiles");
        let ovs = plugin.overrides();
        assert_eq!(ovs.len(), 1);
        match &ovs[0].target {
            crate::plugin::OverrideTarget::Method { class, name } => {
                assert_eq!(class, "Foo");
                assert_eq!(name, "bar");
            }
            other => panic!("expected Method target, got {:?}", other),
        }
        assert_eq!(
            ovs[0].return_type,
            InferredType::ClassName("Foo".into())
        );
        assert_eq!(ovs[0].reason, "test");
    }

    #[test]
    fn rhai_overrides_missing_function_yields_empty() {
        // Plugins without an `overrides()` function must still load.
        // The default registry uses this default for every plugin
        // that doesn't ship overrides — silent absence, not error.
        let src = r#"
            fn id() { "no-overrides" }
            fn triggers() { [] }
        "#;
        let engine = Arc::new(make_engine());
        let plugin = RhaiPlugin::from_source(src, engine).expect("compiles");
        assert!(plugin.overrides().is_empty());
    }

    #[test]
    fn catalyst_plugin_loads_and_has_overrides() {
        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "catalyst")
            .expect("catalyst plugin is bundled");

        // Static overrides manifest must declare at least req/res/stash.
        let ovs = plugin.overrides();
        assert!(!ovs.is_empty(), "catalyst must ship return-type overrides");

        let has_req = ovs.iter().any(|o| {
            matches!(&o.target, crate::plugin::OverrideTarget::Method { class, name }
                if class == "Catalyst" && name == "req")
                && o.return_type == InferredType::ClassName("Catalyst::Request".into())
        });
        assert!(has_req, "missing req → Catalyst::Request override");

        let has_res = ovs.iter().any(|o| {
            matches!(&o.target, crate::plugin::OverrideTarget::Method { class, name }
                if class == "Catalyst" && name == "res")
                && o.return_type == InferredType::ClassName("Catalyst::Response".into())
        });
        assert!(has_res, "missing res → Catalyst::Response override");

        let has_stash = ovs.iter().any(|o| {
            matches!(&o.target, crate::plugin::OverrideTarget::Method { class, name }
                if class == "Catalyst" && name == "stash")
                && o.return_type == InferredType::HashRef
        });
        assert!(has_stash, "missing stash → HashRef override");

        let has_log = ovs.iter().any(|o| {
            matches!(&o.target, crate::plugin::OverrideTarget::Method { class, name }
                if class == "Catalyst" && name == "log")
                && o.return_type == InferredType::ClassName("Catalyst::Log".into())
        });
        assert!(has_log, "missing log → Catalyst::Log override");
    }

    #[test]
    fn catalyst_model_call_emits_method_call_ref() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "catalyst")
            .expect("catalyst is bundled");

        let name_span = sp(5, 20, 5, 23);
        let ctx = CallContext {
            call_kind: CallKind::Method,
            function_name: None,
            method_name: Some("model".into()),
            receiver_text: Some("$c".into()),
            receiver_call_name: None,
            receiver_type: Some(InferredType::ClassName("Catalyst".into())),
            receiver_route_defaults: vec![],
            args: vec![
                ArgInfo {
                    text: "'Foo'".into(),
                    string_value: Some("Foo".into()),
                    string_values: Vec::new(),
                    span: name_span,
                    content_span: Some(name_span),
                    inferred_type: Some(InferredType::String),
                    sub_params: vec![],
                    callable_return_edge: None,
                    ref_sub_name: None, value_shape: Default::default(),
                },
            ],
            call_span: sp(5, 10, 5, 26),
            selection_span: sp(5, 13, 5, 18),
            current_package: Some("MyApp::Controller::Root".into()),
            current_package_parents: vec!["Catalyst::Controller".into()],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_method_call(&ctx);
        // A MethodCallRef pointing at Foo::new (the component class).
        let has_ref = emissions.iter().any(|e| {
            matches!(e, EmitAction::MethodCallRef { method_name, invocant, .. }
                if method_name == "new" && invocant == "Foo")
        });
        assert!(has_ref,
            "model('Foo') must emit MethodCallRef into class Foo; got: {:?}", emissions);
    }

    #[test]
    fn catalyst_forward_emits_dispatch_call() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "catalyst")
            .expect("catalyst is bundled");

        let path_span = sp(7, 20, 7, 38);
        let ctx = CallContext {
            call_kind: CallKind::Method,
            function_name: None,
            method_name: Some("forward".into()),
            receiver_text: Some("$c".into()),
            receiver_call_name: None,
            receiver_type: Some(InferredType::ClassName("Catalyst".into())),
            receiver_route_defaults: vec![],
            args: vec![
                ArgInfo {
                    text: "'/Root/index'".into(),
                    string_value: Some("/Root/index".into()),
                    string_values: Vec::new(),
                    span: path_span,
                    content_span: Some(path_span),
                    inferred_type: Some(InferredType::String),
                    sub_params: vec![],
                    callable_return_edge: None,
                    ref_sub_name: None, value_shape: Default::default(),
                },
            ],
            call_span: sp(7, 10, 7, 40),
            selection_span: sp(7, 13, 7, 20),
            current_package: Some("MyApp::Controller::Root".into()),
            current_package_parents: vec!["Catalyst::Controller".into()],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_method_call(&ctx);
        let has_dispatch = emissions.iter().any(|e| {
            matches!(e, EmitAction::DispatchCall { name, dispatcher, .. }
                if name == "/Root/index" && dispatcher == "forward")
        });
        assert!(has_dispatch,
            "forward('/Root/index') must emit DispatchCall; got: {:?}", emissions);
    }

    #[test]
    fn catalyst_skips_non_catalyst_receiver() {
        use crate::plugin::{ArgInfo, CallKind};

        let engine = Arc::new(make_engine());
        let bundled = load_bundled(engine);
        let plugin = bundled
            .into_iter()
            .find(|p| p.id() == "catalyst")
            .expect("catalyst is bundled");

        // `$schema->model('Foo')` — DBIx::Class::Schema, NOT Catalyst.
        // The plugin must not emit for non-Catalyst receivers.
        let ctx = CallContext {
            call_kind: CallKind::Method,
            function_name: None,
            method_name: Some("model".into()),
            receiver_text: Some("$schema".into()),
            receiver_call_name: None,
            receiver_type: Some(InferredType::ClassName("DBIx::Class::Schema".into())),
            receiver_route_defaults: vec![],
            args: vec![
                ArgInfo {
                    text: "'Foo'".into(),
                    string_value: Some("Foo".into()),
                    string_values: Vec::new(),
                    span: sp(0, 0, 0, 5),
                    content_span: None,
                    inferred_type: Some(InferredType::String),
                    sub_params: vec![],
                    callable_return_edge: None,
                    ref_sub_name: None, value_shape: Default::default(),
                },
            ],
            call_span: sp(0, 0, 0, 20),
            selection_span: sp(0, 10, 0, 15),
            current_package: Some("MyApp::Controller::Root".into()),
            current_package_parents: vec!["Catalyst::Controller".into()],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };

        let emissions = plugin.on_method_call(&ctx);
        assert!(emissions.is_empty(),
            "non-Catalyst receiver must not emit; got: {:?}", emissions);
    }

    #[test]
    fn non_matching_function_returns_empty() {
        let src = r#"
            fn id() { "demo2" }
            fn triggers() { [ #{ Always: () } ] }
            fn on_function_call(ctx) {
                if ctx.function_name == "wanted" { return [#{ FrameworkImport: #{ keyword: "ok" } }]; }
                []
            }
        "#;
        let engine = Arc::new(make_engine());
        let plugin = RhaiPlugin::from_source(src, engine).unwrap();

        let ctx = CallContext {
            call_kind: super::super::CallKind::Function,
            function_name: Some("unrelated".into()),
            method_name: None,
            receiver_text: None,
            receiver_call_name: None,
            receiver_type: None,
            receiver_route_defaults: Vec::new(),
            args: vec![],
            call_span: sp(0, 0, 0, 0),
            selection_span: sp(0, 0, 0, 0),
            current_package: None,
            current_package_parents: vec![],
            current_package_uses: vec![],
            has_options: None,
            arg_names: Vec::new(),
            receiver_is_package: false,
        };
        assert!(plugin.on_function_call(&ctx).is_empty());
    }
}
