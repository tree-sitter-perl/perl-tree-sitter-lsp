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

use super::{CallContext, EmitAction, FrameworkPlugin, Trigger, UseContext};

/// An engine built with our helpers and type registrations. Engines are
/// cheap to reuse; scripts share one instance across all callbacks.
pub fn make_engine() -> Engine {
    let mut engine = Engine::new();
    engine.set_max_expr_depths(64, 64);

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
    engine.register_fn("type_coderef", || to_dynamic(InferredType::CodeRef).unwrap());
    engine.register_fn("type_regexp", || to_dynamic(InferredType::Regexp).unwrap());
    engine.register_fn("type_class", |class: String| {
        to_dynamic(InferredType::ClassName(class)).unwrap_or(Dynamic::UNIT)
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

    engine
}

pub struct RhaiPlugin {
    id: String,
    triggers: Vec<Trigger>,
    engine: Arc<Engine>,
    ast: Arc<AST>,
    has_on_function_call: bool,
    has_on_method_call: bool,
    has_on_use: bool,
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

        Ok(Self {
            has_on_function_call: signatures.iter().any(|n| n == "on_function_call"),
            has_on_method_call: signatures.iter().any(|n| n == "on_method_call"),
            has_on_use: signatures.iter().any(|n| n == "on_use"),
            id,
            triggers,
            engine,
            ast: Arc::new(ast),
        })
    }

    fn dispatch(&self, fn_name: &str, arg: Dynamic) -> Vec<EmitAction> {
        let out: Result<Array, _> =
            self.engine.call_fn(&mut rhai::Scope::new(), &self.ast, fn_name, (arg,));
        let arr = match out {
            Ok(a) => a,
            Err(e) => {
                log::warn!("plugin `{}`::{} failed: {}", self.id, fn_name, e);
                return Vec::new();
            }
        };
        arr.into_iter()
            .filter_map(|d| {
                from_dynamic::<EmitAction>(&d)
                    .map_err(|e| {
                        log::warn!(
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
            receiver_type: None,
            args: vec![],
            call_span: sp(0, 0, 0, 5),
            selection_span: sp(0, 0, 0, 5),
            current_package: Some("Demo::App".into()),
            current_package_parents: vec![],
            current_package_uses: vec!["Demo".into()],
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
            receiver_type: Some(InferredType::ClassName("My::Emitter".into())),
            args: vec![
                ArgInfo {
                    text: "'connect'".into(),
                    string_value: Some("connect".into()),
                    span: evt_span,
                    inferred_type: Some(InferredType::String), sub_params: vec![],
                },
                ArgInfo {
                    text: "sub { ... }".into(),
                    string_value: None,
                    span: cb_span,
                    inferred_type: Some(InferredType::CodeRef), sub_params: vec![],
                },
            ],
            call_span: sp(3, 4, 3, 45),
            selection_span: sp(3, 10, 3, 12),
            current_package: Some("My::Emitter".into()),
            current_package_parents: vec!["Mojo::EventEmitter".into()],
            current_package_uses: vec![],
        };

        let emissions = plugin.on_method_call(&ctx);
        // DispatchCall (ref) + Handler (def).
        assert_eq!(emissions.len(), 2, "dispatch call + handler; got: {:?}", emissions);

        let has_dispatch = emissions.iter().any(|e| {
            matches!(e, EmitAction::DispatchCall { name, dispatcher, .. }
                if name == "connect" && dispatcher == "on")
        });
        assert!(has_dispatch, "missing DispatchCall for 'connect' via ->on");

        let has_handler = emissions.iter().any(|e| {
            matches!(e, EmitAction::Handler { name, .. } if name == "connect")
        });
        assert!(has_handler, "missing Handler symbol for 'connect'");
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
            receiver_type: Some(InferredType::ClassName("Foo".into())),
            args: vec![
                // Dynamic name — string_value is None, so plugin must skip.
                ArgInfo {
                    text: "$name".into(),
                    string_value: None,
                    span: sp(0, 0, 0, 5),
                    inferred_type: None, sub_params: vec![],
                },
                ArgInfo {
                    text: "sub {}".into(),
                    string_value: None,
                    span: sp(0, 6, 0, 12),
                    inferred_type: Some(InferredType::CodeRef), sub_params: vec![],
                },
            ],
            call_span: sp(0, 0, 0, 15),
            selection_span: sp(0, 0, 0, 2),
            current_package: Some("Foo".into()),
            current_package_parents: vec!["Mojo::EventEmitter".into()],
            current_package_uses: vec![],
        };

        let emissions = plugin.on_method_call(&ctx);
        assert!(emissions.is_empty(), "dynamic name must not emit");
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
            receiver_type: None,
            args: vec![],
            call_span: sp(0, 0, 0, 0),
            selection_span: sp(0, 0, 0, 0),
            current_package: None,
            current_package_parents: vec![],
            current_package_uses: vec![],
        };
        assert!(plugin.on_function_call(&ctx).is_empty());
    }
}
