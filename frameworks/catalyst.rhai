// catalyst: Catalyst MVC framework support.
//
// Two detection contexts covered:
//   * Catalyst app class    — `use Catalyst qw(...)` or `use Catalyst;`
//   * Catalyst controllers  — `use parent 'Catalyst::Controller'` /
//     `use base 'Catalyst::Controller'` / ClassIsa: Catalyst::Controller
//
// PRIMARY GOAL: typing of Catalyst's context object `$c` and its
// accessor family inside action methods.
//
// ── $c parameter typing ──────────────────────────────────────────────
//
// `param_types()` with `method: ()` (wildcard — any method name) types
// the 2nd positional param ($c, param index 1) as `Catalyst` for every
// method in a class that `isa Catalyst::Controller`. The mechanism:
//
//   ParamType { method: None, in_role: "Catalyst::Controller",
//               param: 1, type_class: "Catalyst" }
//
// KNOWN CONSTRAINT — cross-file ClassIsa: `transitive_parents` in the
// builder walks only `package_parents` populated from the current file's
// `use parent` / `use base` / `@ISA` / `with` / `extends` declarations.
// `Catalyst::Controller` is always a CPAN/dependency ancestor — never
// declared locally. So the `in_role` match fires only when the file
// itself carries a `use parent 'Catalyst::Controller'` (or equivalent)
// that gets parsed and recorded in `package_parents`. For files whose
// controller base is declared locally (rare in Catalyst apps — most
// controllers DO have an explicit `use parent 'Catalyst::Controller'`),
// this works correctly. For controllers that inherit transitively via
// a local intermediate, the intermediate would need the same manifest
// applied recursively — but `transitive_parents` does walk the full
// local ancestor chain, so a `MyApp::Controller::Base` with `use parent
// 'Catalyst::Controller'` and a child with `use parent
// 'MyApp::Controller::Base'` WILL both get `$c` typed correctly.
// The only gap is when the parent is cross-file and not in the local
// `package_parents` snapshot. This is a resolver-layer concern
// (see PluginRegistry::applicable / trigger cross-file limitation);
// fixing it requires propagating cross-file parents into the build
// walk, which is a separate architectural change.
//
// ── Component accessors ──────────────────────────────────────────────
//
// `$c->model('Foo')` / `$c->view('Foo')` / `$c->controller('Foo')`
// return the named component instance. The component's full class is
// typically `MyApp::Model::Foo`, `MyApp::View::Foo`, or
// `MyApp::Controller::Foo` — app-namespace-prefixed.
//
// The manifest can express a fixed return type for an entire method
// (overrides()) or a workspace-search-based type (no manifest action
// for that). No manifest action today can express "the return type is
// derived from the first string arg by workspace-namespace lookup".
//
// MANIFEST GAP: `$c->model('Foo')` → `MyApp::Model::Foo` requires
// correlating the string arg against the application's namespace prefix,
// which is a two-level workspace scan (find the app class, derive the
// component namespace). The plugin emits a `MethodCallRef` for the
// component name so gd/gr work if the workspace contains the component
// class. Return-type inference for the component instance is deferred
// to a future `InferredType::Parametric` variant that accepts a
// name-to-class resolver function.
//
// ── Action forwarding ────────────────────────────────────────────────
//
// `$c->forward('/MyApp/Controller/action')` dispatches to another action.
// We emit a DispatchCall so gd on the string lands at action definitions
// registered as Handlers. The forward verbs behave like Catalyst's
// internal `url_for` — they reference action paths, not Perl methods.

fn id() { "catalyst" }

fn triggers() {
    [
        // App classes: `use Catalyst` or `use Catalyst::Plugin::*`
        #{ UsesModule: "Catalyst" },
        // Controller subclasses — the most common context for `$c`
        #{ ClassIsa: "Catalyst::Controller" },
        // Catalyst::Model / View subclasses also carry `$c` in
        // any actions they define, and inherit the component accessor family.
        #{ ClassIsa: "Catalyst::Model" },
        #{ ClassIsa: "Catalyst::View" },
        #{ ClassIsa: "Catalyst::Action" },
        #{ ClassIsa: "Catalyst" },
        // Direct `use` of the common controller base classes.
        #{ UsesModule: "Catalyst::Controller" },
        #{ UsesModule: "Catalyst::Controller::REST" },
        #{ UsesModule: "Catalyst::Controller::ActionRole" },
    ]
}

// Static return-type overrides for Catalyst context accessors.
// These are the high-value methods that let the type-inference chain
// continue once $c is typed as Catalyst.
//
// `overrides()` is trigger-independent: it applies to any file where
// a local symbol matches the target — no need for the current file to
// `use Catalyst` itself (e.g. a base controller that doesn't re-use
// the module).
fn overrides() {
    [
        // Request / response objects — the most-used sub-objects.
        #{
            target: #{ Method: #{ class: "Catalyst", name: "req" } },
            return_type: #{ ClassName: "Catalyst::Request" },
            reason: "req is the standard request-object accessor; the builder can't infer its return from source",
        },
        #{
            target: #{ Method: #{ class: "Catalyst", name: "request" } },
            return_type: #{ ClassName: "Catalyst::Request" },
            reason: "request is the long-form alias for req",
        },
        #{
            target: #{ Method: #{ class: "Catalyst", name: "res" } },
            return_type: #{ ClassName: "Catalyst::Response" },
            reason: "res is the standard response-object accessor",
        },
        #{
            target: #{ Method: #{ class: "Catalyst", name: "response" } },
            return_type: #{ ClassName: "Catalyst::Response" },
            reason: "response is the long-form alias for res",
        },
        // `stash` returns a hashref. No typed class — it's a free-form
        // slot, and there's no per-app stash schema the plugin can know.
        #{
            target: #{ Method: #{ class: "Catalyst", name: "stash" } },
            return_type: type_hashref(),
            reason: "stash is a free-form hashref; no sub-schema to model",
        },
        // Logging. `$c->log` returns Catalyst::Log (or the user-configured
        // logger, but Catalyst::Log is the canonical type for dispatch).
        #{
            target: #{ Method: #{ class: "Catalyst", name: "log" } },
            return_type: #{ ClassName: "Catalyst::Log" },
            reason: "log returns the Catalyst::Log instance (or compatible logger)",
        },
        // Config is a hashref of the application's configuration.
        #{
            target: #{ Method: #{ class: "Catalyst", name: "config" } },
            return_type: type_hashref(),
            reason: "config returns the merged application configuration hashref",
        },
        // User / session (Catalyst::Plugin::Authentication territory, but
        // commonly expected on $c in any real Catalyst app).
        #{
            target: #{ Method: #{ class: "Catalyst", name: "user" } },
            return_type: #{ ClassName: "Catalyst::Authentication::Store::DBIx::Class::User" },
            reason: "user() is the authenticated-user accessor (Plugin::Authentication)",
        },
        // Request convenience accessors (common on Catalyst::Request).
        #{
            target: #{ Method: #{ class: "Catalyst::Request", name: "params" } },
            return_type: type_hashref(),
            reason: "params returns the merged GET+POST parameter hashref",
        },
        #{
            target: #{ Method: #{ class: "Catalyst::Request", name: "body_params" } },
            return_type: type_hashref(),
            reason: "body_params returns the POST-only parameter hashref",
        },
        #{
            target: #{ Method: #{ class: "Catalyst::Request", name: "query_params" } },
            return_type: type_hashref(),
            reason: "query_params returns the GET-only parameter hashref",
        },
        #{
            target: #{ Method: #{ class: "Catalyst::Request", name: "body_data" } },
            return_type: type_hashref(),
            reason: "body_data returns decoded body (JSON/form); defaults to hashref",
        },
    ]
}

// Type the 2nd positional param ($c) as `Catalyst` for every method in a
// Catalyst::Controller subclass. `method: ()` is the wildcard selector —
// any method name matches, so all action subs get the context typed.
//
// Selector approximation note: `in_role` is matched against the local
// `package_parents` snapshot (see KNOWN CONSTRAINT above). This covers the
// common case — a controller with `use parent 'Catalyst::Controller'` or a
// transitive local chain to it. Cross-file-only inheritance misses; that's
// a resolver-layer gap, not a manifest gap.
fn param_types() {
    [
        // Every method in a Catalyst::Controller subclass gets $c typed.
        // `method: ()` = wildcard: applies regardless of the action's name.
        #{ in_role: "Catalyst::Controller", param: 1, type_class: "Catalyst" },
        // Catalyst::Model and Catalyst::View controllers also carry $c when
        // they define action methods (less common, but supported by Catalyst).
        #{ in_role: "Catalyst::Model", param: 1, type_class: "Catalyst" },
        #{ in_role: "Catalyst::View", param: 1, type_class: "Catalyst" },
    ]
}

// ── on_use: imports and context setup ────────────────────────────────
//
// When `use Catalyst` or `use Catalyst::Controller` appears, suppress
// unresolved-function diagnostics for Catalyst's action-attribute verbs
// and common exported helpers. These are typically installed into the
// caller's namespace by Catalyst at import time.
fn on_use(ctx) {
    let m = ctx.module_name;

    // Catalyst app class: `use Catalyst qw(...)` loads plugins listed in
    // the import list. No specific function imports, but action attribute
    // verbs need to be silenced.
    let is_catalyst_app = m == "Catalyst";
    let is_catalyst_ctrl = m == "Catalyst::Controller"
        || m == "Catalyst::Controller::REST"
        || m == "Catalyst::Controller::ActionRole";

    if !is_catalyst_app && !is_catalyst_ctrl { return []; }

    let out = [];

    // Action-attribute verbs. These aren't ordinary function calls in Perl
    // source, but some analysis paths surface them as unresolved. Suppress.
    let attr_verbs = [
        "Local", "Path", "Chained", "PathPart", "CaptureArgs",
        "Args", "Private", "Global", "LocalRegex", "GlobalRegex",
        "ActionClass", "Does", "GET", "POST", "PUT", "DELETE", "PATCH",
    ];
    for v in attr_verbs {
        out += #{ FrameworkImport: #{ keyword: v } };
    }

    // Common Catalyst helpers that appear as bare function calls.
    if is_catalyst_app {
        let app_helpers = [
            "__PACKAGE__", "setup", "config",
        ];
        for h in app_helpers {
            out += #{ FrameworkImport: #{ keyword: h } };
        }
    }

    out
}

// ── on_method_call: component accessors + action dispatch ─────────────
//
// `$c->model('Foo')`, `$c->view('Foo')`, `$c->controller('Foo')`:
// Each emits a MethodCallRef pointing at the component class so gd on
// the string arg jumps to the component's package. No return-type
// inference here (gap documented above) — only the cross-ref.
//
// `$c->forward('/Controller/action')` / `$c->detach(...)` /
// `$c->visit(...)`: the path string is treated as a dispatch target.
// DispatchCall ref so gd/gr pair the forward site with action Handler defs.
fn on_method_call(ctx) {
    let out = [];
    let m = ctx.method_name;
    if m == () { return out; }

    // Gate on receiver being Catalyst-typed when we can confirm it.
    // When unknown (unresolved $c), still fire — Catalyst method names
    // are distinctive enough and the trigger already narrowed us to
    // Catalyst-flavored packages.
    let rtype = ctx.receiver_type;
    let receiver_is_catalyst = rtype == ()
        || (rtype.contains("ClassName")
            && (rtype.ClassName == "Catalyst"
                || rtype.ClassName.starts_with("Catalyst::")));

    if !receiver_is_catalyst { return out; }

    let args = ctx.args;

    // ── Component accessors ──────────────────────────────────────────
    // `$c->model('Foo')` / `$c->view('Foo')` / `$c->controller('Foo')`
    // The string arg is a component name. We emit a MethodCallRef so
    // the string is a real reference to the component class — gd on
    // `'Foo'` in `$c->model('Foo')` should navigate to `MyApp::Model::Foo`.
    // The invocant for the MethodCallRef is the component class name
    // itself (we treat the string as a class name, not a method name —
    // using `new` as a synthetic anchor for "the class exists").
    let is_component_accessor = m == "model" || m == "view" || m == "controller";
    if is_component_accessor && args.len() >= 1 {
        let component_name = args[0].string_value;
        if component_name != () && component_name != "" {
            // Emit a MethodCallRef whose method_name is `new` and whose
            // invocant IS the component class string. This is the same
            // pattern as mojo-routes `->to('Controller#action')` — we
            // treat the string as a direct class reference, not a method.
            // Prefer the content_span (inside quotes) if available.
            let name_span = args[0].content_span;
            if name_span == () { name_span = args[0].span; }
            out += #{
                MethodCallRef: #{
                    method_name: "new",
                    invocant: component_name,
                    span: name_span,
                    invocant_span: name_span,
                }
            };
        }
    }

    // ── Action dispatch verbs ─────────────────────────────────────────
    // `$c->forward('/Root/index')` / `$c->detach(...)` / `$c->visit(...)`
    // The path string references an action. Emit DispatchCall so gr on
    // action definitions finds dispatch sites.
    let is_dispatch = m == "forward" || m == "detach" || m == "visit";
    if is_dispatch && args.len() >= 1 {
        let action_path = args[0].string_value;
        if action_path != () && action_path != "" {
            let path_span = args[0].content_span;
            if path_span == () { path_span = args[0].span; }
            // Owner: Catalyst (the context class). All actions share one
            // dispatch table regardless of which controller defines them.
            out += #{
                DispatchCall: #{
                    name: action_path,
                    dispatcher: m,
                    owner: #{ Class: "Catalyst" },
                    span: path_span,
                    var_text: if ctx.receiver_text == () { "" } else { ctx.receiver_text },
                }
            };
        }
    }

    out
}
