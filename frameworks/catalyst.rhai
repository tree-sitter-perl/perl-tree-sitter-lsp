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
// `param_types()` with `method: ()` (wildcard) + `requires_action_attr`
// types the 2nd positional param ($c, param index 1) as `Catalyst` — but
// ONLY for methods carrying an action attribute (`:Local` / `:Chained` /
// `:Args` / …). The attribute is the only honest signal that a sub is a
// dispatch *action* (the slot Catalyst passes `$c` into); a bare wildcard
// would type the 2nd param of every helper too (`$page` in a paginator,
// `$path` in a request builder). Catalyst::Model uses `ACCEPT_CONTEXT`
// instead of attributes, so it gets a *named* rule with no attr gate.
//
//   ParamType { method: None, in_role: "Catalyst::Controller", param: 1,
//               type_class: "Catalyst", requires_action_attr: true }
//
// Cross-file ancestry: the `in_role` match is resolved at QUERY time via
// the `ReceiverGated` seam (the builder is index-free, rule #1). The gated
// TC rides the FA ungated; `gated_param_type_for` → `class_isa` walks the
// enclosing package's local `package_parents` ∪ the cross-file
// `parents_cached` graph — so a controller whose `Catalyst::Controller`
// ancestor is reached through a *workspace intermediate* base (e.g.
// `MetaCPAN::Web::Controller::Author` → `MetaCPAN::Web::Controller` →
// `Catalyst::Controller`) types its `$c` correctly, provided the query
// caller threads the module index (hover / completion / dump all do).
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

// Type the 2nd positional param ($c) as `Catalyst` in Catalyst::Controller
// subclasses. The wildcard rules carry `requires_action_attr: true`, so they
// only fire on subs with an action attribute (`:Local`/`:Chained`/`:Args`/…)
// — actual dispatch actions, the only subs Catalyst hands `$c`. A plain
// helper (`sub pageset { my ($self, $page) = @_; ... }`) has no attribute, so
// its 2nd param is left untyped.
//
// `in_role` ancestry resolves cross-file at query time through the
// `ReceiverGated` seam (`class_isa` over local + `parents_cached`), so a
// controller reaching `Catalyst::Controller` only through a workspace
// intermediate base still types `$c`.
fn param_types() {
    [
        // Controller actions: gated on the attribute the dispatcher requires.
        #{ in_role: "Catalyst::Controller", param: 1, type_class: "Catalyst",
           requires_action_attr: true },
        // Catalyst::View action methods follow the same attribute convention.
        #{ in_role: "Catalyst::View", param: 1, type_class: "Catalyst",
           requires_action_attr: true },
        // Catalyst::Model gets the context through `ACCEPT_CONTEXT($self, $c)`
        // / `COMPONENT($self, $app)` — fixed contract method names, no action
        // attribute — so these are *named* rules, not the attribute-gated
        // wildcard. (`COMPONENT`'s 2nd arg is the application object, also a
        // `Catalyst`-flavored context for our purposes.)
        #{ method: "ACCEPT_CONTEXT", in_role: "Catalyst::Model", param: 1,
           type_class: "Catalyst" },
        #{ method: "COMPONENT", in_role: "Catalyst::Model", param: 1,
           type_class: "Catalyst" },
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
