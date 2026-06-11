// mojo-routes: `$r->get('/path')->to('Controller#action')` binds a URL
// pattern to a controller method. The `'Controller#action'` string is a
// real cross-file method reference hiding inside string data — gd on
// it should jump to Controller::action, gr on Controller::action should
// surface this route as a caller.
//
// This is the first plugin that consumes a string arg as a METHOD
// REFERENCE rather than a Handler name. Two different flavors of
// string-to-semantics — that's why the abstractions stay split:
//   * `->emit('name', ...)` → DispatchCall → Handler (string-dispatched)
//   * `->to('Controller#action')` → MethodCallRef → regular Method
//
// Triggers on any package that looks like a Mojolicious app/controller
// — the route builder is `Mojolicious::Routes::Route` but the call
// site is usually inside the app's startup. For a first pass, fire on
// `use Mojolicious` families and Lite.

fn id() { "mojo-routes" }

fn triggers() {
    [
        #{ UsesModule: "Mojolicious::Lite" },
        #{ UsesModule: "Mojolicious" },
        #{ ClassIsa: "Mojolicious" },
        #{ ClassIsa: "Mojolicious::Controller" },
    ]
}

// Static return-type assertions for sub/methods inference can't reach.
// Read once at plugin load, applied at every build's post-pass — see
// `Builder::apply_type_overrides`. Trigger-independent: the override
// for `Mojolicious::Routes::Route::_route` fires whenever the home
// module is built, even though `_route` doesn't appear inside a
// triggered package.
//
// Why `_route`: it returns `$self` via an `@_`-shift / array-slice
// pattern that the inference engine doesn't model. Without this, every
// chained route builder (`$r->_route('GET')->to('...')->name('...')`)
// loses its receiver type after the first hop and the rest of the
// chain stops resolving.
// Every route-builder verb returns a `Mojolicious::Routes::Route` (the
// child route for `get`/`any`/`under`/…, `$self` for the fluent
// `to`/`name`/`over`/…). They're declared here, not inferred, because
// the real bodies live in external Mojolicious (@INC) and route via
// `_route`/`add_child` through @_-shift / array-slice patterns the
// engine doesn't model — so without these the receiver loses its Route
// type after the first hop and a partial `->to('#action')` never gets a
// brand to inherit from. `apply_type_overrides` publishes each as a
// class-keyed `MethodOnClass` fact, so the chain types even when
// Mojolicious isn't indexed (rule #8: the framework plugin owns the
// rule; core dispatches it generically — no verb name-allowlist in core,
// rule #10). `routes` on the app/controller surface hands back the root
// Route that seeds the whole chain.
fn route_method_override(class, name, return_class, reason) {
    #{
        target: #{ Method: #{ class: class, name: name }},
        return_type: #{ ClassName: return_class },
        reason: reason,
    }
}

fn overrides() {
    let route_verbs = [
        "_route", "route", "any", "get", "post", "put", "patch", "delete",
        "options", "under", "to", "name", "websocket", "bridge", "via",
        "over", "requires", "methods", "add_child",
    ];
    let reason = "Mojolicious route builder returns a Route ($self / child); bodies live in @INC and shift @_";
    let out = [];
    // The verb surface lives on `Mojolicious::Routes::Route` and the
    // router `Mojolicious::Routes` (which `isa` Route in real Mojo).
    // Declared on BOTH so the chain types even when Mojolicious isn't
    // indexed — without an indexed copy there are no inheritance edges
    // to carry the verbs down from Route to the router.
    for v in route_verbs {
        out += route_method_override("Mojolicious::Routes::Route", v, "Mojolicious::Routes::Route", reason);
        out += route_method_override("Mojolicious::Routes", v, "Mojolicious::Routes::Route", reason);
    }
    // The chain root: `$app->routes` / `$c->routes` hand back the
    // top-level router (`Mojolicious::Routes`, NOT an individual Route —
    // forcing Route here would mistype the container). This is what
    // seeds `my $r = $self->routes; $r->any(...)` so the brand chain can
    // start. Real indexed Mojo declares the same; the override makes it
    // hold without a vendored copy.
    out += route_method_override("Mojolicious", "routes", "Mojolicious::Routes",
        "Mojolicious app `routes` accessor returns the top-level router");
    out += route_method_override("Mojolicious::Controller", "routes", "Mojolicious::Routes",
        "Mojolicious controller `routes` accessor returns the top-level router");
    // Framework-assigned attrs: `has [qw(app tx)]` and friends are
    // declared with NO default/isa (the framework assigns them at
    // dispatch), so the synthesized accessors carry no return type and
    // chains die at the first hop. Declared here as overrides — the
    // same trick as the route verbs above — so `$c->app->…`,
    // `$c->tx->…`, `$plugin_app->…` type with or without an indexed
    // Mojo source tree.
    let assigned = [
        ["Mojolicious::Controller", "app", "Mojolicious"],
        ["Mojolicious::Controller", "tx", "Mojo::Transaction::HTTP"],
        ["Mojolicious::Plugin", "app", "Mojolicious"],
        ["Mojo::Server", "app", "Mojolicious"],
        ["Mojolicious", "ua", "Mojo::UserAgent"],
        ["Mojolicious::Controller", "ua", "Mojo::UserAgent"],
        ["Mojo::UserAgent", "transactor", "Mojo::UserAgent::Transactor"],
        // req/res delegate through $tx in real source (`sub req {
        // shift->tx->req }`); declared on BOTH the controller and the
        // transaction (route-verbs pattern) so the chain types without
        // an indexed Mojo tree, and `$tx->req->…` works wherever a
        // transaction shows up (ua callbacks, hooks, proxies).
        ["Mojolicious::Controller", "req", "Mojo::Message::Request"],
        ["Mojolicious::Controller", "res", "Mojo::Message::Response"],
        ["Mojo::Transaction", "req", "Mojo::Message::Request"],
        ["Mojo::Transaction", "res", "Mojo::Message::Response"],
        ["Mojo::Transaction::HTTP", "req", "Mojo::Message::Request"],
        ["Mojo::Transaction::HTTP", "res", "Mojo::Message::Response"],
    ];
    for a in assigned {
        out += route_method_override(a[0], a[1], a[2],
            "framework-assigned attr (has with no default; set at dispatch)");
    }
    out
}

// `register($self, $app, $conf)` — the contract method every
// Mojolicious plugin implements; the framework passes the app. $conf's
// call-site shape is future work (long-distance param typing).
fn param_types() {
    [
        #{ method: "register", in_role: "Mojolicious::Plugin", param: 1,
           type_class: "Mojolicious" },
        // $conf: typed from whatever the loader call passed
        // (`plugin 'X', {...}` — the PluginLoad fact carries the arg
        // span). HashRef is the no-caller static fallback;
        // structure-dominates-rep prefers the gathered shape.
        #{ method: "register", in_role: "Mojolicious::Plugin", param: 2,
           type_class: "HashRef", from_loader_config: true },
    ]
}

// A route is a first-class thing — it has identity (the `Controller#action`
// pair), an owning app/package, and a target method. Emitting only the
// MethodCallRef would make routes "just a ref into Users.pm" which is
// wrong on two counts:
//   * the route has no presence in the outline / workspace symbols
//   * `gr` on Users::list misses route declarations that target it
//     (fixable via the MethodCallRef alone, but only one side of the pair)
//
// So each `->to(...)` emits TWO things:
//   1. MethodCallRef  — the cross-file link into the target method
//      (goto-def, hover, rename on the string argument all compose via
//      the normal method-resolution path).
//   2. Handler        — the route itself, symbolic. `name` is the
//      `Controller#action` identity, `owner` is the current package
//      (where the route is declared), `dispatchers` includes `url_for`
//      so completion offers registered routes. Outline + workspace
//      symbol + workspace rename now see routes as entities.
//
// Mirrors mojo-lite's model where top-level `get '/path' => sub {}` also
// emits a Handler — same abstraction, different syntactic flavor.

// Build and return the two-emission array for a route: the target
// MethodCallRef + the route-as-thing Handler. Called when both the
// controller and action have been resolved from either call shape.
fn route_emissions(controller, action, span, current_package) {
    let items = [];
    if controller == "" || action == "" { return items; }

    // (a) cross-file method reference
    items += #{
        MethodCallRef: #{
            method_name: action,
            invocant: controller,
            span: span,
            invocant_span: (),
        }
    };

    // (b) route-as-thing Handler, owned by Mojolicious::Controller.
    // Mojo's url_for/redirect_to are Controller methods that resolve
    // against a global routes table — so a controller of ANY subclass
    // reaches the same routes. Owning on Controller (not the declaring
    // package) lets `$c->url_for(...)` inside a sibling controller
    // find routes registered in the app file. Not yet app-scoped —
    // once we distinguish multiple apps in a workspace, owners can
    // narrow to the specific app.
    //
    // Display is `Action`, not `Route` — this emission represents the
    // controller action bound by `->to('Users#list')`, not the route
    // itself. No request body lives at this source site; it's a
    // cross-reference into Users::list. Surfacing as `<action>
    // Users#list` in the outline keeps it visually distinct from
    // `<route> GET /path` entries (mojo-lite terminal handlers),
    // which DO carry a body.
    let owner_class = "Mojolicious::Controller";
    let handler_name = controller + "#" + action;
    items += #{
        Handler: #{
            name: handler_name,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for", "redirect_to"],
            params: [],
            span: span,
            selection_span: span,
            display: "Action",
            hide_in_outline: false,
        }
    };

    // Register the route into the Mojolicious::Controller routes
    // namespace. Bridge to Controller so `for_each_entity_bridged_to(
    // "Mojolicious::Controller", ...)` sees every route Handler in the
    // workspace — that's what powers `$c->url_for('|')` completion in
    // ANY controller. Namespace id stays keyed to the declaring
    // package so multi-app workspaces track each app's routes
    // separately for future app-scoping; the bridge is what controls
    // visibility.
    items += #{
        PluginNamespace: #{
            id: "mojo-routes:" + current_package,
            kind: "routes",
            bridges: [ #{ Class: "Mojolicious::Controller" } ],
            entity_names: [handler_name],
            decl_span: span,
        }
    };

    items
}

// `$r->get('/path')->to('Ctrl#act')->name('users_list')` — the
// `->name()` call gives the route a symbolic handle that users then
// pass to `url_for`/`redirect_to`. By the time the plugin sees this
// call the route builder chain has already typed, but we can't
// reliably walk backward to confirm. Pragmatic: in a Mojolicious-ish
// package, treat every `->name('X')` with a single string arg as a
// route-name registration. If the user happens to call `.name()` on
// something that isn't a route, url_for completion will offer an
// extra name — minor false positive, not a regression.
fn emit_route_name(ctx, name, span) {
    // Handler owned by Mojolicious::Controller (same reasoning as
    // route_emissions): url_for/redirect_to are Controller methods
    // that hit a global routes table, so the Handler belongs on the
    // shared base, not the declaring package.
    let owner_class = "Mojolicious::Controller";
    let decl_pkg = if ctx.current_package == () { "main" } else { ctx.current_package };
    let items = [];
    items += #{
        Handler: #{
            name: name,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for", "redirect_to"],
            params: [],
            span: span,
            selection_span: span,
            display: "Route",
            hide_in_outline: false,
        }
    };
    items += #{
        PluginNamespace: #{
            id: "mojo-routes:" + decl_pkg,
            kind: "routes",
            bridges: [ #{ Class: "Mojolicious::Controller" } ],
            entity_names: [name],
            decl_span: span,
        }
    };
    items
}

// Read the inherited controller from the receiver's route brand.
// The builder flattens `InferredType::BrandedRoute` into
// `ctx.receiver_route_defaults` as `[[key, value], ...]`; `controller`
// is the distinguished key. `()` when the receiver carries no brand
// (plain `ClassName(Route)` root, or non-route receiver) — partials on
// an unbranded receiver degrade quietly (no controller, no emission).
fn inherited_controller(ctx) {
    let defs = ctx.receiver_route_defaults;
    if defs == () { return (); }
    for pair in defs {
        if pair[0] == "controller" { return pair[1]; }
    }
    ()
}


// Mojo::Util decamelize: "FooBar" -> "foo_bar", "Foo::Bar" -> "foo-bar".
// Plugin names arrive camelized; the core's controller-token search is
// gated on lowercase-leading tokens (mirroring camelize's /^[A-Z]/
// early-return), so emitting the decamelized form round-trips through
// the exact same machinery Mojo's loader uses.
fn decamelize(name) {
    let out = "";
    let parts = name.split("::");
    let first_part = true;
    for part in parts {
        if !first_part { out += "-"; }
        first_part = false;
        let first = true;
        for ch in part {
            if ch >= 'A' && ch <= 'Z' {
                if !first { out += "_"; }
                out += ch.to_lower();
            } else {
                out += ch;
            }
            first = false;
        }
    }
    out
}

fn on_method_call(ctx) {
    let out = [];

    // `$app->plugin('Thing')` — the OO spelling of the lite `plugin`
    // verb; same register-anchored resolution (see mojo-lite).
    if ctx.method_name == "plugin" {
        let args = ctx.args;
        if args.len() >= 1 {
            let name = args[0].string_value;
            if name != () && name != "" {
                let span = args[0].content_span;
                if span == () { span = args[0].span; }
                out += #{
                    MethodCallRef: #{
                        method_name: "register",
                        invocant: decamelize(name),
                        span: span,
                        invocant_span: (),
                    }
                };
                // "You loaded it" — same as the lite arm: record the
                // load so the resolver pulls the plugin AND the
                // entrypoint-scan lint sees the load fact. Default
                // namespace guess for short names; the lint
                // tail-matches, so app-custom namespaces
                // (Clove::App::Plugin::*) count as loaded too.
                if !name.contains("::") {
                    out += #{ "SyntheticUse": #{ "module": "Mojolicious::Plugin::" + name, "args": [], "imports": [], "span": span }};
                } else {
                    out += #{ "SyntheticUse": #{ "module": name, "args": [], "imports": [], "span": span }};
                }
                // the loader fact: which module, and where its config
                // value sits — the caller half of $conf typing
                let cfg = ();
                if args.len() >= 2 { cfg = args[1].span; }
                out += #{ "PluginLoad": #{ "name": name, "config_span": cfg }};
            }
        }
        return out;
    }

    if ctx.method_name == "name" {
        let args = ctx.args;
        if args.len() != 1 { return out; }
        let name = args[0].string_value;
        if name == () || name == "" { return out; }
        // Use the string_content span — captured by the builder from
        // the `string_content` tree node, so it's already quote-free
        // regardless of literal flavor (`'x'`, `"x"`, `q{x}`, `qq!x!`,
        // heredoc). `content_span` is `()` when the arg isn't a string
        // literal or is empty; fall back to full span in that case.
        let inner_span = args[0].content_span;
        if inner_span == () { inner_span = args[0].span; }
        return emit_route_name(ctx, name, inner_span);
    }

    if ctx.method_name != "to" { return out; }

    let args = ctx.args;
    if args.len() < 1 { return out; }

    // Short form: `'Controller#action'`. Narrow the MethodCallRef span
    // to just the action-name portion so mid-string completion
    // (core-side) can use `source[span_start..cursor]` as the typed
    // prefix directly, without any `#`-aware logic in the core.
    //
    // We subspan INTO the `content_span` (which excludes quote
    // delimiters), not the outer arg span. Offsets are into the
    // string's content, not the raw literal — so a `q{Users#list}`
    // or heredoc-bodied arg works the same as `'Users#list'`.
    let first = args[0].string_value;
    if first != () && first.contains("#") {
        let parts = first.split("#");
        if parts.len() == 2 {
            let controller = parts[0];
            let action = parts[1];
            // Span math is into the ORIGINAL literal — the `#` sits
            // right after the literal's controller substring (length 0
            // for a partial `'#action'`), regardless of what the
            // controller is later inherited to be. Capture it before
            // overwriting `controller` with the inherited value.
            let lit_controller_len = controller.len();
            // Partial target `'#action'`: the controller half is empty,
            // inherited from a parent `->to('ctrl#')` down the builder
            // chain. The brand on the receiver value carries it — read
            // it off `receiver_route_defaults`. `route_emissions`
            // still early-returns if BOTH stay empty (unresolvable).
            if controller == "" {
                let inh = inherited_controller(ctx);
                if inh != () { controller = inh; }
            }
            let base = args[0].content_span;
            if base != () {
                // `#` is right after the literal's controller substring
                // in the content; action starts one column past that.
                let action_start = lit_controller_len + 1;
                let action_end   = action_start + action.len();
                let action_span = subspan_cols(base, action_start, action_end);
                let ems = route_emissions(controller, action, action_span, ctx.current_package);
                // Lite `under(...)->to('ctrl#…')` also WRITES the topic
                // base following sibling routes inherit. The receiver's
                // call name is a generic core fact; "under" is ours.
                if ctx.receiver_call_name == "under" && controller != "" {
                    ems += #{ "SetRouteBase": #{ "controller": controller } };
                }
                return ems;
            }
        }
    }

    // Long form: `->to(controller => 'Users', action => 'list')`.
    // `content_span` gives us the action string's contents directly —
    // no quote math needed.
    let controller = "";
    let action = "";
    let action_span = ();
    let i = 0;
    while i < args.len() - 1 {
        let key = args[i].string_value;
        if key == "controller" {
            let v = args[i + 1].string_value;
            if v != () { controller = v; }
        } else if key == "action" {
            let v = args[i + 1].string_value;
            if v != () {
                action = v;
                action_span = args[i + 1].content_span;
                if action_span == () { action_span = args[i + 1].span; }
            }
        }
        i += 2;
    }

    if action_span != () {
        // Same inheritance as the short form: a long-form target with
        // an `action` but no `controller` reads the inherited one.
        if controller == "" {
            let inh = inherited_controller(ctx);
            if inh != () { controller = inh; }
        }
        return route_emissions(controller, action, action_span, ctx.current_package);
    }

    out
}
