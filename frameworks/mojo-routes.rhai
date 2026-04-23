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

    // (b) route-as-thing Handler, owned by the declaring package.
    // Display is `Action`, not `Route` — this emission represents the
    // controller action bound by `->to('Users#list')`, not the route
    // itself. No request body lives at this source site; it's a
    // cross-reference into Users::list. Surfacing as `<action>
    // Users#list` in the outline keeps it visually distinct from
    // `<route> GET /path` entries (mojo-lite terminal handlers),
    // which DO carry a body.
    let owner_class = if current_package == () { "main" } else { current_package };
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

    // Register the route into a per-app routes namespace. Bridges to
    // the declaring package so workspace tooling that walks
    // `for_each_entity_bridged_to(owner_class, ...)` sees the route
    // Handlers. One namespace per declaring package; multiple routes
    // accumulate entities under the same id.
    items += #{
        PluginNamespace: #{
            id: "mojo-routes:" + owner_class,
            kind: "routes",
            bridges: [ #{ Class: owner_class } ],
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
    let owner_class = if ctx.current_package == () { "main" } else { ctx.current_package };
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
            id: "mojo-routes:" + owner_class,
            kind: "routes",
            bridges: [ #{ Class: owner_class } ],
            entity_names: [name],
            decl_span: span,
        }
    };
    items
}

fn on_method_call(ctx) {
    let out = [];

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
            let base = args[0].content_span;
            if base != () {
                // `#` is right after the controller substring in the
                // content; action starts one column past that.
                let action_start = controller.len() + 1;
                let action_end   = action_start + action.len();
                let action_span = subspan_cols(base, action_start, action_end);
                return route_emissions(controller, action, action_span, ctx.current_package);
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
        return route_emissions(controller, action, action_span, ctx.current_package);
    }

    out
}
