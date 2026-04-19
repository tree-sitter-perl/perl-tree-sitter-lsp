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

    // (b) route-as-thing Handler, owned by the declaring package
    let owner_class = if current_package == () { "main" } else { current_package };
    // Routes display as Methods in the outline — they dispatch to a
    // controller action (a real method elsewhere). Event would be
    // misleading; these aren't things you react to, they're things
    // you *invoke* indirectly.
    // Routes aren't methods — they're named URL patterns that dispatch
    // to a controller action. Rendering them as Function gives them a
    // distinct icon from both Perl methods and events, matching how
    // users think about them (helper-y, framework-y callables).
    items += #{
        Handler: #{
            name: controller + "#" + action,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for"],
            params: [],
            span: span,
            selection_span: span,
            display: "Function",
        }
    };

    items
}

fn on_method_call(ctx) {
    let out = [];

    if ctx.method_name != "to" { return out; }

    let args = ctx.args;
    if args.len() < 1 { return out; }

    // Short form: `'Controller#action'`. We narrow the MethodCallRef
    // span to just the action-name portion so mid-string completion
    // (core-side) can use `source[span_start..cursor]` as the typed
    // prefix directly, without any `#`-aware logic in the core.
    //
    // Position math on the literal `'Users#list'`:
    //   col+0 : opening quote
    //   col+1 : 'U' (start of Controller)
    //   col+1+len(Controller) : '#'
    //   col+2+len(Controller) : 'l' (start of action)
    //   col+2+len(Controller)+len(action) : closing quote
    let first = args[0].string_value;
    if first != () && first.contains("#") {
        let parts = first.split("#");
        if parts.len() == 2 {
            let controller = parts[0];
            let action = parts[1];
            let action_start = 1 + controller.len() + 1;              // quote + ctrl + '#'
            let action_end   = action_start + action.len();
            let action_span = subspan_cols(args[0].span, action_start, action_end);
            return route_emissions(controller, action, action_span, ctx.current_package);
        }
    }

    // Long form: `->to(controller => 'Users', action => 'list')`.
    // For the action string we also tighten the span to exclude the
    // quotes — same reason as above: core-side completion uses
    // `source[span..cursor]` as the typed prefix, so quote characters
    // would leak into prefix matching.
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
                // Narrow to content-only (strip the surrounding quotes).
                action_span = subspan_cols(args[i + 1].span, 1, 1 + action.len());
            }
        }
        i += 2;
    }

    if action_span != () {
        return route_emissions(controller, action, action_span, ctx.current_package);
    }

    out
}
