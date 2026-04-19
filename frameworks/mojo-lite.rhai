// mojo-lite: top-level route declarations in Mojolicious::Lite scripts.
//
//   get  '/users' => sub { my $c = shift; ... };
//   post '/login' => sub { ... };
//   any  '/fallback' => sub { ... };
//
// Each route registers a handler keyed by the URL path. Modeling:
//   * Handler symbol named = URL path
//   * owner = Class(current package)  (Lite scripts are usually `main`)
//   * dispatchers = ["url_for"]  (Mojo's route-name-to-URL helper)
//   * params = handler sub's signature (first param is $c:
//     Mojolicious::Controller; the handler sig stripping already
//     collapses invocant-like first args at display time)
//
// Outline surfaces every route as an EVENT symbol with its path +
// handler params. Completion for `url_for('|')` shows every known
// route in the workspace at top priority. The plugin never has to
// wire any of those features — they fall out of `SymKind::Handler`
// machinery.

fn id() { "mojo-lite" }

fn triggers() {
    [
        #{ UsesModule: "Mojolicious::Lite" },
    ]
}

// `use Mojolicious::Lite` autoimports a fixed verb set. Without this,
// every `get`/`post`/`helper`/etc. call in the Lite script trips the
// unresolved-function diagnostic. We emit a FrameworkImport per verb
// so the diagnostic skip list has them.
fn on_use(ctx) {
    if ctx.module_name != "Mojolicious::Lite" { return []; }
    let verbs = [
        "get", "post", "put", "del", "patch", "options", "any",
        "under", "websocket",
        "app", "helper", "hook", "plugin",
        "group",
    ];
    let out = [];
    for v in verbs {
        out += #{ FrameworkImport: #{ keyword: v } };
    }
    out
}

fn on_function_call(ctx) {
    let out = [];

    // Route-declaration verbs. `under` is a middleware-ish route
    // wrapper; `websocket` registers a WS handler. Both behave like
    // handler registrations for our purposes.
    let verbs = [
        "get", "post", "put", "del", "patch", "any", "under", "websocket",
    ];
    let fname = ctx.function_name;
    if fname == () { return out; }
    let is_route_verb = false;
    for v in verbs {
        if v == fname { is_route_verb = true; break; }
    }
    if !is_route_verb { return out; }

    let args = ctx.args;
    if args.len() < 1 { return out; }

    // Path is the first literal string. Dynamic patterns skip.
    let path = args[0].string_value;
    if path == () { return out; }

    // Owner: the current package is the Lite app's namespace (often
    // `main` for Lite scripts). That's what url_for resolves against
    // at the framework level.
    let owner_class = ctx.current_package;
    if owner_class == () { return out; }

    // Find the handler sub arg. In Lite syntax `get '/x' => sub {...}`
    // the sub is args[1]; but other forms (e.g. `get '/x' => 'Ctrl#act'`
    // with string target) are handled by mojo-routes — skip here.
    let params = [];
    if args.len() >= 2 {
        params = args[1].sub_params;
    }

    // Lite routes render as Function — same reasoning as mojo-routes:
    // they're framework-synthesized URL callables, not Perl methods.
    out += #{
        Handler: #{
            name: path,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for", "redirect_to"],
            params: params,
            span: args[0].span,
            selection_span: args[0].span,
            display: "Function",
        }
    };

    out
}

// Dispatchers that take a URL string and resolve to a Lite route.
// `url_for` generates a URL for a named route; `redirect_to` issues
// a 302 to the given path. Both make the string argument a real
// reference to the route's Handler — gd/gr/hover all compose.
fn on_method_call(ctx) {
    let out = [];
    let mname = ctx.method_name;
    if mname == () { return out; }

    // Rhai: top-level `const` arrays don't hoist into fn scope
    // reliably; keep the list inline here.
    let dispatchers = ["url_for", "redirect_to"];
    let is_url_dispatch = false;
    for d in dispatchers {
        if d == mname { is_url_dispatch = true; break; }
    }
    if !is_url_dispatch { return out; }

    let args = ctx.args;
    if args.len() < 1 { return out; }

    let path = args[0].string_value;
    if path == () { return out; }

    // Owner: routes are registered under the Lite app's own package.
    // `current_package` at the call site is usually `main` (Lite) or
    // whichever controller is calling back into url_for — either way,
    // the Handlers we're pointing at live under the calling package.
    let owner_class = ctx.current_package;
    if owner_class == () { return out; }

    out += #{
        DispatchCall: #{
            name: path,
            dispatcher: mname,
            owner: #{ Class: owner_class },
            span: args[0].span,
            var_text: "",
        }
    };

    out
}
