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

    out += #{
        Handler: #{
            name: path,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for"],
            params: params,
            span: args[0].span,
            selection_span: args[0].span,
        }
    };

    out
}
