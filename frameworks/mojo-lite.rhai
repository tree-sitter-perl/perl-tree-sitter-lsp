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
// DSL verbs Mojolicious::Lite autoimports. Each entry is a
// (name, doc) pair — rendered on hover. `app` and the route verbs
// get proper Sub symbols (with return types where meaningful) so
// hover/gd work; the rest fall back to FrameworkImport to silence
// the unresolved-function diagnostic without over-promising.
fn dsl_docs() {
    #{
        "app":       "The Mojolicious application instance.",
        "get":       "Register a route for HTTP GET. `get '/path' => sub { ... }`",
        "post":      "Register a route for HTTP POST.",
        "put":       "Register a route for HTTP PUT.",
        "del":       "Register a route for HTTP DELETE.",
        "patch":     "Register a route for HTTP PATCH.",
        "options":   "Register a route for HTTP OPTIONS.",
        "any":       "Register a route that matches any HTTP method.",
        "under":     "Register a middleware route. Nested routes fire only when the under-route allows the request.",
        "websocket": "Register a WebSocket route.",
        "helper":    "Register a controller helper. `helper NAME => sub { my ($c, ...) = @_; ... }`",
        "hook":      "Attach a callback to a Mojolicious hook (before_dispatch, after_render, …).",
        "plugin":    "Load a Mojolicious plugin.",
        "group":     "Group routes so common prefixes/middlewares apply to all of them.",
    }
}

fn on_use(ctx) {
    if ctx.module_name != "Mojolicious::Lite" { return []; }
    let out = [];
    let docs = dsl_docs();

    // `app` returns the Mojolicious instance — type it so `app->routes`
    // resolves and completion offers instance methods. FrameworkImport
    // keeps the unresolved-function diagnostic happy; the typed Sub
    // symbol layered on top is what gives hover/gd/completion.
    out += #{ FrameworkImport: #{ keyword: "app" } };
    out += #{
        Symbol: #{
            name: "app",
            kind: "Sub",
            span: ctx.span,
            selection_span: ctx.span,
            detail: #{
                Sub: #{
                    params: [],
                    is_method: false,
                    return_type: #{ ClassName: "Mojolicious" },
                    doc: docs["app"],
                    display: "Function",
                }
            },
        }
    };

    // Route-declaration verbs. No return type — they're called for
    // side effect — but hover surfaces the usage hint. FrameworkImport
    // still emitted alongside to keep the diagnostic path unchanged.
    let verbs_rest = [
        "get", "post", "put", "del", "patch", "options", "any",
        "under", "websocket",
        "helper", "hook", "plugin",
        "group",
    ];
    for v in verbs_rest {
        out += #{ FrameworkImport: #{ keyword: v } };
        out += #{
            Symbol: #{
                name: v,
                kind: "Sub",
                span: ctx.span,
                selection_span: ctx.span,
                detail: #{
                    Sub: #{
                        params: [],
                        is_method: false,
                        return_type: (),
                        doc: docs[v],
                        display: "Function",
                    }
                },
            }
        };
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
    // The route handler's first positional is always the Mojolicious
    // controller; flag it so the call-site display drops it.
    let params = [];
    if args.len() >= 2 {
        params = as_invocant_params(args[1].sub_params);
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

    // Type the route-handler's first positional as Mojolicious::Controller
    // so `$c->...` inside `get '/path' => sub { my $c = shift; $c->... }`
    // resolves to controller methods + helpers.
    if params.len() >= 1 && args.len() >= 2 {
        out += #{
            VarType: #{
                variable: params[0].name,
                at: args[1].span,
                inferred_type: #{ ClassName: "Mojolicious::Controller" },
            }
        };
    }

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
