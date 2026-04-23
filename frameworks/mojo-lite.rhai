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

// `use Mojolicious::Lite` monkey-patches a fixed verb set into the
// caller's package at import time. Modeling:
//
//   * Verbs that delegate to a real method elsewhere — `get`, `post`,
//     `helper`, etc. — land as synthetic `Import`s pointing at the real
//     source module (Mojolicious::Routes::Route for route verbs,
//     Mojolicious for helper/hook/plugin). Hover, gd, and sig-help
//     flow through the normal imported-function path to the real sub's
//     POD/signature/return type.
//
//   * `del` is a rename: `del '/x' => sub {}` calls `$routes->delete(...)`.
//     `ImportedSymbol::renamed("del", "delete")` gives the local name
//     the user types + the remote name the LSP looks up cross-file.
//
//   * `app` and `group` are pure closures with nowhere real to point to
//     (`sub { $app }`, a control-flow block). They stay as plugin-emitted
//     Sub symbols — `app` typed as Mojolicious so the `app->routes` chain
//     starts with intelligence; `group` bare.
//
//   * Every verb also emits `FrameworkImport` so the unresolved-function
//     diagnostic skips them regardless of whether the real source module
//     is resolved cross-file yet.

fn on_use(ctx) {
    if ctx.module_name != "Mojolicious::Lite" { return []; }
    let out = [];

    // `app` — no real sub to link to; monkey-patched `sub { $app }`.
    // Typed Sub is the one acknowledged stub; downstream chain flow
    // (`app->routes->get->to`) depends on this return type.
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
                    doc: "The Mojolicious application instance.",
                    display: "Function",
                    hide_in_outline: true,
                }
            },
        }
    };

    // `group` — control-flow block. No corresponding method.
    out += #{ FrameworkImport: #{ keyword: "group" } };
    out += #{
        Symbol: #{
            name: "group",
            kind: "Sub",
            span: ctx.span,
            selection_span: ctx.span,
            detail: #{
                Sub: #{
                    params: [],
                    is_method: false,
                    return_type: (),
                    doc: "Group routes so common prefixes/middlewares apply to all of them.",
                    display: "Function",
                    hide_in_outline: true,
                }
            },
        }
    };

    // Route verbs → Mojolicious::Routes::Route. Each `$routes->$verb(@_)`
    // internally, so the real POD/signature/return type live on Route.
    let route_same_name_verbs = [
        "get", "post", "put", "patch", "options", "any",
        "under", "websocket",
    ];
    let route_imports = [];
    for v in route_same_name_verbs {
        out += #{ FrameworkImport: #{ keyword: v } };
        route_imports += #{ local_name: v };
    }
    // `del` is the rename: local `del` → remote `delete` on Route.
    out += #{ FrameworkImport: #{ keyword: "del" } };
    route_imports += #{ local_name: "del", remote_name: "delete" };

    out += #{
        Import: #{
            module_name: "Mojolicious::Routes::Route",
            imported_symbols: route_imports,
            span: ctx.span,
        }
    };

    // Helper/hook/plugin verbs → Mojolicious. Each `$app->$name(@_)`
    // internally; the real method is on the app class.
    let mojo_verbs = ["helper", "hook", "plugin"];
    let mojo_imports = [];
    for v in mojo_verbs {
        out += #{ FrameworkImport: #{ keyword: v } };
        mojo_imports += #{ local_name: v };
    }
    out += #{
        Import: #{
            module_name: "Mojolicious",
            imported_symbols: mojo_imports,
            span: ctx.span,
        }
    };

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

    // Lite routes are framework-synthesized URL callables. The
    // HTTP verb gets promoted into `outline_label` ("GET /path",
    // "POST /path") so two handlers on the same URL (the common
    // form+submit pair) don't collapse into two visually-identical
    // outline entries. Dispatch still keys on `name: path` — url_for
    // matches on path, not verb.
    out += #{
        Handler: #{
            name: path,
            owner: #{ Class: owner_class },
            dispatchers: ["url_for", "redirect_to"],
            params: params,
            span: args[0].span,
            selection_span: args[0].span,
            display: "Route",
            hide_in_outline: false,
            outline_label: fname.to_upper() + " " + path,
        }
    };

    // Register the route into a per-app Lite routes namespace. Bridges
    // to the declaring package (usually `main` for Lite scripts) so
    // `for_each_entity_bridged_to(owner_class, ...)` finds the route
    // Handlers. Same emission shape as mojo-routes so the two flavors
    // aggregate through the same query primitive.
    out += #{
        PluginNamespace: #{
            id: "mojo-lite:" + owner_class,
            kind: "routes",
            bridges: [ #{ Class: owner_class } ],
            entity_names: [path],
            decl_span: args[0].span,
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
