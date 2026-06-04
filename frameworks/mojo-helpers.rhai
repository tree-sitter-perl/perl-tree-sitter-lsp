// mojo-helpers: `$app->helper(name => sub { ... })` registers `name` as a
// method callable on any Mojolicious controller. Dotted names chain into
// helper namespaces:
//
//   $app->helper(users      => sub {...})   # $c->users
//   $app->helper('users.create' => sub {...})  # $c->users->create
//   $app->helper('admin.users.purge' => sub {...})  # $c->admin->users->purge
//
// Modeling: each dotted segment (except the leaf) emits a namespace-
// method that returns a synthetic proxy class; the leaf emits the real
// helper sub on the innermost proxy. Completion chains work because
// each link's return type is the next proxy, and standard method-on-
// class inheritance does the rest.
//
// The synthetic proxy classes are named deterministically from the
// chain prefix so two helpers that share a prefix (`users.create`,
// `users.delete`) end up on the same proxy. Don't prettify these
// names — other parts of the system may show them in hovers.

fn id() { "mojo-helpers" }

fn triggers() {
    [
        #{ UsesModule: "Mojolicious::Lite" },
        #{ UsesModule: "Mojolicious" },
        #{ ClassIsa: "Mojolicious" },
        // Mojo plugins are the canonical place to register helpers
        // (`sub register ($self, $app, $conf) { $app->helper(...) }`),
        // and a plugin package is rarely a Mojolicious subclass itself.
        // Without this, every helper defined in a plugin stays dark.
        #{ ClassIsa: "Mojolicious::Plugin" },
    ]
}

// The receiver classes that compose the app surface — the "open
// consumption" axis (docs/adr/plugin-system.md). Helpers bridge to the
// single app-surface class; core gives each of these classes the
// surface as a synthetic ancestor, so `$app->h` AND `$c->h` resolve
// through the existing ancestor + bridge walk. App subclasses inherit
// the surface for free through the normal MRO. Add a consumer here, not
// in each helper's bridge list.
fn app_surface_consumers() {
    ["Mojolicious::Controller", "Mojolicious"]
}

// Helpers live on multiple classes in Mojolicious: the application
// instance (`$app`) AND every controller (`$c`). Users call them on
// both — `$app->current_user` from a hook, `$c->current_user` from an
// action. We emit on both entry points; the shared proxy prefix means
// dotted helpers only pay the fan-out once per leaf.

fn on_method_call(ctx) {
    let out = [];
    if ctx.method_name != "helper" { return out; }

    let args = ctx.args;
    if args.len() < 2 { return out; }

    let name = args[0].string_value;
    if name == () { return out; }

    // `$app->helper(NAME => sub { my ($c, @rest) = @_; ... })` — the
    // helper's first positional is the Mojolicious controller. Flag
    // it so sig help / hover / outline don't show `$c` at call sites.
    let params = as_invocant_params(args[1].sub_params);
    let name_span = args[0].span;

    // Type the first positional inside the callback body as
    // Mojolicious::Controller — lets `$c->...` inside the callback
    // complete against controller methods + helpers. Anchor the
    // constraint to the sub's span; the builder resolves to the
    // callback's actual scope after the walk.
    if params.len() >= 1 {
        out += #{
            VarType: #{
                variable: params[0].name,
                at: args[1].span,
                inferred_type: #{ ClassName: "Mojolicious::Controller" },
            }
        };
    }

    // Named-sub registration: `$app->helper(greet => \&_greet)`. The
    // callback's body lives in a separate named sub, so there's no
    // `sub_params` / body span on the arg — `ref_sub_name` carries the
    // sub name and the builder types that sub's first positional the same
    // way (deferred, so a forward-declared `_greet` resolves too). Same
    // first-param-is-the-controller rule, just keyed by name.
    if args[1].ref_sub_name != () {
        out += #{
            NamedSubParamType: #{
                sub_name: args[1].ref_sub_name,
                param_index: 0,
                inferred_type: #{ ClassName: "Mojolicious::Controller" },
            }
        };
    }

    // Split on '.' — single-segment names are the common case and
    // produce exactly one Method. Dotted names chain.
    let segments = name.split(".");
    // `$c->helper` AND `$app->helper` both need to resolve. We used
    // to fan-out one Method per class (Controller + Mojolicious), but
    // that made namespace entities ambiguous (two Symbols per helper)
    // and required a second scan per call site. Phase 2 of the plugin
    // arch moves to ONE Method, owned by `Mojolicious::Controller`,
    // and a PluginNamespace whose bridges cover both entry classes —
    // for_each_entity_bridged_to matches via bridge union, so the
    // same Method surfaces from a `Mojolicious` query too.

    // App namespace — one per current_package. Bridges to the single
    // fictional "app surface" class (docs/adr/plugin-system.md). The
    // consumer classes (declared by `app_surface_consumers()` below, and
    // any app subclass) inherit the surface as a synthetic parent in
    // core, so the existing ancestor walk + bridge resolution finds the
    // helper from `$app->h` AND `$c->h` — one bridge target, open
    // consumer set. Entity names accumulate per app namespace.
    let app_ns_id = if ctx.current_package == () {
        "mojo-app:main"
    } else {
        "mojo-app:" + ctx.current_package
    };
    let app_surface = "Mojolicious::_AppSurface";
    let app_bridges = [
        #{ Class: app_surface },
    ];

    if segments.len() == 1 {
        // Simple helper: ONE Method on the canonical entry class.
        // `return_via_edge` lifts the helper's return type from
        // the callback body's last expression at query time. The
        // body hasn't been walked when this hook fires, but
        // `Expr(span)` carries the type by the time consumers
        // query — the bag's edge-chase resolver follows the back-
        // pointer. Lets `$c->name->method` chain on a helper that
        // returns a Parametric ResultSet (DBIC), a Promise, etc.,
        // without the plugin inspecting the body at parse time.
        // `callable_return_edge` is bag-aware — it carries the
        // body span whether the callback is a literal in the arg
        // slot (`helper(name => sub { ... })`) or a rebound
        // scalar (`my $sub = sub { ... }; helper(name => $sub)`).
        out += #{
            Method: #{
                name: name,
                span: name_span,
                selection_span: name_span,
                params: params,
                is_method: true,
                return_type: (),
                return_via_edge: args[1].callable_return_edge,
                doc: (),
                on_class: app_surface,
                display: "Helper",
                hide_in_outline: false,
            }
        };
        // Register the helper into the app namespace, bridged to the app
        // surface. The Method's home package is the surface; consumer
        // classes reach it via the synthetic-parent edge in core.
        out += #{
            PluginNamespace: #{
                id: app_ns_id,
                kind: "app",
                bridges: app_bridges,
                entity_names: [name],
                decl_span: ctx.call_span,
            }
        };
        // Anchor a MethodCallRef at the name span so gr/hover on the
        // helper name land on the synthesized Method — otherwise
        // ref_at returns the outer `$app->helper(...)` call's ref
        // (target: `helper`) and gr lists every helper-registration
        // site as a "reference" to the name.
        out += #{
            MethodCallRef: #{
                method_name: name,
                invocant: app_surface,
                span: name_span,
                invocant_span: (),
            }
        };
        return out;
    }

    // Dotted helper: each non-leaf segment becomes a parameterless
    // Method whose return_type is the next proxy class. Build the
    // proxy-class path directly using `::` separators so the name is
    // deterministic per prefix and two helpers sharing a prefix
    // (`users.create`, `users.delete`) collapse onto the same proxy.
    //
    // First segment: ONE Method owned by the app surface (the chain
    // root, reached from `$c`/`$app` via the synthetic-parent edge).
    //
    // `hide_in_outline: true` on non-leaf segments — they're chain
    // plumbing, not registrations the user wrote. Two helpers
    // sharing a prefix (`users.create`, `users.delete`) would each
    // surface an intermediate `users` entry; a three-segment
    // (`admin.users.purge`) would show a second `users` further
    // down. The leaf is the actual registration — give it the full
    // dotted path as `outline_label` so the user sees
    // `helper users.create` not a bare `helper create`.
    let n = segments.len();
    let proxy_base = "Mojolicious::Controller::_Helper";
    let first_seg = segments[0];
    let first_proxy = proxy_base + "::" + first_seg;
    out += #{
        Method: #{
            name: first_seg,
            span: name_span,
            selection_span: name_span,
            params: [],
            is_method: true,
            return_type: #{ ClassName: first_proxy },
            doc: (),
            on_class: app_surface,
            display: "Helper",
            hide_in_outline: true,
            opaque_return: true,
        }
    };

    let proxy_path = first_proxy;
    let owner_class = first_proxy;
    for i in 1..(n - 1) {
        let seg = segments[i];
        proxy_path = proxy_path + "::" + seg;

        out += #{
            Method: #{
                name: seg,
                span: name_span,
                selection_span: name_span,
                params: [],
                is_method: true,
                return_type: #{ ClassName: proxy_path },
                doc: (),
                on_class: owner_class,
                display: "Helper",
                hide_in_outline: true,
                opaque_return: true,
            }
        };

        owner_class = proxy_path;
    }

    // Leaf: the real helper sub, attached to the innermost proxy.
    let leaf = segments[n - 1];
    out += #{
        Method: #{
            name: leaf,
            span: name_span,
            selection_span: name_span,
            params: params,
            is_method: true,
            return_type: (),
            doc: (),
            on_class: owner_class,
            display: "Helper",
            hide_in_outline: false,
            outline_label: name,
        }
    };

    // Register the chain across TWO kinds of namespaces so bridge
    // visibility stays isolated per proxy level:
    //
    //   1. App namespace (bridges: the app surface) — owns ONLY the
    //      first segment (the entry point from `$c`/`$app`, reached via
    //      the synthetic-parent edge). Query a consumer class → gets
    //      top-level helpers + chain roots, but NOT proxy-only entities
    //      (no create/delete leaking into `$c->` completion).
    //   2. One namespace per proxy level (bridges: just that proxy
    //      class) — owns the entities that live ON that proxy.
    //      Query _Helper::users → gets exactly {create, delete, admin}.
    out += #{
        PluginNamespace: #{
            id: app_ns_id,
            kind: "app",
            bridges: app_bridges,
            entity_names: [first_seg],
            decl_span: ctx.call_span,
        }
    };

    // Walk proxy levels: each intermediate segment goes into its
    // parent proxy's namespace; the leaf goes into the innermost
    // proxy's namespace.
    let current_proxy = first_proxy;
    for i in 1..(n - 1) {
        let seg = segments[i];
        out += #{
            PluginNamespace: #{
                id: "mojo-proxy:" + current_proxy,
                kind: "proxy",
                bridges: [#{ Class: current_proxy }],
                entity_names: [seg],
                decl_span: ctx.call_span,
            }
        };
        current_proxy = current_proxy + "::" + seg;
    }
    // Leaf on the innermost proxy.
    out += #{
        PluginNamespace: #{
            id: "mojo-proxy:" + current_proxy,
            kind: "proxy",
            bridges: [#{ Class: current_proxy }],
            entity_names: [segments[n - 1]],
            decl_span: ctx.call_span,
        }
    };

    // Anchor a MethodCallRef at the name span so gr/hover on the
    // helper name land on the synthesized Method — not on the
    // enclosing `$app->helper(...)` call's method ref.
    out += #{
        MethodCallRef: #{
            method_name: leaf,
            invocant: owner_class,
            span: name_span,
            invocant_span: (),
        }
    };

    out
}
