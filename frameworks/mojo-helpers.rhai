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
    ]
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

    // Split on '.' — single-segment names are the common case and
    // produce exactly one Method per entry class. Dotted names chain.
    let segments = name.split(".");
    // Entry classes where helpers become methods. Both Controller and
    // Mojolicious (the app class) so `$c->...` AND `$app->...` complete.
    let entry_classes = ["Mojolicious::Controller", "Mojolicious"];

    // App namespace — one per current_package. Find-or-create on the
    // core side. Bridges to both Controller (so `$c->helper` works) and
    // Mojolicious (so `$app->helper` works). Entity names accumulate
    // as we register more helpers on the same app.
    let app_ns_id = if ctx.current_package == () {
        "mojo-app:main"
    } else {
        "mojo-app:" + ctx.current_package
    };
    let app_bridges = [
        #{ Class: "Mojolicious::Controller" },
        #{ Class: "Mojolicious" },
    ];

    if segments.len() == 1 {
        // Simple helper: Method on each entry class.
        for entry in entry_classes {
            out += #{
                Method: #{
                    name: name,
                    span: name_span,
                    selection_span: name_span,
                    params: params,
                    is_method: true,
                    return_type: (),
                    doc: (),
                    on_class: entry,
                    display: "Helper",
                    hide_in_outline: false,
                }
            };
        }
        // Register the helper into the app namespace. Fan-out on two
        // classes means TWO Symbols share the same name; both land as
        // namespace entities via the name-keyed resolution.
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
                invocant: "Mojolicious::Controller",
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
    // Fan-out first-segment emission across the entry classes, then
    // the deeper chain lives on the shared proxy (one copy).
    let n = segments.len();
    let proxy_base = "Mojolicious::Controller::_Helper";
    let first_seg = segments[0];
    let first_proxy = proxy_base + "::" + first_seg;
    for entry in entry_classes {
        out += #{
            Method: #{
                name: first_seg,
                span: name_span,
                selection_span: name_span,
                params: [],
                is_method: true,
                return_type: #{ ClassName: first_proxy },
                doc: (),
                on_class: entry,
                display: "Helper",
                hide_in_outline: false,
                opaque_return: true,
            }
        };
    }

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
                hide_in_outline: false,
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
        }
    };

    // Register the chain's root into the app namespace. The root is
    // the first segment — the thing you reach from Perl-space. The
    // chain's proxy classes (and the leaf on its innermost proxy) are
    // regular Methods on their respective classes, resolved through
    // normal chain resolution once you're past the first hop.
    out += #{
        PluginNamespace: #{
            id: app_ns_id,
            kind: "app",
            bridges: app_bridges,
            entity_names: [first_seg],
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
