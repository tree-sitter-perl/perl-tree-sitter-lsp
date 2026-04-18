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

fn on_method_call(ctx) {
    let out = [];
    if ctx.method_name != "helper" { return out; }

    let args = ctx.args;
    if args.len() < 2 { return out; }

    let name = args[0].string_value;
    if name == () { return out; }

    let params = args[1].sub_params;
    let name_span = args[0].span;

    // Split on '.' — single-segment names are the common case and
    // produce exactly one Method, same as before. Dotted names chain.
    let segments = name.split(".");

    if segments.len() == 1 {
        // Simple helper: Method on Mojolicious::Controller.
        out += #{
            Method: #{
                name: name,
                span: name_span,
                selection_span: name_span,
                params: params,
                is_method: true,
                return_type: (),
                doc: (),
                on_class: "Mojolicious::Controller",
            }
        };
        return out;
    }

    // Dotted helper: each non-leaf segment becomes a parameterless
    // Method whose return_type is the next proxy class. Build the
    // proxy-class path directly using `::` separators so the name is
    // deterministic per prefix and two helpers sharing a prefix
    // (`users.create`, `users.delete`) collapse onto the same proxy.
    let n = segments.len();
    let proxy_base = "Mojolicious::Controller::_Helper";
    let proxy_path = proxy_base;
    let owner_class = "Mojolicious::Controller";
    for i in 0..(n - 1) {
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
        }
    };

    out
}
