// moo: the Moo/Moose `has` accessor-option vocabulary. A `has` declaration's
// non-default options each synthesize a method whose NAME follows a per-option
// convention:
//
//   predicate => 1   -> has_<attr>     (private attr `_x` -> `_has_x`)
//   clearer   => 1   -> clear_<attr>   (private attr `_x` -> `_clear_x`)
//   builder   => 1   -> _build_<attr>
//   writer    => 'set_x'   -- explicit name, takes one value arg
//   reader    => 'get_x'   -- explicit name
//   handles => { local => 'remote', ... } / [qw/m.../]
//                    -> one method per local name; when the attribute's isa is
//                       a class, the delegated method's return edges to the
//                       remote method on that class
//
// The core walks the `has` CST and hands this plugin the decision-ready
// `ctx.has_options` shape (rule #1 — only the builder touches tree-sitter).
// What lives HERE is the vocabulary: which keyword maps to which name pattern
// and return behavior. New Moo accessor behavior is a new arm in this file,
// not a branch in the builder. The default accessor / isa / constructor-key
// synthesis is still native in `visit_has_call` (a larger move left as future
// work); this plugin is the established seam for everything beyond it.

fn id() { "moo" }

fn triggers() {
    [
        #{ UsesModule: "Moo" },
        #{ UsesModule: "Moo::Role" },
        #{ UsesModule: "Moose" },
        #{ UsesModule: "Moose::Role" },
        #{ UsesModule: "Mouse" },
    ]
}

// Derive a `=> 1` shorthand name: `prefix + attr`, with the Moo/Moose private
// convention that a leading `_` on the attribute moves to the front of the
// derived name (`_token` + predicate -> `_has_token`, not `has__token`).
fn derive_name(prefix, attr) {
    if attr.starts_with("_") {
        "_" + prefix + attr.sub_string(1)
    } else {
        prefix + attr
    }
}

// A plain method symbol with no return type — predicate (returns bool),
// clearer (returns undef), builder (return is whatever the user's sub yields).
fn plain_method(name, span, params) {
    #{
        Method: #{
            name: name,
            span: span,
            selection_span: span,
            params: params,
            is_method: true,
            return_type: (),
            doc: (),
            on_class: (),
            display: (),
            hide_in_outline: false,
            opaque_return: false,
            outline_label: (),
            return_via_edge: (),
        }
    }
}

// Pull the class name out of an `isa_type` map, or () if the isa isn't a
// plain class (Str/Int/HashRef/etc. can't back a delegation return edge).
fn isa_class(isa_type) {
    if isa_type == () { return (); }
    if isa_type.contains("ClassName") { return isa_type.ClassName; }
    ()
}

fn on_function_call(ctx) {
    let out = [];
    if ctx.function_name != "has" { return out; }
    let opts = ctx.has_options;
    if opts == () { return out; }

    let cls = isa_class(opts.isa_type);

    for attr_pair in opts.attr_names {
        let attr = attr_pair[0];
        let span = attr_pair[1];

        for opt in opts.options {
            let kw = opt.keyword;

            if kw == "handles" {
                // One method per delegated name. With a class-typed isa, edge
                // the method's return through MethodOnClass{class, remote} so
                // `$self->log(...)` chains to the remote method's return.
                for pair in opt.handles {
                    let local = pair[0];
                    let remote = pair[1];
                    let edge = ();
                    if cls != () {
                        edge = #{ MethodOnClass: #{ class: cls, name: remote } };
                    }
                    out += #{
                        Method: #{
                            name: local,
                            span: span,
                            selection_span: span,
                            params: [],
                            is_method: true,
                            return_type: (),
                            doc: (),
                            on_class: (),
                            display: (),
                            hide_in_outline: false,
                            opaque_return: false,
                            outline_label: (),
                            return_via_edge: edge,
                        }
                    };
                }
                continue;
            }

            // Resolve the method name: shorthand derives from the attr, an
            // explicit string is used verbatim.
            let name = ();
            if opt.shorthand {
                if kw == "predicate" { name = derive_name("has_", attr); }
                else if kw == "clearer" { name = derive_name("clear_", attr); }
                else if kw == "builder" { name = "_build_" + attr; }
                // writer/reader have no `=> 1` shorthand in Moo.
            } else if opt.explicit_name != () {
                name = opt.explicit_name;
            }
            if name == () { continue; }

            // writer takes a value arg; the rest take none.
            let params = [];
            if kw == "writer" {
                params = [ #{ name: "$val", "default": (), is_slurpy: false, is_invocant: false } ];
            }
            out += plain_method(name, span, params);
        }
    }

    out
}
