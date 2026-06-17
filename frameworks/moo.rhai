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
// The core walks the `has` CST and hands this plugin EVERY fat-comma option
// key→value as a value-shape-classified `ctx.has_options` entry (rule #1 —
// only the builder touches tree-sitter; no keyword allowlist in core, so a
// new keyword needs no core edit). What lives HERE is the vocabulary: which
// keyword maps to which name pattern and return behavior; arms below ignore
// keywords/shapes they don't act on. New Moo accessor behavior is a new arm
// in this file, not a branch in the builder. The default accessor / isa /
// constructor-key synthesis is still native in `visit_has_call` (a larger
// move left as future work); this plugin is the seam for everything beyond it.

fn id() { "moo" }

// The base role engines this family ships: `use`-ing one makes the
// consuming package a ROLE (is_role_package; the composer-mismatch
// diagnostic skips roles — their requires obligations transfer to the
// eventual class composer). Core holds NO list of its own — this
// manifest and any third-party plugin's are the whole set. House kits
// that re-export one of these can instead emit SyntheticUse (see
// adr/role-contracts.md). Role::Tiny::With is deliberately absent: it
// grants `with` to plain classes.
fn role_makers() {
    ["Moo::Role", "Moose::Role", "Mouse::Role", "Role::Tiny"]
}

// The verbs whose args core flattens into `ctx.arg_names` — and, since
// these are Moo's accessor declarators, the gate behind which core also
// builds the richer `has_options` (classified shorthand/handles options a
// flat name list can't carry). Declaring them here is what keeps core from
// hardcoding `has`/`option`; the vocabulary lives with the plugin.
fn arg_name_verbs() {
    ["has", "option"]
}

fn triggers() {
    [
        #{ UsesModule: "Moo" },
        #{ UsesModule: "Moo::Role" },
        #{ UsesModule: "Moose" },
        #{ UsesModule: "Moose::Role" },
        #{ UsesModule: "Mouse" },
        // Dancer2::Plugin re-exports Moo's `has` into the consuming plugin, so
        // `has my_setting => (...)` there wants the same accessor synthesis.
        #{ UsesModule: "Dancer2::Plugin" },
        // MooX::Options' `option` is a `has` with extra option-parsing keys;
        // its accessor-option vocabulary (predicate/clearer/...) is identical.
        #{ UsesModule: "MooX::Options" },
    ]
}

// Keywords whose value names a method to synthesize. Any OTHER option keyword
// (`is`/`isa`/`default`/`lazy`/`coerce`/`required`/`trigger`/…) carries data,
// not a method name — its value (`'ro'`, `'Str'`, …) must NOT mint a symbol.
// This gate is the whole fix for the phantom-`ro`/`rw`/`lazy` regression: core
// hands every fat-comma option through by value shape (rule #1), and the
// vocabulary of "which keyword produces a name" lives here (rule #8/#10).
fn names_a_method(kw) {
    kw == "reader" || kw == "writer" || kw == "accessor"
        || kw == "predicate" || kw == "clearer" || kw == "builder" || kw == "handles"
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
// `attr` enrolls the method in the attribute's projection group: rename
// of the attr re-derives this method's name (when it embeds the attr) and
// references from any group spelling include its call sites. Pass () for
// methods whose identity is NOT the attr's (handles delegations).
fn plain_method(name, span, params, attr) {
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
            attr: attr,
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
    // `option` (MooX::Options) shares `has`'s accessor-option vocabulary.
    if ctx.function_name != "has" && ctx.function_name != "option" { return out; }
    let opts = ctx.has_options;
    if opts == () { return out; }

    let cls = isa_class(opts.isa_type);

    for attr_pair in opts.attr_names {
        let attr = attr_pair[0];
        let span = attr_pair[1];

        for opt in opts.options {
            let kw = opt.keyword;

            // `lazy_build => 1` (Moose) implies a builder/clearer/predicate
            // trio at runtime; expand it to the three companion methods.
            if kw == "lazy_build" && opt.shorthand {
                out += plain_method("_build_" + attr, span, [], attr);
                out += plain_method(derive_name("clear_", attr), span, [], attr);
                out += plain_method(derive_name("has_", attr), span, [], attr);
                continue;
            }

            // Only name-producing keywords mint a method. Everything else
            // (is/isa/default/lazy/…) carries data, not a method name.
            if !names_a_method(kw) { continue; }

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
                            attr: (),
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

            // writer/accessor take a value arg; the rest take none.
            let params = [];
            if kw == "writer" || kw == "accessor" {
                params = [ #{ name: "$val", "default": (), is_slurpy: false, is_invocant: false } ];
            }
            out += plain_method(name, span, params, attr);
        }
    }

    out
}
