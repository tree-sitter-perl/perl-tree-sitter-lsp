// dbic: DBIx::Class result-class DSL → column + relationship accessors.
//
// A Result class declares its table shape with class-method calls:
//     __PACKAGE__->add_columns(qw/id name/);   # or  id => { data_type => ... }
//     __PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');
//     __PACKAGE__->belongs_to(author => 'Schema::Result::User');
//
// A column installs an accessor + a HashKeyDef (so `$rs->search({ name => ... })`
// and `$row->{name}` resolve it). A relationship installs an accessor whose
// return names the related shape: has_many / many_to_many → a ResultSet,
// belongs_to / has_one / might_have → the row class.
//
// Core feeds the flat arg names in `ctx.arg_names`; this plugin owns the verb
// vocabulary. `ClassIsa("DBIx::Class")` scopes synthesis to result classes at
// any depth. `load_components` parent registration stays core (generic mixin
// machinery). Parametric ResultSet typing (per-method `find` → row) is
// follow-up; accessor + plain typed return is the load-bearing 80%.

fn id() { "dbic" }

fn triggers() {
    [ #{ ClassIsa: "DBIx::Class" } ]
}

// The verbs whose args core should flatten into `ctx.arg_names`.
fn arg_name_verbs() {
    ["add_columns", "add_column", "has_many", "many_to_many",
     "belongs_to", "has_one", "might_have"]
}

fn is_resultset_rel(verb) {
    verb == "has_many" || verb == "many_to_many"
}

fn is_row_rel(verb) {
    verb == "belongs_to" || verb == "has_one" || verb == "might_have"
}

// A column accessor: a get/set Method plus the HashKeyDef owned by the
// result class (constructor / search / direct-deref key resolution).
fn column_actions(name, span, pkg) {
    let out = [];
    out += #{
        Method: #{
            name: name,
            span: span,
            selection_span: span,
            params: [ #{ name: "$val", "default": (), is_slurpy: false, is_invocant: false } ],
            is_method: true,
            return_type: (),
            doc: (),
            on_class: (),
            display: (),
            hide_in_outline: false,
            opaque_return: false,
        }
    };
    if pkg != () {
        out += #{
            HashKeyDef: #{
                name: name,
                owner: #{ Class: pkg },
                span: span,
                selection_span: span,
            }
        };
    }
    out
}

// A relationship accessor: a no-arg Method whose return names the
// related shape. `ret` is the InferredType (ResultSet class or row class).
fn relationship_method(name, span, ret) {
    #{
        Method: #{
            name: name,
            span: span,
            selection_span: span,
            params: [],
            is_method: true,
            return_type: ret,
            doc: (),
            on_class: (),
            display: (),
            hide_in_outline: false,
            opaque_return: false,
        }
    }
}

fn on_method_call(ctx) {
    let out = [];
    let verb = ctx.method_name;
    if verb == () { return out; }

    // Class-level declaration only: `__PACKAGE__->add_columns(...)`, never a
    // runtime `$rs->add_columns({...})` (a query op, not a column decl).
    if !ctx.receiver_is_package { return out; }

    let names = ctx.arg_names;
    if names == () || names.len() < 1 { return out; }

    if verb == "add_columns" || verb == "add_column" {
        // Every flattened name is a column (the hashref option values
        // carry no name and never reach `arg_names`).
        for pair in names {
            out += column_actions(pair[0], pair[1], ctx.current_package);
        }
        return out;
    }

    if is_resultset_rel(verb) || is_row_rel(verb) {
        // names[0] is the accessor, names[1] (if present) the related class.
        let acc = names[0];
        let ret = ();
        if is_resultset_rel(verb) {
            ret = #{ ClassName: "DBIx::Class::ResultSet" };
        } else if names.len() >= 2 {
            ret = #{ ClassName: names[1][0] };
        }
        out += relationship_method(acc[0], acc[1], ret);
        return out;
    }

    out
}
