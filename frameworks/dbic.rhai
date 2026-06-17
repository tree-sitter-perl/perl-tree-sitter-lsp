// dbic: DBIx::Class result-class DSL → column + relationship accessors.
//
// A DBIC Result class declares its table shape with class-method calls:
//
//     __PACKAGE__->add_columns(qw/id name email/);
//     __PACKAGE__->add_columns(id => { data_type => 'integer' }, ...);
//     __PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');
//     __PACKAGE__->belongs_to(author => 'Schema::Result::User', 'author_id');
//
// Each column installs an accessor method (and a HashKeyDef so
// `$rs->search({ name => ... })` / `$row->{name}` resolve the column).
// Each relationship installs an accessor whose RETURN names the related
// shape: a `has_many` / `many_to_many` yields a `DBIx::Class::ResultSet`,
// a `belongs_to` / `has_one` / `might_have` yields the row class.
//
// Core walks the call args into `ctx.arg_names` (rule #1 — the shared
// `cst::string_list`: qw-expanded, fat-comma-key + positional strings,
// each with its tight span, separator-agnostic). This plugin owns the
// verb vocabulary: which name is a column vs an accessor+target-class,
// and the typed return. The trigger (`ClassIsa("DBIx::Class")`) scopes
// synthesis to result classes at any inheritance depth.
//
// `load_components` parent registration stays in core — it's generic
// component-mixin machinery, not DBIC-specific. Parametric ResultSet
// typing (per-method projection: `find` → row, `search` → resultset)
// is deliberate follow-up; the accessor + plain typed return is the
// load-bearing 80%.

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
