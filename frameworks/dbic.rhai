// dbic: DBIx::Class result-class DSL → column + relationship accessors.
//
// A Result class declares its table shape with class-method calls:
//     __PACKAGE__->add_columns(qw/id name/);   # or  id => { data_type => ... }
//     __PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');
//     __PACKAGE__->belongs_to(author => 'Schema::Result::User');
//
// A column installs an accessor + a `Bridged` HashKeyDef, so `$row->name` and
// the condition args `$rs->search({ name => ... })` resolve it — but NOT
// `$row->{name}`: a column isn't a hash slot (it's undef in DBIC). A
// relationship installs an accessor whose return names the related shape:
// has_many / many_to_many → a ResultSet, belongs_to / has_one / might_have → row.
//
// Core feeds the flat arg names in `ctx.arg_names`; this plugin owns the verb
// vocabulary. `ClassIsa("DBIx::Class")` scopes synthesis to result classes at
// any depth. `load_components` parent registration stays core (generic mixin
// machinery). `fluent_verbs` carry a resultset through `$rs->search`, so
// `RowOf`/`find` compose a `$rs->find->col` chain; relationship typing is plain.

fn id() { "dbic" }

fn triggers() {
    [ #{ ClassIsa: "DBIx::Class" } ]
}

// The verbs whose args core should flatten into `ctx.arg_names`.
fn arg_name_verbs() {
    ["add_columns", "add_column", "has_many", "many_to_many",
     "belongs_to", "has_one", "might_have"]
}

// Resultset/row verbs whose FIRST hashref arg is keyed by COLUMN names — the
// `\%cond` of `search(\%cond, \%attrs)`, the `\%cols` of `create`/`update`.
// Core links those keys to the class's columns (so renaming a column rewrites
// them) and, crucially, walks ONLY the first hashref: the trailing `\%attrs`
// hash (`order_by`/`rows`/`group_by`/…) is NOT column-keyed and stays out.
// Owning the verb list here (not in core) keeps core from hardcoding DBIC's API.
fn column_keyed_verbs() {
    ["search", "search_rs", "find", "find_or_create", "find_or_new",
     "update", "update_or_create", "create", "new", "count", "single"]
}

// Fluent verbs — the call returns the invocant's type UNCHANGED, so they only
// apply to an actual resultset (`$rs->search` → a same-row resultset). DBIC has
// no `Class->search`: search/find live on the resultset, which only comes from
// `$schema->resultset('X')` (typed separately). Core re-publishes the invocant's
// `Parametric(ResultSet{row})`; `RowOf`/`find` then compose the rest (`$rs->find`
// → a row). Verb→return-shape is DBIC's, so it lives here.
fn fluent_verbs() {
    ["search", "search_rs"]
}

fn is_resultset_rel(verb) {
    verb == "has_many" || verb == "many_to_many"
}

fn is_row_rel(verb) {
    verb == "belongs_to" || verb == "has_one" || verb == "might_have"
}

// A column accessor: a get/set Method plus a `Bridged` HashKeyDef (resolves the
// search-condition / constructor arg keys — never a `$row->{col}` deref).
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
                // A column lives in its own owner namespace, NOT as a `$row->{col}`
                // hash slot (undef in DBIC) — `owner_bridged`, not `Class`.
                owner: owner_bridged(pkg),
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
