//! End-to-end tests for the `ReturnExpr` symbol-declarative
//! return-type machinery. Each test pins a concrete shape that
//! the call-site emission / Edge-without-receiver chase couldn't
//! cover before this work landed:
//!
//! - **A** (`coderef_call_arity_zero_resolves_mojo_getter`,
//!   `coderef_call_arity_one_resolves_mojo_writer`) — Mojo
//!   accessor invoked via `\&Class::name; $cb->($obj)` /
//!   `$cb->($obj, "v")`. The chain typer's coderef arm threads
//!   `arity_hint` and `receiver` into the bag query;
//!   `ReturnExprReducer` claims the `MethodOnClass{...}` union
//!   and dispatches the call's arity into the right arm. Empty
//!   arm answers the getter's `Concrete(default_t)`; AtLeast(1)
//!   arm substitutes `Receiver` for fluent return.
//!
//! - **B** (`coderef_to_resultset_find_returns_row_class`) — DBIC
//!   `find` / `single` / etc. declare `Operator(RowOf(Receiver))`
//!   once on `MethodOnClass{base, method}` via
//!   `emit_parametric_return_expr_decls`. The chain typer's
//!   coderef-call arm chases that with the call's invocant as
//!   receiver; substitution evaluates `RowOf(ResultSet{base, row})`
//!   to the row class.
//!
//! - **C** (`anon_closure_arity_dispatch_via_coderef_call`) —
//!   anon-sub closure with arity-discriminated arms (`return X
//!   unless @_; ...`) invoked via `$cb->()` / `$cb->("v")`. The
//!   anon sub is a first-class symbol (`(anon)` named, kind Sub),
//!   `coderef_return_edge_for` routes through `Symbol(sym_id)`,
//!   `emit_arity_return_witnesses` publishes a `UnionOnArgs`
//!   ReturnExpr on it.
//!
//! - **F** (`dynamic_method_call_routes_through_coderef_edge`) —
//!   `$obj->$cb()` (method-call syntax with a scalar method
//!   field) routes through the same edge chase as
//!   `coderef_call_expression` with `$obj` as receiver.
//!
//! Convention mirrors `parametric_resultset_tests`: each test is
//! self-contained (no fixtures), uses `inferred_type_via_bag` /
//! `find_definition` so it doesn't pin internal encoding.

use super::*;
use tree_sitter::{Parser, Point};

fn parse(source: &str) -> FileAnalysis {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

fn point_at(source: &str, needle: &str) -> Point {
    let byte = source
        .find(needle)
        .unwrap_or_else(|| panic!("needle {:?} not in source:\n{}", needle, source));
    let row = source[..byte].matches('\n').count();
    let col = byte - source[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0);
    Point::new(row, col)
}

// ---- Test A: Mojo accessor via coderef carries arity ----

/// `\&Foo::name; $cb->($foo)` — coderef-call invokes a Mojo getter
/// (arity 0 from the method's perspective; 1 arg passed to the
/// coderef, the first being the receiver). Result: the getter's
/// scalar-default type (`String`).
///
/// Mojo synth declares
/// `UnionOnArgs { (Empty, Concrete(String)), (AtLeast(1), Receiver) }`
/// on `MethodOnClass{Foo, name}`. The chain typer's coderef arm
/// passes `arity_hint = args.len() - 1 = 0` (subtracting the
/// receiver from the coderef's arg count) and `receiver = $foo`'s
/// type. `ReturnExprReducer` matches `Empty`, returns
/// `Concrete(String)`.
#[test]
fn coderef_call_arity_zero_resolves_mojo_getter() {
    let src = "\
package Foo;
use Mojo::Base -base;
has 'name' => 'default';

package main;
my $foo = Foo->new;
my $cb = \\&Foo::name;
my $x = $cb->($foo);
my $sentinel;
";
    let fa = parse(src);
    let pt = point_at(src, "my $sentinel");
    let ty = fa.inferred_type_via_bag("$x", pt);
    assert_eq!(
        ty,
        Some(InferredType::String),
        "coderef call of Mojo getter (arity 0 after dropping receiver) must \
         resolve through ReturnExpr's UnionOnArgs::Empty arm to String"
    );
}

/// Same setup as the getter test, but `$cb->($foo, "v")` → arity 1
/// after dropping the receiver. Hits the `AtLeast(1) => Receiver`
/// arm, substitutes the call's receiver (`$foo`'s type) →
/// `ClassName("Foo")`.
#[test]
fn coderef_call_arity_one_resolves_mojo_writer() {
    let src = "\
package Foo;
use Mojo::Base -base;
has 'name' => 'default';

package main;
my $foo = Foo->new;
my $cb = \\&Foo::name;
my $x = $cb->($foo, \"v\");
my $sentinel;
";
    let fa = parse(src);
    let pt = point_at(src, "my $sentinel");
    let ty = fa.inferred_type_via_bag("$x", pt);
    assert_eq!(
        ty,
        Some(InferredType::ClassName("Foo".into())),
        "coderef call of Mojo writer (arity 1 after dropping receiver) must \
         resolve through ReturnExpr's UnionOnArgs::Any => Receiver arm"
    );
}

// ---- Test B: ResultSet find via coderef projects through RowOf ----

/// `my $cb = \&...::find; $cb->($rs, $id)->{column}` — coderef
/// invocation of `find`, with a Parametric receiver.
///
/// `emit_parametric_return_expr_decls` declares
/// `Operator(RowOf(Receiver))` on
/// `MethodOnClass{"DBIx::Class::ResultSet", "find"}` once per
/// resultset call seen. The chain typer's coderef arm chases with
/// `receiver = $rs`'s `Parametric(ResultSet { row, .. })`.
/// `ReturnExprReducer` substitutes Receiver, evaluates `RowOf(...)`
/// → `Parametric(RowOf(ResultSet))`. Downstream `class_name()` /
/// `hash_key_class()` accessors evaluate the operator to the row
/// class.
#[test]
fn coderef_to_resultset_find_returns_row_class() {
    let src = "\
package Schema::Result::Users;
use base 'DBIx::Class::Core';
__PACKAGE__->add_columns(
    name => { data_type => 'varchar' },
);

package main;
my $schema;
my $rs = $schema->resultset('Schema::Result::Users');
my $cb = \\&DBIx::Class::ResultSet::find;
my $row = $cb->($rs, 1);
$row->{name};
";
    let fa = parse(src);

    // $row's TC carries the unevaluated `Parametric(RowOf(ResultSet))`
    // — the value-side projection. Consumers (`class_name()`,
    // `hash_key_class()`) evaluate it on demand. Pin the structural
    // shape so a regression to a flat / unrelated encoding trips
    // here before any user-visible feature breaks.
    let pt_row = point_at(src, "$row->{name}");
    let ty = fa.inferred_type_via_bag("$row", pt_row);
    let parametric = match ty {
        Some(InferredType::Parametric(p)) => p,
        other => panic!(
            "coderef call of `find` with a Parametric receiver must produce \
             a Parametric (RowOf<ResultSet>); got {:?}",
            other
        ),
    };
    assert_eq!(
        parametric.class_name(),
        Some("Schema::Result::Users"),
        "RowOf<ResultSet>'s class_name() evaluates to the row class for \
         downstream method dispatch + hash-key access"
    );
    assert_eq!(
        parametric.hash_key_class(),
        Some("Schema::Result::Users"),
        "RowOf<ResultSet>'s hash_key_class() evaluates to the row class \
         (delegates to inner ResultSet's hash_key_class)"
    );
}

// ---- Test C: anon-sub closure with arity dispatch via coderef ----

/// Anon-sub with arity-discriminated returns called via `$cb->()` /
/// `$cb->("v")`. Body returns a String literal in the no-args arm,
/// constructs a class instance otherwise — keeps the test focused
/// on the arity dispatch question and avoids the orthogonal
/// "does `$self->{x}` resolve through Mojo accessor return type"
/// question (which has its own resolution path).
///
/// The anon sub is a first-class Sub symbol (`(anon)`); its
/// `return_edge` is `Symbol(sym_id)`.
/// `emit_arity_return_witnesses` publishes a `UnionOnArgs`
/// ReturnExpr with `(Empty, Concrete(String))` and `(Any,
/// Concrete(ClassName(_)))` on the symbol. The chain typer's
/// coderef arm threads `arity_hint = args.len()` into the bag
/// query so each invocation gets its arm-specific answer.
#[test]
fn anon_closure_arity_dispatch_via_coderef_call() {
    let src = "\
package Foo;
sub new { bless {}, shift }

package main;
my $cb = sub {
    return \"getter\" unless @_;
    return Foo->new;
};
my $g = $cb->();
my $w = $cb->(\"v\");
my $sentinel;
";
    let fa = parse(src);
    let pt = point_at(src, "my $sentinel");

    let g_ty = fa.inferred_type_via_bag("$g", pt);
    assert_eq!(
        g_ty,
        Some(InferredType::String),
        "anon-sub called with arity 0 must hit the `unless @_` arm \
         (returning a String literal); chain typer threads arity_hint=0 \
         through the coderef-call edge"
    );

    let w_ty = fa.inferred_type_via_bag("$w", pt);
    assert_eq!(
        w_ty,
        Some(InferredType::ClassName("Foo".into())),
        "anon-sub called with arity 1 falls through to the default arm \
         (returning Foo->new); chain typer threads arity_hint=1"
    );
}

// ---- Test F: dynamic method call ($obj->$cb()) routes through edge ----

/// `$obj->$cb()` is method-call syntax with a scalar-valued method
/// position — the parser emits `method_call_expression` with
/// `method` field shaped `(method (scalar))`. Semantically it's
/// just `$cb->($obj)`: the coderef in `$cb` runs with `$obj` as
/// the first arg.
///
/// The chain typer's `method_call_expression` arm detects the
/// scalar `method` field, resolves the scalar's
/// `CodeRef.return_edge` through the bag, and treats the call like
/// `$cb->($obj, ...)` — receiver = `$obj`'s type, arity =
/// `call_args.len()` (the coderef receives args as-is; the
/// method-call syntax is dispatch shorthand).
#[test]
fn dynamic_method_call_routes_through_coderef_edge() {
    let src = "\
package Foo;
sub new { bless {}, shift }

package main;
my $obj = Foo->new;
my $cb = sub { [1, 2] };
my $r = $obj->$cb();
my $sentinel;
";
    let fa = parse(src);
    let pt = point_at(src, "my $sentinel");
    let ty = fa.inferred_type_via_bag("$r", pt);
    assert_eq!(
        ty,
        Some(InferredType::ArrayRef),
        "dynamic-method call must chase $cb's CodeRef return_edge — \
         the closure returns an arrayref regardless of receiver, so \
         the chase resolves through the Symbol's stored return"
    );
}
