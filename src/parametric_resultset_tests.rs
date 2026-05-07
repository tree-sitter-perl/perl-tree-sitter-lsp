//! External-behavior pins for residual Part 5c — parametric types
//! over DBIC ResultSet shapes. Tests use `find_definition` /
//! `crate::resolve::refs_to` only, so they're agnostic to the
//! internal encoding of the parametric type (single `type_arg:
//! String` vs `type_args: Vec<String>` vs nested `TypeArg` enum).
//! Pins the external surface; encoding is settled separately.
//!
//! ALL TESTS IN THIS FILE FAIL TODAY — they're TDD pins for the
//! Part 5c implementation. See `docs/prompt-type-inference-residual.md`
//! Part 5c for the spec.

use super::*;
use std::path::PathBuf;
use std::sync::Arc;
use tree_sitter::{Parser, Point, Tree};

use crate::file_store::{FileKey, FileStore};
use crate::module_index::ModuleIndex;
use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};

fn parse(source: &str) -> FileAnalysis {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

/// Parse + build, returning both the FA and the tree. The
/// hash-key-arg consumer in `find_definition` needs the tree to
/// know cursor-on-key vs cursor-on-method, so most tests here
/// thread it through.
fn parse_with_tree(source: &str) -> (FileAnalysis, Tree) {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    let fa = crate::builder::build(&tree, source.as_bytes());
    (fa, tree)
}

/// Locate `needle` in `source` and return a Point at the FIRST
/// character past its start (so a needle of `"name =>"` lands the
/// cursor on the `n` of `name`). Panics on miss — fixtures are
/// small enough that ambiguity is a test bug.
fn point_at(source: &str, needle: &str) -> Point {
    let byte = source
        .find(needle)
        .unwrap_or_else(|| panic!("needle {:?} not in source:\n{}", needle, source));
    let row = source[..byte].matches('\n').count();
    let col = byte - source[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0);
    Point::new(row, col)
}

/// Schema::Result::Users with two columns. Reused across most of
/// the fixtures so the line numbers stay stable.
const USERS_RESULT: &str = "package Schema::Result::Users;
use base 'DBIx::Class::Core';
__PACKAGE__->add_columns(
    name  => { data_type => 'varchar' },
    email => { data_type => 'varchar' },
);
1;
";

// Row indices of column defs in USERS_RESULT (0-indexed):
//   row 3: `    name  => { ... }`
//   row 4: `    email => { ... }`
const NAME_COL_DEF_ROW: usize = 3;
const EMAIL_COL_DEF_ROW: usize = 4;

/// (a) **Happy path**. `$schema->resultset('Users')->search({ name => 'X' })`
/// — cursor on `name` in the search args lands on the `name`
/// column def synthesized by `add_columns`.
#[test]
fn goto_def_through_resultset_chain() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->search({{ name => 'X' }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    // Cursor on `name` in the search-arg hash. Pin column-def row
    // by name (hardcoded line numbers rot when fixtures shift).
    let pt = point_at(&src, "name => 'X'");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(NAME_COL_DEF_ROW),
        "cursor on `name` in $$schema->resultset('Users')->search({{ name => ... }}) \
         should land on the `name` column def in add_columns",
    );
}

/// (b) **Variable rebound**. `my $rs = $schema->resultset('Users');
/// $rs->search({ name => ... })` — the parametric type flows
/// through the variable assignment.
#[test]
fn goto_def_via_rebound_resultset_variable() {
    let src = format!(
        "{}
package main;
my $schema;
my $rs = $schema->resultset('Schema::Result::Users');
$rs->search({{ email => 'x@y' }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "email => 'x@y'");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(EMAIL_COL_DEF_ROW),
        "the parametric type must thread through `my $$rs = …->resultset(…)`; \
         got def: {:?}",
        def,
    );
}

/// (c) **Chained search**. `$rs->search({...})->search({ name => ... })`
/// — search-family methods preserve the parametric type, so each
/// hop's hash-key completion still resolves against the row class.
#[test]
fn goto_def_through_chained_search() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->search({{ name => 'A' }})->search({{ email => 'B' }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "email => 'B'");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(EMAIL_COL_DEF_ROW),
        "chained search must keep the parametric type — `email` on the second \
         hop must still resolve against Users",
    );
}

/// (d) **`find` shape**. `$rs->find({ id => 1 })` — `find` is in
/// the search family per the spec; same column resolution as
/// search.
#[test]
fn goto_def_through_find() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->find({{ name => 'X' }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "name => 'X'");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(NAME_COL_DEF_ROW),
        "`->find` must thread the parametric type the same way `->search` does",
    );
}

/// (e) **Receiver dispatch unchanged**. `$rs->all()` and
/// `$rs->count()` are ResultSet methods; the parametric type must
/// NOT make them resolve against the row class. We don't define
/// `all` or `count` anywhere in the fixture, so a regression
/// (where Parametric pretends to be the row class for *every*
/// purpose) would surface as goto-def landing on… nothing on the
/// row class either, but the failure mode we want to pin is "does
/// not hallucinate". Easiest pin: assert that goto-def on `all`
/// returns None — neither the row class nor anything else has an
/// `all` method.
#[test]
fn receiver_method_dispatch_not_redirected_to_row_class() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->all();
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "all()");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    // Either None (no DBIx::Class::ResultSet stub in fixture) or a
    // span pointing into a ResultSet class — but NEVER a span on
    // the Users row-class file. The negative pin is "doesn't
    // resolve to a row-class method," because that's the failure
    // mode a too-aggressive parametric redirect would produce.
    if let Some(span) = def {
        assert!(
            span.start.row > EMAIL_COL_DEF_ROW + 1,
            "`->all()` must not resolve to a position inside the Users \
             row-class definition. got span at row {}",
            span.start.row,
        );
    }
}

/// (f) **Wrong-column negative**. `$rs->search({ not_a_column => 1 })`
/// — the type_arg is honored, so unknown keys don't resolve to
/// some random matching column elsewhere in the workspace.
#[test]
fn goto_def_for_unknown_column_returns_nothing() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->search({{ definitely_not_a_column => 1 }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "definitely_not_a_column");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert!(
        def.is_none(),
        "unknown column must not resolve to anything; got: {:?}",
        def,
    );
}

/// (g) **Cross-file**. Producer file defines `Schema::Result::Users`
/// with `add_columns`; consumer file does
/// `$schema->resultset('Schema::Result::Users')->search({ KEY })`.
/// The parametric type must compose through cross-file enrichment
/// — the column def lives in the producer, the call site in the
/// consumer; refs_to(HashKeyOfClass) finds the consumer's hash-key
/// access ref pointing at the producer's column def.
#[test]
fn cross_file_resultset_column_resolution() {
    let producer_src = USERS_RESULT;
    let consumer_src = "use Schema::Result::Users;
package main;
my $schema;
$schema->resultset('Schema::Result::Users')->search({ name => 'X' });
1;
";

    let producer_path = PathBuf::from("/tmp/parametric_users.pm");
    let consumer_path = PathBuf::from("/tmp/parametric_consumer.pm");

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(producer_path.clone(), Arc::new(parse(producer_src)));

    let mut consumer_fa = parse(consumer_src);
    consumer_fa.enrich_imported_types_with_keys(Some(&idx));

    let store = FileStore::new();
    store.insert_workspace(producer_path.clone(), parse(producer_src));
    store.insert_workspace(consumer_path.clone(), consumer_fa);

    let target = TargetRef {
        name: "name".to_string(),
        kind: TargetKind::HashKeyOfClass("Schema::Result::Users".to_string()),
    };
    let refs = refs_to(&store, Some(&idx), &target, RoleMask::WORKSPACE);
    let consumer_hit = refs
        .iter()
        .any(|r| matches!(&r.key, FileKey::Path(p) if p == &consumer_path));
    assert!(
        consumer_hit,
        "refs_to(HashKeyOfClass(Schema::Result::Users), name) must include the \
         consumer's $$schema->resultset('…')->search({{ name => … }}) call site. \
         the parametric type must compose through cross-file enrichment. hits: {:?}",
        refs.iter().map(|r| (&r.key, r.span.start.row)).collect::<Vec<_>>(),
    );
    // Also: the producer's column def itself shows up (declaration entry).
    let producer_hit = refs
        .iter()
        .any(|r| matches!(&r.key, FileKey::Path(p) if p == &producer_path));
    assert!(
        producer_hit,
        "refs_to must include the producer's `name` column def. hits: {:?}",
        refs.iter().map(|r| (&r.key, r.span.start.row)).collect::<Vec<_>>(),
    );
}

/// (k) **Row-class method dispatch on `->find` result**.
/// `$schema->resultset('Users')->find($id)->name` — cursor on
/// `name` resolves to the row-class column accessor. The
/// idiomatic DBIC pattern (`$row->{name}` works only via
/// `DBIx::Class::ResultClass::HashRefInflator` which we don't
/// support; method-call access via the synthesized accessor is
/// the supported path).
///
/// Status: red-pin. Requires per-method return-type projection:
///   * `->search` / `->search_rs` preserve `Parametric{B, [T]}`
///   * `->find` / `->first` / `->single` / `->next` / `->create`
///     project to `ClassName(T)` (return a single row, not a
///     resultset)
///
/// Today the builder doesn't model either projection — `find`
/// resolves to the cross-file `ResultSet::find` return type,
/// which is unknown without a stub. Queued with the
/// DBIC-as-plugin work; the per-method projection rules belong
/// to the plugin's parametric-semantics declaration (see
/// `docs/prompt-dbic-as-plugin.md` + `docs/prompt-parametric-semantics.md`).
#[test]
#[ignore = "per-method return-type projection (search→preserve, find→row) queued with DBIC plugin work"]
fn method_dispatch_through_find_resolves_to_row_class() {
    let src = format!(
        "{}
package main;
my $schema;
my $name = $schema->resultset('Schema::Result::Users')->find(1)->name;
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    // Cursor on `name` in `->name` (the trailing method call).
    let pt = point_at(&src, "->name");
    let pt = Point::new(pt.row, pt.column + 2);
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(NAME_COL_DEF_ROW),
        "$$row->name where $$row = $$rs->find(...) must dispatch \
         against the row class. Requires `find` to project \
         Parametric{{ResultSet, [Users]}} → ClassName(Users) at \
         the return-type. got: {:?}",
        def,
    );
}

/// (j) **Custom ResultSet method discovery**. `$schema->resultset('Users')`
/// should offer methods defined on the `*::ResultSet::Users`
/// class (a custom resultset that inherits from
/// `DBIx::Class::ResultSet` and adds project-specific helpers like
/// `admins`, `recently_active`, etc.). Today the Parametric base
/// is hard-coded to `DBIx::Class::ResultSet`, so custom methods
/// don't resolve — pinned as a sanity test the user will
/// definitely notice when it changes. Expected to FAIL until
/// resultset_class discovery lands (a follow-up phase). Documented
/// so the gap is visible in the test list, not invisible in the
/// roadmap.
#[test]
#[ignore = "custom resultset_class discovery — follow-up phase, see prompt-type-inference-residual.md Part 5c"]
fn goto_def_offers_custom_resultset_method() {
    let src = r#"
package Schema::Result::Users;
use base 'DBIx::Class::Core';
__PACKAGE__->add_columns(
    name  => { data_type => 'varchar' },
);

package Schema::ResultSet::Users;
use base 'DBIx::Class::ResultSet';
sub admins { my $self = shift; $self->search({ is_admin => 1 }) }

package main;
my $schema;
$schema->resultset('Schema::Result::Users')->admins;
"#;
    let (fa, tree) = parse_with_tree(src);
    let pt = point_at(src, "->admins");
    // Cursor lands on `-` before `admins`; bump past `->` to `a`.
    let pt = Point::new(pt.row, pt.column + 2);
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    // The admins sub is in package Schema::ResultSet::Users,
    // declared on a single line. Goto-def should land on its
    // declaration row (the 8th line of the fixture, 0-indexed
    // accounting for the leading newline).
    assert!(
        def.is_some(),
        "$$schema->resultset('Users')->admins must resolve to the \
         custom resultset_class's `admins` method. got: {:?}. \
         Requires discovering `*::ResultSet::Users` as the \
         Parametric base instead of hard-coding `DBIx::Class::ResultSet`.",
        def,
    );
}

/// (i) **Negative — wrong method name**. Only `resultset` (and
/// search-family follow-ups) carry the parametric type. A
/// same-shape method with a different name (`->frobnicate('Users')`)
/// must NOT silently emit a parametric witness, otherwise
/// completion for arbitrary method calls with string-literal
/// first args would fire false positives.
#[test]
fn unrelated_method_does_not_emit_parametric() {
    let src = format!(
        "{}
package main;
my $schema;
$schema->frobnicate('Schema::Result::Users')->search({{ name => 'X' }});
",
        USERS_RESULT,
    );
    let (fa, tree) = parse_with_tree(&src);
    let pt = point_at(&src, "name => 'X'");
    let def = fa.find_definition(pt, Some(&tree), Some(src.as_bytes()), None);
    // `frobnicate` is not `resultset` — no parametric witness
    // emitted. The chain hop's invocant is therefore unknown,
    // hash-key resolution has no class to look up, def returns
    // None.
    assert!(
        def.is_none(),
        "`frobnicate('Users')` must not be treated as resultset; \
         goto-def on the chained search arg should return None. got: {:?}",
        def,
    );
}
