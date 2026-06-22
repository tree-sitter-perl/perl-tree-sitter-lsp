//! Tests for flow-sensitive narrowing + the `Optional`/`Undef` lattice
//! it drives (`src/builder/narrowing.rs`). A child of the `tests`
//! module so it shares `build_fa`.

use super::*;
use crate::file_analysis::{FileAnalysis, InferredType, TypeProvenance};
use tree_sitter::Point;

// ── Flow-sensitive narrowing (docs/adr/flow-narrowing.md) ──

#[test]
fn narrow_block_if_isa() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo')) {\n        $x->go;\n    }\n    $x->go;\n}",
    );
    // Inside the then-block: narrowed to Foo.
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "isa guard narrows inside the block",
    );
    // After the block: widened back (untyped param → None).
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(6, 4)),
        None,
        "narrowing does not leak past the block",
    );
}

#[test]
fn narrow_block_ref_eq_reftype() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if (ref($x) eq 'HASH') {\n        my $v = $x;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 16)),
        Some(InferredType::HashRef),
        "ref() eq 'HASH' narrows to HashRef",
    );
}

#[test]
fn narrow_early_return_unless() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    return unless $x->isa('Widget');\n    $x->layout;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 4)),
        Some(InferredType::ClassName("Widget".into())),
        "return-unless asserts the type over the remainder",
    );
}

#[test]
fn narrow_logical_or_exit() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    $x->isa('Session') or die;\n    $x->go;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 4)),
        Some(InferredType::ClassName("Session".into())),
        "`G or die` narrows the remainder",
    );
}

#[test]
fn narrow_observed_through_method_dispatch() {
    // The invocant of `$x->paint` inside the block resolves to Foo, so a
    // method call dispatches on the narrowed class (the query seam).
    let fa = build_fa(
        "package Foo;\nsub paint { my ($self) = @_; }\npackage P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo')) {\n        $x->paint;\n    }\n}",
    );
    let r = fa
        .refs
        .iter()
        .find(|r| r.target_name == "paint" && matches!(r.kind, RefKind::MethodCall { .. }))
        .expect("paint method-call ref");
    assert_eq!(
        fa.method_call_invocant_class(r, None).as_deref(),
        Some("Foo"),
        "narrowed invocant dispatches on Foo",
    );
}

#[test]
fn narrow_negative_else_not_applied() {
    // The else-branch knows `$x` is NOT Foo — no positive type to assert.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo')) {\n        $x->a;\n    } else {\n        $x->b;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "then-branch still narrows",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(6, 8)),
        None,
        "else-branch is not narrowed (negative polarity, deferred)",
    );
}

#[test]
fn narrow_negated_guard_skips_then_block() {
    // `if (!G) { BODY }` proves NOT-Foo in BODY — negative, so no narrowing.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if (!$x->isa('Foo')) {\n        $x->a;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 8)),
        None,
        "negated guard does not positively narrow its block",
    );
}

#[test]
fn narrow_truncated_at_reassignment() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo')) {\n        $x->before;\n        $x = bar();\n        $x->after;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "narrowed before the reassignment",
    );
    assert_ne!(
        fa.inferred_type_via_bag("$x", Point::new(6, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "reassignment truncates the narrowing region",
    );
}

#[test]
fn narrow_conjunction_intersects() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo') && $x->can('m')) {\n        $x->go;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "&&-chain narrows on the isa conjunct",
    );
}

// ── Optional types: `return undef` ──

#[test]
fn optional_return_undef_or_value() {
    let fa = build_fa(
        "package P;\nsub maybe_make {\n    my ($ok) = @_;\n    return undef unless $ok;\n    return Foo->new;\n}",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("maybe_make", None),
        Some(InferredType::Optional(Box::new(InferredType::ClassName("Foo".into())))),
        "return undef + return Foo->new folds to Optional<Foo>",
    );
}

#[test]
fn optional_not_applied_without_undef_arm() {
    // No undef arm → plain ClassName, not Optional (existing behavior).
    let fa = build_fa(
        "package P;\nsub make {\n    my ($ok) = @_;\n    return Foo->new;\n}",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("make", None),
        Some(InferredType::ClassName("Foo".into())),
    );
}

#[test]
fn optional_defined_narrows_to_inner() {
    // maybe() : Optional<Foo>; `defined $r` strips it so $r->go dispatches Foo.
    let fa = build_fa(
        "package P;\nsub maybe { return undef unless 1; return Foo->new; }\nsub use_it {\n    my $r = maybe();\n    return unless defined $r;\n    $r->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn optional_blessed_narrows_to_inner() {
    let fa = build_fa(
        "package P;\nsub maybe { return undef unless 1; return Foo->new; }\nsub use_it {\n    my $r = maybe();\n    return unless blessed $r;\n    $r->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn optional_without_defined_does_not_dispatch() {
    // Without the guard, $r stays Optional<Foo>, which dispatches nowhere
    // (an optional is not definitely an instance).
    let fa = build_fa(
        "package P;\nsub maybe { return undef unless 1; return Foo->new; }\nsub use_it {\n    my $r = maybe();\n    $r->go;\n}",
    );
    assert_eq!(
        invocant_class_of(&fa, "go").as_deref(),
        None,
        "Optional<Foo> must be narrowed before it can dispatch",
    );
}

#[test]
fn optional_ternary_production() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($c) = @_;\n    my $x = $c ? Foo->new : undef;\n    $x;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 4)),
        Some(InferredType::Optional(Box::new(InferredType::ClassName("Foo".into())))),
        "$c ? Foo->new : undef produces Optional<Foo>",
    );
}

#[test]
fn optional_ternary_empty_list_arm() {
    // `()` coerces to undef in scalar context, so it's an undef arm too.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($c) = @_;\n    my $x = $c ? Foo->new : ();\n    $x;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 4)),
        Some(InferredType::Optional(Box::new(InferredType::ClassName("Foo".into())))),
        "$c ? Foo->new : () produces Optional<Foo>",
    );
}

#[test]
fn optional_ternary_then_defined_narrows() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($c) = @_;\n    my $x = $c ? Foo->new : undef;\n    return unless defined $x;\n    $x->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn optional_ternary_conflict_stays_none() {
    // Two distinct concrete arms (no undef) still disagree → None.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($c) = @_;\n    my $x = $c ? Foo->new : Bar->new;\n    $x;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 4)),
        None,
        "Foo vs Bar is genuine disagreement, not Optional",
    );
}

// ── Negative lattice: Undef on the `defined` family ──

#[test]
fn negative_return_if_defined_is_undef() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    return if defined $x;\n    my $y = $x;\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 12)),
        Some(InferredType::Undef),
        "after `return if defined $x`, $x is undef",
    );
}

#[test]
fn negative_unless_defined_block_is_undef() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    unless (defined $x) {\n        my $y = $x;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 16)),
        Some(InferredType::Undef),
        "inside `unless (defined $x)`, $x is undef",
    );
}

#[test]
fn negative_defined_then_else_split() {
    // then-branch strips Optional<Foo> → Foo; else-branch → Undef.
    let fa = build_fa(
        "package P;\nsub maybe { return undef unless 1; return Foo->new; }\nsub f {\n    my $r = maybe();\n    if (defined $r) {\n        $r->a;\n    } else {\n        my $y = $r;\n    }\n}",
    );
    assert_eq!(invocant_class_of(&fa, "a").as_deref(), Some("Foo"));
    assert_eq!(
        fa.inferred_type_via_bag("$r", Point::new(7, 16)),
        Some(InferredType::Undef),
        "else-branch: $r is undef",
    );
}

#[test]
fn negative_isa_else_stays_wide() {
    // `isa` has no representable complement, so the else is NOT narrowed.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    if ($x->isa('Foo')) {\n        $x->a;\n    } else {\n        my $y = $x;\n    }\n}",
    );
    assert_eq!(invocant_class_of(&fa, "a").as_deref(), Some("Foo"));
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(6, 16)),
        None,
        "isa else has no positive target — stays wide, not Undef",
    );
}

// ── Place narrowing: `$self->{x}` ──

/// Class an invocant resolves to for the method-call ref named `method`.
fn invocant_class_of(fa: &FileAnalysis, method: &str) -> Option<String> {
    let r = fa
        .refs
        .iter()
        .find(|r| r.target_name == method && matches!(r.kind, RefKind::MethodCall { .. }))
        .unwrap_or_else(|| panic!("no method-call ref for {method}"));
    fa.method_call_invocant_class(r, None)
}

#[test]
fn narrow_place_block_if_isa() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    if ($self->{h}->isa('Foo')) {\n        $self->{h}->go;\n    }\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$self->{h}", Point::new(4, 8)),
        Some(InferredType::ClassName("Foo".into())),
        "place isa guard narrows the slot inside the block",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn narrow_place_early_return() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{cfg}->isa('Cfg');\n    $self->{cfg}->load;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "load").as_deref(), Some("Cfg"));
}

#[test]
fn narrow_place_method_on_value_keeps_slot() {
    // A method call on the slot's VALUE doesn't disturb the slot, so both
    // uses stay narrowed.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{db}->isa('Schema');\n    $self->{db}->connect;\n    $self->{db}->disconnect;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "connect").as_deref(), Some("Schema"));
    assert_eq!(invocant_class_of(&fa, "disconnect").as_deref(), Some("Schema"));
}

#[test]
fn narrow_place_truncated_by_slot_write() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{x}->isa('Foo');\n    $self->{x}->before;\n    $self->{x} = other();\n    $self->{x}->after;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "before").as_deref(), Some("Foo"));
    assert_ne!(
        invocant_class_of(&fa, "after").as_deref(),
        Some("Foo"),
        "writing the slot truncates the narrowing",
    );
}

#[test]
fn narrow_place_truncated_by_opaque_root_op() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{x}->isa('Foo');\n    $self->{x}->before;\n    $self->reset;\n    $self->{x}->after;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "before").as_deref(), Some("Foo"));
    assert_ne!(
        invocant_class_of(&fa, "after").as_deref(),
        Some("Foo"),
        "an opaque op on the root ($self->reset) truncates the narrowing",
    );
}

#[test]
fn narrow_place_dynamic_key_skipped() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self, $k) = @_;\n    return unless $self->{$k}->isa('Foo');\n    $self->{$k}->go;\n}",
    );
    assert_ne!(
        invocant_class_of(&fa, "go").as_deref(),
        Some("Foo"),
        "a dynamic key is not a stable place — no narrowing",
    );
}

#[test]
fn narrow_place_multihop_isa() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{a}{b}->isa('Foo');\n    $self->{a}{b}->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn narrow_place_multihop_method_on_value_keeps_slot() {
    // A method call on the deep slot's value doesn't disturb it.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{a}{b}->isa('Foo');\n    $self->{a}{b}->one;\n    $self->{a}{b}->two;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "one").as_deref(), Some("Foo"));
    assert_eq!(invocant_class_of(&fa, "two").as_deref(), Some("Foo"));
}

#[test]
fn narrow_place_multihop_truncated_by_intermediate_prefix_op() {
    // A method call on the intermediate prefix ($self->{a}) could mutate the
    // object holding {b}, so it truncates the deeper narrowing.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{a}{b}->isa('Foo');\n    $self->{a}{b}->before;\n    $self->{a}->mutate;\n    $self->{a}{b}->after;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "before").as_deref(), Some("Foo"));
    assert_ne!(
        invocant_class_of(&fa, "after").as_deref(),
        Some("Foo"),
        "an op on the intermediate prefix truncates the deep narrowing",
    );
}

#[test]
fn narrow_place_multihop_sibling_write_keeps_slot() {
    // Writing a sibling slot ($self->{a}{c}) doesn't disturb $self->{a}{b}.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless $self->{a}{b}->isa('Foo');\n    $self->{a}{c} = 1;\n    $self->{a}{b}->after;\n}",
    );
    assert_eq!(
        invocant_class_of(&fa, "after").as_deref(),
        Some("Foo"),
        "a sibling-slot write does not truncate",
    );
}

#[test]
fn narrow_place_ref_eq() {
    let fa = build_fa(
        "package P;\nsub f {\n    my ($self) = @_;\n    return unless ref($self->{cfg}) eq 'HASH';\n    my $v = $self->{cfg};\n}",
    );
    assert_eq!(
        fa.inferred_type_via_bag("$self->{cfg}", Point::new(4, 12)),
        Some(InferredType::HashRef),
        "ref(place) eq 'HASH' narrows the slot",
    );
}

#[test]
fn optional_bare_return_then_defined() {
    // The dominant idiom: `return unless …; return Obj->new` is Optional,
    // and a `defined` guard at the call site recovers the class.
    let fa = build_fa(
        "package P;\nsub maybe { return unless 1; return Foo->new; }\nsub use_it {\n    my $r = maybe();\n    return unless defined $r;\n    $r->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}

#[test]
fn optional_return_provenance() {
    // --dump-package should explain an Optional return via the join.
    let fa = build_fa("package P;\nsub maybe { return undef unless 1; return Foo->new; }\n");
    let sid = fa.symbols.iter().find(|s| s.name == "maybe").unwrap().id;
    match fa.type_provenance.get(&sid) {
        Some(TypeProvenance::ReducerFold { evidence, .. }) => assert!(
            evidence.iter().any(|e| e == "optional_join"),
            "evidence should record the optional join, got {evidence:?}",
        ),
        other => panic!("expected ReducerFold provenance, got {other:?}"),
    }
}

#[test]
fn optional_maybe_isa_scalar() {
    let fa = build_fa("package P;\nuse Moo;\nhas x => (is => 'ro', isa => 'Maybe[Int]');\n");
    assert_eq!(
        fa.sub_return_type_at_arity("x", Some(0)),
        Some(InferredType::Optional(Box::new(InferredType::Numeric))),
        "Maybe[Int] accessor returns Optional<Numeric>",
    );
}

#[test]
fn optional_maybe_isa_instance() {
    let fa = build_fa(
        "package P;\nuse Moo;\nhas thing => (is => 'ro', isa => 'Maybe[InstanceOf[\"Foo\"]]');\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::Optional(Box::new(InferredType::ClassName("Foo".into())))),
        "Maybe[InstanceOf['Foo']] accessor returns Optional<Foo>",
    );
}

// ── cst reuse: grouping peel + plain-literal strictness flow through
//    the shared `cst` primitives (cst::peel_groups /
//    cst::first_named_child_where / cst::plain_string_literal_text) ──


#[test]
fn narrow_isa_interpolated_arg_stays_wide() {
    // plain_string_literal_text rejects interpolation, so `isa("$cls")`
    // is NOT a literal class-name guard and must not narrow.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x, $cls) = @_;\n    return unless $x->isa(\"$cls\");\n    $x->go;\n}",
    );
    assert_eq!(
        invocant_class_of(&fa, "go").as_deref(),
        None,
        "interpolated isa arg is not a literal class name",
    );
}

#[test]
fn narrow_ref_eq_paren_wrapped_arg() {
    // first_named_child_where peels the `(...)` around the ref() operand.
    let fa = build_fa(
        "package P;\nsub f {\n    my ($x) = @_;\n    return unless ref(($x)) eq 'Foo';\n    $x->go;\n}",
    );
    assert_eq!(invocant_class_of(&fa, "go").as_deref(), Some("Foo"));
}
