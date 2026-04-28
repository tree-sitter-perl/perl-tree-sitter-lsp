//! Type-inference invariants pinned for the worklist refactor.
//!
//! These tests assert the contracts the worklist will preserve: chain
//! convergence beyond the current 2-fold-pass threshold, plugin-override
//! survivability across folds, fixed-point idempotency, and provenance
//! accessor correctness. They MUST pass on the current implementation
//! and stay passing through every commit on
//! `refactor/type-inference-worklist`.
//!
//! Acceptance evidence (per the spec): each test fails compellingly if
//! the corresponding load-bearing piece of the current pipeline is
//! removed (the second `resolve_return_types` call, the
//! `apply_type_overrides` step, etc.) and re-passes once it's restored.

use super::*;
use crate::file_analysis::{InferredType, SymKind, SymbolDetail, TypeProvenance};
use crate::plugin::{
    CompletionQueryContext, FrameworkPlugin, OverrideTarget, PluginCompletionAnswer,
    PluginRegistry, PluginSigHelpAnswer, SigHelpQueryContext, Trigger, TypeOverride,
};
use std::sync::Arc;

fn parse(source: &str) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    parser.parse(source, None).unwrap()
}

fn build_with(source: &str, plugins: Arc<PluginRegistry>) -> FileAnalysis {
    let tree = parse(source);
    super::build_with_plugins(&tree, source.as_bytes(), plugins)
}

fn build_with_extra_re_fold(source: &str, plugins: Arc<PluginRegistry>) -> FileAnalysis {
    let tree = parse(source);
    super::build_with_plugins_extra_re_fold(&tree, source.as_bytes(), plugins)
}

// ---- Test 1: convergence over reassignment chains --------------------------

/// Two-hop reassignment chain (`make → a`): `sub a { my $x = make()->step;
/// return $x }`. Resolving `a` requires three things to all land in the
/// right order:
///   1. The first `resolve_return_types` fold types `make` (returns
///      `Foo`) and `Foo::step` (returns `Foo`).
///   2. `type_assignments_into_bag` walks the chain `make()->step` and
///      types `$x` as `Foo`.
///   3. `refresh_return_arm_types` re-evaluates `return $x` against the
///      now-typed `$x`, then the **second** `resolve_return_types` fold
///      collapses `a`'s arms to `ClassName(Foo)`.
///
/// Delete any of the three and `a` collapses to `None`. That's the
/// teeth: this test fails compellingly if the second-fold call is
/// removed (or if chain typing is skipped).
///
/// **Depth note.** Empirically the current pipeline reaches **one
/// reassignment hop**. A `b` link (`my $x = a()->step; return $x`)
/// fails today because `type_assignments_into_bag` runs *before* the
/// second fold and can't see `a`'s type at chain-typing time. The
/// spec's depth=4 probe was over-optimistic about today's behavior;
/// once Phase 6 lands the worklist driver, this test should be lifted
/// to deeper chains as a regression check on the new convergence path.
#[test]
fn convergence_chain_resolves_through_one_reassignment_hop() {
    let source = r#"
        package Foo;
        sub new { my ($class) = @_; return bless {}, $class }
        sub step { my ($self) = @_; return Foo->new }

        package main;
        sub make { return Foo->new }
        sub a { my $x = make()->step;     return $x }
    "#;
    let fa = build_with(source, super::default_plugin_registry());

    for name in ["make", "a"] {
        assert_eq!(
            fa.sub_return_type_at_arity(name, None),
            Some(InferredType::ClassName("Foo".into())),
            "chain link `{name}` should resolve to ClassName(Foo); \
             got {:?}. The post-walk fold's two-pass shape is what \
             carries this — if this fails, chain typing or the \
             second resolve_return_types call has regressed.",
            fa.sub_return_type_at_arity(name, None)
        );
    }
}

// ---- Test 2: plugin overrides survive every fold ----------------------------

/// Stub plugin that only contributes a single type override. The override
/// pins `Foo::bar`'s return type to `ClassName("Baz")` even though the
/// source's `sub bar { return undef }` would otherwise infer to nothing
/// (or arguably HashRef under some idioms — the point is the override
/// must dominate).
struct OverrideOnlyPlugin {
    id: &'static str,
    triggers: Vec<Trigger>,
    overrides: Vec<TypeOverride>,
}

impl FrameworkPlugin for OverrideOnlyPlugin {
    fn id(&self) -> &str {
        self.id
    }
    fn triggers(&self) -> &[Trigger] {
        &self.triggers
    }
    fn overrides(&self) -> &[TypeOverride] {
        &self.overrides
    }
    fn on_signature_help(&self, _ctx: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> {
        None
    }
    fn on_completion(&self, _ctx: &CompletionQueryContext) -> Option<PluginCompletionAnswer> {
        None
    }
}

fn registry_with_bar_override() -> Arc<PluginRegistry> {
    let mut reg = PluginRegistry::new();
    reg.register(Box::new(OverrideOnlyPlugin {
        id: "test-override",
        triggers: vec![Trigger::Always],
        overrides: vec![TypeOverride {
            target: OverrideTarget::Method {
                class: "Foo".into(),
                name: "bar".into(),
            },
            return_type: InferredType::ClassName("Baz".into()),
            reason: "test fixture: pin Foo::bar to Baz".into(),
        }],
    }));
    Arc::new(reg)
}

#[test]
fn plugin_override_survives_all_folds() {
    let source = r#"
        package Foo;
        sub new { my ($class) = @_; return bless {}, $class }
        sub bar { return undef }
    "#;
    let fa = build_with(source, registry_with_bar_override());

    let bar_type = fa.sub_return_type_at_arity("bar", None);
    assert_eq!(
        bar_type,
        Some(InferredType::ClassName("Baz".into())),
        "plugin override on Foo::bar must dominate inference; got {bar_type:?}"
    );

    let bar_sym = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "bar"
                && matches!(s.kind, SymKind::Sub | SymKind::Method)
                && s.package.as_deref() == Some("Foo")
        })
        .expect("Foo::bar symbol must exist");
    let prov = fa.return_type_provenance(bar_sym.id);
    match prov {
        TypeProvenance::PluginOverride { plugin_id, reason } => {
            assert_eq!(plugin_id, "test-override");
            assert!(
                reason.contains("Foo::bar to Baz"),
                "reason should propagate from TypeOverride.reason verbatim, got {reason:?}"
            );
        }
        other => panic!("expected PluginOverride provenance, got {other:?}"),
    }
}

// ---- Test 3: idempotent re-fold (observable fixed-point property) ---------

/// Pins that the post-walk fold sequence is **observably** a fixed
/// point — running it once more changes no answer the type-query API
/// returns, no Symbol's `return_type`, and no `type_provenance` entry.
/// Compares two builds of the same source: one through the standard
/// pipeline, one with an extra post-walk fold sandwiched in
/// (`build_with_plugins_extra_re_fold`).
///
/// **Witness count is *not* compared.** Today's `resolve_return_types`
/// re-emits `ArityReturn` witnesses on every invocation (it doesn't
/// track which it has already pushed), so an extra fold pass adds
/// duplicates without changing any reducer's answer. The spec's
/// stronger "no witness was duplicated" assertion is the worklist
/// driver's promise (Phase 6) — by construction it pushes each fact
/// once and terminates when the lattice stops moving. Tightening this
/// test to assert exact witness equality is a Phase 6 follow-up.
#[test]
fn post_walk_fold_is_observably_idempotent() {
    // A fixture with enough shape to exercise the major reducers:
    // framework synthesis, branch-arm fold, return-arm collection,
    // chain typing, and a delegation tail.
    let source = r#"
        package Foo;
        sub new { my ($class) = @_; return bless {}, $class }
        sub step { my ($self) = @_; return Foo->new }

        package Bar;
        use parent -norequire, 'Foo';
        sub make_two {
            my ($self, $cond) = @_;
            if ($cond) {
                return Foo->new;
            } else {
                return Foo->new;
            }
        }
        sub trampoline { my ($self) = @_; return $self->make_two(1) }

        package main;
        sub a { my $x = Bar->new->make_two(1)->step; return $x }
    "#;

    let plugins = super::default_plugin_registry();
    let fa_once = build_with(source, plugins.clone());
    let fa_twice = build_with_extra_re_fold(source, plugins);

    // Every Sub/Method's return_type must be byte-identical across
    // single-fold and double-fold runs. This is the load-bearing
    // observable: if a second fold pass flipped any sub's answer,
    // we'd be in unstable-fold territory, not "fixed-point reached".
    let return_types_of = |fa: &FileAnalysis|
        -> Vec<(String, Option<String>, Option<InferredType>)>
    {
        // Symbol.id is stable across single-fold and double-fold runs
        // (the walk produces them in the same order), so iterating
        // `symbols` directly gives a deterministic comparison key
        // without needing Ord on InferredType.
        fa.symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .map(|s| {
                let rt = match &s.detail {
                    SymbolDetail::Sub { return_type, .. } => return_type.clone(),
                    _ => None,
                };
                (s.name.clone(), s.package.clone(), rt)
            })
            .collect()
    };
    assert_eq!(
        return_types_of(&fa_once),
        return_types_of(&fa_twice),
        "extra post-walk fold pass must not change any Symbol.return_type"
    );

    // Provenance map must also be identical — re-folding shouldn't
    // re-stamp ReducerFold over a stable answer or flip an
    // already-recorded Delegation.
    assert_eq!(
        fa_once.type_provenance, fa_twice.type_provenance,
        "extra post-walk fold pass must not change type_provenance entries"
    );

    // Cross-check via the public type-query API — every Sub/Method's
    // `sub_return_type_at_arity(name, None)` must agree across runs.
    // This is the surface external callers (cursor_context, hover,
    // --dump-package) hit, so equality here is the user-visible
    // idempotency property.
    for sym in fa_once
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
    {
        let a = fa_once.sub_return_type_at_arity(&sym.name, None);
        let b = fa_twice.sub_return_type_at_arity(&sym.name, None);
        assert_eq!(
            a, b,
            "sub_return_type_at_arity({:?}) diverged: single-fold={:?} double-fold={:?}",
            sym.name, a, b
        );
    }
}

// ---- Test 4: provenance accessor + serde round-trip -------------------------

/// `return_type_provenance` is the public reader for `--dump-package`'s
/// "why does this return X?" answer. The dump-golden fixtures already
/// exercise it end-to-end; this test pins the unit-level contract:
///   * a plugin-overridden sub reports `PluginOverride { plugin_id, reason }`;
///   * a fold-derived sub reports `Inferred` (default) or `ReducerFold`/
///     `Delegation` once those variants are wired by build, but never
///     `PluginOverride` for a non-overridden sub;
///   * `TypeProvenance` round-trips through serde JSON without losing
///     variant identity.
#[test]
fn provenance_accessor_and_round_trip() {
    let source = r#"
        package Foo;
        sub new { my ($class) = @_; return bless {}, $class }
        sub bar { return undef }
        sub baz { my ($self) = @_; return Foo->new }
    "#;
    let fa = build_with(source, registry_with_bar_override());

    let find_sub = |name: &str| -> &crate::file_analysis::Symbol {
        fa.symbols
            .iter()
            .find(|s| {
                s.name == name
                    && matches!(s.kind, SymKind::Sub | SymKind::Method)
                    && s.package.as_deref() == Some("Foo")
            })
            .unwrap_or_else(|| panic!("expected sub Foo::{name} in fixture"))
    };

    // Overridden sub: provenance is PluginOverride.
    let bar = find_sub("bar");
    let bar_prov = fa.return_type_provenance(bar.id);
    assert!(
        matches!(bar_prov, TypeProvenance::PluginOverride { .. }),
        "Foo::bar should report PluginOverride, got {bar_prov:?}"
    );

    // Non-overridden sub: provenance must NOT be PluginOverride. It
    // can be `Inferred` (default) or one of the derived variants
    // (`ReducerFold` / `Delegation`); we only assert the negative —
    // an override leak here would be a regression.
    let baz = find_sub("baz");
    let baz_prov = fa.return_type_provenance(baz.id);
    assert!(
        !matches!(baz_prov, TypeProvenance::PluginOverride { .. }),
        "Foo::baz must not carry PluginOverride provenance \
         (no override targets it); got {baz_prov:?}"
    );

    // Serde round-trip — every enum variant must survive
    // serialize → deserialize without shape loss. `--dump-package`
    // and the SQLite cache both rely on this.
    let cases = vec![
        TypeProvenance::Inferred,
        TypeProvenance::PluginOverride {
            plugin_id: "p".into(),
            reason: "r".into(),
        },
        TypeProvenance::ReducerFold {
            reducer: "framework_aware".into(),
            evidence: vec!["framework=Moo".into(), "arms=2 agree".into()],
        },
        TypeProvenance::Delegation {
            kind: "self_method_tail".into(),
            via: "_route".into(),
        },
    ];
    for orig in cases {
        let encoded = serde_json::to_string(&orig).expect("serialize");
        let decoded: TypeProvenance =
            serde_json::from_str(&encoded).expect("deserialize");
        assert_eq!(orig, decoded, "round-trip lost variant: {encoded}");
    }
}
