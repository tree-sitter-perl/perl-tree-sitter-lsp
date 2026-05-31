use super::*;
use tree_sitter::Point;

fn p(row: usize, col: usize) -> Point {
    Point { row, column: col }
}

fn span(r0: usize, c0: usize, r1: usize, c1: usize) -> Span {
    Span {
        start: p(r0, c0),
        end: p(r1, c1),
    }
}

fn wvar(name: &str, scope: u32, payload: WitnessPayload) -> Witness {
    Witness {
        attachment: WitnessAttachment::Variable {
            name: name.to_string(),
            scope: ScopeId(scope),
        },
        source: WitnessSource::Builder("test".into()),
        payload,
        span: span(0, 0, 0, 0),
    }
}

// ---- Phase 0: bag stores and retrieves ----

#[test]
fn witness_bag_stores_and_retrieves_by_attachment() {
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::FirstParamInMethod {
            package: "Foo".into(),
        }),
    ));
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::HashRefAccess),
    ));
    bag.push(wvar(
        "$other",
        0,
        WitnessPayload::Observation(TypeObservation::ArrayRefAccess),
    ));

    let att = WitnessAttachment::Variable {
        name: "$self".into(),
        scope: ScopeId(0),
    };
    let hits = bag.for_attachment(&att);
    assert_eq!(hits.len(), 2);
}

// ---- Part 6 core bug: Mojo sub name, blessed-hashref ----

#[test]
fn mojo_sub_name_does_not_flip_type_to_hashref() {
    // `sub name { my $self = shift; return $self->{name} unless @_; … }`
    // inside a Mojo::Base class — `$self` is blessed, and the
    // hashref access is *backing-rep consistent* with Mojo::Base.
    // The old flat-enum inference would overwrite ClassName with
    // HashRef. The resolver must keep the class.
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::FirstParamInMethod {
            package: "Mojolicious::Routes::Route".into(),
        }),
    ));
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::HashRefAccess),
    ));

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$self".into(),
        scope: ScopeId(0),
    };
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::MojoBase,
        arity_hint: None, receiver: None, context: None,
    };
    let v = reg.query(&bag, &q);
    assert_eq!(
        v,
        ReducedValue::Type(InferredType::ClassName("Mojolicious::Routes::Route".into()))
    );
}

// ---- Sanity: no class evidence — plain hash access folds to HashRef ----

#[test]
fn plain_hashref_access_without_class_evidence() {
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$cfg",
        1,
        WitnessPayload::Observation(TypeObservation::HashRefAccess),
    ));

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$cfg".into(),
        scope: ScopeId(1),
    };
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::HashRef)
    );
}

// ---- Arrayref-backed class via bless target ----

#[test]
fn bless_target_array_with_class_assertion_keeps_class() {
    // `bless [], $c` in scope + `$x = Foo->new` → $x is ClassName,
    // rep is Array. Access as `$x->[0]` matches rep; class wins.
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$x",
        2,
        WitnessPayload::Observation(TypeObservation::ClassAssertion("Arr::Foo".into())),
    ));
    bag.push(wvar(
        "$x",
        2,
        WitnessPayload::Observation(TypeObservation::BlessTarget(Rep::Array)),
    ));
    bag.push(wvar(
        "$x",
        2,
        WitnessPayload::Observation(TypeObservation::ArrayRefAccess),
    ));

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$x".into(),
        scope: ScopeId(2),
    };
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::ClassName("Arr::Foo".into()))
    );
}

// ---- CoreClass (Perl 5.38) — hashref access is a contradiction ----

#[test]
fn core_class_still_holds_class_against_hashref_access() {
    // Perl 5.38 `class` uses opaque fields — `$self->{k}` inside
    // is really a bug, but the type should stay ClassName so the
    // rest of the chain types correctly. A separate diagnostic
    // pass would warn.
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::FirstParamInMethod {
            package: "MyApp::Thing".into(),
        }),
    ));
    bag.push(wvar(
        "$self",
        0,
        WitnessPayload::Observation(TypeObservation::HashRefAccess),
    ));

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$self".into(),
        scope: ScopeId(0),
    };
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::CoreClass,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::ClassName("MyApp::Thing".into()))
    );
}

// ---- Context observations ----

// ---- Part 6 + 7 together: fluent chain via Expression-attached Edge ----

#[test]
fn fluent_chain_get_to_resolves_via_edge_chase() {
    // `$r->get('/x')->to('Y#z')` — the RefIdx for `->get(...)`'s
    // result carries `Edge(Symbol(get_sid))`. Registry materialization
    // chases the edge through `Symbol(get_sid)` (where the resolved
    // return type lives, published by writeback), finds Route, and
    // the chain hop sees a plain `InferredType::ClassName`.
    let mut bag = WitnessBag::new();
    let get_ref = RefIdx(42);
    let get_sid = SymbolId(7);
    bag.push(Witness {
        attachment: WitnessAttachment::Expression(get_ref),
        source: WitnessSource::Builder("chain".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Symbol(get_sid)),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(get_sid),
        source: WitnessSource::Builder("local_return".into()),
        payload: WitnessPayload::InferredType(InferredType::ClassName(
            "Mojolicious::Routes::Route".into(),
        )),
        span: span(0, 0, 0, 0),
    });

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Expression(get_ref);
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::ClassName("Mojolicious::Routes::Route".into()))
    );
}

// Edge with no resolution at the target attachment drops out — the
// query falls through as if the witness wasn't there.
#[test]
fn edge_with_unresolved_target_yields_none() {
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Expression(RefIdx(7)),
        source: WitnessSource::Builder("chain".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Symbol(SymbolId(99))),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Expression(RefIdx(7));
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(reg.query(&bag, &q), ReducedValue::None);
}

// Cycle guard: Edge(A) → Edge(B) → Edge(A) terminates returning None
// rather than recursing forever.
#[test]
fn edge_chase_terminates_on_cycle() {
    let mut bag = WitnessBag::new();
    let a = WitnessAttachment::Symbol(SymbolId(1));
    let b = WitnessAttachment::Symbol(SymbolId(2));
    bag.push(Witness {
        attachment: a.clone(),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::Edge(b.clone()),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: b.clone(),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::Edge(a.clone()),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let q = ReducerQuery {
        attachment: &a,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(reg.query(&bag, &q), ReducedValue::None);
}

// Transitive resolution: Edge(A) → Edge(B) → Type(t) lands t at A.
#[test]
fn edge_chase_resolves_transitively() {
    let mut bag = WitnessBag::new();
    let a = WitnessAttachment::Symbol(SymbolId(1));
    let b = WitnessAttachment::Symbol(SymbolId(2));
    bag.push(Witness {
        attachment: a.clone(),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::Edge(b.clone()),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: b.clone(),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::InferredType(InferredType::String),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let q = ReducerQuery {
        attachment: &a,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::String)
    );
}

// ---- Part 5b — narrowing via scoped-span InferredType witnesses ----

#[test]
fn narrowed_span_wins_over_outer_witness_at_inside_point() {
    // Outer scope says $v: HashRef. An `if (ref $v eq 'ARRAY')`
    // body at rows 4..8 narrows $v to ArrayRef. A query at
    // row=5 (inside the body) must see ArrayRef; a query at
    // row=2 (before the if) must see HashRef.
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Variable {
            name: "$v".into(),
            scope: ScopeId(0),
        },
        source: WitnessSource::Builder("outer".into()),
        payload: WitnessPayload::InferredType(InferredType::HashRef),
        span: span(0, 0, 0, 0), // zero = outer / unconstrained
    });
    bag.push(Witness {
        attachment: WitnessAttachment::Variable {
            name: "$v".into(),
            scope: ScopeId(0),
        },
        source: WitnessSource::Builder("narrowing".into()),
        payload: WitnessPayload::InferredType(InferredType::ArrayRef),
        span: span(4, 0, 8, 0),
    });

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$v".into(),
        scope: ScopeId(0),
    };

    let inside = ReducerQuery {
        attachment: &att,
        point: Some(p(5, 4)),
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &inside),
        ReducedValue::Type(InferredType::ArrayRef)
    );

    let outside = ReducerQuery {
        attachment: &att,
        point: Some(p(2, 0)),
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &outside),
        ReducedValue::Type(InferredType::HashRef)
    );
}

// ---- BranchArm reduction (ternary + explicit if/else) ----
//
// Branch arms are now plain `InferredType` witnesses with source
// `Builder("branch_arm")` (no longer a dedicated TypeObservation
// variant). For variable arms the walker pushes `Edge(Variable{...})`
// instead and the registry materializes through to the resolved
// type before BranchArmFold sees it. These tests pin the agreement
// rule on direct types.

// A ternary arm: one type witness on the ternary's `BranchArm(span)`.
fn wbranch(t: InferredType) -> Witness {
    Witness {
        attachment: WitnessAttachment::BranchArm(span(0, 0, 0, 0)),
        source: WitnessSource::Builder("branch_arm".into()),
        payload: WitnessPayload::InferredType(t),
        span: span(0, 0, 0, 0),
    }
}

#[test]
fn branch_arms_agree_folds_to_that_type() {
    let mut bag = WitnessBag::new();
    bag.push(wbranch(InferredType::Numeric));
    bag.push(wbranch(InferredType::Numeric));
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::BranchArm(span(0, 0, 0, 0));
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::Numeric)
    );
}

#[test]
fn branch_arms_disagree_folds_to_none() {
    let mut bag = WitnessBag::new();
    bag.push(wbranch(InferredType::Numeric));
    bag.push(wbranch(InferredType::String));
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::BranchArm(span(0, 0, 0, 0));
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(reg.query(&bag, &q), ReducedValue::None);
}

// Edge-shaped branch arm: walker emits `Edge(Variable{...})` for a
// variable return; registry materialization chases through the
// variable's witness, giving BranchArmFold a resolved type.
#[test]
fn branch_arm_edge_resolves_through_variable() {
    let mut bag = WitnessBag::new();
    // Two arms of `my $x = $c ? $a : $b;`. $a and $b both have type
    // String — agreement should fold to String even though the
    // walker couldn't bake the arm types.
    bag.push(Witness {
        attachment: WitnessAttachment::Variable {
            name: "$a".into(),
            scope: ScopeId(0),
        },
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::InferredType(InferredType::String),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: WitnessAttachment::Variable {
            name: "$b".into(),
            scope: ScopeId(0),
        },
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::InferredType(InferredType::String),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: WitnessAttachment::BranchArm(span(0, 0, 0, 0)),
        source: WitnessSource::Builder("branch_arm".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Variable {
            name: "$a".into(),
            scope: ScopeId(0),
        }),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: WitnessAttachment::BranchArm(span(0, 0, 0, 0)),
        source: WitnessSource::Builder("branch_arm".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Variable {
            name: "$b".into(),
            scope: ScopeId(0),
        }),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::BranchArm(span(0, 0, 0, 0));
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::String)
    );
}

// ---- ReturnExpr-based arity dispatch ----

fn wsym(sym_id: u32, payload: WitnessPayload) -> Witness {
    Witness {
        attachment: WitnessAttachment::Symbol(crate::file_analysis::SymbolId(sym_id)),
        source: WitnessSource::Builder("arity_detection".into()),
        payload,
        span: span(0, 0, 0, 0),
    }
}

#[test]
fn arity_zero_returns_string_default_returns_self() {
    // `sub name { my $self = shift; return $self->{name} unless @_; ...; $self }`
    // — arity 0 → String; default → ClassName(MojoRoute) via Receiver
    // substitution. After phase 5 retired `FluentArityDispatch`, the
    // bag mechanism is `ReturnExpr::UnionOnArgs` on `Symbol(_)` and
    // the chain typer / class lookup threads the receiver.
    let sub_id = 42;
    let mut bag = WitnessBag::new();
    bag.push(wsym(
        sub_id,
        WitnessPayload::ReturnExpr(ReturnExpr::UnionOnArgs {
            branches: vec![
                (ArgGuard::Empty, ReturnExpr::Concrete(InferredType::String)),
                (ArgGuard::Any, ReturnExpr::Receiver),
            ],
        }),
    ));

    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(crate::file_analysis::SymbolId(sub_id));
    let receiver = InferredType::ClassName("MojoRoute".into());

    // Caller passes no args: `$r->name` — arity 0.
    let q0 = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: Some(0),
        receiver: Some(receiver.clone()),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q0),
        ReducedValue::Type(InferredType::String)
    );

    // Caller passes one arg: `$r->name('x')` — arity 1 falls through
    // to Any → Receiver, returning the invocant class.
    let q1 = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: Some(1),
        receiver: Some(receiver.clone()),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q1),
        ReducedValue::Type(receiver.clone())
    );

    // Arity unknown — Empty doesn't match None (strict Some(0)
    // only), Any does. Returns Receiver substitution.
    let qn = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: Some(receiver.clone()),
        context: None,
    };
    assert_eq!(reg.query(&bag, &qn), ReducedValue::Type(receiver));
}

#[test]
fn numeric_then_string_prefers_numeric_noop_when_class_set() {
    let mut bag = WitnessBag::new();
    bag.push(wvar(
        "$n",
        0,
        WitnessPayload::Observation(TypeObservation::NumericUse),
    ));
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Variable {
        name: "$n".into(),
        scope: ScopeId(0),
    };
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&bag, &q),
        ReducedValue::Type(InferredType::Numeric)
    );
}

// ---- Plugin override priority short-circuit -----------------------
//
// `apply_type_overrides` pushes a Plugin-source `InferredType`
// witness on `Symbol(sym_id)`. The `PluginOverrideReducer` runs
// first in the default registry and must dominate any other
// Symbol-attached InferredType evidence. Pin the priority short-
// circuit at the reducer level so a future refactor that drops the
// priority check fails here, not silently downstream in
// `--dump-package` provenance.

#[test]
fn plugin_override_priority_dominates_builder_inferred_type() {
    let sym_id = SymbolId(0);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("inferred".into()),
        payload: WitnessPayload::InferredType(InferredType::HashRef),
        span: span(0, 0, 0, 0),
    });
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Plugin("test-plugin".into()),
        payload: WitnessPayload::InferredType(InferredType::ClassName("Baz".into())),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    match reg.query(&bag, &q) {
        ReducedValue::Type(InferredType::ClassName(name)) => {
            assert_eq!(
                name, "Baz",
                "Plugin priority must dominate Builder-source InferredType on the same Symbol attachment"
            );
        }
        other => panic!("expected ClassName(Baz) from PluginOverrideReducer; got {other:?}"),
    }
}

// ---- Plugin-override yielding -----------------------------------
//
// (Sub-return delegation reducers — DelegationReducer /
// SelfMethodTailReducer — were retired with the edge-fact
// migration. The chase now happens via WitnessPayload::Edge during
// registry materialization; see edge_chase_resolves_transitively /
// edge_chase_terminates_on_cycle / edge_with_unresolved_target_yields_none
// for the new contract.)

#[test]
fn plugin_override_reducer_declines_builder_priority_witnesses() {
    // Plugin-priority short-circuit must not fire on Builder-only
    // Symbol+InferredType witnesses (priority == 10). Post-D3,
    // `SubReturnReducer` claims those instead and produces the
    // stored-return answer; what we're asserting here is that the
    // claim is NOT routed through `PluginOverrideReducer`'s
    // priority short-circuit.
    let sym_id = SymbolId(7);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("inferred".into()),
        payload: WitnessPayload::InferredType(InferredType::String),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    // SubReturnReducer (registered last) returns the stored type.
    // PluginOverrideReducer's short-circuit didn't fire — if it had,
    // the answer would still be `Type(String)` but this test is
    // mostly a smoke check that the registry resolves through the
    // expected reducer chain.
    assert_eq!(reg.query(&bag, &q), ReducedValue::Type(InferredType::String));
}

// ---- ReturnExpr reducer ----

/// `Concrete(t)` payload reduces to `t` regardless of receiver/arity.
#[test]
fn return_expr_concrete_resolves_directly() {
    let sym_id = SymbolId(11);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::ReturnExpr(ReturnExpr::Concrete(InferredType::HashRef)),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: None,
        context: None,
    };
    assert_eq!(reg.query(&bag, &q), ReducedValue::Type(InferredType::HashRef));
}

/// `Receiver` substitutes with `q.receiver`; without one, returns None.
#[test]
fn return_expr_receiver_substitutes_from_query() {
    let sym_id = SymbolId(12);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::ReturnExpr(ReturnExpr::Receiver),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);
    let q_with = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: Some(InferredType::ClassName("Foo".into())),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q_with),
        ReducedValue::Type(InferredType::ClassName("Foo".into()))
    );
    let q_without = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: None,
        context: None,
    };
    assert_eq!(reg.query(&bag, &q_without), ReducedValue::None);
}

/// `UnionOnArgs` dispatches on `q.arity_hint`. `Empty` matches `Some(0)`,
/// `Any` is the catch-all. Unmatched returns None.
#[test]
fn return_expr_union_on_args_dispatches_by_arity() {
    let sym_id = SymbolId(13);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::ReturnExpr(ReturnExpr::UnionOnArgs {
            branches: vec![
                (ArgGuard::Empty, ReturnExpr::Concrete(InferredType::String)),
                (ArgGuard::Any, ReturnExpr::Receiver),
            ],
        }),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);

    let q_empty = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: Some(0),
        receiver: Some(InferredType::ClassName("Foo".into())),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q_empty),
        ReducedValue::Type(InferredType::String),
        "arity 0 must hit the Empty arm before Any"
    );

    let q_any = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: Some(1),
        receiver: Some(InferredType::ClassName("Foo".into())),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q_any),
        ReducedValue::Type(InferredType::ClassName("Foo".into())),
        "arity 1 falls through to Any => Receiver"
    );

    let q_no_hint = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: Some(InferredType::ClassName("Foo".into())),
        context: None,
    };
    assert_eq!(
        reg.query(&bag, &q_no_hint),
        ReducedValue::Type(InferredType::ClassName("Foo".into())),
        "no hint falls through Empty (strict Some(0) only) and \
         hits Any => Receiver — the catch-all"
    );
}

/// `Operator(RowOf(Receiver))` substitutes Receiver, wraps into the
/// value-side `ParametricType::RowOf` operator. Downstream consumers
/// of `ParametricType::class_name()` evaluate it correctly.
#[test]
fn return_expr_operator_rowof_wraps_receiver() {
    let sym_id = SymbolId(14);
    let mut bag = WitnessBag::new();
    bag.push(Witness {
        attachment: WitnessAttachment::Symbol(sym_id),
        source: WitnessSource::Builder("test".into()),
        payload: WitnessPayload::ReturnExpr(ReturnExpr::Operator(
            ParametricOp::RowOf(Box::new(ReturnExpr::Receiver)),
        )),
        span: span(0, 0, 0, 0),
    });
    let reg = ReducerRegistry::with_defaults();
    let att = WitnessAttachment::Symbol(sym_id);
    let receiver = InferredType::Parametric(ParametricType::ResultSet {
        base: "DBIx::Class::ResultSet".into(),
        row: "Schema::Result::Users".into(),
    });
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None,
        receiver: Some(receiver.clone()),
        context: None,
    };
    let result = reg.query(&bag, &q);
    let ReducedValue::Type(InferredType::ClassName(class)) = result else {
        panic!("expected ClassName(row); got {:?}", result);
    };
    assert_eq!(
        class, "Schema::Result::Users",
        "RowOf(Receiver) over a ResultSet projects eagerly to the row class"
    );
}
