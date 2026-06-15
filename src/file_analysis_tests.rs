use super::*;
use tree_sitter::Point;

/// Helper: build a minimal FileAnalysis with a single file scope and given type constraints.
/// Constraints are pushed via `push_type_constraint` so the witness bag carries them —
/// the bag is the sole store post-D4-followup.
fn fa_with_constraints(constraints: Vec<TypeConstraint>) -> FileAnalysis {
    let mut fa = FileAnalysis::new(FileAnalysisParts {
        scopes: vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        ..Default::default()
    });
    for tc in constraints {
        fa.push_type_constraint(tc);
    }
    fa
}

fn constraint(var: &str, row: usize, inferred_type: InferredType) -> TypeConstraint {
    TypeConstraint {
        variable: var.to_string(),
        scope: ScopeId(0),
        constraint_span: Span {
            start: Point::new(row, 0),
            end: Point::new(row, 20),
        },
        inferred_type,
    }
}

// ---- Type constraint resolution tests ----

#[test]
fn test_resolve_hashref_type() {
    let fa = fa_with_constraints(vec![constraint("$href", 0, InferredType::HashRef)]);
    assert_eq!(
        fa.inferred_type_via_bag("$href", Point::new(1, 0)),
        Some(InferredType::HashRef)
    );
}

#[test]
fn test_resolve_arrayref_type() {
    let fa = fa_with_constraints(vec![constraint("$aref", 0, InferredType::ArrayRef)]);
    assert_eq!(
        fa.inferred_type_via_bag("$aref", Point::new(1, 0)),
        Some(InferredType::ArrayRef)
    );
}

#[test]
fn test_resolve_coderef_type() {
    let fa = fa_with_constraints(vec![constraint(
        "$cref",
        0,
        InferredType::CodeRef { return_edge: None },
    )]);
    assert_eq!(
        fa.inferred_type_via_bag("$cref", Point::new(1, 0)),
        Some(InferredType::CodeRef { return_edge: None })
    );
}

#[test]
fn test_resolve_regexp_type() {
    let fa = fa_with_constraints(vec![constraint("$re", 0, InferredType::Regexp)]);
    assert_eq!(
        fa.inferred_type_via_bag("$re", Point::new(1, 0)),
        Some(InferredType::Regexp)
    );
}

#[test]
fn test_resolve_numeric_type() {
    let fa = fa_with_constraints(vec![constraint("$n", 0, InferredType::Numeric)]);
    assert_eq!(
        fa.inferred_type_via_bag("$n", Point::new(1, 0)),
        Some(InferredType::Numeric)
    );
}

#[test]
fn test_resolve_string_type() {
    let fa = fa_with_constraints(vec![constraint("$s", 0, InferredType::String)]);
    assert_eq!(
        fa.inferred_type_via_bag("$s", Point::new(1, 0)),
        Some(InferredType::String)
    );
}

#[test]
fn test_resolve_reassignment_changes_type() {
    let fa = fa_with_constraints(vec![
        constraint("$x", 0, InferredType::HashRef),
        constraint("$x", 3, InferredType::ArrayRef),
    ]);
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(1, 0)),
        Some(InferredType::HashRef)
    );
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(4, 0)),
        Some(InferredType::ArrayRef)
    );
}

#[test]
fn test_resolve_sub_return_type() {
    use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
    // Hand-craft a FileAnalysis with a sub that has a return type.
    // Post-D3, return types live on `Symbol(_)` only — seed
    // `Symbol(0)` directly so the bag-routed query picks it up.
    let mut fa = FileAnalysis::new(FileAnalysisParts {
        scopes: vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        symbols: vec![Symbol {
            id: SymbolId(0),
            name: "get_config".to_string(),
            kind: SymKind::Sub,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(2, 1),
            },
            selection_span: Span {
                start: Point::new(0, 4),
                end: Point::new(0, 14),
            },
            scope: ScopeId(0),
            package: None,
            detail: SymbolDetail::Sub {
                params: vec![],
                is_method: false,
                doc: None,
                display: None,
                hide_in_outline: false,
                opaque_return: false,
                is_constant: false,
                lexical: false,
            },
            namespace: Namespace::Language,
            outline_label: None,
        }],
        ..Default::default()
    });
    let zero_span = Span {
        start: Point::new(0, 0),
        end: Point::new(0, 0),
    };
    fa.witnesses.push(Witness {
        attachment: WitnessAttachment::Symbol(SymbolId(0)),
        source: WitnessSource::Builder("local_return".into()),
        payload: WitnessPayload::InferredType(InferredType::HashRef),
        span: zero_span,
    });
    assert_eq!(
        fa.sub_return_type_at_arity("get_config", None),
        Some(InferredType::HashRef)
    );
    assert_eq!(fa.sub_return_type_at_arity("nonexistent", None), None);
}

// ---- Return type resolution tests (decoupled, pure function) ----

#[test]
fn test_resolve_return_type_all_agree() {
    assert_eq!(
        resolve_return_type(&[InferredType::HashRef, InferredType::HashRef]),
        Some(InferredType::HashRef),
    );
}

#[test]
fn test_resolve_return_type_disagreement() {
    assert_eq!(
        resolve_return_type(&[InferredType::HashRef, InferredType::ArrayRef]),
        None,
    );
}

#[test]
fn test_resolve_return_type_empty() {
    assert_eq!(resolve_return_type(&[]), None);
}

#[test]
fn test_resolve_return_type_object_subsumes_hashref() {
    // Object + HashRef → Object wins (overloaded objects)
    assert_eq!(
        resolve_return_type(&[InferredType::ClassName("Foo".into()), InferredType::HashRef,]),
        Some(InferredType::ClassName("Foo".into())),
    );
    // Order shouldn't matter
    assert_eq!(
        resolve_return_type(&[InferredType::HashRef, InferredType::ClassName("Foo".into()),]),
        Some(InferredType::ClassName("Foo".into())),
    );
}

#[test]
fn test_resolve_return_type_object_does_not_subsume_arrayref() {
    // Object + ArrayRef → disagreement
    assert_eq!(
        resolve_return_type(&[
            InferredType::ClassName("Foo".into()),
            InferredType::ArrayRef,
        ]),
        None,
    );
}

#[test]
fn test_resolve_return_type_single() {
    assert_eq!(
        resolve_return_type(&[InferredType::CodeRef { return_edge: None }]),
        Some(InferredType::CodeRef { return_edge: None }),
    );
}

#[test]
fn test_class_name_helper() {
    assert_eq!(
        InferredType::ClassName("Foo".into()).class_name(),
        Some("Foo")
    );
    assert_eq!(
        InferredType::FirstParam {
            package: "Bar".into()
        }
        .class_name(),
        Some("Bar")
    );
    assert_eq!(InferredType::HashRef.class_name(), None);
    assert_eq!(InferredType::ArrayRef.class_name(), None);
    assert_eq!(InferredType::CodeRef { return_edge: None }.class_name(), None);
    assert_eq!(InferredType::Regexp.class_name(), None);
    assert_eq!(InferredType::Numeric.class_name(), None);
    assert_eq!(InferredType::String.class_name(), None);
}

// `test_sub_return_type_imported_fallback` and
// `test_enrich_imported_types_pushes_constraints` were removed in
// D3: the imported-returns path no longer rides a name-keyed bag
// attachment. Cross-file imports resolve through
// `query_sub_return_type` walking `module_index.find_exporters` and
// recursing into the cached module's `Symbol(_)` witness; the
// integration tests under `symbols_tests.rs` (e.g.
// `test_demo_chain_empirical_truth_table`) and
// `builder_tests.rs::enrichment_twice_does_not_crash_on_stale_indices`
// exercise the new path end-to-end against a real `ModuleIndex`.

// ---- Phase 5: refs_by_target index + eager HashKeyAccess resolution ----

fn build_fa_from_source(source: &str) -> FileAnalysis {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

/// Fully-qualified same-file call (`Foo::baz()`) — goto-def lands on the
/// local `sub baz` in package Foo, and references on the def includes the
/// qualified call site. The ref's `target_name` is the whole `Foo::baz`
/// path; resolution matches on the bare tail + `resolved_package`.
#[test]
fn test_qualified_call_local_goto_def_and_references() {
    let src = "package Foo;\nsub baz { 42 }\npackage main;\nFoo::baz();\n";
    let fa = build_fa_from_source(src);

    // Cursor on `baz` in `Foo::baz()` (line 3 = 0-indexed; `baz` starts
    // at col 5 after `Foo::`).
    let def = fa
        .find_definition(Point::new(3, 6), None)
        .expect("goto-def on Foo::baz() should resolve to the local sub");
    // The def is `sub baz` on line 1.
    assert_eq!(def.start.row, 1, "should land on `sub baz`, got {:?}", def);

    // References anchored on the call site must include the call span.
    let refs = fa.find_references(Point::new(3, 6), None);
    assert!(
        refs.iter().any(|s| s.start.row == 3),
        "references should include the Foo::baz() call site, got {:?}",
        refs,
    );
    // And the declaration.
    assert!(
        refs.iter().any(|s| s.start.row == 1),
        "references should include the sub baz declaration, got {:?}",
        refs,
    );
}

/// Fully-qualified METHOD call (`$obj->Widget::build()`): Perl dispatches
/// `build` from the NAMED class `Widget`, ignoring the invocant's class. So
/// goto-def / hover / rename resolve against `Widget` even when `$obj` is
/// untyped, the rename narrows to the `build` tail, and references find the
/// call site.
#[test]
fn test_fq_method_call_nav_dispatches_from_named_class() {
    use crate::file_analysis::{RefKind, RenameKind};
    // `$obj` is deliberately untyped — only the qualifier names the class.
    let src = "package Widget;\nsub build { my ($class, $n) = @_; return 1 }\npackage main;\nmy $obj = get_thing();\nmy $r = $obj->Widget::build();\n";
    let fa = build_fa_from_source(src);

    // The FQ method ref keeps the full path in target_name but resolves its
    // dispatch class to the qualifier.
    let mref = fa
        .refs
        .iter()
        .find(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "Widget::build")
        .expect("FQ method ref present");
    assert_eq!(
        fa.method_call_invocant_class(mref, None).as_deref(),
        Some("Widget"),
        "FQ call dispatches from the named class, not the (untyped) invocant"
    );
    // method_name_span is narrowed to the `build` tail (col 22), not the
    // whole `Widget::build` token (col 14) — so rename rewrites only `build`.
    if let RefKind::MethodCall { method_name_span, .. } = &mref.kind {
        assert_eq!(method_name_span.start.column, 22, "rename span is the bare tail");
    }

    // goto-def on the `build` tail (line 4, col 22) lands on `sub build`.
    let def = fa
        .find_definition(Point::new(4, 22), None)
        .expect("FQ method goto-def resolves to the local sub");
    assert_eq!(def.start.row, 1, "should land on `sub build`, got {:?}", def);

    // rename resolves to the bare method on the qualifier class.
    match fa.rename_kind_at(Point::new(4, 22), None) {
        Some(RenameKind::Method { name, class }) => {
            assert_eq!(name, "build");
            assert_eq!(class, "Widget");
        }
        other => panic!("expected Method rename, got {:?}", other),
    }
}

/// `$self->SUPER::m` dispatches to the enclosing package's PARENT method —
/// goto-def jumps to it and renaming the parent method rewrites the SUPER call
/// (a dangling SUPER call would be a broken rename).
/// SUPER walks the FULL parent MRO, not just the first parent: with
/// `@ISA = (A, B)`, `$self->SUPER::m` finds `m` on B when only B defines it,
/// and it skips the enclosing class itself (Perl's SUPER searches parents).
#[test]
fn test_super_resolves_across_all_parents() {
    let src = "package A;\nsub a_only { 1 }\npackage B;\nsub b_only { 2 }\npackage Child;\nuse parent -norequire, 'A', 'B';\nsub go { my $self = shift; return $self->SUPER::b_only }\n";
    let fa = build_fa_from_source(src);
    // Method only on the SECOND parent — first-parent-only would miss it.
    assert_eq!(
        fa.resolve_super_method("Child", "b_only", None).map(|r| r.class().to_string()).as_deref(),
        Some("B"),
        "SUPER must search all parents, not just the first"
    );
    assert_eq!(
        fa.resolve_super_method("Child", "a_only", None).map(|r| r.class().to_string()).as_deref(),
        Some("A"),
    );
    // SUPER skips the enclosing class: Child defines `go`, but SUPER::go from
    // Child must not resolve to Child::go.
    assert!(
        fa.resolve_super_method("Child", "go", None).is_none(),
        "SUPER skips the current package"
    );
}

#[test]
fn test_super_method_nav_resolves_to_parent() {
    use crate::file_analysis::{RefKind, RenameKind};
    let src = "package Base;\nsub greet { return 1 }\npackage Child;\nuse parent -norequire, 'Base';\nsub greet { my $self = shift; return $self->SUPER::greet }\n";
    let fa = build_fa_from_source(src);

    let sref = fa
        .refs
        .iter()
        .find(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "SUPER::greet")
        .expect("SUPER method ref present");
    assert_eq!(
        fa.method_call_invocant_class(sref, None).as_deref(),
        Some("Base"),
        "SUPER dispatches to the enclosing package's parent"
    );
    let span = if let RefKind::MethodCall { method_name_span, .. } = &sref.kind {
        *method_name_span
    } else {
        unreachable!()
    };
    // goto-def on the SUPER call lands on `Base::greet` (line 1).
    let def = fa
        .find_definition(span.start, None)
        .expect("SUPER goto-def resolves");
    assert_eq!(def.start.row, 1, "SUPER::greet resolves to Base::greet, got {:?}", def);
    // Renaming targets the parent method on `Base` (so the SUPER call tracks).
    match fa.rename_kind_at(span.start, None) {
        Some(RenameKind::Method { name, class }) => {
            assert_eq!(name, "greet");
            assert_eq!(class, "Base");
        }
        other => panic!("expected Method rename on Base, got {:?}", other),
    }
}

/// A qualified call to package Foo's `baz` must not resolve to a same-named
/// `baz` in another package — `resolved_package` isolates the qualifier.
#[test]
fn test_qualified_call_does_not_cross_package() {
    let src = "package Foo;\nsub baz { 1 }\npackage Bar;\nsub baz { 2 }\npackage main;\nBar::baz();\n";
    let fa = build_fa_from_source(src);

    // Cursor on `baz` in `Bar::baz()` (line 5, col 5).
    let def = fa
        .find_definition(Point::new(5, 6), None)
        .expect("Bar::baz() should resolve");
    // Bar's `sub baz` is on line 3, not Foo's on line 1.
    assert_eq!(def.start.row, 3, "should land on Bar's sub baz, got {:?}", def);
}

/// Dynamic method dispatch: `my $m = 'get_config'; __PACKAGE__->$m()`.
/// Constant folding recovers the method name "get_config" from the
/// string assignment, so the MethodCallBinding fires and $c picks up
/// the return type. The access $c->{host} then links to the HashKeyDef.
/// This path already worked for method-call types in the old builder;
/// the phase 5 tests pin it so we don't regress when we refactor the
/// fixup.
#[test]
fn test_phase5_dynamic_method_call_via_constant_folding() {
    let fa = build_fa_from_source(
        r#"
            package Demo::dyn;
            sub get_config { return { host => 'localhost', port => 5432 } }
            my $method = 'get_config';
            my $c = __PACKAGE__->$method();
            my $h = $c->{host};
        "#,
    );

    let def = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == "get_config")
        })
        .expect("HashKeyDef host owned by get_config");

    let indexed = fa.refs_to_symbol(def.id);
    assert!(
        !indexed.is_empty(),
        "dynamic method call via $method='get_config' should link $c->{{host}} back to the def",
    );
}

/// Cross-file consumer-side resolution (the phase-5 follow-up).
/// Consumer has `my $c = Imported::get_config(); $c->{host}`. At build
/// time we don't know get_config returns HashRef. Enrichment injects
/// synthetic HashKeyDefs and now ALSO retries the HashKeyAccess owner
/// fixup + rebuilds refs_by_target. Result: the access links to the
/// synthetic def, so rename / references / refs_by_target all work.
#[test]
fn test_phase5_consumer_side_cross_file_resolves_via_enrichment() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let mut fa = build_fa_from_source(
        r#"
            use TestExporter qw(get_config);
            my $c = get_config();
            my $h = $c->{host};
        "#,
    );

    // Sanity: before enrichment the access has no link (there's no
    // local HashKeyDef for "host" and no local get_config sub).
    let hka_before = fa
        .refs
        .iter()
        .find(|r| matches!(r.kind, RefKind::HashKeyAccess { .. }) && r.target_name == "host")
        .expect("HashKeyAccess host");
    assert!(
        hka_before.resolves_to.is_none(),
        "pre-enrichment, access should not be resolved"
    );

    // Stub `TestExporter` with `get_config` returning a hashref whose
    // keys (host, port, name) the builder synthesizes as HashKeyDefs.
    // Enrichment reads these via `cached.sub_info("get_config").hash_keys()`
    // and injects synthetic local HashKeyDefs against the consumer.
    let test_exporter_pm = r#"
package TestExporter;
use Exporter 'import';
our @EXPORT_OK = qw(get_config);
sub get_config { return { host => 'h', port => 1, name => 'n' }; }
1;
"#;
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/TestExporter.pm"),
        Arc::new(build_fa_from_source(test_exporter_pm)),
    );
    fa.enrich_imported_types_with_keys(Some(&idx));

    // The synthetic HashKeyDef `host` owned by Sub{None, "get_config"}
    // must now have the access indexed under it.
    let def = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { package: None, name }, ..
                } if name == "get_config")
        })
        .expect("synthetic HashKeyDef host");

    let indexed = fa.refs_to_symbol(def.id);
    assert!(
            !indexed.is_empty(),
            "consumer-side $c->{{host}} access should link to synthetic HashKeyDef after enrichment, got empty refs_by_target entry",
        );
}

/// End-to-end demo shape: mirrors `test_files/phase5_demo.pl` so a
/// regression in the demo surfaces here first. Three access sites all
/// flow through the qualified `Demo::phase5::get_config()` binding.
#[test]
fn test_phase5_demo_file_shape_resolves_all_access_sites() {
    let fa = build_fa_from_source(
        r#"
            package Demo::phase5;
            sub get_config {
                return { host => 'localhost', port => 5432, name => 'mydb' };
            }
            my $c = Demo::phase5::get_config();
            my $h1 = $c->{host};
            my $h2 = $c->{host};
            my $p  = $c->{port};
        "#,
    );

    let def_host = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { package: Some(p), name }, ..
                } if p == "Demo::phase5" && name == "get_config")
        })
        .expect("Demo::phase5::get_config's HashKeyDef `host`");

    let indexed = fa.refs_to_symbol(def_host.id);
    // Two $c->{host} accesses — both should resolve to the same def.
    assert_eq!(
        indexed.len(),
        2,
        "expected 2 $c->{{host}} accesses to link via refs_by_target, got {} (indexes: {:?})",
        indexed.len(),
        indexed,
    );

    // The port access must NOT be under host's target.
    let def_port = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "port"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == "get_config")
        })
        .expect("HashKeyDef port");
    let port_refs = fa.refs_to_symbol(def_port.id);
    assert_eq!(
        port_refs.len(),
        1,
        "expected exactly one $c->{{port}} access"
    );
}

/// Qualified call: `Demo::phase5::get_config()` should link $c->{host}
/// to the HashKeyDef emitted inside `sub get_config` in the same package.
/// Previously broken: the binding fixup looked up `return_types` by the
/// qualified func_name ("Demo::phase5::get_config"), missed, and left
/// $c->{host} with owner=None. Now the fixup strips the package prefix.
#[test]
fn test_phase5_qualified_sub_call_links_hash_key_access() {
    let fa = build_fa_from_source(
        r#"
            package Demo::phase5;
            sub get_config { return { host => 'localhost', port => 5432 } }
            my $c = Demo::phase5::get_config();
            my $h = $c->{host};
        "#,
    );

    let def = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { package: Some(p), name }, ..
                } if p == "Demo::phase5" && name == "get_config")
        })
        .expect("HashKeyDef host owned by Sub{Some(Demo::phase5), get_config}");

    let indexed = fa.refs_to_symbol(def.id);
    assert!(
            !indexed.is_empty(),
            "qualified call `Demo::phase5::get_config()` should link $c->{{host}} to the def via refs_by_target",
        );
}

/// Chain through an intermediate sub: `sub chain { return get_config() }`
/// with `$c = chain(); $c->{host}`. The hash key ownership must flow
/// through chain to get_config, where the actual HashKeyDefs live.
/// Previously broken: $c's owner was Sub{_, "chain"}, which has no
/// HashKeyDefs, so refs_by_target matched nothing.
#[test]
fn test_phase5_hash_key_owner_flows_through_intermediate_sub() {
    let fa = build_fa_from_source(
        r#"
            package Demo::phase5;
            sub get_config { return { host => 'localhost', port => 5432 } }
            sub chain_helper { return get_config() }
            my $c = chain_helper();
            my $h = $c->{host};
        "#,
    );

    // The HashKeyDef for `host` lives under get_config, not chain_helper.
    let def = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == "get_config")
        })
        .expect("host HashKeyDef owned by get_config");

    // The access $c->{host} must link to this def, even though $c was
    // bound to chain_helper() (the intermediate sub with no keys).
    let indexed = fa.refs_to_symbol(def.id);
    assert!(
        !indexed.is_empty(),
        "chain_helper → get_config delegation should flow $c->{{host}} to get_config's HashKeyDef",
    );
}

/// Previously broken: references on a hash key owned by a sub's return
/// value didn't flow through refs_by_target because resolves_to was never
/// set on HashKeyAccess refs. Phase 5 links HashKeyAccess.resolves_to to
/// the matching HashKeyDef at build time via (target_name, owner) lookup.
#[test]
fn test_phase5_hash_key_access_links_to_def_symbol() {
    let fa = build_fa_from_source(
        r#"
            sub get_config { return { host => 'localhost', port => 5432 } }
            my $cfg = get_config();
            my $h = $cfg->{host};
        "#,
    );

    // Find the HashKeyDef "host" owned by Sub { name: "get_config", .. }.
    let def = fa.symbols.iter().find(|s| {
        s.name == "host"
            && s.kind == SymKind::HashKeyDef
            && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == "get_config")
    });
    assert!(
        def.is_some(),
        "HashKeyDef 'host' owned by Sub get_config should exist"
    );
    let def_id = def.unwrap().id;

    // The `$cfg->{host}` access should be indexed under this symbol.
    let indexed = fa.refs_to_symbol(def_id);
    assert!(
        !indexed.is_empty(),
        "refs_by_target should index $cfg->{{host}} access under the HashKeyDef, got empty"
    );
    let access = &fa.refs[indexed[0]];
    assert!(matches!(access.kind, RefKind::HashKeyAccess { .. }));
    assert_eq!(access.target_name, "host");
}

/// Previously broken: find_references on the HashKeyDef required a tree
/// argument to match accesses (the HashKeyAccess.owner was resolved lazily
/// in the query via `resolve_hash_owner_from_tree`). Phase 5 links the ref
/// to the def at build time, so the query returns the access even when
/// the caller passes no tree.
#[test]
fn test_phase5_find_references_on_hash_key_def_without_tree() {
    let fa = build_fa_from_source(
        r#"
            sub get_config { return { host => 1, port => 2 } }
            my $cfg = get_config();
            my $h = $cfg->{host};
            my $p = $cfg->{port};
        "#,
    );
    let def_host = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "host"
                && s.kind == SymKind::HashKeyDef
                && matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == "get_config")
        })
        .expect("host HashKeyDef");

    // Cursor on the def — convention is that find_references returns the
    // *usages*, not the def itself. Previously this returned 0 without a
    // tree; phase 5 returns the access sites via refs_by_target.
    let point = def_host.selection_span.start;
    let refs = fa.find_references(point, None);
    assert!(
        !refs.is_empty(),
        "expected at least one access for 'host' via refs_by_target (no tree), got empty",
    );
    // Sanity: the access should point at `$cfg->{host}`.
    assert!(
        refs.iter().any(|s| s.start.row == 3),
        "expected to find the access on row 3 ($cfg->{{host}}), got {:?}",
        refs,
    );
}

/// Previously broken: refs_by_target was absent. Even variable refs
/// relied on a linear scan of `refs` per query. Phase 5 exposes an
/// O(1) lookup for any resolved ref.
/// Pin the exhaustive document outline for `test_files/plugin_mojo_demo.pl`.
///
/// Covers every plugin path in a single real file: mojo-helpers
/// (simple + dotted chains), mojo-routes (`->to`), mojo-lite (verb
/// DSL), minion tasks, mojo-events (`->on`). This is what a user
/// actually sees in `<leader>o` — each plugin-emitted entry carries
/// a leading kind word ("helper"/"route"/"task"/"event") and
/// non-invocant params so stacked registrations (GET vs POST on
/// the same path, three events on one emitter) are distinguishable
/// in clients that render only `name` (nvim's builtin
/// document_symbol handler does).
///
/// Internal plumbing — mojo-helpers' synthetic proxy classes and
/// their `[proxy]` namespaces, the intermediate segment Methods
/// that chain dotted helpers — must NOT show up. It's invisible
/// to the user but pins the no-duplicate-`helper users` invariant
/// that was the motivating bug.
#[test]
fn plugin_mojo_demo_outline_pinned() {
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("test_files/plugin_mojo_demo.pl");
    let src = std::fs::read_to_string(&path).expect("plugin_mojo_demo.pl is checked into the repo");
    let fa = build_fa_from_source(&src);
    let rendered = render_outline(&fa.document_symbols());
    let expected = "\
[NAMESPACE] MyApp @L34
[VARIABLE] $app @L44
[FUNCTION] <helper> current_user ($fallback) @L45
[FUNCTION] <helper> users.create ($name, $email) @L52
[FUNCTION] <helper> users.delete ($id) @L56
[FUNCTION] <helper> admin.users.purge ($force) @L62
[VARIABLE] $r @L70
[FUNCTION] <action> Users#list @L71
[FUNCTION] <action> Users#create @L72
[FUNCTION] <action> Admin::Dashboard#index @L73
[FUNCTION] <route> GET /users/profile @L79
[FUNCTION] <route> POST /users/profile ($form_data) @L84
[FUNCTION] <route> ANY /fallback @L90
[VARIABLE] $minion @L105
[FUNCTION] <task> send_email ($to, $subject, $body) @L107
[FUNCTION] <task> resize_image ($path, $width, $height) @L113
[NAMESPACE] MyApp::Progress @L131
[FUNCTION] new @L134
  [EVENT] <event> ready ($ctx) @L137
  [EVENT] <event> step ($n, $total) @L138
  [EVENT] <event> done ($result) @L139
[FUNCTION] tick @L143
";
    // `use` lines absent: outlines show structure, not imports. File-scope
    // `my` vars ($app, $r, $minion) survive; sub-body lexicals ($class/$self
    // in new, $self/$n in tick) are dropped as working state.
    assert_eq!(
        rendered, expected,
        "\n---- ACTUAL ----\n{}\n---- EXPECTED ----\n{}",
        rendered, expected,
    );
}

/// Serialize an outline tree to the `[LSP_KIND] name @L<line>`
/// form the pin test compares against. Indent = two spaces per
/// depth. LSP kind uses the same path symbols.rs takes for the
/// real documentSymbol response (`outline_lsp_kind`), so the
/// snapshot reflects what clients actually receive.
fn render_outline(items: &[OutlineSymbol]) -> String {
    fn walk(o: &OutlineSymbol, depth: usize, buf: &mut String) {
        let kind = crate::symbols::outline_lsp_kind(o);
        // lsp_types SymbolKind's Debug is verbose ("SymbolKind(12)");
        // map the handful of kinds we actually emit by hand.
        let name = lsp_kind_name(kind);
        let pad = "  ".repeat(depth);
        buf.push_str(&format!(
            "{}[{}] {} @L{}\n",
            pad,
            name,
            o.name,
            o.span.start.row + 1,
        ));
        for c in &o.children {
            walk(c, depth + 1, buf);
        }
    }
    let mut buf = String::new();
    for o in items {
        walk(o, 0, &mut buf);
    }
    buf
}

fn lsp_kind_name(k: tower_lsp::lsp_types::SymbolKind) -> &'static str {
    use tower_lsp::lsp_types::SymbolKind as K;
    match k {
        K::FUNCTION => "FUNCTION",
        K::METHOD => "METHOD",
        K::CLASS => "CLASS",
        K::NAMESPACE => "NAMESPACE",
        K::MODULE => "MODULE",
        K::VARIABLE => "VARIABLE",
        K::EVENT => "EVENT",
        K::FIELD => "FIELD",
        K::PROPERTY => "PROPERTY",
        K::CONSTANT => "CONSTANT",
        K::KEY => "KEY",
        _ => "OTHER",
    }
}

#[test]
fn test_phase5_refs_by_target_index_populated_for_variables() {
    let fa = build_fa_from_source(
        r#"
            my $x = 1;
            $x = 2;
            my $y = $x + 1;
        "#,
    );
    // Find the variable symbol $x.
    let x_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "$x" && s.kind == SymKind::Variable)
        .expect("$x variable symbol");
    let indexed = fa.refs_to_symbol(x_sym.id);
    // Two usages after decl: `$x = 2` (write) and `$x + 1` (read).
    assert!(
        indexed.len() >= 2,
        "expected >= 2 refs to $x via refs_by_target, got {}: {:?}",
        indexed.len(),
        indexed,
    );
}

// ---- Part 6/7 witness-bag integration ----

/// The spike's driving case: a Mojo::Base class whose method
/// reads `$self->{k}` (blessed-hashref). The legacy
/// `inferred_type` may flip to HashRef; the witness-based
/// `inferred_type_via_bag` must keep the class.
#[test]
fn test_witnesses_mojo_self_stays_class_on_hashref_access() {
    let fa = build_fa_from_source(
        r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

sub name {
    my $self = shift;
    return $self->{name} unless @_;
    $self->{name} = shift;
    $self;
}
        "#,
    );

    // Framework mode was detected.
    assert_eq!(
        fa.package_framework.get("Mojolicious::Routes::Route"),
        Some(&crate::witnesses::FrameworkFact::MojoBase),
        "Mojo::Base package should record MojoBase framework"
    );

    // Witness bag has entries for $self.
    let self_wits: Vec<_> = fa
        .witnesses
        .all()
        .iter()
        .filter(|w| {
            matches!(&w.attachment,
                crate::witnesses::WitnessAttachment::Variable { name, .. } if name == "$self")
        })
        .collect();
    assert!(
        !self_wits.is_empty(),
        "expected witnesses for $self, got none (bag size: {})",
        fa.witnesses.len()
    );

    // Find the body of `sub name` and pick a point inside it.
    // The `$self->{name} unless @_;` line is row 5 (0-indexed).
    let point_in_body = Point { row: 5, column: 15 };
    let t = fa.inferred_type_via_bag("$self", point_in_body);
    assert!(
        matches!(t, Some(InferredType::ClassName(ref n)) if n == "Mojolicious::Routes::Route"),
        "via-bag type at body point should be ClassName(Route), got {:?}",
        t
    );
}

/// Part 7 fluent-chain: `$r->get('/x')->to('Y#z')` — the `->to`
/// call has an invocant expression that's itself a method call.
/// The bag lets us ask "what's the return type of ref[i]?" without
/// needing a named intermediate variable. Verifies the seed is
/// present and the bag's edge-chase materialization resolves the
/// chain hop to its target sub's return type.
#[test]
fn test_witnesses_fluent_chain_seeds_expression_witnesses() {
    let fa = build_fa_from_source(
        r#"
package MyApp::Route;
sub get  { my $self = shift; return $self; }
sub to   { my $self = shift; return $self; }

package main;
my $r = MyApp::Route->new;
$r->get('/x')->to('Y#z');
        "#,
    );

    // Expected: Expression-attached witnesses for each method
    // call. At minimum: one for `->new`, one for `->get`, one for
    // `->to`.
    let expr_wits: Vec<_> = fa
        .witnesses
        .all()
        .iter()
        .filter(|w| {
            matches!(
                w.attachment,
                crate::witnesses::WitnessAttachment::Expression(_)
            )
        })
        .collect();
    assert!(
        expr_wits.len() >= 3,
        "expected >= 3 Expression witnesses (new, get, to), got {}",
        expr_wits.len()
    );

    // Find the `->get` ref by name.
    let get_ref_idx = fa
        .refs
        .iter()
        .position(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "get")
        .expect("->get ref should exist");

    // Querying the bag for the get-call's return should yield
    // MyApp::Route (since its return is `return $self`, which is
    // FirstParam → ClassName projection).
    let t = fa.method_call_return_type_via_bag(get_ref_idx, None);
    assert!(
        matches!(t, Some(InferredType::ClassName(ref n)) if n == "MyApp::Route"),
        "->get's return type via bag should be ClassName(MyApp::Route), got {:?}",
        t
    );
}

/// Part 1 — mutated keys on the class. Write-access HashKeyAccess
/// refs push mutation facts; the helper returns them for dynamic-
/// key completion.
#[test]
fn test_witnesses_mutated_keys_on_mojo_class() {
    let fa = build_fa_from_source(
        r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

sub boot {
    my $self = shift;
    $self->{name}    = 'root';
    $self->{custom}  = 42;
    $self->{counter} = 0;
    return $self;
}
        "#,
    );

    let mut keys = fa.mutated_keys_on_class("Mojolicious::Routes::Route");
    keys.sort();
    // Depending on how owners resolve (Class vs Sub), we expect at
    // least two of the three writes to surface. Exact set is a
    // function of HashKeyOwner resolution, which isn't in scope
    // for this spike — just assert the helper plumbs through and
    // something lands.
    assert!(
        !keys.is_empty(),
        "expected some mutated keys for Mojolicious::Routes::Route, got {:?} \
             (hash-key witnesses in bag: {})",
        keys,
        fa.witnesses
            .all()
            .iter()
            .filter(|w| matches!(
                &w.attachment,
                crate::witnesses::WitnessAttachment::HashKey { .. }
            ))
            .count(),
    );
}

/// Fluent chain through `resolve_expression_type` public path:
/// `$r->get('/x')->to('Y#z')` — `to`'s invocant is a
/// method_call_expression. We walk the tree, recursively resolve
/// `$r->get('/x')`'s type; for that to work, `get`'s return type
/// must resolve. Inside `get`, `return $self` (where $self =
/// shift inside `sub get` of a class) is FirstParam → ClassName.
///
/// The witness wiring doesn't change this path's logic — it just
/// makes sure the `$self` inside `get` doesn't get flipped to
/// HashRef by intervening `$self->{k}` accesses (the Mojo bug).
#[test]
fn test_wiring_fluent_chain_resolves_through_expression_type() {
    let source = r#"
package MyApp::Route;
use Mojo::Base -base;

sub get {
    my $self = shift;
    $self->{_pattern} = shift;
    return $self;
}

sub to {
    my $self = shift;
    $self->{_target} = shift;
    return $self;
}

package main;
my $r = MyApp::Route->new;
$r->get('/x')->to('Y#z');
"#;

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    let fa = crate::builder::build(&tree, source.as_bytes());

    // Find the `->to` method call node by walking the tree.
    fn find_to_node<'a>(node: tree_sitter::Node<'a>) -> Option<tree_sitter::Node<'a>> {
        if node.kind() == "method_call_expression" {
            if let Some(m) = node.child_by_field_name("method") {
                if m.utf8_text(&[]).ok() == Some("to") {
                    return Some(node);
                }
            }
        }
        for i in 0..node.named_child_count() {
            if let Some(c) = node.named_child(i) {
                if let Some(r) = find_to_node(c) {
                    return Some(r);
                }
            }
        }
        None
    }

    // The `->to`'s invocant is `$r->get('/x')`, a
    // method_call_expression. Grab its type — should be
    // ClassName(MyApp::Route) because `get` returns $self.
    //
    // We walk the tree looking for the method_call_expression
    // whose *invocant* is itself a method_call_expression — that's
    // our fluent chain site.
    let source_bytes = source.as_bytes();
    let to_node = {
        let mut stack: Vec<tree_sitter::Node> = vec![tree.root_node()];
        let mut found: Option<tree_sitter::Node> = None;
        while let Some(n) = stack.pop() {
            if n.kind() == "method_call_expression" {
                if let Some(m) = n.child_by_field_name("method") {
                    if m.utf8_text(source_bytes).ok() == Some("to") {
                        found = Some(n);
                        break;
                    }
                }
            }
            for i in 0..n.named_child_count() {
                if let Some(c) = n.named_child(i) {
                    stack.push(c);
                }
            }
        }
        found.expect("found ->to node")
    };

    let invocant = to_node.child_by_field_name("invocant").expect("invocant");
    let ty = crate::cursor_context::resolve_expression_type(&fa, invocant, source_bytes, None);
    let class = ty.as_ref().and_then(|t| t.class_name());
    assert_eq!(
        class,
        Some("MyApp::Route"),
        "fluent chain: invocant type of `->to` should resolve to \
             MyApp::Route (ClassName or FirstParam), got {:?}",
        ty
    );
    let _ = find_to_node; // silence unused warning in case we refactor
}

/// End-to-end wiring check: the public path
/// `resolve_invocant_class` now goes through the witness bag, so
/// the Mojo `$self->{k}` access inside a Mojo::Base method no
/// longer flips `$self` to HashRef — method chains downstream
/// still see the class.
#[test]
fn test_wiring_mojo_self_invocant_class_via_public_api() {
    let fa = build_fa_from_source(
        r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

sub name {
    my $self = shift;
    return $self->{name} unless @_;
    $self->{name} = shift;
    $self;
}
        "#,
    );

    // At a point inside the method body where `$self` has been
    // assigned via `shift`, the flat `inferred_type` returns
    // HashRef (because the access flipped it). The witness-based
    // path — which is what `resolve_invocant_class` now calls —
    // should return the class.
    let point = Point { row: 5, column: 15 };

    // Walk up the scope chain to find the sub's scope.
    let scope = fa.scope_at(point).expect("scope");

    let resolved = fa.resolve_invocant_class_test("$self", scope, point);
    assert_eq!(
        resolved.as_deref(),
        Some("Mojolicious::Routes::Route"),
        "resolve_invocant_class (public path) should see $self as the class, \
             not HashRef — got {:?}",
        resolved
    );
}

/// Ternary RETURN: `sub f { return $c ? $self : $self }` —
/// both arms agree on `$self` (FirstParam), so `f`'s return
/// type collapses to that class. Same reduction as the RHS-
/// ternary + explicit-if/else-return paths, just emitted from
/// the return-expression visit on a Symbol attachment.
#[test]
fn test_witnesses_ternary_return_on_sub_symbol() {
    let fa = build_fa_from_source(
        r#"
package MyApp::Widget;

sub choose {
    my $self = shift;
    my $flag = shift;
    return $flag ? $self : $self;
}

sub literal_mix {
    my ($flag) = @_;
    return $flag ? 1 : 2;
}

sub disagree {
    my ($flag) = @_;
    return $flag ? 1 : "two";
}
        "#,
    );

    let choose = fa
        .symbols
        .iter()
        .find(|s| s.name == "choose" && matches!(s.kind, SymKind::Sub))
        .expect("sub choose");
    let return_type = fa.symbol_return_type_via_bag(choose.id, None);
    assert!(
        matches!(&return_type, Some(InferredType::FirstParam { package }) if package == "MyApp::Widget")
            || matches!(&return_type, Some(InferredType::ClassName(n)) if n == "MyApp::Widget"),
        "choose's return: ternary arms both $self → class; got {:?}",
        return_type
    );

    let literal = fa
        .symbols
        .iter()
        .find(|s| s.name == "literal_mix" && matches!(s.kind, SymKind::Sub))
        .expect("sub literal_mix");
    let return_type = fa.symbol_return_type_via_bag(literal.id, None);
    assert_eq!(
        return_type,
        Some(InferredType::Numeric),
        "literal_mix: both arms Numeric"
    );

    let disagree = fa
        .symbols
        .iter()
        .find(|s| s.name == "disagree" && matches!(s.kind, SymKind::Sub))
        .expect("sub disagree");
    let return_type = fa.symbol_return_type_via_bag(disagree.id, None);
    assert_eq!(
        return_type, None,
        "disagree: arms Numeric vs String → None"
    );
}

/// Ternary: `my $n = $c ? 1 : 2;` → both arms are Numeric, so
/// the fold via the bag yields Numeric.
#[test]
fn test_witnesses_ternary_agreeing_arms_yield_type() {
    let fa = build_fa_from_source(
        r#"
my $c = 1;
my $n = $c ? 10 : 20;
my $s = $c ? "a" : "b";
my $x = $c ? 10 : "oops";
        "#,
    );

    // $n: both arms Numeric.
    let p_n = Point { row: 2, column: 22 };
    let t_n = fa.inferred_type_via_bag("$n", p_n);
    assert_eq!(t_n, Some(InferredType::Numeric), "agreeing numeric arms");

    // $s: both arms String.
    let p_s = Point { row: 3, column: 22 };
    let t_s = fa.inferred_type_via_bag("$s", p_s);
    assert_eq!(t_s, Some(InferredType::String), "agreeing string arms");

    // $x: disagreeing — reducer returns None. The legacy
    // fallback doesn't handle ternaries either, so overall None.
    let p_x = Point { row: 4, column: 22 };
    let t_x = fa.inferred_type_via_bag("$x", p_x);
    assert_eq!(
        t_x, None,
        "disagreeing arms: bag returns None, legacy has no answer"
    );
}

/// Explicit if/else return arms use the same BranchArm reduction
/// as ternary. `sub pick { if ($c) { return 10 } else { return 20 } }`
/// — both arms Numeric, so the reducer returns Numeric for the
/// sub's return type.
#[test]
fn test_witnesses_if_else_branch_arm_on_sub() {
    let fa = build_fa_from_source(
        r#"
sub pick {
    my $c = shift;
    if ($c) { return 10 }
    else    { return 20 }
}

sub mix {
    my $c = shift;
    if ($c) { return 10 }
    else    { return "nope" }
}
        "#,
    );

    // Find the `pick` sub's symbol.
    let pick_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "pick" && matches!(s.kind, SymKind::Sub))
        .expect("sub pick");
    let mix_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "mix" && matches!(s.kind, SymKind::Sub))
        .expect("sub mix");

    // Query the bag for the sub's Symbol attachment, expect the
    // BranchArmFold to produce Numeric for `pick` and None for
    // `mix` (disagreement).
    use crate::witnesses::{
        FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry, WitnessAttachment,
    };
    let reg = ReducerRegistry::with_defaults();

    let att_p = WitnessAttachment::Symbol(pick_sym.id);
    let q_p = ReducerQuery {
        attachment: &att_p,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&fa.witnesses, &q_p),
        ReducedValue::Type(InferredType::Numeric),
        "pick: both arms Numeric → Numeric"
    );

    let att_m = WitnessAttachment::Symbol(mix_sym.id);
    let q_m = ReducerQuery {
        attachment: &att_m,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint: None, receiver: None, context: None,
    };
    assert_eq!(
        reg.query(&fa.witnesses, &q_m),
        ReducedValue::None,
        "mix: disagreement (Numeric vs String) → None"
    );
}

/// Extended arity idioms (Part C): exact-N matching.
#[test]
fn test_witnesses_arity_exact_n_variants() {
    let fa = build_fa_from_source(
        r#"
sub postfix_eq {
    return "zero" unless @_;
    return "one"  if @_ == 1;
    return "two"  if @_ == 2;
    return "many";
}

sub postfix_scalar {
    return "zero" if !@_;
    return "one"  if scalar(@_) == 1;
    return "dflt";
}

sub explicit_if {
    my ($self) = @_;
    if (@_ == 1) { return "alpha" }
    if (@_ == 2) { return "beta"  }
    return "gamma";
}
        "#,
    );

    // postfix_eq: arity-indexed returns.
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_eq", Some(0)),
        Some(InferredType::String),
        "postfix: arity 0 via `unless @_`"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_eq", Some(1)),
        Some(InferredType::String),
        "postfix: arity 1 via `if @_ == 1`"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_eq", Some(2)),
        Some(InferredType::String),
        "postfix: arity 2 via `if @_ == 2`"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_eq", Some(3)),
        Some(InferredType::String),
        "postfix: arity 3 → default"
    );

    // postfix_scalar: `!@_` and `scalar(@_) == 1` variants.
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_scalar", Some(0)),
        Some(InferredType::String),
        "postfix: arity 0 via `if !@_`"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("postfix_scalar", Some(1)),
        Some(InferredType::String),
        "postfix: arity 1 via `if scalar(@_) == 1`"
    );

    // explicit_if: `if (@_ == N) { return … }`.
    assert_eq!(
        fa.sub_return_type_at_arity("explicit_if", Some(1)),
        Some(InferredType::String),
        "explicit: arity 1 via if (@_ == 1) return arm"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("explicit_if", Some(2)),
        Some(InferredType::String),
        "explicit: arity 2"
    );
}

/// Part 6b — fluent arity dispatch. Two unambiguous return
/// types on either side of `unless @_` — arity 0 vs default
/// return different types. Verifies the reducer picks correctly.
#[test]
fn test_witnesses_fluent_arity_dispatch_literal_returns() {
    let fa = build_fa_from_source(
        r#"
package MyApp::Route;

sub name {
    my $self = shift;
    return "configured" unless @_;
    return 42;
}
        "#,
    );

    let t0 = fa.sub_return_type_at_arity("name", Some(0));
    let t1 = fa.sub_return_type_at_arity("name", Some(1));
    let td = fa.sub_return_type_at_arity("name", None);

    assert_eq!(
        t0,
        Some(InferredType::String),
        "arity=0 fires the `unless @_` branch (String return)"
    );
    assert_eq!(
        t1,
        Some(InferredType::Numeric),
        "arity=1 falls through to default branch (Numeric return)"
    );
    assert_eq!(
        td,
        Some(InferredType::Numeric),
        "no arity hint also falls through to default"
    );
}

/// Non-Mojo class without any framework mode: hashref-only access
/// with no class assertion should still fold to HashRef (the bag
/// mirror of the current flat behavior).
#[test]
fn test_witnesses_plain_hashref_without_class_evidence() {
    let fa = build_fa_from_source(
        r#"
my $cfg = { host => 'localhost' };
my $host = $cfg->{host};
        "#,
    );

    let point = Point { row: 2, column: 20 };
    let t = fa.inferred_type_via_bag("$cfg", point);
    assert!(
        t.as_ref().is_some_and(|t| t.is_hash_shaped() && t.class_name().is_none()),
        "without class evidence, $cfg is hash-shaped and classless: {:?}",
        t,
    );
}

/// Inherited-method rename — pin the chain that the LSP rename
/// path walks. e2e `rename: process → execute` was the surface
/// failure: `rename_method_in_class` strict-matches both the
/// def's `package` and the call site's `invocant_class`, so a
/// rename starting on `$worker->process()` (invocant=MyWorker)
/// only catches the call sites in the child class — never
/// reaches `sub process` in `BaseWorker.pm`. The chain is
/// `[child, …, defining ancestor]`; backend renames in every
/// link and merges. Stops at the first defining ancestor so
/// overrides in unrelated branches aren't lumped in.
#[test]
fn red_pin_method_rename_chain_walks_to_defining_ancestor() {
    // Same-file inheritance — keeps the test free of the
    // module_index, which still gets exercised end-to-end via the
    // e2e suite.
    let src = "\
package Animal;
sub speak { return '...' }
sub breathe { return 1 }

package Dog;
our @ISA = ('Animal');
sub speak { return 'Woof!' }
# `breathe` inherited; no override

package main;
my $dog = bless {}, 'Dog';
$dog->breathe();
$dog->speak();
";
    let fa = build_fa_from_source(src);

    // Inherited (defined in Animal, not in Dog) — chain runs
    // child → defining ancestor and stops.
    let chain = fa.method_rename_chain("Dog", "breathe", None);
    assert_eq!(
        chain,
        vec!["Dog".to_string(), "Animal".to_string()],
        "inherited method: chain must reach the defining ancestor",
    );

    // Override (Dog defines `speak` itself) — chain stops at
    // child. Walking past the override into Animal would lump
    // two semantically distinct methods together in one rename.
    let chain = fa.method_rename_chain("Dog", "speak", None);
    assert_eq!(
        chain,
        vec!["Dog".to_string()],
        "overridden method: chain must stop at the defining (= child) class, \
             not include the parent's same-named method",
    );

    // Unknown method: degrade to the original class so the
    // backend's per-class rename still runs (no edits, no harm).
    let chain = fa.method_rename_chain("Dog", "nonexistent", None);
    assert_eq!(chain, vec!["Dog".to_string()]);
}

// ---- Mojolicious route controller-token → class ----

/// Camelize matches `Mojo::Util::camelize` exactly (verified against
/// the crm-vendored Mojo::Util source): `-` → `::`, `_` within a
/// segment → `ucfirst lc` pieces concatenated, leading-uppercase token
/// passes through unchanged.
#[test]
fn test_camelize_controller_matches_mojo_util() {
    let cases = [
        ("login", "Login"),
        ("sales_reports", "SalesReports"),
        ("integrations-ads_api", "Integrations::AdsApi"),
        ("foo_bar-baz", "FooBar::Baz"),
        ("users", "Users"),
        // Already class-shaped: untouched (camelize's `/^[A-Z]/` gate).
        ("FooBar", "FooBar"),
        ("Foo::Bar", "Foo::Bar"),
        // Mixed-case piece is lowercased first, then ucfirst'd.
        ("sales_REPORTS", "SalesReports"),
    ];
    for (input, want) in cases {
        assert_eq!(
            camelize_controller(input),
            want,
            "camelize_controller({input:?})",
        );
    }
}

#[test]
fn test_module_tail_matches() {
    assert!(module_tail_matches("Clove::Controller::Login", "Login"));
    assert!(module_tail_matches(
        "App::Controller::Integrations::AdsApi",
        "Integrations::AdsApi",
    ));
    assert!(module_tail_matches("Login", "Login"));
    // Tail must align on a `::` boundary — not a substring.
    assert!(!module_tail_matches("Clove::Controller::AdminLogin", "Login"));
    assert!(!module_tail_matches("Clove::Controller::Logins", "Login"));
}

/// Cross-file pin: a route `->to('users#list')` (controller token
/// `users`, lowercase, never a Perl package name on its own) resolves
/// its MethodCall invocant to the controller class under the app's
/// namespace that owns `list`.
#[test]
fn test_route_controller_token_resolves_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // App file declaring the route. The mojo-routes plugin turns the
    // `->to('users#list')` string into a `MethodCall { invocant: "users",
    // method_name: "list" }` — the token is passed raw.
    let app = build_fa_from_source(
        r#"
        package Some::App;
        use Mojolicious::Lite;
        sub startup {
            my $self = shift;
            $self->routes->get('/users')->to('users#list');
        }
        1;
        "#,
    );

    // The controller class lives in another file under the app's
    // controller namespace and defines `list`.
    let controller = build_fa_from_source(
        r#"
        package Some::App::Controller::Users;
        use Mojo::Base 'Mojolicious::Controller';
        sub list { my $c = shift; }
        1;
        "#,
    );

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Some_App_Controller_Users.pm"),
        Arc::new(controller),
    );

    let to_ref = app
        .refs
        .iter()
        .find(|r| {
            r.target_name == "list"
                && matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "users")
        })
        .expect("plugin emits MethodCall {invocant: users, method: list} for ->to('users#list')");

    let class = app.method_call_invocant_class(to_ref, Some(&idx));
    assert_eq!(
        class.as_deref(),
        Some("Some::App::Controller::Users"),
        "controller token `users` should camelize + workspace-resolve to the owning controller class",
    );
}

/// Composition pin (crm's Alerts.pm shape): a partial `->to('#list')`
/// off a route whose controller default was set by an ancestor
/// `->to('alerts#')` must inherit `alerts`, camelize it, and resolve
/// cross-file to the owning controller class — exactly like the full
/// form `->to('alerts#list')` does. Mirrors:
///
/// ```text
/// my $alerts_r = $r->any('/alerts')->to('alerts#');  # brand controller=alerts
/// $alerts_r->get('/')->to('#list');                  # partial: inherits alerts
/// my $crud = $alerts_r->under('/:type')->to('#get'); # nested: inherits alerts
/// $crud->get('/x')->to('#read');                     # partial off nested
/// ```
#[test]
fn test_partial_route_brand_composes_with_camelize_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app = build_fa_from_source(
        r#"
        package Clove::App;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            my $r = $self->routes;
            my $alerts_r = $r->any('/alerts')->to('alerts#');
            $alerts_r->get('/')->to('#list');
            my $crud = $alerts_r->under('/:type')->to('#get_alert');
            $crud->get('/settings')->to('#read_settings');
            my $billing_r = $r->any('/billing')->to('billing#');
            $billing_r->get('/')->to('#index');
        }
        1;
        "#,
    );

    let alerts = build_fa_from_source(
        r#"
        package Clove::Controller::Alerts;
        use Mojo::Base 'Mojolicious::Controller';
        sub list { my $c = shift; }
        sub get_alert { my $c = shift; }
        sub read_settings { my $c = shift; }
        1;
        "#,
    );
    let billing = build_fa_from_source(
        r#"
        package Clove::Controller::Billing;
        use Mojo::Base 'Mojolicious::Controller';
        sub index { my $c = shift; }
        1;
        "#,
    );

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Clove_Controller_Alerts.pm"),
        Arc::new(alerts),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Clove_Controller_Billing.pm"),
        Arc::new(billing),
    );

    // Direct child (`$alerts_r->get('/')->to('#list')`), nested via
    // `under` (`$crud` inherits `alerts`), and a partial off the nested
    // group all resolve to the same controller class as the full form.
    for action in ["list", "get_alert", "read_settings"] {
        let to_ref = app
            .refs
            .iter()
            .find(|r| {
                r.target_name == action
                    && matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "alerts")
            })
            .unwrap_or_else(|| {
                panic!("partial ->to('#{action}') should emit MethodCall {{invocant: alerts, method: {action}}}")
            });

        let class = app.method_call_invocant_class(to_ref, Some(&idx));
        assert_eq!(
            class.as_deref(),
            Some("Clove::Controller::Alerts"),
            "partial ->to('#{action}') should inherit controller `alerts`, camelize, and resolve cross-file",
        );
    }

    // Sibling group re-brands its own descendants — no leak from
    // `alerts`. The partial `#index` inherits `billing`, not `alerts`.
    let index_ref = app
        .refs
        .iter()
        .find(|r| {
            r.target_name == "index"
                && matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "billing")
        })
        .expect("sibling group partial ->to('#index') should inherit `billing`");
    assert_eq!(
        app.method_call_invocant_class(index_ref, Some(&idx)).as_deref(),
        Some("Clove::Controller::Billing"),
        "sibling group must re-brand to `billing`, not leak `alerts`",
    );
}

/// When the same short controller name exists under multiple controller
/// roots, ownership of the action disambiguates: only the class that
/// actually defines the method is chosen.
#[test]
fn test_route_controller_token_disambiguates_by_ownership() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app = build_fa_from_source(
        r#"
        package Some::App;
        use Mojolicious::Lite;
        sub startup {
            my $self = shift;
            $self->routes->get('/r')->to('reports#monthly');
        }
        1;
        "#,
    );

    // Two `*::Controller::Reports` classes; only the second defines
    // `monthly`. The first is a decoy with a different action.
    let decoy = build_fa_from_source(
        r#"
        package Alpha::Controller::Reports;
        sub daily { }
        1;
        "#,
    );
    let owner = build_fa_from_source(
        r#"
        package Beta::Controller::Reports;
        sub monthly { }
        1;
        "#,
    );

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Alpha_Reports.pm"),
        Arc::new(decoy),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Beta_Reports.pm"),
        Arc::new(owner),
    );

    let to_ref = app
        .refs
        .iter()
        .find(|r| {
            r.target_name == "monthly"
                && matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "reports")
        })
        .expect("MethodCall for ->to('reports#monthly')");

    let class = app.method_call_invocant_class(to_ref, Some(&idx));
    assert_eq!(
        class.as_deref(),
        Some("Beta::Controller::Reports"),
        "ownership of `monthly` should pick Beta over the Alpha decoy",
    );
}

// ---- ReceiverGated seam ----

#[test]
fn receiver_gated_applies_on_exact_gate() {
    let pp: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    let gated = ReceiverGated::new("Minion", 7u32);
    assert_eq!(
        gated.resolve_for(Some("Minion"), &pp, None),
        GateResult::Applies(&7u32),
    );
}

#[test]
fn receiver_gated_applies_through_local_ancestry() {
    let mut pp: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    pp.insert("My::Minion".into(), vec!["Minion".into()]);
    let gated = ReceiverGated::new("Minion", "payload");
    // A descendant resolves through the single `class_isa` walk.
    assert_eq!(
        gated.resolve_for(Some("My::Minion"), &pp, None),
        GateResult::Applies(&"payload"),
    );
}

#[test]
fn receiver_gated_does_not_apply_for_unrelated_class() {
    let pp: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    let gated = ReceiverGated::new("Minion", 1u8);
    // A concrete, unrelated receiver is a settled negative — NOT untyped.
    assert_eq!(
        gated.resolve_for(Some("Some::Other"), &pp, None),
        GateResult::DoesNotApply,
    );
}

#[test]
fn receiver_gated_untyped_for_unknown_receiver() {
    let pp: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    let gated = ReceiverGated::new("Minion", 1u8);
    assert_eq!(gated.resolve_for(None, &pp, None), GateResult::ReceiverUntyped);
    assert_eq!(gated.resolve_for(Some(""), &pp, None), GateResult::ReceiverUntyped);
}

// ---- %EXPORT_TAGS membership folds into the export surface (B-tag / NAV-B) ----

#[test]
fn export_tags_plain_hash_members_are_exported() {
    let fa = build_fa_from_source(
        r#"
        package M;
        our @EXPORT_OK = qw( foo );
        our %EXPORT_TAGS = (
            all             => [ @EXPORT_OK ],
            data_conversion => [ qw{ hashify words_from_string } ],
        );
        sub foo { 1 }
        sub hashify { 2 }
        sub words_from_string { 3 }
        1;
        "#,
    );
    // Members reached only via a tag list join the same surface as @EXPORT_OK.
    assert!(fa.exports_name("hashify"), "tag member hashify should export");
    assert!(fa.exports_name("words_from_string"));
    assert!(fa.exports_name("foo"));
    // The tag *names* are selectors, not subs — never exports.
    assert!(!fa.exports_name("data_conversion"));
    assert!(!fa.exports_name("all"));
}

#[test]
fn export_tags_readonly_wrapped_members_are_exported() {
    // The Perl::Critic::Utils shape: the table is hidden inside a
    // `Readonly::Hash our %EXPORT_TAGS => (...)` call. The wrapper function is
    // not load-bearing; the declared variable is the witness.
    let fa = build_fa_from_source(
        r#"
        package M;
        Readonly::Array our @EXPORT_OK => qw( interpolate );
        Readonly::Hash our %EXPORT_TAGS => (
            all             => [ @EXPORT_OK ],
            data_conversion => [ qw{ hashify words_from_string interpolate } ],
        );
        sub interpolate { 1 }
        sub hashify { 2 }
        sub words_from_string { 3 }
        1;
        "#,
    );
    assert!(fa.exports_name("hashify"), "Readonly-wrapped tag member should export");
    assert!(fa.exports_name("words_from_string"));
    assert!(fa.exports_name("interpolate"));
    assert!(!fa.exports_name("data_conversion"));
    assert!(!fa.exports_name("all"));
}

#[test]
fn name_in_no_export_or_tag_is_not_exported() {
    // Regression guard: a sub that is neither in @EXPORT* nor any tag list
    // stays unexported — the fold widens the surface only for tag members.
    let fa = build_fa_from_source(
        r#"
        package M;
        our @EXPORT_OK = qw( foo );
        our %EXPORT_TAGS = (
            data_conversion => [ qw{ hashify } ],
        );
        sub foo { 1 }
        sub hashify { 2 }
        sub private_helper { 3 }
        1;
        "#,
    );
    assert!(fa.exports_name("hashify"));
    assert!(fa.exports_name("foo"));
    assert!(!fa.exports_name("private_helper"), "non-exported sub must stay unexported");
}

#[test]
fn cross_file_slot_write_types_the_read() {
    // A4 v2: the typed slot write lives in the PARENT class's file;
    // the read in the child file narrows through the registry's
    // SlotType cross-file + ancestor arm at query time.
    use std::sync::Arc;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let parent_src = "package Parent;\nsub init {\n    my ($self) = @_;\n    $self->{conn} = My::Conn->new;\n}\n1;\n";
    {
        let mut parser = crate::builder::create_parser();
        let tree = parser.parse(parent_src, None).unwrap();
        let fa = crate::builder::build(&tree, parent_src.as_bytes());
        idx.insert_cache(
            "Parent",
            Some(Arc::new(crate::file_analysis::CachedModule::new(
                std::path::PathBuf::from("/fake/Parent.pm"),
                Arc::new(fa),
            ))),
        );
    }
    let child_src = "package Child;\nuse Moo;\nextends 'Parent';\nsub go {\n    my ($self) = @_;\n    my $x = $self->{conn};\n}\n1;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(child_src, None).unwrap();
    let fa = crate::builder::build(&tree, child_src.as_bytes());

    // the read expression `$self->{conn}` on line 5 (0-based)
    let read_span = fa
        .refs
        .iter()
        .find(|r| {
            matches!(r.kind, RefKind::HashKeyAccess { .. })
                && r.target_name == "conn"
                && r.span.start.row == 5
        })
        .map(|r| r.span)
        .expect("read-site hash key ref");
    // the Expr witness covers the whole `$self->{conn}` element; ask at
    // the element span — expr_type_at_span finds the narrowest cover.
    let t = fa.expr_type_at_span(
        Span { start: Point { row: 5, column: 12 }, end: Point { row: 5, column: 25 } },
        Some(&idx),
    );
    assert_eq!(
        t,
        Some(InferredType::ClassName("My::Conn".into())),
        "read at {read_span:?} should narrow via the parent file's slot write",
    );
    // and the variable that received it — `$x` rides Edge(Expr(read))
    let t2 = fa.inferred_type_via_bag_ctx(
        "$x",
        Point { row: 6, column: 0 },
        Some(&idx),
    );
    assert_eq!(t2, Some(InferredType::ClassName("My::Conn".into())));
}


#[test]
fn loader_config_types_register_conf_cross_file() {
    // #25, framework-mediated: `plugin 'CloveApp', {...}` in the app
    // types `$conf` inside the plugin's register — callers enumerable
    // by construction (the PluginLoad facts name this module).
    use std::sync::Arc;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let app_src = "use Mojolicious::Lite;\nplugin 'CloveApp', { minion => 1, redis => 'r' };\napp->start;\n";
    {
        let mut parser = crate::builder::create_parser();
        let tree = parser.parse(app_src, None).unwrap();
        let fa = crate::builder::build(&tree, app_src.as_bytes());
        assert!(
            fa.plugin_loads.iter().any(|f| f.name == "CloveApp" && f.config_span.is_some()),
            "the lite plugin arm should record the loader fact: {:?}",
            fa.plugin_loads,
        );
        idx.register_workspace_module(
            std::path::PathBuf::from("/fake/conf/app.pl.pm-shim"),
            Arc::new(fa),
        );
    }
    // packageless shim doesn't register; use insert_cache instead
    let app_src2 = "package MyApp::Boot;\nuse Mojolicious::Lite;\nplugin 'CloveApp', { minion => 1, redis => 'r' };\n1;\n";
    {
        let mut parser = crate::builder::create_parser();
        let tree = parser.parse(app_src2, None).unwrap();
        let fa = crate::builder::build(&tree, app_src2.as_bytes());
        idx.insert_cache(
            "MyApp::Boot",
            Some(Arc::new(crate::file_analysis::CachedModule::new(
                std::path::PathBuf::from("/fake/conf/Boot.pm"),
                Arc::new(fa),
            ))),
        );
    }

    let plugin_src = "package Mojolicious::Plugin::CloveApp;\nuse Mojo::Base 'Mojolicious::Plugin';\n\nsub register {\n    my ($self, $app, $conf) = @_;\n}\n1;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(plugin_src, None).unwrap();
    let mut fa = crate::builder::build(&tree, plugin_src.as_bytes());
    assert!(
        !fa.loader_config_params.is_empty(),
        "the param_types from_loader_config rule should mint a marker",
    );
    fa.enrich_imported_types_with_keys(Some(&idx));

    let t = fa.inferred_type_via_bag("$conf", Point { row: 5, column: 0 });
    match t {
        Some(InferredType::HashWithKeys { keys, .. }) => {
            let mut names: Vec<&str> = keys.iter().map(|(k, _)| k.as_str()).collect();
            names.sort();
            assert_eq!(names, vec!["minion", "redis"]);
        }
        other => panic!("expected the gathered config shape, got {other:?}"),
    }
}

/// Branded edges, end to end: two self-contained Mojo::Lite apps in one
/// workspace register different helpers. Both helpers bridge to the same
/// app-surface class, so before brands they MERGED — app-two saw
/// app-one's helper. After per-file branding, each app sees only its own.
/// See `docs/adr/branded-edges.md`.
#[test]
fn multi_app_lite_helpers_do_not_leak_across_apps() {
    use crate::file_analysis::APP_SURFACE_CLASS;
    use std::sync::Arc;

    fn lite_app(idx: &crate::module_index::ModuleIndex, module: &str, file_id: &str, helper: &str) -> FileAnalysis {
        let src = format!(
            "use Mojolicious::Lite;\napp->helper({helper} => sub {{ my $c = shift; 1 }});\napp->start;\n"
        );
        let mut parser = crate::builder::create_parser();
        let tree = parser.parse(&src, None).unwrap();
        let mut fa = crate::builder::build(&tree, src.as_bytes());
        fa.apply_home_brand(file_id);
        // register a clone into the index so the OTHER app sees it cross-file
        idx.insert_cache(
            module,
            Some(Arc::new(crate::file_analysis::CachedModule::new(
                std::path::PathBuf::from(file_id),
                Arc::new({
                    let mut c = crate::builder::build(&tree, src.as_bytes());
                    c.apply_home_brand(file_id);
                    c
                }),
            ))),
        );
        fa
    }

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let app_one = lite_app(&idx, "App::One", "/ws/one.pl", "alpha_helper");
    let _app_two = lite_app(&idx, "App::Two", "/ws/two.pl", "beta_helper");

    // sanity: branding actually happened (Lite app → home_brand set)
    assert_eq!(app_one.home_brand.as_deref(), Some("/ws/one.pl"));

    let labels: Vec<String> = app_one
        .complete_methods_for_class(APP_SURFACE_CLASS, Some(&idx))
        .into_iter()
        .map(|c| c.label)
        .collect();
    assert!(
        labels.iter().any(|l| l == "alpha_helper"),
        "app-one must still see its OWN helper; got {labels:?}",
    );
    assert!(
        !labels.iter().any(|l| l == "beta_helper"),
        "app-one must NOT see app-two's helper (the leak); got {labels:?}",
    );
}

/// Per-VARIABLE instance brands: two Minion instances in one file own
/// distinct task sets. `$a->enqueue('task_b')` must NOT resolve to the
/// task `$b` registered — they're different instances. See
/// `docs/adr/branded-edges.md`.
#[test]
fn minion_tasks_are_scoped_per_instance() {
    let src = "use Minion;\n\
        my $a = Minion->new;\n\
        $a->add_task(task_a => sub { 1 });\n\
        my $b = Minion->new;\n\
        $b->add_task(task_b => sub { 2 });\n\
        $a->enqueue('task_a');\n\
        $a->enqueue('task_b');\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());

    // helper: the resolves_to target name for an enqueue DispatchCall ref.
    let enqueue_target = |task: &str| -> Option<String> {
        let r = fa.refs.iter().find(|r| {
            matches!(&r.kind, crate::file_analysis::RefKind::DispatchCall { dispatcher, .. } if dispatcher == "enqueue")
                && r.target_name == task
        })?;
        r.resolves_to.map(|sid| fa.symbols[sid.0 as usize].name.clone())
    };

    // sanity: both task handlers exist and are branded by distinct instances.
    let brand_of = |task: &str| -> Option<String> {
        fa.symbols.iter().find(|s| s.name == task
            && matches!(s.detail, crate::file_analysis::SymbolDetail::Handler { .. }))
            .and_then(|s| match &s.detail {
                crate::file_analysis::SymbolDetail::Handler { instance_brand, .. } => instance_brand.clone(),
                _ => None,
            })
    };
    let ba = brand_of("task_a");
    let bb = brand_of("task_b");
    assert!(ba.is_some() && bb.is_some(), "both tasks must be instance-branded: {ba:?} {bb:?}");
    assert_ne!(ba, bb, "tasks on $a and $b must carry distinct brands");

    // $a->enqueue('task_a') resolves to $a's own task.
    assert_eq!(enqueue_target("task_a").as_deref(), Some("task_a"),
        "$a->enqueue('task_a') should resolve to $a's task");
    // $a->enqueue('task_b') must NOT resolve — task_b belongs to $b.
    assert_eq!(enqueue_target("task_b"), None,
        "$a->enqueue('task_b') must NOT reach $b's task (the leak)");
}

/// Accessor-chain instance brands (`$app->minion`): the brand keys on the
/// LEAF ACCESSOR (singleton-accessor semantics), not the base expression.
/// So `$app->minion` and `$c->minion` (same accessor, different base —
/// the common plugin-registers / controller-enqueues split) share an
/// instance, while `$app->other_minion` is a distinct one. See
/// `docs/adr/branded-edges.md`.
#[test]
fn accessor_chain_tasks_scoped_by_accessor() {
    let src = "use Minion;\n\
        $app->minion->add_task(only_minion => sub { 1 });\n\
        $app->other_minion->add_task(only_other => sub { 2 });\n\
        $app->minion->enqueue('only_minion');\n\
        $app->minion->enqueue('only_other');\n\
        $c->minion->enqueue('only_minion');\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());
    let resolved_at = |row: usize, task: &str| -> bool {
        fa.refs.iter().any(|r| {
            matches!(&r.kind, crate::file_analysis::RefKind::DispatchCall { dispatcher, .. } if dispatcher == "enqueue")
                && r.target_name == task
                && r.span.start.row == row
                && r.resolves_to.is_some()
        })
    };
    // $app->minion->enqueue('only_minion') resolves (same accessor).
    assert!(resolved_at(3, "only_minion"), "own accessor's task must resolve");
    // $app->minion->enqueue('only_other') must NOT — other_minion's task.
    assert!(!resolved_at(4, "only_other"), "a different accessor's task must NOT resolve (the leak)");
    // $c->minion->enqueue('only_minion') resolves — different base, SAME
    // accessor is the same singleton instance (must not regress).
    assert!(resolved_at(5, "only_minion"), "same accessor on a different base must still resolve");
}

/// F1 regression: a re-`my` of the receiver (Perl shadowing) must brand
/// build-side and resolve query-side on the SAME (latest) declaration —
/// otherwise the brands disagree and the own task silently fails to
/// resolve. `enqueue('task_b')` (the second `$m`'s task) resolves;
/// `enqueue('task_a')` (the shadowed-away first `$m`'s) does not.
#[test]
fn minion_tasks_shadowed_receiver_resolve_consistently() {
    let src = "use Minion;\n\
        my $m = Minion->new;\n\
        $m->add_task(task_a => sub { 1 });\n\
        my $m = Minion->new;\n\
        $m->add_task(task_b => sub { 2 });\n\
        $m->enqueue('task_b');\n\
        $m->enqueue('task_a');\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());
    let resolved = |task: &str| -> bool {
        fa.refs.iter().any(|r| {
            matches!(&r.kind, crate::file_analysis::RefKind::DispatchCall { dispatcher, .. } if dispatcher == "enqueue")
                && r.target_name == task
                && r.resolves_to.is_some()
        })
    };
    assert!(resolved("task_b"), "the current $m's own task must resolve");
    assert!(!resolved("task_a"), "a task on the shadowed-away $m must NOT resolve");
}

/// F2 regression: stacked handlers with the same (name, owner) — the
/// dispatch ref must resolve to the LAST-defined one (the pre-brand
/// `(name,owner)` HashMap was last-write-wins). Two `dup` tasks on the
/// same `$m`; `enqueue('dup')` resolves to the second registration.
#[test]
fn stacked_tasks_resolve_to_last_defined() {
    let src = "use Minion;\n\
        my $m = Minion->new;\n\
        $m->add_task(dup => sub { 1 });\n\
        $m->add_task(dup => sub { 2 });\n\
        $m->enqueue('dup');\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(src, None).unwrap();
    let fa = crate::builder::build(&tree, src.as_bytes());
    // the two handler defs, by declaration order (span).
    let mut defs: Vec<(crate::file_analysis::SymbolId, u32)> = fa.symbols.iter()
        .filter(|s| s.name == "dup" && matches!(s.detail, crate::file_analysis::SymbolDetail::Handler { .. }))
        .map(|s| (s.id, s.span.start.row as u32))
        .collect();
    defs.sort_by_key(|(_, row)| *row);
    assert_eq!(defs.len(), 2, "expected two stacked dup handlers");
    let last = defs.last().unwrap().0;
    let target = fa.refs.iter().find(|r| {
        matches!(&r.kind, crate::file_analysis::RefKind::DispatchCall { dispatcher, .. } if dispatcher == "enqueue")
            && r.target_name == "dup"
    }).and_then(|r| r.resolves_to);
    assert_eq!(target, Some(last), "enqueue('dup') must resolve to the LAST-defined handler");
}

/// The same two apps, but app-one is left UNBRANDED (home_brand None) —
/// the pre-feature state. It is brand-agnostic, so it sees everything,
/// proving the consumer brand is what filters (and that branding is
/// additive, never a regression for unbranded callers).
#[test]
fn unbranded_app_still_sees_everything_no_regression() {
    use crate::file_analysis::APP_SURFACE_CLASS;
    use std::sync::Arc;

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let two_src = "use Mojolicious::Lite;\napp->helper(beta_helper => sub { my $c = shift; 1 });\napp->start;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(two_src, None).unwrap();
    let mut two = crate::builder::build(&tree, two_src.as_bytes());
    two.apply_home_brand("/ws/two.pl");
    idx.insert_cache(
        "App::Two",
        Some(Arc::new(crate::file_analysis::CachedModule::new(
            std::path::PathBuf::from("/ws/two.pl"),
            Arc::new(two),
        ))),
    );

    let one_src = "use Mojolicious::Lite;\napp->helper(alpha_helper => sub { my $c = shift; 1 });\napp->start;\n";
    let tree1 = parser.parse(one_src, None).unwrap();
    let app_one = crate::builder::build(&tree1, one_src.as_bytes()); // NOT branded
    assert!(app_one.home_brand.is_none());

    let labels: Vec<String> = app_one
        .complete_methods_for_class(APP_SURFACE_CLASS, Some(&idx))
        .into_iter()
        .map(|c| c.label)
        .collect();
    // agnostic query sees the cross-file helper — exactly the pre-brand behavior.
    assert!(
        labels.iter().any(|l| l == "beta_helper"),
        "an unbranded (agnostic) app must see every helper, no regression; got {labels:?}",
    );
}
