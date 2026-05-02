use super::*;
use tree_sitter::Point;

/// Helper: build a minimal FileAnalysis with a single file scope and given type constraints.
fn fa_with_constraints(constraints: Vec<TypeConstraint>) -> FileAnalysis {
    FileAnalysis::new(
        vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        vec![],
        vec![],
        constraints,
        vec![],
        vec![],
        vec![],
        HashMap::new(),
        vec![],
        HashSet::new(),
        vec![],
        vec![],
        vec![],
        HashMap::new(),
        HashMap::new(),
        vec![],
    )
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
    let fa = fa_with_constraints(vec![constraint("$cref", 0, InferredType::CodeRef)]);
    assert_eq!(
        fa.inferred_type_via_bag("$cref", Point::new(1, 0)),
        Some(InferredType::CodeRef)
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
    // Hand-craft a FileAnalysis with a sub that has a return type
    let fa = FileAnalysis::new(
        vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        vec![Symbol {
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
                return_type: Some(InferredType::HashRef),
                doc: None,
                display: None,
                hide_in_outline: false,
                opaque_return: false,
                return_self_method: None,
            },
            namespace: Namespace::Language,
            outline_label: None,
        }],
        vec![],
        vec![],
        vec![],
        vec![],
        vec![],
        HashMap::new(),
        vec![],
        HashSet::new(),
        vec![],
        vec![],
        vec![],
        HashMap::new(),
        HashMap::new(),
        vec![],
    );
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
        resolve_return_type(&[InferredType::CodeRef]),
        Some(InferredType::CodeRef),
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
    assert_eq!(InferredType::CodeRef.class_name(), None);
    assert_eq!(InferredType::Regexp.class_name(), None);
    assert_eq!(InferredType::Numeric.class_name(), None);
    assert_eq!(InferredType::String.class_name(), None);
}

// ---- sub_return_type fallback tests ----

#[test]
fn test_sub_return_type_imported_fallback() {
    let mut fa = fa_with_constraints(vec![]);
    let mut imported = HashMap::new();
    imported.insert("get_config".to_string(), InferredType::HashRef);
    fa.enrich_imported_types(imported);

    assert_eq!(
        fa.sub_return_type_at_arity("get_config", None),
        Some(InferredType::HashRef)
    );
    // Local subs should still return None
    assert_eq!(fa.sub_return_type_at_arity("nonexistent", None), None);
}

// ---- enrich_imported_types tests ----

#[test]
fn test_enrich_imported_types_pushes_constraints() {
    let mut fa = FileAnalysis::new(
        vec![Scope {
            id: ScopeId(0),
            parent: None,
            kind: ScopeKind::File,
            span: Span {
                start: Point::new(0, 0),
                end: Point::new(10, 0),
            },
            package: None,
        }],
        vec![],
        vec![],
        vec![],
        vec![],
        vec![],
        // A call binding: my $cfg = get_config()
        vec![CallBinding {
            variable: "$cfg".to_string(),
            func_name: "get_config".to_string(),
            scope: ScopeId(0),
            span: Span {
                start: Point::new(2, 0),
                end: Point::new(2, 30),
            },
        }],
        HashMap::new(),
        vec![],
        HashSet::new(),
        vec![],
        vec![],
        vec![],
        HashMap::new(),
        HashMap::new(),
        vec![],
    );

    let mut imported = HashMap::new();
    imported.insert("get_config".to_string(), InferredType::HashRef);
    fa.enrich_imported_types(imported);

    // Should have pushed a type constraint for $cfg
    assert_eq!(
        fa.inferred_type_via_bag("$cfg", Point::new(3, 0)),
        Some(InferredType::HashRef),
        "Enrichment should propagate imported return type to call binding variable"
    );
}

// ---- Phase 5: refs_by_target index + eager HashKeyAccess resolution ----

fn build_fa_from_source(source: &str) -> FileAnalysis {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
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

    // Simulate what the Backend's enrich_analysis does after the
    // module index resolves TestExporter::get_config returning HashRef
    // with keys { host, port, name }.
    let mut imported_returns = HashMap::new();
    imported_returns.insert("get_config".to_string(), InferredType::HashRef);
    let mut imported_hash_keys = HashMap::new();
    imported_hash_keys.insert(
        "get_config".to_string(),
        vec!["host".to_string(), "port".to_string(), "name".to_string()],
    );
    fa.enrich_imported_types_with_keys(imported_returns, imported_hash_keys, None);

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
    let refs = fa.find_references(point, None, None);
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
[MODULE] use strict @L35
[MODULE] use warnings @L36
[MODULE] use Mojolicious::Lite @L37
[MODULE] use Mojolicious @L38
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
[MODULE] use Minion @L104
[VARIABLE] $minion @L105
[FUNCTION] <task> send_email ($to, $subject, $body) @L107
[FUNCTION] <task> resize_image ($path, $width, $height) @L113
[NAMESPACE] MyApp::Progress @L131
[MODULE] use parent @L132
[FUNCTION] <sub> new @L134
  [VARIABLE] $class @L135
  [VARIABLE] $self @L136
  [EVENT] <event> ready ($ctx) @L137
  [EVENT] <event> step ($n, $total) @L138
  [EVENT] <event> done ($result) @L139
[FUNCTION] <sub> tick ($n) @L143
  [VARIABLE] $self @L144
  [VARIABLE] $n @L144
";
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
    let t = fa.method_call_return_type_via_bag(get_ref_idx);
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
    let ty = fa.resolve_expression_type(invocant, source_bytes, None);
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
    if let SymbolDetail::Sub { return_type, .. } = &choose.detail {
        assert!(
            matches!(return_type, Some(InferredType::FirstParam { package }) if package == "MyApp::Widget")
                || matches!(return_type, Some(InferredType::ClassName(n)) if n == "MyApp::Widget"),
            "choose's return: ternary arms both $self → class; got {:?}",
            return_type
        );
    }

    let literal = fa
        .symbols
        .iter()
        .find(|s| s.name == "literal_mix" && matches!(s.kind, SymKind::Sub))
        .expect("sub literal_mix");
    if let SymbolDetail::Sub { return_type, .. } = &literal.detail {
        assert_eq!(
            return_type,
            &Some(InferredType::Numeric),
            "literal_mix: both arms Numeric"
        );
    }

    let disagree = fa
        .symbols
        .iter()
        .find(|s| s.name == "disagree" && matches!(s.kind, SymKind::Sub))
        .expect("sub disagree");
    if let SymbolDetail::Sub { return_type, .. } = &disagree.detail {
        assert_eq!(
            return_type, &None,
            "disagree: arms Numeric vs String → None"
        );
    }
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
        arity_hint: None, context: None,
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
        arity_hint: None, context: None,
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
    assert_eq!(
        t,
        Some(InferredType::HashRef),
        "without class evidence, $cfg is plain HashRef"
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
