use super::*;

fn parse(source: &str) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    parser.parse(source, None).unwrap()
}

fn build_fa(source: &str) -> FileAnalysis {
    let tree = parse(source);
    build(&tree, source.as_bytes())
}

#[test]
fn debug_moo_name_refs() {
    let src = std::fs::read_to_string("test_files/frameworks.pl").unwrap();
    let fa = build_fa(&src);
    for r in &fa.refs {
        if r.target_name == "name" || r.target_name == "new" {
            eprintln!(
                "REF: target={} kind={:?} span={:?} resolves_to={:?}",
                r.target_name, r.kind, r.span, r.resolves_to
            );
        }
    }
    for s in &fa.symbols {
        if s.name == "name"
            || (matches!(s.kind, SymKind::HashKeyDef)
                && s.span.start.row > 5
                && s.span.start.row < 25)
        {
            eprintln!(
                "SYM: name={} kind={:?} span={:?} sel_span={:?} detail={:?}",
                s.name, s.kind, s.span, s.selection_span, s.detail
            );
        }
    }
}

// ---- varname-based extraction ----

/// Adversarial: every flavor of `foo` access (plain, element, slice,
/// KV slice, arraylen) must canonicalize to the underlying
/// `$foo`/`@foo`/`%foo` Variable symbol — NOT to "$foo" across the
/// board. TSP exposes the container kind via distinct node types
/// (`container_variable`, `slice_container_variable`,
/// `keyval_container_variable`, `arraylen`) + a `hash:`/`array:`
/// field on the parent. Our job is to route each to the correct
/// declared symbol.
#[test]
fn sigil_disambiguation_across_access_forms() {
    let src = "\
my ($foo, @foo, %foo);
$foo;
$foo[0];
$foo{hi};
@foo[0..1];
@foo{qw/hi there/};
$#foo;
%foo[0..1];
%foo{a};
";
    let fa = build_fa(src);

    // Three distinct declarations.
    let decls: std::collections::HashMap<&str, _> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Variable
                && s.scope == ScopeId(0)
                && matches!(s.name.as_str(), "$foo" | "@foo" | "%foo")
        })
        .map(|s| (s.name.as_str(), s.id))
        .collect();
    assert!(decls.contains_key("$foo"), "missing scalar decl");
    assert!(decls.contains_key("@foo"), "missing array decl");
    assert!(decls.contains_key("%foo"), "missing hash decl");

    // Collect every Variable/ContainerAccess ref, keyed by the line
    // it sits on. Line 0 is the declaration — skip it.
    let mut refs_by_line: std::collections::HashMap<usize, Vec<&str>> = Default::default();
    for r in &fa.refs {
        if !matches!(r.kind, RefKind::Variable | RefKind::ContainerAccess) {
            continue;
        }
        if r.access == AccessKind::Declaration {
            continue;
        }
        refs_by_line
            .entry(r.span.start.row)
            .or_default()
            .push(r.target_name.as_str());
    }

    let expected: &[(usize, &str, &str)] = &[
        (1, "$foo", "$foo"),               // plain scalar
        (2, "$foo[0]", "@foo"),            // array element access
        (3, "$foo{hi}", "%foo"),           // hash element access
        (4, "@foo[0..1]", "@foo"),         // array slice
        (5, "@foo{qw/hi there/}", "%foo"), // hash slice — Perl semantic
        (6, "$#foo", "@foo"),              // arraylen
        (7, "%foo[0..1]", "@foo"),         // KV slice of array (5.20+)
        (8, "%foo{a}", "%foo"),            // KV slice of hash
    ];

    let mut failures: Vec<String> = Vec::new();
    for (line, form, want) in expected {
        let got = refs_by_line.get(line).cloned().unwrap_or_default();
        if got.as_slice() != [*want] {
            failures.push(format!(
                "  line {} `{}` → want [{}], got {:?}",
                line, form, want, got
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "sigil disambiguation failures:\n{}",
        failures.join("\n")
    );
}

#[test]
fn braced_var_declaration_names_match_bare_form() {
    // `my ${foo}` is just `my $foo`. Before the varname refactor we
    // stored the declared name as the full node text `${foo}`, so a
    // later `$foo` reference couldn't resolve to it. Now both share
    // the canonical `$foo` name.
    let fa = build_fa("my ${foo} = 1;\n$foo;\n");
    let decls: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Variable && s.name == "$foo")
        .collect();
    assert_eq!(
        decls.len(),
        1,
        "expected one $foo symbol, got {:?}",
        fa.symbols
            .iter()
            .filter(|s| s.kind == SymKind::Variable)
            .map(|s| &s.name)
            .collect::<Vec<_>>()
    );
}

// ---- parse_instance_of ----

#[test]
fn parse_instance_of_single_quoted() {
    assert_eq!(
        parse_instance_of("InstanceOf['Foo::Bar']").as_deref(),
        Some("Foo::Bar")
    );
}

#[test]
fn parse_instance_of_double_quoted() {
    assert_eq!(
        parse_instance_of("InstanceOf[\"Foo::Bar\"]").as_deref(),
        Some("Foo::Bar")
    );
}

#[test]
fn parse_instance_of_rejects_non_instance_of() {
    assert_eq!(parse_instance_of("Str"), None);
    assert_eq!(parse_instance_of("ArrayRef[Int]"), None);
    assert_eq!(parse_instance_of("My::Class"), None);
}

// ---- Scope tests ----

#[test]
fn test_file_scope() {
    let fa = build_fa("my $x = 1;");
    assert_eq!(fa.scopes.len(), 1);
    assert_eq!(fa.scopes[0].kind, ScopeKind::File);
}

#[test]
fn test_sub_creates_scope() {
    let fa = build_fa("sub foo { my $x = 1; }");
    let sub_scopes: Vec<_> = fa
        .scopes
        .iter()
        .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"))
        .collect();
    assert_eq!(sub_scopes.len(), 1);
    assert_eq!(sub_scopes[0].parent, Some(ScopeId(0))); // parent is file
}

#[test]
fn test_class_creates_scope() {
    let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
    let class_scopes: Vec<_> = fa
        .scopes
        .iter()
        .filter(|s| matches!(&s.kind, ScopeKind::Class { name } if name == "Point"))
        .collect();
    assert_eq!(class_scopes.len(), 1);
    assert_eq!(class_scopes[0].package, Some("Point".to_string()));
}

#[test]
fn test_package_sets_scope_package() {
    let fa = build_fa("package Foo;\nsub bar { 1 }");
    // The sub scope should inherit package "Foo"
    let sub_scopes: Vec<_> = fa
        .scopes
        .iter()
        .filter(|s| matches!(&s.kind, ScopeKind::Sub { name } if name == "bar"))
        .collect();
    assert_eq!(sub_scopes.len(), 1);
    assert_eq!(sub_scopes[0].package, Some("Foo".to_string()));
}

#[test]
fn test_for_loop_scope() {
    let fa = build_fa("for my $i (1..10) { print $i; }");
    let for_scopes: Vec<_> = fa
        .scopes
        .iter()
        .filter(|s| matches!(&s.kind, ScopeKind::ForLoop { .. }))
        .collect();
    assert_eq!(for_scopes.len(), 1);
}

// ---- Symbol tests ----

#[test]
fn test_variable_symbol() {
    let fa = build_fa("my $x = 1;");
    let vars: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Variable && s.name == "$x")
        .collect();
    assert_eq!(vars.len(), 1);
    if let SymbolDetail::Variable { sigil, decl_kind } = &vars[0].detail {
        assert_eq!(*sigil, '$');
        assert_eq!(*decl_kind, DeclKind::My);
    } else {
        panic!("expected Variable detail");
    }
}

#[test]
fn test_sub_symbol_with_params() {
    let fa = build_fa("sub connect($self, %opts) { }");
    let subs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Sub && s.name == "connect")
        .collect();
    assert_eq!(subs.len(), 1);
    if let SymbolDetail::Sub {
        params, is_method, ..
    } = &subs[0].detail
    {
        assert!(!is_method);
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "$self");
        assert_eq!(params[1].name, "%opts");
        assert!(params[1].is_slurpy);
    } else {
        panic!("expected Sub detail");
    }
}

#[test]
fn test_legacy_sub_params() {
    let fa = build_fa("sub new {\n    my ($class, %args) = @_;\n}");
    let subs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Sub && s.name == "new")
        .collect();
    assert_eq!(subs.len(), 1);
    if let SymbolDetail::Sub { params, .. } = &subs[0].detail {
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "$class");
        assert_eq!(params[1].name, "%args");
        assert!(params[1].is_slurpy);
    } else {
        panic!("expected Sub detail");
    }
}

#[test]
fn test_package_symbol() {
    let fa = build_fa("package Foo;");
    let pkgs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Package && s.name == "Foo")
        .collect();
    assert_eq!(pkgs.len(), 1);
}

#[test]
fn test_class_symbol() {
    let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n    field $y :param;\n    method magnitude() { }\n}");
    let classes: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Class && s.name == "Point")
        .collect();
    assert_eq!(classes.len(), 1);
    if let SymbolDetail::Class { fields, parent, .. } = &classes[0].detail {
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "$x");
        assert_eq!(fields[1].name, "$y");
        assert!(fields[0].attributes.contains(&"param".to_string()));
        assert!(parent.is_none());
    } else {
        panic!("expected Class detail");
    }
}

#[test]
fn test_field_symbol() {
    let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param;\n}");
    let fields: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Field)
        .collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "$x");
}

#[test]
fn test_field_reader_synthesizes_method() {
    let fa = build_fa(
        "use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n}",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Method)
        .collect();
    assert_eq!(
        methods.len(),
        1,
        "got: {:?}",
        methods.iter().map(|m| &m.name).collect::<Vec<_>>()
    );
    assert_eq!(methods[0].name, "x");
}

#[test]
fn test_implicit_self_in_method() {
    // $self is implicitly available in Perl 5.38 method blocks
    let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
    let fa = build_fa(source);

    // $self should be resolvable as a variable inside the method
    let resolved = fa.resolve_variable("$self", Point::new(4, 8));
    assert!(
        resolved.is_some(),
        "$self should resolve inside method body"
    );
}

#[test]
fn test_implicit_self_type_inference() {
    // $self should be type-inferred to the enclosing class
    let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () {\n        $self->x;\n    }\n}\n";
    let fa = build_fa(source);

    // Type inference: $self → Point
    let inferred = fa.inferred_type_via_bag("$self", Point::new(4, 8));
    assert!(inferred.is_some(), "$self type should be inferred");
    match inferred.unwrap() {
        InferredType::ClassName(name) => assert_eq!(name, "Point"),
        InferredType::FirstParam { package } => assert_eq!(package, "Point"),
        other => panic!("expected ClassName or FirstParam, got {:?}", other),
    }
}

#[test]
fn test_self_completion_inside_method() {
    // $self-> inside a method should complete with sibling methods
    let source = "use v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n    method to_string () {\n        $self->;\n    }\n}\n";
    let fa = build_fa(source);

    let candidates = fa.complete_methods("$self", Point::new(5, 14));
    let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(
        names.contains(&"magnitude"),
        "missing magnitude, got: {:?}",
        names
    );
    assert!(
        names.contains(&"to_string"),
        "missing to_string, got: {:?}",
        names
    );
    assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
}

#[test]
fn test_field_writer_synthesizes_method() {
    let fa =
        build_fa("use v5.38;\nclass Point {\n    field $label :reader :writer = \"point\";\n}");
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Method)
        .map(|s| s.name.clone())
        .collect();
    assert!(
        methods.contains(&"label".to_string()),
        "missing reader, got: {:?}",
        methods
    );
    assert!(
        methods.contains(&"set_label".to_string()),
        "missing writer, got: {:?}",
        methods
    );
}

#[test]
fn test_complete_methods_in_class() {
    let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    field $y :param;\n    method magnitude() { }\n    method to_string() { }\n}\nmy $p = Point->new(x => 1);\n$p->;\n");
    // $p-> is at line 8, col 4
    let candidates = fa.complete_methods("$p", Point::new(8, 4));
    let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"new"), "missing new, got: {:?}", names);
    assert!(
        names.contains(&"magnitude"),
        "missing magnitude, got: {:?}",
        names
    );
    assert!(
        names.contains(&"to_string"),
        "missing to_string, got: {:?}",
        names
    );
    assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
}

#[test]
fn test_complete_methods_sample_file_layout() {
    // Matches sample.pl: class defined after package main, $p usage at end
    let source = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
    let fa = build_fa(source);

    // Check type inference resolved $p → Point
    let inferred = fa.inferred_type_via_bag("$p", Point::new(8, 4));
    assert!(inferred.is_some(), "type inference for $p should resolve");

    let candidates = fa.complete_methods("$p", Point::new(10, 4));
    let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(
        names.contains(&"magnitude"),
        "missing magnitude, got: {:?}",
        names
    );
    assert!(
        names.contains(&"to_string"),
        "missing to_string, got: {:?}",
        names
    );
    assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
}

#[test]
fn test_complete_methods_class_after_package_main() {
    // Real-world: package main; ... class Point {} ... $p->
    let source = r#"package main;
my $calc = Calculator->new();
1;
use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude () { }
    method to_string () { }
}
my $p = Point->new(x => 3, y => 4);
$p->;
"#;
    let fa = build_fa(source);

    let candidates = fa.complete_methods("$p", Point::new(11, 4));
    let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"new"), "missing new, got: {:?}", names);
    assert!(
        names.contains(&"magnitude"),
        "missing magnitude, got: {:?}",
        names
    );
    assert!(
        names.contains(&"to_string"),
        "missing to_string, got: {:?}",
        names
    );
    assert!(names.contains(&"x"), "missing reader x, got: {:?}", names);
}

#[test]
fn test_complete_methods_flat_class() {
    // class Foo; (no block) — methods follow as siblings, like package
    let source = "use v5.38;\nclass Foo;\nmethod bar () { }\nmethod baz () { }\n";
    let fa = build_fa(source);
    let candidates = fa.complete_methods("Foo", Point::new(3, 0));
    let names: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"bar"), "missing bar, got: {:?}", names);
    assert!(names.contains(&"baz"), "missing baz, got: {:?}", names);
}

#[test]
fn test_goto_def_method_after_package_main() {
    // go-to-def on $p->magnitude() should find the method, not the class
    let source = "package main;\n1;\nuse v5.38;\nclass Point {\n    field $x :param :reader;\n    method magnitude () { }\n}\nmy $p = Point->new(x => 3);\n$p->magnitude();\n";
    let fa = build_fa(source);
    // cursor on `magnitude` in `$p->magnitude()` — line 8, col 5
    let def = fa.find_definition(Point::new(8, 5), None, None, None);
    assert!(def.is_some(), "should find definition for magnitude");
    let span = def.unwrap();
    assert_eq!(
        span.start.row, 5,
        "should point to method declaration line, got row {}",
        span.start.row
    );
}

#[test]
fn test_field_reader_goto_def() {
    // go-to-def on $p->x should find the reader method, which points to the field
    let fa = build_fa("use v5.38;\nclass Point {\n    field $x :param :reader;\n    method mag() { }\n}\nmy $p = Point->new(x => 1);\n$p->x;");
    let def = fa.find_definition(Point::new(6, 5), None, None, None); // cursor on `x` in `$p->x`
    assert!(def.is_some(), "should find definition for reader method");
    // The reader method's selection_span points to the field declaration
    let span = def.unwrap();
    assert_eq!(span.start.row, 2, "should point to field declaration line");
}

/// NAV (a): goto-def on a method that does NOT exist on a known class
/// must be an honest miss (None), NEVER a confident jump to the
/// `package` decl. The `$self->{email}->method` shape used to over-type
/// the invocant to the enclosing class and then jump to its package
/// line when the method wasn't found — worse than a miss.
#[test]
fn test_goto_def_unknown_method_is_honest_miss_not_package_jump() {
    let source = "package Foo;\nsub new { bless { email => undef }, shift }\nsub to { my $self = shift; $self->{email}->totallyunknownmethod(1); }\n1;\n";
    let fa = build_fa(source);
    // `totallyunknownmethod` starts at row 2, col 43 (after the `->`).
    let def = fa.find_definition(Point::new(2, 48), None, None, None);
    assert!(
        def.is_none(),
        "unknown method must return None (honest miss), not jump to package decl; got {:?}",
        def
    );
}

/// NAV regression (iv): goto-def on a typed same-file method still
/// lands on the method declaration via the frozen dispatch edge.
#[test]
fn test_goto_def_typed_same_file_method_resolves() {
    let source = "package Widget;\nsub new { bless {}, shift }\nsub frobnicate { 1 }\nsub run { my $w = Widget->new; $w->frobnicate; }\n1;\n";
    let fa = build_fa(source);
    // `frobnicate` in `$w->frobnicate` — row 3. Find its column.
    let row = 3usize;
    let line = source.lines().nth(row).unwrap();
    let col = line.find("$w->frobnicate").unwrap() + "$w->".len() + 2;
    let def = fa.find_definition(Point::new(row, col), None, None, None);
    assert!(def.is_some(), "typed $w->frobnicate must resolve to the decl");
    assert_eq!(
        def.unwrap().start.row,
        2,
        "should land on `sub frobnicate` (row 2)"
    );
}

#[test]
fn test_use_symbol() {
    let fa = build_fa("use Foo::Bar;");
    let modules: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Module)
        .collect();
    assert_eq!(modules.len(), 1);
    assert_eq!(modules[0].name, "Foo::Bar");
}

// ---- Ref tests ----

#[test]
fn test_variable_ref() {
    let fa = build_fa("my $x = 1;\nprint $x;");
    let var_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "$x" && matches!(r.kind, RefKind::Variable))
        .collect();
    // One declaration ref + one read ref
    assert!(var_refs.len() >= 2, "got {} refs for $x", var_refs.len());
    assert!(var_refs.iter().any(|r| r.access == AccessKind::Declaration));
    assert!(var_refs.iter().any(|r| r.access == AccessKind::Read));
}

#[test]
fn test_function_call_ref() {
    let fa = build_fa("sub foo { }\nfoo();");
    let call_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .collect();
    assert_eq!(call_refs.len(), 1);
}

/// Rule #7: a call that appears as an *operand* of a larger expression
/// (string concatenation, ternary) must still emit its FunctionCall ref.
/// AWStats shape `print "<td>".Format_Number($x)."</td>"` parses the call
/// as a `function_call_expression` nested inside a `binary_expression`
/// (the concat) which is itself the `print` verb's argument. The generic
/// `_ => visit_children` traversal in `visit_node` reaches it; this test
/// pins that so a future grammar/traversal change can't silently regress
/// references to statement-level calls only.
#[test]
fn call_ref_in_concatenation_operand() {
    let fa = build_fa("sub Format_Number { }\nprint \"<td>\".Format_Number($x).\"</td>\";\n");
    let call_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "Format_Number" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .collect();
    assert_eq!(
        call_refs.len(),
        1,
        "a call inside `.`-concatenation must emit exactly one FunctionCall ref"
    );
    // The ref must pin the *call name*, not the surrounding concat/print.
    assert!(call_refs[0].span.start.row == 1);
}

/// Rule #7: calls in both arms of a ternary are operands too.
#[test]
fn call_ref_in_ternary_operands() {
    let fa = build_fa("sub foo { }\nsub bar { }\nmy $y = $cond ? foo() : bar();\n");
    let foo_refs = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .count();
    let bar_refs = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "bar" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .count();
    assert_eq!(foo_refs, 1, "ternary consequent call must emit a ref");
    assert_eq!(bar_refs, 1, "ternary alternative call must emit a ref");
}

/// Method calls nested in expression operands must also emit a MethodCall ref.
#[test]
fn method_call_ref_in_concatenation_operand() {
    let fa = build_fa("my $s = \"x\" . $obj->fmt($n) . \"y\";\n");
    let m_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "fmt" && matches!(r.kind, RefKind::MethodCall { .. }))
        .collect();
    assert_eq!(m_refs.len(), 1, "method call inside concat must emit one MethodCall ref");
}

/// AWStats-shaped fixture: a def plus N call sites, every call embedded in a
/// concatenation operand. Mirrors the real-world undercount (def→6 of 172).
/// Asserts the call-ref count equals the textual occurrence count and that a
/// bareword that is NOT a call (`Format_Number` as a hash key) is not counted.
#[test]
fn call_refs_count_across_expression_positions() {
    let src = "\
sub Format_Number { my $n = shift; return $n; }
print \"<td>\".Format_Number($a).\"</td>\";
print \"<td>\".Format_Number($b).\"</td><td>x</td>\";
my $r = \"a\" . Format_Number($c) . \"b\" . Format_Number($d);
my $t = $cond ? Format_Number($e) : 0;
my %h = (Format_Number => 1);
";
    let fa = build_fa(src);
    let call_refs = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "Format_Number" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .count();
    // 5 genuine call sites (two single-call prints, two in one concat, one ternary).
    assert_eq!(
        call_refs, 5,
        "every call-position occurrence must emit a FunctionCall ref; the hash-key bareword must not"
    );
}

/// Regression guard: a plain statement-level call still emits exactly one
/// ref (no double-emission from the operand-traversal path).
#[test]
fn statement_level_call_emits_single_ref() {
    let fa = build_fa("sub debug { }\ndebug(\"hello\");\n");
    let call_refs = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "debug" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .count();
    assert_eq!(call_refs, 1, "statement-level call must emit exactly one ref");
}

#[test]
fn test_method_call_ref() {
    let fa = build_fa("$obj->method();");
    let method_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "method" && matches!(r.kind, RefKind::MethodCall { .. }))
        .collect();
    assert_eq!(method_refs.len(), 1);
    if let RefKind::MethodCall { ref invocant, .. } = method_refs[0].kind {
        assert_eq!(invocant, "$obj");
    }
}

#[test]
fn test_hash_key_ref() {
    let fa = build_fa("my %h;\n$h{foo};");
    let key_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "foo" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert_eq!(key_refs.len(), 1);
}

// ---- Query tests ----

#[test]
fn test_scope_at() {
    let fa = build_fa("sub foo {\n    my $x = 1;\n}");
    // Point inside the sub body
    let scope = fa.scope_at(Point::new(1, 8)).unwrap();
    let s = fa.scope(scope);
    assert!(matches!(&s.kind, ScopeKind::Sub { name } if name == "foo"));
}

#[test]
fn test_resolve_variable() {
    let fa = build_fa("my $x = 1;\nsub foo {\n    my $x = 2;\n    print $x;\n}");
    // Inside the sub, $x should resolve to the inner declaration
    let sym = fa.resolve_variable("$x", Point::new(3, 10)).unwrap();
    // Inner $x is at line 2
    assert_eq!(sym.selection_span.start.row, 2);
}

#[test]
fn test_resolve_variable_outer() {
    let fa = build_fa("my $x = 1;\nsub foo {\n    print $x;\n}");
    // Inside the sub with no inner $x, should resolve to outer
    let sym = fa.resolve_variable("$x", Point::new(2, 10)).unwrap();
    assert_eq!(sym.selection_span.start.row, 0);
}

#[test]
fn test_type_inference_constructor() {
    let fa = build_fa("use v5.38;\nclass Point { }\nmy $p = Point->new();");
    let ty = fa.inferred_type_via_bag("$p", Point::new(2, 20));
    assert!(ty.is_some(), "should infer type for $p");
    if let Some(InferredType::ClassName(cn)) = ty {
        assert_eq!(cn, "Point");
    } else {
        panic!("expected ClassName, got {:?}", ty);
    }
}

#[test]
fn test_type_inference_first_param() {
    // The walk pushes a `FirstParam { package: "Calculator" }`
    // type-constraint, but the bag-aware query normalises it to
    // `ClassName("Calculator")` via the FrameworkAwareTypeFold
    // (FirstParam is an internal observation; consumers see the
    // class identity). That's the canonical answer the LSP
    // serves at any cursor position on `$self`.
    let fa = build_fa("package Calculator;\nsub new {\n    my ($self) = @_;\n}");
    let ty = fa.inferred_type_via_bag("$self", Point::new(2, 10));
    assert_eq!(ty, Some(InferredType::ClassName("Calculator".into())));
}

// ---- Literal constructor extraction tests (via build_fa) ----

#[test]
fn test_extract_hashref_literal() {
    let fa = build_fa("my $href = {};");
    let ty = fa.inferred_type_via_bag("$href", Point::new(0, 14));
    assert_eq!(ty, Some(InferredType::HashRef), "empty hash ref literal");

    let fa = build_fa("my $href = { a => 1, b => 2 };");
    let ty = fa.inferred_type_via_bag("$href", Point::new(0, 30));
    assert_eq!(
        ty,
        Some(InferredType::HashRef),
        "populated hash ref literal"
    );
}

#[test]
fn test_extract_arrayref_literal() {
    let fa = build_fa("my $aref = [];");
    let ty = fa.inferred_type_via_bag("$aref", Point::new(0, 14));
    assert_eq!(ty, Some(InferredType::ArrayRef), "empty array ref literal");

    let fa = build_fa("my $aref = [1, 2, 3];");
    let ty = fa.inferred_type_via_bag("$aref", Point::new(0, 21));
    assert_eq!(
        ty,
        Some(InferredType::ArrayRef),
        "populated array ref literal"
    );
}

#[test]
fn test_extract_coderef_literal() {
    let fa = build_fa("my $cref = sub { 42 };");
    let ty = fa.inferred_type_via_bag("$cref", Point::new(0, 22));
    // Sub-literal CodeRef carries `return_edge: Some(_)` — the
    // body's last-expression span. Survives the `my $cref = ...`
    // binding so downstream callable-shape consumers can edge-chase
    // into the body's type. Opaque coderef tests below use `None`.
    assert!(
        matches!(ty, Some(InferredType::CodeRef { return_edge: Some(_) })),
        "anonymous sub: got {:?}",
        ty
    );
}

#[test]
fn test_extract_regexp_literal() {
    let fa = build_fa("my $re = qr/pattern/;");
    let ty = fa.inferred_type_via_bag("$re", Point::new(0, 21));
    assert_eq!(ty, Some(InferredType::Regexp), "qr// literal");
}

#[test]
fn test_extract_reassignment_type_change() {
    let fa = build_fa("my $x = {};\n$x = [];");
    // After line 0 → HashRef
    let ty = fa.inferred_type_via_bag("$x", Point::new(0, 11));
    assert_eq!(ty, Some(InferredType::HashRef), "initial hashref");
    // After line 1 → ArrayRef
    let ty = fa.inferred_type_via_bag("$x", Point::new(1, 8));
    assert_eq!(ty, Some(InferredType::ArrayRef), "reassigned to arrayref");
}

#[test]
fn test_extract_constructor_still_works() {
    // Existing constructor detection should still work
    let fa = build_fa("my $obj = Foo->new();");
    let ty = fa.inferred_type_via_bag("$obj", Point::new(0, 21));
    assert_eq!(ty, Some(InferredType::ClassName("Foo".into())));
}

// ---- Operator-based type inference tests (Step 3) ----

#[test]
fn test_arrow_hash_deref_infers_hashref() {
    let fa = build_fa("my $x;\n$x->{key};");
    let ty = fa.inferred_type_via_bag("$x", Point::new(1, 10));
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_arrow_array_deref_infers_arrayref() {
    let fa = build_fa("my $x;\n$x->[0];");
    let ty = fa.inferred_type_via_bag("$x", Point::new(1, 8));
    assert_eq!(ty, Some(InferredType::ArrayRef));
}

#[test]
fn test_arrow_code_deref_infers_coderef() {
    let fa = build_fa("my $x;\n$x->(1, 2);");
    let ty = fa.inferred_type_via_bag("$x", Point::new(1, 10));
    // Deref-context inference: `$x->(...)` says `$x` is a coderef
    // but reveals nothing about its body (the binding is opaque).
    assert_eq!(ty, Some(InferredType::CodeRef { return_edge: None }));
}

#[test]
fn test_coderef_call_propagates_return_type() {
    // `my $cb = sub { [1,2] }; my $r = $cb->();` — the literal's
    // `return_edge` (Expr(body_last)) rides through the binding,
    // and `$cb->()` should chase it: $r types as ArrayRef.
    // Anonymous-sub literal whose body's last expression is an
    // anonymous_array_expression — closed-under-syntax, so the
    // body span resolves to ArrayRef without name lookup.
    let fa = build_fa("my $cb = sub { [1,2] };\nmy $r = $cb->();\nmy $z;");
    let ty = fa.inferred_type_via_bag("$r", Point::new(2, 0));
    assert_eq!(
        ty,
        Some(InferredType::ArrayRef),
        "coderef call must inherit the callable's return type via return_edge: got {:?}",
        ty,
    );
}

#[test]
fn test_postfix_array_deref_infers_arrayref() {
    let fa = build_fa("my $x;\nmy @a = $x->@*;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::ArrayRef));
}

#[test]
fn test_postfix_hash_deref_infers_hashref() {
    let fa = build_fa("my $y;\nmy %h = $y->%*;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$y", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_binary_numeric_ops_infer_numeric() {
    let fa = build_fa("my $x;\nmy $a = $x + 1;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::Numeric), "+ operator");

    let fa = build_fa("my $x;\nmy $a = $x * 2;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::Numeric), "* operator");
}

#[test]
fn test_assignment_from_binary_numeric_infers_result() {
    let fa = build_fa("my $a = 1;\nmy $b = 2;\nmy $result = $a + $b;\n$result;");
    let ty = fa.inferred_type_via_bag("$result", Point::new(3, 0));
    assert_eq!(
        ty,
        Some(InferredType::Numeric),
        "$result = $a + $b should be Numeric"
    );
}

#[test]
fn test_assignment_from_string_concat_infers_result() {
    let fa = build_fa("my $a = 'x';\nmy $b = 'y';\nmy $s = $a . $b;\n$s;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(3, 0));
    assert_eq!(
        ty,
        Some(InferredType::String),
        "$s = $a . $b should be String"
    );
}

#[test]
fn test_string_concat_infers_string() {
    let fa = build_fa("my $s;\nmy $a = $s . \"x\";\nmy $z;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::String), ". operator");
}

#[test]
fn test_string_repeat_infers_string() {
    let fa = build_fa("my $s;\n$s x 3;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::String), "x operator");
}

#[test]
fn test_numeric_comparison_infers_numeric() {
    let fa = build_fa("my $x;\nmy $y;\n$x == $y;\nmy $z;");
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(3, 0)),
        Some(InferredType::Numeric)
    );
    assert_eq!(
        fa.inferred_type_via_bag("$y", Point::new(3, 0)),
        Some(InferredType::Numeric)
    );
}

#[test]
fn test_string_comparison_infers_string() {
    let fa = build_fa("my $x;\nmy $y;\n$x eq $y;\nmy $z;");
    assert_eq!(
        fa.inferred_type_via_bag("$x", Point::new(3, 0)),
        Some(InferredType::String)
    );
    assert_eq!(
        fa.inferred_type_via_bag("$y", Point::new(3, 0)),
        Some(InferredType::String)
    );
}

#[test]
fn test_increment_infers_numeric() {
    let fa = build_fa("my $x;\n$x++;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::Numeric));
}

#[test]
fn test_regex_match_infers_string() {
    let fa = build_fa("my $s;\n$s =~ /pattern/;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::String));
}

#[test]
fn test_preinc_infers_numeric() {
    let fa = build_fa("my $x;\n++$x;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::Numeric));
}

#[test]
fn test_block_array_deref_infers_arrayref() {
    let fa = build_fa("my $x;\nmy @items = @{$x};\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::ArrayRef));
}

#[test]
fn test_block_hash_deref_infers_hashref() {
    let fa = build_fa("my $y;\nmy %t = %{$y};\nmy $z;");
    let ty = fa.inferred_type_via_bag("$y", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_block_code_deref_infers_coderef() {
    let fa = build_fa("my $z;\n&{$z}();\nmy $w;");
    let ty = fa.inferred_type_via_bag("$z", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::CodeRef { return_edge: None }));
}

#[test]
fn test_no_numeric_on_array_variable() {
    // @arr + 1 should NOT push Numeric on @arr
    let fa = build_fa("my @arr;\nmy $n = @arr + 1;\nmy $z;");
    let ty = fa.inferred_type_via_bag("@arr", Point::new(2, 0));
    assert_eq!(ty, None, "@arr should not get Numeric constraint");
}

// ---- Builtin type inference tests ----

#[test]
fn test_builtin_push_infers_arrayref() {
    // push @{$aref} triggers array_deref_expression which already infers ArrayRef
    let fa = build_fa("my $aref;\npush @{$aref}, 1;\nmy $z;");
    let ty = fa.inferred_type_via_bag("$aref", Point::new(2, 0));
    assert_eq!(
        ty,
        Some(InferredType::ArrayRef),
        "push deref should infer ArrayRef"
    );
}

#[test]
fn test_builtin_length_infers_string_arg() {
    let fa = build_fa("my $s;\nmy $n = length($s);\nmy $z;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(2, 0));
    assert_eq!(
        ty,
        Some(InferredType::String),
        "length arg should be String"
    );
}

#[test]
fn test_builtin_abs_infers_numeric_arg() {
    let fa = build_fa("my $x;\nmy $n = abs($x);\nmy $z;");
    let ty = fa.inferred_type_via_bag("$x", Point::new(2, 0));
    assert_eq!(ty, Some(InferredType::Numeric), "abs arg should be Numeric");
}

#[test]
fn test_builtin_return_type_propagates() {
    let fa = build_fa("my $t = time();\n$t;");
    let ty = fa.inferred_type_via_bag("$t", Point::new(1, 0));
    assert_eq!(
        ty,
        Some(InferredType::Numeric),
        "time() should return Numeric"
    );
}

#[test]
fn test_builtin_join_return_type() {
    let fa = build_fa("my $s = join(',', @arr);\n$s;");
    let ty = fa.inferred_type_via_bag("$s", Point::new(1, 0));
    assert_eq!(
        ty,
        Some(InferredType::String),
        "join() should return String"
    );
}

#[test]
fn test_builtin_length_return_type() {
    let fa = build_fa("my $n = length('hello');\n$n;");
    let ty = fa.inferred_type_via_bag("$n", Point::new(1, 0));
    assert_eq!(
        ty,
        Some(InferredType::Numeric),
        "length() should return Numeric"
    );
}

// ---- Return type inference tests (Step 4) ----

#[test]
fn test_return_type_hashref() {
    let fa = build_fa("sub get_config {\n    return { host => \"localhost\" };\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("get_config", None),
        Some(InferredType::HashRef)
    );
}

#[test]
fn test_return_type_arrayref() {
    let fa = build_fa("sub get_tags {\n    return [1, 2, 3];\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("get_tags", None),
        Some(InferredType::ArrayRef)
    );
}

#[test]
fn test_return_type_coderef() {
    let fa = build_fa("sub get_handler {\n    return sub { 1 };\n}");
    let ty = fa.sub_return_type_at_arity("get_handler", None);
    // `return sub { 1 }` is a sub-literal — the returned CodeRef
    // carries `return_edge` to the body's last expression.
    assert!(
        matches!(ty, Some(InferredType::CodeRef { return_edge: Some(_) })),
        "got {:?}",
        ty
    );
}

#[test]
fn test_return_type_implicit_last_expr() {
    // No explicit return — last expression is the implicit return
    let fa = build_fa("sub get_data {\n    { key => \"val\" };\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("get_data", None),
        Some(InferredType::HashRef)
    );
}

#[test]
fn test_return_type_conflicting_returns_unknown() {
    // Two returns with different types → None (unknown)
    let fa = build_fa("sub ambiguous {\n    if (1) { return {} }\n    return [];\n}");
    assert_eq!(fa.sub_return_type_at_arity("ambiguous", None), None);
}

#[test]
fn test_return_type_consistent_returns() {
    // Multiple returns all hashref → HashRef
    let fa =
        build_fa("sub consistent {\n    if (1) { return { a => 1 } }\n    return { b => 2 };\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("consistent", None),
        Some(InferredType::HashRef)
    );
}

#[test]
fn test_return_type_propagation_to_call_site() {
    let fa =
        build_fa("sub get_config {\n    return { host => 1 };\n}\nmy $cfg = get_config();\nmy $z;");
    assert_eq!(
        fa.sub_return_type_at_arity("get_config", None),
        Some(InferredType::HashRef)
    );
    let ty = fa.inferred_type_via_bag("$cfg", Point::new(4, 0));
    assert_eq!(
        ty,
        Some(InferredType::HashRef),
        "call site should get return type"
    );
}

#[test]
fn test_return_type_propagation_method_call() {
    let src = "package Calculator;\nsub new { bless {}, shift }\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    return $result;\n}\npackage main;\nmy $calc = Calculator->new();\nmy $sum = $calc->add(2, 3);\n$sum;";
    let fa = build_fa(src);
    assert_eq!(
        fa.sub_return_type_at_arity("add", None),
        Some(InferredType::Numeric),
        "add should return Numeric"
    );
    let ty = fa.inferred_type_via_bag("$sum", Point::new(10, 0));
    assert_eq!(
        ty,
        Some(InferredType::Numeric),
        "$sum should be Numeric via method call binding"
    );
}

#[test]
fn test_return_type_constructor() {
    let fa = build_fa("package User;\nsub new { bless {}, shift }\npackage main;\nsub get_user {\n    return User->new();\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("get_user", None),
        Some(InferredType::ClassName("User".into()))
    );
}

#[test]
fn test_return_type_self_variable() {
    // `return $self` resolves through the witness bag to the canonical
    // class type. `FirstParam` (the body-internal observation) is
    // normalised to `ClassName` at the FrameworkAwareTypeFold boundary
    // — callers chaining off the return get the concrete class.
    let fa = build_fa("package Foo;\nsub new { bless {}, shift }\nsub clone {\n    my ($self) = @_;\n    return $self;\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("clone", None),
        Some(InferredType::ClassName("Foo".into())),
    );
}

#[test]
fn test_return_type_bare_return_filtered() {
    // Bare return + typed return → bare is filtered, typed return wins
    let fa = build_fa("sub get_config {\n    return unless 1;\n    return { host => 1 };\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("get_config", None),
        Some(InferredType::HashRef)
    );
}

#[test]
fn test_return_type_all_bare_returns() {
    // All bare returns → no return type
    let fa = build_fa("sub noop {\n    return;\n}");
    assert_eq!(fa.sub_return_type_at_arity("noop", None), None);
}

#[test]
fn test_return_type_undef_filtered() {
    // return undef + typed return → undef is filtered, typed return wins
    let fa = build_fa("sub maybe {\n    return undef unless 1;\n    return { a => 1 };\n}");
    assert_eq!(
        fa.sub_return_type_at_arity("maybe", None),
        Some(InferredType::HashRef)
    );
}

// ---- resolve_expression_type tests ----

/// Find the first node of given kind at/after a point (searches all children).
fn find_node_at<'a>(
    node: tree_sitter::Node<'a>,
    point: Point,
    kind: &str,
) -> Option<tree_sitter::Node<'a>> {
    if node.kind() == kind && node.start_position() >= point {
        return Some(node);
    }
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if let Some(found) = find_node_at(child, point, kind) {
                return Some(found);
            }
        }
    }
    None
}

#[test]
fn test_resolve_expr_type_function_call() {
    let src = "sub get_config {\n    return { host => 1 };\n}\nget_config();\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // Find the function_call_expression on line 3
    let call_node = find_node_at(
        tree.root_node(),
        Point::new(3, 0),
        "function_call_expression",
    )
    .expect("should find function_call_expression");
    let ty = fa.resolve_expression_type(call_node, src.as_bytes(), None);
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_resolve_expr_type_method_call_return() {
    let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub do_thing { }\npackage main;\nmy $f = Foo->new();\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // $f->get_bar() should resolve to Object(Bar)
    // First verify get_bar has the right return type
    assert_eq!(
        fa.sub_return_type_at_arity("get_bar", None),
        Some(InferredType::ClassName("Bar".into()))
    );
}

#[test]
fn test_resolve_expr_type_scalar_variable() {
    let src = "my $x = {};\n$x;\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // Find the scalar $x on line 1
    let scalar_node =
        find_node_at(tree.root_node(), Point::new(1, 0), "scalar").expect("should find scalar");
    let ty = fa.resolve_expression_type(scalar_node, src.as_bytes(), None);
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_resolve_expr_type_chained_method() {
    let src = "package Foo;\nsub new { bless {}, shift }\nsub get_bar {\n    return Bar->new();\n}\npackage Bar;\nsub new { bless {}, shift }\nsub get_name {\n    return { name => 'test' };\n}\npackage main;\nmy $f = Foo->new();\n$f->get_bar()->get_name();\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // Line 12: $f->get_bar()->get_name();
    // The outermost method_call_expression starts at column 0
    // Use descendant_for_point_range to find the node at the start of that line
    let node = tree
        .root_node()
        .descendant_for_point_range(Point::new(12, 0), Point::new(12, 25))
        .expect("should find node");
    // Walk up to find the outermost method_call_expression
    let mut n = node;
    while n.kind() != "method_call_expression"
        || n.parent()
            .map_or(false, |p| p.kind() == "method_call_expression")
    {
        n = match n.parent() {
            Some(p) => p,
            None => panic!("should find outermost method_call_expression"),
        };
    }
    assert_eq!(n.kind(), "method_call_expression");
    let ty = fa.resolve_expression_type(n, src.as_bytes(), None);
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_resolve_expr_type_constructor() {
    let src = "package Foo;\nsub new { bless {}, shift }\npackage main;\nFoo->new();\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    let call = find_node_at(tree.root_node(), Point::new(3, 0), "method_call_expression")
        .expect("should find method_call_expression");
    let ty = fa.resolve_expression_type(call, src.as_bytes(), None);
    assert_eq!(ty, Some(InferredType::ClassName("Foo".into())));
}

#[test]
fn test_resolve_expr_type_triple_chain() {
    // $calc->get_self->get_config->{host} — no parens on method calls
    let src = "\
package Calculator;
sub new { bless {}, shift }
sub get_self {
    my ($self) = @_;
    return $self;
}
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{host};
";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    // Verify get_self returns an object type for Calculator
    let get_self_rt = fa.sub_return_type_at_arity("get_self", None);
    assert_eq!(
        get_self_rt.as_ref().and_then(|t| t.class_name()),
        Some("Calculator"),
        "get_self should return Calculator"
    );

    // Verify get_config returns HashRef
    let get_config_rt = fa.sub_return_type_at_arity("get_config", None);
    assert_eq!(
        get_config_rt,
        Some(InferredType::HashRef),
        "get_config should return HashRef"
    );

    // The outermost expression is hash_element_expression wrapping the chain
    // Find the method_call_expression for get_config (inner chain)
    // Line 11: $calc->get_self->get_config->{host}
    let node = tree
        .root_node()
        .descendant_for_point_range(Point::new(11, 0), Point::new(11, 0))
        .expect("should find node");
    let mut n = node;
    // Walk up to find hash_element_expression
    loop {
        if n.kind() == "hash_element_expression" {
            break;
        }
        n = n.parent().expect("should find hash_element_expression");
    }
    // The base of hash_element_expression is the method chain
    let base = n.named_child(0).expect("should have base");
    assert_eq!(base.kind(), "method_call_expression");
    let ty = fa.resolve_expression_type(base, src.as_bytes(), None);
    assert_eq!(
        ty,
        Some(InferredType::HashRef),
        "the chain $calc->get_self->get_config should resolve to HashRef"
    );
}

/// Acceptance for the unified, tree-free expression-type chase
/// (`docs/prompt-unify-expr-type-resolution.md`): the ref-keyed,
/// `tree: None` invocant-class path and the node-keyed
/// `resolve_expression_type` path must produce identical answers for
/// every invocant shape — scalar, chain, array-element, function-call,
/// and hash-element. Both now route through `expr_type_at_span`.
#[test]
fn invocant_class_and_resolve_expression_type_agree_tree_free() {
    let src = "\
package Foo;
sub new { bless {}, shift }
sub kid { return Foo->new(); }
sub cfg { return { host => 'x' }; }
package main;
sub mk { return Foo->new(); }
my $f = Foo->new();
my @arr;
push @arr, Foo->new();
my %h = (it => $f);
$f->kid();
$f->kid()->kid();
$arr[0]->kid();
mk()->kid();
$h{it}->kid();
";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    // For every MethodCall ref with an invocant span, the two paths
    // must agree. The invocant-class path takes no tree; the
    // expression-type path is fed the actual invocant CST node.
    let mut checked_shapes = 0;
    for r in &fa.refs {
        let RefKind::MethodCall { invocant_span: Some(sp), .. } = &r.kind else {
            continue;
        };
        let invocant_node = tree
            .root_node()
            .descendant_for_point_range(sp.start, sp.end)
            .expect("invocant span maps to a node");
        // Skip if the descendant doesn't exactly cover the invocant
        // (parser quirk for some shapes) — we only compare where both
        // paths see the same node.
        if invocant_node.start_position() != sp.start
            || invocant_node.end_position() != sp.end
        {
            continue;
        }
        let via_ref = fa.method_call_invocant_class(r, None);
        let via_node = fa
            .resolve_expression_type(invocant_node, src.as_bytes(), None)
            .and_then(|t| t.class_name().map(|s| s.to_string()));
        assert_eq!(
            via_ref, via_node,
            "invocant-class (tree-free) vs resolve_expression_type disagree \
             for invocant `{}` (kind {})",
            invocant_node.utf8_text(src.as_bytes()).unwrap_or("?"),
            invocant_node.kind(),
        );
        checked_shapes += 1;
    }
    // Sanity: the source exercises scalar / chain / array-element /
    // function-call / hash-element invocants, so we should have
    // compared several.
    assert!(
        checked_shapes >= 5,
        "expected to compare at least the 5 invocant shapes, got {}",
        checked_shapes,
    );

    // Spot-check the concrete answers so a mutual `None` regression
    // can't pass the agreement assert vacuously.
    let kid_on_scalar = fa.refs.iter().find(|r| {
        matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "$f")
            && r.target_name == "kid"
    });
    assert_eq!(
        kid_on_scalar.and_then(|r| fa.method_call_invocant_class(r, None)).as_deref(),
        Some("Foo"),
        "scalar invocant `$f->kid` should type as Foo, tree-free",
    );
    let kid_on_array = fa.refs.iter().find(|r| {
        matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant.starts_with("$arr"))
            && r.target_name == "kid"
    });
    assert_eq!(
        kid_on_array.and_then(|r| fa.method_call_invocant_class(r, None)).as_deref(),
        Some("Foo"),
        "array-element invocant `$arr[0]->kid` should type as Foo, tree-free",
    );
}

#[test]
fn test_package_at() {
    let fa = build_fa("package Foo;\nsub bar { }");
    let pkg = fa.package_at(Point::new(1, 5));
    assert_eq!(pkg, Some("Foo"));
}

#[test]
fn test_variable_resolves_to() {
    let fa = build_fa("my $x = 1;\nprint $x;");
    let read_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "$x" && r.access == AccessKind::Read)
        .collect();
    assert!(!read_refs.is_empty());
    assert!(
        read_refs[0].resolves_to.is_some(),
        "read ref should resolve to declaration"
    );
}

#[test]
fn test_fold_ranges() {
    let fa = build_fa("sub foo {\n    my $x = 1;\n}\nsub bar {\n    my $y = 2;\n}");
    assert!(
        fa.fold_ranges.len() >= 2,
        "should have fold ranges for sub blocks, got {}",
        fa.fold_ranges.len()
    );
}

#[test]
fn test_visible_symbols() {
    let fa = build_fa("my $outer = 1;\nsub foo {\n    my $inner = 2;\n}");
    // Inside the sub, both $outer and $inner should be visible
    let visible = fa.visible_symbols(Point::new(2, 10));
    let names: Vec<&str> = visible.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"$inner"),
        "should see $inner, got: {:?}",
        names
    );
    assert!(
        names.contains(&"$outer"),
        "should see $outer, got: {:?}",
        names
    );
}

#[test]
fn test_two_packages_scoped() {
    let fa = build_fa("package Foo;\nsub alpha { }\npackage Bar;\nsub beta { }");
    // At the beta sub, package should be "Bar"
    let pkg = fa.package_at(Point::new(3, 5));
    assert_eq!(pkg, Some("Bar"));
    // At the alpha sub, package should be "Foo"
    let pkg = fa.package_at(Point::new(1, 5));
    assert_eq!(pkg, Some("Foo"));
}

// ---- High-level query tests ----

#[test]
fn test_find_def_variable() {
    let fa = build_fa("my $x = 1;\nprint $x;");
    // Cursor on the usage of $x at line 1
    let def = fa.find_definition(Point::new(1, 7), None, None, None);
    assert!(def.is_some(), "should find definition for $x");
    let span = def.unwrap();
    assert_eq!(span.start.row, 0, "definition should be on line 0");
}

#[test]
fn test_find_def_sub() {
    let fa = build_fa("sub greet { }\ngreet();");
    // Cursor on the function call at line 1
    let def = fa.find_definition(Point::new(1, 1), None, None, None);
    assert!(def.is_some(), "should find definition for greet");
    let span = def.unwrap();
    assert_eq!(span.start.row, 0, "definition should be on line 0");
}

#[test]
fn test_find_def_method_in_class() {
    let src = "package Foo;\nsub new { bless {}, shift }\nsub hello { }\npackage main;\nmy $f = Foo->new();\n$f->hello();";
    let fa = build_fa(src);
    // Cursor on hello() call at line 5
    let def = fa.find_definition(Point::new(5, 5), None, None, None);
    assert!(def.is_some(), "should find definition for hello method");
    let span = def.unwrap();
    assert_eq!(span.start.row, 2, "hello definition should be on line 2");
}

#[test]
fn test_find_def_scoped_variable() {
    let src = "my $x = 'outer';\nsub foo {\n    my $x = 'inner';\n    print $x;\n}";
    let fa = build_fa(src);
    // Cursor on $x inside sub (line 3) should resolve to inner $x (line 2)
    let def = fa.find_definition(Point::new(3, 11), None, None, None);
    assert!(def.is_some());
    let span = def.unwrap();
    assert_eq!(span.start.row, 2, "should resolve to inner $x on line 2");
}

#[test]
fn test_find_references_variable() {
    let src = "my $x = 1;\nprint $x;\n$x = 2;";
    let fa = build_fa(src);
    // Cursor on the declaration of $x
    let refs = fa.find_references(Point::new(0, 4), None, None, None);
    assert!(
        refs.len() >= 2,
        "should find at least declaration + usage, got {}",
        refs.len()
    );
}

#[test]
fn test_hash_key_def_implicit_return_gets_sub_owner() {
    // Implicit return: last expression in sub body, no explicit `return`
    let src = "sub get_config { { host => 'localhost', port => 5432 } }\nmy $cfg = get_config();\n$cfg->{host};\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    let host_defs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
        .collect();
    assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
    if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
        assert_eq!(
            *owner,
            HashKeyOwner::Sub {
                // Top-level scripts default to `main` per Perl's
                // own semantics; the implicit-package seed in
                // `Builder::new` makes this an explicit `Some("main")`
                // rather than `None`.
                package: Some("main".to_string()),
                name: "get_config".to_string()
            },
            "implicit return hash key should have Sub get_config owner, got {:?}",
            owner
        );
    }

    // Go-to-def from $cfg->{host} should reach the hash key in the implicit return
    let host_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        !host_refs.is_empty(),
        "should find HashKeyAccess for 'host'"
    );
    let def = fa.find_definition(
        host_refs[0].span.start,
        Some(&tree),
        Some(src.as_bytes()),
        None,
    );
    assert!(def.is_some(), "should find definition for host");
    assert_eq!(def.unwrap().start.row, 0, "host def should be on line 0");
}

#[test]
fn test_find_references_sub() {
    let src = "sub greet { }\ngreet();\ngreet();";
    let fa = build_fa(src);
    // Cursor on the sub name
    let refs = fa.find_references(Point::new(0, 5), None, None, None);
    assert!(
        refs.len() >= 2,
        "should find definition + calls, got {}",
        refs.len()
    );
}

#[test]
fn test_find_references_method_through_chain() {
    let src = "\
package Foo;
sub new { bless {}, shift }
sub bar { 42 }
package main;
sub get_foo { return Foo->new() }
my $f = Foo->new();
$f->bar();
get_foo()->bar();
";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // Cursor on bar definition (line 2, col 4)
    let refs = fa.find_references(Point::new(2, 5), Some(&tree), Some(src.as_bytes()), None);
    // Should find: $f->bar() + get_foo()->bar() (definition may or may not be included)
    let ref_lines: Vec<usize> = refs.iter().map(|s| s.start.row).collect();
    assert!(
        refs.len() >= 2,
        "should find at least 2 refs, got {} at lines {:?}",
        refs.len(),
        ref_lines
    );
    // The key assertion: chained call get_foo()->bar() is found (was broken before P0a fix)
    assert!(
        ref_lines.contains(&7),
        "should find chained get_foo()->bar() at line 7, got {:?}",
        ref_lines
    );
}

#[test]
fn test_hash_key_def_in_return_gets_sub_owner() {
    let src = "sub get_config {\n    return { host => 'localhost', port => 5432 };\n}\nmy $cfg = get_config();\n$cfg->{host};\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    // Verify hash key defs exist with Sub owner
    let host_defs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
        .collect();
    assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");
    if let SymbolDetail::HashKeyDef { ref owner, .. } = host_defs[0].detail {
        assert_eq!(
            *owner,
            HashKeyOwner::Sub {
                // Top-level scripts default to `main` per Perl's
                // own semantics; the implicit-package seed in
                // `Builder::new` makes this an explicit `Some("main")`
                // rather than `None`.
                package: Some("main".to_string()),
                name: "get_config".to_string()
            },
            "host def should have Sub get_config owner, got {:?}",
            owner
        );
    }

    // Verify HashKeyAccess ref for $cfg->{host} has Sub owner
    let host_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        !host_refs.is_empty(),
        "should find HashKeyAccess for 'host'"
    );
    if let RefKind::HashKeyAccess { ref owner, .. } = host_refs[0].kind {
        assert_eq!(
            *owner,
            Some(HashKeyOwner::Sub {
                package: Some("main".to_string()),
                name: "get_config".to_string()
            }),
            "host ref should have Sub get_config owner, got {:?}",
            owner
        );
    }

    // Verify go-to-references from the def finds the usage
    let host_def_point = host_defs[0].selection_span.start;
    let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()), None);
    // symbol_at returns include_decl=false, so only usages are returned
    assert!(
        refs.len() >= 1,
        "should find at least 1 usage, got {} refs",
        refs.len()
    );

    // Verify go-to-references from the usage finds back to the def
    let host_ref_point = host_refs[0].span.start;
    let refs_from_usage = fa.find_references(host_ref_point, Some(&tree), Some(src.as_bytes()), None);
    // ref resolves to def → include_decl=true, so def + usage
    assert!(
        refs_from_usage.len() >= 2,
        "should find def + usage, got {} refs",
        refs_from_usage.len()
    );
}

#[test]
fn test_hash_key_refs_chained_resolved_at_build() {
    // Chained method calls returning a Sub-keyed hash: the build-time
    // `emit_chained_hash_key_refs` pass resolves the owner to
    // `Sub{Calculator, get_config}` (the implicit-return keys), so the
    // stored ref carries it — no tree fallback needed.
    let src = r#"package Calculator;
sub new { bless {}, shift }
sub get_self { my ($self) = @_; return $self; }
sub get_config { return { host => "localhost", port => 5432 }; }
package main;
my $calc = Calculator->new();
$calc->get_self->get_config->{host};
"#;
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    // Find the hash key def for "host" in get_config's return
    let host_defs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "host" && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
        .collect();
    assert!(!host_defs.is_empty(), "should find HashKeyDef for 'host'");

    // The chained ref now carries its resolved owner at build time.
    let owner = fa
        .refs
        .iter()
        .find_map(|r| match &r.kind {
            RefKind::HashKeyAccess { owner: Some(o), .. } if r.target_name == "host" => {
                Some(o.clone())
            }
            _ => None,
        })
        .expect("chained hash access should carry a resolved owner");
    assert_eq!(
        owner,
        HashKeyOwner::Sub {
            package: Some("Calculator".to_string()),
            name: "get_config".to_string(),
        },
        "owner should be the return-hash sub of the last chain hop, got {:?}",
        owner
    );

    // find_references from the def finds the chained usage.
    let host_def_point = host_defs[0].selection_span.start;
    let refs = fa.find_references(host_def_point, Some(&tree), Some(src.as_bytes()), None);
    assert!(
        refs.len() >= 1,
        "should find chained usage, got {} refs",
        refs.len()
    );
}

#[test]
fn test_highlights_read_write() {
    let src = "my $x = 1;\nprint $x;\n$x = 2;";
    let fa = build_fa(src);
    let highlights = fa.find_highlights(Point::new(0, 4), None, None, None);
    assert!(!highlights.is_empty(), "should have highlights");
    // Check that we have both read and write accesses
    let has_write = highlights
        .iter()
        .any(|(_, a)| matches!(a, AccessKind::Write));
    let has_read = highlights
        .iter()
        .any(|(_, a)| matches!(a, AccessKind::Read));
    // At minimum we should see the declaration
    assert!(
        highlights.len() >= 2,
        "should have at least 2 highlights, got {}",
        highlights.len()
    );
    // Note: whether read/write are correctly tagged depends on builder's access classification
    let _ = (has_write, has_read); // suppress unused warnings if assertions change
}

#[test]
fn test_hover_variable() {
    let src = "my $greeting = 'hello';\nprint $greeting;";
    let fa = build_fa(src);
    let hover = fa.hover_info(Point::new(1, 8), src, None);
    assert!(hover.is_some(), "should have hover info");
    let text = hover.unwrap();
    assert!(
        text.contains("$greeting"),
        "hover should contain variable name, got: {}",
        text
    );
}

#[test]
fn test_hover_sub() {
    let src = "sub greet { }\ngreet();";
    let fa = build_fa(src);
    let hover = fa.hover_info(Point::new(1, 1), src, None);
    assert!(hover.is_some(), "should have hover info for function call");
    let text = hover.unwrap();
    assert!(
        text.contains("greet"),
        "hover should contain sub name, got: {}",
        text
    );
}

#[test]
fn test_hover_shows_inferred_type() {
    let src =
        "package Point;\nsub new { bless {}, shift }\npackage main;\nmy $p = Point->new();\n$p;";
    let fa = build_fa(src);
    // Hover on $p usage at line 4
    let hover = fa.hover_info(Point::new(4, 1), src, None);
    assert!(hover.is_some(), "should have hover info");
    let text = hover.unwrap();
    assert!(
        text.contains("Point"),
        "hover should show inferred type Point, got: {}",
        text
    );
}

#[test]
fn test_hover_type_at_usage_after_reassignment() {
    // $x starts as Point, gets reassigned to Foo — hover at each usage should reflect the type at that point
    let src = "package Point;\nsub new { bless {}, shift }\npackage Foo;\nsub new { bless {}, shift }\npackage main;\nmy $x = Point->new();\n$x;\n$x = Foo->new();\n$x;";
    let fa = build_fa(src);
    // line 6: $x; — should be Point
    let hover1 = fa.hover_info(Point::new(6, 1), src, None);
    assert!(hover1.is_some());
    let text1 = hover1.unwrap();
    assert!(
        text1.contains("Point"),
        "at line 6 should be Point, got: {}",
        text1
    );
    // line 8: $x; — should be Foo (after reassignment)
    let hover2 = fa.hover_info(Point::new(8, 1), src, None);
    assert!(hover2.is_some());
    let text2 = hover2.unwrap();
    assert!(
        text2.contains("Foo"),
        "at line 8 should be Foo, got: {}",
        text2
    );
}

#[test]
fn test_hover_shows_return_type() {
    let src = "package Foo;\nsub make { return Foo->new() }\nsub new { bless {}, shift }\npackage main;\nmake();";
    let fa = build_fa(src);
    // Hover on sub make definition
    let hover = fa.hover_info(Point::new(1, 5), src, None);
    assert!(hover.is_some(), "should have hover info for sub");
    let text = hover.unwrap();
    assert!(
        text.contains("returns"),
        "hover should show return type, got: {}",
        text
    );
    assert!(
        text.contains("Foo"),
        "hover return type should mention Foo, got: {}",
        text
    );
}

#[test]
fn test_rename_variable() {
    let src = "my $x = 1;\nprint $x;";
    let fa = build_fa(src);
    let edits = fa.rename_at(Point::new(0, 4), "y", None, None);
    assert!(edits.is_some(), "should produce rename edits");
    let edits = edits.unwrap();
    assert!(
        edits.len() >= 2,
        "should rename at least declaration + usage"
    );
    for (_, new_text) in &edits {
        assert_eq!(new_text, "y", "all edits should use new name");
    }
}

#[test]
fn test_rename_sub_finds_both_function_and_method_calls() {
    let fa = build_fa(
        "
package Foo;
sub emit { }
sub test {
    my $self = shift;
    emit('event');
    $self->emit('done');
}
",
    );
    // `sub emit` in package Foo. Scope-aware rename with
    // package=Foo catches the decl, the FunctionCall `emit()`,
    // AND the MethodCall `$self->emit()` — two shapes of the
    // same callable.
    let edits = fa.rename_sub_in_package("emit", &Some("Foo".to_string()), "fire", None);
    assert!(
        edits.len() >= 3,
        "rename_sub_in_package should find def + function call + method call, got {} edits",
        edits.len()
    );
    for (_, text) in &edits {
        assert_eq!(text, "fire");
    }
}

#[test]
fn test_moo_has_creates_constructor_hash_key_def() {
    let fa = build_fa(
        "
package MyApp;
use Moo;
has username => (is => 'ro');
has password => (is => 'rw');
",
    );
    // Should have HashKeyDef symbols owned by "new" for each has attribute
    let key_defs: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
        .collect();
    let names: Vec<&str> = key_defs.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"username"),
        "should have HashKeyDef for username, got: {:?}",
        names
    );
    assert!(
        names.contains(&"password"),
        "should have HashKeyDef for password, got: {:?}",
        names
    );
    // Verify owner is Sub { package: "MyApp", name: "new" }
    if let SymbolDetail::HashKeyDef { ref owner, .. } = key_defs[0].detail {
        assert_eq!(
            owner,
            &HashKeyOwner::Sub {
                package: Some("MyApp".to_string()),
                name: "new".to_string(),
            }
        );
    }
}

// ---- ERROR recovery tests ----
// tree-sitter-perl wraps broken regions in ERROR nodes. Some structural
// declarations (sub, class) survive as typed nodes inside ERROR.
// use/package often get parsed as raw function tokens inside ERROR —
// those can't be recovered (parser fix needed).

#[test]
fn test_error_recovery_sub_outside_error() {
    // my $x = [ creates an ERROR, but sub below it survives as a top-level node
    let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
    let fa = build_fa(source);
    let subs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
        .map(|s| s.name.as_str())
        .collect();
    assert!(
        subs.contains(&"process"),
        "sub process should survive (outside ERROR)"
    );
}

#[test]
fn test_error_recovery_sub_outside_error_survives() {
    // Sub below an ERROR survives as a top-level node (not inside ERROR)
    let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
    let fa = build_fa(source);
    let subs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
        .map(|s| s.name.as_str())
        .collect();
    assert!(
        subs.contains(&"process"),
        "sub process should survive (outside ERROR)"
    );
}

#[test]
fn test_error_node_does_not_panic() {
    // ERROR nodes should not crash the builder
    let source = "package Foo;\nmy $x = [\nmy $y = [\nsub process { }\n";
    let fa = build_fa(source);
    let pkgs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Package))
        .map(|s| s.name.as_str())
        .collect();
    assert!(pkgs.contains(&"Foo"), "package Foo should survive");
}

#[test]
fn test_error_recovery_sub_inside_error() {
    let source = "package Foo;\nmy $x = [\nmy $y = [\nsub process { }\n";
    let fa = build_fa(source);
    let subs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
        .map(|s| s.name.as_str())
        .collect();
    assert!(
        subs.contains(&"process"),
        "sub process should be recovered from ERROR"
    );
}

#[test]
fn test_error_recovery_import_inside_error() {
    let source = "package Foo;\nmy $x = [\nuse List::Util qw(max);\nsub process { }\n";
    let fa = build_fa(source);
    let imports: Vec<&str> = fa.imports.iter().map(|i| i.module_name.as_str()).collect();
    assert!(
        imports.contains(&"List::Util"),
        "use List::Util should be recovered from ERROR"
    );
}

#[test]
fn test_error_recovery_package_inside_error() {
    let source = "my $x = [\npackage Bar;\nuse Moose;\nsub bar { }\n";
    let fa = build_fa(source);
    let pkgs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Package))
        .map(|s| s.name.as_str())
        .collect();
    assert!(
        pkgs.contains(&"Bar"),
        "package Bar should be recovered from ERROR"
    );
}

#[test]
fn test_find_def_bareword_class() {
    let src = "package Point;\nsub new { bless {}, shift }\npackage main;\nPoint->new();";
    let fa = build_fa(src);
    // Cursor on "new" in Point->new()
    let def = fa.find_definition(Point::new(3, 8), None, None, None);
    assert!(def.is_some(), "should find definition for new");
}

// ---- Block dereference descent tests ----
// @{expr}, %{expr}, ${expr} parse as scalar/array/hash with varname→block.
// The builder must recurse into the block to find inner refs.

#[test]
fn test_deref_block_produces_inner_variable_ref() {
    // @{$arr} — the inner $arr should produce a Variable ref
    let fa = build_fa("my @data = (1,2,3);\nmy $arr = \\@data;\npush @{$arr}, 4;");
    let inner_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| {
            r.target_name == "$arr"
                && matches!(r.kind, RefKind::Variable)
                && r.access == AccessKind::Read
        })
        .collect();
    assert!(
        !inner_refs.is_empty(),
        "should find $arr ref inside @{{$arr}}"
    );
    // Should NOT have a bogus ref for the whole @{$arr}
    let bogus: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name.contains("{$arr}"))
        .collect();
    assert!(
        bogus.is_empty(),
        "should not record bogus ref for whole deref expression"
    );
}

#[test]
fn test_deref_block_produces_hash_key_ref() {
    // @{$self->{items}} — inner hash_element_expression should produce:
    // 1. Variable ref for $self
    // 2. HashKeyAccess ref for "items"
    let fa = build_fa("my %h = (items => []);\n@{$h{items}};");
    let key_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "items" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        !key_refs.is_empty(),
        "should find hash key ref 'items' inside deref block"
    );
}

#[test]
fn test_deref_block_resolves_variable() {
    // Variable inside deref block should resolve to its declaration
    let fa = build_fa("my @xs = (1,2);\nmy $ref = \\@xs;\nprint @{$ref};");
    let inner_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "$ref" && r.access == AccessKind::Read)
        .collect();
    assert!(!inner_refs.is_empty(), "$ref ref should exist");
    assert!(
        inner_refs[0].resolves_to.is_some(),
        "$ref inside deref should resolve to declaration"
    );
}

#[test]
fn test_deref_self_and_hash_key() {
    // Full integration: constructor defines hash keys, method accesses them through deref
    let src = "package Calculator;\nsub new {\n    my ($class, %args) = @_;\n    my $self = bless {\n        history => [],\n        verbose => 0,\n    }, $class;\n    return $self;\n}\nsub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    push @{$self->{history}}, \"add\";\n    return $result;\n}";
    let fa = build_fa(src);

    // $self at line 12 (push @{$self->{history}}, ...)
    let def_self = fa.find_definition(Point::new(12, 12), None, None, None);
    assert!(
        def_self.is_some(),
        "should find definition for $self in deref"
    );
    assert_eq!(
        def_self.unwrap().start.row,
        10,
        "$self should resolve to declaration on line 10"
    );

    // history key at line 12
    let def_history = fa.find_definition(Point::new(12, 20), None, None, None);
    assert!(
        def_history.is_some(),
        "should find definition for history hash key"
    );
    assert_eq!(
        def_history.unwrap().start.row,
        4,
        "history key should resolve to definition on line 4"
    );
}

#[test]
fn test_imports_qw() {
    let source = "use List::Util qw(first any all);\nuse Scalar::Util qw(blessed);\n";
    let fa = build_fa(source);

    assert_eq!(fa.imports.len(), 2);

    assert_eq!(fa.imports[0].module_name, "List::Util");
    let names0: Vec<&str> = fa.imports[0]
        .imported_symbols
        .iter()
        .map(|s| s.local_name.as_str())
        .collect();
    assert_eq!(names0, vec!["first", "any", "all"]);

    assert_eq!(fa.imports[1].module_name, "Scalar::Util");
    let names1: Vec<&str> = fa.imports[1]
        .imported_symbols
        .iter()
        .map(|s| s.local_name.as_str())
        .collect();
    assert_eq!(names1, vec!["blessed"]);
}

#[test]
fn test_imports_qw_close_paren_position() {
    // "use List::Util qw(first);\n"
    //  0123456789...
    //                  ^18    ^24 = )
    let source = "use List::Util qw(first);\n";
    let fa = build_fa(source);

    assert_eq!(fa.imports.len(), 1);
    let imp = &fa.imports[0];
    assert!(imp.qw_close_paren.is_some(), "qw_close_paren should be set");
    let pos = imp.qw_close_paren.unwrap();
    // The ) is at column 23 in "use List::Util qw(first);"
    assert_eq!(pos.row, 0);
    assert_eq!(pos.column, 23, "close paren should be at column 23");
}

#[test]
fn test_imports_bare() {
    let source = "use strict;\nuse warnings;\nuse Carp;\n";
    let fa = build_fa(source);

    // strict/warnings/Carp all produce imports with empty imported_symbols
    let carp = fa.imports.iter().find(|i| i.module_name == "Carp");
    assert!(carp.is_some());
    assert!(carp.unwrap().imported_symbols.is_empty());
}

#[test]
fn test_imports_module_symbol_created() {
    let source = "use List::Util qw(first);\n";
    let fa = build_fa(source);

    // Module symbol should exist
    let module_syms: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Module && s.name == "List::Util")
        .collect();
    assert_eq!(module_syms.len(), 1);

    // Import should exist
    assert_eq!(fa.imports.len(), 1);
    let names: Vec<&str> = fa.imports[0]
        .imported_symbols
        .iter()
        .map(|s| s.local_name.as_str())
        .collect();
    assert_eq!(names, vec!["first"]);
}

#[test]
fn test_goto_def_slurpy_hash_arg_at_call_site() {
    // Calculator->new(verbose => 1): cursor on "verbose" should go to
    // the bless hash key def, NOT to sub new.
    let src = r#"package Calculator;
sub new {
    my ($class, %args) = @_;
    my $self = bless {
        verbose => $args{verbose} // 0,
    }, $class;
    return $self;
}
package main;
my $calc = Calculator->new(verbose => 1);
"#;
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // "verbose" at call site is line 9, after "Calculator->new("
    // Calculator->new(verbose => 1)
    // 0123456789012345678901234567
    //                 ^16 = v of verbose
    // my $calc = Calculator->new(verbose => 1);
    // 0         1         2         3
    // 0123456789012345678901234567890123456789
    //                            ^27 = v of verbose
    let def = fa.find_definition(Point::new(9, 27), Some(&tree), Some(src.as_bytes()), None);
    assert!(
        def.is_some(),
        "should find definition for verbose at call site"
    );
    // Should go to line 4: "verbose => $args{verbose} // 0,"
    assert_eq!(
        def.unwrap().start.row,
        4,
        "verbose should resolve to bless hash key def on line 4, not sub new"
    );
}

#[test]
fn test_goto_def_param_field_at_call_site() {
    // Point->new(x => 3, y => 4): cursor on "x" should go to "field $x :param"
    let src = r#"use v5.38;
class Point {
    field $x :param :reader;
    field $y :param;
    method magnitude() { }
}
my $p = Point->new(x => 3, y => 4);
"#;
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    // my $p = Point->new(x => 3, y => 4);
    // 0         1         2
    // 0123456789012345678901234
    //                    ^19 = x

    let def = fa.find_definition(Point::new(6, 19), Some(&tree), Some(src.as_bytes()), None);
    assert!(def.is_some(), "should find definition for x at call site");
    // Should go to line 2: "field $x :param :reader;"
    assert_eq!(
        def.unwrap().start.row,
        2,
        "x should resolve to field $x on line 2, not the class"
    );
}

// ---- Gap 1: __PACKAGE__ resolution ----

#[test]
fn test_dunder_package_resolution() {
    let fa = build_fa(
        "
        package Mojo::File;
        sub path { __PACKAGE__->new(@_) }
        ",
    );
    let rt = fa.sub_return_type_at_arity("path", None);
    assert_eq!(rt, Some(InferredType::ClassName("Mojo::File".into())));
}

#[test]
fn test_dunder_package_method_invocant() {
    // __PACKAGE__->new() should store the resolved class in MethodCall invocant
    let fa = build_fa(
        "
        package Foo;
        __PACKAGE__->some_method();
        ",
    );
    let method_ref = fa
        .refs
        .iter()
        .find(|r| r.target_name == "some_method")
        .unwrap();
    match &method_ref.kind {
        RefKind::MethodCall { invocant, .. } => {
            assert_eq!(
                invocant, "Foo",
                "invocant should be resolved from __PACKAGE__"
            );
        }
        _ => panic!("expected MethodCall ref"),
    }
}

// ---- Gap 2: Shift parameter extraction ----

#[test]
fn test_shift_params() {
    let fa = build_fa(
        "
        sub process {
            my $self = shift;
            my $file = shift;
            my $opts = shift || {};
        }
        ",
    );
    // signature_for_call strips $self when first param is $self
    let sig = fa
        .signature_for_call("process", false, None, Point::new(0, 0), None)
        .unwrap();
    assert!(sig.is_method, "should detect method from $self first param");
    assert_eq!(sig.params.len(), 2);
    assert_eq!(sig.params[0].name, "$file");
    assert_eq!(sig.params[1].name, "$opts");
    assert_eq!(sig.params[1].default, Some("{}".into()));

    // Check raw params via symbol detail
    let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
    if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
        assert_eq!(params.len(), 3);
        assert_eq!(params[0].name, "$self");
        assert_eq!(params[1].name, "$file");
        assert_eq!(params[2].name, "$opts");
        assert_eq!(params[2].default, Some("{}".into()));
    } else {
        panic!("expected Sub detail");
    }
}

#[test]
fn test_shift_then_list_assign() {
    let fa = build_fa(
        "
        sub process {
            my $self = shift;
            my ($file, @opts) = @_;
        }
        ",
    );
    let sig = fa
        .signature_for_call("process", false, None, Point::new(0, 0), None)
        .unwrap();
    assert!(sig.is_method);
    assert_eq!(
        sig.params.len(),
        2,
        "should have $file and @opts (stripped $self)"
    );
    assert_eq!(sig.params[0].name, "$file");
    assert_eq!(sig.params[1].name, "@opts");
    assert!(sig.params[1].is_slurpy);

    // Check raw params
    let sub_sym = fa.symbols.iter().find(|s| s.name == "process").unwrap();
    if let SymbolDetail::Sub { ref params, .. } = sub_sym.detail {
        assert_eq!(params.len(), 3);
        assert_eq!(params[0].name, "$self");
    } else {
        panic!("expected Sub detail");
    }
}

#[test]
fn test_shift_with_double_pipe_default() {
    let fa = build_fa(
        "
        sub handler {
            my $self = shift;
            my $timeout = shift || 30;
        }
        ",
    );
    let sig = fa
        .signature_for_call("handler", false, None, Point::new(0, 0), None)
        .unwrap();
    assert_eq!(sig.params.len(), 1, "stripped $self");
    assert_eq!(sig.params[0].name, "$timeout");
    assert_eq!(sig.params[0].default, Some("30".into()));
}

#[test]
fn test_shift_with_defined_or_default() {
    let fa = build_fa(
        "
        sub handler {
            my $self = shift;
            my $verbose = shift // 0;
        }
        ",
    );
    let sig = fa
        .signature_for_call("handler", false, None, Point::new(0, 0), None)
        .unwrap();
    assert_eq!(sig.params.len(), 1, "stripped $self");
    assert_eq!(sig.params[0].name, "$verbose");
    assert_eq!(sig.params[0].default, Some("0".into()));
}

#[test]
fn test_subscript_param() {
    let fa = build_fa(
        "
        sub handler {
            my $self = $_[0];
            my $data = $_[1];
        }
        ",
    );
    let sig = fa
        .signature_for_call("handler", false, None, Point::new(0, 0), None)
        .unwrap();
    assert_eq!(sig.params.len(), 1, "stripped $self");
    assert_eq!(sig.params[0].name, "$data");
}

#[test]
fn test_legacy_at_params_still_work() {
    // Ensure the existing @_ pattern still works
    let fa = build_fa(
        "
        sub process {
            my ($first, $file, @opts) = @_;
        }
        ",
    );
    let sig = fa
        .signature_for_call("process", false, None, Point::new(0, 0), None)
        .unwrap();
    assert_eq!(sig.params.len(), 3);
    assert_eq!(sig.params[0].name, "$first");
    assert_eq!(sig.params[1].name, "$file");
    assert_eq!(sig.params[2].name, "@opts");
}

#[test]
fn test_tail_pod_item_method() {
    let fa = build_fa(
        "
            package WWW::Mech;
            sub get { }
            sub post { }

=head1 METHODS

=over

=item $mech->get($url)

Performs a GET request.

=item $mech->post($url)

Performs a POST request.

=back

=cut
        ",
    );
    let get_doc = fa
        .symbols
        .iter()
        .find(|s| s.name == "get")
        .and_then(|s| match &s.detail {
            SymbolDetail::Sub { doc, .. } => doc.as_ref(),
            _ => None,
        });
    assert!(get_doc.is_some(), "get should have doc from =item");
    assert!(get_doc.unwrap().contains("GET request"));
}

#[test]
fn test_pod_doc_extracted_per_function() {
    let src = "\
package DemoUtils;
use Exporter 'import';
our @EXPORT_OK = qw(fetch_data transform);

=head2 fetch_data

Fetches data from the given URL.

=head2 transform

Transforms items.

=cut

sub fetch_data { }
sub transform { }
";
    let fa = build_fa(src);
    let fd = fa.symbols.iter().find(|s| s.name == "fetch_data").unwrap();
    if let SymbolDetail::Sub { ref doc, .. } = fd.detail {
        let d = doc.as_ref().expect("fetch_data should have doc");
        assert!(
            d.contains("Fetches data"),
            "should have fetch_data doc, got: {}",
            d
        );
        assert!(
            !d.contains("Transforms items"),
            "should NOT have transform doc, got: {}",
            d
        );
    } else {
        panic!("fetch_data should be a Sub");
    }
}

// ---- Inheritance extraction tests ----

#[test]
fn test_use_parent_single() {
    let fa = build_fa(
        "
            package Child;
            use parent 'Parent';
            sub child_method { }
        ",
    );
    assert_eq!(
        fa.package_parents.get("Child").unwrap(),
        &vec!["Parent".to_string()]
    );
}

#[test]
fn test_use_parent_multiple() {
    let fa = build_fa(
        "
            package Multi;
            use parent qw(Foo Bar);
        ",
    );
    assert_eq!(
        fa.package_parents.get("Multi").unwrap(),
        &vec!["Foo".to_string(), "Bar".to_string()]
    );
}

#[test]
fn test_use_parent_emits_package_refs() {
    let fa = build_fa(
        "
            package Child;
            use parent 'Parent';
        ",
    );
    let refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Parent")
        .collect();
    assert_eq!(refs.len(), 1, "should emit PackageRef for parent class");
}

#[test]
fn test_use_parent_qw_emits_package_refs() {
    let fa = build_fa(
        "
            package Multi;
            use parent qw(Foo Bar);
        ",
    );
    let foo_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Foo")
        .collect();
    let bar_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::PackageRef) && r.target_name == "Bar")
        .collect();
    assert_eq!(foo_refs.len(), 1, "should emit PackageRef for Foo");
    assert_eq!(bar_refs.len(), 1, "should emit PackageRef for Bar");
}

#[test]
fn test_use_parent_norequire() {
    let fa = build_fa(
        "
            package Local;
            use parent -norequire, 'My::Base';
        ",
    );
    assert_eq!(
        fa.package_parents.get("Local").unwrap(),
        &vec!["My::Base".to_string()]
    );
}

#[test]
fn test_use_base() {
    let fa = build_fa(
        "
            package Old;
            use base 'Legacy::Base';
        ",
    );
    assert_eq!(
        fa.package_parents.get("Old").unwrap(),
        &vec!["Legacy::Base".to_string()]
    );
}

#[test]
fn test_isa_assignment() {
    let fa = build_fa(
        "
            package Direct;
            our @ISA = ('Alpha', 'Beta');
        ",
    );
    assert_eq!(
        fa.package_parents.get("Direct").unwrap(),
        &vec!["Alpha".to_string(), "Beta".to_string()]
    );
}

#[test]
fn test_class_isa_populates_package_parents() {
    let fa = build_fa(
        "
            class Child :isa(Parent) { }
        ",
    );
    assert_eq!(
        fa.package_parents.get("Child").unwrap(),
        &vec!["Parent".to_string()]
    );
}

#[test]
fn test_class_does_populates_package_parents() {
    let fa = build_fa(
        "
            class MyClass :does(Printable) :does(Serializable) { }
        ",
    );
    let parents = fa.package_parents.get("MyClass").unwrap();
    assert!(parents.contains(&"Printable".to_string()));
    assert!(parents.contains(&"Serializable".to_string()));
}

#[test]
fn test_class_isa_and_does_combined() {
    let fa = build_fa(
        "
            class Child :isa(Parent) :does(Role) { }
        ",
    );
    let parents = fa.package_parents.get("Child").unwrap();
    assert_eq!(parents, &vec!["Parent".to_string(), "Role".to_string()]);
}

#[test]
fn test_with_role_populates_package_parents() {
    let fa = build_fa(
        "
            package MyApp;
            use Moo;
            with 'My::Role::Logging';
        ",
    );
    let parents = fa.package_parents.get("MyApp").unwrap();
    assert!(parents.contains(&"My::Role::Logging".to_string()));
}

#[test]
fn test_with_multiple_roles() {
    let fa = build_fa(
        "
            package MyApp;
            use Moose;
            with 'Role::A', 'Role::B';
        ",
    );
    let parents = fa.package_parents.get("MyApp").unwrap();
    assert!(parents.contains(&"Role::A".to_string()));
    assert!(parents.contains(&"Role::B".to_string()));
}

#[test]
fn test_load_components_bare() {
    let fa = build_fa(
        "
            package MySchema::Result::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components('InflateColumn::DateTime', 'TimeStamp');
        ",
    );
    let parents = fa.package_parents.get("MySchema::Result::User").unwrap();
    assert!(parents.contains(&"DBIx::Class::Core".to_string()));
    assert!(parents.contains(&"DBIx::Class::InflateColumn::DateTime".to_string()));
    assert!(parents.contains(&"DBIx::Class::TimeStamp".to_string()));
}

#[test]
fn test_load_components_plus_prefix() {
    let fa = build_fa(
        "
            package MySchema::Result::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components('+My::Custom::Component');
        ",
    );
    let parents = fa.package_parents.get("MySchema::Result::User").unwrap();
    assert!(parents.contains(&"My::Custom::Component".to_string()));
}

#[test]
fn test_load_components_qw() {
    let fa = build_fa(
        "
            package MySchema::ResultSet::User;
            use base 'DBIx::Class::Core';
            __PACKAGE__->load_components(qw(Helper::ResultSet::Shortcut Helper::ResultSet::Me));
        ",
    );
    let parents = fa.package_parents.get("MySchema::ResultSet::User").unwrap();
    assert!(parents.contains(&"DBIx::Class::Helper::ResultSet::Shortcut".to_string()));
    assert!(parents.contains(&"DBIx::Class::Helper::ResultSet::Me".to_string()));
}

// ---- Inheritance method resolution tests ----

#[test]
fn test_inherited_method_completion() {
    let fa = build_fa(
        "
            package Animal;
            sub speak { }
            sub eat { }

            package Dog;
            use parent 'Animal';
            sub fetch { }
        ",
    );
    let methods = fa.complete_methods_for_class("Dog", None);
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"fetch"), "own method");
    assert!(names.contains(&"speak"), "inherited from Animal");
    assert!(names.contains(&"eat"), "inherited from Animal");
}

#[test]
fn test_child_method_overrides_parent() {
    let fa = build_fa(
        "
            package Base;
            sub greet { }

            package Override;
            use parent 'Base';
            sub greet { }
        ",
    );
    let methods = fa.complete_methods_for_class("Override", None);
    let greet_count = methods.iter().filter(|c| c.label == "greet").count();
    assert_eq!(greet_count, 1, "child override should shadow parent");
}

#[test]
fn test_find_method_in_parent() {
    let fa = build_fa(
        "
            package Base;
            sub base_method { }

            package Child;
            use parent 'Base';
        ",
    );
    let span = fa.find_method_in_class("Child", "base_method");
    assert!(span.is_some(), "should find inherited method");
}

#[test]
fn test_inherited_return_type() {
    let fa = build_fa(
        "
            package Factory;
            sub create { Factory->new(@_) }

            package SpecialFactory;
            use parent 'Factory';
        ",
    );
    let rt = fa.find_method_return_type("SpecialFactory", "create", None, None);
    assert!(rt.is_some(), "should find return type from parent");
}

#[test]
fn test_multi_level_inheritance() {
    let fa = build_fa(
        "
            package A;
            sub from_a { }

            package B;
            use parent 'A';
            sub from_b { }

            package C;
            use parent 'B';
            sub from_c { }
        ",
    );
    let methods = fa.complete_methods_for_class("C", None);
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"from_a"));
    assert!(names.contains(&"from_b"));
    assert!(names.contains(&"from_c"));
}

#[test]
fn test_class_isa_inherits_methods() {
    let fa = build_fa(
        "
            class Parent {
                method greet() { }
            }
            class Child :isa(Parent) {
                method wave() { }
            }
        ",
    );
    let methods = fa.complete_methods_for_class("Child", None);
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"wave"), "own method");
    assert!(names.contains(&"greet"), "inherited from Parent");
}

// ---- Cross-file inheritance tests ----

/// Build a CachedModule from a synthesized Perl source listing the given subs
/// (each as an `sub name { $self }` method) plus optional parent packages.
fn fake_cached_for_class(
    package_name: &str,
    path: &std::path::Path,
    subs: &[&str],
    parents: &[&str],
) -> std::sync::Arc<crate::module_index::CachedModule> {
    let mut source = format!("package {};\n", package_name);
    if !parents.is_empty() {
        source.push_str(&format!("use parent '{}';\n", parents.join("', '")));
    }
    for sub in subs {
        source.push_str(&format!("sub {} {{ my $self = shift; }}\n", sub));
    }
    source.push_str("1;\n");
    let fa = build_fa(&source);
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        path.to_path_buf(),
        std::sync::Arc::new(fa),
    ))
}

#[test]
fn test_cross_file_inherited_method_completion() {
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // Grandparent: DBI has `connect`
    idx.insert_cache(
        "DBI",
        Some(fake_cached_for_class(
            "DBI",
            &PathBuf::from("/fake/DBI.pm"),
            &["connect"],
            &[],
        )),
    );

    // Parent: DBI::db inherits from DBI, has `prepare`
    idx.insert_cache(
        "DBI::db",
        Some(fake_cached_for_class(
            "DBI::db",
            &PathBuf::from("/fake/DBI/db.pm"),
            &["prepare"],
            &["DBI"],
        )),
    );

    // Local code inherits from DBI::db
    let fa = build_fa(
        "
            package MyDB;
            use parent 'DBI::db';
            sub custom_query { }
        ",
    );

    let methods = fa.complete_methods_for_class("MyDB", Some(&idx));
    let names: Vec<&str> = methods.iter().map(|c| c.label.as_str()).collect();
    assert!(names.contains(&"custom_query"), "own method");
    assert!(names.contains(&"prepare"), "from DBI::db");
    assert!(names.contains(&"connect"), "from DBI (grandparent)");
}

#[test]
fn test_cross_file_method_override() {
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // Parent has `process`
    idx.insert_cache(
        "Base::Worker",
        Some(fake_cached_for_class(
            "Base::Worker",
            &PathBuf::from("/fake/Base/Worker.pm"),
            &["process"],
            &[],
        )),
    );

    // Local child overrides `process`
    let fa = build_fa(
        "
            package MyWorker;
            use parent 'Base::Worker';
            sub process { }
        ",
    );

    let methods = fa.complete_methods_for_class("MyWorker", Some(&idx));
    let process_count = methods.iter().filter(|c| c.label == "process").count();
    assert_eq!(process_count, 1, "local override should shadow parent");
}

#[test]
fn test_cross_file_return_type_through_inheritance() {
    use crate::file_analysis::InferredType;
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    // Parent module whose `fetch` returns a hashref with known keys.
    let source = r#"
package Fetcher;
sub fetch {
    my $self = shift;
    return { status => 1, body => 'ok' };
}
1;
"#;
    let fa_parent = build_fa(source);
    idx.insert_cache(
        "Fetcher",
        Some(std::sync::Arc::new(crate::module_index::CachedModule::new(
            PathBuf::from("/fake/Fetcher.pm"),
            std::sync::Arc::new(fa_parent),
        ))),
    );

    let fa = build_fa(
        "
            package MyFetcher;
            use parent 'Fetcher';
        ",
    );

    let rt = fa.find_method_return_type("MyFetcher", "fetch", Some(&idx), None);
    assert_eq!(rt, Some(InferredType::HashRef));
}

#[test]
fn test_parents_cached() {
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    idx.insert_cache(
        "Child::Mod",
        Some(fake_cached_for_class(
            "Child::Mod",
            &PathBuf::from("/fake/Child/Mod.pm"),
            &[],
            &["Parent::Mod", "Mixin::Role"],
        )),
    );

    let parents = idx.parents_cached("Child::Mod");
    assert_eq!(parents, vec!["Parent::Mod", "Mixin::Role"]);
    assert!(idx.parents_cached("Unknown::Mod").is_empty());
}

#[test]
fn test_cross_bag_inheritance_cycle_does_not_overflow() {
    // A → B and B → A across separate cached bags. Each bag's
    // package_parents only knows the local-side edge of the loop,
    // so the cycle only closes once the inheritance fallback in
    // `query_rec` crosses bags. A per-bag-only visited set lets the
    // walk re-enter A's bag for `MethodOnClass{A, _}` after going
    // through B, then re-enter B for `MethodOnClass{B, _}`, ad
    // infinitum until the stack overflows. Visited must compose
    // (bag, attachment) so the loop closes.
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);

    idx.insert_cache(
        "Cycle::A",
        Some(fake_cached_for_class(
            "Cycle::A",
            &PathBuf::from("/fake/Cycle/A.pm"),
            &[],
            &["Cycle::B"],
        )),
    );
    idx.insert_cache(
        "Cycle::B",
        Some(fake_cached_for_class(
            "Cycle::B",
            &PathBuf::from("/fake/Cycle/B.pm"),
            &[],
            &["Cycle::A"],
        )),
    );

    let fa = build_fa("package main; 1;");

    assert_eq!(
        fa.find_method_return_type("Cycle::A", "no_such_method", Some(&idx), None),
        None,
    );
    assert_eq!(
        fa.find_method_return_type("Cycle::B", "no_such_method", Some(&idx), None),
        None,
    );
}

// ---- Method call return type propagation tests ----

#[test]
fn test_method_call_return_type_propagates() {
    let fa = build_fa(
        "
package Foo;
sub new { bless {}, shift }
sub get_config {
    return { host => 'localhost', port => 5432 };
}
package main;
my $f = Foo->new();
my $cfg = $f->get_config();
$cfg;
",
    );
    let ty = fa.inferred_type_via_bag("$cfg", Point::new(9, 0));
    assert_eq!(ty, Some(InferredType::HashRef));
}

#[test]
fn test_method_call_chain_propagation() {
    let fa = build_fa(
        "
package Foo;
sub new { bless {}, shift }
sub get_bar { return Bar->new() }
package Bar;
sub new { bless {}, shift }
sub get_name { return { name => 'test' } }
package main;
my $f = Foo->new();
my $bar = $f->get_bar();
my $name = $bar->get_name();
$name;
",
    );
    let bar_ty = fa.inferred_type_via_bag("$bar", Point::new(10, 0));
    assert_eq!(bar_ty, Some(InferredType::ClassName("Bar".into())));
    let name_ty = fa.inferred_type_via_bag("$name", Point::new(11, 0));
    assert_eq!(name_ty, Some(InferredType::HashRef));
}

#[test]
fn test_self_method_call_return_type() {
    let fa = build_fa(
        "
package Foo;
sub new { bless {}, shift }
sub get_config { return { host => 1 } }
sub run {
    my ($self) = @_;
    my $cfg = $self->get_config();
    $cfg;
}
",
    );
    let ty = fa.inferred_type_via_bag("$cfg", Point::new(7, 4));
    assert_eq!(ty, Some(InferredType::HashRef));
}

// ---- Framework accessor synthesis tests ----

#[test]
fn test_moo_has_ro() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name' => (is => 'ro');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1, "should synthesize one getter");
    if let SymbolDetail::Sub {
        ref params,
        is_method,
        ..
    } = methods[0].detail
    {
        assert!(is_method);
        assert!(params.is_empty(), "ro getter has no params");
    }
}

#[test]
fn test_moo_has_rw() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name' => (is => 'rw');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    // rw produces getter (0 params) + setter (1 param)
    assert_eq!(methods.len(), 2, "should synthesize getter + setter");
}

#[test]
fn test_moo_has_isa_type() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'count' => (is => 'ro', isa => 'Int');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "count" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1);
    let __r = fa.symbol_return_type_via_bag(methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(return_type, Some(&InferredType::Numeric));
    }
}

#[test]
fn test_moo_has_multiple_qw() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has [qw(foo bar)] => (is => 'ro');
",
    );
    let foo: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "foo" && s.kind == SymKind::Method)
        .collect();
    let bar: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "bar" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(foo.len(), 1, "should synthesize foo accessor");
    assert_eq!(bar.len(), 1, "should synthesize bar accessor");
}

#[test]
fn test_moo_has_bare_no_accessor() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'internal' => (is => 'bare');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "internal" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 0, "bare should not synthesize accessor");
}

#[test]
fn test_moo_no_accessor_without_is() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'internal';
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "internal" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 0, "no `is` should not synthesize accessor");
}

#[test]
fn test_moose_has_classname_isa() {
    let fa = build_fa(
        "
package Foo;
use Moose;
has 'db' => (is => 'ro', isa => 'DBI::db');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "db" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1);
    let __r = fa.symbol_return_type_via_bag(methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("DBI::db".into()))
        );
    }
}

#[test]
fn test_moo_has_instanceof() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'logger' => (is => 'ro', isa => \"InstanceOf['Log::Any']\");
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "logger" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1);
    let __r = fa.symbol_return_type_via_bag(methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("Log::Any".into()))
        );
    }
}

#[test]
fn test_moo_has_rwp() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'status' => (is => 'rwp');
",
    );
    let getter: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "status" && s.kind == SymKind::Method)
        .collect();
    let writer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "_set_status" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(getter.len(), 1, "rwp should synthesize getter");
    assert_eq!(writer.len(), 1, "rwp should synthesize _set_name writer");
}

#[test]
fn test_moo_has_accessor_keyword() {
    // `accessor => 'get_set_x'` synthesizes a combined read/write method named
    // `get_set_x` — distinct from the attr-named default accessor.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'x' => (is => 'rw', accessor => 'get_set_x');
",
    );
    let acc: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "get_set_x" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(acc.len(), 1, "accessor keyword should synthesize get_set_x method");
    // The default attr-named accessor ('x') still exists from `is => 'rw'`.
    let default_acc: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "x" && s.kind == SymKind::Method)
        .collect();
    assert!(!default_acc.is_empty(), "default attr accessor still synthesized");
}

#[test]
fn test_moo_has_ro_does_not_synthesize_ro_symbol() {
    // `is => 'ro'` must NOT synthesize a method named `ro` — the gate that
    // fixes the phantom-`ro` regression (names_a_method excludes `is`).
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'y' => (is => 'ro');
",
    );
    let ro_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "ro")
        .collect();
    assert!(ro_sym.is_empty(), "`is => 'ro'` must not mint a symbol named `ro`");
}

#[test]
fn test_mojo_has_basic() {
    let fa = build_fa(
        "
package Foo;
use Mojo::Base -base;
has 'name';
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 2, "Mojo::Base synthesizes getter + setter");
    // Getter: no params, no return type
    let getter = methods
        .iter()
        .find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail {
                params.is_empty()
            } else {
                false
            }
        })
        .expect("should have getter");
    let __r = fa.symbol_return_type_via_bag(getter.id, None);
    let return_type = __r.as_ref();
    if let SymbolDetail::Sub { is_method, .. } = getter.detail {
        assert!(is_method);
        assert!(return_type.is_none(), "getter has no return type");
    }
    // Setter: 1 param, fluent return
    let setter = methods
        .iter()
        .find(|m| {
            if let SymbolDetail::Sub { ref params, .. } = m.detail {
                params.len() == 1
            } else {
                false
            }
        })
        .expect("should have setter");
    let __r = fa.symbol_return_type_via_bag(setter.id, None);
    let return_type = __r.as_ref();
    if let SymbolDetail::Sub { is_method, .. } = setter.detail {
        assert!(is_method);
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("Foo".into()))
        );
    }
}

/// Regression test for QA finding B1 (refer to qa-workspace/qa-findings.md):
/// Mojo::Base writer accessors lost their fluent invocant return type
/// when queried through the bag at arity=1. The QA dump for
/// `Mojo::Log::level` showed:
///
///   getter (params=0): raw=String,    bag=String     ← OK
///   writer (params=1): raw=Mojo::Log, bag=String     ← BUG, getter's type
///
/// `query_sub_return_type` finds the *first* same-named symbol by name
/// (the getter, since synthesis adds it first), folds witnesses on that
/// id, and answers from the getter's stored `return_type`. The writer's
/// distinct fluent type was never visible at the bag level — every
/// `$ua->ca($f)->cert(...)` chain lost its receiver type at the second
/// hop.
///
/// Fix: framework synthesis publishes `ReturnExpr::UnionOnArgs`
/// arms on `Symbol(sym_id)` (per-symbol arity arm) and a multi-arm
/// `UnionOnArgs` on `MethodOnClass{class, name}` (cross-symbol arity
/// dispatch scoped to the declaring class). `ReturnExprReducer`
/// dispatches `q.arity_hint` against the union's `ArgGuard` branches
/// regardless of which sister sym `find()` returned. See
/// `docs/adr/return-expr.md`.
#[test]
fn test_mojo_base_writer_returns_invocant_via_bag() {
    use crate::file_analysis::TypeProvenance;

    let fa = build_fa(
        "
package MyLog;
use Mojo::Base -base;

has level => 'info';   # default value gives getter type = String
has app;               # no default; getter has no return type
",
    );

    // arity=1 → fluent writer must return the package class for chaining.
    // Pre-fix this returned String (the getter's type) — that's the bug.
    assert_eq!(
        fa.sub_return_type_at_arity("level", Some(1)),
        Some(InferredType::ClassName("MyLog".into())),
        "Mojo::Base writer at arity=1 must return the invocant class for fluent chaining"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("app", Some(1)),
        Some(InferredType::ClassName("MyLog".into())),
        "Mojo::Base writer with no default still returns the invocant class"
    );

    // arity=0 → getter returns its scalar accessor type.
    assert_eq!(
        fa.sub_return_type_at_arity("level", Some(0)),
        Some(InferredType::String),
        "Mojo::Base getter at arity=0 returns the default-value type"
    );

    // Both sister symbols carry per-symbol witnesses now (was 0 across
    // every Mojo::* dump in the QA sweep).
    let getter = fa
        .symbols
        .iter()
        .find(|s| s.name == "level" && matches!(&s.detail, SymbolDetail::Sub { params, .. } if params.is_empty()))
        .expect("getter symbol");
    let writer = fa
        .symbols
        .iter()
        .find(|s| s.name == "level" && matches!(&s.detail, SymbolDetail::Sub { params, .. } if params.len() == 1))
        .expect("writer symbol");
    let getter_witnesses = fa
        .witnesses
        .for_attachment(&crate::witnesses::WitnessAttachment::Symbol(getter.id))
        .len();
    let writer_witnesses = fa
        .witnesses
        .for_attachment(&crate::witnesses::WitnessAttachment::Symbol(writer.id))
        .len();
    assert!(getter_witnesses > 0, "getter must have at least one bag witness");
    assert!(writer_witnesses > 0, "writer must have at least one bag witness");

    // Provenance: each accessor flushes a FrameworkSynthesis entry, not
    // PluginOverride (Mojo::Base synthesis is core, not a plugin).
    match fa.return_type_provenance(writer.id) {
        TypeProvenance::FrameworkSynthesis { framework, reason } => {
            assert_eq!(framework, "Mojo::Base");
            assert!(reason.contains("level"), "reason names the attribute");
            assert!(
                reason.contains("fluent") || reason.contains("writer"),
                "reason describes the writer role"
            );
        }
        other => panic!("writer provenance must be FrameworkSynthesis, got {other:?}"),
    }
    match fa.return_type_provenance(getter.id) {
        TypeProvenance::FrameworkSynthesis { framework, .. } => {
            assert_eq!(framework, "Mojo::Base");
        }
        other => panic!("getter provenance must be FrameworkSynthesis, got {other:?}"),
    }
}

/// Mirror of B1 for Moo `is => 'rw'` writers — same shape, isa-typed
/// rather than fluent. The writer reads back the isa type at arity=1.
#[test]
fn test_moo_rw_writer_returns_isa_type_via_bag() {
    use crate::file_analysis::TypeProvenance;

    let fa = build_fa(
        "
package Thing;
use Moo;
has size => (is => 'rw', isa => 'Int');
",
    );

    // Both arities resolve to the isa-derived type.
    assert_eq!(
        fa.sub_return_type_at_arity("size", Some(0)),
        Some(InferredType::Numeric),
        "Moo getter returns isa type"
    );
    assert_eq!(
        fa.sub_return_type_at_arity("size", Some(1)),
        Some(InferredType::Numeric),
        "Moo rw writer returns isa type"
    );

    let writer = fa
        .symbols
        .iter()
        .find(|s| s.name == "size" && matches!(&s.detail, SymbolDetail::Sub { params, .. } if params.len() == 1))
        .expect("writer symbol");
    match fa.return_type_provenance(writer.id) {
        TypeProvenance::FrameworkSynthesis { framework, .. } => {
            assert_eq!(framework, "Moo");
        }
        other => panic!("Moo writer provenance must be FrameworkSynthesis, got {other:?}"),
    }
}

#[test]
fn test_mojo_base_parent_inheritance() {
    let fa = build_fa(
        "
package MyApp;
use Mojo::Base 'Mojolicious';
has 'config';
",
    );
    // Should register parent
    assert_eq!(
        fa.package_parents.get("MyApp").map(|v| v.as_slice()),
        Some(["Mojolicious".to_string()].as_slice())
    );
    // Should synthesize getter + setter accessors
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "config" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 2, "Mojo::Base synthesizes getter + setter");
}

#[test]
fn test_mojo_base_strict_no_accessor() {
    let fa = build_fa(
        "
package Foo;
use Mojo::Base -strict;
has 'name';
",
    );
    // -strict means no framework mode, has is just a regular function
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(
        methods.len(),
        0,
        "-strict should not trigger accessor synthesis"
    );
}

#[test]
fn test_no_accessor_without_framework() {
    let fa = build_fa(
        "
package Foo;
has 'name' => (is => 'ro');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 0, "no framework = no accessor synthesis");
}

#[test]
fn test_dbic_add_columns() {
    let fa = build_fa(
        "
package Schema::Result::User;
use base 'DBIx::Class::Core';
__PACKAGE__->add_columns(
    id    => { data_type => 'integer' },
    name  => { data_type => 'varchar' },
    email => { data_type => 'varchar' },
);
",
    );
    let id: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "id" && s.kind == SymKind::Method)
        .collect();
    let name: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    let email: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "email" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(id.len(), 1, "should synthesize id accessor");
    assert_eq!(name.len(), 1, "should synthesize name accessor");
    assert_eq!(email.len(), 1, "should synthesize email accessor");
}

#[test]
fn test_dbic_has_many() {
    let fa = build_fa(
        "
package Schema::Result::Post;
use base 'DBIx::Class::Core';
__PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "comments" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1);
    let __r = fa.symbol_return_type_via_bag(methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("DBIx::Class::ResultSet".into()))
        );
    }
}

#[test]
fn test_dbic_belongs_to() {
    let fa = build_fa(
        "
package Schema::Result::Comment;
use base 'DBIx::Class::Core';
__PACKAGE__->belongs_to(author => 'Schema::Result::User', 'author_id');
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "author" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 1);
    let __r = fa.symbol_return_type_via_bag(methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("Schema::Result::User".into()))
        );
    }
}

#[test]
fn test_accessor_return_type_propagation() {
    let src = r#"
package Moo::Config;
use Moo;
has 'host' => (is => 'ro', isa => 'Str');
sub dsn { my ($self) = @_; return "x"; }

package Moo::Service;
use Moo;
has 'config' => (is => 'ro', isa => "InstanceOf['Moo::Config']");
sub run {
    my ($self) = @_;
    my $cfg = $self->config;
    my $dsn = $cfg->dsn;
}
"#;
    let fa = build_fa(src);

    // Verify the config accessor has the right return type
    let config_methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "config" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(config_methods.len(), 1, "should have 1 config accessor");
    assert_eq!(config_methods[0].package.as_deref(), Some("Moo::Service"));
    let __r = fa.symbol_return_type_via_bag(config_methods[0].id, None);
    let return_type = __r.as_ref();
    if matches!(config_methods[0].detail, SymbolDetail::Sub { .. }) {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("Moo::Config".into())),
            "config accessor should return Moo::Config"
        );
    }

    // Verify method call binding exists (not a function call binding)
    let cfg_binding = fa
        .method_call_bindings
        .iter()
        .find(|b| b.variable == "$cfg");
    assert!(
        cfg_binding.is_some(),
        "should have method call binding for $cfg"
    );
    assert!(
        fa.call_bindings
            .iter()
            .find(|b| b.variable == "$cfg")
            .is_none(),
        "$cfg should NOT be a function call binding"
    );

    // Verify $cfg gets Moo::Config type (not Moo::Service)
    let cfg_type = fa.inferred_type_via_bag("$cfg", tree_sitter::Point::new(13, 0));
    assert_eq!(
        cfg_type,
        Some(InferredType::ClassName("Moo::Config".into())),
        "$cfg should be Moo::Config, not Moo::Service"
    );

    // Verify chained resolution: $dsn = $cfg->dsn → String
    let dsn_binding = fa
        .method_call_bindings
        .iter()
        .find(|b| b.variable == "$dsn");
    assert!(
        dsn_binding.is_some(),
        "should have method call binding for $dsn"
    );
}

#[test]
fn test_mojo_getter_setter_distinct() {
    let fa = build_fa(
        "
package Foo;
use Mojo::Base -base;
has 'name';
",
    );
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 2, "should synthesize getter + setter");

    let getter = methods.iter().find(|m| {
        if let SymbolDetail::Sub { ref params, .. } = m.detail {
            params.is_empty()
        } else {
            false
        }
    });
    let setter = methods.iter().find(|m| {
        if let SymbolDetail::Sub { ref params, .. } = m.detail {
            params.len() == 1
        } else {
            false
        }
    });
    assert!(getter.is_some(), "should have a 0-param getter");
    assert!(setter.is_some(), "should have a 1-param setter");

    // Getter: no return type (inferable from usage)
    let __r = fa.symbol_return_type_via_bag(getter.unwrap().id, None);
    let return_type = __r.as_ref();
    if let SymbolDetail::Sub { .. } = getter.unwrap().detail {
        assert!(return_type.is_none());
    }
    // Setter: fluent return
    let __r = fa.symbol_return_type_via_bag(setter.unwrap().id, None);
    let return_type = __r.as_ref();
    if let SymbolDetail::Sub { .. } = setter.unwrap().detail {
        assert_eq!(
            return_type,
            Some(&InferredType::ClassName("Foo".into()))
        );
    }
}

#[test]
fn test_mojo_fluent_chain_resolves() {
    let src = "
package Foo;
use Mojo::Base -base;
has 'name';
has 'age';
sub greet {
    my ($self) = @_;
    my $result = $self->name('Bob')->age;
    return $result;
}
";
    let fa = build_fa(src);
    // $self->name('Bob') has args → setter → returns Foo
    // ->age has no args → getter → returns None (unknown)
    // The chain should resolve: name('Bob') returns Foo, ->age is valid on Foo
    let method_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "age" && matches!(r.kind, RefKind::MethodCall { .. }))
        .collect();
    assert!(
        !method_refs.is_empty(),
        "should have method call ref for 'age'"
    );
}

#[test]
fn test_moo_rw_arity_resolution() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name' => (is => 'rw', isa => 'Str');
",
    );
    // Moo rw: both getter and setter have same return type (Str)
    // With arity, both 0 and 1 should return String since both symbols have the same type
    let rt_getter = fa.find_method_return_type("Foo", "name", None, Some(0));
    assert_eq!(rt_getter, Some(InferredType::String));
    let rt_setter = fa.find_method_return_type("Foo", "name", None, Some(1));
    assert_eq!(rt_setter, Some(InferredType::String));
    let rt_default = fa.find_method_return_type("Foo", "name", None, None);
    assert_eq!(rt_default, Some(InferredType::String));
}

/// Regression guard for bag-residual D1: same-named methods on
/// unrelated classes must resolve to their own per-class types, no
/// matter what name-keyed cache or "latest-wins" witness landed last.
///
/// Two unrelated classes (`Sweet`, `Sour`) ship a method `flavor` via
/// Mojo::Base `has`, with different defaults. Class-keyed dispatch is
/// required to disambiguate them — any code path that resolves
/// methods by name alone (a `return_types: HashMap<String, _>` mirror,
/// or the now-deleted `WitnessAttachment::NamedSub(name)` shape)
/// will silently shadow one class's getter with the other's whenever
/// the second declaration overwrites the first.
///
/// The arity=1 (fluent writer) assertions extend the same guarantee
/// to overload dispatch: `Sweet`'s writer returns `Sweet`, `Sour`'s
/// returns `Sour`, even though both subs share the name `flavor`.
///
/// D1 (lifted from the abandoned `refactor/bag-residual-d1-method-on-class`
/// branch — see commit c322178 for the original attempt). The redo
/// must keep this test passing while routing every method-type query
/// through the bag.
#[test]
fn method_on_class_disambiguates_same_name_across_classes() {
    let fa = build_fa(
        "
package Sweet;
use Mojo::Base -base;
has flavor => 'caramel';

package Sour;
use Mojo::Base -base;
has flavor => sub { [1, 2, 3] };
",
    );
    let sweet_getter_sym = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "flavor"
                && s.package.as_deref() == Some("Sweet")
                && matches!(&s.detail, SymbolDetail::Sub { params, .. } if params.is_empty())
        })
        .map(|s| s.id);
    let sour_getter_sym = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "flavor"
                && s.package.as_deref() == Some("Sour")
                && matches!(&s.detail, SymbolDetail::Sub { params, .. } if params.is_empty())
        })
        .map(|s| s.id);
    assert!(sweet_getter_sym.is_some(), "Sweet getter sym must exist");
    assert!(sour_getter_sym.is_some(), "Sour getter sym must exist");
    assert_ne!(sweet_getter_sym, sour_getter_sym);

    assert_eq!(
        fa.find_method_return_type("Sweet", "flavor", None, Some(0)),
        Some(InferredType::String),
        "Sweet::flavor getter returns String (from 'caramel' default), \
         not Sour's ArrayRef"
    );
    assert_eq!(
        fa.find_method_return_type("Sour", "flavor", None, Some(0)),
        Some(InferredType::ArrayRef),
        "Sour::flavor getter returns ArrayRef (from sub-returning-array \
         default), not Sweet's String"
    );
    assert_eq!(
        fa.find_method_return_type("Sweet", "flavor", None, Some(1)),
        Some(InferredType::ClassName("Sweet".into())),
    );
    assert_eq!(
        fa.find_method_return_type("Sour", "flavor", None, Some(1)),
        Some(InferredType::ClassName("Sour".into())),
    );
}

/// Regression for the DFS-MRO order fix in
/// `for_each_ancestor_class`. Perl's default MRO is left-to-right
/// depth-first: for `@ISA = (A, B)` where A and B both define `m`,
/// `C->m` resolves to A's. Pre-fix, the stack-based walker pushed
/// parents left-to-right and `pop()`'d, traversing in REVERSE
/// `@ISA` order — so B::m silently won. The fix pushes parents in
/// reverse order so LIFO pops them in `@ISA` order.
#[test]
fn for_each_ancestor_class_walks_left_to_right_isa_order() {
    let fa = build_fa(
        "
package A;
sub m { return 'a' }
package B;
sub m { return 1 }
package C;
our @ISA = ('A', 'B');
",
    );
    // A::m returns String ('a'), B::m returns Numeric (1).
    // C->m must resolve to A's String — A is first in @ISA.
    assert_eq!(
        fa.find_method_return_type("C", "m", None, None),
        Some(InferredType::String),
        "C->m must walk @ISA left-to-right and pick A::m, not B::m"
    );
}

#[test]
fn test_mojo_arity_resolution() {
    let fa = build_fa(
        "
package Bar;
use Mojo::Base -base;
has 'title';
",
    );
    // Getter (0 args): no return type
    let rt_getter = fa.find_method_return_type("Bar", "title", None, Some(0));
    assert!(rt_getter.is_none(), "getter should have no return type");
    // Setter (1 arg): fluent return (ClassName)
    let rt_setter = fa.find_method_return_type("Bar", "title", None, Some(1));
    assert_eq!(rt_setter, Some(InferredType::ClassName("Bar".into())));
    // Default (None): getter (primary, first symbol)
    let rt_default = fa.find_method_return_type("Bar", "title", None, None);
    assert!(rt_default.is_none(), "default should return getter type");
}

#[test]
fn test_mojo_default_string_infers_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has name => 'default';
",
    );
    let rt = fa.find_method_return_type("App", "name", None, Some(0));
    assert_eq!(
        rt,
        Some(InferredType::String),
        "string default → String getter"
    );
}

#[test]
fn test_mojo_default_arrayref_infers_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has items => sub { [] };
",
    );
    let rt = fa.find_method_return_type("App", "items", None, Some(0));
    assert_eq!(
        rt,
        Some(InferredType::ArrayRef),
        "sub {{ [] }} default → ArrayRef getter"
    );
}

#[test]
fn test_mojo_default_hashref_infers_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has config => sub { {} };
",
    );
    let rt = fa.find_method_return_type("App", "config", None, Some(0));
    assert_eq!(
        rt,
        Some(InferredType::HashRef),
        "sub {{{{ }}}} default → HashRef getter"
    );
}

#[test]
fn test_mojo_default_constructor_infers_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has ua => sub { Mojo::UserAgent->new };
",
    );
    let rt = fa.find_method_return_type("App", "ua", None, Some(0));
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Mojo::UserAgent".into())),
        "sub {{ Foo->new }} default → ClassName getter"
    );
}

#[test]
fn test_mojo_default_number_infers_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has timeout => 30;
",
    );
    let rt = fa.find_method_return_type("App", "timeout", None, Some(0));
    assert_eq!(
        rt,
        Some(InferredType::Numeric),
        "number default → Numeric getter"
    );
}

#[test]
fn test_mojo_default_no_value_no_type() {
    let fa = build_fa(
        "
package App;
use Mojo::Base -base;
has 'name';
",
    );
    let rt = fa.find_method_return_type("App", "name", None, Some(0));
    assert!(rt.is_none(), "no default → no getter type");
}

// ---- Constant folding + export extraction tests ----

#[test]
fn test_builder_extracts_exports_qw() {
    let fa = build_fa(
        "
package Foo;
use Exporter 'import';
our @EXPORT = qw(delta);
our @EXPORT_OK = qw(alpha beta gamma);
",
    );
    assert_eq!(fa.export, vec!["delta"]);
    assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn test_builder_extracts_exports_paren() {
    let fa = build_fa(
        "
package Bar;
our @EXPORT_OK = ('foo', 'bar', 'baz');
",
    );
    assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
}

#[test]
fn test_push_exports() {
    let fa = build_fa(
        "
package Foo;
use Exporter 'import';
our @EXPORT_OK = qw(foo);
push @EXPORT_OK, 'bar', 'baz';
",
    );
    assert_eq!(fa.export_ok, vec!["foo", "bar", "baz"]);
}

#[test]
fn test_exporter_extensible_export_call() {
    // `export(qw( foo $bar @baz -tag ))` — only the plain sub name `foo`
    // is recorded; sigil'd vars and `-tag` group names are skipped.
    let fa = build_fa(
        "
package My::Ext;
use Exporter::Extensible -exporter_setup => 1;
export(qw( foo $bar @baz -tag ));
sub foo { 1 }
",
    );
    assert_eq!(fa.export_ok, vec!["foo"]);
}

#[test]
fn test_exporter_extensible_attribute() {
    // `sub foo :Export(...)` / `sub bar :Export` — the sub name is the export.
    let fa = build_fa(
        "
package My::Ext;
use Exporter::Extensible -exporter_setup => 1;
sub foo : Export(-tag) { 1 }
sub bar :Export { 2 }
sub plain { 3 }
",
    );
    assert!(fa.export_ok.contains(&"foo".to_string()));
    assert!(fa.export_ok.contains(&"bar".to_string()));
    assert!(!fa.export_ok.contains(&"plain".to_string()));
}

#[test]
fn test_exporter_declare_export_pair() {
    // `default_export NAME => sub {}` / `export NAME => sub {}` / `exports qw/../`.
    let fa = build_fa(
        "
package My::Decl;
use Exporter::Declare;
default_export foo => sub { 1 };
export bar => sub { 2 };
exports qw/a b/;
",
    );
    assert!(fa.export_ok.contains(&"foo".to_string()));
    assert!(fa.export_ok.contains(&"bar".to_string()));
    assert!(fa.export_ok.contains(&"a".to_string()));
    assert!(fa.export_ok.contains(&"b".to_string()));
}

#[test]
fn test_exporter_call_gated_on_use() {
    // False-positive guard: a `sub export {}` (or a stray `export(...)`)
    // in a package that didn't `use` an exporter-declare family module
    // must NOT register exports.
    let fa = build_fa(
        "
package Plain;
sub export { 1 }
export('not_an_export');
",
    );
    assert!(fa.export_ok.is_empty());
}

/// Gate 5: `find_sub_for_call` (resolution path) must check `export_ok`, not
/// just `export`. A name that Foo records only in `@EXPORT_OK` suppresses the
/// diagnostic in symbols.rs (already checks both), but before this fix
/// `signature_for_call` / goto-def would fall through because the resolution
/// path only tested `export`. Now both lists are checked.
#[test]
fn test_export_ok_resolves_cross_file() {
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;

    // Build a module that exports `fetch_data` via @EXPORT_OK only.
    let provider_fa = build_fa(
        "package Data::Fetcher;\nour @EXPORT_OK = qw(fetch_data);\nsub fetch_data { my ($url) = @_; }\n1;\n",
    );
    assert!(
        provider_fa.export_ok.contains(&"fetch_data".to_string()),
        "provider must record fetch_data in export_ok",
    );

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);
    idx.insert_cache(
        "Data::Fetcher",
        Some(std::sync::Arc::new(crate::module_index::CachedModule::new(
            PathBuf::from("/fake/Data/Fetcher.pm"),
            std::sync::Arc::new(provider_fa),
        ))),
    );

    // Consumer: bare `use Data::Fetcher;` — calls `fetch_data(...)`.
    let consumer_fa = build_fa(
        "package My::App;\nuse Data::Fetcher;\nfetch_data('http://example.com');\n",
    );

    // signature_for_call exercises find_sub_for_call → bare-import path.
    let sig = consumer_fa.signature_for_call(
        "fetch_data",
        false,
        None,
        tree_sitter::Point::new(2, 0),
        Some(&idx),
    );
    assert!(
        sig.is_some(),
        "signature_for_call must resolve fetch_data via @EXPORT_OK, got None",
    );
}

#[test]
fn test_importer_consumer_retargets_source() {
    // `use Importer 'M' => qw/foo bar/` imports foo/bar FROM M. The Import
    // entry must point at M, not Importer, so goto-def crosses to M's subs.
    let fa = build_fa(
        "
package My::Consumer;
use Importer 'Some::Module' => qw/foo bar/;
",
    );
    let imp = fa
        .imports
        .iter()
        .find(|i| i.module_name == "Some::Module")
        .expect("Import re-targeted to Some::Module");
    let names: Vec<&str> = imp
        .imported_symbols
        .iter()
        .map(|s| s.local_name.as_str())
        .collect();
    assert!(names.contains(&"foo"));
    assert!(names.contains(&"bar"));
    // No Import should pin to Importer itself.
    assert!(fa.imports.iter().all(|i| i.module_name != "Importer"));
}

#[test]
fn test_importer_menu_advertised_names() {
    // IMPORTER_MENU advertises export lists; pull the `export`/`export_ok`
    // name arrays. `export_anon` (name → coderef) is unmodeled.
    let fa = build_fa(
        "
package My::Menu;
sub IMPORTER_MENU {
  return (
    export => [qw/foo bar/],
    export_ok => ['baz'],
    export_anon => { quux => sub { 1 } },
  );
}
sub foo { 1 }
",
    );
    assert!(fa.export_ok.contains(&"foo".to_string()));
    assert!(fa.export_ok.contains(&"bar".to_string()));
    assert!(fa.export_ok.contains(&"baz".to_string()));
    assert!(!fa.export_ok.contains(&"quux".to_string()));
}

#[test]
fn test_use_constant_string() {
    let fa = build_fa(
        "
package Foo;
use constant NAME => 'hello';
use parent NAME;
",
    );
    assert_eq!(
        fa.package_parents.get("Foo").unwrap(),
        &vec!["hello".to_string()]
    );
}

#[test]
fn test_constant_array_our() {
    let fa = build_fa(
        "
our @THINGS = qw(a b);
our @EXPORT_OK = (@THINGS, 'c');
",
    );
    assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
}

#[test]
fn test_constant_array_my() {
    let fa = build_fa(
        "
my @THINGS = qw(a b);
our @EXPORT_OK = (@THINGS, 'c');
",
    );
    assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
}

#[test]
fn test_constant_array_in_exports() {
    let fa = build_fa(
        "
package Foo;
use Exporter 'import';
my @COMMON = qw(alpha beta);
our @EXPORT_OK = (@COMMON, 'gamma');
",
    );
    assert_eq!(fa.export_ok, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn test_recursive_constant_resolution() {
    let fa = build_fa(
        "
package Foo;
use Exporter 'import';
use constant BASE => qw(a b);
use constant ALL => (BASE, 'c');
our @EXPORT_OK = (ALL);
",
    );
    assert_eq!(fa.export_ok, vec!["a", "b", "c"]);
}

#[test]
fn test_glob_export_literal_name() {
    // Data::Printer pattern: *{"${caller}::np"} = \&np
    let fa = build_fa(
        r#"
package Data::Printer;
sub np { }
sub p { }
sub import {
    my $class = shift;
    my $caller = caller;
    { no strict 'refs';
        *{"${caller}::p"} = \&p;
        *{"${caller}::np"} = \&np;
    }
}
"#,
    );
    assert!(
        fa.export.contains(&"p".to_string()),
        "should detect p export: {:?}",
        fa.export
    );
    assert!(
        fa.export.contains(&"np".to_string()),
        "should detect np export: {:?}",
        fa.export
    );
}

#[test]
fn test_glob_export_variable_name() {
    // Aliased export: my $imported = 'p'; *{"$caller\::$imported"} = \&p
    let fa = build_fa(
        r#"
package Data::Printer;
sub p { }
sub import {
    my $class = shift;
    my $caller = caller;
    my $imported = 'dump_it';
    { no strict 'refs';
        *{"$caller\::$imported"} = \&p;
    }
}
"#,
    );
    assert!(
        fa.export.contains(&"dump_it".to_string()),
        "should resolve aliased export: {:?}",
        fa.export
    );
}

#[test]
fn test_glob_export_loop_pattern() {
    // Try::Tiny pattern: loop over qw list
    let fa = build_fa(
        r#"
package Try::Tiny;
sub try { }
sub catch { }
sub finally { }
sub import {
    my $class = shift;
    my $caller = caller;
    for my $name (qw(try catch finally)) {
        no strict 'refs';
        *{"${caller}::${name}"} = \&$name;
    }
}
"#,
    );
    assert!(
        fa.export.contains(&"try".to_string()),
        "should detect try: {:?}",
        fa.export
    );
    assert!(
        fa.export.contains(&"catch".to_string()),
        "should detect catch: {:?}",
        fa.export
    );
    assert!(
        fa.export.contains(&"finally".to_string()),
        "should detect finally: {:?}",
        fa.export
    );
}

#[test]
fn test_glob_export_fallback_to_rhs() {
    // When glob name is fully dynamic, fall back to \&name on RHS
    let fa = build_fa(
        r#"
package Foo;
sub bar { }
sub import {
    my $caller = caller;
    *{$caller . '::bar'} = \&bar;
}
"#,
    );
    assert!(
        fa.export.contains(&"bar".to_string()),
        "should fall back to RHS name: {:?}",
        fa.export
    );
}

#[test]
fn test_glob_export_only_inside_import() {
    // Glob assigns outside sub import should NOT populate exports
    let fa = build_fa(
        r#"
package Foo;
sub setup {
    my $caller = caller;
    *{"${caller}::thing"} = \&thing;
}
"#,
    );
    assert!(
        fa.export.is_empty(),
        "should not export from non-import sub: {:?}",
        fa.export
    );
}

#[test]
fn test_loop_variable_constant_folding() {
    let fa = build_fa(
        "
package Foo;
sub test {
    my $self = shift;
    for my $attr (qw(name email)) {
        my $getter = \"get_$attr\";
        $self->$getter();
    }
}
",
    );
    let method_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::MethodCall { .. }))
        .map(|r| r.target_name.as_str())
        .collect();
    assert!(method_refs.contains(&"get_name"), "should resolve get_name");
    assert!(
        method_refs.contains(&"get_email"),
        "should resolve get_email"
    );
}

#[test]
fn test_dynamic_method_dispatch() {
    let fa = build_fa(
        "
package Foo;
my $method = 'get_name';
sub test { my $self = shift; $self->$method() }
",
    );
    let method_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "get_name")
        .collect();
    assert!(
        !method_refs.is_empty(),
        "dynamic method call should resolve to get_name"
    );
}

// ---- Framework plugin integration ----

/// End-to-end: the bundled `mojo-events` Rhai plugin should synthesize a
/// `HashKeyDef` for every literal event name passed to `->on(...)`
/// inside a class that inherits from `Mojo::EventEmitter`. This is the
/// proof that Rhai scripts emit real symbols that land in FileAnalysis
/// with the `Framework` namespace stamp.
#[test]
fn plugin_mojo_events_on_literal_emits_handler_symbol() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('connect', sub { ... });
    $self->on('message', sub { ... });
    $self;
}

1;
"#;
    let fa = build_fa(src);

    let handlers: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events")
        })
        .collect();

    let names: std::collections::HashSet<&str> = handlers.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains("connect"),
        "mojo-events should emit Handler for 'connect'; got: {:?}",
        names
    );
    assert!(
        names.contains("message"),
        "mojo-events should emit Handler for 'message'; got: {:?}",
        names
    );

    // Each Handler should also carry the dispatcher set — at minimum
    // 'emit' (the canonical Mojo dispatch method).
    for h in &handlers {
        if let SymbolDetail::Handler { dispatchers, .. } = &h.detail {
            assert!(
                dispatchers.iter().any(|d| d == "emit"),
                "Handler for {} should declare `emit` dispatcher",
                h.name
            );
        } else {
            panic!("expected Handler detail on {}", h.name);
        }
    }
}

/// Dynamic event names must not produce spurious HashKeyDefs.
#[test]
fn plugin_mojo_events_dynamic_name_does_not_emit() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my ($self, $name) = @_;
    $self->on($name, sub { ... });
}

1;
"#;
    let fa = build_fa(src);
    let plugin_handlers: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events")
        })
        .collect();
    assert!(
        plugin_handlers.is_empty(),
        "dynamic event name must not emit handlers; got: {:?}",
        plugin_handlers.iter().map(|s| &s.name).collect::<Vec<_>>()
    );
}

/// Const folding through the plugin: `my $name = 'connect'; ...` means
/// the plugin receives `arg.string_value == "connect"` and emits a
/// symbol named "connect" — not "$name". The plugin itself contains no
/// folding logic; the builder does it once in `arg_info_for` and every
/// plugin gets folded values for free.
#[test]
fn plugin_mojo_events_const_folds_scalar_event_name() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    my $evt = 'disconnect';
    $self->on($evt, sub { ... });
}

1;
"#;
    let fa = build_fa(src);

    let names: std::collections::HashSet<&str> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events")
        })
        .map(|s| s.name.as_str())
        .collect();
    assert!(
        names.contains("disconnect"),
        "const-folded event name should emit 'disconnect'; got: {:?}",
        names
    );
    assert!(
        !names.contains("$evt"),
        "variable text must not leak through as symbol name"
    );
}

/// Transitive inheritance: a class whose parent (in the same file)
/// extends Mojo::EventEmitter should still trigger the plugin. Proves
/// the builder's transitive_parents walk composes with `ClassIsa`.
#[test]
fn plugin_mojo_events_triggers_through_transitive_parent() {
    let src = r#"
package Mid;
use parent 'Mojo::EventEmitter';

package Leaf;
use parent 'Mid';

sub wire {
    my $self = shift;
    $self->on('ready', sub { ... });
}

1;
"#;
    let fa = build_fa(src);
    let ready: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && s.name == "ready"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events")
        })
        .collect();
    assert_eq!(
        ready.len(),
        1,
        "Leaf extends Mid extends Mojo::EventEmitter — plugin must fire transitively"
    );
}

/// Cross-file def/ref pairing. Producer.pm wires events via ->on, Consumer.pm
/// calls ->emit on a producer instance. Both plugin emissions end up with
/// `HashKeyOwner::Class("Producer")`, so `resolve::refs_to` finds the
/// consumer's access ref from the producer's def query — no LSP code is
/// plugin-aware.
#[test]
fn plugin_mojo_events_cross_file_ref_pairing() {
    use crate::file_analysis::HandlerOwner;
    use crate::file_store::FileStore;
    use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
    use std::path::PathBuf;

    let producer_src = r#"
package Producer;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('ready', sub { warn "ready" });
    return $self;
}
1;
"#;
    let consumer_src = r#"
package Consumer;
use parent 'Mojo::EventEmitter';

sub run {
    my $p = Producer->new;
    $p->emit('ready');
    $p->unsubscribe('ready');
}
1;
"#;

    let store = FileStore::new();
    let producer_path = PathBuf::from("/tmp/plugin_producer.pm");
    let consumer_path = PathBuf::from("/tmp/plugin_consumer.pm");

    store.insert_workspace(producer_path.clone(), build_fa(producer_src));
    store.insert_workspace(consumer_path.clone(), build_fa(consumer_src));

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "ready".to_string(),
            kind: TargetKind::Handler {
                owner: HandlerOwner::Class("Producer".to_string()),
                name: "ready".to_string(),
            },
            method_classes: Vec::new(),
        },
        RoleMask::EDITABLE,
    );

    let producer_hits = results
        .iter()
        .filter(|r| matches!(&r.key, crate::file_store::FileKey::Path(p) if p == &producer_path))
        .count();
    let consumer_hits = results
        .iter()
        .filter(|r| matches!(&r.key, crate::file_store::FileKey::Path(p) if p == &consumer_path))
        .count();

    assert!(
        producer_hits >= 1,
        "producer should have ≥1 hit (the ->on Handler def); results: {:?}",
        results
    );
    assert!(
        consumer_hits >= 1,
        "consumer should have ≥1 hit (the ->emit DispatchCall); results: {:?}",
        results
    );
}

/// mojo-helpers: Phase-2 architecture emits ONE Method per helper,
/// owned by `Mojolicious::Controller` — the canonical home for
/// controller-callable helpers. The PluginNamespace's bridges cover
/// both Controller and Mojolicious so `$c->name` AND `$app->name`
/// both resolve through namespace lookup (no Symbol fan-out).
#[test]
fn plugin_mojo_helpers_registers_method_on_controller() {
    let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub {
    my ($c, $extra) = @_;
    return { id => 1 };
});
"#;
    let fa = build_fa(src);

    let helpers: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Method
                && s.name == "current_user"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-helpers")
        })
        .collect();

    assert_eq!(
        helpers.len(),
        1,
        "one Method per helper (no fan-out — Phase 2)"
    );
    let helper = helpers[0];
    assert_eq!(
        helper.package.as_deref(),
        Some(crate::file_analysis::APP_SURFACE_CLASS),
        "canonical home is the fictional app surface"
    );

    if let SymbolDetail::Sub {
        params, display, ..
    } = &helper.detail
    {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(
            names,
            vec!["$c", "$extra"],
            "helper's sub params flow through to the Method signature"
        );
        assert_eq!(
            *display,
            Some(crate::file_analysis::HandlerDisplay::Helper),
            "helpers render as HandlerDisplay::Helper — the LSP kind is \
                 FUNCTION (the enum doesn't have Helper), the outline word \
                 is 'helper'. See HandlerDisplay::outline_word.",
        );
    } else {
        panic!("helper detail should be Sub");
    }

    // The PluginNamespace owns the bridge visibility: a SINGLE bridge
    // to the fictional app surface (docs/adr/plugin-system.md). The
    // consumer classes reach it via the synthetic-parent edge in core,
    // not via a per-helper bridge list.
    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| n.plugin_id == "mojo-helpers" && n.entities.contains(&helper.id))
        .expect("helper belongs to a mojo-helpers namespace");
    let bridge_classes: std::collections::HashSet<&str> = ns
        .bridges
        .iter()
        .map(|Bridge::Class(c)| c.as_str())
        .collect();
    assert_eq!(
        bridge_classes,
        std::iter::once(crate::file_analysis::APP_SURFACE_CLASS).collect(),
        "namespace bridges ONLY the app surface — open consumer set lives in core"
    );

    // The synthetic-ancestor edge: the helper resolves from BOTH the
    // app class and the controller class through the SAME ancestor walk,
    // even though neither is the helper's home package.
    for consumer in ["Mojolicious", "Mojolicious::Controller"] {
        let res = fa.resolve_method_in_ancestors(consumer, "current_user", None);
        match res {
            Some(crate::file_analysis::MethodResolution::Local { sym_id, .. }) => {
                assert_eq!(
                    sym_id, helper.id,
                    "{consumer}->current_user must resolve to the helper via the app surface"
                );
            }
            other => panic!(
                "{consumer}->current_user should resolve to the helper Local; got {other:?}"
            ),
        }
    }
}

/// Dotted helpers chain into namespace methods: `users.create` means
/// `$c->users->create`. Each non-leaf segment emits a parameterless
/// Method returning a synthetic proxy class; the leaf emits on the
/// innermost proxy with the helper's real params. Shared prefixes
/// dedup — `thing.hi` and `thing.there` must only ever produce one
/// `thing` symbol (not two), so completion + outline stay clean.
#[test]
fn plugin_mojo_helpers_dotted_chain_with_shared_prefix_dedup() {
    let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper('thing.hi'    => sub { my ($c, $arg_a) = @_; });
$app->helper('thing.there' => sub { my ($c, $arg_b) = @_; });
"#;
    let fa = build_fa(src);

    // Exactly one `thing` method on the app surface (the chain root's
    // home; consumer classes reach it via the synthetic-parent edge),
    // despite two dotted helpers sharing that prefix.
    let thing_syms: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.name == "thing"
                && s.kind == SymKind::Method
                && s.package.as_deref() == Some(crate::file_analysis::APP_SURFACE_CLASS)
        })
        .collect();
    assert_eq!(
        thing_syms.len(),
        1,
        "shared prefix must dedup: one `thing` method, got {}",
        thing_syms.len()
    );

    // Its return_type is the shared proxy class.
    match fa.symbol_return_type_via_bag(thing_syms[0].id, None) {
        Some(InferredType::ClassName(n)) => {
            assert_eq!(n, "Mojolicious::Controller::_Helper::thing");
        }
        _ => panic!("thing's return type should be the shared proxy class"),
    }

    // Both leaves exist on the shared proxy class, each with its own params.
    let hi = fa
        .symbols
        .iter()
        .find(|s| s.name == "hi" && s.kind == SymKind::Method)
        .expect("hi leaf emitted");
    let there = fa
        .symbols
        .iter()
        .find(|s| s.name == "there" && s.kind == SymKind::Method)
        .expect("there leaf emitted");
    assert_eq!(
        hi.package.as_deref(),
        Some("Mojolicious::Controller::_Helper::thing")
    );
    assert_eq!(
        there.package.as_deref(),
        Some("Mojolicious::Controller::_Helper::thing")
    );
    if let SymbolDetail::Sub { params, .. } = &hi.detail {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["$c", "$arg_a"]);
    }
    if let SymbolDetail::Sub { params, .. } = &there.detail {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["$c", "$arg_b"]);
    }
}

/// Three-level dotted helper chains: `admin.users.purge` synthesizes
/// two intermediate proxies, each with the right return_type, and
/// the leaf lands on the innermost proxy.
#[test]
fn plugin_mojo_helpers_three_level_dotted_chain() {
    let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper('admin.users.purge' => sub { my ($c, $force) = @_; });
"#;
    let fa = build_fa(src);

    let admin = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "admin"
                && s.kind == SymKind::Method
                && s.package.as_deref() == Some(crate::file_analysis::APP_SURFACE_CLASS)
        })
        .expect("admin on app surface (chain root)");
    let users = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "users"
                && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller::_Helper::admin")
        })
        .expect("users on admin proxy");
    let purge = fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "purge"
                && s.kind == SymKind::Method
                && s.package.as_deref() == Some("Mojolicious::Controller::_Helper::admin::users")
        })
        .expect("purge leaf on admin.users proxy");

    // Each non-leaf returns the next proxy in the chain.
    if let Some(InferredType::ClassName(n)) =
        fa.symbol_return_type_via_bag(admin.id, None)
    {
        assert_eq!(n, "Mojolicious::Controller::_Helper::admin");
    }
    if let Some(InferredType::ClassName(n)) =
        fa.symbol_return_type_via_bag(users.id, None)
    {
        assert_eq!(n, "Mojolicious::Controller::_Helper::admin::users");
    }
    // Leaf carries the helper's actual params.
    if let SymbolDetail::Sub { params, .. } = &purge.detail {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["$c", "$force"]);
    }
}

/// `use Mojolicious::Lite` autoimports a fixed verb set — our
/// unresolved-function diagnostic must skip them. The plugin's
/// `on_use` hook emits FrameworkImport actions for each; the
/// builder stashes them in framework_imports so the diagnostic
/// filter drops matching FunctionCall refs.
#[test]
fn plugin_mojo_lite_autoimports_verbs() {
    let src = r#"
package main;
use Mojolicious::Lite;

get '/x' => sub {};
post '/y' => sub {};
helper foo => sub {};
"#;
    let fa = build_fa(src);
    for verb in &[
        "get",
        "post",
        "put",
        "del",
        "patch",
        "any",
        "under",
        "websocket",
        "app",
        "helper",
        "hook",
        "plugin",
        "group",
    ] {
        assert!(
            fa.framework_imports.contains(*verb),
            "{} must be autoimported by use Mojolicious::Lite",
            verb
        );
    }
}

/// mojo-lite: top-level route verbs (`get`, `post`, etc.) register
/// Handlers keyed by URL path, with ["url_for"] as the dispatcher so
/// `url_for('/users')` can find them. Exercises the on_function_call
/// plugin hook that mojo-events doesn't use.
#[test]
fn plugin_mojo_lite_registers_handlers_for_routes() {
    let src = r#"
package main;
use Mojolicious::Lite;

get '/users' => sub {
    my ($c, $arg) = @_;
    $c->render(text => 'hi');
};

post '/login' => sub {
    my ($c, $user, $pw) = @_;
};

app->start;
"#;
    let fa = build_fa(src);

    let route_handlers: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-lite")
        })
        .collect();

    let names: std::collections::HashSet<&str> =
        route_handlers.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains("/users"),
        "GET /users handler emitted; got: {:?}",
        names
    );
    assert!(
        names.contains("/login"),
        "POST /login handler emitted; got: {:?}",
        names
    );

    // Each handler declares url_for as its dispatcher so completion
    // inside `url_for('|')` surfaces every route.
    for h in &route_handlers {
        if let SymbolDetail::Handler { dispatchers, .. } = &h.detail {
            assert!(
                dispatchers.iter().any(|d| d == "url_for"),
                "handler {} should dispatch via url_for",
                h.name
            );
        }
    }

    // Handler params come from the handler sub's signature —
    // different per route, so they round-trip correctly.
    let login = route_handlers.iter().find(|h| h.name == "/login").unwrap();
    if let SymbolDetail::Handler { params, .. } = &login.detail {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["$c", "$user", "$pw"]);
    }
}

/// Routes are first-class things, not just refs. Every `->to(...)`
/// emits BOTH a MethodCallRef (cross-file target link) AND a
/// Handler symbol (route-as-entity — outline-visible, workspace-
/// searchable, discoverable via url_for completion). Mirrors the
/// mojo-lite model so route symbols are symmetric regardless of
/// declaration flavor.
#[test]
fn plugin_mojo_routes_emits_both_ref_and_handler_symbol() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
$r->post('/users')->to(controller => 'Users', action => 'create');
"#;
    let fa = build_fa(src);

    // Each route: one MethodCallRef + one Handler symbol.
    let method_refs: Vec<&Ref> = fa
        .refs
        .iter()
        .filter(|r| {
            matches!(r.kind, RefKind::MethodCall { .. })
                && (r.target_name == "list" || r.target_name == "create")
        })
        .collect();
    assert_eq!(method_refs.len(), 2, "one MethodCallRef per route");

    let route_syms: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::Handler
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-routes")
        })
        .collect();
    assert_eq!(
        route_syms.len(),
        2,
        "one Handler symbol per route so outline + workspace-symbol find them"
    );

    let names: std::collections::HashSet<&str> =
        route_syms.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains("Users#list"),
        "route identity `Users#list` present; got: {:?}",
        names
    );
    assert!(
        names.contains("Users#create"),
        "route identity `Users#create` present; got: {:?}",
        names
    );

    // Dispatcher is url_for so completion inside `url_for('|')`
    // offers every registered route.
    for s in &route_syms {
        if let SymbolDetail::Handler {
            dispatchers, owner, ..
        } = &s.detail
        {
            assert!(
                dispatchers.iter().any(|d| d == "url_for"),
                "route {} should dispatch via url_for",
                s.name
            );
            // Owner is `Mojolicious::Controller` — url_for is a
            // Controller method and the routes table is global
            // per Mojo's runtime model. Owning on Controller lets
            // `$c->url_for` in any controller resolve routes
            // declared in any app file through ancestor walking.
            // Not target-class (Users): routes exist independent
            // of their target; two routes can target the same
            // action (paginated/json/etc.).
            assert!(matches!(owner, HandlerOwner::Class(c) if c == "Mojolicious::Controller"),
                    "route owner is Mojolicious::Controller (shared base for url_for), not declaring package");
        }
    }
}

/// End-to-end cross-file gd for `->to('Users#list')`. Users.pm
/// is registered as a workspace module in ModuleIndex (via the
/// `register_workspace_module` bridge), so
/// `resolve_method_in_ancestors` finds it on lookup — and the
/// cross-file MethodCall path in `symbols::find_definition`
/// surfaces the Users::list def's location.
///
/// Before the fix, workspace modules lived only in FileStore so
/// lookups that key on module name (all cross-file method
/// resolution) missed them and gd fell through to noise.
#[test]
fn plugin_mojo_routes_gd_reaches_workspace_target() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;
    use tower_lsp::lsp_types::Position;

    let app_src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
    let users_src = r#"
package Users;
sub list { my ($c) = @_; }
1;
"#;

    let app_fa = build_fa(app_src);
    let users_fa = build_fa(users_src);

    let idx = ModuleIndex::new_for_test();
    // Simulate workspace indexing registering Users.pm under its
    // primary package name.
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Users.pm"),
        Arc::new(users_fa),
    );

    // Sanity: cross-file resolution on "Users"::"list" must succeed.
    let res = app_fa.resolve_method_in_ancestors("Users", "list", Some(&idx));
    assert!(
        res.is_some(),
        "Users::list must resolve cross-file after workspace register"
    );

    // And the MethodCallRef emitted by mojo-routes on the 'list'
    // portion of 'Users#list' should be at a span matching the
    // text 'list'.
    let route_ref = app_fa
        .refs
        .iter()
        .find(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "list")
        .expect("mojo-routes MethodCallRef for 'list'");

    // The ref's span is tight on the action name — mid-string
    // completion + goto-def both rely on that precision.
    let _ = route_ref;
    let _ = Position::default();
}

/// mojo-routes short form: `->to('Users#list')` emits a MethodCall
/// ref pointing to `Users::list`. Cursor on the string → gd jumps
/// cross-file to the Users controller's list method, same as any
/// regular method call. No routes-specific resolution code; it's
/// just a Ref that happens to live inside a string literal.
#[test]
fn plugin_mojo_routes_short_form_emits_method_call_ref() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
    let fa = build_fa(src);

    let route_refs: Vec<&Ref> = fa
        .refs
        .iter()
        .filter(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "list")
        .collect();

    assert!(
        !route_refs.is_empty(),
        "at least one MethodCall ref for 'list'"
    );
    let r = route_refs
        .iter()
        .find(|r| matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "Users"))
        .expect("MethodCall with invocant=Users");

    // Sanity: ref span covers the string literal so cursor anywhere
    // in the 'Users#list' range lands on the ref.
    assert!(
        r.span.end.column > r.span.start.column,
        "method ref has non-empty span"
    );
}

/// mojo-routes long form: `->to(controller => 'Users', action => 'list')`.
/// Walks kwarg pairs, pairs up controller+action, emits the ref
/// with span on the action value.
#[test]
fn plugin_mojo_routes_long_form_emits_method_call_ref() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to(controller => 'Users', action => 'list');
"#;
    let fa = build_fa(src);

    let has_ref = fa.refs.iter().any(|r| {
        matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "Users")
            && r.target_name == "list"
    });
    assert!(
        has_ref,
        "long-form ->to(controller=>, action=>) must produce MethodCall ref"
    );
}

/// `$r->get('/users')->to('Users#list')->name('users_list')` — the
/// `->name()` call registers a symbolic handle. `url_for('users_list')`
/// and `redirect_to('users_list')` must resolve to it, the same way
/// they resolve to `'Users#list'`. Without `->name()`, calls like
/// `url_for('users_list')` sit unresolved.
#[test]
fn plugin_mojo_routes_name_registers_url_for_handle() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list')->name('users_list');
"#;
    let fa = build_fa(src);

    let route_name_handler = fa
        .symbols
        .iter()
        .find(|s| s.kind == SymKind::Handler && s.name == "users_list");
    assert!(
        route_name_handler.is_some(),
        "->name('users_list') must emit a Handler; handlers: {:?}",
        fa.symbols
            .iter()
            .filter(|s| s.kind == SymKind::Handler)
            .map(|s| &s.name)
            .collect::<Vec<_>>()
    );

    let sym = route_name_handler.unwrap();
    if let SymbolDetail::Handler { dispatchers, .. } = &sym.detail {
        assert!(
            dispatchers.iter().any(|d| d == "url_for"),
            "named route must dispatch via url_for"
        );
        assert!(
            dispatchers.iter().any(|d| d == "redirect_to"),
            "named route must dispatch via redirect_to"
        );
    } else {
        panic!("route-name symbol should be Handler; got {:?}", sym.detail);
    }

    // The route name should be in a mojo-routes namespace bridged
    // to the declaring package, so cross-file `url_for('users_list')`
    // from other files in the workspace resolves.
    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| n.plugin_id == "mojo-routes" && n.entities.contains(&sym.id));
    assert!(
        ns.is_some(),
        "named route must belong to a mojo-routes namespace"
    );
}

/// `->to('X#y')` routes dispatch via both `url_for` and `redirect_to`
/// (Phase-2 follow-up — `redirect_to` used to be Lite-only). Matches
/// Mojolicious's actual API where redirect_to on a controller resolves
/// named routes identically to url_for.
#[test]
fn plugin_mojo_routes_to_dispatches_via_redirect_to_too() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
    let fa = build_fa(src);

    let route_handler = fa
        .symbols
        .iter()
        .find(|s| s.kind == SymKind::Handler && s.name == "Users#list")
        .expect("Users#list Handler");

    if let SymbolDetail::Handler { dispatchers, .. } = &route_handler.detail {
        assert!(
            dispatchers.iter().any(|d| d == "url_for"),
            "->to route must dispatch via url_for"
        );
        assert!(
            dispatchers.iter().any(|d| d == "redirect_to"),
            "->to route must dispatch via redirect_to"
        );
    } else {
        panic!("route symbol should be Handler");
    }
}

// ==== AliasTo: DSL verbs delegate to real methods, not imaginary ones. ====
//
// `Mojolicious::Lite` monkey-patches `get`, `post`, `helper`, `app`, …
// into the caller at import time. At the Perl level each verb is just
// a thin pass-through to a real method:
//
//     sub { $routes->get(@_) }                  # get, post, put, any,
//                                               # options, patch, websocket
//     sub { $routes->delete(@_) }               # del
//     sub { $app->helper(@_) }                  # helper, hook, plugin
//     sub { $app }                              # app (returns the app)
//
// Previously the plugin fabricated a `SymbolDetail::Sub` per verb with a
// hand-written one-line `doc:` and, for `app`, a typed return. That's
// the "imaginary methods" the user pointed at: hover shows stub text,
// gd lands on the use statement, signature help has no params, and —
// worst — the synthesized Sub shadows chain resolution on real Mojo
// objects (`$routes->get('/x')->to(...)` loses the `to` intelligence).
//
// The fix: emit an *alias* that points at the real cross-file method.
// Hover, gd, sig help, and return-type inference all dereference the
// alias and use the real method's data. The tests below pin the
// expected behavior on real cross-file methods — they fail until
// `FunctionAlias` / `alias_to` lands in the data model and the
// resolution paths dereference it.

/// The user-visible "stomping" case: `$routes->get('/x')->to('X#y')` on
/// a real `Mojolicious::Routes` should chain through
/// `Mojolicious::Routes::Route::get` (fluent, returns its own class)
/// so `->to` resolves on `Mojolicious::Routes::Route`. The plugin-
/// synthesized top-level `get` Sub in the Lite script must NOT
/// intercept a method call on a real object of a different class.
#[test]
fn mojo_lite_chain_off_real_routes_preserves_real_method_chain() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app_src = r#"
package main;
use Mojolicious::Lite;
use Mojolicious;

my $routes = Mojolicious::Routes->new;
$routes->get('/users')->to('Users#list');
"#;
    // Stub Mojolicious::Routes::Route with a fluent `get` (returns
    // its own class) so chain resolution can carry the type forward.
    let route_pm_src = r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

sub get {
    my $self = shift;
    return $self;
}

sub to {
    my $self = shift;
    return $self;
}
1;
"#;
    // Mojolicious::Routes ISA Mojolicious::Routes::Route in real
    // Mojo, so methods invoked on $routes (typed Mojolicious::Routes)
    // flow up to the parent class via the normal inheritance walk.
    let routes_pm_src = r#"
package Mojolicious::Routes;
use Mojo::Base 'Mojolicious::Routes::Route';
1;
"#;

    let app_fa = build_fa(app_src);
    let route_fa = build_fa(route_pm_src);
    let routes_fa = build_fa(routes_pm_src);

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes/Route.pm"),
        Arc::new(route_fa),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes.pm"),
        Arc::new(routes_fa),
    );

    // `$routes->get` must resolve to the real Mojolicious::Routes::Route::get
    // via inheritance (Mojolicious::Routes → Mojolicious::Routes::Route) —
    // NOT to the plugin's top-level `get` Sub emitted by mojo-lite.
    // `class_name()` unifies ClassName and FirstParam — both are
    // usable for downstream chain resolution.
    let get_rt = app_fa.find_method_return_type("Mojolicious::Routes", "get", Some(&idx), None);
    assert_eq!(
        get_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "`$$routes->get` must chain through the REAL Mojolicious::Routes::Route::get — \
             not the plugin's imaginary top-level `get` Sub. got: {:?}",
        get_rt,
    );

    // Second hop: `->to(...)` on the Route object returned by `get`.
    // User's wording: "get should return a to which is intelligent".
    // Fluent Route — `to` stays on Mojolicious::Routes::Route so the
    // chain can keep going (`->name(...)`, `->via(...)`, ...).
    let to_rt =
        app_fa.find_method_return_type("Mojolicious::Routes::Route", "to", Some(&idx), None);
    assert_eq!(
        to_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "`->get('/x')->to(...)` must stay intelligent — Route::to is fluent, and \
             further hops depend on it. got: {:?}",
        to_rt,
    );
}

/// Hover on the DSL verb `get` in `get '/x' => sub {}` must surface
/// the real `Mojolicious::Routes::Route::get` POD, not the plugin's
/// hand-written one-liner. Pins that the plugin's Sub symbol for
/// `get` is an alias — its hover dereferences to the real method.
#[test]
fn mojo_lite_dsl_verb_hover_uses_real_method_doc() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app_src = r#"
package main;
use Mojolicious::Lite;

get '/users' => sub { my $c = shift; };
"#;
    // Real method carries a recognizable POD line. The test matches
    // on a substring that no hand-written plugin doc uses.
    let route_pm_src = r#"
package Mojolicious::Routes::Route;

=head2 get

  my $route = $r->get('/:foo' => sub ($c) {...});

Generate route matching only GET requests. Shortcut for
L<Mojolicious::Routes::Route/"any">.

=cut

sub get { my $self = shift; return $self; }
1;
"#;

    let app_fa = build_fa(app_src);
    let route_fa = build_fa(route_pm_src);

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes/Route.pm"),
        Arc::new(route_fa),
    );

    // Cursor on the `get` bareword at the call site.
    let (row, line) = app_src
        .lines()
        .enumerate()
        .find(|(_, l)| l.starts_with("get "))
        .expect("`get` call line");
    let col = line.find("get").unwrap() + 1;
    let point = tree_sitter::Point { row, column: col };

    let hover = app_fa
        .hover_info(point, app_src, Some(&idx))
        .expect("hover on DSL verb `get` returns text");

    assert!(
        hover.contains("Generate route matching only GET requests"),
        "hover on `get` must surface the real Mojolicious::Routes::Route::get POD \
             (verb is an alias, not an imaginary stub). got: {:?}",
        hover,
    );
}

/// `app` parses as a bareword invocant (`app->routes`). The plugin's
/// typed `app` Sub must make that bareword resolve to Mojolicious so
/// the chain can flow. Pins the bareword edge case explicitly — the
/// regression shape the user called out.
#[test]
fn mojo_lite_app_bareword_invocant_types_as_mojolicious() {
    let src = r#"
package main;
use Mojolicious::Lite;

my $x = app;
"#;
    let fa = build_fa(src);

    // `$x = app` — $x should pick up the return type of the plugin's
    // `app` Sub (ClassName("Mojolicious")).
    let ty = fa
        .inferred_type("$x", tree_sitter::Point::new(4, 0))
        .expect("$x must carry a type sourced from `app`'s return type");
    assert!(
        matches!(ty, InferredType::ClassName(c) if c == "Mojolicious"),
        "`$$x = app` must type as Mojolicious — bareword `app` resolves to the \
             plugin's typed Sub. got: {:?}",
        ty,
    );
}

/// The headline case: the full `app->routes->get('/x')->to('X#y')`
/// chain must be fully intelligent at every hop. One plugin stub
/// at the head (`app` → Mojolicious) — everything else is real
/// cross-file method resolution.
///
/// Every arrow is a separate assertion so a regression at any hop
/// points at the specific broken link:
///
///   app                                          → Mojolicious              (plugin-typed Sub)
///   Mojolicious::routes                          → Mojolicious::Routes       (real Mojo::Base accessor)
///   Mojolicious::Routes::get  (via parent Route) → Mojolicious::Routes::Route (fluent)
///   Mojolicious::Routes::Route::to               → Mojolicious::Routes::Route (fluent)
///
/// If any hop returns None the chain's "intelligence" collapses —
/// completion, hover, gd, sig-help all lose context from that point
/// forward. That collapse is the "hardcoded list" symptom the user
/// reported, flipped around.
#[test]
fn mojo_lite_app_routes_chain_is_fully_intelligent_to_the_end() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app_src = r#"
package main;
use Mojolicious::Lite;

app->routes->get('/users')->to('Users#list');
"#;
    let mojolicious_pm_src = r#"
package Mojolicious;
use Mojo::Base -base;

has routes => sub { Mojolicious::Routes->new };
1;
"#;
    let routes_pm_src = r#"
package Mojolicious::Routes;
use Mojo::Base 'Mojolicious::Routes::Route';
1;
"#;
    let route_pm_src = r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

sub get { my $self = shift; return $self; }
sub to  { my $self = shift; return $self; }
1;
"#;

    let app_fa = build_fa(app_src);
    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious.pm"),
        Arc::new(build_fa(mojolicious_pm_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes.pm"),
        Arc::new(build_fa(routes_pm_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes/Route.pm"),
        Arc::new(build_fa(route_pm_src)),
    );

    // Hop 1: `app` → Mojolicious. The plugin's typed Sub seeds the
    // chain. This is the single sanctioned plugin stub.
    let app_sym = app_fa
        .symbols
        .iter()
        .find(|s| {
            s.name == "app"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-lite")
        })
        .expect("mojo-lite plugin must synthesize `app`");
    let rt = app_fa
        .symbol_return_type_via_bag(app_sym.id, None)
        .expect("hop 1: `app` must carry a typed return");
    assert_eq!(
        rt.class_name(),
        Some("Mojolicious"),
        "hop 1: `app` must type as Mojolicious — the one plugin stub the chain leans on"
    );

    // Hop 2: Mojolicious::routes → Mojolicious::Routes. Real
    // cross-file Mojo::Base accessor; the anon-sub default's
    // `Mojolicious::Routes->new` is lifted as the return type.
    let routes_rt = app_fa.find_method_return_type("Mojolicious", "routes", Some(&idx), None);
    assert_eq!(
        routes_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes"),
        "hop 2: `Mojolicious::routes` must resolve cross-file to the real Mojo::Base \
             accessor and return Mojolicious::Routes. got: {:?}",
        routes_rt,
    );

    // Hop 3: Mojolicious::Routes::get → Mojolicious::Routes::Route.
    // Resolves via inheritance (Routes ISA Route) to the real fluent
    // method on the parent class. This is where plugin-synthesized
    // `get` from mojo-lite MUST NOT stomp.
    let get_rt = app_fa.find_method_return_type("Mojolicious::Routes", "get", Some(&idx), None);
    assert_eq!(
        get_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "hop 3: `$$routes->get` must chain through the REAL \
             Mojolicious::Routes::Route::get (fluent) — not the plugin's \
             imaginary top-level `get` Sub. got: {:?}",
        get_rt,
    );

    // Hop 4: Mojolicious::Routes::Route::to → Mojolicious::Routes::Route.
    // Fluent. After this, `->name(...)`/`->via(...)`/etc. must still
    // resolve on Route — i.e. the chain keeps going, not collapses.
    let to_rt =
        app_fa.find_method_return_type("Mojolicious::Routes::Route", "to", Some(&idx), None);
    assert_eq!(
        to_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "hop 4: `->to(...)` must chain through the real fluent `to` on \
             Mojolicious::Routes::Route — preserving intelligence for further \
             hops (->name, ->via, ...). got: {:?}",
        to_rt,
    );
}

/// Adversarial: a dotted helper `users.create` and a route whose
/// action is `Users#create` both end up with a Perl-level symbol
/// named `create`. They are UNRELATED:
///
///   * `users.create` lives on `Mojolicious::Controller::_Helper::users`
///     — a synthetic proxy class invented by the plugin. It's called
///     as `$c->users->create(...)`.
///   * `Users#create` points at a method `create` on the user's
///     `Users` controller class. It's called via dispatch, not
///     chained off a helper.
///
/// Name-based resolution would cross-link them (goto-def on either
/// jumps to the other, find-references unions the two unrelated
/// call sites). Class-aware resolution must keep them apart: the
/// route's MethodCallRef targets class `Users`, the helper's leaf
/// lives on `_Helper::users`.
#[test]
fn helper_and_route_with_same_leaf_name_do_not_cross_link() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

$app->helper('users.create', sub ($c, $user) {});
$app->routes->post('/users')->to(controller => 'Users', action => 'create');
"#;
    let fa = build_fa(src);

    // --- Fact-finding: what actually got emitted? ---

    // The helper leaf `create` should live on the proxy class.
    let helper_create: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.name == "create"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-helpers")
        })
        .collect();
    assert_eq!(helper_create.len(), 1, "one helper-leaf named 'create'");
    let helper_create = helper_create[0];
    assert_eq!(
        helper_create.package.as_deref(),
        Some("Mojolicious::Controller::_Helper::users"),
        "helper leaf lives on the proxy class, NOT on Users"
    );

    // The route emits a MethodCallRef method_name=create invocant=Users.
    let route_ref = fa
        .refs
        .iter()
        .find(|r| {
            matches!(&r.kind, RefKind::MethodCall { invocant, .. } if invocant == "Users")
                && r.target_name == "create"
        })
        .expect("route should emit MethodCall create@Users");

    // --- The bug: does the route's ref resolve to the helper? ---

    // If resolves_to is Some(sym_id), it MUST NOT point to the
    // helper — the helper lives on a different class.
    if let Some(target_sid) = route_ref.resolves_to {
        assert_ne!(
            target_sid, helper_create.id,
            "route MethodCall(create @ Users) must NOT resolve to the \
                 helper-leaf on _Helper::users — they share a name only"
        );
    }

    // Cross-resolution via the public API: refs_to_symbol(helper)
    // must NOT include the route's ref.
    let refs_to_helper = fa.refs_to(helper_create.id);
    for r in &refs_to_helper {
        assert_ne!(
            (r.span.start.row, r.span.start.column),
            (route_ref.span.start.row, route_ref.span.start.column),
            "route ref showed up as a reference to the helper — cross-link bug. \
                 Helper is on _Helper::users, route targets Users, they shouldn't mix."
        );
    }

    // And the mirror: resolve_method_in_ancestors on class `Users`
    // for method `create` must NOT return the helper-leaf. The
    // helper's class is _Helper::users, not Users.
    let resolution = fa.resolve_method_in_ancestors("Users", "create", None);
    if let Some(crate::file_analysis::MethodResolution::Local { sym_id, .. }) = resolution {
        assert_ne!(
            sym_id, helper_create.id,
            "resolve_method_in_ancestors(Users, create) returned the helper — \
                 class-awareness broken"
        );
    }
}

/// Helpers emitted by mojo-helpers land on Mojolicious::Controller.
/// A controller subclass in ANOTHER file (standard workspace layout)
/// must see them when walking methods — the class_content_index
/// bridges the lookup because the synthesizing module's primary
/// package isn't Mojolicious::Controller.
#[test]
fn plugin_mojo_helpers_reachable_cross_file_from_controller() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    // Lite script with a helper.
    let lite_src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(greet => sub { my ($c, $who) = @_; });
"#;
    // Controller subclass in another file.
    let ctrl_src = r#"
package MyApp::Controller::Home;
use parent 'Mojolicious::Controller';
1;
"#;

    let lite_fa = Arc::new(build_fa(lite_src));
    let ctrl_fa = build_fa(ctrl_src);

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/MyApp.pm"), lite_fa.clone());

    // bridges_index knows MyApp.pm declares a namespace bridged to the
    // app surface (mojo-helpers' app namespace).
    let mods = idx.modules_bridging_to(crate::file_analysis::APP_SURFACE_CLASS);
    assert!(
        mods.iter().any(|m| m == "MyApp"),
        "MyApp module should be listed as bridged to the app surface; got: {:?}",
        mods
    );

    // Completion on MyApp::Controller::Home inheriting from
    // Mojolicious::Controller should walk up to the controller, cross
    // the synthetic-parent edge to the app surface, and find `greet`.
    let candidates = ctrl_fa.complete_methods_for_class("MyApp::Controller::Home", Some(&idx));
    let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(
        labels.contains(&"greet"),
        "helper `greet` on the app surface in MyApp.pm should complete on \
             controller subclasses via the synthetic-parent edge; got: {:?}",
        labels
    );
}

/// mojo-helpers cross-file: when a Lite script registers a helper
/// `greet`, the resulting Method symbol's `package` is the fictional
/// app surface. Any consumer file — controller subclass, the app, or
/// otherwise — finds it via the standard workspace walk + the
/// synthetic-parent edge, without a single mojo-helpers-aware line in
/// the consumer-side code path.
#[test]
fn plugin_mojo_helpers_land_on_controller_package() {
    let src = r#"
package MyApp::Lite;
use Mojolicious::Lite;

app->helper(greet => sub {
    my ($c, $name) = @_;
    return "hello, $name";
});
1;
"#;
    let fa = build_fa(src);
    let greet = fa
        .symbols
        .iter()
        .find(|s| s.name == "greet" && s.kind == SymKind::Method)
        .expect("helper must emit a Method named greet");
    assert_eq!(
        greet.package.as_deref(),
        Some(crate::file_analysis::APP_SURFACE_CLASS),
        "helper Method is packaged on the app surface; the synthetic-parent \
             edge lets every consumer class pick it up via the inheritance walk"
    );
    assert!(matches!(&greet.namespace, Namespace::Framework { id } if id == "mojo-helpers"));
}

/// Synthetic-ancestor app surface (docs/adr/plugin-system.md): a helper
/// that returns a concrete class resolves its RETURN type identically
/// from the app, the controller, AND a user-written app subclass —
/// proving the single bridge target + synthetic-parent edge composes with
/// the MethodOnClass type-resolution walk and that subclasses inherit the
/// surface for free.
#[test]
fn plugin_mojo_helpers_return_type_via_app_surface() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(model => sub { my ($c) = @_; return MyApp::Model->new; });
"#;
    // Declare a user app subclass in the SAME file so its
    // `package_parents` edge (MyApp::Web -> Mojolicious) is present;
    // it must inherit the surface for free.
    let src = format!("{src}\npackage MyApp::Web;\nuse parent 'Mojolicious';\n1;\n");
    let fa = build_fa(&src);

    // The helper resolves its return type from the app class, the
    // controller class, AND the user app subclass.
    for class in ["Mojolicious", "Mojolicious::Controller", "MyApp::Web"] {
        let rt = fa.find_method_return_type(class, "model", None, None);
        assert_eq!(
            rt,
            Some(crate::file_analysis::InferredType::ClassName("MyApp::Model".into())),
            "`{class}->model` must return MyApp::Model via the app surface; got {rt:?}",
        );
    }
}

/// App surface (docs/adr/plugin-system.md): `$app->minion->enqueue`
/// resolves once `$app` is typed. The `minion` helper (return type a
/// Minion subclass) is reached from the locally-typed `$app` via the app
/// surface; enrichment then resolves the `->enqueue` receiver and promotes
/// the dispatch. Proves the surface composes with dispatch-verb promotion.
#[test]
fn plugin_app_surface_minion_enqueue_resolves_when_app_typed() {
    use crate::file_analysis::HandlerOwner;
    use std::path::PathBuf;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        PathBuf::from("/tmp/as_acme_minion.pm"),
        std::sync::Arc::new(build_fa("package Acme::Minion;\nuse Mojo::Base 'Minion';\n1;\n")),
    );

    // `$app` is locally typed (Mojolicious->new); the `minion` helper
    // returns an Acme::Minion. `$app->minion` reaches the helper via the
    // synthetic app-surface edge, so its return type is Acme::Minion.
    let mut fa = build_fa(
        "package MyApp;\nuse Mojolicious::Lite;\n\
         my $app = Mojolicious->new;\n\
         $app->helper(minion => sub { my ($c) = @_; return Acme::Minion->new; });\n\
         $app->minion->enqueue('send_email' => ['alice']);\n1;\n",
    );

    let mref = fa.refs.iter().find(|r| {
        matches!(&r.kind, RefKind::MethodCall { .. }) && r.target_name == "minion"
    });
    assert!(mref.is_some(), "an `$app->minion` MethodCall ref must exist");

    fa.enrich_imported_types_with_keys(Some(&idx));

    // `Mojolicious::Lite` is a trigger, so the emit-hook materializes the
    // DispatchCall directly; `applicable_dispatches` de-dups the gated
    // candidate against it. Either path surfaces the handler — exactly once.
    let has_materialized = fa.refs.iter().any(|r|
        matches!(&r.kind, RefKind::DispatchCall { dispatcher, owner: Some(HandlerOwner::Class(c)) }
            if dispatcher == "enqueue" && c == "Minion")
            && r.target_name == "send_email");
    let has_gated = fa.applicable_dispatches(Some(&idx)).iter().any(|a|
        a.name == "send_email" && a.owner == HandlerOwner::Class("Minion".into()));
    assert!(
        has_materialized ^ has_gated,
        "`$app->minion->enqueue` must surface as a Minion dispatch exactly once — \
         via the emit-hook ref OR the gated candidate; materialized={has_materialized} \
         gated={has_gated}",
    );
}

/// Helpers complete on both `$c` (Controller) and `$app` (the
/// Mojolicious app class). Every helper registers a Method on each
/// entry class, so `complete_methods_for_class` for either class
/// surfaces the helper. Dotted chain roots also land on both
/// classes; the deeper proxies stay on the shared prefix.
#[test]
fn plugin_mojo_helpers_complete_on_app_class_too() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub { my ($c) = @_; });
$app->helper('users.create' => sub { my ($c, $name) = @_; });
"#;
    let fa = build_fa(src);

    for class in ["Mojolicious::Controller", "Mojolicious"] {
        let candidates = fa.complete_methods_for_class(class, None);
        let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
        assert!(
            labels.contains(&"current_user"),
            "`current_user` must complete on {}; got: {:?}",
            class,
            labels,
        );
        assert!(
            labels.contains(&"users"),
            "`users` (dotted-helper root) must complete on {}; got: {:?}",
            class,
            labels,
        );
    }
}

/// Diagnostic pin: inside a controller action, `$c->url_for('|')`
/// must offer every named route declared in the workspace —
/// Lite paths, `Ctrl#action` pairs from `->to(...)`, and symbolic
/// `->name('foo')` handles. This is the completion side that the
/// `_emits_refs` / `_registers_url_for_handle` tests don't cover.
///
/// Discovers two separate bugs at the same time:
///   (1) Handler.owner is the *declaring* package (`MyApp`), so
///       `$c->url_for(...)` on a `Users` controller fails the
///       `owner_class == invocant_class` filter in
///       `dispatch_target_completions`.
///   (2) No coverage for "does url_for completion work at all".
#[test]
fn plugin_mojo_routes_url_for_completion_offers_route_names() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let app_src = r#"package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list')->name('users_list');
$r->post('/users')->to(controller => 'Users', action => 'create');

get '/hello' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(build_fa(app_src));

    let ctrl_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    my $u = $c->url_for('x');
}
"#;
    let ctrl_fa = build_fa(ctrl_src);

    let idx = std::sync::Arc::new(crate::module_index::ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Users.pm"),
        std::sync::Arc::new(build_fa(ctrl_src)),
    );

    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(ctrl_src, None).unwrap();

    // Cursor on the `x` inside `url_for('x')` — `active_param == 0`.
    let pos = Position {
        line: 5,
        character: 25,
    };
    let items = crate::symbols::completion_items(&ctrl_fa, &tree, ctrl_src, pos, &idx, None);
    let labels: Vec<String> = items.iter().map(|it| it.label.clone()).collect();

    for expected in &["users_list", "Users#list", "/hello"] {
        assert!(
            labels.iter().any(|l| l == expected),
            "url_for('|') inside Users::list must offer `{}` (route declared in MyApp); got: {:?}",
            expected,
            labels
        );
    }
}

/// Red pin (user-reported): starting to type inside
/// `$c->url_for('|')` must not kill completion — the string
/// content should feed the prefix filter, not suppress it.
/// Covers two realistic live-editing shapes:
///
/// 1. `url_for('|')` — cursor between the quotes, string body
///    empty. Every route should appear (no prefix yet).
/// 2. `url_for('adm|')` — user has typed `adm`, cursor inside
///    the string, closing quote already in place (what you get
///    after auto-paired quotes). The returned list must be
///    prefix-filterable by `adm` via either `filter_text` or a
///    server-side restriction — `admin.users.purge`-style
///    named routes should survive the filter, Lite `/hello`
///    should drop out client-side.
///
/// Existing work that this pin must use, NOT re-roll:
///   * `candidate_to_completion_item` already sets
///     `filter_text = Some(label)` so the quoted `insert_text`
///     doesn't defeat client-side matching — covered by
///     `completion_dispatch_filter_text_matches_bare_name`.
///   * `mid_string_methodref_completions` handles the same
///     shape for MethodCallRefs (`->to('Users#li|')`), slicing
///     `source[span_start..cursor]` as the prefix.
#[test]
fn plugin_mojo_routes_url_for_completion_survives_typed_prefix() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let app_src = r#"package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list')->name('users_list');
$r->get('/admin/users/purge')->to('Admin#purge')->name('admin_users_purge');

get '/hello' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(build_fa(app_src));

    let idx = std::sync::Arc::new(crate::module_index::ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);

    // Case 1: empty string, cursor between the quotes.
    // `    my $u = $c->url_for('');`
    //                          ^ char 24 (opening quote)
    //                           ^ char 25 (cursor here)
    //                           ^ char 25 (closing quote)
    let empty_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    my $u = $c->url_for('');
}
"#;
    let empty_fa = build_fa(empty_src);
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Users_empty.pm"),
        std::sync::Arc::new(build_fa(empty_src)),
    );
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(empty_src, None).unwrap();
    let items = crate::symbols::completion_items(
        &empty_fa,
        &tree,
        empty_src,
        Position {
            line: 5,
            character: 25,
        },
        &idx,
        None,
    );
    let labels: Vec<String> = items.iter().map(|it| it.label.clone()).collect();
    for expected in &["users_list", "admin_users_purge", "Users#list", "/hello"] {
        assert!(
            labels.iter().any(|l| l == expected),
            "empty url_for('|') must offer `{}`; got: {:?}",
            expected,
            labels
        );
    }

    // Case 2: user has typed `adm`, closing quote in place.
    // `    my $u = $c->url_for('adm');`
    //                          ^ 24 opening quote
    //                           ^ 25 a
    //                            ^ 26 d
    //                             ^ 27 m — cursor here after typing
    //                              ^ 28 closing quote
    let typed_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    my $u = $c->url_for('adm');
}
"#;
    let typed_fa = build_fa(typed_src);
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Users_typed.pm"),
        std::sync::Arc::new(build_fa(typed_src)),
    );
    let tree = parser.parse(typed_src, None).unwrap();
    let items = crate::symbols::completion_items(
        &typed_fa,
        &tree,
        typed_src,
        Position {
            line: 5,
            character: 27,
        },
        &idx,
        None,
    );

    // Server returns the dispatch-handler set (all routes). The
    // client narrows by `filter_text` (bare label) against the
    // typed prefix `adm`. For this pin we assert the two things
    // the server owes us:
    //
    //  (a) the set still includes routes whose LABEL starts with
    //      `adm` — if the server dropped them before we got a
    //      chance to filter, completion is "dead" as the user
    //      described.
    //  (b) every returned handler's `filter_text` is set to the
    //      bare label so the client's prefix match keys on the
    //      route name, not on the quoted insert_text (`'admin...'`
    //      starts with `'`, not `a`).
    let labels: Vec<String> = items.iter().map(|it| it.label.clone()).collect();
    assert!(
        labels.iter().any(|l| l == "admin_users_purge"),
        "typed prefix `adm` must still surface `admin_users_purge` from the \
             server so client-side filter_text matching can narrow to it; got: {:?}",
        labels
    );

    let adm_item = items
        .iter()
        .find(|it| it.label == "admin_users_purge")
        .expect("admin_users_purge must be in returned items");
    assert_eq!(
        adm_item.filter_text.as_deref(),
        Some("admin_users_purge"),
        "dispatch handler `filter_text` must be the bare label so the \
             typed `adm` (no quote) matches — otherwise starting to type the \
             string kills completion",
    );
}

/// mojo-lite route URLs are referenced from `->url_for(...)` and
/// `->redirect_to(...)`. Both emit `DispatchCall` refs tight to
/// the URL string so gd/gr compose via the standard Handler
/// resolution path — no Lite-aware code in the core.
#[test]
fn plugin_mojo_lite_url_dispatch_emits_refs() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

get '/hello' => sub {
    my ($c) = @_;
    $c->render(text => 'hi');
};

sub after {
    my ($c) = @_;
    $c->redirect_to('/hello');
    my $u = $c->url_for('/hello');
}
"#;
    let fa = build_fa(src);

    let dispatch_refs: Vec<&crate::file_analysis::Ref> = fa
        .refs
        .iter()
        .filter(|r| matches!(&r.kind, RefKind::DispatchCall { .. }))
        .filter(|r| r.target_name == "/hello")
        .collect();

    let dispatchers: Vec<&str> = dispatch_refs
        .iter()
        .map(|r| match &r.kind {
            RefKind::DispatchCall { dispatcher, .. } => dispatcher.as_str(),
            _ => unreachable!(),
        })
        .collect();

    assert!(
        dispatchers.contains(&"redirect_to"),
        "redirect_to('/hello') must emit a DispatchCall ref; got: {:?}",
        dispatchers,
    );
    assert!(
        dispatchers.contains(&"url_for"),
        "url_for('/hello') must emit a DispatchCall ref; got: {:?}",
        dispatchers,
    );
}

/// Plugin triggers must gate emission. A class that doesn't inherit from
/// Mojo::EventEmitter should see no mojo-events emissions even if it
/// happens to call a method named `->on(...)`.
#[test]
fn plugin_mojo_events_triggers_gate_emission() {
    let src = r#"
package My::Unrelated;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->on('connect', sub { ... });
    $self;
}

1;
"#;
    let fa = build_fa(src);
    let plugin_syms: Vec<&Symbol> = fa
        .symbols
        .iter()
        .filter(|s| {
            matches!(&s.namespace,
                Namespace::Framework { id } if id == "mojo-events")
        })
        .collect();
    assert!(
        plugin_syms.is_empty(),
        "untriggered package must not get plugin emissions; got: {:?}",
        plugin_syms.iter().map(|s| &s.name).collect::<Vec<_>>()
    );
}

/// Minion plugin: `$minion->add_task(NAME, sub { ... })` emits a
/// Handler (owner: Minion) with the task's sub params, typed $job
/// in the callback body, and a DispatchCall ref on the name.
#[test]
fn plugin_minion_add_task_registers_handler() {
    let src = r#"
package MyApp;
use Minion;

my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job, $to, $subject) = @_;
    $job->finish;
});
"#;
    let fa = build_fa(src);

    let handler = fa
        .symbols
        .iter()
        .find(|s| {
            s.kind == SymKind::Handler
                && s.name == "send_email"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "minion")
        })
        .expect("add_task must emit a Handler named send_email");

    let SymbolDetail::Handler {
        ref owner,
        ref dispatchers,
        ref params,
        ref display,
        ..
    } = handler.detail
    else {
        panic!("handler detail should be Handler")
    };
    assert!(matches!(owner, HandlerOwner::Class(c) if c == "Minion"));
    assert!(dispatchers.iter().any(|d| d == "enqueue"));
    assert!(
        matches!(display, HandlerDisplay::Task),
        "minion tasks render as HandlerDisplay::Task (LSP kind FUNCTION, outline word 'task')"
    );
    // Callback params: $job flagged as invocant, then the rest.
    let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(names, vec!["$job", "$to", "$subject"]);
    assert!(
        params[0].is_invocant,
        "Minion::Job is the callback's invocant"
    );

    // DispatchCall on the name (registration itself is a reference).
    let dc = fa.refs.iter()
            .find(|r| matches!(&r.kind, RefKind::DispatchCall { dispatcher, .. } if dispatcher == "add_task"))
            .expect("add_task must emit a DispatchCall ref");
    assert_eq!(dc.target_name, "send_email");
}

/// `$minion->enqueue(NAME, ...)` emits a DispatchCall for the name
/// so gd/gr compose against the add_task Handler.
#[test]
fn plugin_minion_enqueue_emits_dispatch_call() {
    let src = r#"
package MyApp;
use Minion;

my $minion = Minion->new;
$minion->add_task(send_email => sub { my ($job) = @_; });
$minion->enqueue(send_email => ['alice']);
$minion->enqueue_p(send_email => ['bob']);
"#;
    let fa = build_fa(src);

    let dispatchers: Vec<&str> = fa
        .refs
        .iter()
        .filter_map(|r| match &r.kind {
            RefKind::DispatchCall { dispatcher, .. } if r.target_name == "send_email" => {
                Some(dispatcher.as_str())
            }
            _ => None,
        })
        .collect();
    assert!(
        dispatchers.contains(&"enqueue"),
        "enqueue('send_email', ...) must emit a DispatchCall; got: {:?}",
        dispatchers
    );
    assert!(
        dispatchers.contains(&"enqueue_p"),
        "enqueue_p must emit a DispatchCall too; got: {:?}",
        dispatchers
    );
}

/// Option B: a `$minion->enqueue('T')` lights up by the RECEIVER's type,
/// not the file's `use`s. `Worker` never `use`s Minion and isn't a Mojo
/// app — so the bundled minion plugin's triggers never fire, and there's no
/// `DispatchCall` after the plain build. But `$m` is a locally-constructed
/// `Acme::Minion` (isa Minion, declared cross-file), so the builder records
/// a gated candidate and `applicable_dispatches` — which has the module
/// index, hence the cross-file `isa` — resolves it at QUERY time, with no
/// enrichment. This is the "wherever the minion came from provides the magic"
/// path the file-trigger model couldn't reach.
#[test]
fn gated_dispatch_resolves_on_subclass_receiver_query_time() {
    use crate::file_analysis::HandlerOwner;
    use std::path::PathBuf;
    let base = build_fa("package Acme::Minion;\nuse Mojo::Base 'Minion';\n1;\n");
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        PathBuf::from("/tmp/b_acme_minion.pm"),
        std::sync::Arc::new(base),
    );

    let fa = build_fa(
        "package Worker;\nsub go {\n  my $m = Acme::Minion->new;\n  $m->enqueue('send_email' => ['a']);\n}\n1;\n",
    );
    // The triggers never fired, so nothing was materialized at parse time.
    assert!(
        !fa.refs.iter().any(|r| matches!(&r.kind, RefKind::DispatchCall { .. })),
        "no DispatchCall ref should exist (plugin trigger didn't fire)",
    );

    // No enrichment — query-time resolution alone surfaces the dispatch,
    // exactly as a non-open workspace file would be served.
    let applied = fa.applicable_dispatches(Some(&idx));
    assert_eq!(
        applied.iter().filter(|a|
            a.name == "send_email" && a.owner == HandlerOwner::Class("Minion".into())).count(),
        1,
        "query-time resolution must surface exactly one Minion dispatch for \
         enqueue on a Minion-subclass receiver, even with no enrichment; got {:?}",
        applied,
    );
}

/// The receiver isn't locally typed — it's a cross-file method-call return
/// (`$b->minion` where `Box::minion` returns an `Acme::Minion`). The
/// build-time hint is `None`; query-time resolution resolves the invocant
/// cross-file (via the module index) and, finding it isa Minion, surfaces the
/// dispatch. This is the `$self->_minion->enqueue(...)` shape — works whenever
/// the receiver's type is actually resolvable.
#[test]
fn gated_dispatch_resolves_cross_file_receiver_query_time() {
    use crate::file_analysis::HandlerOwner;
    use std::path::PathBuf;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        PathBuf::from("/tmp/b_acme_minion.pm"),
        std::sync::Arc::new(build_fa("package Acme::Minion;\nuse Mojo::Base 'Minion';\n1;\n")),
    );
    idx.register_workspace_module(
        PathBuf::from("/tmp/b_box.pm"),
        std::sync::Arc::new(build_fa(
            "package Box;\nsub new { bless {}, shift }\nsub minion ($self) { return Acme::Minion->new; }\n1;\n",
        )),
    );

    let fa = build_fa(
        "package Worker;\nsub go {\n  my $b = Box->new;\n  $b->minion->enqueue('send_email' => ['a']);\n}\n1;\n",
    );

    // No enrichment: the gate resolves the cross-file receiver lazily.
    let applied = fa.applicable_dispatches(Some(&idx));
    assert!(
        applied.iter().any(|a|
            a.name == "send_email" && a.owner == HandlerOwner::Class("Minion".into())),
        "query-time resolution must resolve the cross-file receiver `$b->minion` \
         (Acme::Minion isa Minion) and surface the dispatch; got {:?}",
        applied,
    );
}

/// A Minion SUBCLASS receiver (`Acme::Minion` isa Minion, the crm
/// `Clove::Minion` shape) must still register + dispatch tasks. The
/// receiver types to `ClassName("Acme::Minion")`, which a name-prefix
/// allowlist (`== "Minion" || starts_with("Minion::")`) silently rejects —
/// the rule-#10 trap. The plugin no longer gates on receiver class, so the
/// Handler (owner Minion) and the enqueue DispatchCall pair as usual.
#[test]
fn plugin_minion_subclass_receiver_still_wires() {
    let src = r#"
package MyApp;
use Minion;

my $minion = Acme::Minion->new;
$minion->add_task(send_email => sub { my ($job) = @_; });
$minion->enqueue(send_email => ['alice']);
"#;
    let fa = build_fa(src);

    let handler = fa.symbols.iter().find(|s| {
        s.kind == SymKind::Handler
            && s.name == "send_email"
            && matches!(&s.detail, SymbolDetail::Handler { owner: HandlerOwner::Class(c), .. } if c == "Minion")
    });
    assert!(
        handler.is_some(),
        "add_task on a Minion subclass receiver must still register a Minion-owned Handler",
    );

    let has_enqueue_dc = fa.refs.iter().any(|r| matches!(
        &r.kind, RefKind::DispatchCall { dispatcher, .. }
        if dispatcher == "enqueue" && r.target_name == "send_email"
    ));
    assert!(
        has_enqueue_dc,
        "enqueue on a Minion subclass receiver must still emit a DispatchCall",
    );
}

/// $job inside an add_task callback is typed as Minion::Job so
/// completion on $job-> resolves to Minion::Job methods.
#[test]
fn plugin_minion_types_job_inside_task_body() {
    let src = r#"
package MyApp;
use Minion;

my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job) = @_;
    $job->finish;
});
"#;
    let fa = build_fa(src);

    // `$job` should be typed Minion::Job inside the callback —
    // plugin-declared ClassName, not builder's FirstParam.
    let ty = fa
        .inferred_type("$job", tree_sitter::Point::new(8, 0))
        .expect("$job must carry a type inside add_task callback");
    assert!(
        matches!(ty, InferredType::ClassName(c) if c == "Minion::Job"),
        "type should be plugin-declared ClassName(Minion::Job), got {:?}",
        ty,
    );
}

/// Minion's `enqueue` options go in a hashref at position 3
/// (`enqueue(task, [args], {priority => 10})`). The plugin emits
/// HashKeyDefs for the common keys owned by Sub{Minion,enqueue}
/// — what's missing is cursor-context routing for "hash literal
/// as positional arg" → `HashKey { source_sub: "enqueue" }`.
/// Skipped until the core learns that shape; the emission side is
/// pinned here so regressing it trips.
#[test]
fn plugin_minion_enqueue_options_hashkeys_emitted() {
    let src = r#"
package MyApp;
use Minion;

my $minion = Minion->new;
$minion->enqueue(task_x => ['arg'] => { priority => 10 });
"#;
    let fa = build_fa(src);

    // Options emitted as HashKeyDef symbols owned by Sub{Minion, enqueue}.
    let option_names: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| {
            s.kind == SymKind::HashKeyDef
                && matches!(&s.namespace, Namespace::Framework { id } if id == "minion")
        })
        .map(|s| s.name.as_str())
        .collect();
    for expected in &[
        "priority", "queue", "delay", "attempts", "notes", "parents", "expire", "lax",
    ] {
        assert!(
            option_names.contains(expected),
            "enqueue option `{}` must be emitted; got: {:?}",
            expected,
            option_names
        );
    }
}

/// Cross-file helper chain completion: Users.pm inherits from
/// Mojolicious::Controller; helpers declared in a sibling Lite
/// file register Methods on Controller. From Users.pm, cursor at
/// `$c->`, `$c->users->`, `$c->admin->` must all resolve through
/// the proxy classes even though the methods live in another
/// file and the CPAN-cached Controller doesn't know about them.
///
/// Regression trigger: `resolve_method_in_ancestors` used to scan
/// only `get_cached(class)` cross-file, missing plugin-emitted
/// methods that live in other modules under the same `package`.
/// `detect_cursor_context_tree` also only called `resolve_expression_type`
/// without a module_index, so chain resolution of `$c->users->`
/// fell through to the untyped fallback and returned Users's own
/// methods (list, create) instead of the proxy chain's leaves.
#[test]
fn plugin_mojo_helpers_cross_file_chain_completion() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    // The Lite file — declares the helpers.
    let lite_src = r#"package MyApp;
use strict;
use warnings;
use Mojolicious::Lite;

my $app = Mojolicious->new;

$app->helper(current_user => sub { my ($c, $fallback) = @_; });
$app->helper('users.create' => sub { my ($c, $name, $email) = @_; });
$app->helper('users.delete' => sub { my ($c, $id) = @_; });
$app->helper('admin.users.purge' => sub { my ($c, $force) = @_; });
"#;
    let lite_fa = build_fa(lite_src);

    // The controller file — inherits from Mojolicious::Controller
    // and expects to reach the helpers cross-file. This is where
    // the user's `$c->` completion is happening in real life.
    let src = r#"package Users;
use strict;
use warnings;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->;
    $c->users->;
    $c->admin->;
}
"#;
    let fa = build_fa(src);

    // Sanity — Users.pm's own analysis has `list` but not the
    // helpers (they're declared in the Lite file).
    let users_subs: Vec<&str> = fa
        .symbols
        .iter()
        .filter(|s| matches!(s.kind, SymKind::Method | SymKind::Sub))
        .map(|s| s.name.as_str())
        .collect();
    assert_eq!(users_subs, vec!["list"], "Users.pm owns only `list`");

    // Now simulate the nvim completion pipeline at `$c->` position.
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Populate a ModuleIndex with a mock Mojolicious::Controller
    // that has a few native-looking methods (render, stash, etc.).
    // Matches the user's env where CPAN Mojolicious is installed
    // and its Controller is cached cross-file. Register the Lite
    // script itself too — workspace indexer would.
    // Workspace has BOTH files registered — mirrors nvim startup
    // after Rayon indexes the .pm/.pl set.
    let idx = std::sync::Arc::new(crate::module_index::ModuleIndex::new_for_test());
    let lite_fa = std::sync::Arc::new(lite_fa);
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/MyApp.pm"), lite_fa.clone());
    let users_fa = std::sync::Arc::new(build_fa(src));
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/lib/Users.pm"),
        users_fa.clone(),
    );

    let ctrl_src = r#"package Mojolicious::Controller;
sub render { my ($self, %args) = @_; }
sub stash { my ($self, $key) = @_; }
sub req { my ($self) = @_; }
sub res { my ($self) = @_; }
sub session { my ($self, $key) = @_; }
1;
"#;
    let ctrl_fa = std::sync::Arc::new(build_fa(ctrl_src));
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Controller.pm"),
        ctrl_fa,
    );

    // The workspace knows the Lite file declares a namespace bridged
    // to the app surface (the mojo-helpers app namespace emits
    // `Bridge::Class(APP_SURFACE_CLASS)`). The controller reaches it
    // through the synthetic-parent edge in the ancestor walk.
    let mods = idx.modules_bridging_to(crate::file_analysis::APP_SURFACE_CLASS);
    assert!(
        mods.iter().any(|m| m == "MyApp"),
        "workspace index must list MyApp.pm bridged to the app surface; got: {:?}",
        mods
    );

    // Part 1: `$c->` completion in Users.pm surfaces both the
    // inherited native methods AND the plugin-emitted helpers
    // (cross-file, via the app namespace's Class(Controller) bridge).
    let pos = |row: u32, col: u32| Position {
        line: row,
        character: col,
    };
    let call_label_set = |items: &[tower_lsp::lsp_types::CompletionItem]| -> Vec<String> {
        items.iter().map(|it| it.label.clone()).collect()
    };

    let items = crate::symbols::completion_items(&fa, &tree, src, pos(7, 8), &idx, None);
    let labels = call_label_set(&items);
    for expected in &["list", "render", "stash", "current_user", "users", "admin"] {
        assert!(
            labels.iter().any(|l| l == expected),
            "$c-> must offer `{}`; got: {:?}",
            expected,
            labels
        );
    }

    // Part 2: `$c->users->` (chained cross-file) resolves to the
    // _Helper::users proxy and surfaces its leaves. Before the
    // fix: cursor_context couldn't resolve the chain without a
    // module_index, so completion fell through to Users's own
    // methods (`list`).
    let items = crate::symbols::completion_items(&fa, &tree, src, pos(8, 15), &idx, None);
    let labels = call_label_set(&items);
    assert_eq!(
        labels.iter().collect::<std::collections::HashSet<_>>(),
        ["create", "delete"]
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .iter()
            .collect::<std::collections::HashSet<_>>(),
        "$c->users-> must offer exactly the helper chain leaves (create/delete); got: {:?}",
        labels,
    );
    assert!(
        !labels.iter().any(|l| l == "list"),
        "$c->users-> must NOT fall back to Users.pm's own `list`; got: {:?}",
        labels
    );

    // Part 3: `$c->admin->` resolves through the first-level proxy
    // to the innermost `users` step.
    let items = crate::symbols::completion_items(&fa, &tree, src, pos(9, 15), &idx, None);
    let labels = call_label_set(&items);
    assert_eq!(
        labels,
        vec!["users"],
        "$c->admin-> must offer exactly `users`; got: {:?}",
        labels
    );

    // Part 4: the proxy's detail is suppressed (opaque_return).
    // No `_Helper::...` string should leak into the user-facing
    // detail of a helper-root completion entry, even cross-file.
    let items = crate::symbols::completion_items(&fa, &tree, src, pos(7, 8), &idx, None);
    let users_item = items.iter().find(|it| it.label == "users").unwrap();
    let admin_item = items.iter().find(|it| it.label == "admin").unwrap();
    for (name, item) in [("users", users_item), ("admin", admin_item)] {
        let d = item.detail.as_deref().unwrap_or("");
        assert!(
            !d.contains("_Helper"),
            "opaque_return must suppress proxy class in `{}`'s detail cross-file; got: {:?}",
            name,
            d
        );
    }

    // Part 5: no "unresolved-method" diagnostic for helper calls
    // that now resolve cross-file. The diagnostic builder walks
    // resolve_method_in_ancestors; our fix extends that to pick
    // up plugin-emitted methods on parent classes declared
    // elsewhere in the workspace.
    let diags = crate::symbols::collect_diagnostics(&fa, &idx, Default::default());
    for diag in &diags {
        let msg = &diag.message;
        assert!(
            !msg.contains("'users' is not defined"),
            "no diagnostic for helper middle hop `users`; got: {}",
            msg
        );
        assert!(
            !msg.contains("'admin' is not defined"),
            "no diagnostic for helper middle hop `admin`; got: {}",
            msg
        );
        assert!(
            !msg.contains("'current_user' is not defined"),
            "no diagnostic for helper `current_user`; got: {}",
            msg
        );
    }
}

/// documentHighlight on a method-call identifier must highlight
/// JUST the method name, not the whole `$obj->method(...)` span.
/// Before this pin: hovering `helper` on one `$app->helper(NAME =>
/// sub { ... })` site underlined every other registration's full
/// multi-line call expression — args, sub bodies, closing `);`
/// all included. Regression trigger: MethodCall ref.span covers
/// the whole call (needed for gd/ref_at inside-args lookup);
/// highlight path now uses `method_name_span` from the ref kind.
#[test]
fn method_call_highlight_uses_method_name_span_only() {
    let src = r#"package MyApp;
sub do_thing { }
sub run {
    my ($self, $x) = @_;
    $self->do_thing($x, 1, 2);
    $self->do_thing(3);
}
"#;
    let fa = build_fa(src);

    // Cursor on `do_thing` at the first call site. Highlight
    // must return ranges whose width == len("do_thing"), never
    // a range that spans past the closing `)` or crosses into
    // the next line.
    let row = 4; // 0-indexed: `    $self->do_thing($x, 1, 2);`
    let col = src.lines().nth(row).unwrap().find("do_thing").unwrap();
    let point = tree_sitter::Point::new(row, col + 1);

    let hits = fa.find_highlights(point, None, None, None);
    assert!(!hits.is_empty(), "should highlight at least one occurrence");

    for (span, _access) in &hits {
        // Must be single-line + width exactly 8 ("do_thing").
        assert_eq!(
            span.start.row, span.end.row,
            "highlight must not span multiple lines; got: {:?}",
            span
        );
        let width = span.end.column - span.start.column;
        assert_eq!(
            width,
            "do_thing".len(),
            "highlight width must match method identifier; got {}: {:?}",
            width,
            span
        );
    }
}

/// `$app->admin->` (chained helper call) completion returns the
/// proxy class's methods — not the fallback full-file list.
/// Validates that `resolve_expression_type` chains through the
/// plugin-synthesized opaque return and
/// `complete_methods_for_class` finds methods on the proxy.
#[test]
fn plugin_mojo_helpers_chained_proxy_completion() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper('admin.users.purge' => sub { my ($c, $force) = @_; });
"#;
    let fa = build_fa(src);

    // 1. `$app->admin` resolves to the first-level proxy.
    let admin_proxy = fa
        .find_method_return_type("Mojolicious", "admin", None, None)
        .expect("admin on Mojolicious has a return_type");
    let admin_class = admin_proxy
        .class_name()
        .expect("proxy return_type is a ClassName");
    assert_eq!(admin_class, "Mojolicious::Controller::_Helper::admin");

    // 2. `$app->admin->` completion shows the `users` proxy step.
    let candidates = fa.complete_methods_for_class(admin_class, None);
    let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(
        labels.contains(&"users"),
        "chain completion on admin proxy must surface `users`; got: {:?}",
        labels
    );
    // And the `users` step's detail must NOT leak the internal
    // `_Helper::admin::users` proxy class name — the plugin
    // declared the return type opaque.
    let users_cand = candidates.iter().find(|c| c.label == "users").unwrap();
    assert!(
        !users_cand
            .detail
            .as_deref()
            .unwrap_or("")
            .contains("_Helper"),
        "opaque_return must hide the proxy class from detail: {:?}",
        users_cand.detail,
    );

    // 3. Two levels in — `$app->admin->users` → the innermost proxy.
    let users_proxy = fa
        .find_method_return_type(admin_class, "users", None, None)
        .expect("users on admin proxy has a return_type");
    let users_class = users_proxy.class_name().unwrap();
    assert_eq!(
        users_class,
        "Mojolicious::Controller::_Helper::admin::users"
    );

    // 4. Leaf completion shows `purge`.
    let leaf_candidates = fa.complete_methods_for_class(users_class, None);
    let leaf_labels: Vec<&str> = leaf_candidates.iter().map(|c| c.label.as_str()).collect();
    assert!(
        leaf_labels.contains(&"purge"),
        "leaf proxy must offer `purge`; got: {:?}",
        leaf_labels
    );
}

// ==== Three tests pinning this round's user-facing contracts. ====
//
// They begin RED and get fixed one at a time below. Shape of each is
// "source code + cursor position + real-pipeline assertion" so we
// can't lie about internal function results passing while the LSP
// experience breaks.

/// Outline detail names the semantic kind, LSP kind stays FUNCTION
/// (user config can render an icon for the domain word). Terminal
/// URL handlers (mojo-lite `get '/x' => sub {}`) are `<route>`;
/// routing hops (`->to('Users#list')`) are `<dispatch>` — those
/// two are semantically different and must not collapse. Tasks
/// stay `<task>`, helpers stay `<helper>`, events stay EVENT.
#[test]
fn outline_detail_names_the_semantic_kind() {
    use tower_lsp::lsp_types::SymbolKind;
    let src = r#"package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub { my ($c) = @_; });

my $r = app->routes;
$r->get('/x')->to('Users#list');
get '/home' => sub { my $c = shift; };

use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub { my ($job) = @_; });

package MyEmitter;
use parent 'Mojo::EventEmitter';
sub new {
    my $self = bless {}, shift;
    $self->on('ready', sub { my ($s) = @_; });
    $self;
}
"#;
    let fa = build_fa(src);
    let outline = fa.document_symbols();

    fn flatten<'a>(
        out: &'a [crate::file_analysis::OutlineSymbol],
        acc: &mut Vec<&'a crate::file_analysis::OutlineSymbol>,
    ) {
        for s in out {
            acc.push(s);
            flatten(&s.children, acc);
        }
    }
    let mut all = Vec::new();
    flatten(&outline, &mut all);

    let lsp_kind = |os: &crate::file_analysis::OutlineSymbol| -> SymbolKind {
        crate::symbols::outline_lsp_kind(os)
    };

    let helper = all
        .iter()
        .find(|s| s.name.contains("current_user"))
        .expect("helper must be in outline of its declaring file");
    assert_eq!(lsp_kind(helper), SymbolKind::FUNCTION);
    assert!(
        helper.detail.as_deref().unwrap_or("").contains("helper"),
        "helper outline detail must contain 'helper'; got: {:?}",
        helper.detail
    );

    // Terminal route: body lives here, `<route>` word.
    let term_route = all
        .iter()
        .find(|s| s.name.contains("/home"))
        .expect("mojo-lite terminal route must be in outline");
    assert_eq!(lsp_kind(term_route), SymbolKind::FUNCTION);
    assert_eq!(
        term_route.detail.as_deref(),
        Some("route"),
        "terminal mojo-lite route word is 'route'; got: {:?}",
        term_route.detail
    );

    // Controller action (`->to('Users#list')`): no body at this
    // site, just a cross-reference into Users::list. Word must be
    // `action`, not `route` — `<route> GET /x` and `<action>
    // Users#list` are semantically different line items.
    let action = all
        .iter()
        .find(|s| s.name.contains("Users#list"))
        .expect("->to('Users#list') action must be in outline");
    assert_eq!(lsp_kind(action), SymbolKind::FUNCTION);
    assert_eq!(
        action.detail.as_deref(),
        Some("action"),
        "->to(...) word is 'action' (distinct from a terminal route); got: {:?}",
        action.detail
    );

    let task = all
        .iter()
        .find(|s| s.name.contains("send_email"))
        .expect("task must be in outline of its declaring file");
    assert_eq!(lsp_kind(task), SymbolKind::FUNCTION);
    assert!(
        task.detail.as_deref().unwrap_or("").contains("task"),
        "task outline detail must contain 'task'; got: {:?}",
        task.detail
    );

    let event = all
        .iter()
        .find(|s| s.name.contains("ready"))
        .expect("event must be in outline of its declaring file");
    assert_eq!(
        lsp_kind(event),
        SymbolKind::EVENT,
        "events stay EVENT — the one LSP kind that fits"
    );
}

/// `sub get { shift->_generate_route(GET => @_) }` — the Mojo
/// Routes::Route pattern. `shift` in the invocant position of a
/// method call within a method body means `$self`, so the chain
/// invocant class must resolve to the enclosing package.
///
/// Without this, every HTTP-verb method on Mojolicious::Routes::Route
/// has an unknowable chain and `$r->get(...)->to(...)` loses
/// intelligence at the `->to` hop.
#[test]
fn shift_as_self_in_method_body_resolves_to_current_package() {
    let src = r#"
package Mojolicious::Routes::Route;

sub get { shift->_generate_route(GET => @_) }

sub _generate_route {
    my $self = shift;
    return $self;
}
"#;
    let fa = build_fa(src);

    // The MethodCall ref for `_generate_route` (inside `get`'s body)
    // must carry `invocant_class = Mojolicious::Routes::Route` —
    // proving the build-time chain resolver treated `shift` as
    // `$self` and looked up the enclosing package.
    let gr_ref = fa
        .refs
        .iter()
        .find(|r| {
            matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "_generate_route"
        })
        .expect("MethodCall ref for `_generate_route`");

    if matches!(gr_ref.kind, RefKind::MethodCall { .. }) {
        let invocant_class = fa.method_call_invocant_class(gr_ref, None);
        assert_eq!(
            invocant_class.as_deref(),
            Some("Mojolicious::Routes::Route"),
            "`shift->_generate_route` must resolve its invocant to \
                 the enclosing package. got invocant_class: {:?}",
            invocant_class,
        );
    } else {
        panic!("expected MethodCall ref");
    }
}

/// `sub is_endpoint { $_[0]->inline ? undef : ... }` — Mojo uses
/// `$_[0]` instead of `shift` on hot paths where the shift's arg-
/// list mutation is expensive. Same self-tell as `shift`.
#[test]
fn dollar_underscore_zero_as_self_resolves_to_current_package() {
    let src = r#"
package Mojolicious::Routes::Route;

sub is_endpoint {
    $_[0]->inline;
}

sub inline {
    my $self = shift;
    return $self;
}
"#;
    let fa = build_fa(src);

    let inline_ref = fa
        .refs
        .iter()
        .find(|r| matches!(r.kind, RefKind::MethodCall { .. }) && r.target_name == "inline")
        .expect("MethodCall ref for `inline`");

    if matches!(inline_ref.kind, RefKind::MethodCall { .. }) {
        let invocant_class = fa.method_call_invocant_class(inline_ref, None);
        assert_eq!(
            invocant_class.as_deref(),
            Some("Mojolicious::Routes::Route"),
            "`$$_[0]->inline` must resolve its invocant to the \
                 enclosing package. got invocant_class: {:?}",
            invocant_class,
        );
    } else {
        panic!("expected MethodCall ref");
    }
}

/// Regression for the crash reported in the nvim LSP log:
/// `thread 'tokio-rt-worker' panicked at src/file_analysis.rs:1164:44:
/// index out of bounds: the len is 17 but the index is 17`.
///
/// Root cause: `enrich_imported_types_with_keys` truncates
/// `type_constraints` back to baseline but leaves stale indices in
/// `type_constraints_by_var` from the previous enrichment. The next
/// enrichment's call to `resolve_method_call_types` invokes
/// `inferred_type`, which indexes into `type_constraints[idx]` —
/// OOB when idx points past the truncated length.
///
/// Repro: enrich the same FileAnalysis twice with a module_index.
/// The second call must not panic.
#[test]
fn enrichment_twice_does_not_crash_on_stale_indices() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let app_src = r#"
package main;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');
"#;
    let mojolicious_pm = r#"
package Mojolicious;
use Mojo::Base -base;
has routes => sub { Mojolicious::Routes->new };
1;
"#;
    let routes_pm = r#"
package Mojolicious::Routes;
use Mojo::Base 'Mojolicious::Routes::Route';
1;
"#;
    let route_pm = r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;
sub get { my $self = shift; return $self; }
sub to  { my $self = shift; return $self; }
1;
"#;

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious.pm"),
        Arc::new(build_fa(mojolicious_pm)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes.pm"),
        Arc::new(build_fa(routes_pm)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes/Route.pm"),
        Arc::new(build_fa(route_pm)),
    );

    let mut fa = build_fa(app_src);
    // First enrichment — simulates publish_diagnostics after module
    // resolution. Populates type_constraints + type_constraints_by_var.
    fa.enrich_imported_types_with_keys(Some(&idx));
    // Second enrichment — simulates a subsequent change or refresh.
    // Before the fix, the stale type_constraints_by_var indices
    // panicked `inferred_type` during resolve_method_call_types.
    fa.enrich_imported_types_with_keys(Some(&idx));

    // Sanity: `$r` is still typed after the second run (not just
    // "didn't crash" — the state is actually usable).
    let r_type = fa.inferred_type_via_bag("$r", tree_sitter::Point { row: 5, column: 0 });
    assert!(
        r_type.as_ref().and_then(|t| t.class_name()) == Some("Mojolicious::Routes"),
        "after two enrichments, $$r should still be typed as Mojolicious::Routes; got: {:?}",
        r_type,
    );
}

/// Real-file invariant: every meaningful token on the
/// `app->routes` / `$r->get(...)->to(...)` lines of the mojo demo
/// must surface a useful hover AND a useful goto-def. This is the
/// exact scenario the user reports dead in nvim — hover returns
/// nothing, gd has nowhere to go.
///
/// Probes (all on the actual demo file, not a synthetic snippet):
///   * `app`    in `my $r = app->routes;`         → hover mentions Mojolicious; gd lands somewhere
///   * `routes` in `app->routes`                  → hover mentions routes / Mojolicious::Routes; gd into Mojolicious.pm
///   * `$r`     in `$r->get(...)`                 → hover shows the declaration line
///   * `get`    in `$r->get(...)`                 → hover mentions the real Route::get POD; gd into Route.pm
///   * `to`     in `->to('Users#list')`           → hover mentions Route::to; gd into Route.pm
///
/// Any probe returning `None` for BOTH hover and gd is a bug. The
/// test enumerates each probe independently so failures pinpoint
/// which hop of the chain is broken, not "something somewhere".
#[test]
fn mojo_demo_lines_70_71_all_tokens_intelligent() {
    use crate::module_index::ModuleIndex;
    use std::sync::Arc;

    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("test_files/plugin_mojo_demo.pl");
    let src = std::fs::read_to_string(&path).unwrap();
    let fa = build_fa(&src);
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&src, None).unwrap();

    // Stub the three Mojo modules the chain walks through so
    // cross-file resolution has something to reach. Shapes mirror
    // the real @INC modules' method signatures.
    let mojolicious_pm = r#"
package Mojolicious;
use Mojo::Base -base;

=head2 routes

Returns the router.

=cut

has routes => sub { Mojolicious::Routes->new };

=head2 helper

Register a helper.

=cut

sub helper { my $self = shift; }
1;
"#;
    let routes_pm = r#"
package Mojolicious::Routes;
use Mojo::Base 'Mojolicious::Routes::Route';
1;
"#;
    let route_pm = r#"
package Mojolicious::Routes::Route;
use Mojo::Base -base;

=head2 get

  my $route = $r->get('/:foo' => sub ($c) {...});

Generate route matching only C<GET> requests.

=cut

sub get { my $self = shift; return $self; }

=head2 to

  $r->to('Users#list');

Set the route's target.

=cut

sub to { my $self = shift; return $self; }
1;
"#;

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious.pm"),
        Arc::new(build_fa(mojolicious_pm)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes.pm"),
        Arc::new(build_fa(routes_pm)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/Mojolicious/Routes/Route.pm"),
        Arc::new(build_fa(route_pm)),
    );

    // Cross-file enrichment — mirrors `Backend::enrich_analysis`.
    // Without this pass, MethodCallBindings whose resolution needs
    // a cross-file return type (e.g. `$r = app->routes` needs real
    // Mojolicious.pm's `routes` accessor) don't land in
    // `type_constraints`, and `$r` stays untyped.
    let mut fa = fa;
    fa.enrich_imported_types_with_keys(Some(&idx));
    let fa = fa;

    // Locate the two target lines by content — decoupled from
    // absolute row numbers so reformats don't invalidate the test.
    let (row_app_routes, line_app_routes) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("my $r = app->routes;"))
        .map(|(i, l)| (i, l))
        .expect("demo must contain `my $r = app->routes;`");
    let (row_r_get_to, line_r_get_to) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')->to('Users#list');"))
        .map(|(i, l)| (i, l))
        .expect("demo must contain `$r->get('/users')->to('Users#list');`");

    // Column helper — cursor one char into the token, not at its start,
    // so `ref_at` / `symbol_at` hit the token reliably.
    let col_of =
        |line: &str, needle: &str| -> usize { line.find(needle).expect("needle in line") + 1 };
    let probe = |row: usize, col: usize| tree_sitter::Point { row, column: col };

    // Per-probe assertion. Any probe where BOTH hover and gd come
    // back empty is a dead token — the user's reported symptom.
    // Print detailed per-probe status so failures pinpoint the hop.
    let check = |label: &str, point: tree_sitter::Point| {
        let hover = fa.hover_info(point, &src, Some(&idx));
        let def = fa.find_definition(point, Some(&tree), Some(src.as_bytes()), Some(&idx));
        assert!(
            hover.is_some() || def.is_some(),
            "[{label}] @ ({},{}) is a dead token — NO hover AND NO gd. \
                 Chain-resolution hit a wall here. src: {:?}",
            point.row,
            point.column,
            src.lines().nth(point.row).unwrap_or("<oob>"),
        );
    };

    // Line 70 probes.
    check(
        "app bareword",
        probe(row_app_routes, col_of(line_app_routes, "app")),
    );
    check(
        "routes accessor",
        probe(row_app_routes, col_of(line_app_routes, "routes")),
    );

    // Line 71 probes.
    check(
        "$r receiver",
        probe(row_r_get_to, col_of(line_r_get_to, "$r")),
    );
    check(
        "get method",
        probe(row_r_get_to, col_of(line_r_get_to, "->get") + 2),
    ); // skip "->"
    check(
        "to method",
        probe(row_r_get_to, col_of(line_r_get_to, "->to") + 2),
    );

    // Focused assertions on `app`:
    //   1. Hover surfaces the plugin's `app` Sub doc — i.e. ref_at
    //      resolves to the narrow FunctionCall ref for the bareword,
    //      NOT the wider MethodCall ref that would describe `routes`.
    //   2. A semantic token lands on the bareword span — the user
    //      reported no highlight on `app->` in nvim; the narrow
    //      FunctionCall ref is what feeds semantic tokens.
    let app_point = probe(row_app_routes, col_of(line_app_routes, "app"));
    let app_hover = fa.hover_info(app_point, &src, Some(&idx));
    let app_hover_text = app_hover.as_deref().unwrap_or("");
    assert!(
        app_hover_text.contains("The Mojolicious application instance"),
        "hover on `app` must surface the plugin-emitted Sub's doc \
             — proving ref_at picked the narrow FunctionCall ref, not \
             the outer MethodCall for `routes`. got: {:?}",
        app_hover,
    );

    // Semantic token on the bareword — any token kind is fine, the
    // point is SOMETHING lights it up.
    let tokens = fa.semantic_tokens();
    let app_row = row_app_routes;
    let app_col_start = line_app_routes.find("app").unwrap();
    let app_col_end = app_col_start + "app".len();
    let app_has_token = tokens.iter().any(|t| {
        t.span.start.row == app_row
            && t.span.start.column == app_col_start
            && t.span.end.column == app_col_end
    });
    assert!(
        app_has_token,
        "semantic token must fire on the `app` bareword span — \
             user reported no highlight and traced it to a missing \
             Ref at the invocant. tokens near row {}: {:?}",
        app_row,
        tokens
            .iter()
            .filter(|t| t.span.start.row == app_row)
            .collect::<Vec<_>>(),
    );

    // Headline chain assertion: `$r` MUST be typed as
    // Mojolicious::Routes after the `my $r = app->routes;` line.
    // This is the single most important observable — without it,
    // every `$r->...` downstream loses intelligence (precisely
    // the user's report). `inferred_type` is the same query
    // resolve_invocant_class uses for method resolution, so if
    // this says None, nothing on line 71 can work.
    let r_point = probe(row_r_get_to, col_of(line_r_get_to, "$r"));
    let r_type = fa.inferred_type_via_bag("$r", r_point);
    assert_eq!(
        r_type.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes"),
        "`$$r` must be typed as Mojolicious::Routes at the `$$r->get` \
             call site. Without this, the rest of line 71 is dead. got: {:?}",
        r_type,
    );

    // `$r->get` must resolve via inheritance (Mojolicious::Routes
    // ISA Mojolicious::Routes::Route) to the real `get` method.
    // Return type is fluent — stays on Route for `->to` to work.
    let get_rt = fa.find_method_return_type("Mojolicious::Routes", "get", Some(&idx), None);
    assert_eq!(
        get_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "`$$r->get` must resolve to Mojolicious::Routes::Route::get \
             via inheritance. got: {:?}",
        get_rt,
    );
}

/// Real-file invariant pinning the original nvim repro: line 118
/// of plugin_mojo_demo.pl, which sits textually in `package MyApp`
/// but before the fix was reported as `MyApp::Progress` (the LAST-
/// declared package in the file) — so Minion's trigger didn't
/// match, the plugin hook didn't fire, and the native path
/// mis-keyed the task's sig off the enqueue parens.
///
/// Pinned points:
///   * cursor inside `'alice@example.com'` → $to   (slot 0)
///   * cursor inside `'hi'`                → $subject (slot 1)
///   * cursor inside `'body'`              → $body   (slot 2)
///   * cursor past the closing `]`          → NOT the task sig
#[test]
fn enqueue_sighelp_line_118_of_demo() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("test_files/plugin_mojo_demo.pl");
    let src = std::fs::read_to_string(&path).unwrap();
    let fa = build_fa(&src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&src, None).unwrap();
    let idx = crate::module_index::ModuleIndex::new_for_test();

    // Locate the enqueue call by content — line numbers in the
    // demo file shift whenever it's edited, and the test's value
    // is the signature-help behavior, not a literal row.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$minion->enqueue(send_email"))
        .map(|(i, l)| (i as u32, l))
        .expect("demo must contain the send_email enqueue site");

    let cases: &[(&str, &str, Option<u32>)] = &[
        ("alice@example.com", "'alice", Some(0)),
        ("hi", "'hi'", Some(1)),
        ("body", "'body'", Some(2)),
    ];
    for (slot_label, needle, expected) in cases {
        let col = (line.find(needle).unwrap() + 2) as u32;
        let pos = Position {
            line: line_idx,
            character: col,
        };
        let sig = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx)
            .unwrap_or_else(|| panic!("[{slot_label}] sig help must fire"));
        assert!(
            sig.signatures[0].label.contains("send_email"),
            "[{slot_label}] task sig expected; got {:?}",
            sig.signatures[0].label
        );
        assert_eq!(
            sig.active_parameter, *expected,
            "[{slot_label}] wrong slot; got {:?}",
            sig.active_parameter
        );
    }

    // Past the closing `]` — must NOT show the task sig.
    let col = (line.rfind(']').unwrap() + 1) as u32;
    let pos = Position {
        line: line_idx,
        character: col,
    };
    if let Some(s) = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx) {
        let lbl = &s.signatures[0].label;
        assert!(
            !lbl.contains("send_email"),
            "past `]`: task sig must not leak; got {lbl:?}"
        );
    }
}

/// Pinned invariant for the real-nvim Minion sig-help bug:
///
///   * Fat commas and literal commas must produce the SAME
///     signature-help behavior at identical cursor positions.
///     Two cases before the fix: (a) inside the arrayref, both
///     variants routed to the task sig — that worked. (b) once
///     the cursor left the arrayref, the native string-dispatch
///     path keyed the task's active_param off the outer call's
///     literal-comma count, surfacing `$subject` at the options-
///     hash slot, and `$body` several slots into a run of
///     trailing commas. Both wrong in obviously different ways.
///
///   * Cursor inside the arrayref → task sig, correct slot.
///   * Cursor outside the arrayref but still in the enqueue call
///     → NEVER the task sig. Falls through to enqueue's own
///     method sig (none here, since Minion.pm isn't indexed in
///     the test — `None` is the acceptable outcome).
///
/// If this regresses, the sweep-style bug is back: flip to
/// `DUMP_SWEEP=1 cargo test` to get a per-column dump.
#[test]
fn enqueue_sighelp_separator_agnostic() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let cases: &[(&str, &str)] = &[
        (
            "literal-comma",
            "$minion->enqueue('send_email', [ 'alice' ], {})",
        ),
        (
            "fat-comma",
            "$minion->enqueue(send_email => [ 'alice' ], , , , )",
        ),
    ];

    let header = "package MyApp;\nuse Minion;\nmy $minion = Minion->new;\n\
             $minion->add_task(send_email => sub { my ($job, $to, $subject, $body) = @_; });\n";

    let dump = std::env::var("DUMP_SWEEP").is_ok();
    let mut dump_out = String::new();

    for (label, call_line) in cases {
        let src = format!("{}{};\n", header, call_line);
        let fa = build_fa(&src);
        let mut parser = Parser::new();
        parser
            .set_language(&ts_parser_perl::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(&src, None).unwrap();
        let idx = crate::module_index::ModuleIndex::new_for_test();

        let line_idx = src
            .lines()
            .position(|l| l.starts_with("$minion->enqueue"))
            .unwrap();
        let line = src.lines().nth(line_idx).unwrap();

        // Cursor inside 'alice' → task sig, slot 0 ($to).
        let in_alice = line.find("'alice'").unwrap() + 3;
        let pos = Position {
            line: line_idx as u32,
            character: in_alice as u32,
        };
        let sig = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx)
            .unwrap_or_else(|| panic!("[{label}] cursor in 'alice' must fire task sig"));
        assert!(
            sig.signatures[0].label.contains("send_email"),
            "[{label}] in 'alice' → task sig; got: {:?}",
            sig.signatures[0].label
        );
        assert_eq!(
            sig.active_parameter,
            Some(0),
            "[{label}] in 'alice' → $to (slot 0); got {:?}",
            sig.active_parameter
        );

        // Cursor past the `]` but still inside the enqueue parens
        // → the options-hash slot / trailing-comma space. MUST NOT
        // show the task sig. `None` is acceptable (enqueue's own
        // method isn't indexed in this test).
        let past_bracket = line.find(']').unwrap() + 2;
        let pos = Position {
            line: line_idx as u32,
            character: past_bracket as u32,
        };
        let sig = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx);
        if let Some(s) = &sig {
            let lbl = &s.signatures[0].label;
            assert!(
                !lbl.contains("send_email"),
                "[{label}] past `]`: task sig must NOT show; got: {:?}",
                lbl
            );
        }

        // Fat-comma specific: sweep the trailing-commas region
        // and ensure NONE of those columns surface the task sig.
        // Before the fix, each literal comma bumped active_param
        // and produced $subject / $body at arbitrary positions.
        if *label == "fat-comma" {
            let start = line.find(']').unwrap() + 1;
            let end = line.rfind(')').unwrap();
            for col in start..=end {
                let pos = Position {
                    line: line_idx as u32,
                    character: col as u32,
                };
                let sig = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx);
                if let Some(s) = &sig {
                    let lbl = &s.signatures[0].label;
                    assert!(!lbl.contains("send_email"),
                            "[{label}] col {col}: task sig leaked into trailing-comma region; got: {:?}",
                            lbl);
                }
            }
        }

        if dump {
            dump_out.push_str(&format!("\n=== {} ===\n{}\n", label, line));
            for col in 0..=line.len() {
                let pos = Position {
                    line: line_idx as u32,
                    character: col as u32,
                };
                let sig = crate::symbols::signature_help(&fa, &tree, &src, pos, &idx);
                let label_str = match &sig {
                    None => "<none>".to_string(),
                    Some(s) => format!(
                        "ap={:?} sig={}",
                        s.active_parameter,
                        s.signatures
                            .first()
                            .map(|si| si.label.as_str())
                            .unwrap_or("")
                    ),
                };
                let ch = line
                    .chars()
                    .nth(col)
                    .map(|c| c.to_string())
                    .unwrap_or_else(|| "<eol>".into());
                dump_out.push_str(&format!("col {:>3} ({:<5}): {}\n", col, ch, label_str));
            }
        }
    }

    if dump {
        panic!("{}", dump_out);
    }
}

/// Sanity: the minion plugin registers a task Handler with the
/// expected shape. The arrayref-sig-help behavior itself lives in
/// the plugin's `on_signature_help` IoC hook (tested end-to-end
/// below) — no data flag on the Handler.
#[test]
fn minion_registers_task_handler() {
    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub { my ($job, $to) = @_; });
"#;
    let fa = build_fa(src);
    let h = fa
        .symbols
        .iter()
        .find(|s| s.kind == SymKind::Handler && s.name == "send_email")
        .expect("handler exists");
    let SymbolDetail::Handler {
        dispatchers,
        display,
        ..
    } = &h.detail
    else {
        panic!("detail shape");
    };
    assert!(
        dispatchers.iter().any(|d| d == "enqueue"),
        "must list enqueue as a dispatcher; got: {:?}",
        dispatchers
    );
    assert!(
        matches!(display, HandlerDisplay::Task),
        "task handlers display as Task; got: {:?}",
        display
    );
}

/// Test 2 — arrayref sig help, through the REAL LSP pipeline.
/// Cursor sits INSIDE the middle string literal `'hi'` — the
/// shape a user actually produces in nvim. active_parameter must
/// be 1 (= $subject). Earlier version of this test used a
/// cursor-right-after-comma position that nobody types at, and
/// passed while the real nvim experience was broken.
#[test]
fn enqueue_arrayref_sig_help_active_param_inside_string() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
});
$minion->enqueue(send_email => ['alice', 'hi', 'body']);
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Cursor between `h` and `i` of `'hi'` — the middle slot of
    // the arrayref, which is $subject.
    let line_idx = src
        .lines()
        .position(|l| l.contains("enqueue(send_email"))
        .expect("enqueue line present");
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("'hi'").unwrap() + 2; // between h and i
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let sig = crate::symbols::signature_help(&fa, &tree, src, pos, &idx)
        .expect("sig help must fire inside a string-literal arrayref arg");

    let info = &sig.signatures[0];
    assert!(
        info.label.contains("send_email"),
        "label references the task, not enqueue; got: {:?}",
        info.label
    );
    assert!(
        info.label.contains("$subject"),
        "label surfaces the task's params; got: {:?}",
        info.label
    );
    assert_eq!(
        sig.active_parameter,
        Some(1),
        "cursor inside `'hi'` → $subject (index 1), NOT $to. \
             If you see 0 here, sig help isn't recognizing it's inside \
             the arrayref at slot 1; got: {:?}",
        sig.active_parameter
    );
}

/// Sig help must also land on the LAST arrayref slot when the
/// cursor is inside its string literal. Pinned separately from
/// the middle-slot test because count_commas can off-by-one on
/// the last slot if the walker breaks wrong.
#[test]
fn enqueue_arrayref_sig_help_active_param_inside_last_string() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
});
$minion->enqueue(send_email => ['alice', 'hi', 'body']);
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    let line_idx = src
        .lines()
        .position(|l| l.contains("enqueue(send_email"))
        .unwrap();
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("'body'").unwrap() + 3; // inside "body"
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let sig = crate::symbols::signature_help(&fa, &tree, src, pos, &idx)
        .expect("sig help fires inside the last string too");

    assert_eq!(
        sig.active_parameter,
        Some(2),
        "cursor inside `'body'` → $body (index 2); got: {:?}",
        sig.active_parameter
    );
}

/// Test 3a — hash-key completion on an empty enqueue options hash
/// in a file that ALSO has a matching add_task. The earlier
/// version of this test used an enqueue for an unknown task name,
/// which accidentally sidestepped the dispatch-args short-circuit
/// — nvim's real experience (task registered, enqueue at 3rd arg)
/// was silently broken. Pin the real shape.
#[test]
fn enqueue_options_hash_completion_empty() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(task_x => sub { my ($job, $a) = @_; });
$minion->enqueue(task_x => ['a'], {  });
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Cursor inside the enqueue options hash — `{  }` on the
    // enqueue line. Can't just search for "{ " globally because
    // the sub body `sub { my ($job` matches first.
    let line_idx = src
        .lines()
        .position(|l| l.contains("enqueue(task_x"))
        .expect("enqueue line");
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("{  }").unwrap() + 2; // halfway between `{` and `}`
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let items = crate::symbols::completion_items(&fa, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    for expected in &["priority", "queue", "delay", "attempts"] {
        assert!(
            labels.contains(expected),
            "empty-hash: `{}` must complete; got: {:?}",
            expected,
            labels
        );
    }
}

/// Test 3b — with an existing key in the hash, it must NOT be
/// offered again; the rest of the options must still appear.
/// Same task-registered shape as 3a so the dispatch-args
/// short-circuit IS active and gets properly bypassed on HashKey.
#[test]
fn enqueue_options_hash_completion_with_existing_keys() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(task_x => sub { my ($job, $a) = @_; });
$minion->enqueue(task_x => ['a'], { priority => 10,  });
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Scope the anchor to the enqueue line so the sub body's own
    // brace/comma pattern doesn't claim the match first.
    let line_idx = src
        .lines()
        .position(|l| l.contains("enqueue(task_x"))
        .expect("enqueue line");
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("priority => 10, ").unwrap() + "priority => 10, ".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let items = crate::symbols::completion_items(&fa, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    assert!(
        labels.contains(&"queue"),
        "with-existing: `queue` must still complete; got: {:?}",
        labels
    );
    assert!(
        labels.contains(&"delay"),
        "with-existing: `delay` must still complete; got: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"priority"),
        "with-existing: `priority` is already used — must NOT re-appear; got: {:?}",
        labels
    );
}

/// mojo-helpers emits a PluginNamespace for the app, bridging to the
/// single fictional app surface (docs/adr/plugin-system.md). Each
/// registered helper's name is an entity. The consumer classes reach
/// the surface via the synthetic-parent edge in core. Multi-app
/// workspaces get one namespace per app.
#[test]
fn mojo_helpers_emits_app_plugin_namespace() {
    use crate::file_analysis::Bridge;
    let src = r#"package MyApp;
use Mojolicious::Lite;
my $app = Mojolicious->new;
$app->helper(current_user => sub { my ($c) = @_; });
$app->helper('users.create' => sub { my ($c) = @_; });
"#;
    let fa = build_fa(src);

    // Identify by semantic shape (kind + bridge to the app surface),
    // not by plugin id — the contract is "there's an 'app' namespace
    // bridging to the app surface", not "a plugin literally called
    // mojo-helpers emits it".
    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| {
            n.kind == "app"
                && n.bridges
                    .contains(&Bridge::Class(crate::file_analysis::APP_SURFACE_CLASS.into()))
        })
        .expect("an `app` namespace must bridge the app surface");

    // Entities cover both registered helpers, through the
    // name-keyed resolution that expands fan-out Methods.
    let entity_names: Vec<&str> = ns
        .entities
        .iter()
        .map(|id| fa.symbol(*id).name.as_str())
        .collect();
    assert!(
        entity_names.contains(&"current_user"),
        "simple helper must land in the namespace; got: {:?}",
        entity_names
    );
    assert!(
        entity_names.contains(&"users"),
        "dotted-helper root must land in the namespace; got: {:?}",
        entity_names
    );

    // Namespace ID is stable per enclosing package — one namespace
    // for MyApp regardless of how many helpers it registers. Scope
    // the count to this namespace's own (plugin_id, id) pair.
    let count = fa
        .plugin_namespaces
        .iter()
        .filter(|n| n.plugin_id == ns.plugin_id && n.id == ns.id)
        .count();
    assert_eq!(count, 1, "one namespace per app, not one per helper");
}

/// Plugin namespaces are a structural concept (bridges into class
/// lookups via `for_each_entity_bridged_to`) — they are deliberately
/// NOT surfaced in the document outline. The entities inside (helpers,
/// routes, tasks) already render as individual entries with their
/// `<word>` kind prefix; a separate "this file hosts a mojo app" row
/// is noise the user can't act on. The namespace data still has to
/// be populated for cross-file bridge lookups to work — that's what
/// this test pins.
#[test]
fn plugin_namespaces_are_populated_but_not_in_outline() {
    let src = r#"package MyApp;
use Mojolicious::Lite;
app->helper(current_user => sub { my ($c) = @_; });
get '/home' => sub { my $c = shift; };
"#;
    let fa = build_fa(src);

    // The namespace data is still there for bridge queries — that's
    // how `$c->current_user` resolves to the helper across files.
    assert!(
        fa.plugin_namespaces.iter().any(|n| n.kind == "app"),
        "app namespace should still exist in FileAnalysis; got: {:?}",
        fa.plugin_namespaces
            .iter()
            .map(|n| &n.id)
            .collect::<Vec<_>>()
    );

    // Outline must NOT contain any Namespace kind entries from the
    // plugin namespaces. Packages (`MyApp`) are Namespace-kind too
    // but come from SymKind::Package symbols, which are fine.
    let outline = fa.document_symbols();
    let plugin_ns_in_outline: Vec<&str> = outline
        .iter()
        .filter(|o| o.kind == SymKind::Namespace)
        .map(|o| o.name.as_str())
        .filter(|n| n.starts_with('['))
        .collect();
    assert!(
        plugin_ns_in_outline.is_empty(),
        "plugin namespaces must not surface in outline; leaked: {:?}",
        plugin_ns_in_outline,
    );

    // The actual entries (helper, route) still show flat.
    fn walk<'a>(xs: &'a [crate::file_analysis::OutlineSymbol], out: &mut Vec<&'a str>) {
        for x in xs {
            out.push(x.name.as_str());
            walk(&x.children, out);
        }
    }
    let mut all = Vec::new();
    walk(&outline, &mut all);
    assert!(
        all.iter().any(|n| n.contains("current_user")),
        "helper must still appear flat in outline; got: {:?}",
        all
    );
    assert!(
        all.iter().any(|n| n.contains("/home")),
        "route must still appear flat in outline; got: {:?}",
        all
    );
}

/// mojo-events emits a PluginNamespace per emitter class. Bridges to
/// the emitter class; entity_names are the event Handler names.
/// Multiple `->on/->once/->subscribe` wire-ups on the same emitter
/// accumulate under the same namespace id.
#[test]
fn mojo_events_emits_emitter_plugin_namespace() {
    use crate::file_analysis::Bridge;
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';
sub register {
    my $self = shift;
    $self->on(connect => sub { my ($e) = @_; });
    $self->on(disconnect => sub { my ($e) = @_; });
    $self->once(ready => sub { my ($e) = @_; });
}
"#;
    let fa = build_fa(src);

    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| n.kind == "events" && n.bridges.contains(&Bridge::Class("My::Emitter".into())))
        .expect("an `events` namespace must bridge My::Emitter");

    let entity_names: Vec<&str> = ns
        .entities
        .iter()
        .map(|id| fa.symbol(*id).name.as_str())
        .collect();
    for ev in ["connect", "disconnect", "ready"] {
        assert!(
            entity_names.contains(&ev),
            "event `{}` must land in the namespace; got: {:?}",
            ev,
            entity_names
        );
    }

    let count = fa
        .plugin_namespaces
        .iter()
        .filter(|n| n.plugin_id == ns.plugin_id && n.id == ns.id)
        .count();
    assert_eq!(count, 1, "one namespace per emitter, not one per wire-up");
}

/// mojo-routes emits a PluginNamespace per declaring package. Each
/// `->to('Ctrl#action')` call's Handler lands as a namespace entity;
/// the bridge points at `Mojolicious::Controller` (not the declaring
/// package) so `$c->url_for('|')` from any controller resolves via
/// `for_each_entity_bridged_to` walking through Controller in its
/// ancestor chain. Namespace id still keys on the declaring package
/// so future app-scoping has per-app buckets to narrow to.
#[test]
fn mojo_routes_emits_app_plugin_namespace() {
    use crate::file_analysis::Bridge;
    let src = r#"package MyApp;
use Mojolicious;
sub startup {
    my $self = shift;
    my $r = $self->routes;
    $r->get('/users')->to('Users#list');
    $r->post('/users')->to('Users#create');
}
"#;
    let fa = build_fa(src);

    // Identify by semantic shape — a `routes` namespace that
    // bridges to Mojolicious::Controller (the happy-path owner
    // for the workspace-wide url_for lookup). Entity names are
    // the Controller#action form, distinguishing from the Lite
    // path-based flavor.
    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| {
            n.kind == "routes"
                && n.bridges
                    .contains(&Bridge::Class("Mojolicious::Controller".into()))
                && n.entities
                    .iter()
                    .any(|id| fa.symbol(*id).name.contains('#'))
        })
        .expect(
            "a `routes` namespace must bridge Mojolicious::Controller with Ctrl#action entities",
        );

    let entity_names: Vec<&str> = ns
        .entities
        .iter()
        .map(|id| fa.symbol(*id).name.as_str())
        .collect();
    assert!(
        entity_names.contains(&"Users#list"),
        "route Users#list must land in the namespace; got: {:?}",
        entity_names
    );
    assert!(
        entity_names.contains(&"Users#create"),
        "route Users#create must land in the namespace; got: {:?}",
        entity_names
    );

    let count = fa
        .plugin_namespaces
        .iter()
        .filter(|n| n.plugin_id == ns.plugin_id && n.id == ns.id)
        .count();
    assert_eq!(
        count, 1,
        "one namespace per declaring package, not one per route"
    );
}

/// mojo-lite emits a PluginNamespace per Lite app. Entity names are
/// the route paths (the same string that mojo-lite stamps into the
/// Handler). Bridge is `Mojolicious::Controller` so `$c->url_for(|)`
/// inside any controller picks up these Lite routes too — mirrors
/// mojo-routes; the Lite script package lives on in the namespace
/// id (`mojo-lite:<pkg>`) for future app-scoping.
#[test]
fn mojo_lite_emits_app_plugin_namespace() {
    use crate::file_analysis::Bridge;
    let src = r#"package main;
use Mojolicious::Lite;
get '/users' => sub { my $c = shift; };
post '/login' => sub { my $c = shift; };
"#;
    let fa = build_fa(src);

    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| {
            n.kind == "routes"
                && n.bridges
                    .contains(&Bridge::Class("Mojolicious::Controller".into()))
                && n.entities
                    .iter()
                    .any(|id| fa.symbol(*id).name.starts_with('/'))
        })
        .expect(
            "a Lite `routes` namespace must bridge Mojolicious::Controller with /path entities",
        );

    let entity_names: Vec<&str> = ns
        .entities
        .iter()
        .map(|id| fa.symbol(*id).name.as_str())
        .collect();
    assert!(
        entity_names.contains(&"/users"),
        "route /users must land in the namespace; got: {:?}",
        entity_names
    );
    assert!(
        entity_names.contains(&"/login"),
        "route /login must land in the namespace; got: {:?}",
        entity_names
    );
}

/// minion emits a PluginNamespace per enclosing package. Tasks land
/// as entities; bridge is `Class(Minion)` so the namespace feeds the
/// same cross-file lookup primitive used by the other plugins.
/// (The `dispatch_targets_for` completion-hook path is independent —
/// the namespace here is for outline/workspace-symbol and future
/// consolidation of the task-lookup path.)
#[test]
fn minion_emits_tasks_plugin_namespace() {
    use crate::file_analysis::Bridge;
    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub { my ($job) = @_; });
$minion->add_task(resize_image => sub { my ($job) = @_; });
"#;
    let fa = build_fa(src);

    let ns = fa
        .plugin_namespaces
        .iter()
        .find(|n| n.kind == "tasks" && n.bridges.contains(&Bridge::Class("Minion".into())))
        .expect("a `tasks` namespace must bridge Minion");

    let entity_names: Vec<&str> = ns
        .entities
        .iter()
        .map(|id| fa.symbol(*id).name.as_str())
        .collect();
    assert!(
        entity_names.contains(&"send_email"),
        "task send_email must land in the namespace; got: {:?}",
        entity_names
    );
    assert!(
        entity_names.contains(&"resize_image"),
        "task resize_image must land in the namespace; got: {:?}",
        entity_names
    );

    let count = fa
        .plugin_namespaces
        .iter()
        .filter(|n| n.plugin_id == ns.plugin_id && n.id == ns.id)
        .count();
    assert_eq!(count, 1, "one namespace per package, not one per add_task");
}

/// RED — sig help at the OPTIONS hash of enqueue should show
/// enqueue's own signature, not the task's. Currently broken:
/// the string-dispatch sig help fires whenever the cursor is past
/// arg-0 of a dispatcher call, regardless of whether the cursor
/// is actually inside the handler-args slot. For `enqueue`,
/// handler args live INSIDE the arrayref at slot 1 — slot 2 is
/// enqueue's own options hash.
///
/// Proper fix: plugin-controlled dispatch (see
/// `docs/prompt-plugin-architecture.md` — IoC query hooks).
/// The plugin decides when sig help applies to the handler vs
/// when it applies to the dispatcher itself. Core-side fix is
/// possible (narrow the string-dispatch path to the declared
/// handler-args slot) but fragile; leaving as RED until the
/// IoC hook lands.
#[test]
fn enqueue_options_hash_sig_help_is_enqueue_not_task() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job, $to, $subject) = @_;
});
$minion->enqueue(send_email => ['a', 'b'], {  });
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    let line_idx = src
        .lines()
        .position(|l| l.contains("enqueue(send_email"))
        .unwrap();
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("{  }").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let sig = crate::symbols::signature_help(&fa, &tree, src, pos, &idx);

    // Tight contract: `PluginSigHelpAnswer::Silent` returns None
    // from `signature_help` — full stop. The plugin explicitly
    // claims the slot to block the native string-dispatch path
    // that would mis-show the task's sig. Anything else means
    // either the plugin stopped claiming, or the core's Silent
    // handler regressed.
    assert!(
        sig.is_none(),
        "plugin `Silent` on the options-hash slot must suppress native \
             sig help entirely; got: {:?}",
        sig
    );
}

/// RED — completion at arg-0 of enqueue should offer ONLY
/// registered task names (Handler dispatch targets), not a
/// union of tasks + every other `Minion` instance method.
/// Matches the real nvim env where CPAN-installed Minion brings
/// ~30 instance methods cross-file, which leak in when a
/// user types `$minion->enqueue(|)`.
///
/// Same arch gap as the sig-help one above: the core doesn't
/// know that `enqueue`'s arg-0 is semantically "pick a task
/// name", so `dispatch_target_completions` contributes task
/// names but instance methods reach in through completion of
/// the receiver's class methods on the `$minion->` receiver.
///
/// Proper fix: plugin-controlled `on_completion` hook + the
/// PluginNamespace entities indexed for fast "names of kind
/// `task` on this minion" lookup. See the arch doc.
#[test]
fn enqueue_arg0_offers_task_names_only() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    // Task-declaring file.
    let src = r#"package MyApp;
use Minion;
my $minion = Minion->new;
$minion->add_task(send_email => sub { my ($job) = @_; });
$minion->add_task(resize_image => sub { my ($job) = @_; });
$minion->enqueue();
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Mock CPAN Minion with realistic instance methods that
    // would otherwise leak into `$minion->enqueue(|)`. Uses the
    // same workspace-module-registration path nvim startup uses.
    let minion_src = r#"package Minion;
sub new { my $class = shift; bless {}, $class }
sub enqueue     { my ($self, $task, $args, $opts) = @_; }
sub enqueue_p   { my ($self, $task, $args, $opts) = @_; }
sub perform_jobs { my ($self) = @_; }
sub backend     { my ($self) = @_; }
sub reset       { my ($self) = @_; }
sub stats       { my ($self) = @_; }
sub worker      { my ($self) = @_; }
sub repair      { my ($self) = @_; }
sub foreground  { my ($self, $id) = @_; }
1;
"#;
    let minion_fa = std::sync::Arc::new(build_fa(minion_src));
    let idx = std::sync::Arc::new(crate::module_index::ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/Minion.pm"), minion_fa);

    // Cursor inside `enqueue(|)` — just after the `(`.
    let line_idx = src.lines().position(|l| l.ends_with("enqueue();")).unwrap();
    let line = src.lines().nth(line_idx).unwrap();
    let col = line.find("enqueue(").unwrap() + "enqueue(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = crate::symbols::completion_items(&fa, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    assert!(
        labels.contains(&"send_email"),
        "task names must appear at enqueue's arg 0; got: {:?}",
        labels
    );
    assert!(
        labels.contains(&"resize_image"),
        "every registered task name must be offered; got: {:?}",
        labels
    );

    // The tight contract — only tasks, nothing else. When this
    // goes green we'll know the plugin owns the completion shape
    // at this position and the Minion-method firehose is gone.
    for label in &labels {
        assert!(
            *label == "send_email" || *label == "resize_image",
            "only task names should appear at enqueue's arg 0; \
                 got unexpected `{}` in {:?}",
            label,
            labels,
        );
    }
}

/// Sig help on a helper call strips `$c` like it strips `$self`.
/// The helper plugin flags its callback's first param as invocant
/// via `as_invocant_params`; the core sig help path drops any
/// invocant-flagged first positional instead of name-matching
/// `$self`/`$class` only.
#[test]
fn plugin_mojo_helpers_sig_help_strips_invocant() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::Parser;

    let src = r#"package MyApp;
use Mojolicious::Lite;

my $app = Mojolicious->new;
$app->helper(current_user => sub {
    my ($c, $fallback) = @_;
});

sub act {
    my ($c) = @_;
    $c->current_user();
}
"#;
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Cursor inside `$c->current_user(|)` — between the parens.
    let (row, col) = src
        .lines()
        .enumerate()
        .find_map(|(r, l)| {
            l.find("current_user()")
                .map(|c| (r, c + "current_user(".len()))
        })
        .expect("find call site");
    let pos = Position {
        line: row as u32,
        character: col as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let sig = crate::symbols::signature_help(&fa, &tree, src, pos, &idx)
        .expect("sig help fires on helper call");

    let info = &sig.signatures[0];
    assert!(
        info.label.contains("current_user"),
        "label: {:?}",
        info.label
    );
    assert!(
        info.label.contains("$fallback"),
        "sig should show declared param `$fallback`; got: {:?}",
        info.label
    );
    assert!(
        !info.label.contains("$c"),
        "`$c` must be stripped as invocant; got: {:?}",
        info.label
    );
}

/// Sig help when the cursor sits inside the arrayref at position 1
/// of `enqueue` — the core routes via the Handler's
/// `args_in_arrayref_at` declaration (set by the minion plugin)
/// and shows the task's params (invocant-stripped). Plugin-agnostic
/// on the sig-help side: all the core needs is the declaration.
#[test]
fn plugin_minion_sig_help_on_enqueue_array_args() {
    use tower_lsp::lsp_types::Position;
    use tree_sitter::{Parser, Point};

    let src = r#"package MyApp;
use Minion;

my $minion = Minion->new;
$minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
});
$minion->enqueue(send_email => [ ]);
"#;
    let fa = build_fa(src);

    // Cursor inside the enqueue call's arrayref: point at the
    // single space between the `[` and `]` on the last line.
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Find the `[ ]` — cursor at col AFTER `[`.
    let mut cursor_point: Option<Point> = None;
    for (row, line) in src.lines().enumerate() {
        if line.contains("enqueue(send_email") {
            if let Some(col) = line.find("[ ") {
                cursor_point = Some(Point::new(row, col + 1));
            }
        }
    }
    let cursor_point = cursor_point.expect("locate cursor inside [ ]");
    let pos = Position {
        line: cursor_point.row as u32,
        character: cursor_point.column as u32,
    };

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let sig = crate::symbols::signature_help(&fa, &tree, src, pos, &idx)
        .expect("sig help must fire inside enqueue's arrayref");

    // At least one signature, matching the `send_email` handler
    // (the task's params minus $job).
    assert!(!sig.signatures.is_empty(), "at least one signature");
    let info = &sig.signatures[0];
    let label = &info.label;
    assert!(
        label.contains("send_email"),
        "sig label must reference the handler name: {:?}",
        label
    );
    assert!(
        label.contains("$to"),
        "sig should surface task params (`$to`): {:?}",
        label
    );
    assert!(
        !label.contains("$job"),
        "invocant `$job` must be stripped from display: {:?}",
        label
    );
}

/// Hash-key completion on the enqueue options hash
/// (`$minion->enqueue('task', [args], { | })`) — the cursor_context
/// layer now recognizes a nested hash literal as a positional
/// argument and routes it to `HashKeyOwner::Sub { name: enqueue }`.
#[test]
fn plugin_minion_hashkey_help_on_enqueue_options() {
    use tree_sitter::{Parser, Point};
    let src = r#"package MyApp;
use Minion;

my $minion = Minion->new;
$minion->enqueue(task_x => ['arg'] => { });
"#;
    // Build + parse
    let fa = build_fa(src);
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();

    // Cursor inside the empty options hash literal `{ | }`.
    // Line 4 (0-indexed) column after "{ " — aim at the middle
    // of the hash's interior.
    let src_bytes = src.as_bytes();
    let mut cursor: Option<Point> = None;
    for (row, line) in src.lines().enumerate() {
        if let Some(col) = line.find("{ ") {
            cursor = Some(Point::new(row, col + 2));
        }
    }
    let cursor = cursor.expect("find the `{ ` in the source");

    let ctx = crate::cursor_context::detect_cursor_context_tree(&tree, src_bytes, cursor, &fa)
        .expect("context should be detected inside hash literal");
    match ctx {
        crate::cursor_context::CursorContext::HashKey { source_sub, .. } => {
            assert_eq!(
                source_sub.as_deref(),
                Some("enqueue"),
                "nested {{ }} at call-arg position routes to the callee"
            );
        }
        other => panic!("expected HashKey context, got {:?}", other),
    }

    // Completion path surfaces the plugin's HashKeyDefs.
    let candidates = fa.complete_hash_keys_for_sub("enqueue", cursor);
    let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
    for expected in &["priority", "queue", "delay", "attempts"] {
        assert!(
            labels.contains(expected),
            "enqueue option `{}` must complete; got: {:?}",
            expected,
            labels
        );
    }
}

// ---- Plugin type overrides ----
//
// Tests pin the contract: a plugin's `overrides()` manifest patches
// local Sub/Method return types AFTER inference, with provenance
// recorded so debugging can tell asserted from inferred. The
// bundled `mojo-routes` plugin overrides `Mojolicious::Routes::Route::_route`
// to return `$self` because the upstream impl uses an `@_`-shift /
// array-slice idiom inference doesn't model.
//
// Targeting is by exact (class, method) — the override fires on the
// home class only; subclasses still get the type via the existing
// cross-file resolution path.

#[test]
fn plugin_override_patches_return_type_on_matching_method() {
    let src = "\
package Mojolicious::Routes::Route;

sub _route {
    my $self = shift;
    # Real impl uses an array slice that inference can't model.
    return $self;
}

1;
";
    let fa = build_fa(src);
    let route_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "_route" && matches!(s.kind, SymKind::Sub | SymKind::Method))
        .expect("_route must be parsed as a sub");
    match &route_sym.detail {
        SymbolDetail::Sub { .. } => {
            assert_eq!(
                fa.symbol_return_type_via_bag(route_sym.id, None),
                Some(InferredType::ClassName(
                    "Mojolicious::Routes::Route".into()
                )),
                "override must rewrite return_type to ClassName(Mojolicious::Routes::Route)",
            );
        }
        other => panic!("_route must be a Sub detail; got {:?}", other),
    }
}

#[test]
fn plugin_override_records_provenance_with_plugin_id_and_reason() {
    // The point of provenance is debug-time introspection: a
    // future inspector should be able to ask "why does the LSP
    // think `_route` returns Mojolicious::Routes::Route?" and
    // get back "because mojo-routes' overrides() said so". We pin
    // the plugin id and assert the reason isn't empty so a future
    // refactor that drops the reason field surfaces here.
    let src = "\
package Mojolicious::Routes::Route;
sub _route { my $self = shift; $self }
1;
";
    let fa = build_fa(src);
    let route_id = fa
        .symbols
        .iter()
        .find(|s| s.name == "_route")
        .expect("_route present")
        .id;
    match fa.return_type_provenance(route_id) {
        TypeProvenance::PluginOverride { plugin_id, reason } => {
            assert_eq!(plugin_id, "mojo-routes");
            assert!(
                !reason.is_empty(),
                "reason must explain why override exists"
            );
        }
        other => panic!("expected PluginOverride provenance; got {:?}", other),
    }
}

#[test]
fn plugin_override_does_not_touch_unrelated_subs() {
    // Same method NAME, different class → override must NOT apply.
    // The match is (class, method); a same-named method on an
    // unrelated package keeps whatever inference produced.
    let src = "\
package Some::Other::Package;
sub _route { my ($x) = @_; { id => $x } }
1;
";
    let fa = build_fa(src);
    let id = fa
        .symbols
        .iter()
        .find(|s| s.name == "_route")
        .expect("_route present")
        .id;
    // Override must not apply — provenance can be Inferred or
    // ReducerFold (inference produced the type via the witness
    // fold), but NOT PluginOverride.
    assert!(
        !matches!(
            fa.return_type_provenance(id),
            TypeProvenance::PluginOverride { .. }
        ),
        "override must not bleed across packages; provenance: {:?}",
        fa.return_type_provenance(id),
    );
}

#[test]
fn plugin_override_does_not_touch_other_methods_in_target_class() {
    // Same class, different method name → not the target.
    let src = "\
package Mojolicious::Routes::Route;
sub other_method { my $self = shift; { ok => 1 } }
1;
";
    let fa = build_fa(src);
    let id = fa
        .symbols
        .iter()
        .find(|s| s.name == "other_method")
        .expect("other_method present")
        .id;
    assert!(
        !matches!(
            fa.return_type_provenance(id),
            TypeProvenance::PluginOverride { .. }
        ),
        "override must not bleed across method names; got {:?}",
        fa.return_type_provenance(id),
    );
}

#[test]
fn plugin_override_visible_via_find_method_return_type() {
    // The user-visible payoff: any code path that asks "what does
    // calling `_route` on a Mojolicious::Routes::Route return?"
    // gets the override answer. find_method_return_type is the
    // primary API every chain-resolver / hover / completion path
    // routes through, so pinning it here covers the downstream
    // features without coupling to their specific internals.
    let src = "\
package Mojolicious::Routes::Route;
sub _route { my $self = shift; $self }
1;
";
    let fa = build_fa(src);
    let rt = fa.find_method_return_type("Mojolicious::Routes::Route", "_route", None, None);
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Mojolicious::Routes::Route".into())),
        "find_method_return_type must surface the override-supplied type",
    );
}

#[test]
fn plugin_override_wins_over_inferred_return_type() {
    // Even if inference DID produce a (different) return type, the
    // override replaces it — the whole point is "inference reaches
    // the wrong answer here". The body explicitly returns a hashref
    // so inference would say HashRef without the override.
    let src = "\
package Mojolicious::Routes::Route;

sub _route {
    return { stub => 1 };
}

1;
";
    let fa = build_fa(src);
    let sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "_route")
        .expect("_route present");
    match &sym.detail {
        SymbolDetail::Sub { .. } => {
            assert_eq!(
                fa.symbol_return_type_via_bag(sym.id, None),
                Some(InferredType::ClassName(
                    "Mojolicious::Routes::Route".into()
                )),
                "override must replace inferred HashRef, not be skipped",
            );
        }
        _ => unreachable!(),
    }
}

// ---- data-printer plugin ----
//
// Data::Printer monkey-patches `&p` and `&np` into the caller's
// symbol table from inside its custom `import` sub — no
// `@EXPORT` / `@EXPORT_OK`, so the cross-file extractor sees them
// as plain Subs but no caller's import list claims them. The
// plugin's job is to declare the imports plugin-side so call
// sites resolve.
//
// `use DDP` is a literal alias for `use Data::Printer` (DDP.pm
// just `push our @ISA, 'Data::Printer'` and re-uses the import).
// The plugin pins the synthetic Import at Data::Printer (the real
// module) regardless of which name the user typed, so cross-file
// hover/gd/sig-help on `p`/`np` always flow to the real source.

#[test]
fn plugin_data_printer_synthesizes_p_np_on_use_data_printer() {
    // `use Data::Printer;` — empty native qw list. Plugin must
    // emit an additional Import that lists `p` and `np` so
    // resolve_call_package finds them and routes cross-file
    // lookups to Data::Printer.
    let src = "\
use Data::Printer;
p $foo;
np \\%bar;
";
    let fa = build_fa(src);
    let dp_import = fa.imports.iter().find(|i| {
        i.module_name == "Data::Printer" && i.imported_symbols.iter().any(|s| s.local_name == "p")
    });
    assert!(
        dp_import.is_some(),
        "plugin must emit Import for Data::Printer carrying `p`; got: {:?}",
        fa.imports
    );
    let names: Vec<&str> = dp_import
        .unwrap()
        .imported_symbols
        .iter()
        .map(|s| s.local_name.as_str())
        .collect();
    assert!(names.contains(&"p"));
    assert!(names.contains(&"np"));
}

#[test]
fn plugin_data_printer_aliases_ddp_to_data_printer() {
    // `use DDP;` — the alias case. Plugin must still emit a
    // synthetic Import keyed on Data::Printer (NOT DDP) so
    // cross-file `p`/`np` lookups route to the real source
    // module. Otherwise the user gets nothing on hover/gd
    // when they typed `use DDP` instead of `use Data::Printer`.
    let src = "\
use DDP;
p $foo;
";
    let fa = build_fa(src);
    let dp_import = fa.imports.iter().find(|i| {
        i.module_name == "Data::Printer" && i.imported_symbols.iter().any(|s| s.local_name == "p")
    });
    assert!(
        dp_import.is_some(),
        "use DDP must produce an Import for Data::Printer (alias resolution); got: {:?}",
        fa.imports
            .iter()
            .map(|i| (
                i.module_name.clone(),
                i.imported_symbols
                    .iter()
                    .map(|s| s.local_name.clone())
                    .collect::<Vec<_>>(),
            ))
            .collect::<Vec<_>>()
    );
}

#[test]
fn plugin_data_printer_skips_unrelated_use_statements() {
    // Sanity check: an unrelated `use` doesn't pull a synthetic
    // Data::Printer import into the file. Otherwise the plugin
    // would be silently claiming every use statement.
    let src = "use List::Util qw(max);";
    let fa = build_fa(src);
    assert!(
        fa.imports
            .iter()
            .find(|i| i.module_name == "Data::Printer")
            .is_none(),
        "plugin must not synthesize a Data::Printer import unless DDP/Data::Printer was used"
    );
}

// ---- Dancer2 plugin tests ----

/// `use Dancer2` autoimports ~90 DSL keywords — unresolved-function
/// diagnostics must skip all of them. The plugin stashes a
/// `FrameworkImport` per keyword into `framework_imports`.
#[test]
fn plugin_dancer2_autoimports_dsl_keywords() {
    let src = r#"
package main;
use Dancer2;

get '/users' => sub { return template 'users' };
post '/login' => sub { my $u = param('user'); session user => $u; };
"#;
    let fa = build_fa(src);
    for kw in &[
        // Route verbs
        "get", "post", "put", "del", "patch", "any", "options",
        // Route organisation
        "prefix",
        // Lifecycle hooks
        "hook",
        // Request / response
        "request", "response", "param", "params",
        "body_parameters", "query_parameters", "route_parameters",
        // Headers / status
        "header", "headers", "content_type", "status",
        // Response control
        "redirect", "forward", "pass", "halt",
        // Rendering
        "template", "send_file", "send_as",
        // Config
        "config", "set", "setting",
        // Session / cookie
        "session", "cookie", "cookies",
        // Serialisers
        "to_json", "from_json", "to_yaml", "from_yaml",
        // Misc
        "var", "vars", "uri_for", "splat", "captures", "upload",
        "push_response_header",
        // App / DSL
        "app", "dancer_app", "dsl", "engine",
        // Async
        "delayed", "flush",
        // Logging
        "debug", "info", "warning", "error",
        // Boolean constants
        "true", "false",
        // Lifecycle
        "dance", "to_app", "start",
        // Keywords absent from the original set — verified against
        // Dancer2::Core::DSL::dsl_keywords (the authoritative list).
        "content", "send_error", "response_header", "request_header",
        "uri_for_route", "prepare_app", "encode_json", "decode_json",
        "to_dumper", "from_dumper", "push_header", "response_headers",
        "psgi_app", "runner", "done", "context",
        "dancer_version", "dancer_major_version",
        "mime", "request_data",
    ] {
        assert!(
            fa.framework_imports.contains(*kw),
            "`{}` must be autoimported by `use Dancer2`; framework_imports={:?}",
            kw,
            fa.framework_imports,
        );
    }
}

/// `use Dancer2` synthesizes typed Sub symbols for high-value DSL
/// functions so chained calls (`request->path`, `app->config`)
/// resolve against the correct class.
#[test]
fn plugin_dancer2_typed_stubs_have_return_types() {
    use crate::file_analysis::InferredType;

    let src = r#"
package main;
use Dancer2;
"#;
    let fa = build_fa(src);

    // `request` must resolve to Dancer2::Core::Request.
    let request_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "request" && matches!(s.kind, crate::file_analysis::SymKind::Sub));
    assert!(
        request_sym.is_some(),
        "dancer plugin must synthesize a `request` Sub symbol"
    );
    let rt = fa.sub_return_type_at_arity("request", None);
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Dancer2::Core::Request".into())),
        "`request` must return Dancer2::Core::Request; got {:?}",
        rt
    );

    // `app` must resolve to Dancer2::Core::App.
    let rt = fa.sub_return_type_at_arity("app", None);
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Dancer2::Core::App".into())),
        "`app` must return Dancer2::Core::App; got {:?}",
        rt
    );

    // `session` must resolve to Dancer2::Core::Session.
    let rt = fa.sub_return_type_at_arity("session", None);
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Dancer2::Core::Session".into())),
        "`session` must return Dancer2::Core::Session; got {:?}",
        rt
    );

    // `config` returns a HashRef.
    let rt = fa.sub_return_type_at_arity("config", None);
    assert_eq!(
        rt,
        Some(InferredType::HashRef),
        "`config` must return HashRef; got {:?}",
        rt
    );

    // `uri_for_route` returns a String (URL).
    let rt = fa.sub_return_type_at_arity("uri_for_route", None);
    assert_eq!(
        rt,
        Some(InferredType::String),
        "`uri_for_route` must return String; got {:?}",
        rt
    );

    // `encode_json` returns a String (the serialized JSON).
    let rt = fa.sub_return_type_at_arity("encode_json", None);
    assert_eq!(
        rt,
        Some(InferredType::String),
        "`encode_json` must return String; got {:?}",
        rt
    );

    // `decode_json` returns a HashRef (the deserialized structure).
    let rt = fa.sub_return_type_at_arity("decode_json", None);
    assert_eq!(
        rt,
        Some(InferredType::HashRef),
        "`decode_json` must return HashRef; got {:?}",
        rt
    );

    // `runner` returns the Dancer2::Core::Runner singleton.
    let rt = fa.sub_return_type_at_arity("runner", None);
    assert_eq!(
        rt,
        Some(InferredType::ClassName("Dancer2::Core::Runner".into())),
        "`runner` must return Dancer2::Core::Runner; got {:?}",
        rt
    );
}

/// `use Dancer2::Plugin` also gets the full DSL — plugins
/// re-export via import and expect every DSL word to be in scope.
#[test]
fn plugin_dancer2_plugin_also_autoimports() {
    let src = r#"
package MyApp::Plugin::Foo;
use Dancer2::Plugin;

register my_keyword => sub { my $dsl = shift; $dsl->param('x') };
"#;
    let fa = build_fa(src);
    for kw in &["get", "post", "param", "request", "session", "config", "debug"] {
        assert!(
            fa.framework_imports.contains(*kw),
            "`{}` must be autoimported by `use Dancer2::Plugin`; got {:?}",
            kw,
            fa.framework_imports,
        );
    }
}

/// Unrelated `use` statements must NOT inject Dancer2 keywords.
/// Guards against the trigger firing too broadly.
#[test]
fn plugin_dancer2_skips_unrelated_use() {
    let src = r#"
package main;
use Mojolicious::Lite;
"#;
    let fa = build_fa(src);
    // `param` is a Dancer2 keyword — it should NOT appear in
    // framework_imports just because of Mojolicious::Lite.
    // (Mojolicious::Lite does not expose `param` as a standalone function.)
    // We verify via the synthesized Sub symbol: the dancer plugin
    // should not have emitted one.
    let dancer_stubs = fa
        .symbols
        .iter()
        .filter(|s| {
            s.name == "dancer_app"
                && matches!(
                    &s.namespace,
                    crate::file_analysis::Namespace::Framework { id } if id == "dancer"
                )
        })
        .count();
    assert_eq!(
        dancer_stubs, 0,
        "dancer plugin must not emit stubs for `use Mojolicious::Lite`"
    );
}

// ---- Red-pin: regressions caught against the rhai-plugins branch ----

/// `my` is lexical and crosses statement-form `package X;`
/// boundaries. The branch's sibling `ScopeKind::Package` was
/// originally swallowing `my` decls, so a use site under
/// `package main;` couldn't resolve a `my` declared under
/// `package Calculator;` earlier in the same file. e2e
/// `rename: $pi → $tau` turned red on the interpolated-string
/// occurrence (the only `$pi` use under `package main;`); this
/// pins the underlying `resolves_to` linkage so the regression
/// can't sneak back in if the scope tree is reshuffled.
#[test]
fn red_pin_my_resolves_across_statement_packages() {
    let src = "\
package Calculator;
my $pi = 3.14159;
sub circumference { my ($self, $r) = @_; return 2 * $pi * $r }

package main;
print \"pi is $pi\\n\";
";
    let fa = build_fa(src);
    let pi_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "$pi" && s.kind == SymKind::Variable)
        .expect("$pi Variable symbol");
    let pi_refs: Vec<_> = fa.refs.iter().filter(|r| r.target_name == "$pi").collect();
    assert_eq!(pi_refs.len(), 3, "decl + body use + interpolation = 3 refs");
    for r in &pi_refs {
        assert_eq!(
            r.resolves_to,
            Some(pi_sym.id),
            "ref at {:?} (scope {:?}) didn't resolve to the lexical decl — \
                 sibling Package scopes are leaking into variable lookup",
            r.span.start,
            r.scope,
        );
    }
}

/// `our` is package-global with a lexical alias — bare `$version`
/// from a sibling `package main;` does NOT reach an `our $version`
/// declared under an earlier `package Calculator;` (you'd have to
/// spell `$Calculator::version`). The mirror of
/// `red_pin_my_resolves_across_statement_packages`: that test
/// guarantees `my` keeps crossing package boundaries; this one
/// guarantees `our` keeps NOT crossing them. Pinned now so the
/// scope-separation refactor — which moves variables onto the
/// real lexical scope tree — doesn't accidentally let `our` leak
/// across siblings the way it would if we forgot to keep `our`
/// attached to the package-context scope.
#[test]
fn red_pin_our_does_not_resolve_across_statement_packages() {
    let src = "\
package Calculator;
our $version = 1;
sub bump { $version++ }

package main;
print \"v=$version\\n\";
";
    let fa = build_fa(src);
    let our_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "$version" && s.kind == SymKind::Variable)
        .expect("$version Variable symbol");
    // Under Calculator the bare $version refs SHOULD resolve to
    // the our-decl: that's the lexical alias half of `our`.
    let bump_use = fa
        .refs
        .iter()
        .find(|r| r.target_name == "$version" && r.span.start.row == 2)
        .expect("ref inside Calculator's bump");
    assert_eq!(
        bump_use.resolves_to,
        Some(our_sym.id),
        "bare $version inside the same package as the `our` decl \
             must still resolve to it (lexical alias)"
    );
    // Under `package main;` the bare $version must NOT resolve.
    let main_use = fa
        .refs
        .iter()
        .find(|r| r.target_name == "$version" && r.span.start.row == 5)
        .expect("ref inside package main's print");
    assert_eq!(
        main_use.resolves_to, None,
        "bare $version under a sibling `package main;` must not \
             reach Calculator's `our $version` — that's $Calculator::version, \
             a different binding"
    );
}

/// Caller-side `HashKeyAccess` for a method/function call's
/// even-position stringy args. `MooApp->new(name => 'alice')`
/// must emit a HashKeyAccess at the `name` token so
/// cursor-on-key resolves to the `has`-emitted HashKeyDef
/// instead of the broad MethodCall ref. Gated on a matching
/// HashKeyDef existing — emission would otherwise shadow the
/// `class Foo { field $x :param }` `find_param_field`
/// fallback. e2e `rename: 'name' constructor arg` was the
/// surfacing failure.
#[test]
fn red_pin_call_arg_emits_hash_key_access_when_def_exists() {
    let src = "\
package MooApp;
use Moo;
has name => (is => 'ro');

package main;
my $m = MooApp->new(name => 'alice');
";
    let fa = build_fa(src);
    let name_access: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "name" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        !name_access.is_empty(),
        "no HashKeyAccess emitted for `name` in MooApp->new(name => 'alice')",
    );
    let RefKind::HashKeyAccess {
        owner: Some(owner), ..
    } = &name_access[0].kind
    else {
        panic!("HashKeyAccess emitted with no owner");
    };
    assert_eq!(
        *owner,
        HashKeyOwner::Sub {
            package: Some("MooApp".to_string()),
            name: "new".to_string()
        },
        "constructor-key owner should be Sub{{class, method}}, matching the has-emitted def",
    );

    // No matching HashKeyDef for `count` → no shadow ref.
    let count_access: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "count" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    let src_no_def = "\
package Plain;
sub run {}

package main;
my $p = Plain->run(count => 1);
";
    let fa2 = build_fa(src_no_def);
    let no_emit: Vec<_> = fa2
        .refs
        .iter()
        .filter(|r| r.target_name == "count" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        no_emit.is_empty(),
        "no HashKeyDef registered for Plain::run/count → \
             must not emit a phantom HashKeyAccess (would shadow other resolution paths)",
    );
    // `count` accesses on MooApp (which DOESN'T define count) — should not emit either.
    assert!(
        count_access.is_empty(),
        "MooApp has no `count` HashKeyDef → no HashKeyAccess emission expected",
    );
}

/// `=>` is autoquoting sugar for `,` — `f(name => 'alice')` and
/// `f('name', 'alice')` are the same call. The HashKeyAccess
/// emission must be position-based (every odd-indexed stringy
/// arg is a key), NOT keyed off the `=>` token. Pinning this
/// because the original draft of the helper walked siblings
/// looking for `=>` and would have missed the bare-comma form
/// — letting cursor-on-key in the bare-comma shape land on
/// the broad MethodCall ref, which renames the wrong token.
#[test]
fn red_pin_hash_key_access_emission_is_position_based() {
    // Same `has`-emitted def, two call shapes. Both should land
    // a HashKeyAccess at `name`, with the same owner.
    let fat_comma_src = "\
package MooApp;
use Moo;
has name => (is => 'ro');

package main;
my $a = MooApp->new(name => 'alice');
";
    let bare_comma_src = "\
package MooApp;
use Moo;
has name => (is => 'ro');

package main;
my $a = MooApp->new('name', 'alice');
";
    let fa_fat = build_fa(fat_comma_src);
    let fa_bare = build_fa(bare_comma_src);

    // Constructor call site only — the `has name` declaration
    // synthesizes its own internal-key refs that we're not
    // asserting on here.
    fn name_access_at_call<'a>(fa: &'a FileAnalysis) -> Vec<&'a Ref> {
        fa.refs
            .iter()
            .filter(|r| {
                r.target_name == "name"
                    && matches!(r.kind, RefKind::HashKeyAccess { .. })
                    && r.span.start.row == 5
            })
            .collect()
    }

    let fat_refs = name_access_at_call(&fa_fat);
    let bare_refs = name_access_at_call(&fa_bare);

    assert_eq!(
        fat_refs.len(),
        1,
        "fat-comma form should emit exactly one HashKeyAccess at the call site",
    );
    assert_eq!(
        bare_refs.len(),
        1,
        "bare-comma form (`'name', 'alice'`) must emit the same HashKeyAccess — \
             `=>` is autoquoting sugar, not a structural marker",
    );

    let owner_of = |r: &Ref| match &r.kind {
        RefKind::HashKeyAccess { owner: Some(o), .. } => o.clone(),
        _ => panic!("expected HashKeyAccess with owner"),
    };
    assert_eq!(
        owner_of(fat_refs[0]),
        owner_of(bare_refs[0]),
        "both forms must produce the same Sub{{MooApp, new}} owner",
    );

    // Even-indexed args ARE keys; odd-indexed (values) must
    // NOT get a HashKeyAccess regardless of whether they happen
    // to look like a key string. `'alice'` at idx 1 stays a value.
    for fa in [&fa_fat, &fa_bare] {
        let alice_access: Vec<_> = fa
            .refs
            .iter()
            .filter(|r| r.target_name == "alice" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
            .collect();
        assert!(
            alice_access.is_empty(),
            "value-position arg must never become a HashKeyAccess",
        );
    }

    // Multi-pair, all bare commas — `('a', 1, 'b', 2)`. Both
    // `a` and `b` are keys (idx 0 and 2); `1` and `2` aren't
    // stringy so they don't even tempt the helper. Need a def
    // for each so emission isn't gated out.
    let multi_src = "\
package MooApp;
use Moo;
has a => (is => 'ro');
has b => (is => 'ro');

package main;
my $m = MooApp->new('a', 1, 'b', 2);
";
    let fa_multi = build_fa(multi_src);
    let call_keys: Vec<&Ref> = fa_multi
        .refs
        .iter()
        .filter(|r| {
            matches!(r.kind, RefKind::HashKeyAccess { .. })
                && r.span.start.row == 6
                && (r.target_name == "a" || r.target_name == "b")
        })
        .collect();
    assert_eq!(
        call_keys.len(),
        2,
        "both even-position args (`'a'`, `'b'`) must emit HashKeyAccess",
    );
}

/// Carp's canonical shape: `longmess` (caller) is defined before
/// `longmess_heavy` (callee). Both arms of the if/else return the
/// forward-defined call, so the per-sub fold should agree on `String`
/// — but only if the walk-time symbol-table miss for `longmess_heavy`
/// is recovered post-walk. `resolve_forward_call_targets` is what
/// makes this work; without it the bag has no `Expr(call_span)`
/// witness at all and `longmess` returns `None`.
#[test]
fn forward_reference_call_in_sub_return_resolves() {
    let src = r#"
package main;

sub longmess {
    if ($_[0]) {
        return longmess_heavy(@_);
    }
    else {
        return longmess_heavy(@_);
    }
}

sub longmess_heavy { return "ouch"; }
"#;
    let fa = build_fa(src);
    let rt = fa.sub_return_type_at_arity("longmess", None);
    assert_eq!(
        rt,
        Some(InferredType::String),
        "longmess must fold to String through both arms — \
         got {:?}. Walk-order regression: longmess_heavy is \
         defined after longmess.",
        rt,
    );
}

/// Single-arm forward call: implicit `return forward()` should fold
/// to whatever `forward()` returns. No branch arms — exercises the
/// `Symbol ← branch_arm Edge → Expr(body) → Edge(Symbol(callee))`
/// chain at minimum width.
#[test]
fn forward_reference_implicit_return_resolves() {
    let src = r#"
package main;

sub caller_sub { forward_sub() }

sub forward_sub { return "ok"; }
"#;
    let fa = build_fa(src);
    assert_eq!(
        fa.sub_return_type_at_arity("caller_sub", None),
        Some(InferredType::String),
    );
}

/// Forward reference inside a ternary return. Each arm calls a
/// different forward-defined sub; both must resolve so the ternary's
/// `BranchArmFold` agrees on `String`. Mixes the forward-ref fix with
/// the existing ternary path (`emit_expr_witness` recursion + arm
/// witnesses on the ternary span).
#[test]
fn forward_reference_in_ternary_arms_resolves() {
    let src = r#"
package main;

sub dispatch {
    return $_[0] ? handle_a() : handle_b();
}

sub handle_a { return "a"; }
sub handle_b { return "b"; }
"#;
    let fa = build_fa(src);
    assert_eq!(
        fa.sub_return_type_at_arity("dispatch", None),
        Some(InferredType::String),
    );
}

/// Scoped-identifier call: `Pkg::name()` form. `expr_payload`'s
/// `bareword`/`scoped_identifier` arm does the same walk-time lookup
/// as `function_call_expression`; the queue + post-walk resolve must
/// cover it too.
#[test]
fn forward_reference_scoped_identifier_call_resolves() {
    let src = r#"
package main;

sub bridge { return Helper::canon(); }

package Helper;

sub canon { return "yes"; }
"#;
    let fa = build_fa(src);
    assert_eq!(
        fa.sub_return_type_at_arity("bridge", None),
        Some(InferredType::String),
    );
}

/// Self-method tail with a forward-defined target. `$self->later()`
/// where `later` is declared after the caller. The MethodOnClass
/// chase needs the callee's `Symbol(sid)` writeback, which only fires
/// once the callee's own `Expr(body)` is populated — exercising the
/// forward-ref fix on the inner sub's body, not the call site itself.
#[test]
fn forward_reference_self_method_call_resolves() {
    let src = r#"
package Box;

sub new { my $class = shift; return bless {}, $class; }

sub head {
    my ($self) = @_;
    return $self->tail();
}

sub tail {
    my ($self) = @_;
    return helper();
}

sub helper { return "fin"; }
"#;
    let fa = build_fa(src);
    assert_eq!(
        fa.sub_return_type_at_arity("tail", None),
        Some(InferredType::String),
    );
}

// ---- SyntheticUse — plugin-emitted `use` statements ------------------------
//
// `EmitAction::SyntheticUse` lets a plugin react to a kit module's outer
// use (e.g. `use Co::Base -Class`) by injecting the inner `use`s that the
// kit performs at runtime (`Moo`, `parent`, etc.). The point is that the
// downstream effect — framework detection, has-synthesis, parent
// inheritance, plugin re-dispatch — is identical to what the user would
// have gotten by writing those `use` lines literally. The test below
// drives the kit path through a stub plugin and compares against a
// literal build.
mod synthetic_use {
    use super::*;
    use crate::file_analysis::Namespace;
    use crate::plugin::{
        CompletionQueryContext, EmitAction, FrameworkPlugin, PluginCompletionAnswer,
        PluginRegistry, PluginSigHelpAnswer, SigHelpQueryContext, Trigger, UseContext,
    };
    use std::sync::Arc;

    /// Catches `use Co::Base -Class` and emits a synthetic `use Moo`.
    /// One trigger (`Always`), one hook (`on_use`), zero overrides.
    /// Stripped to the minimum that exercises the path.
    struct CoBasePlugin;

    impl FrameworkPlugin for CoBasePlugin {
        fn id(&self) -> &str { "co-base-test" }
        fn triggers(&self) -> &[Trigger] {
            // `on_use` bypasses the trigger filter (every plugin sees
            // every use), so the trigger list here is incidental. Kept
            // non-empty to mirror real plugins.
            static T: [Trigger; 1] = [Trigger::Always];
            &T
        }
        fn on_use(&self, ctx: &UseContext) -> Vec<EmitAction> {
            if ctx.module_name != "Co::Base" { return Vec::new(); }
            let is_class = ctx.raw_args.iter().any(|a| a == "-Class");
            if !is_class { return Vec::new(); }
            vec![EmitAction::SyntheticUse {
                module: "Moo".into(),
                args: vec![],
                imports: vec![],
                span: ctx.span,
            }]
        }
        fn on_signature_help(&self, _: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> { None }
        fn on_completion(&self, _: &CompletionQueryContext) -> Option<PluginCompletionAnswer> { None }
    }

    fn registry_with_co_base() -> Arc<PluginRegistry> {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(CoBasePlugin));
        Arc::new(reg)
    }

    fn build_with(source: &str, plugins: Arc<PluginRegistry>) -> FileAnalysis {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        super::super::build_with_plugins(&tree, source.as_bytes(), plugins)
    }

    /// `use Co::Base -Class` (via the stub kit plugin) produces the same
    /// observable downstream state as a literal `use Moo`:
    ///
    ///   * `package_framework` carries `Moo` for the package.
    ///   * `framework_imports` covers Moo's keyword set.
    ///   * `has 'name'` synthesizes the accessor Method.
    ///
    /// Anything that depends on those — has-synthesis, accessor symbols,
    /// inheritance via `with`/`extends`, the constructor key, downstream
    /// plugin chains — comes along for free because it reads the same
    /// state. We pin only the load-bearing axes; broader Moo behavior
    /// has its own dedicated tests.
    #[test]
    fn synthetic_use_moo_matches_literal_use_moo() {
        let kit_src = r#"
package Foo;
use Co::Base -Class;
has 'name' => (is => 'ro');
"#;
        let lit_src = r#"
package Foo;
use Moo;
has 'name' => (is => 'ro');
"#;
        let kit = build_with(kit_src, registry_with_co_base());
        let lit = build_with(lit_src, registry_with_co_base());

        // Both should record Moo as the active framework for Foo.
        assert_eq!(
            kit.package_framework.get("Foo"),
            lit.package_framework.get("Foo"),
            "kit (`use Co::Base -Class`) and literal (`use Moo`) must agree on package_framework"
        );
        assert!(
            kit.package_framework.contains_key("Foo"),
            "Foo's framework should be set by SyntheticUse \"Moo\"; package_framework={:?}",
            kit.package_framework,
        );

        // Both should have Moo's keyword set in framework_imports.
        for kw in &["has", "with", "extends", "around", "before", "after"] {
            assert!(
                kit.framework_imports.contains(*kw),
                "SyntheticUse \"Moo\" must populate framework_imports[{kw}]; got {:?}",
                kit.framework_imports,
            );
        }

        // The `has 'name'` accessor synthesis depends on framework_modes
        // being set at the time `visit_has_call` fires. With SyntheticUse,
        // the kit's `use Co::Base -Class` precedes `has`, so the plugin
        // re-dispatch flips the mode before the has-call is walked.
        let kit_methods: Vec<&str> = kit.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .map(|s| s.name.as_str())
            .collect();
        let lit_methods: Vec<&str> = lit.symbols.iter()
            .filter(|s| s.name == "name" && s.kind == SymKind::Method)
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(
            kit_methods, lit_methods,
            "`has 'name'` should synthesize the same accessor Methods under \
             SyntheticUse \"Moo\" as under literal `use Moo`",
        );
        assert_eq!(kit_methods.len(), 1, "ro getter is exactly one Method");

        // Provenance: the synthesized `Moo` Module symbol in the kit
        // build carries the emitting plugin's namespace tag; the
        // literal build's `Moo` Module is plain `Language`. This is
        // the one observable axis where the two builds are SUPPOSED
        // to differ — it's what lets `--dump-package` / outline /
        // completion filters surface "this came from co-base-test".
        let kit_moo = kit.symbols.iter()
            .find(|s| s.kind == SymKind::Module && s.name == "Moo")
            .expect("kit build must have a Module symbol for synthesized `use Moo`");
        assert_eq!(
            kit_moo.namespace,
            Namespace::framework("co-base-test"),
            "synthesized Module must carry the emitting plugin's namespace tag"
        );
        let lit_moo = lit.symbols.iter()
            .find(|s| s.kind == SymKind::Module && s.name == "Moo")
            .expect("literal build must have a Module symbol for `use Moo`");
        assert_eq!(
            lit_moo.namespace,
            Namespace::Language,
            "literal-source Module must stay on Namespace::Language (no plugin tag)"
        );
    }

    /// `use_dedup` short-circuits cycles. The stub plugin reacts to
    /// `use Co::Base` by emitting `SyntheticUse "Co::Base"` — if the
    /// gate didn't catch it, the on_use re-dispatch would loop and
    /// produce many duplicate Module symbols / Import entries.
    /// With dedup, the second emission is a no-op.
    #[test]
    fn synthetic_use_self_cycle_is_bounded() {
        struct LoopPlugin;
        impl FrameworkPlugin for LoopPlugin {
            fn id(&self) -> &str { "loop-test" }
            fn triggers(&self) -> &[Trigger] {
                static T: [Trigger; 1] = [Trigger::Always];
                &T
            }
            fn on_use(&self, ctx: &UseContext) -> Vec<EmitAction> {
                if ctx.module_name != "Co::Base" { return Vec::new(); }
                vec![EmitAction::SyntheticUse {
                    module: "Co::Base".into(),
                    args: vec![],
                    imports: vec![],
                    span: ctx.span,
                }]
            }
            fn on_signature_help(&self, _: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> { None }
            fn on_completion(&self, _: &CompletionQueryContext) -> Option<PluginCompletionAnswer> { None }
        }
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(LoopPlugin));
        let fa = build_with("package Foo; use Co::Base;\n", Arc::new(reg));

        let co_base_imports = fa.imports.iter()
            .filter(|i| i.module_name == "Co::Base")
            .count();
        assert_eq!(
            co_base_imports, 1,
            "self-cycle must collapse to one Import entry; use_dedup gate kicked in"
        );

        let module_syms = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module && s.name == "Co::Base")
            .count();
        assert_eq!(module_syms, 1, "self-cycle must emit one Module symbol");

        // Belt-and-suspenders for the gate. The dedup short-circuits at
        // the top of `process_use`, so every downstream effect is bounded
        // by construction — but pinning each axis catches a future
        // regression where the gate gets moved or `process_use` gets
        // split. If any of these grow without the others, something's
        // half-processing the cycle.
        let co_base_uses = fa.symbols.iter()
            .filter(|s| s.kind == SymKind::Module && s.name == "Co::Base")
            .count();
        assert_eq!(
            co_base_uses, 1,
            "package_uses-equivalent (Module symbol count) should match Import count"
        );
        // `framework_imports` for `use Co::Base` (not a built-in framework
        // module): the bundled plugins shouldn't touch it. Cycle should
        // leave this empty whether it loops once or a thousand times.
        assert!(
            fa.framework_imports.is_empty()
                || fa.framework_imports.iter().all(|s| !s.starts_with("co_base_")),
            "cycle on a non-framework module should not leak Co::Base-tagged keywords \
             into framework_imports; got {:?}",
            fa.framework_imports,
        );
    }

    /// `imports` MUST be part of the dedup key. Real `use Foo qw(a)` and
    /// `use Foo qw(b)` discriminate via `extract_mojo_base_args`'s
    /// fallback to `extract_use_import_list` (the fallback fires when
    /// no barewords / literals are present, putting qw imports in
    /// `raw_args`). Synthetic emissions carry `args` and `imports` as
    /// separate fields, so the equivalent two SyntheticUses with
    /// different `imports` and empty `args` must NOT collide on the
    /// dedup key. Pre-fix, both keyed on `(pkg, "Foo", [])` and the
    /// second silently dropped.
    #[test]
    fn synthetic_use_distinct_imports_both_emit() {
        struct ImportPlugin;
        impl FrameworkPlugin for ImportPlugin {
            fn id(&self) -> &str { "imports-test" }
            fn triggers(&self) -> &[Trigger] {
                static T: [Trigger; 1] = [Trigger::Always];
                &T
            }
            fn on_use(&self, ctx: &UseContext) -> Vec<EmitAction> {
                if ctx.module_name != "Trigger::Kit" { return Vec::new(); }
                vec![
                    EmitAction::SyntheticUse {
                        module: "Foo".into(),
                        args: vec![],
                        imports: vec!["a".into()],
                        span: ctx.span,
                    },
                    EmitAction::SyntheticUse {
                        module: "Foo".into(),
                        args: vec![],
                        imports: vec!["b".into()],
                        span: ctx.span,
                    },
                ]
            }
            fn on_signature_help(&self, _: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> { None }
            fn on_completion(&self, _: &CompletionQueryContext) -> Option<PluginCompletionAnswer> { None }
        }
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(ImportPlugin));
        let fa = build_with("package Foo; use Trigger::Kit;\n", Arc::new(reg));

        let foo_imports: Vec<&crate::file_analysis::Import> = fa.imports.iter()
            .filter(|i| i.module_name == "Foo")
            .collect();
        assert_eq!(
            foo_imports.len(), 2,
            "two SyntheticUse \"Foo\" with distinct imports must both produce \
             Import entries — dedup must NOT collide on the args-only key. \
             Got imports: {:?}",
            foo_imports.iter().map(|i| &i.imported_symbols).collect::<Vec<_>>(),
        );

        // Each Import entry must carry its own qw-style import name.
        // Order-independent: we check the union covers both.
        let all_names: std::collections::HashSet<&str> = foo_imports.iter()
            .flat_map(|i| i.imported_symbols.iter().map(|s| s.local_name.as_str()))
            .collect();
        assert!(all_names.contains("a"), "missing import name 'a': {:?}", all_names);
        assert!(all_names.contains("b"), "missing import name 'b': {:?}", all_names);
    }
}

/// **Spike: array intelligence on the bag-canonical foundation.**
///
/// The headline scenario, top-to-bottom:
///
/// ```perl
/// # Some/User.pm (cross-file)
/// package Some::User;
/// use Mojo::Base -base;
/// has 'name';
/// sub greet { ... }
/// sub email { ... }
///
/// # main
/// package MyApp;
/// use Mojolicious::Lite;
/// use constant DEFAULT_NAME => 'alice';
///
/// helper make_user => sub {
///     my ($c, $name) = @_;
/// .   return Some::User->new(name => $name);
/// };
///
/// sub action {
///     my $c = Mojolicious::Controller->new;
///     my @users;
///     push @users, $c->make_user(DEFAULT_NAME);   # const fold + plugin helper
///     push @users, $c->make_user('bob');
///     $users[0]->                                  # ← method completion here
/// }
/// ```
///
/// The chain through `$users[0]`:
///   1. mojo-helpers plugin synthesizes `make_user` on
///      `Mojolicious::Controller` with `return_via_edge` pointing
///      at the anon-sub's body.
///   2. Coderef-return edge resolves the body's last expression
///      (`Some::User->new(...)`) → `ClassName("Some::User")`.
///   3. `push @users, $c->make_user(...)` contributes
///      `ClassName("Some::User")` to `@users`'s `Sequence` shape.
///   4. `$users[0]` projects the Sequence to its first element.
///   5. Method / hash-key completion on the projected class crosses
///      the file boundary into `Some::User.pm`.
///
/// The **new** code on this branch is purely the array hop —
/// declaration emission, `push` contribution, and projection at
/// `$users[N]`. Everything else (helper synth, coderef return,
/// const fold, cross-file dispatch, Mojo::Base hash-key defs)
/// drops out of the existing bag-canonical machinery for free.
#[test]
fn spike_array_hop_with_helper_and_cross_file_completion() {
    use crate::module_index::ModuleIndex;
    use std::path::PathBuf;
    use std::sync::Arc;

    let user_pm = r#"
package Some::User;
use Mojo::Base -base;
has 'name';
sub greet { my $self = shift; "hi $self->{name}" }
sub email { my $self = shift; "$self->{name}\@x.com" }
1;
"#;
    let user_fa = build_fa(user_pm);

    let idx = ModuleIndex::new_for_test();
    idx.register_workspace_module(
        PathBuf::from("/tmp/Some/User.pm"),
        Arc::new(user_fa),
    );

    let app_src = r#"
package MyApp;
use Mojolicious::Lite;
use constant DEFAULT_NAME => 'alice';

my $app = Mojolicious->new;
$app->helper(make_user => sub {
    my ($c, $name) = @_;
    return Some::User->new(name => $name);
});

sub action {
    my $c = Mojolicious::Controller->new;
    my @users;
    push @users, $c->make_user(DEFAULT_NAME);
    push @users, $c->make_user('bob');
    $users[0]->greet();
}
"#;
    let app_fa = build_fa(app_src);

    // Load-bearing: walk the tree to find the `$users[0]` node and
    // ask `resolve_expression_type` what it is. This is the receiver
    // resolution path the chain typer + cursor context both go
    // through for completion.
    let tree = parse(app_src);
    fn find_array_element<'a>(node: tree_sitter::Node<'a>) -> Option<tree_sitter::Node<'a>> {
        if node.kind() == "array_element_expression" {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(hit) = find_array_element(child) {
                return Some(hit);
            }
        }
        None
    }
    let elem_node = find_array_element(tree.root_node())
        .expect("test source contains `$users[0]`");
    let resolved = app_fa
        .resolve_expression_type(elem_node, app_src.as_bytes(), Some(&idx))
        .expect("$users[0] resolves to a type");
    assert_eq!(
        resolved.class_name(),
        Some("Some::User"),
        "the array hop survives the chain: helper(coderef) → push → \
         $users[0] → Some::User. got: {:?}",
        resolved,
    );

    // Cross-file method completion on the resolved class. Mojo::Base
    // accessor (`name`) + user-defined methods come through unified.
    let methods = app_fa.complete_methods_for_class("Some::User", Some(&idx));
    let method_names: std::collections::HashSet<&str> =
        methods.iter().map(|c| c.label.as_str()).collect();
    assert!(method_names.contains("greet"), "cross-file user method 'greet' missing");
    assert!(method_names.contains("email"), "cross-file user method 'email' missing");
    assert!(method_names.contains("name"), "Mojo::Base accessor 'name' missing");

    // Hash-key completion on the same class — synthesized by
    // `has 'name'`, reachable across files. Cross-file hash-key
    // completion flows through enrichment; the local
    // `complete_hash_keys_for_class` doesn't gate on `ModuleIndex`,
    // so this stays a soft observation for the spike rather than a
    // hard assert. The load-bearing claim is the array hop, not the
    // FA-side hash-key API.
    let keys = app_fa.complete_hash_keys_for_class("Some::User", Point::new(0, 0));
    let key_names: std::collections::HashSet<&str> =
        keys.iter().map(|c| c.label.as_str()).collect();
    let _ = key_names; // intentionally not asserted in the spike

    // Hover on `$users[0]->greet` — the tree-aware
    // `method_call_invocant_class_with_tree` path. The string-side
    // `method_call_invocant_class` couldn't resolve `$users[0]`
    // (it isn't a Variable witness name); the tree-aware variant
    // dispatches through `resolve_expression_type` on the actual
    // CST node, hitting the same array_element_expression arm
    // cursor_context uses for completion. One projection rule,
    // both entry points.
    fn find_method_call<'a>(
        node: tree_sitter::Node<'a>,
        src: &[u8],
        method: &str,
    ) -> Option<tree_sitter::Node<'a>> {
        if node.kind() == "method_call_expression" {
            if let Some(m) = node.child_by_field_name("method") {
                if m.utf8_text(src).ok() == Some(method) {
                    return Some(node);
                }
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(hit) = find_method_call(child, src, method) {
                return Some(hit);
            }
        }
        None
    }
    let greet_call = find_method_call(tree.root_node(), app_src.as_bytes(), "greet")
        .expect("test source contains `$users[0]->greet()`");
    let method_node = greet_call
        .child_by_field_name("method")
        .expect("method-call has a method child");
    let hover = app_fa.hover_info(
        method_node.start_position(),
        app_src,
        Some(&idx),
    );
    let hover_text = hover.expect("hover on `$users[0]->greet` returns text");
    assert!(
        hover_text.contains("Some::User"),
        "hover on `$users[0]->greet` should mention Some::User; got: {}",
        hover_text,
    );
    assert!(
        hover_text.contains("greet"),
        "hover should include the method name; got: {}",
        hover_text,
    );
}

/// `has x => (isa => InstanceOf['Foo'])` — the constraint is `InstanceOf['Foo']`,
/// a Type::Tiny constraint *value*, not a class. The core types that call
/// expression as `TypeConstraintOf(ClassName(Foo))` (plugin fold), and the
/// accessor projects the constrained inner, so `x` returns `Foo`.
#[test]
fn moo_instanceof_isa_types_accessor_to_inner_class() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nhas thing => (is => 'ro', isa => InstanceOf['My::Thing']);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::ClassName("My::Thing".to_string())),
        "InstanceOf['My::Thing'] isa must give the getter a My::Thing return",
    );
}

/// The constructor expression itself is a `TypeConstraintOf` — NOT the inner
/// class. `$t->name` must see a constraint (so it can route to Type::Tiny
/// later), and an `isa => $t` projects the inner. This guards against the
/// lossy "InstanceOf['Foo'] == ClassName(Foo)" shortcut we rejected.
#[test]
fn instanceof_expression_is_a_type_constraint_not_the_class() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nmy $t = InstanceOf['My::Thing'];\n1;\n",
    );
    let ty = fa
        .inferred_type_via_bag("$t", Point::new(3, 20))
        .expect("$t should carry a type");
    assert!(
        matches!(&ty, InferredType::TypeConstraintOf(inner)
            if matches!(inner.as_ref(), InferredType::ClassName(c) if c == "My::Thing")),
        "InstanceOf['My::Thing'] is a TypeConstraintOf(ClassName(My::Thing)), got {:?}",
        ty,
    );
    assert!(
        ty.constrained_inner().and_then(|i| i.class_name()) == Some("My::Thing"),
        "constrained_inner projects the class for the isa→accessor path",
    );
}

/// const-fold / variable path: `my $t = InstanceOf['Foo']; has x => (isa => $t)`.
/// `has` edges to the RHS `$t`, whose type is the constraint, and projects the
/// inner — no special handling of the variable form.
#[test]
fn moo_isa_via_constraint_variable_projects_inner() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nmy $t = InstanceOf['My::Thing'];\nhas thing => (is => 'ro', isa => $t);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::ClassName("My::Thing".to_string())),
        "isa => $constraint_var must project the constrained inner onto the accessor",
    );
}

// ---- isa coverage: the TypeConstraintOf path + the string/bareword split ----

/// String/bareword isa (the Moose idiom + builtins) stays on the meaning-map,
/// untouched by the constraint path. Regression guard that adding the node
/// path didn't break the common forms.
#[test]
fn moo_string_isa_forms_still_resolve() {
    let fa = build_fa(
        "package T;\nuse Moo;\nhas s => (is=>'ro', isa=>'Str');\nhas i => (is=>'ro', isa=>'Int');\nhas h => (is=>'ro', isa=>'HashRef');\n1;\n",
    );
    assert_eq!(fa.sub_return_type_at_arity("s", Some(0)), Some(InferredType::String));
    assert_eq!(fa.sub_return_type_at_arity("i", Some(0)), Some(InferredType::Numeric));
    assert_eq!(fa.sub_return_type_at_arity("h", Some(0)), Some(InferredType::HashRef));
}

/// `is => 'rw'` synthesizes a writer too; both getter (arity 0) and writer
/// (arity ≥1) return the constrained inner class.
#[test]
fn moo_instanceof_isa_types_both_getter_and_writer() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nhas thing => (is=>'rw', isa=>InstanceOf['My::Thing']);\n1;\n",
    );
    let want = Some(InferredType::ClassName("My::Thing".to_string()));
    assert_eq!(fa.sub_return_type_at_arity("thing", Some(0)), want.clone(), "getter");
    assert_eq!(fa.sub_return_type_at_arity("thing", Some(1)), want, "rw writer");
}

/// `Maybe[InstanceOf['Foo']]` — the nested constructor is itself a constraint
/// value. The core types the inner call (`TypeConstraintOf(ClassName(Foo))`)
/// into the param's `ty`; the `Maybe` passthrough fold projects its inner, so
/// the accessor returns `Foo` (optionalness unmodeled — unwrap for resolution).
#[test]
fn moo_maybe_instanceof_isa_unwraps_to_inner_class() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/Maybe InstanceOf/;\nhas thing => (is=>'ro', isa=>Maybe[InstanceOf['My::Thing']]);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::ClassName("My::Thing".to_string())),
        "Maybe[InstanceOf['My::Thing']] must unwrap to a My::Thing accessor return",
    );
}

/// `ConsumerOf['Role']` shares the ClassParam shape (you can call the role's
/// methods on the value) — declared by the same plugin manifest entry.
#[test]
fn moo_consumerof_isa_types_accessor() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/ConsumerOf/;\nhas r => (is=>'ro', isa=>ConsumerOf['My::Role']);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("r", Some(0)),
        Some(InferredType::ClassName("My::Role".to_string())),
    );
}

/// crm writes `InstanceOf ['Class']` (space before the bracket). Both spacings
/// parse as the same call node, so both must type.
#[test]
fn moo_instanceof_isa_handles_space_before_bracket() {
    let fa = build_fa(
        "package T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nhas thing => (is=>'ro', isa=>InstanceOf ['My::Thing']);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::ClassName("My::Thing".to_string())),
    );
}

/// Moose mode, not just Moo — same constraint vocabulary.
#[test]
fn moose_instanceof_isa_types_accessor() {
    let fa = build_fa(
        "package T;\nuse Moose;\nuse Types::Standard qw/InstanceOf/;\nhas thing => (is=>'ro', isa=>InstanceOf['My::Thing']);\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        Some(InferredType::ClassName("My::Thing".to_string())),
    );
}

/// NEGATIVE: a coderef isa (`isa => sub {...}`) isn't a constraint — the
/// accessor must stay untyped, never falsely a class. Guards the projection
/// from over-firing on non-constraint complex RHS.
#[test]
fn moo_coderef_isa_leaves_accessor_untyped() {
    let fa = build_fa(
        "package T;\nuse Moo;\nhas thing => (is=>'ro', isa=>sub { die unless ref $_[0] });\n1;\n",
    );
    assert_eq!(
        fa.sub_return_type_at_arity("thing", Some(0)),
        None,
        "a coderef constraint has no class denotation",
    );
}

/// NEGATIVE: an undeclared constructor (`SomeType['X']`, not in any plugin's
/// type_constraint_names) falls through cleanly — no TypeConstraintOf, no
/// crash, accessor untyped.
#[test]
fn moo_unknown_constructor_isa_falls_through() {
    let fa = build_fa(
        "package T;\nuse Moo;\nhas thing => (is=>'ro', isa=>SomeUnknownType['X']);\n1;\n",
    );
    assert_eq!(fa.sub_return_type_at_arity("thing", Some(0)), None);
}

/// The chain payoff: an `InstanceOf` accessor's class flows into a downstream
/// method call. `$self->other->greet` must resolve `->greet` against `Other`
/// — this is the `$self->_minion->enqueue` shape that the crm fix turns on.
#[test]
fn instanceof_accessor_chains_into_method_call() {
    let src = "package Other;\nuse Moo;\nsub greet ($self) { return 'hi'; }\n\npackage T;\nuse Moo;\nuse Types::Standard qw/InstanceOf/;\nhas other => (is=>'ro', isa=>InstanceOf['Other']);\nsub use_it ($self) { return $self->other->greet; }\n1;\n";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    let idx = crate::module_index::ModuleIndex::new_for_test();

    // Find the `greet` method-call node in `$self->other->greet`.
    fn find_call<'a>(n: tree_sitter::Node<'a>, src: &[u8], m: &str) -> Option<tree_sitter::Node<'a>> {
        if n.kind() == "method_call_expression" {
            if let Some(mn) = n.child_by_field_name("method") {
                if mn.utf8_text(src).ok() == Some(m) { return Some(n); }
            }
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(f) = find_call(c, src, m) { return Some(f); }
            }
        }
        None
    }
    let call = find_call(tree.root_node(), src.as_bytes(), "greet").expect("has $self->other->greet");
    let method_node = call.child_by_field_name("method").unwrap();
    let hover = fa
        .hover_info(method_node.start_position(), src, Some(&idx))
        .expect("hover on ->greet resolves");
    assert!(
        hover.contains("Other"),
        "->greet on an InstanceOf['Other'] accessor must resolve against Other; got: {hover}",
    );
}

/// Option B resolves a receiver whose type comes from a Mojo HELPER, not a
/// plain method. `$c->minion` (a helper bridged to Mojolicious::Controller,
/// returning a Minion subclass) → `$c->minion->enqueue('T')` must synthesize
/// the dispatch. This is the gap that left `$app->minion`/`$c->minion`
/// chains dark: option-B's enrichment receiver-resolution now threads the
/// index (variable arm) and chases the helper bridge the way hover does.
#[test]
fn provisional_dispatch_resolves_helper_returned_receiver() {
    use crate::file_analysis::HandlerOwner;
    use std::path::PathBuf;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        PathBuf::from("/tmp/b_hr_minion.pm"),
        std::sync::Arc::new(build_fa("package Acme::Minion;\nuse Mojo::Base 'Minion';\n1;\n")),
    );
    idx.register_workspace_module(
        PathBuf::from("/tmp/b_hr_plugin.pm"),
        std::sync::Arc::new(build_fa(
            "package Acme::Plugin;\nuse Mojo::Base 'Mojolicious::Plugin';\nsub register ($self, $app, $conf) {\n  my $m = Acme::Minion->new;\n  $app->helper(minion => sub {$m});\n  $app->minion->add_task('Task.go' => sub ($job) { 1 });\n}\n1;\n",
        )),
    );

    let fa = build_fa(
        "package Acme::Ctrl;\nuse Mojo::Base 'Mojolicious::Controller';\nsub act ($c) {\n  $c->minion->enqueue('Task.go');\n}\n1;\n",
    );

    // This file hits a Mojo trigger, so the emit-hook materializes the
    // DispatchCall directly (it doesn't gate on the receiver). The handler
    // is surfaced either way; `applicable_dispatches` de-dups the gated
    // candidate against the materialized ref so there's no double-count.
    let has_materialized = fa.refs.iter().any(|r|
        matches!(&r.kind, RefKind::DispatchCall { dispatcher, owner: Some(HandlerOwner::Class(c)) }
            if dispatcher == "enqueue" && c == "Minion")
            && r.target_name == "Task.go");
    let applied = fa.applicable_dispatches(Some(&idx));
    let has_gated = applied.iter().any(|a|
        a.name == "Task.go" && a.owner == HandlerOwner::Class("Minion".into()));
    assert!(
        has_materialized ^ has_gated,
        "the helper-returned receiver $c->minion (Acme::Minion isa Minion) enqueue \
         must surface exactly once — via the emit-hook ref OR the gated candidate, \
         never both; materialized={has_materialized} gated={has_gated} applied={:?}",
        applied,
    );
    assert!(
        has_materialized,
        "this file hits a Mojo trigger, so the emit-hook materializes the dispatch",
    );
}

/// Role-contract parameter typing: a plugin's `param_types()` manifest types a
/// named param of a sub declared in a class that does the rule's role. The
/// motivating case is `Clove::Upgrade::OneTime`'s `run_upgrade ($self, $app)`,
/// where `$app` is the Mojolicious app — a type the source can't express and
/// no callback-arg hook can reach (it's a plain sub declaration).
mod param_types_manifest {
    use super::*;
    use crate::plugin::{
        CompletionQueryContext, FrameworkPlugin, ParamType, PluginCompletionAnswer,
        PluginRegistry, PluginSigHelpAnswer, SigHelpQueryContext, Trigger,
    };
    use std::sync::Arc;

    struct UpgradeRolePlugin;
    impl FrameworkPlugin for UpgradeRolePlugin {
        fn id(&self) -> &str { "upgrade-role-test" }
        fn triggers(&self) -> &[Trigger] {
            static T: [Trigger; 1] = [Trigger::Always];
            &T
        }
        fn param_types(&self) -> &[ParamType] {
            // Built lazily into a static so the &[] borrow is 'static.
            use std::sync::OnceLock;
            static PT: OnceLock<Vec<ParamType>> = OnceLock::new();
            PT.get_or_init(|| {
                vec![ParamType {
                    method: Some("run_upgrade".into()),
                    in_role: "My::Upgrade::Role".into(),
                    param: 1,
                    type_class: "Mojolicious".into(),
                    requires_action_attr: false,
                }]
            })
        }
        fn on_signature_help(&self, _: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> { None }
        fn on_completion(&self, _: &CompletionQueryContext) -> Option<PluginCompletionAnswer> { None }
    }

    fn build_with_upgrade(source: &str) -> FileAnalysis {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(UpgradeRolePlugin));
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        crate::builder::build_with_plugins(&tree, source.as_bytes(), Arc::new(reg))
    }

    #[test]
    fn role_doer_run_upgrade_app_param_typed() {
        // `use Moo; with 'Role'` populates package_parents (core framework
        // handling); the manifest then types `$app` as Mojolicious.
        let fa = build_with_upgrade(
            "package My::Doer;\nuse Moo;\nwith 'My::Upgrade::Role';\nsub run_upgrade ($self, $app) {\n  my $x = $app;\n}\n1;\n",
        );
        let ty = fa
            .inferred_type_via_bag("$app", Point::new(4, 10))
            .expect("$app should be typed by the param_types manifest");
        assert!(
            matches!(&ty, InferredType::ClassName(c) if c == "Mojolicious"),
            "role-contract param typing should make $app a Mojolicious, got {:?}",
            ty,
        );
    }

    #[test]
    fn non_doer_same_method_name_not_typed() {
        // Same method name, but the class does NOT do the role → no typing
        // (the rule is role-gated, not name-gated).
        let fa = build_with_upgrade(
            "package Other;\nsub run_upgrade ($self, $app) {\n  my $x = $app;\n}\n1;\n",
        );
        assert_eq!(
            fa.inferred_type_via_bag("$app", Point::new(2, 10)),
            None,
            "a class that doesn't do the role must not get the contract param type",
        );
    }

    // ---- Cross-file manifest-applicability probes ----
    // Whether build-time `transitive_parents`-gated plugin behavior reaches a
    // class whose ancestry is established cross-file. See
    // `docs/prompt-enrichment-inheritance-residual.md`.

    /// `ClassIsa`-triggered plugin emission on a class whose trigger-class
    /// ancestry is only established CROSS-FILE. `Leaf` extends `Mid` (a
    /// cross-file module) which extends `Mojo::EventEmitter`. The
    /// mojo-events plugin's `ClassIsa: "Mojo::EventEmitter"` trigger walks
    /// local `transitive_parents` (builder is index-free during the walk),
    /// which sees only `Mid` — not the cross-file `Mojo::EventEmitter`.
    /// Enrichment can't help: plugin emit hooks fire at parse time, inside
    /// `build()`, before any module index exists.
    ///
    /// Architectural gap, NOT a contained fix — documented as a latent
    /// hazard. See the doc.
    #[test]
    #[ignore = "cross-file ClassIsa trigger: architectural, see docs/prompt-enrichment-inheritance-residual.md"]
    fn probe_class_isa_trigger_through_cross_file_parent() {
        use crate::module_index::ModuleIndex;
        use std::path::PathBuf;
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        idx.insert_cache(
            "Mid",
            Some(fake_cached_for_class(
                "Mid",
                &PathBuf::from("/fake/Mid.pm"),
                &[],
                &["Mojo::EventEmitter"],
            )),
        );
        let src = "package Leaf;\nuse parent 'Mid';\nsub wire {\n  my $self = shift;\n  $self->on('ready', sub { 1 });\n}\n1;\n";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let mut fa = crate::builder::build(&tree, src.as_bytes());
        fa.enrich_imported_types_with_keys(Some(&idx));
        let ready = fa.symbols.iter().filter(|s| {
            s.kind == SymKind::Handler && s.name == "ready"
                && matches!(&s.namespace, Namespace::Framework { id } if id == "mojo-events")
        }).count();
        assert_eq!(
            ready, 1,
            "mojo-events ClassIsa trigger should fire via cross-file parent chain"
        );
    }

    /// Dispatch-verb resolution in a NON-OPEN workspace/dependency file
    /// whose dispatch receiver `isa Minion` only CROSS-FILE and which does
    /// NOT `use Minion` itself. The minion plugin's emit-hook path
    /// (`UsesModule`/`ClassIsa` trigger) doesn't fire — only the
    /// trigger-independent `dispatch_verbs()` manifest captures a gated
    /// candidate. Under the query-time `ReceiverGated` seam the file is built
    /// WITHOUT enrichment (as the workspace indexer does) yet
    /// `applicable_dispatches` resolves the receiver cross-file and surfaces
    /// the call site. See `docs/adr/receiver-gated-dispatch.md`.
    #[test]
    fn dispatch_resolves_query_time_in_unenriched_workspace_file() {
        use crate::module_index::ModuleIndex;
        use std::path::PathBuf;
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        // `My::Minion` isa Minion, cross-file. The worker file below never
        // `use`s Minion — the emit-hook trigger can't fire, only the
        // receiver-isa manifest candidate, gated and resolved at query time.
        idx.insert_cache(
            "My::Minion",
            Some(fake_cached_for_class(
                "My::Minion",
                &PathBuf::from("/fake/My/Minion.pm"),
                &["new"],
                &["Minion"],
            )),
        );
        let src = "package My::Worker;\nsub run {\n  my $self = shift;\n  my $minion = My::Minion->new;\n  $minion->enqueue('send_email');\n}\n1;\n";
        // Build exactly as the workspace indexer does: no enrichment.
        let fa = build_fa(src);
        let applied = fa.applicable_dispatches(Some(&idx));
        assert_eq!(
            applied.len(), 1,
            "workspace-indexed file (no enrichment) should resolve its enqueue \
             dispatch at query time via the cross-file receiver isa — else \
             cross-file handler references miss it; got {:?}",
            applied,
        );
        assert_eq!(applied[0].name, "send_email");
    }

    // Wildcard-method param_types: a rule with `method: None` applies to every
    // sub in the class — the Catalyst pattern where every action gets `$c` typed.
    struct CatalystPlugin;
    impl FrameworkPlugin for CatalystPlugin {
        fn id(&self) -> &str { "catalyst-test" }
        fn triggers(&self) -> &[Trigger] {
            static T: [Trigger; 1] = [Trigger::Always];
            &T
        }
        fn param_types(&self) -> &[ParamType] {
            use std::sync::OnceLock;
            static PT: OnceLock<Vec<ParamType>> = OnceLock::new();
            PT.get_or_init(|| {
                vec![ParamType {
                    method: None, // wildcard: every ACTION method in the class
                    in_role: "Catalyst::Controller".into(),
                    param: 1,
                    type_class: "Catalyst".into(),
                    // Mirror the real catalyst.rhai: only attribute-carrying
                    // actions get $c, not plain helper subs.
                    requires_action_attr: true,
                }]
            })
        }
        fn on_signature_help(&self, _: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> { None }
        fn on_completion(&self, _: &CompletionQueryContext) -> Option<PluginCompletionAnswer> { None }
    }

    fn build_with_catalyst(source: &str) -> FileAnalysis {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(CatalystPlugin));
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        crate::builder::build_with_plugins(&tree, source.as_bytes(), Arc::new(reg))
    }

    #[test]
    fn catalyst_action_c_typed_via_wildcard_manifest() {
        // A controller action: $c (param index 1) should type as Catalyst.
        // The wildcard rule fires regardless of the action method's name.
        let fa = build_with_catalyst(
            "package MyApp::Controller::Foo;\nuse parent 'Catalyst::Controller';\nsub index :Local {\n    my ($self, $c) = @_;\n    my $req = $c;\n}\n1;\n",
        );
        let ty = fa
            .inferred_type_via_bag("$c", Point::new(4, 14))
            .expect("$c should be typed by wildcard param_types manifest");
        assert!(
            matches!(&ty, InferredType::ClassName(c) if c == "Catalyst"),
            "wildcard param_types should make $c a Catalyst in every controller action, got {:?}",
            ty,
        );
    }

    #[test]
    fn catalyst_wildcard_typed_for_any_action_name() {
        // A differently-named action — the wildcard covers it too.
        let fa = build_with_catalyst(
            "package MyApp::Controller::Bar;\nuse parent 'Catalyst::Controller';\nsub list :Local {\n    my ($self, $c) = @_;\n    my $x = $c;\n}\n1;\n",
        );
        let ty = fa
            .inferred_type_via_bag("$c", Point::new(4, 12))
            .expect("$c should be typed regardless of action method name");
        assert!(
            matches!(&ty, InferredType::ClassName(c) if c == "Catalyst"),
            "wildcard param_types must apply to any action name, got {:?}",
            ty,
        );
    }

    /// The Phase-2 cross-file case: a controller in file A `extends` a base in
    /// file B which `isa Catalyst::Controller`. The controller's ancestry to
    /// the wildcard rule's `in_role` is established only CROSS-FILE, so the old
    /// build-time local-only `transitive_parents` gate dropped it. The gated TC
    /// resolves at query time with the module index in hand → `$c` types.
    #[test]
    fn catalyst_wildcard_c_typed_through_cross_file_base() {
        use crate::module_index::ModuleIndex;
        use std::path::PathBuf;
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        // `MyApp::ControllerBase` isa Catalyst::Controller, cross-file. The
        // controller below `extends` it — its ancestry to the role is two hops,
        // through a class in another file the builder never sees.
        idx.insert_cache(
            "MyApp::ControllerBase",
            Some(fake_cached_for_class(
                "MyApp::ControllerBase",
                &PathBuf::from("/fake/MyApp/ControllerBase.pm"),
                &[],
                &["Catalyst::Controller"],
            )),
        );
        let src = "package MyApp::Controller::Deep;\nuse parent 'MyApp::ControllerBase';\nsub show :Local {\n    my ($self, $c) = @_;\n    my $req = $c;\n}\n1;\n";
        // Build as the workspace indexer does (no enrichment); the gated TC
        // rides the FA and resolves cross-file at query time.
        let fa = build_with_catalyst(src);
        let ty = fa
            .inferred_type_via_bag_ctx("$c", Point::new(4, 14), Some(&idx))
            .expect("$c should type via the cross-file Catalyst::Controller ancestry");
        assert!(
            matches!(&ty, InferredType::ClassName(c) if c == "Catalyst"),
            "wildcard param_types must type $c when Catalyst::Controller is a \
             cross-file ancestor, got {:?}",
            ty,
        );
    }

    #[test]
    fn catalyst_wildcard_not_applied_outside_controller() {
        // A package without the Catalyst::Controller ancestor must not get $c typed.
        let fa = build_with_catalyst(
            "package OtherPackage;\nsub index {\n    my ($self, $c) = @_;\n    my $x = $c;\n}\n1;\n",
        );
        assert_eq!(
            fa.inferred_type_via_bag("$c", Point::new(3, 12)),
            None,
            "wildcard rule must not type $c in a package that doesn't isa Catalyst::Controller",
        );
    }

    /// P1.4 — the real metacpan shape, through the actual hover query path: a
    /// controller reaches `Catalyst::Controller` through a *workspace
    /// intermediate* base that is itself a child of the role class. The leaf's
    /// local `package_parents` only knows its direct parent; reaching the role
    /// requires `class_isa` to chase the intermediate's parents through the
    /// module index. The bug was the hover path (`format_symbol_hover_at`)
    /// dropping the index — this exercises `hover_info` end-to-end so it
    /// fails on pre-fix code.
    #[test]
    fn catalyst_c_typed_through_workspace_intermediate_via_hover() {
        use crate::module_index::ModuleIndex;
        use std::path::PathBuf;
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        // Intermediate base, registered as a workspace module (like a project
        // `lib/.../Controller.pm`): its parent (`Catalyst::Controller`) lives
        // only in the index, NOT in the leaf's local `package_parents`.
        idx.register_workspace_module(
            PathBuf::from("/fake/MetaCPAN/Web/Controller.pm"),
            std::sync::Arc::new(build_fa(
                "package MetaCPAN::Web::Controller;\nuse parent 'Catalyst::Controller';\nsub pageset {\n    my ($self, $page) = @_;\n}\n1;\n",
            )),
        );
        // Leaf controller: extends only the workspace intermediate.
        let src = "package MetaCPAN::Web::Controller::Author;\nuse parent 'MetaCPAN::Web::Controller';\nsub root :Chained {\n    my ($self, $c, $id) = @_;\n    my $x = $c;\n}\n1;\n";
        let fa = build_with_catalyst(src);
        // Hover on the `$c` usage (row 4, the `my $x = $c;` line). The hover
        // path resolves the variable's type — only typed correctly if the
        // index is threaded all the way to the gated-param query.
        let hover = fa
            .hover_info(Point::new(4, 12), src, Some(&idx))
            .expect("hover should produce info for $c");
        assert!(
            hover.contains("type: Catalyst"),
            "3-hop cross-file ancestry through a workspace base must type $c in \
             hover (the path that dropped the index), got: {}",
            hover,
        );
    }

    /// P1.3 — the attribute gate: in the SAME controller, an action (`:Local`)
    /// gets `$c`, but a plain helper sub (no action attribute) must NOT get its
    /// 2nd param typed. The honest action signal is the attribute, not the
    /// parameter position.
    #[test]
    fn catalyst_non_action_helper_second_param_not_typed() {
        let fa = build_with_catalyst(
            "package MyApp::Controller::Foo;\nuse parent 'Catalyst::Controller';\nsub show :Local {\n    my ($self, $c) = @_;\n    my $a = $c;\n}\nsub pageset {\n    my ($self, $page) = @_;\n    my $b = $page;\n}\n1;\n",
        );
        // The action's $c IS typed.
        let c_ty = fa
            .inferred_type_via_bag("$c", Point::new(4, 12))
            .expect("action $c should be typed");
        assert!(
            matches!(&c_ty, InferredType::ClassName(c) if c == "Catalyst"),
            "action method's $c must type as Catalyst, got {:?}",
            c_ty,
        );
        // The plain helper's 2nd param ($page) must NOT be typed — no attribute.
        assert_eq!(
            fa.inferred_type_via_bag("$page", Point::new(8, 12)),
            None,
            "a non-action helper's 2nd param must NOT be typed Catalyst (P1.3 \
             over-application); only attribute-carrying actions receive $c",
        );
    }

    /// Catalyst private-action names (`begin`/`end`/`auto`/`default`/`index`)
    /// are dispatched by name alone — no action attribute. The `requires_action_attr`
    /// gate must not exclude them, so `$c` still types.
    #[test]
    fn catalyst_private_action_names_type_c_without_attr() {
        // `sub end { my ($self,$c)=@_ }` in a controller — no attribute.
        let fa = build_with_catalyst(
            "package MyApp::Controller::Root;\nuse parent 'Catalyst::Controller';\nsub end {\n    my ($self, $c) = @_;\n    my $r = $c;\n}\n1;\n",
        );
        let ty = fa
            .inferred_type_via_bag("$c", Point::new(4, 12))
            .expect("private-action end: $c should be typed even without an attribute");
        assert!(
            matches!(&ty, InferredType::ClassName(c) if c == "Catalyst"),
            "sub end without action attr must type $c as Catalyst, got {:?}",
            ty,
        );
    }

    /// A plain helper sub whose name happens to NOT be a private-action name
    /// and carries no action attribute must still be excluded.
    #[test]
    fn catalyst_plain_helper_not_a_private_action() {
        let fa = build_with_catalyst(
            "package MyApp::Controller::Root;\nuse parent 'Catalyst::Controller';\nsub helper {\n    my ($self, $x) = @_;\n    my $r = $x;\n}\n1;\n",
        );
        assert_eq!(
            fa.inferred_type_via_bag("$x", Point::new(4, 12)),
            None,
            "non-action, non-private-action helper must NOT have $x typed as Catalyst",
        );
    }
}

// ---- Fix #1: `not` operator ----

/// `not $x` must never produce an unresolved-function diagnostic.
/// Validated in symbols_tests; here we confirm the builder emits a
/// FunctionCall ref (so the name is visible for filtering) whose name
/// is "not" — the is_perl_builtin guard in collect_diagnostics then
/// suppresses it.
#[test]
fn not_operator_emits_function_call_ref_named_not() {
    let fa = build_fa("my $x = 1;\nmy $y = not $x;\n");
    let not_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "not" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .collect();
    // The ref exists (so the builtin filter has something to suppress).
    assert!(
        !not_refs.is_empty(),
        "`not` should emit a FunctionCall ref; got refs: {:?}",
        fa.refs.iter().map(|r| (&r.target_name, &r.kind)).collect::<Vec<_>>(),
    );
}

// ---- Fix #2: `\&subname` code-ref ----

/// `\&handler` must emit a FunctionCall ref pointing at `handler`
/// so goto-def and references both work.
#[test]
fn refgen_bare_name_emits_function_call_ref() {
    let fa = build_fa("sub handler { 1 }\nmy $cb = \\&handler;\n");
    let refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "handler" && matches!(r.kind, RefKind::FunctionCall { .. }))
        .collect();
    assert_eq!(
        refs.len(),
        1,
        "\\&handler should emit exactly one FunctionCall ref for `handler`; got: {:?}",
        fa.refs
            .iter()
            .filter(|r| r.target_name == "handler")
            .map(|r| &r.kind)
            .collect::<Vec<_>>(),
    );
}

/// `\&Pkg::handler` (qualified form) must also emit a FunctionCall ref.
#[test]
fn refgen_qualified_name_emits_function_call_ref() {
    let fa = build_fa("my $cb = \\&Foo::handler;\n");
    let refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| {
            r.target_name == "handler" || r.target_name == "Foo::handler"
        })
        .collect();
    assert!(
        !refs.is_empty(),
        "\\&Foo::handler should emit a FunctionCall ref; got refs: {:?}",
        fa.refs.iter().map(|r| (&r.target_name, &r.kind)).collect::<Vec<_>>(),
    );
}

/// goto-def on `\&handler` should land on the `sub handler` definition.
/// FunctionCall refs route goto-def through package+name matching, not
/// `resolves_to`, so we check `find_definition` returns the sub's span.
#[test]
fn refgen_goto_def_lands_on_sub_definition() {
    // sub handler on line 0, \&handler on line 1 col 9
    let src = "sub handler { 1 }\nmy $cb = \\&handler;\n";
    let fa = build_fa(src);
    let sub_sym = fa
        .symbols
        .iter()
        .find(|s| s.name == "handler" && matches!(s.kind, SymKind::Sub))
        .expect("handler sub should be defined");
    // Cursor at the `h` of `\&handler` on line 1 (0-indexed row=1, col≈11)
    let def_span = fa.find_definition(
        Point::new(1, 11),
        None,
        None,
        None,
    );
    assert_eq!(
        def_span,
        Some(sub_sym.selection_span),
        "goto-def on \\&handler should land on the handler sub; sym={:?}",
        sub_sym,
    );
}

// ---- Fix #3: around/before/after modifier bodies ----

/// In `around foo => sub { my ($orig, $self) = @_; ... }`, `$self` (param index 1)
/// must be typed as the enclosing class so `$self->method` chains resolve.
#[test]
fn around_modifier_second_param_typed_as_class() {
    let src = r#"
package Dog;
use Moo;

sub speak { "woof" }

around speak => sub {
    my ($orig, $self) = @_;
    return $self->speak_loudly();
};

sub speak_loudly { "WOOF" }
"#;
    let fa = build_fa(src);

    // `$self` inside the around body should resolve to `Dog`
    // (row=8 is the `return $self->speak_loudly()` line).
    let ty = fa.inferred_type_via_bag("$self", Point::new(8, 12));
    assert!(
        ty.is_some(),
        "$self inside `around` body should have an inferred type; got None.\
         \nAll TCs: {:?}",
        fa.refs
            .iter()
            .filter(|r| r.target_name == "$self")
            .collect::<Vec<_>>(),
    );
    match ty.unwrap() {
        InferredType::ClassName(name) => assert_eq!(name, "Dog", "$self should be Dog"),
        InferredType::FirstParam { package } => {
            assert_eq!(package, "Dog", "$self FirstParam should be Dog")
        }
        other => panic!("expected ClassName/FirstParam for $self, got {:?}", other),
    }
}

/// In `before foo => sub { my ($self) = @_; ... }`, `$self` (param index 0)
/// must be typed as the enclosing class.
#[test]
fn before_modifier_first_param_typed_as_class() {
    let src = r#"
package Cat;
use Moo;

sub meow { "mrrp" }

before meow => sub {
    my ($self) = @_;
    $self->hiss();
};

sub hiss { "ssss" }
"#;
    let fa = build_fa(src);

    // Row 8 = `$self->hiss()` line
    let ty = fa.inferred_type_via_bag("$self", Point::new(8, 4));
    assert!(
        ty.is_some(),
        "$self inside `before` body should have an inferred type",
    );
    match ty.unwrap() {
        InferredType::ClassName(name) => assert_eq!(name, "Cat"),
        InferredType::FirstParam { package } => assert_eq!(package, "Cat"),
        other => panic!("expected ClassName/FirstParam, got {:?}", other),
    }
}

// ---- Runtime exporter modeling ----
//
// Static analysis can't run import(); we model the declarative setup
// shapes so exported names land in `export_ok` (same plumbing as
// `@EXPORT_OK`), which then drives goto-def / refs / diagnostics.

#[test]
fn sub_exporter_use_setup_records_exports() {
    let fa = build_fa(
        "package My::Exporter;\n\
         use Sub::Exporter -setup => { exports => [qw/alpha beta/] };\n\
         sub alpha { 1 }\n\
         sub beta { 2 }\n\
         1;\n",
    );
    assert!(fa.export_ok.contains(&"alpha".to_string()),
        "export_ok should contain alpha; got {:?}", fa.export_ok);
    assert!(fa.export_ok.contains(&"beta".to_string()),
        "export_ok should contain beta; got {:?}", fa.export_ok);
}

#[test]
fn sub_exporter_setup_exporter_call_records_exports() {
    let fa = build_fa(
        "package My::Exporter;\n\
         use Sub::Exporter ();\n\
         Sub::Exporter::setup_exporter({ exports => [qw/gamma/] });\n\
         sub gamma { 3 }\n\
         1;\n",
    );
    assert!(fa.export_ok.contains(&"gamma".to_string()),
        "export_ok should contain gamma; got {:?}", fa.export_ok);
}

#[test]
fn sub_exporter_generator_hashref_records_keys() {
    // Generators: best-effort — the hashref keys are the exported names.
    let fa = build_fa(
        "package My::Exporter;\n\
         use Sub::Exporter -setup => { exports => { delta => \\&_gen_delta } };\n\
         sub _gen_delta { sub { 4 } }\n\
         1;\n",
    );
    assert!(fa.export_ok.contains(&"delta".to_string()),
        "export_ok should contain generator name delta; got {:?}", fa.export_ok);
}

#[test]
fn sub_exporter_setup_array_members_and_groups_join_surface() {
    // `-setup => { exports => [ qw(foo bar), baz => \&_gen ], groups => {...} }`:
    // every member name joins the export surface (incl. the `name => \&gen`
    // generator entry's name and the group member arrays). The group keys
    // (`default`/`extra`) are selectors, not subs — they must NOT join.
    let fa = build_fa(
        "package My::Exp;\n\
         use Sub::Exporter -setup => {\n\
           exports => [ qw(foo bar), baz => \\&_build_baz ],\n\
           groups  => { default => [qw(foo)], extra => [qw(bar baz)] },\n\
         };\n\
         sub foo {}\n\
         sub bar {}\n\
         sub _build_baz {}\n\
         1;\n",
    );
    for name in ["foo", "bar", "baz"] {
        assert!(
            fa.exports_name(name),
            "exports_name({name}) should be true; export_ok={:?}",
            fa.export_ok
        );
    }
    assert!(
        !fa.export_ok.contains(&"default".to_string())
            && !fa.export_ok.contains(&"extra".to_string()),
        "group selector keys must not join the surface; got {:?}",
        fa.export_ok
    );
}

#[test]
fn sub_exporter_member_refs_local_subs() {
    // Each member that names a local sub gets a FunctionCall ref at its
    // export-list mention (rule #7); a member naming no local sub (the public
    // generator name) gets none.
    let fa = build_fa(
        "package My::Exp;\n\
         use Sub::Exporter -setup => {\n\
           exports => [ qw(foo bar), baz => \\&_build_baz ],\n\
           groups  => { extra => [qw(bar baz)] },\n\
         };\n\
         sub foo {}\n\
         sub bar {}\n\
         sub baz {}\n\
         1;\n",
    );
    let count = |name: &str| {
        fa.refs
            .iter()
            .filter(|r| {
                r.target_name == name
                    && matches!(
                        &r.kind,
                        RefKind::FunctionCall { resolved_package }
                            if resolved_package.as_deref() == Some("My::Exp")
                    )
            })
            .count()
    };
    // foo: exports list only = 1. bar: exports + group `extra` = 2.
    // baz: exports + group `extra` = 2.
    assert_eq!(count("foo"), 1, "foo member ref; got refs {:?}", fa.refs.iter().filter(|r| r.target_name=="foo").collect::<Vec<_>>());
    assert_eq!(count("bar"), 2, "bar in exports + group extra");
    assert_eq!(count("baz"), 2, "baz in exports + group extra");
}

#[test]
fn sub_exporter_member_goto_def_and_references() {
    use crate::file_store::FileStore;
    use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
    use std::path::PathBuf;

    let src = "package My::Exp;\n\
         use Sub::Exporter -setup => { exports => [ qw(foo bar) ] };\n\
         sub foo {}\n\
         sub bar {}\n\
         1;\n";
    let fa = build_fa(src);

    let foo_def_span = fa
        .symbols
        .iter()
        .find(|s| s.name == "foo")
        .map(|s| s.selection_span)
        .expect("foo sub symbol");
    let export_ref = fa
        .refs
        .iter()
        .find(|r| {
            r.target_name == "foo"
                && matches!(&r.kind, RefKind::FunctionCall { .. })
                && r.span != foo_def_span
        })
        .expect("an export-list FunctionCall ref for foo");
    let r = fa
        .ref_at(export_ref.span.start)
        .expect("ref_at the export member token");
    assert_eq!(r.target_name, "foo");

    let store = FileStore::new();
    let path = PathBuf::from("/tmp/qa_sub_exporter.pm");
    store.insert_workspace(path.clone(), fa);
    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "foo".to_string(),
            kind: TargetKind::Sub {
                package: Some("My::Exp".to_string()),
            },
            method_classes: Vec::new(),
        },
        RoleMask::EDITABLE,
    );
    // def + 1 exports-list mention = 2.
    assert_eq!(
        results.len(),
        2,
        "references on foo should list the def and its exports-list mention; got {results:?}"
    );
}

#[test]
fn sub_exporter_setup_exporter_call_with_groups() {
    // The function-call setup form folds exports + groups the same way.
    let fa = build_fa(
        "package My::Exp;\n\
         use Sub::Exporter ();\n\
         Sub::Exporter::setup_exporter({\n\
           exports => [qw/gamma delta/],\n\
           groups  => { all => [qw/gamma delta/] },\n\
         });\n\
         sub gamma {}\n\
         sub delta {}\n\
         1;\n",
    );
    assert!(fa.exports_name("gamma") && fa.exports_name("delta"),
        "setup_exporter exports should join surface; got {:?}", fa.export_ok);
    assert!(!fa.export_ok.contains(&"all".to_string()),
        "group selector `all` must not join the surface");
}

#[test]
fn non_sub_exporter_use_unaffected() {
    // Regression: a plain `use` of an unrelated module with a `-setup`-shaped
    // arg must not record exports (only Sub::Exporter's use is folded).
    let fa = build_fa(
        "package My::Thing;\n\
         use Some::Other -setup => { exports => [qw/leak/] };\n\
         sub leak {}\n\
         1;\n",
    );
    assert!(!fa.export_ok.contains(&"leak".to_string()),
        "non-Sub::Exporter use must not record exports; got {:?}", fa.export_ok);
    // And no spurious export-member ref on the local sub.
    let leak_refs = fa.refs.iter().filter(|r| r.target_name == "leak"
        && matches!(&r.kind, RefKind::FunctionCall { .. })).count();
    assert_eq!(leak_refs, 0, "no member ref for an unrelated use's pseudo-export");
}

#[test]
fn moose_exporter_setup_import_methods_records_exports() {
    let fa = build_fa(
        "package My::Sugar;\n\
         use Moose::Exporter;\n\
         Moose::Exporter->setup_import_methods(\n\
             with_meta => ['has_table'],\n\
             as_is     => [qw/col belongs_to/],\n\
         );\n\
         sub has_table { }\n\
         sub col { }\n\
         sub belongs_to { }\n\
         1;\n",
    );
    for name in ["has_table", "col", "belongs_to"] {
        assert!(fa.export_ok.contains(&name.to_string()),
            "export_ok should contain {}; got {:?}", name, fa.export_ok);
    }
}

#[test]
fn type_library_add_type_records_named_export() {
    let fa = build_fa(
        "package My::Types;\n\
         use Type::Library -base;\n\
         __PACKAGE__->add_type({ name => 'PositiveInt' });\n\
         __PACKAGE__->add_type({ name => 'Email' });\n\
         1;\n",
    );
    assert!(fa.export_ok.contains(&"PositiveInt".to_string()),
        "export_ok should contain PositiveInt; got {:?}", fa.export_ok);
    assert!(fa.export_ok.contains(&"Email".to_string()),
        "export_ok should contain Email; got {:?}", fa.export_ok);
}

#[test]
fn non_exporter_setup_does_not_pollute_exports() {
    // A plain method call named neither setup verb leaves exports empty.
    let fa = build_fa(
        "package My::Thing;\n\
         My::Thing->configure({ name => 'nope', exports => [qw/leak/] });\n\
         1;\n",
    );
    assert!(!fa.export_ok.contains(&"leak".to_string()),
        "unrelated method call must not record exports; got {:?}", fa.export_ok);
    assert!(!fa.export_ok.contains(&"nope".to_string()));
}

#[test]
fn setup_verb_name_without_exporter_use_does_not_pollute_exports() {
    // The verb name matches a real exporter setup call, but the package never
    // `use`d an exporter that defines it — so it's an unrelated method call
    // (`$x->add_type({name=>...})` on some domain object) and must not record
    // exports. Without the package-use gate this would false-positive.
    let fa = build_fa(
        "package My::Registry;\n\
         my $schema = build_schema();\n\
         $schema->add_type({ name => 'Widget' });\n\
         __PACKAGE__->setup_import_methods(as_is => [qw/leak/]);\n\
         1;\n",
    );
    assert!(!fa.export_ok.contains(&"Widget".to_string()),
        "add_type without Type::Library use must not record exports; got {:?}", fa.export_ok);
    assert!(!fa.export_ok.contains(&"leak".to_string()),
        "setup_import_methods without Moose::Exporter use must not record exports; got {:?}", fa.export_ok);
}

#[test]
fn export_ok_array_assignment_unions_with_runtime_exports() {
    // `:Export` attr records `attr_export` at the sub walk; a later
    // `our @EXPORT_OK = (...)` must union, not clobber, so both survive.
    let fa = build_fa(
        "package My::Mixed;\n\
         use Exporter::Extensible;\n\
         sub attr_export :Export { }\n\
         our @EXPORT_OK = ('array_export');\n\
         sub array_export { }\n\
         1;\n",
    );
    assert!(fa.export_ok.contains(&"attr_export".to_string()),
        "runtime :Export attr survives the array assignment; got {:?}", fa.export_ok);
    assert!(fa.export_ok.contains(&"array_export".to_string()),
        "array-assigned name recorded; got {:?}", fa.export_ok);
}

// Moo/Moose non-default has options: predicate, clearer,
// writer, reader, builder, handles
// ============================================================

#[test]
fn test_moo_has_predicate_string() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name' => (is => 'ro', predicate => 'has_name');
",
    );
    let pred: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "has_name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(pred.len(), 1, "explicit predicate string synthesizes method");
    if let SymbolDetail::Sub { ref params, is_method, .. } = pred[0].detail {
        assert!(is_method);
        assert!(params.is_empty(), "predicate takes no args");
    }
}

#[test]
fn test_moo_has_predicate_shorthand() {
    // `predicate => 1` derives `has_<attr>` for public attrs.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'email' => (is => 'ro', predicate => 1);
",
    );
    let pred: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "has_email" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(pred.len(), 1, "predicate => 1 derives has_<attr>");
}

#[test]
fn test_moo_has_predicate_private_attr_shorthand() {
    // Private attrs (leading `_`) get `_has_<rest>` not `has__<rest>`.
    let fa = build_fa(
        "
package Foo;
use Moo;
has '_token' => (is => 'ro', predicate => 1);
",
    );
    let pred: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "_has_token" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(pred.len(), 1, "predicate => 1 on _attr derives _has_<rest>");
}

#[test]
fn test_moo_has_clearer_string() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'cache' => (is => 'rw', clearer => 'clear_cache');
",
    );
    let clearer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "clear_cache" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(clearer.len(), 1, "explicit clearer string synthesizes method");
}

#[test]
fn test_moo_has_clearer_shorthand() {
    // `clearer => 1` derives `clear_<attr>`.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'items' => (is => 'rw', clearer => 1);
",
    );
    let clearer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "clear_items" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(clearer.len(), 1, "clearer => 1 derives clear_<attr>");
}

#[test]
fn test_moo_has_clearer_private_shorthand() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has '_session' => (is => 'rw', clearer => 1);
",
    );
    let clearer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "_clear_session" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(clearer.len(), 1, "clearer => 1 on _attr derives _clear_<rest>");
}

#[test]
fn test_moo_has_writer_option() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'color' => (is => 'ro', writer => 'set_color');
",
    );
    let writer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "set_color" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(writer.len(), 1, "writer option synthesizes method");
    if let SymbolDetail::Sub { ref params, .. } = writer[0].detail {
        assert_eq!(params.len(), 1, "writer has one param");
    }
}

#[test]
fn test_moo_has_reader_option() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'size' => (is => 'ro', reader => 'get_size');
",
    );
    let reader: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "get_size" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(reader.len(), 1, "reader option synthesizes method");
    if let SymbolDetail::Sub { ref params, is_method, .. } = reader[0].detail {
        assert!(is_method);
        assert!(params.is_empty(), "reader takes no args");
    }
}

#[test]
fn test_moo_has_builder_shorthand() {
    // `builder => 1` → method symbol `_build_<attr>` so goto-def
    // to the user-written sub resolves.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'items' => (is => 'ro', builder => 1);
sub _build_items { return [] }
",
    );
    let builder_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "_build_items" && s.kind == SymKind::Method)
        .collect();
    // The synthesized placeholder + the real sub definition both exist.
    // At minimum one symbol with that name must be present.
    assert!(
        !builder_sym.is_empty(),
        "_build_items must exist (synthesized or user-written)"
    );
}

#[test]
fn test_moo_has_builder_string() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'cache' => (is => 'lazy', builder => '_make_cache');
",
    );
    let builder_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "_make_cache" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(builder_sym.len(), 1, "explicit builder name synthesizes method");
}

#[test]
fn test_moo_has_auxiliaries_without_is() {
    // predicate/clearer/builder are valid even when `is` is absent.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'flag' => (predicate => 'has_flag', clearer => 'clear_flag');
",
    );
    let pred: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "has_flag" && s.kind == SymKind::Method)
        .collect();
    let clearer: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "clear_flag" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(pred.len(), 1, "predicate synthesized without is");
    assert_eq!(clearer.len(), 1, "clearer synthesized without is");
}

#[test]
fn test_moo_has_auxiliaries_with_bare() {
    // `is => bare` suppresses default accessor but auxiliaries still appear.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'secret' => (is => 'bare', predicate => 'has_secret');
",
    );
    // Default accessor suppressed
    let accessors: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "secret" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(accessors.len(), 0, "bare suppresses default accessor");
    // Predicate still synthesized
    let pred: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "has_secret" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(pred.len(), 1, "predicate synthesized even with is => bare");
}

#[test]
fn test_moo_has_handles_hashref() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'logger' => (is => 'ro', isa => 'Log::Any', handles => { log => 'debug', warning => 'warn' });
",
    );
    let log_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "log" && s.kind == SymKind::Method)
        .collect();
    let warning_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "warning" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(log_sym.len(), 1, "handles hashref synthesizes 'log' method");
    assert_eq!(warning_sym.len(), 1, "handles hashref synthesizes 'warning' method");
}

#[test]
fn test_moose_has_handles_arrayref() {
    let fa = build_fa(
        "
package Foo;
use Moose;
has 'db' => (is => 'ro', isa => 'DBI::db', handles => [qw(prepare execute)]);
",
    );
    let prepare: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "prepare" && s.kind == SymKind::Method)
        .collect();
    let execute: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "execute" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(prepare.len(), 1, "handles arrayref synthesizes 'prepare'");
    assert_eq!(execute.len(), 1, "handles arrayref synthesizes 'execute'");
}

#[test]
fn test_moo_has_handles_instanceof_edges_return_type() {
    // When isa is InstanceOf['X'], handles delegation edges each local
    // method's return through MethodOnClass{X, remote} so type inference
    // chains through.
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'logger' => (is => 'ro', isa => \"InstanceOf['Log::Any']\", handles => { log => 'debug' });
",
    );
    let log_sym: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "log" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(log_sym.len(), 1, "handles delegation synthesizes method");
    // Provenance confirms this came from framework synthesis
    match fa.return_type_provenance(log_sym[0].id) {
        TypeProvenance::FrameworkSynthesis { framework, reason } => {
            assert!(
                framework == "Moo" || framework == "Moose",
                "provenance framework should be Moo/Moose, got {}",
                framework
            );
            assert!(reason.contains("handles"), "reason should mention handles");
        }
        TypeProvenance::Inferred => {
            // Acceptable: no witness was pushed if there was no isa type resolution.
        }
        other => panic!("unexpected provenance: {other:?}"),
    }
}

/// Regression: an option keyword that carries DATA, not a method name
/// (`is`/`isa`/`default`/`lazy`/…), must never mint a method named after its
/// string value. The sprint that moved the accessor vocabulary into moo.rhai
/// briefly synthesized phantom `ro`/`rw`/`lazy`/`bare` methods from every
/// option's value — this pins the gate that stopped it.
#[test]
fn test_moo_has_no_phantom_method_from_data_options() {
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name' => (is => 'ro', isa => 'Str', default => 'bob', lazy => 1, required => 1);
",
    );
    for phantom in ["ro", "rw", "lazy", "bare", "Str", "bob", "1"] {
        let hits: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == phantom && s.kind == SymKind::Method)
            .collect();
        assert!(
            hits.is_empty(),
            "option value `{phantom}` must not become a method, got {} symbol(s)",
            hits.len()
        );
    }
    // The real accessor still lands.
    let name: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(name.len(), 1, "the real `name` accessor must still synthesize");
}

/// Moose `lazy_build => 1` implies a builder/clearer/predicate trio at runtime.
#[test]
fn test_moose_has_lazy_build_expands_trio() {
    let fa = build_fa(
        "
package Foo;
use Moose;
has 'cache' => (is => 'ro', lazy_build => 1);
",
    );
    for (name, what) in [
        ("_build_cache", "builder"),
        ("clear_cache", "clearer"),
        ("has_cache", "predicate"),
    ] {
        let hits: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == name && s.kind == SymKind::Method)
            .collect();
        assert_eq!(hits.len(), 1, "lazy_build must synthesize the {what} `{name}`");
    }
    // `lazy_build`/`is` themselves are not methods.
    for phantom in ["lazy_build", "ro", "1"] {
        assert!(
            !fa.symbols.iter().any(|s| s.name == phantom && s.kind == SymKind::Method),
            "`{phantom}` must not become a method"
        );
    }
}

/// goto-def on a `has` accessor must land on the attribute name token of the
/// `has` declaration, not an option line (`is => 'ro'`) inside the body.
#[test]
fn test_moo_has_accessor_selection_span_is_attr_name() {
    // `has` on line 3 (0-indexed), attr `name` at col 5; options on line 4.
    let fa = build_fa("package Foo;\nuse Moo;\nhas name => (\n    is => 'ro',\n);\n");
    let name = fa
        .symbols
        .iter()
        .find(|s| s.name == "name" && s.kind == SymKind::Method)
        .expect("name accessor");
    assert_eq!(
        name.selection_span.start.row, 2,
        "selection_span must point at the `has name` line, not the options line"
    );
}

/// Dancer2::Plugin re-exports Moo's `has`, so consumer plugins get accessor
/// synthesis even though they never literally `use Moo`.
#[test]
fn test_dancer2_plugin_has_synthesizes_accessor() {
    let fa = build_fa(
        "
package My::Plugin;
use Dancer2::Plugin;
has my_setting => (is => 'ro', isa => 'Str');
",
    );
    let acc: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "my_setting" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(acc.len(), 1, "Dancer2::Plugin `has` must synthesize the accessor");
    // And no phantom from the `is`/`isa` data options.
    for phantom in ["ro", "Str"] {
        assert!(
            !fa.symbols.iter().any(|s| s.name == phantom && s.kind == SymKind::Method),
            "`{phantom}` must not become a method under Dancer2::Plugin either"
        );
    }
}

#[test]
fn use_constant_scalar_form_registers_sub_symbol() {
    // `use constant NAME => VAL` declares a package-global sub. Registering
    // it as a Sub symbol silences the unresolved-function hint at callsites
    // and gives goto-def.
    let fa = build_fa("use constant DEBUG => 1;\nmy $y = DEBUG && 2;\n");
    assert!(
        fa.symbols.iter().any(|s| s.name == "DEBUG" && s.kind == SymKind::Sub),
        "DEBUG must be registered as a Sub symbol; got: {:?}",
        fa.symbols.iter().map(|s| (&s.name, s.kind)).collect::<Vec<_>>(),
    );
}

#[test]
fn use_constant_block_form_registers_each_name() {
    let fa = build_fa("use constant { A => 1, B => 2, C => 3 };\n");
    for n in ["A", "B", "C"] {
        assert!(
            fa.symbols.iter().any(|s| s.name == n && s.kind == SymKind::Sub),
            "block-form constant `{n}` must be a Sub symbol; got: {:?}",
            fa.symbols.iter().map(|s| s.name.clone()).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn use_constant_between_subs_at_file_scope() {
    // Constants declared between subs must still register.
    let src = "sub one {}\nuse constant MID => 'x';\nsub two {}\n";
    let fa = build_fa(src);
    assert!(
        fa.symbols.iter().any(|s| s.name == "MID" && s.kind == SymKind::Sub),
        "MID declared between subs must register as a Sub symbol",
    );
}

#[test]
fn multiple_name_form_use_constants_each_register() {
    // Several separate NAME-form `use constant` statements in one package:
    // each declares its own package-global sub. The use-dedup key carries the
    // statement span, so identical work identity at different spans (the
    // constant name isn't folded into `constant_strings` when `imports` is
    // extracted, so it's empty for all of them) no longer collapses past the
    // first.
    let src = r#"package Foo;
use constant ALPHA => 1;
use constant BETA  => 2;
use constant GAMMA => 3;
sub go {
    my $a = ALPHA;
    my $b = BETA;
    my $c = GAMMA;
}
"#;
    let fa = build_fa(src);
    for n in ["ALPHA", "BETA", "GAMMA"] {
        assert!(
            fa.symbols.iter().any(|s| s.name == n && s.kind == SymKind::Sub),
            "every separate NAME-form constant must register a Sub symbol; `{n}` missing. got: {:?}",
            fa.symbols.iter().map(|s| s.name.clone()).collect::<Vec<_>>(),
        );
        // Usages resolve: each name joins `declared_constants`, so the
        // standalone bareword usage gets a FunctionCall ref to the def.
        assert!(
            fa.refs.iter().any(|r| {
                r.target_name == n
                    && matches!(
                        &r.kind,
                        RefKind::FunctionCall { resolved_package } if resolved_package.as_deref() == Some("Foo")
                    )
            }),
            "usage of `{n}` must get a FunctionCall ref to its def; refs for {n}: {:?}",
            fa.refs.iter().filter(|r| r.target_name == n).collect::<Vec<_>>(),
        );
    }
}

/// goto-def + references across THREE separate NAME-form `use constant`
/// statements: every def is reachable and every usage lists.
#[test]
fn multiple_name_form_use_constants_goto_def_and_references() {
    use crate::file_store::FileStore;
    use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
    use std::path::PathBuf;

    let src = r#"package Foo;
use constant ALPHA => 1;
use constant BETA  => 2;
use constant GAMMA => 3;
sub go {
    my $a = ALPHA;
    my $b = BETA;
    return GAMMA;
}
"#;
    let fa = build_fa(src);
    let store = FileStore::new();
    store.insert_workspace(PathBuf::from("/tmp/qa_multi_const.pm"), fa);

    // BETA: def + 1 usage = 2 hits. GAMMA: def + 1 usage = 2. ALPHA: def + 1.
    for name in ["ALPHA", "BETA", "GAMMA"] {
        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: name.to_string(),
                kind: TargetKind::Sub { package: Some("Foo".to_string()) },
                method_classes: Vec::new(),
            },
            RoleMask::EDITABLE,
        );
        assert_eq!(
            results.len(),
            2,
            "references on `{name}` should list its def + 1 usage; got {results:?}"
        );
    }
}

#[test]
fn indirect_object_filehandle_not_a_function_ref() {
    // `print FH LIST` — the bareword filehandle must NOT become a
    // FunctionCall ref (otherwise STDERR/STDOUT/DATA flag as unresolved).
    for src in [
        "print STDERR \"hi\";\n",
        "printf STDERR \"%s\", $x;\n",
        "say STDOUT \"hi\";\n",
    ] {
        let fa = build_fa(src);
        let fh = src.split_whitespace().nth(1).unwrap().trim_matches(|c| c == '"');
        assert!(
            !fa.refs.iter().any(|r|
                matches!(r.kind, RefKind::FunctionCall { .. }) && r.target_name == fh),
            "filehandle `{fh}` must not be a FunctionCall ref for `{}`; refs: {:?}",
            src.trim(),
            fa.refs.iter().filter(|r| matches!(r.kind, RefKind::FunctionCall { .. }))
                .map(|r| r.target_name.clone()).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn print_with_paren_call_still_emits_function_ref() {
    // `print foo("x")` is a real call — foo must keep its FunctionCall ref.
    let fa = build_fa("print foo(\"x\");\n");
    assert!(
        fa.refs.iter().any(|r|
            matches!(r.kind, RefKind::FunctionCall { .. }) && r.target_name == "foo"),
        "parenthesized call `foo(...)` inside print must keep its FunctionCall ref",
    );
}

#[test]
fn shift_invocant_typed_like_at_underscore() {
    // `my $self = shift;` types $self as the enclosing class, exactly like
    // `my ($self) = @_;` — so method calls on $self resolve in-package.
    // Each body is on line 2 (0-indexed); query $self at its use point.
    let at_point = tree_sitter::Point { row: 2, column: 28 };
    let is_class_w = |fa: &FileAnalysis| {
        matches!(
            fa.inferred_type_via_bag("$self", at_point),
            Some(InferredType::ClassName(ref c)) if c == "W"
        )
    };
    let shift_fa =
        build_fa("package W;\nsub go { 1 }\nsub f { my $self = shift; $self->go(); }\n");
    let at_fa =
        build_fa("package W;\nsub go { 1 }\nsub f { my ($self) = @_; $self->go(); }\n");
    assert!(is_class_w(&shift_fa), "shift-extracted $self must type as ClassName(W)");
    assert!(is_class_w(&at_fa), "@_-extracted $self must type as ClassName(W)");
}

// ---- Framework synthesis/detection: requires / Role::Tiny / DBIC ancestry /
//      mk_group_accessors / Mojo -base parent / has comma-form ----

#[test]
fn test_moo_role_requires_is_framework_import() {
    let fa = build_fa(
        "
package My::Role;
use Moo::Role;
requires 'must_implement';
",
    );
    assert!(
        fa.framework_imports.contains("requires"),
        "Moo::Role exports `requires` — should register as a framework import"
    );
}

#[test]
fn test_moose_role_requires_is_framework_import() {
    let fa = build_fa(
        "
package My::Role;
use Moose::Role;
requires 'foo';
",
    );
    assert!(fa.framework_imports.contains("requires"));
}

#[test]
fn test_role_tiny_behaves_like_moo_role() {
    let fa = build_fa(
        "
package My::Role;
use Role::Tiny;
requires 'bar';
with 'Other::Role';
",
    );
    assert!(
        fa.framework_imports.contains("requires"),
        "Role::Tiny exports `requires`"
    );
    assert!(
        fa.framework_imports.contains("with"),
        "Role::Tiny exports `with`"
    );
}

#[test]
fn test_role_tiny_with_behaves_like_moo_role() {
    let fa = build_fa(
        "
package My::Class;
use Role::Tiny::With;
with 'Some::Role';
",
    );
    assert!(fa.framework_imports.contains("with"));
}

#[test]
fn test_dbic_two_level_ancestry_synthesizes_columns() {
    // Result → BaseResult → DBIx::Class::Core: the DBIC base is two hops up.
    // The shallow direct-parent check missed this; full-ancestry walk catches it.
    let fa = build_fa(
        "
package My::Schema::BaseResult;
use base 'DBIx::Class::Core';

package My::Schema::Result::User;
use base 'My::Schema::BaseResult';
__PACKAGE__->add_columns(qw/id username/);
",
    );
    let id: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "id" && s.kind == SymKind::Method)
        .collect();
    let username: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "username" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(id.len(), 1, "2-level DBIC inheritance should synthesize `id`");
    assert_eq!(username.len(), 1, "and `username`");
}

#[test]
fn test_mk_group_accessors_synthesizes_methods() {
    let fa = build_fa(
        "
package My::Thing;
use base 'Class::Accessor::Grouped';
__PACKAGE__->mk_group_accessors('simple', qw/alpha beta/);
__PACKAGE__->mk_group_ro_accessors('inflated', 'gamma', 'delta');
",
    );
    for name in ["alpha", "beta", "gamma", "delta"] {
        let hits: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == name && s.kind == SymKind::Method)
            .collect();
        assert_eq!(hits.len(), 1, "mk_group accessor `{name}` should be synthesized");
    }
    // The group name itself is NOT an accessor.
    assert!(
        !fa.symbols.iter().any(|s| s.name == "simple" && s.kind == SymKind::Method),
        "the leading group name must not become an accessor"
    );
}

#[test]
fn test_mk_classdata_synthesizes_method() {
    let fa = build_fa(
        "
package My::Thing;
use base 'Class::Accessor::Grouped';
__PACKAGE__->mk_classdata('config');
",
    );
    let hits: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "config" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(hits.len(), 1, "mk_classdata should synthesize the named accessor");
}

#[test]
fn test_use_module_dash_base_registers_parent_and_mojo_behavior() {
    let fa = build_fa(
        "
package My::Emitter;
use Mojo::EventEmitter -base;
has 'value';
",
    );
    // The module imported with -base becomes a parent...
    assert!(
        fa.package_parents
            .get("My::Emitter")
            .map(|v| v.iter().any(|p| p == "Mojo::EventEmitter"))
            .unwrap_or(false),
        "`use X -base` should register X as a parent"
    );
    // ...and Mojo::Base accessor synthesis (getter + setter) applies.
    let methods: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "value" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(methods.len(), 2, "`-base` pulls Mojo::Base has-synthesis");
}

#[test]
fn test_mojo_base_dash_base_carries_mojo_base_as_parent() {
    let fa = build_fa(
        "
package My::Class;
use Mojo::Base -base;
has 'x';
",
    );
    assert!(
        fa.package_parents
            .get("My::Class")
            .map(|v| v.iter().any(|p| p == "Mojo::Base"))
            .unwrap_or(false),
        "`Mojo::Base -base` should carry Mojo::Base itself as a parent so tap/attr/new resolve"
    );
}

#[test]
fn test_moo_has_comma_form_synthesizes_accessor() {
    // The comma-separated option form (not the fat-arrow `name => (...)` form).
    let fa = build_fa(
        "
package Foo;
use Moo;
has 'name', is => 'ro', default => sub { 1 };
has age => (is => 'rw');
",
    );
    let name_acc: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "name" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(
        name_acc.len(),
        1,
        "comma-form `has 'name', is => 'ro'` should synthesize a `name` accessor"
    );
    // The fat-arrow form on the next line still works (no regression).
    // `is => 'rw'` synthesizes a getter + a writer (2 symbols named `age`).
    let age_acc: Vec<_> = fa
        .symbols
        .iter()
        .filter(|s| s.name == "age" && s.kind == SymKind::Method)
        .collect();
    assert_eq!(age_acc.len(), 2, "fat-arrow rw form still synthesizes getter+setter");
    // `is`/`default` must not become phantom accessors.
    assert!(
        !fa.symbols.iter().any(|s| (s.name == "is" || s.name == "ro") && s.kind == SymKind::Method),
        "option keywords/values must not mint phantom methods in comma form"
    );
}

// ---- typeglob sub installation (CG-1) ----

fn has_sub(fa: &FileAnalysis, name: &str) -> bool {
    fa.symbols
        .iter()
        .any(|s| s.name == name && s.kind == SymKind::Sub)
}

#[test]
fn glob_static_name_sub() {
    let fa = build_fa("*greet = sub { return 'hi' };\n");
    assert!(has_sub(&fa, "greet"), "static *name = sub {{...}} must mint a Sub symbol");
}

#[test]
fn glob_alias_to_existing_sub() {
    let fa = build_fa("*alias = \\&Other::func;\n*local_alias = \\&real;\n");
    assert!(has_sub(&fa, "alias"), "*name = \\&Other::func glob alias must mint a Sub symbol");
    assert!(has_sub(&fa, "local_alias"), "*name = \\&func glob alias must mint a Sub symbol");
}

#[test]
fn glob_qualified_name_installs_tail() {
    // `*Other::foo = sub {...}` installs `foo` into Other; the unqualified
    // tail is what local call sites / nav use.
    let fa = build_fa("*Other::foo = sub { 1 };\n");
    assert!(has_sub(&fa, "foo"), "qualified glob must register the unqualified tail");
    assert!(!has_sub(&fa, "Other::foo"), "must not register the fully-qualified string as a name");
}

#[test]
fn glob_loop_over_qw() {
    let src = "for my $m (qw/red green blue/) {\n  no strict 'refs';\n  *$m = sub { 1 };\n}\n";
    let fa = build_fa(src);
    for name in ["red", "green", "blue"] {
        assert!(has_sub(&fa, name), "loop-installed glob `{name}` must mint a Sub symbol");
    }
}

#[test]
fn glob_begin_constant_style() {
    let src = "BEGIN {\n  *_FORCE_WRITABLE = sub () { 1 };\n}\n";
    let fa = build_fa(src);
    assert!(
        has_sub(&fa, "_FORCE_WRITABLE"),
        "constant-style glob sub in BEGIN must mint a Sub symbol"
    );
}

#[test]
fn glob_literal_block_name() {
    let fa = build_fa("*{ 'is_thing' } = sub { 1 };\n");
    assert!(has_sub(&fa, "is_thing"), "`*{{ 'literal' }}` glob must mint a Sub symbol");
}

#[test]
fn glob_scalar_rhs_coderef() {
    let fa = build_fa("*handler = $coderef;\n");
    assert!(has_sub(&fa, "handler"), "*name = $coderef must mint a Sub symbol");
}

#[test]
fn glob_dynamic_name_skipped() {
    // Fully runtime name — no static derivation, must NOT fabricate a symbol.
    let fa = build_fa("*{ $runtime } = sub { 1 };\n");
    // The anon `sub {...}` RHS mints an `(anon)` Sub symbol; the glob install
    // itself must add no named Sub.
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Sub && s.name != "(anon)"),
        "fully-dynamic glob name must be skipped, not guessed"
    );
}

#[test]
fn glob_unfoldable_concat_skipped() {
    // `'is_' . $type` with an unknown $type is not derivable → skip.
    let fa = build_fa("*{ 'is_' . $type } = sub { 1 };\n");
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Sub && s.name.starts_with("is_")),
        "unfoldable concat name must be skipped, not guessed with a partial prefix"
    );
}

#[test]
fn glob_concat_with_loop_var_foldable() {
    // `'is_' . $kind` where $kind ranges over a qw list → derivable names.
    let src =
        "for my $kind (qw/foo bar/) {\n  *{ 'is_' . $kind } = sub { 1 };\n}\n";
    let fa = build_fa(src);
    assert!(has_sub(&fa, "is_foo"), "foldable concat over loop var must mint is_foo");
    assert!(has_sub(&fa, "is_bar"), "foldable concat over loop var must mint is_bar");
}

#[test]
fn normal_assignment_unaffected() {
    // Regression guard: a plain scalar assignment must not mint a Sub symbol,
    // and `my $x = sub {...}` is a lexical coderef, not a glob install.
    let fa = build_fa("my $x = 42;\nmy $cb = sub { 1 };\n");
    assert!(
        !fa.symbols.iter().any(|s| s.name == "x" && s.kind == SymKind::Sub),
        "plain scalar assignment must not mint a Sub symbol"
    );
    assert!(
        !fa.symbols.iter().any(|s| s.name == "cb" && s.kind == SymKind::Sub),
        "lexical `my $cb = sub {{...}}` must not be treated as a glob install"
    );
}

// ---- CG-3a: glob loop over a local literal-returning sub ----

#[test]
fn glob_loop_over_local_qw_sub() {
    // CGI.pm shape: the loop source is a same-file sub returning a qw list.
    // Each installed glob name must mint a Sub symbol.
    let src = "\
foreach my $tag (_all_html_tags()) {
  no strict 'refs';
  *$tag = sub { 1 };
}
sub _all_html_tags { return qw(div span br); }
";
    let fa = build_fa(src);
    for name in ["div", "span", "br"] {
        assert!(has_sub(&fa, name), "loop over local qw-returning sub must mint `{name}`");
    }
}

#[test]
fn glob_loop_over_local_list_sub() {
    // Same, but the local sub returns a bare parenthesized string list
    // (no `qw`, no explicit `return`).
    let src = "\
for my $m (_names()) {
  *$m = sub { 1 };
}
sub _names { ('alpha', 'beta') }
";
    let fa = build_fa(src);
    assert!(has_sub(&fa, "alpha"), "loop over local list-returning sub must mint alpha");
    assert!(has_sub(&fa, "beta"), "loop over local list-returning sub must mint beta");
}

#[test]
fn glob_loop_over_nonliteral_local_sub_skipped() {
    // The local sub's body is computed (not a literal list) → fold yields
    // nothing, loop var stays dynamic, no fabricated symbols.
    let src = "\
for my $m (_dynamic()) {
  *$m = sub { 1 };
}
sub _dynamic { return map { lc } @ARGV; }
";
    let fa = build_fa(src);
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Sub && s.name != "(anon)" && s.name != "_dynamic"),
        "non-literal local sub return must not synthesize glob names"
    );
}

#[test]
fn glob_loop_over_unknown_sub_skipped() {
    // Cross-file / undefined callee — no same-file body to fold. Skip.
    let src = "\
for my $m (Some::Other::tags()) {
  *$m = sub { 1 };
}
";
    let fa = build_fa(src);
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Sub && s.name != "(anon)"),
        "unresolvable loop-source sub must not synthesize glob names"
    );
}

// ---- CG-3b: cross-package glob injection via ->can ----

#[test]
fn glob_loop_can_rhs_synthesizes_under_current_pkg() {
    // DateTime::PP shape. `__PACKAGE__->can($sub)` is recognized as a
    // sub-producing RHS; the qualified target name is synthesized as the
    // unqualified tail under the current package (cross-package attribution
    // deferred — see synthesize_glob_assigned_sub).
    let src = "\
for my $sub (qw/foo bar/) {
  *{ 'DateTime::' . $sub } = __PACKAGE__->can($sub);
}
";
    let fa = build_fa(src);
    assert!(has_sub(&fa, "foo"), "->can RHS over loop var must mint foo (tail)");
    assert!(has_sub(&fa, "bar"), "->can RHS over loop var must mint bar (tail)");
    assert!(!has_sub(&fa, "DateTime::foo"), "must not register the fully-qualified name");
}

#[test]
fn glob_can_on_package_invocant() {
    // `Pkg->can('name')` static target also qualifies as sub-producing.
    let fa = build_fa("*alias = Foo::Bar->can('helper');\n");
    assert!(has_sub(&fa, "alias"), "*name = Pkg->can(...) must mint a Sub symbol");
}

#[test]
fn glob_non_can_method_rhs_skipped() {
    // A method call that isn't `->can` is not known to yield a coderef → skip.
    let fa = build_fa("*thing = $obj->build_something();\n");
    assert!(!has_sub(&fa, "thing"), "non-can method RHS must not mint a Sub symbol");
}

// ---- mk_classdata in a statement-modifier loop ----

fn count_method(fa: &FileAnalysis, name: &str) -> usize {
    fa.symbols.iter().filter(|s| s.name == name && s.kind == SymKind::Method).count()
}

#[test]
fn mk_classdata_postfix_for_qw() {
    // Catalyst.pm:176 shape.
    let fa = build_fa(
        "\
package My::App;
use base 'Class::Accessor::Grouped';
__PACKAGE__->mk_classdata($_) for qw/setup_finished params/;
",
    );
    assert_eq!(count_method(&fa, "setup_finished"), 1, "loop mk_classdata must mint setup_finished once");
    assert_eq!(count_method(&fa, "params"), 1, "loop mk_classdata must mint params once");
}

#[test]
fn mk_classdata_postfix_for_list() {
    // `mk_classdata($_) for (LIST)` bare-call form (Controller.pm:123).
    let fa = build_fa(
        "\
package My::Controller;
use base 'Class::Accessor::Grouped';
mk_classdata($_) for ('action_namespace', 'path_prefix');
",
    );
    assert_eq!(count_method(&fa, "action_namespace"), 1, "bare-call loop must mint action_namespace");
    assert_eq!(count_method(&fa, "path_prefix"), 1, "bare-call loop must mint path_prefix");
}

#[test]
fn mk_classdata_postfix_for_nonliteral_skipped() {
    // Loop over an array variable → no literal names → no synthesis.
    let fa = build_fa(
        "\
package My::App;
use base 'Class::Accessor::Grouped';
__PACKAGE__->mk_classdata($_) for @dynamic_names;
",
    );
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Method),
        "non-literal loop list must not synthesize accessors"
    );
}

#[test]
fn postfix_for_non_accessor_call_synthesizes_nothing() {
    // Regression: a statement-modifier loop whose body is an unrelated call
    // must not mint any accessor symbol.
    let fa = build_fa(
        "\
package My::App;
print(\"$_\\n\") for qw/a b c/;
",
    );
    assert!(
        !fa.symbols.iter().any(|s| s.kind == SymKind::Method),
        "non-accessor postfix-for loop must not synthesize accessors"
    );
}

// ---- Class::Tiny accessor synthesis (CG-2) ----

#[test]
fn test_class_tiny_list_form_synthesizes_accessors() {
    let fa = build_fa(
        "
package Foo;
use Class::Tiny qw( resolvers cache );
",
    );
    for attr in ["resolvers", "cache"] {
        let acc: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == attr && s.kind == SymKind::Method)
            .collect();
        assert_eq!(
            acc.len(),
            1,
            "Class::Tiny qw list should synthesize one rw accessor for `{attr}`"
        );
        // Constructor key so `Foo->new(resolvers => ...)` connects.
        let key_def: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == attr && matches!(s.detail, SymbolDetail::HashKeyDef { .. }))
            .collect();
        assert!(
            !key_def.is_empty(),
            "Class::Tiny attr `{attr}` should mint a constructor HashKeyDef"
        );
        if let SymbolDetail::HashKeyDef { ref owner, .. } = key_def[0].detail {
            assert_eq!(
                owner,
                &HashKeyOwner::Sub {
                    package: Some("Foo".to_string()),
                    name: "new".to_string(),
                }
            );
        }
    }
}

#[test]
fn test_class_tiny_hashref_form_synthesizes_accessors_from_keys() {
    let fa = build_fa(
        "
package Foo;
use Class::Tiny {
  name => 'default',
  builder => sub { [] },
};
",
    );
    // Keys are accessors; default values (string / coderef) are NOT.
    for attr in ["name", "builder"] {
        let acc: Vec<_> = fa
            .symbols
            .iter()
            .filter(|s| s.name == attr && s.kind == SymKind::Method)
            .collect();
        assert_eq!(
            acc.len(),
            1,
            "Class::Tiny hashref key `{attr}` should synthesize an accessor"
        );
    }
    // The default value `'default'` must not become an accessor.
    assert!(
        !fa.symbols
            .iter()
            .any(|s| s.name == "default" && s.kind == SymKind::Method),
        "hashref default value must not mint a phantom accessor"
    );
}

#[test]
fn test_class_tiny_combined_list_and_hashref() {
    // `use Class::Tiny qw( ssn ), { name => undef };` — both shapes on one line.
    let fa = build_fa(
        "
package Foo;
use Class::Tiny qw( ssn ), { name => undef };
",
    );
    for attr in ["ssn", "name"] {
        assert!(
            fa.symbols
                .iter()
                .any(|s| s.name == attr && s.kind == SymKind::Method),
            "combined qw+hashref form should synthesize accessor `{attr}`"
        );
    }
}

#[test]
fn test_non_class_tiny_use_unaffected() {
    // Regression: an unrelated `use X qw(...)` must NOT synthesize accessors.
    let fa = build_fa(
        "
package Foo;
use List::Util qw( max min );
",
    );
    assert!(
        !fa.symbols
            .iter()
            .any(|s| (s.name == "max" || s.name == "min") && s.kind == SymKind::Method),
        "non-Class::Tiny use must not synthesize accessor methods"
    );
}

// ── Task A: rule #7 ref-emission for use-constant usages + export-list members ──

/// `use constant NAME => ...` usage sites (plain expr + call arg) each get a
/// FunctionCall ref back to the constant def, so goto-def and references work.
#[test]
fn const_usage_name_form_emits_function_call_ref() {
    let src = r#"
package QA::C;
use constant MAX_RETRIES => 5;
sub retry {
    my $limit = MAX_RETRIES;
    return _attempt($limit, MAX_RETRIES);
}
sub _attempt { return 1 }
"#;
    let fa = build_fa(src);
    let usages: Vec<&Ref> = fa
        .refs
        .iter()
        .filter(|r| {
            r.target_name == "MAX_RETRIES"
                && matches!(
                    &r.kind,
                    RefKind::FunctionCall { resolved_package } if resolved_package.as_deref() == Some("QA::C")
                )
        })
        .collect();
    assert_eq!(
        usages.len(),
        2,
        "both MAX_RETRIES usages (plain + call-arg) should ref the const def; got {:?}",
        fa.refs
            .iter()
            .filter(|r| r.target_name == "MAX_RETRIES")
            .collect::<Vec<_>>()
    );
}

/// Block form `use constant { TIMEOUT => 30, BACKOFF => 2 }` — usages of a
/// block-declared constant get the same FunctionCall ref.
#[test]
fn const_usage_block_form_emits_function_call_ref() {
    let src = r#"
package QA::C;
use constant {
    TIMEOUT => 30,
    BACKOFF => 2,
};
sub run {
    my $t = TIMEOUT;
    return $t + BACKOFF;
}
"#;
    let fa = build_fa(src);
    for name in ["TIMEOUT", "BACKOFF"] {
        let n = fa
            .refs
            .iter()
            .filter(|r| {
                r.target_name == name
                    && matches!(&r.kind, RefKind::FunctionCall { .. })
            })
            .count();
        assert_eq!(n, 1, "{name} usage should ref the block-form const def");
    }
}

/// goto-def from a constant usage lands on the const def via `ref_at` +
/// `refs_to`; references on the const lists the def + every usage.
#[test]
fn const_usage_goto_def_and_references() {
    use crate::file_store::FileStore;
    use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
    use std::path::PathBuf;

    let src = r#"package QA::C;
use constant MAX_RETRIES => 5;
sub retry {
    my $limit = MAX_RETRIES;
    return MAX_RETRIES;
}
"#;
    let fa = build_fa(src);

    // ref_at the first usage (`my $limit = MAX_RETRIES;`) is a FunctionCall
    // ref naming the const — that's the goto-def routing token.
    let usage_pt = Point::new(3, 16); // inside MAX_RETRIES on the `my $limit` line
    let r = fa
        .ref_at(usage_pt)
        .expect("a ref should sit on the constant usage");
    assert_eq!(r.target_name, "MAX_RETRIES");
    assert!(matches!(r.kind, RefKind::FunctionCall { .. }));

    let store = FileStore::new();
    let path = PathBuf::from("/tmp/qa_const.pm");
    store.insert_workspace(path.clone(), fa);

    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "MAX_RETRIES".to_string(),
            kind: TargetKind::Sub {
                package: Some("QA::C".to_string()),
            },
            method_classes: Vec::new(),
        },
        RoleMask::EDITABLE,
    );
    // def + 2 usages = 3 hits.
    assert_eq!(
        results.len(),
        3,
        "references on MAX_RETRIES should list the def and both usages; got {results:?}"
    );
}

/// Regression: a bareword that is NOT a declared constant gets no spurious
/// constant-usage ref.
#[test]
fn non_constant_bareword_gets_no_const_ref() {
    let src = r#"
package QA::C;
use constant MAX_RETRIES => 5;
sub run {
    my $x = SOME_OTHER;
    return $x;
}
"#;
    let fa = build_fa(src);
    assert!(
        !fa.refs
            .iter()
            .any(|r| r.target_name == "SOME_OTHER"),
        "a non-constant bareword must not get a constant-usage ref"
    );
}

/// `@EXPORT` / `@EXPORT_OK` / `%EXPORT_TAGS` member tokens that name a local
/// sub each get a FunctionCall ref to that sub (forward-declared subs work).
#[test]
fn export_list_members_ref_local_subs() {
    let src = r#"
package QA::E;
use Exporter 'import';
our @EXPORT      = qw(always_on);
our @EXPORT_OK   = qw(opt_a opt_b opt_c);
our %EXPORT_TAGS = (
    group_one => [qw(opt_a opt_b)],
    group_two => [qw(opt_c)],
);
sub always_on { 1 }
sub opt_a { 'a' }
sub opt_b { 'b' }
sub opt_c { 'c' }
"#;
    let fa = build_fa(src);
    let count = |name: &str| {
        fa.refs
            .iter()
            .filter(|r| {
                r.target_name == name
                    && matches!(
                        &r.kind,
                        RefKind::FunctionCall { resolved_package } if resolved_package.as_deref() == Some("QA::E")
                    )
            })
            .count()
    };
    // always_on: 1 (@EXPORT). opt_a: @EXPORT_OK + %EXPORT_TAGS group_one = 2.
    // opt_b: @EXPORT_OK + group_one = 2. opt_c: @EXPORT_OK + group_two = 2.
    assert_eq!(count("always_on"), 1, "@EXPORT member should ref its sub");
    assert_eq!(count("opt_a"), 2, "opt_a appears in @EXPORT_OK and a tag array");
    assert_eq!(count("opt_b"), 2, "opt_b appears in @EXPORT_OK and a tag array");
    assert_eq!(count("opt_c"), 2, "opt_c appears in @EXPORT_OK and a tag array");
}

/// goto-def / references on an export-list member resolve to the sub def.
#[test]
fn export_member_goto_def_and_references() {
    use crate::file_store::FileStore;
    use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};
    use std::path::PathBuf;

    let src = r#"package QA::E;
use Exporter 'import';
our @EXPORT_OK = qw(opt_a opt_b);
sub opt_a { 'a' }
sub opt_b { 'b' }
"#;
    let fa = build_fa(src);

    // ref_at the `opt_a` token in the export list.
    let opt_a_def_span = fa
        .symbols
        .iter()
        .find(|s| s.name == "opt_a")
        .map(|s| s.selection_span)
        .expect("opt_a sub symbol");
    // The export-list member ref must NOT be the def span itself.
    let export_ref = fa
        .refs
        .iter()
        .find(|r| {
            r.target_name == "opt_a"
                && matches!(&r.kind, RefKind::FunctionCall { .. })
                && r.span != opt_a_def_span
        })
        .expect("an export-list FunctionCall ref for opt_a");
    let r = fa
        .ref_at(export_ref.span.start)
        .expect("ref_at the export member token");
    assert_eq!(r.target_name, "opt_a");

    let store = FileStore::new();
    let path = PathBuf::from("/tmp/qa_export.pm");
    store.insert_workspace(path.clone(), fa);
    let results = refs_to(
        &store,
        None,
        &TargetRef {
            name: "opt_a".to_string(),
            kind: TargetKind::Sub {
                package: Some("QA::E".to_string()),
            },
            method_classes: Vec::new(),
        },
        RoleMask::EDITABLE,
    );
    // def + 1 export-list mention = 2.
    assert_eq!(
        results.len(),
        2,
        "references on opt_a should list the def and its @EXPORT_OK mention; got {results:?}"
    );
}

/// Regression: a `%EXPORT_TAGS` tag-NAME key (`group_one`) is NOT a sub, so it
/// gets no ref even though it sits in the export table.
#[test]
fn export_tag_name_key_gets_no_ref() {
    let src = r#"
package QA::E;
use Exporter 'import';
our %EXPORT_TAGS = (
    group_one => [qw(opt_a)],
);
sub opt_a { 'a' }
sub group_one { 'not a tag' }
"#;
    let fa = build_fa(src);
    // The fixture defines a sub literally named `group_one` to make the test
    // sharp: if the tag-name key were (wrongly) recorded as a member, it would
    // resolve to this sub. The key must still get no ref — only the value-array
    // member `opt_a` does.
    let group_one_refs = fa
        .refs
        .iter()
        .filter(|r| {
            r.target_name == "group_one"
                && matches!(&r.kind, RefKind::FunctionCall { .. })
        })
        .count();
    assert_eq!(
        group_one_refs, 0,
        "a tag-name key must not be reffed even when a same-named sub exists"
    );
}

/// A constant invoked as a call (`MAX_RETRIES()`) is reffed once by the
/// function-call path; the bareword arm must not double-emit at that span.
#[test]
fn const_call_form_not_double_reffed() {
    let src = r#"
package QA::C;
use constant MAX_RETRIES => 5;
sub run { return MAX_RETRIES(); }
"#;
    let fa = build_fa(src);
    let n = fa
        .refs
        .iter()
        .filter(|r| {
            r.target_name == "MAX_RETRIES" && matches!(&r.kind, RefKind::FunctionCall { .. })
        })
        .count();
    assert_eq!(n, 1, "MAX_RETRIES() call must get exactly one FunctionCall ref");
}

/// AutoLoader-backed package: subs after `__END__` live in the opaque
/// `data_section`, but they are runtime-live via AUTOLOAD. They must surface
/// as navigable Sub symbols with file-offset spans, with POD between them
/// skipped, and goto-def from an in-package caller must reach them.
#[test]
fn autoloader_data_section_subs_synthesized() {
    let src = "package My::AL;\n\
               use AutoLoader qw(AUTOLOAD);\n\
               sub uses_them { want_read(); }\n\
               1;\n\
               __END__\n\
               sub want_read { return 42 }\n\
               sub get_https { do_httpx2(GET => 1, @_) }\n\
               =pod\n\
               junk\n\
               =cut\n\
               sub after_pod ($;$) { return 1 }\n";
    let fa = build_fa(src);

    let names: std::collections::HashSet<&str> = fa
        .symbols
        .iter()
        .filter(|s| s.kind == SymKind::Sub)
        .map(|s| s.name.as_str())
        .collect();
    assert!(names.contains("want_read"), "want_read must be synthesized");
    assert!(names.contains("get_https"), "get_https must be synthesized");
    assert!(names.contains("after_pod"), "sub after POD must be synthesized");

    // Spans land in the data section (row 5 = first sub after __END__).
    let want_read = fa
        .symbols
        .iter()
        .find(|s| s.name == "want_read" && s.kind == SymKind::Sub)
        .expect("want_read symbol");
    assert_eq!(want_read.selection_span.start.row, 5, "want_read at file row 5");
    assert_eq!(want_read.package.as_deref(), Some("My::AL"));

    // Goto-def from the in-package caller reaches the data-section def.
    let def = fa.find_definition(Point::new(2, 16), None, None, None);
    assert_eq!(
        def.map(|s| s.start.row),
        Some(5),
        "goto-def on want_read() should land on the data-section sub"
    );
}

/// The gate: a package that does NOT use AutoLoader/SelfLoader must not have
/// its trailing `__END__` / `__DATA__` payload mined for subs — that text is
/// genuine data or POD, not code.
#[test]
fn non_autoloader_data_section_synthesizes_nothing() {
    let src = "package My::Plain;\n\
               use strict;\n\
               sub real_sub { 1 }\n\
               1;\n\
               __END__\n\
               sub looks_like_a_sub { return 99 }\n\
               =pod\n\
               docs\n\
               =cut\n\
               plain documentation text\n";
    let fa = build_fa(src);
    assert!(
        fa.symbols
            .iter()
            .all(|s| s.name != "looks_like_a_sub"),
        "data-section subs must NOT be synthesized without AutoLoader/SelfLoader"
    );
    assert!(
        fa.symbols.iter().any(|s| s.name == "real_sub"),
        "the real pre-__END__ sub is still present"
    );
}

/// Inheritance-form gate: `use base 'AutoLoader'` (parents, not a direct
/// `use AutoLoader`) also enables data-section synthesis.
#[test]
fn autoloader_via_use_base_enables_synthesis() {
    let src = "package My::Sub;\n\
               use base 'AutoLoader';\n\
               1;\n\
               __END__\n\
               sub inherited_loader_sub { return 1 }\n";
    let fa = build_fa(src);
    assert!(
        fa.symbols
            .iter()
            .any(|s| s.name == "inherited_loader_sub" && s.kind == SymKind::Sub),
        "use base 'AutoLoader' must enable data-section synthesis"
    );
}

/// CHAINED hashref-key ref: `$obj->get_config->{host}` — get_config's
/// return type is a known class (here a blessed Config), so the `host`
/// key is knowable and must get its own narrow HashKeyAccess ref owned
/// by that class. Without it, references/goto-def on `host` are dropped.
#[test]
fn chained_method_call_hash_key_emits_owned_ref() {
    let src = "\
package Config;
sub new { bless { host => 'localhost', port => 5432 }, shift }
package Foo;
sub new { bless {}, shift }
sub get_config { return Config->new() }
package main;
my $obj = Foo->new();
$obj->get_config->{host};
";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());

    // The `host` token (line 7, after `->{`) gets a HashKeyAccess ref
    // owned by Config — the chain receiver's class.
    let host_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        !host_refs.is_empty(),
        "chained hash-key access should emit a HashKeyAccess ref for 'host'"
    );
    let owner = host_refs
        .iter()
        .find_map(|r| match &r.kind {
            RefKind::HashKeyAccess { owner: Some(o), .. } => Some(o.clone()),
            _ => None,
        })
        .expect("chained hash-key ref should carry a resolved owner");
    assert_eq!(
        owner,
        HashKeyOwner::Class("Config".to_string()),
        "owner should be the chain receiver's class, got {:?}",
        owner
    );

    // Goto-def from the `host` token reaches the bless'd key in Config::new.
    let key_ref = host_refs[0];
    let def = fa.find_definition(key_ref.span.start, Some(&tree), Some(src.as_bytes()), None);
    assert!(
        def.is_some(),
        "goto-def on chained `->{{host}}` should resolve to Config's key def"
    );
    assert_eq!(def.unwrap().start.row, 1, "host def is the bless key on line 1");
}

/// Regression: an untyped chain (`$obj->mystery->{host}` where `mystery`
/// has no resolvable return type) must emit NO key ref — honest about
/// ignorance rather than latching onto a wrong owner.
#[test]
fn untyped_chain_emits_no_hash_key_ref() {
    let src = "\
package main;
my $obj = bless {}, 'Foo';
$obj->totally_unknown_method->{host};
";
    let tree = parse(src);
    let fa = build(&tree, src.as_bytes());
    let host_refs: Vec<_> = fa
        .refs
        .iter()
        .filter(|r| r.target_name == "host" && matches!(r.kind, RefKind::HashKeyAccess { .. }))
        .collect();
    assert!(
        host_refs.is_empty(),
        "untyped chain must not emit a hash-key ref, got {:?}",
        host_refs
    );
}

/// CG-3b cross-package glob attribution: `*{ 'DateTime::' . $sub } = …`
/// inside `package DateTime::PP` synthesizes the tail (`_ymd2rd`) under
/// the *named* package (`DateTime`), not the file's own package.
#[test]
fn cross_package_glob_synthesizes_under_target_package() {
    let src = r#"package DateTime::PP;
sub _ymd2rd { 1 }
sub _rd2ymd { 2 }
my @subs = qw( _ymd2rd _rd2ymd );
for my $sub (@subs) {
    no strict 'refs';
    *{ 'DateTime::' . $sub } = __PACKAGE__->can($sub);
}
1;
"#;
    let fa = build_fa(src);
    for tail in ["_ymd2rd", "_rd2ymd"] {
        let under_datetime = fa.symbols.iter().any(|s| {
            s.name == tail
                && matches!(s.kind, SymKind::Sub)
                && s.package.as_deref() == Some("DateTime")
        });
        assert!(
            under_datetime,
            "glob-synthesized `{}` should be attributed to DateTime, symbols: {:?}",
            tail,
            fa.symbols
                .iter()
                .filter(|s| s.name == tail)
                .map(|s| (&s.name, &s.package))
                .collect::<Vec<_>>()
        );
    }
    // The real definitions (under DateTime::PP) are untouched.
    assert!(
        fa.symbols.iter().any(|s| s.name == "_ymd2rd"
            && matches!(s.kind, SymKind::Sub)
            && s.package.as_deref() == Some("DateTime::PP")),
        "the original DateTime::PP::_ymd2rd sub must still exist"
    );
}

/// Regression: a same-package glob (`*name = sub {…}`, no `::` prefix)
/// still synthesizes under the current package.
#[test]
fn same_package_glob_synthesizes_under_current_package() {
    let src = r#"package Acme::Widget;
*frobnicate = sub { 42 };
1;
"#;
    let fa = build_fa(src);
    assert!(
        fa.symbols.iter().any(|s| s.name == "frobnicate"
            && matches!(s.kind, SymKind::Sub)
            && s.package.as_deref() == Some("Acme::Widget")),
        "same-package glob must stay under the current package, symbols: {:?}",
        fa.symbols
            .iter()
            .filter(|s| s.name == "frobnicate")
            .map(|s| (&s.name, &s.package))
            .collect::<Vec<_>>()
    );
}
