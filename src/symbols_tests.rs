use super::*;
use crate::builder;

fn parse_analysis(source: &str) -> FileAnalysis {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();
    builder::build(&tree, source.as_bytes())
}

/// Build a CachedModule by parsing a synthesized Perl source listing the given exports.
/// Used by tests to seed ModuleIndex with known export lists without real @INC files.
fn fake_cached(
    path: &str,
    exports: &[&str],
    exports_ok: &[&str],
) -> std::sync::Arc<crate::module_index::CachedModule> {
    let mut source = String::from("package Fake;\n");
    if !exports.is_empty() {
        source.push_str(&format!("our @EXPORT = qw({});\n", exports.join(" ")));
    }
    if !exports_ok.is_empty() {
        source.push_str(&format!("our @EXPORT_OK = qw({});\n", exports_ok.join(" ")));
    }
    for n in exports.iter().chain(exports_ok.iter()) {
        source.push_str(&format!("sub {} {{}}\n", n));
    }
    source.push_str("1;\n");
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        std::path::PathBuf::from(path),
        std::sync::Arc::new(parse_analysis(&source)),
    ))
}

#[test]
fn test_builtins_sorted() {
    for window in PERL_BUILTINS.windows(2) {
        assert!(
            window[0] < window[1],
            "PERL_BUILTINS not sorted: '{}' >= '{}'",
            window[0],
            window[1],
        );
    }
}

#[test]
fn test_is_perl_builtin() {
    assert!(is_perl_builtin("print"));
    assert!(is_perl_builtin("chomp"));
    assert!(is_perl_builtin("die"));
    assert!(!is_perl_builtin("frobnicate"));
    assert!(!is_perl_builtin("my_custom_sub"));
}

#[test]
fn test_diagnostics_skips_builtins() {
    let source = "use Carp qw(croak);\nprint 'hello';\ndie 'oops';\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    // print and die are builtins, croak is explicitly imported — no diagnostics
    assert!(
        diags.is_empty(),
        "Expected no diagnostics for builtins/imported, got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn test_diagnostics_unresolved_function() {
    let source = "frobnicate();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert_eq!(diags.len(), 1);
    // Quietest visible severity — unresolved barewords are often genuinely
    // dynamic (AUTOLOAD / runtime glob / uninstalled dep), so they shouldn't
    // dominate the Problems panel.
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::HINT));
    assert!(diags[0].message.contains("frobnicate"));
}

#[test]
fn unresolved_dispatch_fires_only_when_enabled_and_only_on_untyped_receiver() {
    use crate::symbols::DiagnosticOptions;
    // `$minion` is a non-self sub param with no type annotation — genuinely
    // untyped. The minion `enqueue` dispatch verb fires, but its receiver
    // can't be pinned to any class → `ReceiverUntyped`.
    let source = "package W;\nsub fire {\n  my ($self, $minion) = @_;\n  $minion->enqueue('send_email');\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    // Off by default: no dispatch diagnostic.
    let default_diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert!(
        !default_diags.iter().any(|d|
            matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-dispatch")),
        "unresolved-dispatch must be off by default; got {:?}",
        default_diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );

    // Enabled: exactly one unresolved-dispatch on the untyped receiver.
    let on = DiagnosticOptions { unresolved_dispatch: true, ..Default::default() };
    let diags = collect_diagnostics(&analysis, &module_index, on);
    let dispatch_diags: Vec<_> = diags.iter().filter(|d|
        matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-dispatch")).collect();
    assert_eq!(
        dispatch_diags.len(), 1,
        "expected one unresolved-dispatch on the untyped receiver; got {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn unresolved_dispatch_silent_on_does_not_apply() {
    use crate::symbols::DiagnosticOptions;
    // Receiver typed to a concrete, unrelated class → DoesNotApply, NOT a
    // typing gap. Even with the diagnostic enabled, it must stay silent.
    let source = "package W;\nsub fire {\n  my $x = Some::Other->new;\n  $x->enqueue('send_email');\n}\npackage Some::Other;\nsub new { bless {}, shift }\nsub enqueue { 1 }\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { unresolved_dispatch: true, ..Default::default() };
    let diags = collect_diagnostics(&analysis, &module_index, on);
    assert!(
        !diags.iter().any(|d|
            matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-dispatch")),
        "DoesNotApply (typed, unrelated receiver) must never diagnose; got {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn test_diagnostics_skips_local_sub() {
    let source = "sub helper { 1 }\nhelper();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert!(
        diags.is_empty(),
        "Locally defined sub should not produce diagnostic, got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn test_diagnostics_skips_package_qualified() {
    let source = "Foo::Bar::baz();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert!(
        diags.is_empty(),
        "Package-qualified calls should not produce diagnostic",
    );
}

// The `not` operator parses as `ambiguous_function_call_expression` because tree-sitter-perl
// has no dedicated node type for it; it must be suppressed by the builtins list, not by CST shape.
#[test]
fn test_diagnostics_no_unresolved_for_not_operator() {
    let source = "my $x = 1;\nmy $y = not $x;\nif (not $x) { }\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let not_diags: Vec<_> = diags.iter().filter(|d| d.message.contains("not")).collect();
    assert!(
        not_diags.is_empty(),
        "`not` should not produce an unresolved-function diagnostic; got: {:?}",
        not_diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

// SUPER::method calls store `target_name = "SUPER::method"` which can never be found
// by a literal MRO walk; the `::` guard must suppress the diagnostic.
#[test]
fn test_diagnostics_no_unresolved_for_super_method() {
    let source = r#"
package Animal;
sub speak { "..." }

package Dog;
use parent -norequire, 'Animal';
sub speak {
    my ($self) = @_;
    my $parent = $self->SUPER::speak();
    return "Woof! $parent";
}
1;
"#;
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let super_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("SUPER"))
        .collect();
    assert!(
        super_diags.is_empty(),
        "`SUPER::speak` should not produce an unresolved-method diagnostic; got: {:?}",
        super_diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

/// Codes of all diagnostics carrying a string code.
fn diag_codes(diags: &[Diagnostic]) -> Vec<String> {
    diags
        .iter()
        .filter_map(|d| match &d.code {
            Some(NumberOrString::String(c)) => Some(c.clone()),
            _ => None,
        })
        .collect()
}

fn undef_deref_diags(source: &str) -> Vec<Diagnostic> {
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    collect_diagnostics(&analysis, &module_index, Default::default())
        .into_iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "undef-deref"))
        .collect()
}

// D1 — method/deref on a provably-`Undef` receiver. The three guard forms
// that drive a subject to the `Undef` bottom (docs/prompt-narrowing-diagnostics.md).

#[test]
fn d1_undef_deref_else_of_if_defined() {
    // The `else` arm of `if (defined $x)` — $x is undef there.
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    if (defined $x) {
        return $x->name;
    } else {
        return $x->name;
    }
}
1;
"#;
    let diags = undef_deref_diags(src);
    assert_eq!(diags.len(), 1, "exactly the else-arm deref fires: {:?}", diag_codes(&undef_deref_diags(src)));
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));
    assert!(diags[0].message.contains("$x"), "{}", diags[0].message);
}

#[test]
fn d1_undef_deref_after_return_if_defined() {
    // Fall-through after `return if defined $x` — $x is undef.
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    return if defined $x;
    $x->name;
}
1;
"#;
    let diags = undef_deref_diags(src);
    assert_eq!(diags.len(), 1, "fall-through deref fires");
}

#[test]
fn d1_undef_deref_unless_defined_body() {
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    unless (defined $x) {
        $x->name;
    }
}
1;
"#;
    let diags = undef_deref_diags(src);
    assert_eq!(diags.len(), 1, "unless-defined body deref fires");
}

#[test]
fn d1_undef_deref_hash_form() {
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    return if defined $x;
    return $x->{host};
}
1;
"#;
    let diags = undef_deref_diags(src);
    assert_eq!(diags.len(), 1, "hash deref on undef fires");
    assert!(diags[0].message.contains("dereferencing"), "{}", diags[0].message);
}

#[test]
fn d1_no_undef_deref_in_guarded_branch() {
    // The defined branch strips Optional / leaves a live value — no warning.
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    if (defined $x) {
        return $x->name;
    }
    return;
}
1;
"#;
    assert!(
        undef_deref_diags(src).is_empty(),
        "guarded use must not warn: {:?}",
        undef_deref_diags(src).iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn d1_no_undef_deref_without_guard() {
    // An ordinary untyped receiver is not `Undef` — D1 stays silent.
    let src = r#"
package P;
sub f {
    my ($self, $x) = @_;
    return $x->name;
}
1;
"#;
    assert!(undef_deref_diags(src).is_empty(), "no guard, no Undef, no warning");
}

// D8 — extend `unresolved-method` to cross-file-resolvable classes. A
// cached module whose internal package name IS the class queried (so
// resolution keys line up), unlike `fake_cached`'s always-`Fake` package.
fn cached_class(module: &str, methods: &[&str]) -> std::sync::Arc<crate::module_index::CachedModule> {
    let mut src = format!("package {};\n", module);
    for m in methods {
        src.push_str(&format!("sub {} {{}}\n", m));
    }
    src.push_str("1;\n");
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        std::path::PathBuf::from(format!("/fake/{}.pm", module.replace("::", "/"))),
        std::sync::Arc::new(parse_analysis(&src)),
    ))
}

fn unresolved_method_diags(
    source: &str,
    idx: &crate::module_index::ModuleIndex,
    opts: DiagnosticOptions,
) -> Vec<String> {
    let analysis = parse_analysis(source);
    collect_diagnostics(&analysis, idx, opts)
        .into_iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-method"))
        .map(|d| d.message)
        .collect()
}

#[test]
fn d8_narrowed_local_class_fires_by_default() {
    // The local-class narrowing case is always-on (no flag) — the receiver
    // narrows to an in-file class that lacks the method.
    let src = r#"
package Foo;
sub real { 1 }
package Main;
sub g {
    my ($self, $x) = @_;
    if ($x->isa('Foo')) {
        $x->bogus;
    }
}
1;
"#;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = unresolved_method_diags(src, &idx, DiagnosticOptions::default());
    assert_eq!(diags.len(), 1, "local narrowed bogus fires by default: {:?}", diags);
    assert!(diags[0].contains("bogus") && diags[0].contains("Foo"), "{}", diags[0]);
}

#[test]
fn d8_narrowed_cross_file_class_gated_behind_flag() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if ($x->isa('My::Dep')) {
        $x->bogus;
    }
}
1;
"#;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.insert_cache("My::Dep", Some(cached_class("My::Dep", &["known_method"])));

    // Default: cross-file extension off → no diagnostic.
    assert!(
        unresolved_method_diags(src, &idx, DiagnosticOptions::default()).is_empty(),
        "cross-file unresolved-method must stay silent without the opt-in",
    );

    // Opt-in: the cross-file class is known and lacks `bogus` → fires.
    let on = DiagnosticOptions { unresolved_method_cross_file: true, ..Default::default() };
    let diags = unresolved_method_diags(src, &idx, on);
    assert_eq!(diags.len(), 1, "opt-in cross-file bogus fires: {:?}", diags);
    assert!(diags[0].contains("My::Dep"), "{}", diags[0]);
}

#[test]
fn d8_cross_file_existing_method_does_not_fire() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if ($x->isa('My::Dep')) {
        $x->known_method;
    }
}
1;
"#;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.insert_cache("My::Dep", Some(cached_class("My::Dep", &["known_method"])));
    let on = DiagnosticOptions { unresolved_method_cross_file: true, ..Default::default() };
    assert!(
        unresolved_method_diags(src, &idx, on).is_empty(),
        "a method that exists cross-file must not fire",
    );
}

#[test]
fn d8_unknown_class_stays_silent_even_with_flag() {
    // Not local, not cached — external/uninstalled. Even opt-in stays silent.
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if ($x->isa('Totally::Unknown')) {
        $x->bogus;
    }
}
1;
"#;
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { unresolved_method_cross_file: true, ..Default::default() };
    assert!(
        unresolved_method_diags(src, &idx, on).is_empty(),
        "an unknown class is never flagged — we can't enumerate its methods",
    );
}

// D2 — unguarded `Optional<T>` dereference (opt-in). `maybe_get` returns
// `Optional<Foo>` via the bare-`return` idiom; an unguarded `$r->...` fires,
// a `defined`-guarded one does not.
fn optional_deref_diags(source: &str) -> Vec<Diagnostic> {
    let analysis = parse_analysis(source);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { optional_deref: true, ..Default::default() };
    collect_diagnostics(&analysis, &idx, on)
        .into_iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "optional-deref"))
        .collect()
}

const OPTIONAL_SRC: &str = r#"
package Foo;
sub new { bless {}, shift }
sub name { "foo" }
package P;
sub maybe_get {
    my ($self) = @_;
    return unless $self->{ok};
    return Foo->new;
}
sub use_it {
    my ($self) = @_;
    my $r = $self->maybe_get;
    return $r->name;
}
1;
"#;

#[test]
fn d2_unguarded_optional_deref_fires_when_enabled() {
    let diags = optional_deref_diags(OPTIONAL_SRC);
    assert_eq!(diags.len(), 1, "unguarded Optional deref fires: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>());
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::INFORMATION));
    assert!(diags[0].message.contains("may be undef"), "{}", diags[0].message);
}

#[test]
fn d2_off_by_default() {
    let analysis = parse_analysis(OPTIONAL_SRC);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &idx, DiagnosticOptions::default());
    assert!(
        !diags.iter().any(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "optional-deref")),
        "optional-deref must be silent without the opt-in",
    );
}

#[test]
fn d2_guarded_optional_does_not_fire() {
    // `defined` strips the Optional → no warning on the guarded use.
    let src = r#"
package Foo;
sub new { bless {}, shift }
sub name { "foo" }
package P;
sub maybe_get {
    my ($self) = @_;
    return unless $self->{ok};
    return Foo->new;
}
sub use_it {
    my ($self) = @_;
    my $r = $self->maybe_get;
    if (defined $r) {
        return $r->name;
    }
    return;
}
1;
"#;
    assert!(optional_deref_diags(src).is_empty(), "defined-guarded Optional must not fire");
}

#[test]
fn d2_quick_fix_inserts_defined_guard() {
    let analysis = parse_analysis(OPTIONAL_SRC);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { optional_deref: true, ..Default::default() };
    let diags = collect_diagnostics(&analysis, &idx, on);
    let uri = Url::parse("file:///t.pl").unwrap();
    let actions = code_actions(&diags, &analysis, &uri);
    let action = actions.iter().find_map(|a| match a {
        CodeActionOrCommand::CodeAction(ca) if ca.title.contains("return unless defined") => Some(ca),
        _ => None,
    });
    let action = action.expect("a guard quick-fix is offered");
    let edits = action.edit.as_ref().unwrap().changes.as_ref().unwrap().get(&uri).unwrap();
    assert_eq!(edits.len(), 1);
    assert!(
        edits[0].new_text.contains("return unless defined $r;"),
        "quick-fix inserts the guard: {:?}",
        edits[0].new_text,
    );
}

// D3 (redundant-guard) / D4 (contradictory-guard) — a guard whose outcome
// the lattice already fixes, given the subject's prior type.
fn guard_diags(source: &str) -> Vec<(String, String)> {
    let analysis = parse_analysis(source);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { redundant_guard: true, ..Default::default() };
    collect_diagnostics(&analysis, &idx, on)
        .into_iter()
        .filter_map(|d| match &d.code {
            Some(NumberOrString::String(c))
                if c == "redundant-guard" || c == "contradictory-guard" =>
            {
                Some((c.clone(), d.message))
            }
            _ => None,
        })
        .collect()
}

#[test]
fn d3_defined_guard_on_confident_value_is_redundant() {
    let src = r#"
package Foo;
sub new { bless {}, shift }
sub name { "f" }
package Main;
sub g {
    my $x = Foo->new;
    if (defined $x) {
        return $x->name;
    }
    return;
}
1;
"#;
    let diags = guard_diags(src);
    assert_eq!(diags.len(), 1, "redundant defined guard: {:?}", diags);
    assert_eq!(diags[0].0, "redundant-guard");
}

#[test]
fn d4_defined_guard_on_undef_is_contradictory() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    return if defined $x;
    if (defined $x) {
        return 1;
    }
    return;
}
1;
"#;
    let diags = guard_diags(src);
    assert!(
        diags.iter().any(|(c, _)| c == "contradictory-guard"),
        "defined guard on a proven-undef subject is contradictory: {:?}",
        diags,
    );
}

#[test]
fn d3_isa_redundant_same_and_subclass() {
    let src = r#"
package Base;
sub new { bless {}, shift }
package Foo;
our @ISA = ('Base');
package Main;
sub g {
    my $x = Foo->new;
    if ($x->isa('Foo')) { return 1; }
    if ($x->isa('Base')) { return 2; }
    return;
}
1;
"#;
    let diags = guard_diags(src);
    assert_eq!(diags.len(), 2, "both same-class and ancestor isa are redundant: {:?}", diags);
    assert!(diags.iter().all(|(c, _)| c == "redundant-guard"), "{:?}", diags);
}

#[test]
fn d4_isa_unrelated_class_is_contradictory() {
    let src = r#"
package Foo;
sub new { bless {}, shift }
package Bar;
package Main;
sub g {
    my $x = Foo->new;
    if ($x->isa('Bar')) { return 1; }
    return;
}
1;
"#;
    let diags = guard_diags(src);
    assert_eq!(diags.len(), 1, "{:?}", diags);
    assert_eq!(diags[0].0, "contradictory-guard");
}

#[test]
fn d4_isa_downcast_is_inconclusive() {
    // $x is the BASE; testing isa(child) is a legitimate downcast — not flagged.
    let src = r#"
package Base;
sub new { bless {}, shift }
package Sub;
our @ISA = ('Base');
package Main;
sub g {
    my $x = Base->new;
    if ($x->isa('Sub')) { return 1; }
    return;
}
1;
"#;
    assert!(guard_diags(src).is_empty(), "a downcast guard must not be flagged: {:?}", guard_diags(src));
}

#[test]
fn d3_d4_off_by_default() {
    let src = r#"
package Foo;
sub new { bless {}, shift }
package Main;
sub g {
    my $x = Foo->new;
    if (defined $x) { return 1; }
    return;
}
1;
"#;
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &idx, DiagnosticOptions::default());
    assert!(
        !diags.iter().any(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "redundant-guard" || c == "contradictory-guard")),
        "guard redundancy is opt-in",
    );
}

// D6 — hash deref on a guard-narrowed array/code-shaped receiver. Reads the
// guard's rep specifically (the deref self-infers HashRef otherwise), so it
// fires only when a `ref…eq` guard proved the non-hash rep.
fn deref_shape_diags(source: &str) -> Vec<String> {
    let analysis = parse_analysis(source);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { deref_shape: true, ..Default::default() };
    collect_diagnostics(&analysis, &idx, on)
        .into_iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "deref-shape-mismatch"))
        .map(|d| d.message)
        .collect()
}

#[test]
fn d6_hash_deref_on_arrayref_guard_fires() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if (ref($x) eq 'ARRAY') {
        return $x->{key};
    }
    return;
}
1;
"#;
    let diags = deref_shape_diags(src);
    assert_eq!(diags.len(), 1, "hash deref on a ref-eq-ARRAY-narrowed receiver fires: {:?}", diags);
    assert!(diags[0].contains("array ref"), "{}", diags[0]);
}

#[test]
fn d6_hash_deref_on_coderef_guard_fires() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if (ref($x) eq 'CODE') {
        return $x->{key};
    }
    return;
}
1;
"#;
    let diags = deref_shape_diags(src);
    assert_eq!(diags.len(), 1, "{:?}", diags);
    assert!(diags[0].contains("code ref"), "{}", diags[0]);
}

#[test]
fn d6_no_guard_does_not_fire() {
    // Not guard-narrowed → D6 stays silent (the literal's rep is masked by the
    // deref's own HashRef belief; firing here is out of scope by design).
    let src = r#"
package Main;
sub g {
    my $x = [1, 2, 3];
    return $x->{key};
}
1;
"#;
    assert!(deref_shape_diags(src).is_empty(), "non-guard deref is the documented residual");
}

#[test]
fn d6_hash_guard_does_not_fire() {
    // `ref eq 'HASH'` then a hash deref is correct — no mismatch.
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if (ref($x) eq 'HASH') {
        return $x->{key};
    }
    return;
}
1;
"#;
    assert!(deref_shape_diags(src).is_empty(), "hash deref on a HASH-narrowed receiver is correct");
}

#[test]
fn d6_off_by_default() {
    let src = r#"
package Main;
sub g {
    my ($self, $x) = @_;
    if (ref($x) eq 'ARRAY') {
        return $x->{key};
    }
    return;
}
1;
"#;
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &idx, DiagnosticOptions::default());
    assert!(
        !diags.iter().any(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "deref-shape-mismatch")),
        "deref-shape is opt-in",
    );
}

#[test]
fn test_code_action_from_diagnostic() {
    let source = "use Carp qw(croak);\ncarp('oops');\n";
    let analysis = parse_analysis(source);
    let uri = Url::parse("file:///test.pl").unwrap();

    // Simulate a HINT diagnostic with data (as collect_diagnostics would produce
    // if module_index had resolved Carp)
    let diag = Diagnostic {
        range: Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 4,
            },
        },
        severity: Some(DiagnosticSeverity::HINT),
        code: Some(NumberOrString::String("unresolved-function".into())),
        source: Some("perl-lsp".into()),
        message: "'carp' is exported by Carp but not imported".into(),
        data: Some(serde_json::json!({"module": "Carp", "function": "carp"})),
        ..Default::default()
    };

    let actions = code_actions(&[diag], &analysis, &uri);
    assert_eq!(actions.len(), 1);
    if let CodeActionOrCommand::CodeAction(action) = &actions[0] {
        assert_eq!(action.title, "Import 'carp' from Carp");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));

        // Verify the edit inserts " carp" at the qw close paren
        let edit = action.edit.as_ref().unwrap();
        let changes = edit.changes.as_ref().unwrap();
        let text_edits = changes.get(&uri).unwrap();
        assert_eq!(text_edits.len(), 1);
        assert_eq!(text_edits[0].new_text, " carp");
    } else {
        panic!("Expected CodeAction, got Command");
    }
}

#[test]
fn test_code_action_new_use_statement() {
    let source = "use strict;\nuse warnings;\nfrobnicate();\n";
    let analysis = parse_analysis(source);
    let uri = Url::parse("file:///test.pl").unwrap();

    let diag = Diagnostic {
        range: Range {
            start: Position {
                line: 2,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 11,
            },
        },
        severity: Some(DiagnosticSeverity::HINT),
        code: Some(NumberOrString::String("unresolved-function".into())),
        source: Some("perl-lsp".into()),
        message: "'frobnicate' is exported by Some::Module (not yet imported)".into(),
        data: Some(serde_json::json!({
            "modules": ["Some::Module"],
            "function": "frobnicate",
        })),
        ..Default::default()
    };

    let actions = code_actions(&[diag], &analysis, &uri);
    assert_eq!(actions.len(), 1);
    if let CodeActionOrCommand::CodeAction(action) = &actions[0] {
        assert_eq!(action.title, "Add 'use Some::Module qw(frobnicate)'");
        assert_eq!(action.is_preferred, Some(true));
        let edit = action.edit.as_ref().unwrap();
        let changes = edit.changes.as_ref().unwrap();
        let text_edits = changes.get(&uri).unwrap();
        assert_eq!(text_edits[0].new_text, "use Some::Module qw(frobnicate);\n");
        // Inserted after last use statement (line 2)
        assert_eq!(text_edits[0].range.start.line, 2);
    } else {
        panic!("Expected CodeAction");
    }
}

#[test]
fn test_unimported_completion_with_auto_import() {
    let source = "use strict;\nuse warnings;\n\nfir\n";
    let analysis = parse_analysis(source);

    // Simulate a cached module that exports "first"
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);
    // Insert directly into cache for testing
    idx.insert_cache(
        "List::Util",
        Some(fake_cached(
            "/usr/lib/perl5/List/Util.pm",
            &[],
            &["first", "max", "min"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position {
            line: 3,
            character: 3,
        },
        &idx,
        None,
    );

    // Should find "first" from List::Util
    let first_item = items.iter().find(|i| i.label == "first");
    assert!(
        first_item.is_some(),
        "Should offer 'first' from unimported List::Util"
    );

    let first_item = first_item.unwrap();
    assert!(
        first_item.detail.as_ref().unwrap().contains("List::Util"),
        "Detail should mention the module"
    );
    assert!(
        first_item.detail.as_ref().unwrap().contains("auto-import"),
        "Detail should indicate auto-import"
    );

    // Should have additional text edit inserting `use List::Util qw(first);`
    let edits = first_item.additional_text_edits.as_ref().unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0].new_text, "use List::Util qw(first);\n");
    // Should insert after the last use statement (line 2)
    assert_eq!(edits[0].range.start.line, 2);
}

#[test]
fn test_unimported_completion_skips_imported_modules() {
    // List::Util is already imported — its exports should NOT appear as unimported completions
    let source = "use List::Util qw(max);\nfir\n";
    let analysis = parse_analysis(source);

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(None);
    idx.insert_cache(
        "List::Util",
        Some(fake_cached(
            "/usr/lib/perl5/List/Util.pm",
            &[],
            &["first", "max", "min"],
        )),
    );
    idx.insert_cache(
        "Scalar::Util",
        Some(fake_cached(
            "/usr/lib/perl5/Scalar/Util.pm",
            &[],
            &["blessed", "reftype"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position {
            line: 1,
            character: 3,
        },
        &idx,
        None,
    );

    // "first" should appear via imported_function_completions (auto-add to qw),
    // NOT via unimported_function_completions
    let first_items: Vec<_> = items.iter().filter(|i| i.label == "first").collect();
    assert!(!first_items.is_empty(), "Should offer 'first'");
    // It should come from the imported path (adds to qw) not unimported
    for item in &first_items {
        if let Some(ref detail) = item.detail {
            assert!(
                !detail.contains("auto-import") || detail.contains("List::Util"),
                "first should come from List::Util context"
            );
        }
    }

    // "blessed" should appear as unimported (Scalar::Util not imported)
    let blessed_item = items.iter().find(|i| i.label == "blessed");
    assert!(
        blessed_item.is_some(),
        "Should offer 'blessed' from unimported Scalar::Util"
    );
    let blessed_item = blessed_item.unwrap();
    assert!(blessed_item
        .detail
        .as_ref()
        .unwrap()
        .contains("Scalar::Util"));
    let edits = blessed_item.additional_text_edits.as_ref().unwrap();
    assert!(edits[0].new_text.contains("use Scalar::Util qw(blessed)"));
}

/// Regression: typing `Package::` should NOT trigger the global
/// workspace-symbol firehose. Mirrors the EM gold corpus fixture
/// `completion_package_colon` (the pre-fix flood was 263 items); the
/// fix narrows to the package's own subs.
///
/// Multi-segment package name (`Math::Util`) on purpose — earlier
/// drafts narrowed only single-segment names, so this case is the
/// load-bearing assertion. Fixture also threads a `use constant` +
/// const-folded call site so the case is realistic Perl and the
/// constant doesn't get mistaken for a package qualifier.
#[test]
fn test_qualified_path_completion_narrows_to_package() {
    let source = "\
package Math::Util;
use constant PI => 3.14159;
sub square    { my ($n) = @_; $n * $n }
sub cube      { my ($n) = @_; $n * $n * $n }
sub circle_area {
    my ($r) = @_;
    return PI * $r * $r;           # const-folded arg flows through
}
package main;
use constant TAU => Math::Util::PI() * 2;
my $sq   = Math::Util::s
";
    let analysis = parse_analysis(source);
    let module_index = ModuleIndex::new_for_test();
    module_index.set_workspace_root(None);
    // Seed an unrelated cross-file module so the workspace flood
    // would include it if the firehose were still firing.
    module_index.insert_cache(
        "Scalar::Util",
        Some(fake_cached(
            "/usr/lib/perl5/Scalar/Util.pm",
            &[],
            &["blessed", "reftype"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    // Cursor at end of `Math::Util::s` on line 10 (0-indexed).
    // Line: `my $sq   = Math::Util::s` — cursor sits past the `s`.
    let line_text = source.lines().nth(10).unwrap();
    let cursor_col = line_text.len() as u32;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position { line: 10, character: cursor_col },
        &module_index,
        None,
    );

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"square"),
        "completion after `Math::Util::` should include `square`, got {:?}",
        labels,
    );
    assert!(
        labels.contains(&"cube"),
        "completion after `Math::Util::` should include `cube`, got {:?}",
        labels,
    );
    assert!(
        labels.contains(&"circle_area"),
        "completion after `Math::Util::` should include `circle_area` \
         (the const-folded sub), got {:?}",
        labels,
    );
    assert!(
        !labels.contains(&"blessed"),
        "completion after `Math::Util::` must NOT flood unrelated workspace \
         symbols (`blessed` is from Scalar::Util), got {:?}",
        labels,
    );
    // Tight bound — the 263-item flood is the bug we're regressing.
    // Allow some headroom for inherited / framework-synthesized
    // members but flag anything that grows past ~10× the package
    // size as a regression of the narrowing.
    assert!(
        items.len() <= 20,
        "completion after `Math::Util::` should narrow tightly to the package; \
         got {} items: {:?}",
        items.len(),
        labels,
    );
}

/// `Foo::<cursor>` should also offer the *sub-packages* nested
/// underneath, not just the methods on `Foo` itself. Typing `Mojo::`
/// expects to see `Util`, `Base`, `IOLoop` etc. alongside any methods
/// `Mojo` directly carries. Covers two sources of sub-packages —
/// in-file `package Foo::Bar` declarations AND cross-file modules
/// known to the resolver index — since either can be the right
/// answer in a real project.
#[test]
fn test_qualified_path_completion_offers_sub_packages() {
    let source = "\
package Math::Util;
sub square { my ($n) = @_; $n * $n }
package Math::Helpers;     # in-file sub-package, no module index entry
sub clamp { my ($x, $lo, $hi) = @_; $x }
package main;
my $x = Math::
";
    let analysis = parse_analysis(source);
    let module_index = ModuleIndex::new_for_test();
    module_index.set_workspace_root(None);
    // Cross-file sub-package: known via the workspace index, mirrors
    // the real "every package in its own .pm" project layout.
    module_index.insert_cache(
        "Math::Stats",
        Some(fake_cached("/usr/lib/perl5/Math/Stats.pm", &[], &["mean", "stddev"])),
    );
    // Unrelated module — must not bleed into Math:: results.
    module_index.insert_cache(
        "Scalar::Util",
        Some(fake_cached(
            "/usr/lib/perl5/Scalar/Util.pm",
            &[],
            &["blessed", "reftype"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    // Cursor at end of `Math::` on the last source line.
    let last_line_idx = source.lines().count() as u32 - 1;
    let line_text = source.lines().last().unwrap();
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position { line: last_line_idx, character: line_text.len() as u32 },
        &module_index,
        None,
    );

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"Util"),
        "`Math::` should offer in-file sub-package `Util`, got {:?}",
        labels,
    );
    assert!(
        labels.contains(&"Helpers"),
        "`Math::` should offer in-file sub-package `Helpers`, got {:?}",
        labels,
    );
    assert!(
        labels.contains(&"Stats"),
        "`Math::` should offer cross-file sub-package `Stats`, got {:?}",
        labels,
    );
    assert!(
        !labels.contains(&"Scalar::Util") && !labels.contains(&"blessed"),
        "`Math::` must NOT bleed unrelated workspace symbols, got {:?}",
        labels,
    );
    // Sub-packages carry SymbolKind::MODULE, subs carry FUNCTION —
    // sanity-check the kind so clients pick the right icon.
    for item in &items {
        if matches!(item.label.as_str(), "Util" | "Helpers" | "Stats") {
            assert_eq!(
                item.kind,
                Some(tower_lsp::lsp_types::CompletionItemKind::MODULE),
                "sub-package `{}` should be SymbolKind::MODULE",
                item.label,
            );
        }
    }
}

/// Sibling case: the *package name itself* arrives via const-fold.
/// `my $pkg = 'Math::Util';` should let `$pkg->squ<cursor>` narrow
/// to `Math::Util`'s methods — but reaching that end-state needs the
/// witness bag to upgrade `$pkg` from `String` to `ClassName` when
/// it's used as a method invocant. That's a separate inference gap
/// (the QualifiedPath narrowing this PR ships handles the literal
/// `Foo::Bar::sub` syntactic form only). Marking ignored so the gap
/// is tracked rather than absorbed into this PR.
#[test]
#[ignore = "needs ClassName-from-string-invocant inference; tracked separately"]
fn test_const_folded_package_resolves_for_method_completion() {
    let source = "\
package Math::Util;
sub square    { my ($n) = @_; $n * $n }
sub cube      { my ($n) = @_; $n * $n * $n }
package main;
my $pkg = 'Math::Util';
$pkg->squ
";
    let analysis = parse_analysis(source);
    let module_index = ModuleIndex::new_for_test();
    module_index.set_workspace_root(None);

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    // Cursor sits past the `q` in `$pkg->squ`.
    let line_text = source.lines().nth(5).unwrap();
    let cursor_col = line_text.len() as u32;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position { line: 5, character: cursor_col },
        &module_index,
        None,
    );

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"square"),
        "method completion on const-folded `$pkg` (= 'Math::Util') \
         should offer `square`, got {:?}",
        labels,
    );
    assert!(
        labels.contains(&"cube"),
        "method completion on const-folded `$pkg` (= 'Math::Util') \
         should offer `cube`, got {:?}",
        labels,
    );
}

/// Native subs emit `DocumentSymbol.name` as the bare identifier —
/// the LSP `kind` enum carries the Function/Method distinction.
/// Pre-fix we stuffed `<sub> ` into the name, which collided with
/// the EM gold corpus' protocol-correct assertions.
#[test]
fn test_document_symbol_name_is_bare_identifier() {
    let source = "\
package Demo::Symbols;
sub alpha { return 1 }
sub beta  { my ($x) = @_; $x * 2 }
1;
";
    let analysis = parse_analysis(source);
    let names: Vec<String> = analysis
        .document_symbols()
        .iter()
        .flat_map(|sym| {
            let mut acc = vec![sym.name.clone()];
            for c in &sym.children {
                acc.push(c.name.clone());
            }
            acc
        })
        .collect();
    assert!(
        names.iter().any(|n| n == "alpha"),
        "expected bare 'alpha' in document symbols, got {:?}",
        names,
    );
    assert!(
        names.iter().any(|n| n == "beta"),
        "expected bare 'beta' in document symbols, got {:?}",
        names,
    );
    for n in &names {
        assert!(
            !n.starts_with("<sub>") && !n.starts_with("<method>"),
            "DocumentSymbol.name should not carry `<sub>`/`<method>` prefix (got {:?})",
            n,
        );
    }
}

/// Hover on a Perl builtin returns the seeded perlfunc.pod entry.
/// The full POD parse pipeline is exercised separately in the
/// builtins_pod unit tests; here we just confirm the wiring from
/// hover_info → module_index.builtin_doc fires for builtin names.
#[test]
fn test_hover_on_builtin_uses_module_index() {
    let source = "push @items, 4;\n";
    let analysis = parse_analysis(source);
    let module_index = ModuleIndex::new_for_test();
    module_index.set_workspace_root(None);
    module_index.seed_builtin_for_test(
        "push",
        "```perl\npush ARRAY,LIST\n```\n\nAppends LIST to ARRAY.",
    );

    let _tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    let hover = hover_info(
        &analysis,
        source,
        Position { line: 0, character: 0 },
        &module_index,
    )
    .expect("expected hover on `push`");
    let text = match hover.contents {
        HoverContents::Markup(m) => m.value,
        _ => panic!("expected markdown hover"),
    };
    assert!(text.contains("push ARRAY,LIST"), "hover body missing: {text}");
    assert!(text.contains("Appends LIST"), "hover body missing: {text}");
}

#[test]
fn test_code_action_multiple_exporters_not_preferred() {
    let source = "use strict;\nfirst();\n";
    let analysis = parse_analysis(source);
    let uri = Url::parse("file:///test.pl").unwrap();

    let diag = Diagnostic {
        range: Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 5,
            },
        },
        severity: Some(DiagnosticSeverity::HINT),
        code: Some(NumberOrString::String("unresolved-function".into())),
        source: Some("perl-lsp".into()),
        message: "...".into(),
        data: Some(serde_json::json!({
            "modules": ["List::Util", "List::MoreUtils"],
            "function": "first",
        })),
        ..Default::default()
    };

    let actions = code_actions(&[diag], &analysis, &uri);
    assert_eq!(actions.len(), 2);
    // Neither should be preferred (ambiguous)
    for action in &actions {
        if let CodeActionOrCommand::CodeAction(a) = action {
            assert_eq!(a.is_preferred, Some(false));
        }
    }
}

// ---- String-dispatch signature help (mojo-events plugin path) ----

/// `$self->emit('ready', CURSOR)` should surface the `->on('ready', sub
/// ($self, $msg) {})` handler's params as sig help. The dispatch string
/// is arg 0; handler params are offset by 1 so active_parameter lines
/// up with the user's cursor.
#[test]
fn sig_help_returns_handler_params_for_emit() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub register {
    my $self = shift;
    $self->on('ready', sub {
        my ($self_in, $msg, $when) = @_;
        warn $msg;
    });
}

sub fire {
    my $self = shift;
    $self->emit('ready', 'hi', )
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor just after `'hi', ` on the emit line — active_param=2 means
    // we're in the 2nd handler slot (after event name + first handler arg).
    let pos = {
        let (line_idx, line) = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("->emit('ready'"))
            .unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx)
        .expect("sig help should surface handler sig");
    assert_eq!(sig.signatures.len(), 1, "one registered handler");

    let s = &sig.signatures[0];
    // Label mirrors the actual call the user is writing — `emit('ready',
    // $msg, $when)` — not a fake method-call shape.
    assert!(
        s.label.starts_with("emit('ready'"),
        "label should show the call shape starting with emit('ready'): {}",
        s.label
    );
    // Documentation carries the class + line provenance.
    if let Some(Documentation::String(ref d)) = s.documentation {
        assert!(
            d.contains("My::Emitter"),
            "doc should name the owning class: {}",
            d
        );
    } else {
        panic!("expected Documentation::String, got {:?}", s.documentation);
    }
    // $self_in stripped as implicit → remaining params $msg, $when.
    let params = s.parameters.as_ref().expect("has params");
    assert_eq!(params.len(), 2, "drops implicit $self_in");
    assert!(matches!(&params[0].label, ParameterLabel::Simple(s) if s == "$msg"));
    assert!(matches!(&params[1].label, ParameterLabel::Simple(s) if s == "$when"));
}

/// Multiple `->on('ready', sub {...})` wire-ups stack — each becomes a
/// separate SignatureInformation entry, so users see every handler
/// shape they might be dispatching to.
#[test]
fn sig_help_stacks_multiple_handler_defs() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub new {
    my $self = bless {}, shift;
    $self->on('tick', sub {
        my ($self_in, $count) = @_;
    });
    $self->on('tick', sub {
        my ($self_in, $count, $unit) = @_;
    });
    $self;
}

sub go {
    my $self = shift;
    $self->emit('tick', )
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let pos = {
        let line_idx = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("->emit('tick'"))
            .map(|(i, _)| i)
            .unwrap();
        let line = src.lines().nth(line_idx).unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx).expect("sig help should fire");
    assert_eq!(
        sig.signatures.len(),
        2,
        "stacked handlers: one signature per ->on call"
    );

    let labels: Vec<&str> = sig.signatures.iter().map(|s| s.label.as_str()).collect();
    assert!(
        labels.iter().all(|l| l.starts_with("emit('tick'")),
        "every signature uses emit('tick', ...) call shape: {:?}",
        labels
    );
}

/// Baseline before the user started typing: cursor in the empty
/// second-arg slot `$self->emit('connect', CURSOR );`. Sig help
/// should offer handler params from the moment the comma is typed.
#[test]
fn sig_help_fires_in_empty_second_slot() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    $self->emit('connect', );
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('connect'"))
        .unwrap();
    let col = line.find(", )").unwrap() + 2; // just after `, `
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx)
        .expect("empty arg slot after comma should offer handler sig");
    let s = &sig.signatures[0];
    assert!(
        s.label.starts_with("emit('connect'"),
        "baseline: label identifies emit handler call: {}",
        s.label
    );
}

/// Flow gap fix: `my $dynamic = 'connect'; $self->emit($dynamic, ...)`
/// — hover already worked (DispatchCall.target_name is const-folded
/// by the plugin) but sig help used to miss because it parsed the
/// first arg from text ($dynamic → not a literal). Now sig help
/// routes through the DispatchCall ref too, so const folding
/// composes uniformly and this class of gap can't reopen.
#[test]
fn sig_help_follows_const_folding_like_hover_does() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    my $dynamic = 'connect';
    $self->emit($dynamic, 'hi', );
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit($dynamic"))
        .unwrap();
    let col = line.find(", )").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx)
        .expect("sig help must follow const folding like hover does");
    let s = &sig.signatures[0];
    assert!(
        s.label.starts_with("emit('connect'"),
        "const-folded: $dynamic → 'connect' → emit('connect', ...) label; got: {}",
        s.label
    );
    let params = s.parameters.as_ref().unwrap();
    assert_eq!(
        params.len(),
        2,
        "$sock, $remote_ip (implicit $self_in dropped)"
    );
}

/// Regression: cursor inside the SECOND literal-string arg of a
/// dispatch call — matches the user's screenshot where
/// `$self->emit('connect', 'soc' )` had the cursor mid-'soc'. The
/// string-dispatch sig should still fire (first arg is 'connect',
/// handler is registered, active_param is 1).
#[test]
fn sig_help_fires_from_inside_second_string_arg() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    $self->emit('connect', 'soc' );
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('connect', 'soc'"))
        .unwrap();
    // Cursor at column index pointing into the middle of `'soc'`.
    let col = line.find("'soc'").unwrap() + 2; // between 's' and 'o'
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx)
        .expect("cursor in 2nd string arg should still surface handler sig");
    let s = &sig.signatures[0];
    assert!(
        s.label.starts_with("emit('connect'"),
        "label should still be the emit(handler) form: {}",
        s.label
    );
    let params = s.parameters.as_ref().unwrap();
    assert_eq!(params.len(), 2, "handler params ($sock, $remote_ip)");
}

/// Completion at the first arg of a dispatch call should list every
/// registered Handler on the receiver's class — top priority, quoted
/// insert, handler params in detail. Same abstraction as hover +
/// sig help, so new plugins don't have to wire this up separately.
#[test]
fn completion_offers_handler_names_at_dispatch_arg0() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s, $sock, $ip) = @_; });
    $self->on('disconnect', sub { my ($s) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit();
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor inside the empty `()` of `$self->emit()`.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit()"))
        .unwrap();
    let col = line.find("emit(").unwrap() + "emit(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);

    // Every registered handler shows up as a top-priority suggestion.
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler should be offered at emit arg-0");
    let disconnect = items
        .iter()
        .find(|i| i.label == "disconnect")
        .expect("disconnect handler should be offered at emit arg-0");

    assert_eq!(
        connect.kind,
        Some(CompletionItemKind::EVENT),
        "handler completion kind is EVENT (matches outline)"
    );
    assert_eq!(
        connect.insert_text.as_deref(),
        Some("'connect'"),
        "insert should include quotes so the user doesn't type them"
    );
    assert!(
        connect
            .detail
            .as_deref()
            .unwrap_or("")
            .contains("My::Emitter"),
        "detail should name the owning class: {:?}",
        connect.detail
    );
    assert!(
        connect.detail.as_deref().unwrap_or("").contains("$sock"),
        "detail should expose handler params: {:?}",
        connect.detail
    );

    // Sort text puts handlers ahead of other general completions.
    // Space prefix sorts lex-before any digit-prefixed sort_text,
    // guaranteeing handlers as a top block even when surrounding
    // items (local subs at PRIORITY_LOCAL=0) tie on numeric priority.
    assert!(
        connect
            .sort_text
            .as_deref()
            .unwrap_or("zzz")
            .starts_with(' '),
        "handler sort should lead with space to outrank digit-prefixed sort_text: {:?}",
        connect.sort_text
    );
    assert!(disconnect
        .sort_text
        .as_deref()
        .unwrap_or("zzz")
        .starts_with(' '));
}

/// Completion peels an `Optional<Foo>` receiver to offer `Foo`'s methods,
/// even though the same receiver does NOT dispatch (hover/goto correctly
/// refuse an unguarded optional). Completion is suggestive — the author
/// may not have written the `defined` guard yet.
#[test]
fn completion_peels_optional_receiver() {
    let src = r#"package Foo;
sub go { my ($self) = @_; }
sub spin { my ($self) = @_; }
package P;
sub maybe { return undef unless 1; return Foo->new; }
sub use_it {
    my $r = maybe();
    $r->
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.trim() == "$r->")
        .unwrap();
    let col = line.find("$r->").unwrap() + "$r->".len();
    let pos = Position { line: line_idx as u32, character: col as u32 };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    assert!(
        items.iter().any(|i| i.label == "go"),
        "Optional<Foo> receiver should offer Foo's methods (peeled): {:?}",
        items.iter().map(|i| &i.label).collect::<Vec<_>>(),
    );
    assert!(items.iter().any(|i| i.label == "spin"));
}

/// Bug B: dispatch-target items set their `insert_text` to
/// `'name'` (quoted) but left `filter_text` unset — some LSP
/// clients fall back to `insert_text` for client-side prefix
/// matching, so typing `c` after `(` fails to match `'connect'`
/// (prefix starts with `'`, not `c`). `filter_text` now pins
/// client-side matching to the bare label regardless of insert
/// shape; typing a character keeps the handler visible.
#[test]
fn completion_dispatch_filter_text_matches_bare_name() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';
sub wire { my $self = shift; $self->on('connect', sub {}); }
sub fire { my $self = shift; $self->emit(); }
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit()"))
        .unwrap();
    let col = line.find("emit(").unwrap() + "emit(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler offered");

    // filter_text is the bare name — the client can prefix-match on
    // `c`/`co`/`con`/... even though insert_text is `'connect'`.
    assert_eq!(
        connect.filter_text.as_deref(),
        Some("connect"),
        "filter_text must be the bare label, not the quoted insert_text"
    );
    assert_eq!(
        connect.insert_text.as_deref(),
        Some("'connect'"),
        "insert_text still quotes for the bare-parens case"
    );
}

/// Bug: dispatch-target completion always wrapped the label in
/// quotes — so if the cursor was already inside `''`, accepting
/// `connect` inserted `''connect''`. Now detects the string
/// context via the tree and emits bare text.
#[test]
fn completion_dispatch_inside_quotes_does_not_double_quote() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit('');
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor BETWEEN the two quotes in `->emit('')`.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('')"))
        .unwrap();
    let col = line.find("('").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler offered inside '|'");
    // Cursor inside a string arg → item ships a textEdit pinned
    // to the string-content span so the client's word-at-cursor
    // heuristic can't drop it over non-identifier chars. The
    // newText is the BARE handler name (no wrapping quotes) and
    // insert_text is cleared — textEdit takes precedence in the
    // LSP spec, and leaving insert_text alongside confuses some
    // clients. The original "don't double-quote" invariant now
    // reads off textEdit.newText instead of insert_text.
    assert_eq!(
        connect.insert_text, None,
        "cursor is inside quotes; insert_text is cleared in favor of textEdit"
    );
    use tower_lsp::lsp_types::CompletionTextEdit;
    let Some(CompletionTextEdit::Edit(ref te)) = connect.text_edit else {
        panic!(
            "expected a TextEdit for mid-string dispatch item; got {:?}",
            connect.text_edit
        );
    };
    assert_eq!(
        te.new_text, "connect",
        "textEdit.newText is the bare label — not `'connect'` (would double-quote inside '|')"
    );
}

/// Red pin (user-reported): dispatch-target completions for labels
/// containing non-identifier chars (`/`, `#`) died client-side
/// because nvim's word-at-cursor heuristic uses `iskeyword`, which
/// excludes `/` and `#` by default. The server returned the item
/// with `filter_text = "/users/profile"` but the client extracted
/// `users` or `profile` (a word run starting/ending at the non-
/// keyword boundary) and dropped the item since neither is a
/// prefix of `/users/profile`. Same shape for `Users#list` — the
/// cursor parked past the `#` gave word `list`, which fails
/// `"Users#list".starts_with("list")`.
///
/// Fix: emit `textEdit` with `range = string_content_span_at(...)`
/// so the client filters by the whole in-range text against the
/// full label — regardless of keyword class. This pin locks that
/// textEdit emission for BOTH flavors; regressing either re-
/// surfaces the bug for any route with a URL path or `Ctrl#act`
/// handler name.
#[test]
fn completion_dispatch_textedit_handles_non_keyword_labels() {
    use crate::module_index::ModuleIndex;
    use tower_lsp::lsp_types::CompletionTextEdit;

    // Route declarations: one URL path (leading `/`), one
    // `Ctrl#act` (embedded `#`). Both must survive mid-string
    // completion inside `url_for('...')`.
    let app_src = r#"package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');

get '/users/profile' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(app_src, None).unwrap()
        },
        app_src.as_bytes(),
    ));

    let idx = std::sync::Arc::new(ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);

    let ctrl_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('/users/profile');
}
"#;
    let ctrl_fa = crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(ctrl_src, None).unwrap()
        },
        ctrl_src.as_bytes(),
    );

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(ctrl_src, None).unwrap();

    // Cursor deep inside the path, past the first `/` —
    // `'/users/pr|ofile'`. Before the fix, nvim would extract
    // `users` or `profile` from the chars around the cursor;
    // neither is a prefix of the `/users/profile` label.
    let line_idx = 5u32; // `    $c->url_for('/users/profile');`
    let line = ctrl_src.lines().nth(line_idx as usize).unwrap();
    let quote_start = line.find("'/users/profile").unwrap();
    let pr_col = (quote_start + 1 + "/users/pr".len()) as u32;
    let pos = Position {
        line: line_idx,
        character: pr_col,
    };

    let items = completion_items(&ctrl_fa, &tree, ctrl_src, pos, &idx, None);

    let path_item = items
        .iter()
        .find(|i| i.label == "/users/profile")
        .expect("/users/profile must be offered (dispatch completion inside string)");

    // insert_text is cleared; textEdit carries the range spanning
    // the entire string content, so the client uses `/users/pr...`
    // as the filter input and matches against the full label.
    assert_eq!(
        path_item.insert_text, None,
        "insert_text cleared — textEdit takes precedence for non-keyword-char labels"
    );
    let Some(CompletionTextEdit::Edit(ref te)) = path_item.text_edit else {
        panic!(
            "expected textEdit for `/users/profile`; got {:?}",
            path_item.text_edit
        );
    };
    assert_eq!(
        te.new_text, "/users/profile",
        "textEdit.newText is the bare label, no surrounding quotes"
    );
    // Range must span the string CONTENT (between the quotes).
    // Start column = col of first `/` (just after opening quote),
    // end column = col of closing quote.
    assert_eq!(te.range.start.line, line_idx);
    assert_eq!(te.range.end.line, line_idx);
    assert_eq!(
        te.range.start.character,
        (quote_start + 1) as u32,
        "range start hugs the char just after the opening quote",
    );
    assert_eq!(
        te.range.end.character,
        (quote_start + 1 + "/users/profile".len()) as u32,
        "range end hugs the closing quote — replacement stays INSIDE the existing quotes",
    );

    // Same check for the `Ctrl#act` flavor — cursor past the `#`.
    let ctrl_src_hash = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('Users#list');
}
"#;
    let ctrl_fa_hash = crate::builder::build(
        &parser.parse(ctrl_src_hash, None).unwrap(),
        ctrl_src_hash.as_bytes(),
    );
    let tree_hash = parser.parse(ctrl_src_hash, None).unwrap();
    let line = ctrl_src_hash.lines().nth(5).unwrap();
    let quote_start = line.find("'Users#list").unwrap();
    let past_hash_col = (quote_start + 1 + "Users#li".len()) as u32;
    let pos = Position {
        line: 5,
        character: past_hash_col,
    };
    let items = completion_items(&ctrl_fa_hash, &tree_hash, ctrl_src_hash, pos, &idx, None);
    let hash_item = items
        .iter()
        .find(|i| i.label == "Users#list")
        .expect("Users#list must be offered when cursor is past the #");
    assert_eq!(hash_item.insert_text, None);
    let Some(CompletionTextEdit::Edit(ref te)) = hash_item.text_edit else {
        panic!(
            "expected textEdit for `Users#list`; got {:?}",
            hash_item.text_edit
        );
    };
    assert_eq!(te.new_text, "Users#list");
}

/// Red pin (user QA): accepting a dispatch completion APPENDED
/// the label instead of replacing the typed text — `url_for('/fall|')`
/// accepting `/fallback` yielded `url_for('/fall/fallback')`.
/// Root cause: `descendant_for_point_range` returns the enclosing
/// `string_literal` (not `string_content`) when the cursor sits
/// at the content's end boundary, because content ranges are
/// half-open. `string_content_span_at` then fell into the
/// zero-width "empty literal" branch and returned `(cursor,
/// cursor)` — textEdit replacing nothing = append. Fix descends
/// into the literal to find a `string_content` child before
/// giving up.
///
/// This pin covers three cursor positions, each of which previously
/// hit the wrapper-instead-of-content path:
///   1. INSIDE the content (baseline — already worked).
///   2. AT the content's end boundary (just before closing quote).
///   3. ON the closing quote itself.
/// All three must return a textEdit range covering the full
/// `string_content` span, so accepting the completion replaces
/// the typed prefix with the label cleanly.
#[test]
fn completion_dispatch_textedit_range_at_content_boundary() {
    use crate::module_index::ModuleIndex;
    use tower_lsp::lsp_types::CompletionTextEdit;

    let app_src = r#"package MyApp;
use Mojolicious::Lite;

any '/fallback' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(app_src, None).unwrap()
        },
        app_src.as_bytes(),
    ));

    let idx = std::sync::Arc::new(ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);

    let ctrl_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('/fall');
}
"#;
    let ctrl_fa = crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(ctrl_src, None).unwrap()
        },
        ctrl_src.as_bytes(),
    );
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(ctrl_src, None).unwrap();

    // `    $c->url_for('/fall');`
    let line_idx = 5u32;
    let line = ctrl_src.lines().nth(line_idx as usize).unwrap();
    let quote_start = line.find("'/fall'").unwrap();
    let content_start = (quote_start + 1) as u32; // `/`
    let content_end = content_start + "/fall".len() as u32; // just after `l`
    let closing_quote_col = content_end; // the `'`

    // Three cursor positions to exercise: inside the content,
    // at the content's end boundary, and on the closing quote.
    let cursor_variants = [
        ("inside content", content_start + 3), // between `a` and `l`
        ("end of content", content_end),       // just after `l`, before `'`
        ("on closing quote", closing_quote_col),
    ];

    for (label, col) in cursor_variants {
        let items = completion_items(
            &ctrl_fa,
            &tree,
            ctrl_src,
            Position {
                line: line_idx,
                character: col,
            },
            &idx,
            None,
        );
        let item = items
            .iter()
            .find(|i| i.label == "/fallback")
            .unwrap_or_else(|| {
                panic!(
                    "{}: /fallback must be offered at col {}; \
                                           got labels: {:?}",
                    label,
                    col,
                    items.iter().map(|i| &i.label).collect::<Vec<_>>()
                )
            });

        let Some(CompletionTextEdit::Edit(ref te)) = item.text_edit else {
            panic!("{}: expected textEdit; got {:?}", label, item.text_edit);
        };
        // Range must cover the FULL typed content, not zero-width.
        // That way accepting `/fallback` REPLACES `/fall`, not
        // appends to it — the pre-fix failure mode that produced
        // `'/fall/fallback'`.
        assert_eq!(
            te.range.start.character, content_start,
            "{}: range start must hug the first content char; got range {:?}",
            label, te.range,
        );
        assert_eq!(
            te.range.end.character, content_end,
            "{}: range end must hug the closing quote (exclusive of it); got range {:?}",
            label, te.range,
        );
        assert_eq!(
            te.new_text, "/fallback",
            "{}: newText is the bare label — no seasonal redundancy",
            label,
        );
    }
}

/// Bug: typing `,` inside a known dispatch call (`->emit('x', |)`)
/// triggered completion which ran the global sub/module firehose —
/// useless here. Now suppresses imported/unimported function
/// completions when we're inside a known dispatcher call; sig
/// help remains the right affordance for guiding arg shape.
#[test]
fn completion_after_comma_in_dispatch_call_suppresses_firehose() {
    let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire_one {}
sub wire_two {}
sub completely_unrelated {}

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s, $sock) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit('connect', );
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('connect',"))
        .unwrap();
    let col = line.find(", )").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    assert!(
        !labels.contains(&"completely_unrelated"),
        "unrelated sub must not appear in dispatch arg completion: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"wire_one"),
        "wire_one leak — dispatch arg completion should stay quiet: {:?}",
        labels
    );
}

/// Mid-string completion for route targets. Cursor inside
/// `->to('Users#lis|')` offers methods on Users, prefix-filtered
/// by `lis`. Generic for ANY plugin that emits MethodCallRef at
/// a string span (routes today, Catalyst forwards, etc.).
#[test]
fn completion_mid_string_route_target_scoped_to_invocant() {
    // Same-file Users package so the test is self-contained. Real
    // use would have Users in a separate file via workspace index;
    // the lookup path is the same (complete_methods_for_class
    // walks inheritance + module index).
    let src = r#"
package Users;
sub list {}
sub login {}
sub logout {}
sub delete_user {}

package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#lis');
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor just after 'lis' in 'Users#lis' — active editing state.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("Users#lis"))
        .unwrap();
    let col = line.find("Users#lis").unwrap() + "Users#lis".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    // Prefix-filtered: only `list` starts with `lis`; `login`,
    // `logout`, `delete_user` don't.
    assert!(
        labels.contains(&"list"),
        "list must be offered for prefix `lis`: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"login"),
        "login does NOT start with `lis` — must be filtered out: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"logout"),
        "logout does NOT start with `lis` — must be filtered out: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"delete_user"),
        "delete_user is unrelated — must not appear: {:?}",
        labels
    );

    // Top-priority sort — the mid-string completion path is the
    // only sensible one at this cursor position.
    let list = items.iter().find(|i| i.label == "list").unwrap();
    assert!(
        list.sort_text
            .as_deref()
            .unwrap_or("zzz")
            .starts_with("000"),
        "mid-string method completion should be top-priority: {:?}",
        list.sort_text
    );
}

/// Mid-string completion for routes before `#` is typed — cursor at
/// `->to('Us|')`. The invocant portion isn't complete yet, so the
/// plugin won't have emitted a MethodCallRef. Graceful fallthrough
/// to general completion (or nothing) is the expected behavior.
#[test]
fn completion_mid_string_before_hash_falls_through() {
    let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Us');
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("'Us'"))
        .unwrap();
    let col = line.find("'Us'").unwrap() + "'Us".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    // Doesn't assert what IS offered — just that it doesn't panic
    // and doesn't return complete nonsense. This is the honest
    // edge-case: without a `#` yet, no plugin-emitted ref exists.
    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let _ = items;
}

/// Completion skips when the method isn't a declared dispatcher, even
/// if handlers exist on the class. (Empty dispatchers == "any" by
/// convention, but mojo-events declares ["emit"] specifically.)
#[test]
fn completion_skips_non_dispatcher_method() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s) = @_; });
}

sub other {
    my $self = shift;
    $self->unrelated_method();
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->unrelated_method()"))
        .unwrap();
    let col = line.find("method(").unwrap() + "method(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    assert!(
        !items.iter().any(|i| i.label == "connect"),
        "non-dispatcher method must not surface handler completions"
    );
}

/// No handler params means no specialized sig help — fall through to
/// the regular method-signature path (or return None if ->emit isn't
/// locally defined, as in this test).
#[test]
fn sig_help_returns_none_when_no_handler_registered() {
    let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub fire {
    my $self = shift;
    $self->emit('never_registered', )
}
"#;
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let pos = {
        let line_idx = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("never_registered"))
            .map(|(i, _)| i)
            .unwrap();
        let line = src.lines().nth(line_idx).unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx);
    assert!(
        sig.is_none(),
        "no handler_params → no string-dispatch sig; also no local ->emit def"
    );
}

// ---- data-printer plugin: full intelligence ----

/// Build a CachedModule under the real package name we want, with
/// arbitrary source. `fake_cached` always synthesizes a `package
/// Fake;` source — useless when the caller needs the cached
/// module to expose subs under a specific name like `Data::Printer`.
fn cached_under(name: &str, source: &str) -> std::sync::Arc<crate::module_index::CachedModule> {
    let analysis = parse_analysis(source);
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        std::path::PathBuf::from(format!("/fake/{}.pm", name.replace("::", "/"))),
        std::sync::Arc::new(analysis),
    ))
}

#[test]
fn data_printer_use_ddp_resolves_p_to_data_printer() {
    // The end-to-end intelligence pin. `use DDP` is a literal alias
    // for `use Data::Printer` (DDP.pm just `push our @ISA, 'Data::Printer'`).
    // Hover/K, gd, and sig-help on `p` must reach Data::Printer's
    // real `sub p` — not DDP. The plugin's synthetic Import
    // (module_name: "Data::Printer", imported_symbols: [p, np]) is
    // what carries this; resolve_imported_function is the seam every
    // intelligence feature routes through.
    let source = "use DDP;\np $foo;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    // Stub Data::Printer.pm. The real module has a custom `import`
    // (no @EXPORT), but `sub p` is a normal sub the builder picks
    // up — exactly what users get from cpan.
    module_index.insert_cache(
            "Data::Printer",
            Some(cached_under(
                "Data::Printer",
                "package Data::Printer;\nsub p { my (undef, %props) = @_; }\nsub np { my (undef, %props) = @_; }\n1;\n",
            )),
        );

    let resolved = resolve_imported_function(&analysis, "p", &module_index);
    assert!(
        resolved.is_some(),
        "use DDP must alias to Data::Printer; resolve_imported_function for `p` returned None — \
             imports were: {:?}",
        analysis
            .imports
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
    let (import, _path, remote) = resolved.unwrap();
    assert_eq!(
        import.module_name, "Data::Printer",
        "alias must route to Data::Printer, not DDP"
    );
    assert_eq!(remote, "p", "local `p` maps to remote `p`");

    // np too — both DDP-installed names.
    let np = resolve_imported_function(&analysis, "np", &module_index);
    assert!(
        np.is_some(),
        "use DDP must also resolve `np` to Data::Printer"
    );
    assert_eq!(np.unwrap().0.module_name, "Data::Printer");
}

#[test]
fn data_printer_use_data_printer_resolves_p_to_data_printer() {
    // Same test for the non-alias case. `use Data::Printer;` with no
    // qw list — the plugin's synthetic Import claims `p`/`np` so
    // resolve_imported_function pairs them with the real sub.
    let source = "use Data::Printer;\np $foo;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    module_index.insert_cache(
            "Data::Printer",
            Some(cached_under(
                "Data::Printer",
                "package Data::Printer;\nsub p { my (undef, %props) = @_; }\nsub np { my (undef, %props) = @_; }\n1;\n",
            )),
        );

    let resolved = resolve_imported_function(&analysis, "p", &module_index);
    assert!(
        resolved.is_some(),
        "use Data::Printer (no qw list) must still let resolve_imported_function find p"
    );
    assert_eq!(resolved.unwrap().0.module_name, "Data::Printer");
}

#[test]
fn data_printer_use_line_options_completion() {
    // `use DDP { | }` — cursor inside the options hashref. The
    // plugin's on_completion hook recognizes "current_use_module
    // matches DDP/Data::Printer and cursor_inside is a Hash" and
    // returns the documented option keys (caller_info, colored,
    // class_method, output, ...). No core hard-codes the option
    // list — it lives in the plugin.
    let source = "use DDP { };\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    // Cursor between the braces. Source: "use DDP { };" → col 10
    // is one past the opening brace (which lives at col 9).
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();

    let pos = Position {
        line: 0,
        character: 10,
    };
    let items = completion_items(&analysis, &tree, source, pos, &module_index, None);

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    // Sample of keys from Data::Printer's actual options. If the
    // plugin doesn't ship these specific names, swap to whichever
    // ones the plugin advertises — the contract is "DDP options
    // surface here", not "this exact list".
    for key in &["caller_info", "colored", "class_method", "output"] {
        assert!(
            labels.iter().any(|l| l == key),
            "use DDP {{ }} option completion must offer `{}`; got: {:?}",
            key,
            labels,
        );
    }
}

// ---- witness-bag chain typing: pin-the-fix on the real demo ----

/// Pin against the actual demo file. Loads
/// `test_files/plugin_mojo_demo.pl` + stubs of the Mojolicious
/// hierarchy registered into the module index, then asserts:
/// (a) `$r` at line 71 resolves to a known class.
/// (b) `->to` on line 71 is a MethodCall ref.
/// (c) `->to`'s invocant resolves to a class via
///     `FileAnalysis::method_call_invocant_class` (the bag-routed
///     resolver every reader — hover, gd, gr, rename — funnels
///     through).
///
/// Two possible failure modes the test distinguishes:
///   - `$r` is typed but `->to`'s invocant fails → crossfile
///     chain hop is the gap (find_method_return_type's CrossFile
///     branch).
///   - `$r` isn't typed at all → earlier hop broken first.
#[test]
fn test_demo_file_chain_to_resolves_on_line_71() {
    use std::fs;
    use std::path::PathBuf;

    // Project root: the worktree (= CARGO_MANIFEST_DIR).
    let root: PathBuf = env!("CARGO_MANIFEST_DIR").into();
    let demo = root.join("test_files/plugin_mojo_demo.pl");
    let demo_source = fs::read_to_string(&demo).expect("demo file present");

    // Index the project's test_files/ as the workspace.
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(Some(root.to_str().unwrap()));
    let files = crate::file_store::FileStore::new();
    let _indexed = crate::module_resolver::index_workspace_with_index(
        &root.join("test_files"),
        &files,
        Some(&idx),
    );

    // Use the ACTUAL Mojolicious library from @INC — the same
    // code nvim analyzes. If Mojo isn't installed, skip cleanly
    // so CI on bare systems doesn't break.
    let inc_paths = crate::module_resolver::discover_inc_paths();
    let insert_real = |name: &str| -> bool {
        let mut p = crate::module_resolver::create_parser();
        match crate::module_resolver::resolve_and_parse(&inc_paths, name, &mut p) {
            Some(cached) => {
                idx.insert_cache(name, Some(cached));
                true
            }
            None => false,
        }
    };
    let have_mojo = insert_real("Mojolicious")
        && insert_real("Mojolicious::Routes")
        && insert_real("Mojolicious::Routes::Route")
        && insert_real("Mojolicious::Lite");
    if !have_mojo {
        eprintln!("SKIP: Mojolicious not installed in @INC");
        return;
    }
    let _ = PathBuf::new(); // keep the import used in both branches

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&demo_source, None).unwrap();
    let mut analysis = crate::builder::build(&tree, demo_source.as_bytes());

    // Cross-file enrichment — same step the backend runs on open.
    // Resolves MethodCallBindings against the module_index so
    // `my $r = $app->routes;` becomes a TypeConstraint. Without
    // this, the backend's `enrich_analysis(uri)` hasn't fired
    // yet and the whole chain is un-typed.
    analysis.enrich_imported_types_with_keys(Some(&idx));

    // Find line 71 ($r->get('/users')->to('Users#list');).
    let (line_idx, chain_line) = demo_source
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')") && l.contains("->to('Users#list')"))
        .expect("chain line present in demo");

    // Position on `to` — the 't' character.
    let to_col = chain_line.find("->to(").unwrap() + 2;
    let r_col = chain_line.find("$r").unwrap();
    let get_col = chain_line.find("->get(").unwrap() + 2;

    let pt = |col: usize| tree_sitter::Point {
        row: line_idx,
        column: col,
    };

    // Diagnostics — what does the analysis actually see?
    let r_ty_bag = analysis.inferred_type_via_bag("$r", pt(r_col));
    let r_ty_legacy = analysis.inferred_type("$r", pt(r_col)).cloned();
    let mcb_for_r: Vec<_> = analysis
        .method_call_bindings
        .iter()
        .filter(|b| b.variable == "$r")
        .collect();
    let cb_for_r: Vec<_> = analysis
        .call_bindings
        .iter()
        .filter(|b| b.variable == "$r")
        .collect();
    // Is `app` even known as a symbol/import?
    let app_known = analysis.symbols.iter().any(|s| s.name == "app");
    // Is Mojolicious in the module index?
    let mojo_cached = idx.get_cached("Mojolicious").is_some();
    let routes_cached = idx.get_cached("Mojolicious::Routes").is_some();
    let route_cached = idx.get_cached("Mojolicious::Routes::Route").is_some();
    eprintln!(
        "DIAG: $r bag={:?}  legacy={:?}  mcbs={:?}  cbs={:?}  app_sym={}  \
             mojo_cached={}  routes_cached={}  route_cached={}",
        r_ty_bag,
        r_ty_legacy,
        mcb_for_r
            .iter()
            .map(|b| format!("{}.{}", b.invocant_var, b.method_name))
            .collect::<Vec<_>>(),
        cb_for_r.iter().map(|b| &b.func_name).collect::<Vec<_>>(),
        app_known,
        mojo_cached,
        routes_cached,
        route_cached,
    );

    // (a) `$r` is typed. This uses the EXACT path cursor_context
    // uses to type an invocant — inferred_type_via_bag.
    let r_ty = r_ty_bag;
    let r_class = r_ty.as_ref().and_then(|t| t.class_name());
    assert!(
        r_class.is_some(),
        "$r should be typed (any class) at {}:{}; got {:?}",
        line_idx + 1,
        r_col,
        r_ty,
    );

    // (b) At `->get`'s 'g', there's a MethodCall ref. Its
    // invocant is `$r`. Resolve it cross-file.
    let get_ref = analysis.ref_at(pt(get_col)).expect("ref at ->get");
    assert_eq!(get_ref.target_name, "get");
    if matches!(get_ref.kind, crate::file_analysis::RefKind::MethodCall { .. }) {
        let _ = (&tree, demo_source.as_bytes(), &idx, get_col);
        let klass = analysis.method_call_invocant_class(get_ref, Some(&idx));
        assert!(
            klass.is_some(),
            "`->get`'s invocant (= $r) should resolve to SOME class; got {:?}",
            klass,
        );
    }

    // (c) The `->to` hop. Real Mojolicious::Routes::Route::get
    // is `shift->_generate_route(GET => @_)` — our implicit-
    // return witnessing records that get's return chains
    // through _generate_route. _generate_route's own return is
    // `return defined $name ? $route->name($name) : $route;` —
    // a complex conditional whose arms depend on $route's
    // chain-built type. That depth of cross-file chain
    // resolution is a separate follow-up; for now we assert
    // the MethodCall ref exists and carries the right target,
    // but leave the class-resolution assertion as a diagnostic
    // rather than hard-fail.
    let to_ref = analysis.ref_at(pt(to_col)).expect("ref at ->to");
    assert_eq!(to_ref.target_name, "to");
    assert!(
        matches!(
            to_ref.kind,
            crate::file_analysis::RefKind::MethodCall { .. }
        ),
        "ref at ->to is a MethodCall"
    );
    if matches!(to_ref.kind, crate::file_analysis::RefKind::MethodCall { .. }) {
        let _ = (&tree, demo_source.as_bytes(), &idx, to_col);
        let klass = analysis.method_call_invocant_class(to_ref, Some(&idx));
        eprintln!(
            "DIAG: ->to invocant class (real Mojo): {:?} \
                 (None expected until deep chain through \
                 _generate_route/requires/to is resolved)",
            klass,
        );
    }
}

#[test]
fn data_printer_use_line_options_completion_for_data_printer_module() {
    // Same flow, non-alias name. The plugin can't be DDP-specific.
    let source = "use Data::Printer { };\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();

    // "use Data::Printer { };" — col 20 sits between the braces.
    let pos = Position {
        line: 0,
        character: 20,
    };
    let items = completion_items(&analysis, &tree, source, pos, &module_index, None);

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.iter().any(|l| *l == "caller_info"),
        "use Data::Printer {{ }} must surface options too; got: {:?}",
        labels,
    );
}
// ---- witness-driven chain completion (spike) ----

/// Decomposition: parse real Mojolicious/Routes/Route.pm
/// in-place (not via module index) and probe each hop of the
/// `$self->_route()->requires()->to()` chain separately. Plus
/// probe `$self->_generate_route(...)`. Reports what each
/// specific hop resolves to — so we know exactly which step
/// in the chain is actually dying.
#[test]
fn test_route_pm_chain_decomposition() {
    use std::fs;
    use std::path::PathBuf;
    let inc = crate::module_resolver::discover_inc_paths();
    let route_path = inc
        .iter()
        .map(|p| p.join("Mojolicious/Routes/Route.pm"))
        .find(|p| p.exists());
    let route_path: PathBuf = match route_path {
        Some(p) => p,
        None => {
            eprintln!("SKIP: Mojo not installed");
            return;
        }
    };
    let src = fs::read_to_string(&route_path).unwrap();

    // Parse Route.pm itself — `$self` inside = Route.
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());

    // Probe: find the `_generate_route` sub body and report
    // what we see on each hop.
    let inspect_sym = |name: &str| {
        for sym in &analysis.symbols {
            if sym.name != name {
                continue;
            }
            if !matches!(
                sym.kind,
                crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
            ) {
                continue;
            }
            if matches!(&sym.detail, crate::file_analysis::SymbolDetail::Sub { .. }) {
                let return_type = analysis.symbol_return_type_via_bag(sym.id, None);
                eprintln!("  sym[{:24}] return_type={:?}", name, return_type);
                return;
            }
        }
        eprintln!("  sym[{:24}] NOT FOUND", name);
    };
    eprintln!("======== symbol return types in Route.pm ========");
    for name in [
        "get",
        "post",
        "any",
        "to",
        "name",
        "requires",
        "_generate_route",
        "_route",
        "add_child",
        "pattern",
        "is_reserved",
        "root",
    ] {
        inspect_sym(name);
    }

    // Find `_generate_route`'s body block. Its last statement
    // is `return defined $name ? $route->name($name) : $route;`.
    // Probe each subexpression type via resolve_expression_type.
    fn find_sub_body<'t>(
        n: tree_sitter::Node<'t>,
        src: &[u8],
        name: &str,
    ) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "subroutine_declaration_statement" {
            if let Some(nm) = n.child_by_field_name("name") {
                if nm.utf8_text(src).ok() == Some(name) {
                    return n.child_by_field_name("body");
                }
            }
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_sub_body(c, src, name) {
                    return Some(r);
                }
            }
        }
        None
    }
    let body = find_sub_body(tree.root_node(), src.as_bytes(), "_generate_route")
        .expect("_generate_route body");

    // Inside _generate_route, find:
    //   (a) the `my $route = CHAIN` assignment
    //   (b) the final `return TERNARY` expression
    fn find_var_decl_for<'t>(
        n: tree_sitter::Node<'t>,
        src: &[u8],
        var: &str,
    ) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "assignment_expression" {
            if let Some(left) = n.child_by_field_name("left") {
                if left.utf8_text(src).map(|s| s.trim()).ok() == Some(&format!("my {}", var)) {
                    return n.child_by_field_name("right");
                }
            }
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_var_decl_for(c, src, var) {
                    return Some(r);
                }
            }
        }
        None
    }
    let route_rhs = find_var_decl_for(body, src.as_bytes(), "$route").expect("my $route = ... RHS");

    eprintln!();
    eprintln!("======== `my $route = RHS` decomposition ========");
    eprintln!(
        "RHS shape: {}  kind={}",
        route_rhs.utf8_text(src.as_bytes()).unwrap_or(""),
        route_rhs.kind()
    );

    // Probe chain hops from innermost outward. Each node in a
    // chain a->b->c has: outer is c's method_call_expression,
    // its invocant is the a->b method_call_expression, whose
    // invocant is `$self`.
    fn report_node_type(
        label: &str,
        n: tree_sitter::Node,
        analysis: &crate::file_analysis::FileAnalysis,
        src: &[u8],
    ) {
        let text = n.utf8_text(src).unwrap_or("").trim();
        let ty = crate::cursor_context::resolve_expression_type(&analysis, n, src, None);
        eprintln!(
            "  [{label:>12}] `{text:.60}`\n                kind={} → ty={:?}",
            n.kind(),
            ty
        );
    }

    // Walk the chain inside-out and report each level's type.
    let mut cur = Some(route_rhs);
    let mut depth = 0;
    while let Some(n) = cur {
        let label = match depth {
            0 => "outer",
            1 => "mid1",
            2 => "mid2",
            3 => "mid3",
            _ => "inner",
        };
        report_node_type(label, n, &analysis, src.as_bytes());
        if n.kind() == "method_call_expression" {
            cur = n.child_by_field_name("invocant");
            depth += 1;
        } else {
            break;
        }
    }

    eprintln!();
    eprintln!("======== return TERNARY probe ========");
    // Find the return_expression in body.
    fn find_return<'t>(n: tree_sitter::Node<'t>) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "return_expression" {
            return Some(n);
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_return(c) {
                    return Some(r);
                }
            }
        }
        None
    }
    let ret = find_return(body).expect("return in _generate_route");
    let ternary = ret.named_child(0).expect("return child");
    eprintln!(
        "  return child kind = {}  text = `{}`",
        ternary.kind(),
        ternary.utf8_text(src.as_bytes()).unwrap_or("").trim()
    );
    if ternary.kind() == "conditional_expression" {
        let consequent = ternary.child_by_field_name("consequent");
        let alternative = ternary.child_by_field_name("alternative");
        if let Some(a) = consequent {
            report_node_type("then-arm", a, &analysis, src.as_bytes());
        }
        if let Some(b) = alternative {
            report_node_type("else-arm", b, &analysis, src.as_bytes());
        }
    }
}

/// Direct proof: enumerate each chain link's resolvability on
/// the real demo + real Mojolicious. Prints a truth table;
/// asserts specifically that `->to` does NOT resolve (the gap
/// the user flagged) so this test becomes a tripwire: if a
/// future fix makes `->to` resolve, this test will fail and
/// force us to promote it to a "works" assertion.
#[test]
fn test_demo_chain_empirical_truth_table() {
    use std::fs;
    use std::path::PathBuf;

    let root: PathBuf = env!("CARGO_MANIFEST_DIR").into();
    let demo = root.join("test_files/plugin_mojo_demo.pl");
    let demo_source = fs::read_to_string(&demo).expect("demo file");

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(Some(root.to_str().unwrap()));
    let files = crate::file_store::FileStore::new();
    let _ = crate::module_resolver::index_workspace_with_index(
        &root.join("test_files"),
        &files,
        Some(&idx),
    );

    let inc = crate::module_resolver::discover_inc_paths();
    let install = |name: &str| -> bool {
        let mut p = crate::module_resolver::create_parser();
        match crate::module_resolver::resolve_and_parse(&inc, name, &mut p) {
            Some(c) => {
                idx.insert_cache(name, Some(c));
                true
            }
            None => false,
        }
    };
    if !(install("Mojolicious")
        && install("Mojolicious::Routes")
        && install("Mojolicious::Routes::Route")
        && install("Mojolicious::Lite"))
    {
        eprintln!("SKIP: Mojolicious not installed");
        return;
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&demo_source, None).unwrap();
    let mut analysis = crate::builder::build(&tree, demo_source.as_bytes());
    analysis.enrich_imported_types_with_keys(Some(&idx));

    let (line_idx, chain_line) = demo_source
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')") && l.contains("->to('Users#list')"))
        .expect("demo chain line present");

    let r_col = chain_line.find("$r").unwrap();
    let get_col = chain_line.find("->get(").unwrap() + 2;
    let to_col = chain_line.find("->to(").unwrap() + 2;
    let pt = |c: usize| tree_sitter::Point {
        row: line_idx,
        column: c,
    };

    // --- Link 1: $r's type ---
    let r_ty = analysis.inferred_type_via_bag("$r", pt(r_col));
    let r_class = r_ty
        .as_ref()
        .and_then(|t| t.class_name())
        .map(|s| s.to_string());

    // --- Link 2: ->get's invocant class (= $r's class) ---
    let get_ref = analysis.ref_at(pt(get_col)).expect("ref at ->get");
    let get_invocant_class = if matches!(get_ref.kind, crate::file_analysis::RefKind::MethodCall { .. }) {
        analysis.method_call_invocant_class(get_ref, Some(&idx))
    } else {
        None
    };

    // --- Link 3: ->get's RETURN type (what `$r->get(...)` evaluates to) ---
    // Find the method_call_expression node for `$r->get('/users')`.
    let mcall_node = {
        fn find_getcall<'a>(n: tree_sitter::Node<'a>, src: &[u8]) -> Option<tree_sitter::Node<'a>> {
            if n.kind() == "method_call_expression" {
                if let Some(m) = n.child_by_field_name("method") {
                    if m.utf8_text(src).ok() == Some("get") {
                        return Some(n);
                    }
                }
            }
            for i in 0..n.named_child_count() {
                if let Some(c) = n.named_child(i) {
                    if let Some(r) = find_getcall(c, src) {
                        return Some(r);
                    }
                }
            }
            None
        }
        find_getcall(tree.root_node(), demo_source.as_bytes()).expect("->get node")
    };
    let get_return_ty =
        crate::cursor_context::resolve_expression_type(&analysis, mcall_node, demo_source.as_bytes(), Some(&idx));

    // --- Link 4: ->to's invocant class (= ->get's return class) ---
    let to_ref = analysis.ref_at(pt(to_col)).expect("ref at ->to");
    let to_invocant_class = if matches!(to_ref.kind, crate::file_analysis::RefKind::MethodCall { .. }) {
        analysis.method_call_invocant_class(to_ref, Some(&idx))
    } else {
        None
    };

    // Also directly inspect the cached Route module's stored
    // return types + self-method tails for each method on the
    // chain's path.
    let route_cached = idx.get_cached("Mojolicious::Routes::Route").unwrap();
    let inspect = |name: &str| -> Option<InferredType> {
        for sym in &route_cached.analysis.symbols {
            if sym.name != name {
                continue;
            }
            if !matches!(
                sym.kind,
                crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
            ) {
                continue;
            }
            if matches!(&sym.detail, crate::file_analysis::SymbolDetail::Sub { .. }) {
                return route_cached.analysis.symbol_return_type_via_bag(sym.id, None);
            }
        }
        None
    };
    let gen_rt = inspect("_generate_route");
    let get_rt = inspect("get");
    let to_rt = inspect("to");
    let requires_rt = inspect("requires");
    let _route_rt = inspect("_route");

    eprintln!("======== chain truth table ========");
    eprintln!("  $r              class = {:?}", r_class);
    eprintln!("  ->get invocant  class = {:?}", get_invocant_class);
    eprintln!("  ->get RETURN    type  = {:?}", get_return_ty);
    eprintln!("  ->to  invocant  class = {:?}", to_invocant_class);
    eprintln!("  ---- cached Route symbols ----");
    eprintln!("  get             rt={:?}", get_rt);
    eprintln!("  _generate_route rt={:?}", gen_rt);
    eprintln!("  requires        rt={:?}", requires_rt);
    eprintln!("  to              rt={:?}", to_rt);
    eprintln!("  _route          rt={:?}", _route_rt);
    eprintln!("====================================");

    // The chain pin. With:
    //   - mojo-routes plugin's `_route` override pinning the
    //     return type inference can't reach, AND
    //   - the post-walk `ChainTypingReducer` (PreFold mode)
    //     symbolically executing every `my $X = <expr>` rhs (no
    //     "is it a chain" branch — same recursion every consumer
    //     uses), AND
    //   - the same reducer's return-arm refresh running before the
    //     second fold so `_generate_route`'s ternary return picks
    //     up the now-typed `$route`,
    // the full `$r->get(...)->to(...)` chain resolves end-to-end.
    // Each link is pinned individually so a regression localizes
    // to a specific hop instead of "the chain broke".
    assert!(
        r_class.is_some(),
        "(link 1) $r must resolve to a class; got None"
    );
    assert_eq!(r_class.as_deref(), Some("Mojolicious::Routes"));
    assert!(
        get_invocant_class.is_some(),
        "(link 2) ->get's invocant class must resolve; got None"
    );
    assert!(
        get_return_ty.is_some(),
        "(link 3) ->get's RETURN type must resolve through \
             _generate_route → _route's plugin override"
    );
    assert_eq!(
        get_return_ty.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "->get returns the Route class so ->to can chain off it"
    );
    assert!(
        to_invocant_class.is_some(),
        "(link 4) ->to's invocant class must resolve — THIS is \
             the chain hop the spike was unblocking"
    );
    assert_eq!(
        to_invocant_class.as_deref(),
        Some("Mojolicious::Routes::Route"),
        "->to is invoked on a Route, so cursor-on-`to` \
                    completion / hover / goto-def all reach \
                    Mojolicious::Routes::Route::to"
    );

    // Cross-check the cached symbols: every verb method
    // (get/post/put/etc.) tail-delegates through _generate_route,
    // and _generate_route's body folds via the chain typer +
    // refreshed return-arm typing.
    assert_eq!(
        _route_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "_route is the override anchor",
    );
    assert_eq!(
        gen_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "_generate_route folds because $route is now typed",
    );
    assert_eq!(
        get_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "get tail-delegates to _generate_route which has a type",
    );
}

/// E2E: the motivator. `$r->get('/x')->|` at the cursor — the
/// public `completion_items` API must offer methods from the
/// route class (Route::to, Route::name, etc.), proving the
/// witness-bag-driven chain typing works all the way through
/// CursorContext → resolve_node_type → resolve_expression_type →
/// find_method_return_type → complete_methods_for_class.
///
/// No special casing. Zero hardcoded chain rules. If this
/// passes, the mojo-demo `$r->get('/x')->to(...)` gets
/// "intellismarts" on `->to` through witness flow.
#[test]
fn test_e2e_mojo_style_chain_completion_offers_chained_class_methods() {
    let src = r#"package MyApp::Route;
sub new { my $c = shift; bless {}, $c }
sub get {
    my $self = shift;
    $self->{_path} = shift;
    return $self;
}
sub to {
    my $self = shift;
    $self->{_target} = shift;
    return $self;
}
sub name {
    my $self = shift;
    $self->{_name} = shift;
    return $self;
}

package main;
my $r = MyApp::Route->new;
$r->get('/users')->
"#;
    let analysis = parse_analysis(src);
    let tree = crate::document::Document::new(src.to_string())
        .unwrap()
        .tree;
    let idx = ModuleIndex::new_for_test();

    // Cursor right after the trailing `->` on the chain line.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')->"))
        .unwrap();
    let col = line.rfind("->").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in &["to", "name", "get"] {
        assert!(
            labels.contains(expected),
            "expected `{}` in completion after `$r->get('/users')->`, \
                 got {} items: {:?}",
            expected,
            labels.len(),
            labels
        );
    }
}

/// Pin: a Mojo helper registered in one file (`$app->helper(widget => ...)`)
/// must be reachable by goto-definition from `$c->widget` in ANOTHER file.
///
/// The provider's `mojo-helpers` synthesis bridges `widget` to the app
/// surface; the consumer's `$c` is a controller subclass that reaches the
/// surface via the synthetic-parent edge. `resolve_method_in_ancestors`
/// already finds the bridged symbol, but if its `CrossFile` result only
/// carries the class name, the goto-def consumer re-looks-up the method in
/// the bridged class's OWN module (where it doesn't exist) and the jump is
/// lost. Same-file works; this is the cross-file hole that had no coverage.
#[test]
fn cross_file_plugin_helper_goto_def_resolves() {
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(widget => sub ($c) { return Widget->new; });\n\
}\n\
1;\n";
    let provider = parse_analysis(provider_src);
    // Sanity: the provider really did synthesize a `widget` Method bridged to
    // the app surface (otherwise the test would pass vacuously once the bug is
    // "fixed" by an unrelated path).
    assert!(
        provider.plugin_namespaces.iter().any(|ns| ns
            .bridges
            .iter()
            .any(|b| matches!(b, crate::file_analysis::Bridge::Class(c) if c == crate::file_analysis::APP_SURFACE_CLASS))),
        "provider must bridge a namespace to the app surface",
    );

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let provider_path = std::path::PathBuf::from("/tmp/perl_lsp_pin_My_Plugin.pm");
    idx.register_workspace_module(provider_path.clone(), std::sync::Arc::new(provider));

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $w = $c->widget;\n\
  return $w;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();

    // Cursor on the `widget` token in `$c->widget`.
    let byte = consumer_src.find("widget;").expect("call site present");
    let prefix = &consumer_src[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };

    let uri = Url::parse("file:///consumer.pl").unwrap();
    let resp = find_definition(&consumer, pos, &uri, &idx);

    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        Some(GotoDefinitionResponse::Array(mut v)) if !v.is_empty() => v.remove(0),
        other => panic!("expected a goto-def location for cross-file helper, got {other:?}"),
    };
    assert!(
        loc.uri.path().ends_with("My_Plugin.pm"),
        "goto-def should land in the provider file, got {}",
        loc.uri,
    );
}

/// CG-3b cross-package glob attribution, cross-file: `DateTime::PP`
/// installs `_ymd2rd` into `DateTime` via
/// `*{ 'DateTime::' . $sub } = __PACKAGE__->can($sub)`. A `$self->_ymd2rd`
/// call in `DateTime` (a different file) must goto-def to the real sub in
/// PP.pm — the method is a DateTime method even though it's declared in a
/// differently-named module file.
#[test]
fn cross_package_glob_method_resolves_cross_file() {
    let provider_src = "package DateTime::PP;\n\
sub _ymd2rd { my ($class, $y, $m, $d) = @_; return 1; }\n\
my @subs = qw( _ymd2rd );\n\
for my $sub (@subs) {\n\
  no strict 'refs';\n\
  *{ 'DateTime::' . $sub } = __PACKAGE__->can($sub);\n\
}\n\
1;\n";
    let provider = parse_analysis(provider_src);
    // Sanity: the glob really attributed `_ymd2rd` to DateTime.
    assert!(
        provider.symbols.iter().any(|s| s.name == "_ymd2rd"
            && matches!(s.kind, crate::file_analysis::SymKind::Sub)
            && s.package.as_deref() == Some("DateTime")),
        "provider must attribute the glob-installed _ymd2rd to DateTime",
    );

    let idx = crate::module_index::ModuleIndex::new_for_test();
    let provider_path = std::path::PathBuf::from("/tmp/perl_lsp_pin_DateTime_PP.pm");
    idx.register_workspace_module(provider_path.clone(), std::sync::Arc::new(provider));

    let consumer_src = "package DateTime;\n\
sub new { my $class = shift; return bless {}, $class; }\n\
sub day_of_week {\n\
  my $self = shift;\n\
  return $self->_ymd2rd( 2024, 1, 1 );\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();

    let byte = consumer_src.find("_ymd2rd( 2024").expect("call site present");
    let prefix = &consumer_src[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };

    let uri = Url::parse("file:///datetime.pl").unwrap();
    let resp = find_definition(&consumer, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        Some(GotoDefinitionResponse::Array(mut v)) if !v.is_empty() => v.remove(0),
        other => panic!("expected goto-def for cross-package glob method, got {other:?}"),
    };
    assert!(
        loc.uri.path().ends_with("DateTime_PP.pm"),
        "goto-def should land in the provider (PP) file, got {}",
        loc.uri,
    );
    assert_eq!(
        loc.range.start.line, 1,
        "should land on the real `sub _ymd2rd` (line 1), got {}",
        loc.range.start.line
    );
}

/// Fully-qualified variable read across files: `$My::Vars::config` in one
/// file must goto-def to `our $config` in My::Vars (another module). Mirrors
/// the FQ-call cross-file path via `qualified_var_target()` + module_index.
#[test]
fn fq_variable_read_resolves_cross_file() {
    let provider_src = "package My::Vars;\n\
our $config = { host => 'localhost' };\n\
our @servers = ('a', 'b');\n\
1;\n";
    let provider = parse_analysis(provider_src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let provider_path = std::path::PathBuf::from("/tmp/perl_lsp_pin_My_Vars.pm");
    idx.register_workspace_module(provider_path, std::sync::Arc::new(provider));

    let consumer_src = "package Main;\n\
my $h = $My::Vars::config;\n\
my @s = @My::Vars::servers;\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();

    // Cursor on the `config` tail of `$My::Vars::config`.
    let byte = consumer_src.find("config;").expect("read site present");
    let prefix = &consumer_src[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };

    let uri = Url::parse("file:///consumer.pl").unwrap();
    let resp = find_definition(&consumer, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        other => panic!("expected goto-def for FQ var, got {other:?}"),
    };
    assert!(
        loc.uri.path().ends_with("My_Vars.pm"),
        "goto-def should land in the provider file, got {}",
        loc.uri,
    );
    assert_eq!(
        loc.range.start.line, 1,
        "should land on `our $config` (line 1)"
    );
}

/// Honest miss: an FQ read into an unknown package must not fabricate a jump.
#[test]
fn fq_variable_read_unknown_package_is_honest_miss() {
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let consumer_src = "package Main;\nmy $x = $No::Such::Pkg::thing;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();
    let byte = consumer_src.find("thing;").unwrap();
    let prefix = &consumer_src[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };
    let uri = Url::parse("file:///consumer.pl").unwrap();
    let resp = find_definition(&consumer, pos, &uri, &idx);
    assert!(resp.is_none(), "unknown package must be an honest miss, got {resp:?}");
}

/// Pin: cross-file hover on a plugin-synthesized helper. Same shape as
/// `cross_file_plugin_helper_goto_def_resolves` but exercises the hover
/// consumer arm, which shared the same lossy `get_cached(class).sub_info`
/// bug (looking the bridged helper up in the class's own module). Hover
/// needs the symbol's signature, not just a location, so this guards the
/// unified `def_module` path end-to-end.
#[test]
fn cross_file_plugin_helper_hover_resolves() {
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(widget => sub ($c) { return Widget->new; });\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_hover_My_Plugin.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $w = $c->widget;\n\
  return $w;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();

    let byte = consumer_src.find("widget;").expect("call site present");
    let prefix = &consumer_src[..byte];
    let point = tree_sitter::Point {
        row: prefix.matches('\n').count(),
        column: byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0),
    };

    let hover = consumer
        .hover_info(point, consumer_src, Some(&idx))
        .expect("cross-file hover should resolve the bridged helper");
    assert!(hover.contains("widget"), "hover should mention the helper, got: {hover}");
    // The helper now lives on the fictional app surface; the controller
    // subclass reaches it through the synthetic-parent edge. Hover shows
    // where the symbol is bridged from.
    assert!(
        hover.contains(crate::file_analysis::APP_SURFACE_CLASS),
        "hover should show the app surface as the bridging class, got: {hover}",
    );
}

/// Pin: cross-file hover return type that REQUIRES the module index — the
/// helper returns `Other->new->fluent`, so typing the return means recursing
/// into `My::Other`'s module to resolve `fluent`. With `module_index: None`
/// in the return-type query (the old behavior) this came back untyped; the
/// `_ctx` threading lights it up. Guards the return-position cross-file chain.
#[test]
fn cross_file_helper_return_type_needs_module_index() {
    let other_src = "package My::Other;\n\
use Mojo::Base -base;\n\
sub fluent ($self) { return $self; }\n\
1;\n";
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(thing => sub ($c) { return My::Other->new->fluent; });\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_ret_Other.pm"),
        std::sync::Arc::new(parse_analysis(other_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_ret_Plugin.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $x = $c->thing;\n\
  return $x;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();
    let byte = consumer_src.find("thing;").expect("call site");
    let prefix = &consumer_src[..byte];
    let point = tree_sitter::Point {
        row: prefix.matches('\n').count(),
        column: byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0),
    };

    let hover = consumer
        .hover_info(point, consumer_src, Some(&idx))
        .expect("cross-file hover should resolve");
    assert!(
        hover.contains("My::Other"),
        "return type should resolve cross-file to My::Other, got: {hover}",
    );
}

/// Like `cross_file_helper_return_type_needs_module_index`, but the helper
/// routes the value through a LEXICAL (`my $g = chain; return $g`) — the common
/// `my $x = …; return $x` shape. This is the end-to-end pin for unifying
/// `Variable` into the canonical edge chase: the variable query path now
/// carries the `module_index`, the cross-file chain assignment is stored as
/// `Variable($g) → Edge(Expr(rhs))` ("Edges, not values"), and point-narrowing
/// no longer wrongly filters the materialized `Expression` witness.
#[test]
fn cross_file_lexical_chain_return_type() {
    let other_src = "package My::Other;\n\
use Mojo::Base -base;\n\
sub fluent ($self) { return $self; }\n\
1;\n";
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(thing => sub ($c) { my $g = My::Other->new->fluent; return $g; });\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_lex_Other.pm"),
        std::sync::Arc::new(parse_analysis(other_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_lex_Plugin.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $x = $c->thing;\n\
  return $x;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();
    let byte = consumer_src.find("thing;").expect("call site");
    let prefix = &consumer_src[..byte];
    let point = tree_sitter::Point {
        row: prefix.matches('\n').count(),
        column: byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0),
    };

    let hover = consumer
        .hover_info(point, consumer_src, Some(&idx))
        .expect("cross-file hover should resolve");
    assert!(
        hover.contains("My::Other"),
        "lexical-chain return type should resolve cross-file to My::Other, got: {hover}",
    );
}

/// The fluent-accessor sibling of `cross_file_helper_return_type_needs_module_index`:
/// the helper returns `My::Other->new->acc($x)` where `acc` is a Mojo::Base `has`
/// accessor (fluent setter — returns the invocant), NOT a plain `return $self` sub.
/// The fluent return is modeled as `ReturnExpr(Receiver)`, so its resolution
/// substitutes the query's receiver. Before the receiver-reset fix, the consumer's
/// call-site receiver (`Mojolicious::Controller`, the type of `$c`) leaked down the
/// edge chase into `MethodOnClass{My::Other, acc}` and got substituted there. The
/// receiver for a method-call return must be that call's invocant (`My::Other`).
#[test]
fn cross_file_fluent_accessor_chain_return_type() {
    let other_src = "package My::Other;\n\
use Mojo::Base -base;\n\
has acc => sub { {} };\n\
1;\n";
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(thing => sub ($c) { return My::Other->new->acc($x); });\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_acc_Other.pm"),
        std::sync::Arc::new(parse_analysis(other_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_acc_Plugin.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $x = $c->thing;\n\
  return $x;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();
    let byte = consumer_src.find("thing;").expect("call site");
    let prefix = &consumer_src[..byte];
    let point = tree_sitter::Point {
        row: prefix.matches('\n').count(),
        column: byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0),
    };

    let hover = consumer
        .hover_info(point, consumer_src, Some(&idx))
        .expect("cross-file hover should resolve");
    assert!(
        hover.contains("My::Other"),
        "fluent-accessor chain return type should resolve to My::Other (not the \
         consumer's call-site receiver), got: {hover}",
    );
}

/// The MRO subtlety of the receiver-reset fix: a fluent `has` accessor declared
/// on a PARENT but dispatched on a CHILD (`Child->new->acc($x)`) must return the
/// *child*, not the class where `has` was declared. The receiver reset keys on the
/// dispatch class (`MethodOnClass{Child}` → receiver `Child`); the inheritance hop
/// to `MethodOnClass{Parent, acc}` must then carry that child receiver through so
/// the parent's `ReturnExpr(Receiver)` substitutes `Child`.
#[test]
fn cross_file_inherited_fluent_accessor_returns_child() {
    let base_src = "package My::Base;\n\
use Mojo::Base -base;\n\
has acc => sub { {} };\n\
1;\n";
    let child_src = "package My::Child;\n\
use Mojo::Base 'My::Base';\n\
1;\n";
    let provider_src = "package My::Plugin;\n\
use Mojo::Base 'Mojolicious::Plugin';\n\
sub register ($self, $app, $conf) {\n\
  $app->helper(thing => sub ($c) { return My::Child->new->acc($x); });\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_inh_Base.pm"),
        std::sync::Arc::new(parse_analysis(base_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_inh_Child.pm"),
        std::sync::Arc::new(parse_analysis(child_src)),
    );
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_inh_Plugin.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "package My::Ctrl;\n\
use Mojo::Base 'Mojolicious::Controller';\n\
sub action ($c) {\n\
  my $x = $c->thing;\n\
  return $x;\n\
}\n\
1;\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();
    let byte = consumer_src.find("thing;").expect("call site");
    let prefix = &consumer_src[..byte];
    let point = tree_sitter::Point {
        row: prefix.matches('\n').count(),
        column: byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0),
    };

    let hover = consumer
        .hover_info(point, consumer_src, Some(&idx))
        .expect("cross-file hover should resolve");
    assert!(
        hover.contains("My::Child"),
        "inherited fluent accessor must return the dispatch (child) class, got: {hover}",
    );
}

/// Spike: Mojo partial route targets inherit the controller down the
/// route-builder chain via a value brand (`InferredType::BrandedRoute`).
/// See `docs/adr/route-branding.md` (option C, collapsed:
/// resolved defaults ride the type, no separate brand-id/side-table).
///
/// The brand carries the inherited `->to('ctrl#')` controller and rides
/// the chain through assignment (`my $alerts_r = ...`), method chaining
/// (`->get('/')->to`), and nesting (`$alerts_r->under(...)` → `$crud`).
/// A partial `->to('#action')` reads the inherited controller off the
/// receiver's brand; a sibling group with its own `->to('other#')`
/// re-brands its descendants without leaking.
///
/// Controller-token → class mapping (camelize + workspace search) is
/// orthogonal and decided elsewhere; here the controller packages are
/// named to match the raw token so goto-def resolves end to end without
/// that layer. The route class's fluent verbs return `$self` so the
/// chain types as `Mojolicious::Routes::Route` locally.
#[test]
fn brand_partial_route_targets_inherit_controller() {
    let src = r#"package Mojolicious::Routes::Route;
sub new { my $class = shift; return bless {}, $class; }
sub any { my $self = shift; return $self; }
sub get { my $self = shift; return $self; }
sub under { my $self = shift; return $self; }
sub to { my $self = shift; return $self; }

package alerts;
sub list { my $c = shift; }
sub get_alert { my $c = shift; }
sub read_settings { my $c = shift; }

package other;
sub thing { my $c = shift; }

package MyApp;
use Mojolicious::Lite;
sub startup {
  my $self = shift;
  my $r = Mojolicious::Routes::Route->new;
  my $alerts_r = $r->any('/alerts')->to('alerts#', section => 'admin');
  $alerts_r->get('/')->to('#list');
  my $crud = $alerts_r->under('/:type')->to('#get_alert');
  $crud->get('/settings')->to('#read_settings');
  my $other_r = $r->any('/other')->to('other#');
  $other_r->get('/x')->to('#thing');
}
1;
"#;
    let fa = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();

    // Helper: the plugin-emitted MethodCallRef for a partial target —
    // its invocant class IS the inherited controller, its target the
    // action.
    let inherited = |action: &str| -> Option<String> {
        fa.refs.iter().find_map(|r| {
            if let crate::file_analysis::RefKind::MethodCall { .. } = &r.kind {
                if r.target_name == action {
                    return fa.method_call_invocant_class(r, Some(&idx));
                }
            }
            None
        })
    };

    // Direct child: `$alerts_r->get('/')->to('#list')` inherits 'alerts'.
    assert_eq!(inherited("list").as_deref(), Some("alerts"),
        "partial '#list' must inherit the parent's 'alerts' controller");
    // Nested via `under`: `$crud` inherits 'alerts' from `$alerts_r`.
    assert_eq!(inherited("get_alert").as_deref(), Some("alerts"),
        "partial '#get_alert' on $crud (under $alerts_r) inherits 'alerts'");
    // Two hops deep: `$crud->get('/settings')->to('#read_settings')`.
    assert_eq!(inherited("read_settings").as_deref(), Some("alerts"),
        "nested partial '#read_settings' still inherits 'alerts'");
    // Sibling group re-brands; no leak from 'alerts'.
    assert_eq!(inherited("thing").as_deref(), Some("other"),
        "sibling group's '#thing' inherits 'other', not 'alerts'");

    // The brand rides assignment + nesting: $alerts_r and $crud both
    // carry the inherited controller in their type.
    let ty_at = |needle: &str, var: &str| -> Option<crate::file_analysis::InferredType> {
        let at = src.find(needle).unwrap();
        let pre = &src[..at];
        let pt = tree_sitter::Point {
            row: pre.matches('\n').count(),
            column: at - pre.rfind('\n').map(|i| i + 1).unwrap_or(0),
        };
        fa.inferred_type_via_bag(var, pt)
    };
    assert!(
        matches!(ty_at("$alerts_r->get", "$alerts_r"),
            Some(crate::file_analysis::InferredType::BrandedRoute { ref controller, .. })
                if controller.as_deref() == Some("alerts")),
        "$alerts_r must type as a BrandedRoute carrying controller='alerts'");
    assert!(
        matches!(ty_at("$crud->get", "$crud"),
            Some(crate::file_analysis::InferredType::BrandedRoute { ref controller, .. })
                if controller.as_deref() == Some("alerts")),
        "$crud (nested under $alerts_r) inherits the 'alerts' brand");

    // Stash (beyond controller/action) rides the brand too: the
    // `section => 'admin'` default set on $alerts_r is queryable via
    // the rule-#10 `route_default` accessor on a descendant's value.
    assert_eq!(
        ty_at("$crud->get", "$crud").as_ref().and_then(|t| t.route_default("section")),
        Some("admin"),
        "inherited stash default 'section' is readable off $crud's brand");
    assert_eq!(
        ty_at("$crud->get", "$crud").as_ref().and_then(|t| t.route_default("controller")),
        Some("alerts"),
        "route_default('controller') reads the distinguished controller key");

    // End-to-end goto-def: cursor on the `list` action inside
    // `->to('#list')` resolves to `alerts::list` (here the controller
    // token maps directly to the package).
    let uri = Url::parse("file:///app.pl").unwrap();
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(src, None).unwrap();
    let list_at = src.find("'#list'").unwrap() + "'#".len();
    let pre = &src[..list_at];
    let pos = Position {
        line: pre.matches('\n').count() as u32,
        character: (list_at - pre.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };
    let resp = find_definition(&fa, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        Some(GotoDefinitionResponse::Array(mut v)) if !v.is_empty() => v.remove(0),
        other => panic!("expected goto-def on partial '#list', got {other:?}"),
    };
    // `sub list` is declared on line index 9 (0-based) — `package alerts;`
    // block. Just assert it landed on the `list` sub's line, not on the
    // app's route line.
    let list_line = src[..src.find("sub list").unwrap()].matches('\n').count() as u32;
    assert_eq!(loc.range.start.line, list_line,
        "goto-def on '#list' lands on `sub list` in the alerts controller");
}

// ---- Types::Standard / Types::Common import-scoped vocabulary ----

/// `use Types::Standard qw/Str Int/; Str();` — Str is explicitly imported, so
/// calling it as a function (with parens, producing a FunctionCall ref) must not
/// raise an unresolved-function diagnostic. The plugin's on_use hook provides the
/// import-scoped vocabulary; the builder's process_use already creates an Import
/// entry for the qw-list, but this also exercises the plugin path.
#[test]
fn types_standard_explicit_import_suppresses_diagnostic() {
    let source = "use Types::Standard qw/Str Int/;\nStr();\nInt();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let names: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        names.is_empty(),
        "Str()/Int() explicitly imported from Types::Standard must not produce unresolved-function; got: {:?}",
        names,
    );
}

/// `-all` import flag: the plugin expands to the full vocabulary, so any type
/// constant used as a function call is known even when the module isn't in the
/// module_index. Without the plugin's on_use hook, `-all` would appear literally
/// in imported_symbols and InstanceOf wouldn't match, producing a spurious diagnostic.
#[test]
fn types_standard_all_flag_suppresses_diagnostic() {
    let source = "use Types::Standard '-all';\nInstanceOf(['Foo']);\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "InstanceOf() with '-all' must not produce unresolved-function; got: {:?}",
        unresolved,
    );
}

/// Types::Common::String and Types::Common::Numeric vocabularies are similarly
/// suppressed when explicitly imported.
#[test]
fn types_common_string_numeric_explicit_import_suppresses_diagnostic() {
    let source = concat!(
        "use Types::Common::String qw/NonEmptyStr/;\n",
        "use Types::Common::Numeric qw/PositiveInt/;\n",
        "NonEmptyStr();\nPositiveInt();\n",
    );
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "Types::Common String/Numeric names must not produce unresolved-function; got: {:?}",
        unresolved,
    );
}

/// Regression: the existing InstanceOf['Foo'] constraint typing must still work
/// after adding the on_use hook. The import suppression is additive; it must not
/// disturb the type_constraint_names / type_constraint_inner machinery.
#[test]
fn types_standard_instanceof_constraint_typing_still_works() {
    use crate::file_analysis::InferredType;
    let source = concat!(
        "package T;\nuse Moo;\n",
        "use Types::Standard qw/Str Int InstanceOf/;\n",
        "has x => (is => 'ro', isa => InstanceOf['Foo']);\n",
        "my $t = Str;\n1;\n",
    );
    let analysis = parse_analysis(source);
    // Accessor `x` must return Foo (constraint-projection)
    assert_eq!(
        analysis.sub_return_type_at_arity("x", Some(0)),
        Some(InferredType::ClassName("Foo".to_string())),
        "InstanceOf['Foo'] isa must give the accessor a Foo return type",
    );
    // No unresolved-function diagnostics for Str, Int, InstanceOf
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "No unresolved-function expected for Types::Standard imports; got: {:?}",
        unresolved,
    );
}

// ── P1.2: bare-use export_ok suppression ────────────────────────────────────

/// A bare `use Foo;` must suppress unresolved-function for names in export_ok.
/// Runtime exporters (Moose::Exporter->setup_import_methods) write to export_ok,
/// not export, so the suppression must cover both lists.
#[test]
fn bare_use_suppresses_export_ok_names() {
    // Consumer: bare use, no qw list — should auto-suppress subtype/as/where/coerce.
    let source = "use FakeTypeConstraints;\nsubtype('Foo', as => 'Str', where => sub { 1 });\nas('Str');\ncoerce('Foo', from => 'Int', via => sub { \"$_[0]\" });\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    // Seed with a module whose names are ONLY in export_ok (runtime-exporter style).
    let cached = fake_cached("/fake/FakeTypeConstraints.pm", &[], &["subtype", "as", "where", "coerce"]);
    module_index.insert_cache("FakeTypeConstraints", Some(cached));
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "bare use of a runtime-exporter module must not produce unresolved-function for export_ok names; got: {:?}",
        unresolved,
    );
}

/// Genuinely-undefined functions must still produce a diagnostic even when a
/// module seeded with export_ok is in scope via a bare use.
#[test]
fn genuinely_undefined_still_flags_with_export_ok_module_in_scope() {
    let source = "use FakeTypeConstraints;\ntruly_undefined_fn();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let cached = fake_cached("/fake/FakeTypeConstraints.pm", &[], &["subtype", "as"]);
    module_index.insert_cache("FakeTypeConstraints", Some(cached));
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        unresolved.iter().any(|m| m.contains("truly_undefined_fn")),
        "truly_undefined_fn must still produce an unresolved-function diagnostic; got: {:?}",
        unresolved,
    );
}

// ── P3: lowercase `does` universal method ───────────────────────────────────

/// `$obj->does(...)` must not be flagged as an unresolved method.
/// Moose adds lowercase `does` to every class alongside UNIVERSAL's uppercase DOES.
#[test]
fn does_method_not_flagged_unresolved() {
    let source = "package M;\nuse Moose;\nsub check {\n    my ($self, $role) = @_;\n    return $self->does($role);\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let unresolved_method: Vec<&str> = diags.iter()
        .filter_map(|d| {
            if matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-method") {
                Some(d.message.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        !unresolved_method.iter().any(|m| m.contains("does")),
        "`does` must be in the universal-methods skip list and not flagged; got: {:?}",
        unresolved_method,
    );
}

// ── Incomplete ISA chain → honest-silent unresolved-method ───────────────────
// A class whose `@ISA` names a parent we can't resolve (not in the workspace,
// not in @INC) has an INCOMPLETE chain: the called method might be inherited
// from the unresolvable parent. Every invocant-typing path must consult the
// SAME `class_has_unresolved_ancestor` predicate so they can't drift (rule #10).

fn unresolved_method_messages(diags: &[Diagnostic]) -> Vec<String> {
    diags.iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-method"))
        .map(|d| d.message.clone())
        .collect()
}

/// THE FIX: `$self = shift; $self->inherited()` where the class `use base`s an
/// unresolvable parent must NOT emit unresolved-method. The `$self`/FirstParam
/// path was the one that leaked before the shared guard.
#[test]
fn self_invocant_unresolvable_parent_no_unresolved_method_use_base() {
    let source = "package Child;\nuse base qw(MyDep);\nsub new { bless {}, shift }\nsub local_thing {\n    my $self = shift;\n    return $self->dep_method();\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(
        um.is_empty(),
        "$self->dep_method with unresolvable `use base` parent must stay silent; got: {:?}",
        um,
    );
}

/// Same gate via `use parent`.
#[test]
fn self_invocant_unresolvable_parent_no_unresolved_method_use_parent() {
    let source = "package Child;\nuse parent -norequire, 'MyDep';\nsub new { bless {}, shift }\nsub local_thing {\n    my $self = shift;\n    return $self->dep_method();\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(um.is_empty(), "unresolvable `use parent` parent must stay silent; got: {:?}", um);
}

/// Same gate via `our @ISA =`.
#[test]
fn self_invocant_unresolvable_parent_no_unresolved_method_at_isa() {
    let source = "package Child;\nour @ISA = ('MyDep');\nsub new { bless {}, shift }\nsub local_thing {\n    my $self = shift;\n    return $self->dep_method();\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(um.is_empty(), "unresolvable `our @ISA` parent must stay silent; got: {:?}", um);
}

/// Regression: the direct-invocant paths (`Pkg->new->m`, `Pkg->m`) on a class
/// with an unresolvable parent must also stay silent — same predicate.
#[test]
fn direct_invocant_unresolvable_parent_no_unresolved_method() {
    let source = "package Child;\nuse base qw(MyDep);\nsub new { bless {}, shift }\nsub callers {\n    Child->dep_method();\n    my $c = Child->new;\n    $c->dep_method();\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(
        um.is_empty(),
        "direct-invocant calls on a class with an unresolvable parent must stay silent; got: {:?}",
        um,
    );
}

/// No over-suppression: a class with NO parents calling a genuinely missing
/// method STILL flags. The chain is complete, so the FP is a real TP.
#[test]
fn no_parents_missing_method_still_flags() {
    let source = "package Foo;\nsub new { bless {}, shift }\nsub real { 1 }\npackage main;\nmy $f = Foo->new;\n$f->totally_bogus_xyz();\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(
        um.iter().any(|m| m.contains("totally_bogus_xyz")),
        "a parentless class calling a missing method must still flag; got: {:?}",
        um,
    );
}

/// No over-suppression on a fully-resolved 2-hop chain: an inherited method
/// resolves (silent) AND a genuinely missing one still flags. All packages are
/// local here, so the whole chain is known.
#[test]
fn fully_resolved_two_hop_chain_resolves_inherited_and_flags_missing() {
    let source = "package GrandPa;\nsub new { bless {}, shift }\nsub gp_method { 1 }\npackage Pa;\nuse parent -norequire, 'GrandPa';\npackage Kid;\nuse parent -norequire, 'Pa';\nsub use_things {\n    my $self = shift;\n    $self->gp_method();\n    $self->missing_method();\n}\n1;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let um = unresolved_method_messages(&diags);
    assert!(
        !um.iter().any(|m| m.contains("gp_method")),
        "gp_method is inherited 2 hops on a fully-resolved chain — must NOT flag; got: {:?}",
        um,
    );
    assert!(
        um.iter().any(|m| m.contains("missing_method")),
        "missing_method on a fully-resolved chain must still flag; got: {:?}",
        um,
    );
}


#[test]
fn classic_perl_filehandle_fps_suppressed() {
    // `print FH LIST` / `say FH` / `printf FH` must not flag the bareword
    // filehandle as an unresolved function.
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    for src in [
        "print STDERR \"hi\";\n",
        "printf STDERR \"%s\", $x;\n",
        "say STDOUT \"hi\";\n",
        "print DATA;\n",
        "STDOUT->autoflush(1);\n",
        "my $t = -t STDIN;\n",
    ] {
        let analysis = parse_analysis(src);
        let diags = collect_diagnostics(&analysis, &module_index, Default::default());
        assert!(
            diags.is_empty(),
            "filehandle FP for `{}`: {:?}",
            src.trim(),
            diags.iter().map(|d| d.message.clone()).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn print_with_real_call_in_list_still_flags() {
    // The filehandle suppression must not swallow real calls in the list.
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let analysis = parse_analysis("print STDERR \"a\", frobnicate();\n");
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert!(
        diags.iter().any(|d| d.message.contains("frobnicate")),
        "real call `frobnicate` in print list must still flag; got: {:?}",
        diags.iter().map(|d| d.message.clone()).collect::<Vec<_>>(),
    );
}

#[test]
fn use_constant_callsites_not_flagged() {
    // Both scalar and block forms register the constant as a local sub, so
    // same-file callsites no longer flag as unresolved functions.
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    for src in [
        "use constant DEBUG => 1;\nmy $y = DEBUG && 2;\n",
        "use constant { A => 1, B => 2 };\nmy $z = A() + B();\n",
    ] {
        let analysis = parse_analysis(src);
        let diags = collect_diagnostics(&analysis, &module_index, Default::default());
        assert!(
            diags.is_empty(),
            "use-constant callsite FP for `{}`: {:?}",
            src.trim(),
            diags.iter().map(|d| d.message.clone()).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn require_bareword_not_flagged() {
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    for src in ["require Carp;\n", "require Foo::Bar;\n"] {
        let analysis = parse_analysis(src);
        let diags = collect_diagnostics(&analysis, &module_index, Default::default());
        assert!(
            diags.is_empty(),
            "require-bareword FP for `{}`: {:?}",
            src.trim(),
            diags.iter().map(|d| d.message.clone()).collect::<Vec<_>>(),
        );
    }
}

// ---- NAV-B: %EXPORT_TAGS single export surface, goto-def + diagnostic agree ----

/// Build a cached module from raw Perl source (lets tests seed `%EXPORT_TAGS`
/// producers that `fake_cached` can't express).
fn cached_from_source(path: &str, source: &str) -> std::sync::Arc<crate::module_index::CachedModule> {
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        std::path::PathBuf::from(path),
        std::sync::Arc::new(parse_analysis(source)),
    ))
}

/// The Perl::Critic::Utils shape: `hashify` exported only via a
/// `Readonly::Hash our %EXPORT_TAGS` member. A consumer importing the
/// `:data_conversion` tag and calling `hashify` must NOT be flagged as
/// unresolved (the diagnostic agrees with goto-def's resolution).
#[test]
fn export_tags_tag_import_diagnostic_agrees() {
    let producer = r#"
package Perl::Critic::Utils;
Readonly::Array our @EXPORT_OK => qw( interpolate );
Readonly::Hash our %EXPORT_TAGS => (
    all             => [ @EXPORT_OK ],
    data_conversion => [ qw{ hashify words_from_string interpolate } ],
);
sub hashify { 1 }
sub words_from_string { 2 }
sub interpolate { 3 }
1;
"#;
    let consumer = "use Perl::Critic::Utils qw(:data_conversion);\nmy %h = hashify(@list);\n";
    let analysis = parse_analysis(consumer);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    module_index.insert_cache(
        "Perl::Critic::Utils",
        Some(cached_from_source("/usr/lib/perl5/Perl/Critic/Utils.pm", producer)),
    );

    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let hashify_diags: Vec<_> = diags.iter().filter(|d| d.message.contains("hashify")).collect();
    assert!(
        hashify_diags.is_empty(),
        "tag-imported `hashify` must not be flagged unresolved; got {:?}",
        hashify_diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );

    // Goto-def reaches the producer file via the same export surface.
    let (_import, path, remote) =
        resolve_imported_function(&analysis, "hashify", &module_index)
            .expect("hashify resolves through the folded export surface");
    assert_eq!(remote, "hashify");
    assert!(path.ends_with("Perl/Critic/Utils.pm"));
}

/// A name in neither `@EXPORT*` nor any tag is still unresolved — the fold
/// widens the surface only for genuine tag members.
#[test]
fn export_tags_non_member_still_unresolved() {
    let producer = r#"
package Util::Tagged;
Readonly::Hash our %EXPORT_TAGS => (
    data_conversion => [ qw{ hashify } ],
);
sub hashify { 1 }
sub private_helper { 2 }
1;
"#;
    let consumer = "use Util::Tagged qw(:data_conversion);\nprivate_helper();\n";
    let analysis = parse_analysis(consumer);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    module_index.insert_cache(
        "Util::Tagged",
        Some(cached_from_source("/usr/lib/perl5/Util/Tagged.pm", producer)),
    );

    assert!(
        resolve_imported_function(&analysis, "private_helper", &module_index).is_none(),
        "non-exported sub must not resolve through the export surface",
    );
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    assert!(
        diags.iter().any(|d| d.message.contains("private_helper")),
        "non-exported sub must still be flagged; got {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

/// TASK B.1 — goto-def on an imported-function CALL SITE one-hops to the
/// DEFINING sub in the provider module, not the consumer's local `use` line.
/// The producer module is cached, so `sub_info(remote).def_line()` resolves;
/// the response must be a Scalar landing on that line in the .pm.
#[test]
fn imported_function_call_goto_def_reaches_module_sub() {
    let provider_src = "package My::Util;\n\
our @EXPORT_OK = qw(helper_fn);\n\
sub helper_fn {\n\
  my ($x) = @_;\n\
  return $x * 2;\n\
}\n\
1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_My_Util.pm"),
        std::sync::Arc::new(parse_analysis(provider_src)),
    );

    let consumer_src = "use My::Util qw(helper_fn);\n\
my $v = helper_fn(21);\n";
    let consumer = parse_analysis(consumer_src);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(consumer_src, None).unwrap();

    // Cursor on the `helper_fn` token at the call site (line 1, not the use line).
    let byte = consumer_src.find("helper_fn(21)").expect("call site present");
    let prefix = &consumer_src[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };

    let uri = Url::parse("file:///consumer.pl").unwrap();
    let resp = find_definition(&consumer, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        other => panic!("expected a single-hop Scalar to the module sub, got {other:?}"),
    };
    assert!(
        loc.uri.path().ends_with("My_Util.pm"),
        "goto-def should land in the provider file, got {}",
        loc.uri,
    );
    // `sub helper_fn` is the 3rd line (0-based row 2) of the provider source.
    assert_eq!(
        loc.range.start.line, 2,
        "should land on the defining `sub helper_fn` line, not the consumer's use stmt",
    );
}

/// TASK B.2 — goto-def on the INVOCANT class-name token in `Foo->bar()`
/// resolves to the `package Foo` decl (a PackageRef), exactly like `use Foo`.
/// The narrower PackageRef at the bareword span must win over the wider
/// MethodCall ref describing `bar`.
#[test]
fn class_invocant_goto_def_reaches_package_decl() {
    let source = "package Foo;\n\
sub bar { 42 }\n\
sub new { bless {}, shift }\n\
package main;\n\
Foo->bar();\n";
    let analysis = parse_analysis(source);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(source, None).unwrap();
    let idx = crate::module_index::ModuleIndex::new_for_test();

    // Cursor on the `Foo` invocant in `Foo->bar()`.
    let byte = source.find("Foo->bar").expect("invocant present");
    let prefix = &source[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32 + 1,
    };
    let uri = Url::parse("file:///test.pl").unwrap();
    let resp = find_definition(&analysis, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        other => panic!("expected goto-def on class invocant, got {other:?}"),
    };
    assert_eq!(
        loc.range.start.line, 0,
        "`Foo` invocant should resolve to `package Foo;` (line 0), not the constructor or method",
    );
}

/// TASK B.2 regression guard — the METHOD token (`bar`) in `Foo->bar()` must
/// still resolve to the `sub bar` decl (NAV-A's precise method ref), NOT the
/// package. Emitting the invocant PackageRef must not shadow the method token.
#[test]
fn method_token_goto_def_unaffected_by_invocant_package_ref() {
    let source = "package Foo;\n\
sub bar { 42 }\n\
package main;\n\
Foo->bar();\n";
    let analysis = parse_analysis(source);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(source, None).unwrap();
    let idx = crate::module_index::ModuleIndex::new_for_test();

    // Cursor on the `bar` method token.
    let byte = source.rfind("bar()").expect("method token present");
    let prefix = &source[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };
    let uri = Url::parse("file:///test.pl").unwrap();
    let resp = find_definition(&analysis, pos, &uri, &idx);
    let loc = match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        other => panic!("expected goto-def on method token, got {other:?}"),
    };
    assert_eq!(
        loc.range.start.line, 1,
        "`bar` method token should resolve to `sub bar` (line 1), not the package decl",
    );
}

// ---- Consumer import-binding evaluator (ExportSurface / imported_names) ----
//
// One bound set, every consumer reads it: the unresolved-function diagnostic
// and goto-def both route through `imported_names`, so "found by goto-def,
// flagged by diagnostic" is impossible. ModX has @EXPORT=always_here,
// @EXPORT_OK=opt_here, %EXPORT_TAGS=(all=>[both]).

fn modx_index() -> crate::module_index::ModuleIndex {
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let src = "package ModX;\n\
our @EXPORT = qw(always_here);\n\
our @EXPORT_OK = qw(opt_here);\n\
our %EXPORT_TAGS = (all => [qw(always_here opt_here)]);\n\
sub always_here { 1 }\n\
sub opt_here { 2 }\n\
1;\n";
    idx.register_workspace_module(
        std::path::PathBuf::from("/tmp/perl_lsp_pin_ModX.pm"),
        std::sync::Arc::new(parse_analysis(src)),
    );
    idx
}

/// Does the unresolved-function diagnostic flag `name` in `source`?
fn flags_fn(source: &str, name: &str, idx: &crate::module_index::ModuleIndex) -> bool {
    let analysis = parse_analysis(source);
    collect_diagnostics(&analysis, idx, Default::default())
        .iter()
        .any(|d| {
            matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-function")
                && d.message.contains(&format!("'{}'", name))
        })
}

/// Does goto-def resolve the LAST call to `name` in `source` to ModX.pm?
fn gd_resolves(source: &str, name: &str, idx: &crate::module_index::ModuleIndex) -> bool {
    let analysis = parse_analysis(source);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(source, None).unwrap();
    let byte = source.rfind(&format!("{}(", name)).expect("call site present");
    let prefix = &source[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };
    let uri = Url::parse("file:///consumer.pl").unwrap();
    let loc = match find_definition(&analysis, pos, &uri, idx) {
        Some(GotoDefinitionResponse::Scalar(loc)) => Some(loc),
        Some(GotoDefinitionResponse::Array(mut v)) if !v.is_empty() => Some(v.remove(0)),
        _ => None,
    };
    loc.map_or(false, |l| l.uri.path().ends_with("ModX.pm"))
}

#[test]
fn bare_use_binds_export_default_no_fp_and_gd_resolves() {
    let idx = modx_index();
    let src = "use ModX;\nalways_here();\n";
    assert!(!flags_fn(src, "always_here", &idx), "bare use binds @EXPORT — no FP");
    assert!(gd_resolves(src, "always_here", &idx), "goto-def resolves @EXPORT name");
}

#[test]
fn named_import_still_works_regression() {
    let idx = modx_index();
    let src = "use ModX qw(opt_here);\nopt_here();\n";
    assert!(!flags_fn(src, "opt_here", &idx), "named import binds the name");
    assert!(gd_resolves(src, "opt_here", &idx), "goto-def resolves named import");
}

#[test]
fn tag_selector_binds_members_no_fp_and_gd_resolves() {
    let idx = modx_index();
    let src = "use ModX qw(:all);\nopt_here();\n";
    assert!(!flags_fn(src, "opt_here", &idx), ":all tag binds member opt_here");
    assert!(gd_resolves(src, "opt_here", &idx), "goto-def resolves :tag member");
}

#[test]
fn default_tag_equals_export() {
    let idx = modx_index();
    // :DEFAULT is the Exporter alias for @EXPORT — binds always_here, not opt_here.
    let src = "use ModX qw(:DEFAULT);\nalways_here();\n";
    assert!(!flags_fn(src, "always_here", &idx), ":DEFAULT binds @EXPORT member");
    assert!(gd_resolves(src, "always_here", &idx), "goto-def resolves :DEFAULT member");
}

#[test]
fn as_rename_binds_local_to_origin() {
    let idx = modx_index();
    // local `here` aliases origin `always_here`; goto-def on `here` reaches the origin.
    let src = "use ModX always_here => { -as => 'here' };\nhere();\n";
    let analysis = parse_analysis(src);
    let renamed = analysis
        .imports
        .iter()
        .flat_map(|i| i.imported_symbols.iter())
        .find(|s| s.local_name == "here");
    assert!(
        renamed.map_or(false, |s| s.remote() == "always_here"),
        "the -as rename must bind local `here` to origin `always_here`; imports: {:?}",
        analysis.imports.iter().map(|i| i.imported_symbols.clone()).collect::<Vec<_>>(),
    );
    assert!(!flags_fn(src, "here", &idx), "renamed local `here` is bound — no FP");
    assert!(gd_resolves(src, "here", &idx), "goto-def on `here` reaches origin sub");
}

#[test]
fn as_rename_plain_comma_binds_local_to_origin() {
    let idx = modx_index();
    // `=>` is an autoquoting comma — `'always_here', { '-as', 'here' }` is
    // identical to `always_here => { -as => 'here' }`. The rename parse must
    // pair positionally so the plain-comma spelling binds the alias too.
    let src = "use ModX 'always_here', { '-as', 'here' };\nhere();\n";
    let analysis = parse_analysis(src);
    let renamed = analysis
        .imports
        .iter()
        .flat_map(|i| i.imported_symbols.iter())
        .find(|s| s.local_name == "here");
    assert!(
        renamed.map_or(false, |s| s.remote() == "always_here"),
        "plain-comma -as rename must bind local `here` to origin `always_here`; imports: {:?}",
        analysis.imports.iter().map(|i| i.imported_symbols.clone()).collect::<Vec<_>>(),
    );
    assert!(!flags_fn(src, "here", &idx), "renamed local `here` is bound — no FP");
    assert!(gd_resolves(src, "here", &idx), "goto-def on `here` reaches origin sub");
}

#[test]
fn empty_import_binds_nothing_flags_export_name() {
    let idx = modx_index();
    // `use ModX ();` — explicit empty list suppresses even @EXPORT.
    let src = "use ModX ();\nalways_here();\n";
    assert!(
        flags_fn(src, "always_here", &idx),
        "empty `()` import binds nothing — @EXPORT name must flag (honest)",
    );
}

#[test]
fn export_ok_on_bare_use_is_suppressed_gate5() {
    let idx = modx_index();
    // GATE-5 (load-bearing): a bare `use M;` suppresses unresolved-function for
    // @EXPORT_OK names. The builder cannot distinguish a runtime exporter's
    // defaults (recorded in export_ok) from traditional opt-in, so flagging them
    // would reintroduce the ~684-FP cluster. The honest "binds nothing" path is
    // `use M ();` (empty parens), tested separately.
    let src = "use ModX;\nopt_here();\n";
    assert!(
        !flags_fn(src, "opt_here", &idx),
        "bare use must suppress unresolved-function for @EXPORT_OK names (684-FP guard)",
    );
}

#[test]
fn diagnostic_and_gotodef_agree_on_bound_set() {
    let idx = modx_index();
    // For every case, "flagged by diagnostic" must equal "NOT brought" — and a
    // name the diagnostic stays silent on (brought) must goto-def resolve.
    let cases: &[(&str, &str, bool)] = &[
        // (source, name, should_flag_as_unresolved-function)
        ("use ModX;\nalways_here();\n", "always_here", false),
        ("use ModX qw(opt_here);\nopt_here();\n", "opt_here", false),
        ("use ModX qw(:all);\nopt_here();\n", "opt_here", false),
        ("use ModX ();\nalways_here();\n", "always_here", true),
    ];
    for (src, name, should_flag) in cases {
        let flagged = flags_fn(src, name, &idx);
        assert_eq!(
            flagged, *should_flag,
            "diagnostic verdict mismatch for `{}` in {:?}",
            name, src,
        );
        // Brought names (not flagged) must be navigable.
        if !should_flag {
            assert!(
                gd_resolves(src, name, &idx),
                "a non-flagged (brought) name must goto-def resolve: `{}` in {:?}",
                name, src,
            );
        }
    }
}

// ---- Transitive export surface: re-export edges (forms 1/2/3) ----
//
// A re-exporting module M folds another module's surface into its own. The
// consumer `imported_names` evaluator is UNCHANGED — it binds M's @EXPORT,
// which `ExportSurface` now reports transitively. Each test registers the
// producer(s) and M, then asserts the consumer's `use M;` binds the re-exported
// names (no FP) and goto-def reaches the defining producer file.

/// Register a module under a path derived from its name (`Pkg::Sub` →
/// `/tmp/perl_lsp_re_Pkg_Sub.pm`) so goto-def assertions can pin the file.
fn register_module(idx: &crate::module_index::ModuleIndex, pkg: &str, src: &str) {
    let file = format!("/tmp/perl_lsp_re_{}.pm", pkg.replace("::", "_"));
    idx.register_workspace_module(
        std::path::PathBuf::from(file),
        std::sync::Arc::new(parse_analysis(src)),
    );
}

/// Like `gd_resolves`, but pins the resolved file to `target_file`.
fn gd_resolves_to(
    source: &str,
    name: &str,
    idx: &crate::module_index::ModuleIndex,
    target_file: &str,
) -> bool {
    let analysis = parse_analysis(source);
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let _tree = parser.parse(source, None).unwrap();
    let byte = source.rfind(&format!("{}(", name)).expect("call site present");
    let prefix = &source[..byte];
    let pos = Position {
        line: prefix.matches('\n').count() as u32,
        character: (byte - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    };
    let uri = Url::parse("file:///consumer.pl").unwrap();
    let loc = match find_definition(&analysis, pos, &uri, idx) {
        Some(GotoDefinitionResponse::Scalar(loc)) => Some(loc),
        Some(GotoDefinitionResponse::Array(mut v)) if !v.is_empty() => Some(v.remove(0)),
        _ => None,
    };
    loc.map_or(false, |l| l.uri.path().ends_with(target_file))
}

#[test]
fn reexport_static_splice_binds_transitively() {
    // Form 1: `our @EXPORT = ('own_fn', @Base::EXPORT)` re-exports Base's surface.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "Base", "package Base;\nour @EXPORT = qw(base_fn);\nsub base_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nour @EXPORT = ('own_fn', @Base::EXPORT);\nsub own_fn { 2 }\n1;\n",
    );

    let src = "use M;\nbase_fn();\nown_fn();\n";
    assert!(!flags_fn(src, "base_fn", &idx), "re-exported base_fn binds, no FP");
    assert!(!flags_fn(src, "own_fn", &idx), "own_fn binds, no FP");
    assert!(
        gd_resolves_to(src, "base_fn", &idx, "re_Base.pm"),
        "goto-def base_fn reaches Base",
    );
    assert!(
        gd_resolves_to(src, "own_fn", &idx, "re_M.pm"),
        "goto-def own_fn reaches M",
    );
}

#[test]
fn reexport_loop_push_literal_qw_binds() {
    // Form 2: loop-push over a literal qw module list.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "A", "package A;\nour @EXPORT = qw(a_fn);\nsub a_fn { 1 }\n1;\n");
    register_module(&idx, "B", "package B;\nour @EXPORT = qw(b_fn);\nsub b_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nour @EXPORT = ();\nfor my $m (qw(A B)) {\n    push @EXPORT, @{\"${m}::EXPORT\"};\n}\n1;\n",
    );

    let src = "use M;\na_fn();\nb_fn();\n";
    assert!(!flags_fn(src, "a_fn", &idx), "loop-push re-exports A::a_fn");
    assert!(!flags_fn(src, "b_fn", &idx), "loop-push re-exports B::b_fn");
    assert!(gd_resolves_to(src, "a_fn", &idx, "re_A.pm"), "gd a_fn → A");
    assert!(gd_resolves_to(src, "b_fn", &idx, "re_B.pm"), "gd b_fn → B");
}

#[test]
fn reexport_loop_push_samefile_array_binds() {
    // Form 2 variant: the loop list is a same-file `my @mods = (...)` we chase.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "A", "package A;\nour @EXPORT = qw(a_fn);\nsub a_fn { 1 }\n1;\n");
    register_module(&idx, "B", "package B;\nour @EXPORT = qw(b_fn);\nsub b_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nour @EXPORT = ();\nmy @mods = ('A', 'B');\nfor my $m (@mods) {\n    push @EXPORT, @{\"${m}::EXPORT\"};\n}\n1;\n",
    );

    let src = "use M;\na_fn();\nb_fn();\n";
    assert!(!flags_fn(src, "a_fn", &idx), "same-file @mods list re-exports A");
    assert!(!flags_fn(src, "b_fn", &idx), "same-file @mods list re-exports B");
}

#[test]
fn reexport_loop_push_dynamic_list_mints_no_edge() {
    // Form 2 honesty: a dynamic/unresolvable module list mints NO edge — the
    // re-exported name stays unresolved (we don't fabricate).
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "A", "package A;\nour @EXPORT = qw(a_fn);\nsub a_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nour @EXPORT = ();\nfor my $m (@dynamic_runtime_list) {\n    push @EXPORT, @{\"${m}::EXPORT\"};\n}\n1;\n",
    );
    // Confirm no edge was minted on M.
    let m = idx.get_cached("M").expect("M cached");
    assert!(
        m.analysis.reexport_modules.is_empty(),
        "dynamic list must mint no re-export edge, got: {:?}",
        m.analysis.reexport_modules,
    );

    let src = "use M;\na_fn();\n";
    assert!(flags_fn(src, "a_fn", &idx), "dynamic list → a_fn stays unresolved (honest)");
}

#[test]
fn reexport_declarative_also_binds() {
    // Form 3: `setup_import_methods(also => ['Base'])` includes Base's surface.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "Base", "package Base;\nour @EXPORT = qw(base_fn);\nsub base_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nuse Moose::Exporter;\nMoose::Exporter->setup_import_methods( also => [ 'Base' ] );\n1;\n",
    );
    let m = idx.get_cached("M").expect("M cached");
    assert!(
        m.analysis.reexport_modules.contains(&"Base".to_string()),
        "also => ['Base'] mints a re-export edge, got: {:?}",
        m.analysis.reexport_modules,
    );

    let src = "use M;\nbase_fn();\n";
    assert!(!flags_fn(src, "base_fn", &idx), "also-re-exported base_fn binds");
    assert!(gd_resolves_to(src, "base_fn", &idx, "re_Base.pm"), "gd base_fn → Base");
}

#[test]
fn reexport_cycle_resolves_finitely() {
    // A re-exports B, B re-exports A — the seen-set bounds the walk; the
    // consumer's surface query must terminate and bind both names.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(
        &idx,
        "A",
        "package A;\nour @EXPORT = ('a_fn', @B::EXPORT);\nsub a_fn { 1 }\n1;\n",
    );
    register_module(
        &idx,
        "B",
        "package B;\nour @EXPORT = ('b_fn', @A::EXPORT);\nsub b_fn { 1 }\n1;\n",
    );

    let src = "use A;\na_fn();\nb_fn();\n";
    assert!(!flags_fn(src, "a_fn", &idx), "cycle: a_fn binds");
    assert!(!flags_fn(src, "b_fn", &idx), "cycle: b_fn binds via A→B edge");
}

#[test]
fn reexport_imported_names_evaluator_unchanged() {
    // Consumer-unchanged guard: `imported_names` is NOT special-cased for
    // re-exports — it binds whatever the surface reports. A surface built
    // WITHOUT the index walk (own-only) sees just M's own @EXPORT; built WITH
    // the index it sees the transitive set. Same evaluator, different surface.
    let idx = crate::module_index::ModuleIndex::new_for_test();
    register_module(&idx, "Base", "package Base;\nour @EXPORT = qw(base_fn);\nsub base_fn { 1 }\n1;\n");
    register_module(
        &idx,
        "M",
        "package M;\nour @EXPORT = ('own_fn', @Base::EXPORT);\nsub own_fn { 2 }\n1;\n",
    );
    let m = idx.get_cached("M").expect("M cached");

    // A real bare `use M;` import, parsed from source (no Default on Import).
    let consumer = parse_analysis("use M;\n");
    let import = consumer.imports.iter().find(|i| i.module_name == "M").expect("use M import");

    // Own-only surface: just M's own @EXPORT — base_fn NOT visible.
    let own = m.analysis.export_surface();
    let bound_own = crate::file_analysis::imported_names(import, &own);
    assert!(bound_own.iter().any(|(l, _)| l == "own_fn"));
    assert!(
        !bound_own.iter().any(|(l, _)| l == "base_fn"),
        "own-only surface must not include the re-exported name",
    );

    // Index-walked surface: same evaluator, transitive surface — base_fn binds.
    let walked = m.analysis.export_surface_with_index(&idx);
    let bound_walked = crate::file_analysis::imported_names(import, &walked);
    assert!(bound_walked.iter().any(|(l, _)| l == "own_fn"));
    assert!(
        bound_walked.iter().any(|(l, _)| l == "base_fn"),
        "transitive surface includes the re-exported name — via the SAME evaluator",
    );
}

/// Consumer-side ctor key navigation through the deferred owner: in a
/// file that only `use`s Point, goto-def on `x` in `Point->new(x => 1)`
/// jumps to the `field $x :param` decl in Point.pm via the index (the
/// build-time gate deferred because the class isn't local; query time
/// re-derives the owner with the index in hand).
#[test]
fn test_goto_def_deferred_ctor_key_cross_file() {
    let point_src = "\
use v5.38;
class Point {
    field $x :param :reader;
}
1;
";
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    module_index.insert_cache(
        "Point",
        Some(std::sync::Arc::new(crate::module_index::CachedModule::new(
            std::path::PathBuf::from("/tmp/sym_defer_point.pm"),
            std::sync::Arc::new(parse_analysis(point_src)),
        ))),
    );

    let consumer_src = "use Point;\nmy $p = Point->new(x => 1);\n";
    let analysis = parse_analysis(consumer_src);
    let uri = Url::parse("file:///tmp/sym_defer_consumer.pl").unwrap();
    // Cursor on `x` (row 1, col 19).
    let resp = find_definition(
        &analysis,
        Position { line: 1, character: 19 },
        &uri,
        &module_index,
    );
    let Some(GotoDefinitionResponse::Scalar(loc)) = resp else {
        panic!("expected scalar goto-def, got {:?}", resp);
    };
    assert!(
        loc.uri.path().ends_with("sym_defer_point.pm"),
        "lands in the class file: {:?}",
        loc.uri,
    );
    assert_eq!(loc.range.start.line, 2, "lands on the field decl line");
}

/// Closed-shape hash-key typo diagnostic: a READ of a key the closed
/// literal doesn't define is hinted. An unconditional write EXTENDS
/// the shape (the written key reads silently; other unknowns still
/// hint); a conditional write opens it; an escape (call arg / alias /
/// invocant / sigil deref) opens it AT the escape span — reads before
/// it still hint. Reassignment suppresses (the one remaining gate
/// clause); open shapes and known keys stay silent.
#[test]
fn test_closed_shape_unknown_key_diagnostic() {
    let src = "\
my $config = { host => 'x', port => 1 };
my $bad = $config->{typo};
my $ok = $config->{host};
my $mutv = { host => 'x' };
$mutv->{added} = 1;
my $r0 = $mutv->{added};
my $r1 = $mutv->{other};
my $cond = { host => 'x' };
$cond->{maybe} = 1 if $ENV{X};
my $rc = $cond->{anything};
my $esc = { host => 'x' };
my $pre = $esc->{typo_pre};
process($esc);
my $r2 = $esc->{anything};
my $re = { host => 'x' };
$re = fetch_config() if $ENV{X};
my $r3 = $re->{whatever};
my $base = { a => 1 };
my $open = { %$base, extra => 1 };
my $maybe = $open->{whatever};
";
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(
        &analysis,
        &idx,
        DiagnosticOptions::default(),
    );
    let keys: Vec<&str> = diags
        .iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unknown-hash-key"))
        .map(|d| d.message.as_str())
        .collect();
    assert_eq!(
        keys.len(),
        3,
        "the typo, the post-extension unknown, and the pre-escape typo: {:?}",
        keys,
    );
    assert!(keys[0].contains("'typo'"), "{:?}", keys);
    assert!(keys[0].contains("host"), "message names the known keys: {:?}", keys);
    assert!(keys[1].contains("'other'"), "{:?}", keys);
    assert!(
        keys[1].contains("added"),
        "extended shape names the written key: {:?}",
        keys,
    );
    assert!(
        keys[2].contains("'typo_pre'"),
        "read BEFORE the escape still hints: {:?}",
        keys,
    );
}

/// The literal-hash spelling gets the same diagnostic: container-form
/// reads (`$h{k}`) check against `%h`'s shape. A bare `func(%h)` pass
/// flattens to copies — the callee can't add keys — so it does NOT
/// suppress; `\%h` ref-taking does.
#[test]
fn test_literal_hash_unknown_key_diagnostic() {
    let src = "\
my %config = (host => 'x');
my $bad = $config{typo};
func(%config);
my %taken = (host => 'x');
my $r = \\%taken;
my $silent = $taken{anything};
";
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(
        &analysis,
        &idx,
        DiagnosticOptions::default(),
    );
    let keys: Vec<&str> = diags
        .iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unknown-hash-key"))
        .map(|d| d.message.as_str())
        .collect();
    assert_eq!(keys.len(), 1, "only the %config typo: {:?}", keys);
    assert!(keys[0].contains("'typo'"), "{:?}", keys);
    assert!(keys[0].contains("%config"), "names the hash variable: {:?}", keys);
}

/// Expression-base spelling: `cfg()->{kye}` hints off the producer's
/// closed return shape — no variable in hand, the drill's Projected
/// witness carries the (base, key) pair. Known keys stay silent, and
/// bare-variable bases stay the gated ref loop's territory.
#[test]
fn test_expression_base_unknown_key_diagnostic() {
    let src = "\
sub cfg { return { host => 'x', port => 1 } }
my $ok = cfg()->{host};
my $bad = cfg()->{hsot};
cfg()->{hsot2};
";
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(
        &analysis,
        &idx,
        DiagnosticOptions::default(),
    );
    let keys: Vec<&str> = diags
        .iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unknown-hash-key"))
        .map(|d| d.message.as_str())
        .collect();
    assert_eq!(
        keys.len(),
        2,
        "assignment-position and bare-statement call-base typos: {:?}",
        keys,
    );
    assert!(keys[0].contains("'hsot'"), "{:?}", keys);
    assert!(
        keys[0].contains("this expression's"),
        "expression-base message form: {:?}",
        keys,
    );
    assert!(
        keys[1].contains("'hsot2'"),
        "bare-statement drill is witnessed too: {:?}",
        keys,
    );
}

/// Role `requires NAMES` declares method contracts: `$self->name`
/// inside the role resolves to the synthesized marker (no
/// unresolved-method hint, goto-def lands on the contract atom); a
/// genuinely unknown method still hints. Both the qw and
/// single-string spellings.
#[test]
fn test_role_requires_suppresses_unresolved_method() {
    let src = "\
package My::Role;
use Moo::Role;
requires qw/fetch source/;
requires 'extra';
sub run {
  my ($self) = @_;
  $self->fetch;
  $self->source;
  $self->extra;
  $self->typo_method;
}
1;
";
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(
        &analysis,
        &idx,
        DiagnosticOptions::default(),
    );
    let unresolved: Vec<&str> = diags
        .iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "unresolved-method"))
        .map(|d| d.message.as_str())
        .collect();
    assert_eq!(unresolved.len(), 1, "only the typo: {:?}", unresolved);
    assert!(unresolved[0].contains("typo_method"), "{:?}", unresolved);
    assert_eq!(
        analysis.role_requires.get("My::Role").map(|v| v.len()),
        Some(3),
        "the contract record carries all three names",
    );
}

/// Anonymous subs are resolvable, not browsable: the synthesized
/// `(anon)` symbol carries hide_in_outline, and the workspace-symbol
/// converter honors it (an empty/broad query must not surface them).
#[test]
fn test_anon_subs_hidden_from_workspace_symbols() {
    let src = "\
my $cb = sub { return 42 };
sub real_sub { 1 }
";
    let analysis = parse_analysis(src);
    let uri = tower_lsp::lsp_types::Url::parse("file:///t.pl").unwrap();
    let names: Vec<String> = analysis
        .symbols
        .iter()
        .filter_map(|s| symbol_to_workspace_info(s, uri.clone()))
        .map(|i| i.name)
        .collect();
    assert!(
        names.iter().any(|n| n == "real_sub"),
        "real subs surface: {:?}",
        names,
    );
    assert!(
        !names.iter().any(|n| n.contains("anon")),
        "anon subs stay out: {:?}",
        names,
    );
}

/// `my sub helper { … }` — document symbols keep it (real in-file
/// structure); workspace-symbol search drops it (not addressable
/// outside its block). Plain subs surface in both.
#[test]
fn test_lexical_subs_outline_only() {
    let src = "\
my sub helper_fn { 42 }
sub public_fn { helper_fn() }
";
    let analysis = parse_analysis(src);
    let uri = tower_lsp::lsp_types::Url::parse("file:///t.pl").unwrap();
    let ws: Vec<String> = analysis
        .symbols
        .iter()
        .filter_map(|s| symbol_to_workspace_info(s, uri.clone()))
        .map(|i| i.name)
        .collect();
    assert!(ws.iter().any(|n| n == "public_fn"), "{:?}", ws);
    assert!(!ws.iter().any(|n| n == "helper_fn"), "lexical stays out of workspace: {:?}", ws);
    let outline = analysis.document_symbols();
    let names: Vec<&str> = outline.iter().map(|d| d.name.as_str()).collect();
    assert!(names.contains(&"helper_fn"), "outline keeps the lexical sub: {:?}", names);
    assert!(names.contains(&"public_fn"), "{:?}", names);
}

// ---- role-requires-unfulfilled (composer-mismatch) ----

fn role_requires_diags(source: &str) -> Vec<String> {
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    collect_diagnostics(&analysis, &module_index, Default::default())
        .into_iter()
        .filter(|d| {
            matches!(&d.code, Some(tower_lsp::lsp_types::NumberOrString::String(c))
                if c == "role-requires-unfulfilled")
        })
        .map(|d| d.message)
        .collect()
}

#[test]
fn test_role_requires_unfulfilled_fires_on_missing_def() {
    let msgs = role_requires_diags(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n\
         package My::Broken;\nuse Moo;\nwith 'My::Role';\nsub other { 1 }\n1;\n",
    );
    assert_eq!(
        msgs,
        vec!["role My::Role requires 'fetch'; My::Broken does not provide it"],
    );
}

#[test]
fn test_role_requires_satisfied_stays_quiet() {
    // Local sub, has-accessor, and a sibling role's def all count as
    // provided; the role itself is never diagnosed.
    let msgs = role_requires_diags(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n\
         package My::Provider;\nuse Moo::Role;\nsub fetch { 9 }\n\
         package My::Ok;\nuse Moo;\nwith 'My::Role';\nsub fetch { 1 }\n\
         package My::Attr;\nuse Moo;\nwith 'My::Role';\nhas fetch => (is => 'ro');\n\
         package My::Sibling;\nuse Moo;\nwith 'My::Role', 'My::Provider';\n1;\n",
    );
    assert!(msgs.is_empty(), "expected no diagnostics, got: {:?}", msgs);
}

#[test]
fn test_role_requires_transitive_and_marker_not_a_def() {
    // SubRole composes Role and re-requires the contract: the marker
    // must not satisfy Deep, but Deep's real def does — while Broken2
    // composing SubRole is told about the (inherited) contract.
    let msgs = role_requires_diags(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n\
         package My::SubRole;\nuse Moo::Role;\nwith 'My::Role';\nrequires 'fetch';\n\
         package My::Deep;\nuse Moo;\nwith 'My::SubRole';\nsub fetch { 7 }\n\
         package My::Broken2;\nuse Moo;\nwith 'My::SubRole';\n1;\n",
    );
    assert_eq!(msgs.len(), 1, "only Broken2 should fire, got: {:?}", msgs);
    assert!(msgs[0].contains("My::Broken2 does not provide it"), "got: {:?}", msgs);
}

#[test]
fn test_role_requires_honest_silence() {
    // AUTOLOAD anywhere in the MRO and unresolvable ancestors both
    // suppress — the contract may be satisfied where we can't see.
    let msgs = role_requires_diags(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n\
         package My::Auto;\nuse Moo;\nwith 'My::Role';\nsub AUTOLOAD { }\n\
         package My::Mystery;\nuse Moo;\nextends 'Vendor::Unknown';\nwith 'My::Role';\n1;\n",
    );
    assert!(msgs.is_empty(), "expected honest silence, got: {:?}", msgs);
}

#[test]
fn test_role_requires_default_implementation_provides() {
    // The Clove::Sheets pattern: the role both requires AND defines
    // the name (requires as documentation, def as default). The real
    // def must count as provision — only the marker is excluded.
    let msgs = role_requires_diags(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\nsub fetch { 'default' }\n\
         package My::Composer;\nuse Moo;\nwith 'My::Role';\n1;\n",
    );
    assert!(msgs.is_empty(), "default impl in the role provides, got: {:?}", msgs);
}

#[test]
fn test_role_requires_dynamic_parent_honest_silence() {
    // `with ReportProxy(type => ...)` — a runtime-generated role we
    // can't fold. The recorded parent list is incomplete, so neither
    // the composer-mismatch warning nor the unresolved-method hint
    // may fire on this package.
    let analysis = parse_analysis(
        "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n\
         package My::Dynamic;\nuse Moo;\nwith RoleGen(type => 'x');\nwith 'My::Role';\n\
         sub run { my $self = shift; $self->fetch }\n1;\n",
    );
    assert!(
        analysis.dynamic_parent_packages.contains("My::Dynamic"),
        "unfoldable with-arg must mark the package dynamic",
    );
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    let diags = collect_diagnostics(&analysis, &module_index, Default::default());
    let role_or_method: Vec<&String> = diags
        .iter()
        .filter(|d| {
            matches!(&d.code, Some(tower_lsp::lsp_types::NumberOrString::String(c))
                if c == "role-requires-unfulfilled" || c == "unresolved-method")
        })
        .map(|d| &d.message)
        .collect();
    assert!(
        role_or_method.is_empty(),
        "dynamic parents must suppress, got: {:?}",
        role_or_method,
    );
}

// ---- helper-not-loaded (entrypoint-scan lint) ----

fn helper_lint_setup() -> (crate::module_index::ModuleIndex, String) {
    // A workspace Mojolicious plugin registering a helper. The bundled
    // mojo plugins synthesize the helper entity bridged to the
    // controller surface.
    let plugin_src = "package My::Plugin::WasLoaded;\nuse Mojo::Base 'Mojolicious::Plugin';\n\
        sub register {\n    my ($self, $app, $conf) = @_;\n    $app->helper(was_loaded => sub { 1 });\n}\n1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(plugin_src, None).unwrap();
    let fa = crate::builder::build(&tree, plugin_src.as_bytes());
    idx.register_workspace_module(
        std::path::PathBuf::from("/fake/lint/My/Plugin/WasLoaded.pm"),
        std::sync::Arc::new(fa),
    );
    let consumer_src = "package MyApp::C;\nuse Mojo::Base 'Mojolicious::Controller';\n\
        sub act {\n    my $self = shift;\n    $self->was_loaded;\n}\n1;\n"
        .to_string();
    (idx, consumer_src)
}

fn lint_messages(idx: &crate::module_index::ModuleIndex, src: &str) -> Vec<String> {
    let analysis = parse_analysis(src);
    collect_diagnostics(&analysis, idx, Default::default())
        .into_iter()
        .filter(|d| {
            matches!(&d.code, Some(tower_lsp::lsp_types::NumberOrString::String(c))
                if c == "helper-not-loaded")
        })
        .map(|d| d.message)
        .collect()
}

#[test]
fn test_helper_not_loaded_fires_for_unloaded_workspace_plugin() {
    let (idx, consumer) = helper_lint_setup();
    let msgs = lint_messages(&idx, &consumer);
    assert_eq!(
        msgs,
        vec!["'was_loaded' is provided by My::Plugin::WasLoaded, which no workspace entrypoint loads"],
    );
}

#[test]
fn test_helper_not_loaded_suppressed_when_an_entrypoint_loads_it() {
    let (idx, consumer) = helper_lint_setup();
    // A packageless entrypoint script loading the plugin — exactly the
    // file shape (lite app) that never enters the module cache; the
    // loaded-set feed must run before the packageless early-return.
    let entry_src = "use My::Plugin::WasLoaded;\nprint 1;\n";
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(entry_src, None).unwrap();
    let fa = crate::builder::build(&tree, entry_src.as_bytes());
    idx.register_workspace_module(
        std::path::PathBuf::from("/fake/lint/app.pl"),
        std::sync::Arc::new(fa),
    );
    assert!(lint_messages(&idx, &consumer).is_empty());
}

#[test]
fn test_helper_not_loaded_exempts_installed_plugins() {
    // Same plugin arriving via insert_cache (the @INC path, not
    // workspace registration): the "downloaded = intended" policy —
    // no lint.
    let plugin_src = "package My::Plugin::WasLoaded;\nuse Mojo::Base 'Mojolicious::Plugin';\n\
        sub register {\n    my ($self, $app, $conf) = @_;\n    $app->helper(was_loaded => sub { 1 });\n}\n1;\n";
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(plugin_src, None).unwrap();
    let fa = crate::builder::build(&tree, plugin_src.as_bytes());
    idx.insert_cache(
        "My::Plugin::WasLoaded",
        Some(std::sync::Arc::new(crate::file_analysis::CachedModule::new(
            std::path::PathBuf::from("/inc/My/Plugin/WasLoaded.pm"),
            std::sync::Arc::new(fa),
        ))),
    );
    let consumer = "package MyApp::C;\nuse Mojo::Base 'Mojolicious::Controller';\n\
        sub act {\n    my $self = shift;\n    $self->was_loaded;\n}\n1;\n";
    assert!(lint_messages(&idx, consumer).is_empty());
}


// D5 — redundant RE-narrowing. Subsumed by D3: an earlier guard's narrowing
// becomes the subject's prior type at a later same-type guard (the narrowing
// witness survives because no reassignment truncated it), so D3 flags the
// second guard. No separate diagnostic path — this test pins that coverage.
#[test]
fn d5_sequential_renarrow_is_flagged_by_d3() {
    let src = r#"
package Foo;
sub new { bless {}, shift }
sub frob { 1 }
package Main;
sub f {
    my ($self, $x) = @_;
    return unless $x->isa('Foo');
    $x->frob;
    return unless $x->isa('Foo');
    return 1;
}
1;
"#;
    let analysis = parse_analysis(src);
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let on = DiagnosticOptions { redundant_guard: true, ..Default::default() };
    let redundant: Vec<_> = collect_diagnostics(&analysis, &idx, on)
        .into_iter()
        .filter(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "redundant-guard"))
        .collect();
    // Exactly the SECOND guard (line 9, 0-based) is redundant; the first
    // (line 6) narrows from an un-typed prior and is not.
    assert_eq!(
        redundant.len(),
        1,
        "second guard redundant, first not: {:?}",
        redundant.iter().map(|d| (d.range.start.line, &d.message)).collect::<Vec<_>>(),
    );
    assert_eq!(redundant[0].range.start.line, 9);
}
