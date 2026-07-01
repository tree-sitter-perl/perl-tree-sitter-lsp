//! Driver + registry: Perl always; C++ under `--features cpp`.

use super::*;

#[test]
fn perl_driver_analyzes() {
    let fa = PerlDriver.analyze("package Foo;\nsub bar { 1 }\n");
    assert!(fa.symbols.iter().any(|s| s.name == "bar"), "perl driver finds the sub");
}

#[test]
fn registry_serves_perl_by_default() {
    let reg = LanguageRegistry::with_enabled();
    assert!(reg.languages().contains(&"perl"));
    assert_eq!(reg.for_path(std::path::Path::new("Foo.pm")).map(|d| d.id()), Some("perl"));
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_driver_analyzes_through_reparse() {
    // a declarator-position macro that would otherwise destroy the class
    let src = "#define API __attribute__((visibility(\"default\")))\nclass API Box { public: int width; };\n";
    let fa = cpp_driver().analyze(src);
    assert!(fa.symbols.iter().any(|s| s.name == "Box"), "macro-recovered class: {:?}", fa.symbols.iter().map(|s| &s.name).collect::<Vec<_>>());
    assert!(fa.symbols.iter().any(|s| s.name == "width"));
    // The unknown-macro safety net: `API` isn't in the attribute-macro
    // vocabulary, so the class is recovered but carries NO signal.
    let boxsym = fa.symbols.iter().find(|s| s.name == "Box").unwrap();
    assert!(boxsym.attributes.is_empty(), "unknown macro → no signal: {:?}", boxsym.attributes);
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_known_attribute_macro_signals_the_recovered_class() {
    // A KNOWN declarator macro (Qt's Q_CORE_EXPORT, in the bundled
    // cpp-attributes vocabulary) recovers the class AND stamps its signal.
    let src = "class Q_CORE_EXPORT Widget { public: int x; };\n";
    let fa = cpp_driver().analyze(src);
    let widget = fa.symbols.iter().find(|s| s.name == "Widget")
        .unwrap_or_else(|| panic!("Widget recovered: {:?}", fa.symbols.iter().map(|s| &s.name).collect::<Vec<_>>()));
    assert!(widget.attributes.contains(&"exported".to_string()),
        "Q_CORE_EXPORT signals exported: {:?}", widget.attributes);
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_deprecated_attribute_macro_signals_the_recovered_class() {
    let src = "class Q_DEPRECATED OldThing { public: int x; };\n";
    let fa = cpp_driver().analyze(src);
    let sym = fa.symbols.iter().find(|s| s.name == "OldThing").expect("OldThing recovered");
    assert!(sym.attributes.contains(&"deprecated".to_string()),
        "Q_DEPRECATED signals deprecated: {:?}", sym.attributes);
}

#[cfg(feature = "cpp")]
#[test]
fn registry_serves_cpp_when_enabled() {
    let reg = LanguageRegistry::with_enabled();
    assert!(reg.languages().contains(&"cpp"));
    assert_eq!(reg.for_path(std::path::Path::new("x.cpp")).map(|d| d.id()), Some("cpp"));
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_macro_recovered_spans_are_in_original_coords() {
    // A declarator-position macro expands to a long attribute, shifting
    // byte positions. The recovered `Box` symbol must point at the
    // ORIGINAL `Box`, not the expanded coordinate.
    let src = "#define API __attribute__((visibility(\"default\")))\nclass API Box { public: int width; };\n";
    let fa = cpp_driver().analyze(src);
    let boxsym = fa.symbols.iter().find(|s| s.name == "Box").expect("Box recovered");
    // original: `class API Box {` → Box at row 1, col 10
    let p = boxsym.selection_span.start;
    assert_eq!((p.row, p.column), (1, 10), "Box span in ORIGINAL coords: {:?}", p);
    // and the original source at that point really is "Box"
    let line = src.lines().nth(1).unwrap();
    assert_eq!(&line[p.column..p.column + 3], "Box");
}

#[test]
fn perl_trigger_chars_unchanged() {
    let tc = LanguageRegistry::with_enabled().trigger_chars();
    // The Perl reference set — a perl-only build must keep exactly these.
    for c in ["$", "@", "%", ">", ":", "{", "(", ","] {
        assert!(tc.iter().any(|s| s == c), "missing perl trigger {c}");
    }
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_adds_dot_trigger() {
    let tc = LanguageRegistry::with_enabled().trigger_chars();
    assert!(tc.iter().any(|s| s == "."), "cpp build should add '.' trigger: {tc:?}");
}

#[cfg(feature = "cpp")]
#[test]
fn cpp_enumerator_carries_parent_enum_as_container_and_type() {
    use crate::file_analysis::InferredType;
    // Hovering an enum member surfaces its enum, the same `name: type` way a
    // struct field renders: `RED: Color`. Wired as the enumerator's container
    // (package) + type (ClassName of the enum).
    let fa = cpp_driver().analyze("enum Color { RED, GREEN };\n");
    let red = fa
        .symbols
        .iter()
        .find(|s| s.name == "RED")
        .unwrap_or_else(|| panic!("RED enumerator: {:?}",
            fa.symbols.iter().map(|s| &s.name).collect::<Vec<_>>()));
    assert_eq!(red.package.as_deref(), Some("Color"),
        "enum member's container is its enum");
    assert_eq!(
        fa.inferred_type_via_bag("RED", red.span.start),
        Some(InferredType::ClassName("Color".to_string())),
        "enum member's type is its enum, so hover renders `RED: Color`"
    );
    // A bare `enum` (no @scope) keeps members in the enclosing scope, so a
    // later bare read of RED still resolves to this def.
    assert_eq!(red.scope, crate::file_analysis::ScopeId(0),
        "enumerators leak into the enclosing (file) scope");
}

