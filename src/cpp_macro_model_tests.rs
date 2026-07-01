//! Config-variant macro model: guard eval, reachability ranking, join typing.

use super::*;
use crate::file_analysis::InferredType;

fn cfg(defined: &[&str], universe: &[&str]) -> KnownConfig {
    KnownConfig::new(
        defined.iter().map(|s| s.to_string()).collect(),
        // the universe always contains everything defined (a defined macro is
        // trivially in the closure's definition set)
        universe.iter().chain(defined.iter()).map(|s| s.to_string()).collect(),
    )
}

fn variant(body: &str, guards: &[&str]) -> MacroVariant {
    MacroVariant {
        body: body.to_string(),
        params: None,
        guards: guards.iter().map(|s| s.to_string()).collect(),
        site: MacroSite { file: None, line: 0 },
    }
}

/// cpp `annot_type` (subset) so the join test doesn't depend on the pack.
fn annot(text: &str) -> Option<InferredType> {
    match text.trim() {
        "int" | "unsigned" | "short" | "long" | "U16" => Some(InferredType::Numeric),
        "auto" | "void" => None,
        t => Some(InferredType::ClassName(t.to_string())),
    }
}

#[test]
fn unconditional_define_is_active() {
    assert_eq!(classify(&[], &KnownConfig::default()), Reachability::Active);
}

#[test]
fn defined_guard_active_when_known_on() {
    let r = classify(&["defined(HAS)".into()], &cfg(&["HAS"], &[]));
    assert_eq!(r, Reachability::Active);
}

#[test]
fn platform_macro_never_defined_is_unreachable() {
    // WIN32 is neither predefined nor #defined anywhere in the closure → a
    // platform macro absent on this target → provably false.
    let r = classify(&["defined(WIN32)".into()], &cfg(&[], &["HAS"]));
    assert_eq!(r, Reachability::Unreachable { reason: "WIN32 undefined".into() });
}

#[test]
fn knob_defined_somewhere_but_not_on_is_unknown() {
    // HAS_NON_INT_BITFIELDS is a Configure knob we've SEEN defined in the
    // closure (universe) but can't resolve here → UNKNOWN, not guessed.
    let r = classify(&["defined(HAS)".into()], &cfg(&[], &["HAS"]));
    assert_eq!(r, Reachability::Unknown { guard: "defined(HAS)".into() });
}

#[test]
fn negated_knob_is_unknown_too() {
    // `!defined(HAS)` with HAS a known-but-unresolved knob is equally unknown.
    let r = classify(&["!defined(HAS)".into()], &cfg(&[], &["HAS"]));
    assert!(matches!(r, Reachability::Unknown { .. }));
}

#[test]
fn negated_active_knob_is_unreachable() {
    let r = classify(&["!defined(HAS)".into()], &cfg(&["HAS"], &[]));
    assert_eq!(r, Reachability::Unreachable { reason: "HAS defined".into() });
}

#[test]
fn conjunction_three_valued() {
    // !defined(WIN32) && defined(HAS): WIN32 absent → true; HAS knob → unknown.
    let r = classify(
        &["!defined(WIN32)".into(), "defined(HAS)".into()],
        &cfg(&[], &["HAS"]),
    );
    assert!(matches!(r, Reachability::Unknown { .. }));
    // with WIN32 present the whole thing is provably false regardless of HAS.
    let r2 = classify(
        &["!defined(WIN32)".into(), "defined(HAS)".into()],
        &cfg(&["WIN32"], &["HAS"]),
    );
    assert_eq!(r2, Reachability::Unreachable { reason: "WIN32 defined".into() });
}

#[test]
fn ranking_puts_active_first_unreachable_last() {
    // The PERL_BITFIELD16 shape: three variants, three configs. Known: HAS is
    // ON, WIN32 is a platform macro absent here, CFG is a seen-but-unresolved
    // knob → one of each outcome.
    let mv = MacroVariants {
        name: "M".into(),
        variants: vec![
            variant("U16", &["defined(WIN32)"]),  // unreachable
            variant("unsigned", &["defined(HAS)"]), // active
            variant("int", &["defined(CFG)"]),    // unknown
        ],
    };
    let c = cfg(&["HAS"], &["CFG"]);
    let ranked = mv.ranked(&c);
    assert_eq!(ranked.len(), 3, "nothing is ever pruned");
    // active first
    assert_eq!(ranked[0].0.body, "unsigned");
    assert_eq!(ranked[0].1, Reachability::Active);
    // unknown middle
    assert!(matches!(ranked[1].1, Reachability::Unknown { .. }));
    assert_eq!(ranked[1].0.body, "int");
    // unreachable last, labeled
    assert_eq!(ranked[2].0.body, "U16");
    assert_eq!(
        ranked[2].1.label().as_deref(),
        Some("unreachable: WIN32 undefined")
    );
}

#[test]
fn join_agrees_to_exact_type() {
    let mv = MacroVariants {
        name: "M".into(),
        variants: vec![variant("int", &[]), variant("int", &[])],
    };
    assert_eq!(mv.join_type(annot), Some(InferredType::Numeric));
}

#[test]
fn join_integer_family_widens_to_numeric() {
    // U16 ∨ unsigned → an integer type (the op_type fix).
    let mv = MacroVariants {
        name: "PERL_BITFIELD16".into(),
        variants: vec![
            variant("U16", &["defined(WIN32)"]),
            variant("U16", &["defined(HAS)"]),
            variant("unsigned", &["!defined(HAS)"]),
        ],
    };
    assert_eq!(mv.join_type(annot), Some(InferredType::Numeric));
}

#[test]
fn join_divergent_types_have_no_abstraction() {
    let mv = MacroVariants {
        name: "M".into(),
        variants: vec![variant("int", &[]), variant("Widget", &[])],
    };
    assert_eq!(mv.join_type(annot), None);
}

#[test]
fn join_skips_untypable_void_auto() {
    let mv = MacroVariants {
        name: "M".into(),
        variants: vec![variant("void", &[]), variant("int", &[])],
    };
    assert_eq!(mv.join_type(annot), Some(InferredType::Numeric));
}
