use super::*;

#[test]
fn bundled_perlfunc_yields_common_builtins() {
    let parsed = parse_bundled_perlfunc().expect("bundled POD parses");
    for name in ["push", "pop", "shift", "scalar", "keys", "values", "join", "split"] {
        assert!(
            parsed.entries.contains_key(name),
            "bundled perlfunc.pod should contain `{name}` (got {} entries)",
            parsed.entries.len(),
        );
    }
}

#[test]
fn entries_carry_version_footer() {
    let parsed = parse_bundled_perlfunc().expect("bundled POD parses");
    let push = parsed.entries.get("push").expect("push entry");
    assert!(
        push.contains("perl ") && push.contains("bundled"),
        "expected version footer in entry body, got tail: {}",
        push.chars().rev().take(80).collect::<String>().chars().rev().collect::<String>(),
    );
}

#[test]
fn push_body_describes_array_append() {
    let parsed = parse_bundled_perlfunc().expect("bundled POD parses");
    let push = parsed.entries.get("push").expect("push entry");
    assert!(
        push.to_lowercase().contains("array"),
        "push body should mention `array`, got: {push}"
    );
}
