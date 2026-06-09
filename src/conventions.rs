//! Perl-convention name predicates.
//!
//! Each convention the analyzer leans on is asked through ONE predicate here
//! instead of being re-spelled as a string match at every consumer (rule #10:
//! the value answers the question). When a convention grows — a plugin
//! declaring extra invocant names, configurable constructor verbs — the
//! change lands here once and every consumer inherits it.
//!
//! Pure `&str` predicates only: no tree-sitter, so `file_analysis.rs` (which
//! must stay tree-free) can use them. Node-level semantics live in `cst.rs`.

/// Conventional invocant variable names — `sub f { my ($self) = @_ }` and
/// friends. Accepts the bare identifier or the `$`-sigiled spelling so both
/// param names (`"$self"`) and canonical varnames (`"self"`) route here.
///
/// "Conventional" means: the *name alone* signals receiver-ness. A variable
/// not on this list can still be the invocant (`my ($c) = @_;`) — callers
/// that know the position (first param of a method) must not gate on this.
pub fn is_conventional_invocant_name(name: &str) -> bool {
    matches!(
        name.strip_prefix('$').unwrap_or(name),
        "self" | "class" | "this" | "proto"
    )
}

/// Conventional constructor method name. Perl has no `new` keyword — this is
/// pure convention, but it's the convention every framework and the inference
/// rules ("`Class->new` returns `Class`") build on.
pub fn is_constructor_name(name: &str) -> bool {
    name == "new"
}
