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

/// `__PACKAGE__` — the compile-time token for the enclosing package.
pub fn is_current_package_token(text: &str) -> bool {
    text == "__PACKAGE__"
}

/// The text of a method-call invocant, classified once. Consumers match
/// the variant instead of re-deriving the shape with sigil/keyword string
/// checks at each site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvocantText<'a> {
    /// `$obj` / `@list` / `%h` — a variable; its class comes from
    /// inference, never from the spelling.
    Variable(&'a str),
    /// `__PACKAGE__` — the enclosing package.
    CurrentPackage,
    /// `shift` / `$_[0]` / `@_[0]` — the method's own receiver argument
    /// read positionally (`my $self = shift`); resolves to the enclosing
    /// class. Not real variables — the bag has no witness for them.
    PositionalReceiver,
    /// Anything else — a bareword: a class name, or a class-returning
    /// zero-arg sub (`app->routes`).
    Bareword(&'a str),
}

impl<'a> InvocantText<'a> {
    pub fn parse(text: &'a str) -> Self {
        match text {
            t if is_current_package_token(t) => Self::CurrentPackage,
            "shift" | "$_[0]" | "@_[0]" => Self::PositionalReceiver,
            t if t.starts_with('$') || t.starts_with('@') || t.starts_with('%') => {
                Self::Variable(t)
            }
            t => Self::Bareword(t),
        }
    }
}

/// A method-call name token (`$obj->Foo::Bar::m`, `$self->SUPER::m`,
/// `->::m`, `->m`), parsed once. Consumers match the variant instead of
/// re-deriving qualifier semantics with string ops — the qualifier's
/// *meaning* (SUPER is not a class; `::` is the `main` shorthand; anything
/// else is the literal dispatch package) lives here and nowhere else.
///
/// Scope: method tokens only. Function/decl names (`Foo::bar()`, glob
/// splices, `our @Pkg::EXPORT`) have no SUPER keyword — they keep
/// `file_analysis::split_qualified`, the raw `(package, basename)` seam.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodToken<'a> {
    /// `m` — dispatch starts at the invocant's class.
    Bare(&'a str),
    /// `SUPER::m` — the one qualifier that does NOT name a class: dispatch
    /// starts at the parents of the package the call is *written* in
    /// (and there may be several).
    Super(&'a str),
    /// `::m` — `main::` shorthand; the dispatch package is `main`.
    Main(&'a str),
    /// `Foo::Bar::m` — the qualifier is the literal dispatch package.
    Qualified { package: &'a str, name: &'a str },
}

impl<'a> MethodToken<'a> {
    pub fn parse(token: &'a str) -> Self {
        match token.rsplit_once("::") {
            None => Self::Bare(token),
            Some(("SUPER", tail)) => Self::Super(tail),
            Some(("", tail)) => Self::Main(tail),
            Some((pkg, tail)) => Self::Qualified { package: pkg, name: tail },
        }
    }

    /// The bare method name — the tail after any qualifier.
    pub fn name(&self) -> &'a str {
        match self {
            Self::Bare(n) | Self::Super(n) | Self::Main(n) => n,
            Self::Qualified { name, .. } => name,
        }
    }

    /// The literal dispatch package, when the qualifier names one.
    /// `None` for `Bare` (the invocant decides) and `Super` (the writing
    /// package's parent MRO decides — resolving it needs ancestry).
    pub fn literal_package(&self) -> Option<&'a str> {
        match self {
            Self::Qualified { package, .. } => Some(package),
            Self::Main(_) => Some("main"),
            Self::Bare(_) | Self::Super(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{InvocantText, MethodToken};

    #[test]
    fn invocant_text_variants() {
        assert_eq!(InvocantText::parse("$obj"), InvocantText::Variable("$obj"));
        assert_eq!(InvocantText::parse("@list"), InvocantText::Variable("@list"));
        assert_eq!(InvocantText::parse("%h"), InvocantText::Variable("%h"));
        assert_eq!(InvocantText::parse("__PACKAGE__"), InvocantText::CurrentPackage);
        assert_eq!(InvocantText::parse("shift"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("$_[0]"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("@_[0]"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("Foo::Bar"), InvocantText::Bareword("Foo::Bar"));
    }

    #[test]
    fn method_token_variants() {
        assert_eq!(MethodToken::parse("m"), MethodToken::Bare("m"));
        assert_eq!(MethodToken::parse("SUPER::m"), MethodToken::Super("m"));
        assert_eq!(MethodToken::parse("::m"), MethodToken::Main("m"));
        assert_eq!(
            MethodToken::parse("Foo::Bar::m"),
            MethodToken::Qualified { package: "Foo::Bar", name: "m" }
        );
        // SUPER is only the keyword when it is the WHOLE qualifier.
        assert_eq!(
            MethodToken::parse("Foo::SUPER::m"),
            MethodToken::Qualified { package: "Foo::SUPER", name: "m" }
        );
    }

    #[test]
    fn method_token_projections() {
        assert_eq!(MethodToken::parse("SUPER::m").name(), "m");
        assert_eq!(MethodToken::parse("Foo::Bar::m").literal_package(), Some("Foo::Bar"));
        assert_eq!(MethodToken::parse("::m").literal_package(), Some("main"));
        assert_eq!(MethodToken::parse("SUPER::m").literal_package(), None);
        assert_eq!(MethodToken::parse("m").literal_package(), None);
    }
}
