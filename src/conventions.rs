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

/// A method-call invocant in canonical spelling: variable invocants are
/// sigil + bare varname (`${ sner }` stores as `$sner`, via the grammar's
/// `varname` child), `__PACKAGE__` resolved to the enclosing package,
/// anything else raw expression text. The newtype exists so a raw
/// `node.utf8_text()` can't be slotted into an invocant field by
/// accident — every producer either goes through the builder's
/// canonicalizing path or owns the claim with [`assume_canonical`].
///
/// [`assume_canonical`]: InvocantName::assume_canonical
#[derive(Debug, Clone, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct InvocantName(String);

impl InvocantName {
    /// The caller asserts the text is already canonical: plugin manifests
    /// declaring a literal receiver class, synthesized refs spelled
    /// `$self`, tests. Named so the assertion is grep-able — there is
    /// deliberately no blanket `From<String>`.
    pub fn assume_canonical(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn classify(&self) -> InvocantText<'_> {
        InvocantText::parse(&self.0)
    }
}

impl std::ops::Deref for InvocantName {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl PartialEq<str> for InvocantName {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<&str> for InvocantName {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl std::fmt::Display for InvocantName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// The text of a method-call invocant, classified once. Consumers match
/// the variant instead of re-deriving the shape with sigil/keyword string
/// checks at each site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvocantText<'a> {
    /// `$obj` — a scalar variable, carried WITHOUT its sigil (only a
    /// scalar can dispatch, so the `$` is content-free). Its class comes
    /// from inference, never from the spelling — bag lookups key on the
    /// original sigiled text the caller still holds.
    Scalar(&'a str),
    /// `__PACKAGE__` — the enclosing package.
    CurrentPackage,
    /// `shift` / `$_[0]` / `@_[0]` — the method's own receiver argument
    /// read positionally (`my $self = shift`); resolves to the enclosing
    /// class. Not real variables — the bag has no witness for them.
    PositionalReceiver,
    /// `@list` / `%h` — not a legal invocant (Perl methods dispatch on a
    /// scalar or a class), but tree-sitter-perl's tolerant grammar still
    /// parses `@list->m` as a method call, and mid-edit completion text
    /// can spell anything. Unresolvable by construction; consumers
    /// answer `None`, never a class.
    NonScalar(&'a str),
    /// Anything else — a bareword: a class name, or a class-returning
    /// zero-arg sub (`app->routes`).
    Bareword(&'a str),
}

impl<'a> InvocantText<'a> {
    /// Classify invocant text. Callers with a node in hand canonicalize
    /// FIRST (`cst::canonical_var_name` — the grammar's `varname` child
    /// already strips `${...}` brace spellings); this never re-derives
    /// node structure from text.
    pub fn parse(text: &'a str) -> Self {
        match text {
            t if is_current_package_token(t) => Self::CurrentPackage,
            "shift" | "$_[0]" | "@_[0]" => Self::PositionalReceiver,
            t if t.starts_with('$') => Self::Scalar(&t[1..]),
            t if t.starts_with('@') || t.starts_with('%') => Self::NonScalar(&t[1..]),
            t => Self::Bareword(t),
        }
    }

    /// True when the invocant is a hash/array **element place** — `$h{k}`,
    /// `$a[0]`, `$h{a}{b}`, or the arrow forms `$x->{k}` / `$x->[0]` — i.e.
    /// a stable slot that flow-narrowing may have refined, so the receiver
    /// query consults a place witness for it. A scalar **deref** (`${$ref}`,
    /// `${name}`) is NOT a place: its brace follows the sigil directly,
    /// whereas a place's subscript follows the container name (or an arrow /
    /// a prior subscript). Plain scalars and non-scalars are never places.
    pub fn is_element_place(&self) -> bool {
        let Self::Scalar(inner) = self else { return false };
        // `inner` is the text after the `$` sigil. A deref spelling leads
        // with the brace (`${…}` → inner `"{…}"`); a place has its container
        // name / `->` / prior subscript before the first `{` or `[`.
        let b = inner.as_bytes();
        b.iter().position(|&c| c == b'{' || c == b'[').is_some_and(|i| {
            i > 0 && !matches!(b[i - 1], b'$' | b'@' | b'%')
        })
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
        // Scalar carries the bare name — the sigil is content-free since
        // only a scalar can dispatch.
        assert_eq!(InvocantText::parse("$obj"), InvocantText::Scalar("obj"));
        assert_eq!(InvocantText::parse("@list"), InvocantText::NonScalar("list"));
        assert_eq!(InvocantText::parse("%h"), InvocantText::NonScalar("h"));
        assert_eq!(InvocantText::parse("__PACKAGE__"), InvocantText::CurrentPackage);
        assert_eq!(InvocantText::parse("shift"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("$_[0]"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("@_[0]"), InvocantText::PositionalReceiver);
        assert_eq!(InvocantText::parse("Foo::Bar"), InvocantText::Bareword("Foo::Bar"));
    }

    #[test]
    fn element_place_vs_deref() {
        // Element places: subscript follows the container name / arrow.
        for place in ["$h{k}", "$a[0]", "$h{a}{b}", "$self->{x}", "$self->[0]"] {
            assert!(
                InvocantText::parse(place).is_element_place(),
                "{place} should be an element place",
            );
        }
        // Scalar derefs and plain vars are NOT places — `${...}`'s brace
        // follows the sigil directly.
        for not_place in ["${name}", "${$ref}", "$obj", "@list", "Foo::Bar", "shift"] {
            assert!(
                !InvocantText::parse(not_place).is_element_place(),
                "{not_place} should NOT be an element place",
            );
        }
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
