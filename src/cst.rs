//! Typed view over the tree-sitter-perl CST.
//!
//! The sanctioned tree consumers (`builder.rs`, `cursor_context.rs`, plugin
//! emit hooks) speak to tree-sitter through this module instead of raw
//! `child_by_field_name` / `kind() == "..."` pokes. The point is that each
//! grammar trap is encoded exactly once:
//!
//! - `=>` is a comma that autoquotes its LHS — pair walking
//!   ([`pair_nodes`]) is separator-agnostic by construction, so the
//!   plain-comma spelling can't be silently dropped.
//! - tree-sitter-perl nests pair tails right-associatively (`a => 1, b => 2`
//!   parses as `a, 1, (b, 2)`) — [`flatten_list`] splices the trailing
//!   wrapper so a linear scan sees every pair.
//! - call arguments may arrive as a bare node or a `list_expression` /
//!   `parenthesized_expression` — [`call_args`] always yields the flat
//!   positional sequence.
//!
//! Typed wrappers are declared with [`typed_node!`]; accessors return what
//! the grammar *means*. Name-level Perl conventions (constructor names,
//! conventional invocants) live in `conventions.rs` — this module re-exports
//! them so tree-side callers have one import.

use tree_sitter::Node;

use crate::file_analysis::Span;

pub(crate) use crate::conventions::is_conventional_invocant_name;

/// Ergonomic accessors every tree consumer wants; `use crate::cst::NodeExt`
/// instead of re-spelling `utf8_text(..).ok()` chains.
pub(crate) trait NodeExt<'a>: Sized {
    /// The node's source text. `None` only on invalid UTF-8.
    fn text(&self, src: &'a [u8]) -> Option<&'a str>;
    /// Text of a named field's child.
    fn field_text(&self, field: &str, src: &'a [u8]) -> Option<&'a str>;
    /// The node's span, in `FileAnalysis` coordinates.
    fn span(&self) -> Span;
    /// Named children, by index (no `TreeCursor` borrow gymnastics).
    fn named(&self) -> NamedChildren<'a>;
}

impl<'a> NodeExt<'a> for Node<'a> {
    fn text(&self, src: &'a [u8]) -> Option<&'a str> {
        self.utf8_text(src).ok()
    }
    fn field_text(&self, field: &str, src: &'a [u8]) -> Option<&'a str> {
        self.child_by_field_name(field).and_then(|n| n.text(src))
    }
    fn span(&self) -> Span {
        crate::file_analysis::node_to_span(*self)
    }
    fn named(&self) -> NamedChildren<'a> {
        NamedChildren { node: *self, idx: 0 }
    }
}

pub(crate) struct NamedChildren<'a> {
    node: Node<'a>,
    idx: usize,
}

impl<'a> Iterator for NamedChildren<'a> {
    type Item = Node<'a>;
    fn next(&mut self) -> Option<Node<'a>> {
        while self.idx < self.node.named_child_count() {
            let i = self.idx;
            self.idx += 1;
            if let Some(c) = self.node.named_child(i) {
                return Some(c);
            }
        }
        None
    }
}

/// Declare a zero-copy typed wrapper for one grammar node kind. Generated
/// surface: `cast(node)` (kind-checked), `node()`, and one
/// `Option<Node>`-returning accessor per listed field.
macro_rules! typed_node {
    ($(#[$meta:meta])* $name:ident($kind:literal) { $($(#[$fmeta:meta])* $field:ident),* $(,)? }) => {
        $(#[$meta])*
        #[derive(Clone, Copy)]
        pub(crate) struct $name<'a>(Node<'a>);

        #[allow(dead_code)]
        impl<'a> $name<'a> {
            pub(crate) fn cast(node: Node<'a>) -> Option<Self> {
                (node.kind() == $kind).then_some(Self(node))
            }
            pub(crate) fn node(&self) -> Node<'a> {
                self.0
            }
            $(
                $(#[$fmeta])*
                pub(crate) fn $field(&self) -> Option<Node<'a>> {
                    self.0.child_by_field_name(stringify!($field))
                }
            )*
        }
    };
}

typed_node! {
    /// `$obj->method(args)` / `$obj->$dyn(...)` / `Class->Pkg::method`.
    MethodCall("method_call_expression") {
        invocant,
        method,
        arguments,
    }
}

typed_node! {
    /// `name(args)` — the parenthesized call form.
    FunctionCall("function_call_expression") {
        function,
        arguments,
    }
}

typed_node! {
    /// `name args` — no parens; also `bless { ... }`.
    AmbiguousFunctionCall("ambiguous_function_call_expression") {
        function,
        arguments,
    }
}

/// A call expression's arguments as a flat positional sequence. The
/// `arguments` field may be a bare single node or a `list_expression` /
/// `parenthesized_expression` wrapper — callers never see the difference.
pub(crate) fn call_args<'a>(call_node: Node<'a>) -> Vec<Node<'a>> {
    let Some(args) = call_node.child_by_field_name("arguments") else {
        return Vec::new();
    };
    if matches!(args.kind(), "list_expression" | "parenthesized_expression") {
        args.named().collect()
    } else {
        vec![args]
    }
}

/// Flatten a pair-list container's children into one token stream,
/// descending tree-sitter-perl's right-associative nesting: `a => 1, b => 2`
/// parses as `a, =>, 1, (b, =>, 2)` — the tail pairs hide inside a nested
/// `list_expression`. The nested list's children are spliced inline so a
/// single linear scan sees every pair. A `list_expression` that is itself a
/// *value* (`key => (a, b)`) only nests when it's the last child, so
/// descending the trailing wrapper is safe — a non-trailing list is a
/// genuine multi-element value and is kept whole.
pub(crate) fn flatten_list<'a>(list: Node<'a>, out: &mut Vec<Node<'a>>) {
    let count = list.child_count();
    for i in 0..count {
        let Some(child) = list.child(i) else { continue };
        if child.kind() == "list_expression" && i + 1 == count {
            flatten_list(child, out);
        } else {
            out.push(child);
        }
    }
}

/// Walk a flat sibling sequence as positional `(key_node, value_node)`
/// pairs, separator-agnostic: the `,` / `=>` between tokens is skipped by
/// position, never matched. This is THE pairing primitive — gating pair
/// walking on the `fat_comma` node silently drops the plain-comma spelling
/// (`{ 'GAMMA', 3 }` is identical to `{ GAMMA => 3 }`).
pub(crate) fn pair_nodes_in<'a>(children: &[Node<'a>]) -> Vec<(Node<'a>, Node<'a>)> {
    let mut out = Vec::new();
    let count = children.len();
    let mut i = 0;
    while i < count {
        let k_node = children[i];
        i += 1;
        if !k_node.is_named() {
            continue;
        }
        // Skip the comma / fat comma to the next named node (the value).
        let val = loop {
            match children.get(i) {
                Some(c) if c.is_named() => break Some(*c),
                Some(_) => i += 1,
                None => break None,
            }
        };
        let Some(val) = val else { break };
        i += 1; // step past the value so the next key isn't read off it
        out.push((k_node, val));
    }
    out
}

/// Pair-walk a container node: a bare `list_expression` /
/// `parenthesized_expression`, or an `anonymous_hash_expression` (its inner
/// list is unwrapped). Composition of [`flatten_list`] + [`pair_nodes_in`].
pub(crate) fn pair_nodes<'a>(container: Node<'a>) -> Vec<(Node<'a>, Node<'a>)> {
    let list = if container.kind() == "anonymous_hash_expression" {
        container
            .named()
            .find(|c| c.kind() == "list_expression")
            .unwrap_or(container)
    } else {
        container
    };
    let mut children = Vec::new();
    flatten_list(list, &mut children);
    pair_nodes_in(&children)
}

/// The `varname` child of a variable node (`scalar` / `array` / `hash` /
/// `container_variable`). For nontrivial derefs (`${$h{k}}`) the child is a
/// `block`, not a `varname` — this returns `None` there, so a `Some` is
/// always a simple identifier.
pub(crate) fn varname_child<'a>(node: Node<'a>) -> Option<Node<'a>> {
    node.named().find(|c| c.kind() == "varname")
}

/// Canonical variable name for a container access: `$foo[0]` reads `@foo`,
/// `$foo{k}` reads `%foo`, `@foo{...}` slices `%foo`. Resolves the access
/// node + its parent's shape to the *declared* variable's sigil + bare name.
/// `None` = not a container-element access (caller keeps the raw text).
pub(crate) fn canonical_container_name<'a>(node: Node<'a>, src: &'a [u8]) -> Option<String> {
    let parent = node.parent()?;
    let bare = varname_child(node)?.text(src)?;
    let target_sigil: char = match parent.kind() {
        "array_element_expression" => '@',
        "hash_element_expression" => '%',
        "slice_expression" | "keyval_expression" => {
            if parent.child_by_field_name("array").is_some_and(|c| c == node) {
                '@'
            } else if parent.child_by_field_name("hash").is_some_and(|c| c == node) {
                '%'
            } else {
                return None;
            }
        }
        _ => return None,
    };
    Some(format!("{}{}", target_sigil, bare))
}

/// `Class->new(...)` — a constructor call with a class-shaped invocant.
/// Returns the invocant text (`Foo::Bar` or `__PACKAGE__`); `None` for
/// non-constructor methods and variable/positional invocants, whose class
/// comes from inference. The caller resolves `__PACKAGE__` — the enclosing
/// package is builder state this layer doesn't hold.
pub(crate) fn constructor_invocant<'a>(node: Node<'a>, src: &'a [u8]) -> Option<&'a str> {
    use crate::conventions::InvocantText;
    let call = MethodCall::cast(node)?;
    if !call
        .method()?
        .text(src)
        .is_some_and(crate::conventions::is_constructor_name)
    {
        return None;
    }
    let inv = call.invocant()?.text(src)?;
    match InvocantText::parse(inv) {
        InvocantText::Bareword(_) | InvocantText::CurrentPackage => Some(inv),
        InvocantText::Scalar(_)
        | InvocantText::NonScalar(_)
        | InvocantText::PositionalReceiver => None,
    }
}

/// True when `node` is a `scalar` whose bare varname is a conventional
/// invocant (`$self` / `$class` / `$this` / `$proto`), matching braced
/// spellings (`${self}`) and rejecting derefs (`${$ref}`) by reading the
/// canonical varname child instead of raw text. The node-level half of
/// "is this potentially the receiver" — position-based receiver detection
/// (`$_[0]`, `shift`) stays with the builder, which knows the sub context.
pub(crate) fn is_conventional_invocant_scalar<'a>(node: Node<'a>, src: &'a [u8]) -> bool {
    node.kind() == "scalar"
        && varname_child(node)
            .and_then(|v| v.text(src))
            .is_some_and(is_conventional_invocant_name)
}
