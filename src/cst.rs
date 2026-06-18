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
        node_to_span(*self)
    }
    fn named(&self) -> NamedChildren<'a> {
        NamedChildren { node: *self, idx: 0 }
    }
}

pub(crate) fn node_to_span(node: Node) -> Span {
    Span {
        start: node.start_position(),
        end: node.end_position(),
    }
}

/// The called name of a call node: the `method` field of a method call,
/// the `function` field of either function-call shape.
pub(crate) fn extract_call_name(node: Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "method_call_expression" => node.field_text("method", source).map(str::to_string),
        "function_call_expression" | "ambiguous_function_call_expression" => {
            node.field_text("function", source).map(str::to_string)
        }
        _ => None,
    }
}

/// Narrow `node`'s span to the bare tail after the last `::` in `text` (rule
/// #7): for a qualified name (`Foo::Bar::baz`) the renamable / highlightable
/// token is `baz`, not the whole path — so rename rewrites only the tail and
/// the qualifier survives, while the ref's `target_name` keeps the full path.
/// No `::` → the node's own span. FQ identifiers are single-line tokens, so
/// the tail column is `start.column + byte_offset_of_tail`.
pub(crate) fn fq_tail_span(node: Node, text: &str) -> Span {
    match text.rfind("::") {
        Some(idx) => {
            let s = node.start_position();
            Span {
                start: tree_sitter::Point { row: s.row, column: s.column + idx + 2 },
                end: node.end_position(),
            }
        }
        None => node_to_span(node),
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

/// Flatten a pair-list container's children into one token stream, splicing
/// EVERY nested `list_expression` / `parenthesized_expression` inline.
/// `a => 1, b => 2` parses with its tail pairs tucked into a right-associative
/// trailing `list_expression`, and explicit parens (`a => (b => c)`) add more
/// nesting — but both constructs are pure *grouping* in Perl: a bare list
/// always flattens into its surroundings (`key => (a, b)` IS `key, a, b`; the
/// grouped-value spelling is the ref `[a, b]`). So we descend them wherever
/// they appear, not just trailing, yielding the flat sibling sequence a
/// single linear scan can pair over. Ref literals (`[...]`, `{...}`) are NOT
/// groups and stay whole.
pub(crate) fn flatten_list<'a>(list: Node<'a>, out: &mut Vec<Node<'a>>) {
    let count = list.child_count();
    for i in 0..count {
        let Some(child) = list.child(i) else { continue };
        if matches!(child.kind(), "list_expression" | "parenthesized_expression") {
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

/// Canonical sigiled spelling of a variable node: `${ sner }` → `$sner`
/// (the grammar's `varname` child already excludes the braces). `None`
/// for deref spellings (`${$ref}` — the varname child wraps a `block`,
/// not a bare identifier) and non-variable nodes.
pub(crate) fn canonical_var_name<'a>(node: Node<'a>, src: &'a [u8]) -> Option<String> {
    let sigil = match node.kind() {
        "scalar" => '$',
        "array" => '@',
        "hash" => '%',
        _ => return None,
    };
    let vn = varname_child(node)?;
    if vn.named_child_count() > 0 {
        return None;
    }
    Some(format!("{}{}", sigil, vn.text(src)?))
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

/// Per-word `(text, span)` pairs of a `quoted_word_list`, multi-line
/// aware (rows/cols tracked through internal whitespace).
pub(crate) fn qw_word_spans(qw_node: Node, src: &[u8], results: &mut Vec<(String, Span)>) {
    for j in 0..qw_node.named_child_count() {
        let Some(sc) = qw_node.named_child(j) else { continue };
        if sc.kind() != "string_content" {
            continue;
        }
        let Ok(text) = sc.utf8_text(src) else { continue };
        let sc_start = sc.start_position();
        let bytes = text.as_bytes();
        let mut row = sc_start.row;
        let mut col = sc_start.column;
        let mut i = 0;
        while i < bytes.len() {
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                if bytes[i] == b'\n' {
                    row += 1;
                    col = 0;
                } else {
                    col += 1;
                }
                i += 1;
            }
            let word_start = tree_sitter::Point { row, column: col };
            let word_begin = i;
            while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
                col += 1;
                i += 1;
            }
            if i > word_begin {
                results.push((
                    text[word_begin..i].to_string(),
                    Span { start: word_start, end: tree_sitter::Point { row, column: col } },
                ));
            }
        }
    }
}

/// The `string_content` text of a quoted node — quote-flavor-agnostic.
pub(crate) fn string_content_text(node: Node, src: &[u8]) -> Option<String> {
    for i in 0..node.named_child_count() {
        if let Some(content) = node.named_child(i) {
            if content.kind() == "string_content" {
                return content.utf8_text(src).ok().map(|s| s.to_string());
            }
        }
    }
    None
}

/// Span of the `string_content` inside a quoted node (selection span);
/// the whole node when there is none (empty string).
pub(crate) fn string_content_span(node: Node) -> Span {
    for i in 0..node.named_child_count() {
        if let Some(content) = node.named_child(i) {
            if content.kind() == "string_content" {
                return node_to_span(content);
            }
        }
    }
    node_to_span(node)
}

/// Strings of a list-ish node — qw(), paren/comma lists, arrayrefs,
/// bare string literals — with per-word spans, INCLUDING the case
/// where `node` itself is the leaf (a parenless call's `arguments` IS
/// the qw/string node). Non-literal elements (bareword constants,
/// `@list` variables) go through `fold`: syntax lives here, constant
/// resolution stays with the caller that has the state for it.
/// `map "PREFIX$_", LIST` — the string-template map over a literal
/// list is statically foldable: each produced string is the template
/// with `$_` replaced by the list element, and its span is the
/// ELEMENT's own span, so per-name refs (role parents, package lists)
/// land on the word that produced them. Exactly one `$_` in the
/// template and `map` only (`grep` filters, it doesn't transform);
/// anything fancier is an honest miss. The crm idiom that motivated
/// it: `with map "Clove::Sheets::Roles::$_", qw/CSV DB/;`.
fn map_built_strings(
    node: Node,
    src: &[u8],
    fold: &mut dyn FnMut(Node) -> Vec<(String, Span)>,
) -> Vec<(String, Span)> {
    let Some(kw) = node.child(0) else { return vec![] };
    if kw.utf8_text(src).ok() != Some("map") {
        return vec![];
    }
    let Some(cb) = node.child_by_field_name("callback") else { return vec![] };
    if cb.kind() != "interpolated_string_literal" {
        return vec![];
    }
    let Some(content) = cb.named().find(|c| c.kind() == "string_content") else {
        return vec![];
    };
    let Ok(ctext) = content.utf8_text(src) else { return vec![] };
    let subs: Vec<Node> = content.named().filter(|c| c.kind() == "scalar").collect();
    let [topic] = subs.as_slice() else { return vec![] };
    if topic.utf8_text(src).ok() != Some("$_") {
        return vec![];
    }
    let pre_end = topic.start_byte() - content.start_byte();
    let post_start = topic.end_byte() - content.start_byte();
    let (pre, post) = (&ctext[..pre_end], &ctext[post_start..]);
    let Some(list) = node.child_by_field_name("list") else { return vec![] };
    string_list(list, src, fold)
        .into_iter()
        .map(|(t, sp)| (format!("{pre}{t}{post}"), sp))
        .collect()
}

pub(crate) fn string_list(
    node: Node,
    src: &[u8],
    fold: &mut dyn FnMut(Node) -> Vec<(String, Span)>,
) -> Vec<(String, Span)> {
    string_list_with_residue(node, src, fold).0
}

/// `string_list`, additionally reporting RESIDUE: `true` when at least
/// one list item did not fold to literal strings — a runtime-generated
/// element (`with ReportProxy(type => ...)`, an unresolvable bareword
/// constant, a `map` whose template we can't fold). Callers recording
/// structural facts (parent edges, export lists) use the flag to mark
/// the record incomplete instead of silently dropping the item and
/// presenting a partial list as the whole truth.
pub(crate) fn string_list_with_residue(
    node: Node,
    src: &[u8],
    fold: &mut dyn FnMut(Node) -> Vec<(String, Span)>,
) -> (Vec<(String, Span)>, bool) {
    match node.kind() {
        "quoted_word_list" => {
            let mut results = Vec::new();
            qw_word_spans(node, src, &mut results);
            return (results, false);
        }
        "string_literal" | "interpolated_string_literal" => {
            if let Some(text) = string_content_text(node, src) {
                return (vec![(text, string_content_span(node))], false);
            }
            return (vec![], true);
        }
        "bareword" | "autoquoted_bareword" | "array" => {
            let v = fold(node);
            let residue = v.is_empty();
            return (v, residue);
        }
        "map_grep_expression" => {
            let v = map_built_strings(node, src, fold);
            let residue = v.is_empty();
            return (v, residue);
        }
        _ => {}
    }
    let mut results = Vec::new();
    let mut residue = false;
    for i in 0..node.child_count() {
        let Some(child) = node.child(i) else { continue };
        match child.kind() {
            "quoted_word_list" => qw_word_spans(child, src, &mut results),
            "string_literal" | "interpolated_string_literal" => {
                if let Some(text) = string_content_text(child, src) {
                    results.push((text, string_content_span(child)));
                } else {
                    residue = true;
                }
            }
            "parenthesized_expression" | "list_expression" | "anonymous_array_expression" => {
                let (v, r) = string_list_with_residue(child, src, fold);
                results.extend(v);
                residue |= r;
            }
            "bareword" | "autoquoted_bareword" | "array" => {
                let v = fold(child);
                residue |= v.is_empty();
                results.extend(v);
            }
            "map_grep_expression" => {
                let v = map_built_strings(child, src, fold);
                residue |= v.is_empty();
                results.extend(v);
            }
            // Separators and parens are anonymous; anything NAMED we
            // didn't fold is a real list item we couldn't read.
            _ => {
                if child.is_named() && !matches!(child.kind(), "comment" | "pod") {
                    residue = true;
                }
            }
        }
    }
    (results, residue)
}

/// True when `node` sits in a conditionally-executed position within
/// its enclosing sub: under an if/unless block, a postfix modifier, a
/// ternary arm, a loop, or a short-circuit chain (`and`/`or`; the
/// `binary_expression` arm over-claims `&&`-and-friends' left operands
/// too — over-marking is the safe direction, it only widens a shape to
/// open). Climbs to the nearest sub/file boundary; scope-crossing
/// conditionality (a write inside a block or closure relative to an
/// outer variable's scope) is the caller's check — this answers only
/// what the syntax between here and the boundary says.
pub(crate) fn is_conditionally_executed(node: Node) -> bool {
    let mut cur = node.parent();
    while let Some(p) = cur {
        match p.kind() {
            "subroutine_declaration_statement"
            | "method_declaration_statement"
            | "anonymous_subroutine_expression"
            | "source_file" => return false,
            "conditional_statement"
            | "postfix_conditional_expression"
            | "conditional_expression"
            | "lowprec_logical_expression"
            | "binary_expression"
            | "loop_statement"
            | "for_statement"
            | "postfix_for_expression"
            | "postfix_loop_expression" => return true,
            _ => {}
        }
        cur = p.parent();
    }
    false
}

/// The scalar wrapped by a container node's deref varname (`@$h{…}` —
/// `slice_container_variable > varname > scalar`). `None` for plain
/// (non-deref) containers, whose varname is a leaf.
pub(crate) fn varname_inner_scalar_text<'a>(node: Node<'a>, src: &'a [u8]) -> Option<String> {
    for i in 0..node.named_child_count() {
        let c = node.named_child(i)?;
        if c.kind() != "varname" {
            continue;
        }
        for j in 0..c.named_child_count() {
            let gc = c.named_child(j)?;
            if gc.kind() == "scalar" {
                return gc.text(src).map(|s| s.to_string());
            }
        }
    }
    None
}

/// `$v->{k}` (arrow deref of the scalar's referent) vs `$foo{k}`
/// (element of `%foo` spelled with a `$` sigil): same node kind,
/// different variable — only the arrow form goes through the scalar.
/// The arrow is an anonymous token, so detect it in the source gap
/// between the container and the subscript.
pub(crate) fn element_arrow_deref(element: Node, src: &[u8]) -> bool {
    let Some(container) = element.named_child(0) else { return false };
    let Some(sub) = element
        .child_by_field_name("key")
        .or_else(|| element.child_by_field_name("index"))
    else {
        return false;
    };
    src.get(container.end_byte()..sub.start_byte())
        .is_some_and(|gap| gap.windows(2).any(|w| w == b"->"))
}

/// True when this node is the *container* of an element access —
/// `$c` in `$c->{k}` / `$c->[0]` / `$foo{k}` / `$foo[0]`. The element
/// expressions put the key/index in a named field; the container is the
/// other (first) named child. Element-access bases keep the reference
/// in hand; every other read position (call argument, RHS alias, list,
/// invocant, sigil deref) lets it escape to code that may mutate it.
pub(crate) fn is_element_access_base(node: Node) -> bool {
    let Some(parent) = node.parent() else { return false };
    match parent.kind() {
        "hash_element_expression" | "array_element_expression" => {
            parent.named_child(0).is_some_and(|c| c == node)
        }
        // Postfix deref slice (`$h->@{…}` / `$h->%{…}` / `$a->@[…]`):
        // the scalar fills the hashref/arrayref field. A slice READ
        // copies values out — it can't mutate the referent.
        "slice_expression" | "keyval_expression" => {
            parent.child_by_field_name("hashref").is_some_and(|c| c == node)
                || parent.child_by_field_name("arrayref").is_some_and(|c| c == node)
        }
        // Sigil-deref slice (`@$h{…}`): the scalar sits under the
        // container's varname. Plain sigil derefs (`%$h`, `$$h`) keep
        // their escape classification — only slice containers are
        // value-copying reads.
        "varname" => parent.parent().is_some_and(|gp| {
            matches!(
                gp.kind(),
                "slice_container_variable" | "keyval_container_variable"
            )
        }),
        _ => false,
    }
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
