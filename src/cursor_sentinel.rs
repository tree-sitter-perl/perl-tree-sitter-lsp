//! Sentinel reparse for member-access cursor context — the pack-language
//! member-completion seam (`docs/adr/cursor-context-completion.md`).
//!
//! A member of the reparse family (`cpp_reparse.rs`, `reparse.rs`): a
//! **source edit + reparse + span remap**. The others fix a parse
//! corrupted by a *declaration* (a macro, a prototype). This one fixes a
//! parse corrupted by *incompleteness*: at the instant a user triggers
//! completion the buffer reads `box.` / `box->` / `obj.` — a member access
//! with no member, so tree-sitter produces an ERROR and the receiver is no
//! longer reachable through the typed `field_expression` / `attribute`
//! shape the rest of the engine speaks.
//!
//! The fix is the same shape as expansion: splice a placeholder identifier
//! (`__CURSOR__`) at the cursor so the access becomes syntactically
//! complete, reparse, locate the placeholder, and take its member node's
//! receiver. This module does only step (a) — detect member access +
//! identify the receiver; the backend feeds the receiver span to
//! `expr_type_at_span` + `complete_members_for_class` for (b)/(c).
//!
//! Coordinate remap is trivial here, and that is the whole appeal: the
//! splice lands AT the cursor, strictly AFTER the receiver, so every
//! receiver byte offset is identical in patched and original source — the
//! one anchor the other reparse siblings must carry, this one gets free.
//! Language config is a two-field table (rule #10): the member-access node
//! kinds and the don't-splice-here set are the only facts that vary.

use crate::file_analysis::{
    expected_member_op, CrossFileLookup, FileAnalysis, InferredType, Span,
};
use tree_sitter::{InputEdit, Node, Parser, Point, Tree};

/// The placeholder spliced at the cursor to complete a dangling member
/// access. Chosen to be a legal identifier in every C-family / Python
/// grammar and vanishingly unlikely to collide with real source.
pub const SENTINEL: &str = "__CURSOR__";

/// Per-language facts the sentinel walk needs. Everything else is
/// grammar-agnostic. `member_kinds` are the node kinds that model
/// `receiver OP member`; in all our grammars the receiver is the first
/// named child (`receiver_first`), so we don't hard-code field names.
#[derive(Clone, Copy)]
pub struct LangCfg {
    pub member_kinds: &'static [&'static str],
    /// Node kinds we must NOT splice into (string/char/comment).
    pub skip_kinds: &'static [&'static str],
    /// Transparent wrappers a receiver peels through to the same CLASS:
    /// `(expr)`, `*p` (deref), `&obj` (address-of) all reach the operand's
    /// members (pointer-/reference-ness is dropped for member resolution).
    pub wrapper_kinds: &'static [&'static str],
}

pub const CPP: LangCfg = LangCfg {
    // `box.m` and `box->m` both parse as field_expression.
    member_kinds: &["field_expression"],
    skip_kinds: &["string_literal", "char_literal", "raw_string_literal", "comment"],
    // `(*p).m`, `(&o)->m`: paren wrap + `*`/`&` are `pointer_expression`.
    wrapper_kinds: &["parenthesized_expression", "pointer_expression"],
};

pub const PYTHON: LangCfg = LangCfg {
    member_kinds: &["attribute"],
    skip_kinds: &["string", "string_content", "comment", "concatenated_string"],
    wrapper_kinds: &["parenthesized_expression"],
};

/// Per-language sentinel config by driver id. `None` = no member-access
/// cursor context for this language (in-scope completion only).
pub fn lang_cfg(language: &str) -> Option<&'static LangCfg> {
    match language {
        "cpp" => Some(&CPP),
        "python" => Some(&PYTHON),
        _ => None,
    }
}

/// Byte offset of a `Point` in `src` (Point.column is a byte offset
/// within its row). The inverse of `byte_to_point`.
pub fn point_to_byte(src: &str, point: Point) -> usize {
    let (mut row, mut col) = (0usize, 0usize);
    for (i, ch) in src.char_indices() {
        if row == point.row && col == point.column {
            return i;
        }
        if ch == '\n' {
            row += 1;
            col = 0;
        } else {
            col += ch.len_utf8();
        }
    }
    src.len()
}

/// The receiver of a dangling member access, recovered by sentinel
/// re-parse.
#[derive(Debug, Clone, PartialEq)]
pub struct Receiver {
    /// Receiver source text (`box`, `obj`, `a.b`, `make()` ...).
    pub text: String,
    /// Receiver span in ORIGINAL (unpatched) byte coordinates. Equal to
    /// the patched coordinates because the splice is at/after `end`.
    pub start: usize,
    pub end: usize,
    /// The access operator the sentinel completed: `->` (true) vs `.`.
    pub arrow: bool,
}

/// Splice the sentinel at `cursor` (a byte offset into `src`) and return
/// the patched buffer. The cursor is expected to sit just after a `.` /
/// `->` (possibly with trailing whitespace / a partial member already
/// typed — the caller strips that; here we patch exactly at `cursor`).
pub fn patch(src: &str, cursor: usize) -> String {
    let mut out = String::with_capacity(src.len() + SENTINEL.len());
    out.push_str(&src[..cursor]);
    out.push_str(SENTINEL);
    out.push_str(&src[cursor..]);
    out
}

/// True when `cursor` lands inside a string/char/comment in the ORIGINAL
/// parse — splicing there would be a no-op at best, corruption at worst.
fn cursor_in_skip(orig: &Tree, src: &str, cursor: usize, cfg: &LangCfg) -> bool {
    // Probe one byte back: at `box.` the cursor is past the `.`, but a
    // cursor that is literally inside `"foo`| sits within the string.
    let probe = cursor.saturating_sub(1).min(src.len().saturating_sub(1));
    let Some(node) = orig
        .root_node()
        .descendant_for_byte_range(probe, probe)
    else {
        return false;
    };
    let mut n = Some(node);
    while let Some(x) = n {
        if cfg.skip_kinds.contains(&x.kind()) {
            return true;
        }
        n = x.parent();
    }
    false
}

/// The whole spike: patch a sentinel at `cursor`, re-parse, and return
/// the receiver of the member access the sentinel completed. `None` when
/// the cursor is not at a member access (or sits in a string/comment).
///
/// `parser` must already be set to the target language; `cfg` selects
/// the per-language node vocabulary.
pub fn receiver_at(
    parser: &mut Parser,
    cfg: &LangCfg,
    src: &str,
    cursor: usize,
) -> Option<Receiver> {
    let orig = parser.parse(src, None)?;
    if cursor_in_skip(&orig, src, cursor, cfg) {
        return None;
    }
    let patched = patch(src, cursor);
    let tree = parser.parse(&patched, None)?;
    let node = find_sentinel(tree.root_node(), &patched, cursor)?;
    let member = climb_to_member(node, cfg)?;
    receiver_of(member, &patched, cursor)
}

/// Find the freshly-spliced sentinel identifier node. It begins exactly
/// at `cursor` and its text is `SENTINEL`. We descend to the smallest
/// node covering the sentinel byte range and accept it (or its first
/// matching ancestor) whose text equals the sentinel.
fn find_sentinel<'a>(root: Node<'a>, patched: &str, cursor: usize) -> Option<Node<'a>> {
    let end = cursor + SENTINEL.len();
    let mut node = root.descendant_for_byte_range(cursor, end)?;
    // Descend / climb to the exact identifier token carrying the text.
    loop {
        if node.utf8_text(patched.as_bytes()).ok() == Some(SENTINEL) && node.child_count() == 0 {
            return Some(node);
        }
        // Try a named child that exactly covers the sentinel.
        let mut found = None;
        let mut cur = node.walk();
        for c in node.named_children(&mut cur) {
            if c.start_byte() == cursor && c.end_byte() == end {
                found = Some(c);
                break;
            }
        }
        node = found?;
    }
}

/// Walk up from the sentinel to the member-access node that owns it.
fn climb_to_member<'a>(node: Node<'a>, cfg: &LangCfg) -> Option<Node<'a>> {
    let mut n = node;
    for _ in 0..6 {
        let parent = n.parent()?;
        if cfg.member_kinds.contains(&parent.kind()) {
            return Some(parent);
        }
        n = parent;
    }
    None
}

/// Extract the receiver (first named child) of a member-access node and
/// map its span back to original coordinates. Since the receiver lies
/// entirely before `cursor`, patched and original offsets coincide.
fn receiver_of(member: Node, patched: &str, cursor: usize) -> Option<Receiver> {
    let receiver = member.named_child(0)?;
    // Defensive: the receiver must end at or before the splice site.
    if receiver.end_byte() > cursor {
        return None;
    }
    let arrow = member_uses_arrow(member, patched);
    Some(Receiver {
        text: receiver.utf8_text(patched.as_bytes()).ok()?.to_string(),
        start: receiver.start_byte(),
        end: receiver.end_byte(),
        arrow,
    })
}

/// The `.`/`->` operator token of a member-access node (the anonymous
/// child between the two named children). It lies entirely before the
/// splice site, so its span is identical in patched and original source.
fn operator_token<'a>(member: Node<'a>, patched: &str) -> Option<Node<'a>> {
    let mut cur = member.walk();
    let found = member.children(&mut cur).find(|c| {
        !c.is_named() && matches!(c.utf8_text(patched.as_bytes()), Ok(".") | Ok("->"))
    });
    found
}

/// Detect `->` vs `.` by scanning the member node's anonymous children.
fn member_uses_arrow(member: Node, patched: &str) -> bool {
    operator_token(member, patched).map(|t| t.utf8_text(patched.as_bytes()) == Ok("->")).unwrap_or(false)
}

/// Incremental variant: reuse a cached `old` tree of the unpatched
/// buffer. The splice is a pure insertion at `cursor`, so the
/// `InputEdit` is exact and tree-sitter re-parses only the damaged
/// region around the cursor — the cost completion actually pays per
/// keystroke once the document tree is already in hand (it always is:
/// `document.rs` keeps it). Same recovery, a fraction of the work.
pub fn receiver_at_incremental(
    parser: &mut Parser,
    cfg: &LangCfg,
    src: &str,
    old: &Tree,
    cursor: usize,
) -> Option<Receiver> {
    if cursor_in_skip(old, src, cursor, cfg) {
        return None;
    }
    let patched = patch(src, cursor);
    let mut edited = old.clone();
    let pos = byte_to_point(src, cursor);
    edited.edit(&InputEdit {
        start_byte: cursor,
        old_end_byte: cursor,
        new_end_byte: cursor + SENTINEL.len(),
        start_position: pos,
        old_end_position: pos,
        new_end_position: Point::new(pos.row, pos.column + SENTINEL.len()),
    });
    let tree = parser.parse(&patched, Some(&edited))?;
    let node = find_sentinel(tree.root_node(), &patched, cursor)?;
    let member = climb_to_member(node, cfg)?;
    receiver_of(member, &patched, cursor)
}

/// The completion context at a dangling member access: the receiver's type
/// (for listing members) plus, when the typed operator disagrees with the
/// receiver's pointer depth, the single-level `.`↔`->` fix to swap it. One
/// reparse serves both — completion pays it once per keystroke.
///
/// `op_fix = Some((span, text))` means "replace the operator token at `span`
/// with `text`". `None` when the operator is already correct, the receiver
/// isn't a simple variable, or the depth is DEEP (`Box**` → `(*pp)->`, an
/// expression wrap we don't auto-apply — members still complete, show-only).
pub struct MemberCompletionCtx {
    pub receiver_type: Option<InferredType>,
    pub op_fix: Option<(Span, String)>,
}

pub fn member_completion_ctx_incremental(
    parser: &mut Parser,
    cfg: &LangCfg,
    src: &str,
    old: &Tree,
    cursor: usize,
    analysis: &FileAnalysis,
    module_index: Option<&dyn CrossFileLookup>,
) -> Option<MemberCompletionCtx> {
    if cursor_in_skip(old, src, cursor, cfg) {
        return None;
    }
    let patched = patch(src, cursor);
    let mut edited = old.clone();
    let pos = byte_to_point(src, cursor);
    edited.edit(&InputEdit {
        start_byte: cursor,
        old_end_byte: cursor,
        new_end_byte: cursor + SENTINEL.len(),
        start_position: pos,
        old_end_position: pos,
        new_end_position: Point::new(pos.row, pos.column + SENTINEL.len()),
    });
    let tree = parser.parse(&patched, Some(&edited))?;
    let node = find_sentinel(tree.root_node(), &patched, cursor)?;
    let member = climb_to_member(node, cfg)?;
    let receiver = member.named_child(0)?;
    let receiver_type = resolve_node_type(receiver, cfg, &patched, analysis, module_index);
    let op_fix = operator_fix(member, receiver, &patched, analysis);
    Some(MemberCompletionCtx { receiver_type, op_fix })
}

/// The operator correction for a member access whose receiver is a simple
/// variable. Drives entirely off the receiver's `deref_stack` (rule #10):
/// the depth picks the expected operator, and we offer the swap only when
/// it differs from what was typed AND a single token expresses it.
fn operator_fix(
    member: Node,
    receiver: Node,
    patched: &str,
    analysis: &FileAnalysis,
) -> Option<(Span, String)> {
    if receiver.kind() != "identifier" {
        return None; // only simple-variable receivers carry a resolvable stack
    }
    let name = receiver.utf8_text(patched.as_bytes()).ok()?;
    let stack = analysis.var_deref_stack_at(name, receiver.start_position())?;
    let expected = expected_member_op(stack)?; // None = DEEP → show-only
    let op = operator_token(member, patched)?;
    let typed_arrow = op.utf8_text(patched.as_bytes()) == Ok("->");
    let expected_arrow = expected == crate::file_analysis::MemberOp::Arrow;
    if typed_arrow == expected_arrow {
        return None; // already correct
    }
    let span = Span { start: op.start_position(), end: op.end_position() };
    Some((span, expected.as_str().to_string()))
}

/// Type a receiver node. A member-access node (`field_expression` /
/// `attribute`) is field-on-class — recurse the base, look the field up on
/// its class; anything else (an identifier, a call) resolves by its exact
/// span through the bag (`expr_type_at_span`).
fn resolve_node_type(
    node: Node,
    cfg: &LangCfg,
    src: &str,
    analysis: &FileAnalysis,
    module_index: Option<&dyn CrossFileLookup>,
) -> Option<InferredType> {
    // a member ACCESS `recv.field` — field-on-class.
    if cfg.member_kinds.contains(&node.kind()) {
        let base = node.named_child(0)?;
        let field = node.named_child(node.named_child_count() - 1)?;
        let class = resolve_node_type(base, cfg, src, analysis, module_index)?.class_name()?.to_string();
        let field_name = field.utf8_text(src.as_bytes()).ok()?;
        return analysis.field_type_on_class(&class, field_name, module_index);
    }
    // a method CALL `recv.method(...)` — the method's return on the
    // receiver's class, resolved through MethodOnClass (inheritance +
    // cross-file flow through the same chase, no special-casing).
    if node.kind() == "call_expression" {
        let func = node.child_by_field_name("function")?;
        if cfg.member_kinds.contains(&func.kind()) {
            let recv = func.named_child(0)?;
            let method = func.named_child(func.named_child_count() - 1)?;
            let class = resolve_node_type(recv, cfg, src, analysis, module_index)?
                .class_name()?
                .to_string();
            let method_name = method.utf8_text(src.as_bytes()).ok()?;
            return analysis.find_method_return_type(&class, method_name, module_index, None);
        }
        return None;
    }
    // Transparent wrappers — `(expr)`, `*p`, `&obj` — denote the same class
    // as their operand (pointer-/reference-ness dropped). Peel and recurse so
    // `(*p).m` / `(&o)->m` reach the members `p->m` does.
    if cfg.wrapper_kinds.contains(&node.kind()) {
        return resolve_node_type(node.named_child(0)?, cfg, src, analysis, module_index);
    }
    let span = Span { start: node.start_position(), end: node.end_position() };
    analysis.expr_type_at_span(span, module_index)
}

fn byte_to_point(src: &str, byte: usize) -> Point {
    let mut row = 0;
    let mut col = 0;
    for (i, ch) in src.char_indices() {
        if i >= byte {
            break;
        }
        if ch == '\n' {
            row += 1;
            col = 0;
        } else {
            col += ch.len_utf8();
        }
    }
    Point::new(row, col)
}

#[cfg(test)]
#[path = "cursor_sentinel_tests.rs"]
mod tests;
