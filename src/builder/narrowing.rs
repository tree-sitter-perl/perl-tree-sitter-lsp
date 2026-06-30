//! Flow-sensitive narrowing — guard recognition + span-scoped emission.
//!
//! A child module of `builder` (so it keeps direct access to `Builder`'s
//! private fields) rather than a sibling: narrowing is still part of the
//! single tree-sitter consumer (rule #1) — `build()` drives it, it just
//! lives in its own file. Recognition (`recognize_*`) is pure CST
//! inspection; emission + span/truncation are `Builder` methods.
//! Decisions (engine-is-emission, truncation soundness, polarity +
//! the `Undef` negative lattice): `docs/adr/flow-narrowing.md`.

use tree_sitter::{Node, Point};

use crate::cst::node_to_span;
use crate::file_analysis::{InferredType, ScopeId, Span};

use super::{connector_keyword_between, point_lt, raw_leading_op, raw_mid_op, Builder};

/// The subject a guard narrows — a plain scalar, or a hash/array place
/// (`$self->{x}`, `$h{k}`, `$self->{$k}`) keyed by its source spelling.
/// `key_vars` are the place's dynamic-key scalars (`$k` in `$self->{$k}`);
/// reassigning any of them ends the narrowing region.
pub(super) enum NarrowSubject {
    Variable(String),
    Place { key: String, root: String, key_vars: Vec<String> },
}

/// Classify a guard's operand into the subject it narrows: a plain
/// scalar (`Variable`), else a stable place path (`Place`).
fn narrow_subject_of(node: Node, src: &[u8]) -> Option<NarrowSubject> {
    if let Some(v) = crate::cst::canonical_var_name(node, src) {
        return Some(NarrowSubject::Variable(v));
    }
    let (key, root) = crate::cst::canonical_place_path(node, src)?;
    let key_vars = crate::cst::place_dynamic_key_vars(node, src);
    Some(NarrowSubject::Place { key, root, key_vars })
}

/// What a guard does to the subject's type where it holds.
#[derive(Clone)]
pub(super) enum NarrowOp {
    /// `isa` / `ref…eq` prove a concrete type.
    To(InferredType),
    /// `defined` / `blessed` strip `Optional<T>` to `T`. The strip reads
    /// the subject's incoming type, so it can only run once that type has
    /// converged (a re-emittable fold pass); `query_point` is the subject's
    /// location IN THE GUARD — before the narrowed region, so the read
    /// sees the un-narrowed `Optional`, not the pass's own output.
    StripOptional { query_point: Point },
}

impl NarrowOp {
    /// The op for the region where the guard is FALSE. Only `defined`/
    /// `blessed` have a representable complement — the subject is `undef`
    /// there. "Not a class" (`isa`/`ref-eq` negated) has no positive
    /// target, so it self-suppresses (`None` → emit nothing, wide type
    /// wins). See `docs/adr/flow-narrowing.md` negative-polarity rows.
    fn negated(&self) -> Option<NarrowOp> {
        match self {
            NarrowOp::To(_) => None,
            NarrowOp::StripOptional { .. } => Some(NarrowOp::To(InferredType::Undef)),
        }
    }
}

/// A recognized guard fact: the subject, what the guard proves, and
/// whether the proof holds where the guard *expression* is TRUE (`!`
/// flips it).
pub(super) struct GuardFact {
    subject: NarrowSubject,
    op: NarrowOp,
    asserts_when_true: bool,
}

impl GuardFact {
    /// The op to emit in a region where the guard holds iff the guard
    /// expression is `holds`: the fact's own op when the polarity matches
    /// (positive narrowing), else its negation (which may be
    /// unrepresentable → `None`, so the region stays wide).
    fn op_for_region(&self, holds: bool) -> Option<NarrowOp> {
        if self.asserts_when_true == holds {
            Some(self.op.clone())
        } else {
            self.op.negated()
        }
    }
}

/// A pending `defined`/`blessed` narrowing — its `Optional<T> → T` strip
/// is re-derived each fold iteration once the subject type converges.
#[derive(Clone)]
pub(super) struct DefinedNarrowing {
    name: String,
    scope: ScopeId,
    region: Span,
    query_point: Point,
}

/// Map a `ref(...) eq STRING` right-hand string to the type it proves.
/// The builtin reftype tokens are a fixed Perl language constant (reading
/// a grammar constant, not a rule-#10 shape-table over an open behavior
/// set); tokens with no lattice variant yield `None` (recognized, not
/// narrowable). Anything else is a class name (a blessed ref).
fn ref_string_to_type(s: &str) -> Option<InferredType> {
    Some(match s {
        "HASH" => InferredType::HashRef,
        "ARRAY" => InferredType::ArrayRef,
        "CODE" => InferredType::CodeRef { return_edge: None },
        "Regexp" => InferredType::Regexp,
        "SCALAR" | "REF" | "GLOB" | "LVALUE" | "FORMAT" | "IO" | "VSTRING" => return None,
        other => InferredType::ClassName(other.to_string()),
    })
}

/// The earlier of two optional truncation points (`None` = no bound).
fn earliest_point(a: Option<Point>, b: Option<Point>) -> Option<Point> {
    match (a, b) {
        (Some(x), Some(y)) => Some(if point_lt(x, y) { x } else { y }),
        (some, None) | (None, some) => some,
    }
}

/// A narrowable subject node: a plain scalar, or a constant hash/array
/// place element (`$self->{x}`).
fn is_subject_node_kind(kind: &str) -> bool {
    matches!(
        kind,
        "scalar" | "hash_element_expression" | "array_element_expression"
    )
}

/// The subject argument of a `func1op_call_expression` (`ref($x)` /
/// `ref $self->{x}`) — the first scalar/place operand, groups peeled.
/// Does NOT descend into a call/deref (`ref(f($x))` is `f`'s return, not
/// `$x`), so the group-peeling child finder is exactly right.
fn func1op_subject_arg<'a>(node: Node<'a>) -> Option<Node<'a>> {
    crate::cst::first_named_child_where(node, is_subject_node_kind)
}

/// Recognize the narrowing facts a condition proves. One fact per
/// recognized conjunct (`&&`/`and` intersect the region); a disjunctive
/// (`||`/`or`) or unrecognized condition yields none. `builder` is
/// threaded so recognizers can consult constant-fold state (a folded
/// class name in `$x->isa($CLASS)`).
fn recognize_guards(builder: &Builder, cond: Node, source: &[u8]) -> Vec<GuardFact> {
    // Peel transparent `(...)` / single-element list wrappers up front, so
    // `if (($x->isa('Foo')))` and nested grouping fall through to the real
    // condition — one shared primitive instead of a per-shape arm.
    let cond = crate::cst::peel_groups(cond);
    match cond.kind() {
        "unary_expression" if raw_leading_op(cond, source) == "!" => cond
            .child_by_field_name("operand")
            .map(|op| {
                recognize_guards(builder, op, source)
                    .into_iter()
                    .map(|mut f| {
                        f.asserts_when_true = !f.asserts_when_true;
                        f
                    })
                    .collect()
            })
            .unwrap_or_default(),
        "binary_expression" if matches!(raw_mid_op(cond, source).as_str(), "&&" | "and") => {
            let mut out = Vec::new();
            if let Some(l) = cond.child_by_field_name("left") {
                out.extend(recognize_guards(builder, l, source));
            }
            if let Some(r) = cond.child_by_field_name("right") {
                out.extend(recognize_guards(builder, r, source));
            }
            out
        }
        "method_call_expression" => recognize_isa_guard(builder, cond, source).into_iter().collect(),
        "equality_expression" => recognize_ref_eq_guard(cond, source).into_iter().collect(),
        "func1op_call_expression" => recognize_defined_guard(cond, source).into_iter().collect(),
        "ambiguous_function_call_expression" | "function_call_expression" => {
            recognize_blessed_guard(cond, source).into_iter().collect()
        }
        _ => Vec::new(),
    }
}

/// `defined $x` / `defined($x)` → strip `Optional` off the subject.
fn recognize_defined_guard(call: Node, source: &[u8]) -> Option<GuardFact> {
    if call.child(0)?.utf8_text(source).ok()? != "defined" {
        return None;
    }
    let arg = func1op_subject_arg(call)?;
    Some(GuardFact {
        subject: narrow_subject_of(arg, source)?,
        op: NarrowOp::StripOptional { query_point: arg.start_position() },
        asserts_when_true: true,
    })
}

/// `blessed $x` / `blessed($x)` → strip `Optional` off the subject (v1
/// treats `blessed` as `defined`'s strip; the extra "is an object"
/// precision has no lattice target yet).
fn recognize_blessed_guard(call: Node, source: &[u8]) -> Option<GuardFact> {
    let name = call.child_by_field_name("function")?.utf8_text(source).ok()?;
    if name != "blessed" {
        return None;
    }
    // The `arguments` field is the lone operand; peel grouping and accept
    // it only if it's a scalar/place — never descend into a call/deref.
    let arg = crate::cst::peel_groups(call.child_by_field_name("arguments")?);
    if !is_subject_node_kind(arg.kind()) {
        return None;
    }
    Some(GuardFact {
        subject: narrow_subject_of(arg, source)?,
        op: NarrowOp::StripOptional { query_point: arg.start_position() },
        asserts_when_true: true,
    })
}

/// `$x->isa('Foo')` / `$x->DOES('Role')` → narrow `$x` to `ClassName`.
/// The class argument is a string literal, or a scalar that the builder's
/// constant fold pins to a single class (`my $C = 'Foo'; $x->isa($C)`).
fn recognize_isa_guard(builder: &Builder, call: Node, source: &[u8]) -> Option<GuardFact> {
    let mc = crate::cst::MethodCall::cast(call)?;
    let method = mc.method()?.utf8_text(source).ok()?;
    if method != "isa" && method != "DOES" {
        return None;
    }
    let subject = narrow_subject_of(mc.invocant()?, source)?;
    let arg = call.child_by_field_name("arguments")?;
    let class = crate::cst::plain_string_literal_text(arg, source)
        .or_else(|| builder.folded_class_name_arg(arg))?;
    Some(GuardFact {
        subject,
        op: NarrowOp::To(InferredType::ClassName(class)),
        asserts_when_true: true,
    })
}

/// `ref($x) eq 'Foo'` / `reftype($x) eq 'HASH'` (either operand order) →
/// narrow `$x` to the proven type.
fn recognize_ref_eq_guard(eq: Node, source: &[u8]) -> Option<GuardFact> {
    if raw_mid_op(eq, source) != "eq" {
        return None;
    }
    let left = eq.child_by_field_name("left")?;
    let right = eq.child_by_field_name("right")?;
    let (ref_call, lit) = if left.kind() == "func1op_call_expression" {
        (left, right)
    } else if right.kind() == "func1op_call_expression" {
        (right, left)
    } else {
        return None;
    };
    let fname = ref_call.child(0)?.utf8_text(source).ok()?;
    if fname != "ref" && fname != "reftype" {
        return None;
    }
    let subject = narrow_subject_of(func1op_subject_arg(ref_call)?, source)?;
    let ty = ref_string_to_type(&crate::cst::plain_string_literal_text(lit, source)?)?;
    Some(GuardFact {
        subject,
        op: NarrowOp::To(ty),
        asserts_when_true: true,
    })
}

/// True if a statement-level expression is a guaranteed control-flow exit
/// (`return`/`die`/`croak`/`last`/`next`/`redo`/`goto`) — the shape that
/// makes `STMT if/unless G` narrow the rest of the enclosing block.
fn is_exit_expression(node: Node, source: &[u8]) -> bool {
    const EXITS: [&str; 7] = ["die", "croak", "confess", "last", "next", "redo", "goto"];
    match node.kind() {
        "return_expression" | "last_expression" | "next_expression" | "redo_expression" => true,
        "function" | "bareword" => node
            .utf8_text(source)
            .map(|s| EXITS.contains(&s.trim()))
            .unwrap_or(false),
        "func1op_call_expression"
        | "function_call_expression"
        | "ambiguous_function_call_expression" => node
            .child_by_field_name("function")
            .or_else(|| node.child(0))
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| EXITS.contains(&s.trim()))
            .unwrap_or(false),
        _ => false,
    }
}

/// First named child of `node` with the given `kind` (the `elsif` / `else`
/// arm hanging off a `conditional_statement` or `elsif`).
fn child_of_kind<'a>(node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    (0..node.named_child_count())
        .filter_map(|i| node.named_child(i))
        .find(|c| c.kind() == kind)
}

/// A subject's identity for dedup across arms (its source spelling).
fn subject_key(subject: &NarrowSubject) -> String {
    match subject {
        NarrowSubject::Variable(v) => v.clone(),
        NarrowSubject::Place { key, .. } => key.clone(),
    }
}

impl<'a> Builder<'a> {
    /// A scalar `isa`/`DOES` argument (`$x->isa($C)`) that the constant
    /// fold pins to exactly one class name. A multi-valued fold can't name
    /// a single dispatch class, so it doesn't narrow. `None` for non-scalar
    /// args (literals are handled by `plain_string_literal_text`).
    fn folded_class_name_arg(&self, arg: Node<'a>) -> Option<String> {
        if arg.kind() != "scalar" {
            return None;
        }
        let key = crate::cst::canonical_var_name(arg, self.source)?;
        match self.resolve_constant_strings(&key, 0).as_deref() {
            Some([class]) => Some(class.clone()),
            _ => None,
        }
    }

    /// Narrow an `if` / `elsif`* / `else` chain. Each arm runs only when its
    /// own condition holds (positive narrowing) AND every preceding
    /// condition fell through (cumulative negation). Only representable
    /// negations survive — `defined`/`blessed` complement to `Undef`, while
    /// `isa`/`ref-eq` have no positive complement (stay wide). The `else`
    /// arm is pure cumulative negation; an `elsif` arm combines its own
    /// guard with the priors' negation.
    pub(super) fn narrow_block_guard(&mut self, cond_stmt: Node<'a>) {
        let Some(condition) = cond_stmt.child_by_field_name("condition") else { return };
        let Some(block) = cond_stmt.child_by_field_name("block") else { return };
        // The truth value the first condition must take for its block to run
        // (`if` → true, `unless` → false). `elsif` arms are always positive.
        let Some(first_enter) = self.block_guard_polarity(cond_stmt, condition) else { return };

        // Walk the chain: `(condition?, block, enter_polarity)` per arm.
        // The `else`/`elsif` nest inside the trailing `elsif`, so descend.
        let mut arms: Vec<(Option<Node<'a>>, Node<'a>, bool)> =
            vec![(Some(condition), block, first_enter)];
        let mut tail = cond_stmt;
        loop {
            let Some(next) =
                child_of_kind(tail, "elsif").or_else(|| child_of_kind(tail, "else"))
            else {
                break;
            };
            if next.kind() == "elsif" {
                let (Some(c), Some(b)) =
                    (next.child_by_field_name("condition"), next.child_by_field_name("block"))
                else {
                    break;
                };
                arms.push((Some(c), b, true));
                tail = next;
            } else {
                if let Some(b) = next.child_by_field_name("block") {
                    arms.push((None, b, true));
                }
                break;
            }
        }

        for k in 0..arms.len() {
            let (own_cond, blk, enter) = arms[k];
            let region = node_to_span(blk);
            // The arm's own guard is the more direct statement about a
            // subject; a prior arm's negation must not override it.
            let mut covered: Vec<String> = Vec::new();
            if let Some(c) = own_cond {
                for fact in recognize_guards(self, c, self.source) {
                    if let Some(op) = fact.op_for_region(enter) {
                        covered.push(subject_key(&fact.subject));
                        self.emit_narrowing_fact(&fact.subject, op, region, blk);
                    }
                }
            }
            for &(prior_cond, _, prior_enter) in &arms[..k] {
                let Some(prior_cond) = prior_cond else { continue };
                for fact in recognize_guards(self, prior_cond, self.source) {
                    let key = subject_key(&fact.subject);
                    if covered.contains(&key) {
                        continue;
                    }
                    // A prior arm not taken means its condition held at the
                    // opposite of its enter polarity.
                    if let Some(op) = fact.op_for_region(!prior_enter) {
                        covered.push(key);
                        self.emit_narrowing_fact(&fact.subject, op, region, blk);
                    }
                }
            }
        }
    }

    /// `if` → body holds where the guard is TRUE; `unless` → where FALSE.
    /// `None` for any other leading keyword (`elsif` / `while` / `until`).
    fn block_guard_polarity(&self, cond_stmt: Node<'a>, condition: Node<'a>) -> Option<bool> {
        let between =
            std::str::from_utf8(&self.source[cond_stmt.start_byte()..condition.start_byte()])
                .ok()?;
        match between.split_whitespace().next()? {
            "if" => Some(true),
            "unless" => Some(false),
            _ => None,
        }
    }

    /// `STMT if/unless G;` where STMT is a control-flow exit — narrow the
    /// rest of the enclosing block (the guard holds for the fall-through).
    pub(super) fn narrow_postfix_exit(&mut self, postfix: Node<'a>) {
        let Some(condition) = postfix.child_by_field_name("condition") else { return };
        let Some(modified) = postfix.named_child(0) else { return };
        if !is_exit_expression(modified, self.source) {
            return;
        }
        let Some(kw) = connector_keyword_between(modified, condition, self.source) else { return };
        // `return unless G` → fall-through holds when G TRUE; `if G` → FALSE.
        let holds_when_true = match kw.as_str() {
            "unless" => true,
            "if" => false,
            _ => return,
        };
        self.narrow_block_remainder(postfix, condition, holds_when_true);
    }

    /// `G or EXIT;` / `G and EXIT;` — the bare-logical early-exit idiom.
    pub(super) fn narrow_logical_exit(&mut self, expr: Node<'a>) {
        let Some(left) = expr.child_by_field_name("left") else { return };
        let Some(right) = expr.child_by_field_name("right") else { return };
        if !is_exit_expression(right, self.source) {
            return;
        }
        // `G or EXIT` → fall-through holds when G TRUE; `G and EXIT` → FALSE.
        let holds_when_true = match raw_mid_op(expr, self.source).as_str() {
            "or" | "||" => true,
            "and" | "&&" => false,
            _ => return,
        };
        self.narrow_block_remainder(expr, left, holds_when_true);
    }

    /// Narrow `[statement end .. enclosing-block end]` for a statement-level
    /// guard.
    fn narrow_block_remainder(
        &mut self,
        stmt_expr: Node<'a>,
        condition: Node<'a>,
        holds_when_true: bool,
    ) {
        let mut stmt = stmt_expr;
        while stmt.kind() != "expression_statement" {
            let Some(p) = stmt.parent() else { return };
            stmt = p;
        }
        let Some(block) = stmt.parent() else { return };
        if block.kind() != "block" {
            return;
        }
        let region = Span { start: stmt.end_position(), end: block.end_position() };
        for fact in recognize_guards(self, condition, self.source) {
            if let Some(op) = fact.op_for_region(holds_when_true) {
                self.emit_narrowing_fact(&fact.subject, op, region, block);
            }
        }
    }

    /// Push the span-extent narrowing witness on the subject's home scope,
    /// truncating the region at the first reassignment of the subject.
    /// Emit a guard's narrowing into `region`. `op` is the polarity-
    /// resolved operation (`GuardFact::op_for_region`); the subject is
    /// keyed and the region truncated at the first disturbance, exactly
    /// as for a positive narrowing.
    fn emit_narrowing_fact(
        &mut self,
        subject: &NarrowSubject,
        op: NarrowOp,
        region: Span,
        container: Node<'a>,
    ) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        let (name, end) = match subject {
            NarrowSubject::Variable(var) => {
                let end = self.first_subject_write(var, region, container);
                (var.clone(), end)
            }
            NarrowSubject::Place { key, root, key_vars } => {
                // The place is stable while its container/prefixes AND every
                // dynamic-key scalar are unchanged; truncate at the earliest
                // disturbance of either.
                let mut end = self.first_place_invalidation(key, root, region, container);
                for kv in key_vars {
                    let kv_end = self.first_subject_write(kv, region, container);
                    end = earliest_point(end, kv_end);
                }
                (key.clone(), end)
            }
        };
        let end = end.unwrap_or(region.end);
        if !point_lt(region.start, end) {
            return; // truncated to nothing
        }
        let region = Span { start: region.start, end };
        let scope = self.current_scope();
        match op {
            NarrowOp::To(ty) => {
                self.bag.push(Witness {
                    attachment: WitnessAttachment::Variable { name, scope },
                    source: WitnessSource::Builder("narrowing".into()),
                    payload: WitnessPayload::InferredType(ty),
                    span: region,
                });
            }
            // `defined`/`blessed` strip `Optional<T>` to `T`, but the
            // subject's type may only converge in the fold (a sub return),
            // so record it and re-derive in `emit_defined_narrowing_witnesses`.
            NarrowOp::StripOptional { query_point } => {
                self.defined_narrowings.push(DefinedNarrowing {
                    name,
                    scope,
                    region,
                    query_point,
                });
            }
        }
    }

    /// Re-emittable: `defined`/`blessed` narrowing. For each recorded
    /// guard, read the subject's type at the guard point (BEFORE the
    /// narrowed region, so this pass's own output is excluded — no
    /// oscillation) and, if it is `Optional<T>`, narrow the region to `T`.
    /// Clear-and-emit on tag `defined_narrowing`; converges as the
    /// subject's (possibly fold-derived) `Optional` settles.
    pub(super) fn emit_defined_narrowing_witnesses(&mut self) {
        use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
        self.bag.remove_by_source_tag("defined_narrowing");
        let guards = self.defined_narrowings.clone();
        let mut emits: Vec<(String, ScopeId, InferredType, Span)> = Vec::new();
        for g in &guards {
            if let Some(inner) = self
                .bag_query_variable(&g.name, g.scope, g.query_point)
                .as_ref()
                .and_then(InferredType::optional_inner)
            {
                emits.push((g.name.clone(), g.scope, inner.clone(), g.region));
            }
        }
        for (name, scope, inner, region) in emits {
            self.bag.push(Witness {
                attachment: WitnessAttachment::Variable { name, scope },
                source: WitnessSource::Builder("defined_narrowing".into()),
                payload: WitnessPayload::InferredType(inner),
                span: region,
            });
        }
    }

    /// Point of the first operation in `container` (at or after
    /// `region.start`) that could disturb place `key` rooted at `root`: a
    /// write to the slot, or an opaque use of any **proper prefix** of the
    /// place — the root scalar or an intermediate element (`$self->{a}`
    /// for `$self->{a}{b}`). A prefix is "guarded" (a read, not a
    /// disturbance) when it is the base of a longer access; otherwise it is
    /// a write, a method call on the prefix, or the prefix passed as an
    /// argument. Conservative — it under-narrows.
    fn first_place_invalidation(
        &self,
        key: &str,
        root: &str,
        region: Span,
        container: Node<'a>,
    ) -> Option<Point> {
        fn consider(node: Node, after: Point, best: &mut Option<Point>) {
            let p = node.start_position();
            if !point_lt(p, after) && best.map_or(true, |b| point_lt(p, b)) {
                *best = Some(p);
            }
        }
        // `prefix` is a proper prefix of `key` at an access boundary
        // (`$self` / `$self->{a}` of `$self->{a}{b}`), not a coincidental
        // string prefix (`$selfish`).
        fn is_proper_prefix(prefix: &str, key: &str) -> bool {
            key.len() > prefix.len()
                && key.starts_with(prefix)
                && matches!(key.as_bytes()[prefix.len()], b'-' | b'{' | b'[')
        }
        fn scan(node: Node, key: &str, root: &str, after: Point, src: &[u8], best: &mut Option<Point>) {
            // Slot rewrite: `$self->{a}{b} = ...`.
            if node.kind() == "assignment_expression" {
                if let Some(left) = node.child_by_field_name("left") {
                    if crate::cst::canonical_place_path(left, src).map(|(k, _)| k).as_deref()
                        == Some(key)
                    {
                        consider(node, after, best);
                    }
                }
            }
            // Opaque use of a proper prefix: the root scalar, or an
            // intermediate element (`$self->{a}` of `$self->{a}{b}`). The
            // exact place is excluded — a read / method call on the slot
            // value doesn't disturb the slot.
            let is_prefix = match node.kind() {
                // Arrow-form root scalar (`$self`), or the whole named
                // container of a direct-form place (`%h` / `@h` used as a
                // unit — `keys %h`, `%h = (...)`, `\@h`). A bare
                // appearance of either is an opaque use; the slot reads
                // `$h{...}` go through `container_variable`, which is NOT
                // matched here, so they don't truncate.
                "scalar" | "hash" | "array" => {
                    crate::cst::canonical_var_name(node, src).as_deref() == Some(root)
                }
                // Slice of the named container (`@h{...}` / `@h[...]`) can
                // write multiple slots — treat any appearance as a
                // disturbance (sound; over-truncates a pure slice read).
                "slice_container_variable" | "keyval_container_variable" => {
                    crate::cst::canonical_container_name(node, src).as_deref() == Some(root)
                }
                "hash_element_expression" | "array_element_expression" => {
                    crate::cst::canonical_place_path(node, src)
                        .is_some_and(|(k, _)| is_proper_prefix(&k, key))
                }
                _ => false,
            };
            if is_prefix {
                // Guarded = this prefix is the base of a longer access, i.e.
                // a read; otherwise it is a write / method call / argument.
                let guarded = node.parent().map_or(false, |p| {
                    matches!(p.kind(), "hash_element_expression" | "array_element_expression")
                        && p.named_child(0) == Some(node)
                });
                if !guarded {
                    consider(node, after, best);
                }
            }
            for i in 0..node.named_child_count() {
                if let Some(c) = node.named_child(i) {
                    scan(c, key, root, after, src, best);
                }
            }
        }
        let mut best = None;
        scan(container, key, root, region.start, self.source, &mut best);
        best
    }

    /// Point of the first reassignment of `$var` inside `container` at or
    /// after `region.start` — the narrowing region's truncation bound.
    fn first_subject_write(&self, var: &str, region: Span, container: Node<'a>) -> Option<Point> {
        // Truncate the narrowed region at the first node that rebinds `$var`,
        // in ANY of Perl's binding shapes (`cst::rebinds_scalar` is the single
        // detector — assignment, `my`/`local`, list-assign, foreach loop var,
        // lvalue-sub). Recursing into every node and taking the earliest hit
        // keeps the truncation conservative (a rebind in a nested branch only
        // shrinks the region — under-narrowing, never a false `Undef`).
        fn scan(node: Node, var: &str, after: Point, src: &[u8], best: &mut Option<Point>) {
            if crate::cst::rebinds_scalar(node, var, src) {
                let p = node.start_position();
                if !point_lt(p, after) && best.map_or(true, |b| point_lt(p, b)) {
                    *best = Some(p);
                }
            }
            for i in 0..node.named_child_count() {
                if let Some(c) = node.named_child(i) {
                    scan(c, var, after, src, best);
                }
            }
        }
        let mut best = None;
        scan(container, var, region.start, self.source, &mut best);
        best
    }
}
