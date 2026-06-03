//! Witness bag + reducers — the type-inference engine.
//!
//! A **witness** is typed evidence about a specific code location
//! (variable, expression, symbol, hash key...), tagged with its source
//! (which builder pass / plugin emitted it). A bag of witnesses is
//! folded into concrete answers by **reducers** — pure projections that
//! claim the witnesses they care about.
//!
//! A few API surfaces carry `#[allow(dead_code)]` (`WitnessBag::all`,
//! `filter`, `is_empty`; `ReducedValue::FactMap`; `WitnessReducer::name`):
//! they're the bag's stable contract for plugins and future reducers,
//! held in the public surface deliberately rather than chased dead.

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

use crate::file_analysis::{
    HashKeyOwner, InferredType, ParametricType, Scope, ScopeId, Span, SymbolId,
};

use tree_sitter::Point;

// ---- Core witness types ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Witness {
    pub attachment: WitnessAttachment,
    pub source: WitnessSource,
    pub payload: WitnessPayload,
    /// Source location the witness was emitted at — used by the fold for
    /// narrowing (narrowest containing span wins) and temporal ordering
    /// (witnesses past the query point are skipped). Zero-extent span
    /// means a core-synthesized seed with no single source location.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WitnessAttachment {
    /// Variable-in-scope facts — what `TypeConstraint` / `CallBinding`
    /// index by.
    Variable { name: String, scope: ScopeId },
    /// Result type of a method call, keyed by its index into
    /// `FileAnalysis::refs`. Only `RefKind::MethodCall` refs get
    /// witnesses here (function calls resolve via `Symbol` edges; the
    /// general rvalue axis is `Expr` below). Chain aggregation
    /// (`X->m()->n()`) folds across this axis.
    Expression(RefIdx),
    /// A symbol property ("this sub is a dispatcher").
    Symbol(SymbolId),
    /// Hash key metadata (writes, mutations, derivations).
    HashKey { owner: HashKeyOwner, name: String },
    /// The value of an expression at this span — the one attachment
    /// shape for every rvalue (literals, variable reads, calls,
    /// ternaries, return-arm bodies, implicit-last statements).
    /// Witnesses here are either a direct `InferredType(t)` (literals,
    /// constructors, builtin returns) or an `Edge` to the resolution
    /// target (`Variable` for `$foo`, `Symbol` for a resolved local sub
    /// call, `Expression` for a method call's resolved type, or another
    /// `Expr` for compound expressions). The per-sub fold reads
    /// `Edge(Expr(span))` arms via `Symbol(sub_id)`.
    Expr(Span),
    /// Class-keyed method dispatch: "what does method `name` return on
    /// class `class`?" — the cross-class disambiguation a name-keyed
    /// attachment can't carry. Inheritance composes through
    /// `Edge(MethodOnClass(parent, name))` witnesses the builder emits
    /// per `package_parents[C]`, so the registry's cycle-guarded edge
    /// chase walks the MRO with no procedural ancestor walker. With a
    /// `BagContext.module_index`, the materialize step recurses into the
    /// cached module's bag for `class`.
    MethodOnClass { class: String, name: String },
    /// Per-arm return collector for a sub. Each `return EXPR` arm pushes
    /// one `Edge(Expr(body_span))` here; the parent `Symbol(sub_id)`
    /// carries one `Edge(SymbolReturnArm(_))` so consumers querying the
    /// symbol still see arm-fold answers via edge materialization.
    /// Distinct from `Symbol(_)` so `SymbolReturnArmFold` claims by
    /// attachment shape, not source-tag exclusion.
    SymbolReturnArm(SymbolId),
    /// Per-arm collector for a ternary `$c ? A : B`, keyed by the
    /// conditional expression's span. Each arm pushes one
    /// `Edge(Expr(arm_span))` here; the ternary's own `Expr(span)`
    /// carries a single `Edge(BranchArm(span))` so consumers querying
    /// the expression materialize the agreed arm type. Distinct shape
    /// (like `SymbolReturnArm`) so `BranchArmFold` claims by attachment,
    /// which is why `ExprReturn` / `FrameworkAwareTypeFold` no longer
    /// have to exclude branch-arm witnesses from the shared `Expr` /
    /// `Variable` attachments.
    BranchArm(Span),
}

/// Index into `FileAnalysis::refs`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RefIdx(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WitnessSource {
    /// Named builder pass — "signature_extraction", "narrowing", …
    Builder(String),
    /// Plugin id.
    Plugin(String),
    /// Post-build enrichment source.
    Enrichment(String),
    /// Derived from another ref — rename transport chases these as a DAG.
    DerivedFrom(RefIdx),
}

impl WitnessSource {
    /// Priority for "highest-priority source wins" tie-breaking in
    /// reducers. Plugin overrides dominate everything else (the whole
    /// point of an override is "inference reaches the wrong answer
    /// here"); the weights only need `Plugin > everything else`.
    pub fn priority(&self) -> u8 {
        match self {
            WitnessSource::Plugin(_) => 100,
            WitnessSource::Builder(_)
            | WitnessSource::Enrichment(_)
            | WitnessSource::DerivedFrom(_) => 10,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WitnessPayload {
    /// Final-form type belief — what legacy `TypeConstraint` carries.
    InferredType(InferredType),
    /// An **observation** — raw evidence about a value's use, folded by
    /// the framework-aware resolver.
    Observation(TypeObservation),
    /// Edge fact: "the value at my attachment is whatever resolves at
    /// `target`." The registry materializes these at query time — chase
    /// the target via recursive query, replace the edge with a synthetic
    /// `InferredType` witness preserving source + span, then run reducers
    /// against the materialized list. A cycle guard breaks `A → B → A`.
    Edge(WitnessAttachment),
    /// Edge fact for a **method call at a known arity**: "the value at my
    /// attachment is `target`'s return type, dispatched at `arity` args."
    /// Distinct from a plain `Edge` because the call site's arity is
    /// intrinsic to the *call*, not to whatever outer query reached it —
    /// a hint-less `$x` type query that chases here must still pick the
    /// fluent-writer arm of `$obj->setter($v)` (arity ≥ 1), not the
    /// getter arm a hint-less `UnionOnArgs` defaults to. Emitted by
    /// `emit_method_call_return_edges` (`Expression(refidx)` → its
    /// `MethodOnClass{class, method}` at the call's `count_call_args`);
    /// chased like `Edge` but overrides `q.arity_hint` with `arity`.
    CallReturn { target: WitnessAttachment, arity: u32 },
    /// **Symbol-declarative return type.** A receiver-relative /
    /// arity-relative expression that `ReturnExprReducer` substitutes at
    /// query time using `q.receiver` and `q.arity_hint`. Subsumes both
    /// call-site projection (DBIC `find` emits `Operator(RowOf(Receiver))`
    /// once on the symbol) and arity dispatch (Mojo `has`'s getter/writer
    /// collapse to a single `UnionOnArgs`).
    ///
    /// Attached to `Symbol(_)` (per-sub) and `MethodOnClass{...}`
    /// (class-keyed). Latest wins, so a plugin override re-publishes over
    /// a build-time inference.
    ReturnExpr(ReturnExpr),
    /// Keyed fact. Family + key + value schema is the reducer's
    /// responsibility.
    Fact { family: String, key: String, value: FactValue },
    /// "This witness's subject derives from another ref." Rename
    /// transport walks these.
    Derivation,
    /// Escape hatch for plugin-defined payloads that don't fit above.
    Custom { family: String, json: String },
}

/// A sub's return type as a **deferred computation**, not a value:
/// conceptually `(receiver, arity) -> InferredType`. `ReturnExprReducer`
/// evaluates it against the query's `q.receiver` / `q.arity_hint`.
///
/// This is deliberately distinct from `InferredType` and must NOT be
/// merged into it: `Receiver` is a free variable and `UnionOnArgs` is an
/// arity-indexed dispatch table — neither is a concrete type. Folding
/// these into `InferredType` would force every type *consumer* to handle
/// "what if this is still an unsubstituted hole / a dispatch table?".
/// Keeping the schema (`ReturnExpr`) separate from the value
/// (`InferredType`) confines that concern to `eval_return_expr`.
///
/// See `docs/adr/return-expr.md` and `docs/adr/parametric-types.md` for
/// the sealed-enum rationale (every consumer matches, no `_ => …`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ReturnExpr {
    /// Concrete type — equivalent to a plain `InferredType` payload on a
    /// Symbol attachment.
    Concrete(InferredType),
    /// Receiver placeholder. Evaluates to `q.receiver`; `None` when the
    /// query carries no receiver (build-time lookup, not a call site) —
    /// the reducer returns `None` rather than guessing.
    Receiver,
    /// Apply a parametric operator with `ReturnExpr`-valued sub-positions.
    /// Substitution recurses, evaluates, and re-wraps as `ParametricType`
    /// so the value-side accessors (`class_name`, `hash_key_class`, …)
    /// handle consumption downstream.
    Operator(ParametricOp),
    /// Union over arg-shape. Each branch is `(guard, expr)`; for a
    /// concrete `arity_hint` the first matching guard wins. For a
    /// hint-less query the `Any` branch is preferred, falling back to
    /// `Empty` (so a Mojo `has` getter+writer pair surfaces its primary).
    /// Branch order matters when the hint is concrete — narrow guards
    /// (`Empty`, `Exact`, `AtLeast`) before `Any`.
    UnionOnArgs { branches: Vec<(ArgGuard, ReturnExpr)> },
}

/// Type-level operators with `ReturnExpr`-valued sub-positions —
/// projections that can't resolve until the receiver is substituted, so
/// they live on the deferred (`ReturnExpr`) side, not on the concrete
/// `InferredType`/`ParametricType` value side. No `_ => …` fall-throughs.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParametricOp {
    /// `RowOf<T>` — projects a `ResultSet { base, row }` to its row class.
    /// `eval_return_expr` evaluates the sub-expression and projects
    /// eagerly: `ResultSet { row, .. }` → `ClassName(row)`, anything else
    /// → `None`.
    RowOf(Box<ReturnExpr>),
}

/// Guard for `ReturnExpr::UnionOnArgs` branches, matched against
/// `ReducerQuery.arity_hint`. A `None` hint matches `Any` only.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArgGuard {
    Empty,
    Exact(u32),
    AtLeast(u32),
    Any,
}

impl ArgGuard {
    /// Match against the call's arity hint. Strict: a guard fires only
    /// when the hint positively matches it; `Any` is the only catch-all,
    /// so a `None` hint never silently fires `Empty`/`Exact`/`AtLeast`.
    ///
    /// Introspection callers pass `None`; sym-introspection entry points
    /// compensate by defaulting the hint from the sym's own `params`
    /// count (a writer sym, params=1, matches `AtLeast(1)`; a getter,
    /// params=0, matches `Empty`).
    pub fn matches(self, arity_hint: Option<u32>) -> bool {
        match (self, arity_hint) {
            (ArgGuard::Empty, Some(0)) => true,
            (ArgGuard::Exact(n), Some(h)) => n == h,
            (ArgGuard::AtLeast(n), Some(h)) => h >= n,
            (ArgGuard::Any, _) => true,
            _ => false,
        }
    }
}

/// Raw observations about a value's use, consumed by the
/// framework-aware resolver. These do NOT commit to a concrete type; the
/// resolver projects them to `InferredType` using framework context.
///
/// Hash/Eq are intentionally NOT derived: `InferredType` is `PartialEq`
/// but not `Hash`. Attachment-keyed indexing uses `WitnessAttachment`,
/// not the payload, so Hash on the observation isn't needed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeObservation {
    /// `my $x = Foo->new` or direct `InferredType::ClassName(_)` assertion.
    ClassAssertion(String),
    /// `my $self = shift` / `$_[0]` at the head of a method body.
    FirstParamInMethod { package: String },
    /// `$v->{k}`, `%$v`, `@$v{...}` — hashref-like access.
    HashRefAccess,
    /// `$v->[i]`, `@$v`.
    ArrayRefAccess,
    /// `$v->()`, `&$v`.
    CodeRefInvocation,
    NumericUse,
    StringUse,
    RegexpUse,
    /// `bless [], $c` pins the representation axis to Array.
    BlessTarget(Rep),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Rep {
    Hash,
    Array,
    Scalar,
    Code,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FactValue {
    Str(String),
    List(Vec<FactValue>),
    Bool(bool),
    Num(f64),
    Map(Vec<(String, FactValue)>),
}

// ---- Framework-mode mirror (builder's FrameworkMode is private to
// builder.rs, so the resolver duplicates a small view) ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum FrameworkFact {
    Moo,
    Moose,
    /// Mojo::Base — hashref-backed, fluent-by-default.
    MojoBase,
    /// Perl 5.38 `class` — opaque / inside-out.
    CoreClass,
    /// No framework detected.
    Plain,
}

impl FrameworkFact {
    /// Which representation does this framework's instances back onto?
    /// `None` = rep-agnostic.
    pub fn backing_rep(self) -> Option<Rep> {
        match self {
            FrameworkFact::Moo | FrameworkFact::Moose | FrameworkFact::MojoBase => Some(Rep::Hash),
            FrameworkFact::CoreClass => None, // opaque
            FrameworkFact::Plain => None,
        }
    }
}

// ---- Witness bag ----

/// Attachment-indexed bag. Kept separate from the raw witness vec so
/// callers can iterate all witnesses for one attachment without scanning.
///
/// `index` is `serde(skip)` (cheap to recompute, redundant on disk). The
/// custom `Deserialize` impl below rebuilds it — without that, every
/// consumer that loads a `FileAnalysis` from bincode (SQLite cache,
/// dump-package, cross-file enrichment) would have reducers claim empty
/// witness slices and the bag would silently return `None`.
#[derive(Debug, Default, Serialize)]
pub struct WitnessBag {
    witnesses: Vec<Witness>,
    #[serde(skip)]
    index: HashMap<WitnessAttachment, Vec<usize>>,
}

impl<'de> Deserialize<'de> for WitnessBag {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // Helper mirrors the on-disk shape (just the witness vec; `index`
        // is recomputed). The derived impl would expect both fields.
        #[derive(Deserialize)]
        struct WitnessBagOnDisk {
            witnesses: Vec<Witness>,
        }
        let on_disk = WitnessBagOnDisk::deserialize(deserializer)?;
        let mut bag = WitnessBag {
            witnesses: on_disk.witnesses,
            index: HashMap::new(),
        };
        bag.rebuild_index();
        Ok(bag)
    }
}

impl WitnessBag {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, w: Witness) -> usize {
        let idx = self.witnesses.len();
        self.index.entry(w.attachment.clone()).or_default().push(idx);
        self.witnesses.push(w);
        idx
    }

    #[allow(dead_code)]
    pub fn all(&self) -> &[Witness] {
        &self.witnesses
    }

    pub fn for_attachment(&self, att: &WitnessAttachment) -> Vec<&Witness> {
        self.index
            .get(att)
            .map(|ixs| ixs.iter().map(|&i| &self.witnesses[i]).collect())
            .unwrap_or_default()
    }

    /// Iterate witnesses matching a predicate. O(n).
    #[allow(dead_code)]
    pub fn filter<P: Fn(&Witness) -> bool>(&self, pred: P) -> Vec<&Witness> {
        self.witnesses.iter().filter(|w| pred(w)).collect()
    }

    pub fn rebuild_index(&mut self) {
        self.index.clear();
        for (i, w) in self.witnesses.iter().enumerate() {
            self.index.entry(w.attachment.clone()).or_default().push(i);
        }
    }

    /// Drop every witness past `baseline` and rebuild the index. Lets
    /// enrichment revert post-build additions before re-deriving them,
    /// keeping the bag idempotent across repeat enrichment calls.
    pub fn truncate(&mut self, baseline: usize) {
        if baseline >= self.witnesses.len() {
            return;
        }
        self.witnesses.truncate(baseline);
        self.rebuild_index();
    }

    /// Drop every `Builder(tag)`-sourced witness and rebuild the index;
    /// returns the count removed. Re-emittable builder passes call this
    /// at the start of each fold iteration so the bag stays
    /// duplicate-free no matter how many times the fold runs.
    pub fn remove_by_source_tag(&mut self, tag: &str) -> usize {
        let before = self.witnesses.len();
        self.witnesses.retain(|w| match &w.source {
            WitnessSource::Builder(s) => s != tag,
            _ => true,
        });
        let removed = before - self.witnesses.len();
        if removed > 0 {
            self.rebuild_index();
        }
        removed
    }

    pub fn len(&self) -> usize {
        self.witnesses.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.witnesses.is_empty()
    }
}

// ---- Reducers ----

/// Input to a reducer query: the attachment plus an optional point of
/// interest (so narrowing-scoped reducers pick the closest containing
/// witness).
#[derive(Clone)]
pub struct ReducerQuery<'a> {
    pub attachment: &'a WitnessAttachment,
    pub point: Option<tree_sitter::Point>,
    pub framework: FrameworkFact,
    /// Arity hint for arity-dispatch reducers. `Some(N)` = caller passed
    /// N additional args; `None` = unknown (reducer returns the default
    /// branch).
    pub arity_hint: Option<u32>,
    /// Receiver type for `ReturnExpr::Receiver` substitution. Set by the
    /// chain typer's coderef/dynamic-method-call arms and by
    /// `MethodOnClass{...}` chases originating from a method call with a
    /// known invocant. `None` for build-time symbol probes — `Receiver`
    /// then evaluates to `None` rather than guessing.
    pub receiver: Option<InferredType>,
    /// Optional scope topology + per-package framework. Lets the
    /// registry chase `Edge(Variable{...})` with `query_variable_type`
    /// semantics (scope-chain walk + framework fold) instead of a flat
    /// lookup. `None` for context-free queries (tests, self-contained
    /// targets).
    pub context: Option<&'a BagContext<'a>>,
}

/// File-scope context the registry needs to chase edges correctly.
/// Carries the scope tree and per-package framework so materialization
/// can run `query_variable_type` for `Variable` targets — the only edge
/// target whose resolution needs more than the bag itself.
///
/// `module_index` lets materialization recurse into cached modules' bags
/// when a `MethodOnClass{class,...}` names a class in another file.
/// `package_parents` is the per-class inheritance graph (Perl DFS-MRO);
/// the registry walks it for `MethodOnClass{C, m}` queries the local bag
/// can't answer, chasing `MethodOnClass{P, m}` per parent. Both are
/// `None`/empty for in-file callers.
pub struct BagContext<'a> {
    pub scopes: &'a [Scope],
    pub package_framework: &'a HashMap<String, FrameworkFact>,
    pub module_index: Option<&'a crate::module_index::ModuleIndex>,
    pub package_parents: &'a HashMap<String, Vec<String>>,
    /// Manifest-declared app-surface consumer classes — threaded so the
    /// `MethodOnClass` inheritance walk injects the synthetic surface
    /// parent via `parents_of`, matching the FA-side ancestor walks.
    /// Empty for in-file callers that don't carry consumer state.
    pub app_surface_consumers: &'a [String],
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)] // FactMap reserved for payload-bearing reducers
pub enum ReducedValue {
    Type(InferredType),
    FactMap(Vec<(String, FactValue)>),
    None,
}

pub trait WitnessReducer: Send + Sync {
    #[allow(dead_code)] // identity for tracing/debug; see module-level note
    fn name(&self) -> &str;

    fn claims(&self, w: &Witness) -> bool;

    fn reduce(&self, ws: &[&Witness], q: &ReducerQuery) -> ReducedValue;
}

// ---- Built-in: framework-aware type-fold reducer ----

/// Folds class / rep / scalar observations into a type:
///
/// 1. `ClassAssertion(Foo)` dominates.
/// 2. `FirstParamInMethod { package }` under a matching framework's
///    backing rep is NOT dethroned by rep observations matching that rep
///    (the Mojo `sub name` bug fix).
/// 3. `BlessTarget(Rep)` pins the rep axis.
/// 4. Rep observations with no class evidence project to flat
///    `HashRef` / `ArrayRef` / `CodeRef`.
/// 5. `NumericUse` / `StringUse` / `RegexpUse` project to their types.
pub struct FrameworkAwareTypeFold;

impl WitnessReducer for FrameworkAwareTypeFold {
    fn name(&self) -> &str {
        "framework_aware_type_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(
            w.attachment,
            WitnessAttachment::Variable { .. } | WitnessAttachment::Expression(_)
        ) && matches!(
            w.payload,
            WitnessPayload::InferredType(_) | WitnessPayload::Observation(_)
        )
    }

    fn reduce(&self, ws: &[&Witness], q: &ReducerQuery) -> ReducedValue {
        // Point-narrowing is variable-lifetime semantics: at the query point,
        // which assignment of `$x` is live. It only makes sense for `Variable`
        // attachments. This reducer also claims `Expression` (a method call's
        // resolved return type) — those carry one witness spanning the call,
        // and filtering them by a point inherited from a chasing variable's
        // scope wrongly discards them (`my $g = X->new->m` chased at `$g`'s
        // scope-end, where the call span doesn't contain it). So only narrow
        // for variables; expressions fold every witness.
        let narrow_point = q
            .point
            .filter(|_| matches!(q.attachment, WitnessAttachment::Variable { .. }));
        // Narrowing: with a `point`, pick the narrowest-span
        // InferredType witness containing it (already post-narrowing).
        // Falls through to the full fold otherwise.
        if let Some(point) = narrow_point {
            let mut narrow: Option<(&Witness, u64)> = None;
            for w in ws {
                if let WitnessPayload::InferredType(_) = w.payload {
                    if span_contains(&w.span, point) && !span_is_zero(&w.span) {
                        let area = span_area(&w.span);
                        if narrow.map(|(_, a)| area < a).unwrap_or(true) {
                            narrow = Some((*w, area));
                        }
                    }
                }
            }
            if let Some((w, _)) = narrow {
                if let WitnessPayload::InferredType(t) = &w.payload {
                    return ReducedValue::Type(t.clone());
                }
            }
        }

        let mut class_assertion: Option<String> = None;
        let mut first_param_class: Option<String> = None;
        // A `BrandedRoute` is a class identity that carries extra
        // inherited-default data. It must dominate the bare
        // `ClassName(base)` companion that the same assignment also
        // pushes (so a partial route target reads the brand, not the
        // brandless class). Track the latest brand separately and
        // return it ahead of the class axis.
        let mut branded: Option<InferredType> = None;
        let mut rep_obs: Option<Rep> = None;
        let mut bless_rep: Option<Rep> = None;
        let mut num = false;
        let mut str_ = false;
        let mut re = false;
        let mut plain_type: Option<InferredType> = None;

        for w in ws {
            // Temporal ordering: only consider witnesses emitted at or
            // before the query point — a later reassignment shouldn't
            // influence a lookup at an earlier line.
            if let Some(point) = narrow_point {
                if w.span.start > point {
                    continue;
                }
            }
            // Skip scoped InferredType witnesses that don't contain the
            // query point — narrowing facts for a different slice of the
            // variable's lifetime.
            if let (Some(point), WitnessPayload::InferredType(_)) = (narrow_point, &w.payload) {
                if !span_is_zero(&w.span) && !span_contains(&w.span, point) {
                    continue;
                }
            }
            match &w.payload {
                WitnessPayload::InferredType(t) => match t {
                    InferredType::ClassName(name) => class_assertion = Some(name.clone()),
                    InferredType::FirstParam { package } => {
                        first_param_class = Some(package.clone())
                    }
                    b @ InferredType::BrandedRoute { .. } => branded = Some(b.clone()),
                    other => plain_type = Some(other.clone()),
                },
                WitnessPayload::Observation(obs) => match obs {
                    TypeObservation::ClassAssertion(name) => class_assertion = Some(name.clone()),
                    TypeObservation::FirstParamInMethod { package } => {
                        first_param_class = Some(package.clone())
                    }
                    TypeObservation::HashRefAccess => rep_obs = merge_rep(rep_obs, Rep::Hash),
                    TypeObservation::ArrayRefAccess => rep_obs = merge_rep(rep_obs, Rep::Array),
                    TypeObservation::CodeRefInvocation => rep_obs = merge_rep(rep_obs, Rep::Code),
                    TypeObservation::BlessTarget(r) => bless_rep = Some(*r),
                    TypeObservation::NumericUse => num = true,
                    TypeObservation::StringUse => str_ = true,
                    TypeObservation::RegexpUse => re = true,
                },
                _ => {}
            }
        }

        // A branded route dominates the bare-class companion: the
        // brand IS the class identity plus inherited defaults.
        if let Some(b) = branded {
            return ReducedValue::Type(b);
        }

        // Class axis wins when consistent with the rep axis. On
        // contradiction or unknown rep, still return the class — the
        // user's intent is object-typed use; a rep mismatch is a
        // separate diagnostic.
        if let Some(name) = class_assertion.clone().or(first_param_class.clone()) {
            let backing = bless_rep.or_else(|| q.framework.backing_rep());
            match (rep_obs, backing) {
                (None, _) => return ReducedValue::Type(InferredType::ClassName(name)),
                (Some(obs), Some(b)) if obs == b => {
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
                (Some(obs), None) => {
                    let _ = obs;
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
                (Some(obs), Some(b)) => {
                    let _ = (obs, b);
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
            }
        }

        // Explicit assignments dominate rep observations — `my $x = []`
        // overrides an earlier `$x->{k}` inference because reassignment
        // breaks the binding. Latest by iteration order (source order).
        if let Some(t) = plain_type {
            return ReducedValue::Type(t);
        }

        // No class evidence, no plain type — project rep observations.
        if let Some(r) = rep_obs.or(bless_rep) {
            return ReducedValue::Type(match r {
                Rep::Hash => InferredType::HashRef,
                Rep::Array => InferredType::ArrayRef,
                Rep::Code => InferredType::CodeRef { return_edge: None },
                Rep::Scalar => InferredType::String,
            });
        }

        // Scalar-context observations.
        if re {
            return ReducedValue::Type(InferredType::Regexp);
        }
        if num {
            return ReducedValue::Type(InferredType::Numeric);
        }
        if str_ {
            return ReducedValue::Type(InferredType::String);
        }

        ReducedValue::None
    }
}

fn span_contains(span: &Span, point: tree_sitter::Point) -> bool {
    span.start <= point && point <= span.end
}

fn span_is_zero(span: &Span) -> bool {
    span.start == span.end
}

/// "Area" measure — rows * many + cols. Used only for picking the
/// narrowest span; overflow isn't a concern for Perl source.
fn span_area(span: &Span) -> u64 {
    let rows = span.end.row.saturating_sub(span.start.row) as u64;
    if rows == 0 {
        span.end.column.saturating_sub(span.start.column) as u64
    } else {
        rows * 10_000 + (span.end.column as u64)
    }
}

fn merge_rep(existing: Option<Rep>, new: Rep) -> Option<Rep> {
    match existing {
        None => Some(new),
        Some(r) if r == new => Some(r),
        // Conflict shouldn't really fire; prefer the newer observation
        // and leave it for a later diagnostic.
        Some(_) => Some(new),
    }
}

// ---- Branch-arm fold reducer (ternary `$c ? A : B`) ----

/// Folds a ternary's per-arm types on the `BranchArm(span)` attachment.
/// Agreement across ≥2 arms → that type; a single arm → None (ternaries
/// always carry both arms syntactically, so one witness means inference
/// for the other arm failed). Claims by attachment shape — the ternary's
/// `Expr(span)` carries one `Edge(BranchArm(span))` so a query on the
/// expression materializes this fold's answer. Symbol-attached return
/// arms go through `SymbolReturnArmFold` instead (1+ arms rule).
pub struct BranchArmFold;

impl WitnessReducer for BranchArmFold {
    fn name(&self) -> &str {
        "branch_arm_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::BranchArm(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        let arms: Vec<&InferredType> = ws
            .iter()
            .filter_map(|w| match &w.payload {
                WitnessPayload::InferredType(t) => Some(t),
                _ => None,
            })
            .collect();
        if arms.len() < 2 {
            return ReducedValue::None;
        }
        let first = arms[0];
        if arms.iter().all(|t| *t == first) {
            return ReducedValue::Type(first.clone());
        }
        ReducedValue::None
    }
}

// ---- Symbol-attached return-arm fold ----
//
// Claims `SymbolReturnArm(sub_id)` attachments carrying `InferredType`
// payloads (Edges materialized into types). Each witness is one return
// arm; `resolve_return_type` agrees them (1 arm → that type, agreeing →
// that type, disagreeing → None, HashRef subsumed by Object).
// `Symbol(sub_id)` carries an `Edge(SymbolReturnArm(sub_id))` chain so
// consumers querying the symbol see the arm-fold answer via standard
// edge materialization.

pub struct SymbolReturnArmFold;

impl WitnessReducer for SymbolReturnArmFold {
    fn name(&self) -> &str {
        "symbol_return_arm_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::SymbolReturnArm(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        let arms: Vec<InferredType> = ws
            .iter()
            .filter_map(|w| match &w.payload {
                WitnessPayload::InferredType(t) => Some(t.clone()),
                _ => None,
            })
            .collect();
        match crate::file_analysis::resolve_return_type(&arms) {
            Some(t) => ReducedValue::Type(t),
            None => ReducedValue::None,
        }
    }
}

// Sub-return delegation chains (`return other()`, `shift->method(...)`)
// are `Edge(Symbol(...))` / `Edge(MethodOnClass{...})` payloads; registry
// materialization chases them, so no procedural delegation pass remains.
// `TypeProvenance::Delegation` is recorded at synthesis time by the
// emitter that pushes the Edge, and preserved across worklist iterations.

// ---- Expression reducer ----
//
// Claims `InferredType` payloads on `Expr(_)` — the unified
// expression-result attachment every rvalue publishes through. The
// walker pushes `Type(t)` directly or `Edge(target)` for resolution;
// edges are materialized by the registry before any reducer claims, so
// this reducer always sees plain types. Latest-wins: `emit_expr_witness`
// runs from multiple walk sites, so the same node may receive several
// witnesses; reading from the back picks the most recent.

pub struct ExprReturn;

impl WitnessReducer for ExprReturn {
    fn name(&self) -> &str {
        "expr_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Expr(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        for w in ws.iter().rev() {
            if let WitnessPayload::InferredType(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

// ---- Symbol return reducer ----
//
// Claims plain `InferredType` payloads on `Symbol(_)` — the id-keyed
// "what does THIS sym return?" answer. Pushed by writeback (local
// subs/methods) and hand-crafted test FileAnalyses. Class-scoped
// multi-overload dispatch goes through `MethodOnClass{class, name}`
// instead. Latest wins, so a later writeback re-publish dominates;
// registered AFTER every more-precise reducer (plugin override,
// ReturnExpr arity dispatch) so those claim first. Per-arm answers route
// through `SymbolReturnArm(_)`; `Symbol(sub_id)` carries an
// `Edge(SymbolReturnArm(_))` that materializes the arm-fold answer here.

pub struct SubReturnReducer;

impl WitnessReducer for SubReturnReducer {
    fn name(&self) -> &str {
        "sub_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        // `Symbol(_) + InferredType`, latest-wins. No source-tag filter
        // — the claim discriminator lives on the attachment shape.
        matches!(w.attachment, WitnessAttachment::Symbol(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        for w in ws.iter().rev() {
            if let WitnessPayload::InferredType(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

// ---- Class-keyed method-on-class reducer ----
//
// Claims `MethodOnClass{class, name}` carrying a plain `InferredType` —
// the class-scoped, name-keyed default return. `write_back_sub_return_types`
// publishes the primary as `Edge(Symbol(sid))` (materialized to a type
// before this reducer sees it). `ReturnExprReducer` runs first and
// handles arity-aware / receiver-relative dispatch; this fires only when
// no symbol-declarative ReturnExpr answers. Latest-wins: writeback clears
// its `local_return` / `plugin_bridge` / `inheritance` witnesses each
// fold iteration and re-publishes from current state.

pub struct MethodOnClassReducer;

impl WitnessReducer for MethodOnClassReducer {
    fn name(&self) -> &str {
        "method_on_class"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::MethodOnClass { .. })
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        for w in ws.iter().rev() {
            if let WitnessPayload::InferredType(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

// ---- Plugin-override priority reducer ----
//
// Claims `Symbol(_)` attachments with an `InferredType` from a
// high-priority source (Plugin). When such a witness exists it
// short-circuits the symbol-return fold — overrides dominate inference.
// Registered first so its short-circuit beats every inferred fold.
//
// Kept a distinct, named reducer (rather than a branch in
// FrameworkAwareTypeFold) so (a) the `claims` predicate only fires on a
// priority>10 witness, not every Symbol+InferredType fact, and (b)
// dump-package can attribute the answer to `plugin_override` specifically.

pub struct PluginOverrideReducer;

impl WitnessReducer for PluginOverrideReducer {
    fn name(&self) -> &str {
        "plugin_override"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Symbol(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
            && w.source.priority() > 10
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        // Highest priority wins; ties go to the last pushed.
        let mut best: Option<(&Witness, u8)> = None;
        for w in ws {
            let pr = w.source.priority();
            match best {
                None => best = Some((*w, pr)),
                Some((_, prev)) if pr >= prev => best = Some((*w, pr)),
                _ => {}
            }
        }
        if let Some((w, _)) = best {
            if let WitnessPayload::InferredType(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

// ---- Return-expression reducer ----
//
// Claims `Symbol(_)` and `MethodOnClass{...}` attachments carrying a
// `ReturnExpr(_)` payload — the symbol-declarative return machinery.
// Substitutes `q.receiver` for `Receiver`, dispatches `UnionOnArgs`
// against `q.arity_hint`, and evaluates `Operator(RowOf(_))`. Registered
// before `MethodOnClassReducer` / `SubReturnReducer` so declarative
// answers dominate primary-sym writeback.
//
// Per CLAUDE.md #10: no peeking at method names, classes, or payloads
// beyond `q.receiver` / `q.arity_hint` — the sub's policy lives entirely
// in the pushed `ReturnExpr`. Highest-priority source wins, ties to
// latest-pushed, so a plugin re-publish dominates a build-time inference.

pub struct ReturnExprReducer;

impl WitnessReducer for ReturnExprReducer {
    fn name(&self) -> &str {
        "return_expr"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(
            w.attachment,
            WitnessAttachment::Symbol(_) | WitnessAttachment::MethodOnClass { .. }
        ) && matches!(w.payload, WitnessPayload::ReturnExpr(_))
    }

    fn reduce(&self, ws: &[&Witness], q: &ReducerQuery) -> ReducedValue {
        // Highest-priority source wins; ties resolve to latest-pushed.
        let mut best: Option<(&Witness, u8)> = None;
        for w in ws.iter().rev() {
            let pr = w.source.priority();
            match best {
                None => best = Some((*w, pr)),
                Some((_, prev)) if pr > prev => best = Some((*w, pr)),
                _ => {}
            }
        }
        let Some((w, _)) = best else {
            return ReducedValue::None;
        };
        let WitnessPayload::ReturnExpr(re) = &w.payload else {
            return ReducedValue::None;
        };
        match eval_return_expr(re, q) {
            Some(t) => ReducedValue::Type(t),
            None => ReducedValue::None,
        }
    }
}

/// Evaluate a `ReturnExpr` against a query. Pure substitution — the only
/// context read is `q.receiver` / `q.arity_hint`. Returns `None` when:
///   - `Receiver` is encountered but `q.receiver` is `None`.
///   - No `UnionOnArgs` branch matches `q.arity_hint`.
///   - An operator's sub-expression evaluates to `None`, or to a type
///     the operator can't project (`RowOf(NotAResultSet)` → `None`).
fn eval_return_expr(re: &ReturnExpr, q: &ReducerQuery) -> Option<InferredType> {
    match re {
        ReturnExpr::Concrete(t) => Some(t.clone()),
        ReturnExpr::Receiver => q.receiver.clone(),
        ReturnExpr::Operator(op) => match op {
            ParametricOp::RowOf(inner) => {
                // Project eagerly: `RowOf` only has meaning over a
                // `ResultSet` (→ its row class). Any other operand has no
                // row dimension, so → `None`. A projected row is a plain
                // class with no further row dimension, so nested
                // `RowOf<RowOf<…>>` correctly bottoms out at `None`.
                match eval_return_expr(inner, q)? {
                    InferredType::Parametric(ParametricType::ResultSet { row, .. }) => {
                        Some(InferredType::ClassName(row))
                    }
                    _ => None,
                }
            }
        },
        ReturnExpr::UnionOnArgs { branches } => {
            // First-match wins when the hint is concrete.
            if q.arity_hint.is_some() {
                for (guard, sub) in branches {
                    if guard.matches(q.arity_hint) {
                        return eval_return_expr(sub, q);
                    }
                }
                return None;
            }
            // Hint-less query (introspection / class-keyed lookup with no
            // call site): prefer the `Any` arm (the union's catch-all),
            // else fall back to `Empty` (the typical primary for Mojo
            // `has` getter+writer pairs and DBIC accessors). Keeps
            // per-call-site dispatch strict while answering introspection.
            for (guard, sub) in branches {
                if matches!(guard, ArgGuard::Any) {
                    return eval_return_expr(sub, q);
                }
            }
            for (guard, sub) in branches {
                if matches!(guard, ArgGuard::Empty) {
                    return eval_return_expr(sub, q);
                }
            }
            None
        }
    }
}

// ---- Reducer registry ----

/// Cycle guard for recursive bag queries, keyed by
/// `(bag_ptr, attachment, receiver_disc, arity_hint)`. Per-bag entries
/// stay separate so a legitimate cross-bag query for the same attachment
/// (the common `MethodOnClass{C, m}` jump into C's own bag) isn't
/// misread as a cycle. The receiver discriminant + arity hint widen the
/// key so two queries differing only in `receiver` / `arity_hint` aren't
/// treated as duplicates — `UnionOnArgs` and `Receiver` substitution can
/// produce different answers, and the monotone bag makes re-asking safe.
type VisitedKey = (usize, WitnessAttachment, u8, Option<u32>);
type VisitedSet = std::collections::HashSet<VisitedKey>;

/// Per-top-level-`query` traversal state: the cycle guard plus a result
/// memo. The bag forms a DAG of edges; without memoization a diamond
/// (two paths reaching one shared sub-attachment) re-chases the shared
/// subtree on every path, which is exponential on dense files
/// (SQL::Abstract's method graph took minutes). The memo caches each
/// attachment's resolved value *for the duration of one top-level query*
/// so a re-reached node returns in O(1).
///
/// Soundness vs the cycle guard: `query_rec` only consults/stores the
/// memo for a key that is NOT currently on the path (the visited-guard
/// has already returned for on-path keys). A cached value is therefore
/// the node's resolution computed with that node off the path — exactly
/// what any other off-path reentry would compute. The memo is dropped
/// when the top-level query returns, so it never leaks state across
/// queries whose context (scopes / module_index / framework) differs.
struct QueryState {
    visited: VisitedSet,
    memo: std::collections::HashMap<VisitedKey, ReducedValue>,
}

impl QueryState {
    fn new() -> Self {
        QueryState {
            visited: std::collections::HashSet::new(),
            memo: std::collections::HashMap::new(),
        }
    }
}

/// Cycle-guard discriminant for `q.receiver` — `0` for `None`, else the
/// `InferredType` variant index. Values aren't load-bearing; only
/// "different variants compare unequal" matters. Exhaustive match (no
/// `_ =>`) so a new variant trips the compiler instead of colliding.
fn receiver_discriminant(r: &Option<InferredType>) -> u8 {
    let Some(t) = r else { return 0 };
    match t {
        InferredType::ClassName(_) => 1,
        InferredType::FirstParam { .. } => 2,
        InferredType::HashRef => 3,
        InferredType::ArrayRef => 4,
        InferredType::CodeRef { .. } => 5,
        InferredType::Regexp => 6,
        InferredType::Numeric => 7,
        InferredType::String => 8,
        InferredType::Parametric(_) => 9,
        InferredType::Sequence(_) => 10,
        InferredType::TypeConstraintOf(_) => 11,
        InferredType::BrandedRoute { .. } => 12,
    }
}

/// Depth backstop for `query_rec`. The `(bag, attachment)` visited set is
/// the primary cycle guard; this cap is belt-and-braces against a new,
/// unaccounted-for recursion shape blowing the stack. On hit, log once
/// per process and return `None` (give up cleanly rather than abort).
const QUERY_REC_DEPTH_CAP: u32 = 512;

thread_local! {
    static QUERY_REC_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
    /// One-shot so we don't flood stderr while a deep walk unwinds.
    static QUERY_REC_DEPTH_WARNED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

#[derive(Default)]
pub struct ReducerRegistry {
    reducers: Vec<Box<dyn WitnessReducer>>,
}

impl ReducerRegistry {
    pub fn new() -> Self {
        Self { reducers: Vec::new() }
    }

    pub fn with_defaults() -> Self {
        let mut r = Self::new();
        // Order is load-bearing — earlier reducers claim first.
        // Plugin overrides short-circuit before any inferred fold.
        r.register(Box::new(PluginOverrideReducer));
        // ReturnExpr is symbol-declarative — before every value-side
        // reducer so a sub's declared shape (Mojo `has`'s UnionOnArgs,
        // DBIC `find`'s Operator(RowOf, Receiver)) wins over per-arity
        // observations or primary-sym writeback.
        r.register(Box::new(ReturnExprReducer));
        // SymbolReturnArmFold claims the dedicated `SymbolReturnArm(_)`
        // shape; single-arm answers surface here, where BranchArmFold's
        // ≥2-arm rule would reject them.
        r.register(Box::new(SymbolReturnArmFold));
        // BranchArmFold claims the dedicated `BranchArm(_)` shape, so its
        // order relative to the Variable/Expr folds below is no longer
        // load-bearing — they no longer overlap.
        r.register(Box::new(BranchArmFold));
        r.register(Box::new(FrameworkAwareTypeFold));
        r.register(Box::new(ExprReturn));
        // MethodOnClass primary-fallback after ReturnExprReducer so
        // per-arity declarations win when one matches.
        r.register(Box::new(MethodOnClassReducer));
        // Last — fallback for "this Symbol's stored return type".
        r.register(Box::new(SubReturnReducer));
        r
    }

    pub fn register(&mut self, r: Box<dyn WitnessReducer>) {
        self.reducers.push(r);
    }

    /// Query the registry for the first reducer returning a non-`None`
    /// value. Edge materialization runs first: `Edge(target)` witnesses
    /// on the queried attachment are chased via recursive query and
    /// replaced by synthetic `InferredType` witnesses (preserving source
    /// + span) before reducers see the list, so edges compose with
    /// existing reducers without reducer-side awareness.
    ///
    /// The cycle guard is threaded across both edge chases (within one
    /// bag) and the inheritance fallback (which crosses bags), closing
    /// mutual-inheritance loops that span files.
    pub fn query(&self, bag: &WitnessBag, q: &ReducerQuery) -> ReducedValue {
        let mut state = QueryState::new();
        self.query_rec(bag, q, &mut state)
    }

    fn query_rec(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        state: &mut QueryState,
    ) -> ReducedValue {
        let depth = QUERY_REC_DEPTH.with(|c| {
            let d = c.get();
            c.set(d + 1);
            d
        });
        if depth >= QUERY_REC_DEPTH_CAP {
            QUERY_REC_DEPTH_WARNED.with(|w| {
                if !w.get() {
                    w.set(true);
                    eprintln!(
                        "perl-lsp: query_rec depth cap ({}) hit on attachment {:?} — \
                         returning None to avoid stack overflow. \
                         This indicates an un-broken recursion path; \
                         please report.",
                        QUERY_REC_DEPTH_CAP, q.attachment,
                    );
                }
            });
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return ReducedValue::None;
        }
        let key: VisitedKey = (
            bag as *const _ as usize,
            q.attachment.clone(),
            receiver_discriminant(&q.receiver),
            q.arity_hint,
        );
        // Memo hit: this key was fully resolved earlier in THIS query and
        // isn't on the current path (cycle guard handles on-path keys).
        if let Some(cached) = state.memo.get(&key) {
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return cached.clone();
        }
        if !state.visited.insert(key.clone()) {
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return ReducedValue::None;
        }
        let result = self.query_rec_body(bag, q, state);
        state.visited.remove(&key);
        // Cache the off-path resolution. The query depends only on
        // `(bag, attachment, receiver-class, arity)` (all in `key`) plus
        // the static context, which is fixed for one top-level query.
        state.memo.insert(key, result.clone());
        QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
        result
    }

    fn query_rec_body(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        state: &mut QueryState,
    ) -> ReducedValue {
        let materialized = self.materialize(bag, q, state);

        for r in &self.reducers {
            let claimed: Vec<&Witness> =
                materialized.iter().filter(|w| r.claims(w)).collect();
            if claimed.is_empty() {
                continue;
            }
            let v = r.reduce(&claimed, q);
            if v != ReducedValue::None {
                return v;
            }
        }

        // Inheritance + bridge fallback for `MethodOnClass{C, m}` queries
        // the local bag couldn't answer. Most cases are covered by
        // build-time edge emission (local writeback emits
        // `MethodOnClass(child, m) → Edge(MethodOnClass(parent, m))`;
        // enrichment projects the same for cross-file parents), resolved
        // by the generic edge-chase. This fallback covers the residual:
        // hand-crafted FAs / isolated tests, and cross-file
        // plugin-namespace bridges declared in other files. Three
        // structural facts compose:
        //
        //   1. `module_index.get_cached(C)` — when `C` lives in another
        //      file, recurse into its cached bag for C's direct facts.
        //   2. `package_parents[C]` (local) ∪ `parents_cached(C)`
        //      (cross-file) — the Perl DFS-MRO chain; recurse on
        //      `MethodOnClass{P, m}` per parent.
        //   3. `for_each_entity_bridged_to(class, ...)` — entities in
        //      other files' plugin namespaces bridged to `class`; query
        //      each cached bag by `Symbol(sym.id)` (per-FA SymbolIds
        //      can't be portably edge-encoded).
        //
        // The shared visited set breaks local and cross-file cycles.
        if let WitnessAttachment::MethodOnClass { class, name } = q.attachment {
            if let Some(ctx) = q.context {
                // (1) Cross-file primary lookup.
                if let Some(idx) = ctx.module_index {
                    if let Some(cached) = idx.get_cached(class) {
                        if !std::ptr::eq(bag, &cached.analysis.witnesses) {
                            let cached_ctx = BagContext {
                                scopes: &cached.analysis.scopes,
                                package_framework: &cached.analysis.package_framework,
                                module_index: Some(idx),
                                package_parents: &cached.analysis.package_parents,
                                app_surface_consumers: &cached.analysis.app_surface_consumers,
                            };
                            let sub_q = ReducerQuery {
                                attachment: q.attachment,
                                point: q.point,
                                framework: q.framework,
                                arity_hint: q.arity_hint,
                                receiver: q.receiver.clone(),
                                context: Some(&cached_ctx),
                            };
                            let v = self.query_rec(
                                &cached.analysis.witnesses,
                                &sub_q,
                                state,
                            );
                            if v != ReducedValue::None {
                                return v;
                            }
                        }
                    }
                }
                // (2) Inheritance walk via package_parents (local ∪
                // cross-file ∪ synthetic app-surface edge — `parents_of`
                // is the single edge-injection site shared with the
                // FA-side ancestor walks).
                let parents = crate::file_analysis::parents_of(
                    class,
                    ctx.package_parents,
                    ctx.module_index,
                    ctx.app_surface_consumers,
                );
                for p in parents {
                    let parent_att = WitnessAttachment::MethodOnClass {
                        class: p,
                        name: name.clone(),
                    };
                    let sub_q = ReducerQuery {
                        attachment: &parent_att,
                        point: q.point,
                        framework: q.framework,
                        arity_hint: q.arity_hint,
                        receiver: q.receiver.clone(),
                        context: q.context,
                    };
                    let v = self.query_rec(bag, &sub_q, state);
                    if v != ReducedValue::None {
                        return v;
                    }
                }
                // (3) Cross-file plugin-namespace bridges. Plugin entities
                // declared in OTHER files bridged to `class` aren't
                // reachable via the local bag's edges nor the cross-file
                // primary (`get_cached(class)` returns the canonical class
                // file, not the bridging-plugin file). Ask each matching
                // cached entity for `Symbol(sym.id)` at arity=None —
                // bridged Methods aren't arity-discriminated.
                if let Some(idx) = ctx.module_index {
                    let mut found: Option<InferredType> = None;
                    idx.for_each_entity_bridged_to(class, |_mod, cached, sym| {
                        if found.is_some() {
                            return;
                        }
                        if !matches!(
                            sym.kind,
                            crate::file_analysis::SymKind::Sub
                                | crate::file_analysis::SymKind::Method
                        ) {
                            return;
                        }
                        if &sym.name != name {
                            return;
                        }
                        if let Some(t) = cached
                            .analysis
                            .symbol_return_type_via_bag(sym.id, None)
                        {
                            found = Some(t);
                        }
                    });
                    if let Some(t) = found {
                        return ReducedValue::Type(t);
                    }
                }
            }
        }

        ReducedValue::None
    }

    /// Resolve every Edge witness on `q.attachment` to an `InferredType`
    /// witness via recursive query; non-edge witnesses pass through. The
    /// returned list is fresh-owned so reducers can borrow into it.
    ///
    /// `Edge(Variable{...})` targets are special-cased — variable
    /// resolution needs a scope-chain walk + the scope's framework. With
    /// a `BagContext`, this delegates to `query_variable_with_visited` so
    /// the recursion shares the caller's cycle guard (calling the public
    /// `query_variable_type` would reset visited and reopen mutual
    /// `Edge(Variable)` loops).
    fn materialize(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        state: &mut QueryState,
    ) -> Vec<Witness> {
        let raw = bag.for_attachment(q.attachment);
        let mut out: Vec<Witness> = Vec::with_capacity(raw.len());
        for w in raw {
            match &w.payload {
                WitnessPayload::Edge(target) => {
                    let resolved = match (target, q.context) {
                        (
                            WitnessAttachment::Variable { name, scope },
                            Some(ctx),
                        ) => {
                            let point = scope_point(ctx.scopes, *scope);
                            self.query_variable_with_visited(
                                bag,
                                ctx.scopes,
                                ctx.package_framework,
                                ctx.package_parents,
                                ctx.app_surface_consumers,
                                ctx.module_index,
                                name,
                                *scope,
                                point,
                                state,
                            )
                        }
                        _ => {
                            // A `MethodOnClass{class,..}` reached through an edge is
                            // a fresh method dispatch: its receiver is that call's
                            // invocant (`class`), so a fluent `ReturnExpr(Receiver)`
                            // substitutes the dispatch class — not whatever the outer
                            // query carried. Mirrors `query_sub_return_type`'s
                            // `effective_receiver`. The exception is an inheritance
                            // hop (`MethodOnClass{child} → Edge(MethodOnClass{parent})`):
                            // there the source is itself a `MethodOnClass`, and the
                            // child's receiver must carry through so an inherited fluent
                            // accessor returns the child, not where `has` was declared.
                            let receiver = match target {
                                WitnessAttachment::MethodOnClass { class, .. }
                                    if !matches!(
                                        q.attachment,
                                        WitnessAttachment::MethodOnClass { .. }
                                    ) =>
                                {
                                    Some(InferredType::ClassName(class.clone()))
                                }
                                _ => q.receiver.clone(),
                            };
                            let sub_q = ReducerQuery {
                                attachment: target,
                                point: q.point,
                                framework: q.framework,
                                arity_hint: q.arity_hint,
                                receiver,
                                context: q.context,
                            };
                            if let ReducedValue::Type(t) = self.query_rec(bag, &sub_q, state) {
                                Some(t)
                            } else {
                                None
                            }
                        }
                    };
                    if let Some(t) = resolved {
                        out.push(Witness {
                            attachment: w.attachment.clone(),
                            source: w.source.clone(),
                            payload: WitnessPayload::InferredType(t),
                            span: w.span,
                        });
                    }
                    // An edge that didn't resolve drops out — same as a
                    // witness no reducer claims.
                }
                WitnessPayload::CallReturn { target, arity } => {
                    // A fresh method dispatch at the call's own arity. The
                    // receiver is the dispatch class (`target`'s class, for
                    // a `MethodOnClass`) so a fluent `Receiver` substitutes
                    // it; the arity is the call site's, NOT the outer
                    // query's — that's the whole point of this variant.
                    let receiver = match target {
                        WitnessAttachment::MethodOnClass { class, .. } => {
                            Some(InferredType::ClassName(class.clone()))
                        }
                        _ => q.receiver.clone(),
                    };
                    let sub_q = ReducerQuery {
                        attachment: target,
                        point: q.point,
                        framework: q.framework,
                        arity_hint: Some(*arity),
                        receiver,
                        context: q.context,
                    };
                    if let ReducedValue::Type(t) = self.query_rec(bag, &sub_q, state) {
                        out.push(Witness {
                            attachment: w.attachment.clone(),
                            source: w.source.clone(),
                            payload: WitnessPayload::InferredType(t),
                            span: w.span,
                        });
                    }
                }
                _ => out.push(w.clone()),
            }
        }
        out
    }

    /// Scope-chain variable lookup with an explicit visited set.
    /// `query_variable_type` is the public entry; this is the inner loop,
    /// factored out so callers already inside a `query_rec` recursion
    /// (currently `materialize` for `Edge(Variable)`) can thread their
    /// cycle guard through, closing mutual `$a → $b → $a` edge cycles.
    fn query_variable_with_visited(
        &self,
        bag: &WitnessBag,
        scopes: &[Scope],
        package_framework: &HashMap<String, FrameworkFact>,
        package_parents: &HashMap<String, Vec<String>>,
        app_surface_consumers: &[String],
        module_index: Option<&crate::module_index::ModuleIndex>,
        var: &str,
        scope: ScopeId,
        point: Point,
        state: &mut QueryState,
    ) -> Option<InferredType> {
        let mut chain: Vec<ScopeId> = Vec::new();
        let mut cur = Some(scope);
        while let Some(sid) = cur {
            chain.push(sid);
            cur = scopes[sid.0 as usize].parent;
        }
        let framework = chain
            .iter()
            .find_map(|sid| scopes[sid.0 as usize].package.as_ref())
            .and_then(|pkg| package_framework.get(pkg).copied())
            .unwrap_or(FrameworkFact::Plain);
        // Preserve the caller's index + parents. Resolving a variable whose
        // value is a cross-file method chain needs both; rebuilding them empty
        // here is what dropped the index mid-chase and made `Variable` the lone
        // attachment that couldn't resolve across files.
        let ctx = BagContext {
            scopes,
            package_framework,
            module_index,
            package_parents,
            app_surface_consumers,
        };
        for sid in chain {
            let att = WitnessAttachment::Variable {
                name: var.to_string(),
                scope: sid,
            };
            let q = ReducerQuery {
                attachment: &att,
                point: Some(point),
                framework,
                arity_hint: None,
                receiver: None,
                context: Some(&ctx),
            };
            if let ReducedValue::Type(t) = self.query_rec(bag, &q, state) {
                return Some(t);
            }
        }
        None
    }
}

/// Pick the "where am I asking from?" `Point` for a scope-chained
/// Variable query. The scope's end position works for temporal
/// narrowing; materialize doesn't have the chasing witness's span, so
/// this is a safe approximation.
fn scope_point(scopes: &[Scope], scope: ScopeId) -> tree_sitter::Point {
    scopes
        .get(scope.0 as usize)
        .map(|s| s.span.end)
        .unwrap_or(tree_sitter::Point { row: 0, column: 0 })
}

// ---- Single shared query entrypoints ----
//
// Both the in-builder return fold and `FileAnalysis`'s public queries go
// through these helpers, so the scope-chain walk + framework lookup +
// reducer dispatch lives in exactly one place.

/// Canonical query for "what does this sub return?". Handles local subs
/// (resolved via the `symbols` table to a `Symbol` attachment) and
/// imported / cross-file subs (resolved through
/// `BagContext.module_index`'s exporter index → recurse into the cached
/// module's bag). Callers don't branch on "is this local".
pub fn query_sub_return_type(
    bag: &WitnessBag,
    symbols: &[crate::file_analysis::Symbol],
    sub_name: &str,
    arity_hint: Option<u32>,
    receiver: Option<InferredType>,
    context: Option<&BagContext>,
) -> Option<InferredType> {
    let reg = ReducerRegistry::with_defaults();
    // Local-symbol query first — `ReturnExprReducer` picks up any
    // arity-discriminated `UnionOnArgs` on the matching sym.
    let local_sym = symbols.iter().find(|s| {
        s.name == sub_name
            && matches!(
                s.kind,
                crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
            )
    });
    if let Some(sym) = local_sym {
        let att = WitnessAttachment::Symbol(sym.id);
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint,
            receiver: receiver.clone(),
            context,
        };
        if let ReducedValue::Type(t) = reg.query(bag, &q) {
            return Some(t);
        }
        // Cross-symbol dispatch within the sym's class (Mojo::Base
        // getter+writer share a name; at arity=1 the writer's answer is
        // required). `MethodOnClass{class, name}` carries every per-arity
        // arm synthesis published.
        if let Some(class) = sym.package.as_ref() {
            // Default receiver for class-keyed lookup: when the caller
            // didn't pass one, fall back to `ClassName(class)` — the Mojo
            // writer's `Receiver` then evaluates to the fluent return,
            // matching what `$obj->writer()` would produce. A supplied
            // receiver passes through.
            let att = WitnessAttachment::MethodOnClass {
                class: class.clone(),
                name: sub_name.to_string(),
            };
            let effective_receiver = receiver
                .clone()
                .or_else(|| Some(InferredType::ClassName(class.clone())));
            let q = ReducerQuery {
                attachment: &att,
                point: None,
                framework: FrameworkFact::Plain,
                arity_hint,
                receiver: effective_receiver,
                context,
            };
            if let ReducedValue::Type(t) = reg.query(bag, &q) {
                return Some(t);
            }
        }
    }
    // Cross-file imports: walk the module_index for exporters of
    // `sub_name` and recurse into each cached bag for the matching
    // `Symbol`. The recursion shares the registry — same arity dispatch,
    // overrides, and fold rules; only the bag and symbols change.
    if let Some(ctx) = context {
        if let Some(idx) = ctx.module_index {
            for module_name in idx.find_exporters(sub_name) {
                let Some(cached) = idx.get_cached(&module_name) else { continue };
                let Some(sym) = cached.analysis.symbols.iter().find(|s| {
                    s.name == sub_name
                        && matches!(
                            s.kind,
                            crate::file_analysis::SymKind::Sub
                                | crate::file_analysis::SymKind::Method
                        )
                }) else {
                    continue;
                };
                let cached_ctx = BagContext {
                    scopes: &cached.analysis.scopes,
                    package_framework: &cached.analysis.package_framework,
                    module_index: Some(idx),
                    package_parents: &cached.analysis.package_parents,
                    app_surface_consumers: &cached.analysis.app_surface_consumers,
                };
                let att = WitnessAttachment::Symbol(sym.id);
                let q = ReducerQuery {
                    attachment: &att,
                    point: None,
                    framework: FrameworkFact::Plain,
                    arity_hint,
                    receiver: receiver.clone(),
                    context: Some(&cached_ctx),
                };
                if let ReducedValue::Type(t) = reg.query(&cached.analysis.witnesses, &q) {
                    return Some(t);
                }
            }
        }
    }
    None
}

/// Walk the scope chain from `scope` upward, folding every Variable
/// witness for `var`; returns the first scope that produces a typed
/// answer, else `None`.
///
/// Public entry: starts a fresh cycle-guard set. Recursive callers
/// already inside `query_rec` must use `query_variable_with_visited`
/// instead so the shared visited set catches mutual `Edge(Variable)`
/// loops.
pub fn query_variable_type(
    bag: &WitnessBag,
    scopes: &[Scope],
    package_framework: &HashMap<String, FrameworkFact>,
    package_parents: &HashMap<String, Vec<String>>,
    app_surface_consumers: &[String],
    module_index: Option<&crate::module_index::ModuleIndex>,
    var: &str,
    scope: ScopeId,
    point: Point,
) -> Option<InferredType> {
    let reg = ReducerRegistry::with_defaults();
    let mut state = QueryState::new();
    reg.query_variable_with_visited(
        bag,
        scopes,
        package_framework,
        package_parents,
        app_surface_consumers,
        module_index,
        var,
        scope,
        point,
        &mut state,
    )
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------

#[cfg(test)]
#[path = "witnesses_tests.rs"]
mod tests;
