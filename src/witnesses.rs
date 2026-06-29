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
    /// (like `SymbolReturnArm`) so `BranchArmFold` claims by attachment
    /// and the shared `Expr` / `Variable` reducers never see arm
    /// witnesses.
    BranchArm(Span),
    /// Typed-slot collector: "what type does instance slot `key` hold on
    /// class `class`?" Seeded from typed hash-key WRITEs
    /// (`$obj->{key} = <rhs>`) as one `Edge(Expr(rhs_span))` per write;
    /// `SlotTypeFold` agrees the arms via `resolve_return_type` (1+ agree
    /// → that type, disagree → None). Class-keyed so `$self->{h}` and a
    /// differently-typed `$other->{h}` don't cross-contaminate. Nothing
    /// consumes this yet — `$obj->{h}->m()` typing through it is a later
    /// step.
    SlotType { class: String, key: String },
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
    /// **Explicitly-qualified method dispatch** — the method token carried a
    /// `::` so Perl dispatches from a *named* class, not the invocant's: look
    /// the method up on `method_lookup` but type the result relative to
    /// `receiver_class` (the invocant / enclosing class). Two spellings, one
    /// rule (see `emit_method_call_return_edges`):
    ///   - `$obj->SUPER::m` → `method_lookup` is `MethodOnClass{<enclosing
    ///     package's parent>, m}` (SUPER searches the *writing* package's
    ///     `@ISA`, skipping it);
    ///   - `$obj->Foo::Bar::m` → `method_lookup` is `MethodOnClass{Foo::Bar,
    ///     m}` (fully-qualified: search starts at the named class).
    /// In both, the call still blesses into the CALLER's class, so a ctor
    /// returning `ReturnExpr::ReceiverOr` must substitute the invocant — the
    /// dynamic outer receiver wins when it is a subclass of `receiver_class`.
    /// (A plain `CallReturn` can't express this: its receiver defaults to the
    /// dispatch class, which here is the parent / named class — wrong.)
    QualifiedCallReturn {
        method_lookup: WitnessAttachment,
        receiver_class: String,
        arity: u32,
    },
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
    /// Edge fact with a projection: "the value at my attachment is the
    /// `step`-projection of whatever resolves at `base`." Emitted for
    /// `expr->{key}` / `expr->[N]` expressions so the drill participates
    /// in the edge graph — the chase materializes `base` at QUERY time
    /// (when cross-file knowledge like an imported literal's
    /// `HashWithKeys` is in hand) and narrows through the step. Kept at
    /// the END for bincode variant-index stability (bump
    /// `EXTRACT_VERSION`).
    Projected {
        base: WitnessAttachment,
        step: ProjectionStep,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProjectionStep {
    HashKey(String),
    ArrayIndex(i32),
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
    /// Receiver-polymorphic constructor return: the call-site invocant's
    /// class, else the carried fallback when there is no receiver. This is
    /// the `bless {}, $class` / `bless {}, ref $self || $self` idiom — an
    /// inherited constructor returns whatever class it was *called on*
    /// (`Child->new` → `Child`), so it must substitute the receiver; the
    /// fallback (the enclosing class) keeps bare `sub_return_type` queries
    /// answering instead of going `None`. Composes through `SUPER::new`.
    ReceiverOr(InferredType),
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

    /// Does `att` carry a witness sourced from `Builder(tag)`? Used to ask
    /// "was this variable's type written EXPLICITLY" (`skeleton-annot`) vs
    /// inferred — the inlay-hint suppression for languages with explicit
    /// types (`int c` needs no `: int` hint; `auto x` does).
    pub fn has_builder_source(&self, att: &WitnessAttachment, tag: &str) -> bool {
        self.index.get(att).is_some_and(|idxs| {
            idxs.iter().any(|&i| {
                matches!(&self.witnesses[i].source, WitnessSource::Builder(s) if s == tag)
            })
        })
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
    pub module_index: Option<&'a dyn crate::file_analysis::CrossFileLookup>,
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

        // Class assertions break ties on source priority first, then
        // iteration order — a `Plugin`-sourced assertion (the helper-`$c`
        // override) dominates a `Builder` one (`my $c = shift` typed as the
        // enclosing class). Same axis as `PluginOverrideReducer` on Symbols.
        let mut class_assertion: Option<String> = None;
        let mut class_assertion_priority: u8 = 0;
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
            let prio = w.source.priority();
            match &w.payload {
                WitnessPayload::InferredType(t) => match t {
                    InferredType::ClassName(name) => {
                        if prio >= class_assertion_priority {
                            class_assertion = Some(name.clone());
                            class_assertion_priority = prio;
                        }
                    }
                    InferredType::FirstParam { package } => {
                        first_param_class = Some(package.clone())
                    }
                    b @ InferredType::BrandedRoute { .. } => branded = Some(b.clone()),
                    // Latest wins UNLESS the standing answer subsumes the
                    // newcomer — structure dominates rep (`HashWithKeys` is
                    // not downgraded by a deref's re-derived `HashRef`),
                    // mirroring the class-dominates-rep rule below.
                    other => {
                        if !plain_type
                            .as_ref()
                            .is_some_and(|have| have.subsumes_narrowing(other))
                        {
                            plain_type = Some(other.clone());
                        }
                    }
                },
                WitnessPayload::Observation(obs) => match obs {
                    TypeObservation::ClassAssertion(name) => {
                        if prio >= class_assertion_priority {
                            class_assertion = Some(name.clone());
                            class_assertion_priority = prio;
                        }
                    }
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
        if !matches!(w.attachment, WitnessAttachment::BranchArm(_)) {
            return false;
        }
        match &w.payload {
            WitnessPayload::InferredType(_) => true,
            WitnessPayload::Fact { family, .. } => family == "undef_arm",
            _ => false,
        }
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        let mut typed: Vec<InferredType> = Vec::new();
        let mut undef_arms = 0usize;
        for w in ws {
            match &w.payload {
                WitnessPayload::InferredType(t) => typed.push(t.clone()),
                WitnessPayload::Fact { family, .. } if family == "undef_arm" => undef_arms += 1,
                _ => {}
            }
        }
        // Both arms must have contributed — the ≥2 rule guards a single
        // materialized arm from masquerading as agreement.
        if typed.len() + undef_arms < 2 {
            return ReducedValue::None;
        }
        // Strict agreement among the typed arms (a ternary wants exact
        // agreement, NOT the loose hash/object subsumption the return-arm
        // join uses). An `undef` arm then lifts the agreed `T` to
        // `Optional<T>`.
        let agreed = match typed.split_first() {
            Some((first, rest)) if rest.iter().all(|t| t == first) => Some(first.clone()),
            _ => None,
        };
        match agreed {
            Some(t) if undef_arms > 0 && !matches!(t, InferredType::Optional(_)) => {
                ReducedValue::Type(InferredType::Optional(Box::new(t)))
            }
            Some(t) => ReducedValue::Type(t),
            None => ReducedValue::None,
        }
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
        if !matches!(w.attachment, WitnessAttachment::SymbolReturnArm(_)) {
            return false;
        }
        match &w.payload {
            WitnessPayload::InferredType(_) => true,
            // The `return undef` arm marker (no rvalue type to materialize).
            WitnessPayload::Fact { family, .. } => family == "undef_arm",
            _ => false,
        }
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        let mut arms: Vec<InferredType> = Vec::new();
        let mut has_undef_arm = false;
        for w in ws {
            match &w.payload {
                WitnessPayload::InferredType(t) => arms.push(t.clone()),
                WitnessPayload::Fact { family, .. } if family == "undef_arm" => {
                    has_undef_arm = true
                }
                _ => {}
            }
        }
        match crate::file_analysis::join_return_arms(&arms, has_undef_arm) {
            Some(t) => ReducedValue::Type(t),
            None => ReducedValue::None,
        }
    }
}

// ---- Typed-slot fold ----
//
// Claims `SlotType{class, key}` attachments carrying `InferredType`
// payloads (per-write `Edge(Expr(rhs_span))` witnesses, materialized to
// types by the registry). Each witness is one `$obj->{key} = <rhs>`
// WRITE; the per-write arms agree via `resolve_return_type` (1+ agree →
// that type, HashRef subsumed by an Object) with one added guard: two
// DISTINCT concrete classes are honest disagreement → None. The guard
// matters here because `resolve_return_type`'s Object/HashRef
// subsumption was tuned for return arms (one Object absorbs sibling
// HashRefs) and otherwise picks the last of two different classes —
// the wrong answer when `$self->{h} = A->new` in one method and
// `= B->new` in another genuinely conflict. Nothing consumes this
// attachment yet — it's the typed half of the hash-key-write seed,
// paired with the untyped `mutation` Fact.

pub struct SlotTypeFold;

impl WitnessReducer for SlotTypeFold {
    fn name(&self) -> &str {
        "slot_type_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::SlotType { .. })
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
        // Two distinct class identities never agree: a slot can't be
        // both `A` and `B`. `class_name()` is the value's own "what
        // class am I" answer (rule #10) — distinct answers → None.
        let mut seen_class: Option<String> = None;
        for t in &arms {
            if let Some(cn) = t.class_name() {
                match &seen_class {
                    None => seen_class = Some(cn.to_string()),
                    Some(prev) if prev != cn => return ReducedValue::None,
                    _ => {}
                }
            }
        }
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
        // The policy lives on the payload, not the attachment: a
        // `ReturnExpr(_)` is a deferred (receiver/arity)-relative return
        // wherever it's pushed. `Symbol(_)` / `MethodOnClass{..}` carry
        // the class-keyed declarations; `Expr(_)` carries a method body's
        // own deferred return (`sub me { return $_[0] }` → `Receiver` on
        // the `$_[0]` body span), reached through the `SymbolReturnArm`
        // edge chase. Claiming all three lets a self-returning method
        // substitute the call's receiver at arbitrary chain depth.
        matches!(
            w.attachment,
            WitnessAttachment::Symbol(_)
                | WitnessAttachment::MethodOnClass { .. }
                | WitnessAttachment::Expr(_)
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
        ReturnExpr::ReceiverOr(fallback) => {
            Some(q.receiver.clone().unwrap_or_else(|| fallback.clone()))
        }
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

/// Cycle guard + result memo key for recursive bag queries, keyed by
/// `(bag_ptr, attachment, receiver_identity, arity_hint)`. Per-bag
/// entries stay separate so a legitimate cross-bag query for the same
/// attachment (the common `MethodOnClass{C, m}` jump into C's own bag)
/// isn't misread as a cycle. The receiver **identity** + arity hint
/// widen the key so two queries differing only in `receiver` /
/// `arity_hint` aren't treated as duplicates — `UnionOnArgs` and
/// `Receiver` substitution can produce different answers.
///
/// The receiver slot is the receiver's FULL structural identity, not a
/// variant tag. `ReturnExpr::Receiver` substitutes the whole receiver,
/// so `ClassName("Foo")` and `ClassName("Bar")` reaching one attachment
/// resolve to different classes; a variant-only discriminant collapses
/// them to one memo key and the memo hands Foo's answer to Bar (silent
/// wrong type). A same-receiver diamond (the inheritance walk holds
/// `q.receiver` constant within one `MethodOnClass` query) still hashes
/// to one key, so memoization still kills the exponential re-chase.
type VisitedKey = (usize, WitnessAttachment, Option<String>, Option<u32>);
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
    // `Arc` so a memo store/hit clones one heap pointer, not the
    // (String-bearing) `ReducedValue`. `HashMap::new()` pre-allocates
    // no buckets, so a shallow query that never re-reaches a node (the
    // common hover/completion 1–2-hop case) pays nothing for the memo —
    // the table is lazily allocated on the first insert.
    memo: std::collections::HashMap<VisitedKey, std::sync::Arc<ReducedValue>>,
}

impl QueryState {
    fn new() -> Self {
        QueryState {
            visited: std::collections::HashSet::new(),
            memo: std::collections::HashMap::new(),
        }
    }
}

/// Hashable full-identity projection of `q.receiver` for the cycle/memo
/// key. `None` stays `None`; otherwise the receiver's complete structural
/// identity (Debug projection) so two distinct receivers — including two
/// `ClassName(_)` with different class names — never share a key. This is
/// the soundness-load-bearing slot: `ReturnExpr::Receiver` substitutes the
/// whole receiver, so the memo must keep different receivers apart. Debug
/// is structurally faithful for every `InferredType` variant (each field
/// is itself `Debug`), so equality of the string implies equality of the
/// receiver for keying purposes.
fn receiver_key(r: &Option<InferredType>) -> Option<String> {
    r.as_ref().map(|t| format!("{t:?}"))
}

/// Receiver to substitute when a chase reaches a *fresh* method dispatch
/// on `MethodOnClass{class}` (an `Edge` or `CallReturn` into a class's
/// method): the receiver is that call's invocant, i.e. `class`. A fluent
/// `ReturnExpr(Receiver)` substitutes the dispatch class.
///
/// But when the outer query already carries the invocant's *resolved
/// value* and that value's class identity IS `class`, prefer the richer
/// value — it carries parametric structure (`Parametric(ResultSet{base,
/// row})`) that a bare `ClassName(class)` drops, which is exactly what
/// `Operator(RowOf(Receiver))` (DBIC `find`) needs to project the row
/// class. Same class, strictly more information; the value answers the
/// projection (rule #10), the chase never inspects the shape.
fn fresh_dispatch_receiver(
    incoming: &Option<InferredType>,
    class: &str,
    ctx: Option<&BagContext>,
) -> Option<InferredType> {
    if let Some(t) = incoming {
        if let Some(cn) = t.class_name() {
            // Preserve a receiver that IS the dispatch class — or a SUBCLASS of
            // it (SUPER:: dispatch, inherited methods): more specific, still valid.
            if cn == class || ctx.is_some_and(|c| is_subclass_of(cn, class, c)) {
                return Some(t.clone());
            }
        }
    }
    Some(InferredType::ClassName(class.to_string()))
}

/// Is `child` a (transitive) subclass of `ancestor`? Bounded BFS over `parents_of`.
fn is_subclass_of(child: &str, ancestor: &str, ctx: &BagContext) -> bool {
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut queue: std::collections::VecDeque<String> =
        std::collections::VecDeque::from([child.to_string()]);
    let mut steps = 0;
    while let Some(c) = queue.pop_front() {
        steps += 1;
        if steps > 64 {
            break;
        }
        if !seen.insert(c.clone()) {
            continue;
        }
        for p in crate::file_analysis::parents_of(
            &c,
            ctx.package_parents,
            ctx.module_index,
            ctx.app_surface_consumers,
        ) {
            if p == ancestor {
                return true;
            }
            if !seen.contains(&p) {
                queue.push_back(p);
            }
        }
    }
    false
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
        // SlotTypeFold claims the dedicated `SlotType{..}` shape. Nothing
        // consumes it yet (typed `$obj->{k}` resolution is a later step),
        // so placement here is non-load-bearing — grouped with the other
        // arm-agreement folds for legibility.
        r.register(Box::new(SlotTypeFold));
        // BranchArmFold claims the dedicated `BranchArm(_)` shape — no
        // overlap with the Variable/Expr folds below, so order here is
        // not load-bearing.
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
        // Sole boundary where an owned `ReducedValue` is required; the
        // internal recursion threads `Arc` to avoid deep clones per hop.
        (*self.query_rec(bag, q, &mut state)).clone()
    }

    /// Returns an `Arc` so the memo, the cycle-guard early-outs, and the
    /// edge-chase recursion all share one heap allocation per resolved
    /// node instead of deep-cloning a (String-bearing) `ReducedValue` on
    /// every store, hit, and return.
    fn query_rec(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        state: &mut QueryState,
    ) -> std::sync::Arc<ReducedValue> {
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
            return std::sync::Arc::new(ReducedValue::None);
        }
        let key: VisitedKey = (
            bag as *const _ as usize,
            q.attachment.clone(),
            receiver_key(&q.receiver),
            q.arity_hint,
        );
        // Memo hit: this key was fully resolved earlier in THIS query and
        // isn't on the current path (cycle guard handles on-path keys).
        if let Some(cached) = state.memo.get(&key) {
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return std::sync::Arc::clone(cached);
        }
        // `key` has two owners (the visited set, transiently; the memo,
        // for the rest of the query). Clone once for visited, then move
        // the original into the memo store below.
        if !state.visited.insert(key.clone()) {
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return std::sync::Arc::new(ReducedValue::None);
        }
        let result = std::sync::Arc::new(self.query_rec_body(bag, q, state));
        state.visited.remove(&key);
        // Cache the off-path resolution. The query depends only on
        // `(bag, attachment, receiver-class, arity)` (all in `key`) plus
        // the static context, which is fixed for one top-level query.
        state.memo.insert(key, std::sync::Arc::clone(&result));
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
                            if *v != ReducedValue::None {
                                return (*v).clone();
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
                    if *v != ReducedValue::None {
                        return (*v).clone();
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
                    idx.for_each_entity_bridged_to(class, &mut |_mod, cached, sym| {
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

        // `SlotType{C, k}` the local bag couldn't answer: the typed
        // slot WRITE may live in C's own file (cross-file primary) or
        // anywhere up C's ancestry (a base class's BUILD populating
        // `$self->{conn}`). Hops (1) and (2) of the `MethodOnClass`
        // fallback above, same shared visited set; no bridge hop —
        // slot writes are real code, not plugin entities.
        if let WitnessAttachment::SlotType { class, key } = q.attachment {
            if let Some(ctx) = q.context {
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
                                arity_hint: None,
                                receiver: q.receiver.clone(),
                                context: Some(&cached_ctx),
                            };
                            let v = self.query_rec(&cached.analysis.witnesses, &sub_q, state);
                            if *v != ReducedValue::None {
                                return (*v).clone();
                            }
                        }
                    }
                }
                let parents = crate::file_analysis::parents_of(
                    class,
                    ctx.package_parents,
                    ctx.module_index,
                    ctx.app_surface_consumers,
                );
                for p in parents {
                    let parent_att = WitnessAttachment::SlotType {
                        class: p,
                        key: key.clone(),
                    };
                    let sub_q = ReducerQuery {
                        attachment: &parent_att,
                        point: q.point,
                        framework: q.framework,
                        arity_hint: None,
                        receiver: q.receiver.clone(),
                        context: q.context,
                    };
                    let v = self.query_rec(bag, &sub_q, state);
                    if *v != ReducedValue::None {
                        return (*v).clone();
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
                            // Narrowing point: an edge reached FROM a positioned
                            // expression (a variable read recorded at `Expr(span)`)
                            // resolves the slot at the read's own location, so a
                            // flow-sensitive guard refines it only inside the
                            // guard's region (docs/adr/flow-narrowing.md). Other
                            // edge sources have no read position; the scope end is
                            // the standing temporal approximation.
                            let point = match q.attachment {
                                WitnessAttachment::Expr(span) => span.start,
                                _ => scope_point(ctx.scopes, *scope),
                            };
                            self.query_variable_with_visited(
                                bag, ctx, name, *scope, point, state,
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
                                    fresh_dispatch_receiver(&q.receiver, class, q.context)
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
                            if let ReducedValue::Type(t) = &*self.query_rec(bag, &sub_q, state) {
                                Some(t.clone())
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
                            fresh_dispatch_receiver(&q.receiver, class, q.context)
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
                    if let ReducedValue::Type(t) = &*self.query_rec(bag, &sub_q, state) {
                        out.push(Witness {
                            attachment: w.attachment.clone(),
                            source: w.source.clone(),
                            payload: WitnessPayload::InferredType(t.clone()),
                            span: w.span,
                        });
                    }
                }
                WitnessPayload::Projected { base, step } => {
                    // Materialize the base, then narrow through the step —
                    // the value-side mirror of the build-time
                    // `invocant_type_at_node` drill, run where the index is
                    // in hand so imported structural types project too.
                    // A Variable base scope-walks like the Edge arm above
                    // (`$h{k}` projects off `%h`, whose witnesses live on
                    // the decl scope, not the access scope).
                    let base_t = match (base, q.context) {
                        (WitnessAttachment::Variable { name, scope }, Some(ctx)) => {
                            let point = scope_point(ctx.scopes, *scope);
                            self.query_variable_with_visited(
                                bag, ctx, name, *scope, point, state,
                            )
                        }
                        _ => {
                            let sub_q = ReducerQuery {
                                attachment: base,
                                point: q.point,
                                framework: q.framework,
                                arity_hint: None,
                                receiver: q.receiver.clone(),
                                context: q.context,
                            };
                            match &*self.query_rec(bag, &sub_q, state) {
                                ReducedValue::Type(t) => Some(t.clone()),
                                _ => None,
                            }
                        }
                    };
                    if let Some(t) = base_t {
                        let projected = match step {
                            ProjectionStep::HashKey(k) => {
                                t.key_value_type(k).flatten().cloned().or_else(|| {
                                    // Class-typed base: the structural
                                    // literal can't answer, but a typed
                                    // slot WRITE can — `SlotType{class,
                                    // key}`, local or (via the arm in
                                    // query_rec_body) cross-file and up
                                    // the ancestry. The read drills
                                    // through the registry, never a
                                    // baked value.
                                    let class = t.class_name()?.to_string();
                                    let att = WitnessAttachment::SlotType {
                                        class,
                                        key: k.clone(),
                                    };
                                    let sub_q = ReducerQuery {
                                        attachment: &att,
                                        point: q.point,
                                        framework: q.framework,
                                        arity_hint: None,
                                        receiver: q.receiver.clone(),
                                        context: q.context,
                                    };
                                    match &*self.query_rec(bag, &sub_q, state) {
                                        ReducedValue::Type(t) => Some(t.clone()),
                                        _ => None,
                                    }
                                })
                            }
                            ProjectionStep::ArrayIndex(i) => t.element_at(*i).cloned(),
                        };
                        if let Some(t) = projected {
                            out.push(Witness {
                                attachment: w.attachment.clone(),
                                source: w.source.clone(),
                                payload: WitnessPayload::InferredType(t),
                                span: w.span,
                            });
                        }
                    }
                }
                WitnessPayload::QualifiedCallReturn { method_lookup, receiver_class, arity } => {
                    // Look the method up on the named/parent class, but the
                    // receiver is the INVOCANT (enclosing) class — prefer a
                    // dynamic outer receiver only when it's a subclass of it
                    // (same rule as a fresh dispatch onto `receiver_class`).
                    let receiver =
                        fresh_dispatch_receiver(&q.receiver, receiver_class, q.context);
                    let sub_q = ReducerQuery {
                        attachment: method_lookup,
                        point: q.point,
                        framework: q.framework,
                        arity_hint: Some(*arity),
                        receiver,
                        context: q.context,
                    };
                    if let ReducedValue::Type(t) = &*self.query_rec(bag, &sub_q, state) {
                        out.push(Witness {
                            attachment: w.attachment.clone(),
                            source: w.source.clone(),
                            payload: WitnessPayload::InferredType(t.clone()),
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
        ctx: &BagContext,
        var: &str,
        scope: ScopeId,
        point: Point,
        state: &mut QueryState,
    ) -> Option<InferredType> {
        let chain = crate::file_analysis::scope_chain_of(ctx.scopes, scope);
        let framework = chain
            .iter()
            .find_map(|sid| ctx.scopes[sid.0 as usize].package.as_ref())
            .and_then(|pkg| ctx.package_framework.get(pkg).copied())
            .unwrap_or(FrameworkFact::Plain);
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
                context: Some(ctx),
            };
            if let ReducedValue::Type(t) = &*self.query_rec(bag, &q, state) {
                return Some(t.clone());
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
    ctx: &BagContext,
    var: &str,
    scope: ScopeId,
    point: Point,
) -> Option<InferredType> {
    let reg = ReducerRegistry::with_defaults();
    let mut state = QueryState::new();
    reg.query_variable_with_visited(bag, ctx, var, scope, point, &mut state)
}

/// Fold `KeyWrite`s into variable shape witnesses — the mutation-
/// extension pass. For each write on a variable whose shape at the
/// write point is `HashWithKeys`:
///
/// - unconditional static-key write → push the EXTENDED shape (key
///   joins the list, value typed from the RHS expression, `open`
///   preserved). A write to an already-known key retypes its value.
/// - dynamic key, syntactically conditional write, or a write whose
///   scope chain crosses a boundary before reaching the attachment
///   scope (nested block / closure — execution unknowable) → push the
///   same keys with `open: true`.
///
/// Witnesses attach to the scope the variable's existing witnesses
/// live on (so the read-side scope walk finds them) with a zero-width
/// span at the write position — the same temporal contract as TC
/// mirrors: invisible to reads before the write, latest-wins after
/// (`HashWithKeys` subsumption is equality-only, so a different shape
/// legitimately replaces the standing one).
///
/// Re-emittable: fold callers pass `clear = true` (clear-and-emit per
/// iteration, tag `mutation_extension`). Enrichment passes `false` —
/// post-finalize the bag is append-only (removal would shift the
/// sealed `base_witness_count`); duplicate pushes are idempotent under
/// latest-wins and truncated away by the next enrichment cycle.
pub(crate) fn emit_mutation_extension_witnesses(
    bag: &mut WitnessBag,
    ctx: &BagContext,
    key_writes: &[crate::file_analysis::KeyWrite],
    clear: bool,
) {
    if clear {
        bag.remove_by_source_tag("mutation_extension");
    }
    // Per-var doc order so later writes see earlier extensions.
    let mut writes: Vec<&crate::file_analysis::KeyWrite> = key_writes.iter().collect();
    writes.sort_by_key(|w| (&w.var_text, w.span.start));
    for w in writes {
        // Attach where the variable's existing witnesses live; note
        // whether getting there crosses a scope boundary (nested block
        // or closure — the write may not have executed by read time).
        // First scope up the shared chain whose Variable attachment has
        // witnesses; `crossed` = we climbed past the start scope (index
        // > 0) to find it.
        let mut attach: Option<(ScopeId, bool)> = None;
        for (i, &sid) in crate::file_analysis::scope_chain_of(ctx.scopes, w.scope)
            .iter()
            .enumerate()
        {
            let att = WitnessAttachment::Variable { name: w.var_text.clone(), scope: sid };
            if !bag.for_attachment(&att).is_empty() {
                attach = Some((sid, i > 0));
                break;
            }
        }
        let Some((attach_sid, scope_crossed)) = attach else { continue };
        let Some(base) = query_variable_type(bag, ctx, &w.var_text, w.scope, w.span.start)
        else {
            continue;
        };
        let rhs_type = |s: Span| {
            let reg = ReducerRegistry::with_defaults();
            let att = WitnessAttachment::Expr(s);
            let q = ReducerQuery {
                attachment: &att,
                point: None,
                framework: FrameworkFact::Plain,
                arity_hint: None,
                receiver: None,
                context: Some(ctx),
            };
            match reg.query(bag, &q) {
                ReducedValue::Type(t) => Some(t),
                _ => None,
            }
        };
        let shape = match base {
            InferredType::HashWithKeys { mut keys, open } => {
                // An Index write on a hash-shaped var is contradictory
                // evidence — widen like any unknowable write.
                let widen = !matches!(w.key, crate::file_analysis::WriteKey::Hash(_))
                    || w.conditional
                    || scope_crossed;
                if widen {
                    if open {
                        continue; // already open — nothing to add
                    }
                    InferredType::HashWithKeys { keys, open: true }
                } else {
                    let crate::file_analysis::WriteKey::Hash(ref k) = w.key else {
                        unreachable!()
                    };
                    let vtype = w.rhs_span.and_then(rhs_type);
                    match keys.iter_mut().find(|(name, _)| name == k) {
                        Some(entry) => {
                            if vtype.is_none() || entry.1.as_deref() == vtype.as_ref() {
                                continue; // no new information
                            }
                            entry.1 = vtype.map(Box::new);
                        }
                        None => keys.push((k.to_string(), vtype.map(Box::new))),
                    }
                    InferredType::HashWithKeys { keys, open }
                }
            }
            // Sequence slot write: only the sound moves — retype an
            // in-bounds slot, append at exactly len. Everything else
            // (out-of-bounds, conditional, crossed, Unknown) is
            // unmodeled: Sequence has no open flag to widen into, and
            // a bare-ArrayRef downgrade loses to structure-dominates-
            // rep subsumption. No array-index diagnostic exists, so
            // `element_at`'s honest None covers the residual.
            InferredType::Sequence(mut elems) => {
                let crate::file_analysis::WriteKey::Index(i) = w.key else { continue };
                if w.conditional || scope_crossed || i < 0 {
                    continue;
                }
                let Some(vt) = w.rhs_span.and_then(rhs_type) else { continue };
                let i = i as usize;
                if i < elems.len() {
                    if elems[i] == vt {
                        continue; // no new information
                    }
                    elems[i] = vt;
                } else if i == elems.len() {
                    elems.push(vt);
                } else {
                    continue;
                }
                InferredType::Sequence(elems)
            }
            _ => continue,
        };
        bag.push(Witness {
            attachment: WitnessAttachment::Variable {
                name: w.var_text.clone(),
                scope: attach_sid,
            },
            source: WitnessSource::Builder("mutation_extension".into()),
            payload: WitnessPayload::InferredType(shape),
            span: Span { start: w.span.start, end: w.span.start },
        });
    }
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------

#[cfg(test)]
#[path = "witnesses_tests.rs"]
mod tests;
