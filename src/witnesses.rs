//! Witness bag + reducers — Part 7 of the type-inference spec.
//!
//! A **witness** is typed evidence that some proposition holds about a
//! specific code location (variable, expression, symbol, hash key...),
//! tagged with its source (which builder pass / plugin emitted it). A
//! bag of witnesses is folded into concrete answers by **reducers** —
//! pure projections claiming the witnesses they care about.
//!
//! Spike goals (not the full Phase 0-7 plan):
//! - Data types (Witness, attachments, sources, payloads).
//! - Attachment-indexed bag on `FileAnalysis`.
//! - Reducer trait + a built-in framework-aware type-fold reducer
//!   (Part 6 from the spec).
//!
//! `#[allow(dead_code)]` on a few API surfaces (`WitnessBag::all`,
//! `filter`, `is_empty`; `ReducedValue::FactMap`; `WitnessReducer::name`) —
//! these are part of the bag's stable contract for plugins and future
//! reducers (payload-bearing reductions via `FactMap`). They're held
//! in the public surface deliberately rather than chased dead.

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
    /// Where the user can see why — rendered as "because: …" in hovers
    /// and diagnostics. Zero-extent span means core-synthesized.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WitnessAttachment {
    /// Traditional variable-in-scope facts — what `TypeConstraint` /
    /// `CallBinding` indexes by today.
    Variable { name: String, scope: ScopeId },
    /// An expression-result fact. Every method call is an index into
    /// `FileAnalysis::refs` via `RefIdx`; facts about the returned
    /// value attach here. Chain aggregation lives on this axis.
    Expression(RefIdx),
    /// A symbol property ("this sub is a dispatcher").
    Symbol(SymbolId),
    /// A specific call site — property of the call, not of its result
    /// or its receiver.
    CallSite(RefIdx),
    /// Hash key metadata (writes, mutations, derivations).
    HashKey { owner: HashKeyOwner, name: String },
    /// Package-level facts (isa edges, framework mode).
    Package(String),
    /// The value of an expression at this span. One attachment shape
    /// for every rvalue: literals, variable reads, function calls,
    /// method calls, ternaries, return-arm bodies, implicit-last
    /// statements. Witnesses on an `Expr` are either a direct
    /// `InferredType(t)` (literals, constructors, builtin returns) or
    /// an `Edge` to the resolution target (`Variable{name, scope}` for
    /// `$foo`, `Symbol(sym_id)` for a resolved local sub call,
    /// `Expression(refidx)` for a method call's
    /// receiver-and-method-resolved type, or another `Expr(inner_span)`
    /// for compound expressions). Replaces both the old
    /// `ReturnArm(Span)` and the per-arm dispatch in `arm_payload`:
    /// every node in rvalue position takes the same shape now, and the
    /// per-sub fold reads `Edge(Expr(span))` arms via Symbol(sub_id).
    Expr(Span),
    /// Class-keyed method dispatch: "what does method `name` return on
    /// class `class`?" — the cross-class disambiguation a name-keyed
    /// attachment can't carry. Inheritance composes through `Edge(MethodOnClass(parent, name))`
    /// witnesses emitted by the builder for each `package_parents[C] = [P, ...]`,
    /// so the registry's cycle-guarded edge chase walks the MRO without
    /// any procedural ancestor walker. Cross-file resolution: when the
    /// query carries a `BagContext.module_index`, the materialize step
    /// recurses into the cached module's bag for `class` — same shape,
    /// different bag.
    MethodOnClass { class: String, name: String },
    /// Per-arm return collector for a sub. Each `return EXPR` arm
    /// pushes one `Edge(Expr(body_span))` witness here; the parent
    /// `Symbol(sub_id)` carries a single `Edge(SymbolReturnArm(_))`
    /// chain so consumers querying the symbol still see arm-fold
    /// answers via standard edge materialization. Distinct from
    /// `Symbol(_)` so `SymbolReturnArmFold` claims by attachment
    /// shape, not by source-tag exclusion — `SubReturnReducer` and
    /// `SymbolReturnArmFold` no longer share an attachment family.
    SymbolReturnArm(SymbolId),
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
    /// This witness was derived from another ref — rename transport
    /// chases these as a DAG.
    DerivedFrom(RefIdx),
}

impl WitnessSource {
    /// Priority hint for "highest-priority source wins" tie-breaking
    /// in reducers that fold multiple witnesses into one answer.
    /// Plugin overrides dominate everything else — the whole point of
    /// an override is "inference reaches the wrong answer here". The
    /// exact weights only need to satisfy `Plugin > everything else`.
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
    /// An **observation** — raw evidence about a value's use, to be
    /// folded by the framework-aware resolver (Part 6).
    Observation(TypeObservation),
    /// Edge fact: "the value at my attachment is whatever resolves at
    /// `target`." The reducer registry materializes these at query
    /// time — chase the target via recursive query, replace the edge
    /// with a synthetic `InferredType` witness preserving source +
    /// span, then run reducers against the materialized list. Cycle
    /// guard breaks `A → B → A`.
    ///
    /// Edges replace the manual closure-driven chase that used
    /// `ReducerQuery::return_of` for `ReturnOfName` / `ReturnOf` —
    /// the bag now expresses "follow this reference" as a first-class
    /// payload kind, so reducers don't need to know about chasing.
    Edge(WitnessAttachment),
    /// **Symbol-declarative return type.** The sub's return type is
    /// a `ReturnExpr` — a receiver-relative / arity-relative
    /// expression that the bag's `ReturnExprReducer` substitutes at
    /// query time using `q.receiver` and `q.arity_hint`. Subsumes
    /// the call-site projection (DBIC `find` etc. emit
    /// `Operator(RowOf(Receiver))` once on the symbol instead of
    /// at every call site) and arity dispatch (Mojo `has`'s
    /// getter / writer collapse to a single `UnionOnArgs`).
    ///
    /// Attached to `Symbol(_)` (per-sub) and `MethodOnClass{...}`
    /// (class-keyed). The reducer claims both; latest wins on the
    /// declarative attachment so a plugin override re-publishes
    /// over a build-time inference.
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

/// Receiver-relative / arity-relative return-type expression. Lives
/// on `WitnessPayload::ReturnExpr` and is evaluated by
/// `ReturnExprReducer` against the query's `receiver` and
/// `arity_hint`. See `docs/adr/return-expr.md` for the load-bearing
/// decisions and `docs/adr/parametric-types.md` for the rationale
/// behind the sealed-enum shape (every consumer dispatches via
/// match, no `_ => …` fall-throughs).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ReturnExpr {
    /// Concrete type — equivalent to today's plain `InferredType`
    /// payload on a Symbol attachment. Used for non-parametric,
    /// non-arity-discriminated subs once they migrate to the
    /// declarative shape.
    Concrete(InferredType),
    /// Receiver placeholder. Evaluates to `q.receiver` at query
    /// time. `None` when the query doesn't carry a receiver
    /// (build-time look-up that wasn't initiated by a call site)
    /// — the reducer returns `None` rather than guessing.
    Receiver,
    /// Apply a parametric operator with `ReturnExpr`-valued
    /// sub-positions. Today only `RowOf` carries a sub-expression;
    /// `ResultSet { base, row }` is concrete data, no nesting.
    /// Substitution recurses into the sub-expression, evaluates,
    /// and re-wraps as `ParametricType`. The bag's existing
    /// `ParametricType` accessors (`class_name`, `hash_key_class`,
    /// `method_arg_owner`) handle the value-side projection
    /// downstream.
    Operator(ParametricOp),
    /// Union over arg-shape. Each branch is `(guard, expr)`; for a
    /// concrete `arity_hint` the first matching guard wins. For a
    /// hint-less introspection query (no specific call site) the
    /// `Any` branch is preferred, falling back to `Empty` so a Mojo
    /// `has`-style getter+writer pair surfaces its primary
    /// (`Empty`) arm. Branch order matters when the hint is
    /// concrete — narrow guards (`Empty`, `Exact`, `AtLeast`)
    /// before `Any`.
    UnionOnArgs { branches: Vec<(ArgGuard, ReturnExpr)> },
}

/// Operators with `ReturnExpr`-valued sub-positions. Mirrors
/// `ParametricType`'s operator-flavor structure; concrete flavors
/// (`ResultSet`) live in `ReturnExpr::Concrete(Parametric(...))`
/// because they carry data, not sub-expressions. Match invariant:
/// no `_ => …` fall-throughs.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParametricOp {
    /// `RowOf<T>` — projects a `ResultSet { base, row }` to
    /// `ClassName(row)`. After substitution into a concrete
    /// `InferredType`, evaluates by wrapping in
    /// `ParametricType::RowOf(Box::new(t))` so the existing
    /// value-side `class_name()` / `hash_key_class()` accessors
    /// pick up the right answer at consumption.
    RowOf(Box<ReturnExpr>),
}

/// Guard for `ReturnExpr::UnionOnArgs` branches. Matched against
/// `ReducerQuery.arity_hint`. `None` arity hint matches `Any` only
/// (no guess about which arm a hint-less call wants — the symbol's
/// declarative shape knew at synth time, the caller is opting out).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArgGuard {
    Empty,
    Exact(u32),
    AtLeast(u32),
    Any,
}

impl ArgGuard {
    /// Match against the call's arity hint. Strict semantics: a
    /// guard fires only when the hint *positively matches* it. The
    /// `Any` branch is the only catch-all, so a `None` hint never
    /// silently fires `Empty` / `Exact` / `AtLeast` arms.
    ///
    /// `None` callers are introspection queries that don't pin a
    /// call-site arity. Sym-introspection entry points
    /// (`symbol_return_type_via_bag`) compensate by defaulting the
    /// hint from the sym's own `params` count — a writer sym
    /// (params=1) gets `arity_hint = Some(1)` and matches its
    /// `AtLeast(1)` arm; a getter sym (params=0) gets
    /// `arity_hint = Some(0)` and matches `Empty`.
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

/// Part 6 — raw observations about a value's use, consumed by the
/// framework-aware resolver. These do NOT commit to a concrete type;
/// the resolver projects them to `InferredType` using framework
/// context.
///
/// Hash/Eq are intentionally NOT derived here — `InferredType` is
/// `PartialEq` but not `Hash`, and `TypeObservation::BranchArm`
/// needs to carry one. Attachment-keyed indexing uses
/// `WitnessAttachment`, not the payload, so Hash on the observation
/// itself isn't needed.
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

// ---- Framework-mode mirror (plain enum; builder's FrameworkMode is
// private to builder.rs, so we duplicate a small view for the resolver) ----

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

/// Lightweight attachment-indexed bag. Kept separate from the raw
/// witness vec so callers can iterate all witnesses for one attachment
/// without scanning. Indexes are rebuilt on demand.
///
/// `index` is `serde(skip)` (cheap to recompute, redundant on disk),
/// but every consumer that loads a `FileAnalysis` from bincode (the
/// SQLite cache, the dump-package debug tool, the cross-file enrichment
/// pass, …) routes through `Deserialize`. The custom `Deserialize` impl
/// rebuilds the index so `for_attachment` queries answer correctly on
/// the deserialized bag — without it, every reducer claims an empty
/// witness slice and the bag silently returns `None`. Symptom: a
/// freshly-built `FileAnalysis` answers correctly, but the same bytes
/// after a serialize/deserialize round-trip don't. `--dump-package`'s
/// own bincode copy hit exactly this.
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
        // Helper struct mirrors the on-disk shape (just the witness
        // vec — `index` is recomputed). Without an explicit helper the
        // derived impl would expect both fields and fail bincode loads.
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

    /// Iterate witnesses that match a predicate on attachment. O(n).
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

    /// Drop every witness past `baseline` and rebuild the index.
    /// Used by enrichment to revert post-build additions before
    /// re-deriving them — keeps the bag idempotent across repeat
    /// enrichment calls without accumulating duplicates.
    pub fn truncate(&mut self, baseline: usize) {
        if baseline >= self.witnesses.len() {
            return;
        }
        self.witnesses.truncate(baseline);
        self.rebuild_index();
    }

    /// Drop every witness whose source is `WitnessSource::Builder(tag)`
    /// and rebuild the index. Returns the number of witnesses removed.
    /// Used by re-emittable builder passes (arity-return emission,
    /// call-binding propagator) inside the worklist driver: each pass
    /// clears its prior outputs at the start of every fold iteration so
    /// the bag stays canonical (no duplicates) regardless of how many
    /// times the fold runs to reach fixed point.
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

/// Input to a reducer query: the attachment and an optional "point of
/// interest" span (so narrowing-scoped reducers can pick the closest
/// containing witness).
#[derive(Clone)]
pub struct ReducerQuery<'a> {
    pub attachment: &'a WitnessAttachment,
    pub point: Option<tree_sitter::Point>,
    pub framework: FrameworkFact,
    /// Arity hint for arity-dispatch reducers. `Some(N)` = caller
    /// passed exactly N additional arguments to the sub; `None` =
    /// unknown — the reducer should return the default branch's type.
    pub arity_hint: Option<u32>,
    /// Receiver type for `ReturnExpr::Receiver` substitution. Set by
    /// the chain typer's `coderef_call_expression` arm (the operand's
    /// CodeRef target may declare a `Receiver` placeholder), by the
    /// dynamic-method-call arm (`$obj->$cb()` — receiver is `$obj`),
    /// and by `MethodOnClass{...}` chases that originate from a
    /// `method_call_expression` with a known invocant type. `None`
    /// for build-time symbol probes that don't have a call site —
    /// `ReturnExpr::Receiver` evaluates to `ReducedValue::None` in
    /// that case rather than guessing.
    pub receiver: Option<InferredType>,
    /// Optional scope topology + per-package framework. Threaded into
    /// the registry's materialize step so `Edge(Variable{name, scope})`
    /// chases use `query_variable_type` semantics (scope-chain walk
    /// + framework-aware fold) instead of a flat single-attachment
    /// lookup. `None` for context-free queries (tests, lookups
    /// where the target is known to be self-contained).
    pub context: Option<&'a BagContext<'a>>,
}

/// File-scope context the registry needs to chase Variable edges
/// correctly. Carries the scope tree and per-package framework so
/// materialization can run `query_variable_type` for `Variable`
/// targets — which is the only edge target whose resolution
/// fundamentally requires more than the bag itself.
///
/// `module_index` lets the materialize step recurse into cached
/// modules' bags when a `MethodOnClass{class,...}` target names a
/// class defined in another file. `None` for in-file callers
/// (build-time, isolated tests where cross-file inheritance can't
/// be reached).
///
/// `package_parents` is the per-class inheritance graph (Perl's
/// default DFS-MRO). The registry walks it for `MethodOnClass{C, m}`
/// queries that the local bag can't answer — chasing
/// `MethodOnClass{P, m}` for each parent `P`. Cross-file parents are
/// resolved through `module_index`. Single recursive lookup on the
/// same attachment shape, not a procedural method-dispatch walker.
pub struct BagContext<'a> {
    pub scopes: &'a [Scope],
    pub package_framework: &'a HashMap<String, FrameworkFact>,
    pub module_index: Option<&'a crate::module_index::ModuleIndex>,
    pub package_parents: &'a HashMap<String, Vec<String>>,
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

// ---- Built-in: framework-aware type-fold reducer (Part 6) ----

/// Resolver that implements Part 6's rules:
///
/// 1. `ClassAssertion(Foo)` dominates.
/// 2. `FirstParamInMethod { package }` under a matching framework's
///    backing rep is NOT dethroned by rep observations that match the
///    backing rep (the Mojo `sub name` bug fix).
/// 3. `BlessTarget(Rep)` pins the rep axis.
/// 4. Rep observations with no class evidence project to the flat
///    `HashRef` / `ArrayRef` / `CodeRef`.
/// 5. `NumericUse` / `StringUse` / `RegexpUse` project to their types.
pub struct FrameworkAwareTypeFold;

impl WitnessReducer for FrameworkAwareTypeFold {
    fn name(&self) -> &str {
        "framework_aware_type_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        // Excludes `branch_arm`-source witnesses — those are claimed
        // by `BranchArmFold` (registered earlier) so the agreement /
        // disagreement rule applies. Without this filter the
        // multi-axis fold below would treat a disagreeing pair of
        // arms as "latest-wins," silently picking one instead of
        // surfacing ambiguity.
        matches!(
            w.attachment,
            WitnessAttachment::Variable { .. } | WitnessAttachment::Expression(_)
        ) && matches!(
            w.payload,
            WitnessPayload::InferredType(_) | WitnessPayload::Observation(_)
        ) && !matches!(&w.source, WitnessSource::Builder(s) if s == "branch_arm")
    }

    fn reduce(&self, ws: &[&Witness], q: &ReducerQuery) -> ReducedValue {
        // Part 5b — narrowing. If the query has a `point`, pick the
        // narrowest-span InferredType witness whose span *contains*
        // that point; if any exists, return it directly (it already
        // represents the post-narrowing type). Falls through to the
        // full fold otherwise.
        if let Some(point) = q.point {
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
        let mut rep_obs: Option<Rep> = None;
        let mut bless_rep: Option<Rep> = None;
        let mut num = false;
        let mut str_ = false;
        let mut re = false;
        let mut plain_type: Option<InferredType> = None;

        for w in ws {
            // Temporal ordering: a witness's span.start is the
            // location where the fact was emitted (for zero-span
            // seeds) or the narrowing-scope start (for scoped ones).
            // Only consider witnesses whose point-of-emission is at
            // or before the query point — a later reassignment
            // shouldn't influence a lookup at an earlier line.
            if let Some(point) = q.point {
                if w.span.start > point {
                    continue;
                }
            }
            // Skip scoped InferredType witnesses that don't contain
            // the query point — they're narrowing facts for a
            // different slice of the variable's lifetime.
            if let (Some(point), WitnessPayload::InferredType(_)) = (q.point, &w.payload) {
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

        // Class axis wins when it's consistent with the rep axis.
        if let Some(name) = class_assertion.clone().or(first_param_class.clone()) {
            let backing = bless_rep.or_else(|| q.framework.backing_rep());
            match (rep_obs, backing) {
                (None, _) => return ReducedValue::Type(InferredType::ClassName(name)),
                (Some(obs), Some(b)) if obs == b => {
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
                (Some(obs), None) => {
                    // Framework says "unknown rep" (CoreClass). Rep
                    // observation is probably wrong — warn later; keep
                    // class.
                    let _ = obs;
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
                (Some(obs), Some(b)) => {
                    // Contradiction: class says rep is `b`, but we saw
                    // an access as `obs`. For the spike: still return
                    // the class — the user's intent is object-typed
                    // use, and the rep contradiction would be a
                    // separate diagnostic.
                    let _ = (obs, b);
                    return ReducedValue::Type(InferredType::ClassName(name));
                }
            }
        }

        // Explicit `InferredType` assignments dominate rep
        // observations — `my $x = []` overrides earlier `$x->{k}`
        // access inferences because reassignment breaks the binding.
        // We picked the latest (by iteration order; seeds are in
        // source order) during the fold.
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
        // Conflict: prefer Hash (most common). The rule shouldn't
        // really fire; leave as an observation for later diagnostic.
        Some(_) => Some(new),
    }
}

// ---- Branch-arm fold reducer (ternary RHS + Expr-attached arms) ----

/// Branch-arm reduction for `Variable` and `Expr` attachments —
/// `my $x = $c ? A : B` and the per-arm Expr witnesses a ternary's
/// own `Expr(span)` carries. Agreement across ≥2 arms → that type;
/// single arm → None (ternaries always carry both arms by syntax,
/// so a single witness here means inference for one arm failed and
/// we shouldn't claim the other arm represents the whole
/// expression). Symbol-attached return arms go through
/// `SymbolReturnArmFold` instead — single-return subs DO carry their
/// answer via one witness, so the ≥2 rule is wrong there.
pub struct BranchArmFold;

impl WitnessReducer for BranchArmFold {
    fn name(&self) -> &str {
        "branch_arm_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        // Source-tag + attachment claim — the walker (and any
        // downstream pass that wants to contribute a branch-arm fact)
        // emits with source `Builder("branch_arm")`. After registry
        // materialization, every claimed witness has an
        // `InferredType` payload. We restrict to Variable/Expr here;
        // Symbol attachments are owned by `SymbolReturnArmFold`,
        // which uses `resolve_return_type` semantics (1+ arms).
        matches!(
            w.attachment,
            WitnessAttachment::Variable { .. } | WitnessAttachment::Expr(_)
        ) && matches!(&w.source, WitnessSource::Builder(s) if s == "branch_arm")
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
// payloads (Edges materialized into types). Each witness is one
// return arm — a `return EXPR` body, an explicit if/else arm,
// or an implicit-last-expression edge. `resolve_return_type` agrees
// them: 1 arm → that type, agreeing arms → that type, disagreeing →
// None (with HashRef subsumed by Object). Replaces the per-RI
// `returns_by_scope` Vec fold the builder used to do by hand.
//
// Symbol(sub_id) carries one `Edge(SymbolReturnArm(sub_id))` chain
// witness pushed per arm, so consumers querying the symbol's return
// still see arm-fold answers through standard edge materialization.
// Claim by attachment shape — no source-tag filter needed.

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

// Sub-return delegation chains (`return other()`,
// `shift->method(...)`) are represented as `Edge(Symbol(...))` /
// `Edge(MethodOnClass{...})` payloads. Registry materialization
// handles the chase transparently. The procedural fixed-point loops
// in `Builder::propagate_via_delegation` /
// `Builder::propagate_via_self_method_tails` continue to own
// build-time delegation propagation (they read the
// `sub_return_delegations` / `self_method_tails` maps directly,
// not the bag).

// ---- Expression reducer ----
//
// Claims `InferredType` payloads on `Expr(_)` attachments — the unified
// expression-result attachment that every rvalue node publishes
// through. The walker pushes either `Type(t)` directly (literals,
// constructors, builtin-determined returns) or `Edge(target)` for
// resolution (Variable lookups for `$x`, Symbol(sym_id) for resolved
// local sub calls, Expression for method-call receivers, or recursive
// Expr edges for compound expressions). Edge witnesses get materialized by the
// registry into synthetic `Type` witnesses before any reducer claims,
// so this reducer always sees plain types. Latest-wins:
// `emit_expr_witness` is called from multiple walk sites (return
// body, top-level expression statement, ternary parent that recurses
// into its arms), so the same node may receive several witnesses;
// reading from the back picks the most recently published.

pub struct ExprReturn;

impl WitnessReducer for ExprReturn {
    fn name(&self) -> &str {
        "expr_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Expr(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
            // BranchArmFold (registered earlier) handles `branch_arm`-source
            // witnesses on Expr — ternary arms agree under its rule. This
            // reducer covers the remaining "this expr just has a type"
            // case (a literal, a variable read, a chased call result).
            && !matches!(&w.source, WitnessSource::Builder(s) if s == "branch_arm")
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
// Claims plain `InferredType` payloads on `Symbol(_)` attachments —
// the id-keyed answer for "what does THIS specific sym return?".
// Pushed by writeback (local subs/methods) and tests that hand-craft
// a FileAnalysis without going through the builder.
// Class-scoped multi-overload dispatch goes through
// `MethodOnClass{class, name}` instead — see `MethodOnClassReducer`
// and the `arity_hint` short-circuit in `reduce` below.
//
// Latest wins so a later writeback re-publish (the worklist clears
// `local_return` on every iteration and re-pushes from
// `resolved_returns`) dominates an older value. Registered AFTER
// every more-precise reducer (Plugin override, ReturnExpr arity
// dispatch) so those still get to claim first.
//
// Per-arm return witnesses live on `SymbolReturnArm(sub_id)` and are
// claimed by `SymbolReturnArmFold`; `Symbol(sub_id)` carries an
// `Edge(SymbolReturnArm(_))` chain that materializes the arm-fold's
// agreed answer (or None on disagreement) as a synthetic
// `InferredType` witness here. Latest-wins resolves the writeback-
// vs-arm-chain race: writeback runs in step 6 of the build pipeline,
// after the walk-time arm pushes, so it dominates when present.

pub struct SubReturnReducer;

impl WitnessReducer for SubReturnReducer {
    fn name(&self) -> &str {
        "sub_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        // `Symbol(_) + InferredType`, latest-wins. No source-tag
        // filter — every claim discriminator lives on the attachment
        // shape now. Per-arm answers route through
        // `SymbolReturnArm(_)` (claimed by `SymbolReturnArmFold`);
        // arity-dispatched answers route through
        // `WitnessPayload::ReturnExpr` (claimed by `ReturnExprReducer`,
        // registered earlier).
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
// Claims `MethodOnClass{class, name}` attachments carrying a plain
// `InferredType` payload — the class-scoped, name-keyed answer.
// Pushed by `write_back_sub_return_types` for the primary symbol of
// each `(class, method)` pair: that's the "default" return type when
// the caller doesn't constrain by arity.
//
// `ReturnExprReducer` runs first and handles arity-aware /
// receiver-relative dispatch when a `WitnessPayload::ReturnExpr`
// witness exists on the same attachment. This reducer only fires
// when no symbol-declarative ReturnExpr answers — and produces the
// primary's stored return.
//
// Latest-wins: the worklist clears `method_on_class` source witnesses
// each iteration and re-pushes from current state. The last witness
// represents the converged answer.

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
// Claims `Symbol(_)` attachments carrying an `InferredType` payload
// from a high-priority source (Plugin). When such a witness exists
// in the bag, it short-circuits the symbol-return fold — overrides
// dominate inference. Registered first in `with_defaults()` so its
// short-circuit runs before any inferred fold (BranchArmFold,
// ReturnExprReducer, SubReturnReducer) gets a chance to claim
// Symbol attachments.
//
// Why a separate reducer rather than a branch in
// FrameworkAwareTypeFold: keeping the priority short-circuit a
// distinct, named reducer means
// (a) the `claims` predicate is precise — only fires when a
//     priority>10 witness is present, not on every Symbol+InferredType
//     fact (which become more common as Phase 4–6 land);
// (b) the dump-package debug output can attribute the answer to
//     `plugin_override` specifically, not a generic `framework_aware`.

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
        // Highest priority wins. Multiple plugins overriding the
        // same symbol is a registration-order question — last one
        // pushed wins on tie, matching the registry's iteration
        // order and the existing "last seed wins" rule for
        // InferredType witnesses elsewhere.
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
// `WitnessPayload::ReturnExpr(_)` payload — the symbol-declarative
// return-type machinery. Substitutes `q.receiver` for `Receiver`
// placeholders, dispatches `UnionOnArgs` branches against
// `q.arity_hint`, and evaluates `Operator(RowOf(_))` by recursing
// into the sub-expression and re-wrapping into `ParametricType`.
// Registered before `MethodOnClassReducer` and `SubReturnReducer`
// so symbol-declarative answers dominate primary-sym writeback.
//
// **No side-effects, no special-cases.** Per CLAUDE.md #10, the
// reducer doesn't peek at method names, classes, or attachment
// payloads beyond `q.receiver` / `q.arity_hint`. The sub's policy
// lives entirely in the `ReturnExpr` value the synth/plugin pushed.
//
// Latest-wins among multiple `ReturnExpr` witnesses on the same
// attachment (consistent with `MethodOnClassReducer`'s rule) — a
// re-publish during the worklist fold replaces the earlier value.
// Plugin-priority sources still dominate via the source-priority
// short-circuit (highest `WitnessSource::priority()` wins on tie).

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
        // Highest-priority source wins; ties resolve to latest-pushed
        // (rev-iteration). Mirrors `PluginOverrideReducer`'s rule so
        // a plugin-priority `ReturnExpr` re-publish dominates a
        // build-time inference.
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

/// Evaluate a `ReturnExpr` against a query. Pure substitution — the
/// only "context" the evaluator reads is `q.receiver` / `q.arity_hint`.
/// Returns `None` when:
///   - `Receiver` is encountered but `q.receiver` is `None`.
///   - No `UnionOnArgs` branch matches `q.arity_hint`.
///   - An operator's sub-expression evaluates to `None`.
///   - An operator's sub-expression evaluates to a type the operator
///     can't project (`RowOf(NotAResultSet)` → `None`, same as the
///     value-side `ParametricType::class_name()` answer).
fn eval_return_expr(re: &ReturnExpr, q: &ReducerQuery) -> Option<InferredType> {
    match re {
        ReturnExpr::Concrete(t) => Some(t.clone()),
        ReturnExpr::Receiver => q.receiver.clone(),
        ReturnExpr::Operator(op) => match op {
            ParametricOp::RowOf(inner) => {
                let inner_t = eval_return_expr(inner, q)?;
                // Wrap into the value-side `RowOf` operator so the
                // existing `ParametricType` accessors (class_name,
                // hash_key_class) handle consumption uniformly. The
                // accessors evaluate `RowOf(ResultSet { row, .. })` to
                // `Some(row)` and other operands to `None` — same
                // policy here, no second projection layer.
                Some(InferredType::Parametric(ParametricType::RowOf(Box::new(
                    inner_t,
                ))))
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
            // Hint-less query (sym introspection / class-keyed
            // lookup with no specific call site): prefer the Any
            // arm — it's the union's catch-all, semantically
            // closest to "default branch." If no Any arm exists,
            // fall back to the Empty arm (the typical "primary"
            // for Mojo `has` getter+writer pairs and DBIC
            // accessors). This keeps the per-call-site dispatch
            // strict (no Empty matching None directly) while still
            // answering legitimate introspection queries.
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

/// Cycle guard for recursive bag queries. Keyed by
/// `(bag_ptr, attachment, receiver_disc, arity_hint)` so re-entries
/// into the same bag for the same attachment close cross-file
/// mutual-inheritance loops; per-bag entries stay separate so a
/// legitimate cross-bag query for the same attachment shape (the
/// common case for `MethodOnClass{C, m}` jumping into C's own bag) is
/// not misidentified as a cycle. The receiver discriminant + arity
/// hint widen the key so two queries on the same attachment with
/// different `q.receiver` / `q.arity_hint` aren't false-positive
/// duplicates — `ReturnExpr::UnionOnArgs` and `Receiver` substitution
/// can produce different answers for the same attachment, and the
/// monotone witness bag means re-asking with a different receiver is
/// always safe.
///
/// `receiver_disc` is just the `InferredType`'s variant discriminant
/// (a `u8`), not the full payload — finer than the full type
/// (catches the common case of "same shape, different class") would
/// blow the key arithmetic without buying anything: even
/// `Some(ClassName(_))` vs `Some(Parametric(_))` yields different
/// substitutions that legitimately deserve a fresh recursion.
type VisitedKey = (usize, WitnessAttachment, u8, Option<u32>);
type VisitedSet = std::collections::HashSet<VisitedKey>;

/// Cycle-guard discriminant for `q.receiver`. `0` for `None`, then
/// each `InferredType` variant maps to a stable `u8` (1..=12). The
/// numeric values aren't load-bearing — only the property that
/// "different variants compare unequal" matters. Stays in sync with
/// `InferredType` by exhaustive match (no `_ =>`); a new variant
/// trips the compiler instead of silently colliding.
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
    }
}

/// Depth backstop for `query_rec`. The `(bag, attachment)` visited set
/// is the primary cycle guard; this cap is a belt-and-braces safeguard
/// against any *new* recursion shape we haven't accounted for blowing
/// the stack on real-world `@INC`. When the cap is hit we log the
/// offending attachment once per process and return `None` — the query
/// gives up cleanly instead of aborting. Set high enough that legit
/// inheritance + edge chases never trip it; tune down only with
/// evidence that a deeper walk is needed.
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
        // Plugin overrides short-circuit before any inferred fold —
        // registered first so it sees Symbol+InferredType witnesses
        // before FrameworkAwareTypeFold or BranchArmFold get a chance.
        // BranchArmFold runs before FrameworkAwareTypeFold so the
        // agreement / disagreement rule for branch_arm-source
        // witnesses applies before the multi-axis fold's latest-wins
        // accumulator could pick one of two disagreeing arms. Plugin
        // overrides still dominate (priority short-circuit on
        // `Symbol(_)` attachments — different attachment shape than
        // BranchArmFold's typical Variable / Symbol-via-edge claims).
        r.register(Box::new(PluginOverrideReducer));
        // ReturnExpr is symbol-declarative — registered before every
        // value-side reducer so a sub's declared shape (Mojo `has`'s
        // UnionOnArgs, DBIC `find`'s Operator(RowOf, Receiver), etc.)
        // wins over per-arity observations or primary-sym writeback.
        // Plugin-priority is preserved via the in-reducer source
        // priority compare; PluginOverrideReducer above still
        // short-circuits Symbol+InferredType plugin payloads.
        r.register(Box::new(ReturnExprReducer));
        // SymbolReturnArmFold claims `SymbolReturnArm(_)` — a
        // dedicated attachment shape distinct from BranchArmFold's
        // `Variable{..}` / `Expr(_)` ternary scope. Single-arm
        // answers (a sub with one `return X`) need to surface here,
        // where BranchArmFold's ≥2-arm rule would reject them.
        r.register(Box::new(SymbolReturnArmFold));
        r.register(Box::new(BranchArmFold));
        r.register(Box::new(FrameworkAwareTypeFold));
        r.register(Box::new(ExprReturn));
        // MethodOnClass primary-fallback runs after the
        // symbol-declarative `ReturnExprReducer` so per-arity
        // declarations win when one matches; only when no
        // ReturnExpr answers does the primary's plain
        // `InferredType` surface.
        r.register(Box::new(MethodOnClassReducer));
        // Last — fallback for "this Symbol's stored return type"
        // queries that none of the more-precise reducers claimed.
        r.register(Box::new(SubReturnReducer));
        r
    }

    pub fn register(&mut self, r: Box<dyn WitnessReducer>) {
        self.reducers.push(r);
    }

    /// Query the registry for the first reducer that returns a
    /// non-`None` value. For the type-fold there's only one claimant;
    /// for fact families you'd scan all.
    ///
    /// Edge materialization runs first: any `WitnessPayload::Edge(target)`
    /// witnesses on the queried attachment are chased via recursive
    /// query and replaced by synthetic `InferredType` witnesses
    /// (preserving the original source + span) before reducers see the
    /// witness list. This lets edges compose with existing reducers
    /// without reducer-side awareness — `FrameworkAwareTypeFold` reads
    /// a chased class as a plain `InferredType::ClassName`, same as if
    /// the walker had baked it directly.
    ///
    /// The cycle guard keys on `(bag_ptr, attachment)` and is threaded
    /// across both edge chases (within one bag) and the inheritance
    /// fallback (which crosses bags). Re-entering the same bag for the
    /// same attachment closes mutual-inheritance loops that span files
    /// (`package A; use parent 'B'; package B; use parent 'A';`) — a
    /// per-bag-only set would let the walk reset visited every time it
    /// crossed `module_index.get_cached(...)`, recursing until the
    /// stack overflows.
    pub fn query(&self, bag: &WitnessBag, q: &ReducerQuery) -> ReducedValue {
        let mut visited: VisitedSet = std::collections::HashSet::new();
        self.query_rec(bag, q, &mut visited)
    }

    fn query_rec(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        visited: &mut VisitedSet,
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
        if !visited.insert(key.clone()) {
            QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
            return ReducedValue::None;
        }
        let result = self.query_rec_body(bag, q, visited);
        visited.remove(&key);
        QUERY_REC_DEPTH.with(|c| c.set(c.get() - 1));
        result
    }

    fn query_rec_body(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        visited: &mut VisitedSet,
    ) -> ReducedValue {
        let materialized = self.materialize(bag, q, visited);

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

        // Inheritance + bridge fallback for `MethodOnClass{C, m}`
        // queries that the local bag couldn't answer.
        //
        // D3 added build-time and enrichment-time edge emission for
        // the common cases: local writeback emits
        // `MethodOnClass(child, m) → Edge(MethodOnClass(parent, m))`
        // for every method `m` on a *local* parent;
        // `enrich_imported_types_with_keys` projects the same shape
        // for *cross-file* parents. When edges are present, the
        // generic edge-chase machinery resolves inheritance without
        // touching this fallback.
        //
        // The fallback covers the residual: callers that build a
        // FileAnalysis without going through `build()` +
        // enrichment (hand-crafted FAs, isolated tests), and
        // cross-file plugin-namespace bridges declared in *other*
        // files whose entities would otherwise need an N×N
        // pre-emission to reach. Three structural facts compose:
        //
        //   1. `module_index.get_cached(C)` — when `C` lives in
        //      another file, recurse into its cached bag for the
        //      direct facts on C.
        //   2. `package_parents[C]` (local) ∪
        //      `module_index.parents_cached(C)` (cross-file) — the
        //      Perl DFS-MRO chain. Recurse on `MethodOnClass{P, m}`
        //      for each parent. Cross-file P resolves through (1)
        //      on the recursive call.
        //   3. `module_index.for_each_entity_bridged_to(class, ...)`
        //      — entities in *other* files' plugin namespaces
        //      bridged to `class`. Each match feeds the cached bag
        //      a `Symbol(sym.id)` query, since per-FA SymbolIds
        //      can't be portably edge-encoded.
        //
        // The shared `(bag, attachment)`-keyed visited set breaks
        // local cycles (`class :isa Self`) and cross-file loops
        // (`A use parent B; B use parent A`).
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
                                visited,
                            );
                            if v != ReducedValue::None {
                                return v;
                            }
                        }
                    }
                }
                // (2) Inheritance walk via package_parents.
                let mut parents: Vec<String> =
                    ctx.package_parents.get(class).cloned().unwrap_or_default();
                if let Some(idx) = ctx.module_index {
                    for p in idx.parents_cached(class) {
                        if !parents.contains(&p) {
                            parents.push(p);
                        }
                    }
                }
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
                    let v = self.query_rec(bag, &sub_q, visited);
                    if v != ReducedValue::None {
                        return v;
                    }
                }
                // (3) Cross-file plugin-namespace bridges. Plugin
                // entities declared in OTHER files (mojo-helpers'
                // helpers, etc.) bridged to `class` aren't
                // reachable via the local bag's edges nor the
                // cross-file primary (`get_cached(class)` returns
                // the canonical class file, not the file with the
                // bridging plugin). Walk the index for matching
                // entities and ask each cached module for
                // `Symbol(sym.id)` at arity=None — bridged Methods
                // aren't arity-discriminated, so the writeback's
                // plain `InferredType` answer is correct.
                if let Some(idx) = ctx.module_index {
                    let mut found: Option<InferredType> = None;
                    idx.for_each_entity_bridged_to(class, |cached, sym| {
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

    /// Resolve every Edge witness on `q.attachment` to an
    /// `InferredType` witness via recursive query; non-edge witnesses
    /// pass through unchanged. The returned list is fresh-owned so
    /// reducers can take `&Witness` references into it.
    ///
    /// `Edge(Variable{...})` targets are special-cased: variable
    /// resolution requires walking the scope chain and using the
    /// scope's package framework. When the query carries a
    /// `BagContext`, materialize delegates to
    /// `query_variable_with_visited` so the recursion shares the
    /// caller's cycle guard — calling the public `query_variable_type`
    /// from here would reset visited and leave us open to mutual
    /// `Edge(Variable{a}) ↔ Edge(Variable{b})` loops.
    fn materialize(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        visited: &mut VisitedSet,
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
                                name,
                                *scope,
                                point,
                                visited,
                            )
                        }
                        _ => {
                            let sub_q = ReducerQuery {
                                attachment: target,
                                point: q.point,
                                framework: q.framework,
                                arity_hint: q.arity_hint,
                                receiver: q.receiver.clone(),
                                context: q.context,
                            };
                            if let ReducedValue::Type(t) = self.query_rec(bag, &sub_q, visited) {
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
                    // Edge that didn't resolve drops out — same as a
                    // witness no reducer claims.
                }
                _ => out.push(w.clone()),
            }
        }
        out
    }

    /// Scope-chain variable lookup with an explicit visited set.
    /// `query_variable_type` is the public entry; this is the inner
    /// loop, factored out so callers already inside a `query_rec`
    /// recursion (currently only `materialize` for `Edge(Variable)`)
    /// can thread their cycle guard through. Each scope on the chain
    /// goes through `query_rec` with the *same* visited set, so a
    /// mutual `$a → $b → $a` edge cycle closes.
    fn query_variable_with_visited(
        &self,
        bag: &WitnessBag,
        scopes: &[Scope],
        package_framework: &HashMap<String, FrameworkFact>,
        var: &str,
        scope: ScopeId,
        point: Point,
        visited: &mut VisitedSet,
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
        let empty_parents: HashMap<String, Vec<String>> = HashMap::new();
        let ctx = BagContext {
            scopes,
            package_framework,
            module_index: None,
            package_parents: &empty_parents,
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
            if let ReducedValue::Type(t) = self.query_rec(bag, &q, visited) {
                return Some(t);
            }
        }
        None
    }
}

/// Pick a `Point` to use as the "where am I asking from?" for a
/// scope-chained Variable query. The scope's start position works —
/// `query_variable_type` uses it for temporal narrowing
/// (witnesses past this point are excluded). For Edge chases the
/// emitting witness's span is the right answer, but materialize
/// doesn't have access to the chasing witness's span here; the
/// scope start is a safe approximation.
fn scope_point(scopes: &[Scope], scope: ScopeId) -> tree_sitter::Point {
    scopes
        .get(scope.0 as usize)
        .map(|s| s.span.end)
        .unwrap_or(tree_sitter::Point { row: 0, column: 0 })
}

// ---- Single shared query entrypoint ----
//
// Both the in-builder return-arm fold and `FileAnalysis`'s public
// `inferred_type` go through this helper. Callers that already have a
// `FileAnalysis` use the convenience method on it; the builder calls
// this directly because its bag is in-progress and not yet wrapped in
// a `FileAnalysis`. There is exactly ONE place where the scope-chain
// walk + framework-fact lookup + reducer dispatch lives.

/// Single canonical query for "what does this sub return?". Handles
/// both local subs (resolved via the `symbols` table to a `Symbol`
/// attachment) and imported / cross-file subs (resolved through
/// `BagContext.module_index`'s exporter index → recurse into the
/// cached module's bag with `Symbol(cached_sid)`). Callers don't
/// need to branch on "is this local"; one path covers both.
pub fn query_sub_return_type(
    bag: &WitnessBag,
    symbols: &[crate::file_analysis::Symbol],
    sub_name: &str,
    arity_hint: Option<u32>,
    receiver: Option<InferredType>,
    context: Option<&BagContext>,
) -> Option<InferredType> {
    let reg = ReducerRegistry::with_defaults();
    // Local-symbol bag query first — `ReturnExprReducer` picks up
    // any arity-discriminated `UnionOnArgs` declaration on the
    // matching sym.
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
        // getter+writer share a name; `find()` returns the getter
        // first but at `arity=1` the writer's answer is required).
        // `MethodOnClass{class, name}` carries every per-arity arm
        // that synthesis published.
        if let Some(class) = sym.package.as_ref() {
            // Default receiver for class-keyed lookup: when the
            // caller didn't pass one, fall back to `ClassName(class)`
            // — the natural answer for a name-keyed query at the
            // class boundary. Mojo writer's `ReturnExpr::Receiver`
            // evaluates to `ClassName(class)` (fluent return),
            // matching what `$obj->writer()`'s call site would
            // produce. Passes through when a specific receiver IS
            // supplied (chain typer's method-call arm threads
            // `$obj`'s type explicitly).
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
    // `sub_name` and recurse into each cached module's bag for the
    // matching `Symbol(cached_sid)`. The recursion shares the
    // registry — same arity dispatch, same plugin overrides, same
    // fold rules — only the bag and symbols change.
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

/// Walk the scope chain from `scope` upward, asking the reducer
/// registry to fold every Variable witness for `var`. Returns the
/// first scope that produces a typed answer; `None` if no scope on
/// the chain has any matching witness or the reducer rejects them all.
///
/// Public entry: starts a fresh cycle-guard set. Recursive callers
/// already inside `query_rec` must use
/// `ReducerRegistry::query_variable_with_visited` instead so the
/// shared visited set catches mutual `Edge(Variable{a})` ↔
/// `Edge(Variable{b})` loops.
pub fn query_variable_type(
    bag: &WitnessBag,
    scopes: &[Scope],
    package_framework: &HashMap<String, FrameworkFact>,
    var: &str,
    scope: ScopeId,
    point: Point,
) -> Option<InferredType> {
    let reg = ReducerRegistry::with_defaults();
    let mut visited: VisitedSet = std::collections::HashSet::new();
    reg.query_variable_with_visited(
        bag,
        scopes,
        package_framework,
        var,
        scope,
        point,
        &mut visited,
    )
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------

#[cfg(test)]
#[path = "witnesses_tests.rs"]
mod tests;
