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
//! `filter`, `is_empty`; `ReturnOfKey::{Symbol, Name, MethodOnClass}`
//! payloads; `ReducedValue::FactMap`; `WitnessReducer::name`) — these
//! are part of the bag's stable contract for plugins and future
//! reducers (e.g. dispatch chain folding via `MethodOnClass`,
//! payload-bearing reductions via `FactMap`). They're held in the
//! public surface deliberately rather than chased dead.

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

use crate::file_analysis::{
    HashKeyOwner, InferredType, Scope, ScopeId, Span, SymbolId,
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
    /// A symbol referenced by name without a local `SymbolId` — i.e.
    /// imported from another module. The bag carries the imported
    /// return type here so the bag-query path is uniform across
    /// local and cross-file callees. Without this attachment kind
    /// imported subs would need a separate lookup (the old
    /// `imported_return_types` map), reintroducing parallel paths.
    NamedSub(String),
    /// A specific call site — property of the call, not of its result
    /// or its receiver.
    CallSite(RefIdx),
    /// Hash key metadata (writes, mutations, derivations).
    HashKey { owner: HashKeyOwner, name: String },
    /// Package-level facts (isa edges, framework mode).
    Package(String),
    /// One return-arm of a sub, identified by the return expression's
    /// span. Carries the arm's resolved type as either a direct
    /// `InferredType(t)` (literals, constructors) or an `Edge` to the
    /// arm-source attachment (a variable's `Variable{name, scope}` for
    /// `return $foo`). The per-sub fold queries `Symbol(sub_id)` whose
    /// `branch_arm` witnesses are themselves edges to per-arm
    /// `ReturnArm` attachments — registry materialization chases the
    /// edges, so `BranchArmFold` sees each arm's resolved type
    /// uniformly. Arity dispatch (`emit_arity_return_witnesses`)
    /// queries each `ReturnArm` directly to read per-arm types.
    ReturnArm(Span),
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
    /// Keyed fact. Family + key + value schema is the reducer's
    /// responsibility.
    Fact { family: String, key: String, value: FactValue },
    /// "This witness's subject derives from another ref." Rename
    /// transport walks these.
    Derivation,
    /// Escape hatch for plugin-defined payloads that don't fit above.
    Custom { family: String, json: String },
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
    /// A single arity-dispatch fact: for arity `arg_count`, the sub
    /// returns `return_type`. `None` for `arg_count` means the
    /// fall-through / default branch. Attached to a `Symbol(sub_id)`.
    ArityReturn { arg_count: Option<u32>, return_type: InferredType },
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
pub struct BagContext<'a> {
    pub scopes: &'a [Scope],
    pub package_framework: &'a HashMap<String, FrameworkFact>,
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
                    TypeObservation::ArityReturn { .. } => {
                        // Arity dispatch is a separate reducer.
                    }
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
                Rep::Code => InferredType::CodeRef,
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

// ---- Branch-arm fold reducer (ternary + explicit if/else arms) ----

/// Generic branch-arm reduction: collects `BranchArm` observations
/// across any attachment kind (Variable, Symbol, Expression). Same
/// semantics as the ternary fold inside FrameworkAwareTypeFold —
/// agreement → that type; disagreement → None.
///
/// Registered alongside FrameworkAwareTypeFold so *any* attachment
/// can carry branch-arm witnesses — ternary into a variable, if/else
/// returns on a Symbol (sub), chain-collapsing expressions on an
/// Expression, etc.
pub struct BranchArmFold;

impl WitnessReducer for BranchArmFold {
    fn name(&self) -> &str {
        "branch_arm_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        // Source-tag claim — the walker (and any downstream pass that
        // wants to contribute a branch-arm fact) emits with source
        // `Builder("branch_arm")`. After registry materialization,
        // every claimed witness has an `InferredType` payload (the
        // walker pushed `Type(t)` directly for bakeable arms; the
        // registry chased `Edge(target)` arms through to a resolved
        // type and replaced them with synthetic `InferredType`
        // witnesses preserving source + span). Reducer needs only
        // the values.
        matches!(&w.source, WitnessSource::Builder(s) if s == "branch_arm")
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
        // A single arm is not "agreement" — `if (cond) { return X }`
        // alone leaves the alternative undetermined. The fold only
        // produces a type when at least two arms exist AND they all
        // agree. Otherwise yield to the next reducer / fallback (e.g.
        // `Symbol.return_type` carries the agreement across every
        // return arm via `resolve_return_type`).
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

// ---- Fluent arity dispatch reducer ----

/// Picks an `InferredType` from a bag of `ArityReturn` witnesses
/// attached to a `Symbol(sub_id)`, using the query's `arity_hint`.
/// Shapes it knows:
///
/// - `arg_count: Some(N)` + query arity N → that return type.
/// - `arg_count: None` (default branch) → fallback when no N matches.
///
/// "Fluent arity dispatch" — the `sub name { return $self->{name} unless @_; … return $self }`
/// pattern — emits `ArityReturn { arg_count: Some(0), return_type: String }`
/// and `ArityReturn { arg_count: None, return_type: ClassName(Self) }`.
pub struct FluentArityDispatch;

impl WitnessReducer for FluentArityDispatch {
    fn name(&self) -> &str {
        "fluent_arity_dispatch"
    }

    fn claims(&self, w: &Witness) -> bool {
        // Two attachment shapes carry `ArityReturn` observations:
        //   - `Symbol(_)` — single sub with internal arity branches
        //     (`return X unless @_; return Y;`), one Symbol id, one
        //     witness per arm.
        //   - `NamedSub(_)` — multiple Symbol ids share a logical name
        //     (Mojo::Base / Moo `has` synthesizes a getter and writer
        //     under the same method name). Per-Symbol witnesses can't
        //     dispatch across distinct symbols, so the framework
        //     synthesis publishes name-keyed observations and this
        //     reducer folds them at query time.
        matches!(
            w.attachment,
            WitnessAttachment::Symbol(_) | WitnessAttachment::NamedSub(_)
        ) && matches!(
            w.payload,
            WitnessPayload::Observation(TypeObservation::ArityReturn { .. })
        )
    }

    fn reduce(&self, ws: &[&Witness], q: &ReducerQuery) -> ReducedValue {
        let mut exact: Option<InferredType> = None;
        // Default is the agreement across every non-arity-gated arm.
        // We collect each Default's type and call resolve_return_type
        // (the existing combinator that returns None on disagreement)
        // to decide. Without this, two conflicting bare returns —
        // e.g. `if (cond) { return {} } return [];` — would silently
        // surface the last-emitted arm and lose the disagreement.
        let mut default_arms: Vec<InferredType> = Vec::new();
        for w in ws {
            if let WitnessPayload::Observation(TypeObservation::ArityReturn {
                arg_count,
                return_type,
            }) = &w.payload
            {
                match (arg_count, q.arity_hint) {
                    (Some(a), Some(h)) if *a == h => exact = Some(return_type.clone()),
                    (None, _) => default_arms.push(return_type.clone()),
                    _ => {}
                }
            }
        }
        let default = crate::file_analysis::resolve_return_type(&default_arms);
        if let Some(t) = exact.or(default) {
            return ReducedValue::Type(t);
        }
        ReducedValue::None
    }
}

// Sub-return delegation chains (`return other()`,
// `shift->method(...)`) used to be represented as
// `ReturnDelegation(name)` / `SelfMethodTail(name)` observations
// chased via `ReducerQuery::return_of`. After the edge-fact
// migration (May 2026) the bag-side representation is
// `Edge(NamedSub(name))` — registry materialization handles the
// chase transparently, so dedicated reducers are no longer
// needed. The procedural fixed-point loops in
// `Builder::propagate_via_delegation` /
// `Builder::propagate_via_self_method_tails` continue to own
// build-time delegation propagation (they read the
// `sub_return_delegations` / `self_method_tails` maps directly,
// not the bag).

// ---- Named-sub return reducer ----
//
// Claims plain `InferredType` payloads on `NamedSub` attachments —
// the import-side equivalent of `Symbol.return_type` for local subs.
// Produced by `FileAnalysis::enrich_imported_types_with_keys` so
// imports flow through the SAME bag-query path as locals; no
// separate `imported_return_types` lookup at consumer sites.

pub struct NamedSubReturn;

impl WitnessReducer for NamedSubReturn {
    fn name(&self) -> &str {
        "named_sub_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::NamedSub(_))
            && matches!(w.payload, WitnessPayload::InferredType(_))
    }

    fn reduce(&self, ws: &[&Witness], _q: &ReducerQuery) -> ReducedValue {
        // One witness per import — the latest one wins (enrichment
        // truncates back to baseline before re-deriving, so there's
        // never accumulated stale state).
        for w in ws.iter().rev() {
            if let WitnessPayload::InferredType(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

// ---- Return-arm reducer ----
//
// Claims `InferredType` payloads on `ReturnArm(_)` attachments —
// the per-arm storage that every `return X` in a sub body
// publishes. The walker pushes either `Type(t)` directly (literals,
// constructors) or `Edge(target)` (variable returns). Edge witnesses
// get materialized by the registry into synthetic `Type` witnesses
// before any reducer claims, so this reducer always sees plain
// types. Latest-wins: chain typing's post-walk refresh pass pushes
// updated `Type` witnesses on a different source tag, and we want
// that latest value to dominate the original walk-time push.

pub struct ReturnArmReturn;

impl WitnessReducer for ReturnArmReturn {
    fn name(&self) -> &str {
        "return_arm_return"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::ReturnArm(_))
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
// FluentArityDispatch) gets a chance to claim Symbol attachments.
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

// ---- Reducer registry ----

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
        r.register(Box::new(BranchArmFold));
        r.register(Box::new(FrameworkAwareTypeFold));
        r.register(Box::new(FluentArityDispatch));
        r.register(Box::new(NamedSubReturn));
        r.register(Box::new(ReturnArmReturn));
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
    /// the walker had baked it directly. Cycle guard via the visited
    /// set breaks `A → B → A`.
    pub fn query(&self, bag: &WitnessBag, q: &ReducerQuery) -> ReducedValue {
        let mut visited: std::collections::HashSet<WitnessAttachment> =
            std::collections::HashSet::new();
        self.query_rec(bag, q, &mut visited)
    }

    fn query_rec(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        visited: &mut std::collections::HashSet<WitnessAttachment>,
    ) -> ReducedValue {
        if !visited.insert(q.attachment.clone()) {
            return ReducedValue::None;
        }
        let materialized = self.materialize(bag, q, visited);
        visited.remove(q.attachment);

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
    /// `BagContext`, materialize delegates to `query_variable_type`
    /// (the same helper file-side variable lookups use). Without a
    /// context the chase is a flat single-attachment query —
    /// adequate for tests and for cases where the witness exists at
    /// the exact scope.
    fn materialize(
        &self,
        bag: &WitnessBag,
        q: &ReducerQuery,
        visited: &mut std::collections::HashSet<WitnessAttachment>,
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
                            query_variable_type(
                                bag,
                                ctx.scopes,
                                ctx.package_framework,
                                name,
                                *scope,
                                point,
                            )
                        }
                        _ => {
                            let sub_q = ReducerQuery {
                                attachment: target,
                                point: q.point,
                                framework: q.framework,
                                arity_hint: q.arity_hint,
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
/// attachment) and imported / cross-file subs (`NamedSub` attachment,
/// pushed by enrichment) through one path. Callers don't need to
/// branch on "is this local"; the bag has the right witnesses either
/// way and the reducer registry produces an answer if one exists.
pub fn query_sub_return_type(
    bag: &WitnessBag,
    symbols: &[crate::file_analysis::Symbol],
    sub_name: &str,
    arity_hint: Option<u32>,
) -> Option<InferredType> {
    // Local-symbol bag query first — picks up arity-discriminated
    // dispatch via FluentArityDispatch.
    let reg = ReducerRegistry::with_defaults();
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
            context: None,
        };
        if let ReducedValue::Type(t) = reg.query(bag, &q) {
            return Some(t);
        }
    }
    // Name-keyed multi-dispatch and cross-file imports both ride
    // `NamedSub` — frameworks like Mojo::Base publish per-arity
    // observations on the logical name (one getter + one writer share
    // it), and `enrich_imported_types_with_keys` mirrors imported
    // return types here. We query this BEFORE falling back to the
    // local symbol's stored `return_type`, otherwise the first matching
    // Symbol's type would pre-empt the multi-dispatch answer (e.g.
    // `level(1)` would return the getter's String instead of the
    // writer's invocant ClassName).
    let att = WitnessAttachment::NamedSub(sub_name.to_string());
    let q = ReducerQuery {
        attachment: &att,
        point: None,
        framework: FrameworkFact::Plain,
        arity_hint,
        context: None,
    };
    if let ReducedValue::Type(t) = reg.query(bag, &q) {
        return Some(t);
    }
    // Final fallback: the local symbol's stored `return_type`. This
    // covers plain subs whose return came from the per-arm fold and
    // got written back to `Symbol.return_type` directly, with no
    // name-keyed observations published.
    if let Some(sym) = local_sym {
        if let crate::file_analysis::SymbolDetail::Sub { return_type, .. } = &sym.detail {
            if let Some(t) = return_type {
                return Some(t.clone());
            }
        }
    }
    None
}

/// Walk the scope chain from `scope` upward, asking the reducer
/// registry to fold every Variable witness for `var`. Returns the
/// first scope that produces a typed answer; `None` if no scope on
/// the chain has any matching witness or the reducer rejects them all.
pub fn query_variable_type(
    bag: &WitnessBag,
    scopes: &[Scope],
    package_framework: &HashMap<String, FrameworkFact>,
    var: &str,
    scope: ScopeId,
    point: Point,
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

    let reg = ReducerRegistry::with_defaults();
    let ctx = BagContext { scopes, package_framework };
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
            context: Some(&ctx),
        };
        if let ReducedValue::Type(t) = reg.query(bag, &q) {
            return Some(t);
        }
    }
    None
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------

#[cfg(test)]
#[path = "witnesses_tests.rs"]
mod tests;
