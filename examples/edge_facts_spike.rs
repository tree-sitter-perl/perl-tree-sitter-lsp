//! Spike: edge facts in the witness bag.
//!
//! Self-contained demonstration of `WitnessPayload::Edge(target)` —
//! a payload variant that means "the value at my attachment is
//! whatever `ctx.query(target)` resolves to." Reducers stay
//! attachment-claim-shaped; the registry chases edges transitively
//! via internal recursion, with a cycle guard.
//!
//! Companion to `docs/spike-edge-facts.md`. Use:
//!     cargo run --example edge_facts_spike
//!     cargo test --example edge_facts_spike
//!
//! Types are simplified — no tree-sitter, no scopes, no narrowing —
//! so the registry mechanics are visible without integration noise.
//! The mechanism shown here is what ports into `src/witnesses.rs`.

#![allow(dead_code)] // design surface — Lit / name() / etc. are illustrative

use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------
// Simplified core types
// ---------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum InferredType {
    Numeric,
    Str,
    ClassName(String),
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WitnessAttachment {
    /// Sub identified by name (stand-in for `Symbol(SymbolId)` /
    /// `NamedSub(String)` in the real bag).
    Sub(String),
    /// Variable identified by name (stand-in for `Variable { name, scope }`).
    Var(String),
    /// Literal at a span (stand-in for `Expression(RefIdx)` for literals).
    Lit(usize),
}

#[derive(Debug, Clone, PartialEq)]
enum WitnessPayload {
    /// Direct value — walker baked it because it could (literal,
    /// constructor, plugin override).
    Type(InferredType),
    /// Edge fact — "my type is whatever resolves at `target`."
    /// Reducers see this and the registry chases it transitively.
    Edge(WitnessAttachment),
}

#[derive(Debug, Clone, PartialEq)]
enum WitnessSource {
    Builder,
    Plugin,
}

impl WitnessSource {
    fn priority(&self) -> u8 {
        match self {
            WitnessSource::Plugin => 100,
            WitnessSource::Builder => 10,
        }
    }
}

#[derive(Debug, Clone)]
struct Witness {
    attachment: WitnessAttachment,
    payload: WitnessPayload,
    source: WitnessSource,
}

#[derive(Default)]
struct WitnessBag {
    by_attachment: HashMap<WitnessAttachment, Vec<Witness>>,
}

impl WitnessBag {
    fn push(&mut self, w: Witness) {
        self.by_attachment
            .entry(w.attachment.clone())
            .or_default()
            .push(w);
    }

    fn for_attachment(&self, att: &WitnessAttachment) -> &[Witness] {
        self.by_attachment
            .get(att)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }
}

// ---------------------------------------------------------------
// Reducer machinery
// ---------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum ReducedValue {
    Type(InferredType),
    None,
}

/// What a reducer sees: every witness on the queried attachment that
/// it claimed, plus — for any `Edge` witnesses on the same attachment
/// the registry already chased — the resolved values. The reducer
/// folds these into one answer.
struct ReducerInput<'a> {
    /// Non-edge witnesses the reducer claimed.
    direct: Vec<&'a Witness>,
    /// `(witness, resolved_value)` pairs for edges the registry
    /// resolved on this attachment. Reducers that care about edges
    /// (arm folding, delegation chasing) read these.
    chased: Vec<(&'a Witness, InferredType)>,
}

trait WitnessReducer {
    fn name(&self) -> &str;
    /// Does this reducer claim the witness as one of its inputs?
    /// `Edge` witnesses are also offered — a reducer can claim them
    /// or ignore them. The registry chases unclaimed edges anyway
    /// when it's running an attachment query, but a reducer that
    /// claims edges sees them in `ReducerInput.chased` after the
    /// chase resolves.
    fn claims(&self, w: &Witness) -> bool;
    fn reduce(&self, input: ReducerInput) -> ReducedValue;
}

#[derive(Default)]
struct ReducerRegistry {
    reducers: Vec<Box<dyn WitnessReducer>>,
}

impl ReducerRegistry {
    fn register(&mut self, r: Box<dyn WitnessReducer>) {
        self.reducers.push(r);
    }

    /// Top-level query — wraps `query_with_visited` with an empty
    /// cycle-guard set.
    fn query(&self, bag: &WitnessBag, att: &WitnessAttachment) -> ReducedValue {
        let mut visited = HashSet::new();
        self.query_with_visited(bag, att, &mut visited)
    }

    /// Recursive query with cycle guard. Visiting an attachment we're
    /// already resolving returns `None` — breaks delegation cycles
    /// (`A → B → A`).
    fn query_with_visited(
        &self,
        bag: &WitnessBag,
        att: &WitnessAttachment,
        visited: &mut HashSet<WitnessAttachment>,
    ) -> ReducedValue {
        if !visited.insert(att.clone()) {
            return ReducedValue::None;
        }
        let result = self.query_inner(bag, att, visited);
        visited.remove(att);
        result
    }

    fn query_inner(
        &self,
        bag: &WitnessBag,
        att: &WitnessAttachment,
        visited: &mut HashSet<WitnessAttachment>,
    ) -> ReducedValue {
        let witnesses = bag.for_attachment(att);
        if witnesses.is_empty() {
            return ReducedValue::None;
        }

        // Each reducer runs in registry order. First non-`None` wins.
        for r in &self.reducers {
            let claimed: Vec<&Witness> = witnesses.iter().filter(|w| r.claims(w)).collect();
            if claimed.is_empty() {
                continue;
            }

            // Split into direct values vs edges. Resolve edges via
            // recursive registry query — this is the "edge facts
            // reduce wherever they point" mechanic.
            let mut direct: Vec<&Witness> = Vec::new();
            let mut chased: Vec<(&Witness, InferredType)> = Vec::new();
            for w in &claimed {
                match &w.payload {
                    WitnessPayload::Type(_) => direct.push(*w),
                    WitnessPayload::Edge(target) => {
                        if let ReducedValue::Type(t) =
                            self.query_with_visited(bag, target, visited)
                        {
                            chased.push((*w, t));
                        }
                    }
                }
            }

            let v = r.reduce(ReducerInput { direct, chased });
            if v != ReducedValue::None {
                return v;
            }
        }

        ReducedValue::None
    }
}

// ---------------------------------------------------------------
// Reducers
// ---------------------------------------------------------------

/// Plugin override — claims any `Type(_)` witness with `Plugin`
/// priority on a `Sub(_)` attachment. Highest priority wins. Direct
/// value, no edge chase.
///
/// Registered first so plugin pins dominate before arm-folding /
/// delegation-chasing get a chance.
struct PluginOverride;

impl WitnessReducer for PluginOverride {
    fn name(&self) -> &str {
        "plugin_override"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Sub(_))
            && matches!(w.payload, WitnessPayload::Type(_))
            && w.source.priority() > 10
    }

    fn reduce(&self, input: ReducerInput) -> ReducedValue {
        // Highest-priority direct wins. Edges aren't valid plugin
        // overrides — plugin pins are concrete types.
        let mut best: Option<(&Witness, u8)> = None;
        for w in &input.direct {
            let pr = w.source.priority();
            if best.map(|(_, p)| pr >= p).unwrap_or(true) {
                best = Some((w, pr));
            }
        }
        if let Some((w, _)) = best {
            if let WitnessPayload::Type(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

/// Sub-return arm fold. Claims `Type(_)` and `Edge(_)` on `Sub(_)`
/// — value arms (return 42) and edge arms (return $foo) coexist.
///
/// Multi-arm rule: at least 2 arms required for "agreement"; all
/// must reduce to the same type. Single arm yields to the next
/// reducer (matches the existing `BranchArmFold` semantics in
/// `src/witnesses.rs:660`). Disagreement → `None`, never silently
/// pick one arm.
///
/// Single-arm or zero-arm cases (a sub with one return, or only an
/// implicit return) intentionally fall through to `DelegationChase`
/// — which can resolve when the sub's sole arm is an edge to
/// another sub's return.
struct SubArmFold;

impl WitnessReducer for SubArmFold {
    fn name(&self) -> &str {
        "sub_arm_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        // Claim Builder-priority witnesses on Sub attachments —
        // arms come from the walker, not plugins.
        matches!(w.attachment, WitnessAttachment::Sub(_))
            && w.source == WitnessSource::Builder
    }

    fn reduce(&self, input: ReducerInput) -> ReducedValue {
        let mut arms: Vec<InferredType> = Vec::new();
        for w in &input.direct {
            if let WitnessPayload::Type(t) = &w.payload {
                arms.push(t.clone());
            }
        }
        for (_, t) in &input.chased {
            arms.push(t.clone());
        }
        if arms.len() < 2 {
            return ReducedValue::None;
        }
        let first = &arms[0];
        if arms.iter().all(|t| t == first) {
            return ReducedValue::Type(first.clone());
        }
        ReducedValue::None
    }
}

/// Delegation chase. Same claim shape as `SubArmFold`, but only
/// fires on the single-edge case — the sub has exactly one
/// outgoing edge and no direct value witnesses. The edge IS the
/// answer.
///
/// In the real bag this collapses `DelegationReducer` and
/// `SelfMethodTailReducer`: both today push `ReturnDelegation(name)`
/// / `SelfMethodTail(name)` which the closure-driven reducers chase.
/// With edges, both become `Edge(NamedSub(name))` and this single
/// reducer handles them.
struct DelegationChase;

impl WitnessReducer for DelegationChase {
    fn name(&self) -> &str {
        "delegation_chase"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Sub(_))
            && w.source == WitnessSource::Builder
    }

    fn reduce(&self, input: ReducerInput) -> ReducedValue {
        // Single edge, no direct → resolve to the edge's chased value.
        if input.direct.is_empty() && input.chased.len() == 1 {
            return ReducedValue::Type(input.chased[0].1.clone());
        }
        ReducedValue::None
    }
}

/// Variable type fold — direct values only. Models the
/// `FrameworkAwareTypeFold` for the simplified case (no class /
/// rep / scalar axes here). Latest-wins.
struct VarTypeFold;

impl WitnessReducer for VarTypeFold {
    fn name(&self) -> &str {
        "var_type_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Var(_))
            && matches!(w.payload, WitnessPayload::Type(_))
    }

    fn reduce(&self, input: ReducerInput) -> ReducedValue {
        for w in input.direct.iter().rev() {
            if let WitnessPayload::Type(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

/// Literal type — same shape as VarTypeFold but for `Lit`
/// attachments. Lets the spike show that an edge from a Sub to a
/// literal expression resolves the same way as an edge from a Sub
/// to a variable. In the real bag this is a sub-case of
/// `FrameworkAwareTypeFold` reading `Expression(RefIdx)`.
struct LiteralFold;

impl WitnessReducer for LiteralFold {
    fn name(&self) -> &str {
        "literal_fold"
    }

    fn claims(&self, w: &Witness) -> bool {
        matches!(w.attachment, WitnessAttachment::Lit(_))
            && matches!(w.payload, WitnessPayload::Type(_))
    }

    fn reduce(&self, input: ReducerInput) -> ReducedValue {
        for w in input.direct.iter().rev() {
            if let WitnessPayload::Type(t) = &w.payload {
                return ReducedValue::Type(t.clone());
            }
        }
        ReducedValue::None
    }
}

fn default_registry() -> ReducerRegistry {
    let mut r = ReducerRegistry::default();
    // Order matters: PluginOverride short-circuits before
    // attachment-walking reducers; SubArmFold (≥2 arms agreement)
    // before DelegationChase (1 edge fallthrough); leaf-attachment
    // reducers register too.
    r.register(Box::new(PluginOverride));
    r.register(Box::new(SubArmFold));
    r.register(Box::new(DelegationChase));
    r.register(Box::new(VarTypeFold));
    r.register(Box::new(LiteralFold));
    r
}

// ---------------------------------------------------------------
// Demonstration
// ---------------------------------------------------------------

fn main() {
    let mut bag = WitnessBag::default();
    let reg = default_registry();

    // Walker-emitted: $foo's type is String (from earlier assignment).
    bag.push(Witness {
        attachment: WitnessAttachment::Var("$foo".into()),
        payload: WitnessPayload::Type(InferredType::Str),
        source: WitnessSource::Builder,
    });

    // Walker-emitted: $bar's type is also String.
    bag.push(Witness {
        attachment: WitnessAttachment::Var("$bar".into()),
        payload: WitnessPayload::Type(InferredType::Str),
        source: WitnessSource::Builder,
    });

    // Sub `say_thing`'s body:  return $foo;  return $bar;
    // Walker emits TWO edges from Sub("say_thing") to the variables.
    // No deferred field. No baked type. Both arms are edge facts.
    bag.push(Witness {
        attachment: WitnessAttachment::Sub("say_thing".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Var("$foo".into())),
        source: WitnessSource::Builder,
    });
    bag.push(Witness {
        attachment: WitnessAttachment::Sub("say_thing".into()),
        payload: WitnessPayload::Edge(WitnessAttachment::Var("$bar".into())),
        source: WitnessSource::Builder,
    });

    let result = reg.query(&bag, &WitnessAttachment::Sub("say_thing".into()));
    println!("say_thing returns: {result:?}");
    // Expected: Type(Str) — both arms resolve to String, agreement holds.
}

// ---------------------------------------------------------------
// Tests — each exercises one design property
// ---------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn builder_type(att: WitnessAttachment, t: InferredType) -> Witness {
        Witness {
            attachment: att,
            payload: WitnessPayload::Type(t),
            source: WitnessSource::Builder,
        }
    }

    fn builder_edge(att: WitnessAttachment, target: WitnessAttachment) -> Witness {
        Witness {
            attachment: att,
            payload: WitnessPayload::Edge(target),
            source: WitnessSource::Builder,
        }
    }

    fn plugin_type(att: WitnessAttachment, t: InferredType) -> Witness {
        Witness {
            attachment: att,
            payload: WitnessPayload::Type(t),
            source: WitnessSource::Plugin,
        }
    }

    /// Property 1: a plain literal-typed Var resolves to its type.
    /// Establishes the trivial base case.
    #[test]
    fn var_with_direct_type_resolves() {
        let mut bag = WitnessBag::default();
        bag.push(builder_type(
            WitnessAttachment::Var("$x".into()),
            InferredType::Numeric,
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Var("$x".into())),
            ReducedValue::Type(InferredType::Numeric)
        );
    }

    /// Property 2: arms-as-edges fold by agreement when targets
    /// resolve to the same type.
    /// THIS IS THE CORE WIN — the walker emitted no `BranchArm(t)`
    /// and never baked a type for the variable arms. The registry
    /// chased each edge, the values agree, the fold lands.
    #[test]
    fn sub_with_two_edge_arms_agreeing() {
        let mut bag = WitnessBag::default();
        bag.push(builder_type(
            WitnessAttachment::Var("$a".into()),
            InferredType::Str,
        ));
        bag.push(builder_type(
            WitnessAttachment::Var("$b".into()),
            InferredType::Str,
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("f".into()),
            WitnessAttachment::Var("$a".into()),
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("f".into()),
            WitnessAttachment::Var("$b".into()),
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("f".into())),
            ReducedValue::Type(InferredType::Str)
        );
    }

    /// Property 3: arm fold returns `None` on disagreement. Caller
    /// surfaces the disagreement; the bag never silently picks an arm.
    #[test]
    fn sub_with_two_edge_arms_disagreeing() {
        let mut bag = WitnessBag::default();
        bag.push(builder_type(
            WitnessAttachment::Var("$a".into()),
            InferredType::Str,
        ));
        bag.push(builder_type(
            WitnessAttachment::Var("$b".into()),
            InferredType::Numeric,
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("f".into()),
            WitnessAttachment::Var("$a".into()),
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("f".into()),
            WitnessAttachment::Var("$b".into()),
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("f".into())),
            ReducedValue::None
        );
    }

    /// Property 4: mixed direct + edge arms fold uniformly. The
    /// walker bakes literal returns directly, defers variable
    /// returns to edges. The fold treats them identically.
    #[test]
    fn sub_with_mixed_direct_and_edge_arms() {
        let mut bag = WitnessBag::default();
        // Direct arm: `return 42;`
        bag.push(builder_type(
            WitnessAttachment::Sub("f".into()),
            InferredType::Numeric,
        ));
        // Edge arm: `return $n;` where $n is Numeric.
        bag.push(builder_type(
            WitnessAttachment::Var("$n".into()),
            InferredType::Numeric,
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("f".into()),
            WitnessAttachment::Var("$n".into()),
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("f".into())),
            ReducedValue::Type(InferredType::Numeric)
        );
    }

    /// Property 5: a sub with a single edge and no direct values —
    /// classic delegation. The chase resolves through `DelegationChase`.
    /// THIS IS THE CLOSURE-FREE EQUIVALENT of today's
    /// `DelegationReducer` + `q.return_of` closure.
    #[test]
    fn delegation_resolves_via_edge_chase() {
        let mut bag = WitnessBag::default();
        // sub b returns String directly.
        bag.push(builder_type(
            WitnessAttachment::Sub("b".into()),
            InferredType::Str,
        ));
        // Wait — Sub takes an arm fold. We need ≥2 arms for fold,
        // OR we're modelling delegation where sub b's body is a
        // single direct return. With one direct witness on b,
        // SubArmFold returns None (need ≥2), DelegationChase
        // returns None (no edges), so b would resolve to None.
        //
        // Real-world fix: b should have either ≥2 arms or be
        // resolved via implicit-return mechanics. For the test, give
        // b a second direct arm to satisfy the fold.
        bag.push(builder_type(
            WitnessAttachment::Sub("b".into()),
            InferredType::Str,
        ));
        // sub a delegates to b via an edge.
        bag.push(builder_edge(
            WitnessAttachment::Sub("a".into()),
            WitnessAttachment::Sub("b".into()),
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("a".into())),
            ReducedValue::Type(InferredType::Str)
        );
    }

    /// Property 6: cycle protection. `a → b → a` doesn't blow the
    /// stack; the visited set kicks in and returns None.
    #[test]
    fn delegation_cycle_terminates() {
        let mut bag = WitnessBag::default();
        bag.push(builder_edge(
            WitnessAttachment::Sub("a".into()),
            WitnessAttachment::Sub("b".into()),
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("b".into()),
            WitnessAttachment::Sub("a".into()),
        ));
        let reg = default_registry();
        // Both query results are None — neither can resolve without
        // the other. Critically: the call returns rather than
        // recursing forever.
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("a".into())),
            ReducedValue::None
        );
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("b".into())),
            ReducedValue::None
        );
    }

    /// Property 7: plugin override beats edges and direct arms.
    /// Priority short-circuit dominates the fold. This is the
    /// canonicality guarantee — overrides win because of source
    /// priority, not because of seed-pass ordering.
    #[test]
    fn plugin_override_dominates_arms() {
        let mut bag = WitnessBag::default();
        // Two real arms that would fold to Numeric.
        bag.push(builder_type(
            WitnessAttachment::Sub("f".into()),
            InferredType::Numeric,
        ));
        bag.push(builder_type(
            WitnessAttachment::Sub("f".into()),
            InferredType::Numeric,
        ));
        // Plugin says: it's actually a class.
        bag.push(plugin_type(
            WitnessAttachment::Sub("f".into()),
            InferredType::ClassName("Quux".into()),
        ));
        let reg = default_registry();
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("f".into())),
            ReducedValue::Type(InferredType::ClassName("Quux".into()))
        );
    }

    /// Property 8: resolution chains. sub a → sub b → literal.
    /// Demonstrates that edges compose without special-case wiring.
    #[test]
    fn transitive_edge_chain_resolves() {
        let mut bag = WitnessBag::default();
        // Literal at index 7 has type Str.
        bag.push(builder_type(
            WitnessAttachment::Lit(7),
            InferredType::Str,
        ));
        // sub b's body: return Lit(7); — needs 2 arms for fold, so
        // duplicate. (The duplication is just a quirk of the
        // simplified SubArmFold contract; the real `BranchArmFold`
        // has a single-arm fallback path. The point being tested
        // here is multi-hop edge resolution, not the single-arm
        // case.)
        bag.push(builder_edge(
            WitnessAttachment::Sub("b".into()),
            WitnessAttachment::Lit(7),
        ));
        bag.push(builder_edge(
            WitnessAttachment::Sub("b".into()),
            WitnessAttachment::Lit(7),
        ));
        // sub a's body: return b(); — single edge, single hop.
        bag.push(builder_edge(
            WitnessAttachment::Sub("a".into()),
            WitnessAttachment::Sub("b".into()),
        ));
        let reg = default_registry();
        // a resolves to whatever b resolves, which resolves to Str.
        assert_eq!(
            reg.query(&bag, &WitnessAttachment::Sub("a".into())),
            ReducedValue::Type(InferredType::Str)
        );
    }
}
