# Spike: edge facts in the witness bag

> Status: design sketch + runnable demo
> (`examples/edge_facts_spike.rs`, 8 passing tests). Goal is to
> evaluate whether one new payload variant collapses the
> deferred-field / closure-plumbing / walk-time-baking trio into a
> single uniform mechanism. The example is self-contained
> (simplified types, no tree-sitter integration) so the registry
> mechanics — edge chase, cycle guard, plugin priority — are visible
> in isolation.
>
> Run:
>
> ```
> cargo test --example edge_facts_spike
> cargo run  --example edge_facts_spike
> ```

## TL;DR

The bag-as-facts model is right. The rot is that **payloads are
value-shaped when half of them want to be edge-shaped**. Three
existing observation variants (`ReturnOf`, `ReturnOfName`,
`ReturnDelegation`, `SelfMethodTail`) are already morally edges —
they carry a target identifier and the reducer chases via a closure.
The other half (`BranchArm(InferredType)`, `ArityReturn { return_type }`,
`BlessTarget(Rep)`) carry baked values, forcing the walker to
eager-resolve, which spawns the `deferred_var` side-channel.

**Minimal proposal:** add one payload variant —
`Edge(WitnessAttachment)` — meaning "the value at my attachment
is whatever `ctx.query(target)` resolves to." Reducers see edge
witnesses and fold via the registry on the target, transitively.

The edge-fact doesn't replace existing observations. It complements
them: where a witness today has to pick "I'm a value" or "I'm a
deferred lookup" (forcing the deferred field to handle the second
case), it can now say "I'm an edge to that other attachment."
Walker emits the edge; reducer resolves at query time.

## What the existing reducers actually do

Quick taxonomy of `src/witnesses.rs` reducers and their *real* shape,
not their nominal one:

| Reducer | Claims | Real operation |
|---|---|---|
| `FrameworkAwareTypeFold` (407-617) | Variable / Expression with InferredType / Observation | Multi-axis fold (class × rep × scalar context). Half of it chases edges (`ReturnOf` → `q.return_of` closure) but pretends it's "folding observations." |
| `BranchArmFold` (660-697) | Any with `BranchArm(InferredType)` | Equality-fold over baked values. Forces the walker to bake every arm's type at emit time. |
| `FluentArityDispatch` (711-766) | Symbol / NamedSub with `ArityReturn { return_type }` | Per-arity pick of baked return. Same baking problem as BranchArm. |
| `DelegationReducer` (788-823) | Symbol with `ReturnDelegation(name)` | **Edge chase** via `q.return_of` closure. Already an edge in disguise. |
| `SelfMethodTailReducer` (828-859) | Symbol with `SelfMethodTail(name)` | Same. Edge chase via closure. |
| `NamedSubReturn` (869-892) | NamedSub with InferredType | Latest-wins value fold. |
| `PluginOverrideReducer` (912-947) | Symbol with InferredType, source priority > 10 | Priority short-circuit. |

Three things jump out:

1. **Two distinct chase paths exist already** — `ReturnOf(SymbolId)`
   and `ReturnOfName(String)` (chased inside `FrameworkAwareTypeFold`,
   lines 518-528, 543-553) plus `ReturnDelegation(name)` /
   `SelfMethodTail(name)` (in their dedicated reducers). All four
   route through the same `q.return_of: Option<&dyn Fn(&ReturnOfKey) -> Option<InferredType>>`
   closure. That closure is the manual implementation of
   "follow this edge."

2. **The baking-forced reducers are exactly the ones that need a
   deferred field.** `BranchArmFold` claims `BranchArm(InferredType)`
   — type pre-baked. When the walker hits `return $foo`, it can't
   bake $foo's type, so it goes around the bag and stashes the
   variable identity in `ReturnInfo.deferred_var`. The bag never
   sees the arm until `collect_return_arm_types` resolves the
   variable post-walk and pushes a synthesized `ArityReturn` /
   per-arm fold into `returns_by_scope`.

3. **`FrameworkAwareTypeFold` is not really one reducer.** It's a
   multi-axis fold (class × rep × scalar) plus an edge-chase escape
   hatch for `ReturnOf`. The edge-chase doesn't belong; it's a
   separate concern that happens to share the attachment.

## Proposed shape: one new payload variant

```rust
pub enum WitnessPayload {
    InferredType(InferredType),
    Observation(TypeObservation),

    /// Edge fact: "the value at my attachment is whatever the bag
    /// resolves at `target`." Reducers handle this uniformly via
    /// `ctx.query(&target)` — same registry, transitive.
    ///
    /// Caller-supplied semantics live in the *attachment shape* of
    /// the witness, not in the edge: a witness on `Symbol(S)` with
    /// `Edge(target)` means "S's return type comes from target";
    /// a witness on `Variable { name, scope }` with `Edge(target)`
    /// means "the variable's type comes from target."
    Edge(WitnessAttachment),

    Fact { family: String, key: String, value: FactValue },
    Derivation,
    Custom { family: String, json: String },
}
```

That's the **only** new variant. No `EdgeKind`. No multi-target
edges. The semantic role of an edge — "is this an arm, a
delegation, a tail call?" — is encoded by **which reducer claims
the witness**, not by the edge itself. Multiplicity is what
distinguishes "arm" (≥2 edges, fold by agreement) from "delegation"
(exactly 1 edge, take it). That's a property of the reducer that
claims the witnesses on a given attachment shape, not of the edge.

### What does "reduces wherever it points" mean concretely?

The registry's `query` gains a recursive form:

```rust
impl ReducerRegistry {
    pub fn query(&self, bag: &WitnessBag, q: &ReducerQuery) -> ReducedValue {
        let claimed: Vec<&Witness> = bag
            .for_attachment(q.attachment)
            .into_iter()
            .filter(|w| /* any reducer claims it OR it's an Edge */)
            .collect();

        // Edge resolution: any Edge witness contributes the type
        // at its target. The reducer that ultimately answers the
        // query sees the resolved values, not the raw edges.
        let resolved_via_edges: Vec<InferredType> = claimed
            .iter()
            .filter_map(|w| match &w.payload {
                WitnessPayload::Edge(target) => {
                    let sub_q = q.with_attachment(target);
                    if let ReducedValue::Type(t) = self.query(bag, &sub_q) {
                        Some(t)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();

        // Reducers that claim non-edge witnesses run as today, but
        // also see the resolved-edge contributions in their
        // `&[InferredType]` view.
        for r in &self.reducers {
            // ... claim filter, reduce(&claimed, &resolved_via_edges, q) ...
        }
        ReducedValue::None
    }
}
```

The chase is bounded: `WitnessAttachment` is a finite identity, the
bag is monotone (witnesses don't get deleted during a query), and
the lattice (`InferredType`) is finite. Cycle detection: a small
`HashSet<WitnessAttachment>` carried through recursion, returning
`None` on revisit.

### Reducers, post-spike

```rust
// BranchArmFold — disappears as a separate reducer. Or, equivalently,
// keeps the same role but reads from the registry's resolved-edge
// view instead of `BranchArm(InferredType)` payloads. The walker
// stops emitting BranchArm; it emits Edge instead.

// Walker, today:
//   pending_witnesses.push(Witness {
//       attachment: Symbol(sub_id),
//       payload: Observation(BranchArm(arm_type)),  // baked!
//       ...
//   });

// Walker, after:
//   pending_witnesses.push(Witness {
//       attachment: Symbol(sub_id),
//       payload: Edge(WitnessAttachment::Variable {
//           name: "$foo".into(),
//           scope: ri.scope,
//       }),
//       ...
//   });

// ReturnInfo.deferred_var: gone. The walker doesn't capture it
// because the Edge IS the capture — and it lives in the bag, where
// every other piece of analysis can see it.
```

```rust
// DelegationReducer / SelfMethodTailReducer — collapse into
// "the edge IS the witness." No more closure plumbing:

// Walker, today (push_delegation_witnesses, builder.rs:~6770):
//   bag.push(Witness {
//       attachment: Symbol(sub_id),
//       payload: Observation(ReturnDelegation(callee_name)),
//       ...
//   });

// Walker, after:
//   bag.push(Witness {
//       attachment: Symbol(sub_id),
//       payload: Edge(WitnessAttachment::NamedSub(callee_name)),
//       ...
//   });

// Reducer: gone. The registry's edge-chase resolves it. No
// `ReducerQuery::return_of` closure needed.
```

```rust
// FrameworkAwareTypeFold's ReturnOf branches (518-528, 543-553) —
// gone. Same edge.
```

`FluentArityDispatch` is the interesting case. `ArityReturn` carries
both an `arg_count` (the dispatch key) and a `return_type` (the
baked value). The arity dispatch is a real semantic fact — it's not
collapsible to a generic edge. But the return_type can be:

```rust
// Today:
TypeObservation::ArityReturn {
    arg_count: Some(0),
    return_type: InferredType::String,  // baked
}

// After (mixed observation + edge — payload becomes a small struct):
TypeObservation::ArityReturn {
    arg_count: Some(0),
    type_source: TypeSource::Direct(InferredType::String),
}
TypeObservation::ArityReturn {
    arg_count: None,
    type_source: TypeSource::Edge(Variable { name: "$self", ... }),
}

enum TypeSource {
    Direct(InferredType),
    Edge(WitnessAttachment),  // chase via ctx.query
}
```

Same idea: the *type half* of the observation becomes either-direct-
or-edge. The reducer dispatches by arity, then resolves the
type-source (direct value or edge chase).

### What stays as values

Not everything wants to be an edge. The walker has genuine value
knowledge for:

- Literal types: `42` is `Numeric`. `"hi"` is `String`. `[]` is
  `ArrayRef`. `{}` is `HashRef`.
- Constructor calls: `Foo->new` → `ClassName("Foo")`.
- Rep observations from access: `$v->{k}` → `HashRefAccess`. The
  walker sees the syntactic shape; baking the rep is fine.
- Plugin overrides: `InferredType` direct value with high priority.

These remain `InferredType(_)` / `Observation(_)` payloads. Edges
appear *only* when the walker can't bake — which is exactly the
deferred-field situation today, plus the closure-chase situations.

## What disappears

| Today | After |
|---|---|
| `ReturnInfo.deferred_var: Option<(String, Point)>` | Gone — walker emits `Edge(Variable {..})` directly. |
| `collect_return_arm_types` variable-resolution branch | Gone — bag query resolves the edge. |
| `ReducerQuery::return_of` closure | Gone — edge chase is registry-internal. |
| `TypeObservation::ReturnDelegation(name)` | Replaced by `Edge(NamedSub(name))`. |
| `TypeObservation::SelfMethodTail(name)` | Replaced by `Edge(NamedSub(name))`. |
| `TypeObservation::ReturnOf(SymbolId)` | Replaced by `Edge(Symbol(id))`. |
| `TypeObservation::ReturnOfName(name)` | Replaced by `Edge(NamedSub(name))`. |
| `BranchArm(InferredType)` baking pressure | Gone — walker emits `Edge` to the arm-source attachment. |
| `DelegationReducer`, `SelfMethodTailReducer` | Gone (subsumed by registry edge-chase). |
| `seed_plugin_overrides_into_return_types` etc. (the four-pass collapse Step 2 was after) | Becomes one query loop — same as Step 2's endpoint, but without closure wiring. |

## What stays the same

- `WitnessAttachment` — same identity space, same variants. (Possibly
  worth adding `WitnessAttachment::SubReturn(SymbolId)` later for
  cleaner factoring; not strictly needed for the spike.)
- `WitnessSource` + priority — plugin overrides still win via
  `WitnessSource::priority()`. Plugin emits `InferredType(_)`, not
  edge — overrides are direct values, not deferrals.
- `WitnessBag` storage — attachment-keyed `Vec<Witness>`. No change.
- The fact that `FrameworkAwareTypeFold` does multi-axis folding
  (class × rep × scalar) — that's a real semantic fold, edge-fact
  doesn't change it. Just removes the `ReturnOf` escape hatch.
- All plugin emissions (`emit_actions` from `on_use` / `on_function_call`
  / `on_method_call` hooks) — no schema change visible to plugins
  unless they were emitting `BranchArm`s (none do).

## Cost estimate

Migration is bounded by audit of three things:

1. **Walker emission sites.** Every `BranchArm(t)` / `ReturnDelegation`
   / `SelfMethodTail` / `ReturnOf` push needs to become an `Edge`
   push. Targets are usually obvious from context: the variable
   name + scope for return-arm captures, the called-sub's NamedSub
   for delegation. ~20-30 sites, all in `builder.rs`.

2. **`FrameworkAwareTypeFold`.** Strip the `ReturnOf` / `ReturnOfName`
   branches (~30 lines). Add an edge-chase invocation for whatever
   contributions the reducer needs — or accept that
   `resolved_via_edges` is computed by the registry before the
   reducer runs and threaded into `reduce()` via a new param. ~50
   lines net.

3. **Registry recursion.** New ~30 lines in `ReducerRegistry::query`
   for edge-chase + cycle guard. Tested standalone; the test surface
   is small (the bag is finite, the lattice is finite, terminate
   on revisit).

Affected tests: any test pushing `BranchArm` / `ReturnDelegation` /
`SelfMethodTail` / `ReturnOf` / `ReturnOfName` directly. They migrate
by replacing payload construction. Search shows ~15 such sites in
test files; mechanical.

Net diff: probably +200 / -350 across `witnesses.rs` + `builder.rs`
+ tests. Comparable to Step 2 of the unification staircase, but
delivers the actual semantic cleanup instead of just collapsing
seed passes.

## Why this beats the staircase

The unification staircase (`docs/prompt-type-inference-unification.md`)
collapses parallel paths into the bag, which is good. But it never
addresses the value/edge shape mismatch in payloads. After Step 4
the walker stops doing eager inference, every type query goes through
the bag, and the deferred field disappears — but only because the
walker no longer needs it. The bag itself still pretends every
payload is a value; the closures (`return_of`, the spike's
`array_shape_of`) survive as the kludge for "follow this name to
that other type."

Edge-facts attack the shape directly. After this spike's proposed
change:

- The walker still does eager inference where it can (literals,
  constructors, rep observations). That's fine — they're real values.
- Where it can't, it emits an edge. No deferred field. No closure
  wiring. No baked-type fiction.
- Reducers fold what they receive. The registry handles edge chase
  invisibly via recursion.

That's what "the bag is canonical" means in practice: not "every
query goes through the bag" (you can do that with closures and
side-channels, as Step 3-4 plan to), but **"the bag's payloads
encode the actual structure of partial knowledge."**

The staircase is patching the semantics. Edge-facts fix the shape.

## What this doesn't solve

- **Cross-method / cross-file mutation effects** — same as the
  staircase doc's §"What this won't solve". Effects aren't return
  values; they need their own witness family. Edge-facts compose
  with mutation effects but don't supply them.
- **`wantarray` / context-dispatched returns** — same; needs a
  context dimension on the dispatch key.
- **Aliasing through scalars across function boundaries** — same.
- **The `_route` chain in Mojo** — partially solved. With edges,
  the chain hop's invocant type is reachable via the bag (the slot
  shape attaches via `HashKey { owner, name }` and the chain hop's
  Expression edges to it). But the chain typer still needs to
  *emit* those edges, which means the walker's chain pass needs to
  understand that an Expression's invocant type is an edge to the
  slot shape. That's a builder change, edge-facts make it possible.

## Open questions

1. **Do we need named edge kinds, or is shape-by-attachment enough?**
   Today's draft says "reducer claims by attachment shape, edge is
   un-tagged." If two distinct semantic edges land on the same
   attachment (e.g. `Symbol(S)` could carry "delegation edge" AND
   "arm edge" simultaneously), the reducer at `Symbol` would need
   to distinguish. In practice today no symbol carries both — a
   sub is either delegation-shaped or arm-shaped — but if the
   distinction surfaces later, adding `Edge { kind, target }` is a
   minor extension. Pay for it then.

2. **Should `ReducerQuery` carry a `ReducerContext` handle for
   sub-queries?** This is Step 3 of the staircase doc. With
   edge-facts, the registry handles the recursion internally — most
   reducers don't *need* a context. But `FrameworkAwareTypeFold`
   does multi-axis folding; if it wants to re-query a sibling
   attachment (e.g. to look up a specific Variable's narrowing
   slice), a context handle helps. Suggest: add `ReducerContext`
   only when the first reducer actually needs it; don't fight that
   fight in this spike.

3. **`ArityReturn` schema change.** Adding `TypeSource::Edge` to its
   `return_type` field is a real schema change with cache
   implications (`bincode`). Bump `EXTRACT_VERSION`. Migration is
   one-shot (cache rebuilds), but it's a bump.

4. **What about `Container` / sequence types?** The spike's
   `SequenceAccess` attachment kind doesn't exist on this branch —
   sequence types live on a separate branch. Edge-facts compose
   cleanly with whatever sequence work lands: a sequence's element
   type is "edge to the contributing push site," same shape as
   everything else.

## Recommended next move

If the design holds up: skip Steps 1-2 of the unification staircase
entirely. Land edge-facts as a single PR, which delivers everything
Steps 1-3 were aiming at:

- Generalized deferred mechanism (Step 1) — falls out automatically;
  no `DeferredType` enum, no field at all.
- Collapsed seed passes (Step 2) — falls out; the registry's
  edge-chase replaces every procedural fixed-point loop with bag
  query plus monotone iteration to fixed point.
- Closure-leak elimination (Step 3) — falls out; `return_of` is
  unnecessary because the registry recurses internally.

Step 4 (delete walker eager inference) is independent. Edge-facts
make it cheaper (because every walk-time-baked type can become an
edge), but doesn't force it. Land separately as appetite allows.

If the design doesn't hold up — i.e., if the open questions surface
real obstacles — fall back to Step 2 only on the staircase. That's
still real cleanup and doesn't entrench the value-shape further
(the `Edge` variant can be added later atop a smaller bag).
