# Bag-residual action plan

The principle: **the bag is the only truth.** Every dual-store, every
syntactic bake, every "the field is canonical at this phase" caveat is
a hack we have to remember. They all go.

What's left is structured around four directives. Items are grouped
under the directive they serve so the cleanup never feels like
unrelated chores.

---

## Directive 1 — Delete `Symbol.return_type` and the FA-side method-typing — **LANDED (with residue)**

> **Status:** landed in `dc4315f` on `refactor/bag-residual-d1-redo`.
> The directive's biggest subtractions shipped (field gone, FA-side
> method typing gone, build-time chase gone, MRO bug fixed). Two
> bullets did not land as written and have been folded into D3 — see
> "Residue routed to D3" below. Smaller stylistic cleanups are in
> `docs/prompt-cleanups.md`.

The bag is the only type-query path in principle (CLAUDE.md: "the bag
is THE single type-query path"). In practice today, `Symbol.return_type`
is a load-bearing field that consumers read directly, and
`find_method_return_type_seen` is an FA-side recursive ancestor walk
that exists in parallel to the bag. Method dispatch is split: the bag
answers `NamedSub(name)` (name-keyed, conflates classes), the FA-side
walks the ancestor chain, and the seam is maintained by hand
(`fill_returns_via_bag_chase` is "restricted to delegators/tails"
precisely to dodge that conflation; that guard is the visible scar).

This directive **deletes the dual path**. The framing is subtractive,
not additive. Don't add a `MethodOnClass` attachment "alongside" the
field — delete the field, let the compiler enumerate the consumers,
and route every one of them through the bag's class-keyed query.

### The first attempt: what NOT to repeat

`refactor/bag-residual-d1-method-on-class` (commit `c322178`) was an
additive landing. It added `WitnessAttachment::MethodOnClass{class,
name}` and a procedural `walk_dispatch_chain` helper, routed
`find_method_return_type` through them, and kept everything else.
The build-time chase + guard stayed (under a confession comment
acknowledging it). The PR was abandoned because the residue
(field-still-load-bearing, build-time chase still fires `NamedSub`)
would have re-infected D3 and forced a follow-up PR to undo work.

Three specific traps from that attempt — name them so the redo
doesn't repeat them:

1. **Don't teach Builder to ask `MethodOnClass` while keeping the
   build-time chase.** That's the additive temptation: it makes the
   guard removable without deleting the field. It also locks in the
   chase as a permanent code path. Delete the chase instead. The
   chase only exists because `Symbol.return_type` is a field that
   consumers read; delete the field and the chase has no consumer.

2. **Don't reach for procedural walkers in the registry's
   `materialize`.** The first attempt added `walk_dispatch_chain`
   and called it from inside the reducer. Inheritance is supposed to
   compose through `Edge(MethodOnClass(parent, name))` witnesses
   emitted at build time — the registry's cycle-guarded edge chase
   gives MRO walking for free. If you find yourself writing a
   procedural walker that the materialize calls, stop and ask why the
   edge-witness path doesn't work. Cross-file bridges are not an
   exception — `BagContext` carries `&ModuleIndex` and the registry
   chases edges into cached modules' bags the same way it chases
   in-file edges.

3. **Don't shape helpers around borrow-bearing structs that escape
   closures.** The first attempt's `DispatchHit<'a>` carried
   `&Arc<CachedModule>` borrowed from
   `for_each_entity_bridged_to`'s loop body, which forced
   `dispatch_method_on_class` into a two-walk-then-find-by-sym-id
   shape (fragile: `SymbolId` is per-FA, so the second walk's
   sym-id lookup happens to work only because both walks iterate in
   identical order). If a helper is needed, hits are owned —
   `Arc<CachedModule>` clone is cheap, owned class names are fine.
   Or, if edge-witnesses replace the walker, no helper is needed at
   all.

### Concrete deletions (the directive's actual content)

Each bullet names a delete-able thing. The order is "delete biggest
first, let the compiler tell you what's missing, fix exactly that —
do not pre-emptively rewrite consumers."

- [x] **Delete `SymbolDetail::Sub.return_type` field.** Landed.
      Construction sites drop the literal; walk-time synthesis writes
      `Builder.resolved_returns` (build-only map); writeback publishes
      `Symbol(sid)` / `NamedSub(name)` / `MethodOnClass{class, name}`
      witnesses into the bag. The intermediate `resolved_returns`
      map was not in the original design — see
      `docs/prompt-cleanups.md` for its scheduled deletion.

- [~] **Delete `imported_return_types`** — partial. The FA field is
      gone, but `backend.rs::build_imported_return_types` survives and
      still copies imported sub return types into the local bag as
      `NamedSub` witnesses via `enrich_imported_types_with_keys`. The
      cross-file `MethodOnClass` query reaches the same data through
      `BagContext.module_index`, so the copy is now a parallel path.
      **Routed to D3** — bullet "Delete `build_imported_return_types`."

- [x] **Delete `find_method_return_type_seen`,
      `find_method_return_type_raw`, and `self_method_tail`.** Landed.
      `find_method_return_type` is now a thin wrapper that builds a
      `MethodOnClass{class, name}` `ReducerQuery` and dispatches
      through the registry.

- [x] **Delete `fill_returns_via_bag_chase`** and its
      delegators-and-tails-only guard. Landed — the function and its
      sole call site in `resolve_return_types` are gone.

- [x] **Add `WitnessAttachment::MethodOnClass { class, name }`.**
      Landed with owned `String`s, plus `MethodOnClassReducer` for the
      primary fallback. `FluentArityDispatch::claims` was extended to
      include `MethodOnClass` so per-arity facts dispatch on the same
      class-keyed shape.

- [~] **Emit `MethodOnClass(C, m) → Edge(MethodOnClass(P, m))`
      witnesses for inheritance** — **did NOT land as written.**
      Inheritance + plugin-namespace bridges resolve via a structural
      walk inside `query_rec`'s `MethodOnClass` fallback (consults
      `package_parents`, `module_index.parents_cached`, and
      `for_each_entity_bridged_to`), recursing through the same
      registry. It's centralized — one site, not many — but it is the
      "another bespoke walker" shape the next architectural pillar
      (`prompt-graph-walking.md`) wants to eliminate, and it reads as
      a softer version of trap #2 from the abandoned-attempt list.
      **Routed to D3** — bullet "Inheritance via Edge witnesses."

- [x] **Extend `BagContext` to carry `Option<&ModuleIndex>`** and
      `&HashMap<String, Vec<String>>` for `package_parents`. Landed.

- [x] **Fix `for_each_ancestor_class`'s DFS to left-to-right
      `@ISA` order.** Landed. Parents are now collected, then
      `into_iter().rev()`-pushed so LIFO pops in `@ISA` order. New
      regression test
      `for_each_ancestor_class_walks_left_to_right_isa_order` pins
      it. The structural disambiguation test
      (`method_on_class_disambiguates_same_name_across_classes`)
      is also green.

- [x] **Keep this test green:**
      `method_on_class_disambiguates_same_name_across_classes` —
      green on `dc4315f`.

### Residue routed to D3

Two D1 bullets did not land as written. They are listed verbatim in
D3's action items below so the tracking lives in one place:

1. **Inheritance via `Edge(MethodOnClass(parent, name))` witnesses.**
   Replace `query_rec`'s structural walk with build-time edge emission.
   The registry's existing edge-chase mechanism walks them with no
   special-case code, and the new code becomes nothing more than
   "push edges that already correspond to `package_parents` /
   bridges entries." Eliminates a bespoke walker before the
   graph-walking pillar inherits it.

2. **Delete `build_imported_return_types`.** Now that
   `BagContext.module_index` lets the registry reach into cached
   modules' bags directly, the function and the imported-return push
   inside `enrich_imported_types_with_keys` are redundant. Keep the
   `HashKeyDef` synthesis side of `enrich_imported_types_with_keys`;
   only the return-type push goes.

A third related cleanup — the new source-tag claims (`"delegation"`,
`"self_method_tail"`) added to `SubReturnReducer::claims` in this PR —
already lives under D3's "Kill source-tag claim filters" bullet. The
list there is no longer two filters, it's four.

### Non-goals (explicit)

- **Symbol.return_type as lazy cache.** The original D4 framing was
  "make `return_type` a lazy cache filled by reading the bag." After
  this directive deletes the field outright, the cache step is
  unnecessary unless profiling later shows the bag query is hot. Do
  not add the cache pre-emptively.

- **Procedural `walk_dispatch_chain` helper.** Tempting to lift from
  c322178; resist. If you find a consumer that genuinely cannot be
  expressed as an edge chase (e.g. completion enumerating *all*
  methods on a class, not resolving one), name it and we'll discuss —
  but the default is no procedural helper.

### Why one PR

The field deletion, the edge emitter, the `BagContext` extension, the
`find_method_return_type_seen` rewrite, and the chase-guard removal
are interlocked: any one of them landed alone leaves a dual path that
the next one has to undo. The first attempt proved this — it landed
the additive half and the residue was non-removable without redoing
work. ROADMAP.md's rule applies: do not split a directive across PRs.

---

## Directive 2 — All expressions are the same thing — **LANDED**

> **Status:** landed on `refactor/bag-residual-d2-expr-attachment`.
> Every action item below shipped. The walker now publishes through
> a single `Expr(Span)` attachment, the per-RI `arm_types_per_ri` /
> `returns_by_scope` plumbing is gone, and `fold_per_sub_return_arms`
> is one `query(Symbol(sid))` plus the implicit-last-expr fallback.

### What landed

- [x] **One expression attachment.** `WitnessAttachment::Expr(Span)`
      replaces `ReturnArm(Span)` and is the new "value of this
      expression" key. `Expression(RefIdx)` survives as ref metadata
      (the method-call chain typer still reads it for receiver
      resolution); the type answer for any rvalue expression rides
      `Expr(span)`.

- [x] **Walker emits `Expr` witnesses uniformly.** `expr_payload`
      is the single dispatch — literals bake `InferredType`, names
      push `Edge(Variable{...})` / `Edge(NamedSub)` /
      `Edge(Expression(refidx))`, ternaries push two `branch_arm`
      Edges to each arm's `Expr(span)` and recurse so chained
      ternaries each carry their own arm payloads.

- [x] **Killed `arm_payload`.** Replaced by `expr_payload` (every
      rvalue node, not just return arms) and `emit_expr_witness`.

- [x] **Killed `ReturnArm` and `ReturnArmReturn`.** `return EXPR` now
      emits `Edge(Expr(body_span))` source `branch_arm` on
      `Symbol(sub_id)`. Ternary returns expand to per-arm Edges so
      BranchArmFold has the ≥2 arms it needs; non-ternary returns
      go through the new `SymbolReturnArmFold` which accepts 1+ arms
      via `resolve_return_type` (replaces the per-RI Vec fold).

- [x] **Killed `last_expr_type`.** Replaced with `last_expr_span:
      HashMap<ScopeId, Span>` — the fold reads `Expr(last_expr_span)`
      via the bag, no side-channel type cache.

- [x] **Killed the `arm_types_per_ri` / `returns_by_scope` plumbing.**
      `emit_arity_return_witnesses` queries `Expr(body_span)` per RI
      inline; `fold_per_sub_return_arms` queries `Symbol(sub_id)`
      directly with `last_expr_span` as the implicit-return fallback.
      `collect_return_arm_types` and its tuple return are gone.

### Reducer shape

`SymbolReturnArmFold` is the new home for "this sub's return type
agreed across its arms." It's distinct from `BranchArmFold` because
the two have different agreement rules:

- `BranchArmFold` (Variable + Expr): ≥2 arms must agree. A single
  arm = inference fell short; surface ambiguity.
- `SymbolReturnArmFold` (Symbol): 1+ arms via `resolve_return_type`.
  A single `return X` IS the answer; multiple agreeing arms collapse
  to that type; disagreement → None (with HashRef subsumed by Object).

Registered before `BranchArmFold` so the registry's first-match
dispatch routes Symbol-attached witnesses through the Symbol-fold.
Both reducers claim source `branch_arm` + payload `InferredType`,
but their `claims` predicates filter by attachment shape, so they
never fight over the same witness set.

### Open design questions, resolved

1. **`Span` vs `NodeId` keying.** Spans. They survive the cache
   boundary; NodeIds don't.

2. **Do refs keep `Expression(RefIdx)`?** Yes. The method-call chain
   typer reads receiver-and-method-resolved types through it.
   `expr_payload` Edges from `Expr(span)` to `Expression(refidx)` for
   method-call rvalues, so the type answer still rides `Expr` —
   `Expression(refidx)` is an internal hop, not a parallel attachment.

3. **Anonymous compound expressions.** Lazy: the walker only emits
   `Expr(span)` for nodes that have a payload via `expr_payload` (or
   for ternaries via the `emit_expr_witness` recursion). Bare
   subexpressions only get witnesses when something Edges to them.

---

## Directive 3 — One attachment per fact, no source-tag claims — **LANDED (with residue)**

> **Status:** landed on `refactor/bag-residual-d3`. The variant
> `WitnessAttachment::NamedSub` is gone, the dual writeback collapsed
> to `Symbol(sid)` + `MethodOnClass{class, name}` only, cross-file
> imports route through `module_index.find_exporters` + recursive
> `Symbol(cached_sid)` query, and source-tag claim filters on
> `SubReturnReducer` collapsed to a single `branch_arm` exclusion.
> Two design choices departed from the original directive — see
> "Residue" below.

### What landed

- [x] **Killed `WitnessAttachment::NamedSub`** entirely. `cargo build`
      enumerated 17 consumers; each was migrated to `Symbol(sid)`
      (id-keyed) or `MethodOnClass{class, name}` (class-keyed) or
      deleted outright (writeback's name-keyed mirror,
      `record_framework_accessor_witness`'s name-keyed observation,
      `emit_delegation_edges`' name-keyed primary).

- [x] **Drop the dual `Symbol(_)` + `NamedSub(_)` writeback.** Local
      subs publish on `Symbol(sym_id)` + `MethodOnClass{class, name}`
      only. Plugin synth (`Method` and `Symbol` arms) drops the
      free-function `NamedSub` push and relies on `Symbol(sid)`.

- [x] **Cross-file imports stop riding `NamedSub`.**
      `query_sub_return_type` walks `idx.find_exporters(name)` and
      recurses into the cached module's bag with
      `Symbol(cached_sid)`. Same registry, same arity dispatch, same
      framework rules — only the bag and symbols change.

- [x] **Delete `build_imported_return_types`** (backend.rs) and the
      `imported_returns` / `imported_hash_keys` parameters on
      `enrich_imported_types_with_keys`. The function now derives
      `imported_hash_keys` inline from `self.imports` +
      `module_index`. Imported return types are reached lazily by
      the bag-query path; no local mirror is needed.

- [x] **Build-time `Edge(MethodOnClass(parent, name))` inheritance
      witnesses.** *(Routed from D1.)* Local writeback emits
      `MethodOnClass(child, m) → Edge(MethodOnClass(parent, m))` for
      every method `m` declared on a *local* parent, with
      first-parent-wins dedup so DFS-MRO order is preserved.
      `enrich_imported_types_with_keys` does the same projection for
      *cross-file* parents (their methods are read from the cached
      analysis). Tag `inheritance` (writeback) and
      `inheritance_cross` (enrichment).

- [x] **Method-call expression edges keyed on class.** D2 left
      `Edge(NamedSub(method))` on `Expression(refidx)` as the
      chain-typer's bootstrap. D3 replaces it with a re-emittable
      pass `emit_method_call_return_edges` that publishes
      `Edge(MethodOnClass{class, method})` for every `MethodCall` ref
      whose `invocant_class` is currently filled. Re-emission inside
      the worklist driver picks up newly-resolvable invocants each
      iteration; `apply_chain_typing_invocants` now runs in PreFold
      too (was PostFold-only) so the variable-typed receivers'
      classes land in the bag while the worklist is still iterating.
      Snapshot extended to track filled-`invocant_class` count so
      the loop registers the progression as movement.

- [x] **Source-tag claim filters collapsed.** `SubReturnReducer::claims`
      now matches by attachment shape + the single `branch_arm`
      exclusion (which still must stay — see residue). The four-way
      filter on `local_return` / `delegation` / `self_method_tail` /
      `imported_return` is gone.

### Residue

Two pieces of the original directive shipped differently than the
plan called for; both were design adjustments forced by the "what
if the caller bypasses enrichment?" question and the "build-time
edges N×N for cross-file plugin bridges" question.

1. **`query_rec`'s structural inheritance + bridge fallback was
   NOT deleted.** Build-time edges optimize the common path
   (writeback handles every local-parent method; enrichment handles
   every cross-file-parent method), but tests and tools that build
   a `FileAnalysis` without going through enrichment (hand-crafted
   FAs in unit tests, isolated builders) need *some* path to find
   inherited methods. Restoring the structural walk preserves that
   floor. The walk is now documented as a transitional fallback
   pending the graph-walking pillar; build-time edges remain the
   primary path.

   Cross-file plugin-namespace bridges (#3 of the structural walk —
   `for_each_entity_bridged_to`) also stayed for a different reason:
   pre-emitting `MethodOnClass(bridged_class, helper_name) →
   Edge(...)` for every helper in every other file is N×N in the
   worst case. The lazy `for_each_entity_bridged_to` walk is
   strictly more efficient. The directive's "extend the writeback to
   cover cross-file bridges" is feasible but unnecessary while the
   walk is integrated with the registry (not a parallel system).

2. **`SubReturnReducer`'s `arity_hint.is_some()` short-circuit
   stayed.** D2 was supposed to move every branch_arm witness off
   `Symbol(_)` so the source-tag filter could collapse, but the
   materializer's `Symbol(_) + InferredType` synthetic witnesses
   (one per arm, source preserved) still land on Symbol after edge
   chase — `SymbolReturnArmFold` claims them via its `branch_arm`
   filter. SubReturnReducer's matching exclusion is the floor that
   keeps disagreement signals propagating. Same for the arity_hint
   guard: removing it lets the per-getter-Symbol query at arity=1
   wrong-answer with the getter's stored String instead of routing
   to the writer's per-class arm. The `arity_hint=Some` →
   `MethodOnClass{class, name}` escalation in `query_sub_return_type`
   is what handles the cross-symbol dispatch.

   Net effect: source-tag claims in `SubReturnReducer` collapsed
   from four entries to one (`branch_arm` only). Same for
   `FrameworkAwareTypeFold` and `ExprReturn`. Full collapse waits
   on a follow-up that moves materialized branch_arm witnesses to a
   dedicated attachment shape.

### Validation

After this directive: 507 unit tests + 93 e2e tests green. The
unit count went up by 1 (rewrote
`plugin_override_reducer_yields_when_only_builder_witnesses_present`
to assert the post-D3 behavior; deleted two tests of the now-dead
imported-returns parameter; rewrote
`test_phase5_consumer_side_cross_file_resolves_via_enrichment` to
build a real `ModuleIndex` stub instead of passing
`imported_returns` as a parameter).

Cache `EXTRACT_VERSION` bumped 19 → 20 (bag shape changed; cached
blobs from older builds are re-resolved with priority).

---

## Directive 4 — One canonical store, never two

Every dual-store is a synchronization hazard. The bag is canonical;
everything else is at most a derived index rebuilt from it.

**Action items, ordered by leverage.**

- [x] **Kill `Symbol.return_type`** — absorbed into Directive 1.
      Originally framed here as "lazy cache"; D1's redo deletes the
      field outright. Worklist snapshot needs a parallel update — it
      currently tracks `Symbol.return_type` for fixed-point detection
      and must switch to hashing `Symbol(sid)` registry answers
      directly. That switch lands in D1 with the deletion.

- [x] **Kill `imported_return_types`** — also absorbed into Directive 1.
      Cross-file `MethodOnClass` queries through `BagContext.module_index`
      replace the explicit copy.

- [~] **Kill `FileAnalysis.type_constraints` as a write target.**
      Done as a partial: writes restricted to `push_type_constraint`
      (which mirrors both stores), and the legacy non-helper
      mutations are gone — `propagate_call_bindings_to_constraints`
      is bag-only, `apply_chain_typing_assignments`'s
      "already typed" check reads the bag, `resolve_hash_key_owners`
      builds its type_map from bag witnesses, and the legacy
      `inferred_type` API reads bag Variable+InferredType witnesses
      directly. The vec field stays for serialized blobs and
      debug/dump readers (main.rs `--dump-package`, plugin sandbox
      diff) — those are non-load-bearing and worth the API churn
      only when the next refactor pillar takes them.

- [x] **Kill the walk-time TC-first read** in
      `invocant_type_at_node`'s scalar arm. `push_type_constraint`
      now mirrors TCs into Variable witnesses live during the walk,
      so `bag_query_variable` always sees seed state.

- [x] **Kill `pending_witnesses` staging.** Walk-time idiom emit
      sites push directly to `self.bag`.

- [x] **Kill the FA-side `resolve_invocant_class` bareword arm field
      read.** Routes through `sub_return_type_at_arity(bare, Some(0))`.

- [x] **Kill the Builder's `last_expr_type` map.** Landed in
      Directive 2. Replaced with `last_expr_span: HashMap<ScopeId, Span>`
      whose value is the structural pointer to the implicit-return
      span; the type rides the bag via `Expr(last_expr_span)`. D4
      can still kill `last_expr_span` itself by emitting an
      `Edge(Expr(span))` source `branch_arm` on `Symbol(sub_id)`
      from the walker (so the implicit-return is just one more
      arm), but that requires deciding when to flush "this is the
      last statement we saw" — easiest at sub-body-close, which
      means a small visit hook.

- [x] **Kill `sub_return_delegations` and `self_method_tails` Builder
      maps as inputs to bag emission.** `self_method_tails` and
      `emit_delegation_edges` are gone — the bag-routed
      `Symbol(_) ← branch_arm Edge → Expr(body) → Edge(call_target)`
      chain handles delegation/tail propagation through D2's
      `Edge(Expr(span))` emission. `sub_return_delegations` survives
      as a procedural input to `fixup_call_bound_hash_key_owners`
      only — explicitly NOT a type-inference path. The vestigial
      `return_self_method` Symbol field is gone too (was set only
      from `self_method_tails`; tests + `--dump-package` updated).

- [x] **Collapse the per-arm Edge expansion in
      `publish_return_arm_witnesses` and
      `emit_branch_arm_witnesses_for_ternary`.** D2 left a residual
      duplication of mechanism: when the return body (or the RHS of
      `my $x = $c ? A : B`) is a ternary, the walker emits per-arm
      `branch_arm` Edges *both* on `Expr(ternary_span)` (via
      `emit_expr_witness`) *and* on the consumer attachment
      (`Symbol(sub_id)` / `Variable($x)`). A single
      `Edge(Expr(body_span))` from the consumer would be enough —
      the registry's edge chase calls `BranchArmFold` on `Expr(_)`,
      which already handles per-arm agreement. Both agreement and
      disagreement traces fold to the same answer either way (today
      and post-collapse), so this is purely a cleanup. After the
      change, `publish_return_arm_witnesses` becomes
      `emit_expr_witness(body); push Symbol(sid) ← Edge(Expr(body_span))`
      with no ternary special-case, and
      `emit_branch_arm_witnesses_for_ternary` becomes one Edge from
      `Variable($x)` to `Expr(ternary_span)`.

- [ ] **Drop `SubReturnReducer`'s `arity_hint.is_some()` short-circuit
      and the matching `branch_arm` source-tag exclusion**
      (witnesses.rs:863-902). These are the last source-tag claim
      filters in the registry — D3 collapsed four to one for
      `SubReturnReducer`, but the one stayed because two distinct
      facts still share `Symbol(_)`:

      1. The per-symbol stored return ("if you ask THIS sym, ignoring
         arity, this is the answer").
      2. Per-arm `branch_arm` witnesses materialized onto `Symbol(_)`
         after edge chase (one synthetic `InferredType` per arm,
         source preserved). `SymbolReturnArmFold` claims those for
         agreement / disagreement; `SubReturnReducer` must NOT
         paper-over their `None` with a latest-wins pick.

      The `arity_hint.is_some()` guard is the same compromise on the
      other axis: at `arity=Some`, we want the per-class arm dispatch
      via `MethodOnClass{class, name}`, not a single sym's stored
      return (otherwise Mojo getter-vs-writer wrong-answers — the
      getter sym surfaces String when level(1) was asked).

      **Real fix:** move per-arm branch witnesses off `Symbol(_)` to
      a dedicated attachment shape (`SymbolReturnArm(SymbolId)` or
      reuse `Expr(body_span)` per the per-arm-Edge collapse above),
      and route arity dispatch through `MethodOnClass` exclusively
      so `Symbol(_)` carries only one fact family. After that, both
      the source-tag filter and the arity_hint short-circuit can
      go — `SubReturnReducer::claims` becomes plain
      "Symbol + InferredType, period" and `reduce` becomes
      unconditional latest-wins. **Until this lands the
      "claim by attachment shape, not source tag" rule has one
      documented exception.**

---

## Roadmap — separate design session

- **Arity is bolted on, not first-class.** It has its own
  `ReducerQuery` field, its own `TypeObservation` variant, its own
  reducer, its own re-emittable pass, its own walk-time `ArityBranch`
  enum. The "should just flow" version has return arms carry
  `Vec<Guard>` and the registry filter arms by satisfied guards; arity
  is one guard kind among many. But arity is the only guard kind today
  — generalizing now is premature.
  
  **Schedule a separate design session** when a second guard kind
  appears (type-of-arg dispatch, truthiness gating, `wantarray`
  context). Until then, leave the bolt-on alone — but do NOT ship more
  arity-shaped facts that need their own reducer.

- **Reducer-claim discipline.** D3 landed with the rule almost
  enforced: "a reducer claims by attachment-shape, not by source tag.
  If you need source-tag disambiguation, you're modeling two facts —
  give them two attachments." One documented exception remains
  (`SubReturnReducer`'s `branch_arm` filter + `arity_hint`
  short-circuit) and is tracked in the D4 list above; the rule
  becomes unconditional once that item lands.

- **Post-D4-G: delete `SubReturnReducer` entirely by unifying
  synthesis into the arm-fold protocol.** D4-G's endgame leaves
  `SubReturnReducer` alive as a passthrough — it claims
  `Symbol + InferredType` and returns the writeback's primary
  stored return. That witness only exists because synthesized
  accessors (Mojo `has`, DBIC columns, plugin overrides, framework
  accessor synth) emit a *direct* `InferredType` on `Symbol(sid)`
  instead of going through the arm-fold shape. With no `return X`
  source-level statements, there's no `branch_arm` Edge for
  `SymbolReturnArmFold` to chase — so the direct witness is the
  only record.

  The unification: every synthesis site emits a synthetic
  `Expr(synth_span) + expression: InferredType(t)` plus a
  `Symbol(sid) ← branch_arm Edge → Expr(synth_span)`. Then
  `SymbolReturnArmFold`'s `resolve_return_type([t])` returns `t`
  exactly the same way it handles a real one-arm sub. Plugin
  overrides keep their priority short-circuit, just relocated to
  whichever attachment carries the synth Edge.

  After unification, `Symbol(_)` carries no direct InferredType
  witnesses at all — it's purely a query handle that Edges through
  to the arm-fold attachment. `SubReturnReducer` becomes dead code
  (no claimable witnesses) and gets deleted. The registry shrinks
  to one per-sym fold reducer.

  Cost: ~8–12 synthesis sites switch from "write `resolved_returns`"
  / "push direct `Symbol + InferredType`" to "emit synth Expr + arm
  Edge." Slight verbosity at the emit site, dramatic simplification
  on the read side. Stacks on top of D4-G + `prompt-cleanups.md` #1
  (which kills the `resolved_returns` intermediate but keeps the
  direct-witness shape). The Mojo getter+writer pair on
  `MethodOnClass{class, name}` stays primary-deduped — that part
  is orthogonal.

  **Schedule:** after D4-G ships and the rule is unconditional. The
  unification turns "rule has zero exceptions" into "rule has zero
  exceptions AND the registry has no fallback reducer for Symbol."

---

## Hardening (not principle-driven, but should land alongside)

- [x] `MAX_FOLD_ITERATIONS` is now an all-builds bound: `debug_assert!`
      stays as the dev-time tripwire, plus a release-mode `eprintln!`
      + `break` so a regression keeps the LSP responsive instead of
      spinning forever.

---

## Validation gate

After each directive: `cargo test`, `./run_e2e.sh`. Numbers as of this
plan: 506 unit + 93 e2e green; both must stay green.

Each directive is a single PR — don't ship them piecemeal. The whole
point is that the bag is the only truth at every commit; a partial
migration where two paths coexist is exactly the state we're trying
to leave.
