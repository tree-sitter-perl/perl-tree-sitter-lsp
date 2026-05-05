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

## Directive 2 — All expressions are the same thing

Arms aren't a separate concept. `return $x`, `return Foo->new`,
`return $self->method`, `42 if cond else "x"` — every one of these is
"the value of an expression." The current code special-cases:

- `arm_payload` (builder.rs:3591) dispatches on node kind to decide
  bake-vs-Edge per arm shape. Strings, numbers, anon hashes,
  arithmetic, regexp get baked; variables, method calls, function
  calls get Edges; ternaries are recursed into; everything else
  returns `None`.
- `ReturnArm(Span)` is its own attachment kind, with its own reducer
  (`ReturnArmReturn`).
- The chain typer's `arm_types_per_ri` array threads bag-resolved arm
  types alongside `ReturnInfo` records.
- Variable reads, method-call results, and function-call results each
  publish to a different attachment shape (`Variable`, `Expression(RefIdx)`,
  `NamedSub`).

The fix is one attachment that means "the value of the expression at
this span," with every expression — literal, ref-bearing, or compound —
publishing to it.

**Action items.** Probably needs a short design session before
implementation; flag the open questions inline.

- [ ] **One expression attachment.** Candidate shape: `Expr(Span)` (or
      `Node(NodeId)` if we want to key by AST node identity). Replaces
      both `ReturnArm(Span)` and `Expression(RefIdx)`. Refs that are
      expressions (method calls in rvalue position) keep their RefIdx
      as metadata for non-type lookups (rename, ref_at) but the type
      answer rides `Expr(span)`.

- [ ] **Walker emits `Expr` witnesses uniformly.** Literals push
      `InferredType` (closed under syntax — `42` is `Numeric`). Refs
      push `Edge(Variable{...})` / `Edge(NamedSub)` / `Edge(MethodOnClass(...))`
      to the resolution target. Compound exprs (ternary, do-block,
      block-as-rvalue) push `Edge`s to each component's `Expr(span)`
      and let `BranchArmFold` agree across them.

- [ ] **Kill `arm_payload`.** Its job is "given a node, what witness
      shape?" — but every node in rvalue position needs the same shape
      (an `Expr(span)` witness). The dispatch becomes one walker
      callback that runs on every expression node.

- [ ] **Kill `ReturnArm` and `ReturnArmReturn`.** A `return EXPR`
      becomes `Edge(Expr(expr_span))` on `Symbol(sub_id)` —
      `BranchArmFold` already handles agreement across multiple
      `branch_arm`-source witnesses on a sub.

- [ ] **Kill `last_expr_type`.** Implicit-last-expression-as-return is
      just one more branch_arm-source `Edge(Expr(last_stmt_span))` on
      `Symbol(sub_id)`. Same machinery as explicit returns; no
      side-channel map.

- [ ] **Kill the `arm_types_per_ri` / `returns_by_scope` plumbing.**
      Once arms are `Edge`s, `fold_per_sub_return_arms` is just
      `query(Symbol(sid))` — the registry materializes the edges and
      `BranchArmFold` agrees them.

**Open design questions, decide before starting.**

1. **`Span` vs `NodeId` keying.** Spans are stable across
   serialize/deserialize and align with how `ReturnArm` is keyed today.
   NodeIds are tighter against the AST but die at the cache boundary.
   Recommend `Span`.

2. **Do refs keep `Expression(RefIdx)`?** Rename and "find references"
   need ref-keyed lookup. Easiest: keep `RefIdx` as a non-type
   side-index, route the type answer through `Expr(span)`, and let
   `Expression(RefIdx)` fade to "the ref's metadata," not "the
   expression's type."

3. **Anonymous expressions that aren't refs** (a bare arithmetic
   subexpression nested inside a return). Today they don't have an
   attachment because no consumer asks. After the unification they
   would — but only if something Edges to them. Lazy emission (only
   when needed) is fine; the walker doesn't need to publish for every
   span unconditionally.

---

## Directive 3 — One attachment per fact, no source-tag claims

Multiple reducers claim `Symbol(_)` + `InferredType` today. They
disambiguate by `WitnessSource` tag: `PluginOverrideReducer` claims
priority>10, `BranchArmFold` claims `branch_arm`, `SubReturnReducer`
claims `local_return` / `imported_return` / `delegation` /
`self_method_tail` (the last two added by D1's writeback rewrite),
`FluentArityDispatch` claims a different payload. A new push that
lands on the same attachment with a new source tag silently bypasses
the gate that the original author of the source filter wrote.

This is the "fill in the hack somewhere else" pattern. Kill it by
giving each semantic category its own attachment shape. **D3 also
absorbs two bullets from D1 that did not land as the directive
required.**

**Action items.**

- [ ] **Inheritance via `Edge(MethodOnClass(parent, name))` witnesses.**
      *(Routed from D1.)* Today `query_rec` chases inheritance
      structurally — for a `MethodOnClass{C, m}` query the local bag
      can't answer, the registry consults `ctx.package_parents`,
      `ctx.module_index.parents_cached`, and
      `idx.for_each_entity_bridged_to(class, ...)` and recurses on
      `MethodOnClass{P, m}`. That's a hand-rolled walker over a graph
      that's about to be unified by `prompt-graph-walking.md`. Replace
      it with build-time emission: for each `package_parents[C] = [P1,
      P2, ...]`, push `MethodOnClass(C, m) → Edge(MethodOnClass(P_i, m))`
      witnesses; for each PluginNamespace bridged to `class`, push
      `MethodOnClass(class, name) → Edge(Symbol(entity_id))`
      witnesses (the bridge half already lands in
      `write_back_sub_return_types`'s plugin-bridge pass — extend it
      to also cover cross-file bridges by reading
      `module_index.workspace_namespaces()` or equivalent). Then
      delete the structural fallback in `query_rec` — the registry's
      existing edge-chase machinery handles MRO and bridge walking
      without further code. Cross-file primary lookup
      (`module_index.get_cached(class)`) can stay as a single
      cross-bag recursion or move to a `MethodOnClass(class, _) →
      Edge(... in cached bag)` shape; pick whichever the residual
      `query_rec` code reduces to most cleanly.

- [ ] **Delete `build_imported_return_types`** (backend.rs:134) and
      the imported-return push inside
      `FileAnalysis::enrich_imported_types_with_keys`. *(Routed from
      D1.)* The cross-file `MethodOnClass` query subsumes both —
      `BagContext.module_index` is wired through, so registry queries
      against a bag without local `NamedSub("foo")` witnesses already
      reach the cached module's bag for `foo`'s return type. Keep the
      `HashKeyDef` synthesis half of
      `enrich_imported_types_with_keys` (it's a separate concern —
      injecting synthetic symbols for fat-comma key completion). The
      `imported_returns` HashMap parameter on
      `enrich_imported_types_with_keys` becomes vestigial once the
      function only synthesizes hash keys; either drop the parameter
      or rename the function to reflect its narrower scope.

- [ ] **Drop the dual `Symbol(_)` + `NamedSub(_)` writeback** in
      `write_back_sub_return_types` (builder.rs around the
      `writeback_witnesses` push). Pick one attachment per sub. For
      local subs that's `Symbol(sym_id)`. The registry exposes a
      name→sym_id resolver (built from the symbols table) so
      name-keyed callers (cross-file imports, plugin overrides on
      names that haven't been resolved to ids yet) hit the right id
      without a parallel attachment. `MethodOnClass{class, name}`
      stays — it's the class-keyed shape that earned its keep in D1.

- [ ] **Cross-file imports stop riding `NamedSub`.** Imported subs
      do have a sym_id once enrichment runs
      (`enrich_imported_types_with_keys` synthesizes HashKeyDef
      symbols already; do the same for the sub itself, or extend the
      registry's name-resolver to chase across `module_index`).
      Combines with the bullet above to remove the last `NamedSub`
      consumer.

- [ ] **Kill `WitnessAttachment::NamedSub`.** Once the two callers
      above migrate, the variant goes away.

- [ ] **Kill source-tag claim filters.** `SubReturnReducer::claims`
      currently matches `local_return` / `imported_return` /
      `delegation` / `self_method_tail` (four filters; D1 added the
      latter two). Should match by attachment-shape disjointness
      instead. After arms move to `Expr` attachments (Directive 2),
      branch_arm witnesses no longer land on `Symbol(_)` — the
      source-tag exclusion in `FrameworkAwareTypeFold::claims` and
      `SubReturnReducer::claims` becomes unnecessary. Delegation /
      self-method-tail edges already ride `Edge(...)` payloads; the
      tag-filter on the `InferredType` payload exists only because
      writeback also pushes plain `InferredType` on the same
      attachment under those tags — and that goes away when the
      writeback collapses to one attachment per sub (bullet above).

- [ ] **Kill `SubReturnReducer`'s `if q.arity_hint.is_some()`
      short-circuit** (witnesses.rs around line 875). With distinct
      attachments for "stored return" vs "guarded arm," the
      registry's first-match dispatch handles ordering naturally —
      no reducer needs to know another reducer exists.

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

- [ ] **Kill `FileAnalysis.type_constraints` as a write target.** It
      can survive as a derived projection over Variable witnesses
      built post-finalize for legacy consumers, OR die outright if
      nothing reads it that can't read the bag instead. Audit
      `push_type_constraint` callers to decide.

- [ ] **Kill the walk-time TC-first read** in
      `invocant_type_at_node`'s scalar arm (builder.rs:1358-1362). The
      reason it exists is "walk-time bag isn't populated yet, TCs
      are." Fix by populating the bag live during the walk (push
      Variable witnesses as TCs are seeded, not in one shot at
      `populate_witness_bag`). Then the bag is canonical at every
      phase, walk-time included.

- [ ] **Kill `pending_witnesses` staging.** With live walk-time bag
      population, the staging vec is dead — push directly to
      `self.bag` from the walk emit sites.

- [ ] **Kill the FA-side `resolve_invocant_class` bareword arm field
      read** (file_analysis.rs:3768-3775). Already a known followup;
      route through `sub_return_type_at_arity(bare, Some(0))`. Becomes
      trivial after Directive 1 lands the unified attachment shape.

- [ ] **Kill the Builder's `last_expr_type` map.** Already listed
      under Directive 2 — repeated here because it's a dual-store, not
      just an arm-special-case.

- [ ] **Kill `sub_return_delegations` and `self_method_tails` Builder
      maps** as inputs to bag emission. After Directive 2, each return
      is just an `Edge(Expr(span))` — the walker emits the edge
      directly, no bookkeeping map. `fixup_call_bound_hash_key_owners`
      (the only other reader) walks delegation chains for HashKeyOwner
      resolution, which is a different question — that piece either
      gets its own bag-routed answer (a `HashKeyOwnerEdge`?) or stays
      procedural and explicitly NOT a type-inference path.

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

- **Reducer-claim discipline.** After Directive 3 lands, write down
  the rule: "a reducer claims by attachment-shape, not by source tag.
  If you need source-tag disambiguation, you're modeling two facts —
  give them two attachments."

---

## Hardening (not principle-driven, but should land alongside)

- [ ] `MAX_FOLD_ITERATIONS` is `debug_assert!`-only — release builds
      have no cap, just rely on the lattice argument. Add a real
      release-mode bound with a `tracing::error!` and break, or trust
      the lattice and remove the debug-only check entirely. Pick one.

---

## Validation gate

After each directive: `cargo test`, `./run_e2e.sh`. Numbers as of this
plan: 506 unit + 93 e2e green; both must stay green.

Each directive is a single PR — don't ship them piecemeal. The whole
point is that the bag is the only truth at every commit; a partial
migration where two paths coexist is exactly the state we're trying
to leave.
