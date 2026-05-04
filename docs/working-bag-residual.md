# Bag-residual action plan

The principle: **the bag is the only truth.** Every dual-store, every
syntactic bake, every "the field is canonical at this phase" caveat is
a hack we have to remember. They all go.

What's left is structured around four directives. Items are grouped
under the directive they serve so the cleanup never feels like
unrelated chores.

---

## Directive 1 — Methods belong in the bag

Today `find_method_return_type_seen` (file_analysis.rs:2103) chases
`return_self_method` recursively at FA query time, calling
`resolve_method_in_ancestors` to thread class context through the
inheritance chain. The bag's `Edge(NamedSub(b))` for the same shape is
name-keyed only — it can't represent "method `b` on class `C`."

Result: cross-class dispatch has its own typer. The bag answers some
questions and the FA-side recursion answers the rest. The seam is
maintained by hand (`fill_returns_via_bag_chase` is "restricted to
delegators/tails" precisely to avoid colliding with what the FA-side
will answer differently).

**Action items.** Treat as one design pass; do not split.

- [ ] **Add `MethodOnClass { class, name }` attachment.** A reserved
      `ReturnOfKey::MethodOnClass` payload already exists (`witnesses.rs:18-21`,
      held under `#[allow(dead_code)]`) — the design saw this coming.
      Pick the final shape now (struct vs tuple, `Arc<str>` vs `String`)
      and commit.

- [ ] **Inheritance edge emitter.** For each `package_parents[C] = [P1, P2]`
      and each method `m` resolvable on `P1`, push
      `MethodOnClass(C, m) → Edge(MethodOnClass(P1, m))`. The registry's
      cycle-guarded materialize gives MRO walking for free. Emitter
      lives in builder.rs (rule #1 stays intact — it consumes
      `package_parents`, doesn't walk the tree).

- [ ] **Cross-file bridges become edges, not callback walks.**
      `for_each_entity_bridged_to` and the plugin-namespace dispatch
      walk both fold into the same Edge(MethodOnClass(...)) shape.
      `BagContext` extends to carry `&ModuleIndex` so cross-file edges
      materialize through the registry just like in-file ones.

- [ ] **Replace `find_method_return_type_seen`** with
      `MethodOnClass(C, m)` queries. Drop `find_method_return_type_raw`'s
      ancestor walk + procedural overload picking — the registry handles
      both via edges. Drop `self_method_tail` (the FA helper, not the
      Builder map) entirely.

- [ ] **Drop the "restricted to delegators/tails" guard** in
      `fill_returns_via_bag_chase` (builder.rs:6736). With class-aware
      edges, `NamedSubReturn`'s name conflation problem goes away — the
      bag can ask `MethodOnClass(C, m)` directly and never confuse a
      Mojo `level` getter with its writer pair.

**Why one design pass.** The attachment shape, the edge emitter, the
`BagContext` extension, and the `find_method_return_type_seen` rewrite
are interlocked. Doing them piecemeal means living with two code paths
mid-refactor and another "remember to migrate" hack.

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
claims `local_return` / `imported_return`, `FluentArityDispatch`
claims a different payload. A new push that lands on the same
attachment with a new source tag silently bypasses the gate that the
original author of the source filter wrote.

This is the "fill in the hack somewhere else" pattern. Kill it by
giving each semantic category its own attachment shape.

**Action items.**

- [ ] **Drop the dual `Symbol(_)` + `NamedSub(_)` writeback** in
      `write_back_sub_return_types` (builder.rs:6917). Pick one
      attachment per sub. For local subs that's `Symbol(sym_id)`. The
      registry exposes a name→sym_id resolver (built from the symbols
      table) so name-keyed callers (cross-file imports, plugin
      overrides on names that haven't been resolved to ids yet) hit
      the right id without a parallel attachment.

- [ ] **Cross-file imports stop riding `NamedSub`.** Imported subs do
      have a sym_id once enrichment runs (`enrich_imported_types_with_keys`
      synthesizes HashKeyDef symbols already; do the same for the sub
      itself, or extend the registry's name-resolver to chase across
      `module_index`).

- [ ] **Kill `WitnessAttachment::NamedSub`.** Once the two callers
      above migrate, the variant goes away.

- [ ] **Kill source-tag claim filters.** `SubReturnReducer::claims`
      should match by attachment-shape disjointness. After arms move
      to `Expr` attachments (Directive 2), branch_arm witnesses no
      longer land on `Symbol(_)` — the source-tag exclusion in
      `FrameworkAwareTypeFold::claims` and `SubReturnReducer::claims`
      becomes unnecessary.

- [ ] **Kill `SubReturnReducer`'s `if q.arity_hint.is_some()`
      short-circuit** (witnesses.rs:875). With distinct attachments
      for "stored return" vs "guarded arm," the registry's first-match
      dispatch handles ordering naturally — no reducer needs to know
      another reducer exists.

---

## Directive 4 — One canonical store, never two

Every dual-store is a synchronization hazard. The bag is canonical;
everything else is at most a derived index rebuilt from it.

**Action items, ordered by leverage.**

- [ ] **Kill `Symbol.return_type` as load-bearing state.** Today
      `write_back_sub_return_types` sets it inside the worklist, and
      the worklist's snapshot tracks it for fixed-point detection.
      Make it a pure cache filled lazily by reading the bag at first
      access (or at FA construction time, post-finalize). The worklist
      snapshot tracks bag content directly — every Symbol(sid) answer
      from the registry, hashed.

- [ ] **Kill `imported_return_types`** (file_analysis.rs:1668). Bag's
      sub-return witnesses (whatever attachment shape we settle on
      after Directive 3) are the one source.

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
