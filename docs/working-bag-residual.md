# Bag-residual action plan

The principle: **the bag is the only truth.** Every dual-store, every
syntactic bake, every "the field is canonical at this phase" caveat is
a hack we have to remember. They all go.

What's left is structured around four directives. Items are grouped
under the directive they serve so the cleanup never feels like
unrelated chores.

---

## Directive 1 — Delete `Symbol.return_type` and the FA-side method-typing

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

- [ ] **Delete `SymbolDetail::Sub.return_type` field.** This is the
      forcing function for the rest. Pre-measured blast radius:
      ~55 `cargo check --tests` errors across 14 files. Distribution:
      builder.rs ~17 (construction sites — drop the field, push a
      `Symbol(sid) → InferredType(t)` witness in its place),
      builder_tests.rs ~17 (test fixtures — drop the field from
      literals; tests asserting on the field route through bag query
      methods), file_analysis.rs ~5 (internal field-fallback reads
      inside bag-routed query methods — these are exactly the code
      paths to delete, not patch), backend.rs ~2 (the
      `build_imported_return_types` machinery — see next bullet),
      and ~14 elsewhere across symbols.rs, main.rs, plugin/cli.rs,
      module_resolver.rs, module_index.rs, witnesses.rs, tests.

- [ ] **Delete `imported_return_types`** (backend.rs's
      `build_imported_return_types` plus the FA field it populates).
      The cross-file `MethodOnClass` query subsumes it — Symbol(sid)
      witnesses on a cached module's bag are reachable via the
      registry once `BagContext` carries `&ModuleIndex`. The field
      deletion above will surface this code path as compile errors
      that should be deleted, not patched.

- [ ] **Delete `find_method_return_type_seen`,
      `find_method_return_type_raw`, and `self_method_tail`.** The
      sole public entry point `find_method_return_type` becomes a
      thin wrapper that routes through `MethodOnClass(class, name)`
      with the caller's `arity_hint`. No procedural ancestor walk;
      no procedural overload picking. The registry's
      `Edge(MethodOnClass(parent, name))` chase handles MRO; arity
      is selected by the existing `FluentArityDispatch` reducer
      claiming `Symbol(_)`.

- [ ] **Delete `fill_returns_via_bag_chase`'s
      "delegators-and-tails-only" guard** (currently
      builder.rs:6738-6756). Once the field is gone, the chase has
      no consumer and the function itself is deletable.
      `sub_return_delegations` and `self_method_tails` Builder maps
      may stay as walk-time bookkeeping for the bag-emit path
      (they're inputs, not outputs), but their use as inputs to a
      build-time chase ends here.

- [ ] **Add `WitnessAttachment::MethodOnClass { class, name }`.**
      Stable shape, owned strings (cache blob serializes via
      bincode+zstd; `Arc<str>` is fine if you want the
      space saving, but `String` matches every other attachment
      payload and keeps the diff smaller).

- [ ] **Emit `MethodOnClass(C, m) → Edge(MethodOnClass(P, m))`
      witnesses for inheritance.** For each
      `package_parents[C] = [P1, P2, ...]`, for each method `m`
      defined locally on `C` or accessible on `P_i`, emit the edge.
      Cross-file bridges (`for_each_entity_bridged_to` consumers) and
      cross-file primary cached modules become the same edge shape —
      the registry walks them through `BagContext.module_index`. No
      callback walkers in the registry's materialize.

- [ ] **Extend `BagContext` to carry `Option<&ModuleIndex>`.** The
      registry uses it to chase edges into cached modules' bags
      during `MethodOnClass` resolution. In-file callers
      (build-time, isolated tests) pass `None`.

- [ ] **Fix `for_each_ancestor_class`'s DFS to left-to-right
      `@ISA` order.** Currently it pushes parents left-to-right then
      `stack.pop()`s, traversing in reverse `@ISA` order — wrong for
      Perl's default DFS-MRO. Add a regression test:
      `@ISA = (A, B)` where both `A::m` and `B::m` exist with
      different return types; `find_method_return_type(C, "m", _, _)`
      must return `A`'s.

- [ ] **Keep this test green:**
      `method_on_class_disambiguates_same_name_across_classes`
      (builder_tests.rs, lifted from the abandoned attempt). It's
      the structural pin: same-named methods on unrelated classes
      resolve to their own per-class types at every arity.

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
