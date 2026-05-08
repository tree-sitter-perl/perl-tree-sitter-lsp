# Cleanups — small, independent prompts

Each section is one self-contained prompt. They don't depend on each
other (unless explicitly noted) — pick whichever hurts most, ship it,
move on. Land each as its own PR.

These are not part of the bag-residual directive arc — they're the
small parallel-store / dead-state / cosmetic items that surface
alongside larger refactors and would otherwise rot the codebase one
half-removed-construct at a time.

---

## 1. Delete `Builder.resolved_returns` map

**Where:** `src/builder.rs` — `Builder` struct field
`resolved_returns: HashMap<SymbolId, InferredType>` and every write
site (`record_framework_accessor_witness`,
`Method`/`Symbol` plugin emit handlers,
`fold_per_sub_return_arms`,
`seed_plugin_overrides_into_return_types`) plus the read site in
`write_back_sub_return_types` and the `fold_state_snapshot` snapshot.

**Why:** D1 left this as build-only bookkeeping but it's a parallel
store of the same data the bag carries. The original directive said
construction sites should "drop the field, push a `Symbol(sid) →
InferredType(t)` witness in its place" — direct emit, no
intermediate. Right now writeback iterates the map and re-publishes,
which means we maintain bag-and-map in sync inside the worklist.

**How to apply:** at every site that currently writes
`resolved_returns.insert(sid, rt)`, push the bag witness directly
(`Symbol(sid)` plain `InferredType`; plus the `MethodOnClass{class,
name}` primary-slot mirror — see "Watch out for" #2 for the
primary-dedup question that blocked this in D3). Writeback's job
becomes "ensure the canonical witnesses are present" rather than
"iterate the map." `fold_state_snapshot`'s first tuple element
switches from "reading the map" to "querying `Symbol(sid)` via the
registry" — the worklist's fixed-point detection then watches what
consumers actually see, which is the snapshot's intent.

**Watch out for:**

1. The worklist re-runs each iteration. The bag already has
   `remove_by_source_tag` for re-emittable passes — use it before
   pushing the writeback witnesses on every iteration so we don't
   accumulate stale `Symbol(sid)` entries from earlier rounds.

2. `MethodOnClass` primary-dedup is what kept `resolved_returns` alive
   through D3. The current writeback walks symbols in declaration
   order and the FIRST `(class, name)` pair wins the primary slot.
   Walk-time direct emit needs an equivalent rule. Two viable shapes:
   (a) push at synthesis but stamp the witness with a sequence number
   the reducer reads to pick first-seen, or (b) accept latest-wins on
   `MethodOnClass` and ensure the primary's witness is published last
   by ordering the synthesis sites. (b) is simpler if it works for the
   Mojo getter/writer test (`builder_tests.rs::test_mojo_arity_resolution`)
   — verify before committing to it.

---

## 2. Delete `SymbolDetail::Sub.return_self_method` field

**Where:** `src/file_analysis.rs:307` (declaration) plus every
`SymbolDetail::Sub { return_self_method: ... }` literal in
`src/builder.rs` and the writeback assignment at builder.rs:6961.
Also the `--dump-package` JSON output in `src/main.rs:868` (just dumps
it for introspection) and the test dumps in `src/symbols_tests.rs`.

**Why:** Was set by the writeback for subs that tail-called
`shift->M(...)` so the FA-side `self_method_tail` chase could chain
through them. D1 deleted the chase; nothing reads
`return_self_method` for type resolution any more. The remaining
reads are:
- `Builder::fold_state_snapshot` — fixed-point detection. Now
  redundant with `resolved_returns` (or, after cleanup #1, the bag
  `Symbol(sid)` query).
- `--dump-package` JSON — pure introspection.
- `src/symbols_tests.rs` — test assertions on the dump format.

**How to apply:** delete the field. Replace `--dump-package` JSON
output with a query to the bag for `Edge(MethodOnClass{...})`
payloads on the sub's `Symbol(sid)` attachment — that's where the
tail-call edge actually lives now (post-D3, name-keyed sub edges
were collapsed into the class-keyed `MethodOnClass` shape). Tests
that assert on the field move to asserting on the edge witness in
the bag.

**Watch out for:** `fold_state_snapshot` currently includes
`return_self_method` as the third tuple element. Drop it from the
tuple after confirming the snapshot's fixed-point detection still
converges (the bag-side answers should be enough — `resolved_returns`
already captures what `return_self_method` was hinting at).

---

## 3. Delete `#[allow(dead_code)]` on `SubInfo` accessors

**Where:** `src/module_index.rs` — the `overloads` field on `SubInfo`
plus the methods `param_counts`, `return_type_for_arity`,
`primary_id`, `id_for_arity`. Six `#[allow(dead_code)]` were added in
D1.

**Why:** these accessors were called only by
`find_method_return_type_raw`'s arity-aware cross-file dispatch,
which D1 deleted. With no remaining callers, they're dead code muffled
under `allow(dead_code)` — the user's stated preference (per
`feedback_global_fixes.md`) is to delete unused code, not silence the
warning.

**How to apply:** delete the field and the four methods. `cargo
check --tests` should stay clean — verify nothing else compiles
against them. If a future cross-file caller needs them, the bag-
routed `symbol_return_type_via_bag(sid, Some(arity))` is the path to
take, not these accessors.

**Watch out for:** `SubInfo::return_type` (the no-arity version) is
still load-bearing — it's read from `complete_import_list`,
`imported_function_completions`, `format_imported_signature`, etc.
Don't conflate it with the arity variant.

---

## 4. Tidy `BagContext { ... };` indentation drift

**Where:** `src/file_analysis.rs` lines around 1473 / 1513 / 2054 and
`src/builder.rs` lines around 6355 / 6403 / 6434 / 6566. The
`module_index: None, package_parents: &self.package_parents,`
additions are followed by a closing brace with extra indentation:

```rust
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
            };  // <- extra indent
```

**Why:** purely cosmetic, but `rustfmt` will move on it eventually
and the diff churn is easier to land while these sites are fresh.

**How to apply:** run `cargo fmt` and inspect the diff to ensure no
unrelated changes; commit just the indentation fix.

---

## 5. Replace `fold_state_snapshot` ad-hoc tuple with bag generation counter

**Where:** `src/builder.rs:6075-6103` — the hand-coded
`(answers, type_constraints.len(), invocant_filled)` tuple that
`fold_to_fixed_point` uses to detect convergence.

**Why:** the snapshot is a maintainability hazard. Every re-emittable
pass that produces an observable signal needs a manual entry in the
tuple, or the worklist terminates early. D3 had to add the
`invocant_filled` term specifically because
`emit_method_call_return_edges` produced no other change visible to
the snapshot — an iteration that only filled new `invocant_class`
slots would otherwise return the same `(answers, tc_len)` and the
loop would exit before chain typing finished propagating. This is the
kind of trap that bites later, after the person who knew about it
moves on.

**How to apply:** replace the tuple with a single monotonically
increasing `bag_generation: u64` on `WitnessBag`, bumped by every
`push` and `remove_by_source_tag`. `fold_state_snapshot` returns
`(generation, type_constraints.len())` and convergence is
"two consecutive iterations with the same pair." Any future
re-emittable pass automatically participates because every bag write
bumps the counter; no per-pass bookkeeping. The existing comment at
`fold_state_snapshot` documenting the "what registers as movement?"
trap can then be deleted — the structure prevents the trap.

**Watch out for:** `populate_witness_bag` runs once before the loop,
so the loop's first iteration already sees the seeded generation.
`apply_type_overrides` runs even earlier — same. The fixed point
detector compares only across loop iterations, so initial pushes
don't matter. But re-emittable passes that `remove_by_source_tag`
before re-pushing will bump the generation TWICE per iteration
(remove + push); that's still fine — it just means the snapshot
strictly grows when work happens. The convergence rule is unchanged.

---

## 6. Unify name resolution via a name-keyed witness attachment

**Where:** `src/builder.rs` — `expr_payload`'s
`function_call_expression` / `ambiguous_function_call_expression` /
`bareword` / `scoped_identifier` arm,
`Builder::find_callee_symbol`, `Builder::forward_callee_name`,
`Builder::resolve_forward_call_targets`, and the
`unresolved_call_targets: Vec<UnresolvedCallTarget>` field on
`Builder`. Witness attachment enum in `src/witnesses.rs`.

**Why:** the forward-reference fix (PR #32) is a two-phase
recovery: walk-time `find_callee_symbol` for the fast path, plus a
queue + post-walk retry for misses. The structure works but
*duplicates the lookup rule* (`s.kind ∈ {Sub, Method}` and
`s.name == bare`) across two sites. If the rule ever has to grow —
package-aware lookup, plugin-synthesized callees, private/anonymous
exclusions — both sites have to change in lockstep, and the
`find_callee_symbol` helper hides but doesn't eliminate the
duplication (the retry path still has to know *when* to call it).

The principled shape is to push the lookup off the walk entirely:
emit a name-keyed witness at walk time (no symbol-table consult), and
let a single post-walk reducer or pass resolve all such witnesses
against the final symbol table.

**How to apply:** add a `WitnessAttachment::CalleeByName { name:
String }` variant (or a `WitnessPayload::CalleeByName` payload —
attachment-vs-payload depends on whether other reducers want to
chase into "the sub named X" abstractly, or only at the Expr-arm
level). `expr_payload`'s call arm becomes:

```rust
"function_call_expression"
| "ambiguous_function_call_expression"
| "bareword"
| "scoped_identifier" => Some(WitnessPayload::Edge(
    WitnessAttachment::CalleeByName {
        name: self.forward_callee_name(node)?,
    },
)),
```

A single post-walk pass (`resolve_callee_by_name_witnesses`, replacing
`resolve_forward_call_targets`) walks the bag for
`Edge(CalleeByName{..})` payloads and rewrites each to
`Edge(Symbol(sid))` once `find_callee_symbol` resolves — or drops
the rewrite if it doesn't (still monotone; consumers see no Edge,
which is the same as today's "name didn't resolve" semantics).

After this lands, `unresolved_call_targets`, `forward_callee_name`'s
.to_string() owned-name return, and the post-walk recovery queue all
collapse — the attachment IS the queue. `find_callee_symbol` survives
as the single lookup helper, called only from the resolver pass.

**Watch out for:**

1. **Bag generation.** Pushing a name-keyed attachment that gets
   *rewritten* in place is a bag mutation, which violates monotonicity.
   Two viable shapes: (a) the resolver pass pushes a *new*
   `Edge(Symbol(sid))` witness alongside the original (the
   `CalleeByName` one becomes a non-resolving dead end the registry
   ignores once a `Symbol`-keyed sibling exists) — strictly monotone,
   matches today's pattern; or (b) the registry resolves
   `CalleeByName` lazily at query time via a reducer that does the
   lookup itself. (b) is simpler in code but pushes I/O-shaped work
   into the query path, which the rest of the registry takes pains to
   avoid. (a) is the right shape for this repo.

2. **Cross-file imports.** Today, `find_callee_symbol` returns `None`
   for cross-file imports (the local symbol table doesn't carry
   imported subs as own-Symbol entries). The `CalleeByName` resolver
   should preserve that — the chain-receiver path through enrichment
   covers cross-file return types via `MethodOnClass` / `NamedSub`
   reducers, not via the `Expr(span)` Edge. Verify by removing the
   recovery queue and confirming the cross-file enrichment tests
   (`test_cross_file_*` in `builder_tests.rs`) still pass.

3. **Span equality of synthetic witnesses.** The current path uses the
   call's `expr_span` for both the Expr attachment and the Witness
   span; the rewrite must keep that exact span so
   `FrameworkAwareTypeFold`'s point-contains filter behaves
   identically. Hand-test against `forward_reference_in_ternary_arms_resolves`
   (the trickiest of the four sibling tests — recursion into ternary arms
   means the same span is referenced from multiple Edge witnesses).

4. **Cache version.** Adding a `WitnessAttachment` variant changes the
   bincode shape. Bump `EXTRACT_VERSION` in `module_cache.rs` so
   stale cache blobs are re-resolved.

**Shipping check:** the four `forward_reference_*` sibling tests in
`builder_tests.rs` should pass byte-for-byte (same return types) before
and after this refactor — that's the regression bar. The diff should
be net-negative LOC: the `unresolved_call_targets` field, the
`UnresolvedCallTarget` struct, and the pre-walk-vs-post-walk split in
`emit_expr_witness` all collapse.

---

## Notes on what's NOT in this list

- **Two D1 bullets that didn't land as written** (inheritance via
  edge witnesses; `build_imported_return_types` deletion) are in
  `working-bag-residual.md` D3, not here. They're directive-scoped,
  not stylistic.

- **`MAX_FOLD_ITERATIONS` debug-only check vs. release-mode bound** is
  in `working-bag-residual.md`'s "Hardening" section. Ditto.

- **Cache version bumps** are not parked here either — they go with
  the structural change that requires them. The D1 commit
  (`dc4315f`) bumped `EXTRACT_VERSION` to 18 alongside the field
  deletions; future shape changes follow the same rule.
