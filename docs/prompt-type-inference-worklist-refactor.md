# Type Inference Pipeline → Worklist Refactor

> **Status: landed (April 2026).** All seven phases shipped on
> `refactor/type-inference-worklist`; `Builder::fold_to_fixed_point` is
> the live driver, the 13-step pipeline collapsed to ~9 phases (see
> `CLAUDE.md` "Build pipeline phases" and "Worklist invariants"), and
> `docs/prompt-type-inference-spec.md`'s status snapshot reflects the
> new shape. This document is preserved as the design rationale and
> phasing record; the references below to "the 13-step pipeline being
> collapsed", "fold pass 1 / fold pass 2", etc. are historical context
> for how the system looked before the refactor, not a description of
> current behavior.
>
> Collapse the manually-scheduled phases 4–10 of `build_with_plugins()` into a
> monotone fixed-point worklist over the witness bag. Test contract is the
> hard constraint: every existing test stays green at every commit on the
> branch.
>
> Cross-refs:
> - `docs/prompt-type-inference-spec.md` — the witness/reducer model this builds on.
> - `CLAUDE.md` "Build pipeline phases" + "Worklist invariants" — the
>   landed pipeline + reducer-extension contract.

## Why

Architectural review (April 2026) of the 13-phase pipeline surfaced
specific smells:

1. **Step 6 and step 9 are the same function.** `resolve_return_types` runs
   twice. The reason in the spec is candid: "the second run closes the
   chicken-and-egg between sub-return resolution and chain-variable typing."
   That is a fixed-point iteration with the iteration count hardcoded to 2.
   No termination argument; "two passes happens to suffice on Mojo" is the
   only justification. Deeper chains (`my $a=…; my $b=$a->m; my $c=$b->m;
   my $d=$c->m` where each rhs needs the previous sub's return folded) silently
   degrade — they don't fail tests, they just stop completing.

2. **Three post-walk CST walks doing the same shape of work.**
   `type_assignments_into_bag` (step 7), `refresh_return_arm_types` (step 8),
   `resolve_invocant_classes_post_pass` (step 10) each index nodes of one kind
   by span, then re-run `resolve_invocant_class_tree` against the bag. Three
   separate indexing passes for one conceptual operation: "for spans whose
   type wasn't known at walk time, type them now."

3. **Plugin overrides are a special case in fold, not a witness.**
   `apply_type_overrides` (step 4) writes `Symbol.return_type` directly. Step 6
   has a "skip subs flagged `PluginOverride`" branch so the override isn't
   clobbered. The override is leaking out of the bag and forcing the fold to
   special-case it, instead of being a high-priority witness the reducer
   naturally prefers.

4. **`resolve_return_types` is doing five jobs in 274 lines.** Arm fold,
   ArityReturn emission, delegation propagation, self-method-tail
   propagation, call-binding mirror, plugin-override skip. Each is a
   reducer's-worth of concern.

5. **The spec ships a witness/reducer architecture with worklist semantics
   in mind, but the implementation drives it via 13 hand-ordered phases.** The
   bag is the right structure; the scheduling around it is incremental
   sediment.

The witness bag, `Builder::resolve_invocant_class_tree`, provenance, and
`push_type_constraint` sync are all sound and stay. The refactor replaces
the **outer driver** only.

## Test contract — the hard rule

This is a TDD-shaped refactor. **Existing tests are the specification of
correct behavior.** They do not move and do not change. Every commit on
this branch is `cargo test` + `./run_e2e.sh` green.

Public API in scope (must stay stable):

- `builder::build(tree, source) -> FileAnalysis`
- `builder::build_with_plugins(tree, source, plugins) -> FileAnalysis`
- `FileAnalysis::inferred_type_via_bag(var, point) -> Option<InferredType>` (line 1404)
- `FileAnalysis::method_call_return_type_via_bag(ref_idx) -> Option<InferredType>` (line 1425)
- `FileAnalysis::sub_return_type_at_arity(name, arity) -> Option<InferredType>` (line 1482)
- `FileAnalysis::mutated_keys_on_class(class) -> Vec<String>` (line 1500)
- `FileAnalysis::return_type_provenance(sym_id) -> TypeProvenance` (line 1552)
- `FileAnalysis::resolve_expression_type(...)` (cursor_context / hover use it)
- The `--dump-package` CLI output shape (provenance test surface).

The CST-walk plus query-after-build pipeline ordering must survive: tests
that build a tree, run `build()`, then query `inferred_type_via_bag` /
`sub_return_type_at_arity` / `mutated_keys_on_class` / `--dump-package`
must produce **byte-identical** answers throughout the refactor.

Tests that touch internals (raw `witnesses`, `type_constraints`,
`sub_return_delegations`, `self_method_tails`) are coupled to fields we
might restructure. **Those tests do not change** — if a refactor would
require updating their assertions, we instead route the new structure
through a compatibility view that preserves the field shape they read.
The motto: change internals around stable read sites, never the other
way around.

## Architecture target

Replace phases 4–9 with one monotone fixed-point fold:

```
                ┌──────────────────────────────────┐
                │  Witness Bag (live from walk)    │
                │  attachment-indexed              │
                │  source-priority preserved       │
                └──────────────────┬───────────────┘
                                   │
                ┌──────────────────▼───────────────┐
                │  ReducerRegistry (stateless)     │
                │  + PluginOverrideReducer (new)   │
                │  + FrameworkAwareTypeFold        │
                │  + BranchArmFold                 │
                │  + FluentArityDispatch           │
                │  + NamedSubReturn                │
                │  + DelegationReducer (new, ext.) │
                │  + SelfMethodTailReducer (new)   │
                │  + ChainTypingReducer (new)      │
                └──────────────────┬───────────────┘
                                   │
                ┌──────────────────▼───────────────┐
                │  Worklist Driver                 │
                │  enqueue dirty attachments       │
                │  pop → run reducers → if changed │
                │  enqueue dependents              │
                │  terminate when empty            │
                └──────────────────────────────────┘
```

**Termination:** witnesses are monotonically appended; reduced answers
move only `None → Some(T)` or refine within a finite lattice
(`InferredType` is a flat enum, ~12 variants). The lattice is finite, so
the worklist terminates. We assert this with a convergence test (Step 1).

**Pipeline steps 1–3, 11–13 do not move.** (Numbering here refers to the
original 13-step pipeline in CLAUDE.md, not the refactor phases below.)
They are doing genuinely different things (ref resolution, hash-key
linker, POD attach, FA construction, finalize). Only the manually-ordered
middle (steps 4–10) collapses.

**`Builder::resolve_invocant_class_tree` does not move.** It becomes the
body of `ChainTypingReducer`; identical behavior, different caller.
Tests that currently pass through it still pass through it.

## Phased plan (TDD ordering)

Each phase is a separate commit. Each commit ends in `cargo test` green
(plus `./run_e2e.sh` for phases that touch query paths). Push **none**;
`commit straight to main` was the user's instruction for the spec, but
the implementation work happens on a branch and merges normally.

### Phase 0 — Lift tests to sibling modules

**Goal:** make the codebase navigable before the refactor lands.

`builder.rs` is 13k lines; `file_analysis.rs` is 6.8k lines with ~1300
lines of inline tests starting around line 5556. Both are past the
threshold where inline `mod tests` blocks bury the production code. The
idiomatic Rust move is `#[cfg(test)] mod tests;` declaring a sibling
file that still gets `super::*` access — what tokio/hyper/serde do.

Steps (one commit per file, fully mechanical):

1. For each `src/<file>.rs` containing a non-trivial `mod tests`:
   - Cut the entire `#[cfg(test)] mod tests { ... }` body.
   - Replace with:
     ```
     #[cfg(test)]
     #[path = "<file>_tests.rs"]
     mod tests;
     ```
     The `#[path]` attribute is required because `src/<file>.rs` is a flat
     module file (not `src/<file>/mod.rs`), so the default lookup
     (`src/<file>/tests.rs`) doesn't match the sibling layout we want.
   - Paste the body into a new `src/<file>_tests.rs`. Strip the outer
     `#[cfg(test)] mod tests {` / closing `}`. **Do not dedent by hand**
     — a blanket `sed 's/^    //'` corrupts lines inside multi-line raw
     string literals (`r#"..."#`), shifting columns inside test fixtures
     and breaking position-sensitive assertions (e.g. `Position { line:
     5, character: 25 }`). Instead, lift the body verbatim (still
     indented +4 from the original `mod tests {` block), then run
     `rustfmt --edition 2021 src/*_tests.rs` to dedent to column 0.
     Rustfmt is raw-string-aware and leaves their contents alone.
   - **Do not run `cargo fmt`** during the lift commit. `cargo fmt`
     formats the whole crate regardless of arguments after `--`, which
     sweeps unrelated production files and buries the test-lift diff
     under a huge formatting churn. Use `rustfmt <file>` directly to
     scope the format strictly to lifted files. The `use super::*;`
     and all inner `use` lines come along verbatim.
2. Files in scope:
   - `src/file_analysis.rs` — biggest payoff.
   - `src/builder.rs` — biggest payoff.
   - `src/witnesses.rs`, `src/symbols.rs`, `src/cursor_context.rs`,
     `src/module_index.rs`, `src/cpanfile.rs`, `src/pod.rs`,
     `src/file_store.rs` — any file with > ~100 lines of tests.
   - Skip files where the test block is < 50 lines; the lift isn't
     worth the churn.
3. After each file lift: `cargo test` green. No assertion changes, no
   helper-function moves, no reordering. If a test references a `super`
   item not previously exposed to its `mod tests` (rare — sibling files
   re-derive `super` exactly the same way), copy the `pub(super)`
   declaration along with it.
4. Verify with `cargo test 2>&1 | grep "test result" | sort | uniq -c` —
   pass count must be identical to pre-lift baseline.

Commit per file: `chore: lift <file>.rs tests into sibling module`
(or one combined commit if all lifts are clean — separate commits are
easier to bisect if something breaks).

**Acceptance:** `cargo test` green; pass count unchanged; every
production source file ≤ ~80% of its pre-lift size for the heavy ones;
test code lives in `*_tests.rs` siblings.

### Phase 1 — Lock the test surface

**Goal:** know exactly what the test suite asserts before touching the
pipeline.

Steps:

1. `cargo test 2>&1 | tee /tmp/baseline.txt` — capture pass count + names.
   Expectation: 488+ tests pass.
2. `./run_e2e.sh 2>&1 | tee /tmp/baseline-e2e.txt` — capture e2e pass.
3. Audit which tests reach into internals vs query through the public API:
   ```
   grep -rn "fa\.\(witnesses\|type_constraints\|sub_return_delegations\|self_method_tails\|return_infos\|call_bindings\|method_call_bindings\)" src/
   ```
   List every test that reads internal fields. These are the at-risk
   tests; the refactor must preserve those field shapes (or provide
   identical-shaped accessor methods) until a separate cleanup PR.
4. `cargo build --release && perl-lsp --dump-package <root> <pkg> > /tmp/dump-baseline-<pkg>.json` for representative packages
   in fixtures (Mojolicious::Routes::Route, the demo Mojo app's main
   class, a Moo class, a DBIC Result class). These are the **golden
   provenance baselines**. Save them in `tests/fixtures/dump-golden/`
   (or commit alongside this branch's working notes — the user's call;
   default to fixtures so they're CI-checked).

Commit: `chore: capture type-inference test baseline before worklist refactor`

**Acceptance:** baseline files committed; no source changes.

### Phase 2 — Pin the architectural invariants the refactor relies on

**Goal:** add tests that assert the contracts the worklist will preserve.
These tests **must pass on the current implementation** (the second-fold
trick mostly satisfies them) and stay passing through the refactor.

Add to `src/witnesses.rs::tests` (or a new module):

1. **Convergence test.** Build a fixture file with deeply-chained
   reassignment:
   ```perl
   sub make { return Foo->new }
   sub a { my $x = make()->step;     return $x }
   sub b { my $x = a()->step;        return $x }
   sub c { my $x = b()->step;        return $x }
   sub d { my $x = c()->step;        return $x }
   ```
   Assert: `sub_return_type_at_arity("d", None)` returns `ClassName("Foo")`.
   This currently passes if the depth ≤ 2-fold-pass threshold, fails
   silently otherwise. Pin with depth = 4 so the second-fold trick is
   visibly inadequate; raise depth as needed to fail today, then leave.
   *(If this test passes today at depth=4, the second-fold trick is
   accidentally sufficient — push depth higher until it fails. The point
   is to lock chain-depth-correctness before the refactor lands.)*

2. **Plugin-override survives all folds.** A plugin overrides `Foo::bar`
   to return `ClassName("Baz")`. After build, `sub_return_type_at_arity("bar", None)`
   returns `ClassName("Baz")` AND `return_type_provenance(sym(bar))` is
   `PluginOverride { plugin_id: "...", reason: ... }`. Pin both.

3. **Idempotent re-fold.** Build a file. Capture every Symbol's
   `return_type` and the bag's witness count. Run the post-walk pipeline
   *one more time* (expose a test-only `re_fold` hook on Builder). Assert:
   no Symbol's return_type changed; no witness was duplicated. This pins
   that the fold is a fixed point, not an open-ended accumulator.

4. **Provenance round-trip.** For every sub in the fixture's
   `dump-baseline-*.json`, parsing the dump back yields the same
   provenance variants (`PluginOverride`/`ReducerFold`/`Delegation`/`Inferred`).
   Already implicit in the dump golden, but write a unit test that
   exercises `return_type_provenance` directly so it's not only e2e-tested.

Commit: `test: pin convergence + override + idempotency invariants for type inference`

**Acceptance:** `cargo test` green. New tests fail compellingly if you
artificially break the post-walk fold (e.g., delete the second
`resolve_return_types` call), then re-pass once you restore it. This
proves the tests have teeth.

### Phase 3 — Plugin overrides become priority witnesses

**Goal:** kill the "skip PluginOverride subs" branch in
`resolve_return_types`. No external behavior change.

Steps:

1. Extend `WitnessSource` with priority hint. Either an explicit
   `priority: u8` on `Witness`, or a method `WitnessSource::priority()`
   returning `u8` (Builder=10, Plugin=100, Enrichment=10, DerivedFrom=10).
   The exact weights only need to satisfy "Plugin > everything else."
2. `apply_type_overrides` (step 4) stops writing `Symbol.return_type`
   directly. Instead, for each override entry, push a witness:
   ```rust
   Witness {
       attachment: WitnessAttachment::Symbol(sym_id),
       source: WitnessSource::Plugin(plugin_id.clone()),
       payload: WitnessPayload::InferredType(override_type.clone()),
       span: Span::empty(),
   }
   ```
   Plus the existing provenance write
   (`type_provenance.insert(sym_id, PluginOverride{..})`) — that's the
   reader-visible record and stays.
3. `FrameworkAwareTypeFold` (or whichever reducer claims `Symbol`
   attachments) checks witness priority: highest-priority source wins;
   ties fall back to existing rules. Add a small unit test in
   `witnesses.rs` for the priority short-circuit.
4. Delete the `if symbol.is_plugin_override { continue }` branch in
   `resolve_return_types`.
5. Verify `Symbol.return_type` ends up identical post-build for every
   override case in the dump-golden fixtures.

Commit: `refactor: plugin overrides become priority Symbol witnesses`

**Acceptance:** `cargo test` green. `./run_e2e.sh` green. Dump-golden
diff is empty. The "skip PluginOverride" special case no longer exists in
the codebase (`grep -rn "is_plugin_override\|PluginOverride.*skip"` empty).

### Phase 4 — Decompose `resolve_return_types`

**Goal:** turn the 274-line monolith into named reducers + a tiny
driver, without changing observable output. Step 6 and step 9 still run
sequentially; the body of each is now reducer dispatch.

Decomposition (each becomes a public-in-witnesses reducer):

| New reducer | Claims | Replaces |
|---|---|---|
| `ArityReturnReducer` | `Symbol(sym)` with `ArityReturn` payloads | the arity-fold portion of resolve_return_types |
| `DelegationReducer` | `Symbol(sym)` where `sub_return_delegations[sym]` exists | the `return other()` propagation |
| `SelfMethodTailReducer` | `Symbol(sym)` where `self_method_tails[sym]` exists | the `shift->m(...)` tail propagation |
| `CallBindingPropagator` | not a true reducer; runs after the above to mirror Symbol return types into call-binding TCs (the "bag-and-TC sync" line in CLAUDE.md) | the post-fold call-binding loop |

`resolve_return_types`'s remaining body:

```rust
fn resolve_return_types(&mut self) {
    // 1. Group return arms by sub (unchanged).
    // 2. Push ArityReturn witnesses for each arm (unchanged).
    // 3. For each Sub symbol:
    //      let answer = self.registry.reduce_symbol(sym, ...);
    //      if let Some(t) = answer { self.write_sub_return_type(sym, t); }
    // 4. Propagate via DelegationReducer + SelfMethodTailReducer to fixed point
    //    (existing nested fixed-point loop, but each iteration is just a registry call).
    // 5. CallBindingPropagator: sync to TCs.
}
```

**This phase does not introduce the worklist.** Step 6 and step 9 still
both call `resolve_return_types`; the function is just smaller and
delegates to reducers. Internal field shapes (`sub_return_delegations`,
`self_method_tails`) stay so coupled tests are unaffected.

Commit: `refactor: split resolve_return_types into named reducers`

**Acceptance:** `cargo test` green. `./run_e2e.sh` green. Dump-golden
unchanged. `resolve_return_types` body ≤ 80 lines. Each new reducer has
a unit test pinning its claim predicate + a positive fold case + a
negative (declines) case.

### Phase 5 — Old phases 7 + 10 + 8 collapse into `ChainTypingReducer`

**Goal:** the three post-walk CST walks (assignment typing, return arm
refresh, invocant class refresh) become one reducer that runs against
the bag. Tree access is still required — the reducer holds a `&Tree`
through Builder.

Steps:

1. New reducer `ChainTypingReducer` claims `Variable` and `Expression`
   attachments where the witness's payload is `PendingChainType {
   rhs_span }`. The reducer body **is** `resolve_invocant_class_tree`,
   moved verbatim. Existing call sites in `type_assignments_into_bag`
   etc. now push pending witnesses instead of calling the typer
   in-line.
2. The CST walks at steps 7/8/10 stop typing; they only emit
   `PendingChainType` witnesses keyed by the relevant span. They become
   one walk per node-kind class — but since the work is now uniform
   ("emit pending witness, attach to bag, span = node span"), they can
   collapse into one CST walk that handles assignment_expression,
   return_expression, and method_call_invocant.
3. The reducer writes the resolved type back as a `Variable`
   `InferredType` witness + the matching `TypeConstraint` via
   `FileAnalysis::push_type_constraint`. Bag-and-TC sync is preserved.
4. Step 6's first invocation feeds chain types via the same path; step
   9 (still hardcoded for now) re-runs and picks up any chain types
   that needed the first fold to land. Behavior identical to today.

Commit: `refactor: chain typing + return arm refresh + invocant typing collapse into ChainTypingReducer`

**Acceptance:** `cargo test` green. `./run_e2e.sh` green. Dump-golden
unchanged. Three CST walks → one (or zero — if all pending witnesses
can be sourced from the live walk's structural data, no post-walk CST
walk is needed for chain typing).

### Phase 6 — Worklist driver replaces manual ordering

**Goal:** drop the explicit step 6 / step 9 invocations. One
worklist-driven fold to fixed point.

Driver:

```rust
fn fold_to_fixed_point(&mut self) {
    let mut queue: VecDeque<Attachment> = self.initial_dirty_set();
    let mut iterations = 0;
    while let Some(attach) = queue.pop_front() {
        iterations += 1;
        debug_assert!(iterations < MAX_FOLD_ITERATIONS,
                      "fold did not converge — lattice or dependency tracking broken");
        let before = self.snapshot(attach);
        self.registry.reduce_in_place(&mut self.fa, attach);
        if self.snapshot(attach) != before {
            queue.extend(self.dependents_of(attach));
        }
    }
}
```

Initial dirty set: every Sub symbol, every Variable with at least one
witness, every Expression attachment with a pending chain type.

Dependents:
- A Sub's return type changing → its callers' call-binding TCs
  (`call_bindings.iter().filter(|cb| cb.func_name == sym.name)`) and any
  `self_method_tails` / `sub_return_delegations` entry pointing at it.
- A Variable's type changing → method-call refs whose receiver is that
  variable; chain expressions whose rhs span includes a use of it.
- An Expression's type changing → upstream chains that read it.

`MAX_FOLD_ITERATIONS` is a debug-only safety net (e.g., 10× the witness
count); the lattice argument guarantees termination, the assertion
catches dependency-tracking bugs.

Steps:

1. Implement `Attachment` enum (probably already implicit), `snapshot`,
   `dependents_of` in `witnesses.rs` or `builder.rs`.
2. Replace the `resolve_return_types` / chain typing / etc. sequence in
   `build_with_plugins()` with one `fold_to_fixed_point()` call.
3. Run tests. They should pass — the worklist subsumes the two-pass
   trick, including the deep-chain convergence test from Phase 2.
4. `apply_type_overrides` still runs *before* the fold (overrides seed
   the bag). Old pipeline steps 1–3 still run before; old steps 11–13
   still run after.

Commit: `refactor: replace manual fold scheduling with worklist driver`

**Acceptance:** `cargo test` green including the Phase 2 deep-chain
test. `./run_e2e.sh` green. Dump-golden unchanged. The string
`resolve_return_types` is called exactly once in `build_with_plugins`
(or zero times — the worklist may have absorbed it). No
`for _ in 0..2 { resolve_return_types() }` shape anywhere.

### Phase 7 — Documentation reconciliation

**Goal:** docs and code agree.

Steps:

1. Update CLAUDE.md "Build pipeline phases" — the 13-step list collapses
   to ~7 phases (live walk, ref resolution, hash-key linker, plugin
   override seed, worklist fold, POD docs, FA construction + finalize).
2. Update `docs/prompt-type-inference-spec.md` "Status snapshot" — note
   the worklist landing; remove "two fold passes" language.
3. Update `docs/prompt-unification-spec.md` if it cross-references the
   13 phases.
4. Add a short "Worklist invariants" section to CLAUDE.md naming the
   reducers and the dependents relation, so future contributors know
   how to add a new fact-fold without touching the driver.

Commit: `docs: type inference now worklist-driven; reflect in CLAUDE.md and specs`

**Acceptance:** `grep -rn "fold pass 1\|fold pass 2\|13[ -]step\|13 phases"` in `docs/`
and CLAUDE.md returns nothing stale.

## Out of scope for this branch

The architectural review listed several other smells. They land
**separately**, not on this branch:

- **Drift between `Builder::resolve_invocant_class_tree` and
  `FileAnalysis::resolve_expression_type`.** Address by either folding
  the FA-side typer into bag-stored answers (preferred) or formalizing a
  `TypeContext` trait. Both are bigger than this refactor and risk
  changing query-path behavior; defer to a focused branch with its own
  spec.
- **Reducer registry rebuilt per query.** Trivial perf optimization
  (`OnceLock` + a `Default::default()`); orthogonal.
- **Walk-time TypeConstraint emission timing.** Today walk-time TCs
  carry guesses; the worklist subsumes their refinement. Once the
  worklist lands, the walk could in principle stop computing types
  inline, but that's a follow-up — the current behavior is preserved by
  Phases 3–6 without that change.
- **Plugin-registered reducers from Rhai.** Trait exists in Rust,
  exposing it to Rhai is a separate plugin-system concern.

## Acceptance summary

A single PR (or stack of commits on `main`) where:

- Every commit is `cargo test` + `./run_e2e.sh` green.
- Dump-golden fixtures committed in Phase 1 are byte-identical at every
  commit on the branch (proves no observable behavior change).
- Final state has: one fold driver (no `resolve_return_types` called
  twice), no "skip PluginOverride" branch, `ChainTypingReducer` owning
  the three former post-walk CST walks, named reducers for Arity /
  Delegation / SelfMethodTail / Override.
- New invariant tests (deep chain, plugin override, idempotency,
  provenance) pass throughout.
- CLAUDE.md and the type-inference spec describe the worklist, not the
  13-step list.

## Bootstrapping the implementation session

The fresh session should:

1. Read this spec, `docs/prompt-type-inference-spec.md`, the
   architectural-review feedback in the user's prior conversation
   (summarized in §"Why" above), and CLAUDE.md.
2. Run Phase 0 (test lift) and Phase 1 (baseline capture) before any
   refactor work. Don't skip the dump-golden capture in Phase 1 — it
   is the single best regression net for this refactor.
3. Land each phase as its own commit on a branch. **Do not push;** the
   user reviews the stack locally.
4. If a phase's tests fail in a way that requires changing internal
   field shapes that existing tests read, **stop and check in** before
   proceeding. The default is "preserve internal field shapes via
   compatibility views"; deviating is a spec change, not a tactical
   decision.
5. After Phase 6 lands and is green, run a full diff of dump-golden
   fixtures and report any drift to the user. Drift is a regression by
   definition for this refactor.
