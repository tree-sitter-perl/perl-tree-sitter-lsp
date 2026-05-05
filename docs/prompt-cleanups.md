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
(`Symbol(sid)` plain `InferredType`, plus the matching `NamedSub` /
`MethodOnClass{class, name}` mirrors). Writeback's job becomes
"ensure the canonical witnesses are present" rather than "iterate the
map." `fold_state_snapshot`'s second tuple element switches from
"reading the map" to "querying `Symbol(sid)` via the registry" — the
worklist's fixed-point detection then watches what consumers actually
see, which is the snapshot's intent.

**Watch out for:** the worklist re-runs each iteration. The bag
already has `remove_by_source_tag` for re-emittable passes — use it
before pushing the writeback witnesses on every iteration so we don't
accumulate stale `Symbol(sid)` entries from earlier rounds.

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
output with a query to the bag for `Edge(NamedSub(_))` /
`Edge(MethodOnClass{...})` payloads on the sub's `Symbol(sid)`
attachment — that's where the tail-call edge actually lives now.
Tests that assert on the field move to asserting on the edge witness
in the bag.

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
