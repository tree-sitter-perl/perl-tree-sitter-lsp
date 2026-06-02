# Enrichment + inheritance residual — diagnosis

Does plugin/framework behavior reach a class through the **inheritance**
and **cross-file-dependency** chain? Two angles, both surfaced during the
cross-project usability sweep.

The short verdict:

- **Return-type / method resolution** through inheritance + cross-file
  bridges is **correct by design** — the `MethodOnClass{class, name}`
  query walks `parents_of` (the single seam) and reaches into cached
  modules' bags. Build-time edges (`inheritance` / `inheritance_cross`)
  optimize the common path; the `query_rec` structural fallback is the
  floor for any FA that skipped enrichment.

- **Plugin EMISSION gated on the class's ancestry** (`ClassIsa` triggers;
  `param_types` `in_role`) is **NOT cross-file aware**, and can't be
  without changing the index-free-builder contract (rule #1). Latent
  hazard, documented, deferred.

- **Dispatch-verb promotion in non-open files** WAS a real gap caused by
  the resolver lifecycle (only OPEN documents enriched). **Landed:** the
  `ReceiverGated<DispatchCandidate>` seam resolves the receiver isa-check at
  query time (`docs/adr/receiver-gated-dispatch.md`), so call sites in non-
  open workspace/dependency files surface like open ones.

Two `#[ignore]`-d failing repros pin the two real gaps
(`builder_tests.rs::param_types_manifest::probe_*`).

---

## A. Per-manifest applicability matrix

For each declarative manifest member (`docs/adr/plugin-system.md`
"Declarative manifests"), whether its applicability through inheritance is
(i) correctly local-only by Perl semantics, (ii) correctly
cross-file/isa-based already, or (iii) BROKEN.

| Manifest member | Applicability gate | Verdict |
|---|---|---|
| `overrides()` | Trigger-INDEPENDENT post-pass (`apply_type_overrides`, `builder.rs:320`). Applied to every build regardless of `use`s. | **(ii) correct.** Plugin-priority witness on `Symbol(sid)`; the home module picks it up even without `use Mojolicious`. Cross-file consumers reach it via the `MethodOnClass → Edge(Symbol)` writeback + cross-file primary lookup. |
| `dispatch_verbs()` | RECEIVER-isa at QUERY time (`FileAnalysis::applicable_dispatches`). Each candidate rides the cache as `ReceiverGated<DispatchCandidate>`; `resolve_for` walks the single `class_isa` seam (local `package_parents` ∪ `module_index.parents_cached`). | **(ii) correct, all files.** The emit-hook path (`EmitAction::DispatchCall`) covers files that `use Minion`; the manifest path covers cross-file/subclass receivers and now resolves lazily at the query, so non-open workspace/dependency files surface identically. See `docs/adr/receiver-gated-dispatch.md`. |
| `type_constraint_names()` / `type_constraint_inner()` | Syntactic gate on constraint-constructor call names where the `has`/`isa` appears. | **(i) correctly local.** The constraint vocabulary applies where the constructor expression is written. Not an inheritance question. |
| `param_types()` | `in_role` gate on the enclosing package, checked CROSS-FILE at QUERY time. The builder emits one `ReceiverGated<TypeConstraint>` per matching sub (ungated, `gate = in_role`); `FileAnalysis::gated_param_type_for` resolves it via `resolve_for(enclosing_package, …)` when the variable is typed. | **(ii) correct, all files.** A controller whose `in_role` ancestor is reachable only through a cross-file intermediate now types its param (the catalyst `$c` case). Resolution shares the single `class_isa` seam; the `ReceiverGated` wrapper keeps the typed TC unreadable without the isa check. Landed via the receiver-gated Phase 2 (`docs/adr/receiver-gated-dispatch.md`). |
| `app_surface_consumers()` | Trigger-INDEPENDENT bake (`builder.rs:254`) onto `FileAnalysis.app_surface_consumers`; consumed by `parents_of` as the synthetic `APP_SURFACE_CLASS` edge. | **(ii) correct.** `parents_of` is the single seam; the `MethodOnClass` walk + ancestor walks all route through it, so the surface resolves cross-file from every consumer. |

### The `ClassIsa` trigger axis (not a manifest member, same root cause)

Several bundled plugins fire on `ClassIsa` triggers (mojo-events
`Mojo::EventEmitter`, minion `Minion`, mojo-helpers / mojo-routes
`Mojolicious`, …). `PluginRegistry::applicable` (`plugin/mod.rs:991`)
matches the trigger against the `parents` list, which the builder fills
from `transitive_parents` — **local `package_parents` only**
(`builder.rs:2121`, `:2175`, `:2200`).

Consequence: a class whose trigger-class ancestry is established
cross-file does NOT get the plugin's emit hooks. E.g. `package Leaf; use
parent 'Mid';` where `Mid` (another file) extends `Mojo::EventEmitter` —
`$self->on('ready', …)` in `Leaf` synthesizes no Handler symbol, because
the `ClassIsa("Mojo::EventEmitter")` trigger sees only `Mid`.

Confirmed by `probe_class_isa_trigger_through_cross_file_parent` (FAILS).

**Why it can't be a contained fix.** Plugin emit hooks (`on_method_call`,
`on_function_call`, `on_use`) run at PARSE TIME inside `build()`, where the
builder is index-free by rule #1. There is no module index to consult
mid-walk, and even if one were threaded in, indexing order isn't
guaranteed (the dependency carrying `Mojo::EventEmitter` may not be indexed
yet). The `dispatch_verbs` path answers this by deferring the receiver
isa-check to QUERY time (the `ReceiverGated` seam, `applicable_dispatches`),
which owns the module index. Making `ClassIsa` cross-file-aware is the same
move for *emission*: a gated producer resolved post-index. Phase 2 mints it
on the same seam — see `docs/adr/receiver-gated-dispatch.md`.

The in-file case composes correctly today
(`plugin_mojo_events_triggers_through_transitive_parent` — Mid + Leaf same
file). It's strictly the cross-file ancestry that's missed.

---

## B. Enrichment for dependencies / non-open files

`enrich_imported_types_with_keys` (which runs the imported-hash-key
synthesis, the cross-file `inheritance_cross` edge projection, and
`resolve_method_call_types(Some(idx))`) is called ONLY for:

- OPEN documents — `backend.rs:50` (resolver refresh, `for_each_open_mut`)
  and `backend.rs:72` (`publish_diagnostics`).
- The CLI focus file — `main.rs:469/513/556/761`.

**Dependency modules** (`module_resolver.rs::resolve_and_parse_inner:552`)
and **workspace-index files** (`index_workspace_with_index:655`) are built
via `crate::builder::build(...)` which ends at `finalize_post_walk()` —
they NEVER get `enrich_imported_types_with_keys`. The resolver refresh
callback re-enriches only open docs (`for_each_open_mut`,
`backend.rs:49`).

What this means per enrichment pass:

| Enrichment pass | Reaches non-open files? | Compensated at query time? |
|---|---|---|
| `inheritance_cross` edge projection | No | **Yes** — `query_rec`'s structural `MethodOnClass` fallback (`witnesses.rs:1190`+) walks `parents_of` + recurses into cached bags + `for_each_entity_bridged_to`, with `BagContext.module_index` set. So a dep class inheriting from another dep, or bridging a plugin namespace, still resolves method return types. The build-time edges are an optimization the dep misses, not a correctness floor. |
| imported hash-key synthesis + `resolve_method_call_types` | No | Partially — cross-file method-return queries route through the same `MethodOnClass` walk; imported-hash-key COMPLETION on a binding inside a non-open dep file would miss the synthetic `HashKeyDef`, but that's a completion-in-a-dependency scenario the LSP doesn't surface (you complete in open files). |
| dispatch resolution | N/A (no enrichment pass) | **Yes — query-time.** Gated candidates ride the cache; `resolve::refs_to` for a Handler calls `applicable_dispatches`, which resolves the receiver isa-check on demand against the module index. A `$minion->enqueue('task')` in a non-open workspace file whose receiver isa-Minion is cross-file (and which doesn't `use Minion`) now surfaces. The `ReceiverGated` seam makes the inner handler payload unreadable without that check (`docs/adr/receiver-gated-dispatch.md`). |

Confirmed by `probe_dispatch_promotion_in_unenriched_workspace_file`
(FAILS).

### Verdict on B

The cross-file `MethodOnClass` walk (the structural `query_rec` fallback
the working-bag-residual doc flagged as "transitional pending
graph-walking") is doing exactly the job it was kept for: it is the
correctness floor for return-type / method resolution when a caller
bypasses enrichment. The "what if the caller bypasses enrichment?"
question from D3's residue is **answered correctly** for type resolution.

The dispatch gap is **closed**: the query-time middle option below landed.
The Handler ref query (and dispatch goto-def) scans each file's gated
dispatch candidates and resolves the receiver isa-check lazily, the same
way the `MethodOnClass` walk resolves types — so dispatch resolution now
joins type resolution as a query-time answer, not an enrichment-time ref.

The landed shape: dispatch candidates ride the cache as
`ReceiverGated<DispatchCandidate>`; `refs_to` calls
`FileAnalysis::applicable_dispatches`, which resolves each candidate's
receiver isa at query time. Keeps the "no enrichment over deps" invariant,
mirrors the type path, and the `ReceiverGated` type makes the inner handler
payload unreadable without the isa filter (`docs/adr/receiver-gated-dispatch.md`).

---

## What is correct-by-design vs broken vs latent hazard

- **Correct by design:** `overrides`, `app_surface_consumers`,
  `type_constraint_names`, and all return-type/method resolution through
  inheritance + cross-file bridges (the `parents_of` seam + `MethodOnClass`
  walk). The structural `query_rec` fallback is the deliberate floor for
  enrichment-bypassing callers.

- **Latent hazard (deferred):** cross-file `ClassIsa`-trigger emission and
  `param_types` `in_role` via a cross-file ancestor. Both stem from
  `transitive_parents` being local-only, which is forced by the
  index-free-builder contract. Fixing means moving emit-hook firing into a
  post-index pass — the same migration `dispatch_verbs` already made.

- **Landed:** dispatch-verb resolution in non-open files, via the
  query-time `ReceiverGated` seam (`docs/adr/receiver-gated-dispatch.md`).
  References-to-handler and dispatch goto-def now reach call sites in
  unenriched workspace/dependency files.

## Repros

`src/builder_tests.rs`:

- `param_types_manifest::probe_class_isa_trigger_through_cross_file_parent`
  — `#[ignore]`, FAILS. Cross-file `ClassIsa` trigger. STILL OPEN; Phase 2
  mints this on the same `ReceiverGated` seam.
- `param_types_manifest::dispatch_resolves_query_time_in_unenriched_workspace_file`
  — PASSES (un-ignored). The dispatch gap's acceptance test, now green via
  query-time gated resolution.
- `resolve::tests::refs_to_handler_finds_dispatch_in_unenriched_cross_file_subclass`
  — PASSES. A Handler `refs_to` reaches a dispatch call site in a non-open
  workspace file whose receiver isa-resolves cross-file.
