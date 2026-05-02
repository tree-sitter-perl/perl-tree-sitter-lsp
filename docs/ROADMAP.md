# perl-lsp Roadmap

> Where the engine is heading. Cross-refs into individual prompt-* and
> adr-* docs for detail. Read the ADRs for landed architecture; read
> the prompts for what's queued.

## Direction

Focus is **type intelligence** + **engine architecture**. Most user-facing
LSP features are already wired (completion, hover, goto-def, references,
rename, semantic tokens, sig help, inlay hints, etc.). The next round of
user-visible wins comes from making them **smarter**, not adding more —
which is downstream of a better-typed engine and a cleaner core.

User-facing features (Mojo polish, CLI diagnostics, distribution) are
backburner: ship when types + architecture are in a healthy place.

---

## Primary arc: type inference

Three docs, three axes, one engine:

| Doc | Axis | Status |
|---|---|---|
| `prompt-type-inference-unification.md` | **HOW** — collapse walker + bag into one path | 4-step staircase, each ships green |
| `prompt-type-inference-residual.md` | **WHAT'S MISSING** — Parts 1–5 fact classes | each is a reducer+emitter pair |
| `prompt-sequence-types.md` | **FIRST BIG CONSUMER** — sequence type lattice | 5 phases; ~half the spike's diff on a clean foundation |

### Dependency map

```
unification step 1 ─┐
                    ├─▶ step 2 ─▶ step 3 ─┬─▶ step 4 (full collapse)
                    │                     │
                    │                     └─▶ sequence-types phases 1-3
                    │
                    └─▶ residual Parts 1-5  (independent of the staircase)
```

- **Unification steps 1+2** are small (~50 + ~150 lines, mostly deletions). No new
  capability; debt service that makes everything after it cheaper.
- **Unification step 3** retires the `ReducerQuery` closures and replaces them
  with `ReducerContext`. Enforces "the bag is canonical" *by construction*.
  Unblocks sequence-types proper.
- **Step 4** is the full collapse (delete `infer_returned_value_type_seen` etc.).
  Optional in the sense that 1-3 already pay for themselves.
- **Sequence-types phases 1-3** depend on step 3 for the closure-free
  `ReducerContext`. Phase 4-5 (cross-file mutation effects, pipelines) are
  separate design surfaces that compose on top.
- **Residual Parts 1-5** (invocant mutations, hash-key unions, method loops,
  functional operators, value-indexed returns / sum types / parametric types)
  are each independent reducer+emitter pairs. Order by value; pick what hurts
  most when missing.

### Recommended sequencing

1. Steps 1+2 of the unification staircase (debt service, fast).
2. Step 3 (canonicality enforcement). The keystone for what follows.
3. Sequence-types phases 1-3, fresh on the clean foundation.
4. Residual Parts (start with whichever is hurting most — likely Part 5b
   narrowing or Part 5c parametric types for DBIC, depending on workload).
5. Step 4 of the staircase, when convenient.

---

## Architecture debt

### Graph traversal — needs a rethink, not just a Namespace enum

The original `prompt-unification-residual.md` Phase 1 proposed widening
`package_parents: HashMap<String, Vec<String>>` to a `Namespace` enum
(Package / MojoApp / MojoController / MojoAction / …). That's the wrong
keystone.

**The real problem:** we don't have a clean graph-walk primitive. Several
walkers do conceptually similar work via different code paths —
`resolve_method_in_ancestors`, `for_each_entity_bridged_to`, the cross-file
resolver in `resolve.rs`, the plugin-namespace bridges, the import-list
walks. Each handles different edge types (parent class, plugin bridge,
package qualified, imported) and forgetting one is a class of "cross-file
feature mysteriously degraded" bug.

Widening the key from `String` to `enum Namespace` doesn't fix that — it
just spreads the same ad-hoc walking across a richer alphabet.

**The shape we actually need:** a typed-edge graph (whatever the nodes are
called) with one walk function. Edges have kinds (parent / role / bridge /
import / framework-scope). Queries supply a kind-mask (analogous to how
`RoleMask` works for `FileStore` today). Today's bespoke walkers collapse
into "graph-walk + filter."

**Until that lands**, Phase 6 (Openness diagnostic) and Phase 7's
`home_namespace` move are blocked. Whether the resulting node identity uses
`String` or an enum is incidental — the question worth answering is "what's
the graph?".

### Eager Ref targets (Phase 5)

`Ref.resolves_to: Option<SymbolId>` → `Ref.target: SymbolId` (with
`UNRESOLVED` sentinel). Removes lazy owner-resolution from query paths.
Independent of the graph question; can land any time.

`refs_by_target` already exists; just the eager-target invariant is missing.

### Why this matters for type inference

The type-inference unification staircase is the immediate priority, but the
graph rework is the next big architectural pillar. Both follow the same
pattern: identify a pile of ad-hoc code paths doing morally-similar work,
collapse them into one canonical mechanism, enforce the invariant by
construction. Worth queuing the graph design discussion before Phase 6 work
becomes urgent.

---

## Backburner (user-facing)

Wired today; smarter is downstream of the engine work above.

| Doc | What |
|---|---|
| `prompt-mojo-todo.md` | Stash keys, hook completion, route naming + url_for, plugin chain transitive helpers, config completion |
| `prompt-cli-tools.md` | Diagnostic framework (PL001–PL010, `.perl-lsp.json`, suppression directives, SARIF), `--migrate`, remaining analysis subcommands |
| `prompt-ref-provenance.md` | Constant-fold `folded_from`, framework-attribute unified rename, package→file rename, inheritance override scoping |
| `prompt-wasm-web-extension.md` | Cargo workspace split + browser extension; orthogonal to everything else |

These are ship-when-ready, not blocked. Several are worth slotting between
type-inference phases when an engine refactor needs cooling time.

---

## Out of scope

- Multi-workspace / monorepo.
- Cross-file rename of deps (deps are read-only — `RoleMask::EDITABLE`
  enforces this).
- Effect facts (mutation / throw / IO).
- Full dependent inference / univalence.
- `wantarray` / context-dispatched returns (mechanical to add when the
  pattern surfaces; not currently prioritized).
- Aliasing through scalars across function boundaries.
- Dynamic Namespace extension at runtime by third-party plugins (depends on
  the graph rework above).

---

## Reading order for someone joining

1. `CLAUDE.md` — live architecture, build pipeline, worklist invariants,
   plugin mechanics, type-inference query path. Source of truth.
2. `docs/adr/*.md` — load-bearing decisions for landed work (pod rendering,
   plugin system, error recovery, file store + resolve).
3. This roadmap — what's queued, what blocks what.
4. The relevant `prompt-*.md` for the workstream you're picking up.
