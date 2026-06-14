# ADR: Graph walking — one lazy walker over the visibility edges

Several queries answered "what is visible from here" by walking
conceptually-one graph through different bespoke code: the class MRO
(`for_each_ancestor_class`), the composer fan-out (`children_index`),
plugin-namespace bridges (`for_each_entity_bridged_to`). Forgetting one
was a class of "cross-file feature mysteriously degraded" bug. `walk`
is the single traversal those collapse into.

## The shape: a lazy derived view, not a built graph

`src/graph.rs::GraphView` holds two borrows — `&FileAnalysis` and
`Option<&dyn CrossFileLookup>` — and stores nothing. There is no node
list, no adjacency map, no build step. `walk(origin, mask, visit)`
DFS-chases edges on demand: `edges_from` derives a node's neighbours at
the moment the walker asks, by calling the stores that already exist
(`parents_of`, `direct_children_of`, `for_each_entity_bridged_to`). A
walk materialises only its transient state (seen-set + per-node
neighbour vec), discarded on return. "Chasing an edge" *is* a map
lookup against `package_parents` — the same primitive the ported
consumers used, so a port is parity by construction.

`walk` is the caching seam: every consumer goes through it and never
sees how edges are produced, so caching slots in behind `edges_from`
with zero consumer change. Per-edge-kind, not all-or-nothing —
`children_index` is the materialised form of the INHERITS_INV edge (the
reverse walk was expensive, precomputed once), while INHERITS stays a
lazy `parents_of` lookup. The home for an edge cache is `ModuleIndex`
(the only `CrossFileLookup` impl), behind the trait method — the view
never owns cross-query state. Lazy is the default so nothing pays for
that machinery until a profiler asks.

## `EdgeKind` is a closed enum; `edges_from` is exhaustive

The edge kinds are a closed `enum EdgeKind { Inherits, InheritsInv,
Bridges }`, and `edges_from` matches it **exhaustively** — adding a
kind is a compile error until its derivation is written. This is the
"one match site, never a parallel walker" rule with compiler teeth.
The `EdgeKindMask` bitflags exists only for set membership + ergonomic
`|` (a walk traverses several kinds at once); `EdgeKind` is the source
of truth, and `EdgeKind::ALL` + `flag()` keep the two in lockstep (a
test pins that the `ALL`-union covers every mask bit, the one hole the
fixed-length array can't catch at compile time).

## Model layer — why the view sits below the index

`GraphView` imports only `file_analysis` (the `CrossFileLookup` trait +
`parents_of`) — zero Index-layer deps — so it lives at the Model layer.
That lets the model-internal walkers (`for_each_ancestor_class` and the
method/dispatch/bridge resolution that funnels through it) call `walk`
directly, no up-layer import. The DAG test permits this: graph and
`file_analysis` mutually reference at the same layer (only a strictly-
upward import is a violation). The alternative — relocating the
ancestry query functions up out of the model — is available if the
model ever gets fat, but is not needed: the view stays pure, and any
caching lives at the Index layer behind the trait.

## The inheritance axis is one walk

Every inheritance consumer funnels through `for_each_ancestor_class`,
which is now `visit(self)` then `walk(class, INHERITS)`. Self-handling
lives in that one wrapper — it runs the consumer's own closure on the
origin, which is consumer-specific (`class_isa` checks equality,
`resolve_method_in_ancestors` checks the method) and so cannot move
into `walk`, which holds no closure for the origin. `SUPER::` — parents
only, never self — is the bare `walk`; there is no `skip_self` flag,
because *checking self is not a walk*. The cycle guard is `walk`'s
path-depth cap (the seen-set already breaks cycles); a class with many
ancestors is no longer silently truncated the way the old total-node
cap did.

## Boundaries — what is NOT a walk

Two of the "parallel models" the graph was meant to absorb are not
edge-following traversals, and forcing them in would be contortion:

- **File roles** (`refs_to`/`references_mask_for` open→workspace→
  dependency tiers) ENUMERATE every file and scan its analysis — there
  is no origin and no edge to chase. The completeness this guarantees
  (re-exports, framework DSL, dynamic dispatch produce refs with no
  clean import edge) is the whole point of the scan. `RoleMask` is a
  partition, not a traversal, and stays.
- **Lexical scope chains** are a single-parent linked-list climb to the
  file scope — no branching, no cycles, no cross-file. `walk`'s
  machinery buys nothing. The one parent-climb is `scope_chain_of(&[Scope],
  start)`, shared by `FileAnalysis::scope_chain` and the witness-bag
  query path (which holds a `&[Scope]` slice, not a `&FileAnalysis`).

The migration ends when `package_parents` and `PluginNamespace.bridges`
have no direct consumers — every *reachability* question is a `walk`
with a mask. `RoleMask` is not in that set.
