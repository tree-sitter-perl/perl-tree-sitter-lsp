# Graph walking — forward work

The walker landed: `src/graph.rs` (`GraphView` + `walk`), the closed
`EdgeKind` enum with exhaustive `edges_from`, and the INHERITS /
INHERITS_INV / BRIDGES edge kinds. The whole inheritance axis routes
through it; `children_index`'s descendant fan-out and plugin bridges
too. Landed design + rationale: `docs/adr/graph-walking.md`. This doc
is what's left.

## Next: branded edges

A branded edge fires only when the walker's *context* matches a brand —
the instance-identity cases the plain class/bridge edges can't express.

```rust
enum Brand {
    Variable { file: FileId, scope: ScopeId, name: String },
    File(FileId),
    Custom { kind: String, payload: serde_json::Value },  // plugins
}
```

Concrete cases:

- **`my $minion = Minion->new(...)`** — entities of `$minion`'s plugin
  namespace bridge to `Brand::Variable { … "$minion" }`. A second
  Minion instance in the same workspace doesn't collide.
- **`my $app = Mojolicious::Lite->new`** — the app's namespace bridges
  to `Brand::File(file_id)` (the multi-app Mojo case).

The goal is for brands to fit `walk` cleanly — a brand is walk
*context* that gates branded edges, not a bolt-on. `walk` already
carries the origin; brand context rides alongside, and `edges_from`
consults it for branded edge kinds.

**The one real decision is brand-resolution timing.** The hard case
isn't `my $minion = Minion->new` (static, visible at the decl) — it's
`$app->minion` vs `$app->other_minion`, where the brand depends on what
the accessor returns (type inference, possibly cross-file). Three
options:

1. **Static at emission** — covers `my $x = Class->new` and per-file
   singletons; misses accessor-returned brands.
2. **Cross-file enrichment post-pass** — after inference settles, walk
   method-call refs whose receiver resolves to a branded namespace and
   materialise the branded edges. Same machinery as
   `enrich_imported_types_with_keys`.
3. **Lazy at query time** — branded edges carry a compute-brand
   closure. Reintroduces the query-time logic the unification is
   trying to remove; avoid.

Plan: **(1) for the cheap cases, (2) for accessor chains.** Land (1)
first; the accessor-chain case shouldn't block it. Decide the timing
deliberately when this is picked up — don't defer it into the code.

## Deferred: Scope nodes (the future taxonomy)

`Node::Scope` + a PARENT edge would let the graph model lexical scopes,
packages, and plugin namespaces as one node space — the foundation for:

- **Openness diagnostic.** Walk the namespace chain from a ref's site:
  terminates `Closed` without resolving → warn; hits `Open`
  (role/plugin/abstract base) first → suppress. Replaces the bespoke
  `framework_imports` suppression and unifies unresolved
  function/method/stash-key/route/helper.
- **`Symbol.home_namespace`.** `Symbol.package: Option<String>` →
  `Symbol.home_scope: NodeId`; `Bridge::Class(...)` → a bridge to a
  framework scope node directly.

Deferred deliberately: the scope parent-climb is a linked-list, not a
walk (`adr/graph-walking.md`), so `Node::Scope` earns its keep only
when Openness / `home_namespace` are actually built — not to port the
trivial `scope_chain_of`. Build it when those land, with their needs
shaping the node.

## Out of scope (decided, not deferred)

- **File roles** stay `RoleMask` — enumeration, not traversal
  (`adr/graph-walking.md`).
- **Stack-graphs upstream** — evaluated, not adopted. The Rust-API
  construction path would restate Perl-specific emission (plugin
  namespaces, framework synthesis) in their pipeline; a custom
  typed-edge view integrates with the existing builder + witness/
  reducer pipeline at a few hundred lines. Revisit only if their path
  semantics + tooling become worth that boundary.
