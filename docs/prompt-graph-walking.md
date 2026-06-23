# Graph walking — forward work

The walker landed: `src/graph.rs` (`GraphView` + `walk`), the closed
`EdgeKind` enum with exhaustive `edges_from`, and the INHERITS /
INHERITS_INV / BRIDGES edge kinds. The whole inheritance axis routes
through it; `children_index`'s descendant fan-out and plugin bridges
too. Landed design + rationale: `docs/adr/graph-walking.md`. This doc
is what's left.

## PARKED: instance brands (behind the long-distance value-provenance tier)

The motivation: scope plugin-synthesized dispatch to the right
*instance* so the LSP doesn't merge across co-resident objects —
`$a->enqueue('tb')` shouldn't reach `$b`'s task; `$app->minion` and
`$app->other_minion` are different queues; two Mojo::Lite apps in one
workspace shouldn't see each other's helpers.

**A spike was built and then closed (PRs #65/#66, branches
`branded-edges` / `branded-edges-accessor`, kept for reference).** It is
parked, not abandoned — but it must NOT be rebuilt the way it was. The
spike keyed an accessor receiver on its *syntactic name* (`$app->minion`
→ `acc:minion`). That is the rule-#10 mistake in disguise: it regressed
the moment a variable aliased the accessor (`my $m = $app->minion;
$m->enqueue` — different key, no resolve), and it can never be made
right by adding more string cases.

### Why it's parked, and on what

"Which instance is this receiver" is a *runtime* fact. Recovering it
statically is **value provenance**: trace a receiver to the site where
its instance was *born*. And that is the exact same engine as the
high-value implicit-contract features we want anyway — "what types can
this hash key hold", "what type is the n-th arg of this callback",
value-indexed returns. Those live in `prompt-type-inference-residual.md`
(Parts 1–5) and are **not yet built**. Instance brands are one
downstream *consumer* of that engine, so they wait for it. Building a
bespoke half-chase now (the spike) is strictly the wrong move — it
reproduces the type system's provenance badly and rots.

### The rule, for when the foundation exists

Brand = the **birth-site of the instance**, one uniform rule, no
fallback and no special case:

- `my $x = Class->new` → `$x`'s decl.
- `$obj->accessor` → resolve the accessor → the instance its body
  *returns* (the captured lexical / `state` var) → recurse.
- `Class->new` inline (anonymous) → the `->new` call-site span.

The accessor case **rides the existing return-edge chase** — the same
`Symbol(sid) → Edge(Expr(last_expr_span))` the return-*type* chase walks
(`adr/return-expr.md`), projected to the tail expr's `resolves_to` decl
instead of its `InferredType`. Verified against the real substrate: the
Minion helper is `$app->helper(minion => sub {$minion})` (lexical
capture, in `gold-corpus/local/.../Mojolicious/Plugin/Minion.pm` — the
LSP already indexes it), so `$app->minion`, `$c->minion`, and a direct
`$minion->add_task` all chase to the same `my $minion` decl → one brand.
This collapses the per-variable, accessor, and alias cases into a single
"trace to the birth site" — none are separate tiers.

### What must stay OUT of core

The "home app" notion (which app instance owns this minion) is a
*plugin* concept and must never enter core. Core ships only:

- generic value-provenance birth-site resolution (framework-free), and
- bounded direct keys (a *file* identity, a *same-file lexical* decl).

The Mojo-specific per-app overlay — needed ONLY to disambiguate multiple
apps loading the same plugin (one static `my $minion` decl, N runtime
instances) — is a plugin-supplied qualifier composed as an opaque
`(birth-site, Option<home>)` tuple with additive component-wise matching
(`None` home = shared, visible to all). Single-app needs zero home
concept. Core treats the brand as an opaque string; the plugin/entrypoint
layer supplies the qualifier. The irreducible gap (a shared controller
serving multiple apps with an app-specific task — statically
unattributable, same wall as full-Mojo multi-app helpers) is documented,
not solved.

### The line

- **Direct keys** (file identity, same-file lexical decl) need no chase
  and could stand alone if the DX is ever judged worth it on its own.
- **Flow keys** (accessor-return instance, constructor injection,
  fields, the "class that takes a minion") need the value-provenance
  tier — and that is the gate this whole feature waits behind.

Prerequisite to un-park: the value-provenance layer of
`prompt-type-inference-residual.md` (esp. Part 2 hash-key unions,
Part 5a value-indexed returns, and constructor/field value flow). When
that lands, build the birth-site rule as one of its consumers.

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
- **Stack-graphs upstream** — evaluated with a running spike + a gold-corpus
  coverage census + a dependency/LOC count; **not adopted**. Full write-up:
  `docs/adr/stack-graphs-evaluation.md` (spike: `docs/stack-graphs-spike.rs`).
  The short of it: stack graphs do name binding, not type inference, and in
  Perl the dominant `$obj->method` case is type-gated — so they'd cover ~38%
  of resolution rows while re-encoding rules we already have, and leave the
  ~62% type/framework-gated rows on the witness bag. The custom typed-edge
  view (`graph.rs`, 172 lines) integrates with the witness/reducer pipeline
  and carries types on edges, which stack graphs cannot. Revisit only if
  stack graphs gain value/type-flow semantics AND an upstream Perl definition
  exists.
