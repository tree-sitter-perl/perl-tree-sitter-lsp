# Graph Walking: One Typed-Edge Graph, One Walker

> **Scheduled NOW (June 2026).** The type-inference arc's remaining
> items are all explicitly deferred-by-design; meanwhile six bespoke
> walk mechanisms have accumulated (`parents_of`, `ModuleEdgeIndexes`'
> three maps, `for_each_descendant_package`, `ReceiverGated`, RoleMask
> tiers) — each with its own B6-style feed discipline. The strangler-
> fig consumers exist; this is the next pillar.
>
> Cross-refs: `adr/file-store-and-resolve.md` (today's `RoleMask` model
> that this retires); `adr/plugin-system.md` (today's `PluginNamespace`
> + `Bridge` that becomes typed edges); `adr/role-contracts.md`
> (children_index — the inverse edge this graph makes first-class).
> This doc also absorbed the unification residuals (Openness,
> `home_namespace`, the eager `Ref.target` field) — see the final
> section.

## The problem

Several walkers do conceptually-similar work via different code paths:

- `resolve_method_in_ancestors` — class hierarchy walk.
- `for_each_entity_bridged_to(class, ...)` — plugin namespace bridges.
- `resolve.rs::resolve_symbol(name, from, RoleMask::VISIBLE)` — file tier.
- `cursor_context.rs` scope-chain walks — lexical resolution.
- Import-list resolution scattered across `enrich_*` passes.

Each handles different edge types (parent / role / bridge / import /
file-tier). Forgetting one is a class of "cross-file feature mysteriously
degraded" bug. Adding a new edge kind today means touching N walkers.

The string-keyed `package_parents` graph is the core, but it's siloed from
the lexical `Scope` tree, the plugin `Bridge` enum, and the file-tier
`RoleMask`. Four parallel models for what's morally one graph.

## The shape

Three node kinds. One walker. Typed edges. Branded edges where identity
matters.

### Nodes

- **Scope** — lexical block, package, module/file, plugin namespace,
  builtin. Loose superset of "anything that can contain a binding."
- **Symbol** — definition. Has a home scope.
- **Ref** — use site. Resolves to a symbol via the walk.

### Edges

Every "X is visible from Y" relationship in the codebase, typed:

#### Lexical structure
- **`parent`** — scope → enclosing scope.
- **`member`** — symbol → scope.

#### Variable binding (sigil-aware)
- **`binds_lexical`** — `my $x`. Lives in declaring scope.
- **`binds_persistent`** — `state $x`. Lexical-shaped, init once.
- **`binds_param`** — sub/method params. Marker for invocant + position.
- **`binds_dynamic`** — `local $x`. **See open question below.**

`our $x` is **not** a separate edge kind. Instead: every lexical scope
inside `package Foo` has a `parent` edge to `Foo`'s package scope. `our`
declarations install the symbol *in* the package scope, and the normal
walk finds it through the existing parent chain. **Invariant to enforce
at emission**: lexical scope → package scope `parent` edges are
mandatory. This is a single path; no second mechanism for `our`.

#### Package structure
- **`provides_package`** — file → package (multiple files contribute).
- **`inherits`** — package → package. Subkind: `extends` (use parent /
  `@ISA` / `class :isa` / `Mojo::Base 'X'`) | `with_role` (`with` /
  `class :does` / `does => 'Role'`) | `loads_component` (DBIC).
- DFS walk + kind-mask is the policy. **No MRO/C3 modeling.**

#### Cross-package access
- **`imports_sub`** — package → symbol. Subkind: `qw_explicit` |
  `tag_export` | `framework_dsl` (Mojo::Lite suppression entries).
- **`re_exports`** — package → symbol (`our @EXPORT = qw(bar)`).
- **`uses_module`** — file → file (the import's source file).

#### Hash key ownership
- **`owns_hashkey`** — scope → hashkey symbol. Subkind: `Sub(name)` |
  `Class(name)` | `Variable(scope, name)`.
- **No `Constructor` subkind.** Today's `HashKeyOwner::Sub("new")` for
  Moo/Moose `has`-derived constructor keys collapses into a single owner
  path. Pick whichever (Class is the natural one — the keys belong to
  the class, not the synthesized `new` sub) and rename-transport
  through. Two paths is a footgun; one path is correct.

#### Plugin-namespace bridges
- **`bridges_class`** — plugin_namespace → package. Visible from class
  and descendants. (Today: `Bridge::Class`.)
- **`bridges_bareword`** — plugin_namespace → bareword name. (Today:
  `Bridge::Bareword`.) Used for `app` in Mojolicious::Lite.
- **`bridges_variable`** — plugin_namespace → branded variable identity.
  See "Branded edges" below.
- **`bridges_file`** — plugin_namespace → file. Used for per-file
  singletons (the Mojolicious::Lite app case).

#### File attribution — `RoleMask` STAYS (the unification thesis's boundary)
**Correction (graph-walking-1, after porting the inheritance axis):**
this was an overreach. `refs_to`/`references_mask_for`/`group_refs`
ENUMERATE file tiers (open → workspace → dependency), collecting
matching refs from *every* file — a brute-force scan partitioned by
role. There is no origin and no edge to follow, so it is not a
`walk(origin, mask)`: forcing it needs a synthetic "root" node with a
`file_role` edge to every file, which still enumerates everything,
dressed as a graph. Zero benefit.

The only real graph version is an import-reachability OPTIMIZATION
(scan only files that transitively import the target via reverse
`uses_module` edges) — but that trades the completeness `refs_to`
exists for (re-exports, framework DSL, dynamic dispatch produce refs
with no clean import edge) for speed. Not a parity strangle.

So `RoleMask` is a PARTITION, not a traversal model, and stays as the
right abstraction. `walk` unifies the three edge-following models
(`package_parents`→INHERITS, `bridges`→BRIDGES, the lexical scope
tree→PARENT); file-role is orthogonal and out of scope. If the three
tier-iteration sites ever want DRYing, that's a shared
`for_each_file_in_mask` helper in resolve/file_store — not the graph.

### Branded edges (first-class)

A branded edge fires only when the walker's *context* matches the brand:

```rust
struct Edge {
    from: NodeId,
    to: NodeId,
    kind: EdgeKind,
    brand: Option<Brand>,
}

enum Brand {
    Variable { file: FileId, scope: ScopeId, name: String },
    File(FileId),
    Custom { kind: String, payload: serde_json::Value },  // for plugins
}
```

Concrete cases:

- **`my $minion = Minion->new(...)`** — entities of `$minion`'s plugin
  namespace bridge to brand `Variable { … "$minion" }`. A different
  Minion instance in the same workspace doesn't collide.
- **`my $app = Mojolicious::Lite->new`** — the app's plugin namespace
  bridges to brand `File(file_id)`.

### Brand resolution likely needs enrichment

The motivating case isn't `my $minion = Minion->new` — that's static and
straightforward. It's `$app->minion` returning a specific branded
instance, vs `$app->other_minion_for_those_jobs` returning a different
one. The brand isn't visible at the call site; it depends on what
`minion` returns, which depends on type inference + possibly cross-file
resolution.

Three timing options for brand resolution:

1. **Static at emission.** Works for `my $x = Class->new` and
   per-file singletons. Doesn't cover accessor-returned brands.
2. **Cross-file enrichment post-pass.** After type inference settles,
   walk method-call refs whose receiver resolves to a branded namespace
   type; materialize the branded edges. Same machinery as
   `enrich_imported_types_with_keys`.
3. **Lazy at query time.** Branded edges carry a "compute brand from
   context" closure. Walker evaluates per-query.

Plausible: **(1) for the cheap cases, (2) for the accessor-chain
cases.** (3) is tempting but reintroduces query-time logic that the
unification refactor is trying to eliminate.

The accessor-chain case (`$app->minion`) is genuinely hard and shouldn't
block the rest of the design. Land (1) first; tackle (2) when real
multi-instance Minion / multi-app cases come up.

## Architecture: graph lives in isolation

**The builder does not build the graph.** The builder stays focused on
CST → `FileAnalysis`. `src/graph.rs` consumes `&FileAnalysis` (+ the
`CrossFileLookup` trait) and answers `walk` queries.

**As built, the graph is NOT materialized — it is a lazy derived
view.** `GraphView` holds two borrows (`&FileAnalysis`, `Option<&dyn
CrossFileLookup>`) and nothing else: no node list, no adjacency map, no
build step. `walk` DFS-chases edges on demand — `edges_from` derives a
node's neighbors at the moment the walker asks, by calling the stores
that already exist (`parents_of`, `direct_children_of`,
`for_each_entity_bridged_to`). A walk materializes only its transient
traversal state (seen-set + per-node neighbor vec), discarded on
return. "Chasing an edge" *is* a map lookup against `package_parents`
— the same primitive the legacy walkers call, which is why a ported
consumer is provably parity (same lookups, one walker).

```
                  ┌─────────────────────┐
   CST  ──build──▶│   FileAnalysis      │  (today's data model)
                  │   scopes, symbols,  │
                  │   refs, owners,     │
                  │   plugin namespaces │
                  └──────────┬──────────┘
                             │  (borrowed; nothing built)
                  ┌──────────▼──────────┐
                  │  GraphView (lazy)   │  walk(origin, mask) →
                  │  edges chased       │  edges_from derives
                  │  on demand          │  neighbors per call
                  └──────────┬──────────┘
                             │
                       (queries via `walk`)
```

**`walk` is the caching seam.** Because every consumer goes through
`walk(origin, mask)` and never sees how edges are produced, caching
slots in behind `edges_from` with ZERO consumer changes — memoize
`(node, kind) → neighbors` on the view, or hang a longer-lived edge
cache off the index. Lazy is the default precisely so you don't pay
for that machinery until a profiler flags an edge kind. And it's
per-edge-kind, not all-or-nothing: `children_index` is already the
materialized form of the INHERITS_INV edge (the reverse walk was
expensive, so it's precomputed once), while INHERITS stays a cheap
lazy lookup — each edge kind picks lazy-vs-materialized on its own
cost, behind the same seam.

Consequences:

- Graph is independently testable: feed in a hand-built `FileAnalysis`,
  assert on the resulting graph shape.
- Builder stays the size it is; doesn't grow more responsibilities.
- Brand-enrichment is a clear secondary phase between graph construction
  and queries.
- Queries route through the graph; `FileAnalysis` stays the canonical
  data model. No bidirectional dependency.
- Migration is strangler fig: build the graph, port one query at a time,
  retire each old walker as its consumers move over.

## Open-extension hook

Edge kind enum has a `Custom { kind: String }` variant. Plugins emit
edges with their own kinds; queries that care match on the string;
native code paths match on the enum variants. Brand kinds similarly
extensible (`Brand::Custom`).

This is the same pluggability question the original Namespace enum
faced — but at the edge level, where it's actually load-bearing.
Frameworks add new visibility relationships (`Catalyst::Controller →
Catalyst::App`, etc.) without recompiling the host.

## Query model

```rust
fn walk(
    graph: &Graph,
    origin: NodeId,
    edge_kinds: EdgeKindMask,
    brand_ctx: &BrandContext,
) -> impl Iterator<Item = NodeId>;
```

`BrandContext` carries everything brand-match needs: type-inference
results, current file id, the receiver variable for the calling
expression. Walker invokes the brand-match closure when it encounters a
branded edge.

Today's resolvers re-implement themselves as `walk` calls with
different masks:

| Today | Tomorrow |
|---|---|
| `resolve_method_in_ancestors` | `walk(class_scope, INHERITS)` |
| `for_each_entity_bridged_to(class)` | `walk(class_scope, BRIDGES_*)` with brand match |
| `resolve_symbol(name, from, VISIBLE)` | `walk(from_scope, VISIBILITY \ EDITABLE.invert())` |
| Cursor-context scope walks | `walk(cursor_scope, PARENT \| BINDS_*)` |
| `enrich_imported_types_with_keys` | brand-resolution post-pass |

## Migration shape

Strangler fig, lasts a while. Phases:

1. **Stand up `Graph` as a parallel structure.** ~~Populated alongside
   today's data; not yet queried.~~ **LANDED (graph-walking-1)** as a
   DERIVED view, not parallel storage: `src/graph.rs` `GraphView` +
   `walk(origin, EdgeKindMask, visit)` — visit-at-pop DFS preserving
   @ISA order, seen-set, the legacy depth cap. Edge derivation is one
   function (`edges_from`): INHERITS via `parents_of` (the shared
   injection site, so graph and legacy walkers cannot disagree),
   INHERITS_INV via the children index (`direct_children_of`, depth 1
   — the walker supplies transitivity), BRIDGES via
   `for_each_entity_bridged_to` (terminal Module nodes; composes with
   INHERITS through the synthetic app-surface edge, replacing the
   ancestor+bridge two-step). First ported consumer:
   `resolve.rs::implementations_of` (the requires→composers fan-out)
   — its gold rows are the parity net.
2. **Port one query at a time**, retiring old data structures as their
   last consumer moves over:
   - `resolve_method_in_ancestors` first (simplest, smallest blast
     radius).
   - Then `for_each_entity_bridged_to` (needs branded edges working).
   - Then cross-file `resolve_symbol` (needs file-role edges).
   - Then cursor-context scope walks (needs full lexical edges).
   - Then enrichment becomes brand-resolution.
3. **Retire `package_parents`, `RoleMask`-as-separate-concept,
   `PluginNamespace.bridges`, `imports`/`framework_imports`/
   `imported_hash_keys` field-by-field** as they become unused.

Type-inference work doesn't pause. Witness reducers that need scope info
can keep using today's helpers until their query path is ported; the
graph-walk version is a drop-in replacement when ready.

## What this retires

- `package_parents: HashMap<String, Vec<String>>` → typed `inherits`
  edges.
- `PluginNamespace.bridges: Vec<Bridge>` → typed `bridges_*` edges.
- `RoleMask` (as a separate concept) → file-edge-kind mask.
- `imports`, `framework_imports`, `imported_hash_keys` → typed edges.
- The bespoke walkers in `resolve_method_in_ancestors`,
  `for_each_entity_bridged_to`, `cursor_context.rs` scope chains,
  `resolve.rs` tier walking → one `walk` call.

## What this unblocks

- **Openness diagnostic** (absorbed residuals, below).
  Walking the namespace chain to ask "does this terminate Closed?"
  becomes one `walk` call with an `inherits | imports | bridges` mask.
- **Multi-app Mojo support** (from `prompt-mojo-todo.md`). Two
  Mojolicious::Lite apps in one workspace stop colliding once branded
  edges work.
- **`home_namespace` move** (absorbed residuals, below).
  Once nodes are typed, `Symbol.package: Option<String>` becomes
  `Symbol.home_scope: NodeId`.
- **Cleaner type-inference scope queries.** `query_variable_type`'s
  scope chain walk goes through `walk` instead of duplicating logic.

## `stack-graphs` upstream — evaluate, don't auto-adopt

`stack-graphs` (the GitHub Rust crate) is the closest existing
implementation of this model. Pros: proven path semantics, well-tested,
tooling. Cons: their model is general; some Perl-specific semantics
(plugin namespaces, framework synthesis) require restating builder
emission rules in their pipeline.

Decision deferred until the edge taxonomy here stabilizes against real
code. Adopting the model is the load-bearing decision; using their
crate vs rolling our own typed-edge graph + walker is a tactical call.
A custom impl is ~few-hundred-lines and integrates cleanly with the
existing builder + witness/reducer pipeline. The crate buys path
semantics + tooling at the cost of a TSG-shaped emission boundary.

> Note: TSG (the tree-sitter graph DSL stack-graphs originally used) is
> dead. If we go with `stack-graphs`, the Rust-API construction path is
> what we'd use, not TSG.

## Open questions

### `local` semantics + async boundaries

`local $x = ...` masks the package binding for the *dynamic extent* of
the lexical block. For static analysis we approximate as lexical
shadowing — close enough for navigation; type-inference inside the
block sees the new binding.

The async-boundary case is real: `local` across `await` / event loop
boundaries doesn't behave intuitively. `Syntax::Keyword::Dynamic`
solves this at runtime; we'd want to model it for static analysis too.

Plausible shape: an "escape" edge or a `binds_dynamic` subkind that
flags "this binding crosses async boundaries; assume re-entry from any
outer dynamic context." Doesn't need to be precise; just needs to not
silently lie about visibility.

**Open for discussion.** Land the rest of the graph first; revisit
when async-flavored Perl shows up in real workloads.

### Brand resolution for accessor chains

Static-at-emission covers `my $x = Class->new`. Cross-file enrichment
covers `$app->minion` once type inference resolves the accessor's
return type to a branded namespace. The enrichment pass needs to be
careful about ordering: type inference must settle before brand
resolution, but graph queries needed by *intermediate* type-inference
steps can't yet see branded edges. Shouldn't be a problem in practice
(type inference doesn't typically resolve through plugin bridges), but
worth pinning when the implementation lands.

### `Custom` edge / brand kinds — wire format

If plugins emit custom edges via Rhai, we need to define the wire
format (string kind + serde JSON payload, probably). Same shape as
`EmitAction::Custom` would be — the existing plugin emission ABI
extends naturally.

## Out of scope

- MRO/C3 method resolution order.
- `wantarray` / context-dispatched returns (different layer; lives in
  type inference).
- Cross-workspace graph (monorepo multi-root).
- Effect tracking on edges (which edges mutate state, throw, etc.).
- Runtime-dynamic graph mutation (e.g. `*Foo::bar = sub { ... }`
  installing a method at runtime). Static-only.

## Absorbed residuals (from the retired unification doc)

**Openness classification (was phase 6).** Unresolved-* diagnostics
either over-fire (roles, plugins, abstract bases) or under-fire
(framework-imported names). One derived-never-stored rule:
`enum Openness { Closed, Open }` per namespace —
`use Moo::Role`/`Role::Tiny` → Open; plugin-shaped `Mojo::Base
'Mojolicious::Plugin'` → Open; an app with `sub startup` → Closed;
controllers with routes targeting them → Closed; referenced-as-parent
but never instantiated → Open; scripts → Closed; default Closed.
Diagnostic rule: walk upward from the ref's namespace via `inherits |
imports | bridges`; unresolved at a Closed termination → warn; an Open
node on the way → suppress. Replaces the bespoke `framework_imports`
suppression and covers unresolved function/method/stash-key/route-
target/helper uniformly.

**`Symbol.home_namespace` (was phase 7).** Once nodes are typed,
`Symbol.package: Option<String>` becomes `Symbol.home_scope: NodeId`,
and `Bridge::Class(...)` becomes a bridge to a framework node directly
(no more routing app surfaces through a controller class name).

**Eager `Ref.target` (was phase 5, mostly dissolved).** Lazy TREE
resolution is dead (the one behavior it carried — ctor-key → Corinna
field — became eager `HashKeyDef` synthesis); MethodCalls carry
`resolved_method_target`, FunctionCalls `resolved_package`, variables
`resolves_to`. What remains is the field SHAPE: `resolves_to:
Option<SymbolId>` → non-optional `target` with an `UNRESOLVED`
sentinel, which is what the Openness rule's "unresolved bucket" reads.
Do it as part of this rework — a ref's target is the walk's output.

**The pluggability question (was phase 1's blocker) is this graph's
node-identity question.** Enum-only nodes (exhaustive, recompile per
framework) vs `Custom { kind, name }` (open, loses exhaustiveness) vs
hybrid. The plugin-system's trajectory (manifests over enums —
`role_makers`, `app_surface_consumers`) argues hybrid with the open
arm as the default for plugin-declared scopes.

## The ancestry-axis strangle — design space (the #3 decision)

The strangle table above listed "bridges" and "ancestry" as separate
steps. Porting the real consumers proved that wrong: **they're one
strangle.** `resolve_method_in_ancestors`, `for_each_dispatch_handler_
on_class`, `class_isa`, `class_has_unresolved_ancestor`,
`resolve_super_method`, `method_rename_chain`, `collect_ancestor_
methods`, `complete_methods_for_class` — all ~8 — funnel through ONE
function, `for_each_ancestor_class`, and each pulls local symbols +
bridged entities (the BRIDGES axis) at every class the INHERITS walk
visits. `for_each_entity_bridged_to` is the bridge *primitive* (the
graph's BRIDGES edge derivation already calls it), not a consumer to
strangle. So:

> The ancestry funnel IS the strangle. Port `for_each_ancestor_class`
> to delegate to `walk(class, INHERITS)` and all 8 consumers come
> along; their bridge/local pulls stay in their visit closures,
> untouched.

### The enabler decision (resolved)

Both this and every model-internal walk were gated on a layering
wall: the consumers live in `file_analysis.rs` (Model); `walk` lived
in `graph.rs` (Index); Model can't import up. Two ways out:

- **(A) Move the graph DOWN to Model.** `graph.rs` imports only
  `file_analysis` (the `CrossFileLookup` trait + `parents_of`) — zero
  Index deps. It was *misfiled* at Index in phase 1. Reassigning it to
  Model (one line in `layer_map`) lets every model walker call `walk`
  directly; graph and `file_analysis` mutually reference at the same
  layer, which the DAG test permits (only `target_layer > my_layer` is
  a violation). Tiny, no function relocation.
- **(B) Move the ancestry QUERY functions UP** out of `file_analysis`
  into a query/Index layer. Aligns with the arch-review note that
  these `&dyn CrossFileLookup`-taking functions were misfiled in the
  model — but it's a wide mechanical relocation of ~8 functions + all
  callers, large risk surface.

**Chosen: (A).** Landed. `class_isa` is the first consumer ported as
proof (`class_isa_matches_legacy_ancestor_walk` pins parity). (B) stays
available as an independent later cleanup if the model gets fat.

### What the funnel port still needs

1. **`skip_self` — DECIDED: no such parameter. `walk` is edge-only;
   callers add the reflexive check.** A walk discovers what you
   *reach* by following edges; the origin is the thing you already
   *have*, so yielding it conflates the two. Decisive reason it can't
   be a `walk` option anyway: the self-handling is CONSUMER-SPECIFIC —
   `class_isa` checks `self == ancestor`, `resolve_method_in_ancestors`
   checks `method_resolution_on_class(self)`, `collect_ancestor_
   methods` gathers self's methods, `class_has_unresolved_ancestor`
   checks self's parents. `walk` couldn't handle self without calling
   back into per-consumer logic, which is just the consumer doing it.
   So: include-self consumers prepend their own one-line check (as
   `class_isa` did); the old `skip_self=true` variant
   (`resolve_super_method` — SUPER searches parents only) becomes the
   BARE `walk(enclosing, INHERITS)`, no flag. The seen-set already
   seeds the origin (cycle-safe for both shapes), so nothing else
   changes. `for_each_ancestor_class_opt`'s bool is deleted, not
   ported.
2. **Visit signature.** `for_each_ancestor_class` yields `&str` class
   names. `walk` yields `&Node`. The funnel port maps `Node::Class(c)
   → c`; Module nodes (from a combined BRIDGES mask) are filtered or
   handled per consumer. Keep the funnel INHERITS-only; the bridge
   pull stays in each consumer's closure (no behavior change).
3. **Parity proof = the existing net.** The 8 consumers drive
   inheritance/dispatch/SUPER/role e2e + gold rows. Port the funnel,
   run them; a parity unit test per consumer is cheap insurance for
   the hot ones (`resolve_method_in_ancestors`).

### Remaining strangles after the funnel

- **lexical scope chains** (`cursor_context`) — needs Scope nodes +
  `PARENT`/`BINDS_*` edges. New taxonomy. This IS a genuine
  walk-from-origin (cursor scope → enclosing scopes until a binding),
  so it fits `walk`; the cost is adding the node/edge kinds.
- **branded bridges** (`$minion`/`$app` instance identity) — branded
  edges, the design-heavy finale.
- ~~file-role tier~~ — REMOVED: enumeration, not a walk (see "File
  attribution" above). `RoleMask` stays.

Each grows the node/edge set additively. The migration ends when
`package_parents` and `PluginNamespace.bridges` have no direct
consumers — every *reachability* question is a `walk` with a mask.
(`RoleMask` is not in that set; it partitions files, it doesn't
traverse.) Openness diagnostic + multi-app Mojo fall out then.
