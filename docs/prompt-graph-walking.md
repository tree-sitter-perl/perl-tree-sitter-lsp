# Graph Walking: One Typed-Edge Graph, One Walker

> **Scheduled after the type-inference triplet ships.** Type intelligence
> is the priority — the lights-off problem in real Perl projects. Graph
> rework is the next architectural pillar; type inference benefits from
> a cleaner graph but doesn't block on it.
>
> Cross-refs: `adr/file-store-and-resolve.md` (today's `RoleMask` model
> that this retires); `adr/plugin-system.md` (today's `PluginNamespace`
> + `Bridge` that becomes typed edges); `prompt-unification-residual.md`
> (Phase 6 Openness + Phase 7 home_namespace move depend on this).

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

#### File attribution (replaces `RoleMask` as an edge concept)
- **`file_role`** — file edges tagged: `open` | `workspace` |
  `dependency` | `builtin`.

The "files are special" abstraction goes away. `RoleMask::EDITABLE`
becomes "edge-kind mask: don't traverse `dependency`-tagged file edges."
Same walker, kind-mask varies. The `RoleMask` type can survive as a
convenience constructor.

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
CST → `FileAnalysis`. A new module (call it `src/graph.rs` or
`src/scope_graph.rs`) consumes `&FileAnalysis` and produces the graph as
a derived structure.

```
                  ┌─────────────────────┐
   CST  ──build──▶│   FileAnalysis      │  (today's data model)
                  │   scopes, symbols,  │
                  │   refs, owners,     │
                  │   plugin namespaces │
                  └──────────┬──────────┘
                             │
                  ┌──────────▼──────────┐
                  │   Graph (derived)   │  (typed-edge graph)
                  │   nodes, edges,     │
                  │   brand index       │
                  └──────────┬──────────┘
                             │
                       (queries via `walk`)
```

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

1. **Stand up `Graph` as a parallel structure.** Populated alongside
   today's data; not yet queried.
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

- **Phase 6 Openness diagnostic** (from `prompt-unification-residual.md`).
  Walking the namespace chain to ask "does this terminate Closed?"
  becomes one `walk` call with an `inherits | imports | bridges` mask.
- **Multi-app Mojo support** (from `prompt-mojo-todo.md`). Two
  Mojolicious::Lite apps in one workspace stop colliding once branded
  edges work.
- **Phase 7 `home_namespace` move** (from `prompt-unification-residual.md`).
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
