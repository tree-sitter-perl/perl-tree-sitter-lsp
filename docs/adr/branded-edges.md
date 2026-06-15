# ADR: Branded edges — instance-scoped plugin visibility

Plugin-synthesized content (Mojo helpers, Minion tasks) reaches Perl
space through a `Bridge` to a class. Two receivers of the SAME class
therefore saw the SAME content — even when they are distinct instances.
The sharp case: two self-contained `Mojolicious::Lite` apps in one
workspace both run in `main` and bridge their helpers to the single
`Mojolicious::_AppSurface` class, so app-two resolved app-one's helpers.
A "branded edge" fires only when the query's brand context matches the
namespace's brand, so distinct instances stay distinct.

## The shape: brand on the namespace, not the bridge or the type

The brand is `PluginNamespace.brand: Option<String>` — instance identity
lives on the namespace, because a namespace already *is* one instance's
content. `None` = global (the pre-brand default: visible everywhere).
`Some(b)` = scoped. The additive rule lives in exactly one method,
`PluginNamespace::visible_under(query_brand)`:

| namespace.brand | query_brand | visible |
|---|---|---|
| `None` (global) | anything | yes |
| `Some(b)` | `None` (agnostic) | yes — sees everything, no regression |
| `Some(b)` | `Some(q)` | `b == q` |

Consumers ask the namespace; they never re-derive the rule from the
field (rule #10). Three placements were prototyped in a bake-off
(`docs/prompt-graph-walking.md` history): brand on the `Bridge` enum
variant, brand on `InferredType`, brand on the namespace. The namespace
won: zero bincode-variant churn (a `#[serde(default)]` field, not a new
enum variant — old blobs deserialize `None`), and the same-file
consumer gating falls out cleanly because the namespace is already in
scope at every `for ns in &self.plugin_namespaces` site.

## Brand is walk context, not an EdgeKind

The query's brand rides as *context* — `GraphView::new_branded(.., brand)`
and `BagContext.home_brand` — threaded into the single BRIDGES edge
derivation and the cross-file `for_each_entity_bridged_to_branded`
primitive (the unbranded form is its `None`-brand case). No `EdgeKind`
was added: the closed `{Inherits, InheritsInv, Bridges}` set and its
lockstep test are untouched. Brand answers "does THIS bridge fire in
THIS walk", orthogonal to "what KIND of edge is this". The inheritance
edges are brand-agnostic, so the existing walk call sites are byte-identical.

**The walk/primitive is the only brand filter (no-double-filter
invariant).** A module node is reachable iff a namespace was
brand-compatible, so a consumer reads the reached entities raw and must
NOT re-apply `visible_under` with a second (possibly wrong) context —
that silently drops branded content. The bake-off's BETA spike hit this
live; it is now a documented invariant on the primitive.

## Producer and consumer agree by file identity

The brand is assigned at **registration**, not build — the builder never
sees a path, and instance identity is a property of where a file lives.
`FileAnalysis::apply_home_brand(file_id)` (file_id = canonical path)
sets `home_brand` and stamps the same id onto the file's namespaces,
**only for a self-contained Mojo::Lite app**. That restriction is what
makes it regression-free: it's the one shape where the brand's CONSUMER
(`$self->helper` queries in the file) and PRODUCER (the app's helper
namespaces in the file) are the same file, so a per-file brand both
exists and matches. Everything else (full Mojo with separate controller
files, non-Mojo) stays `home_brand = None` → agnostic → sees every
helper exactly as before. Applied at the open-doc enrich path
(`backend.rs`) and the workspace/dep build path (`module_resolver.rs`).

The consumer brand is a property of the querying `self` — every
visibility site reads `self.query_brand()`, so no brand parameter
threads through the query API. Cross-file recursion in the witness bag
carries the *origin's* brand (`BagContext.home_brand`), not the visited
file's.

## Per-variable instances (landed for owner-keyed dispatch)

`my $a = Minion->new; $a->add_task(ta); my $b = Minion->new;
$b->add_task(tb); $a->enqueue('tb')` — `tb` (registered on `$b`) must
NOT dispatch on `$a`. Minion tasks resolve via owner-keyed Handlers
(`handlers_for_owner`), not namespaces, so the brand lives on the
**Handler** (`SymbolDetail::Handler.instance_brand`) and shares the rule
via the free `brand_visible(item, query)` (which `visible_under` also
delegates to).

The brand source is the **receiver variable's declaration identity** —
`instance_brand_at(receiver, point)` resolves a plain lexical scalar to
its `my` decl and keys on the decl's location (`inst:$a@row:col`). It is
computed identically at:

- **emission** (build time, `Builder::assign_task_instance_brands`, after
  `resolve_variable_refs` so the decl is reachable) — the plugin threads
  the raw receiver text on `EmitAction::Handler.receiver_text`, core
  resolves it and stamps the handler;
- **query** — the brand-aware DispatchCall→Handler pairing
  (`dispatch_ref_receiver_brand`, goto-def + references) and the
  completion filter (`receiver_brand_at`, `dispatch_target_items_for`).

`$self` / `__PACKAGE__` / a chain / an undeclared name resolve to `None`
→ the task stays global and dispatch keeps its pre-brand behavior. The
brand key is same-FILE (a lexical decl's location), so cross-file
handlers are left unfiltered — instance identity doesn't cross the file
boundary, and same-file row:col keys would otherwise risk a collision.

## Boundaries — what this does NOT do yet

- **Accessor chains** (`$app->minion`) need the brand derived from what
  the accessor returns — lazy-at-query-time from the resolved invocant,
  NOT a cross-file enrichment post-pass (enrichment is open-documents-
  only and would re-merge on workspace/dependency files).
- **Full Mojo multi-app** (helpers on an app class, consumed by separate
  controller files) stays merged — the consumer file can't carry the
  app's brand. Documented degradation, not a regression.

The graph's branded BRIDGES walk (`new_branded`) is the forward seam for
a BRIDGES-walking consumer; production bridge resolution currently goes
through the direct primitive, which carries the same one filter. The
graph tests pin that the two paths agree (R3).
