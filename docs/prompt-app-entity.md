# A fictional "app surface" entity for Mojolicious helpers/plugins

> Helpers conceptually belong to the **app**, not to a controller. Today
> `mojo-helpers` makes them visible by bridging each helper to *both*
> `Mojolicious` and `Mojolicious::Controller` — a working approximation, but the
> consumer set is hardcoded per helper. This note designs the cleaner model the
> approximation is standing in for: a single open "app surface" entity that
> plugins contribute to and a declared set of receiver types compose.

## The shape of the problem (from the source)

`Mojolicious` (the app) and `Mojolicious::Controller` both `use Mojo::Base -base`
— **siblings**, no app-specific common ancestor. Helpers physically live on the
app's renderer (`sub helpers { $_[0]->app->renderer->get_helper('')->(...) }`),
and a controller reaches them via `->app`. At runtime Mojolicious installs each
helper as a method on the controller class *and* makes it callable on the app.
So a helper is reachable from: the app (`$app->h`), any controller (`$c->h`),
the app subclass the user wrote, commands (`$cmd->app->h`), Minion jobs
(`$job->app->h`), etc. — a set of receivers with no single shared class to hang
the helpers on.

## Today: bridge to both entry classes

`mojo-helpers` emits one `Method` (owned by `Mojolicious::Controller`) plus a
`PluginNamespace` whose `bridges` list is `[Mojolicious::Controller,
Mojolicious]`. `for_each_entity_bridged_to(C)` finds the helper when resolving a
method on either class; a Mojolicious *subclass* app inherits the `Mojolicious`
bridge through the normal ancestor walk. This works and is why `$app->minion`
resolves once `$app` is typed.

Its limit is **openness on the consumer side**: the set of receiver classes that
can see helpers is baked into every helper's `bridges` list. Add a new consumer
(`Mojolicious::Command`, a house controller base that isn't a Mojo subclass,
`$job->app`) and you edit the bridge list in every helper-emitting plugin. The
contribution side is already open (any plugin's `register` adds helpers, and
`bridges_index` aggregates them cross-file); the consumption side is not.

## The model: one open "app surface", composed by a declared receiver set

Introduce a fictional entity — call it the **app surface** (a synthetic
namespace, e.g. `Bridge::App` / PluginNamespace kind `app`). Then:

- **Contribution (open):** every `$app->helper(name => …)` — from any plugin,
  any file — bridges the helper to the *app surface*, not to concrete classes.
  `bridges_index` already unions these cross-file, so the surface is the
  aggregate of every plugin's helpers automatically.
- **Consumption (open, declared once):** the receiver types that can see the
  app surface are declared in **one** place, not per-helper. The cleanest
  encoding is a **synthetic ancestor edge**: `Mojolicious`,
  `Mojolicious::Controller` (and `Mojolicious::Command`, …) gain the app surface
  as a synthetic parent in `package_parents`. Then the *existing* ancestor walk
  + bridge resolution finds helpers with zero new resolver logic — and a
  Mojolicious-subclass app or a custom controller base inherits it for free.
  Adding a consumer = one synthetic-parent edge; adding a helper = one bridge to
  the surface. The two axes are decoupled.

This is what "plugins should belong to a fictional app entity" means concretely:
the surface is the entity; plugins extend it; a declared receiver set composes
it; no concrete class has to *own* the union.

## Why "openness" is the crux

The entity only composes correctly if it's open on **both** axes:

1. **Open contribution** — N plugins each add helpers; the entity is their union.
   (Have this: `PluginNamespace` + `bridges_index`.)
2. **Open consumption** — N receiver types see the union, declared once and
   extensible. (Missing: today the receiver set is the hardcoded 2-class bridge
   list copied into each helper.)

A closed class can't be the home because (a) no real class is an ancestor of all
the receivers, and (b) helpers arrive from many plugins after the class is
"defined". The entity must be an *open namespace*, which is exactly the
`PluginNamespace` primitive — just pointed at a fictional app surface instead of
two concrete framework classes.

## Multi-app caveat (deferred, same as routes)

One global app surface means every helper is visible from every controller/app
in the workspace — fine for the overwhelmingly common single-app project, wrong
for a workspace with two apps (helpers leak between them). This is the identical
scoping limitation already documented for the routes namespace
(`mojo-routes.rhai`: "Not yet app-scoped"). Per-app surfaces keyed by the app
class are the eventual refinement; not worth it until a real two-app workspace
shows up.

## Relationship to the immediate work

- **`$app->minion` doesn't need this.** The 2-class bridge already covers
  `Mojolicious`; typing `$app` (via `param_types`, see
  `docs/prompt-param-typing.md`) is the only missing piece. Ship that first.
- The app surface is a **cleanliness / extensibility** refactor: it replaces the
  hardcoded 2-class bridge list with one open entity + a declared consumer set,
  so future consumers (Commands, `$job->app`, house bases) and future app-level
  synths (plugin-registered routes, app config accessors) get a single home.

## Open questions

- Does the synthetic-ancestor encoding interact cleanly with the real MRO walk
  (`resolve_method_in_ancestors`, depth cap, cycle guard)? A fictional parent
  that itself has no parents should be inert beyond contributing its bridge.
- Should the surface also own things beyond helpers — `$app->routes`,
  `$app->config`, plugin-registered commands — i.e. become *the* model of "the
  app", or stay helper-scoped? (Leaning: start helper-scoped; let it grow only
  if a second app-level surface need appears, to avoid a speculative god-entity.)
- Naming + namespace `kind`: `app` surface vs reusing the existing
  `Bridge::Class` machinery with a fictional class name. Prefer whatever keeps
  `for_each_entity_bridged_to` unchanged.

## Landed

The synthetic-ancestor encoding (above) shipped:

- **One bridge target.** `mojo-helpers.rhai` bridges every helper to a single
  fictional class `file_analysis::APP_SURFACE_CLASS`
  (`Mojolicious::_AppSurface`) — never a real package, no parents, inert in the
  walk beyond contributing its bridge. The per-helper 2-class bridge list is
  gone.
- **Open consumer set via a manifest.** `FrameworkPlugin::app_surface_consumers()`
  (default empty; registry union; Rhai array-of-strings reader — same
  declarative family as `overrides()` / `dispatch_verbs()` / `param_types()`)
  declares the receiver classes. `mojo-helpers` declares
  `[Mojolicious::Controller, Mojolicious]`. The Mojo names live in the plugin,
  not in core (no `APP_SURFACE_CONSUMERS` const). The builder bakes the union
  onto `FileAnalysis.app_surface_consumers` so query-time walks read it without
  re-touching the registry; it rides the cache blob (`#[serde(default)]`).
- **Single edge-injection seam.** `file_analysis::parents_of(class,
  package_parents, module_index, consumers)` is the ONE place the synthetic
  `APP_SURFACE_CLASS` parent is appended. All three parent-enumeration sites —
  `for_each_ancestor_class`, `collect_ancestor_methods`, and the `MethodOnClass`
  inheritance walk in `witnesses.rs` — route through it, so MRO rules and the
  edge can't drift. `BagContext` carries `app_surface_consumers` so the
  bag-side walk has the same set.

The open questions resolved as expected: the fictional parent is inert (no
parents, bounded by the existing seen-set + depth cap), and the surface stays
helper-scoped. The multi-app caveat is unchanged (one global surface).
