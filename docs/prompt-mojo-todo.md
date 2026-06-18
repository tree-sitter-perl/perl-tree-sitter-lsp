# Mojo Intelligence: What's Left

> Mojo support is plugin-driven (`frameworks/mojo-*.rhai`). The original
> mojo-intelligence spec proposed a framework-Namespace emission
> architecture; the plugin path replaced it. This doc lists the Mojo
> features still missing from the bundled plugins. New Mojo features
> usually land as plugin patches.
>
> See `adr/plugin-system.md` for the host machinery and
> `docs/PLUGIN_AUTHORING.md` for how to add hooks.

## What works today (via plugins)

| Feature | Plugin |
|---|---|
| Route → Controller#Action goto-def | `mojo-routes.rhai` |
| Helper completion + goto-def | `mojo-helpers.rhai` |
| Minion task registration + enqueue sig help | `minion.rhai` |
| EventEmitter `on`/`emit`/`once` | `mojo-events.rhai` |
| Mojolicious::Lite DSL imports + routes (incl. under/group nesting) | `mojo-lite.rhai` |
| Plugin-name goto-def (`plugin 'Thing'` → register) | `mojo-lite.rhai` / `mojo-routes.rhai` |
| Framework-assigned attr typing (`$app`, `$tx`, `$c->req/res`) | `mojo-routes.rhai` |
| DefaultHelpers/TagHelpers discovery (framework-implied SyntheticUse) | core `process_use` + plugins |
| Data::Printer use-line completion | `data-printer.rhai` |

## What's missing

### Route naming + `url_for`

```perl
$r->get('/users/:id')->to('users#show')->name('show_user');
$c->url_for('show_user', id => 42);
```

Need: a `MojoRouteName` registry (accumulate from `->name('…')` and Lite
auto-naming). Completion inside `url_for('…')` / `redirect_to('…')` offers
known route names. Goto-def from name → route definition.

Implementation: extend `mojo-routes.rhai` to emit a Handler keyed by route
name at `->name(…)` chain links, plus `MethodCallRef`s at `url_for` /
`redirect_to` argument spans.

### Stash intelligence

```perl
# app / startup (route declarations)
$r->under('/admin')->to('users#', layout => 'admin');   # inherited default
$r->get('/list')->to('#list', title => 'Users');        # + local default

# Users.pm (controller body)
sub list {
  my $c = shift;
  $c->render(template => 'users/list', count => 5);      # render also stashes
  $c->stash('layout');        # read  — should complete: layout, title, count
  $c->stash->{title};         # read  — same key set
}
```

Goal: completion / hover / goto-def on stash keys at `$c->stash('…')`,
`$c->stash->{…}`, and the `render(k=>v)` tail.

**Decision — stash keys belong to the brand and land per-action.**
Not per-controller (`HashKeyOwner::Class(controller)` over-broadens: two
actions in one controller reached through different `under` branches have
different in-force stash, e.g. `users#list` under `/admin` sees `layout`
but `users#show` does not). The authoritative "what stash is in force
here" is the accumulated `BrandedRoute.stash` (`adr/route-branding.md`) at
the route value that targets the action — so the key set is per-action,
sourced from the brand.

**Sources of keys (emission, route-declaration side):**
- `->to('c#a', k => v)` and `->to(controller => …, action => …, k => v)`
  route defaults. The brand already accumulates these — at each *terminal*
  `->to` (one that names an action), the in-force `stash` (inherited
  overlay + local) is that action's default set. We read the pairs with
  the shared `classified_pairs` + `value_shape` now that the `to` work
  landed; the keys we want are exactly the non-`controller`/`action` ones
  `merge_to_defaults` already drops into `BrandedRoute.stash`.
- `render(k => v)` / `stash(k => v)` *inside* the action body — action-
  LOCAL keys (not inherited via the brand; transient per request). Detect
  the fat-comma tail after the known render options (`template`/`format`/
  `status`/`handler`/…). These belong to the same per-action set but are
  defined at the call site in the controller file, not the route.

So an action's key set is the union of two origins: brand-inherited
(declared in the app file, flows through `under`/`->to`) and body-local
(`render`/`stash` in the controller file).

**Identity + the cross-file bridge.** The action is `Controller#action`.
On the read side, inside `sub action` the identity is
`<current_package>#<enclosing_sub_name>`, and `current_package` must map
to the route's controller token through the SAME decamelize + namespace
resolution goto-def already uses (`mojo-routes` controller resolution) —
otherwise the body-side `users#list` won't meet the decl-side
`MyApp::Controller::Users#list`. `route_emissions` already mints a Handler
named `Controller#action` bridged to `Mojolicious::Controller` and a
per-package `PluginNamespace`; stash defs ride the same bridge so the
controller body (another file) finds keys declared in the app file via
`for_each_entity_bridged_to`.

**Ownership shape — pick one (the fork the old note hand-waved):**
- (a) new `HashKeyOwner::MojoAction { class, action }` — explicit, lets
  `HashKeyAccess` on `$c->stash->{k}` resolve through the normal owner
  path; or
- (b) a per-action `PluginNamespace` (`mojo-routes:<pkg>#<action>`)
  holding the keys as entities, consistent with how routes are namespaced
  today. (a) is better for the `->{k}` deref read (owner-keyed
  `HashKeyAccess` is the existing machinery); (b) is better for the
  `stash('…')` string-arg completion (namespace enumeration). They are not
  exclusive — emit the `HashKeyDef`s with an action owner AND register the
  namespace bridge for enumeration.

**Read-side consumers (the part actually missing):**
- `$c->stash('|')` — string-arg completion: a `MojoBranded`/query hook
  enumerating the current action's key set (brand-inherited ∪ body-local).
- `$c->stash->{|}` — type `stash()`'s return as a hash whose keys are the
  action's set (or resolve the `HashKeyAccess` owner to the action), so
  hash-key completion + goto-def to the defining `->to`/`render` work.
- hover on an in-force key showing where it was set (which `->to`/`render`).

**Ready vs missing.** Ready: `BrandedRoute.stash` accumulates through all
chain edges; `classified_pairs`/`value_shape` read the keyvals; the
`Controller#action` Handler + bridge exist. Missing: (1) emit per-action
stash `HashKeyDef`s from the brand at terminal `->to` + from body-local
`render`/`stash`, owned per-action; (2) the controller-token↔package
identity match on the read side; (3) the two completion/deref consumers.

Out of scope here (same dark edge as branding): an action whose route
chain roots at an unbranded hashref-param boundary never accumulates a
brand, so its inherited stash stays empty — body-local `render`/`stash`
keys still resolve.

### Hook completion

```perl
$app->hook(before_dispatch => sub { ... });
```

Static set of hook names — pre-seed as completions inside `->hook('…')`.
Per-hook handler signatures (`before_dispatch($c)` vs `around_dispatch($next, $c)`
vs `before_server_start($server, $app)`) are the complete table:

| Hook | Params |
|---|---|
| `before_dispatch` / `after_dispatch` / `before_routes` / `after_static` | `($c)` |
| `around_dispatch` | `($next, $c)` |
| `around_action` | `($next, $c, $action, $last)` |
| `before_server_start` | `($server, $app)` |
| `after_build_tx` | `($tx, $app)` |

Plugin shape: a new `mojo-hooks.rhai` (or extend `mojo-helpers.rhai`) with
`on_completion` claiming the cursor inside `->hook('|')` and returning the
known set, plus `on_signature_help` for the handler-sub argument shape.

### Plugin chain — transitive helper discovery

`$app->plugin('Authentication')` loads
`Mojolicious::Plugin::Authentication`. The plugin's `register()` method
calls `$app->helper(...)` — those helpers should appear in the host's
controllers.

Short-name resolution + goto-def LANDED (crm QA sweep), and `plugin
'X'` SyntheticUses the plugin module so its register-time helpers
resolve. What's left is TRANSITIVE chains: plugin A's register loading
plugin B — B's helpers should reach the host. Needs the chain followed
one more hop at resolve time.

### Config completion (stretch)

```perl
my $host = $app->config->{db}{host};
```

If a `.conf` file exists in workspace, parse it and offer its keys as
completions on `$app->config->{...}`. Reuses the hash-key completion
machinery; the new bit is the `.conf` parser.

## Cross-cutting: multi-app workspaces

Two `Mojolicious::Lite` apps in one workspace today fold into one set of
helpers/routes (helpers attach to `Mojolicious::Controller`, routes to
the literal package). The right shape is `PluginNamespace` with bridges
— see `adr/plugin-system.md`. Not blocking for any single feature
above; a quality bar to revisit once two-app cases come up in real
usage.
