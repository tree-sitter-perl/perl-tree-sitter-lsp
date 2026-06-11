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
$c->stash(users => $list, title => 'Users');
$c->render(template => 'users/list', count => 5);
$c->stash('users');           # read
$c->stash->{count};           # read
```

Need: stash-key registry per controller / action. Completion inside
`$c->stash('…')` and `$c->stash->{…}`. Detection covers `stash(k=>v)`,
fat-comma pairs in `render(k=>v)` after known render options, fat-comma
pairs in `->to('c#a', k=>v)` route defaults.

`BrandedRoute` already carries an arbitrary `stash: Vec<(String,
String)>` through `under`/`->to` chains (`adr/route-branding.md`) — a
hover/completion surfacing the in-force inherited stash keys at a
descendant route is the read side waiting for a consumer.

Plugin shape: emit `HashKeyDef` with `HashKeyOwner::Class` tagged to the
controller (or eventually `MojoAction` namespace once phase 1 of the
unification residual lands).

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
