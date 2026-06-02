# Declarative parameter typing (callbacks + role-contract methods)

> The LSP already types the *first arg* of framework callbacks — `$job` in a
> Minion task, `$self` in a Mojo event handler, `$c` in a helper. It does this
> ad-hoc per plugin via a `VarType` emission. There's a second, structurally
> identical need it *can't* meet today: typing a parameter of a plain `sub`
> declaration that a role's contract gives a known type — `$app` in
> `sub run_upgrade ($self, $app)` for a class that `with`s
> `Clove::Upgrade::OneTime`. This note finds the shared abstraction and plans
> a single declarative mechanism for both.

## What works today: callback first args

Minion / mojo-events / mojo-helpers all do the same thing. The plugin sees the
callback as a call **argument** (`ctx.args[N].sub_params` gives the sub's
params) and emits:

```rhai
VarType: #{ variable: params[0].name, at: ctx.args[1].span, inferred_type: #{ ClassName: "Minion::Job" } }
```

The builder applies it through `deferred_var_types`, typing the variable inside
the callback's scope. This is driven from `on_method_call` / `on_function_call`
because the trigger is a *call* the plugin already hooks.

## The gap: params of a plain sub declaration

`run_upgrade ($self, $app)` isn't an argument to anything — it's a method
declaration in a class that does the `Clove::Upgrade::OneTime` role, and the
role's contract fixes `$app` as the Mojolicious app. To type it we'd need to
observe the **sub declaration** and its params. But the emit-hook surface is
`on_use` / `on_function_call` / `on_method_call` (+ query hooks) — **there is
no `on_sub` / sub-declaration hook.** So no plugin can reach it.

## The shared abstraction

Both cases reduce to the same statement:

> *parameter N of a sub has type T, determined by the sub's context.*

They differ only in the **selector** — how you identify the sub and which
context fixes the type:

- **Callback selector:** "the sub passed as arg `i` to a call of verb `V`"
  (Minion `add_task` arg 1 → param 0 is `Minion::Job`).
- **Contract selector:** "a sub named `M` declared in a class that does role
  `R`" (`run_upgrade` in a `Clove::Upgrade::OneTime` doer → param 1 is the app).

Both end in the same action: emit a `VarType` for param N within the sub's
scope. The mechanism that's missing is only the *contract selector* and the
sub-declaration observation it needs.

## Proposed mechanism: a `param_types()` manifest

A declarative manifest, same family as `overrides()` / `dispatch_verbs()` /
`type_constraint_names()` — trigger-independent, read once at load:

```rhai
fn param_types() {
    [
        // contract selector
        #{ method: "run_upgrade", in_role: "Clove::Upgrade::OneTime",
           param: 1, type_class: "Mojolicious" },
        // (callback selector — optional future migration of the VarType cases)
        // #{ callback_of: "add_task", arg: 1, param: 0, type_class: "Minion::Job" },
    ]
}
```

Core application (in `builder.rs`, at the sub-declaration walk — the one place
allowed to see sub decls, rule #1):

1. When visiting a `sub`/`method` declaration, look up its name + the enclosing
   package's roles/parents (`package_parents`, which already unifies `with` /
   `extends` / `isa` / `does`).
2. For each `param_types` entry whose `method` matches and whose `in_role` is in
   the package's resolved ancestry, emit a `VarType` for the param at `param`
   index (typed `ClassName(type_class)`), scoped to the sub body.

That's it — the contract selector reuses the existing `VarType` + scope
machinery; only the "observe a sub decl + consult the manifest" step is new,
and it's a small, localized addition to the declaration visitor.

### Why a manifest, not an `on_sub` hook

Same reasoning as the other manifests: a plugin can't walk nodes (rule #1), and
the selector data is static. A manifest keeps the plugin declarative and
cacheable; the core does the (single) sub-decl observation and applies every
plugin's rules. An imperative `on_sub` hook would hand plugins a sub-decl
context, but nothing here needs imperative logic — the selector + index + type
is pure data.

### Unifying the callback case (optional)

The callback first-arg `VarType`s in minion/events/helpers could migrate to the
`callback_of` selector so all parameter typing lives in one declarative place.
Worth doing only if it actually reduces duplication — the callback emission is
already a one-liner in each plugin and is driven from a hook those plugins need
anyway (for the Handler/DispatchCall emission). Recommend landing the contract
selector first (it's the real gap), then evaluating migration.

## crm plugin (the concrete payoff)

A crm `.perl-lsp` plugin declaring the upgrade-role contract:

```rhai
fn id() { "clove-upgrade" }
fn triggers() { [] }            // manifest is global
fn param_types() {
    [ #{ method: "run_upgrade", in_role: "Clove::Upgrade::OneTime",
         param: 1, type_class: "Mojolicious" } ]
}
```

Then in `Clove::Upgrade::OneTime::v04303_missing_items`,
`sub run_upgrade ($self, $app)` types `$app` as `Mojolicious` → `$app->minion`
resolves (the `minion` helper bridged to `Mojolicious::Controller`… see the
caveat below) → `$app->minion->enqueue('Borkage…')` lights up via option-B
dispatch. Closes the `$app->minion` form, the last dark minion shape.

**Caveat to verify:** helpers are bridged to `Mojolicious::Controller`, but
`$app` is the `Mojolicious` app, not a controller. In Mojo, helpers are callable
on both the app and controllers. So either the `minion` helper must also bridge
to the app class, or `param_types` should type `$app` as
`Mojolicious::Controller` (pragmatic: the helper set is the same), or the helper
bridge widens to "app + controller." Decide during implementation — typing
`$app` as the class that actually carries the `minion` helper is what matters.

## Sequencing

The core step touches `builder.rs` (the sub-decl visitor) — land it AFTER the
expr-resolution unification worktree merges, to avoid conflicts. The crm plugin
is in a separate repo and can be written anytime; it's inert until the core
`param_types` application exists.

## Acceptance

- `sub run_upgrade ($self, $app)` in a `Clove::Upgrade::OneTime` doer →
  `$app` types as the declared class (hover / `--type-at`).
- crm `$app->minion->enqueue('Borkage.retry_all_borkage')` resolves to the
  `add_task` site (the last dark minion form).
- A generic unit pin: a method in a role-doer gets its declared param type,
  and a non-doer with the same method name does NOT (selector is role-gated).
- No regression to the callback first-arg typing (minion `$job`, etc.).
