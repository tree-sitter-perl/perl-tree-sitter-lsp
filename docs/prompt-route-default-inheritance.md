# Partial route targets: inheriting controller + stash defaults down a route chain

> `->to('#action')` and `->to('controller#')` carry only part of a route
> target. The missing half (and any stash defaults) is established by an
> ancestor route and **inherited** down the builder chain. To make gd / gr
> / hover work on partial targets — crm's dominant idiom — the LSP has to
> reconstruct "what defaults are in force at this `->to`?" This doc maps the
> option space for *how the defaults flow*; it does not commit to one.
>
> The controller-token → class mapping is already decided: **camelize the
> token, then workspace-search for a `*::Controller::<Camelized>` that owns
> the action** (`'login'` → `Clove::Controller::Login`,
> `'sales_reports'` → `…::Sales::Reports`). This doc is orthogonal to that —
> it's about *which controller token / stash is live* at a partial `->to`.

## Symptom (crm)

Mojolicious routes inherit a default stash from parent to child. crm leans
on this hard — most `->to(...)` calls are partial:

```perl
sub register ($self, $app, $conf) {
  my $r = $conf->{root};                              # root crosses a file/sub boundary

  my $alerts_r = $r->any('/alerts')->to('alerts#');   # sets controller default = 'alerts'
  $alerts_r->get('/')->to('#list');                   # action='list', controller INHERITED → 'alerts'

  my $crud = $alerts_r->under('/:type')->to('#get_alert');  # controller inherited 'alerts'
  $crud->get('/settings')->to('#read_settings');      # controller still 'alerts'
  $crud->post('/settings')->to('#update_settings');
}
```

Today `route_emissions` (in `frameworks/mojo-routes.rhai`) early-returns when
controller **or** action is empty, so every line above except the (useless,
action-less) `->to('alerts#')` emits **nothing**. gd on `#list` →
`Clove::Controller::Alerts::list` is dead; gr on that method never finds the
route. Full targets (`->to('login#login_dev')`) are *also* dead today, but
only for the token-mapping reason (decided above), not this one.

Stash is not just controller/action. crm also writes plain stash defaults:

```perl
$some_r->to(setting => $setting);   # a stash default children inherit; not a controller
```

So whatever carries the inheritance must carry an **arbitrary key→value
stash map**, of which `controller` and `action` are two distinguished keys —
not a bare controller string.

## How routes resolve today

- `frameworks/mojo-routes.rhai` `on_method_call` sees `->to(STR)`, splits on
  `#`, and for the **full** form emits a `MethodCallRef { invocant: token,
  method_name: action }` (+ a route-as-thing `Handler`). The token is passed
  **raw**; the builder never camelizes or namespaces it (the decided fix).
- The route builder chains because the plugin pushes a type override:
  `Mojolicious::Routes::Route::_route` returns `ClassName(Mojolicious::Routes::Route)`.
  So `$r->any(...)->to(...)->name(...)` keeps the receiver typed across hops.
  **This is the only per-chain state today** — and it is *class-level*, not
  *instance-level*: every route value is the same `ClassName(Route)`. There
  is no notion of "*this* route vs *that* route." A brand needs exactly that
  distinction.
- Chain typing is done once, in `Builder::resolve_invocant_class_tree` (the
  single symbolic-execution pass; CLAUDE.md forbids a second). Anything that
  reads "the receiver's accumulated defaults" has to compose with that pass
  or run as its own named build phase.

## The actual problem

Route defaults accumulate along a graph of route-builder **values** linked
by four kinds of edge. Any solution is really a question of "does my
representation survive all four?":

1. **Method chain** — `$r->any('/x')->to('a#')` (same statement).
2. **Assignment** — `my $alerts_r = $r->any('/x')->to('a#'); $alerts_r->...`.
3. **Nesting** — `my $crud = $alerts_r->under('/:t')->to('#g'); $crud->...`
   ( `$crud` inherits `$alerts_r`'s controller, *plus* its own action default ).
4. **Boundary** — the root arrives as `$conf->{root}` (param / hashref from
   another file). In crm the *brand* is always established locally after the
   boundary, so #4 matters for completeness, not for the common case — but
   it's the cleanest discriminator between the options below.

Inheritance semantics to preserve:
- `->to('ctrl#')` / `->to('ctrl#action')` / `->to(controller=>…)` **set/override**
  the controller (and possibly action) default for everything downstream.
- `->to('#action')` **reads** the inherited controller, **sets** action locally.
- `->to(key => val)` merges `key` into the inherited stash.
- A child never mutates its parent (defaults flow down only).

## Options

### A. Brand as a virtual class (identity encoded in the type)

Mint a stable per-route identity and express it *as the type* the chain
already carries — e.g. `ClassName("Mojolicious::Routes::Route#<brandId>")`
or a parallel `InferredType::BrandedRoute { base, brand }`. Because it lives
in the type, it rides assignment, chaining, return values, **and param /
file boundaries for free** through the existing witness-bag flow — the user's
"if it's expressed as a class, it flows for free."

- **Sets:** `->to('a#')` on a route typed `Route#7` produces `Route#7` still,
  and records `brand[7].controller = 'a'`.
- **Reads:** `->to('#list')` looks up `brand[<receiver brand>].controller`.
- **Stash:** the brand id keys a side-table of accumulated defaults (the
  virtual class is the *handle*; the stash map is the *data*). So A in
  practice is A-as-identity + a brand table — see C.

Tensions:
- **Minting a stable id.** Span of the originating route expression? Variable
  identity? A monotonic counter (forbidden to be nondeterministic — must key
  off something in the source, e.g. the decl span, so cache/bincode is
  reproducible)? Two routes that *should* share a brand (aliased var) must get
  the same id; two that shouldn't must differ.
- **Override = new brand or mutate?** `$crud = $alerts_r->under->to('#g')`
  must give `$crud` a *fresh* brand that **inherits** `$alerts_r`'s map then
  overlays its own — so brands form a parent-pointer tree, not a flat map.
- **Type-system blast radius.** A new `InferredType` variant touches every
  match on the enum, the reducers, serialization, `EXTRACT_VERSION`. Encoding
  the brand inside the existing `ClassName` string (a `#suffix`) avoids the
  variant but smuggles structure into a string — itself a rule-#10 smell.

### B. Brand as a supplementary fact (witness on the value)

Keep the type plain `ClassName(Route)`; attach the accumulated defaults as a
**separate witness/fact** keyed to the route value's attachment
(`Variable{name,scope}` / `Expr(span)`), propagated alongside the type.

- `->to('a#')` pushes a `RouteDefaults { controller:'a', stash:{} }` fact on
  the receiver's attachment; chain/assignment edges copy-merge it forward.
- `->to('#list')` reads the fact off its receiver attachment.

Tensions (the user's "must make sure it composes everywhere"):
- Has to ride **every** edge the type rides — assignment edges, method-return
  edges, `under` nesting — or it silently drops at the first hop it forgot.
  The type already does this; a parallel fact has to re-derive the same
  plumbing and stay in sync with `resolve_invocant_class_tree`. This is the
  "parallel store that drifts" the bag-canonical ADR warns about.
- **Boundary (#4):** a fact keyed on `Variable{name,scope}` does **not** cross
  a param/file boundary — `$conf->{root}`'s brand would be lost. (Acceptable
  for crm today; a real ceiling.)
- Cheaper than A *if* the plumbing already exists to hang arbitrary facts on
  the same attachments the chain typer walks; expensive if it doesn't.

### C. Hybrid — virtual-class identity + brand table (recommended starting lens)

Split identity from data: a lightweight brand **id** lives in the type (A,
so it flows for free across all four edges), and the brand **contents** (the
accumulated `{controller, action?, stash…}`, plus a parent-brand pointer for
inheritance) live in a `HashMap<BrandId, RouteDefaults>` side-table built
during the chain pass.

- Flows across boundaries because identity is in the type.
- Holds arbitrary stash because data is in the table, not the type string.
- `->to` resolves `receiver.brand` → table → walk parent pointers for
  inherited keys, overlay local.
- Cost: needs the id-minting discipline of A *and* a table; but the table is
  plain build state (like `package_parents`), and the type change can be the
  minimal `#suffix`-on-ClassName rather than a new enum variant.

### D. No brand — build-time backward chain walk

At each partial `->to('#action')`, symbolically walk the receiver backward to
the nearest ancestor `->to('ctrl#…')` in its chain, *at emit time*, and bake
the resolved controller into the `MethodCallRef`. No value-level state at all.

- Matches crm's reality: brand origin and use are almost always in the **same
  sub** (intra-`register`), reachable by walking var-definition + chain.
- Rule-#10 cost: this is "special-case the partial shape with a bespoke
  walk," exactly the backward-enumeration the guidelines warn rots over time
  (passes through a helper sub, a map/loop, a cross-file root → walk gives up).
- Cannot cross boundary #4, and degrades silently when the chain isn't
  syntactically local.
- Cheapest to ship; weakest to extend to stash defaults (each key needs its
  own walk) and to non-local chains.

### E. No brand — explicit route-tree side structure

Build a first-class route graph during the build: nodes = route values,
edges = parent/child (`under`, chained `->to`), each node carrying its local
defaults; resolve any `->to` against the node's inherited path. Essentially
modeling Mojolicious' own route tree.

- Most faithful to Mojo semantics; naturally handles stash, `name`, nested
  groups, even `url_for` targets.
- Biggest build; a new data model + its own serialization. Likely overkill
  unless route features keep growing (named routes, `url_for`, websockets,
  format constraints…). Worth it only if routes become a major surface.

### F. Status quo — full forms only

Resolve `->to('ctrl#action')` (after the token fix) and do nothing for
partials. Leaves most of crm's routes dark. Baseline to beat.

## Evaluation axes

| Option | flows across assign/chain | across nesting | across boundary #4 | arbitrary stash | rule-#10 | type-system blast | build cost |
|---|---|---|---|---|---|---|---|
| A virtual-class | ✓ (in type) | ✓ (brand tree) | ✓ | only via side-table → becomes C | ok if not string-smuggled | high (variant) or medium (#suffix) | medium |
| B fact | ✓ if replumbed | ✓ if replumbed | ✗ | ✓ | ok | low | low–medium |
| C hybrid | ✓ | ✓ | ✓ | ✓ | ok | medium | medium |
| D backward walk | n/a | partial | ✗ | weak | **smell** | none | low |
| E route-tree | ✓ | ✓ | ✓ (if keyed right) | ✓ | clean but heavy | new model | high |
| F status quo | — | — | — | — | — | none | none |

## Open questions (for discussion, not yet answered)

1. **Is boundary #4 in scope?** If "brand established locally in the same sub"
   covers ~all of crm, D/B get much more attractive and we can defer the
   in-the-type cost. Need a quick census: how many crm partial `->to` resolve
   within their own `register`?
2. **Brand id source.** What's the deterministic, cache-stable key — the decl
   span of the route-rooting expression? This decides whether A/C are even
   reproducible across a bincode round-trip.
3. **Does anything *read* stash defaults besides routing?** If stash inherit
   only ever matters for `controller`, we can ship controller-only (A/D) and
   treat general stash as a later layer. If hover/completion want stash keys,
   C/E pull ahead now.
4. **Relationship to the existing route Handler / `url_for` namespace.**
   Branding interacts with the `mojo-routes:<pkg>` PluginNamespace and the
   `url_for`/`redirect_to` Handlers — does a resolved partial also register a
   nameable route? Keep these orthogonal or unify?
5. **One pass or two?** Reading inherited defaults at a `->to` requires the
   ancestor's brand to be resolved first. Within `resolve_invocant_class_tree`
   (already fixpoints) this may fall out; as a separate fact (B) it needs its
   own ordering guarantee.

## Acceptance (whatever option wins)

- gd on `->to('#list')` (Alerts.pm) → `Clove::Controller::Alerts::list`.
- gd on `->to('alerts#')` controller token → `Clove::Controller::Alerts`
  (the class; no action).
- gr on `Clove::Controller::Alerts::read_settings` surfaces the
  `$crud->get('/settings')->to('#read_settings')` route site.
- Nested: `$crud` (under `$alerts_r`) partials inherit `alerts`, not the root.
- Override: a sibling group `->to('other#')` re-brands its own descendants;
  no leak between groups.
- Stash (stretch): an inherited `->to(setting => …)` default is queryable at a
  descendant terminal.
- Negative: a `->to('#x')` with no resolvable ancestor brand degrades quietly
  (no panic, no false controller).
- Cross-file/param (#4, option-dependent): root via `$conf->{root}` either
  resolves or is explicitly documented as out of scope for the chosen option.
