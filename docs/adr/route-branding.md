# ADR: Route branding — partial `->to` defaults ride the value's type

Mojolicious routes inherit a default stash from parent to child, and
crm leans on it hard — most `->to(...)` calls are partial:

```perl
my $alerts_r = $r->any('/alerts')->to('alerts#');  # controller default = 'alerts'
$alerts_r->get('/')->to('#list');                  # action='list', controller INHERITED
my $crud = $alerts_r->under('/:type')->to('#get_alert');
$crud->get('/settings')->to('#read_settings');     # controller still 'alerts'
```

`->to('#action')` and `->to('controller#')` carry only half a target;
the missing half is established by an ancestor route and inherited down
the builder chain. To make goto-def / references / hover work on a
partial `->to`, the analyzer has to know "what defaults are in force
here?" That answer accumulates along a graph of route-builder *values*
linked by four edge kinds: method chain, assignment, `under` nesting,
and the param/file boundary (`$conf->{root}`).

## Decision: the brand IS the type, content-addressed

```rust
pub enum InferredType {
    // … existing variants
    BrandedRoute {
        base: String,                  // dispatch class (Mojolicious::Routes::Route)
        controller: Option<String>,    // inherited controller default
        stash: Vec<(String, String)>,  // arbitrary inherited stash defaults
    },
}
```

The accumulated defaults live *in* the type the chain already carries.
Because the type rides assignment / chaining / nesting through the
witness bag for free, the brand survives all three local edges with no
new plumbing — the user's "if it's expressed as a class, it flows for
free."

This is the design-doc's option C **collapsed**: the original sketch
split a brand *id* (in the type) from brand *contents* (a
`HashMap<BrandId, RouteDefaults>` side-table). Collapsing the two
removes the hardest part — minting a deterministic, cache-stable brand
id — entirely. The resolved defaults *are* the value, content-
addressed: two routes with the same accumulated defaults are the same
`BrandedRoute`, and there is no id to keep reproducible across a bincode
round-trip.

### Inheritance is overlay, not mutation

Each route method that sets a default produces a **new** `BrandedRoute`
overlaying its own keys onto the receiver's. Children never mutate
parents; a sibling group with its own `->to('other#')` re-brands its
descendants without leaking into the first group. The parent-pointer
tree the side-table option needed falls out of the value being
immutable — overlay-on-read is just "construct the child with the
parent's map plus the local key."

### Reads route through the existing chain typer

`->to('ctrl#')` / `->to(controller => …)` **set** the controller (and
stash) on the produced `BrandedRoute`; `->to('#action')` **reads** the
inherited controller off its receiver's brand. The plugin exposes the
receiver's accumulated defaults to its `on_method_call` hook as
`ctx.receiver_route_defaults` (`[[key, value], ...]`, `()` when
unbranded); `controller` is the distinguished key. No second
symbolic-execution pass — the brand resolves inside
`Builder::resolve_invocant_class_tree`, the single chain typer (rule:
no second one).

## Why not the alternatives

- **A supplementary fact keyed on the attachment** (plain
  `ClassName(Route)` + a parallel `RouteDefaults` witness) is the
  "parallel store that drifts" the bag-canonical ADR warns about: it
  has to re-derive every edge the type already rides and stay in sync
  with the chain typer. Putting the defaults *in* the type means there
  is one thing to propagate, not two.
- **A build-time backward chain walk** at each partial `->to` is the
  rule-#10 backward-enumeration that rots: it works for the
  syntactically-local case and gives up the moment the chain passes
  through a helper sub, a loop, or a cross-file root.
- **A first-class route-tree side structure** (modeling Mojo's own
  route tree) is the most faithful but a whole new data model +
  serialization — overkill unless named routes / `url_for` / websocket
  upgrades turn routes into a major surface.

## The boundary that stays dark

The fourth edge — **the param/hashref boundary** — is deliberately out
of scope. crm roots its route chain from `my $r = $conf->{root}`, an
untyped hashref-param boundary: the value arrives as a hash element of a
sub parameter with no inferable type, so the `BrandedRoute` chain never
*starts*, and every partial `->to` downstream of an unbranded root
stays dark. This is not route-specific — it is the general
untyped-param / hash-element boundary breaking value-flow chains. See
`docs/open-problems.md`.

In crm the brand is almost always established locally after the
boundary (intra-`register`), so the common case resolves regardless.
The boundary matters for completeness, not for the dominant idiom.

## Trade-offs

- **`EXTRACT_VERSION` bump** for the variant (kept at the END of
  `InferredType` for bincode variant-index stability).
- **One global namespace.** A resolved partial route is keyed per
  declaring package, not per app instance — same single-app scoping
  limitation as the helper app surface and the routes
  `PluginNamespace`. Per-app scoping waits for a real two-app
  workspace.
