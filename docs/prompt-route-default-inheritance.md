# Partial route targets: what's still open

> The landed design — branding the route value with its accumulated
> defaults so partial `->to('#action')` reads the inherited controller —
> lives in `docs/adr/route-branding.md` (`InferredType::BrandedRoute`,
> option C collapsed). This doc keeps only the residual: the boundary the
> brand can't cross, and the route features beyond controller/action that
> branding could grow into.

## What landed (pointer only)

`->to('ctrl#')` sets the controller default on the produced
`BrandedRoute`; `->to('#action')` reads it; `->to(key => val)` merges a
stash default. The brand rides assignment / chaining / `under` nesting
through the witness bag, content-addressed (no brand-id, no side-table).
Overlay-on-construct gives inheritance; siblings re-brand their own
descendants without leaking. See the ADR for the rationale and the
rejected alternatives (supplementary fact, backward chain walk,
route-tree side structure).

## Option #4 — the param/hashref boundary (out of scope, deliberately)

crm roots its route chain from `my $r = $conf->{root}` — an untyped
hashref-param boundary. The value arrives as a hash element of a sub
param with no inferable type, so the `BrandedRoute` chain never gets its
starting brand and every partial `->to` downstream of an unbranded root
stays dark.

This is the general untyped-param / hash-element boundary breaking
value-flow chains, not anything route-specific — see
`docs/open-problems.md`. In crm the brand is almost always established
locally after the boundary (intra-`register`), so the dominant idiom
resolves without it; the boundary matters for completeness only. It
needs either a declared type at the boundary (the `param_types()` manifest
in `docs/adr/plugin-system.md` types by role/callback contract, not
arbitrary hashref params)
or cross-procedure value-flow propagating a type into the param from its
call sites. Neither exists; deferred until a value-flow story does.

## Stash beyond controller/action

Branding already carries an arbitrary `stash: Vec<(String, String)>`, so
`->to(setting => $setting)` inheritance is representable. What's not
wired: anything that *reads* inherited stash besides controller
resolution — hover/completion surfacing the in-force stash keys at a
descendant terminal. Land when a consumer wants it; the data is already
on the value.

## Open questions

1. **Relationship to the route `Handler` / `url_for` namespace.** A
   resolved partial registers a `MethodCallRef`; does it also register a
   nameable route in the `mojo-routes:<pkg>` `PluginNamespace` so
   `url_for`/`redirect_to` reach it? Currently orthogonal — unify only
   if named-route navigation grows.
2. **Per-app scoping.** One global brand namespace per declaring package
   — the same single-app limitation as the helper app surface. Per-app
   scoping waits for a real two-app workspace.
