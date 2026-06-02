# Open problems

Genuinely-unsolved problems, stated abstractly — the underlying gap,
not a feature request. Each is deliberately out of scope until a
motivating case makes the fix's shape clear. Forward *design* corpus
lives in the `prompt-*.md` docs and `ROADMAP.md`; this file is for the
hard boundaries those designs run into.

## Untyped param / hash-element boundaries break value-flow chains

A value that arrives as a **sub parameter** or a **hash element with no
inferable type** is a dead end for any analysis that flows along the
value. The chain can't start, so everything downstream of the boundary
stays dark — even when the rest of the chain is fully modeled.

The motivating case is the Alerts partial-route boundary: crm's route
plugins root their route chain from `my $r = $conf->{root}`. `$conf` is
an untyped sub param and `root` is a hash element of it, so the
`BrandedRoute` chain (`docs/adr/route-branding.md`) never gets its
starting brand, and partial `->to('#action')` calls hanging off that
root never resolve their inherited controller. This is *not*
route-specific — the same gap swallows any chain whose origin is an
untyped param or hash element (a DBIC resultset handed in as an arg, a
helper-returned object passed through `%opts`, etc.).

The fix is not local to any one feature. It needs either declared param
types at the boundary (the `param_types()` manifest in
`docs/adr/plugin-system.md` is the landed first cut, but it types by
role/callback contract, not arbitrary hashref params) or cross-procedure
value-flow that propagates a type *into* a param from its call sites. The route doc enumerates the option
space and explicitly chose to leave boundary #4 (param/hashref) out;
see `docs/prompt-route-default-inheritance.md` option #4. Deferred until
a value-flow story exists; the in-`register` local case is the dominant
idiom and resolves without it.

## Qualified-name resolution suppression is coarse

`SUPER::method` and other `::`-qualified call forms are flagged
unresolved more aggressively than they should be. The suppression rule
keys on syntactic shape rather than asking "does this qualified name
resolve through the MRO / the named package?" — so legitimate
`SUPER::` dispatch reads as a diagnostic-worthy unresolved call. The
honest fix resolves the qualified target against the same ancestor /
package graph method dispatch already walks, rather than special-casing
the `SUPER::`/`::` token shape (a rule-#10 smell). Hint-level today, so
low blast radius; revisit alongside the Openness diagnostic rework
(`docs/prompt-unification-residual.md` Phase 6), which owns the
"when is an unresolved call real?" question.

## Static analysis can't run runtime export generators

`Sub::Exporter::setup_exporter`, `Moose::Exporter->setup_import_methods`,
and `Exporter::Tiny -base` install exported subs (and type-library
constants) at *runtime*, by executing the exporter. The analyzer sees
the `use X 'name'` import line but can't follow it to the exporting sub,
so cross-file references / goto-def for dynamically-exported names break
and the names show as unresolved-function. This is the same root as the
crm `Clove::Types` constant residual (`Str` / `Int` / `Maybe` /
`InstanceOf`).

Static analysis fundamentally can't evaluate the generator. The
tractable path is modeling the *common exporter shapes* declaratively (a
plugin that knows "a `Type::Library` exports its registered type
constants", "a `Sub::Exporter` config's `-as` renames map to these
subs") rather than executing anything — the same "plugin owns the
vocabulary, core owns the mechanism" split the framework plugins use.
Out of scope until the exporter-shape catalog is worth building; the
explicit-`qw` import path is fully handled in the meantime.

## Residual single-store cleanups (low-leverage, no motivating bug)

A few small parallel-store / dead-state items surfaced alongside the
bag-canonical refactor and never earned a landing because nothing
depends on them yet:

- **`SubInfo` arity accessors** (`param_counts`, `return_type_for_arity`,
  `primary_id`, `id_for_arity`) are `#[allow(dead_code)]` — a public API
  surface with no current caller. Delete when a cross-file caller wants
  them (route through `symbol_return_type_via_bag`) or when a dead-code
  sweep reaches them.
- **`fold_state_snapshot`'s hand-coded convergence tuple** could become
  a single monotonic `bag_generation` counter bumped on every bag
  `push` / `remove_by_source_tag`, so any future re-emittable pass
  participates in fixed-point detection automatically instead of needing
  a manual tuple entry. Cosmetic until a new re-emittable pass forgets
  the entry and the worklist exits early.
- **Forward-call resolution duplicates its lookup rule** across
  walk-time (`find_callee_symbol`) and a post-walk retry. The principled
  shape pushes a name-keyed witness (`CalleeByName`) at walk time and
  resolves it in one post-walk pass against the final symbol table.
  Worth it only when the lookup rule has to grow (package-aware lookup,
  plugin-synthesized callees).

These are recorded so the next refactor in the area picks them up; none
justifies a standalone PR.
