# QA findings — open worklist

From a real-world QA sweep of `perl-lsp` against **crm** (Mojolicious + Clove + DBIC + Minion),
**Dancer2**, **metacpan-web** (Catalyst + Moose), and **Moose**. No crashes/panics on any tree.
This file now tracks only what's **still open** — closed items are one-lined below.

## ✅ Closed (landed in PR #45)
P0.1 moo phantom `ro`/`rw`/`lazy` accessors · P1.2 `export_ok` bare-`use` suppression (+ resolution path) ·
P1.3 catalyst `param_types` over-application (action-gated) · P1.4 cross-file `$c` multi-hop ·
P2.8 dancer DSL keyword gaps (+ `Dancer2::Plugin` `has`) · cold-start 9 min → ~8 s ·
P3: lowercase `does`, `lazy_build` expansion, `accessor` keyword, completion-path `$c` ·
plus the review-flagged perf cleanups (fold registry hoist, `Arc` memo, O(1) export lookup).

---

## Open — framework completeness

### P2.5 — DBIC inherited methods flagged unresolved
- **Where:** crm — ~420 of 1144 unresolved-method hints, across 286 files (the dominant method-noise source).
- **Symptom:** `$self->result_source` / `->schema` / `->search` / `->update` / `->columns` / `->populate` / … on local Result/ResultSet classes flagged, though inherited from `DBIx::Class::ResultSet`/`Row`/`Core` via `Clove::Base 'Result'|'ResultSet'`.
- **Fix direction:** model the DBIC base-class method surface so `resolve_method_in_ancestors` finds them — encode the methods on the synthetic DBIC base class/type (rule #10: not a name-allowlist in the diagnostic consumer). Goal is killing the false unresolved-method diagnostics (methods become *known*); typed returns are a later nicety. Was bundled with a fix agent that got stopped — not started.

### P2.6 — `requires` (Moo::Role / Role::Tiny) not in framework imports
- **Where:** crm — 37 false positives.
- **Root cause:** `framework_imports` for `Moo::Role` (`src/builder.rs:~3897`) lists `has`/`with`/`extends`/`around`/`before`/`after` but not `requires`; plain `Role::Tiny` isn't handled at all.
- **Fix:** add `requires` to the Moo::Role/Moose::Role import set; handle `Role::Tiny`. Trivial (was bundled with the stopped DBIC agent).

### P2.7 — dynamic Type::Library + ResultDDL exports flagged
- **Where:** crm — ~261 type-constant FPs (`InstanceOf`/`ArrayRef`/`Maybe`/`Dict`/`Enum`/`SDict`/… across 96 files via `Clove::Types -types`); ~1920 ResultDDL hints (`view`/`col`/`table`/… in Result files via `Clove::Base 'Result'`).
- **Detail:** both are dynamically registered (`Type::Library -base`; `DBIx::Class::ResultDDL`) — can't be materialized statically by the current kit plugins. This is the **exporter-deepening / probe** territory (common exporters are core's job — renames/bundles/args; the open/house long tail rides BYO plugins/probe). See `docs/open-problems.md` and the `exporters-core-vs-byo` decision. **Parked for design.**

---

## Open — minor
- **`$c->request`** (metacpan API.pm) resolves to a local `sub request`, shadowing the `Catalyst::Request` override → goto-def fails. Method-resolution priority: a local sub of the same name shadows an inherited/override target.
- **crm plain `$minion->enqueue('Task')` nav blind** — *crm-side config, not perl-lsp*: `.perl-lsp/clove-minion.rhai` `name_arg_index()` omits plain `enqueue`/`enqueue_p` (its comment claims otherwise). Tenant-sugar (`enqueue_t`) works. Fix in the crm repo's plugin.

---

## Reference — confirmed NOT bugs
- **`--dump-package`** is a faithful mirror of the editor query path (dump and hover agree on the same symbol). No drift.
- **Partial route `->to('#action')`** — documented open problem (untyped `$conf->{root}` boundary, `docs/open-problems.md`); full-form `->to('ctrl#action')` works.
- **Cross-file `ClassIsa` trigger** — documented Phase-2 deferral (`docs/prompt-enrichment-inheritance-residual.md`); the `#[ignore]`d probe is the tripwire.
- **Dynamic helper goto-def** (crm `$app->helper('dotted.name', …)` / `add_namespace`) — no static def by design.

---

## Diagnostic-noise note
Default `--check` (`--severity warning`) is **clean** (zero output) on all four codebases — noise lives in the hint/info channel. With P0/P1 closed, the remaining crm hint/info noise is dominated by **P2.5 (DBIC methods)** and **P2.7 (Type::Library + ResultDDL)**. Clearing those two is what would make the hint channel trustworthy enough to consider surfacing at warning level — the real gate on a VS Code marketplace release.
