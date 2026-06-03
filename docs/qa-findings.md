# QA findings — real-world sweep (usability sprint)

Bug worklist from a real-world QA pass of `perl-lsp 0.3.0` against four codebases:
**crm** (in-house Mojolicious + Clove kits + DBIC + Minion), **Dancer2**, **metacpan-web**
(Catalyst + Moose), and **Moose** itself. No crashes/panics on any tree.

> **Binary caveat:** the QA agents ran a binary built at `922f7c2` (pre-`ReceiverGated`
> Phases 1–2). Findings about non-pillar features are valid as-is. The two
> pillar-dependent verdicts (cross-file `$c`, query-time dispatch refs) were
> re-verified on a fresh `ce24c81` build — cross-file `$c` **still fails** (see P1.4),
> so that finding stands for a deeper reason than the stale binary.

Severity: **P0** regression this sprint · **P1** high-impact · **P2** completeness · **P3** minor.

---

## P0 — regression introduced this sprint

### P0.1 — `moo.rhai` mints phantom `ro`/`rw`/`lazy`/`bare` accessor methods
- **Where:** all four codebases. Scale: ~178 phantom symbols in crm (94 `rw`, 47 `ro`, 37 `lazy`); **147 of 385** `.pm` files in Moose; also Dancer2 / metacpan.
- **Symptom:** `has x => (is => 'ro', ...)` synthesizes the real `x` accessor **plus** a spurious method named `ro` (likewise `rw`, `lazy`, `bare`, and any string option value). They pollute outline / workspace-symbol / completion and make `$obj->ro` / `->lazy` falsely goto-def-resolvable.
- **Root cause:** the sprint's #5 change made `Builder::extract_has_options` (`src/builder.rs:~6462`) capture **every** option key generically, classifying "by value shape, not by keyword", and `frameworks/moo.rhai:133` (`else if opt.explicit_name != ()`) then synthesizes a method named after *any* option's string value. `is => 'lazy'` is indistinguishable from `reader => 'get_x'` under value-shape-only classification.
- **Fix:** the plugin owns the vocabulary now, so the keyword gate belongs in `moo.rhai`: only synthesize a named method when the option keyword is one of `reader` / `writer` / `predicate` / `clearer` / `builder` / `handles`. (`is`/`isa`/`default`/`lazy`/`coerce`/… never produce a named method.)
- **Status:** regression caused by this sprint's #5 "plugin-owned has vocabulary" work. **Fix first.**
- **Adjacent (low confidence):** `Clove::Grist` dump shows `document_id` twice — possible duplicate accessor synthesis; worth a glance while in this code.

---

## P1 — high-impact false positives / unfinished pillar

### P1.2 — bare-`use` auto-import suppression checks `export` but not `export_ok`
- **Where:** Moose — **~684 false positives** (641 `Moose::Util::TypeConstraints`, 43 `Moose::Role`).
- **Symptom:** `use Moose::Util::TypeConstraints;` auto-imports `subtype`/`as`/`where`/`coerce`/… but the diagnostic fires "`subtype` is exported by … but not imported".
- **Root cause:** `collect_diagnostics` (`src/symbols.rs:~2291-2302`) suppresses bare-`use` auto-imports by checking only `cached.analysis.export` (traditional `@EXPORT`). Runtime-exporter names (`Moose::Exporter->setup_import_methods`) land in `export_ok` via `record_runtime_exports`, not `export`.
- **Fix:** extend the bare-`use` check to also consult `export_ok`. (Aligns with the "common exporters are core's job" decision — runtime exports must be visible to consumers.)
- **Status:** pre-existing gap, exposed by the exporter work.

### P1.3 — catalyst `param_types` wildcard over-applies to every method's 2nd param
- **Where:** metacpan-web — e.g. `$page` in `pageset`, `$path` in `request`, `$dist` in `via_dist` all typed `Catalyst` (15/16 type_constraints wrong on `ReleaseInfo.pm`).
- **Root cause:** `catalyst.rhai` `param_types()` uses a method-name wildcard (`method: ()`, `param: 1`), typing the 2nd positional param of **every** method in a `Catalyst::Controller`/`Model`/`View` subclass — not just action methods.
- **Fix:** scope to actual action methods (e.g. attribute-gated `:Local`/`:Path`/`:Chained`), or otherwise distinguish actions from plain methods. The correct hits are `ACCEPT_CONTEXT`/`COMPONENT`; the rest are noise.
- **Status:** FIXED. `ParamType` gained `requires_action_attr` (`#[serde(default)]`); the
  builder records whether a sub carries any declaration attribute (`collect_attributes`,
  rule #1) and skips attribute-gated wildcard rules on attribute-less subs. `catalyst.rhai`'s
  wildcard `param_types` now set `requires_action_attr: true`; `Catalyst::Model` gets
  `ACCEPT_CONTEXT`/`COMPONENT` as *named* (un-gated) rules. Verified on metacpan-web:
  `pageset`/`$page`, `request`/`$path` no longer typed; action `$c` + Model `ACCEPT_CONTEXT`/
  `COMPONENT` context args do type.

### P1.4 — cross-file `$c` fails at multi-hop ancestry
- **Where:** metacpan-web — `$c` untyped in every action controller (`Author → MetaCPAN::Web::Controller → Catalyst::Controller`). Direct-parent classes *do* get typed.
- **Detail:** Phase 2 (gated `param_types` via `ReceiverGated` + query-time `resolve_for` → `class_isa`) passed its unit probe, but the probe only exercised a shape where `class_isa` could see both classes. On a real app the controller base is a **workspace intermediate**, and the cross-file walk isn't traversing it. Re-verified on a fresh `ce24c81` build (dump and hover agree: `$page` 1-hop → `Catalyst`, `$c` 3-hop → `null`).
- **Fix:** make `class_isa`'s cross-file walk traverse workspace-file parents (the intermediate base) at query time, not just `module_index` @INC deps. Also confirm the gated param-type fallback is the single read path for *all* variable-type consumers (it's a fallback in `inferred_type_via_bag_ctx`, not type-enforced like the dispatch side — latent drift risk).
- **Status:** FIXED. Root cause was NOT `class_isa` — it already walks local `package_parents`
  ∪ `parents_cached`, and workspace intermediates ARE registered in the index
  (`register_workspace_module`). The bug was the *query-site callers dropping the module index*:
  the variable-type hover/dump paths called the bare `inferred_type_via_bag` (index `None`), so
  `gated_param_type_for` → `class_isa` could only see the open file's local parents — which carry
  just the direct-parent edge, never the intermediate's parents. Direct-parent controllers typed
  (one local hop); 3-hop didn't (needed `parents_cached` on the intermediate, unreachable without
  the index). Fix: thread `module_index` through `format_symbol_hover`/`_at`, the
  `resolve_expression_type` scalar arm, and the `--dump-package` param probe. Verified: `$c` in
  `MetaCPAN::Web::Controller::Author` (Author → MetaCPAN::Web::Controller → Catalyst::Controller)
  now types `Catalyst` via both `--dump-package` and `--hover`. Benefits ALL gated resolution
  (dispatch was already index-threaded; this closes the param-type path).

---

## P2 — framework completeness

### P2.5 — DBIC inherited methods flagged unresolved
- **Where:** crm — ~420 of 1144 unresolved-method hints, across 286 files.
- **Symptom:** `$self->result_source` / `->schema` / `->search` / `->update` / `->columns` / `->populate` / … on local Result/ResultSet classes flagged, though inherited from `DBIx::Class::ResultSet`/`::Core` via `Clove::Base 'ResultSet'`.
- **Fix:** model the DBIC base-class method surface (so `resolve_method_in_ancestors` finds them). Dominant crm method-noise source.

### P2.6 — `requires` (Moo::Role / Role::Tiny) not in framework imports
- **Where:** crm — 37 false positives.
- **Root cause:** `framework_imports` for `Moo::Role` (`src/builder.rs:~3897`) lists `has`/`with`/`extends`/`around`/`before`/`after` but not `requires`; plain `Role::Tiny` isn't handled at all.
- **Fix:** add `requires` to the Moo::Role/Moose::Role import set; handle `Role::Tiny`. Trivial.

### P2.7 — dynamic Type::Library + ResultDDL exports flagged
- **Where:** crm — ~261 type-constant FPs (`InstanceOf`/`ArrayRef`/`Maybe`/`Dict`/`Enum`/`SDict`/… across 96 files via `Clove::Types -types`); ~1920 ResultDDL hints (`view`/`col`/`table`/… in Result files via `Clove::Base 'Result'`).
- **Detail:** both are dynamically registered (`Type::Library -base`; `DBIx::Class::ResultDDL`) and can't be materialized statically by the current kit plugins. This is the "common exporters are core's job (renames/bundles/args)" + probe/BYO territory — see `docs/open-problems.md`.

### P2.8 — dancer.rhai DSL vocabulary gaps
- **Where:** Dancer2 — keywords firing in real test code but absent from `dancer.rhai`: `content` (10), `send_error` (9), `response_header` (7), `uri_for_route` (4), `prepare_app` (2), `encode_json`/`decode_json` (2 each), `request_header` (1).
- **Also:** consumer plugins using `use Dancer2::Plugin; has my_setting => (...)` get **no** accessor synthesis — `moo.rhai` triggers on `UsesModule("Moo")`, which doesn't fire for `Dancer2::Plugin`'s re-exported `has`.

---

## P3 — minor

- **`$obj->does(...)`** not in the universal-methods skip list (`symbols.rs`) — 29 Moose FPs (only uppercase `DOES` is listed).
- **`lazy_build => 1`** (Moose) not expanded to `_build_x`/`clear_x`/`has_x` — accessor synthesized, the three companions are not.
- **goto-def on a `has` accessor** lands one line into the `has` body (`is => 'ro'` line) instead of the `has` keyword.
- **`$c->request`** (metacpan API.pm) resolves to a local `sub request`, shadowing the `Catalyst::Request` override; goto-def then fails.
- **crm plain `$minion->enqueue('Task')` navigation blind** (def + refs) — but this is crm-side plugin data: `.perl-lsp/clove-minion.rhai` `name_arg_index()` omits plain `enqueue`/`enqueue_p` despite its comment claiming to claim them. Tenant-sugar (`enqueue_t`) dispatch works correctly.
- **Cold-start perf:** first workspace index on crm (794 `.pm`) took ~9 min before the SQLite cache fully warmed (then ~1.5 s). First-open UX on a fresh checkout.

---

## Confirmed NOT bugs
- **`--dump-package`** is a faithful mirror of the editor query path — dump and hover agree exactly on the same symbol (`$page`→`Catalyst`, `$c`→`null`). No drift; no second source.
- **Partial route `->to('#action')`** — documented open problem (untyped `$conf->{root}` boundary); full-form `->to('ctrl#action')` works.
- **Cross-file `ClassIsa` trigger** — documented Phase-2 deferral (`docs/prompt-enrichment-inheritance-residual.md`); the `#[ignore]`d probe is the tripwire.
- **Dynamic helper goto-def** (crm `$app->helper('dotted.name', …)` / `add_namespace`) — no static def by design.

---

## Diagnostic-noise note
Default `--check` (`--severity warning`) is **clean** (zero output) on all four codebases — the noise lives in the hint/info channel. On real crm code, well over half of the ~3762 hint/info diagnostics are false positives, dominated by P2.5 (DBIC methods), P2.7 (Type::Library + ResultDDL), and P1.2-class export issues. Clearing P0/P1/P2 above is what makes the diagnostic channel trustworthy enough to consider surfacing at warning level — and is the real gate on a VS Code marketplace release.
