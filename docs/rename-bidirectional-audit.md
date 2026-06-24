# Rename bidirectional-symmetry audit

**Status:** investigation + forward plan. Three fixes landed (see below); the
rest is open work, gated on the `supports_cross_file_rename` archaeology in
§3 which should be resolved *before* fixing the individual findings.

**Invariant under test:** for every renameable entity, renaming from the
definition site and renaming from any usage/reference site must produce the
**same complete edit set**. References must agree with rename on the target.

---

## 1. Landed fixes (branch `fix/rename-bidirectional`)

| Commit | Fix |
|---|---|
| `49049f1` | Exported sub across the bare-`use` boundary. Bare `use Bank;` auto-imports `@EXPORT`; the single-file walk left the call `resolved_package: None`, so neither direction linked source↔consumer. Added `FileAnalysis::deferred_call_package` (lazy index query: "which `use`d module binds this name"), wired into `rename_kind_at` (from-usage) + `collect_from_analysis` (from-def). |
| `3a0ebff` | Old-school `bless { key => … }, $class` keys ↔ their `$self->{key}` slots. Bless keys are owned `Sub{C,new}`, accesses `Class(C)`; `found_by` is directional (Class lookup finds Sub defs, not vice-versa), so only access→key worked. Now `visit_anon_hash` emits an `InternalKey` projection for blessed hashes (rule #9), so the projection group's `InternalHashKey` member reaches the slots. Bumped `EXTRACT_VERSION`. |
| `716290e` | Return-hash key (`sub get_config { return { host => 1 } }`) def↔access symmetry. `rename_kind_at` returned `None` for a `HashKeyDef` symbol, and `resolve_target_at` returned `include_decl = false` on a decl cursor (a hash-key def has no ref at its own token, unlike a variable decl). Mapped `HashKeyDef` → `RenameKind::HashKey` and opted hash-key defs into `include_decl`. |

These three address the originally-reported bugs + the return-hash-key case.
The audit below (run by a background review agent over a controlled
multi-file workspace + the `test_files/` DBIC and Mojo fixtures) found the
fixes hold and surfaced **six more asymmetries** — all tracing to the same
seam (§3).

---

## 2. Symmetry status table

| Entity kind | def→usages | usage→def/usages | Status |
|---|---|---|---|
| Plain same-file sub | ✓ | ✓ | symmetric |
| Package-qualified call `Foo::bar()` | ✓ | ✓ | symmetric |
| Cross-file exported sub — bare `@EXPORT` | ✓ | ✓ | symmetric (fix `49049f1`) |
| Cross-file exported sub — explicit `qw/…/` | ✓ | ✓ | symmetric |
| Cross-file exported sub — `@EXPORT_OK` | ✓ | ✓ | symmetric |
| Cross-file exported sub — `:tag` | ✓ | ✓ | symmetric |
| Renamed import `name => {-as=>'x'}` | partial | partial | **ASYMMETRIC (minor)** — finding #6 |
| Package/class name + `use` stmt | ✓ | ✓ | symmetric |
| `use parent`/`use base` parent ref | ✓ | ✓ | symmetric |
| Non-overridden inherited method (`$child->m`) | ✓ | ✓ | symmetric |
| Overridden method (base + child both `sub m`) | subset | subset | **ASYMMETRIC** — finding #5 |
| Lexical variable | ✓ | ✓ | symmetric (single-file by design) |
| `our`/package variable | decl-only | decl-only | symmetric-but-broken — see §4 |
| Corinna `field` (decl/ctor-key/reader, cross-file) | ✓ | ✓ | symmetric |
| Blessed-hash key ↔ internal slot | ✓ | ✓ | symmetric (fix `3a0ebff`) |
| Moo `has` internal slot `$self->{attr}` | reaches slot | slot→group fails | **ASYMMETRIC** — finding #2 |
| Moo `has` mapped accessors (`has_x`/`clear_x`) | renames them | accessor→group fails | **ASYMMETRIC** — finding #2 |
| Moo `has` ctor-key / accessor-call / decl | ✓ | ✓ | symmetric (among themselves) |
| Return-hash key (`HashKeyOfSub`), **cross-file** | ✓ | ✓ | symmetric (finding #3 — fixed) |
| Mojo-events handler (`->on`/`->emit`) | 1-of-N in-file | 0 edits | **ASYMMETRIC + lossy** — finding #1 |
| DBIC column (def/accessor/search-key) | HashKeyOfClass | Method | **ASYMMETRIC** — finding #4 |

---

## 3. Cross-cutting root cause — and why the gate is likely STALE

Rename runs **two different resolution engines** depending on
`TargetRef::supports_cross_file_rename()` (`src/resolve.rs`):

```rust
pub fn supports_cross_file_rename(&self) -> bool {
    matches!(self.kind, TargetKind::Sub { .. } | TargetKind::Method { .. } | TargetKind::Package)
}
```

- **true** (Sub / Method / Package) → `resolve_symbol` → `refs_to`: matches by
  **(owner, scope) structure** across the whole workspace + index. Symmetric,
  cross-file by construction.
- **false** (HashKey* / Handler) → falls to single-file `rename_at` →
  `find_references(point, None)` → `resolve_target_at`: matches by
  **`resolves_to` symbol-identity within one file**. This (a) misses sibling
  occurrences carrying *different* synthesized symbols (stacked `->on`
  registrations, name-mapped accessors), and (b) returns nothing from a cursor
  with no local def symbol (a consumer file).

References does **not** hit this — its `Target` arm uses `refs_to` even for
hash keys. That's the whole asymmetry in one sentence: **references walks hash
keys cross-file; rename does not, solely because of this predicate.**

### Archaeology: the gate is a frozen "for now" stopgap, not a decision

`git` says `supports_cross_file_rename` was introduced **once**, in
`eea96f2` ("land resolve_symbol", **2026-06-10**), and never revisited. That
commit *centralized* rename policy onto the target (rule #10) — but it
codified the **then-current behavior**, it didn't reason about it.

Looking at the parent commit `eea96f2~1`, the per-handler code it replaced:

- **references** for a `HashKey` whose owner resolved at build time
  (`HashKeyOwner::Sub`/`Class`) **already did a cross-file walk via `refs_to`**.
  Only owner-unresolved (lexical) keys fell back to single-file.
- **rename** had, verbatim:
  ```rust
  Some(RenameKind::Handler { .. }) => {
      // Cross-file handler rename not yet wired — fall back to
      // single-file for the moment.
      ...
  }
  Some(RenameKind::HashKey(_)) => {
      // Hash keys: single-file for now.
      ...
  }
  ```

So the single-file rename path was an explicit **"for now" / "not yet wired"
TODO**, while the cross-file machinery it needed (`refs_to` over
`HashKeyOfClass`/`HashKeyOfSub` targets) **already existed and was exercised by
references**. The refactor turned that TODO into an innocuous-looking predicate
that reads like principled policy.

**Timeline:**
- 2026-04-17 (`f94ba2d`) — `refs_to` + RoleMask cross-file references land.
- 2026-06-10 (`eea96f2`) — `supports_cross_file_rename` gate freezes the
  "for now" rename behavior.
- 2026-06-14 (`a0aa529`) — graph-walking / `EdgeKind` proper lands.

The gate predates proper edges by four days, and postdates the cross-file refs
machinery by ~8 weeks. There is **no surviving principled reason** for hash
keys / handlers to be single-file: the group-based hash-key kinds (Corinna
`field`, Moo `has`, and now bless keys via fix `3a0ebff`) **already rename
cross-file** through `group_rename_edits → refs_to`. The gate is the only thing
forcing *plain* `HashKeyOfClass`/`HashKeyOfSub`/`Handler` targets down the
single-file `rename_at` path.

### Concerns that ARE real (and how the existing machinery already answers them)

- *"Hash key names are promiscuous in Perl — cross-file fan-out by name risks
  false matches."* True for **name-only** matching; `refs_to` matches by
  **owner** (`HashKeyOwner::Sub{pkg,name}` / `Class(C)` via `found_by`), not by
  bare name — the same discipline references already relies on.
- *"Resolving an arbitrary `$x->{key}` owner in another file needs `$x`'s type
  cross-file."* That's the deferred-owner seam (`deferred_hash_key_owner`,
  `applicable_dispatches`) which `collect_from_analysis` already invokes at
  query time. Where the owner can't be resolved, the site simply doesn't match
  — no false edit.

**Recommended direction (decide before fixing #1–#4):** route the
owner-resolved hash-key/handler kinds through `refs_to` (masked to EDITABLE),
exactly like references, leaving the single-file `rename_at` path only for
genuinely lexical (`Local`) targets.

### Update — the gate is necessary but NOT sufficient (experiment, this branch)

Flipping the gate per kind and measuring (fixtures in `scratchpad/hk/`, plus
`test_files/plugin_events_*.pl`) revised the picture. **Removing the gate alone
delivers none of the cross-file wins** — it's a symptom, not the lever:

- **Handler** (finding #1): flipping it *does* fan out in-file + cross-file
  symmetrically (def↔emit↔unsubscribe, both stacked `->on` registrations) —
  BUT every edit spans the event name *with its surrounding quotes*
  (`'connect'` → `RENAMED`, dropping the quotes and breaking the literal). The
  name span is `args[0].span` from `frameworks/mojo-events.rhai` (the whole
  string-literal node). So the gate's "not wired yet" was partly *real*:
  handler rename needs a rename-ready inner-name span (and matching
  `prepareRename`) before it can be unblocked. This is true even single-file —
  the old path was already quote-stripping, just on one site.
- **Hash keys** (findings #2/#3): flipping `HashKeyOfClass`/`HashKeyOfSub`
  changed the cross-file result **not at all**. Renaming a return-hash key from
  the producer def still doesn't reach the consumer access, because the
  producer's key is owned `Sub{Some("Cfg"), get_config}` while the consumer's
  enriched access is owned `Sub{None, get_config}` (`enrich_imported_types_
  with_keys` stamps imported subs with `package: None`). The `HashKeyOfSub` arm
  in `collect_from_analysis` requires `package` *and* `name` to match, so the
  two never link. The phantom `(0,0)` synthetic-def edit (finding #3b) is also
  present **with the gate either way**. In-file rename stays correct after the
  flip (the three landed fixes already made it symmetric via the single-file
  path; `refs_to` returns the same set), so the flip is *safe* but inert.
- One test, `resolve::tests::test_resolve_symbol_owned_hash_key`, explicitly
  asserts `!supports_cross_file_rename()` for a `HashKeyOfClass` — it pins the
  in-file-only assumption and must be updated when the gate moves.

**Revised plan — fix the real blockers first, then drop the gate per kind as
the *last* step of each:**

1. **Hash keys cross-file** (finding #3): reconcile consumer owner identity —
   either stamp the enriched/synthetic access owner with the *producer's*
   package (`Sub{Some(pkg),f}`), or relax the `HashKeyOfSub` match to treat a
   `None` package as a wildcard for a known imported sub. Then suppress the
   zero-span synthetic def (#3b). *Then* flip the gate for `HashKeyOfSub`/
   `HashKeyOfClass` and update the pinning test. In-file already symmetric.
2. **Moo slot / mapped accessors** (finding #2): add the `field_group_at` arm
   (`HashKeyAccess{owner:Class(C)}` → group) + mapped-accessor→attr reverse
   index. This routes through the `Group` path (already cross-file), so it may
   not even need the gate change.
3. **Handlers** (finding #1): give event names a rename-ready inner-name span
   (plugin emit + `prepareRename`), *then* flip the gate for `Handler`.

The shared end-state is still "rename == references" — but each kind needs its
own blocker cleared; the gate is the final flip, not the fix.

---

## 4. Findings, ranked by severity

Repros use `./target/release/perl-lsp --rename <root> <file> <row> <col> <new>`
(0-based row/col). Fixture workspace + diff harness left at
`scratchpad/ws/` + `scratchpad/sym.pl` (under the session scratchpad).

### #1 — Mojo-events handler rename is lossy and direction-broken — HIGH
Handlers are `HashKeyOfClass` → `supports_cross_file_rename=false` → single-file
`rename_at` → symbol-identity `resolve_target_at`. References is fully symmetric
(all 6 sites both directions); rename is not.
- From def `test_files/.../Producer.pm:31:15` → **1 edit** (line 32 only; misses
  lines 44, 60 in the *same file* — each stacked `->on` mints a distinct
  synthesized symbol, and the symbol-identity path collects only the one under
  the cursor).
- From consumer `test_files/plugin_events_consumer.pl:41:14` → **0 edits** (no
  local def symbol to resolve to).
- **Fix:** route `Handler` rename through `refs_to` (it already supports the
  `Handler` target + `applicable_dispatches`), i.e. the §3 gate change.

### #2 — Moo `has` internal slot + mapped accessors don't reverse-map to the attr group — HIGH
The `has`-decl, accessor-call, ctor-key, and consumer-accessor cursors mint the
full 7-member group. But:
- Internal slot `$self->{size}` → **2 edits** (slot + `has` token). References
  confirms: `has`-decl finds 7, slot finds 1.
- Mapped accessor `$m->has_size` / `$m->clear_size` → **2 edits** each (call +
  `has` token).
- **Root cause:** `field_group_at` (`src/file_analysis.rs:5278`) has no arm for
  a `HashKeyAccess{owner: Class(C)}` cursor, and its method-call arm only
  matches when the bare method name *is* the attr name (so name-mapped
  `has_size`/`clear_size` find no `attr_pair_group`).
- **Fix:** add a `field_group_at` arm for `HashKeyAccess{owner:Class(C)}` (look
  up attr/field by name+class → group); add a reverse index mapped-accessor
  method name → owning attr so a `has_size` cursor finds the `size` group.
  (Contrast: bless keys are symmetric because key + slot share one synthesized
  symbol/owner.)

### #3 — Return-hash key cross-file + phantom `0:0` edit — FIXED (total unification)
**Resolved** by unifying owner identity and removing the consumer-side stub:
- Enrichment stamps the consumer access owner with the **producer's package**
  (`Sub{Some("Cfg"), get_config}`) instead of the lossy `None` — so producer
  and consumer agree on the owner.
- The consumer-side synthetic `HashKeyDef` stub is **gone**. The producer's
  real def is the single source: completion reaches it cross-file
  (`complete_hash_keys_for_owner` walks the index for a `Sub` owner via
  `imported_sub_keys`); rename/references/goto-def reach it via the owner edge.
  No stub → no phantom `0:0` by construction.
- For a producer-origin rename, the consumer lives in an **unenriched**
  workspace file (owner `Variable`), so `deferred_hash_key_owner` grew a
  function-call-binding arm: `$c = get_config(); $c->{key}` re-derives
  `Sub{Some(pkg), get_config}` at query time through the index — the same lazy
  seam method dispatch + deferred owners use. `collect_from_analysis` now tries
  it for `None` *and* `Variable` owners.
- `HashKeyOfSub` flipped into `supports_cross_file_rename`.

Zero special-casing (the user's bar): one owner identity, one source, one lazy
resolver shared by eager (open) + lazy (workspace) paths. `HashKeyOfClass`
(finding #2) and `Handler` (finding #1) remain single-file.

Original report retained below for context.

### #3 (orig) — Return-hash key cross-file + phantom `0:0` edit — MEDIUM
`Config1::get_config` returns `{ host => …, port => … }`; consumer does
`$cfg->{host}`.
- From def `Config1.pm:7:13` → 1 edit (def only; `HashKeyOfSub` is
  `supports_cross_file_rename=false`, so never reaches the consumer access).
- From consumer `cfgcons.pl:7:14` → 2 edits: the real access **plus a spurious
  `0:0–0:0` edit**. References shows the same phantom.
- **Two defects:** (a) cross-file half is the §3 gate; (b) the phantom is
  independent — `enrich_imported_types_with_keys` injects a synthetic
  `HashKeyDef` into the consumer with a zero/origin `selection_span`, and both
  references and rename surface it as a decl location.
- **Fix (b):** flag enrichment-synthetic HashKeyDefs (or give them an
  unmatchable span) and exclude them from reference/rename decl emission in
  `collect_from_analysis` / `collect_refs_for_target`.

### #4 — DBIC column vs accessor resolve to different target kinds — MEDIUM
`add_columns` synthesizes a column hash-key *and* an accessor method, but they
aren't joined into one group (unlike Corinna). Column-def cursor →
`HashKeyOfClass` (and via `found_by` lands on `add_columns`/search-arg spans,
even cross-file into `oop_playground.pl`); `$user->name` accessor cursor →
`Method{class}` (accessor calls only). Disjoint sets each direction.
- **Fix:** synthesize a projection group for DBIC columns the way Corinna
  fields / Moo `has` ctor-key do (column-def ↔ accessor-call ↔ search-arg-key).

### #5 — Overridden-method rename splits into two disjoint groups — MEDIUM (partly by-design)
`shared_method` defined in both `Base1` and `Child1`. Base1-def + the
`SUPER::shared_method` call form one group (`Method{Base1}`); Child1-def +
`$self->shared_method` + consumer `$c->shared_method` form another
(`Method{Child1}`). `method_rename_chain` deliberately stops at the first
defining ancestor (documented: an overriding intermediate is "a different
method"). Internally consistent, but a single logical rename needs two
operations and silently breaks the override if only one is done.
- **Decision needed:** treat an override + its base as one renameable unit
  (union both classes' chains) vs. keep strict. Not obviously a bug.

### #6 — Renamed-import `-as` origin token not updated — LOW (suspected-minor)
Renaming the source sub `beta_optional` does not rewrite the origin-name token
in `use Exporter1 beta_optional => {-as=>'renamed_beta'}` (consumer), leaving a
dangling reference. Renaming from the local alias `renamed_beta()` touches only
the call, not the spec string. Rare syntax; low priority.

---

## 5. Not bugs / out of scope
- **Lexical variables** — single-file by design (`ResolvedTarget::Local`).
- **`our`/package variable** — rename touches only the decl, never the in-file
  uses (`resolves_to` unset for `our`-var uses). *Symmetric* across cursors
  (consistently incomplete), so not a rename **asymmetry** — but a real
  correctness gap (renaming a package var breaks its uses). Possibly intentional
  caution (package vars are reachable cross-file as `$Pkg::var`). Worth a
  separate look.
