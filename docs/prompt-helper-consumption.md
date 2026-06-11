# Helper consumption — scoping & the phase ladder

**Phase 1 LANDED** (crm-qa-sweep): dependency-registered helpers
resolve end-to-end. The gap was a four-layer onion, each layer hiding
the next: (1) the warm-start rebuild fed only the reverse index, never
`bridges_index` — helpers worked on the cold run that resolved them
and vanished every warm session after (B6 class); (2) the resolver
thread never received `bridges_index` at all; (3) nothing literally
`use`s DefaultHelpers — the framework loads it at runtime — so the
import-driven resolver never pulled it in: `use Mojolicious::Lite` now
SyntheticUses DefaultHelpers + TagHelpers, and `plugin 'X'`
SyntheticUses the default-namespace module; (4) `new_for_cli` spawned
NO resolver — CLI sessions could only read what editor sessions had
cached. The headless resolver now serves CLI sessions, and `run_one`
resolves the query file's imports under a 5s global budget (each
resolved module persists, so one-shot runs converge to warm).

## Scoping decision (veesh, June 2026)

- **Framework-loaded plugins** (DefaultHelpers, TagHelpers): apply
  unconditionally — Mojolicious loads them, full stop.
- **Installed/workspace plugins**: "you downloaded it, you probably
  intend to use it" — resolve generously; precision is the
  diagnostic's job, not the resolver's (the exporter-hint pattern).
- **NOT auto-applied**: nothing. The over-offer risk is bounded by a
  few phantom completions from installed-but-unloaded plugins.

## Phase 2 — entrypoint-scan diagnostic (open)

At the USAGE site: "`$c->was_loaded` is provided by
Clove::App::Plugin::WasLoaded, which no entrypoint loads
(`plugin 'WasLoaded'`)". Hint severity. Scan = workspace lite scripts
+ Mojolicious subclasses' startup for `plugin` registrations. This is
a read-only miniature of long-distance collection (task #25) — its
first muscle. Auto-fix code action (insert the `plugin` line) mirrors
auto-import.

## Phase 3 — per-app surfaces (needs graph walking / multi-app)

mojo-helpers' namespace ids are already per-package; when app
attribution exists, the generous union narrows to per-app surfaces
and the phase-2 lint gets exact. Tracked with the graph-walking
pillar.
