# Helper consumption — phases 2–3

Phase 1 (dependency-registered helpers resolve end-to-end, warm and
cold, editor and CLI) landed in the crm QA sweep. The standing policy
from that round:

- **Framework-loaded plugins** (DefaultHelpers, TagHelpers): apply
  unconditionally — Mojolicious loads them, full stop.
- **Installed/workspace plugins**: "you downloaded it, you probably
  intend to use it" — resolve generously; PRECISION is the
  diagnostic's job, not the resolver's.

## Phase 2 — entrypoint-scan diagnostic (LANDED: long-distance-remainder)

At the USAGE site: "`$c->was_loaded` is provided by
Clove::App::Plugin::WasLoaded, which no entrypoint loads
(`plugin 'WasLoaded'`)". HINT severity; fires only for WORKSPACE
plugin modules (installed CPAN plugins keep the generous policy).
Loaded = imported (literally or via SyntheticUse) by any workspace
file, entrypoint scripts included. Auto-fix code action (insert the
`plugin` line) mirrors auto-import — later.

## Phase 3 — per-app surfaces (graph walking)

mojo-helpers' namespace ids are already per-package; when app
attribution exists (branded edges), the generous union narrows to
per-app surfaces and the phase-2 lint gets exact. Tracked with
`prompt-graph-walking.md`.
