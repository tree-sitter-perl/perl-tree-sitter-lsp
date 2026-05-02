# ADR: Framework plugin system

Framework intelligence (Mojo, Moo, Minion, DBIC, …) is open-ended. Hardcoding
each in the builder is a losing battle, and shipping a new framework would
otherwise need a Rust rebuild per author. Plugins live in `frameworks/*.rhai`
(bundled, `include_str!`'d into the binary) and `$PERL_LSP_PLUGIN_DIR/*.rhai`
(user). Plugin emissions are indistinguishable from native ones — completion,
goto-def, hover, rename, inheritance walks compose for free.

## Decisions worth keeping

### Pure-function plugin model

A plugin is `CallContext → Vec<EmitAction>`. **No `&mut Builder` ever crosses
the script boundary.** Scripts inspect context and return emissions; they
can't poke scope stacks, rewrite type constraints, or read other plugins'
state. Consequences:

- Plugins are unit-testable as plain functions.
- A broken script fails its own emissions; it can't break the host.
- Plugin-to-plugin coordination has exactly one channel: the resulting
  `FileAnalysis`. No event bus, no ordering guarantees.

When a plugin needs something `EmitAction` doesn't expose, **grow the enum**
— don't hand out raw builder access.

### Why Rhai (not TOML, not a custom DSL)

Evaluated TOML, embedded Rhai, hybrid TOML+compiled-Rust. TOML lost on
expressivity (every escape hatch grows into a mini-language). Hybrid lost on
split-brain UX. Rhai won on:

- Rust-native, no FFI, no sandbox worries.
- Familiar enough to Perl developers.
- Turing-complete — any framework shape we haven't foreseen is expressible.
- Pure-function model means scripts unit-test without mocking the builder.

### Namespace tagging on every emission

`Symbol.namespace: Namespace` (`Language` for native, `Framework { id }` for
plugins). Default behavior is blind to it; provenance-sensitive features
(completion bucketing, diagnostic attribution, coordinated rename) read it.
The enum is intentionally small so it can grow without migration.

### Triggers are filters, not logic

Three shapes: `UsesModule(M)`, `ClassIsa(P)`, `Always`. Receiver type / arg
shape / fold-success decisions live **inside** `on_*` callbacks. The tradeoff
is simplicity: triggers are one index lookup per call-site.

`on_use` is the exception — it runs for every package regardless, because the
`UsesModule(X)` filter is false until `use X` has been processed.

### Type overrides are priority witnesses, not field writes

A plugin's `overrides()` manifest asserts return types inference can't reach
(motivating case: `Mojolicious::Routes::Route::_route` returns `$self` via an
`@_`-shift / array-slice idiom we don't model).

Application semantics:

- **Trigger-independent.** Overrides apply at every build's post-pass
  regardless of which packages the file uses, so the home module picks them
  up even when it doesn't `use Mojolicious` itself.
- **Read once at compile time.** No runtime call cost; cached on the plugin
  struct.
- **Priority short-circuit.** `WitnessSource::priority()` returns 100 for
  `Plugin`, 10 for everything else. The `PluginOverrideReducer` runs first in
  the registry and dominates per-arm folds. There is **no** `if
  is_plugin_override { skip }` branch in the writeback — priority handles it.
- **Provenance recorded.** `FileAnalysis.type_provenance` keyed by `SymbolId`
  carries `PluginOverride { plugin_id, reason }`. Surfaces via
  `--dump-package`. The free-form `reason` is how a future reader answers
  "why does the LSP think `_route` returns X?" without re-running the build.

### Reserved-keyword footgun (Rhai)

Rhai parses `ctx.call` as a method call, not a field read. Reads that collide
with reserved keywords use bracket access: `ctx["call"]`. Map literals with
reserved keys use bracket-insert: `m["package"] = …`. `--plugin-check`
warns on `ctx.call`; other collisions surface as cryptic Rhai parse errors.

### Failure mode: silent drop

`RhaiPlugin::dispatch` logs failures at `log::error!` and returns empty
results. A plugin bug (bad emission shape, parse error, kill-switch trigger)
drops the emission — the user sees "my plugin isn't doing anything." Diagnose
with `RUST_LOG=error`.

Kill switch: `make_engine()` sets `max_operations = 1_000_000` (overridable
via `PERL_LSP_RHAI_MAX_OPS`). Third-party plugins get the same limit — no
trust boundary between bundled and user scripts.

## What's intentionally out of scope

- **Full builder API exposure.** Plugins emit through `EmitAction`. Period.
- **Plugin-to-plugin communication.** No event bus. Read each other's results
  from `FileAnalysis` after the build, or don't.
- **Cross-file plugin state.** Plugins are invoked per file; no persistent DB.
  If a plugin needs to remember module X's exports, it reads `FileAnalysis`
  the same way the LSP does.
- **Hot reload.** Editing a plugin requires an LSP restart. Plugin
  fingerprint hashing clears the SQLite module cache on startup when it
  changes, so cache doesn't serve stale plugin output.

## Reading list

- `src/plugin/mod.rs` — authoritative type definitions for `EmitAction`,
  `CallContext`, `Trigger`, query-hook answer shapes.
- `frameworks/*.rhai` — six bundled examples; `mojo-routes.rhai` exercises
  `MethodCallRef` + `overrides()`, `minion.rhai` exercises `on_completion` +
  `on_signature_help` + the `ctx["call"]` workaround.
- `docs/PLUGIN_AUTHORING.md` — author-facing reference (write, validate,
  snapshot, ship).
