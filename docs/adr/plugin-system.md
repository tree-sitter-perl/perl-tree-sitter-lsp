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

### Declarative manifests (the `overrides()` family)

`overrides()` is the first of a family of **static, trigger-independent
manifests** a plugin declares once (read at load, no per-call cost) and the
*core* applies. They share a shape: the plugin owns the *vocabulary* (which
names/verbs/types mean what); the core owns the *mechanism* (it walks the CST —
rule #1 — and applies the rule). Use a manifest, not an imperative hook, when
the rule is pure data and a plugin couldn't do the work anyway (it can't walk
nodes). The registry unions each manifest across all plugins.

Members today:

- **`overrides()` → `[TypeOverride]`** — priority return-type assertions (above).
- **`dispatch_verbs()` → `[DispatchVerb]`** — "method `verb` on a receiver that
  `isa target_class` dispatches a named handler into `owner_class`'s registry"
  (`enqueue`→Minion tasks). The builder records a `ProvisionalDispatch` per
  matching call; **enrichment** (which has the module index) promotes the ones
  whose receiver `isa` the target — resolved by *receiver type*, cross-file, not
  by the file's `use`s. That's why a `Minion` subclass (`Clove::Minion`) or a
  helper-returned receiver (`$c->minion->enqueue`) lights up where a file-level
  trigger never would. See `file_analysis.rs::promote_provisional_dispatches`.
- **`type_constraint_names()` / `type_constraint_inner(name, params)`** — the
  Type::Tiny `isa` vocabulary. Names gate which call expressions are constraint
  constructors; the core extracts each constructor's params (rule #1) into a
  `[ConstraintParam]` and the plugin *folds* them into the constrained inner
  type. The core wraps the result as `TypeConstraintOf(inner)` and the `has`
  isa path projects the inner onto the accessor. Arity lives in the fold, not
  the core, so a constructor can take 0/1/N params. (`frameworks/type-tiny.rhai`:
  `InstanceOf`/`ConsumerOf`.)
- **`param_types()` → `[ParamType]`** — type a sub's parameter by selector (a
  callback arg, or a method in a role-doer). Applied at the sub-declaration
  walk. (`docs/prompt-param-typing.md`.)
- **`app_surface_consumers()` → `[String]`** — the receiver classes that compose
  the fictional app surface (`file_analysis::APP_SURFACE_CLASS`). The "open
  consumption" axis of the helper/plugin model: each listed class gains the
  surface as a synthetic ancestor in the MRO walk (`file_analysis::parents_of`,
  the single edge-injection seam shared by every parent-enumeration site), so
  entities a plugin bridges to the surface resolve from every consumer
  (`$app->h`, `$c->h`, app subclasses) through the existing ancestor + bridge
  resolution — one bridge target, the consumer set declared once. The builder
  bakes the union onto `FileAnalysis.app_surface_consumers`.
  (`frameworks/mojo-helpers.rhai`; `docs/prompt-app-entity.md`.)

The recurring rule: **a manifest entry is the plugin naming a property; the core
asks the value the question** (rule #10). Adding a member is a trait method
defaulting to empty + a registry union + a Rhai reader (mirror `overrides`),
plus the one core application site.

### Lazy return types via `Edge`

Plugins synthesize callables (`mojo-helpers` for `$app->helper(name => sub
{ ... })`, similar shapes elsewhere) whose return type lives inside the
callback body. At plugin-call time the body hasn't been walked, so eager
inference returns nothing useful — but `Expr(span)` for the body's last
expression IS populated by the time anyone queries the synthesized
callable's return type.

Mechanism: `EmitAction::Method.return_via_edge: Option<Span>`. Plugin
sets it to `args[N].sub_body_last_expr_span` (populated by the builder
on anonymous-sub args). Builder pushes `Symbol(sid) → Edge(Expr(span))`
instead of the usual `Symbol(sid) → InferredType(rt)`. The bag's edge-
chase resolver follows it at query time. Class-scoped synth gets the
edge mirrored to `MethodOnClass{class, name}` by
`write_back_sub_return_types` so cross-file `find_method_return_type`
queries reach it via the class-keyed attachment.

Anonymous subs push a Sub scope so `Expr(body_last)` gets its return-
arm witness; the scope exists for return-arm semantics only — anon
subs remain values, not symbols.

The mechanism composes cross-file with no special case. The synthesized
Method's `MethodOnClass` edge resolves through the cached producer's
bag and edge-chases into the body's `Expr(span)`; the bag's existing
cross-file primary lookup carries the chain through.

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
