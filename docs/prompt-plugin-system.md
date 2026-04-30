# Framework plugin system

**Status:** Phase 1 (namespace) + plugin machinery + Handler/DispatchCall abstraction landed. Four bundled plugins covering the core Mojolicious surface:

| Plugin | Pattern | Emits | Drove |
|---|---|---|---|
| `mojo-events` | `$emitter->on('evt', sub)` / `->emit('evt', ...)` | Handler + DispatchCall | Initial Handler abstraction; string-dispatch sig help; stacked registrations |
| `mojo-helpers` | `$app->helper('name' => sub)`, `'foo.bar'`, `'a.b.c'` | Method (on Mojolicious::Controller; synthesized proxy classes for dotted chains) | `on_class` override; per-(name, package, namespace) Method dedup |
| `mojo-routes` | `->to('Ctrl#act')`, `->to(controller=>..., action=>...)` | MethodCallRef | Plugin-emitted refs at arbitrary spans (strings as method-call sites) |
| `mojo-lite` | `get '/path' => sub`, `post`, `any`, `websocket`, ... | Handler (keyed by URL path, dispatched via `url_for`) | `on_function_call` hook dispatch |

## Motivation

Framework support in a Perl LSP is an open-ended set. Moo, Moose, Mojo::Base, DBIC, Catalyst, Mojolicious, Test::Class::Moose, and dozens of in-house frameworks all have their own ways of declaring attributes, events, routes, roles, and columns. Hardcoding every one of them in the builder is a losing battle — and shipping a new framework would otherwise require a Rust compile step for every plugin author.

The plugin system lets anyone add framework intelligence by dropping a `.rhai` script into a plugin directory. Plugin emissions are indistinguishable from native ones: const folding, cross-file references, rename, hover, completion, and inheritance walks all compose over plugin-synthesized symbols for free. That "everything must JUST flow" invariant is the whole point — if a plugin has to reimplement cross-file resolution, we've designed it wrong.

## Architecture

```
  Rhai script  ──emit list──▶  EmitAction  ──apply_emit_action──▶  FileAnalysis
   (pure fn)                     (serde)                            (native)
```

A plugin is a pure function from `CallContext` → `Vec<EmitAction>`. The host:
1. Compiles the script once via `plugin::rhai_host::make_engine()` + `RhaiPlugin::from_source`.
2. Reads `id()` and `triggers()` at load time — no runtime call cost for irrelevant packages.
3. At each call-site, asks the registry for applicable plugins (via `TriggerQuery`), serializes the read-only `CallContext` into a Rhai `Dynamic`, calls `on_method_call` / `on_function_call`, deserializes the returned array of action object-maps back into `EmitAction` values.
4. `apply_emit_action` converts each action into an `add_symbol_ns` / `framework_imports.insert` / etc., stamping `Namespace::Framework { id: plugin_id }` on every synthesized symbol.

No mutable reference to `Builder` ever crosses the script boundary. Scripts can't silently corrupt builder state — the contract is "inspect context, return emissions." This keeps scripts testable as plain functions and makes error budgets finite: a broken script fails its own emissions, it can't break the host.

## Namespace tag

Every symbol carries a `namespace: Namespace` field (`file_analysis.rs`):

```rust
pub enum Namespace {
    Language,                           // native Perl
    Framework { id: String },           // plugin-attributed
}
```

Built-in builder emissions default to `Language`. Plugin emissions stamp their `id()`. This is Phase 1 of the namespace-widening work; the enum is intentionally small so we can grow it later without a migration. Phase 2+ may add `Custom { id }` or per-feature sub-tags once we see what queries want to filter on.

Downstream features read `namespace` where provenance matters — completion bucketing, diagnostic attribution, coordinated rename — but the default behavior is blind to it. An LSP query that doesn't care where a symbol came from keeps working unchanged.

## Trigger contract

Triggers decide *which packages* a plugin fires on. They are deliberately narrow:

- `UsesModule("Mojo::Base")` — fires when the current package has `use Mojo::Base`.
- `ClassIsa("Mojo::EventEmitter")` — fires when any parent equals or is prefixed by `Mojo::EventEmitter::`.
- `Always` — unconditional; plugin filters internally.

Complex decisions (receiver type, arg shape, constant-fold success) belong inside `on_*` callbacks, not in triggers. The tradeoff is simplicity: triggers are one index lookup per call-site, and any plugin that wants richer gating can check `ctx.receiver_type`, `ctx.current_package_uses`, etc. inside its body.

## Plugin script contract

A plugin is a Rhai file that defines:

```rhai
fn id() { "my-plugin" }               // identifier, stamped on Namespace
fn triggers() { [ ... ] }             // array of Trigger object-maps
fn overrides() { [ ... ] }            // optional — static type-override manifest
fn on_method_call(ctx) { [ ... ] }    // optional — returns EmitAction array
fn on_function_call(ctx) { [ ... ] }  // optional
fn on_use(ctx) { [ ... ] }            // optional — reserved for future
```

Each callback returns an *array* of emissions (possibly empty). Object-map shape matches the `EmitAction` enum's serde representation; helpers like `owner_class(name)`, `owner_sub(pkg, name)`, and `type_class("Foo")` are registered on the engine so scripts don't have to hand-craft enum discriminants.

## Type overrides — `overrides()` manifest

Some idioms inference can't (or shouldn't) reach: Mojolicious'
`Mojolicious::Routes::Route::_route` returns `$self` via an `@_`-shift
followed by an array slice; modeling that requires array inference we
haven't built. Plugins assert the right answer via a static
`overrides()` manifest:

```rhai
fn overrides() {
    [
        #{
            target: #{ Method: #{
                class: "Mojolicious::Routes::Route",
                name: "_route",
            }},
            return_type: #{ ClassName: "Mojolicious::Routes::Route" },
            reason: "_route returns $self via @_-shift; inference doesn't model array-slice returns",
        }
    ]
}
```

**Targets** are `Method { class, name }` (matched by exact home-class
package name on the symbol — no inheritance walk; the override is
about where the sub is *defined*, subclasses inherit it through the
existing cross-file path) or `Sub { package, name }` (`package: ()`
matches top-level/script subs).

**Return type** is any `InferredType` shape — `ClassName(...)`,
`HashRef`, `ArrayRef`, etc. Use `ClassName` over `FirstParam` when you
can: same downstream semantics (both flow through `is_object()` /
`class_name()` and pin chain receivers identically) but flatter Rhai
syntax with no reserved-keyword bracket-insert workaround.

**Reason** is free-form prose. It surfaces in
`TypeProvenance::PluginOverride.reason` for debug introspection — keep
it explanatory, this is how a future reader answers "why does the LSP
think `_route` returns `Mojolicious::Routes::Route`?" without
re-running the build.

### Application semantics

- **Trigger-independent.** Overrides are *not* gated by `triggers()`.
  They apply at every build's post-pass regardless of which packages
  the file uses, so `Mojolicious::Routes::Route.pm`'s build picks up
  the `_route` override even though that module doesn't itself
  `use Mojolicious`.
- **Read once at plugin compile time.** No runtime call cost — the
  host calls `overrides()` once during `RhaiPlugin::from_source` and
  caches the list on the plugin struct.
- **Wins over inference.** `apply_type_overrides` runs *before*
  `populate_witness_bag` and the worklist fold; each override pushes a
  Plugin-priority `InferredType` witness onto the target Symbol. The
  `PluginOverrideReducer` short-circuit (`WitnessSource::priority()`
  returns 100 for Plugin, 10 for Builder) makes that witness dominate
  every per-arm fold the inference pipeline runs — overriding an
  existing inferred type is the whole point ("inference reaches the
  wrong answer here"). Provenance is recorded in
  `FileAnalysis.type_provenance` so the inferred answer isn't lost to
  the debugger.
- **Cross-file falls out for free.** When the resolver builds the
  home module, the override patches the cached symbol's
  `return_type`. User code's `find_method_return_type(class, name)`
  reads the patched cached symbol — no changes to lookup logic, no
  separate override table consulted at query time.

### Provenance — `FileAnalysis.type_provenance`

Sidecar map `HashMap<SymbolId, TypeProvenance>` keyed by symbol id.
Two variants:

- `Inferred` — the default. Never stored; missing entry == inferred.
- `PluginOverride { plugin_id, reason }` — the override fired here.

Read via `fa.return_type_provenance(sym_id)` (always returns a value,
never `None`). Round-trips through bincode along with the rest of
`FileAnalysis`. Consumers today are tests + future debug inspectors;
nothing on the hot path branches on provenance, so keeping the common
case zero-cost matters.

The map is modeled this way deliberately: an alternative was wrapping
`InferredType` in a `Typed { ty, provenance }` struct, but provenance
is interesting only for the small minority of types that came from
overrides — wrapping the type itself would have rippled through every
return-type callsite for a feature only debug introspection consumes.

### Loading

- **Bundled:** `const BUNDLED` in `src/plugin/rhai_host.rs` lists scripts `include_str!`'d into the binary. Edit this list to ship new built-in plugins.
- **User-installed:** `$PERL_LSP_PLUGIN_DIR` env var points at a directory of `.rhai` files. Loaded at first `build()` call and cached process-wide.

### Testing

A plugin's `on_*` functions are pure — tests pass in a hand-built `CallContext`, call `plugin.on_method_call(&ctx)`, assert the returned `Vec<EmitAction>`. See `src/plugin/rhai_host.rs::tests` for the pattern. End-to-end coverage lives in `src/builder.rs::tests::plugin_mojo_events_*` — real Perl source parsed through the builder with the bundled plugin active.

## What's out of scope (intentionally)

- **Full builder API exposure.** Plugins can emit `Symbol`, `Ref`, `HashKeyDef`, `PackageParent`, `FrameworkImport`. They can't poke scope stacks, rewrite type constraints, or mutate `call_bindings` directly. When a plugin needs something we haven't exposed, grow `EmitAction` rather than handing out raw builder access.
- **Plugin-to-plugin communication.** Each plugin's emissions land in the shared `FileAnalysis` — that's the only channel. No event bus, no ordering guarantees.
- **Cross-file plugin state.** Plugins are invoked per file; no persistent plugin DB. If a plugin needs to remember what module X exported, it reads `FileAnalysis` just like the LSP does.

## What composes for free

Because plugin emissions go into the same `symbols` / `refs` / `call_bindings` / `package_parents` tables as native ones:

- **`refs_by_target` index** reverse-indexes plugin-emitted HashKeyDefs for O(1) access → def lookup.
- **Cross-file ref walks** (`resolve::refs_to`) find plugin-emitted symbols in workspace + dependency files with no extra code.
- **Enrichment** synthesizes consumer-side HashKeyDefs for imported plugin symbols (Phase-5 consumer-side fixup already handles `HashKeyOwner::Class` and `HashKeyOwner::Sub` variants uniformly).
- **Inheritance walks** include plugin-emitted `PackageParent` edges, so methods synthesized for a plugin's base class surface in descendant-class completion.
- **Constant folding** is already used inside the plugin (`ArgInfo.string_value`), which means `my $name = 'connect'; $self->on($name, sub {});` resolves and still emits — the plugin doesn't duplicate folding logic.

## Next directions (not in this PR)

- Expose `on_function_call` hook in the builder for `has`-style function calls (currently only method dispatch is wired).
- Port Moo/Moose/Mojo::Base/DBIC from hardcoded Rust to bundled Rhai plugins, once the emission catalog has stabilized through real plugin authoring.
- Richer receiver-type resolution at mid-build so `$obj->on(...)` where `$obj` has an inferred class type also triggers plugins.
- Plugin-provided `Trigger::Custom(String)` with a host-side evaluator — once a real plugin hits a case the current three triggers can't express.

## Why Rhai, not TOML or a custom DSL

Evaluated three options (see conversation log): declarative TOML, embedded Rhai, hybrid TOML+compiled-Rust. TOML lost on expressivity (every escape hatch grows into a mini-language inside the config). Hybrid lost on split-brain UX. Rhai won on:

- Rust-native, no FFI, no sandbox worries.
- Syntax familiar enough to Perl developers to not be a barrier.
- Turing-complete — any framework shape we haven't foreseen is still expressible.
- Pure-function model means scripts are unit-testable without mocking the builder.
