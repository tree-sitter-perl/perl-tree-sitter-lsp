# Authoring perl-lsp plugins

> Audience: someone who wants their custom Perl framework to get the same
> editor intelligence the LSP gives Mojolicious, Moo, or DBIC — without
> learning Rust or rebuilding the binary.

## What a plugin is

A plugin is a single `.rhai` file. The LSP loads it at startup, and at
parse time it gets called with a context describing each Perl function
call, method call, or `use` statement. The plugin returns a list of
**emissions** (symbols, refs, hash-key defs, …) that flow through the
exact same pipeline as the LSP's native analysis. Completion, goto-def,
hover, rename, and inheritance walks all compose over plugin emissions
for free.

Plugins are pure functions: they observe context, they don't mutate
builder state. That's how they stay testable as ordinary Rhai functions
and how a broken plugin fails its own emissions without breaking the
host.

For the design rationale (why Rhai, namespace tagging, query vs. emit
hooks), read [`prompt-plugin-architecture.md`](prompt-plugin-architecture.md).
This doc is the **author's** view: write, validate, snapshot, ship.

## Your first plugin in 30 lines

`my-helpers.rhai`:

```rhai
// Required: identifier the LSP stamps on every emission this plugin produces.
fn id() { "my-helpers" }

// Required: when does this plugin fire?
//   - UsesModule(M) — current package has `use M`
//   - ClassIsa(P)   — current package has parent equal to or under P::
//   - Always        — every package; filter inside the hooks
fn triggers() {
    [ #{ UsesModule: "MyApp::Helpers" } ]
}

// Optional: react to function calls (top-level barewords). Returns an
// array of emissions — `[]` means "nothing to contribute here".
fn on_function_call(ctx) {
    if ctx.function_name != "register_widget" { return []; }
    if ctx.args.len() < 1 { return []; }

    let arg0 = ctx.args[0];
    if arg0.string_value == () { return []; }   // not a literal — skip

    // Synthesize a method on the current package whose name comes from
    // the first string arg. After this, `$obj->arg0_value(...)`
    // completes, hovers, and gotos-def back to `register_widget`'s span.
    [
        #{
            Method: #{
                name: arg0.string_value,
                span: arg0.span,
                selection_span: arg0.span,
                params: [],
                is_method: true,
                return_type: (),
                doc: (),
                on_class: (),
                display: (),
                hide_in_outline: false,
                opaque_return: false,
                outline_label: (),
            }
        }
    ]
}
```

Drop it in your plugin directory (see below), restart the LSP — done.

## Distribution & discovery

The LSP loads plugins from two sources:

1. **Bundled plugins** — `frameworks/*.rhai` in this repo, compiled into
   the binary. Use these as worked examples.
2. **User plugins** — every `.rhai` file in the directory pointed to by
   `$PERL_LSP_PLUGIN_DIR`.

```bash
export PERL_LSP_PLUGIN_DIR=~/.config/perl-lsp/plugins
mkdir -p "$PERL_LSP_PLUGIN_DIR"
cp my-helpers.rhai "$PERL_LSP_PLUGIN_DIR"/
```

The plugin directory is scanned **once** on the first build of the
process. In LSP mode that's effectively startup; in CLI mode it's the
first file processed. **Adding or modifying a plugin requires the LSP
to be restarted.** Use `--plugin-run` (below) to iterate without that
loop.

Plugins are loose `.rhai` scripts today — no manifest, no version,
no per-plugin enable/disable. If you publish a plugin, ship the
single file plus a README; users drop it into their plugin dir.

### Cache invalidation

The LSP keeps a per-workspace SQLite cache of analyzed dependency
modules at `$XDG_CACHE_HOME/perl-lsp/<hash>/modules.db` (defaults to
`~/.cache/perl-lsp/...`). Cached `FileAnalysis` blobs include
plugin-emitted symbols, so the LSP fingerprints the plugin set on
every startup and clears the cache when the fingerprint changes:

- **Bundled plugin edit** (you rebuilt the binary after touching a
  `frameworks/*.rhai`) → source hash changes → cache clears.
- **User plugin edit / add / remove / rename** in
  `$PERL_LSP_PLUGIN_DIR` → cache clears.

This is what makes plugin QA actually work: edit your plugin, restart
the LSP, and the next build re-runs your plugin against every
workspace + dependency module instead of serving the previous build's
analysis. You'll see one slow startup (full re-resolution), then the
cache rewarms under the new fingerprint.

If you suspect cached state is interfering during deeper debugging:
delete the cache dir manually (`rm -rf ~/.cache/perl-lsp`) — the
fingerprint guard is meant to be invisible, not a thing you opt into.

## The reserved-keyword footgun

Rhai treats some words as reserved. If you write `ctx.call` (the
innermost call frame inside a query hook context), Rhai parses it as
`ctx.call(...)` — a method call — and fails at runtime, **silently**
on the LSP side. The fix is bracket-access:

```rhai
// WRONG — Rhai parse error / silent runtime fail
let frame = ctx.call;

// RIGHT
let frame = ctx["call"];
```

`call` is the only context field today that hits this. If you add new
fields whose names collide with `package`, `fn`, `let`, `if`, `for`,
`while`, `return`, `try`, `throw`, `import`, `export`, `as`, `in`,
`is`, `this`, `const`, `private`, `public`, `loop`, `break`,
`continue`, or `switch` — same workaround.

`perl-lsp --plugin-check` warns on `ctx.call` specifically. Anything
Rhai itself can flag at compile time (typos, syntax) is reported by
the compile step.

## Trigger model

Triggers are filters: the LSP only invokes your hooks for packages
that match. Three shapes:

| Trigger | Fires when… |
|---|---|
| `#{ UsesModule: "M" }` | the current package has `use M` |
| `#{ ClassIsa: "P" }` | any parent equals or is prefixed by `P::` |
| `#{ Always: () }` | every package; gate inside the hook |

`on_use` is the exception — it runs for **every** package regardless of
triggers, because the `UsesModule(X)` filter is false until `use X` has
been processed. Filter on `ctx.module_name` inside the hook.

## Hooks

### Emit hooks (parse time)

Each returns `Vec<EmitAction>` — see `frameworks/*.rhai` for real-world
shapes, and `src/plugin/mod.rs::EmitAction` for the full enum.

| Hook | Fires on |
|---|---|
| `on_function_call(ctx)` | `bar(...)`, `bar 'a', 'b'` (top-level function calls) |
| `on_method_call(ctx)` | `$x->bar(...)`, `Foo->bar(...)` |
| `on_use(ctx)` | `use Foo qw(...)` |

The most common emissions:

- **`Method`** — synthesizes a method symbol on the current package
  (or `on_class` to attach to another class). After emission,
  `$obj->name` completes and goto-defs to your span.
- **`Handler`** — a string-dispatched callable (event names, route
  names, task names). `dispatchers` lists the method names that
  route to it (`["emit", "subscribe"]` for events). `params`
  carries the handler's signature for sig help.
- **`HashKeyDef`** — synthesizes a hash-key definition. Use for
  named option keys (`enqueue` options, `connect` config keys).
- **`MethodCallRef`** — emit a method-call ref at an arbitrary span,
  letting goto-def/rename treat string DSL like
  `->to('Users#list')` as if the user had typed `Users->list()`.
- **`PackageParent`** — register an inheritance edge.
- **`FrameworkImport`** — declare names that show up as if `use`-d
  (suppresses unresolved-function diagnostics).

### Query hooks (cursor time)

Run after the native pipeline, used for shape-dependent answers that
data emission can't express. Each is optional.

- `on_signature_help(ctx)` — return `PluginSigHelpAnswer::Show`,
  `Silent`, `ShowHandler`, or `ShowCallSig`.
- `on_completion(ctx)` — return `PluginCompletionAnswer { items,
  exclusive, dispatch_targets_for }`. `exclusive: true` claims the
  slot (suppresses native completion).

The query context exposes the innermost call frame as `ctx["call"]`
(reserved-keyword workaround required), the cursor's nested container
(array/hash literal), and `current_use_module` for use-line option
completion.

## Helper functions

Registered on the engine; available in every plugin without import.

| Helper | Purpose |
|---|---|
| `owner_sub(pkg, name)` | `HashKeyOwner::Sub` — keys owned by `Pkg::name` |
| `owner_sub_unscoped(name)` | `HashKeyOwner::Sub` with `package: None` |
| `owner_class(class)` | `HashKeyOwner::Class` — keys owned by a class |
| `type_string()` | `InferredType::String` |
| `type_numeric()` | `InferredType::Numeric` |
| `type_hashref()`, `type_arrayref()`, `type_coderef()`, `type_regexp()` | matching primitives |
| `type_class("Foo")` | `InferredType::ClassName("Foo")` |
| `as_invocant_params(params)` | mark first param as the implicit invocant (drops it from sig help) |
| `subspan_cols(base, start_delta, end_delta)` | narrow a span to a column range — useful for picking the method-name half of `"Ctrl#action"` |

## CLI authoring tools

Three subcommands, all standalone (no LSP restart, no Rust toolchain).

### `--plugin-check <file.rhai>`

Compile, surface errors, lint footguns. Use as the first step when a
plugin "isn't doing anything" — most loading failures show up here.

```bash
perl-lsp --plugin-check my-helpers.rhai
# my-helpers.rhai
#   id:        my-helpers
#   triggers:  UsesModule("MyApp::Helpers")
#   hooks:     on_function_call
#   ok
```

Add `--format json` for CI use.

### `--plugin-run <file.rhai> --on <fixture.pl>`

Apply your plugin (and **only** your plugin) to one Perl file, dump the
emissions as JSON. The output is a diff against a baseline build with
no plugins, so you see exactly what your plugin contributes.

```bash
perl-lsp --plugin-run my-helpers.rhai --on examples/widgets.pl
```

Use during iteration: edit the plugin, re-run, eyeball emissions,
repeat. No LSP restart, no editor in the loop.

### `--plugin-test <plugin-dir> [--update]`

Snapshot harness. Layout:

```
plugin-dir/
  my-helpers.rhai
  tests/
    widgets.pl
    widgets.expected.json   ← captured snapshot
    nested.pl
    nested.expected.json
```

For each `.pl` fixture, the harness runs the plugin, compares to
`<name>.expected.json`, and reports pass/fail.

```bash
# First run: capture snapshots from current behavior
perl-lsp --plugin-test ./plugin-dir --update

# Subsequent runs: assert no drift
perl-lsp --plugin-test ./plugin-dir
# PASS  ./plugin-dir/my-helpers.rhai on ./plugin-dir/tests/widgets.pl
# 1 pass, 0 fail, 0 updated, 0 captured, 0 load-error
```

Diff is line-based — a single insertion early in the snapshot will
look like a wide divergence (every later line shifts). When that
happens, eyeball the emissions, then `--update`.

## Snapshot testing recipe

1. Author the plugin against a fixture using `--plugin-run`.
2. When emissions look right, lay out the directory with the plugin
   plus `tests/<case>.pl` files covering the edge cases (no-arg call,
   nested call, multi-package file, the case that motivated the
   plugin in the first place).
3. `perl-lsp --plugin-test <dir> --update` to capture initial
   `.expected.json`.
4. Commit `.rhai` + `.pl` + `.expected.json` together.
5. CI: `perl-lsp --plugin-test <dir>` (exits non-zero on drift).

This is exactly how the bundled plugins are tested — the
`tests/<case>.expected.json` files double as readable specs ("what
does this plugin emit on this input").

## Debugging

- **Plugin produces no emissions but compiles fine.** Check
  `RUST_LOG=warn perl-lsp --plugin-run …` — Rhai runtime errors get
  logged at WARN/ERROR. They include the offending function name and
  the underlying Rhai message.
- **`already_emitted` returning early.** `Method` emissions dedup on
  `(name, package, namespace)`. Two emissions with the same name in
  the same package collapse to one. If your plugin should produce
  multiple per call, vary the name (e.g. derive from a string arg).
- **Span out of bounds.** No bounds check today — a plugin emitting
  `row: 999999` will silently corrupt downstream queries. Always
  build spans from `ctx.call_span` / `ctx.args[i].span` /
  `subspan_cols(...)`, never hand-roll row/col numbers.
- **Kill switch tripped.** Default Rhai operation budget is 1M ops
  per call. Heavy plugins can override with `PERL_LSP_RHAI_MAX_OPS`.
  If you're hitting it accidentally, you probably have an infinite
  loop — Rhai's error message names the function.

## What you can't do (yet)

- **Mutate builder state.** Plugins observe context and emit; they
  can't poke the symbol table, rewrite refs, or read other plugins'
  emissions.
- **Plugin-to-plugin coordination.** No event bus, no ordering
  guarantees. If two plugins need to share state, they both read
  the resulting `FileAnalysis` after the build, not from each
  other.
- **Cross-file plugin state.** Plugins are invoked per file. Need
  to remember what module X exported? Read `FileAnalysis` like the
  LSP does.
- **Hot reload.** Editing a plugin requires an LSP restart. Use
  `--plugin-run` for the fast iteration loop.

## Reading list

- `frameworks/data-printer.rhai` — small (~200 lines), exercises
  `on_use`, `on_completion`, and use-line option completion. Best
  starter example.
- `frameworks/mojo-events.rhai` — `Handler` + `DispatchCall`
  emissions, the canonical event-emitter pattern.
- `frameworks/minion.rhai` — most complex bundled plugin: emit hooks
  + query hooks + the `ctx["call"]` workaround in action.
- `docs/prompt-plugin-architecture.md` — design rationale, namespace
  semantics, what's intentionally out of scope.
- `src/plugin/mod.rs` — authoritative type definitions for
  `EmitAction`, `CallContext`, query-hook answer shapes.
