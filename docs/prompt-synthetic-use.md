# SyntheticUse: Plugin-Emitted `use` Statements

> Extend the plugin system so plugins can inject `use X args...` as if the
> user had written it. Goal: **trivially support style kits** — `Import::Base`
> subclasses, `ToolKit`, custom company-wide `use Co::Base -Class` shims —
> common in real Perl shops, without growing core knowledge of which
> frameworks exist.

## Why this exists

Real Perl codebases routinely centralize their import dance into a single
"kit" module:

```perl
package My::Class;
use Co::Base -Class;        # → Moo + MooX::late + Co::Types + parent + ...
has name => ( is => 'ro' );
```

`Co::Base` is typically an `Import::Base` subclass with an
`%IMPORT_BUNDLES` map; the user-visible `use` line collapses a dozen
real `use` lines into one. The LSP currently has no way to follow that:
`has` doesn't synthesize an accessor because the package's
`framework_modes` was never set, since no literal `use Moo` appeared.

The instinct is to add `EmitAction::PackageFramework { mode: "Moo" }`.
That's wrong — it pins core to a closed list of framework modes, and
every new framework grows the enum. The right primitive is one level
down: **let the plugin emit a synthetic `use` and let the core's
existing `use`-handling do the rest.** Same machinery the user's real
`use Moo` would have hit. Same plugins fire. Same provenance trail.

## Non-goal

This is not "let plugins set framework_modes." That field is an
implementation detail of the Moo/Moose/Mojo::Base built-in path. A
plugin that wants Moo behavior emits `SyntheticUse "Moo"`; the existing
Moo branch in `visit_use` flips the mode. If a plugin invents a brand
new framework, it ships its own `on_use("ThatFramework")` and emits the
accessor synthesis itself — same path Moo would take if it weren't
built in.

## The new EmitAction

```rust
pub enum EmitAction {
    // ... existing variants

    /// Inject a `use` statement as if it appeared in source at `span`.
    /// Equivalent in every respect to the user having written
    /// `use <module> <args>` at that point — same plugin dispatch,
    /// same framework detection, same Import/Module symbol emission,
    /// same package_uses / package_parents / framework_imports writes.
    SyntheticUse {
        module: String,
        /// Raw arg tokens, exactly as `extract_mojo_base_args` would
        /// produce from a real CST. Includes barewords like `-Class`
        /// and quoted parents like `'Mojolicious::Plugin'`.
        args: Vec<String>,
        /// qw-style imports (the named-symbol list a real
        /// `extract_use_import_list` would yield).
        imports: Vec<String>,
        /// Span the synthetic use is "attributed to" — typically the
        /// emitting plugin's `ctx.span` (the original real `use` that
        /// triggered the kit expansion).
        span: Span,
    },
}
```

All four fields mirror what `visit_use` extracts from a real CST node.
That's deliberate — the synthetic path must be call-compatible with the
real path's worker, with no `synthetic: bool` flag inside the worker
that skips a step.

## Refactor `visit_use` to a value-taking worker

`visit_use` (builder.rs:2742) currently reads everything off the
`use_statement` Node. Split:

```rust
fn visit_use(&mut self, node: Node<'a>) {
    let module = ...;                              // pull from node
    let raw_args = self.extract_mojo_base_args(node);
    let (imports, _qw_close) = self.extract_use_import_list(node);
    let span = node_to_span(node);
    self.process_use(module, raw_args, imports, span);
}

fn process_use(
    &mut self,
    module: String,
    raw_args: Vec<String>,
    imports: Vec<String>,
    span: Span,
) {
    // current body of visit_use, with `node` references replaced by
    // the value-passed equivalents. Mojo::Base's emit_refs_for_strings
    // calls — which currently scope by `node` — take an explicit span
    // instead.
    ...
}
```

`apply_emit_action` for `SyntheticUse { module, args, imports, span }`
calls `process_use` with the same arguments. **One body, two callers,
no flag.**

## Plugin re-entry — required, not skipped

Inside `process_use`, the existing call to `dispatch_use_plugins` runs
unconditionally. A `SyntheticUse "Moo"` emitted by `clove-base.rhai`
fires every registered `on_use` hook — including any user-written Moo
plugin, including bundled framework plugins, including `clove-base.rhai`
itself if it `use`s something it also reacts to.

This is the whole point. Without it, kit plugins are second-class:
their ghost-uses don't compose, and any feature built on plugin
dispatch (provenance, observability, future hook surfaces) silently
fails to apply. With it, a user writing
`use Co::Base -Schema` gets exactly what `use parent 'Co::Common::Schema'`
would have produced — including any `Co::Common::Schema`-aware plugin
the user later writes.

## Termination — per-build seen-set, uniform

Re-entry creates obvious cycle risk: plugin A emits
`SyntheticUse "Moo"`, a hypothetical user plugin reacting to `use Moo`
emits `SyntheticUse "Co::Base"`, A's hook re-fires. The fix is **not**
a depth counter on synthetic vs real (that would be the special-case
the principle rejects). It's a per-build seen-set on the
work-identity tuple:

```rust
struct Builder<'a> {
    // ...
    /// (current_package, module, args) tuples already processed in
    /// this build. Real `use Moo; use Moo;` writes the second insert
    /// to a no-op; SyntheticUse cycles short-circuit at the dispatcher.
    /// Cleared between files.
    use_dedup: HashSet<(Option<String>, String, Vec<String>)>,
}
```

`process_use` checks-and-inserts at the top:

```rust
let key = (self.current_package.clone(), module.clone(), raw_args.clone());
if !self.use_dedup.insert(key) { return; }
```

Real `use` statements participate in the same dedup. `framework_modes`
is a HashMap insert (already idempotent); `package_uses` was a Vec push
(would dup without the gate, harmlessly today, but cleaner with it);
`Import` and `Module` symbol emissions were not deduped before and
benefit from being so. **No path-of-origin distinction anywhere.**

The lookup-key includes `current_package` because the same `(module,
args)` tuple is genuinely distinct work in two different packages —
the framework_modes write, package_uses entry, and plugin dispatch
context all change.

## Provenance — the chain falls out for free

Every emission downstream of a synthetic-use already carries
`Namespace::Framework { id }` from whichever plugin emitted it.
A `has`-synthesized accessor produced via the chain
"user → `use Co::Base -Class` → clove-base plugin →
`SyntheticUse "Moo"` → core Moo branch → has-synthesizer" inherits
the framework id of the **direct** emitter (the has-synthesizer's
namespace), but the trail is preserved in the Span fields:

- `SyntheticUse.span` points at the originating real-use span.
- The synthesized accessor's `span` points at the `has` call.
- `--dump-package` provenance can walk both and report
  "synthesized by Moo's has-detection at line 7, triggered by
  ghost-use of Moo at line 2 (originating: `use Co::Base -Class`)."

Concrete Provenance variant to add when the time comes:

```rust
pub enum TypeProvenance {
    // ... existing
    SyntheticUse {
        plugin_id: String,
        synthesized_module: String,
        origin_span: Span,
    },
}
```

Wired through `Builder.type_provenance` the same way `PluginOverride`
and `Delegation` are today.

## What this enables — the `clove-base.rhai` example

```rhai
fn id() { "clove-base" }
fn triggers() { [ #{ Always: () } ] }

fn on_use(ctx) {
    if ctx.module_name != "Clove::Base" &&
       ctx.module_name != "Clove::Base::Local" { return []; }
    let out = [];
    let bundle = first_bundle_flag(ctx.raw_args);  // "-Class", etc.

    if bundle == "-Class" || bundle == "-Role" || bundle == "-Sheet" {
        out += #{ SyntheticUse: #{
            module: "Moo",
            args: [],
            imports: [],
            span: ctx.span,
        }};
    }
    if bundle == "-Schema" {
        out += #{ SyntheticUse: #{
            module: "parent",
            args: ["Clove::Common::Schema"],
            imports: ["Clove::Common::Schema"],
            span: ctx.span,
        }};
    }
    if bundle == "-Controller" {
        out += #{ SyntheticUse: #{ module: "Moo", args: [], imports: [], span: ctx.span }};
        out += #{ SyntheticUse: #{
            module: "parent",
            args: ["Mojolicious::Controller"],
            imports: ["Mojolicious::Controller"],
            span: ctx.span,
        }};
        // Role composition reuses the parent map, per CLAUDE.md
        // Inheritance & frameworks section.
        out += #{ PackageParent: #{
            package: ctx.current_package,
            parent: "Clove::Role::REST",
        }};
    }
    // ... -Plugin / -Command / -Exception / -Tool / -Builtins / -Types

    out
}
```

The plugin author writes Perl-equivalent statements, not LSP-internal
mode flips. New built-in framework? Plugin doesn't change. Removed
built-in framework (rewritten as a plugin)? Plugin doesn't change.
The plugin's vocabulary is "morally, the user wrote this Perl" — and
that vocabulary is closed.

## Implementation steps

1. Refactor `visit_use` → `process_use(module, args, imports, span)`
   value-taking worker. No behavior change. Snapshot/builder tests stay
   green.
2. Add `Builder.use_dedup: HashSet<(Option<String>, String, Vec<String>)>`,
   gate the top of `process_use`. Verify real-use snapshots unchanged
   (idempotency was already implicit at every write site).
3. Add `EmitAction::SyntheticUse { module, args, imports, span }`,
   match arm in `apply_emit_action` calling `process_use`.
4. Rhai surface: `SyntheticUse` action shape in `rhai_host.rs` action
   parser. Validation in `--plugin-check`.
5. Test fixture: a kit plugin emitting `SyntheticUse "Moo"` produces
   the same `framework_modes` / `framework_imports` / has-synthesis
   output as a literal `use Moo`. Diff the two FileAnalyses; they
   should be identical modulo the `Module` symbol's span.
6. Optional follow-up: `TypeProvenance::SyntheticUse` variant for
   `--dump-package` chain reporting. Not required for v1; add when a
   user asks "why does this method exist?" and the answer is
   non-obvious.

## Out of scope

- **Parsing kit modules' `%IMPORT_BUNDLES` from disk.** Tempting
  ("auto-support every Import::Base kit") but requires evaluating Perl
  data structures that contain coderefs. Per-kit Rhai plugins are
  bounded, testable, and match every other framework plugin.
- **Synthetic emissions for things that aren't `use`.** `SyntheticCall`,
  `SyntheticAssignment`, etc. are not requested by any current plugin.
  Add only when a real use case shows up; resist the generalization
  until then.
- **Cross-file synthetic use.** A plugin in file A cannot emit a
  synthetic use into file B. Plugins are per-file; cross-file effects
  ride the existing `FileAnalysis` channel.

## Reading list

- `docs/adr/plugin-system.md` — the principles synthetic-use extends.
- `src/builder.rs:2742` — `visit_use`, target of the refactor.
- `src/plugin/mod.rs:78` — `UseContext`, what plugins receive.
- `src/plugin/mod.rs:438` — `on_use` hook, re-entered by SyntheticUse.
- Memory: `feedback_no_special_cases.md` — why the worker has no
  `synthetic: bool` flag.
