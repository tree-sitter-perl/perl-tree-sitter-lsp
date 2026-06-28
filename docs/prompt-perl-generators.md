# Productive Perl Projection (PPP) — symbol generators

A **symbol generator** is a Perl helper whose call synthesizes a GROUP of
symbols from its literal arguments — `make_crud_helpers('user')` projecting a
`user_id` accessor plus `get_user`/`set_user` methods. This is the
generalization of Moo `has` synthesis (`visit_has_call`): a call witness
projected into a symbol group, every member tracing (provenance) to the call
site so the outline shows it and goto-def / rename on it land on the
invocation.

Design is **structural projection, never execution**: the plugin DECLARES the
synthesis rules abstractly (parameterized over the call args); core substitutes
the literal args and runs a fixpoint worklist. Lifted from the
`spike/cpp-support` PoC (`src/perl_generators.rs`).

## What landed (first slice)

The seam is a new plugin **manifest** `generators()` (rule #10 — the plugin
owns the rules, core owns the mechanism), parallel to `dispatch_verbs()` /
`role_makers()`.

- **`src/plugin/mod.rs`** — `GeneratorDef { name, params, actions }` +
  `GeneratorAction` (a flat struct: `emit`+`kind` to synthesize a symbol, or
  `generate`+`args` to invoke a nested generator). Trait method
  `FrameworkPlugin::generators()` (default empty) and
  `PluginRegistry::generators()` yielding `(plugin_id, &GeneratorDef)`.
- **`src/plugin/rhai_host.rs`** — parses `fn generators()` from a Rhai script
  (fail-safe, same contract as the other manifests); `generators_manifest_parses_from_rhai`
  test proves the round-trip.
- **`src/generators.rs`** — the provenance-generic projection worklist
  (`project(defs, root) -> Vec<GenSymbol>`): `${param}` interpolation, a
  per-call seen-set (recursive generator terminates), nested `Generate`
  chaining the root provenance. Pure unit tests.
- **`src/builder.rs`** — bakes `generator_manifest` + `generator_plugin` at
  init; `synthesize_generator_call(name, ctx)` runs at function/method-call
  dispatch (same seam as `record_provisional_dispatch`), emitting each member
  as a REAL `Namespace::Framework` symbol with `span = call site` (provenance)
  and `selection_span = generator-name token`.
- **`src/builder_tests.rs`** (`mod symbol_generators`) — integration test: the
  synthesized group is present in `FileAnalysis` with span pinned to the call
  site; nested generation synthesizes transitively with root provenance; a
  non-generator call synthesizes nothing.

Verified: `cargo test` (1009 pass). The generator fires regardless of triggers
(keyed off the global manifest), so it works the same in any package.

## What's next

1. **Verify references / goto-def / rename end-to-end.** The synthesized
   symbols are real `Method` symbols in the package, so `$self->get_user()`
   should resolve via `stamp_method_call_targets` →
   `resolve_method_in_ancestors` → the synthesized `sym_id`, and rename should
   land on the generator call (its `selection_span`). Add a CLI/e2e test that
   drives goto-def from a call site to the generator invocation, and a rename
   round-trip. (Not yet covered — only symbol presence + provenance are
   asserted.)

2. **Ship a real bundled generator.** No universal CPAN generator was targeted
   in this slice (the PoC's `make_crud_helpers` is illustrative). Candidates:
   `MooseX::*` attribute-group sugar, `Class::Method::Modifiers`-style
   installers, project-house `make_*` exporters. A bundled `frameworks/*.rhai`
   should gate with a `UsesModule` trigger so it only fires for its module —
   but note synthesis itself is trigger-independent, so the gate must be
   enforced by checking the generator's provenance module if false positives
   appear. (Today any call matching a declared generator name synthesizes.)

3. **Args beyond positional string literals.** `synthesize_generator_call`
   collects `ctx.args[*].string_value` in order. Extend to qw-lists / folded
   constants / hashref option pairs (reuse the `classified_pairs` /
   `string_list` helpers) so a generator can take `(name => ..., columns =>
   [...])`. The `=>` is a human hint only — keep the walk separator-agnostic
   (CLAUDE.md fat-comma rule).

4. **Richer emitted symbols.** Members are `SymbolDetail::Sub` with empty
   params and no return type. Let `GeneratorAction` carry params / return-type
   templates and a `HashKeyDef` action (constructor key), matching the full
   `has` synthesis (accessor + constructor key + internal hash key).

5. **Cross-file generators.** A generator imported from another module: the
   manifest is global so it already fires, but the synthesized symbols live in
   the calling file. Confirm they survive the module-cache blob and resolve
   cross-file (they are ordinary symbols, so they should).

## Chosen seam (why)

A `generators()` manifest, not an `on_function_call` that emits the group
directly: the manifest gives core the worklist for free (nested + recursive
generation in one pass), and keeps the synthesis rules as declarative plugin
data (rule #10) rather than imperative script logic. The projection engine is
isolated and provenance-generic, so the worklist/seen-set/interpolation is unit
tested without the builder.
