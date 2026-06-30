# Method-resolution residuals (forward work)

The `unresolved-method` diagnostic — the always-on local form and its opt-in
cross-file extension (`unresolvedMethodCrossFile`) — doubles as a
**knowledge-gap finder**: every false positive names a method we failed to
synthesize or a receiver we failed to type. Auditing its output across a large
real codebase, the always-on false positives are closed (AUTOLOAD-in-MRO skip;
the narrowing-rebind truncation) and the `has \@const_array` synthesis gap is
closed. The remaining false positives cluster into four gaps below — most also
improve completion / hover / goto-def, not just the lint. Until they're closed,
the cross-file form stays opt-in.

## 1. `monkey_patch`-generated methods → bundled plugin

Classes that install methods at load time via a `monkey_patch $class => $name
=> sub {…}` loop (`Mojo::Util::monkey_patch`, `Sub::Install`,
`Class::Method::Modifiers`) expose no static `sub` — e.g. `Mojo::UserAgent`'s
HTTP-verb methods (`get`/`post`/`put`/`delete`/…) are generated this way. But
the registration call site **is** visible in source. A bundled plugin (or core)
should recognize `monkey_patch` / `install`-shaped registrations and synthesize
the named methods, the same way Moo `has` synthesizes accessors. Knowable in
the engine.

## 2. `Sub::HandlesVia` `handles => { … }` delegations → bundled Moo plugin

`has $attr => (handles => { method => [target, @args], … })` (Sub::HandlesVia,
and the Moo/Moose `handles` family) delegates named methods onto the consuming
class. These aren't synthesized today — the same shape as Moo accessor
synthesis but for delegated names.

This is **bundled-plugin** territory, NOT core. The accessor-option vocabulary
(`predicate`/`clearer`/`writer`/…/`handles`) already lives in the bundled Moo
plugin (`frameworks/moo.rhai`): core walks the `has` CST into decision-ready
options (rule #1) and the plugin owns which keyword maps to which synthesized
method (rule #10). Adding `handles => { … }` delegation is the plugin emitting
the delegated method names — a per-keyword delegation table baked into core
would be exactly the rule-#10 violation to avoid.

## 3. Opaque generated / XS classes → scoping + probe-gen

Fully generated API clients (OpenAPI/Swagger codegen), XS classes, and
Exporter-injected method sets have no statically visible method declarations at
all — there is nothing in source to read. Two complementary mitigations:

- **WORKSPACE-only scoping for the cross-file lint.** Flag a receiver only when
  its class is one you wrote (WORKSPACE role); stay silent on pure
  `@INC`/DEPENDENCY classes, where generated/XS methods you can't see are
  common. The `RoleMask` already distinguishes these — a principled gate, not
  whack-a-mole.
- **Probe-based plugin generation.** Run the generator against a recording
  probe to capture the produced method surface and emit a plugin (see the
  generator-coderef probe direction).

## 4. Untyped-receiver value-flow → long-distance provenance

Methods called on a receiver whose type the walker can't pin: an exported
resolver sub (`Exporter::Shiny` and friends) whose first parameter is the live
framework object passed in by the caller; a record/row handed in as a
parameter; etc. The method exists on the runtime receiver, but the static type
is unknown. This rides the long-distance value-provenance tier
(`docs/prompt-type-inference-residual.md`) — typing the parameter from its call
sites — plus the receiver-gated dispatch seam for role bodies that call methods
the composing class (or a sibling role) provides.

---

These are also the gating list for promoting the cross-file `unresolved-method`
lint past opt-in: each closed gap removes a false-positive cluster and the lint
gets correspondingly safer to default on.
