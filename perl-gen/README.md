# perl-gen — Import::Base → .rhai plugin generator (POC)

A Perl module that translates an `Import::Base` subclass into a
`.rhai` plugin emitting `EmitAction::SyntheticUse` actions per
bundle flag. The generated plugin drops into
`$PERL_LSP_PLUGIN_DIR` and the LSP picks it up on next startup.

See `docs/prompt-importbase-plugin-gen.md` (repo root) for the
design rationale.

## Usage

```sh
perl-gen/bin/perl-lsp-gen-importbase Co::Base::Local \
  --lib /path/to/project/lib \
  --lib /path/to/project/kit-common/lib \
  --alias Co::Base \
  --out plugins/co-base-local.rhai
```

Repeatable `--lib` paths are prepended to `@INC` before `require`.
`--alias <class>` adds extra module names that should trigger the
plugin (kits often have an install-wide shim that forwards to the
local-dev class — `Co::Base` → `Co::Base::Local`).

By default the output goes to stdout; pass `--out <file>` to write
to disk.

## Requires

- Perl ≥ 5.42 (the generator uses `use v5.42`).
- `Import::Base` and any modules the target kit transitively
  requires must be installed (the generator calls `require <Kit>`,
  which loads the kit's parent class chain). `cpm install -g
  Import::Base` for the bare minimum; whatever else the kit needs
  for its own load (Mojolicious, DBIC, etc.) too.

## Tests

```sh
prove perl-gen/t/01_basic.t
```

The test fixture (`perl-gen/t/fixtures/lib/My/TestKit.pm`) is a
minimal Import::Base subclass covering every sigil + a coderef.
18 assertions; runs in ~50ms.

## What it handles

| Source                          | Output                                                                |
|---------------------------------|-----------------------------------------------------------------------|
| `MOD => [args]`                 | `SyntheticUse { module: MOD, args, imports }`                         |
| `MOD` (bareword, no fat-comma)  | `SyntheticUse { module: MOD, args: [], imports: [] }`                 |
| `>MOD => [args]`                | Same as plain — `Import::Into` runtime equivalent of `use MOD args`   |
| `<MOD => [args]`                | Same shape, emitted **first** within its bundle (base-class ordering) |
| `-MOD => [args]`                | `// TODO: unimport ...` (no SyntheticUnuse primitive yet)             |
| `>-MOD => [args]`               | Same TODO                                                             |
| coderef                         | `// TODO: coderef at <file>:<line>` — user hand-authors               |

The generator `require`s the kit (so `our`-vars populate) but
**never invokes its `import` method** — no side effects, no probe
package, no `Import::Into` monkey-patching. Coderef bodies don't
execute. Multi-arg expressions like `map "-$_", qw/.../` evaluate
at module-compile time (inside the literal array initializer) and
survive into the generator as plain string lists.

## What it doesn't handle

- **Import::Into kits** (hand-rolled `sub import { Pkg->import::into(...) }`).
  Imperative bodies have no data tables to read; needs a runtime
  recorder. Deferred.
- **Coderef interpretation** — the LSP's `PackageParent` action can
  represent `extends('X')` / `with('X')` / `load_components('A', 'B')`
  side effects, but recognizing those idioms from inside a coderef
  needs either `B::Deparse` pattern-matching or a probe-package
  interception layer. Deferred. For now the TODO points at the kit
  source line so a human can hand-author the equivalent.
- **Unimport** — `no MOD args` has no SyntheticUse counterpart. Add a
  `SyntheticUnuse` EmitAction in the LSP if these turn out to matter.
- **`--check` mode** — CI gate for "committed .rhai differs from a
  fresh regen". Add when the regen workflow is established.

See `docs/prompt-importbase-plugin-gen.md` for the full out-of-scope
list and the reasoning.
