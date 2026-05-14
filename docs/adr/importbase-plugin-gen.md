# ADR: Generate kit plugins from Import::Base data tables

`Import::Base` subclasses are how real Perl shops centralize their
import dance — `use Co::Base -Class;` collapses a dozen real `use`
lines via `@IMPORT_MODULES` / `%IMPORT_BUNDLES`. PR #38's
`EmitAction::SyntheticUse` gave us the runtime primitive to express
the expansion. This ADR is about authorship: the plugins get
generated, not hand-written. The generator lives in `perl-gen/`; the
LSP side adds nothing.

## Decisions worth keeping

### `require`, don't `use` — read variables, don't execute import

`Import::Base` subclasses ARE their `our @IMPORT_MODULES` and
`%IMPORT_BUNDLES` declarations. `require <Kit>` populates them at
compile time without triggering the kit's `import` method. The
generator reads them directly. No probe namespace, no
`Import::Into` monkey-patching, no coderef bodies executing at
generator time.

The alternative — monkey-patch `Import::Into` and run `Kit->import`
in a probe — works and is the only path for hand-rolled
`sub import` kits. Skip it when reading the tables suffices.

### Generator is Perl, output is a committed `.rhai`

Lives in `perl-gen/` as a CPAN-shaped Perl module. Specifically not a
Rust subcommand of `perl-lsp`. Kit authors write Perl; their tools
should too. Output is a normal `.rhai` plugin file the user commits
to their `$PERL_LSP_PLUGIN_DIR` — same loader, same `--plugin-check`,
same fingerprint-invalidates-cache pipeline as any hand-written
plugin. The generator is just authorship automation.

Specifically not in-memory runtime auto-detection. That would
execute Perl during LSP indexing and hide what's actually loaded.
Committed `.rhai` is inspectable and diffable.

### Sigil interpretation mirrors what real source produces

Import::Base's prefix sigils (`>` Import::Into, `<` emit-first /
base-class, `-` / `>-` unimport) map 1:1 to `SyntheticUse` field
shape (`module`, `args`, `imports`). The synthetic and real paths
end up with identical data going into `process_use`, so downstream
framework detection / parent classes / plugin re-dispatch fire
identically. The `<` prefix's emit-first ordering is preserved in
the rendered `.rhai` so `<Mojo::Base 'X'` registers before
bundle-mates that depend on it.

Bundle dispatch matches Import::Base's `_parse_args`: the first
non-dash arg is the bundle name (`'Class'`); the dashed form
(`-Class`) is accepted as a synonym because some kit READMEs
document it that way.

### Coderefs are TODO comments

`%IMPORT_BUNDLES` values can contain coderefs (`load_components`,
`extends('X')`, ...). Without executing the kit, the generator
can't decode these. v1 emits `// TODO: coderef at <kit>.pm:<line>`
at the right position; the user reads the kit source and hand-
authors the equivalent `SyntheticUse` / `PackageParent` emissions.
B::Deparse + pattern-matching is deferred — adds machinery only
worth it if the same coderef shapes recur across many kits.

## What's intentionally not here

- **Import::Into / hand-rolled `sub import` kits.** Imperative
  bodies have no data tables to read; needs the probe-package
  recorder. Hand-author a per-kit plugin meanwhile.
- **Runtime auto-discovery.** No silent in-memory generation from
  the workspace; what the LSP loads must be on disk.
- **`SyntheticUnuse`.** No primitive yet for `no MOD` directives.
- **Merge mode for coderef hand-edits.** v1 regenerates whole
  files; keep manual additions in a separate companion plugin
  until sentinel-marker preservation earns its weight.
- **Coderef recognition / DDL DSL plugins.** Separate plugin
  concerns, not generator concerns. Belong in `frameworks/` if
  they earn their keep.
