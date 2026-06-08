# Changelog

All notable changes to perl-lsp are documented here. Format loosely follows
[Keep a Changelog](https://keepachangelog.com/); versions are the published
crate / VS Code extension versions.

## v0.4.0

A large release: cross-file intelligence everywhere, framework and exporter
awareness, an extensible plugin system, and a reproducible regression net.
Robustness-swept clean across 2,293 real CPAN modules (no crashes, panics, or
hangs).

### Highlights

- **Cross-file everything.** Go-to-definition, references, rename, hover,
  completion, and signature help follow imports, inheritance, re-exported
  surfaces, and dynamic dispatch across files — not just the open buffer.
- **Type inference.** A witness-bag engine infers variable types, method-call
  return types, and chained-expression types — through constructors, accessors,
  fluent setters, and cross-file calls.
- **Framework intelligence.** Moo, Moose, Mojolicious (Mojo::Base), DBIx::Class,
  Class::Tiny, and MooX::Options: `has`/`extends`/`with`/`use parent`
  inheritance, generated accessors with typed returns, and constructors.
- **Bundled plugins + a plugin system.** Ships plugins for Catalyst, Dancer2,
  Mojolicious (helpers, events, Minion tasks), and DBIx::Class ResultDDL; you can
  drop your own `.rhai` plugins in a repo-local `.perl-lsp/`, or generate one from
  an `Import::Base` kit with `perl-gen`.

### Navigation & references

- Cross-file go-to-definition / references / rename for functions, methods, and
  packages (workspace-indexed), inheritance- and role-aware.
- Fully-qualified calls (`Foo::Bar::baz()`), `goto &Foo::bar` / `&Foo::bar()`,
  and imported functions resolve to their defining sub.
- **Re-export surfaces:** goto-def through a module that re-exports another's
  `@EXPORT` (static-splice and loop-push forms) lands on the original sub.
- Dynamic dispatch & events resolve cross-file: Mojo helpers / plugin app
  surface, event emitters (`->on` / `->emit`), and Minion tasks.
- Refs reach into more places: chained hash-ref keys at any depth, `use constant`
  usages, `s///e` replacement code, forward-reference calls (call above its sub),
  and sigil-deref spellings.

### Type inference

- Constructors via `bless` (including the `bless { %{ $_[0] } }, ref $_[0]` clone
  idiom) type to the right class; fluent setters (`return $self`) chain.
- Type::Tiny / Types::Standard / Types::Common `isa` constraints map to types
  (`InstanceOf['X']`, `Maybe[...]` unwrapping).
- `$self` / `$c` controller typing for Mojolicious and Catalyst actions.
- Parameters declared as `my ($self, $name) = (shift, shift)` are recognized for 
  signature help

### Exporters & imports

- Models `Exporter`, `Exporter::Tiny`, `Sub::Exporter`, `Exporter::Extensible`,
  and `Importer` / `Exporter::Declare`, plus `%EXPORT_TAGS`, `@EXPORT_OK`, and
  runtime-exporter setups — so imported names resolve and unimported-but-exported
  names get an actionable hint.
- One import-binding evaluator backs diagnostics and navigation, so "what does
  this `use` bring into scope" is answered the same way everywhere.

### Completion, outline & tokens

- `$self->` completion offers inherited methods (cross-file via `use parent` /
  `use Mojo::Base -base`), plus hash keys, imported subs, module names on `use`
  lines, and `qw()` import lists. Auto-import code action for unresolved calls.
- Outline collapses each Moo/Mojo `has` accessor to a single entry, hides
  sub-body lexicals, drops `use` statements, and de-dupes forward declarations.
- Semantic tokens: `use constant` names emit as enum members; `qr//` literals as
  regexp.

### Codegen & dynamic Perl

- Recognizes typeglob-assigned subs (`*name = \&x`, `*name = $cond ? \&a : sub
  {…}`), `AutoLoader`/`SelfLoader` `__DATA__` subs, and accessor codegen
  (`mk_classdata`, `mk_group_accessors`, Class::Tiny).

### Parser

- Adopted **ts-parser-perl 1.1.0 → 1.1.1**: many more valid Perl constructs parse
  cleanly — braced variable declarations (`my ${foo}`), sub/method forward
  declarations (`sub NAME;`), bare dotted versions (`use 5.14.0`), glued
  `x`-repetition, the `not` operator, `\&subname` refgen, arrow-chained subscripts
  in interpolated strings, and correct hashref-vs-block disambiguation
  (`bless {@_}, $class`). Fixes the long-standing `s{}{}` external-scanner abort
  on large modules.

### Diagnostics

- Framework-aware false-positive suppression (incomplete ISA chains,
  `@EXPORT_OK` on a bare `use`, runtime exporters, XS/typeglob installs).
- Quieter by design: unresolved-function/method are emitted at **hint**
  severity, since dynamic Perl (AUTOLOAD, runtime glob installs, uninstalled
  deps) is common and shouldn't flood the Problems panel. This is frequently
  solvable by using a custom plugin.

### Editors & CLI

- **VS Code extension** auto-downloads the matching binary (config path → cached
  → `PATH` → GitHub Release), so install-and-go works with no separate setup.
- New CLI query modes for scripting/CI: `--batch` (one startup, many queries on
  stdin), plus `--completion`, `--signature-help`, `--semantic-tokens`,
  `--document-highlight`, `--linked-editing`, and `--timings`.

### Under the hood

- Cold-start and hot-path performance: memoized witness-bag edge chases, indexed
  fold loops, and a memoized per-pass export surface.
- A CI-gated **gold corpus** — exact-assertion LSP checks (164 rows) against a
  version-pinned CPAN substrate — guards every release.

### Known gaps

A handful of inference edge cases degrade gracefully (a plain symbol instead of
a typed one), tracked in
[`gold-corpus/KNOWN-GAPS.md`](gold-corpus/KNOWN-GAPS.md) — notably cross-file
receiver-polymorphic constructors (`bless {}, ref $self || $self` through
`SUPER::new`) and first-param-self over-reach for helpers/callbacks inside OO
classes.
