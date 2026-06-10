# Changelog

All notable changes to perl-lsp are documented here. Format loosely follows
[Keep a Changelog](https://keepachangelog.com/); versions are the published
crate / VS Code extension versions.

## Unreleased

### Structural hash shapes & nested drills

- **Hash literals carry their keys in the type.** `{ host => 'x', port => 5432 }`
  is a shape, not a bare HashRef: `->{key}` narrows to the per-key type through
  assignment hops, double-drills (`$config->{db}{host}` → String), sub-return
  literals, and imports (cross-file drill hops included). Array literals type as
  per-index tuples — the mixed drill `$obj->{users}[0]{name}` chains end-to-end,
  and hover shows `Sequence<…>`.
- **`$row->{name}` on a typed row** (DBIC and friends) resolves to the column
  def — goto-def, references, hover.
- **Both spellings.** `my %h = (k => v)` types through the same shape builder
  as the hashref literal; `$h{k}` reads check against `%h`'s shape. Spreads —
  hash and array (`my %opts = (default => 1, @_)`) — mark the shape open.
- **Mutation is modeled, not guessed.** An unconditional `$v->{k} = …` write
  extends the shape (the read drills back typed); a conditional, dynamic-key,
  closure, or slice write (every slice spelling: `@h{…}`, `%h{k}`, `@$h{…}`,
  `$h->@{…}`, `$h->%{…}`) switches it open.
- **New diagnostic: `unknown-hash-key`** (hint severity). A read of a key a
  CLOSED literal shape doesn't define — the typo catcher — naming the known
  keys. Covers variables in both spellings AND expression bases
  (`cfg()->{kye}`, `$obj->get_config->{kye}`); calibrated to **zero false
  positives across the 2,293-module substrate**. Design:
  `docs/adr/structural-shapes.md`.
- **Escapes widen temporally instead of suppressing.** Passing `$config` to a
  call opens its shape AT that point — a typo read *before* the escape still
  hints, instead of the whole variable going dark. Sequence tuples learn from
  direct index writes too (slot retype, append at len).
- **Batch/CI diagnostics match the editor.** `--check` and `--batch`
  diagnostics now run the same cross-file enrichment as open buffers
  (memoized per process), so imported shapes hint there too.

### Gold corpus

- **Cost report.** Every suite run ends with per-capability timing
  (n/total/mean/max wall ms), per-root startup latency, child CPU, and peak
  RSS — so feature work sees what it costs as it lands. `METRICS_OUT=<file>`
  writes JSON for run-over-run comparison; `--emit --root <dir>` authors rows
  against per-row-root fixtures.
- New committed `nested-fixture/` workspace pinning the shape work (typo hints
  in both spellings, mutation extension, drill type-at/hover/def/refs).
- Harness fix: `normalize()` decoded character strings as bytes, so any
  non-ASCII response character silently dropped a row to the text fallback
  where `none` assertions vacuously pass.

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
- **Field & attribute groups.** A `has` attr or Corinna field and ALL its
  spellings — decl, accessor calls, constructor keys, `$self->{slot}` pokes,
  even derived names like `has_size` — rename and find-references as one
  entity, across files.
- **Bundled plugins + a plugin system.** Ships plugins for Catalyst, Dancer2,
  Mojolicious (helpers, events, Minion tasks), and DBIx::Class ResultDDL; you can
  drop your own `.rhai` plugins in a repo-local `.perl-lsp/`, or generate one from
  an `Import::Base` kit with `perl-gen`.

### Navigation & references

- Cross-file go-to-definition / references / rename for functions, methods, and
  packages (workspace-indexed), inheritance- and role-aware.
- Fully-qualified calls (`Foo::Bar::baz()`), `goto &Foo::bar` / `&Foo::bar()`,
  and imported functions resolve to their defining sub.
- Fully-qualified and SUPER method dispatch: `$obj->Foo::Bar::m`, `SUPER::m`
  (resolved over the full parent MRO, lazily and cross-file), and the `->::m`
  `main::` shorthand work across goto-def, references, rename, hover, and
  signature help.
- **Re-export surfaces:** goto-def through a module that re-exports another's
  `@EXPORT` (static-splice and loop-push forms) lands on the original sub.
- Dynamic dispatch & events resolve cross-file: Mojo helpers / plugin app
  surface, event emitters (`->on` / `->emit`), and Minion tasks.
- Refs reach into more places: chained hash-ref keys at any depth, `use constant`
  usages, `s///e` replacement code, forward-reference calls (call above its sub),
  and sigil-deref spellings.

### Field & attribute projection groups

- **Moo/Moose/Mojo::Base** `has` attrs tie the decl token, accessor calls,
  constructor keys (`Widget->new(size => …)`), and internal `$self->{size}`
  slot pokes into one rename/references entity — subclasses reaching into the
  parent's hashref included (ancestry-aware, while unrelated same-named arg
  keys stay out).
- **Corinna** (`use v5.38; class`): `field $x :param :reader` ties the field
  variable, constructor key, and reader calls; `:param` fields synthesize
  constructor keys so goto-def from `Point->new(x => …)` lands on the field.
- **Derived accessor names re-derive on rename**: `predicate => 1`'s
  `has_size` becomes `has_extent` when the attr renames — at call sites, in
  consumer files, and on user-written `sub _build_size` definitions. Plugins
  enroll synthesized methods with a single `attr:` field.
- Constructor keys in files that only `use` the class are indexed (the
  build-time gate defers to query time), so cross-file references, rename,
  and goto-def see them.

### Type inference

- Constructors via `bless` (including the `bless { %{ $_[0] } }, ref $_[0]` clone
  idiom) type to the right class; fluent setters (`return $self`) chain.
- Receiver-polymorphic constructors (`bless {}, ref($class) || $class`,
  including through `SUPER::new`) type as the call-site receiver, cross-file.
- Constant-folded invocants dispatch: `my $c = 'Counter'; $c->bump` resolves
  to `Counter::bump` — the same fold dynamic method names get.
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

- One cursor→target resolution entry point shared by every LSP handler and CLI
  mode — references and rename can no longer disagree on what the cursor means.
- A typed CST layer and a Perl-conventions module encode each grammar trap and
  name rule once (qualified method tokens, invocant shapes, canonical variable
  spellings — `${ sner }` ≡ `$sner`).
- Lazy tree resolution is gone: every query answers from build-time facts.
- Cold-start and hot-path performance: memoized witness-bag edge chases, indexed
  fold loops, and a memoized per-pass export surface.
- A CI-gated **gold corpus** — exact-assertion LSP checks (165 rows) against a
  version-pinned CPAN substrate — guards every release.

### Known gaps

A handful of inference edge cases degrade gracefully (a plain symbol instead of
a typed one), tracked in
[`gold-corpus/KNOWN-GAPS.md`](gold-corpus/KNOWN-GAPS.md) — notably
first-param-self over-reach for helpers/callbacks inside OO classes.
