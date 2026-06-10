# Changelog

## 0.4.0 (unreleased)

### Field & attribute projection groups

A field/attr decl and all its spellings are now ONE renameable,
referenceable entity — across files, via LSP and the CLI alike:

- **Corinna**: `field $x :param :reader` ties the field variable, the
  constructor key (`Point->new(x => …)`), and reader calls. `:param`
  fields synthesize constructor keys (goto-def from a ctor key lands on
  the field decl, cross-file included).
- **Moo/Moose/Mojo::Base**: `has size => (…)` ties the decl token,
  accessor calls, constructor keys, and — new — internal `$self->{size}`
  slot pokes (strict class-owner matching widened only by ancestry, so a
  subclass reaching into the parent's hashref renames too, while other
  subs' same-named arg keys stay out).
- **Name-mapped accessors** (`predicate => 1` → `has_size`, `clearer`,
  `builder`, explicit `writer`/`reader`) re-derive their names on rename
  (`has_size` → `has_extent`), including user-written `sub _build_x`
  definitions and call sites in consumer files. Plugins enroll a method
  with a single `attr:` field on their existing emission.
- Constructor keys in files that only `use` the class are now indexed
  (the build-time gate defers to query time when the class isn't local),
  so references/rename/goto-def see them.

### Fully-qualified & SUPER dispatch

- `$obj->Foo::Bar::m`, `SUPER::m`, and `->::m` resolve through one
  parsed representation across goto-def, references, rename, hover, and
  signature help; SUPER resolves over the full parent MRO, lazily and
  cross-file.
- Receiver-polymorphic constructors (`bless {}, ref($class) || $class`)
  type as the call-site receiver.
- Constant-folded invocants dispatch: `my $c = 'Counter'; $c->bump`
  resolves to `Counter::bump`.

### Internals & correctness

- One cursor→target entry point (`resolve_symbol`) shared by every LSP
  handler and CLI mode — references and rename can no longer disagree on
  what the cursor means.
- Typed CST layer (`cst.rs`) and Perl-convention module
  (`conventions.rs`): grammar traps and name semantics (qualified
  method tokens, invocant shapes, canonical variable spellings —
  `${ sner }` ≡ `$sner`) are encoded once.
- Lazy tree resolution is gone: every query answers from build-time
  facts; `file_analysis` no longer walks syntax trees.
- The dependency index is consumed through a capability trait
  (`CrossFileLookup`); module cache extraction version 67 → 73 (caches
  re-extract lazily on first use).

## 0.3.0

See the v0.3.0 release.
