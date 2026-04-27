# Lexical vs. package scopes — split them

Scratchpad for the architectural cleanup the rhai-plugins branch
deferred. Lives here so compaction can't eat it; sister doc to
`prompt-plugin-architecture.md`.

## The smell

`ScopeKind::Package` is doing two unrelated jobs that share no
semantics in Perl:

1. **Lexical scoping** for variable lookup.
2. **Package-context grouping** — what `package_at(point)` returns,
   what scopes subs/methods to a class, how the document outline
   groups namespace contents.

These look similar (both involve "the region after `package Foo;`")
but they aren't the same thing in Perl, and the conflation forces
two band-aids that landed on the rhai-plugins branch:

- `Builder::add_symbol_ns` lifts `Variable`/`Field` symbols past any
  enclosing `ScopeKind::Package` via `nearest_lexical_scope` so `my`
  decls don't get trapped in a sibling Package scope and become
  invisible to later `package X;` sections of the same file. The
  e2e regression that caught this: `rename: $pi → $tau` failed on
  the interpolated-string occurrence under `package main;` (only
  use site outside Calculator's section).
- `FileAnalysis::document_symbols` collects file-scope children and
  sibling-Package-scope children separately, then sorts by source
  position to recover code order — the appended-bucket ordering
  visibly reshuffles the outline.

Both are tolerance shims for the same root cause: variable lookup
walks the *lexical* scope tree, while subs/methods/`package_at` use
*package-context* metadata that happens to be modeled in the same
tree. They want to be the same tree only by accident.

## Perl semantics, precisely

- **`my $x`**: lexical. Scope = enclosing real block (file scope if
  at top level; the nearest `{ … }` block, sub, or for-loop body
  otherwise). `package Foo;` does **not** end its scope. Visible to
  the rest of the lexical region regardless of how many `package X;`
  statements appear in between.

- **`our $x`**: package-global with a lexical alias. The actual
  variable is `$Package::x` and lives in the package's symbol
  table. The `our` line introduces a lexical alias — within its
  lexical scope, bare `$x` resolves to `$Package::x`. From a
  sibling `package main;` section, bare `$x` does **not** reach
  the original — you'd have to spell `$Calculator::x` (or
  re-declare `our $x` inside `package main;`, which would alias
  `$main::x`, a different variable). This means `our` is correctly
  modeled as living in *package* scope, not lexical scope.

- **`state $x`**: same lexical scoping as `my` (the `state`
  difference is initialization timing, not scope).

- **`field $x` (Perl 5.38+ `class`)**: scope = the class block.
  Always inside `class Foo { … }`, which is a real block scope
  (`ScopeKind::Class`).

- **Subs / methods**: their *name* is package-scoped (a sub
  declared after `package Foo;` is reachable as `Foo::run`); their
  body is lexically scoped like any block.

- **`package Foo;` (statement form)**: changes the package context
  for everything that follows until the next `package X;` or end
  of file/block. Does **not** create a lexical scope.

- **`package Foo { … }` (block form)**: changes the package
  context for the contents of the block AND creates a real lexical
  block scope.

The two jobs `ScopeKind::Package` tries to cover map to two
*different* answers per construct above. That's the whole problem.

## Target shape

Two orthogonal data structures:

### Lexical scope tree

`Vec<Scope>` containing only real lexical boundaries:

```rust
pub enum ScopeKind {
    File,
    Block,                      // bare { … }, if/while/for bodies, do { … }
    Sub { name: String },       // sub foo { … } body
    Method { name: String },    // method foo { … } body
    Class { name: String },     // class Foo { … } block
    ForLoop { var: String },    // for my $x (…) { … }
}
```

`ScopeKind::Package` does not exist. `class Foo;` (no block) does
**not** push a scope; `class Foo { … }` pushes `Class`.
`package Foo;` does not push a scope either.

Variable lookup walks `scope.parent` chains in this tree. No
"skip the Package bucket" hack.

### Package-context map

A flat per-file `Vec<PackageRange>` populated by the builder when
it sees `package X;`, `package X { … }`, `class X;`, or `class X { … }`:

```rust
pub struct PackageRange {
    pub package: String,
    pub span: Span,           // byte range governed by this declaration
    pub kind: PackageKind,    // Statement or Block
}
```

`package_at(point)` becomes a flat range query against this list
(latest-starting-range-containing-point wins for nested cases).

Sub/method `sym.package` is set by querying the map at the decl's
position. `our $x` records its package at the same time so lookup
can route bare `$x` to `$Package::x`.

### Outline composition

`document_symbols` queries the package map for each top-level
range and produces grouped namespaces. No more "file scope first,
sibling Package scopes appended, then sort by position" — there's
only one source of ordering: source position, with namespace
grouping driven by the package map.

## What to delete / replace

**Delete:**
- `ScopeKind::Package` variant.
- `Builder::open_sibling_scope` / `close_sibling_scope_if_open`.
- `Builder::nearest_lexical_scope` (no longer needed once
  variables attach to real lexical scopes directly).
- The "is_lexical → lift" branch in `Builder::add_symbol_ns`.
- The post-collection sort in `FileAnalysis::document_symbols`
  (source position falls out of normal traversal).

**Replace:**
- `Scope::package: Option<String>` — keep on `Scope` for now or
  read on demand from the package map. Probably keep, populated
  via the map at scope creation time.
- `Builder::current_package` — the field stays (set/restore around
  `package X;` or block-form `{ … }`), but the *consumers*
  (`add_symbol_ns` for sub.package, `detect_anon_hash_owner`)
  should ideally read from the map for consistency at query time.
  At build time, the field is fine.

**Add:**
- `pub package_ranges: Vec<PackageRange>` on `FileAnalysis`,
  serialized.
- `FileAnalysis::package_at(point) -> Option<&str>` (replaces the
  current scope-walk `package_at`).

## Migration plan

This is meaty (~20 callsites of `scope.kind == Package` or scope
membership for outline grouping). Land in a dedicated PR after
rhai-plugins so the diff isn't smushed with the plugin work.

1. **Build the package map alongside the existing scope tree.**
   Don't remove `ScopeKind::Package` yet. The map is purely
   additive; `package_at` can switch to it as a no-op behavioral
   change. Tests should keep passing.

2. **Migrate consumers.** Each callsite reading `scope.kind ==
   Package` or `scope.package` is examined; package-context
   readers move to `package_at(point)`, lexical-scope readers
   either lose their Package check (variables — already done via
   the lift) or have their semantics tightened.

3. **Migrate outline.** `document_symbols` uses the package map
   for namespace grouping; the ad-hoc bucket-merge + sort goes
   away. Pin tests to verify outline shapes don't change in
   user-visible ways.

4. **Remove the Package variant, the sibling-scope helpers, and
   the lift hack.** At this point the Scope tree is purely
   lexical. `ScopeKind` shrinks to `File / Block / Sub / Method /
   Class / ForLoop`.

5. **Bump `EXTRACT_VERSION`.** Bincode shape change on the cache
   blob.

## Red-pin tests to keep green throughout

- `builder::tests::red_pin_my_resolves_across_statement_packages`
  — `my $pi` in Calculator is resolvable from `package main;`.
  Step 1 doesn't change behavior; steps 2–4 each verify this stays
  green.

- An equivalent for `our`: `our $version` in Calculator is **not**
  resolvable as bare `$version` from `package main;` (it would
  need `$Calculator::version`). Currently latent — not exercised
  by any test. Add it as part of the migration so we don't
  regress in the other direction.

- `file_analysis::tests::plugin_mojo_demo_outline_pinned` — outline
  source order. Sensitive to bucket-merge changes; rerun after
  step 3.

- `e2e: rename: $pi → $tau`, `rename: process → execute`,
  `rename: 'name' constructor arg`. The three rename regressions
  this spec exists to prevent the next time scope shape moves.

## Why now (and not earlier on the rhai branch)

Doing it on the rhai-plugins branch would have meant landing a
~20-callsite scope refactor inside an already-large PR (75
commits, 22k lines). The lift-and-sort shims close the rename
regressions cleanly enough to ship the plugins; this spec
captures the right shape for a follow-up where the cleanup is
the entire diff.
