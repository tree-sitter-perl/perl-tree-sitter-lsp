# CLI Tools: Residual Forward Work

> Many subcommands have landed (`--check`, `--outline`, `--hover`, `--type-at`,
> `--definition`, `--references`, `--rename`, `--workspace-symbol`,
> `--plugin-{check,run,test}`, `--dump-package`). This doc covers what's
> still to build: a real diagnostic framework, more analysis subcommands,
> and `--migrate`.

## Diagnostic framework (the CI-readiness gate)

`--check` exists but emits one kind of diagnostic ("unresolved
function/method"), hardcoded severity, no suppression, no config. For CI
adoption we need:

### Diagnostic codes + severity config

| Code | Name | Default | Notes |
|---|---|---|---|
| `PL001` | unresolved-function | warning | already emitted; just needs code |
| `PL002` | unresolved-method | warning | already emitted; just needs code |
| `PL003` | unused-import | info | new |
| `PL004` | unused-variable | info | new |
| `PL005` | unused-export | info | new — also as `--unused-exports` |
| `PL006` | dead-sub | info | new — also as `--dead-code` |
| `PL007` | shadow-variable | hint | new |
| `PL008` | missing-import | warning | new |
| `PL009` | circular-dependency | warning | new |
| `PL010` | deprecated-pattern | hint | new — `use base` → `use parent` etc. |

### `.perl-lsp.json` config (workspace root)

```json
{
  "diagnostics": {
    "unresolved-function": "error",
    "unused-import": "info",
    "dead-sub": "off"
  },
  "exclude": ["t/**", "script/**"]
}
```

Read on startup + `workspace/didChangeConfiguration`.

### Comment suppressions

```perl
# perl-lsp: ignore(PL001)              — suppress on this line
# perl-lsp: ignore-next-line(PL001)    — suppress next line
# perl-lsp: ignore-file(PL004)         — first 10 lines, whole file
```

Implementation: scan `comment` nodes during build; store on
`FileAnalysis.suppressions: Vec<Suppression>`. `collect_diagnostics` checks
suppressions before emitting.

### SARIF output (`--check --format sarif`)

Static Analysis Results Interchange Format. GitHub Actions, GitLab CI, Azure
DevOps consume natively → diagnostics as PR annotations. This is the
killer CI feature.

Files to create: `src/diagnostics.rs`, `src/config.rs`. Refactor
`collect_diagnostics` in `symbols.rs` to use the framework.

## Analysis subcommands still missing

- `--completions <root> <file> <line> <col>` — completion at position; same
  engine as LSP, different entry point. Useful for REPLs and custom editor
  plugins.
- `--repl-complete` — read accumulated REPL session from stdin, complete at
  end-of-input. Pipes from Reply/Devel::REPL on tab.
- `--unused-exports <root>` — names in `@EXPORT_OK` no workspace file
  imports. Could also be diagnostic PL005.
- `--dead-code <root>` — Sub/Method symbols with zero refs. Skip lifecycle
  methods (`new`, `AUTOLOAD`, `BUILD`…), classes with `AUTOLOAD`. Could
  also be PL006.
- `--dependency-graph <root> [--format dot|json|list]` — module imports as
  graph; cycles via DFS.
- `--export-api <root> <module> [--format json|markdown]` — machine-readable
  API summary (exports, params, return types, parents, framework).
- `--impact <root> <file-or-module>` — reverse dependency analysis (who
  imports / inherits from this).
- `--framework-report <root>` — count + list classes by framework
  (Moo/Moose/Mojo::Base/DBIC/plain bless).

All are thin wrappers around existing `FileAnalysis` queries. Implementation
order is roughly the order above (diagnostics + completions first, framework
audit last).

## `--migrate` — framework translation

The marquee feature. `FileAnalysis` is already framework-agnostic at the
semantic level — it knows "this class has a read-only `name` accessor of
type Str." Translation between framework DSLs is mechanical from there.

```
perl-lsp --migrate <root> <file-or-module> --from <framework> --to <framework> [--dry-run]
```

### Targets

**Moo/Moose → Perl core `class`** (the headline migration):

| Moo/Moose | Core class |
|---|---|
| `package Foo; use Moo;` | `class Foo {` |
| `extends 'Parent'` | `:isa(Parent)` |
| `with 'Role'` | `:does(Role)` |
| `has name => (is => 'ro')` | `field $name :param :reader;` |
| `has name => (is => 'rw')` | `field $name :param :reader :writer;` |
| `has name => (is => 'ro', default => sub { [] })` | `field $name :reader = [];` |
| `has name => (is => 'lazy', builder => '_build_name')` | `field $name :reader; ADJUST { $name = $self->_build_name }` |
| `sub foo { my ($self) = @_; ... }` | `method foo () { ... }` |
| `around foo => sub { my ($orig, $self) = @_; ... }` | `method foo () { my $orig = $self->SUPER::can('foo'); ... }` |

Untranslatable: `BUILD`/`BUILDARGS` (use `ADJUST`), `DEMOLISH` (no core
destructor), complex type constraints (`isa => 'ArrayRef[HashRef[Str]]'`),
coercions, triggers, delegation. For these: emit `# TODO: manual migration
needed — <reason>` comment.

**Other supported migrations:** Moose → Moo, Moo → Moose, `use base` → `use
parent`, bless-based OOP → Moo (heuristic; detects `bless { … }` constructor
hashes + accessor patterns).

### Implementation shape

Span-based editing (like `--rename`), not whole-file regeneration —
preserves comments, formatting, non-framework code. Only the framework
boilerplate changes.

```rust
fn cli_migrate(root: &str, target: &str, from: &str, to: &str, dry_run: bool) {
    // 1. Index workspace
    // 2. For each file matching --from:
    //    a. Build FileAnalysis (semantic model is framework-agnostic)
    //    b. Apply --to writer to produce edits
    //    c. dry_run → emit diff; else write
}
```

The semantic model (`Symbol`s with framework metadata, `package_parents`,
typed accessors) is already there. The work is the per-target writer + the
edit-collection harness.

### Phase order

Easy → ambitious:

1. `use base` → `use parent` (single-line regex-shaped, but go through the
   semantic model for correctness)
2. Moose → Moo (Moo is a subset; mostly removals)
3. Moo → Moose (add `__PACKAGE__->meta->make_immutable`, type strings)
4. Moo/Moose → core `class` (the big one)
5. bless → Moo (heuristic; the legacy migration)
