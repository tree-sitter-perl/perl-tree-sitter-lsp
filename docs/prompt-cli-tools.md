# CLI Analysis Tools + Diagnostic Framework

## Motivation

The LSP protocol is one UI for the analysis engine. The CLI turns it into a unix tool — composable, scriptable, CI-integrable. Every query on `FileAnalysis` and every workspace index search becomes a potential command.

This spec covers:
1. CLI tool suite (batch analysis, refactoring, exploration)
2. Diagnostic framework (configurable checks, suppressions, severity levels)
3. Output formats (human-readable, JSON, SARIF for CI)

---

## Part 1: Diagnostic Framework

### Problem

We currently have one kind of diagnostic: "unresolved function/method." It's hardcoded in `collect_diagnostics` in `symbols.rs`, always Warning severity, no way to suppress, no way to configure. For CI adoption, we need:

- Multiple diagnostic types (not just unresolved — dead code, unused imports, style)
- Configurable severity (error/warning/info/hint/off)
- Suppression via comments (`# perl-lsp: ignore(unused-import)`) and config
- Output formats for CI (JSON, SARIF)

### Diagnostic types

| Code | Name | Default severity | Description |
|------|------|-----------------|-------------|
| `PL001` | `unresolved-function` | Warning | Function call doesn't resolve to any definition or import |
| `PL002` | `unresolved-method` | Warning | Method call doesn't resolve via type inference or inheritance |
| `PL003` | `unused-import` | Info | Imported symbol never referenced in the file |
| `PL004` | `unused-variable` | Info | Variable declared but never read |
| `PL005` | `unused-export` | Info | Symbol in `@EXPORT_OK` but never imported by any workspace file |
| `PL006` | `dead-sub` | Info | Sub defined but never called (locally or cross-file) |
| `PL007` | `shadow-variable` | Hint | Inner scope variable shadows outer scope with same name |
| `PL008` | `missing-import` | Warning | Function used but not imported (no `use` for the module providing it) |
| `PL009` | `circular-dependency` | Warning | Module A uses Module B which uses Module A |
| `PL010` | `deprecated-pattern` | Hint | Usage of patterns with known better alternatives (e.g., `use base` → `use parent`) |

### Configuration: `.perl-lsp.json`

```json
{
  "diagnostics": {
    "unresolved-function": "error",
    "unresolved-method": "warning",
    "unused-import": "info",
    "unused-variable": "off",
    "unused-export": "info",
    "dead-sub": "off",
    "shadow-variable": "off",
    "missing-import": "warning",
    "circular-dependency": "warning",
    "deprecated-pattern": "hint"
  },
  "exclude": [
    "t/**",
    "script/**"
  ]
}
```

Looked up from workspace root. Falls back to defaults if not present. The LSP reads this at startup and on `workspace/didChangeConfiguration`.

### Suppression via comments

```perl
# perl-lsp: ignore(unresolved-function)
some_dynamic_call();

# perl-lsp: ignore-next-line
$self->$dynamic_method();

# perl-lsp: ignore-file(unused-variable)
# (at top of file — suppresses for entire file)
```

**Implementation:** During the builder walk (or as a post-pass), scan comments for `perl-lsp:` directives. Store suppressions on `FileAnalysis`:

```rust
pub suppressions: Vec<Suppression>,

struct Suppression {
    kind: SuppressionKind,  // IgnoreLine(usize), IgnoreNextLine(usize), IgnoreFile
    codes: Vec<String>,     // diagnostic codes to suppress
}
```

`collect_diagnostics` checks suppressions before emitting.

### Diagnostic data model

```rust
pub struct PerlDiagnostic {
    pub code: &'static str,      // "PL001"
    pub name: &'static str,      // "unresolved-function"
    pub severity: Severity,       // from config, or default
    pub span: Span,
    pub message: String,
    pub file: PathBuf,
    /// Suggested fix (for code actions)
    pub fix: Option<DiagnosticFix>,
}

pub struct DiagnosticFix {
    pub label: String,
    pub edits: Vec<(Span, String)>,
}
```

### Integration with LSP

The existing `collect_diagnostics` in `symbols.rs` refactors to use this framework. Each diagnostic type has its own collection function. The LSP `publishDiagnostics` maps `PerlDiagnostic` to LSP `Diagnostic` with `code`, `source: "perl-lsp"`, and severity from config.

---

## Part 2: CLI Commands

### Architecture

All CLI commands share a common pattern:

```
1. Parse args
2. Index workspace (if needed) — reuse index_workspace()
3. Run analysis query
4. Format output (human / json / sarif)
5. Exit with code 0 (success) or 1 (found issues, for --check)
```

Common flags across all commands:
- `--format json|human|sarif` — output format (default: human for tty, json for pipe)
- `--exclude <glob>` — skip files matching pattern
- `--include <glob>` — only process files matching pattern

### `--check` — Batch diagnostics (THE CI command)

```
perl-lsp --check [<root>] [--severity <min>] [--format json|human|sarif]
```

Index the workspace, run all enabled diagnostics on every file, output results. Exit code 1 if any diagnostics at or above `--severity` threshold.

```bash
# CI usage:
$ perl-lsp --check . --severity warning
lib/MyApp/Controller/Users.pm:47:12: warning[PL001] unresolved function 'proccess_data'
lib/MyApp/App.pm:23:8: warning[PL002] unresolved method 'formatt_date' on MyApp::App
2 warnings, 0 errors in 47 files (indexed 89ms)

$ echo $?
1

# JSON for CI integration:
$ perl-lsp --check . --format json
[
  {
    "file": "lib/MyApp/Controller/Users.pm",
    "line": 47, "col": 12,
    "code": "PL001", "name": "unresolved-function",
    "severity": "warning",
    "message": "unresolved function 'proccess_data'"
  }
]

# SARIF for GitHub Actions:
$ perl-lsp --check . --format sarif > results.sarif
# Upload as GitHub Actions artifact → appears as code annotations
```

**SARIF output** is the killer CI feature. GitHub Actions, GitLab CI, Azure DevOps all consume SARIF natively. Diagnostics appear as inline annotations on PRs.

**Implementation:** Iterate workspace index, call `collect_diagnostics` on each `FileAnalysis` (with module index for cross-file resolution), collect into `Vec<PerlDiagnostic>`, format, exit.

For cross-file diagnostics (`unused-export`, `dead-sub`, `circular-dependency`): run after initial per-file collection, using the full workspace index for cross-referencing.

### `--outline` — Document symbol outline

```
perl-lsp --outline <file> [--format json|human]
```

```bash
$ perl-lsp --outline lib/MyApp/Model/User.pm
package MyApp::Model::User (L1)
  sub new (L5) → MyApp::Model::User
  sub process_data (L10, $input) → HashRef
  sub get_display_name (L16) → String
  has name (accessor, ro)
  has email (accessor, rw)

$ perl-lsp --outline lib/MyApp/Model/User.pm --format json
[
  {"name": "MyApp::Model::User", "kind": "Package", "line": 0, "children": [
    {"name": "new", "kind": "Sub", "line": 4, "return_type": "MyApp::Model::User"},
    {"name": "process_data", "kind": "Sub", "line": 9, "params": ["$input"], "return_type": "HashRef"}
  ]}
]
```

**Implementation:** Parse single file, build FileAnalysis, iterate symbols with hierarchy.

### `--references` — Semantic find-references

```
perl-lsp --references <root> <file> <line> <col> [--format json|human]
```

```bash
$ perl-lsp --references . lib/MyApp/Utils.pm 7 4
lib/MyApp/Utils.pm:7:4            def   sub format_date
lib/MyApp/Controller/Users.pm:16:20  call  format_date('2024-01-01')
lib/MyApp/App.pm:14:16            call  format_date('today')
t/basic.t:6:14                    call  format_date('2024-06-15')
lib/MyApp/App.pm:3:0              import  use MyApp::Utils qw(format_date)
5 references across 4 files
```

**Implementation:** Index workspace, find target at cursor, search all files with `rename_sub` (which already collects all defs + refs). Add import list refs.

### `--definition` — Cross-file goto-def

```
perl-lsp --definition <root> <file> <line> <col>
```

```bash
$ perl-lsp --definition . app.pl 12 8
lib/MyApp/Model/User.pm:10:4  sub process_data

# Pipe to editor:
$ vim $(perl-lsp --definition . app.pl 12 8 --format vim)
# outputs: +10 lib/MyApp/Model/User.pm
```

**Implementation:** Index workspace, resolve definition, output location. `--format vim` outputs `+line file` for `vim` consumption.

### `--hover` — Type info and docs

```
perl-lsp --hover <file> <line> <col>
```

```bash
$ perl-lsp --hover lib/MyApp/App.pm 12 4
$ctrl: ClassName(MyApp::Controller::Users)

$ perl-lsp --hover lib/MyApp/Utils.pm 7 4
sub format_date($date) → String

Format a date for display. Accepts ISO 8601 or relative dates.
```

**Implementation:** Parse file, call hover function, output markdown (or strip to plain text for terminal).

### `--completions` — Completion at position

```
perl-lsp --completions <root> <file> <line> <col> [--format json|human]
```

```bash
$ perl-lsp --completions . lib/MyApp/App.pm 15 10
$ctrl->  (method completion for MyApp::Controller::Users):
  list          Sub    → HashRef
  show          Sub    → HashRef
  process       Sub    (from BaseWorker)
  new           Sub    → MyApp::Controller::Users
```

**Implementation:** Index workspace (for cross-file completion), detect cursor context, run completion, output. Useful for REPL integration and custom editor plugins.

### `--unused-exports` — Dead public API

```
perl-lsp --unused-exports <root> [--format json|human]
```

```bash
$ perl-lsp --unused-exports .
MyApp::Utils: parse_config (in @EXPORT_OK, never imported)
MyApp::Legacy: process_v1, validate_v1 (2 unused exports)
3 unused exports across 2 modules
```

**Implementation:** For each module in workspace index, check its `export_ok` list. For each exported name, search all other files' imports for a `use Module qw(name)` that includes it. Report names with zero importers.

This could also be diagnostic `PL005` — shown inline in the LSP on the `our @EXPORT_OK` line.

### `--dead-code` — Unreferenced subs

```
perl-lsp --dead-code <root> [--format json|human]
```

```bash
$ perl-lsp --dead-code .
lib/MyApp/Utils.pm:23     sub _legacy_format     (0 references)
lib/MyApp/Model/User.pm:45  sub _migrate_v1      (0 references)
lib/MyApp/Controller/Admin.pm:8  sub _deprecated  (0 references, has deprecation notice)
3 unreferenced subs across 47 files

# Exclude private subs (underscore prefix):
$ perl-lsp --dead-code . --include-private=false
0 unreferenced subs
```

**Implementation:** Collect all Sub/Method symbols across workspace. For each, search all files' refs for FunctionCall/MethodCall targeting that name. Report symbols with zero cross-file refs.

Heuristics to reduce noise:
- Skip `new`, `AUTOLOAD`, `DESTROY`, `import`, `BUILD`, `BUILDARGS` — lifecycle methods
- Skip methods in classes with `AUTOLOAD` — can't statically determine what's called
- Option to skip private (`_prefixed`) subs — often internal and called dynamically
- Skip methods that are framework-required (`startup`, `register`, `run`)

Could also be diagnostic `PL006`.

### `--dependency-graph` — Module dependency visualization

```
perl-lsp --dependency-graph <root> [--format dot|json|list] [--depth <n>]
```

```bash
$ perl-lsp --dependency-graph . --format dot | dot -Tsvg > deps.svg

$ perl-lsp --dependency-graph . --format json
{
  "nodes": ["MyApp::App", "MyApp::Utils", "MyApp::Model::User", ...],
  "edges": [
    {"from": "MyApp::App", "to": "MyApp::Utils", "type": "use"},
    {"from": "MyApp::App", "to": "MyApp::Controller::Users", "type": "use"},
    {"from": "MyApp::Model::User", "to": "MyApp::Utils", "type": "use"}
  ],
  "cycles": []
}

# Focused on one module:
$ perl-lsp --dependency-graph . --root MyApp::App --depth 2
MyApp::App
  → MyApp::Utils (use)
  → MyApp::Controller::Users (use)
    → MyApp::Model::User (use)
    → MyApp::Utils (use)
```

**Implementation:** Iterate workspace index, collect `imports` from each `FileAnalysis`, build adjacency list. Detect cycles with DFS. Output in requested format.

### `--export-api` — Machine-readable API docs

```
perl-lsp --export-api <root> <module-or-file> [--format json|markdown]
```

```bash
$ perl-lsp --export-api . MyApp::Utils --format json
{
  "package": "MyApp::Utils",
  "file": "lib/MyApp/Utils.pm",
  "exports": ["format_date", "parse_config", "normalize_name"],
  "subs": {
    "format_date": {
      "line": 7,
      "params": [{"name": "$date", "type": "String"}],
      "return_type": "String",
      "doc": "Format a date for display.",
      "is_method": false
    }
  },
  "parents": [],
  "framework": null
}

$ perl-lsp --export-api . MyApp::Model::User --format markdown
# MyApp::Model::User

**Inherits from:** (none)
**Framework:** Moo

## Accessors

- `name` (ro) — String
- `email` (rw) — String

## Methods

### `process_data($input) → HashRef`

Process input data and return normalized result.

### `get_display_name() → String`

Returns formatted display name.
```

**Implementation:** Index workspace, find the module, extract symbols + exports + inheritance + framework info from FileAnalysis, format.

### `--impact` — Reverse dependency analysis

```
perl-lsp --impact <root> <file-or-module>
```

```bash
$ perl-lsp --impact . lib/MyApp/Utils.pm
Direct importers:
  lib/MyApp/Controller/Users.pm  imports: format_date, normalize_name
  lib/MyApp/App.pm               imports: parse_config, format_date
  t/basic.t                      imports: format_date, normalize_name, parse_config
Inheritors: (none)
Method callers:
  (MyApp::Utils has no methods — only exported functions)

Total: 3 files directly affected by changes to MyApp::Utils
```

**Implementation:** Reverse the dependency graph — for each file in workspace, check if it imports from the target module. Also check inheritance chain (who inherits from this class?).

### `--type-at` — Single type query

```
perl-lsp --type-at <file> <line> <col>
```

```bash
$ perl-lsp --type-at lib/MyApp/App.pm 12 4
ClassName(MyApp::Controller::Users)

$ perl-lsp --type-at lib/MyApp/App.pm 14 4
HashRef
```

One line, one type. For scripting.

### `--framework-report` — OOP audit

```
perl-lsp --framework-report <root> [--format json|human]
```

```bash
$ perl-lsp --framework-report .
Moo: 12 classes
  MyApp::Model::User (3 accessors, 2 methods)
  MyApp::Model::Post (5 accessors, 4 methods, has roles: Serializable)
  ...
Moose: 0 classes
Mojo::Base: 3 classes
  MyApp::App (parent: Mojolicious, 2 helpers, 5 routes)
  MyApp::Controller::Users (parent: Mojolicious::Controller)
  ...
DBIC: 8 result classes
  MyApp::Schema::Result::User (4 columns, 2 relationships)
  ...
Plain OOP (bless): 4 classes
  MyApp::Legacy::Parser
  ...

Total: 27 classes across 27 files
```

**Implementation:** Iterate workspace index, check `framework_modes` and `package_parents` on each FileAnalysis, aggregate by framework.

### `--repl-complete` — REPL completion backend

```
echo '<accumulated code>' | perl-lsp --repl-complete [--line <n>] [--col <n>]
```

Read accumulated REPL code from stdin. Parse it as a single file. Run completion at the last cursor position (or specified position). Output completions.

```bash
$ echo 'use Mojo::UserAgent; my $ua = Mojo::UserAgent->new; $ua->' | perl-lsp --repl-complete
get            Method  → Mojo::Transaction::HTTP
post           Method  → Mojo::Transaction::HTTP
head           Method  → Mojo::Transaction::HTTP
put            Method  → Mojo::Transaction::HTTP
delete         Method  → Mojo::Transaction::HTTP
start          Method  → Mojo::UserAgent
on             Method
build_tx       Method  → Mojo::Transaction::HTTP
```

**Implementation:** Read stdin, parse with tree-sitter, build FileAnalysis, detect cursor context at end of input (or at specified position), run completion. The same engine as LSP completion, different entry point.

For integration with Reply or Devel::REPL, the REPL would pipe accumulated session code + current line to this command on each tab press.

---

## Part 3: Output Formats

### Human (default for tty)

Colored, columnar, with context. Detect tty via `isatty(stdout)`.

### JSON

Machine-readable, one top-level array or object. For piping to `jq`, CI systems, custom tools.

### SARIF (for `--check` only)

Static Analysis Results Interchange Format. GitHub, GitLab, Azure DevOps consume natively. Diagnostics appear as PR annotations.

```json
{
  "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json",
  "version": "2.1.0",
  "runs": [{
    "tool": {
      "driver": {
        "name": "perl-lsp",
        "version": "0.1.0",
        "rules": [
          {"id": "PL001", "name": "unresolved-function", "shortDescription": {"text": "Unresolved function call"}},
          {"id": "PL002", "name": "unresolved-method", "shortDescription": {"text": "Unresolved method call"}}
        ]
      }
    },
    "results": [
      {
        "ruleId": "PL001",
        "level": "warning",
        "message": {"text": "unresolved function 'proccess_data'"},
        "locations": [{
          "physicalLocation": {
            "artifactLocation": {"uri": "lib/MyApp/Controller/Users.pm"},
            "region": {"startLine": 47, "startColumn": 12}
          }
        }]
      }
    ]
  }]
}
```

### Vim/Emacs quickfix format

For `--definition` and `--references`:

```
# --format vim: +line file
+10 lib/MyApp/Model/User.pm

# --format quickfix: file:line:col: message
lib/MyApp/Utils.pm:7:4: def sub format_date
```

---

## Part 4: Suppression System

### Comment-based suppression

```perl
# Suppress one diagnostic on the next line:
# perl-lsp: ignore-next-line(PL001)
some_dynamic_call();

# Suppress on the same line:
some_dynamic_call(); # perl-lsp: ignore(PL001)

# Suppress multiple:
# perl-lsp: ignore-next-line(PL001, PL002)

# Suppress for entire file (must be in first 10 lines):
# perl-lsp: ignore-file(PL004)

# Suppress all diagnostics on next line:
# perl-lsp: ignore-next-line
```

### Config-based suppression

`.perl-lsp.json`:
```json
{
  "diagnostics": {
    "unused-variable": "off"
  },
  "exclude": ["t/**", "script/**"],
  "suppress": {
    "PL006": {
      "patterns": ["^_"],
      "comment": "Private subs may be called dynamically"
    }
  }
}
```

### Implementation

**Comment scanning:** During the builder walk (or as a post-pass), scan `comment` nodes for `perl-lsp:` prefix. Parse the directive and store in `FileAnalysis.suppressions`.

**Config loading:** Read `.perl-lsp.json` at startup. Store on `Backend`. Pass to diagnostic collection.

**Diagnostic filtering:** `collect_diagnostics` checks:
1. Is this diagnostic code disabled in config? Skip.
2. Is this line suppressed by a comment directive? Skip.
3. Does this match a suppress pattern? Skip.
4. Map severity from config (may differ from default).

---

## Part 5: Framework Migration via CLI

### The insight

The builder already decomposes framework DSL into semantic parts:
- `has name => (is => 'ro', isa => 'Str')` → accessor name, readability, type constraint
- `use Moo; extends 'Parent'; with 'Role'` → framework, inheritance, role composition
- `__PACKAGE__->add_columns('name', { ... })` → DBIC columns with types
- `use Mojo::Base 'Mojolicious::Controller'` → framework, parent class

We have all the information needed to **translate between framework DSLs**. The analysis engine is framework-agnostic at the semantic level — it knows "this class has a read-only accessor named `name` of type String that returns `$self`'s value." The source framework is just syntax.

### `--migrate` — Framework translation

```
perl-lsp --migrate <root> <file-or-module> --from <framework> --to <framework> [--dry-run] [--format diff|full]
```

#### Moo/Moose → Perl core `class` (feature 'class')

```bash
$ perl-lsp --migrate . lib/MyApp/Model/User.pm --from moo --to core-class --dry-run
```

**Input (Moo):**
```perl
package MyApp::Model::User;
use Moo;
use Types::Standard qw(Str Int);

extends 'MyApp::Model::Base';
with 'MyApp::Role::Serializable';

has name  => (is => 'ro', isa => Str, required => 1);
has email => (is => 'rw', isa => Str);
has age   => (is => 'ro', isa => Int, default => sub { 0 });

sub greet {
    my ($self) = @_;
    return "Hello, " . $self->name;
}

1;
```

**Output (core class):**
```perl
use feature 'class';

class MyApp::Model::User :isa(MyApp::Model::Base) :does(MyApp::Role::Serializable) {
    field $name  :param :reader;
    field $email :param :reader :writer;
    field $age   :param :reader = 0;

    method greet () {
        return "Hello, " . $self->name;
    }
}
```

**Translation rules — Moo/Moose → core class:**

| Moo/Moose | Core class |
|-----------|-----------|
| `package Foo; use Moo;` | `class Foo {` |
| `extends 'Parent'` | `:isa(Parent)` |
| `with 'Role'` | `:does(Role)` |
| `has name => (is => 'ro')` | `field $name :param :reader;` |
| `has name => (is => 'rw')` | `field $name :param :reader :writer;` |
| `has name => (is => 'ro', required => 1)` | `field $name :param :reader;` (param implies required) |
| `has name => (is => 'ro', default => sub { [] })` | `field $name :reader = [];` (no :param if default) |
| `has name => (is => 'lazy', builder => '_build_name')` | `field $name :reader; ADJUST { $name = $self->_build_name }` |
| `has name => (is => 'rwp')` | `field $name :param :reader; method _set_name ($val) { $name = $val }` |
| `sub foo { my ($self) = @_; ... }` | `method foo () { ... }` |
| `sub foo { my ($self, $x, $y) = @_; ... }` | `method foo ($x, $y) { ... }` |
| `around foo => sub { my ($orig, $self) = @_; ... }` | `method foo () { my $orig = $self->SUPER::can('foo'); ... }` |
| `1;` at end | (removed) |
| `use Types::Standard qw(...)` | (removed — core class doesn't need type modules) |

**What we CAN'T translate:**
- `BUILD`/`BUILDARGS` — no direct core class equivalent (use `ADJUST`)
- `DEMOLISH` → core class has no destructor hook yet
- Complex type constraints (`isa => 'ArrayRef[HashRef[Str]]'`) — core class has no type system
- Coercions — no core equivalent
- Trigger (`trigger => sub { ... }`) — no core equivalent
- Delegation (`handles => [qw(foo bar)]`) — no core equivalent

For untranslatable patterns, emit a comment: `# TODO: manual migration needed — Moo trigger not supported in core class`

#### Moose → Moo

```bash
$ perl-lsp --migrate . lib/MyApp/Model/ --from moose --to moo --dry-run
```

| Moose | Moo |
|-------|-----|
| `use Moose;` | `use Moo;` |
| `use Moose::Role;` | `use Moo::Role;` |
| `has name => (is => 'ro', isa => 'Str')` | `has name => (is => 'ro', isa => Str)` (with `use Types::Standard`) |
| `__PACKAGE__->meta->make_immutable;` | (removed — Moo is immutable by default) |
| `override 'foo' => sub { super(); ... }` | `around foo => sub { my ($orig, $self) = @_; $self->$orig(); ... }` |
| `augment` / `inner` | `# TODO: no Moo equivalent for augment/inner` |
| `MooseX::*` extensions | `# TODO: check Moo compatibility for MooseX::Foo` |

#### Moo → Moose (reverse)

Straightforward — Moose is a superset. Main change: `use Moo` → `use Moose`, add `__PACKAGE__->meta->make_immutable` at end. Type constraints: `use Types::Standard` → inline strings.

#### `use base` → `use parent`

```bash
$ perl-lsp --migrate . --from base --to parent
```

Simple regex-level change, but we can do it semantically:
- `use base 'Foo'` → `use parent 'Foo'`
- `use base qw(Foo Bar)` → `use parent qw(Foo Bar)`
- Remove `-norequire` if present (parent has it too, same syntax)

#### Bless-based OOP → Moo

The ambitious one. `bless {}` constructors → Moo classes with `has` declarations.

```perl
# Input:
package Foo;
sub new {
    my ($class, %args) = @_;
    return bless {
        name  => $args{name} || 'default',
        count => $args{count} || 0,
    }, $class;
}
sub name { return $_[0]->{name} }
sub count { return $_[0]->{count} }
```

```perl
# Output:
package Foo;
use Moo;
has name  => (is => 'ro', default => sub { 'default' });
has count => (is => 'ro', default => sub { 0 });
```

**How:** The builder already extracts:
- Constructor hash keys from `bless { key => val }` → these become `has` attributes
- Accessor methods (subs that return `$_[0]->{name}`) → detected by return pattern
- Whether the accessor is read-only (no setter) or read-write (has setter pattern)

This is heuristic — the builder can detect the common patterns, not every possible OOP style. Untranslatable patterns get TODO comments.

### `--migrate` implementation

```rust
fn cli_migrate(root: &str, target: &str, from: &str, to: &str, dry_run: bool) {
    // 1. Index workspace
    // 2. Find target file(s) — single file or directory
    // 3. For each file:
    //    a. Check framework_modes matches --from
    //    b. Read source
    //    c. Build FileAnalysis
    //    d. Apply translation rules to produce new source
    //    e. If --dry-run, output diff; else write file
}
```

The translation engine needs:
- **Source reader:** understands the original framework's `has` syntax, inheritance, roles
- **Target writer:** generates the target framework's syntax
- **Span-based editing:** rather than generating a whole new file, produce edits (like rename does) that transform specific regions

Span-based editing preserves comments, formatting, and non-framework code. Only the framework boilerplate changes.

### Translation data flow

```
FileAnalysis (framework-agnostic semantic model)
    ↓
┌─────────────────────────────────────────────┐
│ Semantic: class Foo                         │
│   parent: Bar                               │
│   roles: [Serializable]                     │
│   attributes:                               │
│     name: {ro, type: Str, required: true}   │
│     email: {rw, type: Str}                  │
│     age: {ro, type: Int, default: 0}        │
│   methods: [greet, process]                 │
│   method_params: {greet: [], process: [$x]} │
└─────────────────────────────────────────────┘
    ↓
Target framework writer
    ↓
Edits (span-based, preserving non-framework code)
```

The `FileAnalysis` is already framework-agnostic at the semantic level. The translation is: read framework-specific syntax → (semantic model already exists) → write different framework syntax.

## Implementation Ordering

| Phase | Commands | Effort | Value |
|-------|----------|--------|-------|
| **1** | `--check` + diagnostic framework + SARIF | High | Critical for CI adoption |
| **2** | `--outline`, `--hover`, `--type-at` | Low | Quick wins, useful for scripting |
| **3** | `--references`, `--definition` | Low | Building blocks |
| **4** | `--unused-exports`, `--dead-code` (+ as diagnostics PL005/PL006) | Medium | High value for cleanup |
| **5** | `--completions`, `--repl-complete` | Medium | REPL integration |
| **6** | `--dependency-graph`, `--impact` | Medium | Architecture understanding |
| **7** | `--export-api`, `--framework-report` | Medium | Documentation/audit |
| **8** | Suppression system (comments + config) | Medium | Required for CI adoption at scale |
| **9** | `--migrate` — Moo→core class, Moose→Moo, base→parent | High | Modernization killer feature |
| **10** | `--migrate` — bless→Moo (heuristic) | High | Legacy migration |

Phase 1 + 8 = "CI-ready" milestone. Phases 2-3 are trivial wiring. Phase 4 is the most requested cleanup feature. Phase 5 is the REPL story. Phase 9 is the "holy crap" feature that makes conference talks.

---

## Files to modify/create

| File | Change |
|------|--------|
| `src/main.rs` | CLI arg dispatch for all new commands |
| `src/diagnostics.rs` | **New.** Diagnostic types, severity config, collection framework, SARIF output |
| `src/config.rs` | **New.** `.perl-lsp.json` parsing, diagnostic config, suppression rules |
| `src/file_analysis.rs` | Add `suppressions: Vec<Suppression>` field |
| `src/builder.rs` | Scan comments for suppression directives |
| `src/symbols.rs` | Refactor `collect_diagnostics` to use diagnostic framework |
| `src/backend.rs` | Load config, pass to diagnostic collection, read suppression from FA |

---

## Tests

### Diagnostic framework
```rust
#[test]
fn test_unresolved_function_diagnostic() {
    // Call to nonexistent function → PL001
}

#[test]
fn test_unused_import_diagnostic() {
    // use Foo qw(bar); — bar never used → PL003
}

#[test]
fn test_unused_variable_diagnostic() {
    // my $x = 1; — $x never read → PL004
}

#[test]
fn test_suppression_ignore_next_line() {
    // # perl-lsp: ignore-next-line(PL001)
    // dynamic_call(); — should NOT produce PL001
}

#[test]
fn test_suppression_same_line() {
    // dynamic_call(); # perl-lsp: ignore(PL001)
}

#[test]
fn test_severity_config_override() {
    // Config: PL001 = "error" → severity should be Error, not Warning
}
```

### CLI commands
```bash
# --check
perl-lsp --check test_project/ --format json | jq length  # should be > 0

# --outline
perl-lsp --outline test_files/sample.pl | grep 'Calculator'  # should find the class

# --references
perl-lsp --references . test_files/sample.pl 20 4 | wc -l  # should find refs

# --dead-code
perl-lsp --dead-code test_project/ | grep '_unused_func'  # should find it

# --unused-exports
perl-lsp --unused-exports test_project/ | grep 'never_imported'  # should find it

# --repl-complete
echo 'use List::Util qw(max min); max' | perl-lsp --repl-complete  # should show max
```

### E2E
```lua
t.test("diagnostics: unused import flagged", function() end)
t.test("diagnostics: suppression via comment works", function() end)
t.test("diagnostics: severity from config respected", function() end)
```
