# Task: Cross-file Intelligence Gaps

**Branch:** `cross-file-gaps-v2` (already created from `main`)
**Baseline:** 200 unit tests, 53 e2e tests, 0 warnings

## Goal

Three improvements to cross-file intelligence, motivated by testing against real codebases (Rex: 343 modules, Mojo: 112 modules):

1. **`__PACKAGE__` resolution** — `__PACKAGE__->new(...)` should resolve to the enclosing package, not literal `"__PACKAGE__"`
2. **Implicit parameter extraction** — `shift` and `$_[N]` patterns alongside the existing `my (...) = @_`
3. **POD documentation on hover** — show the POD section for imported functions

## Gap 1: `__PACKAGE__` resolution

### Problem

`Mojo::File` has:
```perl
package Mojo::File;
sub path { __PACKAGE__->new(@_) }
```

The builder infers `path()` returns `Object:__PACKAGE__` instead of `Object:Mojo::File`. This affects 7 exported functions in Mojo alone (`b()`, `c()`, `path()`, `curfile()`, `tempdir()`, `tempfile()`, `scope_guard()`).

### Fix

Wherever `__PACKAGE__` appears as a class name (method call invocant, `bless` second arg), resolve it to the current `package` from the scope chain.

The builder already tracks the current package — every scope has an optional `package: Option<String>` field, and `ScopeKind::Package` records the package name. The scope chain lookup is already implemented for other purposes.

**Specific locations to fix:**

1. **Method call expression handling** — when the invocant is `__PACKAGE__`, resolve it to the enclosing package name before creating the `ClassName(...)` type.

2. **`bless` handling** — when the second argument to `bless` is `__PACKAGE__`, resolve it.

3. **Return type inference** — when `sub_return_type()` encounters a return of `__PACKAGE__->new(...)`, the type should already be correct if (1) is fixed.

**How to find the current package:**

```rust
fn current_package(&self) -> Option<&str> {
    // Walk scope chain from current scope to root
    let mut scope_id = self.current_scope;
    loop {
        let scope = &self.scopes[scope_id.0 as usize];
        if let Some(ref pkg) = scope.package {
            return Some(pkg.as_str());
        }
        match scope.parent {
            Some(parent) => scope_id = parent,
            None => return None,
        }
    }
}
```

This method likely already exists or is trivially constructable from existing scope-chain walking code. When the builder sees `__PACKAGE__` as a bareword in invocant position, substitute the result of `current_package()`.

### Test

```rust
#[test]
fn test_dunder_package_resolution() {
    let fa = build_fa("
        package Mojo::File;
        sub path { __PACKAGE__->new(@_) }
    ");
    let rt = fa.sub_return_type("path");
    assert_eq!(rt, Some(&InferredType::ClassName("Mojo::File".into())));
}
```

## Gap 2: Implicit parameter extraction

### Problem

The builder currently extracts parameters only from:
- Signature syntax: `sub foo($x, $y) { }`
- List assignment: `my ($self, $file, @opts) = @_;`

But many Perl subs use `shift` or `$_[N]`:

```perl
# shift pattern — 209 files in Rex use this
sub do_something {
    my $self = shift;
    my $file = shift;
    my $opts = shift || {};
    ...
}

# $_[N] pattern — 25 files in Rex, 65 in Mojo
sub handler {
    my $self = $_[0];
    ...
}
```

**Coverage impact:** In Rex, 127/272 exported subs (47%) have no extracted params. Many of these use `shift`.

### Approach

When processing statements inside a sub body, detect `shift`-based parameter extraction.

#### Pattern 1: `my $var = shift;`

At the start of a sub body, consecutive `my $var = shift;` statements are parameter declarations. Stop collecting when a non-shift statement is encountered.

**Tree-sitter structure:**
```
(variable_declaration
  variable: (scalar)
  (assignment_expression
    right: (ambiguous_function_call_expression  ; or function_call_expression
      function: (bareword) @fn (#eq? @fn "shift"))))
```

Also handle: `my $var = shift || $default;` and `my $var = shift // $default;` — these are params with defaults. The default is the RHS of `||` or `//`.

**Implementation:** After the existing `my (...) = @_` check, add a second pass that scans top-level statements for consecutive `shift` assignments. Collect them as `ParamInfo { name, is_slurpy: false }`. Stop at the first non-shift statement.

**Important:** Only extract `shift` params if no `my (...) = @_` pattern was found — the two patterns are mutually exclusive. If a sub has `my ($self) = @_; my $file = shift;`, the `@_` pattern already captured `$self` and the `shift` is something else.

Actually, a common pattern IS:
```perl
my $self = shift;
my ($file, @opts) = @_;
```

Handle this: if the first statement(s) are `shift` and then there's a `my (...) = @_`, combine them. The shifts become the first N params, the `@_` unpacking becomes the rest.

#### Pattern 2: `$_[N]` access (lower priority)

Less common and harder to extract cleanly — `$_[0]` might be `$self`, `$_[1]` might be the first real param, but the names are unknown. Consider this out of scope for now unless there's a `my $var = $_[N]` pattern.

If there is `my $self = $_[0];` at the top of a sub, extract it as a param. But don't try to infer param names from bare `$_[N]` usage.

### Data model

No changes to `ParamInfo` — it already has `name`, `default`, `is_slurpy`. The `shift` params populate the same struct.

For `my $file = shift || "/tmp/default"`, set `default: Some("/tmp/default".into())`.

### Test

```rust
#[test]
fn test_shift_params() {
    let fa = build_fa("
        sub process {
            my $self = shift;
            my $file = shift;
            my $opts = shift || {};
        }
    ");
    let sig = fa.signature_for_call("process", false, None, Point::new(0, 0)).unwrap();
    assert_eq!(sig.params.len(), 3);
    assert_eq!(sig.params[0].name, "$self");
    assert_eq!(sig.params[1].name, "$file");
    assert_eq!(sig.params[2].name, "$opts");
    assert_eq!(sig.params[2].default, Some("{}".into()));
}

#[test]
fn test_shift_then_list_assign() {
    let fa = build_fa("
        sub process {
            my $self = shift;
            my ($file, @opts) = @_;
        }
    ");
    let sig = fa.signature_for_call("process", false, None, Point::new(0, 0)).unwrap();
    assert_eq!(sig.params.len(), 3);
    assert_eq!(sig.params[0].name, "$self");
    assert_eq!(sig.params[1].name, "$file");
    assert_eq!(sig.params[2].name, "@opts");
    assert!(sig.params[2].is_slurpy);
}
```

## Gap 3: POD documentation on hover

### Problem

When hovering over an imported function, we show `sub foo($x, $y) → HashRef` and "imported from Module". But Perl modules have rich POD documentation right above each sub:

```perl
=head2 file($file, %options)

Manage a file on the remote system. Options:

  ensure  => present/absent
  content => string
  source  => local file

=cut

sub file {
    my ($file, @options) = @_;
```

This documentation should appear in hover.

### Architecture

**No tree-sitter-pod dependency.** tree-sitter-perl already parses POD as opaque `pod` nodes and comments as `comment` nodes. We use the existing tree to locate documentation, then convert raw POD to markdown with a regex-based state machine (ported from [PerlNavigator's pod.ts](../PerlNavigator/server/src/pod.ts)).

**Two POD placement styles exist in the wild:**

| Style | Example | Prevalence |
|---|---|---|
| **Interleaved** — POD block immediately before each `sub` | Rex (54/56 modules) | Authoring tools, older CPAN |
| **Tail POD** — all code first, then `1;`/`__END__`, then all POD with `=head2 func_name` sections | Mojo (108/110 modules), Carp, List::Util | Mojo convention, core Perl |

Both must be supported. The extraction strategy has two passes:

1. **`prev_sibling` walk** — find `pod` or `comment` nodes immediately preceding the sub's tree node (handles interleaved style)
2. **`=head2` name scan** — if pass 1 finds nothing, scan ALL `pod` nodes in the file for a `=head2` header matching the function name, then extract that section (handles tail POD style)

**Conversion:** Regex-based line-by-line POD→markdown converter in a new `src/pod.rs` module.

**Storage:** Add `doc: Option<String>` to `ExportedSub`. Store as pre-rendered markdown (conversion is cheap, avoids needing raw POD at query time).

**Display:** In hover, append the doc string below the signature.

### POD extraction per sub

For each sub, find documentation using the two-pass strategy:

**Pass 1: Preceding documentation (interleaved style).**

Walk backwards through preceding siblings of the sub node. Collect POD and/or comment blocks. POD takes priority. Stop when you hit code (not POD/comment).

**Pass 2: Name-matched POD section (tail POD style).**

If pass 1 finds no POD, scan all `pod` nodes in the file for a `=head2` header matching the function name. Extract that section (from `=head2 func_name` until the next `=head` at the same or higher level, or `=cut`). The `=head2` line may include a signature: `=head2 path($file)` — match by bare name before parens.

**Choosing what to show (priority order):**
1. Preceding POD block (interleaved) → convert to markdown
2. Preceding comments → show as-is (stripped of `#`)
3. Name-matched `=head2` section from any POD block in the file (tail style) → convert to markdown

### POD → Markdown converter (`src/pod.rs`)

New module: `src/pod.rs` — a regex-based line-by-line state machine, ported from PerlNavigator's `pod.ts` (566 lines). The Rust port will be more concise.

**Reference:** `../PerlNavigator/server/src/pod.ts`

**Public API:**

```rust
/// Convert raw POD text to markdown.
pub fn pod_to_markdown(pod_text: &str) -> String
```

**Conversions (priority order — implement these first, iterate on the rest):**

| POD construct | Markdown output | Priority |
|---|---|---|
| `=head1 Title` | `### Title` | P0 |
| `=head2 Title` | `#### Title` | P0 |
| `=head3 Title` / `=head4 Title` | `##### Title` | P1 |
| `C<code>` | `` `code` `` | P0 |
| `B<bold>` | `**bold**` | P0 |
| `I<italic>` | `*italic*` | P0 |
| `L<Foo::Bar>` | `Foo::Bar` | P1 |
| `L<text\|url>` | `text` | P1 |
| `F<file>` | `` `file` `` | P1 |
| `E<lt>` / `E<gt>` | `<` / `>` | P1 |
| `=over` / `=item *` / `=back` | Bullet list | P1 |
| `=item Label` | `- **Label**` | P1 |
| Verbatim (4+ spaces or tab) | Fenced code block | P1 |
| `=cut` | Stop processing | P0 |
| `=pod` | Start processing | P0 |
| `=begin`/`=end`/`=for`/`=encoding` | Skip or passthrough | P2 |

**Inline formatting:** Process interior sequences handling nesting. `C<< code >>` (double angle brackets) is also valid POD.

**Start with P0, ship, iterate on P1/P2.** Even just headings + `C<>` + `B<>` + `I<>` covers 80% of POD in the wild.

**Truncation:** Cap the rendered markdown at ~2000 chars. Most per-function POD is well under this.

### Store in ExportedSub

Add field to `ExportedSub` in `module_index.rs`:

```rust
pub struct ExportedSub {
    pub def_line: u32,
    pub params: Vec<ExportedParam>,
    pub is_method: bool,
    pub return_type: Option<InferredType>,
    pub hash_keys: Vec<String>,
    pub doc: Option<String>,  // NEW: pre-rendered markdown from POD or comments
}
```

**JSON serialization:** Add `"doc": "markdown string"` to the subprocess JSON output. Only include when present.

**SQLite:** The `subs TEXT` column already stores JSON per-sub. The `doc` field is just another key in the JSON object. No schema change needed — the JSON is self-describing. Bump `SCHEMA_VERSION` to force cache invalidation so stale entries without `doc` are re-resolved.

### Display in hover

After the signature line, append the doc, then the module attribution:

```rust
if let Some(ref doc) = sub_info.doc {
    parts.push(doc.clone());
}
parts.push(format!("*imported from `{}`*", import.module_name));
```

The hover output becomes:
```
sub file($file, @options) → HashRef

Manage a file on the remote system. Options:
- `ensure` — present/absent
- `content` — string
- `source` — local file

*imported from `Rex::Commands::File`*
```

## What NOT to do

- Don't add a `tree-sitter-pod` dependency — use the existing Perl tree + regex conversion
- Don't try to extract params from bare `$_[N]` usage (no variable name to show) — only `my $var = $_[N]`
- Don't parse POD at query time — extract and cache during module analysis
- Don't try to perfectly render all POD on v1 — start with P0 conversions (headings, `C<>`, `B<>`, `I<>`, `=cut`), iterate on P1/P2
- Don't change the enrichment interface — `doc` is only used in hover, not type inference
- Don't extract POD for local subs — this is cross-file only (local subs already show the declaration line)

## Test plan

### Unit tests

1. **`__PACKAGE__` resolution**: `build_fa("package Foo; sub new { __PACKAGE__->new }") → ClassName("Foo")`
2. **`shift` params**: sub with consecutive shifts → correct param list
3. **`shift` + `@_` combo**: `my $self = shift; my ($x, @y) = @_` → combined param list
4. **`shift` with default**: `my $x = shift || "default"` → param with default
5. **POD extraction**: sub with preceding POD → raw text extracted and converted
6. **Tail POD extraction**: sub with POD in `=head2` section after `1;` → extracted by name match
7. **Comment extraction**: sub with preceding `comment` nodes → stripped text
8. **POD→markdown conversion** (`pod.rs`): raw POD string → correct markdown (headings, `C<>`, `B<>`, `I<>`, verbatim, lists)
9. **ExportedSub with doc**: roundtrip through JSON and SQLite

### E2E tests

1. **Hover on imported function shows POD** (requires a test module with POD)
2. **Go-to-def lands on correct line for `shift`-param subs**
3. **Signature help shows `shift` params**

## Verification

1. `cargo test` — all 200 existing tests pass + new tests
2. `cargo build` — no warnings
3. All 53 existing e2e tests pass
4. Manual: `./target/release/perl-lsp --parse-exports /tmp/Rex/lib/Rex/Commands.pm` — check `shift` params extracted
5. Manual: `./target/release/perl-lsp --parse-exports /tmp/mojo/lib/Mojo/File.pm` — check `path()` returns `Object:Mojo::File` (not `__PACKAGE__`)
6. Manual: `./target/release/perl-lsp --parse-exports /tmp/Rex/lib/Rex/Commands/File.pm` — check `file()` has `doc` field with POD markdown
7. Manual: `./target/release/perl-lsp --parse-exports /tmp/mojo/lib/Mojo/File.pm` — check `path()` has `doc` field (tail POD)
