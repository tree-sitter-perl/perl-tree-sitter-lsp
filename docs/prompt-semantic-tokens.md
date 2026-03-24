# Semantic Tokens: Making Perl Readable

## Motivation

We currently emit semantic tokens only for variables (scalar/array/hash + declaration/modification). The analysis engine knows far more — parameter roles, framework DSL, hash key semantics, import provenance, type info. Surfacing this via semantic tokens makes Perl code dramatically more readable, especially in VS Code which doesn't use tree-sitter for highlighting.

## Token Types

### Current
| Type | What |
|------|------|
| `variable` | `$x`, `@arr`, `%hash` |

### Proposed additions

| Type | What | Why it matters |
|------|------|---------------|
| `parameter` | `$name` in `my ($self, $name) = @_` | Distinct from local vars — most themes style params differently |
| `selfKeyword` | `$self`, `$class` as first param / invocant | Like `self`/`this` in other languages — brain skips it instantly |
| `function` | `format_date()`, `croak()` | Distinguish from method calls |
| `method` | `$obj->process()`, `$self->name()` | Distinguish from function calls |
| `macro` | `has`, `with`, `extends`, `around`, `before`, `after`, `hook`, `helper` | Framework DSL stands out from regular function calls |
| `property` | `$obj->{name}`, `name => 'val'`, `$hash{timeout}` | Hash keys look like properties, not strings |
| `namespace` | `Foo::Bar` in `use Foo::Bar`, `Foo::Bar->new()`, `package Foo::Bar` | Type/class coloring instead of bareword |
| `regexp` | `qr/pattern/`, `/pattern/` | Regex literal highlighting |
| `enumMember` | `use constant FOO => 'bar'` — the constant name | Constants look constant |

### LSP token type mapping

LSP has a fixed set of standard token types. We map to them:

```rust
pub fn semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::VARIABLE,       // 0: variables
        SemanticTokenType::PARAMETER,      // 1: sub parameters
        SemanticTokenType::FUNCTION,       // 2: function calls
        SemanticTokenType::METHOD,         // 3: method calls
        SemanticTokenType::MACRO,          // 4: framework DSL keywords
        SemanticTokenType::PROPERTY,       // 5: hash keys
        SemanticTokenType::NAMESPACE,      // 6: package/class names
        SemanticTokenType::REGEXP,         // 7: regex literals
        SemanticTokenType::ENUM_MEMBER,    // 8: constants
        SemanticTokenType::KEYWORD,        // 9: $self/$class
        SemanticTokenType::STRING,         // 10: interpolated vars in strings (VS Code)
    ]
}
```

Note: `$self` maps to `KEYWORD` because there's no standard `selfKeyword` type. Most themes render keywords distinctly. Alternatively, use `VARIABLE` + a custom modifier, but `KEYWORD` gives the best out-of-box visual in most themes.

## Token Modifiers

### Current
| Modifier | What |
|----------|------|
| `declaration` | `my $x`, `sub foo` |
| `modification` | `$x = ...`, `$x++` |
| `scalar` | `$` sigil |
| `array` | `@` sigil |
| `hash` | `%` sigil |

### Proposed additions

| Modifier | What | Why |
|----------|------|-----|
| `readonly` | `use constant`, Moo `is => 'ro'` accessor, Perl class `:reader` | Visual hint that it's immutable |
| `defaultLibrary` | Imported function (`use Foo qw(bar)` → `bar()`) | Know at a glance if a function is imported |
| `documentation` | Sub with POD docs attached | Subtle indicator that docs exist |
| `deprecated` | Unused variable, `use base` pattern | Editors gray these out |
| `static` | Class method calls (`Foo->method()`) vs instance (`$obj->method()`) | Distinguish static from instance context |

```rust
pub fn semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,      // 0
        SemanticTokenModifier::MODIFICATION,     // 1
        SemanticTokenModifier::READONLY,         // 2
        SemanticTokenModifier::DEFAULT_LIBRARY,  // 3
        SemanticTokenModifier::DOCUMENTATION,    // 4
        SemanticTokenModifier::DEPRECATED,       // 5
        SemanticTokenModifier::STATIC,           // 6
        // Custom modifiers (bitfield):
        SemanticTokenModifier::new("scalar"),    // 7
        SemanticTokenModifier::new("array"),     // 8
        SemanticTokenModifier::new("hash"),      // 9
    ]
}
```

## Token Emission by Pattern

### Variables (enhanced from current)

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `my $x = 1` | variable | declaration, scalar |
| `$x = 2` (assignment) | variable | modification, scalar |
| `$x` (read) | variable | scalar |
| `my ($self, $name) = @_` — `$self` | keyword | declaration |
| `my ($self, $name) = @_` — `$name` | parameter | declaration, scalar |
| `shift` result assigned as first param | keyword | declaration |
| `my @items` | variable | declaration, array |
| `my %opts` | variable | declaration, hash |
| Unused `my $x` (never read) | variable | declaration, deprecated, scalar |

### $self / $class detection

The builder already identifies the first parameter of methods. Rules:
- `my ($self) = @_` or `my $self = shift` in a sub → `$self` is keyword
- `my ($class) = @_` or `my $class = shift` in `sub new` → `$class` is keyword
- Perl 5.38 `method` keyword → implicit `$self` is keyword
- All subsequent uses of `$self`/`$class` in the same scope → keyword (no modifiers)

### Function calls

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `format_date($x)` (local sub) | function | |
| `format_date($x)` (imported via `use`) | function | defaultLibrary |
| `croak("error")` (Perl builtin) | function | defaultLibrary |
| `Foo::bar()` (package-qualified) | function | static |

### Method calls

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `$obj->process()` | method | |
| `$self->name()` (ro accessor) | method | readonly |
| `$self->name("new")` (rw setter) | method | modification |
| `Foo->new()` (class method) | method | static |
| `$self->$dynamic_method()` | method | | (after constant folding)

### Framework DSL

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `has name => (...)` (Moo/Moose) | macro | |
| `has name => 'default'` (Mojo) | macro | |
| `with 'Role'` | macro | |
| `extends 'Parent'` | macro | |
| `around 'method' => sub { ... }` | macro | |
| `before`, `after` | macro | |
| `helper name => sub { ... }` (Mojo::Lite) | macro | |
| `hook before_dispatch => sub { ... }` | macro | |
| `get '/path' => sub { ... }` (Mojo::Lite) | macro | |
| `plugin 'Name'` (Mojo::Lite) | macro | |

Detection: check `framework_imports` set — if the function name is in there, emit `macro` instead of `function`.

### Hash keys / properties

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `$obj->{name}` | property | |
| `$hash{timeout}` | property | |
| `name => 'value'` (fat-comma key) | property | |
| `{ status => 'ok' }` (hash literal key) | property | declaration |
| `is => 'ro'` (inside `has`) | property | |

### Package/class names

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `package Foo::Bar` | namespace | declaration |
| `use Foo::Bar` | namespace | |
| `Foo::Bar->new()` | namespace | |
| `isa => 'Foo::Bar'` | namespace | |
| `use parent 'Foo::Bar'` | namespace | |
| `class Foo::Bar` | namespace | declaration |

### Constants

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `use constant FOO => 'bar'` — `FOO` | enumMember | declaration, readonly |
| `FOO` (usage of constant) | enumMember | readonly |

### Regex

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `qr/pattern/` | regexp | |
| `/pattern/` in match context | regexp | |
| `m/pattern/` | regexp | |
| `s/pattern/replacement/` — the pattern part | regexp | |

### String interpolation (VS Code support)

For editors without tree-sitter highlighting, emit variable tokens inside interpolated strings:

| Pattern | Type | Modifiers |
|---------|------|-----------|
| `"Hello $name"` — the `$name` part | variable | scalar |
| `"Count: @{[ $x + 1 ]}"` — the `$x` | variable | scalar |
| `"Hash: $hash{key}"` — `$hash` | variable | hash |

Only emit these when the editor signals it doesn't use tree-sitter highlighting (or always emit and let tree-sitter editors override — semantic tokens are lower priority than syntax highlighting in most editors).

## Implementation

### Builder changes

The builder already visits every relevant node. For each new token type, we need to emit a semantic token entry during the walk. Store tokens in a new vec on the builder:

```rust
struct SemanticToken {
    span: Span,
    token_type: u32,   // index into semantic_token_types()
    modifiers: u32,    // bitmask of semantic_token_modifiers()
}

// On Builder:
semantic_tokens: Vec<SemanticToken>,
```

Emission points:
- `visit_variable` → already emits variable tokens. Add parameter/self detection.
- `visit_function_call` / `visit_method_call` → emit function/method tokens. Check framework_imports for macro.
- `visit_hash_key` (new or extend existing) → emit property tokens for fat-comma keys and hash access keys.
- `visit_package` / `visit_use` → emit namespace tokens.
- `visit_constant` (new) → emit enumMember tokens.
- `visit_regex` (new) → emit regexp tokens.

### Symbols.rs changes

`semantic_tokens_full` already exists and converts builder output to LSP format. Extend the token type/modifier registration and the conversion logic.

### Performance consideration

Semantic tokens are requested for the full document on every edit. The current implementation iterates `analysis.refs` — this is fine for variables but adding all the new token types means more entries. Keep the token vec sorted by position (already is) and the delta encoding efficient (already is).

For large files (>5000 lines), consider `textDocument/semanticTokens/range` support — only compute tokens for the visible range. Not needed initially but worth noting.

## Test playground file

Create `test_files/semantic_tokens_playground.pl` — a file that exercises every token type and modifier, designed for visual QA:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# ── Package / namespace tokens ──
package MyApp::Demo;
use Moo;
use List::Util qw(max min sum);
use Carp qw(croak);
use parent 'MyApp::Base';

# ── Constants ──
use constant MAX_RETRIES => 5;
use constant DEFAULT_NAME => 'anonymous';

# ── Framework DSL (macro tokens) ──
extends 'MyApp::Base';
with 'MyApp::Role::Logging';

has name    => (is => 'ro', isa => 'Str', required => 1);
has email   => (is => 'rw', isa => 'Str');
has retries => (is => 'ro', default => sub { MAX_RETRIES });

# ── Method with $self (keyword token) ──
sub process {
    my ($self, $input, $options) = @_;    # $self=keyword, $input/$options=parameter

    # ── Local variables ──
    my $result = {};                       # variable, declaration
    my @items = (1, 2, 3);               # variable, declaration, array
    my %config = (timeout => 30);         # variable, declaration, hash

    # ── Hash keys (property tokens) ──
    $result->{status} = 'ok';             # property: status
    $result->{count} = scalar @items;     # property: count
    my $t = $config{timeout};             # property: timeout

    # ── Method calls ──
    my $name = $self->name;               # method, readonly
    $self->email('new@example.com');      # method, modification
    my $display = $self->get_display;     # method

    # ── Function calls ──
    my $total = sum(@items);              # function, defaultLibrary (imported)
    my $biggest = max(1, 2, 3);          # function, defaultLibrary
    croak("bad input") unless $input;     # function, defaultLibrary

    # ── Class method calls ──
    my $user = MyApp::Model::User->new(   # namespace + method, static
        name  => $input,                  # property
        email => $self->email,            # property + method
    );

    # ── Regex ──
    if ($input =~ /^\w+$/) {              # regexp
        my $cleaned = $input;
    }
    my $re = qr/\d{3}-\d{4}/;            # regexp

    # ── String interpolation (VS Code) ──
    my $greeting = "Hello, $name!";       # $name = variable inside string
    my $info = "You have $config{timeout}s timeout";

    # ── Unused variable (deprecated modifier) ──
    my $unused = 42;                      # variable, declaration, deprecated

    # ── Dynamic method via constant folding ──
    my $method = 'process';
    $self->$method($input);               # method (resolved)

    return $result;
}

# ── Constructor with $class ──
sub custom_new {
    my ($class, %args) = @_;              # $class=keyword, %args=parameter
    return bless {
        verbose => $args{verbose} || 0,   # property
    }, $class;
}

# ── Perl 5.38 class syntax ──
class Counter {
    field $count :param = 0;              # variable, declaration, parameter

    method increment () {
        $count++;                         # variable, modification
    }

    method get_count () {
        return $count;                    # variable
    }
}

# ── Mojolicious::Lite DSL ──
# (uncomment to test Mojo tokens)
# use Mojolicious::Lite;
# get '/hello' => sub { ... };           # macro
# helper db => sub { ... };              # macro
# hook before_dispatch => sub { ... };   # macro
# app->start;                            # macro

1;
```

## Neovim test config update

Add semantic token highlight groups to `test_nvim_init.lua` so e2e tests can visually verify:

```lua
-- Semantic token highlight overrides for perl-lsp
vim.api.nvim_create_autocmd("LspTokenUpdate", {
  callback = function(args)
    local token = args.data.token
    -- Map our custom token types to highlight groups
    if token.type == "macro" then
      vim.api.nvim_buf_set_extmark(args.buf, ..., { hl_group = "Keyword" })
    end
  end,
})

-- Or simpler — link semantic token groups:
vim.api.nvim_set_hl(0, "@lsp.type.macro.perl", { link = "Keyword" })
vim.api.nvim_set_hl(0, "@lsp.type.property.perl", { link = "Identifier" })
vim.api.nvim_set_hl(0, "@lsp.type.namespace.perl", { link = "Type" })
vim.api.nvim_set_hl(0, "@lsp.type.parameter.perl", { link = "Special" })
vim.api.nvim_set_hl(0, "@lsp.type.keyword.perl", { link = "Constant" })  -- $self
vim.api.nvim_set_hl(0, "@lsp.type.enumMember.perl", { link = "Constant" })
vim.api.nvim_set_hl(0, "@lsp.type.regexp.perl", { link = "String" })
vim.api.nvim_set_hl(0, "@lsp.mod.readonly.perl", { italic = true })
vim.api.nvim_set_hl(0, "@lsp.mod.deprecated.perl", { strikethrough = true })
vim.api.nvim_set_hl(0, "@lsp.mod.defaultLibrary.perl", { italic = true })
```

## Files to modify/create

| File | Change |
|------|--------|
| `src/symbols.rs` | Extend `semantic_token_types()`, `semantic_token_modifiers()`, `semantic_tokens_full()` |
| `src/builder.rs` | Emit additional semantic token entries during walk |
| `src/file_analysis.rs` | Add `semantic_tokens: Vec<SemanticToken>` or extend existing storage |
| `test_files/semantic_tokens_playground.pl` | **New.** Visual QA file exercising all token types |
| `test_nvim_init.lua` | Add `@lsp.type.*` highlight links |

## Tests

```rust
#[test]
fn test_semantic_token_self_is_keyword() {
    // sub process { my ($self) = @_; $self->foo }
    // $self tokens should be KEYWORD type
}

#[test]
fn test_semantic_token_parameter() {
    // sub foo { my ($self, $name, $age) = @_; }
    // $name, $age should be PARAMETER type
}

#[test]
fn test_semantic_token_framework_macro() {
    // use Moo; has name => (is => 'ro');
    // 'has' should be MACRO type
}

#[test]
fn test_semantic_token_hash_key_property() {
    // $obj->{name} — 'name' should be PROPERTY type
    // name => 'val' — 'name' should be PROPERTY type
}

#[test]
fn test_semantic_token_imported_function() {
    // use List::Util qw(max); max(1,2,3);
    // 'max' should be FUNCTION + defaultLibrary modifier
}

#[test]
fn test_semantic_token_constant() {
    // use constant FOO => 42; my $x = FOO;
    // FOO should be ENUM_MEMBER + readonly
}

#[test]
fn test_semantic_token_namespace() {
    // use Foo::Bar; Foo::Bar->new();
    // 'Foo::Bar' should be NAMESPACE type
}

#[test]
fn test_semantic_token_unused_deprecated() {
    // my $unused = 1; — never read → VARIABLE + deprecated
}

#[test]
fn test_semantic_token_readonly_accessor() {
    // use Moo; has name => (is => 'ro');
    // $self->name → METHOD + readonly
}

#[test]
fn test_semantic_token_class_method_static() {
    // Foo->new() → METHOD + static
}
```

## Implementation ordering

| Phase | Tokens | Effort |
|-------|--------|--------|
| **1** | `$self`/`$class` as keyword, parameters as parameter | Low — builder already identifies these |
| **2** | Framework DSL as macro (check `framework_imports`) | Low — one set lookup |
| **3** | Hash keys as property (fat-comma + deref) | Medium — need to emit during hash walks |
| **4** | Function vs method distinction, imported modifier | Medium — check ref kinds + import lists |
| **5** | Namespace tokens for package names | Low — already have PackageRef |
| **6** | Constants as enumMember + readonly | Low — from constant_strings |
| **7** | Regex tokens | Low — tree-sitter node kind check |
| **8** | Unused/deprecated, readonly accessor, static class methods | Medium — analysis queries |
| **9** | String interpolation variables (VS Code) | Medium — walk string children |

Phases 1-2 are the biggest visual impact for the least work. The playground file should be created first so every phase can be visually verified.
