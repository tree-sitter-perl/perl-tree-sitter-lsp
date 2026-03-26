# perl-lsp

A Perl language server with deep semantic intelligence. Built on [tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl) and tower-lsp.

## Install

```bash
cargo install perl-lsp
```

## Editor Setup

### Neovim (0.11+)

```lua
vim.lsp.config["perl-lsp"] = {
  cmd = { "perl-lsp" },
  filetypes = { "perl" },
  root_markers = { "cpanfile", "Makefile.PL", "Build.PL", ".git" },
}
vim.lsp.enable("perl-lsp")
```

### Neovim (0.10 or earlier)

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "perl",
  callback = function()
    vim.lsp.start({
      name = "perl-lsp",
      cmd = { "perl-lsp" },
      root_dir = vim.fs.root(0, { "cpanfile", "Makefile.PL", "Build.PL", ".git" }),
    })
  end,
})
```

### Helix

Add to `~/.config/helix/languages.toml`:

```toml
[language-server.perl-lsp]
command = "perl-lsp"

[[language]]
name = "perl"
language-servers = ["perl-lsp"]
```

### Emacs (eglot)

```elisp
(add-to-list 'eglot-server-programs '(perl-mode . ("perl-lsp")))
```

### Semantic Token Colors (Neovim)

perl-lsp emits rich semantic tokens. Add these to your config for the best experience:

```lua
vim.api.nvim_set_hl(0, "@lsp.type.macro.perl", { link = "Keyword" })      -- has, with, extends
vim.api.nvim_set_hl(0, "@lsp.type.property.perl", { link = "Identifier" }) -- hash keys
vim.api.nvim_set_hl(0, "@lsp.type.namespace.perl", { link = "Type" })     -- Foo::Bar
vim.api.nvim_set_hl(0, "@lsp.type.parameter.perl", { link = "Special" })  -- sub params
vim.api.nvim_set_hl(0, "@lsp.type.keyword.perl", { link = "Constant" })   -- $self/$class
vim.api.nvim_set_hl(0, "@lsp.mod.scalar.perl", { fg = "#61afef" })        -- $ blue
vim.api.nvim_set_hl(0, "@lsp.mod.array.perl", { fg = "#c678dd" })         -- @ purple
vim.api.nvim_set_hl(0, "@lsp.mod.hash.perl", { fg = "#e5c07b" })          -- % gold
vim.api.nvim_set_hl(0, "@lsp.mod.modification.perl", { fg = "#e06c75" })  -- writes in red
vim.api.nvim_set_hl(0, "@lsp.mod.declaration.perl", { bold = true })
vim.api.nvim_set_hl(0, "@lsp.mod.readonly.perl", { italic = true })
vim.api.nvim_set_hl(0, "@lsp.mod.defaultLibrary.perl", { italic = true }) -- imported functions
```

## Features

### Type Inference

No annotations needed. perl-lsp infers types from how your code uses values:

- `Foo->new()` → `$obj` is `ClassName(Foo)`
- `$obj->{key}` → `$obj` is HashRef
- `$x + 1` → `$x` is Numeric
- Method chains propagate: `$self->get_config()->{host}` resolves through return types
- Cross-file: return types and parameter types flow across module boundaries

### Framework Intelligence

| Framework | What perl-lsp understands |
|-----------|--------------------------|
| **Moo/Moose** | `has` accessor synthesis with `is`/`isa` type mapping, getter/setter arity, `extends`/`with` for inheritance and roles |
| **Mojo::Base** | Accessor synthesis with default value type inference, fluent setter chaining, parent class detection |
| **DBIC** | `add_columns` column accessors, `has_many`/`belongs_to`/`has_one` relationship accessors, `load_components` mixin resolution |
| **Perl 5.38 `class`** | `field` with `:param`/`:reader`/`:writer`, `:isa(Parent)`, `:does(Role)`, implicit `$self` |

Framework DSL keywords (`has`, `with`, `extends`, `around`, etc.) are recognized and won't trigger unresolved-function diagnostics.

### Constant Folding + Dynamic Dispatch

```perl
my $method = "get_$field";
$self->$method();          # goto-def works — resolves through the constant
```

perl-lsp folds `use constant`, package-scope variables, string interpolation, and loop variables. Dynamic method calls via `$self->$var()` resolve to their targets.

### Cross-File Intelligence

- Module resolution from `@INC` + cpanfile dependencies
- Inheritance chain walking (DFS, roles, mixins, `load_components`)
- Return type and parameter type propagation across files
- Cross-file rename: 289 edits across 56 files in Mojolicious in <1 second
- SQLite cache for instant repeat resolution
- Workspace indexing via Rayon: 274-file Mojolicious in 204ms

### LSP Capabilities

| Capability | Highlights |
|-----------|-----------|
| **Completion** | Variables, methods (type-inferred), hash keys, auto-import, module names on `use` lines, import lists in `qw()` |
| **Go-to-definition** | Scope-aware variables, cross-file methods via inheritance, hash keys through expression chains |
| **Find references** | Scope-aware variables, cross-file functions/methods/packages |
| **Rename** | Variables (scope-aware), functions/methods/packages (cross-file via workspace index) |
| **Hover** | Types, POD docs (tree-sitter-pod AST), signatures, class provenance |
| **Signature help** | Parameter names with inferred types, cross-file parameter types |
| **Semantic tokens** | 10 types (variable, parameter, `$self`, function, method, macro, property, namespace, regexp, constant), 9 modifiers |
| **Inlay hints** | Variable type annotations, sub return types |
| **Diagnostics** | Unresolved function/method warnings with framework awareness |
| **Code actions** | Auto-import for unresolved functions |
| **Workspace symbol** | Search across all indexed project files |
| **Document symbols** | Nested outline with packages, subs, classes, fields |
| **Formatting** | perltidy (full document + range) |
| **Highlights** | Read/write distinction |
| **Selection range** | Tree-sitter node hierarchy |
| **Folding** | Blocks, subs, classes, POD |
| **Linked editing** | Simultaneous editing of references in scope |

### POD Documentation

POD is rendered via tree-sitter-pod — proper AST walk, not regex. Handles nested lists, `=begin`/`=end` data regions, multi-angle-bracket formatting (`C<<<  $hash->{key}  >>>`), bold-italic nesting, `L<>` links to metacpan, and `=item`-based method documentation.

## CLI Tools

perl-lsp doubles as a command-line analysis toolkit:

```bash
# Batch diagnostics (CI-ready — resolves imports, uses SQLite cache)
perl-lsp --check [<root>] [--severity error|warning] [--format json|human]

# Code exploration
perl-lsp --outline <file>
perl-lsp --hover <file> <line> <col>
perl-lsp --type-at <file> <line> <col>
perl-lsp --definition <root> <file> <line> <col>
perl-lsp --references <root> <file> <line> <col>

# Refactoring
perl-lsp --rename <root> <file> <line> <col> <new_name>
perl-lsp --workspace-symbol <root> <query>
```

`--check` resolves modules from `@INC`, uses the per-project SQLite cache, and exits with code 1 if issues are found. Integrate into CI with:

```bash
perl-lsp --check . --severity warning
```

## Building from Source

```bash
git clone https://github.com/tree-sitter-perl/perl-tree-sitter-lsp
cd perl-tree-sitter-lsp
cargo build --release
```

## Testing

```bash
cargo test                                    # 317 unit tests
cargo build --release && ./run_e2e.sh         # 93 e2e tests (requires nvim)
```

## License

Artistic License 2.0 — same as Perl itself.
