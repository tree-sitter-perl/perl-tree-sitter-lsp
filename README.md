# perl-tree-sitter-lsp

A Language Server Protocol (LSP) implementation for Perl, built on tree-sitter for parsing and scope graphs for name resolution.

## Motivation

Perl has no good LSP. The existing options (Perl::LanguageServer, PLS) are slow, incomplete, and struggle with Perl's dynamic nature. This project takes a different approach: use tree-sitter for fast, incremental, error-tolerant parsing, and scope graphs for principled name resolution — rather than trying to bolt IDE features onto the Perl interpreter.

## Architecture

```
┌─────────────┐     ┌──────────────────┐     ┌───────────┐
│  Editor      │◄───►│  LSP Server       │◄───►│  Scope    │
│  (any LSP    │     │  (tower-lsp)      │     │  Graph    │
│   client)    │     │                    │     │  Engine   │
└─────────────┘     └────────┬───────────┘     └─────┬─────┘
                             │                       │
                     ┌───────▼───────┐       ┌───────▼───────┐
                     │  tree-sitter   │       │  scopegraphs  │
                     │  (perl grammar)│       │  (rust crate) │
                     └───────────────┘       └───────────────┘
```

### Components

1. **[tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl)** — Our tree-sitter grammar for Perl. Produces a concrete syntax tree (CST) that the rest of the system works from. Already handles much of Perl's parsing oddities (context-sensitive tokenization, regex vs division ambiguity, etc.).

2. **Scope graph builder** — Walks the tree-sitter CST and builds a scope graph using the `scopegraphs` Rust crate ([metaborg/rust-scopegraphs](https://github.com/metaborg/rust-scopegraphs)). This is where Perl's scoping rules get encoded.

3. **LSP server** — A Rust binary using `tower-lsp` that exposes standard LSP capabilities backed by the scope graph. Handles document synchronization, incremental re-parsing, and translates LSP requests into scope graph queries.

## Key Dependencies

| Crate | Purpose |
|---|---|
| `tree-sitter` | Incremental parsing framework |
| `tree-sitter-perl` | Perl grammar ([tree-sitter-perl/tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl)) |
| `scopegraphs` | Scope graph construction and querying ([docs](https://docs.rs/scopegraphs/latest/scopegraphs/)) |
| `tower-lsp` | LSP server framework |

## Background: Why Scope Graphs

[Scope graphs](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/) are a language-agnostic formalism for modeling name resolution. They encode which names are defined in which scopes and how scopes relate to each other (nesting, imports, inheritance). You build the graph, then query it to resolve references to definitions.

GitHub's [stack-graphs](https://github.com/github/stack-graphs) (now archived, Sept 2025) was an adaptation of this theory. It used declarative `.tsg` files to map tree-sitter nodes to graph operations, but maintaining those files proved unsustainable — they only ever covered 4 languages.

The `scopegraphs` crate from Metaborg is the canonical Rust implementation of the underlying theory. Instead of declarative `.tsg` rules, you write Rust code that walks the CST and imperatively builds the scope graph. This is more work upfront but far more flexible — which matters for Perl.

Key properties of scope graphs:
- **Incremental**: graph construction and querying can happen concurrently
- **Language-agnostic**: the framework doesn't care about the language, you encode the rules
- **Principled**: based on the formal theory from ["Scopes as Types" (van Antwerpen et al., 2018)](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/)

## Perl Scoping Challenges

Perl's scoping is notoriously complex. The scope graph builder must handle:

### Lexical scoping (`my`, `state`)
- `my $x` introduces a lexical variable visible from declaration to end of enclosing block
- `state $x` is lexical but persistent across calls
- Lexical variables shadow outer scopes

### Dynamic scoping (`local`)
- `local $x` temporarily replaces the value of a package variable for the dynamic extent of the enclosing block
- This is runtime behavior — the scope graph can model the declaration site but not the dynamic resolution

### Package variables and namespaces
- `$Foo::bar`, `@Foo::baz` — fully qualified package variables
- `package Foo;` switches the current namespace
- `our $x` declares a lexical alias to a package variable

### Imports and exports
- `use Module qw(func1 func2)` — imports names into the current namespace
- `use Module;` — calls `Module->import()`, which can inject arbitrary names
- Exporter-based patterns (`@EXPORT`, `@EXPORT_OK`)
- `use parent`, `use base` — inheritance, affects method resolution

### Subroutines
- `sub foo { }` — named subroutine in current package
- `my $f = sub { }` — anonymous sub, lexical variable
- `sub foo :prototype($$) { }` — prototypes affect parsing of call sites
- Method resolution via `@ISA` / C3 linearization (`use mro 'c3'`)

### Special variables
- `$_`, `@_`, `$!`, `$/`, etc. — implicitly scoped, some dynamically scoped
- `$self` — convention, not a keyword

### Closures
- Subroutines close over lexical variables from enclosing scopes
- This is critical for go-to-definition to work across closure boundaries

## Building

### Prerequisites

- Rust toolchain (install via [rustup](https://rustup.rs/))
- [tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl) cloned as a sibling directory

### Setup

```bash
# Clone tree-sitter-perl next to this repo (if not already)
git clone https://github.com/tree-sitter-perl/tree-sitter-perl ../tree-sitter-perl

# Generate the parser (requires tree-sitter CLI)
cargo install tree-sitter-cli
cd ../tree-sitter-perl && tree-sitter generate && cd -

# Build
cargo build --release
```

The binary is at `target/release/perl-lsp`.

## Editor Setup

### Neovim (0.11+)

Add to your Neovim config (e.g. `~/.config/nvim/init.lua`):

```lua
vim.lsp.config["perl-lsp"] = {
  cmd = { "/absolute/path/to/perl-lsp" },
  filetypes = { "perl" },
  root_markers = { ".git", "Makefile", "cpanfile", "Makefile.PL", "Build.PL" },
}
vim.lsp.enable("perl-lsp")
```

Or test with the included throwaway config:

```bash
nvim -u test_nvim_init.lua test_files/sample.pl
```

**Keybindings** (with the test config or standard Neovim LSP defaults):

| Key | Action |
|---|---|
| `gd` | Go to definition |
| `gr` | Find references |
| `K` | Hover info |

### VS Code

1. Install the **[Generic LSP Client](https://marketplace.visualstudio.com/items?itemName=nicolo-ribaudo.vscode-glsp-client)** extension (or any generic LSP extension).

2. Add to your VS Code settings (`.vscode/settings.json`):

```json
{
  "glspClient.serverConfigs": [
    {
      "id": "perl-lsp",
      "name": "Perl LSP",
      "command": "${workspaceFolder}/target/release/perl-lsp",
      "languages": ["perl"]
    }
  ]
}
```

Alternatively, a `.vscode/settings.json` for this repo is included — just install the extension and open the workspace.

## LSP Features (Priority Order)

### Phase 1 — Core navigation
- [x] `textDocument/definition` — go to definition of variables, subs, packages
- [x] `textDocument/references` — find all references to a symbol
- [x] `textDocument/hover` — show type/scope info for a symbol
- [x] `textDocument/documentSymbol` — outline of subs, packages, variables in a file

### Phase 2 — Cross-file intelligence
- [ ] `textDocument/completion` — autocomplete variables, subs, package names
- [ ] Workspace-wide indexing — resolve `use` statements, track cross-file definitions
- [ ] `textDocument/rename` — rename a symbol across its scope

### Phase 3 — Diagnostics and advanced features
- [ ] `textDocument/diagnostic` — report undeclared variables, unused imports
- [ ] `textDocument/signatureHelp` — sub signatures and prototypes
- [ ] Method resolution across inheritance hierarchies

## Development Plan

1. **tree-sitter-perl is ready** — the grammar lives at [tree-sitter-perl/tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl) and is maintained in-house. Grammar improvements needed for the LSP can be done upstream.
2. **Prototype the scope graph builder** — start with lexical scoping (`my`, `our`, `local`, `sub`). Get go-to-definition working for variables within a single file.
3. **Wire up the LSP** — minimal `tower-lsp` server that does document sync + go-to-definition.
4. **Expand incrementally** — add cross-file resolution, completion, references, diagnostics.

## References

- [scopegraphs crate](https://docs.rs/scopegraphs/latest/scopegraphs/)
- [metaborg/rust-scopegraphs](https://github.com/metaborg/rust-scopegraphs)
- [Scope Graphs: A Theory of Name Resolution](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/)
- [github/stack-graphs (archived)](https://github.com/github/stack-graphs) — prior art, useful for understanding the approach
- [tower-lsp](https://github.com/ebkalderon/tower-lsp)
- [tree-sitter-perl/tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl)
- [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
