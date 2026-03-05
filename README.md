# perl-lsp

A Language Server Protocol (LSP) implementation for Perl, built on [tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl) for fast, incremental, error-tolerant parsing.

## Motivation

Perl has no good LSP. The existing options (Perl::LanguageServer, PLS) are slow, incomplete, and struggle with Perl's dynamic nature. This project takes a different approach: use tree-sitter for parsing and a hand-built scope graph for name resolution — rather than trying to bolt IDE features onto the Perl interpreter.

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

## LSP Features

### Single-file

- **documentSymbol** — outline of subs, packages, variables, classes (with fields/methods as children)
- **definition** — go-to-def for variables (scope-aware), subs, methods (type-inferred), packages/classes, hash keys
- **references** — scope-aware for variables, file-wide for functions/packages/hash keys
- **hover** — shows declaration line, class-aware for methods, hash key info
- **rename** — scope-aware for variables, file-wide for functions/packages/hash keys
- **completion** — scope-aware variables (cross-sigil forms), subs, methods (type-inferred), packages, hash keys (class-aware)
- **signatureHelp** — parameter info for subs/methods (signature syntax + legacy `@_` pattern), triggers on `(` and `,`
- **documentHighlight** — highlight all occurrences with read/write distinction
- **selectionRange** — expand/shrink selection via tree-sitter node hierarchy
- **foldingRange** — blocks, subs, classes, pod sections
- **formatting** — shells out to perltidy (respects .perltidyrc)
- **semanticTokens/full** — variable tokens with modifiers: scalar/array/hash, declaration, modification
- **codeAction** — auto-import: adds `use Module qw(func);` for unresolved functions

### Cross-file

- **Module resolution** — resolves `@EXPORT` / `@EXPORT_OK` from imported modules via `@INC`
- **cpanfile pre-scan** — indexes project dependencies at startup with progress reporting
- **Auto-import completions** — suggests exported functions from cached modules with `additionalTextEdits`
- **Diagnostics** — warns on unresolved function calls (skips builtins, local subs, imported functions)
- **SQLite cache** — per-project persistent cache, survives restarts, validates against `@INC` changes

## Architecture

```
src/
├── main.rs            Entry point, stdio transport, --parse-exports subprocess mode
├── backend.rs         LanguageServer trait impl (tower-lsp), request routing
├── document.rs        Document store with tree-sitter parsing
├── analysis.rs        Legacy analysis (being migrated to builder/file_analysis)
├── file_analysis.rs   Data model: scopes, symbols, refs, imports, type inference
├── builder.rs         Single-pass CST → FileAnalysis builder
├── cursor_context.rs  Cursor position analysis: completion/signature/selection context
├── symbols.rs         LSP adapter: converts FileAnalysis types to LSP types
└── module_index.rs    Cross-file: module resolution, SQLite cache, cpanfile parsing
```

### Key dependencies

| Crate | Purpose |
|---|---|
| `tower-lsp 0.20` | LSP framework (`#[tower_lsp::async_trait]`) |
| `tree-sitter 0.25` | Incremental parsing |
| `tree-sitter-perl` | Perl grammar (path dep to `../tree-sitter-perl`) |
| `dashmap 6` | Concurrent document store + module cache |
| `rusqlite 0.32` | SQLite persistence for module index (bundled) |

### How module resolution works

1. When a file is opened/edited, `use` statements trigger background module resolution
2. A dedicated `std::thread` (not tokio) does all filesystem I/O — never blocks the async runtime
3. Modules are located via `@INC`, exports extracted via string matching (not tree-sitter — avoids memory blowup)
4. Results stored in `Arc<DashMap>` (async handlers read) + SQLite (persists across restarts)
5. At startup, `cpanfile` is parsed with tree-sitter queries to pre-resolve project dependencies

## Debugging

The LSP server uses `env_logger`. Debug mode is opt-in via `PERL_LSP_DEBUG`:

```bash
# Normal usage — no logging
nvim --clean -u test_nvim_init.lua test_files/sample.pl

# Debug mode — full logging to /tmp/perl-lsp.log
PERL_LSP_DEBUG=1 nvim --clean -u test_nvim_init.lua test_files/sample.pl
```

Then tail the log: `tail -f /tmp/perl-lsp.log`

| Level | What |
|---|---|
| `info` | Module resolution: queued, resolved, export counts; cpanfile parsing |
| `warn` | Slow parses (>100ms), subprocess timeouts, skipped files |
| `debug` | Every `did_change` dumps document text to `/tmp/perl-lsp-last-update.pl` |

### Reproducing parser hangs

tree-sitter-perl's external scanner can occasionally enter an infinite loop. Module resolution runs in isolated child processes with a 5-second timeout — the child is SIGKILL'd and the module skipped. The main LSP process stays alive.

The file `/tmp/perl-lsp-last-update.pl` contains the last document text sent to the parser, useful for reproducing hangs.

## Roadmap

### Done
- Single-file: all core LSP features (definition, references, hover, rename, completion, signature help, highlights, selection range, folding, formatting, semantic tokens)
- Scope-aware variable resolution (my, our, state, signatures, for-loops, class fields)
- Core Perl `class` support (5.38+): class extraction, field/method symbols, type-inferred method resolution
- Hash key intelligence: go-to-def, refs, rename, hover, completion (class-aware via bless patterns)
- Cross-file module resolution with background resolver thread
- cpanfile pre-scan with tree-sitter queries and progress reporting
- Auto-import completions and code actions
- SQLite persistence with per-project cache segregation

### Next
- Phase-aware cpanfile filtering (test deps only in `t/` files) — [design doc](docs/cpanfile-indexing-plan.md)
- Framework submodule whitelist (`Mojo::*`, `Moose::*` patterns)
- OOP framework stubs (Moo, Moose, Class::Accessor) — declarative DSL → class shape mapping
- Bulk `@INC` scan for go-to-def into any installed module

### Future
- Workspace-wide references and rename
- Return type tracking and parameter type inference
- Method resolution across inheritance hierarchies (`@ISA` / C3)

## References

- [tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl)
- [tower-lsp](https://github.com/ebkalderon/tower-lsp)
- [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
- [Scope Graphs: A Theory of Name Resolution](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/) — influenced our approach to name resolution
