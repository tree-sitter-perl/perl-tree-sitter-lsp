# perl-lsp

A Perl LSP server built on tree-sitter-perl and tower-lsp.

## Build

```
cargo build          # debug
cargo build --release  # optimized
```

Requires `tree-sitter-perl` as a sibling directory (`../tree-sitter-perl`).
If `src/parser.c` is missing from tree-sitter-perl, generate it:
```
cd ../tree-sitter-perl && tree-sitter generate
```

## Architecture

- `src/main.rs` — Entry point, stdio transport, `--parse-exports` subprocess mode
- `src/backend.rs` — `LanguageServer` trait implementation (tower-lsp), request routing
- `src/document.rs` — Document store with tree-sitter parsing
- `src/file_analysis.rs` — Data model: scopes, symbols, refs, imports, type inference, priority constants
- `src/builder.rs` — Single-pass CST → FileAnalysis builder
- `src/cursor_context.rs` — Cursor position analysis: completion/signature/selection context
- `src/symbols.rs` — LSP adapter layer (converts FileAnalysis types to LSP types)
- `src/module_index.rs` — Cross-file: public API, reverse index (`func → modules`), concurrent cache
- `src/module_resolver.rs` — Background resolver thread, subprocess isolation, export extraction
- `src/module_cache.rs` — SQLite persistence, schema migrations, mtime validation
- `src/cpanfile.rs` — cpanfile parsing via tree-sitter queries

## Key Dependencies

- `tower-lsp 0.20` — LSP framework (uses `#[tower_lsp::async_trait]`)
- `tree-sitter 0.25` — Parsing
- `tree-sitter-perl` — Path dep to `../tree-sitter-perl`, exports `LANGUAGE: LanguageFn`
- `dashmap 6` — Concurrent document store + module cache
- `rusqlite 0.32` — SQLite persistence for module index (bundled)

## tree-sitter-perl Node Types

Key nodes and their fields:
- `subroutine_declaration_statement` — fields: `name` (bareword), `body` (block), `lexical` ("my")
- `method_declaration_statement` — same fields as above
- `variable_declaration` — fields: `variable` (scalar/array/hash), `variables` (paren list)
- `package_statement` — field: `name` (package)
- `class_statement` — field: `name` (package)
- `use_statement` — field: `module` (package)
- `function_call_expression` — field: `function`, `arguments`
- `ambiguous_function_call_expression` — field: `function`, `arguments` (no-paren calls)
- `method_call_expression` — fields: `invocant`, `method`
- `scalar` = "$" + varname, `array` = "@" + varname, `hash` = "%" + varname

## Testing

```
cargo test           # run all tests
```

## Cross-file Module Resolution

- `ModuleIndex` uses a dedicated `std::thread` for filesystem I/O (never blocks tokio)
- `Arc<DashMap>` shared between resolver thread and async LSP handlers
- Reverse index: `DashMap<func_name, Vec<module_name>>` for O(1) exporter lookup
- Export extraction uses tree-sitter in isolated subprocesses (5s timeout + SIGKILL)
- cpanfile parsed with tree-sitter queries at startup, deps pre-resolved with progress reporting
- SQLite cache per project (`~/.cache/perl-lsp/<hash>/modules.db`)
- Async handlers only use `_cached` methods — zero I/O
- After resolution, diagnostics are refreshed for all open files (clears stale false positives)

## LSP Capabilities

- `textDocument/documentSymbol` — outline of subs, packages, variables, classes (with fields/methods as children)
- `textDocument/definition` — go-to-def for variables (scope-aware), subs, methods (type-inferred), packages/classes, hash keys; resolves through expression chains
- `textDocument/references` — scope-aware for variables, file-wide for functions/packages/hash keys; resolves through expression chains
- `textDocument/hover` — shows declaration line, inferred types, return types, class-aware for methods
- `textDocument/rename` — scope-aware for variables, file-wide for functions/packages/hash keys
- `textDocument/completion` — scope-aware variables (cross-sigil forms), subs, methods (type-inferred with return type detail), packages, hash keys, auto-import from cached modules, deref snippets for typed references
- `textDocument/signatureHelp` — parameter info with inferred types for subs/methods (signature syntax + legacy @_ pattern), triggers on `(` and `,`
- `textDocument/inlayHint` — type annotations for variables (Object/HashRef/ArrayRef/CodeRef) and sub return types
- `textDocument/documentHighlight` — highlight all occurrences with read/write distinction
- `textDocument/selectionRange` — expand/shrink selection via tree-sitter node hierarchy
- `textDocument/foldingRange` — blocks, subs, classes, pod sections
- `textDocument/formatting` — shells out to perltidy (respects .perltidyrc)
- `textDocument/semanticTokens/full` — variable tokens with modifiers: scalar/array/hash, declaration, modification
- `textDocument/codeAction` — auto-import for unresolved functions
- Diagnostics — unresolved function/method warnings (skips builtins, local subs, imported functions)
