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

- `src/main.rs` — Entry point, stdio transport
- `src/backend.rs` — `LanguageServer` trait implementation (tower-lsp)
- `src/document.rs` — Document store with tree-sitter parsing
- `src/analysis.rs` — Core analysis: symbols, definitions, references, highlights, completion, folding
- `src/symbols.rs` — LSP adapter layer (converts analysis types to LSP types)

## Key Dependencies

- `tower-lsp 0.20` — LSP framework (uses `#[tower_lsp::async_trait]`)
- `tree-sitter 0.25` — Parsing
- `tree-sitter-perl` — Path dep to `../tree-sitter-perl`, exports `LANGUAGE: LanguageFn`
- `dashmap 6` — Concurrent document store

## tree-sitter-perl Node Types

Key nodes and their fields:
- `subroutine_declaration_statement` — fields: `name` (bareword), `body` (block), `lexical` ("my")
- `method_declaration_statement` — same fields as above
- `variable_declaration` — fields: `variable` (scalar/array/hash), `variables` (paren list)
- `package_statement` — field: `name` (package)
- `class_statement` — field: `name` (package)
- `use_statement` — field: `module` (package)
- `function_call_expression` — field: `function`
- `method_call_expression` — fields: `invocant`, `method`
- `scalar` = "$" + varname, `array` = "@" + varname, `hash` = "%" + varname

## Testing

```
python3 /tmp/test_lsp2.py ./target/debug/perl-lsp ./test_files/sample.pl
```

## LSP Capabilities (current)

- `textDocument/documentSymbol` — outline of subs, packages, variables, classes (with fields/methods as children)
- `textDocument/definition` — go-to-def for variables (scope-aware), subs, methods (type-inferred), packages/classes
- `textDocument/references` — scope-aware for variables, file-wide for functions/packages
- `textDocument/hover` — shows declaration line, class-aware for methods
- `textDocument/rename` — scope-aware for variables, file-wide for functions/packages
- `textDocument/completion` — scope-aware variables (cross-sigil forms), subs, methods (type-inferred), packages
- `textDocument/documentHighlight` — highlight all occurrences with read/write distinction
- `textDocument/selectionRange` — expand/shrink selection via tree-sitter node hierarchy
- `textDocument/foldingRange` — blocks, subs, classes, pod sections
- `textDocument/formatting` — shells out to perltidy (respects .perltidyrc)
- `textDocument/semanticTokens/full` — variable tokens with modifiers: scalar/array/hash, declaration, modification
- Diagnostics — infrastructure in place, readonly write detection deferred to framework stubs
