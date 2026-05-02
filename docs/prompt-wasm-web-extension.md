# WASM Build + VS Code Web Extension

> Goal: run perl-lsp in vscode.dev (browser). Single-file intelligence
> only — no @INC resolve, no SQLite, no perltidy. Same limitation
> PerlNavigator's web mode has, but our single-file feature set is
> deeper.

## Architecture: core + shell split

```
perl-lsp-core/          — WASM-compatible analysis engine
  builder, file_analysis, witnesses, cursor_context,
  pod, query_cache, cpanfile, file_store, resolve

perl-lsp/               — native LSP server (current binary)
  main, backend, module_resolver, module_cache, module_index

editors/vscode-web/     — web extension
  extension.ts          — VS Code activation
  server.ts             — LSP server in web worker
  pkg/perl_lsp_core_bg.wasm
```

Cargo workspace; `perl-lsp` depends on `perl-lsp-core`.

`symbols.rs` splits: pure logic moves into `perl-lsp-core` returning
framework-agnostic types; the `tower-lsp` adapter stays in `perl-lsp`.
This mirrors the existing "thin adapter" architecture rule but makes the
boundary a crate boundary.

## What works in the browser

Full single-file intelligence — every LSP feature that runs over one
`FileAnalysis`: completion, hover, goto-def, find-refs, rename, semantic
tokens, document symbols, signature help, inlay hints, document
highlight, selection range, folding, single-file diagnostics.

## What doesn't work (at least without VS Code's virtual FS)

Cross-file module resolution, `@INC` discovery (no `perl` binary),
workspace indexing (no FS), SQLite cache, perltidy formatting, cross-
file rename / references.

Stretch: VS Code provides a virtual filesystem API (`vscode.workspace.fs`).
The web extension could read workspace files, build an in-memory module
index. Lifts cross-file completion + goto-def in the browser.

## tree-sitter in WASM

Compile everything to WASM via Rust — `tree-sitter` crate compiled with
`wasm32-unknown-unknown`, grammar `.c` files compile alongside via
emscripten or wasi-sdk. Cleaner than the JS-side `web-tree-sitter` route
(no JS↔WASM serialization boundary).

## Phase order

1. **Cargo workspace split.** Move pure-computation modules to
   `perl-lsp-core`. No behavior change. This is the real work — once it's
   done, WASM compilation is mostly mechanical.
2. **`#[wasm_bindgen]` exports** on `perl-lsp-core` behind a `wasm` feature.
3. **VS Code web extension.** TypeScript wrapper using
   `vscode-languageserver/browser`. Well-documented pattern.
4. **CI for WASM build + extension packaging.**
5. **Cross-file via VS Code virtual FS.** Stretch.

## Size budget

| Component | Estimated WASM size |
|---|---|
| tree-sitter runtime | ~200KB |
| tree-sitter-perl grammar | ~150KB |
| tree-sitter-pod grammar | ~50KB |
| `perl-lsp-core` logic | ~300KB |
| wasm-bindgen glue | ~50KB |
| **Total (gzipped)** | **~250KB** |

PerlNavigator's web extension is ~2MB → we'd be ~8× smaller.
