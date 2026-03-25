# WASM Build + VS Code Web Extension

## Goal

Run perl-lsp in vscode.dev (browser). PerlNavigator already does this — we should too.

## The Challenge

The browser has no filesystem, no subprocesses, no threads, no SQLite. Our codebase has:

| Component | WASM-compatible? | Why |
|-----------|-----------------|-----|
| `builder.rs` | Yes | Pure computation over tree-sitter nodes |
| `file_analysis.rs` | Yes | Pure data structures + queries |
| `cursor_context.rs` | Yes | Pure computation |
| `pod.rs` | Yes | tree-sitter-pod walk, pure computation |
| `symbols.rs` | Partially | Logic is pure, but imports tower-lsp types |
| `query_cache.rs` | Yes | OnceLock + tree-sitter queries |
| `module_resolver.rs` | No | Filesystem I/O, @INC discovery, Rayon |
| `module_cache.rs` | No | rusqlite (C FFI) |
| `module_index.rs` | No | DashMap + thread spawning |
| `backend.rs` | No | tower-lsp, tokio |
| `cpanfile.rs` | Yes | Pure tree-sitter queries |

**The analysis engine is WASM-compatible. The I/O and LSP layers aren't.**

## Architecture: Core + Shell

Split the crate into two:

```
perl-lsp-core/          ← WASM-compatible analysis engine
  src/
    lib.rs              ← Public API: parse, analyze, complete, hover, etc.
    builder.rs          ← Same as today
    file_analysis.rs    ← Same as today
    cursor_context.rs   ← Same as today
    pod.rs              ← Same as today
    query_cache.rs      ← Same as today
    cpanfile.rs         ← Same as today

perl-lsp/               ← Native LSP server (current binary)
  src/
    main.rs             ← CLI + LSP server
    backend.rs          ← tower-lsp, workspace indexing
    module_resolver.rs  ← @INC, Rayon, filesystem
    module_cache.rs     ← SQLite
    module_index.rs     ← DashMap, threading

editors/vscode-web/     ← VS Code web extension
  src/
    extension.ts        ← VS Code web extension entry point
    server.ts           ← LSP server running in web worker
    wasm-bridge.ts      ← Calls into perl-lsp-core WASM
  pkg/
    perl_lsp_core_bg.wasm  ← Compiled WASM from perl-lsp-core
```

### perl-lsp-core API

```rust
// perl-lsp-core/src/lib.rs
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct AnalysisResult {
    // Serialized FileAnalysis or specific query results
}

#[wasm_bindgen]
pub fn parse_and_analyze(source: &str) -> AnalysisResult {
    let mut parser = Parser::new();
    parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
    let tree = parser.parse(source, None).unwrap();
    let analysis = builder::build(&tree, source.as_bytes());
    // Serialize to a WASM-friendly format
    AnalysisResult { /* ... */ }
}

#[wasm_bindgen]
pub fn complete(source: &str, line: u32, col: u32) -> JsValue {
    let (tree, analysis) = parse(source);
    let items = symbols::completion_items(&analysis, &tree, source, ...);
    serde_wasm_bindgen::to_value(&items).unwrap()
}

#[wasm_bindgen]
pub fn hover(source: &str, line: u32, col: u32) -> Option<String> {
    let (tree, analysis) = parse(source);
    analysis.hover_info(Point::new(line as usize, col as usize), source, Some(&tree), None)
}

#[wasm_bindgen]
pub fn diagnostics(source: &str) -> JsValue {
    let (tree, analysis) = parse(source);
    // Run diagnostics without module index (single-file mode)
    let diags = collect_diagnostics_standalone(&analysis);
    serde_wasm_bindgen::to_value(&diags).unwrap()
}

// ... goto_definition, find_references, rename, semantic_tokens, etc.
```

### VS Code web extension

```typescript
// editors/vscode-web/src/extension.ts
import { ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
} from 'vscode-languageclient/browser';

export function activate(context: ExtensionContext) {
  const serverMain = context.asAbsolutePath('dist/server.js');
  const worker = new Worker(serverMain);

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'perl' }],
  };

  const client = new LanguageClient('perl-lsp', 'Perl LSP', clientOptions, worker);
  client.start();
}
```

```typescript
// editors/vscode-web/src/server.ts
import {
  createConnection,
  BrowserMessageReader,
  BrowserMessageWriter,
} from 'vscode-languageserver/browser';
import init, { parse_and_analyze, complete, hover, diagnostics } from '../pkg/perl_lsp_core';

const reader = new BrowserMessageReader(self);
const writer = new BrowserMessageWriter(self);
const connection = createConnection(reader, writer);

// Initialize WASM
let wasmReady = false;
init().then(() => { wasmReady = true; });

connection.onInitialize(() => ({
  capabilities: {
    completionProvider: { triggerCharacters: ['$', '@', '%', '>', ':', '{'] },
    hoverProvider: true,
    definitionProvider: true,
    referencesProvider: true,
    // ... same capabilities as native
  }
}));

// Document store (in-memory, since there's no filesystem)
const documents = new Map<string, string>();

connection.onDidOpenTextDocument(params => {
  documents.set(params.textDocument.uri, params.textDocument.text);
  publishDiagnostics(params.textDocument.uri);
});

connection.onDidChangeTextDocument(params => {
  // Full sync
  documents.set(params.textDocument.uri, params.contentChanges[0].text);
  publishDiagnostics(params.textDocument.uri);
});

function publishDiagnostics(uri: string) {
  const source = documents.get(uri);
  if (!source || !wasmReady) return;
  const diags = diagnostics(source);
  connection.sendDiagnostics({ uri, diagnostics: diags });
}

connection.onCompletion(params => {
  const source = documents.get(params.textDocument.uri);
  if (!source || !wasmReady) return [];
  return complete(source, params.position.line, params.position.character);
});

connection.onHover(params => {
  const source = documents.get(params.textDocument.uri);
  if (!source || !wasmReady) return null;
  const md = hover(source, params.position.line, params.position.character);
  return md ? { contents: { kind: 'markdown', value: md } } : null;
});

connection.listen();
```

## What works in the browser

**Full single-file intelligence:**
- Completion (variables, methods, hash keys, framework DSL)
- Hover (types, POD docs, signatures)
- Go-to-definition (within the file)
- Find references (within the file)
- Rename (within the file)
- Semantic tokens (all 10 types, 9 modifiers)
- Document symbols
- Signature help
- Inlay hints
- Document highlight
- Selection range
- Folding ranges
- Diagnostics (unresolved functions — limited without module index)

**What doesn't work in the browser:**
- Cross-file module resolution (no filesystem)
- @INC discovery (no `perl` binary)
- Workspace indexing (no filesystem traversal)
- SQLite cache (no C FFI)
- perltidy formatting (no shell)
- Cross-file rename/references

This is the same limitation PerlNavigator has in web mode — single-file intelligence only. But our single-file intelligence is much deeper (type inference, framework synthesis, constant folding, semantic tokens).

## tree-sitter in WASM

tree-sitter already supports WASM builds. `ts-parser-perl` and `ts-parser-pod` would need to be compiled to WASM. tree-sitter's `web-tree-sitter` package provides the browser runtime.

Two approaches:

**A: Use web-tree-sitter (JavaScript API):**
The WASM core would receive a pre-parsed tree from JavaScript. The JS layer uses web-tree-sitter to parse, then passes the tree to Rust WASM. This avoids compiling tree-sitter's C runtime to WASM inside Rust.

**B: Compile tree-sitter fully to WASM via Rust:**
Use `tree-sitter` crate compiled with `wasm32-unknown-unknown` target. tree-sitter's C code compiles to WASM via Emscripten or wasi-sdk. The grammar `.c` files compile alongside.

Option B is cleaner (pure Rust → WASM, no JS tree-sitter dependency) but requires the C compilation toolchain for WASM. Option A is easier to set up but adds a serialization boundary between JS and WASM.

**Recommended: Option B** — compile everything to WASM. The `tree-sitter` crate has been compiled to WASM by other projects (tree-sitter playground, Zed's WASM extensions). Use `wasm-pack` with the `wasm32-unknown-unknown` target.

## Build pipeline

```bash
# Compile perl-lsp-core to WASM
cd perl-lsp-core
wasm-pack build --target web --out-dir ../editors/vscode-web/pkg

# Build the VS Code web extension
cd ../editors/vscode-web
npm install
npm run build   # bundles server.ts + WASM into dist/

# Package
npx @vscode/vsce package --target web
```

The `--target web` flag for vsce produces a `.vsix` that VS Code recognizes as a web extension.

## Cargo workspace

Restructure as a Cargo workspace:

```toml
# Cargo.toml (workspace root)
[workspace]
members = ["perl-lsp-core", "perl-lsp"]

# perl-lsp-core/Cargo.toml
[package]
name = "perl-lsp-core"

[lib]
crate-type = ["cdylib", "rlib"]  # cdylib for WASM, rlib for native

[dependencies]
tree-sitter = "0.25"
ts-parser-perl = "1"
ts-parser-pod = "1"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
wasm-bindgen = { version = "0.2", optional = true }
serde-wasm-bindgen = { version = "0.6", optional = true }

[features]
default = []
wasm = ["wasm-bindgen", "serde-wasm-bindgen"]

# perl-lsp/Cargo.toml
[package]
name = "perl-lsp"

[[bin]]
name = "perl-lsp"
path = "src/main.rs"

[dependencies]
perl-lsp-core = { path = "../perl-lsp-core" }
tower-lsp = "0.20"
tokio = { version = "1", features = ["full"] }
dashmap = "6"
rusqlite = { version = "0.32", features = ["bundled"] }
rayon = "1"
ignore = "0.4"
log = "0.4"
env_logger = "0.11"
```

## Refactoring plan

### Phase 1: Extract perl-lsp-core (no behavior change)

Move pure-computation modules to `perl-lsp-core/`:
- `builder.rs`
- `file_analysis.rs`
- `cursor_context.rs`
- `pod.rs`
- `query_cache.rs`
- `cpanfile.rs`

Keep in `perl-lsp/`:
- `main.rs`
- `backend.rs`
- `module_resolver.rs`
- `module_cache.rs`
- `module_index.rs`
- `symbols.rs` (needs refactoring — split LSP types from logic)
- `document.rs`

**`symbols.rs` split:**
The logic in `symbols.rs` (completion, hover, diagnostics, semantic tokens) is pure computation over `FileAnalysis`. But it returns tower-lsp types (`CompletionItem`, `Diagnostic`, etc.). Split into:
- `perl-lsp-core/src/intelligence.rs` — returns framework-agnostic types
- `perl-lsp/src/symbols.rs` — thin adapter converting core types to LSP types

This mirrors the existing architecture rule (symbols.rs is a thin adapter) but makes the boundary a crate boundary.

### Phase 2: wasm-bindgen API

Add `#[wasm_bindgen]` exports to `perl-lsp-core` behind the `wasm` feature flag. Only compiled when targeting WASM.

### Phase 3: VS Code web extension

Build the TypeScript wrapper that bridges `vscode-languageserver/browser` to the WASM API.

### Phase 4: Cross-file in browser (stretch)

For vscode.dev, VS Code provides a virtual filesystem API (`vscode.workspace.fs`). The web extension could:
1. Read workspace files via the VS Code API
2. Pass file contents to the WASM analysis engine
3. Build an in-memory module index

This would enable cross-file completion and goto-def in the browser, limited to files in the open workspace/repository.

## Implementation ordering

| Phase | Work | Effort |
|-------|------|--------|
| **1** | Extract `perl-lsp-core` crate (Cargo workspace) | Medium — refactoring, no new features |
| **2** | `wasm-bindgen` API on core | Low — just annotations |
| **3** | VS Code web extension (TypeScript) | Medium — new project, LSP glue |
| **4** | CI for WASM build + web extension packaging | Low |
| **5** | Cross-file via VS Code virtual filesystem | High — new module resolution path |

Phase 1 is the real work. Once the core is extracted, WASM compilation is mostly mechanical. Phase 3 is straightforward TypeScript using `vscode-languageserver/browser` (well-documented pattern).

## Files to create/modify

| File | Change |
|------|--------|
| `Cargo.toml` | Convert to workspace |
| `perl-lsp-core/Cargo.toml` | **New.** Core crate config |
| `perl-lsp-core/src/lib.rs` | **New.** Public API + wasm_bindgen exports |
| `perl-lsp/Cargo.toml` | **New.** Binary crate, depends on core |
| `editors/vscode-web/` | **New.** Web extension project |
| `editors/vscode-web/package.json` | Web extension manifest |
| `editors/vscode-web/src/extension.ts` | Extension entry point |
| `editors/vscode-web/src/server.ts` | LSP server in web worker |
| `.github/workflows/wasm.yml` | CI for WASM build |

## Size budget

The WASM binary should be small enough for fast browser loading:

| Component | Estimated WASM size |
|-----------|-------------------|
| tree-sitter runtime | ~200KB |
| tree-sitter-perl grammar | ~150KB |
| tree-sitter-pod grammar | ~50KB |
| perl-lsp-core logic | ~300KB |
| wasm-bindgen glue | ~50KB |
| **Total (gzipped)** | **~250KB** |

For comparison, PerlNavigator's web extension is ~2MB. We'd be 8x smaller.
