# Release & Distribution

## Prerequisites

- `ts-parser-perl` published on crates.io (done)
- Cargo.toml already uses `ts-parser-perl` as the dep name (done, renamed in earlier commit)

## Phase 1: GitHub Releases CI

Everything downstream depends on this. Every package manager (Mason, Homebrew, VS Code, Zed) pulls pre-built binaries from GitHub Releases.

### Release workflow

`.github/workflows/release.yml` — triggered on tag push (`v*`):

```yaml
name: Release

on:
  push:
    tags: ["v*"]

permissions:
  contents: write

jobs:
  build:
    strategy:
      matrix:
        include:
          - target: x86_64-unknown-linux-gnu
            os: ubuntu-latest
            archive: tar.gz
          - target: x86_64-apple-darwin
            os: macos-latest
            archive: tar.gz
          - target: aarch64-apple-darwin
            os: macos-latest
            archive: tar.gz
          - target: x86_64-pc-windows-msvc
            os: windows-latest
            archive: zip

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Checkout tree-sitter-perl
        uses: actions/checkout@v4
        with:
          repository: tree-sitter-perl/tree-sitter-perl
          path: ../tree-sitter-perl

      - name: Generate parser
        run: |
          cd ../tree-sitter-perl
          npx tree-sitter-cli generate

      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: ${{ matrix.target }}

      - name: Build
        run: cargo build --release --target ${{ matrix.target }}

      - name: Package (unix)
        if: matrix.archive == 'tar.gz'
        run: |
          cd target/${{ matrix.target }}/release
          tar czf ../../../perl-lsp-${{ matrix.target }}.tar.gz perl-lsp
          cd ../../..

      - name: Package (windows)
        if: matrix.archive == 'zip'
        run: |
          cd target/${{ matrix.target }}/release
          7z a ../../../perl-lsp-${{ matrix.target }}.zip perl-lsp.exe
          cd ../../..

      - name: Upload release asset
        uses: softprops/action-gh-release@v2
        with:
          files: perl-lsp-${{ matrix.target }}.${{ matrix.archive }}
```

### tree-sitter-perl dependency issue

The build currently requires `../tree-sitter-perl` as a sibling directory with generated `src/parser.c`. In CI, we need to:
1. Check out tree-sitter-perl alongside the main repo
2. Run `tree-sitter generate` to produce `src/parser.c`

**Longer term fix:** Once `ts-parser-perl` on crates.io includes the generated parser source (which it should — that's how tree-sitter parser crates work), switch the Cargo.toml dep from path to crates.io version. Then CI just needs `cargo build` with no sibling checkout. This also unblocks `cargo install perl-lsp`.

### Switching to crates.io dep

```toml
# Before (path dep):
[dependencies]
ts-parser-perl = { path = "../tree-sitter-perl" }

# After (crates.io):
[dependencies]
ts-parser-perl = "X.Y.Z"
```

This is the highest-priority change for distribution — it simplifies CI, enables `cargo install`, and makes the build self-contained.

### Versioning

Use semver. Tag format: `v0.1.0`. The version in `Cargo.toml` should match the tag (strip the `v` prefix).

Automate version bumping: the release workflow can validate that the tag matches `Cargo.toml` version and fail fast if they diverge.

### CI for PRs

Separate workflow for CI on PRs (not releases):

`.github/workflows/ci.yml`:

```yaml
name: CI

on:
  pull_request:
  push:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Checkout tree-sitter-perl
        uses: actions/checkout@v4
        with:
          repository: tree-sitter-perl/tree-sitter-perl
          path: ../tree-sitter-perl

      - name: Generate parser
        run: |
          cd ../tree-sitter-perl
          npx tree-sitter-cli generate

      - uses: dtolnay/rust-toolchain@stable

      - run: cargo test

      - run: cargo clippy -- -D warnings
```

Once we switch to crates.io dep, the tree-sitter checkout steps go away.

---

## Phase 2: crates.io

### Requirements

- Switch `ts-parser-perl` from path dep to crates.io version dep (see above)
- Fill in `Cargo.toml` metadata:

```toml
[package]
name = "perl-lsp"
version = "0.1.0"
edition = "2021"
authors = ["tree-sitter-perl contributors"]
license = "MIT"
description = "A Perl LSP server built on tree-sitter-perl and tower-lsp"
repository = "https://github.com/tree-sitter-perl/perl-tree-sitter-lsp"
keywords = ["perl", "lsp", "tree-sitter", "language-server"]
categories = ["development-tools", "text-editors"]

[[bin]]
name = "perl-lsp"
path = "src/main.rs"
```

- `cargo publish --dry-run` to validate
- `cargo publish`
- Users install with: `cargo install perl-lsp`

---

## Phase 3: Editor integrations (all can be done in parallel)

### 3a. nvim-lspconfig

PR to `neovim/nvim-lspconfig` adding `lua/lspconfig/configs/perl_lsp.lua`:

```lua
local util = require 'lspconfig.util'

return {
  default_config = {
    cmd = { 'perl-lsp' },
    filetypes = { 'perl' },
    root_dir = util.root_pattern('cpanfile', 'Makefile.PL', 'Build.PL', '.git'),
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/tree-sitter-perl/perl-tree-sitter-lsp

A Perl language server built on tree-sitter-perl. Features type inference,
cross-file module resolution, framework intelligence (Moo/Moose/Mojo::Base/DBIC),
and 15+ LSP capabilities.

Install: `cargo install perl-lsp` or download from GitHub Releases.
]],
  },
}
```

Also add `doc/configs/perl_lsp.md` with setup instructions.

### 3b. Mason

PR to `mason-org/mason-registry` adding `packages/perl-lsp/package.yaml`:

```yaml
---
name: perl-lsp
description: Perl language server built on tree-sitter-perl
homepage: https://github.com/tree-sitter-perl/perl-tree-sitter-lsp
licenses:
  - MIT
languages:
  - Perl
categories:
  - LSP

source:
  id: pkg:github/tree-sitter-perl/perl-tree-sitter-lsp@{{ version }}
  asset:
    - target: linux_x64
      file: perl-lsp-x86_64-unknown-linux-gnu.tar.gz
      bin:
        perl-lsp: perl-lsp
    - target: darwin_x64
      file: perl-lsp-x86_64-apple-darwin.tar.gz
      bin:
        perl-lsp: perl-lsp
    - target: darwin_arm64
      file: perl-lsp-aarch64-apple-darwin.tar.gz
      bin:
        perl-lsp: perl-lsp
    - target: win_x64
      file: perl-lsp-x86_64-pc-windows-msvc.zip
      bin:
        perl-lsp: perl-lsp.exe
```

### 3c. Helix

PR to `helix-editor/helix` modifying `languages.toml`:

```toml
[language-server.perl-lsp]
command = "perl-lsp"

[[language]]
name = "perl"
language-servers = ["perl-lsp"]
```

### 3d. Emacs lsp-mode

PR to `emacs-lsp/lsp-mode` adding to `clients/lsp-perl.el` (or new file):

```elisp
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("perl-lsp"))
  :activation-fn (lsp-activate-on "perl")
  :server-id 'perl-lsp
  :priority -1))  ;; lower than PLS if both installed
```

### 3e. Homebrew tap

Create repo `tree-sitter-perl/homebrew-tap` with `Formula/perl-lsp.rb`:

```ruby
class PerlLsp < Formula
  desc "Perl language server built on tree-sitter-perl"
  homepage "https://github.com/tree-sitter-perl/perl-tree-sitter-lsp"
  url "https://github.com/tree-sitter-perl/perl-tree-sitter-lsp/archive/refs/tags/v0.1.0.tar.gz"
  sha256 "TO_BE_FILLED"
  license "MIT"

  depends_on "rust" => :build
  depends_on "node" => :build  # for tree-sitter generate

  def install
    # Clone tree-sitter-perl for the build
    system "git", "clone", "--depth=1",
           "https://github.com/tree-sitter-perl/tree-sitter-perl.git",
           buildpath/"../tree-sitter-perl"
    cd buildpath/"../tree-sitter-perl" do
      system "npx", "tree-sitter-cli", "generate"
    end
    system "cargo", "install", *std_cargo_args
  end

  test do
    assert_match "perl-lsp", shell_output("#{bin}/perl-lsp --version 2>&1", 1)
  end
end
```

**Note:** Once the path dep is replaced with crates.io dep, the formula simplifies dramatically — no git clone, no tree-sitter generate. Just `cargo install`.

Install: `brew tap tree-sitter-perl/tap && brew install perl-lsp`

### 3f. VS Code extension

Create `editors/vscode/` with:

**`package.json`:**
```json
{
  "name": "perl-lsp",
  "displayName": "Perl (tree-sitter LSP)",
  "description": "Perl language support via perl-lsp",
  "version": "0.1.0",
  "publisher": "tree-sitter-perl",
  "engines": { "vscode": "^1.75.0" },
  "categories": ["Programming Languages"],
  "activationEvents": ["onLanguage:perl"],
  "main": "./out/extension.js",
  "contributes": {
    "configuration": {
      "title": "Perl LSP",
      "properties": {
        "perlLsp.path": {
          "type": "string",
          "default": "perl-lsp",
          "description": "Path to the perl-lsp binary"
        }
      }
    }
  },
  "dependencies": {
    "vscode-languageclient": "^9.0.0"
  }
}
```

**`src/extension.ts`:**
```typescript
import { workspace, ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const command = workspace.getConfiguration('perlLsp').get<string>('path', 'perl-lsp');

  const serverOptions: ServerOptions = {
    command,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'perl' }],
  };

  client = new LanguageClient('perl-lsp', 'Perl LSP', serverOptions, clientOptions);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
```

Publish with `vsce package && vsce publish`. Requires a marketplace publisher account.

**Future enhancement:** Auto-download the binary from GitHub Releases based on platform, instead of requiring the user to install it separately.

### 3g. Zed extension

You own the plugin. Update it to point to the binary from GitHub Releases. The extension implements `language_server_command()` in Rust WASM to download the appropriate platform binary.

### 3h. Claude Code plugin

Claude Code has a plugin marketplace with LSP support. Two JSON files in a repo:

Create repo `tree-sitter-perl/perl-lsp-claude-code` (or subdirectory `editors/claude-code/`):

**`.claude-plugin/plugin.json`:**
```json
{
  "name": "perl-lsp",
  "description": "Perl language intelligence — type inference, cross-file resolution, framework support (Moo/Moose/Mojo/DBIC), semantic diagnostics",
  "version": "0.1.0",
  "author": { "name": "tree-sitter-perl" },
  "homepage": "https://github.com/tree-sitter-perl/perl-tree-sitter-lsp",
  "repository": "https://github.com/tree-sitter-perl/perl-tree-sitter-lsp",
  "lspServers": {
    "perl": {
      "command": "perl-lsp",
      "args": [],
      "extensionToLanguage": {
        ".pl": "perl",
        ".pm": "perl",
        ".t": "perl"
      }
    }
  }
}
```

**`.lsp.json`:**
```json
{
  "perl": {
    "command": "perl-lsp",
    "args": [],
    "extensionToLanguage": { ".pl": "perl", ".pm": "perl", ".t": "perl" }
  }
}
```

Users install with:
```bash
cargo install perl-lsp             # get the binary
# then in Claude Code:
/plugin install perl-lsp            # enable the plugin
```

Claude Code will start perl-lsp automatically when opening Perl files and use:
- Diagnostics for edit-diagnose-fix loops (catches typos in method names, unresolved imports)
- Goto-def and references for understanding code before modifying it
- Hover for type info and POD docs
- Workspace symbols for navigating large codebases

This is high-value for Claude Code's Perl comprehension — semantic understanding instead of pattern-matching on source text.

### 3i. AI agent compatibility (Cursor, Windsurf, Continue, Cline, Aider)

The VS Code extension (3f) automatically covers **five AI coding tools**:

| Tool | How they get perl-lsp | What they use |
|------|----------------------|---------------|
| **Cursor** | VS Code extension (Cursor is a fork) | All 17 LSP features + AI uses diagnostics as context |
| **Windsurf** | VS Code extension (also a fork) | All LSP features |
| **Continue** | VS Code extension (inherits via host editor) | Diagnostics via `@problems` context, symbols |
| **Cline** | VS Code extension (inherits via host editor) | Diagnostics for agentic edit-diagnose-fix loops |
| **Aider** | `--lint-cmd "perl-lsp --check ."` config | Diagnostics via CLI (already built) |

No extra work needed for Cursor/Windsurf/Continue/Cline — the VS Code extension is sufficient. For Aider, users configure `--lint-cmd` to point at our `--check` CLI mode.

---

## Phase 4: Project polish for release

### README

Needs a proper README with:
- Feature highlights (the competitive advantage list vs PerlNavigator)
- Installation instructions for each editor
- Screenshot/GIF of key features (completion, goto-def, type inference, dynamic method dispatch)
- Configuration options
- Link to CLAUDE.md for architecture

### `--version` flag

Add `--version` CLI flag that prints the version from `Cargo.toml`. Homebrew and Mason sometimes use this for validation.

```rust
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("perl-lsp {}", env!("CARGO_PKG_VERSION"));
        return;
    }
    // ... existing main
}
```

### LICENSE file

Add MIT LICENSE file to the repo root if not already present.

---

## Execution order

| Step | Work | Blocks |
|------|------|--------|
| **1** | Switch ts-parser-perl to crates.io dep | CI simplification, cargo install, Homebrew |
| **2** | GitHub Releases CI workflow | Mason, VS Code, Zed, Homebrew |
| **3** | `--version` flag + Cargo.toml metadata + LICENSE | All distribution channels |
| **4** | `cargo publish` | cargo install |
| **5** | nvim-lspconfig PR | Neovim users |
| **6** | Mason PR | Neovim users (easy install) |
| **7** | Helix PR | Helix users |
| **8** | Emacs lsp-mode PR | Emacs users |
| **9** | Homebrew tap | macOS/Linux users |
| **10** | VS Code extension | VS Code + Cursor + Windsurf + Continue + Cline users |
| **11** | Zed extension update | Zed users |
| **12** | Claude Code plugin | Claude Code users |
| **13** | Aider docs | Aider users (just docs — `--check` CLI already works) |

Steps 1-4 are the critical path. Steps 5-13 are all independent and can be done in parallel. Step 10 (VS Code extension) has the highest reach — it covers 5 AI coding tools in one shot.
