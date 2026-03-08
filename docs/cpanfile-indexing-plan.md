# cpanfile Indexing & Module Source Tagging

## How it works

At startup, the resolver thread parses `{workspace_root}/cpanfile` with
tree-sitter queries, extracts all `requires 'Module::Name'` calls, and
resolves them from `@INC`. Results are cached in SQLite with a `source`
tag distinguishing how each module was discovered.

### Source tags

| Tag        | How it gets there                      | Completion | Auto-import | Go-to-def |
|------------|----------------------------------------|------------|-------------|-----------|
| `cpanfile` | Pre-scanned from cpanfile at startup   | Yes        | Yes         | Yes       |
| `import`   | Lazy discovery via `use` in open files | Yes        | Yes         | Yes       |
| `scan`     | Future: bulk `@INC` scan               | No*        | No          | Yes       |

### Progress reporting

Uses LSP `$/progress` (WorkDoneProgress) during cpanfile pre-scan:
- `begin`: "Indexing Perl modules"
- `report`: "List::Util (14/37)", percentage
- `end`: "Indexed 37 modules"

Only shown during startup cpanfile batch, not lazy per-file resolution.

## Future work

### Phase-aware filtering

cpanfile `on test` deps (Test::More, Test::Deep, etc.) should only
auto-complete in test files, not pollute completions in lib code.

Proposed rules:
- `on test` requires tagged `cpanfile-test` in source column
- Auto-import completions for `cpanfile-test` modules only shown when:
  - File path is under `t/` directory, OR
  - File already imports a `Test::` module
- Go-to-def and hover still work everywhere regardless of tag

### Framework submodule whitelist

Problem: `requires 'Mojolicious'` doesn't give you `Mojo::File`,
`Mojo::UserAgent`, etc.

Proposed: config (`.perl-lsp.json` or `initializationOptions`) with
glob patterns like `"extraModules": ["Mojo::*", "Moose::*"]` to scan
`@INC` for matching `.pm` files.

### Bulk @INC scan

For go-to-def into any installed module without polluting completions.
Walk all `.pm` files under `@INC`, parse exports, tag as `scan`.
