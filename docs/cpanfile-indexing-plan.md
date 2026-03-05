# cpanfile Pre-Scan & Module Source Tagging

## Overview

Pre-scan direct dependencies from `cpanfile` at startup so auto-import
completions and diagnostics work for project deps without needing to
first type a `use` statement.

## Schema Change: `source` Column

Add a `source TEXT NOT NULL DEFAULT 'import'` column to the `modules` table:

| Tag        | How it gets there                      | Completion | Auto-import | Go-to-def |
|------------|----------------------------------------|------------|-------------|-----------|
| `cpanfile` | Pre-scanned from cpanfile at startup   | Yes        | Yes         | Yes       |
| `import`   | Lazy discovery via `use` in open files | Yes        | Yes         | Yes       |
| `scan`     | Future: bulk `@INC` scan               | No*        | No          | Yes       |

*`scan` items could be offered as fallback when no other matches exist (open design question).

## cpanfile Parsing

- Location: `{workspace_root}/cpanfile`
- Parse with **tree-sitter-perl** (cpanfile is valid Perl)
- `requires 'Module::Name'` parses as a function call with a string arg
- Extract module names from `requires` calls (ignore `on`, phase blocks
  for now — all `requires` are relevant)
- Also check `{workspace_root}/cpanfile.snapshot` existence as a signal
  that deps are managed via Carton

## Startup Flow

1. `initialize()` receives workspace root
2. Call `set_workspace_root(root)` — unblocks resolver thread, opens DB
3. Parse `{root}/cpanfile` with tree-sitter, extract module names
4. Feed each to `request_resolve()` with `source: "cpanfile"` tag
5. Resolver thread processes queue — SQLite cache hits are instant,
   uncached modules get parsed from `@INC`

## Progress Reporting

Use LSP `$/progress` (WorkDoneProgress):

```
create:  token = "perl-lsp/indexing"
begin:   title = "Indexing Perl modules"
report:  message = "List::Util (14/37)", percentage = 37
end:     message = "Indexed 37 modules"
```

- `client.send_request::<request::WorkDoneProgressCreate>()` in initialize
- `client.send_notification::<notification::Progress>()` for begin/report/end
- Only shown during cpanfile pre-scan, not lazy per-file resolution

## Parallelism

- **Single resolver thread** (current design) — sequential processing,
  won't saturate anyone's box
- Subprocess isolation already handles hangs (5s timeout + SIGKILL)
- Future option: configurable thread pool via LSP `initializationOptions`
  (e.g., `{"indexThreads": 2}`)

## Future: Framework Submodule Whitelist

Problem: `requires 'Mojolicious'` doesn't give you `Mojo::File`,
`Mojo::UserAgent`, etc. — framework users want the whole namespace.

Proposed solution: config file (`.perl-lsp.json` or `initializationOptions`)
with glob patterns for extra scanning:

```json
{
  "extraModules": ["Mojo::*", "Moose::*"]
}
```

This would scan `@INC` for matching `.pm` files and resolve them all.
Tagged as `cpanfile` (same priority as direct deps).

Not implementing now — depth 0 (direct cpanfile deps) gives immediate value.

## Future: Phase-Aware Filtering

cpanfile `on test` deps (Test::More, Test::Deep, etc.) should only
auto-complete in test files, not pollute completions in lib code.

Proposed rules:
- `on test` requires → tagged `cpanfile-test` in source column
- Auto-import completions for `cpanfile-test` modules only shown when:
  - File path is under `t/` directory, OR
  - File already imports a `Test::` module (signals test context)
- Go-to-def and hover still work everywhere regardless of tag

Requires: adding `source` field to `ModuleExports` struct (or a parallel
`DashMap<String, String>` for source tags), threading phase info from
`parse_cpanfile` through the resolver, and filtering in
`unimported_function_completions`.

## Future: Bulk @INC Scan

For go-to-def into any installed module (without polluting completions):

- Walk all `.pm` files under `@INC` directories
- Parse exports, tag as `scan`
- Filter from completion results by default
- Useful for: go-to-def, hover, references across module boundaries

Open question: offer `scan` results as completion fallback when there
are zero matches from `cpanfile`/`import` sources?

## Implementation Checklist

- [ ] Add `source` column to modules table (schema version bump)
- [ ] Parse cpanfile with tree-sitter in `initialize()`
- [ ] Pass source tag through `request_resolve()` → `save_to_db()`
- [ ] Filter `unimported_function_completions()` by source tag
- [ ] Add `$/progress` reporting to resolver thread
- [ ] Tests: cpanfile parsing, source tag filtering, progress messages
