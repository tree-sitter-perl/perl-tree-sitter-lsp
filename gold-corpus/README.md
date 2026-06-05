# perl-lsp gold corpus

A reproducible regression net of verified LSP-capability rows, captured against the **current usability-sprint binary**. Every row reproduces deterministically via the CLI query modes; each carries the exact cursor (`file:line:col`), expected behavior, actual output, a verdict, and a confidence. Positions are **0-based on input, 1-based on output**. `gold` = high-confidence verified; `provisional` = verified but with a soft/open-ended expected or a cosmetic drift; `reject` = the row's expected was indefensible and the row is excluded (kept visible in a Rejected section per file so the exclusion is not silent).

## Harness

`gold-corpus/run.pl` re-runs every row against the current binary. It needs the QA corpus checked out (default `~/perl-qa-corpus`, override with `CORPUS=`) and a release build (override with `BIN=`); it is **not** a cargo/CI test (it depends on that external corpus).

```sh
gold-corpus/run.pl                 # all capabilities  (~1m, exits non-zero on any crash/regression)
gold-corpus/run.pl definition hover
```

It is a **smoke + liveness** guard, not an exact-assertion runner — the `expected`/`actual` columns are human prose, not machine fixtures. It flags two things, both high-value: **CRASH** (a query that aborts the process — the check that catches the scanner-overflow class) and **REGRESS** (a row recorded `PASS` that now returns empty — a resolution that used to work and no longer does). Wrong-but-non-empty answers are *not* caught; exact target-matching would need a structured (JSON) fixture sidecar generated alongside this markdown — deliberately future work.

## Capabilities

| capability | file | gold | provisional | reject |
|------------|------|------|-------------|--------|
| outline (documentSymbol) | [outline.md](outline.md) | 11 | 1 | 0 |
| definition (goto-def) | [definition.md](definition.md) | 16 | 0 | 0 |
| references | [references.md](references.md) | 15 | 0 | 0 |
| hover | [hover.md](hover.md) | 14 | 4 | 0 |
| type-at | [type-at.md](type-at.md) | 15 | 3 | 1 |
| rename | [rename.md](rename.md) | 8 | 2 | 1 |
| workspace-symbol | [workspace-symbol.md](workspace-symbol.md) | 15 | 0 | 0 |
| diagnostics | [diagnostics.md](diagnostics.md) | 12 | 2 | 0 |
| completion | [completion.md](completion.md) | 12 | 0 | 0 |
| signature-help | [signature-help.md](signature-help.md) | 9 | 3 | 0 |
| semantic-tokens | [semantic-tokens.md](semantic-tokens.md) | 12 | 1 | 0 |
| document-highlight | [document-highlight.md](document-highlight.md) | 8 | 2 | 0 |
| linked-editing | [linked-editing.md](linked-editing.md) | 10 | 2 | 0 |
| **total** | | **157** | **20** | **3** |

## Already in corpus

Two earlier repo-keyed matrices cover definition + type-inference rows organized by repo and predate this capability-keyed corpus:

- [`matrix-A-oo-framework.md`](matrix-A-oo-framework.md) — Moo / Plack / Catalyst / Minion
- [`matrix-B-exporter-classic.md`](matrix-B-exporter-classic.md) — Exporter-Tiny / Sub-Exporter / URI / DateTime / Log-Log4perl

## Editor-only capabilities (not CLI-queryable — covered by e2e)

The following capabilities have **no CLI query mode** and are therefore **not** covered by this corpus. They are exercised by `run_e2e.sh` (drives a real nvim LSP client). This corpus does not pretend to cover them:

- inlayHint
- foldingRange
- selectionRange
- codeAction / auto-import
- formatting + rangeFormatting (perltidy)

> **Note:** completion, signatureHelp, semanticTokens/full, documentHighlight, and linkedEditingRange are now CLI-queryable and covered by this corpus (see the capability files above). They are no longer editor-only.

## Known failing (gold FAIL rows)

Confirmed real gaps at gold confidence — worth a human's attention:

- **def-16-codegen-type-function** (definition) — `Types::Standard::Any` has no literal `sub`; Type::Library codegen generates it at runtime, no navigable target.
- **ref-uri-const-fq** (references) — use-constant value not exported; FQ `URI::HAS_RESERVED_SQUARE_BRACKETS` refs don't link to def (and FQ-site->def is 0 refs).
- **ref-expt-mkopt** (references) — split reference graph: def/@EXPORT_OK cluster does not cross-link to the 3 in-body `mkopt(...)` call sites.
- **ref-expt-tag_opts-lexical** (references) — lexical `%$tag_opts` hash-deref spelling at :173 not emitted as a ref (9/10).
- **ref-uri-escape_char-fq** (references) — non-exported sub; FQ-only calls mostly fail to resolve cross-file.
- **hover-jsonpp-encode-json-imported-t** (hover) — imported-function hover works in `.pm` but returns nothing in `.t` consumers.
- **rename-01-mkopt-def** (rename) — from-def rename drops in-body `mkopt(...)` calls in other sub bodies.
- **rename-02-mkopt-callsite** (rename) — from-callsite rename gives the inverse (call-sites only); package resolves None.
- **rename-06-build-exporter** (rename) — in-body `build_exporter($config)` call inside setup_exporter dropped.
- **rename-09-base-constant** (rename) — use-constant rename drops `BASE` in `$w *= (BASE - $t)` at :79 (10/11).
- **diag-06** (diagnostics) — FP: own export `gettimeofday` called within its defining package.
- **diag-09 / diag-10** (diagnostics) — FP: dynamic typeglob codegen methods (`is_warn` / `warn`) flagged as undefined.
- **diag-11** (diagnostics) — FP: nested export `:tag` (`:standard` -> `:form` -> `submit`) not recursively expanded.
- **diag-12** (diagnostics) — FP: plan directive (`use Test::More 'no_plan'`) misread as a restricting import list, suppressing default @EXPORT.
- **completion-datetime-hashkey** (completion) — `$self->{` offers only 2 of 13 mutated keys; mutated-key harvesting misses keys assigned via `$self->{k}=...` in `_new` and elsewhere.
- **completion-typetiny-imported-blessed** (completion) — imported subs absent from bareword-statement completion; `use Scalar::Util qw(blessed)` not surfaced (only local subs offered).
- **sig-uri-check-path-function-noinvocant** (signature-help) — `$path` wrongly elided as invocant on a PLAIN function call `_check_path($rest, $$self)`; signature drops `$path`, shows only `($pre)`.
- **st-uri-constant-name-enummember** (semantic-tokens) — `use constant BASE` name tagged `function` not `enumMember`; `TOK_ENUM_MEMBER` is `#[allow(dead_code)]`, never emitted.
- **st-tt-regex-literal-regexp** (semantic-tokens) — `qr//` regex literal gets no semantic token; `TOK_REGEXP` is `#[allow(dead_code)]`, never emitted anywhere.
- **le-dt-p-hash-slices** (linked-editing) — three real `$p{time_zone}` refs inside the paren-wrapped ternary at L211/212/213 dropped (27 of 30); construct-specific builder ref-emission gap (rule #7).
