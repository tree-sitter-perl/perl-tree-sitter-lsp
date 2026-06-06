# perl-lsp gold corpus

A reproducible regression net of verified LSP-capability rows, run against a **cpm-installed, snapshot-pinned substrate**. The substrate is a hermetic, version-locked module tree built from [`gold-corpus/cpanfile`](cpanfile) + [`gold-corpus/cpanfile.snapshot`](cpanfile.snapshot) into `gold-corpus/local/lib/perl5`, so every query resolves against the **same pinned versions** in dev and CI. The LSP is pointed at that tree as both workspace root and `PERL5LIB` (its arch dir, e.g. `x86_64-linux/`, included), so resolution is deterministic and editor-free.

Rebuild the substrate:

```sh
cd gold-corpus && carton install --deployment          # exact, from snapshot
# or, faster:    cpm install -L local --resolver snapshot
```

Positions are **0-based on input, 1-based on output**.

## Harness

`gold-corpus/run.pl` is an **exact-assertion** regression runner. **`fixtures/*.json` is the source of truth** (machine-checkable rows); each per-capability **`*.md` is the human view** of the same rows. The runner needs the substrate (default `gold-corpus/local/lib/perl5`, override `CORPUS=`) and a release build (override `BIN=`); it is **not** a cargo/CI test (it depends on the locally-installed substrate).

```sh
gold-corpus/run.pl                 # run the suite (exits non-zero on FAIL/CRASH/XPASS)
gold-corpus/run.pl definition hover
gold-corpus/run.pl --list          # capabilities + gold/xfail/prov counts
gold-corpus/run.pl --emit definition LWP/RobotUA.pm 64 23   # author against this
```

Each fixture row asserts the query's **normalized** output against two substring lists — `expect.all` (every string must appear) and `expect.none` (none may) — with a `status`:

- **gold** — the assertion must hold; otherwise **FAIL** (a real regression).
- **xfail** — a known gap: the (correct) assertion must currently *not* hold. If it starts holding → **XPASS**, a soft failure telling you to promote the row to gold. Known gaps can't silently rot, and a fix is detected automatically.
- **provisional** — run and reported, never fails the suite.

A process abort (the scanner-overflow class) is always a hard **CRASH** fail. Output is normalized before matching — absolute paths reduced to basenames; JSON outputs (references / workspace-symbol / outline / rename / diagnostics) decoded and re-encoded canonically (sorted keys, compact) so one substring ties file+line+kind. The **same** `normalize()` backs `--emit`, so fixtures authored against `--emit` output match the runner by construction.

## Capabilities

| capability | file | gold | xfail | provisional | dropped |
|------------|------|------|-------|-------------|---------|
| outline (documentSymbol) | [outline.md](outline.md) | 8 | 0 | 2 | 1 |
| definition (goto-def) | [definition.md](definition.md) | 15 | 1 | 0 | 0 |
| references | [references.md](references.md) | 13 | 2 | 0 | 1 |
| hover | [hover.md](hover.md) | 12 | 0 | 4 | 1 |
| type-at | [type-at.md](type-at.md) | 12 | 0 | 2 | 5 |
| rename | [rename.md](rename.md) | 7 | 1 | 1 | 2 |
| workspace-symbol | [workspace-symbol.md](workspace-symbol.md) | 14 | 0 | 0 | 1 |
| diagnostics | [diagnostics.md](diagnostics.md) | 4 | 1 | 0 | 9 |
| completion | [completion.md](completion.md) | 6 | 2 | 0 | 3 |
| signature-help | [signature-help.md](signature-help.md) | 8 | 1 | 3 | 0 |
| semantic-tokens | [semantic-tokens.md](semantic-tokens.md) | 8 | 2 | 1 | 2 |
| document-highlight | [document-highlight.md](document-highlight.md) | 7 | 0 | 1 | 2 |
| linked-editing | [linked-editing.md](linked-editing.md) | 9 | 1 | 2 | 0 |
| **total** | | **123** | **11** | **16** | **27** |

The substrate is installed lib content only — it contains **no test files (`t/*.t`), `bin/`, or examples**. Rows whose original cursor lived in a test file, or whose module is not in the snapshot (e.g. JSON::PP, Time::HiRes), are listed in each capability's "Dropped" section. Rows that leaned on test-file call sites were re-authored to the surviving in-lib locations.

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

> **Note:** completion, signatureHelp, semanticTokens/full, documentHighlight, and linkedEditingRange are CLI-queryable and covered by this corpus (see the capability files above). They are no longer editor-only.

## Known failing (xfail rows)

Confirmed gaps captured at xfail — the correct assertion does not currently hold, so the runner pins the gap (XPASS the day it's fixed):

- **def-16-codegen-type-function** (definition) — `Types::Standard::Any` has no literal `sub`; Type::Library codegen mints it at runtime, so goto-def degrades to the package decl (`Standard.pm:1:1`) instead of the `name => "Any"` declaration at `Standard.pm:215`.
- **ref-uri-const-fq** (references) — use-constant value not exported; FQ `URI::HAS_RESERVED_SQUARE_BRACKETS` refs (`_server.pm:26`, `_generic.pm:13`) don't link to the def.
- **ref-expt-tag_opts-lexical** (references) — lexical `%$tag_opts` hash-deref spelling at `:173` not emitted as a ref (9/10).
- **rename-09-base-constant** (rename) — use-constant rename drops `BASE` in `$w *= (BASE - $t)` at `:79` (10/11).
- **diag-08** (diagnostics) — `bootstrap Net::SSLeay $VERSION` is a true unresolved-function, but the diagnostic is `info` severity and the harness binds `--severity warning`, so it is suppressed below threshold (not a regression).
- **completion-datetime-hashkey** (completion) — `$self->{` offers only 2 of 13 mutated keys; keys assigned via `$self->{k}=...` in `_new` aren't harvested.
- **completion-typetiny-imported-blessed** (completion) — imported `blessed` (`use Scalar::Util qw(blessed)`) absent from bareword-statement completion; only local subs offered.
- **sig-uri-check-path-function-noinvocant** (signature-help) — `$path` wrongly elided as invocant on a PLAIN function call `_check_path($rest, $$self)`; signature shows only `($pre)`.
- **st-uri-constant-name-enummember** (semantic-tokens) — `use constant BASE` name tagged `function` not `enumMember`; `TOK_ENUM_MEMBER` is `#[allow(dead_code)]`, never emitted.
- **st-tt-regex-literal-regexp** (semantic-tokens) — `qr//` literal gets no semantic token; `TOK_REGEXP` is `#[allow(dead_code)]`, never emitted.
- **le-dt-p-hash-slices** (linked-editing) — three real `$p{time_zone}` refs inside the paren-wrapped ternary at L211/212/213 dropped (27 of 30); construct-specific builder ref-emission gap (rule #7).
