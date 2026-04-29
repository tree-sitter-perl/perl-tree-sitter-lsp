# QA harness

Reproducible scripts for indexing-perf and type-correctness QA over the
system Perl's `@INC`. Generates a synthetic workspace that `use`s every
discoverable `.pm`, runs the LSP's full startup with bench instrumentation,
and dumps `--dump-package` JSON for a curated pick list.

## Quickstart

```sh
cargo build --release
./scripts/qa/run-all.sh
```

Outputs everything into `qa-workspace/` (gitignored):

- `qa-workspace/modules.list` — every `Module::Name` discoverable under `@INC`
- `qa-workspace/cpanfile`, `qa-workspace/lib/QA.pm` — synthetic project
- `qa-workspace/bench-1.log`, `bench-2.log` — `PERL_LSP_BENCH=1` stderr
- `qa-workspace/picks-run.txt` — curated picks + 10 random
- `qa-workspace/dumps/*.json` — `--dump-package` output per pick

## Individual steps

| script | purpose |
|---|---|
| `gen-workspace.sh [DIR]` | walk `@INC`, write workspace |
| `run-bench.sh [DIR]` | two `--dump-package` runs with bench env var |
| `analyze-bench.sh BENCH_LOG` | top slow modules, distribution, dup re-parses |
| `dump-packages.sh [DIR] [PICKS]` | `--dump-package` per line in pick list |
| `analyze-dumps.sh [DUMPS_DIR]` | per-package coverage, raw/bag mismatches, bless-rep, provenance |
| `run-all.sh [DIR] [PICKS]` | runs everything end-to-end |

## Pick list format

`picks-default.txt` is a plain list of `Module::Name`, one per line.
Lines starting with `#` and blank lines are ignored. Append a random
tail via `shuf -n 10 qa-workspace/modules.list` to catch corners the
curated list doesn't.

## Bench log format

Each line emitted under `PERL_LSP_BENCH=1` is tab-separated:

```
bench   <module>   <wall_us>     <symbols>      <bytes>
bench   <module>   <wall_us>     oversize       <bytes>
```

`oversize` indicates the module exceeded the 1MB skip threshold.
