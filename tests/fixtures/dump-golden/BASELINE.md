# Type-inference worklist refactor — Phase 1 baseline

Captured at the start of the worklist refactor (branch
`refactor/type-inference-worklist`) so every later commit can prove no
observable behavior change.

Generation:

```sh
cargo build --release
for pkg in "Mojolicious::Routes::Route" "MojoApp" "Moo::Service" "Schema::Result::User"; do
  fname=$(echo "$pkg" | sed 's|::|__|g')
  target/release/perl-lsp --dump-package test_files "$pkg" 2>/dev/null \
    | sed "s|$HOME|\$HOME|g" \
    > "tests/fixtures/dump-golden/${fname}.json"
done
```

`$HOME` is substituted out so the path-bearing fields (e.g. the @INC
location of `Mojolicious::Routes::Route`) survive across runs on the
same machine. The goldens are **not portable across machines** —
`Mojolicious::Routes::Route` resolves through whatever `perl -V` reports
as the @INC site_perl path, so a different plenv/perlbrew layout will
shift it. They are a per-branch regression net, not a CI artifact.

## Test baselines (current implementation)

| Suite | Count | Source |
|---|---|---|
| `cargo test` | 494 passed, 0 failed | `/tmp/baseline.txt` (not committed; reproducible) |
| `./run_e2e.sh` | 93 passed across 5 nvim suites (27 + 25 + 7 + 14 + 20), 0 failed | `/tmp/baseline-e2e.txt` |

Re-capture: `cargo test 2>&1 | tail -1` and `./run_e2e.sh 2>&1 | tail -10`.

## At-risk test surface (internal-field readers)

These tests reach into `FileAnalysis` private state. The worklist
refactor must preserve the field shapes they read, OR provide
identical-shaped accessor methods. Per the spec: *"change internals
around stable read sites, never the other way around."*

| File | Line | Field read |
|---|---|---|
| `src/file_analysis_tests.rs` | 829 | `.witnesses` |
| `src/file_analysis_tests.rs` | 840 | `fa.witnesses.len()` |
| `src/file_analysis_tests.rs` | 877 | `.witnesses` |
| `src/file_analysis_tests.rs` | 943 | `fa.witnesses` |
| `src/file_analysis_tests.rs` | 1249 | `reg.query(&fa.witnesses, ...)` |
| `src/file_analysis_tests.rs` | 1263 | `reg.query(&fa.witnesses, ...)` |
| `src/builder_tests.rs` | 3391 | `fa.call_bindings` |
| `src/builder_tests.rs` | 4981 | `.type_constraints` |
| `src/builder_tests.rs` | 5754 | `.type_constraints` |

Fields not currently read by any test (free to restructure without
compat shims): `sub_return_delegations`, `self_method_tails`,
`return_infos`, `method_call_bindings`. They are written from
`builder.rs` only.

## Golden fixtures committed

| File | Package | Subs | Source |
|---|---|---|---|
| `Mojolicious__Routes__Route.json` | `Mojolicious::Routes::Route` | 41 | `$HOME/.plenv/.../Mojolicious/Routes/Route.pm` (external @INC) |
| `MojoApp.json` | `MojoApp` | 5 | `test_files/frameworks.pl` |
| `Moo__Service.json` | `Moo::Service` | 6 | `test_files/oop_playground.pl` |
| `Schema__Result__User.json` | `Schema::Result::User` | 8 | `test_files/oop_playground.pl` (DBIC Result) |

These cover the four flavours called out in the spec (Mojolicious
external dep, Mojo::Base demo class, Moo class, DBIC Result class).

## Verification

After every refactor commit, regenerate and diff:

```sh
for pkg in "Mojolicious::Routes::Route" "MojoApp" "Moo::Service" "Schema::Result::User"; do
  fname=$(echo "$pkg" | sed 's|::|__|g')
  target/release/perl-lsp --dump-package test_files "$pkg" 2>/dev/null \
    | sed "s|$HOME|\$HOME|g" \
    | diff -u "tests/fixtures/dump-golden/${fname}.json" -
done
```

Empty diff for all four = no observable behavior change.
