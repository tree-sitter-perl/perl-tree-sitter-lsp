# C++ QA log — real-codebase sweeps, gaps found, fixes

A running record of QA-ing the C++ support against real projects, the gaps
found, and their resolution. Loop: sweep `--outline` (and goto/completion)
across a real codebase → bucket files (OK / EMPTY / STRUCTURE-CORRUPT /
CRASH) → minimal-repro each interesting case → fix or codify as gold
(xfail for deferred). Gold fixtures live under `gold-corpus/fixtures/cpp-*`.

## Done

| gap | found in | status | fixture |
|---|---|---|---|
| cross-file namespace macro (`SPDLOG_NAMESPACE_BEGIN` in another header) | spdlog | **FIXED** — cross-file macro resolution (gather #defines from #included headers) | cpp-cross-file-namespace-macro-resolved (gold), ns_macro (xfail: unincluded) |
| self-referential macro OOM (`#define M x // M M`) | Dear ImGui | **FIXED** — blue-paint guard + comment strip + size cap | cpp-selfref-macro-no-oom (gold) |
| out-of-line `Class::method` loses qualifier (attributed to namespace) | leveldb, fmt | **FIXED** — `@qualifier` capture → package | cpp-out-of-line-method-qualifier (gold) |
| macro-recovered spans in original coords | (design) | FIXED earlier | — |

## Open (scout-found, not yet fixed)

| gap | found in | freq | plan |
|---|---|---|---|
| template member fn → `Sub` not `Method` | json, fmt, range-v3 | high | classification fix when a `template<>` clause precedes a member |
| nested qualified class `class A::B` → `Sub`, wrong kind | leveldb | low | fold into the qualified-declarator fix |
| `concept X = ...` emits no symbol | range-v3 (raw C++20) | low | new symbol kind — deeper |

## Robustness
spdlog 107 headers: 0 crashes; after cross-file macros, structure-corrupt
4→1, empty 14→12. Self-ref OOM was the only crash across ~360 files (6
projects) — now guarded.
