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
| chained METHOD calls (`box.getX().`, incl. inherited) | (design) | **FIXED** — method-return writeback-lite through MethodOnClass | cpp-chained-method-call, cpp-inherited-method-return-chain (gold) |
| template member fn classified as `Sub` | json, fmt, range-v3 | **FIXED** — a sub owned by a class is a method (into_file_analysis) | cpp-template-member-is-method (gold) |
| macro-recovered spans in original coords | (design) | FIXED earlier | — |

## Open (scout-found, not yet fixed)

| gap | found in | freq | plan |
|---|---|---|---|
| `concept X = ...` emits no symbol | range-v3 (raw C++20) | low | new symbol kind — deeper |

## Robustness
spdlog 107 headers: 0 crashes; after cross-file macros, structure-corrupt
4→1, empty 14→12. Self-ref OOM was the only crash across ~360 files (6
projects) — now guarded.

## Re-sweep validation (after cross-file macros + template fix)

- **nlohmann/json** (47 headers): 40 ok, 6 empty (all macro-only/forward-decl
  — legit), **0 real structure-corrupt (was 4), 0 crash**. The cross-file
  namespace-macro fix fully resolved json's corruption.

## Deferred (xfail — need a model change, tracked → XPASS when fixed)

- **`concept X = ...` emits no symbol** (cpp-xfail-concept-symbol). Needs a
  `Concept` SymKind (or a least-wrong mapping) — a model ripple. Low
  frequency (libs hide concepts behind their own macros).
- **`auto`/deduced return types** (cpp-xfail-auto-deduced-return). The
  return depends on the body, which needs the iterative fold the cpp pack
  doesn't run. Declared returns work; `auto` is the tail.

## Note: cpp cross-file resolution

Method-return resolution flows through MethodOnClass, which is cross-file
CAPABLE (the reducer recurses into a cached module's bag). But C++ has no
workspace/module index yet — only the queried file + its #included macros
are analyzed. So cross-file method returns + goto-def into another header
aren't active yet; that's the next cross-file piece (a cpp workspace
indexer, mirroring the Perl module_resolver). Single-file + inheritance
within the file work today.
