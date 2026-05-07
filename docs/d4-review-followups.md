# D4 review follow-ups (PR #31)

Captured during review on 2026-05-07. Not blockers — ship-after items.

## 1. Release-mode `eprintln!` in fold safety net (builder.rs:5953-5959)

LSP server stderr can pollute editor output channels. `tracing::error!`
respects log filtering and matches the rest of the project's error paths.
The doc you replaced explicitly named `tracing::error!` as the alternative.

## 2. `apply_chain_typing_assignments` already-typed check is full-bag scan

builder.rs:6047-6051. `O(M·N)` per worklist iteration over the whole
bag → `O(I·M·N)`. Invisible on small files; if profiling lights up, a
`HashSet<(name, scope, point)>` index amortizes the inner scan to O(1).

## 3. `FileAnalysis::inferred_type` is now full-bag scan per query

file_analysis.rs:1389-1407. Same shape as #2, query-time. Public API.
Same fix when it matters.

## 4. No test for the release-mode `MAX_FOLD_ITERATIONS` break

The new `eprintln! + break` is the kind of safety net that bit-rots
silently. A synthetic oscillating reducer + an assertion that the loop
terminates would lock in the contract.
