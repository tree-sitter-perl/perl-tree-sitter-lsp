# Spike examples — `spike/cpp-support`

Hand-readable examples for the multi-language reparse spikes. These are
illustrative source files, NOT wired into the LSP server yet — the
machinery lives in measured tests. Design: `docs/prompt-cpp-reparse.md`
and `docs/spike-query-extraction.md`.

- **`cpp/macro_reparse.cpp`** — the showpiece. A declarator-position
  macro destroys a class; the reparse seam expands it back, and the
  production witness bag then types variables of that recovered class —
  zero engine edits.
  Run: `cargo test cpp_type_inference_through_macro_reparse -- --nocapture`

- **`perl/prototypes.pl`** — the same reparse seam, local and
  preprocessor-free: prototypes (`($)`, `()`) change how call sites
  parse, fixed by reparenthesizing + re-parsing.
  Run: `cargo test reparse -- --nocapture`

See the before→after macro-expansion measurement over the obstacle
course: `cargo test cpp_reparse_obstacle_delta_report -- --nocapture`.
