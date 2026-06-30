# Config / options schema — forward design

**Landed:** `DiagnosticOptions` is serde-driven. `#[serde(rename_all =
"camelCase", default)]` makes the struct its own schema for the LSP side —
`backend.rs` deserializes `initializationOptions.diagnostics` in one
`from_value` call, no hand-mapped key strings. The CLI surface
(`from_cli_args`) is the one spelling serde can't derive; it stays explicit
and is pinned against drift by `cli_flags_match_diagnostic_option_fields`,
which enumerates the fields *via serde serialization* (no reflection crate)
and asserts each has its canonical `--kebab` flag.

Three pieces were deliberately left for their forcing functions.

## 1. A `Config` god-struct — own at the top, pass slices

When a second config group appears (plugin dirs, workspace tuning,
formatter prefs), introduce one owning struct parsed once:

```rust
#[derive(Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
struct Config { diagnostics: DiagnosticOptions, /* … */ }
```

The backend holds one `Arc<RwLock<Config>>`, parses `initializationOptions`
once, and handles `didChangeConfiguration` in one place. **But call sites
keep taking the narrow slice** — `collect_diagnostics(&cfg.diagnostics,
…)`, never `(&cfg, …)`: the slice is an honest interface (it says exactly
what the function reads), it respects the layer boundary (diagnostic
toggles are an LSP-adapter concern and must not ride into the model), and
it keeps `DiagnosticOptions { optional_deref: true, ..Default::default() }`
a two-line test fixture. Interface segregation: depend on the narrowest
thing that works. Not worth a one-member struct today.

## 2. A generated editor schema (`schemars`)

Add `#[derive(schemars::JsonSchema)]` and a `--dump-options-schema` mode:
the `///` field docs become schema descriptions, and the JSON Schema feeds
the VS Code `contributes.configuration` block + the README options table —
generated, not hand-kept. Worth the (small, additive) dep only once we
ship an editor settings contribution. Until then the serde `rename_all`
already removes the drift-prone surface.

## 3. A `define_options!` macro

A `macro_rules!` listing each option once `(field, "--flag", "doc")` could
generate the struct + serde attrs + `from_cli_args` + doc list, killing CLI
drift by construction instead of by test — consistent with `typed_node!` in
`cst.rs`. Deferred: the drift *test* is cheaper than the macro's loss of
greppability, and serde already covers the LSP side. Reach for it only if
the CLI mapping grows enough hand-written rows that the test stops being
enough.

## The forcing function

The PL-code / per-code-severity framework (`prompt-cli-tools.md`) is what
turns these `bool`s into per-code objects (`{ enabled, severity,
suppressible }`). That is the moment piece 1 (a real `Config`) and the
richer schema (piece 2) pay off — a field's type change is a serde change,
nothing else. Doing the serde-ification now is what makes that a small
diff later.
