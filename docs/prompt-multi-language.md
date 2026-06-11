# Multi-language serving — the wiring design

**Status: design, ready to execute.** The four spikes
(`docs/spike-query-extraction.md`, branch
`worktree-query-extraction-spike`) proved the engine runs every floor
verb for pack-built languages with zero engine edits. This doc is the
remaining piece: how the SERVER routes a second language, written so
the implementation is a checklist, not a design round. The spike
deliberately stopped at this seam — it's the only engine-adjacent
work in the whole program.

## The shape: LanguageDriver + registry

One trait, two implementations on day one:

```rust
/// Everything the server needs to host a language. Lives beside
/// `document.rs` (Index layer — drivers call DOWN into Build).
pub trait LanguageDriver: Send + Sync {
    /// "perl", "r", "python" — keys FileAnalysis.language and cache rows.
    fn id(&self) -> &'static str;
    /// Extensions this driver claims ("pm", "pl", "t" / "R", "r").
    fn extensions(&self) -> &[&'static str];
    fn make_parser(&self) -> tree_sitter::Parser;
    /// Source → FileAnalysis. Perl: `builder::build_with_plugins`
    /// (the existing path, untouched). Pack languages:
    /// `query_extract::extract(...).into_file_analysis()`.
    fn analyze(&self, tree: &Tree, source: &[u8]) -> FileAnalysis;
    /// Module-name → candidate relative paths (the spike's
    /// `module_paths` predicate, promoted). Perl's current
    /// `module_name.replace("::", "/") + ".pm"` in module_resolver.rs
    /// line ~471 IS this function already — it just isn't named yet.
    fn module_paths(&self, module: &str) -> Vec<String>;
    /// Roots to search beyond the workspace (@INC / .libPaths() /
    /// site-packages). Default: workspace-only.
    fn library_roots(&self) -> Vec<PathBuf> { vec![] }
}

pub struct LanguageRegistry { drivers: Vec<Arc<dyn LanguageDriver>> }
impl LanguageRegistry {
    pub fn for_path(&self, path: &Path) -> Option<Arc<dyn LanguageDriver>>;
    pub fn for_language_id(&self, id: &str) -> Option<Arc<dyn LanguageDriver>>;
    pub fn watcher_globs(&self) -> Vec<String>; // union over drivers
}
```

The pack driver is generic over `LangPack`:

```rust
pub struct PackDriver { id: &'static str, exts: &'static [&'static str],
                        language: tree_sitter::Language, pack: LangPack }
impl LanguageDriver for PackDriver { /* extract + into_file_analysis */ }
```

Registering R is then a constructor call: grammar handle + the spike's
`r_pack()` + extensions. No new types per language.

## Touch points (the complete list, surveyed)

| site | today | change |
|---|---|---|
| `document.rs:25,79` | `create_parser()` + `builder::build` | `Document::new(text, driver)`; store the `Arc<dyn LanguageDriver>` on the Document; reparse uses it |
| `backend.rs` did_open/did_change | implicit Perl | route via `registry.for_language_id(params.language_id)` falling back to extension; unroutable files: don't attach (current behavior for non-Perl) |
| `backend.rs:332` watcher globs | hardcoded `**/*.pm`, `**/*.pl` | `registry.watcher_globs()` |
| `backend.rs:930` watcher re-index parse | inline `create_parser` | route by extension via registry |
| `module_resolver.rs:471` | `::`→`/` + `.pm` | `driver.module_paths(name)` — this is the spike predicate, landed |
| `module_resolver.rs:623` workspace walk | `types_builder.add("perl", "*.pm")` | one `add` per driver extension; per-file routing on the Rayon walk |
| `module_resolver.rs:700` registration name | `.pm`-derived package name | driver hook `module_name_for_path(path)` (Perl: first package decl, current logic; R: file name verbatim — matches `source()` keys) |
| `main.rs` `parse_file` / `--parse` | Perl parser | route by extension; `--parse` gains whatever the file is |

Nothing else. `symbols.rs`, `resolve.rs`, `file_store.rs`,
`module_index.rs`, `witnesses.rs`, the cache, and every LSP handler
are already language-free (the layering test is the proof that holds).

## The two real design decisions

**1. Module-name keyspace: per-language ModuleIndex.** Python's `os`,
R's `util.R`, and Perl's `POSIX` must not collide in one
`DashMap<String, _>`. Sharing one index with `(language, name)` keys
infects every existing call site; separate `ModuleIndex` instances per
driver infect none — handlers already receive `&ModuleIndex` from the
backend, so the backend holds `HashMap<&'static str, Arc<ModuleIndex>>`
and passes the document's. The SQLite cache gains a `language` column
(schema bump) or per-language db files under the existing hashed dir
(`modules-r.db`) — prefer the latter, zero migration.

**2. Cross-language ref pollution: a language tag on FileAnalysis.**
`refs_to` walks every workspace file; a Python `helper` must not match
an R `helper`. Add `#[serde(default = "perl")] language: String` to
FileAnalysis (EXTRACT_VERSION bump), stamp it in `analyze()`, and
filter in exactly two places: `refs_to`'s store walks and
`workspace/symbol`. `TargetRef` carries the origin language. This is
the ONE genuine engine edit in the program — a field and two filters.

## What pack languages get on day one vs later

Day one (free, proven by spikes): outline, document/workspace symbols,
goto-def, references, rename, cross-file via `module_paths`,
literal/shape/annotation typing, hover-on-type.

Later, per language, all pack-side: completion context
(`cursor_context` is Perl-only; pack languages start with
symbol-table completion, which `backend` can serve generically from
FileAnalysis — acceptable v1), diagnostics (start with NONE for pack
languages — honesty discipline: no calibration, no diagnostics),
framework plugins (requires keying rhai hooks on capture events — a
design round of its own, deferred).

## Calibration is the ship gate

A pack language ships when it has the gold-corpus sibling: a pinned
substrate (CRAN snapshot for R via `renv`, top-N packages), the
exact-assertion fixture format reused verbatim (`run.pl` is
language-agnostic — it shells the binary), and the zero-false-positive
sweep for any diagnostic before it exists. "Best in class" is this
harness, not the feature list. Budget it as half the work.

## Shipping shape: the engine as a product

Three pieces, phased so no crate is cut before shipping demands it
(consistent with the rejected workspace split — crates when a second
consumer exists, tests for layering until then):

**The core crate (`lsp-engine`).** `layering_tests::layer_map()` IS
the manifest: model (file_analysis, witnesses), the generic driver
(query_extract + capture vocabulary), cross-file (file_store, resolve,
module_index, module_cache), the rhai host, and the GENERIC half of
the LSP adapter. The honest cost: `symbols.rs` must split — the verbs
(documentSymbol/references/rename/workspace-symbol) are pure
FileAnalysis reads and belong to core; the intelligence (diagnostics,
import classification, hover rendering, cursor_context) is
language-flavored and moves driver-side. That disentanglement is the
biggest single line item, bigger than the routing work above.

**The middle: language packs as runtime artifacts (eventually).**
Every pack predicate written in the spikes (ctor_class, module_paths,
cmd_effects, annot_type, shape_ctor, import_call) is trivially rhai —
and the rhai host with fingerprinted cache invalidation already
ships. End state: a pack is a directory `{grammar, skeleton.scm,
predicates.rhai, pack.toml}`, installable like `.perl-lsp/` plugins.
The grammar is the only hard part: compiled-in (crate dep) first;
dynamically loaded (.so / wasm, the Helix/Zed model) later.

**The binary: one multiplexing server.** The registry serves N
languages from one process (LSP document selectors handle this).
perl-lsp doesn't die — it becomes the Perl-configured distribution of
that binary, name and install base intact.

Open design round carried from the spikes: keying framework-plugin
hooks on CAPTURE EVENTS the way rhai hooks key on CallContext today —
that is what gives pack languages a framework tier (tidyverse, CMake
module conventions). Everything else on this page is enumerable work.

## Sequencing

1. **Registry + PerlDriver** — pure refactor, behavior-identical,
   every existing test is the net. Land alone.
2. **FileAnalysis.language + the two filters** — EXTRACT_VERSION bump.
3. **PackDriver + R registration behind a flag** (`--lang r` or
   detect-and-serve) — floor verbs live in-editor.
4. **R substrate harness** — then, and only then, diagnostics and the
   ceiling work (S3 predicate, `$` drills, library_roots).

Steps 1–2 are days and de-risk everything; step 3 is the demo; step 4
is the product. The spike branch carries the working driver, packs,
and tests to lift from.

Then, and only then, the crate cut (shipping shape above): step 5 is
`lsp-engine` split along the layer-test seam with the
`workspace-split` branch as the mechanical playbook; step 6 is
runtime packs. Neither starts before a pack language has shipped
in-editor from the single crate.
