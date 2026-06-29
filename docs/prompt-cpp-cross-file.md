# C++ cross-file resolution — locked design, ready to execute

## Why it's not there yet
C++ was built single-file-first. The cross-file *query* machinery is
already cross-file-capable (`MethodOnClass` recurses into a cached
module's bag via `CrossFileLookup::get_cached`; `refs_to` walks the
`FileStore` + `ModuleIndex`). What's missing is the **workspace indexer**
for C++ — the analog of `module_resolver`'s Perl walk, which is
type-pruned to `*.pm/*.pl/*.t`, so C++ headers are never analyzed or
registered. Nothing answers "class `Box` is defined in `box.h`."

## The decided design: per-language index + per-language DB
**No comingling — separate `ModuleIndex` instances and separate cache
files per language.** A Perl package `Box` and a C++ class `Box` in one
monorepo must never share a key (in-memory `cache` OR the SQLite
`modules` table). Because `get_cached(name)` is bare-name-keyed and
language-agnostic, the partition is at the INDEX level, not the key:

1. **`open_cache_db(workspace_root, lang)`** → `modules.db` for Perl
   (back-compat), `modules-{lang}.db` otherwise. Physically separate `.db`
   files; pack languages never touch the Perl cache file.
2. **A C++ `ModuleIndex` instance**, distinct from Perl's. `ModuleIndex`
   already implements `CrossFileLookup`, so a separate instance is
   naturally scoped — no wrapper, no shared map. Keying C++ classes by
   bare name is fine *within* its own index (the cross-language collision
   only exists across instances, which don't share storage).
3. **`ModuleIndex::register_cpp_classes(path, analysis)`** (landed) — a
   C++ header holds many classes, so register the FileAnalysis under EACH
   `Class` symbol's name (Perl registers one package/file).
4. **A C++ workspace indexer** — walk `*.h/*.hpp/*.cc/*.cpp/...` (the
   registry's pack extensions), `driver.analyze_with_path` each (so
   cross-file macros resolve), `register_cpp_classes` into the C++ index.
   Mirror `index_workspace_with_index`'s Rayon walk + `catch_unwind`.
5. **Per-language routing**: `cli_full_startup` returns the C++ index
   alongside Perl's; `run_one` and the LSP backend pick the index by the
   queried file's language (`reg.for_path(file).id()`), passing it as the
   `CrossFileLookup`. Perl files keep using Perl's index.
6. **Persistence**: the C++ indexer writes analyzed headers to
   `modules-cpp.db` and warms from it at startup (mirror `warm_cache`),
   keyed by class name — so a big monorepo doesn't re-analyze every header
   each launch. Separate DB ⇒ zero overlap with Perl.

## What flows through once this lands
With the C++ index populated, the EXISTING machinery resolves across
files with no new query code:
- method-return chains where the receiver's class is in another header
  (`MethodOnClass` → `get_cached(class)` → the other file's bag),
- member completion of a class defined elsewhere,
- goto-def / references into another header,
- inheritance where the base class is in another file.

## Risk / staging
Touches `ModuleIndex`, `module_resolver`, `module_cache`, and the
CLI/backend query routing — core Perl infrastructure. Do it as a focused
push with the Perl gold + e2e as the regression net (Perl behavior must
stay byte-identical: the C++ index is purely additive). Suggested order:
(1) `open_cache_db(lang)` seam, (2) C++ index + indexer (in-memory) +
CLI routing → test cross-file via `--completion`, (3) LSP backend
routing, (4) `modules-cpp.db` persistence + warm.
