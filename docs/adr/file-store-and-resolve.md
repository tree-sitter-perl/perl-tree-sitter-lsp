# ADR: One file store, role-tagged. One resolve(). One refs_to().

The LSP used to have three parallel stores (`Backend.documents`,
`Backend.workspace_index`, `ModuleIndex.cache`) and per-query tier walks
that each independently picked which subset to consult. Each query was a
place where a tier could be forgotten — the source of "find-references
mysteriously misses dep call sites" type bugs.

`@INC` modules also stored a lossy `ModuleExports` summary (no refs, no
type constraints, no call bindings). Cross-file features degraded at the
module boundary even when both files had been parsed.

## Decisions worth keeping

### Full `FileAnalysis` everywhere

`ModuleExports` is dead. Workspace files, dep modules, and open files all
store an `Arc<FileAnalysis>`. The SQLite cache (`module_cache.rs`,
schema v9) stores `zstd(bincode(FileAnalysis))` — full fidelity round-
trips through the cache. Cross-file refs, type constraints, call bindings,
imports all survive the module boundary.

When you need data from another file: get the file's `FileAnalysis` and
read the field. No "summary type" detour.

### Single store, role-tagged: `FileStore`

```rust
pub enum FileRole { Open, Workspace, Dependency, BuiltIn }
```

`src/file_store.rs` holds every parsed file. Open files have `role: Open`,
indexed by path + url. A workspace file that becomes open flips role; the
`Arc<FileEntry>` is replaced (cheap; readers hold Arcs and see consistent
snapshots).

Rule: **don't add a fourth store**. New file sources (REPL buffers, virtual
docs, whatever) become a new `FileRole`, not a new map.

### Role mask: explicit at every call site

```rust
bitflags::bitflags! {
    pub struct RoleMask: u8 {
        const OPEN       = 1 << 0;
        const WORKSPACE  = 1 << 1;
        const DEPENDENCY = 1 << 2;
        const BUILTIN    = 1 << 3;
        const EDITABLE   = OPEN | WORKSPACE;       // rename stops here
        const VISIBLE    = EDITABLE | DEPENDENCY | BUILTIN;
    }
}
```

Every cross-file query passes a mask. Rename uses `EDITABLE` (deps are
read-only). References uses `VISIBLE` (yes, search deps too). Diagnostics
use `VISIBLE`. Forgetting a tier is now visible at the type level — you
have to write the mask down.

### Single `resolve_symbol` + `refs_to`

`src/resolve.rs` is the only place tier-walking lives. LSP handlers do not
iterate `FileStore` directly. Adding a new query = picking a mask and
calling `resolve_symbol` / `refs_to`.

The walk: start at the `from` namespace, DFS through `package_parents`
(soon: `namespace_parents`), filter file entries by mask. `refs_to` reads
each file's `refs_by_target` for O(1) per-file lookup.

### Lazy enrichment, idempotent

Workspace files aren't enriched at index time — only on-demand when a query
needs cross-file types. `base_type_constraint_count` / `base_symbol_count`
/ `base_witness_count` mark the post-build seal so re-enrichment truncates
back to baseline before re-deriving. The `enriched: bool` flag on
`FileEntry` short-circuits repeat work.

## Where this is going

Forward residual lives in `prompt-unification-residual.md`:

- Phase 1: `Namespace` enum (deferred — pluggability question gates phases
  6+7).
- Phase 5: eager `Ref.target: SymbolId` (not `Option`); the field is still
  `Option<SymbolId>` in `Ref.resolves_to`.
- Phase 6: `Openness` classification + unified diagnostic rule.
- Phase 7: framework emission rules into framework Namespaces (the Mojo
  intelligence path).

CLAUDE.md "Cross-file resolution" + "Cross-file enrichment" sections
describe the live behavior in operational detail.
