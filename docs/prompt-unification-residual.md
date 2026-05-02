# Unification: Residual Forward Work

> Phases 2–4 landed (see `adr/file-store-and-resolve.md`). This is what's
> left.

## Phase 1 — `Namespace` enum [keystone, deferred]

Today's `package_parents: HashMap<String, Vec<String>>` represents
inheritance between Perl packages. It does not handle cross-framework
scopes:

- A Mojo app's helpers conceptually live in the app's namespace, not any
  single Perl package.
- A plugin's helpers register into the host app's namespace at load time,
  not the plugin's package.
- Route actions live in an action-specific scope inheriting from the
  controller, the app, and any `under()`-chain actions.
- Stash keys are scoped per-action with parent edges from `under()`.

```rust
enum Namespace {
    Package(String),
    MojoApp(String),
    MojoPlugin(String),
    MojoController(String),
    MojoAction { controller: String, action: String },
    LiteApp(PathBuf),
    // pluggability TBD
}
```

`package_parents` → `namespace_parents: HashMap<Namespace, Vec<Namespace>>`.
~45 call sites. The `resolve_method_in_ancestors` walker accepts
`&Namespace`. SQLite stays TEXT via canonical `Display`/`FromStr`.

### The pluggability question (the real blocker)

- **Enum-only.** Exhaustive match guarantees no forgotten branches; adding
  a framework = recompile.
- **`Custom { kind: String, name: String }` catch-all.** Open enrollment
  for plugins; loses exhaustiveness.
- **Hybrid.** Known frameworks get enum variants; `Custom { kind, name }`
  covers the long tail. Best of both — exhaustiveness on hot paths, open
  enrollment for the rest.

Decision still pending. Phases 6 and 7 block on this.

## Phase 5 — Eager `Ref.target`

Today `Ref.resolves_to: Option<SymbolId>`. Some refs resolve at build, some
lazily inside queries (which is why `collect_refs_for_target` needs tree +
source — the lazy resolution path).

**Target shape:**

- `Ref.target: SymbolId` (non-optional).
- Reserved sentinel `UNRESOLVED: SymbolId = SymbolId(u32::MAX)` for refs
  whose referent genuinely can't be determined at build. `target_name`
  preserved for diagnostic emission + deferred cross-file resolution.
- Builder post-pass runs full resolution before returning. Hash-key
  ownership, intra-file method-call resolution, import-based resolution
  all happen here. `debug_assert!` no `INVALID` after the pass.
- Cross-file resolution happens in `FileStore::enrich()` and updates
  `refs_by_target` accordingly.

**Already in:** `refs_by_target` exists and is built. Only the eager-
target invariant is missing — `Ref.resolves_to` is still `Option`.

## Phase 6 — `Openness` classification [blocks on phase 1]

Unresolved-call diagnostics today either fire too eagerly (inside roles,
plugins, abstract bases) or miss real bugs (silently skip framework-
imported names). One unified rule:

```rust
enum Openness { Closed, Open }
```

Derived per namespace, never stored as ground truth:

- `use Moo::Role` / `Moose::Role` / `Role::Tiny` → Open
- `use Mojo::Base 'Mojolicious::Plugin'` (plugin shape) → Open
- `use Mojo::Base 'Mojolicious'` + `sub startup` → Closed
- Controllers with routes targeting them → Closed
- Referenced as parent but never instantiated → Open
- `.pl`, `.t`, scripts → Closed
- Default: Closed

**Diagnostic rule:** walk the namespace chain from the ref's site upward
via `namespace_parents`. If it terminates `Closed` without resolving
(via `refs_by_target[UNRESOLVED]` membership) → warn. If it hits `Open`
first → suppress.

Covers unresolved function call, unresolved method call (in role bodies),
unresolved stash key, unresolved route target, unresolved helper/hook.
Replaces the bespoke `framework_imports` suppression.

## Phase 7 — Framework emission into framework Namespaces [blocks on phases 1, 5]

Original phase 7 was "decompose `builder.rs` into `builder_framework.rs` +
`builder_dbic.rs` + `builder_mojo.rs`, each emitting into framework
Namespaces." For Mojo, the plugin path (`frameworks/mojo-*.rhai` +
`PluginNamespace` + `Bridge`) won and shipped — that part is done in a
different shape (see `adr/plugin-system.md`).

What's still useful from the original phase 7 idea:

- **Move `Symbol.package: Option<String>` → `Symbol.home_namespace:
  Namespace`.** Once phase 1 lands, every Symbol carries a Namespace
  rather than a string. The `Bridge::Class(...)` plugin path becomes
  `Bridge::Namespace(MojoApp(...))` etc. — bridges target framework
  Namespaces directly instead of being routed through a controller class.
- **DBIC + native Moo/Moose/Mojo::Base synthesis** still lives in
  `builder.rs` (not plugin-driven). Whether that should move to
  `builder_framework.rs` / `builder_dbic.rs` is a code-organization call,
  not an architecture one. Worth doing when `builder.rs` size becomes
  painful, not before.

For Mojo features still missing (stash keys, hook completion, route
naming + url_for, plugin chain transitive helpers), see
`prompt-mojo-todo.md`. They land as plugin patches, not phase-7 work.

## Open questions

- Pluggability for `Namespace` (above).
- `bincode` vs `postcard` for SQLite — bincode + zstd is current. Switch
  if cache size becomes painful.
- Tree-sitter serde compatibility — confirm `tree_sitter::Point` /
  `tree_sitter::Range` round-trip cleanly, or wrap.

## Non-goals

- Re-parsing on demand for deps. `FileAnalysis` round-trips; tree is not
  serialized. Features needing the tree re-parse on demand (already how
  `cursor_context.rs` works for open files).
- Dynamic Namespace extension at runtime by third-party plugins —
  depends on the phase-1 pluggability decision.
- Cross-file rename of deps. Deps are read-only; rename stops at `role:
  Dependency` (already enforced via `RoleMask::EDITABLE`).
- Cross-workspace symbol graph (monorepo multi-root) — single-workspace
  only.
