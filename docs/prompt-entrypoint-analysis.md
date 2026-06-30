# Entrypoint / program-boundary analysis

**Status: partial (a conservative fallback is landed); the real analysis is
forward work.**

A workspace can hold more than one independent *program*: several entrypoint
scripts (`bin/*`, `script/*`, extensionless shebang files), or several apps
(multiple Mojo::Lite apps). Each program is its own runtime. The analyzer has no
notion of "which program does this file belong to," so it can't tell whether two
files share a namespace *because they're one program* or merely *because the
namespace is global by spelling*. Two distinct concerns fall out of this one gap.

## The concern, two faces

1. **`main::` globals across unrelated scripts.** Every package-less script puts
   its `our $x` in `main::x`. Two unrelated scripts both land there, but they are
   NOT the same variable. A cross-file rename keyed on the global name would
   rewrite one script's `$x` from another's.

   *Landed fallback* (`resolve.rs`, the `package == "main"` arm of
   `resolve_symbol_scoped`): a `main` global resolves **file-local** — its rename
   group is just this file's spellings (`our` decl + bare reads + `$main::x` /
   `$::x`). A real package (`Foo::x`) still fans out cross-file. This is
   deliberately conservative: it under-reaches (a genuinely-shared `main::`
   helper across a program's files won't fan out) rather than over-reaching
   (renaming an unrelated script's global).

2. **Multi-app / per-instance dispatch.** Two Mojo::Lite apps in one workspace
   shouldn't see each other's helpers; `$app->minion` and `$app->other_minion`
   are different queues. This is the **instance-brand** problem, parked behind the
   long-distance value-provenance tier (`docs/prompt-graph-walking.md`). Same
   root: "which program/instance does this receiver belong to."

## The forward work

Assign each file to the program(s) it participates in — an **entrypoint set** —
so namespace resolution scopes per-program instead of per-spelling:

- a `main::` global fans out across exactly the files of *its* program, and stays
  out of other programs' `main::`;
- per-app helper/instance dispatch (the instance brands) keys on the program, not
  a syntactic accessor name.

Entrypoint discovery already has a seam: `scan_entrypoint_scripts` (the shallow
shebang scan over root + `bin/` + `script/`, with the `extra: &[String]` param
reserved for a future workspace-config `entrypoint_dirs`). What's missing is the
**file→program assignment**: walk each entrypoint's `use`/`require`/`do` closure
to the set of files it pulls in, so a file knows its program(s). A file reachable
from two entrypoints belongs to both; a library `use`d everywhere belongs to all.

Once a file knows its program, the `resolve.rs` `main`-global fallback lifts (fan
out within the program), and the instance-brand work gets its scoping key.

## Why it's parked

Cross-cutting (touches workspace indexing, resolve, and the plugin instance
model) and not blocking any landed feature — the conservative fallbacks are
correct, just narrow. Sequenced after the value-provenance tier that the
instance-brand half depends on.
