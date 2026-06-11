# QA design items — punted, need design before code

> The sprint landed everything else from this file (NAV, A4, E2, D1,
> B-cluster); full write-ups in git history (qa-design-items.md @
> 9d34441). What remains are the two punted designs below plus
> MooseX::Role::Parameterized support (no design yet — parameterized
> role generators are the runtime-export-generator open problem wearing
> role clothes). NARROW-1 (flow narrowing) moved to the roadmap's
> type-inference queue.

## MAIN-1 — `main::` aggregation across `require` of package-less scripts

**Problem.** Legacy CGI (AWStats) `require`s package-less `.pm`/plugin files
into the running script; with no `package` statement, every sub in every
such file lands in `main::`. The host calls plugin subs and the plugins call
host subs — all in `main` at runtime — but each file is analyzed in
isolation, so cross-file `main::` symbols never unify. ~270 FPs in both
directions (host→plugin and plugin→host) in `awstats.pl` and its
`require "$pluginpath"` plugins.

**Why it resists a local fix.** Cross-file resolution keys on a *named*
package (`package_parents`, the module index's module→file map, the reverse
index). `main` is the implicit, unnamed package: there's no `package main;`
to anchor a module name, and many unrelated scripts each define their own
`main::` subs, so naïvely unifying all `main` symbols workspace-wide would
cross-link genuinely unrelated files (every `t/*.t` has its own `main`). The
real edge is **`require`-induced**: file A `require`s file B, so B's `main`
subs are visible in A. That's a *file-level dependency edge* the engine
doesn't model — distinct from `@ISA` (it's not inheritance) and from
`use`-import (no export list; everything in `main` is just visible). Modeling
it wrong (union all `main`) is worse than the FP.

**Options.**
1. **Model the `require`-dependency edge.** When a file statically `require`s
   another (literal path, or a resolvable `$var` whose value is a constant
   path), add a directed edge A→B; resolve unqualified calls in A against
   B's `main::` subs along that edge (bounded, seen-set). Only files actually
   reachable via a require edge unify — unrelated `main`s stay separate.
   The hard part is the dynamic `require "$pluginpath"` (path from config);
   degrade silently when the path isn't statically knowable.
2. **`.perl-lsp` manifest of require roots** — let the project declare which
   files aggregate into the script's `main`. Correct, but config most legacy
   projects won't write.
3. Leave deferred — honest; MAIN-1 stays a legacy-CGI-specific standing FP.

**Recommendation.** Option 1 *if* legacy-CGI support is in scope, gated on
statically-resolvable require paths (the AWStats `require "$file"` where
`$file` traces to a constant). Otherwise option 3 — this is a narrow,
legacy-CGI-specific shape (modern code uses packages), and the dynamic-path
require defeats static analysis anyway. Judgment call on whether the corpus
weight justifies the new dependency-edge axis.

---

## H1 — duplicate-package resolution (two files `package Foo;` — which wins?)

**Problem.** Two files declare `package Bugzilla;`
(`contrib/Bugzilla.pm` shadows the root `Bugzilla.pm`); the resolver picks
the wrong one, breaking the singleton's type inference and exports.

**Why it needs design.** "Which file owns `package Foo`?" has no
ground-truth in static analysis — at runtime `@INC` order decides, and the
LSP has no single `@INC`. A heuristic is unavoidable, but it must be
*principled and stable* (rule #10: not "is this path `contrib/`"). The
choice interacts with B4 (a shadowed package exports the wrong `@EXPORT`)
and with workspace-vs-@INC priority (CLAUDE.md: documents → workspace_index
→ module_index already encodes a priority; duplicates *within* a tier are
the gap).

**Options.**
1. **Path-distance / role ranking.** Prefer the file whose path best matches
   the package name (`Bugzilla::Foo` → `lib/Bugzilla/Foo.pm`), then prefer
   `lib/` over `t/`, `contrib/`, `xt/`, `examples/` via a *role* ranking
   (test/contrib/example dirs deprioritized) — but encoded as a ranked
   `FileRole`, not an inline `if path.contains("contrib")` at the resolution
   site. The role is computed once at index time and carried on the entry.
2. Honor a real `@INC` / `.perl-lsp` config order when present — correct but
   requires config most projects won't have.

**Recommendation.** Option 1 with the rank as a typed `FileRole` on the
indexed entry (so the resolver asks the entry "are you canonical?" and the
entry answers — no path-string branch in the resolver). Make `@INC`/config
(option 2) an override when available. Decide this *before* B4: B4 may be
H1 in disguise (the shadow exports the wrong surface), so the duplicate
resolution must land first to test B4 cleanly.

---
