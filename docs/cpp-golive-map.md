# cpp go-live — the altitude map

The `spike/cpp-support` branch's big picture: where each piece sits relative to
the mission, so we don't lose the forest while zoomed into a slice. Status
markers are point-in-time; the *structure* is the durable part.

> **Mission:** go live with C/C++ support, via a hardened LanguagePack /
> query-engine seam. cpp-first; Python is a generality forcer (no hard DX
> runs); everything resolves via ref/edge, never a cursor-time shape pile.

```
ARC 1  cpp seam refactor ............................... ✅ DONE
       member-as-ref, Peel combinator, op-DX-on-ref, LangPack fold

ARC 2  Flow combinator / value-flow tier (FlowEdge spine) 🔵 IN PROGRESS
       A  @flow query mints FlowEdges in build() ........ ✅
       B  query lowers as fallback; retire manual assign_edge ✅
       C  list/destructuring onto the query ............. ✅
       D  array-source Sequence typing .................. ✅
       E  narrowing cutoff-on-edges ..................... ✅
          finding: cutoff needed — a narrowing is a SCOPED ASSERTION
            over a region, not a temporal value, so a later reassignment
            edge does NOT supersede it; it must be explicitly
            region-bounded. (Contrast slice B: an assignment TC IS a
            temporal value the edge reproduces, so that fixup was redundant.)
          LANDED: `cst::rebinds_scalar` deleted; cutoff is the shared
            `earliest_rebind_in`, edge-driven, consumed by Perl AND the
            query engine → cpp `dynamic_cast` + python `isinstance`
            narrowing both sound. (See the cross-language table below.)
       E0 binding-shape coverage ....................... ✅
          perl `my`/`local`/`foreach`, cpp range-for, python `for x in`
            all mint Rebind edges. (Cleared→Undef typing deferred to the
            narrowing tier — a clear is a region assertion, not a witness.)
       F  folded_from rename provenance ................. ⬜
       G  eager→edge single source ..................... ⬜

ARC 3  Perl-on-query-engine migration (builder.rs shrink) 🔵 fused with ARC 2
       the forcing function — each Flow slice ports a builder shape
       off hand-written CST walking onto declarative `.scm` capture

ARC 4  cpp LSP experience (was "residual cleanups") .. ⬜ NEXT
       TWO layers — see docs/cpp-lsp-experience-research.md:
       (a) PLUMBING: `==perl`→capabilities (un-gate diagnostics
           `backend.rs:198`, file-watch globs `backend.rs:601`) — cpp
           can't SHOW a diagnostic until this lands. Prerequisite.
       (b) DIFFERENTIATORS (flow-aware, where we beat clangd): optional/
           variant/null narrowing in hover+inlay, use-after-move
           diagnostic — both consume the narrowing/value-flow tier.
       Honest line: full overload-resolution lattice / template
       instantiation / ADL are compiler-grade, OUT of reach (no frontend).

ARC 5  SHIP cpp ...................................... ⬜ THE GOAL
```

## The load-bearing insight: the tier is SHARED, not Perl-specific

It is tempting to read the Flow/narrowing depth as Perl gold-plating. It is not.
The **primitive** (FlowEdge) and the **region machinery** (scoped-assertion
narrowing + the rebind cutoff) are language-agnostic seam; only the *surface
shapes* are per-language:

| concept            | Perl                         | C++                                   | Python                    |
|--------------------|------------------------------|---------------------------------------|---------------------------|
| value flow (edge)  | `my ($a,$b)=…`, `$x=…`       | `auto [a,b]=…`, structured bindings   | `a, b = …`, walrus `:=`   |
| bind / clear       | bare `my`/`local` → undef    | `T x;` default-init, `std::move` from | `x: T` annotation, `del`  |
| narrowing guard    | `defined`, `ref`, `blessed`  | `dynamic_cast`, `if(opt)`/`has_value`, `holds_alternative`/`get_if`, null-check | `isinstance`, `if x is not None`, `assert` |
| escape (future)    | pass into unstable sub       | pass to `&`/by-ref, `std::move`       | pass into mutating call   |

C++ **does** have runtime type inspection (RTTI / `dynamic_cast` / `typeid`,
`std::variant`, `std::optional`, `std::any`, null pointers) — so narrowing is a
first-class cpp feature, not a Perl quirk. Every tier we build should be
**exercised across perl + cpp + python** (the generality forcer): the FlowEdge
spine, the bind shapes, the narrowing guards. If a tier only works for Perl,
the seam isn't actually generic yet.

### Cross-language narrowing/bind — LANDED

The narrowing tier is now genuinely cross-language: one shared cutoff
(`file_analysis::earliest_rebind_in`, edge-driven) consumed by both the Perl
builder AND the query engine. The grammar scan (`cst::rebinds_scalar`) is gone.

| language | `@flow` assign/decl | bind shapes (rebind) | `narrow_guard` | cutoff |
|----------|---------------------|----------------------|----------------|--------|
| perl     | ✅                  | ✅ `my`/`local`/`foreach` | ✅ defined/ref/blessed | ✅ edges |
| cpp      | ✅                  | ✅ range-for (`std::move` ⬜) | ✅ `dynamic_cast` (`opt`/`variant` ⬜) | ✅ edges |
| python   | ✅                  | ✅ `for x in` (`del`/annot ⬜) | ✅ `isinstance` | ✅ edges |

Remaining long-tail (additive, each a `.scm` pattern + maybe a `narrow_guard`
arm; the cutoff already reads whatever edges they mint):
- cpp `std::move(x)` rebind, structured bindings `auto [a,b] = …`.
- cpp `narrow_guard`: `if(opt)`/`has_value()` → engaged, `holds_alternative<T>`
  / `get_if<T>` → `T` (needs an optional/variant lattice — a real type-model
  step, not just a wire).
- python `del x` rebind, walrus `:=` already flows.

## On-target discipline

- ARC 2/3 (Flow spine + Perl-on-engine) **hardens the seam** — shared, cpp
  benefits (escape analysis, member flows, narrowing are FlowEdge/region
  consumers). Legitimate per the "Perl-on-engine as forcing function" goal.
- ARC 4/5 (cpp residuals + ship) is the **actual go-live critical path** and is
  still untouched. When the goal shifts from "prove the seam correct/clean" to
  "ship cpp," pivot there.

Both are valid; this file exists so the choice between them stays *explicit*
instead of drifting.
