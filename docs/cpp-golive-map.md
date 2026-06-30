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
       E  narrowing cutoff-on-edges ..................... 🔵
          finding: cutoff needed — a narrowing is a SCOPED
            ASSERTION over a region, not a temporal value, so a
            later reassignment edge does NOT supersede it; it must
            be explicitly region-bounded. (Contrast slice B: an
            assignment TC IS a temporal value the edge reproduces,
            so that fixup was redundant.)
          prerequisite: COMPLETE bind coverage (below) — else the
            edge-driven cutoff silently under-truncates and narrowing
            leaks past unrecorded rebinds.
       E0 binding-shape coverage ....................... 🔵 ← current
          every rebind shape mints a FlowEdge: bare `my`/`local`
            (→ Cleared/Undef), `foreach` var (element flow), `our`
            (rebind-only), lvalue-sub. Then cutoff = "earliest edge
            targeting $x in region", and `cst::rebinds_scalar` deletes.
       F  folded_from rename provenance ................. ⬜
       G  eager→edge single source ..................... ⬜

ARC 3  Perl-on-query-engine migration (builder.rs shrink) 🔵 fused with ARC 2
       the forcing function — each Flow slice ports a builder shape
       off hand-written CST walking onto declarative `.scm` capture

ARC 4  cpp residual cleanups .......................... ⬜ DEFERRED
       layering teeth, `==perl`→capabilities, go-live checklist

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

### Cross-language narrowing/bind backlog (the generality steps)

Current state (verified): the `narrow_guard` LangPack hook spans languages, but
coverage is uneven, and the **rebind cutoff has no cross-language consumer yet**
— it lives in the Perl builder (`first_subject_write`), not the query engine.

| language | `@flow` assign/decl | bind shapes (rebind) | `narrow_guard` |
|----------|---------------------|----------------------|----------------|
| perl     | ✅                  | ✅ `my`/`local`/`foreach` → Rebind | ✅ defined/ref/blessed |
| cpp      | ✅                  | ⬜ range-for, `std::move`-from     | ⬜ STUBBED (`\|_,_\| None`) — wire `dynamic_cast`/`if(opt)`/`holds_alternative` |
| python   | ✅                  | ⬜ `for x in`, `x: T`, `del x`     | ✅ `isinstance` |

The steps, in dependency order — DON'T mint cpp/python rebinds before there's a
reader, or they're edges into a void:

1. **Generic narrowing-cutoff (ARC 2/E):** lift `first_subject_write` off the
   Perl CST scan onto a FlowEdge query (`earliest edge targeting $x in region`).
   This is what makes rebinds *consumable* by any language — the cutoff becomes
   language-agnostic the moment it reads edges instead of `cst::rebinds_scalar`.
2. **Per-language bind captures** (once #1 lands): cpp range-for /
   structured-binding / `std::move`; python `for`/annotation/`del` → `Rebind`
   FlowEdges via each skeleton + the shared `query_extract` minter. (cpp `T x;`
   is NOT a clear — it's a typed default-init via `@type.annot`.)
3. **cpp `narrow_guard` wiring:** `dynamic_cast<T*>` → `T*`, `if(opt)` /
   `has_value()` → engaged, `holds_alternative<T>`/`get_if<T>` → `T`. The hook
   exists; cpp just returns `None` today.

## On-target discipline

- ARC 2/3 (Flow spine + Perl-on-engine) **hardens the seam** — shared, cpp
  benefits (escape analysis, member flows, narrowing are FlowEdge/region
  consumers). Legitimate per the "Perl-on-engine as forcing function" goal.
- ARC 4/5 (cpp residuals + ship) is the **actual go-live critical path** and is
  still untouched. When the goal shifts from "prove the seam correct/clean" to
  "ship cpp," pivot there.

Both are valid; this file exists so the choice between them stays *explicit*
instead of drifting.
