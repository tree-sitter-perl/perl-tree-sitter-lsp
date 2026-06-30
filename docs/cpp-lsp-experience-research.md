# A Full C/C++ LSP Experience — Market Survey, Deep-Semantics Frontier, and a Flow-Aware Differentiation Strategy

> Strategic frame: this engine is a **lighter, flow-aware, multi-language** tree-sitter
> server, not a compiler frontend. The question is not "can we beat clangd at being clangd"
> (we cannot — clangd *is* the compiler), but **where does a fast, flow-sensitive, declarative
> engine deliver an experience clangd structurally does not** — and which features are
> table-stakes we must reach "good enough" on so the tool doesn't feel broken.

---

## Executive summary

The dominant C/C++ language server is **clangd** (LLVM/Clang frontend); the other real
players are **ccls** (also libclang, index-heavy), **Microsoft's C/C++ extension / IntelliSense**
(closed, EDG-based), and **JetBrains CLion** (a *dual engine*: its own resolver + a bundled
clangd). They are all, at heart, **compiler frontends**: they are accurate because they
actually parse, preprocess, and type-check the whole translation unit (TU). That accuracy is
also the source of every documented pain point.

**The four pains people actually feel with clangd:**

1. **Setup friction** — everything hinges on a correct `compile_commands.json`; without it,
   headers don't resolve, the standard library isn't found, and the whole experience collapses
   to "red squiggles everywhere." This is the #1 reason people are *underwhelmed*.
2. **Cost** — multi-GB RAM and multi-minute (Chrome-scale: multi-hour) indexing; opening a
   handful of heavy-template TUs can blow past 16 GB.
3. **Template / macro opacity** — references *inside* template instantiations are disabled for
   performance; dependent types and `auto`-in-templates frequently don't resolve on hover;
   macro-heavy code degrades navigation.
4. **Latency on open** — heavy headers mean 30s–1min before a freshly opened TU is fully live;
   to stay responsive clangd *skips* parsing header-defined function bodies, producing false
   positives.

**Where a flow-aware tree-sitter engine can genuinely win (differentiators):**

- **Zero-config, instant**: no `compile_commands.json`, no preprocessor, no index wait. Works
  on a single file, a partial checkout, a header with no TU. This alone answers pains #1, #2, #4.
- **Flow-sensitive value narrowing as a first-class, *navigable* feature** — `std::optional`
  engaged-state, `std::variant` active-alternative, `dynamic_cast` refinement, null-flow,
  **use-after-move** — surfaced as *hover/inlay/diagnostic*, reusing the **FlowEdge + scoped-
  assertion narrowing + rebind-cutoff** tier already built. Clang ships these only as separate,
  *expensive, opt-in* `clang-tidy` checks (`bugprone-unchecked-optional-access`,
  `bugprone-use-after-move`) — never as inline editor intelligence.
- **Multi-language uniformity & speed** — one engine, same UX across C, C++, Perl, Python; sub-
  second on files clangd needs a full TU parse for.

**The honest out-of-reach line** (needs a real frontend; do NOT promise):

- **Full overload resolution** (viable-candidate ranking via implicit conversion sequences,
  SFINAE, partial ordering of templates).
- **Template instantiation** (the actual type of `container[i]` where `container` is
  `vector<vector<T>>`; concept satisfaction; dependent-name lookup).
- **Accurate ADL** and **full preprocessor-correct** macro expansion / conditional compilation.
- TU-accurate diagnostics. We will be *approximate* here; pretending otherwise is the one way
  to lose trust.

**Recommended first three to build** (detail in the roadmap): (1) **document/workspace symbols +
goto-def/refs/hover that don't lie on the common path** — the broken-feeling baseline; (2)
**`std::optional`/`std::variant`/pointer null narrowing surfaced in hover + inlay** — our
flagship flow differentiator, mostly a `narrow_guard` lattice on the existing tier; (3)
**use-after-move flow diagnostic** — high-signal, directly a FlowEdge + rebind-cutoff consumer,
and something clangd does not do inline.

---

## 1. The market

| Tool | Architecture | Strengths | Documented pain |
|------|-------------|-----------|-----------------|
| **clangd** (LLVM) | Clang **compiler frontend** + background **index** (per-TU AST, persisted) | Compiler-accurate; fast goto/refs once indexed; include-cleaner; inlay hints; call & type hierarchy; strong diagnostics & quick-fixes | `compile_commands.json` dependency; multi-GB RAM; long index; refs disabled inside template instantiations; macro/template opacity; open-file latency; header bodies skipped → false positives |
| **ccls** | libclang-based, **index-centric** (heavy persisted DB) | Robust cross-references at scale; mature; low steady-state cost after index | Same compiler-DB dependency; slower-moving; clangd has caught up and surpassed it for most |
| **MS C/C++ ext (IntelliSense)** | Closed-source, **EDG** front-end ("Edison Design Group") | Bundles the **debugger** (the reason to keep it); decent completion; configurable include paths without a compile DB | Slower navigation than clangd ("a second or two" vs near-instant); closed; less frequent improvement |
| **CLion** | **Dual engine** — JetBrains' own C++ resolver **+** bundled clangd in parallel | Best-in-class refactoring/inspections; detailed overload-resolution *error* explanations; C++20 Concepts UI; navigation merges both engines | Heavyweight IDE; CMake/compile-DB project model still required; commercial |
| **cquery** (historical) | libclang index server; clangd/ccls's predecessor | Pioneered LSP-for-C++ at scale | **Abandoned**; superseded by ccls then clangd |

Sources: clangd architecture & index — <https://clangd.llvm.org/design/indexing>, <https://clangd.llvm.org/design/code>;
state-of-the-field overview (clangd/ccls/cquery lineage, libclang basis) — <https://medium.com/@dougschaefer/current-state-of-c-c-language-servers-ab87e6fc186b>;
clangd vs MS IntelliSense (closed, EDG, speed) — <https://goodbyte.software/how-about-clangd-instead-of-intelllisense/>, <https://news.ycombinator.com/item?id=43788332>, <https://www.kdab.com/supercharging-vs-code-with-c-extensions/>;
CLion dual engine — <https://www.jetbrains.com/help/clion/c-support.html>, <https://www.jetbrains.com/help/clion/settings-languages-cpp-clangd.html>.

### Why people are underwhelmed by clangd specifically

Real, documented complaints (the commissioner's "I think it's clangd and it disappoints me"):

- **Indexing is slow and huge.** Global index can take *hours* on Chrome-sized projects;
  opening a new `.cpp` can take **30s–1min** before highlight/jump work.
  <https://github.com/clangd/clangd/issues/2355>, <https://clangd.llvm.org/design/indexing>
- **Memory.** ~10 open files → **>16 GB** in some projects; heavy-template / C++23 code is worse;
  the official mitigation is to offload to a **remote index server**.
  <https://github.com/clangd/clangd/discussions/2578>, <https://clangd.llvm.org/guides/remote-index>
- **Template references are intentionally crippled.** "References inside template instantiations
  are disabled in clangd for performance reasons."
  <https://clangd.llvm.org/design/indexing>; template/dependent-type quality is an open,
  long-standing gap — <https://github.com/clangd/clangd/issues/1095>,
  <https://github.com/clangd/clangd/issues/443>, <https://github.com/clangd/clangd/issues/2047>
- **`auto` / decltype in templates often won't deduce on hover.**
  <https://github.com/clangd/clangd/issues/1015>, <https://github.com/clangd/clangd/issues/58>
- **Header-not-found / compile-DB hell.** clangd "might fail to find suitable compile flags for
  headers outside your project directory"; std-lib must be installed and findable; the fix is a
  zoo of flags (`--compile-commands-dir`, `--query-driver`) and wrapper tools (`bear -- make`).
  <https://clangd.llvm.org/troubleshooting>, <https://clangd.llvm.org/design/compile-commands>,
  <https://github.com/clangd/clangd/issues/695>, <https://discourse.nixos.org/t/clang-clang-and-clangd-cant-find-headers-even-with-compile-commands-json/54657>
- **Responsiveness trade-off causes false positives.** clangd "skips parsing the bodies of
  functions defined in included headers… which can result in false positive diagnostics."
  <https://clangd.llvm.org/faq>
- **Cross-file rename is index-plus-text-match**, historically flag-gated to a small symbol set —
  it is not as bulletproof as the in-TU case. <https://reviews.llvm.org/D96578>,
  <https://reviews.llvm.org/D69263>

**The throughline:** every pain is the *cost of compiler accuracy* — the full preprocess + parse
+ type-check + persisted index. A tree-sitter engine pays none of that toll, at the price of
*not being a type checker*. That trade is the entire strategy.

---

## 2. The full LSP feature surface for C/C++

### 2a. Standard LSP methods (the table-stakes plane)

These are language-neutral and the engine's LangPack seam already mints most of the underlying
refs/edges for Perl/Python; the question is C/C++ *coverage quality*, not novelty.

- Goto **definition / declaration / type-definition / implementation**
- **Hover** (declared type, signature, doc/comment)
- **References**, **document highlight**, **rename** (+ prepareRename)
- **Completion** (members, scope, includes), **signature help**
- **Diagnostics**
- **Semantic tokens**, **document symbols**, **workspace symbols**
- **Call hierarchy**, **type hierarchy** (clangd ships both — the type hierarchy = base/derived
  walk, which our `@parent` inheritance edges already model)
- **Inlay hints** (parameter names, deduced types, default args)
- **Code actions / quick-fixes**, **code lens**, **folding**, **selection range**,
  **linked editing** (rename-as-you-type matched tokens)

Reference for clangd's extension surface (inlay hints, type hierarchy, include-cleaner):
<https://clangd.llvm.org/extensions>, <https://clangd.llvm.org/guides/include-cleaner>,
<https://deepwiki.com/clangd/vscode-clangd/3.1-inlay-hints>.

### 2b. The C/C++-specific intelligence that separates great from mediocre

This is the "deep semantics" plane. Honest verdicts on each, against a tree-sitter+flow engine:

| Capability | What "great" looks like | Compiler-grade? | Tree-sitter+flow reach |
|---|---|---|---|
| **Overload resolution** | Show *which* overload a call binds to; the **candidate set**; *why* (viable set, ranking by implicit conversion sequence, template partial ordering, SFINAE-rejected) | **Yes — frontier of the frontend.** Clang has `OverloadCandidateSet` internally; CLion surfaces detailed *failure* reasons | **Mostly out of reach.** Can approximate *arity + obvious-type* disambiguation; cannot do ICS ranking / SFINAE. Be honest: show "N overloads" and best-effort arity pick, never claim THE answer |
| **Templates** | Instantiation-aware hover/goto; `container[i]` type when `container=vector<vector<T>>`; dependent-name resolution; **concept/constraint satisfaction** (C++20) | **Yes.** This is exactly what clangd disables for perf and what its open issues track | **Out of reach** for true instantiation. Can give the *template* declaration, the param names, the constraint *text*. Cannot give the instantiated type |
| **ADL** | Resolve unqualified calls through argument namespaces | **Yes** (needs full name lookup) | **Out of reach** accurately; can fall back to same-namespace + global heuristic |
| **Macro expansion** | Inline expansion preview on hover; goto *through* macros | **Yes** (needs the preprocessor) | **Largely out of reach** — we don't preprocess. Can goto the `#define` (we already mint macro symbols) and show the *raw* macro body, not the argument-substituted expansion |
| **Include management** | include-what-you-use; unused-include diagnostics; auto-add missing include | Partly compiler (IWYU needs the AST), partly structural | **Partial reach.** "Symbol X is used but no header providing it is included" is doable structurally with a workspace symbol→header index; *unused* requires knowing every symbol a header *really* provides (preprocessor) — approximate only |
| **`auto`/`decltype` display** | Show the deduced concrete type inline | Needs deduction (compiler) for the hard cases | **Partial.** For `auto x = make_widget();` we can chase the FlowEdge to the initializer's return type (we already do this for the cross-var chase). For `auto` params / template-dependent `auto`, out of reach |
| **const / ref / value-category** | Surface `const`, `&`/`&&`, lvalue/rvalue, "this binds a temporary" | Mostly structural (declarator) + some deduction | **Good reach.** The declarator carries cv/ref; we already peel pointer/reference stacks (`peel_nested`, `deref_stack`). Value-category of *expressions* needs more |
| **`std::optional`/`std::variant` engaged-state** | Narrow `opt` to "has value" after `if (opt)`/`has_value()`; `variant` to active alternative after `holds_alternative<T>`/`get_if<T>`; warn on unchecked access | clang-tidy does it via **expensive opt-in flow analysis** (`bugprone-unchecked-optional-access`) — **not** inline LSP | **STRONG reach — our flagship.** Exactly a `narrow_guard` arm + a small optional/variant lattice on the existing scoped-assertion + rebind-cutoff tier |
| **Nullability flow** | Narrow `T*` to non-null inside `if (p)`; warn on deref of known-null | clang static analyzer (path-sensitive, heavy) | **Good reach** (flow-sensitive, not path-sensitive — same class as clang-tidy's use-after-move) |
| **move-from / use-after-move** | Flag use of a variable after `std::move(x)`; clear the flag on reassignment | clang-tidy `bugprone-use-after-move` — flow-sensitive, opt-in, not inline | **STRONG reach.** A `std::move(x)` mints a Rebind (already noted as a `.scm` todo); the rebind-cutoff machinery already truncates a region at reassignment — *use between move and rebind* is the warning |
| **RAII / lifetime hints** | "this temporary's lifetime ends here"; dangling-reference hints | Full lifetime analysis (very heavy; clang's lifetime work is research-grade) | **Mostly out of reach**; narrow heuristics only |

Precedents that prove the flow capabilities are *known-valuable but only shipped as heavy opt-in
checks today* (i.e., the differentiation gap is real):
`bugprone-unchecked-optional-access` (flow-sensitive, "more resource intensive… RAM/CPU", branch-
state-aware) — <https://clang.llvm.org/extra/clang-tidy/checks/bugprone/unchecked-optional-access.html>;
`bugprone-use-after-move` (flow-sensitive not path-sensitive; clears on reinitialization; smart-
pointer aware) — <https://clang.llvm.org/extra/clang-tidy/checks/bugprone/use-after-move.html>.

#### On overload resolution specifically (the commissioner's explicit interest)

The ideal UX, drawn from how the compiler models it
(<https://en.cppreference.com/cpp/language/overload_resolution>,
<https://clang.llvm.org/doxygen/classclang_1_1OverloadCandidateSet.html>):

1. **Name lookup + ADL** → candidate set.
2. **Filter to viable** (an implicit conversion sequence exists for each argument).
3. **Rank** viable candidates (each arg's ICS no-worse + at-least-one-better; template partial
   ordering as tie-break; SFINAE removes non-substitutable templates).

A *great* tool would render this as an **"overload lattice"**: the candidate list, viable vs
rejected (with the rejection reason — "no conversion from `const char*` to `int`", "SFINAE'd
out"), and the winner highlighted. **No mainstream LSP renders the full lattice inline today**;
CLion comes closest by explaining overload-resolution *failures*
(<https://blog.jetbrains.com/clion/tag/overload-resolution/>). **This is genuinely
compiler-grade** — ICS ranking and SFINAE require real type-checking, so a tree-sitter engine
**cannot** be the source of truth here. The honest, valuable subset we *can* offer: list the
candidate overloads (we already index all declarations of a name), and make a **best-effort
arity + literal-type** guess, clearly marked as a guess. Promising the full lattice would be
the one feature most likely to destroy trust when wrong.

---

## 3. Differentiation for a flow-aware tree-sitter engine

The architecture note (`docs/cpp-golive-map.md`) already states the load-bearing insight: the
**FlowEdge primitive** and the **region machinery** (scoped-assertion narrowing + edge-driven
rebind cutoff, `earliest_rebind_in`) are **language-agnostic** and already shared across Perl /
C++ / Python. The cross-language narrowing table there explicitly lists the C++ guards we have
not yet wired: `if(opt)`/`has_value()` → engaged, `holds_alternative<T>`/`get_if<T>` → `T`,
`std::move(x)` rebind. **Those are the differentiators, and they are mostly *wiring on an
existing tier*, not new architecture.**

### Achievable AND differentiating (the "fast + flow-aware beats accurate + heavy" zone)

1. **Optional/variant/null engaged-state narrowing, surfaced in the editor.** clangd does *not*
   do this inline at all; clang-tidy does it as a slow opt-in check. We can do it *live, inline,
   zero-config* because it's flow-structural, not type-checked. → `narrow_guard` arm +
   optional/variant lattice; the cutoff already ends the region at any rebind.
2. **Use-after-move diagnostic.** Flow-sensitive (not path-sensitive) is *exactly* the class our
   tier already implements (and exactly clang-tidy's stated bound). `std::move(x)` is a Rebind;
   a read between the move and the next rebind is the warning. → already a noted `.scm` todo +
   a diagnostic consumer of the FlowEdge region.
3. **Zero-config instant startup.** No compile-DB, no preprocessor, no multi-GB index, no 30–60s
   open latency. Works on one file / partial checkout / a header with no TU. This is not a
   feature, it's a *category-level* advantage against every compiler-frontend tool.
4. **Uniform multi-language UX.** Same hover/goto/refs/narrowing across C, C++, Perl, Python from
   one engine and one capability contract.
5. **`auto x = expr();` deduced-type display via the edge chase.** For the common
   initialize-from-call case we already chase the FlowEdge to the initializer's type — clangd's
   own open issues show this is *not* reliably solved on their side for many cases.
6. **Member-access navigation through typedef/alias chains** (already partly built via the
   `typedef struct op OP;` → `@parent` alias edge) — structural, fast, and exactly where
   clangd's macro/template opacity hurts.

### Fundamentally out of reach (needs a real frontend — acknowledge, don't promise)

- Full **overload resolution** (ICS ranking, SFINAE, template partial ordering).
- **Template instantiation** types, dependent-name resolution, **concept satisfaction**.
- Accurate **ADL**.
- Preprocessor-correct **macro expansion** (argument substitution, conditional compilation) and
  TU-accurate diagnostics.
- **Lifetime / dangling** analysis.

The discipline: where we're approximate, **say so in the UX** (label guesses, prefer silence to a
confident wrong answer). Our brand is "fast, flow-aware, never-needs-a-build"; a single confident
wrong overload pick erodes that more than ten honest "couldn't determine"s.

---

## 4. Prioritized roadmap — "full C/C++ LSP experience"

Tags: **[TABLE-STAKES]** (must have or it feels broken) · **[DIFFERENTIATOR]** (flow-aware edge
over clangd) · **[OUT-OF-SCOPE]** (needs a frontend; acknowledge).

### Phase 0 — the non-broken baseline **[TABLE-STAKES]**
*(skeleton.scm already mints most of these refs; this phase is coverage-hardening, not new tiers)*

1. **Document & workspace symbols** — outline incl. classes, namespaces, methods, macros, typedefs. *(largely landed in `skeleton.scm`)*
2. **Goto def/decl + references + hover** on the common path: free functions, methods, members, types, macros, includes. Hover shows the **declared** type/signature.
3. **Completion** — scope members + `obj.`/`obj->` member completion (the sentinel-reparse member-access path already exists) + include-path completion.
4. **Rename + prepareRename + document highlight + linked editing** — ref-driven, same machinery as Perl.
5. **Type hierarchy** (base/derived) — directly the `@parent` inheritance edges.
6. **Signature help** — declared parameter list (no overload ranking — show all overloads).

### Phase 1 — the flow-aware flagship **[DIFFERENTIATOR]**

7. **`std::optional`/`std::variant`/pointer null narrowing in hover + inlay.**
   *How it reaches:* `narrow_guard` arm mapping `if(opt)`/`has_value()`→engaged,
   `holds_alternative<T>`/`get_if<T>`→`T`, `if(p)`→non-null; a small optional/variant/pointer
   lattice; region scoping + `earliest_rebind_in` cutoff already exist.
8. **Use-after-move diagnostic.**
   *How it reaches:* `std::move(x)` mints a Rebind (`.scm` pattern), the region between move and
   next rebind is flagged on read; the rebind-cutoff machinery already truncates at reassignment
   exactly as clang-tidy's "reinitialization clears it" rule requires.
9. **`auto`/`decltype` deduced-type-on-hover for the initialize-from-expression case.**
   *How it reaches:* the FlowEdge edge-chase to the initializer's resolved type (already used for
   the cross-var chase; `auto` already defers `annot_type` to the edge per the skeleton).
10. **const/ref/value-category surfacing in hover/inlay.**
    *How it reaches:* declarator-derived (`deref_stack`/`peel_nested` already peel pointer/ref/cv);
    render it.

### Phase 2 — structural deep-semantics approximations **[DIFFERENTIATOR, clearly-bounded]**

11. **"Missing include for symbol X" code action** — workspace symbol→header index says which
    header declares an unresolved name; offer to add it. *(approximate: no preprocessor)*
12. **Candidate-overload *listing*** (not resolution) on hover/signature-help — "3 overloads of
    `foo`," best-effort arity highlight, **explicitly marked best-effort**.
13. **Macro goto + raw-body hover** — goto the `#define` (macro symbols already minted), show the
    unexpanded body. *(no argument substitution — say so)*
14. **Call hierarchy** — incoming/outgoing built from the call refs we already mint.

### Out of scope — acknowledge, route to clangd if the user needs it **[OUT-OF-SCOPE]**

- Full overload resolution lattice (ICS/SFINAE/partial ordering).
- Template instantiation types, dependent-name resolution, concept satisfaction.
- Accurate ADL; preprocessor-correct macro expansion; conditional-compilation-accurate analysis.
- Lifetime/dangling analysis; TU-accurate diagnostics.

> Positioning line for docs/marketing: **"Instant, build-free, flow-aware C/C++ intelligence —
> optional-state, null-flow, and use-after-move narrowing inline, with zero `compile_commands.json`.
> For full overload/template/macro accuracy on a configured build, pair it with clangd."** We are
> not the replacement for the compiler; we are the thing that *works the instant you open the file*
> and *sees value-flow clangd doesn't surface inline*.

---

## Sources

- clangd index design / template-refs-disabled — <https://clangd.llvm.org/design/indexing>
- clangd code walkthrough (rename = main-file refs + index) — <https://clangd.llvm.org/design/code>
- clangd FAQ (skips header bodies → false positives) — <https://clangd.llvm.org/faq>
- clangd slow on large projects — <https://github.com/clangd/clangd/issues/2355>
- clangd memory (>16 GB) — <https://github.com/clangd/clangd/discussions/2578>
- clangd remote index — <https://clangd.llvm.org/guides/remote-index>
- compile-commands troubleshooting — <https://clangd.llvm.org/troubleshooting>, <https://clangd.llvm.org/design/compile-commands>, <https://github.com/clangd/clangd/issues/695>, <https://discourse.nixos.org/t/clang-clang-and-clangd-cant-find-headers-even-with-compile-commands-json/54657>
- template/dependent-type & auto gaps — <https://github.com/clangd/clangd/issues/1095>, <https://github.com/clangd/clangd/issues/443>, <https://github.com/clangd/clangd/issues/2047>, <https://github.com/clangd/clangd/issues/1015>, <https://github.com/clangd/clangd/issues/58>
- macro expansion in hover (cap 2048) — <https://reviews.llvm.org/D127082>, <https://github.com/clangd/vscode-clangd/issues/203>
- inlay hints / extensions / type hierarchy — <https://clangd.llvm.org/extensions>, <https://deepwiki.com/clangd/vscode-clangd/3.1-inlay-hints>
- include-cleaner — <https://clangd.llvm.org/guides/include-cleaner>
- cross-file rename history — <https://reviews.llvm.org/D96578>, <https://reviews.llvm.org/D69263>
- clang-tidy unchecked-optional-access (flow-sensitive, heavy, opt-in) — <https://clang.llvm.org/extra/clang-tidy/checks/bugprone/unchecked-optional-access.html>
- clang-tidy use-after-move (flow- not path-sensitive; clears on reinit) — <https://clang.llvm.org/extra/clang-tidy/checks/bugprone/use-after-move.html>
- overload resolution model — <https://en.cppreference.com/cpp/language/overload_resolution>, <https://clang.llvm.org/doxygen/classclang_1_1OverloadCandidateSet.html>
- CLion dual engine / overload-failure explanation / Concepts — <https://www.jetbrains.com/help/clion/c-support.html>, <https://www.jetbrains.com/help/clion/settings-languages-cpp-clangd.html>, <https://blog.jetbrains.com/clion/tag/overload-resolution/>, <https://www.jetbrains.com/help/clion/c-20-concepts.html>
- market overview / lineage / MS-EDG / speed — <https://medium.com/@dougschaefer/current-state-of-c-c-language-servers-ab87e6fc186b>, <https://goodbyte.software/how-about-clangd-instead-of-intelllisense/>, <https://news.ycombinator.com/item?id=43788332>, <https://www.kdab.com/supercharging-vs-code-with-c-extensions/>
- tree-sitter is syntactic-only (the honest ceiling) — <https://automadocs.com/blog/tree-sitter-vs-lsp-code-analysis>, <https://lambdaland.org/posts/2026-01-21_tree-sitter_vs_lsp/>
