# Reparse seam — prototypes, macros, and the stratified fixpoint

**Status: design, evidence-backed, ready to prototype.** Branch
`spike/cpp-support`. Sibling to `docs/spike-query-extraction.md` (the
four-language skeleton spike) and `docs/prompt-multi-language.md` (the
serving wiring). This doc is the design for the one thing the skeleton
spike deliberately did not touch: **a source transform that runs
*before* extraction**, because a declaration elsewhere changes how this
file parses.

It started as a C++ macro question and became a shared Perl+C++ design.
That generalization is the thesis.

## The phenomenon (one, not two)

Perl prototypes and C++ macros are the same fact wearing two hats:

> **A declaration in a (possibly imported) dependency changes how this
> file parses.**

- **Perl prototype.** `sub sner($) {...}` makes `sner` unary, so
  `sner 1, 2` parses as `sner(1), 2`; `sub sner() {...}` makes it
  nullary, so `sner + 1` is `sner() + 1`. tree-sitter-perl cannot know
  the grouping until it has seen the prototype — which may be in another
  file behind a `use`.
- **C++ macro.** `class API_EXPORT Widget {...}` only parses as a class
  once `API_EXPORT` is known to expand to (an attribute, i.e. nearly
  nothing). Until then tree-sitter-cpp reparses the whole class as a
  `function_definition` (measured below). The definition is in a header,
  reached via `#include`.

Both make the **parse of a file depend on declarations in its
dependencies** — not on inferred types, on *declarations*. The engine
already crosses files for types; this says the parse crosses files too,
along the same dependency edge. That single sentence is what unifies the
two features and what keeps the design sound (next section).

## The obstacle course (the evidence)

`src/cpp_obstacle.rs` isolates one macro idiom per sample; two reports
in `query_extract_tests.rs` measure (a) raw parse damage and (b) actual
Tier-1 skeleton extraction through the production driver. The losses
sort into three buckets:

| bucket | samples | what happens | Tier-1 |
|---|---|---|---|
| **clean / conditional** | `clean_baseline`, `ifdef_split` | parser robust; `#ifdef` keeps both arms as well-formed defs | ~100% |
| **ring-3 codegen** | `x_macro`, `decl_macro`, `token_paste`, `gtest` | symbols exist *only* post-expansion; nowhere in the tree | unreachable by any pattern |
| **ring-2 corruption** | `api_export_attr`, `qt_object` | a macro in declarator position flips the parse | silent damage |

The headline finding: `class API_EXPORT Widget {...}` reparses as a
**`function_definition`** — the class evaporates (its methods are
*recovered* as free functions, so `draw`/`resize` survive but `Widget`
and the class structure are gone). One error node, names mostly present,
structure destroyed — the insidious kind, and the single most ubiquitous
real-world idiom (every exported / `__declspec` / Windows header).

So the raw "16/28 symbols" undersells it: most misses are genuine ring-3
(need expansion no matter the medium), and the *parser-corruption* loss
is essentially **one pattern** — declarator-position macros. That is
precisely the pattern with the highest reparse ROI, because its
offenders (`API_EXPORT`, `Q_OBJECT`, `__declspec(...)`) expand to nothing
or near-nothing. **You do not need clang's semantics — you need macro
expansion**, and the worst case is fixed by expanding known-empty macros.

## Soundness: stratify, and the worklist stays monotone for free

The worklist's monotonicity is about *witnesses*: append-only, finite
lattice, fixpoint when the snapshot stops moving. A reparse changes the
**tree**, hence spans, hence the identity of every `Expr(span)` /
`Variable{scope}` attachment. A reparse **interleaved** with the fold
would orphan witnesses and force deletion → non-monotone → dead.

The fact that saves it: **the parse-changing facts are
type-independent.** A prototype shape and a macro definition are
*declarations*, known before any inference runs. So the dependency is
strictly one-directional, and the pipeline is two **stratified**
fixpoints that never interleave:

```
parse → reparse*(facts) → extract → worklist*(types)
        └── fact fixpoint ──┘        └── type fixpoint ──┘
```

The bag is seeded *only* from the settled tree, so worklist
monotonicity is untouched — it never sees an intermediate tree. The
reparse loop gets its own, simpler soundness argument:

- **State** = the set of known proto/macro facts. Monotone-increasing,
  bounded by the finite set of names in scope.
- **Terminates**: each round learns ≥1 new fact (a newly-resolvable
  prototype; a macro call that expansion *reveals* — nested / X-macros
  do iterate) or halts.
- **Confluent**: a fact's content is fixed, so learn-order does not
  change the fixpoint.

**Rule: reparse stays strictly upstream of the bag.** Anchors are not
needed for soundness — only for coordinate remapping (below).

### The type→parse "leak" is actually declaration→parse

The one apparent exception is a parse that depends on a *type*: C++'s
`a < b > (c)` — template instantiation vs two comparisons. But the
disambiguator is **"is `a` a template name?"**, which is answered by
`a`'s *declaration* (`template<...> a`), not by an inferred type. Same
for `typename T::x` and the most-vexing-parse cases: the deciding fact
is declaration-level (is-this-a-type, is-this-a-template), so it lives in
the **same monotone fact-set** as prototypes and macros. It reparses in
the stratified phase — via a plugin — and never forces interleaving.

The truly-interleaved case (an *inferred* type changing the parse) does
not appear to exist for our purposes. If one ever does, isolate it; do
not promote the whole pipeline to interleaved.

## The seam: one transform, two hook flavors

```
(facts in scope) → source transform + anchor map → parse → extract
```

- **Perl prototype reparenthesizer** — a built-in transform:
  `sner 1, 2` → `sner(1), 2` under `($)`.
- **C++ macro expansion** — a transform supplied by a **plugin**.
- **C++ template-angle disambiguation** — also a plugin transform
  (insert the disambiguating parse hint), same stratified phase.

Two flavors of plugin hook, distinguished by what they emit:

- **emit-hook** → emits *symbols / witnesses*. Declaration-macros
  (`DECLARE_DYNAMIC(X)` → a static field, a `GetRuntimeClass` method, a
  `X* Ptr` typedef) are pure ring-3 emission — same shape as Moo
  `has`. No reparse needed; the call site stays put and the hook mints
  the synthesized members.
- **reparse-hook** → emits *source* (+ contributes to the anchor map).
  Declarator-position macros (`API_EXPORT` flipping class→function) need
  this, because the *parse* must be fixed before extraction can see a
  class at all.

A macro plugin may carry both: expand-for-parse where structure is
corrupted, emit-for-symbols where it is merely missing.

## Expansion scope: learn once, bake a plugin

The per-edit path **never shells to `cpp`**. Expansion is a
plugin-*generation*-time tool, batched into the existing Rayon index
phase:

1. **Sentinel-probe** each macro definition: expand
   `DECLARE_DYNAMIC(⟪PROBE⟫)`, parse the output, find where `⟪PROBE⟫`
   lands → you have learned the *parameterized template*, not a frozen
   instance. (This is the inspect-generator / recording-probe pattern
   already used for Import::Base coderefs.)
2. **Bake** it as a learned emit/reparse plugin, **fingerprinted on the
   `#define` text** — editing the macro invalidates it through the
   plugin-fingerprint machinery that already hard-clears the cache.
3. **Degrade honestly.** Learned → hook fires. Not learned → opaque
   tokens, skeleton degrades to navigation. No witness, no claim, no lie.

> **Full `cpp` is fine when it's amortized to once.**

The "full preprocessor vs partial expansion" question dissolves: you pay
the real preprocessor once per *definition*, offline; the runtime path
is cheap learned plugins. Nested macros just mean the reparse fact
fixpoint iterates a few rounds — bounded, terminating, as above.

## Anchors: original ↔ transformed coordinates

A transform shifts byte offsets, so every span the skeleton extracts
from the transformed buffer must map back to **original** source for
goto / rename / refs to land on real user text. Borrow **Zed's `Anchor`**
model: a position stable under edits because it is bound to a logical
point, not a raw offset. Carry an anchor map alongside the transformed
source; `SkelSymbol` / `SkelRef` spans resolve through it.

Granularity choice (open): per-token (precise, expensive) vs
per-transform-region (a macro call site is one span; symbols inside
inherit the call-site anchor — coarse but enough for navigation, and it
degrades honestly: "this symbol came from `DECLARE_DYNAMIC` *here*").
Start per-region.

## Sequencing

1. **Perl-prototype reparenthesizer** — the thinnest end-to-end slice
   that proves the stratified seam: local facts only (no preprocessor,
   no cross-file), detect prototypes from the first parse, reparenthesize
   call sites, re-parse, remap spans through the anchor map. Measured
   like the skeleton spike — not yet wired into the build pipeline.
2. **Anchor map** as a reusable structure (Index/Model-layer; it is
   language-free coordinate bookkeeping).
3. **Cross-file facts** — prototypes/macros resolved along the existing
   dependency edge before a dependent's final parse.
4. **C++ macro learning** — sentinel-probe + bake, fingerprinted; the
   emit-hook flavor first (declaration-macros), the reparse-hook flavor
   second (declarator-position corruption).

Step 1 is days and de-risks the soundness claim; everything after is
additive against a fixed engine, same discipline the skeleton spike
proved.

## What landed (spike, measured)

`src/reparse.rs` (Perl) and `src/cpp_reparse.rs` (C++) carry the seam
end to end, each measured against its obstacle course, neither wired
into the build pipeline.

**Perl reparenthesizer** — learns prototype shapes by query,
reparenthesizes call sites (`sner 1, 2` → `sner(1), 2` under `($)`;
`sner + 1` → `sner() + 1` under `()`), re-parses to the corrected tree,
and remaps spans through an `AnchorMap`.

**C++ macro expansion** — `preprocess` expands object-like and
function-like macros (single source-level pass, bodies pre-expanded);
`preprocess_validated` keeps the transform only if it does not increase
the parser's own ERROR+MISSING count. The before→after delta over the
obstacle course:

| sample | errors b→a | Tier-1 recall b→a | note |
|---|---|---|---|
| `api_export_attr` | 1 → 0 | 2/3 → **3/3** | class recovered (reparse-hook) |
| `decl_macro` | 1 → 0 | 1/3 → **2/3** | `GetRuntimeClass` synthesized by expansion (emit via expand) |
| `x_macro` | 2 → 2 | 2/5 | nested macro CALLS — gate discards, no-op |
| `token_paste` | 2 → 2 | 0/3 | `##` paste — gate discards, no-op |
| others | 0 → 0 | unchanged | no expandable macros |

Two findings the spike pinned:

1. **Expansion fixes the parse with no special-casing.** The probe
   showed tree-sitter-cpp parses `__attribute__((...))` and
   `__declspec(...)` in declarator position cleanly — the macro only
   *hid* the attribute. So "expand to body, re-parse" is the whole rule;
   no strip-vs-expand heuristic.
2. **The parser is the validator.** The validate-by-reparse gate makes
   expansion monotone (errors 9 → 7 over the course, never up): the
   common, high-value idioms land; the nested/token-paste tail is
   discarded rather than corrupting a file. That tail is the
   "amortize full cpp to once" case — a learned plugin that captures the
   real preprocessor's output, not a per-keystroke `cpp`.

Surfaced en route: a genuine Tier-1 skeleton gap — pointer/reference
returning methods (`Foo* m()`) nest the `function_declarator` inside a
`pointer_declarator`; added to `queries/cpp/skeleton.scm`.

## The worklist, layered (landed)

The bag re-entered. `queries/cpp/skeleton.scm` now emits type witnesses
and `cpp_pack` carries the predicates — C++'s leak is *declared types*
(pervasive, unlike Python's optional annotations), so `annot_type` on
every `declaration` carries the load, with literals and an `auto` edge
filling the rest. Two tests prove the whole stack on a **walker-free,
production** FileAnalysis:

- `cpp_walker_free_type_queries` — `int n = 5` → Numeric, `std::string s`
  → String, `geo::Circle c` → ClassName, and `auto m = n` resolves
  through the **three-hop edge chase** (`m → Expr(n) → Variable(n) →
  Numeric`) in the production reducer registry, untouched. Temporality
  holds for free: a variable has no type before its declaration point.
- `cpp_type_inference_through_macro_reparse` — the compose: a class
  destroyed by `class API_EXPORT Box` is recovered by validated
  expansion, and *then* the bag types `Box b` as `ClassName("Box")` and
  chases `auto same = b` — on the macro-reparsed tree. The nominal type
  always existed; reparse made it RESOLVABLE (the class symbol + its
  `width` field come back).

The result the skeleton spike predicted, now true for a third axis: the
witness bag is the language-agnostic core, and a language the engine
never heard of — reparsed past its preprocessor — answers type queries
through the same reducers, with **zero engine edits**. Everything lived
in two `.scm` files, the pack predicates, the driver, and the reparse
seam.

Open for the next pass: the validate gate is per-file, wants to be
per-splice; the source pass is single, wants a fixpoint for nested
calls (X-macros); method-call return typing (`MethodOnClass`) needs the
class-shape emission the pack does not do yet; and none of it is wired
into the build pipeline — these remain measured spikes, lifted when a
second language ships in-editor (`prompt-multi-language.md`).
