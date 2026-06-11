# Spike: query-driven extraction — can the core go language-agnostic?

**Status: spike, measured, not for merge.** Branch
`worktree-query-extraction-spike`. The question: re-model FileAnalysis
extraction as declarative queries returning entities, procedural state
managed by a generic driver + per-language predicates — and thereby
make the core language-agnostic. Context that matters: perl-lsp exists
*because* tree-sitter-stack-graphs failed for this; the bet under test
is whether our engine is the piece TSG was missing.

## What was built

- `queries/perl/skeleton.scm` — ~70 lines. The **capture vocabulary is
  the language-neutral contract** (`@def.<kind>` / `@def.<kind>.name`,
  `@scope`, `@context.package`, `@ref.<kind>`, `@import`): the driver
  knows these names, never a node kind.
- `src/query_extract.rs` — ~200 lines, knows no Perl. Flattens query
  matches to ordered events; runs a state machine (scope stack from
  `@scope` ranges, sticky namespace from `@context.*`); consults a
  `LangPack` of host predicates (name shaping, default names) — the
  "back and forth". Output: skeleton symbols/refs/imports.
- `src/query_extract_tests.rs` — differential harness vs the real
  builder over test_files + a 60-module substrate sample (88 files).

## Numbers (the deliverable)

| entity   | builder rows | skeleton recall |
|----------|-------------:|----------------:|
| package  | 122 | **100%** |
| class    | 3 | **100%** |
| sub      | 703 | **96.2%** |
| variable | 1742 | **99.8%** |
| method   | 179 | **2.2%** |
| refs (call/method/var) | 7039 | 77.3% (+23% over-capture) |

Plus **202 symbols with zero claimable syntax** (plugin namespaces,
requires markers, `has` projections).

## The three rings the numbers draw

**Ring 1 — syntactic skeleton: queries win.** Defs, scopes, namespaces,
imports, most refs: ~70 declarative lines + a 200-line generic driver
reproduce 96–100% of what the walker extracts, including the
scope/package state everyone assumes needs procedural code. The driver
is genuinely language-free; a second language's outline would be a new
.scm.

**Ring 2 — pattern-inexpressible syntax: small, nasty, enumerable.**
The residue inside the recall gaps:
- positional pairing — `use constant { A => 1, B => 2 }` ("element 2k
  is a key" cannot be said in a pattern; this is the fat-comma trap);
- codegen loops (`*{$_} = sub` over a qw list) — five symbols at one
  span;
- dynamic method names (`$obj->$m`), interpolated code;
- **the field-table trap (new, found BY this spike):** the
  `variables:` field the CST prints and `child_by_field_name` serves
  matches **zero** in the query engine on `variable_declaration`,
  while `variable:` on `for_statement` matches fine. Cost: 45% of
  variable recall, silently, until measured. Pinned by
  `field_queryability_must_be_probed_per_node`.

This ring is exactly cst.rs's catalog — and the .scm medium re-hosts it
with a *worse* failure mode: a wrong pattern doesn't fail a test, it
silently extracts nothing.

**Ring 3 — semantic synthesis: queries never get there.** 98% of
Method symbols (175/179) have no claimable syntax: `has` accessors,
DBIC relationship methods, plugin helpers, requires markers. Same for
everything downstream — witnesses, chain typing, provenance,
enrichment. No pattern medium reaches this ring; it is computation
over facts, not matching over trees.

## Why TSG failed, and what's actually different here

tree-sitter-stack-graphs pushed rings 2 AND 3 into the DSL: .tsg files
carry scoping rules, graph construction, push/pop symbol semantics —
so a language's .tsg becomes a large program in a weak language with
ring-2's silent failure modes and no escape hatch into a real type
system. That's the death spiral.

Our architecture already separates the rings, which is the genuinely
new fact relative to TSG: ring 3 lives in the engine (witness bag +
reducers, FileAnalysis queries, resolve/refs_to, module index — **none
of which import tree-sitter**, enforced by the layering test) and in
plugins keyed on decision-ready shapes (`CallContext.has_options`,
emit actions). The walker's irreplaceable jobs are ring 2 plus the
ring-3 *emission* (translating Perl semantics into witnesses/symbols)
— and emission is a compiler front-end, not an extraction rule. No
query medium absorbs it; TSG proved what happens when one tries.

## What "far" looks like, honestly

- **Not viable / negative ROI:** porting the Perl builder to queries.
  We'd re-encode ring 2 in a silently-failing medium to replace code
  that already works, and ring 3 wouldn't move at all.
- **Viable and cheap, IF multi-language ever matters:** a **skeleton
  tier** for other languages — outline, workspace symbols, lexical
  single-file refs — from a .scm pack per language, riding the
  already-language-free upper layers (FileStore, ModuleIndex, resolve,
  LSP adapter). This spike's driver is that tier's core, and the
  engine layering means it plugs in without touching Perl's path.
- **The real multi-language frontier** is ring-3 emission: a second
  language gets perl-lsp-grade intelligence only when something
  translates *its* semantics into witnesses. The honest design
  question for that future round: can emit-hooks be keyed on the
  capture vocabulary (skeleton events in, witnesses out) the way rhai
  plugins are keyed on CallContext today? That's the engine-as-product
  bet — and unlike TSG, the typed escape hatch (host predicates, real
  plugins) is load-bearing from day one rather than bolted on.

## Recommendation

Keep the branch as evidence; don't merge. If/when a second language is
wanted, start from the skeleton tier (this driver, nearly as-is) and
spend the design round on witness-emission keying — not on making
queries do more than ring 1.
