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

## Spike 2: the witness bag fed from captures alone

Round 2 pushed past extraction into the engine seam. New vocabulary:
`@expr.lit.<t>` (literal → `InferredType` witness on `Expr(span)`),
`@expr.read.var` (variable read → `Edge(Variable)` witness),
`@flow.target`/`@flow.source` (assignment → `Variable →
Edge(Expr(rhs))`), `@type.annot` (annotation → direct type witness via
a pack predicate). The driver assembles REAL `Scope` rows, REAL
`Symbol` rows, and a REAL `WitnessBag` into a REAL `FileAnalysis` via
`FileAnalysisParts` — no builder anywhere.

**Result 1 — the production engine answers on a walker-free
FileAnalysis.** `inferred_type_via_bag` resolves literals, and the
three-hop edge chase `my $y = $x` → `Variable($x)` → `Expr(literal)`
runs through the production reducer registry untouched
(`walker_free_file_analysis_answers_type_queries`).

**Result 2 — a second language, for real.** tree-sitter-python +
`queries/python/skeleton.scm` (~45 lines) + a pack with ONE host
predicate (`annot_type`). Same driver, same engine: Python outline
(class/def/vars), imports, literal typing, **annotation-driven
typing** (`n: int = compute()` — ring 3 partly in the tree, exactly as
predicted), the cross-variable edge chase, and correctly-scoped
locals inside a method body (`python_pack_same_driver_same_engine`).

**Result 3 — the engine-touch list, the number that answers "how far":
ZERO.** No edit to `file_analysis.rs`, `witnesses.rs`, `resolve.rs`,
or `module_index.rs`. Everything lived in the driver, two .scm files,
and pack predicates. The bag discipline (edges-not-values, monotone,
temporal spans) transferred to a second language verbatim — the
witness bag is the language-agnostic core, today, by construction.

What spike 2 did NOT prove, in fairness: cross-file resolution for a
second language (module-name → file strategy is per-language and
unbuilt), method dispatch (`MethodOnClass` needs class-shape emission
the Python pack doesn't do yet), and any of ring 2 at production
fidelity. The gradient from here is real work, but it is *additive*
pack work against a fixed engine — not engine surgery.

## Spike 3: cross-file, and the Any-land question

**Cross-file works, with per-language resolution as one predicate.**
Two new pack hooks: `module_paths(module) → candidate relative paths`
(the entire per-language resolution strategy — Python's is two
format! lines) and `ctor_class(callee)` (PEP8 capitalization → class
instantiation; the pack's conventions.rs). With those:

- `python_cross_file_function_refs_through_refs_to`: two pack-built
  FileAnalyses in the production FileStore; production `refs_to`
  returns the def in a.py and the call in b.py.
- `python_cross_file_method_dispatch_through_mro_walk`: a generic
  ~20-line import loop registers a.py in the production ModuleIndex
  via `insert_cache` (which feeds `ModuleEdgeIndexes` — names index,
  children index — for free); b.py's `g = Greeter()` types via the
  ctor predicate; `resolve_method_in_ancestors("Greeter", "greet")`
  resolves CROSS-FILE through the production index arms
  (`module_declaring_method_in_package` / `has_sub_in_package`),
  untouched.

A bonus architectural datapoint: the layering test REJECTED the
import-resolver from the Build layer (Build importing Index) — the
resolver is Index-shaped, exactly where module_resolver sits for
Perl. The enforced DAG held against the spike's first sloppy
placement; the language-pack world inherits the same architecture
discipline for free.

**The Any-land question (operator orientation).** Perl's inference
ceiling is high because its syntax LEAKS types at usage sites:
mono-typed operators (`+` numeric, `.`/`eq` string), sigils, deref
shapes. Unannotated Python leaks almost nothing at usage sites — `+`
is polymorphic — so its unannotated ceiling is genuinely lower.
Spike 3b makes the asymmetry concrete in both directions:

- Perl's leakiness IS capture vocabulary: `@obs.numeric` /
  `@obs.string` arms on operator patterns emit the production
  `TypeObservation` payloads, and `operator_evidence_types_through_
  usage` proves a variable with an unknowable initializer types from
  `$x + 1` alone — usage-site evidence through the production
  FrameworkAwareTypeFold.
- A Python pack has almost no such arms to write. That is a fact
  about Python, not about the design: the witness bag is an
  *evidence* framework, and each pack harvests whatever its language
  leaks — sigils + operators (Perl, unusually rich), annotations +
  constructors + literals (Python), less (Ruby). Where evidence runs
  out, the bag's honesty discipline degrades features to navigation
  instead of guessing — no witness, no claim, no lie.

So "how far can language X get" decomposes: ring-1 navigation is
free for every language; the typed ceiling is proportional to
syntax leakiness × ecosystem annotation density. Perl sits at one
extreme by both measures (operators leak, frameworks declare). For
real-world Python the working answer is the same one pyright found:
annotations exist where it matters (typeshed covers the stdlib and
maintained libraries), so the pack's `annot_type` predicate carries
the load, with constructor and literal evidence filling locals.

**Workspace rename, for free.** The question answered itself once
refs worked: rename in this architecture IS `resolve_symbol` →
`refs_to` → text edits, and all three are engine. The test
(`python_workspace_rename_for_free`) puts the cursor on the call in
b.py and renames `helper` → `fetch_all` across the def, the call,
AND the `from a import helper` spelling — the import line cost
exactly one .scm pattern (imported names are refs). Two fixes were
needed, both in the driver/pack, neither in the engine: name-token
selection spans (were zero-width) and the import-name capture.

## Recommendation (revised after spike 2)

Keep the branch as evidence; don't merge. The Perl path stays
walker-built — its ring 2/3 fidelity is years ahead of what packs
reach. But the multi-language thesis is now demonstrated, not
speculated: the engine answers type queries for a language it has
never heard of, through a 45-line query pack and one predicate. If a
second language ever matters, the path is: skeleton tier (outline /
workspace symbols — driver as-is) → lexical typing (this spike's
vocabulary) → per-language module resolution → dispatch emission.
Each step is pack work. The design round worth pre-investing is none;
the next round worth having is module resolution strategy, and only
when a concrete second language shows up.
