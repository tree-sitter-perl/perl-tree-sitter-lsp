# QA design items — needs design, do not implement yet

QA-findings clusters that are **not** quick fixes: each needs a design
decision (where the rule lives, what type/witness shape carries it,
whether it's core or plugin) before code. Implementing the smallest diff
here would violate rule #10 (no special-casing on shape) or paint over a
boundary the engine hasn't modeled yet. Each item below: the problem, why
it resists a local fix, the option space, and a recommendation.

Cross-refs: `docs/qa-findings.md` (the cluster letters), `docs/open-
problems.md` (the untyped-boundary statement), `docs/adr/receiver-gated-
dispatch.md` and `docs/adr/plugin-system.md` (the manifest seams a fix
would extend), the `exporters-core-vs-byo` memory note (B-cluster policy).

---

## E2 — helper/callback `$c` param typing (param typed by registration context)

**Problem.** `sub _helper($c, ...) { $c->stash(...) }` inside a Mojolicious
plugin types `$c` as the enclosing plugin package (its lexical home), so
every `$c->`-method flags. The author means `$c` to be a
`Mojolicious::Controller` — but that's known only from the *callback
contract* the sub is registered under (`$app->helper(foo => \&_helper)`),
not from anything lexically visible in the sub. ~75 FPs in Mojo alone.

**Why it needs design.** A sub parameter is currently typed from its
*lexical* surroundings (first param of a method → enclosing class via
`FirstParamInMethod`). Here the type flows from a **callsite in a
different statement** — the registration — into the param. That's
cross-procedure value-flow *into* a parameter, the exact gap
`docs/open-problems.md` calls boundary #4 and explicitly left out of the
route work. Special-casing "param named `$c` → Controller" is forbidden
(rule #10: the name carries no contract; tomorrow's `$ctrl` or `$self`
helper silently misses).

**Options.**
1. **Callback-contract manifest** — a plugin declares "subs registered via
   `helper`/`hook`/`under` have first param `Mojolicious::Controller`",
   analogous to `param_types()` / `dispatch_verbs()`. The builder, on
   seeing the registration, pushes a `FirstParam`-style witness on the
   referenced sub's param keyed to the declared type. Plugin owns the
   verb→type map; core stays generic.
2. **General callsite→param value-flow** — propagate the type of `\&sub`'s
   eventual invocation argument into the param across procedures. Correct
   and reusable, but it's the full boundary-#4 solve (a new inference
   axis), far larger than this FP.
3. Status quo + suppression heuristic — rejected (shape-branch).

**Recommendation.** Option 1. It's the receiver-gated/manifest pattern the
engine already uses, keeps the "which verb implies which type" knowledge in
the Mojo plugin, and is a strict subset of option 2 that a later general
value-flow pass can subsume. Ties directly to D1 (the catalyst multi-hop
`$c` is the same "param typed by external contract, then resolved through
inheritance" shape).

---

## D1 — multi-hop classic `use base` / `@ISA` method resolution at depth

**Problem.** Single-hop classic inheritance resolves (`SpamAssassin` 244
refs through one `@ISA` level). A 3-hop chain (`Result → BaseResult → Core`)
doesn't — methods defined two-plus levels up flag as unresolved.
Bugzilla ~304 FPs, schema-loader.

**Why it needs design.** `resolve_method_in_ancestors()` has a depth-20
DFS, so the *walk* isn't the cap — the breakage is that intermediate
ancestors aren't all present/typed at query time. Classic `@ISA`/`use base`
parents are cross-file; whether hop-2's parents are known depends on which
files were indexed/enriched, and enrichment runs **OPEN-only** (per CLAUDE.md
cross-file enrichment). So depth works when every hop happens to be an open
doc and degrades when a middle hop is a workspace-index/dependency file.
This is the same root as the catalyst multi-hop `$c`: ancestry resolution
that's correct one hop deep but loses the thread when a hop lives in a
file built without enrichment.

**Options.**
1. **Lift ancestry resolution onto the `MethodOnClass` query-time walk**
   for classic parents too — the registry already chases inheritance edges
   cross-file (`MethodOnClass{child} → Edge(MethodOnClass{parent})`); ensure
   classic `@ISA`/`use base` parents mint those edges at every hop, not just
   the open-doc ones, so the walk doesn't depend on enrichment having run.
2. Eagerly enrich the transitive ancestor set of open docs — narrower, but
   re-introduces the OPEN-only asymmetry one level out and risks fan-out.

**Recommendation.** Option 1, unified with the
`prompt-enrichment-inheritance-residual` Phase-2 work. The principle: depth
must be a property of the query-time edge walk (which already survives
enrichment), never of which files happened to be enriched. Verify the
`parents_of` seam emits classic-parent edges for *every* hop.

---

## B2 / B3 — exporter consumer-semantics SYSTEM (tags / renames / re-exports)

**Problem.** The consumer half of the exporter story is a large, coherent
FP source: tag/bundle expansion (`use M qw(:tag)`, `:DEFAULT`, `:all`,
`-V2`, `:log` driven by `%EXPORT_TAGS`); `-as` renames
(`use M foo => { -as => 'bar' }`); generic re-export (`use Test::Most`
re-exporting all of Test::More's `ok`/`is`/...). Individually:
Test::Most 253 FPs, Test::Spec 129, Type::Library/ResultDDL `-V2` 25/file.

**Why it needs design.** These aren't three bugs; they're one missing
**model of what a `use` imports**. Today import resolution is roughly
"names literally in the `qw()` list." A coherent fix must represent:
(a) a module's *export surface* — `@EXPORT`, `@EXPORT_OK`, `%EXPORT_TAGS`
(tag → name-set), and re-export chains (a module's surface *includes* the
surface of what it `use`s and re-exports); (b) the *consumer*'s import
spec — a list of selectors (bare name, tag, `-as` rename, regex, negation)
applied to that surface to produce the locally-bound name set. Picking off
`:all` alone, then `-as` alone, then Test::Most alone, builds three
shape-branches that don't compose (a `:tag` *with* an `-as` rename inside it
falls between them). This is squarely the "exporters are core's job"
decision (`exporters-core-vs-byo`): renames/bundles/re-exports are common
CPAN mechanics, not house style, so they belong in core's model — not a
per-module plugin.

**Options.**
1. **Two-stage model.** Stage A (producer, cross-file): each indexed module
   computes a resolved *export surface* = name → origin, with tags expanded
   and re-export `use` chains followed (bounded, seen-set). Stage B
   (consumer): the import spec is a selector list evaluated against that
   surface, yielding `(local_name, origin)` bindings — `-as` renames the
   local name, regex/negation filter, tags select sub-sets. Refs bind to
   `origin` for goto-def; locals suppress the unresolved warning.
2. Incremental per-feature patches — rejected; they don't compose and each
   re-touches the same import-parsing site.

**Recommendation.** Option 1, designed as one subsystem with the surface as
the shared data structure both stages name. It also subsumes B1
(re-export), B4 (`@EXPORT` at scale), B5 (callsite resolution — a bound
local name is resolvable everywhere, not just in the `qw` list), B7 (regex
import args). Sequence the producer surface first (it's the dependency);
the consumer selector evaluator reads it.

---

## A4 — hash-extracted invocant typing + the untyped-param/hash-element boundary

**Problem.** `my $x = $self->{field}; $x->method` types `$x` as `HashRef`
(the rep of a hash element), so `$x->method` flags even when `_field` holds
a typed object. SpamAssassin / perltidy (44) / Mojo. Sibling of A2 (`shift`
form) and the untyped-boundary open problem.

**Why it needs design.** This is **boundary #4** in `docs/open-problems.md`:
a value arriving from a hash element (or untyped param) has no declared
type, and the engine has deliberately not modeled "what type does this hash
slot hold." A naive fix — "extracting `$self->{x}` yields whatever was last
written to `$self->{x}`" — is real intra-procedural flow but collides with
the mutation-tracking the bag already does for `mutated_keys_on_class`, and
it's still silent when the write is cross-procedural (a constructor in
another file stuffs the slot). The boundary is principled, not an oversight:
modeling it wrong pollutes inference with `HashRef`-typed everything.

**Options.**
1. **Slot-type witnesses from observed writes.** When the builder sees
   `$self->{field} = <typed-expr>`, push a per-class slot→type witness;
   a later `my $x = $self->{field}` reads it. Covers same-file writes,
   degrades silently (no witness → stays untyped, today's behavior) for
   cross-file. Composes with the existing mutation Facts.
2. **Declared slot types** — a manifest (Moo `has`, DBIC columns already do
   this for accessors) extends to raw `$self->{field}` reads. Narrower but
   only helps framework classes.
3. Leave deferred (current state) — honest, but A4 stays a standing FP.

**Recommendation.** Option 1 as the general mechanism, *informed by* option
2's declarations where present (a `has`-synthesized slot already knows its
type). Keep the failure mode "no evidence → untyped," never "no evidence →
`HashRef`," so it can't regress typed chains. This is a genuine new inference
axis — schedule it with the boundary-#4 / value-flow work, not as a patch.

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

## B4 — cross-file `@EXPORT` bare-`use` not suppressed at scale (Bugzilla ~899)

**Problem.** `use Bugzilla::Util;` (bare, no import list) should pull the
module's `@EXPORT`; instead every exported function flags. ~899 FPs in
Bugzilla — *surprising*, because GATE-5 (the landed `export_ok` bare-`use`
work) was supposed to cover this.

**Why it needs design.** It's not obviously its own bug — it's likely a
*symptom* of one of the items above and needs investigation to attribute
before any fix:
- **H1?** If the resolver picks `contrib/Bugzilla.pm` over the real one, it
  reads the wrong `@EXPORT` and suppresses nothing. Bugzilla is exactly the
  project where H1 reproduces — strong candidate.
- **Workspace-exporter resolution at scale?** GATE-5 may resolve `@EXPORT`
  for OPEN/indexed-as-dep modules but not for sibling workspace files at
  Bugzilla's size (hundreds of intra-project `use`s).
- **B6 warm-cache regression?** The "exported by X" attribution is lost on
  the warm/cached path (enrichment not persisted in the cache blob). If QA
  ran warm, the suppression that works cold is gone — making B4 a
  manifestation of B6, not a new defect.

**Why it needs design (not code).** The fix is different for each cause
(canonical-file selection vs. workspace-tier exporter lookup vs. cache-blob
persistence), and shipping one without attributing risks masking the others.

**Recommendation.** **Investigate first, in order:** (1) dump which file the
resolver picked for `package Bugzilla::Util` (H1); (2) re-run cold vs. warm
and diff the FP count (B6); (3) confirm whether `@EXPORT` is resolved for
workspace-tier files at all. Then fix the attributed cause. Most likely
chain: H1 lands → re-test → residual is B6 (persist enrichment in the cache
blob, bump `EXTRACT_VERSION`). Do not write a B4-specific suppression — it
would paper over whichever of H1/B6 is the real defect.
