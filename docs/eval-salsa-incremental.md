# Evaluation: adopting Salsa (or a lighter incremental framework)

**Question (from the brief):** should we adopt Salsa or a similar incremental
framework? The motivating pain is that *enrichment is underpowered* — it only
runs on open documents — and we want to know what we could unlock if analyses
were **always enriched** (e.g. a fully-resolved workspace we can query like a
database). The eval was asked to include an actual refactor attempt + benchmark,
and to weigh **future blast radius** (less bookkeeping?) and **elegance**.

**Bottom line up front.** Salsa is the *right shape* for the problem and a spike
proves the payoff is large: with automatic dependency tracking, "always
enriched" costs **~1–2 ms per edit instead of ~370 ms**, with **near-zero
hand-written invalidation bookkeeping**. But a wholesale port of this 73k-line
codebase is high-risk for three concrete reasons that the research surfaced
(cyclic fixed-point inference vs. Salsa's panic-on-cycle default, span-heavy
data defeating early-cutoff, and a real memory-blowup track record), and Salsa
still has **no production-grade persistence**, so our SQLite cache stays either
way. The recommendation is a **phased, boundary-respecting adoption** (or a
hand-rolled equivalent), not a big-bang rewrite — details in §9.

---

## 1. The real finding: enrichment isn't open-only because it's *expensive*

Enrichment (`FileAnalysis::enrich_imported_types_with_keys`,
`file_analysis.rs:3585`) injects cross-file knowledge into a file: imported
return types as `TypeConstraint` witnesses, synthetic `HashKeyDef` symbols for
imported-hash-key completion, and cross-file inheritance edges. It is idempotent
by `truncate(base_*_count)` + reappend, and it is called **only for open
documents** — from `publish_diagnostics` and the resolver-refresh callback
(`backend.rs`), plus every CLI query mode.

Measured cost of one enrichment pass: **~0.1 ms/file** (phase timing on a single
hover). Enriching all 340 files in the benchmark workspace would be ~34 ms. So
**raw CPU is not the barrier.** The barriers are structural:

1. **Ownership / mutability.** An enriched `FileAnalysis` is *mutable* and owned
   by the open `Document`. Workspace-index and dependency analyses are immutable
   `Arc<FileAnalysis>` shared in a `DashMap` (and serialized verbatim into
   SQLite). You cannot enrich them in place without changing who owns them.
2. **No dependency graph.** Today, when file B changes, B's *consumers* are
   **not** recomputed. Open docs paper over this by brute-force re-enriching on
   *every* resolver callback; dependency files just stay stale until re-resolved
   on demand. Correctness is held up by a *query-time* structural fallback
   (`query_rec` + `parents_of` walks in `witnesses.rs`), which is the real
   reason enrichment can be skipped at all — it's a precomputed optimization +
   completion convenience, **not** a correctness floor.

"Always enriched" therefore requires the one thing we don't have: a
**consumer→dependency edge with automatic invalidation**, so that changing B
re-enriches exactly B's affected consumers and nothing else. That is precisely
what an incremental framework provides for free. This eval is really "should we
buy that dependency-tracking machinery, and from whom?"

## 2. What "always enriched" unlocks

If every workspace file carried fully-resolved cross-file types cheaply and
durably, several features stop requiring bespoke per-query cross-file walks:

- **Workspace-wide semantic queries** ("find all implementors of role X", "every
  caller whose argument type is Y", unused-export detection, call/▸type graphs)
  become reads over a materialized, already-resolved index — the "simple SQLite
  queries for interesting data" the brief imagines.
- **Completion / hover / goto inside dependency and non-focused files** gains the
  imported-key and imported-return knowledge that today only open docs get.
- **Diagnostics** can be workspace-global without the O(open-docs) re-enrich
  storm on every resolver tick.

Caveat the brief should hear: Salsa gives you *always-enriched in memory,
cheaply kept fresh*. It does **not** give you the SQLite-queryable artifact by
itself (its persistence is a prototype — §7). The "query it like a database"
feature is a *separate, small* materialization step (serialize the enriched
analyses into the SQLite we already maintain); always-enrichment is what makes
keeping that table fresh affordable.

## 3. Salsa in 2026 (state of the art)

- Latest crate: **`salsa` 0.27.0 (2026-06-04), still pre-1.0.** The "new Salsa"
  rewrite (`#[salsa::input]` / `#[salsa::tracked]` / `#[salsa::interned]` /
  `Database` / accumulators / durability) has been on crates.io since 0.18
  (2025-02); the old `query_group!` API (0.16.x) is dead.
- Production users: **rust-analyzer** (migrated, user-facing 2025-03), Astral's
  **ty** type checker, and **ruff_db**.
- Model: declare *inputs* (mutable roots, e.g. file text), write *tracked*
  functions that are pure memoized computations; reading an input/another query
  **automatically records a dependency edge**. A global revision counter +
  red-green validation + **backdating** (if a re-run yields an equal value,
  downstream doesn't re-run) deliver early-cutoff. **Durability** lets "stdlib
  didn't change" skip graph traversal entirely on each keystroke.
- Parallelism is now first-class: the DB is a cheap `Arc`-clone; worker threads
  run read-only and are auto-cancelled when the host bumps a revision.

## 4. The spike (real builder, real corpus, real Salsa)

Rather than hand-wave, I wired Salsa into the crate behind a feature flag and
modeled the *actual* enrichment dependency graph on the *real* builder.

- Code: `src/salsa_bench.rs`, gated by `--features salsa_bench`, run with
  `perl-lsp --salsa-bench <root>`. Reproducible; off by default; all 986 tests
  still pass and the default build is untouched (Salsa is an optional dep).
- Shape modeled:
  - `#[salsa::input] SourceFile { module, text }`.
  - `#[salsa::tracked] surface(file)` — runs the **real** `builder::build`, then
    projects the cross-file *surface* enrichment consumes (imports, parents,
    public methods' return types + hash-key counts). Deliberately **span-free**,
    so a body-only edit yields an equal `Surface` → backdating fires.
  - `#[salsa::tracked] enriched(file)` — reads its own surface + the surfaces of
    its imports and recurses `enriched` along the parent chain. *Reading those
    surfaces is the entire dependency declaration* — there is no manual edge
    bookkeeping.
- Corpus (`bench/gen_corpus.py`): worst case for blast radius — a deep base
  chain `Base::Level0 ← … ← Level{D-1}` plus a wide consumer fan that all
  `extends` the tip, so editing `Base::Level0` can invalidate the whole tree.

### Numbers

340-file workspace (D=40 chain, 300 consumers), release build:

| Scenario | wall | expensive `surface` builds | `enriched` re-runs |
|---|---|---|---|
| Warm (cold memoization of everything) | 390 ms | 340 | 340 |
| Re-query, **no edit** | 0.01 ms | 0 | 0 |
| **Body-only edit** to root base class | **1.0 ms** | **1** | **0** |
| **Surface-changing edit** to root base class | **2.3 ms** | **1** | 340 |
| Naive "always-enriched, no dep-tracking" (rebuild all) | **367 ms** | 340 | — |

760-file workspace (D=60, 700 consumers) — scaling check:

| Scenario | wall |
|---|---|
| Body-only edit to root | **1.1 ms** (flat vs. 340-file) |
| Surface-changing edit to root | 4.3 ms (grows with #dependents) |
| Naive rebuild-all | 851 ms (grows with workspace size) |

### What this proves

- **Early-cutoff is the whole game.** A body-only edit to the root of a 339- (or
  759-) descendant tree recomputes the *one* edited file's analysis and **zero**
  dependents — because the edit didn't change the file's *surface*. Cost is
  **flat ~1 ms regardless of workspace size**. This is exactly rust-analyzer's
  "typing in a body never invalidates global data" firewall.
- **When a surface genuinely changes, only the cheap tier cascades.** Adding an
  exported method to the root re-runs 340 `enriched` aggregations (which only
  read cached surfaces) but only **one** expensive `builder::build`. The costly
  work stays firewalled to the file that actually changed.
- **The contrast is ~160–200×.** "Always enriched" the naive way (no
  dependency tracking) means paying the full-workspace rebuild — 367 ms at 340
  files, 851 ms at 760, growing without bound — *on every relevant edit*. With
  Salsa it's 1–4 ms.

(Honesty note: the spike's `surface` build is ~1.15 ms/file because it builds a
single file without the cross-file module index; the production fold against a
40-deep ancestry costs ~11 ms/file under `--check`. That inflates the *absolute*
warm/naive numbers a real port would see, but the *relative* story — incremental
vs. naive, and flat early-cutoff — is unaffected, and in fact gets **more**
favorable as per-file build cost rises.)

## 5. Blast radius / bookkeeping (the maintenance question)

This is where Salsa is strongest *on paper* and the spike confirms it:

- The dependency edge between a consumer and its dependency is **the act of
  calling `surface(db, dep)`**. There is no reverse index to maintain, no
  dirty-set to propagate, no "when B changes, find B's consumers" code. Today we
  have *none* of that machinery for cross-file invalidation (open docs brute-
  force re-enrich); Salsa would let us delete the brute-force and gain
  correctness for non-open files at the same time.
- Adding a new derived fact = adding a tracked function; its inputs are tracked
  automatically. Compare to our current discipline (every re-emittable fold pass
  must `remove_by_source_tag` then re-push to stay idempotent, base-count sealing
  for enrichment, manual `rebuild_*_indices`): a lot of that bookkeeping exists
  *because* we hand-roll incrementality.

The catch is that this elegance assumes your computations fit Salsa's mold
(acyclic, value-equality-friendly, db-resident). §6 is where ours don't, cleanly.

## 6. Costs & risks specific to *this* codebase

1. **Our core inference is a cyclic fixed-point; Salsa panics on cycles by
   default.** `fold_to_fixed_point` + the `MethodOnClass` inheritance edges in
   the witness registry are mutually recursive and iterate to a fixed point.
   Salsa supports fixpoint cycles, but only soundly when every query in the cycle
   is monotone over a bounded-height lattice, and rust-analyzer still hits "cycle
   detected" panics in practice. **Mitigation (important):** keep the entire
   fixed-point fold *inside one opaque tracked query* `analysis(file) ->
   Arc<FileAnalysis>` so Salsa never sees the cycle — only the *cross-file* tier
   (enriched / method-resolution / refs) becomes Salsa queries. This is how RA
   contains its own fixpoints, and it's the only way I'd attempt this.
2. **Span-heavy data defeats early-cutoff.** Backdating only fires when a re-run
   produces an *equal* value. Our `FileAnalysis` is saturated with byte spans /
   `Point`s that shift on every edit, so a naive `analysis(file)` would *never*
   backdate and the firewall would collapse. matklad names this exact trap as
   "the bulk of the work." The spike sidesteps it by querying a **span-free
   surface** — a real port must introduce that surface/`ItemTree`-style
   position-independent projection as the cutoff boundary. That's a non-trivial
   design task, not a mechanical port.
3. **Memory.** New Salsa had **no GC for interned values** until late 2025, and
   memoized values are unbounded unless you opt into LRU. rust-analyzer's
   migration **quadrupled** memory on a 700-crate workspace (5–6 GB → ~22 GB)
   and regressed `prime_caches` ~4.3× before LRU/`#[salsa::enum]` tuning clawed
   it back. We'd be signing up for ongoing memory tuning.
4. **The `'db` lifetime is viral.** Tracked/interned values borrow the db and
   can't be held across a `&mut db` mutation. It propagates into every signature
   that touches them. The spike dodged it (input ids stored in a `OnceLock` map,
   no tracked structs); a real port threading `FileAnalysis` views through query
   layers would feel it everywhere.
5. **Pre-1.0 churn + proc-macro compile cost.** 0.27 is pre-1.0 with frequent
   breaking releases; the macros are codegen-heavy (documented monomorphization
   bloat). The spike pulled in **12 transitive crates**
   (`salsa`/`salsa-macros`/`salsa-macro-rules`, `boxcar`, `foldhash`,
   `rustc-hash`, `crossbeam-queue`, `intrusive-collections`, `inventory`,
   `allocator-api2`, `memoffset`, `typeid`).
6. **Counterpoint worth respecting:** **Pyrefly (Meta) deliberately declined
   Salsa**, judging fine-grained incrementality "much more complex and harder to
   maintain… with minimal performance gain" for their design, and chose
   module-level recompute with tighter memory control. Reasonable teams say no.

### What was *easy* (pleasant surprises)

The Salsa 0.27 API itself compiled essentially first-try: `#[salsa::input]`,
`#[salsa::tracked]`, the `#[salsa::db]` storage struct, `#[derive(salsa::Update)]`,
and the `Setter` API all worked as documented. The mechanics are not the hard
part — the *modeling* (cycles, cutoff boundaries) is.

## 7. Persistence reality (updates a likely prior)

Historically "Salsa is in-memory only." As of 0.24 (2025-08) there is an
**opt-in `persistence` feature** (serde-based) — but it is **prototype-grade**:
all transitive deps forced serializable, no way to update inputs after
deserialize, accumulators not serializable. **rust-analyzer does not persist its
Salsa db**; it re-indexes on startup. rustc *does* persist (its bespoke
`OnDiskCache`), but that's compiler-internal, not a crate.

Consequence for us: our **SQLite `FileAnalysis` cache stays** regardless of this
decision — it's what gives us fast cold starts, and Salsa won't replace it. The
two are complementary: SQLite = cross-restart cold-start persistence; Salsa =
in-session incremental freshness. This also means a Salsa adoption is **purely
additive risk** on the persistence axis (we keep what works, add an engine on
top), which is reassuring.

## 8. Alternatives

| Option | Fit | Trade-off |
|---|---|---|
| **Salsa 0.27** | Best fit for a deep LSP query graph; proven in RA/ty/ruff | Heavy mental model, viral `'db`, memory tuning, pre-1.0, cycle care |
| **comemo 0.5** (Typst) | Lightweight: annotate pure fns `#[memoize]`, mark `#[track]` args; *constrained* memoization (tracks which parts of an input were read); no central db, no `'db` | In-memory only, no durability/revision machinery; less suited to many-layer query graphs |
| **Hand-rolled** (reverse-dep index + dirty-set) | We already have the module reverse index + SQLite; the missing piece is a consumer→dependency edge + invalidation — a few hundred lines | We own correctness/early-cutoff ourselves; no framework leverage, but no framework risk either |
| **rustc query system** | Most mature persistent incremental engine | Not a reusable crate |
| **adapton** | — | Effectively unmaintained |

`comemo` is the credible "Salsa-lite": much easier retrofit (no database, no
lifetime), good when inputs are lazily loaded — but it gives up the revision /
durability machinery that makes the keystroke-scale firewall robust.

## 9. Recommendation

**Do not big-bang port to Salsa.** Do pursue always-enrichment via a
**boundary-respecting, phased** approach. Concretely:

- **Phase 0 — this eval.** Done. Spike + numbers + risks landed behind a feature
  flag for anyone to reproduce.
- **Phase 1 — introduce a span-free cross-file *surface* type** (the real
  prerequisite, independent of which engine wins). A position-independent
  projection of a `FileAnalysis`'s cross-file-visible facts (exported/public
  method return types, hash keys, parents, bridges). This is the early-cutoff
  boundary and is useful on its own (it's a smaller cache blob and a cleaner
  cross-file API). **This is the highest-leverage next step and carries no
  framework commitment.**
- **Phase 2 — add dependency-tracked enrichment** over that surface, keeping the
  cyclic builder/fold as **one opaque function** (no Salsa cycles). Two ways to
  buy the tracking:
  - **Salsa**, if we want the long-term query-graph foundation an LSP ultimately
    wants (this is where RA/ty/ruff all landed). Scope it to the cross-file tier;
    keep SQLite for cold start.
  - **Hand-rolled** reverse-dep + dirty-set over the existing module index, if we
    want zero framework risk and are willing to own early-cutoff. Given we
    *already* have the reverse index and SQLite, this may be the
    better-elegance-per-risk choice for a first cut, with Salsa as a later
    upgrade if the query graph deepens.
- **Phase 3 — materialize the enriched surface into SQLite** for the
  "query interesting data like a database" feature. Cheap once Phase 2 keeps it
  fresh incrementally.

**Decision driver:** the gating risk is *not* "is Salsa good" (it is) — it's the
**span-free surface boundary (Phase 1)**, which we need no matter what. Build
that first; the engine choice (Salsa vs. hand-rolled vs. comemo) becomes a
lower-stakes, reversible decision made against a real boundary instead of
against the whole `FileAnalysis`.

## 10. Reproduce

```sh
# generate the worst-case corpus (deep base chain + wide consumer fan)
python3 bench/gen_corpus.py /tmp/bench-corpus 40 300

# build the spike + run it
cargo build --release --features salsa_bench
target/release/perl-lsp --salsa-bench /tmp/bench-corpus

# baseline cold/warm startup of the real server on the same corpus
PERL_LSP_TIMINGS=1 target/release/perl-lsp --check /tmp/bench-corpus
```

The gold-corpus harness could give real-CPAN timing, but its substrate
(`gold-corpus/local/`) isn't installed in this environment (needs `carton`); a
draft PR running CI against it is the way to get those numbers if wanted.

## Sources

Salsa 0.27.0 / crates.io versions; rust-analyzer migration PR #18964 &
changelog-277; matklad "Three Architectures for a Responsive IDE" (2020) &
"Durable Incrementality" (2023); Salsa book (overview, durability, cycles,
db_lifetime, algorithm); RA memory/perf issues #19402, #19404, LRU PR #19378;
Salsa persistence PR #967 & issue #10; comemo (typst/comemo); Pyrefly-vs-ty
writeup (Edward Li); rustc-dev-guide incremental/OnDiskCache.
