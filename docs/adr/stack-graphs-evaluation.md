# ADR: Stack graphs — evaluated (with a spike + benchmark), not adopted

> Status: **decided — do not adopt** (revisit trigger at the end).
> Supersedes the one-paragraph dismissal in `docs/prompt-graph-walking.md`
> ("Out of scope") with measured evidence: a running stack-graphs spike, a
> coverage census of the gold corpus, and a dependency/LOC cost count.

## The question

Would adopting [stack graphs](https://github.com/github/stack-graphs)
(GitHub's `stack-graphs` crate, or the `tree-sitter-stack-graphs` TSG-DSL path)
— or a similar name-resolution framework — buy us wins, specifically in
**future blast radius** (less bookkeeping) and **elegance**?

## Verdict

No. Stack graphs solve **name binding** (a reference → its definition, as a
path-finding search over a graph). Our hard, high-value problem is **type
inference**, and in Perl name resolution is *not separable* from it: the
dominant navigation case is `$obj->method`, whose target depends on the
*inferred* class of `$obj`. Stack graphs cannot derive that class — it must be
computed elsewhere (our witness bag) and fed in. So adoption would:

- cover **at most ~38%** of resolution queries (the pure name-binding rows),
- **leave the ~62% type/framework-gated rows exactly where they are** (still
  needing the witness bag),
- **re-implement** the name-binding rules we already have (each Perl exporter
  dialect, `use parent`/`@ISA`, `use constant`) in a second formalism, and
- **add a 36-crate dependency** plus a parallel graph store to keep in sync.

Net: more bookkeeping, not less; a second model to hold in your head, not a
simpler one. The witness bag is already a graph framework — edges + reducers +
a fixed-point — that is *strictly more expressive* than stack graphs because it
carries **types along the edges**, not just symbols. We already have the better
generalization.

## What stack graphs are (and the load-bearing limitation)

A stack graph encodes a language's *name-binding* rules as a graph: scope nodes,
plus push/pop **symbol** nodes that thread a symbol stack and a scope stack.
Resolving a reference is a path-find from the reference node to a definition
node ([arxiv 2211.01224](https://arxiv.org/abs/2211.01224)). It is
file-incremental (each file → an isolated subgraph; cross-file paths stitch at
query time) and language-agnostic (the algorithm is written once; each language
ships a graph-construction definition).

The limitation that decides this for Perl: **stack graphs do name binding, not
type inference.** A path can only be found if the graph *structurally* connects
the reference to the definition. For `foo(A).bar` that works *when* `A` is a
literal class passed positionally and `foo` is the identity — the value flow is
expressible as scope-stack threading (this is literally an upstream test
fixture). For Perl's `my $o = Obj->new; $o->method`, the connection from `$o` to
`Obj`'s members is a *type-inference* fact (what does `new` bless into? what did
the `has`/`with`/DBIC framework synthesize? which ancestor defines `method`?).
Nothing in the syntax states it. GitHub's own docs name the wall: "dynamic
dispatch, reflection, macros, code generation, runtime imports make many
dependencies difficult to recover via static analysis."

## The spike (we actually built it)

`docs/stack-graphs-spike.rs` — a runnable program against `stack-graphs = 0.14`
that builds name-binding graphs by hand (faithful adaptations of the upstream
`sequenced_import_star` and `class_field_through_function_parameter` fixtures to
Perl surface syntax) and runs the real `ForwardPartialPathStitcher`. Results:

```
== (A) NAME BINDING: use-import + classic call + lexical my ==
  [main.pl(2) reference helper] -> [Util.pm(10) definition helper]   ✓ cross-file import
  [main.pl(4) reference Util]   -> [Util.pm(7)  definition Util]      ✓ module ref
  [main.pl(6) reference x]      -> [main.pl(5)  definition x]         ✓ lexical my
  -> 3 references resolved in 30µs

== (B0) METHOD DISPATCH, syntax only ($o = Obj->new; $o->greet) ==
  [main.pl(9) reference o] -> [main.pl(6) definition o]
  -> `greet` resolutions: 0      ← UNRESOLVED: receiver type unknown

== (B1) METHOD DISPATCH, with `$o : Obj` reified as edges ==
  [main.pl(7) reference greet] -> [Obj.pm(4) definition greet]
  -> `greet` resolutions: 1      ← resolves ONLY because the type fact was injected
```

The B0 → B1 delta is the whole argument made concrete: the *only* difference is
two hand-added edges encoding `$o : Obj`. Stack graphs cannot derive those edges;
they are the witness bag's output. And in real Perl that fact depends on `new`'s
bless target, framework synthesis (the `greet` def may not exist as syntax at
all — it's minted by `has`/Mojo/DBIC), and the inheritance walk — i.e. you would
have to run our entire type/framework pipeline *during graph construction* to
mint the nodes and edges, which is precisely "restate Perl-specific emission in
their pipeline."

Build: `stack-graphs` core pulls **36 transitive crates** (21 compiled here in
9.5s); the `tree-sitter-stack-graphs` DSL path adds more (regex, the TSG engine).

## Coverage census (gold corpus)

Categorizing all **122** resolution-capability rows (`definition`, `references`,
`rename`, `hover`, `type-at`, `workspace-symbol`, `completion`,
`implementations`, nested, re-export) by `semantic_area`:

| bucket | rows | share | stack-graphs reach |
|---|---|---|---|
| **type/framework-gated** (type-inference, moose, moo, mojo, bless, chain, fluent, codegen, roles, method-modifier, inheritance) | 51 | 41.8% | none — needs the witness bag |
| **method-dispatch (mixed)** (`oo-isa`: `$self->`/`$obj->`/`Foo->`) | 24 | 19.7% | only when receiver class is statically known; usually inferred |
| **name-binding** (exporter, classic, constants, fq, pod) | 46 | 37.7% | yes — *if* each Perl dialect is re-encoded in TSG |
| other | 1 | 0.8% | — |

Even the 37.7% "addressable" slice is not free: each row rides an exporter
dialect (Exporter, Exporter::Tiny, Sub::Exporter, `use constant`, `use parent`)
that the builder + plugins **already** model — and some are *runtime* re-export
surfaces stack graphs fundamentally can't trace. Concretely, our engine already
resolves the loop-push re-export `push @EXPORT, @{"${m}::EXPORT"}` transitively
(`reexport-fixture/consumer.pl:2` → `RexBase.pm:4`, verified): a runtime
construct with no static path.

## Cost / blast-radius / elegance

**Blast radius (the user's "less bookkeeping?" question): it increases.**
- A second name-binding formalism *alongside* the witness bag — every Perl
  surface that produces a ref (re-exports, framework DSL, dynamic dispatch)
  would need encoding in *both* until full migration, and a clean full
  migration is impossible because ~62% of rows can't leave the bag.
- A parallel persisted graph to keep in sync with `FileAnalysis` (we already
  carry a file-incremental SQLite module cache + idempotent enrichment; this
  would be a *second* incremental store, not a replacement).
- +36 crates in the dependency tree.

**Elegance: we already have the more elegant generalization.**
- `src/graph.rs` is **172 lines total** for the whole derived-edge view:
  `GraphView` holds two borrows and *stores nothing*; edges materialize on
  demand from the stores that already exist; one `walk` (seen-set, depth cap,
  edge-kind mask) backs every reachability query; `EdgeKind` is a closed enum
  with an exhaustive `edges_from` ("one match site, never a parallel walker,"
  with compiler teeth). That is the elegance stack graphs are praised for —
  *without* a build step, a second store, or losing the type dimension.
- The spike needed ~26 graph-construction calls to wire **3** resolvable
  references across 2 files via the Rust API. The intricate push/pop
  scope-threading per construct is exactly the bookkeeping `graph.rs` avoids by
  deriving edges from the model instead of materializing them.
- Lexical resolution is a single-parent linked-list climb (`scope_chain_of`);
  the graph-walking ADR already records that a graph framework "buys nothing"
  there.

## Similar frameworks (also evaluated)

- **Scope graphs** (Visser et al., the academic predecessor): same boundary —
  name binding, not type inference. No win for the type-gated 62%.
- **Salsa** (rust-analyzer's incremental-recompute engine): orthogonal to stack
  graphs — it's a caching/invalidation framework, not a resolver. Could in
  principle replace our fold driver + cache, but we already have file-
  incremental SQLite caching, idempotent enrichment (`base_*_count` seals), and
  a fixed-point that terminates by a lattice argument. Adopting Salsa is a large
  driver rewrite for marginal, unmeasured gain; out of scope here.
- **rust-analyzer's split** (name res in `hir-def`, types in `hir-ty`/chalk):
  the lesson *against* adoption — that split works because Rust's types are a
  bounded, statically-decidable add-on. In Perl the type layer **is** the
  product, so you cannot collect the name-resolution "win" of a framework
  without dragging the type layer through it anyway — which no name-binding
  framework provides.

## When to revisit

Reconsider only if **both**: (1) stack graphs grow first-class value/type-flow
semantics (so `$obj->method` resolves without us pre-computing the receiver
class), and (2) an upstream Perl `tree-sitter-stack-graphs` definition exists
(today TS/Python/Java ship; Perl does not — we'd author the entire binding).
Absent both, the custom typed-edge view (`graph.rs`) integrated with the
witness/reducer pipeline remains the smaller, more elegant, type-aware design.

## Baseline (CI-measured, gold corpus, PR #95)

The current engine — *with* type inference — on the pinned substrate
(`run.pl`, GitHub Actions): **187 PASS / 12 xfail / 0 FAIL / 0 CRASH**.
Per-capability query cost (mean / max ms, end-to-end incl. harness overhead):

| capability | n | mean | max |
|---|---|---|---|
| definition | 22 | 19.6 | 80.8 |
| references | 21 | 32.9 | 114.8 |
| rename | 9 | 23.8 | 27.8 |
| hover | 28 | 35.2 | 103.9 |
| type-at | 20 | 31.5 | 102.9 |

Startup (cold workspace index + cache warm): tiny fixtures 28–65 ms; the **full
CPAN substrate 5.65 s** (one-time, cached after). Suite wall 16.2 s, peak RSS
540 MB. Takeaway: resolution is already interactive (~20–35 ms mean) *including*
the type inference stack graphs can't do, and the dominant cost is the cross-file
index — which a second stack-graph store would also have to pay, on top. There is
no latency headroom for stack graphs to unlock.

## Reproduce

- Spike: copy `docs/stack-graphs-spike.rs` into a crate with
  `stack-graphs = "0.14"` as `src/main.rs`; `cargo run --release`.
- Coverage census: the `semantic_area` aggregation over `gold-corpus/fixtures/*.json`.
- Baseline timing: `gold-corpus/run.pl` (needs the pinned substrate; CI installs
  it via `cpm` and prints the per-capability cost table on every PR).
