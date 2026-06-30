# Code-usage heatmap (`perl-lsp --heatmap`)

A reporting view over the **existing** cross-file reference graph — not a new
analysis tier. It reuses the same `resolve::refs_to` machinery that powers
references and rename; the heatmap just counts over it and labels the result.

The incumbent it mirrors is SciTools Understand's "Butterfly" view
(callers + callees = fan-in / fan-out over a cross-file reference graph). It is
positioned as an **insight / DX** deliverable, not a defect catalog — so it
sidesteps the compliance / tool-qualification apparatus a MISRA checker carries.

## Invocation

```
perl-lsp --heatmap <root> [--csv|--html] [--include-deps] [--all]
```

- `<root>` — workspace root; runs `cli_full_startup(root)` ("act like the LSP
  just started": workspace index + @INC resolve + SQLite warm).
- `--csv` — emit CSV instead of JSON (both are always ingestible; SARIF is the
  gold interchange format but is deferred — see "What's next").
- `--html` — emit a self-contained, offline HTML viewer over the same report
  (see "Visualization" below). Mutually exclusive with `--csv`; if both are
  passed, `--csv` wins (it returns first).
- `--include-deps` — also count references found in cached `@INC` dependency
  modules (default: open + workspace files only).
- `--all` — keep every counted symbol in the `symbols` array; by default the
  array is trimmed to callables and dead candidates (packages with nonzero
  fan-in are summarized but not individually listed unless `--all`).

Output goes to stdout; the startup chatter (`Indexed N files`, cache lines)
goes to stderr, so `--heatmap … 2>/dev/null | jq` is clean.

## Metrics

Per symbol (subs, methods, packages/classes/modules — anonymous and
non-identifier-named symbols are skipped, they have no nameable graph):

- **`fan_in`** — number of reference *sites* across the searched roles, with the
  symbol's own declaration(s) excluded. A "reference site" is **any** mention
  the builder records: call sites, qualified accesses, import-spec mentions
  (`use M qw(foo)`), and export-list mentions (`our @EXPORT_OK = qw(foo)`). This
  is deliberately the broad "how many references" definition; it never
  *under*-counts a live symbol's reachability (the safe direction for dead-code).
- **`fan_out`** — number of **distinct** callees a sub/method references inside
  its own body (intra-file span containment over `FunctionCall` / `MethodCall` /
  `DispatchCall` refs; self-recursion excluded). `null` for packages.
- **`exported`** — whether the symbol is in the file's export surface.
- **`dead_code_candidate`** — `fan_in == 0` **and** no reachability guard fired.
- **`reachable_guard`** — when `fan_in == 0` but the symbol is *not* flagged,
  the reason it is treated as reachable (see below). `null` otherwise.

## Output schema (JSON, `schema: "perl-lsp.heatmap.v1"`)

```jsonc
{
  "schema": "perl-lsp.heatmap.v1",
  "kind": "usage-heatmap",
  "label": "dead_code_candidate = UNREFERENCED SYMBOL (reachability heuristic); NOT MISRA C:2012 Rule 2.2 dead code, which is undecidable",
  "soundness": "over-approximate reachability: ...",
  "root": "<root>",
  "files_indexed": 29,
  "dynamic_dispatch_sites": 2,     // workspace-wide count of $obj->$method sites
  "include_deps": false,
  "summary": { "symbols_reported": 273, "dead_code_candidates": 43 },
  "symbols": [
    { "name": "add", "kind": "Sub", "package": "Calc::Util",
      "file": "/abs/path/Util.pm", "line": 7, "col": 5,
      "fan_in": 4, "fan_out": 0, "exported": true,
      "dead_code_candidate": false, "reachable_guard": null }
  ],
  "dead_code_candidates": [ /* the subset with dead_code_candidate == true */ ]
}
```

`symbols` is sorted heaviest-`fan_in` first (the hotspots). `line`/`col` are
1-based, character-counted (same rendering as `--references`). CSV mode emits
the same columns with RFC-4180 escaping.

## The over-approximation (honest labelling)

This is **not** MISRA C:2012 Rule 2.2 dead code — that rule is undecidable, and
asserting it would invite a tool-qualification burden. A
`dead_code_candidate` here is an **unreferenced symbol**: a reachability
*heuristic*. The output `label` and `soundness` fields say so in-band, and the
`--heatmap` help text repeats it.

Reachability is **over-approximated**: the analysis errs toward *reachable*
(may under-report dead code) so it never falsely flags a live symbol. A
zero-fan-in symbol is shielded from the dead list — with `reachable_guard` set —
when any of these hold (checked most-specific first):

| guard | rule |
|---|---|
| `exported` | name is in the file's export surface — an external consumer may import it |
| `constructor` | conventional constructor (`new`) — frameworks instantiate it |
| `framework-synthesized` | symbol is plugin-minted (Moo accessors, routes, DBIC rels), not user-written; the framework calls it through machinery the static graph doesn't model |
| `package-implicit-use` | packages/classes/modules — reachable via `require`, app entrypoints, dynamic class strings; too many invisible vectors to flag |
| `dynamic-dispatch` | a **method-shaped** sub (declared in a non-`main` package) when the workspace contains **any** `$obj->$method` dispatch — see below |

### Dynamic dispatch is the load-bearing soundness gate

Perl method dispatch is fundamentally dynamic. The builder records a
`dynamic_dispatch_sites` count per file: every `$obj->$method(...)` whose method
name is a scalar rather than a bareword. Such a call produces **no nameable
`MethodCall` ref** (the dispatched method is unknown at build time unless
constant-folding happens to resolve it), so it is invisible to the static
reference graph.

When that count is `> 0` anywhere in the workspace, a sub that *could* be a
method (declared in a class — i.e. a non-`main` package) cannot be proven
unreferenced: an unresolved dynamic dispatch could target it. So it is shielded.
`main`-script free functions are excluded from this shield — they aren't class
methods, so their `FunctionCall` graph is authoritative.

## Honest failure modes

Even for a flagged candidate, "unreferenced" ≠ "safe to delete". The static
graph cannot see:

- **Symbolic code refs** — `\&name`, `&{$name}`, `*{"${pkg}::name"}` — invoke a
  sub by a string the analysis doesn't track. A flagged *function* candidate
  assumes none of these reach it.
- **`->$method` with an unresolved name** — counted as a `dynamic_dispatch_site`
  (which shields methods workspace-wide) but the *specific* target is unknown.
- **`AUTOLOAD`** — methods materialized at call time have no declaration to count.
- **String `eval`** — code (and calls) built at runtime are opaque.
- **External callers** — anything outside the indexed workspace (and, without
  `--include-deps`, outside open+workspace files). Exported symbols are guarded
  for exactly this reason.

Treat the dead list as a **review queue**, not a delete list.

## Implementation notes

- CLI entry: `cli_heatmap` in `src/main.rs`, dispatched from the `--heatmap`
  arm, mirroring `--workspace-symbol`.
- Target construction: `resolve::TargetRef::for_symbol(sym, origin, idx)` — the
  one place a declared `Symbol` maps to a `TargetRef` for fan-in counting
  (kept in `resolve.rs` so target-identity policy stays out of the handler,
  per the file-store-and-resolve ADR). Methods use the inheritance rename-chain
  exactly as references does.
- Fan-in counts `refs_to(... , mask)` results minus `AccessKind::Declaration`.
- Dynamic-dispatch signal: `FileAnalysis.dynamic_dispatch_sites` (`u32`),
  populated in `Builder::visit_method_call` when the method name is a scalar.
  Rides the bincode cache blob (`#[serde(default)]`); `EXTRACT_VERSION` bumped.

## What's next

- **SARIF 2.1.0** output (`--format sarif`) — the gold interchange format for
  the automotive/static-analysis tool ecosystem; deferred from v1.
- **Fan-out depth / transitive reach** — current fan-out is one hop, intra-file.
  A transitive callee count needs the cross-file call graph walked (the
  `GraphView` seam already exists for inheritance/bridges).
- **Precision split for fan-in** — optionally separate call-site fan-in from
  declaration-adjacent mentions (import/export lists) once `RefLocation`
  carries its `RefKind`.

## Visualization (`--html`)

`--heatmap <root> --html` renders the *same* report (no new computation —
`heatmap_html()` just wraps the JSON value the JSON/CSV paths share) as one
self-contained HTML document on stdout:

```
perl-lsp --heatmap <root> --html 2>/dev/null > heatmap.html && open heatmap.html
```

Why this shape:

- **Self-contained, offline.** The template `src/heatmap.html` is compiled in
  via `include_str!` and the report JSON is inlined into a
  `<script type="application/json">` blob, so the file opens off a `file://`
  URL with no server, no CDN, no build step. The embed replaces every `<`
  with its JSON unicode escape so a hostile path can't close the script
  element early; it round-trips back to `<` through `JSON.parse`. Drawing
  is dependency-free SVG.
- **Three views over `symbols[]`, one dataset:**
  - **Treemap** — squarified (Bruls/Huizing/van Wijk), grouped by package;
    tile area is `fan_in + 1` (so zero-fan-in symbols still occupy a cell),
    color is a `sqrt`-lifted fan-in heat ramp, dead-code candidates get a
    dashed-amber outline. Hover for detail; click copies `file:line:col`.
  - **Butterfly** — back-to-back fan-in (callers) / fan-out (callees) bars
    for the hottest symbols, the classic "who calls / who do I call" read.
  - **Dead code** — the candidate queue as a sortable table, framed as a
    *review queue, not a delete list* (carrying the same honest labelling).
- **Carries the soundness story.** The `label` / `soundness` strings and the
  `dynamic_dispatch_sites` count render in the header, so the viewer can't be
  mistaken for a sound dead-code prover.
