# perl-lsp Roadmap

Landed work lives in `docs/adr/` and `CHANGELOG.md` — never here.
This file is only what's NEXT, in order.

## Now (in order)

1. **Graph walking** — the walker + the whole inheritance axis landed
   (`adr/graph-walking.md`). The deferred Scope-node taxonomy (Openness
   diagnostic, `home_namespace`) is forward work in
   `prompt-graph-walking.md`. **Instance brands** (`$minion`/`$app`
   instance identity, multi-app Mojo) were spiked and PARKED — they're a
   consumer of the long-distance value-provenance tier (see Queued), not
   a standalone feature; rationale + the birth-site design in
   `prompt-graph-walking.md`.
2. **DBIC out of core** — ungated; phase ladder in
   `prompt-dbic-as-plugin.md`. Ends with core plugin-free except
   generic dispatch.

## Queued (pull-driven — QA findings decide order)

Type intelligence:
- Residual fact classes Parts 1–5 (invocant mutations, hash-key
  unions, method loops, functional operators, value-indexed returns)
  — `prompt-type-inference-residual.md`.
- Flow-sensitive narrowing (`$x->isa('Foo')` / `ref($x) eq` /
  Type::Tiny guards). Needs its own design doc first; the open
  question is un-narrowing at block exit against monotone witnesses.
- Conditional-reassignment disagreement-to-widen (`$spec = {...}
  unless ref $spec`) — replaces the `reassigned_scalars` trust-gate
  clause with a real lattice fold.
- A4 v2: cross-FILE slot writes (`$self->{k} = Obj->new` in another
  file) — the `MethodOnClass` bridge pattern.

Plugin genericity:
- `has_options` final dissolution: the option pairing already moved out
  of core — the plugin reads accessor options via the shared
  `classified_pairs` over the flattened, per-arg `value_shape`-classified
  args. The one Moo-semantic field still in core is the
  `isa`-string→`InferredType` mapping; moving it onto the
  `type_constraint_names()` / `type_constraint_inner()` plugin seam is the
  last step, after which `HasOptions` dissolves entirely (attr names come
  from `value_shape`/`arg_names`, options from `classified_pairs`).

Hardening:
- Fold safety net: `eprintln!` → `tracing::error!` (builder.rs
  ~12061) + a synthetic-oscillator test so the release-mode
  `MAX_FOLD_ITERATIONS` break can't bit-rot.
- Full-bag scans in `apply_chain_typing_assignments` /
  `FileAnalysis::inferred_type` — index when profiling flags them.
- DBIC parametric column-key completion at an empty `->search({ | })`
  (goto-def proves the chain; `complete_keyval_args` lacks the
  parametric-receiver branch; pin in `test_e2e_dbic_parametric.lua`).
- Cursor-context qualified-path/invocant detection should ask the
  tree, not byte-walk (`extract_package_from_prefix` & sibling).
- `return_via_edge` chases lack `TypeProvenance` (stamp
  `Delegation{kind: "callable_return_edge"}` on the chase).
- cst/conventions migration backlog — `prompt-cst-migration.md`.
- Unify autoquoted-key-as-literal into `cst::string_list`. Today
  `string_list` routes `autoquoted_bareword` through the caller's
  `fold` (const resolution), so the DSL-arg callers (`extract_arg_name_list`)
  carry a per-caller fold that special-cases autoquoted→literal. An
  autoquoted bareword is a grammar-certified literal for *every* caller,
  so the right home for the rule is `string_list` itself — then
  `extract_arg_name_list` deletes and the DBIC/keyval paths just use
  `extract_string_list`. **Blocked on** a latent use-import bug it
  unmasks: `use constant NAME => v`'s autoquoted key gets emitted as a
  spurious `FunctionCall` import ref (resolved_package `"constant"`) by
  the use-list walker — the old fold hid it by dropping non-constant
  barewords. Regression-guarded by `const_call_form_not_double_reffed`.
  Fix the use-`constant` path to not feed its declared names to the
  generic import-ref emitter (it already routes them to
  `accumulate_use_constant`), THEN move the autoquoted arm into
  `string_list` and drop the per-caller fold. Proper unification; not
  urgent (the per-caller fold is correct, just not DRY).

QA tail:
- MAIN-1 (`main::` across `require`) and H1 (duplicate packages) —
  designs in `qa-design-items.md`. MooseX::Role::Parameterized — no
  design yet.
- Per-row known gaps: `gold-corpus/KNOWN-GAPS.md` (xfail rows are the
  live tracker).

## Parked (explicit unblock conditions)

- **Re-export chains** — branch `worktree-agent-aae99d42f4d5d74bc`
  (correct in isolation; design in `adr/reexport-surface.md` on the
  branch). Blocked on the ts-parser-perl X1 scanner thread-safety fix
  (`parser-shortcomings.md`). On rework: rebase, confirm no
  Bugzilla-cold abort, re-verify Test::Most → Test::More end-to-end.
- **Sequence-types phases** — QA pulls; `prompt-sequence-types.md`.
- **Type-system encoding** (axis dispatch) — waits for the full axis
  set; graph walking informs it. `prompt-type-system-encoding.md`.
- **Type-is-the-gate generalization** — waits for a second motivating
  site. `prompt-type-is-the-gate.md`.

## Backburner (user-facing, ship-when-ready)

- Mojo polish: route naming/url_for, stash intelligence, hooks,
  transitive plugin chains, config completion —
  `prompt-mojo-todo.md`.
- CLI diagnostic framework (PL-codes, suppression, SARIF), --migrate —
  `prompt-cli-tools.md`.
- Ref provenance: constant-fold `folded_from`, package→file rename,
  inheritance override scoping — `prompt-ref-provenance.md`.
- Aspirational type features (effects/throws) —
  `prompt-type-system-futures.md`.
- Web extension — `prompt-wasm-web-extension.md` (the crate split it
  assumed was executed and REJECTED; branch `workspace-split` is the
  playbook if wasm ever forces it).
- Multi-language engine — proven in spikes; design + working packs on
  branch `worktree-query-extraction-spike`
  (`docs/prompt-multi-language.md` there).

## Out of scope

Multi-workspace/monorepo · cross-file rename of deps (read-only by
`RoleMask::EDITABLE`) · effect facts · full dependent inference ·
`wantarray` returns · cross-function scalar aliasing · runtime
namespace extension (graph-gated).

## Reading order for someone joining

1. `CLAUDE.md` — live architecture. Source of truth.
2. `docs/adr/*.md` — load-bearing decisions for landed work.
3. This roadmap.
4. `docs/open-problems.md` — the deliberate deferrals.
5. The `prompt-*.md` for your workstream.
6. `gold-corpus/README.md` + `KNOWN-GAPS.md` — the regression net.
