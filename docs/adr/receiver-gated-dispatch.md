# ADR: Receiver-gated dispatch — filter-on-read, resolved at query time

A dispatch verb (`$minion->enqueue('task')`, `$emitter->emit('ready')`)
links a call site to a handler registered elsewhere, but only when the
*receiver* `isa` the verb's target class. That class often resolves only
cross-file (`Clove::Minion` extends `Minion` in another module; a Mojo
helper returns a `Minion` subclass). Two facts force the design:

- The builder is index-free (rule #1), so a call site can't be confirmed
  against cross-file ancestry at parse time.
- Only OPEN documents are enriched. Workspace-index and dependency files
  are built and never enriched, so any promotion that runs at enrichment
  never reaches them.

## The seam: `ReceiverGated<T>`

```rust
pub struct ReceiverGated<T> { gate: String /*ClassName*/, inner: T }
pub enum GateResult<U> { Applies(U), DoesNotApply, ReceiverUntyped }
impl<T> ReceiverGated<T> {
    pub fn resolve_for(&self, receiver_class: Option<&str>,
        parents: &PackageParents, mi: Option<&ModuleIndex>) -> GateResult<&T>;
}
```

The inner payload is readable through exactly one method, `resolve_for`,
which gates on the receiver. `inner` has no `pub` field, no `Deref`, no
`into_inner` — a consumer **cannot** observe gated content without first
asking "does this receiver qualify?". A future caller that forgets the
filter is a compile error, not a silent drift. This is rule #10 made
structural: the type carries the rule, the consumer can't re-decide it.

`resolve_for` walks the single isa seam — the free `class_isa(class,
target, package_parents, module_index)` over local `package_parents` ∪
cross-file `parents_cached`, the same nodes `parents_of` enumerates. No
second ancestry walk exists.

The `gate` is one `ClassName` in Phase 1. Widening it to a small set later
is a change to `resolve_for`'s body, not to call sites — they only ever
see the three `GateResult` arms.

## Query-time dispatch (option C)

Each per-file dispatch candidate rides the cache as
`ReceiverGated<DispatchCandidate>`, recorded at build, ungated, per-file
(the builder still owns the CST walk that finds the call). The receiver
isa-check runs at **query time**:

- `FileAnalysis::applicable_dispatches(mi)` resolves every gated candidate
  against its receiver and returns the `Applies` ones. `resolve.rs`
  (`refs_to` for a `Handler` target) and dispatch goto-def
  (`FileAnalysis::dispatch_at`) both route through it, so a call site in a
  NON-open workspace/dependency file surfaces exactly like an open one.
- The receiver class comes from the existing invocant resolution
  (build-time `ClassName` hint, else `method_call_invocant_class`, bag-
  routed and cross-file aware) — gate *input*, distinct from gated
  *content*. Only the handler-link payload (name, owner) is protected.

Why C over the alternatives:

- **Enrichment-only promotion** (what landed before) materialized
  `DispatchCall` refs at enrichment, which only open files get — the gap.
- **Eager index over all workspace files** re-enriches dependency/
  workspace `Arc`'d analyses in a post-index pass, introducing an
  ordering + invalidation lifecycle. Large, and it duplicates the lazy
  resolution the type-inference `MethodOnClass` walk already proved is the
  correctness floor for enrichment-bypassing callers.
- **Query-time (C)** keeps the "no enrichment over deps" invariant and
  mirrors the type path: candidates are facts on the cache, resolution is
  lazy at the query.

The emit-hook path (`EmitAction::DispatchCall`, for files that trigger on
`use Minion` / `ClassIsa`) still materializes refs at build — it doesn't
gate on the receiver. `applicable_dispatches` de-dups gated candidates
against those materialized refs by (span, dispatcher), so a triggered file
surfaces each site exactly once. There are not two promotion paths to keep
in sync: the materialized refs are the emit-hook's own output, and the
gated candidates are the only thing the manifest path produces — there is
no enrichment-time promotion of candidates into refs anymore.

## The 3-way `GateResult` + opt-in diagnostic

Splitting `DoesNotApply` from `ReceiverUntyped` is load-bearing.
`DoesNotApply` is a settled negative (the receiver typed, it just isn't a
descendant of the gate). `ReceiverUntyped` is a typing gap — the receiver
of a known dispatch verb couldn't be pinned to any class, so applicability
is unknown.

The opt-in `unresolved-dispatch` diagnostic fires ONLY on
`ReceiverUntyped`, NEVER on `DoesNotApply`. Collapsing the two would
either bury real gaps or spew an INFO on every unrelated receiver. It is a
QA/plugin-author tool, so it is **off by default**, behind
`initializationOptions.diagnostics.unresolvedDispatch` (LSP) and
`--unresolved-dispatch` (the `--check` CLI). The always-on hints
(`unresolved-function` / `unresolved-method`) ignore the toggle.

## Phase 2: emission-conditionals on the same seam

The receiver-isa rule is one instance of a broader shape: *a plugin
emission applies only when an ancestry/role/surface predicate holds, and
that predicate is cross-file*. The same `ReceiverGated` producer mints
the other deferred emission-conditionals (see
`docs/prompt-enrichment-inheritance-residual.md`).

**`param_types` `in_role` — LANDED.** A plugin `param_types()` rule types a
named param when the enclosing package `isa` the rule's `in_role` class. The
builder now emits one `ReceiverGated<TypeConstraint>` per matching sub
(`gate = in_role`), UNGATED — no local-ancestry precondition, preserving the
index-free contract. `FileAnalysis::gated_param_type_for` resolves it at
query time via `resolve_for(enclosing_package, …)`, so a controller whose
`Catalyst::Controller` ancestor is reachable only through a cross-file base
types its `$c`. The gated TC is a *value read through one query seam*
(`inferred_type_via_bag_ctx`), which is what makes the single-seam gate
sufficient.

Still deferred:

- `ClassIsa` triggers (mojo-events `$self->on('ready')` through a
  cross-file parent chain). Unlike `param_types`, these fire EMIT HOOKS
  that synthesize symbols feeding every symbol-table consumer — there's no
  single query seam to gate. Needs an overlay-symbols mechanism; see
  `docs/open-problems.md` and the graph-walking pillar.
- the app-surface consumer set (already isa-shaped via `parents_of` +
  `app_surface_consumers`; folding it onto `resolve_for` is a small
  consolidation, not a correctness gap).
