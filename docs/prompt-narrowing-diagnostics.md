# Narrowing / Optional diagnostics — forward plan

**Status: D1, D2, D3/D4, D6, D8 landed; D5 subsumed by D3; D7/D9 parked.**
The flow-narrowing + `Optional<T>` + `Undef` lattice
(`adr/flow-narrowing.md`, `adr/optional-types.md`) was built for hover/goto
precision, but its real payoff is **bug detection**: each new lattice
element lets the analyzer *see* a class of defect it previously couldn't
represent. This doc enumerates those diagnostics in confidence/value tiers.

**Landed shape.** All consumers read the lattice through two shared seams,
never matching syntax (rule #10): `FileAnalysis::deref_receiver_sites`
(each scalar-receiver deref + its narrowed receiver type — D1/D2/D6) and
`FileAnalysis::guard_redundancies` over build-recorded `guard_sites`
(D3/D4). D1 is always-on (`undef-deref`, WARNING); the rest are opt-in
`DiagnosticOptions` flags (`optionalDeref`, `redundantGuard`,
`derefShape`, `unresolvedMethodCrossFile`) mirrored as `--…` CLI flags,
default-off until each earns trust. All four arrow-deref forms are
covered: the method-call invocant and scalar hash deref come from refs;
array (`$x->[i]`) and code (`$x->()`) derefs carry no typed ref, so the
builder records them as `arrow_deref_sites` and `deref_receiver_sites`
merges the two sources. D6 generalizes over them via `DerefForm::demands_rep`
vs `RepKind::of(guard_rep)` — one axis, every direction.

**The thesis.** A value's type at a point now answers questions it
couldn't before — "are you `undef` here?", "might you be `undef`?", "are
you the class this guard tests for?". A diagnostic is just a consumer
that asks. So every diagnostic below **reads the lattice at the use
site** (`inferred_type_via_bag(recv, point)` /
`method_call_invocant_class(ref)` — both already observe narrowing) and
**asks the type**, never pattern-matches syntax (rule #10). The receiver
answers; the diagnostic never sees the shape.

**Infrastructure.** These reuse the diagnostic framework planned in
`prompt-cli-tools.md` (PL-codes, per-code severity config, comment
suppression `# perl-lsp: ignore(PLxxx)`, SARIF) and the existing
`symbols.rs::collect_diagnostics` seam + `DiagnosticOptions` opt-in flags
(precedent: `unresolved_dispatch`). They do **not** invent a parallel
mechanism. Each is a pass over method-call / element-access refs that
queries the lattice; default-off until each earns trust on the gold
corpus + real projects.

---

## Tier 1 — definite bugs (high confidence; default-on `WARNING`)

### D1 — method/deref on a provably-`Undef` receiver
`$x->m` / `$x->{k}` / `$x->[i]` / `$x->()` where `$x` resolves to
`InferredType::Undef` at that point — the `else` of `if (defined $x)`,
the remainder after `return if defined $x`, the body of `unless (defined
$x)`. Runtime outcome is a hard die ("Can't call method `m` on an
undefined value", "Can't use an undefined value as a HASH reference").

- **Reads:** `inferred_type_via_bag(recv, use_point) == Some(Undef)`.
- **Confidence:** maximal — the lattice says it *is* undef, not *may
  be*. This is the single cleanest win and the right first build.
- **Quick-fix:** none obvious (the code is already wrong); the message
  points at the guard that proved it undef.

---

## Tier 2 — likely bugs (opt-in lint; `WARNING`/`INFORMATION`)

### D2 — unguarded `Optional` dereference  (the strictNullChecks analog)
`$r->m` / `$r->{k}` where `$r` is `Optional<T>` at the use point and no
`defined`/`blessed` guard dominates it. The narrowing already strips the
`Optional` where a guard *does* dominate (so guarded uses don't fire);
an unguarded use is a possible undef-deref.

- **Reads:** type at the use point is `Optional(_)` (un-narrowed).
- **Confidence:** "may be undef", not "is" → opt-in flag, lower default
  severity. Risk of noise on idiomatic code that the author knows is
  safe — hence opt-in, plus suppression comments.
- **Quick-fix:** insert `return unless defined $r;` before the use, or
  wrap in `if (defined $r) { … }`. This is where narrowing pays off
  *interactively* — the fix produces exactly the guard the narrower then
  consumes.
- **Source:** `Optional` arises from `return undef` / bare `return;`
  arms, `$c ? T : undef`, `Maybe[T]` isa — all already typed.

### D3 — always-true guard / redundant narrowing
`if ($x->isa('Foo'))` where `$x` is *already* confidently `Foo` (or a
subclass) at the guard; `if (defined $x)` where `$x` is provably
non-`Optional`/non-`Undef`. The guard is redundant and its `else` is
dead.

- **Reads:** the subject's type *before* the guard already satisfies the
  guard's predicate.
- **Confidence:** only when the prior type is *confidently* known
  (a concrete `ClassName`, not merely "absent"). Gate hard against
  unknown-typed subjects or it floods.

### D4 — always-false / contradictory guard
`if ($x->isa('Foo'))` where `$x` is already a *different*, non-related
class; `if (defined $x)` where `$x` is `Undef`. The `then`-branch is
dead.

- **Reads:** prior type contradicts the guard predicate (incompatible
  `ClassName`, or `Undef` vs `defined`).
- **Confidence:** needs a real "are these classes related?" check
  (MRO / cross-file) to avoid flagging legitimate downcasts — reuse
  `resolve_method_in_ancestors`/`parents_of`, don't hand-roll.

### D5 — redundant re-narrowing — SUBSUMED BY D3
Two guards narrowing the same subject to the same type with no
intervening invalidation. **No separate path:** the first guard's
narrowing witness *is* the subject's prior type at the second guard (it
survives precisely because no reassignment truncated it — the truncation
scan the doc anticipated reusing is what keeps it alive), so D3's
prior-vs-predicate check already flags the second guard as redundant.
Pinned by `d5_sequential_renarrow_is_flagged_by_d3`. Building a parallel
scan would duplicate D3 (rule #5).

---

## Tier 3 — structural & flow (needs the rep guards / more infra)

### D6 — deref shape mismatch
`$x->{k}` where `$x` is narrowed to `ArrayRef` / `CodeRef` /
`ClassName` (not hash-shaped); `$x->[i]` on a `HashRef`; `$x->()` on a
non-`CodeRef`. The `ref($x) eq 'ARRAY'`-style guards make the rep known,
so a mismatched deref is a representable type error.

- **Reads:** the receiver's rep at the use point vs the access kind.
- **Confidence:** high *when a rep guard narrowed it*; otherwise the rep
  is often `HashRef` by default and would false-positive — fire only on
  guard-narrowed reps initially.

### D7 — `Optional` into a non-optional sink — PARKED
A `Maybe[T]` / `return undef` value assigned to a slot/attribute typed
non-optional, or returned from a sub whose declared contract is
non-optional. **Blocked on** declared sink types (the `param_types` /
signature-return work) — without a non-optional *target* there's nothing
to violate. Unblock condition: a declared non-optional return/param/slot
type lands; then this is a one-pass comparison of the source `Optional`
against the sink's declared type. Park until then.

### D8 — method-doesn't-exist on a narrowed receiver
Narrowing makes a previously-unknown receiver class known (`if
($x->isa('Foo')) { $x->bogus }`). This is **not a new diagnostic** —
the existing `unresolved-method` pass already queries
`inferred_type_via_bag(invocant, use_point)`, which observes the
span-scoped narrowing witness, so **the local-class case already fires
today** (verified: `if ($x->isa('Foo')) { $x->bogus }` flags when `Foo`
is defined in-file and lacks `bogus`).

**Residual = the cross-file class case.** The pass's `is_local_class`
gate (`symbols.rs`, checks only the current file's symbols) short-circuits
before method resolution runs, so a narrowed cross-file class
(`$x->isa('Some::Dep')`) doesn't flag even though the narrowing resolves
`ClassName("Some::Dep")` and `resolve_method_in_ancestors` /
`class_has_unresolved_ancestor` already take a `module_index`. Lifting D8
cross-file = relaxing that gate to "local OR cross-file-resolvable with a
*complete* ancestor chain" (the unresolved-ancestor honest-silent guard
is the existing safety valve — without it, every external class with an
out-of-workspace parent would false-positive). Same gate the whole
`unresolved-method` diagnostic shares; not narrowing-specific.

### D9 — dead code after exhaustive early-exit — PARKED
`return unless defined $x; …; if (!defined $x) { DEAD }`. Needs a
reachability notion on top of narrowing (the lattice proves the *type*,
not the *unreachability*). Furthest out; unblock condition is a
control-flow-reachability pass — none exists today. Note D4 already
catches the *guard* here (`if (!defined $x)` is contradictory once `$x`
is proven defined), so the marginal D9-only signal is the dead *block*,
not the dead guard. Park until a reachability pass exists.

---

## Sequencing — as landed

1. **D1** (provably-`Undef` deref) — LANDED, always-on `undef-deref`
   WARNING. The flagship: it's the reason `Undef` is in the lattice.
2. **D8** (narrowed-receiver unresolved-method) — LANDED behind
   `unresolvedMethodCrossFile`. The always-on local case was already
   there; this lifts the `is_local_class` gate to cross-file (kept opt-in
   because cross-file classes carry codegen/XS methods the walker can't
   see — the diag-09/10 class).
3. **D2** (unguarded `Optional` deref) — LANDED behind `optionalDeref`,
   INFORMATION + the `return unless defined` quick-fix.
4. **D3/D4** (redundant / contradictory guards) — LANDED behind
   `redundantGuard`, INFORMATION. Confident prior types + MRO relatedness
   via `for_each_ancestor_class`; scoped to isa/DOES + defined/blessed
   (rep `ref…eq` redundancy is the residual).
5. **D6** (deref shape mismatch) — LANDED behind `derefShape`, WARNING,
   guard-narrowed reps only (reads `guard_narrowed_rep`, not the merged
   type, to dodge the deref's self-inferred rep). Covers every form ×
   every guarded rep (`$x->{k}` on array/code, `$x->[i]` on hash/code,
   `$x->()` on hash/array) via the `demands_rep`/`RepKind::of` axis;
   objects (`RepKind::of` → `None`) are never a mismatch.
6. **D5** — SUBSUMED by D3 (above). **D7 / D9** — PARKED with explicit
   unblock conditions (declared sink types / a reachability pass).

**Promotion path.** The opt-in flags graduate to default-on once the gold
substrate + real projects show no false-positive flood. D1 already
defaults on (maximal confidence). D8's cross-file extension is the most
codegen-sensitive and should promote last.

## Discipline (read before building)

- **Ask the type, not the syntax.** Every fire reads
  `inferred_type_via_bag` / `method_call_invocant_class` at the use
  point. No `if method_name == …`, no receiver-spelling checks.
- **Default-off, earn trust.** Each PL-code ships behind a
  `DiagnosticOptions` flag; promote to default-on only after gold + real
  projects show no false-positive flood. `Undef` (D1) may default-on
  early because its confidence is maximal.
- **Suppressible.** Honor `# perl-lsp: ignore(PLxxx)` (per
  `prompt-cli-tools.md`) before emitting.
- **Under-narrowing is your friend.** The narrower deliberately
  under-narrows (conservative truncation). That means these diagnostics
  *miss* some real bugs but don't *invent* them — exactly the bias a
  diagnostic wants. Never tighten the narrower to catch more bugs at the
  cost of a false `Undef`/`Optional`.
