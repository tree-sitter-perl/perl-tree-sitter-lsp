# Narrowing / Optional diagnostics — forward plan

**Status: planned, not built.** The flow-narrowing + `Optional<T>` +
`Undef` lattice (`prompt-flow-narrowing.md`, `prompt-optional-types.md`)
was built for hover/goto precision, but its real payoff is **bug
detection**: each new lattice element lets the analyzer *see* a class of
defect it previously couldn't represent. This doc enumerates those
diagnostics in confidence/value tiers.

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

### D5 — redundant re-narrowing
Two guards narrowing the same subject to the same type with no
intervening invalidation (the truncation scan already computes
invalidation points — reuse it). Low value, cheap once D3/D4 exist.

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

### D7 — `Optional` into a non-optional sink
A `Maybe[T]` / `return undef` value assigned to a slot/attribute typed
non-optional, or returned from a sub whose declared contract is
non-optional. **Blocked on** declared sink types (the `param_types` /
signature-return work) — without a non-optional *target* there's nothing
to violate. Park until declared returns/params land.

### D8 — method-doesn't-exist on a narrowed receiver
Narrowing makes a previously-unknown receiver class known (`if
($x->isa('Foo')) { $x->bogus }`). This is **not a new diagnostic** —
it's wiring the narrowed invocant class into the *existing*
`unresolved-method` path (`collect_diagnostics`), so `$x->bogus` flags
when `Foo` + ancestors don't define `bogus`. Lowest-infra, high-value:
narrowing extends the reach of a diagnostic that already exists.

### D9 — dead code after exhaustive early-exit
`return unless defined $x; …; if (!defined $x) { DEAD }`. Needs a
reachability notion on top of narrowing; furthest out.

---

## Sequencing

1. **D1** (provably-`Undef` deref) — definite bug, cleanest, default-on.
   The flagship: it's the reason `Undef` is in the lattice.
2. **D8** (narrowed-receiver unresolved-method) — reuses the existing
   diagnostic; narrowing just widens its reach. Cheap, high value.
3. **D2** (unguarded `Optional` deref) — opt-in nullable-deref + the
   interactive quick-fix. The headline IDE feature.
4. **D3/D4** (always-true/false guards) — the redundancy family; gated
   on confident prior types + MRO relatedness.
5. **D6** (deref shape mismatch) — guard-narrowed reps only at first.
6. **D5 / D7 / D9** — as the redundancy/infra/reachability pieces land.

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
