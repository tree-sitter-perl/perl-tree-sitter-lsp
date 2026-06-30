# ADR: Narrowing / Optional diagnostics

The flow lattice (`flow-narrowing.md`, `optional-types.md`) was built for
hover/goto precision; its payoff is **bug detection**. A value's type now
answers "are you `undef` here?", "might you be?", "are you the class this
guard tested?" — and a diagnostic is just a consumer that asks. Every fire
reads `inferred_type_via_bag` / a narrowing witness at the use point and
asks the *type*; none match syntax (rule #10), so a new narrowing shape
they can't yet see is a silent miss, never a false positive.

## Two seams, not one pass per diagnostic

Every diagnostic routes through one of two `FileAnalysis` reads:

- **`deref_receiver_sites`** — every scalar-receiver arrow deref paired
  with the receiver's narrowed type at the use point. `undef-deref` (D1),
  `optional-deref` (D2), and `deref-shape-mismatch` (D6) are filters over
  this one stream. Method-call and hash-deref receivers come from refs;
  array (`$x->[i]`) and code (`$x->()`) derefs carry no typed ref, so the
  builder records them as `arrow_deref_sites` and the seam merges the two
  sources — `DerefForm` is the common currency.
- **`guard_redundancies`** over build-recorded `guard_sites` — the
  redundant/contradictory-guard verdicts (D3/D4).

A diagnostic that needs a new fact extends a seam; it does not grow a
parallel walk.

## D1 is the only always-on lint; the rest earn trust

`undef-deref` is always-on `WARNING`: the lattice says the receiver *is*
`Undef` (the negative side of a `defined`/`blessed` guard), not *may be*,
so its confidence is maximal and any deref on it is a hard runtime die.
Everything else ships behind a default-off `DiagnosticOptions` flag
(`optionalDeref`, `redundantGuard`, `derefShape`,
`unresolvedMethodCrossFile`), mirrored as `--…` CLI flags. They graduate
to default-on per code once the gold substrate and real projects show no
false-positive flood; `unresolvedMethodCrossFile` promotes last, because
cross-file classes carry the codegen/XS methods the static walker can't
see (the `diag-09/10` Log4perl-accessor class), exactly the surface a
cross-file unresolved-method lint trips over.

## D6 reads the guard's rep, not the merged type

A `$x->{k}` deref pushes a zero-extent `HashRef` belief sitting *exactly*
at the use point — the narrowest possible span, so it wins the merged
`inferred_type_via_bag` query against the wider `ref…eq 'ARRAY'` narrowing
region. Reading the merged type for D6 is therefore circular: the deref
masks the very conflict the diagnostic looks for. `guard_narrowed_rep`
scans **narrowing-sourced witnesses only** (`Builder("narrowing")` /
`"defined_narrowing"`), so it sees the guard's assertion and not the
deref's self-belief. A consequence falls out for free: D6 fires *only* on
guard-narrowed reps — a plain `my $x = []; $x->{k}` is silent (no guard,
no witness), which is the intended restriction, not a special case.

The mismatch is one axis, every direction: `DerefForm::demands_rep()`
(hash / array / code, or `None` for a method call) versus
`RepKind::of(guard_rep)`. `RepKind::of` answers `None` for a `ClassName`
— a blessed object overloads any deref, so it is never a mismatch — and
`Some` only for a concrete container rep. Fire iff both are `Some` and
differ. No per-form branch, no per-rep table.

## Build-recorded, because recognition is CST-bound

`guard_sites` and `arrow_deref_sites` are populated in the builder and
ride `FileAnalysis` (serde, cache blob), because the facts they hold are
recognized by walking the tree — guard conditions by the narrowing
recognizers, arrow derefs by the `array_element` / `coderef_call` visit
arms — and query-time is tree-free. A `GuardSite` is the query-time
projection of a recognized guard: subject, predicate, polarity
(`asserts_when_true`), and the point at which to read the subject's *prior*
(un-narrowed) type — the guard's own location, before any narrowed region,
so the read can't see the narrowing the guard itself produces.

D3/D4 then compare prior-vs-predicate on confident types only: an absent
or merely-`Optional` prior leaves the guard meaningful and is skipped.
Class relatedness routes through the one `for_each_ancestor_class` walk
(`$x` typed `Foo`, guard `isa('Base')` where `Foo` ⊆ `Base` → redundant;
unrelated class → contradictory; a downcast — subject is the base, guard
tests a child — is inconclusive and stays silent).

## D5 is D3 on a narrowed prior

Redundant re-narrowing (two guards, same subject, same type, no
intervening invalidation) needs no separate path: the first guard's
narrowing witness *is* the subject's prior type at the second guard — it
survives precisely because no reassignment truncated it (the truncation
scan of `flow-narrowing.md` is what keeps it alive) — so D3 already flags
the second guard. Pinned by `d5_sequential_renarrow_is_flagged_by_d3`.

## Under-narrowing is the friend

The narrower deliberately under-narrows (conservative truncation), so
these diagnostics *miss* some real bugs but never *invent* one. That bias
is load-bearing: never tighten the narrower to catch more bugs at the cost
of a false `Undef`/`Optional`, or every diagnostic built on it inherits
the false positive.

## Forward work

- **D7 — `Optional` into a non-optional sink.** Blocked on declared sink
  types: without a non-optional return/param/slot there is nothing to
  violate. Unblocks when the `param_types` / signature-return work lands;
  then it is a one-pass compare of the source `Optional` against the sink.
- **D9 — dead code after exhaustive early-exit.** Needs a control-flow
  reachability pass (the lattice proves the *type*, not the
  *unreachability*); none exists. D4 already catches the dead *guard*, so
  the D9-only signal is the dead *block*.
- **Rep `ref…eq` redundancy in D3/D4.** D3/D4 evaluate `isa` (class) and
  `defined` predicates; a `ref($x) eq 'HASH'` guard on an
  already-hash-shaped subject is recognized but not yet folded to a
  verdict. Same shape as the class case, on `RepKind` instead of MRO.
- **DBIC column hash-deref (`$row->{col}`).** `$row->{col}` where `$row` is
  a DBIC Result class and `col` is one of its `Bridged` columns — a column
  isn't a hash slot, so the deref is `undef` (the author meant `$row->col`).
  Same thesis (ask the invocant type; the row answers "my columns aren't
  slots"); the detection seam is TODO-marked in `collect_diagnostics`.
  **Blocked on HashRefInflator tracking**: with a `HashRefInflator` result
  class, find/search return plain hashrefs where `$row->{col}` IS valid, and
  we don't model that override yet — so the warning must gate on NOT-HRI first.
