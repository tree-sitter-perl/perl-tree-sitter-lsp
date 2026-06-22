# Flow-sensitive narrowing — design

**Status: design, not built.** Active workstream (ROADMAP "Now" #1).
Playground / executable spec: `test_files/narrowing_playground.pl`.

The feature: a guard expression (`$x->isa('Foo')`, `ref($x) eq 'HASH'`,
`return unless blessed $x`) refines a variable's type for the region of
code that the guard dominates, then the type widens back at the region's
exit.

## The engine already exists — this is an emission feature

The query path already does flow-sensitive lookup. `ReducerRegistry`'s
`reduce` (`witnesses.rs:581`) on a `Variable` attachment with a `point`:

- **Narrowest-span-containing-point wins** (`witnesses.rs:596–613`): of
  the non-zero-extent `InferredType` witnesses whose span contains the
  query point, the smallest-area one is returned.
- **Temporal ordering** (`witnesses.rs:640–644`): a witness whose span
  *starts after* the point is skipped (a later reassignment can't poison
  an earlier read).
- **Scoped-witness exclusion** (`witnesses.rs:648–651`): a non-zero
  `InferredType` witness whose span does **not** contain the point is
  skipped entirely.

That third rule **is the un-narrowing**. There is nothing to remove or
widen at block exit — a narrowing witness scoped to the guarded region
simply stops containing the point past the region, so the outer (wide,
zero-extent) witness wins again. Monotone bag, no retraction. This is
why `prompt-flow-narrowing`'s old "un-narrowing at block exit against
monotone witnesses" worry dissolves: it was never a retraction problem,
it's a **span-computation** problem.

Proof the substrate works today: `witnesses_tests.rs::
narrowed_span_wins_over_outer_witness_at_inside_point` pushes a
`HashRef` zero-extent outer witness and an `ArrayRef` span-extent
witness over rows 4..8, then asserts `ArrayRef` inside and `HashRef`
before. That test is exactly one hand-built narrowing; this feature
makes the builder emit those witnesses from guard syntax.

### The one trap: do NOT route through `push_type_constraint`

`push_type_constraint` (`file_analysis.rs:3819`) **zero-extents** the
primary `InferredType` witness (`span.start..span.start`,
`file_analysis.rs:3828`) — by design, because an assignment's type holds
from that point forward via temporal ordering, not over a bounded span.
A narrowing witness is the opposite: it must carry the **full region
span** so narrowest-span-wins picks it inside and scoped-exclusion drops
it outside.

So narrowing gets a dedicated emission helper, not the TC path:

```rust
// builder.rs — pushes a SPAN-EXTENT InferredType witness on the
// variable's home scope. Span = the dominated region; the span does the
// lifetime slicing, the scope stays the variable's (mirrors the proven
// test, which keeps ScopeId(0) for both outer and narrowing witnesses).
fn emit_narrowing(&mut self, var: &str, scope: ScopeId,
                  ty: InferredType, region: Span)
```

Source tag `Builder("narrowing")`. Emitted during the **live walk**
(rule #1), so it lands before `finalize_post_walk` seals
`base_witness_count` and therefore survives enrichment truncation
(enrichment truncates to the base and re-derives only enrichment
witnesses). It is **not** re-emittable — it's a once-derived syntactic
fact, monotone — so it needs no clear-and-emit (worklist invariants).

## Guard catalog (v1 — native, dependency-free)

Each guard recognizes a `(variable, narrowed_type, polarity)` triple
from the condition CST. The variable must be a plain scalar (`Variable`
attachment); guards on hash elements / method returns are deferred (see
Open questions). Name→type mapping is pure `&str` and belongs in
`conventions.rs`; the CST recognition (which call shape) is builder-side
via `cst.rs` accessors.

| Guard syntax | narrows `$x` to |
| --- | --- |
| `$x->isa('Foo')`, `$x->isa(Foo::)` | `ClassName("Foo")` |
| `$x->DOES('Role')` | `ClassName("Role")` |
| `ref($x) eq 'Foo'` (Foo a class) | `ClassName("Foo")` |
| `ref($x) eq 'HASH'` | `HashRef` |
| `ref($x) eq 'ARRAY'` | `ArrayRef` |
| `ref($x) eq 'CODE'` | `CodeRef { return_edge: None }` |
| `ref($x) eq 'Regexp'` | `Regexp` |
| `blessed($x)` | *(object, class unknown)* — weak, see below |
| `defined($x)` | *(removes undef)* — weak, see below |

**`ref` reftype-token vs class** is a *closed grammar constant*, not an
open behavior set — the reftype strings are a fixed Perl language list
(`HASH ARRAY CODE SCALAR REF GLOB LVALUE FORMAT IO Regexp`). Classifying
`ref($x) eq S` by "is `S` in that constant set → rep variant, else →
`ClassName(S)`" is therefore *not* a rule-#10 shape-table (which is about
open sets of behaviors); it's reading a language constant. Put the
constant in `conventions.rs` with a comment saying so.

**`blessed` / `defined` are recognized-but-weak in v1.** Neither has a
refinement target in today's lattice — there is no "some object" type
and no undef/Optional. Recognize them (compute their variable + polarity
+ span) but emit nothing, OR emit only the negative-space removal once
the lattice can express it. They become load-bearing the moment Optional
lands (next section): `defined $x` / `blessed $x` then narrow
`Optional<T> → T`. Wiring the recognition now means the refinement is a
one-line target swap later, not new guard plumbing.

## Span + polarity — the actual design content

Two guard **positions**, two span rules:

**A. Block guard** — `if (G) { BODY }`, `unless`, `elsif`. Narrowing
span = the **BODY block span**. The narrowing asserts G over BODY.

**B. Early-exit guard** — a statement-level exit (`return` / `die` /
`croak` / `next` / `last` / `goto`) gated by a postfix/`or`/`and`
condition. Narrowing span = **[guard-statement-end .. enclosing-block
-end]** — the "rest of the block" after the guard. This is the assert
idiom and the user-confirmed shape: *scope the witness to the remainder
of the block.*

**Polarity** — does the span assert G-true or G-false?

| Form | dominated region | asserts |
| --- | --- | --- |
| `if (G) { T }` | T | **G true** ✅ |
| `if (G) { } else { E }` | E | G false ✘ |
| `unless (G) { T }` | T | G false ✘ |
| `return unless G;` | remainder | **G true** ✅ |
| `G or return;` | remainder | **G true** ✅ |
| `return if G;` | remainder | G false ✘ |
| `G and return;` | remainder | G false ✘ |

A guard-internal negation (`if (!G)`, `return unless !G`) flips the row.

**v1 shipped the G-true (✅) rows** — the guarded block and the
early-return assert. They yield a *positive* refinement (`$x` IS Foo).

**The G-false (✘) rows now land for the `defined`/`blessed` family only**
(`InferredType::Undef`, the negative lattice). The complement of "is
defined" is the concrete bottom element `Undef`, so the else of `if
(defined $x)`, the remainder of `return if defined $x`, and the body of
`unless (defined $x)` all narrow `$x` to `Undef`. Mechanically this is
`NarrowOp::negated()`: `StripOptional` negates to `To(Undef)`, and the
existing polarity plumbing (`GuardFact::op_for_region`) does the rest —
the early-exit negatives came for free; only the `else`-block emit
(`trailing_else`) was new.

The `isa` / `ref-eq` G-false rows stay wide: `To(_).negated()` is `None`
("not Foo" has no positive lookup target — no class to dispatch, complete,
or goto, so it can't improve any consumer). General negation
(`Not`/`Difference`) is parked; unblock condition is a real union lattice
+ a consumer that derives value from a negative fact (`docs/prompt
-optional-types.md`). Emitting nothing leaves the wide type — sound.

**Compound conditions:** a top-level `&&`/`and` chain narrows the body by
the *intersection* — emit one narrowing per recognized conjunct
(`if ($x->isa('Foo') && $x->can('m'))` narrows on the `isa` conjunct,
ignores `can`). A top-level `||`/`or` chain narrows nothing (neither
disjunct is guaranteed) — intentionally punted, no union to express it.

## Subjects: variables, expressions, places

A guard's subject is one of **three** kinds, and only two have existing
machinery. The narrowing must compose with both.

| Subject | identity | machinery |
| --- | --- | --- |
| `$x` — **variable** | a named slot with a lifetime | flow-sensitive span-scoped witnesses (exists) |
| `$obj->thing` — **expression** | functional derivation from receiver type | edge chase (exists) |
| `$self->{x}` — **place** | slot-like identity, *spelled* as an expression | the gap |

**The edge chase structurally cannot narrow.** Point-narrowing is gated
to `Variable` attachments on purpose (`witnesses.rs:582–589`): Expression
attachments fold *every* witness with no point filter, because a method
call's return type doesn't depend on where you ask. That referential
transparency is the feature for dispatch — and the exact opposite of
narrowing, whose whole job is that `$self->{x}` is `Foo` *here* and
`Unknown` *there*. Expressing narrowing as an edge chase is a category
error: you'd have to break the point-independence that makes the chase
correct. A **place** is slot-like (narrowable) but spelled as an
expression (the chase re-derives it functionally on each occurrence,
ignoring the narrowing) — so it falls between the two mechanisms. That
gap is the hardness.

**Why variables narrow for free but places don't — soundness.** A
variable's writes are *syntactically attributable*: `$x = …` names `$x`,
pushes a later-starting witness, temporal ordering picks the live one. A
place's writes can hide behind **aliasing**: `$self->reset` or
`frob($self)` can replace the `{x}` slot without ever naming
`$self->{x}`. A narrowing that silently survives such a mutation lies.

### v1a — variables · v1b — places (one feature, two flowing increments)

**Status:** v1a landed; v1b landed for hash/array places, **including
multi-hop** (`$self->{a}{b}`) — a chain of constant key/index
projections bottoming out at a scalar root. Zero-arg **accessor**
projections (`$self->name`) are explicitly deferred: an accessor is not
a stable slot (it can return a different object each call and may have
side effects), so any intervening call could invalidate it — sound
support needs a stricter "no call between guard and use" model, out of
scope here. A place is keyed by its source spelling on the existing
`Variable` attachment (the doc's "a `Place` witness is *just* a
span-scoped slot witness" — so the engine, narrow-filter, and scope-walk
are untouched); the place-vs-variable distinction lives only in the
truncation rule and the one `method_call_invocant_class` query seam.

The truncation rule generalizes cleanly to multi-hop: it invalidates on
an opaque use of any **proper prefix** of the place — the root scalar or
an intermediate element (`$self->{a}` for `$self->{a}{b}`) — where a
prefix is "guarded" (a read, not a disturbance) exactly when it is the
base of a longer access. So a method call on the *slot value*
(`$self->{a}{b}->m`) and a *sibling* write (`$self->{a}{c} = …`) keep
the narrowing, while a write to the slot, a root/intermediate op
(`$self->{a}->mutate`), or the prefix passed as an argument truncate it.

The narrowing soundness lives **entirely on the emit side**, so places
are *additive*, not a query-path rewrite — provided v1a's seams admit
them from day one:

- `emit_narrowing` takes a `NarrowSubject { Variable | Place(AccessPath) }`,
  never a bare `&str` variable.
- the reducer narrow-filter (`witnesses.rs:590`) reads `Variable | Place`
  from the start; the narrowest-span / temporal / scoped-exclusion logic
  is shared — at query time a `Place` witness is *just* a span-scoped
  slot witness, the reducer needn't know it's a place.
- the use-site query forms a `NarrowSubject` key generically.

**v1b adds three pieces, zero engine churn:**

1. **Access-path canonicalization (`cst.rs`).** Extend
   `canonical_container_name` (today single-container, sigil-normalizing)
   to a multi-hop path: a root (`$self` / `$x`) + projections, where a
   projection is a *constant* key/index or a *zero-arg pure accessor*
   (`->{x}`, `->[0]`, `->name`). Dynamic key `$self->{$k}` is not a
   stable place — no narrowing. Two occurrences of `$self->{x}`
   canonicalize identically.

2. **Emit-time invalidation truncation — the soundness model.** When
   emitting a place narrowing over `[guard-end .. block-end]`, scan the
   region (the builder is already walking it) and truncate the span at
   the first operation that could disturb the slot:
   - an assignment to the place or a **proper prefix** (`$self->{x} =`,
     `$self =`, `%$self =`);
   - an **opaque op on a prefix** — a method call whose receiver is a
     prefix (`$self->reset`), or a prefix passed as an argument
     (`frob($self)`).
   - **Not** invalidating: reads of the place, or method calls on the
     place *value* (`$self->{x}->render` mutates the Foo, not the slot).

   The witness carries `[guard-end .. min(block-end, first-invalidation)]`.
   Static, monotone, computed once. It **under-narrows** (conservative)
   rather than lies — past anything unprovable the place re-widens via
   the same scoped-exclusion rule.

3. **One query seam.** At a deref / method-call use site, form the place
   key for the receiver and check `Place(path)` for a live narrowing
   *before* the functional chase (`key_value_type` / `MethodOnClass`).
   Live narrowing wins; else fall through. Additive.

**The escape hatch that already works in v1a:** `my $x = $self->{x};
return unless $x->isa('Foo'); $x->render` narrows — it's a `Variable`.
Idiomatic Perl already binds slots to lexicals to avoid re-derefing, so
much real-world slot-narrowing is captured the moment v1a lands. v1b is
the "don't force the user to bind first" upgrade, on top of a flowing
v1a — not the thing that unblocks the idiom.

## Sum / optional types — the user's question

**Not scope creep — it's the complement of narrowing — but it is a
separate, sequenced lattice change, and I'd scope it to Optional/Maybe
first, not arbitrary unions.**

Why it pairs with narrowing: a function that early-returns undef
(`return undef unless $ok; return Foo->new`) produces `Foo | undef`.
Today `SymbolReturnArmFold` / `BranchArmFold` collapse arm *disagreement*
to `None` (the "1+ arms agree → Some, disagree → None" rule in
`witnesses.rs`). An `Optional(Box<InferredType>)` variant captures it
precisely instead of dropping it — and then `defined $r` / `blessed $r`
at the call site has something to bite on: `Optional<Foo> → Foo`. So the
two features are two halves of one story: **optionals create the
imprecision that narrowing resolves.** The `defined`/`blessed` guards in
the catalog above are wired *for* this.

Why it's separate and *after* narrowing v1:

- It's a **lattice-wide** change. New variant (appended at the enum END
  for bincode index stability, bump `EXTRACT_VERSION` — same discipline
  as `Sequence`/`TypeConstraintOf`/`BrandedRoute`). Every reducer fold
  that today does "agree → T, disagree → None" must instead **join** into
  `Optional`. `subsumes_narrowing` gains a meet. That's a type-system
  commit, not a narrowing rider — bundling them makes narrowing
  un-shippable until the join is right.
- Narrowing v1 stands alone against the existing lattice (ClassName /
  HashRef / ArrayRef / CodeRef / Regexp) and ships small. The weak
  guards no-op until Optional arrives, then flip their target in one
  line.

Why **Optional, not full unions**: arbitrary `Foo | Bar` is a bigger
lift (n-ary join, method dispatch over a union receiver) and is YAGNI
until a non-undef union has a motivating case. `Maybe<T>` (value-or
-undef) covers the dominant Perl idiom and maps 1:1 onto Type::Tiny's
`Maybe[T]` / `Optional[T]`, so it also feeds the `type_constraint_names()`
seam the Type::Tiny guards will use. It also *is* the join for the
already-queued "Conditional-reassignment disagreement-to-widen" item and
for the arm-disagreement `None` — all three are the same missing-join
gap.

**Recommendation:** land narrowing v1 against the current lattice; open a
sibling roadmap entry for `Optional<T>` (its own design doc) as the
immediate follow-on; park arbitrary sum types until a `Foo|Bar` case
demands them.

## Open questions

1. **Place subjects** — `ref($self->{x}) eq 'Foo'`,
   `$self->{x}->render`. Designed in §"Subjects" as v1b (canonical
   access paths + emit-time invalidation truncation + one query seam).
   Not punted — v1a's seams (`NarrowSubject`, the `Variable | Place`
   filter) admit it additively. A *method-return* subject
   (`$obj->thing->isa`) with no backing slot stays out — that's the
   untyped-boundary problem (`open-problems.md`), not a place.
2. **Loop-condition narrowing** — `while (my $x = shift) { ... }` (defined
   in body). Ties to Optional; defer with it.
3. **Reassignment inside the region** — sound for free: the reassignment
   pushes a later-starting witness; temporal + narrowest-span pick the
   live one at each point. Worth a playground row to pin.
4. **Negative / else / `return if`** — deferred to Optional/union (above).
5. **Postfix-on-block-exit precision** — does the early-exit span end at
   the lexical block, or follow `last`/`next` semantics into the loop?
   v1: lexical enclosing block (the conservative, common case).

## Test plan

- Substrate is proven: `narrowed_span_wins_over_outer_witness_at_inside
  _point` (keep as the engine regression).
- `test_files/narrowing_playground.pl` is the authoring source — every
  row carries a `# => Type` annotation at a point. Drive with
  `perl-lsp --dump-package` / the gold harness `--emit`.
- Per-row unit tests in `builder_tests.rs`, red-pinned `#[ignore]` until
  v1 lands (repo precedent: the cross-file ClassIsa probe), unignored as
  each row goes green. A row that should NOT narrow (the ✘ polarity
  rows, the `||` case) gets a negative-space test asserting the wide
  type still wins — these are the guardrails against over-narrowing.
