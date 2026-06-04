# Aspirational type-system features (NEW features — NOT QA-pass FP fixes)

Two capabilities we want but have deliberately **not** built. Both are *new features*, not
false-positive cleanups, so they sit outside the QA-fix loop and land on their own schedule.
Both **extend what the witness bag carries**, and both take `InferredType::BrandedRoute`
(`docs/adr/route-branding.md`) as the mechanism precedent — *option C collapsed*: extra
resolved info lives **in** the carried value (no side-table, no id), rides the bag's existing
edges/fold for free, and is read through the one chain typer. The two differ in **which edges**
the payload rides and **how much control-flow** they need.

---

## 1. Narrowing (flow-sensitivity) — the "we don't model conditionals" pillar

A guard refines a variable's type *within a branch*:

```perl
if (ref $x eq 'Foo')                  { $x->foo_method }   # $x is Foo inside the if
if (Params::Util::_INSTANCE($x,'Foo')){ $x->foo_method }   # ditto (the QA NARROW-1 case)
return unless $x->isa('Bar');         # $x is Bar after the guard
```

**Why it's deferred / hard:** we do not model conditionals *anywhere*. Narrowing is intrinsically
flow-sensitive — a type that holds only on one side of a branch and must re-widen at the join.
That needs a branch/flow layer we've never built. Do it as **one pillar**, not piecemeal — picking
off `isa` then `ref eq` then `_INSTANCE` is the rule-#10 partial-enumeration trap.

**Cases that live behind this wall (group them):**
- **NARROW-1** — `ref $x eq 'C'` / `_INSTANCE($x,'C')` / `$x->isa('C')` / `$x->can('m')` guards.
- **A4's cross-procedural tail** — a hash slot written by a constructor in *another* file
  (path-dependent; the same-file write case is conditional-free and is the actual A4 round).
- **re-export form (4)** — runtime `Mod->import` delegation inside `sub import` (control-flow).

**Sketch (when we build it):** branch-span-scoped narrowing witnesses — a witness valid only within
`[branch_start, branch_end)`. The query already carries a `point`, so a point-scoped witness reads
correctly inside the branch and is invisible outside it; the hard part is the join/else re-widening
and negative guards (`unless`, early `return`/`die` flipping the fallthrough). Seeds: a small,
type-encoded set of guard recognizers (`isa`/`can`/`ref eq`/`_INSTANCE`), recognized by shape but
folded into a single "narrows `$v` to `C` on the true edge" witness, not consumer branches.

---

## 2. Effects (throws) — "ride like brands", on the call graph

Track which subs can **throw** (`die`/`croak`/`confess`/`->throw`), propagate it up the call graph,
and mask it at `eval`/`try`. The user's framing — *effects ride like brands* — is right at the
mechanism level. (Throws first; the effect-set generalizes — pure / IO / mutates — later.)

**Brand-style mechanism (what carries over):**
- **Collapse into the carried value, no side-table.** Carry an `effects: EffectSet` alongside the
  `Symbol`'s reduced return value (`{ return: T, effects }`), exactly as `BrandedRoute` carries its
  defaults *in* the type rather than a parallel store (which "drifts" — bag-canonical ADR).
- **Ride the bag's fold to a fixpoint.** Effects propagate along the **call/return edges** the bag
  already chases for return-types (`Symbol`, `MethodOnClass`, call-bindings). A call to a `Throws`
  sub adds `Throws` to the caller. Monotone (effect-set only grows) over a finite lattice ⇒ the
  existing worklist fixpoint terminates with no new driver.
- **Overlay/union on combine**, like the brand overlay: an expression's effects = union of its
  sub-expressions' / callees' effects.

**Where it DIFFERS from brands (the important part):**
- Brands ride **value-flow** edges (assignment / method chain / nesting). Effects ride **call-flow**
  edges (the call graph). Same machine, different edges.
- Effects are **not** an `InferredType` variant — the return type is orthogonal to "can it throw."
  The payload hangs on the `Symbol`/call witness, not on the value's `InferredType`.

**Seeding (walk-time, recognize by shape):** a body that directly `die`/`croak`/`confess`/
`Carp::croak`/`->throw`s ⇒ seed `Throws` on that `Symbol`. The throw-primitive vocabulary is a small
real set (die/croak/confess/…), plugin-ownable later (same shape as the exporter vocabulary).

**The one control-flow concession — and why it's tractable:** `eval { }` / `try { }` **catches**, so
effects inside it are masked. This is the *only* flow bit effects need, and it's a **syntactic scope
boundary** (a block, like a sub scope), **not a general conditional** — so it does NOT require the
narrowing pillar above. "Subtract Throws within an eval/try block" is a scope rule we can do today.
*Dark tail:* an `eval` that re-throws (`die $@ if $@`) or partially catches — the common
"wrap-in-eval ⇒ caught" is clean; the re-throw case is the documented boundary.

**Payoff (why the user wants it IRL):** opt-in diagnostic "call to `X` may die and isn't in an
`eval`," hover "throws", and it *composes* — the effect propagates up so a leaf `croak` surfaces at
every uncaught caller. `throws` alone is a large real-world unlock; the effect-set is the extensible
frame for the rest.

---

## Relationship to A4 (hash-slot typing — a QA-pass item, not a future)

A4 (`my $x = $self->{field}; $x->m`) is the **value-flow** cousin and is literally brand-shaped:
a slot's resolved type rides value-flow through the bag, "no evidence ⇒ untyped, never HashRef."
Its **same-file-write** case is conditional-free and is a normal QA round (Pillar 2). Only its
**cross-file / conditional-write tail** falls behind the narrowing wall (§1). So: A4-same-file = do
it; A4-cross-proc = narrowing pillar; effects = §2.
