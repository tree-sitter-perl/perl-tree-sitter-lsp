# Type-system encoding for type axes + bag attachments

**Status:** discussion. Two related questions deferred to graph-
walking (or sooner if cheap).

## Problem 1 — the dual-class read

Phase 1 of Part 5c surfaced that `Parametric { base, type_args }`
answers two questions:
- What class do this value's *methods* come from? (base)
- What class do this value's *hash-key args* belong to? (type_args[0])

Today both are accessor calls on `InferredType`:

```rust
ty.class_name()          // dispatch — base
ty.hash_key_class()      // hash-key args — type_args[0]
```

A consumer that grabs the wrong one silently misresolves. This
already bit me once: I drafted `hash_key_lookup_class` with a
hardcoded `target_name in {search,...}` allowlist before the rule
was lifted onto `InferredType`. The compiler had no way to flag
"you read `class_name()` when you should have read `hash_key_class()`."

## What the type system could encode

### A. Phantom type parameter on a `Class` newtype

```rust
#[derive(Clone, Debug)]
pub struct ClassFor<Axis> { name: String, _axis: PhantomData<Axis> }

pub struct DispatchAxis;     // method dispatch
pub struct HashKeyAxis;      // hash-key arg owner
pub struct ElementAxis;      // collection element narrowing (future)

impl InferredType {
    pub fn dispatch_class(&self) -> Option<ClassFor<DispatchAxis>> { ... }
    pub fn hash_key_class(&self) -> Option<ClassFor<HashKeyAxis>> { ... }
}

fn lookup_hash_keys(c: &ClassFor<HashKeyAxis>) { ... }
fn resolve_method(c: &ClassFor<DispatchAxis>, name: &str) { ... }
```

A consumer that calls `dispatch_class()` and passes the result to
`lookup_hash_keys` gets a type error. Wrong-axis reads become
unrepresentable.

**Cost:** every existing `Option<String>` consumer site touches
the type. Refactor scope is real but mechanical — the compiler
drives the work. Estimated 30-50 sites, maybe a day of focused
work.

**Risk:** boilerplate at axis-naive sites that don't care which
axis they got (e.g. logging, debug formatting). Either every
Display impl handles all axes, or there's an `.into_string()`
that drops the phantom — the latter mostly works.

### B. Trait-based axis dispatch

```rust
trait DispatchClass { fn dispatch_class(&self) -> Option<&str>; }
trait HashKeyArgClass { fn hash_key_arg_class(&self) -> Option<&str>; }
trait ElementType { fn element_type(&self) -> Option<&InferredType>; }

impl DispatchClass for InferredType { ... }
impl HashKeyArgClass for InferredType { ... }
```

Same idea as A but without phantom types — the trait method NAME
is the discrimination. Consumer imports the trait it needs; calling
the wrong method requires the wrong import.

**Cost:** lighter than A. Each axis adds one trait. No phantom
machinery. Can dispatch generically (`fn x<T: HashKeyArgClass>(t:
&T)`).

**Risk:** consumers can still call BOTH traits' methods on the
same value if they import both. The discipline is "import only
the trait that fits your question" — nudge, not enforcement.

### C. No type-system encoding, lint-style discipline

Today's state. Document the axes (we did, in the parametric-types
ADR). Code review catches wrong-axis reads. Tests cover the cases
where it matters (we have them).

**Cost:** zero today, accumulates as the type model grows. Each
new axis (Element, Wrapped, Effect, …) widens the gap.

**Risk:** silent misresolution in the long tail. The dual-class
case bit *us*; the next one will too.

## Problem 2 — two bag attachments for "expression value"

`Expr(span)` and `Expression(refidx)` both represent "the value of
an expression." They're connected by a single `Edge` witness emitted
from `expr_payload` for method-call shapes. Implementing Part 5c, I
emitted at the wrong attachment first; the chain receiver lookup
reads `Expression(refidx)`, the variable-binding chain typer reads
through `Expr(span)`, and the edge between them is invisible at
the call site.

### Why two?

- `Expr(span)` is the D2 unification: every expression value gets
  one. Used by branch-arm folds, return-arm propagation, the
  implicit-last-expression chain.
- `Expression(refidx)` is per-call-ref: it's the slot
  `emit_method_call_return_edges` publishes Edge(MethodOnClass)
  into. Method-call return-type queries read it.

For non-call expressions there's no `Expression(refidx)` — only
Expr(span). For call expressions, both exist; the edge from
Expr(span) → Expression(refidx) is what makes them feel like "the
same thing" at query time.

### What's awkward

- Emitters need to know which attachment to push to. The right
  answer is "publish at the structurally-correct one and let the
  Edge propagate" — but discovering this requires reading
  `expr_payload`'s closed-under-syntax shape, not obvious.
- Consumers need to know which one to read. `bag_query_expr_span`
  vs `bag_query_expression(refidx)` are both registry queries on
  different attachments. They should agree (modulo the edge
  chase) but you have to know to pick.
- Ref attachments at all (`Expression(refidx)`) feel like a
  graph-walking artifact: the call ref is a node, the witness
  is an edge. The graph-walking pillar
  (`docs/prompt-graph-walking.md`) probably collapses both into
  edges of one typed-edge graph, with `(span, attachment_kind)`
  as the natural key.

### Options

**Defer to graph-walking.** The rework is queued anyway. The
disciplined-for-now answer: a docs note in CLAUDE.md saying
"emitters publish at `Expression(refidx)` for call-shaped
expressions, `Expr(span)` for the rest; consumers read `Expr(span)`
and let the Edge resolve through; they're connected via D2's
unification."

**Collapse pre-graph-rework.** Make `Expr(span)` the only
attachment — remove `Expression(refidx)`. Method-call return
edges retarget to `Expr(span)` of the call-expression node.
Loses the per-ref-idx keying; `RefIdx` mostly exists for chain-
typing-index-style invocant lookups, which can also be span-keyed.
Maybe smaller than it sounds. Worth measuring.

**Document and live with it.** The bag is canonical; the two
attachments are by-construction correct; the edge ties them.
Consumers who confuse them get bugs that integration tests catch
quickly (this PR is evidence). Cost of doing nothing is the same
as #2-C above — accumulates, eventually one bug slips past tests.

### Recommendation

For dual-class (problem 1): **option B (traits)** is the lightest
encoding that gets the safety guarantee. Cheap enough to land
before graph-walking. **Option A (phantom types)** is the strongest
form — defer until the full axis set is known (will end up with
~5 axes once Element / Wrapped / Effect land).

For two-attachments (problem 2): **defer to graph-walking** with a
CLAUDE.md note. The collapse-pre-graph option is cheaper than
expected but also wasted work if graph-walking restructures
attachments anyway. Cost-benefit not in our favor right now.

## Open questions

1. Trait-based axis dispatch (1B) interacts with the "everything
   is a type" direction. Does each trait correspond to a TypeArg
   slot, or are some axes purely behavioral?
2. If we do (1A) phantom types, does plugin-emitted `Custom(idx)`
   parametric semantics (see `prompt-return-type-expressions.md`)
   need a corresponding `Custom<Idx>` axis marker, or do plugins
   pick from the fixed set of axes?
3. For (2), does the eventual collapse keep `Expr(span)` or
   `Expression(refidx)` as canonical? Span-keyed plays better with
   tree-walking; refidx-keyed plays better with serde stability.
