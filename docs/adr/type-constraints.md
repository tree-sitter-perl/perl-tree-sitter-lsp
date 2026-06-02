# ADR: `TypeConstraintOf` — a Type::Tiny constraint is not the class it constrains

`has x => (isa => InstanceOf['Foo'])` should give the accessor `x` a
return type of `Foo`, so `$self->x->method` resolves. The obvious fix
— map `InstanceOf['Foo']` to `ClassName(Foo)` — is wrong, and wrong in
a way that the rule-#10 "lossy projection" trap predicts.

`InstanceOf['Foo']` evaluates to a `Type::Tiny` *value*. You call
`->check` / `->assert_valid` / `->coerce` / `->name` on it, never
`Foo`'s methods. Typing it `ClassName(Foo)` conflates the constraint
with the thing it constrains: `$constraint->name` would wrongly
resolve against `Foo`. The accessor yields a `Foo`; the isa
*expression* is a constraint over `Foo`. Two types, related by "the
accessor yields what the constraint constrains."

## Decisions worth keeping

### The constraint is a wrapper type; the accessor projects through it

```rust
pub enum InferredType {
    // … existing variants
    TypeConstraintOf(Box<InferredType>),   // a Type::Tiny constraint over the inner
}
```

A value typed `TypeConstraintOf(X)` dispatches methods against
`Type::Tiny` (when that's indexed); the inner `X` is recoverable by
projection. `InferredType::constrained_inner()` is the accessor —
consumers ask the value, they never destructure the serde shape.
Pairs with `Sequence(Vec<_>)` / `Parametric(_)` (inner-carrying) — the
`…Of` reads as "constraint of `<inner>`".

### Core extracts the params; the plugin folds them to the inner

A type library exports a vocabulary of constructors of varying arity
(`ArrayRef` at 0, `InstanceOf['Foo']` at 1, `Enum['a','b']` at N).
Core can't enumerate shapes — it hands the plugin a param list and the
plugin folds it. The split obeys rule #1: only the builder walks the
CST.

- **Core** intercepts a call whose name is in the plugin's
  `type_constraint_names()` gate, extracts each param as a
  `ConstraintParam { string, ty }` (`'Foo'` → `{string}`; a nested
  constructor → `{ty}`, typed through the bag — see below), and wraps
  the plugin's fold result in `TypeConstraintOf`.
- **Plugin** (`frameworks/type-tiny.rhai`) declares the names and a
  `type_constraint_inner(name, params)` fold returning the inner type
  or `()`. Arity lives in the fold, not the core: a constructor takes
  0/1/N params and the fold branches on `params.len()`. New
  constructor = a name + a few lines of fold, zero core change.

This is one member of the declarative-manifest family in
`adr/plugin-system.md` — the plugin owns the vocabulary, the core owns
the mechanism.

### The accessor unwraps; the constraint value does not

`has` resolves the isa RHS expression's type through the bag and asks
the *constraint* what it constrains — `bag_query_expr_span(rhs)
.constrained_inner()`. The unwrap is the accessor's projection, not a
property of the constraint value: `my $t = InstanceOf['Foo']` keeps `$t`
typed `TypeConstraintOf` so `$t->name` resolves against the constraint,
while `has x => (isa => $t)` gives `x` the inner `Foo`. Asking the value
its question (rule #10) is why the same path covers the bare constructor,
the const-folded binding (`isa => $t`), and a `CodeRef` isa
(`isa => sub {...}` has no constrained inner → correctly untyped) with no
per-shape branching in `has`.

### Nested vocabulary recurses through the same expr typing

`Maybe[InstanceOf['Foo']]`'s single param is the call
`InstanceOf['Foo']`. The core types it *through the bag*
(`emit_expr_witness(el); bag_query_expr_span(el)`) and lands it in
`ConstraintParam.ty`. Because this reuses `expr_payload` — the path the
outer call already walks — it recurses to arbitrary depth
(`ArrayRef[InstanceOf[X]]`, `Dict[...]`) for free. The plugin asks the
value its question via a `constrained_inner(ty)` Rhai helper mirroring
`InferredType::constrained_inner`, never the serde shape.

## `Maybe[T]` is erased, not modeled — and that is on purpose

`Maybe[InstanceOf['Foo']]` resolves to `TypeConstraintOf(ClassName
(Foo))`, identical to the bare `InstanceOf['Foo']`. The plugin's
`Maybe` fold is a **passthrough** — it returns the inner's constrained
type; the optionalness ("might be undef") is discarded.

Erasure is the right call because it satisfies every *resolution* need
— goto-def, hover, completion, chain dispatch all want the inner class.
A first-class `InferredType::Maybe(_)` would buy exactly one capability
erasure can't: the unguarded-optional-access diagnostic (`$t->process`
on a `Maybe` with no intervening `if ($t)` / `//` guard). That
capability is not buildable on the variant alone — it needs
flow-sensitive guard narrowing the engine doesn't have, and to avoid
being a misleading partial lint it needs optionalness sourced from
`//` / guards / `->{k}` / signature defaults, not just `Maybe[...]`.

A speculative variant would ripple through every `match` on
`InferredType` (the "never `_ =>`" invariant means every consumer must
handle it), every reducer, the bincode wire format, and the
`class_name()` / `constrained_inner()` / `element_at()` projection
family — all to carry a bit nothing reads. The slot stays clean for a
later landing: a delegating `class_name()` (so dispatch sees through,
like `Parametric` delegates to its flavor) plus a new `maybe_inner()`
projection, the plugin fold flips passthrough → wrap. Additive, not a
refactor — which is *why* deferring is safe.

**Revisit only when the nullability diagnostic is committed as a
feature, and budget the flow-narrowing + idiom-sourcing with it.**

## Trade-offs

**`EXTRACT_VERSION` bump** for the `TypeConstraintOf` variant and the
nested-`ty` shape. Bumping is free; old blobs re-resolve lazily.

**Constraint-name registration is global today.** The
`type_constraint_names()` gate is a flat list — the cheap first cut. The
authoritative source is the *import* (`use Types::Standard qw/InstanceOf
.../` injects exactly those names), so an unrelated `InstanceOf` in a
package that didn't import it can mis-fire. Moving registration to the
injection seam (the `use` / `SyntheticUse` / `FrameworkImport` handling)
is package-scoped and free for synthesized libs (crm's `Clove::Types`
re-exporting Types::Standard rides its kit's `SyntheticUse`).
Forward-compatible: only the name-gate migrates; the fold and the
`TypeConstraintOf` / `has`-projection plumbing are unchanged.

## What's deferred

- **Method dispatch on a constraint value** (`$t->assert_valid`,
  `->check`, `->coerce`) → `Type::Tiny`. No-op until `Type::Tiny` is
  indexed from CPAN; the projection design above is chosen partly so
  this composes — `$t` keeps its `TypeConstraintOf` type for dispatch
  while the accessor projects the inner.
- **Richer vocabulary** (`ArrayRef[InstanceOf[X]]`, `Enum`, `Dict`,
  `ConsumerOf['Role']`). The `ty`-filling plumbing exists; each is one
  fold entry.
