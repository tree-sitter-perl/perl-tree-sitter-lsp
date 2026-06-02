# Type::Tiny constraints as a first-class type (`isa => InstanceOf['Foo']`)

> **CORE LANDED** (`InferredType::TypeConstraintOf`, the `has` projection, the
> `type_constraint_names()`/`type_constraint_inner()` plugin surface,
> `frameworks/type-tiny.rhai` for `InstanceOf`/`ConsumerOf`). The manifest
> family is summarized in `docs/adr/plugin-system.md`. **Residual / deferred**
> (still forward work): method dispatch on a constraint value → `Type::Tiny`;
> richer/nested vocabulary (`ArrayRef[InstanceOf[X]]`, `Maybe`, `Enum`, `Dict`);
> moving constraint-name registration from the global gate to the import seam
> (so it's package-scoped — `Clove::Types` re-exporting Types::Standard).

> A Moo/Moose attribute declared `has x => (isa => InstanceOf['Foo'])` should
> give its accessor a return type of `Foo`, so `$self->x->method` types. Today
> it returns `None` — the parametric Type::Tiny constraint never resolves.
>
> The fix is NOT "map `InstanceOf['Foo']` to `ClassName(Foo)`." That's wrong:
> `InstanceOf['Foo']` is a *value* — a Type::Tiny constraint object — and you
> call `->check` / `->assert_valid` / `->coerce` on it, never `Foo`'s methods.
> Its type is "constraint constraining to Foo". Model that honestly and the
> accessor return falls out by projection.
>
> Design direction only — implementation deferred to a later round.

## Why the obvious fix is wrong

`InstanceOf['Foo']` evaluates to a `Type::Tiny` object. Typing it as
`ClassName(Foo)` would (a) let `$constraint->name` wrongly resolve against
`Foo`, and (b) conflate the constraint with the thing it constrains. The
accessor returns a `Foo`; the isa *expression* is a constraint. Two different
types, related by "the accessor yields what the constraint constrains."

## The model

A new variant in the `InferredType` zoo:

```rust
TypeConstraintOf(Box<InferredType>)   // a Type::Tiny constraint over the inner type
```

Naming: pairs with `Sequence(Vec<_>)` / `Parametric(_)` (inner-carrying) and
with `CodeRef { return_edge }` (a type that carries a projection). `…Of`
reads as "constraint of `<inner>`".

Two consumers know how to read it ("what to do with TypeConstraint types"):

1. **Projection (this round):** the inner type is recoverable. The isa→accessor
   path projects `TypeConstraintOf(X)` → `X`.
2. **Method dispatch (deferred — see below):** a value typed
   `TypeConstraintOf(_)` dispatches methods against `Type::Tiny`, not the inner
   type.

## Who produces it (the `has` "split", same output shape)

`has` never branches on the isa shape — it always edges to the RHS expression
and unwraps. The split is only in *who types the RHS*:

- **Constructor form** `InstanceOf['Foo']` — a call to `InstanceOf` with an
  arrayref arg. The **Type::Tiny plugin** owns the vocabulary (which
  constructor names are constraints and their shape); the **builder** extracts
  the `'Foo'` param from the CST (rule #1 — a plugin may not walk nodes, and
  re-parsing `ArgInfo.text` is the lossy antipattern we rejected). Result:
  `TypeConstraintOf(ClassName(Foo))`.
- **String / bareword form** `'Str'`, `'My::Class'` (the Moose idiom) — the
  **core** meaning-map (`map_isa_to_type`) resolves the string's *meaning*
  (note: meaning ≠ the string literal's type — `'Int'` the literal is a
  `String`, but as an isa it means `Numeric`), respecting constant folding
  (`my $t = 'Str'; isa => $t`). Result: `TypeConstraintOf(String)` etc.

Both land as `TypeConstraintOf(inner)`, so downstream is uniform.

## Plugin surface: providing constraints (core extracts, plugin folds)

A type library exports a vocabulary of constructors (`InstanceOf`, `ArrayRef`,
`Enum`, …) of varying arity (0 args: `ArrayRef`; 1: `InstanceOf['Foo']`; N:
`Enum['a','b']`, `Dict[k=>Str,…]`). So the core can't enumerate "shapes" — it
hands the plugin a **param list** and the plugin folds it.

Core contract (rule #1 — only the builder walks nodes): for an intercepted
call `Name[p1, p2, …]`, extract
```
params: [ ConstraintParam { string: Option<String>, ty: Option<InferredType> } ]
```
(`'Foo'` → `{string:"Foo"}`; a nested `Int` / `InstanceOf[...]` → `{ty:<resolved>}`
— the `ty` slot is where nesting lands later). The core then wraps the plugin's
result: `TypeConstraintOf(<result>)`. The plugin never sees a node nor builds
the `TypeConstraintOf`.

Plugin surface — declare-then-fold (mirrors `has_on_X` + the hook):
```rhai
fn type_constraint_names() { ["InstanceOf", "ConsumerOf"] }   // cheap dispatch gate
fn type_constraint_inner(name, params) {                      // fold → inner type, or ()
    if params.is_empty() { return (); }
    type_class(params[0].string)        // InstanceOf['Foo'] -> ClassName(Foo)
}
```
```rust
fn type_constraint_names(&self) -> &[String] { &[] }
fn type_constraint_inner(&self, name: &str, params: &[ConstraintParam]) -> Option<InferredType> { None }
```
Arity lives in the fold, not the core: `Enum[...]`→`type_string()` (ignores N
params), `ArrayRef` vs `ArrayRef[Int]`→branch on `params.len()`. New
constructors = a name + a few lines of fold, zero core change. Any plugin
(MooseX::Types, Specio, a house lib) does the same.

### Future: registration moves to the injection level

The name-gate (`type_constraint_names()`) is a **global** list now — the cheap
first cut. But constraint constructors are *synthesized and exported* by a type
library, not hand-written; `use Types::Standard qw/InstanceOf .../` injects
exactly those names into the package. So the authoritative source is the
**import**, not a global list. Next iteration moves name registration to the
injection seam (the `use` / `SyntheticUse` / `FrameworkImport` handling): a type
library's plugin declares "I export these constraints, here's each fold" as part
of its import, making it **import-scoped** (an unrelated `InstanceOf` in a
package that didn't import it won't mis-fire) and free for synthesized/custom
libs. crm's `Clove::Types` is a `Type::Library` re-exporting Types::Standard +
Types::Common — it rides the clove-class kit's `SyntheticUse`, so the same seam
covers it. **Forward-compatible:** only the *name-gate* migrates; the
`type_constraint_inner` fold and the `TypeConstraintOf` / `has`-projection
plumbing are unchanged.

## What `has` does (uniform, edge-based)

The synthesized accessor's return witness is a **projection edge** to the isa
RHS expression that unwraps a constraint:

```
Symbol(accessor) → ConstrainedValueEdge(Expr(rhs_span))
```

At query time: resolve `Expr(rhs_span)`; if it's `TypeConstraintOf(X)`, the
accessor returns `X`; otherwise it returns nothing (a non-constraint isa, e.g.
`isa => sub {...}` → `CodeRef`, has no constrained type — correctly untyped).

Why a *dedicated* projection payload rather than a plain `Edge` + a reducer:
a bare `my $t = InstanceOf['Foo']` must stay a `TypeConstraintOf` (so `$t->name`
works); we only unwrap in the isa→accessor context. Encoding the unwrap on the
edge keeps it local and explicit, mirroring `CodeRef { return_edge }`.

Composes "for free":
- `my $t = InstanceOf['Foo']; has x => (isa => $t)` — `$t` is
  `TypeConstraintOf(...)` via the binding; the accessor edge unwraps it. No
  special path.
- `isa => sub {...}` / `isa => \&validator` — `CodeRef`; not a constraint; no
  unwrap; untyped. Uniform, no per-shape branching in `has`.

## crm payoff

`Clove::PostExternal`: `has _minion => (isa => InstanceOf['Clove::Minion'])`
→ `_minion` returns `Clove::Minion` → `$self->_minion->enqueue('…')` types →
option-B dispatch resolution lights up the last plain-`enqueue` site. And the
win generalizes: every `$self->attr->method` chain where `attr` is an
`InstanceOf['X']` attribute starts resolving (hover / goto-def / completion).

## Deferred to the next round (jot only)

- **Method dispatch on a constraint.** `$t->assert_valid(...)` / `->check` /
  `->coerce` / `->name` should resolve against `Type::Tiny`. Needs
  `TypeConstraintOf(_)` to route method dispatch to the `Type::Tiny` class.
  No-op until `Type::Tiny` is indexed (CPAN), so low urgency — land the type +
  accessor projection first. The projection design above is chosen partly so
  this composes: `$t` keeps its `TypeConstraintOf` type for dispatch while the
  accessor projects the inner.
- **Richer vocabulary.** `ArrayRef[InstanceOf['X']]`, `Maybe[...]`,
  `Dict[...]`, `Enum[...]`, `ConsumerOf['Role']`. Each is a plugin manifest
  entry once the constructor→`TypeConstraintOf` path exists; nesting
  (`ArrayRef[InstanceOf[X]]`) needs the inner constructor to type first, then
  the outer to wrap — recursion through the same expr-typing.

## Scaffolding that already exists (reuse, don't rebuild)

A prior pass began the plugin-manifest plumbing; it was reset pending this
design, but the shape is right and reusable:
- `plugin::DispatchVerb`-style manifest hook `type_constraints()` on the trait
  + `PluginRegistry` + the Rhai reader (`fn type_constraints()`), declaring the
  constructor vocabulary.
- `frameworks/type-tiny.rhai` — a pure-manifest plugin.
- Builder-side structural extraction of a parametric constraint node
  (function name + first string param) — keep this in core (rule #1).

What changes vs that pass: the produced type is `TypeConstraintOf(inner)`, not
`ClassName`; and `has` consumes it via the `ConstrainedValueEdge` projection
instead of a constraint-aware branch inside `visit_has_call`.

## Acceptance (next round)

- `has x => (isa => InstanceOf['Foo'])` → accessor `x` returns `Foo`
  (`--dump-package` `bag_return_type` = `Foo`).
- `$self->x->method` resolves cross-file.
- `my $t = InstanceOf['Foo']; isa => $t` → same (const-fold path).
- `$t->name` (a constraint method) does NOT resolve to a `Foo` method
  (stays a `TypeConstraintOf` — guards against the lossy `ClassName` shortcut).
- String forms unchanged: `isa => 'Str'` → `String`, `isa => 'My::Class'`
  → `ClassName(My::Class)` accessor return.
- crm `$self->_minion->enqueue('Pipedrive.sign_up')` resolves to the task.
