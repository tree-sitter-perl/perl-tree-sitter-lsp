# Receiver-relative return types — `ReturnExpr`

**Status:** queued. Next architectural pillar after the parametric
flavor redesign (`docs/adr/parametric-types.md`). Subsumes per-
method projection (DBIC `find` etc.) AND arity dispatch (Mojo
`has` accessors) in one mechanism.

Two existing mechanisms collapse into one: the call-site emitter
that bakes per-method projection (`extract_resultset_parametric` +
flavor `return_projection`), and the arity-dispatch family
(`FluentArityDispatch` + `ArityReturn` + its bespoke reducer).
Both are "the symbol's return type depends on context" — receiver
in one case, args in the other. Symbol-declarative wins for the
same reason `RowOf` did: the rule belongs on the type, not at
every call site.

## The shape

```rust
pub enum ReturnExpr {
    /// Concrete type — same as today's `Option<InferredType>`.
    Concrete(InferredType),
    /// `Receiver` placeholder — substituted with the call site's
    /// receiver type at query time.
    Receiver,
    /// Apply a parametric operator to a sub-expression.
    /// `find`'s return is `Operator(RowOf, Receiver)`.
    Operator(ParametricType /* with sub-exprs as ReturnExpr */),
    /// Union over arg-shape — subsumes arity dispatch.
    /// Mojo fluent accessor: `{ args.is_empty() => T, _ => Self }`.
    UnionOnArgs { branches: Vec<(ArgGuard, ReturnExpr)> },
}

pub enum ArgGuard {
    Empty,
    AtLeast(u32),
    Exact(u32),
    Any,
}
```

## What it subsumes

**Per-method projection.** DBIC's `find` declares
`return_type: ReturnExpr::Operator(RowOf(Receiver))` once on
the symbol. The bag substitutes Receiver at each call site,
evaluates `RowOf`, returns the projection. The plugin's
`on_method_call` emit hook for find/search/etc. retires —
symbol-declarative wins.

**Arity dispatch.** Mojo `has 'name' => 'default';`:

```rust
return_type: ReturnExpr::UnionOnArgs {
    branches: vec![
        (ArgGuard::Empty, ReturnExpr::Concrete(String)),
        (ArgGuard::Any, ReturnExpr::Receiver),
    ],
}
```

`FluentArityDispatch`, `ArityReturn` observation variant,
`ArityBranch` enum — all retire. One union on the symbol,
evaluation is the same machinery that evaluates `RowOf`.

**Future:** truthiness dispatch, `wantarray` context, type-of-
arg dispatch — all become new `ArgGuard` variants. No new
reducers, no new bespoke families.

## Cost & sequencing

Adding `ReturnExpr` + the substitution machinery is ~200–300
LOC + retiring the arity reducer's bespoke parts. Buys:
- Per-method projection moves from call-site emission to
  symbol-declarative.
- Arity dispatch family collapses to the union shape.
- Future `*-on-args` dispatch is mechanical.

Sequencing:
1. `ReturnExpr` + substitution + `ParametricProjectionReducer`
   extension. Land alone, no consumer change yet.
2. Migrate Mojo `has` to `UnionOnArgs`. Retire
   `FluentArityDispatch` + `ArityReturn`.
3. Migrate DBIC plugin's per-method emission to
   `Operator(RowOf, Receiver)` declared on the symbol.
   Drop the call-site `extract_resultset_parametric` emission
   (or keep it as a fallback for non-symbol-declarative
   plugins).

## Adjacent

- **First-class function types.** Today `my $sub = sub {...};
  $app->helper(thing => $sub)` doesn't propagate the sub-literal
  type through the scalar binding — the helper plugin sees
  `args[1].inferred_type: CodeRef`, no body span. ReturnExpr
  doesn't fix this directly; needs a Function<Args, Ret> shape
  (queued separately).
- **Effect facts.** `Promise<X>` carries IO; `Try<X>` carries
  exception possibility. Effects on `Wrapped` (regardless of
  class) compose with the inner T's facts. ReturnExpr's
  `Operator` slot is a natural attachment point.
