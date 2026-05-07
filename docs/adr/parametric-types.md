# ADR: Parametric types — recursive `TypeArg` from day one

## Context

Residual Part 5c (`docs/prompt-type-inference-residual.md`) added type
intelligence for parametric collections — DBIC's
`$schema->resultset('Users')->search({ name => ... })` was the
motivating case. Hash-key completion in `search({...})` resolves
against the row class (Users' columns), but method dispatch on the
same value (`$rs->all`, `$rs->count`) resolves against the resultset
base class. One value carries two type dimensions.

Open question at design time: **does the parametric type carry its
type arg as a flat string (`type_arg: String` or `type_args:
Vec<String>`), or as a recursive enum that admits boxed
`InferredType`?**

Strawman cases that motivated the question:

| Shape | Flat `Vec<String>` viable? | Recursive needed? |
|---|---|---|
| DBIC `ResultSet<Users>` | yes | no |
| DBIC `prefetch` joined unions (`Users + Posts`) | yes — flat list of class names | no |
| Mojo::Promise<X> + async | only if X is a flat class | yes if X is itself parametric (`Promise<ResultSet<Users>>`) |
| Type::Tiny `HashRef[ArrayRef[Str]]` | no — direct recursive constraint shape | yes |
| GraphQL `[[User!]!]!` | no — recursive by spec | yes |
| Future `Result<T, E>` style | flat OK for two flat args | yes when T or E are themselves typed |

The CRM repo's existing usage of Mojo::Promise (10+ files; `await`,
`->then(sub ($x) {...})`, `->all_settled`) and Type::Tiny-style
constraints make recursion a near-term real motivation, not a
speculative one.

## Decision

**Ship `InferredType::Parametric(ParametricType)` where
`ParametricType` is a sealed enum per flavor, each carrying its
own data shape and per-axis policy methods.**

```rust
pub enum InferredType {
    // ... existing variants ...
    Parametric(ParametricType),
}

pub enum ParametricType {
    /// DBIC. Two distinct fields — dual identity is intrinsic.
    ResultSet { base: String, row: String },

    /// Type-level projection. `RowOf<ResultSet { row, .. }>`
    /// reduces to `ClassName(row)`. Lazy — bag-side reducer.
    RowOf(Box<InferredType>),

    // Future: Wrapped { class, inner }, ListOf { class?, element },
    // HashRef { key, value? }, Plugin escape hatch (deferred).
    // See `docs/prompt-parametric-redesign.md`.
}
```

Per-axis methods on `ParametricType` carry per-flavor policy:
- `class_name()` — dispatch class. `ResultSet { base, .. }` returns
  `&base`; `RowOf(inner)` evaluates the operator (returns the inner's
  row if it's ResultSet, else None).
- `hash_key_class()` — class to consult for direct `recv->{key}`
  access. `ResultSet` returns `&row`.
- `method_arg_owner(method)` — `Some(owner)` for hash-key args of
  `recv->method({KEY})` claimed by this flavor; `None` to fall
  through to the receiver-class strict-eq path. `ResultSet` claims
  the row-keyed methods (search/find/create/update/...); returns
  None for count/exists.

The dual-class semantics (`$rs->search` dispatches against
`class_name()` = base; `search({KEY})` looks up keys via
`method_arg_owner` = row) live INSIDE the flavor's impl — not as
consumer-side accessor accessors that special-case on data layout.
**No method-name allowlist anywhere in consumers.** Per CLAUDE.md
invariant #10.

**No `_ => …` fall-through arms** on `ParametricType` matches.
Compiler exhaustiveness is the safety net for the future `Plugin`
escape hatch — when it lands, every consumer lights up
mechanically.

## Why recursive from day one

We considered `Vec<String>` and rejected it.

**Cost of recursive vs flat is essentially zero.** One enum, one
indirection (`Box`) only on the recursive arm, identical serde
behaviour, identical builder emission cost. Display rendering
(`format_inferred_type`) is one extra match arm. ~10 LOC delta.

**Migration cost from flat to recursive would be real.** Every
consumer that wrote `&type_arg` would switch to
`type_args[0].as_class()` (mechanical, ~20 sites today, growing as
parametric residuals land), AND any consumer that started threading
the inner-string structure as a parsing-an-encoded-format kludge
would need rewriting. The flat model invites
encoding-over-strings (`"ResultSet<Users>"` parsed at the consumer
side, `"k,v"` for two-arg parametrics, etc.) — exactly the
footgun-by-encoding problem we wanted to avoid.

**The Promise/async story is on the near-term horizon.** First-
class support for `Mojo::Promise<X>` + `Future::AsyncAwait` `await`
narrowing is queued (own future workstream — async needs more than
just types: async boundaries break globals, syntax warnings change,
etc.). Those PRs will lean on the recursive shape. Doing the data-
model upgrade once is cheaper than twice.

**Type::Tiny / Moose isa is already idiomatic Perl with recursive
type constraints in the wild** — `HashRef[ArrayRef[Str]]`, `ArrayRef
[InstanceOf['Tree']]` in the same Perl files we already analyze.
The LSP under-models them today (records `isa` as a string and
pattern-matches the leaves); a future plugin pass that surfaces
nested constraint structure has nowhere to put the result without
recursion.

The `HashRef[ArrayRef[Str]]` example was the decisive one: it's not
hypothetical, it's not future, it's already in Moo / Moose code
bases that exist now. Locking the data model against that shape
would have been a self-inflicted limitation.

## Trade-offs

**More variants for consumers to handle.** `format_inferred_type`,
`inferred_type_to_tag`, plugin Dynamic conversions, every match on
`InferredType` gets one more arm. The compiler enforces
exhaustiveness so adding the variant lit up every site that needed
updating — which is also the *benefit*: no silent fall-throughs.

**Recursive serde of `Box<InferredType>` is fine.** bincode + zstd
+ JSON all handle it. The cache invalidates on `EXTRACT_VERSION`
bumps (22 → 23 for the initial flat shape, 23 → 24 for the v2
sealed-enum redesign before the branch merged) so old blobs
without the variant don't get deserialized.

**Variant enum vs trait objects.** `Box<dyn Parametric>` (literal
"abstract base class" in OO terms) doesn't roundtrip through
serde without `typetag` or hand-rolled SerDe — both pain we don't
want. The sealed enum gives the same expressivity (per-flavor
dispatch via `match`), full serde derives, and exhaustiveness
checking that catches missing-variant bugs at compile time.

## Why two class-name accessors per flavor

A `Parametric(ResultSet { base, row })` value answers two morally
different questions:

1. **What class do this value's methods come from?** Base
   (`$rs->search`, `$rs->all`, `$rs->count`).
2. **What class do this value's hash-key args belong to?** Row
   (the row class with `add_columns`-synthesized columns).

If we only exposed one accessor, consumers would route through it
for both questions and get the wrong answer for one of them. The
first design pass had a `hash_key_lookup_class` helper on
`FileAnalysis` that hardcoded `target_name in {search, search_rs,
find}` — a per-method allowlist. The v1 redesign made the rule
type-level via a `hash_key_class()` accessor on `InferredType`
that special-cased the Parametric variant. **The v2 redesign
goes further**: each flavor's policy lives in its own impl
(`ResultSet::class_name`, `ResultSet::method_arg_owner`, …); no
data-layout-special-casing on `InferredType` accessors.

When a future flavor — `Wrapped<X>`, `ListOf<X>`, `Plugin<id>` —
wants different narrowing semantics, it implements its own
`class_name` / `hash_key_class` / `method_arg_owner`. No
consumer changes.

## Out of scope (queued)

- **Custom `resultset_class` discovery.** `$schema->resultset
  ('Users')` should resolve `base` to a discovered
  `<Schema_NS>::ResultSet::Users` if that class exists, else fall
  back to `DBIx::Class::ResultSet`. Currently hard-coded to the
  base. Pinned by `goto_def_offers_custom_resultset_method`
  (`#[ignore]`-tagged), tracked in
  `docs/prompt-type-inference-residual.md` Part 5c follow-up.
- **Nested hash-key intelligence (Tiers 1–3).** The recursive
  shape via per-flavor `Box<InferredType>` fields is in place;
  emission + consumer narrowing is its own workstream. Spec:
  `docs/prompt-nested-hashkey.md`.
- **Receiver-relative return types.** `return_type: ReturnExpr`
  admitting `Receiver` placeholders + `UnionOnArgs` branches —
  subsumes per-method projection (`find` declares
  `RowOf(Receiver)` once on the symbol) AND arity dispatch
  (Mojo `has` accessors as `{ args.is_empty() => T, _ =>
  Self }`). Next architectural pillar after this redesign;
  spec'd in `docs/prompt-parametric-redesign.md` Section 2.
- **Promise<X> threading + async boundaries.** Own pillar — async
  semantics (boundary-rekks-globals, syntax warnings, await
  narrowing) are richer than just types.
- **Type::Tiny constraint AST surfacing.** A future Moo/Moose
  plugin enhancement that reads `isa => HashRef[ArrayRef[Str]]`
  and emits the recursive Parametric directly. The data model is
  ready; the emitter isn't.

## Test discipline

Tests in `src/parametric_resultset_tests.rs` are mostly
encoding-agnostic — assert against the public surface
(`find_definition`, `crate::resolve::refs_to`). They survived
the v1 → v2 encoding change without modification.

ONE test (`parametric_resultset_carries_base_and_row`)
deliberately pins the internal shape:
`Parametric(ResultSet { base, row })` at the resultset call's
witness. This is the encoding-direction pin called for during
review — without it, a refactor to a single-class encoding
(`ResultSet(String)` for either dispatch class OR row class)
would silently break `RowOf` at the projection site rather
than tripping a test. External behavior tests can't catch this
class of regression because they only see the projection's
output.

The pin pattern: behavior tests (the bulk) + one shape-pin per
load-bearing variant. Behavior tests survive encoding changes;
shape pins catch encoding regressions.

## Commit history

- v1 (commits c0f5c03 + c3f7863): TDD pins + `Parametric { base,
  type_args: Vec<TypeArg> }` shape with `class_name()` /
  `hash_key_class()` accessor pair on `InferredType`,
  `emit_call_arg_key_accesses_open` sibling, search-family
  allowlist in consumers. Captured the recursion + dual-class
  insight but encoded it as flat-shape reflexes.
- v2 (this commit): sealed-enum `ParametricType` redesign per
  `docs/prompt-parametric-redesign.md`. Per-flavor methods,
  `RowOf` operator, `method_arg_owner` driving emission gating,
  no `_ => …` fall-throughs. Internal-shape pin added for
  `ResultSet { base, row }`. EXTRACT_VERSION 23 → 24.
- Red-pins queued: `goto_def_offers_custom_resultset_method`
  (custom resultset_class discovery) and
  `method_dispatch_through_find_resolves_to_row_class` (per-
  method return-type projection — `find` → ClassName(row)) —
  both queued with the DBIC-as-plugin port and ReturnExpr
  pillar respectively.
