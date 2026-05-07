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

**Ship `InferredType::Parametric { base: String, type_args:
Vec<TypeArg> }` with `TypeArg` recursive from day one.**

```rust
pub enum InferredType {
    // ... existing variants ...
    Parametric { base: String, type_args: Vec<TypeArg> },
}

pub enum TypeArg {
    Class(String),                 // 95% case: ResultSet<Users>
    Type(Box<InferredType>),       // recursive: HashRef<ArrayRef<Str>>
}
```

`Class(String)` is the flat-string variant for the DBIC happy path
— no Box overhead, the most common case stays cheap. `Type(Box<…>)`
is the recursive door, taken whenever a type arg is itself
parametric or any non-class shape (HashRef, ArrayRef, …).

Accessors:
- `TypeArg::as_class() -> Option<&str>` for the 95% case.
- `InferredType::class_name()` returns `Some(&base)` for Parametric
  — method dispatch (`$rs->search`) resolves against the base.
- `InferredType::hash_key_class()` returns
  `type_args[0].as_class()` for Parametric, `class_name()` else —
  hash-key arg lookup resolves against the row class.
- `InferredType::parametric_type_args()` exposes the full slice for
  consumers that need recursive walking (planned Tier 2/3
  nested-hashkey work, future Promise reducer).

The two-accessor split (`class_name` vs `hash_key_class`) is the
load-bearing piece — it carries the dual-class semantics generically
so consumers don't special-case method targets.

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
bump (22 → 23) so old blobs without the variant don't get
deserialized.

**The `Class(String)` happy-path arm is a soft denormalization.**
`Type(Box::new(InferredType::ClassName(c)))` is logically
equivalent. Keeping `Class(c)` as a peer eliminates a Box on the
common case (DBIC) and reads cleaner at match sites. `as_class()`
papers over the difference for consumers.

## Why two class-name accessors instead of one

A `Parametric { base: ResultSet, type_args: [Users] }` value answers
two morally different questions:

1. **What class do this value's methods come from?** ResultSet
   (`->search`, `->all`, `->count`).
2. **What class do this value's hash-key args belong to?** Users
   (the row class with `add_columns`-synthesized columns).

If we only exposed one accessor, consumers would route through it
for both questions and get the wrong answer for one of them. The
first design pass had a `hash_key_lookup_class` helper on
`FileAnalysis` that hardcoded `target_name in {search, search_rs,
find}` — a per-method allowlist. That smelled wrong: the rule
isn't about which method is being called, it's about which dimension
of the type the consumer is reading. Pushing the rule onto
`InferredType` (each accessor returns its dimension) generalizes
without method allowlists.

When a future parametric type — `Promise<X>`, `Mojo::Collection<X>`
— wants different class-narrowing semantics, it overrides
`hash_key_class` (or whichever accessor) on its own terms. No
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
  `TypeArg` is in place; emission + consumer narrowing is its own
  workstream. Spec: `docs/prompt-nested-hashkey.md`.
- **Promise<X> threading + async boundaries.** Own pillar — async
  semantics (boundary-rekks-globals, syntax warnings, await
  narrowing) are richer than just types.
- **Type::Tiny constraint AST surfacing.** A future Moo/Moose
  plugin enhancement that reads `isa => HashRef[ArrayRef[Str]]`
  and emits the recursive Parametric directly. The data model is
  ready; the emitter isn't.

## Test discipline

Tests in `src/parametric_resultset_tests.rs` assert against the
public surface only — `find_definition`, `crate::resolve::refs_to`.
None pattern-match on `InferredType::Parametric` or `TypeArg`
internals. The encoding could change (e.g. `Vec<TypeArg>` →
`Vec<NamedTypeArg>` with positional names) without rewriting any
test. The one assertion that *would* depend on encoding —
plugin-emitted Parametric witnesses — was deferred from the
test-first commit until after the design settled.

## Commit history

This ADR was written alongside Phase 1's implementation:

- TDD-first commit: 8 external-behavior pins for column lookup.
- Implementation: `InferredType::Parametric` + `TypeArg`,
  `extract_resultset_parametric` emitter, `hash_key_class()`
  accessor, `emit_call_arg_key_accesses_open` for cross-file
  Parametric receivers (skips the `has_hash_key_def` strict gate
  the type itself replaces).
- Pinned `goto_def_offers_custom_resultset_method` as the next
  red-pin (custom resultset_class discovery — known gap, queued
  follow-up).
