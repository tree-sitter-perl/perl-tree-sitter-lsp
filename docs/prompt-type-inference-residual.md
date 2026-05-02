# Type Inference: Residual Forward Work

> Witness bag + reducer registry + framework-aware fold + branch-arm fold +
> fluent arity dispatch + plugin-override priority — all landed.
> CLAUDE.md "Type inference (witness bag)" / "Worklist invariants" describe
> the live shape. This doc is the still-pending fact classes. Each lands
> as a **new reducer + emitter pair** without expanding `InferredType`.
>
> Cross-refs: `prompt-type-inference-unification.md` (the staircase to
> collapse walk-time + bag-time into one path); `prompt-sequence-types.md`
> (positional containers, lands after the unification's Step 3).

## Part 1 — Invocant mutations

`$self->{count}++` / `$self->{items} = [...]` change the *contents* of
`$self`, not just read them. Today: emitted as `HashKeyAccess` refs with
`AccessKind::Write`, but type beliefs about the slot's shape don't update.

**Status:** `mutated_keys_on_class(class)` exists and reads `Fact { family:
"mutation", … }` witnesses on `HashKey` attachments. Not wired into
diagnostics or completion yet.

**Forward work:**

- **Dynamic key completion.** `$self->{|` inside a class completes with
  the union of `has` declarations + every mutated key observed across the
  class's methods.
- **Read-only violation diagnostics.** Writing through a Moo `is => 'ro'`
  accessor is a hint.
- **Narrower `$self->{k}` types.** If every mutation of `$self->{queue}`
  has `rhs_type = ArrayRef`, the read site assumes ArrayRef.

Class-scoped union is the right granularity (method-scoped is rarely
useful). `delete $self->{k}` as a "drop" signal — ignore for now.

## Part 2 — Hash key unions

Today each `HashKeyDef` has exactly one owner. Real code composes:

```perl
my $full = { %$defaults, %$overrides };
$full->{host};   # key from either source

sub make_result {
    return { %$base, extra => 1 };   # merge with literal
}
```

We emit `InferredType::HashRef` for `$full` but don't know where its keys
come from.

**Forward work:** Add `HashKeyOwner::Union(Vec<HashKeyOwner>)` (or a
union-witness pattern). Builder walks `%$var` splices in
`visit_anon_hash` and records origins. Linker expands Union defs into one
entry per member at index time so `(target_name, owner)` lookups work.

Cap recursion on cycles (`$a = { %$b }; $b = { %$a }`). Cross-file
splices defer to enrichment.

## Part 3 — Method loops (dynamic dispatch over lists)

```perl
for my $meth (@various_methods) {
    $invocant->$meth(9001);
}
```

Constant folding gives up when `$meth` is a loop variable. But the list
constrains the invocant: it must support every method in the list.

**Forward work:** New `DynamicDispatchLoop` record collected during the
walk:

- `for my $m (LIST)` + `$inv->$m(...)` body → one record.
- LIST resolved via `resolve_constant_strings` (already handles `qw`,
  `@const_array`, fat-comma lists).
- Method-existence diagnostics for each name in the list.
- Invocant typing: a name only on class `Foo` narrows `$invocant`.

Loops over computed lists (`@{ $obj->methods }`) → mark `Unknown`.

## Part 4 — Functional operator invariants

```perl
my @titles = map $_->{title}, @articles;
```

Three facts in one expression: `@articles` contains hashrefs; each
element has a `title` key; `@titles` is whatever `title` resolves to.

**Forward work:** New pass — list-element inference from `map` / `grep`
/ `first` / `any` / `all` / `reduce`:

1. Walk the block, collect facts about `$_` (`->{k}` access, `->method`
   call, `@$_` deref).
2. Translate to constraints on the LIST (`HashRef + key 'k'` per
   element).
3. Emit synthetic `TypeConstraint` + `HashKeyDef` (owner
   `ListElement { array_name }` or fold into existing owner).

Block-return inference for `map { ... }` types `@titles` from the
last-expression type.

`reduce` is cleanest with `$a` and `$b` tracked separately — `$a` is the
accumulator, `$b` the element.

Nested `map { map { ... } @$_ } @outer` — facts on inner `$_` apply to
inner scope; outer `$_` gets the ArrayRef fact.

## Part 5a — Value-indexed returns (Pi types)

```perl
sub get_config {
    my ($key) = @_;
    return { host => 'localhost', port => 5432 }->{$key};
}
get_config('host');   # → String
get_config('port');   # → Numeric
```

Return type is a function of the literal first-arg.

**Forward work:** `keyed_returns: Option<HashMap<String, InferredType>>`
on `SymbolDetail::Sub`. Set during `visit_sub` when the body is
`{ ... }->{$param}` (or `my %t = (...); return $t{$param}`).

`infer_expression_type` on a call with a literal first-arg looks
`keyed_returns[lit]` first; falls back to `return_type` otherwise. Wires
into existing `CallBinding` fixup; no new structures.

## Part 5b — Sum-type narrowing (the narrowing half)

```perl
my $r = fetch();
if (ref $r eq 'HASH') {
    $r->{ok};              # narrowed to HashRef
} elsif (defined $r) {
    # narrowed to non-undef
}
```

**Status:** The agreement-across-branches half (`BranchArmFold`) landed.
Scope-narrowed-by-condition beliefs aren't wired.

**Forward work:** Walk `if`/`unless`/`elsif` conditions. Patterns:

| Condition | Effect in then-body |
|---|---|
| `if ($v)` / `if (defined $v)` | narrow away undef |
| `if (ref $v eq 'ARRAY')` | narrow to `ArrayRef` |
| `if (blessed $v)` | narrow away non-objects |
| `if ($v->{tag} eq 'literal')` | narrow Union to matching tag-shape |

Each emits a `Witness { attachment: Variable, payload:
InferredType(narrowed), span: if_body_span }`. The bag-fold prefers the
narrowest-span witness containing the query point. No new
`narrowing_scope` field — the span IS the narrowing.

## Part 5c — Parametric types (DBIC, indexed string collections)

```perl
my $rs = $schema->resultset('Users');     # ResultSet<Users>
$rs->search({ KEY => ... });              # KEY narrowed to Users columns
```

**Forward work:**

```rust
InferredType::Parametric { base: String, type_arg: String }
```

Emission rules:

- `$schema->resultset('Users')` (literal first arg) → `Parametric {
  base: "ResultSet", type_arg: canonicalized_class }`.
- `->search(...)` / `->search_rs(...)` / `->find(...)` return the same
  Parametric — the parameter threads through.

Consumer: `cursor_context.rs` recognizes hash-key position inside a
search-family call on a `Parametric { base: "ResultSet", type_arg }`
receiver, looks up `HashKeyDef`s + synthesized column accessors in
`Package(type_arg)`. Diagnostic for unknown columns.

Relationship prefetch (`->search({...}, { join => 'posts' })`) extends
the valid key set with `posts.col` from the joined Result class. Phase 2.

## Part 7 — Plugin-registered reducers from Rhai

The `WitnessReducer` trait exists in Rust; the Rhai surface doesn't.
Today plugins ship type assertions via `overrides()` — the "I know
better" case. Full reducer registration unlocks the route-aggregation
use case (App A in the original spec): one chain reducer that collapses
`$r->get('/users')->to('Users#list')->name('users_list')` into a
single `<route>` outline entry.

```rhai
fn reducers() {
    [#{
        claims: #{ attachment_kind: "Expression", source_plugin: "mojo-routes",
                   payload_family: "route" },
        reduce: "route_reduce",
        on_complete: "route_emit_if_complete",
        sufficient: "route_is_complete",
    }]
}
```

Lets plugins choose fold semantics (LWW, append, max, custom merge) and
emission timing (threshold-based, end-of-statement, end-of-file).

## Out of scope

- **Univalence / first-class identity types.** Renaming = transport
  along equality; we do it procedurally via `refs_by_target` and that's
  the right level.
- **Higher inductive types.** Perl has no user-visible quotient
  structures.
- **Full dependent inference.** Undecidable. The patterns above are the
  sweet spot — value-indexed lookups, sum-type narrowing, parametric
  collections — with bounded analysis cost.
- **Effect facts** (mutation/throw/IO). Out of scope for the type/
  structure/derivation model.
- **`wantarray` / context-dispatched returns.** Mechanical to add (new
  reducer with a context dimension on top of arity); not currently
  prioritized.
- **Aliasing through scalars across function boundaries.** Effect
  signatures cover common cases; full aliasing is out of scope
  indefinitely.
