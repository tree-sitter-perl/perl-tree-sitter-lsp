# Type-is-the-gate generalization

**Status:** discussion. Two instances landed on the parametric
ResultSet branch (`_open` emitter for Parametric-claimed
receivers; cross-file deferred owner fix for chain receivers
unresolvable at build). The general refactor — replacing every
strict-eq local-symbol gate with a "the type knows" answer —
remains open.

## Origin

Phase 1 of Part 5c ran into a soundness gate:
`emit_call_arg_key_accesses` checks `has_hash_key_def(&key, owner)`
strict-eq against the local symbol table before emitting a
`HashKeyAccess` ref. The gate prevents bogus emissions like
`Foo::bar(name=>1)` latching onto Foo's `Sub{Foo,new}` keys when
`name` isn't a `bar` arg.

For Parametric receivers (DBIC `$rs->search({KEY=>...})`), the
producer's `HashKeyDef` lives in another file — not visible at
consumer build time. Strict-eq blocks the emission, the
HashKeyAccess never gets pushed, `refs_to(HashKeyOfClass(Users))`
silently misses cross-file call sites.

The `_open` emitter sibling was the first instance: when the
receiver types as Parametric, the *type* is the gate (the
flavor's `method_arg_owner` returns Some only when it claims).
Skip the strict local-symbol check; emit unconditionally with
the flavor-pinned owner.

The cross-file deferred owner fix was the second instance:
chain receivers whose type only resolves cross-file get
`HashKeyAccess { owner: None }` emitted at build, with
post-walk + post-enrichment fixup filling the owner once the
receiver's flavor is resolvable. Same pattern: build emits
what it can, the type's eventual answer fills the gap.

## The pattern

Same shape recurs throughout the codebase:

| Symbol-table gate | What the type already says |
| --- | --- |
| `has_hash_key_def(name, owner)` | "this Parametric's args are row-keyed" |
| `resolve_method_in_ancestors(class, method)` | "this Parametric's element type has methods" |
| `find_param_field(class, key)` | "this `:param`-bearing class has these fields" |
| `symbols_named(name)` for cross-file resolution | "this import's source module has this sym" |

Each gate guards "don't emit / don't claim if local evidence doesn't
support it." The gate is sound when local evidence is the only
truth available. With the witness graph + cross-file enrichment,
the type carries authoritative answers; the local symbol table is
a *cache* of one perspective, not the source of truth.

## What "type is the gate" means architecturally

The witness graph already encodes "what we know about this value."
A consumer that asks "should I emit / claim X?" has two questions
to combine:

1. **Is the operation valid for this value?** Type axis. The type
   answers via accessors / traits.
2. **Is the local symbol table consistent with the answer?** Cache
   axis. Strict-eq gates ask this.

When (1) and (2) disagree, today (1) wins iff a sibling helper
exists (the open-gate variant). The general pattern is: **trust
(1) when the type carries a definite answer; consult (2) only
when (1) is silent.**

## Generalization sketch

A generic emit/claim helper:

```rust
fn emit_if_typed_or_locally_known<F: Fn() -> Option<HashKeyOwner>>(
    type_says: F,        // the Parametric's hash_key_class etc.
    fall_back_to_local: bool,
    name: &str,
    owner: &HashKeyOwner,
) -> bool {
    if let Some(owner) = type_says() { return true; }      // type wins
    if fall_back_to_local && self.has_hash_key_def(name, owner) {
        return true;                                         // local known
    }
    false
}
```

Replaces every strict-eq gate that has a Parametric-aware open path.

More ambitious: strip the strict-eq gates entirely, replacing them
with type-side answers everywhere. Cross-file enrichment fills the
type's witness with cross-file evidence; local emission becomes
"the type tells me what's true here."

## Cost estimate

Inventory of strict-eq gates that would migrate:
- `has_hash_key_def` — already handled by Part 5c's `_open`
  sibling.
- `find_param_field` — Perl 5.38 :param resolution. Cross-file
  `Class :param` declarations would similarly miss without a
  Parametric-aware path. Lower frequency than DBIC, hasn't bitten
  yet.
- `resolve_method_in_ancestors` — already cross-file aware via
  `module_index`. Not strict-eq exactly; uses `package_parents` to
  walk MRO. Different pattern.
- `symbols_named` for cross-file — generally cross-file aware via
  `module_index.find_exporters`. Specific to the imported-sub flow.

Realistic migration: probably 3-5 sites per direction (gate-strip,
or generic helper introduction), maybe 50-80 LOC of refactor + new
trait/helper API. Smaller than Phase 1 of Part 5c; mostly
mechanical.

## When to do it

**Don't do it speculatively.** The first migration was Part 5c's
`_open` variant — necessary because cross-file DBIC was the
motivating test. The next site that wants the same generalization
will be one of:

- Plugin-emitted parametrics with cross-file row-class definitions.
- Effect facts (Promise<X>'s typed body crossing async boundaries).
- Sequence types phase 4-5 (cross-file mutation effects).

When the second site shows up, generalize. The shape will be clearer
with two real cases than one + speculation. Until then, Part 5c's
`_open` is a pin; CLAUDE.md invariant #10 says future "I want
behavior X for this shape" goes on the type. The next implementer
hits the right rule by default.

## Risk: false positives from over-trust

If the type axis is wrong (Parametric witness is bogus / mis-
emitted), removing the strict gate means the bogus emission lands
in the bag instead of being filtered out. Today's gate is a
*correctness backstop* against bad witness emission upstream. Type-
is-the-gate trades that backstop for the cross-file completeness
win.

Mitigation: tests that exercise the negative cases
(`unrelated_method_does_not_emit_parametric`,
`goto_def_for_unknown_column_returns_nothing`). Both already exist
for Part 5c. Same discipline for future migrations.

## Recommendation

Defer the general refactor until a second site arrives. Document the
pattern (this file). When it lands, the second site builds the
generic helper; the first migrates to it; CLAUDE.md invariant #10
gets a concrete cross-reference.
