# Cross-file invocant_class refresh

**Status:** known regression. Pinned by
`references_cross_file_invocant_resolved_post_enrichment` in
`resolve_tests.rs` (`#[ignore]` until this lands).

## The bug

`Ref::MethodCall { invocant_class, .. }` is **set once at build time**
and never refreshed. When a consumer file's `$b` invocant only becomes
typeable post-enrichment (because the producer's return type lives in
another file), the consumer's `$b->method()` MethodCall ref keeps
`invocant_class: None` forever — the bag learns the type but the ref
field doesn't get re-derived.

Repro:

```perl
# Producer (B.pm)
package B;
use Exporter 'import';
our @EXPORT_OK = qw(make_b);
sub new    { return bless {}, shift }
sub make_b { return B->new }
sub touch  { 1 }
1;
```

```perl
# Consumer (A.pm)
use B qw(make_b);
my $b = make_b();
$b->touch();   # invocant_class on this ref stays None forever
```

`enrich_imported_types_with_keys` on the consumer pushes a Variable
witness for `$b: ClassName('B')` (so `inferred_type_via_bag` answers
correctly) but does NOT re-fill the MethodCall ref's `invocant_class`.
`refs_to`'s class-scope filter (`resolve.rs:256-258`):

```rust
match (invocant_class, scope) {
    (Some(cn), Some(pkg)) => cn == pkg,
    _ => false, // package-less sub or unpinned method
}
```

excludes unresolved invocants, so the cross-file call site silently
doesn't show up in references for `B::touch`. Same gap hits
`find_highlights`'s cross-file fallback (`file_analysis.rs:2746`) and
the same-class match in `find_references`'s
`collect_refs_for_target`.

## Why now

This bug is older than D4 — the audit just surfaced it. `invocant_class`
has been a build-time-only field since chain typing landed. It works
fine when the invocant's class is locally inferrable (`Foo->new`,
`$self`, in-file constructor) because then the field is filled at the
end of the worklist. It silently fails when the type only crystallizes
during cross-file enrichment, which is a separate phase that doesn't
touch refs.

## The asymmetry that makes this hurt

Type-query callers (hover, signature_help, completion, inlay_hints) ask
the bag at query time — they always see post-enrichment state. Refs are
the one place where type info is **cached at build time** for
performance: `refs_to` over a workspace would otherwise need to bag-query
every method call's invocant just to answer "is this a call to my
target class's method?". The cache assumed types were monotonic and
final by the end of build. They aren't, when enrichment is in the
picture.

## Fix surfaces

Three options, ordered by surface area:

1. **Re-fill `invocant_class` post-enrichment.** Call
   `apply_chain_typing_invocants` (or an FA-side equivalent) from
   inside `enrich_imported_types_with_keys` after the new TCs are
   pushed. Cheapest fix; couples builder logic to FA's enrichment
   path. The FA-side mirror would need access to the same chain-typing
   index the builder uses — not currently exposed.

2. **Query-time materialization.** Drop `invocant_class` from the ref
   schema entirely; derive it on demand inside `refs_to` /
   `collect_refs_for_target` from a live bag query against the ref's
   invocant_span. Cleanest architecturally — kills the parallel cache
   — but `refs_to` walks every ref in every file in scope, so a per-ref
   bag query could be a real perf cost on big workspaces. Worth
   measuring before adopting.

3. **Hybrid: cache + bag fallback.** Keep `invocant_class` as a
   build-time cache; when it's `None`, have the reader (refs_to,
   find_references, find_highlights) consult the bag for the
   invocant's type. Smallest behavior change — the cache hit path
   stays fast, only unresolved cases pay the bag-query cost. Closest
   to "monotonic refinement": adding cross-file types narrows results,
   never widens them incorrectly.

(3) is the recommended path. (1) is acceptable as a stopgap if the
reader-side change feels too invasive; (2) is the right architectural
end state but should wait until perf is measured.

## Audit before implementing

Before fixing, grep for every read of `invocant_class` and
`resolved_package` (the FunctionCall analog — though that one is
name-resolution, not type, so it shouldn't have the same gap). Confirm
each one would tolerate a `None → Some` transition mid-session. The
known readers are in `resolve.rs::refs_to`, `file_analysis.rs`'s
`collect_refs_for_target` / `find_highlights` / various `class_name =
invocant_class.clone().or_else(...)` callsites for hover and
completion. The hover/completion sites already use the
`.or_else(resolve_method_invocant)` text fallback — they're
self-healing, but the text fallback is a parallel path, which is its
own smell worth folding into option 3.

## Test matrix when the fix lands

Drop `#[ignore]` on the pinned test. Add coverage for:

- Consumer build runs *before* producer is loaded (the original
  scenario).
- Producer is loaded but its types haven't propagated yet (timing
  window between resolver thread completion and enrichment refresh).
- Producer's exported sub returns through an inheritance chain
  (multiple cross-file hops).
- `find_highlights` cross-file fallback path
  (`file_analysis.rs:2746-2757`).
- Hover/completion already work via the text fallback — assert they
  keep working *and* that the bag-routed path agrees with the text
  fallback's answer (parallel-path elimination).
