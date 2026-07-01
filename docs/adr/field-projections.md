# ADR: Field projections — one decl, every spelling, one rename group

> **Shared identity with C:** `AttrProjection { class, attr }` IS the Perl
> realization of the language-generic `WitnessAttachment::Field { owner, name }`
> bag primitive (the C struct-member domain subject) — same `(owner, name)`
> shape. `FileAnalysis::field_subject(class, name)` mints that canonical subject
> for any Perl field (owner = declaring class via the ancestor walk);
> `field_subject_of_ref` routes any access shape (accessor call / `$self->{k}` /
> Corinna field var) onto it without seeing the flavor. The refs-splat below and
> the C domain fold are two consumers of the ONE subject. Perl *domain* typing on
> that subject is deferred — see `docs/cpp-golive-map.md` item 3.

A framework field declaration is ONE name spelled several ways. Moo:
`has size` ↔ accessor `$w->size` ↔ ctor key `Widget->new(size => …)` ↔
internal `$self->{size}`. Corinna: `field $x :param :reader` ↔ ctor key
↔ reader call ↔ body `$x` uses. Rename and references must treat the
constellation as one entity — and must NOT admit lookalikes (another
sub's `name =>` argument is not the attr).

## The entity: `AttrProjection`, minted at synthesis

`AttrProjection { class, attr, kind: CtorKey | InternalKey | Accessor }`
rows on `FileAnalysis`, emitted by the synthesis that already knows the
semantics — Moo/Moose/Mojo::Base `has` mints CtorKey + Accessor +
InternalKey; Corinna fields mint CtorKey/Accessor only (fields are not
hash entries, so `->{name}` on a Corinna class is unrelated — or a
bug). The repr gate IS whether InternalKey was minted: no query-time
"is this class hash-backed" side condition exists, because the minting
site is the one place that knows. Plugins enroll name-mapped
projections (`predicate => 1` → `has_size`) through the same entity.

## Membership is strict, never `found_by`

Internal-key membership matches `HashKeyOwner::Class(class)` by strict
equality. The rejected broadening — collecting via `found_by` — would
admit *other subs'* same-named arg keys (`$obj->search(name => …)`)
into the attr's rename. This is the load-bearing line: the group grows
only through projections minted by synthesis or owners resolved to the
exact class.

## Query side: `ResolvedTarget::Group` with per-member rename

`resolve_symbol` from ANY spelling returns the whole group:
`local_spans` (the class file's own spellings), `pinned_spans` (class
file spans when the cursor sat in a consumer — the group is minted from
the CLASS's analysis, fetched via `CrossFileLookup`, so consumer-side
cursors see the full constellation), and `members`, each a walkable
`TargetRef` carrying its own `MemberRename`:

- `Bare` — the spelling takes the new name verbatim;
- `Affixed { prefix, suffix }` — derived names re-derive (`has_size` →
  `has_extent`), including user-written `sub _build_size` defs;
- `Skip` — members whose names don't embed the attr join references
  but honestly decline rename.

Rename and references read the SAME group, so they cannot disagree —
the same single-sourcing rule as `resolve_symbol`/`refs_to`
(`adr/file-store-and-resolve.md`).

## Consumer-file keys without the class

Files that only `use` the class still index ctor keys:
`Gate::StrictOrDefer` emits `owner: None` candidates at build, and
query-time re-derives the owner (`deferred_hash_key_owner`) — the
receiver-gated discipline applied to hash keys. This is the one
deliberately lazy resolution left in the ref model: dependency files
are never enriched, so candidates ride the cache and resolve when
asked.
