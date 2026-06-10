# Field projections: the rename tie, what's done and what's left

A framework field decl is ONE name spelled several ways. Corinna:
`field $x :param :reader` ↔ ctor key `Point->new(x => …)` ↔ reader
`$p->x` ↔ body uses of `$x`. Moo: `has name` ↔ accessor `$obj->name` ↔
ctor key ↔ internal `$self->{name}`.

## Done (June 2026)

- `FileAnalysis::field_group_at` / `rename_field_group`: in-file rename
  from ANY Corinna spelling (field decl, body use, ctor key at a call
  site) rewrites the whole group, sigil-preserved. Wired as the first
  branch of `rename_at`, so LSP + CLI single-file renames both get it.
- Synthesized projections (`:param` ctor HashKeyDef, `:reader`/`:writer`
  methods) select the **bare-name sub-span** of the `$x` token — a bare
  replacement can no longer eat the sigil.
- `:reader` call sites are included in the group's edit set.

## Landed (the union, June 2026 overnight session)

1–4 of the original gaps are DONE: `ResolvedTarget::Group` rides
`resolve_symbol` (reader-call cursors included), `resolve::group_refs`
walks the group cross-file for both references and rename (backend +
both CLI mirrors), in-file `find_references` unions (highlights /
linked-editing inherit), and Moo `has` attrs join via the pair
signature (accessor Method + ctor HashKeyDef sharing name, package,
and selection span). Consumer files' ctor keys exist now too:
`Gate::StrictOrDefer` emits `owner: None` candidates when the class
isn't local, and `refs_to`'s key arm / goto-def re-derive the owner at
query time (`FileAnalysis::deferred_hash_key_owner`) — the
receiver-gated discipline applied to hash keys.

## Remaining gaps

1. **Internal `$self->{name}` keys aren't in the group** —
   CONTENTIOUS, deliberately deferred: collecting them via
   `HashKeyOwner::found_by` broadening would also admit *other subs'*
   same-named arg keys (`$obj->search(name => …)`) into the attr's
   rename. Needs a strict-Class-owner membership design first.
2. **Consumer-side cursor, class elsewhere**: rename/references from a
   ctor key or accessor call in a file that only `use`s the class falls
   back to narrower behavior (the group is detected against the local
   analysis only). Fix direction: in `resolve_symbol`, on a deferred
   key / cross-file accessor, fetch the class's cached analysis via
   `CrossFileLookup` and mint the group from THERE (its variable spans
   become `RefLocation`s in the class file).
3. **`:writer` (`set_x`) and Moo `predicate`/`clearer`/custom-named
   accessors** aren't group members — their names differ from the attr;
   tying them means a name-mapping on the group (plugin-owned for Moo
   per the accessor-vocabulary split).
