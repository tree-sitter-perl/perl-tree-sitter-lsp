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

## Remaining gaps

1. **Cursor ON a reader call** (`$p->x` → rename): resolves as
   `RenameKind::Method` and takes the cross-file `refs_to` path, which
   renames reader calls + the decl token (text-correct now) but misses
   ctor keys and field body uses. Fix direction: `resolve_symbol` (or
   `rename_kind_at`) should recognize the reader projection and answer
   with the field group; needs a decision on how a group rides
   `TargetRef` for the cross-file walk.
2. **Cross-file ctor keys**: `Point->new(x => …)` in *another* file.
   The group rename is the single-file primitive; cross-file fan-out
   for field groups isn't wired (`TargetKind::HashKeyOfSub` walks exist
   for references — rename policy currently says hash keys are
   in-file-only, which is right for plain keys but wrong for
   `:param`-derived ones).
3. **Moo `has` has the same holes** (probed): decl rename misses the
   ctor key; ctor-key rename renames only itself; internal
   `$self->{name}` accesses aren't tied. The group concept extends —
   Moo's group = accessor Method + ctor HashKeyDef + Class-owned hash
   keys, no sigil variable — but its members differ enough that
   `field_group_at` shouldn't be force-fit; add a Moo group constructor
   feeding the same `rename_field_group`-style union.
4. **References aren't unioned**: find-references on the ctor key lists
   key accesses only, not field uses (and vice versa). Same group, read
   side.
