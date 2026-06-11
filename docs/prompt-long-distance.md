# Long-distance intelligence — epic record

The epic's shape: information a position needs lives in files that
REFERENCE this one; resolution flows use → definition. ALL FOUR
members + the shared primitive are landed (PRs #56 +
`long-distance-remainder`):

1. **children_index** + goto-implementation + composer-mismatch —
   `adr/role-contracts.md`.
2. **Entrypoint-scan helper lint** (`helper-not-loaded`) — loaded
   facts from imports + the `PluginLoad`/SyntheticUse both plugin
   arms emit; exact-or-tail matching (loose only toward silence);
   workspace providers only. House kits with custom loaders
   (Clove::Test) declare ONE SyntheticUse/PluginLoad arm in their own
   `.perl-lsp` plugin — the BYO principle.
3. **Loader-config param typing** (the framework-mediated case of
   #25): `plugin 'X', {...}` → `register`'s `$conf` types as the
   gathered config shape. Manifest declares the contract
   (`param_types` + `from_loader_config`), callers record the value
   (`PluginLoad{name, config_span}`), enrichment joins with
   agreement: all equal → the shape; keyed disagreement → OPEN key
   union; otherwise decline. Static `type_class` is the no-caller
   fallback; structure-dominates-rep prefers the gathered shape.
4. **A4 v2** (joined the epic mid-flight): `$self->{k}` reads narrow
   through slot writes in OTHER files and up the ancestry — Projected
   falls back to `SlotType{class,key}`, which gained the
   MethodOnClass-style cross-file + parents recursion.

## What stays open (the epic's honest boundary)

**Open-world caller gather** — typing params of an ordinary sub from
arbitrary call sites. The honesty gate stands: ANY un-enumerable
caller (dynamic dispatch, exported surface) poisons the claim. The
loader-config machinery is the template — what's missing is a
WITNESS of enumerability for plain subs, which the graph rework's
unresolved bucket (`prompt-graph-walking.md`) could provide. Revisit
then.
