# DBIC support as a plugin

**Status: open — and UNGATED (June 2026).** This was queued behind
type-system-encoding "so the plugin owns its semantics from day one";
that gate is stale. Since it was set, the plugin system grew the
muscles the migration needs: decision-ready `CallContext` shapes (the
`extract_has_options` pattern), the manifest families (`overrides`,
`param_types`, `dispatch_verbs`, `role_makers`), `return_via_edge`,
and the moo.rhai seam as the executed playbook for moving native
synthesis out. Only phase 3 below still touches the axis question.

## Phase ladder

1. **Accessor/relationship synthesis → `frameworks/dbic.rhai`.** ✅
   LANDED. The `visit_dbic_*` family is gone; `frameworks/dbic.rhai`
   (trigger `ClassIsa("DBIx::Class")`) synthesizes column accessors +
   HashKeyDefs and relationship accessors (typed return: ResultSet for
   has_many/many_to_many, row class for belongs_to/has_one/might_have).
   The decision-ready context is the generic `CallContext.arg_names`
   (the call's args as a flat `(name, span)` string list via the shared
   `cst::string_list`), populated only for verbs a plugin registers via
   the `arg_name_verbs()` manifest — core hardcodes no DSL verb. moo was
   moved onto the same registration gate. `load_components` parent
   registration stays core (generic parent machinery). Custom-resultset
   discovery + per-column `data_type` typing are deferred to phase 3.
2. **Meta-method suppression → manifest.** The DBIC entries in
   `symbols.rs`' `universal_methods` (comment-flagged debt) become a
   plugin manifest field; core's diagnostic consults the registry.
3. **Parametric emission + per-method projection** (the tables below)
   — the one genuinely axis-shaped piece. A `parametric_returns`
   manifest field may sidestep full type-system-encoding; decide at
   the boundary, not before.

End state: core is plugin-free except generic dispatch.

## Why move DBIC out

Today the builder has DBIC-specific code paths inline:
- `visit_dbic_add_columns` (column accessor + HashKeyDef synthesis)
- `visit_dbic_relationship` (`has_many` / `belongs_to` / `has_one`
  / `might_have` / `many_to_many` accessors)
- `is_dbic_class` gating (parent-walk against `DBIx::Class::*`)
- `visit_dbic_class_method` dispatching on method names
- `__PACKAGE__->load_components` parent registration (general but
  motivated by DBIC)
- Part 5c additions: `extract_resultset_parametric`,
  `emit_call_arg_key_accesses_open`,
  `parametric_emitted_refs` set
- Built-in DBIC class names hardcoded as base for Parametric

This is a non-trivial DSL surface in core that doesn't conceptually
belong there. CLAUDE.md invariant #10: never special-case for a
particular shape. DBIC is a particular shape — the most common
ORM in Perl, but still one library among many. Other Perl ORMs
(DBIx::Class::Schema::Loader, Rose::DB::Object, Class::DBI) want
similar treatment without inheriting DBIC's specifics.

The plugin system already supports:
- `EmitAction::Method` with typed return — the row-class accessor
  emission story.
- `EmitAction::HashKeyDef` — column-key synthesis.
- `EmitAction::PackageParent` — `load_components` style parent
  registration.
- `EmitAction::PluginNamespace` with bridges — the cross-file
  reachability story for plugin-synthesized entities.
- `EmitAction::Symbol` with explicit return type — for the
  `resultset` return baking.
- `Trigger::ClassIsa("DBIx::Class")` — fires for any class with
  DBIx::Class in its parent chain.

What it *doesn't* support yet:
- Emitting `InferredType::Parametric` witnesses tied to a specific
  call site. Plugins emit witnesses indirectly (through Method's
  `return_type`, Symbol's `return_type`, VarType, TypeOverride) but
  not "publish a Parametric InferredType on this call's
  Expression(refidx)."
- Per-flavor parametric semantics (`hash_key_class`, dispatch class
  override, etc.) — see `adr/return-expr.md` for the receiver-
  relative machinery the plugin would feed into. Without pluggable
  semantics, a DBIC plugin can emit Parametric but the core's
  hardcoded `hash_key_class()` behavior owns the read side.

## What the plugin would own

### Emission

For DBIC — methods + DSL calls — the plugin replaces today's
`visit_dbic_*` family:

```rhai
fn on_method_call(ctx) {
    if ctx.target_name == "add_columns" && ctx.invocant_is_package() {
        for col in ctx.string_or_fat_comma_keys() {
            emit_method(col.name, ...);
            emit_hash_key_def(col.name, owner: Class(ctx.package));
        }
    }
    if ctx.target_name == "resultset" {
        if let Some(row_class) = ctx.first_string_literal_arg() {
            emit_parametric(
                ctx.call_ref_idx,
                base: discover_resultset_class(row_class)
                    .unwrap_or("DBIx::Class::ResultSet"),
                type_args: [TypeArg::Class(row_class)],
            );
        }
    }
    // … has_many / belongs_to / load_components / etc.
}
```

`discover_resultset_class(row_class)` is the custom-ResultSet-
class resolution gap (currently red-pinned by
`goto_def_offers_custom_resultset_method`). Plugin owns the
discovery: it knows where to look (the schema's namespace pattern,
DBIC conventions), iterates `module_index` for matching
`<NS>::ResultSet::<arg>` packages, returns the resolved class or
None.

### Per-method return-type projection

DBIC's `Parametric{ResultSet, [Row]}` doesn't preserve uniformly
across method calls. The plugin needs to declare projection rules
per method:

| Method | Returns |
| --- | --- |
| `search` / `search_rs` | `Parametric{ResultSet, [Row]}` (preserve) |
| `find` / `first` / `single` / `next` | `ClassName(Row)` (project to row) |
| `create` / `find_or_new` / `update_or_create` | `ClassName(Row)` (project to row) |
| `all` / `slice` | `ArrayRef<Row>` — Tier 3 nested-hashkey territory |
| `count` / `exists` | `Numeric` |
| `update` / `delete` (no args) | `Numeric` (DBIC returns rows-affected; `$count`) |

The Phase 1 spec ("search/search_rs/find return same Parametric")
was a simplification — only true for column-key arg resolution.
For receiver-method dispatch on the result, `find`'s return is
the row class. Without per-method projection, `$rs->find(1)->name`
can't dispatch to the row's column accessor.

This is exactly the kind of per-base behavior the
parametric-semantics registry exists for. The plugin declares
the projection table; the core's `find_method_return_type` (or
the bag's MethodOnClass reducer) consults it before falling
through to the generic ResultSet method lookup.

### Semantics

The plugin registers per-flavor semantics for the Parametric values
it emits. With the `ReturnExpr` machinery landed
(`adr/return-expr.md`), DBIC's plugin contributes:

```rhai
fn parametric_semantics() {
    [
        #{
            base: "DBIx::Class::ResultSet",
            hash_key_arg_class: "type_args[0]",   // the row class
            element_type: "type_args[0]",          // ->find returns the row
            dispatch_class: "self.base",           // search/all/count on ResultSet
        },
        // For custom resultset_class — emit base = discovered name,
        // semantics rule still applies.
    ]
}
```

### Cross-file row-class references

Cross-file column completion (Part 5c test g) requires the
consumer FA to "know about" the producer's row class without
importing it. Today's enrichment story handles `imports` (explicit
`use Foo qw(...)`), not "this string-literal arg names a class
defined elsewhere." A DBIC plugin would want to participate in
enrichment: when a Parametric witness names class C as type_arg, and
C exists in `module_index`, the plugin can pre-pull C's HashKeyDefs
into the consumer's analysis (or signal to the existing enrichment
pass to do so).

`emit_call_arg_key_accesses_open`'s "the type is the gate" trick
(skip strict has_hash_key_def) becomes unnecessary if cross-file
HashKeyDefs are pre-pulled — the local symbol table HAS the
producer's keys. Cleaner.

## Cost estimate

Plugin infrastructure additions:
- New `EmitAction::ParametricInferredType` (or similar) — emit a
  Parametric witness on a call's Expression(refidx). Maybe 30 LOC
  in the plugin EmitAction enum + builder dispatch.
- Plugin-side parametric semantics registry — see
  `adr/return-expr.md` for the receiver-relative shape the
  registry plugs into. ~50 LOC depending on which option lands.
- Cross-file row-class enrichment hook — bigger; ~100 LOC of
  enrichment-pass extension. Could be cheaper if it piggybacks on
  the existing imports loop with a "Parametric type_args" map.

DBIC plugin itself:
- Port `visit_dbic_*` (~250 LOC in Rust) to Rhai or a Rust plugin
  module. Should be smaller in Rhai because the existing emit
  helpers compose more.
- Tests: most of the existing `test_dbic_*` tests in
  `builder_tests.rs` migrate to `parametric_resultset_tests.rs`-
  style fixtures (parser-driven, public surface). The
  add_columns / has_many / belongs_to fixtures are ~5 tests; the
  Part 5c parametric tests are 9.

Total: ~500 LOC plugin infra + ~300-400 LOC DBIC-specific port +
test migration. Multi-PR workstream. Worth doing in this order:

1. Land plugin infra for Parametric emission + semantics registry.
2. Move DBIC `visit_dbic_*` over to a plugin in-tree (Rust, not
   Rhai — it's load-bearing enough that we don't want runtime
   startup cost on parse).
3. Drop the in-builder DBIC code; the plugin replaces it.
4. Fix custom-resultset_class discovery (test j) inside the plugin.

## Open questions

1. **Rust plugin or Rhai plugin?** Mojo helpers / events are
   already in plugin/*.rs. DBIC's complexity (column type
   inference from `data_type`, relationship arity, custom
   resultset class discovery, prefetch unions) probably warrants
   Rust. Performance-sensitive — DBIC is in the parse path of
   most CRM-shaped projects.
2. **Where do `data_type => 'integer'` annotations land?** Today
   `add_columns(... { data_type => 'integer' })` is parsed but the
   data_type isn't typed. Plugin should map `'integer'` → Numeric,
   `'varchar'` → String, etc. Per-column return type for the
   accessor, separate from the row-class structure.
3. **Schema discovery scope.** A DBIC schema can have hundreds of
   sources. The plugin needs to find `<NS>::ResultSet::<source>`
   for each — which is a `module_index` walk per-call-site. Cache
   the discovery? Per-schema? When does it invalidate?
4. **Plugin fingerprint.** DBIC is core enough that the bundled-
   plugin fingerprint mechanism (cache invalidation when plugin
   sources change) needs to cover it — but if it's in-tree Rust,
   that's already the case.
5. **Coexistence with other Perl ORMs.** Rose::DB::Object,
   DBIx::Class::Schema::Loader, Class::DBI all have similar
   shapes. Each gets its own plugin (with similar Parametric
   emission patterns). Worth factoring shared "ORM accessor
   synthesis" helpers? Or stay per-plugin?

## Sequencing

DBIC-as-plugin is queued behind:
- `adr/return-expr.md` — landed; the receiver-relative machinery is
  the contract the plugin's semantics return.
- `prompt-type-system-encoding.md` — discussion; axis traits clarify
  what the plugin's semantics return.

The remaining gate is type-system-encoding.
Then DBIC plugin infra → DBIC plugin port → cleanup of in-builder
DBIC code.

User direction (from the design conversation): "I plan on moving
DBIC support out to a plugin, so this may want to wait on that."
The custom-resultset_class discovery, nested-hashkey Tier 2/3, and
this whole spec all queue behind the plugin extraction. The current
in-builder DBIC code is the *baseline*; the plugin port is "do it
right" once the core enables it.
