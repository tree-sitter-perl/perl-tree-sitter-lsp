# Plugin architecture — next-gen

Scratchpad for the architectural refactor discussed in the chat that
produced commits 52708ca and earlier. Lives here so compaction can't
eat it.

## The critique (user's, in their own framing)

> we don't need a million reverse indices — we need a separate kind of
> lookup that looks up in the PLUGIN CONTROLLED namespace — b/c it's
> not the PACKAGE that owns a helper, it's the plugin-controlled APP.
> what happens if you have 2 apps — we can't even model that now.

Short version of what's wrong today:

- Plugin-synthesized things (helpers, routes, tasks, events) get
  bolted onto Perl class identity. Helpers become `Method`s on
  `Mojolicious::Controller`. Tasks become `Handler`s owned by the
  literal string `"Minion"`. Lite routes attach to whatever package
  the `use Mojolicious::Lite` lives in.
- Two `Mojolicious::Lite` apps in one workspace? Two `Minion`
  instances with different task sets? The current model unions them
  under the class name and can't distinguish.
- Every cross-file lookup has to learn to walk auxiliary indices
  (`reverse_index`, `class_content_index`, `modules_with_symbol`,
  `modules_with_class_content`). Each lookup path that misses one of
  these indices breaks silently. I keep adding these paths; the user
  keeps catching me missing one.

## The direction (agreed)

A first-class `PluginNamespace`. Each is a scope managed by a plugin:

```rust
pub struct PluginNamespace {
    /// Plugin-generated unique ID. E.g. "mojo-app:/abs/path/to/MyApp.pm"
    /// or "minion:$minion@<file>:L5". Plugin decides how to disambiguate
    /// multiple instances in a single workspace.
    pub id: String,
    /// Which plugin registered this namespace.
    pub plugin_id: String,
    /// Plugin-defined kind: "app" | "minion" | "emitter" | ...
    pub kind: String,
    /// Entities registered in this namespace. `SymbolId`s — reuses the
    /// existing symbol table for name/span/kind/detail machinery.
    pub entities: Vec<SymbolId>,
    /// How Perl-space reaches into this namespace.
    pub bridges: Vec<Bridge>,
    /// The span and file where the namespace was declared (for gd/hover
    /// on the namespace itself — `gd` on a helper name can jump to the
    /// declaring `$app->helper(...)` call, or a level higher to the
    /// namespace's origin).
    pub decl_span: Span,
    pub decl_file: PathBuf,
}

pub enum Bridge {
    /// Any expression of this class (or subclass) can see this
    /// namespace's entities. `Mojolicious::Controller` for helpers;
    /// each emitter subclass for its events.
    Class(String),
    /// Bareword function call returns an instance in this namespace.
    /// `app` for Mojolicious::Lite.
    Bareword(String),
    /// Specific variable name (rare — plugin can sometimes identify
    /// a particular var: `$minion`, `$app`).
    Variable(String),
}
```

### Lookup model

When resolving a method call `$c->foo` on class `C`:

1. Real Perl methods on `C` and its ancestors (existing logic).
2. Union with entities from every `PluginNamespace` whose `bridges`
   include `Class(C)` or `Class(C's ancestor)`.

One reverse lookup: `namespaces_bridged_to(class: &str) -> &[&PluginNamespace]`.
Replaces the current patchwork of `reverse_index`, `class_content_index`,
`modules_with_symbol`, `modules_with_class_content`.

### Multi-instance support

Two Mojolicious::Lite apps in different workspace files → two
`PluginNamespace`s with `kind = "app"`, each with its own entities
and the same `Class("Mojolicious::Controller")` bridge. Unioning them
at the bridge point is lossy (no way to tell which app the user
meant) but that's OK — static analysis can't know, and at runtime
there's one-app-per-invocation anyway. Data model supports it.

## IoC — the other half

The user pushed back on pure materialization: "maybe we can consider
an event driven approach, where we ask each plugin on demand if it
has what to add?"

Pure IoC doesn't scale to enumeration queries (outline,
workspace-symbol) — every plugin would iterate its whole world on
every request. Materialization is load-bearing there.

The hybrid:

**Materialization** for enumerable data.
`PluginNamespace.entities` feeds outline + workspace-symbol + cross-file
name lookup. Plugins emit at parse time what they can statically know.

**Query hooks** for contextual answers.
Plugins implement:

```rust
trait FrameworkPlugin {
    // existing emit hooks...
    fn on_use(...) -> Vec<EmitAction>;
    fn on_function_call(...) -> Vec<EmitAction>;
    fn on_method_call(...) -> Vec<EmitAction>;

    // NEW query hooks:
    fn on_signature_help(&self, ctx: &SigHelpQueryContext) -> Option<PluginSignatureHelp> { None }
    fn on_completion(&self, ctx: &CompletionQueryContext) -> Vec<PluginCompletion> { vec![] }
    // Future: on_hover, on_diagnostic_skip, etc.
}
```

Plugin inspects the cursor context and returns what it knows. Core
runs native pipeline first, then asks plugins. Rhai plugins stay
ergonomic via a simpler wire format — plugins return Rhai maps,
the host converts to full LSP structs.

```rust
pub struct SigHelpQueryContext {
    pub call: CallFrame,           // method/function, name, receiver, args
    pub cursor_arg_index: usize,   // slot in the outer call
    pub cursor_inside: Option<ContainerFrame>,  // nested [...] or {...}
}

pub struct ContainerFrame {
    pub kind: ContainerKind,       // Array | Hash
    pub active_slot: usize,        // comma count INSIDE the container before cursor
    pub existing_keys: Vec<String>,// hash only — keys already written
}

pub struct PluginSignatureHelp {
    pub label: String,
    pub params: Vec<String>,       // param names/labels
    pub active_param: usize,
}

pub struct PluginCompletion {
    pub label: String,
    pub kind: CompletionKindHint,  // host maps to LSP kind
    pub detail: Option<String>,
    pub insert_text: Option<String>,
}
```

## What to retire once this lands

- `args_in_arrayref_at` on `SymbolDetail::Handler` and
  `EmitAction::Handler`. Replaced by minion plugin's
  `on_signature_help` checking the `ContainerFrame` itself.
- `modules_with_class_content`, `class_content_index`,
  `reverse_index` — replaced by `namespaces_bridged_to(class)`.
  Keep `get_cached` for real Perl-class cross-file lookup.
- Plugin-emitted `HashKeyDef` with `HashKeyOwner::Sub { name:
  "enqueue" }` for minion enqueue options. Replaced by minion's
  `on_completion` hook returning options directly when it sees a
  hash-literal at arg 3 of an enqueue call.

## Phased migration

**Phase 1:** Introduce `PluginNamespace` as a `FileAnalysis` field.
Add the `Bridge` enum. Keep all existing emissions (Method-on-class,
HashKeyDef-owned-by-Sub, etc.). Add `namespaces_bridged_to(class)`
and prove it returns the same shape as `modules_with_class_content`
would for existing plugins.

**Phase 2:** Migrate mojo-helpers. Emit a `PluginNamespace` with
`bridges = [Class("Mojolicious::Controller"), Class("Mojolicious")]`
and `entities = [current_user, users, admin, ...]`. Remove the
Method-on-Controller-and-Mojolicious fan-out. Update
`resolve_method_in_ancestors` + `collect_ancestor_methods` to union
entities from bridged namespaces. Old fan-out code deletes.

**Phase 3:** Same for mojo-routes, mojo-lite routes, mojo-events,
minion. Each becomes one commit.

**Phase 4:** Add `on_signature_help` + `on_completion` trait hooks.
Migrate minion's `args_in_arrayref_at` logic out of the core into
the minion plugin's hook. Remove the data flag. Similarly for
enqueue-options hash keys — becomes a `on_completion` hook return
rather than emitted HashKeyDefs.

**Phase 5:** Retire `class_content_index` / `reverse_index` /
`modules_with_class_content` / `modules_with_symbol` — no more
call sites.

## Outline scoping (user flagged)

> "tho outline would need more care, i woudn't expect the helper
> which i define in one file to be in the outline of a different file"

Outline is per-file (LSP `documentSymbol` is per-document). A
`PluginNamespace` entity's Symbol lives in the file that declared it.
Other files see the namespace via bridges for lookup/completion, but
their own outline only enumerates their own Symbols. This is already
how it works today — mojo-helpers' Method symbols are in MyApp.pm's
analysis, not in Users.pm's. No new scoping logic needed.

## Open questions for next time

- Bridge resolution order: when a plugin namespace's bridge matches a
  class AND a cross-file cached module also has that class, which
  wins on a name conflict? Probably local first, then namespaces by
  ID order. Need a test.
- How do plugin namespaces round-trip through the SQLite cache?
  Entities are Symbols, already serialized. Namespaces themselves are
  small — just IDs + bridges. Store as JSON in a new column.
- Can two plugins collaborate on one namespace? E.g. mojo-lite declares
  the app namespace; mojo-helpers adds entities to it. Probably yes
  — namespaces keyed by ID, plugins emit into existing IDs. Need to
  pin the contract.
