//! Framework plugin system.
//!
//! Plugins are pure functions from a hook `Context` to a `Vec<EmitAction>`.
//! Each `EmitAction` is converted into normal builder emissions by
//! `apply_action`, so plugin-produced `Symbol`/`Ref`/`HashKeyDef` entries are
//! indistinguishable from native ones downstream — const folding, cross-file
//! refs, and enrichment all compose automatically.
//!
//! The trait is object-safe so we can hold a mix of native Rust plugins and
//! Rhai-script plugins behind one registry.

use serde::{Deserialize, Serialize};

use crate::file_analysis::{
    AccessKind, Bridge, HandlerDisplay, HandlerOwner, HashKeyOwner, InferredType, ParamInfo, Span,
    SymKind, SymbolDetail,
};

pub mod rhai_host;

/// Process-wide plugin registry, built once with the bundled Rhai plugins
/// plus anything in `plugin_search_dirs()` (`$PERL_LSP_PLUGIN_DIR` and the
/// nearest repo-local `.perl-lsp/`). All `build()` calls share it; tests
/// that need isolation use `build_with_plugins()`.
pub fn default_plugin_registry() -> std::sync::Arc<PluginRegistry> {
    use std::sync::{Arc, OnceLock};
    static REG: OnceLock<Arc<PluginRegistry>> = OnceLock::new();
    REG.get_or_init(|| {
        let engine = Arc::new(rhai_host::make_engine());
        let mut reg = PluginRegistry::new();
        for p in rhai_host::load_bundled(engine.clone()) {
            reg.register(p);
        }
        for dir in rhai_host::plugin_search_dirs() {
            for p in rhai_host::load_plugin_dir(&dir, engine.clone()) {
                reg.register(p);
            }
        }
        Arc::new(reg)
    })
    .clone()
}

// ---- Context snapshots passed to plugins ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallKind {
    Function,
    Method,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArgInfo {
    /// Raw source text of the argument node.
    pub text: String,
    /// Constant-folded string if the arg is a literal/bareword or resolves
    /// through `constant_strings`. `None` means "runtime/unknown".
    /// Multi-valued folds (a loop variable over `qw(...)`, the postfix
    /// `for` topic) surface their FIRST value here for back-compat;
    /// `string_values` carries them all.
    pub string_value: Option<String>,
    /// Every constant-folded candidate for this arg. Single-valued for
    /// literals; the full list for loop-variable / `$_` folds — a
    /// registration loop (`$app->helper($_ => …) for qw(a b c)`) is N
    /// registrations, and plugins fan out over this.
    #[serde(default)]
    pub string_values: Vec<String>,
    pub span: Span,
    /// For string-literal args, the span of the INNER content only —
    /// excludes the quote delimiters, heredoc markers, or `q{}`/`qq!!`
    /// wrappers. Comes directly from the `string_content` tree node,
    /// so plugins never need to compute offsets into the raw text.
    ///
    /// `None` for non-string args or empty string literals (no
    /// `string_content` child to point at).
    #[serde(default)]
    pub content_span: Option<Span>,
    pub inferred_type: Option<InferredType>,
    /// The arg's syntactic value shape, classified one level deep —
    /// `Str` / `Num` / `HashPairs` (`{ k => v }`) / `ArrayItems`
    /// (`[ a, b ]` / `[qw/…/]`) / `Other`. This is the generic primitive
    /// for argument-shape-polymorphic verbs: a plugin reads
    /// `args[i].value_shape` and branches on the shape its own vocabulary
    /// cares about (Mojo's `query`/`stash`/`to` each accept a hashref,
    /// arrayref, pair list, or string for the same slot). Core owns the
    /// tree walk (rule #1); the plugin owns "what this shape means here"
    /// (rule #8/#10). Populated for every arg.
    #[serde(default)]
    pub value_shape: ValueShape,
    /// If this arg is an anonymous sub (`sub ($a, $b) { ... }` or a block
    /// that begins with `my ($a, $b) = @_`), its extracted param list.
    /// Used by handler-registration plugins (Mojo events, Dancer routes,
    /// etc.) to capture the handler's signature and store it on the
    /// HashKeyDef so signature help can surface it at call sites.
    #[serde(default)]
    pub sub_params: Vec<EmittedParam>,
    /// Witness-bag attachment whose type IS this arg's callable
    /// return when invoked, projected from
    /// `arg.inferred_type.callable_return_edge()`. Plugins emit
    /// `Symbol(method_id) → Edge(target)` against it so a
    /// synthesized callable's return follows the source
    /// callable's return at query time.
    ///
    /// Three reachability shapes resolve uniformly:
    ///
    /// ```perl
    /// $app->helper(name => sub { ... });          # anon literal
    /// my $sub = sub { ... };
    /// $app->helper(name => $sub);                 # rebound anon
    /// $app->helper(name => \&Foo::bar);           # named ref (cross-file ok)
    /// ```
    ///
    /// Anon-sub origins yield `Expr(body_last_expr_span)`; named-
    /// sub references yield `MethodOnClass{class, name}`, which
    /// resolves through the bag's MRO + `module_index` machinery
    /// — so cross-file `\&Foo::bar` works without any consumer-
    /// side branching. `None` for genuinely opaque coderefs
    /// (params typed `CodeRef`, deref-shape narrowing, etc.).
    #[serde(default)]
    pub callable_return_edge: Option<crate::witnesses::WitnessAttachment>,
    /// If this arg is a refgen of a named sub (`\&foo`, `\&Foo::bar`,
    /// `\&$const_folded`), the referenced sub name (qualified or bare,
    /// exactly as `extract_names_from_refgen` yields). Lets a
    /// registration plugin (`->helper(name => \&_greet)`) carry the
    /// callback's first-param typing to the *named* sub's body, the
    /// same as it does for an inline `sub { ... }` via `sub_params`.
    /// `None` for non-refgen args and unresolvable `\&$var` names.
    #[serde(default)]
    pub ref_sub_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallContext {
    pub call_kind: CallKind,
    /// For `function_call_expression`: the callee name (e.g. `"has"`,
    /// `"__PACKAGE__->add_columns"`). For method calls: `None`.
    pub function_name: Option<String>,
    /// For method calls: the method identifier (e.g. `"on"`).
    pub method_name: Option<String>,
    /// Raw text of receiver (`$self`, `__PACKAGE__`, etc.). Methods only.
    pub receiver_text: Option<String>,
    /// When the receiver is itself a CALL (`get('/x')->to(...)`), the
    /// called function's name — a generic syntax fact, so plugins can
    /// match their own DSL verbs without core knowing any names.
    #[serde(default)]
    pub receiver_call_name: Option<String>,
    /// Resolved receiver type if inference succeeded.
    pub receiver_type: Option<InferredType>,
    /// Route defaults inherited by the receiver value, flattened to a
    /// `[[key, value], ...]` list the Rhai side can read directly
    /// (`controller` is the distinguished key). Filled from the
    /// receiver's `InferredType::BrandedRoute` brand by the builder so
    /// a partial `->to('#action')` plugin can recover the inherited
    /// controller without inspecting the enum's serde shape. Empty for
    /// non-route receivers. See `docs/adr/route-branding.md`.
    #[serde(default)]
    pub receiver_route_defaults: Vec<(String, String)>,
    pub args: Vec<ArgInfo>,
    pub call_span: Span,
    pub selection_span: Span,
    pub current_package: Option<String>,
    pub current_package_parents: Vec<String>,
    pub current_package_uses: Vec<String>,
    /// Structurally-extracted Moo/Moose `has` options, present ONLY on the
    /// `has` function call. The builder walks the option nodes (rule #1) and
    /// hands the plugin the decision-ready shape — attribute name(s), the
    /// resolved `isa` type, and each accessor option keyword with its value
    /// already classified (shorthand `=> 1` vs explicit name vs `handles`
    /// delegation pairs). The plugin owns the *vocabulary*: which keyword
    /// synthesizes which method-name pattern and return behavior. See
    /// `frameworks/moo.rhai`. `None` for every other call.
    #[serde(default)]
    pub has_options: Option<HasOptions>,
    /// The call's positional args as a flat `(name, span)` string list —
    /// string-literal content, barewords, autoquoted keys, `qw(...)`
    /// words, foldable constants; non-string args (hashrefs, coderefs)
    /// carry no name. Via `cst::string_list` (rule #1), so a list-DSL
    /// plugin reads its column / export names tree-free. Populated only
    /// for verbs a plugin registered via [`FrameworkPlugin::arg_name_verbs`].
    #[serde(default)]
    pub arg_names: Vec<(String, Span)>,
    /// True when a method call's receiver is the current package itself
    /// (`__PACKAGE__->m(...)` / `CurrentClass->m(...)`) — a class-level
    /// call. Class-declaration DSLs (`add_columns`, `load_components`)
    /// gate on it: an instance call (`$rs->add_columns(...)`) is a runtime
    /// op, not a declaration. False for function calls.
    #[serde(default)]
    pub receiver_is_package: bool,
}

/// A Moo/Moose `has` declaration's non-pair head: the attribute name(s)
/// and the resolved `isa` type. The accessor *options* are read by the
/// plugin itself via the shared `classified_pairs` over the flattened args.
///
/// `isa_type` is the one Moo-semantic field core resolves (`'Str'` →
/// `String`, `"InstanceOf['X']"` → `InstanceOf`). Moving it onto the
/// `type_constraint_*` seam — after which this struct dissolves entirely —
/// is roadmapped.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HasOptions {
    /// `(attribute_name, name_token_span)` — one per attr (`has [qw/a b/]`
    /// declares several). The span is the synthesized method's
    /// selection_span so goto-def lands on the `has` line.
    pub attr_names: Vec<(String, Span)>,
    /// The attribute's resolved `isa` type, if any. When this is a
    /// `ClassName`, a `handles` delegation can edge the delegated method's
    /// return to the remote method on that class.
    #[serde(default)]
    pub isa_type: Option<InferredType>,
}

/// The classified shape of a fat-comma pair's value — generic, no DSL
/// vocabulary; the plugin maps `(keyword, shape)` to behavior. Every
/// variant carries data so it serializes to a single-key map: Rhai reads
/// `value.Str` / `value.HashPairs` / … as `()` when absent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ValueShape {
    /// A string literal's content or a bareword's text.
    Str(String),
    /// A numeric literal's text (`1` is the Moo `=> 1` shorthand).
    Num(String),
    /// `{ k => v, ... }` flattened to `(key, value)` string pairs.
    HashPairs(Vec<(String, String)>),
    /// `[ a, b ]` / `[qw/a b/]` flattened to its string items.
    ArrayItems(Vec<String>),
    /// A value we don't classify (coderef, scalar var, nested expr);
    /// carries the node kind for debugging.
    Other(String),
}

impl Default for ValueShape {
    /// Unclassified — the back-compat default when a serialized
    /// `ArgInfo` omits `value_shape`.
    fn default() -> Self {
        ValueShape::Other(String::new())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UseContext {
    pub module_name: String,
    /// Imports parsed out of the use-list (strings, barewords, qw()).
    pub imports: Vec<String>,
    /// Raw text of each arg — lets plugins detect flags like `-strict`.
    pub raw_args: Vec<String>,
    pub current_package: Option<String>,
    pub span: Span,
}

// ---- Emit actions — what a plugin produces ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmittedParam {
    pub name: String,
    pub default: Option<String>,
    pub is_slurpy: bool,
    /// Set on params that the user never types at the call site
    /// (`$self` for Perl methods, `$c` for Mojolicious helpers, …).
    /// Whoever emits the params decides — the core never infers
    /// invocancy from a name.
    #[serde(default)]
    pub is_invocant: bool,
}

impl From<EmittedParam> for ParamInfo {
    fn from(p: EmittedParam) -> Self {
        ParamInfo {
            name: p.name,
            default: p.default,
            is_slurpy: p.is_slurpy,
            is_invocant: p.is_invocant,
        }
    }
}

/// What a plugin's emit hook can contribute to the builder.
///
/// Two classes of action live in this enum; the distinction matters
/// when adding a new variant.
///
/// **Data emissions** (`Method`, `HashKeyDef`, `HashKeyAccess`,
/// `Handler`, `DispatchCall`, `MethodCallRef`, `Symbol`, `PackageParent`,
/// `PluginNamespace`) contribute to the serialized `FileAnalysis` graph.
/// They round-trip through bincode and survive the module boundary.
/// Adding one of these is purely additive — no builder coupling.
///
/// **Builder side-effects** (`FrameworkImport`, `VarType`) reach INTO
/// the builder's in-progress state (`framework_imports`,
/// `deferred_var_types`). They're pragmatic: the Perl-semantic data
/// they carry doesn't fit the plain Symbol/Ref vocabulary (the callback
/// body's scope doesn't exist yet when `VarType` is emitted), but they
/// ARE the exceptions — prefer a data emission when you can express the
/// same thing that way. New side-effect variants need explicit sign-off.
/// See [`FrameworkPlugin::topic_route_dsl`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TopicRouteDsl {
    /// The `use` that puts the DSL in scope (gates the stack).
    pub module: String,
    /// Route verbs whose CALL can stand as a `->to` receiver.
    pub verbs: Vec<String>,
    /// The block function that scopes the base (push on entry, pop on
    /// exit).
    pub group_fn: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EmitAction {
    // ==== Data emissions ====

    /// Set the current topic-route base (the controller following
    /// sibling routes inherit). Applied to the innermost open scope
    /// frame of the walk's topic-route stack; the plugin that parses
    /// `->to('ctrl#…')` emits it when the receiver is its declared
    /// base-setter verb. Core never parses route specs.
    SetRouteBase {
        controller: String,
    },

    /// Emit an accessor-style method. Shorthand for Symbol + SymKind::Method +
    /// SymbolDetail::Sub. The plugin's id becomes the symbol's Namespace tag.
    ///
    /// `on_class` lets a plugin attach the method to a class other than the
    /// current one — used by Mojo helpers where `$app->helper(name => sub)`
    /// in a Lite script actually registers `name` as a method on
    /// `Mojolicious::Controller`. Default `None` means "use current
    /// package", matching the normal behavior for everything else.
    Method {
        name: String,
        span: Span,
        selection_span: Span,
        params: Vec<EmittedParam>,
        is_method: bool,
        return_type: Option<InferredType>,
        doc: Option<String>,
        #[serde(default)]
        on_class: Option<String>,
        /// Plugin's choice of LSP display kind (shown in outline + completion).
        /// `None` = use the default (Method → METHOD, Sub → FUNCTION) — the
        /// common case for plugins that just need "a method lives here". Set
        /// this when the synthesized callable is semantically distinct from a
        /// hand-written method (helpers, routes, DSL verbs, …).
        #[serde(default)]
        display: Option<HandlerDisplay>,
        /// Hide this symbol from the outline. Use for framework
        /// imports (Mojolicious::Lite's `get`/`post`/...) and other
        /// synthesized infrastructure that shouldn't clutter
        /// navigation — hover/gd/completion still work.
        #[serde(default)]
        hide_in_outline: bool,
        /// The return type is internal plumbing. Participates in
        /// chain resolution (`$app->admin->users` walks through it)
        /// but isn't rendered in completion / hover / inlay hints.
        /// Plugin's call — it knows whether its return type is a
        /// real user-facing class or a synthetic pass-through.
        #[serde(default)]
        opaque_return: bool,
        /// Outline-only identifier override. `name` stays authoritative
        /// for method resolution — a Mojo helper chained leaf needs
        /// `name: "create"` so `$c->users->create` resolves via the
        /// proxy — but in the outline the user wants to see the full
        /// dotted path `"users.create"`. Plugin sets this to the
        /// identifier part only; the core prepends the kind word
        /// ("helper") and appends the non-invocant params.
        #[serde(default)]
        outline_label: Option<String>,
        /// Lazy return type via `Edge` to a witness-bag attachment
        /// — the synthesized Method's return type IS that
        /// attachment's type, resolved at query time. The target
        /// can be:
        ///
        ///   - `Expr(span)` for anon-sub bodies (the bag walks
        ///     the body's last-expression witnesses).
        ///   - `MethodOnClass{class, name}` for named-sub
        ///     references (the bag's MRO + cross-file machinery
        ///     resolves it).
        ///
        /// Set to `args[N].callable_return_edge` for the relevant
        /// callback arg — works for anon literals, rebound
        /// scalars, AND `\&Foo::bar` references (cross-file). The
        /// host populates `callable_return_edge` from the arg's
        /// bag-resolved `CodeRef` shape.
        ///
        /// Mutually exclusive with `return_type` in spirit — if
        /// both are set, `return_type` wins (the plugin overrode
        /// explicitly). Builder pushes `Symbol(sid) → Edge(target)`;
        /// the bag's edge-chase resolver follows it at query time.
        #[serde(default)]
        return_via_edge: Option<crate::witnesses::WitnessAttachment>,
        /// The attr this accessor PROJECTS (Moo `predicate`/`clearer`/
        /// custom `reader`/`writer` names derived from `has 'attr'`).
        /// Setting it enrolls the method in the attr's projection group:
        /// references from any group spelling include this accessor's
        /// call sites, and rename re-derives the name when the accessor
        /// embeds the attr (`has_size` → `has_extent`). One field — the
        /// whole group DX for plugins.
        #[serde(default)]
        attr: Option<String>,
    },
    /// Synthesize a `HashKeyDef` for a constructor/stash/etc. key.
    HashKeyDef {
        name: String,
        owner: HashKeyOwner,
        span: Span,
        selection_span: Span,
    },
    /// Emit a `HashKeyAccess` reference targeting a def (typically emitted
    /// by a partner call site — e.g. `->emit('x')` references the def
    /// produced by `->on('x', sub {})`). The owner + name pair is how
    /// `refs_by_target` pairs accesses to their definition across files.
    HashKeyAccess {
        /// Key name (e.g. `"connect"`). This is the `target_name` on the Ref.
        name: String,
        owner: HashKeyOwner,
        /// Text of the receiver expression for hover info. Empty is fine.
        var_text: String,
        /// Exact span of the key name — this is what cursor-at-token lookups
        /// match against, so it must be tight around the token.
        span: Span,
        access: AccessKind,
    },
    /// Emit an import-name reference at a span, pinned to a package — the
    /// plugin-facing equivalent of core's `qw(...)` / `-as` import-token refs
    /// (`emit_refs_for_strings`). A BYO exporter plugin uses it to make its own
    /// rename-spec tokens navigable, identically to core: emit the **remote**
    /// name with `package = Some(exporting_module)` so it renames with the
    /// source sub, and a renaming alias's **local** name with `package =
    /// Some(consuming_package)` so it forms a self-contained local group that
    /// never touches the exporter.
    ImportRef {
        name: String,
        #[serde(default)]
        package: Option<String>,
        span: Span,
    },
    /// Register a parent class on the current package. Used for
    /// `use Mojo::Base 'App'` style inheritance detection.
    PackageParent { package: String, parent: String },
    /// Register a named Handler on a class — a string-dispatched callable
    /// that isn't a Perl method. Multiple Handlers with the same
    /// (owner, name) stack instead of overriding. `dispatchers` is the
    /// set of method names that route to this handler by string (e.g.
    /// `["emit", "subscribe"]` for Mojo events). `params` is the
    /// handler sub's signature, used by signature help at call sites.
    /// `display` picks the LSP kind shown in outline/completion —
    /// routes are `Method`, events are `Event`, config keys are
    /// `Field`, etc. Plugin's call, not the core's.
    Handler {
        name: String,
        owner: HandlerOwner,
        dispatchers: Vec<String>,
        params: Vec<EmittedParam>,
        span: Span,
        selection_span: Span,
        #[serde(default)]
        display: HandlerDisplay,
        /// Hide from document outline (see Method variant).
        #[serde(default)]
        hide_in_outline: bool,
        /// Outline-only identifier override. `name` stays authoritative
        /// for dispatch lookups (a `->emit('ready')` DispatchCall
        /// matches Handlers by `name`), but the outline can show a
        /// richer identifier — e.g. a mojo-lite route uses this to
        /// prepend the HTTP verb ("GET /users/profile") so two
        /// handlers on the same path (GET + POST) don't look
        /// identical. Core prepends the kind word ("route") and
        /// appends non-invocant params.
        #[serde(default)]
        outline_label: Option<String>,
    },
    /// Emit a call-site reference for a Handler — e.g. the cursor is on
    /// `'ready'` in `$x->emit('ready', ...)`. `dispatcher` is the
    /// method name doing the dispatching so features can describe the
    /// call shape; `owner` pairs the access to its Handler def across
    /// files via `resolve::refs_to`.
    DispatchCall {
        /// Handler name (first string arg of the dispatch call).
        name: String,
        dispatcher: String,
        owner: HandlerOwner,
        /// Span of the handler-name token — tight so cursor lookups hit.
        span: Span,
        /// Receiver expression text, e.g. `"$emitter"` or `"$self"`.
        /// Informational only; empty is fine.
        var_text: String,
    },
    /// Emit a MethodCall reference pointing at a named method on a known
    /// class. Used where a string or other expression represents a
    /// cross-file call — e.g. Mojolicious routes write
    /// `->to('Users#list')`, Catalyst writes `->forward('/users/list')`,
    /// DBIC has `->$relname(...)` patterns. The plugin parses the
    /// target out of its own domain syntax and surfaces it as a
    /// generic MethodCall ref; from that point on goto-def, references,
    /// rename, and hover work via the standard method-resolution path
    /// (with inheritance walk + workspace index).
    /// "This call LOADS module `name` and passes it a config value."
    /// The caller-side half of loader-config param typing: the fact
    /// rides the calling file's FileAnalysis; enrichment of the LOADED
    /// module joins it with a `from_loader_config` param marker and
    /// types the param from the value at `config_span`.
    PluginLoad {
        name: String,
        #[serde(default)]
        config_span: Option<Span>,
    },
    MethodCallRef {
        /// The method name (e.g. `"list"` from `"Users#list"`).
        method_name: String,
        /// The invocant as text (e.g. `"Users"` — treated the same as
        /// a bare package receiver in normal method-call resolution).
        invocant: String,
        /// Span of the method-name token for cursor-precision features.
        /// Plugins often pass the whole string-literal span when they
        /// can't subspan cheaply; that's fine for goto-def.
        span: Span,
        /// Optional span of just the invocant token (when the plugin
        /// can extract it — e.g. the `"Users"` part of `"Users#list"`).
        #[serde(default)]
        invocant_span: Option<Span>,
    },
    /// Full control: emit an arbitrary Symbol.
    ///
    /// `return_type` lives at the action level (not on the
    /// `SymbolDetail`) since D1 of the bag-residual refactor: the
    /// per-symbol return type is bag-resident now, not a field on
    /// `SymbolDetail::Sub`. Plugins synthesizing typed callables
    /// (Mojolicious::Lite's `app`, etc.) populate this so the
    /// builder publishes the type into the bag at the same time it
    /// adds the symbol.
    Symbol {
        name: String,
        kind: SymKind,
        span: Span,
        selection_span: Span,
        detail: SymbolDetail,
        #[serde(default)]
        return_type: Option<InferredType>,
    },
    /// Declare a plugin namespace — a scope the plugin owns, with
    /// bridges describing how Perl-space expressions reach it. The
    /// builder collects namespaces declared via this action and
    /// places them in `FileAnalysis.plugin_namespaces`; lookups
    /// union entities from every namespace whose bridges match the
    /// Perl class at the cursor.
    ///
    /// Entities are `SymbolId`s — plugins emit `Method` / `Handler` /
    /// `Symbol` actions as usual to populate the symbol table, then
    /// reference their IDs here. Rhai plugins: index returned
    /// SymbolIds from `symbol_id(...)` helper (populated as of Phase 1).
    PluginNamespace {
        /// Plugin-generated unique identifier.
        id: String,
        /// Plugin-defined kind tag — `"app"`, `"minion"`, `"emitter"`, ….
        kind: String,
        /// Which Perl-space shapes reach this namespace.
        bridges: Vec<Bridge>,
        /// Names of entities the plugin emitted in this same dispatch.
        /// The builder resolves each name to every matching `Symbol`
        /// in the symbol table stamped with the same plugin's
        /// `Namespace::Framework { id }`. Fan-out-on-multiple-classes
        /// emissions (e.g. mojo-helpers' current_user on Controller
        /// AND Mojolicious) all land in the same namespace via one
        /// name lookup — plugins don't have to track SymbolIds.
        #[serde(default)]
        entity_names: Vec<String>,
        /// Span the plugin is registering at — typically the
        /// registration call (`$app->plugin('Minion', ...)` etc.).
        decl_span: Span,
    },
    // ==== Builder side-effects ====
    // These reach into builder internals rather than adding to the
    // serialized graph. They work, but they're the exception, not
    // the pattern — expand the data vocabulary before adding new ones.

    /// Implicit keyword import (same purpose as builder's
    /// `framework_imports`). Silences the unresolved-function
    /// diagnostic for DSL verbs (`get`, `post`, `has`, `with`, ...)
    /// that a framework auto-imports. Side-effect: mutates
    /// `Builder.framework_imports`.
    FrameworkImport { keyword: String },

    /// Synthesize an `Import` for names a framework pulls into scope at
    /// import time. `use Mojolicious::Lite` monkey-patches `get`, `post`,
    /// `helper`, ... into the caller as thin pass-throughs to real
    /// methods on `Mojolicious::Routes::Route` and `Mojolicious` — the
    /// plugin emits this so hover/gd/sig-help on those names flow through
    /// the existing imported-function resolution path to the real
    /// source module (no fabricated docs).
    ///
    /// `imported_symbols` supports renaming imports: `del` in
    /// Mojolicious::Lite is really `delete` on Route, so the plugin
    /// emits `ImportedSymbol::renamed("del", "delete")`. Same-name
    /// imports (the common case) use `ImportedSymbol::same(name)`.
    ///
    /// `span` is the `use` statement's span — used for gd's "jump to
    /// use statement" branch and for range-based diagnostics.
    Import {
        module_name: String,
        imported_symbols: Vec<crate::file_analysis::ImportedSymbol>,
        span: Span,
    },

    /// Declare a type for a variable inside a scope. Plugins use this
    /// when they know a framework-provided variable's type that the
    /// builder can't infer — the classic case being callback arguments:
    /// `$app->helper(NAME => sub { my ($c) = @_; ... })` — the plugin
    /// knows `$c` is a Mojolicious controller, the core doesn't.
    /// `at` names any point inside the scope the constraint should
    /// apply to (typically the callback body's span); the builder
    /// resolves it to an actual scope via `scope_at(at)`. Side-effect:
    /// queues a deferred scope resolution into
    /// `Builder.deferred_var_types`.
    VarType {
        variable: String,
        at: Span,
        inferred_type: InferredType,
    },

    /// Type the `param_index`-th positional of a *named* sub by name.
    /// The named-sub analogue of `VarType`: `->helper(name => \&_greet)`
    /// types `_greet`'s first positional as `Mojolicious::Controller`,
    /// just as the inline `->helper(name => sub ($c) {...})` form types
    /// the closure's first positional. The plugin can't anchor a
    /// `VarType` at the sub's body — a `\&name` arg carries no body span,
    /// and the sub may be a forward reference not yet walked. The builder
    /// defers resolution to end-of-build (`deferred_named_sub_param_types`):
    /// it finds the sub's scope + the named param's variable, then pushes
    /// the TC. Independent of the sub's name allowlist — driven purely by
    /// the registration shape (rule #10). `class` matches via the
    /// enclosing package (bare names) or qualifier (`Foo::bar`).
    NamedSubParamType {
        sub_name: String,
        param_index: usize,
        inferred_type: InferredType,
    },

    /// Inject a `use` statement as if it appeared in source at `span`.
    /// Equivalent in every respect to the user having written
    /// `use <module> <args>` at that point — same plugin dispatch,
    /// same framework detection, same Import/Module symbol emission,
    /// same `package_uses` / `package_parents` / `framework_imports`
    /// writes.
    ///
    /// Powers "style kits" (`Import::Base` subclasses, `ToolKit`,
    /// company-wide `use Co::Base -Class` shims) — a single user-facing
    /// `use` line collapses a dozen real `use`s, and the LSP needs to
    /// see those reals to make `has`, `with`, `extends`, etc. work.
    /// Kit plugins react to the user's outer use via `on_use`, then
    /// emit one `SyntheticUse` per inner real-use the kit performs.
    ///
    /// Re-entry is the point: the synthetic re-dispatches every
    /// applicable `on_use` hook, including the emitting plugin's own.
    /// `Builder.use_dedup` breaks cycles by
    /// `(package, module, args, imports)`.
    ///
    /// All four fields mirror what `visit_use` extracts from a real
    /// CST node, so the synthetic path is call-compatible with the
    /// real path's worker — no `synthetic: bool` flag inside the
    /// builder.
    ///
    /// **Provenance.** The synthesized Module symbol carries the
    /// emitting plugin's `Namespace::Framework { id }` tag (vs. real
    /// `use` lines, whose Module symbol stays on `Namespace::Language`).
    /// `--dump-package` / outline / completion filters use the
    /// namespace channel to surface "this came from plugin X" for
    /// every other plugin-emitted symbol; SyntheticUse joins the same
    /// channel. Anything downstream of the synthetic (re-entered
    /// `on_use` hooks, has-synthesizers, etc.) gets its OWN emitter's
    /// id through the regular `apply_emit_action` path — so a
    /// `co-base → Moo → has` chain ends up with the Module tagged
    /// `co-base` and each synthesized accessor tagged `moo`.
    ///
    /// **Known limitation: `use constant`.** `accumulate_use_constant`
    /// reads the value side of the fat-comma pair from the CST. A
    /// synthetic `SyntheticUse { module: "constant", ... }` has no
    /// source to scan, so `constant_strings` does NOT get populated —
    /// the rest of the use-handling (Module symbol, package_uses,
    /// Import entry, plugin re-dispatch) still runs. Kit plugins
    /// that need to inject constants should request a dedicated
    /// `EmitAction::ConstantString { name, values }` (not yet
    /// available — add when a real plugin needs it).
    SyntheticUse {
        module: String,
        /// Raw arg tokens, exactly as `extract_mojo_base_args` would
        /// produce from a real CST. Includes barewords like `-Class`
        /// and quoted parents like `'Mojolicious::Plugin'`.
        #[serde(default)]
        args: Vec<String>,
        /// qw-style imports (the named-symbol list a real
        /// `extract_use_import_list` would yield).
        #[serde(default)]
        imports: Vec<String>,
        /// Span the synthetic use is "attributed to" — typically the
        /// emitting plugin's `ctx.span` (the original real `use` that
        /// triggered the kit expansion).
        span: Span,
    },
}

// ---- Plugin trait ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Trigger {
    /// Fire when the current package has `use MODULE`. Exact match on the
    /// module argument of `use`.
    UsesModule(String),
    /// Fire when any parent of the current package equals `prefix` or starts
    /// with `prefix::` (prefix match covers `DBIx::Class::Core` under the
    /// trigger `DBIx::Class`).
    ClassIsa(String),
    /// Unconditional — useful for plugins that do their own checks in hooks.
    Always,
}

/// A plugin-asserted return type for a known sub/method. Plugins ship
/// these as a static manifest (`overrides()` on the trait, `fn
/// overrides()` at the top of a `.rhai` script) for cases where
/// inference can't, or shouldn't, reach the right answer — Mojolicious'
/// `_route` returns `$self` via an array slice the inference engine
/// doesn't model, for example.
///
/// Overrides are NOT gated by triggers. They're consulted during every
/// build's post-pass; if a local symbol matches the target, the
/// override wins over whatever inference produced (with provenance
/// recorded in `FileAnalysis.type_provenance` for debugging).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeOverride {
    pub target: OverrideTarget,
    pub return_type: InferredType,
    /// Free-form prose surfacing in `TypeProvenance::PluginOverride.reason`.
    /// Read by humans only — keep it explanatory ("returns $self via
    /// the @_-shift / array-slice idiom that inference doesn't model").
    pub reason: String,
}

/// A plugin-declared "this method dispatches a named handler when its
/// receiver is a `target_class` (or a subclass)" rule. Unlike the
/// `on_method_call` emit hook — which fires only when the *file*'s
/// triggers match and can't see cross-file inheritance — a dispatch verb
/// is resolved at **enrichment** time, against the receiver's actual
/// (cross-file-resolved) class. So `$minion->enqueue('T')` lights up
/// wherever a `$minion` typed as a Minion subclass is in scope, no matter
/// what `use` statements the surrounding file has. The builder records a
/// provisional candidate for every call matching `verb`; enrichment keeps
/// the ones whose receiver `isa target_class` and pairs them to the
/// `owner_class` Handler.
///
/// Like `overrides`, this manifest is NOT trigger-gated — it's read from
/// every loaded plugin and unioned.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DispatchVerb {
    /// Method name that performs the dispatch (`enqueue`, `enqueue_p`).
    pub verb: String,
    /// Receiver class the verb belongs to; the call only counts when the
    /// receiver's resolved class `isa` this (cross-file inheritance walk).
    pub target_class: String,
    /// Handler owner the synthesized `DispatchCall` pairs against —
    /// usually the same as `target_class`, but kept distinct so a verb on
    /// one class can dispatch into another's registry.
    pub owner_class: String,
    /// Positional index of the handler-name argument. Almost always 0;
    /// some sugar passes a label first and the name second.
    #[serde(default)]
    pub name_arg_index: usize,
}

/// A method that LOADS a module, naming it in a positional string
/// argument — `$app->plugin('X')`, `$app->plugin($_) for qw/A B/`.
/// Recognized TRIGGER-INDEPENDENTLY (like `DispatchVerb`): the load
/// happens on the receiver app regardless of the enclosing file's
/// class — a `Mojolicious::Plugin` subclass, never an app, for the
/// whole nested-plugin cascade. `receiver_class` is the honest intent
/// (the app type); the load FACT is a suppression signal for the
/// entrypoint lint, so recording errs toward silence rather than
/// gating on a receiver type that only resolves at query time (see
/// `record_plugin_loads`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoadVerb {
    pub verb: String,
    pub receiver_class: String,
    #[serde(default)]
    pub name_arg_index: usize,
}

/// One extracted parameter of a parametric type-constraint constructor
/// (`InstanceOf['Foo']` → one param with `string: "Foo"`). The builder
/// fills exactly one of the two fields per param (rule #1 — it walked the
/// node so the plugin doesn't have to): `string` for a string-literal
/// param (class names, enum values), `ty` for a nested type param (`Int`,
/// `InstanceOf[...]`, …) once nesting is resolved. Handed to
/// `type_constraint_inner` so the plugin folds however many params there
/// are into the constrained inner type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintParam {
    #[serde(default)]
    pub string: Option<String>,
    #[serde(default)]
    pub ty: Option<InferredType>,
}

/// A plugin-declared parameter type for a role-contract method: "in a class
/// that does `in_role`, the sub `method`'s parameter at index `param` has type
/// `ClassName(type_class)`." The motivating case is a framework role whose
/// required method has a typed argument the source can't express — e.g.
/// `Clove::Upgrade::OneTime`'s `sub run_upgrade ($self, $app)`, where `$app`
/// is the `Mojolicious` app. The builder applies this at the sub-declaration
/// walk (the one place that sees a sub's params, rule #1), pushing a Variable
/// type constraint for the param — the same mechanism `detect_first_param_type`
/// uses for `$self`. See `docs/adr/plugin-system.md` (`param_types()`).
/// (Callback-arg param
/// typing — `$job` in a Minion task — stays in the per-plugin `VarType` emit;
/// this manifest is the *declaration*-site selector that has no hook.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamType {
    /// Method name to match. `None` matches every method in the class —
    /// the "any action in a controller" case (Catalyst `$c`, PSGI handlers).
    /// Plugin authors express this by omitting the `method` key in Rhai.
    #[serde(default)]
    pub method: Option<String>,
    /// The enclosing package must do/inherit this role/class (matched against
    /// the package's resolved ancestry — `with` / `extends` / `isa` / `does`).
    pub in_role: String,
    pub param: usize,
    pub type_class: String,
    /// Gate the rule on the sub carrying a declaration attribute (`:Local`,
    /// `:Chained`, `:Args`, …). The Catalyst `$c` case: only *action* methods
    /// receive the context object, and the only honest signal separating an
    /// action from a plain helper is the attribute. A wildcard `param: 1` rule
    /// without this gate types the 2nd param of EVERY sub in the controller —
    /// `$page` in a pagination helper, `$path` in a request builder — all
    /// falsely `Catalyst`. Plugins set `requires_action_attr: true` in Rhai;
    /// `ACCEPT_CONTEXT`/`COMPONENT` (Models, which have no attribute) stay as
    /// *named* rules that don't set this.
    #[serde(default)]
    pub requires_action_attr: bool,
    /// The param's REAL type is whatever value the loader call passed
    /// (`plugin 'X', {...}` → register's `$conf`): enrichment joins
    /// the callee-side marker this mints with caller-side
    /// `PluginLoad` facts and pushes the gathered shape. `type_class`
    /// stays as the static fallback (HashRef for Mojo's `$conf`) —
    /// the bag's structure-dominates-rep subsumption prefers the
    /// gathered shape when both land.
    #[serde(default)]
    pub from_loader_config: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OverrideTarget {
    /// Method `name` defined on the package `class`. Match is by exact
    /// package name on the symbol — does NOT walk the inheritance chain
    /// (override the home class, not its callers).
    Method { class: String, name: String },
    /// Free-standing sub `name` in `package`. `package: None` matches
    /// top-level/script subs (symbols without an enclosing package).
    Sub {
        #[serde(default)]
        package: Option<String>,
        name: String,
    },
}

pub trait FrameworkPlugin: Send + Sync {
    fn id(&self) -> &str;
    fn triggers(&self) -> &[Trigger];

    /// Static type-override manifest. Read once at plugin load and
    /// applied at the end of every build — see `TypeOverride`. Default
    /// is no overrides; only plugins that need them implement this.
    fn overrides(&self) -> &[TypeOverride] {
        &[]
    }

    /// Static dispatch-verb manifest. Read once at plugin load and applied
    /// in the enrichment pass (cross-file receiver isa resolution) — see
    /// `DispatchVerb`. Default empty; only dispatch-style plugins declare.
    fn dispatch_verbs(&self) -> &[DispatchVerb] {
        &[]
    }

    /// Module-loading verbs — see `LoadVerb`. Default empty.
    fn load_verbs(&self) -> &[LoadVerb] {
        &[]
    }

    /// Call verbs (method or function names) whose args this plugin wants
    /// pre-flattened into `CallContext::arg_names` (and, for `has`/`option`,
    /// `has_options` resolved). Core runs the `cst::string_list` extraction
    /// only for verbs an applicable plugin registered, so no DSL verb is
    /// hardcoded in core (rule #10). Default empty. See `frameworks/dbic.rhai`.
    fn arg_name_verbs(&self) -> &[String] {
        &[]
    }

    /// Static role-contract parameter-type manifest — see `ParamType`.
    /// Applied at the sub-declaration walk. Default empty.
    fn param_types(&self) -> &[ParamType] {
        &[]
    }

    /// Names this plugin treats as type-constraint constructors
    /// (`InstanceOf`, `ConsumerOf`, …). A cheap dispatch gate: the builder
    /// only folds a call into a `TypeConstraintOf` when its name is in the
    /// union of these across plugins. (First cut — a global gate; later
    /// this moves to the import seam so it's package-scoped.)
    fn type_constraint_names(&self) -> &[String] {
        &[]
    }

    /// Receiver classes that compose the fictional app surface
    /// (`file_analysis::APP_SURFACE_CLASS`) — the "open consumption" axis
    /// of the helper/plugin model (see `docs/adr/plugin-system.md`). Each
    /// listed class gains the surface as a synthetic ancestor in the MRO
    /// walk, so entities a plugin bridges to the surface resolve from
    /// every consumer (`$app->h`, `$c->h`, app subclasses) through the
    /// existing ancestor + bridge resolution — one bridge target, an open
    /// consumer set declared once. Default empty.
    fn app_surface_consumers(&self) -> &[String] {
        &[]
    }

    /// Modules whose `use` turns the consuming package into a ROLE
    /// (the plugin-declared extension of core's base set: Moo::Role /
    /// Moose::Role / Mouse::Role / Role::Tiny). For role engines that
    /// aren't Moo-shaped — where emitting `SyntheticUse "Moo::Role"`
    /// would be a lie about the keyword surface — a plugin declares the
    /// engine module here and consumers are marked roles directly.
    /// Default empty.
    fn role_makers(&self) -> &[String] {
        &[]
    }

    /// Method verbs whose FIRST hashref arg is keyed by the receiver class's
    /// COLUMN names (DBIC `search(\%cond, …)` / `create(\%cols)` etc.). Core
    /// links those keys to the class's columns for rename/references and walks
    /// only the first hashref (the trailing `\%attrs` hash isn't column-keyed).
    /// The plugin owns its framework's verb list; core stays generic. Default
    /// empty.
    fn column_keyed_verbs(&self) -> &[String] {
        &[]
    }

    /// A topic-scoped route DSL this plugin owns (the
    /// Mojolicious::Lite shape): file-level route verbs whose implicit
    /// base a [`EmitAction::SetRouteBase`] emission sets and a scope
    /// function brackets. Core provides only the generic stack — every
    /// NAME in the mechanism (the gating module, the verb set, the
    /// scope function) is declared here, never hardcoded in core.
    fn topic_route_dsl(&self) -> Option<TopicRouteDsl> {
        None
    }

    /// Fold a constraint constructor's extracted params into the
    /// *constrained inner* type (the type a satisfying value has). The
    /// builder wraps the result in `TypeConstraintOf`. Return `None` to
    /// decline (`name` not ours, or unfoldable params). Arity lives here,
    /// not in the core — see `ConstraintParam`.
    #[allow(unused_variables)]
    fn type_constraint_inner(
        &self,
        name: &str,
        params: &[ConstraintParam],
    ) -> Option<InferredType> {
        None
    }

    // ---- Emit hooks (parse time) ----
    //
    // Plugin observes a CST event and returns facts to push into the
    // symbol/ref/namespace tables. Declarative.

    #[allow(unused_variables)]
    fn on_use(&self, ctx: &UseContext) -> Vec<EmitAction> {
        Vec::new()
    }
    #[allow(unused_variables)]
    fn on_function_call(&self, ctx: &CallContext) -> Vec<EmitAction> {
        Vec::new()
    }
    #[allow(unused_variables)]
    fn on_method_call(&self, ctx: &CallContext) -> Vec<EmitAction> {
        Vec::new()
    }

    // ---- Query hooks (cursor time) ----
    //
    // Plugin inspects the cursor context and answers "do I have
    // something for this spot?". Imperative. Core asks every
    // applicable plugin after the native pipeline runs.

    /// Offer a signature at the cursor or claim the slot silently.
    /// Called BEFORE native sig help so the plugin can both provide
    /// its own shape AND suppress native when the plugin knows the
    /// cursor sits at a position native would mishandle.
    ///   * `None`               — plugin doesn't claim this cursor
    ///   * `Some(Show(sig))`    — show this signature
    ///   * `Some(Silent)`       — suppress native; show nothing
    #[allow(unused_variables)]
    fn on_signature_help(&self, ctx: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> {
        None
    }

    /// Contribute completion items and optionally assert authority
    /// over this cursor. Plugins return a `PluginCompletionAnswer`
    /// with a list of candidates and a flag saying whether to
    /// suppress the core's other completion sources at this spot
    /// (used when the plugin knows the cursor's a name slot for its
    /// dispatch, not a general method slot).
    #[allow(unused_variables)]
    fn on_completion(&self, ctx: &CompletionQueryContext) -> Option<PluginCompletionAnswer> {
        None
    }
}

// ---- Query-hook types ----

/// What the core tells a plugin when asking `on_signature_help`.
/// `on_completion` uses the same shape with `PluginCompletionAnswer`
/// for the return value.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigHelpQueryContext {
    /// Innermost enclosing call at the cursor, if any.
    pub call: Option<CallFrame>,
    /// Nested container (array/hash literal) the cursor sits inside,
    /// when the cursor is past the top-level call args — e.g. cursor
    /// inside `[...]` at call-arg-1 of an enqueue dispatch. Lets a
    /// plugin recognize "the cursor is inside my handler-args slot
    /// wrapped in an arrayref" without the core knowing anything
    /// about arrayref-wrapped dispatch.
    pub cursor_inside: Option<ContainerFrame>,
    /// `ctx.current_package` at the cursor — helps plugins pick the
    /// right app/instance namespace when more than one exists.
    pub current_package: Option<String>,
    /// When the cursor sits inside the args of `use M ...`, the
    /// module name `M`. Plugins use this to claim use-line option
    /// completion (`use DDP { caller_info => 1 }`) without the core
    /// hard-coding any specific module's option list. Pairs with
    /// `cursor_inside` — a `Hash`-kind container plus this field
    /// gives a plugin everything it needs to return option keys.
    /// `None` when the cursor isn't inside a use statement's args.
    #[serde(default)]
    pub current_use_module: Option<String>,
}

pub type CompletionQueryContext = SigHelpQueryContext;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallFrame {
    pub is_method: bool,
    pub name: String,
    pub receiver_text: Option<String>,
    pub receiver_type: Option<InferredType>,
    pub args: Vec<ArgInfo>,
    /// Zero-indexed top-level arg slot the cursor is at (commas at
    /// the call's own arg list, not nested containers).
    pub cursor_arg_index: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerFrame {
    pub kind: ContainerKind,
    /// The cursor's slot within THIS container (comma count INSIDE
    /// the container before the cursor — `[a, b, ^]` = slot 2).
    pub active_slot: usize,
    /// For Hash containers: keys already written before the cursor.
    /// Empty for Array.
    pub existing_keys: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ContainerKind {
    Array,
    Hash,
}

/// A minimal signature-help payload that plugins can construct
/// ergonomically. The core converts to full LSP `SignatureHelp`
/// (filling in active_signature etc.).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginSignatureHelp {
    pub label: String,
    pub params: Vec<String>,
    pub active_param: usize,
}

/// Plugin's answer to an `on_signature_help` query. Either show a
/// sig or silently claim the slot (suppresses native sig help).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PluginSigHelpAnswer {
    /// Plugin-rendered signature — used when the shape is entirely
    /// synthetic (the task wrapper form, say) and no existing Handler
    /// backs it.
    Show(PluginSignatureHelp),
    /// Claim the slot silently — suppresses native sig help without
    /// contributing anything. Used when the cursor sits in a position
    /// where the native path would mis-fire (e.g. the options hash of
    /// `enqueue` — native would show the task's sig instead of enqueue's
    /// own options).
    Silent,
    /// Delegate to the core: "render the sig for this Handler". The
    /// plugin has already resolved which Handler the cursor points at
    /// and which param slot is active; the core does the param lookup
    /// (including cross-file via `handlers_for_owner`) and invocant
    /// stripping. Same delegation pattern as completion's
    /// `dispatch_targets_for`.
    ShowHandler {
        owner_class: String,
        dispatcher: String,
        handler_name: String,
        /// Active-param index in the DISPLAYED signature (post invocant
        /// strip). The core maps it through `saturating_sub(1)` if it
        /// needs the raw Handler-params index.
        active_param: usize,
    },
    /// Cursor is inside a call the plugin recognizes, but not in the
    /// slot it claims (e.g. outside Minion `enqueue`'s arrayref, which
    /// is where the task's args actually live). Core skips the
    /// string-dispatch fallback — which would otherwise show the
    /// task's sig indexed by the OUTER call's comma count, picking a
    /// random task param — and falls through to the method's OWN sig.
    ShowCallSig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginCompletionAnswer {
    pub items: Vec<PluginCompletion>,
    /// When true, core suppresses ALL native completion sources at
    /// this cursor — the plugin has claimed the slot. Use sparingly:
    /// only when the plugin knows the cursor's a dedicated dispatch
    /// name slot (e.g. enqueue arg-0) where method-of-receiver is
    /// nonsensical.
    #[serde(default)]
    pub exclusive: bool,
    /// Optional request to the core: "also populate with every
    /// handler name registered on this owner class via any of these
    /// dispatcher methods". Avoids forcing every plugin to re-walk
    /// the symbol table for a dispatcher-name completion slot. Core
    /// materializes Handler symbols whose owner matches and whose
    /// dispatchers list includes any of the named methods.
    #[serde(default)]
    pub dispatch_targets_for: Option<DispatchTargetRequest>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DispatchTargetRequest {
    pub owner_class: String,
    pub dispatcher_names: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginCompletion {
    pub label: String,
    /// Plugin-chosen LSP-adjacent kind. Core maps to CompletionItemKind.
    pub kind: CompletionKindHint,
    #[serde(default)]
    pub detail: Option<String>,
    #[serde(default)]
    pub insert_text: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompletionKindHint {
    Function,
    Method,
    Field,
    Property,
    Value,
    Event,
    Operator,
    Keyword,
    Task,
    Helper,
    Route,
}

// ---- Registry ----

#[derive(Default)]
pub struct PluginRegistry {
    plugins: Vec<Box<dyn FrameworkPlugin>>,
}

impl std::fmt::Debug for PluginRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PluginRegistry")
            .field("count", &self.plugins.len())
            .field("ids", &self.plugins.iter().map(|p| p.id()).collect::<Vec<_>>())
            .finish()
    }
}

impl PluginRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, plugin: Box<dyn FrameworkPlugin>) {
        self.plugins.push(plugin);
    }

    pub fn is_empty(&self) -> bool {
        self.plugins.is_empty()
    }

    /// Every registered plugin, unfiltered. Used for hooks that
    /// can't rely on the trigger filter — `on_use` in particular,
    /// because the `UsesModule(X)` trigger is false UNTIL `use X`
    /// has been processed, so filtering would prevent the plugin
    /// from ever hooking the statement that introduces the trigger.
    /// Plugins that use `on_use` should filter on `ctx.module_name`.
    pub fn all(&self) -> impl Iterator<Item = &dyn FrameworkPlugin> {
        self.plugins.iter().map(|p| p.as_ref())
    }

    /// Yield every (plugin_id, override) pair across the registry.
    /// Trigger-independent: the builder applies overrides whenever a
    /// local symbol matches their target, regardless of which packages
    /// the file uses. Cheap to call — overrides are static `&[..]`
    /// borrows from each plugin.
    pub fn overrides<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a TypeOverride)> + 'a {
        self.plugins
            .iter()
            .flat_map(|p| p.overrides().iter().map(move |o| (p.id(), o)))
    }

    /// Yield every dispatch-verb declaration across the registry.
    /// Trigger-independent, same rationale as `overrides`.
    pub fn dispatch_verbs<'a>(&'a self) -> impl Iterator<Item = &'a DispatchVerb> + 'a {
        self.plugins.iter().flat_map(|p| p.dispatch_verbs().iter())
    }

    pub fn load_verbs<'a>(&'a self) -> impl Iterator<Item = &'a LoadVerb> + 'a {
        self.plugins.iter().flat_map(|p| p.load_verbs().iter())
    }

    /// Yield every role-contract parameter-type rule across the registry.
    pub fn param_types<'a>(&'a self) -> impl Iterator<Item = &'a ParamType> + 'a {
        self.plugins.iter().flat_map(|p| p.param_types().iter())
    }

    /// Union of constraint-constructor names across the registry — the
    /// builder's dispatch gate for which calls to fold into a constraint.
    pub fn type_constraint_names<'a>(&'a self) -> impl Iterator<Item = &'a str> + 'a {
        self.plugins
            .iter()
            .flat_map(|p| p.type_constraint_names().iter().map(|s| s.as_str()))
    }

    /// Union of app-surface consumer classes across the registry — the
    /// declared receiver set that composes the app surface (one place,
    /// open). The builder bakes this onto `FileAnalysis` so the
    /// query-time ancestor walk can inject the synthetic-parent edge.
    pub fn app_surface_consumers<'a>(&'a self) -> impl Iterator<Item = &'a str> + 'a {
        self.plugins
            .iter()
            .flat_map(|p| p.app_surface_consumers().iter().map(|s| s.as_str()))
    }

    /// Union of role-maker modules across the registry — the open
    /// extension of the builder's base role-engine set.
    pub fn role_makers<'a>(&'a self) -> impl Iterator<Item = &'a str> + 'a {
        self.plugins
            .iter()
            .flat_map(|p| p.role_makers().iter().map(|s| s.as_str()))
    }

    /// Union of column-keyed call verbs across the registry — the verbs whose
    /// first hashref arg is keyed by the receiver class's columns.
    pub fn column_keyed_verbs<'a>(&'a self) -> impl Iterator<Item = &'a str> + 'a {
        self.plugins
            .iter()
            .flat_map(|p| p.column_keyed_verbs().iter().map(|s| s.as_str()))
    }

    /// Fold a constraint constructor → its inner type, asking the plugin(s)
    /// that claim `name`. First non-`None` wins.
    pub fn type_constraint_inner(
        &self,
        name: &str,
        params: &[ConstraintParam],
    ) -> Option<InferredType> {
        self.plugins.iter().find_map(|p| {
            if p.type_constraint_names().iter().any(|n| n == name) {
                p.type_constraint_inner(name, params)
            } else {
                None
            }
        })
    }

    /// Does an applicable plugin want `verb`'s args pre-flattened? Gates
    /// the builder's `cst::string_list` extraction to registered verbs in
    /// packages where the declaring plugin actually fires.
    pub fn wants_arg_names(&self, query: &TriggerQuery<'_>, verb: &str) -> bool {
        self.applicable(query)
            .any(|p| p.arg_name_verbs().iter().any(|v| v == verb))
    }

    /// Return plugins whose triggers match the current package context.
    pub fn applicable<'a>(
        &'a self,
        query: &TriggerQuery<'_>,
    ) -> impl Iterator<Item = &'a dyn FrameworkPlugin> + 'a {
        let uses = query.package_uses.to_vec();
        let parents = query.package_parents.to_vec();
        self.plugins.iter().filter_map(move |p| {
            let fires = p.triggers().iter().any(|t| match t {
                Trigger::Always => true,
                Trigger::UsesModule(m) => uses.iter().any(|u| u == m),
                Trigger::ClassIsa(prefix) => parents.iter().any(|parent| {
                    parent == prefix
                        || parent.starts_with(&format!("{}::", prefix))
                }),
            });
            if fires {
                Some(p.as_ref())
            } else {
                None
            }
        })
    }
}

/// Thin view over per-package state used for trigger matching.
pub struct TriggerQuery<'a> {
    pub package_uses: &'a [String],
    pub package_parents: &'a [String],
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Point;

    fn span(r1: usize, c1: usize, r2: usize, c2: usize) -> Span {
        Span {
            start: Point::new(r1, c1),
            end: Point::new(r2, c2),
        }
    }

    struct StubPlugin {
        id: &'static str,
        triggers: Vec<Trigger>,
        sig_answer: Option<PluginSigHelpAnswer>,
        completion_answer: Option<PluginCompletionAnswer>,
    }

    impl FrameworkPlugin for StubPlugin {
        fn id(&self) -> &str { self.id }
        fn triggers(&self) -> &[Trigger] { &self.triggers }
        fn on_signature_help(&self, _ctx: &SigHelpQueryContext) -> Option<PluginSigHelpAnswer> {
            self.sig_answer.clone()
        }
        fn on_completion(&self, _ctx: &CompletionQueryContext) -> Option<PluginCompletionAnswer> {
            self.completion_answer.clone()
        }
    }

    fn stub(id: &'static str) -> StubPlugin {
        StubPlugin { id, triggers: vec![Trigger::Always], sig_answer: None, completion_answer: None }
    }

    fn empty_qctx() -> SigHelpQueryContext {
        SigHelpQueryContext {
            call: None,
            cursor_inside: None,
            current_package: None,
            current_use_module: None,
        }
    }

    #[test]
    fn trigger_uses_module_matches_only_when_used() {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(StubPlugin {
            id: "mojo-base",
            triggers: vec![Trigger::UsesModule("Mojo::Base".into())],
            sig_answer: None,
            completion_answer: None,
        }));

        let uses_mojo: Vec<String> = vec!["Mojo::Base".into()];
        let uses_none: Vec<String> = vec![];
        let parents: Vec<String> = vec![];

        let m = reg.applicable(&TriggerQuery {
            package_uses: &uses_mojo,
            package_parents: &parents,
        }).count();
        assert_eq!(m, 1);

        let m = reg.applicable(&TriggerQuery {
            package_uses: &uses_none,
            package_parents: &parents,
        }).count();
        assert_eq!(m, 0);
    }

    #[test]
    fn trigger_class_isa_prefix_matches_descendants() {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(StubPlugin {
            id: "dbic",
            triggers: vec![Trigger::ClassIsa("DBIx::Class".into())],
            sig_answer: None,
            completion_answer: None,
        }));

        let uses: Vec<String> = vec![];
        let parents = vec!["DBIx::Class::Core".to_string()];

        let m = reg.applicable(&TriggerQuery {
            package_uses: &uses,
            package_parents: &parents,
        }).count();
        assert_eq!(m, 1, "DBIx::Class::Core should match prefix DBIx::Class");

        let other = vec!["Something::Else".to_string()];
        let m = reg.applicable(&TriggerQuery {
            package_uses: &uses,
            package_parents: &other,
        }).count();
        assert_eq!(m, 0);
    }

    #[test]
    fn emitted_param_converts_to_param_info() {
        let ep = EmittedParam {
            name: "$val".into(),
            default: None,
            is_slurpy: false,
            is_invocant: false,
        };
        let pi: ParamInfo = ep.into();
        assert_eq!(pi.name, "$val");
    }

    #[test]
    fn emit_action_serializable() {
        let action = EmitAction::Method {
            name: "foo".into(),
            span: span(0, 0, 0, 3),
            selection_span: span(0, 0, 0, 3),
            params: vec![],
            is_method: true,
            return_type: None,
            doc: None,
            on_class: None,
            display: None,
            hide_in_outline: false,
            opaque_return: false,
            outline_label: None,
            return_via_edge: None,
            attr: None,
        };
        let json = serde_json::to_string(&action).unwrap();
        assert!(json.contains("\"Method\""));
        assert!(json.contains("\"foo\""));
    }

    // ---- IoC query hook semantics ----
    //
    // The tests below pin the contract that the symbols.rs wiring
    // relies on: iteration order is registration order, first
    // answer wins at a per-plugin level, and `Silent` / `exclusive`
    // are honored without bundled-plugin coupling.

    #[test]
    fn iteration_order_is_registration_order() {
        // If two plugins both claim a slot, the first-registered one
        // wins. symbols.rs uses `applicable()` which returns an iter
        // over `self.plugins` in registration order, so the "who
        // wins" question collapses to "who registered first".
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(stub("first")));
        reg.register(Box::new(stub("second")));

        let uses: Vec<String> = vec![];
        let parents: Vec<String> = vec![];
        let ids: Vec<&str> = reg.applicable(&TriggerQuery {
            package_uses: &uses, package_parents: &parents,
        }).map(|p| p.id()).collect();
        assert_eq!(ids, vec!["first", "second"],
            "applicable() preserves registration order — symbols.rs relies on \
             this for deterministic first-match-wins behavior");
    }

    #[test]
    fn sig_help_silent_variant_is_distinct_from_show() {
        // The `Silent` variant must carry no payload; symbols.rs
        // branches on it directly. Round-tripping through serde
        // catches any future refactor that accidentally flattens it.
        let silent = PluginSigHelpAnswer::Silent;
        let json = serde_json::to_string(&silent).unwrap();
        let back: PluginSigHelpAnswer = serde_json::from_str(&json).unwrap();
        assert!(matches!(back, PluginSigHelpAnswer::Silent));

        // Show carries a signature — distinct JSON shape.
        let show = PluginSigHelpAnswer::Show(PluginSignatureHelp {
            label: "x".into(), params: vec![], active_param: 0,
        });
        let show_json = serde_json::to_string(&show).unwrap();
        assert!(show_json.contains("Show"));
        assert!(!show_json.contains("\"Silent\""));
    }

    #[test]
    fn completion_exclusive_and_dispatch_targets_coexist() {
        // `exclusive` and `dispatch_targets_for` are orthogonal: a
        // plugin can claim the slot AND delegate handler-name
        // population to the core in the same answer. symbols.rs
        // returns early on `exclusive` but still walks
        // `dispatch_targets_for` first.
        let ans = PluginCompletionAnswer {
            items: vec![],
            exclusive: true,
            dispatch_targets_for: Some(DispatchTargetRequest {
                owner_class: "Minion".into(),
                dispatcher_names: vec!["enqueue".into()],
            }),
        };
        assert!(ans.exclusive);
        assert!(ans.dispatch_targets_for.is_some());
        // Round-trip — both fields must survive.
        let json = serde_json::to_string(&ans).unwrap();
        let back: PluginCompletionAnswer = serde_json::from_str(&json).unwrap();
        assert!(back.exclusive);
        assert_eq!(
            back.dispatch_targets_for.unwrap().owner_class,
            "Minion"
        );
    }

    #[test]
    fn first_answering_plugin_claims_sig_help() {
        // Mirror of how symbols.rs iterates: break on first Some.
        // Second plugin never runs. This documents the contract so a
        // future refactor that wants "all plugins contribute" has to
        // explicitly change this test.
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(StubPlugin {
            id: "winner",
            triggers: vec![Trigger::Always],
            sig_answer: Some(PluginSigHelpAnswer::Silent),
            completion_answer: None,
        }));
        reg.register(Box::new(StubPlugin {
            id: "loser",
            triggers: vec![Trigger::Always],
            sig_answer: Some(PluginSigHelpAnswer::Show(PluginSignatureHelp {
                label: "never-seen".into(), params: vec![], active_param: 0,
            })),
            completion_answer: None,
        }));

        let uses: Vec<String> = vec![];
        let parents: Vec<String> = vec![];
        let qctx = empty_qctx();
        let mut answers = Vec::new();
        for p in reg.applicable(&TriggerQuery {
            package_uses: &uses, package_parents: &parents,
        }) {
            if let Some(a) = p.on_signature_help(&qctx) {
                answers.push((p.id().to_string(), a));
                break; // symbols.rs breaks on first Some — tested here
            }
        }
        assert_eq!(answers.len(), 1);
        assert_eq!(answers[0].0, "winner");
        assert!(matches!(answers[0].1, PluginSigHelpAnswer::Silent));
    }
}
