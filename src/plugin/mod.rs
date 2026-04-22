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
    pub string_value: Option<String>,
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
    /// If this arg is an anonymous sub (`sub ($a, $b) { ... }` or a block
    /// that begins with `my ($a, $b) = @_`), its extracted param list.
    /// Used by handler-registration plugins (Mojo events, Dancer routes,
    /// etc.) to capture the handler's signature and store it on the
    /// HashKeyDef so signature help can surface it at call sites.
    #[serde(default)]
    pub sub_params: Vec<EmittedParam>,
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
    /// Resolved receiver type if inference succeeded.
    pub receiver_type: Option<InferredType>,
    pub args: Vec<ArgInfo>,
    pub call_span: Span,
    pub selection_span: Span,
    pub current_package: Option<String>,
    pub current_package_parents: Vec<String>,
    pub current_package_uses: Vec<String>,
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EmitAction {
    // ==== Data emissions ====

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
    Symbol {
        name: String,
        kind: SymKind,
        span: Span,
        selection_span: Span,
        detail: SymbolDetail,
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

pub trait FrameworkPlugin: Send + Sync {
    fn id(&self) -> &str;
    fn triggers(&self) -> &[Trigger];

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
        SigHelpQueryContext { call: None, cursor_inside: None, current_package: None }
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
