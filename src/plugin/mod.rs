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
    AccessKind, HandlerDisplay, HandlerOwner, HashKeyOwner, InferredType, ParamInfo, Span,
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
}

impl From<EmittedParam> for ParamInfo {
    fn from(p: EmittedParam) -> Self {
        ParamInfo {
            name: p.name,
            default: p.default,
            is_slurpy: p.is_slurpy,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EmitAction {
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
    /// Implicit keyword import (same purpose as builder's `framework_imports`).
    FrameworkImport { keyword: String },
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

    pub fn len(&self) -> usize {
        self.plugins.len()
    }

    pub fn is_empty(&self) -> bool {
        self.plugins.is_empty()
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
    }

    impl FrameworkPlugin for StubPlugin {
        fn id(&self) -> &str { self.id }
        fn triggers(&self) -> &[Trigger] { &self.triggers }
    }

    #[test]
    fn trigger_uses_module_matches_only_when_used() {
        let mut reg = PluginRegistry::new();
        reg.register(Box::new(StubPlugin {
            id: "mojo-base",
            triggers: vec![Trigger::UsesModule("Mojo::Base".into())],
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
        };
        let json = serde_json::to_string(&action).unwrap();
        assert!(json.contains("\"Method\""));
        assert!(json.contains("\"foo\""));
    }
}
