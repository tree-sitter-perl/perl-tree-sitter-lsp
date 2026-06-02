//! FileAnalysis: single-pass scope graph for Perl source files.
//!
//! Built once per parse/reparse via `builder::build()`. Every LSP query
//! becomes a lookup against these tables instead of a tree walk.
//!
//! Designed to compose into a project index: `HashMap<PathBuf, FileAnalysis>`.

use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use tree_sitter::Point;

use crate::module_index::ModuleIndex;

// ---- Serde proxy for tree_sitter::Point ----

/// Remote-derive proxy for `tree_sitter::Point`, which doesn't implement serde.
/// Fields mirror `Point` exactly — use `#[serde(with = "PointDef")]` on Point fields.
#[derive(Serialize, Deserialize)]
#[serde(remote = "Point")]
pub(crate) struct PointDef {
    pub row: usize,
    pub column: usize,
}

/// Helper module for `Option<Point>` serialization via the remote-derived proxy.
pub(crate) mod point_opt_serde {
    use super::PointDef;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use tree_sitter::Point;

    pub fn serialize<S: Serializer>(val: &Option<Point>, s: S) -> Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct W<'a>(#[serde(with = "PointDef")] &'a Point);
        val.as_ref().map(W).serialize(s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Option<Point>, D::Error> {
        #[derive(Deserialize)]
        struct W(#[serde(with = "PointDef")] Point);
        Option::<W>::deserialize(d).map(|o| o.map(|W(p)| p))
    }
}

// ---- Shared types ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    #[serde(with = "PointDef")]
    pub start: Point,
    #[serde(with = "PointDef")]
    pub end: Point,
}

/// Extract the function/method name from a call expression node.
pub(crate) fn extract_call_name(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "method_call_expression" => node
            .child_by_field_name("method")
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| s.to_string()),
        "function_call_expression" | "ambiguous_function_call_expression" => node
            .child_by_field_name("function")
            .and_then(|n| n.utf8_text(source).ok())
            .map(|s| s.to_string()),
        _ => None,
    }
}

pub(crate) fn node_to_span(node: tree_sitter::Node) -> Span {
    Span {
        start: node.start_position(),
        end: node.end_position(),
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FoldRange {
    pub start_line: usize,
    pub end_line: usize,
    pub kind: FoldKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum FoldKind {
    Region,
    Comment,
}

// ---- IDs ----

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ScopeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SymbolId(pub u32);

// ---- Scope ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub span: Span,
    /// The enclosing package/class name at this scope level.
    /// For `package Foo;` regions, this is "Foo".
    /// Inherited from parent when not overridden.
    pub package: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum ScopeKind {
    File,
    /// `class Foo { ... }` block.
    Class { name: String },
    /// `sub foo { ... }` block.
    Sub { name: String },
    /// `method foo { ... }` block.
    Method { name: String },
    /// Bare `{ ... }`, if/while/for bodies, etc.
    Block,
    /// `for my $x (...) { }` — loop variable scoped to block.
    ForLoop { var: String },
}

// ---- Package context ----

/// Flat per-file record of which `package`/`class` declaration governs a
/// byte range. Independent of the lexical scope tree — `package Foo;` is
/// not a lexical boundary in Perl, so collapsing the two concepts would
/// force shims (lift `my` past the package "scope", merge buckets in the
/// outline) we'd rather not have.
///
/// Query via `FileAnalysis::package_at(point)` — innermost (latest-starting)
/// containing range wins for nested `package Foo { … package Bar; … }`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PackageRange {
    pub package: String,
    pub span: Span,
    pub kind: PackageKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PackageKind {
    /// `package Foo;` / `class Foo;` — flows until the next sibling
    /// declaration or end of file/block.
    Statement,
    /// `package Foo { … }` / `class Foo { … }` — span equals the block.
    Block,
}

// ---- Namespace ----

/// Origin tag for symbols and refs: native Perl vs produced by a framework
/// rule (built-in or plugin). Downstream features (completion bucketing,
/// diagnostic suppression, plugin-aware rename) read this tag instead of
/// reconstructing provenance from names and positions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Namespace {
    /// Native Perl: subs, variables, packages, classes, hash keys extracted
    /// directly from the CST by the builder.
    Language,
    /// Produced by a framework plugin. `id` is the plugin identifier
    /// (e.g. `"mojo-base"`, `"moo"`, `"dbic-columns"`), allowing per-plugin
    /// filtering, rename coordination, and diagnostic attribution.
    Framework { id: String },
}

impl Default for Namespace {
    fn default() -> Self { Self::Language }
}

impl Namespace {
    pub fn framework(id: impl Into<String>) -> Self {
        Self::Framework { id: id.into() }
    }

    pub fn is_framework(&self) -> bool {
        matches!(self, Self::Framework { .. })
    }
}

// ---- Symbol ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymKind,
    pub span: Span,
    pub selection_span: Span,
    /// Scope this symbol is declared in.
    pub scope: ScopeId,
    /// The package this symbol belongs to (captured at creation time).
    pub package: Option<String>,
    /// Kind-specific extra data.
    pub detail: SymbolDetail,
    /// Provenance tag. Defaults to `Language` for builder-native symbols;
    /// framework plugins stamp their plugin id.
    #[serde(default)]
    pub namespace: Namespace,
    /// Optional outline-only display name override. When set, the document
    /// outline uses this verbatim instead of `name` (and drops any
    /// kind-specific prefix like "helper"/"method"). Plugin-controlled:
    /// a chained Mojo helper leaf has `name: "create"` (so method
    /// resolution works on `$c->users->create`) but needs
    /// `outline_label: "users.create"` so the outline reflects the
    /// declared helper path. A mojo-lite route uses it to prepend the
    /// HTTP verb to the path. Doesn't affect resolution, rename, or
    /// workspace-symbol.
    #[serde(default)]
    pub outline_label: Option<String>,
}

impl Symbol {
    /// Bare variable/field name without the sigil. Uses the sigil stored
    /// in `detail` so we never re-derive it by text-stripping (which would
    /// mis-handle forms like `$$ref` if the name ever carried that shape).
    /// For non-variable symbols, returns `name` unchanged.
    pub fn bare_name(&self) -> &str {
        match &self.detail {
            SymbolDetail::Variable { sigil, .. } | SymbolDetail::Field { sigil, .. } => {
                let off = sigil.len_utf8();
                self.name.get(off..).unwrap_or(&self.name)
            }
            _ => &self.name,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SymKind {
    Variable,
    Sub,
    Method,
    Package,
    Class,
    Module,
    Field,
    HashKeyDef,
    /// Named handler registered on a class via string-dispatch (e.g. Mojo
    /// events, Dancer routes, Catalyst actions). Not a Perl method — it
    /// can't be called as `$self->name()`. It's dispatched through named
    /// methods (`->emit`, `->get`, `->forward`) whose first string arg
    /// selects which Handler to run. Multiple Handlers with the same
    /// `(owner, name)` stack, they don't override.
    Handler,
    /// Plugin-controlled scope (mojo app, Minion instance, mojo-events
    /// emitter). Surfaces in the document outline and workspace symbol
    /// search as a navigable entry. Outline entity — not backed by a
    /// Perl symbol, so most queries (gd/gr/rename) don't hit it.
    Namespace,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum SymbolDetail {
    Variable {
        sigil: char,
        decl_kind: DeclKind,
    },
    Sub {
        params: Vec<ParamInfo>,
        is_method: bool,
        /// Pre-rendered markdown from POD or comments preceding this sub.
        doc: Option<String>,
        /// Optional plugin-provided display override. Framework-synthesized
        /// methods (Mojo helpers, Dancer routes, DBIC relationships, etc.)
        /// resolve/complete/goto-def the same as a regular Method, but the
        /// plugin gets the final word on which LSP kind they render as.
        /// `None` leaves the default (Method → METHOD, Sub → FUNCTION).
        #[serde(default)]
        display: Option<HandlerDisplay>,
        /// Suppress this symbol in the document outline / workspace
        /// symbol list. Plugins synthesizing DSL imports (Mojolicious::Lite's
        /// `get`/`post`/`app`/...) set it so hover/gd/completion still work
        /// on the name, but the outline stays focused on user-visible
        /// structure. Whoever constructs the detail decides — the core
        /// never infers.
        #[serde(default)]
        hide_in_outline: bool,
        /// The return type is plugin-internal plumbing — use it for
        /// chain resolution but don't render it in completion details,
        /// hover return-type lines, or inlay hints. Lets framework
        /// plugins thread proxy classes (Mojo helper namespaces, DBIC
        /// result wrappers, etc.) without leaking "returns:
        /// _Helper::users::create" at every call site. Plugin-declared,
        /// no core heuristic on the type name.
        #[serde(default)]
        opaque_return: bool,
    },
    Class {
        parent: Option<String>,
        roles: Vec<String>,
        fields: Vec<FieldDetail>,
    },
    Field {
        sigil: char,
        attributes: Vec<String>,
    },
    HashKeyDef {
        owner: HashKeyOwner,
        is_dynamic: bool,
    },
    /// String-dispatched handler detail. `owner` ties the handler to a
    /// class (so two classes can each register a handler named "ready"
    /// without collision). `dispatchers` is the set of method names that
    /// select this handler by string — e.g. `["emit", "subscribe"]` for
    /// Mojo events, `["forward"]` for Catalyst actions. `params` is the
    /// handler's sub signature, consumed by signature help at call
    /// sites and by hover to describe the handler shape.
    ///
    /// `display` is the plugin's choice of LSP kind for outline,
    /// completion icon, and workspace-symbol presentation. Handlers
    /// share internal machinery (refs_by_target, stacking semantics,
    /// cross-file resolution) but aren't all "events" — Mojo events
    /// are, but routes are methods, config keys are fields, etc.
    /// The plugin says what the user sees; the abstraction stays one
    /// thing internally.
    Handler {
        owner: HandlerOwner,
        dispatchers: Vec<String>,
        params: Vec<ParamInfo>,
        #[serde(default)]
        display: HandlerDisplay,
        /// Suppress this handler in the document outline. Plugins set
        /// it for framework-synthesized entries that shouldn't clutter
        /// the user's navigation view — hover/gd/completion still find
        /// them via the symbol table.
        #[serde(default)]
        hide_in_outline: bool,
    },
    /// Package, Module, or other kinds needing no extra data.
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DeclKind {
    My,
    Our,
    State,
    Field,
    Param,
    ForVar,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamInfo {
    pub name: String,
    pub default: Option<String>,
    pub is_slurpy: bool,
    /// True when this param is the implicit receiver of the call,
    /// supplied by the caller via `$obj->method(...)` rather than
    /// written in the argument list. Sig help, hover, and outline
    /// drop invocant params so users see what they actually type.
    /// Whoever constructs the ParamInfo is responsible for setting
    /// this — the core never infers it from the name.
    #[serde(default)]
    pub is_invocant: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub struct FieldDetail {
    pub name: String,
    pub sigil: char,
    pub attributes: Vec<String>,
}

// ---- Ref ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ref {
    pub kind: RefKind,
    pub span: Span,
    pub scope: ScopeId,
    pub target_name: String,
    pub access: AccessKind,
    /// For variable refs: which Symbol this resolves to (filled in post-pass).
    pub resolves_to: Option<SymbolId>,
}

/// What kind of entity is being renamed — determines single-file vs cross-file scope.
#[derive(Debug)]
pub enum RenameKind {
    Variable,
    /// A sub defined in (or imported from) a specific package.
    /// `package == None` means a top-level/script sub with no
    /// package context; otherwise package-scoped so cross-file
    /// walks don't rename same-named subs in unrelated packages.
    Function { name: String, package: Option<String> },
    Package(String),
    /// A method with its owning class. Cross-file walks use `class`
    /// to avoid unioning unrelated classes that share a method name
    /// (e.g. `Foo::run` vs `Bar::run`, mojo-helper leaves vs route
    /// targets).
    Method { name: String, class: String },
    HashKey(String),
    /// Rename a `Handler` by (owner, name) — touches the handler symbol's
    /// name + every `DispatchCall` ref targeting it.
    Handler { owner: HandlerOwner, name: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum RefKind {
    Variable,
    /// Bare `foo()` or `Pkg::foo()` call. `resolved_package` is the
    /// package whose `sub` this call actually targets — computed at
    /// build time by walking the enclosing-package-then-imports graph
    /// (see `Builder::resolve_call_package`). `None` means no pin (the
    /// call site has no package context and no import covers it);
    /// class/package-scoped queries treat unpinned refs as no-match
    /// rather than cross-linking same-named subs across packages.
    FunctionCall {
        #[serde(default)]
        resolved_package: Option<String>,
    },
    /// Method call site `$obj->m(...)` / `Class->m(...)` /
    /// `chain()->m(...)`. **Invocant class is NOT cached on the
    /// variant** — it's resolved on demand via
    /// `FileAnalysis::method_call_invocant_class(ref, module_index)`,
    /// which dispatches by invocant shape (variable / chain receiver /
    /// function-call receiver / bareword / `__PACKAGE__` / `shift` /
    /// `$_[0]`) and queries the witness bag. This is intentional:
    /// a cached field would silently miss invocants that get typed
    /// only by post-build cross-file enrichment, and chain hops
    /// can't be cached without invalidation. The bag-routed helper
    /// composes through cross-file enrichment automatically.
    ///
    /// Build-time chain typing still runs in the builder — it
    /// publishes Variable witnesses + chain-receiver `Expression`
    /// edge witnesses; the helper reads those at query time.
    MethodCall {
        invocant: String,
        /// Span of the invocant node. Used by
        /// `method_call_invocant_class` to find an inner-receiver
        /// ref via `call_ref_by_start` (chain dispatch).
        invocant_span: Option<Span>,
        /// Span of just the method name (for rename — r.span covers the whole expression).
        method_name_span: Span,
    },
    PackageRef,
    HashKeyAccess {
        var_text: String,
        owner: Option<HashKeyOwner>,
    },
    /// The container variable in `$hash{key}`, `@arr[0]`, etc.
    ContainerAccess,
    /// Call site that dispatches to a `Handler` symbol by string name,
    /// e.g. `$emitter->emit('ready', ...)`. `dispatcher` is the method
    /// name chosen on the receiver (`"emit"`, `"subscribe"`, etc.).
    /// `owner` is resolved at build time when the receiver type is
    /// known; otherwise left `None` and re-linked by enrichment later.
    /// `target_name` on the enclosing `Ref` is the handler name
    /// (the string literal first-arg).
    DispatchCall {
        dispatcher: String,
        owner: Option<HandlerOwner>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AccessKind {
    Read,
    Write,
    Declaration,
}

// ---- Type constraints ----

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeConstraint {
    pub variable: String,
    pub scope: ScopeId,
    pub constraint_span: Span,
    pub inferred_type: InferredType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum InferredType {
    /// `$p = Point->new(...)` — variable is an instance of ClassName.
    ClassName(String),
    /// `my ($self) = @_` in `package Foo` — first param is the class.
    FirstParam { package: String },
    /// `$x = {}` or `$x = { ... }` — unblessed hash reference.
    HashRef,
    /// `$x = []` or `$x = [ ... ]` — unblessed array reference.
    ArrayRef,
    /// `$x = sub { ... }` — code reference. `return_edge` is a
    /// witness-bag attachment whose type IS the callable's return
    /// when invoked. Two shapes populate it:
    ///
    ///   - Anonymous-sub literals (`sub { ... }`) →
    ///     `Expr(body_last_expr_span)`. The bag walks that span's
    ///     own witnesses at query time, after the body is built.
    ///   - Named-sub references (`\&foo`, `\&Foo::bar`) →
    ///     `MethodOnClass { class, name }`. Same attachment the
    ///     bag's existing edge-chase uses for method dispatch —
    ///     resolves in-file via the named-sub's Symbol witnesses
    ///     AND cross-file via `module_index` (the bag transparently
    ///     recurses into the cached module's bag).
    ///
    /// Survives variable rebinding because chain typing propagates
    /// the whole `InferredType` through `my $sub = ...` via the
    /// bag's TC machinery — so `helper(name => sub {...})` and
    /// `my $cb = \&foo; helper(name => $cb)` both reach the same
    /// attachment-driven resolution.
    ///
    /// `None` for opaque sources (params typed `CodeRef`, deref-
    /// shape narrowing, `Rep::Code` observations) where no body or
    /// named target is reachable from the syntax alone.
    CodeRef { return_edge: Option<crate::witnesses::WitnessAttachment> },
    /// `$x = qr/.../` — compiled regular expression.
    Regexp,
    /// Used in numeric context (`+`, `-`, `==`, etc.).
    Numeric,
    /// Used in string context (`.`, `eq`, `=~`, etc.).
    String,
    /// Parametric type — a sealed enum where each variant carries
    /// its own data shape (concrete flavors) or wraps an operand
    /// for type-level projections. Per-axis methods on
    /// `ParametricType` (`class_name`, `hash_key_class`,
    /// `method_arg_owner`) carry per-flavor policy. **Match
    /// invariant: never `_ => …`** — compiler exhaustiveness is
    /// the safety net for the future `Plugin` escape hatch variant.
    /// See `docs/adr/parametric-types.md`.
    Parametric(ParametricType),
    /// Positional container — `my @arr = (...)` or
    /// `push @arr, ...` contributions accumulated walk-side. The
    /// `Vec` stores per-index types; `element_at(i)` projects.
    /// Tuple shape only (no homogeneous/heterogeneous classification).
    ///
    /// Placed at the END of the enum so bincode-serialized cache
    /// blobs keep their existing variant indices stable. Inserting
    /// new variants in the middle would shift every subsequent
    /// variant's wire-format index and silently misread old blobs.
    Sequence(Vec<InferredType>),
    /// A Type::Tiny / Types::Standard constraint *object* —
    /// `InstanceOf['Foo']`, `ArrayRef[Int]`, … — carrying the type it
    /// constrains values to. The constraint is a value in its own right:
    /// method dispatch on it routes to `Type::Tiny` (deferred), NOT the
    /// inner type. Its one job here is projection: an `isa => <constraint>`
    /// gives its accessor the *constrained* (inner) type via
    /// `constrained_inner()`. A plugin's `type_constraint_inner` fold
    /// produces the inner; the core wraps it. See
    /// `docs/prompt-type-constraint-types.md`. Kept at the END for
    /// bincode variant-index stability (bump `EXTRACT_VERSION`).
    TypeConstraintOf(Box<InferredType>),
}

/// Concrete parametric flavors + type-level operators. Each
/// concrete flavor carries the data its semantics need; operators
/// (`RowOf`) wrap a sub-`InferredType` and project via the
/// value-side accessors (`class_name` / `hash_key_class`), or in
/// symbol-declarative form via `ReturnExprReducer`'s `Operator(RowOf)`
/// arm.
///
/// **Match invariant: never `_ => …` arms.** Every consumer
/// explicitly handles every variant. The compiler enforces this
/// when the (deferred) `Plugin` escape hatch lands.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParametricType {
    /// DBIC `$schema->resultset('Foo')` shape. `base` is the
    /// resolved resultset class (default
    /// `DBIx::Class::ResultSet`, or a discovered custom resultset
    /// class — see `goto_def_offers_custom_resultset_method` red-
    /// pin). `row` is the row class (where `add_columns`
    /// synthesizes its column accessors). Two distinct fields
    /// because the value carries dual identity:
    ///   - method dispatch goes through `base`
    ///   - hash-key arg owner / direct hash-key access go through `row`
    ///
    /// Pinned by internal-shape tests so a refactor to a single-class
    /// encoding can't silently merge the two dimensions.
    ///
    /// The row-of projection (`find`/`first`/… → the row class) lives on
    /// the deferred side as `ReturnExpr::Operator(RowOf)`, which
    /// `eval_return_expr` projects eagerly to `ClassName(row)` — there is
    /// no value-side `RowOf` variant.
    ResultSet { base: String, row: String },
}

impl ParametricType {
    /// Class to consult for method dispatch on this value.
    /// `$rs->all` resolves against `class_name()`'s answer.
    pub fn class_name(&self) -> Option<&str> {
        match self {
            ParametricType::ResultSet { base, .. } => Some(base.as_str()),
        }
    }

    /// Class to consult for direct `recv->{key}` hash-key access
    /// on this value. ResultSet returns `row` (the column-keyed
    /// class — used today only by the cleanup-pass HashKeyAccess
    /// owner-resolution paths; HRI shape isn't supported but the
    /// field is the right one when it lands).
    pub fn hash_key_class(&self) -> Option<&str> {
        match self {
            ParametricType::ResultSet { row, .. } => Some(row.as_str()),
        }
    }

    /// Owner for hash-key args of `recv->method({KEY => ...})`.
    /// `Some(owner)` means "this flavor claims this method's args
    /// — emit the HashKeyAccess unconditionally with this owner;
    /// the type IS the gate." `None` means "this flavor doesn't
    /// claim, fall through to the strict-eq local-symbol path."
    ///
    /// ResultSet claims the row-keyed methods (search, search_rs,
    /// find, find_or_new, find_or_create, update_or_create, create,
    /// update, populate, new_result). Methods that take filters or no
    /// args (count, exists, delete, all without args) return None.
    pub fn method_arg_owner(&self, method: &str) -> Option<HashKeyOwner> {
        match self {
            ParametricType::ResultSet { row, .. } => match method {
                "search" | "search_rs" | "find" | "find_or_new" | "find_or_create"
                | "update_or_create" | "create" | "update" | "populate" | "new_result" => {
                    Some(HashKeyOwner::Class(row.clone()))
                }
                _ => None,
            },
        }
    }


    /// Symbol-declarative projection table — list of `(method_name,
    /// ReturnExpr)` pairs the flavor publishes on
    /// `MethodOnClass{base, method}` so consumers chasing through
    /// inheritance / coderef-edge / dynamic-method routes hit the
    /// projection without the call-site `parametric_resultset` witness
    /// firing. Used by `emit_parametric_return_expr_decls`
    /// after every `extract_resultset_parametric` hit — the `base`
    /// discovered there pins the class slot for the witness.
    ///
    /// `ReturnExpr::Operator(RowOf(Receiver))` evaluates at the
    /// reducer with `q.receiver = the call's invocant type`. For
    /// `\&MyRS::find; $cb->($rs, ...)`, the chain typer's coderef
    /// arm sees the target is `MethodOnClass{MyRS, find}`,
    /// inheritance walks to `MethodOnClass{DBIx::Class::ResultSet,
    /// find}`, finds the `Operator(RowOf, Receiver)` declaration,
    /// substitutes `q.receiver = $rs`'s `Parametric(ResultSet)`,
    /// evaluates `RowOf(ResultSet { row, .. }) → ClassName(row)`.
    pub fn return_method_declarations(
        &self,
    ) -> Vec<(&'static str, crate::witnesses::ReturnExpr)> {
        match self {
            ParametricType::ResultSet { .. } => {
                let row_of_receiver = crate::witnesses::ReturnExpr::Operator(
                    crate::witnesses::ParametricOp::RowOf(Box::new(
                        crate::witnesses::ReturnExpr::Receiver,
                    )),
                );
                ["find", "first", "single", "next", "create",
                 "find_or_new", "find_or_create", "update_or_create",
                 "new_result"]
                    .iter()
                    .map(|m| (*m, row_of_receiver.clone()))
                    .collect()
            }
        }
    }
}

impl InferredType {
    /// Extract the class name if this is an object type
    /// (ClassName, FirstParam, or Parametric — the latter
    /// delegates to the flavor's `class_name()`). For the row-
    /// class / hash-key-arg dimension on a Parametric, see
    /// `hash_key_class`.
    pub fn class_name(&self) -> Option<&str> {
        match self {
            InferredType::ClassName(name) => Some(name.as_str()),
            InferredType::FirstParam { package } => Some(package.as_str()),
            InferredType::Parametric(p) => p.class_name(),
            _ => None,
        }
    }

    /// Project a `TypeConstraintOf(inner)` to its constrained inner type —
    /// the type a value satisfying this constraint has. `None` for any
    /// non-constraint type. This is the rule-#10 "ask the value" entry
    /// point: `has`'s isa→accessor projection calls it without ever
    /// matching on the constraint's shape itself.
    pub fn constrained_inner(&self) -> Option<&InferredType> {
        match self {
            InferredType::TypeConstraintOf(inner) => Some(inner),
            _ => None,
        }
    }

    /// Project a `Sequence(...)` to its element at index `i`. Negative
    /// indices wrap from the end (Perl `$arr[-1]`). `None` for any
    /// non-Sequence type or out-of-bounds index.
    pub fn element_at(&self, i: i32) -> Option<&InferredType> {
        let InferredType::Sequence(elems) = self else { return None };
        let n = elems.len() as i32;
        let idx = if i < 0 { n + i } else { i };
        if idx < 0 || idx >= n { return None; }
        elems.get(idx as usize)
    }

    /// True if this is any object-shaped variant (ClassName,
    /// FirstParam, or a Parametric flavor that has a class_name).
    pub fn is_object(&self) -> bool {
        self.class_name().is_some()
    }

    /// Class to consult for direct `recv->{key}` hash-key access
    /// on this value. For Parametric, delegates to the flavor; for
    /// other variants, falls back to `class_name()` (constructor
    /// keys etc. on `bless { } 'Foo'`-shaped values).
    pub fn hash_key_class(&self) -> Option<&str> {
        match self {
            InferredType::Parametric(p) => p.hash_key_class(),
            _ => self.class_name(),
        }
    }

    /// Direct accessor to the parametric flavor, when this type
    /// is `Parametric(_)`. Lets consumers route to flavor-specific
    /// methods (`method_arg_owner`, etc.) without re-matching.
    pub fn as_parametric(&self) -> Option<&ParametricType> {
        match self {
            InferredType::Parametric(p) => Some(p),
            _ => None,
        }
    }

    /// Witness-bag attachment whose type IS this callable's return
    /// when invoked. `Expr(span)` for anon-sub literals (resolves
    /// at query time via the body's last-expression witnesses);
    /// `MethodOnClass{class, name}` for named-sub references
    /// (`\&foo`, `\&Foo::bar` — resolves via the bag's existing
    /// MRO + cross-file machinery, same shape used by method
    /// dispatch). Returns `None` for opaque coderef sources.
    ///
    /// Survives variable rebinding: chain typing propagates the
    /// `InferredType` through `my $cb = ...` via the bag's TC
    /// machinery, so consumers see the same attachment whether
    /// the callable arrives as a literal or a rebound scalar.
    pub fn callable_return_edge(&self) -> Option<&crate::witnesses::WitnessAttachment> {
        match self {
            InferredType::CodeRef { return_edge } => return_edge.as_ref(),
            _ => None,
        }
    }

    /// True when `self` is at least as informative as `narrowing`
    /// — adding the narrowing's TC would not refine `self` further.
    /// "Informativeness" is defined per-variant: same discriminant
    /// AND, where the variant carries refinable payload, `self`'s
    /// payload is at least as specific as `narrowing`'s.
    ///
    /// Used by `infer_deref_type` to suppress the
    /// `$cb->()`-shaped narrowing TC when the operand was already
    /// typed with a richer attachment (e.g. an anon-sub literal's
    /// `CodeRef { return_edge: Some(_) }` should NOT be clobbered
    /// by the deref's `CodeRef { return_edge: None }` under
    /// latest-wins reduction).
    ///
    /// Conservative: returns false on different discriminants
    /// (let the reducer-stack decide the conflict). Variants
    /// without refinable payload (HashRef/ArrayRef/Regexp/Numeric/
    /// String) subsume themselves trivially.
    pub fn subsumes_narrowing(&self, narrowing: &InferredType) -> bool {
        match (self, narrowing) {
            // Refinable-payload variants — `self` subsumes only
            // if its payload is at least as specific.
            (
                InferredType::CodeRef { return_edge: have },
                InferredType::CodeRef { return_edge: want },
            ) => want.is_none() || have.is_some(),
            (InferredType::ClassName(a), InferredType::ClassName(b)) => a == b,
            (InferredType::FirstParam { package: a }, InferredType::FirstParam { package: b }) => {
                a == b
            }
            (InferredType::Parametric(a), InferredType::Parametric(b)) => a == b,
            // Unit-shape variants subsume themselves; mismatched
            // discriminants don't subsume.
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b),
        }
    }
}

/// Where a type judgement came from. Lets debugging surface "the
/// analyzer worked this out from your code" vs "a plugin override
/// said so" without changing the shape of `InferredType` at every
/// callsite. Stored in a sidecar map keyed by SymbolId — entries
/// only exist for non-default provenances, so the common case (an
/// inferred type) costs nothing.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeProvenance {
    /// Derived from the analyzed source — return statements,
    /// last-expression inference, framework synthesis, constructor
    /// patterns. The default; never stored explicitly.
    Inferred,
    /// Asserted by a plugin's `overrides()` manifest because
    /// inference can't (or shouldn't) reach the right answer here.
    /// Carries the asserting plugin's id and a free-form reason for
    /// the debugger.
    PluginOverride { plugin_id: String, reason: String },
    /// Produced by a witness-bag fold at type-resolution time.
    /// `reducer` names the rule that fired (currently only
    /// `"return_arms"` is recorded — see `seed_return_types_from_bag`);
    /// `evidence` is a short list of human-readable facts the fold
    /// leaned on. Read-only debugging aid surfaced by `--dump-package`.
    /// Empty `evidence` is fine — the reducer name alone often answers
    /// "why".
    ReducerFold { reducer: String, evidence: Vec<String> },
    /// Tail-delegation: the sub's body ends in `shift->M(...)` /
    /// `$self->M(...)` / `return Y()` and inherits the tail's
    /// return type. `via` is the delegate's name; `kind` is
    /// "self_method_tail" or "sub_return". Lets `--dump-package`
    /// answer "get returns ClassName(Route) — because it tails on
    /// _generate_route which the framework-aware reducer typed as
    /// ClassName(Route)".
    Delegation { kind: String, via: String },
    /// Core framework synthesis — Mojo::Base / Moo / Moose `has`,
    /// DBIx::Class `add_columns` / `has_many` / etc. The accessor
    /// has no source body to fold; the type comes directly from the
    /// declaration shape (Mojo writers always return the invocant;
    /// Moo getters honour `isa`; DBIC relationships return the
    /// related class). `framework` names the rule set
    /// ("Mojo::Base" / "Moo" / "Moose" / "DBIx::Class") and
    /// `reason` describes the specific accessor ("`has 'level'`
    /// fluent writer", "DBIx::Class row relationship `book`").
    /// Distinct from `PluginOverride` because plugins are user-installed
    /// and configurable; framework synthesis is built into the analyzer.
    FrameworkSynthesis { framework: String, reason: String },
}

/// Resolve a return type from a list of inferred types (one per return statement).
///
/// Rules (from spec):
/// - All agree → that type
/// - Object subsumes HashRef (overloaded objects are common)
/// - Disagreement → None (Unknown)
///
/// The input should already have bare returns / undef filtered out.
pub fn resolve_return_type(return_types: &[InferredType]) -> Option<InferredType> {
    if return_types.is_empty() {
        return None;
    }
    let first = &return_types[0];
    if return_types.iter().all(|t| t == first) {
        return Some(first.clone());
    }
    // Object subsumes HashRef: if some returns are Object(X) and others are HashRef,
    // the Object wins (overloaded hash access is common in Perl).
    let mut object = None;
    for t in return_types {
        if t.is_object() {
            object = Some(t.clone());
        } else if *t != InferredType::HashRef {
            // Non-HashRef, non-Object disagreement → Unknown
            return None;
        }
    }
    object
}

// ---- Handler owner ----

/// Plugin-chosen LSP display kind for a `Handler`. Handlers all share
/// the same internal mechanism (string-dispatched, stacked, cross-file),
/// but they aren't all the same thing *semantically* — routes are
/// method-ish, events are event-ish, config keys are field-ish. The
/// plugin decides what icon the editor shows.
///
/// Expand this enum when a plugin has a concept that doesn't fit any
/// existing variant; every variant maps to a corresponding LSP
/// `SymbolKind` / `CompletionItemKind` via thin translation in
/// `symbols.rs`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HandlerDisplay {
    Event,
    Method,
    Function,
    Field,
    Property,
    Constant,
    /// Plugin-synthesized callable on the framework's app instance.
    /// Renders with LSP kind FUNCTION; outline detail prints "helper".
    Helper,
    /// Framework-declared URL pattern. Same LSP kind as Helper/Task;
    /// outline detail prints "route" so the client can disambiguate.
    Route,
    /// A controller action referenced from a routing declaration —
    /// e.g. the `Users#list` target of Mojolicious' `->to('Users#list')`
    /// (Mojo's own docs call this an "action") or Catalyst's
    /// `->forward('/users/list')`. Distinct from `Route` because no
    /// request-handling body lives at this source site: it's a
    /// cross-reference to a method that lives elsewhere. Outline word
    /// "action" so `<route> GET /users` and `<action> Users#list` read
    /// as two different kinds of line items.
    Action,
    /// Job-queue / worker task (Minion etc.). Helper-kin for the LSP
    /// kind, distinct "task" word in outline detail.
    Task,
}

impl Default for HandlerDisplay {
    fn default() -> Self { Self::Event }
}

impl HandlerDisplay {
    /// Short human-readable word the outline puts in `detail` so LSP
    /// clients can show `[Function] name — helper` even though the
    /// LSP kind enum doesn't have a Helper variant. Returns `None`
    /// for display kinds that don't carry a distinguishing word
    /// beyond the LSP kind itself.
    pub fn outline_word(&self) -> Option<&'static str> {
        match self {
            HandlerDisplay::Event => Some("event"),
            HandlerDisplay::Helper => Some("helper"),
            HandlerDisplay::Route => Some("route"),
            HandlerDisplay::Action => Some("action"),
            HandlerDisplay::Task => Some("task"),
            _ => None,
        }
    }
}

/// Owner of a `Handler` symbol. Distinct from `HashKeyOwner` because
/// hash keys and dispatch handlers are different concepts even though
/// both happen to be keyed by a name under a class. Keeping them split
/// prevents overload creep — each stays free to evolve on its own axis.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HandlerOwner {
    /// Handler is registered on a specific class (typical for Mojo
    /// events, Moose roles, DBIC relationships, etc.).
    Class(String),
}

// ---- Plugin namespace ----

/// A plugin-controlled scope: the plugin says "I own a namespace — here's
/// its bridges (how Perl-space expressions find it) and its entities",
/// rather than masquerading entities as Methods on a hijacked Perl class.
///
/// Why this exists:
///   * Helpers aren't methods on `Mojolicious::Controller`. They're
///     callables on the app instance, reached THROUGH a controller.
///   * Two apps in one workspace become two `PluginNamespace`s with
///     the same `Class("Mojolicious::Controller")` bridge. Their
///     entities don't collide at the class level — they're owned by
///     the namespace, not the class.
///   * Cross-file lookup is one primitive
///     (`ModuleIndex::for_each_entity_bridged_to(class, ...)`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginNamespace {
    /// Plugin-generated unique identifier. E.g.
    /// `"mojo-app:/abs/path/to/MyApp.pm"` or
    /// `"minion:$minion@MyApp.pm:5"`. Plugin decides how to
    /// disambiguate multiple instances in a single workspace.
    pub id: String,
    /// Which plugin registered this namespace — the Namespace::Framework
    /// `id` that gets stamped on emitted entities.
    pub plugin_id: String,
    /// Plugin-defined kind tag. `"app"`, `"minion"`, `"emitter"`, …
    /// Used by display/completion to tell users what sort of thing
    /// a namespace member is.
    pub kind: String,
    /// Symbols that belong to this namespace. Cross-references into
    /// the same FileAnalysis's `symbols` table — plugins still emit
    /// Methods / Handlers normally; the namespace indexes them.
    pub entities: Vec<SymbolId>,
    /// How Perl-space expressions reach this namespace's entities.
    pub bridges: Vec<Bridge>,
    /// Span where the plugin declared the namespace (typically the
    /// registration call — `$app->plugin('Minion', ...)` etc.).
    pub decl_span: Span,
}

/// A connection from a Perl-space type/shape into a plugin namespace.
/// When a lookup asks "what's reachable from class X?", the core
/// unions Perl-native methods with entities from every namespace
/// whose bridges match X.
///
/// Currently only `Class` is wired — `Bareword` / `Variable` would
/// require lookup machinery (`bareword_index`, per-variable bridge
/// table) that doesn't exist yet. Re-add them when a concrete plugin
/// needs the shape; speculative variants just rot.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Bridge {
    /// Any expression typed as this class (or a subclass reached via
    /// inheritance walk) can see this namespace's entities. The
    /// canonical bridge for framework helpers on controllers.
    Class(String),
}

/// Fictional "app surface" class — the synthetic ancestor that the
/// Mojolicious app / controller / command classes (the manifest-declared
/// consumer set, see `FrameworkPlugin::app_surface_consumers`) all
/// inherit, so a single bridge target reaches every receiver that can see
/// helpers (docs/prompt-app-entity.md). Helpers bridge to THIS one class;
/// the consumer classes get it as a synthetic parent injected in the MRO
/// walk (`parents_of`). The existing ancestor walk + bridge resolution
/// then finds helpers with no per-receiver bridge list. Not a real Perl
/// package — never resolves to a file, has no parents, so it's inert in
/// the walk beyond contributing its bridge.
pub const APP_SURFACE_CLASS: &str = "Mojolicious::_AppSurface";

/// Inject the synthetic app-surface ancestor (`APP_SURFACE_CLASS`) when
/// `class` is one of the declared `consumers`. The ONE place the
/// synthetic-parent edge is added — every parent-enumeration site
/// (`for_each_ancestor_class`, `collect_ancestor_methods`, and the
/// `MethodOnClass` inheritance walk in `witnesses.rs`) routes through
/// here so they can't drift. Real ancestors come first; the surface is
/// appended last so same-name overrides on a real parent win. The
/// surface has no parents of its own, so the walk's seen-set + depth cap
/// bound it like any edge.
pub fn parents_of(
    class: &str,
    package_parents: &HashMap<String, Vec<String>>,
    module_index: Option<&ModuleIndex>,
    consumers: &[String],
) -> Vec<String> {
    let mut parents: Vec<String> = package_parents.get(class).cloned().unwrap_or_default();
    if let Some(idx) = module_index {
        for p in idx.parents_cached(class) {
            if !parents.contains(&p) {
                parents.push(p);
            }
        }
    }
    if class != APP_SURFACE_CLASS
        && consumers.iter().any(|c| c == class)
        && !parents.iter().any(|p| p == APP_SURFACE_CLASS)
    {
        parents.push(APP_SURFACE_CLASS.to_string());
    }
    parents
}

// ---- Hash key owner (for scope graph) ----

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HashKeyOwner {
    Class(String),
    Variable { name: String, def_scope: ScopeId },
    /// Hash keys from a sub's return value: `sub get_config { return { host => 1 } }`.
    /// `package` is the enclosing Perl package at the sub's declaration site
    /// (or `None` for top-level script subs where no `package` statement is
    /// in scope). Without this, two different packages each defining
    /// `sub get_config { ... host ... }` would collide at query time.
    Sub { package: Option<String>, name: String },
}

impl HashKeyOwner {
    /// Directional match: would a lookup with `lookup` owner reach a
    /// def with `self` owner? Strict equality, plus the broadening
    /// rule that a `Class(C)` lookup picks up `Sub{Some(C), _}` defs.
    ///
    /// Why: bless-inside-a-sub registers HashKeyDefs as `Sub{C,
    /// sub_name}` (the constructor sub). `has` does the same. But
    /// `$obj->{key}` deref refs and `complete_hash_keys_for_class`
    /// callers carry `Class(C)` — that's the "any key for objects of
    /// C" lookup. The asymmetry keeps strict `Sub{C, M}` lookups
    /// from accidentally finding keys registered to a *different*
    /// method on the same class.
    pub fn found_by(&self, lookup: &HashKeyOwner) -> bool {
        if self == lookup { return true; }
        match (self, lookup) {
            (HashKeyOwner::Sub { package: Some(c1), .. }, HashKeyOwner::Class(c2)) => c1 == c2,
            _ => false,
        }
    }
}


// ---- Call binding ----

/// A variable assigned from a function call: `my $cfg = get_config()`.
/// Stored in FileAnalysis so query-time resolution can follow the chain.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallBinding {
    pub variable: String,
    pub func_name: String,
    pub scope: ScopeId,
    pub span: Span,
}

/// A method call binding: `$var = $invocant->method()`.
/// Recorded during build, resolved in post-pass via `find_method_return_type`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MethodCallBinding {
    pub variable: String,
    pub invocant_var: String,
    pub method_name: String,
    pub scope: ScopeId,
    pub span: Span,
}

/// A build-time dispatch candidate: a call to a plugin-declared dispatch
/// verb (`$x->enqueue('T')`), recorded before we know whether the receiver
/// actually `isa` the verb's target class. Enrichment resolves that
/// cross-file and, if it holds, promotes this into a `DispatchCall` ref.
/// See `plugin::DispatchVerb` and `FileAnalysis::enrich_imported_types_with_keys`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvisionalDispatch {
    /// The handler name (task / event) — the dispatch's first meaningful arg.
    pub name: String,
    /// Span of the name argument (the future `DispatchCall` ref span).
    pub span: Span,
    /// The verb (`enqueue`), kept for the `DispatchCall.dispatcher`.
    pub dispatcher: String,
    /// Handler owner the synthesized ref pairs against (e.g. `Minion`).
    pub owner_class: String,
    /// Receiver must `isa` this for the candidate to promote.
    pub target_class: String,
    /// Receiver's class as resolved at build time, if any. `None` when the
    /// receiver type wasn't known locally (e.g. a helper-returned value);
    /// enrichment then re-resolves it cross-file via `call_span`'s MethodCall
    /// ref + the module index.
    #[serde(default)]
    pub receiver_class: Option<String>,
    /// Whole-call span of the dispatch call (`node_to_span` of the
    /// `method_call_expression`). Matches the native MethodCall ref's `span`,
    /// so enrichment can find that ref and resolve the receiver class with
    /// the index when `receiver_class` is `None`.
    pub call_span: Span,
}

// ---- Import ----

/// One name brought into scope by a `use` statement.
///
/// `local_name` is how the name appears at call sites in this file.
/// `remote_name` is the sub's real name in the source module — usually
/// identical to `local_name` (the common case, encoded as `None` to
/// keep the serialized form compact) and different only for renaming
/// imports: `del` in Mojolicious::Lite is really `delete` on
/// Mojolicious::Routes::Route, `use Exporter::Tiny ( foo => { -as => 'bar' } )`
/// gives a `bar` locally pointing at the real `foo`, etc.
///
/// Cross-file lookups always resolve via `remote()` so hover, gd, sig
/// help, and return-type inference use the real module's `sub_info`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ImportedSymbol {
    pub local_name: String,
    /// `None` means the local and remote names are the same.
    ///
    /// No `skip_serializing_if` — bincode is a non-self-describing
    /// format and needs the field present on the wire regardless.
    /// Self-describing formats (JSON, YAML) happily encode `null`
    /// for None too, so keeping it always-serialized is fine
    /// everywhere.
    #[serde(default)]
    pub remote_name: Option<String>,
}

impl ImportedSymbol {
    /// Same-name import — the overwhelmingly common case.
    pub fn same(name: impl Into<String>) -> Self {
        Self { local_name: name.into(), remote_name: None }
    }
    /// Renaming import — local name differs from the real sub's name.
    ///
    /// Currently constructed only from Rhai plugins via serde-deserialized
    /// maps (`#{ local_name: ..., remote_name: ... }`), so this Rust-side
    /// constructor has no direct caller yet. Keeping it as documented public
    /// API so future Rust callers (e.g. a hand-written parser for renaming
    /// import syntax, or core plugins) have the canonical way to build one.
    #[allow(dead_code)]
    pub fn renamed(local: impl Into<String>, remote: impl Into<String>) -> Self {
        let remote = remote.into();
        let local = local.into();
        // Collapse `remote == local` to the same-name shape so downstream
        // code doesn't need to handle both representations.
        if remote == local {
            Self { local_name: local, remote_name: None }
        } else {
            Self { local_name: local, remote_name: Some(remote) }
        }
    }
    /// Real name in the source module — used for cross-file sub_info lookup.
    pub fn remote(&self) -> &str {
        self.remote_name.as_deref().unwrap_or(&self.local_name)
    }
}

/// A `use Foo::Bar qw(func1 func2)` statement parsed from the source.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    /// Module name, e.g. "List::Util".
    pub module_name: String,
    /// Explicitly imported names from `qw(...)`. Empty = bare `use Foo;`.
    /// Each entry carries local + (optional) remote name for renaming imports.
    pub imported_symbols: Vec<ImportedSymbol>,
    /// Span of the entire `use` statement.
    pub span: Span,
    /// Position of the closing delimiter of the `qw()` list (the `)` character).
    /// Used to insert new imports into an existing qw list.
    #[serde(with = "point_opt_serde")]
    pub qw_close_paren: Option<Point>,
}

// ---- Outline ----

pub struct OutlineSymbol {
    pub name: String,
    pub detail: Option<String>,
    pub kind: SymKind,
    pub span: Span,
    pub selection_span: Span,
    pub children: Vec<OutlineSymbol>,
    /// For `Handler` symbols, the plugin-chosen LSP display kind.
    /// Carried here so the outline→DocumentSymbol conversion doesn't
    /// need to re-resolve the SymbolDetail. `None` for non-Handler
    /// kinds (they use the fixed SymKind → LSP mapping).
    pub handler_display: Option<HandlerDisplay>,
}

// ---- Semantic tokens ----

// Token type/modifier indices — must match the order in semantic_token_types/modifiers().
// Some are forward-declared for future phases.
pub const TOK_VARIABLE: u32 = 0;
pub const TOK_PARAMETER: u32 = 1;
pub const TOK_FUNCTION: u32 = 2;
pub const TOK_METHOD: u32 = 3;
pub const TOK_MACRO: u32 = 4;
pub const TOK_PROPERTY: u32 = 5;
pub const TOK_NAMESPACE: u32 = 6;
#[allow(dead_code)] pub const TOK_REGEXP: u32 = 7;
#[allow(dead_code)] pub const TOK_ENUM_MEMBER: u32 = 8;
pub const TOK_KEYWORD: u32 = 9;

pub const MOD_DECLARATION: u32 = 0;
pub const MOD_READONLY: u32 = 1;
pub const MOD_MODIFICATION: u32 = 2;
pub const MOD_DEFAULT_LIBRARY: u32 = 3;
#[allow(dead_code)] pub const MOD_DEPRECATED: u32 = 4;
#[allow(dead_code)] pub const MOD_STATIC: u32 = 5;
pub const MOD_SCALAR: u32 = 6;
pub const MOD_ARRAY: u32 = 7;
pub const MOD_HASH: u32 = 8;

#[derive(Debug, Clone)]
pub struct PerlSemanticToken {
    pub span: Span,
    pub token_type: u32,
    pub modifiers: u32,
}

/// Plugin-chosen LSP display for a Sub-detail symbol, if any. Null for
/// everything else — lets completion/outline carry plugin intent
/// without each site re-matching on detail shape.
fn sub_display_override(detail: &SymbolDetail) -> Option<HandlerDisplay> {
    if let SymbolDetail::Sub { display, .. } = detail {
        *display
    } else {
        None
    }
}

fn sigil_modifier(sigil: char) -> u32 {
    match sigil {
        '@' => 1 << MOD_ARRAY,
        '%' => 1 << MOD_HASH,
        _ => 1 << MOD_SCALAR,
    }
}

// ---- FileAnalysis ----

#[derive(Debug, Serialize, Deserialize)]
pub struct FileAnalysis {
    // Core tables
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub refs: Vec<Ref>,
    pub fold_ranges: Vec<FoldRange>,
    pub imports: Vec<Import>,
    pub call_bindings: Vec<CallBinding>,
    pub method_call_bindings: Vec<MethodCallBinding>,

    /// Flat list of `package`/`class` declarations and the byte ranges
    /// they govern. Replaces `Scope::package` walks for `package_at`.
    /// `#[serde(default)]` so older cache blobs deserialize as empty
    /// (`package_at` falls back to the legacy scope walk in that case).
    #[serde(default)]
    pub package_ranges: Vec<PackageRange>,

    /// Parent classes for each package in this file.
    /// Populated by the builder from use parent/base, @ISA, and class :isa.
    pub package_parents: HashMap<String, Vec<String>>,

    /// Manifest-declared app-surface consumer classes
    /// (`FrameworkPlugin::app_surface_consumers`), baked from the plugin
    /// registry at build so the query-time ancestor walk can inject the
    /// synthetic `APP_SURFACE_CLASS` parent (`parents_of`) without
    /// re-reading the registry. `#[serde(default)]` so older cache blobs
    /// deserialize as empty.
    #[serde(default)]
    pub app_surface_consumers: Vec<String>,

    /// Modules `use`-d inside each package in this file. Parallel to
    /// `package_parents`: keyed by the enclosing package name, values are
    /// module names. Powers trigger-matching for plugin query hooks
    /// (emit-path builder state isn't visible at cursor time).
    pub package_uses: HashMap<String, Vec<String>>,

    /// Functions implicitly imported by OOP frameworks (e.g. `has`, `extends`, `with`).
    /// Used to suppress "not defined" diagnostics for these known framework keywords.
    pub framework_imports: HashSet<String>,

    /// Exported function names from `@EXPORT = ...` assignments.
    pub export: Vec<String>,
    /// Exported function names from `@EXPORT_OK = ...` assignments.
    pub export_ok: Vec<String>,

    /// Plugin-declared namespaces. Each is a scope managed by a plugin
    /// (a Mojolicious app, a Minion instance, an event-emitter subclass,
    /// …). Declares bridges into Perl-space and owns a set of entities.
    /// Lookups union these with native Perl resolution — see
    /// `ModuleIndex::for_each_entity_bridged_to` for the cross-file primitive.
    #[serde(default)]
    pub plugin_namespaces: Vec<PluginNamespace>,

    /// Per-symbol provenance for return types. Populated for plugin
    /// `overrides()` and for reducer-driven folds over the witness bag.
    /// Missing entry == `TypeProvenance::Inferred`.
    /// Read-only debugging aid: features like hover/completion don't
    /// branch on it; it exists so `--dump-package` and a future
    /// inspector can answer "why does the LSP think this returns X?"
    /// without re-running the build.
    #[serde(default)]
    pub type_provenance: HashMap<SymbolId, TypeProvenance>,

    /// Detected framework mode per package (for the type resolver).
    /// Populated by the builder when `use Moo` / `use Mojo::Base` /
    /// `use Moose` etc. is observed.
    #[serde(default)]
    pub package_framework: HashMap<String, crate::witnesses::FrameworkFact>,

    /// The witness bag. Canonical store for type facts:
    /// every Variable type, Symbol/MethodOnClass return type, branch
    /// arm Edge, hash-key observation. `inferred_type_via_bag` reads
    /// here. The builder pushes directly via `push_type_constraint`
    /// (Variable witnesses with TC shape) and per-attachment emit
    /// helpers; cache blobs round-trip the bag in full.
    #[serde(default)]
    pub witnesses: crate::witnesses::WitnessBag,

    /// Witness-bag baseline — `enrich_imported_types_with_keys`
    /// truncates back to this length before re-deriving so repeat
    /// calls stay idempotent.
    #[serde(default)]
    base_symbol_count: usize,
    #[serde(default)]
    base_witness_count: usize,
    /// Ref baseline — enrichment synthesizes `DispatchCall` refs from
    /// `provisional_dispatches` (cross-file receiver isa), so it truncates
    /// `refs` back to this length before re-deriving to stay idempotent.
    #[serde(default)]
    base_ref_count: usize,

    /// Build-time dispatch candidates awaiting a cross-file receiver isa
    /// check. The builder records one per call matching a plugin
    /// `DispatchVerb`; `enrich_imported_types_with_keys` promotes the ones
    /// whose receiver `isa` the verb's `target_class` into `DispatchCall`
    /// refs. See `DispatchVerb` — this is the "receiver provides the magic"
    /// path that file-trigger-gated emit hooks can't reach.
    #[serde(default)]
    pub provisional_dispatches: Vec<ProvisionalDispatch>,

    // Indices (built in post-pass — skipped by serde; call rebuild_all_indices() after deserialize)
    #[serde(skip, default)]
    scope_starts: Vec<(Point, ScopeId)>, // sorted by start point
    #[serde(skip, default)]
    symbols_by_name: HashMap<String, Vec<SymbolId>>,
    #[serde(skip, default)]
    symbols_by_scope: HashMap<ScopeId, Vec<SymbolId>>,
    #[serde(skip, default)]
    refs_by_name: HashMap<String, Vec<usize>>,
    /// Refs indexed by the SymbolId they resolve to (phase 5).
    /// Every query for "refs to symbol X" collapses to an O(1) lookup here.
    #[serde(skip, default)]
    refs_by_target: HashMap<SymbolId, Vec<usize>>,
    /// Start-point → call-shaped ref index. Used by
    /// `method_call_invocant_class` to chase a chain receiver:
    /// `Foo->new->m`'s outer `->m` `invocant_span` starts at the
    /// inner `Foo->new` call's start; `make_b()->touch()`'s outer
    /// `invocant_span` starts at `make_b`'s start. Only MethodCall
    /// and FunctionCall refs go in here — the receiver dispatch is
    /// keyed on those two kinds.
    #[serde(skip, default)]
    call_ref_by_start: HashMap<Point, usize>,
}

impl FileAnalysis {
    /// Create a new FileAnalysis with indices built from the raw tables.
    pub fn new(
        scopes: Vec<Scope>,
        symbols: Vec<Symbol>,
        refs: Vec<Ref>,
        fold_ranges: Vec<FoldRange>,
        imports: Vec<Import>,
        call_bindings: Vec<CallBinding>,
        package_parents: HashMap<String, Vec<String>>,
        method_call_bindings: Vec<MethodCallBinding>,
        framework_imports: HashSet<String>,
        export: Vec<String>,
        export_ok: Vec<String>,
        plugin_namespaces: Vec<PluginNamespace>,
        package_uses: HashMap<String, Vec<String>>,
        type_provenance: HashMap<SymbolId, TypeProvenance>,
        package_ranges: Vec<PackageRange>,
    ) -> Self {
        let mut fa = FileAnalysis {
            scopes,
            symbols,
            refs,
            fold_ranges,
            imports,
            call_bindings,
            method_call_bindings,
            package_ranges,
            package_parents,
            app_surface_consumers: Vec::new(),
            package_uses,
            framework_imports,
            export,
            export_ok,
            plugin_namespaces,
            type_provenance,
            witnesses: crate::witnesses::WitnessBag::new(),
            package_framework: HashMap::new(),
            base_symbol_count: 0,
            base_witness_count: 0,
            base_ref_count: 0,
            provisional_dispatches: Vec::new(),
            scope_starts: Vec::new(),
            symbols_by_name: HashMap::new(),
            symbols_by_scope: HashMap::new(),
            refs_by_name: HashMap::new(),
            refs_by_target: HashMap::new(),
            call_ref_by_start: HashMap::new(),
        };
        fa.build_indices();
        // The builder path overwrites `witnesses` with its already-populated
        // bag after construction. Hand-crafted FAs (tests) push witnesses
        // directly via `fa.witnesses.push(...)` or `push_type_constraint`.
        // `finalize_post_walk` runs on the builder path to seal baseline
        // counts and resolve text-based MCB; tests skip it.
        fa
    }

    /// Run the local-only method-call-binding resolution and seal
    /// baseline counts. Called by `builder::build` after the witness
    /// bag has been moved in.
    ///
    /// `Symbol(sym_id)` and `MethodOnClass{class, name}` return-type
    /// witnesses for every local Sub/Method are already in the bag —
    /// published by `Builder::write_back_sub_return_types` at the
    /// end of the worklist (single emission point for "this sub's
    /// return type is known"). Cross-file imports do not get a local
    /// mirror; they resolve lazily through `query_sub_return_type`.
    pub(crate) fn finalize_post_walk(&mut self) {
        self.resolve_method_call_types(None);
        // Fill HashKeyAccess owners that are resolvable in-file
        // via the chain-recursion dispatcher
        // (`method_call_invocant_type`'s `call_ref_by_start`
        // walk). Cross-file gaps stay None until
        // `enrich_imported_types_with_keys` re-runs the same
        // routine with `module_index`.
        self.fix_chain_receiver_hash_key_owners(None);
        self.base_symbol_count = self.symbols.len();
        self.base_witness_count = self.witnesses.len();
        self.base_ref_count = self.refs.len();
    }

    /// Set the `owner` on `HashKeyAccess { owner: None, .. }` refs
    /// whose enclosing `MethodCall`'s receiver types as a
    /// `Parametric` flavor that claims this method's args (DBIC's
    /// `search`/`find`/`update`/...). Build emits these refs
    /// eagerly with `owner: None` for chain receivers it can't
    /// resolve at walk time; this routine fills them once the
    /// receiver's type is resolvable.
    ///
    /// `module_index = None` resolves only in-file chains. The
    /// same routine runs from enrichment with `module_index =
    /// Some(_)` to fill cross-file gaps. Idempotent — only None-
    /// owner refs are touched, so a second run leaves them alone.
    fn fix_chain_receiver_hash_key_owners(&mut self, module_index: Option<&ModuleIndex>) {
        let mut owner_fixes: Vec<(usize, HashKeyOwner)> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if !matches!(r.kind, RefKind::HashKeyAccess { owner: None, .. }) {
                continue;
            }
            // Find the enclosing MethodCall ref by span
            // containment — smallest-span containing MethodCall
            // wins (innermost call's args).
            let mut enclosing: Option<&Ref> = None;
            let mut enclosing_area: u64 = u64::MAX;
            for other in &self.refs {
                if !matches!(other.kind, RefKind::MethodCall { .. }) {
                    continue;
                }
                if !contains_point(&other.span, r.span.start) {
                    continue;
                }
                let area = (other.span.end.row.saturating_sub(other.span.start.row)) as u64
                    * 10_000
                    + other.span.end.column as u64;
                if area < enclosing_area {
                    enclosing = Some(other);
                    enclosing_area = area;
                }
            }
            let Some(call) = enclosing else { continue };
            let Some(ty) = self.method_call_invocant_type(call, module_index) else {
                continue;
            };
            let Some(p) = ty.as_parametric() else { continue };
            let Some(o) = p.method_arg_owner(&call.target_name) else { continue };
            owner_fixes.push((i, o));
        }
        for (i, o) in owner_fixes {
            if let RefKind::HashKeyAccess { ref mut owner, .. } = self.refs[i].kind {
                *owner = Some(o);
            }
        }
    }


    /// Rebuild all derived indices after deserialization.
    /// Idempotent: safe to call on a freshly deserialized `FileAnalysis` whose
    /// index fields were zeroed by `#[serde(skip, default)]`.
    pub fn after_deserialize(&mut self) {
        // Clear first in case this is called on a populated FileAnalysis.
        self.scope_starts.clear();
        self.symbols_by_name.clear();
        self.symbols_by_scope.clear();
        self.refs_by_name.clear();
        self.refs_by_target.clear();
        self.call_ref_by_start.clear();
        self.build_indices();
    }

    fn build_indices(&mut self) {
        // Scope starts — sorted for binary search
        self.scope_starts = self.scopes.iter()
            .map(|s| (s.span.start, s.id))
            .collect();
        self.scope_starts.sort_by_key(|(p, _)| (p.row, p.column));

        // Symbols by name
        for sym in &self.symbols {
            self.symbols_by_name
                .entry(sym.name.clone())
                .or_default()
                .push(sym.id);
        }

        // Symbols by scope
        for sym in &self.symbols {
            self.symbols_by_scope
                .entry(sym.scope)
                .or_default()
                .push(sym.id);
        }

        // Link HashKeyAccess refs to their HashKeyDef symbols whenever the
        // owner is already resolved (the builder's pre-pass handled type
        // constraints + variable identity + call-binding fixups). With this
        // link, `refs_to_symbol(def_id)` returns all accesses in O(1), which
        // is what references, rename, and highlights consume.
        let hashkey_defs: HashMap<(&str, &HashKeyOwner), SymbolId> = self.symbols.iter()
            .filter_map(|sym| {
                if let SymbolDetail::HashKeyDef { owner, .. } = &sym.detail {
                    Some(((sym.name.as_str(), owner), sym.id))
                } else {
                    None
                }
            })
            .collect();
        let mut hashkey_resolutions: Vec<(usize, SymbolId)> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if r.resolves_to.is_some() {
                continue;
            }
            if let RefKind::HashKeyAccess { owner: Some(owner), .. } = &r.kind {
                if let Some(&sid) = hashkey_defs.get(&(r.target_name.as_str(), owner)) {
                    hashkey_resolutions.push((i, sid));
                }
            }
        }
        for (idx, sid) in hashkey_resolutions {
            self.refs[idx].resolves_to = Some(sid);
        }

        // Link DispatchCall refs → Handler symbols by (owner, name). A
        // DispatchCall whose owner couldn't be resolved at build time (e.g.
        // `$obj->emit('x')` where `$obj` type isn't known yet) stays
        // unlinked here and may be re-resolved by enrichment when the
        // cross-file receiver type becomes known.
        //
        // Unlike hash keys, multiple Handlers with the same (owner, name)
        // legitimately coexist (stacked registrations) — we link the ref
        // to the *first* def found so `resolves_to` has a single target,
        // and rely on `refs_to_symbol` walking all stacked defs separately
        // for features like references/rename.
        let handler_defs: HashMap<(&str, &HandlerOwner), SymbolId> = self.symbols.iter()
            .filter_map(|sym| {
                if let SymbolDetail::Handler { owner, .. } = &sym.detail {
                    Some(((sym.name.as_str(), owner), sym.id))
                } else {
                    None
                }
            })
            .collect();
        let mut handler_resolutions: Vec<(usize, SymbolId)> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if r.resolves_to.is_some() { continue; }
            if let RefKind::DispatchCall { owner: Some(owner), .. } = &r.kind {
                if let Some(&sid) = handler_defs.get(&(r.target_name.as_str(), owner)) {
                    handler_resolutions.push((i, sid));
                }
            }
        }
        for (idx, sid) in handler_resolutions {
            self.refs[idx].resolves_to = Some(sid);
        }

        // Refs by target name, and refs by resolved target SymbolId.
        // Same loop populates the start-point → call-ref-idx index
        // used by `method_call_invocant_class` to chase chain
        // receivers. Only MethodCall (whose span covers the whole
        // call expression) and FunctionCall (whose span covers just
        // the function-name node, but whose start point still
        // matches the outer call's invocant_span.start) refs go in.
        for (i, r) in self.refs.iter().enumerate() {
            self.refs_by_name
                .entry(r.target_name.clone())
                .or_default()
                .push(i);
            if let Some(sym_id) = r.resolves_to {
                self.refs_by_target.entry(sym_id).or_default().push(i);
            }
            if matches!(r.kind, RefKind::MethodCall { .. } | RefKind::FunctionCall { .. }) {
                // Smaller span (closer to the actual receiver) wins;
                // a tie keeps the earlier insertion. Method-call refs
                // are visited outer-first, so for a chain like
                // `Foo->new->m` the outer `m` and inner `Foo->new`
                // share a start point — keeping the smaller-span ref
                // points the index at the inner receiver. FunctionCall
                // refs (just the function-name span) are naturally
                // narrower than the enclosing MethodCall, so they win
                // the same way.
                let cur = self.call_ref_by_start.get(&r.span.start).copied();
                let take = match cur {
                    None => true,
                    Some(prev) => {
                        let prev_span = self.refs[prev].span;
                        // Smaller span (closer to the receiver) wins.
                        // Tie-breaker: prefer FunctionCall over MethodCall
                        // when at the same start, since FunctionCall is
                        // narrower (just the function-name span).
                        let new_smaller = (r.span.end.row, r.span.end.column)
                            < (prev_span.end.row, prev_span.end.column);
                        new_smaller
                    }
                };
                if take {
                    self.call_ref_by_start.insert(r.span.start, i);
                }
            }
        }
    }

    /// All refs that resolve to this symbol — O(1) lookup via the index.
    /// Callers typically combine this with a kind filter.
    pub fn refs_to_symbol(&self, sym_id: SymbolId) -> &[usize] {
        self.refs_by_target.get(&sym_id).map(|v| v.as_slice()).unwrap_or(&[])
    }

    // ---- Query methods ----

    /// Find the innermost scope containing a point.
    pub fn scope_at(&self, point: Point) -> Option<ScopeId> {
        let mut best: Option<(ScopeId, usize)> = None; // (id, span_size)
        for scope in &self.scopes {
            if contains_point(&scope.span, point) {
                let size = span_size(&scope.span);
                if best.is_none() || size <= best.unwrap().1 {
                    best = Some((scope.id, size));
                }
            }
        }
        best.map(|(id, _)| id)
    }

    /// Walk the scope chain from a scope upward to file root.
    pub fn scope_chain(&self, start: ScopeId) -> Vec<ScopeId> {
        let mut chain = Vec::new();
        let mut current = Some(start);
        while let Some(id) = current {
            chain.push(id);
            current = self.scopes[id.0 as usize].parent;
        }
        chain
    }

    /// Get the scope struct by ID.
    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0 as usize]
    }

    /// Get the symbol struct by ID.
    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0 as usize]
    }

    /// Find all symbols visible at a point (walks scope chain).
    pub fn visible_symbols(&self, point: Point) -> Vec<&Symbol> {
        let scope = match self.scope_at(point) {
            Some(s) => s,
            None => return Vec::new(),
        };
        let chain = self.scope_chain(scope);
        let mut result = Vec::new();
        for scope_id in &chain {
            if let Some(sym_ids) = self.symbols_by_scope.get(scope_id) {
                for sid in sym_ids {
                    let sym = &self.symbols[sid.0 as usize];
                    // Symbol must be declared before the point (or be a sub/package/class)
                    if sym.span.start <= point || matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class) {
                        result.push(sym);
                    }
                }
            }
        }
        result
    }

    /// Resolve a variable name to its declaration at a given point.
    /// Returns the innermost-scope match (Perl lexical scoping).
    pub fn resolve_variable(&self, name: &str, point: Point) -> Option<&Symbol> {
        let scope = self.scope_at(point)?;
        let chain = self.scope_chain(scope);
        for scope_id in &chain {
            if let Some(sym_ids) = self.symbols_by_scope.get(scope_id) {
                for sid in sym_ids {
                    let sym = &self.symbols[sid.0 as usize];
                    if sym.name == name
                        && matches!(sym.kind, SymKind::Variable | SymKind::Field)
                        && sym.span.start <= point
                    {
                        return Some(sym);
                    }
                }
            }
        }
        None
    }

    /// Raw Variable+InferredType lookup — returns the latest in-scope
    /// witness for `var_name` before `point`, with no framework rules,
    /// no branch fold, no narrowing.
    ///
    /// **NOT the canonical type query.** Use `inferred_type_via_bag`
    /// for any consumer that wants the answer the rest of the LSP
    /// uses — this method exists for two narrow purposes:
    ///
    /// 1. Internal "did we explicitly assign a type to this variable
    ///    yet?" checks (e.g. `resolve_method_call_types` early-out)
    ///    that need the bare seed state, not the bag's reduced answer
    ///    (which can be non-`None` from rep observations alone).
    /// 2. Tests that assert on raw seed state.
    ///
    /// If you find a third use case, prefer `inferred_type_via_bag`
    /// and only fall back here if you have a concrete reason that
    /// would survive a code review.
    pub fn inferred_type(&self, var_name: &str, point: Point) -> Option<&InferredType> {
        use crate::witnesses::{WitnessAttachment, WitnessPayload};
        let mut best: Option<(&InferredType, Point)> = None;
        for w in self.witnesses.all() {
            let WitnessAttachment::Variable { name, scope } = &w.attachment else { continue };
            if name != var_name { continue; }
            let scope_obj = &self.scopes[scope.0 as usize];
            if !contains_point(&scope_obj.span, point) { continue; }
            if w.span.start > point { continue; }
            let WitnessPayload::InferredType(t) = &w.payload else { continue };
            if best.is_none() || w.span.start > best.unwrap().1 {
                best = Some((t, w.span.start));
            }
        }
        best.map(|(t, _)| t)
    }

    /// Query the witness bag via the reducer registry for a variable at
    /// a point, falling back to the legacy `inferred_type()` when the bag
    /// has nothing. Returns owned `InferredType` because the reducer may
    /// synthesize a value not stored anywhere.
    ///
    /// The bag is the canonical store: `push_type_constraint` (TC
    /// shape), `call_bindings` propagation, framework accessor
    /// synthesis, and cross-file enrichment all push witnesses here.
    /// `inferred_type` reads the same Variable+InferredType slice
    /// without applying reducer rules.
    pub fn inferred_type_via_bag(&self, var_name: &str, point: Point) -> Option<InferredType> {
        self.inferred_type_via_bag_ctx(var_name, point, None)
    }

    /// As `inferred_type_via_bag`, but with a `ModuleIndex` so a variable whose
    /// value is a cross-file method chain (`my $x = Foo->new->bar`) resolves —
    /// the chase keeps the index when it crosses the `Variable` edge instead of
    /// dead-ending. Pass the index from query-time callers (hover/completion);
    /// the bare wrapper keeps `None` for build-time / single-file callers. Both
    /// now carry `package_parents` (previously dropped to empty).
    pub fn inferred_type_via_bag_ctx(
        &self,
        var_name: &str,
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        let scope = self.scope_at(point)?;
        crate::witnesses::query_variable_type(
            &self.witnesses,
            &self.scopes,
            &self.package_framework,
            &self.package_parents,
            &self.app_surface_consumers,
            module_index,
            var_name,
            scope,
            point,
        )
    }

    /// Resolve the inferred return type of a method call by its ref index
    /// (into `refs`). Reads the `Expression(refidx)` witnesses seeded by
    /// the builder; `module_index` lets cross-file `MethodOnClass` edges
    /// (e.g. `$r->get(...)` where `get` lives in `Mojolicious::Routes`)
    /// chase through the registry's recursive walker.
    ///
    /// This is the piece that makes `$r->get('/x')->to(...)` fold across
    /// chain hops without needing an intermediate variable.
    #[allow(dead_code)] // documented type-query entry point; CLAUDE.md
    pub fn method_call_return_type_via_bag(
        &self,
        ref_idx: usize,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };

        let att = WitnessAttachment::Expression(crate::witnesses::RefIdx(ref_idx as u32));
        let reg = ReducerRegistry::with_defaults();
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index,
            package_parents: &self.package_parents,
            app_surface_consumers: &self.app_surface_consumers,
            };
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint: None,
            receiver: None,
            context: Some(&ctx),
        };
        match reg.query(&self.witnesses, &q) {
            ReducedValue::Type(t) => {
                // If the return is FirstParam, surface it as the
                // ClassName of the enclosing package — callers chain
                // against a concrete class, not a role.
                if let InferredType::FirstParam { package } = t {
                    Some(InferredType::ClassName(package))
                } else {
                    Some(t)
                }
            }
            _ => None,
        }
    }

    /// Registry query against `Expr(span)` — the bag attachment the
    /// builder seeds at every meaningful expression node (literals,
    /// variable reads, method-call invocants, ternaries). Mirror of the
    /// build-time `Builder::bag_query_expr_span`. `module_index` lets a
    /// recorded `Edge` (e.g. a method-call invocant pointing at an
    /// `Expression(refidx)`) chase cross-file.
    fn bag_query_expr_span(
        &self,
        span: Span,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let att = WitnessAttachment::Expr(span);
        let reg = ReducerRegistry::with_defaults();
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index,
            package_parents: &self.package_parents,
            app_surface_consumers: &self.app_surface_consumers,
        };
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint: None,
            receiver: None,
            context: Some(&ctx),
        };
        match reg.query(&self.witnesses, &q) {
            ReducedValue::Type(t) => Some(t),
            _ => None,
        }
    }

    /// The type of the expression occupying `span`, resolved tree-free
    /// from the bag. This is the single query-time entry that
    /// `method_call_invocant_class` and `resolve_expression_type` both
    /// route through: structure was discovered once in the builder
    /// (recorded as `Expr(span)` witnesses + the `Expression(refidx)`
    /// call axis), and every consumer reads it back by span.
    ///
    /// Resolution order:
    /// 1. A call ref starting at and contained in `span` (chain /
    ///    function-call receiver) — its bag-resolved return type. This
    ///    arm re-derives at enrichment, so cross-file chain receivers
    ///    whose class only becomes known once other modules load still
    ///    resolve here.
    /// 2. The `Expr(span)` witness the builder recorded for the
    ///    expression (variable reads via `Edge(Variable)`, `$arr[N]`
    ///    projections, ternaries, literals).
    pub fn expr_type_at_span(
        &self,
        span: Span,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        // A call whose span IS this expression — its return type. The
        // exact-span match is what distinguishes "the value of
        // `$f->get_bar()->get_name()`" (the outer call's return) from
        // "the inner receiver `$f->get_bar()`" (which has its own,
        // narrower span). `call_ref_by_start` deliberately points at the
        // innermost receiver, so we can't use it here — we want the ref
        // that exactly spans the queried expression.
        if let Some((recv_idx, kind)) = self.refs.iter().enumerate().find_map(|(i, r)| {
            if r.span == span && matches!(r.kind, RefKind::MethodCall { .. } | RefKind::FunctionCall { .. }) {
                Some((i, &r.kind))
            } else {
                None
            }
        }) {
            match kind {
                RefKind::MethodCall { .. } => {
                    if let Some(t) =
                        self.method_call_return_type_via_bag(recv_idx, module_index)
                    {
                        return Some(t);
                    }
                }
                RefKind::FunctionCall { .. } => {
                    if let Some(t) = self.sub_return_type_at_arity(
                        &self.refs[recv_idx].target_name,
                        Some(0),
                    ) {
                        return Some(t);
                    }
                }
                _ => {}
            }
        }
        self.bag_query_expr_span(span, module_index)
    }

    /// Resolve a sub's return type at a call site given the caller's arg
    /// count. Queries the arity-dispatch reducer; if no arity fact
    /// exists, falls back to `sub_return_type` (declared /
    /// inferred-from-returns).
    ///
    /// `arity` is the number of *additional* args passed after the
    /// invocant on methods (or simply the arg count for plain subs).
    pub fn sub_return_type_at_arity(
        &self,
        sub_name: &str,
        arity: Option<u32>,
    ) -> Option<InferredType> {
        let ctx = crate::witnesses::BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index: None,
            package_parents: &self.package_parents,
            app_surface_consumers: &self.app_surface_consumers,
            };
        crate::witnesses::query_sub_return_type(
            &self.witnesses,
            &self.symbols,
            sub_name,
            arity,
            None,
            Some(&ctx),
        )
    }

    /// List hash keys that have been written to on instances of `class`.
    /// Powers dynamic-key completion: `$self->{` completes
    /// with both `has`-declared keys and keys observed as write
    /// targets across the class's methods.
    #[allow(dead_code)] // documented type-query entry point; CLAUDE.md
    pub fn mutated_keys_on_class(&self, class: &str) -> Vec<String> {
        use crate::witnesses::{WitnessAttachment, WitnessPayload};
        let mut out: Vec<String> = Vec::new();
        for w in self.witnesses.all() {
            if let WitnessAttachment::HashKey { owner, name } = &w.attachment {
                let matches_class = match owner {
                    HashKeyOwner::Class(c) if c == class => true,
                    HashKeyOwner::Sub { package: Some(p), .. } if p == class => true,
                    _ => false,
                };
                if !matches_class {
                    continue;
                }
                if matches!(
                    &w.payload,
                    WitnessPayload::Fact { family, .. } if family == "mutation"
                ) && !out.contains(name)
                {
                    out.push(name.clone());
                }
            }
        }
        out
    }

    /// Get the return type of a named sub/method (local definitions
    /// only). Routes through the bag — `Symbol(sid)` writeback witness
    /// is the post-field-deletion authority. Returns owned because
    /// the value is synthesized by the reducer, not stored.
    pub fn sub_return_type_local(&self, name: &str) -> Option<InferredType> {
        for sym in &self.symbols {
            if sym.name == name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if let Some(t) = self.symbol_return_type_via_bag(sym.id, None) {
                    return Some(t);
                }
            }
        }
        None
    }

    /// Get the return type of a named sub/method. Local definitions
    /// first (via the bag's `Symbol(sym_id)` writeback), then imported
    /// sub returns (resolved lazily through `query_sub_return_type`'s
    /// walk of `module_index.find_exporters` into the cached module's
    /// own `Symbol(_)` witnesses).
    #[allow(dead_code)] // public type-query API; used by tooling/tests
    pub fn sub_return_type(&self, name: &str) -> Option<InferredType> {
        self.sub_return_type_at_arity(name, None)
    }

    /// Provenance of a symbol's return type — `Inferred` by default,
    /// `PluginOverride` for plugin-declared overrides. Always returns
    /// a value (never `None`) so debug tooling can ask "where did this
    /// come from?" without branching on missingness.
    #[allow(dead_code)] // debug-introspection accessor; consumed by tests
    pub fn return_type_provenance(&self, sym_id: SymbolId) -> TypeProvenance {
        self.type_provenance
            .get(&sym_id)
            .cloned()
            .unwrap_or(TypeProvenance::Inferred)
    }

    /// Resolve call bindings for imported functions and inject
    /// synthetic HashKeyDef symbols. Walks `self.imports` against
    /// `module_index` to derive each imported sub's return type and
    /// hash-key set, reaching cross-file `Symbol(_)` witnesses through
    /// `BagContext.module_index` directly. Call after building, when the
    /// module index is available.
    pub fn enrich_imported_types_with_keys(
        &mut self,
        module_index: Option<&ModuleIndex>,
    ) {
        // Truncate back to baseline so repeated enrichment doesn't
        // accumulate duplicates. Enrichment pushes Variable witnesses
        // via `push_type_constraint` and synthetic symbols + witnesses
        // for imported-hash-key completion.
        self.symbols.truncate(self.base_symbol_count);
        self.witnesses.truncate(self.base_witness_count);
        self.refs.truncate(self.base_ref_count);

        // Promote provisional dispatches whose receiver `isa` the verb's
        // target class (resolved cross-file here, where the module index is
        // available). This is the receiver-driven dispatch path — a
        // `$minion->enqueue('T')` lights up by the receiver's type, not by
        // the file's `use` statements.
        self.promote_provisional_dispatches(module_index);

        // Build the import → exported-name map inline from
        // `self.imports` + `module_index`. `imported_hash_keys` is
        // the only piece still needed by enrichment; imported return
        // types are reached lazily by `query_sub_return_type` walking
        // `module_index.find_exporters(name)`.
        let mut imported_hash_keys: HashMap<String, Vec<String>> = HashMap::new();
        let mut imported_returns: HashMap<String, InferredType> = HashMap::new();
        if let Some(idx) = module_index {
            for import in &self.imports {
                let Some(cached) = idx.get_cached(&import.module_name) else { continue };
                for sym in &cached.analysis.symbols {
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                        continue;
                    }
                    if !cached.analysis.export.iter().any(|n| n == &sym.name)
                        && !cached.analysis.export_ok.iter().any(|n| n == &sym.name)
                    {
                        continue;
                    }
                    if matches!(sym.detail, SymbolDetail::Sub { .. }) {
                        if let Some(ty) = cached.analysis.symbol_return_type_via_bag(sym.id, None) {
                            imported_returns.insert(sym.name.clone(), ty);
                        }
                    }
                    if let Some(sub_info) = cached.sub_info(&sym.name) {
                        let hk = sub_info.hash_keys();
                        if !hk.is_empty() {
                            imported_hash_keys.insert(sym.name.clone(), hk.to_vec());
                        }
                    }
                }
            }
        }

        // Push call-binding TCs for imports whose return type the
        // cross-file scan resolved. Same shape the local-sub path
        // produces (`propagate_call_bindings_to_constraints`).
        let mut to_push: Vec<TypeConstraint> = Vec::new();
        for binding in &self.call_bindings {
            if self.sub_return_type_local(&binding.func_name).is_some()
                || builtin_return_type(&binding.func_name).is_some()
            {
                continue;
            }
            if let Some(rt) = imported_returns.get(&binding.func_name) {
                to_push.push(TypeConstraint {
                    variable: binding.variable.clone(),
                    scope: binding.scope,
                    constraint_span: binding.span,
                    inferred_type: rt.clone(),
                });
            }
        }
        for tc in to_push {
            self.push_type_constraint(tc);
        }

        // Inject synthetic HashKeyDef symbols for imported functions' hash keys.
        // Imported subs have no local package — package=None mirrors the
        // HashKeyAccess fixup's default for imported bindings.
        for (func_name, keys) in &imported_hash_keys {
            let owner = HashKeyOwner::Sub { package: None, name: func_name.clone() };
            for key_name in keys {
                let id = SymbolId(self.symbols.len() as u32);
                let zero_span = Span {
                    start: Point { row: 0, column: 0 },
                    end: Point { row: 0, column: 0 },
                };
                self.symbols.push(Symbol {
                    id,
                    name: key_name.clone(),
                    kind: SymKind::HashKeyDef,
                    span: zero_span,
                    selection_span: zero_span,
                    scope: ScopeId(0),
                    package: None,
                    detail: SymbolDetail::HashKeyDef {
                        owner: owner.clone(),
                        is_dynamic: false,
                    },
                    namespace: Namespace::Language,
                    outline_label: None,
                });
            }
        }

        // HashKeyAccess owner fixup for imports: the builder's pass
        // only ran for bindings where the func's return type was
        // known at build time. Cross-file return types come in here,
        // so the consumer-side `$cfg = Lib::get_config(); $cfg->{host}`
        // access gets its owner set to Sub{None, "get_config"},
        // matching the synthetic HashKeyDef we just injected.
        let imported_keyed_subs: std::collections::HashSet<String> = imported_hash_keys
            .keys()
            .cloned()
            .collect();
        let binding_by_var: std::collections::HashMap<String, String> = self.call_bindings.iter()
            .filter_map(|b| {
                let bare = b.func_name.rsplit("::").next().unwrap_or(&b.func_name).to_string();
                if imported_keyed_subs.contains(&bare) {
                    Some((b.variable.clone(), bare))
                } else {
                    None
                }
            })
            .collect();
        if !binding_by_var.is_empty() {
            for r in &mut self.refs {
                if let RefKind::HashKeyAccess { ref var_text, ref mut owner } = r.kind {
                    if owner.is_some() && !matches!(owner, Some(HashKeyOwner::Variable { .. })) {
                        continue;
                    }
                    if let Some(func_name) = binding_by_var.get(var_text.as_str()) {
                        if let Some(keys) = imported_hash_keys.get(func_name) {
                            if keys.iter().any(|k| k == &r.target_name) {
                                *owner = Some(HashKeyOwner::Sub {
                                    package: None,
                                    name: func_name.clone(),
                                });
                                r.resolves_to = None;
                            }
                        }
                    }
                }
            }
        }

        // Deferred HashKeyAccess owner fix for chain-receiver
        // method calls — see `fix_chain_receiver_hash_key_owners`.
        // Enrichment runs it with module_index so cross-file
        // receiver types resolve; the same routine runs from
        // `finalize_post_walk` with module_index=None for the
        // in-file-resolvable case (chain recursion via
        // `call_ref_by_start` doesn't need module_index).
        self.fix_chain_receiver_hash_key_owners(module_index);

        // Cross-file inheritance edges. Local writeback emits
        // `MethodOnClass(child, m) → Edge(MethodOnClass(parent, m))`
        // for every method `m` declared on a *local* parent. When
        // the parent class lives in another file (or its methods
        // do, via further parent chaining), we read the cached
        // analysis here and project the same edge shape into the
        // local bag. The registry's edge-chase then follows
        // `MethodOnClass(child, m) → MethodOnClass(parent_cross, m)`
        // and re-enters the cached parent's bag via the existing
        // cross-file primary lookup in `query_rec`.
        if let Some(idx) = module_index {
            use crate::witnesses::{Witness, WitnessAttachment, WitnessPayload, WitnessSource};
            let zero = Span {
                start: Point { row: 0, column: 0 },
                end: Point { row: 0, column: 0 },
            };
            // Snapshot to avoid double-mutable-borrow when pushing.
            let parents_snapshot: Vec<(String, Vec<String>)> = self
                .package_parents
                .iter()
                .map(|(c, ps)| (c.clone(), ps.clone()))
                .collect();
            for (child, parents) in &parents_snapshot {
                // First-parent-wins per method, mirroring Perl's
                // default DFS-MRO. Aligned with the local-parent
                // edge emission in `write_back_sub_return_types`.
                let mut emitted_for_child: std::collections::HashSet<String> =
                    std::collections::HashSet::new();
                for parent in parents {
                    if parent == child {
                        continue;
                    }
                    let Some(cached) = idx.get_cached(parent) else { continue };
                    for sym in &cached.analysis.symbols {
                        if sym.package.as_deref() != Some(parent.as_str()) {
                            continue;
                        }
                        if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                            continue;
                        }
                        if !emitted_for_child.insert(sym.name.clone()) {
                            continue;
                        }
                        self.witnesses.push(Witness {
                            attachment: WitnessAttachment::MethodOnClass {
                                class: child.clone(),
                                name: sym.name.clone(),
                            },
                            source: WitnessSource::Enrichment("inheritance_cross".to_string()),
                            payload: WitnessPayload::Edge(WitnessAttachment::MethodOnClass {
                                class: parent.clone(),
                                name: sym.name.clone(),
                            }),
                            span: zero,
                        });
                    }
                }
            }
        }

        self.resolve_method_call_types(module_index);
        self.rebuild_enrichment_indices();
    }

    /// Resolve method call bindings to type constraints.
    /// Called in local post-pass (None) and cross-file enrichment (Some(module_index)).
    fn resolve_method_call_types(&mut self, module_index: Option<&ModuleIndex>) {
        let bindings = self.method_call_bindings.clone();
        for binding in &bindings {
            // Skip if this variable already has a type at this point
            if self.inferred_type(&binding.variable, binding.span.start).is_some() {
                continue;
            }

            // Resolve invocant to class name
            let class_name = self.resolve_invocant_class(
                &binding.invocant_var,
                binding.scope,
                binding.span.start,
            );

            if let Some(cn) = class_name {
                if let Some(rt) = self.find_method_return_type(&cn, &binding.method_name, module_index, None) {
                    self.push_type_constraint(TypeConstraint {
                        variable: binding.variable.clone(),
                        scope: binding.scope,
                        constraint_span: binding.span,
                        inferred_type: rt,
                    });
                }
            }
        }
    }

    /// Push a `TypeConstraint` shape into the witness bag — a Variable
    /// `InferredType` witness plus a class-assertion observation when
    /// the type is a class identity. The bag is the single store; this
    /// helper exists so callers can keep the legible "I'm seeding a
    /// type constraint on $X" call shape rather than open-coding the
    /// witness construction. Builder has a parallel helper that does
    /// the same thing during the walk.
    pub(crate) fn push_type_constraint(&mut self, tc: TypeConstraint) {
        use crate::witnesses::{
            TypeObservation, Witness, WitnessAttachment, WitnessPayload, WitnessSource,
        };
        let TypeConstraint { variable, scope, constraint_span: span, inferred_type: ty } = tc;
        self.witnesses.push(Witness {
            attachment: WitnessAttachment::Variable { name: variable.clone(), scope },
            source: WitnessSource::Builder("type_constraint".into()),
            payload: WitnessPayload::InferredType(ty.clone()),
            span: Span { start: span.start, end: span.start },
        });
        match ty {
            InferredType::ClassName(n) => {
                self.witnesses.push(Witness {
                    attachment: WitnessAttachment::Variable { name: variable, scope },
                    source: WitnessSource::Builder("type_constraint".into()),
                    payload: WitnessPayload::Observation(TypeObservation::ClassAssertion(n)),
                    span,
                });
            }
            InferredType::FirstParam { package } => {
                self.witnesses.push(Witness {
                    attachment: WitnessAttachment::Variable { name: variable, scope },
                    source: WitnessSource::Builder("type_constraint".into()),
                    payload: WitnessPayload::Observation(TypeObservation::FirstParamInMethod {
                        package,
                    }),
                    span,
                });
            }
            _ => {}
        }
    }

    /// Turn `provisional_dispatches` into real `DispatchCall` refs for the
    /// candidates whose receiver class `isa` the verb's target. Runs inside
    /// `enrich_imported_types_with_keys`, after `refs` was truncated back to
    /// `base_ref_count`, so it's idempotent across repeated enrichment.
    ///
    /// Receiver resolution is two-tier: the build-time `receiver_class` hint
    /// (a locally-constructed `My::Minion->new`, a typed `has`-attribute) when
    /// present, else cross-file resolution of the call's invocant via
    /// `method_call_invocant_class` with the module index — which lights up
    /// helper-/attribute-returned receivers (`$c->minion->enqueue`,
    /// `$self->_minion->enqueue`) that only type once other modules are in
    /// scope.
    fn promote_provisional_dispatches(&mut self, module_index: Option<&ModuleIndex>) {
        if self.provisional_dispatches.is_empty() {
            return;
        }
        // Dedup against refs that already exist at the same site (the
        // parse-time plugin emit, in files whose triggers fired).
        let mut existing: std::collections::HashSet<(Point, Point, String, String)> =
            std::collections::HashSet::new();
        for r in &self.refs {
            if let RefKind::DispatchCall { dispatcher, .. } = &r.kind {
                existing.insert((r.span.start, r.span.end, dispatcher.clone(), r.target_name.clone()));
            }
        }
        let candidates = std::mem::take(&mut self.provisional_dispatches);
        let mut new_refs: Vec<Ref> = Vec::new();
        for c in &candidates {
            // Prefer the build-time hint (locally-constructed receiver);
            // otherwise resolve the call's invocant cross-file with the
            // index — this is what lights up `$app->minion->enqueue(...)`
            // and `$self->_minion->enqueue(...)`, whose receiver type only
            // exists once other modules are in scope.
            let recv = c.receiver_class.clone().or_else(|| {
                self.refs
                    .iter()
                    .find(|r| {
                        r.span == c.call_span
                            && r.target_name == c.dispatcher
                            && matches!(r.kind, RefKind::MethodCall { .. })
                    })
                    .and_then(|r| self.method_call_invocant_class(r, module_index))
            });
            let Some(recv) = recv else { continue };
            if !self.class_isa(&recv, &c.target_class, module_index) {
                continue;
            }
            let key = (c.span.start, c.span.end, c.dispatcher.clone(), c.name.clone());
            if !existing.insert(key) {
                continue;
            }
            new_refs.push(Ref {
                kind: RefKind::DispatchCall {
                    dispatcher: c.dispatcher.clone(),
                    owner: Some(HandlerOwner::Class(c.owner_class.clone())),
                },
                span: c.span,
                scope: self.scope_at(c.span.start).unwrap_or(ScopeId(0)),
                target_name: c.name.clone(),
                access: AccessKind::Read,
                resolves_to: None,
            });
        }
        self.refs.extend(new_refs);
        self.provisional_dispatches = candidates;
    }

    /// Does `class` equal `target` or descend from it? Walks local
    /// `package_parents` first, then the cross-file inheritance graph via
    /// `module_index.parents_cached`. Cycle-guarded by `seen`; `budget` caps
    /// TOTAL classes visited (not ancestry depth) — a backstop against a
    /// pathological graph, set well above any real MRO. `parents_cached` is
    /// keyed by module name, which coincides with the class name here.
    fn class_isa(&self, class: &str, target: &str, module_index: Option<&ModuleIndex>) -> bool {
        if class == target {
            return true;
        }
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut stack: Vec<String> = vec![class.to_string()];
        let mut budget = 0;
        while let Some(cur) = stack.pop() {
            if budget > 200 {
                break;
            }
            budget += 1;
            if !seen.insert(cur.clone()) {
                continue;
            }
            if cur == target {
                return true;
            }
            if let Some(parents) = self.package_parents.get(&cur) {
                for p in parents {
                    stack.push(p.clone());
                }
            }
            if let Some(idx) = module_index {
                for p in idx.parents_cached(&cur) {
                    stack.push(p);
                }
            }
        }
        false
    }

    /// Rebuild indices affected by enrichment (type constraints + symbols +
    /// refs_by_target + HashKeyAccess linkage).
    ///
    /// Enrichment injects synthetic HashKeyDef symbols for imported subs and
    /// clears `resolves_to` on HashKeyAccess refs that now have a matching
    /// owner. This method re-runs the same `(target_name, owner)` linker that
    /// `build_indices` uses, so `refs_by_target` stays accurate after a
    /// cross-file hash-key binding resolves.
    fn rebuild_enrichment_indices(&mut self) {
        self.symbols_by_name.clear();
        self.symbols_by_scope.clear();
        for sym in &self.symbols {
            self.symbols_by_name
                .entry(sym.name.clone())
                .or_default()
                .push(sym.id);
            self.symbols_by_scope
                .entry(sym.scope)
                .or_default()
                .push(sym.id);
        }

        // Re-link HashKeyAccess refs to (possibly newly-injected) HashKeyDef
        // symbols, mirroring build_indices's logic.
        let hashkey_defs: HashMap<(String, HashKeyOwner), SymbolId> = self.symbols.iter()
            .filter_map(|sym| {
                if let SymbolDetail::HashKeyDef { owner, .. } = &sym.detail {
                    Some(((sym.name.clone(), owner.clone()), sym.id))
                } else {
                    None
                }
            })
            .collect();
        let mut hashkey_resolutions: Vec<(usize, SymbolId)> = Vec::new();
        for (i, r) in self.refs.iter().enumerate() {
            if r.resolves_to.is_some() {
                continue;
            }
            if let RefKind::HashKeyAccess { owner: Some(owner), .. } = &r.kind {
                if let Some(&sid) = hashkey_defs.get(&(r.target_name.clone(), owner.clone())) {
                    hashkey_resolutions.push((i, sid));
                }
            }
        }
        for (idx, sid) in hashkey_resolutions {
            self.refs[idx].resolves_to = Some(sid);
        }

        // Refresh refs_by_name + refs_by_target against the current refs.
        self.refs_by_name.clear();
        self.refs_by_target.clear();
        for (i, r) in self.refs.iter().enumerate() {
            self.refs_by_name
                .entry(r.target_name.clone())
                .or_default()
                .push(i);
            if let Some(sym_id) = r.resolves_to {
                self.refs_by_target.entry(sym_id).or_default().push(i);
            }
        }
    }

    /// Resolve the type of an arbitrary CST expression node.
    ///
    /// Recursive tree walk that composes existing atoms:
    /// - `scalar` → `inferred_type(var_text, point)`
    /// - bareword (class name) → `Object(ClassName)`
    /// - `method_call_expression` → resolve invocant → find method → return type
    /// - `function_call_expression` → `sub_return_type(name)`
    /// - `hash_element_expression` → resolve base (for chaining)
    ///
    /// Used at query time (completion, goto-def, hover) — not during the build walk.
    pub fn resolve_expression_type(
        &self,
        node: tree_sitter::Node,
        source: &[u8],
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        // Tree-free first: the builder recorded the type of every
        // meaningful expression at its span (`Expr(span)` + the
        // `Expression(refidx)` call axis). Read it back by span — the
        // same chase `method_call_invocant_class` uses. The node-kind
        // walk below is the degradation path for the raw-cursor /
        // incomplete-ERROR case completion hits where no witness was
        // recorded (a node that never existed at build time).
        let span = Span {
            start: node.start_position(),
            end: node.end_position(),
        };
        if let Some(t) = self.expr_type_at_span(span, module_index) {
            return Some(t);
        }
        let point = node.start_position();
        match node.kind() {
            "scalar" => {
                let text = node.utf8_text(source).ok()?;
                // Route scalar-type resolution through the witness
                // bag so framework / branch / arity rules refine the
                // answer. Legacy `inferred_type` is used as fallback.
                self.inferred_type_via_bag(text, point)
            }
            "package" | "bareword" => {
                // Bareword = class name
                let text = node.utf8_text(source).ok()?;
                Some(InferredType::ClassName(text.to_string()))
            }
            "array_element_expression" => {
                // `$arr[N]` — look up `@arr`'s Sequence shape via the
                // bag, project the index. The bag answer is whatever
                // the walker's last `push` / `my @arr = (...)`
                // contribution left on `Variable{"@arr", scope}`.
                let arr = node.child_by_field_name("array")?;
                let name = arr
                    .named_child(0)
                    .and_then(|n| n.utf8_text(source).ok())?;
                let idx_node = node.child_by_field_name("index")?;
                let idx: i32 = idx_node.utf8_text(source).ok()?.parse().ok()?;
                let arr_var = format!("@{}", name);
                self.inferred_type_via_bag(&arr_var, point)
                    .and_then(|t| t.element_at(idx).cloned())
            }
            "method_call_expression" => {
                let invocant_node = node.child_by_field_name("invocant")?;
                let invocant_type = self.resolve_expression_type(invocant_node, source, module_index)?;
                let class_name = invocant_type.class_name()?;
                let method_name = node.child_by_field_name("method")?;
                let method_text = method_name.utf8_text(source).ok()?;
                // Constructor call returns Object(ClassName)
                if method_text == "new" {
                    return Some(InferredType::ClassName(class_name.to_string()));
                }
                let arg_count = self.count_call_args(node);
                self.find_method_return_type(class_name, method_text, module_index, Some(arg_count))
            }
            "function_call_expression" | "ambiguous_function_call_expression" => {
                let func_node = node.child_by_field_name("function")?;
                let name = func_node.utf8_text(source).ok()?;
                // Route through the arity-aware helper so fluent
                // arity dispatch (`return X unless @_`) can pick the
                // right branch when the caller's arg count is known.
                let arg_count = self.count_call_args(node) as u32;
                self.sub_return_type_at_arity(name, Some(arg_count))
                    .or_else(|| builtin_return_type(name))
            }
            "func1op_call_expression" | "func0op_call_expression" => {
                let name = node.child(0)?.utf8_text(source).ok()?;
                builtin_return_type(name)
            }
            "hash_element_expression" => {
                // For chaining: resolve the base expression
                let base = node.named_child(0)?;
                self.resolve_expression_type(base, source, module_index)
            }
            _ => None,
        }
    }

    /// Resolve the hash key owner from a tree node at a given point.
    ///
    /// Finds the enclosing `hash_element_expression`, resolves its base expression
    /// type, and returns the appropriate `HashKeyOwner`. Used by goto-def,
    /// references, completion, and highlights for chained hash access like
    /// `$calc->get_self->get_config->{host}`.
    pub fn resolve_hash_owner_from_tree(
        &self,
        tree: &tree_sitter::Tree,
        source: &[u8],
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<HashKeyOwner> {
        let hash_elem = tree.root_node()
            .descendant_for_point_range(point, point)
            .and_then(|n| find_ancestor(n, "hash_element_expression"))?;
        let base = hash_elem.named_child(0)?;
        let ty = self.resolve_expression_type(base, source, module_index)?;

        // Extract the function/method name for Sub owner
        let sub_name = match base.kind() {
            "method_call_expression" => base.child_by_field_name("method")
                .and_then(|n| n.utf8_text(source).ok())
                .map(|s| s.to_string()),
            "function_call_expression" | "ambiguous_function_call_expression" =>
                base.child_by_field_name("function")
                    .and_then(|n| n.utf8_text(source).ok())
                    .map(|s| s.to_string()),
            _ => None,
        };

        if let Some(name) = sub_name {
            // Try each package the sub might belong to. In practice, the sub
            // is either defined locally (package comes from its Symbol) or
            // imported (package = None, matching the enrichment synthetic def).
            let mut candidate_owners: Vec<HashKeyOwner> = Vec::new();
            for sym in &self.symbols {
                if (sym.kind == SymKind::Sub || sym.kind == SymKind::Method) && sym.name == name {
                    candidate_owners.push(HashKeyOwner::Sub {
                        package: sym.package.clone(),
                        name: name.clone(),
                    });
                }
            }
            // Fallback: None-package owner (matches imported synthetic defs).
            candidate_owners.push(HashKeyOwner::Sub { package: None, name: name.clone() });
            for sub_owner in candidate_owners {
                if !self.hash_key_defs_for_owner(&sub_owner).is_empty() {
                    return Some(sub_owner);
                }
            }
        }

        // Hash-key context: read `hash_key_class()` (= row-class
        // for Parametric, dispatch class else) instead of bare
        // `class_name()`. CLAUDE.md invariant #10 — never special-
        // case for a particular shape; the type already carries
        // the per-axis answer. For non-Parametric this is
        // equivalent to `class_name()`; for Parametric it's the
        // crucial narrowing to the type's row-class arg.
        if let Some(cn) = ty.hash_key_class() {
            return Some(HashKeyOwner::Class(cn.to_string()));
        }

        None
    }

    /// Check if cursor is on a bareword or string inside call arguments.
    /// Returns the text if so (could be a named arg key).
    fn call_arg_key_at(&self, tree: &tree_sitter::Tree, source: &[u8], point: Point) -> Option<String> {
        let node = tree.root_node().descendant_for_point_range(point, point)?;
        let key_node = match node.kind() {
            "autoquoted_bareword" => node,
            "string_literal" | "interpolated_string_literal" => node,
            _ => {
                let parent = node.parent()?;
                if matches!(parent.kind(), "string_literal" | "interpolated_string_literal") {
                    parent
                } else {
                    return None;
                }
            }
        };
        let text = key_node.utf8_text(source).ok()?;
        if (text.starts_with('\'') || text.starts_with('"')) && text.len() >= 2 {
            Some(text[1..text.len()-1].to_string())
        } else if text.starts_with('\'') || text.starts_with('"') {
            None
        } else {
            Some(text.to_string())
        }
    }

    /// Bag-routed query: "what does `Symbol(sym_id)` return at this
    /// arity?" Runs through the full reducer registry — Plugin
    /// overrides dominate, then arity dispatch, then `SubReturnReducer`
    /// (which claims plain writeback-pushed `InferredType` witnesses).
    /// Returns `None` when nothing in the bag answers.
    pub(crate) fn symbol_return_type_via_bag(
        &self,
        sym_id: SymbolId,
        arg_count: Option<usize>,
    ) -> Option<InferredType> {
        self.symbol_return_type_via_bag_ctx(sym_id, arg_count, None)
    }

    /// As `symbol_return_type_via_bag`, but with a `ModuleIndex` so the
    /// reducer chase can cross module boundaries — the sub's body may return
    /// a value typed by a cross-file method chain (`my $m = Foo->new->bar; …;
    /// return $m`). Without the index that chain dies at the boundary and the
    /// return type comes back `None`. Pass the index whenever the query has
    /// one (hover/completion against a cached module); the bare wrapper above
    /// keeps `None` for the many call sites that don't.
    pub(crate) fn symbol_return_type_via_bag_ctx(
        &self,
        sym_id: SymbolId,
        arg_count: Option<usize>,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let att = WitnessAttachment::Symbol(sym_id);
        let reg = ReducerRegistry::with_defaults();
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index,
            package_parents: &self.package_parents,
            app_surface_consumers: &self.app_surface_consumers,
            };
        // Default the arity hint from the sym's own param count when
        // the caller didn't supply one — the sym's params count IS its
        // native arity. Mojo writer (params=1) answers its
        // `(AtLeast(1), Receiver)` arm, getter (params=0) its
        // `(Empty, Concrete(_))` arm. Without this a writer's
        // UnionOnArgs would be unmatched at a None hint (AtLeast(1)
        // doesn't match None) and the query would silently return None.
        //
        // Default receiver = `ClassName(class)` so the writer's
        // `Receiver` placeholder evaluates to the natural fluent
        // answer at sym-introspection time.
        let resolved_arity = arg_count.map(|n| n as u32).or_else(|| {
            self.symbols
                .get(sym_id.0 as usize)
                .and_then(|s| match &s.detail {
                    SymbolDetail::Sub { params, .. } => Some(params.len() as u32),
                    _ => None,
                })
        });
        let receiver = self
            .symbols
            .get(sym_id.0 as usize)
            .and_then(|s| s.package.clone())
            .map(InferredType::ClassName);
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework: FrameworkFact::Plain,
            arity_hint: resolved_arity,
            receiver,
            context: Some(&ctx),
        };
        match reg.query(&self.witnesses, &q) {
            ReducedValue::Type(t) => Some(t),
            _ => None,
        }
    }

    /// Find a method's return type within a class/package, walking
    /// inheritance. Thin wrapper that queries the bag's class-keyed
    /// `MethodOnClass{class, method}` attachment with the caller's
    /// `arg_count` as arity hint. Inheritance composes through
    /// `package_parents` (carried in `BagContext`); cross-file
    /// classes resolve via `module_index`. No procedural ancestor
    /// walk; no procedural overload picking — the registry's
    /// `ReturnExprReducer` claims `MethodOnClass + ReturnExpr` and
    /// the structural-walk code in `query_rec` handles MRO.
    pub(crate) fn find_method_return_type(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
        arg_count: Option<usize>,
    ) -> Option<InferredType> {
        use crate::witnesses::{
            BagContext, FrameworkFact, ReducedValue, ReducerQuery, ReducerRegistry,
            WitnessAttachment,
        };
        let framework = self
            .package_framework
            .get(class_name)
            .copied()
            .unwrap_or(FrameworkFact::Plain);
        let att = WitnessAttachment::MethodOnClass {
            class: class_name.to_string(),
            name: method_name.to_string(),
        };
        let ctx = BagContext {
            scopes: &self.scopes,
            package_framework: &self.package_framework,
            module_index,
            package_parents: &self.package_parents,
            app_surface_consumers: &self.app_surface_consumers,
        };
        // Default receiver = `ClassName(class_name)` so that
        // `ReturnExpr::Receiver` evaluates correctly for class-keyed
        // method-return queries that don't have a specific
        // call-site invocant — Mojo `has 'title'` writer's
        // Receiver evaluates to ClassName(Bar), DBIC `find`'s
        // RowOf(Receiver) wraps the Parametric (when one is
        // supplied via the `arg_count` Some path elsewhere). Same
        // policy as `query_sub_return_type`'s class-fallback rule.
        let q = ReducerQuery {
            attachment: &att,
            point: None,
            framework,
            arity_hint: arg_count.map(|n| n as u32),
            receiver: Some(InferredType::ClassName(class_name.to_string())),
            context: Some(&ctx),
        };
        let reg = ReducerRegistry::with_defaults();
        if let ReducedValue::Type(t) = reg.query(&self.witnesses, &q) {
            return Some(t);
        }
        None
    }

    /// Count arguments in a method/function call expression (excluding invocant).
    pub(crate) fn count_call_args(&self, node: tree_sitter::Node) -> usize {
        let args = match node.child_by_field_name("arguments") {
            Some(a) => a,
            None => return 0,
        };
        match args.kind() {
            "parenthesized_expression" | "list_expression" => args.named_child_count(),
            _ => 1, // single argument
        }
    }

    /// Find a `:param` field in a class by its bare name (without sigil).
    fn find_param_field(&self, class_name: &str, key: &str) -> Option<Span> {
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Field) {
                if let SymbolDetail::Field { ref attributes, .. } = sym.detail {
                    if attributes.contains(&"param".to_string())
                        && self.symbol_in_class(sym.id, class_name)
                    {
                        if sym.bare_name() == key {
                            return Some(sym.selection_span);
                        }
                    }
                }
            }
        }
        None
    }

    /// Format a method completion detail string, appending return type if known.
    fn method_detail(
        &self,
        class_name: &str,
        method_name: &str,
        defining_class: Option<&str>,
        module_index: Option<&ModuleIndex>,
    ) -> String {
        let base = if let Some(dc) = defining_class {
            if dc != class_name {
                format!("{} (from {})", class_name, dc)
            } else {
                class_name.to_string()
            }
        } else {
            class_name.to_string()
        };
        if let Some(ref rt) = self.find_method_return_type(class_name, method_name, module_index, None) {
            // `opaque_return` lets the declaring plugin say "this chain
            // link is internal plumbing — don't render the class name OR
            // the return type". The chain still resolves; the user just
            // doesn't see the proxy-class path at every completion detail.
            //
            // Check both the context class AND the defining class: the
            // plugin declares opacity on the symbol where the method
            // LIVES, which is the defining class during a cross-class
            // walk (e.g. Users inheriting the helper from
            // Mojolicious::Controller).
            let opaque = self.method_opaque_return_cross_file(class_name, method_name, module_index)
                || defining_class.is_some_and(|dc| {
                    self.method_opaque_return_cross_file(dc, method_name, module_index)
                });
            if opaque {
                return String::new();
            }
            format!("{} → {}", base, format_inferred_type(&rt))
        } else {
            base
        }
    }

    /// Does the Method/Sub `method_name` on `class_name` opt out of
    /// rendering its return type at call sites? Plugin-declared via
    /// `opaque_return` on the symbol's detail. Walks both local
    /// symbols and any cross-file modules that emit content on the
    /// class (plugin helpers land in the file where the registration
    /// runs, not in the target class's own module).
    fn method_opaque_return(&self, class_name: &str, method_name: &str) -> bool {
        let check = |sym: &Symbol| -> bool {
            if sym.name != method_name { return false; }
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { return false; }
            if sym.package.as_deref() != Some(class_name) { return false; }
            matches!(&sym.detail, SymbolDetail::Sub { opaque_return: true, .. })
        };
        for sym in &self.symbols {
            if check(sym) { return true; }
        }
        false
    }

    /// Cross-file-aware variant: used by `method_detail` during
    /// completion to decide whether to suppress the proxy chain in
    /// the detail string, even when the declaring plugin emitted the
    /// method from another file. Same contract as
    /// `method_opaque_return` otherwise.
    fn method_opaque_return_cross_file(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> bool {
        if self.method_opaque_return(class_name, method_name) {
            return true;
        }
        let Some(idx) = module_index else { return false };
        let mut found = false;
        idx.for_each_entity_bridged_to(class_name, |_mod, _cached, sym| {
            if found { return; }
            if sym.name != method_name { return; }
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { return; }
            if matches!(&sym.detail, SymbolDetail::Sub { opaque_return: true, .. }) {
                found = true;
            }
        });
        found
    }

    /// Complete methods for a known class name, walking the inheritance chain.
    pub fn complete_methods_for_class(
        &self,
        class_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();
        let mut seen_names: HashSet<String> = HashSet::new();

        // Check for class definition → implicit new
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Class) && sym.name == class_name {
                candidates.push(CompletionCandidate {
                    label: "new".to_string(),
                    kind: SymKind::Method,
                    detail: Some(self.method_detail(class_name, "new", None, module_index)),
                    insert_text: None,
                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                display_override: None,
                });
                seen_names.insert("new".to_string());
                break;
            }
        }

        // Collect methods from this class and all ancestors
        self.collect_ancestor_methods(class_name, class_name, module_index, &mut candidates, &mut seen_names, 0);

        candidates
    }

    /// Recursively collect methods from a class and its ancestors, deduping by name.
    fn collect_ancestor_methods(
        &self,
        original_class: &str,
        class_name: &str,
        module_index: Option<&ModuleIndex>,
        candidates: &mut Vec<CompletionCandidate>,
        seen_names: &mut HashSet<String>,
        depth: usize,
    ) {
        if depth > 20 {
            return;
        }

        // Local methods in this class
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                if self.symbol_in_class(sym.id, class_name) && !seen_names.contains(&sym.name) {
                    seen_names.insert(sym.name.clone());
                    let defining = if class_name != original_class { Some(class_name) } else { None };
                    let display_override = sub_display_override(&sym.detail);
                    candidates.push(CompletionCandidate {
                        label: sym.name.clone(),
                        kind: sym.kind,
                        detail: Some(self.method_detail(original_class, &sym.name, defining, module_index)),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                        display_override,
                    });
                }
            }
        }

        // Local plugin-namespace entities bridged to this class. The
        // same-file equivalent of `for_each_entity_bridged_to` — plugin
        // namespaces in THIS FileAnalysis whose bridges include
        // `class_name`. Namespace membership is the sole filter (per
        // `for_each_entity_bridged_to` docs); entity packages can be
        // different from `class_name` (e.g. a helper Method whose
        // package is `Mojolicious::Controller` surfacing from a
        // `Mojolicious` query when the namespace bridges both).
        for ns in &self.plugin_namespaces {
            let bridges_class = ns.bridges.iter().any(|b|
                matches!(b, Bridge::Class(c) if c == class_name));
            if !bridges_class { continue; }
            for sym_id in &ns.entities {
                let Some(sym) = self.symbols.get(sym_id.0 as usize) else { continue };
                if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                if seen_names.contains(&sym.name) { continue; }
                seen_names.insert(sym.name.clone());
                let defining = if class_name != original_class { Some(class_name) } else { None };
                let display_override = sub_display_override(&sym.detail);
                candidates.push(CompletionCandidate {
                    label: sym.name.clone(),
                    kind: sym.kind,
                    detail: Some(self.method_detail(original_class, &sym.name, defining, module_index)),
                    insert_text: None,
                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                    display_override,
                });
            }
        }

        // Cross-file entity + own-class method collection. Parent
        // recursion (local ∪ cross-file ∪ synthetic app-surface edge)
        // is the single `parents_of` walk at the end of the fn.
        if let Some(idx) = module_index {
            // Two sources of candidates:
            //   (1) Plugin entities reached through bridges (helpers,
            //       routes, tasks, etc. — explicit `Bridge::Class(X)`
            //       declarations from PluginNamespaces across the
            //       workspace).
            //   (2) The cached module whose primary package IS
            //       class_name (real CPAN/user-defined methods on the
            //       class itself).
            // Collect into a temporary list to avoid borrow-checker
            // issues with the closure capturing &mut seen_names/candidates.
            let mut bridged: Vec<(String, SymKind, Option<SymbolDetail>, Option<InferredType>)> = Vec::new();
            idx.for_each_entity_bridged_to(class_name, |_mod, _cached, sym| {
                if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { return; }
                bridged.push((
                    sym.name.clone(),
                    sym.kind,
                    Some(sym.detail.clone()),
                    None,
                ));
            });
            for (name, kind, detail, _rt) in bridged {
                if seen_names.contains(&name) { continue; }
                seen_names.insert(name.clone());
                let is_method = kind == SymKind::Method
                    || matches!(detail, Some(SymbolDetail::Sub { is_method: true, .. }));
                let kind = if is_method { SymKind::Method } else { SymKind::Sub };
                let defining = if class_name != original_class { Some(class_name) } else { None };
                let method_detail_str = self.method_detail(original_class, &name, defining, module_index);
                let display_override = detail.as_ref()
                    .map(|d| sub_display_override(d))
                    .unwrap_or(None);
                candidates.push(CompletionCandidate {
                    label: name,
                    kind,
                    detail: Some(method_detail_str),
                    insert_text: None,
                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                    display_override,
                });
            }
            // (2) Real methods on class_name's own cached module.
            if let Some(cached) = idx.get_cached(class_name) {
                for sym in &cached.analysis.symbols {
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                    if sym.package.as_deref() != Some(class_name) { continue; }
                    if seen_names.contains(&sym.name) { continue; }
                    seen_names.insert(sym.name.clone());
                    let is_method = sym.kind == SymKind::Method
                        || matches!(sym.detail, SymbolDetail::Sub { is_method: true, .. });
                    let kind = if is_method { SymKind::Method } else { SymKind::Sub };
                    let defining = if class_name != original_class { Some(class_name) } else { None };
                    let detail = self.method_detail(original_class, &sym.name, defining, module_index);
                    let display_override = sub_display_override(&sym.detail);
                    candidates.push(CompletionCandidate {
                        label: sym.name.clone(),
                        kind,
                        detail: Some(detail),
                        insert_text: None,
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                        display_override,
                    });
                }
            }

        }

        // Walk parents: local ∪ cross-file ∪ synthetic app-surface edge,
        // unioned + deduped by `parents_of` (the single edge-injection
        // site). Name dedup across the recursion is the `seen_names` set.
        for parent in parents_of(
            class_name,
            &self.package_parents,
            module_index,
            &self.app_surface_consumers,
        ) {
            self.collect_ancestor_methods(
                original_class, &parent, module_index, candidates, seen_names, depth + 1,
            );
        }
    }

    /// Get the enclosing package name at a point.
    ///
    /// Resolves via `package_ranges` (innermost — latest-starting —
    /// containing range wins). Falls back to a scope walk for older
    /// cache blobs deserialised before `package_ranges` existed.
    #[allow(dead_code)]
    pub fn package_at(&self, point: Point) -> Option<&str> {
        if !self.package_ranges.is_empty() {
            let mut best: Option<&PackageRange> = None;
            for r in &self.package_ranges {
                if !contains_point(&r.span, point) {
                    continue;
                }
                let win = match best {
                    None => true,
                    Some(prev) => {
                        // Latest-starting wins; on a tie, narrower span wins.
                        let cur_start = (r.span.start.row, r.span.start.column);
                        let prev_start = (prev.span.start.row, prev.span.start.column);
                        cur_start > prev_start
                            || (cur_start == prev_start && span_size(&r.span) < span_size(&prev.span))
                    }
                };
                if win {
                    best = Some(r);
                }
            }
            return best.map(|r| r.package.as_str());
        }
        // Fallback: legacy cache blob with no package_ranges.
        let scope = self.scope_at(point)?;
        let chain = self.scope_chain(scope);
        for scope_id in &chain {
            let s = &self.scopes[scope_id.0 as usize];
            if let Some(ref pkg) = s.package {
                return Some(pkg.as_str());
            }
        }
        None
    }

    /// Iterate Handler symbols whose owner class is `owner_class` and
    /// that dispatch through any of `dispatchers`. When `dispatchers`
    /// is empty, all handlers for that class match. Powers plugin
    /// `dispatch_targets_for` (rule #5: this extraction lives on the
    /// data model, not duplicated in symbols.rs).
    pub fn handlers_for_owner<'a>(
        &'a self,
        owner_class: &'a str,
        dispatchers: &'a [String],
    ) -> impl Iterator<Item = &'a Symbol> + 'a {
        self.symbols.iter().filter(move |sym| {
            let SymbolDetail::Handler { owner, dispatchers: dd, .. } = &sym.detail else {
                return false;
            };
            let HandlerOwner::Class(c) = owner;
            if c != owner_class { return false; }
            if !dispatchers.is_empty()
                && !dd.iter().any(|d| dispatchers.iter().any(|n| n == d))
            {
                return false;
            }
            true
        })
    }

    /// Trigger-matching view for plugin query hooks at `point`: the
    /// modules `use`d inside the enclosing package plus the transitive
    /// parent chain. Mirrors what the builder assembles at emit time so
    /// query hooks can be gated by the same `PluginRegistry::applicable`
    /// filter instead of running against every bundled plugin.
    pub fn trigger_view_at(&self, point: Point) -> (Vec<String>, Vec<String>) {
        let pkg = match self.package_at(point) {
            Some(p) => p.to_string(),
            None => return (Vec::new(), Vec::new()),
        };
        let uses = self.package_uses.get(&pkg).cloned().unwrap_or_default();
        let mut parents = Vec::new();
        let mut seen = std::collections::HashSet::new();
        let mut stack = vec![pkg.clone()];
        while let Some(cur) = stack.pop() {
            if let Some(ps) = self.package_parents.get(&cur) {
                for p in ps {
                    if seen.insert(p.clone()) {
                        parents.push(p.clone());
                        stack.push(p.clone());
                    }
                }
            }
        }
        (uses, parents)
    }

    /// Resolve the class name for an invocant expression text (the token
    /// left of `->`). Handles the two Perl conventions `$self` and
    /// `__PACKAGE__` by falling back to the enclosing package; typed
    /// scalars go through `inferred_type`; barewords are treated as
    /// class names verbatim.
    ///
    /// This is Perl-semantic resolution, so it belongs on the data
    /// layer (rule #3). Callers in `symbols.rs` / `cursor_context.rs`
    /// compose this; they don't repeat the rules.
    pub fn invocant_text_to_class(&self, invocant: Option<&str>, point: Point) -> Option<String> {
        let text = invocant?;
        if text == "$self" || text == "__PACKAGE__" {
            return self.package_at(point).map(|s| s.to_string());
        }
        if text.starts_with('$') {
            // Use `InferredType::class_name()` so BOTH `ClassName` and
            // `FirstParam` resolve to their class — without this, a
            // `my ($c) = @_` invocant in a controller method (typed
            // `FirstParam { package: Users }`) falls back to None and
            // dispatch-target completion never fires for `$c->url_for(|)`.
            // Method completion on the same `$c` already uses this
            // accessor; routing through it here keeps the two paths in
            // sync. Rule #3.
            return self.inferred_type_via_bag(text, point)
                .and_then(|t| t.class_name().map(str::to_string));
        }
        if text.starts_with('@') || text.starts_with('%') {
            return None;
        }
        Some(text.to_string())
    }

    /// Find all symbols with a given name.
    pub fn symbols_named(&self, name: &str) -> &[SymbolId] {
        self.symbols_by_name.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Find all symbols in a given scope.
    #[allow(dead_code)]
    pub fn symbols_in_scope(&self, scope: ScopeId) -> &[SymbolId] {
        self.symbols_by_scope.get(&scope).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Find all refs with a given target name.
    #[allow(dead_code)]
    pub fn refs_named(&self, name: &str) -> Vec<&Ref> {
        self.refs_by_name.get(name)
            .map(|idxs| idxs.iter().map(|&i| &self.refs[i]).collect())
            .unwrap_or_default()
    }

    /// Find all refs that resolve to a specific symbol.
    #[allow(dead_code)]
    pub fn refs_to(&self, target: SymbolId) -> Vec<&Ref> {
        self.refs.iter()
            .filter(|r| r.resolves_to == Some(target))
            .collect()
    }

    /// Find all hash key accesses/definitions for a given owner.
    #[allow(dead_code)]
    pub fn hash_keys_for_owner(&self, owner: &HashKeyOwner) -> Vec<&Ref> {
        self.refs.iter()
            .filter(|r| {
                if let RefKind::HashKeyAccess { owner: Some(ref o), .. } = r.kind {
                    o == owner
                } else {
                    false
                }
            })
            .collect()
    }

    /// Find all hash key definition symbols for a given owner.
    pub fn hash_key_defs_for_owner(&self, owner: &HashKeyOwner) -> Vec<&Symbol> {
        self.symbols.iter()
            .filter(|s| {
                if let SymbolDetail::HashKeyDef { owner: ref o, .. } = s.detail {
                    o.found_by(owner)
                } else {
                    false
                }
            })
            .collect()
    }

    // ---- Cursor lookup methods ----

    /// Find the ref at a given point (cursor position).
    pub fn ref_at(&self, point: Point) -> Option<&Ref> {
        self.refs.iter()
            .filter(|r| contains_point(&r.span, point))
            .min_by_key(|r| span_size(&r.span))
    }

    /// Find the symbol whose selection_span contains the point.
    pub fn symbol_at(&self, point: Point) -> Option<&Symbol> {
        self.symbols.iter().find(|s| contains_point(&s.selection_span, point))
    }

    // ---- High-level queries ----

    /// Go-to-definition: resolve the symbol at cursor to its definition span.
    pub fn find_definition(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>, module_index: Option<&ModuleIndex>) -> Option<Span> {
        // 1. Check if cursor is on a ref
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable => {
                    if let Some(sym_id) = r.resolves_to {
                        return Some(self.symbol(sym_id).selection_span);
                    }
                }
                RefKind::FunctionCall { resolved_package } => {
                    // Package-scoped: pick the sub whose package
                    // matches the ref's resolved_package. When the
                    // call pinned a specific package (import or
                    // local-package match), we MUST NOT jump to a
                    // same-named sub in a different package — that's
                    // the cross-class collision we're specifically
                    // guarding against.
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if sym.kind != SymKind::Sub { continue; }
                        if sym.package == *resolved_package {
                            return Some(sym.selection_span);
                        }
                    }
                    // Nothing local; leave cross-file resolution to
                    // the LSP adapter (symbols::find_definition).
                }
                RefKind::MethodCall { .. } => {
                    let class_name = self.method_call_invocant_class(r, module_index);

                    // Cursor on a named arg key (not the method name):
                    // resolve to its hash-key def or :param field.
                    //
                    // The hash-key-arg owner is per-flavor + method-aware:
                    // `ParametricType::method_arg_owner(method)` —
                    // `Some(owner)` means the flavor claims these args
                    // (DBIC ResultSet → row class for search/find/create;
                    // None for count/exists). When unclaimed, fall back to
                    // the receiver's class (constructor keys on
                    // `bless {} 'Foo'`). Method dispatch still uses
                    // `class_name()`, so `$rs->search` resolves against the
                    // ResultSet base.
                    if let (Some(t), Some(s)) = (tree, source_bytes) {
                        if let Some(key_name) = self.call_arg_key_at(t, s, point) {
                            let owner = self
                                .method_call_invocant_type(r, module_index)
                                .as_ref()
                                .and_then(|ty| ty.as_parametric())
                                .and_then(|p| p.method_arg_owner(&r.target_name))
                                .or_else(|| {
                                    class_name
                                        .as_ref()
                                        .map(|c| HashKeyOwner::Class(c.clone()))
                                });
                            if let Some(owner) = owner {
                                for def in self.hash_key_defs_for_owner(&owner) {
                                    if def.name == key_name {
                                        return Some(def.selection_span);
                                    }
                                }
                                if let HashKeyOwner::Class(cn) = &owner {
                                    if let Some(span) = self.find_param_field(cn, &key_name) {
                                        return Some(span);
                                    }
                                }
                            }
                        }
                    }

                    if let Some(ref cn) = class_name {
                        // Find method in the resolved class (walks inheritance)
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, .. }) => {
                                return Some(self.symbol(sym_id).selection_span);
                            }
                            Some(MethodResolution::CrossFile { .. }) => {
                                // Cross-file method — return None so the LSP adapter
                                // can resolve it via ModuleIndex with the correct URI.
                                return None;
                            }
                            None => {
                                // If the class has known parents, the method might be
                                // inherited but the module index hasn't resolved yet.
                                // Return None so the cross-file block in symbols.rs
                                // gets a chance, rather than jumping to the package decl.
                                if self.package_parents.contains_key(cn) {
                                    return None;
                                }
                            }
                        }
                        // Method not found and no parents — go to class def
                        if let Some(span) = self.find_package_or_class(cn) {
                            return Some(span);
                        }
                    }
                    // Last-resort "any sub/method with that name" fallback.
                    // Only fires when we couldn't resolve the invocant at
                    // all — if we knew the target class but couldn't find
                    // the method there, jumping to an unrelated sub is
                    // actively harmful (plugin-emitted MethodCallRefs
                    // targeting `'Users#create'` would otherwise latch
                    // onto a `create` helper synthesized on some unrelated
                    // class). Returning None is better — the LSP adapter
                    // can still try cross-file paths.
                    if class_name.is_none() {
                        for &sid in self.symbols_named(&r.target_name) {
                            let sym = self.symbol(sid);
                            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                                return Some(sym.selection_span);
                            }
                        }
                    }
                }
                RefKind::PackageRef => {
                    return self.find_package_or_class(&r.target_name);
                }
                RefKind::HashKeyAccess { ref owner, .. } => {
                    // Try the pre-resolved owner first
                    let resolved_owner = owner.clone().or_else(|| {
                        let tree = tree?;
                        let source = source_bytes?;
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start, module_index)
                    });
                    if let Some(ref owner) = resolved_owner {
                        for def in self.hash_key_defs_for_owner(owner) {
                            if def.name == r.target_name {
                                return Some(def.selection_span);
                            }
                        }
                    }
                }
                RefKind::ContainerAccess => {
                    return self.resolve_variable(&r.target_name, point)
                        .map(|sym| sym.selection_span);
                }
                RefKind::DispatchCall { owner: Some(owner), .. } => {
                    // Go-to-def on a dispatch call site lands at the
                    // first stacked Handler for this (owner, name).
                    // Features that want all registrations walk
                    // `refs_to_symbol` or use `refs_to` with
                    // `TargetKind::Handler`.
                    for sym in &self.symbols {
                        if sym.name != r.target_name { continue; }
                        if let SymbolDetail::Handler { owner: o, .. } = &sym.detail {
                            if o == owner {
                                return Some(sym.selection_span);
                            }
                        }
                    }
                    // No LOCAL Handler — the registration is cross-file.
                    // Return None (terminal) so the LSP adapter's cross-file
                    // DispatchCall resolver runs. Falling through to the
                    // `symbol_at` fallback below would wrongly grab whatever
                    // symbol overlaps the call-arg string (e.g. a synthesized
                    // hash-key def at the same span).
                    return None;
                }
                RefKind::DispatchCall { owner: None, .. } => {}
            }
        }

        // 2. Check if cursor is on a symbol declaration
        if let Some(sym) = self.symbol_at(point) {
            return Some(sym.selection_span);
        }

        None
    }

    /// Find all references to the symbol at cursor.
    pub fn find_references(
        &self,
        point: Point,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<Span> {
        if let Some((target_id, include_decl)) = self.resolve_target_at(point, tree, source_bytes, module_index) {
            let mut results = self.collect_refs_for_target(target_id, include_decl, tree, source_bytes, module_index);
            results.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            results.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            results.into_iter().map(|(span, _)| span).collect()
        } else {
            Vec::new()
        }
    }

    /// Document highlights: like references but with read/write annotation.
    pub fn find_highlights(
        &self,
        point: Point,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<(Span, AccessKind)> {
        if let Some((target_id, _)) = self.resolve_target_at(point, tree, source_bytes, module_index) {
            let mut results = self.collect_refs_for_target(target_id, true, tree, source_bytes, module_index);
            results.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            results.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            return results;
        }
        // Fallback — cursor is on a ref whose target isn't defined in
        // this file (cross-file method call, unresolved import). Match
        // other refs in the same file that share (target_name, scope).
        // Without this, documentHighlight on a cross-file method call
        // returns empty even when other call sites for the same class+method
        // exist in the file.
        if let Some(r) = self.ref_at(point) {
            let mut results: Vec<(Span, AccessKind)> = Vec::new();
            match &r.kind {
                RefKind::MethodCall { method_name_span, .. } => {
                    // Single bag-routed invocant resolver — same call
                    // for the cursor's ref and every candidate.
                    let Some(wanted_class) = self.method_call_invocant_class(r, module_index) else {
                        return Vec::new();
                    };
                    results.push((*method_name_span, r.access));
                    for other in &self.refs {
                        if std::ptr::eq(other, r) { continue; }
                        if other.target_name != r.target_name { continue; }
                        if !matches!(other.kind, RefKind::MethodCall { .. }) { continue; }
                        let Some(ocn) = self.method_call_invocant_class(other, module_index) else { continue };
                        if ocn != wanted_class { continue; }
                        if let RefKind::MethodCall { method_name_span: ms, .. } = &other.kind {
                            results.push((*ms, other.access));
                        }
                    }
                }
                RefKind::FunctionCall { resolved_package: Some(pkg) } => {
                    let wanted_pkg = pkg.clone();
                    results.push((r.span, r.access));
                    for other in &self.refs {
                        if std::ptr::eq(other, r) { continue; }
                        if other.target_name != r.target_name { continue; }
                        if let RefKind::FunctionCall { resolved_package: Some(op) } = &other.kind {
                            if op == &wanted_pkg {
                                results.push((other.span, other.access));
                            }
                        }
                    }
                }
                _ => {}
            }
            results.sort_by_key(|(s, _)| (s.start.row, s.start.column));
            results.dedup_by(|a, b| a.0.start == b.0.start && a.0.end == b.0.end);
            return results;
        }
        Vec::new()
    }

    /// Shared implementation for find_references and find_highlights.
    fn collect_refs_for_target(
        &self,
        target_id: SymbolId,
        include_decl: bool,
        tree: Option<&tree_sitter::Tree>,
        source_bytes: Option<&[u8]>,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<(Span, AccessKind)> {
        let sym = self.symbol(target_id);
        let mut results: Vec<(Span, AccessKind)> = Vec::new();

        // Include the declaration itself
        if include_decl {
            results.push((sym.selection_span, AccessKind::Declaration));
        }

        // O(1) lookup for every ref resolved to this symbol.
        // This covers variables, HashKeyAccess refs whose owner was resolved at
        // build time, and any future kinds that set resolves_to.
        for &idx in self.refs_to_symbol(target_id) {
            let r = &self.refs[idx];
            results.push((r.span, r.access));
        }

        // For subs/methods/packages/classes, also find refs by name.
        // Scope-filter callable refs (Sub / Method) by the target
        // symbol's package — matches `resolve::refs_to` so
        // documentHighlight and references agree on "same callable".
        // Without this, cursor on one `create` highlights every other
        // same-named method/sub in the file regardless of class
        // (mojo-helper leaf on `_Helper::users` cross-highlighting
        // the route's `Users::create` ref, etc.).
        if matches!(sym.kind, SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class | SymKind::Module) {
            let sym_package = sym.package.clone();
            for r in &self.refs {
                if r.target_name == sym.name && r.resolves_to.is_none() {
                    match (&r.kind, &sym.kind) {
                        (RefKind::FunctionCall { resolved_package }, SymKind::Sub) => {
                            if *resolved_package == sym_package {
                                results.push((r.span, r.access));
                            }
                        }
                        (RefKind::MethodCall { method_name_span, .. },
                         SymKind::Sub | SymKind::Method) => {
                            // Same-class match only; unresolved or
                            // different-class invocants are excluded.
                            // Method-call ref.span covers the whole
                            // `$obj->foo(...)` expression so we use
                            // `method_name_span` to highlight just
                            // the identifier.
                            match (self.method_call_invocant_class(r, module_index), &sym_package) {
                                (Some(cn), Some(pkg)) if cn == *pkg => {
                                    results.push((*method_name_span, r.access));
                                }
                                _ => {}
                            }
                        }
                        (RefKind::PackageRef, SymKind::Package | SymKind::Class | SymKind::Module) => results.push((r.span, r.access)),
                        _ => {}
                    }
                }
            }
        }

        // For hash key definitions, find all accesses with same owner + key name
        if let SymbolDetail::HashKeyDef { ref owner, .. } = sym.detail {
            for r in &self.refs {
                if let RefKind::HashKeyAccess { owner: ref ro, .. } = r.kind {
                    if r.target_name != sym.name {
                        continue;
                    }
                    let matches = match ro {
                        Some(ref ro) => owner.found_by(ro),
                        None => {
                            if let (Some(t), Some(s)) = (tree, source_bytes) {
                                self.resolve_hash_owner_from_tree(t, s, r.span.start, None)
                                    .as_ref()
                                    .map_or(false, |resolved| owner.found_by(resolved))
                            } else {
                                false
                            }
                        }
                    };
                    if matches {
                        results.push((r.span, r.access));
                    }
                }
            }
        }

        results
    }

    /// Hover info: return display text for the symbol at cursor.
    pub fn hover_info(&self, point: Point, source: &str, module_index: Option<&ModuleIndex>) -> Option<String> {
        // Check refs first
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    // Check if this variable is also a dynamic method call target
                    // (e.g. $self->$method() where $method is a known constant)
                    let method_hover = self.refs.iter()
                        .find(|mr| matches!(mr.kind, RefKind::MethodCall { .. })
                            && contains_point(&mr.span, point)
                            && mr.target_name != r.target_name);
                    if let Some(mr) = method_hover {
                        if matches!(mr.kind, RefKind::MethodCall { .. }) {
                            let class_name = self.method_call_invocant_class(mr, module_index);
                            if let Some(ref cn) = class_name {
                                match self.resolve_method_in_ancestors(cn, &mr.target_name, module_index) {
                                    Some(MethodResolution::Local { sym_id, class: ref defining_class, .. }) => {
                                        let sym = self.symbol(sym_id);
                                        let line = source_line_at(source, sym.selection_span.start.row);
                                        let class_label = if defining_class != cn {
                                            format!("{} (from {})", cn, defining_class)
                                        } else {
                                            cn.to_string()
                                        };
                                        let mut text = format!("```perl\n{}\n```\n\n*class {} — resolved from `{}`*", line.trim(), class_label, r.target_name);
                                        if let Some(ref rt) = self.find_method_return_type(cn, &mr.target_name, module_index, None) {
                                            text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(&rt)));
                                        }
                                        if let SymbolDetail::Sub { ref doc, .. } = sym.detail {
                                            if let Some(ref d) = doc {
                                                text.push_str(&format!("\n\n{}", d));
                                            }
                                        }
                                        return Some(text);
                                    }
                                    Some(MethodResolution::CrossFile { ref class, ref def_module }) => {
                                        if let Some(idx) = module_index {
                                            // Bridged helper lives in `def_module`; real
                                            // inherited method in `class`'s own module.
                                            let module = def_module.as_deref().unwrap_or(class.as_str());
                                            if let Some(cached) = idx.get_cached(module) {
                                                if let Some(sub_info) = cached.sub_info(&mr.target_name) {
                                                    let sig = format_cross_file_signature(&mr.target_name, &sub_info);
                                                    let mut text = format!("```perl\n{}\n```\n\n*class {} — resolved from `{}`*", sig, class, r.target_name);
                                                    if let Some(rt) = sub_info.return_type(Some(idx)) {
                                                        text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(&rt)));
                                                    }
                                                    if let Some(doc) = sub_info.doc() {
                                                        text.push_str(&format!("\n\n{}", doc));
                                                    }
                                                    return Some(text);
                                                }
                                            }
                                        }
                                    }
                                    None => {}
                                }
                            }
                        }
                    }
                    if let Some(sym_id) = r.resolves_to {
                        let sym = self.symbol(sym_id);
                        return Some(self.format_symbol_hover_at(sym, source, point));
                    }
                    // Unresolved variable — try resolve ourselves
                    if let Some(sym) = self.resolve_variable(&r.target_name, point) {
                        return Some(self.format_symbol_hover_at(sym, source, point));
                    }
                }
                RefKind::FunctionCall { resolved_package } => {
                    // Package-scoped: hover shows the sub whose
                    // package matches what the ref resolved to.
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if sym.kind != SymKind::Sub { continue; }
                        if sym.package == *resolved_package {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                    // Fall-through: the name might be a function imported
                    // from another module (either hand-written `use` or a
                    // plugin-synthesized Import like Mojolicious::Lite's
                    // route verbs). Cross-file lookup pulls real POD,
                    // real signature, real return type from the source
                    // module's cached analysis.
                    if let Some(idx) = module_index {
                        for import in &self.imports {
                            let matched = import.imported_symbols.iter()
                                .find(|s| s.local_name == r.target_name);
                            let Some(is) = matched else { continue };
                            let Some(cached) = idx.get_cached(&import.module_name) else { continue };
                            let Some(sub_info) = cached.sub_info(is.remote()) else { continue };

                            let sig_params = sub_info.params().iter()
                                .map(|p| p.name.as_str())
                                .collect::<Vec<_>>()
                                .join(", ");
                            let mut sig = format!("sub {}({})", r.target_name, sig_params);
                            if let Some(rt) = sub_info.return_type(Some(idx)) {
                                sig.push_str(&format!(" → {}", format_inferred_type(&rt)));
                            }
                            let mut text = format!("```perl\n{}\n```", sig);
                            if let Some(doc) = sub_info.doc() {
                                text.push_str(&format!("\n\n{}", doc));
                            }
                            if is.remote() != r.target_name {
                                text.push_str(&format!(
                                    "\n\n*imported from `{}` (as `{}`)*",
                                    import.module_name, is.remote()
                                ));
                            } else {
                                text.push_str(&format!(
                                    "\n\n*imported from `{}`*",
                                    import.module_name
                                ));
                            }
                            return Some(text);
                        }
                    }
                }
                RefKind::MethodCall { .. } => {
                    let class_name = self.method_call_invocant_class(r, module_index);
                    if let Some(ref cn) = class_name {
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, class: ref defining_class, .. }) => {
                                let sym = self.symbol(sym_id);
                                let line = source_line_at(source, sym.selection_span.start.row);
                                let class_label = if defining_class != cn {
                                    format!("{} (from {})", cn, defining_class)
                                } else {
                                    cn.to_string()
                                };
                                let mut text = format!("```perl\n{}\n```\n\n*class {}*", line.trim(), class_label);
                                if let Some(ref rt) = self.find_method_return_type(cn, &r.target_name, module_index, None) {
                                    text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(&rt)));
                                }
                                return Some(text);
                            }
                            Some(MethodResolution::CrossFile { ref class, ref def_module }) => {
                                if let Some(idx) = module_index {
                                    // Bridged helper lives in `def_module`; real
                                    // inherited method in `class`'s own module.
                                    let module = def_module.as_deref().unwrap_or(class.as_str());
                                    if let Some(cached) = idx.get_cached(module) {
                                        if let Some(sub_info) = cached.sub_info(&r.target_name) {
                                            let class_label = if class != cn {
                                                format!("{} (from {})", cn, class)
                                            } else {
                                                cn.to_string()
                                            };
                                            let sig = format_cross_file_signature(&r.target_name, &sub_info);
                                            let mut text = format!("```perl\n{}\n```\n\n*class {}*", sig, class_label);
                                            if let Some(rt) = sub_info.return_type(Some(idx)) {
                                                text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(&rt)));
                                            }
                                            if let Some(doc) = sub_info.doc() {
                                                text.push_str(&format!("\n\n{}", doc));
                                            }
                                            return Some(text);
                                        }
                                    }
                                }
                            }
                            None => {}
                        }
                    }
                    // Fallback
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                }
                RefKind::PackageRef => {
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                            return Some(self.format_symbol_hover(sym, source));
                        }
                    }
                }
                RefKind::HashKeyAccess { owner, .. } => {
                    if let Some(ref owner) = owner {
                        let defs = self.hash_key_defs_for_owner(owner);
                        let matching: Vec<_> = defs.iter()
                            .filter(|d| d.name == r.target_name)
                            .collect();
                        if !matching.is_empty() {
                            let lines: Vec<String> = matching.iter()
                                .map(|d| {
                                    let line = source_line_at(source, d.span.start.row);
                                    format!("- `{}`", line.trim())
                                })
                                .collect();
                            return Some(format!("**Hash key `{}`**\n\n{}", r.target_name, lines.join("\n")));
                        }
                    }
                }
                RefKind::DispatchCall { dispatcher, owner } => {
                    if let Some(ref owner) = owner {
                        return Some(self.format_handler_hover(
                            &r.target_name,
                            owner,
                            Some(dispatcher),
                            module_index,
                        ));
                    }
                }
            }
        }

        // Check symbols
        if let Some(sym) = self.symbol_at(point) {
            // Handler symbols get a specialized multi-registration hover
            // (stacked defs, dispatcher list, param shapes).
            if let SymbolDetail::Handler { owner, .. } = &sym.detail {
                return Some(self.format_handler_hover(&sym.name, owner, None, module_index));
            }
            return Some(self.format_symbol_hover(sym, source));
        }

        None
    }

    /// Rename: return all spans + new text for renaming the symbol at cursor.
    pub fn rename_at(&self, point: Point, new_name: &str, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>) -> Option<Vec<(Span, String)>> {
        let refs = self.find_references(point, tree, source_bytes, None);
        if refs.is_empty() {
            return None;
        }

        // Determine if this is a variable (sigil handling needed)
        let is_variable = self.ref_at(point)
            .map(|r| matches!(r.kind, RefKind::Variable | RefKind::ContainerAccess))
            .or_else(|| self.symbol_at(point).map(|s| matches!(s.kind, SymKind::Variable | SymKind::Field)))
            .unwrap_or(false);

        let edits: Vec<(Span, String)> = if is_variable {
            // Strip sigil from new_name if present
            let bare_name = if new_name.starts_with('$') || new_name.starts_with('@') || new_name.starts_with('%') {
                &new_name[1..]
            } else {
                new_name
            };
            refs.into_iter().map(|span| {
                // Each ref span includes the sigil; replace only the name part (after sigil)
                let name_span = Span {
                    start: Point::new(span.start.row, span.start.column + 1),
                    end: span.end,
                };
                (name_span, bare_name.to_string())
            }).collect()
        } else {
            refs.into_iter().map(|span| (span, new_name.to_string())).collect()
        };

        Some(edits)
    }

    /// Determine what kind of rename is appropriate for the cursor position.
    ///
    /// For `RenameKind::Method`, the class is mandatory (so cross-file
    /// walks don't cross-link unrelated classes that share a method
    /// name). When the invocant can't be resolved to a class, rename
    /// falls through to symbol-at resolution; if that also has no
    /// class context (orphan Sub), returns `None` — the cursor isn't
    /// on something we can safely rename.
    pub fn rename_kind_at(&self, point: Point, module_index: Option<&ModuleIndex>) -> Option<RenameKind> {
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => return Some(RenameKind::Variable),
                RefKind::FunctionCall { resolved_package } => {
                    return Some(RenameKind::Function {
                        name: r.target_name.clone(),
                        package: resolved_package.clone(),
                    });
                }
                RefKind::MethodCall { .. } => {
                    if let Some(class) = self.method_call_invocant_class(r, module_index) {
                        return Some(RenameKind::Method {
                            name: r.target_name.clone(),
                            class,
                        });
                    }
                    // Invocant unresolvable — try symbol-at fallback
                    // below; if that also has no class, bail rather
                    // than return a class-less Method rename.
                }
                RefKind::PackageRef => return Some(RenameKind::Package(r.target_name.clone())),
                RefKind::HashKeyAccess { .. } => return Some(RenameKind::HashKey(r.target_name.clone())),
                RefKind::DispatchCall { owner: Some(owner), .. } => {
                    return Some(RenameKind::Handler {
                        owner: owner.clone(),
                        name: r.target_name.clone(),
                    });
                }
                // Unresolved DispatchCall — owner couldn't be determined
                // at build time, so rename can't safely scope.
                RefKind::DispatchCall { owner: None, .. } => return None,
            }
        }
        if let Some(sym) = self.symbol_at(point) {
            return match sym.kind {
                SymKind::Variable | SymKind::Field => Some(RenameKind::Variable),
                SymKind::Sub => Some(RenameKind::Function {
                    name: sym.name.clone(),
                    package: sym.package.clone(),
                }),
                SymKind::Method => {
                    let class = sym.package.clone()?;
                    Some(RenameKind::Method {
                        name: sym.name.clone(),
                        class,
                    })
                }
                SymKind::Package | SymKind::Class => Some(RenameKind::Package(sym.name.clone())),
                SymKind::Handler => {
                    if let SymbolDetail::Handler { owner, .. } = &sym.detail {
                        Some(RenameKind::Handler { owner: owner.clone(), name: sym.name.clone() })
                    } else { None }
                }
                _ => None,
            };
        }
        None
    }

    /// Scope-aware rename for a sub/method: matches decls + both
    /// call shapes that resolve to this (scope, name) pair.
    ///
    ///   * decl walk — `Sub`/`Method` symbols whose `package == scope`
    ///   * FunctionCall refs whose `resolved_package == scope`
    ///   * MethodCall refs whose `invocant_class == scope`
    ///
    /// The two call shapes both resolve to the same underlying sub:
    /// `package Foo; sub run {}` is callable as `run()` OR
    /// `$self->run()` OR `Foo::run()`. A rename must rewrite every
    /// shape, and must NOT rewrite same-named subs/methods in other
    /// packages. When `scope == None`, matches top-level/script subs
    /// with no package; FunctionCall refs whose resolver didn't pin
    /// a package match those. MethodCall refs always need a class —
    /// `None` scope never matches them.
    fn rename_callable_in_scope(
        &self,
        old_name: &str,
        scope: &Option<String>,
        new_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<(Span, String)> {
        let mut edits = Vec::new();
        for sym in &self.symbols {
            if sym.name != old_name { continue; }
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
            if sym.package != *scope { continue; }
            edits.push((sym.selection_span, new_name.to_string()));
        }
        for r in &self.refs {
            if r.target_name != old_name { continue; }
            match &r.kind {
                RefKind::FunctionCall { resolved_package } => {
                    if resolved_package == scope {
                        edits.push((r.span, new_name.to_string()));
                    }
                }
                RefKind::MethodCall { method_name_span, .. } => {
                    // MethodCall refs target a class — `None` scope
                    // doesn't reach methods.
                    if let (Some(cls), Some(wanted)) =
                        (self.method_call_invocant_class(r, module_index), scope.as_ref())
                    {
                        if &cls == wanted {
                            edits.push((*method_name_span, new_name.to_string()));
                        }
                    }
                }
                _ => {}
            }
        }
        edits
    }

    /// Package-scoped sub rename — public-API name used by the
    /// Function rename path. Delegates to the unified
    /// `rename_callable_in_scope`; the distinction between "function"
    /// and "method" in Perl is just call shape, not identity.
    pub fn rename_sub_in_package(
        &self,
        old_name: &str,
        package: &Option<String>,
        new_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<(Span, String)> {
        self.rename_callable_in_scope(old_name, package, new_name, module_index)
    }

    /// Class-scoped method rename — public-API name used by the
    /// Method rename path. Same semantics as
    /// `rename_sub_in_package(old, &Some(class), new)`: function and
    /// method calls into the same package are two shapes of the same
    /// callable.
    pub fn rename_method_in_class(
        &self,
        old_name: &str,
        class: &str,
        new_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<(Span, String)> {
        self.rename_callable_in_scope(old_name, &Some(class.to_string()), new_name, module_index)
    }

    /// Find all occurrences of a package name (def + refs + use statements) for cross-file rename.
    pub fn rename_package(&self, old_name: &str, new_name: &str) -> Vec<(Span, String)> {
        let mut edits = Vec::new();
        for sym in &self.symbols {
            if sym.name == old_name && matches!(sym.kind, SymKind::Package | SymKind::Class | SymKind::Module) {
                edits.push((sym.selection_span, new_name.to_string()));
            }
        }
        for r in &self.refs {
            if r.target_name == old_name && matches!(r.kind, RefKind::PackageRef) {
                edits.push((r.span, new_name.to_string()));
            }
        }
        edits
    }

    // ---- Internal resolution helpers ----

    /// Find the target symbol for the thing at cursor. Returns (SymbolId, include_decl_in_refs).
    fn resolve_target_at(&self, point: Point, tree: Option<&tree_sitter::Tree>, source_bytes: Option<&[u8]>, module_index: Option<&ModuleIndex>) -> Option<(SymbolId, bool)> {
        // Check refs first
        if let Some(r) = self.ref_at(point) {
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    if let Some(sym_id) = r.resolves_to {
                        return Some((sym_id, true));
                    }
                    // Try resolving manually
                    if let Some(sym) = self.resolve_variable(&r.target_name, point) {
                        return Some((sym.id, true));
                    }
                }
                RefKind::FunctionCall { resolved_package } => {
                    for &sid in self.symbols_named(&r.target_name) {
                        let sym = self.symbol(sid);
                        if sym.kind != SymKind::Sub { continue; }
                        if sym.package == *resolved_package {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::MethodCall { .. } => {
                    let class_name = self.method_call_invocant_class(r, module_index);
                    // Try inheritance-aware resolution first
                    if let Some(ref cn) = class_name {
                        match self.resolve_method_in_ancestors(cn, &r.target_name, module_index) {
                            Some(MethodResolution::Local { sym_id, .. }) => {
                                return Some((sym_id, true));
                            }
                            Some(MethodResolution::CrossFile { .. }) => {
                                // Cross-file: no local SymbolId. Match
                                // a local symbol only when its package
                                // equals the resolved class — no
                                // same-name cross-class latching.
                                for &sid in self.symbols_named(&r.target_name) {
                                    let sym = self.symbol(sid);
                                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                                    if sym.package.as_deref() == Some(cn.as_str()) {
                                        return Some((sid, true));
                                    }
                                }
                            }
                            None => {}
                        }
                        // No local match, and the class is known but
                        // has no local decl — return a synthetic
                        // target by name filtered to the class, so
                        // collect_refs_for_target still walks refs
                        // correctly. If there's NO matching symbol on
                        // the class locally, produce no target —
                        // better than cross-linking a same-named
                        // method on a different class.
                        for &sid in self.symbols_named(&r.target_name) {
                            let sym = self.symbol(sid);
                            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                            if sym.package.as_deref() == Some(cn.as_str()) {
                                return Some((sid, true));
                            }
                        }
                        // Class known but not defined locally — no
                        // local symbol to anchor on. Caller may still
                        // collect refs via `refs_to` at the LSP layer,
                        // but highlight/definition within this file
                        // have nothing to return.
                        return None;
                    }
                    // Invocant class couldn't be pinned at all —
                    // last-resort name match. Only reaches here for
                    // refs the builder AND the runtime resolver both
                    // failed on (rare: ERROR-recovered invocants).
                    for &sid in self.symbols_named(&r.target_name) {
                        if matches!(self.symbol(sid).kind, SymKind::Sub | SymKind::Method) {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::PackageRef => {
                    for &sid in self.symbols_named(&r.target_name) {
                        if matches!(self.symbol(sid).kind, SymKind::Package | SymKind::Class | SymKind::Module) {
                            return Some((sid, true));
                        }
                    }
                }
                RefKind::HashKeyAccess { ref owner, .. } => {
                    let resolved_owner = owner.clone().or_else(|| {
                        let tree = tree?;
                        let source = source_bytes?;
                        self.resolve_hash_owner_from_tree(tree, source, r.span.start, module_index)
                    });
                    if let Some(ref owner) = resolved_owner {
                        for def in self.hash_key_defs_for_owner(owner) {
                            if def.name == r.target_name {
                                return Some((def.id, true));
                            }
                        }
                    }
                }
                RefKind::DispatchCall { owner: Some(owner), .. } => {
                    for sym in &self.symbols {
                        if sym.name != r.target_name { continue; }
                        if let SymbolDetail::Handler { owner: o, .. } = &sym.detail {
                            if o == owner {
                                return Some((sym.id, true));
                            }
                        }
                    }
                }
                RefKind::DispatchCall { owner: None, .. } => {}
            }
        }

        // Check if cursor is directly on a symbol declaration
        if let Some(sym) = self.symbol_at(point) {
            return Some((sym.id, false));
        }

        None
    }

    /// Resolve a `MethodCall` ref's invocant class via the witness
    /// bag — **the** invocant resolver. No tree, no text fallback, no
    /// per-reader parallel paths: every reader routes through here.
    ///
    /// Dispatch by invocant shape:
    ///   * `$var` / `@var` / `%var` → `inferred_type_via_bag` (so
    ///     cross-file enrichment's variable types compose automatically).
    ///   * `$self` (untyped) → enclosing class fallback.
    ///   * `__PACKAGE__` → enclosing class.
    ///   * Chain or function-call receiver (invocant_span points at
    ///     another ref) → that ref's bag answer
    ///     (`method_call_return_type_via_bag` for MethodCall;
    ///     `sub_return_type_at_arity` for FunctionCall). Receiver ref
    ///     found via the `call_ref_by_start` index (O(1)).
    ///   * Bareword → `Foo` if a zero-arg sub by that name returns
    ///     ClassName, else the bareword itself.
    ///
    /// Query-only: build-time chain typing already landed its product
    /// in the bag (Variable witnesses, `Expression` edge witnesses on
    /// chain receivers); this never re-derives.
    ///
    /// `module_index` lets chain receivers whose return type lives in
    /// another package resolve (e.g. `$r->get('/x')->to(...)`). Pass
    /// `None` only for CLI debug / isolated tests.
    pub fn method_call_invocant_class(
        &self,
        r: &Ref,
        module_index: Option<&ModuleIndex>,
    ) -> Option<String> {
        let RefKind::MethodCall { invocant, invocant_span, .. } = &r.kind else {
            return None;
        };
        if invocant.is_empty() {
            return None;
        }

        // Pseudo-invocants for "$self at the method's first arg":
        // `shift` (no args; `sub m { my $self = shift; ... }`),
        // `$_[0]` / `@_[0]` (positional), and raw `__PACKAGE__` on
        // synthesized refs. None are real variables, so the bag has no
        // witness for them — resolve to enclosing-class identity here.
        if invocant == "shift"
            || invocant == "$_[0]"
            || invocant == "@_[0]"
            || invocant == "__PACKAGE__"
        {
            return self.enclosing_class_for_scope(r.scope);
        }

        // The invocant's type, resolved tree-free from the bag at its
        // span. Covers every recorded shape: scalar/array/hash reads,
        // chain receivers (the `Expression(refidx)` axis), function-call
        // receivers, baked literals.
        if let Some(span) = invocant_span {
            if let Some(cn) = self
                .expr_type_at_span(*span, module_index)
                .and_then(|t| t.class_name().map(|s| s.to_string()))
            {
                return Some(cn);
            }
        }

        // Cross-file chain-receiver fallback. When the inner receiver's
        // class is only knowable once other modules load (`$c->minion->
        // enqueue` at enrichment), the build-time `Expr(span)` witness
        // is absent and `method_call_return_type_via_bag` has no edge to
        // chase. Re-resolve the receiver's own invocant class fresh with
        // the index, then chase `MethodOnClass{class, method}` through
        // `find_method_return_type` (ancestors + cross-file bridges via
        // the registry). This is the one structure-from-refs step the
        // bag can't pre-record, so it lives here, not in the builder.
        if let Some(span) = invocant_span {
            if let Some(&recv_idx) = self.call_ref_by_start.get(&span.start) {
                let recv_span = self.refs[recv_idx].span;
                let contained = recv_span.start == span.start
                    && (recv_span.end.row, recv_span.end.column)
                        <= (span.end.row, span.end.column);
                let is_self = std::ptr::eq(&self.refs[recv_idx], r);
                if contained && !is_self {
                    if let RefKind::MethodCall { .. } = &self.refs[recv_idx].kind {
                        let recv = &self.refs[recv_idx];
                        if let Some(recv_class) =
                            self.method_call_invocant_class(recv, module_index)
                        {
                            if recv.target_name == "new" {
                                return Some(recv_class);
                            }
                            if let Some(cn) = self
                                .find_method_return_type(
                                    &recv_class,
                                    &recv.target_name,
                                    module_index,
                                    None,
                                )
                                .and_then(|t| t.class_name().map(|s| s.to_string()))
                            {
                                return Some(cn);
                            }
                        }
                        return None;
                    }
                }
            }
        }

        // Variable invocant. `expr_type_at_span` above only answers when
        // the builder pre-recorded an `Expr(span)` — which it can't for a
        // variable whose type flows from a cross-file source resolved
        // only at enrichment (`my $x = $c->helper`, `$$x` re-typed once
        // other modules load). Re-derive from the bag by the variable's
        // name + position, threading the index so the chase follows the
        // cross-file Variable edge. Same single bag query everything else
        // uses; only the var name (which lives on the ref, not the span)
        // brings us here instead of `expr_type_at_span`.
        let point = invocant_span.map(|s| s.start).unwrap_or(r.span.start);
        let first = invocant.as_bytes()[0];
        if first == b'$' || first == b'@' || first == b'%' {
            if let Some(cn) = self
                .inferred_type_via_bag_ctx(invocant, point, module_index)
                .and_then(|t| t.class_name().map(|s| s.to_string()))
            {
                return Some(cn);
            }
            // `$self` enclosing-class fallback for an untyped variable
            // invocant. Other untyped variable invocants stay None —
            // better than poisoning them with the surrounding package.
            if invocant == "$self" {
                return self.enclosing_class_for_scope(r.scope);
            }
            return None;
        }

        // Bareword invocant. Could be a zero-arg sub returning ClassName
        // (`app->routes` where `app` is plugin-emitted); promote that.
        // Otherwise the bareword text *is* the class (`Foo->method`).
        let bare = invocant.rsplit("::").next().unwrap_or(invocant);
        if let Some(InferredType::ClassName(c)) = self.sub_return_type_at_arity(bare, Some(0)) {
            return Some(c);
        }
        Some(invocant.to_string())
    }

    /// Full `InferredType` of a `MethodCall` ref's invocant — same
    /// dispatch shape as `method_call_invocant_class` but returning
    /// the type, not just the class name. Lets readers that care
    /// about Parametric narrowing (hash-key lookup for DBIC search-
    /// family methods, etc.) inspect the `Parametric` flavor (via
    /// `as_parametric`) without re-resolving the invocant from scratch.
    pub fn method_call_invocant_type(
        &self,
        r: &Ref,
        module_index: Option<&ModuleIndex>,
    ) -> Option<InferredType> {
        let RefKind::MethodCall { invocant, invocant_span, .. } = &r.kind else {
            return None;
        };
        if invocant.is_empty() {
            return None;
        }
        let point = invocant_span.map(|s| s.start).unwrap_or(r.span.start);

        // Pseudo-invocants (`shift`, `$_[0]`, `__PACKAGE__`) → enclosing class.
        if invocant == "shift" || invocant == "$_[0]" || invocant == "@_[0]" || invocant == "__PACKAGE__" {
            return self.enclosing_class_for_scope(r.scope).map(InferredType::ClassName);
        }

        // Chain / function-call receiver. Unlike the class-name path,
        // this surfaces the full type so `Parametric` narrowing (DBIC
        // `search`/`find` row-class) survives the hop — and that needs
        // the *innermost* receiver ref (via `call_ref_by_start`), not an
        // exact-span match, because the Parametric witness lands on the
        // receiver-producing call's `Expression(refidx)`. `expr_type_at_
        // span`'s exact-span chase intentionally collapses that flavor to
        // a plain class, so the class-name path can route through it but
        // this one cannot.
        if let Some(span) = invocant_span {
            if let Some(&recv_idx) = self.call_ref_by_start.get(&span.start) {
                let recv_span = self.refs[recv_idx].span;
                let contained = recv_span.start == span.start
                    && (recv_span.end.row, recv_span.end.column)
                        <= (span.end.row, span.end.column);
                let is_self = std::ptr::eq(&self.refs[recv_idx], r);
                if contained && !is_self {
                    match &self.refs[recv_idx].kind {
                        RefKind::MethodCall { .. } => {
                            return self.method_call_return_type_via_bag(recv_idx, module_index);
                        }
                        RefKind::FunctionCall { .. } => {
                            return self.sub_return_type_at_arity(
                                &self.refs[recv_idx].target_name,
                                Some(0),
                            );
                        }
                        _ => {}
                    }
                }
            }
        }

        // Variable invocant.
        let first = invocant.as_bytes()[0];
        if first == b'$' || first == b'@' || first == b'%' {
            if let Some(t) = self.inferred_type_via_bag_ctx(invocant, point, module_index) {
                return Some(t);
            }
            if invocant == "$self" {
                return self.enclosing_class_for_scope(r.scope).map(InferredType::ClassName);
            }
            return None;
        }

        // Bareword: the bareword *is* the class (or a zero-arg
        // ClassName-returning sub — same rule as
        // `method_call_invocant_class`'s bareword branch).
        let bare = invocant.rsplit("::").next().unwrap_or(invocant);
        if let Some(InferredType::ClassName(c)) = self.sub_return_type_at_arity(bare, Some(0)) {
            return Some(InferredType::ClassName(c));
        }
        Some(InferredType::ClassName(invocant.clone()))
    }

    /// Walk the scope chain to find the enclosing class or package.
    fn enclosing_class_for_scope(&self, scope: ScopeId) -> Option<String> {
        for sid in self.scope_chain(scope).iter() {
            let s = self.scope(*sid);
            if let ScopeKind::Class { ref name } = s.kind {
                return Some(name.clone());
            }
            if let Some(ref pkg) = s.package {
                return Some(pkg.clone());
            }
        }
        None
    }

    /// Test-only wrapper over the private `resolve_invocant_class`.
    #[cfg(test)]
    pub(crate) fn resolve_invocant_class_test(
        &self,
        invocant: &str,
        scope: ScopeId,
        point: Point,
    ) -> Option<String> {
        self.resolve_invocant_class(invocant, scope, point)
    }

    /// Resolve an invocant string to a class name. Internal helper
    /// used by string-based completion / hover-context paths that
    /// don't have a `Ref` in hand. The ref-aware
    /// `method_call_invocant_class` is preferred everywhere a
    /// `&Ref` is available.
    fn resolve_invocant_class(&self, invocant: &str, scope: ScopeId, point: Point) -> Option<String> {
        if invocant.starts_with('$') || invocant.starts_with('@') || invocant.starts_with('%') {
            // Variable invocant → infer type via the witness bag so
            // framework/branch/arity rules refine the answer (falls
            // back to legacy `inferred_type` internally).
            self.inferred_type_via_bag(invocant, point)
                .and_then(|t| t.class_name().map(|s| s.to_string()))
                .or_else(|| {
                    // Enclosing-class fallback only applies to
                    // `$self` — other variable invocants whose type
                    // we don't know stay None, not poisoned with the
                    // surrounding package. Otherwise `$r->to(...)`
                    // with `$r` un-typed would pretend `to` is a
                    // method on the enclosing package (MyApp), and
                    // goto-def on the method name lands on
                    // `package MyApp;`. The comment here described
                    // this guard but the code didn't actually check.
                    if invocant != "$self" {
                        return None;
                    }
                    let chain = self.scope_chain(scope);
                    for scope_id in &chain {
                        let s = self.scope(*scope_id);
                        if let ScopeKind::Class { ref name } = s.kind {
                            return Some(name.clone());
                        }
                        if let Some(ref pkg) = s.package {
                            return Some(pkg.clone());
                        }
                    }
                    None
                })
        } else {
            // Bareword invocant: ambiguous between class-name and
            // zero-arg function call. If a sub by this name resolves
            // (locally or via a cross-file import) to a ClassName
            // return type when called with zero args, treat the
            // bareword as the call and use that class. Mirrors the
            // same rule in `receiver_type_for` and
            // `resolve_invocant_class_tree`.
            let bare = invocant.rsplit("::").next().unwrap_or(invocant);
            if let Some(InferredType::ClassName(c)) =
                self.sub_return_type_at_arity(bare, Some(0))
            {
                return Some(c);
            }
            Some(invocant.to_string())
        }
    }

    /// Find a method definition within a class/package.
    #[cfg(test)]
    pub(crate) fn find_method_in_class(&self, class_name: &str, method_name: &str) -> Option<Span> {
        self.find_method_in_class_with_index(class_name, method_name, None)
    }

    /// Find a method definition, walking the inheritance chain if needed.
    #[cfg(test)]
    fn find_method_in_class_with_index(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Option<Span> {
        match self.resolve_method_in_ancestors(class_name, method_name, module_index) {
            Some(MethodResolution::Local { sym_id, .. }) => {
                Some(self.symbol(sym_id).selection_span)
            }
            Some(MethodResolution::CrossFile { .. }) => {
                // Cross-file method found but no local span to return
                // Caller should handle cross-file resolution via ModuleIndex
                None
            }
            None => None,
        }
    }

    /// Single-source-of-truth DFS ancestor walk for every per-class
    /// lookup on this file: walks `class_name` and every ancestor
    /// (local `package_parents` ∪ cross-file `parents_cached`),
    /// cycle-safe via a `seen` set, depth-capped at 20 (Perl's default
    /// MRO bound). Visitor decides when to stop via `ControlFlow::Break`.
    ///
    /// Both `resolve_method_in_ancestors` (find-first) and
    /// `for_each_dispatch_handler_on_class` (collect-all) route
    /// through here so the two code paths can never drift on MRO
    /// rules. New ancestor-aware queries should reuse it too rather
    /// than reroll the walk.
    fn for_each_ancestor_class(
        &self,
        class_name: &str,
        module_index: Option<&ModuleIndex>,
        mut visit: impl FnMut(&str) -> std::ops::ControlFlow<()>,
    ) {
        // Perl's default MRO is left-to-right depth-first: for
        // `@ISA = (A, B)`, walk A and all A's ancestors before
        // visiting B. A stack-based DFS achieves this by pushing
        // parents in REVERSE order so LIFO pops them in `@ISA`
        // order. The previous implementation pushed left-to-right
        // and visited parents in reverse `@ISA` — wrong for
        // method-resolution semantics, surfaced as same-name
        // overrides on a later parent silently winning over an
        // earlier one. Local + cross-file parents concatenate in
        // `@ISA` order before the reverse-push.
        let mut seen: HashSet<String> = HashSet::new();
        let mut stack: Vec<String> = vec![class_name.to_string()];
        while let Some(cur) = stack.pop() {
            if !seen.insert(cur.clone()) { continue; }
            if seen.len() > 21 { break; } // matches the legacy depth guard
            if let std::ops::ControlFlow::Break(()) = visit(&cur) {
                return;
            }
            let parents = parents_of(
                &cur,
                &self.package_parents,
                module_index,
                &self.app_surface_consumers,
            );
            for p in parents.into_iter().rev() {
                stack.push(p);
            }
        }
    }

    /// Inheritance chain for a method rename: `[class, ..., defining_class]`.
    ///
    /// Cross-class method rename has to touch two distinct things:
    ///   * the `sub M` definition in whichever ancestor actually
    ///     defines the method, and
    ///   * every `$obj->M(...)` call site whose static `invocant_class`
    ///     is the rename target *or* an intermediate ancestor that
    ///     inherited (didn't override) the method.
    ///
    /// `rename_method_in_class` is per-class — so callers iterate this
    /// chain. Stops at the first ancestor that defines the method
    /// (inclusive); intermediate ancestors that *override* are
    /// skipped because they're a different method from the
    /// inheritance perspective.
    pub fn method_rename_chain(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<String> {
        let defining = match self.resolve_method_in_ancestors(class_name, method_name, module_index) {
            Some(MethodResolution::Local { class, .. })
            | Some(MethodResolution::CrossFile { class, .. }) => class,
            None => return vec![class_name.to_string()],
        };
        let mut chain = Vec::new();
        self.for_each_ancestor_class(class_name, module_index, |cls| {
            chain.push(cls.to_string());
            if cls == defining {
                std::ops::ControlFlow::Break(())
            } else {
                std::ops::ControlFlow::Continue(())
            }
        });
        if chain.is_empty() { chain.push(class_name.to_string()); }
        chain
    }

    /// Walk the inheritance chain to find a method (DFS, matches Perl's default MRO).
    pub fn resolve_method_in_ancestors(
        &self,
        class_name: &str,
        method_name: &str,
        module_index: Option<&ModuleIndex>,
    ) -> Option<MethodResolution> {
        let mut result: Option<MethodResolution> = None;
        self.for_each_ancestor_class(class_name, module_index, |cls| {
            // (a) Local symbols in this file packaged under `cls`.
            for &sid in self.symbols_named(method_name) {
                let sym = self.symbol(sid);
                if matches!(sym.kind, SymKind::Sub | SymKind::Method)
                    && self.symbol_in_class(sid, cls)
                {
                    result = Some(MethodResolution::Local {
                        class: cls.to_string(),
                        sym_id: sid,
                    });
                    return std::ops::ControlFlow::Break(());
                }
            }
            // (b) Local plugin-namespace entities bridged to `cls`.
            for ns in &self.plugin_namespaces {
                if !ns.bridges.iter().any(|b| matches!(b, Bridge::Class(c) if c == cls)) { continue; }
                for sym_id in &ns.entities {
                    let Some(sym) = self.symbols.get(sym_id.0 as usize) else { continue };
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                    if sym.name == method_name {
                        result = Some(MethodResolution::Local {
                            class: cls.to_string(),
                            sym_id: *sym_id,
                        });
                        return std::ops::ControlFlow::Break(());
                    }
                }
            }
            // (c) Cross-file: the cached module for `cls` itself
            // (real CPAN/user-defined methods on the class) plus
            // plugin-emitted methods registered via bridges from
            // other workspace files. Per rule #8 the bridge index
            // is the lookup — see `for_each_entity_bridged_to`.
            if let Some(idx) = module_index {
                if let Some(cached) = idx.get_cached(cls) {
                    if cached.has_sub(method_name) {
                        // Real method in `cls`'s own module.
                        result = Some(MethodResolution::CrossFile { class: cls.to_string(), def_module: None });
                        return std::ops::ControlFlow::Break(());
                    }
                }
                // Plugin-bridged method (e.g. a Mojo helper synthesized in
                // another file, bridged to `cls`). The SAME bridge walk
                // completion uses — record which module the symbol actually
                // lives in (its registration key, not a re-derived package
                // name) so consumers don't re-look-it-up in `cls`'s module,
                // where a bridged helper doesn't exist.
                let mut bridged_module: Option<String> = None;
                idx.for_each_entity_bridged_to(cls, |mod_name, _cached, sym| {
                    if bridged_module.is_some() { return; }
                    if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { return; }
                    if sym.name == method_name {
                        bridged_module = Some(mod_name.to_string());
                    }
                });
                if bridged_module.is_some() {
                    result = Some(MethodResolution::CrossFile { class: cls.to_string(), def_module: bridged_module });
                    return std::ops::ControlFlow::Break(());
                }
            }
            std::ops::ControlFlow::Continue(())
        });
        result
    }

    /// Collect every Handler visible from `class_name` whose `dispatchers`
    /// list contains `dispatcher`. Walks the inheritance chain and at
    /// each level pulls Handlers from (1) this file's symbols with
    /// matching owner, (2) this file's `plugin_namespaces` bridged to
    /// the current class, and (3) cross-file via
    /// `module_index.for_each_entity_bridged_to` — rule #8.
    ///
    /// Shares `for_each_ancestor_class` with `resolve_method_in_ancestors`
    /// so the two dispatch paths can't drift on MRO rules. Both
    /// `dispatch_target_completions` and `class_has_dispatch_handlers`
    /// in `symbols.rs` funnel through here.
    ///
    /// `visit(symbol, provenance)` — provenance is `"this file"` for
    /// local hits and the cached module's filename for cross-file
    /// hits (matches the existing detail-string contract).
    pub fn for_each_dispatch_handler_on_class(
        &self,
        class_name: &str,
        dispatcher: &str,
        module_index: Option<&ModuleIndex>,
        mut visit: impl FnMut(&Symbol, &str),
    ) {
        let disp_matches = |dd: &[String]| dd.iter().any(|d| d == dispatcher);
        self.for_each_ancestor_class(class_name, module_index, |cls| {
            // (1) Local Handler symbols owned by this class.
            for sym in &self.symbols {
                if let SymbolDetail::Handler { owner, dispatchers, .. } = &sym.detail {
                    let HandlerOwner::Class(n) = owner;
                    if n == cls && disp_matches(dispatchers) {
                        visit(sym, "this file");
                    }
                }
            }
            // (2) Local plugin-namespace bridge to `cls`.
            for ns in &self.plugin_namespaces {
                if !ns.bridges.iter().any(|b| matches!(b, Bridge::Class(c) if c == cls)) { continue; }
                for sym_id in &ns.entities {
                    let Some(sym) = self.symbols.get(sym_id.0 as usize) else { continue };
                    if let SymbolDetail::Handler { dispatchers, .. } = &sym.detail {
                        if disp_matches(dispatchers) {
                            visit(sym, "this file");
                        }
                    }
                }
            }
            // (3) Cross-file plugin-namespace bridge.
            if let Some(idx) = module_index {
                idx.for_each_entity_bridged_to(cls, |_mod, cached, sym| {
                    if let SymbolDetail::Handler { dispatchers, .. } = &sym.detail {
                        if disp_matches(dispatchers) {
                            let prov = cached.path.file_name()
                                .and_then(|s| s.to_str())
                                .unwrap_or("cross-file");
                            visit(sym, prov);
                        }
                    }
                });
            }
            std::ops::ControlFlow::Continue(())
        });
    }

    /// Hover text for a `Handler` symbol or `DispatchCall` ref. Shows
    /// every stacked registration with its param shape, lists the
    /// dispatcher methods that route to it, and names the owning class.
    /// Walks the module index too so consumer-file hovers cross-file
    /// back to the producer's registrations — critical for the common
    /// case where events are defined in a lib and emitted from scripts.
    fn format_handler_hover(
        &self,
        name: &str,
        owner: &HandlerOwner,
        active_dispatcher: Option<&str>,
        module_index: Option<&ModuleIndex>,
    ) -> String {
        let class = match owner {
            HandlerOwner::Class(n) => n.as_str(),
        };

        // Gather stacked registrations from this file first, then any
        // additional ones in the workspace/dependency cache. Each entry
        // is `(line_number, display_params)` rather than a `&Symbol`
        // reference so cross-file handlers (owned by other
        // FileAnalyses) flow through the same formatting path.
        let mut registrations: Vec<(usize, Vec<String>)> = self.symbols.iter()
            .filter(|s| s.name == name)
            .filter_map(|s| match &s.detail {
                SymbolDetail::Handler { owner: o, params, .. } if o == owner => {
                    Some((s.selection_span.start.row + 1, display_handler_params(params)))
                }
                _ => None,
            })
            .collect();

        // Cross-file walk — now O(matches) instead of O(workspace)
        // via the name-based reverse index on ModuleIndex. Only modules
        // that have a symbol with this name are visited; most of the
        // workspace is skipped without any per-module inspection.
        if let Some(idx) = module_index {
            for module_name in idx.modules_with_symbol(name) {
                let Some(cached) = idx.get_cached(&module_name) else { continue };
                for sym in &cached.analysis.symbols {
                    if sym.name != name { continue; }
                    if let SymbolDetail::Handler { owner: o, params, .. } = &sym.detail {
                        if o == owner {
                            registrations.push((
                                sym.selection_span.start.row + 1,
                                display_handler_params(params),
                            ));
                        }
                    }
                }
            }
        }
        registrations.sort();
        registrations.dedup();

        // Dispatchers: union across stacked registrations, current-file only
        // (plugins declare them consistently, no need to walk deps).
        let registrations_ref: Vec<&Symbol> = self.symbols.iter()
            .filter(|s| s.name == name)
            .filter(|s| matches!(
                &s.detail,
                SymbolDetail::Handler { owner: o, .. } if o == owner
            ))
            .collect();

        // Union dispatcher lists across the current-file registrations.
        let mut dispatchers: Vec<String> = registrations_ref.iter()
            .filter_map(|s| match &s.detail {
                SymbolDetail::Handler { dispatchers, .. } => Some(dispatchers.clone()),
                _ => None,
            })
            .flatten()
            .collect();
        // If we have only cross-file registrations, pull dispatchers from
        // the module index cache too so the hover still shows the
        // dispatcher list to the consumer.
        if dispatchers.is_empty() {
            if let Some(idx) = module_index {
                for module_name in idx.modules_with_symbol(name) {
                    let Some(cached) = idx.get_cached(&module_name) else { continue };
                    for sym in &cached.analysis.symbols {
                        if sym.name != name { continue; }
                        if let SymbolDetail::Handler { owner: o, dispatchers: ds, .. } = &sym.detail {
                            if o == owner { dispatchers.extend(ds.clone()); }
                        }
                    }
                }
            }
        }
        dispatchers.sort();
        dispatchers.dedup();

        let mut text = String::new();
        text.push_str(&format!("**handler `{}`** on `{}`\n\n", name, class));

        if registrations.is_empty() {
            text.push_str("*no handler registered in this workspace — dispatch will be a no-op*");
            return text;
        }

        let plural = if registrations.len() == 1 { "" } else { "s" };
        text.push_str(&format!(
            "*{} registration{} stack{}:*\n\n",
            registrations.len(),
            plural,
            if registrations.len() == 1 { "s" } else { "" },
        ));

        for (line, display) in &registrations {
            text.push_str(&format!(
                "- **line {}:** `({})`\n",
                line,
                display.join(", "),
            ));
        }

        if !dispatchers.is_empty() {
            text.push_str(&format!(
                "\n*Dispatch via:* `{}`",
                dispatchers.iter()
                    .map(|d| {
                        if Some(d.as_str()) == active_dispatcher {
                            format!("**->{}(...)**", d)
                        } else {
                            format!("->{}(...)", d)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        text
    }

    // helpers defined at module scope below

    /// Check if a symbol is defined within a class/package.
    pub(crate) fn symbol_in_class(&self, sym_id: SymbolId, class_name: &str) -> bool {
        let sym = self.symbol(sym_id);
        // Fast path: check the package captured at symbol creation time.
        // This is authoritative for multi-package files where the scope's
        // mutable `package` field gets overwritten by later package statements.
        if let Some(ref pkg) = sym.package {
            return pkg == class_name;
        }
        // Fallback: walk the scope chain for symbols without a package field.
        let start_scope = self.scope_at(sym.span.start).unwrap_or(sym.scope);
        let chain = self.scope_chain(start_scope);
        for scope_id in &chain {
            let s = self.scope(*scope_id);
            if let ScopeKind::Class { ref name } = s.kind {
                return name == class_name;
            }
            if let Some(ref pkg) = s.package {
                return pkg == class_name;
            }
        }
        false
    }

    /// Find the definition span of a package or class by name.
    fn find_package_or_class(&self, name: &str) -> Option<Span> {
        for &sid in self.symbols_named(name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                return Some(sym.selection_span);
            }
        }
        None
    }

    fn format_symbol_hover(&self, sym: &Symbol, source: &str) -> String {
        self.format_symbol_hover_at(sym, source, sym.selection_span.end)
    }

    fn format_symbol_hover_at(&self, sym: &Symbol, source: &str, at: Point) -> String {
        let line = source_line_at(source, sym.span.start.row);
        let mut text = format!("```perl\n{}\n```", line.trim());

        // Append inferred type for variables/fields (bag-routed so
        // framework / branch / arity rules refine the answer).
        if matches!(sym.kind, SymKind::Variable | SymKind::Field) {
            if let Some(it) = self.inferred_type_via_bag(&sym.name, at) {
                text.push_str(&format!("\n\n*type: {}*", format_inferred_type(&it)));
            }
        }

        // Append return type + preceding/POD doc for subs/methods.
        if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
            // Disambiguate `Foo::run` vs `Bar::run` by showing the
            // owning package. Without this, two same-named subs in
            // different packages both render identical hover text
            // and the user can't tell which one the LSP resolved to.
            if let Some(pkg) = sym.package.as_deref() {
                text.push_str(&format!("\n\n*package {}*", pkg));
            }
            if let SymbolDetail::Sub { ref doc, .. } = sym.detail {
                if let Some(rt) = self.symbol_return_type_via_bag(sym.id, None) {
                    text.push_str(&format!("\n\n*returns: {}*", format_inferred_type(&rt)));
                }
                if let Some(d) = doc {
                    text.push_str(&format!("\n\n{}", d));
                }
            }
        }

        text
    }

    // ---- Document outline ----

    /// Build document outline as a nested symbol tree.
    /// Returns (name, detail, kind, span, selection_span, children) tuples.
    pub fn document_symbols(&self) -> Vec<OutlineSymbol> {
        // All top-level entities (subs/methods declared after
        // `package X;`, `my`/`our` decls, `use` imports, …) attach
        // to the file scope directly. Statement-form `package X;`
        // is package context, not a lexical boundary, so it doesn't
        // create an intermediate scope. Children come out in
        // declaration order from the symbols vector.
        //
        // Plugin namespaces are NOT surfaced. They exist for
        // cross-file bridge lookups (`for_each_entity_bridged_to`),
        // not as navigation targets — users look for the
        // helpers/routes/tasks themselves, which already render flat
        // with their `<word>` kind prefix.
        self.outline_children_of(ScopeId(0))
    }

    /// Whether a scope sits inside a sub/method body (directly or via nested
    /// blocks). The single rule behind hiding working-state lexicals from the
    /// outline: variables declared here are local scratch, not structure, so
    /// only file/package- and class-body-scoped variables (`our`, class
    /// `field`s) survive. A `Class` or `File` boundary reached first means
    /// "structural"; a `Sub`/`Method` reached first means "working state".
    /// Both the outline tree builder and the `--outline` CLI ask this.
    pub fn scope_within_sub_body(&self, scope: ScopeId) -> bool {
        let mut cur = Some(scope);
        while let Some(id) = cur {
            let Some(s) = self.scopes.iter().find(|s| s.id == id) else { return false };
            match s.kind {
                ScopeKind::Sub { .. } | ScopeKind::Method { .. } => return true,
                ScopeKind::Class { .. } | ScopeKind::File => return false,
                ScopeKind::Block | ScopeKind::ForLoop { .. } => cur = s.parent,
            }
        }
        false
    }

    fn outline_children_of(&self, parent_scope: ScopeId) -> Vec<OutlineSymbol> {
        let mut result = Vec::new();

        // Plugin fan-out (e.g. Mojo helpers register a Method on both
        // Mojolicious::Controller AND Mojolicious) produces multiple
        // Symbols with the same (name, kind, span). Completion and
        // resolution want them all; the outline wants one entry.
        // Keyed by (kind, name, span_start) — tight enough to dedup
        // true fan-out without collapsing user-written overloads.
        let mut outline_seen: HashSet<(SymKind, String, usize, usize)> = HashSet::new();

        for sym in &self.symbols {
            if sym.scope != parent_scope {
                continue;
            }
            // Per-symbol opt-out. Plugins mark DSL imports / internal
            // infrastructure so the outline stays focused on real
            // user-visible structure.
            let hidden = match &sym.detail {
                SymbolDetail::Sub { hide_in_outline, .. } => *hide_in_outline,
                SymbolDetail::Handler { hide_in_outline, .. } => *hide_in_outline,
                _ => false,
            };
            if hidden { continue; }
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) && sym.namespace.is_framework() {
                let key = (
                    sym.kind,
                    sym.name.clone(),
                    sym.span.start.row,
                    sym.span.start.column,
                );
                if !outline_seen.insert(key) {
                    continue;
                }
            }

            let (name, detail, children) = match sym.kind {
                SymKind::Sub | SymKind::Method => {
                    let body_scope = self.find_body_scope(sym);
                    let children = body_scope
                        .map(|s| self.outline_children_of(s))
                        .unwrap_or_default();
                    // LSP `DocumentSymbol.name` should be the bare
                    // identifier; `kind` (Function/Method) is what
                    // tells the client to render the right icon.
                    //
                    // Plugin-tagged subs are the exception: when a
                    // plugin overrides the display word (helper,
                    // action, route, task, event …), SymbolKind has
                    // no enum value that conveys it, so we keep the
                    // `<word>` prefix in `name` for those — it's the
                    // only surviving kind cue once SymbolKind collapses
                    // Helper/Action/Route → FUNCTION. Native subs and
                    // methods get the spec-compliant bare name and
                    // route their kind word through `detail`.
                    let disp = sub_display_override(&sym.detail);
                    let default_word = if matches!(sym.kind, SymKind::Method) { "method" } else { "sub" };
                    let identifier = sym.outline_label.clone().unwrap_or_else(|| sym.name.clone());
                    let params_suffix = match &sym.detail {
                        SymbolDetail::Sub { params, .. } => {
                            let visible: Vec<&str> = params.iter()
                                .filter(|p| !p.is_invocant)
                                .map(|p| p.name.as_str())
                                .collect();
                            if visible.is_empty() { String::new() }
                            else { format!(" ({})", visible.join(", ")) }
                        }
                        _ => String::new(),
                    };
                    let (label, outline_detail) = match disp.and_then(|d| d.outline_word()) {
                        Some(plugin_word) => (
                            format!("<{}> {}{}", plugin_word, identifier, params_suffix),
                            Some(plugin_word.to_string()),
                        ),
                        None => (
                            identifier,
                            Some(format!("{}{}", default_word, params_suffix)),
                        ),
                    };
                    (label, outline_detail, children)
                }
                SymKind::Class => {
                    let body_scope = self.find_body_scope(sym);
                    let children = body_scope
                        .map(|s| self.outline_children_of(s))
                        .unwrap_or_default();
                    (sym.name.clone(), Some("class".to_string()), children)
                }
                SymKind::Package => {
                    (sym.name.clone(), Some("package".to_string()), Vec::new())
                }
                // `use` statements are not structure — mainstream language
                // servers (rust-analyzer, pyright, tsserver, gopls, clangd)
                // all keep imports out of the document outline. The synthetic
                // expansions a kit plugin emits would be even worse (a dozen
                // per `use Clove::Base 'Controller'`), but real ones are noise
                // too. Modules still drive resolution; they're just not
                // navigation targets.
                SymKind::Module => continue,
                SymKind::Variable => {
                    if self.scope_within_sub_body(sym.scope) { continue; }
                    let detail = match &sym.detail {
                        SymbolDetail::Variable { decl_kind, .. } => match decl_kind {
                            DeclKind::My => "my",
                            DeclKind::Our => "our",
                            DeclKind::State => "state",
                            DeclKind::Field => "field",
                            DeclKind::Param => "param",
                            DeclKind::ForVar => "for",
                        },
                        _ => "my",
                    };
                    (sym.name.clone(), Some(detail.to_string()), Vec::new())
                }
                SymKind::Field => {
                    (sym.name.clone(), Some("field".to_string()), Vec::new())
                }
                SymKind::HashKeyDef => continue, // Skip hash key defs from outline
                SymKind::Handler => {
                    // Show registered handlers in the outline. The
                    // semantic word (`event`/`route`/`task`/…) and
                    // params ride along in the NAME — most outline
                    // clients render only `name`, so baking it there
                    // gives stacked registrations (two handlers on
                    // the same name with different sigs, GET + POST
                    // on the same path) visually distinct entries.
                    // `outline_label` lets the plugin inject a
                    // disambiguator (e.g. HTTP verb) into the
                    // identifier slot without affecting dispatch
                    // lookups, which still key on `sym.name`.
                    let (word, params_suffix) = match &sym.detail {
                        SymbolDetail::Handler { params, display, .. } => {
                            let word = display.outline_word().unwrap_or("handler");
                            let visible: Vec<&str> = params
                                .iter()
                                .filter(|p| !p.is_invocant)
                                .map(|p| p.name.as_str())
                                .collect();
                            let suf = if visible.is_empty() { String::new() }
                                else { format!(" ({})", visible.join(", ")) };
                            (word, suf)
                        }
                        _ => ("handler", String::new()),
                    };
                    let identifier = sym.outline_label.clone().unwrap_or_else(|| sym.name.clone());
                    let label = format!("<{}> {}{}", word, identifier, params_suffix);
                    (label, Some(word.to_string()), Vec::new())
                }
                SymKind::Namespace => {
                    // Real Namespace entries are appended by
                    // `document_symbols` from `plugin_namespaces`,
                    // not from the symbol table. Nothing to do here.
                    continue;
                }
            };

            let handler_display = match &sym.detail {
                SymbolDetail::Handler { display, .. } => Some(*display),
                SymbolDetail::Sub { display: Some(d), .. } => Some(*d),
                _ => None,
            };
            result.push(OutlineSymbol {
                name,
                detail,
                kind: sym.kind,
                span: sym.span,
                selection_span: sym.selection_span,
                children,
                handler_display,
            });
        }

        result
    }

    /// Find the body scope for a sub/method/class symbol.
    fn find_body_scope(&self, sym: &Symbol) -> Option<ScopeId> {
        self.scopes.iter().find(|s| {
            let kind_matches = match (&s.kind, &sym.kind) {
                (ScopeKind::Sub { name: sn }, SymKind::Sub) => sn == &sym.name,
                (ScopeKind::Method { name: mn }, SymKind::Method) => mn == &sym.name,
                (ScopeKind::Class { name: cn }, SymKind::Class) => cn == &sym.name,
                _ => false,
            };
            kind_matches && s.span == sym.span
        }).map(|s| s.id)
    }

    // ---- Semantic tokens ----

    /// Collect semantic tokens for all variable references and declarations.
    pub fn semantic_tokens(&self) -> Vec<PerlSemanticToken> {
        let mut tokens: Vec<PerlSemanticToken> = Vec::new();

        // ---- Variable/parameter/self declarations from symbols ----
        for sym in &self.symbols {
            match sym.kind {
                SymKind::Variable | SymKind::Field => {
                    let is_self = sym.name == "$self" || sym.name == "$class";
                    let (sigil, is_readonly, is_param) = match &sym.detail {
                        SymbolDetail::Variable { sigil, decl_kind } => {
                            let readonly = matches!(decl_kind, DeclKind::Field);
                            let is_param = matches!(decl_kind, DeclKind::Param | DeclKind::ForVar);
                            (*sigil, readonly, is_param)
                        }
                        SymbolDetail::Field { sigil, attributes } => {
                            let readonly = !attributes.iter().any(|a| a == "writer" || a == "mutator" || a == "accessor");
                            (*sigil, readonly, true)
                        }
                        _ => continue,
                    };
                    let token_type = if is_self { TOK_KEYWORD } else if is_param { TOK_PARAMETER } else { TOK_VARIABLE };
                    // Don't add sigil modifier for $self/$class — it would override the keyword color
                    let mut mods = if is_self { 0 } else { sigil_modifier(sigil) };
                    mods |= 1 << MOD_DECLARATION;
                    if is_readonly { mods |= 1 << MOD_READONLY; }
                    tokens.push(PerlSemanticToken { span: sym.selection_span, token_type, modifiers: mods });
                }
                SymKind::Package | SymKind::Class => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_NAMESPACE,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                SymKind::Module => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_NAMESPACE,
                        modifiers: 0,
                    });
                }
                SymKind::Sub => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_FUNCTION,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                SymKind::Method => {
                    tokens.push(PerlSemanticToken {
                        span: sym.selection_span,
                        token_type: TOK_METHOD,
                        modifiers: 1 << MOD_DECLARATION,
                    });
                }
                _ => {}
            }
        }

        // ---- Refs: variables, functions, methods, properties, namespaces ----
        let imported_names: std::collections::HashSet<&str> = self.imports.iter()
            .flat_map(|imp| imp.imported_symbols.iter().map(|s| s.local_name.as_str()))
            .collect();

        for r in &self.refs {
            // Skip declaration refs — the symbol loop already emits tokens for declarations
            if matches!(r.access, AccessKind::Declaration) {
                continue;
            }
            match &r.kind {
                RefKind::Variable | RefKind::ContainerAccess => {
                    let sigil = r.target_name.chars().next().unwrap_or('$');
                    let is_self = r.target_name == "$self" || r.target_name == "$class";
                    let token_type = if is_self { TOK_KEYWORD } else { TOK_VARIABLE };
                    // Don't add sigil modifier for $self/$class — it would override the keyword color
                    let mut mods = if is_self { 0 } else { sigil_modifier(sigil) };
                    if matches!(r.access, AccessKind::Write) { mods |= 1 << MOD_MODIFICATION; }
                    tokens.push(PerlSemanticToken { span: r.span, token_type, modifiers: mods });
                }
                RefKind::FunctionCall { .. } => {
                    // Framework DSL keywords → macro, otherwise function
                    let token_type = if self.framework_imports.contains(r.target_name.as_str()) {
                        TOK_MACRO
                    } else {
                        TOK_FUNCTION
                    };
                    let mut mods = 0;
                    if imported_names.contains(r.target_name.as_str()) {
                        mods |= 1 << MOD_DEFAULT_LIBRARY;
                    }
                    tokens.push(PerlSemanticToken { span: r.span, token_type, modifiers: mods });
                }
                RefKind::MethodCall { method_name_span, .. } => {
                    // Use method_name_span for precise highlighting of just the method name
                    let mods = 0; // TODO: readonly for ro accessors, static for class methods
                    tokens.push(PerlSemanticToken { span: *method_name_span, token_type: TOK_METHOD, modifiers: mods });
                }
                RefKind::PackageRef => {
                    tokens.push(PerlSemanticToken { span: r.span, token_type: TOK_NAMESPACE, modifiers: 0 });
                }
                RefKind::HashKeyAccess { .. } => {
                    tokens.push(PerlSemanticToken { span: r.span, token_type: TOK_PROPERTY, modifiers: 0 });
                }
                RefKind::DispatchCall { .. } => {
                    // Colors dispatch-call event names like property keys —
                    // same visual weight as other named members of a class.
                    tokens.push(PerlSemanticToken { span: r.span, token_type: TOK_PROPERTY, modifiers: 0 });
                }
            }
        }

        // ---- HashKeyDef symbols → property tokens ----
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::HashKeyDef) {
                tokens.push(PerlSemanticToken {
                    span: sym.selection_span,
                    token_type: TOK_PROPERTY,
                    modifiers: 1 << MOD_DECLARATION,
                });
            }
        }

        tokens.sort_by_key(|t| (t.span.start.row, t.span.start.column));
        // Dedup by position — if two tokens start at the same (row, col), keep the first
        tokens.dedup_by(|b, a| a.span.start.row == b.span.start.row && a.span.start.column == b.span.start.column);
        tokens
    }
}

// ---- Completion priority constants ----
//
// Lower numbers sort first. Used by both file_analysis (local completions)
// and symbols.rs (cross-file completions).

/// Variables, local subs, methods — direct scope match.
pub const PRIORITY_LOCAL: u8 = 0;
/// General subs, hash keys, keyval args — file-wide match.
pub const PRIORITY_FILE_WIDE: u8 = 10;
/// Explicitly imported via `use Foo qw(bar)`.
pub const PRIORITY_EXPLICIT_IMPORT: u8 = 12;
/// Bare `use Foo;` @EXPORT symbol (no qw list to edit).
pub const PRIORITY_BARE_IMPORT: u8 = 15;
/// Auto-add to existing `qw()` list.
pub const PRIORITY_AUTO_ADD_QW: u8 = 18;
/// Sub already used as first param (less relevant).
pub const PRIORITY_LESS_RELEVANT: u8 = 20;
/// Unimported module — inserts full `use` statement.
pub const PRIORITY_UNIMPORTED: u8 = 25;
/// Dynamic hash keys (may not exist).
pub const PRIORITY_DYNAMIC: u8 = 50;

// ---- Method resolution types ----

/// Result of resolving a method through the inheritance chain.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum MethodResolution {
    /// Found in a local class within this file.
    Local { class: String, sym_id: SymbolId },
    /// Found in a cross-file module. `def_module` names the module the
    /// definition actually lives in: `Some(m)` when the hit came through a
    /// plugin BRIDGE (the synthesized symbol lives in bridging module `m`, not
    /// in `class`'s own module); `None` for a real method in `class`'s module.
    /// Every consumer resolves location/signature the same way —
    /// `get_cached(def_module.unwrap_or(class)).sub_info(method)` — so bridged
    /// helpers and real inherited methods share one code path.
    CrossFile { class: String, def_module: Option<String> },
}

/// Result of resolving a sub/method call — local symbol or cross-file metadata.
pub enum ResolvedSub<'a> {
    /// Found locally in this file's symbols.
    Local(&'a Symbol),
    /// Found in a cross-file module via ModuleIndex.
    CrossFile {
        params: Vec<ParamInfo>,
        /// Inferred type per param (parallel to `params`); `None` if unknown.
        param_types: Vec<Option<InferredType>>,
        is_method: bool,
        hash_keys: Vec<String>,
    },
}

// ---- Completion types ----

/// A completion candidate from FileAnalysis resolution (pure table lookup).
#[derive(Debug, Clone)]
pub struct CompletionCandidate {
    pub label: String,
    pub kind: SymKind,
    pub detail: Option<String>,
    pub insert_text: Option<String>,
    pub sort_priority: u8,
    /// Additional text edits applied when this candidate is accepted (e.g. auto-import).
    pub additional_edits: Vec<(Span, String)>,
    /// Plugin-provided display override. When `Some`, the LSP adapter renders
    /// the candidate with this kind instead of `kind`'s default mapping. Lets
    /// helpers/routes/DSL verbs carry their plugin-chosen icon all the way
    /// through completion without leaking plugin specifics into the core.
    pub display_override: Option<HandlerDisplay>,
}

/// Signature info for a sub/method, resolved from the symbol table.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct SignatureInfo {
    pub name: String,
    pub params: Vec<ParamInfo>,
    pub is_method: bool,
    /// End of the sub body — used to query inferred types for params.
    pub body_end: Point,
    /// Pre-resolved param types (for cross-file subs where body_end is meaningless).
    pub param_types: Option<Vec<Option<String>>>,
}

// ---- Completion query methods ----

impl FileAnalysis {
    /// Complete variables at a point with cross-sigil forms.
    pub fn complete_variables(&self, point: Point, sigil: char) -> Vec<CompletionCandidate> {
        let visible = self.visible_symbols(point);
        let mut seen = HashSet::<(String, char)>::new();
        let mut candidates = Vec::new();

        // Sort by scope size (innermost first) — stable priority ordering
        let mut vars: Vec<(&Symbol, usize)> = visible
            .into_iter()
            .filter(|s| matches!(s.kind, SymKind::Variable | SymKind::Field))
            .filter_map(|s| {
                if let SymbolDetail::Variable { .. } = &s.detail {
                    let scope = &self.scopes[s.scope.0 as usize];
                    let scope_size = span_size(&scope.span);
                    Some((s, scope_size))
                } else if let SymbolDetail::Field { .. } = &s.detail {
                    let scope = &self.scopes[s.scope.0 as usize];
                    let scope_size = span_size(&scope.span);
                    Some((s, scope_size))
                } else {
                    None
                }
            })
            .collect();
        vars.sort_by_key(|(_, sz)| *sz);

        for (sym, scope_size) in vars {
            let (bare_name, decl_sigil) = match &sym.detail {
                SymbolDetail::Variable { sigil: ds, .. } => {
                    (sym.name[1..].to_string(), *ds)
                }
                SymbolDetail::Field { sigil: ds, .. } => {
                    (sym.name[1..].to_string(), *ds)
                }
                _ => continue,
            };
            let key = (bare_name.clone(), decl_sigil);
            if seen.contains(&key) {
                continue;
            }
            seen.insert(key);

            let priority = std::cmp::min(scope_size, 255) as u8;
            let detail = match &sym.detail {
                SymbolDetail::Variable { decl_kind, .. } => {
                    Some(match decl_kind {
                        DeclKind::My => "my".to_string(),
                        DeclKind::Our => "our".to_string(),
                        DeclKind::State => "state".to_string(),
                        DeclKind::Field => "field".to_string(),
                        DeclKind::Param => "param".to_string(),
                        DeclKind::ForVar => "for".to_string(),
                    })
                }
                SymbolDetail::Field { .. } => Some("field".to_string()),
                _ => None,
            };

            generate_cross_sigil_candidates(
                &bare_name,
                decl_sigil,
                sigil,
                detail,
                priority,
                &mut candidates,
            );
        }

        candidates
    }

    /// Complete methods for an invocant (variable or class name) at a point.
    pub fn complete_methods(&self, invocant: &str, point: Point) -> Vec<CompletionCandidate> {
        let class_name = if !invocant.starts_with('$')
            && !invocant.starts_with('@')
            && !invocant.starts_with('%')
        {
            Some(invocant.to_string())
        } else {
            self.resolve_invocant_class(
                invocant,
                self.scope_at(point).unwrap_or(ScopeId(0)),
                point,
            )
        };

        if let Some(ref cn) = class_name {
            let candidates = self.complete_methods_for_class(cn, None);
            if !candidates.is_empty() {
                return candidates;
            }
        }

        // Fallback: native subs/methods in file, deduped by label.
        //
        // Plugin-synthesized entries are skipped on purpose. A plugin
        // emits Methods on specific classes (Mojo helpers on
        // Controller, DBIC accessors on the schema, etc.); without a
        // known receiver type we can't say which ones apply here.
        // Surfacing them blindly dumps framework noise onto every
        // untyped `$x->` call site. Native entries stay — those are
        // always valid candidates regardless of receiver.
        let mut seen = HashSet::<String>::new();
        self.symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Sub | SymKind::Method))
            .filter(|s| !s.namespace.is_framework())
            .filter(|s| seen.insert(s.name.clone()))
            .map(|s| CompletionCandidate {
                label: s.name.clone(),
                kind: s.kind,
                detail: Some(
                    if matches!(s.kind, SymKind::Method) {
                        "method"
                    } else {
                        "sub"
                    }
                    .to_string(),
                ),
                insert_text: None,
                sort_priority: PRIORITY_FILE_WIDE,
                additional_edits: vec![],
                display_override: sub_display_override(&s.detail),
            })
            .collect()
    }

    /// Complete hash keys for a resolved owner.
    fn complete_hash_keys_for_owner(&self, owner: &HashKeyOwner) -> Vec<CompletionCandidate> {
        let defs = self.hash_key_defs_for_owner(owner);
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();

        for def in defs {
            if !seen.insert(def.name.clone()) {
                continue;
            }

            let is_dynamic = matches!(
                &def.detail,
                SymbolDetail::HashKeyDef { is_dynamic: true, .. }
            );

            let detail = match owner {
                HashKeyOwner::Class(name) => format!("{}->{{{}}}", name, def.name),
                HashKeyOwner::Variable { name, .. } => format!("{}{{{}}}", name, def.name),
                HashKeyOwner::Sub { name, .. } => format!("{}()->{{{}}}", name, def.name),
            };

            candidates.push(CompletionCandidate {
                label: def.name.clone(),
                kind: SymKind::Variable,
                detail: Some(detail),
                insert_text: None,
                sort_priority: if is_dynamic { PRIORITY_DYNAMIC } else { PRIORITY_FILE_WIDE },
                additional_edits: vec![],
                display_override: None,
            });
        }

        candidates
    }

    /// Complete hash keys for a variable at a point.
    pub fn complete_hash_keys(&self, var_text: &str, point: Point) -> Vec<CompletionCandidate> {
        match self.resolve_hash_key_owner(var_text, point) {
            Some(owner) => self.complete_hash_keys_for_owner(&owner),
            None => Vec::new(),
        }
    }

    /// Complete hash keys for a known class name (from expression type resolution).
    pub fn complete_hash_keys_for_class(&self, class_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        self.complete_hash_keys_for_owner(&HashKeyOwner::Class(class_name.to_string()))
    }

    /// Complete hash keys for a sub's return value (from expression type resolution).
    ///
    /// Tries the caller's enclosing package first (local subs), then falls
    /// back to an unpackaged owner (imported subs whose synthetic HashKeyDef
    /// was added during enrichment with `package: None`).
    pub fn complete_hash_keys_for_sub(&self, sub_name: &str, _point: Point) -> Vec<CompletionCandidate> {
        // Try each candidate owner variant until one has defs.
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        let push_unique = |cands: Vec<CompletionCandidate>, out: &mut Vec<CompletionCandidate>, seen: &mut HashSet<String>| {
            for c in cands {
                if seen.insert(c.label.clone()) {
                    out.push(c);
                }
            }
        };
        for sym in &self.symbols {
            if (sym.kind == SymKind::Sub || sym.kind == SymKind::Method) && sym.name == sub_name {
                let owner = HashKeyOwner::Sub { package: sym.package.clone(), name: sub_name.to_string() };
                push_unique(self.complete_hash_keys_for_owner(&owner), &mut out, &mut seen);
            }
        }
        let imported_owner = HashKeyOwner::Sub { package: None, name: sub_name.to_string() };
        push_unique(self.complete_hash_keys_for_owner(&imported_owner), &mut out, &mut seen);

        // Final sweep: match any HashKeyDef whose owner is Sub{name: sub_name}
        // regardless of package. Covers plugin-synthesized options (e.g.
        // `Sub { package: Minion, name: enqueue }` from the minion plugin)
        // where the sub itself isn't in the local symbol table.
        let pseudo_syms: Vec<_> = self.symbols.iter()
            .filter(|s| {
                if !matches!(s.kind, SymKind::HashKeyDef) { return false; }
                matches!(&s.detail, SymbolDetail::HashKeyDef {
                    owner: HashKeyOwner::Sub { name, .. }, ..
                } if name == sub_name)
            })
            .collect();
        for def in pseudo_syms {
            if seen.insert(def.name.clone()) {
                let detail = format!("{}() option", sub_name);
                out.push(CompletionCandidate {
                    label: def.name.clone(),
                    kind: SymKind::Variable,
                    detail: Some(detail),
                    insert_text: None,
                    sort_priority: PRIORITY_FILE_WIDE,
                    additional_edits: vec![],
                    display_override: None,
                });
            }
        }

        // Body-derived keys: if the sub has a final hashref-ish param
        // (`sub foo { my ($x, $opts) = @_; $opts->{priority} }`), the
        // key accesses in the body reveal the expected option names.
        // Mirror of `complete_keyval_args` but for the nested-hash-literal
        // call shape (`foo($x, { | })`) routed through HashKey context.
        for sym in &self.symbols {
            if sym.name != sub_name { continue; }
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
            let params = match &sym.detail {
                SymbolDetail::Sub { params, .. } => params,
                _ => continue,
            };
            let hashish = params
                .iter()
                .find(|p| p.is_slurpy && p.name.starts_with('%'))
                .or_else(|| params
                    .last()
                    .filter(|p| !p.is_invocant && p.name.starts_with('$')));
            let bare_name = match hashish {
                Some(p) if p.name.len() > 1 => &p.name[1..],
                _ => continue,
            };
            let Some(body_scope) = self.find_body_scope(sym) else { continue };
            for k in self.hash_keys_in_scope(bare_name, body_scope) {
                if seen.insert(k.clone()) {
                    out.push(CompletionCandidate {
                        label: k,
                        kind: SymKind::Variable,
                        detail: Some(format!("{}() option", sub_name)),
                        insert_text: None,
                        sort_priority: PRIORITY_FILE_WIDE,
                        additional_edits: vec![],
                        display_override: None,
                    });
                }
            }
        }

        out
    }

    /// General completion: all variables (all sigils) + subs + packages.
    pub fn complete_general(&self, point: Point) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();

        // Variables (all sigils)
        for sigil in ['$', '@', '%'] {
            candidates.extend(self.complete_variables(point, sigil));
        }

        // Subs
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                candidates.push(CompletionCandidate {
                    label: sym.name.clone(),
                    kind: sym.kind,
                    detail: Some(
                        if matches!(sym.kind, SymKind::Method) {
                            "method"
                        } else {
                            "sub"
                        }
                        .to_string(),
                    ),
                    insert_text: None,
                    sort_priority: PRIORITY_FILE_WIDE,
                    additional_edits: vec![],
                display_override: None,
                });
            }
        }

        // Packages/classes
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Package | SymKind::Class) {
                candidates.push(CompletionCandidate {
                    label: sym.name.clone(),
                    kind: sym.kind,
                    detail: Some(
                        if matches!(sym.kind, SymKind::Class) {
                            "class"
                        } else {
                            "package"
                        }
                        .to_string(),
                    ),
                    insert_text: None,
                    sort_priority: PRIORITY_LESS_RELEVANT,
                    additional_edits: vec![],
                display_override: None,
                });
            }
        }

        candidates
    }

    /// Complete keyval args at a call site.
    /// Returns `key =>` completions for unused keys.
    pub fn complete_keyval_args(
        &self,
        call_name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        used_keys: &HashSet<String>,
        module_index: Option<&ModuleIndex>,
    ) -> Vec<CompletionCandidate> {
        // For `new` calls on a class, check for :param fields
        if call_name == "new" {
            if let Some(inv) = invocant {
                let class_name = if !inv.starts_with('$') {
                    Some(inv.to_string())
                } else {
                    self.resolve_invocant_class(
                        inv,
                        self.scope_at(point).unwrap_or(ScopeId(0)),
                        point,
                    )
                };
                if let Some(ref cn) = class_name {
                    let param_candidates = self.class_param_completions(cn, used_keys);
                    if !param_candidates.is_empty() {
                        return param_candidates;
                    }
                }
            }
        }

        // Find the sub definition (local or cross-file)
        let resolved = match self.find_sub_for_call(call_name, is_method, invocant, point, module_index) {
            Some(r) => r,
            None => return Vec::new(),
        };

        match resolved {
            ResolvedSub::Local(sub_sym) => {
                let params = match &sub_sym.detail {
                    SymbolDetail::Sub { params, .. } => params,
                    _ => return Vec::new(),
                };

                // Pick the param that carries key=>value pairs:
                //   * slurpy `%opts` is the classic shape
                //   * final `$opts` scalar deref'd as a hashref in the
                //     body (`$opts->{…}`) is the same pattern — we
                //     collect the same way, just strip the `$` sigil
                //     when scanning the body. Previously only slurpy
                //     worked; every "options-hashref" sub missed out.
                let hashish = params
                    .iter()
                    .find(|p| p.is_slurpy && p.name.starts_with('%'))
                    .or_else(|| params
                        .last()
                        .filter(|p| !p.is_invocant && p.name.starts_with('$')));
                let slurpy_name = match hashish {
                    Some(p) => {
                        if p.name.starts_with('%') || p.name.starts_with('$') || p.name.starts_with('@') {
                            &p.name[1..]
                        } else {
                            &p.name
                        }
                    }
                    None => return Vec::new(),
                };

                // Find hash key accesses for this param name within the sub's body scope
                let body_scope = self.find_body_scope(sub_sym);
                let keys = match body_scope {
                    Some(scope_id) => self.hash_keys_in_scope(slurpy_name, scope_id),
                    None => Vec::new(),
                };
                // Bail silently when the chosen scalar doesn't actually
                // get deref'd as a hash — the last-param heuristic is
                // loose; no accesses means "not an options param".
                if keys.is_empty() { return Vec::new(); }

                keys.into_iter()
                    .filter(|k| !used_keys.contains(k))
                    .map(|k| CompletionCandidate {
                        label: format!("{} =>", k),
                        kind: SymKind::Variable,
                        detail: Some(format!("{}(%{})", call_name, slurpy_name)),
                        insert_text: Some(format!("{} => ", k)),
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                display_override: None,
                    })
                    .collect()
            }
            ResolvedSub::CrossFile { hash_keys, params, .. } => {
                // Check if any param is slurpy %hash
                let has_slurpy = params.iter().any(|p| p.is_slurpy && p.name.starts_with('%'));
                if !has_slurpy || hash_keys.is_empty() {
                    return Vec::new();
                }

                hash_keys.into_iter()
                    .filter(|k| !used_keys.contains(k))
                    .map(|k| CompletionCandidate {
                        label: format!("{} =>", k),
                        kind: SymKind::Variable,
                        detail: Some(format!("{}()", call_name)),
                        insert_text: Some(format!("{} => ", k)),
                        sort_priority: PRIORITY_LOCAL,
                        additional_edits: vec![],
                display_override: None,
                    })
                    .collect()
            }
        }
    }

    /// Resolve signature info for a call (sub/method name).
    pub fn signature_for_call(
        &self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<SignatureInfo> {
        let resolved = self.find_sub_for_call(name, is_method, invocant, point, module_index)?;

        match resolved {
            ResolvedSub::Local(sub_sym) => {
                let (params, sym_is_method) = match &sub_sym.detail {
                    SymbolDetail::Sub { params, is_method, .. } => (params.clone(), *is_method),
                    _ => return None,
                };

                let mut params = params;
                let is_method = is_method
                    || sym_is_method
                    || params
                        .first()
                        .map_or(false, |p| p.name == "$self" || p.name == "$class");

                // Strip the implicit invocant from the display list.
                // `is_invocant` covers both Perl-native `$self`/`$class`
                // (flagged by the builder at extract time) and plugin-
                // marked framework invocants (`$c` for helpers, etc.).
                if !params.is_empty() && params[0].is_invocant {
                    params.remove(0);
                }

                Some(SignatureInfo {
                    name: name.to_string(),
                    params,
                    is_method,
                    body_end: sub_sym.span.end,
                    param_types: None, // local — use inferred_type() with body_end
                })
            }
            ResolvedSub::CrossFile {
                params: cross_params,
                param_types: cross_param_types,
                is_method: cf_is_method,
                ..
            } => {
                let mut params: Vec<ParamInfo> = cross_params;
                let mut param_types: Vec<Option<String>> = cross_param_types
                    .into_iter()
                    .map(|t| t.as_ref().map(inferred_type_to_tag))
                    .collect();

                let is_method = is_method || cf_is_method
                    || params.first().map_or(false, |p| p.name == "$self" || p.name == "$class");

                // Same invocant-strip as the local branch — by flag,
                // not by name. Cross-file ParamInfo carries the flag
                // through the cache (set by the plugin or builder at
                // build time), so `$c` on a helper is dropped the
                // same way `$self` on a Perl method is.
                if !params.is_empty() && params[0].is_invocant {
                    params.remove(0);
                    if !param_types.is_empty() {
                        param_types.remove(0);
                    }
                }

                Some(SignatureInfo {
                    name: name.to_string(),
                    params,
                    is_method,
                    body_end: Point::new(0, 0),
                    param_types: Some(param_types),
                })
            }
        }
    }

    // ---- Internal completion helpers ----

    /// Find a sub/method by name, optionally scoped to a class.
    /// Returns `ResolvedSub::Local` for same-file symbols, or `ResolvedSub::CrossFile`
    /// for inherited methods and imported functions found via `ModuleIndex`.
    fn find_sub_for_call<'s>(
        &'s self,
        name: &str,
        is_method: bool,
        invocant: Option<&str>,
        point: Point,
        module_index: Option<&ModuleIndex>,
    ) -> Option<ResolvedSub<'s>> {
        // Resolve class name for scoped lookup
        let class_name = if is_method {
            invocant.and_then(|inv| {
                if !inv.starts_with('$') && !inv.starts_with('@') && !inv.starts_with('%') {
                    Some(inv.to_string())
                } else {
                    self.resolve_invocant_class(
                        inv,
                        self.scope_at(point).unwrap_or(ScopeId(0)),
                        point,
                    )
                }
            })
        } else {
            None
        };

        // Try inheritance-aware class-scoped lookup first
        if let Some(ref cn) = class_name {
            match self.resolve_method_in_ancestors(cn, name, module_index) {
                Some(MethodResolution::Local { sym_id, .. }) => {
                    return Some(ResolvedSub::Local(self.symbol(sym_id)));
                }
                Some(MethodResolution::CrossFile { ref class, .. }) => {
                    if let Some(idx) = module_index {
                        if let Some(cached) = idx.get_cached(class) {
                            if let Some(sub_info) = cached.sub_info(name) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
                None => {}
            }
        }

        // Fallback: any local sub/method with that name
        for &sid in self.symbols_named(name) {
            let sym = self.symbol(sid);
            if matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                return Some(ResolvedSub::Local(sym));
            }
        }

        // Fallback: imported function via ModuleIndex
        if !is_method {
            if let Some(idx) = module_index {
                for import in &self.imports {
                    if let Some(is) = import.imported_symbols.iter().find(|s| s.local_name == *name) {
                        if let Some(cached) = idx.get_cached(&import.module_name) {
                            if let Some(sub_info) = cached.sub_info(is.remote()) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
                // Also check @EXPORT (bare imports)
                for import in &self.imports {
                    if let Some(cached) = idx.get_cached(&import.module_name) {
                        if cached.analysis.export.iter().any(|s| s == name) {
                            if let Some(sub_info) = cached.sub_info(name) {
                                return Some(cross_file_resolved(&sub_info));
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Resolve a variable text to a HashKeyOwner for hash key completion.
    fn resolve_hash_key_owner(&self, var_text: &str, point: Point) -> Option<HashKeyOwner> {
        let bare_name = if var_text.starts_with('$') || var_text.starts_with('@') || var_text.starts_with('%') {
            &var_text[1..]
        } else {
            var_text
        };

        // Try type inference → class owner (bag-routed). Hash-
        // key context: read `hash_key_class()` so Parametric
        // values narrow to their row-class arg (DBIC `$row->{name}`
        // after `find` etc.). For non-Parametric this is
        // equivalent to `class_name()`. CLAUDE.md invariant #10.
        if let Some(it) = self.inferred_type_via_bag(var_text, point) {
            if let Some(cn) = it.hash_key_class() {
                return Some(HashKeyOwner::Class(cn.to_string()));
            }
        }

        // Check call bindings → follow to sub's return hash keys
        for cb in &self.call_bindings {
            if cb.variable == var_text
                && cb.span.start <= point
                && contains_point(&self.scopes[cb.scope.0 as usize].span, point)
            {
                let package = self.sub_defining_package(&cb.func_name);
                return Some(HashKeyOwner::Sub { package, name: cb.func_name.clone() });
            }
        }

        // Check method call bindings → follow to method's return hash keys
        for mcb in &self.method_call_bindings {
            if mcb.variable == var_text
                && mcb.span.start <= point
                && contains_point(&self.scopes[mcb.scope.0 as usize].span, point)
            {
                let package = self.sub_defining_package(&mcb.method_name);
                return Some(HashKeyOwner::Sub { package, name: mcb.method_name.clone() });
            }
        }

        // Try resolving the variable declaration → Variable owner
        // For $hash{}, try %hash first
        let try_names: Vec<String> = if var_text.starts_with('$') {
            vec![format!("%{}", bare_name), var_text.to_string()]
        } else {
            vec![var_text.to_string()]
        };

        for name in &try_names {
            if let Some(sym) = self.resolve_variable(name, point) {
                return Some(HashKeyOwner::Variable {
                    name: name.clone(),
                    def_scope: sym.scope,
                });
            }
        }

        // Check if any existing hash key refs/defs use this bare_name
        for sym in &self.symbols {
            if let SymbolDetail::HashKeyDef { ref owner, .. } = sym.detail {
                match owner {
                    HashKeyOwner::Variable { name, .. } => {
                        let owner_bare = if name.starts_with('$') || name.starts_with('@') || name.starts_with('%') {
                            &name[1..]
                        } else {
                            name
                        };
                        if owner_bare == bare_name {
                            return Some(owner.clone());
                        }
                    }
                    HashKeyOwner::Class(_) | HashKeyOwner::Sub { .. } => {}
                }
            }
        }

        None
    }

    /// Look up the defining package of a sub/method by name. Returns None when
    /// the sub is not found locally (imported, or absent). Used to package-
    /// qualify `HashKeyOwner::Sub` so distinct same-name subs in different
    /// packages don't collide at query time.
    fn sub_defining_package(&self, name: &str) -> Option<String> {
        for sym in &self.symbols {
            if (sym.kind == SymKind::Sub || sym.kind == SymKind::Method) && sym.name == name {
                return sym.package.clone();
            }
        }
        None
    }

    /// Collect :param field names from a core class as keyval completions.
    fn class_param_completions(
        &self,
        class_name: &str,
        used_keys: &HashSet<String>,
    ) -> Vec<CompletionCandidate> {
        let mut candidates = Vec::new();
        for sym in &self.symbols {
            if matches!(sym.kind, SymKind::Field) {
                if let SymbolDetail::Field { ref attributes, .. } = sym.detail {
                    if attributes.contains(&"param".to_string()) {
                        // Check this field belongs to the class
                        if self.symbol_in_class(sym.id, class_name) {
                            let key = sym.bare_name().to_string();
                            if !used_keys.contains(&key) {
                                candidates.push(CompletionCandidate {
                                    label: format!("{} =>", key),
                                    kind: SymKind::Variable,
                                    detail: Some(format!("{}->new(:param)", class_name)),
                                    insert_text: Some(format!("{} => ", key)),
                                    sort_priority: PRIORITY_LOCAL,
                    additional_edits: vec![],
                display_override: None,
                                });
                            }
                        }
                    }
                }
            }
        }
        candidates
    }

    /// Find hash key names accessed via a variable in a specific scope.
    fn hash_keys_in_scope(&self, var_bare_name: &str, scope_id: ScopeId) -> Vec<String> {
        let scope_span = &self.scopes[scope_id.0 as usize].span;
        let mut keys = Vec::new();
        let mut seen = HashSet::new();

        for r in &self.refs {
            if let RefKind::HashKeyAccess { ref var_text, .. } = r.kind {
                // Check the var_text's bare name matches
                let ref_bare = if var_text.starts_with('$')
                    || var_text.starts_with('@')
                    || var_text.starts_with('%')
                {
                    &var_text[1..]
                } else {
                    var_text.as_str()
                };
                if ref_bare == var_bare_name && contains_point(scope_span, r.span.start) {
                    if !seen.contains(&r.target_name) {
                        seen.insert(r.target_name.clone());
                        keys.push(r.target_name.clone());
                    }
                }
            }
        }

        keys
    }
}

/// Generate cross-sigil completion candidates for a variable.
fn generate_cross_sigil_candidates(
    bare_name: &str,
    decl_sigil: char,
    requested_sigil: char,
    detail: Option<String>,
    priority: u8,
    out: &mut Vec<CompletionCandidate>,
) {
    match requested_sigil {
        '$' => {
            if decl_sigil == '$' {
                out.push(CompletionCandidate {
                    label: format!("${}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
            }
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("${}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some(format!("@{}", bare_name))),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
                out.push(CompletionCandidate {
                    label: format!("$#{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail
                        .clone()
                        .or(Some(format!("last index of @{}", bare_name))),
                    insert_text: Some(format!("#{}", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                display_override: None,
                });
            }
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("${}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some(format!("%{}", bare_name))),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
            }
        }
        '@' => {
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("@{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
                out.push(CompletionCandidate {
                    label: format!("@{}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("array slice".to_string()),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                display_override: None,
                });
            }
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("@{}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone().or(Some("hash slice".to_string())),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
            }
        }
        '%' => {
            if decl_sigil == '%' {
                out.push(CompletionCandidate {
                    label: format!("%{}", bare_name),
                    kind: SymKind::Variable,
                    detail: detail.clone(),
                    insert_text: Some(bare_name.to_string()),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
                out.push(CompletionCandidate {
                    label: format!("%{}{{}}", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("hash kv slice".to_string()),
                    insert_text: Some(format!("{}{{", bare_name)),
                    sort_priority: priority.saturating_add(1),
                    additional_edits: vec![],
                display_override: None,
                });
            }
            if decl_sigil == '@' {
                out.push(CompletionCandidate {
                    label: format!("%{}[]", bare_name),
                    kind: SymKind::Variable,
                    detail: Some("array kv slice".to_string()),
                    insert_text: Some(format!("{}[", bare_name)),
                    sort_priority: priority,
                    additional_edits: vec![],
                display_override: None,
                });
            }
        }
        _ => {}
    }
}

// ---- Helpers ----

pub(crate) fn contains_point(span: &Span, point: Point) -> bool {
    (span.start.row < point.row || (span.start.row == point.row && span.start.column <= point.column))
        && (point.row < span.end.row || (point.row == span.end.row && point.column <= span.end.column))
}

/// Walk up from a node to find an ancestor of the given kind.
fn find_ancestor<'a>(node: tree_sitter::Node<'a>, kind: &str) -> Option<tree_sitter::Node<'a>> {
    let mut current = node;
    for _ in 0..20 {
        if current.kind() == kind {
            return Some(current);
        }
        current = current.parent()?;
    }
    None
}

fn span_size(span: &Span) -> usize {
    // Use row difference as primary size metric; column as tiebreaker
    let rows = span.end.row.saturating_sub(span.start.row);
    let cols = if rows == 0 {
        span.end.column.saturating_sub(span.start.column)
    } else {
        0
    };
    rows * 10000 + cols
}

/// Strip the implicit invocant param from a handler signature so hover
/// and sig help don't include the `$self` the user never types.
fn display_handler_params(params: &[ParamInfo]) -> Vec<String> {
    params
        .iter()
        .filter(|p| !p.is_invocant)
        .map(|p| p.name.clone())
        .collect()
}

fn source_line_at(source: &str, row: usize) -> &str {
    source.lines().nth(row).unwrap_or("")
}

/// Return the known return type for a Perl builtin function, if any.
pub(crate) fn builtin_return_type(name: &str) -> Option<InferredType> {
    match name {
        // Numeric returns
        "time" | "length" | "index" | "rindex" | "abs" | "int" | "sqrt"
        | "hex" | "oct" | "ord" | "rand" | "pos" | "tell"
        | "fileno" => Some(InferredType::Numeric),

        // String returns
        "join" | "uc" | "lc" | "ucfirst" | "lcfirst" | "substr" | "sprintf"
        | "ref" | "chr" | "crypt" | "quotemeta" | "pack" | "readline"
        | "readlink" => Some(InferredType::String),

        _ => None,
    }
}

/// Type constraint to push on the first argument of a Perl builtin.
pub(crate) fn builtin_first_arg_type(name: &str) -> Option<InferredType> {
    match name {
        // Numeric arg builtins
        "abs" | "int" | "sqrt" | "chr"
        | "sin" | "cos" | "atan2" | "log" | "exp" => Some(InferredType::Numeric),

        // String arg builtins
        "uc" | "lc" | "ucfirst" | "lcfirst" | "length" | "chomp" | "chop"
        | "substr" | "index" | "rindex" | "quotemeta"
        | "hex" | "oct" | "ord" => Some(InferredType::String),

        _ => None,
    }
}

/// Serialize an InferredType to a simple string tag.
/// Used by signature help's `param_types` field, which piggy-backs on the
/// pre-unification string representation for backwards-compatible JSON output.
pub fn inferred_type_to_tag(ty: &InferredType) -> String {
    match ty {
        InferredType::ClassName(name) => format!("Object:{}", name),
        InferredType::FirstParam { package } => format!("Object:{}", package),
        InferredType::HashRef => "HashRef".to_string(),
        InferredType::ArrayRef => "ArrayRef".to_string(),
        InferredType::CodeRef { .. } => "CodeRef".to_string(),
        InferredType::Regexp => "Regexp".to_string(),
        InferredType::Numeric => "Numeric".to_string(),
        InferredType::String => "String".to_string(),
        // Method dispatch on a Parametric uses its `class_name()`
        // (= the flavor's dispatch class), so the tag follows.
        // Type-arg detail lives in the richer
        // `format_inferred_type` rendering.
        InferredType::Parametric(p) => match p.class_name() {
            Some(c) => format!("Object:{}", c),
            None => "Parametric".to_string(),
        },
        InferredType::Sequence(_) => "Sequence".to_string(),
        // A constraint is a Type::Tiny object; method dispatch (deferred)
        // routes there, so tag it as such rather than as its inner type.
        InferredType::TypeConstraintOf(_) => "Object:Type::Tiny".to_string(),
    }
}

/// Format a cross-file method signature from a SubInfo view.
fn format_cross_file_signature(method_name: &str, sub_info: &crate::module_index::SubInfo<'_>) -> String {
    let params = sub_info.params();
    if params.is_empty() {
        format!("sub {}()", method_name)
    } else {
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        format!("sub {}({})", method_name, names.join(", "))
    }
}

/// Build a `ResolvedSub::CrossFile` from a SubInfo view, snapshotting owned data.
fn cross_file_resolved(sub_info: &crate::module_index::SubInfo<'_>) -> ResolvedSub<'static> {
    let params: Vec<ParamInfo> = sub_info.params().to_vec();
    let param_types: Vec<Option<InferredType>> = params
        .iter()
        .map(|p| sub_info.param_inferred_type(&p.name))
        .collect();
    ResolvedSub::CrossFile {
        params,
        param_types,
        is_method: sub_info.is_method(),
        hash_keys: sub_info.hash_keys().to_vec(),
    }
}

pub(crate) fn format_inferred_type(ty: &InferredType) -> String {
    match ty {
        InferredType::ClassName(name) => name.clone(),
        InferredType::FirstParam { package } => package.clone(),
        InferredType::HashRef => "HashRef".to_string(),
        InferredType::ArrayRef => "ArrayRef".to_string(),
        InferredType::CodeRef { .. } => "CodeRef".to_string(),
        InferredType::Regexp => "Regexp".to_string(),
        InferredType::Numeric => "Numeric".to_string(),
        InferredType::String => "String".to_string(),
        InferredType::Parametric(p) => format_parametric_type(p),
        InferredType::Sequence(elems) => {
            // Angle brackets, not `[...]` — markdown renderers treat
            // bracketed text as link syntax and either swallow it
            // or render it as a broken link. Matches the
            // `Parametric<T1, T2>` style.
            let parts: Vec<String> = elems.iter().map(format_inferred_type).collect();
            format!("Sequence<{}>", parts.join(", "))
        }
        InferredType::TypeConstraintOf(inner) => {
            format!("TypeConstraint<{}>", format_inferred_type(inner))
        }
    }
}

fn format_parametric_type(p: &ParametricType) -> String {
    match p {
        ParametricType::ResultSet { base, row } => {
            format!("{}<{}>", base, row)
        }
    }
}

#[cfg(test)]
#[path = "file_analysis_tests.rs"]
mod tests;

#[cfg(test)]
#[path = "call_ref_index_tests.rs"]
mod call_ref_index_tests;

#[cfg(test)]
#[path = "parametric_resultset_tests.rs"]
mod parametric_resultset_tests;

#[cfg(test)]
#[path = "return_expr_tests.rs"]
mod return_expr_tests;
