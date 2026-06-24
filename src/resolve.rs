//! Unified query surface across FileStore + ModuleIndex.
//!
//! All cross-file LSP queries (references, rename, workspace/symbol) route
//! through this module so that each handler is a one-liner against a single
//! role-masked function instead of reinventing the per-tier walk.
//!
//! `resolve_symbol` is the inverse direction: cursor → target. Every handler
//! that wants "what does this position refer to, cross-file" (LSP references,
//! LSP rename, the CLI mirrors of both) calls it and then hands the
//! `TargetRef` to `refs_to`. Handlers never re-derive the mapping inline —
//! that's how the CLI and LSP used to disagree on hash-key references.

use std::path::PathBuf;

use tower_lsp::lsp_types::Url;

use crate::file_analysis::{AccessKind, CrossFileLookup, FileAnalysis, HandlerOwner, RefKind, Span, SymKind};
use crate::file_store::{FileKey, FileStore};

bitflags::bitflags! {
    /// Which file roles a query should search. Handlers pick the mask that
    /// fits their semantics: rename is EDITABLE (skip deps, they're read-only);
    /// references is VISIBLE (include deps, read-only reads are fine).
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct RoleMask: u8 {
        const OPEN       = 1 << 0;
        const WORKSPACE  = 1 << 1;
        const DEPENDENCY = 1 << 2;
        const BUILTIN    = 1 << 3;

        const EDITABLE = Self::OPEN.bits() | Self::WORKSPACE.bits();
        const VISIBLE  = Self::OPEN.bits() | Self::WORKSPACE.bits() | Self::DEPENDENCY.bits() | Self::BUILTIN.bits();
    }
}

/// Identifies what we're collecting references to.
#[derive(Debug, Clone)]
pub struct TargetRef {
    pub name: String,
    pub kind: TargetKind,
    /// For a `Method` target, the inheritance rename-chain
    /// `[cursor_class, ..., defining_class]` computed ONCE from the
    /// originating analysis (the only file that knows the cursor class's
    /// parents). A `sub NAME` declaration in ANY class on this set is a
    /// declaration of the same callable — see `symbol_defines_target`.
    /// Empty for non-Method kinds (their decl match is the strict scope).
    pub method_classes: Vec<String>,
}

impl TargetRef {
    /// Build a `Method` target, precomputing the inheritance rename-chain
    /// from `origin` so declaration matching in any scanned file can admit
    /// `sub NAME` in an ancestor class — not just the cursor's static class.
    ///
    /// The chain can only be derived here: a base file (`BaseWorker.pm`)
    /// scanned later doesn't know its child `MyWorker`, so it can't recompute
    /// the chain that links the call's `MyWorker` invocant to the parent decl.
    pub fn method(
        name: String,
        class: String,
        origin: &FileAnalysis,
        module_index: Option<&dyn CrossFileLookup>,
    ) -> Self {
        let method_classes = origin.method_rename_chain(&class, &name, module_index);
        TargetRef {
            name,
            kind: TargetKind::Method { class },
            method_classes,
        }
    }

    /// Build a non-Method target (no inheritance fan-out for declarations).
    pub fn new(name: String, kind: TargetKind) -> Self {
        debug_assert!(
            !matches!(kind, TargetKind::Method { .. }),
            "use TargetRef::method so the rename chain is populated"
        );
        TargetRef { name, kind, method_classes: Vec::new() }
    }

    /// Which targets rename through `refs_to` (cross-file, owner/scope-
    /// structural) rather than the single-file `rename_at` fallback —
    /// references walks *every* kind cross-file; rename is being brought into
    /// line one kind at a time (see `docs/rename-bidirectional-audit.md`).
    /// `HashKeyOfSub` joined once producer↔consumer owner identity was unified
    /// (enrichment stamps the consumer access with the producer's package, and
    /// the consumer-side completion stub is gone — the producer's real def is
    /// the single source). Still single-file: `HashKeyOfClass` (Moo slots need
    /// the projection-group arm — finding #2) and `Handler` (event names need
    /// rename-ready spans — their span includes the surrounding quotes today).
    pub fn supports_cross_file_rename(&self) -> bool {
        matches!(
            self.kind,
            TargetKind::Sub { .. }
                | TargetKind::Method { .. }
                | TargetKind::Package
                | TargetKind::HashKeyOfSub { .. }
        )
    }

    /// Map a cursor-resolved `RenameKind` to the cross-file target, sharing
    /// the one mapping across both LSP handlers and both CLI modes so
    /// references and rename can't diverge on target identity (rule #5).
    /// `HashKey`/`Variable` aren't simple cross-file callables — they return
    /// `None` and the caller keeps its owner-expansion / lexical handling.
    pub fn from_rename_kind(
        kind: crate::file_analysis::RenameKind,
        origin: &FileAnalysis,
        module_index: Option<&dyn CrossFileLookup>,
    ) -> Option<Self> {
        use crate::file_analysis::RenameKind;
        Some(match kind {
            RenameKind::Function { name, package } => {
                TargetRef::new(name, TargetKind::Sub { package })
            }
            RenameKind::Method { name, class } => {
                TargetRef::method(name, class, origin, module_index)
            }
            RenameKind::Package(name) => TargetRef::new(name, TargetKind::Package),
            RenameKind::Handler { owner, name } => {
                TargetRef::new(name.clone(), TargetKind::Handler { owner, name })
            }
            RenameKind::HashKey(_) | RenameKind::Variable => return None,
        })
    }
}

/// What the cursor position resolves to, for cross-file queries.
#[derive(Debug, Clone)]
pub enum ResolvedTarget {
    /// A target `refs_to` can walk: callables, packages, handlers, and
    /// hash keys whose owner resolved at build time.
    Target(TargetRef),
    /// A projection group: one source decl spelled several ways (a
    /// Corinna `field $x :param :reader` ↔ its constructor key ↔ its
    /// reader calls). `targets` walk cross-file via `refs_to`;
    /// `local_spans` are the origin-file-only spellings (the field
    /// variable is lexical to the class block). Every span — local and
    /// walked — covers a bare name token, so rename writes one
    /// replacement text everywhere and references list them uniformly.
    Group {
        local_spans: Vec<Span>,
        /// Spellings pinned to a specific file — the class file's
        /// variable/decl spans when the group was minted remotely (the
        /// cursor sat in a consumer; the source decl lives with the
        /// class).
        pinned_spans: Vec<(PathBuf, Span)>,
        members: Vec<GroupMember>,
    },
    /// Inherently file-local: lexical variables, and hash keys with no
    /// resolvable owner. Callers keep their single-file path.
    Local,
}

/// One walkable member of a projection group, carrying its own rename
/// rule: bare spellings take the plain new name; name-mapped accessors
/// (`has_size`) re-derive theirs; members whose names don't embed the
/// attr join references but skip rename (honest).
#[derive(Debug, Clone)]
pub struct GroupMember {
    pub target: TargetRef,
    pub rename: MemberRename,
}

#[derive(Debug, Clone)]
pub enum MemberRename {
    Bare,
    Affixed { prefix: String, suffix: String },
    Skip,
}

impl MemberRename {
    fn text_for(&self, bare_new: &str) -> Option<String> {
        match self {
            MemberRename::Bare => Some(bare_new.to_string()),
            MemberRename::Affixed { prefix, suffix } => {
                Some(format!("{}{}{}", prefix, bare_new, suffix))
            }
            MemberRename::Skip => None,
        }
    }
}

/// Cursor → cross-file target. The single entry point for "what does this
/// position refer to" — both LSP handlers (references, rename) and their CLI
/// mirrors route here, so target identity can never diverge between them.
/// `None` = nothing resolvable at the cursor at all.
pub fn resolve_symbol(
    analysis: &FileAnalysis,
    point: tree_sitter::Point,
    module_index: Option<&dyn CrossFileLookup>,
) -> Option<ResolvedTarget> {
    use crate::file_analysis::{HashKeyOwner, RenameKind};
    // Field projections claim first: from any spelling of a field group,
    // the answer is the whole group (rename and references stay in
    // lockstep with the in-file `rename_at`/`find_references` union).
    if let Some(p) = analysis.field_projections_at(point) {
        return Some(group_from_projections(p, analysis, None, module_index));
    }
    // Consumer-side: the class lives elsewhere. The owner edge the
    // deferred key (or the accessor call's invocant) already carries
    // reaches the class NAME at query time; one more hop through the
    // index reaches the class's analysis, which holds the group facts
    // the cursor file can't see. Its variable/decl spans pin to the
    // class file.
    if let (Some(idx), Some(r)) = (module_index, analysis.ref_at(point)) {
        use crate::file_analysis::HashKeyOwner;
        match &r.kind {
            RefKind::HashKeyAccess { owner: None, .. } => {
                if let Some(HashKeyOwner::Sub { package: Some(class), name }) =
                    analysis.deferred_hash_key_owner(r, module_index)
                {
                    if crate::conventions::is_constructor_name(&name) {
                        if let Some(cached) = idx.get_cached(&class) {
                            if let Some(p) = cached
                                .analysis
                                .field_projections_named(&r.target_name, &class)
                            {
                                return Some(group_from_projections(
                                    p,
                                    &cached.analysis,
                                    Some(cached.path.clone()),
                                    module_index,
                                ));
                            }
                        }
                    }
                }
            }
            RefKind::MethodCall { .. } => {
                let bare = r.unqualified_target_name().to_string();
                if let Some(class) = analysis.method_call_invocant_class(r, module_index) {
                    if let Some(cached) = idx.get_cached(&class) {
                        if let Some(p) =
                            cached.analysis.field_projections_named(&bare, &class)
                        {
                            // Only an accessor-bearing group may claim a
                            // method-call cursor.
                            if p.has_reader {
                                return Some(group_from_projections(
                                    p,
                                    &cached.analysis,
                                    Some(cached.path.clone()),
                                    module_index,
                                ));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Some(match analysis.rename_kind_at(point, module_index)? {
        RenameKind::Variable => ResolvedTarget::Local,
        RenameKind::HashKey(name) => match analysis.hash_key_owner_at(point) {
            Some(HashKeyOwner::Sub { package, name: sub_name }) => ResolvedTarget::Target(
                TargetRef::new(name, TargetKind::HashKeyOfSub { package, name: sub_name }),
            ),
            Some(HashKeyOwner::Class(class)) => {
                ResolvedTarget::Target(TargetRef::new(name, TargetKind::HashKeyOfClass(class)))
            }
            // Lexical-variable-owned or unresolved — scope-local by definition.
            _ => ResolvedTarget::Local,
        },
        kind => ResolvedTarget::Target(
            TargetRef::from_rename_kind(kind, analysis, module_index)
                .expect("Function/Method/Package/Handler kinds map to a target"),
        ),
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetKind {
    /// A sub defined in a specific package. `None` = the sub has
    /// no package context (top-level script). Matches `Sub`/`Method`
    /// symbols whose `package` field equals this, and `FunctionCall`
    /// refs whose `resolved_package` equals this. Package-scoping
    /// mirrors method's class-scoping — name-only matching
    /// cross-links `Foo::run` and `Bar::run`.
    Sub { package: Option<String> },
    /// A method on a specific class. Matches `Sub`/`Method` symbols
    /// whose `package == class`, and `MethodCall` refs whose invocant
    /// resolves to `class`.
    Method { class: String },
    /// A package/class/module name — matches PackageRef refs.
    Package,
    /// A hash key owned by a specific sub's return value. `package` is the
    /// sub's defining package (or None for top-level / unpackaged subs);
    /// matches `HashKeyOwner::Sub { package, name }` by structural equality.
    HashKeyOfSub { package: Option<String>, name: String },
    /// A hash key owned by a class (Moo `has` slots, DBIC columns on a Result class).
    HashKeyOfClass(String),
    /// An attr's internal hash slot (`$self->{attr}` — or any
    /// `$obj->{attr}` poke; Perl culture is promiscuous about reaching
    /// into the hashref). STRICT `HashKeyOwner::Class` matching, never
    /// `found_by` — broadening would leak other subs' same-named arg
    /// keys into the projection group this member serves.
    InternalHashKey { class: String },
    /// A `Handler` symbol registered on a class (Mojo events, Dancer
    /// routes, etc.). Both the definition (`Handler` symbol) and call
    /// sites (`DispatchCall` refs) match; stacked registrations all
    /// surface separately so features can enumerate every handler.
    Handler {
        owner: HandlerOwner,
        name: String,
    },
}

/// A located reference in some file.
#[derive(Debug, Clone)]
pub struct RefLocation {
    pub key: FileKey,
    pub span: Span,
    /// Read/Write/Declaration — used by document_highlight callers that will
    /// migrate to `refs_to` in a follow-up.
    #[allow(dead_code)]
    pub access: AccessKind,
}

impl RefLocation {
    pub fn to_url(&self) -> Option<Url> {
        match &self.key {
            FileKey::Url(u) => Some(u.clone()),
            FileKey::Path(p) => Url::from_file_path(p).ok(),
        }
    }
}

/// Group construction shared by the local arm (cursor in the class
/// file: spans are origin-local) and the consumer arm (group minted
/// from the class's cached analysis: spans pin to the class file). The
/// rename chain on the Method target is computed against the CLASS
/// analysis — the only one that knows its parents.
fn group_from_projections(
    p: crate::file_analysis::FieldProjections,
    class_analysis: &FileAnalysis,
    pinned_path: Option<PathBuf>,
    module_index: Option<&dyn CrossFileLookup>,
) -> ResolvedTarget {
    let mut members = Vec::new();
    if p.has_reader {
        members.push(GroupMember {
            target: TargetRef::method(
                p.bare.clone(),
                p.class.clone(),
                class_analysis,
                module_index,
            ),
            rename: MemberRename::Bare,
        });
    }
    if p.has_param {
        members.push(GroupMember {
            target: TargetRef::new(
                p.bare.clone(),
                TargetKind::HashKeyOfSub {
                    package: Some(p.class.clone()),
                    name: "new".to_string(),
                },
            ),
            rename: MemberRename::Bare,
        });
    }
    if p.has_internal {
        members.push(GroupMember {
            target: TargetRef::new(
                p.bare.clone(),
                TargetKind::InternalHashKey { class: p.class.clone() },
            ),
            rename: MemberRename::Bare,
        });
    }
    for m in &p.mapped {
        members.push(GroupMember {
            target: TargetRef::method(
                m.method.clone(),
                p.class.clone(),
                class_analysis,
                module_index,
            ),
            rename: match &m.affix {
                Some((pre, suf)) => MemberRename::Affixed {
                    prefix: pre.clone(),
                    suffix: suf.clone(),
                },
                None => MemberRename::Skip,
            },
        });
    }
    match pinned_path {
        None => ResolvedTarget::Group {
            local_spans: p.variable_spans,
            pinned_spans: Vec::new(),
            members,
        },
        Some(path) => ResolvedTarget::Group {
            local_spans: Vec::new(),
            pinned_spans: p
                .variable_spans
                .into_iter()
                .map(|s| (path.clone(), s))
                .collect(),
            members,
        },
    }
}

/// Union of `refs_to` over a projection group's targets plus the group's
/// origin-file spans. `mask_override` = `Some(EDITABLE)` for rename;
/// `None` lets each target pick its references mask. Output is sorted +
/// deduped like `refs_to`, and every span covers a bare name token, so a
/// rename caller can write one replacement text at every location.
pub fn group_refs(
    files: &FileStore,
    module_index: Option<&dyn CrossFileLookup>,
    origin: &FileKey,
    local_spans: &[Span],
    pinned_spans: &[(PathBuf, Span)],
    members: &[GroupMember],
    mask_override: Option<RoleMask>,
) -> Vec<RefLocation> {
    let mut out: Vec<RefLocation> = local_spans
        .iter()
        .map(|span| RefLocation {
            key: origin.clone(),
            span: *span,
            access: AccessKind::Read,
        })
        .collect();
    out.extend(pinned_spans.iter().map(|(path, span)| RefLocation {
        key: FileKey::Path(path.clone()),
        span: *span,
        access: AccessKind::Read,
    }));
    for m in members {
        let mask = mask_override
            .unwrap_or_else(|| references_mask_for(files, module_index, &m.target));
        out.extend(refs_to(files, module_index, &m.target, mask));
    }
    out.sort_by(|a, b| {
        key_for_sort(&a.key)
            .cmp(&key_for_sort(&b.key))
            .then_with(|| {
                (a.span.start.row, a.span.start.column)
                    .cmp(&(b.span.start.row, b.span.start.column))
            })
    });
    out.dedup_by(|a, b| file_key_eq(&a.key, &b.key) && a.span == b.span);
    out
}

/// Rename edit set for a projection group: every span paired with ITS
/// member's replacement text (bare for plain spellings, re-derived for
/// affixed accessors). Bare-member spans win collisions — a synthesized
/// accessor's decl token IS the group decl the bare edit covers.
pub fn group_rename_edits(
    files: &FileStore,
    module_index: Option<&dyn CrossFileLookup>,
    origin: &FileKey,
    local_spans: &[Span],
    pinned_spans: &[(PathBuf, Span)],
    members: &[GroupMember],
    bare_new: &str,
) -> Vec<(RefLocation, String)> {
    let mut out: Vec<(RefLocation, String)> = local_spans
        .iter()
        .map(|span| {
            (
                RefLocation { key: origin.clone(), span: *span, access: AccessKind::Read },
                bare_new.to_string(),
            )
        })
        .collect();
    out.extend(pinned_spans.iter().map(|(path, span)| {
        (
            RefLocation {
                key: FileKey::Path(path.clone()),
                span: *span,
                access: AccessKind::Read,
            },
            bare_new.to_string(),
        )
    }));
    // Bare members before affixed ones, so a same-span collision keeps the
    // bare edit (dedup below keeps the first).
    let mut ordered: Vec<&GroupMember> = members
        .iter()
        .filter(|m| matches!(m.rename, MemberRename::Bare))
        .collect();
    ordered.extend(
        members
            .iter()
            .filter(|m| !matches!(m.rename, MemberRename::Bare)),
    );
    for m in ordered {
        let Some(text) = m.rename.text_for(bare_new) else { continue };
        for loc in refs_to(files, module_index, &m.target, RoleMask::EDITABLE) {
            out.push((loc, text.clone()));
        }
    }
    let mut seen = std::collections::HashSet::new();
    out.retain(|(loc, _)| seen.insert((key_for_sort(&loc.key), loc.span)));
    out
}

/// Collect every reference to `target` across the masked file set.
///
/// - `files`   — open + workspace store
/// - `module_index` — dep cache (consulted only if mask includes Dependency)
pub fn refs_to(
    files: &FileStore,
    module_index: Option<&dyn CrossFileLookup>,
    target: &TargetRef,
    mask: RoleMask,
) -> Vec<RefLocation> {
    let mut out = Vec::new();

    // Open files (canonical — workspace entries for open paths are skipped).
    let mut covered_paths: std::collections::HashSet<PathBuf> = std::collections::HashSet::new();
    if mask.contains(RoleMask::OPEN) {
        files.for_each_open_mut(|url, doc| {
            let url = url.clone();
            if let Ok(p) = url.to_file_path() {
                covered_paths.insert(p);
            }
            collect_from_analysis(&FileKey::Url(url), &doc.analysis, target, module_index, &mut out);
        });
    } else {
        // Even if open isn't in the mask, track the paths so a WORKSPACE walk
        // doesn't duplicate them (an open file's pre-close state isn't meaningful).
        files.for_each_open_mut(|url, _doc| {
            if let Ok(p) = url.to_file_path() {
                covered_paths.insert(p);
            }
        });
    }

    // Workspace files.
    if mask.contains(RoleMask::WORKSPACE) {
        for entry in files.workspace_raw().iter() {
            if covered_paths.contains(entry.key()) {
                continue;
            }
            collect_from_analysis(&FileKey::Path(entry.key().clone()), entry.value(), target, module_index, &mut out);
        }
    }

    // Dependencies (read-only modules from @INC).
    if mask.contains(RoleMask::DEPENDENCY) {
        if let Some(idx) = module_index {
            idx.for_each_cached(&mut |_module_name, cached| {
                let key = FileKey::Path(cached.path.clone());
                collect_from_analysis(&key, &cached.analysis, target, module_index, &mut out);
            });
        }
    }

    // Sort for stable output, dedupe by (path, span).
    out.sort_by(|a, b| {
        key_for_sort(&a.key)
            .cmp(&key_for_sort(&b.key))
            .then_with(|| {
                (a.span.start.row, a.span.start.column)
                    .cmp(&(b.span.start.row, b.span.start.column))
            })
    });
    out.dedup_by(|a, b| file_key_eq(&a.key, &b.key) && a.span == b.span);
    out
}

/// `textDocument/implementation`: local defs of `name` in every
/// transitive descendant package of the Method target's class. On a
/// role's `requires` marker that's "every composer's def of the
/// contract"; on a class method it's "every subclass override".
/// Goto-def stays on the contract/def itself; call sites stay on
/// references — this is the third verb, not a variant of either.
///
/// A descendant role's own re-`requires` marker is a contract
/// re-declaration, not an implementation — `role_requires` is the
/// recorded fact that identifies (and excludes) it.
pub fn implementations_of(
    origin: &FileAnalysis,
    module_index: Option<&dyn CrossFileLookup>,
    target: &TargetRef,
) -> Vec<RefLocation> {
    let TargetKind::Method { class } = &target.kind else {
        return Vec::new();
    };
    let Some(idx) = module_index else {
        return Vec::new();
    };
    // The composer fan-out is a graph walk: INHERITS_INV from the
    // contract's class — the first strangler-fig consumer ported onto
    // the one walker (docs/prompt-graph-walking.md).
    let mut descendants: Vec<String> = Vec::new();
    let probe = crate::graph::GraphView::new(origin, Some(idx));
    probe.walk(
        crate::graph::Node::Class(class.clone()),
        crate::graph::EdgeKindMask::INHERITS_INV,
        &mut |n| {
            if let crate::graph::Node::Class(c) = n {
                descendants.push(c.clone());
            }
            std::ops::ControlFlow::Continue(())
        },
    );
    let mut out: Vec<RefLocation> = Vec::new();
    for pkg in &descendants {
        // class → home module(s): exact cache key for the common
        // single-package file; the names index covers cross-named and
        // multi-package homes.
        let mut homes: Vec<std::sync::Arc<crate::file_analysis::CachedModule>> = Vec::new();
        if let Some(c) = idx.get_cached(pkg) {
            homes.push(c);
        } else {
            for m in idx.modules_with_symbol(pkg) {
                if let Some(c) = idx.get_cached(&m) {
                    let declares = c.analysis.symbols.iter().any(|s| {
                        matches!(s.kind, SymKind::Package | SymKind::Class) && &s.name == pkg
                    });
                    if declares {
                        homes.push(c);
                    }
                }
            }
        }
        for cached in homes {
            let is_marker = cached
                .analysis
                .role_requires
                .get(pkg.as_str())
                .is_some_and(|reqs| reqs.iter().any(|r| r == &target.name));
            if is_marker {
                continue;
            }
            for s in &cached.analysis.symbols {
                if s.name == target.name
                    && matches!(s.kind, SymKind::Sub | SymKind::Method)
                    && s.package.as_deref() == Some(pkg.as_str())
                {
                    out.push(RefLocation {
                        key: FileKey::Path(cached.path.clone()),
                        span: s.selection_span,
                        access: AccessKind::Declaration,
                    });
                }
            }
        }
    }
    out.sort_by(|a, b| {
        key_for_sort(&a.key)
            .cmp(&key_for_sort(&b.key))
            .then_with(|| {
                (a.span.start.row, a.span.start.column)
                    .cmp(&(b.span.start.row, b.span.start.column))
            })
    });
    out.dedup_by(|a, b| file_key_eq(&a.key, &b.key) && a.span == b.span);
    out
}

fn key_for_sort(k: &FileKey) -> PathBuf {
    match k {
        FileKey::Path(p) => p.clone(),
        FileKey::Url(u) => u.to_file_path().unwrap_or_else(|_| PathBuf::from(u.as_str())),
    }
}

fn file_key_eq(a: &FileKey, b: &FileKey) -> bool {
    key_for_sort(a) == key_for_sort(b)
}

/// True when `sym` is a declaration of `target` (decl-span match).
/// Shared by `collect_from_analysis` (to emit decl locations) and
/// `mask_for_target` (to decide whether the def lives in editable space).
fn symbol_defines_target(sym: &crate::file_analysis::Symbol, target: &TargetRef) -> bool {
    use crate::file_analysis::{HashKeyOwner, SymbolDetail};
    if sym.name != target.name {
        return false;
    }
    // Treat a sub and a method in the same package as the same
    // callable — Perl's only distinction between them is call shape.
    // `Sub { package }` matches exactly that scope (None = top-level
    // script sub); `Method { class }` is `Sub { package: Some(class) }`
    // with stricter intent.
    match &target.kind {
        TargetKind::Sub { package } => {
            matches!(sym.kind, SymKind::Sub | SymKind::Method) && sym.package == *package
        }
        TargetKind::Method { class } => {
            // A `sub NAME` declaration belongs to this target if it lives in
            // ANY class on the inheritance rename-chain — the parent that
            // actually defines an inherited method, not only the cursor's
            // static class. The chain is precomputed on the target (it can't
            // be re-derived while scanning the base file, which doesn't know
            // its children). Empty chain falls back to the strict class match
            // so a Method built outside `TargetRef::method` still works.
            let on_chain = target
                .method_classes
                .iter()
                .any(|c| Some(c.as_str()) == sym.package.as_deref())
                || sym.package.as_deref() == Some(class.as_str());
            matches!(sym.kind, SymKind::Sub | SymKind::Method) && on_chain
        }
        TargetKind::Package => matches!(
            sym.kind,
            SymKind::Package | SymKind::Class | SymKind::Module
        ),
        TargetKind::HashKeyOfSub { package, name } => matches!(
            &sym.detail,
            SymbolDetail::HashKeyDef {
                owner: HashKeyOwner::Sub { package: op, name: on },
                ..
            } if op == package && on == name
        ),
        TargetKind::HashKeyOfClass(wanted) => matches!(
            &sym.detail,
            SymbolDetail::HashKeyDef { owner: HashKeyOwner::Class(n), .. } if n == wanted
        ),
        // The slot's def is the group decl (the Method/HashKeyDef pair
        // already collect it) — internal-key members contribute access
        // sites only, no decl matching here.
        TargetKind::InternalHashKey { .. } => false,
        TargetKind::Handler { owner, name: hname } => {
            sym.name == *hname
                && matches!(
                    &sym.detail,
                    SymbolDetail::Handler { owner: o, .. } if o == owner
                )
        }
    }
}

/// Pick the role mask for a *references* query: scope to editable space
/// (OPEN + WORKSPACE) when the target is declared in a file we can edit,
/// else widen to VISIBLE so refs into a dependency-defined symbol still
/// surface. "Find references" on a project symbol must not scan CPAN —
/// see the file-store ADR's RoleMask discipline.
pub fn references_mask_for(
    files: &FileStore,
    module_index: Option<&dyn CrossFileLookup>,
    target: &TargetRef,
) -> RoleMask {
    let mut found_in_editable = false;
    files.for_each_open_mut(|_url, doc| {
        if doc.analysis.symbols.iter().any(|s| symbol_defines_target(s, target)) {
            found_in_editable = true;
        }
    });
    if !found_in_editable {
        for entry in files.workspace_raw().iter() {
            if entry.value().symbols.iter().any(|s| symbol_defines_target(s, target)) {
                found_in_editable = true;
                break;
            }
        }
    }
    // A class-keyed Method target whose decl we can't see in editable
    // space (cross-file synthesized accessor, parent in @INC) still wins
    // EDITABLE if the *class* is a workspace package — the callers we
    // care about are project files. Fall back to the module index only
    // when nothing project-side claims it.
    if !found_in_editable {
        if let (TargetKind::Method { class }, Some(_idx)) = (&target.kind, module_index) {
            let mut declared_in_workspace = false;
            for entry in files.workspace_raw().iter() {
                if entry.value().symbols.iter().any(|s| {
                    matches!(s.kind, SymKind::Package | SymKind::Class)
                        && s.name == *class
                }) {
                    declared_in_workspace = true;
                    break;
                }
            }
            if declared_in_workspace {
                found_in_editable = true;
            }
        }
    }
    if found_in_editable {
        RoleMask::EDITABLE
    } else {
        RoleMask::VISIBLE
    }
}

fn collect_from_analysis(
    key: &FileKey,
    analysis: &FileAnalysis,
    target: &TargetRef,
    module_index: Option<&dyn CrossFileLookup>,
    out: &mut Vec<RefLocation>,
) {
    use crate::file_analysis::HashKeyOwner;

    // `name` is constant across all refs in this call (it is `target.name`), so
    // the only varying key is the invocant class. Cache chains keyed by class to
    // avoid an O(refs × ancestor_depth) DFS on large files with many same-method
    // calls against the same class.
    let mut rename_chain_cache: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    // Include declaration spans when this file defines the target.
    for sym in &analysis.symbols {
        if symbol_defines_target(sym, target) {
            out.push(RefLocation {
                key: key.clone(),
                span: sym.selection_span,
                access: AccessKind::Declaration,
            });
        }
    }

    // Collect usage refs.
    let callable_scope_for_refs: Option<Option<String>> = match &target.kind {
        TargetKind::Sub { package } => Some(package.clone()),
        TargetKind::Method { class } => Some(Some(class.clone())),
        _ => None,
    };
    for r in &analysis.refs {
        // A qualified call (`Foo::baz()` / `$o->Foo::Bar::baz()`) keeps its
        // whole path in `target_name`; match it on the bare callable tail (the
        // dispatch-class checks in the call arms below still pin the right
        // package/class). Every other ref kind matches by exact name.
        let name_matches = if matches!(r.kind, RefKind::FunctionCall { .. } | RefKind::MethodCall { .. }) {
            r.unqualified_target_name() == target.name
        } else {
            r.target_name == target.name
        };
        if !name_matches {
            continue;
        }
        // Sub + Method both match any call into that scope — function
        // or method shape — per the "same callable, two shapes"
        // invariant. Filter is a single scope comparison.
        let matches_kind = match (&target.kind, &r.kind) {
            (TargetKind::Sub { .. } | TargetKind::Method { .. },
             RefKind::FunctionCall { resolved_package }) => {
                let scope = callable_scope_for_refs.as_ref().unwrap();
                // A bare imported call the single-file walk couldn't pin
                // (`use Bank;` auto-imports `@EXPORT`, invisible at build) has
                // `resolved_package: None`. Re-derive it here — where the index
                // is in hand — so a rename from the source sub reaches the
                // consumer call site (the lazy seam method dispatch + deferred
                // hash-key owners use elsewhere in this fn).
                match resolved_package {
                    Some(_) => resolved_package == scope,
                    None => &analysis.deferred_call_package(r, module_index) == scope,
                }
            }
            (TargetKind::Sub { .. } | TargetKind::Method { .. },
             RefKind::MethodCall { .. }) => {
                // Prefer the build-time-frozen dispatch edge
                // (`resolved_method_target`) so a call that resolved at build
                // time stays matched regardless of query-time inference. An
                // absent edge means build-time lacked cross-file info (SUPER
                // into a cross-file parent; enrichment re-stamps OPEN docs
                // only) — re-resolve lazily here, where the index is in hand,
                // rather than silently excluding the site. Either way the
                // class then fans out over `method_rename_chain` so
                // `$child->m` matches an ancestor-defined target while
                // unrelated same-named methods stay out.
                let scope = callable_scope_for_refs.as_ref().unwrap();
                let method = r.unqualified_target_name();
                {
                    let resolved_class = match r.resolved_method_target.as_ref() {
                        Some(edge) => Some(edge.invocant_class().to_string()),
                        None => analysis.method_call_invocant_class(r, module_index),
                    };
                    match (resolved_class, scope) {
                        (Some(cn), Some(pkg)) => {
                            cn == *pkg || rename_chain_cache
                                .entry(cn.clone())
                                .or_insert_with(|| {
                                    analysis.method_rename_chain(&cn, method, module_index)
                                })
                                .iter()
                                .any(|c| c == pkg)
                        }
                        _ => false,
                    }
                }
            }
            (TargetKind::Package, RefKind::PackageRef) => true,
            (
                TargetKind::HashKeyOfSub { package, name },
                RefKind::HashKeyAccess { owner, .. },
            ) => match owner {
                Some(HashKeyOwner::Sub { package: op, name: on }) => {
                    op == package && on == name
                }
                // owner `None` (build gate blind) OR `Variable` (the var is
                // bound to an imported call enrichment didn't reach in this
                // unenriched workspace file) — re-derive cross-file, the same
                // lazy seam method dispatch + deferred owners use above. This
                // is what makes a producer-origin rename reach the consumer's
                // `$c->{key}` access without depending on open-doc enrichment.
                _ => analysis
                    .deferred_hash_key_owner(r, module_index)
                    .is_some_and(|o| {
                        matches!(
                            o,
                            HashKeyOwner::Sub { package: op, name: on }
                                if op == *package && on == *name
                        )
                    }),
            },
            (TargetKind::HashKeyOfClass(wanted), RefKind::HashKeyAccess { owner, .. }) => {
                // `Class(wanted)` is the canonical shape for "this
                // class's keys"; a `Sub { package: wanted, .. }`-
                // owned access (constructor / search-arg /
                // accessor) is *also* a hash-key access against
                // `wanted`. Use `found_by` to admit both — same
                // rule the in-file `hash_key_defs_for_owner`
                // dispatcher uses, so cross-file `refs_to` and
                // local find_definition agree on the set.
                let target_owner = HashKeyOwner::Class(wanted.clone());
                matches!(owner, Some(o) if o.found_by(&target_owner))
            }
            (TargetKind::InternalHashKey { class },
             RefKind::HashKeyAccess { owner, .. }) => {
                // STRICT Class-owner shape (see the kind's doc), widened
                // only by ancestry: a subclass poking `$self->{attr}` owns
                // the access as ITS class — `Gadget isa Widget` ties it to
                // Widget's attr. Never `found_by` (Sub-owned arg keys stay
                // out).
                matches!(
                    owner,
                    Some(HashKeyOwner::Class(c))
                        if c == class || analysis.class_isa(c, class, module_index)
                )
            }
            (TargetKind::Handler { owner, name: hname },
             RefKind::DispatchCall { owner: ref_owner, .. }) => {
                r.target_name == *hname
                    && matches!(ref_owner, Some(o) if o == owner)
            }
            _ => false,
        };
        if matches_kind {
            // MethodCall r.span covers the whole call expression; callers
            // (rename, highlight) want just the method-name token so they
            // can replace or underline exactly the right characters.
            let span = if let RefKind::MethodCall { method_name_span, .. } = &r.kind {
                *method_name_span
            } else {
                r.span
            };
            out.push(RefLocation {
                key: key.clone(),
                span,
                access: r.access,
            });
        }
    }

    // Query-time dispatch resolution: gated candidates (which ride the cache
    // ungated, even in non-open workspace/dependency files) resolve their
    // receiver isa-check NOW against the module index. The `Applies` ones are
    // handler call-sites that enrichment-eager promotion would have missed in
    // any file that's never enriched. `applicable_dispatches` skips sites the
    // emit-hook path already materialized above, so no double-count.
    // See `docs/adr/receiver-gated-dispatch.md`.
    if let TargetKind::Handler { owner, name: hname } = &target.kind {
        for applied in analysis.applicable_dispatches(module_index) {
            if &applied.name == hname && &applied.owner == owner {
                out.push(RefLocation {
                    key: key.clone(),
                    span: applied.span,
                    access: AccessKind::Read,
                });
            }
        }
    }
}

#[cfg(test)]
#[path = "resolve_tests.rs"]
mod tests;
