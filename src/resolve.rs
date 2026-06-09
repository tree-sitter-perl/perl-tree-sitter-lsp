//! Unified query surface across FileStore + ModuleIndex.
//!
//! All cross-file LSP queries (references, rename, workspace/symbol) route
//! through this module so that each handler is a one-liner against a single
//! role-masked function instead of reinventing the per-tier walk.
//!
//! As of phase 4, `refs_to` collects references to a named target across the
//! callers-visible set of files. `resolve_symbol` is left as future work — the
//! current handlers still use cursor-position tree resolution to identify the
//! target, then call into `refs_to` for the cross-file collection.

use std::path::PathBuf;

use tower_lsp::lsp_types::Url;

use crate::file_analysis::{AccessKind, FileAnalysis, HandlerOwner, RefKind, Span, SymKind};
use crate::file_store::{FileKey, FileStore};
use crate::module_index::ModuleIndex;

bitflags::bitflags! {
    /// Which file roles a query should search. Handlers pick the mask that
    /// fits their semantics: rename is EDITABLE (skip deps, they're read-only);
    /// references is VISIBLE (include deps, read-only reads are fine).
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
        module_index: Option<&ModuleIndex>,
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

    /// Map a cursor-resolved `RenameKind` to the cross-file target, sharing
    /// the one mapping across both LSP handlers and both CLI modes so
    /// references and rename can't diverge on target identity (rule #5).
    /// `HashKey`/`Variable` aren't simple cross-file callables — they return
    /// `None` and the caller keeps its owner-expansion / lexical handling.
    pub fn from_rename_kind(
        kind: crate::file_analysis::RenameKind,
        origin: &FileAnalysis,
        module_index: Option<&ModuleIndex>,
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

/// Collect every reference to `target` across the masked file set.
///
/// - `files`   — open + workspace store
/// - `module_index` — dep cache (consulted only if mask includes Dependency)
pub fn refs_to(
    files: &FileStore,
    module_index: Option<&ModuleIndex>,
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
            idx.for_each_cached(|_module_name, cached| {
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
    module_index: Option<&ModuleIndex>,
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
    module_index: Option<&ModuleIndex>,
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
                resolved_package == scope
            }
            (TargetKind::Sub { .. } | TargetKind::Method { .. },
             RefKind::MethodCall { .. }) => {
                // Method-shaped call: match on the build-time-frozen
                // dispatch edge (`resolved_method_target`), NOT a
                // query-time re-derivation of the invocant class. The
                // edge was stamped once (PostFold / enrichment) by the
                // bag-routed resolver, so a call that resolved at build
                // time stays matched regardless of query-time inference
                // flakiness, and genuinely unresolved sites (no edge)
                // are honestly excluded. This is the NAV unification —
                // references read a stored edge, the same discipline
                // `Variable` refs use with `resolves_to`.
                //
                // Inheritance: the edge carries the frozen invocant
                // class; `method_rename_chain` (deterministic over
                // package_parents + module_index) yields
                // `[invocant_class, ..., defining_class]`, so `$child->m`
                // matches a target on any class T on that chain, and
                // unrelated same-named methods on other classes stay out
                // (never on the chain).
                let scope = callable_scope_for_refs.as_ref().unwrap();
                let method = r.unqualified_target_name();
                // SUPER calls resolve LAZILY here, not off the frozen edge: the
                // build-time stamp can't name the dispatch class for a SUPER
                // call into a CROSS-FILE parent (dependency files stamp without
                // the index), and the answer needs the full parent MRO anyway.
                // The ref already carries the SUPER marker (`target_name`) and
                // its enclosing class (`scope`), and `refs_to` runs with the
                // index — so resolve the defining ancestor on demand. It IS the
                // referenced method, so a plain class equality is the match.
                if r.target_name.starts_with("SUPER::") {
                    match (analysis.enclosing_class_for_scope(r.scope), scope) {
                        (Some(encl), Some(pkg)) => analysis
                            .resolve_super_method(&encl, method, module_index)
                            .is_some_and(|res| res.class() == pkg.as_str()),
                        _ => false,
                    }
                } else {
                    match (r.resolved_method_target.as_ref(), scope) {
                        (Some(edge), Some(pkg)) => {
                            let cn = edge.invocant_class();
                            cn == pkg || rename_chain_cache
                                .entry(cn.to_string())
                                .or_insert_with(|| {
                                    analysis.method_rename_chain(cn, method, module_index)
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
            ) => matches!(
                owner,
                Some(HashKeyOwner::Sub { package: op, name: on })
                    if op == package && on == name
            ),
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
