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
            collect_from_analysis(&FileKey::Url(url), &doc.analysis, target, &mut out);
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
            collect_from_analysis(&FileKey::Path(entry.key().clone()), entry.value(), target, &mut out);
        }
    }

    // Dependencies (read-only modules from @INC).
    if mask.contains(RoleMask::DEPENDENCY) {
        if let Some(idx) = module_index {
            idx.for_each_cached(|_module_name, cached| {
                let key = FileKey::Path(cached.path.clone());
                collect_from_analysis(&key, &cached.analysis, target, &mut out);
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

fn collect_from_analysis(
    key: &FileKey,
    analysis: &FileAnalysis,
    target: &TargetRef,
    out: &mut Vec<RefLocation>,
) {
    use crate::file_analysis::{HashKeyOwner, SymbolDetail};

    // Include declaration spans when this file defines the target.
    for sym in &analysis.symbols {
        if sym.name != target.name {
            continue;
        }
        // Treat a sub and a method in the same package as the same
        // callable — Perl's only distinction between them is call
        // shape. `Sub { package }` matches exactly that scope (None
        // = top-level script sub); `Method { class }` is the
        // `Sub { package: Some(class) }` case with stricter intent.
        let callable_scope: Option<Option<String>> = match &target.kind {
            TargetKind::Sub { package } => Some(package.clone()),
            TargetKind::Method { class } => Some(Some(class.clone())),
            _ => None,
        };
        let matches_kind = match &target.kind {
            TargetKind::Sub { .. } | TargetKind::Method { .. } => {
                let scope = callable_scope.as_ref().unwrap();
                matches!(sym.kind, SymKind::Sub | SymKind::Method)
                    && sym.package == *scope
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
                sym.name == *hname && matches!(
                    &sym.detail,
                    SymbolDetail::Handler { owner: o, .. } if o == owner
                )
            }
        };
        if matches_kind {
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
        if r.target_name != target.name {
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
             RefKind::MethodCall { invocant_class, .. }) => {
                // Method-shaped call: match only when the ref's
                // invocant resolved to the target scope. No text
                // fallback — unresolved invocants are excluded rather
                // than cross-linked. (Build-time
                // `resolve_invocant_class_tree` handles chains.)
                let scope = callable_scope_for_refs.as_ref().unwrap();
                match (invocant_class, scope) {
                    (Some(cn), Some(pkg)) => cn == pkg,
                    _ => false, // package-less sub or unpinned method
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
                matches!(owner, Some(HashKeyOwner::Class(n)) if n == wanted)
            }
            (TargetKind::Handler { owner, name: hname },
             RefKind::DispatchCall { owner: ref_owner, .. }) => {
                r.target_name == *hname
                    && matches!(ref_owner, Some(o) if o == owner)
            }
            _ => false,
        };
        if matches_kind {
            out.push(RefLocation {
                key: key.clone(),
                span: r.span,
                access: r.access,
            });
        }
    }
}

#[cfg(test)]
#[path = "resolve_tests.rs"]
mod tests;
