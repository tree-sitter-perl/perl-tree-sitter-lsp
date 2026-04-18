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

use crate::file_analysis::{AccessKind, FileAnalysis, HandlerOwner, RefKind, Span, SymKind, SymbolDetail};
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
    /// A sub or function name (cross-file call-site matching).
    Sub,
    /// A method name — matches MethodCall refs anywhere.
    Method,
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
        let matches_kind = match &target.kind {
            TargetKind::Sub => sym.kind == SymKind::Sub,
            TargetKind::Method => matches!(sym.kind, SymKind::Sub | SymKind::Method),
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
    for r in &analysis.refs {
        if r.target_name != target.name {
            continue;
        }
        let matches_kind = match (&target.kind, &r.kind) {
            (TargetKind::Sub, RefKind::FunctionCall) => true,
            (TargetKind::Sub, RefKind::MethodCall { .. }) => true,
            (TargetKind::Method, RefKind::FunctionCall) => true,
            (TargetKind::Method, RefKind::MethodCall { .. }) => true,
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
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse(source: &str) -> FileAnalysis {
        use tree_sitter::Parser;
        let mut parser = Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        crate::builder::build(&tree, source.as_bytes())
    }

    #[test]
    fn test_refs_to_finds_sub_across_workspace_files() {
        let store = FileStore::new();
        let path_a = PathBuf::from("/tmp/resolve_test_a.pm");
        let path_b = PathBuf::from("/tmp/resolve_test_b.pm");

        // File A defines sub foo.
        let fa_a = parse("package A;\nsub foo { 42 }\n1;\n");
        store.insert_workspace(path_a.clone(), fa_a);

        // File B calls foo().
        let fa_b = parse("package B;\nsub bar { foo(); }\n1;\n");
        store.insert_workspace(path_b.clone(), fa_b);

        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "foo".to_string(),
                kind: TargetKind::Sub,
            },
            RoleMask::EDITABLE,
        );

        // Expect at least the decl in A and the call in B.
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)
                && r.access == AccessKind::Declaration),
            "expected declaration of foo in file A, got {:?}", results,
        );
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)
                && r.access == AccessKind::Read),
            "expected call to foo in file B, got {:?}", results,
        );
    }

    #[test]
    fn test_refs_to_empty_when_no_hits() {
        let store = FileStore::new();
        let fa = parse("package Only;\nsub bar { 1 }\n1;\n");
        store.insert_workspace(PathBuf::from("/tmp/resolve_no_hits.pm"), fa);

        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "nonexistent".to_string(),
                kind: TargetKind::Sub,
            },
            RoleMask::EDITABLE,
        );
        assert!(results.is_empty());
    }

    /// Phase 5: hash key references are emitted with owner resolved at build
    /// time when the binding is local. refs_to then matches both the def and
    /// the access in the same file via the HashKeyOfSub target.
    ///
    /// Cross-file HashKeyAccess owners are currently resolved through the
    /// enrichment path (which injects synthetic HashKeyDefs in the consumer
    /// file); this test covers the same-file case that lands purely via phase
    /// 5 build-time linking. A follow-up (enrichment rebuild of
    /// refs_by_target) will close the cross-file consumer-access case.
    #[test]
    fn test_refs_to_finds_hash_key_def_and_access_same_file() {
        let store = FileStore::new();
        let path = PathBuf::from("/tmp/resolve_hash_same.pm");

        let fa = parse(
            "package Lib;\nsub get_config { return { host => 1, port => 2 } }\nmy $cfg = get_config();\nmy $h = $cfg->{host};\n1;\n",
        );
        store.insert_workspace(path.clone(), fa);

        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "host".to_string(),
                kind: TargetKind::HashKeyOfSub {
                    package: Some("Lib".to_string()),
                    name: "get_config".to_string(),
                },
            },
            RoleMask::EDITABLE,
        );

        // Expect at least the HashKeyDef decl AND the HashKeyAccess (both in
        // the same file). Previously, the access required a tree argument to
        // resolve its owner — now it's linked at build time.
        let has_decl = results.iter().any(|r| r.access == AccessKind::Declaration);
        let has_access = results.iter().any(|r| r.access == AccessKind::Read);
        assert!(has_decl, "expected HashKeyDef decl, got {:?}", results);
        assert!(has_access, "expected HashKeyAccess, got {:?}", results);
    }

    /// Cross-file: the HashKeyDef is discoverable in a different file even
    /// when the consumer's access site hasn't been enriched yet. This is a
    /// partial cross-file fix — consumer-side access resolution still needs
    /// enrichment to run in the consumer's FileAnalysis.
    #[test]
    fn test_refs_to_finds_cross_file_hash_key_def() {
        let store = FileStore::new();
        let path_lib = PathBuf::from("/tmp/resolve_hash_cross_lib.pm");

        let fa_lib = parse(
            "package Lib;\nsub get_config { return { host => 1, port => 2 } }\n1;\n",
        );
        store.insert_workspace(path_lib.clone(), fa_lib);

        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "host".to_string(),
                kind: TargetKind::HashKeyOfSub {
                    package: Some("Lib".to_string()),
                    name: "get_config".to_string(),
                },
            },
            RoleMask::EDITABLE,
        );
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_lib)),
            "expected HashKeyDef match in Lib, got {:?}", results,
        );
    }

    /// THE bug the user caught: two different packages each with `sub X`
    /// returning `{ key => ... }` must not cross-pollute. Before the
    /// package-qualified `HashKeyOwner::Sub`, every file's `host` key would
    /// show up regardless of which `get_config` defined it. Now only the
    /// matching package's def does.
    #[test]
    fn test_refs_to_package_qualified_sub_owner_isolates_name_collisions() {
        let store = FileStore::new();
        let path_a = PathBuf::from("/tmp/resolve_pkg_a.pm");
        let path_b = PathBuf::from("/tmp/resolve_pkg_b.pm");

        // Two different packages, same sub name, same key name.
        let fa_a = parse(
            "package Alpha;\nsub get_config { return { host => 'alpha' } }\n1;\n",
        );
        store.insert_workspace(path_a.clone(), fa_a);

        let fa_b = parse(
            "package Beta;\nsub get_config { return { host => 'beta' } }\n1;\n",
        );
        store.insert_workspace(path_b.clone(), fa_b);

        // Query for `host` in Alpha's get_config — must NOT match Beta's.
        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "host".to_string(),
                kind: TargetKind::HashKeyOfSub {
                    package: Some("Alpha".to_string()),
                    name: "get_config".to_string(),
                },
            },
            RoleMask::EDITABLE,
        );
        assert!(
            results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_a)),
            "expected Alpha hit, got {:?}", results,
        );
        assert!(
            !results.iter().any(|r| matches!(&r.key, FileKey::Path(p) if p == &path_b)),
            "Beta's get_config must NOT show up in Alpha's references, got {:?}", results,
        );
    }

    #[test]
    fn test_refs_to_role_mask_excludes_workspace() {
        let store = FileStore::new();
        let fa = parse("package P;\nsub foo {}\n1;\n");
        store.insert_workspace(PathBuf::from("/tmp/masked.pm"), fa);

        // OPEN-only mask should miss this workspace file.
        let results = refs_to(
            &store,
            None,
            &TargetRef {
                name: "foo".to_string(),
                kind: TargetKind::Sub,
            },
            RoleMask::OPEN,
        );
        assert!(results.is_empty());
    }
}
