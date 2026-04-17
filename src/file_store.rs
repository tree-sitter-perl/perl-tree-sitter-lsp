//! Unified file store: one home for every `FileAnalysis` the LSP has.
//!
//! Three roles live in one store:
//! - `Open`    — files the user is editing. Carry a full `Document` (tree + text +
//!               stable outline + analysis) because queries routinely need the tree
//!               for cursor-context-sensitive operations.
//! - `Workspace` — every `.pm`/`.pl`/`.t` in the project root. Stored as
//!               `Arc<FileAnalysis>` only; the tree is re-parsed on demand.
//! - `Dependency` — (owned by `ModuleIndex`, viewed here via secondary index)
//!               `@INC` modules. Not duplicated into this store — calls go
//!               through `ModuleIndex` as of phase 3. A future merge moves them
//!               under this same roof.
//!
//! A single path is never represented twice. Opening a workspace file promotes
//! it to `Open` (workspace entry removed); closing demotes it back.
//!
//! Queries that span files (rename, workspace/symbol, cross-file refs) iterate
//! this store uniformly via `iter_all_analyses` — no per-role handler code.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use dashmap::DashMap;
use tower_lsp::lsp_types::Url;

use crate::document::Document;
use crate::file_analysis::FileAnalysis;

/// Role tag — the only behavioral difference between store entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum FileRole {
    /// User is editing; full Document state available.
    Open,
    /// Project file, not currently open.
    Workspace,
    /// @INC module. (Managed by ModuleIndex; this variant is reserved for
    /// the phase 3/4 merge that folds deps into FileStore.)
    Dependency,
    /// Pre-seeded built-in symbols (future).
    BuiltIn,
}

/// Identifier used by callers who want a role-tagged lookup.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum FileKey {
    Path(PathBuf),
    Url(Url),
}

/// The unified store.
pub struct FileStore {
    /// Open files, keyed by URL. Each carries a full Document.
    open: DashMap<Url, Document>,
    /// Workspace files, keyed by canonical path. Stored as Arc for cheap clones.
    workspace: DashMap<PathBuf, Arc<FileAnalysis>>,
    /// URL → canonical path, for open files. Lets us demote cleanly and
    /// prevents duplicate workspace entries for open files.
    url_to_path: DashMap<Url, PathBuf>,
}

impl Default for FileStore {
    fn default() -> Self {
        Self::new()
    }
}

impl FileStore {
    pub fn new() -> Self {
        FileStore {
            open: DashMap::new(),
            workspace: DashMap::new(),
            url_to_path: DashMap::new(),
        }
    }

    // ---- Open-file mutation ----

    /// Open a file from text. Parses and builds analysis. If a workspace entry
    /// exists for the same path, it's replaced by the Open entry.
    pub fn open(&self, url: Url, text: String) -> bool {
        let doc = match Document::new(text) {
            Some(d) => d,
            None => return false,
        };
        if let Ok(path) = url.to_file_path() {
            self.workspace.remove(&path);
            self.url_to_path.insert(url.clone(), path);
        }
        self.open.insert(url, doc);
        true
    }

    /// Close an open file. If a path is known, demote to workspace (keeping
    /// the latest analysis snapshot).
    pub fn close(&self, url: &Url) {
        let doc = match self.open.remove(url) {
            Some((_, doc)) => doc,
            None => return,
        };
        if let Some((_, path)) = self.url_to_path.remove(url) {
            // Snapshot analysis into workspace. Tree + text are dropped —
            // workspace entries don't carry them.
            self.workspace.insert(path, Arc::new(doc.analysis));
        }
    }

    /// Immutable access to an open Document.
    pub fn get_open(&self, url: &Url) -> Option<dashmap::mapref::one::Ref<'_, Url, Document>> {
        self.open.get(url)
    }

    /// Mutable access to an open Document.
    pub fn get_open_mut(&self, url: &Url) -> Option<dashmap::mapref::one::RefMut<'_, Url, Document>> {
        self.open.get_mut(url)
    }

    // ---- Workspace population ----

    /// Insert or replace a workspace entry, unless the same path is currently
    /// open (in which case the open entry is canonical).
    pub fn insert_workspace(&self, path: PathBuf, analysis: FileAnalysis) {
        // If any open URL already points at this path, skip — the open entry wins.
        let shadowed = self.url_to_path.iter().any(|e| e.value() == &path);
        if !shadowed {
            self.workspace.insert(path, Arc::new(analysis));
        }
    }

    /// Remove a workspace entry (e.g. file deletion via watcher).
    pub fn remove_workspace(&self, path: &Path) {
        self.workspace.remove(path);
    }

    /// Direct access to the workspace DashMap (for parallel indexing via Rayon
    /// and CLI tools that pre-populate then iterate). Values are `Arc<FileAnalysis>`.
    pub fn workspace_raw(&self) -> &DashMap<PathBuf, Arc<FileAnalysis>> {
        &self.workspace
    }

    /// Fetch a workspace-role analysis by path.
    pub fn get_workspace(&self, path: &Path) -> Option<Arc<FileAnalysis>> {
        self.workspace.get(path).map(|e| Arc::clone(e.value()))
    }

    /// Count of workspace entries.
    pub fn workspace_len(&self) -> usize {
        self.workspace.len()
    }

    // ---- Iteration ----

    /// Call `f` for every open Document with mutable access to the analysis.
    /// Used by the module-resolver callback to re-enrich open docs.
    pub fn for_each_open_mut<F: FnMut(&Url, &mut Document)>(&self, mut f: F) {
        for mut entry in self.open.iter_mut() {
            let url = entry.key().clone();
            f(&url, entry.value_mut());
        }
    }

    /// Call `f` for every file-path backed analysis in the store — open files
    /// first (canonical), then workspace files (skipping paths already covered
    /// by an open entry). Borrowed, not cloned.
    pub fn for_each_analysis<F: FnMut(FileKey, &FileAnalysis)>(&self, mut f: F) {
        let mut covered_paths = std::collections::HashSet::new();

        for entry in self.open.iter() {
            let url = entry.key().clone();
            if let Ok(path) = url.to_file_path() {
                covered_paths.insert(path);
            }
            f(FileKey::Url(url), &entry.value().analysis);
        }

        for entry in self.workspace.iter() {
            if covered_paths.contains(entry.key()) {
                continue;
            }
            f(FileKey::Path(entry.key().clone()), entry.value());
        }
    }

    /// Count of open files.
    #[cfg(test)]
    pub fn open_count(&self) -> usize {
        self.open.len()
    }

    /// Count of workspace files.
    #[cfg(test)]
    pub fn workspace_count(&self) -> usize {
        self.workspace.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open_then_close_demotes_to_workspace() {
        let store = FileStore::new();
        let url = Url::parse("file:///tmp/demote_test.pm").unwrap();

        assert!(store.open(url.clone(), "package Foo; 1;\n".to_string()));
        assert_eq!(store.open_count(), 1);
        assert_eq!(store.workspace_count(), 0);

        store.close(&url);
        assert_eq!(store.open_count(), 0);
        assert_eq!(store.workspace_count(), 1);
    }

    #[test]
    fn test_open_shadows_workspace_for_same_path() {
        let store = FileStore::new();
        let path = PathBuf::from("/tmp/shadow_test.pm");
        let url = Url::from_file_path(&path).unwrap();

        // Pre-populate as workspace.
        let analysis = {
            use tree_sitter::Parser;
            let src = "package Stale; 1;\n";
            let mut parser = Parser::new();
            parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            let tree = parser.parse(src, None).unwrap();
            crate::builder::build(&tree, src.as_bytes())
        };
        store.insert_workspace(path.clone(), analysis);
        assert_eq!(store.workspace_count(), 1);

        // Opening the same path removes the workspace entry.
        assert!(store.open(url.clone(), "package Fresh; 1;\n".to_string()));
        assert_eq!(store.open_count(), 1);
        assert_eq!(store.workspace_count(), 0);

        // for_each_analysis yields exactly one entry (the open one).
        let mut count = 0;
        store.for_each_analysis(|_, _| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn test_insert_workspace_skipped_when_already_open() {
        let store = FileStore::new();
        let path = PathBuf::from("/tmp/skip_test.pm");
        let url = Url::from_file_path(&path).unwrap();

        store.open(url, "package Open; 1;\n".to_string());

        // Try to insert workspace entry for the same path — should be ignored.
        let analysis = {
            use tree_sitter::Parser;
            let src = "package Workspace; 1;\n";
            let mut parser = Parser::new();
            parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            let tree = parser.parse(src, None).unwrap();
            crate::builder::build(&tree, src.as_bytes())
        };
        store.insert_workspace(path, analysis);

        assert_eq!(store.open_count(), 1);
        assert_eq!(store.workspace_count(), 0, "workspace insert should be skipped");
    }
}
