//! Module index: public API for cross-file Perl module intelligence.
//!
//! Wraps a concurrent cache (`DashMap`) backed by a background resolver thread.
//! Async LSP handlers only read from the cache (zero I/O). The resolver thread
//! handles @INC discovery, subprocess parsing, SQLite persistence, and cpanfile
//! pre-scanning.
//!
//! See also:
//! - `module_resolver.rs` — resolver thread, subprocess isolation, export extraction
//! - `module_cache.rs` — SQLite persistence
//! - `cpanfile.rs` — cpanfile parsing

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};

use dashmap::DashMap;
use tower_lsp::Client;

use crate::file_analysis::InferredType;
use crate::module_resolver;

// ---- Public types ----

/// Metadata for an exported function, extracted during module analysis.
#[derive(Debug, Clone)]
pub struct ExportedSub {
    /// Line number of the `sub foo {` definition (0-indexed).
    pub def_line: u32,
    /// Parameter names with metadata.
    pub params: Vec<ExportedParam>,
    /// Whether this is a method (has $self/$class first param).
    pub is_method: bool,
    /// Inferred return type, if known.
    pub return_type: Option<InferredType>,
    /// Hash key names from return value, if returns HashRef.
    pub hash_keys: Vec<String>,
    /// Pre-rendered markdown from POD or comments.
    pub doc: Option<String>,
    /// Additional calling conventions (e.g. setter overload for rw accessors).
    /// Empty for normal subs. Primary fields above hold the getter (0-param) variant.
    pub overloads: Vec<ExportedOverload>,
}

/// An additional calling convention for the same method name.
#[derive(Debug, Clone)]
pub struct ExportedOverload {
    pub params: Vec<ExportedParam>,
    pub return_type: Option<InferredType>,
}

#[derive(Debug, Clone)]
pub struct ExportedParam {
    pub name: String,
    pub is_slurpy: bool,
    /// Inferred type from body usage (e.g. "Numeric", "String"), for cross-file signature help.
    pub inferred_type: Option<String>,
}

/// Exports extracted from a .pm file.
#[derive(Debug, Clone)]
pub struct ModuleExports {
    /// Filesystem path to the .pm file.
    pub path: PathBuf,
    /// @EXPORT — auto-imported on bare `use Foo;`.
    pub export: Vec<String>,
    /// @EXPORT_OK — available on request via `use Foo qw(...)`.
    pub export_ok: Vec<String>,
    /// Per-function metadata for exported subs.
    pub subs: HashMap<String, ExportedSub>,
    /// Parent classes for the primary package.
    pub parents: Vec<String>,
}

impl ModuleExports {
    /// Look up metadata for an exported function.
    pub fn sub_info(&self, name: &str) -> Option<&ExportedSub> {
        self.subs.get(name)
    }

    /// Convenience: return type for a function (used by test code).
    #[cfg(test)]
    pub fn return_type(&self, name: &str) -> Option<&InferredType> {
        self.subs.get(name).and_then(|s| s.return_type.as_ref())
    }

}

// ---- Internal sync primitives (pub(crate) for resolver thread) ----

/// Thread-safe queue: Mutex<Vec> + Condvar.
pub(crate) struct ResolveQueue {
    /// High priority: stale modules from open files. Drained first.
    pub priority: Mutex<Vec<String>>,
    /// Normal priority: missing modules.
    pub pending: Mutex<Vec<String>>,
    pub condvar: Condvar,
}

/// Signaled after each module is resolved.
pub(crate) struct ResolveNotify {
    pub mu: Mutex<()>,
    pub cv: Condvar,
}

/// Channel for workspace root from initialize() → resolver thread.
pub(crate) struct WorkspaceRootChannel {
    pub root: Mutex<Option<Option<String>>>,
    pub condvar: Condvar,
}

/// Concurrent module cache with background resolution.
///
/// Async LSP handlers read from `cache` (zero I/O). The background resolver
/// thread populates the cache by parsing `.pm` files in isolated child processes.
#[allow(dead_code)]
pub struct ModuleIndex {
    cache: Arc<DashMap<String, Option<ModuleExports>>>,
    /// Reverse index: function name → list of module names that export it.
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    /// Modules loaded from cache with an old extract_version.
    /// Eligible for priority re-resolution when requested.
    stale_modules: Arc<DashMap<String, ()>>,
    /// Known module names from @INC scan. Name → path. No exports until resolved.
    available_modules: Arc<DashMap<String, std::path::PathBuf>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
    /// Callback to trigger diagnostic re-publish after module resolution.
    refresh_diagnostics: Arc<dyn Fn() + Send + Sync>,
}

impl ModuleIndex {
    pub fn new(client: Client, on_diagnostics_refresh: impl Fn() + Send + Sync + 'static) -> Self {
        let cache: Arc<DashMap<String, Option<ModuleExports>>> = Arc::new(DashMap::new());
        let reverse_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let stale_modules: Arc<DashMap<String, ()>> = Arc::new(DashMap::new());
        let available_modules: Arc<DashMap<String, std::path::PathBuf>> = Arc::new(DashMap::new());
        let queue = Arc::new(ResolveQueue {
            priority: Mutex::new(Vec::new()),
            pending: Mutex::new(Vec::new()),
            condvar: Condvar::new(),
        });
        let resolved = Arc::new(ResolveNotify {
            mu: Mutex::new(()),
            cv: Condvar::new(),
        });
        let workspace_root = Arc::new(WorkspaceRootChannel {
            root: Mutex::new(None),
            condvar: Condvar::new(),
        });

        let refresh = Arc::new(on_diagnostics_refresh);
        let refresh_clone = Arc::clone(&refresh);

        module_resolver::spawn_resolver(
            Arc::clone(&cache),
            Arc::clone(&reverse_index),
            Arc::clone(&stale_modules),
            Arc::clone(&available_modules),
            Arc::clone(&queue),
            Arc::clone(&resolved),
            Arc::clone(&workspace_root),
            client,
            Box::new(move || refresh_clone()),
        );

        ModuleIndex {
            cache,
            reverse_index,
            stale_modules,
            available_modules,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: refresh,
        }
    }

    /// Notify the resolver thread of the workspace root (from LSP initialize).
    pub fn set_workspace_root(&self, root: Option<&str>) {
        let mut guard = self.workspace_root.root.lock().unwrap();
        if root.is_none() {
            log::warn!("No workspace root from client; using global module cache");
        }
        *guard = Some(root.map(String::from));
        self.workspace_root.condvar.notify_one();
    }

    /// Get the workspace root URI if set.
    pub fn workspace_root(&self) -> Option<String> {
        self.workspace_root.root.lock().ok()
            .and_then(|guard| guard.as_ref().and_then(|opt| opt.clone()))
    }

    /// Request background resolution for a module. Non-blocking.
    /// Stale modules (old extract version) are queued with priority.
    pub fn request_resolve(&self, module_name: &str) {
        let is_stale = self.stale_modules.contains_key(module_name);
        if self.cache.contains_key(module_name) && !is_stale {
            return; // fresh and cached
        }
        if is_stale {
            let mut priority = self.queue.priority.lock().unwrap();
            if !priority.contains(&module_name.to_string()) {
                priority.push(module_name.to_string());
            }
        } else {
            let mut pending = self.queue.pending.lock().unwrap();
            pending.push(module_name.to_string());
        }
        self.queue.condvar.notify_one();
    }

    /// Return cached exports only — never does I/O.
    pub fn get_exports_cached(&self, module_name: &str) -> Option<ModuleExports> {
        self.cache.get(module_name).and_then(|entry| entry.clone())
    }

    /// Return cached module path only — never does I/O.
    pub fn module_path_cached(&self, module_name: &str) -> Option<PathBuf> {
        self.cache
            .get(module_name)
            .and_then(|entry| entry.as_ref().map(|e| e.path.clone()))
    }

    /// Return cached parent classes for a module.
    pub fn parents_cached(&self, module_name: &str) -> Vec<String> {
        self.cache
            .get(module_name)
            .and_then(|entry| entry.as_ref().map(|e| e.parents.clone()))
            .unwrap_or_default()
    }

    /// Iterate all cached module exports. Callback receives (module_name, exports).
    pub fn for_each_cached<F: FnMut(&str, &ModuleExports)>(&self, mut f: F) {
        for entry in self.cache.iter() {
            if let Some(ref exports) = *entry.value() {
                f(entry.key(), exports);
            }
        }
    }

    /// Collect module names matching a prefix for completion.
    /// Returns (name, is_resolved) — resolved modules have export data.
    pub fn complete_module_names(&self, prefix: &str) -> Vec<(String, bool)> {
        let prefix_lower = prefix.to_lowercase();
        let mut seen = std::collections::HashSet::new();
        let mut results = Vec::new();

        // Tier 1: resolved modules (have full export data)
        for entry in self.cache.iter() {
            if entry.value().is_some() {
                let name = entry.key();
                if name.to_lowercase().starts_with(&prefix_lower) && seen.insert(name.clone()) {
                    results.push((name.clone(), true));
                }
            }
        }

        // Tier 2: @INC scan (name only, no exports yet)
        for entry in self.available_modules.iter() {
            let name = entry.key();
            if name.to_lowercase().starts_with(&prefix_lower) && seen.insert(name.clone()) {
                results.push((name.clone(), false));
            }
        }

        results
    }

    /// Look up the return type of an imported function. Zero I/O.
    #[cfg(test)]
    pub fn get_return_type_cached(&self, func_name: &str) -> Option<InferredType> {
        let modules = self.reverse_index.get(func_name)?;
        for module_name in modules.value() {
            if let Some(entry) = self.cache.get(module_name) {
                if let Some(ref exports) = *entry {
                    if let Some(ty) = exports.return_type(func_name) {
                        return Some(ty.clone());
                    }
                }
            }
        }
        None
    }

    /// Find all cached modules that export the given function name.
    /// Uses the reverse index — O(1) lookup instead of scanning all modules.
    pub fn find_exporters(&self, func_name: &str) -> Vec<String> {
        match self.reverse_index.get(func_name) {
            Some(modules) => {
                let mut result = modules.clone();
                result.sort();
                result.dedup();
                result
            }
            None => vec![],
        }
    }

    /// Create a minimal ModuleIndex for CLI mode (no resolver thread, no @INC scan).
    /// Provides empty search results — useful for standalone diagnostics.
    pub fn new_for_cli() -> Self {
        ModuleIndex {
            cache: Arc::new(DashMap::new()),
            reverse_index: Arc::new(DashMap::new()),
            stale_modules: Arc::new(DashMap::new()),
            available_modules: Arc::new(DashMap::new()),
            queue: Arc::new(ResolveQueue {
                priority: Mutex::new(Vec::new()),
                pending: Mutex::new(Vec::new()),
                condvar: Condvar::new(),
            }),
            resolved: Arc::new(ResolveNotify {
                mu: Mutex::new(()),
                cv: Condvar::new(),
            }),
            workspace_root: Arc::new(WorkspaceRootChannel {
                root: Mutex::new(None),
                condvar: Condvar::new(),
            }),
            refresh_diagnostics: Arc::new(|| {}),
        }
    }

    // ---- Test-only methods ----

    /// Create a ModuleIndex for testing — no Client, no progress reporting.
    #[cfg(test)]
    pub fn new_for_test() -> Self {
        let cache: Arc<DashMap<String, Option<ModuleExports>>> = Arc::new(DashMap::new());
        let reverse_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let stale_modules: Arc<DashMap<String, ()>> = Arc::new(DashMap::new());
        let available_modules: Arc<DashMap<String, std::path::PathBuf>> = Arc::new(DashMap::new());
        let queue = Arc::new(ResolveQueue {
            priority: Mutex::new(Vec::new()),
            pending: Mutex::new(Vec::new()),
            condvar: Condvar::new(),
        });
        let resolved = Arc::new(ResolveNotify {
            mu: Mutex::new(()),
            cv: Condvar::new(),
        });
        let workspace_root = Arc::new(WorkspaceRootChannel {
            root: Mutex::new(None),
            condvar: Condvar::new(),
        });

        module_resolver::spawn_test_resolver(
            Arc::clone(&cache),
            Arc::clone(&reverse_index),
            Arc::clone(&stale_modules),
            Arc::clone(&available_modules),
            Arc::clone(&queue),
            Arc::clone(&resolved),
            Arc::clone(&workspace_root),
        );

        ModuleIndex {
            cache,
            reverse_index,
            stale_modules,
            available_modules,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: Arc::new(|| {}),
        }
    }

    /// Direct access to the raw cache DashMap (for CLI warm_cache integration).
    pub fn cache_raw(&self) -> &DashMap<String, Option<ModuleExports>> {
        &self.cache
    }

    /// Insert a module directly into the cache (for CLI and testing).
    pub fn insert_cache(&self, module_name: &str, exports: Option<ModuleExports>) {
        // Update reverse index.
        if let Some(ref e) = exports {
            for func in e.export.iter().chain(e.export_ok.iter()) {
                self.reverse_index
                    .entry(func.clone())
                    .or_default()
                    .push(module_name.to_string());
            }
        }
        self.cache.insert(module_name.to_string(), exports);
    }

    /// Block until `module_name` appears in the cache, or timeout.
    #[cfg(test)]
    pub fn wait_resolved(&self, module_name: &str, timeout: std::time::Duration) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        let mut guard = self.resolved.mu.lock().unwrap();
        loop {
            if self.cache.contains_key(module_name) {
                return true;
            }
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (g, result) = self.resolved.cv.wait_timeout(guard, remaining).unwrap();
            guard = g;
            if result.timed_out() && !self.cache.contains_key(module_name) {
                return false;
            }
        }
    }

    /// Get exports synchronously. WARNING: Does blocking I/O. Only for tests.
    #[cfg(test)]
    pub fn get_exports(&self, module_name: &str) -> Option<ModuleExports> {
        if let Some(entry) = self.cache.get(module_name) {
            return entry.clone();
        }
        let inc_paths = module_resolver::discover_inc_paths();
        let mut parser = module_resolver::create_parser();
        let result = module_resolver::resolve_and_parse(&inc_paths, module_name, &mut parser);
        self.cache.insert(module_name.to_string(), result.clone());
        result
    }

    #[cfg(test)]
    fn inc_paths(&self) -> Vec<PathBuf> {
        module_resolver::discover_inc_paths()
    }

    #[cfg(test)]
    pub fn resolve_module(&self, module_name: &str) -> Option<PathBuf> {
        let inc_paths = module_resolver::discover_inc_paths();
        module_resolver::resolve_module_path(&inc_paths, module_name)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_module_list_util() {
        let idx = ModuleIndex::new_for_test();
        let path = idx.resolve_module("List::Util");
        if !idx.inc_paths().is_empty() {
            assert!(path.is_some(), "List::Util should be resolvable");
            let p = path.unwrap();
            assert!(p.to_str().unwrap().contains("List/Util.pm"));
        }
    }

    #[test]
    fn test_extract_exports_list_util() {
        let idx = ModuleIndex::new_for_test();
        if idx.inc_paths().is_empty() {
            return;
        }
        let exports = idx.get_exports("List::Util");
        assert!(exports.is_some(), "Should parse List::Util exports");
        let exports = exports.unwrap();
        assert!(
            exports.export_ok.contains(&"first".to_string()),
            "List::Util should export_ok 'first', got: {:?}",
            exports.export_ok
        );
        assert!(
            exports.export_ok.contains(&"any".to_string()),
            "List::Util should export_ok 'any'"
        );
        assert!(
            exports.export_ok.contains(&"min".to_string()),
            "List::Util should export_ok 'min'"
        );
    }

    #[test]
    fn test_module_resolution_not_found() {
        let idx = ModuleIndex::new_for_test();
        assert!(idx.resolve_module("Nonexistent::Module::XYZ123").is_none());
    }

    #[test]
    fn test_resolver_thread_flow() {
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        if idx.inc_paths().is_empty() {
            return;
        }
        idx.request_resolve("Carp");
        assert!(
            idx.wait_resolved("Carp", std::time::Duration::from_secs(10)),
            "Carp should be resolved via thread"
        );
        let exports = idx.get_exports_cached("Carp").unwrap();
        assert!(
            exports.export.contains(&"carp".to_string()),
            "Carp should export 'carp', got: {:?}",
            exports.export
        );
        assert!(
            exports.export.contains(&"croak".to_string()),
            "Carp should export 'croak'"
        );
    }

    #[test]
    fn test_find_exporters() {
        let idx = ModuleIndex::new_for_test();
        idx.insert_cache(
            "Foo::Bar",
            Some(ModuleExports {
                path: PathBuf::from("/fake/Foo/Bar.pm"),
                export: vec!["alpha".into()],
                export_ok: vec!["beta".into()],
                subs: HashMap::new(),
                parents: vec![],
            }),
        );
        idx.insert_cache(
            "Baz::Qux",
            Some(ModuleExports {
                path: PathBuf::from("/fake/Baz/Qux.pm"),
                export: vec![],
                export_ok: vec!["beta".into(), "gamma".into()],
                subs: HashMap::new(),
                parents: vec![],
            }),
        );

        assert_eq!(idx.find_exporters("alpha"), vec!["Foo::Bar"]);
        assert_eq!(idx.find_exporters("beta"), vec!["Baz::Qux", "Foo::Bar"]);
        assert!(idx.find_exporters("nonexistent").is_empty());
    }

    #[test]
    fn test_find_exporters_uses_reverse_index() {
        let idx = ModuleIndex::new_for_test();
        idx.insert_cache(
            "My::Mod",
            Some(ModuleExports {
                path: PathBuf::from("/fake/My/Mod.pm"),
                export: vec!["foo".into()],
                export_ok: vec!["bar".into()],
                subs: HashMap::new(),
                parents: vec![],
            }),
        );

        // Verify reverse index was populated.
        assert!(idx.reverse_index.contains_key("foo"));
        assert!(idx.reverse_index.contains_key("bar"));
        assert_eq!(idx.find_exporters("foo"), vec!["My::Mod"]);
        assert_eq!(idx.find_exporters("bar"), vec!["My::Mod"]);
    }

    #[test]
    fn test_get_return_type_cached() {
        use crate::file_analysis::InferredType;

        let idx = ModuleIndex::new_for_test();
        let mut subs = HashMap::new();
        subs.insert("get_config".to_string(), ExportedSub {
            def_line: 10,
            params: vec![],
            is_method: false,
            return_type: Some(InferredType::HashRef),
            hash_keys: vec![],
            doc: None,
            overloads: vec![],
        });
        subs.insert("make_obj".to_string(), ExportedSub {
            def_line: 20,
            params: vec![],
            is_method: false,
            return_type: Some(InferredType::ClassName("MyClass".into())),
            hash_keys: vec![],
            doc: None,
            overloads: vec![],
        });

        idx.insert_cache(
            "Config::DB",
            Some(ModuleExports {
                path: PathBuf::from("/fake/Config/DB.pm"),
                export: vec![],
                export_ok: vec!["get_config".into(), "make_obj".into()],
                subs,
                parents: vec![],
            }),
        );

        assert_eq!(
            idx.get_return_type_cached("get_config"),
            Some(InferredType::HashRef)
        );
        assert_eq!(
            idx.get_return_type_cached("make_obj"),
            Some(InferredType::ClassName("MyClass".into()))
        );
        assert_eq!(idx.get_return_type_cached("nonexistent"), None);
    }
}
