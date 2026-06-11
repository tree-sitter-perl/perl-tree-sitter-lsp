//! Module index: public API for cross-file Perl module intelligence.
//!
//! Wraps a concurrent cache (`DashMap`) backed by a background resolver thread.
//! Async LSP handlers only read from the cache (zero I/O). The resolver thread
//! handles @INC discovery, in-process parsing, SQLite persistence, and cpanfile
//! pre-scanning.
//!
//! The cache stores the full `FileAnalysis` (not a lossy summary), so
//! cross-file refs, type constraints, call bindings, and framework context
//! all survive the module boundary.
//!
//! See also:
//! - `module_resolver.rs` — resolver thread, in-process parsing
//! - `module_cache.rs` — SQLite persistence (schema v9, bincode+zstd blobs)
//! - `cpanfile.rs` — cpanfile parsing

use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};

use dashmap::DashMap;
use tower_lsp::Client;

use crate::file_analysis::{CrossFileLookup, FileAnalysis, SymKind};
#[cfg(test)]
use crate::file_analysis::InferredType;
use crate::module_resolver;

// ---- Public types ----

// `CachedModule` / `SubInfo` are pure views over `FileAnalysis` and live
// there (the index depends on the model, not vice versa); re-exported so
// index consumers keep one import site.
pub use crate::file_analysis::{CachedModule, SubInfo};

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
/// thread populates the cache by parsing `.pm` files in-process.
#[allow(dead_code)]
pub struct ModuleIndex {
    cache: Arc<DashMap<String, Option<Arc<CachedModule>>>>,
    /// Reverse symbol index: name → list of module names whose
    /// `FileAnalysis` contains at least one symbol with that name
    /// (of any module-visible kind — Sub, Method, Package, Class,
    /// Module, HashKeyDef, Handler). Populated at resolve + cache-
    /// warm time. Queries that want narrower semantics (only exports,
    /// only Handlers on a given class, only methods of a given kind,
    /// etc.) filter the result themselves via per-module inspection.
    ///
    /// This is the single generic "find me modules with symbol X"
    /// primitive — hover, signature help, goto-def, auto-import, and
    /// the unimported-completion path all route through it instead
    /// of reinventing per-feature cache walks. Different features
    /// apply their own override / stacking rules on top.
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    /// Class → modules declaring at least one `PluginNamespace`
    /// whose `bridges` list contains `Bridge::Class(class)`. The
    /// one reverse index for plugin-synthesized content — explicit
    /// (plugin declares its bridges), supports multiple instances
    /// (each app is its own namespace), and avoids the inferred
    /// "any symbol with this package" variant that used to live
    /// alongside this one. Queried through `for_each_entity_bridged_to`.
    bridges_index: Arc<DashMap<String, Vec<String>>>,
    /// Modules loaded from cache with an old extract_version.
    /// Eligible for priority re-resolution when requested.
    stale_modules: Arc<DashMap<String, ()>>,
    /// Perl builtins hover docs, name → rendered markdown. Hydrated
    /// from SQLite by the resolver thread at startup (parsed from
    /// `perlfunc.pod` on first cold-cache miss). Empty until the
    /// resolver has run its warmup path.
    builtins: Arc<DashMap<String, String>>,
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
        let cache: Arc<DashMap<String, Option<Arc<CachedModule>>>> = Arc::new(DashMap::new());
        let reverse_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let stale_modules: Arc<DashMap<String, ()>> = Arc::new(DashMap::new());
        let available_modules: Arc<DashMap<String, std::path::PathBuf>> = Arc::new(DashMap::new());
        let builtins: Arc<DashMap<String, String>> = Arc::new(DashMap::new());
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

        let bridges_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let refresh = Arc::new(on_diagnostics_refresh);
        let refresh_clone = Arc::clone(&refresh);

        module_resolver::spawn_resolver(
            Arc::clone(&cache),
            Arc::clone(&reverse_index),
            Arc::clone(&bridges_index),
            Arc::clone(&stale_modules),
            Arc::clone(&available_modules),
            Arc::clone(&builtins),
            Arc::clone(&queue),
            Arc::clone(&resolved),
            Arc::clone(&workspace_root),
            client,
            Box::new(move || refresh_clone()),
        );

        ModuleIndex {
            cache,
            reverse_index,
            bridges_index,
            stale_modules,
            available_modules,
            builtins,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: refresh,
        }
    }

    /// Hover markdown for a Perl builtin (e.g. `push`, `scalar`).
    /// Returns `None` for unknown names or before the resolver has
    /// hydrated the index from SQLite.
    pub fn builtin_doc(&self, name: &str) -> Option<String> {
        self.builtins.get(name).map(|e| e.clone())
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

    /// Return the cached CachedModule for a module name. Never does I/O.
    pub fn get_cached(&self, module_name: &str) -> Option<Arc<CachedModule>> {
        self.cache.get(module_name).and_then(|entry| entry.clone())
    }

    /// Breadth-first walk over re-export edges (`reexport_modules`), starting
    /// from `start` and visiting each reachable cached module — the start
    /// modules first, then whatever they re-export. `visit` returns
    /// `ControlFlow::Break` to stop early. Bounded by a seen-set (cycles) and a
    /// fan-out cap; never does I/O. The single place the re-export edge
    /// traversal lives — `defining_module_cached` (def location) and
    /// `FileAnalysis::export_surface_with_index` (transitive surface) both ride
    /// it instead of hand-copying the BFS.
    pub fn for_each_reexport_module<F>(&self, start: impl IntoIterator<Item = String>, mut visit: F)
    where
        F: FnMut(&Arc<CachedModule>) -> std::ops::ControlFlow<()>,
    {
        const MAX: usize = 256;
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut queue: std::collections::VecDeque<String> = start.into_iter().collect();
        let mut visited = 0usize;
        while let Some(module) = queue.pop_front() {
            if !seen.insert(module.clone()) {
                continue;
            }
            visited += 1;
            if visited > MAX {
                break;
            }
            let Some(cached) = self.get_cached(&module) else { continue };
            if visit(&cached).is_break() {
                return;
            }
            for next in &cached.analysis.reexport_modules {
                if !seen.contains(next) {
                    queue.push_back(next.clone());
                }
            }
        }
    }

    /// Find the cached module that actually defines sub `name`, starting at
    /// `entry` and following re-export edges when `entry` re-exports another
    /// module's surface. The directly-`use`d module is tried first;
    /// re-exporters delegate the def location to whoever they re-export.
    pub fn defining_module_cached(
        &self,
        entry: &str,
        name: &str,
    ) -> Option<Arc<CachedModule>> {
        use std::ops::ControlFlow;
        let mut found = None;
        self.for_each_reexport_module(std::iter::once(entry.to_string()), |cached| {
            if cached.sub_info(name).is_some() {
                found = Some(Arc::clone(cached));
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        });
        found
    }

    /// Return cached module path only — never does I/O.
    pub fn module_path_cached(&self, module_name: &str) -> Option<PathBuf> {
        self.cache
            .get(module_name)
            .and_then(|entry| entry.as_ref().map(|m| m.path.clone()))
    }

    /// Return cached parent classes for a module's primary package.
    pub fn parents_cached(&self, module_name: &str) -> Vec<String> {
        let cached = match self.get_cached(module_name) {
            Some(c) => c,
            None => return Vec::new(),
        };
        primary_package_parents(&cached.analysis, module_name)
    }

    /// Iterate all cached modules. Callback receives (module_name, CachedModule).
    pub fn for_each_cached<F: FnMut(&str, &Arc<CachedModule>)>(&self, mut f: F) {
        for entry in self.cache.iter() {
            if let Some(ref cached) = *entry.value() {
                f(entry.key(), cached);
            }
        }
    }

    /// Collect module names matching a prefix for completion.
    /// Returns (name, is_resolved) — resolved modules have full analysis.
    pub fn complete_module_names(&self, prefix: &str) -> Vec<(String, bool)> {
        let prefix_lower = prefix.to_lowercase();
        let mut seen = std::collections::HashSet::new();
        let mut results = Vec::new();

        // Tier 1: resolved modules (have full analysis)
        for entry in self.cache.iter() {
            if entry.value().is_some() {
                let name = entry.key();
                if name.to_lowercase().starts_with(&prefix_lower) && seen.insert(name.clone()) {
                    results.push((name.clone(), true));
                }
            }
        }

        // Tier 2: @INC scan (name only, no analysis yet)
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
            if let Some(cached) = self.get_cached(module_name) {
                if let Some(ty) = cached.analysis.sub_return_type_local(func_name) {
                    return Some(ty.clone());
                }
            }
        }
        None
    }

    /// Find all cached modules that *export* the given function name.
    /// Starts from the generic symbol index, then filters to modules
    /// whose `export` / `export_ok` list actually contains the name —
    /// the reverse_index covers every named symbol, not just exports.
    pub fn find_exporters(&self, func_name: &str) -> Vec<String> {
        let mut result: Vec<String> = self.modules_with_symbol(func_name)
            .into_iter()
            .filter(|m| {
                self.get_cached(m)
                    .map(|c| c.analysis.export.iter().any(|e| e == func_name)
                        || c.analysis.export_ok.iter().any(|e| e == func_name))
                    .unwrap_or(false)
            })
            .collect();
        result.sort();
        result.dedup();
        result
    }

    /// Generic "find modules with a symbol named N" primitive —
    /// O(1) hash + O(matches) scan, replaces every `for_each_cached`
    /// pattern where the predicate is name-based. Callers apply their
    /// own kind/detail filter + override/stacking semantics after
    /// picking which specific symbols matter to them.
    pub fn modules_with_symbol(&self, name: &str) -> Vec<String> {
        match self.reverse_index.get(name) {
            Some(modules) => {
                let mut result = modules.clone();
                result.sort();
                result.dedup();
                result
            }
            None => Vec::new(),
        }
    }

    /// Find the module that declares method `name` *attributed to class*
    /// `class` in a file whose own module name differs (cross-package
    /// typeglob install). Returns the registration key for a follow-up
    /// `get_cached`. The reverse index (keyed by symbol name) scopes the
    /// scan; the per-module `has_sub_in_package` filter pins the package.
    /// `None` when no such cross-package symbol exists — callers fall
    /// back to the class's own module / bridges.
    pub fn module_declaring_method_in_package(
        &self,
        name: &str,
        class: &str,
    ) -> Option<String> {
        self.modules_with_symbol(name)
            .into_iter()
            .find(|mod_name| {
                self.get_cached(mod_name)
                    .map(|c| c.has_sub_in_package(name, class))
                    .unwrap_or(false)
            })
    }

    /// Create a minimal ModuleIndex for CLI mode (no resolver thread, no @INC scan).
    pub fn new_for_cli() -> Self {
        // A real (headless) resolver thread: one-shot CLI sessions used
        // to carry NO resolver, so they could never resolve a module the
        // editor hadn't already cached — framework-implied imports
        // (DefaultHelpers) stayed invisible to every CLI probe and the
        // gold harness. The thread blocks until `set_workspace_root`
        // fires in `cli_full_startup`.
        let cache: Arc<DashMap<String, Option<Arc<CachedModule>>>> = Arc::new(DashMap::new());
        let reverse_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let bridges_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let stale_modules: Arc<DashMap<String, ()>> = Arc::new(DashMap::new());
        let available_modules: Arc<DashMap<String, std::path::PathBuf>> = Arc::new(DashMap::new());
        let builtins: Arc<DashMap<String, String>> = Arc::new(DashMap::new());
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
            Arc::clone(&bridges_index),
            Arc::clone(&stale_modules),
            Arc::clone(&available_modules),
            Arc::clone(&queue),
            Arc::clone(&resolved),
            Arc::clone(&workspace_root),
        );

        ModuleIndex {
            cache,
            reverse_index,
            bridges_index,
            stale_modules,
            available_modules,
            builtins,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: Arc::new(|| {}),
        }
    }

    // ---- Test-only methods ----

    #[cfg(test)]
    pub fn new_for_test() -> Self {
        let cache: Arc<DashMap<String, Option<Arc<CachedModule>>>> = Arc::new(DashMap::new());
        let reverse_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let bridges_index: Arc<DashMap<String, Vec<String>>> = Arc::new(DashMap::new());
        let stale_modules: Arc<DashMap<String, ()>> = Arc::new(DashMap::new());
        let available_modules: Arc<DashMap<String, std::path::PathBuf>> = Arc::new(DashMap::new());
        let builtins: Arc<DashMap<String, String>> = Arc::new(DashMap::new());
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
            Arc::clone(&bridges_index),
            Arc::clone(&stale_modules),
            Arc::clone(&available_modules),
            Arc::clone(&queue),
            Arc::clone(&resolved),
            Arc::clone(&workspace_root),
        );

        ModuleIndex {
            cache,
            reverse_index,
            bridges_index,
            stale_modules,
            available_modules,
            builtins,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: Arc::new(|| {}),
        }
    }

    /// Test-only: seed the builtins map directly (bypasses SQLite +
    /// the resolver thread). Used by hover tests so they don't have
    /// to spin up the perlfunc.pod parse pipeline.
    #[cfg(test)]
    pub fn seed_builtin_for_test(&self, name: &str, doc: &str) {
        self.builtins.insert(name.to_string(), doc.to_string());
    }

    /// Direct access to the raw cache DashMap (for CLI warm_cache integration).
    pub fn cache_raw(&self) -> &DashMap<String, Option<Arc<CachedModule>>> {
        &self.cache
    }

    /// Insert a module directly into the cache (for CLI and testing).
    pub fn insert_cache(&self, module_name: &str, cached: Option<Arc<CachedModule>>) {
        // Update reverse index.
        if let Some(ref m) = cached {
            for func in m.analysis.export.iter().chain(m.analysis.export_ok.iter()) {
                self.reverse_index
                    .entry(func.clone())
                    .or_default()
                    .push(module_name.to_string());
            }
        }
        self.cache.insert(module_name.to_string(), cached);
    }

    /// Register a workspace-indexed file under its primary package name
    /// so cross-file resolution (method lookup, Handler walks) can find
    /// it without first requiring an `@INC` resolve. Workspace files
    /// otherwise live only in `FileStore` — this bridges them into the
    /// ModuleIndex so queries that key on package name work uniformly
    /// whether the target came from `use` or from the project tree.
    ///
    /// No-op for files without a `package` declaration (top-level scripts).
    pub fn register_workspace_module(&self, path: std::path::PathBuf, analysis: Arc<FileAnalysis>) {
        let Some(module_name) = first_package_name(&analysis) else { return };
        // Canonicalize the path — `Url::from_file_path` in symbols.rs
        // requires absolute paths, so relative workspace paths (e.g.
        // "./test_files/lib/Users.pm" from CLI `.` root) would silently
        // fail conversion and break cross-file goto-def.
        let path = std::fs::canonicalize(&path).unwrap_or(path);
        let cached = Arc::new(CachedModule::new(path, analysis.clone()));

        // Re-registration happens on file-watcher save. Old edges in
        // `reverse_index` and `bridges_index` still point at this module,
        // so without a purge they accumulate forever — a name or bridge
        // that a previous version declared lingers after it's removed,
        // and cross-file lookups return phantom modules.
        self.purge_module_edges(&module_name);

        let mut name_seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
        for sym in &analysis.symbols {
            // Name-keyed reverse index (every module-visible symbol).
            if matches!(
                sym.kind,
                SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class
                    | SymKind::Module | SymKind::HashKeyDef | SymKind::Handler,
            ) && name_seen.insert(sym.name.as_str()) {
                self.reverse_index
                    .entry(sym.name.clone())
                    .or_default()
                    .push(module_name.clone());
            }
        }
        // Bridges index: any PluginNamespace whose Bridge::Class(c)
        // matches means this module declares a namespace reachable
        // from class `c`. One entry per class per module (dedup).
        let mut bridge_classes_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        for ns in &analysis.plugin_namespaces {
            for crate::file_analysis::Bridge::Class(c) in &ns.bridges {
                if bridge_classes_seen.insert(c.clone()) {
                    self.bridges_index
                        .entry(c.clone())
                        .or_default()
                        .push(module_name.clone());
                }
            }
        }
        self.cache.insert(module_name, Some(cached));
    }

    /// Rebuild the reverse index (`func → modules`) from the current cache.
    /// `warm_cache` writes straight into `cache_raw()` and never touches the
    /// reverse index, so a CLI/full-startup warm path that skips this leaves
    /// `find_exporters` blind to cached modules — the warm run then degrades
    /// "exported by X (not yet imported)" hints to a bare "not defined"
    /// (the B6 cold/warm attribution regression). The resolver thread already
    /// calls the equivalent rebuild after its own warm.
    pub fn rebuild_reverse_index_from_cache(&self) {
        self.reverse_index.clear();
        self.bridges_index.clear();
        for entry in self.cache.iter() {
            if let Some(ref cached) = *entry.value() {
                for name in crate::module_resolver::reverse_index_names(&cached.analysis) {
                    self.reverse_index
                        .entry(name)
                        .or_default()
                        .push(entry.key().clone());
                }
                // Bridges too — same cold/warm-attribution class as the
                // reverse index above: the warm path writes blobs straight
                // into the cache, so a helper registered in a dependency
                // (DefaultHelpers) was reachable on the cold run that
                // resolved it and INVISIBLE on every warm start after.
                let mut seen: std::collections::HashSet<&str> =
                    std::collections::HashSet::new();
                for ns in &cached.analysis.plugin_namespaces {
                    for crate::file_analysis::Bridge::Class(c) in &ns.bridges {
                        if seen.insert(c.as_str()) {
                            self.bridges_index
                                .entry(c.clone())
                                .or_default()
                                .push(entry.key().clone());
                        }
                    }
                }
            }
        }
    }

    /// Remove `module_name` from every `reverse_index` and `bridges_index`
    /// bucket it currently sits in. Called from
    /// `register_workspace_module` before the re-insert so stale edges
    /// from a prior version of the same module don't accumulate.
    fn purge_module_edges(&self, module_name: &str) {
        self.reverse_index.retain(|_name, mods| {
            mods.retain(|m| m != module_name);
            !mods.is_empty()
        });
        self.bridges_index.retain(|_class, mods| {
            mods.retain(|m| m != module_name);
            !mods.is_empty()
        });
    }

    /// Every cached module that declares at least one `PluginNamespace`
    /// whose `bridges` list includes `Bridge::Class(class_name)`.
    /// Callers then pull the namespace's entities from the module's
    /// `FileAnalysis` and iterate. Explicit bridges rather than
    /// symbol-package inference.
    pub fn modules_bridging_to(&self, class_name: &str) -> Vec<String> {
        match self.bridges_index.get(class_name) {
            Some(mods) => {
                let mut result = mods.clone();
                result.sort();
                result.dedup();
                result
            }
            None => Vec::new(),
        }
    }

    /// Run a closure on every `Symbol` reachable from `class_name`
    /// through a plugin bridge. The namespace-wide `Bridge::Class(X)`
    /// identifies which namespaces to walk; per-entity `package`
    /// narrows to "this entity is bound on class X specifically".
    /// Lets one namespace span multiple classes (Mojo helpers on
    /// both Controller and Mojolicious, plus each proxy class in a
    /// dotted chain) without every entity being visible on every
    /// bridged class.
    ///
    /// Single source of truth for plugin-synthesized entity lookup
    /// across files — explicit bridges rather than a per-caller
    /// `symbol.package` scan.
    pub fn for_each_entity_bridged_to(
        &self,
        class_name: &str,
        // `mod_name` is the cache key the bridging module is registered under
        // — the authoritative handle for a follow-up `get_cached(mod_name)`.
        // Don't re-derive it from the analysis: the registration name and the
        // file's first `package` can differ.
        mut visit: impl FnMut(&str, &Arc<CachedModule>, &crate::file_analysis::Symbol),
    ) {
        for mod_name in self.modules_bridging_to(class_name) {
            let Some(cached) = self.get_cached(&mod_name) else { continue };
            for ns in &cached.analysis.plugin_namespaces {
                let bridges_class = ns.bridges.iter().any(|b|
                    matches!(b, crate::file_analysis::Bridge::Class(c) if c == class_name));
                if !bridges_class { continue; }
                // Namespace membership IS the filter — if this namespace
                // bridges to `class_name`, every entity it owns is
                // visible from `class_name`. We used to additionally
                // require `sym.package == class_name`, which forced
                // mojo-helpers to fan-out one Method per bridge class.
                // Now the plugin picks ONE canonical home package and
                // the namespace's bridges control visibility.
                for sym_id in &ns.entities {
                    let idx = sym_id.0 as usize;
                    let Some(sym) = cached.analysis.symbols.get(idx) else { continue };
                    visit(&mod_name, &cached, sym);
                }
            }
        }
    }

    /// Block until `module_name` appears in the cache, or timeout.
    /// (Used by tests and the one-shot CLI import resolution.)
    #[doc(hidden)]
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

    /// Get cached module synchronously. WARNING: Does blocking I/O. Only for tests.
    #[cfg(test)]
    pub fn get_cached_blocking(&self, module_name: &str) -> Option<Arc<CachedModule>> {
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

/// The capability `file_analysis`/`witnesses` query against. Delegates to
/// the inherent methods (inherent wins name resolution on `self`, so no
/// recursion); the generic inherent iterators accept the `&mut dyn FnMut`
/// trampolines directly.
impl CrossFileLookup for ModuleIndex {
    fn get_cached(&self, module_name: &str) -> Option<Arc<CachedModule>> {
        self.get_cached(module_name)
    }

    fn parents_cached(&self, module_name: &str) -> Vec<String> {
        self.parents_cached(module_name)
    }

    fn modules_with_symbol(&self, name: &str) -> Vec<String> {
        self.modules_with_symbol(name)
    }

    fn find_exporters(&self, func_name: &str) -> Vec<String> {
        self.find_exporters(func_name)
    }

    fn defining_module_cached(&self, entry: &str, name: &str) -> Option<Arc<CachedModule>> {
        self.defining_module_cached(entry, name)
    }

    fn module_declaring_method_in_package(&self, name: &str, class: &str) -> Option<String> {
        self.module_declaring_method_in_package(name, class)
    }

    fn for_each_cached(&self, f: &mut dyn FnMut(&str, &Arc<CachedModule>)) {
        self.for_each_cached(f)
    }

    fn for_each_reexport_module(
        &self,
        start: Vec<String>,
        visit: &mut dyn FnMut(&Arc<CachedModule>) -> std::ops::ControlFlow<()>,
    ) {
        self.for_each_reexport_module(start, visit)
    }

    fn for_each_entity_bridged_to(
        &self,
        class_name: &str,
        f: &mut dyn FnMut(&str, &Arc<CachedModule>, &crate::file_analysis::Symbol),
    ) {
        self.for_each_entity_bridged_to(class_name, f)
    }
}

// ---- Module-level helpers ----

/// Return the parents of the primary package of a module, preferring the
/// package with the same name as `module_name` and falling back to the
/// single-package case if only one package exists in the file.
/// First `package X;` declaration in a FileAnalysis. Used to decide
/// under what name a workspace file should be registered in the
/// module index so cross-file method resolution (which keys on
/// package name, e.g. "Users" for `->to('Users#list')`) can find it.
/// Returns `None` for scripts with no explicit package declaration.
pub fn first_package_name(analysis: &FileAnalysis) -> Option<String> {
    for sym in &analysis.symbols {
        if matches!(sym.kind, SymKind::Package | SymKind::Class) {
            return Some(sym.name.clone());
        }
    }
    None
}

pub fn primary_package_parents(analysis: &FileAnalysis, module_name: &str) -> Vec<String> {
    if let Some(parents) = analysis.package_parents.get(module_name) {
        return parents.clone();
    }
    if analysis.package_parents.len() == 1 {
        if let Some((_pkg, parents)) = analysis.package_parents.iter().next() {
            return parents.clone();
        }
    }
    Vec::new()
}


#[cfg(test)]
#[path = "module_index_tests.rs"]
mod tests;
