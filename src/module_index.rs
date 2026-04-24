//! Module index: public API for cross-file Perl module intelligence.
//!
//! Wraps a concurrent cache (`DashMap`) backed by a background resolver thread.
//! Async LSP handlers only read from the cache (zero I/O). The resolver thread
//! handles @INC discovery, in-process parsing, SQLite persistence, and cpanfile
//! pre-scanning.
//!
//! As of unification phase 2, the cache stores full `FileAnalysis` (not a lossy
//! `ModuleExports` summary). This means cross-file refs, type constraints, call
//! bindings, and framework context all survive the module boundary.
//!
//! See also:
//! - `module_resolver.rs` — resolver thread, in-process parsing
//! - `module_cache.rs` — SQLite persistence (schema v9, bincode+zstd blobs)
//! - `cpanfile.rs` — cpanfile parsing

use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};

use dashmap::DashMap;
use tower_lsp::Client;

use crate::file_analysis::{FileAnalysis, HashKeyOwner, InferredType, ParamInfo, SymKind, Symbol, SymbolDetail};
use crate::module_resolver;

// ---- Public types ----

/// A module in the cache — its filesystem path plus the full FileAnalysis of
/// its source. Shared by reference-count so async handlers don't deep-copy.
#[derive(Debug)]
pub struct CachedModule {
    pub path: PathBuf,
    pub analysis: Arc<FileAnalysis>,
}

impl CachedModule {
    pub fn new(path: PathBuf, analysis: Arc<FileAnalysis>) -> Self {
        CachedModule { path, analysis }
    }

    /// Look up metadata for a sub/method in this module.
    ///
    /// Unlike the old `ExportedSub`, this returns a lightweight view into the
    /// full `FileAnalysis`. Works for any declared sub — exported or not —
    /// because we now store the whole analysis, not just a pre-extracted summary.
    pub fn sub_info(&self, name: &str) -> Option<SubInfo<'_>> {
        // Prefer the first matching Sub/Method symbol. Builder may emit several
        // when rw accessors exist (getter + setter); overloads are collected as
        // additional symbols with the same name.
        let mut syms = self.analysis.symbols.iter().filter(|s| {
            s.name == name && matches!(s.kind, SymKind::Sub | SymKind::Method)
        });
        let primary = syms.next()?;
        let overloads: Vec<&Symbol> = syms.collect();

        // Keys are owned by `Sub { package: primary.package, name }` — the
        // sub's hash keys live under the same package as the sub itself.
        let hash_keys: Vec<String> = self
            .analysis
            .hash_key_defs_for_owner(&HashKeyOwner::Sub {
                package: primary.package.clone(),
                name: name.to_string(),
            })
            .iter()
            .map(|s| s.name.clone())
            .collect();

        Some(SubInfo {
            analysis: &self.analysis,
            primary,
            overloads,
            hash_keys,
        })
    }

    /// True if any sub/method with this name is declared in this module.
    pub fn has_sub(&self, name: &str) -> bool {
        self.analysis.symbols.iter().any(|s| {
            s.name == name && matches!(s.kind, SymKind::Sub | SymKind::Method)
        })
    }
}

/// A view into a module's metadata for a named sub/method.
///
/// Composed of a primary symbol plus any additional symbols with the same
/// name (for rw accessor setter overloads).
pub struct SubInfo<'a> {
    analysis: &'a FileAnalysis,
    primary: &'a Symbol,
    overloads: Vec<&'a Symbol>,
    hash_keys: Vec<String>,
}

impl<'a> SubInfo<'a> {
    pub fn def_line(&self) -> u32 {
        self.primary.span.start.row as u32
    }

    pub fn params(&self) -> &'a [ParamInfo] {
        match &self.primary.detail {
            SymbolDetail::Sub { params, .. } => params,
            _ => &[],
        }
    }

    pub fn is_method(&self) -> bool {
        if self.primary.kind == SymKind::Method {
            return true;
        }
        matches!(
            self.primary.detail,
            SymbolDetail::Sub { is_method: true, .. }
        )
    }

    pub fn return_type(&self) -> Option<&'a InferredType> {
        match &self.primary.detail {
            SymbolDetail::Sub { return_type, .. } => return_type.as_ref(),
            _ => None,
        }
    }

    pub fn doc(&self) -> Option<&'a str> {
        match &self.primary.detail {
            SymbolDetail::Sub { doc, .. } => doc.as_deref(),
            _ => None,
        }
    }

    pub fn hash_keys(&self) -> &[String] {
        &self.hash_keys
    }

    /// Arity list covering the primary and overloads, in declaration order.
    pub fn param_counts(&self) -> Vec<usize> {
        std::iter::once(self.primary)
            .chain(self.overloads.iter().copied())
            .map(|s| match &s.detail {
                SymbolDetail::Sub { params, .. } => params.len(),
                _ => 0,
            })
            .collect()
    }

    /// Return type for an overload with the given arity, if any matches.
    pub fn return_type_for_arity(&self, arity: usize) -> Option<&'a InferredType> {
        for sym in std::iter::once(self.primary).chain(self.overloads.iter().copied()) {
            if let SymbolDetail::Sub { params, return_type, .. } = &sym.detail {
                if params.len() == arity {
                    return return_type.as_ref();
                }
            }
        }
        None
    }

    /// Inferred type for a param by name (if the analysis resolved one).
    pub fn param_inferred_type(&self, param_name: &str) -> Option<&'a InferredType> {
        self.analysis.inferred_type(param_name, self.primary.span.end)
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
            reverse_index,            bridges_index: Arc::new(DashMap::new()),
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

    /// Return the cached CachedModule for a module name. Never does I/O.
    pub fn get_cached(&self, module_name: &str) -> Option<Arc<CachedModule>> {
        self.cache.get(module_name).and_then(|entry| entry.clone())
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

    /// Create a minimal ModuleIndex for CLI mode (no resolver thread, no @INC scan).
    pub fn new_for_cli() -> Self {
        ModuleIndex {
            cache: Arc::new(DashMap::new()),
            reverse_index: Arc::new(DashMap::new()),            bridges_index: Arc::new(DashMap::new()),
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

    #[cfg(test)]
    pub fn new_for_test() -> Self {
        let cache: Arc<DashMap<String, Option<Arc<CachedModule>>>> = Arc::new(DashMap::new());
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
            reverse_index,            bridges_index: Arc::new(DashMap::new()),
            stale_modules,
            available_modules,
            queue,
            resolved,
            workspace_root,
            refresh_diagnostics: Arc::new(|| {}),
        }
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
    /// `FileAnalysis` and iterate. Successor to
    /// `modules_with_class_content` — explicit bridges instead of
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
    /// across files. Replaces the former `modules_with_class_content`
    /// / per-caller symbol.package scan that kept getting forgotten
    /// by new features.
    pub fn for_each_entity_bridged_to(
        &self,
        class_name: &str,
        mut visit: impl FnMut(&Arc<CachedModule>, &crate::file_analysis::Symbol),
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
                    visit(&cached, sym);
                }
            }
        }
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
mod tests {
    use super::*;

    fn parse_source_to_cached(source: &str, module_name: &str) -> Arc<CachedModule> {
        use tree_sitter::Parser;
        let mut parser = Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        let analysis = crate::builder::build(&tree, source.as_bytes());
        Arc::new(CachedModule::new(
            PathBuf::from(format!("/fake/{}.pm", module_name.replace("::", "/"))),
            Arc::new(analysis),
        ))
    }

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
        let cached = idx.get_cached_blocking("List::Util");
        assert!(cached.is_some(), "Should parse List::Util");
        let cached = cached.unwrap();
        assert!(
            cached.analysis.export_ok.contains(&"first".to_string()),
            "List::Util should export_ok 'first', got: {:?}",
            cached.analysis.export_ok
        );
        assert!(
            cached.analysis.export_ok.contains(&"any".to_string()),
            "List::Util should export_ok 'any'"
        );
        assert!(
            cached.analysis.export_ok.contains(&"min".to_string()),
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
        let cached = idx.get_cached("Carp").unwrap();
        assert!(
            cached.analysis.export.contains(&"carp".to_string()),
            "Carp should export 'carp', got: {:?}",
            cached.analysis.export
        );
        assert!(
            cached.analysis.export.contains(&"croak".to_string()),
            "Carp should export 'croak'"
        );
    }

    #[test]
    fn test_find_exporters() {
        let idx = ModuleIndex::new_for_test();

        let foobar_src = "package Foo::Bar;\nour @EXPORT = qw(alpha);\nour @EXPORT_OK = qw(beta);\nsub alpha {}\nsub beta {}\n1;";
        idx.insert_cache("Foo::Bar", Some(parse_source_to_cached(foobar_src, "Foo::Bar")));

        let bazqux_src = "package Baz::Qux;\nour @EXPORT_OK = qw(beta gamma);\nsub beta {}\nsub gamma {}\n1;";
        idx.insert_cache("Baz::Qux", Some(parse_source_to_cached(bazqux_src, "Baz::Qux")));

        assert_eq!(idx.find_exporters("alpha"), vec!["Foo::Bar"]);
        assert_eq!(idx.find_exporters("beta"), vec!["Baz::Qux", "Foo::Bar"]);
        assert!(idx.find_exporters("nonexistent").is_empty());
    }

    #[test]
    fn test_find_exporters_uses_reverse_index() {
        let idx = ModuleIndex::new_for_test();
        let src = "package My::Mod;\nour @EXPORT = qw(foo);\nour @EXPORT_OK = qw(bar);\nsub foo {}\nsub bar {}\n1;";
        idx.insert_cache("My::Mod", Some(parse_source_to_cached(src, "My::Mod")));

        assert!(idx.reverse_index.contains_key("foo"));
        assert!(idx.reverse_index.contains_key("bar"));
        assert_eq!(idx.find_exporters("foo"), vec!["My::Mod"]);
        assert_eq!(idx.find_exporters("bar"), vec!["My::Mod"]);
    }

    #[test]
    fn test_get_return_type_cached() {
        use crate::file_analysis::InferredType;

        let idx = ModuleIndex::new_for_test();

        // Source with two exported subs with clear return types.
        let src = r#"
package Config::DB;
our @EXPORT_OK = qw(get_config make_obj);

sub get_config {
    return { host => 'localhost', port => 5432 };
}

sub make_obj {
    return MyClass->new;
}

1;
"#;
        idx.insert_cache("Config::DB", Some(parse_source_to_cached(src, "Config::DB")));

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
