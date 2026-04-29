//! Module resolver: background thread that resolves Perl modules from `@INC`.
//!
//! Discovers `@INC` paths, locates `.pm` files, parses them in-process with
//! tree-sitter-perl, and extracts export metadata for the module index.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification, request};
use tower_lsp::Client;
use tree_sitter::Parser;

use crate::cpanfile;
use crate::module_cache;
use crate::module_index::{CachedModule, ResolveNotify, ResolveQueue, WorkspaceRootChannel};

/// Callback invoked after each module is resolved. Used to trigger diagnostic refresh.
pub type OnResolved = Box<dyn Fn() + Send + Sync>;

/// Spawn the resolver thread. Returns immediately; the thread runs in the background.
///
/// The `on_resolved` callback fires after each module is inserted into the cache,
/// allowing the backend to re-publish diagnostics.
pub fn spawn_resolver(
    cache: Arc<DashMap<String, Option<Arc<CachedModule>>>>,
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    stale_modules: Arc<DashMap<String, ()>>,
    available_modules: Arc<DashMap<String, PathBuf>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
    client: Client,
    on_resolved: OnResolved,
) {
    let handle = tokio::runtime::Handle::current();

    std::thread::Builder::new()
        .name("module-resolver".into())
        .spawn(move || {
            let mut inc_paths = discover_inc_paths();

            // Wait for workspace root from initialize() for per-project cache path.
            let ws_root = wait_for_workspace_root(&workspace_root);

            // Auto-discover project-local lib paths (lib/, local/lib/perl5/).
            if let Some(ref root_uri) = ws_root {
                if let Some(root_path) = uri_to_path(root_uri) {
                    add_project_lib_paths(&mut inc_paths, &root_path);
                }
            }

            // Scan @INC for available module names (fast, no parsing — just readdir)
            scan_inc_module_names(&inc_paths, &available_modules);
            log::info!("@INC scan: {} modules available", available_modules.len());

            // Warm the in-memory cache from SQLite.
            let db = module_cache::open_cache_db(ws_root.as_deref());
            if let Some(ref conn) = db {
                let _ = module_cache::validate_inc_paths(conn, &inc_paths);
                let _ = module_cache::validate_plugin_fingerprint(
                    conn,
                    &crate::plugin::rhai_host::plugin_fingerprint(),
                );
                let (n, stale_names) = module_cache::warm_cache(conn, &cache);
                log::info!("Warmed module cache: {} entries loaded from disk, {} stale", n, stale_names.len());
                // Queue stale modules for priority re-resolution.
                for name in &stale_names {
                    stale_modules.insert(name.clone(), ());
                }
                if !stale_names.is_empty() {
                    let mut pq = queue.priority.lock().unwrap();
                    pq.extend(stale_names);
                    queue.condvar.notify_one();
                }
                // Build reverse index from warmed cache.
                rebuild_reverse_index(&cache, &reverse_index);
            }

            // Track which extract version each module was resolved at.
            let mut seen: HashMap<String, i64> = HashMap::new();

            // Queue cpanfile dependencies (non-blocking — lets priority items go first).
            // Track total for progress reporting in the main loop.
            let mut cpanfile_total = 0usize;
            let mut cpanfile_done = 0usize;
            if let Some(ref root_uri) = ws_root {
                if let Some(root_path) = uri_to_path(root_uri) {
                    let cpanfile_modules = cpanfile::parse_cpanfile(&root_path);
                    let to_resolve: Vec<String> = cpanfile_modules
                        .into_iter()
                        .filter(|m| !cache.contains_key(m.as_str()))
                        .collect();

                    if !to_resolve.is_empty() {
                        cpanfile_total = to_resolve.len();
                        log::info!("cpanfile: {} modules queued for indexing", cpanfile_total);

                        // Start progress bar.
                        let token = NumberOrString::String("perl-lsp/indexing".to_string());
                        let _ = handle.block_on(client.send_request::<request::WorkDoneProgressCreate>(
                            WorkDoneProgressCreateParams { token: token.clone() },
                        ));
                        handle.block_on(client.send_notification::<notification::Progress>(
                            ProgressParams {
                                token,
                                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                                    WorkDoneProgressBegin {
                                        title: "Indexing Perl modules".into(),
                                        cancellable: Some(false),
                                        message: None,
                                        percentage: Some(0),
                                    },
                                )),
                            },
                        ));

                        let mut pending = queue.pending.lock().unwrap();
                        pending.extend(to_resolve);
                        queue.condvar.notify_one();
                    }
                }
            }

            // Main resolve loop — drain priority first, then pending.
            loop {
                let batch = drain_next_batch(&queue);

                for module_name in batch {
                    // Allow re-resolution when extract version is outdated.
                    if let Some(&ver) = seen.get(&module_name) {
                        if ver >= module_cache::EXTRACT_VERSION {
                            continue;
                        }
                    }
                    seen.insert(module_name.clone(), module_cache::EXTRACT_VERSION);

                    let is_re_resolve = stale_modules.contains_key(&module_name);
                    if is_re_resolve {
                        log::info!("Re-resolving stale module '{}'", module_name);
                    } else {
                        log::info!("Resolving module '{}'", module_name);
                    }

                    let result = parse_module(&inc_paths, &module_name);
                    match &result {
                        Some(m) => log::info!(
                            "Resolved '{}': {} export, {} export_ok",
                            module_name,
                            m.analysis.export.len(),
                            m.analysis.export_ok.len()
                        ),
                        None => log::info!("No exports found for '{}'", module_name),
                    }
                    insert_into_cache(
                        &cache,
                        &reverse_index,
                        &module_name,
                        result.clone(),
                    );

                    if let Some(ref conn) = db {
                        module_cache::save_to_db(conn, &module_name, &result, "import");
                    }

                    // Descend into the module's own dependencies so the
                    // chain keeps resolving beyond the open doc's direct
                    // imports. Without this the cache stops at depth 1 —
                    // e.g. opening a Mojolicious::Lite script resolves
                    // Mojolicious.pm, but Mojolicious.pm's
                    // `has routes => sub { Mojolicious::Routes->new }`
                    // never triggers a resolve on Mojolicious::Routes,
                    // and `$r->get` on line 71 of the demo chain-dies
                    // because the intermediate class is a cache miss.
                    //
                    // The `seen` guard above makes this cycle-safe: a
                    // transitively-enqueued name that was already
                    // resolved at the current EXTRACT_VERSION gets
                    // skipped on its next turn.
                    if let Some(ref m) = result {
                        let mut pending = queue.pending.lock().unwrap();
                        let enqueue = |pending: &mut Vec<String>, name: String| {
                            if name.is_empty() { return; }
                            if cache.contains_key(&name) { return; }
                            if seen.contains_key(&name) { return; }
                            if !pending.iter().any(|p| p == &name) {
                                pending.push(name);
                            }
                        };
                        // Explicit imports — the module's own `use` statements.
                        for imp in &m.analysis.imports {
                            enqueue(&mut pending, imp.module_name.clone());
                        }
                        // Parent classes — inheritance chain.
                        for parents in m.analysis.package_parents.values() {
                            for parent in parents {
                                enqueue(&mut pending, parent.clone());
                            }
                        }
                        // ClassName return types — `has foo => sub { Bar->new }`,
                        // plugin-emitted typed Subs, method return annotations.
                        // These are the chain-invisible-but-reachable classes
                        // the user's chain walks through at query time.
                        for sym in &m.analysis.symbols {
                            use crate::file_analysis::{SymKind, SymbolDetail, InferredType};
                            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
                            if let SymbolDetail::Sub { return_type: Some(InferredType::ClassName(c)), .. } = &sym.detail {
                                enqueue(&mut pending, c.clone());
                            }
                        }
                        if !pending.is_empty() {
                            queue.condvar.notify_one();
                        }
                    }

                    // Remove from stale set after successful re-resolution.
                    if is_re_resolve {
                        stale_modules.remove(&module_name);
                    }

                    // Report cpanfile progress.
                    if cpanfile_total > 0 && cpanfile_done < cpanfile_total {
                        cpanfile_done += 1;
                        let pct = (cpanfile_done * 100 / cpanfile_total) as u32;
                        let token = NumberOrString::String("perl-lsp/indexing".to_string());
                        if cpanfile_done < cpanfile_total {
                            handle.block_on(client.send_notification::<notification::Progress>(
                                ProgressParams {
                                    token,
                                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                                        WorkDoneProgressReport {
                                            cancellable: Some(false),
                                            message: Some(format!("{} ({}/{})", module_name, cpanfile_done, cpanfile_total)),
                                            percentage: Some(pct),
                                        },
                                    )),
                                },
                            ));
                        } else {
                            handle.block_on(client.send_notification::<notification::Progress>(
                                ProgressParams {
                                    token,
                                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(
                                        WorkDoneProgressEnd {
                                            message: Some(format!("Indexed {} modules", cpanfile_total)),
                                        },
                                    )),
                                },
                            ));
                        }
                    }

                    // Signal waiters and trigger diagnostic refresh.
                    {
                        let _g = resolved.mu.lock().unwrap();
                        resolved.cv.notify_all();
                    }
                    on_resolved();
                }
            }
        })
        .expect("failed to spawn module-resolver thread");
}

/// Drain the next batch from the queue, checking priority first.
fn drain_next_batch(queue: &ResolveQueue) -> Vec<String> {
    // Check priority first
    {
        let mut priority = queue.priority.lock().unwrap();
        if !priority.is_empty() {
            return std::mem::take(&mut *priority);
        }
    }
    // Wait for pending
    let mut pending = queue.pending.lock().unwrap();
    loop {
        if !pending.is_empty() {
            // Before draining pending, re-check priority
            let mut priority = queue.priority.lock().unwrap();
            if !priority.is_empty() {
                return std::mem::take(&mut *priority);
            }
            return std::mem::take(&mut *pending);
        }
        pending = queue.condvar.wait(pending).unwrap();
    }
}

/// Simplified resolver for tests — no Client, no progress, no cpanfile.
#[cfg(test)]
pub fn spawn_test_resolver(
    cache: Arc<DashMap<String, Option<Arc<CachedModule>>>>,
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    stale_modules: Arc<DashMap<String, ()>>,
    available_modules: Arc<DashMap<String, PathBuf>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
) {
    std::thread::Builder::new()
        .name("module-resolver-test".into())
        .spawn(move || {
            let mut inc_paths = discover_inc_paths();
            let ws_root = wait_for_workspace_root(&workspace_root);

            if let Some(ref root_uri) = ws_root {
                if let Some(root_path) = uri_to_path(root_uri) {
                    add_project_lib_paths(&mut inc_paths, &root_path);
                }
            }

            scan_inc_module_names(&inc_paths, &available_modules);

            let db = module_cache::open_cache_db(ws_root.as_deref());
            if let Some(ref conn) = db {
                let _ = module_cache::validate_inc_paths(conn, &inc_paths);
                let _ = module_cache::validate_plugin_fingerprint(
                    conn,
                    &crate::plugin::rhai_host::plugin_fingerprint(),
                );
                let (_, stale_names) = module_cache::warm_cache(conn, &cache);
                for name in stale_names {
                    stale_modules.insert(name, ());
                }
                rebuild_reverse_index(&cache, &reverse_index);
            }

            let mut seen: HashMap<String, i64> = HashMap::new();
            loop {
                let batch = drain_next_batch(&queue);
                for module_name in batch {
                    if let Some(&ver) = seen.get(&module_name) {
                        if ver >= module_cache::EXTRACT_VERSION {
                            continue;
                        }
                    }
                    seen.insert(module_name.clone(), module_cache::EXTRACT_VERSION);

                    let result = parse_module(&inc_paths, &module_name);
                    insert_into_cache(&cache, &reverse_index, &module_name, result.clone());
                    if let Some(ref conn) = db {
                        module_cache::save_to_db(conn, &module_name, &result, "import");
                    }
                    stale_modules.remove(&module_name);
                    let _g = resolved.mu.lock().unwrap();
                    resolved.cv.notify_all();
                }
            }
        })
        .expect("failed to spawn test module-resolver thread");
}

// ---- Internal helpers ----

fn wait_for_workspace_root(ws_root_channel: &WorkspaceRootChannel) -> Option<String> {
    let mut guard = ws_root_channel.root.lock().unwrap();
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while guard.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            log::warn!("Timed out waiting for workspace root; using global cache");
            break;
        }
        let (g, _) = ws_root_channel
            .condvar
            .wait_timeout(guard, remaining)
            .unwrap();
        guard = g;
    }
    guard.clone().flatten()
}

/// Names of every module-visible symbol in the analysis. Variables and
/// fields are skipped — they're file-local and can't be queried across
/// files. Deduplicated via a set first so a module with 10 subs named
/// `foo` (unlikely but possible) contributes one entry, not ten.
fn indexable_symbol_names(analysis: &crate::file_analysis::FileAnalysis) -> Vec<String> {
    use crate::file_analysis::SymKind;
    let mut names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for sym in &analysis.symbols {
        if matches!(
            sym.kind,
            SymKind::Sub | SymKind::Method | SymKind::Package | SymKind::Class
                | SymKind::Module | SymKind::HashKeyDef | SymKind::Handler,
        ) {
            names.insert(sym.name.clone());
        }
    }
    names.into_iter().collect()
}

/// Insert a resolved module into the cache and update the reverse index.
fn insert_into_cache(
    cache: &DashMap<String, Option<Arc<CachedModule>>>,
    reverse_index: &DashMap<String, Vec<String>>,
    module_name: &str,
    result: Option<Arc<CachedModule>>,
) {
    if let Some(ref cached) = result {
        for name in indexable_symbol_names(&cached.analysis) {
            reverse_index
                .entry(name)
                .or_default()
                .push(module_name.to_string());
        }
        // `class_content_index` isn't threaded through the resolver
        // APIs yet — resolver-path registration populates it via the
        // `post_insert_hook` call below if provided by the caller.
        // (Initially only workspace-index registration uses it; the
        // resolver thread path is not yet on this hot path for
        // plugin-synthesized content.)
    }
    cache.insert(module_name.to_string(), result);
}

/// Rebuild reverse index from existing cache (e.g. after warming from SQLite).
fn rebuild_reverse_index(
    cache: &DashMap<String, Option<Arc<CachedModule>>>,
    reverse_index: &DashMap<String, Vec<String>>,
) {
    for entry in cache.iter() {
        if let Some(ref cached) = *entry.value() {
            for name in indexable_symbol_names(&cached.analysis) {
                reverse_index
                    .entry(name)
                    .or_default()
                    .push(entry.key().clone());
            }
        }
    }
}

// ---- Module parsing ----

/// Parse a module file directly in-process.
/// tree-sitter-perl is stable — no subprocess isolation needed.
fn parse_module(inc_paths: &[PathBuf], module_name: &str) -> Option<Arc<CachedModule>> {
    let mut parser = create_parser();
    resolve_and_parse(inc_paths, module_name, &mut parser)
}

pub fn create_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .expect("failed to set Perl language");
    parser
}

// ---- Resolution ----

pub fn resolve_module_path(inc_paths: &[PathBuf], module_name: &str) -> Option<PathBuf> {
    let rel_path = module_name.replace("::", "/") + ".pm";
    for inc in inc_paths {
        let full = inc.join(&rel_path);
        if full.is_file() {
            return Some(full);
        }
    }
    None
}

#[allow(dead_code)]
pub fn resolve_and_parse(
    inc_paths: &[PathBuf],
    module_name: &str,
    parser: &mut Parser,
) -> Option<Arc<CachedModule>> {
    let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
    resolve_and_parse_inner(inc_paths, module_name, parser, &mut visiting)
}

fn resolve_and_parse_inner(
    inc_paths: &[PathBuf],
    module_name: &str,
    parser: &mut Parser,
    visiting: &mut std::collections::HashSet<String>,
) -> Option<Arc<CachedModule>> {
    if !visiting.insert(module_name.to_string()) {
        // Cycle in `@ISA` parent fallback — bail rather than blow the stack.
        return None;
    }

    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    if metadata.len() > 1_000_000 {
        return None;
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let tree = parser.parse(&source, None)?;

    let mut analysis = crate::builder::build(&tree, source.as_bytes());

    // If this module has no exports but inherits via @ISA (e.g. DDP → Data::Printer),
    // fall back to the first parent's exports. This only patches `export`/`export_ok`;
    // the parent's own cached analysis is still the source of truth for its symbols.
    if analysis.export.is_empty() && analysis.export_ok.is_empty() {
        let parents = crate::module_index::primary_package_parents(&analysis, module_name);
        for parent in &parents {
            if let Some(parent_cached) = resolve_and_parse_inner(inc_paths, parent, parser, visiting) {
                if !parent_cached.analysis.export.is_empty()
                    || !parent_cached.analysis.export_ok.is_empty()
                {
                    analysis.export = parent_cached.analysis.export.clone();
                    analysis.export_ok = parent_cached.analysis.export_ok.clone();
                    break;
                }
            }
        }
    }

    Some(Arc::new(CachedModule::new(path, Arc::new(analysis))))
}

// ---- @INC discovery ----

pub fn discover_inc_paths() -> Vec<PathBuf> {
    let output = std::process::Command::new("perl")
        .args(["-e", r#"print join "\n", @INC"#])
        .stdin(std::process::Stdio::null())
        .output();

    match output {
        Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout)
            .lines()
            .filter(|l| !l.is_empty())
            .map(PathBuf::from)
            .filter(|p| p.is_dir())
            .collect(),
        _ => vec![],
    }
}

/// Add project-local lib paths (lib/, local/lib/perl5/) to the front of @INC.
/// Called by the resolver thread, test resolver, and CLI tools.
pub fn add_project_lib_paths(inc_paths: &mut Vec<PathBuf>, workspace_root: &std::path::Path) {
    for local_lib in &["lib", "local/lib/perl5"] {
        let p = workspace_root.join(local_lib);
        if p.is_dir() {
            log::info!("Auto-discovered project lib: {:?}", p);
            inc_paths.insert(0, p);
        }
    }
}


/// Index all Perl files in a workspace directory using Rayon for parallelism.
/// Populates the workspace role of the shared `FileStore`. Returns the number
/// of successfully indexed files.
pub fn index_workspace(
    root: &std::path::Path,
    files: &crate::file_store::FileStore,
) -> usize {
    index_workspace_with_index(root, files, None)
}

/// Variant that also registers each indexed file in the given
/// `ModuleIndex` under its primary package name. Used so workspace
/// modules participate in cross-file lookups (method resolution,
/// Handler walks, etc.) without waiting for an on-demand `use`
/// resolve. Without this, `->to('Users#list')` couldn't find
/// `test_files/lib/Users.pm` because nothing ever triggers a
/// module_index populate for workspace files.
pub fn index_workspace_with_index(
    root: &std::path::Path,
    files: &crate::file_store::FileStore,
    module_index: Option<&crate::module_index::ModuleIndex>,
) -> usize {
    use ignore::types::TypesBuilder;
    use ignore::WalkBuilder;
    use rayon::prelude::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    let mut types_builder = TypesBuilder::new();
    types_builder.add("perl", "*.pm").unwrap();
    types_builder.add("perl", "*.pl").unwrap();
    types_builder.add("perl", "*.t").unwrap();
    types_builder.select("perl");
    let types = types_builder.build().unwrap();

    let paths: Vec<PathBuf> = WalkBuilder::new(root)
        .types(types)
        .build()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .filter(|e| e.metadata().map(|m| m.len() < 1_000_000).unwrap_or(false))
        .map(|e| e.into_path())
        .collect();

    let count = AtomicUsize::new(0);

    paths.par_iter().for_each(|path| {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let source = std::fs::read_to_string(path).ok()?;
            let mut parser = create_parser();
            let tree = parser.parse(&source, None)?;
            Some(crate::builder::build(&tree, source.as_bytes()))
        }));

        match result {
            Ok(Some(analysis)) => {
                let arc = std::sync::Arc::new(analysis);
                files.insert_workspace_arc(path.clone(), arc.clone());
                if let Some(idx) = module_index {
                    idx.register_workspace_module(path.clone(), arc);
                }
                count.fetch_add(1, Ordering::Relaxed);
            }
            Ok(None) => { /* parse failed, skip */ }
            Err(_) => {
                log::warn!("Panic while indexing {:?}, skipping", path);
            }
        }
    });

    count.load(Ordering::Relaxed)
}

/// Scan @INC directories for .pm files, populating the available_modules map.
/// Fast — no file reads, just directory traversal + path→module name conversion.
fn scan_inc_module_names(inc_paths: &[PathBuf], available: &DashMap<String, PathBuf>) {
    for inc in inc_paths {
        if inc.is_dir() {
            scan_dir_recursive(inc, inc, available, 0);
        }
    }
}

fn scan_dir_recursive(base: &std::path::Path, dir: &std::path::Path, available: &DashMap<String, PathBuf>, depth: u32) {
    if depth > 15 { return; } // prevent symlink loops
    let entries = match std::fs::read_dir(dir) {
        Ok(rd) => rd,
        Err(_) => return,
    };
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_dir() {
            scan_dir_recursive(base, &path, available, depth + 1);
        } else if path.extension().map(|e| e == "pm").unwrap_or(false) {
            if let Ok(rel) = path.strip_prefix(base) {
                let module_name = rel.to_string_lossy()
                    .trim_end_matches(".pm")
                    .replace(std::path::MAIN_SEPARATOR, "::");
                available.insert(module_name, path.clone());
            }
        }
    }
}

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(PathBuf::from)
}

#[cfg(test)]
#[path = "module_resolver_tests.rs"]
mod tests;
