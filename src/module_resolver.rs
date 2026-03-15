//! Module resolver: background thread that resolves Perl modules from `@INC`.
//!
//! Discovers `@INC` paths, locates `.pm` files, extracts `@EXPORT`/`@EXPORT_OK`,
//! and handles subprocess isolation for safe parsing (tree-sitter hangs → SIGKILL).

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
#[cfg(not(test))]
use crate::file_analysis::inferred_type_from_tag;
use crate::file_analysis::inferred_type_to_tag;
use crate::module_cache;
use crate::module_index::{ExportedParam, ExportedSub, ModuleExports, ResolveNotify, ResolveQueue, WorkspaceRootChannel};

/// Hard timeout for parsing a single .pm file in a child process.
#[cfg(not(test))]
const PARSE_TIMEOUT: Duration = Duration::from_secs(5);

/// Callback invoked after each module is resolved. Used to trigger diagnostic refresh.
pub type OnResolved = Box<dyn Fn() + Send + Sync>;

/// Spawn the resolver thread. Returns immediately; the thread runs in the background.
///
/// The `on_resolved` callback fires after each module is inserted into the cache,
/// allowing the backend to re-publish diagnostics.
pub fn spawn_resolver(
    cache: Arc<DashMap<String, Option<ModuleExports>>>,
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    stale_modules: Arc<DashMap<String, ()>>,
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
            let inc_paths = discover_inc_paths();

            // Wait for workspace root from initialize() for per-project cache path.
            let ws_root = wait_for_workspace_root(&workspace_root);

            // Warm the in-memory cache from SQLite.
            let db = module_cache::open_cache_db(ws_root.as_deref());
            if let Some(ref conn) = db {
                let _ = module_cache::validate_inc_paths(conn, &inc_paths);
                let (n, stale_names) = module_cache::warm_cache(conn, &cache);
                log::info!("Warmed module cache: {} entries loaded from disk, {} stale", n, stale_names.len());
                // Populate stale_modules set for priority re-resolution.
                for name in stale_names {
                    stale_modules.insert(name, ());
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
                        Some(e) => log::info!(
                            "Resolved '{}': {} export, {} export_ok",
                            module_name,
                            e.export.len(),
                            e.export_ok.len()
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
    cache: Arc<DashMap<String, Option<ModuleExports>>>,
    reverse_index: Arc<DashMap<String, Vec<String>>>,
    stale_modules: Arc<DashMap<String, ()>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
) {
    std::thread::Builder::new()
        .name("module-resolver-test".into())
        .spawn(move || {
            let inc_paths = discover_inc_paths();
            let ws_root = wait_for_workspace_root(&workspace_root);

            let db = module_cache::open_cache_db(ws_root.as_deref());
            if let Some(ref conn) = db {
                let _ = module_cache::validate_inc_paths(conn, &inc_paths);
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

/// Insert a resolved module into the cache and update the reverse index.
fn insert_into_cache(
    cache: &DashMap<String, Option<ModuleExports>>,
    reverse_index: &DashMap<String, Vec<String>>,
    module_name: &str,
    result: Option<ModuleExports>,
) {
    if let Some(ref exports) = result {
        for func in exports.export.iter().chain(exports.export_ok.iter()) {
            reverse_index
                .entry(func.clone())
                .or_default()
                .push(module_name.to_string());
        }
    }
    cache.insert(module_name.to_string(), result);
}

/// Rebuild reverse index from existing cache (e.g. after warming from SQLite).
fn rebuild_reverse_index(
    cache: &DashMap<String, Option<ModuleExports>>,
    reverse_index: &DashMap<String, Vec<String>>,
) {
    for entry in cache.iter() {
        if let Some(ref exports) = *entry.value() {
            for func in exports.export.iter().chain(exports.export_ok.iter()) {
                reverse_index
                    .entry(func.clone())
                    .or_default()
                    .push(entry.key().clone());
            }
        }
    }
}

// ---- Module parsing ----

/// In production: subprocess isolation with hard timeout (SIGKILL on hang).
/// In tests: direct parsing (the test binary doesn't handle --parse-exports).
#[cfg(not(test))]
fn parse_module(inc_paths: &[PathBuf], module_name: &str) -> Option<ModuleExports> {
    parse_in_subprocess(inc_paths, module_name)
}

#[cfg(test)]
fn parse_module(inc_paths: &[PathBuf], module_name: &str) -> Option<ModuleExports> {
    let mut parser = create_parser();
    resolve_and_parse(inc_paths, module_name, &mut parser)
}

#[cfg(not(test))]
fn parse_in_subprocess(inc_paths: &[PathBuf], module_name: &str) -> Option<ModuleExports> {
    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    let file_size = metadata.len();
    if file_size > 1_000_000 {
        log::warn!(
            "Skipping '{}' — file too large ({} bytes): {:?}",
            module_name,
            file_size,
            path
        );
        return None;
    }
    log::info!(
        "Subprocess parse '{}' ({} bytes): {:?}",
        module_name,
        file_size,
        path
    );

    let exe = std::env::current_exe().ok()?;
    let mut child = std::process::Command::new(exe)
        .arg("--parse-exports")
        .arg(&path)
        .arg(module_name)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;

    let stdout = child.stdout.take()?;
    let (tx, rx) = std::sync::mpsc::sync_channel(1);
    std::thread::Builder::new()
        .name(format!("read-{}", module_name))
        .spawn(move || {
            let result = std::io::read_to_string(stdout);
            let _ = tx.send(result);
        })
        .ok()?;

    let output = match rx.recv_timeout(PARSE_TIMEOUT) {
        Ok(Ok(s)) => s,
        _ => {
            let _ = child.kill();
            let _ = child.wait();
            log::warn!("Timed out parsing module '{}'", module_name);
            return None;
        }
    };
    let _ = child.wait();

    let json: serde_json::Value = serde_json::from_str(&output).ok()?;
    let export: Vec<String> = json
        .get("export")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();
    let export_ok: Vec<String> = json
        .get("export_ok")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    // Parse subs metadata from JSON
    let subs: HashMap<String, ExportedSub> = json
        .get("subs")
        .and_then(|v| v.as_object())
        .map(|obj| {
            obj.iter().filter_map(|(name, val)| {
                let def_line = val.get("def_line").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
                let is_method = val.get("is_method").and_then(|v| v.as_bool()).unwrap_or(false);
                let params = val.get("params")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().filter_map(|p| {
                        Some(ExportedParam {
                            name: p.get("name")?.as_str()?.to_string(),
                            is_slurpy: p.get("is_slurpy").and_then(|v| v.as_bool()).unwrap_or(false),
                            inferred_type: p.get("type").and_then(|v| v.as_str()).map(|s| s.to_string()),
                        })
                    }).collect())
                    .unwrap_or_default();
                let return_type = val.get("return_type")
                    .and_then(|v| v.as_str())
                    .and_then(inferred_type_from_tag);
                let hash_keys = val.get("hash_keys")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                    .unwrap_or_default();
                let doc = val.get("doc")
                    .and_then(|v| v.as_str())
                    .map(String::from);
                Some((name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys, doc }))
            }).collect()
        })
        .unwrap_or_default();

    // Extract parents from subprocess JSON
    let parents: Vec<String> = json
        .get("parents")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    Some(ModuleExports {
        path,
        export,
        export_ok,
        subs,
        parents,
    })
}

/// Entry point for `--parse-exports <path> [module_name]` subprocess mode.
pub fn subprocess_main(path: &str, module_name: Option<&str>) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => {
            println!("{{}}");
            return;
        }
    };
    let mut parser = create_parser();

    // Extract exports via tree-sitter queries (also returns the parsed tree)
    let (export, export_ok, tree) = match extract_exports(&mut parser, &source) {
        Some(tuple) => tuple,
        None => {
            println!("{{}}");
            return;
        }
    };

    // Run the builder to get full sub metadata for exported functions
    let mut subs_map = serde_json::Map::new();
    let mut parents: Vec<String> = Vec::new();
    {
        use crate::file_analysis::{SymKind, SymbolDetail};
        let analysis = crate::builder::build(&tree, source.as_bytes());

        // Extract parents for the primary package
        // Prefer exact match on module_name (e.g. "Foo::Bar"), then stem match, then single-entry or first
        let expected_pkg = module_name.map(|m| m.replace("::", "::"));
        if let Some(ref pkg_name) = expected_pkg {
            if let Some(pkg_parents) = analysis.package_parents.get(pkg_name.as_str()) {
                parents = pkg_parents.clone();
            }
        }
        if parents.is_empty() {
            if analysis.package_parents.len() == 1 {
                if let Some((_pkg, pkg_parents)) = analysis.package_parents.iter().next() {
                    parents = pkg_parents.clone();
                }
            }
        }

        for name in export.iter().chain(export_ok.iter()) {
            let mut sub_info = serde_json::Map::new();

            // Find the sub symbol for definition line + params + doc
            for sym in &analysis.symbols {
                if sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    sub_info.insert("def_line".into(), sym.span.start.row.into());
                    if let SymbolDetail::Sub { ref params, is_method, ref doc, .. } = sym.detail {
                        sub_info.insert("is_method".into(), is_method.into());
                        let params_json: Vec<serde_json::Value> = params.iter()
                            .map(|p| {
                                let mut obj = serde_json::json!({
                                    "name": p.name,
                                    "is_slurpy": p.is_slurpy,
                                });
                                // Carry inferred param type for cross-file signature help
                                if let Some(ty) = analysis.inferred_type(&p.name, sym.span.end) {
                                    obj["type"] = serde_json::Value::String(inferred_type_to_tag(ty));
                                }
                                obj
                            })
                            .collect();
                        sub_info.insert("params".into(), params_json.into());
                        if let Some(ref d) = doc {
                            sub_info.insert("doc".into(), serde_json::Value::String(d.clone()));
                        }
                    }
                    break;
                }
            }

            // Return type
            if let Some(ty) = analysis.sub_return_type(name) {
                sub_info.insert("return_type".into(),
                    serde_json::Value::String(inferred_type_to_tag(ty)));
            }

            // Hash keys
            let owner = crate::file_analysis::HashKeyOwner::Sub(name.clone());
            let keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
                .iter()
                .map(|s| s.name.clone())
                .collect();
            if !keys.is_empty() {
                sub_info.insert("hash_keys".into(), serde_json::json!(keys));
            }

            if !sub_info.is_empty() {
                subs_map.insert(name.clone(), sub_info.into());
            }
        }

        // Also include all subs not in @EXPORT/@EXPORT_OK — needed for inheritance.
        for sym in &analysis.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
            if subs_map.contains_key(&sym.name) { continue; }
            let is_method = matches!(sym.detail, SymbolDetail::Sub { is_method: true, .. })
                || sym.kind == SymKind::Method;
            let mut sub_info = serde_json::Map::new();
            sub_info.insert("def_line".into(), sym.span.start.row.into());
            sub_info.insert("is_method".into(), is_method.into());
            if let SymbolDetail::Sub { ref params, ref doc, .. } = sym.detail {
                let params_json: Vec<serde_json::Value> = params.iter()
                    .map(|p| {
                        let mut obj = serde_json::json!({
                            "name": p.name,
                            "is_slurpy": p.is_slurpy,
                        });
                        if let Some(ty) = analysis.inferred_type(&p.name, sym.span.end) {
                            obj["type"] = serde_json::Value::String(inferred_type_to_tag(ty));
                        }
                        obj
                    })
                    .collect();
                sub_info.insert("params".into(), params_json.into());
                if let Some(ref d) = doc {
                    sub_info.insert("doc".into(), serde_json::Value::String(d.clone()));
                }
            }
            if let Some(ty) = analysis.sub_return_type(&sym.name) {
                sub_info.insert("return_type".into(),
                    serde_json::Value::String(inferred_type_to_tag(ty)));
            }
            let owner = crate::file_analysis::HashKeyOwner::Sub(sym.name.clone());
            let keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
                .iter()
                .map(|s| s.name.clone())
                .collect();
            if !keys.is_empty() {
                sub_info.insert("hash_keys".into(), serde_json::json!(keys));
            }
            subs_map.insert(sym.name.clone(), sub_info.into());
        }
    }

    let json = serde_json::json!({
        "export": export,
        "export_ok": export_ok,
        "subs": subs_map,
        "parents": parents,
    });
    println!("{}", json);
}

pub fn create_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_perl::LANGUAGE.into())
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
) -> Option<ModuleExports> {
    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    if metadata.len() > 1_000_000 {
        return None;
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let (export, export_ok, tree) = extract_exports(parser, &source)?;

    // Extract full sub metadata and parents from builder analysis
    let mut subs = HashMap::new();
    let mut parents: Vec<String> = Vec::new();
    {
        use crate::file_analysis::{SymKind, SymbolDetail};
        let analysis = crate::builder::build(&tree, source.as_bytes());

        // Extract parents for the primary package
        if let Some(pkg_parents) = analysis.package_parents.get(module_name) {
            parents = pkg_parents.clone();
        } else if analysis.package_parents.len() == 1 {
            if let Some((_pkg, pkg_parents)) = analysis.package_parents.iter().next() {
                parents = pkg_parents.clone();
            }
        }

        for name in export.iter().chain(export_ok.iter()) {
            let mut def_line = 0u32;
            let mut params = Vec::new();
            let mut is_method = false;
            let mut doc = None;

            for sym in &analysis.symbols {
                if sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    def_line = sym.span.start.row as u32;
                    if let SymbolDetail::Sub { params: ref p, is_method: m, doc: ref d, .. } = sym.detail {
                        is_method = m;
                        params = p.iter()
                            .map(|pi| {
                                let param_type = analysis.inferred_type(&pi.name, sym.span.end)
                                    .map(|t| inferred_type_to_tag(t));
                                ExportedParam { name: pi.name.clone(), is_slurpy: pi.is_slurpy, inferred_type: param_type }
                            })
                            .collect();
                        doc = d.clone();
                    }
                    break;
                }
            }

            let return_type = analysis.sub_return_type(name).cloned();
            let owner = crate::file_analysis::HashKeyOwner::Sub(name.clone());
            let hash_keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
                .iter()
                .map(|s| s.name.clone())
                .collect();

            subs.insert(name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys, doc });
        }

        // Also include methods not in @EXPORT/@EXPORT_OK — needed for inheritance.
        for sym in &analysis.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
            if subs.contains_key(&sym.name) { continue; }
            let is_method_flag = matches!(sym.detail, SymbolDetail::Sub { is_method: true, .. })
                || sym.kind == SymKind::Method;
            let def_line = sym.span.start.row as u32;
            let mut params = Vec::new();
            let mut doc = None;
            if let SymbolDetail::Sub { params: ref p, doc: ref d, .. } = sym.detail {
                params = p.iter()
                    .map(|pi| {
                        let param_type = analysis.inferred_type(&pi.name, sym.span.end)
                            .map(|t| inferred_type_to_tag(t));
                        ExportedParam { name: pi.name.clone(), is_slurpy: pi.is_slurpy, inferred_type: param_type }
                    })
                    .collect();
                doc = d.clone();
            }
            let return_type = analysis.sub_return_type(&sym.name).cloned();
            let owner = crate::file_analysis::HashKeyOwner::Sub(sym.name.clone());
            let hash_keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
                .iter()
                .map(|s| s.name.clone())
                .collect();
            subs.insert(sym.name.clone(), ExportedSub {
                def_line, params, is_method: is_method_flag, return_type, hash_keys, doc,
            });
        }
    }

    Some(ModuleExports {
        path,
        export,
        export_ok,
        subs,
        parents,
    })
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

// ---- Export extraction ----

fn extract_exports(parser: &mut Parser, source: &str) -> Option<(Vec<String>, Vec<String>, tree_sitter::Tree)> {
    use tree_sitter::{QueryCursor, StreamingIterator};

    let tree = parser.parse(source, None)?;
    let root = tree.root_node();
    let bytes = source.as_bytes();

    let mut export = Vec::new();
    let mut export_ok = Vec::new();

    // Query 1: @EXPORT = qw(...)
    let qw_query = crate::query_cache::exports_qw();
    let qw_var_idx = qw_query.capture_index_for_name("var").unwrap();
    let qw_words_idx = qw_query.capture_index_for_name("words").unwrap();

    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(qw_query, root, bytes);
    while let Some(m) = matches.next() {
        let mut var_name = "";
        let mut words = Vec::new();
        for cap in m.captures {
            let text = cap.node.utf8_text(bytes).unwrap_or("");
            if cap.index == qw_var_idx {
                var_name = text;
            } else if cap.index == qw_words_idx {
                words.extend(text.split_whitespace().map(String::from));
            }
        }
        match var_name {
            "@EXPORT_OK" => export_ok.extend(words),
            "@EXPORT" => export.extend(words),
            _ => {}
        }
    }

    // Query 2: @EXPORT = ('foo', 'bar')
    let paren_query = crate::query_cache::exports_paren_list();
    let paren_var_idx = paren_query.capture_index_for_name("var").unwrap();
    let paren_word_idx = paren_query.capture_index_for_name("word").unwrap();

    let mut cursor2 = QueryCursor::new();
    let mut matches2 = cursor2.matches(paren_query, root, bytes);
    while let Some(m) = matches2.next() {
        let mut var_name = "";
        let mut words = Vec::new();
        for cap in m.captures {
            let text = cap.node.utf8_text(bytes).unwrap_or("");
            if cap.index == paren_var_idx {
                var_name = text;
            } else if cap.index == paren_word_idx {
                words.push(text.to_string());
            }
        }
        match var_name {
            "@EXPORT_OK" => export_ok.extend(words),
            "@EXPORT" => export.extend(words),
            _ => {}
        }
    }

    Some((export, export_ok, tree))
}

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(PathBuf::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_module_list_util() {
        let inc_paths = discover_inc_paths();
        if inc_paths.is_empty() {
            return;
        }
        let path = resolve_module_path(&inc_paths, "List::Util");
        assert!(path.is_some(), "List::Util should be resolvable");
        let p = path.unwrap();
        assert!(p.to_str().unwrap().contains("List/Util.pm"));
    }

    #[test]
    fn test_extract_exports_qw() {
        let source = r#"
package Foo;
use Exporter 'import';
our @EXPORT_OK = qw(alpha beta gamma);
our @EXPORT = qw(delta);
1;
"#;
        let mut parser = create_parser();
        let (export, export_ok, _tree) = extract_exports(&mut parser, source).unwrap();
        assert_eq!(export, vec!["delta"]);
        assert_eq!(export_ok, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn test_extract_exports_parenthesized() {
        let source = r#"
package Bar;
our @EXPORT_OK = ('foo', 'bar', 'baz');
1;
"#;
        let mut parser = create_parser();
        let (_, export_ok, _tree) = extract_exports(&mut parser, source).unwrap();
        assert_eq!(export_ok, vec!["foo", "bar", "baz"]);
    }

    #[test]
    fn test_discover_inc_paths() {
        let paths = discover_inc_paths();
        if !paths.is_empty() {
            assert!(paths.iter().all(|p| p.is_dir()));
        }
    }

    #[test]
    fn test_uri_to_path() {
        assert_eq!(
            uri_to_path("file:///Users/foo/project"),
            Some(PathBuf::from("/Users/foo/project"))
        );
        assert_eq!(uri_to_path("http://example.com"), None);
    }
}
