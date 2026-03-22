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
use crate::module_index::{ExportedOverload, ExportedParam, ExportedSub, ModuleExports, ResolveNotify, ResolveQueue, WorkspaceRootChannel};

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
            let inc_paths = discover_inc_paths();

            // Scan @INC for available module names (fast, no parsing — just readdir)
            scan_inc_module_names(&inc_paths, &available_modules);
            log::info!("@INC scan: {} modules available", available_modules.len());

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
    available_modules: Arc<DashMap<String, PathBuf>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
) {
    std::thread::Builder::new()
        .name("module-resolver-test".into())
        .spawn(move || {
            let inc_paths = discover_inc_paths();
            scan_inc_module_names(&inc_paths, &available_modules);
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
                let overloads = val.get("overloads")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().filter_map(|o| {
                        let ol_params = o.get("params")
                            .and_then(|p| p.as_array())
                            .map(|pa| pa.iter().filter_map(|pv| {
                                let pname = pv.get("name")?.as_str()?.to_string();
                                let slurpy = pv.get("is_slurpy").and_then(|v| v.as_bool()).unwrap_or(false);
                                let inferred_type = pv.get("type").and_then(|v| v.as_str()).map(String::from);
                                Some(ExportedParam { name: pname, is_slurpy: slurpy, inferred_type })
                            }).collect())
                            .unwrap_or_default();
                        let ol_rt = o.get("return_type")
                            .and_then(|v| v.as_str())
                            .and_then(inferred_type_from_tag);
                        Some(ExportedOverload { params: ol_params, return_type: ol_rt })
                    }).collect())
                    .unwrap_or_default();
                Some((name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys, doc, overloads }))
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

/// Collect export metadata from a FileAnalysis for a set of export names.
/// Returns (subs, parents) ready for serialization or direct use.
fn collect_export_metadata(
    analysis: &crate::file_analysis::FileAnalysis,
    export: &[String],
    export_ok: &[String],
    module_name: Option<&str>,
) -> (HashMap<String, ExportedSub>, Vec<String>) {
    use crate::file_analysis::{SymKind, SymbolDetail, HashKeyOwner};

    // Extract parents for the primary package
    let mut parents: Vec<String> = Vec::new();
    if let Some(pkg_name) = module_name {
        if let Some(pkg_parents) = analysis.package_parents.get(pkg_name) {
            parents = pkg_parents.clone();
        }
    }
    if parents.is_empty() && analysis.package_parents.len() == 1 {
        if let Some((_pkg, pkg_parents)) = analysis.package_parents.iter().next() {
            parents = pkg_parents.clone();
        }
    }

    let mut subs = HashMap::new();

    // Helper closure: build ExportedParam list from symbol params
    let build_params = |sym: &crate::file_analysis::Symbol| -> Vec<ExportedParam> {
        if let SymbolDetail::Sub { ref params, .. } = sym.detail {
            params.iter()
                .map(|p| {
                    let param_type = analysis.inferred_type(&p.name, sym.span.end)
                        .map(|t| inferred_type_to_tag(t));
                    ExportedParam { name: p.name.clone(), is_slurpy: p.is_slurpy, inferred_type: param_type }
                })
                .collect()
        } else {
            Vec::new()
        }
    };

    // Helper closure: build ExportedOverload from a symbol
    let build_overload = |sym: &crate::file_analysis::Symbol| -> ExportedOverload {
        let params = build_params(sym);
        let return_type = if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
            return_type.clone()
        } else {
            None
        };
        ExportedOverload { params, return_type }
    };

    // First loop: exported names
    for name in export.iter().chain(export_ok.iter()) {
        let matching_syms: Vec<_> = analysis.symbols.iter()
            .filter(|sym| sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method))
            .collect();

        let (def_line, params, is_method, doc) = if let Some(primary) = matching_syms.first() {
            let is_method = matches!(primary.detail, SymbolDetail::Sub { is_method: true, .. })
                || primary.kind == SymKind::Method;
            let doc = if let SymbolDetail::Sub { ref doc, .. } = primary.detail {
                doc.clone()
            } else {
                None
            };
            (primary.span.start.row as u32, build_params(primary), is_method, doc)
        } else {
            (0u32, Vec::new(), false, None)
        };

        let return_type = analysis.sub_return_type(name).cloned();
        let owner = HashKeyOwner::Sub(name.clone());
        let hash_keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
            .iter().map(|s| s.name.clone()).collect();

        let overloads: Vec<ExportedOverload> = matching_syms.iter().skip(1)
            .map(|sym| build_overload(sym))
            .collect();

        subs.insert(name.clone(), ExportedSub {
            def_line, params, is_method, return_type, hash_keys, doc, overloads,
        });
    }

    // Names fully handled by the export loop above
    let exported_names: std::collections::HashSet<&str> =
        export.iter().chain(export_ok.iter()).map(|s| s.as_str()).collect();

    // Second loop: non-exported subs (needed for inheritance)
    for sym in &analysis.symbols {
        if !matches!(sym.kind, SymKind::Sub | SymKind::Method) { continue; }
        if exported_names.contains(sym.name.as_str()) { continue; }

        // Merge duplicate names as overloads (e.g. getter + setter for rw accessors)
        if let Some(existing) = subs.get_mut(&sym.name) {
            existing.overloads.push(build_overload(sym));
            continue;
        }

        let is_method = matches!(sym.detail, SymbolDetail::Sub { is_method: true, .. })
            || sym.kind == SymKind::Method;
        let doc = if let SymbolDetail::Sub { ref doc, .. } = sym.detail {
            doc.clone()
        } else {
            None
        };
        let return_type = analysis.sub_return_type(&sym.name).cloned();
        let owner = HashKeyOwner::Sub(sym.name.clone());
        let hash_keys: Vec<String> = analysis.hash_key_defs_for_owner(&owner)
            .iter().map(|s| s.name.clone()).collect();

        subs.insert(sym.name.clone(), ExportedSub {
            def_line: sym.span.start.row as u32,
            params: build_params(sym),
            is_method,
            return_type,
            hash_keys,
            doc,
            overloads: vec![],
        });
    }

    (subs, parents)
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

    let tree = match parser.parse(&source, None) {
        Some(t) => t,
        None => {
            println!("{{}}");
            return;
        }
    };

    let analysis = crate::builder::build(&tree, source.as_bytes());
    let export = &analysis.export;
    let export_ok = &analysis.export_ok;
    let (subs, parents) = collect_export_metadata(&analysis, export, export_ok, module_name);

    // Serialize to JSON for subprocess IPC
    let mut subs_map = serde_json::Map::new();
    for (name, sub) in &subs {
        let mut sub_info = serde_json::Map::new();
        sub_info.insert("def_line".into(), sub.def_line.into());
        sub_info.insert("is_method".into(), sub.is_method.into());
        let params_json: Vec<serde_json::Value> = sub.params.iter()
            .map(|p| {
                let mut obj = serde_json::json!({ "name": p.name, "is_slurpy": p.is_slurpy });
                if let Some(ref ty) = p.inferred_type {
                    obj["type"] = serde_json::Value::String(ty.clone());
                }
                obj
            })
            .collect();
        sub_info.insert("params".into(), params_json.into());
        if let Some(ref d) = sub.doc {
            sub_info.insert("doc".into(), serde_json::Value::String(d.clone()));
        }
        if let Some(ref ty) = sub.return_type {
            sub_info.insert("return_type".into(),
                serde_json::Value::String(inferred_type_to_tag(ty)));
        }
        if !sub.hash_keys.is_empty() {
            sub_info.insert("hash_keys".into(), serde_json::json!(sub.hash_keys));
        }
        if !sub.overloads.is_empty() {
            let overloads: Vec<serde_json::Value> = sub.overloads.iter().map(|ol| {
                let ol_params: Vec<serde_json::Value> = ol.params.iter()
                    .map(|p| {
                        let mut obj = serde_json::json!({ "name": p.name, "is_slurpy": p.is_slurpy });
                        if let Some(ref ty) = p.inferred_type {
                            obj["type"] = serde_json::Value::String(ty.clone());
                        }
                        obj
                    })
                    .collect();
                let mut overload = serde_json::json!({ "params": ol_params });
                if let Some(ref rt) = ol.return_type {
                    overload["return_type"] = serde_json::Value::String(inferred_type_to_tag(rt));
                }
                overload
            }).collect();
            sub_info.insert("overloads".into(), serde_json::json!(overloads));
        }
        subs_map.insert(name.clone(), sub_info.into());
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
) -> Option<ModuleExports> {
    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    if metadata.len() > 1_000_000 {
        return None;
    }
    let source = std::fs::read_to_string(&path).ok()?;
    let tree = parser.parse(&source, None)?;

    let analysis = crate::builder::build(&tree, source.as_bytes());
    let export = analysis.export.clone();
    let export_ok = analysis.export_ok.clone();
    let (subs, parents) = collect_export_metadata(&analysis, &export, &export_ok, Some(module_name));

    Some(ModuleExports { path, export, export_ok, subs, parents })
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
        let tree = parser.parse(source, None).unwrap();
        let analysis = crate::builder::build(&tree, source.as_bytes());
        assert_eq!(analysis.export, vec!["delta"]);
        assert_eq!(analysis.export_ok, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn test_extract_exports_parenthesized() {
        let source = r#"
package Bar;
our @EXPORT_OK = ('foo', 'bar', 'baz');
1;
"#;
        let mut parser = create_parser();
        let tree = parser.parse(source, None).unwrap();
        let analysis = crate::builder::build(&tree, source.as_bytes());
        assert_eq!(analysis.export_ok, vec!["foo", "bar", "baz"]);
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
