//! Module resolver: background thread that resolves Perl modules from `@INC`.
//!
//! Discovers `@INC` paths, locates `.pm` files, extracts `@EXPORT`/`@EXPORT_OK`,
//! and handles subprocess isolation for safe parsing (tree-sitter hangs → SIGKILL).

use std::collections::{HashMap, HashSet};
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
                let n = module_cache::warm_cache(conn, &cache);
                log::info!("Warmed module cache: {} entries loaded from disk", n);
                // Build reverse index from warmed cache.
                rebuild_reverse_index(&cache, &reverse_index);
            }

            let mut seen = HashSet::new();

            // Pre-resolve cpanfile dependencies.
            if let Some(ref root_uri) = ws_root {
                if let Some(root_path) = uri_to_path(root_uri) {
                    let cpanfile_modules = cpanfile::parse_cpanfile(&root_path);
                    let to_resolve: Vec<String> = cpanfile_modules
                        .into_iter()
                        .filter(|m| !cache.contains_key(m.as_str()))
                        .collect();

                    if !to_resolve.is_empty() {
                        resolve_cpanfile_batch(
                            &to_resolve,
                            &inc_paths,
                            &cache,
                            &reverse_index,
                            &db,
                            &client,
                            &handle,
                            &on_resolved,
                        );
                        for m in &to_resolve {
                            seen.insert(m.clone());
                        }
                    }
                }
            }

            // Main resolve loop — drain work queue.
            loop {
                let batch = {
                    let mut pending = queue.pending.lock().unwrap();
                    while pending.is_empty() {
                        pending = queue.condvar.wait(pending).unwrap();
                    }
                    std::mem::take(&mut *pending)
                };

                for module_name in batch {
                    if !seen.insert(module_name.clone()) {
                        continue;
                    }

                    log::info!("Resolving module '{}'", module_name);
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

/// Simplified resolver for tests — no Client, no progress, no cpanfile.
#[cfg(test)]
pub fn spawn_test_resolver(
    cache: Arc<DashMap<String, Option<ModuleExports>>>,
    reverse_index: Arc<DashMap<String, Vec<String>>>,
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
                let _ = module_cache::warm_cache(conn, &cache);
                rebuild_reverse_index(&cache, &reverse_index);
            }

            let mut seen = HashSet::new();
            loop {
                let batch = {
                    let mut pending = queue.pending.lock().unwrap();
                    while pending.is_empty() {
                        pending = queue.condvar.wait(pending).unwrap();
                    }
                    std::mem::take(&mut *pending)
                };
                for module_name in batch {
                    if !seen.insert(module_name.clone()) {
                        continue;
                    }
                    let result = parse_module(&inc_paths, &module_name);
                    insert_into_cache(&cache, &reverse_index, &module_name, result.clone());
                    if let Some(ref conn) = db {
                        module_cache::save_to_db(conn, &module_name, &result, "import");
                    }
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

fn resolve_cpanfile_batch(
    to_resolve: &[String],
    inc_paths: &[PathBuf],
    cache: &Arc<DashMap<String, Option<ModuleExports>>>,
    reverse_index: &Arc<DashMap<String, Vec<String>>>,
    db: &Option<rusqlite::Connection>,
    client: &Client,
    handle: &tokio::runtime::Handle,
    on_resolved: &OnResolved,
) {
    let total = to_resolve.len();
    log::info!("cpanfile: {} modules to index", total);

    let token = NumberOrString::String("perl-lsp/indexing".to_string());
    let _ = handle.block_on(client.send_request::<request::WorkDoneProgressCreate>(
        WorkDoneProgressCreateParams {
            token: token.clone(),
        },
    ));
    handle.block_on(client.send_notification::<notification::Progress>(
        ProgressParams {
            token: token.clone(),
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

    for (i, module_name) in to_resolve.iter().enumerate() {
        let pct = ((i + 1) * 100 / total) as u32;
        handle.block_on(client.send_notification::<notification::Progress>(
            ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                    WorkDoneProgressReport {
                        cancellable: Some(false),
                        message: Some(format!("{} ({}/{})", module_name, i + 1, total)),
                        percentage: Some(pct),
                    },
                )),
            },
        ));

        log::info!(
            "cpanfile: resolving '{}' ({}/{})",
            module_name,
            i + 1,
            total
        );
        let result = parse_module(inc_paths, module_name);
        insert_into_cache(cache, reverse_index, module_name, result.clone());
        if let Some(ref conn) = db {
            module_cache::save_to_db(conn, module_name, &result, "cpanfile");
        }
    }

    handle.block_on(client.send_notification::<notification::Progress>(
        ProgressParams {
            token,
            value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                message: Some(format!("Indexed {} modules", total)),
            })),
        },
    ));

    // Trigger diagnostic refresh after cpanfile batch completes.
    on_resolved();
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

    if export.is_empty() && export_ok.is_empty() {
        return None;
    }

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
                Some((name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys }))
            }).collect()
        })
        .unwrap_or_default();

    Some(ModuleExports {
        path,
        export,
        export_ok,
        subs,
    })
}

/// Entry point for `--parse-exports <path>` subprocess mode.
pub fn subprocess_main(path: &str) {
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
    {
        use crate::file_analysis::{SymKind, SymbolDetail};
        let analysis = crate::builder::build(&tree, source.as_bytes());
        for name in export.iter().chain(export_ok.iter()) {
            let mut sub_info = serde_json::Map::new();

            // Find the sub symbol for definition line + params
            for sym in &analysis.symbols {
                if sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    sub_info.insert("def_line".into(), sym.span.start.row.into());
                    if let SymbolDetail::Sub { ref params, is_method, .. } = sym.detail {
                        sub_info.insert("is_method".into(), is_method.into());
                        let params_json: Vec<serde_json::Value> = params.iter()
                            .map(|p| serde_json::json!({
                                "name": p.name,
                                "is_slurpy": p.is_slurpy,
                            }))
                            .collect();
                        sub_info.insert("params".into(), params_json.into());
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
    }

    let json = serde_json::json!({
        "export": export,
        "export_ok": export_ok,
        "subs": subs_map,
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

    // Extract full sub metadata from builder analysis
    let mut subs = HashMap::new();
    {
        use crate::file_analysis::{SymKind, SymbolDetail};
        let analysis = crate::builder::build(&tree, source.as_bytes());
        for name in export.iter().chain(export_ok.iter()) {
            let mut def_line = 0u32;
            let mut params = Vec::new();
            let mut is_method = false;

            for sym in &analysis.symbols {
                if sym.name == *name && matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                    def_line = sym.span.start.row as u32;
                    if let SymbolDetail::Sub { params: ref p, is_method: m, .. } = sym.detail {
                        is_method = m;
                        params = p.iter()
                            .map(|pi| ExportedParam { name: pi.name.clone(), is_slurpy: pi.is_slurpy })
                            .collect();
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

            subs.insert(name.clone(), ExportedSub { def_line, params, is_method, return_type, hash_keys });
        }
    }

    Some(ModuleExports {
        path,
        export,
        export_ok,
        subs,
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

    if export.is_empty() && export_ok.is_empty() {
        return None;
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
