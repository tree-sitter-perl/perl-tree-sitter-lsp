//! Module resolver: background thread that resolves Perl modules from `@INC`.
//!
//! Discovers `@INC` paths, locates `.pm` files, extracts `@EXPORT`/`@EXPORT_OK`,
//! and handles subprocess isolation for safe parsing (tree-sitter hangs → SIGKILL).

use std::collections::HashSet;
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
use crate::module_index::{ModuleExports, ResolveNotify, ResolveQueue, WorkspaceRootChannel};

/// Hard timeout for parsing a single .pm file in a child process.
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

    Some(ModuleExports {
        path,
        export,
        export_ok,
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
    match extract_exports(&mut parser, &source) {
        Some((export, export_ok)) => {
            let json = serde_json::json!({
                "export": export,
                "export_ok": export_ok,
            });
            println!("{}", json);
        }
        None => println!("{{}}"),
    }
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
    let (export, export_ok) = extract_exports(parser, &source)?;
    Some(ModuleExports {
        path,
        export,
        export_ok,
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

fn extract_exports(parser: &mut Parser, source: &str) -> Option<(Vec<String>, Vec<String>)> {
    let tree = parser.parse(source, None)?;
    let root = tree.root_node();
    let bytes = source.as_bytes();

    let mut export = Vec::new();
    let mut export_ok = Vec::new();

    collect_exports(root, bytes, &mut export, &mut export_ok);

    if export.is_empty() && export_ok.is_empty() {
        return None;
    }
    Some((export, export_ok))
}

fn collect_exports(
    node: tree_sitter::Node,
    source: &[u8],
    export: &mut Vec<String>,
    export_ok: &mut Vec<String>,
) {
    match node.kind() {
        "assignment_expression" => {
            if let Some((var_name, words)) = extract_export_assignment(node, source) {
                match var_name.as_str() {
                    "@EXPORT_OK" | "EXPORT_OK" => export_ok.extend(words),
                    "@EXPORT" | "EXPORT" => export.extend(words),
                    _ => {}
                }
            }
        }
        _ => {
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    collect_exports(child, source, export, export_ok);
                }
            }
        }
    }
}

fn extract_export_assignment(
    node: tree_sitter::Node,
    source: &[u8],
) -> Option<(String, Vec<String>)> {
    let left = node.child_by_field_name("left")?;

    let var_name = extract_array_var_name(left, source)?;
    if !matches!(
        var_name.as_str(),
        "@EXPORT" | "EXPORT" | "@EXPORT_OK" | "EXPORT_OK"
    ) {
        return None;
    }

    let mut words = Vec::new();
    if let Some(right) = node.child_by_field_name("right") {
        words = extract_word_list(right, source);
    }
    if words.is_empty() {
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                let w = extract_word_list(child, source);
                if !w.is_empty() {
                    words = w;
                    break;
                }
            }
        }
    }
    if words.is_empty() {
        return None;
    }

    Some((var_name, words))
}

fn extract_array_var_name(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    match node.kind() {
        "variable_declaration" => {
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    if child.kind() == "array" {
                        return child.utf8_text(source).ok().map(|s| s.to_string());
                    }
                }
            }
            None
        }
        "array" => node.utf8_text(source).ok().map(|s| s.to_string()),
        _ => None,
    }
}

fn extract_word_list(node: tree_sitter::Node, source: &[u8]) -> Vec<String> {
    match node.kind() {
        "quoted_word_list" => {
            let mut words = Vec::new();
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    if child.kind() == "string_content" {
                        if let Ok(text) = child.utf8_text(source) {
                            for word in text.split_whitespace() {
                                if !word.is_empty() {
                                    words.push(word.to_string());
                                }
                            }
                        }
                    }
                }
            }
            words
        }
        "parenthesized_expression" | "list_expression" => {
            let mut words = Vec::new();
            collect_string_literals(node, source, &mut words);
            words
        }
        _ => vec![],
    }
}

fn collect_string_literals(node: tree_sitter::Node, source: &[u8], words: &mut Vec<String>) {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            match child.kind() {
                "string_literal" | "interpolated_string_literal" => {
                    for j in 0..child.named_child_count() {
                        if let Some(content) = child.named_child(j) {
                            if content.kind() == "string_content" {
                                if let Ok(text) = content.utf8_text(source) {
                                    words.push(text.to_string());
                                }
                            }
                        }
                    }
                }
                "list_expression" | "parenthesized_expression" => {
                    collect_string_literals(child, source, words);
                }
                _ => {}
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
        let (export, export_ok) = extract_exports(&mut parser, source).unwrap();
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
        let (_, export_ok) = extract_exports(&mut parser, source).unwrap();
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
