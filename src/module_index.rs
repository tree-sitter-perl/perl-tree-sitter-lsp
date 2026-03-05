//! Module index: resolves Perl module names to filesystem paths and extracts
//! @EXPORT / @EXPORT_OK lists.
//!
//! Used for cross-file intelligence: completion, go-to-def, hover for imported
//! functions.
//!
//! Architecture: a coordinator `std::thread` drains a work queue and dispatches
//! each module parse to an **isolated child process** with a hard timeout. If
//! tree-sitter's external scanner hangs (opaque C code that `set_timeout_micros`
//! cannot interrupt), the child is SIGKILL'd — no leaked spinning threads.
//! Async LSP handlers only read the `DashMap` cache (zero I/O).

use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification, request};
use tower_lsp::Client;
use tree_sitter::Parser;

/// Hard timeout for parsing a single .pm file. The parse runs in a child
/// process; if it exceeds this limit, the child is SIGKILL'd.
const PARSE_TIMEOUT: Duration = Duration::from_secs(5);

// ---- Public types ----

/// Exports extracted from a .pm file.
#[derive(Debug, Clone)]
pub struct ModuleExports {
    /// Filesystem path to the .pm file.
    pub path: PathBuf,
    /// @EXPORT — auto-imported on bare `use Foo;`.
    pub export: Vec<String>,
    /// @EXPORT_OK — available on request via `use Foo qw(...)`.
    pub export_ok: Vec<String>,
}

/// Thread-safe queue: Mutex<Vec> + Condvar. All fields are Sync.
struct ResolveQueue {
    pending: Mutex<Vec<String>>,
    condvar: Condvar,
}

/// Signaled after each module is resolved and inserted into the cache.
struct ResolveNotify {
    mu: Mutex<()>,
    cv: Condvar,
}

/// Channel for passing workspace root from initialize() to the resolver thread.
struct WorkspaceRootChannel {
    /// None = not yet received. Some(Some(root)) = have root. Some(None) = no root available.
    root: Mutex<Option<Option<String>>>,
    condvar: Condvar,
}

/// Concurrent cache of module exports with a dedicated background resolver.
///
/// A coordinator thread drains a `Mutex<Vec>` + `Condvar` queue and dispatches
/// each module parse to an isolated child process with a hard timeout. If the
/// child hangs, it's SIGKILL'd. Async LSP handlers only read the `DashMap`
/// cache (zero I/O).
pub struct ModuleIndex {
    cache: Arc<DashMap<String, Option<ModuleExports>>>,
    queue: Arc<ResolveQueue>,
    resolved: Arc<ResolveNotify>,
    workspace_root: Arc<WorkspaceRootChannel>,
}

impl ModuleIndex {
    pub fn new(client: Client) -> Self {
        let cache: Arc<DashMap<String, Option<ModuleExports>>> = Arc::new(DashMap::new());
        let queue = Arc::new(ResolveQueue {
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

        // Capture tokio handle so the resolver thread can send LSP notifications.
        let handle = tokio::runtime::Handle::current();

        // Coordinator thread — dispatches each parse to an isolated child
        // process. If tree-sitter's external scanner hangs, the child is
        // SIGKILL'd after PARSE_TIMEOUT — no leaked spinning threads.
        let thread_cache = Arc::clone(&cache);
        let thread_queue = Arc::clone(&queue);
        let thread_resolved = Arc::clone(&resolved);
        let thread_ws_root = Arc::clone(&workspace_root);
        std::thread::Builder::new()
            .name("module-resolver".into())
            .spawn(move || {
                let inc_paths = discover_inc_paths();

                // Wait for workspace root from initialize() for per-project cache path.
                // Normally arrives in < 1ms. Timeout after 10s (bad client or no root).
                let ws_root = {
                    let mut guard = thread_ws_root.root.lock().unwrap();
                    let deadline = std::time::Instant::now() + Duration::from_secs(10);
                    while guard.is_none() {
                        let remaining =
                            deadline.saturating_duration_since(std::time::Instant::now());
                        if remaining.is_zero() {
                            log::warn!("Timed out waiting for workspace root; using global cache");
                            break;
                        }
                        let (g, _) = thread_ws_root
                            .condvar
                            .wait_timeout(guard, remaining)
                            .unwrap();
                        guard = g;
                    }
                    guard.clone().flatten()
                };

                // Warm the in-memory cache from SQLite.
                let db = open_cache_db(ws_root.as_deref());
                if let Some(ref conn) = db {
                    let _ = validate_inc_paths(conn, &inc_paths);
                    let n = warm_cache(conn, &thread_cache);
                    log::info!("Warmed module cache: {} entries loaded from disk", n);
                }

                let mut seen = HashSet::new();

                // Pre-resolve cpanfile dependencies.
                if let Some(ref root_uri) = ws_root {
                    if let Some(root_path) = uri_to_path(root_uri) {
                        let cpanfile_modules = parse_cpanfile(&root_path);
                        let to_resolve: Vec<String> = cpanfile_modules
                            .into_iter()
                            .filter(|m| !thread_cache.contains_key(m.as_str()))
                            .collect();

                        if !to_resolve.is_empty() {
                            let total = to_resolve.len();
                            log::info!("cpanfile: {} modules to index", total);

                            // Begin progress.
                            let token = NumberOrString::String(
                                "perl-lsp/indexing".to_string(),
                            );
                            let _ = handle.block_on(client.send_request::<request::WorkDoneProgressCreate>(
                                WorkDoneProgressCreateParams {
                                    token: token.clone(),
                                },
                            ));
                            handle.block_on(client.send_notification::<notification::Progress>(
                                ProgressParams {
                                    token: token.clone(),
                                    value: ProgressParamsValue::WorkDone(
                                        WorkDoneProgress::Begin(WorkDoneProgressBegin {
                                            title: "Indexing Perl modules".into(),
                                            cancellable: Some(false),
                                            message: None,
                                            percentage: Some(0),
                                        }),
                                    ),
                                },
                            ));

                            for (i, module_name) in to_resolve.iter().enumerate() {
                                let pct = ((i + 1) * 100 / total) as u32;
                                handle.block_on(client.send_notification::<notification::Progress>(
                                    ProgressParams {
                                        token: token.clone(),
                                        value: ProgressParamsValue::WorkDone(
                                            WorkDoneProgress::Report(WorkDoneProgressReport {
                                                cancellable: Some(false),
                                                message: Some(format!(
                                                    "{} ({}/{})", module_name, i + 1, total
                                                )),
                                                percentage: Some(pct),
                                            }),
                                        ),
                                    },
                                ));

                                log::info!("cpanfile: resolving '{}' ({}/{})", module_name, i + 1, total);
                                let result = parse_module(&inc_paths, module_name);
                                thread_cache.insert(module_name.clone(), result.clone());
                                if let Some(ref conn) = db {
                                    save_to_db(conn, module_name, &result, "cpanfile");
                                }
                                seen.insert(module_name.clone());
                            }

                            // End progress.
                            handle.block_on(client.send_notification::<notification::Progress>(
                                ProgressParams {
                                    token,
                                    value: ProgressParamsValue::WorkDone(
                                        WorkDoneProgress::End(WorkDoneProgressEnd {
                                            message: Some(format!("Indexed {} modules", total)),
                                        }),
                                    ),
                                },
                            ));
                        }
                    }
                }

                loop {
                    let batch = {
                        let mut pending = thread_queue.pending.lock().unwrap();
                        while pending.is_empty() {
                            pending = thread_queue.condvar.wait(pending).unwrap();
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
                                module_name, e.export.len(), e.export_ok.len()
                            ),
                            None => log::info!("No exports found for '{}'", module_name),
                        }
                        thread_cache.insert(module_name.clone(), result.clone());

                        // Persist to SQLite for next session.
                        if let Some(ref conn) = db {
                            save_to_db(conn, &module_name, &result, "import");
                        }

                        // Signal waiters that a module was resolved.
                        let _g = thread_resolved.mu.lock().unwrap();
                        thread_resolved.cv.notify_all();
                    }
                }
            })
            .expect("failed to spawn module-resolver thread");

        ModuleIndex {
            cache,
            queue,
            resolved,
            workspace_root,
        }
    }

    /// Notify the resolver thread of the workspace root (from LSP initialize).
    /// Pass `Some(uri)` for per-project cache, or `None` if client sent no root.
    pub fn set_workspace_root(&self, root: Option<&str>) {
        let mut guard = self.workspace_root.root.lock().unwrap();
        if root.is_none() {
            log::warn!("No workspace root from client; using global module cache");
        }
        *guard = Some(root.map(String::from));
        self.workspace_root.condvar.notify_one();
    }

    /// Request that a module be resolved in the background.
    /// Non-blocking — just enqueues the name. Safe to call from async handlers.
    pub fn request_resolve(&self, module_name: &str) {
        if self.cache.contains_key(module_name) {
            return;
        }
        let mut pending = self.queue.pending.lock().unwrap();
        pending.push(module_name.to_string());
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

    /// Find all cached modules that export the given function name.
    /// Zero I/O — reads DashMap only. Safe from async handlers.
    /// Iterate all cached module exports. Callback receives (module_name, exports).
    pub fn for_each_cached<F: FnMut(&str, &ModuleExports)>(&self, mut f: F) {
        for entry in self.cache.iter() {
            if let Some(ref exports) = *entry.value() {
                f(entry.key(), exports);
            }
        }
    }

    pub fn find_exporters(&self, func_name: &str) -> Vec<String> {
        let mut result = Vec::new();
        for entry in self.cache.iter() {
            if let Some(ref exports) = *entry.value() {
                if exports.export.iter().any(|s| s == func_name)
                    || exports.export_ok.iter().any(|s| s == func_name)
                {
                    result.push(entry.key().clone());
                }
            }
        }
        result.sort();
        result
    }

    /// Create a ModuleIndex for testing — no resolver thread, no Client needed.
    #[cfg(test)]
    pub fn new_for_test() -> Self {
        let cache: Arc<DashMap<String, Option<ModuleExports>>> = Arc::new(DashMap::new());
        let queue = Arc::new(ResolveQueue {
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

        // Spawn a minimal resolver thread (no Client, no progress reporting).
        let thread_cache = Arc::clone(&cache);
        let thread_queue = Arc::clone(&queue);
        let thread_resolved = Arc::clone(&resolved);
        let thread_ws_root = Arc::clone(&workspace_root);
        std::thread::Builder::new()
            .name("module-resolver-test".into())
            .spawn(move || {
                let inc_paths = discover_inc_paths();

                let ws_root = {
                    let mut guard = thread_ws_root.root.lock().unwrap();
                    let deadline = std::time::Instant::now() + Duration::from_secs(10);
                    while guard.is_none() {
                        let remaining =
                            deadline.saturating_duration_since(std::time::Instant::now());
                        if remaining.is_zero() {
                            break;
                        }
                        let (g, _) = thread_ws_root
                            .condvar
                            .wait_timeout(guard, remaining)
                            .unwrap();
                        guard = g;
                    }
                    guard.clone().flatten()
                };

                let db = open_cache_db(ws_root.as_deref());
                if let Some(ref conn) = db {
                    let _ = validate_inc_paths(conn, &inc_paths);
                    let _ = warm_cache(conn, &thread_cache);
                }

                let mut seen = HashSet::new();
                loop {
                    let batch = {
                        let mut pending = thread_queue.pending.lock().unwrap();
                        while pending.is_empty() {
                            pending = thread_queue.condvar.wait(pending).unwrap();
                        }
                        std::mem::take(&mut *pending)
                    };
                    for module_name in batch {
                        if !seen.insert(module_name.clone()) {
                            continue;
                        }
                        let result = parse_module(&inc_paths, &module_name);
                        thread_cache.insert(module_name.clone(), result.clone());
                        if let Some(ref conn) = db {
                            save_to_db(conn, &module_name, &result, "import");
                        }
                        let _g = thread_resolved.mu.lock().unwrap();
                        thread_resolved.cv.notify_all();
                    }
                }
            })
            .expect("failed to spawn test module-resolver thread");

        ModuleIndex {
            cache,
            queue,
            resolved,
            workspace_root,
        }
    }

    /// Insert a module directly into the cache (for testing).
    #[cfg(test)]
    pub fn insert_cache(&self, module_name: &str, exports: Option<ModuleExports>) {
        self.cache.insert(module_name.to_string(), exports);
    }

    /// Block until `module_name` appears in the cache, or timeout.
    /// Returns true if resolved, false on timeout.
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

    /// Get exports for a module, resolving and caching synchronously.
    /// WARNING: Does blocking I/O on cache miss. Only use from tests.
    #[cfg(test)]
    pub fn get_exports(&self, module_name: &str) -> Option<ModuleExports> {
        if let Some(entry) = self.cache.get(module_name) {
            return entry.clone();
        }
        let inc_paths = discover_inc_paths();
        let mut parser = create_parser();
        let result = resolve_and_parse(&inc_paths, module_name, &mut parser);
        self.cache.insert(module_name.to_string(), result.clone());
        result
    }

    #[cfg(test)]
    fn inc_paths(&self) -> Vec<PathBuf> {
        discover_inc_paths()
    }

    #[cfg(test)]
    pub fn resolve_module(&self, module_name: &str) -> Option<PathBuf> {
        let inc_paths = discover_inc_paths();
        resolve_module_path(&inc_paths, module_name)
    }
}

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

/// Resolve a module and parse its exports in an isolated child process.
/// If the child hangs (e.g. external scanner infinite loop), it's SIGKILL'd
/// after PARSE_TIMEOUT — the OS fully cleans up the process.
fn parse_in_subprocess(inc_paths: &[PathBuf], module_name: &str) -> Option<ModuleExports> {
    let path = resolve_module_path(inc_paths, module_name)?;
    let metadata = std::fs::metadata(&path).ok()?;
    let file_size = metadata.len();
    if file_size > 1_000_000 {
        log::warn!("Skipping '{}' — file too large ({} bytes): {:?}", module_name, file_size, path);
        return None;
    }
    log::info!("Subprocess parse '{}' ({} bytes): {:?}", module_name, file_size, path);

    let exe = std::env::current_exe().ok()?;
    let mut child = std::process::Command::new(exe)
        .arg("--parse-exports")
        .arg(&path)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;

    // Read stdout on a helper thread so we can apply a timeout.
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
            // Timeout or read error — kill the child (SIGKILL on Unix).
            let _ = child.kill();
            let _ = child.wait();
            log::warn!("Timed out parsing module '{}'", module_name);
            return None;
        }
    };
    let _ = child.wait(); // reap

    // Parse the JSON output from the child.
    let json: serde_json::Value = serde_json::from_str(&output).ok()?;
    let export: Vec<String> = json
        .get("export")
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
        .unwrap_or_default();
    let export_ok: Vec<String> = json
        .get("export_ok")
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
        .unwrap_or_default();

    if export.is_empty() && export_ok.is_empty() {
        return None;
    }

    Some(ModuleExports { path, export, export_ok })
}

/// Entry point for `--parse-exports <path>` subprocess mode.
/// Reads the .pm file, parses with tree-sitter, prints exports as JSON to stdout.
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

fn create_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_perl::LANGUAGE.into())
        .expect("failed to set Perl language");
    parser
}

// ---- Resolution (runs only on the resolver thread) ----

fn resolve_module_path(inc_paths: &[PathBuf], module_name: &str) -> Option<PathBuf> {
    let rel_path = module_name.replace("::", "/") + ".pm";
    for inc in inc_paths {
        let full = inc.join(&rel_path);
        if full.is_file() {
            return Some(full);
        }
    }
    None
}

fn resolve_and_parse(
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

fn discover_inc_paths() -> Vec<PathBuf> {
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

// ---- Export extraction (tree-sitter) ----

/// Parse source with the given (reusable) parser, then extract @EXPORT / @EXPORT_OK.
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

/// Recursively walk the tree looking for @EXPORT / @EXPORT_OK assignments.
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

/// Check if an assignment_expression is `@EXPORT(_OK) = qw(...)` or `our @EXPORT(_OK) = qw(...)`.
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

    // Right side: qw(...) → quoted_word_list, or a parenthesized list.
    // child_by_field_name("right") can return a paren token instead of the
    // actual expression (known tree-sitter-perl quirk), so scan all children.
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

// ---- SQLite persistence ----

use rusqlite::{params, Connection};
use std::time::SystemTime;

const SCHEMA_VERSION: &str = "2";

fn cache_base_dir() -> Option<PathBuf> {
    if let Ok(xdg) = std::env::var("XDG_CACHE_HOME") {
        if !xdg.is_empty() {
            return Some(PathBuf::from(xdg).join("perl-lsp"));
        }
    }
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home).join(".cache").join("perl-lsp"));
    }
    None
}

fn cache_dir_for_workspace(workspace_root: Option<&str>) -> Option<PathBuf> {
    let base = cache_base_dir()?;
    match workspace_root {
        Some(root) => {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            root.hash(&mut hasher);
            Some(base.join(format!("{:016x}", hasher.finish())))
        }
        None => Some(base),
    }
}

#[cfg(not(test))]
fn open_cache_db(workspace_root: Option<&str>) -> Option<Connection> {
    let dir = cache_dir_for_workspace(workspace_root)?;
    std::fs::create_dir_all(&dir).ok()?;
    let db_path = dir.join("modules.db");
    log::info!("Module cache: {:?}", db_path);

    match Connection::open(&db_path) {
        Ok(conn) => {
            let _ = conn.execute_batch("PRAGMA journal_mode=WAL;");
            match init_schema(&conn) {
                Ok(()) => Some(conn),
                Err(e) => {
                    log::warn!("Cache DB schema init failed: {}. Recreating.", e);
                    drop(conn);
                    let _ = std::fs::remove_file(&db_path);
                    let conn = Connection::open(&db_path).ok()?;
                    let _ = conn.execute_batch("PRAGMA journal_mode=WAL;");
                    init_schema(&conn).ok()?;
                    Some(conn)
                }
            }
        }
        Err(e) => {
            log::warn!("Failed to open cache DB: {}", e);
            None
        }
    }
}

#[cfg(test)]
fn open_cache_db(_workspace_root: Option<&str>) -> Option<Connection> {
    None
}

fn init_schema(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS meta (
            key   TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS modules (
            module_name TEXT PRIMARY KEY,
            path        TEXT NOT NULL,
            mtime_secs  INTEGER NOT NULL,
            file_size   INTEGER NOT NULL,
            export      TEXT NOT NULL,
            export_ok   TEXT NOT NULL,
            source      TEXT NOT NULL DEFAULT 'import'
        );",
    )?;

    let version: Option<String> = conn
        .query_row(
            "SELECT value FROM meta WHERE key = 'schema_version'",
            [],
            |row| row.get(0),
        )
        .ok();

    match version.as_deref() {
        Some(SCHEMA_VERSION) => Ok(()),
        Some(_) => {
            conn.execute_batch("DROP TABLE IF EXISTS modules;")?;
            conn.execute_batch(
                "CREATE TABLE modules (
                    module_name TEXT PRIMARY KEY,
                    path        TEXT NOT NULL,
                    mtime_secs  INTEGER NOT NULL,
                    file_size   INTEGER NOT NULL,
                    export      TEXT NOT NULL,
                    export_ok   TEXT NOT NULL,
                    source      TEXT NOT NULL DEFAULT 'import'
                );",
            )?;
            conn.execute(
                "INSERT OR REPLACE INTO meta (key, value) VALUES ('schema_version', ?1)",
                params![SCHEMA_VERSION],
            )?;
            Ok(())
        }
        None => {
            conn.execute(
                "INSERT INTO meta (key, value) VALUES ('schema_version', ?1)",
                params![SCHEMA_VERSION],
            )?;
            Ok(())
        }
    }
}

fn compute_inc_hash(inc_paths: &[PathBuf]) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    for p in inc_paths {
        p.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

fn validate_inc_paths(conn: &Connection, inc_paths: &[PathBuf]) -> rusqlite::Result<()> {
    let current_hash = compute_inc_hash(inc_paths);
    let stored: Option<String> = conn
        .query_row(
            "SELECT value FROM meta WHERE key = 'inc_hash'",
            [],
            |row| row.get(0),
        )
        .ok();

    if stored.as_deref() != Some(&current_hash) {
        log::info!(
            "@INC changed (was {:?}, now {}), clearing module cache",
            stored,
            current_hash
        );
        conn.execute("DELETE FROM modules", [])?;
        conn.execute(
            "INSERT OR REPLACE INTO meta (key, value) VALUES ('inc_hash', ?1)",
            params![current_hash],
        )?;
    }
    Ok(())
}

fn mtime_as_secs(path: &std::path::Path) -> Option<(i64, i64)> {
    let meta = std::fs::metadata(path).ok()?;
    let mtime = meta.modified().ok()?;
    let secs = mtime.duration_since(SystemTime::UNIX_EPOCH).ok()?.as_secs() as i64;
    let size = meta.len() as i64;
    Some((secs, size))
}

fn warm_cache(conn: &Connection, cache: &DashMap<String, Option<ModuleExports>>) -> usize {
    let mut stmt = match conn.prepare(
        "SELECT module_name, path, mtime_secs, file_size, export, export_ok FROM modules",
    ) {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let rows = match stmt.query_map([], |row| {
        Ok((
            row.get::<_, String>(0)?,
            row.get::<_, String>(1)?,
            row.get::<_, i64>(2)?,
            row.get::<_, i64>(3)?,
            row.get::<_, String>(4)?,
            row.get::<_, String>(5)?,
        ))
    }) {
        Ok(r) => r,
        Err(_) => return 0,
    };

    let mut count = 0usize;
    for row in rows.flatten() {
        let (module_name, path_str, cached_mtime, cached_size, export_json, export_ok_json) = row;

        // Negative sentinel
        if path_str.is_empty() {
            cache.insert(module_name, None);
            count += 1;
            continue;
        }

        let path = PathBuf::from(&path_str);

        // Validate mtime — skip stale entries
        if let Some((disk_mtime, disk_size)) = mtime_as_secs(&path) {
            if disk_mtime != cached_mtime || disk_size != cached_size {
                continue;
            }
        } else {
            continue; // file deleted
        }

        let export: Vec<String> = serde_json::from_str(&export_json).unwrap_or_default();
        let export_ok: Vec<String> = serde_json::from_str(&export_ok_json).unwrap_or_default();

        if export.is_empty() && export_ok.is_empty() {
            cache.insert(module_name, None);
        } else {
            cache.insert(module_name, Some(ModuleExports { path, export, export_ok }));
        }
        count += 1;
    }

    count
}

fn save_to_db(conn: &Connection, module_name: &str, result: &Option<ModuleExports>, source: &str) {
    let (path_str, mtime, size, export_json, export_ok_json) = match result {
        Some(exports) => {
            let (mtime, size) = mtime_as_secs(&exports.path).unwrap_or((0, 0));
            let ej = serde_json::to_string(&exports.export).unwrap_or_default();
            let eoj = serde_json::to_string(&exports.export_ok).unwrap_or_default();
            (exports.path.to_string_lossy().to_string(), mtime, size, ej, eoj)
        }
        None => (String::new(), 0i64, 0i64, "[]".to_string(), "[]".to_string()),
    };

    let r = conn.execute(
        "INSERT OR REPLACE INTO modules (module_name, path, mtime_secs, file_size, export, export_ok, source)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
        params![module_name, path_str, mtime, size, export_json, export_ok_json, source],
    );
    if let Err(e) = r {
        log::warn!("Failed to save module cache for '{}': {}", module_name, e);
    }
}

// ---- URI / path helpers ----

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(PathBuf::from)
}

// ---- cpanfile parsing ----

/// Parse a cpanfile at `{root}/cpanfile` using tree-sitter and extract
/// module names from `requires 'Module::Name'` calls.
fn parse_cpanfile(root_path: &std::path::Path) -> Vec<String> {
    let cpanfile = root_path.join("cpanfile");
    let source = match std::fs::read_to_string(&cpanfile) {
        Ok(s) => s,
        Err(_) => return vec![],
    };

    let mut parser = create_parser();
    let tree = match parser.parse(&source, None) {
        Some(t) => t,
        None => return vec![],
    };

    let mut modules = Vec::new();
    collect_requires(tree.root_node(), source.as_bytes(), &mut modules);
    modules.sort();
    modules.dedup();
    log::info!("cpanfile: found {} requires: {:?}", modules.len(), modules);
    modules
}

/// Use a tree-sitter query to find all `requires 'Module::Name'` calls.
fn collect_requires(root: tree_sitter::Node, source: &[u8], modules: &mut Vec<String>) {
    use tree_sitter::{Query, QueryCursor, StreamingIterator};

    // Match both `requires 'Foo'` (ambiguous) and `requires('Foo')` (explicit).
    // The `#eq?` predicate filters to only calls where the function name is "requires".
    // Single arg:  `requires 'DBI'`  → string_literal is direct arguments: child
    // Multi arg:   `requires 'Mojo', '>= 9.0'` → args wrapped in list_expression
    // Both node types (ambiguous = no parens, function_call = with parens) need both patterns.
    // The `.` anchor on list_expression ensures only the first string (module name) matches.
    let query_src = r#"
        [
          (function_call_expression
            function: (_) @fn
            (string_literal (string_content) @module))
          (function_call_expression
            function: (_) @fn
            (list_expression . (string_literal (string_content) @module)))
          (ambiguous_function_call_expression
            function: (_) @fn
            (string_literal (string_content) @module))
          (ambiguous_function_call_expression
            function: (_) @fn
            (list_expression . (string_literal (string_content) @module)))
        ]
        (#eq? @fn "requires")
    "#;

    let lang = tree_sitter_perl::LANGUAGE.into();
    let query = match Query::new(&lang, query_src) {
        Ok(q) => q,
        Err(e) => {
            log::error!("cpanfile query error: {}", e);
            return;
        }
    };

    let module_idx = match query.capture_index_for_name("module") {
        Some(idx) => idx,
        None => return,
    };

    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&query, root, source);
    while let Some(m) = matches.next() {
        for cap in m.captures {
            if cap.index == module_idx {
                if let Ok(name) = cap.node.utf8_text(source) {
                    modules.push(name.to_string());
                }
            }
        }
    }
}

// ---- Tests ----

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
    fn test_module_resolution_not_found() {
        let idx = ModuleIndex::new_for_test();
        assert!(idx.resolve_module("Nonexistent::Module::XYZ123").is_none());
    }

    #[test]
    fn test_discover_inc_paths() {
        let paths = discover_inc_paths();
        if !paths.is_empty() {
            assert!(paths.iter().all(|p| p.is_dir()));
        }
    }

    #[test]
    fn test_resolver_thread_flow() {
        let idx = ModuleIndex::new_for_test();
        // Unblock the resolver thread (no workspace root = global cache).
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

    // ---- SQLite persistence tests (in-memory DB) ----

    fn test_db() -> Connection {
        let conn = Connection::open_in_memory().unwrap();
        init_schema(&conn).unwrap();
        conn
    }

    #[test]
    fn test_db_save_and_load_roundtrip() {
        let conn = test_db();

        // Create a real temp file so mtime validation works.
        let dir = std::env::temp_dir();
        let pm = dir.join("TestModule.pm");
        std::fs::write(&pm, "package TestModule; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into(), "bar".into()],
            export_ok: vec!["baz".into()],
        });
        save_to_db(&conn, "TestModule", &exports, "import");

        // Load into a fresh DashMap.
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let loaded = cache.get("TestModule").unwrap();
        let loaded = loaded.as_ref().unwrap();
        assert_eq!(loaded.export, vec!["foo", "bar"]);
        assert_eq!(loaded.export_ok, vec!["baz"]);
        assert_eq!(loaded.path, pm);

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_db_negative_result_roundtrip() {
        let conn = test_db();
        save_to_db(&conn, "Nonexistent::Module", &None, "import");

        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let entry = cache.get("Nonexistent::Module").unwrap();
        assert!(entry.is_none());
    }

    #[test]
    fn test_db_stale_entry_skipped() {
        let conn = test_db();

        // Create a file, save to DB, then modify it.
        let dir = std::env::temp_dir();
        let pm = dir.join("StaleModule.pm");
        std::fs::write(&pm, "v1").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["old".into()],
            export_ok: vec![],
        });
        save_to_db(&conn, "StaleModule", &exports, "import");

        // Modify the file so mtime/size changes.
        std::thread::sleep(std::time::Duration::from_secs(1));
        std::fs::write(&pm, "v2 with more content").unwrap();

        // Warm should skip the stale entry.
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "stale entry should not be loaded");
        assert!(!cache.contains_key("StaleModule"));

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_db_inc_hash_invalidation() {
        let conn = test_db();
        let paths1 = vec![PathBuf::from("/usr/lib/perl5")];
        let paths2 = vec![
            PathBuf::from("/usr/lib/perl5"),
            PathBuf::from("/home/user/lib"),
        ];

        validate_inc_paths(&conn, &paths1).unwrap();
        save_to_db(&conn, "Foo", &None, "import");

        // Changing @INC should clear the table.
        validate_inc_paths(&conn, &paths2).unwrap();
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "cache should be empty after @INC change");
    }

    #[test]
    fn test_db_schema_version_migration() {
        let conn = test_db();

        // Simulate an old schema version.
        conn.execute(
            "UPDATE meta SET value = '0' WHERE key = 'schema_version'",
            [],
        )
        .unwrap();
        save_to_db(&conn, "OldModule", &None, "import");

        // Re-init should drop and recreate.
        init_schema(&conn).unwrap();
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "old data should be gone after migration");
    }

    #[test]
    fn test_find_exporters() {
        let idx = ModuleIndex::new_for_test();
        idx.cache.insert(
            "Foo::Bar".to_string(),
            Some(ModuleExports {
                path: PathBuf::from("/fake/Foo/Bar.pm"),
                export: vec!["alpha".into()],
                export_ok: vec!["beta".into()],
            }),
        );
        idx.cache.insert(
            "Baz::Qux".to_string(),
            Some(ModuleExports {
                path: PathBuf::from("/fake/Baz/Qux.pm"),
                export: vec![],
                export_ok: vec!["beta".into(), "gamma".into()],
            }),
        );

        assert_eq!(idx.find_exporters("alpha"), vec!["Foo::Bar"]);
        assert_eq!(idx.find_exporters("beta"), vec!["Baz::Qux", "Foo::Bar"]);
        assert!(idx.find_exporters("nonexistent").is_empty());
    }

    #[test]
    fn test_parse_cpanfile_basic() {
        let dir = std::env::temp_dir().join("perl_lsp_test_cpanfile");
        let _ = std::fs::create_dir_all(&dir);
        let cpanfile = dir.join("cpanfile");
        std::fs::write(
            &cpanfile,
            r#"requires 'Mojolicious', '>= 9.0';
requires 'DBI';
requires 'JSON::XS';

on test => sub {
    requires 'Test::More';
    requires 'Test::Deep';
};
"#,
        )
        .unwrap();

        let modules = parse_cpanfile(&dir);
        assert!(modules.contains(&"Mojolicious".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"DBI".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"JSON::XS".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"Test::More".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"Test::Deep".to_string()), "got: {:?}", modules);
        assert_eq!(modules.len(), 5);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_parse_cpanfile_missing() {
        let dir = std::env::temp_dir().join("perl_lsp_test_no_cpanfile");
        let _ = std::fs::create_dir_all(&dir);
        let _ = std::fs::remove_file(dir.join("cpanfile")); // ensure it doesn't exist

        let modules = parse_cpanfile(&dir);
        assert!(modules.is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_uri_to_path() {
        assert_eq!(
            uri_to_path("file:///Users/foo/project"),
            Some(PathBuf::from("/Users/foo/project"))
        );
        assert_eq!(uri_to_path("http://example.com"), None);
    }

    #[test]
    fn test_db_source_column() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("SourceTest.pm");
        std::fs::write(&pm, "package SourceTest; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into()],
            export_ok: vec![],
        });
        save_to_db(&conn, "SourceTest", &exports, "cpanfile");

        // Verify source column was saved.
        let source: String = conn
            .query_row(
                "SELECT source FROM modules WHERE module_name = 'SourceTest'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(source, "cpanfile");

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_workspace_cache_dir_uniqueness() {
        let d1 = cache_dir_for_workspace(Some("file:///home/user/project-a"));
        let d2 = cache_dir_for_workspace(Some("file:///home/user/project-b"));
        let d_none = cache_dir_for_workspace(None);
        assert_ne!(d1, d2, "Different roots should produce different paths");
        assert_ne!(d1, d_none, "Root vs no-root should differ");
        assert_eq!(
            d1,
            cache_dir_for_workspace(Some("file:///home/user/project-a")),
            "Same root should produce same path"
        );
    }
}
