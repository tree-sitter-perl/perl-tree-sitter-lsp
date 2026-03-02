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
}

impl ModuleIndex {
    pub fn new() -> Self {
        let cache: Arc<DashMap<String, Option<ModuleExports>>> = Arc::new(DashMap::new());
        let queue = Arc::new(ResolveQueue {
            pending: Mutex::new(Vec::new()),
            condvar: Condvar::new(),
        });
        let resolved = Arc::new(ResolveNotify {
            mu: Mutex::new(()),
            cv: Condvar::new(),
        });

        // Coordinator thread — dispatches each parse to an isolated child
        // process. If tree-sitter's external scanner hangs, the child is
        // SIGKILL'd after PARSE_TIMEOUT — no leaked spinning threads.
        let thread_cache = Arc::clone(&cache);
        let thread_queue = Arc::clone(&queue);
        let thread_resolved = Arc::clone(&resolved);
        std::thread::Builder::new()
            .name("module-resolver".into())
            .spawn(move || {
                let inc_paths = discover_inc_paths();
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

                        log::info!("Resolving module '{}'", module_name);
                        let result = parse_module(&inc_paths, &module_name);
                        match &result {
                            Some(e) => log::info!(
                                "Resolved '{}': {} export, {} export_ok",
                                module_name, e.export.len(), e.export_ok.len()
                            ),
                            None => log::info!("No exports found for '{}'", module_name),
                        }
                        thread_cache.insert(module_name, result);

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
        }
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

// ---- Tests ----

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_module_list_util() {
        let idx = ModuleIndex::new();
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
        let idx = ModuleIndex::new();
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
        let idx = ModuleIndex::new();
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
        let idx = ModuleIndex::new();
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
}
