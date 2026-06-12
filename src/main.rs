mod backend;
mod builder;
mod builtins_pod;
mod conventions;
mod cpanfile;
mod cst;
mod cursor_context;
mod document;
mod file_analysis;
mod file_store;
mod graph;
mod module_cache;
mod module_index;
mod module_resolver;
mod plugin;
mod plugin_cli;
mod pod;
mod query_cache;
mod resolve;
mod symbols;
mod timings;
mod witnesses;

#[cfg(test)]
#[path = "layering_tests.rs"]
mod layering_tests;

use backend::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.iter().any(|a| a == "--help" || a == "-h") {
        print_usage();
        return;
    }

    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("perl-lsp {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    // CLI modes — dispatch before starting LSP server
    match args.get(1).map(|s| s.as_str()) {
        Some("--check") => {
            cli_check(&args[2..]);
            return;
        }
        Some("--outline") if args.len() >= 3 => {
            cli_outline(&args[2]);
            return;
        }
        // `--hover <root> <file> <line> <col>` enables cross-file hover (same
        // setup as the server); the legacy `--hover <file> <line> <col>` form
        // stays single-file. Disambiguated by arg count.
        Some("--hover") if args.len() >= 6 => {
            cli_hover(Some(&args[2]), &args[3], &args[4], &args[5]);
            return;
        }
        Some("--hover") if args.len() == 5 => {
            cli_hover(None, &args[2], &args[3], &args[4]);
            return;
        }
        Some("--type-at") if args.len() >= 5 => {
            cli_type_at(&args[2], &args[3], &args[4]);
            return;
        }
        Some("--definition") if args.len() >= 6 => {
            cli_definition(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--references") if args.len() >= 6 => {
            cli_references(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--implementations") if args.len() >= 6 => {
            cli_implementations(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--completion") if args.len() >= 6 => {
            cli_completion(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--signature-help") if args.len() >= 6 => {
            cli_signature_help(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--semantic-tokens") if args.len() >= 4 => {
            cli_semantic_tokens(&args[2], &args[3]);
            return;
        }
        Some("--document-highlight") if args.len() >= 6 => {
            cli_document_highlight(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--linked-editing") if args.len() >= 6 => {
            cli_linked_editing(&args[2], &args[3], &args[4], &args[5]);
            return;
        }
        Some("--rename") if args.len() == 7 => {
            cli_rename(&args[2], &args[3], &args[4], &args[5], &args[6]);
            return;
        }
        Some("--workspace-symbol") if args.len() >= 4 => {
            cli_workspace_symbol(&args[2], &args[3]);
            return;
        }
        Some("--batch") if args.len() >= 3 => {
            cli_batch(&args[2]);
            return;
        }
        Some("--plugin-check") => {
            plugin_cli::cli_plugin_check(&args[2..]);
            return;
        }
        Some("--plugin-run") => {
            plugin_cli::cli_plugin_run(&args[2..]);
            return;
        }
        Some("--plugin-test") => {
            plugin_cli::cli_plugin_test(&args[2..]);
            return;
        }
        Some("--dump-package") if args.len() >= 4 => {
            cli_dump_package(&args[2], &args[3]);
            return;
        }
        Some("--clear-cache") => {
            cli_clear_cache(args.get(2).map(|s| s.as_str()));
            return;
        }
        Some("--parse") if args.len() >= 3 => {
            cli_parse(&args[2]);
            return;
        }
        _ => {}
    }

    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn print_usage() {
    eprintln!("perl-lsp {} — Perl Language Server", env!("CARGO_PKG_VERSION"));
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  perl-lsp                                              Start LSP server (stdio)");
    eprintln!();
    eprintln!("  Positions: <line> <col> input is 0-based (col = byte offset);");
    eprintln!("             printed line:col output is 1-based (col = character).");
    eprintln!();
    eprintln!("ANALYSIS:");
    eprintln!("  perl-lsp --check [<root>] [--severity error|warning]    Batch diagnostics (CI)");
    eprintln!("                           [--format json|human]");
    eprintln!("                           [--timings]                    Per-module build-timing report (stderr, slowest-first)");
    eprintln!("  perl-lsp --outline <file>                              Document symbol outline");
    eprintln!("  perl-lsp --hover [<root>] <file> <line> <col>         Type info and docs (root = cross-file)");
    eprintln!("  perl-lsp --type-at <file> <line> <col>                 Single type query");
    eprintln!("  perl-lsp --definition <root> <file> <line> <col>       Cross-file goto-def");
    eprintln!("  perl-lsp --implementations <root> <file> <line> <col>  Descendant defs (role composers, overrides)");
    eprintln!("  perl-lsp --references <root> <file> <line> <col>       Cross-file find-refs");
    eprintln!("  perl-lsp --completion <root> <file> <line> <col>       Completion items at point");
    eprintln!("  perl-lsp --signature-help <root> <file> <line> <col>   Signature help at point");
    eprintln!("  perl-lsp --document-highlight <root> <file> <line> <col> In-file occurrences (read/write)");
    eprintln!("  perl-lsp --linked-editing <root> <file> <line> <col>   Linked-editing occurrence set");
    eprintln!("  perl-lsp --semantic-tokens <root> <file>               Semantic token classification");
    eprintln!();
    eprintln!("REFACTORING:");
    eprintln!("  perl-lsp --rename <root> <file> <line> <col> <new>     Cross-file rename");
    eprintln!("  perl-lsp --workspace-symbol <root> <query>             Search symbols");
    eprintln!("  perl-lsp --batch <root>                                Stream JSONL queries (one startup, many)");
    eprintln!();
    eprintln!("PLUGIN AUTHORING:");
    eprintln!("  perl-lsp --plugin-check <file.rhai>                    Lint a Rhai plugin");
    eprintln!("  perl-lsp --plugin-run <file.rhai> --on <fixture.pl>    Run plugin on one Perl file");
    eprintln!("  perl-lsp --plugin-test <plugin-dir> [--update]         Snapshot-test a plugin dir");
    eprintln!();
    eprintln!("DEBUG:");
    eprintln!("  perl-lsp --dump-package <root> <package>               Dump every sub in <package>");
    eprintln!("                                                         with derived type info");
    eprintln!("  perl-lsp --clear-cache [<root>]                        Wipe the module cache for");
    eprintln!("                                                         <root>, or every project if");
    eprintln!("                                                         <root> is omitted");
    eprintln!("  perl-lsp --parse <file|-->                             Print tree-sitter parse tree");
    eprintln!("                                                         (`-` reads from stdin)");
    eprintln!();
    eprintln!("  perl-lsp --version                                     Print version");
}

// ---- Helpers ----

fn parse_file(path: &str) -> (String, tree_sitter::Tree, file_analysis::FileAnalysis) {
    let source = std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Cannot read {}: {}", path, e);
        std::process::exit(1);
    });
    let mut parser = module_resolver::create_parser();
    let tree = parser.parse(&source, None).unwrap_or_else(|| {
        eprintln!("Parse failed: {}", path);
        std::process::exit(1);
    });
    let analysis = builder::build(&tree, source.as_bytes());
    (source, tree, analysis)
}

fn parse_point(line_str: &str, col_str: &str) -> tree_sitter::Point {
    let line: usize = line_str.parse().unwrap_or_else(|_| {
        eprintln!("line must be a number");
        std::process::exit(1);
    });
    let col: usize = col_str.parse().unwrap_or_else(|_| {
        eprintln!("col must be a number");
        std::process::exit(1);
    });
    tree_sitter::Point::new(line, col)
}

/// Canonicalize `root` and produce the matching `file://...` URI.
/// Returns both because callers usually want the path (for `@INC`
/// project-lib discovery, file walking) AND the URI (the cache hash
/// key, since the LSP server's `cache_dir_for_workspace` is fed the
/// initialize request's `root_uri` string verbatim — `file://...`).
/// Falls back to the raw input if `canonicalize` fails (path doesn't
/// exist, permissions): same string lands in both halves so the
/// caller's downstream "does this dir exist?" check still fires.
fn canonical_root_and_uri(root: &str) -> (std::path::PathBuf, String) {
    let path = std::path::Path::new(root)
        .canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(root));
    let uri = format!("file://{}", path.display());
    (path, uri)
}

/// Full CLI workspace setup: index the workspace, open the SQLite cache,
/// warm cached modules, resolve missing imports + ancestors via @INC,
/// save fresh entries back to disk. Mirrors the LSP server's startup
/// minus the resolver thread (one-shot, synchronous). Used by every
/// CLI command that needs cross-file resolution to behave like the
/// running server.
fn cli_full_startup(root: &str) -> (file_store::FileStore, module_index::ModuleIndex) {
    // `--check --timings` already flipped this on; the env var lets any
    // startup-driven CLI mode (dump-package, etc.) opt in too.
    timings::enable_from_env();
    let (root_path, root_uri) = canonical_root_and_uri(root);
    // Pin repo-local `.perl-lsp/` plugin discovery to the same root the
    // cache keys on, before the first build() (workspace indexing) fires.
    plugin::rhai_host::set_workspace_root(Some(&root_uri));

    // Register workspace files INTO the module index (not just a FileStore)
    // so their plugin bridges + package names participate in cross-file
    // lookups — the whole point of "act like the server just started".
    // Indexing without the index (bridge-less) is what forced callers to
    // hand-roll their own `index_workspace_with_index`, and they drifted.
    let module_index = module_index::ModuleIndex::new_for_cli();
    // Wake the headless resolver: it blocks on this channel for the
    // @INC scan + SQLite warm.
    module_index.set_workspace_root(Some(root_uri.as_str()));
    let ws = file_store::FileStore::new();
    let indexed = module_resolver::index_workspace_with_index(&root_path, &ws, Some(&module_index));
    eprintln!("Indexed {} files", indexed);

    let mut inc_paths = module_resolver::discover_inc_paths();
    module_resolver::add_project_lib_paths(&mut inc_paths, &root_path);

    let db = module_cache::open_cache_db(Some(&root_uri));
    let mut stale_set: std::collections::HashSet<String> = std::collections::HashSet::new();
    if let Some(ref conn) = db {
        let _ = module_cache::validate_inc_paths(conn, &inc_paths);
        let _ = module_cache::validate_plugin_fingerprint(
            conn,
            &plugin::rhai_host::plugin_fingerprint(),
        );
        let (warmed, stale) = module_cache::warm_cache(conn, &module_index.cache_raw());
        if warmed > 0 {
            eprintln!("Cache: {} modules loaded from disk", warmed);
        }
        stale_set = stale.into_iter().collect();
    }

    let mut needed = std::collections::HashSet::new();
    for entry in ws.workspace_raw().iter() {
        for imp in &entry.value().imports {
            needed.insert(imp.module_name.clone());
        }
        for parents in entry.value().package_parents.values() {
            for p in parents {
                needed.insert(p.clone());
            }
        }
    }

    let mut parser = module_resolver::create_parser();
    let mut parse_memo: module_resolver::ParseMemo = std::collections::HashMap::new();
    let mut resolved = 0usize;
    let mut already_cached = 0usize;
    // Worklist, not a fixed set: re-exporting modules (Test::Most → Test::More)
    // pull their re-exported producers' surfaces transitively, so those
    // producers must be resolved too even though no workspace file `use`s them
    // directly. Enqueue each resolved module's `reexport_modules`. Bounded by
    // the seen-set (`queued`); the cross-file surface walk handles cycles.
    let mut queue: std::collections::VecDeque<String> = needed.iter().cloned().collect();
    let mut queued: std::collections::HashSet<String> = needed.clone();
    while let Some(name) = queue.pop_front() {
        let cached_arc = if module_index.cache_raw().contains_key(name.as_str())
            && !stale_set.contains(&name)
        {
            already_cached += 1;
            timings::record_cached(&name);
            module_index.get_cached(&name)
        } else {
            if stale_set.contains(&name) {
                parse_memo.remove(&name);
            }
            module_resolver::resolve_and_parse_with_memo(&inc_paths, &name, &mut parser, &mut parse_memo)
                .map(|cached| {
                    if let Some(ref conn) = db {
                        module_cache::save_to_db(conn, &name, &Some(std::sync::Arc::clone(&cached)), "cli");
                    }
                    module_index.insert_cache(&name, Some(std::sync::Arc::clone(&cached)));
                    resolved += 1;
                    cached
                })
        };
        if let Some(cached) = cached_arc {
            for re in &cached.analysis.reexport_modules {
                if queued.insert(re.clone()) {
                    queue.push_back(re.clone());
                }
            }
        }
    }
    eprintln!("Modules: {} cached, {} resolved, {} total", already_cached, resolved, queued.len());

    // `warm_cache` populated `cache_raw()` directly, bypassing the reverse
    // index, and `insert_cache` only indexes export/export_ok. Rebuild the
    // full `func → modules` index from the cache so `find_exporters` answers
    // identically on cold and warm runs (B6 export-attribution regression).
    module_index.rebuild_reverse_index_from_cache();

    (ws, module_index)
}

fn is_json_format(args: &[String]) -> bool {
    args.windows(2).any(|w| w[0] == "--format" && w[1] == "json")
}

fn get_arg_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].as_str())
}

fn severity_rank(s: &str) -> u8 {
    match s {
        "error" => 0,
        "warning" => 1,
        "info" => 2,
        "hint" => 3,
        _ => 1,
    }
}

fn span_to_json(span: file_analysis::Span, text: String) -> serde_json::Value {
    serde_json::json!({
        "line": span.start.row, "col": span.start.column,
        "end_line": span.end.row, "end_col": span.end.column,
        "new_text": text
    })
}

/// One-based, character-counted `(line, col)` for human/editor-facing CLI
/// output. Tree-sitter `Point.column` is a 0-based **byte** offset within the
/// line; an editor shows a 1-based **character** column. Without the byte→char
/// step a line with multi-byte UTF-8 before the token reports a column past the
/// visible position (the "phantom column" QA saw on unicode lines). `source` is
/// the file's full text; rows past its end fall back to byte+1 (best effort).
fn display_line_col(source: &str, row: usize, byte_col: usize) -> (usize, usize) {
    let char_col = source
        .lines()
        .nth(row)
        .map(|line| {
            let upto = line.get(..byte_col.min(line.len())).unwrap_or(line);
            upto.chars().count()
        })
        .unwrap_or(byte_col);
    (row + 1, char_col + 1)
}

/// Per-file source cache for `display_line_col` — references can fan out across
/// many files; read each at most once. Misses (unreadable file) degrade to the
/// raw byte column via `display_line_col`'s fallback.
struct SourceCache(std::collections::HashMap<String, Option<String>>);

impl SourceCache {
    fn new() -> Self {
        SourceCache(std::collections::HashMap::new())
    }

    fn display(&mut self, path: &str, row: usize, byte_col: usize) -> (usize, usize) {
        let src = self
            .0
            .entry(path.to_string())
            .or_insert_with(|| std::fs::read_to_string(path).ok());
        match src {
            Some(s) => display_line_col(s, row, byte_col),
            None => (row + 1, byte_col + 1),
        }
    }
}

// ---- CLI Commands ----

/// --check [<root>] [--format json|human] [--severity error|warning|info|hint] — Batch diagnostics
fn cli_check(args: &[String]) {
    let root = args.iter()
        .find(|a| !a.starts_with("--") && !["json", "human", "error", "warning", "info", "hint"].contains(&a.as_str()))
        .map(|s| s.as_str())
        .unwrap_or(".");
    let json_mode = is_json_format(args);
    let min_severity = get_arg_value(args, "--severity").unwrap_or("warning");
    let min_rank = severity_rank(min_severity);
    // Opt-in QA channel, mirrors the LSP `initializationOptions` toggle.
    let options = symbols::DiagnosticOptions {
        unresolved_dispatch: args.iter().any(|a| a == "--unresolved-dispatch"),
    };

    if args.iter().any(|a| a == "--timings") {
        timings::enable();
    }
    timings::enable_from_env();

    let (ws, module_index) = cli_full_startup(root);

    // Dump the per-module breakdown before diagnostics output so the table
    // isn't buried under (and the early `exit(1)` below doesn't swallow it).
    timings::report();

    let mut all_diagnostics = Vec::new();

    for (file, d) in enriched_tree_diagnostics(&ws, &module_index, options) {
        {
            let sev = match d.severity {
                Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::ERROR => "error",
                Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::WARNING => "warning",
                Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION => "info",
                Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::HINT => "hint",
                _ => "warning",
            };
            // Filter by minimum severity
            if severity_rank(sev) > min_rank {
                continue;
            }
            all_diagnostics.push(serde_json::json!({
                "file": file,
                "line": d.range.start.line,
                "col": d.range.start.character,
                "severity": sev,
                "code": d.code.map(|c| match c {
                    tower_lsp::lsp_types::NumberOrString::String(s) => s,
                    tower_lsp::lsp_types::NumberOrString::Number(n) => n.to_string(),
                }).unwrap_or_default(),
                "message": d.message,
            }));
        }
    }

    if json_mode {
        println!("{}", serde_json::to_string_pretty(&all_diagnostics).unwrap());
    } else {
        for d in &all_diagnostics {
            let severity = d["severity"].as_str().unwrap_or("warning");
            let file = d["file"].as_str().unwrap_or("");
            let line = d["line"].as_u64().unwrap_or(0) + 1;
            let col = d["col"].as_u64().unwrap_or(0) + 1;
            let code = d["code"].as_str().unwrap_or("");
            let msg = d["message"].as_str().unwrap_or("");
            eprintln!("{}:{}:{}: {}[{}] {}", file, line, col, severity, code, msg);
        }
        let total = all_diagnostics.len();
        let files = ws.workspace_len();
        eprintln!("{} diagnostics in {} files", total, files);
    }

    if !all_diagnostics.is_empty() {
        std::process::exit(1);
    }
}

/// --outline <file> — Document symbol outline
fn cli_outline(file: &str) {
    let (_source, _tree, analysis) = parse_file(file);
    println!("{}", outline_json(&analysis));
}

/// --hover [<root>] <file> <line> <col> — Type info and docs.
/// With `root`, runs full startup so hover resolves cross-file (imported
/// types, inherited + plugin-bridged methods); without it, single-file.
fn cli_hover(root: Option<&str>, file: &str, line_str: &str, col_str: &str) {
    let point = parse_point(line_str, col_str);
    // Root form is cross-file: delegate to the single `run_one` path so it can
    // never drift from `--batch`. The no-root single-file form has no index, so
    // it keeps its own path (run_one always carries an idx).
    match root {
        Some(r) => {
            let (ws, idx) = cli_full_startup(r);
            let req = BatchReq {
                id: String::new(), q: "hover".into(),
                file: file.to_string(), line: point.row, col: point.column,
                query: None, newname: None,
            };
            print_run_one(&ws, &idx, &req);
        }
        None => {
            let (source, _tree, analysis) = parse_file(file);
            if let Some(markdown) = analysis.hover_info(point, &source, None) {
                println!("{}", markdown);
            } else {
                eprintln!("No hover info at {}:{}", line_str, col_str);
                std::process::exit(1);
            }
        }
    }
}

/// --type-at <file> <line> <col> — Single type query
fn cli_type_at(file: &str, line_str: &str, col_str: &str) {
    let (_source, _tree, analysis) = parse_file(file);
    let point = parse_point(line_str, col_str);

    // Check refs for inferred type — route through the witness bag
    // so framework / branch / arity rules refine the answer.
    if let Some(r) = analysis.ref_at(point) {
        if let Some(ty) = analysis.inferred_type_via_bag(&r.target_name, point) {
            println!("{}", file_analysis::format_inferred_type(&ty));
            return;
        }
    }
    // Check symbols
    if let Some(sym) = analysis.symbol_at(point) {
        if let Some(ty) = analysis.inferred_type_via_bag(&sym.name, point) {
            println!("{}", file_analysis::format_inferred_type(&ty));
            return;
        }
    }
    eprintln!("No type info at {}:{}", line_str, col_str);
    std::process::exit(1);
}

/// Build a cursor `BatchReq` for the single-mode wrappers that share `run_one`.
fn cursor_req(q: &str, file: &str, line_str: &str, col_str: &str) -> BatchReq {
    let point = parse_point(line_str, col_str);
    BatchReq {
        id: String::new(), q: q.to_string(),
        file: file.to_string(), line: point.row, col: point.column,
        query: None, newname: None,
    }
}

/// Run `run_one` and reproduce the single-mode contract: `Ok` to stdout,
/// `Err` to stderr with exit-1 (preserves the "miss → exit 1" behavior every
/// single-mode command had).
fn print_run_one(ws: &file_store::FileStore, idx: &module_index::ModuleIndex, req: &BatchReq) {
    match run_one(ws, idx, req) {
        Ok(s) => println!("{}", s),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

/// --definition <root> <file> <line> <col> — Cross-file goto-def
fn cli_definition(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("definition", file, line_str, col_str));
}

/// --references <root> <file> <line> <col> — Cross-file find-refs
fn cli_references(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("references", file, line_str, col_str));
}

/// --implementations <root> <file> <line> <col> — descendant defs of a
/// method target (role-requires composers, subclass overrides).
fn cli_implementations(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("implementations", file, line_str, col_str));
}

/// --completion <root> <file> <line> <col> — completion items at point.
fn cli_completion(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("completion", file, line_str, col_str));
}

/// --signature-help <root> <file> <line> <col> — signature help at point.
fn cli_signature_help(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("signature-help", file, line_str, col_str));
}

/// --document-highlight <root> <file> <line> <col> — in-file occurrences with
/// read/write classification.
fn cli_document_highlight(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("document-highlight", file, line_str, col_str));
}

/// --linked-editing <root> <file> <line> <col> — the co-edit occurrence set.
fn cli_linked_editing(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("linked-editing", file, line_str, col_str));
}

/// --semantic-tokens <root> <file> — token classification for the file.
fn cli_semantic_tokens(root: &str, file: &str) {
    let (ws, idx) = cli_full_startup(root);
    print_run_one(&ws, &idx, &cursor_req("semantic-tokens", file, "1", "0"));
}

/// Build an open `Document` (tree + analysis + stable_outline) and enrich it
/// against the index exactly like the server's open-file path. Shared by the
/// interactive CLI modes.
fn cli_open_document(file: &str, idx: &module_index::ModuleIndex) -> document::Document {
    let text = std::fs::read_to_string(file).unwrap_or_else(|e| {
        eprintln!("Cannot read {}: {}", file, e);
        std::process::exit(1);
    });
    let mut doc = document::Document::new(text).unwrap_or_else(|| {
        eprintln!("Parse failed: {}", file);
        std::process::exit(1);
    });
    doc.analysis.enrich_imported_types_with_keys(Some(idx));
    doc
}

#[derive(serde::Deserialize)]
struct BatchReq {
    id: String,
    q: String,
    #[serde(default)] file: String,
    #[serde(default)] line: usize,
    #[serde(default)] col: usize,
    #[serde(default)] query: Option<String>,
    #[serde(default)] newname: Option<String>,
}

/// Single source of truth for every cursor/query CLI capability. Both the
/// single-mode `cli_*` wrappers and `--batch` call this, so their stdout can
/// never drift. The expensive startup state (`ws` + `idx`) is built once by the
/// caller and shared; this re-parses only the one target file per call. Returns
/// Stage an analysis in the workspace store for the lifetime of one cross-file
/// query (references/rename need `refs_to` to see the freshly-enriched target),
/// restoring the prior entry on drop. `--batch` shares one FileStore across all
/// requests; without this restore a references/rename request would leave its
/// enrichment-synthesized symbols in the store, so a later workspace-symbol or
/// diagnostics request in the same batch would see different results depending
/// on ordering.
struct ScopedWorkspaceEntry<'a> {
    ws: &'a file_store::FileStore,
    path: std::path::PathBuf,
    prior: Option<std::sync::Arc<file_analysis::FileAnalysis>>,
}

impl<'a> ScopedWorkspaceEntry<'a> {
    fn insert(
        ws: &'a file_store::FileStore,
        path: std::path::PathBuf,
        analysis: file_analysis::FileAnalysis,
    ) -> Self {
        let prior = ws.workspace_raw().get(&path).map(|r| r.value().clone());
        ws.insert_workspace(path.clone(), analysis);
        Self { ws, path, prior }
    }
}

impl Drop for ScopedWorkspaceEntry<'_> {
    fn drop(&mut self) {
        match self.prior.take() {
            Some(arc) => self.ws.insert_workspace_arc(self.path.clone(), arc),
            None => self.ws.remove_workspace(&self.path),
        }
    }
}

/// Resolve the query file's direct imports before answering. The LSP
/// backend does this on didOpen (request_resolve per import); a CLI
/// query is one-shot, so it requests AND waits, bounded per module.
/// Without it, modules nothing in the workspace literally `use`s —
/// framework-implied SyntheticUses like Mojolicious::Plugin::
/// DefaultHelpers — resolve in the editor but stay invisible to CLI
/// probes and the gold harness.
fn resolve_imports_blocking(
    idx: &module_index::ModuleIndex,
    analysis: &file_analysis::FileAnalysis,
) {
    for imp in &analysis.imports {
        idx.request_resolve(&imp.module_name);
    }
    // Global budget, not per-import: a cold cache resolving a
    // framework's whole dependency tree must not stall the CLI for
    // minutes. Each resolved module persists to the SQLite cache, so
    // repeated runs converge to fully warm; within one run we answer
    // with whatever resolved in time (the editor is eventually-
    // consistent here too — it refreshes when resolution lands).
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    for imp in &analysis.imports {
        let left = deadline.saturating_duration_since(std::time::Instant::now());
        if left.is_zero() {
            break;
        }
        let _ = idx.wait_resolved(&imp.module_name, left);
    }
}

fn run_one(
    ws: &file_store::FileStore,
    idx: &module_index::ModuleIndex,
    req: &BatchReq,
) -> Result<String, String> {
    use tower_lsp::lsp_types::Position;
    let file = req.file.as_str();
    let point = tree_sitter::Point { row: req.line, column: req.col };
    let pos = Position { line: req.line as u32, character: req.col as u32 };

    // Graceful miss instead of process exit — a bad path must not kill --batch.
    let needs_file = !matches!(req.q.as_str(), "workspace-symbol" | "diagnostics");
    if needs_file && (file.is_empty() || !std::path::Path::new(file).exists()) {
        return Err(format!("file not found: {}", file));
    }

    match req.q.as_str() {
        "definition" => {
            let (_source, _tree, mut analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            analysis.enrich_imported_types_with_keys(Some(idx));
            let abs = std::fs::canonicalize(file).unwrap_or_else(|_| std::path::PathBuf::from(file));
            let uri = tower_lsp::lsp_types::Url::from_file_path(&abs)
                .unwrap_or_else(|_| tower_lsp::lsp_types::Url::parse("file:///unknown").unwrap());
            if let Some(resp) = symbols::find_definition(&analysis, pos, &uri, idx) {
                use tower_lsp::lsp_types::GotoDefinitionResponse;
                let first = match resp {
                    GotoDefinitionResponse::Scalar(loc) => Some(loc),
                    GotoDefinitionResponse::Array(v) => v.into_iter().next(),
                    GotoDefinitionResponse::Link(v) => v.into_iter().next().map(|l| {
                        tower_lsp::lsp_types::Location { uri: l.target_uri, range: l.target_range }
                    }),
                };
                if let Some(loc) = first {
                    let path = loc.uri.to_file_path().map(|p| p.display().to_string())
                        .unwrap_or_else(|_| loc.uri.to_string());
                    let (line, col) = SourceCache::new().display(
                        &path, loc.range.start.line as usize, loc.range.start.character as usize);
                    return Ok(format!("{}:{}:{}", path, line, col));
                }
            }
            Err(format!("No definition found at {}:{}", req.line, req.col))
        }
        "references" => {
            let (_s, _t, mut analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            analysis.enrich_imported_types_with_keys(Some(idx));
            let file_path = std::path::Path::new(file).canonicalize()
                .unwrap_or_else(|_| std::path::PathBuf::from(file));
            let resolved = resolve::resolve_symbol(&analysis, point, Some(idx));
            let mut sources = SourceCache::new();
            let mut results = Vec::new();
            match resolved {
                Some(resolve::ResolvedTarget::Local) | None => {
                    let path_str = file_path.display().to_string();
                    for span in &analysis.find_references(point, Some(idx)) {
                        let (line, col) = sources.display(&path_str, span.start.row, span.start.column);
                        results.push(serde_json::json!({"file": path_str, "line": line, "col": col}));
                    }
                }
                Some(resolved) => {
                    let origin = file_store::FileKey::Path(file_path.clone());
                    let _staged = ScopedWorkspaceEntry::insert(ws, file_path.clone(), analysis);
                    let locs = match resolved {
                        resolve::ResolvedTarget::Target(t) => {
                            let mask = resolve::references_mask_for(ws, Some(idx), &t);
                            resolve::refs_to(ws, Some(idx), &t, mask)
                        }
                        resolve::ResolvedTarget::Group { local_spans, pinned_spans, members } => {
                            resolve::group_refs(
                                ws, Some(idx), &origin, &local_spans, &pinned_spans, &members, None,
                            )
                        }
                        resolve::ResolvedTarget::Local => unreachable!("handled above"),
                    };
                    for loc in locs {
                        let path = match &loc.key {
                            file_store::FileKey::Path(p) => p.display().to_string(),
                            file_store::FileKey::Url(u) => u.to_file_path()
                                .map(|p| p.display().to_string()).unwrap_or_else(|_| u.to_string()),
                        };
                        let (line, col) = sources.display(&path, loc.span.start.row, loc.span.start.column);
                        results.push(serde_json::json!({"file": path, "line": line, "col": col}));
                    }
                }
            }
            Ok(serde_json::to_string_pretty(&results).unwrap())
        }
        "implementations" => {
            let (_s, _t, mut analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            analysis.enrich_imported_types_with_keys(Some(idx));
            let mut sources = SourceCache::new();
            let mut results = Vec::new();
            if let Some(resolve::ResolvedTarget::Target(t)) =
                resolve::resolve_symbol(&analysis, point, Some(idx))
            {
                for loc in resolve::implementations_of(&analysis, Some(idx), &t) {
                    let path = match &loc.key {
                        file_store::FileKey::Path(p) => p.display().to_string(),
                        file_store::FileKey::Url(u) => u.to_file_path()
                            .map(|p| p.display().to_string()).unwrap_or_else(|_| u.to_string()),
                    };
                    let (line, col) = sources.display(&path, loc.span.start.row, loc.span.start.column);
                    results.push(serde_json::json!({"file": path, "line": line, "col": col}));
                }
            }
            Ok(serde_json::to_string_pretty(&results).unwrap())
        }
        "hover" => {
            let (source, _t, mut analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            analysis.enrich_imported_types_with_keys(Some(idx));
            analysis.hover_info(point, &source, Some(idx))
                .ok_or_else(|| format!("No hover info at {}:{}", req.line, req.col))
        }
        "type-at" => {
            let (_s, _t, analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            if let Some(r) = analysis.ref_at(point) {
                if let Some(ty) = analysis.inferred_type_via_bag(&r.target_name, point) {
                    return Ok(file_analysis::format_inferred_type(&ty));
                }
            }
            if let Some(sym) = analysis.symbol_at(point) {
                if let Some(ty) = analysis.inferred_type_via_bag(&sym.name, point) {
                    return Ok(file_analysis::format_inferred_type(&ty));
                }
            }
            Err(format!("No type info at {}:{}", req.line, req.col))
        }
        "completion" => {
            let doc = cli_open_document(file, idx);
            let items = symbols::completion_items(
                &doc.analysis, &doc.tree, &doc.text, pos, idx,
                Some(doc.stable_outline.package_lines()));
            let mut out = String::new();
            for it in &items {
                match &it.detail {
                    Some(d) if !d.is_empty() => out.push_str(&format!("{}\t{}\n", it.label, d)),
                    _ => out.push_str(&format!("{}\n", it.label)),
                }
            }
            Ok(out.trim_end_matches('\n').to_string())
        }
        "signature-help" => {
            let doc = cli_open_document(file, idx);
            match symbols::signature_help(&doc.analysis, &doc.tree, &doc.text, pos, idx) {
                Some(sh) => {
                    let active = sh.active_signature.unwrap_or(0) as usize;
                    let mut out = String::new();
                    for (i, sig) in sh.signatures.iter().enumerate() {
                        let marker = if i == active { "* " } else { "  " };
                        out.push_str(&format!("{}{}\n", marker, sig.label));
                        if i == active {
                            if let Some(p) = sh.active_parameter {
                                let plabel = sig.parameters.as_ref()
                                    .and_then(|ps| ps.get(p as usize))
                                    .map(|pi| match &pi.label {
                                        tower_lsp::lsp_types::ParameterLabel::Simple(s) => s.clone(),
                                        tower_lsp::lsp_types::ParameterLabel::LabelOffsets([a, b]) =>
                                            sig.label.get(*a as usize..*b as usize).unwrap_or("").to_string(),
                                    })
                                    .unwrap_or_default();
                                out.push_str(&format!("    active param: {} ({})\n", p, plabel));
                            }
                        }
                    }
                    Ok(out.trim_end_matches('\n').to_string())
                }
                None => Err(format!("No signature help at {}:{}", req.line, req.col)),
            }
        }
        "document-highlight" => {
            let doc = cli_open_document(file, idx);
            let highlights = symbols::document_highlights(&doc.analysis, pos, Some(idx));
            let mut sources = SourceCache::new();
            let path = std::fs::canonicalize(file).map(|p| p.display().to_string())
                .unwrap_or_else(|_| file.to_string());
            let mut out = String::new();
            for h in &highlights {
                let (line, col) = sources.display(&path, h.range.start.line as usize, h.range.start.character as usize);
                let kind = match h.kind {
                    Some(tower_lsp::lsp_types::DocumentHighlightKind::WRITE) => "WRITE",
                    _ => "READ",
                };
                out.push_str(&format!("{}:{}:{}\t{}\n", path, line, col, kind));
            }
            Ok(out.trim_end_matches('\n').to_string())
        }
        "linked-editing" => {
            let doc = cli_open_document(file, idx);
            let path = std::fs::canonicalize(file).map(|p| p.display().to_string())
                .unwrap_or_else(|_| file.to_string());
            match symbols::linked_editing_ranges(&doc.analysis, pos, Some(idx)) {
                Some(ranges) => {
                    let mut sources = SourceCache::new();
                    let mut out = String::new();
                    for r in &ranges {
                        let (line, col) = sources.display(&path, r.start.line as usize, r.start.character as usize);
                        out.push_str(&format!("{}:{}:{}\n", path, line, col));
                    }
                    Ok(out.trim_end_matches('\n').to_string())
                }
                None => Err(format!("No linked-editing ranges at {}:{} (need >= 2 occurrences)", req.line, req.col)),
            }
        }
        "semantic-tokens" => {
            let doc = cli_open_document(file, idx);
            let tokens = symbols::semantic_tokens(&doc.analysis);
            let legend = symbols::semantic_token_types();
            let (mut line, mut col): (u32, u32) = (0, 0);
            let mut out = String::new();
            for t in &tokens {
                line += t.delta_line;
                if t.delta_line == 0 { col += t.delta_start; } else { col = t.delta_start; }
                let name = legend.get(t.token_type as usize).map(|tt| tt.as_str()).unwrap_or("?");
                out.push_str(&format!("{}:{} len={} {}\n", line + 1, col, t.length, name));
            }
            Ok(out.trim_end_matches('\n').to_string())
        }
        "outline" => {
            let (_s, _t, analysis) = parse_file(file);
            resolve_imports_blocking(idx, &analysis);
            Ok(outline_json(&analysis))
        }
        "workspace-symbol" => {
            let q = req.query.clone().unwrap_or_default().to_lowercase();
            let mut results = Vec::new();
            for entry in ws.workspace_raw().iter() {
                for sym in &entry.value().symbols {
                    if sym.name.to_lowercase().contains(&q) {
                        results.push(serde_json::json!({
                            "name": sym.name, "kind": format!("{:?}", sym.kind),
                            "file": entry.key().display().to_string(),
                            "line": sym.selection_span.start.row, "col": sym.selection_span.start.column,
                        }));
                    }
                }
            }
            Ok(serde_json::to_string_pretty(&results).unwrap())
        }
        "rename" => {
            let new_name = req.newname.clone().unwrap_or_else(|| "RENAMED".to_string());
            run_rename(ws, idx, file, point, &new_name)
        }
        "diagnostics" => Ok(batch_diagnostics(ws, idx)),
        other => Err(format!("unknown query: {}", other)),
    }
}

/// Document-symbol outline as the pretty-JSON array string (shared by
/// `cli_outline` and `run_one`).
fn outline_json(analysis: &file_analysis::FileAnalysis) -> String {
    let mut results = Vec::new();
    let mut seen: std::collections::HashSet<(String, String, usize, usize)> =
        std::collections::HashSet::new();
    for sym in &analysis.symbols {
        match sym.kind {
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method
            | file_analysis::SymKind::Package | file_analysis::SymKind::Class
            | file_analysis::SymKind::Variable | file_analysis::SymKind::Handler => {}
            _ => continue,
        }
        let hidden = match &sym.detail {
            file_analysis::SymbolDetail::Sub { hide_in_outline, .. } => *hide_in_outline,
            file_analysis::SymbolDetail::Handler { hide_in_outline, .. } => *hide_in_outline,
            _ => false,
        };
        if hidden { continue; }
        if sym.kind == file_analysis::SymKind::Variable && analysis.scope_within_sub_body(sym.scope) {
            continue;
        }
        let mut entry = serde_json::json!({
            "name": sym.name,
            "kind": format!("{:?}", sym.kind),
            "line": sym.selection_span.start.row,
            "col": sym.selection_span.start.column,
        });
        // Richer per-symbol detail (package, params, inferred return type,
        // method flag, display flavor, handler dispatchers) — the QA/debug
        // signal the outline carries beyond name/kind/line/col.
        if let Some(ref pkg) = sym.package {
            entry["package"] = serde_json::json!(pkg);
        }
        if let file_analysis::SymbolDetail::Sub { ref params, is_method, ref display, .. } = sym.detail {
            if params.iter().any(|p| !p.is_invocant) {
                let param_names: Vec<&str> = params.iter()
                    .filter(|p| !p.is_invocant)
                    .map(|p| p.name.as_str())
                    .collect();
                entry["params"] = serde_json::json!(param_names);
            }
            if let Some(rt) = analysis.symbol_return_type_via_bag(sym.id, None) {
                entry["return_type"] = serde_json::json!(file_analysis::format_inferred_type(&rt));
            }
            if is_method {
                entry["is_method"] = serde_json::json!(true);
            }
            if let Some(d) = display {
                entry["display"] = serde_json::json!(format!("{:?}", d));
            }
        }
        if let file_analysis::SymbolDetail::Handler { ref params, ref dispatchers, ref display, .. } = sym.detail {
            let param_names: Vec<&str> = params.iter()
                .filter(|p| !p.is_invocant)
                .map(|p| p.name.as_str())
                .collect();
            entry["params"] = serde_json::json!(param_names);
            entry["dispatchers"] = serde_json::json!(dispatchers);
            entry["display"] = serde_json::json!(format!("{:?}", display));
        }
        use file_analysis::Namespace;
        let is_framework = matches!(sym.namespace, Namespace::Framework { .. });
        let is_dupeable = is_framework && matches!(sym.kind,
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method);
        if is_dupeable {
            let key = (format!("{:?}", sym.kind), sym.name.clone(),
                sym.span.start.row, sym.span.start.column);
            if !seen.insert(key) { continue; }
        }
        results.push(entry);
    }
    serde_json::to_string_pretty(&results).unwrap()
}

/// Cross-file rename edit-set as the pretty-JSON object string (shared by
/// `cli_rename` and `run_one`). `file` is the originating file; `point` the cursor.
fn run_rename(
    ws: &file_store::FileStore,
    idx: &module_index::ModuleIndex,
    file: &str,
    point: tree_sitter::Point,
    new_name: &str,
) -> Result<String, String> {
    use std::collections::HashMap;
    let file_path = std::path::Path::new(file).canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(file));
    let (_s, _t, mut analysis) = parse_file(file);
    analysis.enrich_imported_types_with_keys(Some(idx));
    let resolved = resolve::resolve_symbol(&analysis, point, Some(idx))
        .ok_or_else(|| format!("Nothing renameable at {}:{}", point.row, point.column))?;
    let mut all_edits: HashMap<String, Vec<serde_json::Value>> = HashMap::new();
    let (locations, replacement) = match resolved {
        resolve::ResolvedTarget::Target(t) if t.supports_cross_file_rename() => {
            let _staged = ScopedWorkspaceEntry::insert(ws, file_path, analysis);
            (
                resolve::refs_to(ws, Some(idx), &t, resolve::RoleMask::EDITABLE),
                new_name.to_string(),
            )
        }
        resolve::ResolvedTarget::Group { local_spans, pinned_spans, members } => {
            // Per-member replacement texts (bare vs affixed accessors).
            let origin = file_store::FileKey::Path(file_path.clone());
            let _staged = ScopedWorkspaceEntry::insert(ws, file_path, analysis);
            let bare_new = new_name.trim_start_matches(['$', '@', '%']);
            let edits = resolve::group_rename_edits(
                ws, Some(idx), &origin, &local_spans, &pinned_spans, &members, bare_new,
            );
            for (loc, text) in edits {
                let path = match &loc.key {
                    file_store::FileKey::Path(p) => p.display().to_string(),
                    file_store::FileKey::Url(u) => u.to_file_path()
                        .map(|p| p.display().to_string()).unwrap_or_else(|_| u.to_string()),
                };
                all_edits.entry(path).or_default().push(span_to_json(loc.span, text));
            }
            return Ok(serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap());
        }
        // Lexical variables, hash keys, handlers: single-file rename — the
        // same policy split the LSP rename handler reads off the target.
        _ => {
            if let Some(edits) = analysis.rename_at(point, new_name) {
                let json_edits: Vec<_> = edits.into_iter()
                    .map(|(span, text)| span_to_json(span, text)).collect();
                all_edits.insert(file_path.display().to_string(), json_edits);
            }
            return Ok(serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap());
        }
    };
    for loc in locations {
        let path = match &loc.key {
            file_store::FileKey::Path(p) => p.display().to_string(),
            file_store::FileKey::Url(u) => u.to_file_path()
                .map(|p| p.display().to_string()).unwrap_or_else(|_| u.to_string()),
        };
        all_edits.entry(path).or_default().push(span_to_json(loc.span, replacement.clone()));
    }
    Ok(serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap())
}

/// Whole-tree diagnostics with enrichment parity: each workspace entry
/// is deep-copied (bincode — `FileAnalysis` isn't `Clone`) and enriched
/// with imported types before `collect_diagnostics` — the same pass
/// `publish_diagnostics` runs on open docs, so cross-file-typed shapes
/// hint here too. A file whose roundtrip fails degrades to its
/// unenriched analysis rather than vanishing.
fn enriched_tree_diagnostics(
    ws: &file_store::FileStore,
    idx: &module_index::ModuleIndex,
    options: symbols::DiagnosticOptions,
) -> Vec<(String, tower_lsp::lsp_types::Diagnostic)> {
    let mut all = Vec::new();
    for entry in ws.workspace_raw().iter() {
        let file = entry.key().display().to_string();
        let enriched = bincode::serialize(&**entry.value())
            .ok()
            .and_then(|bin| bincode::deserialize::<file_analysis::FileAnalysis>(&bin).ok())
            .map(|mut fa| {
                fa.after_deserialize();
                fa.enrich_imported_types_with_keys(Some(idx));
                fa
            });
        let diags = match &enriched {
            Some(fa) => symbols::collect_diagnostics(fa, idx, options),
            None => symbols::collect_diagnostics(entry.value(), idx, options),
        };
        for d in diags {
            all.push((file.clone(), d));
        }
    }
    all
}

/// Whole-tree diagnostics as the pretty-JSON array string (warning+; shared by
/// `--batch` diagnostics requests). Mirrors `cli_check`'s JSON path.
fn batch_diagnostics(ws: &file_store::FileStore, idx: &module_index::ModuleIndex) -> String {
    let options = symbols::DiagnosticOptions { unresolved_dispatch: false };
    let mut all = Vec::new();
    for (file, d) in enriched_tree_diagnostics(ws, idx, options) {
        let sev = match d.severity {
            Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::ERROR => "error",
            Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::WARNING => "warning",
            Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION => "info",
            Some(s) if s == tower_lsp::lsp_types::DiagnosticSeverity::HINT => "hint",
            _ => "warning",
        };
        all.push(serde_json::json!({
            "file": file, "line": d.range.start.line, "col": d.range.start.character,
            "severity": sev,
            "code": d.code.map(|c| match c {
                tower_lsp::lsp_types::NumberOrString::String(s) => s,
                tower_lsp::lsp_types::NumberOrString::Number(n) => n.to_string(),
            }).unwrap_or_default(),
            "message": d.message,
        }));
    }
    serde_json::to_string_pretty(&all).unwrap()
}

/// --batch <root> — read JSONL queries on stdin, share one startup, print one
/// JSON response per line. The harness substrate: amortizes the workspace-index
/// + @INC cost across every query instead of paying it per process.
fn cli_batch(root: &str) {
    use std::io::{BufRead, Write};
    let (ws, idx) = cli_full_startup(root);
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    let mut diag_memo: Option<String> = None;
    for line in stdin.lock().lines() {
        let line = match line { Ok(l) => l, Err(_) => break };
        if line.trim().is_empty() { continue; }
        let req: BatchReq = match serde_json::from_str(&line) {
            Ok(r) => r,
            Err(e) => {
                let _ = writeln!(out, "{}", serde_json::json!({"ok": false, "err": format!("bad request: {}", e)}));
                continue;
            }
        };
        // Whole-tree diagnostics are request-independent within one
        // batch process (ScopedWorkspaceEntry restores any staging) —
        // compute the enriched pass once, replay it for every row.
        let result = if req.q == "diagnostics" {
            Ok(diag_memo
                .get_or_insert_with(|| batch_diagnostics(&ws, &idx))
                .clone())
        } else {
            run_one(&ws, &idx, &req)
        };
        let resp = match result {
            Ok(s)  => serde_json::json!({"id": req.id, "ok": true,  "out": s}),
            Err(e) => serde_json::json!({"id": req.id, "ok": false, "err": e}),
        };
        let _ = writeln!(out, "{}", serde_json::to_string(&resp).unwrap());
        let _ = out.flush();   // flush per line so a later abort still localizes
    }
}

/// --rename <root> <file> <line> <col> <new_name> — Cross-file rename
fn cli_rename(root: &str, file: &str, line_str: &str, col_str: &str, new_name: &str) {
    let point = parse_point(line_str, col_str);
    // Full startup so workspace files are built with the same plugins, type
    // inference, and enrichment that the LSP backend would use.
    let (ws, idx) = cli_full_startup(root);
    match run_rename(&ws, &idx, file, point, new_name) {
        Ok(s) => println!("{}", s),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

/// --dump-package <root> <package> — Dump every sub in a package with
/// derived type info. Debugging aid for the witness/reducer pipeline:
/// prints the raw `return_type` baked into the Symbol, the witness-bag
/// projection at the default and a few common arities, the structural
/// tail-delegation, every param's inferred type at a point
/// just past the sub's signature, and a witness count so you can see at
/// a glance whether the bag has anything to say.
fn cli_dump_package(root: &str, package_name: &str) {
    use std::sync::Arc;
    use file_analysis::{FileAnalysis, SymKind, SymbolDetail};

    let (ws, module_index) = cli_full_startup(root);

    // Find a FileAnalysis whose package matches. Workspace first; fall
    // back to cached @INC modules. No bespoke discovery — only what
    // the normal startup populated.
    let mut found: Option<(String, Arc<FileAnalysis>)> = None;
    for entry in ws.workspace_raw().iter() {
        let analysis = entry.value();
        let has_package = analysis.symbols.iter().any(|s| {
            matches!(s.kind, SymKind::Package | SymKind::Class)
                && s.name == package_name
        });
        if has_package {
            found = Some((entry.key().display().to_string(), Arc::clone(analysis)));
            break;
        }
    }
    if found.is_none() {
        if let Some(cached) = module_index.get_cached(package_name) {
            found = Some((cached.path.display().to_string(), Arc::clone(&cached.analysis)));
        }
    }

    let Some((path, analysis_ro)) = found else {
        eprintln!("Package '{}' not found in workspace or module cache.", package_name);
        eprintln!("(Run the LSP against this workspace once to populate cached @INC modules.)");
        std::process::exit(1);
    };

    // Deep-copy via bincode (FileAnalysis isn't Clone) and enrich the
    // private copy with imported types so cross-file return inferences
    // are visible — same pass the LSP runs in publish_diagnostics.
    let bin = bincode::serialize(&*analysis_ro).expect("bincode FileAnalysis");
    let mut analysis: FileAnalysis = bincode::deserialize(&bin).expect("bincode FileAnalysis roundtrip");
    analysis.enrich_imported_types_with_keys(Some(&module_index));

    // Collect subs/methods declared inside this package.
    let mut subs: Vec<&file_analysis::Symbol> = analysis
        .symbols
        .iter()
        .filter(|s| {
            matches!(s.kind, SymKind::Sub | SymKind::Method)
                && s.package.as_deref() == Some(package_name)
        })
        .collect();
    subs.sort_by_key(|s| (s.span.start.row, s.span.start.column));

    let framework = analysis
        .package_framework
        .get(package_name)
        .map(|f| format!("{:?}", f));

    let mut sub_entries = Vec::with_capacity(subs.len());
    for sym in &subs {
        let SymbolDetail::Sub {
            ref params,
            is_method,
            ref display,
            hide_in_outline,
            opaque_return,
            ref doc,
            ..
        } = sym.detail
        else {
            continue;
        };

        // Pick a point inside the sub body so scope-resolved param
        // lookups land in the right scope. End of line N+1 is past
        // any signature parens for almost every shape.
        let probe = tree_sitter::Point::new(
            sym.span.start.row.saturating_add(1),
            0,
        );

        let bag_default = analysis
            .sub_return_type_at_arity(&sym.name, None)
            .as_ref()
            .map(file_analysis::format_inferred_type);
        let mut by_arity = serde_json::Map::new();
        for arity in 0u32..=2 {
            if let Some(t) = analysis.sub_return_type_at_arity(&sym.name, Some(arity)) {
                by_arity.insert(arity.to_string(), serde_json::json!(file_analysis::format_inferred_type(&t)));
            }
        }

        let raw_return = analysis
            .symbol_return_type_via_bag(sym.id, None)
            .as_ref()
            .map(file_analysis::format_inferred_type);

        let param_entries: Vec<_> = params
            .iter()
            .map(|p| {
                let inferred = analysis
                    .inferred_type_via_bag_ctx(&p.name, probe, Some(&module_index))
                    .as_ref()
                    .map(file_analysis::format_inferred_type);
                serde_json::json!({
                    "name": p.name,
                    "is_invocant": p.is_invocant,
                    "is_slurpy": p.is_slurpy,
                    "default": p.default,
                    "inferred_type": inferred,
                })
            })
            .collect();

        // Witness count on the sub's Symbol attachment — surfaces
        // arity / branch-arm observations the reducer is folding.
        let symbol_witness_count = analysis
            .witnesses
            .for_attachment(&witnesses::WitnessAttachment::Symbol(sym.id))
            .len();

        // Provenance: where did this return type come from? Default
        // (Inferred) is implicit; surfaced explicitly only when
        // something else (plugin override / reducer / delegation)
        // contributed. Critical debugging aid when inference grows
        // complex enough that "the LSP says X" needs to come with
        // "because Y" — without re-running the build.
        let provenance = match analysis.return_type_provenance(sym.id) {
            file_analysis::TypeProvenance::Inferred => None,
            file_analysis::TypeProvenance::PluginOverride { plugin_id, reason } => {
                Some(serde_json::json!({
                    "kind": "PluginOverride",
                    "plugin_id": plugin_id,
                    "reason": reason,
                }))
            }
            file_analysis::TypeProvenance::ReducerFold { reducer, evidence } => {
                Some(serde_json::json!({
                    "kind": "ReducerFold",
                    "reducer": reducer,
                    "evidence": evidence,
                }))
            }
            file_analysis::TypeProvenance::Delegation { kind, via } => {
                Some(serde_json::json!({
                    "kind": "Delegation",
                    "delegation_kind": kind,
                    "via": via,
                }))
            }
            file_analysis::TypeProvenance::FrameworkSynthesis { framework, reason } => {
                Some(serde_json::json!({
                    "kind": "FrameworkSynthesis",
                    "framework": framework,
                    "reason": reason,
                }))
            }
        };

        // Variables typed inside this sub's scope. Surfaces chain
        // assignments like `my $route = $self->_route(...)->...->to(...)`
        // — when `$route` shows up here with a class type, the chain
        // typer worked. When it doesn't, the chain died at some link.
        // The same dump that answers "why is `_generate_route`'s
        // return type None" answers "is `$route` typed at all".
        let sub_scope_id = analysis
            .scopes
            .iter()
            .find(|s| {
                matches!(
                    &s.kind,
                    file_analysis::ScopeKind::Sub { name } | file_analysis::ScopeKind::Method { name }
                        if name == &sym.name
                ) && s.span.start == sym.span.start
            })
            .map(|s| s.id);
        let mut vars_in_scope: Vec<serde_json::Value> = Vec::new();
        if let Some(sid) = sub_scope_id {
            use crate::witnesses::{WitnessAttachment, WitnessPayload};
            for w in analysis.witnesses.all() {
                let WitnessAttachment::Variable { name, scope } = &w.attachment else { continue };
                if *scope != sid { continue; }
                let WitnessPayload::InferredType(t) = &w.payload else { continue };
                vars_in_scope.push(serde_json::json!({
                    "var": name,
                    "type": file_analysis::format_inferred_type(t),
                    "line": w.span.start.row,
                }));
            }
        }

        let mut entry = serde_json::json!({
            "name": sym.name,
            "kind": format!("{:?}", sym.kind),
            "is_method": is_method,
            "line": sym.selection_span.start.row,
            "params": param_entries,
            "raw_return_type": raw_return,
            "bag_return_type": bag_default,
            "bag_return_type_at_arity": serde_json::Value::Object(by_arity),
            "symbol_witness_count": symbol_witness_count,
            "vars_in_scope": vars_in_scope,
        });
        if let Some(prov) = provenance {
            entry["return_type_provenance"] = prov;
        }
        if let Some(d) = display {
            entry["display"] = serde_json::json!(format!("{:?}", d));
        }
        if hide_in_outline {
            entry["hide_in_outline"] = serde_json::json!(true);
        }
        if opaque_return {
            entry["opaque_return"] = serde_json::json!(true);
        }
        if let Some(ref outline) = sym.outline_label {
            entry["outline_label"] = serde_json::json!(outline);
        }
        if let Some(d) = doc.as_ref() {
            let first_line = d.lines().find(|l| !l.trim().is_empty()).unwrap_or("");
            if !first_line.is_empty() {
                entry["doc_first_line"] = serde_json::json!(first_line);
            }
        }
        sub_entries.push(entry);
    }

    let parents = analysis
        .package_parents
        .get(package_name)
        .cloned()
        .unwrap_or_default();

    let out = serde_json::json!({
        "package": package_name,
        "file": path,
        "framework": framework,
        "parents": parents,
        "total_witnesses_in_file": analysis.witnesses.len(),
        "subs": sub_entries,
    });

    eprintln!(
        "Dumped {} subs from {} (file: {})",
        subs.len(),
        package_name,
        out.get("file").and_then(|v| v.as_str()).unwrap_or("?")
    );
    println!("{}", serde_json::to_string_pretty(&out).unwrap());
}

/// --workspace-symbol <root> <query> — Search symbols
fn cli_workspace_symbol(root: &str, query: &str) {
    // Full startup so workspace symbols reflect plugin-synthesized entities
    // (helpers, routes, accessors), built with `root`'s plugins not cwd's.
    let (ws, idx) = cli_full_startup(root);
    let req = BatchReq {
        id: String::new(), q: "workspace-symbol".into(),
        file: String::new(), line: 0, col: 0,
        query: Some(query.to_string()), newname: None,
    };
    print_run_one(&ws, &idx, &req);
}

/// --clear-cache [<root>] — Remove the SQLite module cache.
///
/// Without `<root>`: nuke the entire `~/.cache/perl-lsp` (or
/// `$XDG_CACHE_HOME/perl-lsp`) tree — every project the user has
/// touched. With `<root>`: only the per-project cache dir
/// (`<base>/<workspace_hash>`) is removed; other projects keep theirs.
///
/// `<root>` is canonicalized and joined with `file://` to match the
/// hash key the LSP server and `cli_full_startup` write under (the
/// initialize request hands a `file://` URI; both CLI and server feed
/// that string into `cache_dir_for_workspace`). Without canonicalizing
/// here a relative path would hash to a different bucket and silently
/// "clear" a non-existent dir.
///
/// On the next LSP start the cache is recreated from scratch — the
/// resolver re-resolves modules lazily and the workspace indexer
/// re-walks the tree. Pure side-effect command; prints what was
/// removed so it's obvious whether anything happened.
fn cli_clear_cache(root: Option<&str>) {
    let target = match root {
        Some(r) => {
            let (_, root_uri) = canonical_root_and_uri(r);
            module_cache::cache_dir_for_workspace(Some(&root_uri))
        }
        None => module_cache::cache_base_dir(),
    };
    let Some(path) = target else {
        eprintln!("Cannot determine cache dir: $HOME and $XDG_CACHE_HOME are both unset");
        std::process::exit(1);
    };
    if !path.exists() {
        eprintln!("Cache already absent: {}", path.display());
        return;
    }
    match std::fs::remove_dir_all(&path) {
        Ok(()) => eprintln!("Cleared cache: {}", path.display()),
        Err(e) => {
            eprintln!("Failed to remove {}: {}", path.display(), e);
            std::process::exit(1);
        }
    }
}

/// Pretty-print the tree-sitter parse tree for a Perl source file.
/// `<file>` may be `-` to read from stdin. Mirrors `tree-sitter parse`
/// output shape — `(node_kind [row, col] - [row, col]` per line,
/// 2-space indent per depth, field names prefixed (`field: kind`).
fn cli_parse(path: &str) {
    use std::io::Read;
    let source = if path == "-" {
        let mut s = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut s) {
            eprintln!("read stdin: {}", e);
            std::process::exit(1);
        }
        s
    } else {
        match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("read {}: {}", path, e);
                std::process::exit(1);
            }
        }
    };
    let mut parser = builder::create_parser();
    let Some(tree) = parser.parse(&source, None) else {
        eprintln!("parse failed");
        std::process::exit(1);
    };
    use std::io::IsTerminal;
    let color = std::io::stdout().is_terminal();
    // 6 rainbow ANSI 256-color picks. Both the paren AND the node
    // kind name share the depth's color — that's the visual cue
    // for "this paren matches this kind." Field names + line
    // ranges get distinct colors so they don't blur into the
    // rainbow.
    const RAINBOW: [&str; 6] = [
        "\x1b[38;5;196m", // red
        "\x1b[38;5;208m", // orange
        "\x1b[38;5;226m", // yellow
        "\x1b[38;5;46m",  // green
        "\x1b[38;5;39m",  // blue
        "\x1b[38;5;165m", // magenta
    ];
    const FIELD: &str = "\x1b[38;5;245m"; // gray
    const RANGE: &str = "\x1b[38;5;242m"; // darker gray
    const RESET: &str = "\x1b[0m";
    fn walk(node: tree_sitter::Node, field: Option<&str>, depth: usize, color: bool) {
        let pad = "  ".repeat(depth);
        let (hue, fc, rc, rs) = if color {
            (RAINBOW[depth % 6], FIELD, RANGE, RESET)
        } else {
            ("", "", "", "")
        };
        let prefix = field
            .map(|f| format!("{}{}: {}", fc, f, rs))
            .unwrap_or_default();
        let s = node.start_position();
        let e = node.end_position();
        print!(
            "{}{}{}({}{} {}[{}, {}] - [{}, {}]{}",
            pad,
            prefix,
            hue,
            node.kind(),
            rs,
            rc,
            s.row,
            s.column,
            e.row,
            e.column,
            rs,
        );
        let mut cursor = node.walk();
        let mut field_idx: u32 = 0;
        for child in node.children(&mut cursor) {
            let fname = node.field_name_for_child(field_idx);
            field_idx += 1;
            if !child.is_named() {
                continue;
            }
            println!();
            walk(child, fname, depth + 1, color);
        }
        print!("{}){}", hue, rs);
    }
    walk(tree.root_node(), None, 0, color);
    println!();
}
