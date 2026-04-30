mod backend;
mod builder;
mod cpanfile;
mod cursor_context;
mod document;
mod file_analysis;
mod file_store;
mod module_cache;
mod module_index;
mod module_resolver;
mod plugin;
mod pod;
mod query_cache;
mod resolve;
mod symbols;
mod witnesses;

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
        Some("--hover") if args.len() >= 5 => {
            cli_hover(&args[2], &args[3], &args[4]);
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
        Some("--rename") if args.len() == 7 => {
            cli_rename(&args[2], &args[3], &args[4], &args[5], &args[6]);
            return;
        }
        Some("--workspace-symbol") if args.len() >= 4 => {
            cli_workspace_symbol(&args[2], &args[3]);
            return;
        }
        Some("--plugin-check") => {
            plugin::cli::cli_plugin_check(&args[2..]);
            return;
        }
        Some("--plugin-run") => {
            plugin::cli::cli_plugin_run(&args[2..]);
            return;
        }
        Some("--plugin-test") => {
            plugin::cli::cli_plugin_test(&args[2..]);
            return;
        }
        Some("--dump-package") if args.len() >= 4 => {
            cli_dump_package(&args[2], &args[3]);
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
    eprintln!("ANALYSIS:");
    eprintln!("  perl-lsp --check [<root>] [--severity error|warning]    Batch diagnostics (CI)");
    eprintln!("                           [--format json|human]");
    eprintln!("  perl-lsp --outline <file>                              Document symbol outline");
    eprintln!("  perl-lsp --hover <file> <line> <col>                   Type info and docs");
    eprintln!("  perl-lsp --type-at <file> <line> <col>                 Single type query");
    eprintln!("  perl-lsp --definition <root> <file> <line> <col>       Cross-file goto-def");
    eprintln!("  perl-lsp --references <root> <file> <line> <col>       Cross-file find-refs");
    eprintln!();
    eprintln!("REFACTORING:");
    eprintln!("  perl-lsp --rename <root> <file> <line> <col> <new>     Cross-file rename");
    eprintln!("  perl-lsp --workspace-symbol <root> <query>             Search symbols");
    eprintln!();
    eprintln!("PLUGIN AUTHORING:");
    eprintln!("  perl-lsp --plugin-check <file.rhai>                    Lint a Rhai plugin");
    eprintln!("  perl-lsp --plugin-run <file.rhai> --on <fixture.pl>    Run plugin on one Perl file");
    eprintln!("  perl-lsp --plugin-test <plugin-dir> [--update]         Snapshot-test a plugin dir");
    eprintln!();
    eprintln!("DEBUG:");
    eprintln!("  perl-lsp --dump-package <root> <package>               Dump every sub in <package>");
    eprintln!("                                                         with derived type info");
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

fn index_workspace(root: &str) -> file_store::FileStore {
    use std::path::{Path, PathBuf};
    let root_path = Path::new(root).canonicalize().unwrap_or_else(|_| PathBuf::from(root));
    let store = file_store::FileStore::new();
    let count = module_resolver::index_workspace(&root_path, &store);
    eprintln!("Indexed {} files", count);
    store
}

/// Full CLI workspace setup: index the workspace, open the SQLite cache,
/// warm cached modules, resolve missing imports + ancestors via @INC,
/// save fresh entries back to disk. Mirrors the LSP server's startup
/// minus the resolver thread (one-shot, synchronous). Used by every
/// CLI command that needs cross-file resolution to behave like the
/// running server.
fn cli_full_startup(root: &str) -> (file_store::FileStore, module_index::ModuleIndex) {
    let ws = index_workspace(root);

    let module_index = module_index::ModuleIndex::new_for_cli();
    let root_path = std::path::Path::new(root).canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(root));
    let root_uri = format!("file://{}", root_path.display());

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
    let mut resolved = 0usize;
    let mut already_cached = 0usize;
    for name in &needed {
        if module_index.cache_raw().contains_key(name.as_str()) && !stale_set.contains(name) {
            already_cached += 1;
            continue;
        }
        if let Some(cached) = module_resolver::resolve_and_parse(&inc_paths, name, &mut parser) {
            if let Some(ref conn) = db {
                module_cache::save_to_db(conn, name, &Some(std::sync::Arc::clone(&cached)), "cli");
            }
            module_index.insert_cache(name, Some(cached));
            resolved += 1;
        }
    }
    eprintln!("Modules: {} cached, {} resolved, {} total", already_cached, resolved, needed.len());

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

    let (ws, module_index) = cli_full_startup(root);

    let mut all_diagnostics = Vec::new();

    for entry in ws.workspace_raw().iter() {
        let file = entry.key().display().to_string();
        let diags = symbols::collect_diagnostics(entry.value(), &module_index);
        for d in diags {
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

    let mut results = Vec::new();
    let mut outline_seen: std::collections::HashSet<(String, String, usize, usize)> =
        std::collections::HashSet::new();
    for sym in &analysis.symbols {
        match sym.kind {
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method
            | file_analysis::SymKind::Package | file_analysis::SymKind::Class
            | file_analysis::SymKind::Variable | file_analysis::SymKind::Handler => {}
            _ => continue,
        }
        // Honor plugin-opted-out symbols: DSL imports, framework
        // infrastructure, anything the plugin flagged as non-outline.
        let hidden = match &sym.detail {
            file_analysis::SymbolDetail::Sub { hide_in_outline, .. } => *hide_in_outline,
            file_analysis::SymbolDetail::Handler { hide_in_outline, .. } => *hide_in_outline,
            _ => false,
        };
        if hidden { continue; }
        let mut entry = serde_json::json!({
            "name": sym.name,
            "kind": format!("{:?}", sym.kind),
            "line": sym.selection_span.start.row,
            "col": sym.selection_span.start.column,
        });
        if let Some(ref pkg) = sym.package {
            entry["package"] = serde_json::json!(pkg);
        }
        if let file_analysis::SymbolDetail::Sub { ref params, ref return_type, is_method, ref display, .. } = sym.detail {
            if params.iter().any(|p| !p.is_invocant) {
                let param_names: Vec<&str> = params.iter()
                    .filter(|p| !p.is_invocant)
                    .map(|p| p.name.as_str())
                    .collect();
                entry["params"] = serde_json::json!(param_names);
            }
            if let Some(ref rt) = return_type {
                entry["return_type"] = serde_json::json!(file_analysis::format_inferred_type(rt));
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

        // Fan-out dedup: framework plugins may register the same Symbol
        // on multiple classes (Mojo helpers on Controller AND the app
        // class) for resolution; the outline should only show it once.
        use file_analysis::Namespace;
        let is_framework = matches!(sym.namespace, Namespace::Framework { .. });
        let is_dupeable = is_framework && matches!(sym.kind,
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method);
        if is_dupeable {
            let key = (
                format!("{:?}", sym.kind),
                sym.name.clone(),
                sym.span.start.row,
                sym.span.start.column,
            );
            if !outline_seen.insert(key) {
                continue;
            }
        }
        results.push(entry);
    }

    println!("{}", serde_json::to_string_pretty(&results).unwrap());
}

/// --hover <file> <line> <col> — Type info and docs
fn cli_hover(file: &str, line_str: &str, col_str: &str) {
    let (source, tree, analysis) = parse_file(file);
    let point = parse_point(line_str, col_str);

    // Use the same hover function as the LSP, but without module_index
    if let Some(markdown) = analysis.hover_info(point, &source, Some(&tree), None) {
        println!("{}", markdown);
    } else {
        eprintln!("No hover info at {}:{}", line_str, col_str);
        std::process::exit(1);
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

/// --definition <root> <file> <line> <col> — Cross-file goto-def
fn cli_definition(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (source, tree, analysis) = parse_file(file);
    let point = parse_point(line_str, col_str);

    // Build a real ModuleIndex and register workspace modules the same
    // way the backend does, so CLI gd goes through the production
    // cross-file resolution path (Handler/MethodCallRef/DispatchCall/
    // inheritance walk via class_content_index) instead of the CLI's
    // old ad-hoc workspace scan. Keeps nvim and CLI results in sync.
    let idx = module_index::ModuleIndex::new_for_cli();
    let files = file_store::FileStore::new();
    let root_path = std::path::PathBuf::from(root);
    module_resolver::index_workspace_with_index(&root_path, &files, Some(&idx));

    let abs = std::fs::canonicalize(file).unwrap_or_else(|_| std::path::PathBuf::from(file));
    let uri = tower_lsp::lsp_types::Url::from_file_path(&abs)
        .unwrap_or_else(|_| tower_lsp::lsp_types::Url::parse("file:///unknown").unwrap());
    let pos = tower_lsp::lsp_types::Position {
        line: point.row as u32,
        character: point.column as u32,
    };

    if let Some(resp) = symbols::find_definition(&analysis, pos, &uri, &idx, &tree, &source) {
        use tower_lsp::lsp_types::GotoDefinitionResponse;
        let first = match resp {
            GotoDefinitionResponse::Scalar(loc) => Some(loc),
            GotoDefinitionResponse::Array(v) => v.into_iter().next(),
            GotoDefinitionResponse::Link(v) => v.into_iter().next().map(|l| tower_lsp::lsp_types::Location {
                uri: l.target_uri,
                range: l.target_range,
            }),
        };
        if let Some(loc) = first {
            let path = loc.uri.to_file_path()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|_| loc.uri.to_string());
            println!("{}:{}:{}", path,
                loc.range.start.line + 1,
                loc.range.start.character + 1);
            return;
        }
    }

    let _ = files; // suppress unused-variable warnings
    eprintln!("No definition found at {}:{}", line_str, col_str);
    std::process::exit(1);
}

/// --references <root> <file> <line> <col> — Cross-file find-refs
fn cli_references(root: &str, file: &str, line_str: &str, col_str: &str) {
    let (source, tree, analysis) = parse_file(file);
    let point = parse_point(line_str, col_str);

    let mut results = Vec::new();
    let file_path = std::path::Path::new(file).canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(file));

    // Local refs
    let local_refs = analysis.find_references(point, Some(&tree), Some(source.as_bytes()));
    for span in &local_refs {
        results.push(serde_json::json!({
            "file": file_path.display().to_string(),
            "line": span.start.row,
            "col": span.start.column,
        }));
    }

    // Cross-file refs (for functions/methods/packages)
    if let Some(r) = analysis.ref_at(point) {
        let target = &r.target_name;
        let is_sub = matches!(r.kind,
            file_analysis::RefKind::FunctionCall { .. } | file_analysis::RefKind::MethodCall { .. }
        ) || analysis.symbol_at(point).map(|s| matches!(s.kind,
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method
        )).unwrap_or(false);

        if is_sub || matches!(r.kind, file_analysis::RefKind::PackageRef) {
            let ws = index_workspace(root);
            // Derive the scope (package for Sub, class for Method) so
            // cross-file walks don't union unrelated packages that
            // share a symbol name.
            let scope_from_ref = match &r.kind {
                file_analysis::RefKind::FunctionCall { resolved_package } =>
                    Some(("sub", resolved_package.clone())),
                file_analysis::RefKind::MethodCall { invocant_class, .. } =>
                    invocant_class.as_ref().map(|c| ("method", Some(c.clone()))),
                _ => None,
            };
            let scope_from_sym = analysis.symbol_at(point).and_then(|s| match s.kind {
                file_analysis::SymKind::Sub => Some(("sub", s.package.clone())),
                file_analysis::SymKind::Method => Some(("method", s.package.clone())),
                _ => None,
            });
            let scope = scope_from_ref.or(scope_from_sym);

            for entry in ws.workspace_raw().iter() {
                if *entry.key() == file_path { continue; }
                let edits = if matches!(r.kind, file_analysis::RefKind::PackageRef) {
                    entry.value().rename_package(target, target)
                } else {
                    match &scope {
                        Some(("sub", package)) =>
                            entry.value().rename_sub_in_package(target, package, target),
                        Some(("method", Some(class))) =>
                            entry.value().rename_method_in_class(target, class, target),
                        // Unresolvable method scope — skip rather than
                        // cross-link. Matches refs_to / rename semantics.
                        _ => Vec::new(),
                    }
                };
                for (span, _) in edits {
                    results.push(serde_json::json!({
                        "file": entry.key().display().to_string(),
                        "line": span.start.row,
                        "col": span.start.column,
                    }));
                }
            }
        }
    }

    println!("{}", serde_json::to_string_pretty(&results).unwrap());
    eprintln!("{} references", results.len());
}

/// --rename <root> <file> <line> <col> <new_name> — Cross-file rename
fn cli_rename(root: &str, file: &str, line_str: &str, col_str: &str, new_name: &str) {
    use std::path::{Path, PathBuf};
    use std::collections::HashMap;

    let file_path = Path::new(file).canonicalize().unwrap_or_else(|_| PathBuf::from(file));
    let point = parse_point(line_str, col_str);
    let ws = index_workspace(root);

    // Ensure target file is in the workspace index
    if ws.get_workspace(&file_path).is_none() {
        let (_source, _tree, analysis) = parse_file(file);
        ws.insert_workspace(file_path.clone(), analysis);
    }
    let analysis_ref = ws.get_workspace(&file_path).unwrap();

    let rename_kind = match analysis_ref.rename_kind_at(point) {
        Some(k) => k,
        None => {
            eprintln!("Nothing renameable at {}:{}", line_str, col_str);
            std::process::exit(1);
        }
    };

    eprintln!("Rename kind: {:?}", rename_kind);

    let mut all_edits: HashMap<String, Vec<serde_json::Value>> = HashMap::new();

    match &rename_kind {
        file_analysis::RenameKind::Variable
        | file_analysis::RenameKind::HashKey(_)
        | file_analysis::RenameKind::Handler { .. } => {
            let source = std::fs::read_to_string(&file_path).expect("cannot read file");
            let mut parser = module_resolver::create_parser();
            let tree = parser.parse(&source, None).expect("parse failed");
            if let Some(edits) = analysis_ref.rename_at(point, new_name, Some(&tree), Some(source.as_bytes())) {
                let json_edits: Vec<_> = edits.into_iter()
                    .map(|(span, text)| span_to_json(span, text))
                    .collect();
                all_edits.insert(file_path.display().to_string(), json_edits);
            }
        }
        file_analysis::RenameKind::Function { .. } | file_analysis::RenameKind::Method { .. } => {
            for entry in ws.workspace_raw().iter() {
                let edits = match &rename_kind {
                    file_analysis::RenameKind::Function { name, package } =>
                        entry.value().rename_sub_in_package(name, package, new_name),
                    file_analysis::RenameKind::Method { name, class } =>
                        entry.value().rename_method_in_class(name, class, new_name),
                    _ => unreachable!(),
                };
                if !edits.is_empty() {
                    let json_edits: Vec<_> = edits.into_iter()
                        .map(|(span, text)| span_to_json(span, text))
                        .collect();
                    all_edits.insert(entry.key().display().to_string(), json_edits);
                }
            }
        }
        file_analysis::RenameKind::Package(_) => {
            let name = match &rename_kind {
                file_analysis::RenameKind::Package(n) => n.clone(),
                _ => unreachable!(),
            };
            for entry in ws.workspace_raw().iter() {
                let edits = entry.value().rename_package(&name, new_name);
                if !edits.is_empty() {
                    let json_edits: Vec<_> = edits.into_iter()
                        .map(|(span, text)| span_to_json(span, text))
                        .collect();
                    all_edits.insert(entry.key().display().to_string(), json_edits);
                }
            }
        }
    };

    // Drop the ref before iterating
    drop(analysis_ref);

    println!("{}", serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap());
}

/// --dump-package <root> <package> — Dump every sub in a package with
/// derived type info. Debugging aid for the witness/reducer pipeline:
/// prints the raw `return_type` baked into the Symbol, the witness-bag
/// projection at the default and a few common arities, the structural
/// tail (`return_self_method`), every param's inferred type at a point
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
    let (imported_returns, imported_keys) =
        backend::build_imported_return_types(&analysis, &module_index);
    analysis.enrich_imported_types_with_keys(
        imported_returns,
        imported_keys,
        Some(&module_index),
    );

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
            ref return_type,
            ref display,
            hide_in_outline,
            opaque_return,
            ref return_self_method,
            ref doc,
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

        let raw_return = return_type
            .as_ref()
            .map(file_analysis::format_inferred_type);

        let param_entries: Vec<_> = params
            .iter()
            .map(|p| {
                let inferred = analysis
                    .inferred_type_via_bag(&p.name, probe)
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
            for tc in &analysis.type_constraints {
                if tc.scope == sid {
                    vars_in_scope.push(serde_json::json!({
                        "var": tc.variable,
                        "type": file_analysis::format_inferred_type(&tc.inferred_type),
                        "line": tc.constraint_span.start.row,
                    }));
                }
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
            "return_self_method": return_self_method,
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
    let ws = index_workspace(root);
    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for entry in ws.workspace_raw().iter() {
        for sym in &entry.value().symbols {
            if sym.name.to_lowercase().contains(&query_lower) {
                results.push(serde_json::json!({
                    "name": sym.name,
                    "kind": format!("{:?}", sym.kind),
                    "file": entry.key().display().to_string(),
                    "line": sym.selection_span.start.row,
                    "col": sym.selection_span.start.column,
                }));
            }
        }
    }

    eprintln!("{} symbols matching '{}'", results.len(), query);
    println!("{}", serde_json::to_string_pretty(&results).unwrap());
}
