mod backend;
mod builder;
mod cpanfile;
mod cursor_context;
mod document;
mod file_analysis;
mod module_cache;
mod module_index;
mod module_resolver;
mod pod;
mod query_cache;
mod symbols;

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

fn index_workspace(root: &str) -> dashmap::DashMap<std::path::PathBuf, file_analysis::FileAnalysis> {
    use std::path::{Path, PathBuf};
    let root_path = Path::new(root).canonicalize().unwrap_or_else(|_| PathBuf::from(root));
    let ws = dashmap::DashMap::new();
    let count = module_resolver::index_workspace(&root_path, &ws);
    eprintln!("Indexed {} files", count);
    ws
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

    let ws = index_workspace(root);

    // Resolve imported modules so diagnostics can suppress known imports.
    // Uses SQLite cache (same as the LSP resolver thread) for fast repeat runs.
    let module_index = module_index::ModuleIndex::new_for_cli();
    {
        let root_path = std::path::Path::new(root).canonicalize()
            .unwrap_or_else(|_| std::path::PathBuf::from(root));
        let root_uri = format!("file://{}", root_path.display());

        let mut inc_paths = module_resolver::discover_inc_paths();
        module_resolver::add_project_lib_paths(&mut inc_paths, &root_path);

        // Open (or create) the per-project SQLite cache
        let db = module_cache::open_cache_db(Some(&root_uri));
        let mut stale_set: std::collections::HashSet<String> = std::collections::HashSet::new();
        if let Some(ref conn) = db {
            let _ = module_cache::validate_inc_paths(conn, &inc_paths);
            let (warmed, stale) = module_cache::warm_cache(conn, &module_index.cache_raw());
            if warmed > 0 {
                eprintln!("Cache: {} modules loaded from disk", warmed);
            }
            stale_set = stale.into_iter().collect();
        }

        // Collect all unique module names needed
        let mut needed = std::collections::HashSet::new();
        for entry in ws.iter() {
            for imp in &entry.value().imports {
                needed.insert(imp.module_name.clone());
            }
            for parents in entry.value().package_parents.values() {
                for p in parents {
                    needed.insert(p.clone());
                }
            }
        }

        // Resolve modules not already in cache (or stale from version bump)
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
                    module_cache::save_to_db(conn, name, &Some(std::sync::Arc::clone(&cached)), "cli-check");
                }
                module_index.insert_cache(name, Some(cached));
                resolved += 1;
            }
        }
        eprintln!("Modules: {} cached, {} resolved, {} total", already_cached, resolved, needed.len());
    }

    let mut all_diagnostics = Vec::new();

    for entry in ws.iter() {
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
        let files = ws.len();
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
    for sym in &analysis.symbols {
        match sym.kind {
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method
            | file_analysis::SymKind::Package | file_analysis::SymKind::Class
            | file_analysis::SymKind::Variable => {}
            _ => continue,
        }
        let mut entry = serde_json::json!({
            "name": sym.name,
            "kind": format!("{:?}", sym.kind),
            "line": sym.selection_span.start.row,
            "col": sym.selection_span.start.column,
        });
        if let Some(ref pkg) = sym.package {
            entry["package"] = serde_json::json!(pkg);
        }
        if let file_analysis::SymbolDetail::Sub { ref params, ref return_type, is_method, .. } = sym.detail {
            if !params.is_empty() {
                let param_names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
                entry["params"] = serde_json::json!(param_names);
            }
            if let Some(ref rt) = return_type {
                entry["return_type"] = serde_json::json!(file_analysis::format_inferred_type(rt));
            }
            if is_method {
                entry["is_method"] = serde_json::json!(true);
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

    // Check refs for inferred type
    if let Some(r) = analysis.ref_at(point) {
        if let Some(ty) = analysis.inferred_type(&r.target_name, point) {
            println!("{}", file_analysis::format_inferred_type(ty));
            return;
        }
    }
    // Check symbols
    if let Some(sym) = analysis.symbol_at(point) {
        if let Some(ty) = analysis.inferred_type(&sym.name, point) {
            println!("{}", file_analysis::format_inferred_type(ty));
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

    if let Some(span) = analysis.find_definition(point, Some(&tree), Some(source.as_bytes()), None) {
        println!("{}:{}:{}", file, span.start.row + 1, span.start.column + 1);
        return;
    }

    // Cross-file: check if it's a method/package ref that resolves via workspace
    let ws = index_workspace(root);

    if let Some(r) = analysis.ref_at(point) {
        match &r.kind {
            file_analysis::RefKind::FunctionCall | file_analysis::RefKind::MethodCall { .. } => {
                for entry in ws.iter() {
                    for sym in &entry.value().symbols {
                        if sym.name == r.target_name
                            && matches!(sym.kind, file_analysis::SymKind::Sub | file_analysis::SymKind::Method)
                        {
                            println!("{}:{}:{}", entry.key().display(),
                                sym.selection_span.start.row + 1,
                                sym.selection_span.start.column + 1);
                            return;
                        }
                    }
                }
            }
            file_analysis::RefKind::PackageRef => {
                for entry in ws.iter() {
                    for sym in &entry.value().symbols {
                        if sym.name == r.target_name
                            && matches!(sym.kind, file_analysis::SymKind::Package | file_analysis::SymKind::Class)
                        {
                            println!("{}:{}:{}", entry.key().display(),
                                sym.selection_span.start.row + 1,
                                sym.selection_span.start.column + 1);
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }

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
            file_analysis::RefKind::FunctionCall | file_analysis::RefKind::MethodCall { .. }
        ) || analysis.symbol_at(point).map(|s| matches!(s.kind,
            file_analysis::SymKind::Sub | file_analysis::SymKind::Method
        )).unwrap_or(false);

        if is_sub || matches!(r.kind, file_analysis::RefKind::PackageRef) {
            let ws = index_workspace(root);
            for entry in ws.iter() {
                if *entry.key() == file_path { continue; }
                let edits = if is_sub {
                    entry.value().rename_sub(target, target)
                } else {
                    entry.value().rename_package(target, target)
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
    if !ws.contains_key(&file_path) {
        let (_source, _tree, analysis) = parse_file(file);
        ws.insert(file_path.clone(), analysis);
    }
    let analysis_ref = ws.get(&file_path).unwrap();

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
        file_analysis::RenameKind::Variable | file_analysis::RenameKind::HashKey(_) => {
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
        file_analysis::RenameKind::Function(_) | file_analysis::RenameKind::Method(_) => {
            let name = match &rename_kind {
                file_analysis::RenameKind::Function(n) | file_analysis::RenameKind::Method(n) => n.clone(),
                _ => unreachable!(),
            };
            for entry in ws.iter() {
                let edits = entry.value().rename_sub(&name, new_name);
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
            for entry in ws.iter() {
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

/// --workspace-symbol <root> <query> — Search symbols
fn cli_workspace_symbol(root: &str, query: &str) {
    let ws = index_workspace(root);
    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for entry in ws.iter() {
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
