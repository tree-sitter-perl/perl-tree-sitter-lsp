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

    // CLI mode: --rename <root> <file> <line> <col> <new_name>
    if args.len() == 7 && args[1] == "--rename" {
        cli_rename(&args[2], &args[3], &args[4], &args[5], &args[6]);
        return;
    }

    // CLI mode: --workspace-symbol <root> <query>
    if args.len() == 4 && args[1] == "--workspace-symbol" {
        cli_workspace_symbol(&args[2], &args[3]);
        return;
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
    eprintln!("  perl-lsp                                          Start LSP server (stdio)");
    eprintln!("  perl-lsp --rename <root> <file> <line> <col> <new_name>");
    eprintln!("                                                    Cross-file rename, outputs JSON");
    eprintln!("  perl-lsp --workspace-symbol <root> <query>        Search symbols, outputs JSON");
    eprintln!("  perl-lsp --version                                Print version");
}

fn span_to_json(span: file_analysis::Span, text: String) -> serde_json::Value {
    serde_json::json!({
        "line": span.start.row, "col": span.start.column,
        "end_line": span.end.row, "end_col": span.end.column,
        "new_text": text
    })
}

/// CLI: index a workspace, perform a rename, output JSON edits.
fn cli_rename(root: &str, file: &str, line_str: &str, col_str: &str, new_name: &str) {
    use std::path::{Path, PathBuf};
    use std::collections::HashMap;
    use dashmap::DashMap;

    let root_path = Path::new(root).canonicalize().unwrap_or_else(|_| PathBuf::from(root));
    let file_path = Path::new(file).canonicalize().unwrap_or_else(|_| PathBuf::from(file));
    let line: usize = line_str.parse().expect("line must be a number");
    let col: usize = col_str.parse().expect("col must be a number");
    let point = tree_sitter::Point::new(line, col);

    // Index workspace
    let workspace_index: DashMap<PathBuf, file_analysis::FileAnalysis> = DashMap::new();
    let count = module_resolver::index_workspace(&root_path, &workspace_index);
    eprintln!("Indexed {} files", count);

    // Ensure target file is in the workspace index (it should be, but parse fresh if not)
    if !workspace_index.contains_key(&file_path) {
        let source = std::fs::read_to_string(&file_path).expect("cannot read file");
        let mut parser = module_resolver::create_parser();
        let tree = parser.parse(&source, None).expect("parse failed");
        let analysis = builder::build(&tree, source.as_bytes());
        workspace_index.insert(file_path.clone(), analysis);
    }
    let analysis_ref = workspace_index.get(&file_path).unwrap();

    // Determine rename kind
    let rename_kind = match analysis_ref.rename_kind_at(point) {
        Some(k) => k,
        None => {
            eprintln!("Nothing renameable at {}:{}", line, col);
            std::process::exit(1);
        }
    };

    eprintln!("Rename kind: {:?}", rename_kind);

    let mut all_edits: HashMap<String, Vec<serde_json::Value>> = HashMap::new();

    let rename_fn: fn(&file_analysis::FileAnalysis, &str, &str) -> Vec<(file_analysis::Span, String)> = match &rename_kind {
        file_analysis::RenameKind::Variable | file_analysis::RenameKind::HashKey(_) => {
            // Single-file rename
            if let Some(edits) = analysis_ref.rename_at(point, new_name) {
                let json_edits: Vec<_> = edits.into_iter()
                    .map(|(span, text)| span_to_json(span, text))
                    .collect();
                all_edits.insert(file_path.display().to_string(), json_edits);
            }
            println!("{}", serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap());
            return;
        }
        file_analysis::RenameKind::Function(_) | file_analysis::RenameKind::Method(_) => {
            file_analysis::FileAnalysis::rename_sub
        }
        file_analysis::RenameKind::Package(_) => {
            file_analysis::FileAnalysis::rename_package
        }
    };

    let name = match &rename_kind {
        file_analysis::RenameKind::Function(n) | file_analysis::RenameKind::Method(n) | file_analysis::RenameKind::Package(n) => n.clone(),
        _ => unreachable!(),
    };

    // Search all workspace files
    for entry in workspace_index.iter() {
        let edits = rename_fn(entry.value(), &name, new_name);
        if !edits.is_empty() {
            let json_edits: Vec<_> = edits.into_iter()
                .map(|(span, text)| span_to_json(span, text))
                .collect();
            all_edits.insert(entry.key().display().to_string(), json_edits);
        }
    }

    println!("{}", serde_json::to_string_pretty(&serde_json::json!(all_edits)).unwrap());
}

/// CLI: index a workspace and search for symbols.
fn cli_workspace_symbol(root: &str, query: &str) {
    use std::path::{Path, PathBuf};
    use dashmap::DashMap;

    let root_path = Path::new(root).canonicalize().unwrap_or_else(|_| PathBuf::from(root));
    let workspace_index: DashMap<PathBuf, file_analysis::FileAnalysis> = DashMap::new();
    let count = module_resolver::index_workspace(&root_path, &workspace_index);
    eprintln!("Indexed {} files", count);

    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for entry in workspace_index.iter() {
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
