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
    // Subprocess mode: parse a .pm file and print exports as JSON.
    // Used by the module resolver with a hard timeout to protect against
    // infinite loops in the tree-sitter external scanner.
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 3 && args[1] == "--parse-exports" {
        module_index::subprocess_main(&args[2]);
        return;
    }

    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
