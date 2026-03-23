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
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
