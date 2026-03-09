use std::collections::HashMap;
use std::sync::Arc;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::Document;
use crate::file_analysis::InferredType;
use crate::module_index::ModuleIndex;
use crate::symbols;

pub struct Backend {
    client: Client,
    documents: Arc<DashMap<Url, Document>>,
    module_index: Arc<ModuleIndex>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let documents: Arc<DashMap<Url, Document>> = Arc::new(DashMap::new());

        // We need Arc<ModuleIndex> so the refresh callback can access it.
        // Use a two-phase init: create with a placeholder, then wrap in Arc.
        let refresh_client = client.clone();
        let refresh_docs = Arc::clone(&documents);

        // We'll set the module_index into this Arc after creation.
        // The refresh callback captures Arcs to docs and module_index.
        let module_index_holder: Arc<std::sync::OnceLock<Arc<ModuleIndex>>> =
            Arc::new(std::sync::OnceLock::new());
        let holder_clone = Arc::clone(&module_index_holder);

        // Capture the tokio handle so the callback can spawn async work
        // from the resolver thread (which has no tokio context).
        let tokio_handle = tokio::runtime::Handle::current();
        let on_refresh = move || {
            let client = refresh_client.clone();
            let docs = Arc::clone(&refresh_docs);
            let holder = Arc::clone(&holder_clone);
            // Spawn onto the tokio runtime — can't block the resolver thread.
            tokio_handle.spawn(async move {
                let module_index = match holder.get() {
                    Some(idx) => idx,
                    None => return,
                };
                for mut entry in docs.iter_mut() {
                    let uri = entry.key().clone();
                    let (imported_returns, imported_keys) = build_imported_return_types(&entry.analysis, module_index);
                    entry.analysis.enrich_imported_types_with_keys(imported_returns, imported_keys);
                    let diagnostics = symbols::collect_diagnostics(&entry.analysis, module_index);
                    client.publish_diagnostics(uri, diagnostics, None).await;
                }
            });
        };

        let module_index = Arc::new(ModuleIndex::new(client.clone(), on_refresh));
        let _ = module_index_holder.set(Arc::clone(&module_index));

        Backend {
            module_index,
            client,
            documents,
        }
    }

    fn enrich_analysis(&self, uri: &Url) {
        if let Some(mut doc) = self.documents.get_mut(uri) {
            let (imported_returns, imported_keys) = build_imported_return_types(&doc.analysis, &self.module_index);
            doc.analysis.enrich_imported_types_with_keys(imported_returns, imported_keys);
        }
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        self.enrich_analysis(uri);
        let diagnostics = match self.documents.get(uri) {
            Some(doc) => symbols::collect_diagnostics(&doc.analysis, &self.module_index),
            None => vec![],
        };
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

fn build_imported_return_types(
    analysis: &crate::file_analysis::FileAnalysis,
    module_index: &ModuleIndex,
) -> (HashMap<String, InferredType>, HashMap<String, Vec<String>>) {
    let mut type_map = HashMap::new();
    let mut keys_map = HashMap::new();
    for import in &analysis.imports {
        if let Some(exports) = module_index.get_exports_cached(&import.module_name) {
            for (name, ty) in &exports.return_types {
                type_map.insert(name.clone(), ty.clone());
            }
            for (name, keys) in &exports.hash_keys {
                keys_map.insert(name.clone(), keys.clone());
            }
        }
    }
    (type_map, keys_map)
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Notify resolver thread of workspace root for per-project cache.
        let root = params
            .root_uri
            .as_ref()
            .map(|u| u.as_str())
            .or_else(|| {
                params
                    .workspace_folders
                    .as_ref()
                    .and_then(|f| f.first())
                    .map(|f| f.uri.as_str())
            });
        self.module_index.set_workspace_root(root);

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "perl-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                rename_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        "$".to_string(),
                        "@".to_string(),
                        "%".to_string(),
                        ">".to_string(),
                        ":".to_string(),
                        "{".to_string(),
                        "(".to_string(),
                        ",".to_string(),
                    ]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![")".to_string()]),
                    work_done_progress_options: Default::default(),
                }),
                document_highlight_provider: Some(OneOf::Left(true)),
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: symbols::semantic_token_types(),
                                token_modifiers: symbols::semantic_token_modifiers(),
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "perl-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        if let Some(doc) = Document::new(text) {
            // Enqueue imports for background resolution (non-blocking).
            for imp in &doc.analysis.imports {
                self.module_index.request_resolve(&imp.module_name);
            }
            self.documents.insert(uri.clone(), doc);
        }
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.update(change.text);
                // Resolve any new imports that appeared during editing.
                for imp in &doc.analysis.imports {
                    self.module_index.request_resolve(&imp.module_name);
                }
            }
        }
        self.publish_diagnostics(&uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            let uri = params.text_document.uri;
            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.update(text);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let syms = symbols::extract_symbols(&doc.analysis);
        Ok(Some(DocumentSymbolResponse::Nested(syms)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        Ok(symbols::find_definition(
            &doc.analysis,
            pos,
            uri,
            &self.module_index,
            &doc.tree,
            &doc.text,
        ))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let refs = symbols::find_references(&doc.analysis, pos, uri, &doc.tree, &doc.text);
        if refs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(refs))
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = &params.new_name;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        Ok(symbols::rename(&doc.analysis, pos, uri, new_name))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        Ok(symbols::hover_info(
            &doc.analysis,
            &doc.text,
            pos,
            &self.module_index,
            &doc.tree,
        ))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let items = symbols::completion_items(
            &doc.analysis,
            &doc.tree,
            &doc.text,
            pos,
            &self.module_index,
        );
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        Ok(symbols::signature_help(&doc.analysis, &doc.tree, &doc.text, pos))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let highlights = symbols::document_highlights(&doc.analysis, pos, &doc.tree, &doc.text);
        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let ranges: Vec<SelectionRange> = params
            .positions
            .iter()
            .map(|pos| symbols::selection_ranges(&doc.tree, *pos))
            .collect();
        Ok(Some(ranges))
    }

    async fn folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let ranges = symbols::folding_ranges(&doc.analysis);
        if ranges.is_empty() {
            Ok(None)
        } else {
            Ok(Some(ranges))
        }
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Shell out to perltidy
        let result = tokio::process::Command::new("perltidy")
            .arg("--standard-output")
            .arg("--standard-error-output")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn();

        let mut child = match result {
            Ok(c) => c,
            Err(e) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to run perltidy: {}", e),
                    )
                    .await;
                return Ok(None);
            }
        };

        // Write source to stdin
        if let Some(mut stdin) = child.stdin.take() {
            use tokio::io::AsyncWriteExt;
            let _ = stdin.write_all(doc.text.as_bytes()).await;
            drop(stdin);
        }

        let output = match child.wait_with_output().await {
            Ok(o) => o,
            Err(e) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("perltidy failed: {}", e),
                    )
                    .await;
                return Ok(None);
            }
        };

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("perltidy exited with error: {}", stderr),
                )
                .await;
            return Ok(None);
        }

        let formatted = String::from_utf8_lossy(&output.stdout).to_string();
        if formatted == doc.text {
            return Ok(None);
        }

        // Replace entire document
        let line_count = doc.text.lines().count();
        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: line_count as u32,
                    character: 0,
                },
            },
            new_text: formatted,
        }]))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let actions = symbols::code_actions(&params.context.diagnostics, &doc.analysis, uri);
        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let tokens = symbols::semantic_tokens(&doc.analysis);
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let hints = symbols::inlay_hints(&doc.analysis, params.range);
        if hints.is_empty() {
            Ok(None)
        } else {
            Ok(Some(hints))
        }
    }
}
