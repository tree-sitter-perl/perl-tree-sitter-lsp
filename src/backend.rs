use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::Document;
use crate::symbols;

pub struct Backend {
    client: Client,
    documents: DashMap<Url, Document>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Backend {
            client,
            documents: DashMap::new(),
        }
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        let diagnostics = match self.documents.get(uri) {
            Some(doc) => symbols::collect_diagnostics(&doc.tree, &doc.text),
            None => vec![],
        };
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                    ]),
                    ..Default::default()
                }),
                document_highlight_provider: Some(OneOf::Left(true)),
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
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
            self.documents.insert(uri.clone(), doc);
        }
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.update(change.text);
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
        let syms = symbols::extract_symbols(&doc.tree, &doc.text);
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
        Ok(symbols::find_definition(&doc.tree, &doc.text, pos, uri))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let refs = symbols::find_references(&doc.tree, &doc.text, pos, uri);
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
        Ok(symbols::rename(&doc.tree, &doc.text, pos, uri, new_name))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        Ok(symbols::hover_info(&doc.tree, &doc.text, pos))
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
        let items = symbols::completion_items(&doc.tree, &doc.text, pos);
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
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
        let highlights = symbols::document_highlights(&doc.tree, &doc.text, pos);
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
        let ranges = symbols::folding_ranges(&doc.tree, &doc.text);
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let tokens = symbols::semantic_tokens(&doc.tree, &doc.text);
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}
