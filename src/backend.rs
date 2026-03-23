use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification, request};
use tower_lsp::{Client, LanguageServer};

use crate::document::Document;
use crate::file_analysis::{FileAnalysis, InferredType};
use crate::module_index::ModuleIndex;
use crate::symbols;

pub struct Backend {
    client: Client,
    documents: Arc<DashMap<Url, Document>>,
    module_index: Arc<ModuleIndex>,
    workspace_index: Arc<DashMap<PathBuf, FileAnalysis>>,
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
                    entry.analysis.enrich_imported_types_with_keys(imported_returns, imported_keys, Some(module_index));
                    let diagnostics = symbols::collect_diagnostics(&entry.analysis, module_index);
                    client.publish_diagnostics(uri, diagnostics, None).await;
                }
            });
        };

        let module_index = Arc::new(ModuleIndex::new(client.clone(), on_refresh));
        let _ = module_index_holder.set(Arc::clone(&module_index));
        let workspace_index: Arc<DashMap<PathBuf, FileAnalysis>> = Arc::new(DashMap::new());

        Backend {
            module_index,
            client,
            documents,
            workspace_index,
        }
    }

    fn enrich_analysis(&self, uri: &Url) {
        if let Some(mut doc) = self.documents.get_mut(uri) {
            let (imported_returns, imported_keys) = build_imported_return_types(&doc.analysis, &self.module_index);
            doc.analysis.enrich_imported_types_with_keys(imported_returns, imported_keys, Some(&self.module_index));
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
            for (name, sub_info) in &exports.subs {
                if let Some(ref ty) = sub_info.return_type {
                    type_map.insert(name.clone(), ty.clone());
                }
                if !sub_info.hash_keys.is_empty() {
                    keys_map.insert(name.clone(), sub_info.hash_keys.clone());
                }
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
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                linked_editing_range_provider: Some(LinkedEditingRangeServerCapabilities::Simple(true)),
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
                workspace_symbol_provider: Some(OneOf::Left(true)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: None,
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "perl-lsp initialized")
            .await;

        // Register file watchers for workspace indexing
        let registrations = vec![Registration {
            id: "perl-file-watcher".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![
                    FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.pm".into()), kind: None },
                    FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.pl".into()), kind: None },
                    FileSystemWatcher { glob_pattern: GlobPattern::String("**/*.t".into()), kind: None },
                ],
            }).unwrap()),
        }];
        let _ = self.client.register_capability(registrations).await;

        // Spawn workspace indexing in background with progress reporting
        let ws_index = Arc::clone(&self.workspace_index);
        let client = self.client.clone();
        let root = self.module_index.workspace_root();
        tokio::task::spawn_blocking(move || {
            if let Some(root_uri) = root {
                if let Some(root_path) = root_uri.strip_prefix("file://") {
                    let root_path = PathBuf::from(root_path);
                    let rt = tokio::runtime::Handle::current();
                    let token = NumberOrString::String("perl-lsp/workspace-index".to_string());

                    // Start progress
                    let _ = rt.block_on(client.send_request::<request::WorkDoneProgressCreate>(
                        WorkDoneProgressCreateParams { token: token.clone() },
                    ));
                    rt.block_on(client.send_notification::<notification::Progress>(
                        ProgressParams {
                            token: token.clone(),
                            value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                                WorkDoneProgressBegin {
                                    title: "Indexing workspace".into(),
                                    cancellable: Some(false),
                                    message: Some("Scanning files...".into()),
                                    percentage: Some(0),
                                },
                            )),
                        },
                    ));

                    let count = crate::module_resolver::index_workspace(&root_path, &ws_index);

                    // End progress
                    rt.block_on(client.send_notification::<notification::Progress>(
                        ProgressParams {
                            token,
                            value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(
                                WorkDoneProgressEnd {
                                    message: Some(format!("Indexed {} files", count)),
                                },
                            )),
                        },
                    ));
                }
            }
        });
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
            // Enqueue parent classes for resolution (inheritance chain).
            for parents in doc.analysis.package_parents.values() {
                for parent in parents {
                    self.module_index.request_resolve(parent);
                }
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
                // Resolve parent classes for inheritance.
                for parents in doc.analysis.package_parents.values() {
                    for parent in parents {
                        self.module_index.request_resolve(parent);
                    }
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

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let doc = match self.documents.get(&params.text_document.uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let point = symbols::position_to_point(params.position);
        if let Some(sym) = doc.analysis.symbol_at(point) {
            return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: symbols::span_to_range(sym.selection_span),
                placeholder: sym.name.clone(),
            }));
        }
        if let Some(r) = doc.analysis.ref_at(point) {
            return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: symbols::span_to_range(r.span),
                placeholder: r.target_name.clone(),
            }));
        }
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = &params.new_name;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let point = symbols::position_to_point(pos);
        let rename_kind = doc.analysis.rename_kind_at(point);

        match rename_kind {
            Some(crate::file_analysis::RenameKind::Variable) => {
                // Single-file only — lexical scope doesn't cross files
                Ok(symbols::rename(&doc.analysis, pos, uri, new_name))
            }
            Some(crate::file_analysis::RenameKind::Function(ref name))
            | Some(crate::file_analysis::RenameKind::Method(ref name))
            | Some(crate::file_analysis::RenameKind::Package(ref name))
            | Some(crate::file_analysis::RenameKind::HashKey(ref name)) => {
                let rename_fn: fn(&FileAnalysis, &str, &str) -> Vec<(crate::file_analysis::Span, String)> = match &rename_kind {
                    Some(crate::file_analysis::RenameKind::Function(_)) => FileAnalysis::rename_function,
                    Some(crate::file_analysis::RenameKind::Package(_)) => FileAnalysis::rename_package,
                    Some(crate::file_analysis::RenameKind::Method(_)) => FileAnalysis::rename_method,
                    Some(crate::file_analysis::RenameKind::HashKey(_)) => {
                        // Hash keys: single-file for now
                        return Ok(symbols::rename(&doc.analysis, pos, uri, new_name));
                    }
                    _ => unreachable!(),
                };

                let mut all_changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

                let mut collect = |uri: Url, analysis: &FileAnalysis| {
                    let edits = rename_fn(analysis, name, new_name);
                    if !edits.is_empty() {
                        all_changes.entry(uri).or_default().extend(
                            edits.into_iter().map(|(span, text)| TextEdit {
                                range: symbols::span_to_range(span),
                                new_text: text,
                            })
                        );
                    }
                };

                // Search open documents
                let mut seen_paths = std::collections::HashSet::new();
                for entry in self.documents.iter() {
                    if let Ok(path) = entry.key().to_file_path() {
                        seen_paths.insert(path);
                    }
                    collect(entry.key().clone(), &entry.value().analysis);
                }

                // Search workspace index
                for entry in self.workspace_index.iter() {
                    if seen_paths.contains(entry.key()) { continue; }
                    let ws_uri = Url::from_file_path(entry.key()).unwrap_or_else(|_| {
                        Url::parse(&format!("file://{}", entry.key().display())).unwrap()
                    });
                    collect(ws_uri, entry.value());
                }

                if all_changes.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(WorkspaceEdit {
                        changes: Some(all_changes),
                        ..Default::default()
                    }))
                }
            }
            None => Ok(None),
        }
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
            // If any item is a loading placeholder (empty insert_text), mark as incomplete
            // so the editor re-requests on next keystroke after the module resolves.
            let is_incomplete = items.iter().any(|i| i.insert_text.as_deref() == Some(""));
            if is_incomplete {
                // Trigger resolution for the module being loaded
                for i in &items {
                    if i.insert_text.as_deref() == Some("") {
                        if let Some(ref label) = Some(&i.label) {
                            if let Some(name) = label.strip_prefix("loading ").and_then(|s| s.strip_suffix("...")) {
                                self.module_index.request_resolve(name);
                            }
                        }
                    }
                }
                Ok(Some(CompletionResponse::List(CompletionList {
                    is_incomplete: true,
                    items,
                })))
            } else {
                Ok(Some(CompletionResponse::Array(items)))
            }
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
        Ok(symbols::signature_help(&doc.analysis, &doc.tree, &doc.text, pos, &self.module_index))
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

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();
        let mut results = Vec::new();
        let mut seen_paths = std::collections::HashSet::new();

        // Search open documents first (freshest analysis)
        for entry in self.documents.iter() {
            let uri = entry.key().clone();
            if let Ok(path) = uri.to_file_path() {
                seen_paths.insert(path);
            }
            for sym in &entry.value().analysis.symbols {
                if sym.name.to_lowercase().contains(&query) {
                    if let Some(info) = symbols::symbol_to_workspace_info(sym, uri.clone()) {
                        results.push(info);
                    }
                }
            }
        }

        // Then workspace index (skip files already covered by open docs)
        for entry in self.workspace_index.iter() {
            if seen_paths.contains(entry.key()) { continue; }
            let uri = Url::from_file_path(entry.key()).unwrap_or_else(|_| {
                Url::parse(&format!("file://{}", entry.key().display())).unwrap()
            });
            for sym in &entry.value().symbols {
                if sym.name.to_lowercase().contains(&query) {
                    if let Some(info) = symbols::symbol_to_workspace_info(sym, uri.clone()) {
                        results.push(info);
                    }
                }
            }
        }

        if results.is_empty() {
            Ok(None)
        } else {
            Ok(Some(results))
        }
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let ws_index = Arc::clone(&self.workspace_index);
        tokio::task::spawn_blocking(move || {
            for change in params.changes {
                let path = match change.uri.to_file_path() {
                    Ok(p) => p,
                    Err(_) => continue,
                };
                match change.typ {
                    FileChangeType::DELETED => {
                        ws_index.remove(&path);
                    }
                    _ => {
                        // Re-index the file (created or changed)
                        if let Ok(source) = std::fs::read_to_string(&path) {
                            let mut parser = crate::module_resolver::create_parser();
                            if let Some(tree) = parser.parse(&source, None) {
                                let analysis = crate::builder::build(&tree, source.as_bytes());
                                ws_index.insert(path, analysis);
                            }
                        }
                    }
                }
            }
        });
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Extract lines for the range
        let start_line = params.range.start.line as usize;
        let end_line = params.range.end.line as usize;
        let lines: Vec<&str> = doc.text.lines().collect();
        if start_line >= lines.len() { return Ok(None); }
        let end = (end_line + 1).min(lines.len());
        let range_text: String = lines[start_line..end].join("\n") + "\n";

        // Shell out to perltidy on the range
        let result = tokio::process::Command::new("perltidy")
            .arg("--standard-output")
            .arg("--standard-error-output")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn();

        let mut child = match result {
            Ok(c) => c,
            Err(_) => return Ok(None),
        };

        if let Some(mut stdin) = child.stdin.take() {
            use tokio::io::AsyncWriteExt;
            let _ = stdin.write_all(range_text.as_bytes()).await;
            drop(stdin);
        }

        let output = match child.wait_with_output().await {
            Ok(o) if o.status.success() => o,
            _ => return Ok(None),
        };

        let formatted = String::from_utf8_lossy(&output.stdout).to_string();
        if formatted == range_text {
            return Ok(None);
        }

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position { line: start_line as u32, character: 0 },
                end: Position { line: end as u32, character: 0 },
            },
            new_text: formatted,
        }]))
    }

    async fn linked_editing_range(
        &self,
        params: LinkedEditingRangeParams,
    ) -> Result<Option<LinkedEditingRanges>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let point = symbols::position_to_point(pos);
        let refs = doc.analysis.find_references(point, None, None);
        if refs.len() < 2 {
            return Ok(None);
        }

        let ranges: Vec<Range> = refs.into_iter()
            .map(|span| symbols::span_to_range(span))
            .collect();

        Ok(Some(LinkedEditingRanges {
            ranges,
            word_pattern: None,
        }))
    }
}
