use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification, request};
use tower_lsp::{Client, LanguageServer};

use crate::file_analysis::{FileAnalysis, InferredType};
use crate::file_store::{FileKey, FileStore};
use crate::module_index::ModuleIndex;
use crate::symbols;

pub struct Backend {
    client: Client,
    files: Arc<FileStore>,
    module_index: Arc<ModuleIndex>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let files: Arc<FileStore> = Arc::new(FileStore::new());

        // We need Arc<ModuleIndex> so the refresh callback can access it.
        // Two-phase init: create ModuleIndex whose refresh callback references
        // a later-set Arc<ModuleIndex>, then wire up the Arc.
        let refresh_client = client.clone();
        let refresh_files = Arc::clone(&files);

        let module_index_holder: Arc<std::sync::OnceLock<Arc<ModuleIndex>>> =
            Arc::new(std::sync::OnceLock::new());
        let holder_clone = Arc::clone(&module_index_holder);

        // Capture the tokio handle so the callback can spawn async work
        // from the resolver thread (which has no tokio context).
        let tokio_handle = tokio::runtime::Handle::current();
        let on_refresh = move || {
            let client = refresh_client.clone();
            let files = Arc::clone(&refresh_files);
            let holder = Arc::clone(&holder_clone);
            tokio_handle.spawn(async move {
                let module_index = match holder.get() {
                    Some(idx) => idx,
                    None => return,
                };
                // Collect (uri, diagnostics) first without holding the store lock
                // across the await — publishing is async and could deadlock.
                let mut pending: Vec<(Url, Vec<Diagnostic>)> = Vec::new();
                files.for_each_open_mut(|uri, doc| {
                    let (imported_returns, imported_keys) =
                        build_imported_return_types(&doc.analysis, module_index);
                    doc.analysis.enrich_imported_types_with_keys(
                        imported_returns,
                        imported_keys,
                        Some(module_index),
                    );
                    let diagnostics = symbols::collect_diagnostics(&doc.analysis, module_index);
                    pending.push((uri.clone(), diagnostics));
                });
                for (uri, diags) in pending {
                    client.publish_diagnostics(uri, diags, None).await;
                }
            });
        };

        let module_index = Arc::new(ModuleIndex::new(client.clone(), on_refresh));
        let _ = module_index_holder.set(Arc::clone(&module_index));

        Backend {
            module_index,
            client,
            files,
        }
    }

    fn enrich_analysis(&self, uri: &Url) {
        if let Some(mut doc) = self.files.get_open_mut(uri) {
            let (imported_returns, imported_keys) =
                build_imported_return_types(&doc.analysis, &self.module_index);
            doc.analysis.enrich_imported_types_with_keys(
                imported_returns,
                imported_keys,
                Some(&self.module_index),
            );
        }
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        self.enrich_analysis(uri);
        let diagnostics = match self.files.get_open(uri) {
            Some(doc) => symbols::collect_diagnostics(&doc.analysis, &self.module_index),
            None => vec![],
        };
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

/// Convert a resolve::RefLocation list into the LSP Location list expected by
/// the `references` response, stable-sorted and deduplicated by (uri, span).
fn refs_to_locations(results: Vec<crate::resolve::RefLocation>) -> Option<Vec<Location>> {
    let mut locations: Vec<Location> = results
        .into_iter()
        .filter_map(|r| {
            let uri = r.to_url()?;
            Some(Location {
                uri,
                range: symbols::span_to_range(r.span),
            })
        })
        .collect();
    if locations.is_empty() {
        return None;
    }
    locations.sort_by(|a, b| {
        a.uri.as_str().cmp(b.uri.as_str())
            .then_with(|| a.range.start.line.cmp(&b.range.start.line))
            .then_with(|| a.range.start.character.cmp(&b.range.start.character))
    });
    locations.dedup_by(|a, b| a.uri == b.uri && a.range == b.range);
    Some(locations)
}

fn build_imported_return_types(
    analysis: &crate::file_analysis::FileAnalysis,
    module_index: &ModuleIndex,
) -> (HashMap<String, InferredType>, HashMap<String, Vec<String>>) {
    use crate::file_analysis::{SymKind, SymbolDetail};

    let mut type_map = HashMap::new();
    let mut keys_map = HashMap::new();
    for import in &analysis.imports {
        let cached = match module_index.get_cached(&import.module_name) {
            Some(c) => c,
            None => continue,
        };
        for sym in &cached.analysis.symbols {
            if !matches!(sym.kind, SymKind::Sub | SymKind::Method) {
                continue;
            }
            // Limit to exported names for parity with prior ExportedSub behavior.
            if !cached.analysis.export.iter().any(|n| n == &sym.name)
                && !cached.analysis.export_ok.iter().any(|n| n == &sym.name)
            {
                continue;
            }
            if let SymbolDetail::Sub { ref return_type, .. } = sym.detail {
                if let Some(ref ty) = return_type {
                    type_map.insert(sym.name.clone(), ty.clone());
                }
            }
            if let Some(sub_info) = cached.sub_info(&sym.name) {
                let hk = sub_info.hash_keys();
                if !hk.is_empty() {
                    keys_map.insert(sym.name.clone(), hk.to_vec());
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
        let files = Arc::clone(&self.files);
        let client = self.client.clone();
        let module_index = Arc::clone(&self.module_index);
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

                    let count = crate::module_resolver::index_workspace_with_index(
                        &root_path,
                        &files,
                        Some(&module_index),
                    );

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
        if self.files.open(uri.clone(), text) {
            if let Some(doc) = self.files.get_open(&uri) {
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
            }
        }
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            if let Some(mut doc) = self.files.get_open_mut(&uri) {
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
            if let Some(mut doc) = self.files.get_open_mut(&uri) {
                doc.update(text);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.close(&params.text_document.uri);
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Derive a cross-file target from the rename-kind machinery. If the
        // target is inherently local (variables), fall back to single-file
        // references — no cross-file walk to do.
        use crate::file_analysis::RenameKind;
        use crate::resolve::{refs_to, RoleMask, TargetKind, TargetRef};

        let point = symbols::position_to_point(pos);
        let target = match doc.analysis.rename_kind_at(point) {
            Some(RenameKind::Function(name)) => TargetRef {
                name,
                kind: TargetKind::Sub,
            },
            Some(RenameKind::Method(name)) => TargetRef {
                name,
                kind: TargetKind::Method,
            },
            Some(RenameKind::Package(name)) => TargetRef {
                name,
                kind: TargetKind::Package,
            },
            Some(RenameKind::HashKey(name)) => {
                // Phase 5: if the cursor is on a HashKeyDef or a HashKeyAccess
                // whose owner was resolved at build time, do a cross-file walk
                // keyed on (key name, owner). If we can't determine the owner,
                // fall back to single-file.
                use crate::file_analysis::{HashKeyOwner, RefKind, SymbolDetail};
                let cursor_point = symbols::position_to_point(pos);
                let owner_from_ref = doc.analysis.ref_at(cursor_point).and_then(|r| {
                    if let RefKind::HashKeyAccess { owner: Some(o), .. } = &r.kind {
                        Some(o.clone())
                    } else {
                        None
                    }
                });
                let owner_from_sym = doc.analysis.symbol_at(cursor_point).and_then(|s| {
                    if let SymbolDetail::HashKeyDef { owner, .. } = &s.detail {
                        Some(owner.clone())
                    } else {
                        None
                    }
                });
                let owner = owner_from_ref.or(owner_from_sym);

                match owner {
                    Some(HashKeyOwner::Sub { package, name: sub_name }) => {
                        drop(doc);
                        let results = crate::resolve::refs_to(
                            &self.files,
                            Some(&self.module_index),
                            &crate::resolve::TargetRef {
                                name,
                                kind: crate::resolve::TargetKind::HashKeyOfSub {
                                    package,
                                    name: sub_name,
                                },
                            },
                            crate::resolve::RoleMask::VISIBLE,
                        );
                        return Ok(refs_to_locations(results));
                    }
                    Some(HashKeyOwner::Class(class_name)) => {
                        drop(doc);
                        let results = crate::resolve::refs_to(
                            &self.files,
                            Some(&self.module_index),
                            &crate::resolve::TargetRef {
                                name,
                                kind: crate::resolve::TargetKind::HashKeyOfClass(class_name),
                            },
                            crate::resolve::RoleMask::VISIBLE,
                        );
                        return Ok(refs_to_locations(results));
                    }
                    _ => {
                        // Fall back to single-file (e.g. keys owned by a lexical
                        // variable — scope-local by definition).
                        let refs = symbols::find_references(
                            &doc.analysis, pos, uri, &doc.tree, &doc.text,
                        );
                        return Ok(if refs.is_empty() { None } else { Some(refs) });
                    }
                }
            }
            Some(RenameKind::Variable) | None => {
                // Variables are lexical — single-file only.
                let refs = symbols::find_references(
                    &doc.analysis, pos, uri, &doc.tree, &doc.text,
                );
                return Ok(if refs.is_empty() { None } else { Some(refs) });
            }
            Some(RenameKind::Handler { owner, name }) => TargetRef {
                name: name.clone(),
                kind: TargetKind::Handler { owner, name },
            },
        };

        // Cross-file walk: workspace + open + deps.
        drop(doc); // release the DashMap read lock before the resolve walk
        let results = refs_to(
            &self.files,
            Some(&self.module_index),
            &target,
            RoleMask::VISIBLE,
        );
        Ok(refs_to_locations(results))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let doc = match self.files.get_open(&params.text_document.uri) {
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
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let point = symbols::position_to_point(pos);
        let rename_kind = doc.analysis.rename_kind_at(point);

        match rename_kind {
            Some(crate::file_analysis::RenameKind::Variable) => {
                // Single-file only — lexical scope doesn't cross files
                Ok(symbols::rename(&doc.analysis, pos, uri, new_name, Some(&doc.tree), Some(&doc.text)))
            }
            Some(crate::file_analysis::RenameKind::Handler { .. }) => {
                // Cross-file handler rename not yet wired — fall back to
                // single-file for the moment. This will edit the Handler
                // symbol + every DispatchCall ref in the current file.
                Ok(symbols::rename(&doc.analysis, pos, uri, new_name, Some(&doc.tree), Some(&doc.text)))
            }
            Some(crate::file_analysis::RenameKind::Function(ref name))
            | Some(crate::file_analysis::RenameKind::Method(ref name))
            | Some(crate::file_analysis::RenameKind::Package(ref name))
            | Some(crate::file_analysis::RenameKind::HashKey(ref name)) => {
                let rename_fn: fn(&FileAnalysis, &str, &str) -> Vec<(crate::file_analysis::Span, String)> = match &rename_kind {
                    Some(crate::file_analysis::RenameKind::Function(_)) => FileAnalysis::rename_sub,
                    Some(crate::file_analysis::RenameKind::Package(_)) => FileAnalysis::rename_package,
                    Some(crate::file_analysis::RenameKind::Method(_)) => FileAnalysis::rename_sub,
                    Some(crate::file_analysis::RenameKind::HashKey(_)) => {
                        // Hash keys: single-file for now
                        return Ok(symbols::rename(&doc.analysis, pos, uri, new_name, Some(&doc.tree), Some(&doc.text)));
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

                // Iterate all file-backed analyses (open + workspace, deduped).
                self.files.for_each_analysis(|key, analysis| {
                    let entry_uri = match key {
                        FileKey::Url(u) => u,
                        FileKey::Path(p) => Url::from_file_path(&p).unwrap_or_else(|_| {
                            Url::parse(&format!("file://{}", p.display())).unwrap()
                        }),
                    };
                    collect(entry_uri, analysis);
                });

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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let items = symbols::completion_items(
            &doc.analysis,
            &doc.tree,
            &doc.text,
            pos,
            &self.module_index,
            Some(doc.stable_outline.package_lines()),
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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

        self.files.for_each_analysis(|key, analysis| {
            let uri = match key {
                FileKey::Url(u) => u,
                FileKey::Path(p) => Url::from_file_path(&p).unwrap_or_else(|_| {
                    Url::parse(&format!("file://{}", p.display())).unwrap()
                }),
            };
            for sym in &analysis.symbols {
                if sym.name.to_lowercase().contains(&query) {
                    if let Some(info) = symbols::symbol_to_workspace_info(sym, uri.clone()) {
                        results.push(info);
                    }
                }
            }
            // Plugin namespaces — match on both id and kind so users
            // can find "the minion tasks in this workspace" via either
            // "minion" or "tasks".
            for ns in &analysis.plugin_namespaces {
                let hay = format!("{} {}", ns.id.to_lowercase(), ns.kind.to_lowercase());
                if hay.contains(&query) {
                    results.push(symbols::plugin_namespace_to_workspace_info(ns, uri.clone()));
                }
            }
        });

        if results.is_empty() {
            Ok(None)
        } else {
            Ok(Some(results))
        }
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let files = Arc::clone(&self.files);
        tokio::task::spawn_blocking(move || {
            for change in params.changes {
                let path = match change.uri.to_file_path() {
                    Ok(p) => p,
                    Err(_) => continue,
                };
                match change.typ {
                    FileChangeType::DELETED => {
                        files.remove_workspace(&path);
                    }
                    _ => {
                        // Re-index the file (created or changed)
                        if let Ok(source) = std::fs::read_to_string(&path) {
                            let mut parser = crate::module_resolver::create_parser();
                            if let Some(tree) = parser.parse(&source, None) {
                                let analysis = crate::builder::build(&tree, source.as_bytes());
                                files.insert_workspace(path, analysis);
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
        let doc = match self.files.get_open(uri) {
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
        let doc = match self.files.get_open(uri) {
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
