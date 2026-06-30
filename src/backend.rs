use std::path::PathBuf;
use std::sync::Arc;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::{notification, request};
use tower_lsp::{Client, LanguageServer};

use crate::file_store::{FileKey, FileStore};
use crate::module_index::ModuleIndex;
use crate::symbols;

/// Pack-language completion: member access (sentinel reparse → receiver
/// span → type → members) with an in-scope-symbol fallback. Shared by the
/// LSP completion handler and the CLI/--batch mirror so the editor and
/// gold agree. Perl completion stays in `cursor_context`.
pub fn pack_completion(
    analysis: &crate::file_analysis::FileAnalysis,
    source: &str,
    tree: &tree_sitter::Tree,
    point: tree_sitter::Point,
    language: &str,
    path: Option<&std::path::Path>,
    module_index: &ModuleIndex,
) -> Vec<CompletionItem> {
    if let Some(driver) =
        crate::language_driver::LanguageRegistry::with_enabled().for_id(language)
    {
        // ONE lookup: the driver carries the LangPack (the cursor path's
        // config), `None` for native Perl. No parallel `lang_cfg` registry.
        if let Some(lang_pack) = driver.lang_pack() {
            // Cross-file resolves against THIS language's sub-index (its
            // own cache — no cross-language overlap), falling back to the
            // hub when none is attached.
            let pack = module_index.pack_index(language);
            let xidx: &dyn crate::file_analysis::CrossFileLookup =
                pack.as_deref().map_or(module_index, |i| i);
            let cursor = crate::cursor_sentinel::point_to_byte(source, point);
            let mut parser = driver.make_parser();
            if let Some(ctx) = crate::cursor_sentinel::member_completion_ctx_incremental(
                &mut parser, &lang_pack, source, tree, cursor, analysis, Some(xidx),
            ) {
                if let Some(class) =
                    ctx.receiver_type.and_then(|ty| ty.class_name().map(|s| s.to_string()))
                {
                    // Mode A: the member items carry the operator-swap edit
                    // (`p.` → `p->`) when the receiver's pointer depth wants
                    // a different operator than was typed. The diagnostic
                    // path (Mode B) is the universal fallback.
                    if let Some(items) =
                        symbols::member_completion_for_class(analysis, &class, xidx, ctx.op_fix)
                    {
                        return items;
                    }
                }
            }
        }
    }
    let mut items = symbols::in_scope_completion(analysis, point);
    macro_completion(source, point, language, path, &mut items);
    items
}

/// Identifier-context macro completion (C preprocessor): the `#define`s
/// reachable through `#include`s — the API surface (perl5: `Newx`/`SvPV`).
/// The file's OWN `#define`s are already symbols (in `items`); this adds the
/// cross-file ones. Prefix-filtered server-side (a macro-heavy include
/// closure reaches thousands — perl.h alone is ~2000), and the header cache
/// is warm from analyze, so the re-gather is cheap.
fn macro_completion(
    source: &str,
    point: tree_sitter::Point,
    language: &str,
    path: Option<&std::path::Path>,
    items: &mut Vec<CompletionItem>,
) {
    if language != "cpp" {
        return; // only C/C++ have a preprocessor
    }
    let Some(p) = path else { return };
    let reg = crate::language_driver::LanguageRegistry::with_enabled();
    let Some(driver) = reg.for_id(language) else { return };
    let cursor = crate::cursor_sentinel::point_to_byte(source, point);
    let bytes = source.as_bytes();
    let mut start = cursor;
    while start > 0 && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    let prefix = &source[start..cursor];
    if prefix.is_empty() {
        return; // no bare-cursor dump of the whole macro table
    }
    let mut parser = driver.make_parser();
    let macros = crate::cpp_reparse::included_macros(p, source, &mut parser);
    let seen: std::collections::HashSet<String> =
        items.iter().map(|i| i.label.clone()).collect();
    for (name, m) in macros.iter() {
        if !name.starts_with(prefix) || seen.contains(name) {
            continue;
        }
        let (kind, detail) = match &m.params {
            Some(params) => (
                CompletionItemKind::FUNCTION,
                format!("#define {}({})", name, params.join(", ")),
            ),
            None => (
                CompletionItemKind::CONSTANT,
                format!("#define {} {}", name, m.body.trim()),
            ),
        };
        items.push(CompletionItem {
            label: name.clone(),
            kind: Some(kind),
            detail: Some(detail),
            ..Default::default()
        });
    }
}

pub struct Backend {
    client: Client,
    files: Arc<FileStore>,
    module_index: Arc<ModuleIndex>,
    /// Opt-in `unresolved-dispatch` diagnostic toggle, set from
    /// `initializationOptions.diagnostics.unresolvedDispatch`. Shared with the
    /// resolver refresh callback (which also publishes diagnostics), hence the
    /// atomic. Default off — QA/plugin-author channel.
    unresolved_dispatch: Arc<std::sync::atomic::AtomicBool>,
    /// Per-document edit generation. Each `did_change` bumps it; a debounced
    /// rebuild task only proceeds if its captured generation is still the
    /// latest — so a burst of keystrokes triggers ONE analysis (~0.7s on a
    /// big macro-heavy C file) after typing settles, not one per keystroke.
    /// Pack languages only; Perl rebuilds synchronously (cheap).
    change_gen: Arc<dashmap::DashMap<Url, u64>>,
    /// `initializationOptions.rename.overrideScope`: when set to `"dispatch"`,
    /// method-override references/rename use the precise dispatch scope instead
    /// of the default whole-hierarchy family. Set once at init.
    dispatch_override: Arc<std::sync::atomic::AtomicBool>,
}

impl Backend {
    fn diagnostic_options(&self) -> symbols::DiagnosticOptions {
        symbols::DiagnosticOptions {
            unresolved_dispatch: self
                .unresolved_dispatch
                .load(std::sync::atomic::Ordering::Relaxed),
        }
    }

    /// The configured method-override fan-out scope for references + rename.
    fn override_scope(&self) -> crate::resolve::OverrideScope {
        if self.dispatch_override.load(std::sync::atomic::Ordering::Relaxed) {
            crate::resolve::OverrideScope::Dispatch
        } else {
            crate::resolve::OverrideScope::Hierarchy
        }
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let files: Arc<FileStore> = Arc::new(FileStore::new());

        // We need Arc<ModuleIndex> so the refresh callback can access it.
        // Two-phase init: create ModuleIndex whose refresh callback references
        // a later-set Arc<ModuleIndex>, then wire up the Arc.
        let unresolved_dispatch = Arc::new(std::sync::atomic::AtomicBool::new(false));

        let refresh_client = client.clone();
        let refresh_files = Arc::clone(&files);
        let refresh_unresolved_dispatch = Arc::clone(&unresolved_dispatch);

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
            let unresolved_dispatch = Arc::clone(&refresh_unresolved_dispatch);
            tokio_handle.spawn(async move {
                let module_index = match holder.get() {
                    Some(idx) => idx,
                    None => return,
                };
                // Collect (uri, diagnostics) first without holding the store lock
                // across the await — publishing is async and could deadlock.
                let mut pending: Vec<(Url, Vec<Diagnostic>)> = Vec::new();
                let options = symbols::DiagnosticOptions {
                    unresolved_dispatch: unresolved_dispatch
                        .load(std::sync::atomic::Ordering::Relaxed),
                };
                files.for_each_open_mut(|uri, doc| {
                    let diagnostics = if doc.language == "perl" {
                        doc.analysis.enrich_imported_types_with_keys(Some(module_index.as_ref()));
                        symbols::collect_diagnostics(&doc.analysis, module_index, options)
                    } else {
                        symbols::pack_member_op_diagnostics(&doc.analysis)
                    };
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
            unresolved_dispatch,
            change_gen: Arc::new(dashmap::DashMap::new()),
            dispatch_override: Arc::new(std::sync::atomic::AtomicBool::new(false)),
        }
    }

    /// After a debounce, rebuild the pack analysis for `uri` OFF the document
    /// lock (snapshot text → `spawn_blocking` build → write back) + publish
    /// diagnostics — but only while `generation` is still the latest edit, so
    /// a burst of keystrokes collapses to ONE rebuild after typing settles.
    fn spawn_debounced_rebuild(&self, uri: Url, generation: u64) {
        let files = Arc::clone(&self.files);
        let module_index = Arc::clone(&self.module_index);
        let client = self.client.clone();
        let change_gen = Arc::clone(&self.change_gen);
        tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(150)).await;
            let is_latest = || change_gen.get(&uri).map(|v| *v) == Some(generation);
            if !is_latest() {
                return;
            }
            // Snapshot the latest text off the lock; build on a blocking
            // thread so the ~0.7s analysis never stalls completion/hover.
            let Some((text, path, language)) = files
                .get_open(&uri)
                .map(|d| (d.text.clone(), d.path.clone(), d.language))
            else {
                return;
            };
            let analysis = tokio::task::spawn_blocking(move || {
                crate::language_driver::LanguageRegistry::with_enabled()
                    .for_id(language)
                    .map(|d| d.analyze_with_path(&text, path.as_deref()))
            })
            .await
            .ok()
            .flatten();
            let Some(analysis) = analysis else {
                return;
            };
            if !is_latest() {
                return; // a newer keystroke superseded this build
            }
            for imp in &analysis.imports {
                module_index.request_resolve(&imp.module_name);
            }
            for parents in analysis.package_parents.values() {
                for parent in parents {
                    module_index.request_resolve(parent);
                }
            }
            if let Some(mut doc) = files.get_open_mut(&uri) {
                doc.apply_rebuilt(analysis);
            }
            let diags = files
                .get_open(&uri)
                .map(|doc| symbols::pack_member_op_diagnostics(&doc.analysis));
            if let Some(diags) = diags {
                client.publish_diagnostics(uri, diags, None).await;
            }
        });
    }


    /// A bare identifier at the cursor that names a CROSS-FILE top-level
    /// symbol — a macro (`OP_NULL`, `BASEOP`), enum constant, global, or
    /// type. Resolves off the RAW word, so it works even when the macro
    /// expanded AWAY in the analysis (the token isn't a captured ref).
    /// Returns (target uri, def span, the def's source line for hover).
    fn pack_xfile_word_at(
        &self,
        doc: &crate::document::Document,
        pos: Position,
        idx: &dyn crate::file_analysis::CrossFileLookup,
    ) -> Option<(Option<Url>, crate::file_analysis::Span, String)> {
        let cursor =
            crate::cursor_sentinel::point_to_byte(&doc.text, symbols::position_to_point(pos));
        let bytes = doc.text.as_bytes();
        let is_id = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
        let mut start = cursor;
        while start > 0 && is_id(bytes[start - 1]) {
            start -= 1;
        }
        let mut end = cursor;
        while end < bytes.len() && is_id(bytes[end]) {
            end += 1;
        }
        if start == end {
            return None;
        }
        let word = &doc.text[start..end];
        // Pick the best DEFINITION among same-named symbols (a `#define X` plus
        // its raw usages): prefer the `#define` line, else the earliest.
        let pick = |analysis: &crate::file_analysis::FileAnalysis, src: &str| {
            let lines: Vec<&str> = src.lines().collect();
            let line_of =
                |s: &crate::file_analysis::Symbol| lines.get(s.selection_span.start.row).copied();
            let cands: Vec<&crate::file_analysis::Symbol> =
                analysis.symbols.iter().filter(|s| s.name == word).collect();
            let sym = cands
                .iter()
                .find(|s| line_of(s).is_some_and(|l| l.trim_start().starts_with("#define")))
                .or_else(|| cands.iter().min_by_key(|s| s.selection_span.start.row))
                .copied()?;
            Some((sym.selection_span, line_of(sym).map(|l| l.trim().to_string()).unwrap_or_default()))
        };
        // A macro defined in THIS file (`BASEOP` in op.h) — the usage isn't a
        // captured ref, so find_definition missed it, but the def symbol is
        // local. Fall back to the cross-file index for usages from elsewhere.
        if let Some((span, line)) = pick(&doc.analysis, &doc.text) {
            return Some((None, span, line));
        }
        let cached = idx.get_cached(word)?;
        let text = std::fs::read_to_string(&cached.path).ok()?;
        let (span, line) = pick(&cached.analysis, &text)?;
        Some((Url::from_file_path(&cached.path).ok(), span, line))
    }

    fn enrich_analysis(&self, uri: &Url) {
        if let Some(mut doc) = self.files.get_open_mut(uri) {
            // enrichment is Perl-flavored (imported-type/hash-key keys);
            // pack languages skip it.
            if doc.language == "perl" {
                doc.analysis.enrich_imported_types_with_keys(Some(&*self.module_index));
            }
        }
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        self.enrich_analysis(uri);
        let options = self.diagnostic_options();
        let diagnostics = match self.files.get_open(uri) {
            Some(doc) if doc.language == "perl" => {
                symbols::collect_diagnostics(&doc.analysis, &self.module_index, options)
            }
            // Pack languages stay honest-silent EXCEPT the member-access
            // operator-mismatch check, which is high-confidence (it fires
            // only when a recorded site's typed operator disagrees with its
            // receiver's known pointer depth — no calibration guesswork).
            Some(doc) => symbols::pack_member_op_diagnostics(&doc.analysis),
            None => vec![],
        };
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

/// Build a WorkspaceEdit by finding every reference to `target` in the editable
/// workspace and replacing each ref's span with `new_name`.
///
/// Rename shares the same resolution path as references — both go through
/// `refs_to(EDITABLE)`. This is the single place that decides which spans get
/// edited, so rename and references can't diverge on which call sites qualify.
/// `refs_to` handles inheritance fan-out for Method targets (via
/// `method_rename_chain`) so callers on child classes of a base method are
/// included without any special-casing here.
fn rename_via_refs_to(
    files: &FileStore,
    module_index: Option<&dyn crate::file_analysis::CrossFileLookup>,
    target: &crate::resolve::TargetRef,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    use crate::resolve::{refs_to, RoleMask};

    // Rename must not touch deps — EDITABLE stops at workspace/open files.
    let locations = refs_to(files, module_index, target, RoleMask::EDITABLE);
    locations_to_workspace_edit(locations, new_name)
}

/// `(RefLocation, text)` pairs → one `WorkspaceEdit` (per-member texts).
fn edit_pairs_to_workspace_edit(
    edits: Vec<(crate::resolve::RefLocation, String)>,
) -> Option<WorkspaceEdit> {
    if edits.is_empty() {
        return None;
    }
    let mut all_changes: std::collections::HashMap<Url, Vec<TextEdit>> =
        std::collections::HashMap::new();
    for (loc, text) in edits {
        if let Some(uri) = loc.to_url() {
            all_changes.entry(uri).or_default().push(TextEdit {
                range: symbols::span_to_range(loc.span),
                new_text: text,
            });
        }
    }
    if all_changes.is_empty() {
        None
    } else {
        Some(WorkspaceEdit { changes: Some(all_changes), ..Default::default() })
    }
}

/// `RefLocation`s → one `WorkspaceEdit` writing `new_name` at every span.
/// Callers guarantee every span covers exactly the renamable token
/// (bare-name spans for groups, selection/name spans from `refs_to`).
fn locations_to_workspace_edit(
    locations: Vec<crate::resolve::RefLocation>,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    if locations.is_empty() {
        return None;
    }
    let mut all_changes: std::collections::HashMap<Url, Vec<TextEdit>> =
        std::collections::HashMap::new();
    for loc in locations {
        // Skip non-rewritable sites (a const-folded event name spelled by a
        // variable): it's a reference, not a literal to rewrite. References
        // (`refs_to_locations`) keeps it; only rename drops it.
        if !loc.rewritable {
            continue;
        }
        if let Some(uri) = loc.to_url() {
            all_changes.entry(uri).or_default().push(TextEdit {
                range: symbols::span_to_range(loc.span),
                new_text: new_name.to_string(),
            });
        }
    }
    if all_changes.is_empty() {
        None
    } else {
        Some(WorkspaceEdit {
            changes: Some(all_changes),
            ..Default::default()
        })
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
        // Same root drives repo-local `.perl-lsp/` plugin discovery, so the
        // plugin set and the per-project cache key can't disagree.
        crate::plugin::rhai_host::set_workspace_root(root);

        // Opt-in diagnostics from `initializationOptions.diagnostics`.
        // `{ "diagnostics": { "unresolvedDispatch": true } }` enables the
        // QA/plugin-author `unresolved-dispatch` channel; absent = off.
        if let Some(opts) = &params.initialization_options {
            let on = opts
                .get("diagnostics")
                .and_then(|d| d.get("unresolvedDispatch"))
                .and_then(|v| v.as_bool())
                .unwrap_or(false);
            self.unresolved_dispatch
                .store(on, std::sync::atomic::Ordering::Relaxed);

            // `{ "rename": { "overrideScope": "dispatch" } }` opts into the
            // precise method-override scope; absent / "hierarchy" = the default
            // whole-family refactor.
            let dispatch = opts
                .get("rename")
                .and_then(|r| r.get("overrideScope"))
                .and_then(|v| v.as_str())
                .map(|s| matches!(crate::resolve::OverrideScope::from_option(s), crate::resolve::OverrideScope::Dispatch))
                .unwrap_or(false);
            self.dispatch_override
                .store(dispatch, std::sync::atomic::Ordering::Relaxed);
        }

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
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                linked_editing_range_provider: Some(LinkedEditingRangeServerCapabilities::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    // Union of every served language's trigger chars — Perl
                    // sigils/`->`/`{`, plus a pack language's `.`/`::` etc.
                    // A perl-only build is byte-identical to the old list.
                    trigger_characters: Some(
                        crate::language_driver::LanguageRegistry::with_enabled().trigger_chars(),
                    ),
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
                    // Pack languages → per-language sub-indexes (separate
                    // caches, own modules-{lang}.db) attached to the hub, so
                    // cross-file resolves in the editor too. Additive; Perl
                    // indexing unchanged.
                    crate::module_resolver::index_pack_languages(
                        &root_path,
                        Some(root_uri.as_str()),
                        &module_index,
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
        let Some(change) = params.content_changes.into_iter().next() else {
            return;
        };
        let language = match self.files.get_open(&uri) {
            Some(doc) => doc.language,
            None => return,
        };
        // Perl rebuilds synchronously — its build is cheap. Pack languages
        // (macro-heavy C: ~0.7s/rebuild) update the tree/text immediately so
        // position features stay live, and DEBOUNCE the analysis so a burst
        // of keystrokes pays one rebuild after typing settles, not one each.
        if language == "perl" {
            if let Some(mut doc) = self.files.get_open_mut(&uri) {
                doc.update(change.text);
                for imp in &doc.analysis.imports {
                    self.module_index.request_resolve(&imp.module_name);
                }
                for parents in doc.analysis.package_parents.values() {
                    for parent in parents {
                        self.module_index.request_resolve(parent);
                    }
                }
            }
            self.publish_diagnostics(&uri).await;
            return;
        }
        if let Some(mut doc) = self.files.get_open_mut(&uri) {
            doc.update_text_only(change.text);
        }
        let generation = {
            let mut e = self.change_gen.entry(uri.clone()).or_insert(0);
            *e += 1;
            *e
        };
        self.spawn_debounced_rebuild(uri, generation);
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
        // cpp/pack functions live in the per-language sub-index; route there
        // so cross-file function goto-def resolves (Perl uses the hub).
        let pack = (doc.language != "perl")
            .then(|| self.module_index.pack_index(doc.language))
            .flatten();
        let idx = pack.as_deref().unwrap_or(&self.module_index);
        if let Some(resp) = symbols::find_definition(&doc.analysis, pos, uri, idx) {
            return Ok(Some(resp));
        }
        // Member access (`obj->field`) now flows through `find_definition`
        // above: cpp mints a `MethodCall` ref core resolves like any other.
        if doc.language != "perl" {
            // A macro / enum-constant / global usage (`OP_NULL`, `BASEOP`) —
            // the raw word names a local-or-cross-file symbol.
            if let Some((target, span, _)) = self.pack_xfile_word_at(&doc, pos, idx) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target.unwrap_or_else(|| uri.clone()),
                    range: symbols::span_to_range(span),
                })));
            }
        }
        Ok(None)
    }

    async fn goto_implementation(
        &self,
        params: request::GotoImplementationParams,
    ) -> Result<Option<request::GotoImplementationResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        use crate::resolve::{implementations_of, resolve_symbol, ResolvedTarget};
        let point = symbols::position_to_point(pos);
        let target = match resolve_symbol(&doc.analysis, point, Some(&*self.module_index)) {
            Some(ResolvedTarget::Target(t)) => t,
            // Groups (field projections) and lexicals have no
            // descendant-implementation semantics.
            _ => return Ok(None),
        };
        let results = implementations_of(&doc.analysis, Some(&*self.module_index), &target);
        drop(doc);
        Ok(refs_to_locations(results).map(GotoDefinitionResponse::Array))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        use crate::resolve::{group_refs, refs_to, resolve_symbol_scoped, ResolvedTarget};

        let point = symbols::position_to_point(pos);
        let target = match resolve_symbol_scoped(&doc.analysis, point, Some(&*self.module_index), self.override_scope()) {
            Some(ResolvedTarget::Target(t)) => t,
            Some(ResolvedTarget::Group { local_spans, pinned_spans, members }) => {
                let origin = FileKey::Url(uri.clone());
                drop(doc);
                let results = group_refs(
                    &self.files,
                    Some(&*self.module_index),
                    &origin,
                    &local_spans,
                    &pinned_spans,
                    &members,
                    None,
                );
                return Ok(refs_to_locations(results));
            }
            // Lexical / unowned — single-file references.
            Some(ResolvedTarget::Local) | None => {
                let refs = symbols::find_references(
                    &doc.analysis, pos, uri, Some(&*self.module_index),
                );
                return Ok(if refs.is_empty() { None } else { Some(refs) });
            }
        };

        // Cross-file walk. Scope to editable space when the target is a
        // project symbol so "find references" never scans CPAN; widen to
        // VISIBLE only for dependency-defined targets. See `references_mask_for`.
        drop(doc); // release the DashMap read lock before the resolve walk
        let mask = crate::resolve::references_mask_for(
            &self.files,
            Some(&*self.module_index),
            &target,
        );
        let results = refs_to(&self.files, Some(&*self.module_index), &target, mask);
        Ok(refs_to_locations(results))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        use crate::resolve::{resolve_symbol_scoped, ResolvedTarget};
        let doc = match self.files.get_open(&params.text_document.uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let point = symbols::position_to_point(params.position);
        // Only offer a rename box where `rename` would actually produce edits.
        // Accepting on any `symbol_at`/`ref_at` hit is a UX trap: positions like
        // `@_` or an ownerless constructor key resolve to nothing renameable, so
        // the user gets a box that silently no-ops. Mirror the rename handler's
        // branching, probing the single-file `rename_at` for the kinds it routes
        // there — which is why this gate tracks new single-file renameables (a
        // lexical hash key) automatically, with no change here.
        let renameable = match resolve_symbol_scoped(
            &doc.analysis,
            point,
            Some(&*self.module_index),
            self.override_scope(),
        ) {
            Some(ResolvedTarget::Target(t)) if t.supports_cross_file_rename() => true,
            Some(ResolvedTarget::Group { .. }) => true,
            Some(_) => doc.analysis.rename_at(point, "x").is_some_and(|e| !e.is_empty()),
            None => false,
        };
        if !renameable {
            return Ok(None);
        }
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
        use crate::resolve::{resolve_symbol_scoped, ResolvedTarget};

        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = &params.new_name;
        if !crate::resolve::is_valid_rename_name(new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "rename: the new name must not be empty or whitespace",
            ));
        }
        let doc = match self.files.get_open(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let point = symbols::position_to_point(pos);
        match resolve_symbol_scoped(&doc.analysis, point, Some(&*self.module_index), self.override_scope()) {
            Some(ResolvedTarget::Target(target)) if target.supports_cross_file_rename() => {
                drop(doc);
                Ok(rename_via_refs_to(&self.files, Some(&*self.module_index), &target, new_name))
            }
            Some(ResolvedTarget::Group { local_spans, pinned_spans, members }) => {
                // Every spelling of the group, everywhere editable; each
                // member carries its own replacement (bare vs re-derived
                // affixed accessor names).
                let origin = FileKey::Url(uri.clone());
                drop(doc);
                let bare_new = new_name.trim_start_matches(['$', '@', '%']);
                let edits = crate::resolve::group_rename_edits(
                    &self.files,
                    Some(&*self.module_index),
                    &origin,
                    &local_spans,
                    &pinned_spans,
                    &members,
                    bare_new,
                );
                Ok(edit_pairs_to_workspace_edit(edits))
            }
            // Lexical variables, hash keys, handlers: single-file rename
            // (policy lives on `TargetRef::supports_cross_file_rename`).
            Some(_) => {
                Ok(symbols::rename(&doc.analysis, pos, uri, new_name))
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
        // Perl's hover renderer is Perl-specific; pack languages get a
        // language-agnostic declaration-line hover.
        if doc.language != "perl" {
            // Route cross-file (function hover) to the per-language sub-index.
            let pack = self.module_index.pack_index(doc.language);
            let xidx: &dyn crate::file_analysis::CrossFileLookup =
                pack.as_deref().map_or(&*self.module_index, |i| i);
            if let Some(h) = symbols::pack_hover(
                &doc.analysis,
                &doc.text,
                symbols::position_to_point(pos),
                doc.language,
                Some(xidx),
            ) {
                return Ok(Some(h));
            }
            // Member access (`obj->field`) is handled inside `pack_hover`
            // above — it resolves the MethodCall ref like goto-def does.
            // A macro / enum-constant / global (`OP_NULL`, `BASEOP`) — show
            // its cross-file definition line.
            if let Some((_, _, line)) = self.pack_xfile_word_at(&doc, pos, xidx) {
                if !line.is_empty() {
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```{}\n{}\n```", doc.language, line),
                        }),
                        range: None,
                    }));
                }
            }
            return Ok(None);
        }
        Ok(symbols::hover_info(
            &doc.analysis,
            &doc.text,
            pos,
            &self.module_index,
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
        if doc.language != "perl" {
            let items = pack_completion(
                &doc.analysis,
                &doc.text,
                &doc.tree,
                symbols::position_to_point(pos),
                doc.language,
                doc.path.as_deref(),
                &self.module_index,
            );
            return Ok((!items.is_empty()).then_some(CompletionResponse::Array(items)));
        }
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
        if doc.language != "perl" {
            return Ok(None); // Perl cursor-context handler
        }
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
        let highlights = symbols::document_highlights(&doc.analysis, pos, Some(&*self.module_index));
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
        if doc.language != "perl" {
            return Ok(None); // tree-shape handler, Perl-tuned for v1
        }
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
        // Copy the source out and release the DashMap guard before awaiting
        // perltidy: holding a shard read lock across the await deadlocks any
        // concurrent didChange (which needs the write lock) on the same file.
        let source = match self.files.get_open(uri) {
            Some(doc) => doc.text.clone(),
            None => return Ok(None),
        };

        // Shell out to perltidy
        let output = match run_perltidy(source.clone()).await {
            Ok(o) => o,
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
        if formatted == source {
            return Ok(None);
        }

        // Replace entire document
        let line_count = source.lines().count();
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
        // Copy the source out and release the DashMap guard before awaiting
        // perltidy — see `formatting` for why holding it across the await
        // deadlocks concurrent didChange on the same file.
        let source = match self.files.get_open(uri) {
            Some(doc) => doc.text.clone(),
            None => return Ok(None),
        };

        // Extract lines for the range
        let start_line = params.range.start.line as usize;
        let end_line = params.range.end.line as usize;
        let lines: Vec<&str> = source.lines().collect();
        if start_line >= lines.len() { return Ok(None); }
        let end = (end_line + 1).min(lines.len());
        let range_text: String = lines[start_line..end].join("\n") + "\n";

        // Shell out to perltidy on the range
        let output = match run_perltidy(range_text.clone()).await {
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

        match symbols::linked_editing_ranges(&doc.analysis, pos, Some(&*self.module_index)) {
            Some(ranges) => Ok(Some(LinkedEditingRanges {
                ranges,
                word_pattern: None,
            })),
            None => Ok(None),
        }
    }
}

/// Run perltidy over `input`, returning its captured output.
///
/// `kill_on_drop` so a cancelled formatting request (the editor sends
/// `$/cancelRequest`, tower-lsp aborts the handler future) reaps perltidy
/// instead of leaving a `<defunct>` zombie (#80). The stdin write runs in its
/// own task concurrently with `wait_with_output`'s stdout drain so we never
/// block writing stdin while perltidy is blocked writing stdout.
async fn run_perltidy(input: String) -> std::io::Result<std::process::Output> {
    use tokio::io::AsyncWriteExt;

    let mut child = tokio::process::Command::new("perltidy")
        .arg("--standard-output")
        .arg("--standard-error-output")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true)
        .spawn()?;

    let stdin = child.stdin.take();
    let writer = tokio::spawn(async move {
        if let Some(mut stdin) = stdin {
            let _ = stdin.write_all(input.as_bytes()).await;
            // drop closes stdin, signalling EOF to perltidy
        }
    });

    let output = child.wait_with_output().await;
    let _ = writer.await;
    output
}
