use std::collections::HashMap;
use tower_lsp::lsp_types::*;
use tree_sitter::{Point, Tree};

use crate::cursor_context::{self, CursorContext};
use crate::file_analysis::{
    contains_point, format_inferred_type, CompletionCandidate, FileAnalysis, FoldKind,
    HandlerOwner, InferredType, OutlineSymbol, ParamInfo, RefKind, Span,
    SymKind as FaSymKind, SymbolDetail, PRIORITY_AUTO_ADD_QW, PRIORITY_BARE_IMPORT,
    PRIORITY_EXPLICIT_IMPORT, PRIORITY_UNIMPORTED,
};
use crate::module_index::{CachedModule, ModuleIndex, SubInfo};
use std::sync::Arc;

// ---- Coordinate conversion ----

fn point_to_position(p: Point) -> Position {
    Position {
        line: p.row as u32,
        character: p.column as u32,
    }
}

pub fn position_to_point(pos: Position) -> Point {
    Point::new(pos.line as usize, pos.character as usize)
}

pub fn span_to_range(span: Span) -> Range {
    Range {
        start: point_to_position(span.start),
        end: point_to_position(span.end),
    }
}

// ---- Symbol conversion ----

fn fa_sym_kind_to_lsp(kind: &FaSymKind) -> SymbolKind {
    match kind {
        FaSymKind::Sub => SymbolKind::FUNCTION,
        FaSymKind::Method => SymbolKind::METHOD,
        FaSymKind::Variable | FaSymKind::Field => SymbolKind::VARIABLE,
        FaSymKind::Package => SymbolKind::NAMESPACE,
        FaSymKind::Class => SymbolKind::CLASS,
        FaSymKind::Module => SymbolKind::MODULE,
        FaSymKind::HashKeyDef => SymbolKind::KEY,
        // Handler's actual LSP kind depends on the plugin's
        // `display` choice — this fallback only fires for paths
        // that don't carry detail, which is rare. Event is the
        // conservative default.
        FaSymKind::Handler => SymbolKind::EVENT,
    }
}

/// Plugin-chosen Handler display → LSP SymbolKind. Called from the
/// outline path where OutlineSymbol carries `handler_display`.
fn handler_display_to_symbol_kind(d: &crate::file_analysis::HandlerDisplay) -> SymbolKind {
    use crate::file_analysis::HandlerDisplay as H;
    match d {
        H::Event => SymbolKind::EVENT,
        H::Method => SymbolKind::METHOD,
        H::Function => SymbolKind::FUNCTION,
        H::Field => SymbolKind::FIELD,
        H::Property => SymbolKind::PROPERTY,
        H::Constant => SymbolKind::CONSTANT,
    }
}

fn handler_display_to_completion_kind(d: &crate::file_analysis::HandlerDisplay) -> CompletionItemKind {
    use crate::file_analysis::HandlerDisplay as H;
    match d {
        H::Event => CompletionItemKind::EVENT,
        H::Method => CompletionItemKind::METHOD,
        H::Function => CompletionItemKind::FUNCTION,
        H::Field => CompletionItemKind::FIELD,
        H::Property => CompletionItemKind::PROPERTY,
        H::Constant => CompletionItemKind::CONSTANT,
    }
}

#[allow(deprecated)]
fn outline_to_document_symbol(s: &OutlineSymbol) -> DocumentSymbol {
    let children: Vec<DocumentSymbol> = s.children.iter().map(outline_to_document_symbol).collect();
    // Handler's LSP kind comes from the plugin's chosen display. Falls
    // back to Event only if the outline wasn't populated with a
    // display (older analyses or non-Handler kinds).
    let kind = match s.handler_display {
        Some(ref d) => handler_display_to_symbol_kind(d),
        None => fa_sym_kind_to_lsp(&s.kind),
    };
    DocumentSymbol {
        name: s.name.clone(),
        detail: s.detail.clone(),
        kind,
        tags: None,
        deprecated: None,
        range: span_to_range(s.span),
        selection_range: span_to_range(s.selection_span),
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    }
}

// ---- Public LSP adapter functions ----

#[allow(deprecated)]
pub fn extract_symbols(analysis: &FileAnalysis) -> Vec<DocumentSymbol> {
    analysis.document_symbols()
        .iter()
        .map(outline_to_document_symbol)
        .collect()
}

#[allow(deprecated)]
pub fn symbol_to_workspace_info(sym: &crate::file_analysis::Symbol, uri: Url) -> Option<SymbolInformation> {
    use crate::file_analysis::SymKind as FaSymKind;
    // Only include significant symbols (subs, methods, packages, classes)
    match sym.kind {
        FaSymKind::Sub | FaSymKind::Method | FaSymKind::Package | FaSymKind::Class => {}
        _ => return None,
    }
    Some(SymbolInformation {
        name: sym.name.clone(),
        kind: fa_sym_kind_to_lsp(&sym.kind),
        tags: None,
        deprecated: None,
        location: Location {
            uri,
            range: span_to_range(sym.selection_span),
        },
        container_name: sym.package.clone(),
    })
}

pub fn find_definition(
    analysis: &FileAnalysis,
    pos: Position,
    uri: &Url,
    module_index: &ModuleIndex,
    tree: &Tree,
    source: &str,
) -> Option<GotoDefinitionResponse> {
    let point = position_to_point(pos);

    // Try local definition first
    if let Some(span) = analysis.find_definition(point, Some(tree), Some(source.as_bytes()), Some(module_index)) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range: span_to_range(span),
        }));
    }

    // Check if cursor is on a function call that matches an imported symbol
    if let Some(r) = analysis.ref_at(point) {
        if matches!(r.kind, RefKind::FunctionCall) {
            if let Some((import, module_path)) =
                resolve_imported_function(analysis, &r.target_name, module_index)
            {
                // Jump to the module's use statement in the current file
                // (or the .pm file if we can resolve it)
                if let Ok(module_uri) = Url::from_file_path(&module_path) {
                    // Try to get the actual definition line from the module index.
                    let def_line = module_index
                        .get_cached(&import.module_name)
                        .and_then(|cached| cached.sub_info(&r.target_name).map(|s| s.def_line()));
                    let def_range = match def_line {
                        Some(line) => Range {
                            start: Position { line, character: 0 },
                            end: Position { line, character: 0 },
                        },
                        None => Range::default(),
                    };

                    let pm_location = Location {
                        uri: module_uri,
                        range: def_range,
                    };

                    // If cursor is inside the use statement itself (on the import name),
                    // jump directly to the .pm definition — no need to also show the use stmt.
                    if contains_point(&import.span, point) {
                        return Some(GotoDefinitionResponse::Scalar(pm_location));
                    }

                    return Some(GotoDefinitionResponse::Array(vec![
                        // First: the use statement in this file
                        Location {
                            uri: uri.clone(),
                            range: span_to_range(import.span),
                        },
                        // Second: the sub definition in the .pm file
                        pm_location,
                    ]));
                }
                // Fall back to just the use statement
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: span_to_range(import.span),
                }));
            }
        }

        // Cross-file package goto-def: resolve module name via module index
        if matches!(r.kind, RefKind::PackageRef) {
            if let Some(path) = module_index.module_path_cached(&r.target_name) {
                if let Ok(module_uri) = Url::from_file_path(&path) {
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: module_uri,
                        range: Range::default(),
                    }));
                }
            }
        }

        // Cross-file DispatchCall goto-def: a `$consumer->emit('ready', ...)`
        // in one file should jump to `$producer->on('ready', sub)` in
        // another. `find_definition` above only walks the current file's
        // symbols; for DispatchCall we enumerate every cached module
        // looking for Handlers matching (owner, name). Multiple stacked
        // registrations → return all as an Array so the editor can show
        // the picker.
        if let RefKind::DispatchCall { owner: Some(owner), .. } = &r.kind {
            use crate::file_analysis::SymbolDetail;
            let mut locs: Vec<Location> = Vec::new();
            for module_name in module_index.modules_with_symbol(&r.target_name) {
                let Some(cached) = module_index.get_cached(&module_name) else { continue };
                for sym in &cached.analysis.symbols {
                    if sym.name != r.target_name { continue; }
                    if let SymbolDetail::Handler { owner: o, .. } = &sym.detail {
                        if o == owner {
                            if let Ok(module_uri) = Url::from_file_path(&cached.path) {
                                locs.push(Location {
                                    uri: module_uri,
                                    range: span_to_range(sym.selection_span),
                                });
                            }
                        }
                    }
                }
            }
            if !locs.is_empty() {
                return Some(if locs.len() == 1 {
                    GotoDefinitionResponse::Scalar(locs.into_iter().next().unwrap())
                } else {
                    GotoDefinitionResponse::Array(locs)
                });
            }
        }

        // Cross-file method goto-def: resolve inherited methods through module index
        if let RefKind::MethodCall { ref invocant, ref invocant_span, .. } = r.kind {
            use crate::file_analysis::MethodResolution;
            let class_name = analysis.resolve_method_invocant_public(
                invocant, invocant_span, r.scope, point, Some(tree), Some(source.as_bytes()), Some(module_index),
            );
            if let Some(ref cn) = class_name {
                if let Some(MethodResolution::CrossFile { ref class }) = analysis.resolve_method_in_ancestors(cn, &r.target_name, Some(module_index)) {
                    if let Some(cached) = module_index.get_cached(class) {
                        if let Some(sub_info) = cached.sub_info(&r.target_name) {
                            if let Ok(module_uri) = Url::from_file_path(&cached.path) {
                                let line = sub_info.def_line();
                                let def_range = Range {
                                    start: Position { line, character: 0 },
                                    end: Position { line, character: 0 },
                                };
                                return Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: module_uri,
                                    range: def_range,
                                }));
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

pub fn find_references(analysis: &FileAnalysis, pos: Position, uri: &Url, tree: &Tree, source: &str) -> Vec<Location> {
    analysis.find_references(position_to_point(pos), Some(tree), Some(source.as_bytes()))
        .into_iter()
        .map(|span| Location {
            uri: uri.clone(),
            range: span_to_range(span),
        })
        .collect()
}

pub fn rename(
    analysis: &FileAnalysis,
    pos: Position,
    uri: &Url,
    new_name: &str,
    tree: Option<&tree_sitter::Tree>,
    source: Option<&str>,
) -> Option<WorkspaceEdit> {
    let edits = analysis.rename_at(
        position_to_point(pos), new_name, tree, source.map(|s| s.as_bytes()),
    )?;

    let text_edits: Vec<TextEdit> = edits
        .into_iter()
        .map(|(span, new_text)| TextEdit {
            range: span_to_range(span),
            new_text,
        })
        .collect();

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), text_edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

fn fa_completion_kind(kind: &FaSymKind) -> CompletionItemKind {
    match kind {
        FaSymKind::Sub => CompletionItemKind::FUNCTION,
        FaSymKind::Method => CompletionItemKind::METHOD,
        FaSymKind::Variable | FaSymKind::Field => CompletionItemKind::VARIABLE,
        FaSymKind::Package => CompletionItemKind::CLASS,
        FaSymKind::Class => CompletionItemKind::CLASS,
        FaSymKind::Module => CompletionItemKind::MODULE,
        FaSymKind::HashKeyDef => CompletionItemKind::PROPERTY,
        FaSymKind::Handler => CompletionItemKind::EVENT,
    }
}

fn candidate_to_completion_item(c: CompletionCandidate) -> CompletionItem {
    let additional_text_edits = if c.additional_edits.is_empty() {
        None
    } else {
        Some(
            c.additional_edits
                .iter()
                .map(|(span, text)| TextEdit {
                    range: span_to_range(*span),
                    new_text: text.clone(),
                })
                .collect(),
        )
    };
    // `filter_text` is what LSP clients match the typed prefix against
    // when narrowing the completion list client-side. By default it's
    // the label. But when `insert_text` differs (e.g. dispatch-target
    // candidates insert `'connect'` while the label is just `connect`),
    // some clients fall back to `insert_text` for filtering — then
    // typing `c` after `(` stops matching because insert_text starts
    // with `'`. Set filter_text explicitly to the bare label so
    // client-side filtering keys on the name regardless.
    let filter_text = Some(c.label.clone());

    // Sort text places dispatch handlers ABOVE anything
    // complete_general can produce. Both default to sort_priority 0;
    // tied at "000" they interleave alphabetically (connect, fire,
    // message, wire) which makes handlers look like they're mixed
    // into noise. Prefixing with a space character ensures the
    // handler group sorts first as a block — space (0x20) < digit
    // (0x30) lexicographically. Non-handlers keep the existing
    // priority-based ordering.
    let sort_text = if matches!(c.kind, FaSymKind::Handler) {
        Some(format!(" {:03}{}", c.sort_priority, c.label))
    } else {
        Some(format!("{:03}", c.sort_priority))
    };
    let kind = if let Some(ref d) = c.display_override {
        handler_display_to_completion_kind(d)
    } else {
        fa_completion_kind(&c.kind)
    };
    CompletionItem {
        label: c.label,
        kind: Some(kind),
        detail: c.detail,
        insert_text: c.insert_text,
        filter_text,
        sort_text,
        additional_text_edits,
        ..Default::default()
    }
}

pub fn completion_items(
    analysis: &FileAnalysis,
    tree: &Tree,
    source: &str,
    pos: Position,
    module_index: &ModuleIndex,
    stable_packages: Option<&[(String, usize)]>,
) -> Vec<CompletionItem> {
    let point = position_to_point(pos);

    // Try tree-based detection first for expression-based contexts
    let ctx = cursor_context::detect_cursor_context_tree(tree, source.as_bytes(), point, analysis)
        .unwrap_or_else(|| cursor_context::detect_cursor_context(source, point, Some(analysis)));

    // Mid-string completion for plugin-emitted MethodCallRefs. When the
    // cursor sits inside the span of a MethodCallRef emitted by a plugin
    // (e.g. `->to('Users#lis|')` in mojo-routes), offer methods on the
    // target class — prefix-filtered by whatever's been typed since the
    // `#` (or the whole prefix if none). This generalizes: any plugin
    // that drops a MethodCallRef at a string span gets scoped method
    // completion for free. Runs first so it preempts the generic paths.
    if let Some(refs) = refs_at_point_matching(analysis, point, |r|
        matches!(r.kind, RefKind::MethodCall { .. })
    ) {
        for r in &refs {
            if let RefKind::MethodCall { invocant, .. } = &r.kind {
                let early = mid_string_methodref_completions(
                    analysis, module_index, invocant, source, point, r.span,
                );
                if !early.is_empty() {
                    return early;
                }
            }
        }
    }

    // Dispatch-target completions are orthogonal to the context match:
    // a user typing inside `$obj->emit(^)` is simultaneously after a `->`
    // (tree detects `Method`) and inside call args (general path would
    // apply too). Pull the call context out once, prepend handler
    // completions at arg-0, and — just as importantly — SUPPRESS the
    // global sub/module firehose at arg-N>0 so comma-triggered
    // completion inside a dispatch call doesn't dump hundreds of
    // unrelated symbols. Sig help is the right affordance past arg-0;
    // completion gets out of the way.
    let mut candidates: Vec<CompletionCandidate> = Vec::new();
    let mut suppress_firehose = false;
    if let Some(call_ctx) = cursor_context::find_call_context(tree, source.as_bytes(), point) {
        if call_ctx.is_method {
            let dispatch_class = resolve_invocant_class(
                analysis, call_ctx.invocant.as_deref(), point,
            );
            let has_any_handlers = dispatch_class.as_ref().is_some_and(|c|
                class_has_dispatch_handlers(analysis, module_index, c, &call_ctx.name)
            );

            if call_ctx.active_param == 0 && has_any_handlers {
                // arg-0 of a known dispatcher: handlers at the top,
                // suppress the global sub/module firehose that would
                // otherwise drown them.
                candidates.extend(dispatch_target_completions(
                    analysis,
                    module_index,
                    call_ctx.invocant.as_deref(),
                    &call_ctx.name,
                    point,
                    tree,
                ));
                suppress_firehose = true;
            } else if call_ctx.active_param > 0 && has_any_handlers {
                // Past arg-0 in a known dispatcher: the only sensible
                // completion is variables-in-scope (candidates for
                // passing as the next arg). Sig help handles shape
                // guidance. Short-circuit the context match entirely.
                let vars_only: Vec<CompletionCandidate> = analysis.complete_general(point)
                    .into_iter()
                    .filter(|c| matches!(c.kind, FaSymKind::Variable | FaSymKind::Field))
                    .collect();
                candidates.extend(vars_only);
                return candidates.drain(..).map(candidate_to_completion_item).collect();
            }
        }
    }

    candidates.extend::<Vec<CompletionCandidate>>(match ctx {
        CursorContext::Variable { sigil } => analysis.complete_variables(point, sigil),
        CursorContext::Method { ref invocant_type, ref invocant_text } => {
            if let Some(ref ty) = invocant_type {
                if let Some(cn) = ty.class_name() {
                    analysis.complete_methods_for_class(cn, Some(module_index))
                } else {
                    // Ref types get deref snippet completions (handled below)
                    Vec::new()
                }
            } else {
                analysis.complete_methods(invocant_text, point)
            }
        }
        CursorContext::HashKey { ref owner_type, ref var_text, ref source_sub } => {
            if let Some(ref ty) = owner_type {
                if let Some(cn) = ty.class_name() {
                    analysis.complete_hash_keys_for_class(cn, point)
                } else if let Some(ref sub_name) = source_sub {
                    analysis.complete_hash_keys_for_sub(sub_name, point)
                } else {
                    // HashRef with no source_sub from context — fall back to
                    // resolve_hash_key_owner which checks call bindings
                    analysis.complete_hash_keys(var_text, point)
                }
            } else {
                analysis.complete_hash_keys(var_text, point)
            }
        }
        CursorContext::UseStatement { ref module_prefix, in_import_list, ref module_name } => {
            if in_import_list {
                if let Some(ref name) = module_name {
                    return complete_import_list(name, module_index);
                }
            } else {
                return complete_module_names(module_prefix, module_index);
            }
            Vec::new()
        }
        CursorContext::General => {
            let mut items = Vec::new();
            // Keyval arg completions if inside a call at key position.
            // (Dispatch-target completions are handled above the match
            // regardless of context, so they apply whether tree detection
            // decides we're in Method, General, or anything else.)
            if let Some(call_ctx) =
                cursor_context::find_call_context(tree, source.as_bytes(), point)
            {
                if call_ctx.at_key_position {
                    items.extend(analysis.complete_keyval_args(
                        &call_ctx.name,
                        call_ctx.is_method,
                        call_ctx.invocant.as_deref(),
                        point,
                        &call_ctx.used_keys,
                        Some(module_index),
                    ));
                }
            }
            items.extend(analysis.complete_general(point));

            // Global sub/module firehose: useful at top-level
            // positions, harmful when we just offered dispatch
            // handlers at arg-0 (they'd drown in it). `suppress_firehose`
            // is set above when we know the cursor is at arg-0 of a
            // known dispatcher call.
            if !suppress_firehose {
                items.extend(imported_function_completions(analysis, module_index));
                items.extend(unimported_function_completions(analysis, module_index, point, stable_packages));
            }

            items
        }
    });

    let mut items: Vec<CompletionItem> = candidates
        .drain(..)
        .map(candidate_to_completion_item)
        .collect();

    // Ref-type deref snippets when completing after ->
    if let CursorContext::Method { ref invocant_type, .. } = ctx {
        if let Some(ref ty) = invocant_type {
            if !ty.is_object() {
                items.extend(ref_type_snippet_completions(ty));
            }
        }
    }

    items
}

/// Complete module names on `use` lines from resolved + @INC-scanned modules.
fn complete_module_names(prefix: &str, module_index: &ModuleIndex) -> Vec<CompletionItem> {
    let modules = module_index.complete_module_names(prefix);
    modules.into_iter().map(|(name, is_resolved)| {
        let (detail, priority) = if is_resolved {
            (Some("indexed".to_string()), 10u8)
        } else {
            (Some("available".to_string()), 50u8)
        };
        CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::MODULE),
            detail,
            sort_text: Some(format!("{:03}{}", priority, name)),
            ..Default::default()
        }
    }).collect()
}

/// Complete import list items for `use Module qw(|)`.
fn complete_import_list(module_name: &str, module_index: &ModuleIndex) -> Vec<CompletionItem> {
    let cached: Arc<CachedModule> = match module_index.get_cached(module_name) {
        Some(c) => c,
        None => return vec![CompletionItem {
            label: format!("loading {}...", module_name),
            kind: Some(CompletionItemKind::TEXT),
            detail: Some("Module is being indexed".to_string()),
            insert_text: Some(String::new()),
            sort_text: Some("999".to_string()),
            ..Default::default()
        }],
    };

    let mut items = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for name in &cached.analysis.export {
        if seen.insert(name.clone()) {
            let detail = cached.sub_info(name)
                .and_then(|s| s.return_type().cloned())
                .map(|rt| format!("@EXPORT → {}", format_inferred_type(&rt)))
                .or(Some("@EXPORT".to_string()));
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail,
                sort_text: Some(format!("010{}", name)),
                ..Default::default()
            });
        }
    }

    for name in &cached.analysis.export_ok {
        if seen.insert(name.clone()) {
            let detail = cached.sub_info(name)
                .and_then(|s| s.return_type().cloned())
                .map(|rt| format!("→ {}", format_inferred_type(&rt)));
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail,
                sort_text: Some(format!("020{}", name)),
                ..Default::default()
            });
        }
    }

    items
}

/// Returns snippet completions for ref-type dereference after `->`.
fn ref_type_snippet_completions(ty: &InferredType) -> Vec<CompletionItem> {
    match ty {
        InferredType::ArrayRef => vec![CompletionItem {
            label: "[index]".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("array dereference".to_string()),
            insert_text: Some("[$0]".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("000".to_string()),
            ..Default::default()
        }],
        InferredType::CodeRef => vec![CompletionItem {
            label: "(args)".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("code dereference".to_string()),
            insert_text: Some("($0)".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("000".to_string()),
            ..Default::default()
        }],
        InferredType::HashRef => vec![CompletionItem {
            label: "{key}".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("hash dereference".to_string()),
            insert_text: Some("{$0}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("000".to_string()),
            ..Default::default()
        }],
        _ => Vec::new(),
    }
}

pub fn hover_info(
    analysis: &FileAnalysis,
    source: &str,
    pos: Position,
    module_index: &ModuleIndex,
    tree: &Tree,
) -> Option<Hover> {
    let point = position_to_point(pos);

    // Try local hover first
    if let Some(markdown) = analysis.hover_info(point, source, Some(tree), Some(module_index)) {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: markdown,
            }),
            range: None,
        });
    }

    // Check if cursor is on an imported function call
    if let Some(r) = analysis.ref_at(point) {
        if matches!(r.kind, RefKind::FunctionCall) {
            if let Some((import, _path)) =
                resolve_imported_function(analysis, &r.target_name, module_index)
            {
                let mut parts = Vec::new();

                // Show signature if available.
                if let Some(cached) = module_index.get_cached(&import.module_name) {
                    if let Some(sub_info) = cached.sub_info(&r.target_name) {
                        let sig = format_imported_signature(&r.target_name, &sub_info);
                        parts.push(format!("```perl\n{}\n```", sig));
                        if let Some(doc) = sub_info.doc() {
                            parts.push(doc.to_string());
                        }
                    }
                }

                parts.push(format!("*imported from `{}`*", import.module_name));
                let markdown = parts.join("\n\n");
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: markdown,
                    }),
                    range: None,
                });
            }
        }
    }

    None
}

pub fn document_highlights(analysis: &FileAnalysis, pos: Position, tree: &tree_sitter::Tree, source: &str) -> Vec<DocumentHighlight> {
    use crate::file_analysis::AccessKind;
    analysis.find_highlights(position_to_point(pos), Some(tree), Some(source.as_bytes()))
        .into_iter()
        .map(|(span, access)| DocumentHighlight {
            range: span_to_range(span),
            kind: Some(match access {
                AccessKind::Write => DocumentHighlightKind::WRITE,
                _ => DocumentHighlightKind::READ,
            }),
        })
        .collect()
}

pub fn selection_ranges(tree: &Tree, pos: Position) -> SelectionRange {
    let spans = cursor_context::selection_ranges(tree, position_to_point(pos));
    // Build linked list from innermost to outermost
    let mut result: Option<SelectionRange> = None;
    for span in spans.into_iter().rev() {
        result = Some(SelectionRange {
            range: span_to_range(span),
            parent: result.map(Box::new),
        });
    }
    result.unwrap_or(SelectionRange {
        range: Range::default(),
        parent: None,
    })
}

pub fn folding_ranges(analysis: &FileAnalysis) -> Vec<FoldingRange> {
    analysis.fold_ranges
        .iter()
        .map(|f| FoldingRange {
            start_line: f.start_line as u32,
            start_character: None,
            end_line: f.end_line as u32,
            end_character: None,
            kind: Some(match f.kind {
                FoldKind::Region => FoldingRangeKind::Region,
                FoldKind::Comment => FoldingRangeKind::Comment,
            }),
            collapsed_text: None,
        })
        .collect()
}

// ---- Signature help ----

/// Does this class have ANY registered Handlers matching the method
/// name as a declared dispatcher? Used to decide whether we're in a
/// "known dispatch call" context — gates the noise-suppression logic
/// around it so the firehose of unrelated subs only gets suppressed
/// when we actually know this is a dispatcher call site.
fn class_has_dispatch_handlers(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
    class: &str,
    dispatcher: &str,
) -> bool {
    let matches = |sym: &crate::file_analysis::Symbol| -> bool {
        let SymbolDetail::Handler { owner, dispatchers, .. } = &sym.detail else { return false };
        let HandlerOwner::Class(n) = owner;
        if n != class { return false; }
        dispatchers.is_empty() || dispatchers.iter().any(|d| d == dispatcher)
    };
    if analysis.symbols.iter().any(&matches) { return true; }
    // Workspace-wide scan: same tradeoff as dispatch completion —
    // class-wide "any handler exists?" isn't name-keyed, so the
    // reverse index doesn't speed this up. Short-circuits on first
    // hit, so average cost is tiny when handlers exist and only
    // pays full walk when nothing matches.
    let mut found = false;
    module_index.for_each_cached(|_, cached| {
        if found { return; }
        if cached.analysis.symbols.iter().any(&matches) { found = true; }
    });
    found
}

/// True if the cursor sits somewhere that makes wrapping-quotes in the
/// insert_text wrong. Two cases:
///   * cursor in a `string_literal` / `interpolated_string_literal`
///     (the quotes are already typed)
///   * cursor at a fat-comma LHS autoquote position (the `=>` will
///     autoquote whatever bareword lands there)
fn cursor_in_string_or_autoquote(tree: &Tree, point: Point) -> bool {
    let Some(mut node) = tree.root_node().descendant_for_point_range(point, point) else {
        return false;
    };
    // Walk upward a handful of levels — tree-sitter may hand back a
    // token node first; the enclosing string_literal is typically one
    // or two parents up.
    for _ in 0..4 {
        match node.kind() {
            "string_literal" | "interpolated_string_literal"
            | "string_content" | "autoquoted_bareword" => return true,
            _ => {}
        }
        let Some(p) = node.parent() else { break };
        node = p;
    }
    false
}

/// Completions for the first arg of a string-dispatched method call.
/// Walks every Handler symbol (local + cross-file via module index),
/// filters by (owner class matches receiver, dispatcher matches method
/// name), and returns each as a CompletionItem. Stacked registrations
/// dedup on name; the completion shows the param shape in detail so
/// you know what args the handler will get.
fn dispatch_target_completions(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
    invocant: Option<&str>,
    method_name: &str,
    point: Point,
    tree: &Tree,
) -> Vec<CompletionCandidate> {
    let class = match resolve_invocant_class(analysis, invocant, point) {
        Some(c) => c,
        None => return Vec::new(),
    };

    // Quote-aware insert_text. Three cases:
    //   * cursor inside a string_literal (`->emit('|')`) — the quotes
    //     are already there, emit bare name.
    //   * cursor at fat-comma LHS autoquote position (`->emit(|=>...)`)
    //     — no quotes needed, Perl auto-quotes barewords on fat-comma
    //     LHS. (Dispatch shouldn't normally fire here, but defensive.)
    //   * anywhere else — emit with surrounding quotes so one accept
    //     keystroke produces `'name'` in bare parens.
    // Implemented as a closure so the branching stays adjacent to the
    // decision and every emitted candidate is consistent.
    let needs_quotes = !cursor_in_string_or_autoquote(tree, point);

    // Accumulate (handler_name → display_params + provenance) so stacked
    // registrations across files appear once in the completion list.
    let mut acc: std::collections::BTreeMap<String, (Vec<String>, String)> =
        std::collections::BTreeMap::new();

    let mut consider = |sym: &crate::file_analysis::Symbol, file_tag: &str| {
        let SymbolDetail::Handler { owner, dispatchers, params, .. } = &sym.detail else { return };
        let HandlerOwner::Class(n) = owner;
        if n != &class { return; }
        if !dispatchers.is_empty() && !dispatchers.iter().any(|d| d == method_name) {
            return;
        }
        let display: Vec<String> = params
            .iter()
            .filter(|p| !p.is_invocant)
            .map(|p| p.name.clone())
            .collect();
        acc.entry(sym.name.clone()).or_insert((display, file_tag.to_string()));
    };

    for sym in &analysis.symbols {
        consider(sym, "this file");
    }
    // Dispatch completion: offer handlers from any module on the
    // receiver's class. The reverse index only speeds up by-NAME
    // lookups; we want every handler on this class, across all names.
    // Names vary per handler, so we can't pre-filter — but this is
    // still O(workspace) by design (completion intentionally surfaces
    // everything available). Acceptable because completion UI shows
    // a small subset anyway; the full list is rarely large per class.
    module_index.for_each_cached(|module_name, cached| {
        for sym in &cached.analysis.symbols {
            consider(sym, module_name);
        }
    });

    acc.into_iter().map(|(name, (params, provenance))| {
        let detail = if params.is_empty() {
            format!("handler on {}  ({})", class, provenance)
        } else {
            format!("handler on {} ({})  — {}", class, params.join(", "), provenance)
        };
        CompletionCandidate {
            label: name.clone(),
            // Handler kind flows to CompletionItemKind::EVENT via
            // `fa_completion_kind` — consistent with outline and hover.
            kind: FaSymKind::Handler,
            detail: Some(detail),
            // Bare inside quotes / autoquote, quoted otherwise — see
            // `needs_quotes` above. Accepting the suggestion lands
            // correct source text regardless of where the cursor was.
            insert_text: Some(if needs_quotes {
                format!("'{}'", name)
            } else {
                name.clone()
            }),
            // Top of the list: handlers are the canonical completion in
            // this position when a dispatcher is declared for them.
            sort_priority: 0,
            additional_edits: Vec::new(),
                display_override: None,
        }
    }).collect()
}

/// Collect refs whose span contains `point` and match a predicate.
/// Small generic helper — used by mid-string completion to find the
/// (typically unique) MethodCallRef at the cursor.
fn refs_at_point_matching<'a>(
    analysis: &'a FileAnalysis,
    point: Point,
    pred: impl Fn(&crate::file_analysis::Ref) -> bool,
) -> Option<Vec<&'a crate::file_analysis::Ref>> {
    let out: Vec<&crate::file_analysis::Ref> = analysis.refs.iter()
        .filter(|r| span_contains_point(&r.span, point) && pred(r))
        .collect();
    if out.is_empty() { None } else { Some(out) }
}

fn span_contains_point(span: &crate::file_analysis::Span, p: Point) -> bool {
    let a = (span.start.row, span.start.column);
    let b = (span.end.row, span.end.column);
    let pp = (p.row, p.column);
    a <= pp && pp <= b
}

/// Mid-string completion for a cursor inside a plugin-emitted
/// `MethodCallRef`. Offers methods on the invocant class (walking
/// inheritance + workspace), prefix-filtered by whatever the user has
/// typed since the start of the ref's span.
///
/// The core is deliberately ignorant of plugin-specific string formats
/// (no `#` splitting, no `::` splitting — that's Mojo-routes syntax
/// bleeding in). It's the plugin's job to emit a tight span that
/// covers only the method-name portion of its string; the core just
/// slices `source[ref.span.start..cursor]` and uses that as the prefix.
/// If a plugin wants fuzzier matching behavior it can widen its spans;
/// the semantics stay plugin-controlled.
fn mid_string_methodref_completions(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
    invocant_class: &str,
    source: &str,
    point: Point,
    ref_span: crate::file_analysis::Span,
) -> Vec<CompletionItem> {
    // Pull the typed prefix out of the live source text — not from the
    // parser, because during active editing the two diverge.
    let lines: Vec<&str> = source.lines().collect();
    if ref_span.start.row >= lines.len() || point.row >= lines.len() {
        return Vec::new();
    }
    let typed = if ref_span.start.row == point.row {
        let line = lines[point.row];
        let start = ref_span.start.column.min(line.len());
        let end = point.column.min(line.len());
        &line[start..end]
    } else {
        // Multi-line ref spans: conservative — only use the current line.
        let line = lines[point.row];
        &line[..point.column.min(line.len())]
    };

    let candidates = analysis.complete_methods_for_class(invocant_class, Some(module_index));
    candidates
        .into_iter()
        .filter(|c| typed.is_empty() || c.label.starts_with(typed))
        .map(|c| {
            let mut item = candidate_to_completion_item(c);
            item.sort_text = Some(format!("000{}", item.label));
            item
        })
        .collect()
}

/// Find the enclosing method_call_expression at `point` and return any
/// `DispatchCall` ref whose span sits inside that call's argument list.
/// Returns `(handler_name, owner_class, dispatcher)` — all three are
/// already plugin-resolved, so the caller inherits const-folding and
/// receiver-type inference without re-deriving them.
fn dispatch_info_for_enclosing_call(
    analysis: &FileAnalysis,
    tree: &Tree,
    _source: &[u8],
    point: Point,
) -> Option<(String, String, String)> {
    // Walk up from the cursor until we hit the enclosing method call.
    let mut node = tree.root_node().descendant_for_point_range(point, point)?;
    let call = loop {
        if node.kind() == "method_call_expression" {
            break node;
        }
        node = node.parent()?;
    };
    let call_start = crate::file_analysis::Span {
        start: call.start_position(),
        end: call.end_position(),
    };

    // First DispatchCall ref whose span is contained by this call.
    for r in &analysis.refs {
        let RefKind::DispatchCall { dispatcher, owner } = &r.kind else { continue };
        if !span_contains_span(&call_start, &r.span) { continue; }
        let Some(HandlerOwner::Class(class)) = owner.clone() else { continue };
        return Some((r.target_name.clone(), class, dispatcher.clone()));
    }
    None
}

fn span_contains_span(outer: &crate::file_analysis::Span, inner: &crate::file_analysis::Span) -> bool {
    let o_start = (outer.start.row, outer.start.column);
    let o_end   = (outer.end.row,   outer.end.column);
    let i_start = (inner.start.row, inner.start.column);
    let i_end   = (inner.end.row,   inner.end.column);
    o_start <= i_start && i_end <= o_end
}

/// Build sig help for a known (class, dispatcher, handler_name). Walks
/// the current file's symbols AND every cached module — otherwise a
/// consumer file that emits against a producer-defined handler gets no
/// sig help, even though hover already walks cross-file (the two must
/// agree — same abstraction, same reach).
fn string_dispatch_signature_for(
    analysis: &FileAnalysis,
    module_index: Option<&ModuleIndex>,
    class: &str,
    dispatcher: &str,
    handler_name: &str,
    active_param: usize,
) -> Option<SignatureHelp> {
    let mut signatures: Vec<SignatureInformation> = Vec::new();

    // Shared builder — used both for in-file and cross-file symbol walks
    // so a handler's sig is formatted identically regardless of where
    // it lives.
    let push_sig = |signatures: &mut Vec<SignatureInformation>,
                    sym: &crate::file_analysis::Symbol,
                    provenance: Option<&str>| {
        let SymbolDetail::Handler { owner, dispatchers, params, .. } = &sym.detail else { return };
        let HandlerOwner::Class(n) = owner;
        if n != class { return; }
        let dispatcher_ok = dispatchers.is_empty()
            || dispatchers.iter().any(|d| d == dispatcher);
        if !dispatcher_ok || params.is_empty() { return; }

        let display: Vec<&ParamInfo> = params
            .iter()
            .filter(|p| !p.is_invocant)
            .collect();
        let labels: Vec<String> = display.iter()
            .map(|p| match &p.default {
                Some(d) => format!("{} = {}", p.name, d),
                None => p.name.clone(),
            })
            .collect();
        let parameters: Vec<ParameterInformation> = labels.iter()
            .map(|l| ParameterInformation {
                label: ParameterLabel::Simple(l.clone()),
                documentation: None,
            })
            .collect();
        let doc = match provenance {
            Some(p) => format!(
                "{} handler on `{}`, registered at {} line {}",
                handler_name, class, p, sym.selection_span.start.row + 1,
            ),
            None => format!(
                "{} handler on `{}`, registered at line {}",
                handler_name, class, sym.selection_span.start.row + 1,
            ),
        };
        signatures.push(SignatureInformation {
            label: format!("{}('{}', {})", dispatcher, handler_name, labels.join(", ")),
            documentation: Some(Documentation::String(doc)),
            parameters: Some(parameters),
            active_parameter: None,
        });
    };

    for sym in &analysis.symbols {
        if sym.name != handler_name { continue; }
        push_sig(&mut signatures, sym, None);
    }
    if let Some(idx) = module_index {
        for module_name in idx.modules_with_symbol(handler_name) {
            let Some(cached) = idx.get_cached(&module_name) else { continue };
            for sym in &cached.analysis.symbols {
                if sym.name != handler_name { continue; }
                push_sig(&mut signatures, sym, Some(module_name.as_str()));
            }
        }
    }

    if signatures.is_empty() { return None; }
    Some(SignatureHelp {
        signatures,
        active_signature: Some(0),
        active_parameter: Some(active_param.saturating_sub(1) as u32),
    })
}

/// Resolve the class of an invocant expression at a given cursor point.
///   * `$self` / `__PACKAGE__`  → enclosing package at that position
///   * bare `Pkg::Name`         → the literal class
///   * `$var`                   → looked up via `analysis.inferred_type`
/// Returns `None` when the expression doesn't resolve to a known class.
fn resolve_invocant_class(
    analysis: &FileAnalysis,
    invocant: Option<&str>,
    point: Point,
) -> Option<String> {
    let text = invocant?;
    if text == "$self" || text == "__PACKAGE__" {
        return analysis.package_at(point).map(|s| s.to_string());
    }
    if let Some(stripped) = text.strip_prefix('$') {
        let var_name = format!("${}", stripped);
        if let Some(ty) = analysis.inferred_type(&var_name, point) {
            if let crate::file_analysis::InferredType::ClassName(n) = ty {
                return Some(n.clone());
            }
        }
        return None;
    }
    if text.starts_with('@') || text.starts_with('%') {
        return None;
    }
    Some(text.to_string())
}

/// Text-driven entry point: resolve invocant → class, then delegate to
/// `string_dispatch_signature_for`. Used for mid-editing states where
/// no DispatchCall ref exists yet.
fn string_dispatch_signature(
    analysis: &FileAnalysis,
    module_index: Option<&ModuleIndex>,
    invocant: Option<&str>,
    dispatcher: &str,
    handler_name: &str,
    active_param: usize,
    point: Point,
) -> Option<SignatureHelp> {
    let class = resolve_invocant_class(analysis, invocant, point)?;
    string_dispatch_signature_for(analysis, module_index, &class, dispatcher, handler_name, active_param)
}

pub fn signature_help(
    analysis: &FileAnalysis,
    tree: &Tree,
    text: &str,
    pos: Position,
    module_index: &ModuleIndex,
) -> Option<SignatureHelp> {
    let point = position_to_point(pos);

    // Step 1: cursor_context finds the enclosing call
    let call_ctx = cursor_context::find_call_context(tree, text.as_bytes(), point)?;

    // Step 1a: string-dispatch specialization. `$x->emit('ready', CURSOR)`
    // is a method call whose string arg routes to a registered handler;
    // when handler_params are on record for that (class, event_name) pair,
    // surface them instead of the emit() method's own generic signature.
    // Stacked defs (multiple `->on('ready', sub {...})` wire-ups) each
    // contribute one `SignatureInformation` so users see every handler
    // shape they might be dispatching to.
    if call_ctx.is_method && call_ctx.active_param >= 1 {
        // Primary path: find the DispatchCall ref the plugin already
        // emitted for this call site. Its `target_name`, `owner`, and
        // `dispatcher` were all computed with the builder's full
        // knowledge — including const-folding `$dynamic` back to
        // `'connect'` — so sig help inherits folding for free and
        // can't drift from what hover shows.
        if let Some((handler_name, owner_class, dispatcher)) =
            dispatch_info_for_enclosing_call(analysis, tree, text.as_bytes(), point)
        {
            if let Some(sig) = string_dispatch_signature_for(
                analysis,
                Some(module_index),
                &owner_class,
                &dispatcher,
                &handler_name,
                call_ctx.active_param,
            ) {
                return Some(sig);
            }
        }

        // Fallback: no DispatchCall ref at this call site (e.g. no plugin
        // declared the method as a dispatcher). Try the text-level
        // first-arg string; this covers mid-editing states where a ref
        // hasn't been emitted yet.
        if let Some(ref name) = call_ctx.first_arg_string {
            if let Some(sig) = string_dispatch_signature(
                analysis,
                Some(module_index),
                call_ctx.invocant.as_deref(),
                &call_ctx.name,
                name,
                call_ctx.active_param,
                point,
            ) {
                return Some(sig);
            }
        }
    }

    // Step 2: file_analysis resolves the sub signature (local + cross-file)
    if let Some(sig_info) = analysis.signature_for_call(
        &call_ctx.name,
        call_ctx.is_method,
        call_ctx.invocant.as_deref(),
        point,
        Some(module_index),
    ) {
        // Build param labels with inferred types
        let param_labels: Vec<String> = sig_info
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let base = if let Some(ref default) = p.default {
                    format!("{} = {}", p.name, default)
                } else {
                    p.name.clone()
                };
                // Skip $self/$class — type is obvious
                if p.name == "$self" || p.name == "$class" {
                    return base;
                }
                // Cross-file: use pre-resolved param types
                if let Some(ref types) = sig_info.param_types {
                    if let Some(Some(ref type_tag)) = types.get(i) {
                        return format!("{}: {}", base, type_tag);
                    }
                    return base;
                }
                // Local: look up inferred type at end of sub body
                if let Some(ty) = analysis.inferred_type(&p.name, sig_info.body_end) {
                    format!("{}: {}", base, format_inferred_type(ty))
                } else {
                    base
                }
            })
            .collect();

        let params: Vec<ParameterInformation> = param_labels
            .iter()
            .map(|label| ParameterInformation {
                label: ParameterLabel::Simple(label.clone()),
                documentation: None,
            })
            .collect();

        let label = format!("{}({})", sig_info.name, param_labels.join(", "));

        return Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label,
                documentation: None,
                parameters: Some(params),
                active_parameter: Some(call_ctx.active_param as u32),
            }],
            active_signature: Some(0),
            active_parameter: Some(call_ctx.active_param as u32),
        });
    }

    None
}

// ---- Semantic tokens ----

// Token type/modifier indices are defined in file_analysis.rs (TOK_*, MOD_*).

pub fn semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::VARIABLE,       // 0: variables
        SemanticTokenType::PARAMETER,      // 1: sub parameters
        SemanticTokenType::FUNCTION,       // 2: function calls
        SemanticTokenType::METHOD,         // 3: method calls
        SemanticTokenType::MACRO,          // 4: framework DSL keywords
        SemanticTokenType::PROPERTY,       // 5: hash keys
        SemanticTokenType::NAMESPACE,      // 6: package/class names
        SemanticTokenType::REGEXP,         // 7: regex literals
        SemanticTokenType::ENUM_MEMBER,    // 8: constants
        SemanticTokenType::KEYWORD,        // 9: $self/$class
    ]
}

pub fn semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,      // 0
        SemanticTokenModifier::READONLY,         // 1
        SemanticTokenModifier::MODIFICATION,     // 2
        SemanticTokenModifier::DEFAULT_LIBRARY,  // 3
        SemanticTokenModifier::DEPRECATED,       // 4
        SemanticTokenModifier::STATIC,           // 5
        SemanticTokenModifier::new("scalar"),    // 6
        SemanticTokenModifier::new("array"),     // 7
        SemanticTokenModifier::new("hash"),      // 8
    ]
}

/// Returns inlay hints for the given range.
///
/// Shows type annotations for variable declarations with non-obvious inferred types,
/// and return type annotations for sub/method declarations.
pub fn inlay_hints(analysis: &FileAnalysis, range: Range) -> Vec<InlayHint> {
    let start = position_to_point(range.start);
    let end = position_to_point(range.end);
    let mut hints = Vec::new();

    for sym in &analysis.symbols {
        let decl_point = sym.selection_span.end;
        // Skip symbols outside the requested range
        if decl_point.row < start.row || decl_point.row > end.row {
            continue;
        }

        match sym.kind {
            FaSymKind::Variable => {
                // Skip $self — always the enclosing class, too noisy
                if sym.name == "$self" {
                    continue;
                }
                if let Some(ty) = analysis.inferred_type(&sym.name, sym.span.start) {
                    // Only show Object/HashRef/ArrayRef/CodeRef/Regexp — not Numeric/String
                    if matches!(ty, InferredType::Numeric | InferredType::String) {
                        continue;
                    }
                    hints.push(InlayHint {
                        position: point_to_position(decl_point),
                        label: InlayHintLabel::String(format!(": {}", format_inferred_type(ty))),
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    });
                }
            }
            FaSymKind::Sub | FaSymKind::Method => {
                // Plugin-synthesized subs/methods often have
                // return_type set to internal proxy classes (Mojo
                // helpers' `_Helper::users` chain, DBIC ResultSet
                // wrappers, etc.). The kind icon + hover already
                // carry the useful info; the inlay hint just
                // repeats a long dotted class name at every
                // declaration. Suppress it for framework symbols.
                if sym.namespace.is_framework() {
                    continue;
                }
                if let SymbolDetail::Sub { return_type: Some(ref rt), .. } = sym.detail {
                    // Only show non-trivial return types
                    if matches!(rt, InferredType::Numeric | InferredType::String) {
                        continue;
                    }
                    hints.push(InlayHint {
                        position: point_to_position(decl_point),
                        label: InlayHintLabel::String(format!("→ {}", format_inferred_type(rt))),
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    });
                }
            }
            _ => {}
        }
    }

    hints
}

pub fn semantic_tokens(analysis: &FileAnalysis) -> Vec<SemanticToken> {
    let tokens = analysis.semantic_tokens();

    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for t in &tokens {
        let line = t.span.start.row as u32;
        let start = t.span.start.column as u32;
        let length = if t.span.start.row == t.span.end.row {
            (t.span.end.column as u32).saturating_sub(start).max(1)
        } else {
            1
        };

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start.saturating_sub(prev_start)
        } else {
            start
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: t.token_type,
            token_modifiers_bitset: t.modifiers,
        });

        prev_line = line;
        prev_start = start;
    }

    result
}

// ---- Import resolution helpers ----

/// Build completion candidates for functions imported via `use` statements.
/// Offers all @EXPORT_OK from already-imported modules — not just the ones
/// currently in the qw() list. Functions not yet imported get an auto-import
/// edit that adds them to the existing qw() list.
fn imported_function_completions(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
) -> Vec<CompletionCandidate> {
    use crate::file_analysis::Span;
    let mut candidates = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for import in &analysis.imports {
        let cached: Option<Arc<CachedModule>> = module_index.get_cached(&import.module_name);

        // 1. Always offer explicitly imported symbols (from the qw list)
        for name in &import.imported_symbols {
            if !seen.insert(name.clone()) {
                continue;
            }
            if !analysis.symbols_named(name).is_empty() {
                continue;
            }
            let detail = completion_detail_for_import(name, cached.as_deref(), &import.module_name);
            candidates.push(CompletionCandidate {
                label: name.clone(),
                kind: FaSymKind::Sub,
                detail: Some(detail),
                insert_text: None,
                sort_priority: PRIORITY_EXPLICIT_IMPORT,
                additional_edits: vec![],
                display_override: None,
            });
        }

        // 2. Offer additional @EXPORT_OK functions (not yet imported) if we can resolve the module
        if let Some(ref cached) = cached {
            let fa = &cached.analysis;
            let all_exported: Vec<&String> = if import.imported_symbols.is_empty() {
                // Bare `use Foo;` — offer @EXPORT
                fa.export.iter().collect()
            } else {
                // `use Foo qw(bar)` — offer remaining @EXPORT + @EXPORT_OK
                let mut all = Vec::new();
                all.extend(fa.export.iter());
                all.extend(fa.export_ok.iter());
                all
            };

            for name in all_exported {
                // Skip already-offered (explicitly imported) and locally defined
                if !seen.insert(name.clone()) {
                    continue;
                }
                if !analysis.symbols_named(name).is_empty() {
                    continue;
                }

                let rt_prefix = cached.sub_info(name)
                    .and_then(|s| s.return_type().cloned())
                    .map(|rt| format!("→ {} ", format_inferred_type(&rt)))
                    .unwrap_or_default();

                let (detail, priority, additional_edits) =
                    if let Some(close_pos) = import.qw_close_paren {
                        // Auto-add to existing qw() list
                        let insert_point = Span {
                            start: close_pos,
                            end: close_pos,
                        };
                        (
                            format!("{}{} (auto-import)", rt_prefix, import.module_name),
                            PRIORITY_AUTO_ADD_QW,
                            vec![(insert_point, format!(" {}", name))],
                        )
                    } else {
                        // No qw() list to edit (bare `use Foo;`)
                        (
                            format!("{}imported from {}", rt_prefix, import.module_name),
                            PRIORITY_BARE_IMPORT,
                            vec![],
                        )
                    };

                candidates.push(CompletionCandidate {
                    label: name.clone(),
                    kind: FaSymKind::Sub,
                    detail: Some(detail),
                    insert_text: None,
                    sort_priority: priority,
                    additional_edits,
                    display_override: None,
                });
            }
        }
    }

    candidates
}

/// Build completion candidates for functions from modules that aren't imported
/// at all. Each candidate carries an `additional_edit` that inserts a full
/// `use Module qw(func);` statement.
fn unimported_function_completions(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
    point: Point,
    stable_packages: Option<&[(String, usize)]>,
) -> Vec<CompletionCandidate> {
    use crate::file_analysis::Span;
    let mut candidates = Vec::new();

    // Collect already-imported module names so we skip them.
    let imported_modules: std::collections::HashSet<&str> = analysis
        .imports
        .iter()
        .map(|i| i.module_name.as_str())
        .collect();

    let mut insert_pos = find_use_insertion_position(analysis, point, stable_packages);

    // If the computed position is after the cursor, fall back to inserting
    // after the nearest import or package statement ABOVE the cursor.
    if insert_pos.line as usize > point.row {
        // Find the last import above the cursor
        let last_import_above = analysis.imports.iter().rev()
            .find(|imp| imp.span.start.row < point.row);
        if let Some(imp) = last_import_above {
            insert_pos = Position { line: imp.span.end.row as u32 + 1, character: 0 };
        } else {
            // Find the last package statement above the cursor
            let last_pkg_above = analysis.symbols.iter().rev()
                .find(|s| matches!(s.kind, FaSymKind::Package | FaSymKind::Class) && s.selection_span.start.row < point.row);
            if let Some(pkg) = last_pkg_above {
                insert_pos = Position { line: pkg.selection_span.start.row as u32 + 1, character: 0 };
            }
            // else: keep original position (top of file)
        }
    }

    let insert_span = Span {
        start: tree_sitter::Point {
            row: insert_pos.line as usize,
            column: insert_pos.character as usize,
        },
        end: tree_sitter::Point {
            row: insert_pos.line as usize,
            column: insert_pos.character as usize,
        },
    };

    module_index.for_each_cached(|module_name, cached| {
        if imported_modules.contains(module_name) {
            return;
        }

        let fa = &cached.analysis;
        let all_exported = fa.export.iter().chain(fa.export_ok.iter());
        for name in all_exported {
            // Skip functions already defined locally
            if !analysis.symbols_named(name).is_empty() {
                continue;
            }
            candidates.push(CompletionCandidate {
                label: name.clone(),
                kind: FaSymKind::Sub,
                detail: Some(format!("{} (auto-import)", module_name)),
                insert_text: None,
                sort_priority: PRIORITY_UNIMPORTED,
                additional_edits: vec![(
                    insert_span,
                    format!("use {} qw({});\n", module_name, name),
                )],
                display_override: None,
            });
        }
    });

    // Sort for deterministic order
    candidates.sort_by(|a, b| a.label.cmp(&b.label).then(a.detail.cmp(&b.detail)));
    candidates
}

/// Find which import provides a given function name.
/// Checks both explicitly imported symbols and all @EXPORT/@EXPORT_OK
/// from already-imported modules. Single DashMap lookup per module.
fn resolve_imported_function<'a>(
    analysis: &'a FileAnalysis,
    func_name: &str,
    module_index: &ModuleIndex,
) -> Option<(&'a crate::file_analysis::Import, std::path::PathBuf)> {
    for import in &analysis.imports {
        if let Some(cached) = module_index.get_cached(&import.module_name) {
            let explicitly_imported = import.imported_symbols.iter().any(|s| s == func_name);
            let in_export_lists = cached.analysis.export.iter().any(|s| s == func_name)
                || cached.analysis.export_ok.iter().any(|s| s == func_name);
            if explicitly_imported || in_export_lists {
                return Some((import, cached.path.clone()));
            }
        } else if import.imported_symbols.iter().any(|s| s == func_name) {
            // Module not cached yet but explicitly listed in qw() — trust the import.
            if let Some(path) = module_index.module_path_cached(&import.module_name) {
                return Some((import, path));
            }
        }
    }
    None
}

fn completion_detail_for_import(
    name: &str,
    cached: Option<&CachedModule>,
    module_name: &str,
) -> String {
    if let Some(cached) = cached {
        if let Some(sub_info) = cached.sub_info(name) {
            if let Some(rt) = sub_info.return_type() {
                return format!("→ {} ({})", format_inferred_type(rt), module_name);
            }
        }
    }
    format!("imported from {}", module_name)
}

fn format_imported_signature(name: &str, sub_info: &SubInfo<'_>) -> String {
    let params_str = sub_info
        .params()
        .iter()
        .map(|p| p.name.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    let mut sig = format!("sub {}({})", name, params_str);
    if let Some(rt) = sub_info.return_type() {
        sig.push_str(&format!(" → {}", format_inferred_type(rt)));
    }
    sig
}

// ---- Diagnostics ----

/// Sorted list of Perl built-in functions. Used to avoid false-positive
/// "unresolved function" diagnostics. Checked via binary_search.
static PERL_BUILTINS: &[&str] = &[
    "abs", "accept", "alarm", "atan2",
    "bind", "binmode", "bless",
    "caller", "chdir", "chmod", "chomp", "chop", "chown", "chr", "chroot", "close",
    "closedir", "connect", "cos", "crypt",
    "dbmclose", "dbmopen", "defined", "delete", "die", "do", "dump",
    "each", "endgrent", "endhostent", "endnetent", "endprotoent", "endpwent",
    "endservent", "eof", "eval", "exec", "exists", "exit",
    "fcntl", "fileno", "flock", "fork", "format", "formline",
    "getc", "getgrent", "getgrgid", "getgrnam", "gethostbyaddr", "gethostbyname",
    "gethostent", "getlogin", "getnetbyaddr", "getnetbyname", "getnetent",
    "getpeername", "getpgrp", "getppid", "getpriority", "getprotobyname",
    "getprotobynumber", "getprotoent", "getpwent", "getpwnam", "getpwuid",
    "getservbyname", "getservbyport", "getservent", "getsockname", "getsockopt",
    "glob", "gmtime", "goto", "grep",
    "hex",
    "import", "index", "int", "ioctl",
    "join",
    "keys", "kill",
    "last", "lc", "lcfirst", "length", "link", "listen", "local", "localtime", "log",
    "lstat",
    "map", "mkdir", "msgctl", "msgget", "msgrcv", "msgsnd",
    "my",
    "new", "next", "no",
    "oct", "open", "opendir", "ord", "our",
    "pack", "pipe", "pop", "pos", "print", "printf", "prototype", "push",
    "quotemeta",
    "rand", "read", "readdir", "readline", "readlink", "readpipe", "recv", "redo",
    "ref", "rename", "require", "reset", "return", "reverse", "rewinddir", "rindex",
    "rmdir",
    "say", "scalar", "seek", "seekdir", "select", "semctl", "semget", "semop", "send",
    "setgrent", "sethostent", "setnetent", "setpgrp", "setpriority", "setprotoent",
    "setpwent", "setservent", "setsockopt", "shift", "shmctl", "shmget", "shmread",
    "shmwrite", "shutdown", "sin", "sleep", "socket", "socketpair", "sort", "splice",
    "split", "sprintf", "sqrt", "srand", "stat", "state", "study", "sub", "substr",
    "symlink", "syscall", "sysopen", "sysread", "sysseek", "system", "syswrite",
    "tell", "telldir", "tie", "tied", "time", "times", "truncate",
    "uc", "ucfirst", "umask", "undef", "unlink", "unpack", "unshift", "untie", "use",
    "utime",
    "values", "vec",
    "wait", "waitpid", "wantarray", "warn", "write",
];

fn is_perl_builtin(name: &str) -> bool {
    PERL_BUILTINS.binary_search(&name).is_ok()
}


pub fn collect_diagnostics(analysis: &FileAnalysis, module_index: &ModuleIndex) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for r in &analysis.refs {
        if !matches!(r.kind, RefKind::FunctionCall) {
            continue;
        }
        let name = &r.target_name;

        // Skip package-qualified calls like Foo::bar()
        if name.contains("::") {
            continue;
        }

        // Skip code deref calls like &{$var}()
        if name.starts_with('&') {
            continue;
        }

        // Skip Perl builtins
        if is_perl_builtin(name) {
            continue;
        }

        // Skip locally defined subs
        if !analysis.symbols_named(name).is_empty() {
            continue;
        }

        // Skip functions implicitly imported by OOP frameworks (has, extends, etc.)
        if analysis.framework_imports.contains(name.as_str()) {
            continue;
        }

        // Skip if explicitly listed in any import's qw(...),
        // or auto-imported via @EXPORT on a bare `use Foo;` (no qw list).
        let explicitly_imported = analysis.imports.iter().any(|imp| {
            if imp.imported_symbols.iter().any(|s| s == name) {
                return true;
            }
            // Bare `use Foo;` — check if function is in @EXPORT (auto-imported)
            if imp.imported_symbols.is_empty() {
                if let Some(cached) = module_index.get_cached(&imp.module_name) {
                    if cached.analysis.export.iter().any(|s| s == name) {
                        return true;
                    }
                }
            }
            false
        });
        if explicitly_imported {
            continue;
        }

        // Check if an imported module exports this function
        let range = span_to_range(r.span);
        if let Some((import, _path)) = resolve_imported_function(analysis, name, module_index) {
            // Importable but not yet in the qw() list → actionable hint
            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                code: Some(NumberOrString::String("unresolved-function".into())),
                source: Some("perl-lsp".into()),
                message: format!(
                    "'{}' is exported by {} but not imported",
                    name, import.module_name,
                ),
                data: Some(serde_json::json!({
                    "module": import.module_name,
                    "function": name,
                })),
                ..Default::default()
            });
        } else {
            // Search ALL cached modules for this function.
            let exporters = module_index.find_exporters(name);
            if !exporters.is_empty() {
                let msg = if exporters.len() == 1 {
                    format!(
                        "'{}' is exported by {} (not yet imported)",
                        name, exporters[0],
                    )
                } else {
                    format!(
                        "'{}' is exported by {} and {} other module(s)",
                        name,
                        exporters[0],
                        exporters.len() - 1,
                    )
                };
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::HINT),
                    code: Some(NumberOrString::String("unresolved-function".into())),
                    source: Some("perl-lsp".into()),
                    message: msg,
                    data: Some(serde_json::json!({
                        "modules": exporters,
                        "function": name,
                    })),
                    ..Default::default()
                });
            } else {
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("unresolved-function".into())),
                    source: Some("perl-lsp".into()),
                    message: format!("'{}' is not defined in this file", name),
                    ..Default::default()
                });
            }
        }
    }

    // 5e: Unresolved method diagnostics for locally-defined classes
    let universal_methods = [
        "new", "AUTOLOAD", "DESTROY", "can", "isa", "DOES", "VERSION",
        // DBIC meta-methods (inherited from DBIx::Class::Core)
        "add_columns", "add_column", "set_primary_key", "table", "resultset_class",
        "has_many", "has_one", "belongs_to", "might_have", "many_to_many",
        "load_components",
        // Moose/Moo meta-methods
        "meta",
    ];
    for r in &analysis.refs {
        let (invocant, _invocant_span) = match &r.kind {
            RefKind::MethodCall { invocant, invocant_span, .. } => (invocant, invocant_span),
            _ => continue,
        };
        let method_name = &r.target_name;

        // Skip universal methods
        if universal_methods.contains(&method_name.as_str()) {
            continue;
        }

        // Resolve invocant to class name
        let class_name = if !invocant.starts_with('$')
            && !invocant.starts_with('@')
            && !invocant.starts_with('%')
        {
            Some(invocant.clone())
        } else {
            analysis.inferred_type(invocant, r.span.start)
                .and_then(|ty| ty.class_name().map(|s| s.to_string()))
        };
        let class_name = match class_name {
            Some(cn) => cn,
            None => continue,
        };

        // Only fire for locally-defined classes
        let is_local_class = analysis.symbols.iter().any(|s| {
            matches!(s.kind, FaSymKind::Class | FaSymKind::Package) && s.name == class_name
        });
        if !is_local_class {
            continue;
        }

        // Check the class has at least one method (otherwise likely external)
        let has_methods = analysis.symbols.iter().any(|s| {
            matches!(s.kind, FaSymKind::Sub | FaSymKind::Method)
                && analysis.symbol_in_class(s.id, &class_name)
        });
        if !has_methods {
            continue;
        }

        // Check if the method exists in the class (walks inheritance chain)
        if analysis.resolve_method_in_ancestors(&class_name, method_name, Some(module_index)).is_some() {
            continue;
        }

        diagnostics.push(Diagnostic {
            range: span_to_range(r.span),
            severity: Some(DiagnosticSeverity::HINT),
            code: Some(NumberOrString::String("unresolved-method".into())),
            source: Some("perl-lsp".into()),
            message: format!(
                "'{}' is not defined in {}",
                method_name, class_name,
            ),
            ..Default::default()
        });
    }

    diagnostics
}

// ---- Code actions ----

/// Find the position to insert a new `use` statement, scoped to the package at `point`.
/// Uses line-range approach: finds which package range the cursor is in,
/// then inserts after the last `use` in that range.
/// `stable_packages` provides fallback package lines from the stable outline
/// when the current parse lost packages due to error recovery.
fn find_use_insertion_position(
    analysis: &FileAnalysis,
    point: Point,
    stable_packages: Option<&[(String, usize)]>,
) -> Position {
    // Collect package declaration lines from current parse
    let mut pkg_lines: Vec<usize> = analysis.symbols.iter()
        .filter(|s| matches!(s.kind, FaSymKind::Package | FaSymKind::Class))
        .map(|s| s.selection_span.start.row)
        .collect();

    // If the stable outline has MORE packages than the current parse,
    // merge them in — the parse lost some due to error recovery.
    if let Some(stable) = stable_packages {
        if stable.len() > pkg_lines.len() {
            for (_, line) in stable {
                if !pkg_lines.contains(line) {
                    pkg_lines.push(*line);
                }
            }
        }
    }
    pkg_lines.sort();

    // Find the package range containing `point`
    let pkg_start = pkg_lines.iter().rev()
        .find(|&&line| line <= point.row)
        .copied()
        .unwrap_or(0);
    let pkg_end = pkg_lines.iter()
        .find(|&&line| line > point.row)
        .copied()
        .unwrap_or(usize::MAX);

    // Find the last import within this package's line range
    let last_import = analysis.imports.iter().rev().find(|imp| {
        imp.span.start.row >= pkg_start && imp.span.start.row < pkg_end
    });

    if let Some(imp) = last_import {
        Position {
            line: imp.span.end.row as u32 + 1,
            character: 0,
        }
    } else {
        // No imports in this package range — insert after the package statement
        Position {
            line: pkg_start as u32 + 1,
            character: 0,
        }
    }
}

pub fn code_actions(
    diagnostics: &[Diagnostic],
    analysis: &FileAnalysis,
    uri: &Url,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    for diag in diagnostics {
        let code_matches = matches!(
            &diag.code,
            Some(NumberOrString::String(s)) if s == "unresolved-function"
        );
        if !code_matches {
            continue;
        }
        let data = match &diag.data {
            Some(d) => d,
            None => continue,
        };
        let func_name = match data.get("function").and_then(|v| v.as_str()) {
            Some(f) => f,
            None => continue,
        };

        // Case 1: Already-imported module — add function to existing qw() list
        if let Some(module_name) = data.get("module").and_then(|v| v.as_str()) {
            if let Some(action) =
                make_add_to_qw_action(analysis, uri, diag, module_name, func_name)
            {
                actions.push(action);
            }
            continue;
        }

        // Case 2: New import — add `use Module qw(func);` statement
        if let Some(modules) = data.get("modules").and_then(|v| v.as_array()) {
            let diag_point = position_to_point(diag.range.start);
            let mut insert_pos = find_use_insertion_position(analysis, diag_point, None);
            // If position is after the diagnostic, fall back to nearest import/package above
            if insert_pos.line > diag.range.start.line {
                let last_import_above = analysis.imports.iter().rev()
                    .find(|imp| imp.span.start.row < diag_point.row);
                if let Some(imp) = last_import_above {
                    insert_pos = Position { line: imp.span.end.row as u32 + 1, character: 0 };
                } else {
                    let last_pkg_above = analysis.symbols.iter().rev()
                        .find(|s| matches!(s.kind, FaSymKind::Package | FaSymKind::Class) && s.selection_span.start.row < diag_point.row);
                    if let Some(pkg) = last_pkg_above {
                        insert_pos = Position { line: pkg.selection_span.start.row as u32 + 1, character: 0 };
                    }
                }
            }
            for (i, module_val) in modules.iter().enumerate() {
                if let Some(module_name) = module_val.as_str() {
                    let new_text = format!("use {} qw({});\n", module_name, func_name);
                    let edit = TextEdit {
                        range: Range {
                            start: insert_pos,
                            end: insert_pos,
                        },
                        new_text,
                    };
                    let mut changes = HashMap::new();
                    changes.insert(uri.clone(), vec![edit]);

                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Add 'use {} qw({})'", module_name, func_name),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diag.clone()]),
                        edit: Some(WorkspaceEdit {
                            changes: Some(changes),
                            ..Default::default()
                        }),
                        is_preferred: Some(i == 0 && modules.len() == 1),
                        ..Default::default()
                    }));
                }
            }
        }
    }

    actions
}

/// Generate a code action that adds a function to an existing `qw()` import list.
fn make_add_to_qw_action(
    analysis: &FileAnalysis,
    uri: &Url,
    diag: &Diagnostic,
    module_name: &str,
    func_name: &str,
) -> Option<CodeActionOrCommand> {
    let import = analysis
        .imports
        .iter()
        .find(|imp| imp.module_name == module_name)?;
    let close_pos = import.qw_close_paren?;
    let insert_pos = point_to_position(close_pos);
    let edit = TextEdit {
        range: Range {
            start: insert_pos,
            end: insert_pos,
        },
        new_text: format!(" {}", func_name),
    };
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), vec![edit]);
    Some(CodeActionOrCommand::CodeAction(CodeAction {
        title: format!("Import '{}' from {}", func_name, module_name),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }),
        is_preferred: Some(true),
        ..Default::default()
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;

    fn parse_analysis(source: &str) -> FileAnalysis {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&ts_parser_perl::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        builder::build(&tree, source.as_bytes())
    }

    /// Build a CachedModule by parsing a synthesized Perl source listing the given exports.
    /// Used by tests to seed ModuleIndex with known export lists without real @INC files.
    fn fake_cached(
        path: &str,
        exports: &[&str],
        exports_ok: &[&str],
    ) -> std::sync::Arc<crate::module_index::CachedModule> {
        let mut source = String::from("package Fake;\n");
        if !exports.is_empty() {
            source.push_str(&format!("our @EXPORT = qw({});\n", exports.join(" ")));
        }
        if !exports_ok.is_empty() {
            source.push_str(&format!("our @EXPORT_OK = qw({});\n", exports_ok.join(" ")));
        }
        for n in exports.iter().chain(exports_ok.iter()) {
            source.push_str(&format!("sub {} {{}}\n", n));
        }
        source.push_str("1;\n");
        std::sync::Arc::new(crate::module_index::CachedModule::new(
            std::path::PathBuf::from(path),
            std::sync::Arc::new(parse_analysis(&source)),
        ))
    }

    #[test]
    fn test_builtins_sorted() {
        for window in PERL_BUILTINS.windows(2) {
            assert!(
                window[0] < window[1],
                "PERL_BUILTINS not sorted: '{}' >= '{}'",
                window[0],
                window[1],
            );
        }
    }

    #[test]
    fn test_is_perl_builtin() {
        assert!(is_perl_builtin("print"));
        assert!(is_perl_builtin("chomp"));
        assert!(is_perl_builtin("die"));
        assert!(!is_perl_builtin("frobnicate"));
        assert!(!is_perl_builtin("my_custom_sub"));
    }

    #[test]
    fn test_diagnostics_skips_builtins() {
        let source = "use Carp qw(croak);\nprint 'hello';\ndie 'oops';\n";
        let analysis = parse_analysis(source);
        let module_index = crate::module_index::ModuleIndex::new_for_test();
        let diags = collect_diagnostics(&analysis, &module_index);
        // print and die are builtins, croak is explicitly imported — no diagnostics
        assert!(
            diags.is_empty(),
            "Expected no diagnostics for builtins/imported, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
        );
    }

    #[test]
    fn test_diagnostics_unresolved_function() {
        let source = "frobnicate();\n";
        let analysis = parse_analysis(source);
        let module_index = crate::module_index::ModuleIndex::new_for_test();
        let diags = collect_diagnostics(&analysis, &module_index);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::INFORMATION));
        assert!(diags[0].message.contains("frobnicate"));
    }

    #[test]
    fn test_diagnostics_skips_local_sub() {
        let source = "sub helper { 1 }\nhelper();\n";
        let analysis = parse_analysis(source);
        let module_index = crate::module_index::ModuleIndex::new_for_test();
        let diags = collect_diagnostics(&analysis, &module_index);
        assert!(
            diags.is_empty(),
            "Locally defined sub should not produce diagnostic, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>(),
        );
    }

    #[test]
    fn test_diagnostics_skips_package_qualified() {
        let source = "Foo::Bar::baz();\n";
        let analysis = parse_analysis(source);
        let module_index = crate::module_index::ModuleIndex::new_for_test();
        let diags = collect_diagnostics(&analysis, &module_index);
        assert!(
            diags.is_empty(),
            "Package-qualified calls should not produce diagnostic",
        );
    }

    #[test]
    fn test_code_action_from_diagnostic() {
        let source = "use Carp qw(croak);\ncarp('oops');\n";
        let analysis = parse_analysis(source);
        let uri = Url::parse("file:///test.pl").unwrap();

        // Simulate a HINT diagnostic with data (as collect_diagnostics would produce
        // if module_index had resolved Carp)
        let diag = Diagnostic {
            range: Range {
                start: Position { line: 1, character: 0 },
                end: Position { line: 1, character: 4 },
            },
            severity: Some(DiagnosticSeverity::HINT),
            code: Some(NumberOrString::String("unresolved-function".into())),
            source: Some("perl-lsp".into()),
            message: "'carp' is exported by Carp but not imported".into(),
            data: Some(serde_json::json!({"module": "Carp", "function": "carp"})),
            ..Default::default()
        };

        let actions = code_actions(&[diag], &analysis, &uri);
        assert_eq!(actions.len(), 1);
        if let CodeActionOrCommand::CodeAction(action) = &actions[0] {
            assert_eq!(action.title, "Import 'carp' from Carp");
            assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
            assert_eq!(action.is_preferred, Some(true));

            // Verify the edit inserts " carp" at the qw close paren
            let edit = action.edit.as_ref().unwrap();
            let changes = edit.changes.as_ref().unwrap();
            let text_edits = changes.get(&uri).unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, " carp");
        } else {
            panic!("Expected CodeAction, got Command");
        }
    }

    #[test]
    fn test_code_action_new_use_statement() {
        let source = "use strict;\nuse warnings;\nfrobnicate();\n";
        let analysis = parse_analysis(source);
        let uri = Url::parse("file:///test.pl").unwrap();

        let diag = Diagnostic {
            range: Range {
                start: Position { line: 2, character: 0 },
                end: Position { line: 2, character: 11 },
            },
            severity: Some(DiagnosticSeverity::HINT),
            code: Some(NumberOrString::String("unresolved-function".into())),
            source: Some("perl-lsp".into()),
            message: "'frobnicate' is exported by Some::Module (not yet imported)".into(),
            data: Some(serde_json::json!({
                "modules": ["Some::Module"],
                "function": "frobnicate",
            })),
            ..Default::default()
        };

        let actions = code_actions(&[diag], &analysis, &uri);
        assert_eq!(actions.len(), 1);
        if let CodeActionOrCommand::CodeAction(action) = &actions[0] {
            assert_eq!(action.title, "Add 'use Some::Module qw(frobnicate)'");
            assert_eq!(action.is_preferred, Some(true));
            let edit = action.edit.as_ref().unwrap();
            let changes = edit.changes.as_ref().unwrap();
            let text_edits = changes.get(&uri).unwrap();
            assert_eq!(text_edits[0].new_text, "use Some::Module qw(frobnicate);\n");
            // Inserted after last use statement (line 2)
            assert_eq!(text_edits[0].range.start.line, 2);
        } else {
            panic!("Expected CodeAction");
        }
    }

    #[test]
    fn test_unimported_completion_with_auto_import() {
        let source = "use strict;\nuse warnings;\n\nfir\n";
        let analysis = parse_analysis(source);

        // Simulate a cached module that exports "first"
        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        // Insert directly into cache for testing
        idx.insert_cache(
            "List::Util",
            Some(fake_cached("/usr/lib/perl5/List/Util.pm", &[], &["first", "max", "min"])),
        );

        let tree = crate::document::Document::new(source.to_string()).unwrap().tree;
        let items = completion_items(
            &analysis,
            &tree,
            source,
            Position { line: 3, character: 3 },
            &idx,
            None,
        );

        // Should find "first" from List::Util
        let first_item = items.iter().find(|i| i.label == "first");
        assert!(first_item.is_some(), "Should offer 'first' from unimported List::Util");

        let first_item = first_item.unwrap();
        assert!(
            first_item.detail.as_ref().unwrap().contains("List::Util"),
            "Detail should mention the module"
        );
        assert!(
            first_item.detail.as_ref().unwrap().contains("auto-import"),
            "Detail should indicate auto-import"
        );

        // Should have additional text edit inserting `use List::Util qw(first);`
        let edits = first_item.additional_text_edits.as_ref().unwrap();
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "use List::Util qw(first);\n");
        // Should insert after the last use statement (line 2)
        assert_eq!(edits[0].range.start.line, 2);
    }

    #[test]
    fn test_unimported_completion_skips_imported_modules() {
        // List::Util is already imported — its exports should NOT appear as unimported completions
        let source = "use List::Util qw(max);\nfir\n";
        let analysis = parse_analysis(source);

        let idx = ModuleIndex::new_for_test();
        idx.set_workspace_root(None);
        idx.insert_cache(
            "List::Util",
            Some(fake_cached("/usr/lib/perl5/List/Util.pm", &[], &["first", "max", "min"])),
        );
        idx.insert_cache(
            "Scalar::Util",
            Some(fake_cached("/usr/lib/perl5/Scalar/Util.pm", &[], &["blessed", "reftype"])),
        );

        let tree = crate::document::Document::new(source.to_string()).unwrap().tree;
        let items = completion_items(
            &analysis,
            &tree,
            source,
            Position { line: 1, character: 3 },
            &idx,
            None,
        );

        // "first" should appear via imported_function_completions (auto-add to qw),
        // NOT via unimported_function_completions
        let first_items: Vec<_> = items.iter().filter(|i| i.label == "first").collect();
        assert!(!first_items.is_empty(), "Should offer 'first'");
        // It should come from the imported path (adds to qw) not unimported
        for item in &first_items {
            if let Some(ref detail) = item.detail {
                assert!(
                    !detail.contains("auto-import") || detail.contains("List::Util"),
                    "first should come from List::Util context"
                );
            }
        }

        // "blessed" should appear as unimported (Scalar::Util not imported)
        let blessed_item = items.iter().find(|i| i.label == "blessed");
        assert!(blessed_item.is_some(), "Should offer 'blessed' from unimported Scalar::Util");
        let blessed_item = blessed_item.unwrap();
        assert!(blessed_item.detail.as_ref().unwrap().contains("Scalar::Util"));
        let edits = blessed_item.additional_text_edits.as_ref().unwrap();
        assert!(edits[0].new_text.contains("use Scalar::Util qw(blessed)"));
    }

    #[test]
    fn test_code_action_multiple_exporters_not_preferred() {
        let source = "use strict;\nfirst();\n";
        let analysis = parse_analysis(source);
        let uri = Url::parse("file:///test.pl").unwrap();

        let diag = Diagnostic {
            range: Range {
                start: Position { line: 1, character: 0 },
                end: Position { line: 1, character: 5 },
            },
            severity: Some(DiagnosticSeverity::HINT),
            code: Some(NumberOrString::String("unresolved-function".into())),
            source: Some("perl-lsp".into()),
            message: "...".into(),
            data: Some(serde_json::json!({
                "modules": ["List::Util", "List::MoreUtils"],
                "function": "first",
            })),
            ..Default::default()
        };

        let actions = code_actions(&[diag], &analysis, &uri);
        assert_eq!(actions.len(), 2);
        // Neither should be preferred (ambiguous)
        for action in &actions {
            if let CodeActionOrCommand::CodeAction(a) = action {
                assert_eq!(a.is_preferred, Some(false));
            }
        }
    }

    // ---- String-dispatch signature help (mojo-events plugin path) ----

    /// `$self->emit('ready', CURSOR)` should surface the `->on('ready', sub
    /// ($self, $msg) {})` handler's params as sig help. The dispatch string
    /// is arg 0; handler params are offset by 1 so active_parameter lines
    /// up with the user's cursor.
    #[test]
    fn sig_help_returns_handler_params_for_emit() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub register {
    my $self = shift;
    $self->on('ready', sub {
        my ($self_in, $msg, $when) = @_;
        warn $msg;
    });
}

sub fire {
    my $self = shift;
    $self->emit('ready', 'hi', )
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        // Cursor just after `'hi', ` on the emit line — active_param=2 means
        // we're in the 2nd handler slot (after event name + first handler arg).
        let pos = {
            let (line_idx, line) = src.lines().enumerate()
                .find(|(_, l)| l.contains("->emit('ready'"))
                .unwrap();
            let col = line.find(", )").unwrap() + 2;
            Position { line: line_idx as u32, character: col as u32 }
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx)
            .expect("sig help should surface handler sig");
        assert_eq!(sig.signatures.len(), 1, "one registered handler");

        let s = &sig.signatures[0];
        // Label mirrors the actual call the user is writing — `emit('ready',
        // $msg, $when)` — not a fake method-call shape.
        assert!(s.label.starts_with("emit('ready'"),
            "label should show the call shape starting with emit('ready'): {}", s.label);
        // Documentation carries the class + line provenance.
        if let Some(Documentation::String(ref d)) = s.documentation {
            assert!(d.contains("My::Emitter"),
                "doc should name the owning class: {}", d);
        } else {
            panic!("expected Documentation::String, got {:?}", s.documentation);
        }
        // $self_in stripped as implicit → remaining params $msg, $when.
        let params = s.parameters.as_ref().expect("has params");
        assert_eq!(params.len(), 2, "drops implicit $self_in");
        assert!(matches!(&params[0].label, ParameterLabel::Simple(s) if s == "$msg"));
        assert!(matches!(&params[1].label, ParameterLabel::Simple(s) if s == "$when"));
    }

    /// Multiple `->on('ready', sub {...})` wire-ups stack — each becomes a
    /// separate SignatureInformation entry, so users see every handler
    /// shape they might be dispatching to.
    #[test]
    fn sig_help_stacks_multiple_handler_defs() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub new {
    my $self = bless {}, shift;
    $self->on('tick', sub {
        my ($self_in, $count) = @_;
    });
    $self->on('tick', sub {
        my ($self_in, $count, $unit) = @_;
    });
    $self;
}

sub go {
    my $self = shift;
    $self->emit('tick', )
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let pos = {
            let line_idx = src.lines().enumerate()
                .find(|(_, l)| l.contains("->emit('tick'"))
                .map(|(i, _)| i).unwrap();
            let line = src.lines().nth(line_idx).unwrap();
            let col = line.find(", )").unwrap() + 2;
            Position { line: line_idx as u32, character: col as u32 }
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx)
            .expect("sig help should fire");
        assert_eq!(sig.signatures.len(), 2,
            "stacked handlers: one signature per ->on call");

        let labels: Vec<&str> = sig.signatures.iter()
            .map(|s| s.label.as_str()).collect();
        assert!(labels.iter().all(|l| l.starts_with("emit('tick'")),
            "every signature uses emit('tick', ...) call shape: {:?}", labels);
    }

    /// Baseline before the user started typing: cursor in the empty
    /// second-arg slot `$self->emit('connect', CURSOR );`. Sig help
    /// should offer handler params from the moment the comma is typed.
    #[test]
    fn sig_help_fires_in_empty_second_slot() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    $self->emit('connect', );
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit('connect'"))
            .unwrap();
        let col = line.find(", )").unwrap() + 2; // just after `, `
        let pos = Position {
            line: line_idx as u32,
            character: col as u32,
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx)
            .expect("empty arg slot after comma should offer handler sig");
        let s = &sig.signatures[0];
        assert!(s.label.starts_with("emit('connect'"),
            "baseline: label identifies emit handler call: {}", s.label);
    }

    /// Flow gap fix: `my $dynamic = 'connect'; $self->emit($dynamic, ...)`
    /// — hover already worked (DispatchCall.target_name is const-folded
    /// by the plugin) but sig help used to miss because it parsed the
    /// first arg from text ($dynamic → not a literal). Now sig help
    /// routes through the DispatchCall ref too, so const folding
    /// composes uniformly and this class of gap can't reopen.
    #[test]
    fn sig_help_follows_const_folding_like_hover_does() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    my $dynamic = 'connect';
    $self->emit($dynamic, 'hi', );
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit($dynamic"))
            .unwrap();
        let col = line.find(", )").unwrap() + 2;
        let pos = Position {
            line: line_idx as u32,
            character: col as u32,
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx)
            .expect("sig help must follow const folding like hover does");
        let s = &sig.signatures[0];
        assert!(s.label.starts_with("emit('connect'"),
            "const-folded: $dynamic → 'connect' → emit('connect', ...) label; got: {}", s.label);
        let params = s.parameters.as_ref().unwrap();
        assert_eq!(params.len(), 2, "$sock, $remote_ip (implicit $self_in dropped)");
    }

    /// Regression: cursor inside the SECOND literal-string arg of a
    /// dispatch call — matches the user's screenshot where
    /// `$self->emit('connect', 'soc' )` had the cursor mid-'soc'. The
    /// string-dispatch sig should still fire (first arg is 'connect',
    /// handler is registered, active_param is 1).
    #[test]
    fn sig_help_fires_from_inside_second_string_arg() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
    });
}

sub fire {
    my $self = shift;
    $self->emit('connect', 'soc' );
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit('connect', 'soc'"))
            .unwrap();
        // Cursor at column index pointing into the middle of `'soc'`.
        let col = line.find("'soc'").unwrap() + 2; // between 's' and 'o'
        let pos = Position {
            line: line_idx as u32,
            character: col as u32,
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx)
            .expect("cursor in 2nd string arg should still surface handler sig");
        let s = &sig.signatures[0];
        assert!(s.label.starts_with("emit('connect'"),
            "label should still be the emit(handler) form: {}", s.label);
        let params = s.parameters.as_ref().unwrap();
        assert_eq!(params.len(), 2, "handler params ($sock, $remote_ip)");
    }

    /// Completion at the first arg of a dispatch call should list every
    /// registered Handler on the receiver's class — top priority, quoted
    /// insert, handler params in detail. Same abstraction as hover +
    /// sig help, so new plugins don't have to wire this up separately.
    #[test]
    fn completion_offers_handler_names_at_dispatch_arg0() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s, $sock, $ip) = @_; });
    $self->on('disconnect', sub { my ($s) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit();
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        // Cursor inside the empty `()` of `$self->emit()`.
        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit()"))
            .unwrap();
        let col = line.find("emit(").unwrap() + "emit(".len();
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);

        // Every registered handler shows up as a top-priority suggestion.
        let connect = items.iter().find(|i| i.label == "connect")
            .expect("connect handler should be offered at emit arg-0");
        let disconnect = items.iter().find(|i| i.label == "disconnect")
            .expect("disconnect handler should be offered at emit arg-0");

        assert_eq!(connect.kind, Some(CompletionItemKind::EVENT),
            "handler completion kind is EVENT (matches outline)");
        assert_eq!(connect.insert_text.as_deref(), Some("'connect'"),
            "insert should include quotes so the user doesn't type them");
        assert!(connect.detail.as_deref().unwrap_or("").contains("My::Emitter"),
            "detail should name the owning class: {:?}", connect.detail);
        assert!(connect.detail.as_deref().unwrap_or("").contains("$sock"),
            "detail should expose handler params: {:?}", connect.detail);

        // Sort text puts handlers ahead of other general completions.
        // Space prefix sorts lex-before any digit-prefixed sort_text,
        // guaranteeing handlers as a top block even when surrounding
        // items (local subs at PRIORITY_LOCAL=0) tie on numeric priority.
        assert!(connect.sort_text.as_deref().unwrap_or("zzz").starts_with(' '),
            "handler sort should lead with space to outrank digit-prefixed sort_text: {:?}",
            connect.sort_text);
        assert!(disconnect.sort_text.as_deref().unwrap_or("zzz").starts_with(' '));
    }

    /// Bug B: dispatch-target items set their `insert_text` to
    /// `'name'` (quoted) but left `filter_text` unset — some LSP
    /// clients fall back to `insert_text` for client-side prefix
    /// matching, so typing `c` after `(` fails to match `'connect'`
    /// (prefix starts with `'`, not `c`). `filter_text` now pins
    /// client-side matching to the bare label regardless of insert
    /// shape; typing a character keeps the handler visible.
    #[test]
    fn completion_dispatch_filter_text_matches_bare_name() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';
sub wire { my $self = shift; $self->on('connect', sub {}); }
sub fire { my $self = shift; $self->emit(); }
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit()"))
            .unwrap();
        let col = line.find("emit(").unwrap() + "emit(".len();
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        let connect = items.iter().find(|i| i.label == "connect")
            .expect("connect handler offered");

        // filter_text is the bare name — the client can prefix-match on
        // `c`/`co`/`con`/... even though insert_text is `'connect'`.
        assert_eq!(connect.filter_text.as_deref(), Some("connect"),
            "filter_text must be the bare label, not the quoted insert_text");
        assert_eq!(connect.insert_text.as_deref(), Some("'connect'"),
            "insert_text still quotes for the bare-parens case");
    }

    /// Bug: dispatch-target completion always wrapped the label in
    /// quotes — so if the cursor was already inside `''`, accepting
    /// `connect` inserted `''connect''`. Now detects the string
    /// context via the tree and emits bare text.
    #[test]
    fn completion_dispatch_inside_quotes_does_not_double_quote() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit('');
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        // Cursor BETWEEN the two quotes in `->emit('')`.
        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit('')"))
            .unwrap();
        let col = line.find("('").unwrap() + 2;
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        let connect = items.iter().find(|i| i.label == "connect")
            .expect("connect handler offered inside '|'");
        assert_eq!(connect.insert_text.as_deref(), Some("connect"),
            "cursor is inside quotes; insert_text must NOT wrap with more quotes");
    }

    /// Bug: typing `,` inside a known dispatch call (`->emit('x', |)`)
    /// triggered completion which ran the global sub/module firehose —
    /// useless here. Now suppresses imported/unimported function
    /// completions when we're inside a known dispatcher call; sig
    /// help remains the right affordance for guiding arg shape.
    #[test]
    fn completion_after_comma_in_dispatch_call_suppresses_firehose() {
        let src = r#"
package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire_one {}
sub wire_two {}
sub completely_unrelated {}

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s, $sock) = @_; });
}

sub fire {
    my $self = shift;
    $self->emit('connect', );
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->emit('connect',"))
            .unwrap();
        let col = line.find(", )").unwrap() + 2;
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        assert!(!labels.contains(&"completely_unrelated"),
            "unrelated sub must not appear in dispatch arg completion: {:?}",
            labels);
        assert!(!labels.contains(&"wire_one"),
            "wire_one leak — dispatch arg completion should stay quiet: {:?}",
            labels);
    }

    /// Mid-string completion for route targets. Cursor inside
    /// `->to('Users#lis|')` offers methods on Users, prefix-filtered
    /// by `lis`. Generic for ANY plugin that emits MethodCallRef at
    /// a string span (routes today, Catalyst forwards, etc.).
    #[test]
    fn completion_mid_string_route_target_scoped_to_invocant() {
        // Same-file Users package so the test is self-contained. Real
        // use would have Users in a separate file via workspace index;
        // the lookup path is the same (complete_methods_for_class
        // walks inheritance + module index).
        let src = r#"
package Users;
sub list {}
sub login {}
sub logout {}
sub delete_user {}

package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#lis');
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        // Cursor just after 'lis' in 'Users#lis' — active editing state.
        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("Users#lis")).unwrap();
        let col = line.find("Users#lis").unwrap() + "Users#lis".len();
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Prefix-filtered: only `list` starts with `lis`; `login`,
        // `logout`, `delete_user` don't.
        assert!(labels.contains(&"list"),
            "list must be offered for prefix `lis`: {:?}", labels);
        assert!(!labels.contains(&"login"),
            "login does NOT start with `lis` — must be filtered out: {:?}", labels);
        assert!(!labels.contains(&"logout"),
            "logout does NOT start with `lis` — must be filtered out: {:?}", labels);
        assert!(!labels.contains(&"delete_user"),
            "delete_user is unrelated — must not appear: {:?}", labels);

        // Top-priority sort — the mid-string completion path is the
        // only sensible one at this cursor position.
        let list = items.iter().find(|i| i.label == "list").unwrap();
        assert!(list.sort_text.as_deref().unwrap_or("zzz").starts_with("000"),
            "mid-string method completion should be top-priority: {:?}",
            list.sort_text);
    }

    /// Mid-string completion for routes before `#` is typed — cursor at
    /// `->to('Us|')`. The invocant portion isn't complete yet, so the
    /// plugin won't have emitted a MethodCallRef. Graceful fallthrough
    /// to general completion (or nothing) is the expected behavior.
    #[test]
    fn completion_mid_string_before_hash_falls_through() {
        let src = r#"
package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Us');
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("'Us'")).unwrap();
        let col = line.find("'Us'").unwrap() + "'Us".len();
        let pos = Position { line: line_idx as u32, character: col as u32 };

        // Doesn't assert what IS offered — just that it doesn't panic
        // and doesn't return complete nonsense. This is the honest
        // edge-case: without a `#` yet, no plugin-emitted ref exists.
        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        let _ = items;
    }

    /// Completion skips when the method isn't a declared dispatcher, even
    /// if handlers exist on the class. (Empty dispatchers == "any" by
    /// convention, but mojo-events declares ["emit"] specifically.)
    #[test]
    fn completion_skips_non_dispatcher_method() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub wire {
    my $self = shift;
    $self->on('connect', sub { my ($s) = @_; });
}

sub other {
    my $self = shift;
    $self->unrelated_method();
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let (line_idx, line) = src.lines().enumerate()
            .find(|(_, l)| l.contains("->unrelated_method()"))
            .unwrap();
        let col = line.find("method(").unwrap() + "method(".len();
        let pos = Position { line: line_idx as u32, character: col as u32 };

        let items = completion_items(&analysis, &tree, src, pos, &idx, None);
        assert!(!items.iter().any(|i| i.label == "connect"),
            "non-dispatcher method must not surface handler completions");
    }

    /// No handler params means no specialized sig help — fall through to
    /// the regular method-signature path (or return None if ->emit isn't
    /// locally defined, as in this test).
    #[test]
    fn sig_help_returns_none_when_no_handler_registered() {
        let src = r#"package My::Emitter;
use parent 'Mojo::EventEmitter';

sub fire {
    my $self = shift;
    $self->emit('never_registered', )
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(src, None).unwrap();
        let analysis = crate::builder::build(&tree, src.as_bytes());
        let idx = ModuleIndex::new_for_test();

        let pos = {
            let line_idx = src.lines().enumerate()
                .find(|(_, l)| l.contains("never_registered"))
                .map(|(i, _)| i).unwrap();
            let line = src.lines().nth(line_idx).unwrap();
            let col = line.find(", )").unwrap() + 2;
            Position { line: line_idx as u32, character: col as u32 }
        };

        let sig = signature_help(&analysis, &tree, src, pos, &idx);
        assert!(sig.is_none(),
            "no handler_params → no string-dispatch sig; also no local ->emit def");
    }
}
