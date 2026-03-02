use std::collections::HashMap;
use tower_lsp::lsp_types::*;
use tree_sitter::{Point, Tree};

use crate::analysis;
use crate::cursor_context::{self, CursorContext};
use crate::file_analysis::{
    CompletionCandidate, FileAnalysis, OutlineSymbol, RefKind, SymKind as FaSymKind, VarModifier,
};
use crate::module_index::ModuleIndex;

// ---- Coordinate conversion ----

fn point_to_position(p: Point) -> Position {
    Position {
        line: p.row as u32,
        character: p.column as u32,
    }
}

fn position_to_point(pos: Position) -> Point {
    Point::new(pos.line as usize, pos.character as usize)
}

fn span_to_range(span: analysis::Span) -> Range {
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
    }
}

#[allow(deprecated)]
fn outline_to_document_symbol(s: &OutlineSymbol) -> DocumentSymbol {
    let children: Vec<DocumentSymbol> = s.children.iter().map(outline_to_document_symbol).collect();
    DocumentSymbol {
        name: s.name.clone(),
        detail: s.detail.clone(),
        kind: fa_sym_kind_to_lsp(&s.kind),
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

pub fn find_definition(
    analysis: &FileAnalysis,
    pos: Position,
    uri: &Url,
    module_index: &ModuleIndex,
) -> Option<GotoDefinitionResponse> {
    let point = position_to_point(pos);

    // Try local definition first
    if let Some(span) = analysis.find_definition(point) {
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
                    return Some(GotoDefinitionResponse::Array(vec![
                        // First: the use statement in this file
                        Location {
                            uri: uri.clone(),
                            range: span_to_range(import.span),
                        },
                        // Second: the .pm file itself (line 1)
                        Location {
                            uri: module_uri,
                            range: Range::default(),
                        },
                    ]));
                }
                // Fall back to just the use statement
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: span_to_range(import.span),
                }));
            }
        }
    }

    None
}

pub fn find_references(analysis: &FileAnalysis, pos: Position, uri: &Url) -> Vec<Location> {
    analysis.find_references(position_to_point(pos))
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
) -> Option<WorkspaceEdit> {
    let edits = analysis.rename_at(position_to_point(pos), new_name)?;

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
    CompletionItem {
        label: c.label,
        kind: Some(fa_completion_kind(&c.kind)),
        detail: c.detail,
        insert_text: c.insert_text,
        sort_text: Some(format!("{:03}", c.sort_priority)),
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
) -> Vec<CompletionItem> {
    let point = position_to_point(pos);
    let ctx = cursor_context::detect_cursor_context(source, point);

    let mut candidates: Vec<CompletionCandidate> = match ctx {
        CursorContext::Variable { sigil } => analysis.complete_variables(point, sigil),
        CursorContext::Method { ref invocant } => analysis.complete_methods(invocant, point),
        CursorContext::HashKey { ref var_text } => analysis.complete_hash_keys(var_text, point),
        CursorContext::General => {
            let mut items = Vec::new();
            // Keyval arg completions if inside a call at key position
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
                    ));
                }
            }
            items.extend(analysis.complete_general(point));

            // Add imported function completions
            items.extend(imported_function_completions(analysis, module_index));

            items
        }
    };

    candidates
        .drain(..)
        .map(candidate_to_completion_item)
        .collect()
}

pub fn hover_info(
    analysis: &FileAnalysis,
    source: &str,
    pos: Position,
    module_index: &ModuleIndex,
) -> Option<Hover> {
    let point = position_to_point(pos);

    // Try local hover first
    if let Some(markdown) = analysis.hover_info(point, source) {
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
                let markdown = format!(
                    "```perl\nuse {} qw({});\n```\n\n*imported from `{}`*",
                    import.module_name, r.target_name, import.module_name,
                );
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

pub fn document_highlights(analysis: &FileAnalysis, pos: Position) -> Vec<DocumentHighlight> {
    use crate::file_analysis::AccessKind;
    analysis.find_highlights(position_to_point(pos))
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
                analysis::FoldKind::Region => FoldingRangeKind::Region,
                analysis::FoldKind::Comment => FoldingRangeKind::Comment,
            }),
            collapsed_text: None,
        })
        .collect()
}

// ---- Signature help ----

pub fn signature_help(
    analysis: &FileAnalysis,
    tree: &Tree,
    text: &str,
    pos: Position,
) -> Option<SignatureHelp> {
    let point = position_to_point(pos);

    // Step 1: cursor_context finds the enclosing call
    let call_ctx = cursor_context::find_call_context(tree, text.as_bytes(), point)?;

    // Step 2: file_analysis resolves the sub signature (pure table lookup)
    let sig_info = analysis.signature_for_call(
        &call_ctx.name,
        call_ctx.is_method,
        call_ctx.invocant.as_deref(),
        point,
    )?;

    let params: Vec<ParameterInformation> = sig_info
        .params
        .iter()
        .map(|p| {
            let label = if let Some(ref default) = p.default {
                format!("{} = {}", p.name, default)
            } else {
                p.name.clone()
            };
            ParameterInformation {
                label: ParameterLabel::Simple(label),
                documentation: None,
            }
        })
        .collect();

    let label = format!(
        "{}({})",
        sig_info.name,
        sig_info
            .params
            .iter()
            .map(|p| {
                if let Some(ref default) = p.default {
                    format!("{} = {}", p.name, default)
                } else {
                    p.name.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    );

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: None,
            parameters: Some(params),
            active_parameter: Some(call_ctx.active_param as u32),
        }],
        active_signature: Some(0),
        active_parameter: Some(call_ctx.active_param as u32),
    })
}

// ---- Semantic tokens ----

// Legend indices — must match the order in SEMANTIC_TOKEN_TYPES / SEMANTIC_TOKEN_MODIFIERS
pub const TOKEN_TYPE_VARIABLE: u32 = 0;

pub const TOKEN_MOD_DECLARATION: u32 = 0; // bit 0
pub const TOKEN_MOD_READONLY: u32 = 1;    // bit 1
pub const TOKEN_MOD_MODIFICATION: u32 = 2; // bit 2
pub const TOKEN_MOD_SCALAR: u32 = 3;       // bit 3
pub const TOKEN_MOD_ARRAY: u32 = 4;        // bit 4
pub const TOKEN_MOD_HASH: u32 = 5;         // bit 5

pub fn semantic_token_types() -> Vec<SemanticTokenType> {
    vec![SemanticTokenType::VARIABLE]
}

pub fn semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::MODIFICATION,
        SemanticTokenModifier::new("scalar"),
        SemanticTokenModifier::new("array"),
        SemanticTokenModifier::new("hash"),
    ]
}

pub fn semantic_tokens(analysis: &FileAnalysis) -> Vec<SemanticToken> {
    let var_tokens = analysis.semantic_tokens();

    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for vt in &var_tokens {
        let line = vt.span.start.row as u32;
        let start = vt.span.start.column as u32;
        let length = if vt.span.start.row == vt.span.end.row {
            vt.span.end.column as u32 - start
        } else {
            1 // multi-line token, shouldn't happen for variables
        };

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        let mut modifiers: u32 = 0;
        if vt.is_declaration {
            modifiers |= 1 << TOKEN_MOD_DECLARATION;
        }
        if vt.is_readonly {
            modifiers |= 1 << TOKEN_MOD_READONLY;
        }
        if vt.is_modification {
            modifiers |= 1 << TOKEN_MOD_MODIFICATION;
        }
        match vt.var_type {
            VarModifier::Scalar => modifiers |= 1 << TOKEN_MOD_SCALAR,
            VarModifier::Array => modifiers |= 1 << TOKEN_MOD_ARRAY,
            VarModifier::Hash => modifiers |= 1 << TOKEN_MOD_HASH,
        }

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: TOKEN_TYPE_VARIABLE,
            token_modifiers_bitset: modifiers,
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
    use crate::analysis::Span;
    let mut candidates = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for import in &analysis.imports {
        let exports = module_index.get_exports_cached(&import.module_name);

        // 1. Always offer explicitly imported symbols (from the qw list)
        for name in &import.imported_symbols {
            if !seen.insert(name.clone()) {
                continue;
            }
            if !analysis.symbols_named(name).is_empty() {
                continue;
            }
            candidates.push(CompletionCandidate {
                label: name.clone(),
                kind: FaSymKind::Sub,
                detail: Some(format!("imported from {}", import.module_name)),
                insert_text: None,
                sort_priority: 12,
                additional_edits: vec![],
            });
        }

        // 2. Offer additional @EXPORT_OK functions (not yet imported) if we can resolve the module
        if let Some(ref exports) = exports {
            let all_exported: Vec<&String> = if import.imported_symbols.is_empty() {
                // Bare `use Foo;` — offer @EXPORT
                exports.export.iter().collect()
            } else {
                // `use Foo qw(bar)` — offer remaining @EXPORT + @EXPORT_OK
                let mut all = Vec::new();
                all.extend(exports.export.iter());
                all.extend(exports.export_ok.iter());
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

                let (detail, priority, additional_edits) =
                    if let Some(close_pos) = import.qw_close_paren {
                        // Auto-add to existing qw() list
                        let insert_point = Span {
                            start: close_pos,
                            end: close_pos,
                        };
                        (
                            format!("{} (auto-import)", import.module_name),
                            18u8,
                            vec![(insert_point, format!(" {}", name))],
                        )
                    } else {
                        // No qw() list to edit (bare `use Foo;`)
                        (
                            format!("imported from {}", import.module_name),
                            15u8,
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
                });
            }
        }
    }

    candidates
}

/// Find which import provides a given function name.
/// Checks both explicitly imported symbols and all @EXPORT/@EXPORT_OK
/// from already-imported modules.
fn resolve_imported_function<'a>(
    analysis: &'a FileAnalysis,
    func_name: &str,
    module_index: &ModuleIndex,
) -> Option<(&'a crate::file_analysis::Import, std::path::PathBuf)> {
    for import in &analysis.imports {
        // Check explicit import list first
        if import.imported_symbols.iter().any(|s| s == func_name) {
            if let Some(path) = module_index.module_path_cached(&import.module_name) {
                return Some((import, path));
            }
        }

        // Check module's export lists
        if let Some(exports) = module_index.get_exports_cached(&import.module_name) {
            let in_exports = exports.export.contains(&func_name.to_string())
                || exports.export_ok.contains(&func_name.to_string());
            if in_exports {
                if let Some(path) = module_index.module_path_cached(&import.module_name) {
                    return Some((import, path));
                }
            }
        }
    }
    None
}

// ---- Diagnostics ----

pub fn collect_diagnostics(_analysis: &FileAnalysis) -> Vec<Diagnostic> {
    // Diagnostic infrastructure — currently a stub.
    // Will be populated as FileAnalysis gains diagnostic capabilities.
    Vec::new()
}
