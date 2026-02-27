use std::collections::HashMap;
use tower_lsp::lsp_types::*;
use tree_sitter::{Point, Tree};

use crate::analysis;
use crate::cursor_context::{self, CursorContext};
use crate::file_analysis::{
    CompletionCandidate, FileAnalysis, OutlineSymbol, SymKind as FaSymKind, VarModifier,
};

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
) -> Option<GotoDefinitionResponse> {
    let span = analysis.find_definition(position_to_point(pos))?;
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range: span_to_range(span),
    }))
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
    CompletionItem {
        label: c.label,
        kind: Some(fa_completion_kind(&c.kind)),
        detail: c.detail,
        insert_text: c.insert_text,
        sort_text: Some(format!("{:03}", c.sort_priority)),
        ..Default::default()
    }
}

pub fn completion_items(
    analysis: &FileAnalysis,
    tree: &Tree,
    source: &str,
    pos: Position,
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
            items
        }
    };

    candidates
        .drain(..)
        .map(candidate_to_completion_item)
        .collect()
}

pub fn hover_info(analysis: &FileAnalysis, source: &str, pos: Position) -> Option<Hover> {
    let markdown = analysis.hover_info(position_to_point(pos), source)?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        }),
        range: None,
    })
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

// ---- Diagnostics ----

pub fn collect_diagnostics(_analysis: &FileAnalysis) -> Vec<Diagnostic> {
    // Diagnostic infrastructure — currently a stub.
    // Will be populated as FileAnalysis gains diagnostic capabilities.
    Vec::new()
}
