use std::collections::HashMap;
use tower_lsp::lsp_types::*;
use tree_sitter::{Point, Tree};

use crate::analysis;

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

fn symbol_kind_to_lsp(kind: &analysis::SymbolKind) -> SymbolKind {
    match kind {
        analysis::SymbolKind::Function => SymbolKind::FUNCTION,
        analysis::SymbolKind::Method => SymbolKind::METHOD,
        analysis::SymbolKind::Variable => SymbolKind::VARIABLE,
        analysis::SymbolKind::Package => SymbolKind::NAMESPACE,
        analysis::SymbolKind::Class => SymbolKind::CLASS,
        analysis::SymbolKind::Module => SymbolKind::MODULE,
    }
}

#[allow(deprecated)]
fn to_document_symbol(s: &analysis::SymbolInfo) -> DocumentSymbol {
    let children: Vec<DocumentSymbol> = s.children.iter().map(to_document_symbol).collect();
    DocumentSymbol {
        name: s.name.clone(),
        detail: s.detail.clone(),
        kind: symbol_kind_to_lsp(&s.kind),
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
pub fn extract_symbols(tree: &Tree, source: &str) -> Vec<DocumentSymbol> {
    analysis::extract_symbols(tree, source)
        .iter()
        .map(to_document_symbol)
        .collect()
}

pub fn find_definition(
    tree: &Tree,
    source: &str,
    pos: Position,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let span = analysis::find_definition(tree, source, position_to_point(pos))?;
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range: span_to_range(span),
    }))
}

pub fn find_references(tree: &Tree, source: &str, pos: Position, uri: &Url) -> Vec<Location> {
    analysis::find_references(tree, source, position_to_point(pos))
        .into_iter()
        .map(|span| Location {
            uri: uri.clone(),
            range: span_to_range(span),
        })
        .collect()
}

pub fn rename(
    tree: &Tree,
    source: &str,
    pos: Position,
    uri: &Url,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let edits = analysis::rename(tree, source, position_to_point(pos), new_name)?;

    let text_edits: Vec<TextEdit> = edits
        .into_iter()
        .map(|e| TextEdit {
            range: span_to_range(e.span),
            new_text: e.new_text,
        })
        .collect();

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), text_edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

fn completion_kind(kind: &analysis::SymbolKind) -> CompletionItemKind {
    match kind {
        analysis::SymbolKind::Function => CompletionItemKind::FUNCTION,
        analysis::SymbolKind::Method => CompletionItemKind::METHOD,
        analysis::SymbolKind::Variable => CompletionItemKind::VARIABLE,
        analysis::SymbolKind::Package => CompletionItemKind::CLASS,
        analysis::SymbolKind::Class => CompletionItemKind::CLASS,
        analysis::SymbolKind::Module => CompletionItemKind::MODULE,
    }
}

pub fn completion_items(tree: &Tree, source: &str, pos: Position) -> Vec<CompletionItem> {
    let candidates = analysis::collect_completions(tree, source, position_to_point(pos));

    candidates
        .into_iter()
        .map(|c| CompletionItem {
            label: c.label,
            kind: Some(completion_kind(&c.kind)),
            detail: c.detail,
            insert_text: c.insert_text,
            sort_text: Some(format!("{:03}", c.sort_priority)),
            ..Default::default()
        })
        .collect()
}

pub fn hover_info(tree: &Tree, source: &str, pos: Position) -> Option<Hover> {
    let result = analysis::hover_info(tree, source, position_to_point(pos))?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: result.markdown,
        }),
        range: None,
    })
}

pub fn document_highlights(tree: &Tree, source: &str, pos: Position) -> Vec<DocumentHighlight> {
    analysis::document_highlights(tree, source, position_to_point(pos))
        .into_iter()
        .map(|h| DocumentHighlight {
            range: span_to_range(h.span),
            kind: Some(match h.kind {
                analysis::HighlightKind::Write => DocumentHighlightKind::WRITE,
                analysis::HighlightKind::Read => DocumentHighlightKind::READ,
            }),
        })
        .collect()
}

pub fn selection_ranges(tree: &Tree, pos: Position) -> SelectionRange {
    let spans = analysis::selection_ranges(tree, position_to_point(pos));
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

pub fn folding_ranges(tree: &Tree, source: &str) -> Vec<FoldingRange> {
    analysis::folding_ranges(tree, source)
        .into_iter()
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

pub fn signature_help(tree: &Tree, text: &str, pos: Position) -> Option<SignatureHelp> {
    let result = analysis::signature_help_at_point(tree, text, position_to_point(pos))?;
    let sig = &result.signature;

    let params: Vec<ParameterInformation> = sig
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
        sig.name,
        sig.params
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
            active_parameter: Some(result.active_param as u32),
        }],
        active_signature: Some(0),
        active_parameter: Some(result.active_param as u32),
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

pub fn semantic_tokens(tree: &Tree, source: &str) -> Vec<SemanticToken> {
    let var_tokens = analysis::collect_semantic_tokens(tree, source);

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
            analysis::VarModifier::Scalar => modifiers |= 1 << TOKEN_MOD_SCALAR,
            analysis::VarModifier::Array => modifiers |= 1 << TOKEN_MOD_ARRAY,
            analysis::VarModifier::Hash => modifiers |= 1 << TOKEN_MOD_HASH,
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

pub fn collect_diagnostics(tree: &Tree, source: &str) -> Vec<Diagnostic> {
    analysis::collect_diagnostics(tree, source)
        .into_iter()
        .map(|d| Diagnostic {
            range: span_to_range(d.span),
            severity: Some(match d.severity {
                analysis::DiagnosticSeverity::Error => tower_lsp::lsp_types::DiagnosticSeverity::ERROR,
                analysis::DiagnosticSeverity::Warning => tower_lsp::lsp_types::DiagnosticSeverity::WARNING,
            }),
            source: Some("perl-lsp".to_string()),
            message: d.message,
            ..Default::default()
        })
        .collect()
}
