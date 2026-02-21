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
