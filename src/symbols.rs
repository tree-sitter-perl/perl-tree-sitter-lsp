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

            // Add completions from unimported modules (with auto-import edits)
            items.extend(unimported_function_completions(analysis, module_index));

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

/// Build completion candidates for functions from modules that aren't imported
/// at all. Each candidate carries an `additional_edit` that inserts a full
/// `use Module qw(func);` statement.
fn unimported_function_completions(
    analysis: &FileAnalysis,
    module_index: &ModuleIndex,
) -> Vec<CompletionCandidate> {
    use crate::analysis::Span;
    let mut candidates = Vec::new();

    // Collect already-imported module names so we skip them.
    let imported_modules: std::collections::HashSet<&str> = analysis
        .imports
        .iter()
        .map(|i| i.module_name.as_str())
        .collect();

    let insert_pos = find_use_insertion_position(analysis);
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

    module_index.for_each_cached(|module_name, exports| {
        if imported_modules.contains(module_name) {
            return;
        }

        let all_exported = exports.export.iter().chain(exports.export_ok.iter());
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
                sort_priority: 25, // Lower than local (5) and already-imported (12/15/18)
                additional_edits: vec![(
                    insert_span,
                    format!("use {} qw({});\n", module_name, name),
                )],
            });
        }
    });

    // Sort for deterministic order
    candidates.sort_by(|a, b| a.label.cmp(&b.label).then(a.detail.cmp(&b.detail)));
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

        // Skip Perl builtins
        if is_perl_builtin(name) {
            continue;
        }

        // Skip locally defined subs
        if !analysis.symbols_named(name).is_empty() {
            continue;
        }

        // Skip if explicitly listed in any import's qw(...)
        let explicitly_imported = analysis
            .imports
            .iter()
            .any(|imp| imp.imported_symbols.iter().any(|s| s == name));
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

    diagnostics
}

// ---- Code actions ----

/// Find the position to insert a new `use` statement.
/// Returns the start of the line after the last existing `use`.
fn find_use_insertion_position(analysis: &FileAnalysis) -> Position {
    if let Some(last_import) = analysis.imports.last() {
        Position {
            line: last_import.span.end.row as u32 + 1,
            character: 0,
        }
    } else {
        Position {
            line: 0,
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
            let insert_pos = find_use_insertion_position(analysis);
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
            .set_language(&tree_sitter_perl::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        builder::build(&tree, source.as_bytes())
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
        let module_index = crate::module_index::ModuleIndex::new();
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
        let module_index = crate::module_index::ModuleIndex::new();
        let diags = collect_diagnostics(&analysis, &module_index);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::INFORMATION));
        assert!(diags[0].message.contains("frobnicate"));
    }

    #[test]
    fn test_diagnostics_skips_local_sub() {
        let source = "sub helper { 1 }\nhelper();\n";
        let analysis = parse_analysis(source);
        let module_index = crate::module_index::ModuleIndex::new();
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
        let module_index = crate::module_index::ModuleIndex::new();
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
        let idx = ModuleIndex::new();
        idx.set_workspace_root(None);
        // Insert directly into cache for testing
        idx.insert_cache(
            "List::Util",
            Some(crate::module_index::ModuleExports {
                path: std::path::PathBuf::from("/usr/lib/perl5/List/Util.pm"),
                export: vec![],
                export_ok: vec!["first".into(), "max".into(), "min".into()],
            }),
        );

        let tree = crate::document::Document::new(source.to_string()).unwrap().tree;
        let items = completion_items(
            &analysis,
            &tree,
            source,
            Position { line: 3, character: 3 },
            &idx,
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

        let idx = ModuleIndex::new();
        idx.set_workspace_root(None);
        idx.insert_cache(
            "List::Util",
            Some(crate::module_index::ModuleExports {
                path: std::path::PathBuf::from("/usr/lib/perl5/List/Util.pm"),
                export: vec![],
                export_ok: vec!["first".into(), "max".into(), "min".into()],
            }),
        );
        idx.insert_cache(
            "Scalar::Util",
            Some(crate::module_index::ModuleExports {
                path: std::path::PathBuf::from("/usr/lib/perl5/Scalar/Util.pm"),
                export: vec![],
                export_ok: vec!["blessed".into(), "reftype".into()],
            }),
        );

        let tree = crate::document::Document::new(source.to_string()).unwrap().tree;
        let items = completion_items(
            &analysis,
            &tree,
            source,
            Position { line: 1, character: 3 },
            &idx,
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
}
