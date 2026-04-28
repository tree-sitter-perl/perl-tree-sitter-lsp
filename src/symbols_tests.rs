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
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 4,
            },
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
            start: Position {
                line: 2,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 11,
            },
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
        Some(fake_cached(
            "/usr/lib/perl5/List/Util.pm",
            &[],
            &["first", "max", "min"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position {
            line: 3,
            character: 3,
        },
        &idx,
        None,
    );

    // Should find "first" from List::Util
    let first_item = items.iter().find(|i| i.label == "first");
    assert!(
        first_item.is_some(),
        "Should offer 'first' from unimported List::Util"
    );

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
        Some(fake_cached(
            "/usr/lib/perl5/List/Util.pm",
            &[],
            &["first", "max", "min"],
        )),
    );
    idx.insert_cache(
        "Scalar::Util",
        Some(fake_cached(
            "/usr/lib/perl5/Scalar/Util.pm",
            &[],
            &["blessed", "reftype"],
        )),
    );

    let tree = crate::document::Document::new(source.to_string())
        .unwrap()
        .tree;
    let items = completion_items(
        &analysis,
        &tree,
        source,
        Position {
            line: 1,
            character: 3,
        },
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
    assert!(
        blessed_item.is_some(),
        "Should offer 'blessed' from unimported Scalar::Util"
    );
    let blessed_item = blessed_item.unwrap();
    assert!(blessed_item
        .detail
        .as_ref()
        .unwrap()
        .contains("Scalar::Util"));
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
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 5,
            },
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor just after `'hi', ` on the emit line — active_param=2 means
    // we're in the 2nd handler slot (after event name + first handler arg).
    let pos = {
        let (line_idx, line) = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("->emit('ready'"))
            .unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx)
        .expect("sig help should surface handler sig");
    assert_eq!(sig.signatures.len(), 1, "one registered handler");

    let s = &sig.signatures[0];
    // Label mirrors the actual call the user is writing — `emit('ready',
    // $msg, $when)` — not a fake method-call shape.
    assert!(
        s.label.starts_with("emit('ready'"),
        "label should show the call shape starting with emit('ready'): {}",
        s.label
    );
    // Documentation carries the class + line provenance.
    if let Some(Documentation::String(ref d)) = s.documentation {
        assert!(
            d.contains("My::Emitter"),
            "doc should name the owning class: {}",
            d
        );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let pos = {
        let line_idx = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("->emit('tick'"))
            .map(|(i, _)| i)
            .unwrap();
        let line = src.lines().nth(line_idx).unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx).expect("sig help should fire");
    assert_eq!(
        sig.signatures.len(),
        2,
        "stacked handlers: one signature per ->on call"
    );

    let labels: Vec<&str> = sig.signatures.iter().map(|s| s.label.as_str()).collect();
    assert!(
        labels.iter().all(|l| l.starts_with("emit('tick'")),
        "every signature uses emit('tick', ...) call shape: {:?}",
        labels
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
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
    assert!(
        s.label.starts_with("emit('connect'"),
        "baseline: label identifies emit handler call: {}",
        s.label
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
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
    assert!(
        s.label.starts_with("emit('connect'"),
        "const-folded: $dynamic → 'connect' → emit('connect', ...) label; got: {}",
        s.label
    );
    let params = s.parameters.as_ref().unwrap();
    assert_eq!(
        params.len(),
        2,
        "$sock, $remote_ip (implicit $self_in dropped)"
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
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
    assert!(
        s.label.starts_with("emit('connect'"),
        "label should still be the emit(handler) form: {}",
        s.label
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor inside the empty `()` of `$self->emit()`.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit()"))
        .unwrap();
    let col = line.find("emit(").unwrap() + "emit(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);

    // Every registered handler shows up as a top-priority suggestion.
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler should be offered at emit arg-0");
    let disconnect = items
        .iter()
        .find(|i| i.label == "disconnect")
        .expect("disconnect handler should be offered at emit arg-0");

    assert_eq!(
        connect.kind,
        Some(CompletionItemKind::EVENT),
        "handler completion kind is EVENT (matches outline)"
    );
    assert_eq!(
        connect.insert_text.as_deref(),
        Some("'connect'"),
        "insert should include quotes so the user doesn't type them"
    );
    assert!(
        connect
            .detail
            .as_deref()
            .unwrap_or("")
            .contains("My::Emitter"),
        "detail should name the owning class: {:?}",
        connect.detail
    );
    assert!(
        connect.detail.as_deref().unwrap_or("").contains("$sock"),
        "detail should expose handler params: {:?}",
        connect.detail
    );

    // Sort text puts handlers ahead of other general completions.
    // Space prefix sorts lex-before any digit-prefixed sort_text,
    // guaranteeing handlers as a top block even when surrounding
    // items (local subs at PRIORITY_LOCAL=0) tie on numeric priority.
    assert!(
        connect
            .sort_text
            .as_deref()
            .unwrap_or("zzz")
            .starts_with(' '),
        "handler sort should lead with space to outrank digit-prefixed sort_text: {:?}",
        connect.sort_text
    );
    assert!(disconnect
        .sort_text
        .as_deref()
        .unwrap_or("zzz")
        .starts_with(' '));
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit()"))
        .unwrap();
    let col = line.find("emit(").unwrap() + "emit(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler offered");

    // filter_text is the bare name — the client can prefix-match on
    // `c`/`co`/`con`/... even though insert_text is `'connect'`.
    assert_eq!(
        connect.filter_text.as_deref(),
        Some("connect"),
        "filter_text must be the bare label, not the quoted insert_text"
    );
    assert_eq!(
        connect.insert_text.as_deref(),
        Some("'connect'"),
        "insert_text still quotes for the bare-parens case"
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor BETWEEN the two quotes in `->emit('')`.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('')"))
        .unwrap();
    let col = line.find("('").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let connect = items
        .iter()
        .find(|i| i.label == "connect")
        .expect("connect handler offered inside '|'");
    // Cursor inside a string arg → item ships a textEdit pinned
    // to the string-content span so the client's word-at-cursor
    // heuristic can't drop it over non-identifier chars. The
    // newText is the BARE handler name (no wrapping quotes) and
    // insert_text is cleared — textEdit takes precedence in the
    // LSP spec, and leaving insert_text alongside confuses some
    // clients. The original "don't double-quote" invariant now
    // reads off textEdit.newText instead of insert_text.
    assert_eq!(
        connect.insert_text, None,
        "cursor is inside quotes; insert_text is cleared in favor of textEdit"
    );
    use tower_lsp::lsp_types::CompletionTextEdit;
    let Some(CompletionTextEdit::Edit(ref te)) = connect.text_edit else {
        panic!(
            "expected a TextEdit for mid-string dispatch item; got {:?}",
            connect.text_edit
        );
    };
    assert_eq!(
        te.new_text, "connect",
        "textEdit.newText is the bare label — not `'connect'` (would double-quote inside '|')"
    );
}

/// Red pin (user-reported): dispatch-target completions for labels
/// containing non-identifier chars (`/`, `#`) died client-side
/// because nvim's word-at-cursor heuristic uses `iskeyword`, which
/// excludes `/` and `#` by default. The server returned the item
/// with `filter_text = "/users/profile"` but the client extracted
/// `users` or `profile` (a word run starting/ending at the non-
/// keyword boundary) and dropped the item since neither is a
/// prefix of `/users/profile`. Same shape for `Users#list` — the
/// cursor parked past the `#` gave word `list`, which fails
/// `"Users#list".starts_with("list")`.
///
/// Fix: emit `textEdit` with `range = string_content_span_at(...)`
/// so the client filters by the whole in-range text against the
/// full label — regardless of keyword class. This pin locks that
/// textEdit emission for BOTH flavors; regressing either re-
/// surfaces the bug for any route with a URL path or `Ctrl#act`
/// handler name.
#[test]
fn completion_dispatch_textedit_handles_non_keyword_labels() {
    use crate::module_index::ModuleIndex;
    use tower_lsp::lsp_types::CompletionTextEdit;

    // Route declarations: one URL path (leading `/`), one
    // `Ctrl#act` (embedded `#`). Both must survive mid-string
    // completion inside `url_for('...')`.
    let app_src = r#"package MyApp;
use Mojolicious::Lite;

my $r = app->routes;
$r->get('/users')->to('Users#list');

get '/users/profile' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(app_src, None).unwrap()
        },
        app_src.as_bytes(),
    ));

    let idx = std::sync::Arc::new(ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);

    let ctrl_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('/users/profile');
}
"#;
    let ctrl_fa = crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(ctrl_src, None).unwrap()
        },
        ctrl_src.as_bytes(),
    );

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(ctrl_src, None).unwrap();

    // Cursor deep inside the path, past the first `/` —
    // `'/users/pr|ofile'`. Before the fix, nvim would extract
    // `users` or `profile` from the chars around the cursor;
    // neither is a prefix of the `/users/profile` label.
    let line_idx = 5u32; // `    $c->url_for('/users/profile');`
    let line = ctrl_src.lines().nth(line_idx as usize).unwrap();
    let quote_start = line.find("'/users/profile").unwrap();
    let pr_col = (quote_start + 1 + "/users/pr".len()) as u32;
    let pos = Position {
        line: line_idx,
        character: pr_col,
    };

    let items = completion_items(&ctrl_fa, &tree, ctrl_src, pos, &idx, None);

    let path_item = items
        .iter()
        .find(|i| i.label == "/users/profile")
        .expect("/users/profile must be offered (dispatch completion inside string)");

    // insert_text is cleared; textEdit carries the range spanning
    // the entire string content, so the client uses `/users/pr...`
    // as the filter input and matches against the full label.
    assert_eq!(
        path_item.insert_text, None,
        "insert_text cleared — textEdit takes precedence for non-keyword-char labels"
    );
    let Some(CompletionTextEdit::Edit(ref te)) = path_item.text_edit else {
        panic!(
            "expected textEdit for `/users/profile`; got {:?}",
            path_item.text_edit
        );
    };
    assert_eq!(
        te.new_text, "/users/profile",
        "textEdit.newText is the bare label, no surrounding quotes"
    );
    // Range must span the string CONTENT (between the quotes).
    // Start column = col of first `/` (just after opening quote),
    // end column = col of closing quote.
    assert_eq!(te.range.start.line, line_idx);
    assert_eq!(te.range.end.line, line_idx);
    assert_eq!(
        te.range.start.character,
        (quote_start + 1) as u32,
        "range start hugs the char just after the opening quote",
    );
    assert_eq!(
        te.range.end.character,
        (quote_start + 1 + "/users/profile".len()) as u32,
        "range end hugs the closing quote — replacement stays INSIDE the existing quotes",
    );

    // Same check for the `Ctrl#act` flavor — cursor past the `#`.
    let ctrl_src_hash = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('Users#list');
}
"#;
    let ctrl_fa_hash = crate::builder::build(
        &parser.parse(ctrl_src_hash, None).unwrap(),
        ctrl_src_hash.as_bytes(),
    );
    let tree_hash = parser.parse(ctrl_src_hash, None).unwrap();
    let line = ctrl_src_hash.lines().nth(5).unwrap();
    let quote_start = line.find("'Users#list").unwrap();
    let past_hash_col = (quote_start + 1 + "Users#li".len()) as u32;
    let pos = Position {
        line: 5,
        character: past_hash_col,
    };
    let items = completion_items(&ctrl_fa_hash, &tree_hash, ctrl_src_hash, pos, &idx, None);
    let hash_item = items
        .iter()
        .find(|i| i.label == "Users#list")
        .expect("Users#list must be offered when cursor is past the #");
    assert_eq!(hash_item.insert_text, None);
    let Some(CompletionTextEdit::Edit(ref te)) = hash_item.text_edit else {
        panic!(
            "expected textEdit for `Users#list`; got {:?}",
            hash_item.text_edit
        );
    };
    assert_eq!(te.new_text, "Users#list");
}

/// Red pin (user QA): accepting a dispatch completion APPENDED
/// the label instead of replacing the typed text — `url_for('/fall|')`
/// accepting `/fallback` yielded `url_for('/fall/fallback')`.
/// Root cause: `descendant_for_point_range` returns the enclosing
/// `string_literal` (not `string_content`) when the cursor sits
/// at the content's end boundary, because content ranges are
/// half-open. `string_content_span_at` then fell into the
/// zero-width "empty literal" branch and returned `(cursor,
/// cursor)` — textEdit replacing nothing = append. Fix descends
/// into the literal to find a `string_content` child before
/// giving up.
///
/// This pin covers three cursor positions, each of which previously
/// hit the wrapper-instead-of-content path:
///   1. INSIDE the content (baseline — already worked).
///   2. AT the content's end boundary (just before closing quote).
///   3. ON the closing quote itself.
/// All three must return a textEdit range covering the full
/// `string_content` span, so accepting the completion replaces
/// the typed prefix with the label cleanly.
#[test]
fn completion_dispatch_textedit_range_at_content_boundary() {
    use crate::module_index::ModuleIndex;
    use tower_lsp::lsp_types::CompletionTextEdit;

    let app_src = r#"package MyApp;
use Mojolicious::Lite;

any '/fallback' => sub { my ($c) = @_; };
"#;
    let app_fa = std::sync::Arc::new(crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(app_src, None).unwrap()
        },
        app_src.as_bytes(),
    ));

    let idx = std::sync::Arc::new(ModuleIndex::new_for_test());
    idx.register_workspace_module(std::path::PathBuf::from("/tmp/app.pl"), app_fa);

    let ctrl_src = r#"package Users;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    $c->url_for('/fall');
}
"#;
    let ctrl_fa = crate::builder::build(
        &{
            let mut p = tree_sitter::Parser::new();
            p.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
            p.parse(ctrl_src, None).unwrap()
        },
        ctrl_src.as_bytes(),
    );
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(ctrl_src, None).unwrap();

    // `    $c->url_for('/fall');`
    let line_idx = 5u32;
    let line = ctrl_src.lines().nth(line_idx as usize).unwrap();
    let quote_start = line.find("'/fall'").unwrap();
    let content_start = (quote_start + 1) as u32; // `/`
    let content_end = content_start + "/fall".len() as u32; // just after `l`
    let closing_quote_col = content_end; // the `'`

    // Three cursor positions to exercise: inside the content,
    // at the content's end boundary, and on the closing quote.
    let cursor_variants = [
        ("inside content", content_start + 3), // between `a` and `l`
        ("end of content", content_end),       // just after `l`, before `'`
        ("on closing quote", closing_quote_col),
    ];

    for (label, col) in cursor_variants {
        let items = completion_items(
            &ctrl_fa,
            &tree,
            ctrl_src,
            Position {
                line: line_idx,
                character: col,
            },
            &idx,
            None,
        );
        let item = items
            .iter()
            .find(|i| i.label == "/fallback")
            .unwrap_or_else(|| {
                panic!(
                    "{}: /fallback must be offered at col {}; \
                                           got labels: {:?}",
                    label,
                    col,
                    items.iter().map(|i| &i.label).collect::<Vec<_>>()
                )
            });

        let Some(CompletionTextEdit::Edit(ref te)) = item.text_edit else {
            panic!("{}: expected textEdit; got {:?}", label, item.text_edit);
        };
        // Range must cover the FULL typed content, not zero-width.
        // That way accepting `/fallback` REPLACES `/fall`, not
        // appends to it — the pre-fix failure mode that produced
        // `'/fall/fallback'`.
        assert_eq!(
            te.range.start.character, content_start,
            "{}: range start must hug the first content char; got range {:?}",
            label, te.range,
        );
        assert_eq!(
            te.range.end.character, content_end,
            "{}: range end must hug the closing quote (exclusive of it); got range {:?}",
            label, te.range,
        );
        assert_eq!(
            te.new_text, "/fallback",
            "{}: newText is the bare label — no seasonal redundancy",
            label,
        );
    }
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->emit('connect',"))
        .unwrap();
    let col = line.find(", )").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    assert!(
        !labels.contains(&"completely_unrelated"),
        "unrelated sub must not appear in dispatch arg completion: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"wire_one"),
        "wire_one leak — dispatch arg completion should stay quiet: {:?}",
        labels
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    // Cursor just after 'lis' in 'Users#lis' — active editing state.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("Users#lis"))
        .unwrap();
    let col = line.find("Users#lis").unwrap() + "Users#lis".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

    // Prefix-filtered: only `list` starts with `lis`; `login`,
    // `logout`, `delete_user` don't.
    assert!(
        labels.contains(&"list"),
        "list must be offered for prefix `lis`: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"login"),
        "login does NOT start with `lis` — must be filtered out: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"logout"),
        "logout does NOT start with `lis` — must be filtered out: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"delete_user"),
        "delete_user is unrelated — must not appear: {:?}",
        labels
    );

    // Top-priority sort — the mid-string completion path is the
    // only sensible one at this cursor position.
    let list = items.iter().find(|i| i.label == "list").unwrap();
    assert!(
        list.sort_text
            .as_deref()
            .unwrap_or("zzz")
            .starts_with("000"),
        "mid-string method completion should be top-priority: {:?}",
        list.sort_text
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("'Us'"))
        .unwrap();
    let col = line.find("'Us'").unwrap() + "'Us".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("->unrelated_method()"))
        .unwrap();
    let col = line.find("method(").unwrap() + "method(".len();
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    assert!(
        !items.iter().any(|i| i.label == "connect"),
        "non-dispatcher method must not surface handler completions"
    );
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
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());
    let idx = ModuleIndex::new_for_test();

    let pos = {
        let line_idx = src
            .lines()
            .enumerate()
            .find(|(_, l)| l.contains("never_registered"))
            .map(|(i, _)| i)
            .unwrap();
        let line = src.lines().nth(line_idx).unwrap();
        let col = line.find(", )").unwrap() + 2;
        Position {
            line: line_idx as u32,
            character: col as u32,
        }
    };

    let sig = signature_help(&analysis, &tree, src, pos, &idx);
    assert!(
        sig.is_none(),
        "no handler_params → no string-dispatch sig; also no local ->emit def"
    );
}

// ---- data-printer plugin: full intelligence ----

/// Build a CachedModule under the real package name we want, with
/// arbitrary source. `fake_cached` always synthesizes a `package
/// Fake;` source — useless when the caller needs the cached
/// module to expose subs under a specific name like `Data::Printer`.
fn cached_under(name: &str, source: &str) -> std::sync::Arc<crate::module_index::CachedModule> {
    let analysis = parse_analysis(source);
    std::sync::Arc::new(crate::module_index::CachedModule::new(
        std::path::PathBuf::from(format!("/fake/{}.pm", name.replace("::", "/"))),
        std::sync::Arc::new(analysis),
    ))
}

#[test]
fn data_printer_use_ddp_resolves_p_to_data_printer() {
    // The end-to-end intelligence pin. `use DDP` is a literal alias
    // for `use Data::Printer` (DDP.pm just `push our @ISA, 'Data::Printer'`).
    // Hover/K, gd, and sig-help on `p` must reach Data::Printer's
    // real `sub p` — not DDP. The plugin's synthetic Import
    // (module_name: "Data::Printer", imported_symbols: [p, np]) is
    // what carries this; resolve_imported_function is the seam every
    // intelligence feature routes through.
    let source = "use DDP;\np $foo;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    // Stub Data::Printer.pm. The real module has a custom `import`
    // (no @EXPORT), but `sub p` is a normal sub the builder picks
    // up — exactly what users get from cpan.
    module_index.insert_cache(
            "Data::Printer",
            Some(cached_under(
                "Data::Printer",
                "package Data::Printer;\nsub p { my (undef, %props) = @_; }\nsub np { my (undef, %props) = @_; }\n1;\n",
            )),
        );

    let resolved = resolve_imported_function(&analysis, "p", &module_index);
    assert!(
        resolved.is_some(),
        "use DDP must alias to Data::Printer; resolve_imported_function for `p` returned None — \
             imports were: {:?}",
        analysis
            .imports
            .iter()
            .map(|i| (
                i.module_name.clone(),
                i.imported_symbols
                    .iter()
                    .map(|s| s.local_name.clone())
                    .collect::<Vec<_>>(),
            ))
            .collect::<Vec<_>>()
    );
    let (import, _path, remote) = resolved.unwrap();
    assert_eq!(
        import.module_name, "Data::Printer",
        "alias must route to Data::Printer, not DDP"
    );
    assert_eq!(remote, "p", "local `p` maps to remote `p`");

    // np too — both DDP-installed names.
    let np = resolve_imported_function(&analysis, "np", &module_index);
    assert!(
        np.is_some(),
        "use DDP must also resolve `np` to Data::Printer"
    );
    assert_eq!(np.unwrap().0.module_name, "Data::Printer");
}

#[test]
fn data_printer_use_data_printer_resolves_p_to_data_printer() {
    // Same test for the non-alias case. `use Data::Printer;` with no
    // qw list — the plugin's synthetic Import claims `p`/`np` so
    // resolve_imported_function pairs them with the real sub.
    let source = "use Data::Printer;\np $foo;\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();
    module_index.insert_cache(
            "Data::Printer",
            Some(cached_under(
                "Data::Printer",
                "package Data::Printer;\nsub p { my (undef, %props) = @_; }\nsub np { my (undef, %props) = @_; }\n1;\n",
            )),
        );

    let resolved = resolve_imported_function(&analysis, "p", &module_index);
    assert!(
        resolved.is_some(),
        "use Data::Printer (no qw list) must still let resolve_imported_function find p"
    );
    assert_eq!(resolved.unwrap().0.module_name, "Data::Printer");
}

#[test]
fn data_printer_use_line_options_completion() {
    // `use DDP { | }` — cursor inside the options hashref. The
    // plugin's on_completion hook recognizes "current_use_module
    // matches DDP/Data::Printer and cursor_inside is a Hash" and
    // returns the documented option keys (caller_info, colored,
    // class_method, output, ...). No core hard-codes the option
    // list — it lives in the plugin.
    let source = "use DDP { };\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    // Cursor between the braces. Source: "use DDP { };" → col 10
    // is one past the opening brace (which lives at col 9).
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();

    let pos = Position {
        line: 0,
        character: 10,
    };
    let items = completion_items(&analysis, &tree, source, pos, &module_index, None);

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    // Sample of keys from Data::Printer's actual options. If the
    // plugin doesn't ship these specific names, swap to whichever
    // ones the plugin advertises — the contract is "DDP options
    // surface here", not "this exact list".
    for key in &["caller_info", "colored", "class_method", "output"] {
        assert!(
            labels.iter().any(|l| l == key),
            "use DDP {{ }} option completion must offer `{}`; got: {:?}",
            key,
            labels,
        );
    }
}

// ---- witness-bag chain typing: pin-the-fix on the real demo ----

/// Pin against the actual demo file. Loads
/// `test_files/plugin_mojo_demo.pl` + stubs of the Mojolicious
/// hierarchy registered into the module index, then asserts:
/// (a) `$r` at line 71 resolves to a known class.
/// (b) `->to` on line 71 is a MethodCall ref.
/// (c) `->to`'s invocant resolves to a class via
///     `resolve_method_invocant_public` (the path nvim hover/gd
///     uses internally).
///
/// Two possible failure modes the test distinguishes:
///   - `$r` is typed but `->to`'s invocant fails → crossfile
///     chain hop is the gap (find_method_return_type's CrossFile
///     branch).
///   - `$r` isn't typed at all → earlier hop broken first.
#[test]
fn test_demo_file_chain_to_resolves_on_line_71() {
    use std::fs;
    use std::path::PathBuf;

    // Project root: the worktree (= CARGO_MANIFEST_DIR).
    let root: PathBuf = env!("CARGO_MANIFEST_DIR").into();
    let demo = root.join("test_files/plugin_mojo_demo.pl");
    let demo_source = fs::read_to_string(&demo).expect("demo file present");

    // Index the project's test_files/ as the workspace.
    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(Some(root.to_str().unwrap()));
    let files = crate::file_store::FileStore::new();
    let _indexed = crate::module_resolver::index_workspace_with_index(
        &root.join("test_files"),
        &files,
        Some(&idx),
    );

    // Use the ACTUAL Mojolicious library from @INC — the same
    // code nvim analyzes. If Mojo isn't installed, skip cleanly
    // so CI on bare systems doesn't break.
    let inc_paths = crate::module_resolver::discover_inc_paths();
    let insert_real = |name: &str| -> bool {
        let mut p = crate::module_resolver::create_parser();
        match crate::module_resolver::resolve_and_parse(&inc_paths, name, &mut p) {
            Some(cached) => {
                idx.insert_cache(name, Some(cached));
                true
            }
            None => false,
        }
    };
    let have_mojo = insert_real("Mojolicious")
        && insert_real("Mojolicious::Routes")
        && insert_real("Mojolicious::Routes::Route")
        && insert_real("Mojolicious::Lite");
    if !have_mojo {
        eprintln!("SKIP: Mojolicious not installed in @INC");
        return;
    }
    let _ = PathBuf::new(); // keep the import used in both branches

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&demo_source, None).unwrap();
    let mut analysis = crate::builder::build(&tree, demo_source.as_bytes());

    // Cross-file enrichment — same step the backend runs on open.
    // Resolves MethodCallBindings against the module_index so
    // `my $r = $app->routes;` becomes a TypeConstraint. Without
    // this, the backend's `enrich_analysis(uri)` hasn't fired
    // yet and the whole chain is un-typed.
    let (imported_returns, imported_keys) =
        crate::backend::build_imported_return_types_for_test(&analysis, &idx);
    analysis.enrich_imported_types_with_keys(imported_returns, imported_keys, Some(&idx));

    // Find line 71 ($r->get('/users')->to('Users#list');).
    let (line_idx, chain_line) = demo_source
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')") && l.contains("->to('Users#list')"))
        .expect("chain line present in demo");

    // Position on `to` — the 't' character.
    let to_col = chain_line.find("->to(").unwrap() + 2;
    let r_col = chain_line.find("$r").unwrap();
    let get_col = chain_line.find("->get(").unwrap() + 2;

    let pt = |col: usize| tree_sitter::Point {
        row: line_idx,
        column: col,
    };

    // Diagnostics — what does the analysis actually see?
    let r_ty_bag = analysis.inferred_type_via_bag("$r", pt(r_col));
    let r_ty_legacy = analysis.inferred_type("$r", pt(r_col)).cloned();
    let mcb_for_r: Vec<_> = analysis
        .method_call_bindings
        .iter()
        .filter(|b| b.variable == "$r")
        .collect();
    let cb_for_r: Vec<_> = analysis
        .call_bindings
        .iter()
        .filter(|b| b.variable == "$r")
        .collect();
    // Is `app` even known as a symbol/import?
    let app_known = analysis.symbols.iter().any(|s| s.name == "app");
    // Is Mojolicious in the module index?
    let mojo_cached = idx.get_cached("Mojolicious").is_some();
    let routes_cached = idx.get_cached("Mojolicious::Routes").is_some();
    let route_cached = idx.get_cached("Mojolicious::Routes::Route").is_some();
    eprintln!(
        "DIAG: $r bag={:?}  legacy={:?}  mcbs={:?}  cbs={:?}  app_sym={}  \
             mojo_cached={}  routes_cached={}  route_cached={}",
        r_ty_bag,
        r_ty_legacy,
        mcb_for_r
            .iter()
            .map(|b| format!("{}.{}", b.invocant_var, b.method_name))
            .collect::<Vec<_>>(),
        cb_for_r.iter().map(|b| &b.func_name).collect::<Vec<_>>(),
        app_known,
        mojo_cached,
        routes_cached,
        route_cached,
    );

    // (a) `$r` is typed. This uses the EXACT path cursor_context
    // uses to type an invocant — inferred_type_via_bag.
    let r_ty = r_ty_bag;
    let r_class = r_ty.as_ref().and_then(|t| t.class_name());
    assert!(
        r_class.is_some(),
        "$r should be typed (any class) at {}:{}; got {:?}",
        line_idx + 1,
        r_col,
        r_ty,
    );

    // (b) At `->get`'s 'g', there's a MethodCall ref. Its
    // invocant is `$r`. Resolve it cross-file.
    let get_ref = analysis.ref_at(pt(get_col)).expect("ref at ->get");
    assert_eq!(get_ref.target_name, "get");
    if let crate::file_analysis::RefKind::MethodCall {
        invocant,
        invocant_span,
        ..
    } = &get_ref.kind
    {
        let klass = analysis.resolve_method_invocant_public(
            invocant,
            invocant_span,
            get_ref.scope,
            pt(get_col),
            Some(&tree),
            Some(demo_source.as_bytes()),
            Some(&idx),
        );
        assert!(
            klass.is_some(),
            "`->get`'s invocant (= $r) should resolve to SOME class; got {:?}",
            klass,
        );
    }

    // (c) The `->to` hop. Real Mojolicious::Routes::Route::get
    // is `shift->_generate_route(GET => @_)` — our implicit-
    // return witnessing records that get's return chains
    // through _generate_route. _generate_route's own return is
    // `return defined $name ? $route->name($name) : $route;` —
    // a complex conditional whose arms depend on $route's
    // chain-built type. That depth of cross-file chain
    // resolution is a separate follow-up; for now we assert
    // the MethodCall ref exists and carries the right target,
    // but leave the class-resolution assertion as a diagnostic
    // rather than hard-fail.
    let to_ref = analysis.ref_at(pt(to_col)).expect("ref at ->to");
    assert_eq!(to_ref.target_name, "to");
    assert!(
        matches!(
            to_ref.kind,
            crate::file_analysis::RefKind::MethodCall { .. }
        ),
        "ref at ->to is a MethodCall"
    );
    if let crate::file_analysis::RefKind::MethodCall {
        invocant,
        invocant_span,
        ..
    } = &to_ref.kind
    {
        let klass = analysis.resolve_method_invocant_public(
            invocant,
            invocant_span,
            to_ref.scope,
            pt(to_col),
            Some(&tree),
            Some(demo_source.as_bytes()),
            Some(&idx),
        );
        eprintln!(
            "DIAG: ->to invocant class (real Mojo): {:?} \
                 (None expected until deep chain through \
                 _generate_route/requires/to is resolved)",
            klass,
        );
    }
}

#[test]
fn data_printer_use_line_options_completion_for_data_printer_module() {
    // Same flow, non-alias name. The plugin can't be DDP-specific.
    let source = "use Data::Printer { };\n";
    let analysis = parse_analysis(source);
    let module_index = crate::module_index::ModuleIndex::new_for_test();

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();

    // "use Data::Printer { };" — col 20 sits between the braces.
    let pos = Position {
        line: 0,
        character: 20,
    };
    let items = completion_items(&analysis, &tree, source, pos, &module_index, None);

    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.iter().any(|l| *l == "caller_info"),
        "use Data::Printer {{ }} must surface options too; got: {:?}",
        labels,
    );
}
// ---- witness-driven chain completion (spike) ----

/// Decomposition: parse real Mojolicious/Routes/Route.pm
/// in-place (not via module index) and probe each hop of the
/// `$self->_route()->requires()->to()` chain separately. Plus
/// probe `$self->_generate_route(...)`. Reports what each
/// specific hop resolves to — so we know exactly which step
/// in the chain is actually dying.
#[test]
fn test_route_pm_chain_decomposition() {
    use std::fs;
    use std::path::PathBuf;
    let inc = crate::module_resolver::discover_inc_paths();
    let route_path = inc
        .iter()
        .map(|p| p.join("Mojolicious/Routes/Route.pm"))
        .find(|p| p.exists());
    let route_path: PathBuf = match route_path {
        Some(p) => p,
        None => {
            eprintln!("SKIP: Mojo not installed");
            return;
        }
    };
    let src = fs::read_to_string(&route_path).unwrap();

    // Parse Route.pm itself — `$self` inside = Route.
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&src, None).unwrap();
    let analysis = crate::builder::build(&tree, src.as_bytes());

    // Probe: find the `_generate_route` sub body and report
    // what we see on each hop.
    let inspect_sym = |name: &str| {
        for sym in &analysis.symbols {
            if sym.name != name {
                continue;
            }
            if !matches!(
                sym.kind,
                crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
            ) {
                continue;
            }
            if let crate::file_analysis::SymbolDetail::Sub {
                return_type,
                return_self_method,
                ..
            } = &sym.detail
            {
                eprintln!(
                    "  sym[{:24}] return_type={:?}  return_self_method={:?}",
                    name, return_type, return_self_method
                );
                return;
            }
        }
        eprintln!("  sym[{:24}] NOT FOUND", name);
    };
    eprintln!("======== symbol return types in Route.pm ========");
    for name in [
        "get",
        "post",
        "any",
        "to",
        "name",
        "requires",
        "_generate_route",
        "_route",
        "add_child",
        "pattern",
        "is_reserved",
        "root",
    ] {
        inspect_sym(name);
    }

    // Find `_generate_route`'s body block. Its last statement
    // is `return defined $name ? $route->name($name) : $route;`.
    // Probe each subexpression type via resolve_expression_type.
    fn find_sub_body<'t>(
        n: tree_sitter::Node<'t>,
        src: &[u8],
        name: &str,
    ) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "subroutine_declaration_statement" {
            if let Some(nm) = n.child_by_field_name("name") {
                if nm.utf8_text(src).ok() == Some(name) {
                    return n.child_by_field_name("body");
                }
            }
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_sub_body(c, src, name) {
                    return Some(r);
                }
            }
        }
        None
    }
    let body = find_sub_body(tree.root_node(), src.as_bytes(), "_generate_route")
        .expect("_generate_route body");

    // Inside _generate_route, find:
    //   (a) the `my $route = CHAIN` assignment
    //   (b) the final `return TERNARY` expression
    fn find_var_decl_for<'t>(
        n: tree_sitter::Node<'t>,
        src: &[u8],
        var: &str,
    ) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "assignment_expression" {
            if let Some(left) = n.child_by_field_name("left") {
                if left.utf8_text(src).map(|s| s.trim()).ok() == Some(&format!("my {}", var)) {
                    return n.child_by_field_name("right");
                }
            }
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_var_decl_for(c, src, var) {
                    return Some(r);
                }
            }
        }
        None
    }
    let route_rhs = find_var_decl_for(body, src.as_bytes(), "$route").expect("my $route = ... RHS");

    eprintln!();
    eprintln!("======== `my $route = RHS` decomposition ========");
    eprintln!(
        "RHS shape: {}  kind={}",
        route_rhs.utf8_text(src.as_bytes()).unwrap_or(""),
        route_rhs.kind()
    );

    // Probe chain hops from innermost outward. Each node in a
    // chain a->b->c has: outer is c's method_call_expression,
    // its invocant is the a->b method_call_expression, whose
    // invocant is `$self`.
    fn report_node_type(
        label: &str,
        n: tree_sitter::Node,
        analysis: &crate::file_analysis::FileAnalysis,
        src: &[u8],
    ) {
        let text = n.utf8_text(src).unwrap_or("").trim();
        let ty = analysis.resolve_expression_type(n, src, None);
        eprintln!(
            "  [{label:>12}] `{text:.60}`\n                kind={} → ty={:?}",
            n.kind(),
            ty
        );
    }

    // Walk the chain inside-out and report each level's type.
    let mut cur = Some(route_rhs);
    let mut depth = 0;
    while let Some(n) = cur {
        let label = match depth {
            0 => "outer",
            1 => "mid1",
            2 => "mid2",
            3 => "mid3",
            _ => "inner",
        };
        report_node_type(label, n, &analysis, src.as_bytes());
        if n.kind() == "method_call_expression" {
            cur = n.child_by_field_name("invocant");
            depth += 1;
        } else {
            break;
        }
    }

    eprintln!();
    eprintln!("======== return TERNARY probe ========");
    // Find the return_expression in body.
    fn find_return<'t>(n: tree_sitter::Node<'t>) -> Option<tree_sitter::Node<'t>> {
        if n.kind() == "return_expression" {
            return Some(n);
        }
        for i in 0..n.named_child_count() {
            if let Some(c) = n.named_child(i) {
                if let Some(r) = find_return(c) {
                    return Some(r);
                }
            }
        }
        None
    }
    let ret = find_return(body).expect("return in _generate_route");
    let ternary = ret.named_child(0).expect("return child");
    eprintln!(
        "  return child kind = {}  text = `{}`",
        ternary.kind(),
        ternary.utf8_text(src.as_bytes()).unwrap_or("").trim()
    );
    if ternary.kind() == "conditional_expression" {
        let consequent = ternary.child_by_field_name("consequent");
        let alternative = ternary.child_by_field_name("alternative");
        if let Some(a) = consequent {
            report_node_type("then-arm", a, &analysis, src.as_bytes());
        }
        if let Some(b) = alternative {
            report_node_type("else-arm", b, &analysis, src.as_bytes());
        }
    }
}

/// Direct proof: enumerate each chain link's resolvability on
/// the real demo + real Mojolicious. Prints a truth table;
/// asserts specifically that `->to` does NOT resolve (the gap
/// the user flagged) so this test becomes a tripwire: if a
/// future fix makes `->to` resolve, this test will fail and
/// force us to promote it to a "works" assertion.
#[test]
fn test_demo_chain_empirical_truth_table() {
    use std::fs;
    use std::path::PathBuf;

    let root: PathBuf = env!("CARGO_MANIFEST_DIR").into();
    let demo = root.join("test_files/plugin_mojo_demo.pl");
    let demo_source = fs::read_to_string(&demo).expect("demo file");

    let idx = ModuleIndex::new_for_test();
    idx.set_workspace_root(Some(root.to_str().unwrap()));
    let files = crate::file_store::FileStore::new();
    let _ = crate::module_resolver::index_workspace_with_index(
        &root.join("test_files"),
        &files,
        Some(&idx),
    );

    let inc = crate::module_resolver::discover_inc_paths();
    let install = |name: &str| -> bool {
        let mut p = crate::module_resolver::create_parser();
        match crate::module_resolver::resolve_and_parse(&inc, name, &mut p) {
            Some(c) => {
                idx.insert_cache(name, Some(c));
                true
            }
            None => false,
        }
    };
    if !(install("Mojolicious")
        && install("Mojolicious::Routes")
        && install("Mojolicious::Routes::Route")
        && install("Mojolicious::Lite"))
    {
        eprintln!("SKIP: Mojolicious not installed");
        return;
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&demo_source, None).unwrap();
    let mut analysis = crate::builder::build(&tree, demo_source.as_bytes());
    let (ir, ik) = crate::backend::build_imported_return_types_for_test(&analysis, &idx);
    analysis.enrich_imported_types_with_keys(ir, ik, Some(&idx));

    let (line_idx, chain_line) = demo_source
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')") && l.contains("->to('Users#list')"))
        .expect("demo chain line present");

    let r_col = chain_line.find("$r").unwrap();
    let get_col = chain_line.find("->get(").unwrap() + 2;
    let to_col = chain_line.find("->to(").unwrap() + 2;
    let pt = |c: usize| tree_sitter::Point {
        row: line_idx,
        column: c,
    };

    // --- Link 1: $r's type ---
    let r_ty = analysis.inferred_type_via_bag("$r", pt(r_col));
    let r_class = r_ty
        .as_ref()
        .and_then(|t| t.class_name())
        .map(|s| s.to_string());

    // --- Link 2: ->get's invocant class (= $r's class) ---
    let get_ref = analysis.ref_at(pt(get_col)).expect("ref at ->get");
    let get_invocant_class = if let crate::file_analysis::RefKind::MethodCall {
        invocant,
        invocant_span,
        ..
    } = &get_ref.kind
    {
        analysis.resolve_method_invocant_public(
            invocant,
            invocant_span,
            get_ref.scope,
            pt(get_col),
            Some(&tree),
            Some(demo_source.as_bytes()),
            Some(&idx),
        )
    } else {
        None
    };

    // --- Link 3: ->get's RETURN type (what `$r->get(...)` evaluates to) ---
    // Find the method_call_expression node for `$r->get('/users')`.
    let mcall_node = {
        fn find_getcall<'a>(n: tree_sitter::Node<'a>, src: &[u8]) -> Option<tree_sitter::Node<'a>> {
            if n.kind() == "method_call_expression" {
                if let Some(m) = n.child_by_field_name("method") {
                    if m.utf8_text(src).ok() == Some("get") {
                        return Some(n);
                    }
                }
            }
            for i in 0..n.named_child_count() {
                if let Some(c) = n.named_child(i) {
                    if let Some(r) = find_getcall(c, src) {
                        return Some(r);
                    }
                }
            }
            None
        }
        find_getcall(tree.root_node(), demo_source.as_bytes()).expect("->get node")
    };
    let get_return_ty =
        analysis.resolve_expression_type(mcall_node, demo_source.as_bytes(), Some(&idx));

    // --- Link 4: ->to's invocant class (= ->get's return class) ---
    let to_ref = analysis.ref_at(pt(to_col)).expect("ref at ->to");
    let to_invocant_class = if let crate::file_analysis::RefKind::MethodCall {
        invocant,
        invocant_span,
        ..
    } = &to_ref.kind
    {
        analysis.resolve_method_invocant_public(
            invocant,
            invocant_span,
            to_ref.scope,
            pt(to_col),
            Some(&tree),
            Some(demo_source.as_bytes()),
            Some(&idx),
        )
    } else {
        None
    };

    // Also directly inspect the cached Route module's stored
    // return types + self-method tails for each method on the
    // chain's path.
    let route_cached = idx.get_cached("Mojolicious::Routes::Route").unwrap();
    let inspect = |name: &str| -> (Option<InferredType>, Option<String>) {
        let mut rt = None;
        let mut sm = None;
        for sym in &route_cached.analysis.symbols {
            if sym.name != name {
                continue;
            }
            if !matches!(
                sym.kind,
                crate::file_analysis::SymKind::Sub | crate::file_analysis::SymKind::Method
            ) {
                continue;
            }
            if let crate::file_analysis::SymbolDetail::Sub {
                return_type,
                return_self_method,
                ..
            } = &sym.detail
            {
                rt = return_type.clone();
                sm = return_self_method.clone();
                break;
            }
        }
        (rt, sm)
    };
    let (gen_rt, gen_tail) = inspect("_generate_route");
    let (get_rt, get_tail) = inspect("get");
    let (to_rt, to_tail) = inspect("to");
    let (requires_rt, requires_tail) = inspect("requires");
    let (_route_rt, _route_tail) = inspect("_route");

    eprintln!("======== chain truth table ========");
    eprintln!("  $r              class = {:?}", r_class);
    eprintln!("  ->get invocant  class = {:?}", get_invocant_class);
    eprintln!("  ->get RETURN    type  = {:?}", get_return_ty);
    eprintln!("  ->to  invocant  class = {:?}", to_invocant_class);
    eprintln!("  ---- cached Route symbols ----");
    eprintln!("  get             rt={:?}  tail={:?}", get_rt, get_tail);
    eprintln!("  _generate_route rt={:?}  tail={:?}", gen_rt, gen_tail);
    eprintln!(
        "  requires        rt={:?}  tail={:?}",
        requires_rt, requires_tail
    );
    eprintln!("  to              rt={:?}  tail={:?}", to_rt, to_tail);
    eprintln!(
        "  _route          rt={:?}  tail={:?}",
        _route_rt, _route_tail
    );
    eprintln!("====================================");

    // The chain pin. With:
    //   - mojo-routes plugin's `_route` override pinning the
    //     return type inference can't reach, AND
    //   - the unified post-walk `type_assignments_into_bag` pass
    //     symbolically executing every `my $X = <expr>` rhs (no
    //     "is it a chain" branch — same recursion every consumer
    //     uses), AND
    //   - a refresh of return-arm types before the second
    //     fold so `_generate_route`'s ternary return picks up
    //     the now-typed `$route`,
    // the full `$r->get(...)->to(...)` chain resolves end-to-end.
    // Each link is pinned individually so a regression localizes
    // to a specific hop instead of "the chain broke".
    assert!(
        r_class.is_some(),
        "(link 1) $r must resolve to a class; got None"
    );
    assert_eq!(r_class.as_deref(), Some("Mojolicious::Routes"));
    assert!(
        get_invocant_class.is_some(),
        "(link 2) ->get's invocant class must resolve; got None"
    );
    assert!(
        get_return_ty.is_some(),
        "(link 3) ->get's RETURN type must resolve through \
             _generate_route → _route's plugin override"
    );
    assert_eq!(
        get_return_ty.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "->get returns the Route class so ->to can chain off it"
    );
    assert!(
        to_invocant_class.is_some(),
        "(link 4) ->to's invocant class must resolve — THIS is \
             the chain hop the spike was unblocking"
    );
    assert_eq!(
        to_invocant_class.as_deref(),
        Some("Mojolicious::Routes::Route"),
        "->to is invoked on a Route, so cursor-on-`to` \
                    completion / hover / goto-def all reach \
                    Mojolicious::Routes::Route::to"
    );

    // Cross-check the cached symbols: every verb method
    // (get/post/put/etc.) tail-delegates through _generate_route,
    // and _generate_route's body folds via the chain typer +
    // refreshed return-arm typing.
    assert_eq!(
        _route_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "_route is the override anchor",
    );
    assert_eq!(
        gen_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "_generate_route folds because $route is now typed",
    );
    assert_eq!(
        get_rt.as_ref().and_then(|t| t.class_name()),
        Some("Mojolicious::Routes::Route"),
        "get tail-delegates to _generate_route which has a type",
    );
}

/// E2E: the motivator. `$r->get('/x')->|` at the cursor — the
/// public `completion_items` API must offer methods from the
/// route class (Route::to, Route::name, etc.), proving the
/// witness-bag-driven chain typing works all the way through
/// CursorContext → resolve_node_type → resolve_expression_type →
/// find_method_return_type → complete_methods_for_class.
///
/// No special casing. Zero hardcoded chain rules. If this
/// passes, the mojo-demo `$r->get('/x')->to(...)` gets
/// "intellismarts" on `->to` through witness flow.
#[test]
fn test_e2e_mojo_style_chain_completion_offers_chained_class_methods() {
    let src = r#"package MyApp::Route;
sub new { my $c = shift; bless {}, $c }
sub get {
    my $self = shift;
    $self->{_path} = shift;
    return $self;
}
sub to {
    my $self = shift;
    $self->{_target} = shift;
    return $self;
}
sub name {
    my $self = shift;
    $self->{_name} = shift;
    return $self;
}

package main;
my $r = MyApp::Route->new;
$r->get('/users')->
"#;
    let analysis = parse_analysis(src);
    let tree = crate::document::Document::new(src.to_string())
        .unwrap()
        .tree;
    let idx = ModuleIndex::new_for_test();

    // Cursor right after the trailing `->` on the chain line.
    let (line_idx, line) = src
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("$r->get('/users')->"))
        .unwrap();
    let col = line.rfind("->").unwrap() + 2;
    let pos = Position {
        line: line_idx as u32,
        character: col as u32,
    };

    let items = completion_items(&analysis, &tree, src, pos, &idx, None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in &["to", "name", "get"] {
        assert!(
            labels.contains(expected),
            "expected `{}` in completion after `$r->get('/users')->`, \
                 got {} items: {:?}",
            expected,
            labels.len(),
            labels
        );
    }
}
