//! Author-facing CLI subcommands for plugin development.
//!
//! Three commands, all built around the same idea: give plugin authors a
//! way to validate, exercise, and snapshot-test a `.rhai` plugin without
//! restarting the LSP or writing Rust integration tests.
//!
//!   * `--plugin-check <file.rhai>` — load the script, surface compile
//!     errors, lint a few well-known footguns (reserved-keyword
//!     dot-access on `ctx.call`, missing `id()`/`triggers()`).
//!   * `--plugin-run <file.rhai> --on <fixture.pl>` — apply ONLY this
//!     plugin to a Perl fixture, dump the plugin's emissions as JSON.
//!     Diff against a baseline build (no plugins) so the output reflects
//!     exactly what the plugin contributes.
//!   * `--plugin-test <plugin-dir>` — walk `tests/*.pl` in the plugin
//!     directory and diff the emission JSON against `tests/*.expected.json`.
//!     `--update` captures snapshots on first run.
//!
//! The diff approach is the authoritative way to attribute emissions:
//! `Symbol.namespace` carries the plugin id but `Ref` and most other
//! tables don't, so structural diff (P − B) is more reliable than
//! tag-based filtering.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use serde_json::{json, Value};

use crate::builder::build_with_plugins;
use crate::file_analysis::{FileAnalysis, Namespace, Symbol};
use crate::module_resolver;
use crate::plugin::rhai_host::{make_engine, RhaiPlugin};
use crate::plugin::{FrameworkPlugin, PluginRegistry};

// ---- Public entry points ----

/// `--plugin-check <file.rhai> [--format json|human]`
pub fn cli_plugin_check(args: &[String]) {
    let path = match args.first() {
        Some(p) if !p.starts_with("--") => p,
        _ => {
            eprintln!("usage: perl-lsp --plugin-check <file.rhai> [--format json|human]");
            std::process::exit(2);
        }
    };
    let json_mode = is_json_format(args);
    let report = check_plugin_file(Path::new(path));

    if json_mode {
        println!("{}", serde_json::to_string_pretty(&report.to_json()).unwrap());
    } else {
        report.print_human(path);
    }
    if !report.is_ok() {
        std::process::exit(1);
    }
}

/// `--plugin-run <file.rhai> --on <fixture.pl> [--format json|human]`
pub fn cli_plugin_run(args: &[String]) {
    let plugin_path = match args.first() {
        Some(p) if !p.starts_with("--") => p,
        _ => {
            eprintln!("usage: perl-lsp --plugin-run <file.rhai> --on <fixture.pl> [--format json|human]");
            std::process::exit(2);
        }
    };
    let fixture = match get_arg_value(args, "--on") {
        Some(f) => f,
        None => {
            eprintln!("--plugin-run requires --on <fixture.pl>");
            std::process::exit(2);
        }
    };
    let json_mode = is_json_format(args);

    let plugin = match load_single_plugin(Path::new(plugin_path)) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("plugin load failed: {}", e);
            std::process::exit(1);
        }
    };
    let plugin_id = plugin.id().to_string();

    let emissions = run_plugin_on_fixture(plugin, Path::new(fixture));
    let payload = json!({
        "plugin": plugin_id,
        "fixture": fixture,
        "emissions": emissions,
    });

    if json_mode {
        println!("{}", serde_json::to_string_pretty(&payload).unwrap());
    } else {
        print_emissions_human(&plugin_id, fixture, &emissions);
    }
}

/// `--plugin-test <plugin-dir> [--update] [--format json|human]`
///
/// Layout:
///   <plugin-dir>/<name>.rhai
///   <plugin-dir>/tests/<fixture>.pl
///   <plugin-dir>/tests/<fixture>.expected.json
///
/// If the directory contains exactly one `.rhai` file, that's the plugin
/// under test. If it has multiple, every `.pl` is run against every
/// `.rhai` (allowing a plugin suite directory to share fixtures, though
/// in practice one plugin per dir is the simple case).
pub fn cli_plugin_test(args: &[String]) {
    let dir = match args.first() {
        Some(p) if !p.starts_with("--") => p,
        _ => {
            eprintln!("usage: perl-lsp --plugin-test <plugin-dir> [--update] [--format json|human]");
            std::process::exit(2);
        }
    };
    let update = args.iter().any(|a| a == "--update");
    let json_mode = is_json_format(args);

    let dir = Path::new(dir);
    let plugins = collect_plugins_in_dir(dir);
    if plugins.is_empty() {
        eprintln!("no .rhai plugins found in {}", dir.display());
        std::process::exit(2);
    }
    let fixtures = collect_fixtures_in_dir(dir);
    if fixtures.is_empty() {
        eprintln!("no fixtures found at {}/tests/*.pl", dir.display());
        std::process::exit(2);
    }

    let mut results: Vec<TestResult> = Vec::new();
    for plugin_path in &plugins {
        for fixture in &fixtures {
            results.push(run_one_test(plugin_path, fixture, update));
        }
    }

    let pass = results.iter().filter(|r| matches!(r.outcome, Outcome::Pass)).count();
    let fail = results.iter().filter(|r| matches!(r.outcome, Outcome::Fail { .. })).count();
    let updated = results.iter().filter(|r| matches!(r.outcome, Outcome::Updated)).count();
    let captured = results.iter().filter(|r| matches!(r.outcome, Outcome::Captured)).count();
    let load_failed = results.iter().filter(|r| matches!(r.outcome, Outcome::LoadError(_))).count();

    if json_mode {
        let payload = json!({
            "summary": { "pass": pass, "fail": fail, "updated": updated, "captured": captured, "load_error": load_failed },
            "results": results.iter().map(|r| r.to_json()).collect::<Vec<_>>(),
        });
        println!("{}", serde_json::to_string_pretty(&payload).unwrap());
    } else {
        for r in &results {
            r.print_human();
        }
        eprintln!(
            "\n{} pass, {} fail, {} updated, {} captured, {} load-error",
            pass, fail, updated, captured, load_failed,
        );
    }

    if fail > 0 || load_failed > 0 {
        std::process::exit(1);
    }
}

// ---- Plugin loading ----

fn load_single_plugin(path: &Path) -> Result<RhaiPlugin, String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("read {}: {}", path.display(), e))?;
    let engine = Arc::new(make_engine());
    RhaiPlugin::from_source(&source, engine)
}

fn run_plugin_on_fixture(plugin: RhaiPlugin, fixture: &Path) -> Value {
    let source = std::fs::read_to_string(fixture).unwrap_or_else(|e| {
        eprintln!("Cannot read {}: {}", fixture.display(), e);
        std::process::exit(1);
    });
    let mut parser = module_resolver::create_parser();
    let tree = parser.parse(&source, None).unwrap_or_else(|| {
        eprintln!("Parse failed: {}", fixture.display());
        std::process::exit(1);
    });

    let bytes = source.as_bytes();

    let baseline = build_with_plugins(&tree, bytes, Arc::new(PluginRegistry::new()));

    let mut reg = PluginRegistry::new();
    let plugin_id = plugin.id().to_string();
    reg.register(Box::new(plugin));
    let with_plugin = build_with_plugins(&tree, bytes, Arc::new(reg));

    diff_emissions(&baseline, &with_plugin, &plugin_id)
}

// ---- Diff: extract what the plugin contributed ----

/// Build a JSON snapshot of everything `with_plugin` adds over `baseline`.
/// Symbols are filtered by `Namespace::Framework { id }` so the snapshot
/// is precise even when other plugins might be active later. Refs and
/// other untagged tables are diffed structurally.
fn diff_emissions(baseline: &FileAnalysis, with_plugin: &FileAnalysis, plugin_id: &str) -> Value {
    // Symbols — namespace-tagged, so filter by id.
    let symbols: Vec<Value> = with_plugin
        .symbols
        .iter()
        .filter(|s| matches!(&s.namespace, Namespace::Framework { id } if id == plugin_id))
        .map(symbol_to_snapshot)
        .collect();

    // Refs — diff by (kind, span, target_name).
    let baseline_ref_keys: std::collections::HashSet<String> =
        baseline.refs.iter().map(ref_key).collect();
    let refs: Vec<Value> = with_plugin
        .refs
        .iter()
        .filter(|r| !baseline_ref_keys.contains(&ref_key(r)))
        .map(ref_to_snapshot)
        .collect();

    // framework_imports — set of strings, simple diff.
    let mut framework_imports: Vec<&String> = with_plugin
        .framework_imports
        .difference(&baseline.framework_imports)
        .collect();
    framework_imports.sort();

    // package_parents — diff entries.
    let mut package_parents: Vec<Value> = Vec::new();
    for (pkg, parents) in &with_plugin.package_parents {
        let baseline_parents = baseline.package_parents.get(pkg);
        for p in parents {
            let already = baseline_parents.map(|v| v.contains(p)).unwrap_or(false);
            if !already {
                package_parents.push(json!({ "package": pkg, "parent": p }));
            }
        }
    }
    package_parents.sort_by(|a, b| {
        a["package"].as_str().cmp(&b["package"].as_str())
            .then_with(|| a["parent"].as_str().cmp(&b["parent"].as_str()))
    });

    // plugin_namespaces — owned by plugins, so anything in `with_plugin` is
    // attributable. Filter by plugin_id to be precise.
    let plugin_namespaces: Vec<Value> = with_plugin
        .plugin_namespaces
        .iter()
        .filter(|n| n.plugin_id == plugin_id)
        .map(|n| json!({
            "id": n.id,
            "kind": n.kind,
            "bridges": n.bridges.iter().map(|b| format!("{:?}", b)).collect::<Vec<_>>(),
            "entities": n.entities.iter().map(|e| e.0).collect::<Vec<_>>(),
        }))
        .collect();

    // imports — diff by (module_name, items).
    let baseline_imports: std::collections::HashSet<String> =
        baseline.imports.iter().map(import_key).collect();
    let imports: Vec<Value> = with_plugin
        .imports
        .iter()
        .filter(|i| !baseline_imports.contains(&import_key(i)))
        .map(|i| json!({
            "module": i.module_name,
            "items": i.imported_symbols.iter().map(|s| s.local_name.clone()).collect::<Vec<_>>(),
        }))
        .collect();

    // type_constraints — diff by (var_name, type, span).
    let baseline_tc: std::collections::HashSet<String> =
        baseline.type_constraints.iter().map(tc_key).collect();
    let type_constraints: Vec<Value> = with_plugin
        .type_constraints
        .iter()
        .filter(|tc| !baseline_tc.contains(&tc_key(tc)))
        .map(|tc| json!({
            "var": tc.variable,
            "type": format!("{:?}", tc.inferred_type),
            "scope": tc.scope.0,
            "from_line": tc.constraint_span.start.row,
        }))
        .collect();

    json!({
        "symbols": symbols,
        "refs": refs,
        "framework_imports": framework_imports,
        "package_parents": package_parents,
        "plugin_namespaces": plugin_namespaces,
        "imports": imports,
        "type_constraints": type_constraints,
    })
}

fn symbol_to_snapshot(s: &Symbol) -> Value {
    let mut entry = json!({
        "name": s.name,
        "kind": format!("{:?}", s.kind),
        "package": s.package,
        "span": span_to_json(&s.span),
        "selection": span_to_json(&s.selection_span),
    });
    if let Some(label) = &s.outline_label {
        entry["outline_label"] = json!(label);
    }
    use crate::file_analysis::SymbolDetail;
    match &s.detail {
        SymbolDetail::Sub { params, is_method, return_type, display, hide_in_outline, opaque_return, .. } => {
            entry["detail"] = json!({
                "kind": "Sub",
                "params": params.iter().map(|p| json!({
                    "name": p.name,
                    "is_invocant": p.is_invocant,
                    "is_slurpy": p.is_slurpy,
                    "default": p.default,
                })).collect::<Vec<_>>(),
                "is_method": is_method,
                "return_type": return_type.as_ref().map(|t| format!("{:?}", t)),
                "display": display.as_ref().map(|d| format!("{:?}", d)),
                "hide_in_outline": hide_in_outline,
                "opaque_return": opaque_return,
            });
        }
        SymbolDetail::Handler { owner, dispatchers, params, display, hide_in_outline } => {
            entry["detail"] = json!({
                "kind": "Handler",
                "owner": format!("{:?}", owner),
                "dispatchers": dispatchers,
                "params": params.iter().map(|p| json!({
                    "name": p.name,
                    "is_invocant": p.is_invocant,
                    "is_slurpy": p.is_slurpy,
                })).collect::<Vec<_>>(),
                "display": format!("{:?}", display),
                "hide_in_outline": hide_in_outline,
            });
        }
        SymbolDetail::HashKeyDef { owner, is_dynamic } => {
            entry["detail"] = json!({
                "kind": "HashKeyDef",
                "owner": format!("{:?}", owner),
                "is_dynamic": is_dynamic,
            });
        }
        other => {
            entry["detail"] = json!({ "kind": format!("{:?}", std::mem::discriminant(other)) });
        }
    }
    entry
}

fn ref_to_snapshot(r: &crate::file_analysis::Ref) -> Value {
    json!({
        "kind": format!("{:?}", r.kind),
        "target": r.target_name,
        "span": span_to_json(&r.span),
        "access": format!("{:?}", r.access),
    })
}

fn span_to_json(s: &crate::file_analysis::Span) -> Value {
    json!({
        "start": [s.start.row, s.start.column],
        "end": [s.end.row, s.end.column],
    })
}

fn ref_key(r: &crate::file_analysis::Ref) -> String {
    format!(
        "{:?}|{}|{}:{}-{}:{}",
        r.kind, r.target_name,
        r.span.start.row, r.span.start.column,
        r.span.end.row, r.span.end.column,
    )
}

fn import_key(i: &crate::file_analysis::Import) -> String {
    let mut items: Vec<&str> = i.imported_symbols.iter().map(|s| s.local_name.as_str()).collect();
    items.sort();
    format!("{}|{:?}", i.module_name, items)
}

fn tc_key(tc: &crate::file_analysis::TypeConstraint) -> String {
    format!(
        "{}|{:?}|{}|{}:{}",
        tc.variable, tc.inferred_type, tc.scope.0,
        tc.constraint_span.start.row, tc.constraint_span.start.column,
    )
}

// ---- --plugin-check report ----

#[derive(Default)]
pub struct CheckReport {
    pub plugin_id: Option<String>,
    pub triggers: Vec<String>,
    pub hooks: Vec<String>,
    pub overrides_count: usize,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

impl CheckReport {
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
    pub fn to_json(&self) -> Value {
        json!({
            "id": self.plugin_id,
            "triggers": self.triggers,
            "hooks": self.hooks,
            "overrides": self.overrides_count,
            "errors": self.errors,
            "warnings": self.warnings,
            "ok": self.is_ok(),
        })
    }
    pub fn print_human(&self, path: &str) {
        eprintln!("{}", path);
        match &self.plugin_id {
            Some(id) => eprintln!("  id:        {}", id),
            None => eprintln!("  id:        <not loaded>"),
        }
        if !self.triggers.is_empty() {
            eprintln!("  triggers:  {}", self.triggers.join(", "));
        }
        if !self.hooks.is_empty() {
            eprintln!("  hooks:     {}", self.hooks.join(", "));
        }
        if self.overrides_count > 0 {
            eprintln!("  overrides: {}", self.overrides_count);
        }
        for w in &self.warnings {
            eprintln!("  warn: {}", w);
        }
        for e in &self.errors {
            eprintln!("  error: {}", e);
        }
        if self.is_ok() {
            eprintln!("  ok");
        }
    }
}

fn check_plugin_file(path: &Path) -> CheckReport {
    let mut report = CheckReport::default();

    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            report.errors.push(format!("read: {}", e));
            return report;
        }
    };

    // Static lints — run first so we surface footguns even if compile fails.
    lint_source(&source, &mut report);

    let engine = Arc::new(make_engine());
    match RhaiPlugin::from_source(&source, engine) {
        Ok(p) => {
            report.plugin_id = Some(p.id().to_string());
            report.triggers = p.triggers().iter().map(|t| format!("{:?}", t)).collect();
            report.overrides_count = p.overrides().len();
            // Hooks are inferred from the source listing — RhaiPlugin
            // doesn't expose them publicly. Scan for `fn on_*`.
            for hook in &[
                "on_function_call",
                "on_method_call",
                "on_use",
                "on_signature_help",
                "on_completion",
            ] {
                let needle = format!("fn {}", hook);
                if source.contains(&needle) {
                    report.hooks.push(hook.to_string());
                }
            }
        }
        Err(e) => {
            report.errors.push(e);
        }
    }

    report
}

/// Scan the script source for known footguns. We deliberately keep this
/// narrow — anything Rhai itself can flag (syntax errors, unknown
/// identifiers) is handled by the compile step. This pass catches things
/// Rhai *won't* catch at compile time.
fn lint_source(source: &str, report: &mut CheckReport) {
    // Reserved-keyword dot-access on context. `ctx.call` parses as
    // `ctx.call(...)` (a function call) and fails at runtime when the
    // user expected the `call` field. The fix is `ctx["call"]`.
    //
    // The minion plugin demonstrates this — every `ctx["call"]` in
    // frameworks/minion.rhai is a workaround for this exact issue.
    //
    // Only `ctx.call` is currently in our context fields; the other
    // reserved words don't collide with any field name we ship today.
    // Still warn on them in case authors hit a future field collision.
    let footguns = [
        ("ctx.call", "use ctx[\"call\"] — `call` is a Rhai reserved word"),
    ];
    for (line_idx, line) in source.lines().enumerate() {
        // Strip trailing comments to avoid false positives.
        let code = match line.find("//") {
            Some(i) => &line[..i],
            None => line,
        };
        for (needle, msg) in &footguns {
            if let Some(col) = code.find(needle) {
                // Reject `ctx.callsite` etc. — only flag when followed
                // by a non-identifier char (or end of line).
                let after = code[col + needle.len()..]
                    .chars()
                    .next();
                let is_identifier_continuation = matches!(after, Some(c) if c.is_alphanumeric() || c == '_');
                if !is_identifier_continuation {
                    report.warnings.push(format!(
                        "{}:{}: {}",
                        line_idx + 1,
                        col + 1,
                        msg,
                    ));
                }
            }
        }
    }

    // Required functions — RhaiPlugin::from_source enforces these, but
    // the error messages are easier to read when we say what's missing.
    if !source.contains("fn id(") {
        report.warnings.push("no `fn id()` defined".to_string());
    }
    if !source.contains("fn triggers(") {
        report.warnings.push("no `fn triggers()` defined".to_string());
    }
    // No emit or query hook present at all → plugin does nothing.
    let has_any_hook = ["on_function_call", "on_method_call", "on_use", "on_signature_help", "on_completion"]
        .iter()
        .any(|h| source.contains(&format!("fn {}", h)));
    if !has_any_hook {
        report.warnings.push("no hook function defined — plugin will load but produce no emissions".to_string());
    }
}

// ---- --plugin-test plumbing ----

#[derive(Debug)]
enum Outcome {
    Pass,
    Fail { diff: String },
    Captured,
    Updated,
    LoadError(String),
}

#[derive(Debug)]
struct TestResult {
    plugin: PathBuf,
    fixture: PathBuf,
    outcome: Outcome,
}

impl TestResult {
    fn to_json(&self) -> Value {
        let mut v = json!({
            "plugin": self.plugin.display().to_string(),
            "fixture": self.fixture.display().to_string(),
        });
        match &self.outcome {
            Outcome::Pass => v["outcome"] = json!("pass"),
            Outcome::Fail { diff } => {
                v["outcome"] = json!("fail");
                v["diff"] = json!(diff);
            }
            Outcome::Captured => v["outcome"] = json!("captured"),
            Outcome::Updated => v["outcome"] = json!("updated"),
            Outcome::LoadError(e) => {
                v["outcome"] = json!("load_error");
                v["error"] = json!(e);
            }
        }
        v
    }
    fn print_human(&self) {
        let label = match &self.outcome {
            Outcome::Pass => "PASS",
            Outcome::Fail { .. } => "FAIL",
            Outcome::Captured => "CAPTURED",
            Outcome::Updated => "UPDATED",
            Outcome::LoadError(_) => "LOAD-ERROR",
        };
        eprintln!(
            "{:9} {} on {}",
            label,
            self.plugin.display(),
            self.fixture.display(),
        );
        if let Outcome::Fail { diff } = &self.outcome {
            eprintln!("{}", indent(diff, "  "));
        }
        if let Outcome::LoadError(e) = &self.outcome {
            eprintln!("  {}", e);
        }
    }
}

fn run_one_test(plugin_path: &Path, fixture: &Path, update: bool) -> TestResult {
    let plugin = match load_single_plugin(plugin_path) {
        Ok(p) => p,
        Err(e) => {
            return TestResult {
                plugin: plugin_path.to_path_buf(),
                fixture: fixture.to_path_buf(),
                outcome: Outcome::LoadError(e),
            };
        }
    };

    let actual = run_plugin_on_fixture(plugin, fixture);
    let expected_path = expected_path_for(fixture);
    let actual_pretty = serde_json::to_string_pretty(&actual).unwrap();

    if !expected_path.exists() {
        if update {
            std::fs::write(&expected_path, &actual_pretty).ok();
            return TestResult {
                plugin: plugin_path.to_path_buf(),
                fixture: fixture.to_path_buf(),
                outcome: Outcome::Captured,
            };
        }
        return TestResult {
            plugin: plugin_path.to_path_buf(),
            fixture: fixture.to_path_buf(),
            outcome: Outcome::Fail {
                diff: format!(
                    "no expected snapshot at {}\nrun with --update to capture, or write the file by hand.\nActual:\n{}",
                    expected_path.display(),
                    actual_pretty,
                ),
            },
        };
    }

    let expected_text = std::fs::read_to_string(&expected_path).unwrap_or_default();
    if expected_text.trim() == actual_pretty.trim() {
        return TestResult {
            plugin: plugin_path.to_path_buf(),
            fixture: fixture.to_path_buf(),
            outcome: Outcome::Pass,
        };
    }

    if update {
        std::fs::write(&expected_path, &actual_pretty).ok();
        return TestResult {
            plugin: plugin_path.to_path_buf(),
            fixture: fixture.to_path_buf(),
            outcome: Outcome::Updated,
        };
    }

    TestResult {
        plugin: plugin_path.to_path_buf(),
        fixture: fixture.to_path_buf(),
        outcome: Outcome::Fail {
            diff: format_diff(&expected_text, &actual_pretty),
        },
    }
}

fn collect_plugins_in_dir(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if let Ok(read) = std::fs::read_dir(dir) {
        for entry in read.flatten() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("rhai") {
                out.push(path);
            }
        }
    }
    out.sort();
    out
}

fn collect_fixtures_in_dir(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let tests_dir = dir.join("tests");
    if let Ok(read) = std::fs::read_dir(&tests_dir) {
        for entry in read.flatten() {
            let path = entry.path();
            let ext = path.extension().and_then(|s| s.to_str());
            if matches!(ext, Some("pl") | Some("pm") | Some("t")) {
                out.push(path);
            }
        }
    }
    out.sort();
    out
}

fn expected_path_for(fixture: &Path) -> PathBuf {
    let stem = fixture.file_stem().and_then(|s| s.to_str()).unwrap_or("fixture");
    let mut p = fixture.to_path_buf();
    p.set_file_name(format!("{}.expected.json", stem));
    p
}

/// Cheap line-by-line diff. Doesn't try to be smart — for snapshot
/// drift, eyeballing the changed lines is what matters.
fn format_diff(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();
    let max = expected_lines.len().max(actual_lines.len());
    let mut out = String::new();
    out.push_str("--- expected\n+++ actual\n");
    for i in 0..max {
        let e = expected_lines.get(i).copied().unwrap_or("");
        let a = actual_lines.get(i).copied().unwrap_or("");
        if e == a {
            continue;
        }
        if !e.is_empty() {
            out.push_str(&format!("- {}\n", e));
        }
        if !a.is_empty() {
            out.push_str(&format!("+ {}\n", a));
        }
    }
    if out == "--- expected\n+++ actual\n" {
        out.push_str("(line count differs but content matches)\n");
    }
    out
}

fn indent(s: &str, prefix: &str) -> String {
    s.lines()
        .map(|l| format!("{}{}", prefix, l))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---- Shared helpers (mirror main.rs helpers) ----

fn is_json_format(args: &[String]) -> bool {
    args.windows(2).any(|w| w[0] == "--format" && w[1] == "json")
}

fn get_arg_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].as_str())
}

fn print_emissions_human(plugin_id: &str, fixture: &str, emissions: &Value) {
    eprintln!("plugin: {}", plugin_id);
    eprintln!("fixture: {}", fixture);
    let buckets = [
        "symbols",
        "refs",
        "framework_imports",
        "package_parents",
        "plugin_namespaces",
        "imports",
        "type_constraints",
    ];
    for b in &buckets {
        if let Some(arr) = emissions.get(b).and_then(|v| v.as_array()) {
            eprintln!("  {}: {}", b, arr.len());
        }
    }
    println!("{}", serde_json::to_string_pretty(emissions).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_temp(content: &str, suffix: &str) -> PathBuf {
        use std::io::Write;
        let dir = std::env::temp_dir();
        let nonce = std::process::id();
        let path = dir.join(format!(
            "perl-lsp-plugin-cli-{}-{}-{}",
            nonce,
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            suffix,
        ));
        let mut f = std::fs::File::create(&path).unwrap();
        f.write_all(content.as_bytes()).unwrap();
        path
    }

    #[test]
    fn check_reports_ok_for_minimal_plugin() {
        let path = write_temp(
            r#"
                fn id() { "demo" }
                fn triggers() { [ #{ Always: () } ] }
                fn on_function_call(ctx) { [] }
            "#,
            ".rhai",
        );
        let report = check_plugin_file(&path);
        assert!(report.is_ok(), "expected ok, got errors: {:?}", report.errors);
        assert_eq!(report.plugin_id.as_deref(), Some("demo"));
        assert!(report.hooks.iter().any(|h| h == "on_function_call"));
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn check_warns_on_ctx_dot_call() {
        let path = write_temp(
            r#"
                fn id() { "demo" }
                fn triggers() { [] }
                fn on_signature_help(ctx) {
                    let frame = ctx.call;
                    ()
                }
            "#,
            ".rhai",
        );
        let report = check_plugin_file(&path);
        assert!(
            report.warnings.iter().any(|w| w.contains("reserved word")),
            "expected reserved-word warning, got {:?}",
            report.warnings,
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn check_does_not_warn_on_ctx_call_span() {
        // `ctx.call_span` shouldn't trigger the `ctx.call` lint —
        // we only flag when followed by a non-identifier char.
        let path = write_temp(
            r#"
                fn id() { "demo" }
                fn triggers() { [] }
                fn on_function_call(ctx) {
                    let s = ctx.call_span;
                    []
                }
            "#,
            ".rhai",
        );
        let report = check_plugin_file(&path);
        assert!(
            !report.warnings.iter().any(|w| w.contains("reserved word")),
            "false positive: {:?}",
            report.warnings,
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn check_surfaces_compile_error() {
        let path = write_temp("fn this is not valid rhai", ".rhai");
        let report = check_plugin_file(&path);
        assert!(!report.is_ok());
        assert!(report.errors.iter().any(|e| e.contains("compile") || e.contains("parse")));
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn run_extracts_plugin_emissions() {
        // Synthetic plugin: each `widget('X')` call emits a Method named
        // after `X`. Two calls = two unique names = two emissions
        // (Method dedup keys on `(name, package, namespace)`).
        let plugin = write_temp(
            r#"
                fn id() { "widget" }
                fn triggers() { [ #{ Always: () } ] }
                fn on_function_call(ctx) {
                    if ctx.function_name != "widget" { return []; }
                    if ctx.args.len() < 1 { return []; }
                    let arg0 = ctx.args[0];
                    if arg0.string_value == () { return []; }
                    [
                        #{
                            Method: #{
                                name: arg0.string_value,
                                span: ctx.call_span,
                                selection_span: ctx.selection_span,
                                params: [],
                                is_method: false,
                                return_type: (),
                                doc: (),
                                on_class: (),
                                display: (),
                                hide_in_outline: false,
                                opaque_return: false,
                                outline_label: (),
                            }
                        }
                    ]
                }
            "#,
            ".rhai",
        );
        let fixture = write_temp("widget('first');\nwidget('second');\n", ".pl");
        let plugin_obj = load_single_plugin(&plugin).expect("compiles");
        let emissions = run_plugin_on_fixture(plugin_obj, &fixture);

        let symbols = emissions["symbols"].as_array().expect("symbols array");
        assert_eq!(symbols.len(), 2, "expected 2 widget emissions, got {:?}", symbols);
        let names: Vec<&str> = symbols.iter().map(|s| s["name"].as_str().unwrap()).collect();
        assert!(names.contains(&"first"), "names: {:?}", names);
        assert!(names.contains(&"second"), "names: {:?}", names);

        let _ = std::fs::remove_file(&plugin);
        let _ = std::fs::remove_file(&fixture);
    }
}
