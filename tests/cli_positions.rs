//! CLI position-rendering contract: `--references` / `--definition` print
//! 1-based line:col where col is a **character** column (not a byte offset).
//!
//! Regression guard for the bug where `--references` emitted raw 0-based
//! tree-sitter `Point`s while `--definition` emitted 1-based, and both used
//! byte columns — so a token after multi-byte UTF-8 reported a phantom column
//! past its visible position. The fixture deliberately puts a call site after a
//! unicode string literal on its line.

use std::process::Command;

const BIN: &str = env!("CARGO_BIN_EXE_perl-lsp");

/// `(file, 1-based-line, 1-based-char-col)` of every reported reference.
fn run_references(root: &std::path::Path, file: &str, line: usize, col: usize) -> Vec<(String, u64, u64)> {
    let mut cache = root.to_path_buf();
    cache.push(".test-cache");
    let out = Command::new(BIN)
        .args(["--references", root.to_str().unwrap(), file, &line.to_string(), &col.to_string()])
        .current_dir(root)
        .env("XDG_CACHE_HOME", &cache)
        .output()
        .expect("run perl-lsp --references");
    let stdout = String::from_utf8(out.stdout).expect("utf8 stdout");
    let parsed: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("references JSON parse ({e}): {stdout}"));
    parsed
        .as_array()
        .expect("references array")
        .iter()
        .map(|r| {
            (
                r["file"].as_str().unwrap().to_string(),
                r["line"].as_u64().unwrap(),
                r["col"].as_u64().unwrap(),
            )
        })
        .collect()
}

#[test]
fn references_and_definition_print_one_based_char_columns() {
    let dir = std::env::temp_dir().join(format!("perl-lsp-cli-pos-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let lib = dir.join("lib");
    std::fs::create_dir_all(&lib).unwrap();

    // Line layout (1-based display lines):
    //   1: package Greeter;
    //   2: (blank)
    //   3: sub greet { return "hi" }
    //   4: (blank)
    //   5: my $msg = "héllo wörld→"; greet();
    //   6: greet();
    // Line 5 has multi-byte UTF-8 (é, ö, →) BEFORE the `greet()` call, so a
    // byte-offset column would over-count. We assert the *character* column.
    let src = "package Greeter;\n\nsub greet { return \"hi\" }\n\nmy $msg = \"héllo wörld→\"; greet();\ngreet();\n";
    let file = lib.join("Greeter.pm");
    std::fs::write(&file, src).unwrap();

    // Cursor on the `greet` def name: line 2 (0-based), col 4 (0-based byte).
    let refs = run_references(&dir, "lib/Greeter.pm", 2, 4);

    // Ground truth, computed the same way an editor would: 1-based line,
    // 1-based character column of each `greet` occurrence.
    let expected: Vec<(u64, u64)> = src
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            let mut hits = Vec::new();
            let mut search_from = 0usize;
            while let Some(byte_idx) = line[search_from..].find("greet") {
                let abs = search_from + byte_idx;
                let char_col = line[..abs].chars().count() + 1;
                hits.push(((row + 1) as u64, char_col as u64));
                search_from = abs + "greet".len();
            }
            hits
        })
        .collect();

    let got: Vec<(u64, u64)> = {
        let mut v: Vec<(u64, u64)> = refs.iter().map(|(_, l, c)| (*l, *c)).collect();
        v.sort();
        v
    };
    let mut want = expected.clone();
    want.sort();

    assert_eq!(got, want, "reported positions must match true token line:col");

    // Specifically pin the unicode line: the `greet()` call on display line 5
    // starts after `my $msg = "héllo wörld→"; ` — 26 characters → 1-based col 27.
    // A byte-column renderer would report 31 here (é/ö 2 bytes, → 3 bytes),
    // past the visible token.
    let unicode_call = refs.iter().find(|(_, l, _)| *l == 5);
    assert_eq!(
        unicode_call.map(|(_, _, c)| *c),
        Some(27),
        "unicode-line call site must be a character column, not a byte offset"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// E2 (inline-callback form): a Mojo helper registered as
/// `$app->helper(name => sub ($c, ...) {...})` types the callback's first
/// positional as `Mojolicious::Controller`, not the enclosing class. Driven
/// through `--dump-package` so the real bundled `mojo-helpers` plugin runs.
/// The named-sub form (`$app->helper(name => \&_h); sub _h { my $c = shift }`)
/// is covered by `mojo_helper_named_sub_first_param_is_controller` below.
#[test]
fn mojo_helper_callback_first_param_is_controller() {
    let dir = std::env::temp_dir().join(format!("perl-lsp-cli-helper-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let lib = dir.join("lib");
    std::fs::create_dir_all(&lib).unwrap();
    let src = "package MyApp;\nuse Mojo::Base 'Mojolicious';\n\nsub startup {\n    my $self = shift;\n    $self->helper(greet => sub ($c, $name) {\n        my $x = $c;\n        return $name;\n    });\n}\n\n1;\n";
    std::fs::write(lib.join("MyApp.pm"), src).unwrap();

    let mut cache = dir.clone();
    cache.push(".test-cache");

    let out = Command::new(BIN)
        .args(["--dump-package", dir.to_str().unwrap(), "MyApp"])
        .current_dir(&dir)
        .env("XDG_CACHE_HOME", &cache)
        .output()
        .expect("run perl-lsp --dump-package");
    let stdout = String::from_utf8(out.stdout).expect("utf8 stdout");
    let dump: serde_json::Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("dump JSON parse ({e}): {stdout}"));

    // Find the anonymous helper callback sub and assert its `$c` param +
    // the `$x = $c` binding both type as the controller.
    let subs = dump["subs"].as_array().expect("subs array");
    let anon = subs
        .iter()
        .find(|s| s["name"] == "(anon)")
        .expect("anonymous callback sub in dump");
    let c_param = anon["params"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["name"] == "$c")
        .expect("$c param");
    assert_eq!(
        c_param["inferred_type"], "Mojolicious::Controller",
        "helper callback $c should type as the controller"
    );
    let x_var = anon["vars_in_scope"]
        .as_array()
        .unwrap()
        .iter()
        .find(|v| v["var"] == "$x")
        .expect("$x in scope");
    assert_eq!(
        x_var["type"], "Mojolicious::Controller",
        "binding from $c should propagate the controller type"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// E2 (named-sub form): `$app->helper(name => \&_greet); sub _greet { my $c =
/// shift; ... }` types `_greet`'s first positional as `Mojolicious::Controller`
/// — the same override the inline-callback form gets, carried to the named sub
/// by registration shape (not a name allowlist). Driven through
/// `--dump-package` so the bundled `mojo-helpers` plugin runs end-to-end.
#[test]
fn mojo_helper_named_sub_first_param_is_controller() {
    let dir = std::env::temp_dir().join(format!("perl-lsp-cli-helper-named-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let lib = dir.join("lib");
    std::fs::create_dir_all(&lib).unwrap();
    let src = "package MyApp;\nuse Mojo::Base 'Mojolicious';\n\nsub startup {\n    my $self = shift;\n    $self->helper(greet => \\&_greet);\n}\n\nsub _greet {\n    my $c = shift;\n    my $x = $c;\n    return $x;\n}\n\n1;\n";
    std::fs::write(lib.join("MyApp.pm"), src).unwrap();

    let mut cache = dir.clone();
    cache.push(".test-cache");

    let out = Command::new(BIN)
        .args(["--dump-package", dir.to_str().unwrap(), "MyApp"])
        .current_dir(&dir)
        .env("XDG_CACHE_HOME", &cache)
        .output()
        .expect("run perl-lsp --dump-package");
    let stdout = String::from_utf8(out.stdout).expect("utf8 stdout");
    let dump: serde_json::Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("dump JSON parse ({e}): {stdout}"));

    let subs = dump["subs"].as_array().expect("subs array");
    let greet = subs
        .iter()
        .find(|s| s["name"] == "_greet")
        .expect("_greet sub in dump");
    let c_param = greet["params"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["name"] == "$c")
        .expect("$c param on _greet");
    assert_eq!(
        c_param["inferred_type"], "Mojolicious::Controller",
        "named-sub helper's $c (my $c = shift) types as the controller"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn definition_matches_references_def_site() {
    // The def site appears in --references output; --definition must agree on
    // its line:col (both route through one renderer now).
    let dir = std::env::temp_dir().join(format!("perl-lsp-cli-def-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let lib = dir.join("lib");
    std::fs::create_dir_all(&lib).unwrap();
    let src = "package Greeter;\n\nsub greet { return \"hi\" }\n\ngreet();\n";
    std::fs::write(lib.join("Greeter.pm"), src).unwrap();

    let mut cache = dir.clone();
    cache.push(".test-cache");

    // Goto-def from the call on line 5 (0-based line 4), cursor on `greet`.
    let out = Command::new(BIN)
        .args(["--definition", dir.to_str().unwrap(), "lib/Greeter.pm", "4", "0"])
        .current_dir(&dir)
        .env("XDG_CACHE_HOME", &cache)
        .output()
        .expect("run perl-lsp --definition");
    let stdout = String::from_utf8(out.stdout).expect("utf8 stdout");

    // The def `sub greet` — name `greet` at 1-based line 3, char col 5.
    let trimmed = stdout.trim();
    assert!(
        trimmed.ends_with(":3:5"),
        "definition should print 1-based line:col 3:5, got {trimmed:?}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}
