//! Fan-in acceptance for plugin-bridged route invocants (via `--references`).
//!
//! A Mojo route `->to('users#list')` is emitted by the mojo-routes plugin
//! as a `MethodCall` whose invocant is a controller TOKEN, not a class.
//! Before the `Invocant::Bridged` representation, the builder froze the
//! raw token as a class at build time, so `refs_to` trusted the bogus
//! edge and never linked the route to `Users::list` — the action showed
//! zero fan-in (wrongly "dead"). With bridged invocants the freeze is
//! skipped and `refs_to` consults the owning plugin at query time, so a
//! route-targeted action gets real fan-in while a route-less one stays
//! dead.
//!
//! Bin-only crate: the contract is exercised through the `--references`
//! CLI (the same `resolve_symbol` → `refs_to` path the heatmap counts),
//! treating "a reference in the app file" as one unit of fan-in.

use std::path::{Path, PathBuf};
use std::process::Command;

const BIN: &str = env!("CARGO_BIN_EXE_perl-lsp");

const APP_PL: &str = "\
package Some::App;
use Mojolicious::Lite;
sub startup {
    my $self = shift;
    $self->routes->get('/users')->to('users#list');
}
1;
";

// `--references` takes 0-based line/col: `sub list` is line 2, `sub
// orphan` line 3, and the method-name token begins at column 4
// (`sub ` = 4 chars).
const USERS_PM: &str = "\
package Some::App::Controller::Users;
use Mojo::Base 'Mojolicious::Controller';
sub list { my $c = shift; }
sub orphan { my $c = shift; }
1;
";

fn unique_root() -> PathBuf {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut p = std::env::temp_dir();
    p.push(format!("perl-lsp-heatmap-{}-{}", std::process::id(), nanos));
    p
}

fn write(path: &Path, contents: &str) {
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir).expect("mkdir");
    }
    std::fs::write(path, contents).expect("write fixture");
}

/// Number of references reported in `app.pl` for the method whose name
/// token sits at (`line`, `col`) in the controller file — i.e. its
/// route-driven fan-in.
fn fan_in_from_app(root: &Path, line: usize, col: usize) -> usize {
    let mut cache = root.to_path_buf();
    cache.push(".test-cache");
    let out = Command::new(BIN)
        .args([
            "--references",
            root.to_str().unwrap(),
            "lib/Some/App/Controller/Users.pm",
            &line.to_string(),
            &col.to_string(),
        ])
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
        .filter(|r| {
            r["file"]
                .as_str()
                .is_some_and(|f| f.ends_with("app.pl"))
        })
        .count()
}

#[test]
fn route_targeted_action_is_live_orphan_is_dead() {
    let root = unique_root();
    write(&root.join("app.pl"), APP_PL);
    write(&root.join("lib/Some/App/Controller/Users.pm"), USERS_PM);

    // `list` is targeted by `->to('users#list')` — real fan-in, alive.
    let list_fan_in = fan_in_from_app(&root, 2, 4);
    assert!(
        list_fan_in >= 1,
        "route-targeted `list` must have fan_in >= 1 (got {list_fan_in})",
    );

    // `orphan` has no route — zero fan-in, dead.
    let orphan_fan_in = fan_in_from_app(&root, 3, 4);
    assert_eq!(
        orphan_fan_in, 0,
        "route-less `orphan` must have fan_in == 0 (got {orphan_fan_in})",
    );

    let _ = std::fs::remove_dir_all(&root);
}
