//! Locate + parse `perlfunc.pod` for builtin hover docs.
//!
//! All actual POD walking is delegated to [`crate::pod`] — the
//! tree-sitter-pod-backed renderer that already handles interior
//! sequences (C<>/B<>/L<>/E<>/…), nested formatting, verbatim
//! paragraphs, and multi-angle-bracket variants. This module is just
//! discovery (runtime perl → bundled fallback) + the perl-version
//! footer that gets appended to each rendered entry.
//!
//! Storage and lookup live in [`crate::module_cache`] /
//! [`crate::module_index`]: at startup the resolver thread writes
//! the parsed entries to SQLite (keyed by perl version, same
//! invalidation pattern as `inc_hash` / `plugin_fingerprint`) and
//! hydrates an in-memory mirror. Re-parse only happens when the
//! version tag changes.

use std::collections::HashMap;

/// Parsed entries: builtin name → markdown body (already rendered,
/// with the per-perl-version footer appended).
pub struct ParsedBuiltins {
    /// `"5.42.0 (runtime)"` / `"5.42.0 (bundled)"` — used as the
    /// SQLite invalidation key. The source suffix distinguishes
    /// runtime-parsed entries from bundled-fallback ones so we
    /// re-parse if the user installs `perl-doc` on what was
    /// previously a stripped image.
    pub perl_version: String,
    pub entries: HashMap<String, String>,
}

/// Bundled snapshot from the build host's perl. Only consulted when
/// the running perl doesn't ship `perlfunc.pod` (stripped Alpine
/// `perl-base` is the canonical case).
const BUNDLED_POD: &str = include_str!("../vendored/perlfunc.pod");
const BUNDLED_VERSION: &str = include_str!("../vendored/perlfunc.version");

/// Locate and parse `perlfunc.pod`. Prefers the running perl's copy
/// (so hover docs match the perl the user is targeting); falls back
/// to the bundled snapshot when the runtime POD isn't on disk.
pub fn parse_perlfunc() -> Option<ParsedBuiltins> {
    parse_runtime_perlfunc().or_else(parse_bundled_perlfunc)
}

fn parse_runtime_perlfunc() -> Option<ParsedBuiltins> {
    let perl_version = run_perl(&["-e", r#"printf "%vd", $^V"#])?;
    let priv_lib = run_perl(&["-MConfig", "-e", "print $Config{privlibexp}"])?;
    if perl_version.is_empty() || priv_lib.is_empty() {
        return None;
    }
    let pod = ["pods", "pod"].iter().find_map(|dir| {
        let path = std::path::PathBuf::from(&priv_lib).join(dir).join("perlfunc.pod");
        std::fs::read_to_string(&path).ok()
    })?;
    let tag = format!("{} (runtime)", perl_version);
    Some(ParsedBuiltins {
        entries: render_entries(&pod, &tag),
        perl_version: tag,
    })
}

fn parse_bundled_perlfunc() -> Option<ParsedBuiltins> {
    let version = BUNDLED_VERSION.trim();
    let tag = format!("{} (bundled)", version);
    Some(ParsedBuiltins {
        entries: render_entries(BUNDLED_POD, &tag),
        perl_version: tag,
    })
}

fn run_perl(args: &[&str]) -> Option<String> {
    let out = std::process::Command::new("perl")
        .args(args)
        .stdin(std::process::Stdio::null())
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Walk perlfunc.pod via the shared POD parser and decorate each
/// entry with the source-perl footer. The footer tells hover panels
/// whether the docs came from the user's perl or the bundled
/// fallback, which is the only piece of metadata that needs to ride
/// alongside the rendered body.
fn render_entries(pod: &str, perl_version_tag: &str) -> HashMap<String, String> {
    let mut entries = crate::pod::extract_perlfunc_items(pod);
    let footer = format!("\n\n*from perlfunc.pod (perl {})*", perl_version_tag);
    for body in entries.values_mut() {
        body.push_str(&footer);
    }
    entries
}

#[cfg(test)]
#[path = "builtins_pod_tests.rs"]
mod tests;
