//! Parser for `perlfunc.pod`. Pure — no state, no I/O.
//!
//! Storage and lookup live in [`module_cache`] / [`module_index`]:
//! - At startup, `module_index` discovers the running perl's
//!   `pods/perlfunc.pod`, parses it via this module, and writes the
//!   resulting entries to SQLite keyed by perl version. Re-parse only
//!   happens when the meta row says the perl version changed.
//! - Hover reads from the in-memory mirror that `ModuleIndex` hydrates
//!   from SQLite at construction time.

use std::collections::HashMap;

/// Parsed entries: builtin name → markdown body (already rendered).
pub struct ParsedBuiltins {
    /// `"5.42.0"` etc. — used as the cache invalidation key. The
    /// `source` suffix distinguishes runtime-parsed entries from
    /// bundled-fallback ones so we re-parse if the user installs
    /// `perl-doc` on what was previously a stripped image.
    pub perl_version: String,
    /// `name -> markdown` for each builtin found in perlfunc.pod.
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
/// Returns `None` only when both runtime and bundled paths fail —
/// which shouldn't happen in practice since the bundle ships in the
/// binary.
pub fn parse_perlfunc() -> Option<ParsedBuiltins> {
    if let Some(p) = parse_runtime_perlfunc() {
        return Some(p);
    }
    parse_bundled_perlfunc()
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
        entries: parse_perlfunc_pod(&pod, &tag),
        perl_version: tag,
    })
}

fn parse_bundled_perlfunc() -> Option<ParsedBuiltins> {
    let version = BUNDLED_VERSION.trim();
    let tag = format!("{} (bundled)", version);
    Some(ParsedBuiltins {
        entries: parse_perlfunc_pod(BUNDLED_POD, &tag),
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

/// Walk the POD line-by-line. Open a section on `=item <name>`; close
/// on the next `=item` or `=back`. Body accumulates verbatim, then
/// POD inline markup is lowered to markdown. Each entry gets a small
/// `*from perlfunc.pod (perl X.Y.Z)*` footer so the hover panel says
/// where the doc came from.
pub fn parse_perlfunc_pod(pod: &str, perl_version: &str) -> HashMap<String, String> {
    let mut out: HashMap<String, String> = HashMap::new();
    let mut current: Option<Section> = None;

    for line in pod.lines() {
        if let Some(rest) = line.strip_prefix("=item ") {
            flush(&mut out, current.take(), perl_version);
            let rest = rest.trim();
            let (name_tok, sig_tail) = match rest.split_once(char::is_whitespace) {
                Some((n, t)) => (n, t.trim()),
                None => (rest, ""),
            };
            let name = strip_inline_markup(name_tok)
                .trim_matches(|c: char| !c.is_alphanumeric() && c != '_')
                .to_string();
            if !is_plausible_builtin_name(&name) {
                continue;
            }
            current = Some(Section { name, signature: sig_tail.to_string(), body: String::new() });
            continue;
        }
        if line.starts_with("=back") || line.starts_with("=head") {
            flush(&mut out, current.take(), perl_version);
            continue;
        }
        if line.starts_with("=for ") || line.starts_with("X<") {
            continue;
        }
        if let Some(sec) = current.as_mut() {
            sec.body.push_str(line);
            sec.body.push('\n');
        }
    }
    flush(&mut out, current.take(), perl_version);
    out
}

struct Section {
    name: String,
    signature: String,
    body: String,
}

fn flush(out: &mut HashMap<String, String>, sec: Option<Section>, perl_version: &str) {
    if let Some(s) = sec {
        // First entry wins: perlfunc.pod lists some builtins twice
        // (`pos EXPR` then `pos`). The first body is the canonical one.
        out.entry(s.name.clone()).or_insert_with(|| render_entry(&s, perl_version));
    }
}

fn render_entry(s: &Section, perl_version: &str) -> String {
    let mut md = String::new();
    md.push_str("```perl\n");
    md.push_str(&s.name);
    if !s.signature.is_empty() {
        md.push(' ');
        md.push_str(&strip_inline_markup(&s.signature));
    }
    md.push_str("\n```\n\n");
    let lowered = strip_inline_markup(&s.body);
    md.push_str(trim_to_paragraphs(&lowered, 3).trim());
    md.push_str(&format!("\n\n*from perlfunc.pod (perl {})*", perl_version));
    md
}

fn is_plausible_builtin_name(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_lowercase() || c == '_' || c.is_ascii_digit())
}

fn trim_to_paragraphs(s: &str, n: usize) -> String {
    let mut out = String::new();
    let mut paragraph_count = 0;
    let mut in_paragraph = false;
    for line in s.lines() {
        if line.trim().is_empty() {
            if in_paragraph {
                paragraph_count += 1;
                if paragraph_count >= n {
                    break;
                }
            }
            in_paragraph = false;
        } else {
            in_paragraph = true;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

/// Lower POD inline markup to plain markdown. Non-nested only —
/// enough for perlfunc.pod's body shape.
fn strip_inline_markup(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if matches!(c, 'C' | 'B' | 'I' | 'L' | 'F' | 'S' | 'E' | 'X') && chars.peek() == Some(&'<') {
            chars.next();
            let mut inner = String::new();
            while let Some(&nc) = chars.peek() {
                if nc == '>' { chars.next(); break; }
                inner.push(nc);
                chars.next();
            }
            // L<text|target> → drop the target.
            let inner = inner.split('|').next().unwrap_or(&inner).to_string();
            match c {
                'C' | 'F' => { out.push('`'); out.push_str(&inner); out.push('`'); }
                'B' => { out.push_str("**"); out.push_str(&inner); out.push_str("**"); }
                'I' => { out.push('*'); out.push_str(&inner); out.push('*'); }
                _ => out.push_str(&inner),
            }
        } else {
            out.push(c);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture(body: &str) -> String {
        format!(
            "=head1 NAME\n\nfake perlfunc - test\n\n=over 8\n\n{}\n=back\n",
            body
        )
    }

    #[test]
    fn parses_basic_item() {
        let pod = fixture("=item push ARRAY,LIST\nX<push>\n\nAppends to the array.\n\nReturns the new length.\n");
        let map = parse_perlfunc_pod(&pod, "5.42.0");
        let push = map.get("push").expect("push entry exists");
        assert!(push.contains("push ARRAY,LIST"), "signature: {push}");
        assert!(push.contains("Appends to the array"), "body: {push}");
        assert!(push.contains("perl 5.42.0"), "version footer: {push}");
    }

    #[test]
    fn rejects_non_builtin_items() {
        // `=item *` (bullet) and `=item -X` (file-test) shouldn't slip in.
        let pod = fixture("=item *\n\nbullet\n\n=item -X\n\nfile test\n");
        let map = parse_perlfunc_pod(&pod, "5.42.0");
        assert!(map.is_empty(), "unexpected entries: {:?}", map.keys().collect::<Vec<_>>());
    }

    #[test]
    fn strips_inline_markup() {
        assert_eq!(strip_inline_markup("C<foo>"), "`foo`");
        assert_eq!(strip_inline_markup("B<bold>"), "**bold**");
        assert_eq!(strip_inline_markup("L<perlvar/$.>"), "perlvar/$.");
        assert_eq!(strip_inline_markup("L<text|target>"), "text");
    }

    #[test]
    fn first_entry_wins() {
        let pod = fixture("=item pos SCALAR\n\nfirst body\n\n=item pos\n\nsecond body\n");
        let map = parse_perlfunc_pod(&pod, "5.42.0");
        let pos = map.get("pos").expect("pos entry exists");
        assert!(pos.contains("first body"));
        assert!(!pos.contains("second body"));
    }
}
