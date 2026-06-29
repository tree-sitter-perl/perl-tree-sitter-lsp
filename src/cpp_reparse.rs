//! SPIKE: the C++ reparse seam — macro expansion before extraction.
//!
//! The C++ instance of `docs/prompt-cpp-reparse.md`'s reparse-hook
//! flavor. The obstacle course proved the worst, most common damage is
//! a declarator-position macro: `class API_EXPORT Widget {...}` reparses
//! as a `function_definition`, so the class evaporates. The fix is not
//! clang — it is *expansion*: replace the macro with its body and
//! re-parse. The probe (`dbg_cpp_attr_probe`) showed tree-sitter-cpp
//! handles the real attribute syntax (`__attribute__((...))`,
//! `__declspec(...)`) fine — the macro was merely hiding it. So the
//! transform is generic: **expand to body, let the parser validate.**
//!
//! Two flavors fall out of one pass:
//!   - object-like declarator macros (`API_EXPORT`) — the reparse-hook:
//!     expansion fixes a corrupted parse.
//!   - function-like declaration macros (`DECLARE_DYNAMIC(cls)`) — the
//!     emit-hook outcome achieved BY expansion: the body's member
//!     declarations become real, extractable symbols. Expansion
//!     subsumes the emit-hook for C++ (the doc's bet).
//!
//! Soundness is the stratified seam: this runs strictly upstream of
//! extraction (and of any witness bag), so it never interleaves with a
//! type fixpoint. A `SpliceMap` (transformed byte → original byte, the
//! Zed-anchor idea) carries every recovered span back to user text.
//!
//! Honest scope (measured, not hidden): single source-level pass with
//! pre-expanded bodies. Macros whose expansion itself contains further
//! macro CALLS (X-macros: `COLOR_LIST(X)` → `X(RED) X(GREEN)`) need
//! iterative source passes — out of scope here; that nested tail is
//! exactly the "amortize full cpp to once" case. Deliberately not wired
//! into the build pipeline; measured by `cpp_reparse_tests.rs`.

use std::collections::BTreeMap;
use tree_sitter::{Query, QueryCursor, StreamingIterator, Tree};

#[derive(Debug, Clone)]
pub struct Macro {
    /// `Some(params)` = function-like; `None` = object-like.
    pub params: Option<Vec<String>>,
    pub body: String,
}

/// One source replacement: original `[start,end)` → `replacement`.
#[derive(Debug, Clone)]
struct Splice {
    start: usize,
    end: usize,
    replacement: String,
}

/// Transformed-source ↔ original-source map under arbitrary splices.
/// `to_original(t)` collapses any byte inside a replacement to the
/// splice site (per-region granularity), and otherwise subtracts the
/// net length change of all earlier splices.
#[derive(Debug, Default, Clone)]
pub struct SpliceMap {
    /// (orig_start, orig_end, replacement_len), sorted by orig_start.
    edits: Vec<(usize, usize, usize)>,
}

impl SpliceMap {
    pub fn to_original(&self, transformed: usize) -> usize {
        let mut shift: isize = 0; // trans = orig + shift
        for &(os, oe, nlen) in &self.edits {
            let ts = (os as isize + shift) as usize;
            if transformed < ts {
                return (transformed as isize - shift) as usize;
            }
            if transformed < ts + nlen {
                return os; // inside the replacement → collapse to site
            }
            shift += nlen as isize - (oe - os) as isize;
        }
        (transformed as isize - shift) as usize
    }
}

const MACRO_DEF_QUERY: &str = r#"
(preproc_def name: (identifier) @oname value: (preproc_arg) @obody)
(preproc_function_def
  name: (identifier) @fname
  parameters: (preproc_params) @fparams
  value: (preproc_arg) @fbody)
"#;

/// Spans to never expand inside: string/char literals, comments, and
/// the preprocessor definition/conditional lines themselves.
const EXCLUDE_QUERY: &str = r#"
(string_literal) @x
(char_literal) @x
(comment) @x
(preproc_def) @x
(preproc_function_def) @x
(preproc_call) @x
(preproc_ifdef) @x
(preproc_if) @x
(preproc_include) @x
"#;

pub fn collect_macros(tree: &Tree, src: &[u8]) -> BTreeMap<String, Macro> {
    let lang = tree.language();
    let query = Query::new(&lang, MACRO_DEF_QUERY).expect("macro def query");
    let names: Vec<&str> = query.capture_names().to_vec();
    let mut out = BTreeMap::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&query, tree.root_node(), src);
    while let Some(m) = it.next() {
        let mut oname = None;
        let mut obody = None;
        let mut fname = None;
        let mut fparams: Option<Vec<String>> = None;
        let mut fbody = None;
        for c in m.captures {
            let txt = c.node.utf8_text(src).unwrap_or("");
            match names[c.index as usize] {
                "oname" => oname = Some(txt.to_string()),
                "obody" => obody = Some(clean_body(txt)),
                "fname" => fname = Some(txt.to_string()),
                "fparams" => {
                    fparams = Some(
                        txt.trim_start_matches('(')
                            .trim_end_matches(')')
                            .split(',')
                            .map(|s| s.trim().to_string())
                            .filter(|s| !s.is_empty())
                            .collect(),
                    )
                }
                "fbody" => fbody = Some(clean_body(txt)),
                _ => {}
            }
        }
        if let (Some(n), Some(b)) = (oname, obody) {
            out.insert(n, Macro { params: None, body: b });
        }
        if let (Some(n), Some(p), Some(b)) = (fname, fparams, fbody) {
            out.insert(n, Macro { params: Some(p), body: b });
        }
    }
    out
}

/// Strip line continuations and collapse the multi-line macro body to
/// single-line text suitable for in-place splicing.
fn clean_body(raw: &str) -> String {
    raw.replace("\\\n", " ")
        .replace('\\', " ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Resolve macro refs WITHIN bodies to a fixpoint (depth-capped), so a
/// single source-level pass suffices for non-recursive nesting.
fn pre_expand_bodies(macros: &BTreeMap<String, Macro>) -> BTreeMap<String, Macro> {
    let mut out = macros.clone();
    for _ in 0..8 {
        let mut changed = false;
        let snapshot = out.clone();
        for m in out.values_mut() {
            let expanded = expand_text(&m.body, &snapshot);
            if expanded != m.body {
                m.body = expanded;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    out
}

fn is_ident_byte(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphanumeric()
}

/// Expand object-like macros in a free text fragment (used for body
/// pre-expansion; no arg machinery — function-like refs in bodies are
/// left for the source pass).
fn expand_text(text: &str, macros: &BTreeMap<String, Macro>) -> String {
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < bytes.len() {
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &text[start..i];
            match macros.get(word) {
                Some(m) if m.params.is_none() => out.push_str(&m.body),
                _ => out.push_str(word),
            }
            continue;
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

/// ERROR + MISSING node count — the parser's own verdict on a parse.
pub fn parse_damage(node: tree_sitter::Node) -> usize {
    let mut n = 0;
    let mut cur = node.walk();
    let mut stack = vec![node];
    while let Some(x) = stack.pop() {
        if x.is_error() || x.is_missing() {
            n += 1;
        }
        for c in x.children(&mut cur) {
            stack.push(c);
        }
    }
    n
}

/// Expand, then **let the parser validate**: keep the transform only if
/// it does not increase parse damage. This is the doc's load-bearing
/// principle — an expansion that helps (declarator macros, simple
/// declaration macros) lands; one that hurts (nested macro CALLS like
/// X-macros, `##` token-paste — the tail this single-pass spike does
/// not model) is discarded, so expansion never regresses a file.
///
/// Production refinement (noted, not built): validate per-splice rather
/// than per-file, so a good expansion and a bad one in the same file
/// don't share a fate.
pub fn preprocess_validated(parser: &mut tree_sitter::Parser, src: &str) -> (String, SpliceMap) {
    let Some(tree) = parser.parse(src, None) else {
        return (src.to_string(), SpliceMap::default());
    };
    let before = parse_damage(tree.root_node());
    let (rewritten, map) = preprocess(&tree, src);
    if rewritten == src {
        return (rewritten, map);
    }
    match parser.parse(&rewritten, None) {
        Some(after) if parse_damage(after.root_node()) <= before => (rewritten, map),
        _ => (src.to_string(), SpliceMap::default()),
    }
}

/// `preprocess_validated` seeded with EXTERNAL macros from #included
/// headers — the cross-file path. Same validate-by-reparse gate (keep the
/// rewrite only if it doesn't raise parse damage).
pub fn preprocess_validated_with(
    parser: &mut tree_sitter::Parser,
    src: &str,
    external: &BTreeMap<String, Macro>,
) -> (String, SpliceMap) {
    let Some(tree) = parser.parse(src, None) else {
        return (src.to_string(), SpliceMap::default());
    };
    let before = parse_damage(tree.root_node());
    let (rewritten, map) = preprocess_with(&tree, src, external);
    if rewritten == src {
        return (rewritten, map);
    }
    match parser.parse(&rewritten, None) {
        Some(after) if parse_damage(after.root_node()) <= before => (rewritten, map),
        _ => (src.to_string(), SpliceMap::default()),
    }
}

/// Gather macros from a C++ file's transitively `#include`d headers, so a
/// macro `#define`d in another header (the `SPDLOG_NAMESPACE_BEGIN` idiom)
/// can be expanded in this file. Quoted includes resolve relative to the
/// file's dir, walking ancestor dirs as include roots (the classic search
/// path, discovered not configured). Bounded: depth + visited + header
/// caps; best-effort — unresolvable includes are skipped. The file's OWN
/// macros are NOT included here (the caller collects those).
pub fn included_macros(
    file_path: &std::path::Path,
    src: &str,
    parser: &mut tree_sitter::Parser,
) -> BTreeMap<String, Macro> {
    let mut macros = BTreeMap::new();
    let mut seen = std::collections::HashSet::new();
    if let Some(p) = file_path.canonicalize().ok() {
        seen.insert(p);
    }
    for inc in include_paths(src, parser) {
        if let Some(path) = resolve_include(file_path, &inc) {
            gather_macros_rec(&path, parser, &mut macros, &mut seen, 0);
        }
    }
    macros
}

/// A header's own #defines + its include edges — cached by (path, mtime)
/// so the per-edit re-gather doesn't re-read + re-parse the same dozens of
/// transitive headers every keystroke (the server is long-lived; headers
/// rarely change mid-edit, and mtime invalidates when they do).
struct CachedHeader {
    macros: BTreeMap<String, Macro>,
    includes: Vec<String>,
}

type HeaderCache = std::collections::HashMap<
    std::path::PathBuf,
    (std::time::SystemTime, std::sync::Arc<CachedHeader>),
>;

fn header_cache() -> &'static std::sync::Mutex<HeaderCache> {
    static C: std::sync::OnceLock<std::sync::Mutex<HeaderCache>> = std::sync::OnceLock::new();
    C.get_or_init(|| std::sync::Mutex::new(HeaderCache::new()))
}

fn gather_macros_rec(
    path: &std::path::Path,
    parser: &mut tree_sitter::Parser,
    macros: &mut BTreeMap<String, Macro>,
    seen: &mut std::collections::HashSet<std::path::PathBuf>,
    depth: usize,
) {
    if depth > 16 || seen.len() > 500 {
        return;
    }
    let Some(canon) = path.canonicalize().ok() else { return };
    if !seen.insert(canon.clone()) {
        return;
    }
    let mtime = std::fs::metadata(&canon).and_then(|m| m.modified()).ok();
    let hit = header_cache()
        .lock()
        .ok()
        .and_then(|c| c.get(&canon).and_then(|(t, info)| (Some(*t) == mtime).then(|| info.clone())));
    let info = match hit {
        Some(info) => info,
        None => {
            let Ok(src) = std::fs::read_to_string(&canon) else { return };
            let Some(tree) = parser.parse(&src, None) else { return };
            let info = std::sync::Arc::new(CachedHeader {
                macros: collect_macros(&tree, src.as_bytes()),
                includes: include_paths_tree(&tree, &src),
            });
            if let (Some(t), Ok(mut c)) = (mtime, header_cache().lock()) {
                c.insert(canon.clone(), (t, info.clone()));
            }
            info
        }
    };
    for (k, v) in &info.macros {
        macros.entry(k.clone()).or_insert_with(|| v.clone());
    }
    for inc in &info.includes {
        if let Some(next) = resolve_include(&canon, inc) {
            gather_macros_rec(&next, parser, macros, seen, depth + 1);
        }
    }
}

/// Resolve a quoted include like `spdlog/common.h` to a real path: walk up
/// from the file's dir, first ancestor `R` where `R/<inc>` exists wins.
fn resolve_include(file_path: &std::path::Path, inc: &str) -> Option<std::path::PathBuf> {
    let mut dir = file_path.parent()?;
    loop {
        let cand = dir.join(inc);
        if cand.is_file() {
            return Some(cand);
        }
        dir = dir.parent()?;
    }
}

fn include_paths(src: &str, parser: &mut tree_sitter::Parser) -> Vec<String> {
    match parser.parse(src, None) {
        Some(tree) => include_paths_tree(&tree, src),
        None => Vec::new(),
    }
}

/// Every include's path, quoted (`"x/y.h"`) and angle-bracket
/// (`<lib/y.h>`) alike — library headers write project includes with
/// `<>`, and the walk-up resolver finds both (true system headers like
/// `<vector>` simply don't resolve in the workspace, and are skipped).
fn include_paths_tree(tree: &Tree, src: &str) -> Vec<String> {
    let lang = tree.language();
    let q = Query::new(
        &lang,
        r#"
        (preproc_include path: (string_literal (string_content) @p))
        (preproc_include path: (system_lib_string) @s)
        "#,
    )
    .expect("include query");
    let names = q.capture_names().to_vec();
    let mut out = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&q, tree.root_node(), src.as_bytes());
    while let Some(m) = it.next() {
        for c in m.captures {
            let Ok(t) = c.node.utf8_text(src.as_bytes()) else { continue };
            match names[c.index as usize] {
                "p" => out.push(t.to_string()),
                "s" => out.push(t.trim_start_matches('<').trim_end_matches('>').to_string()),
                _ => {}
            }
        }
    }
    out
}

/// The transform: expand macro invocations in `src`, returning the
/// rewritten source and the anchor map. Single source-level pass.
pub fn preprocess(tree: &Tree, src: &str) -> (String, SpliceMap) {
    preprocess_with(tree, src, &BTreeMap::new())
}

/// `preprocess` plus EXTERNAL macros (gathered from #included headers via
/// `included_macros`). The file's own #defines win on conflict; the
/// external set fills in cross-file names like `SPDLOG_NAMESPACE_BEGIN`.
pub fn preprocess_with(
    tree: &Tree,
    src: &str,
    external: &BTreeMap<String, Macro>,
) -> (String, SpliceMap) {
    let mut macros = collect_macros(tree, src.as_bytes());
    for (k, v) in external {
        macros.entry(k.clone()).or_insert_with(|| v.clone());
    }
    let macros = pre_expand_bodies(&macros);
    if macros.is_empty() {
        return (src.to_string(), SpliceMap::default());
    }
    let excludes = exclusion_spans(tree);
    let bytes = src.as_bytes();
    let mut splices: Vec<Splice> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &src[start..i];
            if excludes.iter().any(|&(s, e)| start >= s && start < e) {
                continue;
            }
            if let Some(m) = macros.get(word) {
                match &m.params {
                    None => splices.push(Splice {
                        start,
                        end: i,
                        replacement: m.body.clone(),
                    }),
                    Some(params) => {
                        if let Some((args_end, args)) = scan_call_args(bytes, i) {
                            let replacement = substitute(&m.body, params, &args);
                            splices.push(Splice { start, end: args_end, replacement });
                            i = args_end;
                        }
                    }
                }
            }
            continue;
        }
        i += 1;
    }
    apply(src, &mut splices)
}

/// From just after a macro name, skip whitespace, require `(`, and scan
/// a balanced paren group; return (end_offset, top-level comma args).
fn scan_call_args(bytes: &[u8], mut j: usize) -> Option<(usize, Vec<String>)> {
    while j < bytes.len() && bytes[j].is_ascii_whitespace() {
        j += 1;
    }
    if j >= bytes.len() || bytes[j] != b'(' {
        return None;
    }
    let mut depth = 0i32;
    let mut args: Vec<String> = Vec::new();
    let mut cur = String::new();
    while j < bytes.len() {
        let c = bytes[j];
        match c {
            b'(' => {
                depth += 1;
                if depth > 1 {
                    cur.push('(');
                }
            }
            b')' => {
                depth -= 1;
                if depth == 0 {
                    if !cur.trim().is_empty() || !args.is_empty() {
                        args.push(cur.trim().to_string());
                    }
                    return Some((j + 1, args));
                }
                cur.push(')');
            }
            b',' if depth == 1 => {
                args.push(cur.trim().to_string());
                cur.clear();
            }
            _ => cur.push(c as char),
        }
        j += 1;
    }
    None
}

/// Whole-word substitute each param with its argument in a body.
fn substitute(body: &str, params: &[String], args: &[String]) -> String {
    let bytes = body.as_bytes();
    let mut out = String::with_capacity(body.len());
    let mut i = 0;
    while i < bytes.len() {
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &body[start..i];
            match params.iter().position(|p| p == word) {
                Some(idx) if idx < args.len() => out.push_str(&args[idx]),
                _ => out.push_str(word),
            }
            continue;
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

fn exclusion_spans(tree: &Tree) -> Vec<(usize, usize)> {
    let lang = tree.language();
    let query = Query::new(&lang, EXCLUDE_QUERY).expect("exclude query");
    let mut spans = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(&query, tree.root_node(), b"" as &[u8]);
    while let Some(m) = it.next() {
        for c in m.captures {
            spans.push((c.node.start_byte(), c.node.end_byte()));
        }
    }
    spans
}

fn apply(src: &str, splices: &mut [Splice]) -> (String, SpliceMap) {
    splices.sort_by_key(|s| s.start);
    let mut out = String::with_capacity(src.len());
    let mut map = SpliceMap::default();
    let mut prev = 0usize;
    for s in splices.iter() {
        if s.start < prev {
            continue; // overlapping (defensive) — skip
        }
        out.push_str(&src[prev..s.start]);
        out.push_str(&s.replacement);
        map.edits.push((s.start, s.end, s.replacement.len()));
        prev = s.end;
    }
    out.push_str(&src[prev..]);
    (out, map)
}

#[cfg(test)]
#[path = "cpp_reparse_tests.rs"]
mod tests;
