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
use std::sync::OnceLock;
use tree_sitter::{Query, QueryCursor, StreamingIterator, Tree};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Macro {
    /// `Some(params)` = function-like; `None` = object-like.
    pub params: Option<Vec<String>>,
    pub body: String,
    /// Enclosing `#if`/`#ifdef`/`#else` conditions at the `#define`, OUTERMOST
    /// first — the config guard trail (`docs`: `cpp_macro_model`). Empty =
    /// unconditional. Rides the expansion-side rep so a config-variant macro
    /// carries WHICH config each body belongs to. `#[serde(default)]` for cache
    /// blobs written before guards existed.
    #[serde(default)]
    pub guards: Vec<String>,
    /// 0-based line of the `#define` — the variant's def site.
    #[serde(default)]
    pub def_line: usize,
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
///
/// Both lookups run per extracted span in `remap_spans` (O(symbols)), so
/// they must be sub-linear in the edit count. The edits partition the
/// TRANSFORMED axis into ordered, disjoint regions — replacement
/// `[ts_i, ts_i + nlen_i)` interleaved with pass-through gaps — so `ts`
/// (the transformed start of each replacement) is non-decreasing and a
/// binary search over it lands the containing region in O(log E). The
/// prefix state each region needs (`ts`, the shift accumulated *after*
/// each edit) is precomputed in `apply`; see `binary_search` for the
/// exact correspondence to the former linear scan.
#[derive(Debug, Default, Clone)]
pub struct SpliceMap {
    /// (orig_start, orig_end, replacement_len), sorted by orig_start.
    edits: Vec<(usize, usize, usize)>,
    /// `ts[i]` = transformed-axis start of edit `i`'s replacement
    /// (`orig_start + shift_before_i`). Non-decreasing — the search key.
    ts: Vec<usize>,
    /// `shift_after[i]` = cumulative `nlen - (oe - os)` through edit `i`
    /// inclusive (`trans = orig + shift`); the shift that applies in the
    /// pass-through gap *after* edit `i`.
    shift_after: Vec<isize>,
}

/// Where a transformed offset lands relative to the splice regions.
enum Region {
    /// Before every replacement, or in a pass-through gap: `orig = trans - shift`.
    PassThrough(isize),
    /// Inside edit `k`'s replacement — collapses to that macro-call site.
    Inside(usize),
}

impl SpliceMap {
    #[cfg(test)]
    pub(crate) fn edits_for_test(&self) -> &[(usize, usize, usize)] {
        &self.edits
    }

    /// Locate `transformed`'s region. `partition_point` returns the count
    /// of edits whose replacement starts at or before `transformed`; the
    /// last of them (`pp - 1`) is the only edit that can contain it
    /// (regions are disjoint and ordered). `<=` with `pp - 1` also picks
    /// the LATER of two edits sharing a `ts` — a zero-width replacement
    /// followed by a real one — matching the linear scan's in-order
    /// processing, where the empty region never claims a byte.
    fn region(&self, transformed: usize) -> Region {
        let pp = self.ts.partition_point(|&t| t <= transformed);
        if pp == 0 {
            return Region::PassThrough(0); // before every splice: shift is 0
        }
        let k = pp - 1;
        let (_os, _oe, nlen) = self.edits[k];
        if transformed < self.ts[k] + nlen {
            Region::Inside(k)
        } else {
            Region::PassThrough(self.shift_after[k])
        }
    }

    pub fn to_original(&self, transformed: usize) -> usize {
        match self.region(transformed) {
            Region::PassThrough(shift) => (transformed as isize - shift) as usize,
            Region::Inside(k) => self.edits[k].0, // collapse to the call site
        }
    }

    /// If `transformed` falls INSIDE a replacement (a macro expansion),
    /// return the replacement's ORIGINAL extent `(orig_start, orig_end)` —
    /// the macro-call site. A symbol/ref that came out of an expansion
    /// (`newThing(5)` → `Perl_newThing(aTHX_ 5)`) collapses to a zero-width
    /// point under `to_original`; callers use this to give it the call
    /// site's span instead, so goto-def/hover land on the macro call.
    pub fn replacement_at(&self, transformed: usize) -> Option<(usize, usize)> {
        match self.region(transformed) {
            Region::Inside(k) => Some((self.edits[k].0, self.edits[k].1)),
            Region::PassThrough(_) => None,
        }
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

const INCLUDE_QUERY: &str = r#"
(preproc_include path: (string_literal (string_content) @p))
(preproc_include path: (system_lib_string) @s)
"#;

/// Compile-once cache for this pipeline's queries. Every tree here comes
/// from the one `tree_sitter_cpp` grammar (the C/C++ driver), so a single
/// `Query` per source is reused across every reparse instead of rebuilding
/// the automaton per keystroke. `Query` is `Send + Sync`, so a static slot
/// is safe.
static MACRO_DEF_Q: OnceLock<Query> = OnceLock::new();
static EXCLUDE_Q: OnceLock<Query> = OnceLock::new();
static INCLUDE_Q: OnceLock<Query> = OnceLock::new();

fn cached_query(slot: &'static OnceLock<Query>, lang: &tree_sitter::Language, src: &str) -> &'static Query {
    slot.get_or_init(|| Query::new(lang, src).expect("cpp_reparse query"))
}

/// Walk `collect_macros`' query, calling `emit(name, Macro)` per `#define`
/// (object- and function-like). The Macro carries its config guard trail —
/// the enclosing `#if`/`#ifdef`/`#else` conditions — captured from the CST
/// ancestors of the def. Both the dedup'd table (expansion side) and the
/// variant-preserving collection route through here so the guard trail is
/// captured once.
fn walk_macro_defs(tree: &Tree, src: &[u8], mut emit: impl FnMut(String, Macro)) {
    let query = cached_query(&MACRO_DEF_Q, &tree.language(), MACRO_DEF_QUERY);
    let names: Vec<&str> = query.capture_names().to_vec();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(query, tree.root_node(), src);
    while let Some(m) = it.next() {
        let mut oname = None;
        let mut obody = None;
        let mut fname = None;
        let mut fparams: Option<Vec<String>> = None;
        let mut fbody = None;
        // Any name capture pins the def site (its parent is the preproc_def).
        let mut name_node: Option<tree_sitter::Node> = None;
        for c in m.captures {
            let txt = c.node.utf8_text(src).unwrap_or("");
            match names[c.index as usize] {
                "oname" => {
                    oname = Some(txt.to_string());
                    name_node = Some(c.node);
                }
                "obody" => obody = Some(clean_body(txt)),
                "fname" => {
                    fname = Some(txt.to_string());
                    name_node = Some(c.node);
                }
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
        let guards = name_node.map(|n| guard_trail(n, src)).unwrap_or_default();
        let def_line = name_node.map(|n| n.start_position().row).unwrap_or(0);
        if let (Some(n), Some(b)) = (oname, obody) {
            emit(n, Macro { params: None, body: b, guards: guards.clone(), def_line });
        }
        if let (Some(n), Some(p), Some(b)) = (fname, fparams, fbody) {
            emit(n, Macro { params: Some(p), body: b, guards, def_line });
        }
    }
}

pub fn collect_macros(tree: &Tree, src: &[u8]) -> BTreeMap<String, Macro> {
    let mut out = BTreeMap::new();
    walk_macro_defs(tree, src, |n, m| {
        out.insert(n, m);
    });
    out
}

/// The COMPLETE variant set per macro name — every `#define`, not the
/// collection-order winner `collect_macros` keeps. This is the config-variant
/// model input: a macro `#define`d three times under three different `#if`s
/// yields three variants, each with its guard trail + def site.
pub fn collect_macro_variants(
    tree: &Tree,
    src: &[u8],
) -> BTreeMap<String, Vec<Macro>> {
    let mut out: BTreeMap<String, Vec<Macro>> = BTreeMap::new();
    walk_macro_defs(tree, src, |n, m| {
        out.entry(n).or_default().push(m);
    });
    out
}

/// The config guard trail for a `#define` at `node` (a name identifier inside
/// the preproc_def): the enclosing `#if`/`#ifdef`/`#ifndef`/`#elif`/`#else`
/// conditions, OUTERMOST first. An else/elif branch negates the condition it
/// falls under; chained elifs accumulate the negations of preceding arms
/// because each `#elif`/`#else` is the `alternative` child of the arm before
/// it, and ascending through an `alternative` edge negates that arm's own
/// condition.
fn guard_trail(node: tree_sitter::Node, src: &[u8]) -> Vec<String> {
    let mut terms: Vec<String> = Vec::new();
    let mut prev = node;
    let mut cur = node.parent();
    while let Some(p) = cur {
        let is_alt = p
            .child_by_field_name("alternative")
            .map(|a| a.id())
            == Some(prev.id());
        match p.kind() {
            "preproc_if" | "preproc_elif" => {
                let cond = p
                    .child_by_field_name("condition")
                    .and_then(|c| c.utf8_text(src).ok())
                    .map(|t| t.split_whitespace().collect::<Vec<_>>().join(" "))
                    .unwrap_or_else(|| "1".to_string());
                terms.push(if is_alt { negate(&cond) } else { cond });
            }
            "preproc_ifdef" => {
                // Node kind is shared by #ifdef and #ifndef; the leading
                // directive text disambiguates.
                let name = p
                    .child_by_field_name("name")
                    .and_then(|c| c.utf8_text(src).ok())
                    .unwrap_or("")
                    .to_string();
                let ndef = src
                    .get(p.start_byte()..)
                    .and_then(|s| std::str::from_utf8(s).ok())
                    .map(|s| s.trim_start().starts_with("#ifndef"))
                    .unwrap_or(false);
                let base = if ndef {
                    format!("!defined({name})")
                } else {
                    format!("defined({name})")
                };
                terms.push(if is_alt { negate(&base) } else { base });
            }
            // #else contributes no condition of its own — the negation of the
            // arm it belongs to is applied when we ascend into the parent
            // conditional and see this else as its `alternative` child.
            _ => {}
        }
        prev = p;
        cur = p.parent();
    }
    terms.reverse();
    terms
}

fn negate(cond: &str) -> String {
    if let Some(inner) = cond.strip_prefix("!(").and_then(|s| s.strip_suffix(')')) {
        inner.to_string()
    } else if cond.starts_with("defined(") {
        format!("!{cond}")
    } else if cond.starts_with("!defined(") {
        cond.trim_start_matches('!').to_string()
    } else {
        format!("!({cond})")
    }
}

/// Largest a macro body may grow to during pre-expansion — a backstop
/// against pathological chains (the self-reference case is already cut by
/// the blue-paint guard; this bounds non-self fan-out too).
const MAX_BODY_LEN: usize = 64 * 1024;

/// Strip line continuations and collapse the multi-line macro body to
/// single-line text suitable for in-place splicing.
fn clean_body(raw: &str) -> String {
    // tree-sitter folds a trailing line comment into `preproc_arg`; the C
    // preprocessor strips comments, so drop it. Without this,
    // `#define M x  // M M` puts `M` in M's body — a self-reference.
    let raw = raw.find("//").map_or(raw, |i| &raw[..i]);
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
        for (name, m) in out.iter_mut() {
            // Blue paint: a macro never re-expands itself inside its own
            // body (C's rule) — without it `#define M M M` explodes.
            let expanded = expand_text(&m.body, &snapshot, Some(name));
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
/// left for the source pass). `exclude` is the macro being expanded (blue
/// paint: it isn't re-expanded in its own body).
fn expand_text(text: &str, macros: &BTreeMap<String, Macro>, exclude: Option<&str>) -> String {
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < bytes.len() {
        if out.len() > MAX_BODY_LEN {
            return out;
        }
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &text[start..i];
            match macros.get(word) {
                Some(m) if m.params.is_none() && Some(word) != exclude => out.push_str(&m.body),
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

/// Length-preserving blanking of an UNRESOLVED declarator-position macro.
/// `class API_EXPORT Foo {` — an export macro from a GENERATED header (Qt's
/// `Q_CORE_EXPORT`, never in the source tree) the gather can't reach —
/// parses as a corrupt function and the class evaporates. A class/struct
/// head with TWO identifiers before its body has a macro in the first slot:
/// valid C++ names the type once (the sole exception is `class Name final`).
/// Blank the macro token with spaces (same length → every extracted span
/// stays put, no SpliceMap needed) so the class parses. Runs unconditionally
/// — a no-op on normal `class Name {`. Returns the rewritten source plus the
/// `(class_name, macro_token)` pairs it recovered — the analyze path looks the
/// token up in the attribute-macro manifest to annotate the class with what
/// the macro signals (`exported`/`deprecated`); an unknown token still
/// recovers the class, it just carries no signal.
fn strip_declarator_macros(src: &str) -> (String, Vec<(String, String)>) {
    let bytes = src.as_bytes();
    let mut out = src.to_string();
    let mut recovered: Vec<(String, String)> = Vec::new();
    // SAFETY: only ASCII spaces are written, over ASCII identifier bytes —
    // length-preserving and UTF-8-valid.
    let ob = unsafe { out.as_bytes_mut() };
    let mut i = 0;
    while i < bytes.len() {
        let kwlen = if bytes[i..].starts_with(b"class") {
            5
        } else if bytes[i..].starts_with(b"struct") {
            6
        } else {
            i += 1;
            continue;
        };
        let word_boundary = (i == 0 || !is_ident_byte(bytes[i - 1]))
            && bytes.get(i + kwlen).is_some_and(|b| b.is_ascii_whitespace());
        if !word_boundary {
            i += kwlen;
            continue;
        }
        // IDENT1 (candidate macro), then IDENT2 (candidate name).
        let mut p = i + kwlen;
        let skip_ws = |p: &mut usize| while *p < bytes.len() && bytes[*p].is_ascii_whitespace() { *p += 1; };
        let read_id = |p: &mut usize| { let s = *p; while *p < bytes.len() && is_ident_byte(bytes[*p]) { *p += 1; } (s, *p) };
        skip_ws(&mut p);
        let (id1s, id1e) = read_id(&mut p);
        skip_ws(&mut p);
        let (id2s, id2e) = read_id(&mut p);
        skip_ws(&mut p);
        let head = p < bytes.len() && matches!(bytes[p], b'{' | b':' | b'<');
        if id1e > id1s && id2e > id2s && head {
            let id2 = &src[id2s..id2e];
            if id2 != "final" && id2 != "sealed" {
                recovered.push((id2.to_string(), src[id1s..id1e].to_string()));
                for b in &mut ob[id1s..id1e] {
                    *b = b' ';
                }
            }
        }
        i += kwlen;
    }
    (out, recovered)
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
    external: &PreExpandedExternal,
) -> (String, SpliceMap, Vec<(String, String)>) {
    // Blank unresolved declarator-position macros first (length-preserving,
    // unconditional — recovers `class Q_CORE_EXPORT Foo` even when the macro
    // is unreachable). Spans stay in original coordinates. `recovered` carries
    // each (class_name, macro_token) so the analyze path can annotate the
    // class with the macro's signal — surviving regardless of which return
    // arm fires below, since `src` is the stripped text throughout.
    let (stripped, recovered) = strip_declarator_macros(src);
    let src = stripped.as_str();
    let Some(tree) = parser.parse(src, None) else {
        return (src.to_string(), SpliceMap::default(), recovered);
    };
    let before = parse_damage(tree.root_node());
    let (rewritten, map) = preprocess_with(&tree, src, external);
    if rewritten == src {
        return (rewritten, map, recovered);
    }
    match parser.parse(&rewritten, None) {
        Some(after) if parse_damage(after.root_node()) <= before => (rewritten, map, recovered),
        _ => {
            // Full rewrite raised damage (perl.h-style macro soup, all-or-
            // nothing). Keep only the provably-safe IDENTIFIER-ALIAS
            // expansions (`op_prune_chain_head → Perl_op_prune_chain_head`)
            // so macro-name indirection — goto-def + references THROUGH the
            // alias — survives even when the rest is discarded.
            let (alias_rw, alias_map) = preprocess_with_mode(&tree, src, external, true);
            match (alias_rw != src).then(|| parser.parse(&alias_rw, None)).flatten() {
                Some(a) if parse_damage(a.root_node()) <= before => (alias_rw, alias_map, recovered),
                _ => (src.to_string(), SpliceMap::default(), recovered),
            }
        }
    }
}

/// Gather macros from a C++ file's transitively `#include`d headers, so a
/// macro `#define`d in another header (the `SPDLOG_NAMESPACE_BEGIN` idiom)
/// can be expanded in this file. Quoted includes resolve relative to the
/// file's dir, walking ancestor dirs as include roots (the classic search
/// path, discovered not configured). Bounded: depth + visited + header
/// caps; best-effort — unresolvable includes are skipped. The file's OWN
/// macros are NOT included here (the caller collects those).
/// Cached transitive-macro table, keyed by (file, its #include set). The
/// gather walks the whole include closure (perl.h reaches ~2000 macros over
/// hundreds of headers — seconds cold), so re-running it per completion
/// keystroke is untenable. The analyze pass warms this on open; completion
/// reuses it for free. Invalidates when the file's `#include` lines change
/// (header *content* edits mid-session aren't tracked — reopen to refresh).
type MacroTable = BTreeMap<String, Macro>;
fn macro_table_cache() -> &'static std::sync::Mutex<
    std::collections::HashMap<std::path::PathBuf, (u64, std::sync::Arc<MacroTable>)>,
> {
    static C: std::sync::OnceLock<
        std::sync::Mutex<std::collections::HashMap<std::path::PathBuf, (u64, std::sync::Arc<MacroTable>)>>,
    > = std::sync::OnceLock::new();
    C.get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()))
}

/// Hash of the file's `#include` directives — the cache key's variable part.
/// Cheap (one line scan); stable across edits that don't touch includes.
fn include_set_hash(src: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for line in src.lines() {
        let t = line.trim_start();
        if t.starts_with('#') && t[1..].trim_start().starts_with("include") {
            t.hash(&mut h);
        }
    }
    h.finish()
}

/// Bump when the persisted macro-table format or the gather's semantics
/// change in a way that invalidates on-disk blobs.
const MACRO_CACHE_VERSION: i64 = 2;

#[derive(serde::Serialize, serde::Deserialize)]
struct PersistedMacros {
    include_hash: u64,
    version: i64,
    /// Every transitively-#included header + its mtime — the table is valid
    /// only while none of them changed (cross-session correctness; the
    /// in-memory cache leans on include_hash alone within a session).
    headers: Vec<(std::path::PathBuf, i64)>,
    table: MacroTable,
}

/// On-disk macro-table cache dir, set once at startup (the CLI / LSP know
/// the workspace root → cache dir). `None` ⇒ persistence off (tests).
fn macro_persist_dir() -> &'static std::sync::OnceLock<Option<std::path::PathBuf>> {
    static D: std::sync::OnceLock<Option<std::path::PathBuf>> = std::sync::OnceLock::new();
    &D
}

/// Point the persisted macro cache at a workspace's cache dir (a `macros/`
/// subdir under it). Idempotent; first call wins.
pub fn set_macro_persist_dir(workspace_cache_dir: Option<std::path::PathBuf>) {
    let resolved = workspace_cache_dir.map(|d| {
        let p = d.join("macros");
        let _ = std::fs::create_dir_all(&p);
        p
    });
    let _ = macro_persist_dir().set(resolved);
}

fn persist_path(file_path: &std::path::Path) -> Option<std::path::PathBuf> {
    use std::hash::{Hash, Hasher};
    let dir = macro_persist_dir().get()?.clone()?;
    let mut h = std::collections::hash_map::DefaultHasher::new();
    file_path.hash(&mut h);
    Some(dir.join(format!("{:016x}.bin", h.finish())))
}

fn load_persisted(file_path: &std::path::Path, inc_hash: u64) -> Option<MacroTable> {
    let p = persist_path(file_path)?;
    let raw = zstd::decode_all(std::fs::read(&p).ok()?.as_slice()).ok()?;
    let pm: PersistedMacros = bincode::deserialize(&raw).ok()?;
    if pm.include_hash != inc_hash || pm.version != MACRO_CACHE_VERSION {
        return None;
    }
    if pm.headers.iter().any(|(hp, mt)| mtime_secs(hp) != *mt) {
        return None; // a header changed on disk
    }
    Some(pm.table)
}

fn save_persisted(
    file_path: &std::path::Path,
    inc_hash: u64,
    headers: Vec<(std::path::PathBuf, i64)>,
    table: &MacroTable,
) {
    let Some(p) = persist_path(file_path) else { return };
    let pm = PersistedMacros { include_hash: inc_hash, version: MACRO_CACHE_VERSION, headers, table: table.clone() };
    if let Ok(raw) = bincode::serialize(&pm) {
        if let Ok(z) = zstd::encode_all(raw.as_slice(), 3) {
            let _ = std::fs::write(&p, z);
        }
    }
}

thread_local! {
    /// When set on the current thread, `included_macros*` skip the cold gather.
    static GATHER_CACHED_ONLY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// When set on the current thread, `included_macros*` return whatever's cached
/// (in-mem or on-disk) but SKIP the cold gather — yielding an empty external
/// set (degraded) instead of blocking. `did_open` sets this so the first
/// analyze of a macro-heavy file is instant; a background task then runs the
/// real gather and re-analyzes (the async-refresh path).
pub fn set_gather_cached_only(v: bool) {
    GATHER_CACHED_ONLY.with(|c| c.set(v));
}
fn gather_cached_only() -> bool {
    GATHER_CACHED_ONLY.with(|c| c.get())
}

pub fn included_macros(
    file_path: &std::path::Path,
    src: &str,
    parser: &mut tree_sitter::Parser,
) -> std::sync::Arc<MacroTable> {
    included_macros_inner(file_path, src, parser, true)
        .unwrap_or_else(|| std::sync::Arc::new(MacroTable::new()))
}

/// The three-tier lookup. `allow_cold=false` (the on-open path) stops after the
/// two cache tiers, returning `None` rather than paying the cold gather — the
/// caller degrades to an empty external set and lets a background task warm it.
fn included_macros_inner(
    file_path: &std::path::Path,
    src: &str,
    parser: &mut tree_sitter::Parser,
    allow_cold: bool,
) -> Option<std::sync::Arc<MacroTable>> {
    let key = file_path.to_path_buf();
    let inc_hash = include_set_hash(src);
    // 1. in-memory (this session)
    if let Ok(cache) = macro_table_cache().lock() {
        if let Some((h, m)) = cache.get(&key) {
            if *h == inc_hash {
                return Some(m.clone());
            }
        }
    }
    // 2. on-disk (across sessions) — kills the cold-start gather
    if let Some(table) = load_persisted(file_path, inc_hash) {
        let arc = std::sync::Arc::new(table);
        if let Ok(mut cache) = macro_table_cache().lock() {
            cache.insert(key, (inc_hash, arc.clone()));
        }
        return Some(arc);
    }
    if !allow_cold {
        return None; // on-open: don't block on the cold gather
    }
    // 3. gather cold, then warm both tiers
    let (table, headers) = gather_included_macros(file_path, src, parser);
    save_persisted(file_path, inc_hash, headers, &table);
    let arc = std::sync::Arc::new(table);
    if let Ok(mut cache) = macro_table_cache().lock() {
        cache.insert(key, (inc_hash, arc.clone()));
    }
    Some(arc)
}

/// One pre-expanded variant of the external table + the identifiers its bodies
/// name. `table` is `pre_expand_bodies`d once; `body_idents` (every identifier
/// in a `table` body) drives the clean-split test — a file-local name in this
/// set means an external expansion would depend on it, so the split can't bake
/// it and the analyze falls to the slow single-tier path.
#[derive(Default)]
struct ExpandedVariant {
    table: MacroTable,
    body_idents: std::collections::HashSet<String>,
}

impl ExpandedVariant {
    fn of(macros: &MacroTable) -> Self {
        let table = pre_expand_bodies(macros);
        let body_idents = body_identifiers(&table);
        ExpandedVariant { table, body_idents }
    }
}

/// The EXTERNAL macro table (from the `#include` closure), mutually pre-expanded
/// ONCE per include-set and cached. External-referencing-external object refs
/// are baked into the variants, so the per-analyze transform never re-fixpoints
/// the huge external set (perl.h ≈ 2000 macros) — it fixpoints only the
/// file-LOCAL macros and resolves external names by lookup here. `raw` is
/// retained for the byte-identical slow fallback, whose single-tier merge +
/// fixpoint needs the un-pre-expanded external bodies.
#[derive(Default)]
pub struct PreExpandedExternal {
    raw: std::sync::Arc<MacroTable>,
    /// Full mutual pre-expansion (the `preprocess_with` path).
    full: ExpandedVariant,
    /// Identifier-alias subset only (the parse-damage `alias_only` fallback):
    /// `is_identifier_alias`-retained BEFORE expansion, matching the old
    /// merge-then-retain-then-fixpoint order.
    alias: ExpandedVariant,
}

impl PreExpandedExternal {
    pub fn empty() -> Self {
        Self::default()
    }

    fn from_raw(raw: std::sync::Arc<MacroTable>) -> Self {
        // This mutual pre-expansion is the O(external) work the two-tier split
        // hoists out of every analyze — paid ONCE per include-set here, then
        // reused warm. Labelled so `PERL_LSP_PHASE_TIMING` shows the per-analyze
        // cost it eliminates.
        let (full, alias) = crate::timings::phase("cpp.external_preexpand", || {
            let full = ExpandedVariant::of(&raw);
            let mut alias_src = (*raw).clone();
            alias_src.retain(|_, m| is_identifier_alias(m));
            (full, ExpandedVariant::of(&alias_src))
        });
        PreExpandedExternal { raw, full, alias }
    }

    fn variant(&self, alias_only: bool) -> &ExpandedVariant {
        if alias_only {
            &self.alias
        } else {
            &self.full
        }
    }

    /// Object-like gathered macros as `(name, body)` — the raw (un-pre-expanded)
    /// bodies, so a `#define X Y` stays an alias EDGE (`X → TypeName(Y)`) the
    /// bag chases, rather than a flattened leaf. The type-alias emission uses
    /// this to carry an include-closure's type macros (`U16TYPE` from a
    /// gitignored generated `config.h`) into every consuming file's bag, where
    /// the cross-file `TypeName` chase can never index the header directly.
    pub fn object_like_macros(&self) -> impl Iterator<Item = (&str, &str)> {
        self.raw
            .iter()
            .filter(|(_, m)| m.params.is_none())
            .map(|(k, m)| (k.as_str(), m.body.as_str()))
    }
}

/// Every identifier token appearing in any macro body — the reference
/// candidates. Used to detect an external body that (transitively, since
/// `expanded` bodies are already baked) names a file-local macro.
fn body_identifiers(macros: &MacroTable) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for m in macros.values() {
        let bytes = m.body.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
                let s = i;
                while i < bytes.len() && is_ident_byte(bytes[i]) {
                    i += 1;
                }
                out.insert(m.body[s..i].to_string());
            } else {
                i += 1;
            }
        }
    }
    out
}

type PreExpandedCache =
    std::collections::HashMap<std::path::PathBuf, (u64, std::sync::Arc<PreExpandedExternal>)>;

fn pre_expanded_cache() -> &'static std::sync::Mutex<PreExpandedCache> {
    static C: std::sync::OnceLock<std::sync::Mutex<PreExpandedCache>> = std::sync::OnceLock::new();
    C.get_or_init(|| std::sync::Mutex::new(PreExpandedCache::new()))
}

/// `included_macros` plus the one-time mutual pre-expansion of the external
/// table, cached by the same (file, include-set) key. Warm analyzes reuse the
/// pre-expanded table for free — the transform then only fixpoints file-local
/// macros. This is the driver's `gather_macros` hook.
pub fn included_macros_pre_expanded(
    file_path: &std::path::Path,
    src: &str,
    parser: &mut tree_sitter::Parser,
) -> std::sync::Arc<PreExpandedExternal> {
    let key = file_path.to_path_buf();
    let inc_hash = include_set_hash(src);
    if let Ok(cache) = pre_expanded_cache().lock() {
        if let Some((h, pe)) = cache.get(&key) {
            if *h == inc_hash {
                return pe.clone();
            }
        }
    }
    // In cached-only mode (on-open), a raw-table miss yields an EMPTY external
    // set that is deliberately NOT cached — so the background gather's real
    // table lands cleanly once it warms and this file is re-analyzed.
    let raw = match included_macros_inner(file_path, src, parser, !gather_cached_only()) {
        Some(raw) => raw,
        None => return std::sync::Arc::new(PreExpandedExternal::empty()),
    };
    let pe = std::sync::Arc::new(PreExpandedExternal::from_raw(raw));
    if let Ok(mut cache) = pre_expanded_cache().lock() {
        cache.insert(key, (inc_hash, pe.clone()));
    }
    pe
}

thread_local! {
    /// Each Rayon worker keeps its own `Parser` — tree-sitter parsers aren't
    /// `Sync`, so the parallel frontier can't share one. Created once per thread.
    static POOL_PARSER: std::cell::RefCell<Option<tree_sitter::Parser>> =
        const { std::cell::RefCell::new(None) };
}

/// Run `f` with this thread's pooled parser for `lang`.
fn with_pooled_parser<T>(
    lang: &tree_sitter::Language,
    f: impl FnOnce(&mut tree_sitter::Parser) -> T,
) -> T {
    POOL_PARSER.with(|slot| {
        let mut b = slot.borrow_mut();
        if b.is_none() {
            let mut p = tree_sitter::Parser::new();
            p.set_language(lang).expect("cpp grammar for pooled parser");
            *b = Some(p);
        }
        f(b.as_mut().expect("pooled parser present"))
    })
}

/// Walk the `#include` closure and collect every reachable header's macros.
///
/// Parallel + memoized: each BFS LEVEL's headers are parsed concurrently (Rayon,
/// one pooled `Parser` per worker); `header_info` memoizes by `(path, mtime)` so
/// a header shared across the closure — or across FILES (op.c and sv.c share
/// ~90% of perl5's tree) — is parsed exactly once. There is no header cap: the
/// `seen` set alone bounds the walk (cycles + re-visits), and the memoize bounds
/// the cost, so op.c's full closure is collected instead of truncated.
///
/// BREADTH-first, first-wins: the file's DIRECT includes are merged before
/// theirs, so the closest (most relevant) header's definition of a name wins —
/// the abseil `mutex.h`-vs-`thread_annotations.h` invariant. Determinism under
/// parallelism: a level is canonicalized + deduped SERIALLY in queue order, and
/// the parsed results are merged (and their children enqueued) in that same
/// order, so the macro table is byte-identical to the old single-queue BFS.
fn gather_included_macros(
    file_path: &std::path::Path,
    src: &str,
    parser: &mut tree_sitter::Parser,
) -> (BTreeMap<String, Macro>, Vec<(std::path::PathBuf, i64)>) {
    use rayon::prelude::*;
    let mut macros = BTreeMap::new();
    let mut headers: Vec<(std::path::PathBuf, i64)> = Vec::new();
    let mut seen = std::collections::HashSet::new();
    if let Ok(p) = file_path.canonicalize() {
        seen.insert(p);
    }
    let Some(lang) = parser.language().map(|l| (*l).clone()) else {
        return (macros, headers);
    };
    let mut frontier: Vec<std::path::PathBuf> = include_paths(src, parser)
        .iter()
        .filter_map(|inc| resolve_include(file_path, inc))
        .collect();
    while !frontier.is_empty() {
        // Canonicalize + dedup this level in queue order (cheap stat) so the
        // parallel parse below can't perturb the first-wins merge order.
        let mut level: Vec<std::path::PathBuf> = Vec::with_capacity(frontier.len());
        for path in frontier.drain(..) {
            let Ok(canon) = path.canonicalize() else { continue };
            if seen.insert(canon.clone()) {
                level.push(canon);
            }
        }
        // header_info is pure per header → parse the level concurrently.
        let infos: Vec<Option<std::sync::Arc<CachedHeader>>> = level
            .par_iter()
            .map(|canon| with_pooled_parser(&lang, |p| header_info(canon, p)))
            .collect();
        let mut next: Vec<std::path::PathBuf> = Vec::new();
        for (canon, info) in level.iter().zip(infos) {
            let Some(info) = info else { continue };
            headers.push((canon.clone(), mtime_secs(canon)));
            for (k, v) in &info.macros {
                macros.entry(k.clone()).or_insert_with(|| v.clone());
            }
            for inc in &info.includes {
                if let Some(nx) = resolve_include(canon, inc) {
                    next.push(nx);
                }
            }
        }
        frontier = next;
    }
    (macros, headers)
}

/// File mtime in whole seconds since the epoch (0 if unreadable) — the
/// persisted macro table's per-header validation stamp.
fn mtime_secs(path: &std::path::Path) -> i64 {
    std::fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

/// A header's cached (macros + include edges), by (path, mtime). The cache
/// makes the per-edit re-gather cheap (warm hits skip read+parse).
fn header_info(canon: &std::path::Path, parser: &mut tree_sitter::Parser) -> Option<std::sync::Arc<CachedHeader>> {
    let mtime = std::fs::metadata(canon).and_then(|m| m.modified()).ok();
    let hit = header_cache()
        .lock()
        .ok()
        .and_then(|c| c.get(canon).and_then(|(t, info)| (Some(*t) == mtime).then(|| info.clone())));
    if let Some(info) = hit {
        return Some(info);
    }
    let src = std::fs::read_to_string(canon).ok()?;
    let tree = parser.parse(&src, None)?;
    let info = std::sync::Arc::new(CachedHeader {
        macros: collect_macros(&tree, src.as_bytes()),
        includes: include_paths_tree(&tree, &src),
    });
    if let (Some(t), Ok(mut c)) = (mtime, header_cache().lock()) {
        c.insert(canon.to_path_buf(), (t, info.clone()));
    }
    Some(info)
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

/// The default C/C++ toolchain's discovered surface (system include roots +
/// predefined macros), probed once via the compiler and cached process-globally
/// (`OnceLock`). `None` when no compiler is on PATH — include resolution then
/// degrades to workspace-only (today's behavior). Probed as C++ so `include_dirs`
/// is the SUPERSET that also resolves the C system headers (`<sys/mman.h>`);
/// `predefined_macros` rides along for the `#if`-eval consumer.
pub fn toolchain_info() -> Option<&'static crate::cpp_toolchain::ToolchainInfo> {
    static INFO: std::sync::OnceLock<Option<crate::cpp_toolchain::ToolchainInfo>> =
        std::sync::OnceLock::new();
    INFO.get_or_init(|| {
        crate::cpp_toolchain::default_compiler(crate::cpp_toolchain::Lang::Cpp)
            .and_then(|c| crate::cpp_toolchain::probe(&c, None))
    })
    .as_ref()
}

/// System/stdlib include roots, in compiler search order — the `<...>` fallback
/// for headers no workspace ancestor holds (`<sys/mman.h>`). Empty when no
/// compiler was found.
fn system_include_dirs() -> &'static [std::path::PathBuf] {
    static DIRS: std::sync::OnceLock<Vec<std::path::PathBuf>> = std::sync::OnceLock::new();
    DIRS.get_or_init(|| toolchain_info().map(|t| t.include_dirs.clone()).unwrap_or_default())
}

/// Resolve an include like `spdlog/common.h` or `<sys/mman.h>` to a real path.
/// Workspace-first: walk up from the file's dir, first ancestor `R` where
/// `R/<inc>` exists wins (project/relative headers, quoted or angle-bracket).
/// Only when no ancestor has it do the toolchain's system roots answer — so a
/// system `<sys/mman.h>` resolves (its subtree was silently lost before), while
/// a project header still shadows a same-named system one.
fn resolve_include(file_path: &std::path::Path, inc: &str) -> Option<std::path::PathBuf> {
    if let Some(mut dir) = file_path.parent() {
        loop {
            let cand = dir.join(inc);
            if cand.is_file() {
                return Some(cand);
            }
            match dir.parent() {
                Some(p) => dir = p,
                None => break,
            }
        }
    }
    for root in system_include_dirs() {
        let cand = root.join(inc);
        if cand.is_file() {
            return Some(cand);
        }
    }
    None
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
    let q = cached_query(&INCLUDE_Q, &tree.language(), INCLUDE_QUERY);
    let names = q.capture_names().to_vec();
    let mut out = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(q, tree.root_node(), src.as_bytes());
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
    preprocess_with(tree, src, &PreExpandedExternal::empty())
}

/// `preprocess` plus EXTERNAL macros (gathered from #included headers via
/// `included_macros`). The file's own #defines win on conflict; the
/// external set fills in cross-file names like `SPDLOG_NAMESPACE_BEGIN`.
/// An object-like macro whose body is a single bare identifier — a pure
/// rename (`op_prune_chain_head → Perl_op_prune_chain_head`). Expanding it
/// is provably parse-safe (an identifier replaces an identifier; the token
/// structure is unchanged), so it can be kept even when the full
/// expansion's validate gate rejects the file.
fn is_identifier_alias(m: &Macro) -> bool {
    m.params.is_none()
        && !m.body.is_empty()
        && m.body.bytes().all(is_ident_byte)
}

pub fn preprocess_with(
    tree: &Tree,
    src: &str,
    external: &PreExpandedExternal,
) -> (String, SpliceMap) {
    preprocess_with_mode(tree, src, external, false)
}

/// The two-tier macro view the source-splice pass queries: file-LOCAL
/// macros (fixpointed per analyze) layered over the cached, pre-expanded
/// EXTERNAL table (external-referencing-external already baked). Local wins on
/// a name conflict. On the slow fallback, `local` holds the full merged +
/// fixpointed map and `external` is empty — a single-tier lookup.
struct EffectiveMacros<'a> {
    local: BTreeMap<String, Macro>,
    external: &'a BTreeMap<String, Macro>,
}

impl EffectiveMacros<'_> {
    fn get(&self, name: &str) -> Option<&Macro> {
        self.local.get(name).or_else(|| self.external.get(name))
    }
    fn is_empty(&self) -> bool {
        self.local.is_empty() && self.external.is_empty()
    }
}

fn empty_table() -> &'static BTreeMap<String, Macro> {
    static E: std::sync::OnceLock<BTreeMap<String, Macro>> = std::sync::OnceLock::new();
    E.get_or_init(BTreeMap::new)
}

fn force_slow_path() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("PERL_LSP_CPP_NO_FASTPATH").is_some())
}

/// Build the macro view for one analyze. FAST path (the common case): the file
/// LOCAL macros are the only set fixpointed here; external names resolve by
/// lookup into the cached, already-expanded `external.expanded`. SLOW path
/// (a local shadows an external name, or an external body references a local
/// name — both cheap to detect against the cached `body_idents`): merge the
/// raw external set with the locals and fixpoint the whole thing, exactly as a
/// single-tier expansion would — byte-identical, at the old cost.
fn build_effective_macros<'a>(
    tree: &Tree,
    src: &str,
    external: &'a PreExpandedExternal,
    alias_only: bool,
    force_slow: bool,
) -> EffectiveMacros<'a> {
    let local_all = collect_macros(tree, src.as_bytes());
    let ext = external.variant(alias_only);
    // Conservative clean-split test (ALL local names, pre-retain): if any local
    // name collides with an external def, or is named by any external body, the
    // two tiers interact and the split can't stay byte-identical → slow path.
    let clean = !force_slow
        && local_all
            .keys()
            .all(|k| !ext.table.contains_key(k) && !ext.body_idents.contains(k));
    if clean {
        let mut local = local_all;
        if alias_only {
            local.retain(|_, m| is_identifier_alias(m));
        }
        let local = pre_expand_local(local, &ext.table);
        EffectiveMacros { local, external: &ext.table }
    } else {
        let mut merged = local_all;
        for (k, v) in external.raw.iter() {
            merged.entry(k.clone()).or_insert_with(|| v.clone());
        }
        if alias_only {
            merged.retain(|_, m| is_identifier_alias(m));
        }
        EffectiveMacros { local: pre_expand_bodies(&merged), external: empty_table() }
    }
}

/// Fixpoint-expand only the LOCAL macro bodies (depth-capped, blue-painted),
/// resolving object-like references to file-local names among `local` and to
/// external names via the already-expanded (terminal) `external` table. The
/// external tier is never re-fixpointed or cloned — the whole point.
fn pre_expand_local(
    local: BTreeMap<String, Macro>,
    external: &BTreeMap<String, Macro>,
) -> BTreeMap<String, Macro> {
    let mut out = local;
    for _ in 0..8 {
        let mut changed = false;
        let snapshot = out.clone();
        for (name, m) in out.iter_mut() {
            let expanded = expand_text_layered(&m.body, &snapshot, external, Some(name));
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

/// `expand_text` over two tiers: an object-like reference resolves against
/// `primary` first (the fixpointing local snapshot), then `secondary` (the
/// terminal external table). Blue-paints `exclude` (a macro isn't re-expanded
/// in its own body).
fn expand_text_layered(
    text: &str,
    primary: &BTreeMap<String, Macro>,
    secondary: &BTreeMap<String, Macro>,
    exclude: Option<&str>,
) -> String {
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < bytes.len() {
        if out.len() > MAX_BODY_LEN {
            return out;
        }
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &text[start..i];
            let m = if Some(word) == exclude {
                None
            } else {
                primary.get(word).or_else(|| secondary.get(word))
            };
            match m {
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

/// `alias_only` restricts expansion to identifier-alias macros (the
/// validate-gate-safe subset) — used as the fallback when the full
/// expansion raises parse damage.
fn preprocess_with_mode(
    tree: &Tree,
    src: &str,
    external: &PreExpandedExternal,
    alias_only: bool,
) -> (String, SpliceMap) {
    preprocess_with_mode_inner(tree, src, external, alias_only, force_slow_path())
}

/// The splice pass proper, `force_slow` explicit (env-gate read at the public
/// boundary) so the differential test can drive the fast and slow tiers on the
/// same input and assert byte-identical output.
fn preprocess_with_mode_inner(
    tree: &Tree,
    src: &str,
    external: &PreExpandedExternal,
    alias_only: bool,
    force_slow: bool,
) -> (String, SpliceMap) {
    let eff = crate::timings::phase("cpp.macro_expand", || {
        build_effective_macros(tree, src, external, alias_only, force_slow)
    });
    if eff.is_empty() {
        return (src.to_string(), SpliceMap::default());
    }
    let excludes = exclusion_spans(tree);
    let bytes = src.as_bytes();
    let mut splices: Vec<Splice> = Vec::new();
    // `excludes` is sorted + disjoint and `start` only advances, so a
    // single cursor over it decides membership in O(1) amortized: drop
    // intervals that end at/before the current word, then the frontier
    // interval is the only one that can contain it.
    let mut ex = 0usize;
    let mut i = 0;
    while i < bytes.len() {
        if is_ident_byte(bytes[i]) && (i == 0 || !is_ident_byte(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident_byte(bytes[i]) {
                i += 1;
            }
            let word = &src[start..i];
            while ex < excludes.len() && excludes[ex].1 <= start {
                ex += 1;
            }
            if ex < excludes.len() && excludes[ex].0 <= start {
                continue; // start ∈ [s, e) of the frontier exclude → skip
            }
            if let Some(m) = eff.get(word) {
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

/// Byte ranges the expansion pass must not touch, returned SORTED and
/// COALESCED into maximal disjoint intervals. Query captures arrive in
/// document order but strings/comments nest inside preproc lines, so the
/// raw spans overlap; merging their union lets the caller test membership
/// with a single forward cursor (the words it tests only ever move
/// rightward) instead of scanning every span per word.
fn exclusion_spans(tree: &Tree) -> Vec<(usize, usize)> {
    let query = cached_query(&EXCLUDE_Q, &tree.language(), EXCLUDE_QUERY);
    let mut spans = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut it = cursor.matches(query, tree.root_node(), b"" as &[u8]);
    while let Some(m) = it.next() {
        for c in m.captures {
            spans.push((c.node.start_byte(), c.node.end_byte()));
        }
    }
    spans.sort_unstable();
    let mut merged: Vec<(usize, usize)> = Vec::with_capacity(spans.len());
    for (s, e) in spans {
        match merged.last_mut() {
            Some(last) if s <= last.1 => last.1 = last.1.max(e),
            _ => merged.push((s, e)),
        }
    }
    merged
}

fn apply(src: &str, splices: &mut [Splice]) -> (String, SpliceMap) {
    splices.sort_by_key(|s| s.start);
    let mut out = String::with_capacity(src.len());
    let mut map = SpliceMap::default();
    let mut prev = 0usize;
    // `shift` tracks `trans = orig + shift` as each applied splice lands,
    // so `ts`/`shift_after` (the binary-search index SpliceMap reads) are
    // built here rather than re-derived on every lookup. Skipped overlaps
    // never touch `shift`, so the index counts only applied edits — exactly
    // what the former linear scan iterated.
    let mut shift: isize = 0;
    for s in splices.iter() {
        if s.start < prev {
            continue; // overlapping (defensive) — skip
        }
        out.push_str(&src[prev..s.start]);
        out.push_str(&s.replacement);
        let nlen = s.replacement.len();
        map.ts.push((s.start as isize + shift) as usize);
        map.edits.push((s.start, s.end, nlen));
        shift += nlen as isize - (s.end - s.start) as isize;
        map.shift_after.push(shift);
        prev = s.end;
    }
    out.push_str(&src[prev..]);
    (out, map)
}

#[cfg(test)]
#[path = "cpp_reparse_tests.rs"]
mod tests;
