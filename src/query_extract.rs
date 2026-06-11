//! SPIKE: query-driven entity extraction.
//!
//! The question under test: can FileAnalysis's extraction be driven by
//! declarative tree-sitter queries — entities out, procedural state
//! managed by a generic driver + per-language predicates — such that
//! the per-language part is DATA (a .scm query pack) rather than a
//! hand-written walker? If yes, the core is language-agnostic the way
//! highlights.scm/tags.scm consumers are.
//!
//! Architecture probed here:
//!   - `queries/perl/skeleton.scm` — patterns whose CAPTURE NAMES form
//!     a language-neutral entity vocabulary (`@def.*`, `@ref.*`,
//!     `@scope`, `@context.*`, `@import`).
//!   - `LangPack` — the per-language bundle: query source + host
//!     predicates for what patterns can't express (name shaping,
//!     suppression rules). The "back and forth": the driver owns
//!     ordered traversal and state (scope stack, sticky contexts);
//!     the pack answers point questions about text it understands.
//!   - `extract()` — the generic driver. Knows NO Perl: it sorts
//!     capture events, maintains the scope stack and sticky contexts,
//!     and assembles `SkelSymbol`/`SkelRef` rows.
//!
//! Findings live in `docs/spike-query-extraction.md`. This module is
//! deliberately not wired into the build pipeline — it exists to be
//! measured against the real builder by `query_extract_tests.rs`.

use tree_sitter::{Point, Query, QueryCursor, StreamingIterator, Tree};

/// The skeleton's symbol row — deliberately stringly-kinded: the kind
/// vocabulary comes from capture names, so the driver never enumerates
/// language entity kinds.
#[derive(Debug, Clone)]
pub struct SkelSymbol {
    pub kind: String,
    pub name: String,
    pub start: Point,
    pub end: Point,
    pub name_start: Point,
    /// Sticky `@context.package` value in force at the def site.
    pub package: Option<String>,
    /// Innermost `@scope` nesting depth (0 = file).
    pub scope_depth: usize,
}

#[derive(Debug, Clone)]
pub struct SkelRef {
    pub kind: String,
    pub name: String,
    pub start: Point,
}

#[derive(Debug, Default)]
pub struct SkeletonAnalysis {
    pub symbols: Vec<SkelSymbol>,
    pub refs: Vec<SkelRef>,
    pub imports: Vec<String>,
    pub scope_count: usize,
}

/// Per-language bundle: the query pack plus host predicates. The
/// predicates are the official escape hatch — the spike keeps them
/// MINIMAL on purpose so the findings honestly measure how far
/// patterns alone go.
pub struct LangPack {
    pub query_source: &'static str,
    /// Shape a captured name token's text (e.g. keep the sigil on a
    /// Perl variable). `capture_kind` is the vocabulary name
    /// (`def.var`, `ref.method`, ...) so one pack hook serves all.
    pub shape_name: fn(capture_kind: &str, raw: &str) -> String,
    /// Name for defs with no name token (anonymous subs).
    pub default_name: fn(kind: &str) -> Option<&'static str>,
}

pub fn perl_pack() -> LangPack {
    LangPack {
        query_source: include_str!("../queries/perl/skeleton.scm"),
        shape_name: |kind, raw| match kind {
            // The builder stores variable symbols WITH sigil; varname
            // captures are sigil-less. Predicate re-attaches nothing —
            // def.var captures the whole `(scalar)` node so raw text
            // already carries the sigil.
            _ => raw.to_string(),
        },
        default_name: |kind| match kind {
            "anon" => Some("(anon)"),
            _ => None,
        },
    }
}

/// One capture event, flattened from query matches and sorted by
/// position so the driver can run its state machine in source order.
struct Event {
    start_byte: usize,
    end_byte: usize,
    start: Point,
    end: Point,
    /// Capture vocabulary name, e.g. "def.sub", "def.sub.name",
    /// "scope", "context.package", "ref.call", "import.name".
    cap: String,
    text: String,
    /// Query match id — name captures join their parent def through it.
    match_id: usize,
}

pub fn extract(tree: &Tree, source: &[u8], pack: &LangPack) -> Result<SkeletonAnalysis, String> {
    let language = tree.language();
    let query = Query::new(&language, pack.query_source).map_err(|e| format!("query: {e}"))?;
    let cap_names: Vec<String> = query
        .capture_names()
        .iter()
        .map(|s| s.to_string())
        .collect();

    // ---- flatten matches into ordered events ----
    let mut events: Vec<Event> = Vec::new();
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&query, tree.root_node(), source);
    let mut match_counter = 0usize;
    while let Some(m) = matches.next() {
        match_counter += 1;
        for c in m.captures {
            let node = c.node;
            events.push(Event {
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
                start: node.start_position(),
                end: node.end_position(),
                cap: cap_names[c.index as usize].clone(),
                text: node.utf8_text(source).unwrap_or("").to_string(),
                match_id: match_counter,
            });
        }
    }
    // Source order; outermost first on ties so scopes push before
    // their contents.
    events.sort_by(|a, b| {
        a.start_byte
            .cmp(&b.start_byte)
            .then(b.end_byte.cmp(&a.end_byte))
    });

    // ---- join def name-captures to their def event ----
    use std::collections::HashMap;
    let mut names_by_match: HashMap<(usize, String), (String, Point)> = HashMap::new();
    for e in &events {
        if let Some(prefix) = e.cap.strip_suffix(".name") {
            names_by_match.insert((e.match_id, prefix.to_string()), (e.text.clone(), e.start));
        }
    }

    // ---- the state machine: scope stack + sticky contexts ----
    let mut out = SkeletonAnalysis::default();
    let mut scope_stack: Vec<usize> = Vec::new(); // end bytes
    let mut package: Option<String> = None;
    let mut def_name_spans: Vec<(usize, usize)> = Vec::new();

    for e in &events {
        while scope_stack.last().is_some_and(|&end| e.start_byte >= end) {
            scope_stack.pop();
        }
        match e.cap.as_str() {
            "scope" => {
                scope_stack.push(e.end_byte);
                out.scope_count += 1;
            }
            cap if cap.starts_with("context.") => {
                // Sticky linear context: in force from here until the
                // next assignment. (Perl's flat `package Foo;`.)
                package = Some(e.text.clone());
            }
            cap if cap.starts_with("def.") && !cap.ends_with(".name") => {
                let kind = cap.strip_prefix("def.").unwrap().to_string();
                let (name, name_start) = names_by_match
                    .get(&(e.match_id, e.cap.clone()))
                    .cloned()
                    .or_else(|| (pack.default_name)(&kind).map(|n| (n.to_string(), e.start)))
                    .unwrap_or((e.text.clone(), e.start));
                def_name_spans.push((e.start_byte, e.end_byte));
                out.symbols.push(SkelSymbol {
                    name: (pack.shape_name)(&format!("def.{kind}"), &name),
                    kind,
                    start: e.start,
                    end: e.end,
                    name_start,
                    package: package.clone(),
                    scope_depth: scope_stack.len(),
                });
            }
            cap if cap.starts_with("ref.") => {
                // Generic suppression: a "reference" inside a def's own
                // header is the declaration, not a use.
                let inside_def = def_name_spans
                    .iter()
                    .any(|&(s, en)| e.start_byte >= s && e.end_byte <= en && {
                        // only suppress when it IS the def name region
                        // (cheap heuristic: same start)
                        s == e.start_byte || en == e.end_byte
                    });
                if !inside_def {
                    out.refs.push(SkelRef {
                        kind: e.cap.strip_prefix("ref.").unwrap().to_string(),
                        name: (pack.shape_name)(&e.cap, &e.text),
                        start: e.start,
                    });
                }
            }
            "import.name" => out.imports.push(e.text.clone()),
            _ => {}
        }
    }
    Ok(out)
}

#[cfg(test)]
#[path = "query_extract_tests.rs"]
mod tests;
