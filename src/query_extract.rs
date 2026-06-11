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

use crate::file_analysis::{InferredType, Span};
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
    pub name_end: Point,
    /// Sticky `@context.package` value in force at the def site.
    pub package: Option<String>,
    /// Innermost `@scope` nesting depth (0 = file).
    pub scope_depth: usize,
    pub scope: crate::file_analysis::ScopeId,
}

#[derive(Debug, Clone)]
pub struct SkelRef {
    pub kind: String,
    pub name: String,
    pub start: Point,
    pub end: Point,
    pub scope: crate::file_analysis::ScopeId,
}

#[derive(Debug, Default)]
pub struct SkeletonAnalysis {
    pub symbols: Vec<SkelSymbol>,
    pub refs: Vec<SkelRef>,
    pub imports: Vec<String>,
    pub scope_count: usize,
    pub scopes: Vec<crate::file_analysis::Scope>,
    pub witnesses: Vec<crate::witnesses::Witness>,
}

impl SkeletonAnalysis {
    /// Assemble a REAL `FileAnalysis` — production model, production
    /// indices, production reducer registry behind every query — from
    /// nothing but capture events. The existence proof that the engine
    /// is language-agnostic above this seam.
    pub fn into_file_analysis(self) -> crate::file_analysis::FileAnalysis {
        use crate::file_analysis::{
            FileAnalysis, FileAnalysisParts, SymKind, Symbol, SymbolDetail, SymbolId,
        };
        let mut bag = crate::witnesses::WitnessBag::default();
        for w in self.witnesses {
            bag.push(w);
        }
        let symbols: Vec<Symbol> = self
            .symbols
            .iter()
            .enumerate()
            .map(|(i, s)| Symbol {
                id: SymbolId(i as u32),
                name: s.name.clone(),
                kind: match s.kind.as_str() {
                    "package" => SymKind::Package,
                    "class" => SymKind::Class,
                    "sub" | "anon" | "constant" => SymKind::Sub,
                    "method" => SymKind::Method,
                    _ => SymKind::Variable,
                },
                span: Span { start: s.start, end: s.end },
                selection_span: Span { start: s.name_start, end: s.name_end },
                scope: s.scope,
                package: s.package.clone(),
                detail: SymbolDetail::None,
                namespace: crate::file_analysis::Namespace::Language,
                outline_label: None,
            })
            .collect();
        let refs: Vec<crate::file_analysis::Ref> = self
            .refs
            .iter()
            .filter(|r| r.kind == "call")
            .map(|r| crate::file_analysis::Ref {
                kind: crate::file_analysis::RefKind::FunctionCall { resolved_package: None },
                span: Span { start: r.start, end: r.end },
                scope: r.scope,
                target_name: r.name.clone(),
                access: crate::file_analysis::AccessKind::Read,
                resolves_to: None,
                resolved_method_target: None,
            })
            .collect();
        FileAnalysis::new(FileAnalysisParts {
            scopes: self.scopes,
            symbols,
            refs,
            witnesses: bag,
            ..Default::default()
        })
    }
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
    /// Map a `@type.annot` token's text to a type — the pack predicate
    /// for languages whose ring 3 is partly in the tree (`x: int`).
    pub annot_type: fn(text: &str) -> Option<InferredType>,
    /// Is a bare call to `name` a CONSTRUCTOR in this language's
    /// conventions? (Python: PEP8 capitalization; Perl: ->new, handled
    /// by the walker.) Returns the instantiated class name. This is a
    /// pack predicate for the same reason conventions.rs exists: name
    /// conventions are language facts, not engine facts.
    pub ctor_class: fn(callee: &str) -> Option<String>,
    /// Module-name → workspace-relative candidate paths — the entire
    /// per-language cross-file resolution strategy ("the one executable
    /// line"). Python: `pkg.mod` → pkg/mod.py | pkg/mod/__init__.py.
    pub module_paths: fn(module: &str) -> Vec<String>,
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
        annot_type: |_| None,
        ctor_class: |_| None,
        module_paths: |m| vec![format!("{}.pm", m.replace("::", "/"))],
    }
}

pub fn python_pack() -> LangPack {
    LangPack {
        query_source: include_str!("../queries/python/skeleton.scm"),
        shape_name: |_, raw| raw.to_string(),
        default_name: |_| None,
        annot_type: |text| match text.trim() {
            "str" => Some(InferredType::String),
            "int" | "float" => Some(InferredType::Numeric),
            "list" => Some(InferredType::ArrayRef),
            "dict" => Some(InferredType::HashRef),
            t if t.chars().next().is_some_and(|c| c.is_uppercase()) => {
                Some(InferredType::ClassName(t.to_string()))
            }
            _ => None,
        },
        ctor_class: |callee| {
            callee
                .chars()
                .next()
                .is_some_and(|c| c.is_uppercase())
                .then(|| callee.to_string())
        },
        module_paths: |m| {
            let base = m.replace('.', "/");
            vec![format!("{base}.py"), format!("{base}/__init__.py")]
        },
    }
}

/// `expr.lit.<t>` suffix → type. ENGINE-side vocabulary, not per-pack:
/// the suffix set names the engine's value lattice, packs just choose
/// which nodes carry each suffix.
fn lit_type(suffix: &str) -> Option<InferredType> {
    match suffix {
        "string" => Some(InferredType::String),
        "number" => Some(InferredType::Numeric),
        "arrayref" => Some(InferredType::ArrayRef),
        "hashref" => Some(InferredType::HashRef),
        _ => None,
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
    let mut names_by_match: HashMap<(usize, String), (String, Point, Point)> = HashMap::new();
    for e in &events {
        if let Some(prefix) = e.cap.strip_suffix(".name") {
            names_by_match
                .insert((e.match_id, prefix.to_string()), (e.text.clone(), e.start, e.end));
        }
    }

    // ---- the state machine: scope stack + sticky contexts ----
    let mut out = SkeletonAnalysis::default();
    // (end_byte, ScopeId) — real Scope rows are minted as we go so the
    // resulting FileAnalysis carries a genuine lexical tree.
    let mut scope_stack: Vec<(usize, crate::file_analysis::ScopeId)> = Vec::new();
    // (registered_at_depth, value) — a context set inside a scope pops
    // with it (Python class blocks); one set at file depth is sticky
    // (Perl's flat `package Foo;`).
    let mut context_stack: Vec<(usize, String)> = Vec::new();
    let mut def_name_spans: Vec<(usize, usize)> = Vec::new();

    use crate::file_analysis::{Scope, ScopeId, ScopeKind};
    out.scopes.push(Scope {
        id: ScopeId(0),
        parent: None,
        kind: ScopeKind::File,
        span: Span { start: tree.root_node().start_position(), end: tree.root_node().end_position() },
        package: None,
    });
    scope_stack.push((tree.root_node().end_byte(), ScopeId(0)));

    // flow.assign joins: match_id → (target name+scope, source span)
    let mut flow_targets: HashMap<usize, (String, ScopeId, Point)> = HashMap::new();
    let mut flow_sources: HashMap<usize, Span> = HashMap::new();
    let mut annots: HashMap<usize, String> = HashMap::new();
    // expr-literal spans, for narrowing an Edge target onto the actual
    // literal when the rhs node wraps it.
    let mut lit_spans: Vec<(usize, usize, Span)> = Vec::new();

    for e in &events {
        while scope_stack.len() > 1
            && scope_stack.last().is_some_and(|&(end, _)| e.start_byte >= end)
        {
            scope_stack.pop();
            while context_stack.last().is_some_and(|&(d, _)| d > scope_stack.len()) {
                context_stack.pop();
            }
        }
        let cur_scope = scope_stack.last().unwrap().1;
        let package: Option<String> = context_stack.last().map(|(_, p)| p.clone());
        match e.cap.as_str() {
            "scope" => {
                let id = ScopeId(out.scopes.len() as u32);
                out.scopes.push(Scope {
                    id,
                    parent: Some(cur_scope),
                    kind: ScopeKind::Block,
                    span: Span { start: e.start, end: e.end },
                    package: package.clone(),
                });
                scope_stack.push((e.end_byte, id));
                out.scope_count += 1;
            }
            cap if cap.starts_with("context.") => {
                // Replace any context at the same depth; deeper ones
                // were already popped with their scopes.
                while context_stack
                    .last()
                    .is_some_and(|&(d, _)| d >= scope_stack.len())
                {
                    context_stack.pop();
                }
                context_stack.push((scope_stack.len(), e.text.clone()));
            }
            cap if cap.starts_with("def.") && !cap.ends_with(".name") => {
                let kind = cap.strip_prefix("def.").unwrap().to_string();
                let (name, name_start, name_end) = names_by_match
                    .get(&(e.match_id, e.cap.clone()))
                    .cloned()
                    .or_else(|| {
                        (pack.default_name)(&kind).map(|n| (n.to_string(), e.start, e.start))
                    })
                    .unwrap_or((e.text.clone(), e.start, e.end));
                def_name_spans.push((e.start_byte, e.end_byte));
                out.symbols.push(SkelSymbol {
                    name: (pack.shape_name)(&format!("def.{kind}"), &name),
                    kind,
                    start: e.start,
                    end: e.end,
                    name_start,
                    name_end,
                    package: package.clone(),
                    scope_depth: scope_stack.len(),
                    scope: cur_scope,
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
                        end: e.end,
                        scope: cur_scope,
                    });
                }
            }
            "import.name" => out.imports.push(e.text.clone()),
            cap if cap.starts_with("expr.lit.") => {
                let suffix = cap.strip_prefix("expr.lit.").unwrap();
                if let Some(t) = lit_type(suffix) {
                    let span = Span { start: e.start, end: e.end };
                    lit_spans.push((e.start_byte, e.end_byte, span));
                    out.witnesses.push(crate::witnesses::Witness {
                        attachment: crate::witnesses::WitnessAttachment::Expr(span),
                        source: crate::witnesses::WitnessSource::Builder("skeleton".into()),
                        payload: crate::witnesses::WitnessPayload::InferredType(t),
                        span,
                    });
                }
            }
            "expr.read.var" => {
                // a variable READ is an edge: Expr(span) resolves to
                // whatever the Variable resolves to — same shape the
                // builder's emit_expr_witness uses.
                let span = Span { start: e.start, end: e.end };
                out.witnesses.push(crate::witnesses::Witness {
                    attachment: crate::witnesses::WitnessAttachment::Expr(span),
                    source: crate::witnesses::WitnessSource::Builder("skeleton".into()),
                    payload: crate::witnesses::WitnessPayload::Edge(
                        crate::witnesses::WitnessAttachment::Variable {
                            name: (pack.shape_name)("ref.var", &e.text),
                            scope: cur_scope,
                        },
                    ),
                    span,
                });
            }
            "flow.target" => {
                flow_targets.insert(
                    e.match_id,
                    ((pack.shape_name)("def.var", &e.text), cur_scope, e.start),
                );
            }
            "flow.source" => {
                flow_sources.insert(e.match_id, Span { start: e.start, end: e.end });
            }
            "type.annot" => {
                annots.insert(e.match_id, e.text.clone());
            }
            cap if cap.starts_with("obs.") => {
                // Usage-site evidence: a mono-typed operator observes
                // its operand. Same Observation payloads the Perl
                // walker emits; the fold is the production one.
                let obs = match cap.strip_prefix("obs.").unwrap() {
                    "numeric" => Some(crate::witnesses::TypeObservation::NumericUse),
                    "string" => Some(crate::witnesses::TypeObservation::StringUse),
                    _ => None,
                };
                if let Some(o) = obs {
                    let span = Span { start: e.start, end: e.end };
                    out.witnesses.push(crate::witnesses::Witness {
                        attachment: crate::witnesses::WitnessAttachment::Variable {
                            name: (pack.shape_name)("ref.var", &e.text),
                            scope: cur_scope,
                        },
                        source: crate::witnesses::WitnessSource::Builder("skeleton-obs".into()),
                        payload: crate::witnesses::WitnessPayload::Observation(o),
                        span,
                    });
                }
            }
            "expr.call" => {
                // Constructor convention: when the pack says this
                // callee instantiates a class, the call expression IS
                // a value of that class — a direct witness, same as a
                // literal. Otherwise no claim (return typing is a
                // later pack layer).
                let callee = events
                    .iter()
                    .find(|x| x.match_id == e.match_id && x.cap == "ref.call")
                    .map(|x| x.text.clone());
                if let Some(class) = callee.and_then(|c| (pack.ctor_class)(&c)) {
                    let span = Span { start: e.start, end: e.end };
                    lit_spans.push((e.start_byte, e.end_byte, span));
                    out.witnesses.push(crate::witnesses::Witness {
                        attachment: crate::witnesses::WitnessAttachment::Expr(span),
                        source: crate::witnesses::WitnessSource::Builder("skeleton-ctor".into()),
                        payload: crate::witnesses::WitnessPayload::InferredType(
                            InferredType::ClassName(class),
                        ),
                        span,
                    });
                }
            }
            _ => {}
        }
    }

    // ---- join flow captures into Variable witnesses ----
    for (mid, (name, scope, at)) in &flow_targets {
        let var = crate::witnesses::WitnessAttachment::Variable {
            name: name.clone(),
            scope: *scope,
        };
        if let Some(annot) = annots.get(mid) {
            if let Some(t) = (pack.annot_type)(annot) {
                out.witnesses.push(crate::witnesses::Witness {
                    attachment: var.clone(),
                    source: crate::witnesses::WitnessSource::Builder("skeleton-annot".into()),
                    payload: crate::witnesses::WitnessPayload::InferredType(t),
                    span: Span { start: *at, end: *at },
                });
            }
        }
        if let Some(src_span) = flow_sources.get(mid) {
            // Narrow onto the outermost literal the rhs wraps, when the
            // rhs node itself carries no witness (paren wrappers).
            let src_bytes = byte_range_of(&events, *mid, "flow.source");
            let target_span = lit_spans
                .iter()
                .filter(|&&(s, en, _)| {
                    src_bytes.is_some_and(|(ss, se)| s >= ss && en <= se)
                })
                .max_by_key(|&&(s, en, _)| en - s)
                .map(|&(_, _, sp)| sp)
                .filter(|sp| sp != src_span)
                .unwrap_or(*src_span);
            out.witnesses.push(crate::witnesses::Witness {
                attachment: var,
                source: crate::witnesses::WitnessSource::Builder("skeleton".into()),
                payload: crate::witnesses::WitnessPayload::Edge(
                    crate::witnesses::WitnessAttachment::Expr(target_span),
                ),
                span: Span { start: *at, end: *at },
            });
        }
    }
    Ok(out)
}

fn byte_range_of(events: &[Event], match_id: usize, cap: &str) -> Option<(usize, usize)> {
    events
        .iter()
        .find(|e| e.match_id == match_id && e.cap == cap)
        .map(|e| (e.start_byte, e.end_byte))
}

#[cfg(test)]
#[path = "query_extract_tests.rs"]
mod tests;
