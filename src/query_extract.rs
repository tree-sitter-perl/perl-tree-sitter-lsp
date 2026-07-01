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
use tree_sitter::{Language, Point, Query, QueryCursor, StreamingIterator, Tree};

/// Compile each pack's skeleton query exactly once and reuse it.
///
/// `Query::new` is expensive (~400ms for the Perl skeleton) and `extract`
/// runs per file, so recompiling every call dominates the workload. A pack's
/// `query_source` is a unique `&'static str`, so its pointer identity keys the
/// compiled query — same pack, same query, one compilation. Leaking the boxed
/// query is bounded (one per language pack) and gives the `&'static Query` the
/// cache needs.
fn cached_query(language: &Language, source: &'static str) -> Result<&'static Query, String> {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};
    static CACHE: OnceLock<Mutex<HashMap<usize, &'static Query>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    let key = source.as_ptr() as usize;
    if let Some(q) = cache.lock().unwrap().get(&key) {
        return Ok(q);
    }
    let query = Query::new(language, source).map_err(|e| format!("query: {e}"))?;
    let leaked: &'static Query = Box::leak(Box::new(query));
    cache.lock().unwrap().insert(key, leaked);
    Ok(leaked)
}

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
    /// Declared return type (`@rettype`), for methods/functions — drives
    /// method-return resolution + chaining through MethodOnClass.
    pub return_type: Option<InferredType>,
    /// Pointer/reference declarator stack, unravelled by `peel_nested` from
    /// a `@nested.target` capture (empty otherwise). Flows to `Symbol.deref_stack`.
    pub deref_stack: Vec<crate::file_analysis::DerefStep>,
}

#[derive(Debug, Clone)]
pub struct SkelRef {
    pub kind: String,
    pub name: String,
    pub start: Point,
    pub end: Point,
    pub scope: crate::file_analysis::ScopeId,
    /// For a member access (`recv.field`) — the receiver subtree's (span,
    /// text), so `into_file_analysis` mints a `RefKind::MethodCall` whose
    /// invocant types query-time via `expr_type_at_span(span)` (text → the
    /// `InvocantName`). `None` for plain calls / var refs.
    pub invocant: Option<(crate::file_analysis::Span, String)>,
    /// The written member operator (`.`/`->`) + its span, mapped from the
    /// `@member.op` token's kind via the pack `op_map`, `Some` only when the
    /// IMMEDIATE receiver is a simple variable. Rides onto the MethodCall ref
    /// so operator-correctness is a ref query, not a separate walk.
    pub member_op: Option<(crate::file_analysis::MemberOp, crate::file_analysis::Span)>,
}

#[derive(Debug, Default)]
pub struct SkeletonAnalysis {
    pub symbols: Vec<SkelSymbol>,
    pub refs: Vec<SkelRef>,
    pub imports: Vec<String>,
    pub scope_count: usize,
    pub scopes: Vec<crate::file_analysis::Scope>,
    pub witnesses: Vec<crate::witnesses::Witness>,
    /// (child class, parent class) inheritance edges — `@parent` captures.
    pub parents: Vec<(String, String)>,
    /// Variable reads (`@expr.read.var`): (name, scope, span). Each resolves
    /// to the nearest visible Variable declaration by lexical scope walk →
    /// local goto-def + hover. Resolution runs in `into_file_analysis`.
    pub var_reads: Vec<(String, crate::file_analysis::ScopeId, crate::file_analysis::Span)>,
    /// `goto LABEL` refs (`@ref.label`): (name, scope, span). Resolve to the
    /// `LABEL:` def (a Variable symbol from `@def.label`) function-wide —
    /// scope-chain walk WITHOUT the declared-before constraint (a forward
    /// goto is valid C).
    pub label_refs: Vec<(String, crate::file_analysis::ScopeId, crate::file_analysis::Span)>,
    /// The pack's receiver param names (Python `self`/`cls`). A Variable so
    /// named is the method receiver, not a class member — its (wrongly
    /// sticky-tagged) class package is cleared in `into_file_analysis`.
    pub receiver_names: Vec<String>,
    /// Value-flow edges minted from `@flow` captures (`source → target`,
    /// extraction). Lowered to type witnesses here; carried onto the FA as the
    /// provenance tier.
    pub flow_edges: Vec<crate::file_analysis::FlowEdge>,
    /// `std::move(x)` sites: (moved var name, move-call span, enclosing scope).
    /// A read of the var after the call and before its next rebind is a
    /// use-after-move bug (`FileAnalysis::use_after_move_reads`).
    pub moved_from: Vec<(String, crate::file_analysis::Span, crate::file_analysis::ScopeId)>,
}

impl SkeletonAnalysis {
    /// Assemble a REAL `FileAnalysis` — production model, production
    /// indices, production reducer registry behind every query — from
    /// nothing but capture events. The existence proof that the engine
    /// is language-agnostic above this seam.
    pub fn into_file_analysis(mut self) -> crate::file_analysis::FileAnalysis {
        use crate::file_analysis::{
            FileAnalysis, FileAnalysisParts, SymKind, Symbol, SymbolDetail, SymbolId,
        };
        // A NAMED typedef `typedef struct N {...} N;` matches both the
        // struct_specifier and the type_definition → two `class N`. C
        // can't have two types of one name, so keep the first.
        {
            let mut seen = std::collections::HashSet::new();
            self.symbols
                .retain(|s| s.kind != "class" || seen.insert(s.name.clone()));
        }
        let mut bag = crate::witnesses::WitnessBag::default();
        for w in self.witnesses {
            bag.push(w);
        }
        let mut symbols: Vec<Symbol> = self
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
                attributes: Vec::new(),
                deref_stack: s.deref_stack.clone(),
            })
            .collect();
        // Tag a typedef-struct's members with its name. `typedef struct
        // {...} T;` names the type AFTER its body, so @context.class can't
        // reach the members (already walked). For each class, members
        // directly in its body scope that are still UNTAGGED inherit the
        // class name. Idempotent for normal classes (members already tagged
        // via @context.class are skipped).
        let class_bodies: Vec<(crate::file_analysis::ScopeId, String)> = symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Class))
            .filter_map(|c| {
                let cs = c.span;
                self.scopes
                    .iter()
                    .find(|s| {
                        s.span != cs
                            && (s.span.start.row, s.span.start.column)
                                >= (cs.start.row, cs.start.column)
                            && (s.span.end.row, s.span.end.column) <= (cs.end.row, cs.end.column)
                    })
                    .map(|s| (s.id, c.name.clone()))
            })
            .collect();
        for (body, cname) in class_bodies {
            for s in &mut symbols {
                if s.package.is_none() && s.scope == body {
                    s.package = Some(cname.clone());
                }
            }
        }

        // A function whose owning package is a CLASS is a method. Covers
        // template members, which tree-sitter parses as a plain
        // `declaration` (identifier, not field_identifier) so they classify
        // as Sub — but a sub owned by a class is a method, by definition.
        let class_names: std::collections::HashSet<String> = symbols
            .iter()
            .filter(|s| matches!(s.kind, SymKind::Class))
            .map(|s| s.name.clone())
            .collect();
        for s in &mut symbols {
            if matches!(s.kind, SymKind::Sub)
                && s.package.as_deref().is_some_and(|p| class_names.contains(p))
            {
                s.kind = SymKind::Method;
            }
        }
        // Writeback-lite: route method returns through MethodOnClass so
        // `box.getInner().` chains, inherited returns, and cross-file all
        // resolve via the SAME chase Perl uses — no bypass. Declared
        // returns need no fixpoint (the type is in the syntax); only the
        // edge-minting half of the fold's writeback is needed here.
        {
            use crate::witnesses::{Witness, WitnessAttachment as WA, WitnessPayload as WP, WitnessSource};
            let mk = |att: WA, pay: WP, span: Span| Witness {
                attachment: att,
                source: WitnessSource::Builder("cpp_method_return".into()),
                payload: pay,
                span,
            };
            for (i, sym) in symbols.iter().enumerate() {
                if !matches!(sym.kind, SymKind::Method | SymKind::Sub) {
                    continue;
                }
                if let Some(ret) = &self.symbols[i].return_type {
                    // A class-shaped declared return edges into the alias graph
                    // (it may be a typedef) instead of committing the spelling;
                    // primitives are leaves. `TypeName` resolves the typedef or
                    // falls back to the same `ClassName`, so struct returns are
                    // unchanged and aliased returns now chase. (edges-not-values)
                    let pay = match ret {
                        InferredType::ClassName(cn) => WP::Edge(WA::TypeName(cn.clone())),
                        other => WP::InferredType(other.clone()),
                    };
                    bag.push(mk(WA::Symbol(sym.id), pay, sym.span));
                }
                if matches!(sym.kind, SymKind::Method) {
                    if let Some(class) = &sym.package {
                        bag.push(mk(
                            WA::MethodOnClass { class: class.clone(), name: sym.name.clone() },
                            WP::Edge(WA::Symbol(sym.id)),
                            sym.span,
                        ));
                    }
                }
            }
            // Inheritance edges: MethodOnClass{child,m} → Edge(parent,m), so
            // the registry walks the MRO for an inherited method's return.
            for (child, parent) in &self.parents {
                for sym in &symbols {
                    if matches!(sym.kind, SymKind::Method)
                        && sym.package.as_deref() == Some(parent.as_str())
                    {
                        bag.push(mk(
                            WA::MethodOnClass { class: child.clone(), name: sym.name.clone() },
                            WP::Edge(WA::MethodOnClass { class: parent.clone(), name: sym.name.clone() }),
                            sym.span,
                        ));
                    }
                }
            }
        }
        // ---- Local/param vars: a variable READ resolves to the nearest
        // visible Variable declaration by lexical scope walk → local
        // goto-def + hover. Declarations are already Variable symbols
        // (`@def.local`/`@def.var`); fields are excluded naturally (their
        // class scope isn't on a local read's scope chain).
        let mut defs_by_name: std::collections::HashMap<
            String,
            Vec<(crate::file_analysis::ScopeId, Span, SymbolId)>,
        > = std::collections::HashMap::new();
        for s in &symbols {
            if matches!(s.kind, SymKind::Variable) {
                defs_by_name
                    .entry(s.name.clone())
                    .or_default()
                    .push((s.scope, s.selection_span, s.id));
            }
        }
        let scope_parent: std::collections::HashMap<
            crate::file_analysis::ScopeId,
            Option<crate::file_analysis::ScopeId>,
        > = self.scopes.iter().map(|s| (s.id, s.parent)).collect();
        let mut local_refs: Vec<crate::file_analysis::Ref> = Vec::new();
        for (name, read_scope, read_span) in &self.var_reads {
            let Some(cands) = defs_by_name.get(name) else { continue };
            let rp = (read_span.start.row, read_span.start.column);
            let mut cur = Some(*read_scope);
            let mut resolved: Option<SymbolId> = None;
            while let Some(sc) = cur {
                // nearest decl of this name in THIS scope level, declared at
                // or before the read (latest-wins for redeclaration).
                let mut best: Option<((usize, usize), SymbolId)> = None;
                for (dscope, dspan, did) in cands {
                    let dp = (dspan.start.row, dspan.start.column);
                    if *dscope == sc && dp <= rp && best.is_none_or(|(bp, _)| dp > bp) {
                        best = Some((dp, *did));
                    }
                }
                if let Some((_, did)) = best {
                    resolved = Some(did);
                    break;
                }
                cur = scope_parent.get(&sc).copied().flatten();
            }
            if let Some(did) = resolved {
                local_refs.push(crate::file_analysis::Ref {
                    kind: crate::file_analysis::RefKind::Variable,
                    span: *read_span,
                    scope: *read_scope,
                    target_name: name.clone(),
                    access: crate::file_analysis::AccessKind::Read,
                    resolves_to: Some(did),
                    resolved_method_target: None,
                    folded_from: None,
                });
            }
        }
        // `goto LABEL` → the `LABEL:` def, function-wide: first matching
        // Variable on the scope chain, NO declared-before constraint (a
        // forward goto is valid).
        for (name, ref_scope, ref_span) in &self.label_refs {
            let Some(cands) = defs_by_name.get(name) else { continue };
            let mut cur = Some(*ref_scope);
            let mut resolved: Option<SymbolId> = None;
            while let Some(sc) = cur {
                if let Some((_, _, did)) = cands.iter().find(|(dscope, _, _)| *dscope == sc) {
                    resolved = Some(*did);
                    break;
                }
                cur = scope_parent.get(&sc).copied().flatten();
            }
            if let Some(did) = resolved {
                local_refs.push(crate::file_analysis::Ref {
                    kind: crate::file_analysis::RefKind::Variable,
                    span: *ref_span,
                    scope: *ref_scope,
                    target_name: name.clone(),
                    access: crate::file_analysis::AccessKind::Read,
                    resolves_to: Some(did),
                    resolved_method_target: None,
                    folded_from: None,
                });
            }
        }

        let mut refs: Vec<crate::file_analysis::Ref> = self
            .refs
            .iter()
            .filter_map(|r| {
                use crate::file_analysis::RefKind;
                let kind = match r.kind.as_str() {
                    "call" => RefKind::FunctionCall { resolved_package: None },
                    // `recv.field` / `recv->field`: the SAME MethodCall ref
                    // core resolves for Perl `$obj->m`. The invocant types
                    // query-time via `expr_type_at_span(invocant_span)`;
                    // `find_definition`/`refs_to`/hover all flow from it.
                    "member" => {
                        let (inv_span, inv_text) = r.invocant.clone()?;
                        RefKind::MethodCall {
                            invocant: crate::conventions::Invocant::assume_canonical(inv_text),
                            invocant_span: Some(inv_span),
                            method_name_span: Span { start: r.start, end: r.end },
                            member_op: r.member_op,
                        }
                    }
                    _ => return None,
                };
                Some(crate::file_analysis::Ref {
                    kind,
                    span: Span { start: r.start, end: r.end },
                    scope: r.scope,
                    target_name: r.name.clone(),
                    access: crate::file_analysis::AccessKind::Read,
                    resolves_to: None,
                    resolved_method_target: None,
                    folded_from: None,
                })
            })
            .collect();
        refs.extend(local_refs);
        let mut package_parents: std::collections::HashMap<String, Vec<String>> =
            std::collections::HashMap::new();
        for (child, parent) in &self.parents {
            package_parents.entry(child.clone()).or_default().push(parent.clone());
        }
        let mut fa = FileAnalysis::new(FileAnalysisParts {
            scopes: self.scopes,
            symbols,
            refs,
            witnesses: bag,
            package_parents,
            flow_edges: std::mem::take(&mut self.flow_edges),
            moved_from: std::mem::take(&mut self.moved_from),
            ..Default::default()
        });
        // Pack-declared receiver names ride the FA so core's member /
        // outline filters can exclude them generically (lang semantics in
        // the pack, generic logic in core).
        fa.receiver_names = std::mem::take(&mut self.receiver_names);
        // Seal base_*_count so a later enrich pass (the CLI/--batch path
        // runs it unconditionally) truncates to the FULL analysis, not to
        // zero — otherwise enrichment wipes every pack-language symbol.
        fa.finalize_post_walk();
        fa
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
    /// Does a call to `callee` construct a KEYED value whose named
    /// arguments are `$`-style accessible keys? (R: list / data.frame.)
    pub shape_ctor: fn(callee: &str) -> bool,
    /// Languages where imports are CALLS, not statements (R's
    /// library()/source()): map (callee, argument) → imported module.
    pub import_call: fn(callee: &str, arg: &str) -> Option<String>,
    /// Command-dispatched languages (CMake): what a command DOES with
    /// its positional arguments. The @cmd/@cmd.arg captures deliver
    /// (name, ordered args); this predicate classifies.
    pub cmd_effects: fn(cmd: &str) -> Vec<CmdEffect>,
    /// Guard narrowing: given the guard token (`@narrow.guard` — a
    /// function/operator like `isinstance`, `has_value`; `None` for the
    /// token-less `if (opt)` truthiness form) and the type text, the
    /// refined type that holds inside the guarded block, or `None` if this
    /// guard doesn't narrow. The type text is the `@narrow.type` capture
    /// when the guard names one (`dynamic_cast<Derived*>`), else the
    /// subject's DECLARED type (the optional-engagement form reads
    /// `std::optional<T>` off the declaration and peels `T`). The pack owns
    /// "which guard means which refinement" (rule #10); core just scopes
    /// the witness to the block.
    pub narrow_guard: fn(guard: Option<&str>, type_text: &str) -> Option<InferredType>,
    /// Does calling `method` on a variable REBIND it — putting a moved-from
    /// object back into a known state (`clear`/`reset`/`assign`/…)? Used to end
    /// a moved-from region (and any narrowing) at the reset call, so a use after
    /// it is clean. Pack-owned language vocab (like `op_map` / `ctor_class`):
    /// core asks the value, never enumerates names itself.
    pub rebind_method: fn(method: &str) -> bool,
    /// Completion trigger characters for the LSP
    /// `completionProvider.triggerCharacters` slot — the client auto-fires
    /// completion (and reports the char in `CompletionContext`) when one is
    /// typed. C++ `. > :` cover `.`/`->`/`::`; the member path keys off them.
    pub trigger_chars: &'static [&'static str],
    /// The language's method-RECEIVER parameter names (Python `self`/`cls`,
    /// C++ `this`). A receiver param is lexically inside the class body, so
    /// the sticky class context tags it — but it is NOT a member. Extraction
    /// clears its package so it reads as a plain local. Lang-specific
    /// semantics → the pack owns it (NOT core `conventions.rs`, which is
    /// Perl's `$self`/`$class`).
    pub receiver_names: &'static [&'static str],
    /// The pointer/reference DECLARATOR peel: a `@nested.target` chain
    /// flattened to its leaf + per-level deref stack — `Box**`, `char****`,
    /// `Box* const&`. THE recursion S-queries can't express (unbounded depth);
    /// the pack declares the grammar, the generic `peel` walks it.
    pub nested_peel: PeelSpec,
    /// The member-access RECEIVER peel: transparent expression wrappers
    /// (`(*p)`, `(&o)`, `(p)` → `p`) dropped so the invocant types via the
    /// inner. The SAME `peel`, no stack, any leaf.
    pub recv_peel: PeelSpec,
    /// Member-access node kinds (`receiver OP member`) — extraction records
    /// each site (simple-variable receiver, operator token span, `->` vs
    /// `.`) for the operator-DX consumer (`p.` on a `Box*` should be `->`).
    /// The member operator's grammar token KIND → the `MemberOp` it means
    /// (`"->"`→Arrow, `"."`→Dot). The `operator:` field of a member access is
    /// captured as `@member.op`; the engine maps its `kind()` through this
    /// table. An OPEN set: unmapped kinds (`.*`) get no op-DX, never a guess.
    /// Empty = no member-operator DX (Perl, single-operator packs).
    pub op_map: &'static [(&'static str, crate::file_analysis::MemberOp)],
    /// Simple-variable node kinds (`identifier`). op-DX fires ONLY when the
    /// IMMEDIATE member-access receiver is one — the receiver whose
    /// `deref_stack` resolves by name to decide the expected operator. Also the
    /// cursor-completion "is this receiver a bare variable" test.
    pub simple_var_kinds: &'static [&'static str],
    /// Member-access node kinds (`field_expression` / `attribute`): a `recv.m`
    /// the cursor-completion path climbs to + types the receiver of. Empty =
    /// no member-access completion (Perl uses `cursor_context`).
    pub member_kinds: &'static [&'static str],
    /// Node kinds the sentinel must NOT splice into (string/char/comment).
    pub skip_kinds: &'static [&'static str],
    /// Call-expression node kinds (`call_expression`/`call`) — a chained
    /// receiver `f().attr` types through the call's inner member.
    pub call_kinds: &'static [&'static str],
}

/// A declarative peel: descend a wrapper chain tree-sitter's fixed-depth
/// S-expression queries cannot express, to the leaf, optionally accumulating a
/// per-level deref stack. ONE combinator the pack parameterizes — `nested_peel`
/// (declarators, stack, leaf→def) and `recv_peel` (expr wrappers, no stack, any
/// leaf) are both instances of it. Empty `wrappers` = the capture is absent.
#[derive(Clone, Copy)]
pub struct PeelSpec {
    /// Wrapper node kinds → the `DerefKind` each contributes (only consulted
    /// when `record_stack`; a placeholder otherwise).
    pub wrappers: &'static [(&'static str, crate::file_analysis::DerefKind)],
    /// Per-level annotation node kinds (cv-qualifiers) collected onto a step.
    pub annot_kinds: &'static [&'static str],
    /// Leaf node kind → the `def.*` capture the synthetic leaf event mints
    /// (`identifier`→`def.local`, `field_identifier`→`def.var`). EMPTY = accept
    /// ANY leaf and mint no def (the receiver-peel case — the leaf is an
    /// invocant, not a declaration).
    pub leaf_to_def: &'static [(&'static str, &'static str)],
    /// Accumulate the per-level `DerefStep` stack (pointer depth) vs descend only.
    pub record_stack: bool,
}

/// One effect of a command-dispatched statement.
#[derive(Debug, Clone, Copy)]
pub enum CmdEffect {
    /// Argument `name_arg` declares an entity of `kind` ("var",
    /// "sub", ...).
    Def { kind: &'static str, name_arg: usize },
    /// Arguments from `from` onward are name references (all-caps
    /// keyword arguments like PRIVATE/STATIC are skipped — CMake's
    /// keyword convention; a finer filter is a later predicate).
    RefArgsFrom { from: usize },
    /// Argument `arg` names an imported module (joins import_call's
    /// role for command languages).
    Import { arg: usize },
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
        shape_ctor: |_| false,
        import_call: |_, _| None,
        cmd_effects: |_| vec![],
        narrow_guard: |_, _| None,
        rebind_method: |_| false,
        trigger_chars: &["$", "@", "%", ">", ":", "{"],
        receiver_names: &[],
        nested_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: true },
        recv_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: false },
        op_map: &[],
        simple_var_kinds: &[],
        member_kinds: &[],
        skip_kinds: &[],
        call_kinds: &[],
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
        shape_ctor: |_| false,
        import_call: |_, _| None,
        cmd_effects: |_| vec![],
        // `isinstance(x, Foo)` narrows x to Foo inside the guard.
        narrow_guard: |guard, ty| (guard == Some("isinstance")).then(|| InferredType::ClassName(ty.to_string())),
        rebind_method: |_| false,
        trigger_chars: &["."],
        receiver_names: &["self", "cls"],
        nested_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: true },
        recv_peel: PeelSpec {
            wrappers: &[("parenthesized_expression", crate::file_analysis::DerefKind::Pointer)],
            annot_kinds: &[],
            leaf_to_def: &[],
            record_stack: false,
        },
        // Python has one member operator (`.`), so no op-DX (op_map empty).
        op_map: &[],
        simple_var_kinds: &["identifier"],
        member_kinds: &["attribute"],
        skip_kinds: &["string", "string_content", "comment", "concatenated_string"],
        call_kinds: &["call"],
    }
}

pub fn r_pack() -> LangPack {
    LangPack {
        query_source: include_str!("../queries/r/skeleton.scm"),
        shape_name: |_, raw| raw.to_string(),
        default_name: |_| None,
        annot_type: |_| None,
        // No reliable lexical ctor convention in R (S4/R5 exist but
        // rare); class typing arrives via shapes and S3 later.
        ctor_class: |_| None,
        // source("util.R") hands us the path verbatim; library(pkg)
        // resolves into the installed-library tree (a real install
        // would consult .libPaths() — out of spike scope).
        module_paths: |m| vec![m.to_string()],
        shape_ctor: |callee| matches!(callee, "list" | "data.frame" | "tibble"),
        import_call: |callee, arg| match callee {
            "library" | "require" | "source" => Some(arg.to_string()),
            _ => None,
        },
        cmd_effects: |_| vec![],
        narrow_guard: |_, _| None,
        rebind_method: |_| false,
        trigger_chars: &["$", "@", ":"],
        receiver_names: &[],
        nested_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: true },
        recv_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: false },
        op_map: &[],
        simple_var_kinds: &[],
        member_kinds: &[],
        skip_kinds: &[],
        call_kinds: &[],
    }
}

pub fn cmake_pack() -> LangPack {
    LangPack {
        query_source: include_str!("../queries/cmake/skeleton.scm"),
        shape_name: |_, raw| raw.to_string(),
        default_name: |_| None,
        annot_type: |_| None,
        ctor_class: |_| None,
        // include(util.cmake) is a literal path; add_subdirectory(src)
        // means src/CMakeLists.txt. The whole resolution strategy.
        module_paths: |m| {
            if m.ends_with(".cmake") {
                vec![m.to_string()]
            } else {
                vec![format!("{m}/CMakeLists.txt"), format!("{m}.cmake")]
            }
        },
        shape_ctor: |_| false,
        import_call: |_, _| None,
        cmd_effects: |cmd| match cmd.to_ascii_lowercase().as_str() {
            "set" | "option" => vec![CmdEffect::Def { kind: "var", name_arg: 0 }],
            "add_library" | "add_executable" | "add_custom_target" => {
                // Targets. SymKind::Target is the real future; "sub"
                // rides the full rename/refs machinery today.
                vec![CmdEffect::Def { kind: "sub", name_arg: 0 }]
            }
            "target_link_libraries" | "target_include_directories"
            | "target_compile_definitions" | "target_sources" => vec![
                CmdEffect::RefArgsFrom { from: 0 },
            ],
            "include" | "add_subdirectory" => vec![CmdEffect::Import { arg: 0 }],
            _ => vec![],
        },
        narrow_guard: |_, _| None,
        rebind_method: |_| false,
        trigger_chars: &["{", "("],
        receiver_names: &[],
        nested_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: true },
        recv_peel: PeelSpec { wrappers: &[], annot_kinds: &[], leaf_to_def: &[], record_stack: false },
        op_map: &[],
        simple_var_kinds: &[],
        member_kinds: &[],
        skip_kinds: &[],
        call_kinds: &[],
    }
}

pub fn cpp_pack() -> LangPack {
    LangPack {
        query_source: include_str!("../queries/cpp/skeleton.scm"),
        shape_name: |_, raw| raw.to_string(),
        default_name: |_| None,
        // C++ declared types ARE the witness source. Primitives → the
        // value lattice; `auto`/`void` defer (None → edge carries);
        // anything else identifier-shaped is a class instance.
        annot_type: |text| {
            use InferredType::*;
            match text.trim() {
                "int" | "long" | "short" | "unsigned" | "size_t" | "int32_t" | "int64_t"
                | "uint32_t" | "uint64_t" | "double" | "float" | "bool" | "char" => Some(Numeric),
                "std::string" | "string" | "std::string_view" => Some(String),
                "auto" | "void" => None,
                t => {
                    // Elaborated type specifier `struct op` / `union u` /
                    // `enum e` — the dominant C spelling (`struct op* o`).
                    // The tag names the type; strip the keyword so it resolves
                    // the same as the bare/typedef'd name.
                    let tag = t
                        .strip_prefix("struct ")
                        .or_else(|| t.strip_prefix("union "))
                        .or_else(|| t.strip_prefix("enum "))
                        .unwrap_or(t)
                        .trim();
                    let typeish = !tag.is_empty()
                        && !tag.contains(' ')
                        && tag.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_');
                    // Strip the namespace qualifier — classes/members are
                    // keyed by the unqualified name (@context.class), so
                    // `geo::Circle` must type as `Circle` to resolve.
                    typeish.then(|| ClassName(tag.rsplit("::").next().unwrap_or(tag).to_string()))
                }
            }
        },
        // explicit constructor call `Foo(...)`: capitalized (or
        // namespaced-capitalized) callee names a class — the C++ idiom,
        // same role conventions.rs plays for Perl.
        ctor_class: |callee| {
            let last = callee.rsplit("::").next().unwrap_or(callee);
            last.chars()
                .next()
                .is_some_and(|c| c.is_uppercase())
                .then(|| callee.to_string())
        },
        // #include "a/b.h" / <vector>: strip the delimiters; a quoted
        // path is workspace-relative verbatim, a system header resolves
        // through include dirs (library_roots, later). Tier 1: identity.
        module_paths: |m| {
            let p = m.trim_matches(|c: char| c == '"' || c == '<' || c == '>');
            vec![p.to_string()]
        },
        shape_ctor: |_| false,
        import_call: |_, _| None,
        cmd_effects: |_| vec![],
        // Two narrowings, both keyed on what the value IS, not a name allowlist:
        //   `if (dynamic_cast<Derived*>(b))` — b is a Derived inside (ty is the
        //     template arg; pointer-ness dropped for navigation, like locals).
        //   `if (opt)` / `if (opt.has_value())` — an engaged std::optional<T>
        //     holds a T inside (ty is opt's DECLARED type; peel the inner T).
        //     The bare form carries no guard token; `.has_value()` gates the
        //     method so `opt.value_or(x)` (not an engagement test) won't narrow.
        narrow_guard: |guard, ty| {
            let class = match guard {
                Some("dynamic_cast") => ty.to_string(),
                None | Some("has_value") => optional_inner(ty)?,
                _ => return None,
            };
            Some(InferredType::ClassName(class))
        },
        // Rebinding methods: a moved-from object is put back into a known state
        // by these std container/optional/smart-ptr resets, so a use after one
        // is NOT a use-after-move. (An ordinary `x.use()` is not here, so the
        // canonical bug still flags.)
        rebind_method: |m| {
            matches!(m, "clear" | "reset" | "assign" | "emplace" | "swap")
        },
        trigger_chars: &[".", ">", ":"],
        receiver_names: &["this"],
        nested_peel: PeelSpec {
            wrappers: &[
                ("pointer_declarator", crate::file_analysis::DerefKind::Pointer),
                ("reference_declarator", crate::file_analysis::DerefKind::Reference),
            ],
            annot_kinds: &["type_qualifier"],
            leaf_to_def: &[("identifier", "def.local"), ("field_identifier", "def.var")],
            record_stack: true,
        },
        // DerefKind placeholder — record_stack false, so it's never read.
        recv_peel: PeelSpec {
            wrappers: &[
                ("parenthesized_expression", crate::file_analysis::DerefKind::Pointer),
                ("pointer_expression", crate::file_analysis::DerefKind::Pointer),
            ],
            annot_kinds: &[],
            leaf_to_def: &[],
            record_stack: false,
        },
        op_map: &[
            ("->", crate::file_analysis::MemberOp::Arrow),
            (".", crate::file_analysis::MemberOp::Dot),
        ],
        simple_var_kinds: &["identifier"],
        member_kinds: &["field_expression"],
        skip_kinds: &["string_literal", "char_literal", "raw_string_literal", "comment"],
        call_kinds: &["call_expression"],
    }
}

/// Peel `T` out of a `std::optional<T>` declared-type text, unqualified
/// (matching how `annot_type` keys classes by their last `::` segment). `None`
/// when the text isn't an optional — the type-side gate that keeps the
/// token-less `if (opt)` narrowing from firing on non-optional subjects.
fn optional_inner(ty: &str) -> Option<String> {
    let inner = ty
        .trim()
        .strip_prefix("std::optional<")
        .or_else(|| ty.trim().strip_prefix("optional<"))?
        .strip_suffix('>')?
        .trim();
    let leaf = inner.rsplit("::").next().unwrap_or(inner).trim();
    (!leaf.is_empty()
        && !leaf.contains(' ')
        && leaf.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_'))
    .then(|| leaf.to_string())
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

/// Flatten a wrapper chain — the recursion tree-sitter's fixed-depth queries
/// cannot express — to its leaf, recording a `DerefStep` per level when
/// `spec.record_stack`. A non-empty `leaf_to_def` REQUIRES the leaf to match
/// (returns its def capture); an empty one accepts ANY leaf and mints nothing
/// (the receiver peel — the leaf is an invocant). Outermost level first
/// (left-to-right display order, `Box*&` → `[Pointer, Reference]`). Depth-
/// capped. The ONE peel: `nested_peel` and `recv_peel` are both this.
fn peel<'a>(
    mut node: tree_sitter::Node<'a>,
    spec: &PeelSpec,
    src: &[u8],
) -> Option<(tree_sitter::Node<'a>, Vec<crate::file_analysis::DerefStep>, Option<&'static str>)> {
    use crate::file_analysis::DerefStep;
    let is_leaf = |k: &str| spec.leaf_to_def.iter().find(|(lk, _)| *lk == k);
    let mut stack = Vec::new();
    for _ in 0..32 {
        if let Some((_, dk)) = spec.wrappers.iter().find(|(k, _)| *k == node.kind()) {
            let mut annotations = Vec::new();
            let mut inner = None;
            let mut cur = node.walk();
            for ch in node.children(&mut cur) {
                if spec.annot_kinds.contains(&ch.kind()) {
                    if let Ok(t) = ch.utf8_text(src) {
                        annotations.push(t.to_string());
                    }
                } else if inner.is_none()
                    && (spec.wrappers.iter().any(|(k, _)| *k == ch.kind())
                        || is_leaf(ch.kind()).is_some()
                        || (spec.leaf_to_def.is_empty() && ch.is_named()))
                {
                    inner = Some(ch);
                }
            }
            if spec.record_stack {
                stack.push(DerefStep { kind: *dk, annotations });
            }
            node = inner?;
        } else if spec.leaf_to_def.is_empty() {
            // receiver peel: the leaf is an invocant of any shape, no def minted.
            return Some((node, stack, None));
        } else if let Some((_, def_cap)) = is_leaf(node.kind()) {
            // `identifier`→`def.local` (param/local), `field_identifier`→
            // `def.var` (a class member), so a pointer field outlines as a member.
            return Some((node, stack, Some(def_cap)));
        } else {
            return None;
        }
    }
    None
}

pub fn extract(tree: &Tree, source: &[u8], pack: &LangPack) -> Result<SkeletonAnalysis, String> {
    let language = tree.language();
    let query = cached_query(&language, pack.query_source)?;
    let cap_names: Vec<String> = query
        .capture_names()
        .iter()
        .map(|s| s.to_string())
        .collect();

    // ---- flatten matches into ordered events ----
    let mut events: Vec<Event> = Vec::new();
    // match_id → the pointer/reference declarator stack a `@nested.target`
    // capture unravelled to. Read by the `def.*` handler to stamp the symbol.
    let mut nested_stacks: std::collections::HashMap<usize, Vec<crate::file_analysis::DerefStep>> =
        std::collections::HashMap::new();
    // match_id → was the IMMEDIATE member-access receiver a simple variable?
    // Recorded at construction (the un-peeled node); gates op-DX at the mint.
    let mut member_simple: std::collections::HashMap<usize, bool> = std::collections::HashMap::new();
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(query, tree.root_node(), source);
    let mut match_counter = 0usize;
    while let Some(m) = matches.next() {
        match_counter += 1;
        for c in m.captures {
            let node = c.node;
            let cap = cap_names[c.index as usize].as_str();
            // `@nested.target`: a pointer/reference declarator CHAIN of any
            // depth. Peel it (where the node is live) to the leaf identifier
            // + the deref stack, then emit the leaf as if the query had
            // captured it directly — downstream join/symbol/witness paths are
            // unchanged, and arbitrary nesting works without enumerating it.
            if cap == "nested.target" {
                if let Some((leaf, stack, Some(def_cap))) = peel(node, &pack.nested_peel, source) {
                    nested_stacks.insert(match_counter, stack);
                    let ltext = leaf.utf8_text(source).unwrap_or("").to_string();
                    for syn in ["flow.target", def_cap] {
                        events.push(Event {
                            start_byte: leaf.start_byte(),
                            end_byte: leaf.end_byte(),
                            start: leaf.start_position(),
                            end: leaf.end_position(),
                            cap: syn.to_string(),
                            text: ltext.clone(),
                            match_id: match_counter,
                        });
                    }
                }
                continue;
            }
            // `@member.recv`: a member access receiver. Peel transparent
            // wrappers (`(*p)`, `(&o)`, `(p)`) to the typed inner where the
            // node is live, so the minted MethodCall ref's invocant_span lands
            // on the inner expression `expr_type_at_span` already types.
            if cap == "member.recv" {
                // op-DX applies only to a bare-variable immediate receiver
                // (its deref_stack resolves by name); a wrapper/chain doesn't.
                member_simple.insert(match_counter, pack.simple_var_kinds.contains(&node.kind()));
                let inner = peel(node, &pack.recv_peel, source)
                    .map(|(leaf, _, _)| leaf)
                    .unwrap_or(node);
                events.push(Event {
                    start_byte: inner.start_byte(),
                    end_byte: inner.end_byte(),
                    start: inner.start_position(),
                    end: inner.end_position(),
                    cap: cap.to_string(),
                    text: inner.utf8_text(source).unwrap_or("").to_string(),
                    match_id: match_counter,
                });
                continue;
            }
            events.push(Event {
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
                start: node.start_position(),
                end: node.end_position(),
                cap: cap.to_string(),
                text: node.utf8_text(source).unwrap_or("").to_string(),
                match_id: match_counter,
            });
        }
    }
    // Source order; outermost first on ties so scopes push before their
    // contents. A `@scope` on the SAME node as a `@def` (a function_definition
    // carries its own body scope) must open AFTER the def is recorded, so the
    // symbol attributes to its ENCLOSING scope, not its own body — hence scope
    // sorts last among identical-span ties.
    events.sort_by(|a, b| {
        a.start_byte
            .cmp(&b.start_byte)
            .then(b.end_byte.cmp(&a.end_byte))
            .then((a.cap == "scope").cmp(&(b.cap == "scope")))
    });

    // ---- join def name-captures to their def event ----
    use std::collections::HashMap;
    let mut names_by_match: HashMap<(usize, String), (String, Point, Point)> = HashMap::new();
    // `@qualifier` (a `Class::` on an out-of-line def) and `@rettype` (a
    // method's declared return type) — pre-collected like names because the
    // `@def` event fires before these inner captures.
    let mut qualifier_by_match: HashMap<usize, String> = HashMap::new();
    let mut rettype_by_match: HashMap<usize, String> = HashMap::new();
    // `@member.recv` → the receiver span of a `recv.field` access, joined to
    // its `@ref.member` by match_id so the minted MethodCall ref carries it.
    let mut member_recv: HashMap<usize, (crate::file_analysis::Span, String)> = HashMap::new();
    // `@member.op` → the written operator mapped through `pack.op_map` + its
    // span; joined to `@ref.member` so op-DX rides the minted ref.
    let mut member_op_raw: HashMap<usize, (crate::file_analysis::MemberOp, crate::file_analysis::Span)> =
        HashMap::new();
    for e in &events {
        if let Some(prefix) = e.cap.strip_suffix(".name") {
            names_by_match
                .insert((e.match_id, prefix.to_string()), (e.text.clone(), e.start, e.end));
        }
        if e.cap == "qualifier" {
            qualifier_by_match.insert(e.match_id, e.text.clone());
        }
        if e.cap == "rettype" {
            rettype_by_match.insert(e.match_id, e.text.clone());
        }
    }
    // (var name, declaring scope) → its declared-type text, joined per
    // declaration match. Feeds the token-less optional-engagement narrowing
    // (`if (opt)`): the guard names no type, so the refinement reads the
    // subject's declared `std::optional<T>`. Keyed by SCOPE (not bare name), so
    // two functions each declaring an `opt` of a DIFFERENT `optional<T>` peel
    // the right inner type — resolved by the guard's scope chain at consumption.
    // Populated inside the main loop (a decl's scope is only known there); the
    // type-annot half is pre-collected here since it precedes the declarator.
    let mut annot_by_match: HashMap<usize, String> = HashMap::new();
    // `@alias.name` (a typedef/using type alias) + `@alias.of` (its
    // underlying type text), joined per match → `TypeName` alias witnesses.
    let mut alias_name_by_match: HashMap<usize, String> = HashMap::new();
    let mut alias_of_by_match: HashMap<usize, String> = HashMap::new();
    // Object-like `#define X body` alias halves — kept SEPARATE from the
    // typedef alias so the emission can gate on a type-shaped body (a
    // macro-heavy header has thousands of value macros we must NOT mint
    // TypeName witnesses for).
    let mut macro_alias_name_by_match: HashMap<usize, String> = HashMap::new();
    let mut macro_alias_of_by_match: HashMap<usize, String> = HashMap::new();
    for e in &events {
        if e.cap == "type.annot" {
            annot_by_match.insert(e.match_id, e.text.clone());
        }
        if e.cap == "alias.name" {
            alias_name_by_match.insert(e.match_id, e.text.clone());
        }
        if e.cap == "alias.of" {
            alias_of_by_match.insert(e.match_id, e.text.clone());
        }
        if e.cap == "macro.alias.name" {
            macro_alias_name_by_match.insert(e.match_id, e.text.clone());
        }
        if e.cap == "macro.alias.of" {
            macro_alias_of_by_match.insert(e.match_id, e.text.clone());
        }
    }
    let mut annot_text_by_var: HashMap<(String, crate::file_analysis::ScopeId), String> =
        HashMap::new();

    // ---- the state machine: scope stack + sticky contexts ----
    let mut out = SkeletonAnalysis::default();
    out.receiver_names = pack.receiver_names.iter().map(|s| s.to_string()).collect();
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
    // Rebind shapes with no inflowing value (loop vars: `for x in …`,
    // `for (auto x : …)`) — they mint a `Rebind` FlowEdge so the narrowing
    // cutoff sees them, exactly like Perl's `foreach` var.
    let mut flow_rebinds: Vec<(String, ScopeId, Point)> = Vec::new();
    let mut annots: HashMap<usize, String> = HashMap::new();
    // keyed-shape collection: ctor + keys grouped per @expr.shape span
    let mut shape_spans: Vec<(usize, usize, Span)> = Vec::new();
    let mut shape_ctors: HashMap<(usize, usize), String> = HashMap::new();
    let mut shape_keys: Vec<(usize, usize, String)> = Vec::new();
    // command-dispatch collection: per match, the command identifier
    // and its ordered arguments
    let mut cmd_names: std::collections::BTreeMap<usize, (String, Span, crate::file_analysis::ScopeId)> =
        Default::default();
    let mut cmd_args: std::collections::BTreeMap<usize, Vec<(String, Span)>> = Default::default();
    // import-call halves, joined per match (BTreeMap: match ids are
    // source-ordered, so imports come out deterministic)
    let mut import_fns: std::collections::BTreeMap<usize, String> = Default::default();
    let mut import_args: std::collections::BTreeMap<usize, String> = Default::default();
    // expr-literal spans, for narrowing an Edge target onto the actual
    // literal when the rhs node wraps it.
    let mut lit_spans: Vec<(usize, usize, Span)> = Vec::new();
    // guard narrowing: match_id → (var, narrowed-type-text). A guard
    // (`isinstance(x, Foo)`, `ref $x eq 'Foo'`, `dynamic_cast<Foo>`) and
    // its guarded `@scope` share a match; when that scope is pushed we
    // emit a Variable witness scoped to the block — so the narrowed type
    // holds INSIDE the guard and nowhere else (scope = the refinement's
    // extent). The condition's captures precede the block in source, so
    // by the time the `@scope` event fires these are populated.
    let mut narrow_var: HashMap<usize, String> = HashMap::new();
    let mut narrow_type: HashMap<usize, String> = HashMap::new();
    let mut narrow_guard: HashMap<usize, String> = HashMap::new();
    // Recognized narrowings deferred to after flow-edge minting, so the region
    // cutoff can read the edges (`apply` below) — (subject, refined type, FULL
    // guarded-block region, block scope).
    let mut pending_narrow: Vec<(String, crate::file_analysis::InferredType, Span, ScopeId)> =
        Vec::new();
    // `std::move(x)` halves, joined per match: the qualifier (`std`) + name
    // (`move`) verify the call IS std::move (no query predicates), the var is
    // the moved subject, the call span the region start + enclosing scope.
    let mut move_scope_txt: HashMap<usize, String> = HashMap::new();
    let mut move_name_txt: HashMap<usize, String> = HashMap::new();
    let mut move_var_txt: HashMap<usize, String> = HashMap::new();
    let mut move_call: HashMap<usize, (Span, ScopeId)> = HashMap::new();
    // A context whose same-match `@scope` starts AFTER it (C++ namespace:
    // `@context` is on the name, `@scope` on the body `{`) must be
    // registered at the BODY depth, not the file depth — else it goes
    // sticky and leaks past the closing brace. Pre-index each match's
    // scope start; defer such contexts to the scope push.
    let mut scope_start_by_match: HashMap<usize, usize> = HashMap::new();
    for e in &events {
        if e.cap == "scope" {
            scope_start_by_match.entry(e.match_id).or_insert(e.start_byte);
        }
    }
    let mut pending_context: HashMap<usize, String> = HashMap::new();
    // A guard narrowing tags its consequence block `@narrow.block` (NOT `@scope`)
    // so the block mints exactly one scope from the general arm-scope pattern;
    // this maps a block's start byte → the narrow match, so when that arm @scope
    // is pushed we recover the guard's var/type/token by block position.
    let mut narrow_block_start: HashMap<usize, usize> = HashMap::new();
    for e in &events {
        if e.cap == "narrow.block" {
            narrow_block_start.entry(e.start_byte).or_insert(e.match_id);
        }
    }
    // Unevaluated-operand regions (`noexcept(...)`/`sizeof(...)`/`decltype(...)`):
    // a `std::move` whose call sits inside one is a type-trait, not a move.
    let unevaluated: Vec<(usize, usize)> = events
        .iter()
        .filter(|e| e.cap == "unevaluated")
        .map(|e| (e.start_byte, e.end_byte))
        .collect();

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
                // a context deferred to THIS scope (C++ namespace) →
                // register at the body depth so it pops with the block.
                if let Some(text) = pending_context.remove(&e.match_id) {
                    while context_stack.last().is_some_and(|&(d, _)| d >= scope_stack.len()) {
                        context_stack.pop();
                    }
                    context_stack.push((scope_stack.len(), text));
                }
                // a guard narrowing whose block is THIS scope → the refined type
                // holds within `id` (invisible outside it). Two join shapes:
                // the narrow condition either shares this @scope's match (python:
                // `consequence: (block) @scope`), or tagged the block by position
                // (`@narrow.block`) so the refinement rides a general arm @scope
                // without a fragile duplicate (cpp if/else arms).
                let narrow_mid = narrow_block_start
                    .get(&e.start_byte)
                    .copied()
                    .filter(|nmid| narrow_var.contains_key(nmid))
                    .or_else(|| narrow_var.contains_key(&e.match_id).then_some(e.match_id));
                if let Some((nmid, var)) =
                    narrow_mid.and_then(|nmid| narrow_var.get(&nmid).map(|v| (nmid, v.clone())))
                {
                    let subject = (pack.shape_name)("ref.var", &var);
                    // Type text: the guard's own `@narrow.type` when it names one
                    // (`dynamic_cast<Derived*>`), else the subject's declared type
                    // (the optional-engagement form peels `T` from it). The guard
                    // token is absent for the bare `if (opt)` truthiness form.
                    // Resolve the subject's declared type up the guard's scope
                    // chain (innermost first), so a same-named var in a sibling
                    // function never supplies the inner type — the nearest
                    // enclosing declaration of `subject` wins.
                    let ty = narrow_type.get(&nmid).cloned().or_else(|| {
                        scope_stack.iter().rev().find_map(|&(_, sid)| {
                            annot_text_by_var.get(&(subject.clone(), sid)).cloned()
                        })
                    });
                    let guard = narrow_guard.get(&nmid).map(String::as_str);
                    if let Some(refined) = ty.and_then(|t| (pack.narrow_guard)(guard, &t)) {
                        // Defer: the region cutoff (first rebind edge) needs the
                        // FlowEdges, minted after this loop. Carry the FULL
                        // guarded-block region [start, end]; the post-pass
                        // truncates it at the earliest rebind.
                        pending_narrow.push((
                            subject,
                            refined,
                            Span { start: e.start, end: e.end },
                            id,
                        ));
                    }
                }
            }
            "narrow.var" => {
                narrow_var.insert(e.match_id, e.text.clone());
            }
            "narrow.type" => {
                narrow_type.insert(e.match_id, e.text.clone());
            }
            "narrow.guard" => {
                narrow_guard.insert(e.match_id, e.text.clone());
            }
            "move.scope" => {
                move_scope_txt.insert(e.match_id, e.text.clone());
            }
            "move.name" => {
                move_name_txt.insert(e.match_id, e.text.clone());
            }
            "move.var" => {
                move_var_txt.insert(e.match_id, e.text.clone());
            }
            "move.call" => {
                // Drop moves inside an unevaluated operand — they never execute,
                // so nothing is moved-from (rule #10: the property is "does this
                // move run", asked of the region, not a shape-branch downstream).
                let unevaluated_move = unevaluated
                    .iter()
                    .any(|&(s, en)| e.start_byte >= s && e.end_byte <= en);
                if !unevaluated_move {
                    move_call
                        .insert(e.match_id, (Span { start: e.start, end: e.end }, cur_scope));
                }
            }
            cap if cap.starts_with("context.") => {
                // If this match's `@scope` starts AFTER this context, the
                // context belongs to that (not-yet-pushed) body — defer it
                // so it registers at the body depth and pops with the block.
                if scope_start_by_match.get(&e.match_id).is_some_and(|&s| s > e.start_byte) {
                    pending_context.insert(e.match_id, e.text.clone());
                } else {
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
            }
            "parent" => {
                // `@parent` (a base class) pairs with the `@def.class.name`
                // in the same match — record the inheritance edge.
                if let Some((child, _, _)) =
                    names_by_match.get(&(e.match_id, "def.class".to_string()))
                {
                    out.parents.push((child.clone(), e.text.clone()));
                }
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
                // An out-of-line def's `Class::` qualifier names its owner
                // (the LAST `::` segment, the unqualified class the engine
                // keys by) — override the enclosing-namespace context.
                let pkg = qualifier_by_match
                    .get(&e.match_id)
                    .map(|q| q.rsplit("::").next().unwrap_or(q).to_string())
                    .or_else(|| package.clone());
                out.symbols.push(SkelSymbol {
                    name: (pack.shape_name)(&format!("def.{kind}"), &name),
                    kind,
                    start: e.start,
                    end: e.end,
                    name_start,
                    name_end,
                    package: pkg,
                    scope_depth: scope_stack.len(),
                    scope: cur_scope,
                    return_type: rettype_by_match
                        .get(&e.match_id)
                        .and_then(|t| (pack.annot_type)(t)),
                    deref_stack: nested_stacks.get(&e.match_id).cloned().unwrap_or_default(),
                });
            }
            "ref.label" => {
                out.label_refs.push((
                    (pack.shape_name)("def.label", &e.text),
                    cur_scope,
                    Span { start: e.start, end: e.end },
                ));
            }
            "member.recv" => {
                member_recv.insert(
                    e.match_id,
                    (crate::file_analysis::Span { start: e.start, end: e.end }, e.text.clone()),
                );
            }
            "member.op" => {
                // Map the operator token's KIND (== its text, an anonymous
                // token) to a MemberOp via the pack's open op_map. Unmapped
                // (`.*`) → no entry → no op-DX. No source-text re-decision.
                if let Some((_, op)) = pack.op_map.iter().find(|(k, _)| *k == e.text) {
                    member_op_raw.insert(
                        e.match_id,
                        (*op, crate::file_analysis::Span { start: e.start, end: e.end }),
                    );
                }
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
                    let member_op = member_simple
                        .get(&e.match_id)
                        .copied()
                        .unwrap_or(false)
                        .then(|| member_op_raw.get(&e.match_id).copied())
                        .flatten();
                    // Reset-via-method: a rebinding method call on a simple-var
                    // receiver (`x.clear()`/`.reset()`/`.assign()`) puts a
                    // moved-from object back into a known state — a rebind. Mint
                    // a Rebind FlowEdge at the RECEIVER position so the moved-from
                    // window (and the narrowing cutoff) end there, sparing the
                    // receiver read itself. The pack owns which method names
                    // rebind (cpp vocab, like its op_map / ctor_class).
                    if e.cap == "ref.member"
                        && (pack.rebind_method)(&e.text)
                        && member_simple.get(&e.match_id).copied().unwrap_or(false)
                    {
                        if let Some((recv_span, recv_text)) = member_recv.get(&e.match_id) {
                            flow_rebinds.push((
                                (pack.shape_name)("def.var", recv_text),
                                cur_scope,
                                recv_span.start,
                            ));
                        }
                    }
                    out.refs.push(SkelRef {
                        kind: e.cap.strip_prefix("ref.").unwrap().to_string(),
                        name: (pack.shape_name)(&e.cap, &e.text),
                        start: e.start,
                        end: e.end,
                        scope: cur_scope,
                        invocant: member_recv.get(&e.match_id).cloned(),
                        member_op,
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
                // …and a candidate local-var reference, resolved to its
                // declaration in into_file_analysis (goto-def + hover).
                out.var_reads.push((
                    (pack.shape_name)("ref.var", &e.text),
                    cur_scope,
                    span,
                ));
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
                // Record the declared-type text keyed by (var, DECLARING scope)
                // for the token-less optional-engagement narrowing. cur_scope is
                // only known here (scopes mint during this walk); the type-annot
                // half was pre-collected (it precedes the declarator in source).
                if let Some(annot) = annot_by_match.get(&e.match_id) {
                    annot_text_by_var.insert(
                        ((pack.shape_name)("ref.var", &e.text), cur_scope),
                        annot.clone(),
                    );
                }
            }
            "flow.rebind" => {
                flow_rebinds.push(((pack.shape_name)("def.var", &e.text), cur_scope, e.start));
            }
            "flow.source" => {
                flow_sources.insert(e.match_id, Span { start: e.start, end: e.end });
            }
            "type.annot" => {
                annots.insert(e.match_id, e.text.clone());
            }
            "expr.shape" => {
                shape_spans.push((e.start_byte, e.end_byte, Span { start: e.start, end: e.end }));
            }
            "shape.ctor" => {
                // belongs to the smallest enclosing expr.shape; matches
                // share the call node so byte keys line up
                shape_ctors
                    .entry(byte_range_of(&events, e.match_id, "expr.shape").unwrap_or((0, 0)))
                    .or_insert_with(|| e.text.clone());
            }
            "shape.key" => {
                if let Some(range) = byte_range_of(&events, e.match_id, "expr.shape") {
                    shape_keys.push((range.0, range.1, e.text.clone()));
                }
            }
            "cmd" => {
                cmd_names.insert(
                    e.match_id,
                    (e.text.clone(), Span { start: e.start, end: e.end }, cur_scope),
                );
            }
            "cmd.arg" => {
                cmd_args
                    .entry(e.match_id)
                    .or_default()
                    .push((e.text.clone(), Span { start: e.start, end: e.end }));
            }
            "import.fn" => {
                import_fns.insert(e.match_id, e.text.clone());
            }
            "import.arg" => {
                import_args.insert(e.match_id, e.text.clone());
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

    // ---- keyed shapes → HashWithKeys witnesses ----
    {
        let mut seen_spans: std::collections::HashSet<(usize, usize)> =
            std::collections::HashSet::new();
        for &(sb, eb, span) in &shape_spans {
            if !seen_spans.insert((sb, eb)) {
                continue;
            }
            let Some(ctor) = shape_ctors.get(&(sb, eb)) else { continue };
            if !(pack.shape_ctor)(ctor) {
                continue;
            }
            let mut keys: Vec<(String, Option<Box<InferredType>>)> = shape_keys
                .iter()
                .filter(|&&(s2, e2, _)| s2 == sb && e2 == eb)
                .map(|(_, _, k)| (k.clone(), None))
                .collect();
            keys.dedup_by(|a, b| a.0 == b.0);
            lit_spans.push((sb, eb, span));
            out.witnesses.push(crate::witnesses::Witness {
                attachment: crate::witnesses::WitnessAttachment::Expr(span),
                source: crate::witnesses::WitnessSource::Builder("skeleton-shape".into()),
                payload: crate::witnesses::WitnessPayload::InferredType(
                    InferredType::HashWithKeys { keys, open: false },
                ),
                span,
            });
        }
    }

    // ---- import CALLS (library/source) → imports ----
    for (mid, f) in &import_fns {
        if let Some(arg) = import_args.get(mid) {
            if let Some(module) = (pack.import_call)(f, arg) {
                if !out.imports.contains(&module) {
                    out.imports.push(module);
                }
            }
        }
    }

    // ---- def dedup: `f <- function` matches both the sub and the
    // generic var pattern; keep the more specific kind per name site ----
    {
        let mut best: HashMap<(usize, usize), usize> = HashMap::new();
        let mut keep = vec![true; out.symbols.len()];
        for (i, sym) in out.symbols.iter().enumerate() {
            let key = (sym.name_start.row, sym.name_start.column);
            match best.get(&key) {
                None => {
                    best.insert(key, i);
                }
                Some(&j) => {
                    let (gen_i, gen_j) =
                        (out.symbols[i].kind == "var", out.symbols[j].kind == "var");
                    if gen_j && !gen_i {
                        keep[j] = false;
                        best.insert(key, i);
                    } else {
                        keep[i] = false;
                    }
                }
            }
        }
        let mut it = keep.iter();
        out.symbols.retain(|_| *it.next().unwrap());
    }

    // ---- command dispatch: classify each command's effects ----
    for (mid, (cmd, cmd_span, scope)) in &cmd_names {
        let args = cmd_args.get(mid).cloned().unwrap_or_default();
        // every invocation identifier is a call ref (user functions
        // rename through it; builtin names match no defs, harmlessly)
        out.refs.push(SkelRef {
            kind: "call".into(),
            name: cmd.clone(),
            start: cmd_span.start,
            end: cmd_span.end,
            scope: *scope,
            invocant: None,
            member_op: None,
        });
        for effect in (pack.cmd_effects)(cmd) {
            match effect {
                CmdEffect::Def { kind, name_arg } => {
                    if let Some((name, span)) = args.get(name_arg) {
                        out.symbols.push(SkelSymbol {
                            kind: kind.to_string(),
                            name: name.clone(),
                            start: cmd_span.start,
                            end: span.end,
                            name_start: span.start,
                            name_end: span.end,
                            package: None,
                            scope_depth: 0,
                            scope: *scope,
                            return_type: None,
                            deref_stack: Vec::new(),
                        });
                    }
                }
                CmdEffect::RefArgsFrom { from } => {
                    for (name, span) in args.iter().skip(from) {
                        let is_keyword =
                            !name.is_empty() && name.chars().all(|c| c.is_ascii_uppercase() || c == '_');
                        if !is_keyword && !name.contains("${") {
                            out.refs.push(SkelRef {
                                kind: "call".into(),
                                name: name.clone(),
                                start: span.start,
                                end: span.end,
                                scope: *scope,
                                invocant: None,
                                member_op: None,
                            });
                        }
                    }
                }
                CmdEffect::Import { arg } => {
                    if let Some((name, _)) = args.get(arg) {
                        if !out.imports.contains(name) {
                            out.imports.push(name.clone());
                        }
                    }
                }
            }
        }
    }

    // ---- typedef / using aliases → TypeName witnesses (the alias graph) ----
    // `typedef unsigned short U16` / `using U16 = unsigned short` push
    // `TypeName("U16") → <underlying>`: a primitive leaf stays an
    // `InferredType`; a class-shaped underlying edges to `TypeName(that)` so
    // an alias chain (`typedef V16 W16`) chases; an unrecognized leaf spelling
    // (`unsigned short` — has a space) is `ClassName(text)` so hover shows the
    // raw spelling. Struct/union/enum tag typedefs (`typedef struct op OP`)
    // don't reach here — the skeleton's @parent edge already aliases them.
    for (mid, alias) in &alias_name_by_match {
        let Some(underlying) = alias_of_by_match.get(mid) else { continue };
        out.witnesses.push(crate::witnesses::Witness {
            attachment: crate::witnesses::WitnessAttachment::TypeName(alias.clone()),
            source: crate::witnesses::WitnessSource::Builder("skeleton-typedef".into()),
            payload: type_alias_payload(underlying.trim(), pack.annot_type),
            span: Span { start: Point { row: 0, column: 0 }, end: Point { row: 0, column: 0 } },
        });
    }

    // ---- object-like `#define X body` type aliases → TypeName witnesses ----
    // Same alias graph as a typedef, so a field/var typed `PERL_BITFIELD16`
    // (defined cross-file in another header via a config-guarded `#define`)
    // chases through to its integer leaf. Gated on a TYPE-shaped body so the
    // sea of value macros (`#define MAX 100`) mints nothing.
    for (mid, alias) in &macro_alias_name_by_match {
        let Some(underlying) = macro_alias_of_by_match.get(mid) else { continue };
        let underlying = underlying.trim();
        if !looks_like_type_spelling(underlying) {
            continue;
        }
        out.witnesses.push(crate::witnesses::Witness {
            attachment: crate::witnesses::WitnessAttachment::TypeName(alias.clone()),
            source: crate::witnesses::WitnessSource::Builder("skeleton-macro-alias".into()),
            payload: type_alias_payload(underlying, pack.annot_type),
            span: Span { start: Point { row: 0, column: 0 }, end: Point { row: 0, column: 0 } },
        });
    }

    // ---- join flow captures into Variable witnesses ----
    for (mid, (name, scope, at)) in &flow_targets {
        let var = crate::witnesses::WitnessAttachment::Variable {
            name: name.clone(),
            scope: *scope,
        };
        if let Some(annot) = annots.get(mid) {
            // A class-shaped declared type edges into the alias graph (it may
            // be a typedef — `U16 x;` where `typedef unsigned short U16`);
            // primitives stay leaves; `None` (auto/void) defers to the flow
            // edge as before. `TypeName` chases the typedef or falls back to
            // the same `ClassName`, so a plain struct/class is unchanged.
            let payload = match (pack.annot_type)(annot) {
                Some(InferredType::ClassName(cn)) => Some(
                    crate::witnesses::WitnessPayload::Edge(
                        crate::witnesses::WitnessAttachment::TypeName(cn),
                    ),
                ),
                Some(t) => Some(crate::witnesses::WitnessPayload::InferredType(t)),
                None => None,
            };
            if let Some(payload) = payload {
                out.witnesses.push(crate::witnesses::Witness {
                    attachment: var.clone(),
                    source: crate::witnesses::WitnessSource::Builder("skeleton-annot".into()),
                    payload,
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
            // Mint a value-flow edge (cpp init is `Whole`); the witness is its
            // lowering, so type inference sees the same `Variable → Edge(Expr)`
            // it always did — now with the source span kept for provenance.
            out.flow_edges.push(crate::file_analysis::FlowEdge {
                target_name: name.clone(),
                target_scope: *scope,
                target_at: *at,
                source: target_span,
                extraction: crate::file_analysis::Extraction::Whole,
            });
        }
    }
    // Bind-shape rebinds (loop vars): no inflowing value, recorded for the
    // narrowing cutoff (`Rebind` lowers to nothing — provenance only).
    for (name, scope, at) in flow_rebinds {
        out.flow_edges.push(crate::file_analysis::FlowEdge {
            target_name: name,
            target_scope: scope,
            target_at: at,
            source: Span { start: at, end: at },
            extraction: crate::file_analysis::Extraction::Rebind,
        });
    }
    // Lower the value-flow edges to type-tier witnesses (the bag is canonical
    // for types; the edges are the provenance tier above it).
    for fe in &out.flow_edges {
        if let Some(w) = fe.lower_to_witness() {
            out.witnesses.push(w);
        }
    }
    // Narrowing cutoffs (THE cross-language lift): truncate each guarded region
    // at the first FlowEdge that rebinds the subject — the SAME edge-driven
    // cutoff the Perl narrowing uses (`earliest_rebind_in`). Deferred to here so
    // the edges exist. The witness gets a REAL region span [start, cutoff], so
    // point-containment ends the narrowing at the rebind — the soundness Perl
    // got from its cutoff, now generic. Every LangPack that narrows (python
    // isinstance, cpp dynamic_cast + optional engagement) gets it free.
    for (name, refined, region, scope) in pending_narrow {
        let end = crate::file_analysis::earliest_rebind_in(&out.flow_edges, &name, region)
            .unwrap_or(region.end);
        if (region.start.row, region.start.column) >= (end.row, end.column) {
            continue; // rebound before the region even opens — nothing holds
        }
        out.witnesses.push(crate::witnesses::Witness {
            attachment: crate::witnesses::WitnessAttachment::Variable { name, scope },
            source: crate::witnesses::WitnessSource::Builder("skeleton-narrow".into()),
            payload: crate::witnesses::WitnessPayload::InferredType(refined),
            span: Span { start: region.start, end },
        });
    }
    // ---- std::move sites → moved-from facts. The qualifier/name verify the
    // call IS `std::move` here (no query predicates), so the diagnostic never
    // sees the call shape — it reads the recorded var + span + scope.
    for (mid, (span, scope)) in &move_call {
        if move_scope_txt.get(mid).map(String::as_str) == Some("std")
            && move_name_txt.get(mid).map(String::as_str) == Some("move")
        {
            if let Some(v) = move_var_txt.get(mid) {
                out.moved_from.push(((pack.shape_name)("ref.var", v), *span, *scope));
            }
        }
    }
    Ok(out)
}

/// The `TypeName(alias) → …` payload for an underlying type spelling, resolving
/// it through the pack's `annot_type`: a class-shaped leaf edges into the alias
/// graph (`Edge(TypeName(cn))`), a primitive is a terminal `InferredType`, an
/// unrecognized spelling (`unsigned short` — has a space) is `ClassName(text)`
/// so hover shows it verbatim. Shared by typedef, file-local `#define`, and
/// gathered-external `#define` alias emission.
pub(crate) fn type_alias_payload(
    underlying: &str,
    annot_type: fn(&str) -> Option<InferredType>,
) -> crate::witnesses::WitnessPayload {
    use crate::witnesses::{WitnessAttachment, WitnessPayload};
    match annot_type(underlying) {
        Some(InferredType::ClassName(cn)) => WitnessPayload::Edge(WitnessAttachment::TypeName(cn)),
        Some(t) => WitnessPayload::InferredType(t),
        None => WitnessPayload::InferredType(InferredType::ClassName(underlying.to_string())),
    }
}

/// Is `body` a bare TYPE spelling (a macro that aliases a type), rather than a
/// value/expression? Accepts identifier/keyword words possibly `::`-qualified
/// and space-separated (`U16`, `unsigned short`, `std::string`, `struct op`);
/// rejects numeric literals (`100`), operators, and punctuation (`1 << 3`,
/// `(x)`, `&y`). Cheap first-token gate: a type spelling never starts with a
/// digit or a symbol, and contains only word/`:`/space bytes.
pub(crate) fn looks_like_type_spelling(body: &str) -> bool {
    let b = body.trim();
    if b.is_empty() {
        return false;
    }
    if !b.chars().next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_') {
        return false;
    }
    b.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':' || c == ' ')
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

#[cfg(test)]
#[path = "cpp_typedef_alias_tests.rs"]
mod cpp_typedef_alias_tests;
