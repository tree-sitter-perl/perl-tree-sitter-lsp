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
    /// `receiver OP member` sites (simple-variable receivers) for the
    /// member-access operator-DX consumer. Collected by a dedicated walk
    /// over `pack.member_kinds`, not the skeleton query.
    pub member_access_sites: Vec<crate::file_analysis::MemberAccessSite>,
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
                    bag.push(mk(WA::Symbol(sym.id), WP::InferredType(ret.clone()), sym.span));
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
                            invocant: crate::conventions::InvocantName::assume_canonical(inv_text),
                            invocant_span: Some(inv_span),
                            method_name_span: Span { start: r.start, end: r.end },
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
            member_access_sites: std::mem::take(&mut self.member_access_sites),
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
    /// function/operator like `isinstance`, `ref`) and the type token
    /// (`@narrow.type`), the refined type that holds inside the guarded
    /// block, or `None` if this guard doesn't narrow. The pack owns
    /// "which guard means which refinement" (rule #10); core just scopes
    /// the witness to the block.
    pub narrow_guard: fn(guard: &str, type_text: &str) -> Option<InferredType>,
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
    /// Declarator node kinds that nest into a pointer/reference stack, each
    /// mapped to the deref it contributes. A `@nested.target` capture on such
    /// a chain is unravelled generically by `peel_nested` — the pack supplies
    /// the grammar, core supplies the walk. Empty = no nesting (the capture
    /// is then simply absent from the pack's query).
    pub nested_peel: &'static [(&'static str, crate::file_analysis::DerefKind)],
    /// Leaf node kinds a `nested_peel` chain bottoms out at, each mapped to
    /// the `def.*` capture the synthetic leaf event mints — `identifier`→
    /// `def.local`, `field_identifier`→`def.var` (so a pointer field still
    /// outlines as a class member).
    pub nested_leaves: &'static [(&'static str, &'static str)],
    /// Per-level annotation node kinds collected onto each `DerefStep`
    /// (`type_qualifier` → `const`/`volatile`/`restrict`). Generic so new
    /// qualifiers + const-correctness diagnostics needn't touch core.
    pub nested_annot_kinds: &'static [&'static str],
    /// Transparent expression wrappers a member-access RECEIVER peels through
    /// to its typed inner (`(*p)`, `(&o)`, `(p)` → `p`): pointer-/ref-ness is
    /// dropped, so the invocant types via the inner's span. The build-time
    /// twin of the cursor-completion peel.
    pub recv_wrapper_kinds: &'static [&'static str],
    /// Member-access node kinds (`receiver OP member`) — extraction records
    /// each site (simple-variable receiver, operator token span, `->` vs
    /// `.`) for the operator-DX consumer (`p.` on a `Box*` should be `->`).
    /// The receiver is the first named child; the operator is the anonymous
    /// child between the two named children. Empty = no member-access DX
    /// (Perl, and packs whose member access isn't operator-correctable).
    pub member_kinds: &'static [&'static str],
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
        trigger_chars: &["$", "@", "%", ">", ":", "{"],
        receiver_names: &[],
        nested_peel: &[],
        nested_leaves: &[],
        nested_annot_kinds: &[],
        recv_wrapper_kinds: &[],
        member_kinds: &[],
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
        narrow_guard: |guard, ty| (guard == "isinstance").then(|| InferredType::ClassName(ty.to_string())),
        trigger_chars: &["."],
        receiver_names: &["self", "cls"],
        nested_peel: &[],
        nested_leaves: &[],
        nested_annot_kinds: &[],
        recv_wrapper_kinds: &[],
        member_kinds: &[],
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
        trigger_chars: &["$", "@", ":"],
        receiver_names: &[],
        nested_peel: &[],
        nested_leaves: &[],
        nested_annot_kinds: &[],
        recv_wrapper_kinds: &[],
        member_kinds: &[],
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
        trigger_chars: &["{", "("],
        receiver_names: &[],
        nested_peel: &[],
        nested_leaves: &[],
        nested_annot_kinds: &[],
        recv_wrapper_kinds: &[],
        member_kinds: &[],
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
        narrow_guard: |_, _| None,
        trigger_chars: &[".", ">", ":"],
        receiver_names: &["this"],
        nested_peel: &[
            ("pointer_declarator", crate::file_analysis::DerefKind::Pointer),
            ("reference_declarator", crate::file_analysis::DerefKind::Reference),
        ],
        nested_leaves: &[("identifier", "def.local"), ("field_identifier", "def.var")],
        nested_annot_kinds: &["type_qualifier"],
        recv_wrapper_kinds: &["parenthesized_expression", "pointer_expression"],
        // `box.m` and `box->m` both parse as field_expression — the one
        // member-access shape, operator distinguished by the inner token.
        member_kinds: &["field_expression"],
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

/// Peel transparent expression wrappers (`(*p)`, `(&o)`, `(p)`) off a
/// member-access receiver to its typed inner. Pointer-/reference-ness is
/// dropped for navigation, so the inner's type IS the receiver's. The pack
/// declares `recv_wrapper_kinds`; core descends the first named child while
/// the kind matches. Depth-capped against a pathological tree.
fn peel_recv<'a>(mut node: tree_sitter::Node<'a>, pack: &LangPack) -> tree_sitter::Node<'a> {
    for _ in 0..32 {
        if !pack.recv_wrapper_kinds.contains(&node.kind()) {
            break;
        }
        match node.named_child(0) {
            Some(inner) => node = inner,
            None => break,
        }
    }
    node
}

/// Unravel a pointer/reference declarator chain (the `@nested.target`
/// capture) to its leaf identifier + the per-level deref stack. The pack
/// declares which node kinds nest (`nested_peel`), which kind is the leaf
/// (`nested_leaves`), and which kinds are per-level annotations
/// (`nested_annot_kinds`, e.g. `type_qualifier`) — core walks generically,
/// branching on no grammar name of its own. Outermost level first, which is
/// also left-to-right display order after the base type (`Box*&` →
/// `[Pointer, Reference]`). Depth-capped against a pathological tree.
fn peel_nested<'a>(
    mut node: tree_sitter::Node<'a>,
    pack: &LangPack,
    src: &[u8],
) -> Option<(tree_sitter::Node<'a>, Vec<crate::file_analysis::DerefStep>, &'static str)> {
    use crate::file_analysis::DerefStep;
    let is_leaf = |k: &str| pack.nested_leaves.iter().find(|(lk, _)| *lk == k);
    let mut stack = Vec::new();
    for _ in 0..32 {
        if let Some((_, kind)) = pack.nested_peel.iter().find(|(k, _)| *k == node.kind()) {
            let mut annotations = Vec::new();
            let mut inner = None;
            let mut cur = node.walk();
            for ch in node.children(&mut cur) {
                if pack.nested_annot_kinds.contains(&ch.kind()) {
                    if let Ok(t) = ch.utf8_text(src) {
                        annotations.push(t.to_string());
                    }
                } else if pack.nested_peel.iter().any(|(k, _)| *k == ch.kind())
                    || is_leaf(ch.kind()).is_some()
                {
                    inner = Some(ch);
                }
            }
            stack.push(DerefStep { kind: *kind, annotations });
            node = inner?;
        } else if let Some((_, def_cap)) = is_leaf(node.kind()) {
            // The leaf kind decides the symbol the synthetic event mints —
            // `identifier`→`def.local` (param/local), `field_identifier`→
            // `def.var` (a class member), so fields outline as members.
            return Some((node, stack, def_cap));
        } else {
            return None;
        }
    }
    None
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
    // match_id → the pointer/reference declarator stack a `@nested.target`
    // capture unravelled to. Read by the `def.*` handler to stamp the symbol.
    let mut nested_stacks: std::collections::HashMap<usize, Vec<crate::file_analysis::DerefStep>> =
        std::collections::HashMap::new();
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&query, tree.root_node(), source);
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
                if let Some((leaf, stack, def_cap)) = peel_nested(node, pack, source) {
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
                let inner = peel_recv(node, pack);
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
    // `@qualifier` (a `Class::` on an out-of-line def) and `@rettype` (a
    // method's declared return type) — pre-collected like names because the
    // `@def` event fires before these inner captures.
    let mut qualifier_by_match: HashMap<usize, String> = HashMap::new();
    let mut rettype_by_match: HashMap<usize, String> = HashMap::new();
    // `@member.recv` → the receiver span of a `recv.field` access, joined to
    // its `@ref.member` by match_id so the minted MethodCall ref carries it.
    let mut member_recv: HashMap<usize, (crate::file_analysis::Span, String)> = HashMap::new();
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
                // a guard narrowing whose block is THIS scope → the
                // refined type holds within `id` (and is invisible
                // outside it). annot_type maps primitive guards; anything
                // else is a class refinement.
                if let (Some(var), Some(ty), Some(guard)) = (
                    narrow_var.get(&e.match_id),
                    narrow_type.get(&e.match_id),
                    narrow_guard.get(&e.match_id),
                ) {
                    if let Some(refined) = (pack.narrow_guard)(guard, ty) {
                        let span = Span { start: e.start, end: e.start };
                        out.witnesses.push(crate::witnesses::Witness {
                            attachment: crate::witnesses::WitnessAttachment::Variable {
                                name: (pack.shape_name)("ref.var", var),
                                scope: id,
                            },
                            source: crate::witnesses::WitnessSource::Builder("skeleton-narrow".into()),
                            payload: crate::witnesses::WitnessPayload::InferredType(refined),
                            span,
                        });
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
                        invocant: member_recv.get(&e.match_id).cloned(),
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
    collect_member_sites(tree.root_node(), pack, source, &mut out.member_access_sites);
    Ok(out)
}

/// Walk the whole tree recording `receiver OP member` sites for the
/// operator-DX consumer. Only simple-variable receivers are kept — the
/// shape whose `deref_stack` the diagnostic pass can resolve by name (a
/// chained `a.b.c` receiver isn't a variable, so it self-skips). No-op for
/// packs without `member_kinds`.
fn collect_member_sites(
    root: tree_sitter::Node,
    pack: &LangPack,
    source: &[u8],
    out: &mut Vec<crate::file_analysis::MemberAccessSite>,
) {
    if pack.member_kinds.is_empty() {
        return;
    }
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        if pack.member_kinds.contains(&node.kind()) {
            if let Some(site) = member_site(node, source) {
                out.push(site);
            }
        }
        let mut cur = node.walk();
        for ch in node.children(&mut cur) {
            stack.push(ch);
        }
    }
}

/// One member-access node → its site, when the receiver is a simple
/// identifier. The operator is the anonymous child whose text is `.`/`->`
/// (reading what the user WROTE — the expected operator is decided later
/// off the receiver's `deref_stack`, never off this token).
fn member_site(
    node: tree_sitter::Node,
    source: &[u8],
) -> Option<crate::file_analysis::MemberAccessSite> {
    use crate::file_analysis::{MemberAccessSite, Span};
    let receiver = node.named_child(0)?;
    if receiver.kind() != "identifier" {
        return None;
    }
    let name = receiver.utf8_text(source).ok()?.to_string();
    let mut cur = node.walk();
    let op = node.children(&mut cur).find(|ch| {
        !ch.is_named() && matches!(ch.utf8_text(source), Ok(".") | Ok("->"))
    })?;
    let arrow = op.utf8_text(source) == Ok("->");
    Some(MemberAccessSite {
        receiver: name,
        receiver_at: receiver.start_position(),
        op_span: Span { start: op.start_position(), end: op.end_position() },
        arrow,
    })
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
