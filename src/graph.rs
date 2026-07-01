//! The typed-edge graph — one walker over what is morally one graph.
//!
//! A DERIVED view, no stored graph. Edges materialize on demand from
//! the stores that already exist (`package_parents` ∪ `parents_of`'s
//! synthetic app-surface edge, the `ModuleEdgeIndexes` children map,
//! plugin-namespace bridges); `walk` is the single traversal — seen-
//! set, depth cap, edge-kind mask — that the ancestry/bridge/descendant
//! queries route through. Design: `docs/adr/graph-walking.md`.
//!
//! `GraphView` consumes `&FileAnalysis` + the `CrossFileLookup` trait
//! and answers queries; `FileAnalysis` stays the canonical model (rule
//! #2), and the builder never touches this.
//!
//! Model layer: a derived view over `&FileAnalysis` and the model-
//! defined `CrossFileLookup` trait, with zero Index-layer deps — so the
//! model-internal walkers (`for_each_ancestor_class` and the dispatch/
//! method/bridge resolution that funnels through it) call `walk`
//! directly, no up-layer import.

use crate::file_analysis::{CrossFileLookup, FileAnalysis};

/// One typed edge family. The CLOSED set of edge kinds — `edges_from`
/// matches on this exhaustively, so adding a kind is a compile error
/// until its derivation is written (the design doc's "one match site,
/// never a parallel walker" invariant, with compiler teeth). The
/// bitflag MASK below is set membership over this enum, not a separate
/// source of truth: `EdgeKind::ALL` + `flag()` keep them in lockstep.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeKind {
    /// class → parent class/role (`use parent`/`@ISA`/`with`/…, plus
    /// the synthetic app-surface edge). `parents_of` is the single
    /// injection site for this edge — the inheritance consumers share
    /// it, so they can't disagree on the MRO.
    Inherits,
    /// parent → direct child/composer (the `children_index` inverse;
    /// `walk` supplies the transitivity).
    InheritsInv,
    /// class → modules whose plugin namespaces bridge to it. Module
    /// nodes are terminal — bridge edges don't compose.
    Bridges,
}

impl EdgeKind {
    /// Every variant. New kinds MUST be added here — the `edges_from`
    /// loop iterates it, so a forgotten kind is never traversed (and
    /// its `flag()` arm + match arm are compile errors meanwhile).
    pub const ALL: [EdgeKind; 3] = [Self::Inherits, Self::InheritsInv, Self::Bridges];

    fn flag(self) -> EdgeKindMask {
        match self {
            EdgeKind::Inherits => EdgeKindMask::INHERITS,
            EdgeKind::InheritsInv => EdgeKindMask::INHERITS_INV,
            EdgeKind::Bridges => EdgeKindMask::BRIDGES,
        }
    }
}

bitflags::bitflags! {
    /// A SET of [`EdgeKind`]s a walk may traverse (`INHERITS | BRIDGES`).
    /// Storage + ergonomic `|` only — `EdgeKind` is the source of truth;
    /// the consts here mirror its variants via `EdgeKind::flag()`.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct EdgeKindMask: u8 {
        const INHERITS     = 1 << 0;
        const INHERITS_INV = 1 << 1;
        const BRIDGES      = 1 << 2;
    }
}

/// A graph node. The class axis + terminal Module nodes for bridges;
/// Scope/Symbol/File nodes are future taxonomy (`adr/graph-walking.md`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Class(String),
    Module(String),
}

/// The derived view: borrows the origin file's analysis (local edges)
/// and the index (cross-file edges). Build one per query — it holds no
/// state beyond the borrows.
pub struct GraphView<'a> {
    fa: &'a FileAnalysis,
    idx: Option<&'a dyn CrossFileLookup>,
}

impl<'a> GraphView<'a> {
    pub fn new(fa: &'a FileAnalysis, idx: Option<&'a dyn CrossFileLookup>) -> Self {
        GraphView { fa, idx }
    }

    /// THE walker. DFS from `origin` over edges in `mask`, depth-capped
    /// and cycle-safe; `visit` sees every reached node (origin
    /// excluded) in traversal order and may stop early. On INHERITS the
    /// order is Perl's left-to-right DFS MRO, so method resolution sees
    /// ancestors in the order dispatch demands.
    pub fn walk(
        &self,
        origin: Node,
        mask: EdgeKindMask,
        visit: &mut dyn FnMut(&Node) -> std::ops::ControlFlow<()>,
    ) {
        let mut seen: std::collections::HashSet<Node> = std::collections::HashSet::new();
        seen.insert(origin.clone());
        let mut stack: Vec<(Node, usize)> = vec![(origin, 0)];
        const MAX_DEPTH: usize = 21; // ancestry-depth backstop (seen-set already breaks cycles)
        while let Some((node, depth)) = stack.pop() {
            // visit at POP — depth-first order, so a left parent's whole
            // ancestry precedes the right parent (the @ISA contract).
            // depth 0 is the origin, which callers already hold.
            if depth > 0 && visit(&node).is_break() {
                return;
            }
            if depth >= MAX_DEPTH {
                continue;
            }
            let mut next: Vec<Node> = Vec::new();
            self.edges_from(&node, mask, &mut next);
            // reverse-push so LIFO pops preserve edge order
            for n in next.into_iter().rev() {
                if seen.insert(n.clone()) {
                    stack.push((n, depth + 1));
                }
            }
        }
    }

    /// Edge derivation — the ONE place graph structure comes from. The
    /// `match` is EXHAUSTIVE over `EdgeKind`, so a new kind can't be
    /// added without writing its derivation here (never a parallel
    /// walker).
    fn edges_from(&self, node: &Node, mask: EdgeKindMask, out: &mut Vec<Node>) {
        let Node::Class(class) = node else { return };
        for kind in EdgeKind::ALL {
            if !mask.contains(kind.flag()) {
                continue;
            }
            match kind {
                EdgeKind::Inherits => {
                    for p in crate::file_analysis::parents_of(
                        class,
                        &self.fa.package_parents,
                        self.idx,
                        &self.fa.app_surface_consumers,
                    ) {
                        out.push(Node::Class(p));
                    }
                }
                EdgeKind::InheritsInv => {
                    // Local children: a class in THIS file naming `class` as a
                    // parent (same-file hierarchies — many structs pasting one
                    // member-block role macro, a single-file Perl `@ISA` chain).
                    // Symmetric with `parents_of` (local ∪ cross-file); the walk's
                    // seen-set dedups against the cross-file index below.
                    for (child, parents) in &self.fa.package_parents {
                        if parents.iter().any(|p| p == class) {
                            out.push(Node::Class(child.clone()));
                        }
                    }
                    if let Some(idx) = self.idx {
                        for (pkg, _module) in idx.direct_children_of(class) {
                            out.push(Node::Class(pkg));
                        }
                    }
                }
                EdgeKind::Bridges => {
                    if let Some(idx) = self.idx {
                        let mut seen_mods: std::collections::HashSet<String> = Default::default();
                        idx.for_each_entity_bridged_to(class, &mut |module, _cached, _sym| {
                            if seen_mods.insert(module.to_string()) {
                                out.push(Node::Module(module.to_string()));
                            }
                        });
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[path = "graph_tests.rs"]
mod tests;
