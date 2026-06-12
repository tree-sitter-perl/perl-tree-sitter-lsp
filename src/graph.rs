//! The typed-edge graph — one walker over what is morally one graph.
//!
//! Phase 1 of `docs/prompt-graph-walking.md`: a DERIVED view, no new
//! storage. Edges materialize on demand from the stores that already
//! exist (`package_parents` ∪ `parents_of`'s synthetic edges, the
//! ModuleEdgeIndexes children map, plugin-namespace bridges); `walk`
//! is the single traversal — seen-set, depth cap, edge-kind mask —
//! that the bespoke walkers collapse into one consumer at a time
//! (strangler fig).
//!
//! The builder does NOT build this. It consumes `&FileAnalysis` +
//! `CrossFileLookup` and answers queries; `FileAnalysis` stays the
//! canonical model (rule #2).

use crate::file_analysis::{CrossFileLookup, FileAnalysis};

bitflags::bitflags! {
    /// Which edges a walk may traverse. Today's bespoke walkers are
    /// masks: ancestry = INHERITS, the descendant/composer fan-out =
    /// INHERITS_INV, plugin-entity reachability = BRIDGES.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct EdgeKindMask: u8 {
        /// class → parent class/role (`use parent`/`@ISA`/`with`/…,
        /// plus the synthetic app-surface edge — `parents_of` is the
        /// single injection site, shared with the legacy walkers so
        /// the two cannot disagree during the migration).
        const INHERITS     = 1 << 0;
        /// parent → direct child/composer (the `children_index`
        /// inverse; `walk` supplies the transitivity).
        const INHERITS_INV = 1 << 1;
        /// class → modules whose plugin namespaces bridge to it.
        /// Module nodes are terminal — bridge edges don't compose.
        const BRIDGES      = 1 << 2;
    }
}

/// A graph node. Phase 1 carries the class axis (+ terminal Module
/// nodes for bridges); Scope/Symbol/File nodes join as their walkers
/// port — see the migration table in the design doc.
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
    /// excluded) in traversal order and may stop early. Perl's
    /// left-to-right DFS order is preserved on INHERITS so method
    /// resolution semantics survive the port.
    pub fn walk(
        &self,
        origin: Node,
        mask: EdgeKindMask,
        visit: &mut dyn FnMut(&Node) -> std::ops::ControlFlow<()>,
    ) {
        let mut seen: std::collections::HashSet<Node> = std::collections::HashSet::new();
        seen.insert(origin.clone());
        let mut stack: Vec<(Node, usize)> = vec![(origin, 0)];
        const MAX_DEPTH: usize = 21; // matches the legacy ancestry guard
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

    /// Edge derivation — the ONE place graph structure comes from. New
    /// edge kinds add an arm here, never a parallel walker.
    fn edges_from(&self, node: &Node, mask: EdgeKindMask, out: &mut Vec<Node>) {
        let Node::Class(class) = node else { return };
        if mask.contains(EdgeKindMask::INHERITS) {
            for p in crate::file_analysis::parents_of(
                class,
                &self.fa.package_parents,
                self.idx,
                &self.fa.app_surface_consumers,
            ) {
                out.push(Node::Class(p));
            }
        }
        if mask.contains(EdgeKindMask::INHERITS_INV) {
            if let Some(idx) = self.idx {
                for (pkg, _module) in idx.direct_children_of(class) {
                    out.push(Node::Class(pkg));
                }
            }
        }
        if mask.contains(EdgeKindMask::BRIDGES) {
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

#[cfg(test)]
#[path = "graph_tests.rs"]
mod tests;
