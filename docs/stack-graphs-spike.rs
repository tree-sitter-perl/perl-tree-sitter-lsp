// Stack-graphs evaluation spike — see docs/adr/stack-graphs-evaluation.md.
// NOT part of the perl-lsp build. To run: place in a fresh crate's src/main.rs
// with `stack-graphs = "0.14"` in Cargo.toml, then `cargo run --release`.
//
//! Stack-graphs evaluation spike for perl-lsp.
//!
//! Builds name-binding graphs by hand via the stack-graphs Rust API and runs
//! the path-stitcher, to test two claims empirically:
//!
//!   (A) pure name binding (cross-file `use`-import, classic sub call, lexical
//!       `my` var) resolves cleanly with stack graphs.
//!   (B) type-gated method dispatch (`my $o = Obj->new; $o->greet`) does NOT
//!       resolve unless the receiver's *inferred type* is supplied to the graph
//!       as extra edges — i.e. the type inference must be reified outside the
//!       framework and fed in.
//!
//! Each graph is a faithful adaptation of the upstream stack-graphs test
//! fixtures (`sequenced_import_star`, `class_field_through_function_parameter`)
//! to Perl surface syntax.

use std::collections::BTreeSet;
use std::time::Instant;

use stack_graphs::arena::Handle;
use stack_graphs::graph::{File, Node, NodeID, StackGraph, Symbol};
use stack_graphs::partial::PartialPaths;
use stack_graphs::stitching::{ForwardPartialPathStitcher, GraphEdgeCandidates, StitcherConfig};
use stack_graphs::NoCancellation;

// --- tiny sugar over the raw StackGraph API (mirrors the upstream test trait) ---
struct G {
    g: StackGraph,
    next: u32,
}
impl G {
    fn new() -> Self {
        G { g: StackGraph::new(), next: 1 }
    }
    fn sym(&mut self, s: &str) -> Handle<Symbol> {
        self.g.add_symbol(s)
    }
    fn file(&mut self, n: &str) -> Handle<File> {
        self.g.get_or_create_file(n)
    }
    fn id(&mut self, f: Handle<File>) -> NodeID {
        let i = self.next;
        self.next += 1;
        NodeID::new_in_file(f, i)
    }
    fn def(&mut self, f: Handle<File>, s: Handle<Symbol>) -> Handle<Node> {
        let id = self.id(f);
        self.g.add_pop_symbol_node(id, s, true).unwrap()
    }
    fn reference(&mut self, f: Handle<File>, s: Handle<Symbol>) -> Handle<Node> {
        let id = self.id(f);
        self.g.add_push_symbol_node(id, s, true).unwrap()
    }
    fn pop(&mut self, f: Handle<File>, s: Handle<Symbol>) -> Handle<Node> {
        let id = self.id(f);
        self.g.add_pop_symbol_node(id, s, false).unwrap()
    }
    fn push(&mut self, f: Handle<File>, s: Handle<Symbol>) -> Handle<Node> {
        let id = self.id(f);
        self.g.add_push_symbol_node(id, s, false).unwrap()
    }
    fn scope(&mut self, f: Handle<File>) -> Handle<Node> {
        let id = self.id(f);
        self.g.add_scope_node(id, false).unwrap()
    }
    fn edge(&mut self, a: Handle<Node>, b: Handle<Node>) {
        self.g.add_edge(a, b, 0);
    }
    fn root(&self) -> Handle<Node> {
        StackGraph::root_node()
    }
}

/// Resolve every reference node and return the set of "ref -> def" strings.
fn resolve_all(g: &StackGraph) -> BTreeSet<String> {
    let mut paths = PartialPaths::new();
    let mut out = BTreeSet::new();
    let refs = g.iter_nodes().filter(|h| g[*h].is_reference());
    ForwardPartialPathStitcher::find_all_complete_partial_paths(
        &mut GraphEdgeCandidates::new(g, &mut paths, None),
        refs,
        StitcherConfig::default(),
        &NoCancellation,
        |g, paths, p| {
            out.insert(p.display(g, paths).to_string());
        },
    )
    .unwrap();
    out
}

/// (A) Name binding: cross-file `use`-import + classic call + lexical `my`.
///
/// ```perl
/// # Util.pm        package Util; our @EXPORT=('helper'); sub helper {...}
/// # main.pl        use Util; helper(); my $x = 1; $x;
/// ```
fn graph_name_binding() -> StackGraph {
    let mut b = G::new();
    let root = b.root();
    let dot = b.sym(".");
    let s_util = b.sym("Util");
    let s_helper = b.sym("helper");
    let s_x = b.sym("x");

    // ---- main.pl ----
    let main = b.file("main.pl");
    let m_scope = b.scope(main); // file scope
    // helper()  -> reference that climbs into module-import scope
    let m_helper = b.reference(main, s_helper);
    let m_dot = b.push(main, dot);
    let m_use_util = b.reference(main, s_util); // `use Util`
    b.edge(m_helper, m_scope);
    b.edge(m_scope, m_dot);
    b.edge(m_dot, m_use_util);
    b.edge(m_use_util, root);
    // my $x = 1;  $x;   (pure lexical, same file)
    let m_def_x = b.def(main, s_x);
    let m_ref_x = b.reference(main, s_x);
    b.edge(m_scope, m_def_x);
    b.edge(m_ref_x, m_scope);

    // ---- Util.pm ----
    let util = b.file("Util.pm");
    let u_def = b.def(util, s_util); // the module itself
    let u_dot = b.pop(util, dot);
    let u_scope = b.scope(util);
    let u_helper = b.def(util, s_helper); // sub helper
    b.edge(root, u_def);
    b.edge(u_def, u_dot);
    b.edge(u_dot, u_scope);
    b.edge(u_scope, u_helper);

    b.g
}

/// (B) Type-gated method dispatch: `my $o = Obj->new; $o->greet`.
///
/// `with_type_flow=false` models a graph built from syntax alone — there is no
/// edge connecting `$o` to class `Obj`, because nothing in the *syntax* says
/// what `Obj->new` returns. `with_type_flow=true` injects that one fact (the
/// witness-bag result `$o : Obj`) as edges.
fn graph_method_dispatch(with_type_flow: bool) -> StackGraph {
    let mut b = G::new();
    let root = b.root();
    let dot = b.sym(".");
    let s_obj = b.sym("Obj");
    let s_greet = b.sym("greet");
    let s_o = b.sym("o"); // the lexical $o

    // ---- Obj.pm : sub greet {...}  (and class member scope) ----
    let obj = b.file("Obj.pm");
    let o_def = b.def(obj, s_obj);
    let o_dot = b.pop(obj, dot);
    let o_members = b.scope(obj); // class member scope
    let o_greet = b.def(obj, s_greet); // sub greet
    b.edge(root, o_def);
    b.edge(o_def, o_dot);
    b.edge(o_dot, o_members);
    b.edge(o_members, o_greet);

    // ---- main.pl : my $o = Obj->new; $o->greet ----
    let main = b.file("main.pl");
    let m_scope = b.scope(main);
    // declaration site of $o
    let m_def_o = b.def(main, s_o);
    b.edge(m_scope, m_def_o);
    // `$o->greet` : reference `greet`, pushed `.`, reference `o`
    let m_greet = b.reference(main, s_greet);
    let m_dot = b.push(main, dot);
    let m_ref_o = b.reference(main, s_o);
    b.edge(m_greet, m_dot);
    b.edge(m_dot, m_ref_o);
    b.edge(m_ref_o, m_scope); // $o resolves to its decl via lexical scope

    if with_type_flow {
        // THE REIFIED TYPE-INFERENCE FACT: `$o : Obj`.
        // Connect $o's declaration to Obj's member scope, so a `.greet` lookup
        // landing on $o continues into Obj's members. In the real LSP this edge
        // is *computed* by the witness bag (bless target of `new`, framework
        // synthesis, inheritance). Stack graphs cannot derive it from syntax.
        let m_to_obj_dot = b.pop(main, dot);
        b.edge(m_def_o, m_to_obj_dot);
        b.edge(m_to_obj_dot, o_members);
    }

    b.g
}

fn main() {
    let t0 = Instant::now();

    println!("== (A) NAME BINDING: use-import + classic call + lexical my ==");
    let g = graph_name_binding();
    let q = Instant::now();
    let res = resolve_all(&g);
    let qa = q.elapsed();
    for r in &res {
        println!("  {r}");
    }
    println!("  -> {} reference(s) resolved in {:?}", res.len(), qa);

    println!("\n== (B0) METHOD DISPATCH, syntax only (no type inference) ==");
    let g0 = graph_method_dispatch(false);
    let r0 = resolve_all(&g0);
    let greet0 = r0.iter().filter(|s| s.contains("greet")).count();
    for r in &r0 {
        println!("  {r}");
    }
    println!("  -> `greet` resolutions: {greet0}  (0 = unresolved: receiver type unknown)");

    println!("\n== (B1) METHOD DISPATCH, with `$o : Obj` reified as edges ==");
    let g1 = graph_method_dispatch(true);
    let r1 = resolve_all(&g1);
    let greet1 = r1.iter().filter(|s| s.contains("greet")).count();
    for r in &r1 {
        println!("  {r}");
    }
    println!("  -> `greet` resolutions: {greet1}  (1 = resolved ONLY because the type fact was injected)");

    println!("\n[spike wall-time incl. graph build + 3 stitches: {:?}]", t0.elapsed());
}
