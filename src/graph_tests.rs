use super::*;
use std::sync::Arc;

fn parse(source: &str) -> FileAnalysis {
    let mut parser = crate::builder::create_parser();
    let tree = parser.parse(source, None).unwrap();
    crate::builder::build(&tree, source.as_bytes())
}

fn cache(idx: &crate::module_index::ModuleIndex, name: &str, src: &str) {
    idx.insert_cache(
        name,
        Some(Arc::new(crate::file_analysis::CachedModule::new(
            std::path::PathBuf::from(format!("/fake/g/{}.pm", name.replace("::", "/"))),
            Arc::new(parse(src)),
        ))),
    );
}

#[test]
fn walk_inherits_preserves_isa_order_and_caps_cycles() {
    // Diamond with a cycle: C isa (A, B); A isa Top; B isa Top; Top isa C (cycle).
    let fa = parse(
        "package Top;\nuse parent -norequire, 'C';\n\
         package A;\nuse parent -norequire, 'Top';\n\
         package B;\nuse parent -norequire, 'Top';\n\
         package C;\nuse parent -norequire, 'A', 'B';\n1;\n",
    );
    let g = GraphView::new(&fa, None);
    let mut order: Vec<String> = Vec::new();
    g.walk(Node::Class("C".into()), EdgeKindMask::INHERITS, &mut |n| {
        if let Node::Class(c) = n {
            order.push(c.clone());
        }
        std::ops::ControlFlow::Continue(())
    });
    // Perl DFS: A first, A's ancestors (Top, then the cycle back to C is
    // seen-guarded), then B.
    assert_eq!(order, vec!["A", "Top", "B"]);
}

#[test]
fn walk_descendants_matches_the_legacy_fan_out() {
    let idx = crate::module_index::ModuleIndex::new_for_test();
    cache(&idx, "My::Role", "package My::Role;\nuse Moo::Role;\nrequires 'fetch';\n1;\n");
    cache(&idx, "My::Composer", "package My::Composer;\nuse Moo;\nwith 'My::Role';\nsub fetch {1}\n1;\n");
    cache(&idx, "My::SubRole", "package My::SubRole;\nuse Moo::Role;\nwith 'My::Role';\n1;\n");
    cache(&idx, "My::Deep", "package My::Deep;\nuse Moo;\nwith 'My::SubRole';\nsub fetch {7}\n1;\n");

    let fa = parse("package Probe;\n1;\n");
    let g = GraphView::new(&fa, Some(&idx));
    let mut got: Vec<String> = Vec::new();
    g.walk(Node::Class("My::Role".into()), EdgeKindMask::INHERITS_INV, &mut |n| {
        if let Node::Class(c) = n {
            got.push(c.clone());
        }
        std::ops::ControlFlow::Continue(())
    });
    got.sort();

    let mut legacy: Vec<String> = Vec::new();
    use crate::file_analysis::CrossFileLookup;
    idx.for_each_descendant_package("My::Role", &mut |pkg: &str, _cached: &Arc<crate::file_analysis::CachedModule>| {
        legacy.push(pkg.to_string());
        std::ops::ControlFlow::Continue(())
    });
    legacy.sort();
    assert_eq!(got, legacy, "graph fan-out must match the legacy walker");
    assert_eq!(got, vec!["My::Composer", "My::Deep", "My::SubRole"]);
}

#[test]
fn walk_bridges_reaches_plugin_modules_terminally() {
    let idx = crate::module_index::ModuleIndex::new_for_test();
    let plugin_src = "package My::Plugin::W;\nuse Mojo::Base 'Mojolicious::Plugin';\n\
        sub register {\n    my ($self, $app) = @_;\n    $app->helper(wcount => sub {1});\n}\n1;\n";
    idx.register_workspace_module(
        std::path::PathBuf::from("/fake/g/W.pm"),
        Arc::new(parse(plugin_src)),
    );
    let fa = parse("package Probe;\n1;\n");
    let g = GraphView::new(&fa, Some(&idx));
    // bridges target the synthetic app surface; Controller reaches it
    // through the INHERITS synthetic edge — the masks compose the way
    // the legacy ancestor+bridge walks did, in ONE walker.
    let mut mods: Vec<String> = Vec::new();
    g.walk(
        Node::Class("Mojolicious::Controller".into()),
        EdgeKindMask::BRIDGES | EdgeKindMask::INHERITS,
        &mut |n| {
            if let Node::Module(m) = n {
                mods.push(m.clone());
            }
            std::ops::ControlFlow::Continue(())
        },
    );
    assert_eq!(mods, vec!["My::Plugin::W"]);
}

