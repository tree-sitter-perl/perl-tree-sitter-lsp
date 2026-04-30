use super::*;

#[test]
fn test_incremental_preserves_packages() {
    let original = std::fs::read_to_string("test_files/frameworks.pl").unwrap();
    let mut doc = Document::new(original.clone()).unwrap();

    // Count packages in clean parse
    let clean_pkgs: Vec<String> = doc
        .analysis
        .symbols
        .iter()
        .filter(|s| {
            matches!(
                s.kind,
                crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class
            )
        })
        .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
        .collect();
    println!("Clean parse packages:");
    for p in &clean_pkgs {
        println!("  {}", p);
    }

    // Simulate typing 'reduce' on line 19 (0-indexed)
    let lines: Vec<&str> = original.lines().collect();
    let mut dirty_lines: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
    dirty_lines.insert(19, "reduce".to_string());
    let dirty = dirty_lines.join("\n");

    // Use incremental update (same as LSP did_change path)
    doc.update(dirty);

    let dirty_pkgs: Vec<String> = doc
        .analysis
        .symbols
        .iter()
        .filter(|s| {
            matches!(
                s.kind,
                crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class
            )
        })
        .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
        .collect();
    println!("\nIncremental parse (after typing 'reduce' on L20):");
    for p in &dirty_pkgs {
        println!("  {}", p);
    }

    println!("\nTree has errors: {}", doc.tree.root_node().has_error());

    // Check if MojoApp survived
    let has_mojo = dirty_pkgs.iter().any(|p| p.contains("MojoApp"));
    println!("MojoApp survived incremental: {}", has_mojo);

    // For comparison, do a FRESH parse of the same dirty text
    let fresh = Document::new(dirty_lines.join("\n")).unwrap();
    let fresh_pkgs: Vec<String> = fresh
        .analysis
        .symbols
        .iter()
        .filter(|s| {
            matches!(
                s.kind,
                crate::file_analysis::SymKind::Package | crate::file_analysis::SymKind::Class
            )
        })
        .map(|s| format!("L{}: {}", s.selection_span.start.row + 1, s.name))
        .collect();
    println!("\nFresh parse (same dirty text, no incremental):");
    for p in &fresh_pkgs {
        println!("  {}", p);
    }
    let fresh_has_mojo = fresh_pkgs.iter().any(|p| p.contains("MojoApp"));
    println!("MojoApp survived fresh: {}", fresh_has_mojo);
}
