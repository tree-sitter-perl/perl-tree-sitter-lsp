use super::*;

#[test]
fn test_document_new() {
    let doc = Document::new("my $x = 1;".to_string()).unwrap();
    assert_eq!(doc.text, "my $x = 1;");
    assert_eq!(doc.tree.root_node().kind(), "source_file");
}

#[test]
fn test_document_update() {
    let mut doc = Document::new("my $x = 1;".to_string()).unwrap();
    doc.update("my $y = 2;".to_string());
    assert_eq!(doc.text, "my $y = 2;");
    assert_eq!(doc.tree.root_node().kind(), "source_file");
}

#[test]
fn test_byte_to_point() {
    assert_eq!(byte_to_point(b"hello\nworld", 0), Point::new(0, 0));
    assert_eq!(byte_to_point(b"hello\nworld", 5), Point::new(0, 5));
    assert_eq!(byte_to_point(b"hello\nworld", 6), Point::new(1, 0));
    assert_eq!(byte_to_point(b"hello\nworld", 8), Point::new(1, 2));
}

#[test]
fn test_incremental_update_preserves_tree() {
    // Simulate typing `$self->` mid-line in a sub body
    let original = "sub add {\n    my ($self, $a, $b) = @_;\n    my $result = $a + $b;\n    return $result;\n}\n";
    let mut doc = Document::new(original.to_string()).unwrap();

    // Insert `$self->` at beginning of line 3 (after "    ")
    let edited = "sub add {\n    my ($self, $a, $b) = @_;\n    $self->my $result = $a + $b;\n    return $result;\n}\n";
    doc.update(edited.to_string());

    // The tree should still have a root source_file, not be completely borked
    assert_eq!(doc.tree.root_node().kind(), "source_file");

    // The sub declaration should still be recognized
    let root = doc.tree.root_node();
    let mut found_sub = false;
    for i in 0..root.named_child_count() {
        if let Some(child) = root.named_child(i) {
            if child.kind() == "subroutine_declaration_statement" {
                found_sub = true;
                break;
            }
        }
    }
    assert!(found_sub, "sub declaration should survive partial edit");
}
