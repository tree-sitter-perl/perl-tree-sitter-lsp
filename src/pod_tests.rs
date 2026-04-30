use super::*;

#[test]
fn test_basic_headings() {
    let pod = "=head1 NAME\n\nFoo - a foo module\n\n=head2 bar\n\nDoes bar things.\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("### NAME"), "got: {}", md);
    assert!(md.contains("#### bar"), "got: {}", md);
    assert!(md.contains("Does bar things."), "got: {}", md);
}

#[test]
fn test_inline_formatting() {
    let pod = "=head2 test\n\nUse C<foo> for B<bold> and I<italic>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("`foo`"), "got: {}", md);
    assert!(md.contains("**bold**"), "got: {}", md);
    assert!(md.contains("*italic*"), "got: {}", md);
}

#[test]
fn test_double_bracket() {
    let pod = "=head2 test\n\nC<< $hash->{key} >>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("`$hash->{key}`"), "got: {}", md);
}

#[test]
fn test_link() {
    let pod = "=head2 test\n\nSee L<Foo::Bar>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(
        md.contains("[Foo::Bar](https://metacpan.org/pod/Foo::Bar)"),
        "got: {}",
        md
    );
}

#[test]
fn test_link_with_text() {
    let pod = "=head2 test\n\nSee L<the docs|http://example.com>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("[the docs](http://example.com)"), "got: {}", md);
}

#[test]
fn test_link_section() {
    let pod = "=head2 test\n\nSee L<Module/method>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("Module (method)"), "got: {}", md);
}

#[test]
fn test_escape() {
    let pod = "=head2 test\n\nE<lt>tag E<gt>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("<tag >"), "got: {}", md);
}

#[test]
fn test_verbatim_block() {
    let pod =
        "=head2 example\n\nSome text:\n\n    my $x = 1;\n    my $y = 2;\n\nMore text.\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(
        md.contains("```perl\nmy $x = 1;\nmy $y = 2;\n```"),
        "got: {}",
        md
    );
}

#[test]
fn test_item_list() {
    let pod = "=over\n\n=item * first\n\n=item * second\n\n=back\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("- first"), "got: {}", md);
    assert!(md.contains("- second"), "got: {}", md);
}

#[test]
fn test_item_label() {
    let pod = "=over\n\n=item ensure\n\npresent or absent\n\n=back\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("- **ensure**"), "got: {}", md);
}

#[test]
fn test_truncation() {
    let long_pod = format!("=head2 foo\n\n{}\n\n=cut\n", "x".repeat(3000));
    let md = pod_to_markdown(&long_pod);
    assert!(md.len() <= 2000);
}

#[test]
fn test_head2_section_extraction() {
    let pod = "=head1 METHODS\n\n=head2 path($file)\n\nCreate a path.\n\nReturns a path object.\n\n=head2 other\n\nOther stuff.\n\n=cut\n";
    let section = extract_head2_section("path", pod).unwrap();
    assert!(section.contains("Create a path."), "got: {}", section);
    assert!(
        section.contains("Returns a path object."),
        "got: {}",
        section
    );
    assert!(!section.contains("Other stuff."), "got: {}", section);
}

#[test]
fn test_file_formatting() {
    let pod = "=head2 test\n\nSee F</etc/config>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("`/etc/config`"), "got: {}", md);
}

#[test]
fn test_item_arrow_method() {
    let pod = "=over\n\n=item $mech->get($url)\n\nPerforms a GET request.\n\n=item $mech->post($url)\n\nPerforms a POST.\n\n=back\n";
    let section = extract_item_section("get", pod).unwrap();
    assert!(
        section.contains("Performs a GET request."),
        "got: {}",
        section
    );
    assert!(!section.contains("Performs a POST."), "got: {}", section);
}

#[test]
fn test_item_bare_name() {
    let pod = "=over\n\n=item connect\n\nConnects to server.\n\n=item disconnect\n\nDisconnects.\n\n=back\n";
    let section = extract_item_section("connect", pod).unwrap();
    assert!(section.contains("Connects to server."), "got: {}", section);
}

#[test]
fn test_item_class_method() {
    let pod = "=over\n\n=item DBI->connect($dsn)\n\nCreates a connection.\n\n=back\n";
    let section = extract_item_section("connect", pod).unwrap();
    assert!(
        section.contains("Creates a connection."),
        "got: {}",
        section
    );
}

#[test]
fn test_item_formatted_name() {
    let pod = "=over\n\n=item C<new>\n\nConstructor.\n\n=back\n";
    let section = extract_item_section("new", pod).unwrap();
    assert!(section.contains("Constructor."), "got: {}", section);
}

#[test]
fn test_item_no_match() {
    let pod = "=over\n\n=item $obj->foo()\n\nDoes foo.\n\n=back\n";
    assert!(extract_item_section("bar", pod).is_none());
}

#[test]
fn test_head2_preferred_over_item() {
    let pod =
        "=head2 path\n\nHead2 doc.\n\n=over\n\n=item $obj->path()\n\nItem doc.\n\n=back\n=cut\n";
    let section = extract_head2_section("path", pod).unwrap();
    assert!(section.contains("Head2 doc."), "got: {}", section);
}

// ---- New edge case tests ----

#[test]
fn test_nested_lists() {
    let pod = "=over\n\n=item * Outer\n\n=over\n\n=item * Inner\n\n=back\n\n=back\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("- Outer"), "got: {}", md);
    assert!(md.contains("- Inner"), "got: {}", md);
}

#[test]
fn test_begin_end_data_region() {
    let pod = "=begin html\n\n<table><tr><td>content</td></tr></table>\n\n=end html\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(
        md.contains("```html"),
        "should have fenced block, got: {}",
        md
    );
    assert!(md.contains("<table>"), "got: {}", md);
}

#[test]
fn test_bold_italic_nesting() {
    let pod = "=head2 test\n\nB<I<important note>>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("***important note***"), "got: {}", md);
}

#[test]
fn test_triple_angle_bracket() {
    let pod = "=head2 test\n\nC<<<  $hash->{key}  >>>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("`$hash->{key}`"), "got: {}", md);
}

#[test]
fn test_for_shorthand() {
    let pod = "=for html <img src=\"logo.png\">\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(
        md.contains("```html"),
        "should have fenced block, got: {}",
        md
    );
}

#[test]
fn test_deep_nested_formatting() {
    let pod = "=head2 test\n\nB<I<C<foo>>>\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("***`foo`***"), "got: {}", md);
}

#[test]
fn test_x_index_invisible() {
    let pod = "=head2 test\n\nSee X<index>this\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(
        md.contains("See this"),
        "X<> should be invisible, got: {}",
        md
    );
    assert!(!md.contains("index"), "got: {}", md);
}

#[test]
fn test_z_zero_width() {
    let pod = "=head2 test\n\naZ<>b\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("ab"), "Z<> should produce nothing, got: {}", md);
}

#[test]
fn test_ordered_list() {
    let pod = "=over\n\n=item 1. First\n\n=item 2. Second\n\n=back\n\n=cut\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("1. First"), "got: {}", md);
    assert!(md.contains("2. Second"), "got: {}", md);
    assert!(
        !md.contains("- **1."),
        "should not bold-wrap ordered items, got: {}",
        md
    );
}

#[test]
fn test_consecutive_verbatim_merged() {
    let pod =
        "=head2 test\n\n    block one\n    line two\n\n    block two\n    line three\n\n=cut\n";
    let md = pod_to_markdown(pod);
    // Should be one code block, not two
    let fence_count = md.matches("```").count();
    assert_eq!(
        fence_count, 2,
        "should have exactly one code block (2 fences), got: {}",
        md
    );
    assert!(md.contains("block one"), "got: {}", md);
    assert!(md.contains("block two"), "got: {}", md);
}

#[test]
fn test_eof_without_cut() {
    let pod = "=head1 NAME\n\nModule - description\n";
    let md = pod_to_markdown(pod);
    assert!(md.contains("### NAME"), "got: {}", md);
    assert!(md.contains("Module - description"), "got: {}", md);
}
