//! POD → Markdown converter using tree-sitter-pod.
//!
//! Sub-parses POD text with tree-sitter-pod to get a proper AST,
//! then walks the tree to render markdown. Handles nested lists,
//! data regions, multi-angle-bracket formatting, and proper inline nesting.

use tree_sitter::{Node, Parser};

/// Convert raw POD text to markdown. Caps output at ~2000 chars.
pub fn pod_to_markdown(pod_text: &str) -> String {
    let tree = match parse_pod(pod_text) {
        Some(t) => t,
        None => return String::new(),
    };
    let mut out = String::new();
    render_children(tree.root_node(), pod_text.as_bytes(), &mut out);

    let result = out.trim_end().to_string();
    if result.len() > 2000 {
        result[..2000].to_string()
    } else {
        result
    }
}

/// Extract a =head2 section for a given sub name from a POD block.
pub fn extract_head2_section(sub_name: &str, pod_text: &str) -> Option<String> {
    let tree = parse_pod(pod_text)?;
    let bytes = pod_text.as_bytes();
    let root = tree.root_node();

    let mut collecting = false;
    let mut section = String::new();

    for i in 0..root.named_child_count() {
        let child = root.named_child(i)?;
        if child.kind() == "command_paragraph" {
            let cmd = get_command_name(&child, bytes);
            if cmd == "=head2" {
                if collecting {
                    break; // next =head2 ends the section
                }
                let content = get_content_text(&child, bytes);
                let head_name = content.split(|c: char| c == '(' || c.is_whitespace()).next().unwrap_or("");
                if head_name == sub_name {
                    collecting = true;
                    continue; // don't include the =head2 line itself
                }
            } else if collecting && (cmd == "=head1" || cmd == "=cut") {
                break;
            }
        }
        if collecting {
            render_node(child, bytes, &mut section);
        }
    }

    let result = section.trim().to_string();
    if result.is_empty() { None } else { Some(result) }
}

/// Extract documentation for a sub from =item blocks within =over/=back.
pub fn extract_item_section(sub_name: &str, pod_text: &str) -> Option<String> {
    let tree = parse_pod(pod_text)?;
    let bytes = pod_text.as_bytes();
    let root = tree.root_node();

    let mut collecting = false;
    let mut section = String::new();

    for i in 0..root.named_child_count() {
        let child = root.named_child(i)?;
        if child.kind() == "command_paragraph" {
            let cmd = get_command_name(&child, bytes);
            if cmd == "=item" {
                if collecting {
                    break; // next =item ends the section
                }
                let content = get_content_text(&child, bytes);
                if let Some(name) = extract_item_method_name(&content) {
                    if name == sub_name {
                        collecting = true;
                        continue;
                    }
                }
            } else if collecting && (cmd == "=back" || cmd == "=head1" || cmd == "=head2" || cmd == "=cut") {
                break;
            }
        }
        if collecting {
            render_node(child, bytes, &mut section);
        }
    }

    let result = section.trim().to_string();
    if result.is_empty() { None } else { Some(result) }
}

// ---- Parser ----

fn parse_pod(pod_text: &str) -> Option<tree_sitter::Tree> {
    let mut parser = Parser::new();
    parser.set_language(&ts_parser_pod::LANGUAGE.into()).ok()?;
    parser.parse(pod_text, None)
}

// ---- Rendering ----

fn render_children(node: Node, source: &[u8], out: &mut String) {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            render_node(child, source, out);
            if out.len() > 2000 { return; }
        }
    }
}

fn render_node(node: Node, source: &[u8], out: &mut String) {
    match node.kind() {
        "command_paragraph" => render_command(node, source, out),
        "plain_paragraph" => render_plain(node, source, out),
        "verbatim_paragraph" => render_verbatim(node, source, out),
        "begin_paragraph" => render_begin(node, source, out),
        "for_paragraph" => render_for(node, source, out),
        "cut_paragraph" => {} // skip
        _ => {}
    }
}

fn render_command(node: Node, source: &[u8], out: &mut String) {
    let cmd = get_command_name(&node, source);
    let content = get_content_text(&node, source);

    match cmd.as_str() {
        "=head1" => {
            out.push_str(&format!("### {}\n\n", render_inline_content(&node, source)));
        }
        "=head2" => {
            out.push_str(&format!("#### {}\n\n", render_inline_content(&node, source)));
        }
        "=head3" | "=head4" => {
            out.push_str(&format!("##### {}\n\n", render_inline_content(&node, source)));
        }
        "=over" => {} // list start — no output needed
        "=back" => {
            out.push('\n');
        }
        "=item" => {
            let rendered = render_inline_content(&node, source);
            if rendered.is_empty() || rendered == "*" {
                out.push_str("- ");
            } else if rendered.starts_with("* ") {
                out.push_str(&format!("- {}\n", &rendered[2..]));
            } else {
                out.push_str(&format!("- **{}**\n", rendered));
            }
        }
        "=pod" | "=encoding" => {} // skip
        _ => {} // unknown commands
    }
    let _ = content; // used via render_inline_content
}

fn render_plain(node: Node, source: &[u8], out: &mut String) {
    let text = render_inline_content(&node, source);
    if !text.is_empty() {
        out.push_str(&text);
        out.push('\n');
    }
    if !out.ends_with("\n\n") {
        out.push('\n');
    }
}

fn render_verbatim(node: Node, source: &[u8], out: &mut String) {
    out.push_str("```\n");
    // Get the verbatim content, stripping one level of indent (4 spaces or tab)
    if let Ok(text) = node.utf8_text(source) {
        for line in text.lines() {
            if line.starts_with("    ") {
                out.push_str(&line[4..]);
            } else if line.starts_with('\t') {
                out.push_str(&line[1..]);
            } else {
                out.push_str(line);
            }
            out.push('\n');
        }
    }
    out.push_str("```\n\n");
}

fn render_begin(node: Node, source: &[u8], out: &mut String) {
    let format_name = node.child_by_field_name("format")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("text");
    // Render data as fenced code block with format name
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "data" {
                if let Ok(text) = child.utf8_text(source) {
                    let text = text.trim();
                    if !text.is_empty() {
                        out.push_str(&format!("```{}\n{}\n```\n\n", format_name, text));
                    }
                }
            }
        }
    }
}

fn render_for(node: Node, source: &[u8], out: &mut String) {
    let format_name = node.child_by_field_name("format")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("text");
    let content = get_content_text(&node, source);
    if !content.is_empty() {
        out.push_str(&format!("```{}\n{}\n```\n\n", format_name, content));
    }
}

// ---- Inline content rendering ----

/// Render the content of a node, handling interior_sequence (B<>, C<>, etc.) recursively.
fn render_inline_content(node: &Node, source: &[u8]) -> String {
    // Find the content child node
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "content" {
                return render_content_node(child, source);
            }
        }
    }
    String::new()
}

fn render_content_node(node: Node, source: &[u8]) -> String {
    // If the content node has no named children, it's plain text
    if node.named_child_count() == 0 {
        return node.utf8_text(source).unwrap_or("").to_string();
    }

    // Walk children, interleaving literal text and interior_sequence nodes
    let mut result = String::new();
    let content_start = node.start_byte();
    let content_end = node.end_byte();
    let mut pos = content_start;

    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            // Literal text before this child
            if child.start_byte() > pos {
                if let Ok(text) = std::str::from_utf8(&source[pos..child.start_byte()]) {
                    result.push_str(text);
                }
            }
            if child.kind() == "interior_sequence" {
                result.push_str(&render_interior_sequence(child, source));
            }
            pos = child.end_byte();
        }
    }
    // Trailing literal text
    if pos < content_end {
        if let Ok(text) = std::str::from_utf8(&source[pos..content_end]) {
            result.push_str(text);
        }
    }

    result
}

fn render_interior_sequence(node: Node, source: &[u8]) -> String {
    let letter = node.child_by_field_name("letter")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("");

    // Get the content — may contain nested interior_sequences
    // Trim for multi-angle-bracket variants (C<< ... >> has extra spaces per POD spec)
    let content = node.named_children(&mut node.walk())
        .find(|c| c.kind() == "content")
        .map(|c| render_content_node(c, source).trim().to_string())
        .unwrap_or_default();

    match letter {
        "C" => format!("`{}`", content),
        "B" => format!("**{}**", content),
        "I" => format!("*{}*", content),
        "F" => format!("`{}`", content),
        "L" => {
            // L<text|url> → text, L<Module::Name> → Module::Name
            if let Some(idx) = content.find('|') {
                content[..idx].to_string()
            } else {
                content
            }
        }
        "E" => match content.as_str() {
            "lt" => "<".to_string(),
            "gt" => ">".to_string(),
            "sol" => "/".to_string(),
            "verbar" => "|".to_string(),
            _ => content,
        },
        _ => content,
    }
}

// ---- Helpers ----

fn get_command_name(node: &Node, source: &[u8]) -> String {
    node.child_by_field_name("command")
        .and_then(|n| n.utf8_text(source).ok())
        .map(|s| s.to_string())
        .unwrap_or_default()
}

fn get_content_text(node: &Node, source: &[u8]) -> String {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "content" {
                return child.utf8_text(source).unwrap_or("").to_string();
            }
        }
    }
    String::new()
}

/// Extract method name from an =item content string.
/// Handles: `$obj->method(...)`, `Class->method(...)`, `C<method>`, `method(...)`, `method`
fn extract_item_method_name(item_rest: &str) -> Option<&str> {
    let s = item_rest.trim();
    // Strip C<...> wrapper
    let s = if s.starts_with("C<") {
        let inner = s.strip_prefix("C<")?.strip_suffix('>')?;
        inner.trim()
    } else if s.starts_with("C<<") {
        let inner = s.strip_prefix("C<<")?.strip_suffix(">>")?;
        inner.trim()
    } else {
        s
    };
    // Strip $obj-> or Class::Name-> prefix
    let s = if let Some(idx) = s.find("->") {
        &s[idx + 2..]
    } else {
        s
    };
    // Strip trailing (...) and whitespace
    let s = if let Some(idx) = s.find('(') {
        s[..idx].trim()
    } else {
        s.trim()
    };
    if s.is_empty() { None } else { Some(s) }
}

#[cfg(test)]
mod tests {
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
        assert!(md.contains("Foo::Bar"), "got: {}", md);
    }

    #[test]
    fn test_escape() {
        let pod = "=head2 test\n\nE<lt>tag E<gt>\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("<tag >"), "got: {}", md);
    }

    #[test]
    fn test_verbatim_block() {
        let pod = "=head2 example\n\nSome text:\n\n    my $x = 1;\n    my $y = 2;\n\nMore text.\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("```\nmy $x = 1;\nmy $y = 2;\n```"), "got: {}", md);
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
        assert!(section.contains("Returns a path object."), "got: {}", section);
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
        assert!(section.contains("Performs a GET request."), "got: {}", section);
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
        assert!(section.contains("Creates a connection."), "got: {}", section);
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
        let pod = "=head2 path\n\nHead2 doc.\n\n=over\n\n=item $obj->path()\n\nItem doc.\n\n=back\n=cut\n";
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
        assert!(md.contains("```html"), "should have fenced block, got: {}", md);
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
        assert!(md.contains("```html"), "should have fenced block, got: {}", md);
    }

    #[test]
    fn test_eof_without_cut() {
        let pod = "=head1 NAME\n\nModule - description\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("### NAME"), "got: {}", md);
        assert!(md.contains("Module - description"), "got: {}", md);
    }
}
