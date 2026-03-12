//! POD → Markdown converter.
//!
//! Regex-based line-by-line state machine. Converts raw POD text to
//! GitHub-flavored markdown for display in LSP hover popups.
//!
//! No tree-sitter imports — this module is pure string processing.
//! Tree-sitter node traversal for locating POD near subs lives in `builder.rs`.

use regex::Regex;
use std::sync::LazyLock;

/// Convert raw POD text to markdown. Caps output at ~2000 chars.
pub fn pod_to_markdown(pod_text: &str) -> String {
    let mut out = String::new();
    let mut in_verbatim = false;
    let mut in_over = false;

    for line in pod_text.lines() {
        // =cut — stop processing
        if line.starts_with("=cut") {
            if in_verbatim {
                out.push_str("```\n");
                in_verbatim = false;
            }
            break;
        }

        // Skip =pod, =encoding, =begin, =end, =for
        if line.starts_with("=pod") || line.starts_with("=encoding") {
            continue;
        }
        if line.starts_with("=begin") || line.starts_with("=end") || line.starts_with("=for") {
            continue;
        }

        // Headings
        if line.starts_with("=head1 ") {
            close_verbatim(&mut out, &mut in_verbatim);
            let title = &line[7..];
            out.push_str(&format!("### {}\n\n", convert_inline(title)));
            continue;
        }
        if line.starts_with("=head2 ") {
            close_verbatim(&mut out, &mut in_verbatim);
            let title = &line[7..];
            out.push_str(&format!("#### {}\n\n", convert_inline(title)));
            continue;
        }
        if line.starts_with("=head3 ") || line.starts_with("=head4 ") {
            close_verbatim(&mut out, &mut in_verbatim);
            let title = &line[7..];
            out.push_str(&format!("##### {}\n\n", convert_inline(title)));
            continue;
        }

        // =over / =back — list context
        if line.starts_with("=over") {
            close_verbatim(&mut out, &mut in_verbatim);
            in_over = true;
            continue;
        }
        if line.starts_with("=back") {
            close_verbatim(&mut out, &mut in_verbatim);
            in_over = false;
            out.push('\n');
            continue;
        }

        // =item
        if line.starts_with("=item") {
            close_verbatim(&mut out, &mut in_verbatim);
            let rest = line[5..].trim();
            if rest == "*" || rest.is_empty() {
                out.push_str("- ");
            } else if rest.starts_with("* ") {
                out.push_str(&format!("- {}\n", convert_inline(&rest[2..])));
            } else {
                out.push_str(&format!("- **{}**\n", convert_inline(rest)));
            }
            continue;
        }

        // Verbatim blocks (indented by 4+ spaces or tab)
        if line.starts_with("    ") || line.starts_with('\t') {
            if !in_verbatim {
                out.push_str("```\n");
                in_verbatim = true;
            }
            // Strip leading indent
            let content = if line.starts_with('\t') { &line[1..] } else { &line[4..] };
            out.push_str(content);
            out.push('\n');
            continue;
        }

        // Normal text — close verbatim if open
        close_verbatim(&mut out, &mut in_verbatim);

        if line.is_empty() {
            // Don't double-newline
            if !out.ends_with("\n\n") {
                out.push('\n');
            }
            continue;
        }

        // In a list context, continuation lines are part of the item
        let converted = convert_inline(line);
        out.push_str(&converted);
        out.push('\n');

        if out.len() > 2000 {
            break;
        }
    }

    close_verbatim(&mut out, &mut in_verbatim);
    let _ = in_over;

    // Trim trailing whitespace
    let result = out.trim_end().to_string();
    if result.len() > 2000 {
        result[..2000].to_string()
    } else {
        result
    }
}

fn close_verbatim(out: &mut String, in_verbatim: &mut bool) {
    if *in_verbatim {
        out.push_str("```\n");
        *in_verbatim = false;
    }
}

/// Convert POD inline formatting sequences to markdown.
fn convert_inline(text: &str) -> String {
    static RE_INLINE: LazyLock<Regex> = LazyLock::new(|| {
        // Match C<...>, B<...>, I<...>, L<...>, F<...>, E<...>
        // Also C<< ... >> double-bracket form
        Regex::new(r"([CBILFE])(<<\s+(.+?)\s+>>|<([^>]*)>)").unwrap()
    });

    RE_INLINE.replace_all(text, |caps: &regex::Captures| {
        let code = caps.get(1).unwrap().as_str();
        let content = caps.get(3).or_else(|| caps.get(4)).map(|m| m.as_str()).unwrap_or("");
        match code {
            "C" => format!("`{}`", content),
            "B" => format!("**{}**", content),
            "I" => format!("*{}*", content),
            "F" => format!("`{}`", content),
            "L" => {
                // L<text|url> → text, L<Module::Name> → Module::Name
                if let Some(idx) = content.find('|') {
                    content[..idx].to_string()
                } else {
                    content.to_string()
                }
            }
            "E" => match content {
                "lt" => "<".to_string(),
                "gt" => ">".to_string(),
                "sol" => "/".to_string(),
                "verbar" => "|".to_string(),
                _ => content.to_string(),
            },
            _ => content.to_string(),
        }
    }).to_string()
}

/// Extract a =head2 section for a given sub name from a POD block.
/// Used by the builder's tail-POD post-pass.
pub fn extract_head2_section(sub_name: &str, pod_text: &str) -> Option<String> {
    let mut collecting = false;
    let mut section = String::new();

    for line in pod_text.lines() {
        if collecting {
            // Stop at next =head at same or higher level, or =cut
            if line.starts_with("=head1 ") || line.starts_with("=head2 ") || line.starts_with("=cut") {
                break;
            }
            section.push_str(line);
            section.push('\n');
        } else {
            // Look for =head2 matching sub name
            if line.starts_with("=head2 ") {
                let rest = &line[7..];
                // Match bare name or name with parens: "path" or "path($file)"
                let head_name = rest.split(|c: char| c == '(' || c.is_whitespace()).next().unwrap_or("");
                if head_name == sub_name {
                    // Don't include the =head2 line itself (hover already shows the signature)
                    collecting = true;
                }
            }
        }
    }

    if section.trim().is_empty() {
        None
    } else {
        Some(section)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_headings() {
        let pod = "=head1 NAME\n\nFoo - a foo module\n\n=head2 bar\n\nDoes bar things.\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("### NAME"));
        assert!(md.contains("#### bar"));
        assert!(md.contains("Does bar things."));
    }

    #[test]
    fn test_inline_formatting() {
        assert_eq!(convert_inline("Use C<foo> for B<bold> and I<italic>"),
            "Use `foo` for **bold** and *italic*");
    }

    #[test]
    fn test_double_bracket() {
        assert_eq!(convert_inline("C<< $hash->{key} >>"), "`$hash->{key}`");
    }

    #[test]
    fn test_link() {
        assert_eq!(convert_inline("See L<Foo::Bar>"), "See Foo::Bar");
        assert_eq!(convert_inline("See L<docs|http://example.com>"), "See docs");
    }

    #[test]
    fn test_escape() {
        assert_eq!(convert_inline("E<lt>tag E<gt>"), "<tag >");
    }

    #[test]
    fn test_verbatim_block() {
        let pod = "=head2 example\n\nSome text:\n\n    my $x = 1;\n    my $y = 2;\n\nMore text.\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("```\nmy $x = 1;\nmy $y = 2;\n```"));
    }

    #[test]
    fn test_item_list() {
        let pod = "=over\n\n=item * first\n\n=item * second\n\n=back\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("- first"));
        assert!(md.contains("- second"));
    }

    #[test]
    fn test_item_label() {
        let pod = "=over\n\n=item ensure\n\npresent or absent\n\n=back\n\n=cut\n";
        let md = pod_to_markdown(pod);
        assert!(md.contains("- **ensure**"));
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
        assert!(section.contains("Create a path."));
        assert!(section.contains("Returns a path object."));
        assert!(!section.contains("Other stuff."));
    }

    #[test]
    fn test_file_formatting() {
        assert_eq!(convert_inline("See F</etc/config>"), "See `/etc/config`");
    }
}
