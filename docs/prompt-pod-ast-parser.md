# POD Converter: tree-sitter-pod AST Architecture

**Status:** Implemented (Phases 1-2 complete)

## Overview

`pod.rs` converts raw POD text to GitHub-flavored markdown for LSP hover popups. It uses `ts-parser-pod` (crates.io) to parse POD into a proper AST, then walks the tree to render markdown.

## Dependency

```toml
[dependencies]
ts-parser-pod = "1"
```

## Architecture

```
tree-sitter-perl tree  →  pod node (opaque text blob)
                                ↓
                           ts-parser-pod parser  →  POD AST
                                ↓
                           render_node() tree walk  →  markdown string
```

tree-sitter-perl parses POD as opaque `pod` blobs. `pod.rs` sub-parses those blobs with tree-sitter-pod to get structured nodes (`command_paragraph`, `plain_paragraph`, `verbatim_paragraph`, `begin_paragraph`, `for_paragraph`, `interior_sequence`).

### Rendering pipeline

- `pod_to_markdown(pod_text)` — entry point: parse → walk → render → truncate at 2000 chars
- `render_children(node)` — walks named children, merges consecutive verbatim paragraphs into one fenced block
- `render_node(node)` — dispatches on node kind to specific renderers
- `render_command(node)` — handles `=head1`–`=head4`, `=over`/`=back`, `=item`, `=pod`, `=encoding`
- `render_plain(node)` — renders paragraph text with inline formatting
- `render_verbatim(node)` — renders as `` ```perl `` fenced code blocks, strips one indent level
- `render_begin(node)` / `render_for(node)` — renders `=begin`/`=end` and `=for` data as fenced blocks with format name
- `render_inline_content(node)` — walks `content` nodes, interleaving literal text with `interior_sequence` children
- `render_interior_sequence(node)` — handles all POD formatting codes recursively:

| Code | Rendering |
|------|-----------|
| `C<code>` | `` `code` `` |
| `B<bold>` | `**bold**` |
| `I<italic>` | `*italic*` |
| `F<file>` | `` `file` `` |
| `L<Module>` | `[Module](https://metacpan.org/pod/Module)` |
| `L<text\|url>` | `[text](url)` |
| `L<Module/section>` | `Module (section)` |
| `E<lt>`, `E<gt>` | `<`, `>` |
| `X<entry>` | (invisible — index metadata) |
| `Z<>` | (invisible — zero-width) |
| `S<text>` | non-breaking spaces |

Multi-angle-bracket variants (`C<< ... >>`, `C<<< ... >>>`) handled naturally by tree-sitter-pod — the content node is trimmed per the POD spec.

Nesting works recursively: `B<I<C<foo>>>` → `***`foo`***`.

### Section extraction for hover

- `extract_head2_section(sub_name, pod_text)` — finds `=head2` matching a sub name, renders everything until next `=head2`/`=head1`/`=cut`. Returns rendered markdown (not raw POD).
- `extract_item_section(sub_name, pod_text)` — finds `=item` matching a sub name (handles `$obj->method()`, `Class->method()`, `C<method>`, bare `method`), renders until next `=item`/`=back`/`=head`/`=cut`. Returns rendered markdown.

Both walk the tree-sitter-pod AST — no line-by-line string matching.

### Integration with builder

The builder's `extract_pod_for_sub` and tail-POD post-pass call `extract_head2_section` / `extract_item_section` directly. These return rendered markdown, which is stored in `SymbolDetail::Sub { doc }`. No double-conversion — the builder does NOT call `pod_to_markdown` on the result.

### Consecutive verbatim merging

POD splits code at blank lines into separate `verbatim_paragraph` nodes. `render_children` merges consecutive verbatim paragraphs into a single fenced block with a blank line between them, matching how developers expect code examples to render.

### Ordered list support

`=item 1. First` renders as `1. First` (ordered list) instead of `- **1. First**` (definition list). Detected by `strip_ordered_prefix` checking for `N.` prefix pattern.

## Edge cases handled

All from the original PerlNavigator PR #142 analysis:

| Edge case | How handled |
|-----------|-------------|
| Nested `=over`/`=back` | AST nesting — natural recursion |
| `=begin html`/`=end html` | Fenced code block with format name |
| `=for html <img>` | Fenced code block with format name |
| `B<I<nested>>` | Recursive `render_interior_sequence` |
| `C<<< $hash->{key} >>>` | tree-sitter-pod handles multi-angle-bracket, content trimmed |
| `=item $obj->method()` | `extract_item_method_name` parses method from item text |
| EOF without `=cut` | tree-sitter-pod handles gracefully |
| `=item` text on next line | AST separates command from content |
| Header hierarchy in lookup | `extract_head2_section` captures `=head3` subsections |

## Future: Phase 3 — POD-based symbol discovery

Some modules document methods only in POD, not as `sub` declarations (XS modules, Moose-generated methods). The POD AST could discover these by parsing `=head2` headings for method signatures, synthesizing lightweight symbols. Low priority — most modules have source.

## Tests

32 tests covering:
- Basic headings, inline formatting, verbatim blocks, item lists
- All interior sequences: `C<>`, `B<>`, `I<>`, `F<>`, `L<>`, `E<>`, `X<>`, `Z<>`, `S<>`
- Multi-angle-bracket (`C<< >>`, `C<<< >>>`)
- Deep nesting (`B<I<C<foo>>>`)
- Nested lists, ordered lists
- `=begin`/`=end` data regions, `=for` shorthand
- Consecutive verbatim merging
- `L<>` markdown links (module, URL, text|url, module/section)
- Section extraction (`extract_head2_section`, `extract_item_section`)
- EOF without `=cut`
- Head2 preferred over item when both match
