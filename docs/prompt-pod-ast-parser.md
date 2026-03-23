# tree-sitter-pod Based POD Parsing

**Status:** Active — `ts-parser-pod` now published on crates.io
**Motivation:** PerlNavigator PR #142 (Aequitosh) rewrites their POD parser as a three-phase pipeline (raw parse → structured AST → markdown). Our current `pod.rs` is a single-pass line-by-line converter that handles ~80% of POD but breaks on structural nesting.

## Why tree-sitter-pod

`ts-parser-pod` is live on crates.io. This gives us a proper AST for free — no need to hand-write a parser. The converter becomes a tree walk over typed nodes rather than regex/line matching.

tree-sitter-perl already parses POD as opaque `pod` blobs. We sub-parse those blobs with tree-sitter-pod to get structured nodes.

## Dependency

```toml
[dependencies]
ts-parser-pod = "X.Y.Z"  # crates.io
```

## Edge cases our current `pod.rs` doesn't handle

Discovered via PerlNavigator PR #142 and real-world testing:

### Nested lists
```pod
=over

=item * Outer item

=over

=item * Inner item

=back

=back
```
Our `in_list` is a boolean, not a depth counter. Nested `=over`/`=back` produces broken output.

### Data regions
```pod
=begin html

<table><tr><td>HTML content</td></tr></table>

=end html
```
We skip `=begin`/`=end` entirely. Should render as fenced code blocks with format name.

### =item-based method documentation
```pod
=over

=item $obj->method_name()

Does something useful.

=item $obj->other_method()

Does something else.

=back
```
Extremely common in OOP modules (`WWW::Mechanize`, `DBI`, `LWP::UserAgent`). Our `extract_head2_section` only matches `=head2`, completely missing `=item`-documented methods.

### Bold-italic nesting
```pod
B<I<important note>>
```
Should render as `***important note***`. Our converter handles them independently, producing `**<em>important note</em>**` or similar broken output.

### Multi-angle-bracket variants
```pod
C<<<  $hash->{key}  >>>
```
We handle `C<< ... >>` (double) but not triple or higher. The POD spec allows any number of angle brackets.

### =item text on next line
```pod
=item *
First item text here

=item *
Second item text here
```
PerlNavigator needed backtracking in the parser for this. Our converter handles `=item * text` on the same line but may mishandle text on the next line.

### EOF without =cut
```pod
=head1 NAME

Module - description
```
Valid POD (spec allows omitting `=cut` at EOF). We handle this but it's worth noting.

### =for shorthand
```pod
=for html <img src="logo.png">
```
Single-paragraph data command. We skip it. Should render appropriately based on format name.

### Header hierarchy in symbol lookup
```pod
=head2 connect

Connects to the server.

=head3 Options

=over

=item timeout

=item retries

=back

=head3 Examples

    $obj->connect(timeout => 30);

=head2 disconnect
```
Looking up `connect` should include everything from `=head2 connect` through the `=head3` subsections until `=head2 disconnect`. Our current extraction stops correctly at the next `=head2`, but a tree-sitter-pod AST would make the hierarchy explicit.

## Architecture when we pivot

```
tree-sitter-perl tree    →  pod node (opaque blob)
                               ↓
                          tree-sitter-pod   →  POD AST (headings, items, verbatim, data blocks)
                               ↓
                          pod_to_markdown() →  walks AST nodes, renders markdown
```

The builder would sub-parse `pod` nodes during the walk (or as a post-pass), producing structured doc attached to `SymbolDetail::Sub`. The markdown converter becomes a tree walk instead of line-by-line regex.

## Implementation approach

### Phase 1: Replace `pod.rs` line-by-line converter

Current `pod.rs` (~300 lines) is a line-by-line regex converter. Replace with:

```rust
fn pod_to_markdown(pod_text: &str) -> String {
    let mut parser = Parser::new();
    parser.set_language(&ts_parser_pod::LANGUAGE.into()).unwrap();
    let tree = parser.parse(pod_text, None).unwrap();
    render_pod_node(tree.root_node(), pod_text.as_bytes())
}
```

`render_pod_node` walks the tree-sitter-pod AST recursively, rendering each node type to markdown. This fixes all the edge cases listed above in one shot — nesting, data regions, multi-angle-brackets, etc.

### Phase 2: Structured doc extraction for hover

Currently `extract_head2_section` does string matching to find the POD section for a function name. With the AST, we can:

1. Walk `=head2` and `=item` nodes
2. Match function names against heading text
3. Extract the subtree rooted at that heading (including `=head3` subsections, `=over`/`=back` lists)
4. Render just that subtree to markdown

This replaces the fragile line-range extraction with precise tree-based extraction.

### Phase 3: POD-based symbol discovery

Some modules document methods only in POD, not as `sub` declarations (e.g., XS modules, Moose-generated methods). The POD AST can discover these:

```pod
=head2 connect

    $obj->connect(%options)

=head2 disconnect

    $obj->disconnect()
```

Parse `=head2` headings for method signatures, synthesize lightweight symbols for modules where we have no source code. Low priority — most modules have source.

### Integration with builder

The builder already visits `pod` nodes in tree-sitter-perl output. Currently it passes the raw text to `pod.rs`. Change to:

1. Extract `pod` node text
2. Sub-parse with tree-sitter-pod
3. Walk the POD AST for `=head2`/`=item` nodes → build function→doc mapping
4. Attach doc strings to `SymbolDetail::Sub` during the build

This keeps all tree-sitter access inside `build()`, preserving the architectural invariant.

## Files to modify

| File | Change |
|------|--------|
| `Cargo.toml` | Add `ts-parser-pod` dependency |
| `src/pod.rs` | Rewrite: tree walk over POD AST instead of line-by-line regex |
| `src/builder.rs` | Sub-parse `pod` nodes with tree-sitter-pod for structured doc extraction |

## Tests

All existing `pod::tests` should continue to pass (same output, different implementation). Add new tests for:
- Nested `=over`/`=back` lists
- `=begin`/`=end` data regions → fenced code blocks
- `B<I<nested>>` formatting
- `C<<<  $hash->{key}  >>>` multi-angle-brackets
- `=item $obj->method()` method doc extraction
- `=for html` shorthand
- EOF without `=cut`

## Reference

- PerlNavigator PR #142: https://github.com/bscan/PerlNavigator/pull/142
- `ts-parser-pod` on crates.io
- POD spec: https://perldoc.perl.org/perlpodspec
