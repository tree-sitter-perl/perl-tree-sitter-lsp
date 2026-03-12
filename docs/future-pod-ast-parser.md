# Future: tree-sitter-pod Based POD Parsing

**Status:** Deferred — blocked on tree-sitter-pod release
**Motivation:** PerlNavigator PR #142 (Aequitosh) rewrites their POD parser as a three-phase pipeline (raw parse → structured AST → markdown). Our current `pod.rs` is a single-pass line-by-line converter that handles ~80% of POD but breaks on structural nesting.

## Why tree-sitter-pod

We already have `tree-sitter-pod` as a sibling repo (`../tree-sitter-pod`). Once released, it gives us a proper AST for free — no need to hand-write a parser. The converter becomes a tree walk over typed nodes rather than regex/line matching.

tree-sitter-perl already parses POD as opaque `pod` blobs. We'd sub-parse those blobs with tree-sitter-pod to get structured nodes.

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

## Reference

- PerlNavigator PR #142: https://github.com/bscan/PerlNavigator/pull/142
- tree-sitter-pod repo: `../tree-sitter-pod`
- POD spec: https://perldoc.perl.org/perlpodspec
