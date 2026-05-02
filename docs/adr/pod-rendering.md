# ADR: POD rendering goes through a real AST

`src/pod.rs` converts raw POD blobs to GitHub-flavored markdown for hover. It
does this by **sub-parsing each POD blob with `ts-parser-pod` and walking the
AST** — not by line/regex matching.

## Decisions worth keeping

### AST-based extraction

`tree-sitter-perl` parses POD as opaque blobs. Inside `pod.rs`:

- `pod_to_markdown(pod_text)` parses with `ts-parser-pod`, walks the tree,
  emits markdown.
- `extract_head2_section(sub_name, pod_text)` and
  `extract_item_section(sub_name, pod_text)` walk the same AST to slice out
  per-sub doc, returning **rendered markdown** (not raw POD).

This rules out the alternative we explicitly rejected: line-by-line string
matching for `=head2`/`=item` boundaries. AST walking handles nested
`=over`/`=back`, multi-angle-bracket interior sequences (`C<<< $h->{k} >>>`),
deep nesting (`B<I<C<foo>>>`), `=begin`/`=end` data regions, and EOF without
`=cut` for free.

### Builder consumes rendered markdown — never re-render

`builder.rs::extract_pod_for_sub` and the tail-POD pass call
`extract_head2_section` / `extract_item_section` directly and store the result
as `SymbolDetail::Sub.doc`. **It must NOT call `pod_to_markdown` on the
result** — those extractors already returned markdown; re-rendering double-
escapes everything.

This is the most likely accident when extending the POD path. The extractors
are named for their POD inputs, but their outputs are markdown. Treat them as
"slice + render in one step."

### 2000-char truncation

`pod_to_markdown` truncates output at 2000 chars. Hover popups have a budget;
long module-level POD would otherwise overflow. Callers that need full POD
should walk the AST themselves.

## What's intentionally not here

- The interior-sequence rendering table (`C<>` → `` `…` ``, `B<>` → `**…**`,
  etc.) lives in `render_interior_sequence`. Read the code.
- Edge cases (multi-bracket, ordered list `=item 1.`, head2 vs item priority)
  are pinned in `src/pod_tests.rs`.
- Phase-3-style POD-based symbol discovery (synthesizing symbols from `=head2
  method_name` for XS / Moose-generated methods) was scoped out — most modules
  have source, the failure mode is a stale hover, not broken navigation.
