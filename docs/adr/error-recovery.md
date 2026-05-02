# ADR: Two-layer error recovery

During typing the document is almost always invalid. Two failure modes
destroy the structural skeleton (packages, imports, subs, classes), and a
single defense doesn't cover both.

## The two failure modes

### ERROR nodes swallow adjacent declarations

Typing `reduce` on a blank line creates an ERROR node that absorbs the next
`package MojoApp;` statement. Tree-sitter wraps the broken region; the
declarations are still children of the ERROR.

### Misparsing without ERROR nodes

```perl
$self->

sub hiThisWasDefinedBefore () {
}
```

Tree-sitter parses `sub` as a method name (invocant `$self`, method `sub`).
The whole sub declaration becomes a method call expression. **No ERROR node**
— the tree is structurally valid but semantically wrong.

## Decisions worth keeping

### Two layers, primary + insurance

**Layer 2 — ERROR-node recovery** (`builder.rs::recover_structural_from_error`)
is the **primary** defense. When the builder hits an ERROR, it recurses into
its children looking for `package_statement`, `use_statement`,
`subroutine_declaration_statement`, `method_declaration_statement`,
`class_statement`, and nested ERROR nodes. The structural elements survive
into `FileAnalysis` as real symbols — every downstream feature works.

**Layer 1 — Stable outline on `Document`** (`document.rs::StableOutline`) is
**insurance** for misparse-without-ERROR (failure mode 2). Per-Document
lightweight skeleton (packages / imports / subs / classes by name + line)
that survives parse degradation.

Layer 2 alone can't catch failure mode 2 (no ERROR node to recurse into).
Layer 1 alone is brittle (stale entries linger). Together they cover the
matrix:

| Scenario | Layer 1 | Layer 2 | Result |
|---|---|---|---|
| Normal typing, no loss | updates from current parse | not needed | correct |
| ERROR swallows `package` | keeps old entry | recovers from ERROR | both work; L2 primary |
| `$self->` eats `sub` decl | keeps old entry | can't help (no ERROR) | L1 saves it |
| User legitimately deletes a package | source-text validation drops it | n/a | correct |
| Mid-edit retyping `package Fo` | source-text check drops stale entry | n/a | correct |

### Stable-outline update rule: max-counts wins, source-text validates

After every parse, compare each category's count vs the stable outline:

- Current parse has ≥ the stored count → **update** from current parse.
- Current parse has fewer → **keep** the stored entries, but re-validate each
  against the source text. If the line at the stored row no longer starts
  with `package`/`use`/`sub`, drop it.

Source-text validation is what distinguishes "user is typing nearby and
tree-sitter misparsed" (line still says `package MojoApp;` — keep) from
"user deleted the package" (line now says something else — drop). Without
it, stable entries linger forever.

### Line-number drift is acceptable

When the user inserts/deletes lines, stored line numbers shift. Source-text
validation fixes this implicitly — we re-check the line range and
near-misses still validate. Approximate line numbers are good enough for
range queries (`find_use_insertion_position` cares which package range
contains the cursor, not exact rows). Don't add edit-tracking complexity for
this.

## Where this lives

- `src/builder.rs` — `recover_structural_from_error`, dispatched on `ERROR`
  node kind in the main visit loop.
- `src/document.rs` — `StableOutline` struct, update logic in
  `Document::update`.
- Source-text validation reads `Document.text` directly; no separate
  invalidation channel needed.

## Cross-ref

Upstream parser improvements that would shrink Layer 1's load (preserve
`sub`/`use` inside ERROR, prevent `package` from being eaten as an argument)
are tracked in `docs/prompt-parser-error-recovery-gaps.md`. They're not
blockers — Layer 1 is the right insurance to keep regardless.
