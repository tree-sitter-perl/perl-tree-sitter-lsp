# ADR: Pack-language completion — in-scope symbols + sentinel-reparse member access

Completion has two halves, and they split cleanly along the
language-agnostic / cursor-context line. The Perl path (`cursor_context.rs`,
rule #6) answers both at once with a Perl-specific tree walk. Pack
languages (the query-driven tier) get them as two independent pieces, so
no Perl semantics leak in.

## Half 1: in-scope symbols — no cursor context

`symbols::in_scope_completion(analysis, point)` lists every symbol visible
from the cursor: top-level defs (functions/classes/packages, globally
addressable) plus the locals/params/members whose declaring scope is on
the cursor's `scope_chain`. The client filters by the typed prefix — names
are stored verbatim (sigils and all), so `$sn` → `$sner`/`$snerful` and
`box.`'s prefix matching are the editor's job, not ours. This is pure
`FileAnalysis` (`scope_at` → `scope_chain` → `symbols`); zero per-language
code, no tree.

It is the fallback whenever the cursor is not a member access.

## Half 2: member access — the crux is the erased operator

At the instant a user triggers completion the buffer reads `box.` /
`box->` / `obj.` — a member access with **no member**. tree-sitter
produces an `ERROR` and the typed `field_expression` / `attribute` node
the rest of the engine speaks never forms; `box.` and `box->` parse to a
byte-identical `(ERROR (identifier))`, so even `.` vs `->` is gone. A query
can't match a node the parser never built, and a node-walk has nothing to
climb. Every production server normalizes the buffer instead (clangd's
completion token, rust-analyzer's dummy-identifier insertion, pyright's
recovery parser) — none read the broken tree.

### The seam: sentinel reparse (`cursor_sentinel.rs`)

A member of the reparse family (`cpp_reparse.rs`, `reparse.rs`): a source
edit + reparse + span remap. The others fix a parse corrupted by a
*declaration* (a macro, a prototype); this one fixes a parse corrupted by
*incompleteness*. Splice a placeholder identifier (`__CURSOR__`) at the
cursor so the access becomes syntactically complete, reparse, find the
placeholder, and take its member node's receiver (`named_child(0)`).

```rust
pub fn receiver_at_incremental(parser, cfg, src, old_tree, cursor) -> Option<Receiver>
pub struct Receiver { text: String, start: usize, end: usize, arrow: bool }
```

Two properties make it cheap and exact:

- **Free anchor.** The splice lands *at* the cursor, strictly after the
  receiver, so every receiver byte offset is identical in patched and
  original source. The `SpliceMap`/`AnchorMap` the other reparse siblings
  must carry, this one gets for free — no remap.
- **Incremental.** The splice is a pure insertion, so the `InputEdit` is
  exact: tree-sitter reparses only the damaged region around the cursor,
  reusing the document tree `document.rs` already holds.

Per-language config is a two-field table (`LangCfg { member_kinds,
skip_kinds }`), not a branch — the member-access node kinds and the
"don't splice into strings/comments" set are the only facts that vary.
`lang_cfg(language)` maps a driver id to its `LangCfg`; `None` means the
language gets in-scope completion only.

### Receiver → members: tree-free, reusing the bag

The backend owns the tree work (it has the parser + cached tree); once it
has the receiver span it hands off to the language-agnostic half:

```
sentinel receiver span
  → expr_type_at_span(span)            // the bag, via the @expr.read.var witness
  → InferredType::class_name()
  → complete_members_for_class(class)  // ancestor-walk methods + data fields
```

`complete_members_for_class` is the pack sibling of
`complete_methods_for_class`: methods come from the shared ancestor walk
(inheritance for free), data fields are the class's `Variable`/`Field`
symbols, and no `new` is synthesized — member access lists real members.
Members resolve to a class because the cpp pack's `@context.class` tags
class-body symbols with the class name (`symbol_in_class` reads `package`).

## The protocol gate

`completionProvider.triggerCharacters` is the union of every served
language's `LanguageDriver::trigger_chars()` (cpp adds `.`; `->`/`::` ride
the existing `>`/`:`). The client auto-fires completion on the trigger and
names it in `CompletionContext` — a free "is-this-member-access" signal at
one slice-literal per language. It is a gate, not a solution: it never
carries the receiver, which is exactly what the sentinel exists to
recover.

## Why this shape

The five candidates (query `.scm` vocabulary, generic node-walk, lexical
text scan, sentinel reparse, protocol signal) were spiked in parallel. The
erased-operator crux eliminates the two tree-reading approaches on
incomplete input and demotes the lexical scan to a lossy heuristic we
declined to make load-bearing. Sentinel reparse is the one that *removes*
the damage rather than interpreting it, it reuses a seam the engine
already has, and its receiver→type→members tail is the same bag +
inheritance walk Perl uses. The protocol signal rides on top as the cheap
gate.

Deferred: the CLI/`--batch` completion arm still does in-scope only (member
completion is backend-wired); inheritance across namespaces (qualified
class names); chained-receiver typing (`a.b.c.`).
