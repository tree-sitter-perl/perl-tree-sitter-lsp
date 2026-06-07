# tree-sitter-perl shortcomings (parser-team hand-off)

Grammar/lexer gaps the QA sweeps surfaced. These are **upstream parser**
bugs, not LSP builder bugs — they corrupt the CST before `build()` ever
sees it, so no amount of recovery in `builder.rs` fully repairs them
(the structural-recovery layer in `docs/adr/error-recovery.md` only
salvages declarations that survive *as ERROR children*; a token-stream
bleed or a whole-file ERROR wrap loses them entirely).

For each: minimal repro, expected vs actual tree, downstream impact.
Inspect any snippet with `perl-lsp --parse <file>` (or `--` for stdin).

Parser version at time of writing: **ts-parser-perl 1.1.1**.

1.1.0 was a large fix release: the `not` prefix-operator gap
(tree-sitter-perl#230), the `$#name`-in-string / empty-heredoc /
bareword-filehandle / `-t FH` / `"${@}"` / v-string / bareword-`&&` /
symbolic-code-deref families, and the `s{}{}` serialize-buffer abort all
landed upstream.

1.1.1 (no node-schema changes) added braced var declarations (`my ${foo}` —
the **R1** regression below, now fixed), bare dotted versions (`use 5.14.0`),
sub/method forward declarations (`sub NAME;`), glued `x`-repetition (`"ab"x3`),
punctuation-var deref-casts, and arrow-chained subscripts in strings; it also
fixed list-op `{…}` parsing as a hashref argument (`bless {@_}, $class`) — which
flipped `bless {…}, ref $_[0]` from a mis-parsed indirect-object block to a
correct two-arg call, so the builder now resolves the clone idiom's class from
`ref EXPR` directly (`bless_class_of`). Only the items below remain.

---

## G2 — top-level bare `{ ... }` block wrapping a whole package

The "non-indenting brace" idiom (perltidy emits `{ #<<< ... }` to contain
a package's lexicals without indenting the body) puts the *entire* package
implementation inside a file-scope bare block that is a **sibling** of the
`package` statement:

```perl
package Perl::Tidy::Formatter;
{ #<<< A non-indenting brace to contain all lexical variables
    use Carp;
    our $VERSION = '...';
    sub AUTOLOAD { ... }
    # ... ~39,000 lines, dozens of subs ...
} ## end package Perl::Tidy::Formatter
1;
```

**Actual** — on the real 39,305-line `Perl/Tidy/Formatter.pm` the parse is
a single root ERROR spanning the **whole file**:
```
(source_file
  (ERROR [0, 0] - [39305, 0]
    (comment ...) (comment ...) ...))
```
509 ERROR nodes total. A small, balanced version of the same shape parses
*cleanly* as `block_statement` — so the root wrap is content-dependent: one
unrecoverable error among the thousands of lines *inside* the block bubbles
all the way out, because the file-scope `{...}` is the recovery boundary.
The block's "is this a hash-ref expression or a block?" ambiguity makes the
parser unable to re-sync at an inner statement boundary; the failure
propagates to the block's extent, which is the file.

**Expected** — a `block_statement` (or `package`-scoped block) whose body
holds the `use`/`our`/`sub` declarations as direct children, so structural
recovery can find them even when an inner sub fails to parse.

**Downstream impact** — **severe**. `perl-lsp --dump-package
Perl::Tidy::Formatter` reports *"Package not found"*: the package is indexed
as `main` (the `package` statement is inside the ERROR, detached from the
body), 31+ subs vanish from the symbol table, goto-def / references /
completion all dead for the file. This is the single highest-impact grammar
gap found — one idiom loses one of perltidy's largest modules wholesale.

---

## `} or next` — `or` after a bare block parsed as a function call

```perl
{
  something();
} or next;
```

**Actual** — the block closes, then `or next` is a **separate statement**
parsed as a function call named `or` taking `next` as an argument:
```
(block_statement (expression_statement (function_call_expression ...)))
(expression_statement
  (ambiguous_function_call_expression
    function: (function [2, 2] - [2, 4])      # "or"  <-- as a function
    arguments: (loopex_expression)))          # "next"
```
**Expected** — `{ ... } or EXPR` is a low-precedence logical expression
(`lowprec_logical_expression`), as it already is for the `do`-block form:
```perl
do { something() } or next;   # parses correctly today
```
**Downstream impact** — `or` flagged as an unresolved function. The fix is
to let a bare `block` be a logical-expression operand the same way
`do_expression` already is.

---

## Pointer

A one-line cross-ref lives in `docs/ROADMAP.md` under the upstream-parser
section. When G2 is fixed upstream, the matching QA-findings entry
(`docs/qa-findings.md` §G) can be closed and the structural-recovery load
it imposes (`docs/adr/error-recovery.md`) shrinks.
