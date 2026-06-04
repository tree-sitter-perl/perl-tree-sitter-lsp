# tree-sitter-perl shortcomings (parser-team hand-off)

Grammar/lexer gaps the QA sweeps surfaced. These are **upstream parser**
bugs, not LSP builder bugs — they corrupt the CST before `build()` ever
sees it, so no amount of recovery in `builder.rs` fully repairs them
(the structural-recovery layer in `docs/adr/error-recovery.md` only
salvages declarations that survive *as ERROR children*; a token-stream
bleed or a whole-file ERROR wrap loses them entirely).

For each: minimal repro, expected vs actual tree, downstream impact.
Inspect any snippet with `perl-lsp --parse <file>` (or `--` for stdin).
The already-filed `not` prefix-operator gap is **tree-sitter-perl#230** —
the items below are its siblings.

Parser version at time of writing: **ts-parser-perl 1.0.3**. Every repro
below was re-verified against it.

The recurring failure signature is **error contagion**: a single
unlexable token doesn't fail locally, it derails the lexer/parser for
the rest of the enclosing construct (often the rest of the file). Perl's
ambiguity (a bareword is a function until proven otherwise; `{` is a
block until proven a hash) means a mis-lex upstream cascades into
spurious `function_call_expression` nodes downstream — which the LSP then
reports as unresolved-function false positives. Containing the blast
radius (fail the *one* token, recover at the next statement boundary)
would help even where a full fix is hard.

---

## G1 — `$#_` / `$#array` interpolated in a double-quoted string

The `$#name` last-index sigil is not lexed inside string interpolation.
The `#` after `$` is treated as a comment-start and swallows to
end-of-line, derailing the rest of the string and the statements after it.

**Repro** (minimal):
```perl
my $x = "n=$#_";
my $y = 1;
```

**Actual** — the whole region collapses into a root-spanning ERROR; the
`#_"` and the following line are mis-lexed (note the `comment` node eating
into the next statement):
```
(source_file
  (ERROR [0, 0] - [2, 0]
    (variable_declaration (scalar (varname)))   # my $x
    (scalar (comment [0, 12] - [0, 16]) (varname [1, 0] - [1, 2]))   # "#_"\nmy
    (scalar (varname))))                          # $y
```

**Expected** — the interpolation contains an `arraylen` (the node
tree-sitter already produces for bareword `$#_`, see below), the string is
a normal `interpolated_string_literal`, and `my $y = 1;` is its own
statement. For comparison, **outside** a string `$#_` parses correctly:
```perl
my $n = $#_;   # => right: (arraylen (varname))   -- fine
```

**Real-world repro** — Bugzilla `Bugzilla/Chart.pm:36,42`:
```perl
die("CGI object not passed in - invalid number of args \($#_\)($_)");
```
The interpolated `\($#_\)` derails parsing; the resulting ERROR wraps the
enclosing `sub` and everything after it.

**Downstream impact** — the comment-swallow eats subsequent lines; string
*contents* later in the file leak into the token stream as code (a SQL
`"SELECT ..."` string surfaced as `unresolved-function 'SELECT'`). The
enclosing sub is lost from the symbol table.

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

## G3 — empty-delimiter heredoc `<<''` body bleeds into the token stream

A heredoc with an empty string delimiter (`<<''`, terminated by the next
blank line) is not recognized. The `<<` + `''` is mis-parsed and the
heredoc **body** is then lexed as ordinary Perl.

**Repro**:
```perl
my $sql = <<'';
SELECT * FROM users
WHERE id = ?

my $next = foo();
```

**Actual** — `<<` errors, `''` becomes an empty `string_literal`, and the
SQL body is parsed as code: `SELECT` → `function_call_expression`, the rest
→ `glob` / `assignment_expression` with nested ERRORs (3 ERROR nodes):
```
(assignment_expression
  left: (variable_declaration (scalar))   # my $sql
  (ERROR [0, 10] - [0, 12])               # <<
  right: (string_literal))                # ''
(ambiguous_function_call_expression
  function: (function [1,0]-[1,6])         # SELECT  <-- body-as-code
  arguments: (assignment_expression ...))  # FROM users WHERE ...
```

**Expected** — `<<''` recognized as a heredoc operator; the body
(everything up to the next empty line) consumed as `string_content`, not
re-lexed as statements.

**Real-world repro** — DBIx-Class (SQL heredocs). The leaked SQL keywords
surface as `unresolved-function 'SELECT'` etc. (same class of false
positive as G1's string bleed).

---

## GR-1 — v-string literal (`v5.6.0`) parsed as a function call

A version-string literal of the form `vN.N.N` is not lexed as a v-string.
The leading `vN` is taken as a bareword function name and the trailing
`.N.N` becomes a concatenation expression handed to it as arguments. The
classic `$^V lt v5.6.0` perl-version guard is the canonical trigger.

**Repro** (minimal):
```perl
my $ok = $^V lt v5.6.0;
my $y = 1;
```

**Actual** — `v5.6.0` collapses into an `ambiguous_function_call_expression`
named `v5` whose argument is `(binary_expression 6 . 0)`:
```
(relational_expression
  left:  (scalar (varname))              # $^V
  right: (ambiguous_function_call_expression
           function: (function)          # "v5"  <-- bareword
           arguments: (binary_expression  # 6.0  treated as 6 . 0
             left:  (number)             # 6
             right: (number))))          # 0
```
Note: the parse otherwise *recovers* — no ERROR node, the following
`my $y = 1;` is a clean statement. The damage is purely the spurious
`function: v5` node, which the LSP then reports as `unresolved-function 'v5'`.

**Expected** — a dedicated v-string / version-string literal node (analogous
to `number` / `string_literal`), so the builder never sees a function call.

**Real-world repro** — AWStats legacy CGI, two sites of the same guard:
```
perl-lsp --parse ~/perl-qa-corpus/AWStats/wwwroot/cgi-bin/awstats.pl   # :1938
perl-lsp --parse ~/perl-qa-corpus/AWStats/tools/awstats_buildstaticpages.pl  # :186
```
both `if ( $level > 1 && $^V lt v5.6.0 ) { ... }`.

**Downstream impact** — `unresolved-function 'v5'` false positive at every
v-string site. No contagion (parse recovers at the statement boundary), so
a builder-side stopgap that recognizes the `vN.N.N` shape and suppresses the
FP is viable — but the clean fix is a literal node upstream.

---

## GR-2 — bareword constant on the LHS of `&&` errors on the operator

When a bareword (a `use constant` name, or any unproven bareword) is the
**left** operand of the high-precedence `&&` operator, the bareword is
greedily parsed as a function call that swallows the right operand as its
argument list, and the `&&` token itself becomes a recoverable ERROR node.

**Repro** (minimal — reproduces in 1.0.3 at any nesting level, including top
level):
```perl
use constant C => 1;
my $y = 0;
my $z = C && !$y;
```

**Actual** — `C` becomes an `ambiguous_function_call_expression`; `&&` is an
ERROR child; the right operand `!$y` is parsed as the call's argument:
```
(ambiguous_function_call_expression
  function: (function)            # "C"  <-- bareword as function
  (ERROR (function (varname)))    # "&&"  <-- recoverable ERROR on the operator
  arguments: (unary_expression    # !$y  becomes the "argument"
    operand: (scalar (varname))))
```

**Trigger boundary** (verified): the error is keyed to **bareword-on-LHS of
high-precedence `&&`**, independent of the right operand —
`C && 1`, `C && !$y`, and `C && foo()` all error identically. It does **not**
fire when the bareword is on the *right* (`$y && C` parses clean) nor with
the low-precedence word operator (`C and !$y` parses clean). So it is the
`&&` precedence/ambiguity interaction with a leading bareword, not the
negation or the `constant` pragma per se.

**Nuance vs. earlier note** — an earlier QA observation suggested the
trivial `use constant C => 1; C && !$y` did *not* reproduce and that the
surrounding sub/inlined-codegen context was required. Against
**ts-parser-perl 1.0.3** the minimal top-level form reproduces directly
(one ERROR on `&&`); the sub/ternary context is incidental, not the trigger.
Documenting both: the real sites are all inside `sub { ... }` ternaries, but
the parser team should reproduce against the minimal top-level form above.

**Expected** — a leading bareword followed by `&&` is a logical-and
expression whose left operand is the bareword call/constant, not a function
call consuming the operator and right operand.

**Real-world repro** — Type::Tiny inlined-constraint codegen (3/47 files),
all `_HAS_REFUTILXS && !$Type::Tiny::AvoidCallbacks ? ... : ...` /
`!_FIXED_PRECEDENCE && $_[2]` shapes inside `sub { ... }` bodies:
```
perl-lsp --parse ~/perl-qa-corpus/Type::Tiny/lib/Types/Standard.pm  # :446,478,509,531
perl-lsp --parse ~/perl-qa-corpus/Type::Tiny/lib/Type/Tiny.pm       # :165,189
perl-lsp --parse ~/perl-qa-corpus/Type::Tiny/lib/Eval/TypeTiny.pm   # :124
```

**Downstream impact** — the constant flags as `unresolved-function`, and the
operator's ERROR node truncates the surrounding expression's structure
(the `&&` RHS is mis-attached as an argument). Parser recovers at the next
statement, so diagnostics stay complete elsewhere, but the LHS bareword and
its `&&`-RHS are mis-modeled at every such site.

---

## G4 — bareword filehandle in the indirect-object slot parsed as a function call

The grammar **already models** the indirect-object filehandle for the scalar
and block forms — `print $fh LIST` emits an `indirect_object` node wrapping a
`scalar`, and `print {$fh} LIST` emits `indirect_object` wrapping a `block`.
But the **bareword** form (`print STDERR ...`, `print FH ...`) is NOT routed
through `indirect_object`: the bareword degrades to a nested
`ambiguous_function_call_expression` (function = the filehandle, arguments =
the print list), i.e. the filehandle is parsed as a *function call*. Same
family as the `not`-operator gap (tree-sitter-perl#230).

**Repro:**
```
printf 'print STDERR "x";\nprint FH @list;\nprint {$fh} "y";\nprint $fh "z";\n' > /tmp/fh.pl
perl-lsp --parse /tmp/fh.pl
#  print STDERR "x"  -> ambiguous_function_call_expression (function: STDERR)   ❌
#  print FH @list    -> ambiguous_function_call_expression (function: FH)        ❌
#  print {$fh} "y"   -> indirect_object (block (... $fh ...))                    ✓
#  print $fh "z"     -> indirect_object (scalar $fh)                             ✓
```

**Expected** — extend the existing `indirect_object` production to accept a
bareword filehandle (the `print`/`printf`/`say` indirect-object slot: a
bareword immediately following the verb with no comma/paren before the list),
matching what it already does for `$fh`/`{$fh}`.

**Downstream impact** — the bareword filehandle flags as
`unresolved-function` (the #1 recurring FP on classic-Perl corpora — 458 in
perltidy alone). The LSP currently papers over this with a builder-side guard
(`is_indirect_object_filehandle_call` in `src/builder.rs`) that sniffs out the
function-call shape and suppresses the ref; that **kludge can be deleted** once
the grammar emits `indirect_object` for the bareword form.

---

## TO VERIFY (parser vs builder — likely parser, confirm intent)

These two reproduce as questionable CST shapes. They look like grammar
issues but could be argued as builder-side handling; flagging for the
parser team to confirm which side owns the fix.

### `} or next` — `or` after a bare block parsed as a function call

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

### `\&{$expr}` / `&{$expr}(...)` — symbolic code-deref modeled as a function

```perl
my $code = \&{$expr};
my $r    = &{$expr}(1, 2);
```

**Actual** — the deref target is a `function` node whose name is a
`block` containing the scalar:
```
(refgen_expression
  (function (varname (block (expression_statement (scalar))))))   # \&{$expr}
```
No ERROR, but the inner `$expr` is buried under `function`/`varname`/`block`
rather than presented as a deref of a code-ref expression. **Expected** — a
dedicated code-dereference node (analogous to `${...}` / `@{...}` deref
forms) so the builder can tell "call/take-ref-of the coderef in `$expr`"
from "call the sub literally named by a block." **Downstream impact** —
risk of the builder emitting a spurious call ref for the synthetic block.
Lower severity than G1–G3 (no contagion); confirm whether the parser team
intends a deref node or expects the consumer to special-case this shape.

---

## Pointer

A one-line cross-ref lives in `docs/ROADMAP.md` under the upstream-parser
section. When any of G1–G3 is fixed upstream, the matching QA-findings
entry (`docs/qa-findings.md` §G) can be closed and the structural-recovery
load it imposes (`docs/adr/error-recovery.md`) shrinks.
