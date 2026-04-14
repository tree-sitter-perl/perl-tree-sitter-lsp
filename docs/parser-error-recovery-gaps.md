# Parser Error Recovery Gaps

## Context

When editing, tree-sitter wraps broken regions in `ERROR` nodes. The LSP builder can recover structural declarations from ERROR children — but only if tree-sitter-perl preserves them as their correct node types.

## The Problem

tree-sitter-perl's error recovery downgrades Perl keywords to bareword/function tokens inside ERROR nodes. This means the builder can't distinguish `sub process { }` from `process({ })` inside an ERROR.

### What happens with `my $x = [`

The unclosed bracket creates an ERROR spanning from the `[` to end-of-file. Inside that ERROR:

| Source | Parsed as | Expected |
|--------|-----------|----------|
| `sub process { }` | `ambiguous_function_call_expression(function: "process", args: anonymous_hash)` | `subroutine_declaration_statement` |
| `use List::Util qw(max)` | `function("use")` + `ambiguous_function_call_expression("List::Util", qw(...))` | `use_statement` |
| `package Bar;` | `package_statement` ✓ | `package_statement` |

Note: `package_statement` already survives inside ERROR (verified). `sub` and `use` do not.

### What happens with `reduce` (bareword)

The bareword `reduce` on a line by itself gets parsed as a function call. tree-sitter-perl then tries to parse the next line as its arguments:

| Source | Parsed as | Expected |
|--------|-----------|----------|
| `reduce\npackage Bar;` | `ambiguous_function_call_expression(reduce, package(Bar))` | `expression_statement(reduce)` + `package_statement(Bar)` |

No ERROR node at all — the parser just misinterprets `package` as a function argument.

## What the Builder Does Today

```rust
"ERROR" => self.recover_structural_from_error(node),
```

Recurses into ERROR children looking for:
- `package_statement`
- `use_statement`
- `subroutine_declaration_statement` / `method_declaration_statement`
- `class_statement`
- `ambiguous_function_call_expression` (for `has` calls)

This infrastructure is ready — it just needs the parser to preserve these node types inside ERROR.

## Desired Parser Improvements

### 1. Preserve `sub`/`method` declarations inside ERROR

When the parser encounters `sub name { ... }` or `method name { ... }` inside an ERROR region, it should emit `subroutine_declaration_statement` / `method_declaration_statement` instead of downgrading `sub` to a bareword.

**Impact:** Function/method symbols survive editing. Completion, outline, goto-def all work during typing.

### 2. Preserve `use` statements inside ERROR

`use Module qw(...)` inside ERROR should emit `use_statement`, not `function("use")`.

**Impact:** Import detection survives editing. Framework mode (`use Moo` → Moo accessor synthesis) doesn't flap. Auto-import insertion position stays correct.

### ~~3. Preserve `package` statements inside ERROR~~

Already works — `package_statement` is preserved inside ERROR. Verified.

### 4. Don't eat `package` as a function argument

When a bareword like `reduce` appears on its own line, the next `package` keyword should NOT be parsed as an argument to `reduce`. The parser should terminate the `ambiguous_function_call_expression` at the newline (or at least at a keyword like `package`/`use`/`sub`).

**Impact:** Multi-package files work correctly during typing. This is the most common case — user types a partial expression, and the next package gets swallowed.

## Why #4 Can't Be Solved in the Parser

This was extensively investigated. The root cause: `ambiguous_function_call_expression` (unparenthesized calls like `print "hello"`) takes `$._listexpr` as arguments, which includes any expression. When `reduce` is on its own line, the parser treats everything on the next line as arguments — `package Bar;` becomes `reduce(package(Bar))`.

### Approaches tried and rejected

**Newline-semicolon token**: Emit a synthetic `;` at newline boundaries when the parser expects a statement terminator. Rejected because:
- Postfix `if`/`unless` across lines (`print "hello"\nunless $cond;`) would break
- GLR branch selection with `optional($.newline_semicolon)` in postfix rules caused tree-sitter to prefer MISSING-insertion branches over clean parses

**Statement-keyword detection**: Have the scanner peek at the next word after a newline and emit `PERLY_SEMICOLON` if it's a statement keyword (`package`, `use`, `sub`, etc.). Rejected because:
- Fat comma autoquoting: `foo(package => 1, use => "bar")` is valid Perl — keywords ARE legitimate function arguments when followed by `=>`
- Distinguishing `package Bar;` from `package => 1` requires peeking past the keyword AND past whitespace to check for `=>` — deep lookahead with no clean fallback
- Returning false from the scanner after advancing triggers error recovery

**Grammar precedence**: Can't give `package_statement` higher precedence than `ambiguous_function_call_expression` arguments because the parser needs `_semicolon` to end the expression statement first, and there's no semicolon.

### Fundamental constraint

tree-sitter's grammar is token-based, not line-based. The external scanner can detect newlines, but it can only emit tokens — it can't prevent the parser from continuing an expression across lines. The parser's GLR machinery and `valid_symbols` union make it impossible to selectively block argument parsing without side effects on valid constructs.

## Recommended LSP-Side Approach

The LSP has context the parser doesn't: the previous parse tree, knowledge of what's a declaration vs expression, and the ability to preprocess input before parsing.

### Input preprocessing strategy

Before parsing, the LSP can insert synthetic semicolons at positions where it detects a missing statement boundary. For example:

1. Walk the previous parse tree to find `ambiguous_function_call_expression` nodes that span multiple lines
2. Check if the second line starts with a keyword (`package`, `use`, `sub`, etc.)
3. If so, insert a `;` at the end of the first line in the input text
4. Parse the modified input — tree-sitter now sees `reduce;\npackage Bar;` and produces two clean statements

This approach is safe because:
- It only modifies the input for INCOMPLETE code (during editing)
- The previous parse tree guides where to insert — no false positives on valid code
- The inserted `;` is at a position where the user likely intended one
- It works for ALL statement keywords without special-casing

### Alternative: post-parse recovery

If preprocessing is too aggressive, the builder can detect the greedy-bareword pattern in the parse tree and split it:

1. Find `ambiguous_function_call_expression` where `arguments` contains another `ambiguous_function_call_expression` whose `function` is a known keyword
2. Treat the inner call as the start of a new statement
3. Reconstruct the document symbols accordingly

This is what the builder already does for `ERROR` children — extend it to detect misparse patterns in non-ERROR trees.

## Test Coverage

The LSP repo has `#[ignore]` tests that will pass once these improvements land:

- `test_error_recovery_sub_inside_error` — needs improvement #1
- `test_error_recovery_import_inside_error` — needs improvement #2
- ~~`test_error_recovery_package_inside_error`~~ — #3 already works
- (No test for #4 — needs LSP-side approach)

Remove the `#[ignore]` attribute to verify each improvement.

## Priority

**#4 (bareword eating package)** is the highest priority — it affects every multi-package file on every keystroke. Needs LSP-side solution (see above).

**#1 (sub inside ERROR)** is second — subs are the most important structural element for LSP features. May be solvable in the parser.

**#2 (use inside ERROR)** is lower priority — `use` inside ERROR is rarer since the ERROR usually ends before swallowing it. May be solvable in the parser.
