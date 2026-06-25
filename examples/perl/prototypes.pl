#!/usr/bin/env perl
# ============================================================
#  Perl prototype reparenthesizer spike — the same reparse seam as
#  the C++ macro case, but local and preprocessor-free.
#  Branch: spike/cpp-support.  Design: docs/prompt-cpp-reparse.md
#  Run:  cargo test reparse -- --nocapture
# ============================================================
use strict;
use warnings;

# A prototype is a DECLARATION that changes how later code PARSES —
# the same phenomenon as a C++ macro, type-independent, so it
# stratifies strictly upstream of the type worklist.

sub sner ($) { return $_[0] + 1 }   # unary  prototype
sub noop ()  { return 42 }          # nullary prototype

# ---- without the prototype fact, tree-sitter parses these WRONG ----

my @xs = sner 1, 2;
#        ^^^^^^^^^^ greedy: tree-sitter grabs BOTH 1 and 2 as args.
#        Knowing `sner` is unary ($), the reparenthesizer rewrites this
#        to `sner(1), 2` and RE-PARSES — now a real arity-1 call whose
#        result is the first element of the list.

my $y = noop + 1;
#       ^^^^^^^^ tree-sitter reads `noop` as a bareword string.
#       Knowing `noop` is nullary (), it rewrites to `noop() + 1` —
#       now a call, then the addition.

# An AnchorMap (transformed byte -> original byte, the Zed-anchor idea
# in miniature) carries every span from the rewritten source back onto
# the tokens you actually typed, so goto/rename/refs land correctly.

print "@xs $y\n";
