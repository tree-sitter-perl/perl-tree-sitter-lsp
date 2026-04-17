#!/usr/bin/perl
# ============================================================================
# Phase 5 demo — what genuinely works, honestly pitched.
#
# Uses a unique sub name (`phase5_demo_config`) so the cross-file walk isn't
# polluted by other test_files/ that happen to also define `sub get_config`
# with a `host` key. The broader "owner by name" scoping is a phase 1
# limitation (Namespace widening) — phase 5 doesn't claim to fix it.
#
# Try each spot with `gr` (references). IDE convention: cursor on a decl
# returns the USES, not the decl itself.
# ============================================================================

use strict;
use warnings;

# ── Source of truth ──

sub phase5_demo_config {
    # ▸ Cursor on `host` in the return hash below, hit `gr`.
    #   Expected: every `$c->{host}` site where `$c` was bound directly to
    #   phase5_demo_config() — i.e. (A1), (A2), (A3) below.
    #   Previously: returned [] without a tree argument.
    return {
        host => 'localhost',
        port => 5432,
        name => 'mydb',
    };
}

my $c = phase5_demo_config();

my $h1 = $c->{host};   # (A1)
my $h2 = $c->{host};   # (A2) — a second access of the same key
my $p  = $c->{port};   # (B)  — different key, used to eyeball that the
                       #         references list contains only `host` hits
                       #         and not `port`.

# A copy of the same binding gets the same owner — owner is keyed on the
# sub name, not on the variable identity.
my $also = phase5_demo_config();
my $h3   = $also->{host};  # (A3)

# ▸ Cursor on `host` at (A1), (A2), or (A3) and hit `gr`.
#   Expected: all three access sites + the decl inside phase5_demo_config().
#   The `include_decl` flag is true here because the cursor is on a ref, not
#   on the symbol itself.

# ── Known limitation that phase 5 does NOT fix ──
#
# The following reassignment breaks the chain because the builder's owner
# propagation is direct-only: `$chained`'s owner becomes Sub("chain_helper"),
# not Sub("phase5_demo_config"). So `$chained->{host}` is NOT included in
# references for the key above. Fixing this is a type-propagation-through-
# returns enhancement (a later phase); calling it out here so the demo
# doesn't mislead.

sub chain_helper { return phase5_demo_config(); }
my $chained = chain_helper();
my $x = $chained->{host};  # (LIMITATION) — won't show up; owner mismatch.

print "$h1 $h2 $h3 $p $x\n";

1;
