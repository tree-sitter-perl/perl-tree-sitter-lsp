#!/usr/bin/perl
# ============================================================================
# Phase 5 demo — qualified calls + delegation chains through intermediates.
#
# Try each labelled cursor spot with `gr` (references). On the RELEASED
# binary every one of these is broken — the previous perl-lsp never linked
# the hash-key def to its access when the call was qualified or routed
# through an intermediate sub.
#
# On this branch (build with `cargo build --release`, or run dev.sh), all
# three spots produce the expected set of hits.
# ============================================================================

package Demo::phase5;

use strict;
use warnings;

sub get_config {
    # ▸ Cursor on `host` in the hash below, hit `gr`.
    #   Expected on this branch: accesses (A1), (A2), (B1).
    #   Previously: no hits, or bled into sample.pl / lib/TestExporter.pm
    #   (those have their own `sub get_config` with a `host` key).
    return {
        host => 'localhost',
        port => 5432,
        name => 'mydb',
    };
}

# Intermediate sub — does nothing but forward to get_config. Before the fix,
# this broke the chain: $via_chain's owner became Sub(chain_helper), which
# had no HashKeyDefs, so refs to its keys resolved to nothing.
sub chain_helper {
    return get_config();
}

# ── Direct qualified call ──
my $c = Demo::phase5::get_config();
my $h1 = $c->{host};    # (A1)
my $h2 = $c->{host};    # (A2)

# ── Through an intermediate sub ──
my $via_chain = chain_helper();
my $h3        = $via_chain->{host};  # (B1)

# ▸ Cursor on `host` at (A1), (A2), or (B1), hit `gr`.
#   All three should return the def + (A1) + (A2) + (B1).

# ── Port, for contrast ──
my $p = $c->{port};  # Different key, proves the query filters by key name.

print "$h1 $h2 $h3 $p\n";

1;
