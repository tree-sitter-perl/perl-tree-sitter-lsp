#!/usr/bin/perl
# ============================================================================
# Phase 5 (+ fix) demo — the actually-new behavior.
#
# This file uses a package + sub name that COLLIDES with other test_files:
#   sample.pl and lib/TestExporter.pm both define `sub get_config` with a
#   `host` key. Before package-qualified HashKeyOwner::Sub, `gr` on `host`
#   here polluted results with those unrelated files.
#
# With the fix: Demo::phase5::get_config is isolated. Only hits in THIS
# file and any file that actually accesses Demo::phase5::get_config show up.
#
# To see the difference, compare:
#   - Released perl-lsp:     gr floods with matches from sample.pl, TestExporter.pm
#   - refactor/unification:  gr shows only this file's hits
# ============================================================================

package Demo::phase5;

use strict;
use warnings;

sub get_config {
    # ▸ Cursor on `host` in the hash below, hit `gr`.
    #   Previously (on the released binary): results include
    #     test_files/lib/TestExporter.pm — unrelated `sub get_config`
    #     test_files/sample.pl           — unrelated `sub get_config`
    #   Now (on this branch's binary): results stay within this file,
    #   because our HashKeyOwner::Sub is package-qualified
    #     (Demo::phase5 ≠ main ≠ TestExporter).
    return {
        host => 'localhost',
        port => 5432,
        name => 'mydb',
    };
}

my $c = Demo::phase5::get_config();

my $h1 = $c->{host};    # (A1) — the sole access sites for Demo::phase5::get_config
my $h2 = $c->{host};    # (A2)
my $p  = $c->{port};    # (B)  — different key, proves the query filters by key name

# ▸ Cursor on `host` at (A1) or (A2), hit `gr`. Same invariant.
# ▸ Try this file first on the released binary, then on the branch binary —
#   the noise from sample.pl / TestExporter.pm is the bug we just fixed.

print "$h1 $h2 $p\n";

1;
