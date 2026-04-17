#!/usr/bin/perl
# ============================================================================
# Phase 5 demo — behaviors that previously didn't work cleanly.
#
# Open this file in a perl-lsp-attached editor and try the cursor positions
# called out below with `gr` (references) and `gd` (goto-def). The key names
# here (host/port/name) are the same the unit tests use, to keep this file
# trivial to eyeball.
#
# What's new in phase 5:
#   1. References on a hash key defined inside a sub's return value now
#      find every access site via an O(1) index, no tree argument required.
#   2. `Backend::references` walks cross-file for hash keys too — previously
#      it fell back to single-file because owner resolution required a tree.
#
# Before phase 5 this was: "goto-def works, but references is empty or only
# finds the def itself." Now: def + every matching access.
# ============================================================================

use strict;
use warnings;

# ── Source of truth: a sub that returns a hashref with three known keys ──

sub get_config {
    # ▸ Put cursor on `host` below and hit `gr`.
    #   Expected: two results — this def, plus the `$cfg->{host}` access on
    #   the line marked (A). Previously this returned [] without a tree.
    return {
        host => 'localhost',
        port => 5432,
        name => 'mydb',
    };
}

# ── Local consumer of get_config ──

my $cfg = get_config();

my $h = $cfg->{host};   # (A) access site for `host`
my $p = $cfg->{port};   # (B) access site for `port`
my $n = $cfg->{name};   # (C) access site for `name`

# ▸ Put cursor on `host` at (A) and hit `gr`.
#   Expected: this access + the def inside get_config().
#   Previously: required tree-walking the invocant; worked in the open-editor
#   case but not when querying a closed workspace file.

# ▸ Put cursor on `host` at (A) and hit `gd`.
#   Expected: jumps to the `host =>` inside the return hash. (Unchanged; this
#   already worked — listed here so you can eyeball symmetry with `gr`.)

# ── A second consumer through an intermediary — tests the "find accesses
# across callers" flow when more than one path exists to the key. ──

sub load_config { return get_config(); }

my $alt = load_config();
my $alt_host = $alt->{host};  # (D) another access to `host`

# ▸ Cursor on `host` inside the def — references should now include (A) and
#   (D) and any others, all without needing the caller's tree.

print "$h $p $n $alt_host\n";

1;
