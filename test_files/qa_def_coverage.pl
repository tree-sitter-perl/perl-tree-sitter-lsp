#!/usr/bin/perl
# ============================================================================
# QA COVERAGE FIXTURE (added by definition-type matrix QA, part 1)
#
# Fills coverage gaps the existing corpus lacked a clean fixture for:
#   * use constant NAME => ...  with a real *usage* site (refs/goto-def)
#   * @EXPORT (vs only @EXPORT_OK elsewhere)
#   * %EXPORT_TAGS tag-member sites (NAV-B)
#
# This file only adds fixtures; it does not modify any existing fixture.
# Expected (aspirational) navigation behavior is noted per block so a
# future fix can assert against it.
# ============================================================================

package QA::Constants;
use strict;
use warnings;
use Exporter 'import';

# ── @EXPORT (auto-imported) + @EXPORT_OK + %EXPORT_TAGS ──
our @EXPORT      = qw(always_on);
our @EXPORT_OK   = qw(opt_a opt_b opt_c);
our %EXPORT_TAGS = (
    group_one => [qw(opt_a opt_b)],   # NAV-B: opt_a/opt_b here SHOULD ref the subs
    group_two => [qw(opt_c)],
);

# ── use constant with downstream usages ──
# CONST gap: a usage of MAX_RETRIES inside a call argument currently does
# not get its own ref linked back to this def; goto-def from the usage
# misroutes to the enclosing call target. This block makes that testable.
use constant MAX_RETRIES => 5;
use constant {
    TIMEOUT => 30,
    BACKOFF => 2,
};

sub always_on { return 1 }
sub opt_a     { return 'a' }
sub opt_b     { return 'b' }
sub opt_c     { return 'c' }

sub retry_loop {
    my ($self) = @_;
    # Usage of the constant in a plain expression AND as a call arg.
    my $limit = MAX_RETRIES;                 # plain usage
    return _attempt($limit, TIMEOUT);        # call-arg usages
}

sub _attempt {
    my ($limit, $timeout) = @_;
    return $limit + $timeout;
}

1;
