#!/usr/bin/env bash
# Usage: ./scripts/qa/gen-workspace.sh [WORKSPACE_DIR]
# Walks the system Perl's @INC and writes a synthetic workspace that
# imports every discoverable .pm. Used to drive the @INC-wide QA bench.
#
# Output: WORKSPACE_DIR/{modules.list, cpanfile, lib/QA.pm}
# Default WORKSPACE_DIR=qa-workspace (gitignored).
set -euo pipefail

WS="${1:-qa-workspace}"
mkdir -p "$WS/lib"

# Walk @INC, collect every .pm path, normalize to Module::Name.
perl -e '
my @inc = grep { -d } @INC;
my %seen;
my @mods;
for my $dir (@inc) {
  open my $fh, "-|", "find", $dir, "-type", "f", "-name", "*.pm";
  while (my $f = <$fh>) {
    chomp $f;
    my $rel = $f;
    $rel =~ s|^\Q$dir\E/||;
    $rel =~ s|\.pm$||;
    my $mod = $rel;
    $mod =~ s|/|::|g;
    next unless $mod =~ /^[A-Z][\w:]*$/;
    next if $seen{$mod}++;
    push @mods, $mod;
  }
}
print "$_\n" for sort @mods;
' > "$WS/modules.list"

awk '{ print "requires \x27" $0 "\x27;" }' "$WS/modules.list" > "$WS/cpanfile"

{
  echo "package QA;"
  echo "use strict; use warnings;"
  awk '{ print "use " $0 "();" }' "$WS/modules.list"
  echo "1;"
} > "$WS/lib/QA.pm"

echo "Wrote $WS/modules.list ($(wc -l < "$WS/modules.list" | tr -d ' ') modules)"
echo "Wrote $WS/cpanfile, $WS/lib/QA.pm"
