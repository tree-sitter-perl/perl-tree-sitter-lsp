#!/usr/bin/env perl
# Gold-corpus harness — re-runs every corpus row against the current binary and
# reports regressions. The corpus markdown is human-readable prose (expected/
# actual are not machine fixtures), so this is a SMOKE + LIVENESS guard, not an
# exact-assertion runner:
#
#   * CRASH    — the query aborted the process (exit 134 / signalled). Always a
#                regression; this is the check that would have caught the X1
#                scanner-overflow class of bug.
#   * REGRESS  — a row whose recorded verdict was PASS now returns empty / errors
#                (a resolution that used to work no longer does).
#   * (rows recorded as FAIL that still fail are reported as KNOWN-GAP, not noise.)
#
# Exact target-matching needs structured fixtures (a JSON sidecar generated
# alongside the markdown) — deliberately out of scope for v1; see README.
#
# Usage:
#   gold-corpus/run.pl [capability ...]      # default: all capability files
#   BIN=path/to/perl-lsp CORPUS=~/perl-qa-corpus gold-corpus/run.pl definition hover
#
# Needs the corpus repos checked out at $CORPUS (default ~/perl-qa-corpus) and a
# release binary built. Not a cargo/CI test — it depends on that external corpus.

use strict;
use warnings;
use FindBin qw($RealBin);
use File::Spec;
use POSIX qw(WIFSIGNALED WTERMSIG);

my $root   = File::Spec->rel2abs("$RealBin/..");
my $bin    = $ENV{BIN}    || "$root/target/release/perl-lsp";
my $corpus = $ENV{CORPUS} || "$ENV{HOME}/perl-qa-corpus";

die "binary not found: $bin (build with: cargo build --release)\n" unless -x $bin;
unless (-d $corpus) {
    die "corpus repos not found at $corpus.\n"
      . "Set CORPUS=<dir> or check out the QA corpus there.\n";
}

# capability file -> how to invoke it. cursor = `path:line:col`; query token is
# the 2nd backtick group (the search string for workspace-symbol).
#   needs: root? file? linecol? — what the CLI flag consumes.
my %CAP = (
    'definition'         => { flag => '--definition',        root => 1, file => 1, lc => 1 },
    'references'         => { flag => '--references',         root => 1, file => 1, lc => 1 },
    'hover'              => { flag => '--hover',              root => 1, file => 1, lc => 1 },
    'type-at'            => { flag => '--type-at',            root => 0, file => 1, lc => 1 },
    'completion'         => { flag => '--completion',         root => 1, file => 1, lc => 1 },
    'signature-help'     => { flag => '--signature-help',     root => 1, file => 1, lc => 1 },
    'document-highlight' => { flag => '--document-highlight', root => 1, file => 1, lc => 1 },
    'linked-editing'     => { flag => '--linked-editing',     root => 1, file => 1, lc => 1 },
    'semantic-tokens'    => { flag => '--semantic-tokens',    root => 1, file => 1, lc => 0 },
    'outline'            => { flag => '--outline',            root => 0, file => 1, lc => 0 },
    'workspace-symbol'   => { flag => '--workspace-symbol',   root => 1, query => 1 },
    'rename'             => { flag => '--rename',             root => 1, file => 1, lc => 1, rename => 1 },
    'diagnostics'        => { check => 1 },   # run --check once per repo; crash-only check
);

my @want = @ARGV ? @ARGV : sort keys %CAP;

my %repo_dir;   # repo name -> resolved corpus path (cache)
sub repo_path {
    my ($repo) = @_;
    return $repo_dir{$repo} if exists $repo_dir{$repo};
    my $p = "$corpus/$repo";
    $repo_dir{$repo} = (-d $p) ? $p : undef;
    return $repo_dir{$repo};
}

# resolve the cursor's file: try <corpus>/<repo>/<path>, then <corpus>/<path>.
sub resolve_file {
    my ($repodir, $path) = @_;
    for my $cand ("$repodir/$path") {
        return $cand if -f $cand;
    }
    # some rows prefix the repo dir into the path already
    (my $stripped = $path) =~ s{^[^/]+/}{};
    my $c2 = "$repodir/$stripped";
    return $c2 if -f $c2;
    return "$repodir/$path";   # let the binary report the miss
}

# run a command (arrayref), capture combined output + exit/signal.
sub run_cmd {
    my ($argv) = @_;
    my $pid = open(my $fh, '-|');
    die "fork: $!" unless defined $pid;
    if ($pid == 0) {
        open(STDERR, '>&', \*STDOUT);
        exec @$argv or exit 127;
    }
    local $/;
    my $out = <$fh> // '';
    close($fh);
    my $st = $?;
    my $crashed = WIFSIGNALED($st) || ($st >> 8) == 134;
    return ($out, $st, $crashed);
}

my %seen_check;  # repo -> [crashed, ran]
my @crashes; my @regress; my @known_gap; my @ran; my @skipped;

for my $cap (@want) {
    my $spec = $CAP{$cap} or do { warn "unknown capability: $cap\n"; next; };
    my $file = "$RealBin/$cap.md";
    unless (-f $file) { warn "no corpus file: $cap.md\n"; next; }
    open(my $md, '<', $file) or die "open $file: $!";
    while (my $line = <$md>) {
        next unless $line =~ /^\|/;                 # table row
        next if $line =~ /^\|\s*id\s*\|/;           # header
        next if $line =~ /^\|[-\s|]+\|$/;           # separator
        my @col = map { s/^\s+|\s+$//gr } split /\|/, $line;
        shift @col if @col && $col[0] eq '';        # leading empty from |
        my ($id, undef, undef, $repo, $cursor, $expected, undef, $verdict) = @col;
        next unless defined $id && length $id;
        next unless defined $verdict;               # only well-formed rows

        my $repodir = repo_path($repo);
        unless ($repodir) { push @skipped, "$id (repo $repo absent)"; next; }

        # diagnostics: one --check per repo, crash-only.
        if ($spec->{check}) {
            unless ($seen_check{$repo}) {
                my ($out, undef, $cr) = run_cmd([$bin, '--check', $repodir, '--severity', 'warning', '--format', 'json']);
                $seen_check{$repo} = 1;
                push @ran, "diagnostics/$repo";
                push @crashes, "diagnostics --check $repo" if $cr;
            }
            next;
        }

        # parse cursor: `path:line:col` and optional `token`
        my @ticks = $cursor =~ /`([^`]*)`/g;
        my ($pathlc, $token) = @ticks;
        next unless defined $pathlc;
        my ($path, $ln, $cl) = $pathlc =~ /^(.*):(\d+):(\d+)$/;
        next unless defined $path;

        my @argv = ($bin, $spec->{flag});
        if ($spec->{query}) {
            push @argv, $repodir, ($token // '');
        } else {
            my $f = resolve_file($repodir, $path);
            push @argv, $repodir if $spec->{root};
            push @argv, $f       if $spec->{file};
            push @argv, $ln, $cl if $spec->{lc};
            push @argv, '__HARNESS_RENAME__' if $spec->{rename};
        }

        my ($out, $st, $crashed) = run_cmd(\@argv);
        push @ran, "$cap/$id";
        my $cmd = join(' ', map { /\s/ ? "'$_'" : $_ } @argv);

        if ($crashed) {
            push @crashes, "$cap/$id\n      $cmd";
            next;
        }
        my $empty = ($out !~ /\S/);
        my $pass  = ($verdict eq 'PASS');
        if ($pass && $empty) {
            push @regress, "$cap/$id  (was PASS, now empty)\n      $cmd";
        } elsif (!$pass && $empty) {
            push @known_gap, "$cap/$id";
        }
    }
    close($md);
}

# ---- report ----
printf "\nGold-corpus harness — %d rows run\n", scalar @ran;
printf "  binary: %s\n  corpus: %s\n", $bin, $corpus;
print  "\n";
printf "  %-10s %d\n", 'CRASH',    scalar @crashes;
printf "  %-10s %d\n", 'REGRESS',  scalar @regress;
printf "  %-10s %d\n", 'known-gap',scalar @known_gap;
printf "  %-10s %d\n", 'skipped',  scalar @skipped;

if (@crashes) {
    print "\n!! CRASHES (process aborted — always a regression):\n";
    print "  - $_\n" for @crashes;
}
if (@regress) {
    print "\n!! REGRESSIONS (recorded PASS, now empty/broken):\n";
    print "  - $_\n" for @regress;
}
if (@skipped) {
    print "\nskipped (corpus repo not checked out):\n";
    print "  - $_\n" for @skipped;
}
print "\n";

exit((@crashes || @regress) ? 1 : 0);
