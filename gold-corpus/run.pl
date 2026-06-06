#!/usr/bin/env perl
# Gold-corpus harness — structured, exact-assertion regression runner.
#
# Substrate: a hermetic, version-pinned module tree installed by cpm/carton from
# gold-corpus/cpanfile + cpanfile.snapshot into gold-corpus/local. The harness
# points the LSP at local/lib/perl5 (workspace root + PERL5LIB), so every query
# resolves against the SAME pinned versions in dev and CI. Rebuild the substrate:
#     cd gold-corpus && carton install --deployment      # exact, from snapshot
#   (or, faster:  cpm install -L local --resolver snapshot)
#
# Source of truth: gold-corpus/fixtures/*.json. Each row re-runs a CLI query and
# asserts its NORMALIZED output:
#     expect.all  — every string MUST appear
#     expect.none — no string may appear
#   status:
#     gold        — assertion MUST hold; else FAIL (regression)
#     xfail       — known gap: assertion must currently NOT hold; if it starts
#                   holding → XPASS (gap fixed → promote to gold). Soft failure.
#     provisional — run + report, never fails the suite
#   a process abort (exit 134 / signal) is always a hard FAIL (the scanner-overflow class).
#
# Output is normalized before matching: absolute paths reduced to basenames; JSON
# outputs (references/workspace-symbol/outline/rename/diagnostics) decoded and
# re-encoded canonically (sorted keys, compact) so one substring ties
# file+line+kind. The SAME normalize() backs `--emit`, which prints the canonical
# text for one ad-hoc query — author fixtures against `--emit` and they match by
# construction.
#
# Usage:
#   gold-corpus/run.pl [capability ...]                       # run the suite
#   gold-corpus/run.pl --list
#   gold-corpus/run.pl --emit <cap> <file> <line> <col> [newname]
#   gold-corpus/run.pl --emit workspace-symbol <query>
#   gold-corpus/run.pl --emit diagnostics
#   BIN=path CORPUS=<lib> gold-corpus/run.pl definition
# CORPUS defaults to gold-corpus/local/lib/perl5; <file> is a path under it.

use strict;
use warnings;
use FindBin qw($RealBin);
use File::Spec;
use JSON::PP;
use POSIX qw(WIFSIGNALED);

my $bin    = $ENV{BIN}    || File::Spec->rel2abs("$RealBin/../target/release/perl-lsp");
my $corpus = $ENV{CORPUS} || "$RealBin/local/lib/perl5";
my $fxdir  = "$RealBin/fixtures";

die "binary not found: $bin (cargo build --release)\n" unless -x $bin;
unless (-d $corpus) {
    die "substrate not found at $corpus.\n"
      . "Build it: cd gold-corpus && carton install   (or cpm install -L local --resolver snapshot)\n";
}
# hermetic @INC: the pinned tree + its arch dir, ahead of anything inherited.
my ($arch) = grep { -d } glob("$corpus/*/auto") ? map { s{/auto$}{}r } glob("$corpus/*/auto") : ();
$ENV{PERL5LIB} = join(':', grep { defined && length } $corpus, $arch, $ENV{PERL5LIB});

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
    'workspace-symbol'   => { flag => '--workspace-symbol',   root => 1, qarg => 1 },
    'rename'             => { flag => '--rename',             root => 1, file => 1, lc => 1, rename => 1 },
    'diagnostics'        => { check => 1 },
);
my %JSON_CAP = map { $_ => 1 } qw(references workspace-symbol outline rename diagnostics);

sub run_cmd {                       # argv -> (stdout, crashed?)  [stderr dropped]
    my ($argv) = @_;
    my $pid = open(my $fh, '-|') // die "fork: $!";
    if ($pid == 0) { open(STDERR, '>', '/dev/null'); exec @$argv or exit 127; }
    local $/; my $out = <$fh> // ''; close($fh);
    my $st = $?;
    return ($out, (WIFSIGNALED($st) || ($st >> 8) == 134) ? 1 : 0);
}
sub _bn { my $s = shift; $s =~ s{^/.*/([^/]+)$}{$1}; return $s; }       # abs path -> basename
sub _text_bn { my $s = shift; $s =~ s{/\S*?/([^/\s":]+\.(?:pm|pl|t|pod))}{$1}g; return $s; }
sub normalize {
    my ($cap, $raw) = @_;
    if ($JSON_CAP{$cap}) {
        my $data = eval { decode_json($raw) };
        return _text_bn($raw) unless defined $data;
        my $walk; $walk = sub {
            my $r = ref $_[0];
            if    ($r eq 'HASH')  { my %h = map { $_ => $walk->($_[0]{$_}) } keys %{$_[0]}; return \%h; }
            elsif ($r eq 'ARRAY') { return [ map { $walk->($_) } @{$_[0]} ]; }
            else { return defined $_[0] ? _bn("$_[0]") : $_[0]; }
        };
        return JSON::PP->new->canonical->encode($walk->($data));
    }
    return _text_bn($raw);
}
# Build the batch request for a fixture row. capability -> the `q` the binary's
# --batch expects; file made absolute under the substrate; ws-symbol carries a
# query string, rename a newname, diagnostics neither.
sub batch_req {
    my ($cap, $spec, $row, $key) = @_;
    my %r = (id => $key, q => $cap);
    if ($spec->{check}) { return \%r; }
    if ($spec->{qarg})  { $r{query} = $row->{query} // ''; return \%r; }
    $r{file} = "$corpus/$row->{file}";
    $r{line} = $row->{line} // 0;
    $r{col}  = $row->{col}  // 0;
    $r{newname} = $row->{newname} // 'RENAMED' if $spec->{rename};
    return \%r;
}
# Run ALL rows through ONE --batch process (one startup), return id -> response.
sub run_batch {
    my ($reqs) = @_;   # arrayref of request hashes
    my $jsonl = join("\n", map { encode_json($_) } @$reqs) . "\n";
    my ($rd, $wr);
    pipe($rd, $wr) or die "pipe: $!";
    my $pid = open(my $out, '-|');
    die "fork: $!" unless defined $pid;
    if ($pid == 0) {
        open(STDIN, '<&', $rd); close $wr;
        open(STDERR, '>', '/dev/null');
        exec($bin, '--batch', $corpus) or exit 127;
    }
    close $rd;
    print $wr $jsonl; close $wr;       # feed all requests, then EOF
    local $/; my $resp = <$out> // ''; close($out);
    my %by_id;
    for my $line (split /\n/, $resp) {
        next unless $line =~ /\S/;
        my $r = eval { decode_json($line) } or next;
        $by_id{$r->{id}} = $r if defined $r->{id};
    }
    return \%by_id;
}

# ---- --emit: canonical output for one ad-hoc query (fixture authoring) ----
if (@ARGV && $ARGV[0] eq '--emit') {
    shift @ARGV;
    my ($cap, @rest) = @ARGV;
    my $spec = $CAP{$cap} or die "unknown capability: $cap\n";
    # line/col arrive as ARGV strings; numify so encode_json emits JSON numbers
    # (the binary's BatchReq line/col are usize and reject quoted strings).
    my $row = $spec->{check} ? {}
            : $spec->{qarg}  ? { query => $rest[0] }
            :                  { file => $rest[0], line => ($rest[1] // 0) + 0, col => ($rest[2] // 0) + 0, newname => $rest[3] };
    my $resp = run_batch([ batch_req($cap, $spec, $row, 'emit') ]);
    my $r = $resp->{emit};
    if    (!$r)        { print "<<CRASH: process aborted>>\n"; }
    elsif (!$r->{ok})  { print "<<ERR: $r->{err}>>\n"; }
    else {
        my $norm = normalize($cap, $r->{out});
        print $norm; print "\n" unless $norm =~ /\n\z/;
    }
    exit 0;
}

# ---- load fixtures ----
die "no fixtures dir: $fxdir (generate it — see README)\n" unless -d $fxdir;
my @want = grep { !/^--/ } @ARGV;
my $list_only = grep { $_ eq '--list' } @ARGV;
my @rows;
for my $jf (sort glob("$fxdir/*.json")) {
    open(my $fh, '<', $jf) or die "open $jf: $!";
    local $/; my $j = decode_json(<$fh>); close($fh);
    next if @want && !grep { $_ eq $j->{capability} } @want;
    push @rows, map { { %$_, capability => $j->{capability} } } @{ $j->{rows} || [] };
}
if ($list_only) {
    my %c; $c{$_->{capability}}{$_->{status} // 'gold'}++ for @rows;
    printf "%-20s %5s %6s %5s\n", 'capability', 'gold', 'xfail', 'prov';
    printf "%-20s %5d %6d %5d\n", $_, $c{$_}{gold}||0, $c{$_}{xfail}||0, $c{$_}{provisional}||0 for sort keys %c;
    exit 0;
}

# ---- run suite: ALL rows through one --batch process ----
my @reqs; my %meta;
for my $r (@rows) {
    my $spec = $CAP{$r->{capability}} or next;
    my $key = "$r->{capability}/$r->{id}";
    $meta{$key} = [ $r, $spec ];
    push @reqs, batch_req($r->{capability}, $spec, $r, $key);
}
my $resp = run_batch(\@reqs);

my (@fail, @xpass, @crash, @skip, @prov); my ($pass, $xfail) = (0, 0);
for my $req (@reqs) {                          # iterate in request order
    my $key = $req->{id};
    my ($r, $spec) = @{ $meta{$key} };
    my $status = $r->{status} // 'gold';
    my $rr = $resp->{$key};
    if (!$rr) { push @crash, "$key (no response — batch aborted at/before this row)"; next; }
    # ok:false is a graceful "no result" (e.g. file-not-found, no def); treat as empty output
    my $norm = $rr->{ok} ? normalize($r->{capability}, $rr->{out}) : '';
    if (!$rr->{ok} && $rr->{err} && $rr->{err} =~ /^file not found/) { push @skip, "$key ($rr->{err})"; next; }
    my $ok = 1;
    for my $s (@{ $r->{expect}{all}  || [] }) { $ok = 0, last if index($norm, $s) < 0; }
    if ($ok) { for my $s (@{ $r->{expect}{none} || [] }) { $ok = 0, last if index($norm, $s) >= 0; } }
    if ($status eq 'gold') {
        if ($ok) { $pass++ } else { push @fail, "$key\n      expect: " . encode_json($r->{expect}) . "\n      got: " . substr($norm, 0, 200) }
    } elsif ($status eq 'xfail') {
        if ($ok) { push @xpass, "$key (XPASS — promote to gold)" } else { $xfail++ }
    } else {
        push @prov, $key unless $ok;
    }
}
printf "\nGold-corpus harness\n  binary: %s\n  corpus: %s\n\n", $bin, $corpus;
printf "  %-9s %d\n", 'PASS',  $pass;
printf "  %-9s %d\n", 'xfail', $xfail;
printf "  %-9s %d\n", 'FAIL',  scalar @fail;
printf "  %-9s %d\n", 'XPASS', scalar @xpass;
printf "  %-9s %d\n", 'CRASH', scalar @crash;
printf "  %-9s %d\n", 'prov?', scalar @prov if @prov;
printf "  %-9s %d\n", 'skip',  scalar @skip if @skip;
print "\n!! CRASH (process aborted):\n",                 map { "  - $_\n" } @crash if @crash;
print "\n!! FAIL (gold assertion no longer holds):\n",   map { "  - $_\n" } @fail  if @fail;
print "\n** XPASS (known gap fixed — update fixture):\n", map { "  - $_\n" } @xpass if @xpass;
print "\nprovisional misses:\n",                         map { "  - $_\n" } @prov  if @prov;
print "\nskipped:\n",                                    map { "  - $_\n" } @skip  if @skip;
print "\n";
exit((@fail || @crash || @xpass) ? 1 : 0);
