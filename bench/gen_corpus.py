#!/usr/bin/env python3
"""Generate a synthetic Perl workspace that stresses cross-file enrichment.

Shape (worst case for "always enriched" invalidation blast radius):
  - A deep base chain:  Base0 <- Base1 <- ... <- Base{D-1}  (each isa the prev)
  - A wide consumer fan: W modules, each `use`s the deep base tip + a few peers
    and binds variables to imported return values (so enrichment has real work:
    imported return types + hash keys propagate into each consumer).

Editing Base0 should, in an always-enriched world, invalidate every descendant
and every consumer that transitively sees its types -> the blast radius we want
to measure.
"""
import os, sys, random

root = sys.argv[1] if len(sys.argv) > 1 else "/tmp/bench-corpus"
D = int(sys.argv[2]) if len(sys.argv) > 2 else 40      # base chain depth
W = int(sys.argv[3]) if len(sys.argv) > 3 else 300     # consumer fan width
random.seed(7)

lib = os.path.join(root, "lib")
os.makedirs(lib, exist_ok=True)

def write(relpath, text):
    p = os.path.join(lib, relpath)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    with open(p, "w") as f:
        f.write(text)

# Deep base chain. Each level adds a Moo-style accessor and a sub returning a
# hashref with documented keys, so enrichment must thread types down the chain.
for i in range(D):
    parent = f"use parent -norequire, 'Base::Level{i-1}';" if i > 0 else ""
    write(f"Base/Level{i}.pm", f"""package Base::Level{i};
use strict; use warnings;
use Moo;
{parent}

has attr{i} => (is => 'rw');

sub make_config_{i} {{
    my ($self) = @_;
    return {{ host_{i} => 'localhost', port_{i} => {1000+i}, level => {i} }};
}}

sub compute_{i} {{
    my ($self, $x) = @_;
    my $cfg = $self->make_config_{i};
    return $cfg->{{port_{i}}} + $x;
}}

1;
""")

tip = f"Base::Level{D-1}"

# Wide consumer fan. Each consumer uses the deep tip + a couple of peers,
# binds vars to imported/return values and reads hash keys -> enrichment work.
for w in range(W):
    peers = random.sample(range(W), k=min(3, W))
    peer_uses = "\n".join(
        f"use Consumer::C{p};" for p in peers if p != w
    )
    write(f"Consumer/C{w}.pm", f"""package Consumer::C{w};
use strict; use warnings;
use Moo;
extends '{tip}';
{peer_uses}

has name{w} => (is => 'ro', default => sub {{ 'c{w}' }});

sub run {{
    my ($self) = @_;
    my $cfg = $self->make_config_{D-1};
    my $port = $cfg->{{port_{D-1}}};
    my $total = $self->compute_{D-1}($port);
    return {{ result => $total, who => $self->name{w} }};
}}

sub helper_{w} {{
    my ($self, @args) = @_;
    return scalar(@args) + {w};
}}

1;
""")

# A top-level app script tying it together.
with open(os.path.join(root, "app.pl"), "w") as f:
    f.write("#!/usr/bin/env perl\nuse strict; use warnings;\n")
    for w in range(min(W, 50)):
        f.write(f"use Consumer::C{w};\n")
    f.write(f"""
my $c = Consumer::C0->new;
my $out = $c->run;
print $out->{{result}}, "\\n";
""")

total = D + W + 1
print(f"Generated {total} files into {root} (D={D} base chain, W={W} consumers)")
