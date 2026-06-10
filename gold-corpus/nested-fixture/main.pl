use strict;
use warnings;
use lib 'lib';
use NestedCfg qw(cfg make_items);

# Tier 2: structural hash narrowing through an assignment hop and a
# direct double-drill.
my $config = cfg();
my $db     = $config->{db};
my $host   = $db->{host};
my $port   = $config->{db}->{port};

# Tier 3: array tuples + the mixed drill.
my $items = make_items();
my $first = $items->[0];
my $obj   = { users => [ { name => 'A', id => 1 } ] };
my $uname = $obj->{users}->[0]->{name};
my $uid   = $obj->{users}->[0]->{id};

# Tier 1: $row->{col} on a typed row resolves to the column def.
my $rs  = $schema->resultset('NestedRow');
my $row = $rs->find(1);
my $n   = $row->{name};

print "$host $port $first $uname $uid $n\n";
