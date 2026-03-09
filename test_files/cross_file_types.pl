#!/usr/bin/perl
use strict;
use warnings;
use TestExporter qw(get_config make_items);

# Cross-file return type: get_config returns HashRef
my $cfg = get_config();
$cfg->{host};

# Cross-file hash key completion: $cfg->{} should complete with host, port, name
$cfg->{};

# Cross-file return type: make_items returns ArrayRef
my $items = make_items();

1;
