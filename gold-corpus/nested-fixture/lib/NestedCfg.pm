package NestedCfg;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(cfg make_items);

# Nested hash literal — tier 2's structurally-typed shape, exported so
# consumers exercise the cross-file narrowing path.
sub cfg {
    return {
        db    => { host => 'localhost', port => 5432 },
        debug => 1,
    };
}

# Array literal — tier 3's per-index tuple, cross-file.
sub make_items {
    return [ 'apple', 'banana', 'cherry' ];
}

1;
