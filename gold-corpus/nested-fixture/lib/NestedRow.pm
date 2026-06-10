package NestedRow;
use strict;
use warnings;
use parent 'DBIx::Class::Core';

__PACKAGE__->add_columns(
    name  => { data_type => 'varchar' },
    email => { data_type => 'varchar' },
);

1;
