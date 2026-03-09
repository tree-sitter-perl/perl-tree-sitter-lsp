package TestExporter;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(get_config make_items build_widget);

sub get_config {
    return { host => "localhost", port => 5432, name => "mydb" };
}

sub make_items {
    return ["apple", "banana", "cherry"];
}

sub build_widget {
    return bless { width => 100, height => 50 }, 'Widget';
}

1;
