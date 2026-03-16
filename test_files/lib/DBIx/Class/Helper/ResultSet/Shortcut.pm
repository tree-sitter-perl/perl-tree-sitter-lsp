package DBIx::Class::Helper::ResultSet::Shortcut;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw();

sub columns {
    my ($self, @cols) = @_;
    return $self;
}

sub order_by {
    my ($self, @cols) = @_;
    return $self;
}

sub limit {
    my ($self, $count) = @_;
    return $self;
}

1;
