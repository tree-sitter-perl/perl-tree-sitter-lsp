package BaseWorker;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw();

sub new {
    my ($class, %args) = @_;
    return bless { name => $args{name}, status => 'idle' }, $class;
}

sub process {
    my ($self, $data) = @_;
    return { result => 'ok', count => 42 };
}

sub status {
    my ($self) = @_;
    return $self->{status};
}

1;
