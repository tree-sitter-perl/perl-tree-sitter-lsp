package Contract::Role;
use Moo::Role;

requires 'fetch';

sub process {
    my $self = shift;
    return $self->fetch + 1;
}

1;
