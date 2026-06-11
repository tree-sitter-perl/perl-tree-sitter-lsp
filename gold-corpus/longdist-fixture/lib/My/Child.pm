package My::Child;
use Moo;
extends 'My::Base';

sub go {
    my ($self) = @_;
    my $x = $self->{conn};
    return $x;
}

1;
