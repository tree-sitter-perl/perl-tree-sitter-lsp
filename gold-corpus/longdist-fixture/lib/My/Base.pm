package My::Base;
use Moo;

sub init {
    my ($self) = @_;
    $self->{conn} = My::Conn->new;
}

1;
