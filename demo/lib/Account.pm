package Account;
use Moo;

has owner   => ( is => 'ro' );
has balance => ( is => 'rw', default => 0 );

sub deposit {
    my ( $self, $amount ) = @_;
    $self->balance( $self->balance + $amount );
    return $self;
}

sub describe {
    my $self = shift;
    return sprintf( '%s has $%d', $self->owner, $self->balance );
}

1;
