package My::Consumer;
use Mojo::Base 'Mojolicious::Controller';

sub act {
    my $self = shift;
    return $self->widget_count;
}

1;
