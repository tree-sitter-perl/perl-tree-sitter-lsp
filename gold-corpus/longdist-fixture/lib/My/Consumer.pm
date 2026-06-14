package My::Consumer;
use Mojo::Base 'Mojolicious::Controller';

sub act {
    my $self = shift;
    return $self->widget_count;
}

1;

package My::Consumer2;
use Mojo::Base 'Mojolicious::Controller';
sub act2 { my $self = shift; return $self->orphan_h; }
1;
