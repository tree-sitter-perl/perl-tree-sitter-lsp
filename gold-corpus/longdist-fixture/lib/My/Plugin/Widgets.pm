package My::Plugin::Widgets;
use Mojo::Base 'Mojolicious::Plugin';

sub register {
    my ($self, $app, $conf) = @_;
    $app->helper(widget_count => sub { 42 });
}

1;
