package Mojolicious::Plugin::Confy;
use Mojo::Base 'Mojolicious::Plugin';

sub register {
    my ($self, $app, $conf) = @_;
    my $a = $conf->{alpha};
    my $oops = $conf->{alpa};
    $app->helper(confy_thing => sub { 1 });
}

1;
