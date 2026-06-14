package My::Plugin::Orphan;
use Mojo::Base 'Mojolicious::Plugin';
sub register { my ($self,$app)=@_; $app->helper(orphan_h => sub {1}); }
1;
