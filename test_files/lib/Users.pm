package Users;
use strict;
use warnings;
use parent 'Mojolicious::Controller';

sub list {
    my ($c) = @_;
    # Because `Users` declares inheritance from Mojolicious::Controller,
    # $c here is typed as the enclosing class — Users — and inheritance
    # resolves method calls through Controller. `$c->render` picks up
    # Mojo's own render() plus any helper synthesized via `$app->helper`.
    $c->render(json => [{ id => 1, name => 'alice' }]);
}

sub create {
    my ($c) = @_;
    # Cursor on 'list' in plugin_mojo_demo.pl's `->to('Users#list')` jumps
    # HERE, cross-file. Same mechanism as any method ref — the plugin just
    # emitted a MethodCall ref pointing at Users::list; goto-def did the
    # rest via normal resolution.
}

1;
