package Users;
use strict;
use warnings;

sub list {
    my ($c) = @_;
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
