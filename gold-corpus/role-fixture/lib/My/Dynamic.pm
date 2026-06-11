package My::Dynamic;
use Moo;
with RoleGen(type => 'x');
with 'My::Role';

sub other { return 2 }

1;
