package My::Broken;
use Moo;
with 'My::Role';

sub other { return 1 }

1;
