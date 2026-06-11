package My::Composer;
use Moo;
with 'My::Role';

sub fetch { return 42 }

1;
