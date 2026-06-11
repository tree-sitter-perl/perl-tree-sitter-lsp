package Contract::Composer;
use Moo;
with 'Contract::Role';

sub fetch { return 42 }

1;
