package Contract::Broken;
use Moo;
with 'Contract::Role';

sub other { return 1 }

1;
