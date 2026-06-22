use strict;
use warnings;
use lib 'lib';
use Bank;

my $acct = make_account( 'Ada', 100 );

$acct->deposit(50);

print $acct->describe, "\n";
