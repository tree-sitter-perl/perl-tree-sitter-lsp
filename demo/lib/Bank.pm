package Bank;
use strict;
use warnings;
use Exporter 'import';
use Account;

our @EXPORT = ('make_account');

sub make_account {
    my ( $owner, $balance ) = @_;
    return Account->new( owner => $owner, balance => $balance );
}

1;
