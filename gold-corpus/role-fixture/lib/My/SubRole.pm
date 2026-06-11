package My::SubRole;
use Moo::Role;
with 'My::Role';

requires 'fetch';

1;
