requires 'List::Util', '>= 1.33';
requires 'Scalar::Util';
requires 'File::Basename';
requires 'JSON';
requires 'Carp';
requires 'DBI';

on test => sub {
    requires 'Test::More';
    requires 'Test::Deep';
};
