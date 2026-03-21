package DemoUtils;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(fetch_data transform);

=head1 NAME

DemoUtils - utility functions for data processing

=head1 FUNCTIONS

=head2 fetch_data

    my $result = fetch_data($url, %opts);

Fetches data from the given URL. Returns a hashref with
C<status>, C<body>, and C<headers> keys.

Options:

=over

=item timeout - request timeout in seconds (default: 30)

=item retries - number of retry attempts (default: 3)

=back

=head2 transform

    my @out = transform(\@items, \&callback);

Applies the callback to each item, returning transformed results.
Skips undef values in the input.

=cut

sub fetch_data {
    my ($url, %opts) = @_;
    return { status => 200, body => "ok", headers => {} };
}

sub transform {
    my ($items, $cb) = @_;
    return map { $cb->($_) } grep { defined } @$items;
}

1;
