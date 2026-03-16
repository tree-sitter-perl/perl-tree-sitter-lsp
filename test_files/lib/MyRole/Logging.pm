package MyRole::Logging;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw();

sub log_info {
    my ($self, $msg) = @_;
    print "[INFO] $msg\n";
}

sub log_error {
    my ($self, $msg) = @_;
    print "[ERROR] $msg\n";
}

1;
