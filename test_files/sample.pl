#!/usr/bin/perl
use strict;
use warnings;
use Carp qw(croak);

package Calculator;

my $pi = 3.14159;
our $version = "1.0";

sub new {
    my ($class, %args) = @_;
    my $self = bless {
        history => [],
        verbose => $args{verbose} // 0,
    }, $class;
    my @args;
    $args[0];
    return $self;
}

sub add {
    my ($self, $a, $b) = @_;
    my $result = $a + $b;
    push @{$self->{history}}, "add($a, $b) = $result";
    return $result;
}

sub subtract {
    my ($self, $a, $b) = @_;
    my $result = $a - $b;
    push @{$self->{history}}, "subtract($a, $b) = $result";
    return $result;
}

sub get_history {
    my ($self) = @_;
    return @{$self->{history}};
}

sub circumference {
    my ($self, $radius) = @_;
    my $result = 2 * $pi * $radius;
    return $result;
}

package main;

my $calc = Calculator->new(verbose => 1);
my $sum = $calc->add(2, 3);
my $diff = $calc->subtract(10, 4);
my @history = $calc->get_history();

for my $entry (@history) {
    print "$entry\n";
}

print "Pi is approximately $pi\n";
print "Version: $version\n";

1;
