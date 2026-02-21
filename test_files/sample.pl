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

# ---- Core class (Perl 5.38+) ----

use v5.38;

class Point :isa(Base) :does(Printable) {
    field $x :param :reader;
    field $y :param;
    field $label = "point";

    method magnitude () {
        return sqrt($x**2 + $y**2);
    }

    method to_string () {
        return "$label: ($x, $y)";
    }
}

my $p = Point->new(x => 3, y => 4);
my $mag = $p->magnitude();
print $p->to_string();
