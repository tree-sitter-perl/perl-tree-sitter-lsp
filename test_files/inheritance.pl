#!/usr/bin/perl
use strict;
use warnings;

# ── Local inheritance chain: Animal → Dog → GoldenRetriever ──

package Animal;
sub new { bless {}, shift }
sub speak {
    my ($self) = @_;
    return "...";
}
sub breathe { return 1; }

package Dog;
use parent -norequire, 'Animal';
sub fetch {
    my ($self) = @_;
    return "stick";
}
sub speak {
    my ($self) = @_;
    return "Woof!";
}

package GoldenRetriever;
use parent -norequire, 'Dog';
sub be_friendly {
    my ($self) = @_;
    return "tail wag";
}

# ── Cross-file inheritance: MyWorker → BaseWorker ──

package MyWorker;
use parent 'BaseWorker';
sub run {
    my ($self) = @_;
    return $self->process({ input => 1 });
}

# ── Usage lines for testing ──

package main;
my $dog = Dog->new();
$dog->speak();
$dog->fetch();

my $golden = GoldenRetriever->new();
$golden->speak();
$golden->be_friendly();
$golden->breathe();

my $worker = MyWorker->new();
$worker->run();
$worker->process();

1;
