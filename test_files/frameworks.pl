#!/usr/bin/perl
use strict;
use warnings;

# ── Moo class with accessor and role ──

package MooApp;
use Moo;
with 'MyRole::Logging';

has name => (is => 'ro');
has count => (is => 'rw');

sub greet {
    my ($self) = @_;
    return "Hello, " . $self->name;
}

# ── Mojo::Base class with has defaults ──

package MojoApp;
use Mojo::Base -base;

has title => 'Untitled';
has items => sub { [] };

sub describe {
    my ($self) = @_;
    return $self->title;
}

# ── DBIC ResultSet with load_components ──

package MySchema::ResultSet::User;
use base 'DBIx::Class::Core';
__PACKAGE__->load_components('Helper::ResultSet::Shortcut');

sub active {
    my ($self) = @_;
    return $self;
}

# ── Perl class keyword with :does ──

class Printable {
    method to_string() { return "printable" }
}

class Report :does(Printable) {
    method generate() { return "report" }
}

# ── Usage lines for testing ──

package main;

my $moo = MooApp->new(name => 'alice', count => 0);
$moo->name();
$moo->count(5);
$moo->greet();
$moo->log_info("started");

my $mojo = MojoApp->new();
$mojo->title();
$mojo->items();
$mojo->describe();

my $rs = MySchema::ResultSet::User->new();
$rs->active();
$rs->columns();

my $report = Report->new();
$report->generate();
$report->to_string();

# ── Dynamic method dispatch via constant folding ──

my $accessor = 'name';
$moo->$accessor();

# ── Constant-based exports ──

use constant MY_EXPORTS => qw(encode decode);
my @BASE = qw(connect disconnect);
our @EXPORT_OK = (MY_EXPORTS, @BASE, 'status');

1;
