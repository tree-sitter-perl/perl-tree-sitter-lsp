#!/usr/bin/perl
# ============================================================================
# CROSS-FILE PLUGIN DEMO — consumer side.
#
# Events are defined in test_files/lib/Demo/Plugin/Producer.pm.
# This file consumes them — calls ->emit / ->unsubscribe on a Producer
# instance. The `mojo-events.rhai` Rhai plugin fires on each call, resolves
# $p's type via mid-build type inference (`my $p = Producer->new` pushes a
# TypeConstraint), and emits HashKeyAccess refs with
# owner=Class("Demo::Plugin::Producer").
#
# Try in nvim (requires `cargo build --release`):
#
#   1. Put the cursor on any 'ready' string below.
#   2. `gd` — jumps to the ->on('ready', ...) def in Producer.pm.
#   3. `gr` — lists ALL occurrences across both files: the two ->on
#      wire-ups in Producer.pm + the ->emit + ->unsubscribe here.
#   4. Repeat on 'failed' or 'done' — same story.
#
# Const folding also works: `my $evt = 'ready'; $p->emit($evt)` is indexed
# as an access to 'ready', not to '$evt'. Proof that the builder's folding
# composes through the plugin boundary.
# ============================================================================

package Demo::Plugin::Consumer;

use strict;
use warnings;
use lib 'test_files/lib';
use Demo::Plugin::Producer;
use parent 'Mojo::EventEmitter';

sub run {
    my $p = Demo::Plugin::Producer->new;

    # Cursor right after the comma below and hit signature-help trigger.
    # The LSP looks up the event name 'ready' against Demo::Plugin::Producer's
    # HashKeyDefs (cross-file!), finds TWO stacked handler registrations,
    # and shows both signatures so you know either shape is live:
    #   Demo::Plugin::Producer::ready($ts, $who)
    #   Demo::Plugin::Producer::ready($ts)
    $p->emit('ready', );

    # Different handler, different signature — sig help shows
    # Demo::Plugin::Producer::failed($err, $retriable).
    $p->emit('failed', 'network timeout', );

    # Const-folded name through a scalar — plugin still emits correctly
    # because the builder's `constant_strings` table resolves $evt before
    # the plugin sees the argument. Sig help on the `,` below offers
    # Demo::Plugin::Producer::done($reason).
    my $evt = 'done';
    $p->emit($evt, );

    # Reference without registering — only HashKeyAccess, no def. gd on
    # 'ready' here jumps to the def in Producer.pm.
    $p->has_subscribers('ready');
    $p->unsubscribe('ready');
}

# Consumer also has its own events (as an EventEmitter subclass itself).
# These are distinct from Producer's — owner=Class("Demo::Plugin::Consumer").
sub wire_self_events {
    my $self = shift;
    $self->on('started', sub { warn "consumer started" });
    $self->on('stopped', sub { warn "consumer stopped" });

    # `my $kickoff = 'started'; $self->emit($kickoff)` — const-folded.
    my $kickoff = 'started';
    $self->emit($kickoff);
}

1;
