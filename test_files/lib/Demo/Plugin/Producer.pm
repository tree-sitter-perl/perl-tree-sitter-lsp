package Demo::Plugin::Producer;

# ============================================================================
# The event-producing side of the cross-file plugin demo.
#
# This module extends Mojo::EventEmitter and wires up three events in its
# constructor. The `mojo-events.rhai` Rhai plugin fires on each ->on call
# and synthesizes:
#
#   * HashKeyDef  — "ready"/"failed"/"done" as a def, owner=Class("Demo::Plugin::Producer")
#   * HashKeyAccess — same location (so `gr` on the name finds the wire-up)
#   * Method — on_ready / on_failed / on_done handler symbols (visible in outline)
#
# The consumer file (test_files/plugin_events_consumer.pl) calls
# $p->emit('ready') — Rhai emits a matching HashKeyAccess with the same
# owner+name. Cross-file `gr` on 'ready' finds BOTH sides.
# ============================================================================

use strict;
use warnings;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self  = bless { queue => [] }, $class;

    # ── Handler params are captured and become signature help ──
    # When cursor lands in the args of `->emit('ready', CURSOR)`, the LSP
    # surfaces THIS handler's params ($ts, $who). Stacked wire-ups (see
    # the second ->on('ready') below) contribute separate SignatureInfo
    # entries — the user sees every handler shape they could dispatch to.
    $self->on('ready', sub {
        my ($self_in, $ts, $who) = @_;
        warn "producer ready at $ts for $who";
    });

    $self->on('failed', sub {
        my ($self_in, $err, $retriable) = @_;
        warn "producer failed: $err (retriable=$retriable)";
    });

    # Second ->on('ready') — different signature shape! `gd` on 'ready' in
    # Consumer's ->emit can jump to either wire-up; sig help stacks both.
    $self->on('ready', sub {
        my ($self_in, $ts) = @_;
        warn "second ready handler: $ts";
    });

    $self->once('done', sub {
        my ($self_in, $reason) = @_;
        warn "producer done: $reason";
    });

    return $self;
}

sub announce {
    my $self = shift;
    # Internal emit — also resolves across files to Consumer's ->emit.
    $self->emit('ready');
}

1;
