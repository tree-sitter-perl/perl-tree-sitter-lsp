#!/usr/bin/perl
# ============================================================================
# FRAMEWORK PLUGIN DEMO — mojo-events.rhai
#
# Built-in Rust knows nothing about Mojo event emitters. The `mojo-events`
# plugin (frameworks/mojo-events.rhai, ~80 lines of Rhai) teaches the LSP
# that `->on('evt', sub {})` is a *definition* of an event name and
# `->emit('evt', ...)` is a *reference* to it — just like how hash-key
# definitions and accesses link to each other.
#
# What to try in nvim (after `cargo build --release && ./dev.sh`):
#
#   1. Put the cursor on any 'connect'/'message'/'disconnect'/'started' string.
#   2. `gr` (references): every wire-up and every emit-site for that event.
#   3. `gd` (goto-def): jumps to the nearest ->on call — the wire-up site.
#   4. `:Telescope lsp_document_symbols` — the plugin-synthesized
#      `on_connect`, `on_message`, etc. handler methods appear in the outline
#      alongside the regular subs.
#
# CRUCIALLY: this works without any Rust code touching events. The plugin
# emits Symbol + Ref entries via the same EmitAction protocol any third
# party can use. The entire events feature is ~80 lines of Rhai.
# ============================================================================

package Demo::Plugin::Events;

use strict;
use warnings;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless { subscribers => 0 }, $class;

    # ── Plain literal event names ──
    # Each wire-up emits a HashKeyDef + HashKeyAccess + EventHandler symbol.
    # The sub's PARAMS are captured too — sig help on ->emit('connect', CURSOR)
    # surfaces ($sock, $remote_ip), and ->emit('message', CURSOR) surfaces
    # ($msg, $headers). Cursor *inside* the sub also has the params typed.
    $self->on('connect', sub {
        my ($self_in, $sock, $remote_ip) = @_;
        warn "connected $remote_ip";
    });

    $self->on('message', sub {
        my ($self_in, $msg, $headers) = @_;
        warn "got message: $msg";
    });

    # ── Constant-folded event name ──
    # `$evt_name` resolves through the builder's constant_strings table;
    # the plugin receives arg.string_value == "disconnect" and emits a
    # symbol named `disconnect`, not `$evt_name`. Proof that const folding
    # composes through the plugin boundary.
    my $evt_name = 'disconnect';
    $self->on($evt_name, sub {
        my ($self_in) = @_;
        warn "bye";
    });

    # ── Same event name used twice — two wire-ups, one event ──
    # Cursor on 'connect' below, `gr` should include BOTH wire-ups + the
    # emit-site in `fire_connect` further down.
    $self->on('connect', sub {
        my ($self_in) = @_;
        warn "also connected";
    });

    # ── ->once is a def too (one-shot wire-up) ──
    $self->once('started', sub {
        my ($self_in) = @_;
        warn "just this once";
    });

    return $self;
}

# Emit-sites reference events without defining them. Cursor on 'connect'
# here → `gd` jumps to the first ->on('connect', ...) in new() above.
# Cursor AFTER the comma (with sig help trigger) → shows the handler sig:
#   Demo::Plugin::Events::connect($sock, $remote_ip)
sub fire_connect {
    my $self = shift;
    $self->emit('connect', );     # ← sig help shows ($sock, $remote_ip)
}

sub fire_message {
    my ($self, $msg) = @_;
    $self->emit('message', $msg, );  # ← sig help shows ($msg, $headers),
                                     #   active_param=1 lands on $headers
}

sub stop_listening {
    my $self = shift;
    $self->unsubscribe('connect');   # ← also a reference
    $self->unsubscribe('message');
}

# Emit a dynamic name — plugin skips this. No spurious symbol gets created
# for `$dynamic`.
sub fire_dynamic {
    my ($self, $dynamic) = @_;
    $self->emit($dynamic);
}

1;
