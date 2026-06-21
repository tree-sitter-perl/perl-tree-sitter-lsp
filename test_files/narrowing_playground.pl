#!/usr/bin/perl
use strict;
use warnings;
use Scalar::Util qw(blessed reftype);

# ============================================================
# Flow-sensitive Narrowing — Playground / Executable Spec
# ============================================================
#
# Design: docs/prompt-flow-narrowing.md
#
# A guard expression refines a variable's type for the region it
# dominates, then the type widens back at the region's exit. This
# file is the authoring source for the per-row tests.
#
# Notation:
#
#   # NARROW $x => Foo      inside this region, $x narrows to Foo
#   # WIDE   $x             back to the outer (un-narrowed) type
#   # NO-NARROW             guard recognized but v1 emits nothing
#                           (negative polarity, or no lattice target)
#
# "Foo" = ClassName(Foo); HashRef/ArrayRef/CodeRef/Regexp as named.
# The outer type is whatever the parameter/assignment gave (often
# Unknown — narrowing's job is to make the inside precise anyway).
#
# ✅ rows MUST narrow in v1.  ✘ rows MUST NOT (over-narrow guards).
# ============================================================

# ── A. Block guard, positive (✅) ────────────────────────────

sub block_isa {
    my ($x) = @_;                       # WIDE $x (Unknown)
    if ($x->isa('Foo')) {
        $x->render;                     # NARROW $x => Foo
    }
    $x->render;                         # WIDE $x  (un-narrowed past block)
}

sub block_ref_eq_class {
    my ($x) = @_;
    if (ref($x) eq 'Point') {
        $x->coords;                     # NARROW $x => Point
    }
}

sub block_ref_eq_reftype {
    my ($x) = @_;
    if (ref($x) eq 'HASH') {
        my $v = $x->{key};              # NARROW $x => HashRef
    }
    if (ref($x) eq 'ARRAY') {
        my $v = $x->[0];                # NARROW $x => ArrayRef
    }
    if (ref($x) eq 'CODE') {
        $x->();                         # NARROW $x => CodeRef
    }
    if (ref($x) eq 'Regexp') {
        'str' =~ $x;                    # NARROW $x => Regexp
    }
}

sub block_does_role {
    my ($x) = @_;
    if ($x->DOES('Drawable')) {
        $x->draw;                       # NARROW $x => Drawable
    }
}

# ── B. Early-exit guard, positive (✅) ───────────────────────
#    Narrowing region = the REST OF THE BLOCK after the guard.

sub assert_unless {
    my ($x) = @_;                       # WIDE $x
    return unless $x->isa('Widget');
    $x->layout;                         # NARROW $x => Widget (remainder)
    $x->paint;                          # NARROW $x => Widget
}

sub assert_or_die {
    my ($x) = @_;
    $x->isa('Session') or die "not a session";
    $x->user;                           # NARROW $x => Session (remainder)
}

sub assert_ref_unless {
    my ($cfg) = @_;
    return unless ref($cfg) eq 'HASH';
    my $port = $cfg->{port};            # NARROW $cfg => HashRef (remainder)
}

sub assert_negated {
    my ($x) = @_;
    die unless !$x->isa('Banned');      # !G under `unless` => G... careful:
    # `unless !isa(Banned)` exits when isa is true → remainder: NOT Banned.
    # NO-NARROW (negative polarity — deferred)
    $x->go;
}

# ── C. Negation flips polarity ───────────────────────────────

sub negated_block_else_deferred {
    my ($x) = @_;
    if (!$x->isa('Foo')) {
        warn "nope";                    # NO-NARROW (region asserts NOT Foo)
    } else {
        $x->render;                     # else of !G asserts G — ✅ if/when
                                        # else-region emission lands; v1 may
                                        # skip else entirely. NARROW $x => Foo
    }
}

# ── D. Negative-space guardrails — MUST NOT narrow (✘) ───────

sub else_branch_negative {
    my ($x) = @_;
    if ($x->isa('Foo')) {
        $x->render;                     # NARROW $x => Foo
    } else {
        $x->fallback;                   # NO-NARROW ($x is NOT Foo; deferred)
    }
}

sub unless_block_negative {
    my ($x) = @_;
    unless ($x->isa('Foo')) {
        $x->fallback;                   # NO-NARROW (region asserts NOT Foo)
    }
}

sub return_if_negative {
    my ($x) = @_;
    return if $x->isa('Legacy');
    $x->modern;                         # NO-NARROW (remainder is NOT Legacy)
}

sub or_disjunction_no_narrow {
    my ($x) = @_;
    if ($x->isa('Foo') || $x->isa('Bar')) {
        $x->common;                     # NO-NARROW (could be Foo OR Bar — no
                                        # join in v1 lattice; ✘ over-narrow)
    }
}

# ── E. Compound conjunction — intersection narrows ───────────

sub and_conjunction {
    my ($x) = @_;
    if ($x->isa('Foo') && $x->can('extra')) {
        $x->extra;                      # NARROW $x => Foo (from the isa
                                        # conjunct; `can` ignored)
    }
}

# ── F. Reassignment inside the region (temporal soundness) ───

sub reassign_in_region {
    my ($x) = @_;
    if ($x->isa('Foo')) {
        $x->before;                     # NARROW $x => Foo
        $x = Bar->new;                  # later-starting witness
        $x->after;                      # $x => Bar (reassignment wins by
                                        # temporal order, NOT Foo)
    }
}

# ── G. Weak guards — recognized, no v1 target (Optional dep) ─

sub weak_defined {
    my ($x) = @_;                       # outer: ideally Optional<T> someday
    return unless defined $x;
    $x->use;                            # NO-NARROW in v1; becomes T once
                                        # Optional<T> lands (defined strips undef)
}

sub weak_blessed {
    my ($x) = @_;
    if (blessed $x) {
        $x->method;                     # NO-NARROW in v1 (no "object" type);
                                        # future: narrows undef/non-ref away
    }
}

# ── P. Place subjects (v1b) — narrow a slot, not a variable ──
#    Canonical access path = root + constant projections. The
#    narrowing region is TRUNCATED at the first op that could
#    disturb the slot (emit-time invalidation scan).

sub place_block_isa {
    my ($self) = @_;
    if ($self->{handler}->isa('Foo')) {
        $self->{handler}->render;       # NARROW $self->{handler} => Foo
    }
    $self->{handler}->render;           # WIDE (past block)
}

sub place_assert_remainder {
    my ($self) = @_;
    return unless ref($self->{cfg}) eq 'HASH';
    my $port = $self->{cfg}->{port};    # NARROW $self->{cfg} => HashRef
}

sub place_method_on_value_ok {
    my ($self) = @_;
    return unless $self->{db}->isa('Schema');
    $self->{db}->connect;               # NARROW $self->{db} => Schema
    $self->{db}->query;                 # NARROW still (method on the VALUE
                                        # doesn't disturb the slot)
}

sub place_invalidated_by_prefix_write {
    my ($self) = @_;
    return unless $self->{x}->isa('Foo');
    $self->{x}->step;                   # NARROW $self->{x} => Foo
    $self->{x} = make_other();          # WRITE to the place → truncates here
    $self->{x}->step;                   # WIDE (re-widened past the write)
}

sub place_invalidated_by_opaque_prefix {
    my ($self) = @_;
    return unless $self->{x}->isa('Foo');
    $self->{x}->step;                   # NARROW $self->{x} => Foo
    $self->reset;                       # opaque op on prefix $self → truncates
    $self->{x}->step;                   # WIDE (couldn't prove slot survived)
}

sub place_dynamic_key_no_narrow {
    my ($self, $k) = @_;
    return unless $self->{$k}->isa('Foo');
    $self->{$k}->step;                  # NO-NARROW (dynamic key — not a
                                        # stable place identity)
}

sub place_bind_to_lexical_already_works {
    my ($self) = @_;
    my $h = $self->{handler};           # v1a escape hatch: bind the slot
    return unless $h->isa('Foo');
    $h->render;                         # NARROW $h => Foo (plain Variable)
}

# ── H. The complement: a function whose return WANTS Optional ─
#    Today this folds to None (arm disagreement). With Optional<Foo>
#    it returns Foo|undef, and weak_defined above resolves it.

sub maybe_make {
    my ($ok) = @_;
    return undef unless $ok;            # arm: undef
    return Foo->new;                    # arm: Foo
    # v1 return type: None (disagreement). Future: Optional<Foo>.
}

1;
