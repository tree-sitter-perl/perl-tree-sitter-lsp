#!/usr/bin/perl
use strict;
use warnings;

# ============================================================
# Type Constraint Discovery — Obstacle Course
# ============================================================
#
# Each section exercises a type inference pattern. Comments show
# the expected inferred type using this notation:
#
#   # => HashRef          scalar is a hash reference
#   # => ArrayRef         scalar is an array reference
#   # => CodeRef          scalar is a code reference
#   # => Object(Foo)      scalar is a blessed Foo instance
#   # => Numeric          scalar is used as a number
#   # => String           scalar is used as a string
#   # => Unknown          no constraint discovered
#
# "=>" on the LHS variable means "after this line, the variable
# has this constraint." Multiple constraints narrow the type.
#
# See docs/type-constraint-discovery.md for the full spec.
# ============================================================

# ── 1. Literal constructors ──────────────────────────────────

my $href = {};                          # $href => HashRef
my $aref = [];                          # $aref => ArrayRef
my $cref = sub { 42 };                  # $cref => CodeRef
my $re   = qr/pattern/;                # $re   => Regexp

my $href2 = { a => 1, b => 2 };        # $href2 => HashRef
my $aref2 = [1, 2, 3];                 # $aref2 => ArrayRef

# ── 2. Arrow dereference operators ──────────────────────────

sub arrow_deref {
    my ($data) = @_;

    $data->{key};                       # $data => HashRef
    $data->{key}{nested};               # $data => HashRef (already known)

    my ($list) = @_;
    $list->[0];                         # $list => ArrayRef
    $list->[-1];                        # $list => ArrayRef (already known)

    my ($callback) = @_;
    $callback->(1, 2);                  # $callback => CodeRef
}

# ── 3. Block dereference syntax ──────────────────────────────

sub block_deref {
    my ($x, $y, $z) = @_;

    my @items = @{$x};                  # $x => ArrayRef
    my %table = %{$y};                  # $y => HashRef
    &{$z}();                            # $z => CodeRef

    # Postfix deref (5.20+)
    my @items2 = $x->@*;               # $x => ArrayRef
    my %table2 = $y->%*;               # $y => HashRef
}

# ── 4. Object method calls ──────────────────────────────────

package User;
sub new { bless {}, shift }
sub name { }
sub email { }

package main;

my $user = User->new();                 # $user => Object(User)
$user->name();                          # $user => Object(User) (consistent)
$user->email();                         # $user => Object(User) (consistent)

# Unknown class — still an object
my $thing = Some::Class->new();         # $thing => Object(Some::Class)
$thing->frobnicate();                   # $thing => Object(Some::Class)

# ── 5. Bless patterns ───────────────────────────────────────

package Widget;

sub new {
    my ($class, %args) = @_;
    my $self = bless {}, $class;        # $self => Object(Widget) via bless
    return $self;
}

sub new_explicit {
    my ($class) = @_;
    my $self = bless {
        name => "default",
    }, 'Widget';                        # $self => Object(Widget) via bless literal
    return $self;
}

# ── 6. Return value tracking ────────────────────────────────

package Config;

sub new { bless {}, shift }

sub get_db_config {
    my ($self) = @_;
    return {                            # return => HashRef
        host => "localhost",
        port => 5432,
    };
}

sub get_tags {
    my ($self) = @_;
    return [qw(perl lsp tree-sitter)]; # return => ArrayRef
}

sub get_handler {
    my ($self) = @_;
    return sub { print "handled\n" };  # return => CodeRef
}

sub get_name {
    my ($self) = @_;
    return $self->{name};               # return => Unknown (hash value)
}

package main;

my $cfg = Config->new();
my $db  = $cfg->get_db_config();        # $db  => HashRef (from return type)
my $tags = $cfg->get_tags();            # $tags => ArrayRef (from return type)
my $handler = $cfg->get_handler();      # $handler => CodeRef (from return type)
$db->{host};                            # consistent: $db is HashRef
push @{$tags}, "new";                   # consistent: $tags is ArrayRef
$handler->();                           # consistent: $handler is CodeRef

# ── 7. Chained access ───────────────────────────────────────

my $app = {
    config => {
        db => { host => "localhost" },
        workers => [1, 2, 3],
    },
};                                      # $app => HashRef

$app->{config};                         # $app => HashRef
$app->{config}{db};                     # $app->{config} => HashRef
$app->{config}{db}{host};              # $app->{config}{db} => HashRef
$app->{config}{workers}[0];            # $app->{config}{workers} => ArrayRef

# ── 8. Operator-based type narrowing ────────────────────────

sub numeric_ops {
    my ($x) = @_;
    my $a = $x + 1;                     # $x => Numeric
    my $b = $x - 1;                     # $x => Numeric
    my $c = $x * 2;                     # $x => Numeric
    my $d = $x / 2;                     # $x => Numeric
    my $e = $x % 3;                     # $x => Numeric
    my $f = $x ** 2;                    # $x => Numeric
    $x++;                               # $x => Numeric
    $x--;                               # $x => Numeric
    my $g = abs($x);                    # $x => Numeric
    my $h = int($x);                    # $x => Numeric
    my $i = sqrt($x);                   # $x => Numeric
}

sub string_ops {
    my ($s) = @_;
    my $a = $s . " world";             # $s => String
    my $b = $s x 3;                     # $s => String
    my $c = uc($s);                     # $s => String
    my $d = lc($s);                     # $s => String
    my $e = length($s);                 # $s => String
    my $f = substr($s, 0, 5);          # $s => String
    my $g = index($s, "needle");       # $s => String
    chomp $s;                           # $s => String
    chop $s;                            # $s => String
    $s =~ /pattern/;                    # $s => String
    $s =~ s/old/new/;                   # $s => String
}

sub comparison_ops {
    my ($x, $y) = @_;

    # Numeric comparisons — both sides are Numeric
    if ($x == $y) { }                   # $x => Numeric, $y => Numeric
    if ($x != $y) { }
    if ($x <=> $y) { }
    if ($x < $y) { }
    if ($x > $y) { }
    if ($x <= $y) { }
    if ($x >= $y) { }

    # String comparisons — both sides are String
    if ($x eq $y) { }                   # $x => String, $y => String
    if ($x ne $y) { }
    if ($x lt $y) { }
    if ($x gt $y) { }
    if ($x cmp $y) { }
}

# ── 9. Mixed / conflicting constraints ──────────────────────

sub mixed_use {
    my ($val) = @_;

    # Perl coerces freely — these are all valid
    my $n = $val + 0;                   # $val => Numeric
    my $s = $val . "";                  # $val => String (also Numeric — Perl coerces)
    # Both constraints are valid. Don't warn, just track both.
}

# ── 10. Re-assignment changes type ──────────────────────────

sub reassignment {
    my $x = {};                         # $x => HashRef
    $x->{key} = 1;                      # $x => HashRef (consistent)

    $x = [];                            # $x => ArrayRef (type changed!)
    $x->[0] = "hello";                 # $x => ArrayRef (consistent)

    $x = Some::Class->new();            # $x => Object(Some::Class)
    $x->method();                       # $x => Object(Some::Class)
}

# ── 11. Conditional / ambiguous types ────────────────────────

sub conditional_type {
    my ($flag) = @_;

    # Type depends on runtime condition — can't statically resolve
    my $x = $flag ? {} : [];            # $x => Unknown (HashRef | ArrayRef)

    # But usage afterwards narrows it
    $x->{key};                          # $x => HashRef (from usage)
}

# ── 12. Overloaded objects (edge case) ───────────────────────

# An object can overload hash/array access. When we see both
# $obj->{key} and $obj->method(), don't panic — it's valid Perl.
# The Object type should win over HashRef in this case, because
# method calls are the stronger signal.

sub overloaded_dual_use {
    my ($obj) = @_;
    $obj->method();                     # $obj => Object (from method call)
    $obj->{key};                        # $obj => Object + HashRef (dual use)
    # Completion should offer both methods AND hash keys.
}

# ── 13. Builtin return types ────────────────────────────────

sub builtins {
    my @items = (1, 2, 3);

    my $len  = scalar @items;           # $len => Numeric
    my $str  = join(",", @items);       # $str => String
    my @keys = keys %ENV;              # @keys => Array (of Strings)
    my $ref  = ref $items[0];          # $ref => String
    my $time = time();                  # $time => Numeric
    my @sorted = sort @items;          # @sorted => Array
    my @reversed = reverse @items;     # @reversed => Array
    my $popped = pop @items;           # $popped inherits element type
    my $shifted = shift @items;        # $shifted inherits element type
}

# ── 14. Parameter type inference from usage ──────────────────

sub process_records {
    my ($records, $callback) = @_;

    for my $rec (@{$records}) {         # $records => ArrayRef
        $rec->{name};                   # $rec => HashRef
        $callback->($rec);              # $callback => CodeRef
    }
}

# ── 15. Variable used before declaration pattern ─────────────
# (common in Perl — declare at top, populate later)

sub late_init {
    my $result;
    # ... lots of code ...
    $result = {};                       # $result => HashRef
    $result->{status} = "ok";           # $result => HashRef (consistent)
    return $result;
}

# ── 16. Sigil context gives partial info ─────────────────────

sub sigil_context {
    my @array;                          # @array is an array (sigil tells us)
    my %hash;                           # %hash is a hash
    my $scalar;                         # $scalar — unknown until used

    # But sigil doesn't tell us what the *elements* are
    $array[0]->{key};                   # $array[0] => HashRef
    $hash{foo}->method();              # $hash{foo} => Object
}

1;
