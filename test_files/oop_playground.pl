#!/usr/bin/perl
use strict;
use warnings;

# ============================================================================
# OOP Playground — QA file for framework accessor synthesis
# Covers: Moo, Moose, Mojo::Base, DBIC, block packages, inheritance chains,
#         method call return types, chaining, cross-sigil, the works.
# ============================================================================

# ---- Moo: read-only, read-write, lazy, rwp, bare ----

package Moo::Person;
use Moo;

has 'name'    => (is => 'ro', isa => 'Str');
has 'age'     => (is => 'rw', isa => 'Int');
has 'email'   => (is => 'lazy', isa => 'Str');
has 'ssn'     => (is => 'bare');              # no accessor
has 'status'  => (is => 'rwp');               # getter + _set_status
has 'address' => (is => 'ro', isa => 'HashRef');

sub _build_email {
    my ($self) = @_;
    return $self->name . '@example.com';
}

sub greet {
    my ($self) = @_;
    return "Hi, I'm " . $self->name;
}

# ---- Moo: multi-attribute has ----

package Moo::Config;
use Moo;

has [qw(host port)] => (is => 'ro', isa => 'Str');
has 'timeout' => (is => 'ro', isa => 'Num');

sub dsn {
    my ($self) = @_;
    return $self->host . ':' . $self->port;
}

# ---- Moo: InstanceOf isa ----

package Moo::Service;
use Moo;

has 'config' => (is => 'ro', isa => "InstanceOf['Moo::Config']");
has 'logger' => (is => 'ro', isa => "InstanceOf['Log::Any']");
has 'data'   => (is => 'ro', isa => 'ArrayRef');
has 'cache'  => (is => 'ro', isa => 'HashRef');
has 'handler'=> (is => 'ro', isa => 'CodeRef');

sub run {
    my ($self) = @_;
    # $self->config should complete as Moo::Config
    my $cfg = $self->config;
    my $dsn = $cfg->dsn;
    # $self->data should be ArrayRef
    my $items = $self->data;
    # $self->cache should be HashRef
    my $c = $self->cache;
    return $dsn;
}

# ---- Moo: inheritance ----

package Moo::Employee;
use Moo;
use parent -norequire, 'Moo::Person';

has 'company'   => (is => 'ro', isa => 'Str');
has 'salary'    => (is => 'rw', isa => 'Num');
has 'badge_num' => (is => 'ro', isa => 'Int');

sub introduce {
    my ($self) = @_;
    # Should see inherited: name, age, email, status, address
    # Plus own: company, salary, badge_num
    return $self->name . " at " . $self->company;
}

# ---- Moose: class names as types ----

package Moose::Animal;
use Moose;

has 'name'   => (is => 'ro', isa => 'Str');
has 'sound'  => (is => 'ro', isa => 'Str');
has 'legs'   => (is => 'ro', isa => 'Int');
has 'keeper' => (is => 'ro', isa => 'Moose::Keeper');

sub describe {
    my ($self) = @_;
    return $self->name . ' says ' . $self->sound;
}

package Moose::Keeper;
use Moose;

has 'name'    => (is => 'ro', isa => 'Str');
has 'animals' => (is => 'ro', isa => 'ArrayRef');

sub head_count {
    my ($self) = @_;
    my $list = $self->animals;
    return scalar @$list;
}

# ---- Moose: inheritance + override ----

package Moose::Dog;
use Moose;
extends 'Moose::Animal';

has 'breed' => (is => 'ro', isa => 'Str');

sub fetch {
    my ($self) = @_;
    return $self->name . ' fetches the ball';
}

# ---- Mojo::Base: fluent chaining ----

package Mojo::Widget;
use Mojo::Base -base;

has 'color';
has 'width';
has 'height';
has 'label';

sub area {
    my ($self) = @_;
    return $self->width * $self->height;
}

sub describe {
    my ($self) = @_;
    return $self->color . ' ' . $self->label;
}

# ---- Mojo::Base: with parent ----

package Mojo::Button;
use Mojo::Base 'Mojo::Widget';

has 'on_click';
has 'disabled';

sub render {
    my ($self) = @_;
    # Should see inherited: color, width, height, label
    # Plus own: on_click, disabled
    return '<button>' . $self->label . '</button>';
}

# ---- Mojo::Base: -strict (NO accessors) ----

package Mojo::Util;
use Mojo::Base -strict;

has 'should_not_appear';

sub helper {
    return 42;
}

# ---- Mojo::Base: block package ----

package Mojo::Card {
    use Mojo::Base -base;

    has 'suit';
    has 'rank';
    has 'face_up';

    sub display {
        my ($self) = @_;
        return $self->rank . ' of ' . $self->suit;
    }
}

# ---- DBIC: columns + relationships ----

package Schema::Result::User;
use base 'DBIx::Class::Core';

__PACKAGE__->add_columns(
    id         => { data_type => 'integer', is_auto_increment => 1 },
    username   => { data_type => 'varchar', size => 255 },
    email      => { data_type => 'varchar', size => 255 },
    created_at => { data_type => 'datetime' },
);

__PACKAGE__->has_many(posts    => 'Schema::Result::Post', 'author_id');
__PACKAGE__->has_one(profile   => 'Schema::Result::Profile', 'user_id');
__PACKAGE__->might_have(avatar => 'Schema::Result::Avatar', 'user_id');

sub full_info {
    my ($self) = @_;
    # Column accessors: id, username, email, created_at
    my $name = $self->username;
    my $mail = $self->email;
    # Relationship: posts returns ResultSet, profile returns row object
    my $posts   = $self->posts;
    my $profile = $self->profile;
    return "$name <$mail>";
}

package Schema::Result::Post;
use base 'DBIx::Class::Core';

__PACKAGE__->add_columns(
    id        => { data_type => 'integer', is_auto_increment => 1 },
    title     => { data_type => 'varchar', size => 255 },
    body      => { data_type => 'text' },
    author_id => { data_type => 'integer' },
);

__PACKAGE__->belongs_to(author => 'Schema::Result::User', 'author_id');
__PACKAGE__->has_many(comments => 'Schema::Result::Comment', 'post_id');

sub summary {
    my ($self) = @_;
    # belongs_to: author returns Schema::Result::User object
    my $author = $self->author;
    my $author_name = $author->username;
    return $self->title . ' by ' . $author_name;
}

package Schema::Result::Comment;
use base 'DBIx::Class::Core';

__PACKAGE__->add_columns(qw(id body post_id user_id created_at));

__PACKAGE__->belongs_to(post   => 'Schema::Result::Post', 'post_id');
__PACKAGE__->belongs_to(author => 'Schema::Result::User', 'user_id');

# ---- DBIC: block package ----

package Schema::Result::Profile {
    use base 'DBIx::Class::Core';

    __PACKAGE__->add_columns(
        user_id => { data_type => 'integer' },
        bio     => { data_type => 'text' },
        website => { data_type => 'varchar' },
    );

    __PACKAGE__->belongs_to(user => 'Schema::Result::User', 'user_id');

    sub display {
        my ($self) = @_;
        return $self->bio . ' — ' . $self->website;
    }
}

# ---- Plain OOP (no framework): constructor + manual accessors ----

package Plain::Stack;
sub new {
    my ($class) = @_;
    return bless { items => [], size => 0 }, $class;
}

sub push_item {
    my ($self, $item) = @_;
    push @{$self->{items}}, $item;
    $self->{size}++;
    return $self;
}

sub pop_item {
    my ($self) = @_;
    $self->{size}--;
    return pop @{$self->{items}};
}

sub peek {
    my ($self) = @_;
    return $self->{items}[-1];
}

# ---- Method call return type chains ----

package Chain::Factory;
sub new { bless {}, shift }

sub create_config {
    my ($self) = @_;
    return { host => 'localhost', port => 5432, ssl => 1 };
}

sub create_worker {
    my ($self) = @_;
    return Chain::Worker->new();
}

package Chain::Worker;
sub new { bless {}, shift }

sub process {
    my ($self, $data) = @_;
    return { status => 'ok', count => 42, errors => [] };
}

sub get_stats {
    my ($self) = @_;
    return { uptime => 9999, jobs => 100 };
}

# ============================================================================
# package main — exercise everything
# ============================================================================

package main;

# ---- Moo objects ----
my $person = Moo::Person->new(name => 'Alice', age => 30);
my $name = $person->name;           # Str
my $age  = $person->age;            # Int → Numeric
$person->age(31);                   # setter
$person->_set_status('active');     # rwp writer
my $addr = $person->address;        # HashRef

my $emp = Moo::Employee->new(name => 'Bob', company => 'Acme');
my $intro = $emp->introduce;
my $emp_name = $emp->name;          # inherited from Person

my $svc = Moo::Service->new();
my $cfg = $svc->config;             # InstanceOf → Moo::Config
my $dsn = $cfg->dsn;                # method on Moo::Config
my $svc_data = $svc->data;          # ArrayRef
my $svc_cache = $svc->cache;        # HashRef

# ---- Moose objects ----
my $animal = Moose::Animal->new(name => 'Rex', sound => 'Woof', legs => 4);
my $desc = $animal->describe;
my $keeper = $animal->keeper;       # Moose::Keeper
my $count = $keeper->head_count;

my $dog = Moose::Dog->new(name => 'Buddy', breed => 'Lab');
my $breed = $dog->breed;
my $fetch_result = $dog->fetch;
my $dog_name = $dog->name;          # inherited from Moose::Animal

# ---- Mojo::Base: fluent chains ----
my $widget = Mojo::Widget->new();
$widget->color('red')->width(100)->height(50)->label('OK');
my $area = $widget->area;

my $btn = Mojo::Button->new();
$btn->label('Submit')->on_click(sub { print "clicked\n" })->disabled(0);
my $html = $btn->render;
my $btn_color = $btn->color;        # inherited from Widget

# ---- Mojo::Base: block package ----
my $card = Mojo::Card->new();
$card->suit('Spades')->rank('Ace');
my $card_display = $card->display;

# ---- Mojo::Base -strict: no accessors ----
my $util = Mojo::Util->new();
# $util->should_not_appear;  # this should NOT complete

# ---- DBIC ----
my $user = Schema::Result::User->new();
my $uname = $user->username;
my $posts = $user->posts;           # ResultSet
my $profile = $user->profile;       # Schema::Result::Profile
my $avatar  = $user->avatar;        # Schema::Result::Avatar

my $post = Schema::Result::Post->new();
my $author = $post->author;         # Schema::Result::User
my $author_email = $author->email;  # chain: Post → User → email column
my $comments = $post->comments;     # ResultSet

# DBIC: qw-style columns
my $comment = Schema::Result::Comment->new();
my $comment_body = $comment->body;
my $comment_post = $comment->post;  # Schema::Result::Post

# DBIC: block package
my $prof = Schema::Result::Profile->new();
my $bio = $prof->bio;
my $prof_user = $prof->user;        # Schema::Result::User

# ---- Plain OOP ----
my $stack = Plain::Stack->new();
$stack->push_item('a')->push_item('b');
my $top = $stack->peek;
$stack->{items};                    # hash key completion
$stack->{size};

# ---- Method call return type chains ----
my $factory = Chain::Factory->new();
my $worker_cfg = $factory->create_config;  # HashRef
$worker_cfg->{host};                       # hash key completion
$worker_cfg->{port};
$worker_cfg->{ssl};

my $worker = $factory->create_worker;      # Chain::Worker
my $result = $worker->process('input');    # HashRef
$result->{status};                         # hash key completion
$result->{count};

my $stats = $worker->get_stats;            # HashRef
$stats->{uptime};
$stats->{jobs};

# ---- Mixed: framework object passed to plain code ----
sub process_person {
    my ($p) = @_;
    # No type info here (param not typed) — that's expected
    return $p->name;
}
process_person($person);

# ---- Edge cases ----
# Ternary — no method call binding (expected limitation)
my $thing = rand() > 0.5 ? $person : $emp;

# Chained in single expression — no binding (expected limitation)
# my $direct = Chain::Factory->new()->create_worker()->process('x');

# Two-step chain works:
my $f2 = Chain::Factory->new();
my $w2 = $f2->create_worker;
my $r2 = $w2->process('x');
$r2->{status};

1;
