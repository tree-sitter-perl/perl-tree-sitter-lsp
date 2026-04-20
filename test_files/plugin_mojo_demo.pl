#!/usr/bin/perl
# ============================================================================
# MOJO FRAMEWORK PLUGIN DEMO — five Rhai plugins, one file.
#
# `mojo-events`   — $emitter->on('evt', sub {}) / $x->emit('evt', ...)
# `mojo-helpers`  — $app->helper('name' => sub {}) registers methods on
#                   Mojolicious::Controller; dotted names chain into
#                   helper namespaces ($c->users->create).
# `mojo-routes`   — ->to('Controller#action') emits a cross-file MethodCall
#                   ref so gd jumps to the controller's method.
# `mojo-lite`     — get/post/etc. at top level register Handler symbols
#                   keyed by URL path, dispatched via url_for().
# `minion`        — $minion->add_task(NAME => sub { ... }) registers a
#                   task; $minion->enqueue(NAME => [args]) dispatches.
#                   `$job` inside the task body is typed Minion::Job.
#
# Open this file in nvim (`./dev.sh test_files/plugin_mojo_demo.pl`). Try:
#   `<leader>o`  → outline shows events (EVENT), helpers (Method), routes
#                  (as refs, not symbols), and Lite routes (EVENT with
#                  path as name).
#   `K`          → hover on anything — event names show stacked handlers +
#                  dispatchers; helper calls show the helper's class +
#                  sig; `'Ctrl#act'` string shows method hover.
#   `gd` / `gr`  → everywhere you'd expect, including cross-file.
#   trigger sig-help inside `->emit('connect', ^` — two registered
#   handlers stack as alternatives.
#   inside `$minion->enqueue('send_email', [^])` — the task's params
#   surface (arrayref-wrapped sig help, declared by the plugin).
#   inside `$minion->enqueue('send_email', [...], { ^ })` — hash-key
#   completion offers `priority`, `queue`, `delay`, … (plugin emits
#   HashKeyDefs for the options hash).
# ============================================================================

package MyApp;
use strict;
use warnings;
use Mojolicious::Lite;

# ── mojo-helpers: simple + dotted chains ──
#
# Cursor on 'current_user' or 'users' or 'create' in `$c->users->create()`
# → gd jumps here.
my $app = Mojolicious->new;
$app->helper(current_user => sub {
    my ($c, $fallback) = @_;
    return $c->session('user') // $fallback;
});

# Two helpers sharing the prefix `users` — one `users` namespace method
# emitted (not two), plus two leaves (`create`, `delete`).
$app->helper('users.create' => sub {
    my ($c, $name, $email) = @_;
    return { name => $name, email => $email };
});
$app->helper('users.delete' => sub {
    my ($c, $id) = @_;
    return { ok => 1, id => $id };
});

# Three-level chain.
$app->helper('admin.users.purge' => sub {
    my ($c, $force) = @_;
});

# ── mojo-routes: string target → cross-file MethodCall ref ──
#
# Cursor on 'list' in 'Users#list' → gd jumps to Users::list (even across
# files — works through the normal method-resolution + workspace index).
my $r = app->routes;
$r->get('/users')->to('Users#list');
$r->post('/users')->to(controller => 'Users', action => 'create');
$r->get('/admin')->to('Admin::Dashboard#index');

# ── mojo-lite: top-level route verbs → Handler with url_for dispatcher ──
#
# Handler name = URL path. `url_for('/users/profile')` in a template or
# helper call would offer these routes as completions (top priority).
get '/users/profile' => sub {
    my ($c) = @_;
    $c->render(template => 'profile');
};

post '/users/profile' => sub {
    my ($c, $form_data) = @_;
    # persist form_data...
    $c->redirect_to('/users/profile');
};

any '/fallback' => sub {
    my $c = shift;
    $c->render(text => 'nope', status => 404);
};

# ── minion: task queue — add_task + enqueue ──────────────────────────
#
# Cursor on `send_email` anywhere → gd jumps to the add_task site,
# gr shows every enqueue + perform site.
# Inside the task body, `$job->` completes against Minion::Job.
# Sig help inside `$minion->enqueue('send_email', [^])` shows the
# task's params ($to, $subject, $body) — $job stripped as invocant.
# Inside the options hash `{ ^ }` — completion offers Minion's
# enqueue-time keys (priority, queue, delay, attempts, ...).
use Minion;
my $minion = Minion->new;

$minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
    $job->note(started_at => time);
    $job->finish({ to => $to, ok => 1 });
});

$minion->add_task(resize_image => sub {
    my ($job, $path, $width, $height) = @_;
    $job->fail('no path') unless $path;
});

# Enqueue sites — gd on the name jumps to add_task.
$minion->enqueue(send_email  => ['alice@example.com', 'hi',  'body']);
$minion->enqueue(resize_image => ['/tmp/a.png', 800, 600], { priority => 10 });
$minion->enqueue_p(send_email => ['bob@example.com', 'hey', 'body']);

# Foreground execution (for tests/debugging).
$minion->perform_jobs;

# ── mojo-events: handlers on an EventEmitter subclass ──
#
# Every ->on registers a Handler (stacked). ->emit references them.
# Cursor on 'ready' anywhere → `gr` shows every wire-up + emit-site.
# Sig-help inside `->emit('ready', ^` shows the handler's params.
package MyApp::Progress;
use parent 'Mojo::EventEmitter';

sub new {
    my $class = shift;
    my $self = bless { step => 0 }, $class;
    $self->on('ready', sub { my ($self_in, $ctx) = @_; });
    $self->on('step', sub { my ($self_in, $n, $total) = @_; });
    $self->on('done', sub { my ($self_in, $result) = @_; });
    return $self;
}

sub tick {
    my ($self, $n) = @_;
    $self->emit('step', $n, 100);
    $self->emit('done', { status => 'ok' }) if $n >= 100;
}

1;
