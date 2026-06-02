#!/usr/bin/perl
# ============================================================
# CATALYST FRAMEWORK PLUGIN DEMO
#
# Covers the catalyst.rhai bundled plugin:
#   - overrides(): $c->req/res/stash/log/config typed returns
#   - on_method_call: $c->model('Foo') → MethodCallRef into Foo
#   - on_method_call: $c->forward('/Root/index') → DispatchCall
#   - on_use: FrameworkImport for action-attribute verbs
#
# NOTE: $c typing in action methods is a manifest gap — the
# plugin cannot automatically type the 2nd parameter of every
# action method at definition time. See catalyst.rhai for
# the full explanation. Completion / type inference on $c
# methods works when $c IS typed (e.g. typed explicitly or
# once the core gains an on_sub_definition hook).
# ============================================================

# ── App class ──────────────────────────────────────────────
package MyApp;
use Catalyst qw(
    Authentication
    Session
    Session::Store::FastMmap
    Session::State::Cookie
);
MyApp->setup();

# ── Controller ─────────────────────────────────────────────
package MyApp::Controller::Root;
use parent 'Catalyst::Controller';

# Catalyst action: the 2nd positional $c is the context object
# (Catalyst instance). Completion on $c->req, $c->res, $c->model
# works once $c is typed; the plugin supplies the return types.
sub index :Path :Args(0) {
    my ($self, $c) = @_;

    # These return types are declared in catalyst.rhai overrides():
    #   $c->req    → Catalyst::Request
    #   $c->res    → Catalyst::Response
    #   $c->stash  → HashRef
    #   $c->log    → Catalyst::Log
    #   $c->config → HashRef
    my $req    = $c->req;
    my $res    = $c->res;
    my $stash  = $c->stash;
    my $log    = $c->log;
    my $cfg    = $c->config;

    # $c->model('Foo') — emits MethodCallRef into class 'Foo'.
    # gd on 'Foo' navigates to MyApp::Model::Foo (or class Foo if present).
    my $model = $c->model('Foo');

    # $c->view('TT') — same, points at MyApp::View::TT.
    my $view = $c->view('TT');

    # $c->controller('Root') — points at MyApp::Controller::Root.
    my $ctrl = $c->controller('Root');

    # $c->forward('/Root/other') — DispatchCall ref.
    # gr on 'other' action definition finds this forward site.
    $c->forward('/Root/other');

    # $c->detach and $c->visit work the same way.
    $c->detach('/Root/error', [404]);

    $c->stash->{template} = 'root/index.tt2';
    $c->res->body('Hello from Root!');
}

sub other :Local {
    my ($self, $c) = @_;
    $c->res->body('other action');
}

1;
