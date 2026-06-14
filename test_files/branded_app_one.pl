package BrandedApp::One;
use Mojolicious::Lite;

# A helper unique to app ONE. App two, a different package in the same
# workspace, must NOT see it — both apps' helpers bridge to the same
# app-surface class, so without brands app two would resolve this one.
# The no-leak fixture. See docs/adr/branded-edges.md.
helper alpha_only => sub {
    my $c = shift;
    return 'alpha';
};

get '/' => sub {
    my $c = shift;
    $c->render(text => app->alpha_only);
};

app->start;
