package BrandedApp::Two;
use Mojolicious::Lite;

# A helper unique to app TWO.
helper beta_only => sub {
    my $c = shift;
    return 'beta';
};

# Completion on `app->` here must offer THIS app's helper (beta_only) but
# NOT app one's (alpha_only). See docs/adr/branded-edges.md.
get '/' => sub {
    my $c = shift;
    $c->render(text => app->beta_only);
};

app->start;
