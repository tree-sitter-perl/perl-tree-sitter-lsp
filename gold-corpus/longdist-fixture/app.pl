use Mojolicious::Lite;

plugin 'Confy', { alpha => 1, beta => 'x' };

app->start;
