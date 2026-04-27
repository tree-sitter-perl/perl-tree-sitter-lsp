#!/usr/bin/perl
# ============================================================================
# MINION PLUGIN DEMO — job queue task registration + enqueue sites.
#
# `minion` plugin models:
#   * $minion->add_task(NAME => sub ($job, @args) { ... })
#       → Handler for NAME (owner: Minion) + DispatchCall ref on the
#         name string; $job typed as Minion::Job inside the callback.
#   * $minion->enqueue / ->enqueue_p / ->perform_jobs / ->foreground
#       → DispatchCall ref on the first-string arg, pairing with the
#         Handler def so gd/gr on the task name jumps between sites.
#
# Open this file in nvim (./dev.sh test_files/plugin_minion_demo.pl):
#   <leader>o  outline shows each task as a Function-kind symbol.
#   K          hover on a task name shows the Handler, stacked with
#              every registration that exists for that name.
#   gd         on 'send_email' in an enqueue call jumps to add_task.
#   gr         on the task name returns registrations + enqueue sites.
#   inside the task body, `$job->` completes against Minion::Job.
# ============================================================================

package MyApp;
use strict;
use warnings;
use Minion;

my $minion = Minion->new;

# ── Task registration ───────────────────────────────────────────────
#
# `$job` lands inside the callback body typed as Minion::Job — so
# `$job->finish`, `->fail`, `->note`, `->retry` all complete.
$minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
    $job->note(started_at => time);
    # Pretend to send...
    $job->finish({ to => $to, ok => 1 });
});

$minion->add_task(resize_image => sub {
    my ($job, $path, $width, $height) = @_;
    $job->fail('no path') unless $path;
});

# ── Enqueue sites ───────────────────────────────────────────────────
#
# Cursor on 'send_email' in the enqueue call → gd jumps up to the
# add_task registration. References on the name returns both sites.
$minion->enqueue(send_email => ['alice@example.com', 'hi', 'body']);
$minion->enqueue(resize_image => ['/tmp/a.png', 800, 600] => { priority => 10 });

# Promise flavor
$minion->enqueue_p(send_email => ['bob@example.com', 'hey', 'body']);

# Foreground execution (for tests / debugging)
$minion->perform_jobs;

1;
