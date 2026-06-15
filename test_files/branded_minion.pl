use Minion;

# Two Minion instances in one file own distinct task sets. Per-variable
# instance brands keep them apart: `$a->enqueue('|')` completion offers
# only `$a`'s tasks, and `$a->enqueue('beta_task')` does not resolve to
# `$b`'s task. See docs/adr/branded-edges.md.
my $a = Minion->new;
$a->add_task(alpha_task => sub { my $job = shift; });

my $b = Minion->new;
$b->add_task(beta_task => sub { my $job = shift; });

# Completion inside the quotes here must offer alpha_task, NOT beta_task.
$a->enqueue('');
