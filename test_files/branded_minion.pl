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

# Accessor-chain instances: tasks scoped by the ACCESSOR name, not the
# base. `$app->minion` and `$app->other_minion` are distinct instances.
$app->minion->add_task(acc_minion_task => sub { my $job = shift; });
$app->other_minion->add_task(acc_other_task => sub { my $job = shift; });

# Completion here must offer acc_minion_task, NOT acc_other_task.
$app->minion->enqueue('');
