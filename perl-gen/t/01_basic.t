#!/usr/bin/env perl
use v5.36;
use utf8;
use warnings;
use open ':std', ':encoding(UTF-8)';
use experimental 'signatures';

use Test::More;
use FindBin ();
use lib "$FindBin::RealBin/../lib";
use lib "$FindBin::RealBin/fixtures/lib";

use App::PerlLSP::PluginGen::ImportBase ();

my $rhai = App::PerlLSP::PluginGen::ImportBase::generate_plugin(
    'My::TestKit',
    lib_paths => ["$FindBin::RealBin/fixtures/lib"],
);

# --- Plugin metadata -------------------------------------------------

like $rhai, qr/fn id\(\) \{ "my-testkit-generated" \}/,
    'plugin id derived from kit class';
like $rhai, qr/fn triggers\(\) \{ \[ \#\{ Always: \(\) \} \] \}/,
    'unconditional trigger (on_use checks module_name itself)';
like $rhai, qr/if ctx\.module_name != "My::TestKit" \{ return \[\]; \}/,
    'module-name gate at top of on_use';

# --- @IMPORT_MODULES (no-prefix, bareword, `>`, coderef, multi-arg) --

like $rhai,
    qr/out \+= \#\{ "SyntheticUse": \#\{ "module": "Moo", "args": \[\], "imports": \[\], "span": ctx\.span \}\};/,
    'no-prefix entry → empty args + imports';
like $rhai,
    qr/out \+= \#\{ "SyntheticUse": \#\{ "module": "strict", "args": \[\], "imports": \[\], "span": ctx\.span \}\};/,
    'bareword (no fat-comma) → no args';
like $rhai,
    qr/"module": "feature", "args": \[":5.38"\], "imports": \[":5.38"\]/,
    '`>` prefix → use-into; args + imports both filled with same list';
like $rhai,
    qr/"module": "Future::AsyncAwait", "args": \["future_class", "Mojo::Promise"\], "imports": \["future_class", "Mojo::Promise"\]/,
    'multi-element args list preserved';

# --- Unimport sigils → TODO ------------------------------------------

like $rhai,
    qr|// TODO: unimport feature \[switch\] \(via use\)|,
    '`-` prefix → unimport TODO (via use)';
like $rhai,
    qr|// TODO: unimport warnings \[experimental::for_list\] \(via import_into\)|,
    '`>-` prefix → unimport TODO (via import_into)';

# --- Coderef probing -------------------------------------------------
# The @IMPORT_MODULES coderef returns `'Carp::Clan' => ["^Mojo"]`;
# probing runs it and turns the return value into a SyntheticUse.

like $rhai,
    qr/"module": "Carp::Clan", "args": \[\], "imports": \[\]/,
    'coderef return value → bare `use` SyntheticUse (runtime args dropped)';

# The -Controller bundle coderef calls
# `$args->{package}->can('extends')->('Mojolicious::Controller')`;
# the probe records it and emits a PackageParent on the consumer.

like $rhai,
    qr/"PackageParent": \#\{ "package": ctx\.current_package, "parent": "Mojolicious::Controller" \}/,
    'coderef `extends` side effect → PackageParent (probe recorded the call)';

# --- Bundle dispatch -------------------------------------------------

like $rhai, qr/if bundle == "Class" \{/, '-Class bundle branch present';
like $rhai, qr/if bundle == "Controller" \{/, '-Controller bundle branch present';
like $rhai, qr/if bundle == "Plugin" \{/,  '-Plugin bundle branch present';
like $rhai, qr/if bundle == "Schema" \{/,  '-Schema bundle branch present';

# --- `<` prefix emits early within its bundle ------------------------
# In -Plugin: `<Mojo::Base => ['Mojolicious::Plugin']` is listed
# AFTER `'Some::Helper'` in the source but must emit FIRST so the
# base-class is set up before non-`<` entries.

my ($plugin_block) = $rhai =~ /if bundle == "Plugin" \{(.+?)\n    \}/s;
ok defined $plugin_block, 'extracted -Plugin block';
my $mojo_pos = index $plugin_block, 'Mojo::Base';
my $helper_pos = index $plugin_block, 'Some::Helper';
ok $mojo_pos != -1 && $helper_pos != -1, 'both entries present in -Plugin block';
ok $mojo_pos < $helper_pos,
    '`<`-prefixed Mojo::Base emits before Some::Helper despite source order';

# --- Real path's both-fields-filled discipline -----------------------
# `use parent 'Co::Schema'` in real source ends up with
# args=["Co::Schema"] AND imports=["Co::Schema"].
# Generator must mirror that for the synthetic version so the LSP's
# dedup key (which includes both lists) discriminates correctly.

like $rhai,
    qr/"module": "parent", "args": \["Co::Schema"\], "imports": \["Co::Schema"\]/,
    'parent-shape entry fills both args and imports (matches real-source dedup behavior)';

done_testing;
