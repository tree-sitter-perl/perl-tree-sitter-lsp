package My::TestKit;

# POC fixture covering every sigil the generator needs to handle.
# Mirrors a typical production Import::Base subclass's shape but
# smaller and self-contained — no transitive `require`s (we use bare
# module names that don't need to exist on disk; the generator never
# invokes import).

use strict;
use warnings;

# Stub a parent to silence the load-time `require Import::Base`. The
# generator reads our package variables but never calls our parent's
# `import`, so a dummy is fine.
package My::TestKit::FakeBase;
sub import {}

package My::TestKit;
our @ISA = ('My::TestKit::FakeBase');

our @IMPORT_MODULES = (

    # No prefix, with args:  use Moo;
    'Moo' => [],

    # Bareword (no fat-comma):  use strict;
    'strict',

    # `>` (Import::Into runtime, static effect == use):  use feature ':5.38';
    '>feature' => [':5.38'],

    # Unimport (-): TODO
    '-feature' => [qw/switch/],

    # Unimport via Import::Into (>-): TODO
    '>-warnings' => [qw/experimental::for_list/],

    # Coderef: TODO with file:line
    sub {
        return 'Carp::Clan' => ["^Mojo"];
    },

    # With multi-element args (fat-comma in arg list):
    'Future::AsyncAwait' => [future_class => 'Mojo::Promise'],
);

our %IMPORT_BUNDLES = (

    # Simple bundle: just adds one use.
    Class => ['My::Stub::Class'],

    # `<` prefix — emit-first (base-class shape).
    Plugin => [
        '<Mojo::Base' => ['Mojolicious::Plugin'],
        'Some::Helper',
    ],

    # Multiple entries, including a coderef.
    Controller => [
        'My::Stub::Class',
        sub {
            my ($bundles, $args) = @_;
            $args->{package}->can('extends')->('Mojolicious::Controller');
        }
    ],

    # `<parent` shape — common for inheritance kits.
    Schema => [
        '<parent' => ['Co::Schema'],
        mro       => ['c3'],
    ],
);

1;
