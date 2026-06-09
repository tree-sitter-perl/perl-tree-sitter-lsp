# hover

Generated from `fixtures/hover.json` (the source of truth) against the cpm-installed, snapshot-pinned substrate (`gold-corpus/local/lib/perl5`).
Positions are 0-based on input, 1-based on output. Run via `gold-corpus/run.pl hover`.

| id | difficulty | semantic_area | cursor | expect.all / expect.none | status | actual |
|----|------------|---------------|--------|--------------------------|--------|--------|
| hover-dt-now | simple | type-inference | `x86_64-linux/DateTime.pm:542:4` | all: sub now {; package DateTime; returns: DateTime | gold | ```perl sub now { ``` *package DateTime* *returns: DateTime* |
| hover-dt-clone | simple | type-inference | `x86_64-linux/DateTime.pm:764:4` | all: sub clone; package DateTime; returns: DateTime | gold | sub clone { bless { %{ $_[0] } }, ref $_[0] } returns DateTime |
| hover-dt-duration-class-const | simple | constants | `x86_64-linux/DateTime.pm:87:4` | all: sub duration_class () {'DateTime::Duration'}; package DateTime; returns: String | gold | sub duration_class () {'DateTime::Duration'} returns String |
| hover-dt-seconds-per-day-const | simple | constants | `x86_64-linux/DateTime.pm:85:4` | all: sub SECONDS_PER_DAY () {86400}; package DateTime; returns: Numeric | gold | sub SECONDS_PER_DAY () {86400} returns Numeric |
| hover-dt-today-chain | tricky | type-inference | `x86_64-linux/DateTime.pm:568:4` | all: sub today { shift->now(@_)->truncate( to => 'day' ) }; package DateTime; returns: DateTime | gold | sub today chain resolves DateTime |
| hover-dt-truncate-in-chain | tricky | type-inference | `x86_64-linux/DateTime.pm:568:28` | all: sub truncate {; class DateTime; returns: DateTime | provisional | sub truncate { class DateTime returns DateTime |
| hover-uri-new-abs-pod | simple | pod | `URI.pm:86:4` | all: sub new_abs; package URI; Constructs a new absolute URI object | gold | sub new_abs Constructs a new absolute URI object. |
| hover-uri-const-base | simple | constants | `URI/_punycode.pm:14:13` | all: use constant BASE => 36;; package URI::_punycode / none: returns: Numeric | provisional | use constant BASE => 36; package URI::_punycode |
| hover-uri-imported-uri-unescape | tricky | exporter | `URI/_server.pm:97:11` | all: sub uri_unescape(); Returns a string with each %XX sequence replaced; imported from `URI::Escape` | gold | sub uri_unescape() ... imported from URI::Escape |
| hover-catalyst-has-address | simple | moose | `Catalyst/Request.pm:47:4` | all: has address => (is => 'rw');; package Catalyst::Request / none: returns: | gold | has address => (is => 'rw'); package Catalyst::Request |
| hover-catalyst-has-private-path-isa | tricky | moose | `Catalyst/Action.pm:34:4` | all: has private_path => (; package Catalyst::Action; Returns absolute private path for this action / none: returns: String | provisional | has private_path => ( ... Returns absolute private path for this action. |
| hover-minion-worker-return | tricky | type-inference | `Minion.pm:145:4` | all: sub worker {; package Minion; returns: Minion::Worker | gold | sub worker { package Minion returns: Minion::Worker |
| hover-minion-has-app-mojo | simple | mojo | `Minion.pm:16:4` | all: has app => sub; package Minion; Application for job queue | gold | has app => sub {...} ... Application for job queue |
| hover-minion-var-worker | simple | type-inference | `Minion.pm:151:6` | all: my $worker = Minion::Worker->new(minion => $self);; type: Minion::Worker | gold | my $worker = Minion::Worker->new(minion => $self); type: Minion::Worker |
| hover-minion-self-shift-classic | simple | classic | `Minion.pm:94:7` | all: my ($self, $id) = @_;; type: Minion | gold | my ($self, $id) = @_; type: Minion |
| hover-minion-super-new | tricky | oo-isa | `Minion.pm:108:6` | all: my $self = shift->SUPER::new;; type: Minion | gold | my $self = shift->SUPER::new; type: Minion |
| hover-minion-emit-inherited | tricky | oo-isa | `Minion.pm:152:9` | all: sub emit(); class Minion (from Mojo::EventEmitter); Emit event. | provisional | sub emit() class Minion (from Mojo::EventEmitter) returns: HashRef ... Emit event. |

## Dropped (non-lib, absent from installed tree)

- hover-jsonpp-encode-json-imported-t — old cursor lived in t/003_types.t (a .t test file), which is not part of the installed module tree; additionally JSON::PP itself is not installed in this substrate (only Cpanel::JSON::XS and JSON::MaybeXS are present), so neither the consumer nor the def is portable.
