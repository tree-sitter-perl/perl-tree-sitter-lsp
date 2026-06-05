# hover

Verified `hover` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| hover-dt-now | simple | type-inference | DateTime | `lib/DateTime.pm:542:4` `now` | sub now; package DateTime; returns: DateTime | `sub now {`; *package DateTime*; *returns: DateTime* (=head3 POD correctly does not attach) | PASS | gold |
| hover-dt-clone | simple | type-inference | DateTime | `lib/DateTime.pm:764:4` `clone` | sub clone; package DateTime; returns: DateTime | `sub clone { bless { %{ $_[0] } }, ref $_[0] }`; *package DateTime*; *returns: DateTime* | PASS | gold |
| hover-dt-duration-class-const | simple | constants | DateTime | `lib/DateTime.pm:87:4` `duration_class` | constant-sub; package DateTime; returns: String ('DateTime::Duration') | `sub duration_class () {'DateTime::Duration'}`; *package DateTime*; *returns: String* | PASS | gold |
| hover-dt-seconds-per-day-const | simple | constants | DateTime | `lib/DateTime.pm:85:4` `SECONDS_PER_DAY` | constant-sub; package DateTime; returns: Numeric (86400) | `sub SECONDS_PER_DAY () {86400}`; *package DateTime*; *returns: Numeric* | PASS | gold |
| hover-dt-today-chain | tricky | type-inference | DateTime | `lib/DateTime.pm:568:4` `today` | sub today; returns: DateTime | `sub today { shift->now(@_)->truncate( to => 'day' ) }`; *package DateTime*; *returns: DateTime* (two-hop chain) | PASS | gold |
| hover-dt-truncate-in-chain | tricky | type-inference | DateTime | `lib/DateTime.pm:568:28` `truncate` | ->truncate; receiver resolved to DateTime; returns: DateTime | `sub truncate {`; *class DateTime*; *returns: DateTime* | PASS | provisional |
| hover-uri-new-abs-pod | simple | pod | URI | `lib/URI.pm:86:4` `new_abs` | sub new_abs; package URI; =item POD body | `sub new_abs`; *package URI*; "Constructs a new absolute URI object..." (=item POD attaches) | PASS | gold |
| hover-uri-const-base | simple | constants | URI | `lib/URI/_punycode.pm:14:13` `BASE` | use constant BASE => 36; package URI::_punycode; (returns Numeric ideal) | `use constant BASE => 36;`; *package URI::_punycode* (no *returns: Numeric*) | REVIEW | provisional |
| hover-uri-imported-uri-unescape | tricky | exporter | URI | `lib/URI/_server.pm:97:11` `uri_unescape` | imported sub; POD from URI::Escape =item; 'imported from URI::Escape' | `sub uri_unescape()`; full POD; *imported from `URI::Escape`* | PASS | gold |
| hover-catalyst-has-address | simple | moose | Catalyst-Runtime | `lib/Catalyst/Request.pm:47:4` `address` | Moose has accessor (is => 'rw'); no isa so no return type | `has address => (is => 'rw');`; *package Catalyst::Request* | PASS | gold |
| hover-catalyst-has-private-path-isa | tricky | moose | Catalyst-Runtime | `lib/Catalyst/Action.pm:34:4` `private_path` | Moose has accessor isa => 'Str'; returns: String; POD attached | `has private_path => (`; *package Catalyst::Action*; POD attaches but NO *returns: String* | REVIEW | provisional |
| hover-minion-worker-return | tricky | type-inference | Minion | `lib/Minion.pm:145:4` `worker` | sub worker; returns: Minion::Worker | `sub worker {`; *package Minion*; *returns: Minion::Worker* (+ 'worker' event =head2 POD) | PASS | gold |
| hover-minion-has-app-mojo | simple | mojo | Minion | `lib/Minion.pm:16:4` `app` | Mojo::Base 'has app => sub {...}'; POD 'Application for job queue...' | `has app => sub { ... }, weak => 1;`; *package Minion*; usage example + =head2 app POD | PASS | gold |
| hover-minion-var-worker | simple | type-inference | Minion | `lib/Minion.pm:151:6` `$worker` | variable $worker; type: Minion::Worker | `my $worker = Minion::Worker->new(minion => $self);`; *type: Minion::Worker* | PASS | gold |
| hover-minion-self-shift-classic | simple | classic | Minion | `lib/Minion.pm:94:7` `$self` | variable $self; type: Minion (first param) | `my ($self, $id) = @_;`; *type: Minion* | PASS | gold |
| hover-minion-emit-inherited | tricky | oo-isa | Minion | `lib/Minion.pm:152:9` `emit` | $self->emit inherited via @ISA (Mojo::EventEmitter); emit returns $self (Minion); POD 'Emit event.' | `sub emit()`; *class Minion (from Mojo::EventEmitter)*; *returns: HashRef* (WRONG — should be invocant); POD + ancestry correct | REVIEW | provisional |
| hover-jsonpp-encode-json-imported-t | tricky | exporter | JSON-PP | `t/003_types.t:35:19` `encode_json` | imported encode_json (@EXPORT default); signature + =head2 POD | No hover info at 35:19 (empty, exit 1) | FAIL | gold |

## Known-failing detail

- **hover-jsonpp-encode-json-imported-t** — imported-function hover works in a `.pm` consumer but returns nothing in a `.t` file. JSON::PP @EXPORT default import of encode_json not resolved for hover in `.t` consumers (the def itself, PP.pm 109:4, hovers fine).

## REVIEW-class gaps (return-type omissions, in main table)

- **hover-uri-const-base / hover-catalyst-has-private-path-isa** — headline + POD correct but Moose `isa`/use-constant value does not surface as `*returns: ...*`.
- **hover-minion-emit-inherited** — inheritance + cross-file POD correct, but `*returns: HashRef*` mis-inferred (emit does `return $self`).
