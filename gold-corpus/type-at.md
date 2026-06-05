# type-at (variable type inference)

Verified `type-at` rows (`--type-at`). Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| ti-01 | simple | oo-isa | JSON-PP | `JSON-PP/lib/JSON/PP.pm:142:10` `$self` | JSON::PP | JSON::PP (bless $self,$class) | PASS | gold |
| ti-02 | tricky | a4 | JSON-PP | `JSON-PP/lib/JSON/PP.pm:134:7` `$self` | HashRef | HashRef (pre-bless anon-hash; temporal flips to JSON::PP after) | PASS | gold |
| ti-03 | simple | oo-isa | JSON-PP | `JSON-PP/lib/JSON/PP.pm:167:9` `$self` | JSON::PP | JSON::PP (first param my ($self,$v)=@_) | PASS | gold |
| ti-04 | simple | classic | Moo | `Moo/lib/Moo/Object.pm:12:6` `$class` | Moo::Object | Moo::Object (my $class = shift) | PASS | gold |
| ti-05 | tricky | oo-isa | URI | `URI/lib/URI.pm:102:7` `$self` | URI | URI (bless \$str into $class) | PASS | gold |
| ti-06 | simple | classic | URI | `URI/lib/URI.pm:55:8` `$class` | URI | URI (list-unpack first param) | PASS | gold |
| ti-07 | simple | oo-isa | DateTime | `DateTime/lib/DateTime.pm:205:8` `$self` | DateTime | DateTime (bless {}, $class) | PASS | gold |
| ti-08 | simple | classic | DateTime | `DateTime/lib/DateTime.pm:543:8` `$class` | DateTime | DateTime (my $class = shift) | PASS | gold |
| ti-09 | tricky | oo-isa | Type::Tiny | `Type::Tiny/lib/Type/Tiny.pm:397:4` `$self` | Type::Tiny | Type::Tiny (bless \%params, $class) | PASS | gold |
| ti-10 | tricky | a4 | Type::Tiny | `Type::Tiny/lib/Type/Tiny.pm:400:5` `$uniq` | Numeric | Numeric (hash-slot read traced from $params{uniq}=$uniq++) | PASS | gold |
| ti-11 | simple | mojo | Minion | `Minion/lib/Minion.pm:26:9` `$self` | Minion | Minion (Mojo::Base first param) | PASS | gold |
| ti-12 | tricky | oo-isa | Minion | `Minion/lib/Minion.pm:108:6` `$self` | Minion | Minion (shift->SUPER::new -> invocant class) | PASS | gold |
| ti-13 | simple | type-inference | Minion | `Minion/lib/Minion.pm:110:6` `$class` | String | String ('Minion::Backend::' . shift concat) | PASS | gold |
| ti-14 | tricky | type-inference | Minion | `Minion/lib/Minion.pm:151:5` `$worker` | Minion::Worker | Minion::Worker (Class->new) | PASS | gold |
| ti-15 | simple | moose | Dist-Zilla | `Dist-Zilla/lib/Dist/Zilla.pm:90:7` `$self` | Dist::Zilla | Dist::Zilla (Moose my ($self)=@_) | PASS | gold |
| ti-17 | tricky | mojo | Minion | `Minion/lib/Minion.pm:48:5` `$dispatched` | (unknown — accessor return untyped) | (none) | REVIEW | provisional |
| ti-18 | tricky | exporter | Minion | `Minion/lib/Minion.pm:111:5` `$e` | (unknown — imported sub, untyped return) | (none) | REVIEW | provisional |
| ti-19 | simple | constants | JSON-PP | `JSON-PP/lib/JSON/PP.pm:24:13` `P_ASCII` | use constant P_ASCII => 0 (def shown) | `use constant P_ASCII => 0;` *package JSON::PP* (no Int type label; --type-at returns nothing) | REVIEW | provisional |

## REVIEW-class detail

- **ti-17** — chain breaks at `backend` (untyped Mojo::Base accessor), so `dispatch_schedules`' receiver is unknown. Documents the accessor-return-typing gap.
- **ti-18** — `load_class` imported from Mojo::Loader has no return-type contract to project.
- **ti-19** — `--type-at` on a use-constant name returns nothing (constants aren't variables); `--hover` shows the def + package but no Int value type.

## Rejected (excluded from corpus)

- **ti-16** (`DateTime/lib/DateTime.pm:2524:8` `$local_time_zone`, expected DateTime::TimeZone) — REJECT. The cursor lands inside a POD block (`=head2 ...` at 2518, no `=cut` until 4636). The `my $local_time_zone = DateTime::TimeZone->new(...)` is a documentation example, not live code; the tool correctly returns nothing for a token inside POD. The collector mistook a POD example for real code. Recommend dropping or replacing with a real (non-POD) `Class->new` site.
