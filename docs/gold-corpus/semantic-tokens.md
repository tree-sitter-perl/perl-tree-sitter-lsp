# semantic-tokens

Verified `semantic-tokens` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| st-moo-pkgname-namespace | simple | classic | Moo | `lib/Moo/Object.pm:1:8` `Moo::Object` | namespace at 1:8 (package name) | 1:8 len=11 namespace | PASS | gold |
| st-moo-class-keyword | simple | oo-isa | Moo | `lib/Moo/Object.pm:13:5` `$class` | keyword at 13:5 ($class invocant → keyword) | 13:5 len=6 keyword | PASS | gold |
| st-moo-self-keyword | simple | oo-isa | Moo | `lib/Moo/Object.pm:47:5` `$self` | keyword at 47:5 ($self → keyword) | 47:5 len=5 keyword | PASS | gold |
| st-moo-methodcall-method | simple | oo-isa | Moo | `lib/Moo/Object.pm:22:22` `BUILDARGS` | method at 22:22 ($class->BUILDARGS method call) | 22:22 len=9 method | PASS | gold |
| st-moo-fqclass-and-new | tricky | fq | Moo | `lib/Moo/Object.pm:18:8` `Method::Generate::DemolishAll` | namespace at 18:8 (FQ class before ->new); new at 18:39 → method | 18:8 len=29 namespace; 18:39 len=3 method | PASS | gold |
| st-moo-carp-function | tricky | classic | Moo | `lib/Moo/Object.pm:37:14` `croak` | function at 37:14 (Carp::croak qualified function call) | 37:14 len=5 function | PASS | gold |
| st-moo-quoted-hashkey-property | tricky | classic | Moo | `lib/Moo/Object.pm:64:17` `'Moose/Role.pm'` | property at 64:17 ($INC{'Moose/Role.pm'} hash key, quotes included) | 64:17 len=15 property | PASS | gold |
| st-moo-has-macro | simple | moo | Moo | `t/accessor-default.t:15:2` `has` | macro at 15:2 (Moo 'has' DSL keyword) | 15:2 len=3 macro | PASS | gold |
| st-moo-extends-with-macro | tricky | moo | Moo | `t/has-plus.t:48:2` `extends` | macro at 48:2 (extends DSL); also 'with' at 29:2 → macro | 48:2 len=7 macro; 29:2 len=4 macro | PASS | gold |
| st-tt-hashkey-property | simple | type-inference | Type::Tiny | `lib/Type/Tiny.pm:97:15` `PERL_TYPE_TINY_XS` | property at 97:15 ($ENV{PERL_TYPE_TINY_XS} bareword hash key) | 97:15 len=17 property | PASS | gold |
| st-tt-foreach-loopvar-parameter | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:694:8` `$comparator` | parameter at 694:8 (`for my $comparator` loop var; ForVar deliberately grouped with Param) | 694:8 len=11 parameter | REVIEW | provisional |
| st-uri-constant-name-enummember | tricky | constants | URI | `lib/URI/_punycode.pm:15:13` `BASE` | enumMember at 15:13 (use constant BASE => 36 name) | 15:13 len=4 function | FAIL | gold |
| st-tt-regex-literal-regexp | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:1510:22` `qr/...\z/` | regexp at 1510:22 (qr// regex literal) | no token emitted at 1510:22 | FAIL | gold |

## Known-failing detail

- **st-uri-constant-name-enummember** — `use constant BASE => 36;` name tagged `function`, not `enumMember`; refs (L48:22) also `function`. `ENUM_MEMBER` is registered in the legend (symbols.rs:1881) but `TOK_ENUM_MEMBER` is `#[allow(dead_code)]` (file_analysis.rs:1728) — never emitted.
- **st-tt-regex-literal-regexp** — `qr//` regex literal gets no semantic token; `REGEXP` is in the legend (symbols.rs:1880) but `TOK_REGEXP` is `#[allow(dead_code)]` (file_analysis.rs:1727) — never emitted anywhere.

## Provisional detail

- **st-tt-foreach-loopvar-parameter** — `for my $comparator` loop var tagged `parameter` by design (the producer explicitly maps `DeclKind::ForVar | DeclKind::Param` → `TOK_PARAMETER`, file_analysis.rs:6165). Whether a loop var should color as `parameter` vs `variable` is a design judgment call, hence provisional.

## Rejected (excluded from corpus)

None.
