# semantic-tokens

Generated from `fixtures/semantic-tokens.json` (the source of truth) against the cpm-installed, snapshot-pinned substrate (`gold-corpus/local/lib/perl5`).
Positions are 0-based on input, 1-based on output. Run via `gold-corpus/run.pl semantic-tokens`.

| id | difficulty | semantic_area | cursor | expect.all / expect.none | status | actual |
|----|------------|---------------|--------|--------------------------|--------|--------|
| st-moo-pkgname-namespace | simple | classic | `Moo/Object.pm:0:8` | all: 1:8 len=11 namespace | gold | 1:8 len=11 namespace |
| st-moo-class-keyword | simple | oo-isa | `Moo/Object.pm:12:5` | all: 13:5 len=6 keyword | gold | 13:5 len=6 keyword |
| st-moo-self-keyword | simple | oo-isa | `Moo/Object.pm:46:5` | all: 47:5 len=5 keyword | gold | 47:5 len=5 keyword |
| st-moo-methodcall-method | simple | oo-isa | `Moo/Object.pm:21:22` | all: 22:22 len=9 method | gold | 22:22 len=9 method |
| st-moo-fqclass-and-new | tricky | fq | `Moo/Object.pm:17:8` | all: 18:8 len=29 namespace; 18:39 len=3 method | gold | 18:8 len=29 namespace; 18:39 len=3 method |
| st-moo-carp-function | tricky | classic | `Moo/Object.pm:36:14` | all: 37:14 len=5 function | gold | 37:14 len=5 function |
| st-moo-quoted-hashkey-property | tricky | classic | `Moo/Object.pm:63:17` | all: 64:17 len=15 property | gold | 64:12 len=4 variable; 64:17 len=15 property; 64:37 len=4 variable; 64:42 len=14 property |
| st-tt-hashkey-property | simple | type-inference | `Type/Tiny.pm:96:15` | all: 97:15 len=17 property | gold | 97:10 len=4 variable; 97:15 len=17 property; 97:40 len=4 variable; 97:45 len=17 property |
| st-tt-foreach-loopvar-parameter | tricky | type-inference | `Type/Tiny.pm:693:8` | all: 694:8 len=11 parameter | provisional | 694:8 len=11 parameter; 694:22 len=4 variable |
| st-uri-constant-name-enummember | tricky | constants | `URI/_punycode.pm:14:13` | all: 15:13 len=4 enumMember / none: 15:13 len=4 function | gold | 15:4 len=8 namespace; 15:13 len=4 enumMember |
| st-tt-regex-literal-no-regexp-token | tricky | type-inference | `Type/Tiny.pm:1509:22` | all: 1510:3 len=16 variable; none: regexp | gold | 1510:3 len=16 variable |

## Dropped (non-lib, absent from installed tree)

- st-moo-has-macro: old cursor t/accessor-default.t:15 is an author test file, not under lib/ -> absent from installed tree
- st-moo-extends-with-macro: old cursor t/has-plus.t:48 is an author test file, not under lib/ -> absent from installed tree
