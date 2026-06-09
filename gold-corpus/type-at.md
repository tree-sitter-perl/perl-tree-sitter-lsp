# type-at

Generated from `fixtures/type-at.json` (the source of truth) against the cpm-installed, snapshot-pinned substrate (`gold-corpus/local/lib/perl5`).
Positions are 0-based on input, 1-based on output. Run via `gold-corpus/run.pl type-at`.

| id | difficulty | semantic_area | cursor | expect.all / expect.none | status | actual |
|----|------------|---------------|--------|--------------------------|--------|--------|
| ti-04 | simple | classic | `Moo/Object.pm:12:6` | all: Moo::Object | gold | Moo::Object |
| ti-05 | tricky | oo-isa | `URI.pm:102:9` | all: URI | gold | URI |
| ti-06 | simple | classic | `URI.pm:55:8` | all: URI | gold | URI |
| ti-07 | simple | oo-isa | `x86_64-linux/DateTime.pm:205:8` | all: DateTime | gold | DateTime |
| ti-08 | simple | classic | `x86_64-linux/DateTime.pm:543:8` | all: DateTime | gold | DateTime |
| ti-09 | tricky | oo-isa | `Type/Tiny.pm:397:5` | all: Type::Tiny | gold | Type::Tiny |
| ti-10 | tricky | a4 | `Type/Tiny.pm:400:6` | all: Numeric | gold | Numeric |
| ti-11 | simple | mojo | `Minion.pm:26:9` | all: Minion | gold | Minion |
| ti-12 | tricky | oo-isa | `Minion.pm:108:6` | all: Minion | xfail | type-at CLI is single-file (no module index); SUPER::new can't reach Mojo::Base cross-file. LSP resolves it — see hover-minion-super-new (gold). |
| ti-13 | simple | type-inference | `Minion.pm:110:6` | all: String | gold | String |
| ti-14 | tricky | type-inference | `Minion.pm:151:5` | all: Minion::Worker | gold | Minion::Worker |
| ti-15 | simple | moose | `Dist/Zilla.pm:90:7` | all: Dist::Zilla | gold | Dist::Zilla |
| ti-17 | tricky | mojo | `Minion.pm:48:5` | none: Mojo::Collection; ARRAY; ArrayRef | provisional |  |
| ti-18 | tricky | exporter | `Minion.pm:111:5` | none: Mojo::Loader; String; Str | provisional |  |

## Dropped (non-lib, absent from installed tree)

- ti-01: JSON::PP not installed in the cpm substrate (only JSON::MaybeXS + Mojo::JSON present); old file JSON-PP/lib/JSON/PP.pm has no installed counterpart
- ti-02: same — JSON::PP absent from local/lib/perl5
- ti-03: same — JSON::PP absent from local/lib/perl5
- ti-16: already rejected in source md — cursor lands inside a POD example block (no live code); not portable
- ti-19: JSON::PP absent (use constant P_ASCII lived in JSON/PP.pm); also a constant, not a variable type-at target
