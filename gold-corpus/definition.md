# definition (goto-definition)

Verified `definition` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| def-01-crossfile-import-call | tricky | exporter | Type::Tiny | `lib/Type/Registry.pm:74:12` `mkopt` | Exporter/Tiny.pm:430 (sub mkopt) | .../Exporter/Tiny.pm:430:1 (resolves to plenv @INC copy; content identical) | PASS | gold |
| def-02-import-list-member | tricky | exporter | Type::Tiny | `lib/Type/Registry.pm:13:23` `mkopt` | Exporter/Tiny.pm:430 (sub mkopt) | .../Exporter/Tiny.pm:430:1 | PASS | gold |
| def-03-samefile-method | simple | oo-isa | Type::Tiny | `lib/Type/Tiny.pm:414:9` `coercion` | lib/Type/Tiny.pm:525 (sub coercion) | .../Type::Tiny/lib/Type/Tiny.pm:525:5 | PASS | gold |
| def-04-multihop-isa-method | tricky | oo-isa | libwww-perl | `lib/LWP/RobotUA.pm:64:23` `_elem` | lib/LWP/MemberMixin.pm:5 (sub _elem) | .../libwww-perl/lib/LWP/MemberMixin.pm:5:1 (2-hop ISA: RobotUA->UserAgent->MemberMixin) | PASS | gold |
| def-05-use-parent-target | simple | oo-isa | libwww-perl | `lib/LWP/RobotUA.pm:2:14` `LWP::UserAgent` | lib/LWP/UserAgent.pm:1 (package decl) | .../libwww-perl/lib/LWP/UserAgent.pm:1:1 | PASS | gold |
| def-06-onehop-isa-method | tricky | oo-isa | libwww-perl | `lib/LWP/UserAgent.pm:711:36` `_elem` | lib/LWP/MemberMixin.pm:5 (sub _elem) | .../libwww-perl/lib/LWP/MemberMixin.pm:5:1 (1-hop ISA) | PASS | gold |
| def-07-export-member-to-sub | simple | exporter | libwww-perl | `lib/LWP/Simple.pm:8:17` `get` | lib/LWP/Simple.pm:32 (sub get) | .../libwww-perl/lib/LWP/Simple.pm:32:5 (@EXPORT bareword -> defining sub) | PASS | gold |
| def-08-moose-has-accessor | tricky | moose | Dist-Zilla | `lib/Dist/Zilla.pm:182:29` `version` | lib/Dist/Zilla.pm:82 (has version) | .../Dist-Zilla/lib/Dist/Zilla.pm:82:5 | PASS | gold |
| def-09-with-role-target | simple | moose | Dist-Zilla | `lib/Dist/Zilla.pm:4:6` `Dist::Zilla::Role::ConfigDumper` | lib/Dist/Zilla/Role/ConfigDumper.pm:1 (package decl) | .../Dist-Zilla/lib/Dist/Zilla/Role/ConfigDumper.pm:1:1 | PASS | gold |
| def-10-use-constant | simple | constants | URI | `lib/URI.pm:16:18` `HAS_RESERVED_SQUARE_BRACKETS` | lib/URI.pm:9 col 14 (use constant) | .../URI/lib/URI.pm:9:14 | PASS | gold |
| def-11-crossfile-imported-func-call | tricky | exporter | URI | `lib/URI/_query.pm:95:10` `uri_unescape` | lib/URI/Escape.pm:216 (sub uri_unescape) | .../URI/lib/URI/Escape.pm:216:1 (bareword imported call -> defining sub) | PASS | gold |
| def-12-fq-variable | tricky | fq | URI | `lib/URI.pm:116:33` `schemes_without_host_part_re` | lib/URI.pm:27 (our $schemes_without_host_part_re) | .../URI/lib/URI.pm:27:5 | PASS | gold |
| def-13-fq-sub-call | tricky | fq | URI | `lib/URI/otpauth.pm:47:39` `uri_unescape` | lib/URI/Escape.pm:216 (sub uri_unescape) | .../URI/lib/URI/Escape.pm:216:1 | PASS | gold |
| def-14-classic-lexical-var | simple | classic | libwww-perl | `lib/LWP/MemberMixin.pm:9:11` `old` | lib/LWP/MemberMixin.pm:8 (my $old) | .../libwww-perl/lib/LWP/MemberMixin.pm:8:8 | PASS | gold |
| def-15-classmethod-crossfile | tricky | oo-isa | libwww-perl | `lib/LWP/RobotUA.pm:46:31` `new` | lib/LWP/UserAgent.pm:23 (sub new) | .../libwww-perl/lib/LWP/UserAgent.pm:23:1 (Class->method -> constructor cross-file) | PASS | gold |
| def-16-codegen-type-function | tricky | codegen | Type::Tiny | `lib/Type/Tiny.pm:414:39` `Types::Standard::Any` | Types/Standard.pm:215-ish (generated type fn `Any`, no literal sub) | No definition found at 414:39 | FAIL | gold |

## Known-failing detail

- **def-16-codegen-type-function** — `Types::Standard::Any` has no literal `sub Any` anywhere in Standard.pm (`name => "Any"` is declared at L215; `grep 'sub Any'` is empty). Type::Library generates the function at runtime; the tool cannot synthesize a navigable target. True codegen gap.
