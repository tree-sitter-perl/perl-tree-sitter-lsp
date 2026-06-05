# references

Verified `references` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| ref-uri-host-server | tricky | oo-isa | URI | `lib/URI/_server.pm:71:4` `host` | N>=3 incl. def _server.pm:72, uses :104,:139; EXCLUDES sibling def file.pm:39 | 3 refs: _server.pm:72 (def), :104, :139. file.pm:39 NOT present. | PASS | gold |
| ref-uri-uri_unescape | tricky | exporter | URI | `lib/URI/Escape.pm:215:4` `uri_unescape` | N>=10 incl. def Escape.pm:216, @EXPORT :149; cross-file plain + FQ (ldapi.pm:14) | 47 refs incl. Escape.pm:149 (@EXPORT), :216 (def), URL.pm, file.pm:39, gopher.pm, ldapi.pm:14 (FQ), news.pm, ... | PASS | gold |
| ref-moo-make_class | simple | oo-isa | Moo | `lib/Moo.pm:53:4` `make_class` | N>=2: def Moo.pm:54 and call :50 | 2 refs: Moo.pm:50 (call), :54 (def). | PASS | gold |
| ref-datetime-year | tricky | oo-isa | DateTime | `lib/DateTime.pm:766:4` `year` | N>=20 incl. def :767 and ->year calls in DateTime.pm + t/*.t; no Duration merge | 53 refs across DateTime.pm + 12 test files; def :767. No Duration.pm (Duration has only `sub years`). | PASS | gold |
| ref-uri-const-fq | tricky | constants | URI | `lib/URI.pm:8:13` `HAS_RESERVED_SQUARE_BRACKETS` | N>=8: def :9, unqualified :17,21,22,115, FQ cross-file _generic.pm:13, _server.pm:26,27 | 5 refs: :9 (def), :17,:21,:22,:115. MISSING FQ uses; reverse query from FQ site returns 0. | FAIL | gold |
| ref-uri-canonical-http | tricky | oo-isa | URI | `lib/URI/http.pm:11:4` `canonical` | Only http.pm:12 def; EXCLUDES _server.pm:150 and URI.pm:353; SUPER::canonical targets parent | 1 ref: http.pm:12 (def only). _server.pm:150, URI.pm:353 NOT present. | PASS | gold |
| ref-expt-mkopt | tricky | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:429:4` `mkopt` | N>=6: def :430, @EXPORT_OK :9, call sites :70,:105,:168,:457 | 3 refs: :9, :430, :457. MISSING :70,:105,:168 (query from :70 returns {70,105,168} but not def/export). | FAIL | gold |
| ref-expt-tag_opts-lexical | simple | classic | Exporter-Tiny | `lib/Exporter/Tiny.pm:161:6` `tag_opts` | N==10 lexical uses incl. deref `%$tag_opts` at :173 | 9 refs (162,164x2,166x2,174,175,176,177). MISSING deref-form `%$tag_opts` at :173. | FAIL | gold |
| ref-uri-escape_char-fq | tricky | fq | URI | `lib/URI/Escape.pm:233:4` `escape_char` | N>=10: def :234 and many FQ `URI::Escape::escape_char(...)` call sites | 2 refs: Escape.pm:234 (def), URI.pm:344. MISSING other FQ sites. (escape_char NOT exported.) | FAIL | gold |
| ref-plack-toapp-component | tricky | oo-isa | Plack | `lib/Plack/Component.pm:46:4` `to_app` | N>=10 for Plack::Component::to_app incl. def Component.pm:47; EXCLUDES Builder.pm:53/125/129/131 | 22 refs incl. Component.pm:47 (def), CGIBin.pm:49, Builder.pm:59 (only), Middleware.pm:16, Runner.pm:259, t/*.t. Builder def + self-calls excluded. | PASS | gold |
| ref-plack-toapp-builder | tricky | oo-isa | Plack | `lib/Plack/Builder.pm:52:4` `to_app` | N>=2 for Plack::Builder::to_app incl. def :53 and self-call :131; EXCLUDES Component.pm:47 | 7 refs: Builder.pm:53 (def), :131, t/Plack-Builder/oo_interface.t:30,38,46,52,73. Component.pm:47 excluded. | PASS | gold |
| ref-minion-backend-accessor | tricky | moo | Minion | `lib/Minion.pm:17:5` `backend` | N>=10: Mojo::Base accessor def :18 and ->backend call sites in Minion.pm + t/*.t | 79 refs incl. :18 (has accessor synth), :39..:182, examples, t/pg.t (60), t/pg_worker.t (2). (Cursor must sit on bareword col 5, not quote col 4 = 0 refs.) | PASS | gold |
| ref-minion-enqueue | tricky | type-inference | Minion | `lib/Minion.pm:56:4` `enqueue` | N>=10 Minion::enqueue incl. def :57; EXCLUDES Backend.pm:18, Pg.pm:75 | 105 refs; def :57; t/pg.t (95), t/pg_worker.t (6), minion_bench.pl (3), Minion.pm (1). 0 Backend matches. | PASS | gold |
| ref-datetime-p-lexical | simple | classic | DateTime | `lib/DateTime.pm:190:8` `p` | N>=20 uses of lexical %p in _new; confined to sub body; EXCLUDES the separate %p in sub new | 27 refs all within :191-256 (sub _new body). The %p in sub new excluded. | PASS | gold |
| ref-datetime-era_abbr-glob | tricky | codegen | DateTime | `lib/DateTime.pm:779:4` `era_abbr` | N>=3: def :780, typeglob alias `*era = \&era_abbr` :783, use :790, cross-file test calls | 7 refs: :780 (def), :783 (alias, col 10), :790, t/03components.t:95,96,234,248. | PASS | gold |

## Known-failing detail

- **ref-uri-const-fq** — `use constant` value is not exported, so FQ `URI::HAS_RESERVED_SQUARE_BRACKETS` references do not link to the def; the FQ-site->def link is also broken (0 refs from a FQ site).
- **ref-expt-mkopt** — split reference graph: the def/@EXPORT_OK cluster (+ call :457) does not cross-link to the three `mkopt(...)` call sites inside import/unimport/_exporter_merge_opts.
- **ref-expt-tag_opts-lexical** — lexical scoping correct (all in-sub) but the `%$tag_opts` hash-deref spelling at line 173 is not emitted as a ref.
- **ref-uri-escape_char-fq** — escape_char is NOT in @EXPORT/@EXPORT_OK; FQ-only calls to a non-exported sub mostly fail to resolve cross-file (contrast exported uri_unescape, which resolves its FQ calls).
