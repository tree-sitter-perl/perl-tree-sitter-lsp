# completion

Verified `completion` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| completion-datetime-self-classic | simple | classic | DateTime | `lib/DateTime.pm:207:11` `$self->` | offers DateTime's own methods: new, now, today, clone, year, month, formatter, _set_locale | new/now/today/clone/year/month/formatter/_set_locale (full method list with return types) | PASS | gold |
| completion-catalyst-action-has-moose | tricky | moose | Catalyst-Runtime | `lib/Catalyst/Action.pm:52:24` `$self->` | offers Moose has-synthesized accessors: class, instance, has_instance (predicate), namespace, reverse, attributes, name, code, private_path, number_of_args | all 10 accessors incl. predicate has_instance; number_of_args → Int\|Undef | PASS | gold |
| completion-uri-http-multilevel-isa | tricky | oo-isa | URI | `lib/URI/http.pm:14:23` `$self->` | own default_port, canonical + inherited host/port/userinfo (URI::_server), authority/path (URI::_generic), scheme/new (URI) — full 3-level chain | full chain walked, each method origin-tagged | PASS | gold |
| completion-datetime-hashkey | tricky | type-inference | DateTime | `lib/DateTime.pm:315:30` `$self->{` | offers all `$self->{...}` keys: tz, local_rd_days, local_rd_secs, formatter, locale, local_c, utc_year, rd_nanosecs, offset_modifier, utc_rd_days, utc_rd_secs, utc_c, utc_vals | only utc_vals + tz offered (2 of 13 keys) | FAIL | gold |
| completion-uri-escape-fq-crossfile | tricky | fq | URI | `lib/URI.pm:141:41` `URI::Escape::` | offers URI::Escape's subs cross-file: uri_escape, uri_unescape, uri_escape_utf8, escape_char | all 4 cross-file subs present (uri_escape/uri_escape_utf8 → String) | PASS | gold |
| completion-datetime-partial-off | simple | classic | DateTime | `lib/DateTime.pm:318:36` `$self->off` | candidate set (client-filtered) includes offset and _offset_for_local_datetime | both present in returned candidate set | PASS | gold |
| completion-distzilla-self-moose-typed | tricky | moose | Dist-Zilla | `lib/Dist/Zilla.pm:109:23` `$self->` | has accessors with typed returns: chrome, name (DistName), version (LaxVersionStr), abstract (String), license (License), authors, plugins, distmeta (HashRef), main_module; + predicate/clearer (_has_main_module_override, _clear_license_class) | all accessors with typed returns + synthesized predicate/clearer | PASS | gold |
| completion-jsonpp-arrayidx-invocant | tricky | classic | JSON-PP | `lib/JSON/PP.pm:147:18` `$_[0]->` | offers JSON::PP methods on $_[0] invocant: new, encode, decode, pretty, max_depth, PP_encode_json | all 6 methods present (encode → String, etc.) | PASS | gold |
| completion-plack-request-invocant | simple | classic | Plack | `lib/Plack/Request.pm:41:25` `$_[0]->` | offers Plack::Request methods: env, address, method, path, scheme, secure, cookies, content | all 8 methods present (cookies → HashRef, content → String) | PASS | gold |
| completion-typetiny-imported-blessed | tricky | exporter | Type::Tiny | `lib/Type/Tiny.pm:165:10` `blessed` | bareword-call completion offers imported sub blessed (from `use Scalar::Util qw(blessed)`) alongside local subs | 140 local subs offered; blessed absent (cross-file import gap) | FAIL | gold |
| completion-jsonpp-constants-bareword | simple | constants | JSON-PP | `lib/JSON/PP.pm:342:43` `P_INDENT` | bareword completion offers use-constant subs: P_ASCII, P_LATIN1, P_UTF8, P_INDENT, P_CANONICAL, P_SHRINK | all 6 P_* constants present (plus full P_* set) | PASS | gold |
| completion-catalyst-actionchain-crossfile-isa | tricky | moose | Catalyst-Runtime | `lib/Catalyst/ActionChain.pm:37:11` `$self->` | own has accessors (chain, _context, _chain_last_action) + cross-file inherited Catalyst::Action accessors (class, namespace, reverse, attributes, name, number_of_args) via extends | own + cross-file inherited accessors, origin-tagged | PASS | gold |

## Known-failing detail

- **completion-datetime-hashkey** — `$self->{` offers only 2 keys (utc_vals, tz); misses 10+ keys assigned via `$self->{k}=...` in `_new` and elsewhere. Real gap in mutated-key harvesting for `$self->{` completion (13 distinct keys exist in source).
- **completion-typetiny-imported-blessed** — imported subs absent from bareword-statement completion. Only local package subs offered; the `use Scalar::Util qw(blessed)` import is not surfaced. Real cross-file import-completion gap.

## Rejected (excluded from corpus)

None.
