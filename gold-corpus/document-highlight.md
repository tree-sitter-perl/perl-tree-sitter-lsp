# document-highlight

Verified `document-highlight` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| dh-uri-impclass | simple | classic | URI | `lib/URI.pm:64:8` `$impclass` | 5 occurrences in sub new: decl 65:8 READ, writes 70:7/77:5/80:6, read 83:12; compound `\|\|=` and paren-assign are WRITE | 65:8 READ; 70:7 WRITE; 77:5 WRITE; 80:6 WRITE; 83:12 READ | PASS | gold |
| dh-uri-str-init | tricky | classic | URI | `lib/URI.pm:97:9` `$str` | 7 occurrences in sub _init: decl 98:8, writes 100:5/101:5, reads 100:33/101:21 (interpolated)/101:34/103:23 | 98:8 READ; 100:5 WRITE; 100:33 READ; 101:5 WRITE; 101:21 READ; 101:34 READ; 103:23 READ | PASS | gold |
| dh-uri-scheme-shadow | tricky | classic | URI | `lib/URI.pm:55:21` `$scheme` | 10 occurrences in sub new (decl 56:22, writes 67:2/71:6/74:6, 6 reads); package var $scheme_re NOT merged | 56:22 READ; 67:2 WRITE; 70:23 READ; 71:6 WRITE; 71:16 READ; 73:9 READ; 73:20 READ; 74:6 WRITE; 77:31 READ; 83:35 READ | PASS | gold |
| dh-uri-uri-subst | tricky | classic | URI | `lib/URI.pm:55:17` `$uri` | 10 occurrences in sub new: decl 56:16, write 58:5, reads 58:21/58:30, s/// targets 60:5/61:5/62:5/63:5, reads 66:9/83:29 | 56:16 READ; 58:5 WRITE; 58:21 READ; 58:30 READ; 60:5 READ; 61:5 READ; 62:5 READ; 63:5 READ; 66:9 READ; 83:29 READ | REVIEW | provisional |
| dh-uri-implements-hashkey | tricky | classic | URI | `lib/URI.pm:12:3` `%implements` | 5 occurrences of package hash: decl 13:4, read 162:19, write 164:9, read 168:14, write 196:5; POD prose (L435) excluded | 13:4 READ; 162:19 READ; 164:9 WRITE; 168:14 READ; 196:5 WRITE | PASS | gold |
| dh-jsonpp-self-new-shadow | tricky | classic | JSON-PP | `lib/JSON/PP.pm:134:8` `$self` | 3 occurrences confined to sub new: decl-with-init 135:8, reads 141:5/143:11; sibling-sub $self excluded | 135:8 READ; 141:5 READ; 143:11 READ | REVIEW | provisional |
| dh-jsonpp-object_to_json-sub | simple | classic | JSON-PP | `lib/JSON/PP.pm:348:12` `object_to_json` | 5 occurrences of method: def 349:9 + calls 341:27/390:35/425:55/446:41; sibling value_to_json excluded | 341:27 READ; 349:9 READ; 390:35 READ; 425:55 READ; 446:41 READ | PASS | gold |
| dh-datetime-normalize_seconds-sub | simple | classic | DateTime | `lib/DateTime.pm:394:8` `_normalize_seconds` | 4 occurrences of sub: def 395:5 + calls 343:24/363:24/1925:16 | 343:24 READ; 363:24 READ; 395:5 READ; 1925:16 READ | PASS | gold |
| dh-moo-target-import-shadow | tricky | moo | Moo | `lib/Moo.pm:37:5` `$target` | 4 occurrences confined to sub import: decl 38:6, reads 40:52/49:25/50:22; ~20 sibling-sub $target decls excluded | 38:6 READ; 40:52 READ; 49:25 READ; 50:22 READ | PASS | gold |
| dh-plack-env-scoped | simple | classic | Plack | `lib/Plack/Request.pm:206:8` `$env` | 6 occurrences in sub _uri_base: decl 207:8 + 5 hash-deref reads 209:16/211:10/211:33/211:69/212:10; other-sub $env excluded | 207:8 READ; 209:16 READ; 211:10 READ; 211:33 READ; 211:69 READ; 212:10 READ | PASS | gold |

## Provisional detail

- **dh-uri-uri-subst** — occurrence SET is fully correct (10/10), but the `$uri =~ s/.../.../;` substitutions at 60–63 mutate `$uri` in place yet are classified READ (`=~` treated as a read of the binding). Defensible, but a substitution is arguably WRITE — provisional on the READ/WRITE ambiguity.
- **dh-jsonpp-self-new-shadow** — scoping is perfect (only new()'s 3 sites; dozens of sibling `$self` excluded), but `my $self = {...}` at 135 is an initializer assignment yet classified READ (systematic: `my` decls always READ regardless of initializer). Provisional on the write-classification subtlety.

## Rejected (excluded from corpus)

None.
