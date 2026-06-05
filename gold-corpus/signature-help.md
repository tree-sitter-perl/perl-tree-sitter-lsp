# signature-help

Verified `signature-help` rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| sig-typetiny-failed-check-p0 | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:962:23` `_failed_check` | `_failed_check($name, $value, %attrs)` active param 0 ($name); method-call invocant $self elided (def L1062) | `_failed_check($name, $value, %attrs)` active param 0 ($name) | PASS | gold |
| sig-typetiny-failed-check-p1 | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:962:31` `_failed_check` | active param 1 ($value); cursor past the comma advances active param | `_failed_check($name, $value, %attrs)` active param 1 ($value) | PASS | gold |
| sig-stuffer-addr-list-fatcomma-p0 | simple | classic | Email::Stuffer | `lib/Email/Stuffer.pm:312:30` `_assert_addr_list_ok` | `_assert_addr_list_ok($header, $allow_empty, $list)` active param 0 ($header); invocant elided | `_assert_addr_list_ok($header, $allow_empty, $list)` active param 0 ($header) | PASS | gold |
| sig-stuffer-addr-list-fatcomma-p2 | tricky | classic | Email::Stuffer | `lib/Email/Stuffer.pm:312:41` `_assert_addr_list_ok` | active param 2 ($list); two `=>` separators counted as commas | `_assert_addr_list_ok($header, $allow_empty, $list)` active param 2 ($list) | PASS | gold |
| sig-uri-new-class-p0 | simple | oo-isa | URI | `lib/URI.pm:233:24` `new` | `new($uri, $scheme)` active param 0 ($uri); class call, invocant $class elided | `new($uri: String, $scheme)` active param 0 ($uri: String) | PASS | gold |
| sig-uri-new-class-p1 | tricky | oo-isa | URI | `lib/URI.pm:89:29` `new` | `new($uri, $scheme)` active param 1 ($scheme); cursor on 2nd arg | `new($uri: String, $scheme)` active param 1 ($scheme) | PASS | gold |
| sig-uri-check-path-function-noinvocant | tricky | classic | URI | `lib/URI/_generic.pm:58:13` `_check_path` | `_check_path($path, $pre)` active param 0 ($path); PLAIN function call (no `->`), BOTH params must show | `_check_path($pre: String)` active param 0 ($pre: String) — $path wrongly elided as invocant | FAIL | gold |
| sig-stuffer-create-crossfile | tricky | fq | Email::Stuffer | `lib/Email/Stuffer.pm:216:6` `create` | `create(%args)` active param 0 (%args); Email::MIME->create resolved CROSS-DIST (def Email-MIME.pm:211), invocant elided | `create(%args)` active param 0 (%args) | PASS | gold |
| sig-gld-norm-imply-function | simple | classic | Getopt-Long-Descriptive | `lib/Getopt/Long/Descriptive.pm:656:25` `_norm_imply` | `_norm_imply($what)` active param 0 ($what); plain function call, single param preserved | `_norm_imply($what)` active param 0 ($what) | PASS | gold |
| sig-typetiny-new-dynamic-args | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:1246:27` `new` | `new(...)`; def is `my $class = shift; my %params = (@_==1)?%{$_[0]}:@_` — no static positional list, empty `new()` acceptable | `new()` active param 0 () | REVIEW | provisional |
| sig-typetiny-check-shift-style | simple | type-inference | Type::Tiny | `lib/Type/Tiny.pm:902:30` `check` | `check(...)`; def `my $self = shift; (...)->(@_)` — no positional list, empty `check()` acceptable | `check()` active param 0 () | REVIEW | provisional |
| sig-datetime-normalize-nanoseconds-indexarg | simple | classic | DateTime | `lib/DateTime.pm:226:8` `_normalize_nanoseconds` | `_normalize_nanoseconds(...)`; def uses `$_[1]`/`$_[2]` directly (no my-list), empty label acceptable | `_normalize_nanoseconds()` active param 0 () | REVIEW | provisional |

## Known-failing detail

- **sig-uri-check-path-function-noinvocant** — `$path` wrongly elided as invocant on a PLAIN function call `_check_path($rest, $$self)` (no method arrow); signature drops `$path`, shows only `($pre)`, and mislabels active param 0 as `$pre`. Counter-case `sig-gld-norm-imply-function` (single-param plain fn preserved) confirms the heuristic only spares the first param when eliding would empty the list — so the invocant-elision is firing on a non-method call.

## Provisional detail

- **sig-typetiny-new-dynamic-args** / **sig-typetiny-check-shift-style** / **sig-datetime-normalize-nanoseconds-indexarg** — subs that unpack `@_` dynamically (`shift` + `%params`, `shift` + forward-`@_`, or direct `$_[N]` indexing) expose no static positional `my (...) = @_` list, so the empty signature is honest but low-value. Future-enhancement candidates: a richer extractor could surface unnamed positional slots.

## Rejected (excluded from corpus)

None.
