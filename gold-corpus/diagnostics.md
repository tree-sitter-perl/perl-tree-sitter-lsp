# diagnostics

Verified `diagnostics` rows (`--check`). Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary. Three subtypes: `diagnostic-absence` (no diagnostic expected), `diagnostic-tp` (true-positive flag expected), and the FAIL rows are false-positives (a flag where none should fire).

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| diag-01 | simple | classic | JSON-PP | `lib/JSON/PP.pm:63:16` `require B` | no diagnostic (require Bareword) | no diagnostic | PASS | gold |
| diag-02 | simple | classic | JSON-PP | `lib/JSON/PP.pm:133:16` `my $class = shift` | no diagnostic | no diagnostic (shift is builtin) | PASS | gold |
| diag-03 | simple | constants | JSON-PP | `lib/JSON/PP.pm:342:41` `use constant P_INDENT` | no diagnostic (use-constant bareword in hash-slot) | no diagnostic | PASS | gold |
| diag-04 | tricky | codegen | JSON-PP | `lib/JSON/PP.pm:171:8` `indent` | unresolved-method ('indent' on JSON::PP — eval-string codegen) | hint unresolved-method 171:8 "'indent' is not defined in JSON::PP" | PASS | gold |
| diag-05 | tricky | type-inference | Time-HiRes | `HiRes.pm:62:24` `constant` | unresolved-function ('constant' — XS-defined) | info unresolved-function 62:24 "'constant' is not defined in this file" | REVIEW | provisional |
| diag-06 | tricky | exporter | Time-HiRes | `HiRes.pm:100:10` `gettimeofday` | no diagnostic (package calling its own XS/exported function) | hint "'gettimeofday' is exported by Time::HiRes (not yet imported)" | FAIL | gold |
| diag-07 | simple | classic | Net-SSLeay | `lib/Net/SSLeay.pm:24:8` `require Exporter` | no diagnostic (require Bareword) | no diagnostic | PASS | gold |
| diag-08 | tricky | codegen | Net-SSLeay | `lib/Net/SSLeay.pm:1023:1` `bootstrap` | unresolved-function ('bootstrap' — DynaLoader-injected) | info unresolved-function 1023:1 "'bootstrap' is not defined in this file" | REVIEW | provisional |
| diag-09 | tricky | codegen | Log-Log4perl | `lib/Log/Log4perl/Logger.pm:879:6` `is_warn` | no diagnostic (typeglob `*{NAME}=sub` codegen) | hint unresolved-method 879:6 "'is_warn' is not defined in Log::Log4perl::Logger" | FAIL | gold |
| diag-10 | tricky | codegen | Log-Log4perl | `lib/Log/Log4perl/Logger.pm:883:4` `warn` | no diagnostic (typeglob `*{NAME}=sub` codegen) | hint unresolved-method 883:4 "'warn' is not defined in Log::Log4perl::Logger" | FAIL | gold |
| diag-11 | tricky | exporter | CGI-pm | `t/no_tabindex.t:28:3` `submit` | no diagnostic (function imported via nested export :tag) | hint unresolved-function 28:3 "'submit' is exported by CGI but not imported" | FAIL | gold |
| diag-12 | tricky | exporter | CGI-pm | `t/query_string.t:11:4` `is` | no diagnostic (default @EXPORT name with plan-arg import list) | hint unresolved-function 11:4 "'is' is exported by Test::More but not imported" | FAIL | gold |
| diag-13 | simple | codegen | CGI-pm | `lib/CGI.pm:1206:0` `*in` | no diagnostic (typeglob alias assignment) | no diagnostic (CGI.pm has only 1 diag at L299) | PASS | gold |
| diag-14 | simple | exporter | JSON-PP | `xt/900_pod.t:8:0` `all_pod_files_ok` | unresolved-function (imported only via eval-string use) | info unresolved-function 8:0 "'all_pod_files_ok' is not defined in this file" | PASS | gold |

## Known-failing detail (false positives — flags that should not fire)

- **diag-06 (gettimeofday)** — own export called within defining package: telling you to import a name from the very module you're defining is a true FP. File is single package Time::HiRes; gettimeofday is its own @EXPORT_OK called inside sub tv_interval.
- **diag-09 / diag-10 (is_warn / warn)** — dynamic typeglob codegen methods (`*{__PACKAGE__."::is_$lclevel"}=sub{...}` driven by a foreach over %PRIORITY) the server cannot fold; FP on real methods. Logger.pm has 18 such diagnostics.
- **diag-11 (submit)** — nested export `:tag` not recursively expanded: `use CGI (":standard")` -> ':standard' includes ':form' -> ':form' contains 'submit'. Server does not recurse. One of 16 such on this file.
- **diag-12 (is)** — plan directive misread as restricting import list: `use Test::More 'no_plan'` — the arg is a plan directive, not an export selector; Test::More still ships default @EXPORT (is/ok/like/...). Server reads the arg list as a restricting import list.

## REVIEW-class detail

- **diag-05 (constant) / diag-08 (bootstrap)** — XS/DynaLoader-provided functions: semantically defined at runtime but invisible to static analysis. Straddle TP/FP — honest-silent gaps.
