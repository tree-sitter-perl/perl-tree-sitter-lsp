# QA scout: R and CMake language packs

Scope: the two minimal packs (`queries/r/skeleton.scm` ~61 lines,
`queries/cmake/skeleton.scm` ~32 lines) driven by `src/query_extract.rs`.
Method: sweep `--outline` over real codebases, bucket each file, reduce
every gap to a minimal verified repro. Binary built with
`cargo build --release --features all-langs`.

Corpora swept:
- **R**: `tidyverse/dplyr` (106 `R/*.R`) + `r-lib/cli` (74 `R/*.R`) = 180 files.
- **CMake**: spdlog, nlohmann/json, abseil, re2 under `/tmp` — 64 `CMakeLists.txt`, 14 `*.cmake`.

Note: `perl-lsp --parse` always uses the **Perl** grammar, so it cannot
inspect R/CMake CSTs. Every claim below is verified through the actual
pack driver via `--outline` / `--completion`.

---

## R

### Buckets (180 files)
| bucket | count |
|---|---|
| CRASH | 0 |
| EMPTY-with-decls | 0 (all 10 empty files are legit: `data-*.R`, `reexport-*.R`, roxygen-only) |
| STRUCTURE-CORRUPT | 0 |
| OK (top-level defs extracted) | 170 |

Top-level def extraction is solid. The R problem is **under-extraction of
nested OOP methods**, which the non-empty bucket count hides.

### What works (verified)
- `f <- function(x) {...}` and `f = function(x) {...}` → `Sub f` + params. The dominant R def form.
- S3 dot-named methods: `print.myclass <- function(...)` → `Sub print.myclass`.
- Function parameters surface as `Variable`.
- In-scope completion: `--completion` returns visible function/var names.

### Gaps, ranked by prevalence

**1. (HIGH) `name = function(...)` inside `list()` / `R6Class()` is not extracted.**
The R OOP idiom — R6 classes, S3 method tables, environment objects — defines
methods as *named arguments* (`argument name:(identifier) value:(function_definition)`),
not `binary_operator`. The skeleton's `@def.sub` only matches
`(binary_operator lhs:(identifier) ["<-" "="] rhs:(function_definition))`.

Repro:
```r
obj <- list(greet = function(x) x + 1,
            bye   = function() 0)
```
Extracted: `[Variable obj, Variable x]` — `greet`/`bye` missing (only the
orphan param `x` leaks through). Same for `R6Class("Foo", public=list(initialize=function()..., run=function(x) x))`
→ only `Variable Foo`.

Prevalence: **29 / 180 files, 195 method defs missed.** Single biggest
hit (e.g. `cli/R/cliapp.R` 39 methods, `cli/R/themes.R`, `dplyr/R/data-mask.R`).

Fix: **quick-ish skeleton add** — a pattern like
`(argument name:(identifier) @def.sub.name value:(function_definition)) @def.sub @scope`.
Tagging each method with its owning object/class (package) is deeper (needs
a `@context`-style sticky from the enclosing `list(`/`R6Class(` call).

**2. (MEDIUM, deep) `$` member completion does not surface keyed-shape keys.**
The skeleton builds `HashWithKeys` witnesses for `list`/`data.frame`/`tibble`
(via `shape_ctor`), but `$` member completion is not wired to them.

Repro:
```r
df <- data.frame(age = 1, name = "a")
df$        # completion here
```
`--completion … 1 3` returns `df` (in-scope vars), **not** `age`/`name`.
Same for `o <- list(alpha=1, beta=2); o$`.

Fix: **deep** — member-access completion path (cursor sentinel) must handle
R's `$` receiver and read the shape witnesses. Not a skeleton change.

**3. (LOW, trivial) `<<-` superassignment functions not captured.**
```r
h <<- function(x) x + 1
```
→ `[Variable x]`, no `Sub h`. The def pattern's operator set is `["<-" "="]`.
Fix: **trivial** — add `"<<-"` to the alternation. Lower prevalence (closures/memoization).

**4. (corpus-dependent, deep) S4 `setClass`/`setGeneric`/`setMethod` produce no defs.**
```r
setClass("Acct", representation(bal="numeric"))
setGeneric("dep", function(x) standardGeneric("dep"))
setMethod("dep", "Acct", function(x) x@bal)
```
→ `[]` / orphan params only. Zero in the tidyverse corpus (0/180 files), but
common in Bioconductor / classic CRAN. Fix: **deep** — R has no
call-classification predicate (the CMake pack's `cmd_effects` analogue);
the string-arg-as-name convention would need new machinery.

**5. (SUSPECTED, not outline-verified) `pkg::fn()` calls not captured as refs.**
`@ref.call` keys on `function:(identifier)`; a qualified call's `function:`
is a `namespace_operator`, so `dplyr::filter(df, …)` registers no call ref.
Does not affect `--outline` (refs aren't outline symbols); would affect
goto-def/references/rename on qualified calls. Prevalence high — **118 / 180
files use `pkg::fn`.** Flagged at lower confidence because the QA surface
(outline/completion) can't observe it directly. Fix: skeleton add for the
namespaced call shape.

---

## CMake

### Buckets
| bucket | count |
|---|---|
| `CMakeLists.txt` → EMPTY (routing) | **64 / 64** |
| `*.cmake` OK-but-noisy | 13 / 14 |
| `*.cmake` legit-empty (generated `list(APPEND …)` only) | 1 / 14 |
| CRASH | 0 |

### What works (verified)
- `function(name …)` / `macro(name …)` defs → `Sub name` (name correct).
- Single-arg `set(SOLO)` → one `Variable SOLO`.
- In-scope completion returns user function/macro names.

### Gaps, ranked

**1. (CRITICAL, quick fix) `CMakeLists.txt` routes to the Perl builder → empty outline.**
`LanguageRegistry::for_path` matches by **extension only** (`exts: &["cmake"]`),
and `CMakeLists.txt` has no extension, so it falls through to the Perl
builder and produces `[]`. This is *the* dominant CMake filename.

Verified: `--outline /tmp/spdlog/CMakeLists.txt` → `[]`; the identical bytes
copied to `*.cmake` → **205 symbols**. **64/64 swept `CMakeLists.txt` empty.**

Fix: **quick** — match the full filename `CMakeLists.txt` (and `*.cmake.in`)
in `for_path`, not just the extension. The driver comment already flags it
as "a follow-up."

**2. (HIGH, structure-corrupt, medium fix) Multi-arg commands turn every argument into a def.**
The pattern `(normal_command (identifier) @cmd (argument_list (argument) @cmd.arg))`
produces **one tree-sitter match per argument**, so each argument forms its
own single-arg "command match." The driver groups `cmd_args` by `match_id`,
then `cmd_effects` runs `Def { name_arg: 0 }` against each single-arg match —
so **every** argument becomes arg[0] and is emitted as a def.

Repro:
```cmake
set(MY_VAR aaa bbb ccc)
```
→ `[Variable MY_VAR, Variable aaa, Variable bbb, Variable ccc]` (only `MY_VAR` is a var).

Worse on real code (`spdlog/cmake/utils.cmake`):
```cmake
set(SPDLOG_VERSION_MAJOR ${ver_major} PARENT_SCOPE)
```
→ emits `SPDLOG_VERSION_MAJOR`, `${ver_major}` (a variable *reference*, mis-tagged
as a def), **and** `PARENT_SCOPE` (a keyword) all as `Variable`. Likewise
`add_library(t STATIC a.cpp b.cpp)` → `t`, `STATIC`, `a.cpp`, `b.cpp` all `Sub`/target.
`option(BUILD_TESTS "desc" ON)` → all three as `Variable`. Affects essentially
every non-trivial CMake file.

Fix: **medium** — group a command's args by the **command node** (not
`match_id`), so `cmd_effects` sees the full ordered arg list once and
`name_arg` indexing is meaningful again. Either capture `(argument_list)`
once, or key the driver's `cmd_args`/`cmd_names` on the command identifier span.

**3. (LOW) `project(MyProj …)` declares no symbol.**
`project` / `enable_language` aren't in `cmd_effects`; the project name is
not surfaced. Minor — add a `Def` effect.

---

## Fix-effort summary

| # | lang | gap | prevalence | effort |
|---|---|---|---|---|
| C1 | CMake | `CMakeLists.txt` → empty (filename routing) | 64/64 files | quick (filename match) |
| C2 | CMake | multi-arg cmd → every arg a def (corrupt) | ~all files | medium (driver arg-grouping) |
| R1 | R | `name = function` in `list()`/`R6Class()` missed | 29 files / 195 defs | quick-ish skeleton add |
| R2 | R | `$` member completion ignores shape keys | all keyed shapes | deep (completion wiring) |
| R5 | R | `pkg::fn()` not a call ref (suspected) | 118 files | quick skeleton add |
| R3 | R | `<<-` function defs missed | low | trivial (add operator) |
| R4 | R | S4 setClass/Generic/Method | 0 in tidyverse | deep (no classify predicate) |
| C3 | CMake | `project()` name | every project | low |
