# C/C++ stdlib + include-path auto-configuration — how a build-free, tree-sitter LSP can discover what it needs with (near) zero user config

> Strategic frame: this engine's category-level edge over clangd is **zero-config + speed**
> (`docs/cpp-lsp-experience-research.md`). clangd's #1 documented pain is that *everything*
> hinges on a correct `compile_commands.json`; without it the stdlib isn't found and the
> experience "collapses to red squiggles everywhere." If we make a hand-maintained compile DB
> a *requirement*, we forfeit the whole advantage. The `#include`-closure macro gather in
> `src/cpp_reparse.rs` already walks quoted includes up ancestor dirs — but it skips `<...>`
> system includes because it doesn't know **where the stdlib lives**. This doc is about
> discovering (a) system/stdlib header roots, (b) the project's own `-I` dirs, and (c) key
> predefined macros — **without running a build**, cheaply, cached, and degrading gracefully.

---

## Executive summary

There is exactly one authoritative, portable, cheap source of the truth we need that does **not**
require a build: **ask the compiler itself.** Every serious tool (clangd's `--query-driver`, MS
C/C++ IntelliSense's `compilerPath` query, Bear-generated flags that name a driver) ultimately
bottoms out in *executing the compiler with introspection flags and parsing stdout/stderr*. Three
one-shot invocations give us essentially everything:

1. **System include search dirs** — `echo | cc -xc++ -E -v -` → parse the block between
   `#include <...> search starts here:` and `End of search list:`.
2. **Predefined macros** — `cc -xc++ -dM -E - < /dev/null` → every `#define` the compiler bakes in
   (`__GNUC__`, `__cplusplus`, `_WIN32`, `__x86_64__`, `__SIZEOF_LONG__`, feature-test macros…).
3. **Toolchain anchors** — `clang -print-resource-dir` (built-in headers: `stddef.h`, `stdarg.h`,
   intrinsics) and `gcc -print-search-dirs` (GCC internal dirs). On macOS,
   `xcrun --show-sdk-path` for the SDK sysroot.

These are **fast** (single `execvp`, no compilation, milliseconds), **portable** across gcc/clang
on Linux/macOS (identical `-E -v` / `-dM` markers), and their output is **stable per toolchain** —
so we run them **once per (compiler, target) and persist**, exactly like the engine already
persists the zstd macro table (`PersistedMacros` in `cpp_reparse.rs`).

When a real compile DB *is* present (CMake with `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`, Bear, Meson,
Bazel, xmake all emit `compile_commands.json`), we should **consume it** for the project's own
`-I`/`-isystem`/`-D` — but even then, the *stdlib* paths usually aren't in it (they're implicit in
the driver), so the compiler-probe of `arguments[0]` remains necessary. The two layers compose:
**compile DB → project flags + which driver; compiler probe → stdlib roots + predefined macros.**

**Recommended layered order** (fully detailed at the end):
0. Respect an explicit `.clangd` / `c_cpp_properties.json` / `compile_flags.txt` if the user has one.
1. Find & parse `compile_commands.json` (walk up from the file; check `build/`), take per-file
   `-I`/`-isystem`/`-D` and the driver `arguments[0]`.
2. Probe that driver (or a discovered default `cc`/`c++`/`cl`) **once**, cache: system include
   roots + `-dM` predefined macros + resource-dir.
3. Heuristic fallback when no compiler and no DB: known stdlib locations + walk-up for project
   `-I` roots.

**Single highest-payoff first step:** implement the compiler probe (#2) — `cc -E -v -` +
`cc -dM -E -` + `-print-resource-dir`, cached per toolchain — because it delivers the stdlib
header roots *and* the predefined macros that unblock the existing `<...>` macro-gather in one
cheap, build-free shot, and it is the fallback that makes every other layer optional.

---

## 1. How the incumbents discover it (and what they actually invoke)

| Tool | Project flags source | Stdlib / system-header discovery | Predefined macros | What it *executes* |
|---|---|---|---|---|
| **clangd** | `compile_commands.json` (walk up from file, then parents) or `compile_flags.txt`; `.clangd` overlays via `CompileFlags: Add/Remove` | Heuristic from **driver location + target triple** first; **`--query-driver`** (opt-in, glob-allowlisted) runs the real compiler to get search paths & target; built-in headers forced to clangd's own via injected `-resource-dir=` | Comes from parsing the TU with clang's own preprocessor once flags/target are known; query-driver aligns the target triple | `‹driver› -E -xc++ -v /dev/null` (query-driver); internally runs clang as a library |
| **MS C/C++ (IntelliSense)** | `c_cpp_properties.json` (`includePath`, `defines`, `compilerPath`, `compilerArgs`) or a `compile_commands.json` reference | **Queries the compiler at `compilerPath`** to retrieve system include dirs and default defines; auto-detects installed compilers into a "Select IntelliSense Configuration" quick-pick; on Windows defaults to latest MSVC | Obtained from the same compiler query; `intelliSenseMode` emulates target arch so `size_t`/pointer widths are right | Runs `compilerPath` with query args (`-E -v`-style for gcc/clang; MSVC via its own probe). `compilerPath: ""` **skips** the query |
| **ccls** | `compile_commands.json` in project root or `compilationDatabaseDirectory`; `.ccls` file (driver on line 1, one arg/line, `-I`/`-D`), applied to files not in the DB (or overlaid) | `clang.resourceDir` — the clang **resource dir is hard-coded at build time** (`.../lib/clang/<ver>`); `--gcc-toolchain` in `.ccls`/`initializationOptions` to borrow GCC's libstdc++ | Via the bundled libclang once flags are set | Uses libclang; resource-dir is compile-time, not probed |
| **CLion** | CMake/compile-DB project model (its own resolver + bundled clangd) | Through the configured toolchain's compiler | Compiler-derived | Drives CMake; runs the toolchain compiler |

**The throughline:** the *stdlib* is discovered **relative to the compiler driver**, not by
assuming `/usr/include`. clangd's own docs: *"the standard library headers are often found relative
to the compiler… configure with an absolute path to the compiler (`/usr/bin/gcc`, not `gcc`)."*
And the recommended way to get it right is **`--query-driver` over hand-listing include paths**,
because *"getting the latter right can be tricky (the order of include paths is important)."* That
is a direct endorsement of the compiler-probe strategy this engine should adopt as its default.

Sources: clangd system headers — <https://clangd.llvm.org/guides/system-headers>; clangd compile
commands / DB search — <https://clangd.llvm.org/design/compile-commands>; clangd troubleshooting
(driver-relative stdlib, `-v` to see search dirs) — <https://clangd.llvm.org/troubleshooting>;
MS IntelliSense config (compiler query, `compilerPath: ""` skips) —
<https://code.visualstudio.com/docs/cpp/configure-intellisense>,
<https://code.visualstudio.com/docs/cpp/customize-cpp-settings>; ccls customization (resource dir
hard-coded, `.ccls`, `compilationDatabaseDirectory`) —
<https://github.com/MaskRay/ccls/wiki/Customization>,
<https://github.com/MaskRay/ccls/wiki/Project-Setup>.

---

## 2. Build-system signals (extract flags without a full build)

### 2a. `compile_commands.json` — the gold standard, consume it when present

The **JSON Compilation Database** is an array of *command objects*, one per translation unit:

| Field | Meaning | We use it for |
|---|---|---|
| `directory` | working dir of the compile; **all relative paths in `command`/`file` resolve against it** | resolving `-I../foo`, relative driver path |
| `file` | the TU source path | keying: which entry applies to which file |
| `arguments` (preferred) or `command` | the full invocation; `arguments[0]` = the driver (`clang++`, `/usr/bin/c++`, `cl`) | extract `-I` / `-isystem` / `-iquote` / `-D` / `-std=` / `--target=` / `-isysroot`; and **which driver to probe** |
| `output` (optional) | object file name | (ignored) |

Prefer `arguments` (a real argv, no shell-unescaping guesswork) over `command` (a string you must
tokenize). Extraction is a **pure flag scan** — no compilation. Note two things: (1) the **stdlib
`-isystem` paths are usually NOT in the entry** — they're implicit in the driver, which is why the
compiler probe (§3) is still needed even with a perfect DB; (2) **header files typically have no
entry** — clangd's trick is to "borrow" the command from a TU that includes the header, or infer
`bar/foo.h` from `bar/foo.cc` by filename. We should do the same (nearest-TU / sibling-name).

**Where to look** (mirror clangd exactly): the source file's directory, then walk **up** parent
dirs; also probe a `build/` subdir (CMake's default out-of-tree location). ccls adds an explicit
`compilationDatabaseDirectory` knob for out-of-tree builds — worth exposing as an
`initializationOption`.

**How each build system emits it (all build-free or a cheap configure step, never a full build):**

| Build system | How to get `compile_commands.json` | Cost |
|---|---|---|
| **CMake** | `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON` (or `set(... ON)` in `CMakeLists.txt`); written to the **build dir**. Makefile + Ninja generators only | **configure only**, no build |
| **Meson** | emits `compile_commands.json` in the build dir automatically on `meson setup` | configure only |
| **Bazel** | via `hedronvision/bazel-compile-commands-extractor` (analyzes the action graph) | analysis, no full build |
| **xmake** | `xmake project -k compile_commands` | configure only |
| **Make / autotools / anything** | **Bear**: `bear -- make` intercepts the exec calls and records the real argv → most reliable for opaque builds. Also `compiledb -n make` (dry-run parse, no compile) | Bear needs one build; `compiledb -n` is a dry run |

So the right posture: **if a `compile_commands.json` already exists, use it; do not require the user
to generate one.** We never run CMake/Bear ourselves for the user — that reintroduces the exact
friction we're differentiating against. We *can* mention in docs "if you have a build, point us at
its `compile_commands.json` for best project-include accuracy," but the default path must not need it.

Sources: JSON DB spec (fields, `arguments` preferred, `arguments[0]` = executable) —
<https://clang.llvm.org/docs/JSONCompilationDatabase.html>; `CMAKE_EXPORT_COMPILE_COMMANDS`
(Makefile/Ninja only, writes to build dir) —
<https://cmake.org/cmake/help/latest/variable/CMAKE_EXPORT_COMPILE_COMMANDS.html>; Bear (intercepts
build, prefer native export for CMake/Meson/Bazel) — <https://github.com/rizsotto/Bear>; a gallery
of per-build-system recipes — <https://blog.bkryza.com/posts/compile-commands-json-gallery/>.

### 2b. CMake without a compile DB — File API / `--system-information`

If there's a `CMakeLists.txt` but no `compile_commands.json`, two build-free options exist, but both
are **heavier and less portable than the compiler probe**, so treat them as optional enrichment:

- **CMake File API** (`.cmake/api/v1/query`): after a `cmake` configure, structured JSON codemodel
  files expose per-target include dirs and defines. Requires having *configured* the project.
- **`cmake --system-information`**: dumps cache/platform variables including some default include
  info, but it's coarse and generator-dependent.

Verdict: not worth it as a primary path. If a project uses CMake, either a `compile_commands.json`
already exists (§2a) or the compiler probe (§3) covers the stdlib and a walk-up covers project
`-I`. Reach for File API only if we later want *accurate per-target* project defines with no DB.

### 2c. Make / automake directly — Bear is the only sane route

There is nothing reliably extractable from a hand-written `Makefile` short of intercepting the
compiler exec (Bear) or a dry-run (`compiledb -n make`). Do **not** try to parse `Makefile`s for
`-I`/`-D` — recursive make, computed variables, and generated flags make it a losing game. If the
user has run Bear, we get the DB via §2a; otherwise we fall back to §3 + §4.

---

## 3. Compiler introspection — the zero-config core

This is the load-bearing section. These invocations are cheap (one process spawn, **no
compilation**), portable across gcc/clang on Linux and macOS, and their output is deterministic per
toolchain. Run once; cache keyed on the toolchain identity.

### 3a. System include search directories

```sh
# C++ (use -xc for a C TU); reads empty stdin, preprocesses nothing real:
echo | cc -xc++ -E -v - 2>&1
```

Parse **stderr** for the block delimited by two exact literal markers:

```
#include <...> search starts here:
 /usr/include/c++/13
 /usr/include/x86_64-linux-gnu/c++/13
 /usr/lib/gcc/x86_64-linux-gnu/13/include
 /usr/local/include
 /usr/include/x86_64-linux-gnu
 /usr/include
End of search list.
```

Each non-empty line between the markers (trim leading space) is a system include root, **in search
order** (order matters — preserve it). There's also a preceding `#include "..." search starts here:`
block for quote-includes; the `<...>` block is the one for angle-bracket/system headers. This exact
recipe is the widely-used one:

```sh
echo | gcc -E -x c++ - -v 2>&1 | sed -n '/#include <...> search starts here:/,/End of search list/p'
```

- **gcc and clang both** emit these identical markers → one parser handles both.
- `-xc++` vs `-xc` selects the C++ vs C search path (C++ adds the `c++/<ver>` dirs). We should probe
  **both** and pick per-file-language.
- macOS: `cc`/`gcc` is really Apple clang; the same command works. The paths point into the active
  Xcode SDK. Combine with `xcrun --show-sdk-path` (SDK sysroot) and `xcrun -f clang` (tool path) if
  we need to pin the SDK, honoring `SDKROOT`.

### 3b. Predefined macros (unblocks the existing macro-gather)

```sh
cc -xc++ -dM -E - < /dev/null        # C++ ; use -xc for C
# equivalently: cc -dM -E -xc++ /dev/null
```

Emits **every** compiler-baked `#define`, one per line:

```
#define __GNUC__ 13
#define __cplusplus 201703L
#define __x86_64__ 1
#define __SIZEOF_LONG__ 8
#define __STDC_HOSTED__ 1
#define _WIN32 1        (on MSVC-targeting clang)
...
```

This is **directly what `cpp_reparse.rs` wants**: these macros (`__GNUC__`, `__cplusplus`,
`_WIN32`, `__x86_64__`, feature-test macros, `__has_include` gating values) are exactly the ones
that decide which `#if`/`#ifdef` arms in system headers are live. Feeding the `-dM` set into the
macro table as the **EXTERNAL/toolchain seed** (alongside the per-file `#include`-closure gather)
lets the conditional-compilation in `<...>` headers resolve the way the real toolchain would.
gcc and clang share the `-dM -E` interface; the macro *contents* differ (that's the point — we
capture the actual toolchain's truth, not a guess). `-std=` / `--target=` from the compile DB should
be forwarded to this probe so `__cplusplus` and width macros match the project.

### 3c. Toolchain anchors (built-in headers & internal dirs)

```sh
clang -print-resource-dir      # → .../lib/clang/18  ; built-in headers live in <that>/include
                               #   (stddef.h, stdarg.h, stdbool.h, intrinsics, __stddef_*.h)
gcc   -print-search-dirs       # → install:/programs:/libraries: lines; the GCC-internal include dir
gcc   -print-prog-name=cc1     # locate the actual cc1 (rarely needed)
clang --version / gcc --version# toolchain identity for the cache key
```

`-print-resource-dir` matters because a handful of headers (`<stddef.h>`, `<stdarg.h>`,
`<stdbool.h>`, `<stdint.h>` shims, SIMD intrinsics) are **compiler-owned**, not in `/usr/include` —
they live under the resource dir. clangd literally injects `-resource-dir=` to force these; ccls
hard-codes the path at build time and lets you override `clang.resourceDir`. For us, adding
`<resource-dir>/include` to the system roots is the cheap correct move. (Note: `-print-resource-dir`
is a clang flag; for gcc the equivalent internal dir already shows up in the §3a `-E -v` block and
in `-print-search-dirs`.)

### 3d. MSVC (Windows) — the one non-`-E -v` platform

`cl.exe` has no `-E -v` search-dir dump; it reads the **`INCLUDE` environment variable** set by
`vcvarsall.bat` / `vcvars64.bat`. To discover MSVC's system include paths build-free: locate the VS
install (via `vswhere.exe`), run `vcvarsall.bat x64` in a subshell, and **diff the environment** to
capture `INCLUDE` (semicolon-separated system roots) plus predefined defines. clang-cl can also be
probed with `-Xclang -dM -E` but still needs the MSVC `INCLUDE` for the actual headers. This is the
messiest platform; ship Linux/macOS `-E -v`/`-dM` first, add the `vcvarsall`+env-diff path for MSVC
as a follow-up.

Sources: gcc CPP search path / `-v` (`#include <...> search starts here` markers) —
<https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html>; gcc preprocessor options (`-dM`, `-v`) —
<https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html>; predefined-macro dump one-liners
(`gcc -x c /dev/null -dM -E`, `clang -x c /dev/null -dM -E`) —
<https://blog.kowalczyk.info/article/j/guide-to-predefined-macros-in-c-compilers-gcc-clang-msvc-etc..html>;
clangd query-driver runs `‹driver› -E -xc++ -v /dev/null` —
<https://clangd.llvm.org/guides/system-headers>; `-print-resource-dir` to derive the system include
dir — <https://github.com/sdilts/cl-bindgen/issues/4>; macOS `xcrun --show-sdk-path` / SDKROOT —
<https://keith.github.io/xcode-man-pages/xcrun.1.html>; MSVC `vcvarsall.bat` sets `INCLUDE` for
`cl.exe` — <https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line>,
<https://learn.microsoft.com/en-us/cpp/build/reference/cl-environment-variables>.

---

## 4. Heuristic fallbacks (no compiler, no DB)

Ordered by trustworthiness; only reached when §1–3 all fail (e.g. sandbox with no compiler on PATH):

1. **Honor explicit user config even in fallback:** `.clangd` (`CompileFlags: Add: [-I…, -D…]`),
   `compile_flags.txt` (one flag per line, applies to all files), `.ccls`,
   `c_cpp_properties.json` (`includePath`, `defines`, `compilerPath`). These are *cheap to read*
   and represent user intent — check them **before** guessing, and if `compilerPath` is named, that
   *is* the driver to probe in §3. (Reading them ≠ requiring them.)
2. **Walk up for project include roots:** from the open file, the ancestor dirs plus conventional
   `include/`, `src/`, `inc/` siblings — the same ancestor-walk `resolve_include` already does for
   quoted includes, generalized to be `<...>`-eligible for in-project headers.
3. **Known stdlib locations (last resort, platform-templated):**
   - Linux: `/usr/include`, `/usr/local/include`, `/usr/include/<triple>` (multiarch, e.g.
     `x86_64-linux-gnu`), `/usr/include/c++/<ver>`, `/usr/lib/gcc/<triple>/<ver>/include`.
   - macOS: the Xcode SDK `usr/include` + `usr/include/c++/v1` under `xcrun --show-sdk-path` (still
     one cheap `xcrun` call — prefer it over hardcoding).
   - Windows: the MSVC + Windows SDK `INCLUDE` roots if discoverable, else give up gracefully.
   A wrong guess here is genuinely harmful (multilib / sysroot / cross-toolchain → wrong libc), so
   flag heuristic-sourced roots as **low-confidence** and never let them override a probe or a DB.

The discipline (matches `docs/cpp-system-headers.md`): system-header content lands under a distinct
**DEPENDENCY-like role** so it never pollutes project diagnostics or workspace-symbol noise, and is
resolved **lazily per-missing-symbol**, not eagerly indexed at startup.

Sources: clangd `compile_flags.txt` / `.clangd` overlay —
<https://clangd.llvm.org/design/compile-commands>, <https://clangd.llvm.org/config>; baseline gcc
default include dirs on Linux — <https://www.baeldung.com/linux/gcc-default-include-directories>.

---

## 5. Recommendation for THIS engine — ranked layered strategy

Design tenets, from the engine's philosophy (`CLAUDE.md`, `docs/cpp-lsp-experience-research.md`):
**zero-config first**, **build-free always**, **cheap + cached**, **lazy not eager**, **degrade
gracefully** (C stdlib solid; STL templates best-effort), and **never require what clangd requires**.

### The layered pipeline (each layer overrides the ones below it, on a per-file basis)

| Rank | Layer | What it yields | Cost | Trigger |
|---|---|---|---|---|
| **0** | **Explicit user config** — `.clangd`, `compile_flags.txt`, `c_cpp_properties.json`, an `initializationOption` for compile-DB dir / extra flags | project `-I`/`-D`; possibly the driver path | file read | present in tree |
| **1** | **`compile_commands.json`** — walk up from file + check `build/`; parse `arguments`, borrow nearest-TU/sibling-name entry for headers | project `-I`/`-isystem`/`-D`/`-std`/`--target`; **which driver to probe** | file read + JSON parse | DB found |
| **2** | **Compiler probe (the zero-config core)** — of the driver from L1, else a discovered default (`cc`/`c++`, `clang`/`clang++`, or `cl`) | **stdlib system-include roots** (`-E -v`), **predefined macros** (`-dM`), **resource-dir** built-ins | 2–3 process spawns, **once per toolchain**, then cached | always (unless L2 disabled) |
| **3** | **Heuristics** — user-config include hints, ancestor-dir walk, platform-templated known stdlib paths | low-confidence stdlib + project roots | negligible | no compiler & no DB |

**Composition, not exclusivity:** L1 and L2 are *complementary*, not either/or. Even with a perfect
compile DB, the stdlib roots and predefined macros come from L2 (they're implicit in the driver, not
in the DB entry). The normal steady state is **L1 (project flags + driver) + L2 (stdlib + macros),
cached**. L0 overrides specifics; L3 is the no-toolchain safety net.

### Caching — reuse the machinery that already exists

- **Toolchain probe cache.** L2's output (`Vec<system_include_root>`, `HashMap<macro→value>`,
  resource-dir) is **stable per (compiler abs-path, version, `-std`, `--target`, `SDKROOT`)**. Cache
  it under that key, *separate from the per-project module cache* (a libc/toolchain upgrade must
  invalidate it independently — a project edit must not). This mirrors `docs/cpp-system-headers.md`'s
  "cache keys on toolchain identity, not project." The engine already persists a zstd macro blob
  (`PersistedMacros`, `MACRO_CACHE_VERSION` in `cpp_reparse.rs`) — add a sibling toolchain-keyed
  blob for the probe results; feed the `-dM` macros in as the **EXTERNAL seed** the gather already
  accepts (`preprocess_validated` "seeded with EXTERNAL macros").
- **Compile-DB cache.** Parse `compile_commands.json` once, invalidate on its mtime.
- **Lazy system-header parse.** Keep startup free: only when a symbol misses in the project do we
  search the L2/L3 roots, find the declaring header, and parse *that one* (bounded by the existing
  parse-damage/size guards), tagging it the DEPENDENCY-like role.

### Concrete shell-outs to implement, and exactly what to parse

```sh
# L2a  system include roots (run for xc++ and xc; preserve order)
echo | «driver» -xc++ -E -v -            2>&1   # take lines between
                                                # "#include <...> search starts here:" and "End of search list."

# L2b  predefined macros (forward the project's -std / --target from L1)
«driver» -xc++ -dM -E - < /dev/null             # each "#define NAME VALUE" → macro seed

# L2c  compiler-owned built-in headers
clang -print-resource-dir                       # append "<out>/include" to the roots  (clang)
gcc   -print-search-dirs                        # gcc-internal include dir (also appears in L2a)

# macOS SDK sysroot (if driver is Apple clang)
xcrun --show-sdk-path                            # → -isysroot root; honor $SDKROOT

# Windows / MSVC (follow-up platform)
vswhere -latest -property installationPath  → run vcvars64.bat in a subshell → diff env → capture INCLUDE
```

Parsing rules: L2a — literal-marker slice, trim, keep order, dedup. L2b — split each `#define` line
into name + rest. Both gcc and clang share these interfaces, so **one parser** covers Linux + macOS;
MSVC is the separate env-diff path.

### The single highest-payoff first step

**Ship L2 (the compiler probe), cached per toolchain, before anything else.** Rationale:

- It directly unblocks the **existing** `<...>` macro-gather in `cpp_reparse.rs` — today it skips
  system includes purely because it lacks the roots; L2a hands it the roots and L2b hands it the
  predefined-macro seed that makes conditional-compilation in those headers resolve correctly.
- It's the **fallback that makes L0/L1 optional** — with L2 working, the engine is genuinely
  zero-config on any machine that has a compiler on PATH (i.e. any machine that can build C/C++),
  which is the whole differentiator.
- It's **cheap and one-shot** — 2–3 spawns, cached under toolchain identity, amortized to zero.
- It's **portable** — the same `-E -v` / `-dM` parser serves gcc and clang on Linux and macOS.

L1 (consume `compile_commands.json` when present) is the natural **second** step — it sharpens
*project* include/define accuracy for configured builds at near-zero cost — and L3 heuristics are a
thin safety net for the no-compiler sandbox. Do **not** build CMake File API or Makefile parsing
until real usage shows L1+L2 leaving a gap; they're heavier and mostly redundant with the probe.

---

## Sources

- clangd system headers (driver-relative stdlib, `--query-driver` runs `‹driver› -E -xc++ -v /dev/null`, resource-dir, prefer query-driver over hand-listing) — <https://clangd.llvm.org/guides/system-headers>
- clangd compile commands (DB search walks up from file; `compile_flags.txt`; header command borrowing; `.clangd` `CompileFlags: Add/Remove`; fallback `clang foo.cc`) — <https://clangd.llvm.org/design/compile-commands>
- clangd troubleshooting (absolute compiler path; `-v` shows search dirs + target) — <https://clangd.llvm.org/troubleshooting>
- clangd config reference — <https://clangd.llvm.org/config>
- JSON Compilation Database spec (`directory`/`file`/`arguments`/`command`/`output`; `arguments` preferred; `arguments[0]` = executable) — <https://clang.llvm.org/docs/JSONCompilationDatabase.html>
- `CMAKE_EXPORT_COMPILE_COMMANDS` (Makefile/Ninja only, writes to build dir) — <https://cmake.org/cmake/help/latest/variable/CMAKE_EXPORT_COMPILE_COMMANDS.html>
- Bear (intercepts build to emit DB; prefer native export for CMake/Meson/Bazel) — <https://github.com/rizsotto/Bear>
- compile_commands.json recipe gallery (per-build-system) — <https://blog.bkryza.com/posts/compile-commands-json-gallery/>
- gcc CPP search path (`#include <...> search starts here:` / `End of search list` markers; `-v`) — <https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html>
- gcc preprocessor options (`-dM`, `-v`, `-E`) — <https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html>
- gcc directory options — <https://gcc.gnu.org/onlinedocs/gcc/Directory-Options.html>
- predefined-macro dump one-liners (`gcc/clang -x c /dev/null -dM -E`) — <https://blog.kowalczyk.info/article/j/guide-to-predefined-macros-in-c-compilers-gcc-clang-msvc-etc..html>
- gcc default include dirs (Linux baseline) — <https://www.baeldung.com/linux/gcc-default-include-directories>
- `clang -print-resource-dir` → system include dir — <https://github.com/sdilts/cl-bindgen/issues/4>
- macOS `xcrun` (`--show-sdk-path`, `-f`, `SDKROOT`) — <https://keith.github.io/xcode-man-pages/xcrun.1.html>
- MS C/C++ IntelliSense (queries `compilerPath` for system includes + defines; auto-detect quick-pick; `compilerPath: ""` skips) — <https://code.visualstudio.com/docs/cpp/configure-intellisense>, <https://code.visualstudio.com/docs/cpp/customize-cpp-settings>
- ccls (resource dir hard-coded/overridable; `.ccls`; `compilationDatabaseDirectory`; `--gcc-toolchain`) — <https://github.com/MaskRay/ccls/wiki/Customization>, <https://github.com/MaskRay/ccls/wiki/Project-Setup>
- MSVC `vcvarsall.bat` sets `INCLUDE` for `cl.exe` — <https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line>, <https://learn.microsoft.com/en-us/cpp/build/reference/cl-environment-variables>
</content>
</invoke>
