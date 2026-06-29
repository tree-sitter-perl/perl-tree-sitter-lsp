# MISRA / embedded-C scout — QA findings

Scouting the `spike/cpp-support` C++ driver against MISRA-flavored embedded
C/C++ codebases. Every gap below is reduced to a minimal repro and verified
with an actual `target/release/perl-lsp --outline` / `--completion` run
(`cargo build --release --features cpp`).

## Codebases swept

| Codebase | domain | `.c` | `.h` | `.cpp/.cc` | sampled |
|---|---|---|---|---|---|
| FreeRTOS-Kernel | RTOS, C | 284 | 371 | 0 | 60 |
| libopencm3 | embedded HAL, C | 272 | 753 | 0 | 60 |
| Mbed-TLS | safety-critical crypto, C | 67 | 64 | 0 | 55 |
| stm32f4xx_hal_driver | vendor HAL, C | 96 | 102 | 0 | 60 |
| cppcheck | static analyzer + MISRA addon corpus | 86 | 167 | 277 | 50 |
| Apollo (sampled) | autonomous-driving, C++ | 0 | 33 | 41 | 30 |

The embedded-C codebases are overwhelmingly `.c`-implementation: FreeRTOS and
libopencm3 ship **zero** `.cpp`. cppcheck's MISRA test corpus
(`addons/test/misra/*.c`, `test/cfg/*.c` — incl. the 567-decl `misra-test.c`)
is all `.c`.

## Bucket counts (CRASH / EMPTY-with-decls / OK)

`.c` files (routing probe):

| Codebase | n | OK | EMPTY(>3 decls) | CRASH |
|---|---|---|---|---|
| FreeRTOS | 30 | **0** | 24 | 0 |
| libopencm3 | 30 | **0** | 21 | 0 |
| Mbed-TLS | 30 | **0** | 25 | 0 |
| stm32hal | 30 | **0** | 24 | 1\* |
| cppcheck | 20 | **0** | 11 | 0 |

\* not a parser crash — `stm32f4xx_hal_timebase_rtc_wakeup_template.c` is
non-UTF-8; the CLI exits 1 on `read_to_string` failure (cosmetic).

`.h` / `.cpp` files (cpp-routed):

| Codebase | ext | n | OK | EMPTY(>3 decls) |
|---|---|---|---|---|
| FreeRTOS | .h | 30 | 17 | 5 (macro-only portmacro/secure headers) |
| libopencm3 | .h | 30 | 10 | 0 (rest are register-`#define`-only) |
| stm32hal | .h | 30 | 30 | 0 |
| Mbed-TLS | .h | 25 | 15 | 0 |
| cppcheck | .cpp | 30 | 29 | 0 |
| Apollo | .cc/.cpp | 30 | 29 | 0 |

No analysis crashes anywhere. Headers and `.cpp` route correctly; the EMPTY
header cases are genuinely macro-only files.

## The `.c`-routing verdict — P0, the dominant gap

**Pure C `.c` files are not analyzed at all.** Driver selection is by
extension (`LanguageRegistry::for_path`); the cpp driver claims
`cpp/cc/cxx/hpp/hh/h` (`src/language_driver.rs:156`) — **no `c`**. A `.c`
file matches no pack driver and falls through to the **Perl** builder
(`main.rs::parse_file` → `builder::build`), which produces `[]`.

Verified — identical content, three extensions:

```c
typedef struct { uint32_t count; uint8_t flags; } Widget;
static inline uint32_t widget_sum(const Widget *w) { return w->count + w->flags; }
int main(void) { Widget x = {0}; return widget_sum(&x); }
```

- `min.c`   → `[]`
- `min.h`   → 4 symbols (count, flags, widget_sum, main)
- `min.cpp` → 4 symbols (identical)

C support is a **research spike, not a wired driver**: `tree-sitter-c` is a
**dev-dependency only** (`Cargo.toml:66`), feeding `c_reparse.rs` /
`c_superpose.rs` (the `a * b;` declaration-vs-multiply ambiguity probe).
There is no `c` feature, no `c_driver()`, no `queries/c/`, no `.c` extension.
tree-sitter-cpp parses C as a superset, so copying any `.c` to `.cpp`
extracts fine (modulo the content gaps below) — the routing is the wall, and
the `a*b` ambiguity (already spiked) is the one real C-vs-C++ snag a wired C
driver must clear.

Impact: 100% of FreeRTOS/libopencm3/mbedTLS/stm32-HAL implementation, and the
entire cppcheck MISRA `.c` test corpus, return empty outlines today.

## Content gaps that persist even when routed (`.cpp`/`.h`)

These hit C *and* C++ but bite hardest in embedded C. Root cause for all is
missing query patterns in `queries/cpp/skeleton.scm`.

### G1 — `typedef struct {..} Name;` loses the typedef alias (P0)

The single most common C type idiom. The skeleton has **no `type_definition`
pattern**, and `struct_specifier` requires `name:` (skeleton.scm:31), so an
anonymous struct never matches and the alias is never captured.

```c
typedef struct { int x; int y; } Point;           // Point  -> DROPPED; x,y orphaned as bare Variables
typedef struct Node { int val; } Node;            // Node   -> Class (tag kept); alias coincides so survives
struct Bare { int z; };                           // Bare   -> Class (works)
```

`--outline` yields: `x`, `y` (bare `Variable`, **no package**), `Node`,
`Bare`. `Point` is gone; its fields are orphaned.

Real-world, `stm32f4xx_hal_uart.h`: 4 public `*TypeDef` aliases in source,
**only 1 `Class` symbol** extracted. The named-tag form
`typedef struct __UART_HandleTypeDef {..} UART_HandleTypeDef;` keeps the
internal tag `__UART_HandleTypeDef` but drops the public alias
`UART_HandleTypeDef` — i.e. the engine indexes the name developers *never*
write and drops the one they always do. The anonymous form
`typedef struct {..} UART_InitTypeDef;` drops the alias entirely.

### G2 — member completion on function parameters fails (P0)

The type-witness patterns (skeleton.scm:129-160) match only `(declaration …)`
— locals. There is **no `parameter_declaration` pattern**, so a function
parameter gets no type binding and `param.` / `param->` cannot scope.

```c
struct Alpha { int aa; int ab; };
void g(Alpha *a) { a-> }     // completion: Alpha, Beta, g   (flat dump — NOT aa,ab)
void g(Alpha  a) { a.  }     // completion: Alpha, g         (flat dump)
void g(Alpha &a) { a.  }     // completion: Alpha, g         (flat dump)
```

Contrast — **locals work**:

```c
void g() { Alpha *a = &av; a-> }   // completion: aa, ab   (correct)
void g() { Alpha a;        a.  }   // completion: aa, ab   (correct)
```

Value, reference, *and* pointer parameters all fail; only local declarations
are typed. In embedded C the dominant completion site is exactly the failing
one: `void HAL_X(UART_HandleTypeDef *huart){ huart->... }`.

### G3 — `typedef enum`/enum constants dropped (P1)

`enum_specifier name:` is captured (skeleton.scm:53) but there is **no
`enumerator` capture** and no `type_definition`, so enum *constants* are never
symbols and `typedef enum` aliases vanish.

```c
typedef enum { RED, GREEN, BLUE } Color;   // Color -> DROPPED; RED/GREEN/BLUE never extracted
enum Direction { NORTH, SOUTH };           // Direction -> Class; NORTH/SOUTH DROPPED
enum Color { RED = 1, GREEN, BLUE };       // Color -> Class; RED/GREEN/BLUE DROPPED
```

MISRA style prefers named enum constants over magic numbers, so these are
exactly the symbols a reviewer wants to navigate. None are indexed.

### G4 — function-pointer & scalar typedefs invisible (P1)

No `type_definition` pattern → these produce a completely empty outline:

```c
typedef void (*Callback)(int code);              // -> []
typedef int  (*Comparator)(const void*, const void*);  // -> []
typedef uint32_t MyInt;                          // -> []
```

Function-pointer typedefs (callback/ISR/vtable tables) and fixed-width scalar
aliases are pervasive in embedded/MISRA code; all are unnavigable.

### G5 — declaration-generating macros (P2-3, deep)

```c
#define STATES(X) X(IDLE) X(RUN)
enum { STATES(MK) };       // -> []   (X-macro enum)
int add(a,b) int a; int b; { return a+b; }   // -> []   (K&R; MISRA bans K&R, low priority)
```

X-macros need the macro *expanded* before extraction — out of scope by
construction (skeleton.scm header; `cpp_obstacle.rs` measures this tier).
K&R is forbidden by MISRA, so low value.

## Ranking (by frequency in real embedded/MISRA C)

1. **`.c` not routed** — every implementation file. Feature, not xfail: wire a
   C driver (the `c_reparse` spike is the hard part; the rest is registering
   `exts:["c"]` + a `queries/c/skeleton.scm`, likely a near-copy of cpp's).
2. **G1 typedef-struct alias dropped** — every HAL/embedded type.
3. **G2 param-receiver completion** — the dominant completion site in C.
4. **G3 typedef-enum alias + enum constants** — MISRA named-constant style.
5. **G4 function-pointer / scalar typedefs** — callback tables, fixed-width aliases.
6. **G5 X-macro / K&R** — common but deep / MISRA-banned.

## Recommendations: gold xfail vs deep

**Worth gold xfail rows now** (deterministic, clearly-correct expected output,
fixable by bounded `queries/cpp/skeleton.scm` additions — they XPASS the
moment the pattern lands):

- G1: `(type_definition declarator:(type_identifier) @def.class.name …)` +
  let the body `struct_specifier` carry `@context.class` via the alias. Cover
  both anonymous and named-tag forms; the alias should be the primary symbol.
- G3: add `(enumerator name:(identifier) @def.const.name)` and a
  `type_definition`-over-`enum_specifier` alias pattern.
- G4: the same `type_definition` pattern captures function-pointer and scalar
  typedef aliases (a `type_identifier` declarator).
- G2: add a `parameter_declaration type:_ declarator:…` type-witness pattern
  (pointer/reference/value declarator shapes), mirroring the local-`declaration`
  patterns — a self-contained addition, biggest practical payoff for C.

**Deep / out of scope:**

- G5 X-macro enum expansion — a declaration-generating macro; needs the
  preprocessing tier, explicitly deferred by the skeleton's design note.
- `.c` driver itself — the `a * b;` ambiguity (`c_reparse.rs`) is genuine
  research; the registry wiring is trivial but the correctness work is not.

**Cosmetic:** non-UTF-8 source makes the CLI exit 1 (`parse_file` →
`read_to_string`). Lossy-read or a clear diagnostic would avoid a misleading
"crash" in batch sweeps.
