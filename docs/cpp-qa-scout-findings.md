# C++ QA scout — next classes of gaps after the namespace-macro

Sweep of `perl-lsp --outline` (built `--features cpp`) across six diverse real
C++ projects, hunting for the *next* gap classes after the already-known
cross-file namespace-wrapping-macro idiom (`SPDLOG_NAMESPACE_BEGIN`, codified as
`cpp-xfail-cross-file-namespace-macro`).

Method: shallow-clone, sample ~20–80 headers/sources per project, bucket each
file (CRASH / EMPTY / STRUCT-CORRUPT / OK), then reduce every interesting file
to a minimal repro and verify with an actual `--outline` run. All crash probes
were run under `ulimit -v 2.5G` so a runaway allocation aborts fast instead of
taking down the box.

## Projects & bucket counts

| Project | stresses | sampled | OK | trivial | EMPTY | STRUCT-CORRUPT | CRASH |
|---|---|---|---|---|---|---|---|
| nlohmann/json | header-only templates / SFINAE | 47 | 37 | 5 | 1 | 4 | 0 |
| fmt | templates + some C++20, `.h`/`.cc` split | 19 | 17 | 2 | 0 | 0* | 0 |
| leveldb | normal `.h`/`.cc` separation, DB domain | 80 | 77 | 1 | 0 | 2 | 0 |
| Dear ImGui | heavy macro / UI-game | 53 | 50 | 1 | 1 | 0 | **1** |
| Godot core | `GDCLASS` declaration-macros, engine | 80 | 78 | 2 | 0 | 0 | 0 |
| range-v3 | concepts / ranges / modern | 80 | 67 | 13 | 0 | 0 | 0 |

\* fmt's coarse bucket is "OK" but its `.cc` files hit gap #2 below — the
heuristic only flags *zero-Class* files, not mis-parented methods.

Notes that confirm prior work, not new gaps:
- **json's 4 STRUCT-CORRUPT files are all the known namespace-macro gap** —
  every json header opens with `NLOHMANN_JSON_NAMESPACE_BEGIN` (`#define`d in
  `detail/abi_macros.hpp`). Same root as spdlog. json is a *good stress of how
  pervasive* that idiom is (it decapitates the class AND strips the package off
  the leaked methods), but it is not a new class.
- **Godot's `GDCLASS(Name, Base);` macro does not corrupt** — it sits as a
  statement inside the class body, so the class and its 290 real methods
  extract fine (`core_bind.h`: 26 Class / 290 Method). Declaration-macros in
  *body* position are safe; only *declarator*-position macros bite. The
  `GDCLASS`-synthesized members (`_get`/`_set`/...) are missing, but that's a
  deep feature, not corruption.

---

## NEW gaps, ranked by how often they hit real code

### 1. CRASH — self-referential macro → exponential expansion → OOM  (HARD FAIL)

**Severity: critical (crash / 28 GB OOM). Frequency: moderate.**

A single 380-line file — `imgui/backends/imgui_impl_opengl2.cpp` — drives the
process to **28 GB RSS and a SIGKILL after 18 s**. Minimal repro is one line:

```cpp
#define M x  // M M
```

`--outline` on that line allocates without bound (aborts at the 2.5 G ulimit).
Boundary, all verified:

| input | result |
|---|---|
| `#define M x  // M`   (name once in comment) | OK |
| `#define M x  // M M` (name twice in comment) | **OOM** |
| `#define M x  /* M M */` (block comment) | OK |
| `#define M y M`  (name once in body) | OK |
| `#define M M M`  (name twice in body) | **OOM** |
| `#define M(a) a // M M` (function-like) | OK |

Real-world trigger in imgui (verbatim):
```cpp
#define APIENTRY __stdcall   // It is customary to use APIENTRY ... needs APIENTRY.
```
The trailing comment mentions `APIENTRY` twice — boom.

**Root cause** (`src/cpp_reparse.rs`): two compounding bugs in the same-file
`#define` pre-expander.
- `clean_body` (line 145) does **not strip the trailing `//` line comment** —
  tree-sitter-cpp's `preproc_arg` captures everything to EOL, so the comment
  text `// M M` becomes part of the macro replacement body.
- `pre_expand_bodies` / `expand_text` (lines 155–202) have **no C "blue paint"
  rule** — a macro whose body contains its own name is re-expanded into itself.
  Because the name appears ≥2× per pass, each of the 8 capped iterations
  super-exponentially multiplies the body (`#define M M M`-style is enough; the
  comment bug just smuggles extra self-references in from innocent comments).

Either fix kills the imgui case; the blue-paint guard is the correctness fix
(real recursive object-macros `#define X (X+1)` are legal and common). Stripping
`//`/`/* */` from `clean_body` is independently correct (C strips comments
before tokenizing the body).

### 2. Out-of-line method definitions are mis-parented to the namespace  (highest frequency)

**Severity: medium (wrong nesting, breaks outline grouping + container-scoped
nav). Frequency: very high — every `.cpp` with separated definitions.**

```cpp
namespace ns {
int Widget::area() const { return w_ * h_; }
Widget::Widget(int w) : w_(w) {}
void Widget::resize(int x) { w_ = x; }
}
```
Extracts:
```
Package ns
Method area   pkg=ns      <- should be pkg=Widget
Method Widget pkg=ns      <- ctor, should be pkg=Widget
Method resize pkg=ns      <- should be pkg=Widget
```
The `Widget::` qualifier in the function declarator is **dropped entirely** —
the method is attributed to the enclosing namespace (or to `None` when at file
scope, as in `fmt/src/os.cc`: `buffered_file::close` → `Method close pkg=None`).
Confirmed in leveldb (`table/block.cc`, `table/table.cc`) and fmt
(`src/os.cc` — 22 out-of-line defs, almost all un-parented).

This is the single most common real-world shape (header declares, source
defines) and the heuristic sweep *under*-counts it because the file still has a
namespace Package and non-zero methods.

A sub-case of the same root: a **qualified-name nested class definition**
`class Block::Iter : public Iterator { ... }` is extracted as `Sub Iter`
(wrong kind, qualifier flattened) instead of `Class Block::Iter`.

### 3. Template member functions are classified `Sub`, not `Method`

**Severity: low (wrong kind/icon; nesting + package are correct). Frequency:
high in header-only template libraries.**

```cpp
template<typename T, typename>
struct Ser {
  template<typename B, typename U = T>
  static U from(B& j) { return U(); }   // -> Sub 'from'  (should be Method)
  template<typename B>
  static void to(B& j, T v) {}          // -> Sub 'to'    (should be Method)
};
```
The class itself extracts fine (`Class Ser`) and the members get
`package=Ser`, but the `template<...>` clause wrapping each member function
breaks the "is this a member?" classification, so they come out as free
functions (`Sub`). Pervasive in json / fmt / range-v3 (every templated method).

### 4. `concept` declarations are not extracted at all

**Severity: low–medium (missing symbol). Frequency: low–moderate (C++20; many
libs hide it behind their own macro).**

```cpp
template<typename T>
concept Addable = requires(T a, T b) { a + b; };
```
→ no symbol emitted. A named `concept` is a top-level entity and should appear
in the outline (like a type alias). Note: range-v3 / fmt mostly use their own
`CPP_concept` macro rather than raw `concept`, so raw-`concept` headers are rare
*in these libs* (2 in range-v3, 1 in fmt) — but any modern C++20 codebase using
plain concepts loses them. All *other* modern idioms probed extract correctly:
`requires`-clauses, constrained member functions (`... requires C<T>`),
abbreviated templates (`auto f(auto a)`), structured bindings, `consteval` /
`constinit`.

---

## Recommendations

**Fix now (crash — a crash is always a hard fail):**
- **Gap #1** is the priority. Add the blue-paint no-self-reexpansion guard to
  `expand_text`/`pre_expand_bodies`, and strip `//` + `/* */` comments in
  `clean_body`. Ship a gold **xfail-crash** fixture from the one-liner
  `#define M x  // M M` (→ XPASS when guarded). This is the only crash found
  across ~360 sampled real files, so the parse-robustness story is otherwise
  excellent.

**Worth a gold xfail fixture (cheap, high-signal regression rows):**
- **Gap #2** (out-of-line `Class::method` → wrong package). Highest real-world
  payoff. Self-contained repro, exact-assertion: assert `package=Widget` on
  `area`. Promote on fix. Likely a tractable skeleton-query fix
  (handle a `qualified_identifier` declarator name → split off the class
  qualifier as the container).
- **Gap #3** (template member → `Sub`). One-file fixture asserting `kind=Method`
  on a `template<...>` member. Also likely a query tweak (match
  `template_declaration > function_definition` inside `field_declaration_list`
  as a method).

**Defer / deeper feature (lower frequency or larger lift):**
- **Gap #4** (concepts). New symbol kind + query arm; low frequency in the
  surveyed libs. Track as a known gap, fixture optional.
- Nested qualified-class (`class A::B`) — fold into the gap #2 fix (qualified
  declarator handling) rather than a separate feature.

**Skip (confirmed not-a-new-gap):** GDCLASS / body-position declaration macros
(extract fine); the namespace-wrapping macro (already codified).
