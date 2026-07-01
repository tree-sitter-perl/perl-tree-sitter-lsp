# ADR: Semantic handling of C/C++ macros

Status: accepted (incremental). Measured baseline: `docs/macro-semantic-map.md`.

## Context

Macros are pervasive in C / perl5 and carry real program meaning, but a macro
is not one thing. The gather historically **expanded every harmless macro**
(keep the rewrite if `parse_damage` doesn't increase), which (a) destroys the
macro's identity for hover/goto-def, (b) generates a splice per use — bloating
the no-copy `SpliceMap` and its span-remap bugs, and (c) still leaves macro
goto-def broadly broken (a use resolves to itself; cross-file `#define`s aren't
registered by name; the reachability ranking is computed but never consulted).

The type lane already works (object-like **type** macros → `TypeName` edges;
config-variant → variant capture + reachability + join, `cpp_macro_model.rs`).
Everything else — identity, navigation, function-like typing — does not.

## Decision

Treat a macro as a **named entity with (optional) params and a body**, and
lane it by what the body *is*. Model, don't expand, wherever the body is
already valid syntax.

### Lanes

| kind | body | treatment |
|---|---|---|
| object-like, type-valued | `#define X U16` | `TypeName(X)` alias edge (landed) |
| object-like, value | `#define MAX 100` | typed constant (infer type from body) |
| object-like, marker | `#define FLAG` | flag symbol — refs/goto, no type |
| function-like, expression | `#define MAX(a,b) …` | **global sub, implied return typing** |
| function-like, delegation | `#define F(x) G(x)` | as above + **see-through value-witness** |
| **member-block** | `#define BASEOP … op_type:9; …` (used in a struct body) | **role: a `package_parents` edge; blank the use** |
| syntactic / statement / `##` | `do {…} while(0)` | **expand** (parse repair only) |

### The classifier: usage position, not body-parse

A macro body is an ambiguous token blob; its grammatical ROLE is fixed by
**where it is used**, not by parsing the body in isolation. Use position is the
classifier; the body-parse only supplies contents *once the role is known*:

| use position | role |
|---|---|
| type position (`FOO x;`) | type macro → `TypeName` |
| expression / call `FOO(args)` | value / function-like → sub-return |
| standalone in a struct/class body (`FOO;`) | **member-block → role** |
| statement position | syntactic → expand |

Usage-driven and lazy: a never-used macro is just a navigable macro symbol
until a use classifies it. Parked zero-usage fallback — *test-parse the body in
each context, take the one that parses clean* — a real but ambiguous signal,
heuristic not primary.

### The load-bearing move: three expansion modes, keyed by role

The gather no longer "expands if harmless." Each use is **leave / blank /
expand** by its classified role:

- **leave** — parses clean unexpanded (type → `type_identifier`, function-like →
  `call_expression`). Zero splices; the macro is a first-class symbol/edge, and
  a function-like macro *is* a package-global sub typed from its body.
- **blank** — inline tokens would parse as a phantom, but the meaning is an edge
  (member-block → role). Replace the use with whitespace so it parses clean
  (`struct op { BASEOP };` → `struct op { };`); one splice; members come from
  the role. **Blank is only in the transformed/parse view — the original source
  keeps the token, so goto-def-on-`BASEOP` identity is untouched.**
- **expand** — syntactic/statement macros that must be present for a clean parse
  (`parse_damage` strictly decreases).

The splice count collapses to blank+repair sites only; most `SpliceMap`
span-remap bugs evaporate structurally. (`alias_only` was the baby step.)

### Member-block macros = roles (the copypasta = inheritance)

`#define BASEOP …fields…` pasted into every op struct is a **role** — the shape
of a Perl `with`, already modeled as a `package_parents` edge. Mint **one**
visible synthetic base per macro (a navigable, reusable symbol), members parsed
from the body under the field-block role, taken from the **config-active
variant** (reachability reuse). Add a parent edge per pasting struct
(`struct op → BASEOP`). The *existing* ancestor walk then delivers everything —
`o->op_type` resolution, completion, hover, and the **references splat**: every
`->op_type` across every pasting struct converges on the one `BASEOP::op_type`
def (op.h:55), which is both the copypasta anchor and the inheritance anchor.
Roles all the way (rule #10) — even a one-member macro is a role, never a
special-cased inline.

**Edges handle the re-sourcing.** op_type's typing moves from expanded-field to
role-member, but because the model is edges, the role member simply emits the
same `TypeName` edge the expanded field did; the registry chase is agnostic to
the emission site, so slice 2's hover leaf and the type chase keep working
unchanged. The emission site is fungible; the edge is canonical.

### Config-variant = superposition, join over the flowing dimension

A config-variant macro is N `#if` variants = **return arms** of a superposition.
The existing `SymbolReturnArmFold` unions them ("arms agree → the type, disagree
→ widen"). The *flowing dimension* is the return type today; when effects are
modeled they are a second dimension with their own fold — same invariant, "flow
over the blob, union what flows." The blob is never collapsed; the reducer
unions per query.

### Typing vs. display: abstraction for one, concrete leaf for the other

The type that *flows* is the join **abstraction** (an integer). Hover recovers
the **concrete leaf via provenance** — the terminal spelling of the
**reachability-ranked** (config-active) variant (`PERL_BITFIELD16 → U16 →
unsigned short`). Abstraction for inference, concrete leaf for the human.

### Goto-def / navigation

- **Register** object-like / type / function-like macros as **named cross-file
  symbols** in the pack index (fixes `PERL_BITFIELD16` / `SvREFCNT_inc`
  cross-file "No definition found").
- **Prefer the `#define`** over the use's self-span (fixes bare `BASEOP`
  resolving to itself).
- **Multi-location, reachability-ranked** — return **all** def sites (never
  prune: portability), ranked config-active first, others kept + labeled
  ("unreachable: WIN32 undefined"). This finally *consumes* the `MacroVariants`
  ranking (fixes win32-wins).
- **See-through** — a direct-delegation wrapper's goto-def reaches the delegate
  (`SvREFCNT_inc → Perl_SvREFCNT_inc`) via a delegation value-witness.

### Resolution visibility = the include-closure lie (generalizes beyond macros)

C linkage is globally flat, so today every symbol registers into one global
namespace — and two unrelated translation units that both declare `class Box`
(fixtures, vendored deps, multiple independent binaries, generated variants)
**collide**. The determinism fix made the collision *stable* (order-independent
winner) but not *correct*: a file can still resolve a name to a same-named
symbol it can't actually see. This surfaced as the autoret flakiness — the real
bug was "cross-file resolution of two same-named classes is arbitrary."

The fiction that fixes it: **scope cross-file *resolution* by include-
reachability.** A file resolves names only against symbols reachable from *its*
include closure — `autoret.cpp` sees *its* `Box`, `methodchain.cpp` sees *its*.
It's a lie (linkage is global) but it's the visibility the programmer reasons
with ("what's in scope *here*"), and it's the **same lie the gather already
tells for macros** (a file's macros = its include closure). Making the symbol
model tell it too is consistency, not novelty.

Shape: keep the **global registry** (find-anywhere, portability) and add a
**reachability filter/rank** on resolution — identical to the macro multi-def
ranking above (all defs kept, ranked by config-reachability). It reuses that
machinery (`cpp_macro_model::classify` + the per-file include closure the
gather already computes). Symbol resolution and macro resolution converge on
one pattern: **global set + reachability scope.**

## Parked (we'll get there — correctness on a solid base, deepen as needed)

- **Parametric return** (`#define ID(x) (x)` → the arg's type) — arity/union tier.
- **Effects** — the second superposition dimension.

## Consequences

- Determinism precondition: the join-vs-chase winner must be a **principled,
  deterministic** rule, not witness/iteration order (owned by the flakiness
  fix). The function-like *return-typing* slice waits on that verdict.
- Slice order (independent slices first; the coupled pair last):
  1. **goto-def overhaul** — **LANDED** (959b388).
  2. **provenance-leaf hover display** — **LANDED** (a6d8223). op_type hovers
     the concrete leaf; typing stays the abstraction.
  3. **member-block macros = roles** (the field-splat / copypasta-as-
     inheritance) — classify member-block macros by struct-body usage, **blank**
     the use (introduces the blank mode), mint the visible synthetic base +
     parent edges, members from the config-active variant. Fixes `BASEOP`
     field refs-splat / hover / goto-def. Independent, high user-pain value.
  4. **resolution visibility = include-closure scope** — the lie above; reuses
     slice 1's reachability machinery. Correctness for vendored/monorepo name
     collisions. Independent of the expansion policy.
  5. **function-like implied return typing + full expansion policy flip** —
     COUPLED: the "unexpanded macro parses as a `call_expression` → sub-return
     bag path" mechanism only exists once expansion is parse-repair-only (the
     full leave/blank/expand generalization of slice 3's blank). Biggest blast
     radius, last, after the splice/gather work stabilizes.
