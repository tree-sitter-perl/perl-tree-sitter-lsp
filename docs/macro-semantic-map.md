# C/C++ macro semantic map

Input to the upcoming macro-handling ADR. This documents the **measured**
behavior of goto-definition, hover, and references for C-preprocessor macros
as of the `spike/cpp-support` branch — not the intended design. Cases were
probed against the real perl5 tree (`/home/veesh/personal/perl5`) and the
in-repo `gold-corpus/cpp-fixture/` corpus, driving both the live LSP
(`textDocument/definition` over JSON-RPC) and the CLI mirrors
(`--definition`, `--hover`).

## The three lens macros

| lens | kind | perl5 site | shape |
|---|---|---|---|
| `BASEOP` (the "OP_BASE" of the brief) | object-like, **config-variant** | `op.h:47-65` | `#ifdef BASEOP_DEFINITION` → `#define BASEOP BASEOP_DEFINITION` (op.h:48) **else** a multi-line field-list expansion (op.h:50-64). Used bare in every op struct (`op.h:222`, `:226`, …). |
| `PERL_BITFIELD16` | object-like **type** macro, **config-variant** | `perl.h:4506-4512` | `#ifdef HAS_NON_INT_BITFIELDS` → `U16` (perl.h:4508) **else** `unsigned` (perl.h:4510). Used as a field type in `BASEOP` and `typedef PERL_BITFIELD16 Optype;` (op.h:45). |
| `SvREFCNT_inc` | **function-like** wrapper (direct-delegation) | `sv.h:381` | `#define SvREFCNT_inc(sv) Perl_SvREFCNT_inc(MUTABLE_SV(sv))` — pure forwarding to a real function. |

(There is no `OP_BASE` macro in perl5; the brief's `OP_BASE` is `BASEOP`,
the base-op field-list macro — an object-like config-variant macro.)

## Behavior matrix (measured)

| macro kind | goto-def | hover | references |
|---|---|---|---|
| object-like value (`#define MAX 100`) | **works** — resolves to `#define` | **partial** — shows name as `*variable*`, no body/value | works same-file |
| object-like **type** macro, in-file (`macro_field_type.cpp`) | works | **works** — field hover chases the alias to the leaf (`unsigned short`); GOLD `cpp-hitlist-macro-field-type` | works same-file |
| **config-variant** field type (`cfgvar_macro_field.cpp`) | works (to a variant) | **works** — join over variant arms yields `unsigned short`; GOLD `cpp-hitlist-config-variant-macro-field-type` | works same-file |
| config-variant object-like **used bare** (`BASEOP` at `op.h:222`) | **wrong** — lands on the *usage site itself* (`op.h:222:5`), not the `#define` | **partial** — `*variable*`, no expansion | not verified cross-file |
| object-like type macro **used cross-file** (`PERL_BITFIELD16` at `op.h:45`, defined in `perl.h`) | **missing** — "No definition found" (even with 247 pack files + perl.h indexed) | **missing** at the raw usage; **works** when the macro is a resolved *field type* | missing cross-file |
| **function-like wrapper** (`SvREFCNT_inc` at `sv.c:472`, def in `sv.h`) | **missing** — "No definition found"; no see-through to `Perl_SvREFCNT_inc` | not surfaced | missing |
| cross-file macro **type-leaf** chain (`op.c` op_type → PERL_BITFIELD16 → U16 → U16TYPE → unsigned short) | n/a (hover) | **now XPASSes** — fixture `cpp-hitlist-crossfile-macro-member-field-type` is marked `xfail` but the harness resolves `op_type unsigned short` (see Surprises) | — |

### Evidence (exact probes)

- `perl-lsp --definition perl5 op.h 221 4` → `op.h:222:5` — **BASEOP usage resolves to itself**, not the `#define` at `op.h:50`. `--lang-analyze` confirms the only `BASEOP` symbols are the two `#define` arms (`op.h:48`, `op.h:50`); the bare use at line 222 is captured as a ref that resolves to its own span, and because `find_definition` returns `Some`, the `pack_xfile_word_at` fallback (which *does* prefer the `#define` line, `backend.rs:437-457`) never runs.
- `perl-lsp --definition perl5 op.h 44 8` (PERL_BITFIELD16) → **"No definition found"**. `pack_xfile_word_at` reaches `idx.get_cached("PERL_BITFIELD16")` and misses: object-like/type macros defined in another file are not registered by name in the pack cross-file index the way classes/functions are (`module_index.rs:register_symbols`).
- `perl-lsp --definition perl5 sv.c 471 4` (SvREFCNT_inc) → **"No definition found"** — function-like wrapper is neither resolved to its `#define` nor seen-through to the delegated `Perl_SvREFCNT_inc`.
- `perl-lsp --hover perl5 op.h 221 4` (BASEOP) → renders only `*variable*` — no body, no expansion, no value.

## How the model represents macros today

- **Collection** — `cpp_reparse.rs::collect_macro_variants` walks every
  `#define`, capturing body, params (`Some(..)` ⇒ function-like), def line,
  and the **guard trail** (enclosing `#if/#ifdef/#else`, outermost-first).
  Multiple `#ifdef`-gated definitions of one name accumulate as
  `MacroVariants` (`cpp_macro_model.rs:46-64`).
- **Config-variant model** (`cpp_macro_model.rs`) — landed. Each variant gets
  a tri-valued `Reachability` (`Active` / `Unknown{guard}` / `Unreachable{reason}`)
  from a Kleene evaluation of its guards against a `KnownConfig`
  (`defined` set ∪ `universe` of all knobs seen in the closure). Nothing is
  pruned; variants are ranked (Active → Unknown → Unreachable). `join_type`
  folds the variant bodies into one abstraction type (integer family widens
  to `Numeric`; divergent types → `None`). This is what makes the
  config-variant *field-type* hover land on `unsigned short`.
- **TypeName edges** (`query_extract.rs`, `emit_external_type_aliases`) —
  landed. An object-like type macro (`#define PERL_BITFIELD16 U16`) mints a
  `TypeName(name) → Edge(TypeName(underlying))` / `InferredType(..)` witness,
  so a field *typed* by the macro chases the alias to its leaf. External
  type-alias macros gathered from the `#include` closure ride in as span-less
  `TypeName` witnesses (this is how a gitignored generated header's alias
  still reaches the field).
- **Expansion policy** — currently **expand-and-reparse**
  (`cpp_reparse.rs::preprocess_validated`): macro *bodies* are pre-expanded to
  a fixpoint (depth cap 8) and *usages* are spliced inline; a `SpliceMap`
  remaps expanded spans back to original coordinates so refs/hover land on the
  call site. Declaration-generating macros (`DECLARE_DYNAMIC(cls)` → members)
  and X-macros are explicitly out of scope.

## Why the working cases work and the broken ones don't

The model is strong exactly where a macro participates as a **type** in the
type lattice: field-type hover routes through the witness bag (TypeName edges,
config-variant join), so `unsigned short` falls out end-to-end, including the
cross-file leaf chain. The model is absent exactly where the macro is the
**navigation target itself** — a bare object-like use (`BASEOP`), a cross-file
object-like/type macro (`PERL_BITFIELD16`), or a function-like wrapper
(`SvREFCNT_inc`). Those go through the generic symbol/ref goto-def, which has
no macro-aware step: the `MacroVariants` reachability ranking is computed but
never consulted by `find_definition`, and there is no macro entry in the pack
cross-file index, so cross-file and see-through navigation simply miss.

## Gaps → which redesign slice closes it

The branch is moving toward **parse-repair-only** expansion: model
type/expression/function-like macros as first-class symbols/edges instead of
splicing them away. Mapping each measured gap to that direction:

1. **BASEOP use → itself, not the `#define`.** The bare use is captured as a
   ref that resolves to its own span, shadowing the `#define`-preferring
   `pack_xfile_word_at` fallback. → *First-class object-like macro symbol*: the
   use resolves to the macro symbol, and multi-def presentation (below) ranks
   the arms. Closes goto-def for same-file object-like macros.

2. **Multi-def / config-variant goto-def picks arbitrarily** (the reported
   "win32.h-wins"). `MacroVariants` already ranks Active > Unknown >
   Unreachable but goto-def ignores it. → *Wire ranked variants into
   `GotoDefinitionResponse::Array`*, Active arm first. Closes multi-location
   presentation.

3. **PERL_BITFIELD16 cross-file goto-def missing.** No by-name pack-index entry
   for object-like/type macros. → *Register macros as global symbols in the
   pack cross-file index* (the same slice that makes function-like macros
   "global functions"). Closes cross-file macro goto-def/hover at the raw use.

4. **SvREFCNT_inc wrapper — no resolution, no see-through.** → *Function-like
   macros = global functions with implied return typing* (reuse the sub-return
   bag path), and *direct-delegation wrappers carry a value-witness* to the
   delegated callee (`Perl_SvREFCNT_inc`) for see-through nav. Closes
   function-like goto-def + see-through.

5. **Config-variant as return arms.** The join already works for field types;
   the direction generalizes it — *config-variant macro = return arms unioned
   by the existing arm-fold* — so the same abstraction covers value macros and
   function-like returns, not just field types.

Working today (keep as the model's spine): object-like **type**-macro field
hover, config-variant field-type **join**, cross-file type-**leaf** chain
(currently an XPASS), attribute-macro class recovery.
