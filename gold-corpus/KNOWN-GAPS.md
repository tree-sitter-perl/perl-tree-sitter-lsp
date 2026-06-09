# Gold-corpus known gaps (xfail rows)

The harness pins each gap as an `xfail` fixture row: the assertion is the
**correct** expected behavior, marked as currently *not holding*. When a gap is
fixed the row flips to **XPASS** and the harness fails until you promote it to
`gold` — so a fix can't land silently and a gap can't rot. This file is the
prose write-up of the 7 open xfails (the README's "Known failing" list is the
one-line index). Verify any one with:

```sh
gold-corpus/run.pl --emit <capability> <file> <line> <col>   # 0-based line/col
```

All seven are in distinct subsystems — none are ref-graph (those are all closed).

---

## 1. Type-system / runtime codegen

### `def-16-codegen-type-function` — goto-def on a Type::Library-minted type
- **Capability:** definition · **Cursor:** `Type/Tiny.pm:414:56` (a use of `Any`)
- **Expect:** resolves to the type's *declaration*, `Standard.pm:215` (`name => "Any"`).
- **Actual:** degrades to the package decl `Standard.pm:1:1` (`expect.none`).
- **Root cause:** `Types::Standard::Any` has no literal `sub Any`. `Type::Library`
  mints a sub per type **at runtime** from an `add_type(name => "Any", ...)` table
  entry. We resolve the call to the package but there's no static symbol at the
  declaration to land on, so goto-def falls back to the package top.
- **Fix sketch:** a Type::Library-aware synthesis (plugin or core framework pass)
  that reads `add_type`/`declare` tables and synthesizes a Sub symbol per type at
  the `name =>` span — the same shape as the Moo `has`/DBIC `add_columns`
  accessor synthesis. Medium: needs the table-walk + the type-name → decl-span map.
- **Difficulty:** medium. Framework-specific synthesis; well-trodden pattern.

---

## 2. Dynamic-install recognition (diagnostics false-positives)

These three are unresolved-X diagnostics fired on subs/methods that *do* exist
but are installed by a mechanism we don't yet treat as a definition. The
assertion is "no diagnostic at this site."

### `diag-08` — XS `bootstrap` flagged unresolved-function
- **Cursor:** `Net/SSLeay.pm:1023:1` · **Expect.none:** a diagnostic at `SSLeay.pm:1023`.
- **Construct:** `bootstrap Net::SSLeay $VERSION;` — the XS loader. `bootstrap` is
  a builtin-ish loader call; the subs it installs are XS (no Perl body).
- **Root cause:** `bootstrap` isn't in our resolved set, so the call flags
  unresolved-function. (Separately, real XS-installed subs have no Perl def — the
  reference NOT-a-bug case.) The narrow fix here is recognizing `bootstrap` itself.
- **Fix sketch:** treat `bootstrap`/`XSLoader::load`/`DynaLoader` as
  defining/loader builtins (suppress unresolved on the loader call, and optionally
  mark the package as XS-backed so its missing-body subs aren't flagged).
- **Difficulty:** low (the loader call); the broader "this package is XS, don't
  flag its absent bodies" is medium.

### `diag-09` / `diag-10` — Log4perl typeglob-codegen accessors flagged unresolved-method
- **Cursors:** `Log/Log4perl/Logger.pm:879` (`is_warn`), `:883` (`warn`).
- **Construct:** Log4perl generates its level accessors (`warn`, `is_warn`,
  `error`, …) by installing closures into the symbol table in a `for` loop
  (`*{"$class\::$level"} = sub {...}`), then calls `$self->warn(...)` /
  `$self->is_warn` elsewhere.
- **Root cause:** the typeglob install is a *dynamic* `*{"...interp..."} = sub`
  inside a loop over a runtime list, so we don't synthesize symbols for the
  generated method names → the call sites flag unresolved-method.
- **Fix sketch:** extend the existing typeglob-codegen recognizer (it already
  handles `*name = sub` / `*{'literal'} = ...` / `*$m = sub`) to the
  loop-over-known-list case where the level names are a statically-derivable list
  (`qw(debug info warn error fatal)` constant-folded). Mints a Method symbol per
  generated name. Same provenance machinery as the current synthesis.
- **Difficulty:** medium. The names must be statically recoverable (constant-fold
  the loop list); truly-runtime lists stay out of scope (rule: no guessing).

---

## 3. Completion harvest

### `completion-datetime-hashkey` — `$self->{` offers too few keys
- **Cursor:** `DateTime.pm:315:30` (inside a `$self->{` in a method)
- **Expect:** the constructor-assigned keys appear — `local_rd_days`,
  `local_rd_secs`, `formatter`, `locale`, `offset_modifier`, `rd_nanosecs`,
  `utc_year`, … (offered as `key\tDateTime->{key}`).
- **Actual:** only a couple keys offered (the harvest finds ~2 of ~13).
- **Root cause:** the mutated-key set for a class is harvested from `$self->{k} =
  ...` writes, but DateTime's keys are assigned in a separate constructor helper
  (`_new` / `_recalc_*`) and via patterns the harvest doesn't walk (e.g.
  `@{$self}{@keys} = ...` hash-slice assignment, or keys set on a differently
  named lexical that becomes `$self`). So most slots never enter
  `mutated_keys_on_class`.
- **Fix sketch:** broaden slot-write harvesting — hash-slice writes
  (`@{$self}{...} = `), keys assigned on the blessed lexical before `return`, and
  keys flowing through a constructor helper (A4's cross-procedural tail). Overlaps
  the deferred "A4 v2 cross-file/cross-proc slot writes."
- **Difficulty:** medium–high. The cross-procedural part is the narrowing/flow
  frontier; the hash-slice-write part is a contained emission add.

### `completion-typetiny-imported-blessed` — imported subs absent from bareword completion
- **Cursor:** `Type/Tiny.pm:165:15` (a partial bareword in statement position)
- **Expect:** `blessed` (imported via `use Scalar::Util qw(blessed)`) is offered.
- **Actual:** only local subs are offered; imported names are missing.
- **Root cause:** bareword-statement completion sources candidates from local
  symbols (and maybe builtins) but doesn't fold the file's import surface
  (`imports[].imported_symbols`) into the candidate set.
- **Fix sketch:** add imported names to the bareword/function completion
  candidates (they're already in `analysis.imports`; the goto-def/diagnostic paths
  use them — completion just doesn't). Contained.
- **Difficulty:** low.

---

## 4. Signature-help invocant elision

### `sig-uri-check-path-function-noinvocant` — first arg dropped on a plain call
- **Cursor:** `URI/_generic.pm:58:13` (inside `_check_path( ... )`)
- **Expect:** `_check_path($path, $pre)` with `active param: 0 ($path)`.
- **Actual:** signature shows only `($pre)` — `$path` is wrongly elided as if it
  were an invocant.
- **Construct:** a **plain function call** `_check_path($rest, $$self)` (not a
  method call) whose sub is `sub _check_path { my ($path, $pre) = @_; ... }`.
- **Root cause:** signature-help drops the first parameter as the implicit
  invocant (`$self`) — correct for `$obj->meth(...)`, wrong for a plain
  `func(...)`. The call-shape (method vs function) isn't consulted before eliding.
- **Fix sketch:** gate the first-param-is-invocant elision on the call actually
  being a method call (`->`); plain function calls keep all params. The call ref
  already knows its kind (FunctionCall vs MethodCall) — signature-help should ask.
- **Difficulty:** low. A shape the producer already has; just consult it.

---

## 5. `--type-at` single-file CLI boundary

### `ti-12` — `$self = shift->SUPER::new` types only via the cross-file path
- **Cursor:** `Minion.pm:108:6` (the `$self` in `my $self = shift->SUPER::new;`)
- **Expect:** `$self` typed `Minion` (the enclosing class).
- **Resolved in the LSP.** Explicitly-qualified method dispatch (`SUPER::X` and
  fully-qualified `Foo::Bar::X`) now composes end-to-end. `emit_method_call_
  return_edges` peels the dispatch class from any `::`-bearing method token via
  one seam — `SUPER` → the *enclosing package's parents*, any other qualifier →
  that literal class — and emits a `QualifiedCallReturn` witness that looks the
  method up on the named class while typing the result relative to the invocant.
  `Mojo::Base::new` is receiver-polymorphic (`bless …, ref $class || $class` →
  `ReturnExpr::ReceiverOr`), so the parent ctor blesses into `Minion` and `$self`
  types `Minion`. Locked in by the **gold** `hover-minion-super-new` and the
  unit tests `test_super_new_types_to_calling_class`,
  `test_fq_method_call_dispatches_from_named_class`.
- **Why this row stays xfail:** the `--type-at` CLI mode is *single-file*
  (`cli_type_at` parses one file, no module index), so it cannot reach
  `Mojo::Base` cross-file to resolve `SUPER::new`. This is an inherent boundary
  of that debug CLI mode, not an LSP limitation — every cross-file query mode
  (`--hover`, `--definition`, the LSP server) takes a `<root>` and resolves it.
- **Reproduce:** `gold-corpus/run.pl hover` → `hover-minion-super-new` passes
  (`Minion`); `--type-at Minion.pm 108 6` returns `No type info` (no root).

---

## 6. Big QA sweep (mined against the snapshot substrate)

New gaps surfaced while mining gold from fresh CPAN modules. Each is pinned at
xfail (expected-correct confirmed from source; tool genuinely wrong).

### `hover-mojo-url-clone-via-new` / `ti-mojo-url-abs-clone-chain` — clone via `$self->new` — **MOSTLY FIXED**
`Mojo::URL::clone` does `my $clone = $self->new; @$clone{…}=…; return $clone`.
The **variable** `$clone` now types `Mojo::URL` end-to-end — at the assignment,
through the hash-slice mutation, and at `return $clone` (verified via `--hover`
at every point). Two landed pieces:

1. **Receiver-polymorphic constructors** (`bless {}, $class` /
   `bless {}, ref $self || $self`) emit `ReturnExpr::ReceiverOr` — the call-site
   receiver, else the enclosing class as fallback. So `Mojo::URL->new` →
   `Mojo::URL`, `Child->new` (inherited ctor) → `Child`. The fold lets a class
   dominate a hashref rep, so a hash-slice-mutated clone keeps the class.
2. **`SUPER::X` return typing** via the `SuperCallReturn` witness:
   `emit_method_call_return_edges` detects a `SUPER::`-prefixed method, looks `X`
   up on the *enclosing package's parents* (not the invocant's own class), and
   defaults the receiver to the invocant / enclosing class — preferring a dynamic
   outer receiver only when it `is_subclass_of` the enclosing class. So
   `Mojo::URL::new`'s body `shift->SUPER::new` resolves to `Mojo::Base::new` with
   receiver `Mojo::URL`, whose `ReturnExpr::ReceiverOr` substitutes `Mojo::URL`.

Locked in by the **gold** `hover-minion-super-new` and the unit test
`test_super_new_types_to_calling_class`.

**Residual — the SUB's stored *return type* (not the variable).** The fixture
`hover-mojo-url-clone-via-new` cursors the `sub clone` declaration, which reports
the sub's *declared* return type — still `HashRef`/null, NOT `Mojo::URL`. Root
cause is **build-time vs query-time**: `clone`'s `return_types` entry is seeded
in the fold (`seed_return_types_from_bag`) at build time, where the module index
isn't consulted, so the cross-file `$self->new → SUPER::new → Mojo::Base::new`
chain that the variable resolves *at query time* isn't visible to the build-time
seed. Only locally-available evidence (the `@$clone{…}` hash-slice rep) survives
into the stored return type. The variable hover recomputes with the module index
and gets `Mojo::URL`; the sub-return seed does not. **This is the same class of
gap as `ClassIsa`/`param_types` ancestry-gated *emission* deferred to the
ReceiverGated seam** (a sub-return whose value depends on a cross-file chain must
resolve on a query-time seam, not the build-time `return_types` map). Distinct
from SUPER itself — kept xfail. **Subsystem:** build-time `return_types` seed vs
query-time cross-file method-return composition. **Difficulty:** medium–high.

**Note on the earlier "base-class corruption" red herring.** A first SUPER
attempt appeared to make `find_method_return_type("Base", "new")` return `None`
whenever a sibling SUPER-delegating `new` was present, which was blamed on a
shared-`QueryState` memo / cycle-guard interaction. That was a **test artifact**,
not a memo bug: the probe blessed via `bless {}, ref $c || $c`, but
`bless_class_is_receiver` only recognises the canonical invocant names
(`$self` / `$class` / `$this` / `$proto`) — `$c` is not one, so the ctor fell to
`bless_class_of(||)` → `None`. Re-running with `$class`/`$self` shows same-file
SUPER resolving with no cross-query contamination. The memo is per-top-level
query (dropped on return) and keyed on `(bag, attachment, receiver, arity)`, so
it never leaks across queries — the corruption never existed.

### Fully-qualified method calls (`$obj->Foo::Bar::m`) — **supported across the LSP**
Perl dispatches `$obj->Foo::Bar::m` from the *named* class `Foo::Bar` (its MRO),
not the invocant's class. Now handled across every feature on two seams that
mirror how FunctionCall already treats `Foo::Bar::baz()`:
- **Return typing** — `emit_method_call_return_edges` peels the qualifier off
  any `::`-bearing method token and emits `QualifiedCallReturn{MethodOnClass{
  Foo::Bar, m}, …}`.
- **Navigation** — `method_call_invocant_class` returns the qualifier as the
  dispatch class; consumers resolve the method on the bare tail
  (`unqualified_target_name()`), and `method_name_span` narrows to the tail
  (`fq_tail_span`) so rename rewrites only `m`. This single class seam +
  bare-name projection drives goto-def, references, rename/prepareRename, hover,
  document-highlight, and signature-help.
- **Completion** — both `Foo::Bar::` (bare) and `$obj->Foo::Bar::` (on a variable
  invocant) complete the qualifier package's subs; the tree cursor-context detects
  a `::`-bearing method token after `->` and routes to `QualifiedPath`.
- **SUPER navigation** — `$self->SUPER::m` resolves to whichever ancestor
  actually defines `m`, walking the **full parent MRO** (`resolve_super_method`
  = the shared ancestor DFS with `skip_self`, so it searches every `@ISA` entry
  and skips the current package). goto-def jumps to it, hover shows it, and
  renaming the defining ancestor's method rewrites the SUPER call (a dangling
  SUPER call would be a broken rename — see the updated
  `rename-11-urn-canonical-precision`, which asserts the isbn SUPER call IS
  renamed while the subclass override and unrelated `_generic.pm` are not).
  references/rename resolve SUPER **lazily** in `refs_to` (the ref carries the
  `SUPER::` marker + its enclosing scope, and `refs_to` runs with the module
  index), so a SUPER call into a *cross-file* parent in a dependency file —
  which the build-time stamp can't name — still resolves. `$self->SUPER::`
  completion falls through to ordinary method completion (SUPER isn't a
  package), so inherited methods still show.

Proven by `test_fq_method_call_dispatches_from_named_class` (return typing),
`test_fq_method_call_nav_dispatches_from_named_class` (goto-def / rename / class
resolution / tail-span), `test_super_method_nav_resolves_to_parent` +
`test_super_resolves_across_all_parents` (SUPER nav incl. multi-parent + skip-
self), and `test_tree_context_fq_method_is_qualified_path` (completion context).

### `return bless {BLOCK}, CLASS` class arg stranding — **FIXED**
tree-sitter-perl parses `return bless {}, $class` as `return((bless {}), $class)`:
the brace block greedily ends the parenless `bless`, stranding the class arg as a
sibling of the `return_expression` in the enclosing `list_expression`. Perl parses
`bless` as a list operator (the comma binds to bless), so that sibling IS the
class. `bless_args` splices it back (gated to the parenless `ambiguous_function_
call_expression` so an explicit `bless({}), $x` 2-element return isn't misread).
This masked for non-inherited ctors (enclosing package == the class) but broke
foreign-literal (`bless {}, 'Other'` → enclosing, not `Other`) and bare-`{}`
receiver-poly ctors (`return bless {}, ref $class || $class` → enclosing, not
`ReceiverOr`, so inherited/SUPER ctors lost the subclass). Locked in by
`test_bless_return_strands_class_arg_recovered`,
`test_bless_positional_self_is_receiver` (`$_[0]` is the positional invocant,
already recognised via `is_positional_receiver`).

### `diag-mojo-cookiejar-helper-fp` / `diag-mojo-daemon-callback-fp` — first-param-self over-reach in OO classes
In an OO class, a plain helper (`sub _compare { my ($cookie,…)=@_ }`) or an
anonymous callback (`on(request => sub { my $tx = shift; … })` ) has its first
param typed as the enclosing class, so a method call on it (`$cookie->path`,
`$tx->req`) fires a false `unresolved-method`. The `-strict` (non-OO module) case
of this is now fixed; the in-OO-class helper/callback case is the harder residual —
there's no clean static signal distinguishing a method from a helper in an OO
class. **Subsystem:** first-param-self heuristic (`detect_first_param_type`).
**Difficulty:** high (inherently ambiguous).

---

## Triage summary

| gap | subsystem | difficulty |
|---|---|---|
| sig-uri-check-path-function-noinvocant | signature-help invocant | **low** |
| completion-typetiny-imported-blessed | completion candidates | **low** |
| diag-08 (loader call) | XS loader recognition | **low** |
| diag-09 / diag-10 | typeglob-codegen synthesis | medium |
| def-16-codegen-type-function | Type::Library synthesis | medium |
| completion-datetime-hashkey | slot-write harvest (A4 tail) | medium–high |
| mojo-url clone *sub-return* (variable is fixed) | build-time `return_types` seed vs query-time cross-file method-return | medium–high |
| diag-mojo-cookiejar/daemon first-param-self | invocant heuristic in OO class | **high** (ambiguous) |

Quickest wins: the signature-help invocant gate, imported-names in completion,
the `bootstrap` loader recognition, and the `(shift, shift)` param extraction —
all "the producer already has the signal, just consult it."
