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

## 5. Parser-version regression

### `ti-12` — `$self = shift->SUPER::new` no longer types (ts-parser-perl 1.1.0)
- **Cursor:** `Minion.pm:108:6` (the `$self` in `my $self = shift->SUPER::new;`)
- **Expect:** `$self` typed `Minion` (the enclosing class).
- **Actual:** `No type info` on **1.1.0**; returned `Minion` on **1.0.3**.
- **Construct:** `my $self = shift->SUPER::new;` in a `Mojo::Base`-derived class.
- **Root cause (narrowed; obvious causes ruled out):** A/B'd both parser versions.
  Findings:
  - **Cross-file only.** Single-file `--type-at` and `--dump-package` show `$self`
    with NO type constraint (`vars_in_scope` = `$class`, `$e`) on **both** 1.0.3
    and 1.1.0, and the minimal `my $self = shift->SUPER::new` is `None` on both.
    Only the substrate/batch path (root + `PERL5LIB` + module index) yields
    `Minion` on 1.0.3 / `None` on 1.1.0. So `$self`'s type is a *query-time*
    bag-edge chase (`Variable $self → Edge(Expr(shift->SUPER::new))`), not a TC.
  - **CST identical.** The `shift->SUPER::new` parse is byte-identical across
    versions (invocant `func1op_call_expression` for bare `shift`, method token
    `"SUPER::new"`) — verified, not assumed.
  - **Not the local suspects.** `use Mojo::Base 'Mojo::EventEmitter'` parses
    identically (base extraction intact); `Mojo::Base::new` returns `None` on
    both (so the `Minion` did NOT come from the parent's `new` return) — it's a
    constructor/invocant-class fallback (`shift`→Minion, `->…new` → invocant
    class) resolved cross-file.
  - **Conclusion:** 1.1.0 disrupted the cross-file `new`/SUPER chain-return
    resolution via a parse change in *some other file the resolution walk
    touches* (an ancestor in Minion's MRO, or the module-index method-return
    path) — not Minion's own decl. Needs chain-typer instrumentation to pin the
    exact hop.
- **Reproduce:** `gold-corpus/run.pl --emit type-at Minion.pm 108 6` (→ `Minion`
  on 1.0.3, `None` on 1.1.0). The minimal single-file repro does NOT reproduce.
- **Difficulty:** unknown (exact broken hop open; obvious causes eliminated).

---

## 6. Big QA sweep (mined against the snapshot substrate)

New gaps surfaced while mining gold from fresh CPAN modules. Each is pinned at
xfail (expected-correct confirmed from source; tool genuinely wrong).

### `hover-mojo-url-clone-via-new` / `ti-mojo-url-abs-clone-chain` — clone via `$self->new` types as HashRef
`Mojo::URL::clone` does `my $clone = $self->new; @$clone{…}=…; return $clone` →
expected `Mojo::URL`, actual `HashRef`.

**PARTIALLY FIXED.** Receiver-polymorphic constructors now type correctly:
`bless {}, $class` / `bless {}, ref $self || $self` emit `ReturnExpr::ReceiverOr`
(the call-site receiver, else the enclosing class as fallback). So `Mojo::URL->new`
→ `Mojo::URL`, `Child->new` (inherited ctor) → `Child`, and the `SUPER::new` chain
composes — all at **query time**. The fold already lets a class dominate a hashref
rep, verified: a *local*-ctor `my $x = $self->new; $x->{k}=…; return $x` returns the
class, not HashRef.

**Residual (the actual remaining break):** `$clone` never gets stamped with the
class at the **assignment**, because resolving `$self->new` there requires the
cross-file `SUPER::new` hop (Mojo::URL::new → Mojo::Base::new) at **build time**,
where the module index is not warm (it is at query time — which is why
`Mojo::URL->new` typed directly works, but `$clone = $self->new` inside the method
does not). With no class witness on `$clone`, only the `@$clone{…}` deref-rep
observations remain → HashRef. **Fix path:** make the build-time chain typer stamp
the assignment via a query-time-chaseable edge (`Variable($clone) →
Edge(Expression($self->new))`) so the cross-file SUPER chain resolves when the
variable is folded, instead of relying on a build-time materialized class.
**Subsystem:** build-time chain typer / assignment edge for cross-file method
returns. **Difficulty:** high (build-time vs query-time cross-file resolution).

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
| ti-12 + mojo-url clone-via-new | cross-file receiver-polymorphic ctor (`bless {}, ref $self \|\| $self`) → `Receiver` emission + no-receiver fallback | **high** |
| diag-mojo-cookiejar/daemon first-param-self | invocant heuristic in OO class | **high** (ambiguous) |

Quickest wins: the signature-help invocant gate, imported-names in completion,
the `bootstrap` loader recognition, and the `(shift, shift)` param extraction —
all "the producer already has the signal, just consult it."
