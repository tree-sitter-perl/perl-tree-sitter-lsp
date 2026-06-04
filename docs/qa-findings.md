# QA findings — open worklist

Real-world QA sweeps to date: (1) crm + Dancer2/metacpan-web/Moose; (2) 17 local `~/personal`
projects; (3) **Round 1 fresh corpus** (`~/perl-qa-corpus`): Email::Stuffer (rjbs), Type::Tiny
(tobyink), Perl::Critic, AWStats (legacy CGI), cpanminus (FatPacked). This file tracks only what's **open**.

## ✅ Closed
**PR #45 sprint:** moo phantom accessors · `export_ok` bare-`use` · catalyst `param_types` over-application ·
cross-file `$c` multi-hop · dancer DSL keywords · cold-start 9 min → ~8 s (witness-bag exponential memoized).

**Round 1 (landed `5c3b3e5`, EXTRACT_VERSION 44):**
A1 bare-word filehandles (indirect-object guard + builtin FHs) · A3 `use constant` (scalar + block forms) ·
A2 `my $x = shift` typing (already worked — parity lock test) · H3 `require Bareword` (lock test) ·
F `requires`/Role::Tiny in framework imports · C1 `mk_group_accessors`/`mk_classdata` synthesis ·
C2 `is_dbic_class` ancestry walk (2+-level DBIC) · E1 `use X -base` parent registration · E3 `has` comma-form ·
B6 warm-cache exporter attribution (reverse-index rebuilt on CLI warm path).
**Docs/handoff:** `docs/parser-shortcomings.md` (G1 `$#_`-in-string, G2 top-level bare block, G3 `<<''`) ·
`docs/qa-design-items.md` · ts-parser-perl bump 1.0.1 → **1.0.3** · `--timings` per-module build timing.

## ✅ Round 1 — stability/perf PASS
**Zero crashes/panics across all 5 fresh projects** (incl. 22k-line `awstats.pl`, 1654-line FatPacked `cpanm`,
Type::Tiny inlined-codegen). Cold ≤ 1.6s, warm ≤ 0.9s everywhere; no per-file outlier. First-party source is
clean (cpanminus 0% FP on hand-written code; all FPs in bundled/dep code).

---

## ★ NAV — navigation reliability (NEW, Round 1 headline) — **DESIGN, discuss**
goto-def / references / hover are **less reliable than diagnostics and diverge from them** — every Round-1
project hit this. This is the residual `resolve_symbol` cursor→target unification (CLAUDE.md: "planned but not
landed"; handlers map `RenameKind`→`TargetRef` inline, separate from the diagnostic resolution path).
- **NAV-1 confident WRONG jumps** *(Email::Stuffer)*. `$self->_assert_addr_list_ok` (def :296) → goto-def lands on
  **line 3 (`use warnings`)**; `shift()->_self` → resolves to the *wrong* sub. Worse than a miss. Hover *does*
  report `type: Email::Stuffer`, so the type is known but not threaded into method-call def resolution.
- **NAV-2 references unreliable both directions** *(Email::Stuffer, AWStats, Perl::Critic)*. Local-method refs
  return **0** despite 10+ in-file calls; common-name `new` floods **98 cross-corpus** matches (name-only, ignores
  invocant class). AWStats: `Format_Number` 172 call sites → refs reports **6**.
- **NAV-3 diagnostic resolves, goto-def doesn't** *(Perl::Critic, Type::Tiny)*. Inherited 1-hop method: not
  flagged (diagnostic walks inheritance) but goto-def "No definition found". Explicitly-named cross-file import
  (`to_TypeTiny`, `hashify`): not flagged + found by workspace-symbol, yet goto-def/references/hover miss it —
  **even intra-file** (`Utils.pm` `hashify`). Divergent resolution paths is the smoking gun.

## REF-1 — ref emission for calls embedded in expressions (NEW) — **contained, builder**
*(AWStats)* Sub-calls as operands in `.`-concatenation / complex expressions don't emit a ref: `print "<td>".Format_Number($x)."</td>"` — the `Format_Number` call gets no `FunctionCall` ref, so references undercount
(172→6) and goto-def is intermittent on byte-identical adjacent lines. Rule #7 violation in the builder.
Likely a partial driver of NAV-2's undercount. **Investigate where call-ref emission is gated to statement level.**

## CG — sub codegen not synthesized as symbols (NEW) — **contained, high volume**
- **CG-1 typeglob accessors** *(cpanminus File::Fetch/IPC::Cmd/File::Path ~half its diagnostics; Type::Tiny)*.
  `*name = sub {...}`, `*$method = sub {...}` (in `no strict 'refs'` loops), `*{ 'is_'.$n } = $coderef`,
  `*_FORCE_WRITABLE = sub () {...}` in BEGIN. Recognize the glob-assign-to-sub shape → synthesize Sub/Method
  symbols (same "synthesize by shape" pattern as `has`/`add_columns`/`mk_group_accessors`). Dominant FP source.
- **CG-2 Class::Tiny** *(cpanminus)*. `use Class::Tiny qw/resolvers cache/` and the hashref form synthesize
  accessors like Moo/Moose `has` — not recognized (`framework: null`). Diagnostics + goto/refs/hover all miss.

## B. Exporter consumer-side semantics — refined triggers (corroborates known cluster)
Still mostly **DESIGN** (the tag/rename/re-export system, `qa-design-items.md`), but with sharp new triggers:
- **B-tag — `:tag`/`:ALL`/`:DEFAULT` expansion not applied** *(Email::Stuffer File::Spec `:ALL`; Perl::Critic;
  Type::Tiny Exporter::Tiny `-types`/`-is`/`-assert`)*. Named imports resolve; the same name via tag doesn't.
  **Sharp NEW trigger — `Readonly::Array`/`Readonly::Hash` export tables** *(Perl::Critic, isolates cleanly:
  `hashify` named-import not flagged, same `hashify` via `:data_conversion` built by `Readonly::Hash our %EXPORT_TAGS => (...)` flagged 41×)*. Reading `%EXPORT_TAGS` membership (incl. the Readonly-wrapped form) to
  expand tags on the consumer's `use` line is a **contained-ish sub-fix** of the larger system.
- **B1 re-exporting test frameworks** *(Test::Most 253, Test::Spec 129)* — runtime `push @EXPORT => @{"$mod::EXPORT"}` re-export idiom. DESIGN (no re-export-chain concept yet).
- **B4 cross-file `@EXPORT` bare-`use` at scale** *(Bugzilla ~899)* · **B5 imported-function CALL SITES don't
  resolve** *(see NAV-3 — likely the same divergence)* · **B7 Exporter::Extensible / regex import args**.

## D. Multi-hop classic inheritance — **DESIGN**
- **D1 deep `use base`/`@ISA` method resolution** *(Bugzilla ~304, schema-loader)*. Single-hop works; 3-hop
  chains don't. (Inherited-method *goto-def* miss is NAV-3.)

## E. Framework-specific (Mojo / Moo) — remaining
- **E2 helper-sub `$c` param typing** *(Mojo ~75)* — `sub _helper($c, …)` types `$c` as plugin class not
  `Mojolicious::Controller`. DESIGN (`qa-design-items.md`).
- **E4 `MooX::Options` `option`; `with 'Role'` required methods cross-file; MooseX::Role::Parameterized;
  multiline `has => sub{…}` default return.**

## A4 / NARROW — type-narrowing & boundary loss
- **A4 hash-extracted invocant loses type** *(SpamAssassin/perltidy/Mojo)*. `my $x = $self->{field}` → HashRef.
- **NARROW-1 (NEW)** *(Email::Stuffer)* `if (Params::Util::_INSTANCE($x,'IO::All::File')) { $x->binmode }` — the
  `_INSTANCE($x,'Class')` guard isn't recognized as narrowing `$x` to `Class`. Type-narrowing-guard family.

## MAIN-1 — `main::` aggregation across `require` (NEW classic variant) — **DESIGN**
*(AWStats ~270 FPs both directions)* Package-less scripts `require`'d into `main::` (legacy CGI): plugins call
host subs and vice-versa, all `main`, but each file is analyzed in isolation so cross-file `main::` symbols don't
unify. Distinct from `@ISA` — implicit `main` aggregation. Legacy-CGI-specific; judgment call whether to model.

## G. Upstream grammar gaps (→ `docs/parser-shortcomings.md`, hand off to parser team)
- **G1 `$#_` interpolated in a string** · **G2 top-level bare `{…}` block** (perltidy) · **G3 `<<''` heredoc**.
- **GR-1 (NEW) v-string** `$^V lt v5.6.0` → `v5.6.0` parsed as `v5(...)` function call *(AWStats; 3 sites)*.
  Stopgap option: builder-side suppress the FP by recognizing the `vN.N.N` shape.
- **GR-2 (NEW) `CONSTANT && expr`** → recoverable ERROR on `&&` when LHS is a bareword constant, inside
  sub/inlined bodies *(Type::Tiny; 3/47 files)*. Parser recovers; diagnostics stay complete.
- (`not` operator: tree-sitter-perl#230.)

## H. Minor / nav
- **H1 duplicate-package resolution** *(Bugzilla)* · **H2 block-scoped `package` reversion** *(DBD::Pg; also
  Type::Tiny Type/Parser.pm — 4 packages/file, invocant mis-attributed to enclosing package)* · **H4 bless-
  constructor type** · goto-def off-by-one; `} or next`; `\&{$expr}`.

## Reference — confirmed NOT bugs
- XS-defined methods (DBI, PPI methods on untyped param invocants) — inherent, expected (Perl::Critic's 280
  `unresolved-method` are PPI methods on `$elem`/`$psib` args with no inferred class).
- Dynamically-installed subs (typeglob/glob-alias) are invisible *unless* the install shape is statically
  recognizable — CG-1 is the recognizable subset; truly dynamic `*{$runtime_expr} = ...` stays out of scope.
- `--dump-package` faithfully mirrors the editor query path.

## Diagnostic-noise note
Default `--check --severity warning` stays clean (0 output) everywhere; noise lives in hint/info. The dominant
open FP drivers are now **CG (codegen synthesis)**, **B-tag (exporter tags)**, and the **NAV** reliability gap.
Clearing CG + B-tag + landing NAV unification is what makes the hint channel trustworthy at warning level.
