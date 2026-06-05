# QA findings — open worklist

Tracks what's **open**; landed work is collapsed into the summary. `usability-sprint @ 4071e46`, EV 54,
853 unit + 108 e2e green. Corpus: ~45 real projects across eras/authors.

## ✅ Landed (PR #45 — full sprint, EV 54)
**Zero crashes across the corpus; warning-severity channel clean everywhere.**
- **Classic-Perl FPs:** filehandles (indirect-object), `use constant` (scalar / block / multi-NAME-form),
  `my $x = shift`, `require Bareword`.
- **Frameworks/codegen:** `requires`/Role::Tiny, Class::Tiny, `use X -base`, `has` comma-form, DBIC ancestry,
  `mk_group_accessors`/`mk_classdata` (incl. `for`-loop), typeglob codegen (`*name`/`*$x`=sub, literal-return
  loop, cross-pkg `*{'Pkg::'.$n}=->can()`), AutoLoader/SelfLoader `__END__` subs, hashref-value typing,
  MooX::Options `option`, E2 helper `$c` (named-sub + inline).
- **NAV unification:** method-call refs carry a build-time resolved-target edge (refs_to/goto-def/hover
  single-sourced); precise **and** complete (0% false-exclusion, 100% typed recall), **arbitrary depth** incl.
  chained-method-return invocants; package-decl + same-name fallbacks dropped → honest-miss on untyped receivers.
- **A4 hash-slot typing** (the `Mutates` effect, v1 within-file): `SlotType{class,key}` witness + `SlotTypeFold`;
  `$self->{k}=Obj->new; my $x=$self->{k}; $x->m` resolves. (Over-typing-as-container-class fixed.)
- **Exporter:** consumer import-binding (bare→`@EXPORT`, `:tag`/`:DEFAULT`, `-as`), single-sourced
  diagnostic+goto-def; FQ-global `@Pkg::EXPORT` folded (**Bugzilla unresolved-function 1163→95**); export-member
  refs; Sub::Exporter `-setup`/`setup_exporter`; `%EXPORT_TAGS` (incl. Readonly).
- **Generic FQ symbols** (`split_qualified`): `Foo::bar()` calls + `$Foo::Bar::x` var reads.
- **Fat-comma:** every pair-walker positional (`=>` ≡ `,`); helpers renamed `*_fat_comma_*`→`*_pair_*`.
- **Resolution:** qualified `Pkg::sub` calls; imported-fn goto-def → the sub; `Class->method`→`package`; multi-hop
  `@ISA` (**verified closed**); cross-`@INC` inheritance (when the parent is installed); incomplete-ISA →
  unresolved-method suppressed.
- **Robustness/perf:** cache-clobber fix (a `None` on-demand miss no longer overwrites an indexed `Some`);
  cold-start 9 min→seconds; CLI `--references`/`--definition` position renderer; `--timings`; committed e2e
  warmup; ts-parser-perl 1.0.1→1.0.3.
- **Parser handoff:** `docs/parser-shortcomings.md` G1–G7, GR-1/2/3, X1 scanner thread-safety race.

## OPEN — sprint finish
### re-export chains — PARKED on the upstream scanner fix
Branch `worktree-agent-aae99d42f4d5d74bc` (7503933; worktree pruned, branch kept). Correct in isolation
(Test::Most `ok`/`is` resolve at the unit level) but triggers the **X1 scanner abort** under concurrent parsing.
Unblocks when the ts-parser-perl external scanner is made thread-safe (or re-export serializes its added
parsing). On rework: rebase, confirm no Bugzilla-cold abort, and **re-verify Test::Most→Test::More end-to-end**
(my CLI showed "0 resolved" — possibly the now-fixed cache-clobber bug). Design: `docs/adr/reexport-surface.md`
(on the branch); forms 1/2/3 done, form-4 (runtime `import` delegation) deferred (control-flow).
### BIG QA PASS — the capstone (after re-export lands; Veesh: one comprehensive pass)
Corpus sweep + def-type matrix + strictness audit, **and investigate Bugzilla `unresolved-method` 251→267
(+16)** introduced by the FQ/H2 wave.
### Then: finalize **PR #45 → main**.

## Punted (next sprint)
MAIN-1 (`main::`-across-`require`), H1 (dup-package path/role ranking — hard), MooseX::Role::Parameterized,
**narrowing / flow-sensitivity** (NARROW-1 + A4's cross-*branch* tail — `docs/prompt-type-system-futures.md`).

## Post-land tech debt / futures
Exporter recognition→**plugin extraction** (the `ExportSurface` seam) · **effects/`throws`** (designed,
`docs/prompt-type-system-futures.md`) · **A4 v2: cross-FILE slot writes** (module_index bridge, the
`MethodOnClass` pattern).

## Gold-corpus gaps (confirmed real, gold confidence)
Surfaced by the adversarially-verified gold corpus (`docs/gold-corpus/`, 157 gold rows). Each is a *true*
known-failing row (expected confirmed correct, tool genuinely wrong). Grouped by theme:
- **Ref/rename completeness** (largest cluster — rename rides the ref graph): in-body call sites not cross-linked
  (`ref-expt-mkopt`, `rename-06-build-exporter`); FQ refs to **non-exported** subs/constants don't link to def
  (`ref-uri-escape_char-fq`, `ref-uri-const-fq`); disjoint **def-side vs call-site** rename buckets
  (`rename-01/02-mkopt`); a `use constant` occurrence dropped (`rename-09-base-constant`); lexical deref spellings
  dropped — `%$tag_opts` (`ref-expt-tag_opts-lexical`), `$p{k}` inside a paren-wrapped ternary
  (`le-dt-p-hash-slices`, 27/30).
- **Diagnostics false-positives** (FP-suppression tail): own export called within its defining package (`diag-06`);
  dynamic typeglob-codegen methods flagged (`diag-09/10`); nested export `:tag` not recursively expanded
  (`diag-11`); `Test::More` plan directive misread as restricting the import list (`diag-12`).
- **Completion gaps**: `$self->{` offers 2 of 13 mutated keys — mutated-key harvest misses `$self->{k}=...` writes in
  constructors (`completion-datetime-hashkey`); imported subs absent from bareword completion
  (`completion-typetiny-imported-blessed`).
- **Semantic-token dead code**: `TOK_ENUM_MEMBER` never emitted — `use constant` names tag as `function`
  (`st-uri-constant-name-enummember`); `TOK_REGEXP` never emitted — `qr//` gets no token (`st-tt-regex-literal-regexp`).
- **Misc**: codegen type-fn (`Types::Standard::Any`) has no navigable target (`def-16`); imported-fn hover empty in
  `.t` consumers (`hover-jsonpp-encode-json-imported-t`); `$path` wrongly elided as invocant on a *plain function*
  call (`sig-uri-check-path-function-noinvocant`).

## Reference — confirmed NOT bugs
XS methods (DBI, PPI-on-untyped-param) · truly-dynamic `*{$runtime}=…` installs · methods from not-installed
deps · `--dump-package` faithfully mirrors the editor query path.
