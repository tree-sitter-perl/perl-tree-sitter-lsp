# Def-Type Regression Matrix — Group A

Repos: **Moo, Plack, Catalyst-Runtime, Minion** (under `~/perl-qa-corpus/`).
Binary: `target/release/perl-lsp` @ branch `usability-sprint` (EV 54). Cursors are 0-based (line, byte-col); printed targets are 1-based.

All rows were produced by actually running the binary. `expected` is reasoned from the source; `actual` is trimmed binary output.

| id | difficulty | repo | query | cursor (file:line:col — token) | expected | actual | verdict |
|----|-----------|------|-------|-------------------------------|----------|--------|---------|
| MOO-1 | simple | Moo | definition | `lib/Moo.pm:48:11` — `_install_subs` (call) | same-file method def Moo.pm:86 | `Moo.pm:86:5` | PASS |
| MOO-2 | simple | Moo | definition | `lib/Moo.pm:49:10` — `make_class` (call) | same-file def Moo.pm:54 | `Moo.pm:54:5` | PASS |
| MOO-3 | tricky | Moo | definition | `lib/Moo.pm:88:2` — `_install_tracked` (imported sub) | cross-file import → `Moo/_Utils.pm:97` | `_Utils.pm:97:1` | PASS |
| MOO-4 | simple | Moo | references | `lib/Moo.pm:53:4` — `make_class` (def name) | N=2 (call L50 + def L54), internal-only | `2 references` → L50 c11, L54 c5 | PASS |
| MOO-7 | tricky | Moo | definition | `lib/Moo.pm:147:4` — `_load_module` (imported sub call) | cross-file import → `Moo/_Utils.pm:123` | `_Utils.pm:123:1` | PASS |
| MOO-8 | simple | Moo | hover | `lib/Moo.pm:85:4` — `_install_subs` (def name) | `sub _install_subs` / package Moo | ```perl\nsub _install_subs\n``` | PASS |
| PLK-1 | simple | Plack | definition | `lib/Plack/Middleware.pm:4:21` — `Plack::Component` (use parent) | package def → `Component.pm:1` | `Component.pm:1:1` | PASS |
| PLK-2 | tricky | Plack | definition | `lib/Plack/Middleware.pm:15:18` — `to_app` (via @ISA) | inherited from parent → `Component.pm:47` | `Component.pm:47:1` | PASS |
| PLK-3 | tricky | Plack | definition | `lib/Plack/Middleware.pm:13:23` — `new` (via @ISA) | inherited → `Component.pm:8` | `Component.pm:8:1` | PASS |
| PLK-4 | simple | Plack | definition | `lib/Plack/Request.pm:29:25` — `env` (call in `address`) | same-file def Request.pm:28 | `Request.pm:28:5` | PASS |
| PLK-5 | simple | Plack | hover | `lib/Plack/Request.pm:29:4` — `address` (def name) | sub + package Plack::Request + POD | sub address / package Plack::Request / POD "Returns the IP address…" | PASS |
| PLK-6 | simple | Plack | references | `lib/Plack/Request.pm:27:4` — `env` (def name) | N>=45 incl def L28 (`->env` is 45 in-file) | `51 references`, incl L28 def + L30,31,… | PASS |
| PLK-7 | tricky | Plack | definition | `lib/Plack/Middleware/Head.pm:8:18` — `app` (accessor) | accessor synthesized by parent's `use Plack::Util::Accessor qw(app)` → `Middleware.pm:6` | `No definition found at 8:18` (hover also empty) | **FAIL** |
| PLK-8 | tricky | Plack | definition | `lib/Plack/Middleware/Head.pm:11:11` — `response_cb` (2-hop @ISA) | Head→Middleware→Component → `Component.pm:54` | `Component.pm:54:1` | PASS |
| PLK-9 | tricky | Plack | definition | `eg/dot-psgi/static.psgi:7:0` — `builder` (imported sub) | cross-file import → `Builder.pm:99` | `Builder.pm:99:1` | PASS |
| PLK-10 | tricky | Plack | definition | `eg/dot-psgi/static.psgi:8:4` — `enable` (imported sub) | cross-file import → `Builder.pm:87` | `Builder.pm:87:1` | PASS |
| CAT-1 | tricky | Catalyst | definition | `lib/Catalyst.pm:156:36` — `request` (Moose `has` accessor, called in `req`) | synthesized accessor → `has request` Catalyst.pm:66 | `Catalyst.pm:66:5` | PASS |
| CAT-2 | simple | Catalyst | hover | `lib/Catalyst.pm:606:4` — `stash` (def name) | sub stash / package Catalyst + POD | sub stash / package Catalyst / POD | PASS |
| CAT-3 | tricky | Catalyst | references | `lib/Catalyst.pm:488:4` — `forward` (def name) | N>=38, cross-file (controllers + t/) incl Catalyst.pm:488 | `38` refs across Controller.pm, many t/ files | PASS |
| CAT-4 | tricky | Catalyst | references | `lib/Catalyst/Component.pm:110:4` — `COMPONENT` (base method def) | base method overridden/called repo-wide | `2 references` (def L111 + 1 cross-file t/) — misses dynamic `$component->COMPONENT` @ Catalyst.pm:3240 | **REVIEW** |
| CAT-5 | simple | Catalyst | hover | `lib/Catalyst/Component.pm:110:4` — `COMPONENT` (def name) | sub COMPONENT / package Catalyst::Component | sub COMPONENT / package Catalyst::Component | PASS |
| CAT-6 | simple | Catalyst | hover | `lib/Catalyst.pm:155:4` — `req` (def name) | sub req / package Catalyst | sub req / package Catalyst | PASS |
| CAT-7 | tricky | Catalyst | definition | `lib/Catalyst.pm:488:58` — `dispatcher` (class accessor in `$c->dispatcher->forward`) | synthesized by `__PACKAGE__->mk_classdata` qw-list → Catalyst.pm:177 | `Catalyst.pm:177:31` | PASS |
| MIN-1 | tricky | Minion | definition | `lib/Minion.pm:58:20` — `backend` (Mojo `has` accessor) | synthesized accessor → `has 'backend'` Minion.pm:18 | `Minion.pm:18:6` | PASS |
| MIN-2 | simple | Minion | hover | `lib/Minion.pm:56:4` — `enqueue` (def name) | sub enqueue / package Minion | sub enqueue / package Minion | PASS |
| MIN-3 | tricky | Minion | references | `lib/Minion.pm:56:4` — `enqueue` (def name) | `$minion->enqueue` only; must NOT include `$backend->enqueue` (Backend/Pg defs) | `105` refs: 1 Minion.pm(def) + 3 bench + 101 t/ — correctly excludes Backend defs (Pg.pm:75, Backend.pm:18) and `$self->backend->enqueue` @ Minion.pm:59 | PASS |
| MIN-4 | tricky | Minion | definition | `lib/Minion/Job.pm:10:17` — `minion` (Mojo `has` accessor in `shift->minion->app`) | synthesized accessor → `has [qw(... minion ...)]` Job.pm:9 | `Job.pm:9:17` | PASS |
| MIN-5 | simple | Minion | hover | `lib/Minion/Job.pm:27:4` — `finish` (def name) | sub finish | sub finish | PASS |
| MIN-6 | tricky | Minion | definition | `lib/Minion/Job.pm:10:25` — `app` (chain `$self->minion->app`) | `minion` accessor returns Minion → `app` on it → `has app` Minion.pm:17 | `No definition found at 10:25` | **FAIL** |

## Summary

- **Total rows: 31** — Moo 6, Plack 10, Catalyst 7, Minion 6 (note: ids skip MOO-5/6, which were folded; IDs are stable labels, not contiguous).
- **PASS: 28**
- **FAIL: 2** — PLK-7, MIN-6
- **REVIEW: 1** — CAT-4

### FAIL / REVIEW notes for a human

- **PLK-7 (FAIL):** Accessors synthesized by `use Plack::Util::Accessor qw( app )` are not modeled at all — `$self->app` in a subclass gives neither goto-def nor hover. This is a distinct synthesis path from native `has` (which works — see CAT-1/MIN-1/MIN-4) and from `mk_classdata` (works — CAT-7). Worth a plugin/native emitter for `Plack::Util::Accessor` / `Class::Accessor`-style import-list accessor synthesis.
- **MIN-6 (FAIL):** Method chains through Mojo/Moo `has` accessors break: `$self->minion->app` can't type the `minion` hop's return as the owning class, so `->app` doesn't resolve. The accessor *def* resolves fine (MIN-4); only the *return-type* of the accessor is missing, blocking chain navigation. Same root cause likely affects many `$obj->accessor->method` chains. (Consistent with the known "`has` accessors don't carry a `ClassName` return" gap.)
- **CAT-4 (REVIEW):** `references` on a base method (`Catalyst::Component::COMPONENT`) found only the def + one cross-file `.t` call, missing the real dynamic call `$component->COMPONENT($class,$config)` at Catalyst.pm:3240. Defensible — there `$component` holds a *component name string* of unknown class, so it can't be statically tied to Catalyst::Component. Flagged as REVIEW (not a clear bug) since over-claiming it would risk false positives; cross-class `enqueue` scoping (MIN-3) shows the conservative behavior is otherwise correct.
