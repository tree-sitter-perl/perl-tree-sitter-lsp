# Long-distance intelligence — the "look elsewhere" epic

**Status: members 1–2 + the shared primitive LANDED (role-contracts-2); 3–4 open.** Every item here shares one
inversion: the information a position needs lives in OTHER files that
REFERENCE this one — composers of this role, callers of this sub,
entrypoints loading this plugin. Normal resolution flows definition →
use; this family flows use → definition. The members keep arriving
independently (four so far, all from veesh's QA sessions), which is
the tell that the SHARED PRIMITIVE is the real deliverable.

## The shared primitive: reverse edges

We already maintain three reverse indexes, built at module
registration and warm-rebuild (module_index.rs — and note the B6
lesson, twice paid: every reverse index MUST be fed by BOTH the
insert path and the warm rebuild):

- `reverse_index`: name → defining/exporting modules
- `bridges_index`: bridge class → registering modules
- (workspace + dependency modules both feed these since the
  helper-consumption fix)

The epic needs one more, and most members fall out of it:

- **`children_index`: class/role → modules that `isa`/compose it**
  (inverse `package_parents`). Same registration/rebuild discipline,
  same purge-on-rereg. "Who composes Clove::Sheets" becomes O(1).

Plus one harder collector for the param-typing member:

- **arg-shape collection**: call sites of a named sub, with each
  call's arg `InferredType`s. The refs/bindings exist per-file; the
  cross-file gather + fold (agree → type, disagree → widen) is new.
  This is witness-bag-shaped: a `Param{sub, index}` attachment fed by
  caller-side witnesses, folded by the existing lattice rules.

## Members

### 1. requires → implementations (task #27) — LANDED

In a role, `requires 'fetch'` declares a contract; the
implementations live in composers. Protocol call: **goto-def stays on
the contract** (the requires atom — it IS the declaration), and
**textDocument/implementation** (not currently provided at all)
returns every composer's def — exactly the interface→impls semantics
the LSP method was designed for. gr from the marker stays "call
sites". Mechanics: `children_index[role]` → transitive composers →
each one's local def of the name (plus has-synthesized accessors).
Also works from `$self->fetch` inside the role (resolves to the
marker, then the same fan-out). Cheap once children_index exists;
the implementation-provider plumbing (capability, backend handler,
CLI mirror, gold rows) is the bulk.

### 2. Composer-mismatch diagnostic — LANDED (`adr/role-contracts.md`)

Same edge, opposite assertion: composer P of role R must provide
every name in `R.role_requires`. Fully designed; blocked on nothing
— `role_requires` is recorded, ancestry walks exist. WARNING
severity (Perl dies at compose time). Needs substrate calibration
(dynamically-installed methods, AUTOLOAD composers).

### 3. Long-distance param typing (task #25)

`plugin 'CloveApp', { minion => 1, … }` → `$conf:
HashWithKeys{minion, redis, schema}` inside register. Generalizes to
any sub with enumerable callers: gather call-site arg shapes, fold
with the existing agreement rules (all agree → the type; disagree →
widen; any unknown caller → open/decline). Half the machinery exists
(BranchArmFold's shape); the gather is the new arg-shape collector.
Highest ceiling, highest cost — and the one that must NOT lie:
a single un-enumerable caller (dynamic dispatch, exported surface)
poisons the claim, so the gate is "callers enumerable" first.

### 4. Entrypoint-scan helper lint (docs/prompt-helper-consumption.md phase 2)

At `$c->was_loaded`: "provided by Clove::App::Plugin::WasLoaded,
which no entrypoint loads". Scan = workspace lite scripts +
Mojolicious subclasses for `plugin` registrations — a read-only
miniature of member 3's gather. Hint severity + future auto-fix
(insert the `plugin` line, mirroring auto-import).

## Sequencing recommendation

1. **children_index** — the primitive; small, pure infrastructure,
   B6-disciplined from birth.
2. **#27 requires→implementations** — first consumer, visible
   payoff in crm immediately, exercises the index.
3. **Composer-mismatch diagnostic** — second consumer of the same
   index + role_requires; turns the "golden" hint family into a
   two-sided contract check.
4. **Entrypoint-scan lint** — first taste of cross-file gathering
   with trivial fold semantics.
5. **Param typing (#25)** — last: it wants the gather muscle warmed
   up by 4, and its honesty gate (caller enumerability) deserves its
   own design round.

Items 1–3 are one PR-sized arc ("the role contracts epic, part 2").
4 and 5 are each their own.
