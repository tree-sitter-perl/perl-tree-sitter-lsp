# ADR: Role contracts — markers, the provision predicate, and honest ancestry

A role's `requires 'name'` declares a contract: the role's body may
call `$self->name`, and every composing class must provide it. The
contract is one fact consumed by three verbs and a diagnostic, and the
design holds them together so they can't drift:

- **goto-def** lands on the contract — the requires atom IS the
  declaration.
- **goto-implementation** (`resolve.rs::implementations_of`) fans out
  to every transitive composer's def via `children_index`. One rule for
  every Method target: local defs of the name in descendant packages —
  so plain class methods get subclass overrides from the same code
  path, no role-vs-class branch (rule #10).
- **references** stays call-sites.
- **`role-requires-unfulfilled`** (WARNING — Perl dies at compose
  time) tells a composer it doesn't fulfill the contract
  (`FileAnalysis::unfulfilled_role_requires`).

## The marker is a symbol, identified by SymbolId

`requires NAMES` synthesizes one Method symbol per name (span = the
name's atom) so in-role `$self->name` resolves: no unresolved-method
hint, completion offers it, hover documents it. The names also land in
`FileAnalysis.role_requires` (per-package lists) and the synthesized
symbols' ids in `FileAnalysis.contract_symbols`.

The id set, not the name list, is what "is this def a marker?" must
consult. A role may both require AND define a name — the
default-implementation pattern (`requires 'fetch'; ... sub fetch {...}`
— requires as documentation, def as override-point). Name-keyed marker
checks fail in **both** directions:

- they eat the real def sitting beside the marker ("fetch is in
  `role_requires`, so every fetch symbol here is a marker" → the
  default impl doesn't count as provision);
- the names reverse index contains the marker symbol, so a name-keyed
  provision probe (`module_declaring_method_in_package`) finds the
  marker itself and every requires satisfies itself — the diagnostic
  goes permanently silent.

Both stay correct only symbol-keyed. The provision predicates
(`class_provides_method`, `provides_method_anywhere`,
`provides_method_in_package`) filter `contract_symbols` by id;
`method_resolution_on_class` stays marker-INCLUSIVE on purpose, because
dispatch resolution wants the contract (that's how in-role calls land
on it).

## Provision = a real def anywhere in the composer's MRO

For each local package with role parents: gather the required-name set
by walking **role-to-role edges only** from each direct parent (a base
CLASS's composed roles were checked at its own composition site;
role-composing-role defers its requires to the eventual class
composer). A name is provided when any class on the composer's MRO —
walked by `for_each_ancestor_class`, the same walk dispatch uses — has
a non-marker def: local sub, inherited method, `has` accessor, a
sibling role's def, a cross-package typeglob install, or a
plugin-bridged entity. Modifiers (`around`/`before`/`after`) never
synthesize symbols, so they correctly don't provide.

Role-ness itself is `FileAnalysis::is_role_package`, reading the
baked `role_packages` verdict. One predicate; consumers never
re-derive from use lists. The maker set behind the verdict is OPEN —
core holds no list at all:

- every plugin's `role_makers()` manifest contributes engine modules;
  the four base engines (Moo::Role / Moose::Role / Mouse::Role /
  Role::Tiny — `Role::Tiny::With` deliberately absent, it grants
  `with` to plain classes) live in `frameworks/moo.rhai`'s manifest,
  not in core;
- Moo-shaped house kits can instead emit `SyntheticUse "Moo::Role"` —
  `process_use` feeds the verdict identically for real and synthetic
  uses, so kit chains mark through either hop (crm's
  `clove-role.rhai` is the live example, exercised by calibration).

A role engine that is neither declared nor Moo-shaped is one
`role_makers()` line in a plugin away.

## Honest silence rides the one incompleteness seam

The diagnostic must not fire when provision could exist where we can't
see. All three escape hatches are facts, not message-level special
cases:

- **Roles are never diagnosed** — their obligations transfer.
- **AUTOLOAD anywhere in the MRO** can satisfy any contract at runtime.
- **Incomplete ancestry** — `class_has_unresolved_ancestor`, the
  single source of "is the inheritance chain incomplete" (it already
  gated the unresolved-method hint). This ADR added one input to it:
  `FileAnalysis.dynamic_parent_packages`, packages with a
  `with`/`extends` argument that didn't fold to a literal name.
  `with ReportProxy(type => ...)` generates a role at runtime; the
  recorded parent list is not the whole ancestry, and pretending
  otherwise produced false warnings on the contract check AND false
  unresolved-method hints on every method the generated role provides.
  `cst::string_list_with_residue` is the detection: same fold as
  `string_list`, plus a residue flag for any list item that didn't
  yield strings. Callers recording structural facts use the residue
  variant — silently dropping an unfoldable item presents a partial
  list as the whole truth.

Calibration (the diagnostic's done-criterion): zero hits across the
2,293-module substrate, zero false positives on the motivating
production codebase — where the raw first sweep produced 84, all from
the two patterns above.

## The reverse-edge bundle

`children_index` (parent class/role → modules whose packages
isa/compose it — inverse `package_parents`) is the fan-out's index,
walked transitively by `ModuleIndex::for_each_descendant_package`.
It lives in `ModuleEdgeIndexes` together with the names and bridges
maps: one struct, one `feed`/`purge`/`clear`, called by every
registration path (resolver insert, SQLite warm rebuild, workspace
registration). A reverse map fed on insert but not on rebuild serves
cold sessions and starves warm ones — paid twice before this bundle
made the divergence unrepresentable. New reverse edges go in this
struct, never as a free-standing map.
