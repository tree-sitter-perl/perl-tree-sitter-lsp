# Flow-sensitive narrowing

**Landed.** Decision record: `docs/adr/flow-narrowing.md`.
Executable spec / playground: `test_files/narrowing_playground.pl`.

## Residual forward work

### Subject coverage

- **Direct element places** (`$h{k}`, `$h[0]`) — only the *arrow* form
  (`$h->{k}`, `$h->[0]`) narrows today, because `canonical_place_path`
  requires a `scalar` base. The direct form's base is a
  `container_variable` (the named `%h` / `@h`), so it's rejected. Same
  soundness model (container + key stability), keyed on the named hash/
  array as the root — accept a `container_variable` base in
  `canonical_place_path`. (Smallest of the place extensions; do first.)
- **Dynamic-key places** (`$self->{$k}` where `$k` is a plain scalar) —
  narrowable: such a place is stable iff both the container `$self` *and*
  the key scalar `$k` are stable, so the delta is (a) let
  `canonical_place_path` accept a plain-scalar key (keyed by spelling,
  *not* an arbitrary key expression like `$self->{compute()}`) and (b)
  add one truncation rule — reassigning `$k` ends the region. Inherits
  the constant-key aliasing conservatism (a write via a *different* key
  spelling that equals `$k` at runtime doesn't truncate — already true
  for constant keys; a precision gap, never a crash).
  - *Tradeoff to decide at implementation:* alternatively treat **any
    dynamic-key write to the container** (`$self->{$j} = …`) as an
    *escape* that re-widens every narrowed slot of that container. That
    closes the aliasing hole (sound) at the cost of precision
    (over-truncates when the dynamic write hit a different key). Applies
    to constant-key places too — it's the general soundness-vs-precision
    knob for slot narrowing.
- **Accessor places** (`$self->name`) — parked. An accessor isn't a
  stable slot (it can return a different object per call, with side
  effects), so soundness needs a stricter no-call-between-guard-and-use
  model.

### Guard recognition

- **Const-folded class-name guards** (`$x->isa($CLASS)` with a folded
  `$CLASS`) — `cst::plain_string_literal_text` reads only literals, so a
  constant class name doesn't narrow. The fold lives on `Builder`
  (`resolve_constant_strings`), and recognizers are pure `(node, source)`
  free fns, so this needs fold state threaded into recognition (a
  recognizer-takes-`&Builder` change, not a move).

### Negation

- **elsif-chain cumulative negation** — the `else` of an `elsif` chain
  asserts the negation of *every* preceding condition; needs intersection
  across conditions.
- **General `Not` / `Difference` negation** — parked: no positive lookup
  target, no consumer value. "Not Foo" has nothing to dispatch on.

Diagnostics the lattice now enables: `docs/prompt-narrowing-diagnostics.md`.
