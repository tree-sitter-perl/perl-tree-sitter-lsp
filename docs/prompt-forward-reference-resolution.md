# Forward-reference resolution

**Status:** **LANDED.** Pinned test
`forward_reference_call_in_sub_return_resolves` (and four siblings —
implicit return, ternary arms, scoped-identifier call, self-method
tail) green in `builder_tests.rs`. Implemented as
`Builder::resolve_forward_call_targets`, called between
`populate_witness_bag` and `fold_to_fixed_point`. The remaining
sections below are kept as historical context for the diagnosis.

## The bug

```perl
sub longmess {
    if ($_[0]) { return longmess_heavy(@_); }
    else       { return longmess_heavy(@_); }
}

sub longmess_heavy { return "ouch"; }
```

`longmess`'s return type comes back as `None` instead of `String`. Reverse
the order — define `longmess_heavy` *before* `longmess` — and it resolves.
The bug is walk-order-dependent: forward-defined sub calls don't
contribute to the inference chain.

The canonical real-world case is Perl's own `Carp.pm`:
`longmess` (line 261) calls `longmess_heavy` (line 566).

## Asymmetry

Perl's name-resolution rules are not symmetric across kinds:

- **Subs have forward-reference semantics.** A sub call name resolves at
  symbol-table time, not parse time. `sub a { b() } sub b { … }` is
  legal and works.
- **Lexicals do not.** A `my $x` must appear before its use; the parser
  enforces this.

Our walk emits witnesses *during* the walk, which means walk-time
`self.symbols.iter().find(name)` is inherently order-dependent. That's
correct for lexicals (they couldn't have been used before declaration
anyway) but wrong for subs.

## Why D4-E removed the workaround

D3 had `emit_delegation_edges`, a post-walk pass that pushed
`Symbol(delegator) → Edge → Symbol(delegate)` for every recorded
`return foo()` shape. It ran after the full walk, so
`self.symbols.iter().find(delegate)` always succeeded — forward
references were never observed.

D4-E removed it on the premise that the bag-routed chain
`Symbol ← branch_arm Edge → Expr(body) → Edge(call_target)` would
subsume it. That premise held for ternary returns (`emit_expr_witness`
recurses) and works for backward-defined callees. It silently breaks
for forward-defined callees because `expr_payload`'s
`function_call_expression` arm (`builder.rs:3537-3546`) returns `None`
when the target sym doesn't exist yet, and no Edge gets pushed at all.

## What should land

A single post-walk "compile-esque" pass that performs all
definedness-dependent lookups against the now-final symbol table.

The walk-time emit path stays but is allowed to push placeholder
witnesses (or skip emission) for unresolved names; the post-walk pass
is the authoritative resolver. This generalizes beyond `expr_payload` —
audit every `self.symbols.iter().find` in the walk path and route them
through this pass.

Concretely:

1. **Track unresolved call-target lookups during the walk.** Whatever
   `expr_payload` would have emitted as `Edge → Symbol(sid)` for a
   `function_call_expression` / `bareword` / `scoped_identifier`, but
   couldn't because the sym didn't exist yet, gets queued as
   `(call_name, attachment_to_pin, body_span)`.
2. **Run the resolver post-walk**, between `populate_witness_bag` and
   `fold_to_fixed_point`. For each queued entry: re-look up the sym,
   push the Edge if found.
3. **Audit other sym-lookup walk paths** for the same
   forward-reference gap. Likely candidates: any
   `self.symbols.iter().find` in walk methods. Either route them
   through the same post-walk resolver, or document why the lookup is
   safe at walk time (e.g. it's a re-find against a sym we just
   pushed).

Not the right fix: re-adding `emit_delegation_edges` as a parallel
mechanism. That's the band-aid D4-E was right to remove — re-adding
it just moves the problem. The point is that walk-time symbol lookups
are unsafe in general and need a single principled resolver.

## Scope check before implementing

- Confirm the audit list. The only confirmed gap today is
  `function_call_expression` / `bareword` / `scoped_identifier` arms
  of `expr_payload`. If a wider survey turns up just this one site,
  the "compile-esque pass" framing might be over-engineering — a
  scoped fix on the call-target Edge alone could be enough.
- The pinned test is the Carp shape only. Once the fix lands, expand
  the test matrix to method calls (`$self->forward_method`),
  scoped-identifier calls (`Forward::name`), and ternary-return
  variants — those all flow through the same `expr_payload` arms.
- D2's `Edge(Expr(span))` emission already runs at walk time via
  `emit_expr_witness`; the fix is additive (re-emit on resolve), not a
  replacement. No bag-shape change required.
