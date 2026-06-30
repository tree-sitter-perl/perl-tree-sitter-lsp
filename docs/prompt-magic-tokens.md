# Resolve Perl's magic compile-time tokens to their values

Perl has a small closed set of compile-time `__TOKEN__`s that evaluate to a
value. Resolving each to its value in the canonical type/expression machinery
makes every downstream seam — method dispatch, column-keyed args, goto-def,
references, rename, hover — work for free, with no per-consumer handling. Today
only `__PACKAGE__` is partly wired, and unevenly.

## The tokens and their values

| Token | Value | Type to mint | Unlocks |
|---|---|---|---|
| `__PACKAGE__` | the enclosing package name | `ClassName(current_package)` | `__PACKAGE__->method` dispatch, column-keyed/ctor args, goto-def/rename on the class |
| `__SUB__` | a coderef to the enclosing sub | `CodeRef { return_edge: enclosing sub }` | `__SUB__->(…)` recursion: arg typing, return type, goto-def to the sub |
| `__FILE__` | the file path | `String` | hover/type only (minor) |
| `__LINE__` | the current line number | `Numeric` | hover/type only (minor) |

`__DATA__` / `__END__` are section markers, not value expressions — out of scope.

All four parse as `func0op_call_expression` (text = the token). `conventions.rs`
already models the invocant shape (`InvocantText::CurrentPackage` for
`__PACKAGE__`); the gap is **type resolution**, not recognition.

## The load-bearing one: `__PACKAGE__` → `ClassName(current_package)`

`invocant_type_at_node` already mints `ClassName(current_package)` for
`__PACKAGE__` in the constructor / bless / `mk_classdata` paths
(`builder.rs` ~2213). But it isn't resolved **uniformly**: the column-keyed-verb
dispatch (`method_call_invocant_class` / the arg-key seam) doesn't consult it, so
e.g. `__PACKAGE__->new({ name => 1 })` (a valid Row constructor) doesn't link its
`name` arg key to the column, and `__PACKAGE__->some_method` doesn't always reach
goto-def/rename.

**Fix shape (rule #10, single seam):** resolve `__PACKAGE__` to
`ClassName(current_package)` ONCE in the canonical expression/invocant typing —
`Builder::invocant_type_at_node` (build) and `FileAnalysis::expr_type_at_span`
(query, via an `Expr(span)` witness the walker emits for the token). The
enclosing package is known at build time (`current_package`) and recoverable at
query time (the package in scope at the node's span). Every consumer — the
column-keyed seam, method-call invocant resolution, goto-def, references, rename
— then reads the type and gets `__PACKAGE__` for free. Do NOT add a
`__PACKAGE__` special-case in each consumer.

`__SUB__` is the same shape one level over: emit a `CodeRef` whose `return_edge`
points at the enclosing sub's symbol, so the existing coderef-call typing resolves
`__SUB__->(args)` (recursion) and goto-def lands on the sub.

## What is NOT a gap (verified against real DBIC)

`__PACKAGE__->search` / `->create` / `->find` / `->resultset` on a Result class
are **errors** — those methods are resultset-only (`DBIx::Class::Core->can(...)`
is false for all of them; a resultset only comes from
`$schema->resultset('X')`). So not linking them is correct, not a gap — same
reason the `Class->search` fluent lift was dropped. The valid class-method cases
that the fix above closes: `__PACKAGE__->new({col})`,
`__PACKAGE__->insert`, `__PACKAGE__->add_columns` (already works), and any
user-defined `__PACKAGE__->method`.

## Scope note

Small, self-contained, and high-leverage (one seam, many consumers). Parked only
because the rename-bidirectional epic is already large; nothing here depends on
unlanded machinery.
