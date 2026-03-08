# Type Constraint Discovery — Round 1

## Motivation

Perl is dynamically typed but **operator-oriented**: the operators you use
on a value reveal what type it must be. `$x->{key}` means `$x` is a
hashref. `$x->[0]` means arrayref. `$x->method()` means object. This
is free type information — we just have to collect it.

Currently the LSP infers types in 3 cases:

1. `my $obj = Foo->new()` → `$obj` is `Object(Foo)` (constructor)
2. `my ($self) = @_` in package Foo → `$self` is `Object(Foo)` (first-param)
3. `bless {}, 'Foo'` → result is `Object(Foo)`

Round 1 expands this significantly.

## Type Taxonomy

```
InferredType
├── Object(ClassName)         # blessed reference — has methods
│   ├── ClassName(String)     # from Foo->new()
│   ├── FirstParam(String)    # from my ($self) = @_ in package
│   └── BlessResult(String)   # from bless {}, 'Foo'
├── HashRef                   # unblessed hash reference
├── ArrayRef                  # unblessed array reference
├── CodeRef                   # code reference
├── Regexp                    # compiled regex (qr//)
├── Numeric                   # used in numeric context
└── String                    # used in string context
```

### Type priority (for conflicts)

When a variable has multiple constraints, Object wins over
HashRef/ArrayRef (overloaded objects are common in Perl).
Numeric and String can coexist (Perl coerces freely).

Re-assignment creates a new constraint at the new span — the latest
constraint before the query point wins (already how `inferred_type` works).

## Discovery Rules

### Tier 1: Literal constructors (from RHS of assignment)

| Pattern | Type |
|---|---|
| `$x = {}` or `$x = { ... }` | HashRef |
| `$x = []` or `$x = [ ... ]` | ArrayRef |
| `$x = sub { ... }` | CodeRef |
| `$x = qr/.../` | Regexp |
| `$x = Foo->new(...)` | Object(Foo) — **already implemented** |
| `$x = bless ...` | Object — **already implemented** |

### Tier 2: Operator-based inference (from usage)

**Reference dereference (strongest signal):**

| Pattern | Constraint on `$x` |
|---|---|
| `$x->{...}` | HashRef (or Object, if method calls also seen) |
| `$x->[...]` | ArrayRef |
| `$x->(...)` | CodeRef |
| `$x->method()` | Object — **already implemented** |
| `@{$x}`, `$x->@*` | ArrayRef |
| `%{$x}`, `$x->%*` | HashRef |
| `&{$x}` | CodeRef |

**Numeric operators (weaker signal, scalar subtype):**

`+`, `-`, `*`, `/`, `%`, `**`, `++`, `--`,
`==`, `!=`, `<=>`, `<`, `>`, `<=`, `>=`,
`abs()`, `int()`, `sqrt()`

**String operators (weaker signal, scalar subtype):**

`.`, `x`, `eq`, `ne`, `lt`, `gt`, `le`, `ge`, `cmp`,
`uc()`, `lc()`, `length()`, `substr()`, `index()`,
`chomp`, `chop`, `=~`, `!~`

### Tier 3: Return value tracking

For subs/methods defined in the same file, examine `return` statements:

```perl
sub get_config {
    return { host => "localhost" };    # → HashRef
}

my $cfg = get_config();               # $cfg → HashRef
```

**Rules:**
- Collect types from all `return` statements + the implicit last expression
- If all returns agree on a type, that's the return type
- If returns disagree (e.g., hashref in one path, arrayref in another), return type is Unknown
- If a return is `$self` or a constructor call, the return type is Object(ClassName)
- If a return expression is a variable, use its type constraint at that point
- Store return type as a property of the Sub/Method symbol: `SymbolDetail::Sub { return_type: Option<InferredType> }`

**What to skip during return type aggregation:**
- Bare `return` (no argument) — exit, not a type signal
- `return undef` — not useful for type inference, skip
- `die`, `croak`, `confess`, `Carp::croak` — exceptions, not returns

**Resolution (decoupled, zero CST):**
```
fn resolve_return_type(return_types: &[InferredType]) -> Option<InferredType>
```
- All agree → that type
- Object subsumes HashRef (overloaded objects)
- Disagreement → None (Unknown)

**Propagation — two paths:**

1. **Assignment binding:** `my $cfg = get_config()` — during the walk,
   `visit_assignment` records `(lhs_var, func_name, span)` when the RHS
   is a function call. The post-pass resolves the function's return type
   and pushes a type constraint on the LHS variable. Structural, not
   heuristic — the assignment node has both sides.

2. **Inline expression:** `get_config()->{host}` — no variable involved.
   The invocant of `->{}` is a function call, not a scalar. When resolving
   hash key owners or method targets, if the invocant is a call expression,
   look up the callee's return type and use it as the owner/class. This
   enables completion on return values without intermediate variables:
   ```perl
   get_config()->{host}          # hash key completion from return type
   get_config()->{db}->connect() # chained: return type → hash key → method
   build_user()->name()          # method completion from return type
   ```
   Implementation: extend `resolve_hash_key_owner` and `resolve_invocant_class`
   to handle call expression invocants by looking up the callee in the
   return type map.

### Tier 3b: Cross-file return types (forward-looking)

The module index already caches exports for cpanfile deps. Extend to
include return types:

1. **`ExportInfo` struct** — extend with `return_type: Option<InferredType>`
2. **Inference from source** — the resolver subprocess already runs the
   builder on module source. Once Tier 3 local return tracking works,
   cross-file inference comes for free.
3. **Type stubs for XS/opaque modules** — hardcoded overrides for common
   CPAN modules where source inference fails:
   - `JSON::decode_json` → HashRef
   - `JSON::encode_json` → String
   - `DBI->connect` → Object(DBI::db)
   - `File::Slurp::read_file` → String
   - etc. (initially hardcoded, could become a data file)
4. **SQLite schema migration** — add `return_type TEXT` column (nullable,
   JSON-serialized InferredType)
5. **Priority** — stubs override inference (for correctness on XS modules)

### Tier 4: Builtin return types (hardcoded)

| Builtin | Return type |
|---|---|
| `time`, `length`, `scalar @arr`, `index`, `rindex` | Numeric |
| `join`, `uc`, `lc`, `substr`, `ref`, `sprintf` | String |
| `sort`, `reverse`, `grep`, `map`, `keys`, `values` | Array |
| `pop`, `shift` | Element type (if known) |

### Return type modifiers (future — not in Round 1)

Two properties of return types that are worth tracking but whose UX
needs design work before implementing:

**Nullable** — a sub can return early with bare `return` or `return undef`:

```perl
sub find_user {
    return unless $id;          # nullable: bare return
    return User->new(id => $id);
}
# return type: Object(User), but nullable
```

Currently bare returns are filtered out during aggregation (the typed
return wins). A future step could add a `nullable: bool` flag to the
return type metadata, signaling that the caller should handle the
`undef` case. Open question: should this show as a hover annotation?
A diagnostic? A different completion behavior?

**Throwable** — a sub can exit via `die`, `croak`, `confess`, etc.:

```perl
sub must_get {
    return $cache{$key} if exists $cache{$key};
    die "not found: $key";
}
# return type: Unknown, but throwable
```

Currently `die`/`croak`/`confess` are naturally excluded (they're
function calls, not `return_expression` nodes). A future step could
track a `throws: bool` flag, enabling diagnostics like "this call
can throw — consider wrapping in eval {}". Open question: what level
of certainty is useful? Almost any Perl sub can die.

Both modifiers are orthogonal to the type itself and to each other.
They should be flags on `SymbolDetail::Sub`, not variants of
`InferredType`.

## What we explicitly DON'T do in Round 1

- **Cross-file type propagation** — types from `use`d modules stay in module_index territory
- **Union types** — `$flag ? {} : []` is Unknown, not `HashRef | ArrayRef`
- **Overload detection** — we don't check `use overload`. If `$x->{key}` and `$x->method()` both appear, Object wins
- **Context-sensitive returns** — no `wantarray` tracking
- **Parametric types** — no "ArrayRef of HashRef of String"
- **Nullable / throwable return modifiers** — see section above
- **Tied variables** — ignored
- **eval/string code** — ignored

## Implementation Plan

### Step 1: Expand `InferredType` enum

Add variants: `HashRef`, `ArrayRef`, `CodeRef`, `Regexp`, `Numeric`, `String`.

### Step 2: Literal constructor detection

In `visit_assignment`, when the RHS is `{}`, `[]`, `sub {}`, or `qr//`,
push a type constraint on the LHS variable. This extends the existing
constructor detection (`Foo->new()`) to cover reference literals.

### Step 3: Operator-based inference

In the builder's expression visitors, when we see dereference operators
(`->{}`, `->[]`, `->()`, `@{}`, `%{}`, `&{}`), push a type constraint
on the operand variable. For arithmetic/string operators, push
Numeric/String constraints.

### Step 4: Return value tracking

Post-pass after the tree walk: for each Sub/Method symbol, find all
`return` statements in its scope, infer the return type, and store it.
Then, for assignments where the RHS is a function call to a local sub,
propagate the return type to the LHS variable.

### Step 5: Wire into LSP features

- **Completion**: after `$x->` if `$x` is HashRef, offer hash keys seen on `$x`; if ArrayRef, nothing special; if Object, offer methods (already works)
- **Hover**: show inferred type in hover info
- **Diagnostics** (future): warn on `$hashref->[0]` (HashRef used as ArrayRef)

## Obstacle Course

See `test_files/type_obstacle_course.pl` — a Perl file covering all patterns
with annotated expected types. Use this for TDD: write unit tests that
parse the file and assert the expected constraints, then implement.

## LSP Impact

| Feature | Current | After Round 1 |
|---|---|---|
| `$href->` completion | nothing | hash keys of `$href` |
| `$obj->` completion | methods (if type known) | methods (more types known) |
| `$aref->` completion | nothing | `[idx]` hint |
| hover on variable | declaration line | declaration + inferred type |
| `$x = get_config(); $x->` | nothing | hash keys (from return type) |
| diagnostics | unresolved functions only | + type mismatch (future) |
