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
- **Rename through expression chains** — `$cfg->get_db()->prepare()` with
  cursor on `prepare`: rename requires resolving the chain to find the owning
  class. If `resolve_expression_type` gets it wrong, the rename silently hits
  the wrong method — and rename is destructive. The safe workflow is: goto-def
  (which *does* resolve through chains) → rename at the definition. All the
  plumbing exists (`resolve_expression_type` feeds rename the same way it feeds
  goto-def), so this is unlockable later once we have confidence in resolution
  accuracy. But rename is a killer feature of ours and we must not ship it
  broken.
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

**Gap: method call binding propagation.** Currently call bindings only
propagate for plain function calls (`my $cfg = get_config()`). Method
calls like `my $sum = $calc->add(2, 3)` also record a call binding
(via `extract_call_name`), but the post-pass resolves return types by
**name only** — `return_types.get(&binding.func_name)` — which is not
class-scoped. This works when sub names are unique, but will silently
pick the wrong return type when two classes define methods with the
same name and different return types.

Test case:

```perl
package Calculator;
sub add {
    my ($self, $a, $b) = @_;
    my $result = $a + $b;    # $result: Numeric (from + operator)
    return $result;           # add returns: Numeric
}
package main;
my $calc = Calculator->new();
my $sum = $calc->add(2, 3);  # $sum should be Numeric
```

Fix: the post-pass should build a class-scoped return type map
(`(class, method) → InferredType`) and look up bindings using the
invocant's resolved class. This requires storing the invocant info
in `CallBinding` so the post-pass can resolve it.

### Step 5: Wire into LSP features

**Done:**
- **Hover**: show inferred type on variables, return type on subs/methods.
  Usage-point aware — type updates after reassignment.
- **Completion**: method/hash key completion through expression chains
  (`get_config()->{host}`, `$obj->get_bar()->method()`).
- **Goto-def**: through expression chains + call-site named arg resolution
  (cursor on `verbose` in `Foo->new(verbose => 1)` → bless hash key def).

**Remaining — should be one-shot implementable:**

#### 5a. Inlay hints (`textDocument/inlayHint`)

Walk symbols in the visible range, emit type annotations for variables
with known types and subs with known return types:

```perl
my $cfg = get_config();       ← : HashRef
my $p = Point->new(x => 3);  ← : Point
my $sum = $calc->add(2, 3);  ← : Numeric
sub get_config { ... }        ← → HashRef
```

Implementation: new LSP handler, reads `inferred_type()` for variable
symbols and `SymbolDetail::Sub { return_type }` for sub symbols. Filter
to the requested range. No type system changes needed.

Suppression heuristics:
- Don't show on declarations where the RHS is a literal (`my $x = 42`)
  — the type is obvious
- Don't show on `$self` — always the enclosing class, noise
- Don't show `Numeric`/`String` — too noisy for scalar subtypes, only
  show Object/HashRef/ArrayRef/CodeRef/Regexp

#### 5b. Completion detail with return types

When building method completion candidates, include the return type in
the `detail` field:

```
add        → Numeric
get_config → HashRef
get_self   → Calculator
new        → Calculator
```

Implementation: in `complete_methods` and `complete_methods_for_class`,
look up `find_method_return_type(class, method)` or
`SymbolDetail::Sub { return_type }` and append to the detail string.
A few lines per completion builder.

#### 5c. Signature help with parameter types

Enrich the existing signature help with inferred types on parameters:

```
add($self: Calculator, $a: Numeric, $b: Numeric)
```

Implementation: for each param in the sub's signature, call
`inferred_type(param_name, sub_body_end_point)` — querying the type
at the end of the sub body captures all operator-based inference on
the param within the body. Append to the parameter label or use the
LSP `ParameterInformation.documentation` field.

Skip `$self` / `$class` types (noise — the user knows).

#### 5d. ArrayRef/CodeRef completion hints

After `$aref->` when type is ArrayRef, offer `[$0]` as a snippet
completion. After `$cref->` when type is CodeRef, offer `($0)`.

Implementation: in the `MethodOnExpression` and `Method` completion
branches, check the resolved type. If it's not Object, offer
type-appropriate snippets instead of methods:

| Type | Completion | insertText (snippet) |
|---|---|---|
| ArrayRef | `[index]` | `[$0]` |
| CodeRef | `(args)` | `($0)` |
| HashRef | `{key}` | `{$0}` |

Use `InsertTextFormat::Snippet` on the completion item.

#### 5e. Unresolved method diagnostic

If `$obj` is `Object(Foo)` and `Foo` is defined in the current file,
and `$obj->nonexistent()` is called where `nonexistent` doesn't exist
in Foo, emit a HINT diagnostic.

Guards:
- Only fire when the class is **locally defined** (not cross-file — avoid
  false positives for modules we haven't analyzed)
- Don't fire if the class has no methods at all (likely an external class
  with only a `new` we don't see)
- Don't fire for `AUTOLOAD`, `DESTROY`, `can`, `isa`, `DOES`, `VERSION`
  — universal methods
- Severity: HINT (not warning) — Perl's dynamic dispatch means we can't
  be sure

### Step 6: Context-aware argument completions

When completing inside a function/method call, use the expected parameter
type to synthesize and prioritize completions. The key insight is not
just sorting existing candidates — it's **synthesizing deref forms** that
bridge a variable's type to the expected context.

**Motivating example:**

```perl
push |, "item";   # completing first arg — push expects array context
```

Available in scope: `@items` (array), `$list` (ArrayRef), `$name` (String).
Completions should be:

```
@items       ← array, direct match — boosted
$list->@*    ← ArrayRef, synthesized deref — boosted
$name        ← String, irrelevant — demoted
```

The flow:

1. Detect call context: `push(` at param 0
2. Look up expected type for that param (builtin: push expects array)
3. For each variable in scope:
   - `@array` → direct match, boost
   - `$ref` with ArrayRef type → synthesize `$ref->@*`, boost
   - `$ref` with HashRef type → synthesize `%{$ref}` for hash-context
     builtins like `keys`, `values`, `each`
   - Unknown/mismatch → normal priority

**Synthesis rules:**

| Expected context | Variable type | Synthesized completion |
|---|---|---|
| array | ArrayRef | `$var->@*` |
| hash | HashRef | `$var->%*` |
| code | CodeRef | `$var->()` |
| scalar | ArrayRef | `scalar $var->@*` |
| Object(Foo) | Object(Foo) | `$var` (direct, boosted) |

**Builtin context signatures** (extend the type stubs):

| Builtin | Param 0 context | Param 1+ context |
|---|---|---|
| `push`, `pop`, `shift`, `unshift`, `splice` | array | scalar |
| `keys`, `values`, `each`, `delete`, `exists` | hash | — |
| `sort`, `reverse`, `grep`, `map` | — | array (last arg) |
| `chomp`, `chop` | scalar (String) | — |
| `print`, `say` | — | scalar |

**Complexity notes:**

- Requires merging `CallContext` (param position) with type info
  (variable types) during completion building
- Synthesis creates new `CompletionCandidate`s that don't correspond
  to a single symbol — they're composite (variable + deref operator)
- Need to handle both `$ref->@*` (postfix) and `@{$ref}` (block)
  deref forms — prefer postfix for modern Perl
- For user-defined subs, param context comes from the param's inferred
  type within the sub body (Step 5c)
- For builtins, param context comes from the type stub

Scope this after Steps 5a-5e — it builds on the same infrastructure
but adds the synthesis layer.

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

## Round 2: Cross-file Type Analysis

Depends on the module index infrastructure. Round 1 is same-file only;
Round 2 extends type information across file boundaries.

### Return types from module source

The module index already runs tree-sitter on module source in subprocesses
for export extraction. Once Round 1 return tracking works locally, running
the same `build()` pass on module source gives us return types for free.

Extend `ExportInfo` with `return_type: Option<InferredType>`. The resolver
subprocess already has the builder — just read `sub_return_type()` from
the resulting `FileAnalysis` and serialize it back.

SQLite schema: add `return_type TEXT` column (nullable, JSON-serialized).

### Type stubs for XS/opaque modules

Hardcoded overrides for common CPAN modules where source inference fails:

```
JSON::decode_json       → HashRef
JSON::encode_json       → String
DBI->connect            → Object(DBI::db)
DBI::db->prepare        → Object(DBI::st)
LWP::UserAgent->new     → Object(LWP::UserAgent)
HTTP::Response->code    → Numeric
File::Slurp::read_file  → String
Path::Tiny->new         → Object(Path::Tiny)
```

Initially hardcoded in a `HashMap`, later could become a data file or
user-configurable. Stubs override inference (for correctness on XS).

### Cross-file method completion

When `$obj` is `Object(Foo::Bar)` and `Foo::Bar` is not defined locally,
look up the module index for `Foo::Bar`'s methods. The index already has
export lists; extend to include method lists for OO modules.

### Workspace-wide rename

Once cross-file type resolution exists: rename a method across all files
that have `Object(ClassName)` variables calling `->method()`. Requires
the module index to track type info per-file, not just exports. This is
a significant extension — the index becomes a type database.

### Workspace-wide references

"Find all callers of `Foo->bar()`" across the workspace. Same
infrastructure as workspace rename but read-only.

## Round 3: OOP Intelligence (ongoing)

### Inheritance resolution (`@ISA` / `use parent` / `use base`)

Detect inheritance declarations in the builder:
- `use parent 'Foo'` / `use parent -norequire, 'Foo'`
- `use base 'Foo'`
- `our @ISA = ('Foo', 'Bar')`
- `extends 'Foo'` (Moose)
- `class Foo :isa(Bar)` (Perl 5.38)

Store parent chain on the class/package scope. Walk it during method
resolution — `complete_methods`, `find_method_in_class`,
`find_method_return_type` all gain parent-chain traversal.

Composes at `Object(ClassName)` — type constraints produce the class
name, inheritance tells you what's available on it.

### Moo/Moose/Mouse attribute tracking

```perl
has 'name' => (is => 'ro', isa => 'Str');
has 'config' => (is => 'rw', isa => 'HashRef');
has 'logger' => (is => 'ro', isa => 'InstanceOf["Log::Any"]');
```

Generates accessors: `$obj->name` (reader), `$obj->config` (reader +
writer). The `is => 'rw'` also generates `$obj->config($new_val)`.

Implementation: in the builder, pattern-match `has(...)` calls. Extract
the attribute name and options. Synthesize method symbols (like we do
for `:reader`/`:writer` on core class fields). The `isa` value gives
free type info — map to `InferredType` (see Type::Tiny section below).

### Type::Tiny / Types::Standard integration

**Design needed** — this is the richest source of explicit type info in
modern Perl, but the mapping to our type system needs work.

Type::Tiny is the standard type constraint library used with Moo/Moose.
Users literally write down types:

```perl
use Types::Standard qw(Str Int Num HashRef ArrayRef InstanceOf);

has 'name'   => (is => 'ro', isa => Str);
has 'count'  => (is => 'rw', isa => Int);
has 'config' => (is => 'ro', isa => HashRef);
has 'db'     => (is => 'ro', isa => InstanceOf['DBI::db']);
```

**Base type mapping (works today):**

| Type::Tiny | InferredType |
|---|---|
| `Str` | String |
| `Int`, `Num` | Numeric |
| `HashRef` | HashRef |
| `ArrayRef` | ArrayRef |
| `CodeRef` | CodeRef |
| `RegexpRef` | Regexp |
| `InstanceOf['Foo']` | Object(Foo) |
| `Object` | Object (unknown class) |

**Parametric forms (needs parametric types):**

| Type::Tiny | Meaning |
|---|---|
| `HashRef[Str]` | HashRef where values are Strings |
| `ArrayRef[InstanceOf['User']]` | ArrayRef of User objects |
| `Map[Str, Int]` | Hash with string keys, int values |
| `Tuple[Str, Int, Int]` | Fixed-length array with typed slots |

These need `InferredType` to grow parametric variants, which is
a significant extension. The base forms give us most of the value.

**Type::Params for function signatures:**

```perl
use Type::Params qw(signature);
method add (Num $a, Num $b) {   # Kavorka syntax
    return $a + $b;
}
# or
sub add {
    state $check = signature(
        method => 1,
        positional => [Num, Num],
    );
    my ($self, $a, $b) = $check->(@_);
}
```

This gives parameter types directly. The `signature()` pattern is
parseable (tree-sitter sees the function call + args), and the types
map to our existing type system.

**Open questions:**
- Should we parse the Type::Tiny import to know which type names are
  available, or hardcode the Types::Standard set?
- How to handle custom type libraries (`use MyApp::Types qw(...)`)
- Coercions (`coerce HashRef, from Str, via { decode_json($_) }`)
  — probably ignore for now
- `Optional[Str]` → nullable? We don't have nullable yet

### Role composition (Moose roles / Role::Tiny)

```perl
with 'Printable', 'Serializable';
```

Roles provide methods like inheritance but via composition. Treat
`with` like `use parent` for method resolution purposes — the role's
methods are available on the consuming class.
