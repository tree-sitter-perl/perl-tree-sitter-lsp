# Roadmap

## Current state

Single-file LSP with hand-rolled scope resolution. Supports:
- documentSymbol, goto-definition, references, hover, rename
- Scope-aware variable resolution (my, our, state, signatures, for-loops, class fields)
- Core Perl `class` support (5.38+): class extraction, field/method symbols, type-inferred method go-to-def
- Simple type inference: `my $obj = ClassName->new(...)` → method calls on `$obj` resolve to that class
- Bareword invocants in method calls treated as class names (refs, rename work)
- String interpolation, heredocs: variables inside `"$foo"` and `<<END` bodies tracked correctly (tree-sitter-perl handles this natively)
- Completion: scope-aware (variables, subs, methods from current scope and class context)

## Known issues to investigate

### Reparse reliability

Intermittent reports of go-to-definition failing on valid code until file is re-saved. Possible causes:
- `didChange` with `TextDocumentSyncKind::FULL` should trigger full reparse, but incremental parse (`parser.parse(&new_text, Some(&self.tree))`) may behave oddly if the old tree doesn't match the new text
- Race condition: rapid edits may cause tree/text desync if `did_change` fires while a previous parse is still running (though tree-sitter parsing is fast and synchronous)
- Editor-specific: some editors batch changes or delay `didChange` notifications

**Next steps**: Add debug logging around `did_change`/`did_save` to capture when parses happen and whether the tree is consistent. Consider always doing a fresh parse (pass `None` for old tree) as a safety net.

## Phase 1: Scopegraphs (prerequisite for multi-file)

Replace the hand-rolled scope walking (`find_variable_def_walk`, `enclosing_scope`, per-construct special cases) with the [scopegraphs](https://docs.rs/scopegraphs) crate.

**Why**: Each new scoping construct (for-loops, signatures, class fields) currently needs its own special case. Scopegraphs encodes scoping rules as graph edges, making it declarative and extensible. It also naturally supports cross-scope references (class inheritance, package imports) which the current approach can't do.

**What changes**:
- Build a scope graph from the tree-sitter CST: one scope node per block/sub/class/package, edges for declarations and references
- `find_definition` becomes a graph query instead of a tree walk
- Cross-file: scope graphs from different files connect via import/export edges

**Scoping constructs to encode**:
- `my`/`our`/`local`/`state` → lexical scope = enclosing block
- `sub`/`method` signatures → scope = sub body
- `for my $var` → scope = loop body
- `class` fields → scope = class block
- `package` → scope = until next package or EOF
- `use` → import edge from module's export scope

## Phase 2: Type inference

Separate from scope resolution. Scopegraphs answer "which `$x` am I looking at?"; type inference answers "what value does that `$x` hold?"

**Levels** (implement incrementally):

1. **Pattern matching** (current): `my $obj = Foo->new(...)` → `$obj` is `Foo`
2. **Return type tracking**: `my $obj = get_widget()` → if `get_widget` has a known return type (annotation, single return of `Foo->new`), propagate it
3. **Parameter type tracking**: `sub foo ($x) { $x->method() }` → if all call sites pass a `Foo`, infer `$x` is `Foo`
4. **Container types**: `push @list, Foo->new(...); $list[0]->method()` → track element types

Each level builds on scopegraphs being in place (need to resolve names before tracking what flows through them).

## Phase 3: Pluggable OOP framework stubs

One stub file per OOP framework. The stub describes the framework's DSL — what functions it exports and what they mean for a class's shape. The LSP applies these rules to any package that `use`s the framework.

**Format**: Perl-like syntax + `# <-` annotations (same style as tree-sitter highlight tests).

```perl
# stubs/moo.framework
# trigger: use Moo

has $FIELD_NAME => (
  is => 'ro',   # <- creates_reader
  is => 'rw',   # <- creates_reader, creates_writer
);

extends '$CLASS_NAME';
# <- sets_parent

with '$ROLE_NAME';
# <- adds_role

# <- provides: new
```

**How it works**:
- `# trigger:` declares what `use` statement activates the framework
- Metavariables (`$FIELD_NAME`, `$CLASS_NAME`) are capture patterns
- `# <-` annotations map patterns to OOP semantics
- `# <- provides:` declares auto-created methods (like `new`)
- Stub files are parseable by tree-sitter-perl (valid-ish Perl)

**Discovery**:
- Built-in stubs ship in `stubs/` for Moo, Moose, Class::Accessor
- Users can add custom stubs in `.perl-lsp/stubs/`
- The LSP scans `use` statements and matches against trigger patterns

**Resolution**: Framework stubs produce the same `ClassInfo` structure as core classes, so method resolution, hover, and go-to-def work identically regardless of OOP style.

## Phase 4: Multi-file resolution

Requires scopegraphs (phase 1) + framework stubs (phase 3).

- Index all project files, build per-file scope graphs
- Connect graphs via `use`/`require` import edges
- `Foo->method()` resolves across files: find `Foo`'s file, look up method
- Workspace-wide references and rename
