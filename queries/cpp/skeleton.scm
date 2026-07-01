; C++ language pack — Tier 1 (ring-1 skeleton): outline, scopes,
; namespaces, includes, calls. The capture vocabulary is the
; language-neutral contract the driver reads; node kinds are C++'s.
;
; What the preprocessor costs this tier is MEASURED by the obstacle
; course (cpp_obstacle.rs): declaration-generating macros (ring 3) are
; invisible here by construction, and declarator-position macros
; corrupt the parse (a `class API_EXPORT Foo` reparses as a function) —
; that damage is the input to the preprocessing-design question, not a
; bug in these patterns.

; ---- includes: the import edge (header path is the module name).
; capture the string CONTENT for quoted paths so the cache key is the
; clean relative path ("util.h", not "\"util.h\""); system <...>
; headers have no content node, so keep the whole token. ----
(preproc_include path: (string_literal (string_content) @import.name))
(preproc_include path: (system_lib_string) @import.name)

; ---- #define macros become SYMBOLS (completion / goto-def / outline).
; For a macro-heavy API (perl5: Newx/SvPV; embedded HALs) the macros ARE
; the surface. Object-like (`#define MAX 1`) → a constant (Variable);
; function-like (`#define MIN(a,b) ...`) → a callable (Sub).
(preproc_def name: (identifier) @def.var.name) @def.var
(preproc_function_def name: (identifier) @def.sub.name) @def.sub
; An object-like macro whose body is a bare type spelling is a TYPE ALIAS the
; same as a `typedef` — `#define PERL_BITFIELD16 U16` / `#define BITF16 unsigned`.
; The alias graph resolves it (incl. cross-file, since the #define is a
; file-scope symbol): a field typed `PERL_BITFIELD16` in another header chases
; through to `unsigned short`. Non-type bodies (`#define MAX 100`) are gated out
; at emission by `annot_type`.
(preproc_def name: (identifier) @macro.alias.name value: (preproc_arg) @macro.alias.of)

; ---- namespaces: a Package SYMBOL (so its members nest under it in the
; outline) + a sticky context + a real scope for its body ----
(namespace_definition
  name: (namespace_identifier) @def.package.name @context.namespace
  body: (declaration_list) @scope) @def.package

; ---- type defs: class / struct / union / enum ----
; @context.class tags the body's members with the class name (package),
; so member completion (`obj.`) and symbol_in_class resolve them.
(class_specifier
  name: (type_identifier) @def.class.name @context.class
  body: (field_declaration_list) @scope) @def.class
(struct_specifier
  name: (type_identifier) @def.class.name @context.class
  body: (field_declaration_list) @scope) @def.class

; out-of-line nested class definition `class Outer::Inner { ... }` — the
; name is a qualified_identifier; @qualifier carries the `Outer::` owner.
(class_specifier
  name: (qualified_identifier
    scope: (_) @qualifier
    name: (type_identifier) @def.class.name @context.class)
  body: (field_declaration_list) @scope) @def.class

; ---- inheritance: `class Circle : public Shape` → Circle parent Shape.
; A dedicated pattern (non-inheriting classes keep matching the body
; pattern above); one @parent per base, so multiple inheritance works.
(class_specifier
  name: (type_identifier) @def.class.name
  (base_class_clause (type_identifier) @parent))
(struct_specifier
  name: (type_identifier) @def.class.name
  (base_class_clause (type_identifier) @parent))
(union_specifier name: (type_identifier) @def.class.name) @def.class
(enum_specifier name: (type_identifier) @def.class.name) @def.class

; ---- C typedef'd aggregates: `typedef struct { ... } Name;` — the
; dominant C type idiom (the anonymous struct has no name of its own, so
; the typedef NAME is the type). The name comes AFTER the body, so
; @context.class can't tag the members (already walked) — a body-scope
; post-pass in into_file_analysis does that instead.
(type_definition
  type: (struct_specifier body: (field_declaration_list) @scope)
  declarator: (type_identifier) @def.class.name) @def.class
(type_definition
  type: (union_specifier body: (field_declaration_list) @scope)
  declarator: (type_identifier) @def.class.name) @def.class
(type_definition
  type: (enum_specifier)
  declarator: (type_identifier) @def.class.name) @def.class
; enum CONSTANTS (RED, GREEN) — named values, findable + completable.
; @def.enumerator (not @def.var) marks them so the extractor can tag each
; with its parent enum (span-contained): `enum Color { RED }` → RED's
; container + type is `Color`, so hover renders `RED: Color` the same
; `name: type` way a struct field does. They stay in the ENCLOSING scope
; (C enumerators leak out of the enum body — no @scope), so a bare
; `x = RED` still resolves to the enumerator.
(enumerator name: (identifier) @def.enumerator.name) @def.enumerator
; scalar / function-pointer typedefs: `typedef uint32_t u32;`,
; `typedef void (*CB)(int);` — the named alias is a findable type. (The
; struct/union/enum forms above already matched with a @scope; the
; name-dedup in into_file_analysis collapses the overlap.)
(type_definition
  declarator: (type_identifier) @def.class.name) @def.class
(type_definition
  declarator: (function_declarator
    declarator: (pointer_declarator
      declarator: (type_identifier) @def.class.name))) @def.class

; ---- scalar / primitive / alias-chain typedefs → the alias EDGE. ----
; `typedef unsigned short U16;`, `typedef uint32_t u32;`, `typedef V16 W16;`
; — the underlying is a SCALAR (a struct/union/enum, bodied or bare tag,
; aliases through @parent above, so it's excluded here). @alias.of carries
; the underlying type TEXT, joined to @alias.name by match; the extractor
; mints a `TypeName(alias) → <underlying>` witness so a declared `U16 x;`
; chases the alias to its leaf type (`unsigned short`) for hover / typing.
(type_definition
  type: [(primitive_type) (sized_type_specifier) (type_identifier)] @alias.of
  declarator: (type_identifier) @alias.name)
; C++ `using U16 = unsigned short;` — same alias, `alias_declaration` shape.
(alias_declaration
  name: (type_identifier) @alias.name
  type: (type_descriptor) @alias.of)

; typedef of a NAMED tag whose body is elsewhere: `typedef struct op OP;`
; (perl5's dominant idiom — `struct op` is defined in op.h, OP is the public
; name). OP is an ALIAS for the tag, so record the tag as OP's @parent: member
; completion + goto-def then see through the alias to `struct op`'s fields via
; the cross-file ancestor walk. (The bodied forms above already give the tag
; its own members; this only adds the alias edge.)
(type_definition
  type: (struct_specifier name: (type_identifier) @parent)
  declarator: (type_identifier) @def.class.name) @def.class
(type_definition
  type: (union_specifier name: (type_identifier) @parent)
  declarator: (type_identifier) @def.class.name) @def.class
(type_definition
  type: (enum_specifier name: (type_identifier) @parent)
  declarator: (type_identifier) @def.class.name) @def.class

; ---- free functions & out-of-line / inline method definitions ----
; the name lives at the bottom of the declarator chain; one pattern per
; shape it can take (plain / member / qualified / pointer-return).
;
; @scope is minted SEPARATELY, by the universal `(function_definition) @scope`
; below — one pattern for EVERY body shape (operator[]/operator=/conversion
; operators/constructors/destructors/templated/out-of-line), so no function's
; body ever leaks into the enclosing class scope. The name patterns here only
; carry @def; they no longer double as the scope source (which missed the
; operator/cast/in-class-destructor declarator shapes).
(function_definition
  declarator: (function_declarator
    declarator: (identifier) @def.sub.name)) @def.sub
(function_definition
  type: (_) @rettype
  declarator: (function_declarator
    declarator: (field_identifier) @def.method.name)) @def.method
; out-of-line definition `RetT Class::method(...) { ... }` — @qualifier
; carries the `Class::` so the method attributes to its class, not the
; enclosing namespace.
(function_definition
  type: (_) @rettype
  declarator: (function_declarator
    declarator: (qualified_identifier
      scope: (_) @qualifier
      name: (identifier) @def.method.name))) @def.method
(function_definition
  declarator: (pointer_declarator
    declarator: (function_declarator
      declarator: (identifier) @def.sub.name))) @def.sub

; every function body is a lexical scope — one node-kind, so operator methods
; (`operator[]`/`operator=`), conversion operators (`operator bool()`),
; constructors (with or without member-init lists), destructors (in-class
; `~S()` + out-of-line `S::~S()`), templated methods, and out-of-line
; `Ret Class::m()` bodies ALL mint a @scope, not just the plain/field/qualified
; declarator shapes the name patterns above enumerate. Params sit inside the
; function_definition span, so they scope to the function (drives declared-type
; inference); the scope-based moved-from region + narrowing cutoff no longer
; leak across scope-less sibling functions.
(function_definition) @scope

; ---- top-level / namespaced function prototypes (the bulk of any
; header file) — a `declaration`, not a `function_definition` ----
(declaration
  declarator: (function_declarator
    declarator: (identifier) @def.sub.name)) @def.sub
(declaration
  declarator: (function_declarator
    declarator: (qualified_identifier
      scope: (_) @qualifier
      name: (identifier) @def.method.name))) @def.method

; ---- in-class method declarations (prototypes) & member fields ----
; @rettype carries the declared return type → the method's return-type
; witness (drives `box.getInner().` chaining through MethodOnClass).
(field_declaration
  type: (_) @rettype
  declarator: (function_declarator
    declarator: (field_identifier) @def.method.name)) @def.method
; pointer- / reference-returning methods (`Foo* m()`, `Foo& m()`):
; the function_declarator nests inside a pointer/reference wrapper.
(field_declaration
  type: (_) @rettype
  declarator: (pointer_declarator
    declarator: (function_declarator
      declarator: (field_identifier) @def.method.name))) @def.method
(field_declaration
  type: (_) @rettype
  declarator: (reference_declarator
    (function_declarator
      declarator: (field_identifier) @def.method.name))) @def.method
; destructor `~Widget()` — tree-sitter parses it as a `declaration` (no
; return type), with a `destructor_name` declarator, so the field_declaration
; method patterns above miss it. @def.sub + the in-class method
; reclassification make it a Method. (Constructors are field_identifiers
; and already match.)
(declaration
  declarator: (function_declarator
    declarator: (destructor_name) @def.sub.name)) @def.sub
; out-of-line `Class::~Class() {...}` / `Class::Class() {...}` definitions.
; (@scope comes from the universal `(function_definition) @scope` above.)
(function_definition
  declarator: (function_declarator
    declarator: (qualified_identifier
      scope: (_) @qualifier
      name: (destructor_name) @def.method.name))) @def.method

(field_declaration
  declarator: (field_identifier) @def.var.name) @def.var
; a data member's TYPE — the type witness needs field_declaration (the
; `declaration` patterns below only see locals). Only plain-field
; declarators match (a function_declarator is a method, not a field), so
; member-access chains (`box.inner.`) can type `inner` on its class.
(field_declaration
  type: (_) @type.annot
  declarator: (field_identifier) @flow.target)
; pointer / reference data members of any depth (`Box* inner;`, `Node** next;`).
; The leaf is a field_identifier, so core mints a @def.var (a class member),
; not a @def.local — `peel_nested`'s leaf→def-capture map handles that.
(field_declaration
  type: (_) @type.annot
  declarator: [(pointer_declarator) (reference_declarator)] @nested.target)

; ---- C goto labels: `done:` is a nav target, `goto done;` jumps to it.
; The def is an unpackaged Variable symbol (outline-hidden, like a local);
; the goto resolves to it function-wide (order-independent — forward gotos).
(labeled_statement label: (statement_identifier) @def.label)
(goto_statement label: (statement_identifier) @ref.label)

; ---- calls ----
(call_expression function: (identifier) @ref.call) @expr.call

; ---- member access (`recv.field` / `recv->field`, AND `recv.method(...)`):
; the field is the "method", the receiver subtree the invocant. Mints the same
; MethodCall ref core resolves for Perl `$obj->m` — goto-def / hover /
; references / rename all flow from it. @member.recv carries the receiver
; (span+text) for query-time typing via expr_type_at_span. The trailing `()`
; of a method call doesn't change the reference, so calls + plain field access
; share one pattern.
(field_expression
  argument: (_) @member.recv
  operator: _ @member.op
  field: (field_identifier) @ref.member)

; ---- type witnesses: C++ leaks types at every DECLARATION site (its
; static-typing richness — the annot_type predicate carries the load).
; `T x = init;` emits both the declared-type witness and a flow edge to
; the initializer; `T x;` emits the declared type alone. `auto` defers
; to the edge (annot_type returns None), driving the cross-var chase. ----
(declaration
  type: (_) @type.annot
  declarator: (init_declarator
    declarator: (identifier) @flow.target @def.local
    value: (_) @flow.source))
(declaration
  type: (_) @type.annot
  declarator: (identifier) @flow.target @def.local)

; pointer / reference locals of ANY depth, bare and initialized — `T* p;`,
; `T** pp;`, `T& r = x;`, `if (Derived* d = dynamic_cast<...>(b))`. The
; @nested.target chain is unravelled by core (see params). The init form also
; carries @flow.source so the initializer's type flows to the leaf.
(declaration
  type: (_) @type.annot
  declarator: [(pointer_declarator) (reference_declarator)] @nested.target)
(declaration
  type: (_) @type.annot
  declarator: (init_declarator
    declarator: [(pointer_declarator) (reference_declarator)] @nested.target
    value: (_) @flow.source))

; ---- function PARAMETERS carry a type too (the dominant embedded site:
; `void f(Handle *h) { h->... }`). Value / pointer / reference forms;
; pointer-/reference-ness dropped for navigation, like locals. ----
(parameter_declaration
  type: (_) @type.annot
  declarator: (identifier) @flow.target @def.local)
; pointer / reference parameters of ANY depth — `Handle* h`, `OP** op_p`,
; `char**** x`, `Box*& rp`. One @nested.target capture; core (`peel_nested`)
; unravels the declarator chain to the leaf identifier + the deref stack
; (arbitrary depth, cv-qualifiers per level), emitting the leaf as a synthetic
; @flow.target/@def.local. The plain-value form above has no chain.
(parameter_declaration
  type: (_) @type.annot
  declarator: [(pointer_declarator) (reference_declarator)] @nested.target)

; ---- literals + variable reads (the edge-chase substrate) ----
(number_literal) @expr.lit.number
(string_literal) @expr.lit.string
(identifier) @expr.read.var

; ---- bind shapes + guard narrowing (the value-flow tier, cpp side) ----

; `for (auto x : items)` — the range-for var rebinds per element (a Rebind, no
; inflowing type yet) so the narrowing cutoff ends a region at the loop.
(for_range_loop
  declarator: (identifier) @flow.rebind)

; a plain `x = rhs` reassignment rebinds x with the rhs value — mints a Whole
; FlowEdge (like a declaration's init) so the moved-from region AND the
; narrowing cutoff end at the reassignment, via the same edge-driven cutoff.
(assignment_expression
  left: (identifier) @flow.target
  right: (_) @flow.source)

; `std::move(x)` leaves x in a moved-from (valid-but-unspecified) state: a
; subsequent READ of x before it is reassigned is a use-after-move bug.
; Capture the moved var + the whole call span; the minter checks scope/name
; against std/move (the driver has no query predicates). The moved-from region
; runs from the call to the first @flow rebind of x (or scope end) — the same
; cutoff the narrowing tier uses (`earliest_rebind_in`).
(call_expression
  function: (qualified_identifier
    scope: (namespace_identifier) @move.scope
    name: (identifier) @move.name)
  arguments: (argument_list (identifier) @move.var)) @move.call

; unevaluated operands — `noexcept(...)` / `sizeof(...)` / `decltype(...)`
; don't RUN their operand, so a `std::move` inside one never moves anything.
; Extraction records these regions and drops moves whose call sits inside one
; (the noexcept-specifier `noexcept(noexcept(T(std::move(b))))` is the dominant
; real-world spelling — the move there is a type-trait, not a move).
(noexcept) @unevaluated
(sizeof_expression) @unevaluated
(decltype) @unevaluated

; `if (dynamic_cast<Derived*>(b)) { b->... }` narrows b to Derived INSIDE the
; block — the cpp analog of python `isinstance`. The pack's narrow_guard maps
; `dynamic_cast` + the template type to the refinement; core scopes it to
; @scope and the edge-driven cutoff ends it at any rebind of b.
(if_statement
  condition: (condition_clause
    value: (call_expression
      function: (template_function
        name: (identifier) @narrow.guard
        arguments: (template_argument_list
          (type_descriptor type: (type_identifier) @narrow.type)))
      arguments: (argument_list (identifier) @narrow.var)))
  consequence: (compound_statement) @narrow.block)

; `std::optional<T>` engaged-state narrowing. Guard-testing an optional as
; engaged proves it HOLDS a T inside the block, so `opt->m` / `*opt` resolve on
; T there. No type token rides these guards (unlike dynamic_cast) — the pack's
; narrow_guard reads the subject's DECLARED type (std::optional<T>) and peels T,
; so the refinement keys on the type being optional, not on the guard name (a
; bare `if (ptr)` over a non-optional declares no inner type → no narrowing).
; Two clean engagement shapes: bare truthiness `if (opt)` (no @narrow.guard),
; and `if (opt.has_value())` (guard token gates the method — an arbitrary
; `opt.foo()` won't narrow). `!= std::nullopt` needs both operator + operand
; checks the one-token hook can't express, so it's left out.
(if_statement
  condition: (condition_clause value: (identifier) @narrow.var)
  consequence: (compound_statement) @narrow.block)
(if_statement
  condition: (condition_clause
    value: (call_expression
      function: (field_expression
        argument: (identifier) @narrow.var
        field: (field_identifier) @narrow.guard)))
  consequence: (compound_statement) @narrow.block)

; ---- branch arms are lexical scopes (conditional-move soundness) ----
; if/else arm bodies each mint a @scope, so a `std::move` in one arm bounds its
; moved-from region to THAT arm — a read in a sibling arm (or after the if) is
; in a different scope subtree and never false-flags. This is ALSO the scope a
; guard narrowing above attaches to: extraction joins the @narrow.block (the
; condition-tagged consequence) to the general arm @scope by block position, so
; the block mints exactly ONE scope (no fragile duplicate). Switch cases are not
; compound_statements, so a per-case region is a residual (a move+read across
; two `case:` labels still shares the switch-body scope).
(if_statement consequence: (compound_statement) @scope)
(if_statement alternative: (else_clause (compound_statement) @scope))
