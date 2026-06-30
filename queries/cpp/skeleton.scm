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
(enumerator name: (identifier) @def.var.name) @def.var
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
(function_definition
  declarator: (function_declarator
    declarator: (identifier) @def.sub.name)) @def.sub @scope
(function_definition
  type: (_) @rettype
  declarator: (function_declarator
    declarator: (field_identifier) @def.method.name)) @def.method @scope
; out-of-line definition `RetT Class::method(...) { ... }` — @qualifier
; carries the `Class::` so the method attributes to its class, not the
; enclosing namespace.
(function_definition
  type: (_) @rettype
  declarator: (function_declarator
    declarator: (qualified_identifier
      scope: (_) @qualifier
      name: (identifier) @def.method.name))) @def.method @scope
(function_definition
  declarator: (pointer_declarator
    declarator: (function_declarator
      declarator: (identifier) @def.sub.name))) @def.sub @scope

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
(function_definition
  declarator: (function_declarator
    declarator: (qualified_identifier
      scope: (_) @qualifier
      name: (destructor_name) @def.method.name))) @def.method @scope

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
