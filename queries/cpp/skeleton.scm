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
(field_declaration
  declarator: (field_identifier) @def.var.name) @def.var
; a data member's TYPE — the type witness needs field_declaration (the
; `declaration` patterns below only see locals). Only plain-field
; declarators match (a function_declarator is a method, not a field), so
; member-access chains (`box.inner.`) can type `inner` on its class.
(field_declaration
  type: (_) @type.annot
  declarator: (field_identifier) @flow.target)

; ---- calls ----
(call_expression function: (identifier) @ref.call) @expr.call
(call_expression
  function: (field_expression field: (field_identifier) @ref.method))

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

; pointer-declared locals: `T* p;`, `T* p = init;`, and the
; condition-form `if (Derived* d = dynamic_cast<Derived*>(b))` — the
; pointee names the variable's class (pointer-ness dropped for nav). The
; cast-in-condition is captured by the bare form (its `value` field is
; on the declaration, simply ignored here).
(declaration
  type: (_) @type.annot
  declarator: (pointer_declarator
    declarator: (identifier) @flow.target @def.local))
(declaration
  type: (_) @type.annot
  declarator: (init_declarator
    declarator: (pointer_declarator declarator: (identifier) @flow.target @def.local)
    value: (_) @flow.source))
; reference-declared locals: `T& r = x;` (the referent names the class).
(declaration
  type: (_) @type.annot
  declarator: (reference_declarator (identifier) @flow.target @def.local))
(declaration
  type: (_) @type.annot
  declarator: (init_declarator
    declarator: (reference_declarator (identifier) @flow.target @def.local)
    value: (_) @flow.source))

; ---- function PARAMETERS carry a type too (the dominant embedded site:
; `void f(Handle *h) { h->... }`). Value / pointer / reference forms;
; pointer-/reference-ness dropped for navigation, like locals. ----
(parameter_declaration
  type: (_) @type.annot
  declarator: (identifier) @flow.target @def.local)
(parameter_declaration
  type: (_) @type.annot
  declarator: (pointer_declarator declarator: (identifier) @flow.target @def.local))
(parameter_declaration
  type: (_) @type.annot
  declarator: (reference_declarator (identifier) @flow.target @def.local))

; ---- literals + variable reads (the edge-chase substrate) ----
(number_literal) @expr.lit.number
(string_literal) @expr.lit.string
(identifier) @expr.read.var
