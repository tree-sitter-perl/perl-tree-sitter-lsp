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

; ---- namespaces: sticky context + a real scope for its body ----
(namespace_definition
  name: (namespace_identifier) @context.namespace
  body: (declaration_list) @scope)

; ---- type defs: class / struct / union / enum ----
(class_specifier
  name: (type_identifier) @def.class.name
  body: (field_declaration_list) @scope) @def.class
(struct_specifier
  name: (type_identifier) @def.class.name
  body: (field_declaration_list) @scope) @def.class
(union_specifier name: (type_identifier) @def.class.name) @def.class
(enum_specifier name: (type_identifier) @def.class.name) @def.class

; ---- free functions & out-of-line / inline method definitions ----
; the name lives at the bottom of the declarator chain; one pattern per
; shape it can take (plain / member / qualified / pointer-return).
(function_definition
  declarator: (function_declarator
    declarator: (identifier) @def.sub.name)) @def.sub @scope
(function_definition
  declarator: (function_declarator
    declarator: (field_identifier) @def.method.name)) @def.method @scope
(function_definition
  declarator: (function_declarator
    declarator: (qualified_identifier
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
      name: (identifier) @def.method.name))) @def.method

; ---- in-class method declarations (prototypes) & member fields ----
(field_declaration
  declarator: (function_declarator
    declarator: (field_identifier) @def.method.name)) @def.method
; pointer- / reference-returning methods (`Foo* m()`, `Foo& m()`):
; the function_declarator nests inside a pointer/reference wrapper.
(field_declaration
  declarator: (pointer_declarator
    declarator: (function_declarator
      declarator: (field_identifier) @def.method.name))) @def.method
(field_declaration
  declarator: (reference_declarator
    (function_declarator
      declarator: (field_identifier) @def.method.name))) @def.method
(field_declaration
  declarator: (field_identifier) @def.var.name) @def.var

; ---- calls ----
(call_expression function: (identifier) @ref.call)
(call_expression
  function: (field_expression field: (field_identifier) @ref.method))
