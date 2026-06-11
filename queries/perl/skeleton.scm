; Perl language pack for the query-extraction spike.
;
; The CAPTURE VOCABULARY is the language-agnostic contract — the
; driver (src/query_extract.rs) knows only these names, never node
; kinds. A new language = a new .scm speaking the same vocabulary
; (plus host predicates for what patterns can't say).
;
;   @def.<kind>          whole definition node
;   @def.<kind>.name     its name token
;   @scope               a lexical scope region
;   @context.package     sticky namespace context (flat `package Foo;`)
;   @ref.<kind>          a reference; text is the name
;   @import + @import.name

; ---- namespace context + package defs ----
(package_statement
  name: (package) @def.package.name @context.package) @def.package

; ---- subs / methods ----
(subroutine_declaration_statement
  name: (bareword) @def.sub.name) @def.sub

(method_declaration_statement
  name: (_) @def.method.name) @def.method

; ---- variable declarations (single and paren-list forms) ----
; FINDING: the `variable:`/`variables:` fields the CST prints (and
; child_by_field_name reads) match ZERO in the query engine — a
; grammar/field-table mismatch invisible until measured. The
; field-less structural form below is the workaround; a .scm pack
; accumulates exactly the same grammar traps cst.rs encodes, with
; far worse debuggability (silent non-match vs a failing unit test).
(variable_declaration
  [(scalar) (array) (hash)] @def.var.name) @def.var

; ---- scopes ----
(block) @scope
(anonymous_subroutine_expression) @scope

; ---- imports ----
(use_statement
  module: (package) @import.name) @import

; ---- references ----
(function_call_expression
  function: (function) @ref.call)
(ambiguous_function_call_expression
  function: (function) @ref.call)
(method_call_expression
  method: (method) @ref.method)
(scalar (varname) @ref.var)
(array (varname) @ref.var)
(hash (varname) @ref.var)

; ---- iteration 2: pushed as far as patterns honestly go ----

; Corinna classes: a block-scoped namespace, both context and def.
(class_statement
  name: (package) @def.class.name @context.package) @def.class

; Anonymous subs: a def with no name token — the driver asks the
; pack's default_name predicate.
(anonymous_subroutine_expression) @def.anon

; Signature parameters declare variables.
(signature
  (mandatory_parameter [(scalar) (array) (hash)] @def.var.name) @def.var)
(signature
  (optional_parameter [(scalar) (array) (hash)] @def.var.name) @def.var)

; Single-name `use constant PI => 3;` — the autoquoted key directly
; under the args is patternable. The `{ A => 1, B => 2 }` multi-key
; form is NOT: flat positional pairing (element 2k = key) cannot be
; said in a pattern. Deliberately left as residue — see findings.
(use_statement
  module: (package) @_const
  (list_expression . (autoquoted_bareword) @def.constant.name @def.constant)
  (#eq? @_const "constant"))

; Loop variables (`for my $x (...)`). NOTE: `variable:` here is a
; field on for_statement and DOES match in queries — the
; variable_declaration field failure above is node-specific, which is
; itself the finding: per-node field queryability must be empirically
; verified, never assumed from the printed CST.
(for_statement
  variable: (scalar) @def.var.name) @def.var

; ---- spike 2: typed-value events feeding the witness bag ----
; The driver maps `expr.lit.<t>` suffixes to InferredType generically;
; assignment events become Variable → Edge(Expr(rhs)) witnesses the
; PRODUCTION reducer registry chases. Same bag discipline as the
; builder: literals are source values, everything else is an edge.
(string_literal) @expr.lit.string
(interpolated_string_literal) @expr.lit.string
(number) @expr.lit.number
(anonymous_array_expression) @expr.lit.arrayref
(anonymous_hash_expression) @expr.lit.hashref
(scalar) @expr.read.var

(assignment_expression
  left: (variable_declaration [(scalar) (array) (hash)] @flow.target)
  right: (_) @flow.source) @flow.assign

(assignment_expression
  left: (scalar) @flow.target
  right: (_) @flow.source) @flow.assign

; ---- spike 3b: operator evidence ----
; Perl's edge over other dynamic languages is operator orientation:
; mono-typed operators leak operand types at USAGE sites. That
; leakiness is itself capture vocabulary — `@obs.<t>` arms feed
; TypeObservation witnesses that the production FrameworkAwareTypeFold
; folds, so a variable with an unknowable initializer still types from
; how it's USED. (A Python pack has no such arms to write — that
; asymmetry is a fact about the languages, not about the design.)
(binary_expression
  left: (scalar) @obs.numeric
  ["+" "-" "*" "/" "%" "**" "==" "!=" "<=>"])
(binary_expression
  ["+" "-" "*" "/" "%" "**" "==" "!=" "<=>"]
  right: (scalar) @obs.numeric)
(binary_expression
  left: (scalar) @obs.string
  ["." "eq" "ne" "cmp" "lt" "gt"])
(binary_expression
  ["." "eq" "ne" "cmp" "lt" "gt"]
  right: (scalar) @obs.string)
