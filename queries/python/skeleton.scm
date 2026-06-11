; Python language pack — same capture vocabulary, same driver, same
; engine. ~40 lines for outline + lexical typing through the
; production witness bag.

(class_definition
  name: (identifier) @def.class.name @context.package) @def.class @scope

(function_definition
  name: (identifier) @def.sub.name) @def.sub

(function_definition) @scope

(parameters
  (identifier) @def.var.name @def.var)

(import_statement
  name: (dotted_name) @import.name) @import
(import_from_statement
  module_name: (dotted_name) @import.name) @import

; Imported names are references to the remote def — this single
; pattern is what makes cross-file RENAME rewrite the import line too.
(import_from_statement
  name: (dotted_name (identifier) @ref.call))

; Assignment IS declaration in Python — the same identifier is both
; the def and the flow target.
(assignment
  left: (identifier) @def.var.name @def.var @flow.target
  right: (_) @flow.source) @flow.assign

; Annotated assignment: `x: int = ...` — ring 3 is partly IN the tree
; here; the annotation emits a direct type witness via the pack's
; annot_type predicate.
(assignment
  left: (identifier) @flow.target
  type: (type) @type.annot)

(call
  function: (identifier) @ref.call) @expr.call
(call
  function: (attribute attribute: (identifier) @ref.method))
(identifier) @expr.read.var

(string) @expr.lit.string
(integer) @expr.lit.number
(float) @expr.lit.number
(list) @expr.lit.arrayref
(dictionary) @expr.lit.hashref
