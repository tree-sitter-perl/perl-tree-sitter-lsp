; R language pack. Same vocabulary, same driver, same engine.
;
; R's bones fit the engine unusually well: `list(a=1)` /
; `data.frame(age=...)` ARE keyed shapes (HashWithKeys), `df$age` IS a
; key access, S3 methods are name conventions (`print.myclass`), and
; `source("util.R")` hands cross-file resolution a literal path.

; ---- defs ----
; `f <- function(...)` — a sub. The general var pattern below also
; matches; the driver's def-dedup prefers the more specific kind.
(binary_operator
  lhs: (identifier) @def.sub.name
  ["<-" "="]
  rhs: (function_definition)) @def.sub

; `name = function(...)` as a list()/R6Class() member — the R OOP idiom.
; These are `argument` nodes, not `binary_operator`, so the pattern above
; misses them (the scout found 195 such method defs lost across tidyverse).
(argument
  name: (identifier) @def.sub.name
  value: (function_definition)) @def.sub @scope

(binary_operator
  lhs: (identifier) @def.var.name @flow.target
  ["<-" "="]
  rhs: (_) @flow.source) @def.var @flow.assign

(parameter
  name: (identifier) @def.var.name) @def.var

; ---- scopes ----
(function_definition) @scope

; ---- refs ----
(call
  function: (identifier) @ref.call) @expr.call
(extract_operator
  lhs: (identifier) @ref.var
  rhs: (identifier) @ref.key)
(identifier) @expr.read.var

; ---- imports: library(pkg) / require(pkg) / source("path") ----
; Which call names import, and what the argument means, is the pack's
; import_module predicate — the query only ships the shape.
(call
  function: (identifier) @import.fn
  arguments: (arguments
    (argument value: (identifier) @import.arg)))
(call
  function: (identifier) @import.fn
  arguments: (arguments
    (argument value: (string (string_content) @import.arg))))

; ---- literals ----
(string) @expr.lit.string
(float) @expr.lit.number
(integer) @expr.lit.number

; ---- keyed shapes: list(a = 1, b = 2) / data.frame(age = ..., ...) ----
; Named arguments of a shape constructor are the keys; the driver
; groups @shape.key by enclosing @expr.shape span and asks the pack's
; shape_ctor predicate which callees actually construct $-accessible
; values.
(call
  function: (identifier) @shape.ctor
  arguments: (arguments
    (argument
      name: (identifier) @shape.key))) @expr.shape
