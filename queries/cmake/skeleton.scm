; CMake language pack. The language is COMMAND-dispatched: `set`,
; `add_library`, and user functions are all (normal_command) — so defs
; come from the @cmd/@cmd.arg family, classified by the pack's
; cmd_effects predicate (case-insensitive, as CMake is).
;
; The grammar parses ${VAR} inside quoted strings as real nodes —
; interpolated variable refs are free here.

; ---- function / macro defs: name = first argument, rest = params ----
(function_def
  (function_command
    (argument_list . (argument) @def.sub.name))) @def.sub @scope

(function_def
  (function_command
    (argument_list (argument) @def.var.name @def.var)))

(macro_def
  (macro_command
    (argument_list . (argument) @def.sub.name))) @def.sub @scope

; ---- every command: name + ALL args in ONE match, ordered. The `+`
; quantifier is load-bearing: `(argument)` alone matches once PER argument,
; so each arg landed in its own match at index 0 and `Def{name_arg:0}`
; named every one (`set(V ${X} PARENT_SCOPE)` → V + the ref + the keyword).
; With `+` the driver groups them by command and indexes correctly: arg 0
; is the def, RefArgsFrom refs the rest (keywords filtered). ----
(normal_command
  (identifier) @cmd
  (argument_list (argument)+ @cmd.arg))
; commands with no arguments still get their invocation ref
(normal_command
  (identifier) @cmd)

; ---- variable references, including inside quoted strings ----
(variable_ref
  (normal_var (variable) @ref.var))
