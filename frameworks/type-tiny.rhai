// type-tiny: the Type::Tiny / Types::Standard constraint vocabulary used in
// `isa` (`has minion => (isa => InstanceOf['Clove::Minion'])`).
//
// A constraint constructor (`InstanceOf[...]`) is a *value* — a Type::Tiny
// object — not a class. The core types its call expression as
// `TypeConstraintOf(inner)` and an `isa => …` projects the inner (the
// constrained type) onto the accessor. This plugin owns the vocabulary:
// which names are constraints, and how each folds its params into the inner
// type. The core does the node-walking (rule #1) and hands over a flat param
// list; arity lives here.
//
// Scope: the "first string param is a class" shape — InstanceOf and its role
// twin ConsumerOf — plus `Maybe[T]`, a passthrough wrapper whose param is
// itself a constructor. That unlocks `$self->attr->method` chains where attr
// is `isa => InstanceOf['X']` or `isa => Maybe[InstanceOf['X']]`.
//
// Import scoping (docs/prompt-type-constraint-types.md §"Future: registration
// moves to the injection level"): when a package does `use Types::Standard
// qw/Str Int/`, those names become known via an Import action — suppressing
// unresolved-function diagnostics for explicitly imported type constants.
// `-all` / `:all` / bare use → full vocab emitted.  `Clove::Types` and
// other Type::Library re-exporters ride the same seam via their own plugin's
// `on_use` emitting `SyntheticUse "Types::Standard"`.

fn id() { "type-tiny" }

// Manifest hooks are global (consulted by name), so triggers are irrelevant.
fn triggers() { [] }

// Cheap dispatch gate: which call names are constraint constructors.
fn type_constraint_names() { ["InstanceOf", "ConsumerOf", "Maybe"] }

// Fold the constructor's params into the constrained INNER type, or () to
// decline. `params` is a list of #{ string, ty }. Arity is ours to handle.
fn type_constraint_inner(name, params) {
    if params.is_empty() { return (); }
    if name == "Maybe" {
        // `Maybe[T]` permits undef OR a T; for resolution purposes the
        // constrained value (when present) is whatever T constrains to. The
        // single param is a nested constructor the core typed
        // `TypeConstraintOf(T)`; pass through to T's inner. (Optionalness
        // itself isn't modeled — see docs/prompt-optional-types.md.)
        return constrained_inner(params[0].ty);
    }
    // InstanceOf['Foo'] / ConsumerOf['Role'] — first string param is the class.
    let cls = params[0].string;
    if cls == () { return (); }
    type_class(cls)
}

// ---- Import-scoped vocabulary ----

// Full export lists by module — authoritative from @EXPORT_OK at the
// installed version. These cover every name a user can legally import;
// the on_use hook narrows to exactly what they requested (or expands
// to all of them for -all / bare use). New type constants added to a
// future Types::Standard release land here, nowhere else.

fn types_standard_exports() {
    [
        // Core type hierarchy
        "Any", "Item",
        "Bool",
        "Undef", "Defined", "Value",
        "Str", "LaxNum", "StrictNum", "Num", "Int",
        "ClassName", "RoleName",
        "Ref", "CodeRef", "RegexpRef", "GlobRef", "FileHandle",
        "ArrayRef", "HashRef", "ScalarRef",
        "Object",
        // Parameterisable container types
        "Map", "Optional", "Slurpy", "Tuple", "CycleTuple", "Dict",
        // Type combinators and utilities
        "Maybe", "Overload", "StrMatch", "OptList", "Tied",
        "Enum",
        // Constraint constructors (also in type_constraint_names)
        "InstanceOf", "ConsumerOf", "HasMethods",
        // Coercion / validation helpers exported as functions
        "slurpy", "MkOpt", "Join", "Split",
        // is_* / assert_* / to_* families: every type constant above
        // ships these three companions. Listed individually so the
        // import-gate suppresses `assert_InstanceOf(...)` etc. too.
        "is_Any", "assert_Any", "to_Any",
        "is_Item", "assert_Item", "to_Item",
        "is_Bool", "assert_Bool", "to_Bool",
        "is_Undef", "assert_Undef", "to_Undef",
        "is_Defined", "assert_Defined", "to_Defined",
        "is_Value", "assert_Value", "to_Value",
        "is_Str", "assert_Str", "to_Str",
        "is_LaxNum", "assert_LaxNum", "to_LaxNum",
        "is_StrictNum", "assert_StrictNum", "to_StrictNum",
        "is_Num", "assert_Num", "to_Num",
        "is_Int", "assert_Int", "to_Int",
        "is_ClassName", "assert_ClassName", "to_ClassName",
        "is_RoleName", "assert_RoleName", "to_RoleName",
        "is_Ref", "assert_Ref", "to_Ref",
        "is_CodeRef", "assert_CodeRef", "to_CodeRef",
        "is_RegexpRef", "assert_RegexpRef", "to_RegexpRef",
        "is_GlobRef", "assert_GlobRef", "to_GlobRef",
        "is_FileHandle", "assert_FileHandle", "to_FileHandle",
        "is_ArrayRef", "assert_ArrayRef", "to_ArrayRef",
        "is_HashRef", "assert_HashRef", "to_HashRef",
        "is_ScalarRef", "assert_ScalarRef", "to_ScalarRef",
        "is_Object", "assert_Object", "to_Object",
        "is_Maybe", "assert_Maybe", "to_Maybe",
        "is_Map", "assert_Map", "to_Map",
        "is_Optional", "assert_Optional", "to_Optional",
        "is_Slurpy", "assert_Slurpy", "to_Slurpy",
        "is_Tuple", "assert_Tuple", "to_Tuple",
        "is_CycleTuple", "assert_CycleTuple", "to_CycleTuple",
        "is_Dict", "assert_Dict", "to_Dict",
        "is_Overload", "assert_Overload", "to_Overload",
        "is_StrMatch", "assert_StrMatch", "to_StrMatch",
        "is_OptList", "assert_OptList", "to_OptList",
        "is_Tied", "assert_Tied", "to_Tied",
        "is_Enum", "assert_Enum", "to_Enum",
        "is_InstanceOf", "assert_InstanceOf", "to_InstanceOf",
        "is_ConsumerOf", "assert_ConsumerOf", "to_ConsumerOf",
        "is_HasMethods", "assert_HasMethods", "to_HasMethods",
    ]
}

fn types_common_string_exports() {
    [
        "SimpleStr", "NonEmptySimpleStr",
        "NonEmptyStr",
        "LowerCaseStr", "UpperCaseStr",
        "LowerCaseSimpleStr", "UpperCaseSimpleStr",
        "StrLength", "DelimitedStr",
        "NumericCode", "Password", "StrongPassword",
        "is_SimpleStr", "assert_SimpleStr", "to_SimpleStr",
        "is_NonEmptySimpleStr", "assert_NonEmptySimpleStr", "to_NonEmptySimpleStr",
        "is_NonEmptyStr", "assert_NonEmptyStr", "to_NonEmptyStr",
        "is_LowerCaseStr", "assert_LowerCaseStr", "to_LowerCaseStr",
        "is_UpperCaseStr", "assert_UpperCaseStr", "to_UpperCaseStr",
        "is_LowerCaseSimpleStr", "assert_LowerCaseSimpleStr", "to_LowerCaseSimpleStr",
        "is_UpperCaseSimpleStr", "assert_UpperCaseSimpleStr", "to_UpperCaseSimpleStr",
        "is_StrLength", "assert_StrLength", "to_StrLength",
        "is_DelimitedStr", "assert_DelimitedStr", "to_DelimitedStr",
        "is_NumericCode", "assert_NumericCode", "to_NumericCode",
        "is_Password", "assert_Password", "to_Password",
        "is_StrongPassword", "assert_StrongPassword", "to_StrongPassword",
    ]
}

fn types_common_numeric_exports() {
    [
        "PositiveNum", "PositiveOrZeroNum",
        "PositiveInt", "PositiveOrZeroInt",
        "NegativeNum", "NegativeOrZeroNum",
        "NegativeInt", "NegativeOrZeroInt",
        "SingleDigit",
        "NumRange", "IntRange",
        "is_PositiveNum", "assert_PositiveNum", "to_PositiveNum",
        "is_PositiveOrZeroNum", "assert_PositiveOrZeroNum", "to_PositiveOrZeroNum",
        "is_PositiveInt", "assert_PositiveInt", "to_PositiveInt",
        "is_PositiveOrZeroInt", "assert_PositiveOrZeroInt", "to_PositiveOrZeroInt",
        "is_NegativeNum", "assert_NegativeNum", "to_NegativeNum",
        "is_NegativeOrZeroNum", "assert_NegativeOrZeroNum", "to_NegativeOrZeroNum",
        "is_NegativeInt", "assert_NegativeInt", "to_NegativeInt",
        "is_NegativeOrZeroInt", "assert_NegativeOrZeroInt", "to_NegativeOrZeroInt",
        "is_SingleDigit", "assert_SingleDigit", "to_SingleDigit",
        "is_NumRange", "assert_NumRange", "to_NumRange",
        "is_IntRange", "assert_IntRange", "to_IntRange",
    ]
}

// `use Types::Standard qw/Str Int .../` → emit Import so the named constants
// are known in this package. The builder's `process_use` already pushes an
// Import entry for the literal qw-list, but that entry only covers what the
// user typed; this hook also handles `-all` / `:all` / bare use (where the
// qw-list is empty or contains a meta-import flag) by expanding to the full
// vocabulary. The same Import action is the hook that `Clove::Types` and other
// Type::Library re-exporters ride via `SyntheticUse "Types::Standard"` in
// their own plugin.
fn on_use(ctx) {
    let vocab = ();

    if ctx.module_name == "Types::Standard" {
        vocab = types_standard_exports();
    } else if ctx.module_name == "Types::Common::String" {
        vocab = types_common_string_exports();
    } else if ctx.module_name == "Types::Common::Numeric" {
        vocab = types_common_numeric_exports();
    } else if ctx.module_name == "Types::Common" {
        // Umbrella re-exporter: everything from all three sub-modules.
        vocab = types_standard_exports();
        for n in types_common_string_exports() { vocab += n; }
        for n in types_common_numeric_exports() { vocab += n; }
    } else {
        return [];
    }

    // Determine the effective import list. Meta-imports (-all, :all,
    // -default) and an empty list (bare `use`) both mean "all of vocab".
    // A genuine name list is used verbatim — it's already a subset of vocab,
    // and the caller is entitled to import names from any source; we don't
    // filter by our list so future additions don't break existing code.
    let requested = ctx.imports;
    let is_meta = requested.is_empty();
    if !is_meta {
        for r in requested {
            if r == "-all" || r == ":all" || r == "-default" || r.starts_with(":") {
                is_meta = true;
                break;
            }
        }
    }

    let names = if is_meta { vocab } else { requested };
    if names.is_empty() { return []; }

    let syms = [];
    for n in names { syms += #{ local_name: n }; }

    [#{
        Import: #{
            module_name: ctx.module_name,
            imported_symbols: syms,
            span: ctx.span,
        }
    }]
}
