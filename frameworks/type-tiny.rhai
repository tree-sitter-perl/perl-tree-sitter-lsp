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
