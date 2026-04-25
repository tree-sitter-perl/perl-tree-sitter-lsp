// data-printer: alias DDP to Data::Printer, synthesize the
// `p` / `np` imports it monkey-patches into every caller, and
// surface its option keys for use-line config completion.
//
// ---- Imports
//
// Data::Printer's `import` sub installs `&p` and `&np` directly into
// the caller's symbol table — no `@EXPORT` / `@EXPORT_OK`, so the
// cross-file extractor sees them as plain Subs but no caller's
// import list claims them, and intelligence (hover, gd, sig-help)
// has nothing to bind to. The plugin declares the imports plugin-
// side so call sites resolve through the normal imported-function
// path.
//
// `use DDP` is a literal alias: DDP.pm is just
//
//   package DDP;
//   use Data::Printer;
//   push our @ISA, 'Data::Printer';
//
// Both spellings install the same subs from the same source, so the
// plugin pins the synthetic Import at Data::Printer (the real
// module) regardless of which name the user typed. K-on-`p` lands
// on Data::Printer's POD whether the file said `use DDP` or
// `use Data::Printer`.
//
// ---- Use-line options
//
// Data::Printer's import accepts a hashref of config options
// (`use DDP { caller_info => 1, colored => 1 }`). Plugins receive
// `current_use_module` in the completion query context — when it's
// "DDP" / "Data::Printer" and the cursor sits inside a Hash
// container, the plugin returns the option keys directly. Core
// stays generic — the option list is data inside this script, not
// a baked-in table.

fn id() { "data-printer" }

// `Always` so the plugin appears in `applicable()` for every
// completion query — `on_completion` then filters internally on
// `ctx.current_use_module`. (`on_use` separately bypasses the
// trigger filter, so it'd fire either way.) The plugin defines
// no `on_method_call` / `on_function_call`, so unconditional
// triggering costs nothing for non-DDP code.
fn triggers() {
    [ #{ Always: () } ]
}

// Single source of truth for which spellings the plugin claims.
// Used by both on_use (Import synthesis) and on_completion
// (option-list claiming).
fn is_data_printer(name) {
    name == "DDP" || name == "Data::Printer"
}

fn on_use(ctx) {
    let m = ctx.module_name;
    if !is_data_printer(m) { return []; }

    // One synthetic `use Data::Printer qw(p np)` regardless of which
    // alias the user wrote. resolve_imported_function walks
    // analysis.imports looking for the call name in
    // imported_symbols.local_name; that's the seam every cross-file
    // intelligence feature routes through (hover, gd, sig-help,
    // unresolved-function diagnostic). Pinning module_name to
    // Data::Printer (not DDP) means the lookup hits the real source.
    [
        #{
            Import: #{
                module_name: "Data::Printer",
                imported_symbols: [
                    #{ local_name: "p" },
                    #{ local_name: "np" },
                ],
                span: ctx.span,
            }
        }
    ]
}

// Top-level Data::Printer config options — the keys that appear
// directly in `use DDP { ... }`. Sub-namespaced options under
// `class => { ... }`, `filters => [ ... ]`, etc. are not surfaced
// here; the cursor's nested-hash detection would need to track the
// outer key for that, which the current ctx shape doesn't carry.
//
// Source: Data::Printer 1.x "Properties Quick Reference" POD plus
// Data::Printer::Object accessor list. Each entry: [name, doc].
fn dp_options() {
    [
        // Scalar / string
        ["show_tainted",      "Boolean. Show taint flag on tainted scalars (default 1)."],
        ["show_unicode",      "Boolean. Show :utf8 flag on unicode scalars (default 1)."],
        ["show_lvalue",       "Boolean. Mark lvalue scalars (default 1)."],
        ["print_escapes",     "Boolean. Render escape chars (\\n, \\t, ...) literally (default 0)."],
        ["scalar_quotes",     "String. Quote char for string scalars (default '\"')."],
        ["escape_chars",      "String. 'none' | 'nonascii' | 'nonlatin1' | 'all' (default 'none')."],
        ["string_max",        "Integer. Max chars before truncation (default 4096)."],
        ["string_preserve",   "String. 'begin' | 'middle' | 'end' (default 'begin')."],
        ["string_overflow",   "String. Truncation marker (default '(...skipping __SKIPPED__ chars...)')."],
        ["unicode_charnames", "Boolean. Show unicode char names (default 0)."],

        // Array
        ["array_max",         "Integer. Max array elements before truncation (default 100)."],
        ["array_preserve",    "String. 'begin' | 'middle' | 'end' (default 'begin')."],
        ["array_overflow",    "String. Truncation marker for arrays."],
        ["index",             "Boolean. Show array indices (default 1)."],

        // Hash
        ["hash_max",          "Integer. Max hash keys before truncation (default 100)."],
        ["hash_preserve",     "String. 'begin' | 'middle' | 'end' (default 'begin')."],
        ["hash_overflow",     "String. Truncation marker for hashes."],
        ["hash_separator",    "String. Between key and value (default '   ')."],
        ["align_hash",        "Boolean. Align hash values to widest key (default 1)."],
        ["sort_keys",         "Boolean. Sort hash keys (default 1)."],
        ["quote_keys",        "String. 'auto' | 0 | 1 (default 'auto')."],

        // General
        ["name",              "String. Variable name shown in caller_info (default 'var')."],
        ["return_value",      "String. 'pass' | 'dump' | 'void' (default 'pass')."],
        ["output",            "String. 'stderr' | 'stdout' | filename | filehandle (default 'stderr')."],
        ["use_prototypes",    "Boolean. Use prototypes for p()/np() (default 1)."],
        ["indent",            "Integer. Indent width in spaces (default 4)."],
        ["show_readonly",     "Boolean. Mark readonly values (default 1)."],
        ["show_tied",         "Boolean. Mark tied values (default 1)."],
        ["show_dualvar",      "String. 'lax' | 'strict' | 'off' (default 'lax')."],
        ["show_weak",         "Boolean. Mark weak refs (default 1)."],
        ["show_refcount",     "Boolean. Show refcount on refs (default 0)."],
        ["show_memsize",      "Boolean. Show memory footprint (default 0)."],
        ["memsize_unit",      "String. 'auto' | 'b' | 'k' | 'm' (default 'auto')."],
        ["separator",         "String. Between elements (default ',')."],
        ["end_separator",     "Boolean. Trailing separator (default 0)."],
        ["caller_info",       "Boolean. Print file/line info (default 0)."],
        ["caller_message",    "String. caller_info template (default 'Printing in line __LINE__ of __FILENAME__')."],
        ["max_depth",         "Integer. Max nesting depth, 0 = unlimited (default 0)."],
        ["deparse",           "Boolean. Show coderef bodies via B::Deparse (default 0)."],
        ["alias",             "String. Override exported name from 'p' (default 'p')."],
        ["warnings",          "Boolean. Print Data::Printer's own warnings (default 1)."],
        ["multiline",         "Boolean. Multi-line output (default 1)."],
        ["quiet",             "Boolean. Suppress all output (default 0)."],
        ["live_update",       "Integer. Seconds between .dataprinter rc reloads, 0 = off."],

        // Color / theme
        ["colored",           "Boolean | 'auto'. ANSI color output (default 'auto')."],
        ["theme",             "String. Color theme name (default 'Material')."],

        // Object output
        ["class_method",      "String. Custom dump method on objects (default '_data_printer')."],
        ["class",             "Hashref. Class display options (parents, linear_isa, expand, ...)."],
        ["filters",           "Arrayref. Filter modules + inline filter hashes."],
        ["filter_class",      "String. Class-filter mode."],
        ["profile",           "String. Profile module to load."],
    ]
}

// Use-line option completion. Plugin claims the slot exclusively
// (`exclusive: true`) so the generic "loading Module..." placeholder
// from the export-list path doesn't appear alongside the option
// keys — when the cursor is `use DDP { | }`, the user is picking
// a config key, never an exported sub.
fn on_completion(ctx) {
    let m = ctx.current_use_module;
    if m == () { return (); }
    if !is_data_printer(m) { return (); }

    let inside = ctx.cursor_inside;
    if inside == () { return (); }
    if inside.kind != "Hash" { return (); }

    let used = inside.existing_keys;

    let items = [];
    for opt in dp_options() {
        let name = opt[0];
        let doc  = opt[1];
        // Suppress keys the user already wrote at this hash level —
        // ContainerFrame.existing_keys carries them. Drops noise from
        // re-suggesting a key already typed.
        let already = false;
        for k in used { if k == name { already = true; break; } }
        if already { continue; }

        items += #{
            label: name,
            kind: "Field",
            detail: doc,
            insert_text: name + " => ",
        };
    }

    #{
        items: items,
        exclusive: true,
    }
}
