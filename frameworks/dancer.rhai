// dancer: Dancer2 DSL keyword injection.
//
// `use Dancer2` (or `use Dancer2 appname => ...`) monkey-patches ~90
// DSL keywords into the caller's package at import time. Without this
// plugin, every call to `get`, `post`, `param`, `request`, etc. fires
// an unresolved-function diagnostic — noise that drowns the real ones.
//
// Strategy mirrors mojo-lite's `on_use` pattern:
//
//   * Every keyword gets a `FrameworkImport` so the diagnostic filter
//     drops FunctionCall refs that match.
//
//   * High-value keywords that return typed objects get a synthesized
//     `Symbol` with `return_type` so chain calls resolve:
//       - `request` → `Dancer2::Core::Request`
//       - `response` → `Dancer2::Core::Response`
//       - `app` / `dancer_app` → `Dancer2::Core::App`
//       - `session` → `Dancer2::Core::Session`
//       - `config` / `setting` → HashRef (config is a plain hash snapshot)
//       - `params` / `body_parameters` / `query_parameters`
//         / `route_parameters` → HashRef
//       - `cookies` → HashRef
//       - `upload` → `Dancer2::Core::Request::Upload`
//       - `dsl` → `Dancer2::Core::DSL`
//       - `engine` → `Dancer2::Core::Factory::Engine`
//
//   Accessor-shaped keywords (`param`, `var`, `cookie`, `header`,
//   `status`) return a scalar whose type depends on the argument; they
//   get no typed stub here — the diagnostic suppression is the win.
//
// `Dancer2::Plugin` re-exports Moo's `has` plus `plugin_keywords`.
// Its trigger is a separate `ClassIsa` entry below.

fn id() { "dancer" }

fn triggers() {
    [
        #{ UsesModule: "Dancer2" },
        // Dancer2 apps that load plugins via `use Dancer2::Plugin` also
        // get the DSL in scope via re-export.
        #{ UsesModule: "Dancer2::Plugin" },
        // Apps that sub-class Dancer2 — `use parent 'Dancer2::Core::App'`
        // or derive via `Dancer2::Plugin` scaffolding.
        #{ ClassIsa: "Dancer2::Core::App" },
    ]
}

// The full Dancer2 DSL keyword set from Dancer2::Core::DSL::dsl_keywords.
// Keeping these in a function avoids the Rhai top-level const scoping
// issue (top-level `const` arrays are not reliably visible inside fn).
fn all_keywords() {
    [
        // HTTP verb route registrations — analogous to Mojolicious::Lite verbs.
        "get", "post", "put", "del", "patch", "any", "options",
        // Route organisation
        "prefix", "route",
        // Middleware / lifecycle hooks
        "hook",
        // App / DSL introspection
        "app", "dancer_app", "dsl",
        // Config
        "config", "set", "setting",
        // Request/response objects
        "request", "response",
        // Request params
        "param", "params", "body_parameters", "query_parameters",
        "route_parameters", "splat", "captures", "upload",
        // Request metadata
        "uri_for",
        // Headers
        "header", "headers", "content_type",
        // Response control
        "status", "redirect", "forward", "pass", "halt",
        "push_response_header",
        // Sending data
        "send_file", "send_as",
        // Template / rendering
        "template",
        // Engine access
        "engine",
        // Session
        "session",
        // Cookies
        "cookie", "cookies",
        // Serialisation helpers
        "to_json", "from_json", "to_yaml", "from_yaml",
        // Misc vars
        "var", "vars",
        // Async / streaming
        "delayed", "flush",
        // Logging
        "debug", "info", "warning", "error",
        // Boolean constants exported by Dancer2::Core::DSL
        "true", "false",
        // App lifecycle
        "dance", "to_app", "start",
        // Plugin keywords (available after `use Dancer2::Plugin`)
        "plugin_keywords", "plugin_setting",
        // has re-exported from Moo via Dancer2::Plugin
        "has",
    ]
}

// Keywords for which we synthesize typed stubs. Each entry is
// [name, return_type_map, doc]. The return_type_map is an InferredType
// dynamic map — `#{ ClassName: "..." }` or `#{ HashRef: () }` etc.
fn typed_keywords() {
    [
        ["request",          #{ ClassName: "Dancer2::Core::Request" },
                             "The current HTTP request object."],
        ["response",         #{ ClassName: "Dancer2::Core::Response" },
                             "The current HTTP response object."],
        ["app",              #{ ClassName: "Dancer2::Core::App" },
                             "The current Dancer2 app instance."],
        ["dancer_app",       #{ ClassName: "Dancer2::Core::App" },
                             "The current Dancer2 app instance (alias for app)."],
        ["dsl",              #{ ClassName: "Dancer2::Core::DSL" },
                             "The DSL object — provides access to every DSL method."],
        ["session",          #{ ClassName: "Dancer2::Core::Session" },
                             "The session object for the current request."],
        ["config",           #{ HashRef: () },
                             "The application configuration as a plain hash snapshot."],
        ["setting",          #{ HashRef: () },
                             "Alias for config — returns the application settings hash."],
        ["params",           #{ HashRef: () },
                             "Merged request parameters (route + query + body) as a hashref."],
        ["body_parameters",  #{ ClassName: "Hash::MultiValue" },
                             "POST body parameters as a Hash::MultiValue object."],
        ["query_parameters", #{ ClassName: "Hash::MultiValue" },
                             "Query-string parameters as a Hash::MultiValue object."],
        ["route_parameters", #{ ClassName: "Hash::MultiValue" },
                             "Named route captures as a Hash::MultiValue object."],
        ["cookies",          #{ HashRef: () },
                             "All cookies for the current request as a hashref of Cookie objects."],
        ["upload",           #{ ClassName: "Dancer2::Core::Request::Upload" },
                             "The file-upload object for the named field."],
        ["engine",           #{ ClassName: "Dancer2::Core::Factory::Engine" },
                             "Returns the named engine (template, session, logger, serializer)."],
    ]
}

fn on_use(ctx) {
    let m = ctx.module_name;
    // Accept `use Dancer2`, `use Dancer2 appname => 'MyApp'`, and
    // `use Dancer2::Plugin` (re-exports the DSL into plugin packages).
    let is_dancer = m == "Dancer2" || m == "Dancer2::Plugin";
    if !is_dancer { return []; }

    let out = [];

    // Suppress unresolved-function diagnostics for every DSL keyword.
    // This is the primary deliverable — without these, every `get`,
    // `param`, `debug`, etc. fires a noise diagnostic.
    for kw in all_keywords() {
        out += #{ FrameworkImport: #{ keyword: kw } };
    }

    // For high-value keywords, synthesize a Sub symbol with a declared
    // return type so `request->...`, `app->...`, `session->...` chains
    // resolve against the real class. `hide_in_outline: true` keeps the
    // symbols out of the document outline — they're DSL infrastructure,
    // not user-authored subs.
    for entry in typed_keywords() {
        let name    = entry[0];
        let rtype   = entry[1];
        let doc_str = entry[2];

        out += #{
            Symbol: #{
                name: name,
                kind: "Sub",
                span: ctx.span,
                selection_span: ctx.span,
                detail: #{
                    Sub: #{
                        params: [],
                        is_method: false,
                        doc: doc_str,
                        display: "Function",
                        hide_in_outline: true,
                    }
                },
                return_type: rtype,
            }
        };
    }

    out
}
