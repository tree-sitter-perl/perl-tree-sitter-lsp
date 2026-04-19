// mojo-events: first-class Mojo::EventEmitter wire-ups / emit sites.
//
//   * ->on / ->once / ->subscribe   emit a Handler symbol for the event
//     name (stacked — each wire-up is its own registration) AND a
//     DispatchCall ref so `references` on the name reaches here.
//   * ->emit / ->unsubscribe / ->has_subscribers / ->catch  emit a
//     DispatchCall ref only. Cursor on the event name → goto-def jumps
//     to the nearest Handler, references shows every wire-up + emit.
//
// The Handler/DispatchCall types are part of the core file-analysis
// model (not HashKey-overloaded), so every downstream LSP feature
// (hover, sig help, ref, rename, workspace symbol) composes without
// any mojo-events-specific branching.

fn id() { "mojo-events" }

fn triggers() {
    [ #{ ClassIsa: "Mojo::EventEmitter" } ]
}

// Shared resolver: name + owner class + tight span around the name
// argument. Returns () when the call isn't eligible (dynamic name,
// no owner class, etc.).
fn resolve_event(ctx) {
    let args = ctx.args;
    if args.len() < 1 { return (); }

    let name = args[0].string_value;
    if name == () { return (); }

    let owner_class = ();
    let rtype = ctx.receiver_type;
    if rtype != () && rtype.contains("ClassName") {
        owner_class = rtype.ClassName;
    }
    if owner_class == () {
        owner_class = ctx.current_package;
    }
    if owner_class == () { return (); }

    #{
        name: name,
        owner: #{ Class: owner_class },
        name_span: args[0].span,
    }
}

fn on_method_call(ctx) {
    let out = [];
    let m = ctx.method_name;

    let is_register = m == "on" || m == "once" || m == "subscribe";
    let is_access = m == "emit" || m == "unsubscribe"
        || m == "has_subscribers" || m == "catch";

    if !is_register && !is_access { return out; }

    let ev = resolve_event(ctx);
    if ev == () { return out; }

    // Every call-site is a reference — emit and unsubscribe sites as well
    // as wire-ups. References on the handler name return all of them.
    out += #{
        DispatchCall: #{
            name: ev.name,
            dispatcher: m,
            owner: ev.owner,
            span: ev.name_span,
            var_text: if ctx.receiver_text == () { "" } else { ctx.receiver_text },
        }
    };

    // Registering verbs stamp a Handler def. Handler params come from
    // the handler sub's signature (extracted by the builder — sig syntax
    // or `my (...) = @_` — and surfaced in ArgInfo.sub_params).
    if is_register {
        let params = [];
        if ctx.args.len() >= 2 {
            // Mojo::EventEmitter passes the emitter as the callback's
            // first positional. The param list is otherwise verbatim
            // from the handler sub; `as_invocant_params` flags just
            // position 0 so sig help drops it at display time.
            params = as_invocant_params(ctx.args[1].sub_params);
        }

        out += #{
            Handler: #{
                name: ev.name,
                owner: ev.owner,
                dispatchers: ["emit"],
                params: params,
                span: ev.name_span,
                selection_span: ev.name_span,
            }
        };

        // Type the callback's first positional as the emitter class
        // so the user can ->-complete on it inside the handler body.
        // `ev.owner` is a Class map — project back to the string.
        if params.len() >= 1 && ctx.args.len() >= 2 {
            out += #{
                VarType: #{
                    variable: params[0].name,
                    at: ctx.args[1].span,
                    inferred_type: #{ ClassName: ev.owner.Class },
                }
            };
        }
    }

    out
}
