// minion: Minion job-queue task registration + enqueue sites.
//
// Usage in a Mojolicious app:
//
//     $minion->add_task(send_email => sub ($job, $to, $subj, $body) { ... });
//     $minion->enqueue(send_email => ['alice@x', 'hi', 'body']);
//     $minion->enqueue(send_email => ['bob@x',   'hey', '...'] => {priority => 10});
//
// Modeling mirrors mojo-events (same shape, different verbs):
//   * `->add_task(NAME, sub { ... })` stamps a Handler for NAME and a
//     DispatchCall ref on the string so `gr` on the task name reaches
//     the registration site. `display: "Function"` (tasks aren't
//     events — they're callables you invoke indirectly via enqueue).
//   * `->enqueue` / `->enqueue_p` / `->perform_jobs` / `->perform_jobs_in_foreground`
//     / `->foreground` emit a DispatchCall ref only.
//   * The task callback's first positional is `$job: Minion::Job` —
//     `as_invocant_params` drops it from sig help; `VarType` types
//     it inside the callback body so `$job->finish`/`->fail`/`->note`
//     complete against Minion::Job methods.
//
// Two warts for a later pass (core-side work — deferred here):
//   * Sig help on the ARRAY argument in position 2 of `enqueue`
//     (`enqueue(task, [|])`). Minion passes the array's elements to
//     the task callback as positionals, so the ideal UX would surface
//     the task's param types when the cursor is inside the `[...]`.
//     Needs core support for "dispatcher's positional N is the
//     argument list in an arrayref".
//   * Hash-key completion on the OPTIONS hashref in position 3 of
//     `enqueue` (`enqueue(task, [args], {|})`). The plugin emits
//     HashKeyDefs for the common Minion options below so the data is
//     present; what's missing is cursor-context logic to recognize
//     "hash literal is a positional argument to method X" and route
//     the completion to HashKeyOwner::Sub{package,name:'enqueue'}.

fn id() { "minion" }

// Options accepted in the enqueue hashref (position 3). Emitted once
// per file as HashKeyDefs owned by `Sub { package: Minion, name: enqueue }`
// so cross-file completion + cross-file rename can pair them up.
fn enqueue_options() {
    [
        ["attempts", "Integer. Max retry attempts (default 1)."],
        ["delay",    "Integer. Seconds to wait before the job becomes runnable."],
        ["expire",   "Integer. Seconds after which an unclaimed job expires."],
        ["lax",      "Boolean. Skip dependency failure checks (default 0)."],
        ["notes",    "Hashref. Arbitrary notes attached at enqueue time."],
        ["parents",  "Arrayref of job IDs. Run only after parents finish."],
        ["priority", "Integer. Higher runs first (default 0)."],
        ["queue",    "String. Named queue (default 'default')."],
    ]
}

fn triggers() {
    [
        // Fire wherever Minion is in play. `use Minion;` covers
        // standalone scripts; `ClassIsa: "Minion"` handles subclasses.
        // Mojolicious plugin loads (`$app->plugin('Minion', ...)`)
        // generally don't `use Minion`, but the receiver type will
        // still be Minion once typed, and the per-method receiver
        // check below gates us on that.
        #{ UsesModule: "Minion" },
        #{ ClassIsa: "Minion" },
        // Lite apps commonly load Minion via the plugin system — fire
        // there too and rely on method-name + receiver checks.
        #{ UsesModule: "Mojolicious::Lite" },
        #{ UsesModule: "Mojolicious" },
        #{ ClassIsa: "Mojolicious" },
    ]
}

// Extract the task name, owner class, and tight name-span from a
// method call. Returns () when the call isn't eligible (dynamic name,
// unrecognized receiver, etc.).
fn resolve_task(ctx) {
    let args = ctx.args;
    if args.len() < 1 { return (); }

    let name = args[0].string_value;
    if name == () { return (); }

    // Gate on receiver class when we have one — Minion itself or any
    // Minion:: subclass. Unknown-typed receivers get a pass so long as
    // the dispatcher method name matched; the trigger filter plus the
    // method-name check already narrow this to Minion-shaped calls.
    let rtype = ctx.receiver_type;
    if rtype != () && rtype.contains("ClassName") {
        let rcn = rtype.ClassName;
        let looks_minion = rcn == "Minion" || rcn.starts_with("Minion::");
        if !looks_minion { return (); }
    }

    #{
        name: name,
        owner: #{ Class: "Minion" },
        name_span: args[0].span,
    }
}

// One-shot HashKeyDef emission for the enqueue options. The plugin
// has no per-file setup hook, so we emit them alongside the first
// eligible call we see and de-dup at the builder's Method-dedup
// layer (HashKeyDef dedup by (name, owner) happens naturally in the
// symbol table).
fn enqueue_option_defs(anchor_span) {
    let out = [];
    // `package` collides with Rhai's reserved keyword in object-map
    // literals, so build the owner as Dynamic and insert under the
    // string key directly.
    let sub_owner = #{};
    sub_owner["package"] = "Minion";
    sub_owner["name"] = "enqueue";
    let owner = #{ Sub: sub_owner };
    for opt in enqueue_options() {
        out += #{
            HashKeyDef: #{
                name: opt[0],
                owner: owner,
                span: anchor_span,
                selection_span: anchor_span,
            }
        };
    }
    out
}

fn on_method_call(ctx) {
    let out = [];
    let m = ctx.method_name;
    if m == () { return out; }

    let is_register = m == "add_task";
    let is_dispatch = m == "enqueue" || m == "enqueue_p"
        || m == "perform_jobs" || m == "perform_jobs_in_foreground"
        || m == "foreground";
    if !is_register && !is_dispatch { return out; }

    let task = resolve_task(ctx);
    if task == () { return out; }

    // Every call-site is a reference — both registration and enqueue
    // sites. `gr` on a task name returns the full wiring.
    out += #{
        DispatchCall: #{
            name: task.name,
            dispatcher: m,
            owner: task.owner,
            span: task.name_span,
            var_text: if ctx.receiver_text == () { "" } else { ctx.receiver_text },
        }
    };

    // Surface the options-hash keys whenever we see an enqueue call.
    // The builder de-dupes HashKeyDefs by (name, owner) at add_symbol
    // time, so multiple enqueue sites don't stack duplicates.
    if is_dispatch {
        out += enqueue_option_defs(task.name_span);
    }

    // `->add_task(NAME, sub { ... })` stamps the Handler def.
    if is_register {
        let params = [];
        if ctx.args.len() >= 2 {
            // Minion passes the job object as the callback's first
            // positional, then the enqueue array's elements after.
            // `as_invocant_params` flags position 0 only.
            params = as_invocant_params(ctx.args[1].sub_params);
        }

        out += #{
            Handler: #{
                name: task.name,
                owner: task.owner,
                // Dispatchers that look the task up by name — the
                // ones that produce a DispatchCall above. Keeps the
                // sig-help / gd machinery pairing refs to defs.
                dispatchers: [
                    "enqueue", "enqueue_p",
                    "perform_jobs", "perform_jobs_in_foreground",
                    "foreground",
                ],
                params: params,
                span: task.name_span,
                selection_span: task.name_span,
                // Tasks render as Function, not Event — you don't
                // subscribe to them, you enqueue calls. Matches how
                // mojo-routes chose Function over Event.
                display: "Task",
                // Minion unpacks the arrayref at position 1 into the
                // task callback's positionals (`enqueue(task, [@args])`).
                // Flags sig help to fire when cursor is inside `[...]`
                // at that slot — the core reads the declaration and
                // routes; no Minion-specific branching anywhere else.
                args_in_arrayref_at: 1,
            }
        };

        // Type the callback's `$job` (or whatever the user named it)
        // as Minion::Job so `$job->fail`, `->finish`, `->note`,
        // `->retry` complete inside the task body.
        if params.len() >= 1 && ctx.args.len() >= 2 {
            out += #{
                VarType: #{
                    variable: params[0].name,
                    at: ctx.args[1].span,
                    inferred_type: #{ ClassName: "Minion::Job" },
                }
            };
        }
    }

    out
}

// ---- Query hooks (IoC) ----

// Dispatchers that look a task up by name — the hook-side analog of
// the `dispatchers:` list on the Handler emission above.
fn is_enqueue_dispatcher(name) {
    let names = ["enqueue", "enqueue_p", "perform_jobs",
                 "perform_jobs_in_foreground", "foreground"];
    let hit = false;
    for d in names { if d == name { hit = true; break; } }
    hit
}

fn is_minion_dispatch_call(callframe) {
    if callframe == () { return false; }
    if !callframe.is_method { return false; }
    if !is_enqueue_dispatcher(callframe.name) { return false; }
    let rt = callframe.receiver_type;
    if rt != () && rt.contains("ClassName") {
        let rcn = rt.ClassName;
        return rcn == "Minion" || rcn.starts_with("Minion::");
    }
    // Untyped receiver is OK — the method-name + trigger filter
    // already narrowed us to Minion-shaped calls.
    true
}

// Sig help at dispatcher cursor positions we claim.
//   * Inside the handler-args arrayref → Show(task sig, active=arrayref slot)
//   * Inside the options hash          → Silent (suppress native so
//                                         the task sig doesn't leak
//                                         into enqueue's own options)
//   * Anywhere else at our dispatcher  → None (no claim — let the
//                                         native path render enqueue's
//                                         own signature)
fn on_signature_help(ctx) {
    let callframe = ctx["call"];
    if !is_minion_dispatch_call(callframe) { return (); }

    let container = ctx.cursor_inside;
    if container == () { return (); }

    if container.kind == "Hash" {
        // Options hash is enqueue's OWN arg — suppress the native
        // string-dispatch sig help that would show the task's shape.
        return #{ Silent: () };
    }
    if container.kind != "Array" { return (); }

    if callframe.args.len() < 1 { return (); }
    let name = callframe.args[0].string_value;
    if name == () { return (); }

    // `params` is a placeholder — the task's real signature lives
    // on the Handler symbol. Full resolution requires a symbol-table
    // lookup primitive the host will expose (see
    // docs/prompt-plugin-architecture.md "Open questions").
    #{
        Show: #{
            label: "enqueue('" + name + "', [ args ])",
            params: [name + " args"],
            active_param: container.active_slot,
        }
    }
}

// Completion at enqueue's arg-0 — the task name slot. Suppress the
// native method-of-receiver firehose; let the core populate items
// from Handler symbols with owner=Minion via the dispatch_targets_for
// delegation.
fn on_completion(ctx) {
    let callframe = ctx["call"];
    if !is_minion_dispatch_call(callframe) { return (); }

    if callframe.cursor_arg_index != 0 { return (); }
    if ctx.cursor_inside != () { return (); }

    #{
        items: [],
        exclusive: true,
        dispatch_targets_for: #{
            owner_class: "Minion",
            dispatcher_names: [
                "enqueue", "enqueue_p", "perform_jobs",
                "perform_jobs_in_foreground", "foreground",
            ],
        },
    }
}
