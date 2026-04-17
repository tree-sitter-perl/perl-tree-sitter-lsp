# Mojolicious Deep Intelligence + Builder Plugin Architecture

> **Status: blocked on unification phase 1.** See `docs/prompt-unification-spec.md`.
>
> **Superseded sections.** The data model in this document (Part E's `FrameworkEntity`, the per-feature Rust structs in Part A like `MojoRoute`/`MojoTask`/`MojoEvent`, and Part C's `framework_entities: Vec<FrameworkEntity>` field) is **replaced** by the unified Symbol/Ref/Namespace model. Mojo features do not introduce parallel data structures; they are **emission rules** that create Symbols and Refs in framework-specific Namespaces. The Pattern Catalog (Part A's Perl code + "What we provide" lists) and detection logic remain valid. The data-model replacement is in the **Emission Rules Catalog** section below (which supersedes Parts C, D, and E).

## Part A: Mojo-Specific Intelligence — Complete Pattern Catalog

### 1. Route → Controller#Action Resolution

```perl
# Full app — method call style:
sub startup {
    my $self = shift;
    my $r = $self->routes;

    # String shorthand: 'controller#action'
    $r->get('/users')->to('users#list');
    $r->post('/users')->to('users#create');
    $r->any('/login')->to('Auth#login');
    $r->put('/users/:id')->to('users#update');
    $r->delete('/users/:id')->to('users#remove');
    $r->patch('/users/:id')->to('users#patch');
    $r->options('/cors')->to('cors#preflight');

    # Hash-style: controller => 'name', action => 'name'
    $r->get('/')->to(controller => 'main', action => 'index');
    $r->get('/about')->to(controller => 'main', action => 'about');

    # Default action is 'index' when only controller given
    $r->get('/dashboard')->to('dashboard#');

    # Nested/chained routes via under()
    my $auth = $r->under('/')->to('auth#check');
    $auth->get('/dashboard')->to('dashboard#index');
    my $api = $r->under('/api/v1')->to('api-auth#verify');
    $api->get('/users')->to('api-users#list');

    # Route with conditions
    $r->get('/admin')->over(authenticated => 1)->to('admin#index');

    # Bridges (legacy name for under)
    my $bridge = $r->bridge('/admin')->to('admin#auth');

    # Inline handler (no controller resolution, but still a route)
    $r->get('/healthz')->to(cb => sub { $_[0]->render(text => 'ok') });
}

# Lite app — function call style:
use Mojolicious::Lite -signatures;
get '/hello' => sub ($c) { $c->render(text => 'Hello') };
post '/data' => sub ($c) { ... };
any '/catch' => sub ($c) { ... };
put '/update' => sub ($c) { ... };
del '/remove' => sub ($c) { ... };  # 'del' not 'delete' in Lite
patch '/patch' => sub ($c) { ... };
options '/cors' => sub ($c) { ... };

# Under in Lite:
under '/api' => sub ($c) { ... };
group {
    get '/data' => sub ($c) { ... };
};

# Lite with controller dispatch (hybrid):
get '/users' => {controller => 'users', action => 'list'};
```

**What we provide:**
- **Goto-def** from `'users#list'` string → jumps to `MyApp::Controller::Users::list`
- **Goto-def** from `controller => 'users'` value → jumps to `MyApp::Controller::Users`
- **Goto-def** from `action => 'list'` value → jumps to `list` method in resolved controller
- **Completion** inside `->to('...')` string — offer `controller#action` combinations
- **Completion** for `controller =>` values — offer known controller names
- **Completion** for `action =>` values — offer methods from the resolved controller
- **Diagnostics** — warn on `->to('controller#nonexistent')` when controller exists but action doesn't
- **Hover** on route string — show the resolved controller class + method signature

**Detection:**
- `->to(string)` where string contains `#` → route target (method call)
- `->to(controller => 'x', action => 'y')` → hash-style route target
- `->to(cb => sub { ... })` → inline handler, skip controller resolution
- Lite: `get`, `post`, `any`, `put`, `del`, `patch`, `options` function calls with hashref arg containing controller/action

**Controller class resolution:**
- Convention: `{AppNamespace}::Controller::{ucfirst(controller)}`
- Hyphenated names: `'api-users'` → `ApiUsers` (Mojo convention: hyphens become camelCase)
- CamelCase passthrough: `'Auth'` → `Auth` (already capitalized)
- Namespace separator: `'admin-users'` → `AdminUsers`, NOT `Admin::Users`

**App namespace detection:**
- Full app: package that `use Mojo::Base 'Mojolicious'` → that package name
- Lite app: `main` by default, or the current package
- Fallback: strip `::Controller::*` from known controller packages

**Data emitted:** see the **Emission Rules Catalog** section at the end of this doc. Routes do not introduce a new data type; they emit Refs pointing at existing Controller/Method Symbols.

### 2. Route Naming + url_for Resolution

```perl
# Naming routes:
$r->get('/users/:id')->to('users#show')->name('show_user');
$r->get('/login')->to('auth#form')->name('login_page');

# Using named routes:
$c->url_for('show_user', id => 42);      # completion + goto-def
$c->redirect_to('login_page');            # completion + goto-def
$self->url_for('show_user');              # in templates too

# Lite app auto-naming:
get '/hello' => sub { ... } => 'hello_route';  # third arg is the name
```

**What we provide:**
- **Route name registry** — accumulate from `->name('...')` chains and Lite auto-names
- **Completion** inside `url_for('...')`, `redirect_to('...')` — offer known route names
- **Goto-def** from route name string → jumps to the route definition
- **Hover** on route name — shows the URL pattern + controller#action

**Data addition to MojoRoute:**
The `name` field captures `->name('...')`. Build a lookup map `route_names: HashMap<String, usize>` (name → route index) for completion/goto-def.

### 3. Helper Intelligence

```perl
# Defining helpers — method call in full app:
sub startup {
    my $self = shift;
    $self->helper(current_user => sub {
        my $c = shift;
        return $c->session->{user_id} ? MyApp::Model::User->find($c->session->{user_id}) : undef;
    });
    $self->helper(db => sub {
        state $db = MyApp::DB->new;
        return $db;
    });
    $self->helper(is_admin => sub { shift->current_user && shift->current_user->role eq 'admin' });
    $self->helper(flash_msg => sub {
        my ($c, $type, $msg) = @_;
        $c->flash(message => { type => $type, text => $msg });
    });
}

# Defining helpers — function call in Lite app:
use Mojolicious::Lite;
helper current_user => sub { ... };
helper db => sub { state $db = MyDB->new; return $db };

# Defining helpers — in plugin register():
sub register {
    my ($self, $app, $conf) = @_;
    $app->helper(authenticate => sub { ... });
    $app->helper(is_authenticated => sub { ... });
}

# Using helpers — on controller $c / $self:
my $user = $c->current_user;
my $db = $self->db;
if ($c->is_admin) { ... }
$c->flash_msg('error', 'Not found');

# Using helpers — in templates (.ep):
% my $user = current_user;
%= db->query(...)
```

**What we provide:**
- **Synthesize** each helper as `SymKind::Method` symbol on the app package
- **Goto-def** from `$c->current_user` → jumps to the `helper(current_user => ...)` call
- **Completion** — `$c->` in controllers offers helpers alongside regular methods
- **Return type inference** — analyze helper sub body (`state $db = MyDB->new` → `ClassName(MyApp::DB)`)
- **Parameter info** — helpers with extra params (`flash_msg($type, $msg)`) get signature help
- **Hover** — shows helper source, return type, parameter names

**Detection — all forms:**
- Method call: `->helper(string, sub)` where invocant is `$self`/`$app` or has app type
- Function call: `helper string => sub` (Lite imports)
- Both use `ambiguous_function_call_expression` or `method_call_expression` in tree-sitter

**Helper parameters:**
The first param of the helper sub is always `$c` (controller). Additional params are user-facing:
```perl
$self->helper(format_date => sub {
    my ($c, $date, $format) = @_;  # $c is implicit, $date and $format are the real params
    ...
});
$c->format_date($date, $format);  # signature help shows ($date, $format)
```
Strip `$c`/`$self`/`$_[0]` from the param list when synthesizing the method symbol.

### 4. Minion Task Intelligence

```perl
# Registering tasks — inline sub:
$app->minion->add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
    # $job is Minion::Job, rest are user args
    send_mail(to => $to, subject => $subject, body => $body);
});

# Registering tasks — class name:
$app->minion->add_task(process_order => 'MyApp::Task::ProcessOrder');

# Registering tasks — with options (Minion 10+):
$app->minion->add_task(
    cleanup => { timeout => 300, sub => \&do_cleanup }
);

# Enqueuing — positional args:
$app->minion->enqueue('send_email', [$to, $subject, $body]);
$c->minion->enqueue('send_email', ['user@example.com', 'Hello', 'World']);

# Enqueuing — with options:
$c->minion->enqueue('process_order', [$order_id], {
    priority => 5,
    delay => 60,
    queue => 'important',
    attempts => 3,
    expire => 3600,
    notes => { order_id => $order_id },
});

# Enqueuing — from helpers:
$c->enqueue_task('send_email', ...);  # if app defines a helper wrapper

# Task result checking:
my $result = $minion->job($id)->info;
```

**What we provide:**
- **Task registry** — accumulate `add_task` calls, map name → definition
- **Completion** inside `enqueue('...')` first arg — offer known task names
- **Goto-def** from `enqueue('send_email')` → jumps to `add_task(send_email => ...)` definition
- **Signature help for task args** — when cursor is in `enqueue('send_email', [|])`, show the task sub's parameter names (minus `$job`)
- **Hover** on task name in enqueue → shows task definition, parameter names
- **Diagnostics** — warn on `enqueue('nonexistent_task')` when task isn't registered

**Task arg completion/signature help:**
The inline sub's params tell us what the task expects:
```perl
add_task(send_email => sub {
    my ($job, $to, $subject, $body) = @_;
    ...
});
enqueue('send_email', [$to, $subject, $body]);  # show: $to, $subject, $body
```
Strip `$job` (first param, always `Minion::Job`) from the displayed params. When cursor is inside the arrayref arg of `enqueue`, show which positional parameter the cursor is at.

For class-based tasks:
```perl
add_task(process_order => 'MyApp::Task::ProcessOrder');
```
Resolve the class, find its `run` method, extract params from there.

**Data emitted:** see the **Emission Rules Catalog** section. Tasks are Symbols in the `MojoApp` Namespace with `SymKind::MojoTask`; `enqueue('name')` string literals become Refs pointing at them.

### 5. Stash Intelligence

```perl
# Setting stash values — in controller actions:
sub list {
    my $c = shift;
    $c->stash(users => $users);
    $c->stash(title => 'User List', count => scalar @$users);
    $c->stash->{custom_key} = 'value';
}

# Setting via render:
$c->render(
    template => 'users/list',
    users => $users,         # these go to stash
    title => 'User List',
);

# Setting via route defaults:
$r->get('/users')->to('users#list', per_page => 20);

# Reading stash values:
my $users = $c->stash('users');
my $title = $c->stash->{title};
my $per_page = $c->param('per_page');  # also checks stash
```

**What we provide:**
- **Stash key registry** per controller — accumulate keys from `$c->stash(key =>)`, `$c->render(key =>)`, and route defaults
- **Completion** inside `$c->stash('...')` and `$c->stash->{...}` — offer known stash keys for this controller
- **Goto-def** from stash read → jumps to where the key was set
- **Type inference** — if `$c->stash(users => $users)` and `$users` is ArrayRef, then `$c->stash('users')` returns ArrayRef

**Detection:**
- `->stash(key => val, ...)` — fat-comma pairs, keys are stash entries
- `->stash('key')` — single string arg is a read
- `->stash->{key}` — hash deref, key is a stash entry
- `->render(key => val, ...)` — fat-comma pairs after known render options (`template`, `json`, `text`, `data`, `status`, `format`) are stash entries
- `->to('controller#action', key => val)` — fat-comma pairs after the route target are default stash values

### 6. Plugin Intelligence

```perl
# Loading plugins:
$app->plugin('Mojolicious::Plugin::Authentication', {
    load_user => sub { ... },
    validate_user => sub { ... },
});

# Short form (Mojolicious::Plugin:: prefix assumed):
$app->plugin('Authentication');
$app->plugin('Config');
$app->plugin('NotYAMLConfig');

# Plugin with config:
$app->plugin('OAuth2', {
    github => {
        key => $ENV{GITHUB_KEY},
        secret => $ENV{GITHUB_SECRET},
    },
});

# Inside a plugin:
package Mojolicious::Plugin::MyPlugin;
use Mojo::Base 'Mojolicious::Plugin';

sub register {
    my ($self, $app, $conf) = @_;
    $app->helper(my_helper => sub { ... });
    $app->hook(before_dispatch => sub { ... });
}
```

**What we provide:**
- **Goto-def** from `->plugin('Authentication')` → jumps to `Mojolicious::Plugin::Authentication`
- **Plugin class resolution** — short names get `Mojolicious::Plugin::` prefix
- **Transitive helper discovery** (Phase 2) — resolve plugin class → find `register()` → find `helper()` calls → add to helper registry
- **Plugin config completion** (stretch) — if plugin documents its config keys, offer completion inside the config hash

**Detection:**
- `->plugin(string)` or `->plugin(string, hashref)` method call
- First string arg is the plugin name
- Resolve: if name contains `::`, use as-is; otherwise prefix with `Mojolicious::Plugin::`

### 7. Hook Intelligence

```perl
# Registering hooks:
$app->hook(before_dispatch => sub {
    my $c = shift;
    $c->res->headers->header('X-Framework' => 'Mojolicious');
});

$app->hook(after_dispatch => sub { ... });
$app->hook(around_dispatch => sub {
    my ($next, $c) = @_;
    $next->();
});
$app->hook(before_server_start => sub {
    my ($server, $app) = @_;
});

# All hook names:
# before_dispatch, after_dispatch, around_dispatch,
# before_routes, around_action, after_static,
# before_server_start, after_build_tx
```

**What we provide:**
- **Completion** for hook names inside `->hook('...')` — offer the known hook names
- **Signature help** — show the expected sub signature for each hook type (different hooks get different args)
- **Hover** on hook name — describe what the hook does and when it fires

**Hook parameter signatures:**

| Hook | Params |
|------|--------|
| `before_dispatch` | `($c)` |
| `after_dispatch` | `($c)` |
| `around_dispatch` | `($next, $c)` |
| `before_routes` | `($c)` |
| `around_action` | `($next, $c, $action, $last)` |
| `after_static` | `($c)` |
| `before_server_start` | `($server, $app)` |
| `after_build_tx` | `($tx, $app)` |

These are static — hardcode the known hook names and their signatures.

### 8. Mojolicious::Lite DSL Intelligence

```perl
use Mojolicious::Lite -signatures;

# Routes (function call style):
get '/users' => sub ($c) { ... };
post '/users' => sub ($c) { ... };
any '/catch' => sub ($c) { ... };
any ['GET', 'POST'] => '/multi' => sub ($c) { ... };
put '/update' => sub ($c) { ... };
del '/delete' => sub ($c) { ... };    # Note: 'del' not 'delete'
patch '/patch' => sub ($c) { ... };
options '/cors' => sub ($c) { ... };
websocket '/ws' => sub ($c) { ... };

# Under (authentication/middleware):
under '/' => sub ($c) { return $c->session->{user} ? 1 : 0 };

# Groups (nested routes):
group {
    under '/admin' => sub ($c) { ... };
    get '/dashboard' => sub ($c) { ... };
    get '/settings' => sub ($c) { ... };
};

# Named routes (third positional arg):
get '/users/:id' => sub ($c) { ... } => 'show_user';

# Route with defaults:
get '/pages/:page' => {page => 'index'} => sub ($c) { ... };

# Helpers:
helper current_user => sub ($c) { ... };
helper db => sub ($c) { state $db = MyDB->new };

# Hooks:
hook before_dispatch => sub ($c) { ... };

# Plugin loading:
plugin 'Config';
plugin 'Authentication', { ... };

# App instance:
app->start;
app->secrets(['mysecret']);
app->config->{mykey};
```

**What we provide:**
- **Framework import suppression** — `get`, `post`, `any`, `put`, `del`, `patch`, `options`, `websocket`, `under`, `group`, `helper`, `hook`, `plugin`, `app` are all imported by `Mojolicious::Lite` and should not trigger "unresolved function" diagnostics
- **Route detection** — same intelligence as full app routes, but from function calls instead of method calls
- **Helper detection** — `helper name => sub` as function call (already partially handled by framework_imports)
- **Hook detection** — `hook name => sub` as function call
- **Plugin detection** — `plugin 'Name'` as function call

**Detection for Lite mode:**
When `use Mojolicious::Lite` is detected:
1. Add all DSL keywords to `framework_imports`
2. Treat `get`, `post`, etc. function calls as route registrations
3. The route's handler is the inline sub — no controller resolution needed, but the sub's params get signature help
4. Named routes: if the call has 3 args and the last is a bare string, it's a route name

### 9. EventEmitter Intelligence (on/emit/once)

Mojolicious is built on `Mojo::EventEmitter`. Every Mojo object — controllers, user agents, IO loops, transactions, websockets — emits and listens to events. This is a pervasive pattern across the entire framework.

```perl
# ── Websocket events (on Mojolicious::Controller in WS context) ──
$r->websocket('/ws')->to('ws#connect');

sub connect {
    my $c = shift;
    $c->on(message => sub {
        my ($c, $msg) = @_;
        $c->send("Echo: $msg");
    });
    $c->on(finish => sub {
        my ($c, $code, $reason) = @_;
    });
    $c->on(drain => sub {
        my $c = shift;
        $c->send(time);
    });
}

# Lite websocket:
websocket '/ws' => sub ($c) {
    $c->on(message => sub ($c, $msg) { ... });
    $c->on(json => sub ($c, $hash) { ... });
};

# ── Mojo::UserAgent events ──
my $ua = Mojo::UserAgent->new;
$ua->on(start => sub {
    my ($ua, $tx) = @_;
    $tx->req->headers->header('X-Custom' => 'value');
});
$ua->on(prepare => sub { my ($ua, $tx) = @_; ... });

# ── Mojo::IOLoop events ──
Mojo::IOLoop->on(finish => sub { ... });
Mojo::IOLoop->on(reset => sub { ... });

# ── Mojo::Transaction events ──
$tx->on(finish => sub { my $tx = shift; ... });
$tx->on(connection => sub { my ($tx, $connection) = @_; ... });
$tx->on(upgrade => sub { my ($tx, $ws) = @_; ... });
$tx->on(request => sub { my $tx = shift; ... });

# ── Mojo::Transaction::WebSocket events ──
$ws->on(text => sub { my ($ws, $bytes) = @_; ... });
$ws->on(binary => sub { my ($ws, $bytes) = @_; ... });
$ws->on(json => sub { my ($ws, $value) = @_; ... });
$ws->on(message => sub { my ($ws, $msg) = @_; ... });
$ws->on(drain => sub { my $ws = shift; ... });
$ws->on(finish => sub { my ($ws, $code, $reason) = @_; ... });
$ws->on(frame => sub { my ($ws, $frame) = @_; ... });
$ws->on(resume => sub { my $ws = shift; ... });

# ── User-defined events ──
package MyApp::Notifier;
use Mojo::Base 'Mojo::EventEmitter';

sub notify {
    my ($self, $msg) = @_;
    $self->emit('notification', $msg);
    $self->emit('log', "Sent: $msg");
}

# Consuming:
my $n = MyApp::Notifier->new;
$n->on(notification => sub { my ($self, $msg) = @_; ... });
$n->on(log => sub { my ($self, $entry) = @_; ... });

# once() — same as on() but fires only once:
$ua->once(start => sub { ... });
$c->once(finish => sub { ... });

# unsubscribe:
$ua->unsubscribe('start');

# emit_safe (catches exceptions in handlers):
$self->emit_safe('error', $err);
```

**What we provide:**

**Built-in event completion:**
- **Completion** for event names in `->on('...')`, `->once('...')`, `->unsubscribe('...')` — offer known events for the invocant's class
- **Signature help** per event — show the handler's expected parameters

Known event sets (hardcoded per class):

| Class | Events |
|-------|--------|
| `Mojolicious::Controller` (WS) | `message`, `json`, `text`, `binary`, `drain`, `finish`, `frame`, `resume` |
| `Mojo::UserAgent` | `start`, `prepare` |
| `Mojo::IOLoop` | `finish`, `reset` |
| `Mojo::Transaction` | `finish`, `connection`, `upgrade`, `request` |
| `Mojo::Transaction::WebSocket` | `text`, `binary`, `json`, `message`, `drain`, `finish`, `frame`, `resume` |
| `Mojo::EventEmitter` (base) | (no built-ins, but inherits pattern) |

Per-event handler signatures (hardcoded):

| Event | Class | Handler params |
|-------|-------|---------------|
| `message` | Controller/WS | `($c, $msg)` |
| `json` | Controller/WS | `($c, $hash)` |
| `text` | WS | `($ws, $bytes)` |
| `binary` | WS | `($ws, $bytes)` |
| `drain` | Controller/WS | `($c)` |
| `finish` | Controller/WS | `($c, $code, $reason)` |
| `finish` | Transaction | `($tx)` |
| `finish` | IOLoop | `()` |
| `frame` | WS | `($ws, $frame)` |
| `start` | UserAgent | `($ua, $tx)` |
| `prepare` | UserAgent | `($ua, $tx)` |
| `connection` | Transaction | `($tx, $connection)` |
| `upgrade` | Transaction | `($tx, $ws)` |
| `request` | Transaction | `($tx)` |
| `reset` | IOLoop | `()` |

**User-defined event completion:**
- **Accumulate** event names from `->emit('name')`, `->emit_safe('name')` calls on objects of a given class
- **Offer** accumulated event names in `->on('...')` completion for the same class
- **Goto-def** from `->on('custom_event')` → jumps to the `->emit('custom_event')` call site (or vice versa)
- **Cross-reference** — find-references on an event name shows all `on` + `emit` + `once` + `unsubscribe` calls

**Detection:**
- `->on(string, sub)` — event registration. First string arg is event name.
- `->once(string, sub)` — same as `on` but single-fire.
- `->emit(string, ...)` — event firing. First string arg is event name.
- `->emit_safe(string, ...)` — same as `emit` with exception catching.
- `->unsubscribe(string)` — event unregistration.

**Data emitted:** see the **Emission Rules Catalog**. Event names are Symbols in the emitting class's Namespace with `SymKind::MojoEvent`; `on('name')` / `emit('name')` / `once('name')` / `unsubscribe('name')` strings become Refs with distinguishing `RefKind::MojoEventOn` / `MojoEventEmit` / `MojoEventOnce` / `MojoEventUnsubscribe`. The first `emit('name')` synthesizes the Symbol; subsequent references are Refs to it.

**Cross-class event resolution:**
When we see `$ua->on('start')` and we know `$ua` is `Mojo::UserAgent`, we offer UA-specific events. When class is unknown, fall back to showing all accumulated user-defined events + the base `Mojo::EventEmitter` events. The existing type inference system tells us the invocant's class — same machinery used for method completion.

### 10. Config Intelligence

```perl
# Reading config:
my $db_host = $app->config->{db}{host};
my $secret = $app->config('secret');
my $all = $app->config;

# Setting config:
$app->config(hypnotoad => { listen => ['http://*:8080'] });

# Config from file (via Config plugin):
# myapp.conf:
{
    db => { host => 'localhost', port => 5432 },
    secret => 'mysecret',
}
```

**What we provide (stretch):**
- **Hash key completion** on `$app->config->{...}` — if a `.conf` file exists in the workspace, parse it and offer its keys
- This leverages our existing hash key completion infrastructure

### 11. Mojo::DOM / CSS Selector Intelligence (stretch)

```perl
my $dom = Mojo::DOM->new($html);
my $titles = $dom->find('h1.title')->map('text');
my $link = $dom->at('a.nav-link');
$dom->find('div.item')->each(sub { ... });
```

Out of scope for now — CSS selector completion is a whole separate domain. But worth noting for the future.

---

## Part B: Builder Plugin Architecture

### Problem

`builder.rs` is 5925 lines and growing. It contains:

| Concern | ~Lines | Coupled to core? |
|---------|--------|-----------------|
| Core walk + scopes + symbols + refs | ~2000 | Yes — this IS the core |
| Type inference + constraints | ~500 | Yes — used everywhere |
| String list extraction | ~200 | Utility — used by many |
| Constant folding | ~200 | Used by string extraction |
| Framework detection + accessor synthesis | ~500 | No — framework-specific |
| DBIC synthesis | ~200 | No — DBIC-specific |
| Export extraction | ~100 | Could be separate |
| Helper functions | ~400 | Mixed |
| Tests | ~1800 | — |

Adding Mojo intelligence would add another ~500+ lines. This is the right time to decompose.

### Proposed architecture

```
src/
  builder.rs              ← Core only: walk, scopes, symbols, refs, types,
                            constant folding, string extraction, exports.
                            Calls plugin passes after main walk.
  builder_framework.rs    ← Framework detection + accessor synthesis
                            (Moo/Moose/Mojo::Base has, extends, with)
  builder_dbic.rs         ← DBIC: add_columns, has_many, belongs_to, load_components
  builder_mojo.rs         ← Mojo intelligence: routes, helpers, tasks, hooks,
                            stash, plugins, Lite DSL, websockets
```

### Plugin contract

Two kinds of plugins:

**Walk-time plugins** — called from specific `visit_*` methods during the tree walk:

```rust
// builder_framework.rs
pub fn synthesize_framework_accessors(builder: &mut Builder) { ... }

// builder_dbic.rs
pub fn synthesize_dbic_accessors(builder: &mut Builder) { ... }
```

These need `&mut Builder` because they add symbols/refs during the walk. They're called from visit points like `visit_has_call`, `visit_method_call`.

**Post-build plugins** — run after the walk, read completed FileAnalysis:

```rust
// builder_mojo.rs
pub fn analyze_mojo_patterns(fa: &mut FileAnalysis, tree: &Tree, source: &[u8]) { ... }
```

Gets `&mut FileAnalysis` + tree + source. Scans existing refs/symbols for Mojo patterns, synthesizes new symbols/refs. May use tree-sitter queries for targeted node extraction (e.g., getting string content from a `->to()` call's arguments).

### How `build()` changes

```rust
pub fn build(tree: &Tree, source: &[u8]) -> FileAnalysis {
    let mut b = Builder::new(source);
    b.walk(tree.root_node());

    let mut fa = FileAnalysis::new(
        b.scopes, b.symbols, b.refs, /* ... */
    );

    // Post-build plugin passes
    builder_mojo::analyze_mojo_patterns(&mut fa, tree, source);

    fa
}
```

### Extraction plan

| Step | What moves | From → To |
|------|-----------|-----------|
| 1 | `FrameworkMode`, `visit_has_call`, `visit_extends_call`, `map_isa_to_type`, accessor synthesis | `builder.rs` → `builder_framework.rs` |
| 2 | `visit_dbic_class_method`, `visit_dbic_add_columns`, `visit_dbic_relationship`, `is_dbic_class` | `builder.rs` → `builder_dbic.rs` |
| 3 | New Mojo intelligence | New `builder_mojo.rs` |

Steps 1 and 2 are pure refactors. Step 3 is new feature code.

---

## Part C: Mojo Detection and Data Flow

### Detecting a Mojo app

**Full app:**
- Package has `Mojolicious` in its parent chain (via `use Mojo::Base 'Mojolicious'`, `use parent 'Mojolicious'`)
- Has `sub startup` method
- App namespace = this package name

**Lite app:**
- `use Mojolicious::Lite` in file
- App namespace = `main` (or current package)
- All DSL functions imported

**Controller:**
- Package has `Mojolicious::Controller` in parent chain
- Or namespace matches `*::Controller::*`

**Plugin:**
- Package has `Mojolicious::Plugin` in parent chain
- Has `sub register` method

Detection leverages existing `package_parents` — just check if the parent chain includes the Mojo base classes.

### FileAnalysis changes

**No new top-level vector on `FileAnalysis`.** Mojo features are expressed as Symbols and Refs in framework-specific Namespaces (see `docs/prompt-unification-spec.md` phase 1 for the Namespace enum). Two small flags on `FileAnalysis`:

```rust
/// Which Mojo-app namespace this file participates in.
/// Used during emission to route Symbols into the right Namespace.
pub mojo_app_namespace: Option<String>,
pub is_mojo_lite: bool,
```

### Post-build pass flow

```rust
pub fn analyze_mojo_patterns(fa: &mut FileAnalysis, tree: &Tree, source: &[u8]) {
    let app_ns = detect_app_namespace(fa);
    let is_lite = detect_lite_mode(fa);
    if app_ns.is_none() && !is_lite && !is_controller(fa) && !is_plugin(fa) {
        return;
    }

    fa.mojo_app_namespace = app_ns.clone();
    fa.is_mojo_lite = is_lite;

    // Emission rules — each appends Symbols and/or Refs to fa.symbols/fa.refs
    // with the appropriate home_namespace and target resolution.
    emit_helpers(fa, tree, source, &app_ns, is_lite);
    emit_routes(fa, tree, source, &app_ns);          // PackageRef + MethodCall refs from route strings
    emit_route_names(fa, tree, source, &app_ns);     // MojoRouteName symbols, Refs at url_for sites
    emit_tasks(fa, tree, source, &app_ns);           // MojoTask symbols, Refs at enqueue sites
    emit_hooks(fa, tree, source);                    // Refs against pre-seeded BuiltIn hook symbols
    emit_events(fa, tree, source);                   // MojoEvent symbols + Refs across on/emit/once/unsubscribe
    emit_plugins(fa, tree, source, &app_ns);         // MojoPluginRef + namespace_parents edge
    emit_stash_keys(fa, tree, source);               // HashKeyDef/HashKeyAccess in MojoAction namespace
    emit_under_chain_edges(fa, tree, source);        // namespace_parents edges between MojoAction namespaces
}
```

Each `emit_*` function is a pure emission rule: inspect the tree for a specific construct, append to `fa.symbols` / `fa.refs` with the correct `home_namespace` / `target`. No shared mutable "framework entity" bag. LSP queries never look up Mojo features through a special path — `resolve_symbol` / `refs_to` / `ref_at` already cover them.

### Route string parsing

```rust
fn parse_route_target(s: &str) -> Option<(String, String)> {
    let (controller, action) = s.split_once('#')?;
    let action = if action.is_empty() { "index" } else { action };
    Some((controller.to_string(), action.to_string()))
}

fn resolve_controller_class(controller: &str, app_ns: Option<&str>) -> String {
    let capitalized = capitalize_controller(controller);
    match app_ns {
        Some(ns) => format!("{}::Controller::{}", ns, capitalized),
        None => format!("Controller::{}", capitalized),
    }
}

fn capitalize_controller(s: &str) -> String {
    s.split(|c: char| c == '-' || c == '_')
        .map(|part| {
            let mut c = part.chars();
            match c.next() {
                None => String::new(),
                Some(f) => f.to_uppercase().chain(c).collect(),
            }
        })
        .collect()
}
```

### Task arg signature help

When cursor is inside `enqueue('send_email', [|])`:

1. `cursor_context.rs` identifies the cursor as "inside arrayref arg of an `enqueue`-family call."
2. Resolve the task name from arg[0] via `resolve_symbol(task_name, from_ns, VISIBLE)` filtered to `SymKind::MojoTask`.
3. The Symbol's `params` (already stripped of `$job`) map to array positions.
4. Return `SignatureHelp` with the param at the cursor's array index highlighted.

No Mojo-specific plumbing — the task Symbol carries all the info already, and `resolve_symbol` is the same one every other query uses.

---

## Part D: Cross-File

After unification phase 2 lands, cross-file is not a Mojo concern at the data-model level. Every file (open, workspace, dep) stores a full `FileAnalysis` in the shared `FileStore`. Mojo Symbols and Refs emitted in one file are automatically visible to queries from another:

- Route in app file → goto-def jumps to controller method in another file.
- Helper defined in app → completion in controller files via `namespace_parents` walk.
- Task defined in app → completion/goto-def from any file.
- Plugin loaded in host → plugin's helpers appear in host's controllers through the `MojoApp ← MojoPlugin` edge added at emission time.

Plugin chain resolution traverses `namespace_parents` normally; transitive loads walk edges with an existing recursion cap of 3.

---

## Part E: Emission Rules Catalog

This section replaces the earlier `FrameworkEntity` data model. **Core rule:** every Mojo concept is a **Symbol** (definition) + **Refs** (references) in an appropriate **Namespace**. No parallel vector, no per-framework fields on `FileAnalysis`, no bespoke query methods.

### Namespaces used

From unification phase 1:

- `Package(String)` — ordinary Perl package.
- `MojoApp(String)` — the Mojo app scope.
- `MojoPlugin(String)` — plugin-local scope.
- `MojoController(String)` — controller class; parent edge to its `MojoApp`.
- `MojoAction { controller, action }` — action scope; parent edges to its `MojoController` plus any `under()` bridge actions.
- `LiteApp(PathBuf)` — Lite app's namespace (the file itself).
- `BuiltIn` — pre-seeded framework symbols (hook names, built-in event names per class).

### New SymKind variants

- `MojoHelper` — helper registered via `$app->helper(name => sub { ... })`. A parallel `Method` Symbol is also emitted so `$c->name` completes as a method.
- `MojoTask` — Minion task.
- `MojoRouteName` — named route (`->name('show_user')`).
- `MojoEvent` — event emittable by a class.
- `MojoHook` — pre-seeded hook name.
- `MojoStashKey` — (may be unified with existing `HashKeyDef`).

### New RefKind variants (roles)

- `MojoRouteTarget { controller_span, action_span }` — a `->to('ctrl#act')` string literal. The builder emits **two separate refs** from one string, one per sub-span — existing narrowest-span-wins logic in `ref_at()` disambiguates at cursor time.
- `MojoRouteName` — `url_for('name')` / `redirect_to('name')` string.
- `MojoTaskEnqueue` — task name string in `enqueue('name', ...)`.
- `MojoEventOn` / `MojoEventEmit` / `MojoEventOnce` / `MojoEventUnsubscribe` — event name strings in respective call contexts.
- `MojoHookName` — hook name string in `hook('name' => ...)`.
- `MojoStashAccess` — (may be unified with existing `HashKeyAccess`).
- `MojoPluginRef` — plugin name string in `$app->plugin('Name')`.

### Per-feature emission

| Feature | Symbols emitted | Namespace | Refs at call sites |
|---------|-----------------|-----------|--------------------|
| Helper in `startup` | `Symbol { kind: MojoHelper }` + parallel `Symbol { kind: Method }` | `MojoApp(app)` | `$c->name` → ordinary `MethodCall` Ref, resolved via `namespace_parents` (controller → app) |
| Helper in plugin `register` | Same Symbols | `MojoPlugin(plugin)` | Plugin-load emits edge `MojoApp(host) ← MojoPlugin(plugin)`; host controllers see helper automatically |
| Route `->to('users#list')` | None (Controller+Method already exist) | — | **Two Refs from one string**: `PackageRef` on `users` sub-span → existing `Package(MyApp::Controller::Users)`; `MethodCall` on `list` sub-span → existing `list` method Symbol |
| Route `->to(controller=>'x', action=>'y')` | None | — | Same two Refs, sourced from fat-comma values |
| Route `->to('ctrl#')` | None | — | `PackageRef` + `MethodCall` against implicit `index` |
| Inline route `->to(cb => sub {})` | None | — | None (handler is the inline sub itself) |
| Route name `->name('show_user')` | `Symbol { kind: MojoRouteName }` | `MojoApp(app)` | `url_for('show_user')` etc. → `Ref { kind: MojoRouteName }` |
| Minion task (inline sub) | `Symbol { kind: MojoTask, params: from sub minus $job }` | `MojoApp(app)` | `enqueue('name', [...])` → `Ref { kind: MojoTaskEnqueue }` |
| Minion task (class ref) `add_task(n => 'Class')` | `Symbol { kind: MojoTask, target_class: "Class" }` | `MojoApp(app)` | Same; hover/goto-def follows `target_class` to its `run` method |
| Hook `$app->hook('name' => ...)` | None — hook names are pre-seeded `BuiltIn` Symbols | `BuiltIn` | `Ref { kind: MojoHookName, target: pre-seeded Symbol }` |
| Stash set in action body | `HashKeyDef` Symbol (or `MojoStashKey`) | `MojoAction(ctrl, act)` | `$c->stash('k')` / `$c->stash->{k}` → `HashKeyAccess` Ref via `namespace_parents` walk |
| Stash via route default `->to('c#a', k => v)` | Same Symbol | `MojoAction(c, a)` | Same |
| Stash via render `->render(k => v)` | Same Symbol | Enclosing `MojoAction` | Same |
| Plugin load `$app->plugin('Auth')` | None (plugin's Package Symbol already exists) | — | `Ref { kind: MojoPluginRef }`. **Side effect:** add `MojoApp(host) ← MojoPlugin("Mojolicious::Plugin::Auth")` to `namespace_parents` |
| Event first `emit('x')` site | `Symbol { kind: MojoEvent, home_namespace: Package(emitter) }` | `Package(emitter)` | The emit site itself becomes the def span; subsequent emits are Refs to it |
| Event `on('x', ...)` | None | — | `Ref { kind: MojoEventOn }` — UNRESOLVED sentinel until an emit appears, resolved via enrichment |
| `once('x', ...)` / `unsubscribe('x')` | None | — | `MojoEventOnce` / `MojoEventUnsubscribe` Ref |
| Built-in events (UserAgent `start`, Transaction `finish`, etc.) | Pre-seeded Symbols per class | `BuiltIn` | Same Ref kinds, target pre-seeded Symbol; handler params populated from static table |
| Lite route `get '/p' => sub {}` | Anonymous handler Symbol | `LiteApp(file)` | — |
| Lite named route `get ... => 'name'` | Additional `MojoRouteName` Symbol | `LiteApp(file)` | Same as full-app named routes |
| Lite DSL keywords (`get`, `post`, `under`, `app`, `helper`, `hook`, `plugin`, `group`, `websocket`) | Pre-seeded framework-import Symbols | `BuiltIn` | Ordinary `FunctionCall` Refs |

### Pre-seeded Symbol tables

Two small static tables bootstrap at FileStore initialization:

- **Hook names + handler signatures.** `before_dispatch($c)`, `after_dispatch($c)`, `around_dispatch($next, $c)`, `before_routes($c)`, `around_action($next, $c, $action, $last)`, `after_static($c)`, `before_server_start($server, $app)`, `after_build_tx($tx, $app)`. Loaded once as Symbols in `Namespace::BuiltIn`.
- **Event names per class.** `Mojo::UserAgent` → `[start, prepare]`; `Mojo::Transaction` → `[finish, connection, upgrade, request]`; `Mojo::Transaction::WebSocket` → `[text, binary, json, message, drain, finish, frame, resume]`; `Mojo::IOLoop` → `[finish, reset]`; `Mojolicious::Controller` (WS context) → `[message, json, text, binary, drain, finish, frame, resume]`.

Refs from user code resolve to these pre-seeded Symbols via ordinary `resolve_symbol` — no special lookup path.

### Under-chain edges

```perl
my $auth = $r->under('/')->to('auth#check');
$auth->get('/dashboard')->to('dashboard#index');
```

`emit_under_chain_edges` tracks each route-chain's accumulated under-actions and adds `namespace_parents` edges so stash inheritance follows the dispatch order:

```
MojoAction("MyApp::Controller::Dashboard", "index")
    ← parent: MojoAction("MyApp::Controller::Auth", "check")
    ← parent: MojoController("MyApp::Controller::Dashboard")
    ← parent: MojoApp("MyApp")
```

Multi-level chains (under → under → leaf) compose edges the same way.

### DBIC retroactive alignment

DBIC framework synthesis adopts the same pattern:

- `add_columns('name', ...)` emits the existing `SymKind::Method` accessor **and** a `HashKeyDef` Symbol in `Package(result_class)` so `$rs->search({ name => ... })` resolves via ordinary `HashKeyAccess`.
- `has_many` / `belongs_to` / `has_one` / `might_have` emit the accessor Method Symbol with `target_class` so chained lookups (`$row->posts->search(...)`) thread through the foreign class's ResultSet scope.
- `__PACKAGE__->table('users')` emits a small Symbol (kind TBD: `DBICTable` or just `Method`) for hover + rename support.

No `DBICColumn` / `DBICRelationship` enum variants needed. Same pipeline as Mojo.

### LSP features fall out for free

Every feature below uses `resolve_symbol` / `refs_to` / `ref_at` — no per-kind wiring:

- **Goto-def on route string `'users#list'`** — `ref_at(cursor)` returns either the `PackageRef` or `MethodCall` Ref (narrowest span wins); navigate to ordinary Controller/Method Symbol.
- **Goto-def on helper `$c->current_user`** — ordinary `MethodCall` Ref, resolved via `namespace_parents` walk from controller → `MojoApp`.
- **Completion inside `enqueue('|')`** — `cursor_context.rs` returns `InsideMojoTaskString`; completion calls `resolve_symbol(prefix, MojoApp(app), VISIBLE)` filtered to `SymKind::MojoTask`.
- **Completion inside `->stash('|')`** — returns `InsideMojoStashKey(ctrl, action)`; completion walks `namespace_parents` from `MojoAction(ctrl, action)`.
- **References on a helper** — `refs_to(helper_sym, VISIBLE)`; works across plugin + host + every controller.
- **Rename across plugin and host** — `refs_to(helper_sym, EDITABLE)`.
- **Hover on `hook('before_dispatch')`** — resolves to the pre-seeded built-in Symbol with its signature.
- **Diagnostics on `enqueue('nonexistent')`** — unified diagnostic rule: `resolve_symbol` empty in a Closed chain (endpoint app) → warn. In a plugin (Open chain), suppressed.

### REPL compatibility

The query engine is purely functional: `FileStore` + `FileAnalysis` + cursor context → results. A REPL parses accumulated code into a `FileAnalysis`, inserts into `FileStore` (role: Open, or a new `REPL` role), and queries the same `resolve_symbol` / `refs_to` pipeline. No REPL-specific data model.

---

## Implementation ordering

> **Blocked on unification spec phases 1 (Namespace), 2 (kill ModuleExports), 3 (FileStore), 4 (resolve), 5 (eager Ref target).** See `docs/prompt-unification-spec.md`. Once those land, Mojo implementation is almost entirely emission rules with no new data structures.

| Phase | Work | Depends on |
|-------|------|-----------|
| **0** | Builder decomposition: `builder.rs` core + `builder_framework.rs` (Moo/Moose/Mojo::Base) + `builder_dbic.rs` (DBIC) + `builder_mojo.rs` (Mojo). Pure refactor, no behavior change. | Unification phases 1, 5 |
| **1a** | `builder_mojo.rs` — app/controller/plugin/Lite detection (writes `mojo_app_namespace`, `is_mojo_lite`) | Phase 0 |
| **1b** | Helper emission: `MojoHelper` Symbols + parallel `Method` Symbols; plugin-load edges | Phase 1a |
| **1c** | Route emission: two Refs from one string at `->to()` sites, resolving to existing Controller/Method Symbols | Phase 1a |
| **1d** | Task emission + `enqueue` arg signature help via `cursor_context.rs` | Phase 1a |
| **1e** | Hook pre-seeded Symbols + `MojoHookName` Refs | Phase 1a |
| **1f** | Route name emission + `url_for` / `redirect_to` Refs | Phase 1c |
| **1g** | Event emission: `MojoEvent` Symbols + `MojoEventOn/Emit/Once/Unsubscribe` Refs; pre-seeded built-in events | Phase 1a |
| **1h** | Stash key emission in `MojoAction` namespaces; under-chain parent edges | Phase 1a |
| **1i** | Plugin Refs + `MojoApp ← MojoPlugin` namespace parent edges | Phase 1a |
| **1j** | Lite DSL detection + `BuiltIn` pre-seeded import Symbols | Phase 1a |
| **1k** | `cursor_context.rs` variants: `InsideMojoRouteString`, `InsideMojoTaskString`, `InsideMojoHookString`, `InsideMojoRouteName`, `InsideMojoEventString`, `InsideMojoStashKey` | Phase 1b–g |
| **2** | Cross-file already works via unified `FileStore`; this phase exists only to add regression coverage | Phases 1b–k |
| **3** | Plugin-chain transitive resolution — `namespace_parents` walk handles it; add recursion cap | Phase 2 |

---

## Files to modify/create

| File | Change |
|------|--------|
| `src/builder.rs` | Extract framework + DBIC code; call post-build plugins after the main walk. `pub(crate)` visibility for shared helpers. |
| `src/builder_framework.rs` | **New.** Moo/Moose/Mojo::Base accessor synthesis + framework detection. Emits Method Symbols + HashKeyDef Symbols into the right Namespaces. |
| `src/builder_dbic.rs` | **New.** DBIC column/relationship synthesis. Emits Method Symbols + HashKeyDef Symbols in `Package(result_class)`. |
| `src/builder_mojo.rs` | **New.** Mojo emission rules: routes, helpers, tasks, hooks, stash, plugins, events, Lite DSL, under-chain edges. |
| `src/file_analysis.rs` | Add `mojo_app_namespace: Option<String>` and `is_mojo_lite: bool` flags. New `SymKind` variants. New `RefKind` variants (or role extension). Pre-seeded BuiltIn Symbol tables. |
| `src/namespace.rs` | `MojoApp`, `MojoPlugin`, `MojoController`, `MojoAction`, `LiteApp` variants on the `Namespace` enum (from unification phase 1). |
| `src/cursor_context.rs` | String-position contexts: `InsideMojoRouteString`, `InsideMojoTaskString`, `InsideMojoHookString`, `InsideMojoRouteName`, `InsideMojoEventString`, `InsideMojoStashKey`. |
| `src/symbols.rs` | Cursor-context routing to `resolve_symbol` with appropriate `SymKind` filter. No bespoke Mojo lookup paths. |
| `src/resolve.rs` | No Mojo-specific code (already handles Symbol/Ref + Namespace walk). |

---

## Tests

> **Note on test assertions.** The test snippets below describe the **intent** of each feature (what should work from a user's perspective). They were originally written against `fa.framework_entities` with `EntityKind` filters; under the unified model they must be rewritten to assert against Symbols + Refs + Namespaces. A typical translation:
>
> ```rust
> // OLD (pre-unification):
> let routes: Vec<_> = fa.mojo_routes().collect();
> assert_eq!(routes[0].target_class.as_deref(), Some("MyApp::Controller::Users"));
>
> // NEW:
> let ref_at_users = fa.ref_at_text("'users#list'", sub_span: "users").unwrap();
> let sym = fa.symbol(ref_at_users.target).unwrap();
> assert_eq!(sym.qualified_name(), "MyApp::Controller::Users");
> // ...or, for the action:
> let ref_at_list = fa.ref_at_text("'users#list'", sub_span: "list").unwrap();
> let sym = fa.symbol(ref_at_list.target).unwrap();
> assert_eq!(sym.qualified_name(), "MyApp::Controller::Users::list");
> ```
>
> The assertions below are retained as-is for intent; translate when implementing.

### Route resolution
```rust
#[test]
fn test_mojo_route_shorthand() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup { my $self = shift; $self->routes->get('/users')->to('users#list') }
    ");
    let routes: Vec<_> = fa.mojo_routes().collect();
    assert_eq!(routes[0].target_class.as_deref(), Some("MyApp::Controller::Users"));
    assert_eq!(routes[0].target_method.as_deref(), Some("list"));
}

#[test]
fn test_mojo_route_hash_style() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->routes->get('/')->to(controller => 'main', action => 'index');
        }
    ");
    let routes: Vec<_> = fa.mojo_routes().collect();
    assert_eq!(routes[0].target_class.as_deref(), Some("MyApp::Controller::Main"));
    assert_eq!(routes[0].target_method.as_deref(), Some("index"));
}

#[test]
fn test_mojo_route_hyphenated_controller() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup { my $self = shift; $self->routes->get('/api')->to('api-users#list') }
    ");
    let routes: Vec<_> = fa.mojo_routes().collect();
    assert_eq!(routes[0].target_class.as_deref(), Some("MyApp::Controller::ApiUsers"));
}

#[test]
fn test_mojo_route_default_action() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup { my $self = shift; $self->routes->get('/dash')->to('dashboard#') }
    ");
    let routes: Vec<_> = fa.mojo_routes().collect();
    assert_eq!(routes[0].target_method.as_deref(), Some("index"));
}

#[test]
fn test_mojo_route_named() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->routes->get('/users/:id')->to('users#show')->name('show_user');
        }
    ");
    assert!(fa.mojo_route_names().any(|e| e.name == "show_user"));
}

#[test]
fn test_mojo_under_route() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            my $auth = $self->routes->under('/')->to('auth#check');
            $auth->get('/dash')->to('dashboard#index');
        }
    ");
    assert_eq!(fa.mojo_routes().count(), 2);
}
```

### Helper synthesis
```rust
#[test]
fn test_mojo_helper_entity_and_symbol() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->helper(current_user => sub { my $c = shift; return $c->session->{uid} });
        }
    ");
    // Entity registered
    let helpers: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoHelper)).collect();
    assert_eq!(helpers.len(), 1);
    assert_eq!(helpers[0].name, "current_user");
    // Symbol also synthesized (for $c->current_user completion)
    let syms: Vec<_> = fa.symbols.iter().filter(|s| s.name == "current_user").collect();
    assert!(!syms.is_empty());
}

#[test]
fn test_mojo_helper_strips_controller_param() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->helper(format_date => sub { my ($c, $date, $fmt) = @_; });
        }
    ");
    let helpers: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoHelper)).collect();
    assert_eq!(helpers[0].params.len(), 2);
    assert_eq!(helpers[0].params[0].name, "$date");
}

#[test]
fn test_mojo_lite_helper() {
    let fa = build_fa("
        use Mojolicious::Lite;
        helper db => sub { state $db = MyDB->new; return $db };
        app->start;
    ");
    let helpers: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoHelper)).collect();
    assert_eq!(helpers.len(), 1);
    assert_eq!(helpers[0].name, "db");
}
```

### Minion tasks
```rust
#[test]
fn test_mojo_task_inline() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->minion->add_task(send_email => sub {
                my ($job, $to, $subject, $body) = @_;
            });
        }
    ");
    let tasks: Vec<_> = fa.mojo_tasks().collect();
    assert_eq!(tasks[0].name, "send_email");
    assert_eq!(tasks[0].params.len(), 3); // $job stripped
    assert_eq!(tasks[0].params[0].name, "$to");
}

#[test]
fn test_mojo_task_class_ref() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->minion->add_task(process => 'MyApp::Task::Process');
        }
    ");
    let tasks: Vec<_> = fa.mojo_tasks().collect();
    assert_eq!(tasks[0].name, "process");
    assert_eq!(tasks[0].target_class.as_deref(), Some("MyApp::Task::Process"));
}
```

### Hooks
```rust
#[test]
fn test_mojo_hook() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            $self->hook(before_dispatch => sub { my $c = shift; });
        }
    ");
    let hooks: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoHook)).collect();
    assert_eq!(hooks.len(), 1);
    assert_eq!(hooks[0].name, "before_dispatch");
}
```

### Stash
```rust
#[test]
fn test_mojo_stash_keys() {
    let fa = build_fa("
        package MyApp::Controller::Users;
        use Mojo::Base 'Mojolicious::Controller';
        sub list {
            my $c = shift;
            $c->stash(users => [], title => 'Users');
            $c->render(template => 'users/list');
        }
    ");
    let keys: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoStashKey))
        .filter(|e| e.scope_class.as_deref() == Some("MyApp::Controller::Users"))
        .collect();
    assert!(keys.iter().any(|e| e.name == "users"));
    assert!(keys.iter().any(|e| e.name == "title"));
}
```

### Lite DSL
```rust
#[test]
fn test_mojolicious_lite_routes() {
    let fa = build_fa("
        use Mojolicious::Lite;
        get '/hello' => sub { my $c = shift; $c->render(text => 'hi') };
        post '/data' => sub { my $c = shift; };
        app->start;
    ");
    assert!(fa.is_mojo_lite);
    assert!(fa.mojo_routes().count() >= 2);
}

#[test]
fn test_mojolicious_lite_suppresses_dsl_keywords() {
    let fa = build_fa("
        use Mojolicious::Lite;
        get '/hello' => sub { };
        app->start;
    ");
    assert!(fa.framework_imports.contains("get"));
    assert!(fa.framework_imports.contains("post"));
    assert!(fa.framework_imports.contains("app"));
}
```

### Plugin resolution
```rust
#[test]
fn test_mojo_plugin_short_name() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup { my $self = shift; $self->plugin('Authentication') }
    ");
    let plugins: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoPlugin)).collect();
    assert_eq!(plugins[0].target_class.as_deref(), Some("Mojolicious::Plugin::Authentication"));
}

#[test]
fn test_mojo_plugin_full_name() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup { my $self = shift; $self->plugin('MyApp::Plugin::Custom') }
    ");
    let plugins: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoPlugin)).collect();
    assert_eq!(plugins[0].target_class.as_deref(), Some("MyApp::Plugin::Custom"));
}
```

### Events
```rust
#[test]
fn test_mojo_event_on_builtin() {
    let fa = build_fa("
        package MyApp::Controller::WS;
        use Mojo::Base 'Mojolicious::Controller';
        sub connect {
            my $c = shift;
            $c->on(message => sub { my ($c, $msg) = @_; });
            $c->on(finish => sub { my ($c, $code, $reason) = @_; });
        }
    ");
    let ons: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoEvent { op: EventOp::On })).collect();
    assert_eq!(ons.len(), 2);
    assert!(ons.iter().any(|e| e.name == "message"));
    assert!(ons.iter().any(|e| e.name == "finish"));
}

#[test]
fn test_mojo_event_user_defined_emit() {
    let fa = build_fa("
        package MyApp::Notifier;
        use Mojo::Base 'Mojo::EventEmitter';
        sub notify {
            my ($self, $msg) = @_;
            $self->emit('notification', $msg);
            $self->emit('log', \"Sent: $msg\");
        }
    ");
    let emits: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoEvent { op: EventOp::Emit })).collect();
    assert_eq!(emits.len(), 2);
    assert!(emits.iter().any(|e| e.name == "notification"));
}

#[test]
fn test_mojo_event_once() {
    let fa = build_fa("
        package MyApp;
        use Mojo::Base 'Mojolicious';
        sub startup {
            my $self = shift;
            my $ua = Mojo::UserAgent->new;
            $ua->once(start => sub { my ($ua, $tx) = @_; });
        }
    ");
    let onces: Vec<_> = fa.entities_by_kind(|k| matches!(k, EntityKind::MojoEvent { op: EventOp::Once })).collect();
    assert_eq!(onces.len(), 1);
    assert_eq!(onces[0].name, "start");
}
```

### DBIC entity integration
```rust
#[test]
fn test_dbic_column_entity_for_search() {
    let fa = build_fa("
        package MySchema::Result::User;
        use base 'DBIx::Class::Core';
        __PACKAGE__->table('users');
        __PACKAGE__->add_columns('name', 'email', 'age');
    ");
    let cols: Vec<_> = fa.dbic_columns_for_class("MySchema::Result::User").collect();
    assert!(cols.iter().any(|e| e.name == "name"));
    assert!(cols.iter().any(|e| e.name == "email"));
    // Accessor symbols should ALSO exist (for $row->name)
    let syms: Vec<_> = fa.symbols.iter().filter(|s| s.name == "name").collect();
    assert!(!syms.is_empty());
}

#[test]
fn test_dbic_relationship_entity_for_join() {
    let fa = build_fa("
        package MySchema::Result::User;
        use base 'DBIx::Class::Core';
        __PACKAGE__->has_many(posts => 'MySchema::Result::Post', 'user_id');
    ");
    let rels: Vec<_> = fa.dbic_relationships_for_class("MySchema::Result::User").collect();
    assert_eq!(rels.len(), 1);
    assert_eq!(rels[0].name, "posts");
    assert_eq!(rels[0].target_class.as_deref(), Some("MySchema::Result::Post"));
}
```

### E2E
```lua
t.test("goto-def: ->to('users#list') jumps to Controller::Users::list", function() end)
t.test("completion: $c-> offers registered helper 'current_user'", function() end)
t.test("completion: enqueue(' offers task names", function() end)
t.test("signature: enqueue('send_email', [|]) shows ($to, $subject, $body)", function() end)
t.test("completion: ->hook(' offers hook names", function() end)
t.test("completion: url_for(' offers route names", function() end)
t.test("goto-def: url_for('show_user') jumps to route definition", function() end)
t.test("completion: ->on(' offers event names for controller", function() end)
t.test("completion: ->on(' offers user-defined event names from emit()", function() end)
t.test("completion: ->emit(' offers registered event names from on()", function() end)
t.test("completion: ->search({ offers DBIC columns for result class", function() end)
t.test("completion: join => ' offers DBIC relationships for result class", function() end)
t.test("no unexpected diagnostics in Lite app", function() end)
```
