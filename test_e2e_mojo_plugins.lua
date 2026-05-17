-- E2E tests for the Mojo plugin family — uses test_files/plugin_mojo_demo.pl.
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_mojo_plugins.lua
--
-- Covers four plugins in one fixture (the demo file deliberately
-- exercises all of them):
--
--   mojo-helpers   — $app->helper('name' => sub {...}) → synthesized Method
--                    on Mojolicious::Controller, including dotted chains
--                    ($c->users->create)
--   mojo-routes    — $r->get('/x')->to('Users#list') → cross-file MethodCall
--                    ref on the controller action
--   mojo-lite      — top-level `get '/path' => sub {...}` → Handler with
--                    url_for dispatch keyed by URL path
--   minion         — $minion->add_task(NAME => sub {...}) → Handler keyed by
--                    task name; $minion->enqueue(NAME, [args]) references it

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/plugin_mojo_demo.pl")

-- Outline names from plugin-synthesized symbols carry a `<word>` prefix
-- so SymbolKind's lossy mapping (Helper/Action/Route → FUNCTION) doesn't
-- erase the kind cue. We assert by substring, not equality.
local function names_contain(names, needle)
  for _, n in ipairs(names) do
    if n:find(needle, 1, true) then return true end
  end
  return false
end

local function assert_outline_has(test_name, needle)
  local names = lsp.symbol_names(buf)
  if not t.ok(test_name, names_contain(names, needle),
    "outline missing '" .. needle .. "', got: [" .. table.concat(names, ", ") .. "]")
  then return false end
  return true
end

-- ── mojo-helpers ────────────────────────────────────────────────────

t.test("mojo-helpers: outline includes synthesized helpers", function()
  local N = "mojo-helpers: outline includes synthesized helpers"
  local ok = assert_outline_has(N, "<helper> current_user")
  ok = assert_outline_has(N, "<helper> users.create") and ok
  ok = assert_outline_has(N, "<helper> users.delete") and ok
  -- Three-level dotted chain.
  ok = assert_outline_has(N, "<helper> admin.users.purge") and ok
  if ok then t.pass(N) end
end)

t.test("mojo-helpers: helper sig shows params in outline name", function()
  local N = "mojo-helpers: helper sig shows params in outline name"
  -- `users.create` registers `sub ($c, $name, $email)` — the helper
  -- params (minus the $c invocant) should land in the outline name
  -- so users see the helper's signature at a glance.
  if assert_outline_has(N, "users.create ($name, $email)") then
    t.pass(N)
  end
end)

-- ── mojo-routes (controller action dispatch) ─────────────────────────

t.test("mojo-routes: ->to('Ctrl#action') becomes a <action> outline entry", function()
  local N = "mojo-routes: ->to('Ctrl#action') becomes a <action> outline entry"
  local ok = assert_outline_has(N, "<action> Users#list")
  ok = assert_outline_has(N, "<action> Users#create") and ok
  -- Multi-segment controller — the `Admin::Dashboard#index` form is
  -- the load-bearing assertion that the action parser handles
  -- nested package names.
  ok = assert_outline_has(N, "<action> Admin::Dashboard#index") and ok
  if ok then t.pass(N) end
end)

-- ── mojo-lite (top-level route verbs) ────────────────────────────────

t.test("mojo-lite: top-level verbs register routes keyed by path", function()
  local N = "mojo-lite: top-level verbs register routes keyed by path"
  -- get / post / any are exported from Mojolicious::Lite; plugin
  -- emits a Handler-kind symbol with the path as name.
  local ok = assert_outline_has(N, "<route> GET /users/profile")
  ok = assert_outline_has(N, "<route> POST /users/profile") and ok
  ok = assert_outline_has(N, "<route> ANY /fallback") and ok
  if ok then t.pass(N) end
end)

-- ── minion (task registration) ───────────────────────────────────────

t.test("minion: add_task registers tasks with the callback's signature", function()
  local N = "minion: add_task registers tasks with the callback's signature"
  -- `add_task(send_email => sub ($job, $to, $subject, $body) {})` —
  -- $job is the invocant (stripped); the visible params surface in
  -- the outline so the user can read the task's contract at a glance.
  local ok = assert_outline_has(N, "<task> send_email ($to, $subject, $body)")
  ok = assert_outline_has(N, "<task> resize_image ($path, $width, $height)") and ok
  if ok then t.pass(N) end
end)

t.test("minion: enqueue sig help surfaces the registered task's params", function()
  local N = "minion: enqueue sig help surfaces the registered task's params"
  -- `$minion->enqueue(send_email => ['alice@example.com', 'hi',  'body']);`
  -- — sig help on the first inner arg should reflect the registered
  -- task signature ($to, $subject, $body).
  local line, col = b.find_pos(buf, "enqueue(send_email  => ['alice")
  if not t.ok(N, line, "couldn't find enqueue site") then return end
  -- Position past `enqueue(send_email  => [` — cursor sits at the
  -- first arrayref element, which is arg 0 of the registered task.
  local search_col = col + #"enqueue(send_email  => ['"
  local sig = lsp.signature_label(buf, line, search_col)
  if not t.ok(N, sig, "no sig help at enqueue first arg") then return end
  -- The task registered `$to` as the first non-invocant param.
  -- Plugin must surface it; otherwise the sig help is just enqueue's
  -- own shape, which would NOT mention $to.
  if t.ok(N, sig:find("$to", 1, true) ~= nil,
    "sig help should mention '$to', got: " .. sig)
  then t.pass(N) end
end)

-- ── diagnostics ──────────────────────────────────────────────────────
-- Allow Mojo internals we don't index (render, session, redirect_to,
-- enqueue_p, perform_jobs) — the demo file uses them but they're
-- declared in Mojo core which isn't in the workspace.
lsp.assert_no_diagnostics(t, buf, {
  "'render' is not defined",
  "'session' is not defined",
  "'redirect_to' is not defined",
  "'enqueue_p' is not defined",
  "'perform_jobs' is not defined",
  "'on' is not defined",
  "'emit' is not defined",
}, 5)

t.finish()
