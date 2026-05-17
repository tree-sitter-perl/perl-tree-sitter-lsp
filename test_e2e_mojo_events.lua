-- E2E tests for the mojo-events plugin — uses the cross-file
-- consumer fixture (test_files/plugin_events_consumer.pl).
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_mojo_events.lua
--
-- The producer (test_files/lib/Demo/Plugin/Producer.pm) wires up
-- 'ready' / 'failed' / 'done' handlers via $self->on(EVT, sub {...}).
-- The consumer (open file here) calls $p->emit(EVT, ...). Plugin
-- emits HashKeyDef on the producer side + HashKeyAccess on the
-- consumer side, paired by class+name — cross-file `gr` reaches
-- both, and sig help on emit() surfaces the producer's handler
-- signature.

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/plugin_events_consumer.pl")

-- Cross-file resolution needs the workspace index to warm up. Poll
-- on a known cross-file effect (sig help with a producer param)
-- before asserting.
local function wait_for_xfile_sig(buf, line, col, needle, ms_budget)
  ms_budget = ms_budget or 10000
  local elapsed = 0
  while elapsed < ms_budget do
    local sig = lsp.signature_label(buf, line, col)
    if sig and sig:find(needle, 1, true) then return sig end
    vim.wait(250)
    elapsed = elapsed + 250
  end
  return lsp.signature_label(buf, line, col)
end

-- ── cross-file sig help on $p->emit('ready', |) ─────────────────────

t.test("emit sig help surfaces cross-file handler params", function()
  local N = "emit sig help surfaces cross-file handler params"
  -- The fixture line: `$p->emit('ready', );` — cursor right after
  -- the comma, sig help should see Producer.pm's
  -- `->on('ready', sub ($self, $ts, $who) {...})` and surface ($ts, $who).
  local line, col = b.find_pos(buf, "$p->emit('ready', );")
  if not t.ok(N, line, "couldn't find emit('ready') site") then return end
  -- Skip past `$p->emit('ready', ` to land in the args slot.
  local arg_col = col + #"$p->emit('ready', "
  local sig = wait_for_xfile_sig(buf, line, arg_col, "$ts")
  if not t.ok(N, sig, "no sig help inside emit('ready', |)") then return end
  if t.ok(N, sig:find("$ts", 1, true) ~= nil,
    "expected $ts in handler sig, got: " .. sig)
  then t.pass(N) end
end)

-- ── const-folded event name still resolves ──────────────────────────

t.test("const-folded event name flows through to sig help", function()
  local N = "const-folded event name flows through to sig help"
  -- `my $evt = 'done'; $p->emit($evt, );` — the builder's
  -- constant_strings table resolves $evt before the plugin sees it,
  -- so sig help should resolve `done`'s handler (param $reason).
  local line, col = b.find_pos(buf, "$p->emit($evt, );")
  if not t.ok(N, line, "couldn't find $p->emit($evt, );") then return end
  local arg_col = col + #"$p->emit($evt, "
  local sig = wait_for_xfile_sig(buf, line, arg_col, "$reason")
  if not t.ok(N, sig, "no sig help on const-folded emit") then return end
  if t.ok(N, sig:find("$reason", 1, true) ~= nil,
    "expected $reason from const-folded `done` handler, got: " .. sig)
  then t.pass(N) end
end)

-- ── goto-def on emit string lands in the producer's ->on call ───────

t.test("gd on event name in emit jumps to ->on call cross-file", function()
  local N = "gd on event name in emit jumps to ->on call cross-file"
  -- Cursor on 'ready' inside `$p->emit('ready', );` — gd should
  -- jump to the FIRST ->on('ready', ...) in Producer.pm.
  local line, col = b.find_pos(buf, "$p->emit('ready', );")
  if not t.ok(N, line, "couldn't find emit('ready') site") then return end
  -- Land cursor on the `r` of 'ready' inside the string.
  local ready_col = col + #"$p->emit('"
  local loc = lsp.def_location(buf, line, ready_col)
  if not t.ok(N, loc, "no goto-def from event name") then return end
  if t.ok(N, loc.uri and loc.uri:find("Producer.pm", 1, true) ~= nil,
    "expected goto-def to land in Producer.pm, got uri: " .. (loc.uri or "<nil>"))
  then t.pass(N) end
end)

-- ── diagnostics (Mojo internals + cross-file warmup) ─────────────────
lsp.assert_no_diagnostics(t, buf, {
  "'on' is not defined",
  "'once' is not defined",
  "'emit' is not defined",
  "'unsubscribe' is not defined",
  "'has_subscribers' is not defined",
}, 5)

t.finish()
