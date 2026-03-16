-- E2E tests for framework intelligence: Moo/Moose accessors, roles,
-- Mojo::Base defaults, DBIC load_components, class :does.
--
-- Usage:
--   cargo build --release
--   PERL5LIB=$PWD/test_files/lib nvim --headless --clean -u test_nvim_init.lua -l test_e2e_frameworks.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/frameworks.pl")

-- Wait for cross-file resolution (MyRole::Logging, DBIC Shortcut).
-- Poll by checking completion on $moo-> for "log_info" from the role.
local function wait_for_cross_file(max_secs)
  for _ = 1, max_secs * 4 do
    local line, col = b.find_pos(buf, '$moo->log_info("started")')
    if line then
      local labels = lsp.completion_labels(buf, line, col + 6)
      for _, l in ipairs(labels) do
        if l == "log_info" then return true end
      end
    end
    vim.wait(250)
  end
  return false
end

local cross_file_ready = wait_for_cross_file(15)
if not cross_file_ready then
  io.write("\27[33mWARN: cross-file resolution did not complete within 15s — some tests may fail\27[0m\n")
  io.write("      Make sure PERL5LIB includes test_files/lib\n\n")
end

-- ── 1. Moo accessor completion ───────────────────────────────────────

t.test("completion: $moo-> offers accessor 'name'", function()
  local N = "completion: $moo-> offers accessor 'name'"
  local line, col = b.find_pos(buf, "$moo->name();")
  if not t.ok(N, line, "couldn't find '$moo->name()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "name", "completions") then t.pass(N) end
end)

t.test("completion: $moo-> offers accessor 'count'", function()
  local N = "completion: $moo-> offers accessor 'count'"
  local line, col = b.find_pos(buf, "$moo->name();")
  if not t.ok(N, line, "couldn't find '$moo->name()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "count", "completions") then t.pass(N) end
end)

t.test("completion: $moo-> offers own method 'greet'", function()
  local N = "completion: $moo-> offers own method 'greet'"
  local line, col = b.find_pos(buf, "$moo->greet();")
  if not t.ok(N, line, "couldn't find '$moo->greet()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "greet", "completions") then t.pass(N) end
end)

-- ── 2. Moo role methods via `with` ──────────────────────────────────

t.test("completion: $moo-> offers log_info from MyRole::Logging", function()
  local N = "completion: $moo-> offers log_info from MyRole::Logging"
  local line, col = b.find_pos(buf, '$moo->log_info("started")')
  if not t.ok(N, line, "couldn't find '$moo->log_info()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "log_info", "completions") then t.pass(N) end
end)

t.test("completion: $moo-> offers log_error from MyRole::Logging", function()
  local N = "completion: $moo-> offers log_error from MyRole::Logging"
  local line, col = b.find_pos(buf, '$moo->log_info("started")')
  if not t.ok(N, line, "couldn't find '$moo->log_info()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "log_error", "completions") then t.pass(N) end
end)

t.test("hover: $moo->log_info() shows MyRole::Logging provenance", function()
  local N = "hover: $moo->log_info() shows MyRole::Logging provenance"
  local line, col = b.find_pos(buf, '$moo->log_info("started")')
  if not t.ok(N, line, "couldn't find '$moo->log_info()'") then return end
  local text = lsp.hover_text(buf, line, col + 6)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, text:find("Logging", 1, true),
    "hover should mention 'Logging', got: " .. text) then
    t.pass(N)
  end
end)

-- ── 3. Mojo::Base accessor completion ────────────────────────────────

t.test("completion: $mojo-> offers accessor 'title'", function()
  local N = "completion: $mojo-> offers accessor 'title'"
  local line, col = b.find_pos(buf, "$mojo->title();")
  if not t.ok(N, line, "couldn't find '$mojo->title()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 7)
  if t.contains(N, labels, "title", "completions") then t.pass(N) end
end)

t.test("completion: $mojo-> offers accessor 'items'", function()
  local N = "completion: $mojo-> offers accessor 'items'"
  local line, col = b.find_pos(buf, "$mojo->title();")
  if not t.ok(N, line, "couldn't find '$mojo->title()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 7)
  if t.contains(N, labels, "items", "completions") then t.pass(N) end
end)

-- ── 4. Mojo::Base default type inference ─────────────────────────────

t.test("hover: $mojo->title() mentions String (from default)", function()
  local N = "hover: $mojo->title() mentions String (from default)"
  local line, col = b.find_pos(buf, "$mojo->title();")
  if not t.ok(N, line, "couldn't find '$mojo->title()'") then return end
  local text = lsp.hover_text(buf, line, col + 7)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, text:find("String", 1, true),
    "hover should mention 'String', got: " .. text) then
    t.pass(N)
  end
end)

t.test("hover: $mojo->items() mentions ArrayRef (from sub default)", function()
  local N = "hover: $mojo->items() mentions ArrayRef (from sub default)"
  local line, col = b.find_pos(buf, "$mojo->items();")
  if not t.ok(N, line, "couldn't find '$mojo->items()'") then return end
  local text = lsp.hover_text(buf, line, col + 7)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, text:find("ArrayRef", 1, true),
    "hover should mention 'ArrayRef', got: " .. text) then
    t.pass(N)
  end
end)

-- ── 5. DBIC load_components ──────────────────────────────────────────

t.test("completion: $rs-> offers 'columns' from Shortcut component", function()
  local N = "completion: $rs-> offers 'columns' from Shortcut component"
  local line, col = b.find_pos(buf, "$rs->columns();")
  if not t.ok(N, line, "couldn't find '$rs->columns()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 5)
  if t.contains(N, labels, "columns", "completions") then t.pass(N) end
end)

t.test("completion: $rs-> offers 'order_by' from Shortcut component", function()
  local N = "completion: $rs-> offers 'order_by' from Shortcut component"
  local line, col = b.find_pos(buf, "$rs->columns();")
  if not t.ok(N, line, "couldn't find '$rs->columns()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 5)
  if t.contains(N, labels, "order_by", "completions") then t.pass(N) end
end)

t.test("completion: $rs-> offers own method 'active'", function()
  local N = "completion: $rs-> offers own method 'active'"
  local line, col = b.find_pos(buf, "$rs->active();")
  if not t.ok(N, line, "couldn't find '$rs->active()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 5)
  if t.contains(N, labels, "active", "completions") then t.pass(N) end
end)

-- ── 6. class :does ───────────────────────────────────────────────────

t.test("completion: $report-> offers 'generate' (own method)", function()
  local N = "completion: $report-> offers 'generate' (own method)"
  local line, col = b.find_pos(buf, "$report->generate();")
  if not t.ok(N, line, "couldn't find '$report->generate()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 9)
  if t.contains(N, labels, "generate", "completions") then t.pass(N) end
end)

t.test("completion: $report-> offers 'to_string' from :does(Printable)", function()
  local N = "completion: $report-> offers 'to_string' from :does(Printable)"
  local line, col = b.find_pos(buf, "$report->to_string();")
  if not t.ok(N, line, "couldn't find '$report->to_string()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 9)
  if t.contains(N, labels, "to_string", "completions") then t.pass(N) end
end)

t.test("goto-def: $report->to_string() jumps to Printable::to_string", function()
  local N = "goto-def: $report->to_string() jumps to Printable::to_string"
  local line, col = b.find_pos(buf, "$report->to_string();")
  if not t.ok(N, line, "couldn't find '$report->to_string()'") then return end
  local def = lsp.def_line(buf, line, col + 9)
  local expected = b.find_line(buf, "method to_string")
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

-- ── done ─────────────────────────────────────────────────────────────

t.finish()
