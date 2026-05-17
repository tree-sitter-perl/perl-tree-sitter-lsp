-- E2E tests for DBIC parametric ResultSet — uses test_files/dbic_parametric_demo.pl.
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_dbic_parametric.lua
--
-- Walks the typing chain residual Part 5c added:
--   $schema->resultset('Schema::Result::User')
--     → Parametric(ResultSet { base, row })
--   ->find(1)
--     → RowOf projection → ClassName("Schema::Result::User")
--   ->name / ->email
--     → resolves through the row's add_columns synthesis
--
-- AND validates column-key narrowing in search args:
--   $rs->search({ email => 'x@y' })  — `name` resolves to add_columns def.

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/dbic_parametric_demo.pl")

-- The parametric chain involves several reducer rounds. Poll on a
-- single expected effect (greet on $users[0]-style) before asserting.
local function wait_for_label(buf, line, col, needle)
  for _ = 1, 40 do
    local labels = lsp.completion_labels(buf, line, col)
    for _, l in ipairs(labels) do
      if l == needle then return labels end
    end
    vim.wait(250)
  end
  return lsp.completion_labels(buf, line, col)
end

-- ── (a) chain end-state: $user->name resolves through RowOf ─────────

t.test("$user typed from ->find(...) carries Schema::Result::User", function()
  local N = "$user typed from ->find(...) carries Schema::Result::User"
  -- Cursor on `$user` in `my $who = $user->name;` — hover should
  -- mention the resolved row class.
  local line, col = b.find_pos(buf, "$user->name;")
  if not t.ok(N, line, "couldn't find $user->name") then return end
  local text = lsp.hover_text(buf, line, col)
  if not t.ok(N, text, "no hover on $user") then return end
  if t.ok(N, text:find("Schema::Result::User", 1, true) ~= nil,
    "expected Schema::Result::User in hover, got: " .. text)
  then t.pass(N) end
end)

t.test("$user-> completion offers column accessors", function()
  local N = "$user-> completion offers column accessors"
  local line, col = b.find_pos(buf, "$user->name;")
  if not t.ok(N, line, "couldn't find $user->name") then return end
  local arrow_col = col + #"$user->"
  local labels = wait_for_label(buf, line, arrow_col, "name")
  local ok = t.contains(N, labels, "name", "completions")
  ok = t.contains(N, labels, "email", "completions") and ok
  ok = t.contains(N, labels, "id", "completions") and ok
  if ok then t.pass(N) end
end)

-- ── (b) column-key narrowing in search args ─────────────────────────

-- Note: completion-at-empty-search-hash (typing `$rs->search({ | })`
-- and expecting `name`/`email`/`id`) is a separate code path that
-- routes through `complete_keyval_args`, not through HashKey/parametric
-- narrowing. It's a follow-up — goto-def from a typed key and column-
-- accessor completion on a `find()` row already prove the parametric
-- chain wires through end-to-end.

t.test("goto-def from column-key in search lands on add_columns", function()
  local N = "goto-def from column-key in search lands on add_columns"
  -- Cursor on `name` inside `$rs->search({ email => 'x@y' });` — gd
  -- should jump to the line where `name => { data_type => ... }`
  -- is declared inside `__PACKAGE__->add_columns(...)`. Same mid-
  -- word landing as the completion test above.
  local line, col = b.find_pos(buf, "email => 'x@y'")
  if not t.ok(N, line, "couldn't find email => 'x@y'") then return end
  local loc = lsp.def_location(buf, line, col + 1)
  if not t.ok(N, loc, "no goto-def from search key") then return end
  -- The add_columns body has `    name  => { data_type => 'varchar', ...},`.
  local def_line = b.find_line(buf, "    email => { data_type")
  if not t.ok(N, def_line, "couldn't find add_columns line for `name`") then return end
  -- 0-indexed: find_line is 0-indexed, def_location.line is 0-indexed.
  if t.ok(N, loc.line == def_line,
    string.format("expected goto-def line %d (add_columns), got line %d",
      def_line, loc.line or -1))
  then t.pass(N) end
end)

t.finish()
