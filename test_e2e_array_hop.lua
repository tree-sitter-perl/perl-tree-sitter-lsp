-- E2E tests for the array intelligence hop — uses test_files/array_hop_demo.pl.
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_array_hop.lua
--
-- The fixture chains: const fold (DEFAULT_NAME → 'alice') → Mojo helper
-- synth → coderef return → array contribution → array projection →
-- cross-file completion on `$users[0]->`. Single assertion: completion
-- past the `->` offers `greet`, `email`, `name` (Some::User methods,
-- including the Mojo::Base `has 'name'` accessor).

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/array_hop_demo.pl")

-- Wait for cross-file resolution: `Some::User` lives in
-- test_files/lib/Some/User.pm and we need the workspace index to find
-- it before invocant typing narrows the completion list.
local function wait_for_some_user(buf, line, col)
  for _ = 1, 40 do
    local labels = lsp.completion_labels(buf, line, col)
    for _, l in ipairs(labels) do
      if l == "greet" then return labels end
    end
    vim.wait(250)
  end
  return lsp.completion_labels(buf, line, col)
end

t.test("array-hop: $users[0]-> offers Some::User methods", function()
  local N = "array-hop: $users[0]-> offers Some::User methods"
  local line, col = b.find_pos(buf, "$users[0]->greet();")
  if not t.ok(N, line, "couldn't find '$users[0]->greet();'") then return end
  -- col is at the `$`; jump past `$users[0]->` so the cursor sits
  -- just after the arrow and the trigger context is `Method`.
  local arrow_col = col + #"$users[0]->"
  local labels = wait_for_some_user(buf, line, arrow_col)
  local ok = t.contains(N, labels, "greet", "completions")
  ok = t.contains(N, labels, "email", "completions") and ok
  ok = t.contains(N, labels, "name",  "completions") and ok
  if ok then t.pass(N) end
end)

t.finish()
