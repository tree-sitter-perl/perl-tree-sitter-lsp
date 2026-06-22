-- Headless verification that the demo fixture exercises the features we record.
-- Run from repo root:
--   nvim --headless --clean -u test_nvim_init.lua -l demo/verify.lua
package.path = package.path .. ";./lua/?.lua"
local lsp = require("test.lsp")

local function has(list, want)
  for _, v in ipairs(list) do if v == want then return true end end
  return false
end

local buf = lsp.open_and_attach("demo/app.pl")

-- Poll for cross-file readiness: $acct->  must surface Account's methods, which
-- requires inferring make_account()'s return type (cross-file) -> Account.
local labels = {}
for _ = 1, 80 do
  labels = lsp.completion_labels(buf, 7, 7) -- after `$acct->` (line 8, 0-indexed 7)
  if has(labels, "deposit") and has(labels, "describe") then break end
  vim.wait(250)
end

io.write("completion@$acct-> : " .. table.concat(labels, ", ") .. "\n")
io.write("  deposit?  " .. tostring(has(labels, "deposit")) .. "\n")
io.write("  describe? " .. tostring(has(labels, "describe")) .. "\n")

-- $acct typed through the imported function's return type
local hov = lsp.hover_text(buf, 5, 4) -- $acct in `my $acct = make_account(...)`
io.write("hover@$acct        : " .. tostring(hov and hov:gsub("\n", " ⏎ ")) .. "\n")

-- goto-def on the IMPORTED function make_account -> Bank.pm (default export)
local def = lsp.def_location(buf, 5, 12) -- `make_account` on line 6
io.write("goto-def make_account: " .. tostring(def and (def.uri:gsub(".*/demo/", "") .. " L" .. def.line)) .. "\n")

-- goto-def on a method (deposit) -> Account.pm
local mdef = lsp.def_location(buf, 7, 9) -- `deposit` in `$acct->deposit`
io.write("goto-def deposit   : " .. tostring(mdef and (mdef.uri:gsub(".*/demo/", "") .. " L" .. mdef.line)) .. "\n")

vim.cmd("qa!")
