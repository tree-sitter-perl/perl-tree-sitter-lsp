-- C++/Python member completion via sentinel reparse generalizes: `w.` → Widget's methods.
vim.opt.rtp:prepend(".")
local t = require("test.runner")
local lsp = require("test.lsp")
local b = require("test.buf")

local buf = lsp.open_and_attach("test_files/python/members.py")

t.test("member completion: w. offers Widget methods", function()
  local N = "py member completion"
  local line, col = b.find_pos(buf, "w.")
  if not t.ok(N, line, "no 'w.' line") then return end
  local labels = lsp.completion_labels(buf, line, col + 2)
  if not t.contains(N, labels, "grow", "member labels") then return end
  if t.contains(N, labels, "shrink", "member labels") then t.pass(N) end
end)

t.finish()
