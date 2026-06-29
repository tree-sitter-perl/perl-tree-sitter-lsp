-- C++ member completion via sentinel reparse: `bx.` → Box's members.
vim.opt.rtp:prepend(".")
local t = require("test.runner")
local lsp = require("test.lsp")
local b = require("test.buf")

local buf = lsp.open_and_attach("test_files/cpp/members.cpp")

t.test("member completion: bx. offers Box members", function()
  local N = "member completion"
  local line, col = b.find_pos(buf, "bx.")
  if not t.ok(N, line, "no 'bx.' line") then return end
  -- cursor immediately after the dot: col(b) + len("bx.")
  local labels = lsp.completion_labels(buf, line, col + 3)
  if not t.contains(N, labels, "width", "member labels") then return end
  if not t.contains(N, labels, "height", "member labels") then return end
  if t.contains(N, labels, "grow", "member labels") then t.pass(N) end
end)

t.finish()
