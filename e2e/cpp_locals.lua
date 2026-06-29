-- E2E: local-var goto-def + hover + outline filtering (cpp-lsp).
vim.opt.rtp:prepend(".")
local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/cpp/locals_sample.cpp")

t.test("goto-def local var: result use -> its decl", function()
  local N = "local-goto"
  local line, col = b.find_pos(buf, "return result")
  local def = lsp.def_line(buf, line, col + 7)  -- on 'result'
  local expected = b.find_line(buf, "int result")
  if t.eq(N, expected, def, "result decl line") then t.pass(N) end
end)

t.test("goto-def param: factor use -> param decl", function()
  local N = "param-goto"
  local line, col = b.find_pos(buf, "factor * 2")
  local def = lsp.def_line(buf, line, col)
  local expected = b.find_line(buf, "int compute")
  if t.eq(N, expected, def, "factor param line") then t.pass(N) end
end)

t.test("hover on param use shows it", function()
  local N = "param-hover"
  local line, col = b.find_pos(buf, "factor * 2")
  local h = lsp.hover_text(buf, line, col)
  if t.ok(N, h and h:find("factor", 1, true), "hover: " .. tostring(h)) then t.pass(N) end
end)

t.test("outline hides locals, keeps fields+funcs", function()
  local N = "outline"
  local names = lsp.symbol_names(buf)
  local set = {}; for _, n in ipairs(names) do set[n] = true end
  if not t.ok(N, set["compute"] and set["Box"] and set["width"], "has compute/Box/width: " .. vim.inspect(names)) then return end
  if t.ok(N, not set["result"] and not set["factor"], "no result/factor: " .. vim.inspect(names)) then t.pass(N) end
end)

t.done()
