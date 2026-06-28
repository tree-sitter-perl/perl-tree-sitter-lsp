-- E2E: cpp-lsp via headless nvim (perl-lsp --features cpp).
-- Usage: PERL_LSP_BIN=target/release/perl-lsp \
--          nvim --headless --clean -u e2e/init_cpp.lua -l e2e/cpp.lua
vim.opt.rtp:prepend(".")
local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/cpp/sample.cpp")

t.test("documentSymbol returns C++ classes and functions", function()
  local N = "documentSymbol C++"
  local names = lsp.symbol_names(buf)
  if not t.ok(N, #names > 0, "no symbols returned") then return end
  local ok = t.contains(N, names, "Shape", "symbols")
  ok = t.contains(N, names, "Circle", "symbols") and ok
  ok = t.contains(N, names, "compute", "symbols") and ok
  ok = t.contains(N, names, "main", "symbols") and ok
  if ok then t.pass(N) end
end)

t.test("goto-def: compute(21) jumps to int compute", function()
  local N = "goto-def compute"
  local line, col = b.find_pos(buf, "compute(21)")
  if not t.ok(N, line, "couldn't find 'compute(21)'") then return end
  local def = lsp.def_line(buf, line, col)
  local expected = b.find_line(buf, "^int compute")
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.finish()
