-- E2E: function-like macro calls SEE THROUGH to the wrapped function.
vim.opt.rtp:prepend(".")
local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")
local buf = lsp.open_and_attach("test_files/cpp/macro_calls.c")

t.test("macro-wrapper call wrap() -> realFunc", function()
  local N = "wrap-seethrough"
  local l, c = b.find_pos(buf, "wrap(5)")
  local def = lsp.def_line(buf, l, c)
  if t.eq(N, b.find_line(buf, "int realFunc"), def, "wrap resolves to realFunc") then t.pass(N) end
end)

t.test("thread-context wrapper newThing() -> Perl_newThing", function()
  local N = "newThing-seethrough"
  local l, c = b.find_pos(buf, "newThing(7)")
  local def = lsp.def_line(buf, l, c)
  if t.eq(N, b.find_line(buf, "int Perl_newThing"), def, "newThing resolves to Perl_newThing") then t.pass(N) end
end)
t.done()
