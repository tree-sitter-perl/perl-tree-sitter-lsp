-- E2E tests for perl-lsp via headless nvim
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e.lua

vim.opt.rtp:prepend(".")

local t    = require("test.runner")
local lsp  = require("test.lsp")
local b    = require("test.buf")

local buf = lsp.open_and_attach("test_files/sample.pl")

-- ── tests ────────────────────────────────────────────────────────────

t.test("documentSymbol returns packages and classes", function()
  local N = "documentSymbol returns packages and classes"
  local names = lsp.symbol_names(buf)
  if not t.ok(N, #names > 0, "no symbols returned") then return end
  local ok = t.contains(N, names, "Calculator", "symbols")
  ok = t.contains(N, names, "Point", "symbols") and ok
  if ok then t.pass(N) end
end)

t.test("goto-def: $calc->add jumps to sub add", function()
  local N = "goto-def: $calc->add jumps to sub add"
  local line, col = b.find_pos(buf, "->add(2, 3)")
  if not t.ok(N, line, "couldn't find '->add(2, 3)'") then return end
  local def = lsp.def_line(buf, line, col + 2)
  local expected = b.find_line(buf, "^sub add ")
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: $p->magnitude jumps to method", function()
  local N = "goto-def: $p->magnitude jumps to method"
  local line, col = b.find_pos(buf, "$p->magnitude()")
  if not t.ok(N, line, "couldn't find '$p->magnitude()'") then return end
  local def = lsp.def_line(buf, line, col + 4)
  local expected = b.find_line(buf, "method magnitude")
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: $p->x jumps to field reader (via inserted line)", function()
  local N = "goto-def: $p->x jumps to field reader"
  local insert_at = b.append(buf, { "$p->x;" })
  vim.wait(500)

  local line, col = b.find_pos(buf, "$p->x;")
  if not t.ok(N, line, "couldn't find inserted '$p->x;'") then return end
  local def = lsp.def_line(buf, line, col + 4)
  local expected = b.find_line(buf, "field $x :param :reader")

  b.remove(buf, insert_at, insert_at + 1)
  vim.wait(200)

  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("completion: $calc-> returns Calculator methods", function()
  local N = "completion: $calc-> returns Calculator methods"
  local line, col = b.find_pos(buf, "$calc->add(2, 3)")
  if not t.ok(N, line, "couldn't find '$calc->add'") then return end
  local labels = lsp.completion_labels(buf, line, col + 7)
  local ok = t.contains(N, labels, "add", "completions")
  ok = t.contains(N, labels, "subtract", "completions") and ok
  ok = t.contains(N, labels, "get_history", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("completion: $p-> returns Point methods", function()
  local N = "completion: $p-> returns Point methods"
  local line, col = b.find_pos(buf, "$p->magnitude()")
  if not t.ok(N, line, "couldn't find '$p->magnitude()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 4)
  local ok = t.contains(N, labels, "magnitude", "completions")
  ok = t.contains(N, labels, "to_string", "completions") and ok
  ok = t.contains(N, labels, "x", "completions") and ok
  ok = t.contains(N, labels, "new", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("hover: sub add shows signature", function()
  local N = "hover: sub add shows signature"
  local line = b.find_line(buf, "^sub add ")
  if not t.ok(N, line, "couldn't find 'sub add'") then return end
  local text = lsp.hover_text(buf, line, 5)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, type(text) == "string" and text:find("add"), "hover doesn't mention 'add'") then
    t.pass(N)
  end
end)

t.test("references: $pi finds declaration and usage", function()
  local N = "references: $pi finds declaration and usage"
  local decl = b.find_line(buf, "my $pi = ")
  if not t.ok(N, decl, "couldn't find '$pi' declaration") then return end
  local refs = lsp.reference_lines(buf, decl, 4)
  if not t.ok(N, #refs >= 2, string.format("expected >=2 refs, got %d", #refs)) then return end
  local usage = b.find_line(buf, "%$pi %* %$radius")
  if not t.ok(N, usage, "couldn't find $pi usage line") then return end
  if t.contains(N, refs, usage, "ref lines") then t.pass(N) end
end)

-- ── done ─────────────────────────────────────────────────────────────

t.finish()
