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

t.test("completion: $self-> inside method returns sibling methods", function()
  local N = "completion: $self-> inside method returns sibling methods"
  -- to_string has: my $m = $self->magnitude();
  local line, col = b.find_pos(buf, "$self->magnitude()")
  if not t.ok(N, line, "couldn't find '$self->magnitude()' in file") then return end
  col = col + 7  -- after "$self->"
  local labels = lsp.completion_labels(buf, line, col)
  local ok = t.contains(N, labels, "magnitude", "completions")
  ok = t.contains(N, labels, "to_string", "completions") and ok
  ok = t.contains(N, labels, "x", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("goto-def: $self->magnitude inside method jumps to method", function()
  local N = "goto-def: $self->magnitude inside method"
  local line, col = b.find_pos(buf, "$self->magnitude()")
  if not t.ok(N, line, "couldn't find '$self->magnitude()'") then return end
  col = col + 7  -- on "magnitude"
  local def = lsp.def_line(buf, line, col)
  local expected = b.find_line(buf, "^    method magnitude")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
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

t.test("completion: $db_config-> offers hash keys from return type", function()
  local N = "completion: $db_config-> offers hash keys from return type"
  local line, col = b.find_pos(buf, "$db_config->{host}")
  if not t.ok(N, line, "couldn't find '$db_config->{host}'") then return end
  local labels = lsp.completion_labels(buf, line, col + 13) -- after "$db_config->{"
  local ok = t.contains(N, labels, "host", "completions")
  ok = t.contains(N, labels, "port", "completions") and ok
  ok = t.contains(N, labels, "name", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("goto-def: chained $calc->get_self()->add resolves to sub add", function()
  local N = "goto-def: chained $calc->get_self()->add resolves to sub add"
  local line, col = b.find_pos(buf, "->get_self()->add(1, 2)")
  if not t.ok(N, line, "couldn't find chained call") then return end
  -- Position cursor on "add" in the chain: skip "->get_self()->" (14 chars) then on "add"
  local def = lsp.def_line(buf, line, col + 14)
  local expected = b.find_line(buf, "^sub add ")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: $db_config->{host} jumps to key in get_config return", function()
  local N = "goto-def: $db_config->{host} jumps to key in get_config return"
  local line, col = b.find_pos(buf, "$db_config->{host}")
  if not t.ok(N, line, "couldn't find '$db_config->{host}'") then return end
  -- cursor on "host": after "$db_config->{" = 13 chars
  local def = lsp.def_line(buf, line, col + 13)
  local expected = b.find_line(buf, "host => \"localhost\"")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: $calc->get_self->get_config->{host} jumps to return hash key", function()
  local N = "goto-def: chained get_config->{host}"
  local line, col = b.find_pos(buf, "$calc->get_self->get_config->{host}")
  if not t.ok(N, line, "couldn't find chained hash access") then return end
  -- cursor on "host": "$calc->get_self->get_config->{" = 30 chars, so col+30
  local def = lsp.def_line(buf, line, col + 30)
  local expected = b.find_line(buf, "host => \"localhost\"")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("completion: $calc->get_self->get_config->{ offers hash keys", function()
  local N = "completion: $calc->get_self->get_config->{ offers hash keys"
  local line, col = b.find_pos(buf, "$calc->get_self->get_config->{host}")
  if not t.ok(N, line, "couldn't find chained hash access") then return end
  -- cursor after "{": $calc->get_self->get_config->{ = 31 chars, cursor at col+31
  local labels = lsp.completion_labels(buf, line, col + 31)
  local ok = t.contains(N, labels, "host", "completions")
  ok = t.contains(N, labels, "port", "completions") and ok
  ok = t.contains(N, labels, "name", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("goto-def: x in Point->new(x => 3) jumps to field $x :param", function()
  local N = "goto-def: x in Point->new(x => 3) jumps to field $x :param"
  local line, col = b.find_pos(buf, "Point->new(x => 3")
  if not t.ok(N, line, "couldn't find 'Point->new(x => 3'") then return end
  -- cursor on "x": after "Point->new(" = 11 chars
  local def = lsp.def_line(buf, line, col + 11)
  local expected = b.find_line(buf, "field $x :param :reader")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: verbose in Calculator->new(verbose => 1) jumps to bless hash key", function()
  local N = "goto-def: verbose in Calculator->new(verbose => 1) jumps to bless hash key"
  local line, col = b.find_pos(buf, "Calculator->new(verbose")
  if not t.ok(N, line, "couldn't find 'Calculator->new(verbose'") then return end
  -- cursor on "verbose": after "Calculator->new(" = 16 chars
  local def = lsp.def_line(buf, line, col + 16)
  local expected = b.find_line(buf, "verbose => $args{verbose}")
  if not t.ok(N, def, "no definition result") then return end
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

-- ── done ─────────────────────────────────────────────────────────────

t.finish()
