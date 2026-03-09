-- E2E tests for type inference — uses test_files/type_obstacle_course.pl
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_types.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/type_obstacle_course.pl")

-- helper: hover at (search_str, col_offset) and check it mentions type_str
local function hover_has(name, search, offset, type_str)
  local line, col = b.find_pos(buf, search)
  if not t.ok(name, line, "couldn't find '" .. search .. "'") then return end
  local text = lsp.hover_text(buf, line, col + offset)
  if not t.ok(name, text, "no hover result") then return end
  if t.ok(name, text:find(type_str, 1, true),
    "hover should mention '" .. type_str .. "', got: " .. text) then
    t.pass(name)
  end
end

-- helper: inlay hint on a line mentions a type
local function hint_has(name, pattern, type_str, start)
  local line = b.find_line(buf, pattern, start)
  if not t.ok(name, line, "couldn't find line matching '" .. pattern .. "'") then return end
  local hints = lsp.inlay_hints(buf, line, line)
  if not t.ok(name, #hints > 0, "no inlay hints") then return end
  local found = false
  for _, h in ipairs(hints) do
    local label = type(h.label) == "string" and h.label or (h.label[1] and h.label[1].value or "")
    if label:find(type_str, 1, true) then found = true; break end
  end
  if t.ok(name, found, "no hint mentioning '" .. type_str .. "'") then t.pass(name) end
end

-- ── 1. Literal constructors ────────────────────────────────────────

t.test("hover: $href shows HashRef", function()
  hover_has("hover: $href shows HashRef", "$href = {};", 0, "HashRef")
end)

t.test("hover: $aref shows ArrayRef", function()
  hover_has("hover: $aref shows ArrayRef", "$aref = [];", 0, "ArrayRef")
end)

t.test("hover: $cref shows CodeRef", function()
  hover_has("hover: $cref shows CodeRef", "$cref = sub { 42 };", 0, "CodeRef")
end)

t.test("hover: $re shows Regexp", function()
  hover_has("hover: $re shows Regexp", "$re   = qr/pattern/;", 0, "Regexp")
end)

-- ── 4. Object method calls ─────────────────────────────────────────

t.test("hover: $user shows User type", function()
  hover_has("hover: $user shows User type", "$user = User->new()", 0, "User")
end)

t.test("completion: $user-> offers name and email", function()
  local N = "completion: $user-> offers name and email"
  local line, col = b.find_pos(buf, "$user->name();")
  if not t.ok(N, line, "couldn't find '$user->name()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 7)
  local ok = t.contains(N, labels, "name", "completions")
  ok = t.contains(N, labels, "email", "completions") and ok
  if ok then t.pass(N) end
end)

-- ── 5. Bless patterns ──────────────────────────────────────────────

t.test("hint: sub new in Widget shows → Widget", function()
  local N = "hint: sub new in Widget shows → Widget"
  local pkg_line = b.find_line(buf, "^package Widget;")
  if not t.ok(N, pkg_line, "no Widget package") then return end
  local sub_line = b.find_line(buf, "^sub new ", pkg_line + 2)
  if not t.ok(N, sub_line, "no sub new after Widget") then return end
  local hints = lsp.inlay_hints(buf, sub_line, sub_line)
  if not t.ok(N, #hints > 0, "no inlay hints") then return end
  local found = false
  for _, h in ipairs(hints) do
    local label = type(h.label) == "string" and h.label or (h.label[1] and h.label[1].value or "")
    if label:find("Widget", 1, true) then found = true; break end
  end
  if t.ok(N, found, "no hint mentioning Widget") then t.pass(N) end
end)

-- ── 6. Return value tracking ───────────────────────────────────────

t.test("hover: $db shows HashRef from return type", function()
  hover_has("hover: $db shows HashRef from return type",
    "$db  = $cfg->get_db_config()", 0, "HashRef")
end)

t.test("hover: $tags shows ArrayRef from return type", function()
  hover_has("hover: $tags shows ArrayRef from return type",
    "$tags = $cfg->get_tags()", 0, "ArrayRef")
end)

t.test("hover: $handler shows CodeRef from return type", function()
  hover_has("hover: $handler shows CodeRef from return type",
    "$handler = $cfg->get_handler()", 0, "CodeRef")
end)

t.test("completion: $cfg-> offers Config methods", function()
  local N = "completion: $cfg-> offers Config methods"
  local line, col = b.find_pos(buf, "$cfg->get_db_config()")
  if not t.ok(N, line, "couldn't find '$cfg->get_db_config()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  local ok = t.contains(N, labels, "get_db_config", "completions")
  ok = t.contains(N, labels, "get_tags", "completions") and ok
  ok = t.contains(N, labels, "get_handler", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("completion detail: $cfg->get_db_config shows → HashRef", function()
  local N = "completion detail: $cfg->get_db_config shows → HashRef"
  local line, col = b.find_pos(buf, "$cfg->get_db_config()")
  if not t.ok(N, line, "couldn't find call") then return end
  local items = lsp.completion_items(buf, line, col + 6)
  local found = false
  for _, item in ipairs(items) do
    if item.label == "get_db_config" and item.detail and item.detail:find("HashRef") then
      found = true; break
    end
  end
  if t.ok(N, found, "no 'get_db_config' with HashRef detail") then t.pass(N) end
end)

t.test("completion: $db->{} offers hash keys from return type", function()
  local N = "completion: $db->{} offers hash keys from return type"
  local line, col = b.find_pos(buf, "$db->{host};")
  if not t.ok(N, line, "couldn't find '$db->{host}'") then return end
  -- col+6 = on 'h' inside braces (after "$db->{")
  local labels = lsp.completion_labels(buf, line, col + 6)
  local ok = t.contains(N, labels, "host", "completions")
  ok = t.contains(N, labels, "port", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("hint: sub get_db_config shows → HashRef", function()
  hint_has("hint: sub get_db_config shows → HashRef", "^sub get_db_config", "HashRef")
end)

t.test("hint: sub get_tags shows → ArrayRef", function()
  hint_has("hint: sub get_tags shows → ArrayRef", "^sub get_tags", "ArrayRef")
end)

t.test("hint: sub get_handler shows → CodeRef", function()
  hint_has("hint: sub get_handler shows → CodeRef", "^sub get_handler", "CodeRef")
end)

-- ── 8. Operator-based type narrowing ───────────────────────────────

t.test("hover: $x in numeric_ops shows Numeric", function()
  -- hover on the $x in "my $a = $x + 1;" — $x starts 8 chars after "my $a = "
  hover_has("hover: $x in numeric_ops shows Numeric",
    "my $a = $x + 1;", 8, "Numeric")
end)

t.test("hover: $s in string_ops shows String", function()
  hover_has("hover: $s in string_ops shows String",
    'my $a = $s . " world";', 8, "String")
end)

-- ── 10. Re-assignment changes type ─────────────────────────────────

t.test("hover: $x after reassignment to [] shows ArrayRef", function()
  hover_has("hover: $x after reassignment to [] shows ArrayRef",
    '$x->[0] = "hello";', 0, "ArrayRef")
end)

-- ── 13. Builtin return types ───────────────────────────────────────

t.test("hover: $len from scalar @items shows Numeric", function()
  hover_has("hover: $len from scalar @items shows Numeric",
    "$len  = scalar @items;", 0, "Numeric")
end)

t.test("hover: $str from join shows String", function()
  hover_has("hover: $str from join shows String",
    '$str  = join(",", @items);', 0, "String")
end)

t.test("hover: $time from time() shows Numeric", function()
  hover_has("hover: $time from time() shows Numeric",
    "$time = time();", 0, "Numeric")
end)

-- ── 14. Parameter type inference from usage ─────────────────────────

t.test("hover: $records inferred as ArrayRef from @{} usage", function()
  hover_has("hover: $records inferred as ArrayRef from @{} usage",
    "@{$records}", 2, "ArrayRef")
end)

t.test("hover: $callback inferred as CodeRef from ->() usage", function()
  hover_has("hover: $callback inferred as CodeRef from ->() usage",
    "$callback->($rec);", 0, "CodeRef")
end)

-- ── done ────────────────────────────────────────────────────────────

t.finish()
