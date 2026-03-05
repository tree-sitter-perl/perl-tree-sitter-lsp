-- E2E tests for perl-lsp via headless nvim
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e.lua
--
-- Reuses test_nvim_init.lua for LSP config. Exits 0 on pass, 1 on failure.
-- No external deps — just nvim + the built LSP binary.

-- ── mini test framework ─────────────────────────────────────────────

local green = "\27[32m"
local red = "\27[31m"
local dim = "\27[2m"
local reset = "\27[0m"

local results = { passed = 0, failed = 0, errors = {} }

local function pass(name)
  results.passed = results.passed + 1
  io.write(green .. "  ✓ " .. reset .. name .. "\n")
end

local function fail(name, msg)
  results.failed = results.failed + 1
  table.insert(results.errors, { name = name, msg = msg })
  io.write(red .. "  ✗ " .. reset .. name .. "\n")
  io.write(dim .. "    " .. msg .. reset .. "\n")
end

--- Run a named test. Catches errors so one failure doesn't abort the suite.
local function test(name, fn)
  local ok, err = pcall(fn)
  if not ok then
    fail(name, "error: " .. tostring(err))
  end
end

--- Assertion helpers (call fail() and return false, or return true).
local function assert_eq(name, expected, actual, label)
  label = label or ""
  if expected ~= actual then
    fail(name, string.format("%s: expected %s, got %s", label, tostring(expected), tostring(actual)))
    return false
  end
  return true
end

local function assert_contains(name, list, value, label)
  label = label or "list"
  for _, v in ipairs(list) do
    if v == value then return true end
  end
  fail(name, string.format("%s missing '%s', got: [%s]", label, value, table.concat(list, ", ")))
  return false
end

local function assert_truthy(name, value, label)
  if not value then
    fail(name, label or "expected truthy value")
    return false
  end
  return true
end

-- ── file helpers (find positions by content, not line numbers) ───────

local lines_cache = nil

local function get_lines(buf)
  if not lines_cache then
    lines_cache = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  end
  return lines_cache
end

--- Find 0-indexed line number containing `pattern` (lua pattern).
--- Optional `start` (1-indexed into lines array) to skip earlier matches.
local function find_line(buf, pattern, start)
  local ll = get_lines(buf)
  for i = (start or 1), #ll do
    if ll[i]:find(pattern, 1, false) then
      return i - 1  -- 0-indexed
    end
  end
  return nil
end

--- Find 0-indexed (line, col) of `needle` (plain string) in the file.
local function find_pos(buf, needle, start)
  local ll = get_lines(buf)
  for i = (start or 1), #ll do
    local col = ll[i]:find(needle, 1, true)
    if col then
      return i - 1, col - 1  -- 0-indexed
    end
  end
  return nil, nil
end

-- ── LSP helpers ──────────────────────────────────────────────────────

local timeout_ms = 10000

local function lsp_request(buf, method, params)
  local results = vim.lsp.buf_request_sync(buf, method, params, timeout_ms)
  if not results then return nil end
  for _, res in pairs(results) do
    if res.result then return res.result end
  end
  return nil
end

local function make_params(buf, line, col)
  return {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    position = { line = line, character = col },
  }
end

local function get_def_line(buf, line, col)
  local result = lsp_request(buf, "textDocument/definition", make_params(buf, line, col))
  if not result then return nil end
  local loc = vim.islist(result) and result[1] or result
  if loc and loc.range then return loc.range.start.line end
  return nil
end

local function get_completion_labels(buf, line, col)
  local result = lsp_request(buf, "textDocument/completion", make_params(buf, line, col))
  if not result then return {} end
  local items = result.items or result
  local labels = {}
  for _, item in ipairs(items) do
    table.insert(labels, item.label)
  end
  return labels
end

local function get_hover_text(buf, line, col)
  local result = lsp_request(buf, "textDocument/hover", make_params(buf, line, col))
  if not result or not result.contents then return nil end
  return result.contents.value or result.contents
end

local function get_reference_lines(buf, line, col)
  local params = make_params(buf, line, col)
  params.context = { includeDeclaration = true }
  local result = lsp_request(buf, "textDocument/references", params)
  if not result then return {} end
  local ll = {}
  for _, ref in ipairs(result) do
    table.insert(ll, ref.range.start.line)
  end
  table.sort(ll)
  return ll
end

-- ── setup: open file, wait for LSP ──────────────────────────────────

local test_file = vim.fn.fnamemodify("test_files/sample.pl", ":p")
io.write("file: " .. test_file .. "\n")
vim.cmd("edit " .. vim.fn.fnameescape(test_file))
local buf = vim.api.nvim_get_current_buf()

local attached = false
for _ = 1, 150 do
  local clients = vim.lsp.get_clients({ bufnr = buf })
  if #clients > 0 then
    attached = true
    io.write("lsp:  " .. clients[1].name .. "\n\n")
    break
  end
  vim.wait(100)
end

if not attached then
  io.write(red .. "ERROR: LSP did not attach within 15s" .. reset .. "\n")
  vim.cmd("cquit! 1")
  return
end

vim.wait(500)  -- let server finish initial parse

-- ── tests ────────────────────────────────────────────────────────────

test("documentSymbol returns packages and classes", function()
  local name = "documentSymbol returns packages and classes"
  local result = lsp_request(buf, "textDocument/documentSymbol", {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
  })
  if not assert_truthy(name, result and #result > 0, "no symbols returned") then return end
  local names = {}
  for _, sym in ipairs(result) do table.insert(names, sym.name) end
  local ok = assert_contains(name, names, "Calculator", "symbols")
  ok = assert_contains(name, names, "Point", "symbols") and ok
  if ok then pass(name) end
end)

test("goto-def: $calc->add jumps to sub add", function()
  local name = "goto-def: $calc->add jumps to sub add"
  local line, col = find_pos(buf, "->add(2, 3)")
  if not assert_truthy(name, line, "couldn't find '->add(2, 3)' in file") then return end
  col = col + 2  -- on "add"
  local def_line = get_def_line(buf, line, col)
  local expected = find_line(buf, "^sub add ")
  if assert_eq(name, expected, def_line, "definition line") then pass(name) end
end)

test("goto-def: $p->magnitude jumps to method", function()
  local name = "goto-def: $p->magnitude jumps to method"
  local line, col = find_pos(buf, "$p->magnitude()")
  if not assert_truthy(name, line, "couldn't find '$p->magnitude()' in file") then return end
  col = col + 4  -- on "magnitude"
  local def_line = get_def_line(buf, line, col)
  local expected = find_line(buf, "method magnitude")
  if assert_eq(name, expected, def_line, "definition line") then pass(name) end
end)

test("goto-def: $p->x jumps to field reader (via inserted line)", function()
  local name = "goto-def: $p->x jumps to field reader"
  -- Insert a line at end of buffer, let LSP re-parse, then test goto-def.
  local line_count = vim.api.nvim_buf_line_count(buf)
  vim.api.nvim_buf_set_lines(buf, line_count, line_count, false, { "$p->x;" })
  lines_cache = nil  -- bust cache
  vim.wait(500)  -- let LSP re-parse

  local line, col = find_pos(buf, "$p->x;")
  if not assert_truthy(name, line, "couldn't find inserted '$p->x;'") then return end
  col = col + 4  -- on "x"
  local def_line = get_def_line(buf, line, col)
  local expected = find_line(buf, "field $x :param :reader")
  if not assert_truthy(name, def_line, "no definition result") then return end
  local ok = assert_eq(name, expected, def_line, "definition line")

  -- Clean up: remove the inserted line
  vim.api.nvim_buf_set_lines(buf, line_count, line_count + 1, false, {})
  lines_cache = nil
  vim.wait(200)

  if ok then pass(name) end
end)

test("completion: $calc-> returns Calculator methods", function()
  local name = "completion: $calc-> returns Calculator methods"
  local line, col = find_pos(buf, "$calc->add(2, 3)")
  if not assert_truthy(name, line, "couldn't find '$calc->add' in file") then return end
  -- Position right after ->
  col = col + 7  -- after "$calc->"
  local labels = get_completion_labels(buf, line, col)
  local ok = assert_contains(name, labels, "add", "completions")
  ok = assert_contains(name, labels, "subtract", "completions") and ok
  ok = assert_contains(name, labels, "get_history", "completions") and ok
  if ok then pass(name) end
end)

test("completion: $p-> returns Point methods", function()
  local name = "completion: $p-> returns Point methods"
  local line, col = find_pos(buf, "$p->magnitude()")
  if not assert_truthy(name, line, "couldn't find '$p->magnitude()' in file") then return end
  col = col + 4  -- after "$p->"
  local labels = get_completion_labels(buf, line, col)
  local ok = assert_contains(name, labels, "magnitude", "completions")
  ok = assert_contains(name, labels, "to_string", "completions") and ok
  ok = assert_contains(name, labels, "x", "completions") and ok
  ok = assert_contains(name, labels, "new", "completions") and ok
  if ok then pass(name) end
end)

test("hover: sub add shows signature", function()
  local name = "hover: sub add shows signature"
  local line = find_line(buf, "^sub add ")
  if not assert_truthy(name, line, "couldn't find 'sub add' in file") then return end
  local text = get_hover_text(buf, line, 5)
  if not assert_truthy(name, text, "no hover result") then return end
  if assert_truthy(name, type(text) == "string" and text:find("add"), "hover doesn't mention 'add'") then
    pass(name)
  end
end)

test("references: $pi finds declaration and usage", function()
  local name = "references: $pi finds declaration and usage"
  local decl_line = find_line(buf, "my $pi = ")
  if not assert_truthy(name, decl_line, "couldn't find '$pi' declaration") then return end
  local ref_lines = get_reference_lines(buf, decl_line, 4)
  if not assert_truthy(name, #ref_lines >= 2, string.format("expected >=2 refs, got %d", #ref_lines)) then return end
  -- Should include the usage in circumference sub
  local usage_line = find_line(buf, "%$pi %* %$radius")
  if not assert_truthy(name, usage_line, "couldn't find $pi usage line") then return end
  if assert_contains(name, ref_lines, usage_line, "ref lines") then
    pass(name)
  end
end)


-- ── summary ──────────────────────────────────────────────────────────

io.write(string.format(
  "\n%s%d passed%s, %s%d failed%s\n",
  green, results.passed, reset,
  results.failed > 0 and red or dim, results.failed, reset
))

if results.failed > 0 then
  vim.cmd("cquit! 1")
else
  vim.cmd("quit!")
end
