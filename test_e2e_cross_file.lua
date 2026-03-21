-- E2E tests for cross-file return type propagation.
-- Requires PERL5LIB to include test_files/lib so the LSP can resolve TestExporter.
--
-- Usage:
--   cargo build --release
--   PERL5LIB=$PWD/test_files/lib nvim --headless --clean -u test_nvim_init.lua -l test_e2e_cross_file.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/cross_file_types.pl")

-- The module resolver runs in the background. We need to wait for
-- TestExporter to be resolved before the imported return types propagate.
-- Poll by checking hover on $cfg — once it shows HashRef, types are ready.
local function wait_for_cross_file_types(max_secs)
  for _ = 1, max_secs * 4 do
    local line, col = b.find_pos(buf, "$cfg = get_config()")
    if line then
      local text = lsp.hover_text(buf, line, col)
      if text and text:find("HashRef", 1, true) then
        return true
      end
    end
    vim.wait(250)
  end
  return false
end

local ready = wait_for_cross_file_types(15)
if not ready then
  io.write("\27[33mWARN: cross-file types did not propagate within 15s — tests may fail\27[0m\n")
  io.write("      Make sure PERL5LIB includes test_files/lib\n\n")
end

-- ── helpers ──────────────────────────────────────────────────────────

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

local function hint_has(name, search, type_str)
  local line = b.find_line(buf, search)
  if not t.ok(name, line, "couldn't find line matching '" .. search .. "'") then return end
  local hints = lsp.inlay_hints(buf, line, line)
  if not t.ok(name, #hints > 0, "no inlay hints") then return end
  local found = false
  for _, h in ipairs(hints) do
    local label = type(h.label) == "string" and h.label or (h.label[1] and h.label[1].value or "")
    if label:find(type_str, 1, true) then found = true; break end
  end
  if t.ok(name, found, "no hint mentioning '" .. type_str .. "'") then t.pass(name) end
end

-- ── 1. Hover shows imported return types on variables ────────────────

t.test("hover: $cfg shows HashRef from imported get_config", function()
  hover_has("hover: $cfg shows HashRef from imported get_config",
    "$cfg = get_config()", 0, "HashRef")
end)

t.test("hover: $items shows ArrayRef from imported make_items", function()
  hover_has("hover: $items shows ArrayRef from imported make_items",
    "$items = make_items()", 0, "ArrayRef")
end)

-- ── 2. Inlay hints show imported return types ────────────────────────

t.test("inlay hint: $cfg shows HashRef", function()
  hint_has("inlay hint: $cfg shows HashRef", "my %$cfg = get_config", "HashRef")
end)

t.test("inlay hint: $items shows ArrayRef", function()
  hint_has("inlay hint: $items shows ArrayRef", "my %$items = make_items", "ArrayRef")
end)

-- ── 3. Hash key completion from imported module ──────────────────────

t.test("completion: $cfg->{} offers hash keys from get_config", function()
  local name = "completion: $cfg->{} offers hash keys from get_config"
  local line, col = b.find_pos(buf, "$cfg->{};")
  if not t.ok(name, line, "couldn't find '$cfg->{};'") then return end
  -- Position cursor inside the braces: after the '{'
  local labels = lsp.completion_labels(buf, line, col + 6)
  if not t.ok(name, #labels > 0, "no completion items") then return end
  local found_host = false
  local found_port = false
  local found_name = false
  for _, l in ipairs(labels) do
    if l == "host" then found_host = true end
    if l == "port" then found_port = true end
    if l == "name" then found_name = true end
  end
  if t.ok(name, found_host, "should offer 'host' key, got: " .. table.concat(labels, ", "))
    and t.ok(name, found_port, "should offer 'port' key")
    and t.ok(name, found_name, "should offer 'name' key") then
    t.pass(name)
  end
end)

-- ── 4. Goto-def on imported function name in use statement ───────────

t.test("goto-def: 'get_config' in use qw() jumps to TestExporter.pm", function()
  local N = "goto-def: 'get_config' in use qw() jumps to TestExporter.pm"
  local line, col = b.find_pos(buf, "use TestExporter qw(get_config")
  if not t.ok(N, line, "couldn't find use TestExporter line") then return end
  -- Position cursor on "get_config" inside qw() — col + 20 lands inside the word
  local loc = lsp.def_location(buf, line, col + 20)
  if not t.ok(N, loc, "no definition result") then return end
  if t.ok(N, loc.uri and loc.uri:find("TestExporter.pm", 1, true),
    "uri should point to TestExporter.pm, got: " .. tostring(loc.uri)) then
    t.pass(N)
  end
end)

-- ── diagnostics ──────────────────────────────────────────────────────

lsp.assert_no_diagnostics(t, buf)

-- ── done ─────────────────────────────────────────────────────────────

t.finish()
