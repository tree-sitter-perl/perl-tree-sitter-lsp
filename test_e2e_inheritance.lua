-- E2E tests for inheritance chain resolution.
-- Tests local multi-level inheritance AND cross-file parent resolution.
--
-- Usage:
--   cargo build --release
--   PERL5LIB=$PWD/test_files/lib nvim --headless --clean -u test_nvim_init.lua -l test_e2e_inheritance.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/inheritance.pl")

-- Wait for cross-file types (BaseWorker) to resolve.
-- Poll by checking completion on $worker-> for "process" from BaseWorker.
local function wait_for_cross_file(max_secs)
  for _ = 1, max_secs * 4 do
    local line, col = b.find_pos(buf, "$worker->run()")
    if line then
      local labels = lsp.completion_labels(buf, line, col + 9)
      for _, l in ipairs(labels) do
        if l == "process" then return true end
      end
    end
    vim.wait(250)
  end
  return false
end

local cross_file_ready = wait_for_cross_file(15)
if not cross_file_ready then
  io.write("\27[33mWARN: cross-file inheritance did not resolve within 15s — some tests may fail\27[0m\n")
  io.write("      Make sure PERL5LIB includes test_files/lib\n\n")
end

-- ── 1. Local: completion through inheritance ─────────────────────────

t.test("completion: $dog-> offers own method fetch", function()
  local N = "completion: $dog-> offers own method fetch"
  local line, col = b.find_pos(buf, "$dog->fetch()")
  if not t.ok(N, line, "couldn't find '$dog->fetch()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "fetch", "completions") then t.pass(N) end
end)

t.test("completion: $dog-> offers inherited speak from Animal (overridden in Dog)", function()
  local N = "completion: $dog-> offers inherited speak from Animal (overridden in Dog)"
  local line, col = b.find_pos(buf, "$dog->speak()")
  if not t.ok(N, line, "couldn't find '$dog->speak()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 6)
  if t.contains(N, labels, "speak", "completions") then t.pass(N) end
end)

t.test("completion: $golden-> offers multi-level inherited breathe from Animal", function()
  local N = "completion: $golden-> offers multi-level inherited breathe from Animal"
  local line, col = b.find_pos(buf, "$golden->breathe()")
  if not t.ok(N, line, "couldn't find '$golden->breathe()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 10)
  if not t.ok(N, #labels > 0, "no completions") then return end
  local ok = t.contains(N, labels, "breathe", "completions")
  ok = t.contains(N, labels, "be_friendly", "completions") and ok
  ok = t.contains(N, labels, "speak", "completions") and ok
  if ok then t.pass(N) end
end)

-- ── 2. Local: goto-def to inherited method ───────────────────────────

t.test("goto-def: $golden->breathe() jumps to Animal::breathe", function()
  local N = "goto-def: $golden->breathe() jumps to Animal::breathe"
  local line, col = b.find_pos(buf, "$golden->breathe()")
  if not t.ok(N, line, "couldn't find '$golden->breathe()'") then return end
  local def = lsp.def_line(buf, line, col + 10)
  local expected = b.find_line(buf, "^sub breathe")
  if t.eq(N, expected, def, "definition line") then t.pass(N) end
end)

t.test("goto-def: $dog->speak() jumps to Dog::speak (override, not Animal)", function()
  local N = "goto-def: $dog->speak() jumps to Dog::speak (override, not Animal)"
  local line, col = b.find_pos(buf, "$dog->speak()")
  if not t.ok(N, line, "couldn't find '$dog->speak()'") then return end
  local def = lsp.def_line(buf, line, col + 6)
  -- Dog::speak is the second "sub speak", Animal::speak is the first
  local animal_speak = b.find_line(buf, "^sub speak")
  local dog_speak = b.find_line(buf, "^sub speak", (animal_speak or 0) + 2)
  if not t.ok(N, dog_speak, "couldn't find Dog::speak line") then return end
  if t.eq(N, dog_speak, def, "definition line (should be Dog, not Animal)") then t.pass(N) end
end)

-- ── 3. Local: hover shows provenance ─────────────────────────────────

t.test("hover: $golden->breathe() shows '(from Animal)'", function()
  local N = "hover: $golden->breathe() shows '(from Animal)'"
  local line, col = b.find_pos(buf, "$golden->breathe()")
  if not t.ok(N, line, "couldn't find '$golden->breathe()'") then return end
  local text = lsp.hover_text(buf, line, col + 10)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, text:find("Animal", 1, true), "hover should mention 'Animal', got: " .. text) then
    t.pass(N)
  end
end)

t.test("hover: $dog->speak() does NOT show '(from Animal)' since Dog overrides", function()
  local N = "hover: $dog->speak() does NOT show '(from Animal)' since Dog overrides"
  local line, col = b.find_pos(buf, "$dog->speak()")
  if not t.ok(N, line, "couldn't find '$dog->speak()'") then return end
  local text = lsp.hover_text(buf, line, col + 6)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, not text:find("from Animal", 1, true),
    "hover should NOT mention 'from Animal' since Dog overrides, got: " .. text) then
    t.pass(N)
  end
end)

-- ── 4. Cross-file: completion from inherited BaseWorker ──────────────

t.test("completion: $worker-> offers process from BaseWorker", function()
  local N = "completion: $worker-> offers process from BaseWorker"
  local line, col = b.find_pos(buf, "$worker->process()")
  if not t.ok(N, line, "couldn't find '$worker->process()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 10)
  if not t.ok(N, #labels > 0, "no completions") then return end
  local ok = t.contains(N, labels, "process", "completions")
  ok = t.contains(N, labels, "run", "completions") and ok
  if ok then t.pass(N) end
end)

t.test("completion: $worker-> offers status from BaseWorker", function()
  local N = "completion: $worker-> offers status from BaseWorker"
  local line, col = b.find_pos(buf, "$worker->process()")
  if not t.ok(N, line, "couldn't find '$worker->process()'") then return end
  local labels = lsp.completion_labels(buf, line, col + 10)
  if t.contains(N, labels, "status", "completions") then t.pass(N) end
end)

-- ── 5. Cross-file: goto-def to BaseWorker::process ───────────────────

t.test("goto-def: $worker->process() jumps to BaseWorker.pm", function()
  local N = "goto-def: $worker->process() jumps to BaseWorker.pm"
  local line, col = b.find_pos(buf, "$worker->process()")
  if not t.ok(N, line, "couldn't find '$worker->process()'") then return end
  local loc = lsp.def_location(buf, line, col + 10)
  if not t.ok(N, loc, "no definition result") then return end
  if not t.ok(N, loc.uri and loc.uri:find("BaseWorker.pm", 1, true),
    "uri should point to BaseWorker.pm, got: " .. tostring(loc.uri)) then return end
  -- process is defined around line 12 in BaseWorker.pm (0-indexed)
  if t.ok(N, loc.line and loc.line >= 10 and loc.line <= 15,
    "def_line should be ~12, got: " .. tostring(loc.line)) then
    t.pass(N)
  end
end)

-- ── 6. Cross-file: hover shows provenance ────────────────────────────

t.test("hover: $worker->process() shows BaseWorker provenance", function()
  local N = "hover: $worker->process() shows BaseWorker provenance"
  local line, col = b.find_pos(buf, "$worker->process()")
  if not t.ok(N, line, "couldn't find '$worker->process()'") then return end
  local text = lsp.hover_text(buf, line, col + 10)
  if not t.ok(N, text, "no hover result") then return end
  if t.ok(N, text:find("BaseWorker", 1, true),
    "hover should mention 'BaseWorker', got: " .. text) then
    t.pass(N)
  end
end)

-- ── 7. Goto-def on parent class name in use parent ──────────────────

t.test("goto-def: 'BaseWorker' in use parent opens BaseWorker.pm", function()
  local N = "goto-def: 'BaseWorker' in use parent opens BaseWorker.pm"
  local line, col = b.find_pos(buf, "use parent 'BaseWorker'")
  if not t.ok(N, line, "couldn't find 'use parent BaseWorker'") then return end
  -- Position cursor on "BaseWorker" inside the string (col + 12 = inside the string content)
  local loc = lsp.def_location(buf, line, col + 12)
  if not t.ok(N, loc, "no definition result") then return end
  if t.ok(N, loc.uri and loc.uri:find("BaseWorker.pm", 1, true),
    "uri should point to BaseWorker.pm, got: " .. tostring(loc.uri)) then
    t.pass(N)
  end
end)

-- ── 8. Cross-file rename ─────────────────────────────────────────────

t.test("rename: process → execute produces edits in both files", function()
  local N = "rename: process → execute produces edits in both files"
  local line, col = b.find_pos(buf, "$worker->process()")
  if not t.ok(N, line, "couldn't find '$worker->process()'") then return end

  -- Request rename but DON'T apply — just check the edit structure
  local edit = lsp.rename(buf, line, col + 10, "execute")
  if not t.ok(N, edit, "rename returned no edit") then return end
  if not t.ok(N, edit.changes, "rename has no changes") then return end

  -- Should have edits in at least the current file
  local has_local = false
  local has_cross_file = false
  for uri, _ in pairs(edit.changes) do
    if uri:find("inheritance.pl", 1, true) then has_local = true end
    if uri:find("BaseWorker.pm", 1, true) then has_cross_file = true end
  end

  local ok = t.ok(N, has_local, "should have edits in inheritance.pl")
  ok = t.ok(N, has_cross_file, "should have edits in BaseWorker.pm (cross-file)") and ok
  if ok then t.pass(N) end
end)

-- ── diagnostics ──────────────────────────────────────────────────────

lsp.assert_no_diagnostics(t, buf)

-- ── done ─────────────────────────────────────────────────────────────

t.finish()
