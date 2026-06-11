-- E2E tests for role contracts: textDocument/implementation on a
-- `requires` marker fans out to every transitive composer's def.
-- Fixture: test_files/lib/Contract/{Role,Composer,SubRole,Deep}.pm —
-- Composer composes Role directly; Deep composes SubRole which
-- composes Role (the transitive hop). SubRole re-`requires` the
-- contract, which is a re-declaration, not an implementation.
--
-- Usage:
--   cargo build --release
--   nvim --headless --clean -u test_nvim_init.lua -l test_e2e_roles.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/lib/Contract/Role.pm")

--- Implementation locations as a sorted list of "basename:line" strings.
local function impl_locations(line, col)
  local result = lsp.request(buf, "textDocument/implementation", lsp.pos_params(buf, line, col))
  if not result then return nil end
  local locs = vim.islist(result) and result or { result }
  local out = {}
  for _, loc in ipairs(locs) do
    local uri = loc.uri or loc.targetUri
    local range = loc.range or loc.targetSelectionRange
    table.insert(out, vim.fs.basename(vim.uri_to_fname(uri)) .. ":" .. range.start.line)
  end
  table.sort(out)
  return out
end

-- The composer fan-out reads the workspace index; poll until the
-- background registration has both composers visible.
local function wait_for_composers(line, col, max_secs)
  for _ = 1, max_secs * 4 do
    local locs = impl_locations(line, col)
    if locs and #locs >= 2 then return locs end
    vim.wait(250)
  end
  return impl_locations(line, col)
end

t.test("implementation: requires atom fans out to transitive composers", function()
  local name = "implementation: requires atom fans out to transitive composers"
  local line, col = b.find_pos(buf, "requires 'fetch'")
  if not t.ok(name, line, "couldn't find the requires line") then return end
  -- Cursor on the 'fetch' atom itself.
  local locs = wait_for_composers(line, col + 10, 15)
  if not t.ok(name, locs, "no implementation response") then return end
  local joined = table.concat(locs, ", ")
  if not t.ok(name, #locs == 2, "expected 2 implementations, got: " .. joined) then return end
  if not t.ok(name, locs[1]:find("Composer.pm", 1, true), "missing Composer.pm in: " .. joined) then return end
  if t.ok(name, locs[2]:find("Deep.pm", 1, true), "missing Deep.pm (transitive hop) in: " .. joined) then
    t.pass(name)
  end
end)

t.test("implementation: $self->fetch in the role body fans out the same way", function()
  local name = "implementation: $self->fetch in the role body fans out the same way"
  local line, col = b.find_pos(buf, "$self->fetch")
  if not t.ok(name, line, "couldn't find the method call") then return end
  -- Cursor on the method name after the arrow.
  local locs = impl_locations(line, col + 7)
  if not t.ok(name, locs, "no implementation response") then return end
  local joined = table.concat(locs, ", ")
  if not t.ok(name, #locs == 2, "expected 2 implementations, got: " .. joined) then return end
  if not t.ok(name, locs[1]:find("Composer.pm", 1, true), "missing Composer.pm in: " .. joined) then return end
  if t.ok(name, locs[2]:find("Deep.pm", 1, true), "missing Deep.pm in: " .. joined) then
    t.pass(name)
  end
end)

-- ── composer-mismatch diagnostic ─────────────────────────────────────

t.test("diagnostic: broken composer warns role-requires-unfulfilled", function()
  local name = "diagnostic: broken composer warns role-requires-unfulfilled"
  local bbuf = lsp.open_and_attach("test_files/lib/Contract/Broken.pm")
  -- Diagnostics publish after cross-file resolution; poll for the warning.
  local found = nil
  for _ = 1, 40 do
    for _, d in ipairs(lsp.diagnostics(bbuf)) do
      if d.message:find("does not provide it", 1, true) then found = d; break end
    end
    if found then break end
    vim.wait(250)
  end
  if not t.ok(name, found, "no role-requires-unfulfilled diagnostic arrived") then return end
  if not t.ok(name, found.severity == vim.diagnostic.severity.WARN,
    "expected WARNING severity, got " .. tostring(found.severity)) then return end
  if t.ok(name, found.message:find("role Contract::Role requires 'fetch'", 1, true),
    "unexpected message: " .. found.message) then
    t.pass(name)
  end
end)

t.finish()
