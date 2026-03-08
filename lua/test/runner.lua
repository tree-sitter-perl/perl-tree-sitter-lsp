-- Mini test runner for headless nvim e2e tests.
-- No external dependencies.

local M = {}

local green = "\27[32m"
local red = "\27[31m"
local dim = "\27[2m"
local reset = "\27[0m"

local state = { passed = 0, failed = 0, errors = {} }

function M.pass(name)
  state.passed = state.passed + 1
  io.write(green .. "  ✓ " .. reset .. name .. "\n")
end

function M.fail(name, msg)
  state.failed = state.failed + 1
  table.insert(state.errors, { name = name, msg = msg })
  io.write(red .. "  ✗ " .. reset .. name .. "\n")
  io.write(dim .. "    " .. msg .. reset .. "\n")
end

--- Run a named test. Catches errors so one failure doesn't abort the suite.
function M.test(name, fn)
  local ok, err = pcall(fn)
  if not ok then
    M.fail(name, "error: " .. tostring(err))
  end
end

--- Print summary and exit with appropriate code.
function M.finish()
  io.write(string.format(
    "\n%s%d passed%s, %s%d failed%s\n",
    green, state.passed, reset,
    state.failed > 0 and red or dim, state.failed, reset
  ))
  if state.failed > 0 then
    vim.cmd("cquit! 1")
  else
    vim.cmd("quit!")
  end
end

-- ── assertion helpers ────────────────────────────────────────────────
-- Each returns true on success, calls fail() and returns false on failure.

function M.eq(name, expected, actual, label)
  label = label or ""
  if expected ~= actual then
    M.fail(name, string.format("%s: expected %s, got %s", label, tostring(expected), tostring(actual)))
    return false
  end
  return true
end

function M.contains(name, list, value, label)
  label = label or "list"
  for _, v in ipairs(list) do
    if v == value then return true end
  end
  M.fail(name, string.format("%s missing '%s', got: [%s]", label, value, table.concat(list, ", ")))
  return false
end

function M.ok(name, value, label)
  if not value then
    M.fail(name, label or "expected truthy value")
    return false
  end
  return true
end

return M
