-- E2E: per-variable branded edges — two Minion instances in one file
-- own distinct task sets. Completion on `$a->enqueue('|')` must offer
-- $a's task, not $b's. Single buffer (no cross-file), so no index-warmup
-- timing. See docs/adr/branded-edges.md.
--
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_branded_minion.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local buf = lsp.open_and_attach("test_files/branded_minion.pl")
vim.wait(500)

local function has(labels, name)
  for _, l in ipairs(labels) do if l == name then return true end end
  return false
end

-- Cursor between the quotes of `$a->enqueue('')`.
local function task_completions()
  local line, col = b.find_pos(buf, "$a->enqueue('')")
  if not line then return {} end
  return lsp.completion_labels(buf, line, col + 13) -- inside the ''
end

t.test("branded edges: $a->enqueue offers $a's task", function()
  local N = "branded edges: $a->enqueue offers $a's task"
  if t.contains(N, task_completions(), "alpha_task", "task completions") then
    t.pass(N)
  end
end)

t.test("branded edges: $a->enqueue does NOT offer $b's task", function()
  local N = "branded edges: $a->enqueue does NOT offer $b's task"
  local labels = task_completions()
  if t.ok(N, not has(labels, "beta_task"),
      "beta_task ($b's) must NOT be offered on $a; got: [" .. table.concat(labels, ", ") .. "]") then
    t.pass(N)
  end
end)

-- Accessor-chain: cursor between the quotes of `$app->minion->enqueue('')`.
local function accessor_completions()
  local line, col = b.find_pos(buf, "$app->minion->enqueue('')")
  if not line then return {} end
  return lsp.completion_labels(buf, line, col + 23) -- inside the ''
end

t.test("branded edges: $app->minion->enqueue offers its accessor's task", function()
  local N = "branded edges: $app->minion->enqueue offers its accessor's task"
  if t.contains(N, accessor_completions(), "acc_minion_task", "accessor tasks") then
    t.pass(N)
  end
end)

t.test("branded edges: $app->minion does NOT offer $app->other_minion's task", function()
  local N = "branded edges: $app->minion does NOT offer $app->other_minion's task"
  local labels = accessor_completions()
  if t.ok(N, not has(labels, "acc_other_task"),
      "other_minion's task must NOT leak onto ->minion; got: [" .. table.concat(labels, ", ") .. "]") then
    t.pass(N)
  end
end)

t.finish()
