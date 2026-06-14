-- E2E: branded edges — two named Mojo::Lite apps in one workspace must
-- not leak helpers across each other. Both apps' helpers bridge to the
-- same app-surface class; per-file branding keeps them separate.
-- See docs/adr/branded-edges.md.
--
-- Usage: nvim --headless --clean -u test_nvim_init.lua -l test_e2e_branded_apps.lua

vim.opt.rtp:prepend(".")

local t   = require("test.runner")
local lsp = require("test.lsp")
local b   = require("test.buf")

local function has(labels, name)
  for _, l in ipairs(labels) do if l == name then return true end end
  return false
end

-- Completion on `$c->` (the controller, an app-surface consumer) lists
-- helpers reachable through the app-surface bridge.
local function helpers_on(buf)
  local line, col = b.find_pos(buf, "$c->render")
  if not line then return {} end
  return lsp.completion_labels(buf, line, col + 4) -- after "$c->"
end

-- Poll until the cross-file workspace index settles: `buf`'s OWN helper
-- `own` must surface on `$c->`.
local function wait_ready(buf, own)
  for _ = 1, 60 do
    if has(helpers_on(buf), own) then return true end
    vim.wait(250)
  end
  io.write("\27[33mWARN: helper completion not ready within 15s for " .. own .. "\27[0m\n")
  return false
end

local two = lsp.open_and_attach("test_files/branded_app_two.pl")
wait_ready(two, "beta_only")

t.test("branded edges: app two sees its own helper (branding doesn't hide it)", function()
  local N = "branded edges: app two sees its own helper (branding doesn't hide it)"
  if t.contains(N, helpers_on(two), "beta_only", "own helper") then t.pass(N) end
end)

t.test("branded edges: app one's helper does NOT leak into app two", function()
  local N = "branded edges: app one's helper does NOT leak into app two"
  local labels = helpers_on(two)
  -- `render` proves real method completion fired (not an empty/degenerate
  -- result). app one is a named .pl in test_files, so it IS workspace-
  -- indexed — were app two NOT branded, alpha_only would surface (cf. the
  -- unbranded integration test). Its absence is the brand filter.
  local ok = t.contains(N, labels, "render", "controller methods")
  ok = t.ok(N, not has(labels, "alpha_only"),
    "alpha_only must NOT leak into app two; got: [" .. table.concat(labels, ", ") .. "]") and ok
  if ok then t.pass(N) end
end)

t.finish()
