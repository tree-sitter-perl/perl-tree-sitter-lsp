-- C++ member-access operator DX: completion auto-fix (Mode A) + diagnostic
-- with quick-fix (Mode B). The correct operator is driven off the receiver's
-- pointer depth (deref_stack): `Box* p` → `->`, value → `.`, `Box** pp` → no
-- auto-fix (deep `(*pp)->` is a wrap, not a swap — members shown only).
vim.opt.rtp:prepend(".")
local t = require("test.runner")
local lsp = require("test.lsp")
local b = require("test.buf")

local buf = lsp.open_and_attach("test_files/cpp/member_op.cpp")

-- The additionalTextEdit newText on the item labelled `label`, or nil.
local function op_edit(items, label)
  for _, it in ipairs(items) do
    if it.label == label then
      local edits = it.additionalTextEdits
      if edits and edits[1] then return edits[1].newText end
      return nil
    end
  end
  return nil
end

t.test("Mode A: p. on a Box* offers members + swaps `.`->`->`", function()
  local N = "ptr-dot-autofix"
  local line, col = b.find_pos(buf, "p.", b.find_line(buf, "void ptr_dot") + 1)
  if not t.ok(N, line, "no 'p.' line") then return end
  local items = lsp.completion_items(buf, line, col + 2) -- after the dot
  local labels = {}
  for _, it in ipairs(items) do labels[#labels + 1] = it.label end
  if not t.contains(N, labels, "width", "member labels") then return end
  if t.eq(N, "->", op_edit(items, "width"), "width carries `.`->`->` edit") then t.pass(N) end
end)

t.test("Mode A: v-> on a value offers members + swaps `->`->`.`", function()
  local N = "val-arrow-autofix"
  local line, col = b.find_pos(buf, "v->", b.find_line(buf, "void val_arrow") + 1)
  if not t.ok(N, line, "no 'v->' line") then return end
  local items = lsp.completion_items(buf, line, col + 3) -- after the arrow
  local labels = {}
  for _, it in ipairs(items) do labels[#labels + 1] = it.label end
  if not t.contains(N, labels, "width", "member labels") then return end
  if t.eq(N, ".", op_edit(items, "width"), "width carries `->`->`.` edit") then t.pass(N) end
end)

t.test("Mode A: pp. on a Box** shows members but NO auto-fix (deep)", function()
  local N = "deep-show-only"
  local line, col = b.find_pos(buf, "pp.", b.find_line(buf, "void deep_dot") + 1)
  if not t.ok(N, line, "no 'pp.' line") then return end
  local items = lsp.completion_items(buf, line, col + 3)
  local labels = {}
  for _, it in ipairs(items) do labels[#labels + 1] = it.label end
  if not t.contains(N, labels, "width", "member labels still offered") then return end
  if t.eq(N, nil, op_edit(items, "width"), "no operator edit for deep receiver") then t.pass(N) end
end)

-- Poll until the operator-mismatch diagnostic publishes (did_open is async).
local function op_diag(buf)
  for _ = 1, 40 do
    for _, d in ipairs(vim.diagnostic.get(buf)) do
      local lsp_d = d.user_data and d.user_data.lsp
      if lsp_d and lsp_d.code == "member-access-operator" then return d, lsp_d end
    end
    vim.wait(250)
  end
  return nil
end

t.test("Mode B: diagnostic on p.width with a working `->` quick-fix", function()
  local N = "diag-quickfix"
  local d, lsp_d = op_diag(buf)
  if not t.ok(N, d, "operator-mismatch diagnostic present") then return end
  -- It lands on the diag_case `p.width` line, at the `.` operator.
  local diag_line = b.find_line(buf, "p.width")
  if not t.eq(N, diag_line, d.lnum, "diagnostic on the p.width line") then return end

  -- Request the quick-fix over the diagnostic's range.
  local actions = lsp.request(buf, "textDocument/codeAction", {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    range = lsp_d.range,
    context = { diagnostics = { lsp_d } },
  }) or {}
  local fix
  for _, a in ipairs(actions) do
    if a.edit then fix = a; break end
  end
  if not t.ok(N, fix, "a quick-fix with an edit") then return end

  lsp.apply_workspace_edit(fix.edit)
  b.invalidate()
  local fixed = b.find_line(buf, "p%->width") -- now an arrow
  if t.ok(N, fixed, "p.width rewritten to p->width") then t.pass(N) end
end)

t.finish()
