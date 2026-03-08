-- LSP request helpers for e2e tests.

local M = {}

M.timeout_ms = 10000

--- Send a sync LSP request, return the first result or nil.
function M.request(buf, method, params)
  local results = vim.lsp.buf_request_sync(buf, method, params, M.timeout_ms)
  if not results then return nil end
  for _, res in pairs(results) do
    if res.result then return res.result end
  end
  return nil
end

--- Build textDocument/position params.
function M.pos_params(buf, line, col)
  return {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    position = { line = line, character = col },
  }
end

--- Get the 0-indexed line number of a definition result, or nil.
function M.def_line(buf, line, col)
  local result = M.request(buf, "textDocument/definition", M.pos_params(buf, line, col))
  if not result then return nil end
  local loc = vim.islist(result) and result[1] or result
  if loc and loc.range then return loc.range.start.line end
  return nil
end

--- Get completion labels as a plain list of strings.
function M.completion_labels(buf, line, col)
  local result = M.request(buf, "textDocument/completion", M.pos_params(buf, line, col))
  if not result then return {} end
  local items = result.items or result
  local labels = {}
  for _, item in ipairs(items) do
    table.insert(labels, item.label)
  end
  return labels
end

--- Get hover text (markdown string), or nil.
function M.hover_text(buf, line, col)
  local result = M.request(buf, "textDocument/hover", M.pos_params(buf, line, col))
  if not result or not result.contents then return nil end
  return result.contents.value or result.contents
end

--- Get reference locations as a sorted list of 0-indexed line numbers.
function M.reference_lines(buf, line, col)
  local params = M.pos_params(buf, line, col)
  params.context = { includeDeclaration = true }
  local result = M.request(buf, "textDocument/references", params)
  if not result then return {} end
  local ll = {}
  for _, ref in ipairs(result) do
    table.insert(ll, ref.range.start.line)
  end
  table.sort(ll)
  return ll
end

--- Get document symbols as a list of names.
function M.symbol_names(buf)
  local result = M.request(buf, "textDocument/documentSymbol", {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
  })
  if not result then return {} end
  local names = {}
  for _, sym in ipairs(result) do
    table.insert(names, sym.name)
  end
  return names
end

--- Get completion items (full) for a position.
function M.completion_items(buf, line, col)
  local result = M.request(buf, "textDocument/completion", M.pos_params(buf, line, col))
  if not result then return {} end
  return result.items or result
end

--- Get inlay hints for a line range (0-indexed, inclusive).
function M.inlay_hints(buf, start_line, end_line)
  local result = M.request(buf, "textDocument/inlayHint", {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    range = {
      start = { line = start_line, character = 0 },
      ["end"] = { line = end_line + 1, character = 0 },
    },
  })
  return result or {}
end

--- Get signature help at a position.
function M.signature_label(buf, line, col)
  local result = M.request(buf, "textDocument/signatureHelp", M.pos_params(buf, line, col))
  if not result or not result.signatures or #result.signatures == 0 then return nil end
  return result.signatures[1].label
end

--- Get diagnostics for the buffer.
function M.diagnostics(buf)
  return vim.diagnostic.get(buf)
end

--- Open a file, wait for LSP to attach. Returns buf or calls vim.cmd("cquit! 1").
function M.open_and_attach(path)
  local abs = vim.fn.fnamemodify(path, ":p")
  io.write("file: " .. abs .. "\n")
  vim.cmd("edit " .. vim.fn.fnameescape(abs))
  local buf = vim.api.nvim_get_current_buf()

  for _ = 1, 150 do
    local clients = vim.lsp.get_clients({ bufnr = buf })
    if #clients > 0 then
      io.write("lsp:  " .. clients[1].name .. "\n\n")
      vim.wait(500)  -- let server finish initial parse
      return buf
    end
    vim.wait(100)
  end

  io.write("\27[31mERROR: LSP did not attach within 15s\27[0m\n")
  vim.cmd("cquit! 1")
end

return M
