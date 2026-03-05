-- Buffer / position helpers for e2e tests.
-- Find positions by content rather than hardcoded line numbers.

local M = {}

local lines_cache = nil

function M.get_lines(buf)
  if not lines_cache then
    lines_cache = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  end
  return lines_cache
end

--- Bust the line cache (call after modifying the buffer).
function M.invalidate()
  lines_cache = nil
end

--- Find 0-indexed line number containing `pattern` (lua pattern).
--- Optional `start` (1-indexed into lines array) to skip earlier matches.
function M.find_line(buf, pattern, start)
  local ll = M.get_lines(buf)
  for i = (start or 1), #ll do
    if ll[i]:find(pattern, 1, false) then
      return i - 1  -- 0-indexed
    end
  end
  return nil
end

--- Find 0-indexed (line, col) of `needle` (plain string) in the file.
function M.find_pos(buf, needle, start)
  local ll = M.get_lines(buf)
  for i = (start or 1), #ll do
    local col = ll[i]:find(needle, 1, true)
    if col then
      return i - 1, col - 1  -- 0-indexed
    end
  end
  return nil, nil
end

--- Insert lines at the end of the buffer. Returns the 0-indexed line of the first inserted line.
function M.append(buf, text_lines)
  local count = vim.api.nvim_buf_line_count(buf)
  vim.api.nvim_buf_set_lines(buf, count, count, false, text_lines)
  M.invalidate()
  return count
end

--- Remove lines from `from` to `to` (0-indexed, exclusive end).
function M.remove(buf, from, to)
  vim.api.nvim_buf_set_lines(buf, from, to, false, {})
  M.invalidate()
end

return M
