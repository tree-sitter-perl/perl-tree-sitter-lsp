-- Throwaway nvim config for testing perl-lsp
-- Usage: nvim --clean -u test_nvim_init.lua test_files/sample.pl

-- Minimal settings
vim.opt.number = true
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 300
vim.opt.completeopt = { "menuone", "noselect", "popup" }
vim.opt.pumheight = 15

-- Path to the built binary
local lsp_bin = vim.fn.fnamemodify("target/release/perl-lsp", ":p")

-- Debug mode: set PERL_LSP_DEBUG=1 env var before launching nvim to enable.
--   PERL_LSP_DEBUG=1 nvim --clean -u test_nvim_init.lua test_files/sample.pl
-- Then tail the log: tail -f /tmp/perl-lsp.log
local debug_mode = vim.env.PERL_LSP_DEBUG == "1"
local log_level = debug_mode and "debug" or "warn"
local log_file = "/tmp/perl-lsp.log"

-- Set up perl-lsp via vim.lsp.config (nvim 0.11+)
local cmd
if debug_mode then
  cmd = {
    "sh", "-c",
    "RUST_LOG=perl_lsp=" .. log_level .. " exec " .. vim.fn.shellescape(lsp_bin) .. " 2>>" .. log_file,
  }
else
  cmd = { lsp_bin }
end

vim.lsp.config["perl-lsp"] = {
  cmd = cmd,
  filetypes = { "perl" },
  root_markers = { ".git", "Makefile", "cpanfile", "Makefile.PL", "Build.PL" },
}
vim.lsp.enable("perl-lsp")

-- Keybindings (set up on LspAttach)
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local buf = args.buf
    local client_id = args.data.client_id
    local opts = { buffer = buf }

    -- Built-in LSP completion (nvim 0.11+)
    -- autotrigger: fires on trigger characters from the server ($, @, %, ->, etc.)
    -- Use C-x C-o for manual trigger, C-y to accept, C-n/C-p to navigate
    vim.lsp.completion.enable(true, client_id, buf, { autotrigger = true })

    -- Inlay hints (type annotations inline)
    vim.lsp.inlay_hint.enable(true, { bufnr = buf })

    -- Navigation
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)

    -- Rename
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

    -- Symbol outline
    vim.keymap.set("n", "<leader>o", vim.lsp.buf.document_symbol, opts)

    -- Document highlight: highlight symbol under cursor
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      buffer = buf,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = buf,
      callback = vim.lsp.buf.clear_references,
    })

    -- Smart expand/shrink selection (selection range)
    -- + expands to next syntactic parent, - shrinks back
    local sel_stack = {}

    -- Clamp a (1-indexed line, 0-indexed col) to valid buffer position
    local function clamp(lnum, col)
      local last_line = vim.api.nvim_buf_line_count(buf)
      lnum = math.max(1, math.min(lnum, last_line))
      local line_text = vim.api.nvim_buf_get_lines(buf, lnum - 1, lnum, false)[1] or ""
      col = math.max(0, math.min(col, math.max(0, #line_text - 1)))
      return lnum, col
    end

    -- Flatten the linked-list selectionRange into a sorted list of ranges
    local function flatten_sr(node)
      local ranges = {}
      while node do
        local r = node.range
        table.insert(ranges, r)
        node = node.parent
      end
      return ranges
    end

    local function set_visual(r)
      local sl, sc = clamp(r.start.line + 1, r.start.character)
      local el, ec = clamp(r["end"].line + 1, math.max(0, r["end"].character - 1))
      vim.cmd("normal! \\<Esc>")
      vim.api.nvim_win_set_cursor(0, { sl, sc })
      vim.cmd("normal! v")
      vim.api.nvim_win_set_cursor(0, { el, ec })
    end

    vim.keymap.set({ "n", "v" }, "+", function()
      local sr = vim.lsp.buf_request_sync(buf, "textDocument/selectionRange", {
        textDocument = vim.lsp.util.make_text_document_params(buf),
        positions = { vim.lsp.util.make_position_params(0, "utf-16").position },
      }, 1000)
      if not sr then return end
      for _, res in pairs(sr) do
        if res.result and res.result[1] then
          local ranges = flatten_sr(res.result[1])
          -- Pick the next level up from where we are in the stack
          local idx = #sel_stack + 1
          if idx <= #ranges then
            sel_stack[idx] = ranges[idx]
            set_visual(ranges[idx])
          end
          return
        end
      end
    end, opts)

    vim.keymap.set("v", "-", function()
      if #sel_stack > 1 then
        table.remove(sel_stack)
        set_visual(sel_stack[#sel_stack])
      elseif #sel_stack == 1 then
        sel_stack = {}
        vim.cmd("normal! \\<Esc>")
      end
    end, opts)

    -- Reset stack when leaving visual mode
    vim.api.nvim_create_autocmd("ModeChanged", {
      pattern = "v:n",
      callback = function() sel_stack = {} end,
    })

    -- Signature help: auto-trigger on ( and , ; re-trigger while inside parens
    vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, opts)
    vim.api.nvim_create_autocmd("TextChangedI", {
      buffer = buf,
      callback = function()
        local col = vim.fn.col(".") - 1
        if col <= 0 then return end
        local line = vim.api.nvim_get_current_line()
        local before = line:sub(1, col)
        local char = before:sub(-1)
        -- Always trigger right after ( or ,
        if char == "(" or char == "," then
          vim.schedule(function()
            if vim.fn.mode() == "i" then vim.lsp.buf.signature_help() end
          end)
          return
        end
        -- For any other char (space, letters, etc.), re-trigger if inside parens
        local opens = select(2, before:gsub("%(", ""))
        local closes = select(2, before:gsub("%)", ""))
        if opens > closes then
          vim.schedule(function()
            if vim.fn.mode() == "i" then vim.lsp.buf.signature_help() end
          end)
        end
      end,
    })

    -- Format
    vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, opts)

    -- Diagnostics (readonly field writes, etc.)
    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

    -- Manual trigger with C-Space
    vim.keymap.set("i", "<C-Space>", function()
      vim.lsp.completion.get()
    end, opts)

    -- Auto-trigger completion on bareword typing
    -- Uses InsertCharPre (like nvim's own autotrigger) to avoid TextChangedI loops
    vim.api.nvim_create_autocmd("InsertCharPre", {
      buffer = buf,
      callback = function()
        if vim.fn.pumvisible() == 1 then return end
        local char = vim.v.char
        -- Only trigger on word characters (letters, digits, underscore)
        if not char:match("[%w_]") then return end
        -- Check that we're building a word of at least 2 chars
        local col = vim.fn.col(".") - 1  -- before the char being inserted
        if col <= 0 then return end
        local line = vim.api.nvim_get_current_line()
        local before = line:sub(1, col)
        local word = before:match("[%a_][%w_:]*$")
        -- word + the char being typed = 2+ chars
        if not word then return end
        vim.schedule(function()
          if vim.fn.mode() == "i" and vim.fn.pumvisible() == 0 then
            vim.lsp.completion.get()
          end
        end)
      end,
    })

    print("perl-lsp attached! gd=def gr=refs K=hover <leader>rn=rename <leader>o=symbols <leader>f=format")
  end,
})

-- Semantic token highlight groups for perl-lsp custom modifiers
-- These map @lsp.mod.<modifier>.perl to colors.
-- Adjust colors to taste — these are sensible defaults.
vim.api.nvim_set_hl(0, "@lsp.mod.scalar.perl", { fg = "#61afef" })  -- blue
vim.api.nvim_set_hl(0, "@lsp.mod.array.perl", { fg = "#c678dd" })  -- purple
vim.api.nvim_set_hl(0, "@lsp.mod.hash.perl", { fg = "#e5c07b" })   -- gold
vim.api.nvim_set_hl(0, "@lsp.mod.modification.perl", { fg = "#e06c75" })  -- red for writes
vim.api.nvim_set_hl(0, "@lsp.mod.declaration.perl", { bold = true })
