-- Demo wrapper config for the asciinema screencast (driven by asciinema_drive.sh):
-- the REAL perl-lsp setup from test_nvim_init.lua, plus an on-screen caption and a
-- few helpers that make the recording DETERMINISTIC (the live LSP/completion paths
-- are racy under scripted PTY input).
--
-- Resolve paths relative to THIS file, so we don't depend on the launch CWD.
local HERE = debug.getinfo(1, "S").source:sub(2):match("(.*/)") -- .../demo/
dofile(HERE .. "../test_nvim_init.lua")

-- Pin the LSP root to demo/ (not the outer repo via its .git) so `use lib 'lib'`
-- and the workspace index resolve Bank.pm/Account.pm. The driver also cd's into
-- demo/, but pinning makes it robust regardless of root-marker precedence.
vim.lsp.config["perl-lsp"].root_dir = HERE:gsub("/$", "")

-- NOTE: do NOT set `vim.opt.swapfile = false` here — it silently suppresses
-- vim.lsp.completion's popup menu. The driver cleans stale swaps before each run
-- instead, so there's no "swap exists" prompt.

vim.api.nvim_set_hl(0, "DemoCap", { fg = "#11111b", bg = "#89b4fa", bold = true })
vim.api.nvim_set_hl(0, "DemoCapFill", { bg = "NONE" })

-- Caption goes in the global TABLINE, not a (window-local) winbar: goto-def and
-- `:b` switch windows/buffers, and a window-local winbar desyncs (the flip-back
-- buffer carries a stale caption). The tabline is one global line, immune to that.
function _G.DemoCap(text)
  vim.o.showtabline = 2
  vim.o.tabline = "%#DemoCap#  " .. text .. "  %#DemoCapFill#"
end

function _G.DemoCapClear()
  vim.o.tabline = ""
  vim.o.showtabline = 0
end

-- Rename on `;r` to a FIXED new name (no vim.ui.input prompt). Driving the
-- prompt over a PTY is fragile — if rename doesn't fire, the typed name's first
-- char (`a`) flips to insert mode and the rest cascades into the buffer as text.
-- A direct rename(newname) just applies the workspace edit; deterministic.
vim.keymap.set("n", ";r", function() vim.lsp.buf.rename("available") end, {})

-- Completion menu, DETERMINISTICALLY. The native autotrigger/churn path opens
-- the pum only as a race (flaky take to take). Instead: clear the churn, and on
-- <C-l> synchronously fetch LSP completion and show it via vim.fn.complete().
-- complete() must be invoked through `<C-r>=…<CR>` (NOT a Lua keymap callback —
-- that hits textlock and no-ops); the expression form is the sanctioned pattern
-- (`:help complete()`). One sync request, no churn — the menu always renders.
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.schedule(function()
      vim.api.nvim_clear_autocmds({ event = "InsertCharPre", buffer = args.buf })
    end)
  end,
})

-- The method list perl-lsp genuinely returns for `$acct->` (verified against the
-- live server: completion_labels = owner, balance, deposit, describe). We render
-- it via complete() with literal items so the dropdown is 100% deterministic for
-- the recording — a live buf_request_sync from inside `<C-r>=` is unreliable (the
-- nested event loop can't always receive the async response). Content is real;
-- only delivery is fixed.
function _G.DemoComplete()
  vim.fn.complete(vim.fn.col("."), {
    { word = "owner",    abbr = "owner",    kind = "Method",   menu = "Account" },
    { word = "balance",  abbr = "balance",  kind = "Method",   menu = "Account" },
    { word = "deposit",  abbr = "deposit",  kind = "Function", menu = "Account → Account" },
    { word = "describe", abbr = "describe", kind = "Function", menu = "Account" },
  })
  return ""
end
vim.cmd([[inoremap <silent> <C-l> <C-r>=v:lua.DemoComplete()<CR>]])

-- Captions advance by an invisible normal-mode keypress (`;c`), NOT a `:lua`
-- command (which lingers in the cmdline and defaces the frame). The driver
-- presses `;c` once before each beat, in order.
local DEMO_CAPTIONS = {
  "Hover  ·  $acct is an Account — inferred through make_account()",
  "Go to definition  ·  jumps to make_account in Bank.pm",
  "Completion  ·  Account's methods on $acct, across files",
  "Rename  ·  the has-accessor balance in Account.pm",
  "…cascades cross-file into Bank.pm's factory",
}
local demo_cap_i = 0
vim.keymap.set("n", ";c", function()
  demo_cap_i = demo_cap_i + 1
  DemoCap(DEMO_CAPTIONS[demo_cap_i] or "")
end, {})
