-- Throwaway nvim config for testing perl-lsp
-- Usage: nvim -u test_nvim_init.lua test_files/sample.pl

-- Minimal settings
vim.opt.number = true
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 300

-- Path to the built binary
local lsp_bin = vim.fn.fnamemodify("target/release/perl-lsp", ":p")

-- Set up perl-lsp via vim.lsp.config (nvim 0.11+)
vim.lsp.config["perl-lsp"] = {
  cmd = { lsp_bin },
  filetypes = { "perl" },
  root_markers = { ".git", "Makefile", "cpanfile", "Makefile.PL", "Build.PL" },
}
vim.lsp.enable("perl-lsp")

-- Keybindings (set up on LspAttach)
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local buf = args.buf
    local opts = { buffer = buf }

    -- Navigation
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)

    -- Rename
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

    -- Symbol outline
    vim.keymap.set("n", "<leader>o", vim.lsp.buf.document_symbol, opts)

    -- Diagnostics (for when we add them)
    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

    -- Completion: use omnifunc backed by LSP
    vim.bo[buf].omnifunc = "v:lua.vim.lsp.omnifunc"

    -- Auto-trigger completion on Perl sigils and ->
    for _, char in ipairs({ "$", "@", "%" }) do
      vim.keymap.set("i", char, char .. "<C-x><C-o>", { buffer = buf })
    end
    vim.keymap.set("i", "->", "-><C-x><C-o>", { buffer = buf })

    print("perl-lsp attached! gd=def gr=refs K=hover <leader>rn=rename <leader>o=symbols C-x C-o=complete")
  end,
})

-- Completion menu settings
vim.opt.completeopt = { "menuone", "noinsert", "preview" }
vim.opt.pumheight = 15
