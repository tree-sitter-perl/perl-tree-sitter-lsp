-- Throwaway nvim config: drive perl-lsp (built --features cpp) on C++.
vim.opt.signcolumn = "yes"
local lsp_bin = vim.env.PERL_LSP_BIN
  and vim.fn.fnamemodify(vim.env.PERL_LSP_BIN, ":p")
  or vim.fn.fnamemodify("target/release/perl-lsp", ":p")
vim.lsp.config["perl-lsp"] = {
  cmd = { lsp_bin },
  filetypes = { "cpp" },
  root_markers = { ".git", "CMakeLists.txt", "Makefile" },
}
vim.lsp.enable("perl-lsp")
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function() print("cpp-lsp attached!") end,
})
