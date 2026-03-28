local util = require 'lspconfig.util'

return {
  default_config = {
    cmd = { 'perl-lsp' },
    filetypes = { 'perl' },
    root_dir = util.root_pattern('cpanfile', 'Makefile.PL', 'Build.PL', '.git'),
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/tree-sitter-perl/perl-tree-sitter-lsp

A Perl language server built on tree-sitter-perl. Features type inference,
cross-file module resolution, framework intelligence (Moo/Moose/Mojo::Base/DBIC),
and 20+ LSP capabilities including semantic tokens, workspace symbol search,
and cross-file rename.

Install: `cargo install perl-lsp` or download from GitHub Releases.
]],
  },
}
