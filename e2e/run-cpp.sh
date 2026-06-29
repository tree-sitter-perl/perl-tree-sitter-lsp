#!/usr/bin/env bash
# C++ e2e: builds a --features all-langs release and drives cpp-lsp in headless
# nvim (reuses the same lua harness as the Perl suites). Separate from
# run.sh because it needs the cpp-feature binary.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
echo "building --features all-langs release..."
cargo build --release --features all-langs >/dev/null 2>&1
PERL_LSP_BIN="$PWD/target/release/perl-lsp" \
  nvim --headless --clean -u e2e/init_cpp.lua -l e2e/cpp.lua
  PERL_LSP_BIN="$PWD/target/release/perl-lsp" \
  nvim --headless --clean -u e2e/init_cpp.lua -l e2e/cpp_members.lua
  nvim --headless --clean -u e2e/init_cpp.lua -l e2e/cpp_locals.lua
  nvim --headless --clean -u e2e/init_cpp.lua -l e2e/cpp_macro_calls.lua

PERL_LSP_BIN="$PWD/target/release/perl-lsp" \
  nvim --headless --clean -u e2e/init_python.lua -l e2e/python_members.lua
