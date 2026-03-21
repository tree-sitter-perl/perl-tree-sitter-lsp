#!/usr/bin/env bash
# Quick-launch nvim with perl-lsp for manual testing.
# Builds release, sets PERL5LIB to include test_files/lib, opens the given file.
#
# Usage:
#   ./dev.sh                          # opens test_files/frameworks.pl
#   ./dev.sh test_files/sample.pl     # opens a specific file
#   PERL_LSP_DEBUG=1 ./dev.sh         # with debug logging (tail -f /tmp/perl-lsp.log)

set -euo pipefail
cd "$(dirname "$0")"

cargo build --release 2>&1

export PERL5LIB="${PERL5LIB:-}:$PWD/test_files/lib"

file="${1:-test_files/frameworks.pl}"
exec nvim --clean -u test_nvim_init.lua "$file"
