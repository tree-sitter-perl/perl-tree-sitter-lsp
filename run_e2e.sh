#!/usr/bin/env bash
set -euo pipefail

export PERL5LIB="${PERL5LIB:-$PWD/test_files/lib}"

failed=0
for test in test_e2e.lua test_e2e_types.lua test_e2e_cross_file.lua test_e2e_inheritance.lua test_e2e_frameworks.lua; do
  echo "── $test ──"
  if ! nvim --headless --clean -u test_nvim_init.lua -l "$test"; then
    failed=1
  fi
  echo
done

exit $failed
