#!/usr/bin/env bash
set -euo pipefail

export PERL5LIB="${PERL5LIB:-$PWD/test_files/lib}"

suites=(test_e2e.lua test_e2e_types.lua test_e2e_cross_file.lua test_e2e_inheritance.lua test_e2e_frameworks.lua)

total_passed=0
total_failed=0
failed_suites=()

for test in "${suites[@]}"; do
  echo "── $test ──"
  # Capture so we can sum per-suite tallies; tee back to stdout
  # so per-test ✓/✗ output stays visible.
  if output=$(nvim --headless --clean -u test_nvim_init.lua -l "$test" 2>&1); then
    rc=0
  else
    rc=$?
  fi
  echo "$output"
  # Per-suite summary lines look like `N passed, M failed`
  # (with ANSI color codes). Strip ANSI and grep — there's
  # exactly one summary line per lua suite.
  summary=$(echo "$output" | sed 's/\x1b\[[0-9;]*m//g' | grep -E '^[0-9]+ passed, [0-9]+ failed' | tail -1 || true)
  if [[ -n "$summary" ]]; then
    p=$(echo "$summary" | sed -E 's/^([0-9]+) passed.*/\1/')
    f=$(echo "$summary" | sed -E 's/.* ([0-9]+) failed/\1/')
    total_passed=$((total_passed + p))
    total_failed=$((total_failed + f))
  fi
  if [[ $rc -ne 0 ]]; then
    failed_suites+=("$test")
  fi
  echo
done

echo "════════════════════════════════════════════"
if [[ ${#failed_suites[@]} -eq 0 ]] && [[ $total_failed -eq 0 ]]; then
  printf '\033[32mTOTAL: %d passed, 0 failed\033[0m across %d suites\n' \
    "$total_passed" "${#suites[@]}"
  exit 0
else
  printf '\033[31mTOTAL: %d passed, %d failed\033[0m across %d suites\n' \
    "$total_passed" "$total_failed" "${#suites[@]}"
  if [[ ${#failed_suites[@]} -gt 0 ]]; then
    printf '\033[31mFailing suites:\033[0m %s\n' "${failed_suites[*]}"
  fi
  exit 1
fi
