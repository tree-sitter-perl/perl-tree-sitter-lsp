#!/usr/bin/env bash
# Usage: ./scripts/qa/run-all.sh [WORKSPACE_DIR] [PICK_LIST]
# Full QA harness: regenerate the @INC workspace, run cold+warm bench,
# dump the curated pick list (plus 10 random tail), and analyze.
# Assumes `cargo build --release` already ran.
#
# Defaults: WORKSPACE_DIR=qa-workspace, PICK_LIST=scripts/qa/picks-default.txt
set -euo pipefail

WS="${1:-qa-workspace}"
LIST="${2:-scripts/qa/picks-default.txt}"
SELF=$(cd "$(dirname "$0")" && pwd)

"$SELF/gen-workspace.sh" "$WS"

# Add 10 random tail entries to the curated pick list for this run.
PICKS_RUN="$WS/picks-run.txt"
{
  cat "$LIST"
  echo
  echo "# random tail"
  shuf -n 10 "$WS/modules.list"
} > "$PICKS_RUN"

"$SELF/run-bench.sh" "$WS"
echo
echo "##### bench analysis (run 1, cold-ish) #####"
"$SELF/analyze-bench.sh" "$WS/bench-1.log"
echo
echo "##### bench analysis (run 2, warm) #####"
"$SELF/analyze-bench.sh" "$WS/bench-2.log"

"$SELF/dump-packages.sh" "$WS" "$PICKS_RUN"
echo
echo "##### dump analysis #####"
"$SELF/analyze-dumps.sh" "$WS/dumps"
