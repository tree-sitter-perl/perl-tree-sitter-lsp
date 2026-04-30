#!/usr/bin/env bash
# Usage: ./scripts/qa/analyze-bench.sh BENCH_LOG
# Summarizes a PERL_LSP_BENCH=1 stderr dump: top slow modules, latency
# distribution, and parent-fallback duplicate re-parses.
set -euo pipefail

LOG="${1:?usage: analyze-bench.sh BENCH_LOG}"

echo "=== top 30 slowest (microseconds) ==="
grep "^bench\b" "$LOG" \
  | awk -F'\t' '$3 ~ /^[0-9]+$/ { print $3 "\t" $2 "\t" $4 "\t" $5 }' \
  | sort -rn | head -30

echo
echo "=== oversize (>1MB skipped) ==="
grep "^bench\b" "$LOG" \
  | awk -F'\t' '$4 == "oversize" { print $5 "\t" $2 }' \
  | sort -rn

echo
echo "=== distribution buckets ==="
grep "^bench\b" "$LOG" | awk -F'\t' '
  $3 ~ /^[0-9]+$/ {
    ms=$3/1000;
    if (ms<1)        b="<1ms";
    else if (ms<5)   b="1-5ms";
    else if (ms<20)  b="5-20ms";
    else if (ms<100) b="20-100ms";
    else if (ms<500) b="100-500ms";
    else             b=">500ms";
    c[b]++;
  }
  END { for (k in c) print k": "c[k] }
' | sort

echo
echo "=== top 10 redundantly resolved (parent-fallback dup) ==="
grep "^bench\b" "$LOG" \
  | awk -F'\t' '{ print $2 }' \
  | sort | uniq -c | sort -rn | head -10

total=$(grep -c "^bench\b" "$LOG" || echo 0)
unique=$(grep "^bench\b" "$LOG" | awk -F'\t' '{ print $2 }' | sort -u | wc -l | tr -d ' ')
echo
echo "=== totals ==="
echo "bench events: $total"
echo "unique modules: $unique"
echo "redundant re-parses: $((total - unique))"

echo
echo "=== run wall-clock ==="
grep -E "^(Cache:|Modules:|real |user |sys )" "$LOG" || true
