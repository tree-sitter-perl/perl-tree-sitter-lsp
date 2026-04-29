#!/usr/bin/env bash
# Usage: ./scripts/qa/analyze-dumps.sh [DUMPS_DIR]
# Categorizes type-inference findings across a directory of
# `--dump-package` JSON outputs:
#   - per-package raw/bag coverage table
#   - subs where Symbol.return_type is set but bag is null
#   - subs where Symbol.return_type and bag disagree
#   - non-framework classes where $self typed as the rep (HashRef/ArrayRef)
#   - return_type_provenance distribution (PluginOverride / ReducerFold / Delegation / null)
set -euo pipefail

DUMPS="${1:-qa-workspace/dumps}"
[ -d "$DUMPS" ] || { echo "missing dumps dir: $DUMPS"; exit 1; }

shopt -s nullglob
files=("$DUMPS"/*.json)
[ ${#files[@]} -gt 0 ] || { echo "no .json files in $DUMPS"; exit 1; }

echo "=== per-package coverage ==="
for f in "${files[@]}"; do
  jq -r '
    {
      pkg: .package,
      total: (.subs | length),
      raw: ([.subs[] | select(.raw_return_type != null)] | length),
      bag: ([.subs[] | select(.bag_return_type != null)] | length),
      raw_no_bag: ([.subs[] | select(.raw_return_type != null and .bag_return_type == null)] | length),
      raw_neq_bag: ([.subs[] | select(.raw_return_type != null and .bag_return_type != null and .raw_return_type != .bag_return_type)] | length)
    } | "\(.pkg)\t\(.total)\t\(.raw)\t\(.bag)\t\(.raw_no_bag)\t\(.raw_neq_bag)"
  ' "$f"
done | sort | awk -F'\t' '
  BEGIN { printf "%-45s %-6s %-6s %-6s %-12s %-12s\n", "package", "subs", "raw", "bag", "raw_no_bag", "raw_neq_bag" }
  { printf "%-45s %-6s %-6s %-6s %-12s %-12s\n", $1, $2, $3, $4, $5, $6 }
'

echo
echo "=== Symbol.return_type set but bag is null ==="
for f in "${files[@]}"; do
  jq -r '.package as $p | .subs[]
    | select(.raw_return_type != null and .bag_return_type == null)
    | "\($p)\t\(.name)\tparams=\(.params|length)\traw=\(.raw_return_type)"' "$f"
done | sort | uniq

echo
echo "=== Symbol.return_type and bag disagree ==="
for f in "${files[@]}"; do
  jq -r '.package as $p | .subs[]
    | select(.raw_return_type != null and .bag_return_type != null and .raw_return_type != .bag_return_type)
    | "\($p)\t\(.name)\tparams=\(.params|length)\traw=\(.raw_return_type)\tbag=\(.bag_return_type)"' "$f"
done | sort | uniq

echo
echo "=== bless-rep precedence: \$self typed as rep in non-framework classes ==="
for f in "${files[@]}"; do
  jq -r '
    .package as $p | .framework as $fw
    | if $fw == null then
        (.subs[] | select(.vars_in_scope | map(.var == "$self" and (.type == "HashRef" or .type == "ArrayRef" or .type == "Hash" or .type == "Array")) | any) | "\($p)::\(.name)")
      else empty end
  ' "$f" 2>/dev/null
done | sort -u

echo
echo "=== return_type_provenance distribution ==="
for f in "${files[@]}"; do
  jq -r '.subs[] | .return_type_provenance
    | if . == null then "null"
      elif type == "object" then (.kind // (keys[0]))
      else "other" end' "$f"
done | sort | uniq -c | sort -rn

echo
echo "=== analyzed: ${#files[@]} dumps in $DUMPS ==="
