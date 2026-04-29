#!/usr/bin/env bash
# Usage: ./scripts/qa/dump-packages.sh [WORKSPACE_DIR] [PICK_LIST]
# Runs `perl-lsp --dump-package` on each package in PICK_LIST and writes
# JSON dumps to WORKSPACE_DIR/dumps/<pkg with :: -> __>.json.
# Empty/missing dumps are skipped (typical for packages not installed).
#
# Defaults: WORKSPACE_DIR=qa-workspace, PICK_LIST=scripts/qa/picks-default.txt
# Combine with `shuf -n N WORKSPACE_DIR/modules.list` to add a random tail.
set -euo pipefail

WS="${1:-qa-workspace}"
LIST="${2:-scripts/qa/picks-default.txt}"
BIN="./target/release/perl-lsp"

[ -x "$BIN" ] || { echo "missing $BIN — run 'cargo build --release'"; exit 1; }
[ -f "$LIST" ] || { echo "missing pick list: $LIST"; exit 1; }
[ -f "$WS/lib/QA.pm" ] || { echo "missing $WS/lib/QA.pm — run gen-workspace.sh first"; exit 1; }

mkdir -p "$WS/dumps"
ok=0; empty=0
while IFS= read -r pkg; do
  [ -z "$pkg" ] && continue
  case "$pkg" in '#'*) continue ;; esac
  safe=$(printf '%s' "$pkg" | sed 's/::/__/g')
  out="$WS/dumps/${safe}.json"
  "$BIN" --dump-package "$WS" "$pkg" 2>/dev/null > "$out"
  if [ -s "$out" ]; then
    ok=$((ok+1))
  else
    rm -f "$out"
    empty=$((empty+1))
    echo "EMPTY: $pkg"
  fi
done < "$LIST"

echo "Dumped $ok packages to $WS/dumps/ ($empty skipped as empty)"
