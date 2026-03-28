#!/usr/bin/env bash
# Usage: ./scripts/bump-version.sh 0.3.0
# Updates version in all the right places, commits, and tags.
set -euo pipefail

VERSION="${1:?Usage: $0 <version>}"

# Validate semver-ish format
if ! echo "$VERSION" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    echo "ERROR: version must be semver (e.g. 0.3.0), got: $VERSION" >&2
    exit 1
fi

# Ensure clean working tree
if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "ERROR: working tree is dirty — commit or stash first" >&2
    exit 1
fi

# Ensure we're on main
BRANCH=$(git branch --show-current)
if [ "$BRANCH" != "main" ]; then
    echo "ERROR: must be on main branch, currently on: $BRANCH" >&2
    exit 1
fi

echo "Bumping to v${VERSION}..."

# 1. Cargo.toml
sed -i.bak "s/^version = \".*\"/version = \"${VERSION}\"/" Cargo.toml
rm -f Cargo.toml.bak

# 2. Cargo.lock (regenerate)
cargo check --quiet 2>/dev/null

# 3. VS Code extension
sed -i.bak "s/\"version\": \".*\"/\"version\": \"${VERSION}\"/" editors/vscode/package.json
rm -f editors/vscode/package.json.bak
sed -i.bak "s/const VERSION = \".*\"/const VERSION = \"${VERSION}\"/" editors/vscode/src/extension.ts
rm -f editors/vscode/src/extension.ts.bak

# 4. Commit + tag
git add Cargo.toml Cargo.lock editors/vscode/package.json editors/vscode/src/extension.ts
git commit -m "chore: bump version to ${VERSION}"
git tag "v${VERSION}"

echo ""
echo "Done! Version bumped to ${VERSION}."
echo "  Cargo.toml:        ${VERSION}"
echo "  Cargo.lock:        updated"
echo "  VS Code extension: ${VERSION}"
echo "  Git tag:           v${VERSION}"
echo ""
echo "To release:  git push origin main && git push origin v${VERSION}"
