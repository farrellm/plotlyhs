#!/usr/bin/env bash
# Vendor plotly.js: fetch dist/plotly.min.js and dist/plot-schema.json from the
# npm package (via jsDelivr) into vendor/plotly.js/, and record the version and
# checksums beside them.
#
# Usage: scripts/update-plotly-js.sh [VERSION]   (default: the latest on npm)
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$root/vendor/plotly.js"

version="${1:-}"
if [[ -z "$version" ]]; then
  version="$(curl -sSfL https://registry.npmjs.org/plotly.js/latest \
    | sed -n 's/.*"version":"\([^"]*\)".*/\1/p')"
fi
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: not a release version: '$version'" >&2
  exit 1
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for file in plotly.min.js plot-schema.json; do
  curl -sSfL -o "$tmp/$file" "https://cdn.jsdelivr.net/npm/plotly.js@$version/dist/$file"
done

# The bundle's banner names its version; refuse a mismatch.
if ! head -c 200 "$tmp/plotly.min.js" | grep -q "plotly.js v$version"; then
  echo "error: plotly.min.js does not identify itself as v$version" >&2
  exit 1
fi

mkdir -p "$dest"
mv "$tmp/plotly.min.js" "$tmp/plot-schema.json" "$dest/"
echo "$version" > "$dest/VERSION"
(cd "$dest" && sha256sum plotly.min.js plot-schema.json > SHA256SUMS)

echo "vendored plotly.js $version into ${dest#"$root"/}"
