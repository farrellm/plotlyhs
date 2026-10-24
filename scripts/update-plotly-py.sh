#!/usr/bin/env bash
# Vendor plotly.py's templates and colour scales: fetch the plotly wheel from
# PyPI, copy plotly/package_data/templates/*.json into
# vendor/plotly.py/templates/, extract the colour scales into
# vendor/plotly.py/colors.json, and record the version and checksums beside
# them. Run `cabal run plotlyhs-gen` afterwards.
#
# Usage: scripts/update-plotly-py.sh [VERSION]   (default: the latest on PyPI)
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$root/vendor/plotly.py"

version="${1:-}"
if [[ -z "$version" ]]; then
  version="$(curl -sSfL https://pypi.org/pypi/plotly/json \
    | python3 -I -c 'import json, sys; print(json.load(sys.stdin)["info"]["version"])')"
fi
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: not a release version: '$version'" >&2
  exit 1
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

url="$(curl -sSfL "https://pypi.org/pypi/plotly/$version/json" \
  | python3 -I -c 'import json, sys
urls = [u["url"] for u in json.load(sys.stdin)["urls"] if u["packagetype"] == "bdist_wheel"]
print(urls[0] if urls else "")')"
if [[ -z "$url" ]]; then
  echo "error: no wheel for plotly $version" >&2
  exit 1
fi
curl -sSfL -o "$tmp/plotly.whl" "$url"
mkdir "$tmp/wheel"
unzip -q "$tmp/plotly.whl" 'plotly/package_data/templates/*.json' '_plotly_utils/colors/*.py' -d "$tmp/wheel"

python3 -I "$root/scripts/extract-plotly-colors.py" "$tmp/wheel/_plotly_utils/colors" "$tmp/colors.json"

rm -rf "$dest"
mkdir -p "$dest/templates"
cp "$tmp"/wheel/plotly/package_data/templates/*.json "$dest/templates/"
mv "$tmp/colors.json" "$dest/"
echo "$version" > "$dest/VERSION"
(cd "$dest" && sha256sum colors.json templates/*.json > SHA256SUMS)

echo "vendored plotly.py $version into ${dest#"$root"/}"
