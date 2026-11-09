#!/usr/bin/env bash
set -euo pipefail

skill_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$skill_dir/../../.." && pwd)
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/php-roundtrip.XXXXXXXX")
trap 'rm -rf -- "$build_dir"' EXIT

cd "$repo"
cabal build -j1 lib:php-parser
cabal exec -- ghc -O1 -outputdir "$build_dir" \
  -o "$build_dir/verify-roundtrip" "$skill_dir/harness/ExhaustiveRoundTrip.hs"
"$build_dir/verify-roundtrip" "$@"
