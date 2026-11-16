#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
base=${1:-1c1586e3d46503a97a499b194a6dd7f2b359509b}
build=$(mktemp -d "${TMPDIR:-/tmp}/php-state-equivalence.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
mkdir "$build/baseline" "$build/baseline-objects" "$build/current-objects"
git -C "$repo" archive "$base" | tar -x -C "$build/baseline"
for version in baseline current; do
  source_repo="$repo"
  [[ "$version" == baseline ]] && source_repo="$build/baseline"
  (
    cd "$source_repo"
    cabal build -j1 lib:php-parser
    cabal exec -- ghc -v0 -O0 -Wall -Werror -XGHC2021 -XLambdaCase -XOverloadedStrings \
      -package=php-parser -itest -outputdir "$build/$version-objects" \
      -o "$build/$version-check" "$repo/test/StateCharacterization.hs"
    "$build/$version-check" > "$build/$version.txt"
  )
done
if ! cmp -s "$build/baseline.txt" "$build/current.txt"; then
  python3 - "$build/baseline.txt" "$build/current.txt" <<'PY'
from itertools import zip_longest
from pathlib import Path
import sys

before, after = (Path(path).read_text().splitlines() for path in sys.argv[1:])
changed = [index for index, pair in enumerate(zip_longest(before, after), 1) if pair[0] != pair[1]]
print(f"FAIL: {len(changed)} characterization records differ; first record numbers: {changed[:20]}")
PY
  exit 1
fi
tail -n 1 "$build/current.txt"
printf 'PASS: annotated ASTs, printer output, acceptance and diagnostics match %s exactly.\n' "$base"
