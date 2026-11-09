#!/usr/bin/env bash
set -euo pipefail
skill=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$skill/../../.." && pwd)
if [[ $# -ne 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 2
fi
cd "$repo"
cabal exec -- ghc -v0 -XGHC2021 -XLambdaCase -XOverloadedStrings \
  -e "$(<"$skill/Imports.expr")" test/Test/Gen/PHPSource.hs
