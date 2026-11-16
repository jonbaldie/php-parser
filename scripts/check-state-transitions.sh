#!/usr/bin/env bash
set -euo pipefail
repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
build=$(mktemp -d "${TMPDIR:-/tmp}/php-state-transitions.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
cd "$repo"
cabal build -j1 lib:php-parser
cabal exec -- ghc -v0 -O0 -Wall -Werror -XGHC2021 -XOverloadedStrings -XLambdaCase -XGADTs -XTypeFamilies -XRecordWildCards \
  -isrc -outputdir "$build" -o "$build/check" test/StateTransitions.hs
"$build/check"
