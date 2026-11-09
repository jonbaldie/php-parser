#!/usr/bin/env bash
set -euo pipefail
skill=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$skill/../../.." && pwd)
if [[ $# -ne 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 2
fi
build=$(mktemp -d "${TMPDIR:-/tmp}/php-oracle-check.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
mkdir "$build/empty-path"
: > "$build/lint.log"
export PHP84_BIN="$skill/fake-php.sh"
export FAKE_ORACLE_PATH="$build/empty-path"
export FAKE_ORACLE_LOG="$build/lint.log"
cd "$repo"
cabal exec -- ghc -v0 -O0 -threaded -Wall -Werror -XGHC2021 -XLambdaCase \
  -i"$repo/test" -i"$repo/.agents/skills/state-check-support" \
  -outputdir "$build" -o "$build/check" "$skill/CacheFlow.hs"
"$build/check" +RTS -N2 -RTS
