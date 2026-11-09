#!/usr/bin/env bash
set -euo pipefail

support=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$support/../../.." && pwd)
if [[ $# -lt 1 || ! -f "$repo/$1" ]]; then
  printf 'Usage: %s REPOSITORY_RELATIVE_HASKELL_FILE [CHECK_ARGS...]\n' "$0" >&2
  exit 2
fi
source_file=$1
shift

build=$(mktemp -d "${TMPDIR:-/tmp}/php-state-check.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
cd "$repo"
cabal build -j1 lib:php-parser
cabal exec -- ghc -O0 -Wall -Werror -package=php-parser -i"$support" \
  -outputdir "$build" -o "$build/check" "$repo/$source_file"
"$build/check" "$@"
