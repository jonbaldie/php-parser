#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
logs=${1:-"$repo/fold-query-history-results"}
mkdir -p "$logs"
logs=$(cd -- "$logs" && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/php-fold-query-history.XXXXXXXX")
trap 'rm -rf -- "$work"' EXIT

check() {
  local source=$1
  local build=$2
  shift 2
  (
    cd "$source"
    cabal build -j1 lib:php-parser --ghc-options="-Wall -Werror"
    cabal exec -- ghc -O0 -Wall -Werror -package=php-parser -i"$repo/test" \
      -outputdir "$build" -o "$build/check" "$repo/test/FoldQueryHistory.hs"
    "$build/check" "$@"
  )
}

mkdir -p "$work/current"
check "$repo" "$work/current" > "$logs/current.log" 2>&1
printf 'PASS current shared-query cases\n'

for entry in '6da6638^|Issue #109' 'e80030d^|Issue #122'; do
  revision=${entry%%|*}
  label=${entry#*|}
  commit=$(git -C "$repo" rev-parse "$revision")
  snapshot="$work/$commit"
  mkdir -p "$snapshot/source" "$snapshot/build"
  git -C "$repo" archive "$commit" | tar -x -C "$snapshot/source"
  log="$logs/$commit.log"
  if check "$snapshot/source" "$snapshot/build" "$label" > "$log" 2>&1; then
    printf 'FAIL historical omission was not detected at %s (%s)\n' "$commit" "$label" >&2
    exit 1
  fi
  if ! grep '^FAIL ' "$log" | grep -F "$label"; then
    cat "$log" >&2
    printf 'FAIL historical check did not reach its behavioral assertion\n' >&2
    exit 1
  fi
  printf 'PASS detected real historical omission at %s (%s)\n' "$commit" "$label"
done
