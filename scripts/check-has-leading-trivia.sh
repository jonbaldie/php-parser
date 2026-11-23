#!/usr/bin/env bash
# Check the HasLeadingTrivia contract from Issue #406:
#
#  * Pretty.hs has no OVERLAPPABLE (catch-all) instance.
#  * Printing a custom annotation type with no instance fails to compile
#    with "No instance for 'HasLeadingTrivia ...'".
#  * Printing a custom annotation type with an explicit instance compiles.
#
# The fixtures under test/fixtures/ are not part of the Cabal build; they
# are compiled here, on purpose, with ghc -fno-code against the built library.
set -uo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo"

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

# No universal fallback may remain.
if grep -Fq 'OVERLAPPABLE' src/Language/PHP/Pretty.hs; then
  grep -n 'OVERLAPPABLE' src/Language/PHP/Pretty.hs >&2
  fail 'Pretty.hs still contains an OVERLAPPABLE instance'
fi
printf 'PASS no OVERLAPPABLE instance in Pretty.hs\n'

# The library must be built so cabal exec can find it.
cabal build lib:php-parser >/dev/null || fail 'cabal build lib:php-parser failed'

# Negative: must fail, and say why.
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT

negative_log="$scratch/negative.log"
if cabal exec -- ghc -fno-code -package php-parser test/fixtures/HasLeadingTriviaNegative.hs >"$negative_log" 2>&1; then
  cat "$negative_log" >&2
  fail 'negative fixture compiled; a custom annotation without an instance is printable'
fi
if ! grep -Fq "No instance for" "$negative_log" || ! grep -Fq "HasLeadingTrivia" "$negative_log"; then
  cat "$negative_log" >&2
  fail 'negative fixture failed for an unrelated reason (expected No instance for HasLeadingTrivia)'
fi
grep -F "No instance for" "$negative_log" | head -n 1
printf 'PASS negative fixture rejected with No instance for HasLeadingTrivia\n'

# Positive: must compile.
positive_log="$scratch/positive.log"
if ! cabal exec -- ghc -fno-code -package php-parser test/fixtures/HasLeadingTriviaPositive.hs >"$positive_log" 2>&1; then
  cat "$positive_log" >&2
  fail 'positive fixture (explicit HasLeadingTrivia instance) did not compile'
fi
printf 'PASS positive fixture with explicit instance compiled\n'
