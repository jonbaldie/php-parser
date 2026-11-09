#!/usr/bin/env bash
set -euo pipefail
skill=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$skill/../../.." && pwd)
if [[ $# -ne 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 2
fi
build=$(mktemp -d "${TMPDIR:-/tmp}/php-roundtrip-runner.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
cd "$repo"
cabal build -j1 lib:php-parser
source_file="$repo/.agents/skills/verify-roundtrip/harness/ExhaustiveRoundTrip.hs"
mkdir "$build/original-objects" "$build/injected-objects"
cabal exec -- ghc -v0 -O0 -outputdir "$build/original-objects" -o "$build/original" "$source_file"
python3 - "$source_file" "$build/Injected.hs" <<'PY'
from pathlib import Path
import sys

source = Path(sys.argv[1]).read_text()
replacements = [
    ('("variable", "$x")', '("variable", "(")'),
    ('''          problem = case parseExpression "printed.php" printed of
            Left err -> Just ("printed source was rejected: " ++ show (formatParseError err))
            Right reparsed
              | ast == stripAnnotations reparsed -> Nothing
              | otherwise -> Just ("AST changed\\n  expected: " ++ show ast
                                  ++ "\\n  actual:   " ++ show (stripAnnotations reparsed))''',
     '''          problem = if caseGroup testCase == "binary/+"
            then Just "injected reporting failure"
            else case parseExpression "printed.php" printed of
              Left err -> Just ("printed source was rejected: " ++ show (formatParseError err))
              Right reparsed
                | ast == stripAnnotations reparsed -> Nothing
                | otherwise -> Just ("AST changed\\n  expected: " ++ show ast
                                    ++ "\\n  actual:   " ++ show (stripAnnotations reparsed))'''),
]
for old, new in replacements:
    if source.count(old) != 1:
        raise SystemExit("injection site changed; review the runner check")
    source = source.replace(old, new, 1)
Path(sys.argv[2]).write_text(source)
PY
cabal exec -- ghc -v0 -O0 -outputdir "$build/injected-objects" -o "$build/injected" "$build/Injected.hs"

run() {
  local label=$1 expected=$2 binary=$3 status
  shift 3
  set +e
  "$binary" "$@" > "$build/$label.log" 2>&1
  status=$?
  set -e
  if [[ "$status" -ne "$expected" ]]; then
    printf 'FAIL %s: expected exit %s, got %s\n' "$label" "$expected" "$status" >&2
    tail -n 15 "$build/$label.log" >&2
    exit 1
  fi
}
contains() {
  local label=$1 text=$2
  if ! grep -Fq -- "$text" "$build/$label.log"; then
    printf 'FAIL %s: missing %s\n' "$label" "$text" >&2
    tail -n 15 "$build/$label.log" >&2
    exit 1
  fi
}
count_lines() {
  grep -c "^$2 " "$build/$1.log" || true
}

run invalid-option 2 "$build/original" --bogus
contains invalid-option 'Usage:'
run missing-group 2 "$build/original" --group
contains missing-group 'Usage:'
run duplicate-group 2 "$build/original" --group 'binary/+' --group 'binary/+'
contains duplicate-group 'Usage:'
run unknown-group 2 "$build/original" --group 'binary/++'
contains unknown-group 'COVERAGE FAILURE: the selected group has no generated cases.'
run valid-group 0 "$build/original" --group 'binary/+'
contains valid-group 'Selected group: binary/+'
contains valid-group 'round-trip failures 0.'
contains valid-group 'PASS: every accepted source preserved its expression AST.'
run one-rejection 0 "$build/original" --group 'context/prefix increment' --show-rejections
run all-rejections 0 "$build/original" --group 'context/prefix increment' --all-rejections
[[ $(count_lines one-rejection REJECTED) -eq 1 ]] || { echo 'FAIL one-rejection count' >&2; exit 1; }
rejected=$(count_lines all-rejections REJECTED)
summary=$(grep '^Accepted ' "$build/all-rejections.log")
[[ $summary =~ rejected\ ([0-9]+)\; ]] || { echo 'FAIL rejection summary' >&2; exit 1; }
[[ $rejected -gt 1 && $rejected -eq ${BASH_REMATCH[1]} ]] || { echo 'FAIL all-rejections count' >&2; exit 1; }
run injected-coverage 2 "$build/injected" --group 'seed/variable' --show-rejections
contains injected-coverage 'COVERAGE FAILURE: no accepted input for seed/variable'
contains injected-coverage 'REJECTED seed/variable'
run injected-first 1 "$build/injected" --group 'binary/+'
run injected-all 1 "$build/injected" --group 'binary/+' --all-failures
[[ $(count_lines injected-first FAIL) -eq 1 ]] || { echo 'FAIL first failure count' >&2; exit 1; }
failures=$(count_lines injected-all FAIL)
summary=$(grep '^Accepted ' "$build/injected-all.log")
[[ $summary =~ Accepted\ ([0-9]+)\;\ rejected\ ([0-9]+)\;\ round-trip\ failures\ ([0-9]+)\. ]] || { echo 'FAIL failure summary' >&2; exit 1; }
[[ $failures -gt 1 && $failures -eq ${BASH_REMATCH[1]} && $failures -eq ${BASH_REMATCH[3]} ]] || { echo 'FAIL all-failures count' >&2; exit 1; }
contains injected-all 'injected reporting failure'
printf 'Checked 10 CLI scenarios; failures 0. Injected reporting failures: %s; rejected examples: %s.\n' "$failures" "$rejected"
