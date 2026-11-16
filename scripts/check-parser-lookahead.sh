#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo"
python3 scripts/check-parser-lookahead.py
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/src/Language/PHP"
cp -R src/Language/PHP/Parser "$scratch/src/Language/PHP/Parser"
for case_name in property-hook omitted-slot; do
  case "$case_name" in
    property-hook) revision=55517da^; file=Statement.hs; call='M.lookAhead (symbol "}")' ;;
    omitted-slot) revision=664cf3a^; file=Internal.hs; call='M.lookAhead comma' ;;
  esac
  git show "$revision:src/Language/PHP/Parser/$file" > "$scratch/src/Language/PHP/Parser/$file"
  grep -Fq "$call" "$scratch/src/Language/PHP/Parser/$file"
  if python3 scripts/check-parser-lookahead.py "$scratch" > "$scratch/result" 2>&1; then
    printf 'FAIL: historical %s raw lookahead was accepted\n' "$case_name" >&2
    exit 1
  fi
  if ! grep -Fq 'use qualified Language.PHP.Parser.Megaparsec as M' "$scratch/result"; then
    cat "$scratch/result" >&2
    exit 1
  fi
  printf 'PASS historical %s was rejected\n' "$case_name"
  cp "src/Language/PHP/Parser/$file" "$scratch/src/Language/PHP/Parser/$file"
done
