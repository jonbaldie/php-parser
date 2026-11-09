#!/bin/bash
set -eu
if [[ "${2:-}" == "-r" ]]; then
  printf '8.4'
  exit 0
fi
if [[ "$*" != '-n -d display_errors=1 -l' ]]; then
  printf 'unexpected arguments: %s\n' "$*" >&2
  exit 2
fi
IFS= read -r source || true
printf '%s\n' "$source" >> "$FAKE_ORACLE_LOG"
if [[ "$source" == 'bad' ]]; then
  printf 'Parse error: bad input in Standard input code on line 2\n'
  exit 1
fi
exit 0
