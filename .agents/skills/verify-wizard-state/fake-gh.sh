#!/bin/bash
set -eu
case "${1:-} ${2:-}" in
  'auth status') [[ "${FAKE_AUTH:-no}" == yes ]] ;;
  'secret set')
    [[ "${FAKE_SET:-ok}" == ok ]] || exit 1
    IFS= read -r _ || true
    printf 'secret %s\n' "$3" >> "$FAKE_GH_LOG"
    ;;
  'variable set')
    [[ "${FAKE_SET:-ok}" == ok ]] || exit 1
    printf 'variable %s\n' "$3" >> "$FAKE_GH_LOG"
    ;;
  *) exit 2 ;;
esac
