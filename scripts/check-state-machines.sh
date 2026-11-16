#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
logs=${1:-"$repo/state-check-results"}
mkdir -p "$logs"
cd "$repo"
failed=0
run() {
  local name=$1
  shift
  if "$@" > "$logs/$name.log" 2>&1; then
    printf 'PASS %s\n' "$name"
    tail -n 1 "$logs/$name.log"
  else
    failed=1
    printf 'FAIL %s\n' "$name"
    cat "$logs/$name.log"
  fi
}
run session-transitions bash scripts/check-state-transitions.sh
for flow in script declaration call type literal trivia; do
  case "$flow" in
    script) source_file=ScriptFlow ;;
    declaration) source_file=DeclarationFlow ;;
    call) source_file=CallFlow ;;
    type) source_file=TypeFlow ;;
    literal) source_file=LiteralFlow ;;
    trivia) source_file=TriviaFlow ;;
  esac
  run "$flow" bash .agents/skills/state-check-support/run-haskell.sh ".agents/skills/verify-$flow-flow/$source_file.hs"
done
run oracle-cache bash .agents/skills/verify-php-oracle-cache/check.sh
run import-generator bash .agents/skills/verify-import-generator/check.sh
run roundtrip-runner bash .agents/skills/verify-roundtrip-runner/check.sh
run wizard bash .agents/skills/verify-wizard-state/check.sh
exit "$failed"
