#!/usr/bin/env bash
set -euo pipefail
skill=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd -- "$skill/../../.." && pwd)
if [[ $# -ne 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 2
fi
build=$(mktemp -d "${TMPDIR:-/tmp}/php-wizard-check.XXXXXXXX")
trap 'rm -rf -- "$build"' EXIT
mkdir "$build/bin"
cp "$skill/fake-gh.sh" "$build/bin/gh"
chmod +x "$build/bin/gh"
export PATH="$build/bin:/usr/bin:/bin"
export ENV_FILE="$build/wizard.env"
export FAKE_GH_LOG="$build/gh.log"
export FAKE_AUTH=yes FAKE_SET=ok
export WIZARD_LIB="$build/library.sh"
: > "$FAKE_GH_LOG"
template="$repo/.agents/skills/wizard/template.sh"
[[ $(grep -c '^# STAGES:' "$template") -eq 1 ]] || { echo 'FAIL: wizard stage marker changed' >&2; exit 1; }
awk '/^# STAGES:/{exit} {print}' "$template" > "$WIZARD_LIB"
# Only the library is sourced: the template's browser and GitHub stages never run.
# shellcheck disable=SC1090
source "$WIZARD_LIB"
checks=0
assert_eq() {
  checks=$((checks + 1))
  if [[ "$2" != "$3" ]]; then
    printf 'FAIL %s: expected <%s>, got <%s>\n' "$1" "$3" "$2" >&2
    exit 1
  fi
}
assert_absent() {
  checks=$((checks + 1))
  if grep -Fq -- "$2" "$1"; then
    printf 'FAIL: unexpected text in %s\n' "$1" >&2
    exit 1
  fi
}
transcript="$build/transcript"
{
write_env OTHER preserved
write_env API first
write_env API second
assert_eq 'upsert keeps one key' "$(grep -c '^API=' "$ENV_FILE")" 1
assert_eq 'upsert keeps unrelated key' "$(_existing OTHER)" preserved
assert_eq 'upsert keeps latest value' "$(_existing API)" second
assert_eq 'writes recorded this run' "${WRITTEN_ENV[*]}" 'OTHER API API'
ask API 'API value' <<< ''
assert_eq 'ask keeps existing default' "$API" second
ask NEW_VALUE 'new value' <<< 'provided'
assert_eq 'ask accepts new value' "$NEW_VALUE" provided
write_env SECRET dummy-secret-42
ask_secret SECRET 'secret value' <<< ''
assert_eq 'secret prompt keeps existing default' "$SECRET" dummy-secret-42
export TOTAL_STAGES=2
stage one
stage two
assert_eq 'stage index increments' "$_STAGE_INDEX" 2
set_secret TOKEN dummy-secret-42
set_var MODE test
assert_eq 'successful secret recorded' "${WRITTEN_SECRET[*]}" TOKEN
assert_eq 'successful secret invoked fixture' "$(grep -c '^secret TOKEN$' "$FAKE_GH_LOG")" 1
assert_eq 'successful variable invoked fixture' "$(grep -c '^variable MODE$' "$FAKE_GH_LOG")" 1
FAKE_AUTH=no
set_secret MISSING not-a-real-secret
set_var MISSING_VAR test
assert_eq 'unavailable operations recorded as skipped' "${#SKIPPED[@]}" 2
assert_eq 'unavailable secret not sent' "$(grep -c '^secret MISSING$' "$FAKE_GH_LOG" || true)" 0
FAKE_AUTH=yes FAKE_SET=fail
set_secret FAILED not-a-real-secret
assert_eq 'failed secret recorded as skipped' "${#SKIPPED[@]}" 3
finish > "$build/summary"
assert_eq 'summary includes write count' "$(grep -c 'wrote 4 value(s)' "$build/summary")" 1
assert_eq 'summary includes secret count' "$(grep -c 'set 1 GitHub secret(s)' "$build/summary")" 1
assert_eq 'summary includes skipped entries' "$(grep -c 'still to do by hand' "$build/summary")" 1
assert_absent "$build/summary" dummy-secret-42
fresh=$(bash -c 'source "$WIZARD_LIB"; ask API prompt <<< "" >/dev/null; printf "%s|%s|%s" "$API" "$_STAGE_INDEX" "${#WRITTEN_ENV[@]}"')
assert_eq 'new run remembers values, not stage or run summary' "$fresh" 'second|0|0'
} >> "$transcript"
assert_absent "$transcript" dummy-secret-42
printf 'Checked %s wizard-state assertions; failures 0.\n' "$checks"
