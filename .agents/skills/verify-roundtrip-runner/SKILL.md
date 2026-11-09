---
name: verify-roundtrip-runner
description: Check the bounded round-trip verifier's CLI, group coverage, counts and failure reporting. Use when changing verify-roundtrip or its harness.
---

# Verify the round-trip runner

The verifier must reject invalid options and empty groups, distinguish rejected source from accepted source, fail when a required group has no accepted input, and report one or all counterexamples as requested. This skill checks the verifier's own state machine; `verify-roundtrip` checks parser and printer behavior.

From the repository root run:

```bash
bash .agents/skills/verify-roundtrip-runner/check.sh
```

The check builds the current library and the actual round-trip harness, then runs ten CLI scenarios. It checks exact group selection, invalid and duplicate options, rejection counts, and example reporting. To exercise otherwise-green failure paths, it compiles a **temporary copy** of the harness with one invalid required seed and an injected failure for every `binary/+` case. It checks exit 2 for missing accepted coverage, exit 1 for failures, one default failure example, and the complete `--all-failures` count. The original harness and product files are not changed. Baseline: ten scenarios pass, with 196 deliberately injected failures and 70 real rejected examples. Python 3 is needed to make and validate the temporary injections.

This is bounded testing of the CLI and reporting behavior, not a proof of every option combination or a substitute for the full round-trip check. The injected failures exercise reporting, not a real parser defect. If an injection site changes, the script fails and asks for review rather than silently skipping it.

Covered code: `.agents/skills/verify-roundtrip/harness/ExhaustiveRoundTrip.hs`, the library it calls, and this skill. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-roundtrip-runner/SKILL.md`.

After a covered change, inspect the diff and rerun. In a disposable worktree change the `--show-rejections` option to leave `showRejections` false. The one-rejection scenario must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` when stale.
