---
name: verify-wizard-state
description: Check wizard value persistence, per-run progress and summaries, and skipped GitHub operations. Use when changing the wizard template library.
---

# Verify wizard state

Writing a key must preserve unrelated values and replace earlier values of that key. A new run should offer saved values, but stage progress and the run summary should start fresh. Successful and skipped GitHub operations must be recorded without printing secret values.

From the repository root run:

```bash
bash .agents/skills/verify-wizard-state/check.sh
```

The check extracts and sources only the real library above the `STAGES` marker in `.agents/skills/wizard/template.sh`. It never runs the template's browser or GitHub stages. A temporary `.env` and fake `gh` executable exercise writes, defaults, secret and variable success and failure, stage increments, summary output, and a fresh process. Baseline: 20 assertions, zero failures. Empty write summaries are checked explicitly, including on Bash 3.2. No real browser, GitHub operation, or user `.env` is touched. Temporary files are removed on exit. Extra arguments exit 2; failures return nonzero.

This is bounded testing, not a proof. It covers the template library, not generated wizard stages, terminal echo behavior, arbitrary `.env` syntax, or real GitHub authentication. The stage index is deliberately not persisted; the saved values, not the completed stage, are what a rerun remembers.

Review changes to `.agents/skills/wizard/template.sh` above its `STAGES` marker and `.agents/skills/verify-wizard-state/`. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-wizard-state/SKILL.md`.

After a covered change, inspect the diff and rerun. In a disposable worktree remove the `WRITTEN_ENV+=("$key")` update from `write_env`; the run-summary assertions must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` if stale.
