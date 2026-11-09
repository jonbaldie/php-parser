---
name: verify-declaration-flow
description: Check accumulated class, method, property and constant modifiers and duplicate property hooks. Use when changing declaration parsing.
---

# Verify declaration flow

Repeated or conflicting modifiers and repeated `get` or `set` hooks must be rejected regardless of order. Valid combinations in the selected grammar must remain accepted.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-declaration-flow/DeclarationFlow.hs
```

The check enumerates sequences of zero to three selected modifiers for classes, methods, properties, and class constants, and zero to three property hooks. It compares the actual parser with small independent uniqueness and conflict rules. Baseline: 510 cases, zero failures. Exit 1 reports mismatched acceptance and the source; build failures are also nonzero.

This is bounded testing, not a proof of PHP's complete declaration grammar. It does not cover every visibility, promoted-parameter modifier, attribute, hook body, or enclosing class kind. The rules describe the selected parser contract, not PHP runtime semantics.

Review changes under `src/Language/PHP/`, `php-parser.cabal`, this skill, and `.agents/skills/state-check-support/`; cases focus on `src/Language/PHP/Parser/Statement.hs`. The sync commit is the last commit changing this file: `git log -1 --format=%H -- .agents/skills/verify-declaration-flow/SKILL.md`.

When covered code changes, inspect the diff and rerun the check. In a disposable worktree, replace the duplicate-hook guard `when (hookT \`elem\` seen)` with `when False`; duplicate-hook sequences must fail the check. Discard the worktree, rerun the original, and update this file's sync commit only after reviewing any contract changes. Use `/maintain-formal-verification-skill` when stale.
