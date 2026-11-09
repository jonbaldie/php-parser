---
name: verify-import-generator
description: Check the test source generator's stateful import deduplication. Use when changing import candidates or the PHP source generator.
---

# Verify import generation

Keep the first import whose bound names have not already been kept. A rejected grouped import must not reserve any of its names. Class, function, and constant bindings are distinct keys.

From the repository root run:

```bash
bash .agents/skills/verify-import-generator/check.sh
```

GHC evaluates `Imports.expr` in the actual `Test.Gen.PHPSource` module, so it calls the unexported `dedupeImports` directly without copying or exporting it. The expression compares it with a list-based reference rule for all sequences of up to four synthetic candidates, including overlapping groups and distinct symbol tables. Baseline: 781 cases, zero failures. Mismatches print the input, expected statements, and actual statements; exit 1 signals failure. Extra arguments exit 2.

This is bounded testing, not a proof or a test of every random generator output. The current candidate catalogue has distinct binding names, so sampling `genProgram` alone would not exercise collision removal. The test's candidates deliberately collide and do not claim to be the catalogue.

Covered code: `test/Test/Gen/PHPSource.hs` (`dedupeImports` and its binding-key convention) and this skill. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-import-generator/SKILL.md`. Review the catalogue separately if its binding convention changes.

After a covered change, inspect the diff and rerun. In a disposable worktree replace the `any` collision test in `dedupeImports` with `all`; overlapping grouped imports must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` if stale.
