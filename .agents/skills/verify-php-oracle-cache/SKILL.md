---
name: verify-php-oracle-cache
description: Check PHPOracle cache keys, verdict reuse, handle isolation, and concurrent distinct insertions. Use when changing the differential test oracle.
---

# Verify the PHP oracle cache

A resolved oracle caches both accepted and rejected verdicts by exact source text. Separate handles have separate caches. Concurrent insertions for different sources must not lose cached entries.

From the repository root run:

```bash
bash .agents/skills/verify-php-oracle-cache/check.sh
```

The check compiles the real `test/Test/Oracle/PHP.hs` and its dependencies. It resolves an executable fixture reporting PHP 8.4, records actual lint subprocess invocations in a temporary file, and checks six properties, including eight concurrent distinct misses followed by cache hits. The fixture returns one accepted and one rejected verdict; it does **not** implement PHP or validate PHP syntax. Baseline: six checks, zero failures. Extra arguments exit 2; failures or build errors are nonzero. Temporary files are removed on exit.

This is bounded testing, not a proof of concurrency safety. It does not assert single-flight behavior: concurrent misses for the same source may invoke the executable more than once. It does not replace the real-PHP oracle tests or their version checks.

Covered code: `test/Test/Oracle/PHP.hs` (resolution, cache, lint invocation, and diagnostic parsing), `test/Test/Gen/PHPSource.hs` (version type), `php-parser.cabal` (test dependencies), this skill's fixture and check, and `.agents/skills/state-check-support/SourceCheck.hs`. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-php-oracle-cache/SKILL.md`.

After a covered change, inspect the diff and rerun. In a disposable worktree change the cache lookup in `checkSource` to return `Nothing`. Repeated-source subprocess counts must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` when stale.
