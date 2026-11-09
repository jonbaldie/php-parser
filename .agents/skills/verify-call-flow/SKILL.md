---
name: verify-call-flow
description: Check argument and parameter ordering and nullsafe receiver history. Use when changing call parsing, parameter validation, or postfix chains.
---

# Verify call flow

A positional argument must not follow a named or unpacked argument. Parameter names must be unique and only the last parameter may be variadic. A nullsafe receiver chain cannot be used as a write target or a first-class method callable.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-call-flow/CallFlow.hs
```

The real parser is compared with finite rules for every argument and parameter sequence of up to three items. Twelve postfix examples compare ordinary and nullsafe receivers across calls, writes, and increments. Baseline: 137 cases, zero failures. Mismatches include the source; exit 1 signals failure and build failures are nonzero.

These are bounded tests, not a proof or a PHP runtime check. Argument names and unpacked values are syntax only; no call executes. The check does not enumerate every postfix constructor, expression, parameter context, or attribute.

Covered code: `src/Language/PHP/Parser/Internal.hs` (argument and parameter checks, postfix chain and receiver predicates), `Parser/Statement.hs` (function parameters), `Parser/Expression.hs`, `Parser/Lexer.hs`, `AST.hs`, and this skill and `state-check-support`. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-call-flow/SKILL.md`.

After a covered change, inspect the diff and rerun the check. In a disposable worktree change the result of the `isPositional arg && seenNamed` guard in `hasPositionalAfterNamed` from `True` to `False`. The named-then-positional examples must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` when stale.
