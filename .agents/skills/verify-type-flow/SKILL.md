---
name: verify-type-flow
description: Check nullable, union, intersection, and DNF parsing phases and selected declaration-type restrictions. Use when changing PHP type parsing.
---

# Verify type flow

Nullable shorthand must stand alone. A bare intersection cannot be mixed into a union; a parenthesized intersection can. Accepted source must produce the expected outer AST type, not merely parse successfully.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-type-flow/TypeFlow.hs
```

The check enumerates one to three atoms chosen from two simple names, a nullable name, and a parenthesized intersection, joined by union or intersection operators. It checks both acceptance and AST shape through the real `parseType`. Eleven additional checks cover selected property and constant contexts, including that `public static $x` parses `static` as a modifier rather than a type. Baseline: 303 cases, zero failures. Mismatches print the type and actual result; exit 1 indicates failure and build failures are nonzero.

This is bounded testing, not an unbounded proof or a PHP runtime oracle. It does not cover arbitrary names, all nested forms, every type context, or PHP's complete semantic type restrictions.

Covered code: `src/Language/PHP/Parser/Type.hs`, `Parser/Statement.hs`, `Parser/Name.hs`, `Parser/Lexer.hs`, `AST.hs`, and this skill and `state-check-support`. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-type-flow/SKILL.md`.

After a covered change, review the diff and rerun. In a disposable worktree disable the guard in `failIfNullable`; unions containing `?A` must fail this check. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` if stale.
