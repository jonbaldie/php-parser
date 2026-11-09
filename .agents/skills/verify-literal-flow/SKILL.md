---
name: verify-literal-flow
description: Check string, command, comment and heredoc scanning and heredoc printing. Use when changing literal lexing or formatting.
---

# Verify literal flow

Escapes must decode as specified in the selected examples, comments must stop before a close tag, unterminated literals must reject promptly, and printing a parsed interpolated literal must not change its annotation-free AST. In particular, a decoded heredoc line resembling the closing label must not terminate the printed heredoc early.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-literal-flow/LiteralFlow.hs
```

The check runs 32 explicit cases against the real lexer, parser, and printer: selected single and double quotes and backticks; two heredoc labels with interpolated and non-interpolated bodies; nowdocs; one indentation example; close-tag line-comment boundaries; and four unterminated inputs with a two-second timeout each. Baseline: zero failures. It prints rejected sources or changed ASTs and exits 1 on failure.

This is bounded testing, not a proof. It does not enumerate all escape sequences, indentation layouts, Unicode, comment positions, labels, or nesting contexts. AST comparisons strip source annotations; the separate comment-boundary checks count trivia directly.

Covered code: `src/Language/PHP/Parser/Lexer.hs`, `Parser/Internal.hs`, `Parser/Statement.hs`, `Pretty.hs`, `AST.hs`, and this skill and `state-check-support`. The sync commit is `git log -1 --format=%H -- .agents/skills/verify-literal-flow/SKILL.md`.

After a covered change, inspect the diff and rerun. In a disposable worktree replace `True : repeat False` with `False : repeat False` in `prettyHeredocParts`. The decoded label-at-line-start cases must fail. Discard the worktree, rerun the original, and update this file's sync commit only after review. Use `/maintain-formal-verification-skill` if stale.
