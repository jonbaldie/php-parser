---
name: verify-trivia-flow
description: Check comment preservation, source-span ownership, and root annotation updates. Use when changing lexer trivia, speculative parsing, annotations, or printing.
---

# Verify trivia flow

A single source comment should appear once in the parsed AST and once after printing and reparsing. For shared spans, only the first annotation should own trivia. Widening a root expression's span must not overwrite child spans.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-trivia-flow/TriviaFlow.hs
```

**Known failing baseline:** 51 checks, 32 failures. Four comment styles are each exercised in ten concrete positions. Bracketed and anonymous namespaces, property hooks, trait adaptations, first-class and exit callables, methods, and typed constants duplicate the comment in the initial parse. The two controls pass. The output prints both trivia lists and the generated PHP. The program returns 1 if any case fails; it does not treat these failures as success.

For the separate ownership and widened-root checks, add `--ownership`; all 11 pass on the baseline. Add `--comments` to run only the 40 comment examples. Other options exit 2.

This is bounded testing against the real parser and printer, not a proof. It checks the listed positions and four comment spellings, not every lookahead, possible comment placement, parse error, or AST node. A failed parse or failed reparse is a failure, not a discarded test.

## Coverage and maintenance

Covered files include `src/Language/PHP/Parser/Lexer.hs` (trivia state, endpoints, and failed `withSpan` restoration), `Parser.hs` (first owner for each span), `Parser/Internal.hs` (parenthesized root annotation), `Parser/Statement.hs` (speculative lookahead), `Pretty.hs`, `AST.hs`, and this skill and `state-check-support`.

The sync commit is the last commit changing this file: `git log -1 --format=%H -- .agents/skills/verify-trivia-flow/SKILL.md`. If covered code changes, review its diff and rerun both groups. Do not remove or weaken the comment invariant to make the baseline green. Once the duplication defect is fixed, update the documented baseline after rerunning all cases.

To check sensitivity, use a disposable worktree and change the first `False` in `setAnnotation`'s `mapAccumL` step to `True`. Run `--ownership`: the widened-root case must report altered child spans. Restore that mutation, then remove the `Set.member sp seen` guard in `annotateWithTrivia`; the typed-constant ownership case must fail. Discard the worktree and rerun the unmodified group. Update the sync commit only after review; use `/maintain-formal-verification-skill` if the check is stale.
