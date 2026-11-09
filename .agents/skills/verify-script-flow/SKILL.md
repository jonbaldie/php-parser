---
name: verify-script-flow
description: Check PHP and HTML transitions, statement context, declaration order, and namespace form. Use when changing script parsing or shared lexer state.
---

# Verify script flow

A first namespace must not follow blocking top-level content, namespace forms must not mix, and `strict_types` must remain in the declaration prologue. A regression can make the parser accept an invalid file or reject a valid one.

From the repository root run:

```bash
bash .agents/skills/state-check-support/run-haskell.sh .agents/skills/verify-script-flow/ScriptFlow.hs
```

The check builds the current library with one worker. It compares parsing with an independent finite transition table for every sequence of up to three events chosen from a strict declaration, ordinary statement, empty statement, bracketed namespace, and unbracketed namespace. It also checks eight PHP and HTML boundary examples and three public lexer-state lifecycle cases. The baseline is 167 cases and zero failures. A failure prints the event sequence and source. Exit 1 means a check failed; a build failure also returns nonzero.

A rejection is expected for invalid sequences. The check is bounded testing of the real parser, not a proof or a PHP interpreter comparison. It does not enumerate arbitrary statements, deeper nesting, every tag arrangement, or shebang text.

## Coverage and maintenance

The covered code is `Parser/Lexer.hs` (`withStatement`, declaration and namespace state, and close-tag handshake), `Parser/Statement.hs` (`parseProgram`, script modes, and declaration checks), `Parser.hs`, `AST.hs`, and the case and runner files under this skill and `state-check-support`. Paths under `Parser/` are relative to `src/Language/PHP/`.

The sync commit is the last commit that changed this file. Find it with `git log -1 --format=%H -- .agents/skills/verify-script-flow/SKILL.md`. When covered code changes after that commit, inspect the diff and compare the transition table with the new code. Update the cases only for an intended contract change. In a disposable worktree, replace `namespaceDeclarationTooLate` with `pure False` and confirm that the check reports an ordinary statement followed by a namespace. Discard the worktree and rerun the original check. Move the sync commit forward by changing this file only after confirming the cases still describe the code. Use `/maintain-formal-verification-skill` if the check has fallen behind.
