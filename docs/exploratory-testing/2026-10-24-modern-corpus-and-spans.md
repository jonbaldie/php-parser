# 2026-10-24: Modern corpus, Laravel re-run and node-span slices

Scope: a consumer program drives the public API (`parseProgram`, `parseExpression`, `parseStatement`, `prettyPrint`, `stripAnnotations`, `queryStmt allExprs`, `nodeSpan`). It runs over 13,041 modern PHP files:

- tempestphp/tempest-framework at `1509573` (3.x, requires PHP ^8.5), 2,793 files.
- symfony/symfony at `6f841c0` (8.0), 10,252 files.

It also re-runs the 6,341-file Laravel corpus from 2026-10-10. Library under test: `0.1.13.0` (commit `f6272ca`).

## Setup

- GHC 9.12.1 and Cabal 3.16.1.0. The project has no lockfile (no `cabal.project.freeze`), so the build used plain `cabal build` with dependencies from the shared Cabal store. Build outputs stay in the workspace `dist-newstyle`.
- Baseline: `cabal test all` passed all 642 tests before the pass (`logs/baseline-test.log`). The PHP oracle tests partly skip without `PHP82_BIN` and the other version variables.
- Oracle: `php -n -l` on PHP 8.2.33, 8.3.33, 8.4.25 and 8.5.9. 13,041 of the 13,044 corpus files pass on 8.5.9, and only those were used (`logs/valid-85.txt`).
- Consumer: a separate Cabal package that depends on `php-parser` through `cabal.project`. It extends the 2026-10-10 driver with 3 modes:
  - `rawast` prints the annotated AST.
  - `nodespans` slices the source at `nodeSpan` for every expression and statement, re-parses the slice with `parseExpression`/`parseStatement`, and compares stripped ASTs.
  - `badspans` lists each failing expression.
- No Docker was used.
- Evidence (driver, corpus, logs, probes, counterexamples, replay script) lives locally under `exploratory-evidence/2026-10-24-afk-run/` and is not committed.

## Journeys

1. **Parse real modern code.** Goal: `parseProgram` accepts every file PHP 8.5 accepts.
   - Ordinary path: the full corpus.
   - Variation: re-run the 2026-10-10 Laravel corpus to check the fixes for #367–#372. Each remaining failure was reduced and probed across operand positions against all four interpreters.
   - Result: 2 of 13,041 files rejected (#389). The Laravel corpus improved from 689 rejects at the earlier `0.1.13.0` build to 1, which is the same cause.
2. **Format and re-run.** Goal: `prettyPrint` output is valid PHP with the same AST, and printing is idempotent.
   - Ordinary path: print, reparse, compare, and lint the printed file on 8.5.
   - Variation: print the printed output twice more, then reduce the non-fixpoints and lint failures.
   - Results:
     - 62 files are not a fixed point. All are comment-only differences, and every one of those files contains a hooked property (#390).
     - 2 printed Symfony files fail lint (#392). The cause is hidden from the AST round trip by #393.
     - On Laravel, the 3 non-fixpoints come from #390 and #391.
3. **Locate nodes by span.** Goal: slicing the source at `nodeSpan` yields that node's source text.
   - Ordinary path: 3,529,996 expressions and 344,993 statements.
   - Variation: reduce each failing constructor to one line, check the README example, and build the consumer against older commits.
   - Result: 264,392 expressions in 7,624 files have wrong spans (#394, #395). For statements, the 101 failures are all `<?=`/`?>` template statements (see *Rejected*).

## Confirmed findings

Filed as #389–#395. Each reproducer was replayed 3 times from a fresh scratch directory with `replay.sh` (`logs/replay-{1,2,3}.txt`). The outputs were identical apart from the temporary directory name.

| Issue | Direction | Construct | Corpus impact | Since |
| ----- | --------- | --------- | ------------- | ----- |
| [#389](https://github.com/jonbaldie/php-parser/issues/389) | false reject | `include`/`require`/`print`/`yield` as cast, binary-right or `??`-right operand: `(string) include $f`, `false === include $f`, `!print 1` | 2 Symfony files + 1 Laravel | ≤ 43e341e |
| [#390](https://github.com/jonbaldie/php-parser/issues/390) | printer | the doc comment on the member after a hooked property is duplicated, and doubles on each print | 62 files + 2 Laravel | ≤ 43e341e |
| [#391](https://github.com/jonbaldie/php-parser/issues/391) | printer | comments in skipped `list()` slots are duplicated, gaining one copy per print | 1 Laravel | not checked |
| [#392](https://github.com/jonbaldie/php-parser/issues/392) | printer | parentheses dropped on the instanceof right operand: `$x instanceof ($a->b())` prints as invalid PHP | 2 Symfony files | ≤ 43e341e |
| [#393](https://github.com/jonbaldie/php-parser/issues/393) | false accept | `$x instanceof f()`, `A::B`, `A::class`, `A::m()`, `"A"` accepted; PHP 8.2–8.5 reject them | hides #392 | ≤ 43e341e |
| [#394](https://github.com/jonbaldie/php-parser/issues/394) | span | method, static and nullsafe call spans end before `(args)`: `$this->a(1)` slices as `$this->a` | 221,367 nodes, 7,524 files | ≤ v0.1.0 |
| [#395](https://github.com/jonbaldie/php-parser/issues/395) | span | spans exclude parentheses and braces at their edges: `1 + ($a)` slices as `1 + ($a` | ~10,700 nodes, ~1,700 files | ≤ 43e341e |

### Highlights

- **#394 affects almost every file.** The span is built as `combineSpans (annotation base) (annotation name)` and ignores the parsed arguments. The README's span example is correct for `echo 1;`, but applied to `$this->send($msg, 2);` it slices the call as `"$this->send"`. Function calls and `new` are correct.
- **#392 and #393 hide each other.** The printer drops parentheses that PHP requires, and the parser accepts the broken output with the same AST. Only linting the printed file with PHP exposed the problem.
- **#390 and #391 are trivia duplication from backtracking.** Trivia is buffered in `State` beneath `ParsecT` (#320), so a backtracked branch can re-attach comments. Printing then repeats them, and the count grows on every print, from 2 to 4 to 8 for #390.
- **#389 is the same shape as #370.** #383 fixed unary prefix operands only.

## Rejected candidates

- **`<?=` and `?>` statement spans** (101 statements). The span of `<?= $x ?>` covers the expression only, and the open tag and terminator are lexical. The slice is not a parseable statement on its own. This is recorded as an observation, not a bug, because no span contract says tags are included.
- **#390 covers non-hook comments.** A first heuristic flagged 54 non-fixpoints as having no hook just before the duplicate. That check was broken: it inspected already-duplicated context and used `\s`, which BSD `grep -E` does not support. A POSIX recheck shows that all 62 files contain a hooked property, and the reduced reproducer needs one: the same class with a plain property prints 1 copy, but with a hooked property it prints 2.
- **Every instanceof print is invalid.** Rejected: `($a->b)`, `($a[0])` and `($a::$b)` print without parentheses and are still valid PHP.

## Unresolved

None.

## Usability observations

- **README snippets do not compile as written.** The `nodeSpan` example needs `{-# LANGUAGE OverloadedStrings #-}`. Without it, GHC reports `Couldn't match type '[Char]' with 'T.Text'` (`logs/readme-compile-as-written.txt`). Suggested improvement: show the pragma and imports.
- **#389 errors point at the keyword with a truncated token** (`unexpected "in"` or `unexpected "inc"`). They do not say that a keyword expression is not allowed there.
- **No documented span contract.** Nothing states whether a span includes surrounding parentheses, argument lists or `<?=`/`?>` tags. Consumers cannot tell #394 and #395 from intended behaviour without the README example. Suggested improvement: document that `nodeSpan` covers every token of the node's source, then add a slice-and-reparse property test.

## Limitations

- No lockfile exists, so the install was not frozen. Dependency versions are whatever the Cabal solver chose from the shared store at build time.
- The corpus oracle is PHP 8.5 only, because Tempest requires ^8.5. Reduced reproducers were checked on 8.2–8.5.
- AST comparison uses `stripAnnotations`. #390 and #391 were found through fixpoint failures, not AST differences.
- The span check covers expressions and statements reachable via `queryStmt allExprs`. Class members, types and params were not sliced. The #395 counts are a heuristic (non-call `ERR`/`DIFF` slices). Expressions inside `StmtInlineHtml` were skipped.
- "Since" versions come from building the consumer against detached commits (`43e341e`, `333a0da`, v0.1.0). They are not bisected.
