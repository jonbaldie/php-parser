# 2026-10-10: Real-world corpus round trip

Scope: a consumer program drives the public API (`parseProgram`, `prettyPrint`, `queryStmt`, `getAnnotation`, `formatParseError`) over 6,341 real-world PHP files. The corpus is laravel/framework at `08f9417` plus its vendored symfony, nikic/php-parser, nesbot/carbon, guzzlehttp, league, monolog, phpunit and doctrine sources. Library under test: `0.1.13.0` (commit `aa3712d`).

## Setup

- GHC 9.12.1, Cabal 3.16.1.0. Dependencies come from the shared Cabal store, and build outputs stay in the workspace's `dist-newstyle`.
- Before this pass the baseline test suite ran clean: all 591 tests passed.
- Oracle: `php -n -l` on PHP 8.2.33, 8.3.33, 8.4.25 and 8.5.9. All four accept all 6,341 corpus originals, so any parser rejection is a false reject.
- Consumer: a separate Cabal package that depends on `php-parser` through `cabal.project`. It has 4 modes:
  - `corpus`: parse, print, reparse, and compare ASTs with `stripAnnotations`.
  - `print`
  - `ast`
  - `spans`: checks every variable's span against the source text.
- Evidence (driver, corpus, logs, counterexamples, bisect scripts) lives locally under `exploratory-evidence/2026-10-10-afk-run/` and is not committed.

## Journeys

1. **Parse real code.** Goal: `parseProgram` accepts every file PHP accepts.
   - Ordinary path: the full corpus.
   - Variation: each failure was reduced with line-level delta debugging (`reduce.py`) to a one-line reproducer. The reproducer was checked against all four interpreters and bisected across releases with a detached worktree.
   - Result: 689 of 6,341 files rejected. All are false rejects, and they fall into findings 1–5 below.
2. **Format and re-run.** Goal: `prettyPrint` output is valid PHP with the same AST and runtime behaviour, and printing is idempotent.
   - Ordinary path: print every parsed file, then reparse it, compare ASTs, and lint with `php -l`.
   - Variation: print the printed output again, and execute reduced reproducers before and after printing.
   - Result: all 5,652 printed files lint clean. However, 88 files have a different AST and 2,970 files are not a fixed point (finding 6).
3. **Locate nodes by span.** Goal: every node's span lets a consumer recover the source text it covers.
   - Ordinary path: for all 125,596 simple variables in the parsed corpus, the source slice at `posOffset` must equal `$name`, and `posLine`/`posColumn` must match values recomputed from the offset.
   - Variation: minimal inputs with combining marks and wide characters before a variable, to explain the mismatches.
   - Result: offsets were exact in every case, and line/column matched except in 2 cases explained under *Rejected*.

## Confirmed findings

Filed as #367–#373. Each reproducer was replayed 3 times from a fresh build with identical results, and each is accepted by all four interpreters (findings 1–5) or changes runtime output (finding 6).

| Issue | Direction | Construct | Corpus files | Since |
| ----- | --------- | --------- | ------------ | ----- |
| [#367](https://github.com/jonbaldie/php-parser/issues/367) | false reject | closures with statement bodies, and anonymous classes, as call arguments: `f(function () { return 1; })` | ~370 | ≤ 0.1.5 |
| [#368](https://github.com/jonbaldie/php-parser/issues/368) | false reject | assignment as right operand of a tighter operator: `$a && $b = 1`, `$a ?? $c = 1`, `! $v = 1` | ~280 | 311a2f0 (#300) |
| [#369](https://github.com/jonbaldie/php-parser/issues/369) | false reject | ternary assignment inside short ternary: `$l ?: $l = $a ? 1 : 2` | 1 | a8f4d08 (#299) |
| [#370](https://github.com/jonbaldie/php-parser/issues/370) | false reject | `throw` as operand of `\|\|`, `&&`, arithmetic, `!` | 11 | ≤ 0.1.5 |
| [#371](https://github.com/jonbaldie/php-parser/issues/371) | false reject | method named `class`: `public function class() {}` | 4 | ≤ 0.1.7 |
| [#372](https://github.com/jonbaldie/php-parser/issues/372) | false reject | `@require $f`, `@include "x"` | 1 | ≤ 0.1.7 |
| [#373](https://github.com/jonbaldie/php-parser/issues/373) | **printer** | embedded newlines in strings, inline HTML and comments re-indented inside blocks | 88 AST diffs, 2,970 non-fixpoints | n/a |

File counts are approximate because some files hit more than one cluster.

### Highlights

- **#367** is the largest cluster. `parseCallArgs` uses the stub `parseExpr` (`Expression.hs:160`), so a closure body in call-argument position accepts only expression statements. This is the same tie-the-knot defect as #321, surfacing in call arguments.
- **#368 and #369 are regressions** from the parser-strictness fixes in 0.1.8–0.1.11. Both forms were accepted at `fc3069a` (0.1.7). The first bisect blamed a docs commit because the assumed-good endpoint was itself bad. Re-testing the endpoints directly corrected this, and `git bisect` over built commits then identified each culprit.
- **#373 changes runtime behaviour.** For `function f() { return 'a⏎b'; }`, the printed program returns `"a\n    b"`, and each further print adds 4 more spaces. Every one of the 88 AST diffs and 2,970 non-fixpoints traces to this one cause: Prettyprinter's `pretty :: Text -> Doc` turns embedded newlines into `line`, which picks up the enclosing `indent 4`. Heredocs are re-indented together with their closer, so their values survive.

## Rejected candidates

- **Printed output may be invalid PHP.** Rejected: 5,652 of 5,652 printed files pass `php -n -l` on 8.5.
- **Column is wrong after non-ASCII text** (2 variables in `nesbot/carbon/src/Carbon/Lang/si.php:36`, column 37 where the code-point count gives 38). Rejected as a bug. `posColumn` comes from megaparsec ≥ 9.7, which counts *display width*: combining marks count 0 and East Asian wide characters count 2. Minimal checks:
  - `'e◌́'; $x` gives column 12, one less than the code-point count.
  - `'漢'; $x` gives column 13, one more.
  - `posOffset` is an exact code-point offset in every case.
  
  This is deliberate upstream behaviour, but it is undocumented here (see usability).

## Unresolved

None.

## Usability observations

- **Error locations point at the outer expression.** For #367 the error is reported where the enclosing expression starts, not at the closure. For example, Laravel's `Process/Pool.php` reports `72:16: unexpected "ne"` at `return new InvokedProcessPool(`, while the offending closures sit on lines 74–82. Suggested improvement: commit to the closure branch (`M.try` scope) so the error lands inside it.
- **#371's message is misleading.** It reports `unexpected end of input` at column 38 of a complete one-line file.
- **`SourcePos` has no field documentation.** A consumer cannot tell that `posColumn` is display width (via megaparsec), not code points or bytes, and that `posOffset` counts code points. Suggested improvement: one Haddock line per field.

## Limitations

- The corpus is one ecosystem (Laravel and its dependencies). It contains no PHP 8.4/8.5-only syntax beyond what those projects use.
- AST comparison uses `stripAnnotations`, so trivia and spans are not compared across the round trip. Span checks covered simple variables only.
- The reduction is line-based, and the final one-line reproducers were minimised by hand.
- "Since" versions come from building release tags in a worktree. Only #368 and #369 were bisected to a single commit.
