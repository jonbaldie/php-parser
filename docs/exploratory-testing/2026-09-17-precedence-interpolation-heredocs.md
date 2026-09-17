# Exploratory Testing Report: Precedence, Interpolation, Heredocs & Trivia

Date: 2026-09-17  
Starting revision: `8f92f6f` (library `0.1.6.0`)  
Toolchain: GHC 9.12.1, Cabal, macOS aarch64  
Independent oracle: PHP CLI 8.4.1 — `php -l` for accept/reject, **and `php <file>` for runtime behaviour**  
Existing test suite status: `cabal build` clean; `cabal test` — all 404 tests passed.  
Evidence directory: `exploratory-evidence/2026-09-17-afk/`

Drivers:
- `Harness.hs` — streaming differential driver: parse, `queryStmt allVariables`, `prettyPrint`, reparse, AST-equality round-trip check, one case per line.
- `probe.py` — feeds a JSON case list through the harness and cross-checks each case against PHP, emitting verdicts `FALSE_REJECT`, `FALSE_ACCEPT`, `REPARSE_FAIL`, `ROUNDTRIP_DIFF`, `PRINTED_INVALID`, `BEHAVIOUR_DIFF`.
- `Refactor.hs` + `rename_probe.py` — rename-and-execute oracle: rename one variable via `transformStmt`, then require PHP to produce identical output from the original and the renamed program.
- `Idem.hs` — formatter idempotence checker (`prettyPrint . parse` must reach a fixed point after one pass).
- `Dump.hs` / `TriviaDump.hs` — AST and annotation reducers used to explain surprises.
- `Replay.hs` — replays all ten confirmed findings from a clean start; transcript in `replay-output.txt`.

**Method note.** The novel lever this pass added was executing both the original and the pretty-printed program with PHP and comparing stdout. AST-equality round-tripping cannot see a printer that emits a *different but still self-consistent* program; running it can. Findings F1, F2, F5 and F8 were all caught this way.

---

## Journeys Exercised

### A. Static analysis: accept exactly what PHP accepts

Goal: a linter parses a file with `parseProgram` and reaches the same accept/reject verdict as `php -l`. A false reject makes the rest of the file invisible; a false accept lets broken code through to production.

- **Ordinary path**: 238 differential snippets across `cases1.json`–`cases6.json` (precedence ladder, string interpolation forms, compound assignment, heredoc/nowdoc, numeric literals, declarations, lexical edge cases). The large majority agreed with PHP.
- **Variations attempted**:
  1. *Full compound-assignment sweep* (`cases4.json`): all thirteen operators. Exactly three — `**=`, `<<=`, `>>=` — are rejected ([#237](https://github.com/jonbaldie/php-parser/issues/237)); the other ten parse.
  2. *Interpolation offsets*: `"$a[k]"`, `"$a[0]"`, `"$a[$i]"`, `"$a[-1]"`, `"$a[+1]"`. Only `"$a[-1]"` disagrees — PHP accepts it, the library rejects ([#235](https://github.com/jonbaldie/php-parser/issues/235)). `"$a[+1]"` is correctly rejected by both.
  3. *Heredoc closer indentation*: less-indented, equally indented, and over-indented closers. The over-indented closer is a false accept ([#240](https://github.com/jonbaldie/php-parser/issues/240)).
  4. *Numeric literals*: `0`, `07`, `08`, `09`, `010`, `0o10`, `0x1f`, `0b101`, `1_000`, `0_1`. Legacy octal decoding is correct (`010` → 8), but the digit range is unchecked, so `08`/`09` are accepted ([#241](https://github.com/jonbaldie/php-parser/issues/241)).

### B. Formatting: print a program that still means the same thing

Goal: a formatter parses, `prettyPrint`s, and writes the file back. The reprinted file must reparse to an equivalent AST, be accepted by `php -l`, **and produce byte-identical output when executed**.

- **Ordinary path**: the printed form reparsed to an equal `stripAnnotations` AST for the great majority of the 238 cases, and 231 cases reached a printing fixed point after one pass (`idem_report.txt` is empty — no idempotence violations found).
- **Variations attempted**:
  1. *Mixed-operator expressions*: `.` is bound at additive precedence, so `"a" . 1 + 2` parses as `(("a" . 1) + 2)` and the printed program fatals; `1 . 2 << 3` prints 96 where PHP prints 116 ([#232](https://github.com/jonbaldie/php-parser/issues/232)).
  2. *Interpolation printing*: `"$a[k]"` is printed as `"{$a[k]}"`, turning a string key into a constant fetch ([#233](https://github.com/jonbaldie/php-parser/issues/233)).
  3. *Heredoc printing*: escape sequences are decoded into the stored body and re-emitted raw, so `\t` becomes a tab and `\$notvar` becomes a live interpolation ([#236](https://github.com/jonbaldie/php-parser/issues/236)).
  4. *Comments and trivia*: a comment with no following statement is dropped entirely; `<?php /* only a comment */` prints as an empty program ([#238](https://github.com/jonbaldie/php-parser/issues/238)).
  5. *Close tags*: a `?>` that terminates a `//` or `#` comment ends the PHP block in PHP. The library silently truncates the program there and returns `Right`, so the formatter deletes the remaining inline HTML and the following PHP block ([#239](https://github.com/jonbaldie/php-parser/issues/239)).
  6. *Stress* (`cases6.json`): 200 nested parentheses, 150 nested arrays, a 2000-term concatenation, 2000 statements, 100 nested `if`s, a 300-link nullsafe chain, and a 300-member class all parsed and round-tripped cleanly with no stack or performance failure.

### C. Refactoring: rename a variable without changing behaviour

Goal: a refactoring engine lists variables with `queryStmt allVariables`, renames one to a fresh name with `transformStmt`, prints the result, and PHP produces identical output before and after.

- **Ordinary path**: 55 rename cases (`cases3.json`) covering assignments, arithmetic, closures with `use`, `foreach`, string interpolation, array access, and nested functions. Renames were behaviour-preserving in all of them except the heredoc case.
- **Variations attempted**:
  1. *Variable inside an interpolating heredoc*: `allVariables` reports the variable once instead of twice, and the rename leaves the heredoc occurrence behind, producing an undefined-variable warning at runtime ([#234](https://github.com/jonbaldie/php-parser/issues/234)). This is the same class as the already-fixed #122 for `LitInterpolated`.
  2. *Nowdoc*: correctly inert — a `$v` inside `<<<'EOT'` must not be renamed, and is not.
  3. *Double-quoted interpolation and `{$a["k"]}`*: renames correctly in lockstep.

---

## Confirmed Findings (Filed in Issue Tracker)

Each was reduced to a minimal reproducer, grounded against PHP 8.4.1, and replayed 3/3 times from a clean start via `Replay.hs` (transcript: `exploratory-evidence/2026-09-17-afk/replay-output.txt`). All filed with labels `bug` and `ready-for-agent`.

| Issue | Finding | Impact |
| ----- | ------- | ------ |
| [#232](https://github.com/jonbaldie/php-parser/issues/232) | F1: `.` is parsed at additive precedence | `"a" . 1 + 2` becomes `(("a" . 1) + 2)`; PHP 8 moved `.` below `+`/`-`/`<<`/`>>`. Printed program fatals; `1 . 2 << 3` prints 96 vs PHP's 116. Root cause: `OpConcat` sits in `parseAddSub`. |
| [#233](https://github.com/jonbaldie/php-parser/issues/233) | F2: `"$a[k]"` printed as `"{$a[k]}"` | The unquoted key becomes a constant fetch → `Error: Undefined constant "k"`. Also a `ROUNDTRIP_DIFF`. |
| [#234](https://github.com/jonbaldie/php-parser/issues/234) | F4: heredoc bodies opaque to traversals | `LitHeredoc` stores flat text, so `allVariables`/`transformExpr` never see variables inside it; renames corrupt the program. |
| [#235](https://github.com/jonbaldie/php-parser/issues/235) | F3: `"$a[-1]"` rejected | Valid PHP; simple interpolation allows a leading `-` on the offset. |
| [#236](https://github.com/jonbaldie/php-parser/issues/236) | F5: heredoc bodies re-emitted unescaped | `\t` → real tab, `\$notvar` → live interpolation. `LitHeredoc` carries no raw text to fall back on. |
| [#237](https://github.com/jonbaldie/php-parser/issues/237) | F6: `**=`, `<<=`, `>>=` rejected | `parseTernary` runs first and `**`/`<<`/`>>` lack `notFollowedBy '='` guards, so the `parseAssignOp` branches for them are unreachable. |
| [#238](https://github.com/jonbaldie/php-parser/issues/238) | F7: trailing comments dropped | Trivia is only attached as *leading* trivia, so anything after the last statement is discarded. |
| [#239](https://github.com/jonbaldie/php-parser/issues/239) | F8: code after a `?>` inside a line comment dropped | `<?php echo "x"; // c ?>tail<?php echo "y";` prints `xtaily` in PHP; the library keeps only the first `echo` and returns `Right`. Silent code loss. |
| [#240](https://github.com/jonbaldie/php-parser/issues/240) | F9: over-indented heredoc closer accepted | PHP: `Invalid body indentation level`. |
| [#241](https://github.com/jonbaldie/php-parser/issues/241) | F10: `08`/`09` accepted as 8/9 | PHP: `Invalid numeric literal`. Octal *decoding* is correct; only the digit-range check is missing. |

## Unresolved Candidates

None. Every candidate raised in this pass was either confirmed and replayed, or rejected with evidence below.

---

## Rejected Candidates & Observations (Not Filed)

1. **`1 + 2 . "x"`, `"a" . 2 * 3`, `"a" . 1 == "a1"`** — suspected further precedence damage. Rejected: all three parse with the correct association and execute identically before and after printing. The concatenation defect is confined to `.` versus `+`/`-`/`<<`/`>>`, which is exactly what #232 states.
2. **`${var}` normalised to `{$var}` on printing** — a printed-text difference, but behaviour-preserving and it removes a PHP 8.2 deprecation. Deliberate improvement, not a bug.
3. **Formatter idempotence** — 231 cases, zero violations (`idem_report.txt` empty). Not a defect; recorded because it bounds the printer problems above: they are *first-pass* corruptions, not divergence.
4. **Stress and depth limits** — no failure found at 200 nested parens, 150 nested arrays, 2000-term concatenation, 2000 statements, 100 nested `if`s, 300-link nullsafe chain, 300-member class.
5. **PHP 8.5-only syntax** — cases marked `"oracle": false` cannot be differentially checked on PHP 8.4.1 and were excluded from accept/reject verdicts rather than counted as disagreements.

---

## Usability Observations

Grounded in the journeys above; these are observations, with suggestions marked as such.

- **A `Right` result does not mean the whole file was parsed.** #239 is dangerous precisely because the API gives no signal: a consumer has no way to learn that input remained. *Suggestion*: either error on unconsumed input, or expose how much of the source the returned `Program` spans.
- **Literal constructors are inconsistent about raw text.** `LitString` and `LitInt` retain their source text (`LitInt 8 "08"`), which is what makes faithful reprinting possible; `LitHeredoc` does not. #236 follows directly from that asymmetry.
- **Round-trip AST equality is a weaker guarantee than it looks.** #232 and #236 both produce printed programs that reparse to an equal AST while meaning something different. Consumers relying on `stripAnnotations ast == stripAnnotations ast'` as a formatter safety check will not be protected.
- **Trivia has one attachment direction only.** Leading-only trivia is a reasonable model until the end of the file, where it silently loses data (#238).

---

## Scope and Limitations

- Three user-critical journeys completed (static analysis, formatting, refactoring), each with an ordinary path and several variations, with lasting effects checked by executing the printed and renamed programs.
- Oracle is PHP CLI **8.4.1**, so PHP 8.5-only grammar could not be differentially validated; those cases were opted out rather than judged.
- Differential coverage is 238 accept/print cases + 55 rename cases + 231 idempotence cases + 7 stress cases. It is sampled, not exhaustive: absence of a finding in an area is weak evidence.
- No library source was modified during the pass. `cabal test` (404 tests) was run before the pass and passed; the pass was read-only with respect to the library.
- Temporary PHP files written under `/tmp` by the drivers were deleted by the drivers themselves. Compiled driver artifacts (`.o`, `.hi`, binaries) were removed. Drivers and case corpora remain in `exploratory-evidence/2026-09-17-afk/`, which is local and untracked, matching the convention of previous passes.
