# 2026-09-21 — Coverage-guided differential testing: the file shell

Scope: the parser's lexer/declaration entry paths reached by mutating the
*file shell* — open tags, inline HTML transitions, declare pragmas, the
`<?=` terminator, class-body placement — rather than the code inside
statements. Library under test: `0.1.7.0` (commit `fc3069a`).

## Setup

- Oracle: a real `php -l` per version — 8.2.33, 8.3.33, 8.4.25, 8.5.9 —
  installed via `shivammathur/php`. (The previously installed Homebrew
  prefixes had drifted to report PHP 8.5; the oracle's verify-not-trust
  discovery refused them and skipped `no false accepts` until they were
  reinstalled. Worth remembering when the oracle group is mysteriously
  short of properties.)
- Existing suite deep-fuzzed first: the oracle group at 1000 QuickCheck
  tests per property with all four interpreters — fully green. The bug
  batch below was found *after* that green run.

## What was new

The suite's generator corpus (`Test.Gen.PHPSource.renderProgram`) always
opens with `<?php\n\n`, never emits inline HTML, never re-opens a tag,
only ever places `declare(strict_types=1);` first in plain mode, and only
uses `<?php` casing. Thirteen grammar/lexer/declaration rules live outside
that shell's reach by construction. A CGPT harness that mutates the file
shell (trivia frames, junk-atom injection at arbitrary positions, seed
splicing, energy by AST-shape novelty) plus targeted probe matrices made
all of them reachable.

Full detail, drivers, and the reproduction transcripts are kept locally
under `exploratory-evidence/2026-09-21-cgpt-differential/` (not committed).

## Findings

Filed as #271–#283, each reproduced three consecutive times against all
four interpreters and minimised. PHP verdicts unanimous across versions.

| Issue | Direction | Construct |
| ----- | --------- | --------- |
| [#271](https://github.com/jonbaldie/php-parser/issues/271) | false accept | a second `<?php` open tag mid-code is silently swallowed |
| [#272](https://github.com/jonbaldie/php-parser/issues/272) | **false reject** | full open tag matched case-sensitively (`<?PHP`) |
| [#273](https://github.com/jonbaldie/php-parser/issues/273) | false accept | `declare(strict_types=...)` rules: position, block mode, 0/1 value |
| [#274](https://github.com/jonbaldie/php-parser/issues/274) | false accept | `declare(encoding=...)` accepted when not the first statement |
| [#275](https://github.com/jonbaldie/php-parser/issues/275) | **false reject** | `declare(encoding='UTF-8' . '')` — compile-time-constant values |
| [#276](https://github.com/jonbaldie/php-parser/issues/276) | false accept | `<?= 1` accepted at EOF without `;` or `?>` |
| [#277](https://github.com/jonbaldie/php-parser/issues/277) | false accept | plain statements inside class and trait bodies |
| [#278](https://github.com/jonbaldie/php-parser/issues/278) | false accept | duplicate property hook on one property |
| [#279](https://github.com/jonbaldie/php-parser/issues/279) | false accept | form feed (0x0C) treated as code whitespace |
| [#280](https://github.com/jonbaldie/php-parser/issues/280) | false accept | unparenthesized nested ternary chains |
| [#281](https://github.com/jonbaldie/php-parser/issues/281) | false accept | invalid `\u{...}` escapes (empty, codepoint > 0x10FFFF) |
| [#282](https://github.com/jonbaldie/php-parser/issues/282) | false accept | `---1` parsed as unary chains instead of decrement lexing |
| [#283](https://github.com/jonbaldie/php-parser/issues/283) | false accept | assignment to non-assignable expressions (`$a3.0 = 1`) |

The two false rejects are the first this suite has recorded; a false reject
hides otherwise-valid files from every downstream consumer, so they matter
most.

## Non-findings worth keeping

- `\x0b` (vertical tab) in code is accepted by *PHP* too — only `\x0c`
  (form feed) diverges. Don't "fix" the wrong control character.
- `"\u{D800}"` (surrogate) is accepted by both sides.
- `<? $x = 1;` at file start is accepted by both under `php -n`; with
  leading HTML both reject.
- `public public(set)` / `protected protected(set)` are legal PHP 8.4+
  spellings (both accept); `private public(set)` is rejected by both
  (#193 rule holds). The library already implements "Multiple access type
  modifiers" with the exact PHP message.
- `$x = 0x_FF;` and `$x = 08;` are rejected by both — the existing
  lexer rules hold.
- Duplicate class const/property/method false accepts and import-name
  redeclares were re-observed and are already pinned by the catalogue and
  `outOfContractRules` respectively.
- No crashes, hangs, or round-trip failures in ~1700 adversarial mutated
  programs (including 20,000-deep parens, 1M-digit literals, 100k-char
  identifiers); no hlint errors; GHC warning set clean.
