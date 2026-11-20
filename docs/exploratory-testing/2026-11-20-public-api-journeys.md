# 2026-11-20: Public-API journeys and declaration validation

Scope: a consumer program drives the public API (`parseProgram`,
`parseExpression`, `parseStatement`, `prettyPrint`, `stripAnnotations`,
`queryStmt`, `transformExpr`, `nodeSpan`). It covers 11 synthetic valid
files (PHP 8.2–8.5 features), 22 malformed files, 297 real-world PHP files,
and fragment/diagnostic probes. The declaration-validation probes were
checked against `php -n -l` on PHP 8.5.4. Library under test: `0.1.15.0`
(commit `40ab880`).

## Setup

- GHC 9.12.1 and Cabal 3.16.1.0. The project has no lockfile (no
  `cabal.project.freeze`), so the build used plain `cabal build` with
  dependencies from the shared Cabal store. Build outputs stay in the
  workspace `dist-newstyle`.
- Baseline: `cabal test all --enable-tests --test-options='--pattern
  "!/PHP interpreter oracle/"'` passed all 736 tests before the pass;
  `cabal build all --enable-tests --ghc-options="-Wall -Werror"` was clean.
- The repository's own check scripts (`scripts/check-state-machines.sh`,
  `scripts/check-parser-lookahead.sh`, `scripts/check-fold-query-history.sh`)
  all passed pre-fix. The `FAIL` lines inside the fold-query script are the
  historical snapshot reports; it exits 0 with the `PASS` detection lines.
- Oracle: `php -n -l` on PHP 8.5.4 only. PHP 8.2–8.4 were not installed, so
  the oracle's 8.2–8.4 sub-groups skipped locally (`[SKIPPED: no binary
  reporting PHP 8.2/8.3/8.4]`). The full 8.2–8.5 matrix runs in CI.
- Consumer: a separate Cabal package, depending on the local `php-parser`
  by absolute path in `cabal.project`, with 7 modes: `roundtrip` (parse →
  print → reparse, asserting AST equality and a parse→print→parse
  fixpoint), `expr`/`stmt` (print + slice at `nodeSpan` + reparse), `spans`
  (slice every expression at `nodeSpan` and reparse), `traverse`
  (`queryStmt allVariables`, `allExprs`), `rename` (`transformStmt`
  renaming a variable), and `dump` (stripped-AST dump).
- Evidence (driver, corpora, logs, transcripts):
  `.audit/php-parser-uat-20261120/` locally; not committed.

Two confirmed findings (both fixed in this pass) and four settled items
(rejected candidates, known-divergence confirmations) came out of it.

## Journeys

1. **Parse and format real code.** Goal: `parseProgram` accepts every file
   PHP 8.5 accepts, and `prettyPrint` round-trips losslessly.
   - Ordinary path: 11 synthetic valid files and 297 real-world files
     (Laravel app under `author-publishing-desktop/website/{app,config,
     database,routes,tests}` and `vendor/nikic/php-parser/lib`).
     Result: 297/297 `AST_MATCH`, 297/297 `FIXPOINT_MATCH`, and the printed
     output lints clean under `php -n -l` for 297/297 — both before and
     after the fix.
   - Variation: 22 malformed files. 20 were rejected with precise
     `file:line:column` + expected-token diagnostics; the 2 accepted ones
     are pinned known divergences (see *Rejected*).
   - Variation: robustness. BOM and CRLF accepted with fixpoints; NUL
     rejected; 5,000 nested parens in 0.23 s; 3,000 nested arrays in
     0.34 s; 2,000 concatenations in 0.90 s; 5,000 statements in 1.58 s —
     all with AST fixpoint matches. No freezes, no crashes.
   - Variation: edge files — empty, garbage HTML, open-tag-only,
     unclosed tag, comment-only. All match PHP's verdicts.

2. **Slice nodes by span.** Goal: slicing the source at `nodeSpan` yields
   that node's source text. `parseExpression` on each slice must equal the
   stripped node.
   - Ordinary path: 72,626 expressions over the 297-file corpus.
     Result: 0 disagreements, on every file.
   - Variation: fragments (`1+2`, `new C()->m()`, `match`, `f(a: 1)`,
     `strlen(...)`, `$x |> strlen(...)`, clone-with, `goto end;`,
     `list()`, heredoc/nowdoc, shebang, `__halt_compiler()`, postfix
     torture). All consistent with PHP.

3. **Query and transform.** Goal: the fold/query API finds and rewrites
   every occurrence a user would expect.
   - Ordinary path: `queryStmt allVariables` on `rename.php` found every
     occurrence through the body, heredoc, interpolated string, shell
     backtick, and closure `use`-clause; parameters, `catch`, and
     `foreach` variables were untouched.
   - Variation: `transformExpr`/`transformStmt` renaming `a→b` is
     behavior-identical: `php -n` output of the original and the rewritten
     file diff cleanly.

4. **Declaration validation.** Goal: the parser's declaration-local rules
   match PHP's compile-time fatals. Probed with `php -n -l` as ground
   truth: modifiers, hooks, enum/trait/interface/anonymous-class
   combinations, abstract members, promoted properties, typed constants,
   and param positions.
   - Result: one new family — abstract members in non-abstract classes,
     enums, and anonymous classes — was accepted where PHP 8.5 rejects it
     (confirmed finding 1). The fix was verified by comparing the 6-fixture
     loop before/after, the full suite (736 tests), the oracle group (75
     tests against 8.5, including the 4 new mutations), and re-running the
     297-file corpus and 72,626-slice sweep.

## Confirmed findings

### 1. Abstract members in non-abstract classes, enums and anonymous classes are accepted

Direction: false accept.

| Case | `php -n -l` (8.5) | Library |
| ---- | ------------------ | ------- |
| `<?php class C { abstract public function m(): void; }` | 255, "Class C declares abstract method m() and must therefore be declared abstract" | `PARSE_OK` |
| `<?php enum E { case A; abstract public function m(): void; }` | 255, "Enum method E::m() must not be abstract" | `PARSE_OK` |
| `<?php $x = new class { abstract public function m(): void; };` | 255, "Anonymous class method m() must not be abstract" | `PARSE_OK` |
| `<?php class C { abstract public int $x { get; } }` | 255, "Class C contains 1 abstract method … (C::$x::get)" | `PARSE_OK` |
| `<?php abstract class C { abstract public function m(): void; }` | 0, OK | `PARSE_OK` |
| `<?php trait T { abstract public function m(): void; }` | 0, OK | `PARSE_OK` |

The existing `StatementSpec` even asserted
`<?php class C { abstract public function f(); }` as parses-ok, so the gap
was enshrined in the expected behavior.

Diagnosis, per `/diagnosing-bugs`: a 6-fixture compare loop went red before
the fix (`OVERALL: FAIL`) and green after (`PASS`). The three hypotheses
(the `abstract` flag never reaches the AST; the modifier parser drops it;
validation never consults class abstractness) were resolved by an AST
probe: `classAbstract`, `methodAbstract` and `propAbstract` all flow
correctly, but `checkMember` had no branch consulting class/enum/anonymous
abstractness — validation read an incomplete context (`ClassContext` carried
name + readonly flag, not abstract).

Fix: `ClassContext` now carries the abstract flag and `checkMember` rejects
abstract methods/properties in non-abstract classes, abstract
methods/properties in anonymous classes, and abstract methods in enums,
with PHP-shaped diagnostics (`Abstract method f() must be declared in an
abstract class`, `Enum method E::f() must not be abstract`,
`Anonymous class method f() must not be abstract`, `Abstract property must
be declared in an abstract class`). Traits remain accepted, matching PHP.
A regression `testCase` in `StatementSpec` moves the previously wrong
parses-ok case to the rejects list and pins the valid controls; four oracle
mutations (`abstract-method-in-non-abstract-class`,
`abstract-property-in-non-abstract-class`, `abstract-method-in-enum`,
`abstract-method-in-anonymous-class`, stance `Caught`) pin the fix
differentially — reverting any check makes its stance fail.

Verification: 736 tests pass with `--pattern "!/PHP interpreter oracle/"`;
then the oracle group, 75 tests, all passed against PHP 8.5 (8.2–8.4
sub-groups skipped locally, CI covers the matrix). The 297-file corpus and
the 72,626-slice sweep re-ran clean post-fix (identical totals).

### 2. README quick-start snippets do not compile as written

The first snippet passes a `String` literal to `parseProgram :: Text ->
...` and passes the `Text` returned by `prettyPrint` to `putStrLn`, which
takes a `String`. The `nodeSpan` snippet has the same `Text`-literal
problem. Neither reflects the actual API. Fixed by adding
`{-# LANGUAGE OverloadedStrings #-}` and `Data.Text.IO.putStrLn`; the
fixed snippets were compiled and run to verify the printed output.

## Rejected candidates

- **Span-slice `DIFF` for `${d}` (in `strings.php`).** Slicing the `ExprVar`
  for `"${d}"` yields `${d}`; parsing `${d}` standalone gives
  `ExprVar (DynamicVar (ExprConstFetch "d"))`, while inside a string it is
  `SimpleVar "d"`. The slice parser is context-sensitive by design: two
  legitimately different ASTs for the same text in different contexts. Not
  a bug.
- **Duplicate-member and `final private const` acceptance** (`dup-member.php`,
  `final-private-const.php`). Both match pinned `KnownFalseAccept`
  divergences (`duplicate-property`, `final-private-class-constant`) in
  `Test.Gen.PHPMutation`, each with a justification in the table; the
  `PinnedStance` tests confirm the stance still holds. Known by design —
  not new findings, no duplicate issues filed.

## Unresolved

Two open architecture issues reproduce exactly as their tickets describe.
Both fixes belong to their tickets (each has an agreed design contract) and
are deliberately not taken here.

- [#337](https://github.com/jonbaldie/php-parser/issues/337) — nested
  `namespace`/`use`/`const` accepted: `<?php function f(): void { namespace
  X; }` and the `use`/`const` variants parse. Root cause confirmed: the
  "first statement" script-structure guard never fires inside a body;
  statement parsing passes the three declarations into `parseMixedBody`
  unguarded. Top-level misplaced `namespace` is still correctly rejected.
- [#406](https://github.com/jonbaldie/php-parser/issues/406) —
  `mapAnnotation` / arbitrary-annotation printing silently drops comments:
  `<?php // hello\n$x = 1; // world` keeps both comments when printing the
  original and none after `mapAnnotation (const ())`. Confirmed the
  `OVERLAPPABLE` catch-all in `Language.PHP.Pretty` returns `[]` for any
  non-`Annotated` annotation. `stripAnnotations` dropping comments alone is
  by design.

## Usability observations

- **None outstanding.** Both snippet fixes landed in this pass; the earlier
  diagnostic-shape complaint from 2026-10-24 ("errors point at the keyword
  with a truncated token") was re-probed for the new rejections: the three
  new diagnostics name the offending method/property and the rule.
- Error messages for the new rejections (e.g. `Abstract method f() must be
  declared in an abstract class`) are positioned on the member and use the
  same `forbidden` form as the existing declaration-validation errors, so
  `errorCustom` substring checks work uniformly.

## Limitations

- The oracle is PHP 8.5.4 only (8.2–8.4 skipped locally; CI covers the
  matrix). Around 210 s of oracle-group time per run.
- Return-value consistency with the declared return type (`return 1;` in a
  `void` function, `yield` in `void`) is not checked. That is a semantic
  rule needing whole-body analysis, not declaration-local validation, and
  is distinguished in `docs/testing/php-oracle.md`'s contract tiers. It is
  noted for completeness; it is not a parser finding and was not filed,
  because the parser contract does not claim semantic-type checks.
- The probes lean on `php -n -l` acceptance, plus round-trip/no-false-reject
  coverage from the 297-file corpus. Heredoc-escape edge cases (known
  divergence #236) are covered only by the `cabal test` oracle, not
  re-probed here.
- No lockfile exists, so the install was not frozen. Dependency versions
  are whatever the Cabal solver chose from the shared store at build time.
