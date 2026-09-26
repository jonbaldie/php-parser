# 2026-09-26 — Index, format-and-run, fragment repair

Scope: three consumer journeys through the public `Language.PHP` API — index a
module by its spans, pretty-print a script and run both forms, repair a broken
fragment from the diagnostic. Library under test: `0.1.8.0` (commit `8220a2a`).

## Setup

- GHC 9.12.1, cabal 3.16.1.0. Driver: `exploratory-evidence/2026-09-26-consumer-journeys/`
  (local, not committed), calling only `parseProgram` / `parseStatement` /
  `parseExpression` and the lazy twins, plus `prettyPrint`, `queryStmt`,
  `allVariables`, `transformStmt`.
- Oracle: `php -n -l` and, where a version accepts the source, execution with
  `display_errors=stderr`. Binaries verified, not trusted by name:
  8.2.33, 8.3.33, 8.4.25, 8.5.9.
- Starting state: clean `main` at `8220a2a` apart from the uncommitted
  2026-09-21 report, which this pass did not depend on.

## Journeys

### A. Index a module

Goal: list every declared name in a billing module (interface, trait const,
backed enum, class, promoted properties, function) and point a span at each
name. Expectation: `parseProgram` succeeds, each name is in the AST, and the
span slice contains that name (`README`: precise source spans).

Ordinary path succeeded. Reopening the same source produced an equal stripped
AST. Imports `Money`, `format_money`, and `SCALE` were present in the AST. A
function name after a UTF-8 comment (`café`) still sliced back to `hello`.

Variation: `<?php function total( { }` is rejected with
`invoice.php:1:23: error: unexpected "{"`. Replacing it with the original
module parses again.

### B. Format a script and run it

Goal: pretty-print a script (match, interpolation, closure `use`, alternative
`if`/`endif`) and have PHP 8.5 print the same stdout. Then rename `$name` to
`$who` with `transformStmt` and run the reprint. Expectation: round-trip
invariance, and a rename that the fold docs say reaches occurrences including
interpolated ones.

Ordinary path and the rename both matched (stdout and exit). Variations of
this journey are where the confirmed printer bugs showed up.

### C. Repair a fragment

Goal: parse an expression and a statement with no `<?php`, get a diagnostic
for `1 +`, correct it to `1 + 2`, and see the lazy entry points agree.
Expectation: the Haddocks — statement parsing does not require an open tag;
`formatParseError` includes the filename and a real span.

`1 + 2 * 3`, `echo 1;`, and the three lazy entry points succeeded. `1 +`
failed at column 4 with `unexpected end of input` and named `snip.php`. The
correction parsed.

## Confirmed findings

Filed as #304–#309. Each minimized reproducer failed the same way on three
consecutive runs. PHP verdicts were unanimous across the versions that claim
the construct, except #308, which is a version split by design.

| Issue | Direction | Construct |
| ----- | --------- | --------- |
| [#304](https://github.com/jonbaldie/php-parser/issues/304) | **false reject** | backtick execution operator (`` `echo hi` ``) |
| [#305](https://github.com/jonbaldie/php-parser/issues/305) | false accept | `else if` in alternative syntax (`else if (1):`) |
| [#306](https://github.com/jonbaldie/php-parser/issues/306) | false accept | `namespace` after a non-declare statement, inline HTML, or a close tag |
| [#307](https://github.com/jonbaldie/php-parser/issues/307) | false accept | bracketed and unbracketed namespaces in one file |
| [#308](https://github.com/jonbaldie/php-parser/issues/308) | printer | `(new C())->n()` reprinted as `new C()->n()`, which 8.2/8.3 reject |
| [#309](https://github.com/jonbaldie/php-parser/issues/309) | printer / misparse | `clone($c, with: [...])` reprinted as positional clone-with; PHP fatals, the reprint prints `2` |

#304 is the false reject: it hides otherwise-valid files. #308 and #309 are
invisible to stripped-AST equality. #308 still executes on 8.5; it fails the
per-version printer contract in `docs/testing/php-oracle.md`. #309 changes a
fatal into a success. PHP 8.5.9's parameter is `withProperties`, not `with`
(manual and `ReflectionFunction('clone')`). Positional clone-with and
`withProperties:` already matched under execution. #131 asked for `with:`;
released PHP does not have that parameter.

## Rejected candidates

- Promoted-property spans looked short (`$i` vs `id`) only because the check
  sliced `length(name)` from the span start. The span is `$id` (columns
  20–23). Declaration spans for classes, methods, and the UTF-8 case matched.
- `return ;` is valid PHP. A first "broken file" that used it was a bad
  fixture, not a missed diagnostic. The real syntax error above was diagnosed.
- `parseStatement` rejects `<?php echo 1;` (`unexpected "<?p"`). The Haddock
  says opening tags are not required, and `echo 1;` parses. A pasted open tag
  is not skipped. Observation, not filed: the parenthetical does not promise
  that a present tag is consumed.
- `<?xml ...?>` is rejected by both sides under `php -n` (short_open_tag on).
  With `short_open_tag=0`, PHP accepts it. The library has no such switch.
- Closed regressions still hold, both sides reject: short echo at EOF (#276),
  form feed (#279), nested ternary (#280), invalid `\u{}` (#281), `---1`
  (#282), assignment to a concat (#283), a second `<?php` (#271), mixed
  heredoc indentation (#262).
- Exercised once without a diverge: shebang, BOM, halt-compiler payload,
  pipe chains (`5 |> $add |> $dbl`, `1 + 2 |> $id`), first-class callables,
  nullsafe, interpolation including `"$a[k]"` and `"$a[-1]"`, concat
  precedence, heredoc escapes, nowdoc, by-ref, variable-variables, dynamic
  class const, trait `insteadof`/`as` (execution matched; the 8.2 reprint
  failure on that case is #308), goto, yield, readonly anonymous class, DNF,
  match, property hooks, asymmetric static visibility, group use, two
  namespaces of one form, `declare` then `namespace`.

## Usability

- A `Right` from `parseStatement` on a snippet that still contains `<?php` is
  not available: the error token is the three characters `<?p`, which does
  not say "open tags are not accepted here".
- #309 is the same class of hole as #232 and #236: stripped-AST equality holds
  while the printed program means something else. Consumers using that
  equality as a formatter check will not see it.

## Scope and limitations

- Three journeys completed (index, format-and-run, fragment repair), each with
  an ordinary path and a correction or rename, plus a variation corpus of
  about 60 snippets compared with all four interpreters.
- No library source was modified. Evidence, the driver, and replay files stay
  in `exploratory-evidence/2026-09-26-consumer-journeys/`.
- Execution comparison used stdout and exit status. Two fatals with empty
  stdout would not have been distinguished; the confirmed execution bug
  (#309) differed on both.
- `<?xml` under `short_open_tag=0` was not treated as a library bug. A
  consumer who needs that php.ini mode has no API for it.
