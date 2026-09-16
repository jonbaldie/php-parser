# Exploratory Testing Report: Modern PHP Grammar & Traversal Schemes

Date: 2026-09-16  
Starting revision: `7cb2559` (library `0.1.5.0`)  
Toolchain: GHC 9.12.1, Cabal 3.16.1.0, macOS aarch64  
Independent syntax oracle: PHP CLI 8.4.1 (`php -l`)  
Existing test suite status: `cabal test` — 366 tests passed cleanly.  
Evidence directory: `exploratory-evidence/2026-09-16/`

Drivers:
- `probe.py`: Automated differential test runner comparing PHP CLI with `Language.PHP` across 136 language construct snippets.
- `traversal_test.hs`: Catamorphic traversal and query tester exercising `allVariables`, `queryStmt`, and `transformStmt`.
- `test_diagnostics.py`: Linter and interactive parsing driver testing `parseExpression`, `parseStatement`, and diagnostic error spans.
- `Replay.hs`: Dedicated reproducer verification driver exercising all confirmed findings against both `Language.PHP` and `php -l`.

---

## Journeys Exercised

### A. Format a realistic PHP 8.4/8.5 application module & round-trip

Goal: A code formatter / analyzer parses a comprehensive set of modern PHP programs (PHP 8.2 through 8.5) covering property hooks, asymmetric visibility, DNF types, attributes on all targets, constructor property promotion, clone-with, pipe operator, alternative syntax, and template files, pretty-prints them with `prettyPrint`, and reparses them with `parseProgram`. The round-trip must preserve AST equivalence (`stripAnnotations ast == stripAnnotations reparsed`), and PHP CLI (`php -l`) must accept both the input and the printed output.

- **Ordinary path**: Comprehensive modules with attributes, classes, methods, match expressions, property hooks, asymmetric visibility, closures, arrow functions, and nullsafe access parsed cleanly, pretty-printed, and reparsed to an equivalent AST (`stripAnnotations` equal).
- **Variations attempted**:
  1. *Binary strings and heredocs*: Single and double-quoted binary strings (`b'...'`, `b"..."`) parsed and round-tripped cleanly. However, binary-prefixed heredocs and nowdocs (`b<<<EOT`, `b<<<'EOT'`, `B<<<EOT`) were rejected with parse errors despite being accepted by PHP CLI (confirmed as [#186](https://github.com/jonbaldie/php-parser/issues/186)).
  2. *First-class callables and nullsafe operator*: Standard first-class callables (`strlen(...)`, `$obj->method(...)`, `Class::method(...)`) parsed and round-tripped. However, combining nullsafe navigation with first-class callables (`$obj?->method(...)`) was accepted by the parser despite being a fatal compile error in PHP (confirmed as [#187](https://github.com/jonbaldie/php-parser/issues/187)).
  3. *Interface property hooks*: Abstract property hooks in interfaces (`interface Foo { public string $bar { get; set; } }`) parsed and round-tripped. However, hooks with bodies (`get => 'bar'`) and final hooks (`final get;`) were accepted inside interfaces despite being forbidden in PHP (confirmed as [#189](https://github.com/jonbaldie/php-parser/issues/189)).
  4. *Argument unpacking in calls*: Unpacking at start (`foo(...$args)`) and end (`foo($a, ...$args)`) parsed and round-tripped. However, positional arguments after unpacked arguments (`foo($a, ...$args, $b)`) were accepted despite PHP rejecting them at compile time (confirmed as [#190](https://github.com/jonbaldie/php-parser/issues/190)).
  5. *Constructor property promotion*: Standard promotion (`public int $x`, asymmetric `public private(set) string $x`, `readonly`) parsed and round-tripped. However, variadic promoted properties (`public ...$x`) were accepted despite being forbidden in PHP (confirmed as [#191](https://github.com/jonbaldie/php-parser/issues/191)).

### B. Rename variables and query AST via public traversal schemes

Goal: A refactoring engine consumes PHP code, inspects variables with `queryStmt allVariables`, performs syntax-directed renaming via `transformStmt`, pretty-prints the result, and verifies that the output is syntactically valid PHP with all target variable references consistently renamed.

- **Ordinary path**: Renaming variables across assignments, arithmetic expressions, function bodies, closures (`use ($var)` captures), and foreach loops successfully updated all occurrences in lockstep.
- **Variations attempted**:
  1. *Global declarations*: `global $total; $total++;` — `queryStmt allVariables` returned `["total"]`, and renaming `$total` to `$sum` updated both the declaration (`global $sum;`) and the usage (`$sum++`).
  2. *Static variable declarations*: `static $count = 0; $count++;` — `queryStmt allVariables` returned `[]` (missing `$count`), and `transformStmt` failed to rename the declared static variable, leaving `static $count = 0; $tally++;` which breaks code execution (confirmed as [#188](https://github.com/jonbaldie/php-parser/issues/188)).
  3. *Catch clause variables*: `try { ... } catch (Exception $e) { ... }` — when the body does not use the variable, `queryStmt allVariables` returns `[]`. In addition, `transformStmt` does not rewrite `catchVar`, leaving the catch variable unrenamed.
  4. *Expressions in modern constructs*: Traversed expressions inside `ExprClone` (`with:` payload), `ExprPipe`, `ExprMatch`, and `ExprThrow`.

### C. Linter / interactive diagnostics and partial parsing

Goal: An interactive tool or language server parses isolated expressions (`parseExpression`) and statements (`parseStatement`) without `<?php` prefixes, receives structured `ParseError` diagnostics with precise spans on invalid input, and recovers immediately once the error is corrected.

- **Ordinary path**: 16 expression forms (arithmetic, pipe, clone-with, new-dereferencing, match, arrow function, nullsafe, first-class callable, array spread, ternary, throw-expr, yield-expr) and 13 statement forms (echo, return, if, while, for, foreach, try/catch/finally, switch, global, static, declare, unset, throw) parsed cleanly and round-tripped through `prettyPrintExpr` and `prettyPrintStmt`.
- **Variations attempted**:
  1. *Syntactic errors*: Tested missing semicolons, unclosed braces, unclosed parentheses, unclosed strings, double arrow misuse, removed casts (`(real)`), mutually exclusive class modifiers (`final abstract`), `try` without `catch`/`finally`, extra closing braces, and truncated pipe operators. All 10 malformed snippets returned `Left ParseError` without runtime exceptions (no crashes or unhandled Megaparsec exceptions).
  2. *Diagnostic accuracy*: Error positions reported exact line and column numbers matching the syntax error location.

---

## Confirmed Findings (Filed in Issue Tracker)

All confirmed bugs were reduced to minimal reproducers, verified against PHP CLI 8.4.1 (`php -l`), replayed multiple times via `Replay.hs`, and filed as GitHub issues with labels `bug` and `ready-for-agent`:

| Issue | Title | Summary |
| ----- | ----- | ------- |
| [#186](https://github.com/jonbaldie/php-parser/issues/186) | Parser rejects binary-prefixed heredocs and nowdocs (`b<<<EOT`, `b<<<'EOT'`) | `b<<<EOT` and `b<<<'EOT'` fail with `unexpected "<EO"`. Valid in PHP CLI. |
| [#187](https://github.com/jonbaldie/php-parser/issues/187) | Parser accepts nullsafe operator combined with first-class callables (`$obj?->method(...)`) | Parser accepts `?->` with `(...)`. PHP CLI rejects with fatal compile error: `Cannot combine nullsafe operator with Closure creation`. |
| [#188](https://github.com/jonbaldie/php-parser/issues/188) | `allVariables` and `transformStmt` omit static variable declarations (`StmtStatic`) | `queryStmt allVariables` returns `[]` on `static $count = 0;`. Variable renaming leaves declaration unchanged, corrupting functions with undefined variables. |
| [#189](https://github.com/jonbaldie/php-parser/issues/189) | Parser accepts hook bodies and final modifier on interface property hooks | Interface property hooks with bodies (`get => 'bar'`) or `final` modifier are accepted. PHP CLI rejects both at compile time. |
| [#190](https://github.com/jonbaldie/php-parser/issues/190) | Parser accepts positional arguments after argument unpacking (`foo($a, ...$args, $b)`) | Positional argument following `...$args` is accepted. PHP CLI rejects at compile time: `Cannot use positional argument after argument unpacking`. |
| [#191](https://github.com/jonbaldie/php-parser/issues/191) | Parser accepts variadic constructor-promoted properties (`public ...$x`) | Promoted variadic parameter `public ...$x` is accepted. PHP CLI rejects at compile time: `Cannot declare variadic promoted property`. |

---

## Rejected Candidates & Observations (Not Filed)

1. **PHP 8.5 features rejected by PHP 8.4 CLI**:
   - `clone($b, ['x' => 1])` and `clone($b, with: ['x' => 1])`
   - Pipe operator `$x |> 'strlen'`
   - Static asymmetric visibility `public private(set) static string $bar;`
   - *Observation*: PHP CLI 8.4.1 rejects these as expected for a PHP 8.4 runtime; `php-parser` accepts and round-trips them cleanly as part of its documented PHP 8.5 grammar support. Not a bug.

2. **Named arguments after argument unpacking (`foo($a, ...$args, b: 1)`)**:
   - *Observation*: PHP allows named arguments after unpack, but forbids positional arguments. Both PHP and `php-parser` accept named arguments after unpacking.

3. **By-reference constructor promoted properties (`public function __construct(public &$x) {}`)**:
   - *Observation*: Both PHP CLI and `php-parser` accept by-reference promoted properties. Valid PHP.

4. **Abstract methods in concrete classes**:
   - *Observation*: `class Foo { abstract public function bar(); }` is accepted by `php-parser`. In PHP, this is a class completeness validation error rather than a syntax grammar rejection. Not filed.

---

## Scope and Limitations

- Three user-critical journeys completed (Formatting/Round-trip, Traversal/Refactoring, Diagnostics/Partial-parse), each with ordinary paths and diverse variations.
- Environment: Local testing against PHP CLI 8.4.1 (`php -l`), GHC 9.12.1.
- All 366 unit and property tests in `cabal test` continue to pass without regression.
- Intermediate build artifacts (`.o`, `.hi`, and binary executables) were cleaned up. Test scripts and reproduction cases remain preserved in `exploratory-evidence/2026-09-16/`.
