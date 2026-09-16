# Exploratory Testing Report: Declarations, Clauses & Exception Handling

Date: 2026-09-16  
Starting revision: `81453a9` (library `0.1.5.0`)  
Toolchain: GHC 9.12.1, Cabal 3.16.1.0, macOS aarch64  
Independent syntax oracle: PHP CLI 8.4.1 (`php -l`)  
Existing test suite status: `cabal test` — 366 tests passed cleanly.  
Evidence directory: `exploratory-evidence/2026-09-16-round2/`

Drivers:
- `Replay.hs`: Dedicated reproducer verification driver exercising all 7 confirmed findings against both `Language.PHP` and `php -l`.
- `test_asym.hs` & `test_asym_promoted.hs`: Asymmetric visibility strength validations on property and constructor-promoted parameter declarations.
- `test_catch_rename.hs`: Catch clause variable querying (`allVariables`) and renaming consistency (`transformStmt`).
- `test_switch_default.hs`: Multi-default switch statement parser checks.
- `test_match_default.hs`: Multi-default match expression arm parser checks.
- `test_enum_cases.hs`: Backed vs pure enum case value constraints.
- `test_duplicate_param.hs`: Parameter uniqueness across functions, methods, and closures.
- `test_interface_modifiers.hs`: Interface member visibility and modifier constraints.

---

## Journeys Exercised

### A. Class, Enum, and Interface Declaration Constraints

Goal: A developer or code generation tool defines modern object-oriented structures (enums, interfaces, and classes with asymmetric visibility). The parser must accept valid declarations while rejecting malformed visibility hierarchies, illegal interface modifiers, and mismatched enum case values as PHP does at parse/compile time.

- **Ordinary path**: Standard classes with asymmetric visibility (e.g. `public private(set) string $bar;`), backed enums with values (`enum S: string { case Draft = 'draft'; }`), pure enums without values (`enum S { case Draft; }`), and public interface methods and constants parse and round-trip cleanly.
- **Variations attempted**:
  1. *Asymmetric visibility strength*: Properties and constructor-promoted parameters where read visibility is weaker than set visibility (`private public(set)`, `protected public(set)`, `private protected(set)`). PHP CLI rejects these with fatal parse errors. The parser accepted them without validation (confirmed as [#193](https://github.com/jonbaldie/php-parser/issues/193)).
  2. *Enum case value constraints*: A pure enum declaring a case with a value (`enum Status { case Draft = 1; }`) and a backed enum declaring a case without a value (`enum Status: string { case Draft; }`). PHP CLI rejects both at parse time. The parser accepted both without checking against the enum declaration context (confirmed as [#197](https://github.com/jonbaldie/php-parser/issues/197)).
  3. *Interface member modifiers*: Interfaces declaring `private` or `protected` constants, or declaring `private`, `protected`, `final`, or `abstract` methods. PHP CLI rejects each with a fatal parse error. The parser accepted all of them without enforcing interface modifier constraints (confirmed as [#199](https://github.com/jonbaldie/php-parser/issues/199)).

### B. Control Flow Clause Exclusivity

Goal: A linter or static analysis tool validates `switch` statements and `match` expressions. The parser must ensure syntactic clause cardinality rules are respected, specifically that at most one `default` clause or arm is specified.

- **Ordinary path**: Switch statements with one default clause (in both brace syntax and alternate colon syntax) and match expressions with one default arm parse and round-trip accurately.
- **Variations attempted**:
  1. *Multiple switch defaults*: `switch ($x) { default: break; default: break; }` and colon syntax `switch ($x): default: break; default: break; endswitch;`. PHP CLI rejects these at compile/parse time (`Switch statements may only contain one default clause`). The parser accepted multiple defaults (confirmed as [#195](https://github.com/jonbaldie/php-parser/issues/195)).
  2. *Multiple match default arms*: `$a = match ($x) { default => 1, default => 2 };`. PHP CLI rejects this at compile/parse time (`Match expressions may only contain one default arm`). The parser accepted multiple default arms (confirmed as [#196](https://github.com/jonbaldie/php-parser/issues/196)).

### C. Function and Method Parameter Uniqueness

Goal: A tool parses function, method, closure, and arrow function declarations. Parameter lists must have unique parameter names within their scope.

- **Ordinary path**: Parameter lists with distinct names, optional types, defaults, variadics at the end, and by-reference modifiers parse and round-trip cleanly.
- **Variations attempted**:
  1. *Duplicate parameter names*: `function foo($a, $a) {}`, `class Foo { function bar($x, $x) {} }`, `$f = function ($x, $x) {};`, and `$f = fn($x, $x) => 1;`. PHP CLI rejects these with fatal errors (`Redefinition of parameter $a`). While `parseParamList` already checks for non-final variadic parameters, it does not check for duplicate names and accepted all four cases (confirmed as [#198](https://github.com/jonbaldie/php-parser/issues/198)).

### D. Exception Handling and AST Traversal Consistency

Goal: A refactoring engine searches for bound variables using `queryStmt allVariables` and renames variables across statements using `transformStmt`.

- **Ordinary path**: Traversing try blocks, finally blocks, and expressions within statements properly visits and transforms variables.
- **Variations attempted**:
  1. *Catch clause variable binding*: `try { risky(); } catch (Exception $e) { log($e); }`. `catchVar` is stored as `Maybe (VarName a)` on `CatchClause`. `queryStmt allVariables` completely misses `$e` when the body does not use it.
  2. *Catch variable renaming*: When renaming `$e` to `$ex` via `transformStmt`, the references in the body are transformed (`log($ex)`), but `catchVar` is left untouched (`catch (Exception $e)`), generating syntactically invalid code with an unbound variable in the catch body (confirmed as [#194](https://github.com/jonbaldie/php-parser/issues/194)).

---

## Confirmed Findings (Filed in Issue Tracker)

All confirmed bugs were reduced to minimal reproducers, verified against PHP CLI 8.4.1 (`php -l`), replayed via `Replay.hs`, and filed as GitHub issues with labels `bug,ready-for-agent`:

| Issue | Title | Summary |
| ----- | ----- | ------- |
| [#193](https://github.com/jonbaldie/php-parser/issues/193) | Parser accepts property declarations where read visibility is weaker than set visibility (`private public(set)`) | Explicit read visibility weaker than write visibility is accepted on standard and promoted properties. Fatal parse error in PHP 8.4+. |
| [#194](https://github.com/jonbaldie/php-parser/issues/194) | `allVariables` and `transformStmt` omit catch clause variables (`catchVar`) | `queryStmt allVariables` returns `[]` on catch blocks with empty bodies. Variable renamers leave `catch (Exception $e)` unrenamed while renaming the body, producing broken PHP. |
| [#195](https://github.com/jonbaldie/php-parser/issues/195) | Parser accepts multiple default clauses in switch statements | Switch statements containing more than one `default:` clause are accepted. PHP CLI rejects at compile time with a fatal error. |
| [#196](https://github.com/jonbaldie/php-parser/issues/196) | Parser accepts multiple default arms in match expressions | Match expressions containing more than one `default =>` arm are accepted. PHP CLI rejects at compile time with a fatal error. |
| [#197](https://github.com/jonbaldie/php-parser/issues/197) | Parser accepts pure enum cases with values and backed enum cases without values | Pure enums with case values (`case Draft = 1;`) and backed enums without case values (`case Draft;`) are accepted. PHP CLI rejects both at parse time. |
| [#198](https://github.com/jonbaldie/php-parser/issues/198) | Parser accepts duplicate parameter names in parameter lists | Parameter lists with repeated names (`foo($a, $a)`) are accepted. PHP CLI rejects at compile time with `Redefinition of parameter $a`. |
| [#199](https://github.com/jonbaldie/php-parser/issues/199) | Parser accepts non-public constants and non-public, final, or abstract methods in interfaces | Non-public interface constants (`private const`) and illegal method modifiers (`private`, `protected`, `final`, `abstract`) are accepted in interfaces. PHP CLI rejects each. |

---

## Rejected Candidates & Observations (Not Filed)

1. **Non-integer operand on `break` / `continue` (`break $x;`, `break 0;`)**:
   - *Observation*: PHP CLI rejects non-integer break operands with `'break' operator with non-integer operand is no longer supported` and `'break' operator accepts only positive integers`. However, `php-parser`'s AST model defines `StmtBreak a (Maybe (Expr a))` as a generic expression container (historically supporting expressions). While a strict linter could flag dynamic operands, keeping `Expr a` in the AST is a conscious AST modeling choice. Not filed as a parser syntax bug.

2. **Abstract private methods in traits**:
   - *Observation*: PHP permits `abstract private function f();` inside traits (allowing traits to require implementing classes to define a private helper), while forbidding it in classes. `php-parser` allows it in both. The trait behavior was verified as legal PHP.

3. **Interface method bodies**:
   - *Observation*: `interface I { public function f() {} }` was tested and confirmed to be properly rejected by `php-parser` with a parse error (expecting `;` or `:`).

---

## Scope and Limitations

- Four user-critical journeys completed across OOP declarations, clause cardinality, parameter uniqueness, and traversal consistency.
- Environment: Local testing against PHP CLI 8.4.1 (`php -l`), GHC 9.12.1.
- All 366 unit and property tests in `cabal test` continue to pass without regression.
- Test scripts and replay drivers remain preserved in `exploratory-evidence/2026-09-16-round2/`.
