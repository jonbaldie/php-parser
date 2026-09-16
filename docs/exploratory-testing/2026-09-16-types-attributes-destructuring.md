# Exploratory Testing Report: Types, Attributes & Destructuring Constraints

Date: 2026-09-16  
Starting revision: `6e0b56b` (library `0.1.5.0`)  
Toolchain: GHC 9.12.1, Cabal 3.16.1.0, macOS aarch64  
Independent syntax oracle: PHP CLI 8.4.1 (`php -l`)  
Existing test suite status: `cabal test` — 371 tests passed cleanly.  
Evidence directory: `exploratory-evidence/2026-09-16-round3/`

Drivers:
- `probe3.py`: Automated differential test runner comparing PHP CLI with `Language.PHP` across 64 language construct snippets.
- `Harness.hs`: Streaming parser harness driving `Language.PHP` across test cases.
- `Replay.hs`: Dedicated reproducer verification driver exercising all 7 confirmed findings against both `Language.PHP` and `php -l`.
- `test_prop_types.hs`: Standalone test script validating property type constraints.

---

## Journeys Exercised

### A. Call Arguments and Attribute Syntax Constraints

Goal: A consumer parses modern PHP 8.0+ function calls, method invocations, and attribute groups. The parser must enforce argument ordering rules and attribute parameter constraints.

- **Ordinary path**: Standard calls with positional arguments, trailing named arguments, argument unpacking at the beginning or end of calls, and standard attribute declarations parse and round-trip cleanly.
- **Variations attempted**:
  1. *Positional arguments after named arguments*: `foo(a: 1, $b);`, `$obj->bar(a: 1, $b);`, and `new Foo(a: 1, $b);`. PHP CLI rejects positional arguments following named arguments at compile/parse time (`Cannot use positional argument after named argument`). The parser accepted them and constructed ASTs without error (confirmed as [#202](https://github.com/jonbaldie/php-parser/issues/202)).
  2. *Argument unpacking in attribute argument lists*: `#[Attr(...$args)] class Foo {}` and `#[Attr(...$args)] function bar() {}`. In PHP, argument unpacking is strictly forbidden in attribute argument lists (`Cannot use unpacking in attribute argument list`). The parser accepted unpacking in attributes (confirmed as [#203](https://github.com/jonbaldie/php-parser/issues/203)).

### B. Type System Syntax and Modifier Interactions

Goal: A developer uses PHP 8.0–8.4 types (union types, intersection types, and DNF types). The parser must enforce grammatical rules regarding where type modifiers may appear.

- **Ordinary path**: Standalone types, union types (`int|string`), intersection types (`A&B`), parenthesized DNF types (`(A&B)|C`), and nullable types (`?string`) parse and round-trip accurately.
- **Variations attempted**:
  1. *Nullable shorthand in composite types*: `?int|string`, `int|?string`, and `A&?B`. In PHP, the prefix `?Type` is exclusively a standalone type shorthand; when combined with `|` or `&`, PHP CLI raises a syntax error (`unexpected token "|"` or `unexpected token "?"`). The parser accepted `?` on elements of union and intersection types (confirmed as [#205](https://github.com/jonbaldie/php-parser/issues/205)).
  2. *Illegal property types*: `class Foo { public void $x; public never $y; public callable $z; }`. In PHP, `void` and `never` are return-only types, and `callable` is not permitted on properties. PHP CLI rejects each with a fatal parse/compile error (`Property Foo::$x cannot have type void/never/callable`). The parser accepted all three on property declarations (confirmed as [#207](https://github.com/jonbaldie/php-parser/issues/207)).

### C. Object-Oriented Declarations and Modifier Constraints

Goal: A static analysis tool parses classes and constructor-promoted parameters, ensuring modifier exclusivity and completeness rules are respected.

- **Ordinary path**: Typed readonly properties (`public readonly int $x;`), promoted properties, and abstract methods parse and round-trip cleanly.
- **Variations attempted**:
  1. *Untyped readonly properties*: `class Foo { public readonly $bar; }` and `class Bar { public function __construct(public readonly $baz) {} }`. In PHP 8.1+, readonly properties must declare a type (`Readonly property Foo::$bar must have type`). The parser only enforced this inside readonly classes, omitting validation for individual readonly properties in standard classes and promoted parameters (confirmed as [#204](https://github.com/jonbaldie/php-parser/issues/204)).
  2. *Abstract private methods in classes*: `abstract class C { abstract private function f(); }`. In PHP, abstract methods in classes cannot be private (`Abstract function C::f() cannot be declared private`). The parser accepted `abstract private` methods in class declarations (confirmed as [#208](https://github.com/jonbaldie/php-parser/issues/208)).

### D. Assignment and Destructuring Patterns

Goal: A refactoring engine parses assignments and array/list destructuring statements.

- **Ordinary path**: Standard assignments, array destructuring (`[$a, $b] = $arr;`), and list destructuring (`list($a, $b) = $arr;`) parse and round-trip cleanly.
- **Variations attempted**:
  1. *Empty destructuring assignments*: `[] = $arr;` and `list() = $arr;`. In PHP, destructuring patterns cannot be empty (`Cannot use empty list`). The parser accepted both empty array and list destructuring assignments (confirmed as [#206](https://github.com/jonbaldie/php-parser/issues/206)).

---

## Confirmed Findings (Filed in Issue Tracker)

All confirmed bugs were reduced to minimal reproducers, verified against PHP CLI 8.4.1 (`php -l`), replayed via `Replay.hs`, and filed as GitHub issues with labels `bug,ready-for-agent`:

| Issue | Title | Summary |
| ----- | ----- | ------- |
| [#202](https://github.com/jonbaldie/php-parser/issues/202) | Parser accepts positional arguments after named arguments (`foo(a: 1, $b)`) | Positional arguments following named arguments are accepted in call argument lists. Fatal compile error in PHP 8.0+. |
| [#203](https://github.com/jonbaldie/php-parser/issues/203) | Parser accepts argument unpacking in attribute argument lists (`#[Attr(...$args)]`) | Unpacked arguments (`...$expr`) are accepted inside attribute argument lists. Fatal compile error in PHP 8.0+. |
| [#204](https://github.com/jonbaldie/php-parser/issues/204) | Parser accepts untyped readonly properties in classes and constructor promotion (`public readonly $bar`) | Readonly properties without a declared type are accepted in standard classes and constructor promotion. Fatal compile error in PHP 8.1+. |
| [#205](https://github.com/jonbaldie/php-parser/issues/205) | Parser accepts nullable shorthand `?` in union and intersection types (`?int|string`, `int|?string`, `A&?B`) | `?Type` is accepted as an operand of `|` and `&`. In PHP, nullable shorthand is exclusively a standalone type modifier. |
| [#206](https://github.com/jonbaldie/php-parser/issues/206) | Parser accepts empty array and list destructuring assignments (`[] = $arr`, `list() = $arr`) | Empty destructuring patterns on the left-hand side of assignments are accepted. Fatal compile error in PHP. |
| [#207](https://github.com/jonbaldie/php-parser/issues/207) | Parser accepts `void`, `never`, and `callable` types on property declarations | Properties and promoted parameters typed as `void`, `never`, or `callable` are accepted. Fatal compile error in PHP. |
| [#208](https://github.com/jonbaldie/php-parser/issues/208) | Parser accepts `abstract private` methods in class declarations | `abstract private` method declarations are accepted in classes (where PHP forbids them; only traits permit them). |

---

## Rejected Candidates & Observations (Not Filed)

1. **Multiple visibility modifiers on properties and constants (`public private $x;`, `public private const X = 1;`)**:
   - *Observation*: `php-parser` correctly checks `duplicateModifier "access type"` and rejects duplicate visibility modifiers on properties and constants. Matches PHP CLI.

2. **Abstract static methods in classes (`abstract class Foo { abstract static function bar(); }`)**:
   - *Observation*: PHP CLI accepts abstract static methods in classes without error. `php-parser` also accepts them. Valid PHP.

3. **Readonly modifier on class constants (`class Foo { readonly const X = 1; }`)**:
   - *Observation*: `php-parser` correctly rejects `readonly` on constants as constants only accept visibility and `final`. Matches PHP CLI.

4. **Empty heredocs / nowdocs**:
   - *Observation*: Correctly parsed and round-tripped after ensuring newline requirements of heredoc closing markers are met.

---

## Scope and Limitations

- Four user-critical journeys completed across call argument rules, attribute constraints, type grammar, and destructuring patterns.
- Environment: Local testing against PHP CLI 8.4.1 (`php -l`), GHC 9.12.1.
- All 371 unit and property tests in `cabal test` continue to pass without regression.
- Test scripts and replay drivers remain preserved in `exploratory-evidence/2026-09-16-round3/`.
