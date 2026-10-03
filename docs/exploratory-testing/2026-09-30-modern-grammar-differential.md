# 2026-09-30 — Modern grammar, parenthesisation, nullsafe write contexts, hooks

Scope: differential testing of modern grammar features across PHP 8.2–8.5 — first-class callables on language constructs, parenthesisation of anonymous invocations and member access, DNF type grammar rules, nullsafe write-context restrictions, static property hooks, and constant initializer constraints. Library under test: `0.1.11.0` (commit `36739bb`).

## Setup

- GHC 9.12.1, Cabal 3.16.1.0. Driver: `exploratory-evidence/2026-09-30-afk-run/` (local, not committed).
- Oracle: `php -n -l` and execution across all four supported versions:
  PHP 8.2.33, PHP 8.3.33, PHP 8.4.25, PHP 8.5.9.
  Binaries verified by version discovery.
- Baseline test suite ran clean before this pass: all 553 tests passed (including the QuickCheck differential oracle matrix across all 4 versions).

## Scope and Journeys

1. **First-class callables & expression entry points**:
   Examined first-class callables (`(...)`) combined with modern language constructs, closures, and language constructs that became functions in PHP 8.4 (`exit`, `die`).
2. **Pretty-printer parenthesisation invariants**:
   Tested whether pretty-printed output preserves validity and execution semantics under real PHP interpreters when dereferencing or invoking anonymous entities (closures, anonymous classes).
3. **Type system combinations (DNF vs Union vs Intersection)**:
   Tested type syntax rules introduced in PHP 8.0–8.2, verifying whether unparenthesized mixing of `&` and `|` is correctly rejected as grammar syntax errors.
4. **Nullsafe operator write contexts**:
   Tested expressions placing `?->` in write contexts (`unset`, by-reference assignment, pre/post increment and decrement).
5. **Property hooks (PHP 8.4+)**:
   Tested combinations of modifiers (static, abstract) on hooked property declarations and hook bodies.
6. **Constant initializers (`new` in initializers)**:
   Tested `new` in initializers across global constants, class constants, interface constants, and enum constants.

## Confirmed findings

Filed as #322–#328. Each reproducer reproduced three consecutive times across all four interpreters.

| Issue | Direction | Construct |
| ----- | --------- | --------- |
| [#322](https://github.com/jonbaldie/php-parser/issues/322) | **false reject** | `exit(...)` and `die(...)` first-class callables in PHP 8.4+ |
| [#323](https://github.com/jonbaldie/php-parser/issues/323) | **printer** | dropping parentheses around anonymous functions in invocations `(function () {})()` and member access `(function () {})->bindTo($obj)` |
| [#324](https://github.com/jonbaldie/php-parser/issues/324) | **printer** | dropping parentheses around anonymous class instantiation `(new class {})->m()`, which PHP 8.2 and 8.3 reject |
| [#325](https://github.com/jonbaldie/php-parser/issues/325) | false accept | unparenthesized DNF types mixing `&` and `|` (`A&B|C`, `A|B&C`) |
| [#326](https://github.com/jonbaldie/php-parser/issues/326) | false accept | nullsafe operator in write contexts (`unset($a?->b)`, `$x = &$a?->b`, `$a?->b++`, `++$a?->b`) |
| [#327](https://github.com/jonbaldie/php-parser/issues/327) | false accept | property hooks on static properties (`public static int $x { get => 1; }`) |
| [#328](https://github.com/jonbaldie/php-parser/issues/328) | false accept | `new` expressions in class, interface, and enum constant initializers |

### Highlighted Findings

- **#322 (False reject)**: PHP 8.4 made `exit` and `die` true functions rather than special syntactic forms, allowing them to be passed as first-class callables (`$f = exit(...);`). `php-parser` unconditionally rejects the placeholder with a parse error, failing to parse valid PHP 8.4+ files.
- **#323 and #324 (Pretty-printer regressions)**:
  - While #308 fixed `ExprNew` parenthesisation before postfix access for PHP 8.2/8.3, `ExprNewAnonClass` (`new class()`) was omitted and still drops required parentheses.
  - Closures (`ExprClosure`) were omitted from `needsCallParens` and `needsPostfixParens`, so IIFE calls `(function () { return 1; })()`, FCCs `(function () {})(...)`, and method calls `(function () {})->bindTo($obj)` are printed without parentheses as `function () {}()`, which all PHP versions reject with a syntax error.
- **#325 (False accept)**: `parseUnionOrIntersection` treated `&` as binding tighter than `|` in operator precedence without requiring the mandatory parentheses `(A&B)|C`. Unparenthesized `A&B|C` and `A|B&C` are syntax errors in PHP.

## Non-findings and rejections with evidence

- Arrow function FCCs `(fn () => 1)(...)` and invocations `(fn () => 1)()` are correctly parenthesized by the printer because `ExprArrowFunction` is present in `needsCallParens`.
- Interface hooks without bodies (`interface I { public int $x { get; set; } }`) and abstract property declarations without hook bodies are correctly accepted.
- Interface hooks with bodies (`interface I { public int $x { get => 1; } }`) are rejected by both PHP and `php-parser` (#189 rule holds).
- Global constant initializers with `new` (`const A = new MyClass();`) are valid PHP 8.1+ and accepted by both sides; only member constants (class/interface/enum) reject `new`.
- `new` without parentheses on named classes in PHP 8.4 (`new C()->m()`) is properly accepted, and its reprint is parenthesized for PHP 8.2/8.3 compatibility (#308 holds).

## Evidence

Drivers, probe corpora, and minimized counterexample reports are kept locally under `exploratory-evidence/2026-09-30-afk-run/` (not committed).
