# 2026-10-03 — Modern grammar, member modifiers, typed constants, match parenthesisation, and lexical boundaries

Scope: differential testing of modern grammar features across PHP 8.2–8.5 — property modifier combinations (`var` interactions, asymmetric visibility restrictions, abstract property and hook body invariants), typed constant constraints (`callable`, `void`, `never`), match expression pretty-printer parenthesisation and postfix dereferencing, and lexical concatenation token boundaries. Library under test: `0.1.12.0` (commit `a4927eb`).

## Setup

- GHC 9.12.1, Cabal 3.16.1.0. Driver and case corpora: `exploratory-evidence/2026-10-03-afk-run/` (local, not committed).
- Oracle: `php -n -l` and execution across all four supported versions:
  PHP 8.2.33, PHP 8.3.33, PHP 8.4.25, PHP 8.5.9.
  Binaries verified by version discovery.
- Baseline test suite ran clean before this pass: all 578 tests passed.

## Scope and Journeys

1. **Modern class member declarations & modifier combinations**:
   Tested combinations of visibility, asymmetric visibility (`private(set)`, `protected(set)`), legacy `var`, `readonly`, `static`, `final`, and `abstract` on property declarations and hook blocks across PHP 8.2–8.5.
2. **Type system constraints across constants and properties**:
   Tested type syntax and semantic restrictions on class, interface, trait, and enum constants introduced in PHP 8.3 (RFC: Typed class constants), specifically validating forbidden type positions (`callable`, `void`, `never`).
3. **Match expressions, parenthesisation invariants, and lexical token boundaries**:
   Tested round-trip pretty-printing and parser acceptance for postfix operations (method calls, property access, invocations, first-class callables, array indexing) on `match` expressions, as well as lexical token boundary interactions between operators (such as binary concatenation `.`) and immediate numeric literals.

## Confirmed findings

Filed as #341–#349. Each reproducer reproduced stably across all four interpreters.

| Issue | Direction | Construct |
| ----- | --------- | --------- |
| [#341](https://github.com/jonbaldie/php-parser/issues/341) | false accept | `var` combined with property modifiers (`var readonly`, `var static`, `var private(set)`, etc.) |
| [#342](https://github.com/jonbaldie/php-parser/issues/342) | false accept | typed class, interface, enum, and trait constants with forbidden types (`callable`, `void`, `never`) |
| [#343](https://github.com/jonbaldie/php-parser/issues/343) | ~~false accept~~ not a bug | static properties with asymmetric visibility (`public static private(set) int $x;`): PHP 8.5 accepts them, see below |
| [#344](https://github.com/jonbaldie/php-parser/issues/344) | false accept | untyped properties with asymmetric visibility (`public private(set) $x;`) |
| [#345](https://github.com/jonbaldie/php-parser/issues/345) | **printer** | dropping parentheses around dereferenced match expressions (`(match ($x) { ... })->m()`, `(match ($x) { ... })()`, `(match ($x) { ... })[0]`) |
| [#346](https://github.com/jonbaldie/php-parser/issues/346) | false accept | unparenthesized postfix dereferencing of match expressions (`match ($x) { ... }->m()`) |
| [#347](https://github.com/jonbaldie/php-parser/issues/347) | false accept | binary concatenation operator immediately followed by a digit parsed as concat with integer, accepting invalid syntax and corrupting AST |
| [#348](https://github.com/jonbaldie/php-parser/issues/348) | false accept | abstract properties without property hooks (`abstract public int $x;`) |
| [#349](https://github.com/jonbaldie/php-parser/issues/349) | false accept | non-abstract properties with abstract hooks (`public int $x { get; }`) and abstract properties without abstract hooks (`abstract public int $x { get => 1; }`) |

### Highlighted Findings

- **#341 (False accept)**: In PHP grammar, `var` is legacy PHP 4 syntax for declaring a property. It is grammatically forbidden from combining with any modifier (`readonly`, `static`, `final`, `abstract`, or asymmetric visibility `private(set)` / `protected(set)`). `php-parser` accepts all combinations in both prefix and suffix order.
- **#342 (False accept)**: In PHP 8.3+, typed constants in classes, interfaces, traits, and enums cannot have types `callable`, `void`, or `never`. While `disallowedPropertyType` enforces this for properties and promoted parameters, constant declarations were unvalidated.
- **#344 (False accept)**: PHP 8.4's asymmetric visibility feature requires an explicit type. `php-parser` accepted untyped asymmetric properties.
- **#343 (Not a bug, retracted)**: This pass reported asymmetric visibility on static properties as rejected by PHP 8.4 and 8.5. Re-measured with `php -n -l`, only 8.2 and 8.3 ("Multiple access type modifiers are not allowed") and 8.4.25 ("Static property may not have asymmetric visibility") reject it. 8.5.9 accepts it, because PHP 8.5 allows asymmetric visibility on static properties. `parseProgram` parses the union of 8.2-8.5, so accepting it is correct. `test/Test/PHP85Spec.hs` now pins acceptance in every modifier order.
- **#345 & #346 (Pretty-printer regression & false accept)**: PHP requires `match` expressions to be enclosed in parentheses before postfix dereferencing (method call `->m()`, property access `->prop`, function invocation `()`, first-class callable `(...)`, array index `[0]`). `php-parser`'s pretty-printer omitted `ExprMatch` from `needsPostfixParens` and `needsCallParens`, stripping parentheses and emitting broken PHP syntax. In the parser, `parsePostfix` directly permitted postfix operators on `parseMatch` without parentheses.
- **#347 (False accept / AST corruption)**: In PHP's lexical analyzer, any dot (`.`) immediately followed by a digit is scanned as a floating-point literal (`T_DNUMBER`), not a concatenation operator. Code like `$a = "hello" .1;` or `$x = 10..20;` is rejected by all PHP interpreters with a syntax error. `php-parser`'s `parseConcat` lookahead did not check for following digits, consuming `.` as `OpConcat` and parsing the subsequent digits as an integer (e.g. `$x = ("hello" . 1);`), accepting invalid code and mutating the literal value.
- **#348 & #349 (False accept)**: In PHP 8.4+, only hooked properties may be declared `abstract`. Furthermore, a non-abstract property cannot declare abstract hooks (`get;`), and an abstract property must define at least one abstract hook. `php-parser` allowed unhooked abstract properties, concrete properties with abstract hooks, and abstract properties with exclusively concrete hook bodies.

## Non-findings and rejections with evidence

- Top-level typed constants (`const int X = 1;`) are forbidden in PHP and correctly rejected by `php-parser` with a parse error.
- First-class callables with arrow functions `(fn () => 1)(...)` and closures `(function () {})(...)` correctly retain parentheses in the pretty-printer.
- Interface property hooks with bodies (`interface I { public int $x { get => 1; } }`) are rejected by both PHP and `php-parser`.
- Final property hooks in interfaces (`interface I { public int $x { final get; } }`) are rejected by `php-parser` with `"Property hook cannot be both abstract and final"`.
- Asymmetric visibility ordering rules (`checkVisibilityOrdering`) correctly reject set-visibility that is less restrictive than get-visibility (e.g. `private public(set)`).
- Member constant initializers containing `new` continue to be correctly rejected across classes, interfaces, traits, and enums (#328 holds).

## Evidence

Drivers, probe corpora, and minimized counterexample reports are kept locally under `exploratory-evidence/2026-10-03-afk-run/` (not committed).
