# Changelog

## 0.1.3.0

* Reject duplicate declaration modifiers on classes, properties, methods, class constants, and promoted parameters (e.g. `final final class C`, `static static int $x`, `public public function`), and repeated access-type modifiers (`public private`), matching PHP's "Multiple ... modifiers are not allowed" diagnostics (#92).

* Reject `try` statements with neither a `catch` clause nor a `finally` block, matching PHP (#91).

* Reject enum backing types other than `int` or `string` (case-insensitive), matching PHP, instead of accepting arbitrary types (#90).

* Reject class members that PHP forbids in their enclosing declaration: properties in enums, enum cases outside enums, bare (unhooked) properties and trait use in interfaces, matching PHP 8.4; hooked interface properties remain allowed (#89).

* Fix `prettyPrintExpr` to re-escape literal dollars and backslashes in interpolated-string text parts, so text like `literal \$name` from a decoded escape does not reparse as variable interpolation (#88).

* Fix float literal parsing so a mantissa ending in the decimal point followed by an exponent (`1.e2`, `1.e-2`) parses to PHP's value instead of `0.0`, and keep the original source spelling in `LitFloat`'s raw text instead of normalizing it to `1.e+2` (#86).
* Reject incomplete base-prefixed integer literals (`0x`, `0b`, `0o` with no digits), matching PHP, instead of accepting them as `0` (#85).
* Fix `prettyPrintExpr` to parenthesize `include`/`require` expressions used as postfix bases, so `(include 'f.php')++` and similar round-trip instead of mutating precedence (#67).
* Fix `parseCloseTag` to consume the whole newline following `?>`, including CRLF and lone CR, so Windows-style newlines do not leak into inline HTML (#53).
* Fix expression traversal and transformation helpers (`allExprs`, `queryExpr`, `transformExpr`, `queryStmt`, and `transformStmt`) to visit expressions stored in attributes (#52).
* Parse double-quoted string interpolation into `LitInterpolated` with `StringPart` segments instead of a single literal `LitString` (#51).
* Reject visibility modifiers on property hooks, matching PHP 8.4 (#50).
* Fix `parsePropertyHook` to retain `final` on property hooks and `prettyHook` to print it (#49).
* Fix comment trivia association so comments attach to the node they precede and survive pretty printing (#48).
* Fix successful AST spans to report input offsets instead of always setting `posOffset` to 0 (#47).
* Fix `parseExpression` to parse attributes on anonymous class expressions after `new`, and keep `prettyPrintExpr` attributes in valid `new #[...] class` order (#46).
* Fix `parseExpression` to bind exponentiation tighter than prefix operators and casts, so `-2 ** 2` parses as `-(2 ** 2)`, and make `prettyPrintExpr` keep parentheses around such operands of `**` (#45).
* Fix `blockOrDocComment` to treat `/**/` as an empty block comment instead of consuming the closer as a doc marker (#44).
* Fix `parseClone` to parse `clone` as a prefix operator over postfix expressions, so `clone ($obj)->prop`, `clone ($obj)[0]`, and `clone ($obj)->method()` parse (#28).
* Fix `parseArgWith` named argument label matcher to not consume the first colon of `::`, so static calls, static property fetches, and class constants parse in argument lists (#27, #68).

## 0.1.2.0

* Fix `parseClosure` and `parseArrowFunction` to support attribute groups (#34, #38).
* Fix `parseVar` to support variable-variable syntax (`$$var` and `$$$var`) (#35, #37).
* Fix `parseAttributeGroup` to accept trailing commas in attribute lists (#32, #40).
* Fix `parseGroupUse` to accept trailing commas in grouped use imports (#33, #39).
* Fix `foldStmt` to retain statements inside functions, methods, and property hooks (#31, #41).
* Fix variable property fetch and method call syntax in `parseMemberName` (#30, #42).
* Fix parsing of `__halt_compiler` statements (#29, #43).
* Fix `parsePropertyModifier` to accept `final` and `abstract` property modifiers (#5, #54).
* Fix `parsePropertyHook` to accept bodyless property hooks (#6, #55).
* Fix `parseProgram` to preserve leading inline HTML beginning with `#` or whitespace (#7, #56).
* Fix statement terminators before PHP close tags (#9, #57).
* Support `var` as a public visibility modifier in property declarations (#11, #58).
* Fix `prettyPrintExpr` to retain parentheses around assignment expressions in composite expressions (#12, #59).

## 0.1.1.0

* Fix `parseNamespace` backtracking on relative namespace statements (#24, #26).
* Fix `parseIf` backtracking when matching `else if` in `parseElseIf` (#17, #25).
* Fix `prettyPrintExpr` on `ExprCall` with property fetch to parenthesize callee (#10, #16).
* Fix dynamic class instantiation using variables in `parseNew` (#8, #15).
* Fix heredoc and nowdoc lexer to require identifier boundary on closing tag (#4, #14).
* Fix constant modifier ordering to allow `final` before visibility (#3, #13).
* Configure GitHub Actions CI workflow for build, test, and documentation matrix across GHC 9.8, 9.10, and 9.12 on Ubuntu and macOS (#1).
* Configure agent skills, issue tracker, and triage labels (#2).

## 0.1.0.0

* Initial release of `php-parser`: Idiomatic parser for PHP 8.2 through 8.5.
