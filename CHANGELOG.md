# Changelog

## 0.1.2.0

* Fix `parseExpression` to parse attributes on anonymous class expressions after `new`, and keep `prettyPrintExpr` attributes in valid `new #[...] class` order (#46).
* Fix `parseExpression` to bind exponentiation tighter than prefix operators and casts, so `-2 ** 2` parses as `-(2 ** 2)`, and make `prettyPrintExpr` keep parentheses around such operands of `**` (#45).
* Fix `blockOrDocComment` to treat `/**/` as an empty block comment instead of consuming the closer as a doc marker (#44).
* Fix `parseClone` to parse `clone` as a prefix operator over postfix expressions, so `clone ($obj)->prop`, `clone ($obj)[0]`, and `clone ($obj)->method()` parse (#28).
* Fix `parseArgWith` named argument label matcher to not consume the first colon of `::`, so static calls, static property fetches, and class constants parse in argument lists (#27, #68).
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
