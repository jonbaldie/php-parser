# Changelog

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
