# Changelog

## 0.1.8.0

* Accept a `declare(strict_types=...)` that follows earlier top-level `declare` statements, which PHP allows but the library rejected: `<?php declare(ticks=1); declare(strict_types=1);` and `<?php declare(encoding='UTF-8'); declare(strict_types=1);` failed with "strict_types declaration must be the very first statement in the script" even though the only preceding top-level statements were declares. The strict-types position check now uses the same declare-prologue rule the encoding check already applied (#274), so a close-tag-separated second `strict_types` declaration parses too, while any non-declare statement, inline HTML, a `<?=` echo, or an empty statement still comes too late, and nested or block-mode strict_types declarations stay rejected (#290).

* Reject duplicate `get` or `set` property hooks within one declaration, matching PHP's `Cannot redeclare property hook` compile-time fatal. A declaration may still contain one hook of each kind, and separate properties keep independent hooks (#278).

* Reject a property declared without any modifier, which PHP refuses with `syntax error, unexpected variable "$x", expecting "function"` but the library accepted: `<?php class C { $x = 1; }` and `<?php trait T { $x = 1; }` parsed as properties because the property modifier loop allowed an empty modifier list. A property now needs at least one of `var`, a visibility, asymmetric set visibility, `static`, `readonly`, `final` or `abstract`, so a modifierless typed or hooked property such as `int $x;` is rejected as well, in class, trait, anonymous-class and interface bodies alike (#277).

* Reject a `declare(encoding=...)` that is not the first statement of the script, which PHP refuses with "Encoding declaration pragma must be the very first statement in the script" but the library accepted anywhere: `<?php $x = 1; declare(encoding='UTF-8');` parsed. As in PHP, only earlier top-level `declare` statements may precede it; any other statement, inline HTML, a `<?=` echo, an empty statement, or a close tag that does not itself end a statement comes too early, and a nested encoding declaration is never first. Comments before it remain allowed, and `ticks` and other directives keep their position rules (#274).

* Accept a compile-time constant expression as a `declare(encoding=...)` value: PHP folds a concatenation of literals into a single literal while parsing, so `declare(encoding='UTF-8' . '')` is a compile error the interpreter never reaches, yet the value parser here was literal-only and stopped at the `.`, rejecting the program with a syntax error. The directive value is now a constant expression in the AST (`DeclareDirective`'s value changed from `Literal` to `Expr`, currently a literal or a chain of `.`-concats of literals), literal values keep parsing, and non-constant runtime expressions such as `$x` are still rejected with PHP's "Encoding must be a literal" verdict; `strict_types` keeps its integer-literal rule and `ticks` keeps its literal-only behavior (#275).

* Enforce PHP's `declare(strict_types=...)` rules: the declaration must be the first script statement, use semicolon mode, and have an integer value of `0` or `1`; comments remain allowed before it (#273).

* Match the full PHP open tag case-insensitively, as PHP does. `<?PHP`, `<?pHp` and other casings of `<?php` were rejected because `parseOpenTag` used a case-sensitive string; a mismatch then fell through to the short-open branch, so `<?pHp $x = 1;` was read as a short tag plus the identifier `pHp` and failed at the following variable. Short `<?` and `<?=` guards are unchanged (#272).

* Reject a second PHP open tag (`<?php`, `<?` or `<?=`) met while the parser is already in code mode, which PHP refuses with `syntax error, unexpected token "<"` but the library accepted by silently consuming the tag and losing it from the AST and from `prettyPrint`'s output: `<?php $x = 1; <?php $y = 2;` parsed as two statements with the second tag gone. An open tag is now only ever consumed out of HTML mode -- at the start of a file, or directly after a close tag's inline HTML -- matching PHP's lexer, where code mode ends only at `?>`. A close tag, HTML, open-tag sequence still parses, and the short-open (`<?`) and short-echo (`<?=`) behaviour is otherwise unchanged (#271).

## 0.1.7.0

* Reject a heredoc or nowdoc whose body indentation is of a different whitespace character from its closing marker, which PHP refuses with "Invalid indentation - tabs and spaces cannot be mixed" but the lexer accepted: it compared indentation lengths only, so a space-indented body line stripped cleanly under a tab-indented closer. Body indentation must now agree with the closer character for character as far as the two overlap, including on a line that is whitespace to its end, and a closer whose own indentation mixes tabs and spaces is rejected outright, as in PHP (#262).

* Reject a heredoc or nowdoc whose closing marker is indented deeper than a body line, which PHP refuses with "Invalid body indentation level" but the lexer accepted, silently leaving the under-indented line's own indentation in the body. The parse error names the required level and points at the offending line; a line that is whitespace to its end is exempt, as in PHP (#240).

* End a `//` or `#` comment at a `?>` close tag, as PHP does. `<?php echo "x"; // c ?>tail<?php echo "y";` swallowed the close tag, the inline HTML and every later PHP block into the comment text, so `parseProgram` returned a silently truncated `Program` and `prettyPrint` wrote the truncation back. The close tag is now left for the statement parser, which resumes in HTML mode. A block comment still only ends at `*/`, and a `?` not followed by `>` stays inside the comment (#239).

* Reject a leading-zero integer literal containing an `8` or `9`, such as `08`, `09` or `0128`, which PHP refuses as an invalid numeric literal but the lexer decoded as decimal. Leading-zero floats such as `08.5` and `09e1` are still accepted, as in PHP (#241).

* Parse an interpolating heredoc body like a double-quoted string, so `allVariables`, `allExprs`, `queryExpr` and `transformExpr` reach the variables and expressions embedded in it; renaming a variable no longer leaves its heredoc occurrences behind. A heredoc that embeds expressions is now the new `LitHeredocInterpolated` constructor, holding its label and `StringPart`s; one that embeds none stays a `LitHeredoc`, and a nowdoc stays literal. Heredoc text also no longer treats `\"` as an escape (PHP keeps the backslash), and a blank line just before the closing label is kept (#234).

* Accept a negative number as a simple-interpolation subscript, as in `"$a[-1]"`, which PHP allows but the key parser rejected. `-1` parses to the same unary-minus key as the braced form `"{$a[-1]}"`; a non-canonical number such as `-0` or `-0x1F` is the string key PHP makes of it. `+1`, `-x` and `-$i` stay rejected (#235).

* Keep comments that follow the last statement of a file, which were dropped because trivia only attaches to a following node. A `Program`'s annotation trivia now holds this trailing trivia, and `prettyPrint` re-emits it after the last statement, so `<?php $x = 1; // note` and `<?php /* only a comment */` round-trip (#238).

* Accept the `**=`, `<<=` and `>>=` compound assignments, which the `**`, `<<` and `>>` operator parsers were swallowing before the assignment could see the `=` (#237).

* Fix the pretty-printer re-emitting decoded heredoc bodies, which turned `\t` into a literal tab and `\$notvar` into a live interpolation. `LitHeredoc` now carries a fifth field, the raw body text with escapes intact (as `LitString` does), and the printer emits it (#236).

* Give concatenation (`.`) its own precedence level, below `<<`/`>>` and above comparison operators, matching PHP 8.0's [concatenation precedence RFC](https://wiki.php.net/rfc/concatenation_precedence); `.` no longer binds at the same level as `+`/`-`, so `"a" . 1 + 2` now parses (and prints) as `"a" . (1 + 2)` instead of `("a" . 1) + 2` (#232).

* Reject abstract private methods in class declarations, matching PHP's "Abstract function <class>::<method>() cannot be declared private" compile-time fatal error (#208).

## 0.1.6.0

* Fix `queryStmt`/`queryExpr` double- and triple-counting matches from fully recursive queries (like `allVariables`) composed into a larger traversal, by short-circuiting once a node's query result is non-empty; `allExprs` and `foldExpr`/`foldStmt` are unaffected and continue to visit every node exactly once (#215).

* Reject `void`, `never`, and `callable` types on property declarations and constructor-promoted parameters (including within compound types), matching PHP's "Property cannot have type <type>" compile-time fatal error (#207).

* Reject empty array and list destructuring patterns (`[] = $arr`, `list() = $arr`, `[,] = $arr`, `[[]] = $arr`, `foreach ($arr as [])`), matching PHP's "Cannot use empty list" compile-time fatal error (#206).

* Reject the nullable shorthand (`?Type`) as a member of union or intersection types (`?int|string`, `int|?string`, `A&?B`), matching PHP's parse error; `?` remains valid only on a standalone type (#205).

* Reject untyped `readonly` property declarations and constructor-promoted parameters (`public readonly $prop`), matching PHP's requirement that readonly properties must have an explicit type (#204).

* Reject argument unpacking (`...$args`) in attribute argument lists (`#[Attr(...$args)]`), matching PHP's compile-time fatal error (#203).

* Reject positional arguments following named arguments in call and instantiation argument lists (`foo(a: 1, $b)`), matching PHP's "Cannot use positional argument after named argument" compile-time fatal error (#202).

* Reject non-public constants and non-public, final, or abstract methods in interfaces, matching PHP's compile-time fatal errors (#199).

* Reject duplicate parameter names in parameter lists, matching PHP's compile-time fatal error (#198).

* Reject match expressions containing more than one `default` arm, matching PHP's compile-time fatal error (#196).

* Reject switch statements containing more than one `default` clause, in both brace and alternative (colon) syntax, matching PHP's compile-time fatal error (#195).

* Include catch clause variables (`CatchClause`) in `queryStmt` (e.g. `queryStmt allVariables`) and rewrite declared variable names during `transformStmt`, ensuring exception variables are discovered and refactored consistently alongside local variables, closure uses, and static declarations (#194).

* Reject pure enum cases with values and backed enum cases without values, matching PHP's compile-time fatal errors for non-backed and backed enum cases (#197).

* Reject variadic constructor-promoted properties (`public ...$items`), matching PHP's "Cannot declare variadic promoted property" compile-time fatal error (#191).

* Reject property and promoted parameter declarations where explicit read visibility is weaker than asymmetric set visibility (e.g. `private protected(set)`), matching PHP's "Visibility of property must not be weaker than set visibility" compile-time fatal error (#193).

* Reject positional arguments following an unpacked argument in call and instantiation argument lists (`foo(...$args, $pos)`), matching PHP's "Cannot use positional argument after argument unpacking" compile-time fatal error (#190).

* Reject property hooks with bodies (`get => ...`, `get { ... }`) or the `final` modifier inside interfaces, matching PHP's compile-time fatal errors for abstract hooks that have a body or are declared both abstract and final (#189).

* Include static variable declarations (`StmtStatic`) in `queryStmt` (e.g. `queryStmt allVariables`) and rewrite declared variable names during `transformStmt`, ensuring static variables are discovered and refactored consistently alongside local variables and global declarations (#188).

* Reject combining the nullsafe operator with first-class callables (`$obj?->method(...)`, `$obj?->prop->method(...)`), producing a parse error matching PHP's compile-time fatal error (#187).

* Support binary-prefixed heredocs and nowdocs (`b<<<EOT`, `b<<<'EOT'`, `B<<<EOT`, `B<<<'EOT'`), mirroring PHP's lexer; the prefix must abut `<<<` so a bare `b` remains an identifier (#186).

## 0.1.5.0

* Accept comma-separated expressions in short echo tags (`<?= $a, $b ?>`) inside alternative-syntax bodies, matching top-level short echo tags and `echo` (#153).

* Allow alternative-syntax closers (`endif`, `endwhile`, `endfor`, `endforeach`, `endswitch`, `enddeclare`) to be terminated by a PHP closing tag (`?>`) without a semicolon, matching mixed-template PHP (#152).

* Support typed by-reference parameters in functions, methods, closures, and arrow functions (`function f(int &$a)`), disambiguating intersection types from parameter-level by-reference modifiers (#151).

* Reject removed PHP `(unset)` and `(real)` casts while retaining the supported cast aliases (#146).

* Support `final` modifier on PHP 8.4 constructor-promoted properties (`final private int $x`, `public final int $y`), tracking the modifier via `paramFinal :: !Bool` in `Param`, pretty-printing `final` on promoted parameters, and rejecting `final` on non-promoted parameters (#145).

* Parse dollar-brace string interpolation (`"${var}"`) as `LitInterpolated` containing the variable, matching `"$var"` and `"{$var}"`, instead of leaving it as a literal string; pretty-print escapes a literal `${ident}` in interpolated text so it does not reparse as interpolation (#144).

* Support binary-prefixed string literals (`b'hello'`, `b"hi $name"`, `B'hello'`, `B"hello"`), which PHP treats as an alias for an ordinary string literal; the prefix is kept in `LitString`'s raw text and must abut the quote, so a bare `b` is still an identifier (#143).

* Support mixed-kind grouped `use` imports (`use Foo\{function bar, const BAZ, Qux}`), recording an optional clause-level `useClauseType` on `UseClause` and pretty-printing per-clause `function`/`const` keywords (#140).

* Add support for by-reference array items in square-bracket array literals and `array(...)` constructs (`[&$a]`, `array(&$a)`, `['k' => &$v]`), tracking by-reference items via `itemByRef :: !Bool` in `ArrayItem`, pretty-printing `&` before values, and preserving by-reference elements in AST traversals and round-tripping (#139).

* Add support for by-reference assignment (`$a =& $b`, `$a = &$b`, `$a =& foo()`), represented in the AST as `ExprAssignRef a (Expr a) (Expr a)` and printed as `$a =& $b`; the source must be variable-like, so `$a =& new Foo()` and `$a =& 1` remain syntax errors as in PHP (#138).

* Fix `parseCallArgs` to disambiguate first-class callable syntax `foo(...)` from leading argument unpacking `foo(...$args)`, allowing argument unpacking as the first call argument (#137).

* Fix PHP 8.5 clone-with syntax to accept arbitrary expressions (such as variables, function calls, and array expressions) and optional trailing commas for modification payloads, with or without the named `with:` parameter, representing the payload in AST as `ExprClone a (Expr a) (Maybe (Expr a))` (#131).

* Reject member dereferencing (`->`, `?->`, `::`, `[`) directly on an unparenthesized `new` over a named class (`new Service->process()`), matching PHP 8.4's requirement for call parentheses before dereferencing; anonymous classes (`new class {}->method()`) and `new` with call arguments remain dereferenceable (#130).

* Reject promoted property modifiers (visibility and readonly) on non-constructors and abstract constructors, matching PHP's "Cannot declare promoted property outside a constructor" and "Cannot declare promoted property in an abstract constructor" fatal errors (#129).

* Reject mutually exclusive declaration modifiers: `final abstract` classes and methods, and `static readonly` properties, in either order, matching PHP's fatal errors (#128).

## 0.1.4.0

* Reject duplicate declaration modifiers on classes, properties, methods, class constants, and promoted parameters (e.g. `final final class C`, `static static int $x`, `public public function`), and repeated access-type modifiers (`public private`), matching PHP's "Multiple ... modifiers are not allowed" diagnostics (#92).

* Reject `try` statements with neither a `catch` clause nor a `finally` block, matching PHP (#91).

* Reject enum backing types other than `int` or `string` (case-insensitive), matching PHP, instead of accepting arbitrary types (#90).

* Reject class members that PHP forbids in their enclosing declaration: properties in enums, enum cases outside enums, bare (unhooked) properties and trait use in interfaces, matching PHP 8.4; hooked interface properties remain allowed (#89).

* Fix `prettyPrintExpr` to re-escape literal dollars and backslashes in interpolated-string text parts, so text like `literal \$name` from a decoded escape does not reparse as variable interpolation (#88).

* Fix double-quoted string escape decoding so complete PHP escape sequences (including numeric escapes) decode, unknown escapes keep their backslash, and nowdoc content is preserved raw (#87).
* Fix float literal parsing so a mantissa ending in the decimal point followed by an exponent (`1.e2`, `1.e-2`) parses to PHP's value instead of `0.0`, and keep the original source spelling in `LitFloat`'s raw text instead of normalizing it to `1.e+2` (#86).
* Reject incomplete base-prefixed integer literals (`0x`, `0b`, `0o` with no digits), matching PHP, instead of accepting them as `0` (#85).
* Fix unterminated heredoc and nowdoc bodies looping forever by requiring the closing label before end of input (#84).
* Reject property hooks on properties in readonly contexts, matching PHP 8.4 (#96).
* Fix parsing of semi-reserved enum case names, so keyword-like names such as `New` are accepted (#81).

## 0.1.3.0

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
