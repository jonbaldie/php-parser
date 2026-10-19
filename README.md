# php-parser

A purely functional, idiomatic Haskell library for parsing modern PHP source code (PHP 8.2, 8.3, 8.4, and 8.5).

## Features

- Modern PHP Grammar Coverage (PHP 8.2 – 8.5):
  - PHP 8.2: Disjunctive Normal Form (DNF) types (`(A&B)|C`), `readonly` classes, trait constants, standalone scalar types (`null`, `true`, `false`).
  - PHP 8.3: Typed class constants, dynamic class constant fetch (`Class::{$var}`), anonymous `readonly` classes.
  - PHP 8.4: Property hooks (`get` / `set` expressions and blocks), asymmetric visibility (`public private(set)`), member dereferencing on instantiation without parentheses (`new Service()->process()`, `new Config()::KEY`).
  - PHP 8.5: Pipe operator (`|>`), clone-with syntax (`clone($obj, [...])` and `clone($obj, withProperties: [...])`), static asymmetric visibility (`public private(set) static`).
- Complete Language Constructs: First-class callables (`strlen(...)`), match expressions, constructor property promotion, nullsafe chains (`?->`), non-capturing catch, throw expressions, array unpacking with spread (`...$arr`), Heredoc/Nowdoc with indentation stripping, numeric literals with underscores, inline HTML chunks, short echo tags (`<?=`), and comment/PHPDoc trivia preservation.
- Parametric Abstract Syntax Tree: AST nodes (`Program a`, `Stmt a`, `Expr a`, `Type a`) are parameterized over annotation types (e.g. source spans and trivia).
- Algebraic Pretty Printer: A Wadler/Leijen-style algebraic pretty printer guaranteeing round-trip invariance (`parse . prettyPrint`).
- Catamorphisms and Recursion Schemes: Bottom-up transformations (`transformExpr`), queries (`queryExpr`, `queryStmt`), variable extraction (`allVariables`), and annotation mapping/stripping (`stripAnnotations`).
- Structured Error Diagnostics: Precise source spans (`line`, `column`, `offset`), expected tokens, and context reporting without runtime exceptions.

## Quick Start

```haskell
import Language.PHP
import Data.Text (Text)

main :: IO ()
main = do
  let phpCode = "<?php echo 'Hello, World!'; ?>"
  case parseProgram "example.php" phpCode of
    Left err -> putStrLn ("Parse error: " ++ show (formatParseError err))
    Right ast -> do
      putStrLn "Successfully parsed AST:"
      print (stripAnnotations ast)
      putStrLn "\nPretty printed output:"
      putStrLn (prettyPrint ast)
```

## Node annotations and source spans

`annotation :: HasAnnotation f => f a -> a` reads the root metadata of any
annotated node, including statements, class members, types and parameters.
Wrapper nodes such as `ClassMember` delegate to their contained declaration.
`CallArgs`, `HookBody` and `StringPart` have no root annotation and deliberately
have no instance. `getAnnotation :: Expr a -> a` remains a compatibility alias.

For results from the public parser, use
`nodeSpan :: HasAnnotation f => f (Annotated Span) -> Span`:

```haskell
case parseStatement "example.php" "echo 1;" of
  Left err -> print err
  Right stmt -> print (nodeSpan stmt)
```

`HasSpan` remains available, with `getSpan = nodeSpan` instances for parsed
`Expr`, `Stmt`, `ClassMember`, `Type` and `Param` nodes. For all other annotated
node families, use `nodeSpan` directly.

## Running Tests

The test suite uses [Tasty](https://hackage.haskell.org/package/tasty) with HUnit and QuickCheck:

```bash
cabal test
```

One group, the differential PHP oracle, shells out to real `php -l` binaries. It
skips itself when no interpreter is on `PATH` and says so in its own test names,
so `cabal test` is green on a machine with no PHP installed. See
[docs/testing/php-oracle.md](docs/testing/php-oracle.md) to run it for real.

## License

This project is licensed under the [MIT License](LICENSE).
