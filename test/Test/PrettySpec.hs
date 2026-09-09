{-# LANGUAGE OverloadedStrings #-}

module Test.PrettySpec (prettyTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Language.PHP

prettyTests :: TestTree
prettyTests = testGroup "Pretty Printer Specifications"
  [ testCase "Pretty print simple class with property and method" $ do
      let src = "<?php\n\nclass Greeter {\n    public string $greeting = \"Hello\";\n    public function greet(string $name): string {\n        return ($this->greeting . $name);\n    }\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains class Greeter" ("class Greeter" `T.isInfixOf` printed)
          assertBool "Printed contains function greet" ("function greet" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.4 property hooks" $ do
      let src = "<?php\n\nclass User {\n    public string $name {\n        get => $this->name;\n        set(string $val) {\n            $this->name = $val;\n        }\n    }\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains get =>" ("get =>" `T.isInfixOf` printed)
          assertBool "Printed contains set(" ("set(" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.4 asymmetric visibility" $ do
      let src = "<?php\n\nclass Status {\n    public private(set) string $title;\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains private(set)" ("private(set)" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.5 pipe operator" $ do
      let exprSrc = "(($x |> 'trim') |> 'strtolower')"
      case parseExpression "test.php" exprSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertBool "Printed contains |>" ("|>" `T.isInfixOf` printed)

  , testCase "prettyPrintExpr on ExprCall with property fetch parenthesizes callee" $ do
      let expr = ExprCall () (ExprPropertyFetch () (ExprVar () (SimpleVar () (VarName () "obj"))) (MemberIdent (Ident () "prop"))) (ArgsList [])
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around property fetch" "($obj->prop)()" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> do
          let reparsedStripped = stripAnnotations reparsed
          assertEqual "Round-trips back to ExprCall rather than ExprMethodCall" expr reparsedStripped

  , testCase "prettyPrintExpr on ExprCall with nullsafe property fetch parenthesizes callee" $ do
      let expr = ExprCall () (ExprNullsafePropertyFetch () (ExprVar () (SimpleVar () (VarName () "obj"))) (MemberIdent (Ident () "prop"))) (ArgsList [])
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around nullsafe property fetch" "($obj?->prop)()" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> do
          let reparsedStripped = stripAnnotations reparsed
          assertEqual "Round-trips back to ExprCall rather than ExprNullsafeMethodCall" expr reparsedStripped

  , testCase "prettyPrintExpr on ExprCall with static property fetch parenthesizes callee" $ do
      let expr = ExprCall () (ExprStaticPropertyFetch () (ClassTargetName (QualifiedName () NameUnqualified ["Foo"])) (VarName () "prop")) (ArgsList [])
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around static property fetch" "(Foo::$prop)()" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> do
          let reparsedStripped = stripAnnotations reparsed
          assertEqual "Round-trips back to ExprCall rather than static method call" expr reparsedStripped

  , testCase "prettyPrintExpr on ExprCall with class const fetch parenthesizes callee" $ do
      let expr = ExprCall () (ExprClassConstFetch () (ClassTargetName (QualifiedName () NameUnqualified ["Foo"])) (ConstNameIdent (Ident () "CONST"))) (ArgsList [])
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around class const fetch" "(Foo::CONST)()" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> do
          let reparsedStripped = stripAnnotations reparsed
          assertEqual "Round-trips back to ExprCall rather than ExprStaticCall" expr reparsedStripped

  , testCase "prettyPrintExpr on ExprAssign operand in ExprBinary parenthesizes the assignment (Issue #12)" $ do
      let assign = ExprAssign () Nothing (ExprVar () (SimpleVar () (VarName () "y"))) (ExprLit () (LitInt () 1 "1"))
      let expr = ExprBinary () OpAdd assign (ExprLit () (LitInt () 2 "2"))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around the assignment" "(($y = 1) + 2)" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving precedence" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprAssign operand in ExprTernary condition parenthesizes the assignment (Issue #12)" $ do
      let assign = ExprAssign () Nothing (ExprVar () (SimpleVar () (VarName () "x"))) (ExprConstFetch () (QualifiedName () NameUnqualified ["foo"]))
      let expr = ExprTernary () assign (Just (ExprLit () (LitInt () 1 "1"))) (ExprLit () (LitInt () 2 "2"))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around the assignment" "(($x = foo) ? 1 : 2)" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving precedence" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on top-level ExprAssign prints without extraneous outer parentheses (Issue #12)" $ do
      let expr = ExprAssign () Nothing (ExprVar () (SimpleVar () (VarName () "x")))
                   (ExprBinary () OpAdd (ExprLit () (LitInt () 1 "1")) (ExprLit () (LitInt () 2 "2")))
      assertEqual "Top-level assignment has no outer parens" "$x = (1 + 2)" (prettyPrintExpr expr)
      assertEqual "Statement-level assignment has no outer parens" "$x = (1 + 2);"
        (prettyPrintStmt (StmtExpr () expr))

  , testCase "prettyPrintExpr preserves unparenthesized chained property and array accesses" $ do
      let chainProp = ExprPropertyFetch () (ExprPropertyFetch () (ExprVar () (SimpleVar () (VarName () "obj"))) (MemberIdent (Ident () "a"))) (MemberIdent (Ident () "b"))
      assertEqual "Property chain has no parens" "$obj->a->b" (prettyPrintExpr chainProp)
      let methodOnProp = ExprMethodCall () (ExprPropertyFetch () (ExprVar () (SimpleVar () (VarName () "obj"))) (MemberIdent (Ident () "a"))) (MemberIdent (Ident () "foo")) (ArgsList [])
      assertEqual "Method call on property has no parens" "$obj->a->foo()" (prettyPrintExpr methodOnProp)
      let arrayOnProp = ExprArrayAccess () (ExprPropertyFetch () (ExprVar () (SimpleVar () (VarName () "obj"))) (MemberIdent (Ident () "a"))) (Just (ExprLit () (LitInt () 0 "0")))
      assertEqual "Array access on property has no parens" "$obj->a[0]" (prettyPrintExpr arrayOnProp)

  , testCase "prettyQualifiedName on NameRelative does not duplicate namespace prefix (Issue #18)" $ do
      case parseExpression "test.php" "namespace\\Foo" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Prints a single namespace prefix" "namespace\\Foo" printed
          case parseExpression "test.php" printed of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed ->
              assertEqual "Round-trips" (stripAnnotations ast) (stripAnnotations reparsed)

  , testCase "prettyQualifiedName on nested NameRelative does not duplicate namespace prefix (Issue #18)" $ do
      case parseExpression "test.php" "namespace\\Foo\\Bar" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Prints a single namespace prefix" "namespace\\Foo\\Bar" printed
          case parseExpression "test.php" printed of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed ->
              assertEqual "Round-trips" (stripAnnotations ast) (stripAnnotations reparsed)

  , testCase "prettyQualifiedName on NameRelative whose first identifier is namespace (Issue #18)" $ do
      case parseExpression "test.php" "namespace\\namespace" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Prints prefix plus identifier namespace" "namespace\\namespace" printed
          case parseExpression "test.php" printed of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed ->
              assertEqual "Round-trips" (stripAnnotations ast) (stripAnnotations reparsed)

  , testCase "prettyQualifiedName on constructed NameRelative without namespace in parts (Issue #18)" $ do
      let expr = ExprConstFetch () (QualifiedName () NameRelative ["Foo"])
      assertEqual "Prints a single namespace prefix" "namespace\\Foo" (prettyPrintExpr expr)
      case parseExpression "test.php" (prettyPrintExpr expr) of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips" expr (stripAnnotations reparsed)

  , testCase "prettyQualifiedName on NameUnqualified is unaffected (Issue #18)" $ do
      case parseExpression "test.php" "Foo" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Unqualified name is unchanged" "Foo" printed
          case parseExpression "test.php" printed of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed ->
              assertEqual "Round-trips" (stripAnnotations ast) (stripAnnotations reparsed)

  , testCase "prettyQualifiedName on NameFullyQualified is unaffected (Issue #18)" $ do
      case parseExpression "test.php" "\\Foo\\Bar" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Fully-qualified name is unchanged" "\\Foo\\Bar" printed
          case parseExpression "test.php" printed of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed ->
              assertEqual "Round-trips" (stripAnnotations ast) (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprYield property fetch parenthesizes the yield (Issue #22)" $ do
      let expr = ExprPropertyFetch () (ExprYield () Nothing (Just (ExprVar () (SimpleVar () (VarName () "x"))))) (MemberIdent (Ident () "prop"))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around yield" "(yield $x)->prop" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving postfix structure" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprArrowFunction call parenthesizes the callee (Issue #22)" $ do
      let expr = ExprCall () (ExprArrowFunction () [] False False [] Nothing (ExprVar () (SimpleVar () (VarName () "x")))) (ArgsList [])
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around arrow function" "(fn () => $x)()" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving call structure" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprThrow property fetch parenthesizes the throw (Issue #22)" $ do
      let expr = ExprPropertyFetch () (ExprThrow () (ExprVar () (SimpleVar () (VarName () "e")))) (MemberIdent (Ident () "prop"))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around throw" "(throw $e)->prop" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving postfix structure" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprYield array access parenthesizes the yield (Issue #22)" $ do
      let expr = ExprArrayAccess () (ExprYield () Nothing (Just (ExprVar () (SimpleVar () (VarName () "x"))))) (Just (ExprLit () (LitInt () 0 "0")))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around yield" "(yield $x)[0]" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving array access structure" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on ExprYieldFrom property fetch parenthesizes the yield from (Issue #22)" $ do
      let expr = ExprPropertyFetch () (ExprYieldFrom () (ExprVar () (SimpleVar () (VarName () "x")))) (MemberIdent (Ident () "prop"))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around yield from" "(yield from $x)->prop" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving postfix structure" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on top-level yield, arrow function, and throw prints without extraneous outer parentheses (Issue #22)" $ do
      assertEqual "Top-level yield has no outer parens" "yield $x"
        (prettyPrintExpr (ExprYield () Nothing (Just (ExprVar () (SimpleVar () (VarName () "x"))))))
      assertEqual "Top-level arrow function has no outer parens" "fn () => $x"
        (prettyPrintExpr (ExprArrowFunction () [] False False [] Nothing (ExprVar () (SimpleVar () (VarName () "x")))))
      assertEqual "Top-level throw has no outer parens" "throw $e"
        (prettyPrintExpr (ExprThrow () (ExprVar () (SimpleVar () (VarName () "e")))))
  ]
