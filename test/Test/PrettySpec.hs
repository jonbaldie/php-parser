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

  , testCase "Pretty print anonymous class attributes after new (Issue #46)" $ do
      let attrs = [AttributeGroup () [Attribute () (QualifiedName () NameUnqualified ["Attribute"]) []]]
          expr = ExprNewAnonClass () attrs (ClassModifier False False False) [] Nothing [] []
          printed = prettyPrintExpr expr
      assertBool "Attributes follow new" ("new #[Attribute]" `T.isInfixOf` printed)
      case parseExpression "anon.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Pretty output round-trips" expr (stripAnnotations reparsed)

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

  , testCase "Pretty print interpolated string with escaped double quotes (Issue #111)" $ do
      case parseExpression "test.php" "\"hello \\\"world\\\" $x\"" of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertEqual "Escapes double quotes in output" "\"hello \\\"world\\\" {$x}\"" printed

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

  , testCase "prettyPrintExpr on post-increment of cast parenthesizes the cast (Issue #23)" $ do
      let expr = ExprUnary () OpPostInc
                   (ExprCast () CastInt (ExprVar () (SimpleVar () (VarName () "x"))))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around the cast" "((int)$x)++" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving precedence" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on post-decrement of cast parenthesizes the cast (Issue #23)" $ do
      let expr = ExprUnary () OpPostDec
                   (ExprCast () CastString (ExprVar () (SimpleVar () (VarName () "s"))))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around the cast" "((string)$s)--" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving precedence" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on postfix increment of prefix constructs parenthesizes the operand (Issue #23)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
      assertEqual "Post-increment of clone has parens" "(clone $x)++"
        (prettyPrintExpr (ExprUnary () OpPostInc (ExprClone () varX Nothing)))
      assertEqual "Post-increment of pre-increment has parens" "(++$x)++"
        (prettyPrintExpr (ExprUnary () OpPostInc (ExprUnary () OpPreInc varX)))
      assertEqual "Post-increment of throw has parens" "(throw $e)++"
        (prettyPrintExpr (ExprUnary () OpPostInc
          (ExprThrow () (ExprVar () (SimpleVar () (VarName () "e"))))))

  , testCase "prettyPrintExpr on plain variable post-increment stays unparenthesized (Issue #23)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
      assertEqual "Variable post-increment has no parens" "$x++"
        (prettyPrintExpr (ExprUnary () OpPostInc varX))
      assertEqual "Pre-increment of cast has no parens" "++(int)$x"
        (prettyPrintExpr (ExprUnary () OpPreInc (ExprCast () CastInt varX)))

  , testCase "prettyPrintExpr on post-increment of include parenthesizes the include (Issue #67)" $ do
      let expr = ExprUnary () OpPostInc
                   (ExprInclude () IncInclude (ExprLit () (LitString () "f.php" "'f.php'")))
      let printed = prettyPrintExpr expr
      assertEqual "Prints with parens around the include" "(include 'f.php')++" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed ->
          assertEqual "Round-trips preserving precedence" expr (stripAnnotations reparsed)

  , testCase "prettyPrintExpr on postfix operators of include constructs parenthesizes the operand (Issue #67)" $ do
      let lit = ExprLit () (LitString () "f.php" "'f.php'")
          inc t = ExprInclude () t lit
          prop = MemberIdent (Ident () "prop")
          idx = Just (ExprLit () (LitInt () 0 "0"))
      assertEqual "Post-increment of include has parens" "(include 'f.php')++"
        (prettyPrintExpr (ExprUnary () OpPostInc (inc IncInclude)))
      assertEqual "Post-decrement of include has parens" "(include 'f.php')--"
        (prettyPrintExpr (ExprUnary () OpPostDec (inc IncInclude)))
      assertEqual "Post-increment of include_once has parens" "(include_once 'f.php')++"
        (prettyPrintExpr (ExprUnary () OpPostInc (inc IncIncludeOnce)))
      assertEqual "Post-increment of require has parens" "(require 'f.php')++"
        (prettyPrintExpr (ExprUnary () OpPostInc (inc IncRequire)))
      assertEqual "Post-increment of require_once has parens" "(require_once 'f.php')++"
        (prettyPrintExpr (ExprUnary () OpPostInc (inc IncRequireOnce)))
      assertEqual "Property fetch on include has parens" "(include 'f.php')->prop"
        (prettyPrintExpr (ExprPropertyFetch () (inc IncInclude) prop))
      assertEqual "Array access on include has parens" "(include 'f.php')[0]"
        (prettyPrintExpr (ExprArrayAccess () (inc IncInclude) idx))
      assertEqual "Call on include has parens" "(include 'f.php')()"
        (prettyPrintExpr (ExprCall () (inc IncInclude) (ArgsList [])))

  , testCase "prettyPrintExpr on top-level include prints without extraneous outer parentheses (Issue #67)" $ do
      let lit = ExprLit () (LitString () "f.php" "'f.php'")
      assertEqual "Top-level include has no outer parens" "include 'f.php'"
        (prettyPrintExpr (ExprInclude () IncInclude lit))

  , testCase "prettyPrintExpr preserves print precedence and postfix contexts (Issue #123)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          litOne = ExprLit () (LitInt () 1 "1")
          printX = ExprPrint () varX
          contexts =
            [ ("top-level print", printX, "print $x")
            , ("binary operand", ExprBinary () OpAdd printX litOne, "((print $x) + 1)")
            , ("property fetch", ExprPropertyFetch () printX (MemberIdent (Ident () "prop")), "(print $x)->prop")
            , ("call", ExprCall () printX (ArgsList []), "(print $x)()")
            ]
      mapM_ (\(name, expr, expected) -> do
        let printed = prettyPrintExpr expr
        assertEqual name expected printed
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": " ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ " round-trip") (stripAnnotations expr) (stripAnnotations reparsed)
        ) contexts

  , testCase "prettyPrintExpr renders exit and die in nested contexts (Issue #124)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          litOne = ExprLit () (LitInt () 1 "1")
          bareExit = ExprExit () ExitExit Nothing
          dieStatus = ExprExit () ExitDie (Just varX)
          contexts =
            [ ("bare exit", bareExit, "exit")
            , ("bare die", ExprExit () ExitDie Nothing, "die")
            , ("exit with status", ExprExit () ExitExit (Just litOne), "exit(1)")
            , ("die with status", dieStatus, "die($x)")
            , ("binary operand", ExprBinary () OpLogicalOr varX dieStatus, "($x or die($x))")
            , ("assignment operand", ExprAssign () Nothing varX bareExit, "$x = exit")
            , ("call base", ExprCall () bareExit (ArgsList []), "(exit)()")
            ]
      mapM_ (\(name, expr, expected) -> do
        let printed = prettyPrintExpr expr
        assertEqual name expected printed
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": " ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ " round-trip") (stripAnnotations expr) (stripAnnotations reparsed)
        ) contexts

  , testCase "prettyPrintExpr on operator operands parenthesizes yield, yield from, arrow function, throw, and include (Issue #112)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          litOne = ExprLit () (LitInt () 1 "1")
          litTwo = ExprLit () (LitInt () 2 "2")
          yieldExpr = ExprYield () Nothing (Just varX)
          yieldFromExpr = ExprYieldFrom () varX
          arrowExpr = ExprArrowFunction () [] False False [] Nothing varX
          throwExpr = ExprThrow () varX
          incExpr = ExprInclude () IncInclude (ExprLit () (LitString () "a.php" "'a.php'"))
          constructs = [ ("yield", yieldExpr, "yield $x")
                       , ("yield from", yieldFromExpr, "yield from $x")
                       , ("arrow function", arrowExpr, "fn () => $x")
                       , ("throw", throwExpr, "throw $x")
                       , ("include", incExpr, "include 'a.php'")
                       ]
      mapM_ (\(name, construct, raw) -> do
        -- Binary operands
        let binLhs = ExprBinary () OpAdd construct litOne
        let binRhs = ExprBinary () OpAdd litOne construct
        assertEqual (name ++ " binary lhs has parens") (T.pack ("((" ++ raw ++ ") + 1)")) (prettyPrintExpr binLhs)
        assertEqual (name ++ " binary rhs has parens") (T.pack ("(1 + (" ++ raw ++ "))")) (prettyPrintExpr binRhs)

        -- Unary operand
        let unExpr = ExprUnary () OpBoolNot construct
        assertEqual (name ++ " unary operand has parens") (T.pack ("!(" ++ raw ++ ")")) (prettyPrintExpr unExpr)

        -- Ternary operands
        let ternCond = ExprTernary () construct (Just litOne) litTwo
        let ternThen = ExprTernary () litOne (Just construct) litTwo
        let ternElse = ExprTernary () litOne (Just litTwo) construct
        assertEqual (name ++ " ternary cond has parens") (T.pack ("((" ++ raw ++ ") ? 1 : 2)")) (prettyPrintExpr ternCond)
        assertEqual (name ++ " ternary then has parens") (T.pack ("(1 ? (" ++ raw ++ ") : 2)")) (prettyPrintExpr ternThen)
        assertEqual (name ++ " ternary else has parens") (T.pack ("(1 ? 2 : (" ++ raw ++ "))")) (prettyPrintExpr ternElse)

        -- Null coalesce operands
        let coalLhs = ExprNullCoalesce () construct litTwo
        let coalRhs = ExprNullCoalesce () litOne construct
        assertEqual (name ++ " coalesce lhs has parens") (T.pack ("((" ++ raw ++ ") ?? 2)")) (prettyPrintExpr coalLhs)
        assertEqual (name ++ " coalesce rhs has parens") (T.pack ("(1 ?? (" ++ raw ++ "))")) (prettyPrintExpr coalRhs)
        ) constructs

  , testCase "prettyPrintExpr and prettyPrintStmt on top-level and statement-level constructs do not over-parenthesize (Issue #112)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          yieldExpr = ExprYield () Nothing (Just varX)
          yieldFromExpr = ExprYieldFrom () varX
          arrowExpr = ExprArrowFunction () [] False False [] Nothing varX
          throwExpr = ExprThrow () varX
          incExpr = ExprInclude () IncInclude (ExprLit () (LitString () "a.php" "'a.php'"))
          constructs = [ ("yield", yieldExpr, "yield $x")
                       , ("yield from", yieldFromExpr, "yield from $x")
                       , ("arrow function", arrowExpr, "fn () => $x")
                       , ("throw", throwExpr, "throw $x")
                       , ("include", incExpr, "include 'a.php'")
                       ]
      mapM_ (\(name, construct, raw) -> do
        assertEqual (name ++ " top-level expr has no outer parens") (T.pack raw) (prettyPrintExpr construct)
        assertEqual (name ++ " statement level has no outer parens") (T.pack (raw ++ ";")) (prettyPrintStmt (StmtExpr () construct))
        ) constructs

  , testCase "prettyPrintExpr renders list(...) destructuring constructs (Issue #125)" $ do
      let varA = ExprVar () (SimpleVar () (VarName () "a"))
          varB = ExprVar () (SimpleVar () (VarName () "b"))
          varC = ExprVar () (SimpleVar () (VarName () "c"))
          varArr = ExprVar () (SimpleVar () (VarName () "arr"))
          litKey = ExprLit () (LitString () "k" "'k'")
          itemA = ArrayItem () Nothing varA False
          itemB = ArrayItem () Nothing varB False
          itemC = ArrayItem () Nothing varC False
          itemEmpty = ArrayItemEmpty ()
          itemKeyed = ArrayItem () (Just litKey) varA False
          contexts =
            [ ("empty list", ExprList () [], "list()")
            , ("simple list", ExprList () [itemA, itemB], "list($a, $b)")
            , ("keyed list", ExprList () [itemKeyed], "list('k' => $a)")
            , ("nested list", ExprList () [itemA, ArrayItem () Nothing (ExprList () [itemB, itemC]) False], "list($a, list($b, $c))")
            , ("omitted slot", ExprList () [itemA, itemEmpty, itemB], "list($a, , $b)")
            , ("leading omitted slot", ExprList () [itemEmpty, itemB], "list(, $b)")
            , ("assignment target", ExprAssign () Nothing (ExprList () [itemA, itemB]) varArr, "list($a, $b) = $arr")
            ]
      mapM_ (\(name, expr, expected) -> do
        let printed = prettyPrintExpr expr
        assertEqual name expected printed
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": " ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ " round-trip") (stripAnnotations expr) (stripAnnotations reparsed)
        ) contexts
  ]
