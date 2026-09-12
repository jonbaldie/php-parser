{-# LANGUAGE OverloadedStrings #-}

module Test.ExpressionSpec (expressionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (evaluate)
import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Language.PHP

expressionTests :: TestTree
expressionTests = testGroup "Expression Specifications"
  [ testCase "Empty block comment /**/ is skipped before an expression" $ do
      case parseExpression "comment.php" "/**/ 1" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprLit (Annotated _ triv) (LitInt _ 1 _)) ->
          assertEqual "trivia is empty block comment" [CommentBlock ""] triv
        other -> assertFailure ("Expected integer 1, got: " ++ show other)

  , testCase "Successful AST spans report input offsets (Issue #47)" $ do
      case parseExpression "offsets.php" "  $a + $b" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprBinary _ _ left right) -> do
          let startA = spanStart (annValue (getAnnotation left))
              startB = spanStart (annValue (getAnnotation right))
          assertEqual "first variable column" 3 (posColumn startA)
          assertEqual "second variable column" 8 (posColumn startB)
          assertEqual "first variable offset" 2 (posOffset startA)
          assertEqual "second variable offset" 7 (posOffset startB)
        other -> assertFailure ("Expected $a + $b, got: " ++ show other)

  , testCase "Unary minus binds looser than exponentiation (Issue #45)" $ do
      let cases =
            [ ("-2 ** 2", "-(2 ** 2)")
            , ("+2 ** 2", "+(2 ** 2)")
            , ("~2 ** 2", "~(2 ** 2)")
            , ("-2 ** 3 ** 2", "-(2 ** (3 ** 2))")
            ] :: [(Text, String)]
      forM_ cases $ \(src, expected) ->
        case parseExpression "pow.php" src of
          Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
          Right (ExprUnary _ _ (ExprBinary _ OpPow _ _)) -> pure ()
          Right other -> assertFailure (show src ++ ": expected " ++ expected
                                        ++ ", got: " ++ show other)

  , testCase "Parenthesized negative base stays the base of ** (Issue #45)" $ do
      case parseExpression "pow.php" "(-2) ** 2" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprBinary _ OpPow (ExprUnary _ OpUnaryMinus _) _) -> pure ()
        other -> assertFailure ("Expected (-2) ** 2 exponentiation, got: " ++ show other)

  , testCase "Anonymous class expression attributes parse into the AST (Issue #46)" $ do
      case parseExpression "anon.php" "new #[Attribute] class {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprNewAnonClass _ [AttributeGroup _ [Attribute _ (QualifiedName _ NameUnqualified ["Attribute"]) []]] _ _ _ _ _) -> pure ()
        other -> assertFailure ("Expected an attributed anonymous class expression, got: " ++ show other)
      case parseExpression "anon.php" "new #[Attribute] NamedClass()" of
        Left _ -> pure ()
        Right other -> assertFailure ("Expected attributes on named instantiation to be rejected, got: " ++ show other)

  , testCase "Match expression with multiple patterns and default" $ do
      let src = "match ($status) { 200, 201 => 'success', 400 => 'bad request', default => 'unknown' }"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprMatch _ _ arms -> do
            assertEqual "match arm count" 3 (length arms)
            case arms of
              [MatchArm _ [_, _] _, MatchArm _ [_] _, MatchDefault _ _] -> pure ()
              _ -> assertFailure "Unexpected arm structure"
          other -> assertFailure ("Expected ExprMatch, got: " ++ show other)

  , testCase "Match arm condition list with trailing comma (Issue #115)" $ do
      let src = "match ($x) { 1, 2, => 'val' }"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprMatch _ _ [MatchArm _ conds _] ->
            assertEqual "condition count" 2 (length conds)
          other -> assertFailure ("Expected two-condition match arm, got: " ++ show other)
      forM_ [ "match ($x) { 1, 2 => 'val' }"
            , "match ($x) { 1 => 'val' }"
            , "match ($x) { default => 'd' }"
            , "match ($x) { 1, 2, => 'val', 3, => 'three', default => 'd' }"
            ] $ \okSrc ->
        case parseExpression "test.php" okSrc of
          Left err -> assertFailure (show okSrc ++ ": " ++ show (formatParseError err))
          Right _ -> pure ()

  , testCase "First-class callable syntax: strlen(...) and $obj->method(...)" $ do
      let src1 = "strlen(...)"
          src2 = "$this->process(...)"
          src3 = "Config::load(...)"
      case parseExpression "test.php" src1 of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprCall _ _ FirstClassCallable) -> pure ()
        other -> assertFailure ("Expected FirstClassCallable, got: " ++ show other)

      case parseExpression "test.php" src2 of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprMethodCall _ _ _ FirstClassCallable) -> pure ()
        other -> assertFailure ("Expected method FirstClassCallable, got: " ++ show other)

      case parseExpression "test.php" src3 of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprStaticCall _ _ _ FirstClassCallable) -> pure ()
        other -> assertFailure ("Expected static FirstClassCallable, got: " ++ show other)

  , testCase "Named arguments in function calls: foo(name: $val, count: 42)" $ do
      let src = "render(template: 'home.php', cache: false, timeout: 30)"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprCall _ _ (ArgsList args)) -> do
          assertEqual "arg count" 3 (length args)
          let names = [n | Arg _ (Just (Ident _ n)) _ _ <- args]
          assertEqual "argument names" ["template", "cache", "timeout"] names
        other -> assertFailure ("Expected call with named args, got: " ++ show other)

  , testCase "Static member access in arguments: func(Foo::CONST), func(Foo::$prop), func(Foo::method())" $ do
      let assertPositional src = case parseExpression "test.php" src of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprCall _ _ (ArgsList [Arg _ mName _ False])) ->
              assertEqual ("no named label expected in " ++ show src) Nothing mName
            other -> assertFailure ("Expected positional call arg, got: " ++ show other)
      assertPositional "func(Foo::CONST)"
      assertPositional "func(Foo::$prop)"
      assertPositional "func(Foo::method())"
      case parseExpression "test.php" "func(Foo::CONST)" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprCall _ _ (ArgsList [Arg _ _ (ExprClassConstFetch _ _ (ConstNameIdent (Ident _ "CONST"))) False])) -> pure ()
        other -> assertFailure ("Expected class constant fetch arg, got: " ++ show other)

  , testCase "Named argument with static member value: func(name: Foo::CONST)" $ do
      let src = "func(name: Foo::CONST)"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprCall _ _ (ArgsList [Arg _ (Just (Ident _ "name")) (ExprClassConstFetch _ _ (ConstNameIdent (Ident _ "CONST"))) False])) -> pure ()
        other -> assertFailure ("Expected named arg with class constant value, got: " ++ show other)

  , testCase "Nullsafe operator chain: $user?->getProfile()?->name" $ do
      let src = "$user?->getProfile()?->name"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprNullsafePropertyFetch _ (ExprNullsafeMethodCall _ _ (MemberIdent (Ident _ "getProfile")) _) (MemberIdent (Ident _ "name")) ->
            pure ()
          other -> assertFailure ("Expected nullsafe chain, got: " ++ show other)

  , testCase "clone postfix on parenthesized operand: clone ($obj)->prop, [0], ->method() (Issue #28)" $ do
      case parseExpression "test.php" "clone ($obj)->prop" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprPropertyFetch _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberIdent (Ident _ "prop"))) Nothing ->
            pure ()
          other -> assertFailure ("Expected ExprClone of ExprPropertyFetch, got: " ++ show other)

      case parseExpression "test.php" "clone ($obj)[0]" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprArrayAccess _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (Just (ExprLit _ (LitInt _ 0 "0")))) Nothing ->
            pure ()
          other -> assertFailure ("Expected ExprClone of ExprArrayAccess, got: " ++ show other)

      case parseExpression "test.php" "clone ($obj)->method()" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprMethodCall _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberIdent (Ident _ "method")) (ArgsList [])) Nothing ->
            pure ()
          other -> assertFailure ("Expected ExprClone of ExprMethodCall, got: " ++ show other)

      case parseExpression "test.php" "clone($obj, ['key' => 'val'])" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (Just [_]) ->
            pure ()
          other -> assertFailure ("Expected clone-with ExprClone, got: " ++ show other)

  , testCase "Throw expression in coalescing and ternary" $ do
      let src1 = "$value ?? throw new InvalidArgumentException('Missing value')"
          src2 = "$ready ? $go : throw new RuntimeException('Not ready')"
      assertParsesOkExpr src1
      assertParsesOkExpr src2

  , testCase "Array unpacking with spread: [...$first, 'middle', ...$second]" $ do
      let src = "[...$first, 'middle', ...$second]"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprArray _ items) -> do
          assertEqual "item count" 3 (length items)
          assertEqual "unpacks" [True, False, True] (map itemUnpack items)
        other -> assertFailure ("Expected ExprArray, got: " ++ show other)

  , testCase "Numeric literals: underscores, hex, binary, octal, floats" $ do
      let tests =
            [ ("1_000_000", 1000000)
            , ("0x1A_3F", 0x1A3F)
            , ("0b1010_0101", 165)
            , ("0o755", 493)
            , ("0755", 493)
            ]
      mapM_ (\(s, expectedVal) ->
        case parseExpression "test.php" s of
          Right (ExprLit _ (LitInt _ val _)) ->
            assertEqual ("Value for " ++ show s) expectedVal val
          other -> assertFailure ("Failed on " ++ show s ++ ": " ++ show other)) tests

      let floatTests =
            [ ("1e5", 100000.0)
            , ("2E+3", 2000.0)
            , ("1.5e-2", 0.015)
            , ("3.14_15", 3.1415)
            ]
      mapM_ (\(s, expectedVal) ->
        case parseExpression "test.php" s of
          Right (ExprLit _ (LitFloat _ val _)) ->
            assertEqual ("Value for " ++ show s) expectedVal val
          other -> assertFailure ("Failed on float " ++ show s ++ ": " ++ show other)) floatTests

  , testCase "Float literals with a trailing dot and exponent (Issue #86)" $ do
      -- Each case: source, PHP's value, and the raw text the AST must keep
      -- (the original spelling, never a normalized form like "1.e+2").
      let cases =
            [ ("1.e2", 100.0, "1.e2")
            , ("1.e+2", 100.0, "1.e+2")
            , ("1.e-2", 0.01, "1.e-2")
            , ("1.E2", 100.0, "1.E2")
            , ("1_0.e2", 1000.0, "1_0.e2")
            , ("1e2", 100.0, "1e2")
            , ("2E3", 2000.0, "2E3")
            , (".5e2", 50.0, ".5e2")
            , ("1.5", 1.5, "1.5")
            , (".5", 0.5, ".5")
            , ("5.", 5.0, "5.")
            ]
      mapM_ (\(s, expectedVal, expectedRaw) ->
        case parseProgram "test.php" ("<?php return " <> s <> ";") of
          Left err -> assertFailure ("Failed on float " ++ show s ++ ": " ++ show (formatParseError err))
          Right (Program _ [StmtReturn _ (Just (ExprLit _ (LitFloat _ val raw)))] ) -> do
            assertEqual ("Value for " ++ show s) expectedVal val
            assertEqual ("Raw text for " ++ show s) expectedRaw raw
          other -> assertFailure ("Expected float " ++ show s ++ ", got: " ++ show other)) cases

  , testCase "Float literal parser rejects tokens starting with an underscore as float literals (Issue #113)" $ do
      case parseProgram "test.php" "<?php _1e2();" of
        Right (Program _ [StmtExpr _ (ExprCall _ (ExprConstFetch _ (QualifiedName _ NameUnqualified ["_1e2"])) _)]) -> pure ()
        other -> assertFailure ("Expected call to identifier _1e2, got: " ++ show other)

      case parseProgram "test.php" "<?php $x = 1._0;" of
        Left _ -> pure ()
        Right other -> assertFailure ("Expected parse error for 1._0, got: " ++ show other)

      case parseProgram "test.php" "<?php $x = _1.0;" of
        Right (Program _ [StmtExpr _ (ExprAssign _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "x"))) (ExprBinary _ OpConcat (ExprConstFetch _ (QualifiedName _ NameUnqualified ["_1"])) (ExprLit _ (LitInt _ 0 "0"))))]) -> pure ()
        other -> assertFailure ("Expected $x = _1 . 0, got: " ++ show other)

      let validFloats =
            [ ("1_000.5", 1000.5, "1_000.5")
            , ("1e10", 1e10, "1e10")
            , ("0.5", 0.5, "0.5")
            , ("1.5e-3", 1.5e-3, "1.5e-3")
            , ("1_0.0", 10.0, "1_0.0")
            ]
      mapM_ (\(s, expectedVal, expectedRaw) ->
        case parseProgram "test.php" ("<?php return " <> s <> ";") of
          Left err -> assertFailure ("Failed on valid float " ++ show s ++ ": " ++ show (formatParseError err))
          Right (Program _ [StmtReturn _ (Just (ExprLit _ (LitFloat _ val raw)))] ) -> do
            assertEqual ("Value for " ++ show s) expectedVal val
            assertEqual ("Raw text for " ++ show s) expectedRaw raw
          other -> assertFailure ("Expected float " ++ show s ++ ", got: " ++ show other)) validFloats

      case parseProgram "test.php" "<?php return 1_000;" of
        Left err -> assertFailure ("Failed on valid int 1_000: " ++ show (formatParseError err))
        Right (Program _ [StmtReturn _ (Just (ExprLit _ (LitInt _ val raw)))] ) -> do
          assertEqual "Value for 1_000" 1000 val
          assertEqual "Raw text for 1_000" "1_000" raw
        other -> assertFailure ("Expected int 1_000, got: " ++ show other)

  , testCase "Incomplete base-prefixed integer literals are rejected (Issue #85)" $ do
      let incomplete = ["0x", "0b", "0o"]
      mapM_ (\s -> case parseProgram "test.php" ("<?php $x = " <> s <> ";") of
        Right r -> assertFailure ("Expected parse error for " ++ show s ++ ", got: " ++ show r)
        Left _ -> pure ()) incomplete

      let validTests =
            [ ("0x1", 1)
            , ("0x7fF", 2047)
            , ("0b1", 1)
            , ("0b1010", 10)
            , ("0o7", 7)
            , ("0o755", 493)
            , ("0xFF_FF", 65535)
            ]
      mapM_ (\(s, expectedVal) ->
        case parseExpression "test.php" s of
          Right (ExprLit _ (LitInt _ val _)) ->
            assertEqual ("Value for " ++ show s) expectedVal val
          other -> assertFailure ("Failed on " ++ show s ++ ": " ++ show other)) validTests

  , testCase "Heredoc and Nowdoc flexible syntax" $ do
      let hereSrc = "<<<EOF\nHello World\nEOF"
          nowSrc = "<<<'NOW'\nSingle $quoted raw\nNOW"
      case parseExpression "test.php" hereSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" _ False)) -> pure ()
        other -> assertFailure ("Heredoc failed: " ++ show other)

      case parseExpression "test.php" nowSrc of
        Right (ExprLit _ (LitHeredoc _ "NOW" _ True)) -> pure ()
        other -> assertFailure ("Nowdoc failed: " ++ show other)

      let doubleQuotedHereSrc = "<<<\"EOF\"\nDouble quoted heredoc\nEOF"
      case parseExpression "test.php" doubleQuotedHereSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" _ False)) -> pure ()
        other -> assertFailure ("Double-quoted Heredoc failed: " ++ show other)

  , testCase "Heredoc and nowdoc lines starting with closing tag prefix" $ do
      let hereSrc = "<<<EOF\nEOF_MORE\nEOF"
      case parseExpression "test.php" hereSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" content False)) ->
          assertEqual "content matches" "EOF_MORE" content
        other -> assertFailure ("Heredoc prefix in body failed: " ++ show other)

      let nowSrc = "<<<'NOW'\nNOW_MORE\nNOW123\nNOW"
      case parseExpression "test.php" nowSrc of
        Right (ExprLit _ (LitHeredoc _ "NOW" content True)) ->
          assertEqual "nowdoc content matches" "NOW_MORE\nNOW123" content
        other -> assertFailure ("Nowdoc prefix in body failed: " ++ show other)

      let indentedSrc = "<<<EOF\n    EOF_MORE\n    EOF123\n    EOF"
      case parseExpression "test.php" indentedSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" content False)) ->
          assertEqual "indented content matches" "EOF_MORE\nEOF123" content
        other -> assertFailure ("Indented heredoc prefix in body failed: " ++ show other)

      let doubleQuotedSrc = "<<<\"EOF\"\nEOF_MORE\nEOF"
      case parseExpression "test.php" doubleQuotedSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" content False)) ->
          assertEqual "double quoted heredoc content matches" "EOF_MORE" content
        other -> assertFailure ("Double quoted heredoc prefix in body failed: " ++ show other)

  , testCase "Unterminated heredoc or nowdoc terminates with a parse error (Issue #84)" $ do
      -- The heredoc scanner used to loop forever at end of input. Bound each
      -- case with an in-process timeout so a regression fails instead of
      -- hanging the suite; a healthy parse is far faster than this bound.
      let check :: String -> Maybe (Either ParseError a) -> Assertion
          check name = \case
            Nothing -> assertFailure (name ++ ": did not terminate within 2s (Issue #84 loop)")
            Just (Left err) ->
              unless (any (T.isInfixOf "heredoc") (errorExpected err)) $
                assertFailure (name ++ ": error does not mention the heredoc end: "
                               ++ show (formatParseError err))
            Just (Right _) -> assertFailure (name ++ ": parsed, but unterminated heredoc must fail")
      check "heredoc via parseExpression" =<<
        timeout 2000000 (evaluate (parseExpression "issue84.php" "<<<TAG\ncontent"))
      check "nowdoc via parseExpression" =<<
        timeout 2000000 (evaluate (parseExpression "issue84.php" "<<<'TAG'\ncontent"))
      check "heredoc via parseProgram" =<<
        timeout 2000000 (evaluate (parseProgram "issue84.php" "<?php $x = <<<TAG\ncontent"))

  , testCase "Generators: yield, yield key => val, yield from" $ do
      assertParsesOkExpr "yield"
      assertParsesOkExpr "yield $value"
      assertParsesOkExpr "yield $key => $value"
      assertParsesOkExpr "yield from $generator"

  , testCase "Closures and Arrow functions" $ do
      let fnSrc = "fn(int $x, int $y): int => $x + $y"
          closureSrc = "function ($a) use ($b, &$c): void { return; }"
      assertParsesOkExpr fnSrc
      assertParsesOkExpr closureSrc

  , testCase "Arrow function body precedence: fn($x) => $x == 1 and fn($x) => $x && $y" $ do
      let src = "fn($x) => $x == 1"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprArrowFunction _ _ _ _ _ _ body) -> case body of
          ExprBinary _ OpEq _ _ -> pure ()
          other -> assertFailure ("Expected ExprBinary OpEq in arrow body, got: " ++ show other)
        Right other -> assertFailure ("Expected ExprArrowFunction, got: " ++ show other)

  , testCase "Null coalescing assignment operator: $a ??= $b" $ do
      let src = "$a ??= $b"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "pretty-printed representation" "$a ??= $b" (prettyPrintExpr expr)

  , testCase "Array access on cast expression requires parentheses: ((int)$x)[0]" $ do
      let expr = ExprArrayAccess () (ExprCast () CastInt (ExprVar () (SimpleVar () (VarName () "x")))) (Just (ExprLit () (LitInt () 0 "0")))
          printed = prettyPrintExpr expr
      assertEqual "pretty printed" "((int)$x)[0]" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

  , testCase "Double unary minus does not merge into pre-decrement: - (-$x)" $ do
      let expr = ExprUnary () OpUnaryMinus (ExprUnary () OpUnaryMinus (ExprVar () (SimpleVar () (VarName () "x"))))
          printed = prettyPrintExpr expr
      assertEqual "pretty printed" "- -$x" printed
      case parseExpression "test.php" printed of
        Left err -> assertFailure (show (formatParseError err))
        Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

  , testCase "Dynamic class instantiation using variable: new $c()" $ do
      let src = "new $c()"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNew _ (ClassTargetExpr (ExprVar _ (SimpleVar _ (VarName _ "c")))) [] ->
              pure ()
            other -> assertFailure ("Expected ExprNew with dynamic variable ClassTargetExpr, got: " ++ show other)
          assertEqual "pretty printed" "new $c()" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

  , testCase "Dynamic class instantiation without parentheses: new $c" $ do
      let src = "new $c"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprNew _ (ClassTargetExpr (ExprVar _ (SimpleVar _ (VarName _ "c")))) [] ->
            pure ()
          other -> assertFailure ("Expected ExprNew without parens, got: " ++ show other)

  , testCase "Dynamic class instantiation with property fetch: new $this->serviceClass()" $ do
      let src = "new $this->serviceClass()"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNew _ (ClassTargetExpr (ExprPropertyFetch _ (ExprVar _ (SimpleVar _ (VarName _ "this"))) (MemberIdent (Ident _ "serviceClass")))) [] ->
              pure ()
            other -> assertFailure ("Expected ExprNew with property fetch ClassTargetExpr, got: " ++ show other)
          assertEqual "pretty printed" "new $this->serviceClass()" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure (show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

  , testCase "Dynamic class instantiation with array access: new $classes[0]('arg')" $ do
      let src = "new $classes[0]('arg')"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprNew _ (ClassTargetExpr (ExprArrayAccess _ (ExprVar _ (SimpleVar _ (VarName _ "classes"))) (Just (ExprLit _ (LitInt _ 0 "0"))))) [Arg _ Nothing (ExprLit _ (LitString _ "arg" "'arg'")) False] ->
            pure ()
          other -> assertFailure ("Expected ExprNew with array access ClassTargetExpr, got: " ++ show other)

  , testCase "Dynamic class instantiation with static property: new Foo::$class()" $ do
      let src = "new Foo::$class()"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNew _ (ClassTargetExpr (ExprStaticPropertyFetch _ (ClassTargetName (QualifiedName _ NameUnqualified ["Foo"])) (VarName _ "class"))) [] ->
              pure ()
            other -> assertFailure ("Expected ExprNew with static property ClassTargetExpr, got: " ++ show other)
          assertEqual "pretty printed" "new Foo::$class()" (prettyPrintExpr expr)

  , testCase "Dynamic class instantiation with method chaining: new $c()->process()" $ do
      let src = "new $c()->process()"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprMethodCall _ (ExprNew _ (ClassTargetExpr (ExprVar _ (SimpleVar _ (VarName _ "c")))) []) (MemberIdent (Ident _ "process")) (ArgsList []) ->
            pure ()
          other -> assertFailure ("Expected ExprMethodCall on dynamic ExprNew, got: " ++ show other)

  , testCase "Issue #8 reproducer: parseProgram with $obj = new $c();" $ do
      let src = "<?php\n$obj = new $c();\n"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtExpr _ (ExprAssign _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (ExprNew _ (ClassTargetExpr (ExprVar _ (SimpleVar _ (VarName _ "c")))) []))] ->
            pure ()
          other -> assertFailure ("Expected StmtExpr with ExprAssign and dynamic ExprNew, got: " ++ show other)

  , testCase "Variable-variable syntax: $$var and $$$var (Issue #35)" $ do
      case parseExpression "test.php" "$$x" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "x")))) -> pure ()
            other -> assertFailure ("Expected ExprVar DynamicVar SimpleVar, got: " ++ show other)
          assertEqual "pretty printed $$x" "$$x" (prettyPrintExpr expr)

      case parseExpression "test.php" "$$$x" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprVar _ (DynamicVar _ (ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "x")))))) -> pure ()
            other -> assertFailure ("Expected nested DynamicVar, got: " ++ show other)
          assertEqual "pretty printed $$$x" "$$$x" (prettyPrintExpr expr)

      case parseExpression "test.php" "$x" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprVar _ (SimpleVar _ (VarName _ "x"))) -> pure ()
        other -> assertFailure ("Expected SimpleVar, got: " ++ show other)

      case parseExpression "test.php" "${$x}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "x"))))) -> pure ()
        other -> assertFailure ("Expected DynamicVar, got: " ++ show other)

      case parseExpression "test.php" "$$$$x" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "pretty printed $$$$x" "$$$$x" (prettyPrintExpr expr)

      case parseExpression "test.php" "${'prefix_' . $name}" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "pretty printed complex dynamic var" "${('prefix_' . $name)}" (prettyPrintExpr expr)

      case parseExpression "test.php" "$$obj->prop" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprPropertyFetch _ (ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))))) (MemberIdent (Ident _ "prop")) -> pure ()
            other -> assertFailure ("Expected ExprPropertyFetch on DynamicVar, got: " ++ show other)
          assertEqual "pretty printed $$obj->prop" "$$obj->prop" (prettyPrintExpr expr)

      case parseExpression "test.php" "$$arr['key']" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprArrayAccess _ (ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "arr"))))) (Just (ExprLit _ (LitString _ "key" _))) -> pure ()
            other -> assertFailure ("Expected ExprArrayAccess on DynamicVar, got: " ++ show other)
          assertEqual "pretty printed $$arr['key']" "$$arr['key']" (prettyPrintExpr expr)

      case parseExpression "test.php" "new $$c()" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNew _ (ClassTargetExpr (ExprVar _ (DynamicVar _ (ExprVar _ (SimpleVar _ (VarName _ "c")))))) [] -> pure ()
            other -> assertFailure ("Expected ExprNew with DynamicVar, got: " ++ show other)
          assertEqual "pretty printed new $$c()" "new $$c()" (prettyPrintExpr expr)

  , testGroup "Attributes on closures and arrow functions (Issue #34)"
      [ testCase "Arrow function with attribute: #[Test] fn() => 1" $ do
          case parseExpression "test.php" "#[Test] fn() => 1" of
            Left err -> assertFailure ("#[Test] fn() => 1 failed: " ++ show (formatParseError err))
            Right expr -> do
              case expr of
                ExprArrowFunction _ attrs _ _ _ _ _ ->
                  assertEqual "arrow function attrs" 1 (length attrs)
                other -> assertFailure ("Expected ExprArrowFunction, got: " ++ show other)
              let printed = prettyPrintExpr expr
              assertEqual "pretty printed #[Test] fn() => 1" "#[Test]\nfn () => 1" printed
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Arrow function parameter with attribute: fn(#[SensitiveParameter] $pass) => $pass" $ do
          case parseExpression "test.php" "fn(#[SensitiveParameter] $pass) => $pass" of
            Left err -> assertFailure ("fn(#[SensitiveParameter] $pass) => $pass failed: " ++ show (formatParseError err))
            Right expr -> do
              case expr of
                ExprArrowFunction _ _ _ _ [p] _ _ ->
                  assertEqual "arrow param attrs" 1 (length (paramAttrs p))
                other -> assertFailure ("Expected ExprArrowFunction with 1 param, got: " ++ show other)
              let printed = prettyPrintExpr expr
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Closure with attribute: #[Inline] function() {}" $ do
          case parseExpression "test.php" "#[Inline] function() {}" of
            Left err -> assertFailure ("#[Inline] function() {} failed: " ++ show (formatParseError err))
            Right expr -> do
              case expr of
                ExprClosure _ attrs _ _ _ _ _ _ ->
                  assertEqual "closure attrs" 1 (length attrs)
                other -> assertFailure ("Expected ExprClosure, got: " ++ show other)
              let printed = prettyPrintExpr expr
              assertEqual "pretty printed #[Inline] function() {}" "#[Inline]\nfunction () {\n    \n}" printed
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Closure parameter with attribute: function(#[SensitiveParameter] $pass) {}" $ do
          case parseExpression "test.php" "function(#[SensitiveParameter] $pass) {}" of
            Left err -> assertFailure ("function(#[SensitiveParameter] $pass) {} failed: " ++ show (formatParseError err))
            Right expr -> do
              case expr of
                ExprClosure _ _ _ _ [p] _ _ _ ->
                  assertEqual "closure param attrs" 1 (length (paramAttrs p))
                other -> assertFailure ("Expected ExprClosure with 1 param, got: " ++ show other)
              let printed = prettyPrintExpr expr
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Static arrow function with attributes: #[Attr] static fn() => 1" $ do
          case parseExpression "test.php" "#[Attr] static fn() => 1" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprArrowFunction _ attrs _ isStat _ _ _ -> do
                  assertEqual "attrs" 1 (length attrs)
                  assertBool "isStatic" isStat
                other -> assertFailure ("Expected ExprArrowFunction, got: " ++ show other)
              let printed = prettyPrintExpr expr
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Static closure with attributes: static #[Attr] function() {}" $ do
          case parseExpression "test.php" "static #[Attr] function() {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprClosure _ attrs _ isStat _ _ _ _ -> do
                  assertEqual "attrs" 1 (length attrs)
                  assertBool "isStatic" isStat
                other -> assertFailure ("Expected ExprClosure, got: " ++ show other)
              let printed = prettyPrintExpr expr
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      , testCase "Multiple attribute groups and trailing comma: #[A, B,] #[C] fn(#[D] $x): int => $x" $ do
          let src = "#[A, B,] #[C] fn(#[D] $x): int => $x"
          case parseExpression "test.php" src of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprArrowFunction _ attrs _ _ [p] _ _ -> do
                  assertEqual "arrow function attr groups" 2 (length attrs)
                  assertEqual "param attr groups" 1 (length (paramAttrs p))
                other -> assertFailure ("Expected ExprArrowFunction, got: " ++ show other)
              let printed = prettyPrintExpr expr
              case parseExpression "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right reparsed ->
                  assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)
      ]
  , testCase "Variable property fetch and method call syntax: $obj->$prop and $obj->$method() (Issue #30)" $ do
      case parseExpression "test.php" "$obj->$prop" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprPropertyFetch _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberExpr (ExprVar _ (SimpleVar _ (VarName _ "prop")))) -> pure ()
            other -> assertFailure ("Expected ExprPropertyFetch with MemberExpr, got: " ++ show other)
          assertEqual "pretty printed $obj->$prop" "$obj->$prop" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      case parseExpression "test.php" "$obj->$method()" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprMethodCall _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberExpr (ExprVar _ (SimpleVar _ (VarName _ "method")))) (ArgsList []) -> pure ()
            other -> assertFailure ("Expected ExprMethodCall with MemberExpr, got: " ++ show other)
          assertEqual "pretty printed $obj->$method()" "$obj->$method()" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      case parseExpression "test.php" "$obj?->$prop" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNullsafePropertyFetch _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberExpr (ExprVar _ (SimpleVar _ (VarName _ "prop")))) -> pure ()
            other -> assertFailure ("Expected ExprNullsafePropertyFetch with MemberExpr, got: " ++ show other)
          assertEqual "pretty printed $obj?->$prop" "$obj?->$prop" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      case parseExpression "test.php" "$obj?->$method()" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          case expr of
            ExprNullsafeMethodCall _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberExpr (ExprVar _ (SimpleVar _ (VarName _ "method")))) (ArgsList []) -> pure ()
            other -> assertFailure ("Expected ExprNullsafeMethodCall with MemberExpr, got: " ++ show other)
          assertEqual "pretty printed $obj?->$method()" "$obj?->$method()" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

      case parseExpression "test.php" "$obj->$$prop" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "pretty printed $obj->$$prop" "$obj->$$prop" (prettyPrintExpr expr)
          case parseExpression "test.php" (prettyPrintExpr expr) of
            Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
            Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

  , testGroup "Issue 19: late static binding"
      [ testCase "static::bar() parses as a static method call with target static" $ do
          case parseExpression "test.php" "static::bar()" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprStaticCall _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) (MemberIdent (Ident _ "bar")) (ArgsList []) ->
                  pure ()
                other -> assertFailure ("Expected ExprStaticCall with target static, got: " ++ show other)
              assertEqual "pretty printed" "static::bar()" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "static::$foo parses as a static property fetch with target static" $ do
          case parseExpression "test.php" "static::$foo" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprStaticPropertyFetch _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) (VarName _ "foo") ->
                  pure ()
                other -> assertFailure ("Expected ExprStaticPropertyFetch with target static, got: " ++ show other)
              assertEqual "pretty printed" "static::$foo" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "static::CONSTANT parses as a class constant fetch with target static" $ do
          case parseExpression "test.php" "static::CONSTANT" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprClassConstFetch _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) (ConstNameIdent (Ident _ "CONSTANT")) ->
                  pure ()
                other -> assertFailure ("Expected ExprClassConstFetch with target static, got: " ++ show other)
              assertEqual "pretty printed" "static::CONSTANT" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "static::class parses as a class constant fetch with target static" $ do
          case parseExpression "test.php" "static::class" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprClassConstFetch _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) (ConstNameIdent (Ident _ "class")) ->
                  pure ()
                other -> assertFailure ("Expected static::class, got: " ++ show other)
              assertEqual "pretty printed" "static::class" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "new static() parses as ExprNew with target static" $ do
          case parseExpression "test.php" "new static()" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprNew _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) [] ->
                  pure ()
                other -> assertFailure ("Expected ExprNew with target static, got: " ++ show other)
              assertEqual "pretty printed" "new static()" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "new static parses as ExprNew with target static" $ do
          case parseExpression "test.php" "new static" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprNew _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) [] ->
                  pure ()
                other -> assertFailure ("Expected ExprNew with target static, got: " ++ show other)
              assertEqual "pretty printed" "new static()" (prettyPrintExpr expr)
              assertRoundTripExpr expr
      ]
  , testGroup "Issue 20 reproducer: language construct expressions"
      [ testCase "isset($x) and isset($x, $y) parse into ExprIsset" $ do
          case parseExpression "test.php" "isset($x)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprIsset _ [ExprVar _ (SimpleVar _ (VarName _ "x"))] -> pure ()
                other -> assertFailure ("Expected ExprIsset, got: " ++ show other)
              assertEqual "pretty printed" "isset($x)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "isset($x, $y)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprIsset _ [ExprVar _ (SimpleVar _ (VarName _ "x")), ExprVar _ (SimpleVar _ (VarName _ "y"))] -> pure ()
                other -> assertFailure ("Expected ExprIsset with 2 args, got: " ++ show other)
              assertEqual "pretty printed" "isset($x, $y)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "empty($x) parses into ExprEmpty" $ do
          case parseExpression "test.php" "empty($x)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprEmpty _ (ExprVar _ (SimpleVar _ (VarName _ "x"))) -> pure ()
                other -> assertFailure ("Expected ExprEmpty, got: " ++ show other)
              assertEqual "pretty printed" "empty($x)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "eval($code) parses into ExprEval" $ do
          case parseExpression "test.php" "eval('$a = 1;')" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprEval _ (ExprLit _ (LitString _ "$a = 1;" _)) -> pure ()
                other -> assertFailure ("Expected ExprEval, got: " ++ show other)
              assertEqual "pretty printed" "eval('$a = 1;')" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "include and include_once parse into ExprInclude" $ do
          case parseExpression "test.php" "include 'file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncInclude (ExprLit _ (LitString _ "file.php" _)) -> pure ()
                other -> assertFailure ("Expected ExprInclude IncInclude, got: " ++ show other)
              assertEqual "pretty printed" "include 'file.php'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "include_once 'file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncIncludeOnce (ExprLit _ (LitString _ "file.php" _)) -> pure ()
                other -> assertFailure ("Expected ExprInclude IncIncludeOnce, got: " ++ show other)
              assertEqual "pretty printed" "include_once 'file.php'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "require and require_once parse into ExprInclude" $ do
          case parseExpression "test.php" "require 'file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncRequire (ExprLit _ (LitString _ "file.php" _)) -> pure ()
                other -> assertFailure ("Expected ExprInclude IncRequire, got: " ++ show other)
              assertEqual "pretty printed" "require 'file.php'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "require_once 'file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncRequireOnce (ExprLit _ (LitString _ "file.php" _)) -> pure ()
                other -> assertFailure ("Expected ExprInclude IncRequireOnce, got: " ++ show other)
              assertEqual "pretty printed" "require_once 'file.php'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "isset with trailing comma parses correctly" $ do
          case parseExpression "test.php" "isset($x, $y,)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprIsset _ [ExprVar _ (SimpleVar _ (VarName _ "x")), ExprVar _ (SimpleVar _ (VarName _ "y"))] -> pure ()
                other -> assertFailure ("Expected ExprIsset with 2 args, got: " ++ show other)
              assertEqual "pretty printed" "isset($x, $y)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "empty and eval with complex expressions" $ do
          case parseExpression "test.php" "empty($obj->prop)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprEmpty _ (ExprPropertyFetch _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (MemberIdent (Ident _ "prop"))) -> pure ()
                other -> assertFailure ("Expected ExprEmpty on property fetch, got: " ++ show other)
              assertEqual "pretty printed" "empty($obj->prop)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "empty($arr['k'])" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprEmpty _ (ExprArrayAccess _ (ExprVar _ (SimpleVar _ (VarName _ "arr"))) (Just (ExprLit _ (LitString _ "k" _)))) -> pure ()
                other -> assertFailure ("Expected ExprEmpty on array access, got: " ++ show other)
              assertEqual "pretty printed" "empty($arr['k'])" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "include with concatenation and parentheses" $ do
          case parseExpression "test.php" "include $dir . '/file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncInclude (ExprBinary _ OpConcat (ExprVar _ (SimpleVar _ (VarName _ "dir"))) (ExprLit _ (LitString _ "/file.php" _))) -> pure ()
                other -> assertFailure ("Expected ExprInclude with concat, got: " ++ show other)
              assertRoundTripExpr expr

          case parseExpression "test.php" "include('file.php')" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprInclude _ IncInclude (ExprLit _ (LitString _ "file.php" _)) -> pure ()
                other -> assertFailure ("Expected ExprInclude from parenthesized argument, got: " ++ show other)
              assertEqual "pretty printed" "include 'file.php'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

      , testCase "language constructs are case-insensitive" $ do
          assertParsesOkExpr "ISSET($x)"
          assertParsesOkExpr "Empty($x)"
          assertParsesOkExpr "EVAL('$x = 1;')"
          assertParsesOkExpr "Include 'file.php'"
          assertParsesOkExpr "Require_Once 'file.php'"

      , testCase "language constructs in assignment and boolean context" $ do
          case parseExpression "test.php" "$res = include 'file.php'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprAssign _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "res"))) (ExprInclude _ IncInclude (ExprLit _ (LitString _ "file.php" _))) -> pure ()
                other -> assertFailure ("Expected ExprAssign with ExprInclude, got: " ++ show other)
              assertRoundTripExpr expr

          case parseExpression "test.php" "!isset($x) && !empty($y)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprBinary _ OpBoolAnd (ExprUnary _ OpBoolNot (ExprIsset _ [_])) (ExprUnary _ OpBoolNot (ExprEmpty _ _)) -> pure ()
                other -> assertFailure ("Expected binary bool with isset and empty, got: " ++ show other)
              assertRoundTripExpr expr

      , testCase "print is an expression language construct (Issue #123)" $ do
          case parseExpression "test.php" "print 'hello'" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprPrint _ (ExprLit _ (LitString _ "hello" _)) -> pure ()
                other -> assertFailure ("Expected ExprPrint, got: " ++ show other)
              assertEqual "pretty printed" "print 'hello'" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "$x = print 1" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprAssign _ Nothing
                  (ExprVar _ (SimpleVar _ (VarName _ "x")))
                  (ExprPrint _ (ExprLit _ (LitInt _ 1 _))) -> pure ()
                other -> assertFailure ("Expected assignment with ExprPrint, got: " ++ show other)
              assertRoundTripExpr expr

          case parseExpression "test.php" "print 1 + 2" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprPrint _ (ExprBinary _ OpAdd _ _)) -> pure ()
            Right other -> assertFailure ("Expected print to contain the addition, got: " ++ show other)

          case parseExpression "test.php" "print $x and $ok" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprBinary _ OpLogicalAnd (ExprPrint _ _) _) -> pure ()
            Right other -> assertFailure ("Expected and to remain outside print, got: " ++ show other)

          case parseExpression "test.php" "print $x = 1" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprPrint _ (ExprAssign _ Nothing _ _)) -> pure ()
            Right other -> assertFailure ("Expected assignment inside print, got: " ++ show other)

          case parseExpression "test.php" "print print 1" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprPrint _ (ExprPrint _ _)) -> pure ()
            Right other -> assertFailure ("Expected right-associative print, got: " ++ show other)

          case parseProgram "test.php" "<?php print 'hello';" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtExpr _ (ExprPrint _ _)]) -> pure ()
            Right other -> assertFailure ("Expected print expression statement, got: " ++ show other)

      , testCase "exit and die are expression language constructs (Issue #124)" $ do
          let statusless =
                [ ("exit", ExitExit)
                , ("exit()", ExitExit)
                , ("die", ExitDie)
                , ("die()", ExitDie)
                ] :: [(Text, ExitKind)]
          forM_ statusless $ \(src, kind) ->
            case parseExpression "test.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right expr -> do
                case expr of
                  ExprExit _ k Nothing | k == kind -> pure ()
                  other -> assertFailure (show src ++ ": expected statusless " ++ show kind
                                          ++ ", got: " ++ show other)
                assertRoundTripExpr expr

          case parseExpression "test.php" "exit(0)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprExit _ ExitExit (Just (ExprLit _ (LitInt _ 0 _))) -> pure ()
                other -> assertFailure ("Expected exit with integer status, got: " ++ show other)
              assertEqual "pretty printed" "exit(0)" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "die('err')" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprExit _ ExitDie (Just (ExprLit _ (LitString _ "err" _))) -> pure ()
                other -> assertFailure ("Expected die with string status, got: " ++ show other)
              assertEqual "pretty printed" "die('err')" (prettyPrintExpr expr)
              assertRoundTripExpr expr

          case parseExpression "test.php" "$x = exit(1)" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprAssign _ Nothing
                  (ExprVar _ (SimpleVar _ (VarName _ "x")))
                  (ExprExit _ ExitExit (Just (ExprLit _ (LitInt _ 1 _)))) -> pure ()
                other -> assertFailure ("Expected assignment with ExprExit, got: " ++ show other)
              assertRoundTripExpr expr

          case parseExpression "test.php" "$file or die('fail')" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr -> do
              case expr of
                ExprBinary _ OpLogicalOr (ExprVar _ _) (ExprExit _ ExitDie (Just _)) -> pure ()
                other -> assertFailure ("Expected or with die on the right, got: " ++ show other)
              assertRoundTripExpr expr

          case parseExpression "test.php" "exit($code . 'bye')" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprExit _ ExitExit (Just (ExprBinary _ OpConcat _ _))) -> pure ()
            Right other -> assertFailure ("Expected the concatenation as the status, got: " ++ show other)

          case parseExpression "test.php" "EXIT(2)" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprExit _ ExitExit (Just _)) -> pure ()
            Right other -> assertFailure ("Expected case-insensitive exit, got: " ++ show other)

          forM_ (["<?php exit;", "<?php exit();", "<?php exit(0);", "<?php exit(\"msg\");"] :: [Text]) $ \src ->
            case parseProgram "test.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right (Program _ [StmtExpr _ (ExprExit _ ExitExit _)]) -> pure ()
              Right other -> assertFailure (show src ++ ": expected exit statement, got: " ++ show other)

          forM_ (["<?php die;", "<?php die();", "<?php die(1);", "<?php die(\"err\");"] :: [Text]) $ \src ->
            case parseProgram "test.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right (Program _ [StmtExpr _ (ExprExit _ ExitDie _)]) -> pure ()
              Right other -> assertFailure (show src ++ ": expected die statement, got: " ++ show other)

          case parseProgram "test.php" "<?php $obj->die(); $obj->exit();" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtExpr _ (ExprMethodCall {}), StmtExpr _ (ExprMethodCall {})]) -> pure ()
            Right other -> assertFailure ("Expected method calls named die and exit, got: " ++ show other)
      ]

  , testGroup "Double-quoted string escapes (Issue #87)"
      [ testCase "decode octal, hexadecimal, and unknown escapes" $ do
          let quoted body = "\"" <> body <> "\""
              cases =
                [ (quoted "\\q", "\\q")
                , (quoted "\\101", "A")
                , (quoted "\\10", T.singleton (toEnum 8))
                , (quoted "\\0", T.singleton (toEnum 0))
                , (quoted "\\1011", "A1")
                , (quoted "\\377", T.singleton (toEnum 255))
                , (quoted "\\x41", "A")
                , (quoted "\\xFF", T.singleton (toEnum 255))
                , (quoted "\\x414", "A4")
                ]
          forM_ cases $ \(src, expected) ->
            case parseExpression "issue87.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right (ExprLit _ (LitString _ actual _)) ->
                assertEqual ("Unescaped value for " ++ show src) expected actual
              other -> assertFailure (show src ++ ": expected LitString, got " ++ show other)

      , testCase "known single-character escapes stay decoded" $ do
          let sourceBody = T.pack
                ['\\', 'n', '\\', 't', '\\', 'r', '\\', 'v', '\\', 'e'
                , '\\', 'f', '\\', '\\', '\\', '$', '\\', '"']
              expected = T.pack ['\n', '\t', '\r', '\v', '\ESC', '\f', '\\', '$', '"']
          case parseExpression "issue87.php" ("\"" <> sourceBody <> "\"") of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitString _ actual _)) ->
              assertEqual "known escape values" expected actual
            other -> assertFailure ("Expected LitString, got " ++ show other)

      , testCase "interpolated literal chunks use decoded escape values" $ do
          case parseExpression "issue87.php" "\"\\101 $name\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ [StrLit "A ", StrExpr (ExprVar _ (SimpleVar _ (VarName _ "name")))])) -> pure ()
            other -> assertFailure ("Expected decoded interpolated string, got " ++ show other)

      , testCase "heredoc decodes escapes while nowdoc preserves them" $ do
          let body = "\\q \\101 \\x41 \\n"
              heredoc = "<<<EOF\n" <> body <> "\nEOF"
              nowdoc = "<<<'EOF'\n" <> body <> "\nEOF"
          case parseExpression "issue87.php" heredoc of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitHeredoc _ "EOF" content False)) ->
              assertEqual "heredoc content" "\\q A A \n" content
            other -> assertFailure ("Expected heredoc, got " ++ show other)
          case parseExpression "issue87.php" nowdoc of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitHeredoc _ "EOF" content True)) ->
              assertEqual "nowdoc content" body content
            other -> assertFailure ("Expected nowdoc, got " ++ show other)
      ]

  , testGroup "Double-quoted string interpolation (Issue #51)"
      [ testCase "simple variable interpolation parses into LitInterpolated" $ do
          case parseExpression "test.php" "\"hello $name\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ parts)) ->
              case parts of
                [StrLit "hello ", StrExpr (ExprVar _ (SimpleVar _ (VarName _ "name")))] -> pure ()
                _ -> assertFailure ("Unexpected string parts: " ++ show parts)
            other -> assertFailure ("Expected LitInterpolated, got: " ++ show other)

      , testCase "escaped dollar sign stays a plain literal string" $ do
          case parseExpression "test.php" "\"escaped \\$name\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitString _ "escaped $name" _)) -> pure ()
            other -> assertFailure ("Expected LitString, got: " ++ show other)

      , testCase "simple syntax property access and subscript become StrExpr parts" $ do
          case parseExpression "test.php" "\"$obj->a and $arr[0] and $arr[key] and $arr[$k]\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ parts)) -> do
              when (length parts /= 7) $
                assertFailure ("Expected 7 string parts, got: " ++ show parts)
              case parts of
                [ StrExpr (ExprPropertyFetch _ _ _)
                 , StrLit " and "
                 , StrExpr (ExprArrayAccess _ _ (Just (ExprLit _ (LitInt _ 0 _))))
                 , StrLit " and "
                 , StrExpr (ExprArrayAccess _ _ (Just (ExprLit _ (LitString _ "key" _))))
                 , StrLit " and "
                 , StrExpr (ExprArrayAccess _ _ (Just (ExprVar _ (SimpleVar _ (VarName _ "k")))))
                 ] -> pure ()
                _ -> assertFailure ("Unexpected string parts: " ++ show parts)
            other -> assertFailure ("Expected LitInterpolated, got: " ++ show other)

      , testCase "complex curly syntax parses the full expression" $ do
          case parseExpression "test.php" "\"a{$arr['k']}b{$obj->b->c}c{ $name }d{notvar}e\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ parts)) ->
              case parts of
                [ StrLit "a"
                 , StrExpr (ExprArrayAccess _ _ (Just (ExprLit _ (LitString _ "k" _))))
                 , StrLit "b"
                 , StrExpr (ExprPropertyFetch _ (ExprPropertyFetch _ _ _) _)
                 , StrLit "c{ "
                 , StrExpr (ExprVar _ (SimpleVar _ (VarName _ "name")))
                 , StrLit " }d{notvar}e"
                 ] -> pure ()
                _ -> assertFailure ("Unexpected string parts: " ++ show parts)
            other -> assertFailure ("Expected LitInterpolated, got: " ++ show other)

      , testCase "plain and stray-dollar strings remain LitString" $
          forM_ [ "\"plain\"" :: Text
                , "\"$\""
                , "\"$$\""
                , "\"$1abc\""
                , "\"{b}\""
                ] $ \src ->
            case parseExpression "test.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right (ExprLit _ (LitString _ _ _)) -> pure ()
              other -> assertFailure ("Expected LitString for " ++ show src ++ ", got: " ++ show other)

      , testCase "space after brace keeps it literal but the variable still interpolates" $
          case parseExpression "test.php" "\"a{ $name }b\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ [StrLit "a{ ", StrExpr (ExprVar _ (SimpleVar _ (VarName _ "name"))), StrLit " }b"])) -> pure ()
            other -> assertFailure ("Expected literal brace around interpolation, got: " ++ show other)

      , testCase "dollar-dollar keeps the first dollar literal and interpolates the name" $
          case parseExpression "test.php" "\"$$name\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprLit _ (LitInterpolated _ [StrLit "$", StrExpr (ExprVar _ (SimpleVar _ (VarName _ "name")))])) -> pure ()
            other -> assertFailure ("Expected literal $ plus interpolation, got: " ++ show other)

      , testCase "interpolated strings round-trip through pretty printing" $
          forM_ [ "\"hello $name\"" :: Text
                , "\"$obj->a and $arr[0]\""
                , "\"a{$arr['k']}b\""
                ] $ \src -> do
            expr <- case parseExpression "test.php" src of
              Left err -> assertFailure (show (formatParseError err))
              Right e -> pure e
            reparsed <- case parseExpression "test.php" (prettyPrintExpr expr) of
              Left err -> assertFailure ("Reparse failed: " ++ show (formatParseError err))
              Right e -> pure e
            assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)
      ]

  , testGroup "Escaped dollar printing (Issue #88)"
      [ testCase "literal dollars and backslashes in text parts survive printing" $
          forM_
            [ "\"literal \\$name $other\"" :: Text
            , "\"\\$name $a\""
            , "\"\\${x} $a\""
            , "\"\\\\$name $a\""
            ] $ \src ->
            case parseExpression "issue88.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right expr -> assertRoundTripExpr expr

      , testCase "escaped dollars reprint escaped, safe dollars stay bare" $ do
          let printedFor src = do
                expr <- case parseExpression "issue88.php" src of
                  Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
                  Right e -> pure e
                pure (prettyPrintExpr expr)
          escaped <- printedFor (T.pack "\"literal \\$name $other\"")
          assertEqual "literal $name reprints as \\$name"
            (T.pack "\"literal \\$name {$other}\"") escaped
          safe <- printedFor (T.pack "\"a\\$ $a\"")
          assertEqual "a $ before a space needs no escape"
            (T.pack "\"a$ {$a}\"") safe
      ]

  , testGroup "Escaped double quotes in interpolated strings (Issue #111)"
      [ testCase "escaped double quotes in interpolated text survive printing and round-trip (minimal)" $ do
          let src = "\"\\\"$x\"" :: Text
          case parseExpression "issue111.php" src of
            Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
            Right expr -> assertRoundTripExpr expr
      , testCase "escaped double quotes reprint escaped (minimal)" $ do
          case parseExpression "issue111.php" "\"\\\"$x\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr ->
              assertEqual "escaped double quotes reprint as \\\""
                "\"\\\"{$x}\"" (prettyPrintExpr expr)
      , testCase "user reported reproducer round-trips" $ do
          let src = "\"hello \\\"world\\\" $x\"" :: Text
          case parseExpression "issue111.php" src of
            Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
            Right expr -> assertRoundTripExpr expr
      , testCase "user reported reproducer reprints escaped" $ do
          case parseExpression "issue111.php" "\"hello \\\"world\\\" $x\"" of
            Left err -> assertFailure (show (formatParseError err))
            Right expr ->
              assertEqual "escaped double quotes reprint as \\\""
                "\"hello \\\"world\\\" {$x}\"" (prettyPrintExpr expr)
      , testCase "interpolated strings with combinations of escaped double quotes, dollars, and backslashes" $ do
          forM_
            [ "\"hello \\\"world\\\" \\$name \\\\ $x\"" :: Text
            , "\"\\\"\\\\\\\" \\$x $y\""
            , "\"\\\\\\\"$x\""
            , "\"\\\"$x\\\"\""
            , "\"a \\\" b \\$c \\\\ d $e\""
            ] $ \src ->
            case parseExpression "issue111.php" src of
              Left err -> assertFailure (show src ++ ": " ++ show (formatParseError err))
              Right expr -> assertRoundTripExpr expr
      ]

  , testGroup "List destructuring syntax (Issue #125)"
      [ testCase "basic list assignment" $ do
          assertParsesOkExpr "list($a, $b) = $arr"
          case parseExpression "test.php" "list($a, $b) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItem _ Nothing (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "keyed items" $ do
          assertParsesOkExpr "list(\"k\" => $v) = $arr"
          case parseExpression "test.php" "list(\"k\" => $v) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItem _ (Just (ExprLit _ (LitString _ "k" _))) (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "nested list" $ do
          assertParsesOkExpr "list($a, list($b, $c)) = $arr"
          case parseExpression "test.php" "list($a, list($b, $c)) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItem _ Nothing (ExprList _ [_, _]) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "omitted slots" $ do
          assertParsesOkExpr "list($a, , $b) = $arr"
          case parseExpression "test.php" "list($a, , $b) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "multiple and leading omitted slots" $ do
          assertParsesOkExpr "list(, $b) = $arr"
          assertParsesOkExpr "list(, , $c) = $arr"
          assertParsesOkExpr "list($a, , , $b) = $arr"
          case parseExpression "test.php" "list(, , $c) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItemEmpty _, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "trailing commas" $ do
          assertParsesOkExpr "list($a, $b,) = $arr"
          assertParsesOkExpr "list($a, ,) = $arr"
          case parseExpression "test.php" "list($a, $b,) = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItem _ Nothing (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "empty list" $ do
          assertParsesOkExpr "list() = $arr"
          case parseExpression "test.php" "list() = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprList _ []) _) -> pure ()
      , testCase "case insensitivity" $ do
          assertParsesOkExpr "LIST($a, $b) = $arr"
          assertParsesOkExpr "List($a, $b) = $arr"
      ]
  , testGroup "Array destructuring with omitted elements (Issue #126)"
      [ testCase "basic array destructuring with omitted elements" $ do
          assertParsesOkExpr "[$a, , $b] = $arr"
          case parseExpression "test.php" "[$a, , $b] = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "a"))) False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "b"))) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "leading omitted elements" $ do
          assertParsesOkExpr "[, $b] = $arr"
          assertParsesOkExpr "[, , $c] = $arr"
          case parseExpression "test.php" "[, , $c] = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprArray _ [ArrayItemEmpty _, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "c"))) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "multiple and trailing omitted slots" $ do
          assertParsesOkExpr "[$a, , , $b] = $arr"
          assertParsesOkExpr "[$a, ,] = $arr"
          case parseExpression "test.php" "[$a, ,] = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "a"))) False, ArrayItemEmpty _]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "keyed items with omitted slots" $ do
          assertParsesOkExpr "[\"k\" => $v, , \"x\" => $y] = $arr"
          case parseExpression "test.php" "[\"k\" => $v, , \"x\" => $y] = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprArray _ [ArrayItem _ (Just (ExprLit _ (LitString _ "k" _))) (ExprVar _ _) False, ArrayItemEmpty _, ArrayItem _ (Just (ExprLit _ (LitString _ "x" _))) (ExprVar _ _) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "nested array destructuring with omitted elements" $ do
          assertParsesOkExpr "[$a, [$b, , $c]] = $arr"
          case parseExpression "test.php" "[$a, [$b, , $c]] = $arr" of
            Left err -> assertFailure (show (formatParseError err))
            Right (ExprAssign _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItem _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ _) False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ _) False]) False]) _) -> pure ()
            other -> assertFailure ("Unexpected AST: " ++ show other)
      ]
  ]

assertRoundTripExpr :: Expr (Annotated Span) -> Assertion
assertRoundTripExpr expr =
  case parseExpression "test.php" (prettyPrintExpr expr) of
    Left err -> assertFailure ("Reparse failed: " ++ show (formatParseError err))
    Right reparsed -> assertEqual "round trip AST" (stripAnnotations expr) (stripAnnotations reparsed)

assertParsesOkExpr :: Text -> Assertion
assertParsesOkExpr src = case parseExpression "test.php" src of
  Left err -> assertFailure (show (formatParseError err))
  Right _ -> pure ()
