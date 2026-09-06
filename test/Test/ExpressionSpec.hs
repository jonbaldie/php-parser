{-# LANGUAGE OverloadedStrings #-}

module Test.ExpressionSpec (expressionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Language.PHP

expressionTests :: TestTree
expressionTests = testGroup "Expression Specifications"
  [ testCase "Match expression with multiple patterns and default" $ do
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

  , testCase "Nullsafe operator chain: $user?->getProfile()?->name" $ do
      let src = "$user?->getProfile()?->name"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprNullsafePropertyFetch _ (ExprNullsafeMethodCall _ _ (MemberIdent (Ident _ "getProfile")) _) (MemberIdent (Ident _ "name")) ->
            pure ()
          other -> assertFailure ("Expected nullsafe chain, got: " ++ show other)

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

  , testCase "Heredoc and Nowdoc flexible syntax" $ do
      let hereSrc = "<<<EOF\nHello World\nEOF"
          nowSrc = "<<<'NOW'\nSingle $quoted raw\nNOW"
      case parseExpression "test.php" hereSrc of
        Right (ExprLit _ (LitHeredoc _ "EOF" _ False)) -> pure ()
        other -> assertFailure ("Heredoc failed: " ++ show other)

      case parseExpression "test.php" nowSrc of
        Right (ExprLit _ (LitHeredoc _ "NOW" _ True)) -> pure ()
        other -> assertFailure ("Nowdoc failed: " ++ show other)

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
  ]

assertParsesOkExpr :: Text -> Assertion
assertParsesOkExpr src = case parseExpression "test.php" src of
  Left err -> assertFailure (show (formatParseError err))
  Right _ -> pure ()
