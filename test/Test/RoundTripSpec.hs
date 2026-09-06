{-# LANGUAGE OverloadedStrings #-}

module Test.RoundTripSpec (roundTripTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP

roundTripTests :: TestTree
roundTripTests = testGroup "Round-Trip & Property Verification"
  [ testCase "Round-trip class declaration with methods and properties" $ do
      let src = "<?php\nclass Account {\n    public private(set) string $id;\n    public function getId(): string {\n        return $this->id;\n    }\n}"
      assertRoundTrips src

  , testCase "Round-trip match expression" $ do
      let src = "<?php\n$res = match ($val) {\n    1, 2 => 'low',\n    default => 'high'\n};"
      assertRoundTrips src

  , testCase "Round-trip PHP 8.4 property hooks" $ do
      let src = "<?php\nclass Hooked {\n    public string $name {\n        get => $this->raw;\n        set(string $v) {\n            $this->raw = $v;\n        }\n    }\n}"
      assertRoundTrips src

  , testCase "Round-trip PHP 8.5 pipe operator" $ do
      let src = "<?php\n$result = (($x |> 'trim') |> 'strtolower');"
      assertRoundTrips src

  , testCase "Round-trip DNF types" $ do
      let src = "<?php\nfunction check((A&B)|C $param): void {\n}"
      assertRoundTrips src

  , testProperty "Arbitrary generated simple expressions round-trip cleanly" $
      forAll genSimpleExpr $ \origExpr ->
        let printed = prettyPrintExpr origExpr
        in case parseExpression "gen.php" printed of
             Left err -> counterexample ("Failed to parse printed: " ++ T.unpack printed ++ "\nError: " ++ show err) False
             Right reParsed ->
               counterexample ("Printed: " ++ T.unpack printed)
                 (stripAnnotations origExpr == stripAnnotations reParsed)
  ]

assertRoundTrips :: Text -> Assertion
assertRoundTrips src = case parseProgram "test.php" src of
  Left err -> assertFailure ("Initial parse failed: " ++ show (formatParseError err))
  Right ast -> do
    let printed = prettyPrint ast
    case parseProgram "test.php" printed of
      Left err2 -> assertFailure ("Round-trip parse failed on printed output:\n" ++ T.unpack printed ++ "\nError: " ++ show (formatParseError err2))
      Right ast2 ->
        assertEqual "AST structure preserves equality"
          (stripAnnotations ast)
          (stripAnnotations ast2)

-- | QuickCheck generator for simple AST expressions.
genSimpleExpr :: Gen (Expr ())
genSimpleExpr = sized genExprSized

genExprSized :: Int -> Gen (Expr ())
genExprSized n
  | n <= 0 = oneof
      [ pure (ExprLit () (LitInt () 42 "42"))
      , pure (ExprLit () (LitString () "hello" "'hello'"))
      , pure (ExprLit () (LitBool () True))
      , pure (ExprLit () (LitNull ()))
      , pure (ExprVar () (SimpleVar () (VarName () "x")))
      ]
  | otherwise = oneof
      [ pure (ExprLit () (LitInt () 42 "42"))
      , pure (ExprVar () (SimpleVar () (VarName () "item")))
      , do
          e1 <- genExprSized (n `div` 2)
          e2 <- genExprSized (n `div` 2)
          op <- elements [OpAdd, OpSub, OpMul, OpConcat, OpPipe]
          pure (ExprBinary () op e1 e2)
      , do
          e <- genExprSized (n - 1)
          pure (ExprUnary () OpBoolNot e)
      ]
