{-# LANGUAGE OverloadedStrings #-}

module Test.RecursionSchemesSpec (recursionSchemesTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Monoid (Sum (..))
import Language.PHP

recursionSchemesTests :: TestTree
recursionSchemesTests = testGroup "Recursion Schemes & Traversal Specifications"
  [ testCase "Strip and map annotations" $ do
      let src = "$x + $y * 2"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let stripped = stripAnnotations expr
          assertEqual "Annotation is unit" () (getAnnotation stripped)
          let mapped = mapAnnotation (const ("annotated" :: Text)) expr
          assertEqual "Mapped annotation is text" "annotated" (getAnnotation mapped)

  , testCase "allVariables extracts all referenced variables" $ do
      let src = "($user->name . $prefix . $user->suffix . $fallback)"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let vars = allVariables expr
          assertEqual "Extracted variables" ["user", "prefix", "user", "fallback"] vars

  , testCase "transformExpr rewrites variable names" $ do
      let src = "($a + $b) * $a"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let renameAtoZ = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "a")) ->
                  ExprVar a (SimpleVar sv (VarName vn "z"))
                e -> e) expr
          let vars = allVariables renameAtoZ
          assertEqual "Variables after transformation" ["z", "b", "z"] vars

  , testCase "queryStmt counts total expressions in a block" $ do
      let src = "if ($cond) { $x = 1; return $x + 2; }"
      case parseStatement "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right stmt -> do
          let Sum exprCount = queryStmt (const (Sum (1 :: Int))) stmt
          assertBool "Expression count is positive" (exprCount >= 3)

  , testCase "allVariables and transformExpr handle variable-variables" $ do
      let src = "$$var + $$$nested"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let vars = allVariables expr
          assertEqual "Extracted variables from variable-variables" ["var", "nested"] vars
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "var")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Renamed variables" ["renamed", "nested"] (allVariables renamed)

  , testCase "foldStmt visits return inside block" $ do
      case parseStatement "test.php" "{ return 1; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right stmt ->
          assertEqual "returns inside block" [True] (foldReturns stmt)

  , testCase "foldStmt visits return inside function declaration" $ do
      assertFoldsReturn "<?php function foo() { return 1; }"

  , testCase "foldStmt visits return inside class method" $ do
      assertFoldsReturn "<?php class Foo { public function bar() { return 1; } }"

  , testCase "foldStmt visits return inside trait method" $ do
      assertFoldsReturn "<?php trait Foo { public function bar() { return 1; } }"

  , testCase "foldStmt visits return inside enum method" $ do
      assertFoldsReturn "<?php enum Foo { public function bar() { return 1; } }"

  , testCase "foldStmt visits return inside interface method" $ do
      assertFoldsReturn "<?php interface Foo { public function bar() { return 1; } }"

  , testCase "foldStmt visits return inside property hook block" $ do
      assertFoldsReturn "<?php class Book { public string $title { set(string $value) { return; } } }"

  , testCase "foldStmt still visits returns in if/while/try" $ do
      case parseStatement "test.php" "if ($c) { return 1; } elseif ($d) { return 2; } else { return 3; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right stmt ->
          assertEqual "returns inside if" [True, True, True] (foldReturns stmt)
      case parseStatement "test.php" "while ($c) { return 1; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right stmt ->
          assertEqual "returns inside while" [True] (foldReturns stmt)
      case parseStatement "test.php" "try { return 1; } catch (E $e) { return 2; } finally { return 3; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right stmt ->
          assertEqual "returns inside try" [True, True, True] (foldReturns stmt)
  ]

foldReturns :: Stmt a -> [Bool]
foldReturns = foldStmt (\case
  StmtReturn _ _ -> [True]
  _ -> [])

assertFoldsReturn :: Text -> IO ()
assertFoldsReturn src =
  case parseProgram "test.php" src of
    Left err -> assertFailure (show (formatParseError err))
    Right (Program _ [stmt]) ->
      assertEqual "returns inside declaration" [True] (foldReturns stmt)
    Right other -> assertFailure ("Expected one statement, got: " ++ show other)
