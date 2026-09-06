{-# LANGUAGE OverloadedStrings #-}

module Test.PHP85Spec (php85Tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Language.PHP

php85Tests :: TestTree
php85Tests = testGroup "PHP 8.5 Specifications"
  [ testCase "Pipe operator: $x |> 'trim' |> 'strtolower'" $ do
      let src = "$x |> 'trim' |> 'strtolower'"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprBinary _ OpPipe (ExprBinary _ OpPipe (ExprVar _ _) (ExprLit _ (LitString _ "trim" _))) (ExprLit _ (LitString _ "strtolower" _)) ->
            pure ()
          other -> assertFailure ("Expected pipe binary expression, got: " ++ show other)

  , testCase "Pipe operator with arrow functions" $ do
      let src = "$val |> fn($x) => $x * 2 |> fn($y) => $y + 1"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprBinary _ OpPipe (ExprBinary _ OpPipe _ (ExprArrowFunction _ _ _ _ _ _ _)) (ExprArrowFunction _ _ _ _ _ _ _) ->
            pure ()
          other -> assertFailure ("Expected pipe with arrow functions, got: " ++ show other)

  , testCase "Clone-with syntax: clone($obj, ['key' => 'val'])" $ do
      let src = "clone($record, ['status' => 'archived', 'updated_at' => $now])"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprVar _ (SimpleVar _ (VarName _ "record"))) (Just pairs) -> do
            assertEqual "clone pairs count" 2 (length pairs)
          other -> assertFailure ("Expected ExprClone with modifications, got: " ++ show other)

  , testCase "Clone-with with named 'with' parameter: clone($obj, with: ['a' => 1])" $ do
      let src = "clone($obj, with: ['a' => 1])"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just [(_, _)]) -> pure ()
          other -> assertFailure ("Expected ExprClone with pairs, got: " ++ show other)

  , testCase "Asymmetric visibility on static properties" $ do
      let src = "<?php class Database { public private(set) static string $activeConnection = 'default'; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberProperty pd] -> do
              let modif = propModifier pd
              assertEqual "static prop read vis" (Just Public) (propVis modif)
              assertEqual "static prop write vis" (Just Private) (propWriteVis modif)
              assertBool "prop is static" (propStatic modif)
            _ -> assertFailure "Expected MemberProperty"
          _ -> assertFailure "Expected StmtClass"
  ]
