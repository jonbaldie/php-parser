{-# LANGUAGE OverloadedStrings #-}

module Test.PHP83Spec (php83Tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Language.PHP

php83Tests :: TestTree
php83Tests = testGroup "PHP 8.3 Specifications"
  [ testCase "Typed class constants" $ do
      let src = "<?php class Config { public const string APP_ENV = 'prod'; final const int MAX_LIMIT = 100; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberConst c1, MemberConst c2] -> do
              assertEqual "c1 visibility" (Just Public) (constVis c1)
              assertBool "c1 has type" (case constType c1 of Just (SimpleType _ _) -> True; _ -> False)
              assertBool "c2 is final" (constFinal c2)
              assertBool "c2 has type int" (case constType c2 of Just (SimpleType _ _) -> True; _ -> False)
            _ -> assertFailure "Expected 2 MemberConst"
          _ -> assertFailure "Expected StmtClass"

  , testCase "Dynamic class constant fetch Class::{$var}" $ do
      let src = "Config::{$varName}"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClassConstFetch _ _ (ConstNameDynamic (ExprVar _ (SimpleVar _ (VarName _ v)))) ->
            assertEqual "variable name" "varName" v
          other -> assertFailure ("Expected dynamic class const fetch, got: " ++ show other)

  , testCase "Dynamic class constant fetch with complex expression" $ do
      let src = "App\\Config::{$prefix . '_KEY'}"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClassConstFetch _ (ClassTargetName (QualifiedName _ NameQualified parts)) (ConstNameDynamic (ExprBinary _ OpConcat _ _)) ->
            assertEqual "qualified name parts" ["App", "Config"] parts
          other -> assertFailure ("Expected dynamic const fetch, got: " ++ show other)

  , testCase "Anonymous readonly class" $ do
      let src = "new readonly class { public function ping(): string { return 'pong'; } }"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprNewAnonClass _ _ modif _ _ _ members -> do
            assertBool "anonymous class is readonly" (classReadonly modif)
            assertEqual "member count" 1 (length members)
          other -> assertFailure ("Expected ExprNewAnonClass, got: " ++ show other)
  ]
