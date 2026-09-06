{-# LANGUAGE OverloadedStrings #-}

module Test.PHP84Spec (php84Tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Language.PHP

php84Tests :: TestTree
php84Tests = testGroup "PHP 8.4 Specifications"
  [ testCase "Property hooks: get arrow and set block" $ do
      let src = "<?php class Book { public string $title { get => $this->rawTitle; set(string $value) { $this->rawTitle = trim($value); } } }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberProperty pd] -> do
              assertEqual "property hook count" 2 (length (propHooks pd))
              case propHooks pd of
                [hGet, hSet] -> do
                  assertEqual "hook 1 type" HookGet (hookType hGet)
                  assertBool "hook 1 is expression" (case hookBody hGet of HookExpr _ -> True; _ -> False)
                  assertEqual "hook 2 type" HookSet (hookType hSet)
                  assertBool "hook 2 is block" (case hookBody hSet of HookBlock _ -> True; _ -> False)
                  assertBool "hook 2 has param" (case hookParam hSet of Just (VarName _ "value", Just (SimpleType _ _)) -> True; _ -> False)
                _ -> assertFailure "Expected 2 hooks"
            _ -> assertFailure "Expected MemberProperty"
          _ -> assertFailure "Expected StmtClass"

  , testCase "Asymmetric property visibility public private(set)" $ do
      let src = "<?php class Order { public private(set) string $status; protected private(set) int $id; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberProperty p1, MemberProperty p2] -> do
              assertEqual "p1 read vis" (Just Public) (propVis (propModifier p1))
              assertEqual "p1 write vis" (Just Private) (propWriteVis (propModifier p1))
              assertEqual "p2 read vis" (Just Protected) (propVis (propModifier p2))
              assertEqual "p2 write vis" (Just Private) (propWriteVis (propModifier p2))
            _ -> assertFailure "Expected 2 MemberProperty"
          _ -> assertFailure "Expected StmtClass"

  , testCase "Constructor promotion with asymmetric visibility" $ do
      let src = "<?php class Point { public function __construct(public private(set) int $x, public private(set) int $y) {} }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberMethod md] -> case methodParams md of
              [p1, p2] -> do
                assertEqual "p1 read vis" (Just Public) (paramVis p1)
                assertEqual "p1 write vis" (Just Private) (paramWriteVis p1)
                assertEqual "p2 read vis" (Just Public) (paramVis p2)
                assertEqual "p2 write vis" (Just Private) (paramWriteVis p2)
              _ -> assertFailure "Expected 2 params"
            _ -> assertFailure "Expected MemberMethod"
          _ -> assertFailure "Expected StmtClass"

  , testCase "New without parentheses method call: new Service()->process()" $ do
      let src = "new Service()->process()"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprMethodCall _ (ExprNew _ (ClassTargetName (QualifiedName _ NameUnqualified ["Service"])) []) (MemberIdent (Ident _ "process")) (ArgsList []) ->
            pure ()
          other -> assertFailure ("Expected ExprMethodCall on ExprNew, got: " ++ show other)

  , testCase "New without parentheses class constant fetch: new Config()::KEY" $ do
      let src = "new Config()::KEY"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClassConstFetch _ (ClassTargetExpr (ExprNew _ _ [])) (ConstNameIdent (Ident _ "KEY")) ->
            pure ()
          other -> assertFailure ("Expected ExprClassConstFetch on ExprNew, got: " ++ show other)

  , testCase "New with arguments method call chain: new Client($host, $port)->connect()->send('ping')" $ do
      let src = "new Client($host, $port)->connect()->send('ping')"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprMethodCall _ (ExprMethodCall _ (ExprNew _ _ [_, _]) _ _) _ _ ->
            pure ()
          other -> assertFailure ("Expected chained call, got: " ++ show other)
  ]
