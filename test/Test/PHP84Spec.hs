{-# LANGUAGE OverloadedStrings #-}

module Test.PHP84Spec (php84Tests) where

import Control.Monad (forM)
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

  , testCase "bodyless property hooks parse in interfaces and abstract classes (Issue #6)" $ do
      -- Interface: hooks with no bodies, each terminated by a semicolon.
      let ifaceSrc = "<?php interface HasName { public string $name { get; set; } }"
      case parseProgram "test.php" ifaceSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtInterface _ id'] -> case ifaceMembers id' of
            [MemberProperty pd] -> case propHooks pd of
              [hGet, hSet] -> do
                assertEqual "hook 1 type" HookGet (hookType hGet)
                assertEqual "hook 2 type" HookSet (hookType hSet)
              _ -> assertFailure "Expected 2 hooks"
            _ -> assertFailure "Expected MemberProperty"
          _ -> assertFailure "Expected StmtInterface"
      -- Abstract class: abstract property with a single bodyless hook.
      let abstractSrc = "<?php abstract class Base { abstract public string $name { get; } }"
      case parseProgram "test.php" abstractSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberProperty pd] -> case propHooks pd of
              [hGet] -> assertEqual "hook type" HookGet (hookType hGet)
              _ -> assertFailure "Expected 1 hook"
            _ -> assertFailure "Expected MemberProperty"
          _ -> assertFailure "Expected StmtClass"
      -- Bodyless hook with an explicit parameter list.
      let paramSrc = "<?php abstract class Base { abstract public int $count { set(string $val); } }"
      case parseProgram "test.php" paramSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberProperty pd] -> case propHooks pd of
              [hSet] -> do
                assertEqual "hook type" HookSet (hookType hSet)
                assertBool "hook has param" (case hookParam hSet of Just (VarName _ "val", Just (SimpleType _ _)) -> True; _ -> False)
              _ -> assertFailure "Expected 1 hook"
            _ -> assertFailure "Expected MemberProperty"
          _ -> assertFailure "Expected StmtClass"

  , testCase "bodyless hooks round-trip through pretty printing (Issue #6)" $ do
      let src = "<?php interface I { public string $name { get; set; } }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right prog -> do
          let printed = prettyPrint prog
          case parseProgram "test.php" printed of
            Left err -> assertFailure ("reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
            Right prog2 ->
              assertEqual "round-trip AST equal" (stripAnnotations prog) (stripAnnotations prog2)

  , testCase "bodyless hook with final modifier and parameter round-trips (Issue #6)" $ do
      let src = "<?php abstract class A { abstract public int $count { final set(string $val); } }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right prog -> do
          let printed = prettyPrint prog
          case parseProgram "test.php" printed of
            Left err -> assertFailure ("reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
            Right prog2 ->
              assertEqual "round-trip AST equal" (stripAnnotations prog) (stripAnnotations prog2)

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

  , testGroup "Property final/abstract modifiers (issue #5)"
    [ testCase "final public property parses" $ do
        let src = "<?php class C { final public string $name = \"test\"; }"
        case parseProgram "test.php" src of
          Left err -> assertFailure (show (formatParseError err))
          Right (Program _ stmts) -> case stmts of
            [StmtClass _ cd] -> case classMembers cd of
              [MemberProperty pd] -> do
                let m = propModifier pd
                assertBool "final modifier set" (propFinal m)
                assertEqual "read vis" (Just Public) (propVis m)
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

    , testCase "abstract property in abstract class parses without default" $ do
        let src = "<?php abstract class C { abstract public string $name; }"
        case parseProgram "test.php" src of
          Left err -> assertFailure (show (formatParseError err))
          Right (Program _ stmts) -> case stmts of
            [StmtClass _ cd] -> case classMembers cd of
              [MemberProperty pd] -> do
                let m = propModifier pd
                assertBool "abstract modifier set" (propAbstract m)
                assertEqual "no default value" Nothing
                  (case propItems pd of [(_, v)] -> v; _ -> Just (error "unreachable"))
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

    , testCase "modifier permutations parse to equivalent ASTs" $ do
        let srcs =
              [ "<?php class C { final public string $name; }"
              , "<?php class C { public final string $name; }"
              ]
        mods <- forM srcs $ \src ->
          case parseProgram "test.php" src of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberProperty pd] -> pure (propModifier pd)
              _ -> assertFailure ("Expected MemberProperty in " ++ show src)
            _ -> assertFailure ("Expected one class in " ++ show src)
        case mods of
          [m1, m2] -> assertEqual "permutations equal" m1 m2
          _ -> assertFailure "Expected 2 modifier sets"

    , testCase "final/abstract methods still parse after modifier extension" $ do
        let src = "<?php abstract class C { final public function f() {} public final static function g() {} abstract protected function h(); }"
        case parseProgram "test.php" src of
          Left err -> assertFailure (show (formatParseError err))
          Right (Program _ stmts) -> case stmts of
            [StmtClass _ cd] -> case classMembers cd of
              [MemberMethod m1, MemberMethod m2, MemberMethod m3] -> do
                assertBool "m1 final" (methodFinal (methodModifier m1))
                assertBool "m2 final+static" (methodFinal (methodModifier m2) && methodStatic (methodModifier m2))
                assertBool "m3 abstract" (methodAbstract (methodModifier m3))
              _ -> assertFailure "Expected 3 MemberMethod"
            _ -> assertFailure "Expected StmtClass"

    , testCase "final/abstract survive pretty-print round-trip" $ do
        let src = "<?php abstract class C { final public string $a; abstract protected int $b; }"
        case parseProgram "test.php" src of
          Left err -> assertFailure (show (formatParseError err))
          Right prog -> do
            let printed = prettyPrint prog
            case parseProgram "test.php" printed of
              Left err -> assertFailure ("reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
              Right prog2 ->
                assertEqual "round-trip AST equal" (stripAnnotations prog) (stripAnnotations prog2)
    ]
  ]
