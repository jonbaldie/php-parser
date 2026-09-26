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
      let src = "$val |> (fn($x) => $x * 2) |> (fn($y) => $y + 1)"
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
          ExprClone _ (ExprVar _ (SimpleVar _ (VarName _ "record"))) (Just (ExprArray _ pairs)) -> do
            assertEqual "clone pairs count" 2 (length pairs)
          other -> assertFailure ("Expected ExprClone with modifications, got: " ++ show other)

  , testCase "Clone-with with named 'withProperties' parameter: clone($obj, withProperties: ['a' => 1]) (Issue #309)" $ do
      let src = "clone($obj, withProperties: ['a' => 1])"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just (ExprArray _ [_])) -> pure ()
          other -> assertFailure ("Expected ExprClone with pairs, got: " ++ show other)

  , testCase "Clone-with rejects named 'with' parameter: clone($obj, with: ['a' => 1]) (Issue #309)" $ do
      let src = "clone($obj, with: ['a' => 1])"
      case parseExpression "test.php" src of
        Left _ -> pure ()
        Right expr -> assertFailure ("Expected parse failure for with: parameter, but parsed as: " ++ show expr)

  , testCase "Clone-with with variable modification payload: clone($obj, $mods) (Issue #131)" $ do
      let src = "clone($obj, $mods)"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (Just (ExprVar _ (SimpleVar _ (VarName _ "mods")))) ->
            pure ()
          other -> assertFailure ("Expected ExprClone with variable mods, got: " ++ show other)

  , testCase "Clone-with with array() function call payload: clone($obj, array('a' => 1)) (Issue #131)" $ do
      let src = "clone($obj, array('a' => 1))"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just (ExprArray _ [_])) -> pure ()
          other -> assertFailure ("Expected ExprClone with array(...) payload, got: " ++ show other)

  , testCase "Clone-with with named withProperties: parameter and variable: clone($obj, withProperties: $mods) (Issue #131, #309)" $ do
      let src = "clone($obj, withProperties: $mods)"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ (ExprVar _ (SimpleVar _ (VarName _ "obj"))) (Just (ExprVar _ (SimpleVar _ (VarName _ "mods")))) ->
            pure ()
          other -> assertFailure ("Expected ExprClone with named withProperties: and variable, got: " ++ show other)

  , testCase "Clone-with with trailing comma in argument list (Issue #131, #309)" $ do
      let src1 = "clone($obj, $mods,)"
          src2 = "clone($obj, withProperties: ['a' => 1],)"
      case parseExpression "test.php" src1 of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just (ExprVar _ _)) -> pure ()
          other -> assertFailure ("Expected ExprClone with trailing comma, got: " ++ show other)
      case parseExpression "test.php" src2 of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just (ExprArray _ [_])) -> pure ()
          other -> assertFailure ("Expected ExprClone with trailing comma and named withProperties, got: " ++ show other)

  , testCase "Clone-with with arbitrary expressions: clone($obj, get_mods()) (Issue #131)" $ do
      let src = "clone($obj, get_mods())"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> case expr of
          ExprClone _ _ (Just (ExprCall _ _ _)) -> pure ()
          other -> assertFailure ("Expected ExprClone with ExprCall payload, got: " ++ show other)

  , testCase "Clone-with rejects named 'with' parameter with variable (Issue #309)" $ do
      let src = "clone($obj, with: $mods)"
      case parseExpression "test.php" src of
        Left _ -> pure ()
        Right expr -> assertFailure ("Expected parse failure for with: parameter, but parsed as: " ++ show expr)

  , testCase "Clone-with rejects named 'with' parameter with trailing comma (Issue #309)" $ do
      let src = "clone($obj, with: ['a' => 1],)"
      case parseExpression "test.php" src of
        Left _ -> pure ()
        Right expr -> assertFailure ("Expected parse failure for with: parameter, but parsed as: " ++ show expr)

  , testCase "Clone-with reproducer program with withProperties vs with (Issue #309)" $ do
      let validSrc = "<?php\nclass C { public function __construct(public int $n) {} }\n$c = new C(1);\n$d = clone($c, withProperties: [\"n\" => 2]);\necho $d->n;\n"
          invalidSrc = "<?php\nclass C { public function __construct(public int $n) {} }\n$c = new C(1);\n$d = clone($c, with: [\"n\" => 2]);\necho $d->n;\n"
      case parseProgram "valid.php" validSrc of
        Left err -> assertFailure ("Expected valid reproducer to parse: " ++ show (formatParseError err))
        Right prog -> do
          let printed = prettyPrint prog
          case parseProgram "reprinted.php" printed of
            Left err -> assertFailure ("Reprinted reproducer failed to parse: " ++ show (formatParseError err))
            Right _ -> pure ()
      case parseProgram "invalid.php" invalidSrc of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected invalid program with 'with:' parameter to fail parsing"


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
