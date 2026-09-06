{-# LANGUAGE OverloadedStrings #-}

module Test.PHP82Spec (php82Tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Language.PHP

php82Tests :: TestTree
php82Tests = testGroup "PHP 8.2 Specifications"
  [ testCase "Readonly class definition" $ do
      let src = "<?php readonly class ImmutableUser { public string $name; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> do
            assertEqual "class name" "ImmutableUser" (let Ident _ n = className cd in n)
            assertBool "class is readonly" (classReadonly (classModifier cd))
          _ -> assertFailure "Expected StmtClass"

  , testCase "DNF types: (A&B)|C" $ do
      let src = "<?php function process((A&B)|C $param): void {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtFunction _ fn] -> case funcParams fn of
            [p] -> case paramType p of
              Just (UnionType _ [DNFType _ [IntersectionType _ _], SimpleType _ _]) -> pure ()
              other -> assertFailure ("Unexpected type structure: " ++ show other)
            _ -> assertFailure "Expected 1 param"
          _ -> assertFailure "Expected StmtFunction"

  , testCase "DNF types: (A&B)|(C&D)" $ do
      let src = "<?php function process((A&B)|(C&D) $param): void {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtFunction _ fn] -> case funcParams fn of
            [p] -> case paramType p of
              Just (UnionType _ [DNFType _ _, DNFType _ _]) -> pure ()
              other -> assertFailure ("Unexpected DNF type: " ++ show other)
            _ -> assertFailure "Expected 1 param"
          _ -> assertFailure "Expected StmtFunction"

  , testCase "Standalone null, false, and true types" $ do
      let srcNull = "<?php function alwaysNull(): null { return null; }"
          srcFalse = "<?php function alwaysFalse(): false { return false; }"
          srcTrue = "<?php function alwaysTrue(): true { return true; }"
      assertParsesOk srcNull
      assertParsesOk srcFalse
      assertParsesOk srcTrue

  , testCase "Constants defined in traits" $ do
      let src = "<?php trait Loggable { public const LOG_LEVEL = 'INFO'; const DEFAULT_CHAN = 1; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtTrait _ td] -> do
            assertEqual "trait name" "Loggable" (let Ident _ n = traitName td in n)
            assertEqual "member count" 2 (length (traitMembers td))
            case traitMembers td of
              [MemberConst c1, MemberConst c2] -> do
                assertEqual "const 1 visibility" (Just Public) (constVis c1)
                assertEqual "const 2 visibility" Nothing (constVis c2)
              _ -> assertFailure "Expected 2 MemberConst"
          _ -> assertFailure "Expected StmtTrait"
  ]

assertParsesOk :: Text -> Assertion
assertParsesOk src = case parseProgram "test.php" src of
  Left err -> assertFailure (show (formatParseError err))
  Right _ -> pure ()
