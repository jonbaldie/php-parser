{-# LANGUAGE OverloadedStrings #-}

module Test.StatementSpec (statementTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Language.PHP

statementTests :: TestTree
statementTests = testGroup "Statement & Declaration Specifications"
  [ testCase "Attributes on classes, methods, and parameters" $ do
      let src = "<?php #[Entity] class Post { #[Id, GeneratedValue] public int $id; #[Route('/posts')] public function show(#[CurrentUser] User $user): void {} }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> do
            assertEqual "class attrs" 1 (length (classAttrs cd))
            case classMembers cd of
              [MemberProperty pd, MemberMethod md] -> do
                assertEqual "prop attrs" 1 (length (propAttrs pd))
                assertEqual "method attrs" 1 (length (methodAttrs md))
                case methodParams md of
                  [p] -> assertEqual "param attrs" 1 (length (paramAttrs p))
                  _ -> assertFailure "Expected 1 param"
              _ -> assertFailure "Expected property and method"
          _ -> assertFailure "Expected StmtClass"

  , testCase "Constructor property promotion" $ do
      let src = "<?php class Customer { public function __construct(public string $name, private readonly int $age = 18) {} }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberMethod md] -> case methodParams md of
              [p1, p2] -> do
                assertEqual "p1 vis" (Just Public) (paramVis p1)
                assertEqual "p2 vis" (Just Private) (paramVis p2)
                assertBool "p2 readonly" (paramReadonly p2)
              _ -> assertFailure "Expected 2 params"
            _ -> assertFailure "Expected constructor"
          _ -> assertFailure "Expected StmtClass"

  , testCase "Enums: pure and backed with cases" $ do
      let pureEnumSrc = "<?php enum Direction { case North; case South; case East; case West; }"
          backedEnumSrc = "<?php enum HttpStatus: int { case OK = 200; case NotFound = 404; }"
      case parseProgram "test.php" pureEnumSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtEnum _ ed]) -> do
          assertEqual "enum name" "Direction" (let Ident _ n = enumName ed in n)
          assertEqual "case count" 4 (length (enumMembers ed))
        other -> assertFailure ("Pure enum failed: " ++ show other)

      case parseProgram "test.php" backedEnumSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtEnum _ ed]) -> do
          assertEqual "enum name" "HttpStatus" (let Ident _ n = enumName ed in n)
          assertBool "backed type is int" (case enumBackedType ed of Just (SimpleType _ _) -> True; _ -> False)
          assertEqual "case count" 2 (length (enumMembers ed))
        other -> assertFailure ("Backed enum failed: " ++ show other)

  , testCase "Non-capturing catch statement" $ do
      let src = "<?php try { doWork(); } catch (NetworkException | TimeoutException) { logError(); }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtTry _ _ catches _] -> case catches of
            [c] -> do
              assertEqual "catch type count" 2 (length (catchTypes c))
              assertEqual "catch var is Nothing (non-capturing)" Nothing (catchVar c)
            _ -> assertFailure "Expected 1 catch"
          _ -> assertFailure "Expected StmtTry"

  , testCase "Namespaces and grouped use imports" $ do
      let bracketedSrc = "<?php namespace App\\Services { use App\\Core\\{Logger, Config as Cfg}; class Runner {} }"
          unbracketedSrc = "<?php namespace Vendor\\Package; use function App\\Utils\\{sanitize, escape};"
      assertParsesOk bracketedSrc
      assertParsesOk unbracketedSrc

  , testCase "Trait use adaptations (insteadof and as)" $ do
      let src = "<?php class Composed { use TraitA, TraitB { TraitA::smallTalk insteadof TraitB; TraitB::bigTalk as talk; TraitB::secret as private; } }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
          [MemberTraitUse tu] -> do
            assertEqual "trait names" 2 (length (traitUseNames tu))
            assertEqual "trait adaptations" 3 (length (traitUseAdaptations tu))
          _ -> assertFailure "Expected MemberTraitUse"
        other -> assertFailure ("Trait adaptations failed: " ++ show other)

  , testCase "Inline HTML and PHP short echo tags" $ do
      let src = "<html><body><?= $title ?><div><?php echo 'content'; ?></div></body></html>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> do
          assertBool "Has inline HTML and statements" (length stmts >= 3)
          case stmts of
            StmtInlineHtml _ txt : _ -> assertEqual "starts with html" "<html><body>" txt
            other -> assertFailure ("Expected leading html, got: " ++ show other)

  , testCase "Preserves comments and docblocks in trivia" $ do
      let src = "<?php /** PHPDoc for Service */ class Service {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program (Annotated _ triv) _) -> do
          assertBool "trivia contains DocBlock" (any (\case DocBlock _ -> True; _ -> False) triv)

  , testCase "Parse errors report precise source spans" $ do
      let src = "<?php class Invalid { public string ; }"
      case parseProgram "test.php" src of
        Right _ -> assertFailure "Expected parse error on invalid syntax"
        Left err -> do
          let sp = errorSpan err
          assertEqual "error line" 1 (posLine (spanStart sp))
          assertBool "error column > 0" (posColumn (spanStart sp) > 0)

  , testCase "Foreach by-reference without key: foreach ($arr as &$val)" $ do
      let src = "<?php foreach ($items as &$item) { $item *= 2; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtForeach _ _ Nothing _ True _]) -> pure ()
        other -> assertFailure ("Unexpected foreach AST: " ++ show other)

  , testCase "Class constant with final before visibility: final public const A = 1;" $ do
      let src = "<?php class Foo { final public const A = 1; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtClass _ cd] -> case classMembers cd of
            [MemberConst (ConstDecl _ _ vis isFinal _ items)] -> do
              assertEqual "vis is Public" (Just Public) vis
              assertEqual "isFinal is True" True isFinal
              assertEqual "items count" 1 (length items)
            other -> assertFailure ("Expected MemberConst, got: " ++ show other)
          _ -> assertFailure "Expected StmtClass"

  , testCase "Class constant modifier order permutations" $ do
      let src = "<?php class Foo { final public const A = 1; public final const B = 2; final protected const int C = 3; final private const ?string D = 'd'; final const E = 5; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
          [ MemberConst (ConstDecl _ _ (Just Public) True Nothing _)
            , MemberConst (ConstDecl _ _ (Just Public) True Nothing _)
            , MemberConst (ConstDecl _ _ (Just Protected) True (Just (SimpleType _ _)) _)
            , MemberConst (ConstDecl _ _ (Just Private) True (Just (NullableType _ _)) _)
            , MemberConst (ConstDecl _ _ Nothing True Nothing _)
            ] -> pure ()
          members -> assertFailure ("Unexpected class members: " ++ show (length members))
        other -> assertFailure ("Unexpected program AST: " ++ show other)

  , testCase "If statement with else clause parses correctly" $ do
      let src = "<?php if (1) {} else {}"
      assertParsesOk src

  , testCase "Issue 17 reproducer: parseProgram with if (true) { echo 1; } else { echo 2; }" $ do
      let src = "<?php if (true) { echo 1; } else { echo 2; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtIf _ _ [StmtEcho _ [ExprLit _ (LitInt _ 1 "1")]] [] (Just [StmtEcho _ [ExprLit _ (LitInt _ 2 "2")]])] ->
            pure ()
          other -> assertFailure ("Unexpected if AST: " ++ show other)

  , testCase "If statement with elseif and else clauses parses correctly" $ do
      let src = "<?php if ($a) { echo 1; } elseif ($b) { echo 2; } else { echo 3; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtIf _ _ [_] [(_, [_])] (Just [_])] -> pure ()
          other -> assertFailure ("Unexpected if AST: " ++ show other)

  , testCase "If statement with spaced else if and else clauses parses correctly" $ do
      let src = "<?php if ($a) { echo 1; } else if ($b) { echo 2; } else { echo 3; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> case stmts of
          [StmtIf _ _ [_] [(_, [_])] (Just [_])] -> pure ()
          other -> assertFailure ("Unexpected if AST: " ++ show other)
  ]

assertParsesOk :: Text -> Assertion
assertParsesOk src = case parseProgram "test.php" src of
  Left err -> assertFailure (show (formatParseError err))
  Right _ -> pure ()
