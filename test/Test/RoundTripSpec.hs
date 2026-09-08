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

  , testCase "Round-trip variable-variables" $ do
      let src = "<?php\n$$var = 1;\n$$$nested = 2;\n"
      assertRoundTrips src

  , testCase "Round-trip callable attributes on closures and arrow functions" $ do
      let src = "<?php\n$f = #[Test]\nfn (#[SensitiveParameter]\n$pass) => $pass;\n$g = #[Inline]\nfunction (#[SensitiveParameter]\n$pass) {\n};\n"
      assertRoundTrips src

  , testCase "Round-trip grouped use imports with trailing comma" $ do
      let src = "<?php\nuse Foo\\{Bar, Baz,};\n"
      assertRoundTrips src

  , testCase "Round-trip attribute groups with trailing comma" $ do
      let src = "<?php\n#[Attr1, Attr2,]\nclass Foo {\n    #[Attr,]\n    public int $x;\n}\n"
      assertRoundTrips src

  , testCase "Round-trip variable property fetch and method calls (Issue #30)" $ do
      let src = "<?php\n$val = $obj->$prop;\n$res = $obj->$method();\n$opt = $obj?->$prop;\n$optRes = $obj?->$method();\n"
      assertRoundTrips src

  , testProperty "Arbitrary generated simple expressions round-trip cleanly" $
      forAll genSimpleExpr $ \origExpr ->
        let printed = prettyPrintExpr origExpr
        in case parseExpression "gen.php" printed of
             Left err -> counterexample ("Failed to parse printed: " ++ T.unpack printed ++ "\nError: " ++ show err) False
             Right reParsed ->
               counterexample ("Printed: " ++ T.unpack printed)
                 (stripAnnotations origExpr == stripAnnotations reParsed)

  , testProperty "Arbitrary generated statements round-trip cleanly" $
      forAll genSimpleStmt $ \origStmt ->
        let printed = prettyPrintStmt origStmt
        in case parseStatement "gen.php" printed of
             Left err -> counterexample ("Failed to parse printed stmt: " ++ T.unpack printed ++ "\nError: " ++ show err) False
             Right reParsed ->
               counterexample ("Printed stmt: " ++ T.unpack printed)
                 (stripAnnotations origStmt == stripAnnotations reParsed)
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
      , pure (ExprLit () (LitFloat () 3.14 "3.14"))
      , pure (ExprLit () (LitString () "hello" "'hello'"))
      , pure (ExprLit () (LitBool () True))
      , pure (ExprLit () (LitNull ()))
      , pure (ExprVar () (SimpleVar () (VarName () "x")))
      , pure (ExprVar () (SimpleVar () (VarName () "item")))
      ]
  | otherwise = oneof
      [ pure (ExprLit () (LitInt () 42 "42"))
      , pure (ExprLit () (LitFloat () 3.14 "3.14"))
      , pure (ExprVar () (SimpleVar () (VarName () "item")))
      , do
          e1 <- genExprSized (n `div` 2)
          e2 <- genExprSized (n `div` 2)
          op <- elements
            [ OpAdd, OpSub, OpMul, OpDiv, OpMod, OpConcat, OpPipe
            , OpBitAnd, OpBitOr, OpBitXor, OpEq, OpIdentical, OpNotEq
            , OpLt, OpLte, OpGt, OpGte, OpSpaceship, OpBoolAnd, OpBoolOr
            ]
          pure (ExprBinary () op e1 e2)
      , do
          e <- genExprSized (n - 1)
          op <- elements [OpBoolNot, OpBitNot, OpUnaryMinus, OpUnaryPlus]
          pure (ExprUnary () op e)
      , do
          cond <- genExprSized (n `div` 3)
          t <- genExprSized (n `div` 3)
          f <- genExprSized (n `div` 3)
          pure (ExprTernary () cond (Just t) f)
      , do
          cond <- genExprSized (n `div` 2)
          f <- genExprSized (n `div` 2)
          pure (ExprTernary () cond Nothing f)
      , do
          e1 <- genExprSized (n `div` 2)
          e2 <- genExprSized (n `div` 2)
          pure (ExprNullCoalesce () e1 e2)
      , do
          ct <- elements [CastInt, CastFloat, CastString, CastBool, CastArray]
          e <- genExprSized (n - 1)
          pure (ExprCast () ct e)
      , do
          items <- listOf1 (ArrayItem () Nothing <$> genExprSized (n `div` 2) <*> pure False)
          pure (ExprArray () (take 3 items))
      , do
          arr <- genExprSized (n `div` 2)
          idx <- genExprSized (n `div` 2)
          pure (ExprArrayAccess () arr (Just idx))
      ]

genSimpleStmt :: Gen (Stmt ())
genSimpleStmt = sized genStmtSized

genStmtSized :: Int -> Gen (Stmt ())
genStmtSized n
  | n <= 0 = oneof
      [ StmtExpr () <$> genExprSized 0
      , StmtReturn () <$> oneof [pure Nothing, Just <$> genExprSized 0]
      , pure (StmtBreak () Nothing)
      , pure (StmtContinue () Nothing)
      ]
  | otherwise = oneof
      [ StmtExpr () <$> genExprSized 1
      , StmtReturn () <$> (Just <$> genExprSized 1)
      , do
          cond <- genExprSized 1
          thens <- listOf1 (genStmtSized (n `div` 2))
          pure (StmtIf () cond (take 2 thens) [] Nothing)
      , do
          cond <- genExprSized 1
          body <- listOf1 (genStmtSized (n `div` 2))
          pure (StmtWhile () cond (take 2 body))
      , do
          arr <- genExprSized 1
          val <- genExprSized 1
          body <- listOf1 (genStmtSized (n `div` 2))
          pure (StmtForeach () arr Nothing val False (take 2 body))
      , do
          stmts <- listOf1 (genStmtSized (n `div` 2))
          pure (StmtBlock () (take 3 stmts))
      ]
