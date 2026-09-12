{-# LANGUAGE OverloadedStrings #-}

module Test.RoundTripSpec (roundTripTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad (forM_)
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

  , testCase "Round-trip match arm with trailing comma in condition list (Issue #115)" $ do
      let src = "<?php\n$res = match ($val) {\n    1, 2, => 'low',\n    default => 'high'\n};"
      assertRoundTrips src

  , testCase "Round-trip PHP 8.4 property hooks" $ do
      let src = "<?php\nclass Hooked {\n    public string $name {\n        get => $this->raw;\n        set(string $v) {\n            $this->raw = $v;\n        }\n    }\n}"
      assertRoundTrips src

  , testCase "Round-trip attributed and by-reference property hooks (Issue #116)" $ do
      let src = "<?php\nclass Hooked {\n    public string $name {\n        #[Example]\n        get => $this->raw;\n        final &get => $this->raw;\n    }\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right prog -> do
          let printed = prettyPrint prog
          assertBool "prettyPrint should retain hook attributes" ("#[Example]" `T.isInfixOf` printed)
          assertBool "prettyPrint should retain by-reference get" ("&get" `T.isInfixOf` printed)
          case parseProgram "test.php" printed of
            Left err -> assertFailure ("reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
            Right prog2 ->
              assertEqual "round-trip AST equal" (stripAnnotations prog) (stripAnnotations prog2)

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

  , testCase "Round-trip late static binding expressions (Issue #19)" $ do
      assertRoundTrips "<?php static::bar();"
      assertRoundTrips "<?php static::$foo;"
      assertRoundTrips "<?php static::CONSTANT;"
      assertRoundTrips "<?php static::class;"
      assertRoundTrips "<?php $x = new static();"
      assertRoundTrips "<?php $x = new static;"

  , testCase "Round-trip relative qualified names (Issue #18)" $ do
      assertRoundTrips "<?php namespace\\Foo;"
      assertRoundTrips "<?php namespace\\Foo\\Bar;"
      assertRoundTrips "<?php namespace\\Foo::bar();"
      assertRoundTrips "<?php namespace\\func();"
      assertRoundTrips "<?php namespace\\MY_CONST;"
      assertRoundTrips "<?php namespace\\namespace;"

  , testCase "Round-trip language construct expressions (Issue #20)" $ do
      assertRoundTrips "<?php isset($x);"
      assertRoundTrips "<?php isset($x, $y);"
      assertRoundTrips "<?php empty($x);"
      assertRoundTrips "<?php eval('$a = 1;');"
      assertRoundTrips "<?php include 'file.php';"
      assertRoundTrips "<?php include_once 'file.php';"
      assertRoundTrips "<?php require 'file.php';"
      assertRoundTrips "<?php require_once 'file.php';"
      assertRoundTrips "<?php $a = (isset($x) && !empty($y));"

  , testCase "Round-trip print language construct expressions (Issue #123)" $ do
      assertRoundTrips "<?php print 'hello';"
      assertRoundTrips "<?php $x = print 1;"
      assertRoundTrips "<?php $ok ? print 'yes' : print 'no';"
      assertRoundTrips "<?php print 1 and $ok;"

  , testCase "Round-trip exit and die language constructs (Issue #124)" $ do
      assertRoundTrips "<?php exit;"
      assertRoundTrips "<?php exit();"
      assertRoundTrips "<?php exit(0);"
      assertRoundTrips "<?php exit('msg');"
      assertRoundTrips "<?php die;"
      assertRoundTrips "<?php die();"
      assertRoundTrips "<?php die(1);"
      assertRoundTrips "<?php die(\"err\");"
      assertRoundTrips "<?php $x = exit(1);"
      assertRoundTrips "<?php $file or die('fail');"
      assertRoundTrips "<?php $ok ? exit(0) : die(1);"

  , testCase "Round-trip list(...) destructuring constructs (Issue #125)" $ do
      assertRoundTrips "<?php list($a, $b) = $arr;"
      assertRoundTrips "<?php list(\"k\" => $v) = $arr;"
      assertRoundTrips "<?php list($a, list($b, $c)) = $arr;"
      assertRoundTrips "<?php list($a, , $b) = $arr;"
      assertRoundTrips "<?php list(, $b) = $arr;"
      assertRoundTrips "<?php list(, , $c) = $arr;"
      assertRoundTrips "<?php foreach ($arr as list($a, $b)) {}"
      assertRoundTrips "<?php foreach ($arr as $k => list($a, $b)) {}"
      assertRoundTrips "<?php foreach ($arr as list($a, list($b, $c))): echo $a; endforeach;"

  , testCase "Round-trip array destructuring constructs with omitted elements (Issue #126)" $ do
      assertRoundTrips "<?php [$a, , $b] = $arr;"
      assertRoundTrips "<?php [, $b] = $arr;"
      assertRoundTrips "<?php [, , $c] = $arr;"
      assertRoundTrips "<?php [$a, , $c, , $e] = $arr;"
      assertRoundTrips "<?php foreach ($arr as [$first, , $third]) {}"
      assertRoundTrips "<?php foreach ($arr as $k => [$first, , $third]) {}"
      assertRoundTrips "<?php foreach ($arr as [, $b]) {}"
      assertRoundTrips "<?php foreach ($arr as [$a, [$b, , $c]]): echo $a; endforeach;"

  , testCase "Round-trip yield, arrow function, and throw in postfix positions (Issue #22)" $ do
      let yieldX = ExprYield () Nothing (Just (ExprVar () (SimpleVar () (VarName () "x"))))
          arrow = ExprArrowFunction () [] False False [] Nothing (ExprVar () (SimpleVar () (VarName () "x")))
          throwE = ExprThrow () (ExprVar () (SimpleVar () (VarName () "e")))
          yieldFromX = ExprYieldFrom () (ExprVar () (SimpleVar () (VarName () "x")))
          prop = MemberIdent (Ident () "prop")
          idx = Just (ExprLit () (LitInt () 0 "0"))
          contexts =
            [ ("yield property fetch", ExprPropertyFetch () yieldX prop)
            , ("yield method call", ExprMethodCall () yieldX prop (ArgsList []))
            , ("yield nullsafe property fetch", ExprNullsafePropertyFetch () yieldX prop)
            , ("yield nullsafe method call", ExprNullsafeMethodCall () yieldX prop (ArgsList []))
            , ("yield array access", ExprArrayAccess () yieldX idx)
            , ("yield from property fetch", ExprPropertyFetch () yieldFromX prop)
            , ("arrow function call", ExprCall () arrow (ArgsList []))
            , ("arrow property fetch", ExprPropertyFetch () arrow prop)
            , ("throw property fetch", ExprPropertyFetch () throwE prop)
            , ("throw array access", ExprArrayAccess () throwE idx)
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip ExprAssign nested in composite expressions (Issue #12)" $ do
      let assign = ExprAssign () Nothing (ExprVar () (SimpleVar () (VarName () "y")))
                     (ExprLit () (LitInt () 1 "1"))
          litTwo = ExprLit () (LitInt () 2 "2")
          contexts =
            [ ("binary lhs", ExprBinary () OpAdd assign litTwo)
            , ("binary rhs", ExprBinary () OpAdd litTwo assign)
            , ("unary operand", ExprUnary () OpBoolNot assign)
            , ("ternary condition", ExprTernary () assign (Just litTwo) litTwo)
            , ("coalesce lhs", ExprNullCoalesce () assign litTwo)
            , ("coalesce rhs", ExprNullCoalesce () litTwo assign)
            , ("cast operand", ExprCast () CastInt assign)
            , ("clone operand", ExprClone () assign Nothing)
            , ("assignment lhs", ExprAssign () Nothing assign litTwo)
            , ("assignment rhs", ExprAssign () Nothing litTwo assign)
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip by-reference assignment nested in composite expressions (Issue #138)" $ do
      let refAssign = ExprAssignRef () (ExprVar () (SimpleVar () (VarName () "a")))
                        (ExprVar () (SimpleVar () (VarName () "b")))
          litTwo = ExprLit () (LitInt () 2 "2")
          contexts =
            [ ("bare", refAssign)
            , ("binary lhs", ExprBinary () OpAdd refAssign litTwo)
            , ("binary rhs", ExprBinary () OpAdd litTwo refAssign)
            , ("unary operand", ExprUnary () OpBoolNot refAssign)
            , ("ternary condition", ExprTernary () refAssign (Just litTwo) litTwo)
            , ("cast operand", ExprCast () CastInt refAssign)
            , ("assignment rhs", ExprAssign () Nothing litTwo refAssign)
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip postfix operators on include expressions (Issue #67)" $ do
      let lit = ExprLit () (LitString () "f.php" "'f.php'")
          inc t = ExprInclude () t lit
          prop = MemberIdent (Ident () "prop")
          idx = Just (ExprLit () (LitInt () 0 "0"))
          contexts =
            [ ("post-increment of include", ExprUnary () OpPostInc (inc IncInclude))
            , ("post-decrement of include", ExprUnary () OpPostDec (inc IncInclude))
            , ("post-increment of include_once", ExprUnary () OpPostInc (inc IncIncludeOnce))
            , ("post-increment of require", ExprUnary () OpPostInc (inc IncRequire))
            , ("post-increment of require_once", ExprUnary () OpPostInc (inc IncRequireOnce))
            , ("property fetch on include", ExprPropertyFetch () (inc IncInclude) prop)
            , ("array access on include", ExprArrayAccess () (inc IncInclude) idx)
            , ("call on include", ExprCall () (inc IncInclude) (ArgsList []))
            , ("top-level include", inc IncInclude)
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip post-increment and post-decrement of cast expressions (Issue #23)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          varS = ExprVar () (SimpleVar () (VarName () "s"))
          contexts =
            [ ("post-increment of int cast", ExprUnary () OpPostInc
                (ExprCast () CastInt varX))
            , ("post-decrement of string cast", ExprUnary () OpPostDec
                (ExprCast () CastString varS))
            , ("post-increment of clone", ExprUnary () OpPostInc
                (ExprClone () varX Nothing))
            , ("post-increment of pre-increment", ExprUnary () OpPostInc
                (ExprUnary () OpPreInc varX))
            , ("post-increment of throw", ExprUnary () OpPostInc
                (ExprThrow () varX))
            , ("post-increment of variable", ExprUnary () OpPostInc varX)
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "test.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip unary operands of exponentiation (Issue #45)" $ do
      let lit n = ExprLit () (LitInt () n (T.pack (show n)))
          contexts =
            [ ("negation of a power", ExprUnary () OpUnaryMinus
                (ExprBinary () OpPow (lit 2) (lit 2)))
            , ("power of a negated base", ExprBinary () OpPow
                (ExprUnary () OpUnaryMinus (lit 2)) (lit 2))
            , ("power of a cast base", ExprBinary () OpPow
                (ExprCast () CastInt (lit 2)) (lit 2))
            , ("cast of a power", ExprCast () CastInt
                (ExprBinary () OpPow (lit 2) (lit 2)))
            ]
      forM_ contexts $ \(name, ctx) -> do
        let printed = prettyPrintExpr ctx
        case parseExpression "pow.php" printed of
          Left err -> assertFailure (name ++ ": printed output does not parse: "
                                     ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
          Right reparsed ->
            assertEqual (name ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip declare, goto/label, and unset constructs (Issue #108)" $ do
      assertRoundTrips "<?php declare(strict_types=1); function f() {}"
      assertRoundTrips "<?php declare(ticks=1) { echo 'x'; }"
      assertRoundTrips "<?php goto end; end:"
      assertRoundTrips "<?php unset($a, $b['k']);"
      assertRoundTrips "<?php declare(ticks=1, encoding='UTF-8');"

  , testCase "Round-trip interpolated strings containing escaped double quotes (Issue #111)" $ do
      assertRoundTrips "<?php \"hello \\\"world\\\" $x\";"
      assertRoundTrips "<?php \"\\\"$x\";"
      assertRoundTrips "<?php \"\\\\\\\"$x\";"
      assertRoundTrips "<?php \"\\\"$x\\\"\";"
      assertRoundTrips "<?php \"a \\\" b \\$c \\\\ d $e\";"

  , testCase "Round-trip transformed closures and arrow functions with captures and parameters (Issue #109)" $ do
      let src = "<?php\n$f = function ($a) use ($b, &$c) {\n    return (($a + $b) + $c);\n};\n$g = fn ($x) => ($x + $y);\n"
      case parseProgram "test.php" src of
        Left err -> assertFailure ("Initial parse failed: " ++ show (formatParseError err))
        Right (Program ann stmts) -> do
          let transformed = Program ann (map (transformStmt (\case
                ExprVar a (SimpleVar sv (VarName vn "b")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newB"))
                ExprVar a (SimpleVar sv (VarName vn "c")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newC"))
                ExprVar a (SimpleVar sv (VarName vn "y")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newY"))
                e -> e)) stmts)
              printed = prettyPrint transformed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on transformed printed output:\n" ++ T.unpack printed ++ "\nError: " ++ show (formatParseError err2))
            Right reparsed ->
              assertEqual "Transformed AST round-trips cleanly"
                (stripAnnotations transformed)
                (stripAnnotations reparsed)

  , testCase "Round-trip transformed interpolated strings (Issue #122)" $ do
      let src = "<?php\n$msg = \"hello $name {$foo}\";\n"
      case parseProgram "test.php" src of
        Left err -> assertFailure ("Initial parse failed: " ++ show (formatParseError err))
        Right (Program ann stmts) -> do
          let transformed = Program ann (map (transformStmt (\case
                ExprVar a (SimpleVar sv (VarName vn "name")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedName"))
                ExprVar a (SimpleVar sv (VarName vn "foo")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedFoo"))
                e -> e)) stmts)
              printed = prettyPrint transformed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on transformed printed output:\n" ++ T.unpack printed ++ "\nError: " ++ show (formatParseError err2))
            Right reparsed ->
              assertEqual "Transformed AST round-trips cleanly"
                (stripAnnotations transformed)
                (stripAnnotations reparsed)

  , testCase "Round-trip yield, yield from, arrow function, throw, and include in operator operand positions (Issue #112)" $ do
      let varX = ExprVar () (SimpleVar () (VarName () "x"))
          litOne = ExprLit () (LitInt () 1 "1")
          litTwo = ExprLit () (LitInt () 2 "2")
          litThree = ExprLit () (LitInt () 3 "3")
          yieldExpr = ExprYield () Nothing (Just varX)
          yieldFromExpr = ExprYieldFrom () varX
          arrowExpr = ExprArrowFunction () [] False False [] Nothing varX
          throwExpr = ExprThrow () varX
          incExpr = ExprInclude () IncInclude (ExprLit () (LitString () "a.php" "'a.php'"))
          constructs = [ ("yield", yieldExpr)
                       , ("yield from", yieldFromExpr)
                       , ("arrow function", arrowExpr)
                       , ("throw", throwExpr)
                       , ("include", incExpr)
                       ]
      forM_ constructs $ \(cName, construct) -> do
        let contexts =
              [ ("binary lhs", ExprBinary () OpAdd construct litTwo)
              , ("binary rhs", ExprBinary () OpAdd litTwo construct)
              , ("unary operand", ExprUnary () OpBoolNot construct)
              , ("ternary condition", ExprTernary () construct (Just litTwo) litThree)
              , ("ternary then", ExprTernary () litOne (Just construct) litThree)
              , ("ternary else", ExprTernary () litOne (Just litTwo) construct)
              , ("coalesce lhs", ExprNullCoalesce () construct litTwo)
              , ("coalesce rhs", ExprNullCoalesce () litOne construct)
              ]
        forM_ contexts $ \(posName, ctx) -> do
          let testLabel = cName ++ " in " ++ posName
              printed = prettyPrintExpr ctx
          case parseExpression "test.php" printed of
            Left err -> assertFailure (testLabel ++ ": printed output does not parse: "
                                       ++ T.unpack printed ++ "\n" ++ show (formatParseError err))
            Right reparsed ->
              assertEqual (testLabel ++ ": AST preserved") (stripAnnotations ctx) (stripAnnotations reparsed)

  , testCase "Round-trip calls with leading argument unpacking (Issue #137)" $ do
      assertRoundTrips "<?php foo(...$args);"
      assertRoundTrips "<?php $obj->m(...$args);"
      assertRoundTrips "<?php Foo::m(...$args);"
      assertRoundTrips "<?php foo(...$args, name: 1);"
      assertRoundTrips "<?php strlen(...);"
      assertRoundTrips "<?php $obj->method(...);"
      assertRoundTrips "<?php Config::load(...);"

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
          items <- listOf1 (ArrayItem () Nothing <$> genExprSized (n `div` 2) <*> pure False <*> pure False)
          pure (ExprArray () (take 3 items))
      , do
          arr <- genExprSized (n `div` 2)
          idx <- genExprSized (n `div` 2)
          pure (ExprArrayAccess () arr (Just idx))
      , do
          lhs <- genExprSized (n `div` 2)
          rhs <- genExprSized (n `div` 2)
          mOp <- elements [Nothing, Just OpAdd, Just OpConcat, Just OpCoalesce]
          pure (ExprAssign () mOp lhs rhs)
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
      , pure (StmtGoto () (Ident () "label1"))
      , pure (StmtLabel () (Ident () "label1"))
      , pure (StmtUnset () [ExprVar () (SimpleVar () (VarName () "x"))])
      , pure (StmtDeclare () [DeclareDirective () (Ident () "strict_types") (LitInt () 1 "1")] Nothing)
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
