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

  , testCase "allExprs, allVariables, and transformExpr traverse print (Issue #123)" $ do
      case parseExpression "test.php" "print $value + $other" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Variables inside print" ["value", "other"] (allVariables expr)
          assertEqual "Counts print and its operand expressions" 4 (length (allExprs expr))
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "value")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Renames variables inside print" ["renamed", "other"] (allVariables transformed)
          assertEqual "Pretty prints transformed print expression"
            "print ($renamed + $other)"
            (prettyPrintExpr transformed)

  , testCase "allExprs, allVariables, and transformExpr traverse exit status (Issue #124)" $ do
      case parseExpression "test.php" "die($value . $other)" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Variables inside die" ["value", "other"] (allVariables expr)
          assertEqual "Counts die and its status expressions" 4 (length (allExprs expr))
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "value")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Renames variables inside die" ["renamed", "other"] (allVariables transformed)
          assertEqual "Pretty prints transformed die expression"
            "die(($renamed . $other))"
            (prettyPrintExpr transformed)

      case parseExpression "test.php" "exit" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Statusless exit holds no variables" [] (allVariables expr)
          assertEqual "Counts the statusless exit alone" 1 (length (allExprs expr))

  , testCase "allExprs, allVariables, and transformExpr traverse ExprList (Issue #125)" $ do
      case parseExpression "test.php" "list($a, list($b, $c), \"key\" => $d)" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Variables inside nested list" ["a", "b", "c", "d"] (allVariables expr)
          assertEqual "Counts list and its subexpressions" 7 (length (allExprs expr))
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "b")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Renames variable inside nested list" ["a", "renamed", "c", "d"] (allVariables transformed)

      case parseExpression "test.php" "list($a, , $b)" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Omitted slots don't introduce variables" ["a", "b"] (allVariables expr)
          assertEqual "Counts list and non-empty items" 3 (length (allExprs expr))
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "a")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newA"))
                e -> e) expr
          assertEqual "Renames variables in list with omitted slots" ["newA", "b"] (allVariables transformed)
          assertEqual "Pretty prints list with omitted slot"
            "list($newA, , $b)"
            (prettyPrintExpr transformed)

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

  , testCase "foldStmt visits statements inside closures in expressions (Issue #142)" $ do
      assertFoldsStatementKinds
        "<?php $f = function() { echo 1; return 2; };"
        ["StmtExpr", "StmtEcho", "StmtReturn"]

  , testCase "foldStmt visits statements inside anonymous classes in expressions (Issue #142)" $ do
      assertFoldsStatementKinds
        "<?php $x = new class { function f() { echo 1; return 2; } };"
        ["StmtExpr", "StmtEcho", "StmtReturn"]

  , testCase "foldStmt follows nested expression paths without duplicates (Issue #142)" $ do
      assertFoldsStatementKinds
        "<?php function outer() { return function() { echo new class { function inner() { return 1; } }; return 2; }; }"
        ["StmtFunction", "StmtReturn", "StmtEcho", "StmtReturn", "StmtReturn"]

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

  , testCase "allExprs matches issue #52 reproducer exactly" $ do
      let src = "#[Attr(1 + 2)] fn() => $y"
      case parseExpression "attrs.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr ->
          assertEqual "All expressions printed match expected sequence"
            ["#[Attr((1 + 2))]\nfn () => $y", "(1 + 2)", "1", "2", "$y"]
            (map prettyPrintExpr (allExprs expr))

  , testCase "allExprs visits expressions inside attributes (Issue #52)" $ do
      let src = "#[Attr(1 + 2)] fn(#[ParamAttr($x)] $p) => $y"
      case parseExpression "attrs.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let printed = map prettyPrintExpr (allExprs expr)
          assertBool "Contains attribute argument expression" ("(1 + 2)" `elem` printed)
          assertEqual "Extracts all variables from attributes and body"
            ["x", "y"]
            (allVariables expr)

  , testCase "transformExpr rewrites expressions inside attributes (Issue #52)" $ do
      let src = "#[Attr($old)] fn(#[ParamAttr($old)] $p) => $old"
      case parseExpression "attrs.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "old")) ->
                  ExprVar a (SimpleVar sv (VarName vn "new"))
                e -> e) expr
          assertEqual "All variables renamed inside attributes and body"
            ["new", "new", "new"]
            (allVariables renamed)

  , testCase "allExprs visits expressions in anonymous class attributes (Issue #52)" $ do
      let src = "new #[ClassAttr($a)] class($b) { #[PropAttr($c)] public $prop; }"
      case parseExpression "attrs.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts variables from anon class attributes, args, and member attributes"
            ["a", "b", "c"]
            (allVariables expr)

  , testCase "queryStmt and transformStmt traverse expressions in declaration attributes (Issue #52)" $ do
      let src = "<?php #[FuncAttr($a)] function test(#[ParamAttr($b)] $p) { return $c; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [stmt]) -> do
          let varsInStmt = queryStmt (\case
                ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
                _ -> []) stmt
          assertEqual "Extracts variables from function declaration attributes"
            ["a", "b", "c"]
            varsInStmt
          let transformed = transformStmt (\case
                ExprVar a (SimpleVar sv (VarName vn "a")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedA"))
                e -> e) stmt
          let varsAfter = queryStmt (\case
                ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
                _ -> []) transformed
          assertEqual "Variables renamed in statement attributes"
            ["renamedA", "b", "c"]
            varsAfter
        Right other -> assertFailure ("Expected one statement, got: " ++ show other)

  , testCase "queryStmt and transformStmt traverse property hook attributes (Issue #116)" $ do
      let src = "<?php class C { public int $x { #[HookAttr($old)] get => $value; } }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [stmt]) -> do
          let varsInStmt = queryStmt (\case
                ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
                _ -> []) stmt
          assertEqual "Extracts variables from hook attributes and body"
            ["old", "value"]
            varsInStmt
          let transformed = transformStmt (\case
                ExprVar a (SimpleVar sv (VarName vn "old")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedOld"))
                e -> e) stmt
          assertEqual "Variables renamed in hook attributes"
            ["renamedOld", "value"]
            (queryStmt (\case
              ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
              _ -> []) transformed)
        Right other -> assertFailure ("Expected one statement, got: " ++ show other)

  , testCase "allVariables and transformExpr handle closure use-clause variables (Issue #109)" $ do
      let src = "function () use ($fn) { return $fn(2); }"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts use-clause variable and body variable"
            ["fn", "fn"]
            (allVariables expr)
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "fn")) ->
                  ExprVar a (SimpleVar sv (VarName vn "zz_fn"))
                e -> e) expr
          assertEqual "Renamed use-clause and body variables"
            ["zz_fn", "zz_fn"]
            (allVariables renamed)
          assertEqual "Pretty printed closure preserves renamed capture binding"
            "function () use ($zz_fn) {\n    return $zz_fn(2);\n}"
            (prettyPrintExpr renamed)

  , testCase "transformExpr preserves by-ref flag on closure captures (Issue #109)" $ do
      let src = "function ($arg) use ($val, &$ref) { return $arg + $val + $ref; }"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts use-clause captures and body variables"
            ["val", "ref", "arg", "val", "ref"]
            (allVariables expr)
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "ref")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newRef"))
                ExprVar a (SimpleVar sv (VarName vn "val")) ->
                  ExprVar a (SimpleVar sv (VarName vn "newVal"))
                e -> e) expr
          assertEqual "Renamed variables retain use and body occurrences"
            ["newVal", "newRef", "arg", "newVal", "newRef"]
            (allVariables renamed)
          assertEqual "Pretty printed closure preserves by-ref ampersand on renamed capture"
            "function ($arg) use ($newVal, &$newRef) {\n    return (($arg + $newVal) + $newRef);\n}"
            (prettyPrintExpr renamed)

  , testCase "transformExpr and allVariables on arrow functions with outer variables (Issue #109)" $ do
      let src = "fn($param) => $param + $outer"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Arrow function reports body variable occurrences"
            ["param", "outer"]
            (allVariables expr)
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "outer")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedOuter"))
                e -> e) expr
          assertEqual "Renamed outer variable in arrow function body"
            ["param", "renamedOuter"]
            (allVariables renamed)
          assertEqual "Pretty printed arrow function output"
            "fn ($param) => ($param + $renamedOuter)"
            (prettyPrintExpr renamed)

  , testCase "allVariables, allExprs, and transformExpr traverse interpolated strings (Issue #122)" $ do
      let src = "\"hello $name {$foo}\""
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts variables inside interpolated string"
            ["name", "foo"]
            (allVariables expr)
          assertEqual "Extracts all expressions (outer literal and inner expressions)"
            3
            (length (allExprs expr))
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "name")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Renamed variables inside interpolated string"
            ["renamed", "foo"]
            (allVariables renamed)
          assertEqual "Pretty printed transformed interpolated string"
            "\"hello {$renamed} {$foo}\""
            (prettyPrintExpr renamed)

  , testCase "allVariables and transformExpr handle complex expressions in interpolated strings (Issue #122)" $ do
      let src = "\"prefix {$user->name} {$calc($a + $b)} suffix\""
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts all variables from complex expressions in string"
            ["user", "calc", "a", "b"]
            (allVariables expr)
          let renamed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "a")) ->
                  ExprVar a (SimpleVar sv (VarName vn "alpha"))
                e -> e) expr
          assertEqual "Renamed nested variable inside interpolated string expression"
            ["user", "calc", "alpha", "b"]
            (allVariables renamed)

  , testCase "queryStmt and transformStmt traverse interpolated strings in statements (Issue #122)" $ do
      let src = "<?php echo \"Hello, $user! Welcome to {$site->title}.\";"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [stmt]) -> do
          let vars = queryStmt (\case
                ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
                _ -> []) stmt
          assertEqual "Extracts variables from interpolated string in echo statement"
            ["user", "site"]
            vars
          let transformed = transformStmt (\case
                ExprVar a (SimpleVar sv (VarName vn "user")) ->
                  ExprVar a (SimpleVar sv (VarName vn "guest"))
                e -> e) stmt
          let varsAfter = queryStmt (\case
                ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
                _ -> []) transformed
          assertEqual "Variables renamed in echo statement interpolated string"
            ["guest", "site"]
            varsAfter
        Right other -> assertFailure ("Expected one statement, got: " ++ show other)

  , testCase "non-interpolated literals remain atomic terminal nodes (Issue #122)" $ do
      let src = "42 + 3.14 + 'plain string' + true + null"
      case parseExpression "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Non-interpolated literals have no variables"
            []
            (allVariables expr)
          let exprCount = length (allExprs expr)
          assertBool "Counts outer and literal expressions" (exprCount > 0)

  , testCase "allExprs, allVariables, and transformExpr traverse by-reference assignment (Issue #138)" $ do
      case parseExpression "test.php" "$target =& $source" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Variables on both sides" ["target", "source"] (allVariables expr)
          assertEqual "Counts the assignment and both operands" 3 (length (allExprs expr))
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "source")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamed"))
                e -> e) expr
          assertEqual "Transformed source variable" ["target", "renamed"] (allVariables transformed)

  , testCase "allExprs, allVariables, and transformExpr traverse by-reference array items (Issue #139)" $ do
      case parseExpression "test.php" "[$x, &$y, 'k' => &$z]" of
        Left err -> assertFailure (show (formatParseError err))
        Right expr -> do
          assertEqual "Extracts all variables including by-reference items" ["x", "y", "z"] (allVariables expr)
          let transformed = transformExpr (\case
                ExprVar a (SimpleVar sv (VarName vn "y")) ->
                  ExprVar a (SimpleVar sv (VarName vn "renamedY"))
                e -> e) expr
          assertEqual "Transformed by-ref variable in array item" ["x", "renamedY", "z"] (allVariables transformed)
          case transformed of
            ExprArray _ [_, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "renamedY"))) False True, _] -> pure ()
            other -> assertFailure ("Expected transformed by-reference array item, got: " ++ show other)
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

assertFoldsStatementKinds :: Text -> [String] -> IO ()
assertFoldsStatementKinds src expected =
  case parseProgram "test.php" src of
    Left err -> assertFailure (show (formatParseError err))
    Right (Program _ [stmt]) ->
      assertEqual "statements reachable from expression" expected (foldStatementKinds stmt)
    Right other -> assertFailure ("Expected one statement, got: " ++ show other)

foldStatementKinds :: Stmt a -> [String]
foldStatementKinds = foldStmt (\stmt -> [statementKind stmt])

statementKind :: Stmt a -> String
statementKind = \case
  StmtExpr {} -> "StmtExpr"
  StmtEcho {} -> "StmtEcho"
  StmtReturn {} -> "StmtReturn"
  StmtFunction {} -> "StmtFunction"
  _ -> "Other"
