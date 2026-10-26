module Test.QualifiedNameSpec (qualifiedNameTests) where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Language.PHP

qualifiedNameTests :: TestTree
qualifiedNameTests = testGroup "Qualified name identity"
  [ testGroup "name forms across contexts"
      [ testGroup (T.unpack source)
          [ testCase "expression" $
              case parseExpression "test.php" source of
                Right (ExprConstFetch _ qn) -> stripAnnotations qn @?= expected
                result -> assertFailure (show result)
          , testCase "parameter and return types" $
              case parseProgram "test.php"
                ("<?php function f(" <> source <> " $x): " <> source <> " {}") of
                Right (Program _ [StmtFunction _ fn]) -> do
                  case funcParams fn of
                    [p] -> fmap stripAnnotations (paramType p) @?= Just (SimpleType () expected)
                    _ -> assertFailure "Expected one parameter"
                  fmap stripAnnotations (funcReturnType fn) @?= Just (SimpleType () expected)
                result -> assertFailure (show result)
          ]
      | (source, expected) <-
          [ ("Foo", QualifiedName () NameUnqualified ["Foo"])
          , ("Foo\\bAr", QualifiedName () NameQualified ["Foo", "bAr"])
          , ("\\Foo\\bAr", QualifiedName () NameFullyQualified ["Foo", "bAr"])
          , ("namespace\\Foo", QualifiedName () NameRelative ["Foo"])
          , ("Namespace\\Foo", QualifiedName () NameRelative ["Foo"])
          , ("NAMESPACE\\Foo", QualifiedName () NameRelative ["Foo"])
          , ("nAmEsPaCe\\Foo\\bAr", QualifiedName () NameRelative ["Foo", "bAr"])
          , ("\\namespace\\Foo", QualifiedName () NameFullyQualified ["namespace", "Foo"])
          , ("\\NAMESPACE\\Foo", QualifiedName () NameFullyQualified ["NAMESPACE", "Foo"])
          , ("namespace\\namespace", QualifiedName () NameRelative ["namespace"])
          ]
      ]
  , testGroup "fully qualified namespace segments survive printing"
      [ testCase (T.unpack source) $ do
          case parseExpression "test.php" source of
            Right expr -> prettyPrintExpr expr @?= source
            Left err -> assertFailure (show err)
          let program = "<?php function f(" <> source <> " $x): " <> source <> " {}"
          case parseProgram "test.php" program of
            Right ast -> do
              assertBool "Both type names retain the namespace segment"
                (T.count source (prettyPrint ast) == 2)
              case parseProgram "test.php" (prettyPrint ast) of
                Right reparsed -> stripAnnotations reparsed @?= stripAnnotations ast
                Left err -> assertFailure (show err)
            Left err -> assertFailure (show err)
      | source <- ["\\namespace\\Foo", "\\NAMESPACE\\Foo"]
      ]
  , testCase "constructed relative parameter type round-trips" $ do
      let ty = SimpleType () (QualifiedName () NameRelative ["Foo", "bAr"])
          param = Param () [] Nothing Nothing False False (Just ty) False False (VarName () "x") Nothing
          program = Program () [StmtFunction () (FunctionDecl () [] False (Ident () "f") [param] Nothing [])]
      case parseProgram "test.php" (prettyPrint program) of
        Right reparsed -> stripAnnotations reparsed @?= program
        Left err -> assertFailure (show err)
  , testGroup "type keyword policy"
      [ testCase (T.unpack source) $
          case parseProgram "test.php" ("<?php function f(" <> source <> " $x): " <> source <> " {}") of
            Right (Program _ [StmtFunction _ fn]) -> do
              let expected = Just (SimpleType () (QualifiedName () NameUnqualified [source]))
              map (fmap stripAnnotations . paramType) (funcParams fn) @?= [expected]
              fmap stripAnnotations (funcReturnType fn) @?= expected
            result -> assertFailure (show result)
      | source <- ["array", "callable", "static", "ArRaY", "CALLABLE", "STATIC", "int", "self", "parent"]
      ]
  , testGroup "invalid type keywords"
      [ testCase (T.unpack source) $
          case parseProgram "test.php" ("<?php function f(" <> source <> " $x) {}") of
            Left _ -> pure ()
            Right ast -> assertFailure (show ast)
      | source <- ["var", "VAR", "class", "function", "\\var\\Foo", "var\\Foo"]
      ]
  , testGroup "unqualified expression keywords remain rejected"
      [ testCase (T.unpack source) $
          case parseExpression "test.php" source of
            Left _ -> pure ()
            Right ast -> assertFailure (show ast)
      | source <- ["class", "function", "var", "namespace", "callable", "array", "ArRaY"]
      ]
  , testCase "expression name spans and leading trivia exclude trailing comments" $
      case parseExpression "test.php" "/* lead */ namespace\\Foo /* tail */" of
        Right expr@(ExprConstFetch _ qn) -> do
          nodeSpan expr @?= Span (SourcePos "test.php" 1 12 11) (SourcePos "test.php" 1 25 24)
          nodeSpan qn @?= nodeSpan expr
          annTrivia (annotation expr) @?= [CommentBlock " lead "]
        result -> assertFailure (show result)
  , testCase "type name spans exclude trivia and keep parameter and return comments" $
      case parseProgram "test.php"
        "<?php function f(/* p */ namespace\\Foo /* gap */ $x): /* r */ namespace\\Bar /* end */ {}" of
        Right (Program _ [StmtFunction _ fn]) -> do
          case funcParams fn of
            [p] -> do
              annTrivia (annotation p) @?= [CommentBlock " p "]
              case paramType p of
                Just ty@(SimpleType _ qn) -> do
                  nodeSpan ty @?= Span (SourcePos "test.php" 1 26 25) (SourcePos "test.php" 1 39 38)
                  nodeSpan qn @?= nodeSpan ty
                result -> assertFailure (show result)
            result -> assertFailure (show result)
          case funcReturnType fn of
            Just ty@(SimpleType _ qn) -> do
              nodeSpan ty @?= Span (SourcePos "test.php" 1 63 62) (SourcePos "test.php" 1 76 75)
              nodeSpan qn @?= nodeSpan ty
              annTrivia (annotation ty) @?= [CommentBlock " r "]
            result -> assertFailure (show result)
        result -> assertFailure (show result)
  ]
