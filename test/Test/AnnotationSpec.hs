module Test.AnnotationSpec (annotationTests) where

import qualified Data.Text as T
import Language.PHP
import Test.Tasty
import Test.Tasty.HUnit

annotationTests :: TestTree
annotationTests = testGroup "Public annotations"
  [ testCase "statement annotation is the root, not its child's" $
      annotation (StmtExpr "root" (ExprLit "child" (LitNull "literal"))) @?= ("root" :: String)
  , testCase "annotated node families expose their own metadata" $ do
      let ident = Ident 3 "name"
          name = QualifiedName 3 NameUnqualified ["Name"]
          varName = VarName 3 "value"
          expr = ExprLit 3 (LitNull 3)
          param = Param 17 [] Nothing Nothing False False Nothing False False varName Nothing
          method = MethodDecl 17 [] (MethodModifier Nothing False False False) False ident [] Nothing Nothing
          constant = ConstDecl 17 [] Nothing False Nothing []
          property = PropertyDecl 17 [] (PropertyModifier Nothing Nothing False False False False) Nothing [] []
          traitUse = TraitUse 17 [name] []
          enumCase = EnumCase 17 [] ident Nothing
          check :: HasAnnotation f => f Int -> Assertion
          check node = annotation node @?= 17
      check (Program 17 [])
      check (ExprLit 17 (LitNull 3))
      check (SimpleType 17 name)
      check (LitNull 17)
      check (SimpleVar 17 varName)
      check (VarName 17 "value")
      check (Ident 17 "name")
      check (QualifiedName 17 NameUnqualified ["Name"])
      check param
      check method
      check constant
      check property
      check traitUse
      check enumCase
      check (MemberProperty property)
      check (MemberMethod method)
      check (MemberConst constant)
      check (MemberTraitUse traitUse)
      check (MemberEnumCase enumCase)
      check (MatchArm 17 [expr] expr)
      check (MatchDefault 17 expr)
      check (CatchClause 17 [name] Nothing [])
      check (SwitchCase 17 expr [])
      check (SwitchDefault 17 [])
      check (AttributeGroup 17 [])
      check (Attribute 17 name [])
      check (PropertyHook 17 [] False False HookGet Nothing HookAbstract)
      check (TraitAlias 17 Nothing ident Nothing Nothing)
      check (TraitPrecedence 17 name ident [])
      check (ClassDecl 17 [] (ClassModifier False False False) ident Nothing [] [])
      check (InterfaceDecl 17 [] ident [] [])
      check (TraitDecl 17 [] ident [])
      check (EnumDecl 17 [] ident Nothing [] [])
      check (FunctionDecl 17 [] False ident [] Nothing [])
      check (Arg 17 Nothing expr False)
      check (ArrayItem 17 Nothing expr False False)
      check (ArrayItemEmpty 17)
      check (UseClause 17 name Nothing Nothing)
      check (DeclareDirective 17 ident expr)
      check (ClassTargetName (QualifiedName 17 NameUnqualified ["Name"]))
      check (ClassTargetExpr (ExprLit 17 (LitNull 3)))
      check (MemberIdent (Ident 17 "name"))
      check (MemberExpr (ExprLit 17 (LitNull 3)))
      check (ConstNameIdent (Ident 17 "name"))
      check (ConstNameDynamic (ExprLit 17 (LitNull 3)))
      getAnnotation (ExprLit 17 (LitNull 3)) @?= (17 :: Int)
  , testCase "parsed statements, members, types and parameters have precise spans" $
      case parseProgram "annotation.php" "<?php class C { function f(int $p) {} }" of
        Right (Program _ [stmt@(StmtClass _ cls)]) ->
          case classMembers cls of
            [member@(MemberMethod method)] -> case methodParams method of
              [param] -> case paramType param of
                Just ty -> do
                  assertSpan stmt 6 39
                  assertSpan member 16 37
                  assertSpan ty 27 30
                  assertSpan param 27 33
                  getSpan stmt @?= nodeSpan stmt
                  getSpan member @?= nodeSpan member
                  getSpan ty @?= nodeSpan ty
                  getSpan param @?= nodeSpan param
                Nothing -> assertFailure "Expected parameter type"
              params -> assertFailure ("Unexpected parameters: " ++ show params)
            members -> assertFailure ("Unexpected members: " ++ show members)
        result -> assertFailure ("Unexpected parse result: " ++ show result)
  , testGroup "call spans include arguments"
      [ testCase (T.unpack source) $
          case parseExpression "annotation.php" source of
            Left err -> assertFailure (show err)
            Right expr -> do
              assertSpan expr 0 (T.length source)
              case parseExpression "annotation.php" (source <> " /* trailing */ ") of
                Left err -> assertFailure (show err)
                Right withTrivia -> assertSpan withTrivia 0 (T.length source)
      | source <- [ "$this->a(1)", "A::b(1)", "$a::b(1)", "$a?->b(1)"
                  , "$a->b(...)", "A::b(...)"
                  , "$a->b()", "A::b()", "$a?->b()"
                  , "A::$b(1)", "$a::$b(...)", "A::{foo()}(1)"
                  , "$a->$b(1)", "$a->{foo()}(1)", "$a?->$b(1)"
                  , "$a->b($c->d(1))->e(2)", "$a?->b(1)->c(2)"
                  , "$x = $a->b(1)", "1 + A::b(2)"
                  , "$a->b", "$a?->b", "A::$b", "A::B"
                  , "f(1)", "f(...)", "new A(1)"
                  ]
      ]
  , testCase "multiline call ends at closing parenthesis, not trailing trivia" $
      case parseExpression "annotation.php" "$a?->b(\n  1,\n  2\n) /* trailing */" of
        Left err -> assertFailure (show err)
        Right expr -> nodeSpan expr @?=
          Span (SourcePos "annotation.php" 1 1 0) (SourcePos "annotation.php" 4 2 18)
  , testCase "program call spans slice to equivalent expressions" $ do
      let source = "<?php $this->send($msg, 2); $x = A::b(1); $a?->b(1)->c();"
      case parseProgram "annotation.php" source of
        Left err -> assertFailure (show err)
        Right (Program _ stmts) ->
          mapM_ (checkSlice source) (concatMap (queryStmt (\e -> [e])) stmts)
  ]

checkSlice :: T.Text -> Expr (Annotated Span) -> Assertion
checkSlice source expr = do
  let Span start end = nodeSpan expr
      slice = T.take (posOffset end - posOffset start) (T.drop (posOffset start) source)
  case parseExpression "slice.php" slice of
    Left err -> assertFailure (show err)
    Right reparsed -> stripAnnotations reparsed @?= stripAnnotations expr

assertSpan :: HasAnnotation f => f (Annotated Span) -> Int -> Int -> Assertion
assertSpan node start end =
  nodeSpan node @?= Span (SourcePos "annotation.php" 1 (start + 1) start)
                        (SourcePos "annotation.php" 1 (end + 1) end)
