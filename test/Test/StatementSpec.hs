{-# LANGUAGE OverloadedStrings #-}

module Test.StatementSpec (statementTests) where

import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
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

  , testCase "Attribute argument with class constant: #[Route(Config::PATH)]" $ do
      let src = "<?php #[Route(Config::PATH)] class Post {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtClass _ cd]) ->
          assertEqual "class attrs" 1 (length (classAttrs cd))
        other -> assertFailure ("Expected class with attribute, got: " ++ show other)

  , testCase "Argument unpacking in attribute argument list is rejected (Issue #203)" $ do
      forM_ [ "<?php #[Attr(...$args)] class Foo {}" :: Text
            , "<?php #[Attr(...$args)] function bar() {}"
            , "<?php #[Attr($a, ...$args)] class Foo {}"
            ] $ \src ->
        case parseProgram "issue203.php" src of
          Left err ->
            assertEqual (T.unpack src ++ ": expected specific error message")
              (Just "Cannot use unpacking in attribute argument list")
              (errorCustom err)
          Right prog -> assertFailure (T.unpack src ++ ": expected parse error, got: " ++ show prog)

      assertParsesOk "<?php #[Attr(1, 2)] class Foo {}"
      assertParsesOk "<?php #[Attr(name: 1)] class Foo {}"
      assertParsesOk "<?php #[Attr] class Foo {}"

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

  , testCase "String-backed enum parses and round-trips (Issue #81)" $ do
      let src = "<?php enum State: string { case New = 'new'; }"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ [StmtEnum _ ed]) -> do
          assertBool "backed type is string" (case enumBackedType ed of Just (SimpleType _ (QualifiedName _ NameUnqualified ["string"])) -> True; _ -> False)
          assertEqual "case count" 1 (length (enumMembers ed))
          let printed = prettyPrint ast
          case parseProgram "test.php" printed of
            Left err -> assertFailure ("Reparsing pretty-printed enum failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
            Right ast2 -> assertEqual "round-trip AST equal" (stripAnnotations ast) (stripAnnotations ast2)
        other -> assertFailure ("String-backed enum failed: " ++ show other)

  , testCase "Restrict enum backing types to int or string (Issue #90)" $ do
      mapM_ assertParsesFail
        [ "<?php enum E: bool { case X; }"
        , "<?php enum E: float { case X; }"
        , "<?php enum E: array { case X; }"
        , "<?php enum E: ?int { case X; }"
        , "<?php enum E: int|string { case X; }"
        , "<?php enum E: \\int { case X; }"
        ]
      mapM_ assertParsesOk
        [ "<?php enum E: int { case X = 1; }"
        , "<?php enum E: string { case X = 'x'; }"
        , "<?php enum E: INT { case X = 1; }"
        , "<?php enum E: String { case X = 'x'; }"
        , "<?php enum E { case X; }"
        ]

  , testCase "Reject try without catch or finally (Issue #91)" $ do
      mapM_ assertParsesFail
        [ "<?php try {}"
        , "<?php try { doWork(); }"
        , "<?php function f() { try {} }"
        ]
      mapM_ assertParsesOk
        [ "<?php try {} catch (E) {}"
        , "<?php try {} catch (E $e) {}"
        , "<?php try {} finally {}"
        , "<?php try {} catch (E) {} finally {}"
        ]

  , testCase "Reject duplicate declaration modifiers (Issue #92)" $ do
      mapM_ assertParsesFail
        [ "<?php final final class C {}"
        , "<?php abstract abstract class C {}"
        , "<?php readonly readonly class C {}"
        , "<?php class C { static static int $x; }"
        , "<?php class C { readonly readonly int $x; }"
        , "<?php class C { public public int $x; }"
        , "<?php class C { private(set) private(set) int $x; }"
        , "<?php class C { var var int $x; }"
        , "<?php class C { public public function f() {} }"
        , "<?php class C { static static function f() {} }"
        , "<?php class C { final final function f() {} }"
        , "<?php class C { abstract abstract function f() {} }"
        , "<?php class C { public private function f() {} }"
        , "<?php class C { public public const X = 1; }"
        , "<?php class C { final final const X = 1; }"
        , "<?php class C { public private const X = 1; }"
        , "<?php class C { public function __construct(public public int $x) {} }"
        , "<?php class C { public function __construct(readonly readonly int $x) {} }"
        , "<?php class C { public function __construct(private(set) private(set) int $x) {} }"
        ]
      case parseProgram "test.php" "<?php final final class C {}" of
        Left err -> assertBool
          "error should name the duplicate modifier"
          (maybe False (T.isInfixOf "Multiple final modifiers") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php final class C {}"
        , "<?php abstract class C {}"
        , "<?php readonly class C {}"
        , "<?php final readonly class C {}"
        , "<?php abstract class D extends C {}"
        , "<?php class C { public int $x; }"
        , "<?php class C { public static int $x; }"
        , "<?php class C { static int $x; }"
        , "<?php class C { readonly public int $x; }"
        , "<?php class C { public readonly int $x; }"
        , "<?php class C { public private(set) int $x; }"
        , "<?php class C { var int $x; }"
        , "<?php class C { public function f() {} }"
        , "<?php class C { static public function f() {} }"
        , "<?php class C { public static function f() {} }"
        , "<?php class C { final public function f() {} }"
        , "<?php class C { abstract public function f(); }"
        , "<?php class C { public final const X = 1; }"
        , "<?php class C { final public const X = 1; }"
        , "<?php class C { private const Y = 2; }"
        , "<?php class C { public function __construct(public readonly int $x) {} }"
        , "<?php class C { public function __construct(private readonly int $x) {} }"
        ]

  , testCase "Reject mutually exclusive declaration modifiers (Issue #128)" $ do
      mapM_ assertParsesFail
        [ "<?php final abstract class C {}"
        , "<?php abstract final class C {}"
        , "<?php abstract final readonly class C {}"
        , "<?php class C { final abstract function f(); }"
        , "<?php class C { abstract final function f(); }"
        , "<?php class C { public final abstract function f(); }"
        , "<?php class C { public static readonly int $x; }"
        , "<?php class C { public readonly static int $x; }"
        , "<?php class C { static readonly int $x; }"
        ]
      case parseProgram "test.php" "<?php final abstract class C {}" of
        Left err -> assertBool
          "error should name the conflicting modifiers"
          (maybe False (T.isInfixOf "final and abstract") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class C { public static readonly int $x; }" of
        Left err -> assertBool
          "error should name the conflicting modifiers"
          (maybe False (T.isInfixOf "static and readonly") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php final class C {}"
        , "<?php abstract class C {}"
        , "<?php final readonly class C {}"
        , "<?php abstract readonly class C {}"
        , "<?php class C { final public function f() {} }"
        , "<?php abstract class C { abstract public function f(); }"
        , "<?php class C { public static int $x; }"
        , "<?php class C { public readonly int $x; }"
        ]

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

  , testCase "Inline HTML inside a brace-delimited if body (Issue #154)" $ do
      let src = "<?php if ($x) { ?>ok<?php } ?>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtIf _ _ [StmtInlineHtml _ html] [] Nothing]) ->
          assertEqual "inline HTML" "ok" html
        Right other -> assertFailure ("Unexpected AST: " ++ show other)

  , testCase "Inline HTML inside brace-delimited control bodies (Issue #154)" $ do
      let cases =
            [ ("while", "<?php while ($x) { ?>ok<?php } ?>")
            , ("for", "<?php for ($i = 0; $i < 1; $i++) { ?>ok<?php } ?>")
            , ("foreach", "<?php foreach ($xs as $x) { ?>ok<?php } ?>")
            ]
      mapM_ (\(name, src) -> case parseProgram "test.php" src of
        Left err -> assertFailure (name ++ ": " ++ show (formatParseError err))
        Right (Program _ [stmt]) ->
          assertEqual (name ++ " preserves inline HTML") ["ok"]
            (foldStmt (\case
              StmtInlineHtml _ html -> [html]
              _ -> []) stmt)
        Right other -> assertFailure (name ++ ": unexpected AST: " ++ show other)) cases

  , testCase "Short echo remains parseable after inline HTML in a brace body (Issue #154)" $ do
      let src = "<?php if ($x) { ?>ok<?= $title ?><?php } ?>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtIf _ _ body [] Nothing]) -> do
          assertEqual "body has inline HTML and echo" 2 (length body)
          assertBool "body starts with inline HTML"
            (case body of StmtInlineHtml _ "ok" : _ -> True; _ -> False)
          assertBool "body ends with echo"
            (case reverse body of StmtEcho _ [_] : _ -> True; _ -> False)
        Right other -> assertFailure ("Unexpected AST: " ++ show other)

  , testCase "Inline HTML in a closure body folds correctly (Issue #154)" $ do
      let src = "<?php $render = function () { ?>ok<?php }; ?>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [stmt]) ->
          assertEqual "closure inline HTML" ["ok"]
            (foldStmt (\case
              StmtInlineHtml _ html -> [html]
              _ -> []) stmt)
        Right other -> assertFailure ("Unexpected AST: " ++ show other)

  , testCase "Pretty-printed alternative-syntax inline HTML reparses (Issue #154)" $ do
      let src = "<?php if ($ready): ?><h1><?php echo $title; ?></h1><?php endif; ?>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast ->
          case parseProgram "printed.php" (prettyPrint ast) of
            Left err -> assertFailure (show (formatParseError err))
            Right ast2 -> assertEqual "round-trip AST equal"
              (stripAnnotations ast) (stripAnnotations ast2)

  , testCase "Short echo tag accepts comma-separated expressions (Issue #114)" $ do
      case parseProgram "test.php" "<?= 1, 2 ?>" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtEcho _ exprs]) ->
          assertEqual "echo expression count" 2 (length exprs)
        Right other -> assertFailure ("Expected single echo, got: " ++ show other)

  , testCase "Short echo tag accepts comma-separated expressions inside alternative-syntax bodies (Issue #153)" $ do
      case parseProgram "test.php" "<?php if ($x): ?><?= $a, $b ?><?php endif; ?>" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtIf _ _ [StmtEcho _ exprs] _ _]) ->
          assertEqual "echo expression count" 2 (length exprs)
        Right other -> assertFailure ("Expected if with a two-expression echo, got: " ++ show other)

  , testCase "Short echo tag with commas between inline HTML inside an alternative-syntax body (Issue #153)" $ do
      let src = "<?php if ($x): ?>x<?= $title, ' - ', $subtitle ?>y<?php endif; ?>"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtIf _ _ body _ _]) ->
          case [exprs | StmtEcho _ exprs <- body] of
            [exprs] -> assertEqual "echo expression count" 3 (length exprs)
            other -> assertFailure ("Expected one echo in body, got: " ++ show other)
        Right other -> assertFailure ("Expected single if statement, got: " ++ show other)

  , testCase "Preserves comments and docblocks in trivia" $ do
      let src = "<?php /** PHPDoc for Service */ class Service {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtClass (Annotated _ triv) _]) -> do
          assertBool "trivia contains DocBlock" (any (\case DocBlock _ -> True; _ -> False) triv)
        other -> assertFailure ("Expected one class declaration, got: " ++ show other)

  , testCase "Associates leading trivia and preserves it in prettyPrint (Issue #48)" $ do
      let src = "<?php /** first */ class A {} // between\nfunction f() {}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program root [StmtClass classAnn _, StmtFunction functionAnn _]) -> do
          assertEqual "program trivia" [] (annTrivia root)
          assertEqual "class leading docblock" [DocBlock " first "] (annTrivia classAnn)
          assertEqual "function leading line comment" [CommentLine " between"] (annTrivia functionAnn)
          let printed = prettyPrint ast
          assertEqual "prettyPrint emits one docblock" 1 (T.count "/** first */" printed)
          assertEqual "prettyPrint emits one line comment" 1 (T.count "// between" printed)
        other -> assertFailure ("Expected class and function declarations, got: " ++ show other)

  , testCase "Keeps trivia after the last statement on the program and in prettyPrint (Issue #238)" $ do
      let src = "<?php $x = 1; // final comment\n"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program root [StmtExpr stmtAnn _]) -> do
          assertEqual "program trailing trivia" [CommentLine " final comment"] (annTrivia root)
          assertEqual "statement leading trivia" [] (annTrivia stmtAnn)
          assertEqual "prettyPrint re-emits the comment" "<?php\n\n$x = 1;\n// final comment" (prettyPrint ast)
        other -> assertFailure ("Expected one expression statement, got: " ++ show other)

  , testCase "Keeps the trivia of a comment-only program (Issue #238)" $ do
      let src = "<?php /* only a comment */"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program root []) -> do
          assertEqual "program trailing trivia" [CommentBlock " only a comment "] (annTrivia root)
          let printed = prettyPrint ast
          assertEqual "prettyPrint re-emits the comment" "<?php\n\n/* only a comment */" printed
          case parseProgram "test.php" printed of
            Left err -> assertFailure (show (formatParseError err))
            Right reparsed -> assertEqual "printed output reparses to the same program" (stripAnnotations ast) (stripAnnotations reparsed)
        other -> assertFailure ("Expected an empty program, got: " ++ show other)

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

  , testCase "Alternative syntax control structures (Issue #107)" $ do
      mapM_ assertParsesOk
        [ "<?php if ($x): echo 1; endif;"
        , "<?php if ($x): echo 1; elseif ($y): echo 2; else: echo 3; endif;"
        , "<?php if ($x): echo 1; else if ($y): echo 2; else: echo 3; endif;"
        , "<?php while ($x): echo 1; endwhile;"
        , "<?php for ($i = 0; $i < 10; $i++): echo $i; endfor;"
        , "<?php foreach ($xs as $k => $v): echo $v; endforeach;"
        , "<?php foreach ($xs as &$v): echo $v; endforeach;"
        , "<?php switch ($x): case 1: echo 1; break; case 2: echo 2; break; default: echo 3; endswitch;"
        , "<?php IF ($x): echo 1; ENDIF;"
        ]
      case parseProgram "test.php" "<?php if ($x): echo 1; elseif ($y): echo 2; else: echo 3; endif;" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtIf _ _ [_] [(_, [_])] (Just [_])]) -> pure ()
        other -> assertFailure ("Unexpected if AST: " ++ show other)
      assertParsesOk "<?php foreach ($xs as $x): if ($x): echo 1; endif; endforeach;"
      assertParsesOk "<?php while ($a): if ($b): echo 1; else: echo 2; endif; endwhile;"
      assertParsesOk "<h1>Title</h1>\n<?php if ($x): ?>\n<p>Yes</p>\n<?php endif; ?>\n"
      let alts =
            [ "<?php if ($x): echo 1; endif;"
            , "<?php if ($x): echo 1; elseif ($y): echo 2; else: echo 3; endif;"
            , "<?php while ($x): echo 1; endwhile;"
            , "<?php for ($i = 0; $i < 10; $i++): echo $i; endfor;"
            , "<?php foreach ($xs as $k => $v): echo $v; endforeach;"
            , "<?php switch ($x): case 1: echo 1; break; default: echo 2; endswitch;"
            ]
      mapM_ (\src -> do
              ast <- case parseProgram "test.php" src of
                Left err -> assertFailure (show (formatParseError err))
                Right ast -> pure ast
              let printed = prettyPrint ast
              case parseProgram "test.php" printed of
                Left err -> assertFailure ("Reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
                Right ast2 -> assertEqual "round-trip AST equal" (stripAnnotations ast) (stripAnnotations ast2))
            alts

  , testGroup "Issue 152: alternative-syntax closers may omit semicolon before closing tag"
      [ testCase "endif" $ assertParsesOk "<?php if ($x): ?>ok<?php endif ?>"
      , testCase "endwhile" $ assertParsesOk "<?php while ($x): ?>ok<?php endwhile ?>"
      , testCase "endfor" $ assertParsesOk "<?php for ($i = 0; $i < 10; $i++): ?>ok<?php endfor ?>"
      , testCase "endforeach" $ assertParsesOk "<?php foreach ($xs as $x): ?>ok<?php endforeach ?>"
      , testCase "endswitch" $ assertParsesOk "<?php switch ($x): case 1: echo 1; endswitch ?>"
      , testCase "enddeclare" $ assertParsesOk "<?php declare(ticks=1): echo 1; enddeclare ?>"
      , testCase "explicit semicolon still parses" $ assertParsesOk "<?php if ($x): ?>ok<?php endif; ?>"
      , testCase "mixed HTML endif produces inline HTML" $ do
          case parseProgram "test.php" "<?php if ($x): ?>ok<?php endif ?>" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtIf _ _ [StmtInlineHtml _ html] [] Nothing]) ->
              assertEqual "inline HTML" "ok" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)
      , testCase "pretty-print round-trip" $ do
          let srcs =
                [ "<?php if ($x): echo 1; endif ?>"
                , "<?php while ($x): echo 1; endwhile ?>"
                , "<?php for ($i = 0; $i < 10; $i++): echo $i; endfor ?>"
                , "<?php foreach ($xs as $x): echo $x; endforeach ?>"
                , "<?php switch ($x): case 1: echo 1; endswitch ?>"
                , "<?php declare(ticks=1): echo 1; enddeclare ?>"
                , "<?php if ($x): echo 1; endif; ?>"
                ]
          mapM_ (\src -> do
                  ast <- case parseProgram "test.php" src of
                    Left err -> assertFailure (show (formatParseError err))
                    Right ast -> pure ast
                  let printed = prettyPrint ast
                  case parseProgram "test.php" printed of
                    Left err -> assertFailure ("Reparsing printed output failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
                    Right ast2 -> assertEqual "round-trip AST equal" (stripAnnotations ast) (stripAnnotations ast2))
                srcs
      ]

  , testCase "Issue 24 reproducer: relative namespace statements" $ do
      assertParsesOk "<?php namespace\\Foo::bar();"
      assertParsesOk "<?php namespace\\func();"
      assertParsesOk "<?php namespace\\MY_CONST;"
      assertParsesOk "<?php namespace\\Foo::$bar = 1;"

  , testCase "Issue 33 reproducer: parseGroupUse with trailing commas" $ do
      assertParsesOk "<?php use Foo\\{Bar, Baz,};"
      assertParsesOk "<?php use Foo\\{Bar,};"
      assertParsesOk "<?php use function Foo\\{bar, baz,};"
      assertParsesOk "<?php use const Foo\\{BAR, BAZ,};"
      assertParsesOk "<?php use Foo\\{Bar, Baz};"
      assertParsesFail "<?php use Foo,;"

  , testCase "Issue 140: mixed-kind grouped use imports" $ do
      let src = "<?php use Foo\\{function bar, const BAZ, Qux};"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ [StmtGroupUse _ ut prefix clauses]) -> do
          assertEqual "group use type" UseNormal ut
          assertEqual "prefix" (QualifiedName () NameUnqualified ["Foo"]) (stripAnnotations prefix)
          case map stripAnnotations clauses of
            [ UseClause () (QualifiedName () NameUnqualified ["bar"]) Nothing (Just UseFunction)
              , UseClause () (QualifiedName () NameUnqualified ["BAZ"]) Nothing (Just UseConst)
              , UseClause () (QualifiedName () NameUnqualified ["Qux"]) Nothing Nothing
              ] -> pure ()
            other -> assertFailure ("Expected mixed-kind clauses, got: " ++ show other)
          let printed = prettyPrint ast
          case parseProgram "test.php" printed of
            Left err -> assertFailure ("Reparsing pretty-printed mixed-kind use failed: " ++ show (formatParseError err) ++ "\nprinted: " ++ show printed)
            Right ast2 -> assertEqual "round-trip AST equal" (stripAnnotations ast) (stripAnnotations ast2)
        other -> assertFailure ("Expected StmtGroupUse, got: " ++ show other)

      case parseProgram "test.php" "<?php use Foo\\{Bar, function baz};" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtGroupUse _ UseNormal _ clauses]) ->
          case map stripAnnotations clauses of
            [ UseClause () (QualifiedName () NameUnqualified ["Bar"]) Nothing Nothing
              , UseClause () (QualifiedName () NameUnqualified ["baz"]) Nothing (Just UseFunction)
              ] -> pure ()
            other -> assertFailure ("Expected class then function clauses, got: " ++ show other)
        other -> assertFailure ("Expected StmtGroupUse, got: " ++ show other)

      assertParsesOk "<?php use function Foo\\{bar, baz};"
      assertParsesOk "<?php use const Foo\\{BAR, BAZ};"

  , testCase "Issue 32 reproducer: parseAttributeGroup with trailing commas" $ do
      assertParsesOk "<?php #[Attr,] class Foo {}"
      assertParsesOk "<?php #[Attr1, Attr2,] function bar() {}"
      assertParsesOk "<?php class Foo { #[Attr,] public int $bar; }"
      assertParsesOk "<?php class Foo { #[Attr1, Attr2,] public function baz(#[ParamAttr,] int $p) {} }"
      assertParsesOk "<?php #[Attr,] interface IFoo {}"
      assertParsesOk "<?php #[Attr,] trait TFoo {}"
      assertParsesOk "<?php #[Attr,] enum EFoo {}"
      assertParsesOk "<?php #[Attr] class NonTrailing {}"
      assertParsesFail "<?php #[] class Foo {}"
      assertParsesFail "<?php #[,] class Foo {}"

  , testCase "Issue 29 reproducer: halt compiler captures the remaining file" $ do
      let src = "<?php __halt_compiler(); payload data"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtHaltCompiler _ payload]) -> do
          assertEqual "halt compiler payload" " payload data" payload
          case parseProgram "test.php" (prettyPrint (Program () [StmtHaltCompiler () payload])) of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtHaltCompiler _ reparsedPayload]) ->
              assertEqual "round-trip payload" payload reparsedPayload
            Right other -> assertFailure ("Unexpected round-trip AST: " ++ show other)
        Right other -> assertFailure ("Unexpected AST: " ++ show other)

      case parseProgram "test.php" "<?php __halt_compiler();" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtHaltCompiler _ payload]) ->
          assertEqual "empty halt compiler payload" "" payload
        Right other -> assertFailure ("Unexpected empty-payload AST: " ++ show other)

      case parseStatement "test.php" "__halt_compiler(); payload data" of
        Left err -> assertFailure (show (formatParseError err))
        Right (StmtHaltCompiler _ payload) ->
          assertEqual "standalone halt compiler payload" " payload data" payload
        Right other -> assertFailure ("Unexpected standalone AST: " ++ show other)

  , testCase "Issue 7 reproducer: leading inline HTML starting with # or whitespace" $ do
      let srcWithHash = "# Header\n<?php echo 1;"
      case parseProgram "test.php" srcWithHash of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ (StmtInlineHtml _ html : _)) -> do
          assertEqual "leading # header preserved" "# Header\n" html
          let printed = prettyPrint ast
          assertEqual "prettyPrint preserves leading # verbatim" "# Header\n<?php\n\necho 1;" printed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on:\n" ++ show printed ++ "\n" ++ show (formatParseError err2))
            Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)
        Right other -> assertFailure ("Expected leading StmtInlineHtml, got: " ++ show other)

      let srcWithWhitespace = "   \n\t<?php echo 1;"
      case parseProgram "test.php" srcWithWhitespace of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ (StmtInlineHtml _ html : _)) -> do
          assertEqual "leading whitespace preserved" "   \n\t" html
          let printed = prettyPrint ast
          assertEqual "prettyPrint preserves leading whitespace verbatim" "   \n\t<?php\n\necho 1;" printed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on:\n" ++ show printed ++ "\n" ++ show (formatParseError err2))
            Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)
        Right other -> assertFailure ("Expected leading StmtInlineHtml, got: " ++ show other)

      let srcImmediatePhp = "<?php echo 1;"
      case parseProgram "test.php" srcImmediatePhp of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ (StmtInlineHtml _ _ : _)) ->
          assertFailure "Did not expect leading StmtInlineHtml for immediate <?php"
        Right _ -> pure ()

      let srcImmediateShortEcho = "<?= 1;"
      case parseProgram "test.php" srcImmediateShortEcho of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ (StmtInlineHtml _ _ : _)) ->
          assertFailure "Did not expect leading StmtInlineHtml for immediate <?="
        Right _ -> pure ()

      let srcPureHtml = "<h1>Header</h1><p>Paragraph</p>"
      case parseProgram "test.php" srcPureHtml of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ [StmtInlineHtml _ html]) -> do
          assertEqual "pure html preserved" srcPureHtml html
          let printed = prettyPrint ast
          assertEqual "prettyPrint pure html verbatim" srcPureHtml printed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on:\n" ++ show printed ++ "\n" ++ show (formatParseError err2))
            Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)
        Right other -> assertFailure ("Expected single StmtInlineHtml, got: " ++ show other)

      let srcWithComments = "/* C-comment */\n// line comment\n<?php echo 1;"
      case parseProgram "test.php" srcWithComments of
        Left err -> assertFailure (show (formatParseError err))
        Right ast@(Program _ (StmtInlineHtml _ html : _)) -> do
          assertEqual "leading comment-like html preserved" "/* C-comment */\n// line comment\n" html
          let printed = prettyPrint ast
          assertEqual "prettyPrint preserves comment-like html verbatim" "/* C-comment */\n// line comment\n<?php\n\necho 1;" printed
          case parseProgram "test.php" printed of
            Left err2 -> assertFailure ("Reparse failed on:\n" ++ show printed ++ "\n" ++ show (formatParseError err2))
            Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)
        Right other -> assertFailure ("Expected leading StmtInlineHtml, got: " ++ show other)

      case parseProgram "test.php" "" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ stmts) -> assertEqual "empty input produces empty stmts" [] stmts

  , testGroup "Issue 9: statements may omit semicolon before closing tag"
      [ testCase "echo" $ assertParsesOk "<?php echo 1 ?>"
      , testCase "return" $ assertParsesOk "<?php return $x ?>"
      , testCase "break" $ assertParsesOk "<?php break ?>"
      , testCase "continue" $ assertParsesOk "<?php continue ?>"
      , testCase "global" $ assertParsesOk "<?php global $x ?>"
      , testCase "static" $ assertParsesOk "<?php static $x ?>"
      , testCase "throw" $ assertParsesOk "<?php throw $e ?>"
      , testCase "explicit semicolon" $ assertParsesOk "<?php echo 1; ?>"
      , testCase "close tag switches to inline HTML" $ do
          case parseProgram "test.php" "<?php echo 1 ?><p>content</p>" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML" "<p>content</p>" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)
      ]
  , testGroup "Issue 11: var keyword in property declarations"
      [ testCase "var $x; parses with public visibility and no type annotation" $ do
          case parseProgram "test.php" "<?php class Foo { var $x; }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberProperty pd] -> do
                assertEqual "visibility" (Just Public) (propVis (propModifier pd))
                assertEqual "no type" Nothing (propType pd)
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

      , testCase "var string $x; parses with public visibility and string type" $ do
          case parseProgram "test.php" "<?php class Foo { var string $x; }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberProperty pd] -> do
                assertEqual "visibility" (Just Public) (propVis (propModifier pd))
                assertBool "has string type" (case propType pd of
                  Just (SimpleType _ (QualifiedName _ NameUnqualified ["string"])) -> True
                  _ -> False)
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

      , testCase "var static $x; parses with public visibility and static modifier" $ do
          case parseProgram "test.php" "<?php class Foo { var static $x; }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberProperty pd] -> do
                assertEqual "visibility" (Just Public) (propVis (propModifier pd))
                assertEqual "static" True (propStatic (propModifier pd))
                assertEqual "no type" Nothing (propType pd)
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

      , testCase "static var $x; parses with public visibility and static modifier" $ do
          case parseProgram "test.php" "<?php class Foo { static var $x; }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberProperty pd] -> do
                assertEqual "visibility" (Just Public) (propVis (propModifier pd))
                assertEqual "static" True (propStatic (propModifier pd))
                assertEqual "no type" Nothing (propType pd)
              _ -> assertFailure "Expected MemberProperty"
            _ -> assertFailure "Expected StmtClass"

      , testCase "round-trip formatting and reparsing preserves property AST structure" $ do
          let src = "<?php class Foo { var $x; var string $y; var static $z; }"
          case parseProgram "test.php" src of
            Left err -> assertFailure (show (formatParseError err))
            Right ast -> do
              let printed = prettyPrint ast
              case parseProgram "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed: " ++ show (formatParseError err2))
                Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)

      , testCase "public var $x is rejected" $ do
          assertParsesFail "<?php class Foo { public var $x; }"
      ]

  , testGroup "Issue 19: late static binding statements"
      [ testCase "static::bar(); parses as an expression statement" $ do
          case parseStatement "test.php" "static::bar();" of
            Left err -> assertFailure (show (formatParseError err))
            Right stmt -> case stmt of
              StmtExpr _ (ExprStaticCall _ (ClassTargetName (QualifiedName _ NameUnqualified ["static"])) (MemberIdent (Ident _ "bar")) (ArgsList [])) ->
                pure ()
              other -> assertFailure ("Expected StmtExpr of static::bar(), got: " ++ show other)

      , testCase "static $x; still parses as a static variable declaration" $ do
          case parseStatement "test.php" "static $x;" of
            Left err -> assertFailure (show (formatParseError err))
            Right stmt -> case stmt of
              StmtStatic _ [(VarName _ "x", Nothing)] -> pure ()
              other -> assertFailure ("Expected StmtStatic, got: " ++ show other)
      ]

  , testGroup "Issue 21: semi-reserved keywords as method and class constant names"
      [ testCase "semi-reserved keyword method name in a class" $ do
          case parseProgram "test.php" "<?php class Foo { public function list() {} }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberMethod md] -> assertEqual "method name" "list" (let Ident _ n = methodName md in n)
              other -> assertFailure ("Expected one method, got: " ++ show other)
            other -> assertFailure ("Expected StmtClass, got: " ++ show other)

      , testCase "semi-reserved keyword method name in a trait" $ do
          assertParsesOk "<?php trait T { public function fn() {} }"

      , testCase "semi-reserved keyword method name in an interface" $ do
          assertParsesOk "<?php interface I { public function match(); }"

      , testCase "semi-reserved keyword class constant names" $ do
          case parseProgram "test.php" "<?php class Foo { const DEFAULT = 1; const MATCH = 2; }" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
              [MemberConst c1, MemberConst c2] -> do
                let names = [n | Ident _ n <- map fst (constItems c1 ++ constItems c2)]
                assertEqual "constant names" ["DEFAULT", "MATCH"] names
              other -> assertFailure ("Expected two constants, got: " ++ show other)
            other -> assertFailure ("Expected StmtClass, got: " ++ show other)

      , testCase "round-trips semi-reserved method and constant names" $ do
          let src = "<?php\nclass Foo {\n    public function list() {}\n    public function match() {}\n    const DEFAULT = 1;\n    const MATCH = 2;\n}"
          case parseProgram "test.php" src of
            Left err -> assertFailure (show (formatParseError err))
            Right ast -> do
              let printed = prettyPrint ast
              case parseProgram "test.php" printed of
                Left err2 -> assertFailure ("Reparse failed:\n" ++ T.unpack printed ++ "\nError: " ++ show (formatParseError err2))
                Right ast2 -> assertEqual "round-trip AST matches" (stripAnnotations ast) (stripAnnotations ast2)

      , testCase "class remains rejected as a method name" $ do
          assertParsesFail "<?php class Foo { public function class() {} }"

      , testCase "class remains rejected as a class constant name" $ do
          assertParsesFail "<?php class Foo { const CLASS = 1; }"

      , testCase "top-level function declarations still reject keywords" $ do
          assertParsesFail "<?php function list() {}"
      ]

  , testGroup "Issue 53: close tag consumes CRLF after ?>"
      [ testCase "CRLF immediately after close tag is not inline HTML" $ do
          case parseProgram "close-crlf.php" "<?php echo 1; ?>\r\nHTML" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML starts at HTML" "HTML" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "lone LF after close tag is still consumed" $ do
          case parseProgram "test.php" "<?php echo 1; ?>\nHTML" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML starts at HTML" "HTML" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "close tag with no trailing newline still switches to HTML" $ do
          case parseProgram "test.php" "<?php echo 1; ?>HTML" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML" "HTML" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "second newline after CRLF is preserved as inline HTML" $ do
          case parseProgram "test.php" "<?php echo 1; ?>\r\n\r\nHTML" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "only the first CRLF is consumed" "\r\nHTML" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)
      ]

  , testGroup "Issue 239: ?> terminates a line comment"
      [ testCase "// comment ends at ?> and the rest of the file survives" $ do
          case parseProgram "test.php" "<?php echo \"x\"; // c ?>tail<?php echo \"y\";" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html, StmtEcho _ _]) ->
              assertEqual "inline HTML between the blocks" "tail" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "# comment ends at ?> and the rest of the file survives" $ do
          case parseProgram "test.php" "<?php echo \"x\"; # c ?>tail<?php echo \"y\";" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html, StmtEcho _ _]) ->
              assertEqual "inline HTML between the blocks" "tail" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "a ? not followed by > stays inside the comment" $ do
          case parseProgram "test.php" "<?php echo 1; // is it? yes ?>tail" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML" "tail" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "?> ending a comment also terminates the statement" $ do
          case parseProgram "test.php" "<?php echo 1 // c ?>tail" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html]) ->
              assertEqual "inline HTML" "tail" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "the comment text stops before the close tag" $ do
          case parseProgram "test.php" "<?php echo 1; // c ?>tail" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program (Annotated _ triv) _) ->
              assertEqual "trivia" [CommentLine " c "] triv

      , testCase "a block comment still does not end at ?>" $ do
          case parseProgram "test.php" "<?php echo \"x\"; /* c ?> */ echo \"y\";" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtEcho _ _]) -> pure ()
            Right other -> assertFailure ("Unexpected AST: " ++ show other)
      ]

  , testGroup "Issue 271: a second open tag inside code mode is rejected"
      [ testCase "the three reported mid-code reproducers are rejected" $ do
          forM_ [ "<?php $x = 1; <?php $y = 2;" :: Text
                , "<?php 1; <?php"
                , "a<?php 1; <?php 2;"
                ] assertParsesFail

      , testCase "a bare short echo tag in code mode is rejected" $ do
          assertParsesFail "<?php 1; <?= 2;"

      , testCase "an open tag inside a nested body is rejected" $ do
          assertParsesFail "<?php if (true) { <?php } ?>"
          assertParsesFail "<?php function f() { $x = 1; <?php $y = 2; }"

      , testCase "a close tag, inline HTML, and a new open tag still parse" $ do
          case parseProgram "test.php" "<?php echo 1; ?>tail<?php echo 2;" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html, StmtEcho _ _]) ->
              assertEqual "inline HTML between the blocks" "tail" html
            Right other -> assertFailure ("Unexpected AST: " ++ show other)

      , testCase "a short echo tag may also reopen after a close tag" $ do
          assertParsesOk "<?php echo 1; ?>tail<?= 2; ?>"

      , testCase "a comment right after the reopened open tag parses" $ do
          assertParsesOk "<?php if (true) { ?>x<?php /* c */ } ?>"
          assertParsesOk "<?php ?> <?php /* c */ ?>"

      , testCase "no open tag is silently removed from the printed program" $ do
          case parseProgram "test.php" "<?php echo 1; ?>tail<?php echo 2;" of
            Left err -> assertFailure (show (formatParseError err))
            Right ast -> do
              let printed = prettyPrint ast
              case parseProgram "reparsed.php" printed of
                Left err -> assertFailure (show (formatParseError err) ++ "\nprinted: " ++ T.unpack printed)
                Right (Program _ [StmtEcho _ _, StmtInlineHtml _ html, StmtEcho _ _]) ->
                  assertEqual "HTML survives printing" "tail" html
                Right other ->
                  assertFailure ("Unexpected reparsed AST: " ++ show other ++ "\nprinted: " ++ T.unpack printed)
      ]

  , testGroup "Issue 272: the full open tag is matched case-insensitively"
      [ testCase "mixed-case full open tags parse the same statement body as <?php" $ do
          expected <- case parseProgram "test.php" "<?php $x = 1;" of
            Left err -> assertFailure (show (formatParseError err))
            Right ast -> pure (stripAnnotations ast)
          forM_ [ "<?PHP $x = 1;" :: Text
                , "<?pHp $x = 1;"
                , "<?Php $x = 1;"
                , "<?PhP $x = 1;"
                ] $ \src ->
            case parseProgram "test.php" src of
              Left err -> assertFailure (T.unpack src ++ ": " ++ show (formatParseError err))
              Right ast ->
                assertEqual (T.unpack src ++ ": same program as lowercase tag")
                  expected (stripAnnotations ast)

      , testCase "a mixed-case full tag after inline HTML still opens a PHP region" $ do
          expected <- case parseProgram "test.php" "hello<?php $x = 1;" of
            Left err -> assertFailure (show (formatParseError err))
            Right ast -> pure (stripAnnotations ast)
          case parseProgram "test.php" "hello<?PHP $x = 1;" of
            Left err -> assertFailure (show (formatParseError err))
            Right ast ->
              assertEqual "same program as lowercase tag after HTML"
                expected (stripAnnotations ast)

      , testCase "comments and the existing full-tag whitespace rules still work" $ do
          assertParsesOk "<?PHP /* c */ $x = 1;"
          assertParsesOk "<?pHp\n$x = 1;"
          assertParsesOk "<?PHP\t$x = 1;"
          assertParsesOk "<?php echo 1; ?>tail<?PHP echo 2;"

      , testCase "short <? and short-echo <?= parsing is unchanged" $ do
          assertParsesOk "<? $x = 1;"
          assertParsesOk "<?php echo 1; ?>tail<?= 2; ?>"
          case parseProgram "test.php" "<?= 1 ?>" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtEcho _ [_]]) -> pure ()
            Right other -> assertFailure ("Expected a single echo, got: " ++ show other)
      ]

  , testCase "Reject members invalid in enum, class, and interface contexts (Issue #89)" $ do
      mapM_ assertParsesFail
        [ "<?php enum E { public int $x; }"
        , "<?php class C { case X; }"
        , "<?php interface I { use T; }"
        , "<?php interface I { public int $x; }"
        ]
      mapM_ assertParsesOk
        [ "<?php enum E { case X; }"
        , "<?php enum E { public function f() {} }"
        , "<?php enum E { const C = 1; }"
        , "<?php enum E { use T; }"
        , "<?php class C { use T; }"
        , "<?php class C { const C = 1; }"
        , "<?php class C { public int $x; }"
        , "<?php trait T2 { public int $x; }"
        , "<?php interface I { public function f(); }"
        , "<?php interface I { const C = 1; }"
        , "<?php interface I { public string $name { get; set; } }"
        ]

  , testGroup "Issue 277: class and trait bodies accept only member declarations"
      [ testCase "rejects a plain assignment statement in a class body" $
          assertParsesFail "<?php class C { $x = 1; }"
      , testCase "rejects a plain assignment statement in a trait body" $
          assertParsesFail "<?php trait T { $x = 1; }"
      , testCase "rejects an anonymous-class expression statement in a class body" $
          assertParsesFail "<?php class C { new class {}; }"
      , testCase "rejects an anonymous-class expression statement in a trait body" $
          assertParsesFail "<?php trait T { new class {}; }"
      , testCase "rejects a modifierless typed property in a class body" $
          assertParsesFail "<?php class C { int $x; }"
      , testCase "rejects a modifierless hooked property in a class body" $
          assertParsesFail "<?php class C { string $x { get => 'a'; } }"
      , testCase "rejects a modifierless hooked property in an interface body" $
          assertParsesFail "<?php interface I { string $x { get; } }"
      , testCase "rejects an echo statement in a class body" $
          assertParsesFail "<?php class C { echo 1; }"
      , testCase "rejects a plain assignment statement in an anonymous class body" $
          assertParsesFail "<?php $o = new class { $x = 1; };"
      , testCase "accepts legal class members" $
          assertParsesOk "<?php class C { use T; const A = 1; public $x = 1; var $y; public static int $z = 2; public function f() {} public string $n { get => 'a'; } }"
      , testCase "accepts legal trait members" $
          assertParsesOk "<?php trait T { use U; const A = 1; public $x = 1; var $y; abstract public function f(); public static function g() {} }"
      , testCase "accepts properties declared with any single modifier" $
          assertParsesOk "<?php abstract class C { var $a; public $b; static $c; readonly int $d; final $e; abstract $f { get; } private(set) int $g; }"
      ]

  , testGroup "Issue 276: short echo statements require a terminator"
      [ testCase "rejects an unterminated short echo at EOF" $
          assertParsesFail "<?= 1"
      , testCase "accepts a semicolon-terminated short echo at EOF" $
          assertParsesOk "<?= 1;"
      , testCase "accepts a short echo terminated by a close tag" $
          assertParsesOk "<?= 1 ?>"
      , testCase "accepts comma-separated expressions with a final semicolon" $
          assertParsesOk "<?= 1, 2;"
      , testCase "accepts comma-separated expressions terminated by a close tag" $
          assertParsesOk "<?= 1, 2 ?>"
      , testCase "rejects comma-separated expressions without a final terminator" $
          assertParsesFail "<?= 1, 2"
      ]

  , testCase "Require separator after long opening tag (Issue #93)" $ do
      assertParsesFail "<?php$x = 1;"
      assertParsesFail "<?phpphpinfo();"
      assertParsesOk "<?php $x = 1;"
      assertParsesOk "<?php\n$x = 1;"
      assertParsesOk "<?php\r\n$x = 1;"
      assertParsesOk "<?php\t$x = 1;"
      assertParsesOk "<?php"

  , testCase "Reject form feed as PHP code whitespace (Issue #279)" $ do
      mapM_ assertParsesFail
        [ "<?php $x\x0c= 1;"
        , "<?php\x0c$x = 1;"
        ]
      mapM_ assertParsesOk
        [ "<?php $x\x0b= 1;"
        , "<?php $x = \"\x0c\";"
        , "<?php // comment \x0c\n$x = 1;"
        , "<?php # comment \x0c\n$x = 1;"
        , "<?php /* comment \x0c */ $x = 1;"
        , "<?php /** doc \x0c */ $x = 1;"
        ]

  , testCase "Reject reserved type and literal names as class names (Issue #94)" $ do
      -- Built-in type names
      assertParsesFail "<?php class int {}"
      assertParsesFail "<?php class float {}"
      assertParsesFail "<?php class string {}"
      assertParsesFail "<?php class bool {}"
      assertParsesFail "<?php class void {}"
      assertParsesFail "<?php class iterable {}"
      assertParsesFail "<?php class object {}"
      assertParsesFail "<?php class mixed {}"
      assertParsesFail "<?php class never {}"
      -- Literal names
      assertParsesFail "<?php class true {}"
      assertParsesFail "<?php class false {}"
      assertParsesFail "<?php class null {}"
      -- Contextual names
      assertParsesFail "<?php class self {}"
      assertParsesFail "<?php class parent {}"
      assertParsesFail "<?php class static {}"
      -- Case-insensitivity checks
      assertParsesFail "<?php class Int {}"
      assertParsesFail "<?php class TRUE {}"
      assertParsesFail "<?php class Self {}"
      assertParsesFail "<?php class STATIC {}"
      -- Other class-like declarations
      assertParsesFail "<?php interface int {}"
      assertParsesFail "<?php interface true {}"
      assertParsesFail "<?php interface self {}"
      assertParsesFail "<?php trait int {}"
      assertParsesFail "<?php trait true {}"
      assertParsesFail "<?php trait self {}"
      assertParsesFail "<?php enum int {}"
      assertParsesFail "<?php enum true {}"
      assertParsesFail "<?php enum self {}"
      -- PHP-legal context-sensitive and soft-reserved names still pass
      assertParsesOk "<?php class enum {}"
      assertParsesOk "<?php class resource {}"
      assertParsesOk "<?php class numeric {}"
      assertParsesOk "<?php interface enum {}"
      assertParsesOk "<?php trait enum {}"
      assertParsesOk "<?php enum resource {}"

  , testCase "Parse error spans locate the failure (Issue #106)" $ do
      -- Failure on line 3
      assertErrorAt "<?php\necho 'fine';\n$b = 2 +;\n" (3, 9, 27) (3, 10, 28) (Just "\";\"")
      -- Mid-line failure: found token must not run past the newline
      assertErrorAt "<?php $a = ;\necho 'ok';" (1, 12, 11) (1, 13, 12) (Just "\";\"")
      -- Multiline call list, failure on line 3
      assertErrorAt "<?php foo(1,\n  2,\n  );;; }" (3, 8, 25) (3, 9, 26) (Just "\"}\"")
      -- Unterminated block comment fails at end of input
      assertErrorAt "<?php /* never closed" (1, 22, 21) (1, 22, 21) (Just "end of input")
      case parseProgram "test.php" "<?php\necho 'fine';\n$b = 2 +;\n" of
        Left err -> assertBool "formatted location" ("test.php:3:9: error: unexpected \";\"" `T.isPrefixOf` formatParseError err)
        Right _ -> assertFailure "Expected parse failure but parse succeeded"

  , testCase "Parser rejects declare, goto/label, and unset() constructs (Issue #108)" $ do
      -- 1. declare statement (directive list)
      assertParsesOk "<?php declare(strict_types=1);"
      case parseProgram "test.php" "<?php declare(strict_types=1); function f() {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (ExprLit _ (LitInt _ val _))] Nothing, StmtFunction _ _]) -> do
          assertEqual "directive name" "strict_types" name
          assertEqual "directive value" 1 val
        other -> assertFailure ("Unexpected AST for declare statement: " ++ show other)

      case parseProgram "test.php" "<?php declare(ticks=1) { echo 'x'; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (ExprLit _ (LitInt _ val _))] (Just [StmtEcho _ _])]) -> do
          assertEqual "directive name" "ticks" name
          assertEqual "directive value" 1 val
        other -> assertFailure ("Unexpected AST for block declare: " ++ show other)

      case parseProgram "test.php" "<?php declare(ticks=1): echo 'x'; enddeclare;" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (ExprLit _ (LitInt _ val _))] (Just [StmtEcho _ _])]) -> do
          assertEqual "directive name" "ticks" name
          assertEqual "directive value" 1 val
        other -> assertFailure ("Unexpected AST for alt declare: " ++ show other)

      -- 2. goto and label statements
      case parseProgram "test.php" "<?php goto end; end:" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtGoto _ (Ident _ gName), StmtLabel _ (Ident _ lName)]) -> do
          assertEqual "goto label name" "end" gName
          assertEqual "target label name" "end" lName
        other -> assertFailure ("Unexpected AST for goto/label: " ++ show other)

      -- 3. unset statement
      case parseProgram "test.php" "<?php unset($a, $b['k']);" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtUnset _ [ExprVar _ _, ExprArrayAccess _ _ _]]) -> pure ()
        other -> assertFailure ("Unexpected AST for unset: " ++ show other)

  , testCase "Enforce strict_types declaration rules (Issue #273)" $ do
      mapM_ assertParsesFail
        [ "<?php $x = 1; declare(strict_types=1);"
        , "a<?php declare(strict_types=1);"
        , "<?php $x = 1; ?> <?php declare(strict_types=1);"
        , "<?php ?><?php declare(strict_types=1);"
        , "<?php ; declare(strict_types=1);"
        , "<?php function f() { declare(strict_types=1); }"
        , "<?php declare(strict_types=1) { $x = 1; }"
        , "<?php declare(strict_types=1): echo 1; enddeclare;"
        , "<?php declare(strict_types=1) echo 1;"
        , "<?php declare(strict_types=2);"
        , "<?php declare(strict_types='1');"
        , "<?php declare(strict_types=true);"
        , "<?php declare(strict_types=1.5);"
        , "<?php declare(strict_types=-1);"
        ]

      mapM_ assertParsesOk
        [ "<?php declare(strict_types=0);"
        , "<?php declare(strict_types=1);"
        , "<?php /* leading comment */ declare(strict_types=1);"
        , "<?php // leading comment\ndeclare(strict_types=0);"
        , "<?php declare(ticks=1) { echo 'x'; }"
        , "<?php declare(ticks=1): echo 'x'; enddeclare;"
        , "<?php declare(encoding='UTF-8');"
        ]

  , testCase "Declare directive values accept compile-time constant expressions (Issue #275)" $ do
      -- Encoding accepts what PHP permits: literals and constant concatenation
      -- of literals (PHP folds `'a' . 'b'` into one literal at parse time).
      mapM_ assertParsesOk
        [ "<?php declare(encoding='UTF-8' . '');"
        , "<?php declare(encoding='UTF' . '-8');"
        , "<?php declare(encoding='a' . 1 . 'b');"
        , "<?php declare(encoding=('UTF-8'));"
        , "<?php declare(encoding='a' . ('b'));"
        ]

      -- The concatenated value is preserved as a concat expression in the AST.
      case parseProgram "test.php" "<?php declare(encoding='UTF-8' . '');" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) val] _]) -> do
          assertEqual "directive name" "encoding" name
          case val of
            ExprBinary _ OpConcat _ _ -> pure ()
            other -> assertFailure ("Expected a concat expression, got: " ++ show other)
        other -> assertFailure ("Unexpected AST for encoding concat: " ++ show other)

      -- Literal encoding values keep parsing, and stay literals in the AST.
      case parseProgram "test.php" "<?php declare(encoding='UTF-8');" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ _ (ExprLit _ _)] _]) -> pure ()
        other -> assertFailure ("Unexpected AST for literal encoding: " ++ show other)

      -- Non-constant runtime expressions stay rejected.
      mapM_ assertParsesFail
        [ "<?php declare(encoding=$x);"
        , "<?php declare(encoding=foo());"
        , "<?php declare(encoding=1+1);"
        , "<?php declare(ticks=$x);"
        ]

      -- strict_types keeps its integer-literal rule, and ticks keeps its
      -- literal-only behavior.
      mapM_ assertParsesFail
        [ "<?php declare(strict_types='1' . '');"
        , "<?php declare(strict_types=1+1);"
        , "<?php declare(ticks=1+1);"
        , "<?php declare(ticks='a' . 'b');"
        ]
      mapM_ assertParsesOk
        [ "<?php declare(strict_types=1);"
        , "<?php declare(strict_types=0);"
        , "<?php declare(ticks=1);"
        , "<?php declare(ticks='1');"
        , "<?php declare(ticks=1.5);"
        , "<?php declare(ticks=1) { echo 'x'; }"
        , "<?php declare(ticks=1, encoding='UTF-8');"
        , "<?php declare(encoding='UTF-8', ticks=1);"
        ]

  , testCase "Encoding declaration must be the first statement (Issue #274)" $ do
      -- PHP only lets earlier top-level declare statements precede an
      -- encoding declaration: any other statement, inline HTML, an empty
      -- statement, or a close tag that does not end a statement comes too
      -- early, and a nested declaration is never first.
      mapM_ assertParsesFail
        [ "<?php $x = 1; declare(encoding='UTF-8');"
        , "<?php $x = 1; ?><?php declare(encoding='UTF-8');"
        , "<?php ; declare(encoding='UTF-8');"
        , "<?php ?><?php declare(encoding='UTF-8');"
        , "a<?php declare(encoding='UTF-8');"
        , "<?= 1 ?><?php declare(encoding='UTF-8');"
        , "<?php namespace A; declare(encoding='UTF-8');"
        , "<?php declare(ticks=1); ?><?php declare(encoding='UTF-8');"
        , "<?php declare(ticks=1) {} ?><?php declare(encoding='UTF-8');"
        , "<?php declare(ticks=1) ?>x<?php declare(encoding='UTF-8');"
        , "<?php { declare(encoding='UTF-8'); }"
        , "<?php function f() { declare(encoding='UTF-8'); }"
        , "<?php declare(ticks=1) { declare(encoding='UTF-8'); }"
        , "<?php declare(ticks=1) declare(encoding='UTF-8');"
        ]

      mapM_ assertParsesOk
        [ "<?php declare(encoding='UTF-8');"
        , "<?php /* leading comment */ declare(encoding='UTF-8');"
        , "<?php // leading comment\ndeclare(encoding='UTF-8');"
        , "<?php declare(encoding='UTF-8') ?>tail"
        , "<?php declare(encoding='UTF-8'); ?>"
        , "<?php declare(encoding='UTF-8') { echo 1; }"
        , "<?php declare(ticks=1); declare(encoding='UTF-8');"
        , "<?php declare(strict_types=1); declare(encoding='UTF-8');"
        , "<?php declare(ticks=1) ?><?php declare(encoding='UTF-8');"
        , "<?php declare(ticks=1) { ?>x<?php } declare(encoding='UTF-8');"
        , "<?php declare(encoding='UTF-8'); declare(encoding='UTF-8');"
        , "<?php $x = 1; declare(ticks=1);"
        , "<?php $x = 1; ?>x<?php declare(ticks=1) { echo 1; }"
        ]

  , testGroup "List destructuring syntax in assignments and foreach loops (Issue #125)"
      [ testCase "list(...) destructuring in assignments" $ do
          assertParsesOk "<?php list($a, $b) = $arr;"
          case parseProgram "test.php" "<?php list($a, $b) = $arr;" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtExpr _ (ExprAssign _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "a"))) False False, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "b"))) False False]) (ExprVar _ (SimpleVar _ (VarName _ "arr"))))]) -> pure ()
            other -> assertFailure ("Unexpected AST for list assignment: " ++ show other)
      , testCase "list(...) destructuring in foreach loops" $ do
          assertParsesOk "<?php foreach ($arr as list($a, $b)) {}"
          case parseProgram "test.php" "<?php foreach ($arr as list($a, $b)) {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ (ExprVar _ (SimpleVar _ (VarName _ "arr"))) Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "a"))) False False, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "b"))) False False]) False []]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach with list: " ++ show other)
      , testCase "foreach with key and list(...) value" $ do
          assertParsesOk "<?php foreach ($arr as $k => list($a, $b)) {}"
          case parseProgram "test.php" "<?php foreach ($arr as $k => list($a, $b)) {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ (ExprVar _ _) (Just (ExprVar _ (SimpleVar _ (VarName _ "k")))) (ExprList _ [_, _]) False []]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach with key and list: " ++ show other)
      , testCase "foreach with nested list(...) in alternative syntax" $ do
          assertParsesOk "<?php foreach ($arr as list($a, list($b, $c))): echo $a; endforeach;"
          case parseProgram "test.php" "<?php foreach ($arr as list($a, list($b, $c))): echo $a; endforeach;" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ _ Nothing (ExprList _ [ArrayItem _ Nothing (ExprVar _ _) False False, ArrayItem _ Nothing (ExprList _ [_, _]) False False]) False [StmtEcho _ _]]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach alt syntax with nested list: " ++ show other)
      ]
  , testGroup "Array destructuring syntax with omitted elements in assignments and foreach loops (Issue #126)"
      [ testCase "short array destructuring with omitted elements in assignments" $ do
          assertParsesOk "<?php [$a, , $b] = $arr;"
          assertParsesOk "<?php [, $b] = $arr;"
          assertParsesOk "<?php [, , $c] = $arr;"
          case parseProgram "test.php" "<?php [$a, , $b] = $arr;" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtExpr _ (ExprAssign _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "a"))) False False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "b"))) False False]) (ExprVar _ (SimpleVar _ (VarName _ "arr"))))]) -> pure ()
            other -> assertFailure ("Unexpected AST for array destructuring assignment: " ++ show other)
      , testCase "short array destructuring with omitted elements in foreach loops" $ do
          assertParsesOk "<?php foreach ($arr as [$first, , $third]) {}"
          case parseProgram "test.php" "<?php foreach ($arr as [$first, , $third]) {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ (ExprVar _ (SimpleVar _ (VarName _ "arr"))) Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "first"))) False False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "third"))) False False]) False []]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach with array destructuring: " ++ show other)
      , testCase "foreach with key and array destructuring with omitted elements" $ do
          assertParsesOk "<?php foreach ($arr as $k => [$first, , $third]) {}"
          case parseProgram "test.php" "<?php foreach ($arr as $k => [$first, , $third]) {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ (ExprVar _ _) (Just (ExprVar _ (SimpleVar _ (VarName _ "k")))) (ExprArray _ [_, ArrayItemEmpty _, _]) False []]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach with key and array destructuring: " ++ show other)
      , testCase "foreach with leading omitted elements" $ do
          assertParsesOk "<?php foreach ($arr as [, $b]) {}"
          case parseProgram "test.php" "<?php foreach ($arr as [, $b]) {}" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ (ExprVar _ _) Nothing (ExprArray _ [ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ (SimpleVar _ (VarName _ "b"))) False False]) False []]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach with leading omitted array destructuring: " ++ show other)
      , testCase "foreach with nested array destructuring in alternative syntax" $ do
          assertParsesOk "<?php foreach ($arr as [$a, [$b, , $c]]): echo $a; endforeach;"
          case parseProgram "test.php" "<?php foreach ($arr as [$a, [$b, , $c]]): echo $a; endforeach;" of
            Left err -> assertFailure (show (formatParseError err))
            Right (Program _ [StmtForeach _ _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ _) False False, ArrayItem _ Nothing (ExprArray _ [ArrayItem _ Nothing (ExprVar _ _) False False, ArrayItemEmpty _, ArrayItem _ Nothing (ExprVar _ _) False False]) False False]) False [StmtEcho _ _]]) -> pure ()
            other -> assertFailure ("Unexpected AST for foreach alt syntax with nested array destructuring: " ++ show other)
      ]
  , testCase "Reject promoted property modifiers on non-constructors (Issue #129)" $ do
      mapM_ assertParsesFail
        [ "<?php function foo(public int $x) {}"
        , "<?php function foo(protected int $x) {}"
        , "<?php function foo(private int $x) {}"
        , "<?php function foo(readonly int $x) {}"
        , "<?php function foo(public readonly int $x) {}"
        , "<?php function foo(public private(set) int $x) {}"
        , "<?php function __construct(public int $x) {}"
        , "<?php class C { public function bar(public int $x) {} }"
        , "<?php class C { public function bar(protected int $x) {} }"
        , "<?php class C { public function bar(private int $x) {} }"
        , "<?php class C { public function bar(readonly int $x) {} }"
        , "<?php class C { public function bar(public private(set) int $x) {} }"
        , "<?php trait T { public function bar(public int $x) {} }"
        , "<?php enum E { public function bar(public int $x) {} }"
        , "<?php enum E { public function __construct(public int $x) {} }"
        , "<?php interface I { public function bar(public int $x); }"
        , "<?php interface I { public function __construct(public int $x); }"
        , "<?php abstract class C { abstract public function __construct(public int $x); }"
        , "<?php $o = new class { public function bar(public int $x) {} };"
        ]
      case parseProgram "test.php" "<?php function foo(public int $x) {}" of
        Left err -> assertBool
          "error should reject promoted properties outside constructors"
          (maybe False (T.isInfixOf "Cannot declare promoted property outside a constructor") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class C { public function bar(public int $x) {} }" of
        Left err -> assertBool
          "error should reject promoted properties outside constructors"
          (maybe False (T.isInfixOf "Cannot declare promoted property outside a constructor") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php abstract class C { abstract public function __construct(public int $x); }" of
        Left err -> assertBool
          "error should reject promoted properties in abstract constructors"
          (maybe False (T.isInfixOf "Cannot declare promoted property in an abstract constructor") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php interface I { public function __construct(public int $x); }" of
        Left err -> assertBool
          "error should reject promoted properties in interface constructors"
          (maybe False (T.isInfixOf "Cannot declare promoted property in an abstract constructor") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php function foo(int $x, string $y = 'default') {}"
        , "<?php class C { public function bar(int $x, string $y = 'default') {} }"
        , "<?php class C { public function __construct(public string $name, private readonly int $age = 18) {} }"
        , "<?php class C { public function __Construct(public string $name) {} }"
        , "<?php trait T { public function __construct(public string $name) {} }"
        , "<?php $o = new class { public function __construct(public string $name) {} };"
        , "<?php abstract class C { public function __construct(public string $name) {} }"
        ]
    , testCase "Reject visibility, final, and types on global const declarations (Issue #147)" $ do
      mapM_ assertParsesFail
        [ "<?php public const FOO = 1;"
        , "<?php protected const FOO = 1;"
        , "<?php private const FOO = 1;"
        , "<?php final const FOO = 1;"
        , "<?php const string FOO = 'a';"
        , "<?php #[Attr] public const FOO = 1;"
        , "<?php #[Attr] final const FOO = 1;"
        , "<?php #[Attr] const int FOO = 1;"
        , "<?php namespace Foo { public const BAR = 1; }"
        , "<?php namespace Foo { final const BAR = 1; }"
        , "<?php namespace Foo { const string BAR = 'a'; }"
        ]
      mapM_ assertParsesOk
        [ "<?php const FOO = 1;"
        , "<?php const FOO = 1, BAR = 2;"
        , "<?php #[Attr] const FOO = 1;"
        , "<?php namespace Foo { const BAR = 1; }"
        , "<?php class C { public const int FOO = 1; final protected const BAR = 2; private const string BAZ = 'x'; }"
        , "<?php interface I { public const string FOO = 'a'; }"
        , "<?php trait T { const FOO = 1; }"
        , "<?php enum E { const FOO = 1; }"
        ]
    , testCase "Reject non-final variadic parameters (Issue #149)" $ do
      mapM_ assertParsesFail
        [ "<?php function f(...$a, $b) {}"
        , "<?php class C { function f(...$a, $b) {} }"
        , "<?php $f = function (...$a, $b) {};"
        , "<?php $f = fn(...$a, $b) => $a;"
        ]
      mapM_ assertParsesOk
        [ "<?php function f($a, ...$b) {}"
        , "<?php class C { function f($a, ...$b) {} }"
        , "<?php $f = function ($a, ...$b) {};"
        , "<?php $f = fn($a, ...$b) => $a;"
        , "<?php function f(...$args) {}"
        , "<?php $f = function (...$args) {};"
        , "<?php $f = fn(...$args) => $args;"
        ]
    , testCase "Reject method bodies on interface and abstract methods (Issue #150)" $ do
      mapM_ assertParsesFail
        [ "<?php interface I { public function f() { return 1; } }"
        , "<?php abstract class C { abstract function f() { return 1; } }"
        ]
      mapM_ assertParsesOk
        [ "<?php interface I { public function f(); }"
        , "<?php abstract class C { abstract function f(); }"
        , "<?php class C { public function f() { return 1; } }"
        ]
    , testCase "Typed by-reference parameters (Issue #151)" $ do
      mapM_ assertParsesOk
        [ "<?php function swap(int &$a, int &$b) {}"
        , "<?php function pick(array &$xs) {}"
        , "<?php function f(int &...$xs) {}"
        , "<?php class C { public function merge(array &$out): void {} }"
        , "<?php class C { public function __construct(private array &$ref) {} }"
        , "<?php $f = function (array &$xs) {};"
        , "<?php $f = fn(array &$xs) => $xs;"
        , "<?php function f((A&B) &$xs) {}"
        , "<?php function f(A&B &$xs) {}"
        ]
      case parseProgram "test.php" "<?php function swap(int &$a, int &$b) {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtFunction _ fn]) -> case funcParams fn of
          [p1, p2] -> do
            assertBool "p1 byRef" (paramByRef p1)
            assertBool "p2 byRef" (paramByRef p2)
            case (paramType p1, paramType p2) of
              (Just (SimpleType _ (QualifiedName _ NameUnqualified ["int"])),
               Just (SimpleType _ (QualifiedName _ NameUnqualified ["int"]))) -> pure ()
              other -> assertFailure ("Unexpected param types: " ++ show other)
          _ -> assertFailure "Expected 2 params"
        _ -> assertFailure "Expected StmtFunction"
      case parseProgram "test.php" "<?php function f(int &...$xs) {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtFunction _ fn]) -> case funcParams fn of
          [p] -> do
            assertBool "p byRef" (paramByRef p)
            assertBool "p variadic" (paramVariadic p)
            case paramType p of
              Just (SimpleType _ (QualifiedName _ NameUnqualified ["int"])) -> pure ()
              other -> assertFailure ("Unexpected param type: " ++ show other)
          _ -> assertFailure "Expected 1 param"
        _ -> assertFailure "Expected StmtFunction"
      case parseProgram "test.php" "<?php function f((A&B) &$xs) {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtFunction _ fn]) -> case funcParams fn of
          [p] -> do
            assertBool "p byRef" (paramByRef p)
            case paramType p of
              Just (DNFType _ [IntersectionType _ _]) -> pure ()
              other -> assertFailure ("Unexpected param type: " ++ show other)
          _ -> assertFailure "Expected 1 param"
        _ -> assertFailure "Expected StmtFunction"
      case parseProgram "test.php" "<?php function f(A&B &$xs) {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtFunction _ fn]) -> case funcParams fn of
          [p] -> do
            assertBool "p byRef" (paramByRef p)
            case paramType p of
              Just (IntersectionType _ _) -> pure ()
              other -> assertFailure ("Unexpected param type: " ++ show other)
          _ -> assertFailure "Expected 1 param"
        _ -> assertFailure "Expected StmtFunction"
      case parseProgram "test.php" "<?php function f(A&B $xs) {}" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtFunction _ fn]) -> case funcParams fn of
          [p] -> do
            assertBool "p not byRef" (not (paramByRef p))
            case paramType p of
              Just (IntersectionType _ _) -> pure ()
              other -> assertFailure ("Unexpected param type: " ++ show other)
          _ -> assertFailure "Expected 1 param"
        _ -> assertFailure "Expected StmtFunction"
      case parseProgram "test.php" "<?php class C { public function __construct(private array &$ref) {} }" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtClass _ cd]) -> case classMembers cd of
          [MemberMethod md] -> case methodParams md of
            [p] -> do
              assertEqual "vis" (Just Private) (paramVis p)
              assertBool "byRef" (paramByRef p)
              case paramType p of
                Just (SimpleType _ (QualifiedName _ NameUnqualified ["array"])) -> pure ()
                other -> assertFailure ("Unexpected param type: " ++ show other)
            _ -> assertFailure "Expected 1 param"
          _ -> assertFailure "Expected 1 MemberMethod"
        _ -> assertFailure "Expected StmtClass"
    , testCase "Reject variadic constructor-promoted properties (Issue #191)" $ do
      mapM_ assertParsesFail
        [ "<?php class A { public function __construct(public ...$x) {} }"
        , "<?php class A { public function __construct(protected ...$x) {} }"
        , "<?php class A { public function __construct(private ...$x) {} }"
        , "<?php class A { public function __construct(readonly ...$x) {} }"
        , "<?php class A { public function __construct(public readonly ...$x) {} }"
        , "<?php class A { public function __construct(public(set) ...$x) {} }"
        , "<?php class A { public function __construct(public private(set) ...$x) {} }"
        , "<?php class A { public function __construct(final public ...$x) {} }"
        ]
      case parseProgram "test.php" "<?php class A { public function __construct(public ...$x) {} }" of
        Left err -> assertBool
          "error should reject variadic promoted property"
          (maybe False (T.isInfixOf "Cannot declare variadic promoted property") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php class A { public function __construct(...$x) {} }"
        , "<?php class A { public function __construct(public int $x) {} }"
        , "<?php class A { public function __construct(public int $x, ...$rest) {} }"
        ]
    , testCase "Reject pure enum cases with values and backed enum cases without values (Issue #197)" $ do
      mapM_ assertParsesFail
        [ "<?php enum Status { case Draft = 1; }"
        , "<?php enum Status { case Draft = 'draft'; }"
        , "<?php enum Status { case A; case B = 2; }"
        , "<?php enum Status { #[Attr] case Draft = 1; }"
        , "<?php enum BackedStatus: string { case Draft; }"
        , "<?php enum BackedStatus: int { case Draft; }"
        , "<?php enum BackedStatus: int { case A = 1; case B; }"
        , "<?php enum BackedStatus: string { #[Attr] case Draft; }"
        ]
      case parseProgram "test.php" "<?php enum Status { case Draft = 1; }" of
        Left err -> assertBool
          "error should reject case value in non-backed enum"
          (maybe False (T.isInfixOf "Case Draft of non-backed enum Status must not have a value") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php enum BackedStatus: string { case Draft; }" of
        Left err -> assertBool
          "error should reject missing case value in backed enum"
          (maybe False (T.isInfixOf "Case Draft of backed enum BackedStatus must have a value") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php enum Status { case Draft; }"
        , "<?php enum Status { case Draft; case Published; }"
        , "<?php enum Status { #[Attr] case Draft; }"
        , "<?php enum Status {}"
        , "<?php enum Status { case new; case match; }"
        , "<?php enum Status { case Draft; public function foo() {} const X = 1; }"
        , "<?php enum BackedStatus: string { case Draft = 'draft'; }"
        , "<?php enum BackedStatus: int { case Draft = 1; case Published = 2; }"
        , "<?php enum BackedStatus: int { #[Attr] case Draft = 1; }"
        , "<?php enum BackedStatus: string {}"
        , "<?php enum BackedStatus: int { case new = 1; case match = 2; }"
        , "<?php enum BackedStatus: string { case Draft = 'draft'; public function foo() {} const X = 1; }"
        ]
    , testCase "Reject switch statements with more than one default clause (Issue #195)" $ do
      mapM_ assertParsesFail
        [ "<?php switch ($x) { default: break; default: break; }"
        , "<?php switch ($x) { case 1: break; default: break; default: break; }"
        , "<?php switch ($x): default: break; default: break; endswitch;"
        , "<?php switch ($x): case 1: break; default: break; default: break; endswitch;"
        ]
      case parseProgram "test.php" "<?php switch ($x) { default: break; default: break; }" of
        Left err -> assertBool
          "error should reject multiple default clauses"
          (maybe False (T.isInfixOf "Switch statements may only contain one default clause") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php switch ($x) { default: break; }"
        , "<?php switch ($x) { case 1: break; default: break; }"
        , "<?php switch ($x) { case 1: break; case 2: break; }"
        , "<?php switch ($x): default: break; endswitch;"
        , "<?php switch ($x): case 1: break; default: break; endswitch;"
        ]
    , testCase "Reject duplicate parameter names in parameter lists (Issue #198)" $ do
      mapM_ assertParsesFail
        [ "<?php function foo($a, $a) {}"
        , "<?php function foo($a, $b, $a) {}"
        , "<?php function foo($a, ...$a) {}"
        , "<?php function foo(...$a, $a) {}"
        , "<?php class Foo { public function bar($x, $x) {} }"
        , "<?php interface Foo { public function bar($x, $x); }"
        , "<?php trait Foo { public function bar($x, $x) {} }"
        , "<?php enum Foo { public function bar($x, $x) {} }"
        , "<?php abstract class Foo { abstract public function bar($x, $x); }"
        , "<?php class Foo { public function __construct(public int $x, string $x) {} }"
        , "<?php $f = function ($x, $x) {};"
        , "<?php $f = static function ($x, $x) {};"
        , "<?php $g = fn($x, $x) => 1;"
        , "<?php $g = static fn($x, $x) => 1;"
        ]
      case parseProgram "test.php" "<?php function foo($a, $a) {}" of
        Left err -> assertBool
          "error should reject duplicate parameter names"
          (maybe False (T.isInfixOf "Redefinition of parameter $a") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class Foo { public function bar($x, $y, $x) {} }" of
        Left err -> assertBool
          "error should report redefinition of parameter $x"
          (maybe False (T.isInfixOf "Redefinition of parameter $x") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php function foo($a, $b) {}"
        , "<?php function foo($a, $A) {}"
        , "<?php class Foo { public function bar($x, $y) {} }"
        , "<?php interface Foo { public function bar($x, $y); }"
        , "<?php trait Foo { public function bar($x, $y) {} }"
        , "<?php enum Foo { public function bar($x, $y) {} }"
        , "<?php abstract class Foo { abstract public function bar($x, $y); }"
        , "<?php class Foo { public function __construct(public int $x, string $y) {} }"
        , "<?php $f = function ($x, $y) {};"
        , "<?php $f = static function ($x, $y) {};"
        , "<?php $g = fn($x, $y) => 1;"
        , "<?php $g = static fn($x, $y) => 1;"
        ]
    , testCase "Reject non-public constants and non-public, final, or abstract methods in interfaces (Issue #199)" $ do
      mapM_ assertParsesFail
        [ "<?php interface I { private const X = 1; }"
        , "<?php interface I { protected const Y = 2; }"
        , "<?php interface I { private function f(); }"
        , "<?php interface I { protected function g(); }"
        , "<?php interface I { final function h(); }"
        , "<?php interface I { abstract function k(); }"
        , "<?php interface I { public abstract function k(); }"
        , "<?php interface I { public final function h(); }"
        , "<?php interface I { private static function f(); }"
        , "<?php interface I { static final function f(); }"
        , "<?php interface I { static abstract function f(); }"
        ]
      case parseProgram "test.php" "<?php interface I { private const X = 1; }" of
        Left err -> assertBool
          "error should reject non-public interface constant"
          (maybe False (T.isInfixOf "Access type for interface constant I::X must be public") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php interface I { protected const Y = 2; }" of
        Left err -> assertBool
          "error should reject protected interface constant"
          (maybe False (T.isInfixOf "Access type for interface constant I::Y must be public") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php interface I { private function f(); }" of
        Left err -> assertBool
          "error should reject non-public interface method"
          (maybe False (T.isInfixOf "Access type for interface method I::f() must be public") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php interface I { final function h(); }" of
        Left err -> assertBool
          "error should reject final interface method"
          (maybe False (T.isInfixOf "Interface method I::h() must not be final") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php interface I { abstract function k(); }" of
        Left err -> assertBool
          "error should reject abstract interface method"
          (maybe False (T.isInfixOf "Interface method I::k() must not be abstract") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php interface I { const X = 1; }"
        , "<?php interface I { public const X = 1; const Y = 2; }"
        , "<?php interface I { final const Z = 3; }"
        , "<?php interface I { public final const Z = 3; }"
        , "<?php interface I { function f(); }"
        , "<?php interface I { public function f(); function g(); }"
        , "<?php interface I { public static function f(); static function g(); }"
        ]

  , testCase "Reject untyped readonly properties and constructor promotion (Issue #204)" $ do
      mapM_ assertParsesFail
        [ "<?php class Foo { public readonly $bar; }"
        , "<?php class Foo { protected readonly $bar; }"
        , "<?php class Foo { private readonly $bar; }"
        , "<?php class Foo { readonly $bar; }"
        , "<?php class Foo { readonly public $bar; }"
        , "<?php class Bar { public function __construct(public readonly $baz) {} }"
        , "<?php class Bar { public function __construct(protected readonly $baz) {} }"
        , "<?php class Bar { public function __construct(private readonly $baz) {} }"
        , "<?php class Bar { public function __construct(readonly $baz) {} }"
        , "<?php trait T { public readonly $bar; }"
        , "<?php trait T { readonly $bar; }"
        , "<?php $c = new class { public readonly $bar; };"
        , "<?php $c = new class { readonly $bar; };"
        , "<?php readonly class Foo { public function __construct(public $bar) {} }"
        ]
      case parseProgram "test.php" "<?php class Foo { public readonly $bar; }" of
        Left err -> assertBool
          "error should mention readonly property must have type"
          (maybe False (T.isInfixOf "Readonly property must have type") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class Bar { public function __construct(public readonly $baz) {} }" of
        Left err -> assertBool
          "error should mention readonly property must have type"
          (maybe False (T.isInfixOf "Readonly property must have type") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php class Foo { public readonly int $bar; }"
        , "<?php class Foo { readonly string $bar; }"
        , "<?php class Foo { public $bar; }"
        , "<?php class Foo { var $bar; }"
        , "<?php class Foo { public static $bar; }"
        , "<?php class Bar { public function __construct(public readonly int $baz) {} }"
        , "<?php class Bar { public function __construct(readonly string $baz) {} }"
        , "<?php class Bar { public function __construct(public $baz) {} }"
        , "<?php class Bar { public function __construct(int $baz) {} }"
        , "<?php class Bar { public function __construct($baz) {} }"
        , "<?php trait T { public readonly int $bar; }"
        , "<?php $c = new class { public readonly int $bar; };"
        ]

  , testCase "Reject void, never, and callable property types (Issue #207)" $ do
      mapM_ assertParsesFail
        [ "<?php class Foo { public void $x; }"
        , "<?php class Foo { public never $y; }"
        , "<?php class Foo { public callable $z; }"
        , "<?php class Foo { protected void $x; }"
        , "<?php class Foo { private never $y; }"
        , "<?php class Foo { var callable $z; }"
        , "<?php class Foo { public ?callable $z; }"
        , "<?php class Foo { public callable|int $z; }"
        , "<?php class Foo { public (callable&Bar)|int $z; }"
        , "<?php class Bar { public function __construct(public void $x) {} }"
        , "<?php class Bar { public function __construct(protected never $y) {} }"
        , "<?php class Bar { public function __construct(private callable $z) {} }"
        , "<?php class Bar { public function __construct(readonly void $x) {} }"
        , "<?php class Bar { public function __construct(public ?callable $z) {} }"
        , "<?php class Bar { public function __construct(public callable|int $z) {} }"
        , "<?php trait T { public void $x; }"
        , "<?php trait T { public never $y; }"
        , "<?php trait T { public callable $z; }"
        , "<?php $c = new class { public callable $z; };"
        , "<?php interface I { public void $x { get; } }"
        , "<?php interface I { public callable $z { get; } }"
        ]
      case parseProgram "test.php" "<?php class Foo { public void $x; }" of
        Left err -> assertBool
          "error should mention cannot have type void"
          (maybe False (T.isInfixOf "cannot have type void") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class Foo { public never $y; }" of
        Left err -> assertBool
          "error should mention cannot have type never"
          (maybe False (T.isInfixOf "cannot have type never") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class Foo { public callable $z; }" of
        Left err -> assertBool
          "error should mention cannot have type callable"
          (maybe False (T.isInfixOf "cannot have type callable") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class Bar { public function __construct(public callable $z) {} }" of
        Left err -> assertBool
          "error should mention cannot have type callable"
          (maybe False (T.isInfixOf "cannot have type callable") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php class Foo { public int $x; public ?string $y; }"
        , "<?php function f(callable $c) {}"
        , "<?php function f(): void {}"
        , "<?php function f(): never {}"
        , "<?php class Bar { public function __construct(int $x, callable $c) {} }"
        , "<?php class Bar { public function __construct(callable $c) {} }"
        , "<?php class Foo { public function bar(callable $c): void {} }"
        ]
    , testCase "Reject abstract private methods in classes (Issue #208)" $ do
      mapM_ assertParsesFail
        [ "<?php abstract class C { abstract private function f(); }"
        , "<?php abstract class C { private abstract function f(); }"
        , "<?php class C { abstract private function f(); }"
        , "<?php class C { private abstract function f(); }"
        , "<?php abstract class Foo { static abstract private function bar(); }"
        , "<?php abstract readonly class Foo { abstract private function bar(); }"
        , "<?php class Foo { static private abstract function bar(); }"
        ]
      case parseProgram "test.php" "<?php abstract class Foo { abstract private function bar(); }" of
        Left err -> assertBool
          "error should reject abstract private method with class and method name"
          (maybe False (T.isInfixOf "Abstract function Foo::bar() cannot be declared private") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      case parseProgram "test.php" "<?php class C { private abstract function f(); }" of
        Left err -> assertBool
          "error should reject abstract private method in non-abstract class"
          (maybe False (T.isInfixOf "Abstract function C::f() cannot be declared private") (errorCustom err))
        Right _ -> assertFailure "Expected parse failure but parse succeeded"
      mapM_ assertParsesOk
        [ "<?php abstract class C { abstract public function f(); }"
        , "<?php abstract class C { abstract protected function f(); }"
        , "<?php abstract class C { abstract function f(); }"
        , "<?php class C { private function f() {} }"
        , "<?php class C { private static function f() {} }"
        , "<?php trait T { abstract private function f(); }"
        , "<?php trait T { private abstract function f(); }"
        , "<?php trait T { static abstract private function f(); }"
        ]
  ]




assertParsesOk :: Text -> Assertion
assertParsesOk src = case parseProgram "test.php" src of
  Left err -> assertFailure (show (formatParseError err))
  Right _ -> pure ()

assertParsesFail :: Text -> Assertion
assertParsesFail src = case parseProgram "test.php" src of
  Left _ -> pure ()
  Right _ -> assertFailure "Expected parse failure but parse succeeded"

assertErrorAt :: Text -> (Int, Int, Int) -> (Int, Int, Int) -> Maybe Text -> Assertion
assertErrorAt src start end found = case parseProgram "test.php" src of
  Left err -> do
    let pos p = (posLine p, posColumn p, posOffset p)
    assertEqual "span start" start (pos (spanStart (errorSpan err)))
    assertEqual "span end" end (pos (spanEnd (errorSpan err)))
    assertEqual "found" found (errorFound err)
  Right _ -> assertFailure "Expected parse failure but parse succeeded"
