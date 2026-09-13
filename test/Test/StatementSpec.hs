{-# LANGUAGE OverloadedStrings #-}

module Test.StatementSpec (statementTests) where

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
        [ "<?php enum E: int { case X; }"
        , "<?php enum E: string { case X; }"
        , "<?php enum E: INT { case X; }"
        , "<?php enum E: String { case X; }"
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

  , testCase "Short echo tag accepts comma-separated expressions (Issue #114)" $ do
      case parseProgram "test.php" "<?= 1, 2 ?>" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtEcho _ exprs]) ->
          assertEqual "echo expression count" 2 (length exprs)
        Right other -> assertFailure ("Expected single echo, got: " ++ show other)

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

  , testCase "Require separator after long opening tag (Issue #93)" $ do
      assertParsesFail "<?php$x = 1;"
      assertParsesFail "<?phpphpinfo();"
      assertParsesOk "<?php $x = 1;"
      assertParsesOk "<?php\n$x = 1;"
      assertParsesOk "<?php\r\n$x = 1;"
      assertParsesOk "<?php\t$x = 1;"
      assertParsesOk "<?php"

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
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (LitInt _ val _)] Nothing, StmtFunction _ _]) -> do
          assertEqual "directive name" "strict_types" name
          assertEqual "directive value" 1 val
        other -> assertFailure ("Unexpected AST for declare statement: " ++ show other)

      case parseProgram "test.php" "<?php declare(ticks=1) { echo 'x'; }" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (LitInt _ val _)] (Just [StmtEcho _ _])]) -> do
          assertEqual "directive name" "ticks" name
          assertEqual "directive value" 1 val
        other -> assertFailure ("Unexpected AST for block declare: " ++ show other)

      case parseProgram "test.php" "<?php declare(ticks=1): echo 'x'; enddeclare;" of
        Left err -> assertFailure (show (formatParseError err))
        Right (Program _ [StmtDeclare _ [DeclareDirective _ (Ident _ name) (LitInt _ val _)] (Just [StmtEcho _ _])]) -> do
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
