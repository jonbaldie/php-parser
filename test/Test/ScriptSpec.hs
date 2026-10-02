{-# LANGUAGE OverloadedStrings #-}

-- | 'checkScript' judged directly on hand-built statement lists, without
-- running the parser (Issue #337).
module Test.ScriptSpec (scriptTests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Language.PHP.AST
import Language.PHP.Parser.Script
import Language.PHP.Span

-- | A span starting at the given offset, so a failure can name its statement.
at :: Int -> Span
at n = Span (SourcePos "t.php" 1 (n + 1) n) (SourcePos "t.php" 1 (n + 1) n)

echo, nop :: Int -> ScriptItem
echo n = ScriptStmt (StmtEcho (at n) [ExprLit (at n) (LitInt (at n) 1 "1")])
nop n = ScriptStmt (StmtEmpty (at n))

declare :: Text -> Int -> ScriptItem
declare name n = ScriptStmt (declareStmt name n Nothing)

declareStmt :: Text -> Int -> Maybe [Stmt Span] -> Stmt Span
declareStmt name n =
  StmtDeclare (at n) [DeclareDirective (at n) (Ident (at n) name) (ExprLit (at n) (LitInt (at n) 1 "1"))]

bracketed :: Int -> [Stmt Span] -> ScriptItem
bracketed n body = ScriptStmt (StmtNamespace (at n) Nothing (Just body))

unbracketed :: Int -> ScriptItem
unbracketed n = ScriptStmt (StmtNamespace (at n) (Just (QualifiedName (at n) NameUnqualified ["A"])) Nothing)

stmt :: ScriptItem -> Stmt Span
stmt (ScriptStmt s) = s
stmt BareCloseTag = error "BareCloseTag is not a statement"

accepts :: [ScriptItem] -> Assertion
accepts items = checkScript items @?= Right ()

rejectsAt :: Int -> Text -> [ScriptItem] -> Assertion
rejectsAt n msg items = checkScript items @?= Left (at n, msg)

noCode, nested, tooLate, mixed, strictLate, encodingLate :: Text
noCode = "No code may exist outside of namespace {}"
nested = "Namespace declarations cannot be nested"
tooLate = "Namespace declaration statement has to be the very first statement or after any declare call in the script"
mixed = "Cannot mix bracketed namespace declarations with unbracketed namespace declarations"
strictLate = "strict_types declaration must be the very first statement in the script"
encodingLate = "Encoding declaration pragma must be the very first statement in the script"

scriptTests :: TestTree
scriptTests = testGroup "Script-structure rules (Issue #337)"
  [ testGroup "Code outside a bracketed namespace"
      [ testCase "a statement after a bracketed namespace" $
          rejectsAt 2 noCode [bracketed 0 [], echo 2]
      , testCase "a declare after a bracketed namespace" $
          rejectsAt 3 noCode [declare "ticks" 0, bracketed 1 [], declare "ticks" 3]
      , testCase "a nop, a bare close tag and __halt_compiler are not code" $
          accepts [bracketed 0 [], nop 1, BareCloseTag, ScriptStmt (StmtHaltCompiler (at 3) "junk")]
      , testCase "a leading shebang is not code, but a later one is" $ do
          let shebang n = ScriptStmt (StmtInlineHtml (at n) "#!/usr/bin/env php\n")
          accepts [shebang 0, bracketed 1 []]
          rejectsAt 2 noCode [shebang 0, bracketed 1 [], echo 2]
          rejectsAt 1 tooLate [echo 0, shebang 0, bracketed 1 []]
      ]
  , testGroup "Namespace position and form"
      [ testCase "declares may precede the first namespace" $
          accepts [declare "strict_types" 0, declare "ticks" 1, nop 2, unbracketed 3, echo 4]
      , testCase "other code may not" $
          rejectsAt 1 tooLate [echo 0, unbracketed 1]
      , testCase "later namespaces may follow code of an unbracketed one" $
          accepts [unbracketed 0, echo 1, unbracketed 2, echo 3]
      , testCase "forms may not be mixed" $ do
          rejectsAt 1 mixed [unbracketed 0, bracketed 1 []]
          rejectsAt 1 mixed [bracketed 0 [], unbracketed 1]
      , testCase "a namespace inside a bracketed one is nested, or mixed if unbracketed" $ do
          rejectsAt 1 nested [bracketed 0 [stmt (bracketed 1 [])]]
          rejectsAt 1 mixed [bracketed 0 [stmt (unbracketed 1)]]
      ]
  , testGroup "Declare prologue"
      [ testCase "strict_types and encoding must lead the script" $ do
          accepts [declare "ticks" 0, declare "strict_types" 1, declare "encoding" 2]
          rejectsAt 1 strictLate [echo 0, declare "strict_types" 1]
          rejectsAt 1 encodingLate [nop 0, declare "ENCODING" 1]
          rejectsAt 1 strictLate [BareCloseTag, declare "strict_types" 1]
      , testCase "the prologue closes at the first namespace" $
          rejectsAt 1 strictLate [bracketed 0 [stmt (declare "strict_types" 1)]]
      , testCase "a declare nested anywhere is outside the prologue" $ do
          rejectsAt 1 strictLate [ScriptStmt (declareStmt "ticks" 0 (Just [stmt (declare "strict_types" 1)]))]
          rejectsAt 2 strictLate [ScriptStmt (StmtIf (at 0) (ExprLit (at 0) (LitInt (at 0) 1 "1")) [StmtBlock (at 1) [stmt (declare "strict_types" 2)]] [] Nothing)]
      ]
  ]
